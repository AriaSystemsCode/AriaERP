*-- Include the .H file
#INCLUDE R:\ARIA4XP\PRGS\SY\ARIA.H

*:************************************************************************
*: Program file  : ARIAMAIN.PRG
*: Program desc. : Main Function and Procedures
*:         System: Aria advantage series Ver. 4.0
*:         Module: Main system 
*:           Date: 2011
*:************************************************************************
*: Modifications : 
*! B609711,1 MAH 11/23/2011 Separate Business From Aria.exe Begin [T20110803.0001]
*:************************************************************************



*!*************************************************************
*! Name      : gfSubStr
*! Developer : Yasser Saad Ibrahime
*! Date      : 1993-1995 
*! Purpose   : To extract element from string or to convert string to array
*!*************************************************************
*! Calls     : 
*!      Called by: ARIA3.PRG                
*!      Called by: GFSETUP()                (function  in ARIA3.PRG)
*!      Called by: GFSCRINI()               (function  in ARIA3.PRG)
*!      Called by: GFMODALGEN()             (function  in ARIA3.PRG)
*!      Called by: GFSEEKREC()              (function  in ARIA3.PRG)
*!      Called by: GFDBFFIELD()             (function  in ARIA3.PRG)
*!      Called by: GFFLOCK()                (function  in ARIA3.PRG)
*!      Called by: GFRLOCK()                (function  in ARIA3.PRG)
*!      Called by: GFWAIT()                 (function  in ARIA3.PRG)
*!      Called by: GFGETVLD()               (function  in ARIA3.PRG)
*!*************************************************************
*! Passed Parameters  : String to be used
*!                      poiter to array or element position
*!                      sparators used in the string
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : 
*!*************************************************************
* This function will return eather a string part # OR an array of all
* the string parts according to the type of the second parameter. The
* firest parameter will be the string or string variable. If the
* second parameter have a numeric type, the function will return the
* but if it is an array the function will return the array with each
*  element having a part from the string.
* 
*:->
FUNCTION gfSubStr
PARAMETERS lcString,lnAryOrPos,lcSepta

lcSubstr  =' '
lnAryDim  = 1
lnAryRows = 1
lnAryCols = 1
lcSepta   = IIF(TYPE('lcSepta')='C',lcSepta,',') 

IF LEN(ALLTRIM(lcSepta))>1
  lcColSep  = SUBSTR(lcSepta,2,1)
  lcSepta   = LEFT(lcSepta,1)
  lnAryDim  = IIF(OCCURS(lcSepta,lcString)>0,;
              OCCURS(lcSepta,lcString)+;
              IIF(RIGHT(lcString,1)<>lcSepta,1,0),;
              lnAryDim)
  lnAryCols = IIF(OCCURS(lcColSep,lcString)>0,;
              OCCURS(lcColSep,lcString)+;
              IIF(RIGHT(lcString,1)<>lcColSep,1,0),;
              lnAryDim)
  lnAryRows = (lnAryDim+(lnAryCols-1)) / lnAryCols
  lnAryDim  = lnAryDim +(lnAryCols-1)     
  lcString  = STRTRAN(lcString,lcColSep,lcSepta)
ELSE
  lnAryDim = IIF(OCCURS(lcSepta,lcString)>0,;
             OCCURS(lcSepta,lcString)+;
             IIF(RIGHT(lcString,1)<>lcSepta,1,0),;
             lnAryDim)
ENDIF

*** Chek if second parameter array or numeric
DO CASE
  *** If no parameter found assume firest part of string
  CASE TYPE ('lnAryOrPos')='U'
    lnAryOrPos = 1

  *** If array strich it to hold all string parts
  CASE TYPE ('lnAryOrPos') $ 'C,L'    
    IF lnAryCols > 1
      DIMENSION lnAryOrPos[lnAryRows,lnAryCols]
    ELSE
      IF ALEN(lnAryOrPos,2) > 0
        DIMENSION lnAryOrPos[lnAryDim,ALEN(lnAryOrPos,2)]
      ELSE
        DIMENSION lnAryOrPos[lnAryDim]
      ENDIF  

    ENDIF
    lnAryOrPos  = ' '

ENDCASE

FOR lnArElem  = 1 TO lnAryDim
  IF TYPE ('lnAryOrPos')='N'
    lnArElem = lnAryOrPos
  ENDIF  

  DO CASE
    *** In case of firest string part
    CASE lnArElem = 1
      lcSubstr = SUBSTR(lcString,1,;
      IIF(lcSepta $ lcString,AT(lcSepta,lcString)-1,LEN(lcString)))

    *** In case of last string part
    CASE lnArElem = lnAryDim
      lcSubstr = SUBSTR(lcString,AT(lcSepta,lcString,lnArElem-1)+1)
      lcSubstr = IIF(RIGHT(lcSubstr,1)=lcSepta,;
                 SUBSTR(lcSubstr,1,LEN(lcSubstr)-1),lcSubstr)
    *** In case of any string part from the meddel
    CASE lnArElem > 1
      lcSubstr = SUBSTR(lcString,AT(lcSepta,lcString,lnArElem-1)+1,;
                 AT(lcSepta,lcString,lnArElem)-;
                 AT(lcSepta,lcString,lnArElem-1)-1)
  ENDCAS

  IF TYPE ('lnAryOrPos')='N'
    RETURN lcSubstr
  ENDIF  
  
  IF lnAryCols > 1
    lnAryOrPos[((lnArElem-1)%lnAryRows)+1,INT((lnArElem-1)/lnAryRows)+1] = lcSubstr
  ELSE
    lnAryOrPos[lnArElem] = lcSubstr
  ENDIF
ENDFOR




*!*************************************************************
*! Name      : gfGetTime
*! Developer : Hesham El-Sheltawi
*! Date      : 10/22/96
*! Purpose   : get the current time with am,pm format
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : current time
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : 
*!*************************************************************
*:->
FUNCTION gfGetTime

lcCurrHour = IIF(VAL(SUBSTR(TIME(),1,2))=12 OR VAL(SUBSTR(TIME(),1,2))=0,;
             '12',ALLTRIM(STR(VAL(SUBSTR(TIME(),1,2))%12)))
             
lcCurrTime = IIF(VAL(lcCurrHour)<10,'0','')+lcCurrHour+;
             SUBSTR(TIME(),3)+IIF(VAL(SUBSTR(TIME(),1,2))>=12,' pm',' am')
             
RETURN (lcCurrTime)             


*!*************************************************************
*! Name      : gfMenuBar
*! Developer : Hesham El-Sheltawi
*! Date      : 10/22/96
*! Purpose   : Run a Menu option
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : lcPop_Name,lnBar_Pos
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : 
*!*************************************************************
*:->
FUNCTION gfMenuBar
PARAMETERS lcPop_Name,lnBar_Pos
=oAriaApplication.MenuBar(lcPop_Name,lnBar_Pos)

*!*************************************************************
*! Name      : gfChngComp
*! Developer : Hesham El-Sheltawi
*! Date      : 10/22/96
*! Purpose   : Change the active company
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : lcComp_ID Company ID to be active
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : 
*!*************************************************************
*:->
FUNCTION gfChngComp
PARAMETERS lcComp_id
=oAriaApplication.ChangeCompany(lcComp_Id)


*!*************************************************************
*! Name      : gfDoHelp
*! Developer : Hesham El-Sheltawi
*! Date      : 10/22/96
*! Purpose   : Activate the help screen for aria systems
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : 
*!*************************************************************
*:->
FUNCTION gfDoHelp
HELP



*:************************************************************************
*: Modifications : 
*! E037885,2 MAH 12/03/2004 Separate screen in different session.
*! E037885,4 MAH 02/16/2005 Remove the host form.
*! E037885,4 MAH 02/17/2005 Remove the host form.
*! B039071,1 MAH 02/24/2005 Close Group Problem.
*:************************************************************************

FUNCTION GPEXIT

*! B039071,1 MAH 02/24/2005 [BEGIN]
IF TYPE('oAriaApplication.oMainForm') = 'O' .AND. !ISNULL(oAriaApplication.oMainForm)
  oAriaApplication.oMainForm.QueryUnload()
  *! B609060,1 MMT 10/25/2009 Fix bug of 'gpExit.Prg' is not Found{Start}
  RETURN
  *! B609060,1 MMT 10/25/2009 Fix bug of 'gpExit.Prg' is not Found{End}
ENDIF

*! B609060,1 MMT 10/25/2009 Fix bug of 'gpExit.Prg' is not Found{Start}
*RETURN
*! B609060,1 MMT 10/25/2009 Fix bug of 'gpExit.Prg' is not Found{End}

*! B039071,1 MAH 02/24/2005 [END]

*! E037885,4 MAH 02/17/2005 Maximize the main screen  [BEGIN]
*-- _Screen.Visible = .T.
_SCREEN.Top         = 100
_SCREEN.WindowState = 2 
* MAH
*_SCREEN.Visible     = .T.
* MAH
*! E037885,4 MAH 02/17/2005 [END]

*! E037885,4 MAH 02/16/2005 No need [BEGIN]
*-- *! E037885,2 MAH 12/03/2004 [BEGIN]
*-- IF TYPE('oAriaApplication.oMainForm') = 'O' .AND. !ISNULL(oAriaApplication.oMainForm)
*--   oAriaApplication.oMainForm.Visible = .F.
*-- ENDIF
*-- *! E037885,2 MAH 12/03/2004 [END]
*! E037885,4 MAH 02/16/2005 [END]

CLEAR WINDOWS
LOCAL lnForms,lnCount
lnForms = 0
IF _SCREEN.FORMCOUNT > 0
  FOR lnCount = 1 TO _SCREEN.FORMCOUNT
    IF _SCREEN.FORMS(lnCount).VISIBLE AND _SCREEN.FORMS(lnCount).BASECLASS<>'Toolbar'
      lnForms = 1
      Exit
    ENDIF
  ENDFOR
ENDIF      
IF lnForms < 1
  *! E037885,2 MAH 12/03/2004 [BEGIN]
  ON SHUTDOWN
  *! E037885,2 MAH 12/03/2004 [END]

  *! E037885,4 MAH 02/16/2005 [BEGIN]
  *-- oAriaApplication.Sysexit()
  IF TYPE('oAriaApplication') = 'O' .AND. !ISNULL(oAriaApplication)
    *! B609060,1 MMT 10/25/2009 Fix bug of 'gpExit.Prg' is not Found{Start}
    TRY 
    *! B609060,1 MMT 10/25/2009 Fix bug of 'gpExit.Prg' is not Found{End}
    oAriaApplication.Sysexit()
    *! B609060,1 MMT 10/25/2009 Fix bug of 'gpExit.Prg' is not Found{Start}
    CATCH
    ENDTRY  
    *! B609060,1 MMT 10/25/2009 Fix bug of 'gpExit.Prg' is not Found{End}
  ENDIF
  *! E037885,4 MAH 02/16/2005 [END]
ENDIF  

*!*************************************************************
FUNCTION gfUserList
PARAMETERS llGetCount
PRIVATE ALL LIKE  L*
*SET DATASESSION TO 1
lcOldRep = SET('REPROCESS')
lnDataSession = set('datas')
SET DATASESSION TO 1
SET REPROCESS TO 1
*MEDIA MMT 05-19-2011[Start]
LOCAL lcSQLDICPATH 
lcSQLDICPATH = oAriaApplication.cAria4SysPath    
*MEDIA MMT 05-19-2011[End]
llGetCount = IIF(TYPE('llGetCount')="U",.F.,llGetCount)

DECLARE laUserList[1]
llUserUsed = USED('SYUUSER')
lcCurrFile = ALIAS()
laUserList = " "
lnUsrRec   = 0
IF !USED("SYUSTATC")
  *! E302555,1 MMT 09/18/2008 Add Syustatc File For Aria4 and count A4 users[Start]
  *USE (oAriaApplication.SysPath+"SYUSTATC") IN 0 SHARED 
  *MEDIA MMT 05-19-2011[Start]
  *lcSQLDICPATH = oAriaApplication.DefaultPath + 'SQLDictionary\'
  *! E302567,1 MMT 01/06/2009 Change file paths for SAAS[Start]
  *IF oAriaApplication.multiinst 
    *! E302857,1 HES 02/10/2011 Avoid 'X:\' Fixed Path [BEGIN]
*!*	    lcSQLDICPATH = 'X:\Aria4xp\SQLDictionary\'
   * lcSQLDICPATH = oAriaApplication.cAria4SysPath    
    *! E302857,1 HES 02/10/2011 Avoid 'X:\' Fixed Path [END  ]
  *ENDIF 
  *! E302567,1 MMT 01/06/2009 Change file paths for SAAS[End]
  *MEDIA MMT 05-19-2011[ENd]
  
  USE (lcSQLDICPATH +"SYUSTATC") IN 0 SHARED 
  *! E302555,1 MMT 09/18/2008 Add Syustatc File For Aria4 and count A4 users[End]
ENDIF
SELECT SYUSTATC

*B128052,1 AMH Set order to cuser_id to be sure the order tag is used [Start]
SET ORDER TO cUser_ID
*B128052,1 AMH [End]

*-- Hesham (Start)
*!*	IF !llUserUsed
*!*	  USE (oAriaApplication.SysPath+'SYUUSER') IN 0 ORDER TAG cuser_id
*!*	ENDIF
*!*	IF USED("SYUUSER")
*!*	  lnUsrRec = RECNO("SYUUSER")
*!*	ENDIF

*IF lnRemResult>=1

*-- Hesham (End)
*MAN Speed Optimization, use the curr user as a variable instead of obj. ref.
*!*	SELECT IIF(SYUSTATC.CUSER_ID = oAriaApplication.User_ID AND;
*!*	           SYUSTATC.cstation = oAriaApplication.Station,;
*!*	           "» ","  ")+;
*!*	           PADR(LFGETUSRNM(SYUSTATC.CUSER_ID),35) ;
*!*	     FROM (oAriaApplication.SysPath+"SYUSTATC");
*!*	     WHERE COBJ_TYP+ALLTRIM(COBJ_NAME)+SYUSTATC.CUSER_ID+CSTATION=;
*!*	            'INI'+'OLDVARS' ;
*!*	       .AND.;
*!*	             gfCheckUser(SYUSTATC.CUSER_ID,CSTATION) ;
*!*	       INTO ARRAY  laUserList     
lcCurUser= oAriaApplication.User_ID 
lcCurStat= oAriaApplication.Station
IF !llGetCount
  lnRemResult = oAriaApplication.remotesystemdata.execute("Select * from syuuser",'',"syuuser","",oAriaApplication.SystemConnectionString,3,"",1)
  
  *! E302555,1 MMT 09/18/2008 Add Syustatc File For Aria4 and count A4 users[Start]
  *SELECT IIF(SYUSTATC.CUSER_ID = lcCurUser AND;
           SYUSTATC.cstation = lcCurStat,;
           "» ","  ")+;
           PADR(LFGETUSRNM(SYUSTATC.CUSER_ID),35) ;
     FROM (oAriaApplication.SysPath+"SYUSTATC");
     WHERE COBJ_TYP+ALLTRIM(COBJ_NAME)+SYUSTATC.CUSER_ID+CSTATION=;
            'INI'+'OLDVARS' ;
       .AND.;
             gfCheckUser(SYUSTATC.CUSER_ID,CSTATION) ;
       INTO ARRAY  laUserList     
   *MEDIA MMT 05-19-2011[Start]    
   *lcSQLDICPATH = oAriaApplication.DefaultPath + 'SQLDictionary\'    
   
   
   *! E302567,1 MMT 01/06/2009 Change file paths for SAAS[Start]
   *IF oAriaApplication.multiinst 
     *lcSQLDICPATH = 'X:\Aria4xp\SQLDictionary\'
   *ENDIF 
   *! E302567,1 MMT 01/06/2009 Change file paths for SAAS[End]
   *MEDIA MMT 05-19-2011[End]
   
   SELECT IIF(SYUSTATC.CUSER_ID = lcCurUser AND;
           SYUSTATC.cstation = lcCurStat,;
           "» ","  ")+;
           PADR(LFGETUSRNM(SYUSTATC.CUSER_ID),35) ;
     FROM (lcSQLDICPATH+"SYUSTATC");
     WHERE COBJ_TYP+ALLTRIM(COBJ_NAME)+SYUSTATC.CUSER_ID+CSTATION=;
            'INI'+'OLDVARS' ;
       .AND.;
             gfCheckUser(SYUSTATC.CUSER_ID,CSTATION) ;
       INTO ARRAY  laUserList            
   *! E302555,1 MMT 09/18/2008 Add Syustatc File For Aria4 and count A4 users[End]
ELSE
  IF !USED("SYUSTATC")
  
    *! E302555,1 MMT 09/18/2008 Add Syustatc File For Aria4 and count A4 users[Start]
    *USE (oAriaApplication.SysPath+"SYUSTATC") IN 0 SHARED 
    *MEDIA MMT 05-19-2011[Start]
    *lcSQLDICPATH = oAriaApplication.DefaultPath + 'SQLDictionary\'
    
    *! E302567,1 MMT 01/06/2009 Change file paths for SAAS[Start]
    *IF oAriaApplication.multiinst 
      *lcSQLDICPATH = 'X:\Aria4xp\SQLDictionary\'
    *ENDIF 
    *! E302567,1 MMT 01/06/2009 Change file paths for SAAS[End]
     *MEDIA MMT 05-19-2011[End]
    
    USE (lcSQLDICPATH+"SYUSTATC") IN 0 SHARED 
    *! E302555,1 MMT 09/18/2008 Add Syustatc File For Aria4 and count A4 users[End]
    
  ENDIF
  lnUserCount = 0
  SELECT SYUSTATC
  
  *B128052,1 AMH Use gfCheckUser function to privent lock records with out unlock them [Start]
  *COUNT FOR COBJ_TYP+ALLTRIM(COBJ_NAME)+SYUSTATC.CUSER_ID+CSTATION=;
            'INI'+'OLDVARS' AND !RLOCK() TO lnUserCount
  *UNLOCK 
  
  *! E302555,1 MMT 09/18/2008 Add Syustatc File For Aria4 and count A4 users[Start]
  *COUNT FOR COBJ_TYP+ALLTRIM(COBJ_NAME)+SYUSTATC.CUSER_ID+CSTATION=;
            'INI'+'OLDVARS' AND !gfCheckUser(SYUSTATC.CUSER_ID,CSTATION) TO lnUserCount
   COUNT FOR COBJ_TYP+ALLTRIM(COBJ_NAME)+SYUSTATC.CUSER_ID+CSTATION=;
            'INI'+'OLDVARS' AND gfCheckUser(SYUSTATC.CUSER_ID,CSTATION) TO lnUserCount
  *! E302555,1 MMT 09/18/2008 Add Syustatc File For Aria4 and count A4 users[End]
  *B128052,1 AMH [End]
  
ENDIF

*! E302555,1 MMT 09/18/2008 Add Syustatc File For Aria4 and count A4 users[Start]
*IF oAriaApplication.UserStaticRecord < RECCOUNT('syuStatc')
IF oAriaApplication.UserStaticRecord <= RECCOUNT('syuStatc')
*! E302555,1 MMT 09/18/2008 Add Syustatc File For Aria4 and count A4 users[End]

  GO oAriaApplication.UserStaticRecord  IN syuStatc 
  SET REPROCESS TO 1
  =RLOCK('syuStatc')
ENDIF  
SET REPROCESS TO lcOldRep
IF !llGetCount
  oAriaApplication.DoFormRetVal(oAriaApplication.ScreenHome+"sy\syusrlst")
ENDIF  


IF !EMPTY(lcCurrFile)
  SELECT (lcCurrFile)
ENDIF  

*-- Hesham (Start)
*!*	IF lnUsrRec > 0 .AND. USED("SYUUSER")
*!*	  IF lnUsrRec <= RECCOUNT("SYUUSER")
*!*	    GO lnUsrRec IN SYUUSER
*!*	  ENDIF
*!*	ENDIF
*-- Hesham (End)
IF !llGetCount AND !llUserUsed
  USE IN SYUUSER
ENDIF

SET DATASESSION TO lnDataSession
IF llGetCount
  *RETURN _TALLY 
  RETURN lnUserCount
ENDIF



*!*************************************************************
FUNCTION gfCheckUser
PARAMETERS lcUserID, lcStation
lcStation=IIF(TYPE('lcStation')<>"C","",lcStation)
llRetFlag = .T.
IF SEEK('INI'+'OLDVARS'+lcUserID+lcStation,'SYUSTATC')
  IF RLOCK('SYUSTATC') 
    UNLOCK IN SYUSTATC
    llRetFlag = .F.
  ENDIF
ELSE
  llRetFlag = .F.
ENDIF  
RETURN llRetFlag .OR. (lcUserID+lcStation = lcCurUser+lcCurStat)


*!*************************************************************
FUNCTION lfGetUsrNm
PARAMETER lcUser_ID
SELECT SYUUSER
*-- Hesham (Start)
*IF SEEK(lcUser_ID)
LOCATE FOR CUSER_ID = lcUser_ID
IF FOUND()
*-- Hesham (End)
   RETURN cUsr_Name
ELSE
    RETURN lcUser_ID
ENDIF


*!*************************************************************
FUNCTION GPRELOGIN
IF _SCREEN.FORMCOUNT <= 1 
  oAriaApplication.Login()
  oAriaApplication.LogUser(.T.)
  OAriaApplication.SetMenu(OAriaApplication.ActiveModuleID,IIF(OAriaApplication.ActiveModuleID='SY','S','A'))
ELSE
  =MessageBox("You have to close all programs before login in with new use id",16)  
ENDIF


*!*************************************************************
FUNCTION gfPrintSet
=SYS(1037)

*!*************************************************************
FUNCTION gfTraceKey
LPARAMETERS lcFileName,lcKeyExpr,lcEventOccr,lcUpdtDir,lcUpdtModl
  *--Not Needed
RETURN

*!*************************************************************
*! Name      : gfBrowse
*! Developer : Hesham El-Sheltawi
*! Date      : 11/17/96
*! Purpose   : Browse a File and return .t. if the user select record
*!*************************************************************
*! Parameters: tcBrowseFields   && variable Hold the browse fields to
*!                              && be displayed with the headers if needed
*!             tcBrowseTitle    && browse title
*!             tcAlias          && alias to be browsed if not the default alias
*!             tcKey            && key to be filter in the browse
*!             tcFor            && FOR condition or FOR condition REST
*!             tcOptions        && Options for the shortcut to be displayed
*!*************************************************************
*! Called by : 
*!*************************************************************
*! Returns            : .t. if selected .f. if not
*!*************************************************************
*! Example   : llBrowseSelected=gfBrowse()
*!*************************************************************
*: Modifications : 
*! E038142,2 MAH 09/15/2004 Full support for run forms with SQL with high Performance.
*! B038623,1 MAH 10/12/2004 Fix preference problems.
*:************************************************************************

FUNCTION gfBrowse
*! E038142,2 MAH 09/15/2004 Add additonal parameters [BEGIN]
*-- lParameters tcBrowseFields,tcBrowseTitle,tcAlias,tcKey,tcFor,tcOptions,tlSelect,;
*--             toSelectObj   ,tcSelectMethod,tcUserShortCut,tcTempFile,tcSelField,;
*--             llHalfHeight,lcFieldsNam,lcArrName,llChckKey,lcAliasName,llGetTree
*! B038623,1 MAH 10/12/2004 Add Addtional Parmaeters [BEGIN]
*-- lPARAMETERS tcBrowseFields, tcBrowseTitle, tcAlias, tcKey, tcFor, tcOptions, tlSelect, ;
*--             toSelectObj , tcSelectMethod, tcUserShortCut, tcTempFile, tcSelField, ;
*--             llHalfHeight, lcFieldsNam, lcArrName, llChckKey, lcAliasName, llGetTree, ;
*--             lcBrowseFileName, lcBrowseTableDBEngine, lcBrowsePKIndexName, lcBrowseIndexName, lcCurrentRecordKey
LPARAMETERS tcBrowseFields, tcBrowseTitle, tcAlias, tcKey, tcFor, tcOptions, tlSelect, ;
            toSelectObj , tcSelectMethod, tcUserShortCut, tcTempFile, tcSelField, ;
            llHalfHeight, lcFieldsNam, lcArrName, llChckKey, lcAliasName, llGetTree, ;
            lcBrowseFileName, lcBrowseTableDBEngine, lcBrowsePKIndexName, lcBrowseIndexName, lcCurrentRecordKey, ;
            lcPreferenceKey, lcPreferenceSubKey
            
*! B038623,1 MAH 10/12/2004 [END]
*! E038142,2 MAH 09/15/2004 [END]

LOCAL llReturnValue,lcAlias
lcAlias = SELECT()
IF !EMPTY(tcAlias)
  SELECT (tcAlias)
ENDIF
IF EMPTY(tcBrowseFields)
  tcBrowseFields=gfDataBaseProp('Get',ALIAS(),'Table','BrowseFields')
ENDIF
PRIVATE oBrowse
oBrowse = .Null.

*! E038142,2 MAH 09/15/2004 Add additonal parameters [BEGIN]
*-- DO FORM (oAriaApplication.ScreenHome+"sy\BROWSE") WITH tcBrowseFields,tcBrowseTitle,tcKey,tcFor,tcOptions,tlSelect,;
*--            toSelectObj   ,tcSelectMethod,tcUserShortCut,tcTempFile,tcSelField,;
*--            llHalfHeight,lcFieldsNam,lcArrName,llChckKey,lcAliasName,llGetTree;
*--     TO llReturnValue
*! B038623,1 MAH 10/12/2004 Add Addtional Parmaeters [BEGIN]
*-- DO FORM (oAriaApplication.ScreenHome+"sy\BROWSE") WITH ;
*--         tcBrowseFields, tcBrowseTitle, tcKey, tcFor, tcOptions, tlSelect, ;
*--         toSelectObj, tcSelectMethod, tcUserShortCut, tcTempFile, tcSelField, ;
*--         llHalfHeight, lcFieldsNam, lcArrName, llChckKey, lcAliasName, llGetTree, ;
*--         lcBrowseFileName, lcBrowseTableDBEngine, lcBrowsePKIndexName, lcBrowseIndexName, lcCurrentRecordKey ;
*--     TO llReturnValue
DO FORM (oAriaApplication.ScreenHome+"sy\BROWSE") WITH ;
        tcBrowseFields, tcBrowseTitle, tcKey, tcFor, tcOptions, tlSelect, ;
        toSelectObj, tcSelectMethod, tcUserShortCut, tcTempFile, tcSelField, ;
        llHalfHeight, lcFieldsNam, lcArrName, llChckKey, lcAliasName, llGetTree, ;
        lcBrowseFileName, lcBrowseTableDBEngine, lcBrowsePKIndexName, lcBrowseIndexName, lcCurrentRecordKey, ;
        lcPreferenceKey, lcPreferenceSubKey ;
    TO llReturnValue
*! B038623,1 MAH 10/12/2004 [END]
*! E038142,2 MAH 09/15/2004 [END]

SELECT (lcAlias)    
RETURN llReturnValue


*!*************************************************************
*: Modifications : 
*! E038142,2 MAH 09/16/2004 Full support for run forms with SQL with high Performance.
*! B038623,1 MAH 10/12/2004 Fix preference problems.
*:************************************************************************
FUNCTION AriaBrow

*! E038142,2 MAH 09/16/2004 Add addtional parameters [BEGIN]
*-- PARAMETER tcKey,tcBrowseTitle,lnY1,lnX1,lnY2,lnX2,lcOnSelect,lcBrPushB,lcFieldsNam,;
*--           lcArrName,llChckKey,lcAliasName,llGetTree
*! B038623,1 MAH 10/12/2004 Add Addtional Parmaeters [BEGIN]
*-- PARAMETER tcKey, tcBrowseTitle, lnY1, lnX1, lnY2, lnX2, lcOnSelect, lcBrPushB, lcFieldsNam, ;
*--           lcArrName, llChckKey, lcAliasName, llGetTree, ;
*--           lcBrowseFileName, lcBrowseTableDBEngine, lcBrowsePKIndexName, lcBrowseIndexName, lcSeekToKey

*! B608684,1 MMT 09/10/2008 call gfStyBrw when browse style file in option grid[Start]      
*PARAMETER tcKey, tcBrowseTitle, lnY1, lnX1, lnY2, lnX2, lcOnSelect, lcBrPushB, lcFieldsNam, ;
          lcArrName, llChckKey, lcAliasName, llGetTree, ;
          lcBrowseFileName, lcBrowseTableDBEngine, lcBrowsePKIndexName, lcBrowseIndexName, lcSeekToKey, ;
          lcPreferenceKey, lcPreferenceSubKey
PARAMETER tcKey, tcBrowseTitle, lnY1, lnX1, lnY2, lnX2, lcOnSelect, lcBrPushB, lcFieldsNam, ;
          lcArrName, llChckKey, lcAliasName, llGetTree, ;
          lcBrowseFileName, lcBrowseTableDBEngine, lcBrowsePKIndexName, lcBrowseIndexName, lcSeekToKey, ;
          lcPreferenceKey, lcPreferenceSubKey,lcTmpFile,lcFldtoRet
*! B608684,1 MMT 09/10/2008 call gfStyBrw when browse style file in option grid[End]                
*! B038623,1 MAH 10/12/2004 [END]
*! E038142,2 MAH 09/16/2004 [END]

LOCAL llReturnValue,lcAlias
lcAlias = SELECT()
tcBrowseFields = IIF(TYPE('lcBrFields')='C',lcBrFields,'')
lnY1 = IIF(TYPE('lnY1')='N',lnY1,0)
llHalfHeight = lnY1=gnBrHSRow1
tcFor = .F.
*B608546,1 TMI [START] assign the value of the optin lcBrPushB to tcOptions and then use the last in the browse.scx screen to not trim the sent key
*                      in case of fox tables as this causes problems when keying in the browse screen is the keyed field in the second key in the 
*                      current order and the first key has spece in its right
*tcOptions=''
tcOptions = lcBrPushB
*B608546,1 tmi [end  ]

tlSelect=.T.
tcSelectMethod = lcOnSelect
IF !EMPTY(lcOnSelect) AND LEFT(lcOnSelect,1) <> "=" AND TYPE("_SCREEN.ActiveForm") = "O" AND  TYPE("_SCREEN.ActiveForm.PARENT") = "O"
  toSelectObj=_SCREEN.ActiveForm.PARENT
ELSE
  toSelectObj=.F.
  
  *N037635,1 WSH 07/04/2005 Don't remove the passed OnSelect function if it is not a methiod in a FormSet Object. [Start]
  *tcSelectMethod=.F.
  
  *B128950,1 AMH Fix bug of variable not found when browse [Start]
  *IF !(LEFT(tcSelectMethod,1)="=")
  IF (TYPE('tcSelectMethod')#"C") OR !(LEFT(tcSelectMethod,1)="=")
  *B128950,1 AMH [End]
  
    tcSelectMethod=.F.
  ENDIF
  *N037635,1 WSH 07/04/2005 [End]
  
ENDIF  
tcUserShortCut=.F.
tcTempFile=.F.
tcSelField=.F.

*! B608684,1 MMT 09/10/2008 call gfStyBrw when browse style file in option grid[Start]      
tcTempFile= IIF(TYPE('lcTmpFile')='C' AND !EMPTY(lcTmpFile),lcTmpFile, .F.)
tcSelField =IIF(TYPE('lcFldtoRet')='C' AND !EMPTY(lcFldtoRet),lcFldtoRet,.F.)
IF (TYPE('lcFldtoRet')='C' AND !EMPTY(lcFldtoRet)) AND (TYPE('lcTmpFile')='C' AND !EMPTY(lcTmpFile))
  toSelectObj=.F.
ENDIF
*! B608684,1 MMT 09/10/2008 call gfStyBrw when browse style file in option grid[End]      


PRIVATE oBrowse
oBrowse = .Null.
IF TYPE('tcKey')='C'
  tcFor=IIF(ATC('FOR ',tcKey)>0,SUBSTR(tcKey,ATC('FOR ',tcKey)),'')
ENDIF  
tcKey=IIF(!EMPTY(tcFor),SUBSTR(tcKey,1,ATC(tcFor,tcKey)-1),tcKey)        
tcKey = IIF(TYPE('tcKey')='C' AND !EMPTY(tcKey),tcKey,'')


*! E038142,2 MAH 09/16/2004 Add addtional parameters [BEGIN]
*-- DO FORM (oAriaApplication.ScreenHome+"sy\BROWSE") WITH tcBrowseFields,tcBrowseTitle,tcKey,tcFor,tcOptions,tlSelect,;
*--            toSelectObj   ,tcSelectMethod,tcUserShortCut,tcTempFile,tcSelField,;
*--            llHalfHeight,lcFieldsNam,lcArrName,llChckKey,lcAliasName,llGetTree;
*--     TO llReturnValue
*! B038623,1 MAH 10/12/2004 Add Addtional Parmaeters [BEGIN]
*-- DO FORM (oAriaApplication.ScreenHome+"sy\BROWSE") WITH ;
*--         tcBrowseFields, tcBrowseTitle, tcKey, tcFor, tcOptions, tlSelect, ;
*--         toSelectObj, tcSelectMethod, tcUserShortCut, tcTempFile, tcSelField, ;
*--         llHalfHeight, lcFieldsNam, lcArrName, llChckKey, lcAliasName, llGetTree, ;
*--         lcBrowseFileName, lcBrowseTableDBEngine, lcBrowsePKIndexName, lcBrowseIndexName, lcSeekToKey ;
*--         TO llReturnValue
DO FORM (oAriaApplication.ScreenHome+"sy\BROWSE") WITH ;
        tcBrowseFields, tcBrowseTitle, tcKey, tcFor, tcOptions, tlSelect, ;
        toSelectObj, tcSelectMethod, tcUserShortCut, tcTempFile, tcSelField, ;
        llHalfHeight, lcFieldsNam, lcArrName, llChckKey, lcAliasName, llGetTree, ;
        lcBrowseFileName, lcBrowseTableDBEngine, lcBrowsePKIndexName, lcBrowseIndexName, lcSeekToKey, ;
        lcPreferenceKey, lcPreferenceSubKey ;
        TO llReturnValue
*! B038623,1 MAH 10/12/2004 [END]
*! E038142,2 MAH 09/16/2004 Add addtional parameters [END]

SELECT (lcAlias)    
RETURN llReturnValue

*!*************************************************************
*! Name      : gfDataBaseProp
*! Developer : Hesham El-Sheltawi
*! Date      : 11/20/96
*! Purpose   : Return Or Set or Remove User defined property value 
*!             from database
*!*************************************************************
*! Parameters: tcName          && object name in database
*!             tcType          && type of object in database
*!                             && Table,Field,Index,Relation
*!             tcProperty      && User Defined Property name
*!             tcPropertyValue && value of property to be saved
*!*************************************************************
*! Call      : 
*!*************************************************************
*! Returns            : VALUE OF Property value "Different types"
*!*************************************************************
*! Example   : lcVarName=gfDataBaseProp('Get',"syccomp.ccomp_id",'Field','BrowseField')
*!             WILL return from the database the value of the property
*!             called browsefield for the field ccomp_id in the table
*!             syccomp
*!*************************************************************
FUNCTION gfDataBaseProp
lParameters tcDatabaseFunction,tcName,tcType,tcProperty,tcPropertyValue

LOCAL lnCount,lcAlias,lcFieldName,lcTableName,lcDataBase,lcPath,lcRetrunValue
tcName = UPPER(tcName)
tcType = UPPER(tcType)
tcProperty = UPPER(tcProperty)
tcDatabaseFunction = PROP(ALLTRIM(tcDatabaseFunction))

IF tcType='FIELD'
  IF ATC('.',tcName)>0
    lcAlias = SUBSTR(tcName,1,ATC('.',tcName)-1)
    lcFieldName = SUBSTR(tcName,ATC('.',tcName)+1)
    
    *-- MAB 12/08/2002 Detect database errors.
    LOCAL lcOldFErr, llCurrErr
    lcOldFErr = ON("ERROR")
    ON ERROR llCurrErr = .T.
    lcTableName = CURSORGETPROP('Sourcename',lcAlias)
    ON ERROR &lcOldFErr.
    IF llCurrErr
      RETURN .F.
    ENDIF 

    lcTableName = STRTRAN(lcTableName,'.DBF','')
    tcName = lcTableName+'.'+lcFieldName
  ENDIF
ENDIF

DO CASE
  CASE tcType = 'DATABASE'
    IF DBUSED(tcName)
      SET DATABASE TO (tcName)
    ELSE
      RETURN tcName  
    ENDIF
  CASE tcType $ "FIELD,TABLE,VIEW"
    IF tcType = 'FIELD'
      lcDataBase = CURSORGETPROP("DATABASE",lcAlias)
     ELSE 
       lcDataBase = CURSORGETPROP("DATABASE",tcName)
     ENDIF 
     IF !EMPTY(lcDataBase)
       lcPath =''
       IF ATC('\',lcDataBase)>0
         lcPath = SUBSTR(lcDataBase,1,RAT('\',lcDataBase))
         lcDataBase = STRTRAN(lcDataBase,lcPath,'')
         lcDataBase = STRTRAN(lcDataBase,'.DBC','')
       ENDIF
       IF !DBUSED(lcDataBase)
         OPEN DATABASE (lcPath+lcDataBase)  
       ENDIF
       SET DATABASE TO (lcDataBase)     
     ELSE
       RETURN .F.  
     ENDIF  
ENDCASE
DO CASE
  CASE tcDatabaseFunction == 'Dbgetprop'
    lcRetrunValue = DBGetProp(tcName,tcType,tcProperty)  
  CASE tcDatabaseFunction == 'Dbsetprop'
    lcRetrunValue = DBSetProp(tcName,tcType,tcProperty,tcPropertyValue)  
  CASE tcDatabaseFunction == 'Get'
*    lcRetrunValue = sfsGETProp(tcName,tcType,tcProperty)
  CASE tcDatabaseFunction == 'Set'   
*    lcRetrunValue = sfsSetProp(tcName,tcType,tcProperty,tcPropertyValue)  
  CASE tcDatabaseFunction == 'Remove'   
*    lcRetrunValue = sfsRemoveProp(tcName,tcType,tcProperty)  
ENDCASE     
RETURN lcRetrunValue


*:************************************************************************
*: Program file  : GFGENFLT.PRG
*: Program desc. : 
*: For screen    :
*:         System: Aria advantage series
*:         Module: Main system 
*:      Developer: 
*:************************************************************************
*: Calls : 
*:         Procedures :
*:         Functions  : 
*:************************************************************************
*: Passed Parameters  : 
*:************************************************************************
FUNCTION OldGFGENFLT
PARAMETERS lcArray,llFilter
lcquery=''
lcElmSep=','


lcLineFeed=' ' &&+CHR(10)+CHR(13)
lnFltStart=1
DO WHILE (&lcArray[lnFltStart,1]='.OR.' OR EMPTY(&lcArray[lnFltStart,1]));
        AND !lnFltStart=ALEN(&lcArray,1)
  lnFltStart=lnFltStart+1
ENDDO 
lnFltEnd=ALEN(&lcArray,1)
DO WHILE (&lcArray[lnFltEnd,1]='.OR.' OR EMPTY(&lcArray[lnFltEnd,1]));
         AND lnFltEnd>1
  lnFltEnd=lnFltEnd-1
ENDDO 

lnOr=0       
IF lnFltStart>ALEN(&lcArray,1)
  RETURN ''
ENDIF
IF lnFltEnd=ALEN(&lcArray,1)
   lcWhichElm=lcArray+'['+ALLTRIM(STR(ALEN(&lcArray,1)))+',1]'
  IF TYPE(lcWhichElm)<>'C'
    RETURN ''
  ENDIF
  IF &lcArray[ALEN(&lcArray,1),1]='.OR.'
    RETURN ''
  ENDIF
ENDIF
lnCount=lnFltStart

DO WHILE  lnCount<=lnFltEnd
 IF  &lcArray[lnCount,3]='N' AND EMPTY(VAL(&lcArray[lnCount,6])) ;
     AND &lcArray[lnCount,7]='V'
    lnCount=lnCount+1
    LOOP
 ENDIF
IF !EMPTY(ALLTRIM(STRTRAN(STRTRAN(&lcArray[lnCount,6],lcElmSep,'');
   ,IIF(&lcArray[lnCount,3]='D','/',''),''))) OR &lcArray[lnCount,1]='.OR.'
  IF !EMPTY(&lcArray[lnCount,1])
    IF &lcArray[lnCount,1]<>'.OR.'
      lcQuery=lcQuery+lfGetQCond(lnCount,lcArray,llFilter)
    ELSE
      lcQuery= IIF(RIGHT(lcQuery,9)=lcElmSep+' .AND. '+lcElmSep,SUBSTR(lcQuery,1,LEN(lcQuery)-9),lcQuery)
      IF lnOr>0
       lcQuery=lcQuery+' ) '
       lnOr=0
      ENDIF
      ** THIS CONDITION IS ADDED BY HESHAM 3 AUG.    
      DO WHILE lnCount<lnFltEnd-1 AND (EMPTY(ALLTRIM(STRTRAN(STRTRAN(&lcArray[lnCount+1,6],lcElmSep,'');
      ,IIF(&lcArray[lnCount+1,3]='D','/  /',''),''))) OR &lcArray[lnCount+1,1]='.OR.')
        lnCount=lnCount+1
      ENDDO
      IF !EMPTY(ALLTRIM(STRTRAN(&lcArray[lnCount+1,6],lcElmSep,''))) AND !EMPTY(ALLTRIM(lcQuery))
        lcQuery=ALLTRIM(lcQuery)+' '+lcElmSep+' OR '+lcElmSep+'( '
         lnOr=1
      ENDIF   
    ENDIF  
  ENDIF 
ENDIF
lnCount=lnCount+1   
ENDDO
lcQuery= IIF(RIGHT(lcQuery,9)=lcElmSep+' .AND. '+lcElmSep,SUBSTR(lcQuery,1,LEN(lcQuery)-9),lcQuery)
 IF lnOr>0
   lcQuery=lcQuery+' ) '
 ENDIF    
 lcQuery=STRTRAN(lcQuery,lcElmSep+' .AND. '+lcElmSep,lcLineFeed+' AND '+lcLineFeed)
 lcQuery=STRTRAN(lcQuery,lcElmSep+' OR '+lcElmSep,lcLineFeed+' OR '+lcLineFeed) 
RETURN lcQuery


FUNCTION OldlfGetQCond
PARAMETERS lnCount,lcArray,llFilter
lcFiltExp=''
DO CASE
  CASE &lcArray[lnCount,5] = 'Contains'
  
       lcFiltExp=IIF(!&lcArray[lnCount,4],'','!(')+;
                 lfrightGet(&lcArray[lnCount,6],&lcArray[lnCount,3],;
                 &lcArray[lnCount,5],lcElmSep,&lcArray[lnCount,7])+;
                 ' $ '+ALLTRIM(&lcArray[lnCount,1])+' '+;
                 IIF(!&lcArray[lnCount,4],'',' ) ')+lcElmSep+' .AND. '+lcElmSep                       
                 
  CASE &lcArray[lnCount,5] = 'Like' OR &lcArray[lnCount,5] = 'Exactly Like'
  
       lcFiltExp=IIF(&lcArray[lnCount,3]='D',"DTOS(",'')+ALLTRIM(&lcArray[lnCount,1]);
                  +IIF(&lcArray[lnCount,3]='D',")",'')+' '+IIF(!&lcArray[lnCount,4],;
                   IIF(&lcArray[lnCount,5] = 'Like','=','=='),'<>')+' '+;
                  lfrightGet(&lcArray[lnCount,6],&lcArray[lnCount,3],;
                 &lcArray[lnCount,5],lcElmSep,&lcArray[lnCount,7])+lcElmSep+' .AND. '+lcElmSep                
               
  CASE INLIST(&lcArray[lnCount,5],'Greater Than','Less Than','Greater Or Equal',;
              'Less Or Equal')     
        lcOperator=lfGetOper(ALLTRIM(&lcArray[lnCount,5]),!&lcArray[lnCount,4])              
        lcFiltExp=IIF(&lcArray[lnCount,4],'','!(')+;
                IIF(&lcArray[lnCount,3]='D',"DTOS(",'')+ALLTRIM(&lcArray[lnCount,1]);
                  +IIF(&lcArray[lnCount,3]='D',")",'')+' '+lcOperator+' '+;
                  lfrightGet(&lcArray[lnCount,6],&lcArray[lnCount,3],;
                 &lcArray[lnCount,5],lcElmSep,&lcArray[lnCount,7])+IIF(!&lcArray[lnCount,4],'',' ) ')+;
                 lcElmSep+' .AND. '+lcElmSep                              
  CASE &lcArray[lnCount,5] = 'Between'
    IF llFilter
       lcFiltExp=IIF(!&lcArray[lnCount,4],'BETWEEN(','!BETWEEN(')+;
               IIF(&lcArray[lnCount,3]='D',"DTOS(",'')+ALLTRIM(&lcArray[lnCount,1]);
                   +IIF(&lcArray[lnCount,3]='D',")",'')+','+;
                   lfrightGet(&lcArray[lnCount,6],&lcArray[lnCount,3],;
                   &lcArray[lnCount,5],lcElmSep,&lcArray[lnCount,7])+;
                   ')'+lcElmSep+' .AND. '+lcElmSep
    ELSE
         lcFiltExp= IIF(!&lcArray[lnCount,4],'','!(')+;
                    IIF(&lcArray[lnCount,3]='D',"DTOS(",'')+ALLTRIM(&lcArray[lnCount,1]);
                   +IIF(&lcArray[lnCount,3]='D',")",'')+' BETWEEN '+;
                   lfrightGet(&lcArray[lnCount,6],&lcArray[lnCount,3],;
                   &lcArray[lnCount,5],lcElmSep,&lcArray[lnCount,7])+;
                   IIF(!&lcArray[lnCount,4],'',')')+lcElmSep+' .AND. '+lcElmSep    
    ENDIF  
  CASE &lcArray[lnCount,5] = 'In List'
    IF llFilter
       lcFiltExp=IIF(!&lcArray[lnCount,4],'INLIST(','!INLIST(')+;
               IIF(&lcArray[lnCount,3]='D',"DTOS(",'')+ALLTRIM(&lcArray[lnCount,1]);
                   +IIF(&lcArray[lnCount,3]='D',")",'')+','+;
                   lfrightGet(&lcArray[lnCount,6],&lcArray[lnCount,3],;
                   &lcArray[lnCount,5],lcElmSep,&lcArray[lnCount,7])+;
                   ')'+lcElmSep+' .AND. '+lcElmSep
    ELSE
         lcFiltExp= IIF(!&lcArray[lnCount,4],'','!(')+;
                    IIF(&lcArray[lnCount,3]='D',"DTOS(",'')+ALLTRIM(&lcArray[lnCount,1]);
                   +IIF(&lcArray[lnCount,3]='D',")",'')+' IN('+;
                   lfrightGet(&lcArray[lnCount,6],&lcArray[lnCount,3],;
                   &lcArray[lnCount,5],lcElmSep,&lcArray[lnCount,7])+')'+;
                   IIF(!&lcArray[lnCount,4],'',')')+lcElmSep+' .AND. '+lcElmSep    
    ENDIF    
ENDCASE

RETURN lcFiltExp


FUNCTION OldlfGetOper              
PARAMETER lcOperator,llisnot
DO CASE
  CASE lcOperator = 'Greater Than'
     RETURN '>'
  CASE lcOperator = 'Less Than'
     RETURN '<'
  CASE lcOperator = 'Greater Or Equal'
    RETURN '>='
  CASE lcOperator = 'Less Or Equal'
    RETURN '<='
ENDCASE  


FUNCTION OldlfRightGet
PARAMETERS mRightHead,cLeftType,cOperator,lcElmSep,cRightType
lcRetVal=mRightHead
DO CASE
  CASE cRightType='V'
    DO CASE      
      CASE cLeftType $ 'CM'
          IF INLIST(COPERATOR,'Between','In List')
            lcSeper=IIF(!llFilter AND cOperator='Between',' AND ',',')
               lcRetVal='"'+STRTRAN(ALLTRIM(mRightHead),lcElmSep,'"'+lcSeper+'"')+'"'
          ELSE
             RETURN '"'+mrightHead+'"'   &&'"'+ALLTRIM(mrightHead)+'"'   
          ENDIF
      CASE cLeftType = 'N'
            lcSeper=IIF(COPERATOR='Between' AND !llFilter,' AND ',',')
            lcRetVal=STRTRAN(mRightHead,lcElmSep,lcSeper)
         IF EMPTY(lcRetVal)
           lcRetVal='0'
         ENDIF
      CASE cLeftType = 'D'
            IF INLIST(COPERATOR,'Between','In List')
            lcSeper=IIF(!llFilter AND cOperator='Between',' AND ALLTRIM(DTOS(',',ALLTRIM(DTOS(')            
               lcRetVal='ALLTRIM(DTOS({  '+STRTRAN(ALLTRIM(mRightHead),lcElmSep,'  }))'+lcSeper+'{  ')+'  }))'
            ELSE    
              lcRetVal='ALLTRIM(DTOS({  '+ALLTRIM(MRIGHTHEAD)+'  }))'
           ENDIF   
      CASE cLeftType = 'L'
           RETURN ' '+lcRetVal+' '
    ENDCASE  
  CASE cRightType='F'
    lcRetVal=STRTRAN(ALLTRIM(mRightHead),lcElmSep,',')
ENDCASE  
IF INLIST(cOperator,'Between','In List') AND EMPTY(ALLTRIM(mRightHead))
    lcSeper=IIF(!llFilter AND cOperator='Between',' AND ',',')
    lcRetVal=lcRetVal+lcSeper+lcRetVal
 ENDIF
RETURN lcRetVal



*!*************************************************************
*! Name      : GFCPTOP
*! Developer : Hesham El-Sheltawi
*! Date      : 07/08/98
*! Purpose   : 
*!*************************************************************
*! Parameters: 
*!*************************************************************
*! Called by : MENU BAR FIRST
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example   : 
*!*************************************************************
*
FUNCTION GFCPTOP
oAriaApplication.oToolBar.cmdTop.Click

*!*************************************************************
*! Name      : GFCPBTTM
*! Developer : Hesham El-Sheltawi
*! Date      : 07/08/98
*! Purpose   : 
*!*************************************************************
*! Parameters: 
*!*************************************************************
*! Called by : MENU BAR Bottom
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example   : 
*!*************************************************************
*
FUNCTION GFCPBTTM
oAriaApplication.oToolBar.cmdend.Click

*!*************************************************************
*! Name      : GFCPNEXT
*! Developer : Hesham El-Sheltawi
*! Date      : 07/08/98
*! Purpose   : 
*!*************************************************************
*! Parameters: 
*!*************************************************************
*! Called by : MENU BAR next
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example   : 
*!*************************************************************
*
FUNCTION GFCPNEXT
oAriaApplication.oToolBar.cmdNext.Click

*!*************************************************************
*! Name      : GFCPPRVIS
*! Developer : Hesham El-Sheltawi
*! Date      : 07/08/98
*! Purpose   : 
*!*************************************************************
*! Parameters: 
*!*************************************************************
*! Called by : MENU BAR previous
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example   : 
*!*************************************************************
*
FUNCTION GFCPPRVIS
oAriaApplication.oToolBar.cmdPrev.Click

*!*************************************************************
*! Name      : GFVCPNEW
*! Developer : Hesham El-Sheltawi
*! Date      : 07/08/98
*! Purpose   : 
*!*************************************************************
*! Parameters: 
*!*************************************************************
*! Called by : MENU BAR New
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example   : 
*!*************************************************************
*
FUNCTION GFVCPNEW
oAriaApplication.oToolBar.cmdadd.Click


*!*************************************************************
*! Name      : GFVCPPRINT
*! Developer : Hesham El-Sheltawi
*! Date      : 07/08/98
*! Purpose   : 
*!*************************************************************
*! Parameters: 
*!*************************************************************
*! Called by : MENU BAR Print
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example   : 
*!*************************************************************
*
FUNCTION GFVCPPRINT
*oAriaApplication.oToolBar.cmdprint.Click
oAriaApplication.oToolBar.cmdprint.Click("M")    && MAB 05/01/2003
*-- 

*!*************************************************************
*! Name      : GFCPEDIT
*! Developer : Hesham El-Sheltawi
*! Date      : 07/08/98
*! Purpose   : 
*!*************************************************************
*! Parameters: 
*!*************************************************************
*! Called by : MENU BAR Edit
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example   : 
*!*************************************************************
*
FUNCTION GFCPEDIT
oAriaApplication.oToolBar.cmdedit.Click

*!*************************************************************
*! Name      : GFCPDELETE
*! Developer : Hesham El-Sheltawi
*! Date      : 07/08/98
*! Purpose   : 
*!*************************************************************
*! Parameters: 
*!*************************************************************
*! Called by : MENU BAR Delete
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example   : 
*!*************************************************************
*
FUNCTION GFCPDELETE
oAriaApplication.oToolBar.cmddelete.Click

*!*************************************************************
*! Name      : GFCPBROWS
*! Developer : Hesham El-Sheltawi
*! Date      : 07/08/98
*! Purpose   : 
*!*************************************************************
*! Parameters: 
*!*************************************************************
*! Called by : MENU BAR Browse
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example   : 
*!*************************************************************
*
FUNCTION GFCPBROWS
oAriaApplication.oToolBar.cmdfind.Click

*!*************************************************************
*! Name      : GFCHNGORDR
*! Developer : Hesham El-Sheltawi
*! Date      : 07/08/98
*! Purpose   : 
*!*************************************************************
*! Parameters: 
*!*************************************************************
*! Called by : MENU BAR Order
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example   : 
*!*************************************************************
*
FUNCTION GFCHNGORDR

*!*************************************************************
*! Name      : GFSETFILTR
*! Developer : Hesham El-Sheltawi
*! Date      : 07/08/98
*! Purpose   : 
*!*************************************************************
*! Parameters: 
*!*************************************************************
*! Called by : MENU BAR Filter
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example   : 
*!*************************************************************
*
FUNCTION GFSETFILTR


*!*************************************************************
*! Name      : GPRECHIST
*! Developer : Hesham El-Sheltawi
*! Date      : 07/08/98
*! Purpose   : 
*!*************************************************************
*! Parameters: 
*!*************************************************************
*! Called by : MENU BAR Record History
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example   : 
*!*************************************************************
*! B608156,1 SSH 07/29/2007 Allow the user to unlock record from history options
FUNCTION GPRECHIST
PRIVATE lnOldAls,lnBaseTabel,lnOldDataSession,llReturn

llReturn=.F.
lnOldAls=SELECT(0)
lnBaseTabel = _screen.ActiveForm.Parent.nworkarea
SET DATASESSION TO _screen.ActiveForm.Parent.DatasessionID

SELECT(lnBaseTabel)
IF TYPE('Cedit_user') = 'U' .AND. TYPE('dedit_date') = 'U' .AND. TYPE('cedit_time') = 'U' AND TYPE('lLok_Stat') = 'U'
  =gfModalGen("INM00050B00000","DIALOG")
  llReturn = .T.
ELSE

  lcUser = IIF(EMPTY(Cedit_user),cAdd_user,Cedit_user)
  ldDate = IIF(EMPTY(dedit_date),dAdd_Date,dedit_date)
  ldTime = IIF(EMPTY(cedit_time),cAdd_Time,cedit_time)
  *lcUser = IIF(EMPTY(cLok_User),lcUser,cLok_User)
  IF EMPTY(cAdd_user) .OR. EMPTY(dAdd_Date) .OR. EMPTY(cAdd_Time)
    =gfModalGen("INM00051B00000","DIALOG")
    llReturn = .T.
  ENDIF
ENDIF
IF !llReturn
  lnRemResult = oAriaApplication.remotesystemdata.execute("Select * from syuuser where cUser_ID='"+lcUser+"'",'',"syuuser","",oAriaApplication.SystemConnectionString,3,"",_screen.ActiveForm.Parent.DatasessionID)
  IF lnRemResult>=1
    LOCATE
  ENDIF
  SELECT(lnBaseTabel)
  DO FORM (oAriaApplication.ScreenHome+"sm\smrechist.scx") WITH syuuser.cUsr_Name,ldTime,ldDate,lLok_Stat,lnBaseTabel
  IF USED('SYUUSER')
    USE IN SYUUSER
  ENDIF
ENDIF
SET DATASESSION TO 
SELECT(lnOldAls)


*!*************************************************************
*! Name      : gfCpSave
*! Developer : Hesham El-Sheltawi
*! Date      : 07/08/98
*! Purpose   : 
*!*************************************************************
*! Parameters: 
*!*************************************************************
*! Called by : MENU BAR Save
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example   : 
*!*************************************************************
*
FUNCTION gfCpSave
oAriaApplication.oToolBar.cmdAdd.Click

*!*************************************************************
*! Name      : gfCpClose
*! Developer : Hesham El-Sheltawi
*! Date      : 07/08/98
*! Purpose   : 
*!*************************************************************
*! Parameters: 
*!*************************************************************
*! Called by : MENU BAR Close  / Cancel
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example   : 
*!*************************************************************
*
FUNCTION gfCpClose
IF oAriaApplication.oToolBar.EditMode
  oAriaApplication.oToolBar.cmdEdit.Click
ELSE
  oAriaApplication.oToolBar.cmdexit.Click
ENDIF  

*!*************************************************************

*!*****************************************************************************************
*! Modification:
*! E037885,4 MAH 02/17/2005 Remove the host form.
*!*****************************************************************************************
FUNCTION GFTBARONOF
lnBar = BAR()
lcPop = POPUP()
oAriaApplication.UserUseToolBar = !oAriaApplication.UserUseToolBar
oAriaApplication.oToolBar.Visible = oAriaApplication.UserUseToolBar

*! E037885,4 MAH 02/17/2005 No need [BEGIN]
*-- oAriaApplication.oListView.ArrangeHeight()
*! E037885,4 MAH 02/17/2005 [END]

SET MARK OF BAR lnBar OF (lcPop) oAriaApplication.UserUseToolBar
*!*************************************************************

FUNCTION gfAbotAria
DO FORM syabout
*oAriaApplication.RunProg("SYABOUT")


*!*************************************************************
FUNCTION gfGetName
lParameters lcTagUser
LOCAL lcNames,laTo,laVal,lnCount
DIME laTo[1,1]
STORE '' TO laTo,lcNames
IF !EMPTY(lcTagUser)
  =gfSubStr(lcTagUser,@laTo,'|')
  FOR lnCount = 1 TO ALEN(laTo,1)
    DIMEN laVal[1,1]
    IF !EMPTY(laTo[lnCount,1])
      =gfSubStr(laTo[lnCount,1],@laVal,'~')
      lcNames = lcNames + IIF(EMPTY(lcNames),'',';')+ALLT(laVal[1,1])+'['+ALLT(laVal[1,3])+'] '
    ENDIF 
  ENDFOR  
ENDIF  
RETURN lcNames

*!*************************************************************
FUNCTION gfGetAttch
LPARAMETERS lcMsgID
PRIVATE lnAlias,lcAttach
lnAlias = SELECT()
SELECT MTMSATCH
lcAttach = ""
IF SEEK(lcMsgID,'MTMSATCH')
  SCAN REST WHILE cMsgID = lcMsgID
    lcAttach = lcAttach + cattchfile + CHR(13)+CHR(10)
  ENDSCAN  
ENDIF
SELECT (lnAlias)
RETURN lcAttach

*!*************************************************************
FUNCTION UPDDATE
DO Form syUpDate

*!*************************************************************
*! Name      : gfAdd_Info
*! Developer : Yasser Saad Ibrahime
*! Date      : 1993-1995 
*! Purpose   : To add audit information to any file
*!*************************************************************
*! Calls     : 
*!      Called by: ARIA3.PRG                
*!      Called by: GFSCRINI()               (function  in ARIA3.PRG)
*!      Called by: GFSTATIC()               (function  in ARIA3.PRG)
*!      Called by: GFSEQUENCE()             (function  in ARIA3.PRG)
*!      Called by: GFCPSAVE()               (function  in ARIA3.PRG)
*!      Called by: GFSUSRPRG()              (function  in ARIA3.PRG)
*!          Calls: GFGETTIME()              (function  in ARIA3.PRG)
*!*************************************************************
*! Passed Parameters  : File name to add to
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : 
*!*************************************************************
* Update add user,date and time
*:->
FUNCTION gfAdd_Info
LPARAMETERS lcFileName, oForm

*! B608644,1 MMT 08/05/2008 Fix bug of error in gfAddUserInfo due data session change[Start]
*RETURN oAriaApplication.AddUserInformation(lcFileName, oForm)
lnCurrData  = SET("Datasession")
RETURN oAriaApplication.AddUserInformation(lcFileName, oForm,lnCurrData)
*! B608644,1 MMT 08/05/2008 Fix bug of error in gfAddUserInfo due data session change[End]


FUNCTION gfErrorTrap
LPARAMETERS nError, cMethod, nLine

lcError   = IIF(TYPE("nError")  = "N", ALLTRIM(STR(nError)), ALLTRIM(STR(ERROR())))
lcMethod  = IIF(TYPE("cMethod") = "C", cMethod, "")
lcLine    = IIF(TYPE("nLine")   = "N", ALLTRIM(STR(nLine)), "")

lcNewLine = CHR(13)+CHR(10)
MessageBox("An error has occuerd..."         + lcNewLine + lcNewLine +;
           "Error Number : " + lcError    + lcNewLine +;
           "Error Message: " + MESSAGE()  + lcNewLine +;
           "Method 		: " + lcMethod   + lcNewLine +;
           "Line Number	: " + lcLine     + lcNewLine +;
           "Line Code	: " + MESSAGE(1) + lcNewLine )


CLEAR ALL
CLOSE ALL
QUIT

*!************************************************************
*! Name      : gfDoTriger
*! Developer : WAB - Walid A. Wahab
*! Date      : 04/22/2002
*! Purpose   : Function to control any triggers found in the
*!             triggers file, customized processes and workflow
*!             server requests.
*!*************************************************************
*! Calls              : None.
*!*************************************************************
*! Passed Parameters  : 1) lcProgName, Object ID.
*!                      2) lcEvent, Event ID.
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example            :  =gfDoTriger()
*!*************************************************************
*E301903,1 WAB 
*!*************************************************************
*
FUNCTION gfDoTriger
PARAMETERS lcProgName , lcEvent

PRIVATE lnOldAlias , lcProgToDo , laParamExp , laParam , lcParmStr ,;
        lnCount    , llReturn , llIsOpen
llReturn = .T.
*-- If any of the parameters is not passed or passed incorrectly 
IF TYPE('lcProgName') <> 'C' .OR. EMPTY(lcProgName) .OR.;
   TYPE('lcEvent') <> 'C' .OR. EMPTY(lcEvent)
  RETURN
ENDIF

*-- Save the old alias
lnOldAlias = SELECT(0)

*-- Open the Trigger file if it was not opened
*-- Hesham (Start)
*-- Use trigger file remotely in temprory alias
*!*	llIsOpen = .F.
*!*	IF !USED('SYCTRIGG')
*!*	    SELECT 0
*!*	    *** Open the
*!*	    USE (oARiaApplication.SysPath+"SYCTRIGG")
*!*	    SET ORDER TO 1
*!*	ENDIF

*!*	SELECT SYCTRIGG
lnRemResult = oAriaApplication.remotesystemdata.execute("Select * from SYCTRIGG where cAPObjNam='"+PADR(lcProgName , 10)+"' AND cEvent_ID ='"+PADR(lcEvent , 10)+"'",'',"SYCTRIGGTMP","",oAriaApplication.SystemConnectionString,3,'',SET("DATASESSION"))
IF lnRemResult>=1
  LOCATE
*-- Hesham (End)  

*-- If there is triggers for this Object/Event
*IF SEEK(PADR(lcProgName , 10) + PADR(lcEvent , 10))
  IF FOUND()
  *-- Scan loop to scan the Object/Event triggers
    SCAN REST;
        WHILE cAPObjNam + cEvent_ID = PADR(lcProgName , 10) +;
              PADR(lcEvent , 10)
    
      *-- Get the name of the program that should be executed
      lcProgToDo = cTrig_ID
      *-- Initialize the parameter string variable
      lcParmStr  = ''
    
      *-- Restore the old alias to be able to evaluate the parameter
      *-- expressions properly
      SELECT (lnOldAlias)
    
      *-- If there is one or more parameters that should be passed to the
      *-- program
      IF !EMPTY(SYCTRIGGTMP.mParmExpr)
      
        *-- Get the parameter expressions
        DIMENSION laParamExp[OCCURS('~' , SYCTRIGGTMP.mParmExpr) + 1]
        =gfSubStr(SYCTRIGGTMP.mParmExpr , @laParamExp , '~')
      
        *-- Initialize the parameters array
        DIMENSION laParam[ALEN(laParamExp , 1)]
        laParam = ""
      
        *-- Get the parameters values that will be passed to the program
        FOR lnCount = 1 TO ALEN(laParamExp , 1)
          laParam[lnCount] = EVALUATE(laParamExp[lnCount])
          lcParmStr = lcParmStr + IIF(lnCount = 1 , '' , ' , ') +;
                      'laParam[' + ALLTRIM(STR(lnCount)) + ']'
        
        ENDFOR    && End of FOR lnCount = 1 TO ALEN(laParamExp , 1)
      ENDIF    && End of IF !EMPTY(SYCTRIGG.mParmExpr)
      lcParmStr = "''"+IIF(!EMPTY(lcParmStr),",","")+lcParmStr && Wael
      *-- If custom process
      *Hassan [Begin]
      lcOldPath = FullPath('')
      lcNewPath = SubStr(oAriaApplication.ApplicationHome,1,Rat("\",oAriaApplication.ApplicationHome,2))
      CD (lcNewPath) 
      *Hassan [End]
      IF SYCTRIGGTMP.cActvTyp = 'C'
        *-- Call the program and get the returned value
        llReturn = &lcProgToDo(&lcParmStr)
      ENDIF    && End of IF SYCTRIGG.cActvTyp = 'C'
      *Hassan [Begin]
      CD (lcOldPath) 
      *Hassan [End]
      SELECT SYCTRIGGTMP
    ENDSCAN    && End of SCAN REST WHILE cAPObjNam + cEvent_ID = ...
  ENDIF

ELSE    &&  *In case the process doesn't exist.[START]
  llReturn = .F.
  
ENDIF    && End of IF SEEK(PADR(lcProgName , 10) + PADR(lcEvent , 10))
IF USED("SYCTRIGGTMP")
  USE IN SYCTRIGGTMP
ENDIF
*-- Restore the old alias
SELECT (lnOldAlias)

RETURN (llReturn)


*****************************************************************************
* PROC: GLDIST.PRG
* DESC: A GLOBAL PROCEDURE  THAT WILL BE  CALLED FROM  SEVERAL PLACES WITH
*       DIFFERENT PARAMETERS, DEPENDING ON THE  PARAMETERS THE PROGRAM WILL
*       UPDATE THE G/L DISTRIBUTION FILE WITH THE CORRESPONDING G/L ACCOUNT
*       AND AMOUNT. 
* DATE: 12/24/93
* AUTH: WAEL ALY MOHAMED
* NOTE: PARAMETERS USED:-
*       - _GLLINK  : GL LINK CODE -> THE LINK CODE USED FOR POSTING.
*       - _CATGKEY : CATEGORY KEY
*       - _AMOUNT  : AMOUNT       
*                    DEBITS ARE POSITIVE, CREDITS ARE NEGATIVE.
*       - _TRANTYP : TRANSACTION TYPE :-
*                    'IN' -> INVOICE
*                    'VI' -> VOID INVOICE
*                    'CR' -> CASH RECEIPT
*                    'CA' -> CREDIT ADJUSTMENT                         
*                    'DA' -> DEBIT ADJUSTMENT                         
*                    'RM' -> RETURN MERCHANDISE
*                    'VR' -> VOID RETURN
*                    'IP' -> INVENTORY PHYSICAL
*                    'MA' -> MATERIAL INVENTORY ADJUSTMENT
*                    'MP' -> MATERIAL INVENTORY PHYSICAL
*                    'IA' -> INVENTORY ADJUSTMENT
*                    'PO' -> P/O RECEIVING
*                    'MO' -> MATERIAL P/O RECEIVING
*                    'CT' -> C/T RECEIVING
*                    'ZE' -> ZERO OUT STOCK
*                    'NL' -> NON MAT. LIABILITY
*                    'JC' -> JOB COST CLOSING ADJ
*                    'RO' -> MATERIAL P/O RECEIVING
*                    'RS' -> C/T RECEIVING
*                    'MM' -> ZERO OUT STOCK
*                    'EX' -> EXCHANGE RATE DIFFERENCE  
*                    'KO' -> KEY OFF
*      - _TRANNO   : TRANSACTION NUMBER.
*      - _TRANDAT  : TRANSACTION DATE.
*      - _NFILE    : NAME OF FILE WILL BE USED. 
*      - _FYEAR    : TRANSACTION FISCAL YEAR.
*      - _PRDID    : PERIOD ID.
*****************************************************************************
PROCEDURE GLDIST
PARAMETERS _GLLINK,_CATGKEY,_AMOUNT,_TRANTYP,_TRANNO,_TRANDAT,_FYEAR,_PRDID,;
           _NFILE,lcGlAct, lcCurrCode, lnCurrUnit, lnExRate 

PRIVATE XGLACNT, XTRANDESC, lcAcntType, lcExRSin, lcUntSin, lnEqvAmnt 

*B803032,1 BWA 14/02/2000 [START]
* Fix the bug of Period & year fields in GL distribution file are empty,So there is no way 
* to release those entries unless we are replace in those fields manually.
IF EMPTY(_FYEAR) OR EMPTY(_PRDID)
  =CheckPrd(_TRANDAT,'_FYEAR','_PRDID','_TRANTYP' , .T.)
  IF EMPTY(_FYEAR) OR EMPTY(_PRDID)
    lcYearPeriod = "YYYY,PP"
    *--This Form return valid year and period.
    DO FORM (oAriaApplication.ScreenHome + 'GL\GLDDATE') TO lcYearPeriod
    _FYEAR = ALLTRIM(SUBSTR(lcYearPeriod,1,ATC(",",lcYearPeriod)-1))
    _PRDID = ALLTRIM(SUBSTR(lcYearPeriod,ATC(",",lcYearPeriod)+1))
  ENDIF
ENDIF
*B803032,1 BWA 14/02/2000 [END]

*E301210,1 ASH 04/27/99 (Begin) Don't create GL entries for some transactions 
*E301210,1                      due to the new parameter M_GL_COST.
llGlCost = ALLTRIM(gfGetMemVar('M_GL_COST',oAriaApplication.ActiveCompanyID))='Y'

IF !llGlCost AND _CATGKEY $ '006,007,008,011,012,013,015,016,017,018,019,021,022,023,024,025,026,027'
  RETURN
ENDIF
*E301210,1 ASH 04/27/99 (End)
*** OPEN GL_LINK FILE TO GET G/L ACCOUNT FOR THIS CATEGORY/GL LINK CODE

IF _AMOUNT = 0
  RETURN
ENDIF  

*E300325,1 If the currency code is not passed, or
*E300325,1 If the currency code is the base currency, or
*E300325,1 If the category key of the transaction is either,
*E300325,1   '006' : 'FINISHED GOODS INVENTORY'
*E300325,1    or
*E300325,1   '008' : 'COST OF GOODS' 
*E300325,1 Default the currency fields to the base currency values.
IF EMPTY(lcCurrCode) .OR. INLIST(_CATGKEY, '006', '008');
  .OR. lcCurrCode = oAriaApplication.BaseCurrency
  lcCurrCode = oAriaApplication.BaseCurrency
  lnCurrUnit = 1
  lnExRate   = 1
  lnEqvAmnt  = _AMOUNT
ELSE
  *E300325,1 If either of the exchange rate or the currency unit
  *E300325,1 is not greater than 0, return .F., otherwise, calculate
  *E300325,1 as follows. 
  IF lnExRate > 0 .AND. lnCurrUnit > 0
    *E300325,1 Get the exchange rate sign for the curreny code
    lcUntSin = ''
    lcExRSin = gfGetExSin(@lcUntSin, lcCurrCode)
    *E300325,1 Get the currency unit sign for the curreny code
    lnEqvAmnt  = ROUND(_AMOUNT &lcExRSin lnExRate &lcUntSin lnCurrUnit, 2)
  ELSE
    RETURN .F.
  ENDIF  
ENDIF
*E300325,1 end.

=gfOpenFile(oAriaApplication.DataDir+'GL_LINK','GL_LINK','SH')

SELE GL_LINK  
*-- 03/29/94 WAM 
*-- If the link code not found (Zap the file for example), default to 'DEF'

*E300592,1 Increase the link_Code field to be 6 characters
*IF !SEEK(_GLLINK+_CATGKEY)    
*  SEEK('DEF'+_CATGKEY)
*ENDIF   
IF !SEEK(PADR(_GLLINK,6)+_CATGKEY)    
  SEEK('DEFDEF'+_CATGKEY)
ENDIF   
*E300592,1 (End)

XGLACNT = IIF(EMPTY(lcGlAct),GLACNT,lcGlAct)

*-- WAM 03/29/94
*-- Get Account Type

DO CASE
  CASE _CATGKEY = '001'   && Account Receivable
    *-- Control Account
    lcAcntType = 'C'
  CASE _CATGKEY = '002'   && Cash Receipts
    *-- Distribition Account
    lcAcntType = 'D'  
  CASE _CATGKEY = '003'   && Sales Revenue
    *-- Control Account
    lcAcntType = 'C'  
  CASE _CATGKEY = '004'   && Freight
    *-- Control Account
    lcAcntType = 'C'  
  CASE _CATGKEY = '005'   && Discount
    *-- Distribition Account
    lcAcntType = 'D'  
  *-- WAM 04/20/94  
  *-- Add category key for material inventory control  
  CASE _CATGKEY = '006' .OR. _CATGKEY = '015'  && Inventory Control
    *-- It is a Control account if the inventory decrease, 
    *-- and a distribution account if the inventory increase.
    lcAcntType = IIF(_AMOUNT < 0 , 'C', 'D')
  *-- WAM 04/20/94
  *-- Add category key for material inventory adjustment
  CASE _CATGKEY = '007' .OR. _CATGKEY = '016'  && Inventory Adjustments
    *-- It is a distribution account if the inventory decrease, 
    *-- and a Control account if the inventory increase. 
    lcAcntType = IIF(_AMOUNT < 0 , 'D', 'C')
  CASE _CATGKEY = '008'   && Cost of Goods
    *-- It is a distribution account if the inventory decrease, 
    *-- and a Control account if the inventory increase. 
    lcAcntType = IIF(_AMOUNT < 0 , 'D', 'C')
  CASE _CATGKEY = '009'   && Credit Adjustments
    *-- Distribition Account
    lcAcntType = 'D'  
  CASE _CATGKEY = '010'   && Debit Adjustments
    *-- Distribition Account
    lcAcntType = 'D'  
  CASE _CATGKEY = '011'   && Return Merchandise
    *-- Distribition Account
    lcAcntType = 'D'  
  *-- WAM 04/20/94  
  *-- Add category key for material P/O clearing
  CASE _CATGKEY = '012' .OR. _CATGKEY = '017'  && P/O Clearing
    *-- It is a distribution account if the inventory decrease, 
    *-- and a Control account if the inventory increase. 
    lcAcntType = IIF(_AMOUNT < 0 , 'D', 'C')
  CASE _CATGKEY = '013'   && C/T Clearing
    *-- It is a distribution account if the inventory decrease, 
    *-- and a Control account if the inventory increase. 
    lcAcntType = 'C'
  CASE _CATGKEY = '014'   && Sales Tax Liability
    *-- Control Account
    lcAcntType = 'C'
  CASE _CATGKEY = '018'   && Non material cost liability
    *-- Control Account
    lcAcntType = 'D'    
  CASE _CATGKEY = '019'   && Cost of goods variance
    *-- Distribution Account
    lcAcntType = 'D'    
  CASE _CATGKEY = '020'   && Return Merchandise
    *-- Distribution Account
    lcAcntType = 'C'    
  *E100219,9 WAM 07/04/95 Add new category key '021' in the GL_CATG for
  *E100219,9              Cost of Material Variance.
  CASE _CATGKEY = '021'   && Cost of material variance
    *-- Distribution Account
    lcAcntType = 'D'    
  CASE _CATGKEY = '022'   && Cost of goods variance 1
    *-- Distribution Account
    lcAcntType = 'D'    
  CASE _CATGKEY = '023'   && Cost of goods variance 2
    *-- Distribution Account
    lcAcntType = 'D'    
  CASE _CATGKEY = '024'   && Cost of goods variance 3
    *-- Distribution Account
    lcAcntType = 'D'    
  CASE _CATGKEY = '025'   && Cost of goods variance 4
    *-- Distribution Account
    lcAcntType = 'D'    
  *B603573,1 SSH 16/04/00 Add New Categ key in GlDist Procedure. [Begin]
  CASE _CATGKEY = '026'   && Cost of goods variance 5
    lcAcntType = 'D'
  *B603573,1 SSH 16/04/00 Add New Categ key in GlDist Procedure. [End]

  *B608402,1 SSH 16/04/00 Add New Categ key in GlDist Procedure. [Begin]
  CASE _CATGKEY = '027'   && Cost of goods variance 6
    lcAcntType = 'D'
  CASE _CATGKEY = '028'   && Cost of goods variance 7
    lcAcntType = 'D'
  *B608402,1 SSH 16/04/00 Add New Categ key in GlDist Procedure. [End]
  
  *: E302618,1 MMT 06/17/2009 Add New GL Categories 029,030 For HST,PST Taxes[Start]
  CASE _CATGKEY = '029'   && PST TAX LIABILITIY
    lcAcntType = 'C'
  CASE _CATGKEY = '030'   && HST TAX LIABILITIY
    lcAcntType = 'C'
  *: E302618,1 MMT 06/17/2009 Add New GL Categories 029,030 For HST,PST Taxes[End]
  

ENDCASE

*** GET DESCRIBTION OF THIS TRANSACTION TYPE

*-- WAM 04/20/94
*-- Add three transaction types
*-- 'MP', 'MA' and 'MO' for material inventory physical, inventory adjustment
*-- and P/O receiving.

DO CASE
  CASE _TRANTYP = 'IN' 
    XTRANDESC = 'INVOICE             '
  CASE _TRANTYP = 'VI' 
    XTRANDESC = 'VOID INVOICE        '
  CASE _TRANTYP = 'CR' 
    XTRANDESC = 'CASH RECEIPT        '
  CASE _TRANTYP = 'CA' 
    XTRANDESC = 'CREDIT ADJUSTMENT   '
  CASE _TRANTYP = 'DA' 
    XTRANDESC = 'DEBIT ADJUSTMENT    '                         
  CASE _TRANTYP = 'RM' 
    XTRANDESC = 'RETURN MERCHANDISE  '
  CASE _TRANTYP = 'VR' 
    XTRANDESC = 'VOID RETURN         '
  CASE _TRANTYP = 'IP' 
    XTRANDESC = 'INVENTORY PHYSICAL  '
  CASE _TRANTYP = 'IA' 
    XTRANDESC = 'INVENTORY ADJUSTMENT'
  CASE _TRANTYP = 'MP' 
    XTRANDESC = 'MATERIAL INV. PHYSI.'
  CASE _TRANTYP = 'MA' 
    XTRANDESC = 'MATERIAL INV. ADJUS.'
  CASE _TRANTYP = 'PO' 
    XTRANDESC = 'P/O RECEIVING       '
  CASE _TRANTYP = 'MO' 
    XTRANDESC = 'MATERIAL P/O RECEIV.'
  CASE _TRANTYP = 'CT' 
    XTRANDESC = 'C/T RECEIVING       '
  CASE _TRANTYP = 'ZE' 
    XTRANDESC = 'ZERO OUT STOCK      '
  *-- ARH 11/10/94
  CASE _TRANTYP = 'NL'
    XTRANDESC = 'NON MAT. LIABILITY  '
  *B603862,1 Modify 'JC' type description [Begin] 
  CASE _TRANTYP = 'JC'
    *XTRANDESC = 'JOB COST CLOSING ADJ'
    XTRANDESC = 'P/O JOB COST CLOSING'
  *B603862,1 Modify 'JC' type description [End] 
  *-- END  ARH  11/10/94
  *E100219,9 WAM 07/04/95 Add new type codes for receiving materials & styles
  *N100219,4              from operation
  *N000016,6 WAM 07/04/95 Add new type code for M.F.G. order receivin.
  CASE _TRANTYP = 'RO' 
    XTRANDESC = 'MATERIAL OP. RECEIVE'
  CASE _TRANTYP = 'RS' 
    XTRANDESC = 'STYLE OP. RECEIVE   '
  CASE _TRANTYP = 'MM' 
    XTRANDESC = 'RECEIVE M.F.G. ORDER'
  *E300344,2 Add new transaction type for Differences in Exchange rate
  CASE _TRANTYP = 'EX' 
    XTRANDESC = 'EX. RATE DIFFERENCES'
  CASE _TRANTYP = 'KO' 
    XTRANDESC = 'KEY OFF'
  *B603504,1 (Begin) Add a new transaction type for Closing material MFG and Closing Material PO.
  CASE _TRANTYP = 'MC' 
    XTRANDESC = 'MATERIAL JOB ClOSING'
  *B603504,1 (End)
  *B603862,1 Add new type for the Manufacturing closing cost sheet [Begin] 
  CASE _TRANTYP = 'JP'
    XTRANDESC = 'C/T JOB COST CLOSING'
  *B603862,1 Add new type for the Manufacturing closing cost sheet [End] 
  *B603983,1 Add new type for Inventory Locking [Begin] 
  CASE _TRANTYP = 'LK'
    XTRANDESC = 'INVENTORY LOCKING'
  *B603983,1 Add new type for Inventory Locking [End] 
ENDCASE

***
SELECT &_NFILE
APPEND BLANK

REPLACE CATG_KEY   WITH _CATGKEY  ,;
        TRAN_TYPE  WITH _TRANTYP  ,;
        TRAN_NO    WITH _TRANNO   ,;
        nGlAmount  WITH _AMOUNT   ,;
        TRAN_DATE  WITH _TRANDAT  ,;
        GLACCOUNT  WITH XGLACNT   ,;
        TRAN_DESC  WITH XTRANDESC ,;
        GLPERIOD   WITH _PRDID    ,;
        GLFYEAR    WITH _FYEAR    ,;
        GLACNTTYPE WITH lcAcntType,;
        cCurrCode  WITH lcCurrCode,;
        nCurrUnit  WITH lnCurrUnit,;
        nExRate    WITH lnExRate  ,;
        nEqvAmnt   WITH lnEqvAmnt

=gfAdd_Info(_NFILE)
RETURN
********************
*** EOF of GLDIST
********************

*!*************************************************************
*! Name      : gfGetMemVar
*! Developer : Hesham El-Sheltawi
*! Date      : 10/05/95
*! Purpose   : Return Variable(s) for company settings from SETUP
*!*************************************************************
*! Parameters: lcArray   && variable to restore
*!                       && OR one dimension array to restore the variable(s)
*!                       name with the same variable name
*!                       && OR two dimension array to restore the variable(2)
*!                       in column 1 into variable names in column 2
*!             lcCompID  &&company id to get its settings
*!*************************************************************
*! Called by : 
*!*************************************************************
*! Returns            : VALUE OF VARIABLE OR no of variables restored
*!*************************************************************
*! Example   : lcVarName=gfGetMemVar('LLMULCURR ','01')
*!             WILL return from the sycsetup file the setting
*!             value for company 01 the variable called "LLMULCURR "
*!*************************************************************
*
FUNCTION gfGetMemVar
PARAMETERS lcArray,lcCompID
*WAIT '1 '+lcArray WIND
*WAIT '2 '+lcCompID WIND
PRIVATE lnAliasNo,llCUsedBy,llArrayORvar,llTwoDimen,lcSetupTag,;
        lcConfgTag,lnRetCount,lcOnErr,llError,laVarArr,llUseSycC
*B601818,1  Get company path
PRIVATE lcCompDir, lcSetPath, llReUsedBy, lnCurTag, lnCurRec, llUseComp
*B601818,1  end

lnAliasNo=SELECT()
llUsedBy  = .F.
llCUsedBy = .F.
llSUsedBy = .F.
llUseSycC = .F.
lcCompID  = IIF(TYPE('lcCompId')<>'C',oAriaApplication.ActiveCompanyID,lcCompID)
*B601818,1  Get company path
lcCompDir  = oAriaApplication.DataDir
IF oAriaApplication.ActiveCompanyID <> lcCompID
 *-- Hesham (Start)  
*!*		llUseComp = gfOpenFile(oAriaApplication.SysPath+"SYCCOMP",'CCOMP_ID')
*!*		lcCompDir  = IIF(SEEK(lcCompID, 'SYCCOMP'), gfGetDataDir(ALLTRIM(SYCCOMP.cCom_DDir)), oAriaApplication.DataDir)
	lnRemResult = oAriaApplication.remotesystemdata.execute("Select * from syccomp where cComp_ID='"+lcCompID+"'",'',"CompFile","",oAriaApplication.SystemConnectionString,3,"",SET("DATASESSION"))
	IF lnRemResult>=1
    LOCATE
  ENDIF  
  IF lnRemResult>=1 AND FOUND()
  *-- Hesham (End)  
    *B131801,1 MMT 26/04/2006 fix bug of error in Auto Alloc if Co. Changed[Start]
    *lcCompDir  = gfGetDataDir(ALLTRIM(SYCCOMP.cCom_DDir))
    lcCompDir  = gfGetDataDir(ALLTRIM(CompFile.cCom_DDir))
    *B131801,1 MMT 26/04/2006 fix bug of error in Auto Alloc if Co. Changed[End]
  ENDIF
	USE IN CompFile
ENDIF
llReUsedBy = .F.
*B601818,1  end
llArrayORvar = TYPE('lcArray[1]')='C'
llTwoDimen = IIF(llArrayORvar AND ALEN(lcArray,2)=2,IIF(TYPE('lcArray[1,2]')='L' OR TYPE(lcArray[1,1])='U','A','N'),'V' )
IF !llArrayORvar AND ',' $ lcArray
   DIMENSION laVarArr[1]
   =gfSubStr(lcArray,@laVarArr)
   DIMENSION lcArray[ALEN(laVarArr)]
   =ACOPY(laVarArr,lcArray)
   llArrayORvar = .T.
   llTwoDimen = 'V'
*ELSE
*  lcArray = UPPER(PADR(LCARRAY,10))   
ENDIF
IF !USED('SETUPS')
  SELECT 0
  *B601818,1 use SETUPS from the company directory
  *USE (oAriaApplication.DataDir+'SETUPS')
  USE (lcCompDir+'SETUPS') AGAIN
  *B601818,1 end
  llSUsedBy = .T.
ELSE
  SELECT SETUPS
  *B601818,1 Check if the file is opened from the company path
  lcSetPath = SET('FULLPATH')
  SET FULLPATH ON
  lcSetupsDir = DBF()
  SET FULLPATH &lcSetPath
  IF !(lcCompDir) $ lcSetupsDir
    lnCurTag  = VAL(SYS(21))
    lnCurRec  = RECNO()
    USE (lcCompDir+'SETUPS') AGAIN
    llReUsedBy = .T.
  ENDIF  
  *B601818,1 end  
ENDIF
lcSetupTag =TAG()
SET ORDER TO TAG VARNAME
lcRetVal=''
lnRetCount=0
lcOnErr=ON('ERROR')
ON ERROR llError = .T.
IF !llArrayORvar
  IF SEEK(PADR(UPPER(lcArray),10))
    DO CASE
      CASE cDefa_Typ='V'
        lcRetVal = lfTrnsStr(STRTRAN(mData_Def,CHR(13)+CHR(10),''),cData_Typ)
      CASE cDefa_Typ='E'
        lcRetVal = EVAL(STRTRAN(mData_Def,CHR(13)+CHR(10),''))
    ENDCASE
  ELSE
  *-- Hesham (Start)
*!*	    llUseSycC = .T.
*!*	    IF !USED('SYCCONFG')
*!*	      SELECT 0
*!*	      USE (oAriaApplication.Syspath+'SYCCONFG')
*!*	      llCUsedBy = .T.
*!*	    ELSE
*!*	      SELECT SYCCONFG
*!*	    ENDIF
*!*	    lcConfgTag=TAG()
*!*	    SET ORDER TO TAG VARNAME

*!*	    IF SEEK(PADR(UPPER(lcArray),10))
*!*	      DO CASE
*!*	        CASE cDefa_Typ='V'
*!*	          lcRetVal = lfTrnsStr(STRTRAN(mData_Def,CHR(13)+CHR(10),''),cData_Typ)
*!*	        CASE cDefa_Typ='E'
*!*	         lcRetVal = EVAL(STRTRAN(mData_Def,CHR(13)+CHR(10),''))
*!*	      ENDCASE    
*!*	    ENDIF
		lnRemResult = oAriaApplication.remotesystemdata.execute("Select * from SYCCONFG where cfld_name='"+PADR(UPPER(lcArray),10)+"'",'',"SYCCONFGTMP","",oAriaApplication.SystemConnectionString,3,"",SET("DATASESSION"))
		IF lnRemResult>=1
    	LOCATE
  	ENDIF  
  	IF lnRemResult>=1 AND FOUND()
        DO CASE
        CASE cDefa_Typ='V'
          lcRetVal = lfTrnsStr(STRTRAN(mData_Def,CHR(13)+CHR(10),''),cData_Typ)
        CASE cDefa_Typ='E'
         lcRetVal = EVAL(STRTRAN(mData_Def,CHR(13)+CHR(10),''))
      ENDCASE    
      USE IN SYCCONFGTMP
    ENDIF
  *-- Hesham (Start)

  ENDIF
ELSE
*-- Hesham (Start)
*!*	  llUseSycC = .T.
*!*	  IF !USED('SYCCONFG')
*!*	    SELECT 0
*!*	    USE (oAriaApplication.Syspath+'SYCCONFG')
*!*	    llCUsedBy = .T.
*!*	  ELSE
*!*	    SELECT SYCCONFG
*!*	  ENDIF
*!*	  lcConfgTag=TAG()
*!*	  SET ORDER TO TAG VARNAME
		lnRemResult = oAriaApplication.remotesystemdata.execute("Select * from SYCCONFG ",'',"SYCCONFGTMP","",oAriaApplication.SystemConnectionString,3,"",SET("DATASESSION"))
*-- Hesham (End)
 
  FOR lnCount = 1 TO ALEN(lcArray,1)
    llError = .F.
    lcRetVal=''
    SELECT SETUPS
    IF SEEK(PADR(UPPER(IIF(llTwoDimen = 'V',lcArray[lnCount],lcArray[lnCount,1])),10))
      DO CASE
        CASE cDefa_Typ='V'
          lcRetVal=lfTrnsStr(STRTRAN(mData_Def,CHR(13)+CHR(10),''),cData_Typ)
        CASE cDefa_Typ='E'
         lcRetVal=EVAL(STRTRAN(mData_Def,CHR(13)+CHR(10),''))
      ENDCASE
    ELSE
      *-- Hesham (Start)
      *SELECT SYCCONFG
      *IF SEEK(PADR(UPPER(IIF(llTwoDimen = 'V',lcArray[lnCount],lcArray[lnCount,1])),10))
      SELECT SYCCONFGTMP
      LOCATE FOR cfld_name = PADR(UPPER(IIF(llTwoDimen = 'V',lcArray[lnCount],lcArray[lnCount,1])),10)
      IF FOUND()
      *-- Hesham (End)
        DO CASE
          CASE cDefa_Typ='V'
            lcRetVal = lfTrnsStr(STRTRAN(mData_Def,CHR(13)+CHR(10),''),cData_Typ)
          CASE cDefa_Typ='E'
            lcRetVal = EVAL(STRTRAN(mData_Def,CHR(13)+CHR(10),''))
        ENDCASE    
      ENDIF
    ENDIF  
    DO CASE
      CASE llTwoDimen = 'N'
        &lcArray[lnCount,2] = lcRetVal
      CASE llTwoDimen = 'V'
        &lcArray[lnCount] = lcRetVal
      CASE llTwoDimen = 'A'
        lcArray[lnCount,2] = lcRetVal
    ENDCASE
    lnRetCount=lnRetCount+IIF(!llError,1,0)    
  ENDFOR
  IF USED("SYCCONFGTMP")
    USE IN SYCCONFGTMP
  ENDIF
ENDIF
ON ERROR &lcOnErr
IF llUseSycC
  SELECT SYCCONFG
  IF !EMPTY(lcConfgTag)
    SET ORDER TO TAG (lcConfgTag)
  ELSE
    SET ORDER TO
  ENDIF
  IF llcUsedBy
    USE IN SYCCONFG
  ENDIF
ENDIF  

SELECT SETUPS
IF !EMPTY(lcSetupTag)
  SET ORDER TO TAG (lcSetupTag)
ELSE
  SET ORDER TO
ENDIF
IF llSUsedBy
  USE IN SETUPS
ENDIF

*B601818,1 ReUse SETUPS file
IF llReUsedBy .AND. !EMPTY(lcSetupsDir)
  SELECT SETUPS
  USE (lcSetupsDir) ORDER lnCurTag
  IF BETWEEN(lnCurRec, 1, RECCOUNT())
    GO lnCurRec
  ELSE
    GO TOP
  ENDIF  
ENDIF  
*B601818,1 end  

SELECT (lnAliasNo)
RETURN IIF(!llArrayORvar,lcRetVal,lnRetCount)

*!*************************************************************
*! Name      : gfOpenFile
*! Developer : MALAK - Malak Hanna
*! Date      : 04/18/1995
*! Purpose   : To open database needed by the program.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : File Name, Index Tag, 
*!                          Open Mode "EX" ----> "EXCLUSIVE"
*!                                    "SH" ----> "SHARED"
*!*************************************************************
*! Returns            :  True  ----> If passed file is open by this function
*!                       False ----> If passed file is already open.
*!*************************************************************
*! Example            : =gfOpenFile(QDD+'ORDHDR',QDD+'ORDHDR','SH')
*!*************************************************************
*E300247,1 YMA 06/13/95 Changed the displaying of the file
*E300247,1 name to be in the status message.
*B602015,1 AHM 05/07/98 using alias name to use the file by this alias
*!*************************************************************

FUNCTION gfOpenFile
PARAMETERS NFILE,lcIndex,MODE,lcAliasNam,llForceOp
PRIVATE MODE,lcFileName,lcPath,llReturnVal,lcMsg,lcSetExact

PRIVATE lcMacroSub
lcMacroSub=""
lcFileName = IIF(ATC('\',nfile)<>0,SUBSTR(NFILE,RAT('\',nfile)+1),NFILE)
lcOpenMode = IIF(TYPE('MODE')='C' AND MODE='EX', "EXCLUSIVE", "SHARED")
lcOrderTag = IIF(TYPE('lcIndex')='C',SUBSTR(lcIndex,IIF('\' $ lcIndex,ATC('\',lcIndex,OCCURS('\',lcIndex)),0) +1),'')

PRIVATE llOpen 
llOpen = .F.

lcAliasNam = IIF(TYPE('lcAliasNam')#'C' OR EMPTY(lcAliasNam),ALLTRIM(STRTRAN(UPPER(lcFileName),".DBF")),lcAliasNam)
      
lcMsg = 'Opening '+NFILE+IIF(EMPTY(lcIndex),'', ' Index Tag '+lcOrderTag)+'....'
lcMsg = PROPER(lcMsg)
IF 'SCREEN' $ SYS(101)
  SET MESSAGE TO lcMsg
ENDIF
llReturnVal = .T.
lcFPathSt   = SET('FULLPATH')
SET FULLPATH ON
IF USED(lcAliasNam)
  lcOpenMode = "SHARED"
  *-- if the file is used and it is from the same data directory
  IF DBF(lcAliasNam) == ALLTRIM(STRTRAN(UPPER(nFile), ".DBF") + ".DBF")
    *-- if forced open is desired
    IF llForceOp
      lcAliasNam  = gfTempName()
      lcMacroSub="USE (NFILE) ALIAS (lcAliasNam) AGAIN IN 0 &lcOpenMode"
      &lcMacroSub
      llOpen = .T.
      IF !EMPTY(lcOrderTag)
        SET ORDER TO TAG lcOrderTag IN (lcAliasNam)
      ENDIF    &&IF !EMPTY(lcOrderTag)
    ELSE
      *-- if forced open is not desired
      llReturnVal = .F.
      *-- if there is no tag is desired to set order to
      IF EMPTY(lcOrderTag)
        SET ORDER TO 0 IN (lcAliasNam)
      ELSE
        SET ORDER TO TAG lcOrderTag IN (lcAliasNam)
      ENDIF   &&IF EMPTY(lcOrderTag)
    ENDIF     &&IF llForceOp
  ELSE
    *-- if the file is used but not from the same data directory
    lcAliasNam  = gfTempName()
    lcMacroSub="USE (NFILE) ALIAS (lcAliasNam) AGAIN IN 0 &lcOpenMode"
    &lcMacroSub
    llOpen = .T.
    IF !EMPTY(lcOrderTag)
      SET ORDER TO TAG lcOrderTag IN (lcAliasNam)
    ENDIF  &&IF !EMPTY(lcOrderTag)
  ENDIF   &&IF DBF(lcFilename) == .......
ELSE
  *-- if the file is not used

  lcMacroSub = "USE (NFILE) ALIAS (lcAliasNam) AGAIN IN 0 &lcOpenMode"
  &lcMacroSub
  llOpen = .T.
  IF !EMPTY(lcOrderTag)
    SET ORDER TO TAG lcOrderTag IN (lcAliasNam)
  ENDIF    &&IF !EMPTY(lcOrderTag)
ENDIF    &&IF IF USED(lcFilename)
SELECT (lcAliasNam)
SET FULLPATH &lcFPathSt
*! B609424,1 MMT 10/11/2010 Fix bug of Error in Rebalance program (PO module)[Start]
*!*	IF !UPPER(oAriaApplication.WorkDir) $ UPPER(NFILE)
*!*	  lcErrOn = ON('ERROR')
*!*	  llError = .F.
*!*	  ON ERROR llError = .T.
*!*	  IF TYPE('laFileName')<>'U'
*!*	    lcSetExact = SET('Exact')
*!*	    SET EXACT ON
*!*	    FOR lnFilePos = 1 TO ALEN(laFileName,1)
*!*	      IF ALLTRIM(laFileName[lnFilePos,1]) == ALLTRIM(lcAliasNam)
*!*	        EXIT
*!*	      ENDIF
*!*	    ENDFOR
*!*	    IF lnFilePos > ALEN(laFileName,1)
*!*	      lnFilePos = 0
*!*	    ENDIF
*!*	    IF lnFilePos = 0
*!*	      IF !EMPTY(laFileName[1,1])
*!*	        DIMEN laFileName[ALEN(laFileName,1)+1,ALEN(laFileName,2)]
*!*	      *-- MAN Added ELSE Cond.  
*!*	      ELSE
*!*	        DIME laFileName[1,4] 
*!*	      ENDIF
*!*	      laFileName[ALEN(laFileName,1),1] = lcAliasNam
*!*	      laFileName[ALEN(laFileName,1),2] = lcOrderTag
*!*	      laFileName[ALEN(laFileName,1),3] = NFILE
*!*	      laFileName[ALEN(laFileName,1),4] = lcFilename
*!*	      FOR lnFileElm = 1 TO ALEN(gaMnu_Fl,1)
*!*	        IF ALLTRIM(gaMnu_Fl[lnFileElm,1]) == ALLTRIM(lcAliasNam)
*!*	          EXIT
*!*	        ENDIF
*!*	      ENDFOR
*!*	      IF lnFileElm > ALEN(gaMnu_Fl,1)
*!*	        lnFileElm = 0
*!*	      ENDIF
*!*	      IF lnFileElm > 0 
*!*	        lnRowNo = lnFileElm 
*!*	        gaMnu_Fl[lnRowNo,4] = gaMnu_Fl[lnRowNo,4] + 1
*!*	      ELSE
*!*	        DECLARE gaMnu_Fl[ALEN(gaMnu_Fl,1)+1,ALEN(gaMnu_Fl,2)]
*!*	        =AINS(gaMnu_Fl,1)
*!*	        gaMnu_Fl[1,1] = lcAliasNam
*!*	        gaMnu_Fl[1,2] = lcOrderTag
*!*	        gaMnu_Fl[1,3] = SELECT(0)
*!*	        gaMnu_Fl[1,4] = 1
*!*	        gaMnu_Fl[1,5] = IIF(!llOpen ,'S','P')
*!*	        gaMnu_Fl[1,6] = " "
*!*	      ENDIF
*!*	    ENDIF
*!*	    SET EXACT &lcSetExact
*!*	  ENDIF
*!*	  ON ERROR &lcErrOn
*!*	ENDIF  
*! B609424,1 MMT 10/11/2010 Fix bug of Error in Rebalance program (PO module)[End]
IF 'SCREEN' $ SYS(101)
  SET MESSAGE TO ""
ENDIF
RETURN llReturnVal

FUNCTION gfSysClose
PARAMETERS lcFile
PRIVATE lnFilePos 

lnFilePos = 0
IF (TYPE('laFileName[1,1]')='C' AND !EMPTY(laFileName[1,1])) 
  FOR lnFilePos = 1 TO ALEN(laFileName,1)
    IF ALLTRIM(laFileName[lnFilePos,1]) == ALLTRIM(lcFile)
      EXIT
    ENDIF
  ENDFOR
  IF lnFilePos > ALEN(laFileName,1)
    lnFilePos = 0
  ENDIF
ENDIF
IF lnFilePos = 0 AND USED(lcFile)
  USE IN (lcFile)
ENDIF

*!*************************************************************
*! Name      : gfGetDataDir
*! Developer : Hesham El-Sheltawi
*! Date      : 12/16/1998
*! Purpose   : Function to return company data directory
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 1) company data dir
*!*************************************************************
*! Example	      	 : lcDataDir=gfGetDataDir(syccomp.ccom_ddir)
*!*************************************************************
*E301078,1 HSS 11/29/98 Add this function.
*!*************************************************************
*
FUNCTION gfGetDataDir
PARAMETERS lcDataDir

RETURN (oAriaApplication.GetDataDir(lcDataDir))

*************************************************************************
*FUNCTION CheckPrd
*DESC: Function to validate transaction date
*NOTE: This function is called from evry transaction program.
*DATE: 02/28/1994
*AUTH: Wael Aly Mohamed 
*PARA: ldDate   : Transaction date to be check
*    : lcPeriod : Transaction Period
*    : lcFYear  : Transaction Fiscal Year 
*    : lcTranTyp: Type of transaction calls this function
*! MODI:  WAM 09/19/94
*!        1) Modified to call the function 'gfDialog' instead of the 
*!           function 'MsgCenter' to display messages when validate 
*!           transactions dates. Function'MsgCenter' has been deleted also.
*!B602317,1 WAM 12/06/98 Open SBT system company file with another name
*!B802236,1 AHM 08/05/1999 Allow voiding invoice in prior period
*************************************************************************
FUNCTION Checkprd
PARAMETERS ldDate,lcFYear,lcPeriod,lcTranTyp,llHideMsg
PRIVATE lcDType,lcAddMes1,lcAddMes2,lcSysDir,lcGlVers,lcGlComp, ;
        lcDate,llContinue,lcErrorM1,lcErrorM2, lnAlias
        
lnAlias = SELECT()
STORE '' TO M_POST_PPRD,M_SYS_DIR,M_GL_VERS,M_GL_CO

=gfGetMemVar('M_POST_PPRD,M_SYS_DIR,M_GL_VERS,M_GL_CO',oAriaApplication.ActiveCompanyID)

lcSysDir   = ALLTRIM(M_SYS_DIR)
lcGlVers   = ALLTRIM(M_GL_VERS)
lcGlComp   = ALLTRIM(M_GL_CO)
STORE SPACE(1) TO lcDType,lcAddMes1,lcAddMes2

lcDate = DTOC(ldDate)      && Transaction date as a string used in messages
IF lcGlVers = 'S'            &&   <<<... SBT 2.5 ... >>>
  *B602317,1 Open SBT system company file with another name
  *=gfOpenFile(lcSysDir+'SYCCOMP',lcSysDir+'COMPID','SH')
  *=SEEK(lcGlComp,'SYCCOMP')

  USE (lcSysDir+'SYCCOMP') ORDER TAG 'COMPID' IN 0 AGAIN ALIAS 'SBTCOMP'
  =SEEK(lcGlComp,'SBTCOMP')
  *B602317,1 (End)

  =gfOpenFile(lcSysDir+'SYCHFIS',lcSysDir+'COMPID1','SH')
  =gfOpenFile(lcSysDir+'SYCDFIS',lcSysDir+'COMPID1','SH')
  
  llContinue = .T.
  IF SEEK(lcGlComp)
    LOCATE REST FOR BETWEEN(ldDate,Bdate,Edate) ;
                WHILE (ldDate >= Bdate) .AND. (CompId = lcGlComp)
  ENDIF
  IF !FOUND()                && No period match checked date
    llContinue = .F.
    lcErrorM1 = ' does not fall within any period. '
    lcErrorM2 = ''
  ELSE
    &lcFYear  = SUBSTR(Yearprd,1,4)      && Transaction date year
    &lcPeriod = SUBSTR(Yearprd,5,2)      && Transaction date period     
  ENDIF  
  IF llContinue .AND. Permlck         && Permanently locked period
    llContinue = .F.
    lcErrorM1 = ' falls in a permanently locked period.'
    lcErrorM2 = ''
  ENDIF  
  IF llContinue .AND. Plocked         && Locked period
    llContinue = .F.
    lcErrorM1 = ' falls in a locked period.'
    lcErrorM2 = ''
  ENDIF  
  IF llContinue              && So far so good
    IF Pclosed               && Closed period
      IF !(lcTranTyp $ 'VI2VR2')  && Transaction is neither 
                                  && 'Void invoice' nor 'void return'.
        llDummy =  FErrInfo(lcTranTyp,'lcDType','lcAddMes1','lcAddMes2')
        lcErrorM1 = '&lcDType&lcDate belongs to prior period.'
        lcErrorM2 = ''
        =oAriaApplication.MessageBox('INM00274B00000','ALERT',lcErrorM1+lcErrorM2)
      ELSE  
        llContinue = .F.
      ENDIF
    ELSE    && Period not closed. Check if it is a future period
      *B602317,1 Open SBT system company file with another name
      *IF Yearprd <>  SYCCOMP.CURYR+SYCCOMP.CURPRD .AND. !(lcTranTyp $ 'VI2VR2')
      IF Yearprd <>  SBTCOMP.CURYR+SBTCOMP.CURPRD .AND. !(lcTranTyp $ 'VI2VR2')
      *B602317,1 (End)

        llDummy   =  FErrInfo(lcTranTyp,'lcDType','lcAddMes1','lcAddMes2')
        lcErrorM1 = '&lcDType&lcDate belongs to a future period.'
        lcErrorM2 = ''
        =oAriaApplication.MessageBox('INM00274B00000','ALERT',lcErrorM1+lcErrorM2)
      ENDIF
    ENDIF    
  ENDIF  

  *B602317,1 Open SBT system company file with another name
  USE IN SBTCOMP
  *B602317,1 (End)
ELSE

*  =gfOpenFile(oAriaApplication.SysPath+'SYCCOMP',oAriaApplication.SysPath+'CCOMP_ID','SH')
*  =SEEK(oAriaApplication.PrntCompanyID,'SYCCOMP')
  lcCompAlias = gfTempName()
  lcSelect = "Select * from SYCCOMP where ccomp_id='"+oAriaApplication.PrntCompanyID+"'"
  lnRemResult = oAriaApplication.remotesystemdata.execute(lcSelect,'',lcCompAlias,"",oAriaApplication.SystemConnectionString,3,"",SET("DATAS"))

  IF 'GL' $ &lcCompAlias..mModlset
    *=gfOpenFile(ALLTRIM(SYCCOMP.CCOM_DDIR)+'GLSETUP','','SH')
    USE (gfGetDataDir(ALLTRIM(&lcCompAlias..CCOM_DDIR))+'GLSETUP') SHARED AGAIN ALIAS TGLSETUP IN 0
    lDSETBBDAT=TGLSETUP.DSETBBDAT
    *-- Variable that hold the Allow posting before beginning balance (Start)
    *-- AAMER 11/12/98
    llAllPBB = TGLSETUP.LSETALBBE
    *-- Variable that showes the Allow posting before beginning balance (End)
    USE IN TGLSETUP 
  ELSE  
    lDSETBBDAT={}
    *-- Variable that showes the Allow posting before beginning balance (Start)
    *-- AAMER 11/12/98
    *-- .T. is assigend as default because we need not to check
    *-- if the GL module not installed or not linked
    llAllPBB = .T.
    *-- Variable that hold the Allow posting before beginning balance (End)
  ENDIF  
  *E300692,5 Use FISHD, FSPRD instead of SYCFISHD, SYCFSPRD
  *=gfOpenFile(gcSysHome+'SYCFISHD',gcSysHome+'COMPFYEAR','SH')
  *=gfOpenFile(gcSysHome+'SYCFSPRD',gcSysHome+'COMFYRPRDI','SH')
  =gfOpenFile(oAriaApplication.DataDir+'FISHD',oAriaApplication.DataDir+'COMPFYEAR','SH')
  =gfOpenFile(oAriaApplication.DataDir+'FSPRD',oAriaApplication.DataDir+'COMFYRPRDI','SH')

  *E300692,5 end
  llContinue = .T.
  LOCATE
  IF FOUND()
    LOCATE REST FOR BETWEEN(ldDate,Dfsppbgdt,Dfsppendt) ;
                WHILE (ldDate >= Dfsppbgdt)
  ENDIF
  IF !FOUND()                  && No period match checked date
    llContinue = .F.
    lcErrorM1 = ' does not fall within any period. '
    lcErrorM2 = ''
  ELSE
    &lcFYear  = Cfisfyear      && Transaction date year
    &lcPeriod = Cfspprdid      && Transaction date period     
  ENDIF  
  IF llHideMsg
    *! B609159,2 MMT 03/17/2010 CHKPRD open cursor from syccomp when called & doesn't close [Start]
    USE IN (lcCompAlias)
    *! B609159,2 MMT 03/17/2010 CHKPRD open cursor from syccomp when called & doesn't close [End]    
    SELECT (lnAlias)
    RETURN(llContinue)
  ENDIF
  *** Check if transaction date falls in a history period.
  IF llContinue .AND. Cfisfyear < STR(VAL(&lcCompAlias..CCURR_YER)-1)
    llContinue = .F.
    lcErrorM1 = ' belongs to a history fiscal year.'
    lcErrorM2 = ''
  ENDIF 
  IF llContinue         
    *** Check if the transaction date before the begining balance
    *** date, and if the user is allowed to post before the begining
    *** balance date

    *-- Check if the system is linked to GL And Allow posting before beginning Balance (Start)
    *-- AAMER 11/12/98
    *IF !EMPTY(lDSETBBDAT) .AND. ldDate < lDSETBBDAT
    IF lcGlVers='A' AND !llAllPBB AND !EMPTY(lDSETBBDAT) .AND. ldDate < lDSETBBDAT
    *-- Check if the system is linked to GL And Allow posting before beginning Balance (End) 
      llContinue = .F.
      lcErrorM1 = ' falls before the begining balance date.'
      lcErrorM2 = ' No posting allowed before the begining balance date. '
    ENDIF  
  ENDIF  
  IF llContinue .AND. Lfsplocks         && Locked period
    llContinue = .F.
    lcErrorM1 = ' falls in a locked period.'
    lcErrorM2 = ''
  ENDIF  
  IF llContinue 
    IF Lfspclsds               && Closed period
      IF !(lcTranTyp $ 'VI2VR2')
        llDummy =  FErrInfo(lcTranTyp,'lcDType','lcAddMes1','lcAddMes2')
        lcErrorM1 = '&lcDType&lcDate belongs to prior period.'
        lcErrorM2 = ''
        *E300420,1 Message : 00274
        *E300420,1 
        *E300420,1 Button : 00000 
        *E300420,1 Ok
        IF lcTranTyp # 'VI1'
          =oAriaApplication.MessageBox('INM00274B00000','ALERT',lcErrorM1+lcErrorM2)
        ENDIF  
      ELSE  
        IF lcTranTyp # 'VI2'
          llContinue = .F.
        ENDIF  
      ENDIF
    ELSE      && Period not closed. Check if it is a future period.
      IF Cfisfyear+Cfspprdid <> &lcCompAlias..CCURR_YER+&lcCompAlias..CCURR_PRD .AND. !(lcTranTyp $ 'VI2VR2')
        llDummy =  FErrInfo(lcTranTyp,'lcDType','lcAddMes1','lcAddMes2')
        lcErrorM1 = '&lcDType&lcDate belongs to a future period.'
        lcErrorM2 = ''
        *E300420,1 Message : 00274
        *E300420,1 
        *E300420,1 Button : 00000 
        *E300420,1 Ok
        =oAriaApplication.MessageBox('INM00274B00000','ALERT',lcErrorM1+lcErrorM2)
        *=gfDialog( 'I',lcErrorM1+lcErrorM2)
      ENDIF
    ENDIF    
  ENDIF  
  *B609159,1 MMT 03/01/2010 CHKPRD open cursor from syccomp when called & doesn't close [Start]
  USE IN (lcCompAlias)
  *B609159,1 MMT 03/01/2010 CHKPRD open cursor from syccomp when called & doesn't close [End]  
ENDIF
IF !llContinue             && There is an error.
  IF lcTranTyp $ 'VI2VR2'       && Transaction is either 'Void invoice'
                                && or 'Void return'
    lcErrorM1  = ' not in the current period. '
    lcErrorM2 = ''
  ENDIF
  llDummy =  FErrInfo(lcTranTyp,'lcDType','lcAddMes1','lcAddMes2')
  lcErrorM1= lcDType + lcDate + lcErrorM1
  *E300420,1 Message : 00274
  *E300420,1 
  *E300420,1 Button : 00000 
  *E300420,1 Ok
  IF lcTranTyp # 'VI2'
    =oAriaApplication.MessageBox('INM00274B00000','ALERT',lcErrorM1+lcErrorM2+lcAddMes1+lcAddMes2)
  ENDIF  
  *=gfDialog( 'I',lcErrorM1+lcErrorM2+lcAddMes1+lcAddMes2)
  SELECT (lnAlias)
  RETURN(.F.)
ENDIF
SELECT (lnAlias)
RETURN(.T.)



*!*************************************************************
*! Name      : gfModalGen
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 11/13/2002
*! Purpose   : To display any data driven dialog
*!*************************************************************
*! Passed Parameters  : 
*!                1-lcDlgID   (Dialog ID)
*!                          1st 2 characters are TR for Terminat icon
*!                                               QR for Quiry    icon
*!                                               IN for Inform   icon
*!                          2nd 4 characters are the messag ID 
*!                          3rd 4 characters are the button ID
*!                2-lcDlgTyp  (Dialog type)
*!                          'D' --> Dialog colors
*!                          'A' --> Alert  colors
*!                3-lcVarsStr  (variable(s) to be replased in the messag 
*!                4-lcDlgValid (Validation function name to be used in 
*!                              the valid of the dialog buttons)
*!                5-lcDlgMessg if you want to display a specific message
*!                            send the message string to this parameter         
*!
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : 
*!*************************************************************
*: Modification:
*! B037981,1 MAH 04/17/2004 Toolbar is active while aria message displayed.
*! N119813,FW1 MAH Enable toolbar before return.
*:***********************************************************************
* Function to display any messag with any button from the 
* of dialog object file.  The width and hight of the dialog window 
* will be calculated according to the messag width and No. of buttons.
* Parameters are:
*:->
FUNCTION gfModalGen
  LPARAMETER lcDlgID,lcDlgTyp,lcVarsStr,lcDlgValid,lcDlgMessg
  *RETURN oAriaApplication.MessageBox(lcDlgID,lcDlgTyp,lcVarsStr,lcDlgValid,lcDlgMessg)

  * B037981,1 MAH Disable toolbar before show message
  IF TYPE("oAriaApplication.oToolBar") = 'O' .AND. !ISNULL(oAriaApplication.oToolBar)
    oAriaApplication.oToolBar.Enabled = .F.
  ENDIF
  * B037981,1 MAH End

  LOCAL oMessageBox, llActiveFormLocked, llFormExist
  oMessageBox = NEWOBJECT("AriaMessageBox",ADDBS(oAriaApplication.ClassDir)+"Utility.vcx")
  IF VARTYPE(oMessageBox) != "O"
    * N119813,FW1 MAH Enable toolbar before return
    IF TYPE("oAriaApplication.oToolBar") = 'O' .AND. !ISNULL(oAriaApplication.oToolBar)
      oAriaApplication.oToolBar.Enabled = .T.
    ENDIF
    * N119813,FW1 MAH End

    RETURN 0
  ENDIF 
  
  *-- Get the dialog and buttons from the dictionary.
  IF !oMessageBox.GetMessage(lcDlgID,lcDlgTyp,lcVarsStr,lcDlgValid,lcDlgMessg)
    * N119813,FW1 MAH Enable toolbar before return
    IF TYPE("oAriaApplication.oToolBar") = 'O' .AND. !ISNULL(oAriaApplication.oToolBar)
      oAriaApplication.oToolBar.Enabled = .T.
    ENDIF
    * N119813,FW1 MAH End

    RETURN 0
  ENDIF 

  IF (TYPE("_SCREEN.ActiveForm.LockScreen") = "L") AND !EMPTY(TYPE("_SCREEN.ActiveForm.LockScreen")) AND TYPE('_SCREEN.ActiveForm') = 'O' AND !ISNULL(_SCREEN.ActiveForm)
    llFormExist = .T.
    llActiveFormLocked = _SCREEN.ActiveForm.LockScreen
    _SCREEN.ActiveForm.LockScreen = .F.
  ENDIF 

  PRIVATE lnMessageChoice
  lnMessageChoice = 1
  oMessageBox.SetMessageBox()    && Set message parameters.

  PUSH KEY
  ON KEY
  oMessageBox.Show()  && Show the message.
  POP KEY
  
  oMessageBox = .NULL.
  RELEASE oMessageBox

  IF llFormExist AND TYPE('_SCREEN.ActiveForm') = 'O' AND !ISNULL(_SCREEN.ActiveForm)
    _SCREEN.ActiveForm.LockScreen = llActiveFormLocked
  ENDIF 

  * B037981,1 MAH Enable toolbar before show message
  IF TYPE("oAriaApplication.oToolBar") = 'O' .AND. !ISNULL(oAriaApplication.oToolBar)
    oAriaApplication.oToolBar.Enabled = .T.
  ENDIF
  * B037981,1 MAH End

  RETURN lnMessageChoice  && Return the response.
ENDFUNC 
*-- end of gfModalGen.

*!*************************************************************
*! Name      : gfSequence                    E:300632
*! Developer : Wael Aly Mohamed
*! Date      : 03/04/1997
*! Purpose   : To get new sequance number for any item
*!*************************************************************
*! Calls     :  GFADD_INFO()
*!              gfRltFld()
*!*************************************************************
*! Passed Parameters  : Sequance type
*!                      Company ID
*!                      Group ID
*!                      Division Code
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   :  lcData[1] = gfSequence('CINVOICE')
*!*************************************************************
*! Modifications
*! E300888 06/04/98 YMA Added to generate the required code 
*!                      prefixed with a unique 2 characters 
*!                      code that representthe current site 
*!                      in case if the "CM" Communication 
*!                      module is installed.
*!B802982,1 02/01/2000 HDM Don't GET the GroupID if the system is not set to
*!                     generate seq.# based on division
*!*************************************************************
FUNCTION gfSequence
*B608122,1  TMI [Start] Add three parameters for Document#,Sql Table, sql Tag
*PARAMETERS lcSeqType,lcCompanyId,lcGroupId,lcDivision,lcField
PARAMETERS lcSeqType,lcCompanyId,lcGroupId,lcDivision,lcField,lcTranType,lcTable,lcTag
*B608122,1  TMI [End  ] 
PRIVATE lnRetVal,lcSavAlias,lcDataDir,lnOldGenNm,lcExtraStr,lcToFind,lcKeyExp,;
        gcDataDir,gcComp_Mdl,gcSysHome,gcCurSite,gcAct_Comp,gcOrgPath

*!*	gcDataDir  = oAriaApplication.DataDir
*!*	gcComp_Mdl = oAriaApplication.CompanyInstalledModules
*!*	gcSysHome  = oAriaApplication.SysPath
*!*	gcCurSite  = oAriaApplication.CurrentSite
*!*	gcAct_Comp = oAriaApplication.ActiveCompanyId
*!*	gcOrgPath  = oAriaApplication.DefaultPath
*!*	*TAK E300973,1 end.

*!*	*E300894,1 06/18/98 YMA Validate the optional passed parameter.
*!*	lcField    = IIF(TYPE("lcField")="C", ALLTRIM(UPPER(lcField)), SPACE(0))
*!*	*E300894,1 06/18/98 YMA End.

*!*	lcSavAlias = SELECT(0)
*!*	lcSeqType  = UPPER(lcSeqType)
*!*	lcDataDir = gcDataDir

*!*	*E300888 06/04/98 YMA If the communication module is installed, then
*!*	*E300888              get the unique site prefix for the active site
*!*	*E300888              from the sites file.
*!*	*E301488,1 12/03/2000 MAB Get PreFix Value from SETUPS FILE. [Begin]
*!*	*lcUnqPreFx = SPACE(0)
*!*	*IF "CM" $ gcComp_Mdl
*!*	*  USE (gcSysHome+"SYCSITES") IN 0 AGAIN ALIAS Sites ORDER cSiteID
*!*	*  lcUnqPreFx = IIF(SEEK(gcCurSite, "Sites"), Sites.cUniqStPre, lcUnqPreFx)
*!*	*  USE IN Sites
*!*	*ENDIF
*!*	PRIVATE lcCmpCode
*!*	PRIVATE oGetMemVar
*!*	oGetMemVar = CREATEObject("GetMemVar", This.oForm)

*!*	lcCmpCode = IIF(TYPE('lcCompanyId')='C' AND !EMPTY(lcCompanyId), lcCompanyId , gcAct_Comp)
*!*	lcUnqPreFx = oGetMemVar.Do('M_UNQSTPRX' , lcCmpCode)
*!*	*E301488,1 12/03/2000 MAB Get PreFix Value from SETUPS FILE. [End  ]

*!*	*E300888 06/04/98 YMA End.

*!*	IF TYPE('lcCompanyId')='C' AND !EMPTY(lcCompanyId) AND lcCompanyId <> gcAct_Comp
*!*	  IF USED("sycComp")
*!*	    SELECT sycComp
*!*	    luSycComp = .F.
*!*	    ltSycComp = VAL(SYS(21))
*!*	    leSycComp = RECNO()
*!*	    SET ORDER TO TAG cComp_Id IN syccomp
*!*	  ELSE
*!*	    luSycComp = .T.
*!*	    USE (gcSysHome+"syccomp") ORDER TAG cComp_Id IN 0
*!*	  ENDIF

*!*	  IF SEEK(lcCompanyId,'syccomp')
*!*	    *E301098,1 Hesham (Start)
*!*	    *lcDataDir = ALLTRIM(syccomp.cCom_dDir)
*!*	    *IF UPPER(SUBSTR(gcOrgPath,1,ATC('\',gcOrgPath,2))) = ;
*!*	    *   UPPER(SUBSTR(lcDataDir,1,ATC('\',lcDataDir,2))) AND ;
*!*	    *   UPPER(SUBSTR(gcOrgPath,1,ATC('\',gcOrgPath,2))) <>  ;
*!*	    *   UPPER(SUBSTR(gcSysHome,1,ATC('\',gcSysHome,2)))
*!*	    *  lcDataDir= SUBSTR(gcSysHome,1,ATC('\',gcSysHome,2))+;
*!*	    *             SUBSTR(lcDataDir,ATC('\',lcDataDir,2)+1)
*!*	    *ENDIF
*!*	    lcDataDir = gfGetDataDir(ALLTRIM(syccomp.cCom_dDir))
*!*	  ENDIF
*!*	  IF luSycComp
*!*	    USE IN syccomp
*!*	  ELSE
*!*	    SET ORDER TO TAG ltSycComp IN syccomp
*!*	    IF BETWEEN(leSycComp,1,RECCOUNT('syccomp'))
*!*	      GOTO leSycComp IN 'syccomp'
*!*	    ENDIF
*!*	  ENDIF
*!*	ENDIF
*!*	*E301046,4 Assure that lcGroupId is 3 Char. only
*!*	*lcGroupId  = IIF(TYPE('lcGroupId') ='C',ALLTRIM(lcGroupId),SPACE(2))
*!*	lcGroupId  = IIF(TYPE('lcGroupId') ='C',PADR(lcGroupId,3),SPACE(3))
*!*	*E301046,4 end
*!*	lcDivision = IIF(TYPE('lcDivision')='C',ALLTRIM(lcDivision),SPACE(10))
*!*	lnRetVal   = 0

*!*	*300632,1 Get division sequence group
*!*	*B802982,1 [start] Don't GET the GroupID if the system is not set to
*!*	*                  generate seq.# based on division
*!*	llDivOnSeq = oGetMemVar.Do('M_DIV_SEQ' , lcCompanyId) = 'Y'
*!*	RELEASE oGetMemVar
*!*	*Change this line to check the llDivOnSeq
*!*	*IF EMPTY(lcGroupId) .AND. !EMPTY(lcDivision)
*!*	IF llDivOnSeq AND EMPTY(lcGroupId) .AND. !EMPTY(lcDivision)
*!*	*B802982,1 [End]

*!*	  DECLARE laDivDlt[1,2]
*!*	  laDivDlt[1,1] = 'DIVGROUP'
*!*	  laDivDlt[1,2] = 'lcGroupId'
*!*	  *TAK E300973,1 Changed to work under visual.
*!*	  *=gfRltFld(PADR(lcDivision,6),@laDivDlt,'CDIVISION')
*!*	  PRIVATE oRlatdFields
*!*	  oRlatdFields = CREATEObject("GetRelatedFields", This.oForm)
*!*	  oRlatdFields.Do(PADR(lcDivision,6),@laDivDlt,'CDIVISION')
*!*	  RELEASE oRlatdFields
*!*	  *TAK E300973,1 End.

*!*	  *E301046,4 Change lcGroupId to be 3 Char. only
*!*	  *lcGroupId = SUBSTR(lcGroupId,1,10)
*!*	  lcGroupId = SUBSTR(lcGroupId,1,3)
*!*	  *E301046,4 end  
*!*	ENDIF
*!*	*B802982,1 [start] make sure the group id is empty if the system 
*!*	*                  is not set to generate seq.# based on division
*!*	*                  This case will BE FEASABLE ONLY 
*!*	*                  IF llDivOnSeq = .F.
*!*	*                  AND !EMPTY(lcGroupId)
*!*	lcGroupId = IIF(llDivOnSeq , SUBSTR(lcGroupId,1,3) , SPACE(3))
*!*	*B802982,1 [End]

*!*	IF !USED('SEQUENCE')
*!*	  luSequence = .T.
*!*	  USE &lcDataDir.SEQUENCE IN 0 ORDER TAG 'cSeq_Type'
*!*	ELSE
*!*	  SELECT SEQUENCE
*!*	  luSequence = .F.
*!*	  ltSequence = VAL(SYS(21))
*!*	  leSequence = RECNO()
*!*	  SET ORDER TO TAG Cseq_type IN SEQUENCE
*!*	ENDIF

*!*	IF !SEEK(PADR(lcSeqType,10)+lcGroupId,'SEQUENCE')
*!*	  IF !USED('sydflfld')
*!*	    luSydflfld = .T.
*!*	    USE &gcSysHome.sydflfld ORDER TAG 'Cfld_name' IN 0 SHARED
*!*	  ELSE
*!*	    SELECT Sydflfld
*!*	    luSydflfld = .F.
*!*	    ltSydflfld = VAL(SYS(21))
*!*	    leSydflfld = RECNO()
*!*	    SET ORDER TO TAG Cfld_name IN 'sydflfld'
*!*	  ENDIF
*!*	  IF !USED('sydfield')
*!*	    luSydfield = .T.
*!*	    USE &gcSysHome.sydfield ORDER TAG 'Cfld_name' IN 0 SHARED
*!*	  ELSE
*!*	    SELECT Sydfield
*!*	    luSydfield = .F.
*!*	    ltSydfield = VAL(SYS(21))
*!*	    leSydfield  = RECNO()
*!*	    SET ORDER TO TAG Cfld_name IN 'sydfield'
*!*	  ENDIF
*!*	  
*!*	  *E300894,1 06/18/98 YMA Use the optional field to get the sequence
*!*	  *E300894,1              proprities instead of the sequence field
*!*	  *E300894,1              if any.
*!*	  lcPropFld = IIF(EMPTY(lcField), lcSeqType, lcField)
*!*	  = SEEK(PADR(lcPropFld,10),'sydfield')
*!*	  SELECT sydflfld
*!*	  = SEEK(PADR(lcPropFld,10))
*!*	  LOCATE REST WHILE cFld_Name=PADR(lcPropFld,10) FOR lEnumerate
*!*	  
*!*	  *=SEEK(PADR(lcSeqType,10),'sydfield')
*!*	  *SELECT sydflfld
*!*	  *=SEEK(PADR(lcSeqType,10))
*!*	  *LOCATE REST WHILE cFld_Name=PADR(lcSeqType,10) FOR lEnumerate
*!*	  *E300894,1 06/18/98 YMA End.

*!*	  lnDefSeq = sydflfld.nDef_Seq
*!*	  IF !EMPTY(lcGroupId) AND SEEK(PADR(lcSeqType,10),'SEQUENCE')
*!*	    SELECT SEQUENCE
*!*	    lnDefSeq = 0
*!*	    SCAN REST WHILE cseq_type+cseq_group = PADR(lcSeqType,10)
*!*	      lnDefSeq = MAX(lnDefSeq,nSeq_No)
*!*	    ENDSCAN
*!*	    lnDefSeq = (INT(lnDefSeq/50000)+1)*50000
*!*	  ENDIF
*!*	  
*!*	  INSERT INTO SEQUENCE (cSeq_Type,nSeq_No,cSeq_Group,cData_Typ,nFld_Wdth) ;
*!*	       VALUES (lcSeqType,lnDefSeq,lcGroupId,sydfield.cData_Typ,;
*!*	       sydfield.nFld_Wdth)
*!*	  IF sydflfld.lEnumerate
*!*	    IF !USED('sydfiles')
*!*	      luSydfiles = .T.
*!*	      USE &gcSysHome.sydfiles ORDER TAG 'Cfile_nam' IN 0 SHARED
*!*	    ELSE
*!*	      SELECT Sydfiles
*!*	      luSydfiles = .F.
*!*	      ltSydfiles = VAL(SYS(21))
*!*	      leSydfiles = RECNO()
*!*	      SET ORDER TO TAG Cfile_nam IN 'sydfiles'
*!*	    ENDIF
*!*	    =SEEK(sydflfld.cFile_Nam,'sydfiles')
*!*	    SELECT SEQUENCE
*!*	    REPLACE cFile_Nam WITH sydfiles.cFile_Nam ,;
*!*	            cFile_Tag WITH sydfiles.cFile_Tag
*!*	    IF luSydfiles
*!*	      USE IN Sydfiles
*!*	    ELSE
*!*	      SET ORDER TO TAG ltSydfiles IN Sydfiles
*!*	      IF BETWEEN(leSydfiles,1,RECCOUNT('Sydfiles'))
*!*	        GOTO leSydfiles IN 'Sydfiles'
*!*	      ENDIF
*!*	    ENDIF
*!*	  ENDIF
*!*	  IF luSydflfld
*!*	    USE IN Sydflfld
*!*	  ELSE
*!*	    SET ORDER TO TAG ltSydflfld IN Sydflfld
*!*	    IF BETWEEN(leSydflfld,1,RECCOUNT('Sydflfld'))
*!*	      GOTO leSydflfld IN 'Sydflfld'
*!*	    ENDIF
*!*	  ENDIF
*!*	  IF luSydfield
*!*	    USE IN Sydfield
*!*	  ELSE
*!*	    SET ORDER TO TAG ltSydfield IN Sydfield
*!*	    IF BETWEEN(leSydfield,1,RECCOUNT('Sydfield'))
*!*	      GOTO leSydfield IN 'Sydfield'
*!*	    ENDIF
*!*	  ENDIF
*!*	ENDIF
*!*	*--MAN Added RLOCK Condition[Start]
*!*	   DO WHILE !RLOCK("SEQUENCE")
*!*	   ENDDO
*!*	  lnRetVal = SEQUENCE.nSeq_No
*!*	*--MAN Added RLOCK Condition[End]

*!*	*E300888 06/04/98 YMA Compute the required code width assuming that
*!*	*E300888              the minemum code field width = 6.
*!*	lnRetLen = SEQUENCE.nFld_Wdth - LEN(lcUnqPreFx)
*!*	*E300888 06/04/98 YMA End.
*!*	*B603586,1 SSH 29/02/00  (Begin) Use the new field (cSeq_Chr) from sequence file.
*!*	lnOldGenNm = SEQUENCE.nSeq_No
*!*	lcExtraStr = ''
*!*	IF !EMPTY(SEQUENCE.cSeq_Chr)
*!*	  lcExtraStr = SEQUENCE.cSeq_Chr
*!*	ENDIF
*!*	*B603586,1 SSH 29/02/00 (End)

*!*	IF !EMPTY(SEQUENCE.cFile_Nam) .AND. !EMPTY(SEQUENCE.cFile_Tag)
*!*	  lcSeqFile = ALLTRIM(SEQUENCE.cFile_Nam)
*!*	  lcSeqTag  = ALLTRIM(SEQUENCE.cFile_Tag)
*!*	  IF !USED(lcSeqFile)
*!*	    luSeqFile = .T.
*!*	    *B601946,1 Use the file again to prevent 'File is in use' message
*!*	    *USE &gcDataDir.&lcSeqFile ORDER TAG (lcSeqTag) IN 0 SHARED
*!*	    USE &gcDataDir.&lcSeqFile AGAIN ORDER TAG (lcSeqTag) IN 0 SHARED 
*!*	    *B601946,1 end
*!*	  ELSE
*!*	    SELECT (lcSeqFile)
*!*	    luSeqFile = .F.
*!*	    ltSeqFile = VAL(SYS(21))
*!*	    leSeqFile = RECNO()
*!*	    SET ORDER TO TAG (lcSeqTag) IN (lcSeqFile)
*!*	  ENDIF
*!*	  SELECT (lcSeqFile)
*!*	  lcKeyField = SUBSTR(KEY(),1,AT('+'+lcSeqType,KEY())-1)
*!*	  DECLARE laVldEnt[1]

*!*	  *TAK E300973,1 Changed to work under visual.
*!*	  PRIVATE oGetValid
*!*	  oGetValid = CREATEObject("GetValidEntries")
*!*	* IF !EMPTY(lcKeyField) .AND. gfGetVld(lcKeyField,@laVldEnt) > 0
*!*	  IF !EMPTY(lcKeyField) .AND. oGetValid.Do(lcKeyField,@laVldEnt) > 0
*!*	    FOR lnCount = 1 TO ALEN(laVldEnt,1)
*!*	      *E300888 06/04/98 YMA Search for the generated code prefixed with
*!*	      *E300888              the unique site prefix.
*!*	      *DO WHILE SEEK(laVldEnt[lnCount,2]+PADL(lnRetVal,SEQUENCE.nFld_Wdth,'0'),lcSeqFile)
*!*	      *B603586,1 SSH 29/02/00  (Begin) Use the new field (cSeq_Chr) from sequence file.
*!*	      lcKeyExp = IIF(EMPTY(lcExtraStr),PADL(lnRetVal,lnRetLen,"0"),;
*!*	                                       ALLTRIM(lcExtraStr)+PADL(lnRetVal,lnRetLen-1,"0")) 
*!*	      *DO WHILE SEEK(laVldEnt[lnCount,2]+lcUnqPreFx+PADL(lnRetVal,lnRetLen,"0"),lcSeqFile)
*!*	      DO WHILE SEEK(laVldEnt[lnCount,2]+lcUnqPreFx+lcKeyExp,lcSeqFile)
*!*	      *B603586,1 SSH 29/02/00  (End)      
*!*	      *E300888 06/04/98 YMA End.
*!*	        lnRetVal = lnRetVal + 1
*!*	        *B603586,1 SSH 29/02/00  (Begin) Use the new field (cSeq_Chr) from sequence file.
*!*	        IF !EMPTY(lcExtraStr) .AND. lnRetVal > 99999999
*!*	          lcExtraStr = CHR(ASC(lcExtraStr)+1)
*!*	        ENDIF
*!*	        lcKeyExp = IIF(EMPTY(lcExtraStr),PADL(lnRetVal,lnRetLen,"0"),;
*!*	                                         ALLTRIM(lcExtraStr)+PADL(lnRetVal,lnRetLen-1,"0")) 
*!*	        *B603586,1 SSH 29/02/00  (End)

*!*	      ENDDO
*!*	    ENDFOR
*!*	  ELSE  
*!*	    *E300888 06/04/98 YMA Search for the generated code prefixed with
*!*	    *E300888              the unique site prefix.
*!*	    *B603586,1 SSH 29/02/00  (Begin) Use the new field (cSeq_Chr) from sequence file.
*!*	    lcKeyExp = IIF(EMPTY(lcExtraStr),PADL(lnRetVal,lnRetLen,"0"),;
*!*	                                     ALLTRIM(lcExtraStr)+PADL(lnRetVal,lnRetLen-1,"0")) 
*!*	    *DO WHILE SEEK(PADL(lnRetVal,lnRetLen,'0'),lcSeqFile)
*!*	    DO WHILE SEEK(lcKeyExp,lcSeqFile)
*!*	    *E300888 06/04/98 YMA End.
*!*	      lnRetVal = lnRetVal + 1
*!*	      *B603586,1 SSH 29/02/00  (Begin) Use the new field (cSeq_Chr) from sequence file.
*!*	      IF !EMPTY(lcExtraStr) .AND. lnRetVal > 99999999
*!*	        lcExtraStr = CHR(ASC(lcExtraStr)+1)
*!*	      ENDIF
*!*	      lcKeyExp = IIF(EMPTY(lcExtraStr),PADL(lnRetVal,lnRetLen,"0"),;
*!*	                                       ALLTRIM(lcExtraStr)+PADL(lnRetVal,lnRetLen-1,"0")) 
*!*	      *B603586,1 SSH 29/02/00  (End)
*!*	    ENDDO
*!*	  ENDIF  
*!*	  RELEASE oGetValid
*!*	    
*!*	  IF luSeqFile
*!*	    USE IN (lcSeqFile)
*!*	  ELSE
*!*	    SET ORDER TO TAG ltSeqFile IN (lcSeqFile)
*!*	    IF BETWEEN(leSeqFile,1,RECCOUNT(lcSeqFile))
*!*	      GOTO leSeqFile IN (lcSeqFile)
*!*	    ENDIF
*!*	  ENDIF
*!*	ENDIF
*!*	SELECT SEQUENCE
*!*	*B603586,1 SSH 29/02/00  (Begin) Check if [lnRetVal+1] exceed 6 digit.
*!*	*REPLACE nSeq_No WITH lnRetVal+1
*!*	REPLACE nSeq_No WITH IIF(lnRetVal + 1 > 999999,0,lnRetVal + 1)
*!*	*B603586,1 SSH 29/02/00 (End)

*!*	*B603586,1 SSH 29/02/00  (Begin) Use the new field (cSeq_Chr) from sequence file.
*!*	IF nSeq_No = 0 .AND. lnOldGenNm <> 0
*!*	  REPLACE cSeq_Chr WITH IIF(EMPTY(cSeq_Chr),'A',CHR(ASC(cSeq_Chr)+1))
*!*	ENDIF
*!*	IF !EMPTY(lcExtraStr)
*!*	  lnRetVal = ALLTRIM(lcExtraStr) + PADL(lnRetVal,lnRetLen-1,"0")
*!*	ENDIF
*!*	*B603586,1 SSH 29/02/00 (End)

*!*	*--MAN Added RLOCK Condition[Start]
*!*	UNLOCK
*!*	*--MAN Added RLOCK Condition[End]

*!*	*TAK E300973,1 Changed to work under visual.
*!*	*=gfAdd_info('SEQUENCE')
*!*	*oAriaApplication.AddUserInformation('SEQUENCE')

*!*	        
*!*	*E300888 06/04/98 YMA Never return a numeric code, and return the code
*!*	*E300888              prefixed with the active site unique prefix code 
*!*	*E300888              if any.
*!*	*lnRetVal = IIF(SEQUENCE.cData_Typ='N',lnRetVal,;
*!*	*                PADL(lnRetVal,SEQUENCE.nFld_Wdth,'0'))
*!*	 lnRetVal = lcUnqPreFx + PADL(lnRetVal, lnRetLen, "0")
*!*	*E300888 06/04/98 YMA End.

*!*	IF luSequence
*!*	  USE IN Sequence
*!*	ELSE
*!*	  SET ORDER TO TAG ltSequence IN Sequence
*!*	  IF BETWEEN(leSequence,1,RECCOUNT('Sequence'))
*!*	    GOTO leSequence IN 'Sequence'
*!*	  ENDIF
*!*	ENDIF
*!*	SELECT (lcSavAlias)
*!*	RETURN(lnRetVal)

gcDataDir  = oAriaApplication.DataDir
gcComp_Mdl = oAriaApplication.CompanyInstalledModules
gcSysHome  = oAriaApplication.SysPath
gcCurSite  = oAriaApplication.CurrentSite
gcAct_Comp = oAriaApplication.ActiveCompanyId
gcOrgPath  = oAriaApplication.DefaultPath
*TAK E300973,1 end.

*E300894,1 06/18/98 YMA Validate the optional passed parameter.
lcField    = IIF(TYPE("lcField")="C", ALLTRIM(UPPER(lcField)), SPACE(0))
*E300894,1 06/18/98 YMA End.

lcSavAlias = SELECT(0)
lcSeqType  = UPPER(lcSeqType)
lcDataDir = gcDataDir

*E300888 06/04/98 YMA If the communication module is installed, then
*E300888              get the unique site prefix for the active site
*E300888              from the sites file.
*E301488,1 12/03/2000 MAB Get PreFix Value from SETUPS FILE. [Begin]
*lcUnqPreFx = SPACE(0)
*IF "CM" $ gcComp_Mdl
*  USE (gcSysHome+"SYCSITES") IN 0 AGAIN ALIAS Sites ORDER cSiteID
*  lcUnqPreFx = IIF(SEEK(gcCurSite, "Sites"), Sites.cUniqStPre, lcUnqPreFx)
*  USE IN Sites
*ENDIF
PRIVATE lcCmpCode

*B606902,1 Define a variable for cSeq_Chr updating in sequence file. [Begin]
PRIVATE lcChrToUpd
lcChrToUpd = CHR(0)
*B606902,1 Define a variable for cSeq_Chr updating in sequence file. [End]


lcCmpCode = IIF(TYPE('lcCompanyId')='C' AND !EMPTY(lcCompanyId), lcCompanyId , gcAct_Comp)
lcUnqPreFx = gfGetMemVar('M_UNQSTPRX' , lcCmpCode)
*E301488,1 12/03/2000 MAB Get PreFix Value from SETUPS FILE. [End  ]

*E300888 06/04/98 YMA End.

IF TYPE('lcCompanyId')='C' AND !EMPTY(lcCompanyId) AND lcCompanyId <> gcAct_Comp
	lnRemResult = oAriaApplication.remotesystemdata.execute("Select * from syccomp where cComp_ID='"+lcCompanyId+"'",'',"syccomptmp","",oAriaApplication.SystemConnectionString,3)
	IF lnRemResult>=1
  	LOCATE
    lcDataDir = gfGetDataDir(ALLTRIM(syccomptmp.cCom_dDir))
  ENDIF
  IF USED("syccomptmp")
    USE IN syccomptmp
  ENDIF
ENDIF
*E301046,4 Assure that lcGroupId is 3 Char. only
*lcGroupId  = IIF(TYPE('lcGroupId') ='C',ALLTRIM(lcGroupId),SPACE(2))
lcGroupId  = IIF(TYPE('lcGroupId') ='C',PADR(lcGroupId,3),SPACE(3))
*E301046,4 end
lcDivision = IIF(TYPE('lcDivision')='C',ALLTRIM(lcDivision),SPACE(10))
lnRetVal   = 0

*300632,1 Get division sequence group
*B802982,1 [start] Don't GET the GroupID if the system is not set to
*                  generate seq.# based on division
llDivOnSeq = gfGetMemVar('M_DIV_SEQ' , lcCompanyId) = 'Y'
*Change this line to check the llDivOnSeq
*IF EMPTY(lcGroupId) .AND. !EMPTY(lcDivision)
IF llDivOnSeq AND EMPTY(lcGroupId) .AND. !EMPTY(lcDivision)
*B802982,1 [End]

  DECLARE laDivDlt[1,2]
  laDivDlt[1,1] = 'DIVGROUP'
  laDivDlt[1,2] = 'lcGroupId'
  =gfRltFld(PADR(lcDivision,6),@laDivDlt,'CDIVISION')

  *E301046,4 Change lcGroupId to be 3 Char. only
  *lcGroupId = SUBSTR(lcGroupId,1,10)
  lcGroupId = SUBSTR(lcGroupId,1,3)
  *E301046,4 end  
ENDIF
*B802982,1 [start] make sure the group id is empty if the system 
*                  is not set to generate seq.# based on division
*                  This case will BE FEASABLE ONLY 
*                  IF llDivOnSeq = .F.
*                  AND !EMPTY(lcGroupId)
lcGroupId = IIF(llDivOnSeq , SUBSTR(lcGroupId,1,3) , SPACE(3))
*B802982,1 [End]

IF !USED('SEQUENCE')
  luSequence = .T.
  USE (lcDataDir+"SEQUENCE") IN 0 ORDER TAG 'cSeq_Type'
ELSE
  SELECT SEQUENCE
  luSequence = .F.
  ltSequence = VAL(SYS(21))
  leSequence = RECNO()
  SET ORDER TO TAG Cseq_type IN SEQUENCE
ENDIF

IF !SEEK(PADR(lcSeqType,10)+lcGroupId,'SEQUENCE')
  *-- Hesham Start
  *-- B606591,1 Get the active datasession and send it to the RemoteDataAccess Object
  LOCAL lnDataSess
  lnDataSess = SET("Datasession")  
  *-- Hesham End
  *E300894,1 06/18/98 YMA Use the optional field to get the sequence
  *E300894,1              proprities instead of the sequence field
  *E300894,1              if any.
	lcPropFld = IIF(EMPTY(lcField), lcSeqType, lcField)
  *-- Hesham Start
  *-- B606591,1  send the active datasession to the RemoteDataAccess Object
	lnRemFldResult = oAriaApplication.remotesystemdata.execute("Select * from sydfield where Cfld_name='"+PADR(lcPropFld,10)+"'",'',"sydfieldtmp","",oAriaApplication.SystemConnectionString,3,"",lnDataSess)
  *-- Hesham End
	IF lnRemFldResult=1
   LOCATE
  ENDIF

  *-- Hesham Start
  *-- B606591,1  send the active datasession to the RemoteDataAccess Object
	lnRemFlFldResult = oAriaApplication.remotesystemdata.execute("Select * from sydflfld where Cfld_name='"+PADR(lcPropFld,10)+"' AND lEnumerate=1",'',"sydflfldtmp","",oAriaApplication.SystemConnectionString,3,"",lnDataSess)
  *-- Hesham End
	IF lnRemFlFldResult=1
	  LOCATE
	ENDIF
		
  LOCATE REST WHILE cFld_Name=PADR(lcPropFld,10) FOR lEnumerate
    
  lnDefSeq = sydflfldtmp.nDef_Seq
  IF !EMPTY(lcGroupId) AND SEEK(PADR(lcSeqType,10),'SEQUENCE')
    SELECT SEQUENCE
    lnDefSeq = 0
    SCAN REST WHILE cseq_type+cseq_group = PADR(lcSeqType,10)
      lnDefSeq = MAX(lnDefSeq,nSeq_No)
    ENDSCAN
    lnDefSeq = (INT(lnDefSeq/50000)+1)*50000
  ENDIF
  *B606902,4 KHM 02/09/2003 (Begin) Replacing the cSeq_Chr with CHR(0)  
  *INSERT INTO SEQUENCE (cSeq_Type,nSeq_No,cSeq_Group,cData_Typ,nFld_Wdth) ;
       VALUES (lcSeqType,lnDefSeq,lcGroupId,sydfieldtmp.cData_Typ,;
       sydfieldtmp.nFld_Wdth)

  INSERT INTO SEQUENCE (cSeq_Type,nSeq_No,cSeq_Group,cData_Typ,nFld_Wdth,cSeq_Chr) ;
       VALUES (lcSeqType,lnDefSeq,lcGroupId,sydfieldtmp.cData_Typ,;
       sydfieldtmp.nFld_Wdth,CHR(0))
  *B606902,4 KHM 02/09/2003 (End)
       
  IF sydflfldtmp.lEnumerate
      *-- Hesham Start
      *-- B606591,1  send the active datasession to the RemoteDataAccess Object
  	lnRemFlResult = oAriaApplication.remotesystemdata.execute("Select * from sydfiles where Cfile_nam='"+sydflfldtmp.cFile_Nam+"'",'',"sydfilestmp","",oAriaApplication.SystemConnectionString,3,"",lnDataSess)
  	*-- Hesham End
	  IF lnRemFlResult=1
      LOCATE
    ENDIF
    SELECT SEQUENCE
    REPLACE cFile_Nam WITH sydfilestmp.cFile_Nam ,;
            cFile_Tag WITH sydfilestmp.cFile_Tag
    IF USED("SYDFILESTMP")
      USE IN Sydfilestmp
    ENDIF
  ENDIF
  IF USED("SYDFLFLDTMP")
    USE IN Sydflfldtmp
  ENDIF
  IF USED("sydfieldtmp")
    USE IN Sydfieldtmp
  ENDIF
ENDIF
*--MAN Added RLOCK Condition[Start]
   DO WHILE !RLOCK("SEQUENCE")
   ENDDO
  lnRetVal = SEQUENCE.nSeq_No

  *B606902,1 Get the character expression. [Begin]
  lcChrToUpd = Sequence.cSeq_Chr
  *B606902,1 Get the character expression. [End]
  
*--MAN Added RLOCK Condition[End]

*E300888 06/04/98 YMA Compute the required code width assuming that
*E300888              the minemum code field width = 6.
lnRetLen = SEQUENCE.nFld_Wdth - LEN(lcUnqPreFx)
*E300888 06/04/98 YMA End.
*B603586,1 SSH 29/02/00  (Begin) Use the new field (cSeq_Chr) from sequence file.
lnOldGenNm = SEQUENCE.nSeq_No
lcExtraStr = ''
IF !EMPTY(SEQUENCE.cSeq_Chr)
  *B606902,1 Get the Extra string added to the file. [Begin]
  *lcExtraStr = SEQUENCE.cSeq_Chr
  PRIVATE lcChar , lnCharPos , lnI
  IF !(SEQUENCE.cSeq_Chr = CHR(0))
    IF MOD(ASC(SEQUENCE.cSeq_Chr),26) = 0
      lcChar = "Z"
      lnCharPos = ASC(SEQUENCE.cSeq_Chr)/26
    ELSE
      lcChar =  CHR(MOD(ASC(SEQUENCE.cSeq_Chr),26)+64)
      lnCharPos = INT(ASC(SEQUENCE.cSeq_Chr)/26)+1
    ENDIF  
    FOR lnI = 1 TO lnCharPos - 1
      lcExtraStr = lcExtraStr + "Z"
    ENDFOR
    lcExtraStr = lcExtraStr + lcChar
  ELSE
  lcChar=""
  ENDIF
  *B606902,1 Get the Extra string added to the file. [End]
ENDIF
*B603586,1 SSH 29/02/00 (End)
*IF !EMPTY(SEQUENCE.cFile_Nam) AND !EMPTY(SEQUENCE.cFile_Tag)
IF !EMPTY(SEQUENCE.cFile_Nam) .AND. UPPER(LEFT(SEQUENCE.cFile_Nam,2))<> 'SY' AND !EMPTY(SEQUENCE.cFile_Tag)
  lcSeqFile = ALLTRIM(SEQUENCE.cFile_Nam)
  lcSeqTag  = ALLTRIM(SEQUENCE.cFile_Tag)  

  *B608122,1  TMI [Start] check if the parameter is empty
  lcTranType = IIF(EMPTY(lcTranType),'',lcTranType)
  lcSeqFile = IIF(EMPTY(lcTable),lcSeqFile,lcTable)
  lcSeqTag  = IIF(EMPTY(lcTag)  ,lcSeqTag ,lcTag)
  *B608122,1  TMI [End  ] 
  
  *B132218,1 KHM 05/25/2006 [Start]
  *IF !USED(lcSeqFile)
  *  luSeqFile = .T.
    *B601946,1 Use the file again to prevent 'File is in use' message
    *USE &gcDataDir.&lcSeqFile ORDER TAG (lcSeqTag) IN 0 SHARED
  *  USE (gcDataDir+lcSeqFile) AGAIN ORDER TAG (lcSeqTag) IN 0 SHARED 
    *B601946,1 end
  *ELSE
  *  SELECT (lcSeqFile)
  *  luSeqFile = .F.
  *  ltSeqFile = VAL(SYS(21))
  *  leSeqFile = RECNO()
  *  SET ORDER TO TAG (lcSeqTag) IN (lcSeqFile)
  *ENDIF
  PRIVATE lcNewSeqFile
  lcNewSeqFile = gfTempName()
  =gfOpenTable(gcDataDir+lcSeqFile,lcSeqTag,'SH',lcNewSeqFile)
  *B132218,1 KHM 05/25/2006 [End]
 

  *B132218,1 KHM 05/25/2006 [Start]
  *SELECT (lcSeqFile)
  SELECT (lcNewSeqFile)
  *B132218,1 KHM 05/25/2006 [End]
  lcKeyField = SUBSTR(KEY(),1,AT('+'+lcSeqType,KEY())-1)
  DECLARE laVldEnt[1]
  
  *B608122,1  TMI [Start] Define this variable as local so it will not be affected with defining variables with same name in any called function.
  LOCAL lnCount  
  *B608122,1  TMI [End  ] 

  *TAK E300973,1 Changed to work under visual.
  IF !EMPTY(lcKeyField) .AND. gfGetVld(lcKeyField,@laVldEnt) > 0
    FOR lnCount = 1 TO ALEN(laVldEnt,1)
      *E300888 06/04/98 YMA Search for the generated code prefixed with
      *E300888              the unique site prefix.
      *DO WHILE SEEK(laVldEnt[lnCount,2]+PADL(lnRetVal,SEQUENCE.nFld_Wdth,'0'),lcSeqFile)
      *B603586,1 SSH 29/02/00  (Begin) Use the new field (cSeq_Chr) from sequence file.

      *B606902,1 Check if next sequence number is valid. [Begin]
      *lcKeyExp = IIF(EMPTY(lcExtraStr),PADL(lnRetVal,lnRetLen,"0"),;
      *                                 ALLTRIM(lcExtraStr)+PADL(lnRetVal,lnRetLen-1,"0")) 
      lcKeyExp = lcExtraStr+PADL(lnRetVal,lnRetLen-LEN(lcExtraStr),"0")
      *B606902,1 Check if next sequence number is valid. [End]

      *DO WHILE SEEK(laVldEnt[lnCount,2]+lcUnqPreFx+PADL(lnRetVal,lnRetLen,"0"),lcSeqFile)
      
      *B132218,1 KHM 05/25/2006 [Start]
      *DO WHILE SEEK(laVldEnt[lnCount,2]+lcUnqPreFx+lcKeyExp,lcSeqFile)
      *B608122,1  TMI [Start] call the global funciton gfSeek instead
      *DO WHILE SEEK(laVldEnt[lnCount,2]+lcUnqPreFx+lcKeyExp,lcNewSeqFile)
      DO WHILE gfSEEK(laVldEnt[lnCount,2]+lcUnqPreFx+lcKeyExp,lcNewSeqFile)      
      *B608122,1  TMI [End  ] 
      
      *B132218,1 KHM 05/25/2006 [End]
      
      *B603586,1 SSH 29/02/00  (End)      
      *E300888 06/04/98 YMA End.

        *B606902,1 Check if next sequence number is valid. [Begin]
        *lnRetVal = lnRetVal + 1
        =gfGetSeq(lnRetVal,lcChrToUpd)
        *B606902,1 Check if next sequence number is valid. [End]

        *B606902,1 SSE Commented out. [Begin]
        *IF !EMPTY(lcExtraStr) .AND. lnRetVal > 99999999
        *  lcExtraStr = CHR(ASC(lcExtraStr)+1)
        *ENDIF
        *B606902,1 SSE Commented out. [End]
                
        *B606902,1 Get the new key expression. [Begin]
        *lcKeyExp = IIF(EMPTY(lcExtraStr),PADL(lnRetVal,lnRetLen,"0"),;
        *                                 ALLTRIM(lcExtraStr)+PADL(lnRetVal,lnRetLen-1,"0")) 
        lcKeyExp = lcExtraStr+PADL(lnRetVal,lnRetLen-LEN(lcExtraStr),"0")
        *B606902,1 Get the new key expression. [end]

      ENDDO
    ENDFOR
  ELSE  
    *E300888 06/04/98 YMA Search for the generated code prefixed with
    *E300888              the unique site prefix.
    *B603586,1 SSH 29/02/00  (Begin) Use the new field (cSeq_Chr) from sequence file.

    *B606902,1 Check if next sequence number is valid. [Begin]
    *lcKeyExp = IIF(EMPTY(lcExtraStr),PADL(lnRetVal,lnRetLen,"0"),;
    *                                 ALLTRIM(lcExtraStr)+PADL(lnRetVal,lnRetLen-1,"0")) 
    lcKeyExp = lcExtraStr+PADL(lnRetVal,lnRetLen-LEN(lcExtraStr),"0")
    *B606902,1 Check if next sequence number is valid. [End]
    

    *DO WHILE SEEK(PADL(lnRetVal,lnRetLen,'0'),lcSeqFile)
    
    *B132218,1 KHM 05/25/2006 [Start]
    *B608122,1  TMI [Start] call the global function gfSeek instead with the lcTranType passed
    *DO WHILE SEEK(lcKeyExp,lcSeqFile)
    DO WHILE gfSEEK(lcTranType+lcKeyExp,lcNewSeqFile)
    *B608122,1  TMI [End  ] 
    *B132218,1 KHM 05/25/2006 [End]
    *E300888 06/04/98 YMA End.

      *B606902,1 Check if next sequence number is valid. [Begin]
      *lnRetVal = lnRetVal + 1
      =gfGetSeq(lnRetVal,lcChrToUpd)
      *B606902,1 Check if next sequence number is valid. [End]

      *B606902,1 SSE Commented out. [Begin]
      *IF !EMPTY(lcExtraStr) .AND. lnRetVal > 99999999
      *  lcExtraStr = CHR(ASC(lcExtraStr)+1)
      *ENDIF
      *B606902,1 SSE Commented out. [End]
      
      *B606902,1 Get the new key expression. [Begin]
      *lcKeyExp = IIF(EMPTY(lcExtraStr),PADL(lnRetVal,lnRetLen,"0"),;
      *                                 ALLTRIM(lcExtraStr)+PADL(lnRetVal,lnRetLen-1,"0")) 
      lcKeyExp = lcExtraStr+PADL(lnRetVal,lnRetLen-LEN(lcExtraStr),"0")
      *B606902,1 Get the new key expression. [end]

      *B603586,1 SSH 29/02/00  (End)
    ENDDO
  ENDIF  
    
  *B132218,1 KHM 05/25/2006 [Start]
  *IF luSeqFile
  *  USE IN (lcSeqFile)
  *ELSE
  *  SET ORDER TO TAG ltSeqFile IN (lcSeqFile)
    *IF BETWEEN(leSeqFile,1,RECCOUNT(lcSeqFile))
  *  IF leSeqFile < 0 OR BETWEEN(leSeqFile,1,RECCOUNT(lcSeqFile))
  *    GOTO leSeqFile IN (lcSeqFile)
  *  ENDIF
  *ENDIF
  =gfCloseTable(lcNewSeqFile)
  *B132218,1 KHM 05/25/2006 [End]
ENDIF
SELECT SEQUENCE
*B603586,1 SSH 29/02/00  (Begin) Check if [lnRetVal+1] exceed 6 digit.
*REPLACE nSeq_No WITH lnRetVal+1
REPLACE nSeq_No WITH IIF(lnRetVal + 1 > 999999,0,lnRetVal + 1)
*B603586,1 SSH 29/02/00 (End)

*B606902,1 Always check if we used Characters before. [Begin]
*REPLACE nSeq_No WITH IIF(lnRetVal + 1 > 999999,0,lnRetVal + 1)
*lnRetVal = lnRetVal + 1
*lnRetVal = lcExtraStr+PADL(lnRetVal,lnRetLen-LEN(lcExtraStr),"0")
lnOldGenNm = lcExtraStr+PADL(lnRetVal,lnRetLen-LEN(lcExtraStr),"0")
=gfGetSeq(lnRetVal,lcChrToUpd)
REPLACE nSeq_No WITH lnRetVal , cSeq_Chr WITH lcChrToUpd
*B606902,1 Always check if we used Characters before. [End]

*B603586,1 SSH 29/02/00  (Begin) Use the new field (cSeq_Chr) from sequence file.
*B608122,1  TMI [Start] comment out, no need for this code segment
*IF nSeq_No = 0 .AND. VAL(lnOldGenNm) <> 0
*  REPLACE cSeq_Chr WITH IIF(EMPTY(cSeq_Chr),'A',CHR(ASC(cSeq_Chr)+1))
*ENDIF
*B608122,1  TMI [End  ] 
IF !EMPTY(lcExtraStr)
  lnRetVal = ALLTRIM(lcExtraStr) + PADL(lnRetVal,lnRetLen-1,"0")
ENDIF
*B603586,1 SSH 29/02/00 (End)

*--MAN Added RLOCK Condition[Start]
UNLOCK
*--MAN Added RLOCK Condition[End]

*TAK E300973,1 Changed to work under visual.
*=gfAdd_info('SEQUENCE')
*oAriaApplication.AddUserInformation('SEQUENCE')

        
*E300888 06/04/98 YMA Never return a numeric code, and return the code
*E300888              prefixed with the active site unique prefix code 
*E300888              if any.
*lnRetVal = IIF(SEQUENCE.cData_Typ='N',lnRetVal,;
*                PADL(lnRetVal,SEQUENCE.nFld_Wdth,'0'))

 *B606902,1 Get the value that will be displayed in message box. [Begin]
 *lnRetVal = lcUnqPreFx + PADL(lnRetVal, lnRetLen, "0")
 lnRetVal = lcUnqPreFx + PADL(lnOldGenNm, lnRetLen, "0")
 *B606902,1 Get the value that will be displayed in message box. [End]

*E300888 06/04/98 YMA End.

IF luSequence
  USE IN Sequence
ELSE
  SET ORDER TO TAG ltSequence IN Sequence
  IF BETWEEN(leSequence,1,RECCOUNT('Sequence'))
    GOTO leSequence IN 'Sequence'
  ENDIF
ENDIF
SELECT (lcSavAlias)
RETURN(lnRetVal)


*!*************************************************************
FUNCTION NotePad
*E302656 Hassan.I [Start]
*PARAMETERS lcNoteType, lcNoteKey, llTempltOk, lcDataDir
PARAMETERS lcNoteType, lcNoteKey, llTempltOk, lcDataDir, loFormSet
*E302656 Hassan.I [End]
LOCAL oNotePad

IF llTempltOk
ELSE
  oNotePad = CREATEOBJECT('NotePad')
  *E302656 Hassan.I [Start]
  *RETURN oNotePad.Do(lcNoteType, lcNoteKey)
  RETURN oNotePad.Do(lcNoteType, lcNoteKey, loFormSet)
  *E302656 Hassan.I [End]
ENDIF



*:---------------------------------------------------------------------
*! Name      : gfwCodePop
*! Developer : Yasser Mohammed Aly - (YMA)
*! Date      : 04/27/97
*! Purpose   : Function to fill any code array with on of the 
*:             following values :
*:               1) A list of the available codes from the codes file.
*:               2) The default code value.
*:               3) "ALL"
*:               4) "N/A"
*: Job ID    : E# 300631
*:---------------------------------------------------------------------
*: Calls              : gfIsEdtble()
*:---------------------------------------------------------------------
*: Passed Parameters  : laInfArray : Pointer to the code information array.
*:                      lcField    : The code to be displayed.
*:                      lcFillWith : Fill the array with ...
*:                      "L" : List of the available codes.
*:                      "D" : Default value.
*:                      "A" : "All"
*:                      "N" : "N/A"
*:---------------------------------------------------------------------
*: Example            : = gfwCodePop(@laCodInfo, "CTERMCODE", "A")
*:---------------------------------------------------------------------

FUNCTION gfwCodePop
PARAMETERS laInfArray, lcField, lcFillWith, lcActComp
LOCAL oCodePop

oCodePop = CREATEOBJECT('CodePop')
RETURN oCodePop.Do(laInfArray, lcField, lcFillWith, lcActComp)


*!*************************************************************
*! Name      : gfRltFld
*! Developer : Malak Hanna Aziz
*! Date      : 1993-1995 
*! Purpose   : 
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : 
*!*****************************************************************************
* Modifications:
*!*B603246,1 Fix the bug of the 'Variable lcObjValue not found' error message.
*!*************************************************************
* 
FUNCTION gfRltFld
PARAMETERS lcCodeVal,laArrayNam, lcFldName
PRIVATE laTempCodes,lcObjValue,llFileUsd,lcOldOrdr
*-- Hesham (Start)
*lcSavSelct  = ALIAS()   && Variable to save the currently selected file.

*!*	*B603246,1 (Begin) Open sydfield file with Cfld_name tag .
*!*	*--Is 'sydfield ' file used?
*!*	llFileUsd = .F.
*!*	IF !USED('sydfield')
*!*	  USE oAriaApplication.SysPath+'sydfield' ORDER TAG Cfld_name IN 0
*!*	  llFileUsd = .T.
*!*	ELSE
*!*	  *-- Save old order OF sydfield.
*!*	  lcOldOrdr = ORDER('sydfield')
*!*	  SET ORDER TO TAG Cfld_name IN sydfield
*!*	ENDIF
*!*	*B603246,1 (End)

*!*	IF EMPTY(lcCodeVal)   && Case N/A 
*!*	  FOR lnCount  = 1 TO ALEN(laArrayNam,1) 
*!*	    *B603246,1 (Begin) Remark the following lines and get the field type from sydfield file.
*!*	    *DO CASE
*!*	    *  CASE SUBSTR(laArrayNam[lnCount,1],1,1) $ 'Cc'
*!*	    *    lcObjValue = ''
*!*	    *  CASE SUBSTR(laArrayNam[lnCount,1],1,1) $ 'Nn'
*!*	    *    lcObjValue = 0
*!*	    *  CASE SUBSTR(laArrayNam[lnCount,1],1,1) $ 'Ll'
*!*	    *    lcObjValue = .F.
*!*	    *  CASE SUBSTR(laArrayNam[lnCount,1],1,1) $ 'Dd'
*!*	    *    lcObjValue = {}
*!*	    *ENDCASE
*!*	    *--Get field type from sydfield instead of any guess cases like naming convensions errors..
*!*	    lcObjValue = UPPER(IIF(SEEK(PADR(laArrayNam[lnCount,1],10),'sydfield'),sydfield.cData_Typ,''))
*!*	    *--Initialize the related filed variable according to the field type.
*!*	    DO CASE
*!*	      *-- Case char
*!*	      CASE lcObjValue = "C"
*!*	        lcObjValue = ''
*!*	      *-- Case Numeric
*!*	      CASE lcObjValue = "N"  
*!*	        lcObjValue = 0
*!*	      *-- Case Logical
*!*	      CASE lcObjValue = "L"  
*!*	        lcObjValue = .F.
*!*	      *-- Case date
*!*	      CASE lcObjValue = "D"
*!*	        lcObjValue = {}
*!*	    ENDCASE
*!*	    *B603246,1 (End)    
*!*	    lcFieldNam  = laArrayNam[lnCount,2]
*!*	    &lcFieldNam = lcObjValue
*!*	  ENDFOR  
*!*	  RETURN
*!*	ENDIF

*!*	*E300631,1 YMA 04/06/97 Select the codes file instead of SYCCodes.
*!*	*SELECT SYCCODES         && Select CODES file
*!*	llUseCodes = .F.
*!*	IF !USED("CODES")
*!*	  USE (oAriaApplication.DataDir+"Codes") IN 0
*!*	  llUseCodes = .T.
*!*	ENDIF
*!*	SELECT CODES         && Select CODES file
*!*	*E300631,1 YMA 04/06/97 End.
*!*	lcSavOrder = SYS(22)    && Save the file order
*!*	SET ORDER TO 0          && To activate rushmore

*!*	DECLARE laTempCodes[1]
*!*	laTempCodes = ' '

*!*	*E300631,1 YMA 04/06/97 Changed the file name to be "Codes" instead of "SYCCodes".
*!*	*E300631,1 YMA 04/06/97 And include the lcFldName in the "where" clause.
*!*	*SELECT CRLTD_NAM,CRLTD_TYP,CRLTD_VLU ;
*!*	*  FROM SYCCODES;
*!*	*  WHERE CCOMP_ID + CRLTFIELD + CFLD_NAME = gcAct_Comp + 'Y' ;
*!*	*  AND   CCODE_NO = lcCodeVal ;
*!*	*  INTO ARRAY laTempCodes
*!*	*TAK E300973,1 Changed to work under visual.
*!*	*SELECT CRLTD_NAM,CRLTD_TYP,CRLTD_VLU ;
*!*	*  FROM CODES;
*!*	*  WHERE CCOMP_ID + CRLTFIELD + CFLD_NAME = gcAct_Comp + 'Y' + lcFldName;
*!*	*  AND   CCODE_NO = lcCodeVal ;
*!*	*  INTO ARRAY laTempCodes
*!*	SELECT CRLTD_NAM,CRLTD_TYP,CRLTD_VLU ;
*!*	  FROM CODES;
*!*	  WHERE cDefCode + CRLTFIELD + CFLD_NAME = 'N' + 'Y' + lcFldName;
*!*	  AND   CCODE_NO = lcCodeVal ;
*!*	  INTO ARRAY laTempCodes
*!*	*TAK E300973,1 End.
*!*	*E300631,1 YMA 04/06/97 End.

*!*	FOR lnCount  = 1 TO ALEN(laArrayNam,1) 
*!*	  lnPosition = ASCAN(laTempCodes,laArrayNam[lnCount,1])

*!*	  IF lnPosition = 0     && not found
*!*	    *B603246,1 (Begin) Get the field type from sydfield file and if 
*!*	    *B603246,1         the related field was not saved in the codes file.
*!*	    *B603246,1         then inisialize it's value.
*!*	    *--Get field type.    
*!*	    lcObjValue = UPPER(IIF(SEEK(PADR(laArrayNam[lnCount,1],10),'sydfield'),sydfield.cData_Typ,''))
*!*	    *--Initialize the related filed variable according to the field type.
*!*	    DO CASE
*!*	      *-- case char
*!*	      CASE lcObjValue = "C"
*!*	        lcObjValue = ''
*!*	      *-- case Numeric
*!*	      CASE lcObjValue = "N"  
*!*	        lcObjValue = 0
*!*	      *-- case Logical
*!*	      CASE lcObjValue = "L"  
*!*	        lcObjValue = .F.
*!*	      *-- case date
*!*	      CASE lcObjValue = "D"
*!*	        lcObjValue = {}
*!*	    ENDCASE
*!*	    lcFieldNam  = laArrayNam[lnCount,2]
*!*	    &lcFieldNam = lcObjValue
*!*	    *B603246,1  (End)    
*!*	  ELSE
*!*	    lnPosition = ASUBSCRIPT(laTempCodes,lnPosition,1)
*!*	    DO CASE
*!*	      CASE laTempCodes[lnPosition,2] = 'C'
*!*	        lcObjValue = laTempCodes[lnPosition,3]
*!*	      CASE laTempCodes[lnPosition,2] = 'N'
*!*	        lnDecimPos = AT('.',laTempCodes[lnPosition,3])
*!*	        IF lnDecimPos > 0
*!*	          lcSavDecim = SET('DECIMALS')  && Save old decimals setting
*!*	          SET DECIMALS TO lnDecimPos
*!*	          lcObjValue = VAL(laTempCodes[lnPosition,3])
*!*	          SET DECIMALS TO &lcSavDecim          
*!*	        ELSE
*!*	          lcObjValue = VAL(laTempCodes[lnPosition,3])
*!*	        ENDIF  
*!*	      CASE laTempCodes[lnPosition,2] = 'L'
*!*	        lcObjValue = IIF(ALLTRIM(laTempCodes[lnPosition,3]) $ 'YT',.T.,.F.)

*!*	      CASE laTempCodes[lnPosition,2] = 'D'      
*!*	        lcObjValue = CTOD(laTempCodes[lnPosition,3])
*!*	    ENDCASE

*!*	    lcFieldNam  = laArrayNam[lnCount,2]
*!*	    &lcFieldNam = lcObjValue
*!*	  ENDIF  
*!*	ENDFOR  

*!*	SET ORDER TO &lcSavOrder
*!*	IF llUseCodes
*!*	  USE IN Codes
*!*	ENDIF
*!*	*B603246,1 (Begin) If the sydfield file was not used close it, else restore the old order.
*!*	IF llFileUsd
*!*	  USE IN sydfield
*!*	ELSE
*!*	  SET ORDER TO TAG lcOldOrdr IN sydfield
*!*	ENDIF  
*!*	*B603246,1 (End)

*B039255,1 KHM 04/27/2005 Initialize a temporary name for the sydfield file [Begin] 
PRIVATE lcTmpFile
lcTmpFile   = gfTempName()
*B039255,1 KHM 04/27/2005 [End]

lnSessionID = SET("DATASESSION")
lcSavSelct  = ALIAS()  && Variable to save the currently selected file.

* Open sydfield file Remotely.
lcWhereCond = ""
FOR lnCount = 1 TO ALEN(laArrayNam,1)
  lcWhereCond = lcWhereCond + IIF(lnCount>1," OR ","")+[CFLD_NAME=']+PADR(laArrayNam[lnCount,1],10)+[']
NEXT 
lcSqlCmd = "Select * from sydfield " + IIF(!EMPTY(lcWhereCond),"WHERE ","") + lcWhereCond

*B039255,1 KHM 04/27/2005 Use lcTmpFile instead of sydfieldtmp [Begin]
*lnRemResult = oAriaApplication.remotesystemdata.execute(lcSqlCmd,'',"sydfieldtmp","",oAriaApplication.SystemConnectionString,3,"",lnSessionID)
lnRemResult = oAriaApplication.remotesystemdata.execute(lcSqlCmd,'',lcTmpFile,"",oAriaApplication.SystemConnectionString,3,"",lnSessionID)
*B039255,1 KHM 04/27/2005 [End]

IF EMPTY(lcCodeVal)   && Case N/A 
  FOR lnCount  = 1 TO ALEN(laArrayNam,1) 
    LOCATE FOR cfld_name = PADR(laArrayNam[lnCount,1],10)

    *B039255,1 KHM 04/27/2005 Use lcTmpFile instead of sydfieldtmp [Begin]
    *lcObjValue = UPPER(IIF(FOUND(),sydfieldtmp.cData_Typ,''))
    lcObjValue = UPPER(IIF(FOUND(),EVALUATE(lcTmpFile+'.cData_Typ'),''))
    *B039255,1 KHM 04/27/2005 Use lcTmpFile instead of sydfieldtmp [End]

    *--Initialize the related filed variable according to the field type.
    DO CASE
      *-- Case char
      CASE lcObjValue = "C"
        lcObjValue = ''
      *-- Case Numeric
      CASE lcObjValue = "N"  
        lcObjValue = 0
      *-- Case Logical
      CASE lcObjValue = "L"  
        lcObjValue = .F.
      *-- Case date
      CASE lcObjValue = "D"
        lcObjValue = {}
    ENDCASE
    lcFieldNam  = laArrayNam[lnCount,2]
    &lcFieldNam = lcObjValue
  ENDFOR  
  *-- Amin
  SET DATASESSION TO lnSessionID
	SELECT IIF(EMPTY(lcSavSelct),0,lcSavSelct)
  *-- Amin
  RETURN
ENDIF

llUseCodes = .F.
IF !USED("CODES")
  USE (oAriaApplication.DataDir+"Codes") IN 0
  llUseCodes = .T.
ENDIF
SELECT CODES         && Select CODES file
*E300631,1 YMA 04/06/97 End.
lcSavOrder = SYS(22)    && Save the file order
SET ORDER TO 0          && To activate rushmore

DECLARE laTempCodes[1]
laTempCodes = ' '

SELECT CRLTD_NAM,CRLTD_TYP,CRLTD_VLU ;
  FROM CODES;
  WHERE cDefCode + CRLTFIELD + CFLD_NAME = 'N' + 'Y' + lcFldName;
  AND   CCODE_NO = lcCodeVal ;
  INTO ARRAY laTempCodes
*TAK E300973,1 End.
*E300631,1 YMA 04/06/97 End.

*B039255,1 KHM 04/27/2005 Use lcTmpFile instead of sydfieldtmp [Begin]
*SELECT sydfieldtmp
SELECT (lcTmpFile)
*B039255,1 KHM 04/27/2005 [End]

FOR lnCount  = 1 TO ALEN(laArrayNam,1) 
  lnPosition = ASCAN(laTempCodes,laArrayNam[lnCount,1])

  IF lnPosition = 0     && not found
    *--Get field type.    
    LOCATE FOR CFLD_NAME = PADR(laArrayNam[lnCount,1],10)

    *B039255,1 KHM 04/27/2005 Use lcTmpFile instead of sydfieldtmp [Begin]
    *lcObjValue = UPPER(IIF(FOUND(),sydfieldtmp.cData_Typ,''))
    lcObjValue = UPPER(IIF(FOUND(),EVALUATE(lcTmpFile+'.cData_Typ'),''))
    *B039255,1 KHM 04/27/2005 [End]

    *--Initialize the related filed variable according to the field type.
    DO CASE
      *-- case char
      CASE lcObjValue = "C"
        lcObjValue = ''
      *-- case Numeric
      CASE lcObjValue = "N"  
        lcObjValue = 0
      *-- case Logical
      CASE lcObjValue = "L"  
        lcObjValue = .F.
      *-- case date
      CASE lcObjValue = "D"
        lcObjValue = {}
    ENDCASE
    lcFieldNam  = laArrayNam[lnCount,2]
    &lcFieldNam = lcObjValue
  ELSE
    lnPosition = ASUBSCRIPT(laTempCodes,lnPosition,1)
    DO CASE
      CASE laTempCodes[lnPosition,2] = 'C'
        lcObjValue = laTempCodes[lnPosition,3]
      CASE laTempCodes[lnPosition,2] = 'N'
        lnDecimPos = AT('.',laTempCodes[lnPosition,3])
        IF lnDecimPos > 0
          lcSavDecim = SET('DECIMALS')  && Save old decimals setting
          SET DECIMALS TO lnDecimPos
          lcObjValue = VAL(laTempCodes[lnPosition,3])
          SET DECIMALS TO &lcSavDecim          
        ELSE
          lcObjValue = VAL(laTempCodes[lnPosition,3])
        ENDIF  
      CASE laTempCodes[lnPosition,2] = 'L'
        lcObjValue = IIF(ALLTRIM(laTempCodes[lnPosition,3]) $ 'YT',.T.,.F.)

      CASE laTempCodes[lnPosition,2] = 'D'      
        lcObjValue = CTOD(laTempCodes[lnPosition,3])
    ENDCASE

    lcFieldNam  = laArrayNam[lnCount,2]
    &lcFieldNam = lcObjValue
  ENDIF  
ENDFOR  
SELECT CODES
SET ORDER TO &lcSavOrder

IF llUseCodes
  USE IN Codes
ENDIF
*B039255,1 KHM 04/27/2005 Use lcTmpFile instead of sydfieldtmp [Begin]
*!*	IF USED("sydfieldtmp")
*!*	  USE IN sydfieldtmp
*!*	ENDIF

IF USED(lcTmpFile)
  USE IN (lcTmpFile)
ENDIF
*B039255,1 KHM 04/27/2005 [End]

SET DATASESSION TO lnSessionID
*-- Hesham (End)  

SELECT IIF(EMPTY(lcSavSelct),0,lcSavSelct)



*!*************************************************************
*! Name     : gfItemMask
*! Auth     : MAN - Mohamed Abdel Salam
*! Date     : 07/12/97
*! Task ID  : E300705,1
*!*************************************************************
*! Synopsis : function to return the item code mask or the item 
*!            code header
*!*************************************************************
FUNCTION gfItemMask
*N037782,1 AMH Add new parameter to get the material code structure [Start]
*PARAMETERS lcMaskOrHead, lcDataDr
PARAMETERS lcMaskOrHead, lcDataDr, lcInvType
*N037782,1 AMH [End]

PRIVATE lcReturn,llStructUse,lnRecNo,lcStructOrd,lnCurAlias,llArray,;
        laSeg,lcItemDim,lcHeader,lnStartNonM,lnNoSeg,lnPosistion

IF TYPE('lcDataDr') # 'C' .OR. EMPTY(lcDataDr)
  lcDataDr = oAriaApplication.DataDir
ENDIF

*N037782,1 AMH Suport Old calling [Start]
*B607915,1 MMT 12/28/2006 bug of error in Po screen while save[Start]
*IF TYPE('lcInvType') # 'C'
IF TYPE('lcInvType') # 'C' OR (TYPE('lcInvType') = 'C' AND EMPTY(lcInvType))
*B607915,1 MMT 12/28/2006 bug of error in Po screen while save[End]
  lcInvType = '0001'
ENDIF
*N037782,1 AMH [End]

STORE '' TO lcReturn        
llArray = TYPE('lcMaskOrHead[1]') # 'U'
lcItemDim = 'I'
IF !llArray
  IF TYPE('lcMaskOrHead')<>'C'
    
    *N037782,1 AMH Fix bug of Data type missmatch [Start]
    *RETURN .F.
    RETURN lcReturn
    *N037782,1 AMH [End]
    
  ENDIF
  lcMaskOrHead = UPPER(lcMaskOrHead)
  lcItemDim = IIF(LEN(lcMaskOrHead)>1,RIGHT(lcMaskOrHead,1),'I')  
  lcMaskOrHead = LEFT(lcMaskOrHead,1)  
ENDIF

lcLoopExt = lcItemDim 
lnCurAlias = SELECT()

*N037782,1 AMH Get the inventory type [Start]
LOCAL lcRmtInvType,lnConnectionHandlar,lcSqlStatment,lnDataSession,lcitemstru
lcRmtInvType  = gfTempName()
lcSqlStatment = "SELECT * FROM INVTYPE (INDEX=CINVTYPE) WHERE CINVTYPE='"+lcInvType+"'"
lnDataSession = SET("Datasession")

lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlrun(lcSqlStatment,lcRmtInvType,'INVTYPE',;
                      oAriaApplication.ActiveCompanyConStr,3,'SAVE',lnDataSession)

IF lnConnectionHandlar # 1
  *RETURN lcReturn
  lcitemstru = 'U'
ELSE
  SELECT (lcRmtInvType)
  lcitemstru = citemstru
ENDIF
*N037782,1 AMH [End]

llStructUse = .F.
IF !USED('ICISTRU')
  USE (lcDataDr+'ICISTRU') IN 0
  llStructUse = .T.
ELSE
  SELECT ICISTRU
  lcStructOrd = ORDER()
  lnRecNo = RECNO()
ENDIF
SELECT ICISTRU
SET ORDER TO TAG SEGNO

*N037782,1 AMH Get the code structure of inventory type [Start]
*=SEEK('U1')
=SEEK(lcitemstru+'1')
*N037782,1 AMH [End]

*B607915,1 MMT 12/28/2006 bug of error in Po screen while save[Start]
DIMENSION laSeg[1,7]
laSeg = ''
*B607915,1 MMT 12/28/2006 bug of error in Po screen while save[End]

lcHeader = cIsegHead
lnNoSeg = 0
lnPosistion = 1
*N037782,1 AMH Get the code structure of inventory type [Start]
*SCAN REST WHILE citemrecty+cisegno = 'U'
SCAN REST WHILE citemrecty+cisegno = lcitemstru
*N037782,1 AMH [End]

  IF lcLoopExt <> 'N'
    lnNoSeg = lnNoSeg + 1
    DIMEN laSeg[lnNoSeg,7]
    laSeg[lnNoSeg,1] = cisegType
    laSeg[lnNoSeg,2] = ALLT(cisegsdes)
    laSeg[lnNoSeg,3] = REPL('X',nisegsize)
    laSeg[lnNoSeg,4] = lnPosistion
    laSeg[lnNoSeg,5] = ALLT(CISEGLDES)
    laSeg[lnNoSeg,6] = CISEGSEPR
    laSeg[lnNoSeg,7] = LSEGENDMAJ  
    lcReturn = lcReturn+REPL('X',nisegsize)+ALLT(CISEGSEPR)
  ENDIF
  lnPosistion = lnPosistion + nisegsize + LEN(ALLT(CISEGSEPR))
  IF lcLoopExt = 'N' AND lSegEndMaj
    lcLoopExt = 'I'
  ENDIF
  IF lcItemDim = 'M' AND lSegEndMaj
     EXIT
  ENDIF    
ENDSCAN
IF llArray
  DIMEN lcMaskOrHead[ALEN(laSeg,1),ALEN(laSeg,2)]
  lcReturn=ACOPY(laSeg,lcMaskOrHead)
ELSE  
  DO CASE
    CASE  lcMaskOrHead = 'S'
      lcReturn = lnNoSeg
    CASE  lcMaskOrHead = 'P' AND  lcItemDim='M'
      IF gfItemMask('SN') > 0
        lcReturn = SUBSTR(lcReturn,1,LEN(lcReturn)-1)
      ENDIF  
    CASE lcMaskOrHead = 'H' AND lcItemDim='M'
      IF gfItemMask('SN') > 0
        lcReturn = SUBSTR(lcHeader,1,laSeg[lnNoSeg,4]+LEN(laSeg[lnNoSeg,2])-1)
      ELSE
        lcReturn = lcHeader        
      ENDIF  
    CASE lcMaskOrHead = 'H' AND lcItemDim='N'  AND lnNoSeg>0
      lcReturn = SUBSTR(lcHeader,laSeg[1,4])  
    CASE lcMaskOrHead = 'H' AND lcItemDim='I'
      lcReturn = lcHeader  
  ENDCASE
ENDIF  
IF llStructUse 
  USE IN ICISTRU
ELSE    
  SELECT ICISTRU
  SET ORDER TO TAG (lcStructOrd)
  IF lnRecNo>0 AND lnRecNo<=RECCOUNT()
    GO lnRecNo
  ENDIF  
ENDIF

*N037782,1 AMH Close inventory type [Start]
IF USED(lcRmtInvType)
  USE IN (lcRmtInvType)
ENDIF
*N037782,1 AMH [End]

SELECT (lnCurAlias)
RETURN lcReturn

*!**************************************************************************
*!
*!      Function:  gfGetAdr
*!
*!**************************************************************************
*  Gets address according to the address code, returns address
*
FUNCTION gfGetAdr
PARAMETERS lcAlias, lcTag, lcKeyCode, lcAdrCode,lnLineNo,lcAddGrp,lcCurrCode

*** lcAlias   : source file name 
*** lcTag     : source file tag that is to be used in seeking
*** lckeycode : search key code (of the source file) (optional)
*** lcAdrCode : address code (optional)
*** lnLineNo  : The Address line number to return

PRIVATE lnSavIntTg, lnSavCmpTg, lcCurAlias, lnOldTag,;
        llOpenInt, llOpenCmp, llContinue, lnCompRec,lcCurrCode,lnCounter

lnCounter = 0 
 * You have to send the source file and 1 or more from the following parameters
 * 1 - The alias name for the source file or you have it the currently selected
 * 2 - Address code to be used in getting the address line  OR
 * 3 - Tag name and Seek Expression to get the  Address code
 * 4 - You can have the source file opened with the proper tag and just send
 *     the seek expr. (In case of not sending Tag ID there must be an active one)      
 
 IF EMPTY(lcAlias) .OR. TYPE('lcTag') <> 'C'
   IF EMPTY(ALIAS())
     RETURN .F.
   ELSE   
     lcAlias = ALIAS()
   ENDIF  
 ENDIF
 IF EMPTY(lcAddGrp) OR TYPE('lcAddGrp') <> 'C'
   lcAddGrp  = ''
   lcGrpCode = 'E'
 ELSE  
   lcAddGrp  = ALLTRIM(lcAddGrp)
   lcGrpCode = lcAddGrp
 ENDIF   
 lcCurAlias = ALIAS()
 SELECT (lcAlias)
 lnOldTag = VAL(SYS(21))   

 *-- No Address code has been sent
 IF EMPTY(lcAdrCode) 
   IF !EMPTY(lcKeyCode) .AND. TYPE('lcKeyCode') <> 'C'
     DO CASE

       *-- A Search Expr has been sent and no Tag Has been Sent and no active tag
       CASE (EMPTY(lcTag) .OR. TYPE('lcTag') <> 'C') AND EMPTY(SYS(22))
         SELECT IIF(!EMPTY(lcCurAlias),(lcCurAlias),0)  
         RETURN .F.

       *-- A Search Expr and a Tag ID have been sent 
       CASE !EMPTY(lcTag)
         *lnOldTag = VAL(SYS(21))   
         SET ORDER TO TAG (lcTag)
         *-- The Search expr is not found
         IF !SEEK(lcKeyCode)
           SET ORDER TO lnOldTag
           SELECT IIF(!EMPTY(lcCurAlias),(lcCurAlias),0)  
           RETURN .F.
         ENDIF
 
       *-- A search expr has been sent without a Tag 
       OTHERWISE 
         *-- There is no active tag
         IF EMPTY(SYS(22)) 
           SELECT IIF(!EMPTY(lcCurAlias),(lcCurAlias),0)  
           RETURN .F.
         ENDIF
         *-- The Search Expr. is not found
         IF !SEEK(lcKeyCode)
           SELECT IIF(!EMPTY(lcCurAlias),(lcCurAlias),0)  
           RETURN .F.
         ENDIF
     ENDCASE 
   ENDIF  

   *-- Just to be able to set the old tag even if it has not been 
   *-- changed in the above DO CASE
   *lnOldTag = VAL(SYS(21))   

   lcAdrCode = cCont_Cod&lcGrpCode
 ENDIF

DECLARE laAddress[6,3]
laAddress = " "
*E301278,1 lnLineNo may be a numeric or an array hold addresses (Passed by reference) [begin]
*lnLineNo  = IIF(TYPE('lnLineNo')='N' AND BETWEEN(lnLineNo,1,5),INT(lnLineNo),1)
llRetArray = (TYPE("lnLineNo[1]") = "C")
IF llRetArray
  lnLineNo = ""
ELSE
  lnLineNo  = IIF(TYPE('lnLineNo')='N' AND BETWEEN(lnLineNo,1,5),INT(lnLineNo),1)
ENDIF
*E301278,1 lnLineNo may be a numeric or an array hold addresses (Passed by reference) [end  ]

*-- Hesham (Start)
*-- select data from syccurr remotely and procced the addcurrency
*-- normaly

*!*	STORE .F. TO llOpenInt, llOpenCmp
*!*	*** Check being on a correct alias
*!*	   
*!*	IF !USED('SYCINT')  && Check if the internationals file is open or not.
*!*	  llOpenInt  = .T.     && Indicates that the file is open by the function.
*!*	  ** Use the file and assign the index.
*!*	  USE (oAriaApplication.SysPath + 'SYCINT') ORDER TAG cContCode IN 0 
*!*	ELSE
*!*	  SELECT SYCINT       
*!*	  lnSavIntTg = VAL(SYS(21))
*!*	  SET ORDER TO TAG cContCode   && Change the order
*!*	ENDIF  

*!*	IF !USED('SYCCOMP')  && Check if the internationals file is open or not.
*!*	  llOpenCmp  = .T.     && Indicates that the file is open by the function.
*!*	  ** Use the file and assign the index.
*!*	  USE (oAriaApplication.SysPath + 'SYCCOMP') ORDER TAG cComp_ID IN 0 
*!*	ELSE
*!*	  SELECT SYCCOMP       
*!*	  lnSavCmpTg = VAL(SYS(21))
*!*	  lnCompRec  = RECNO()
*!*	  SET ORDER TO TAG cComp_ID   && Change the order
*!*	ENDIF  

*!*	IF SEEK(lcAdrCode,'SYCINT') .OR. (SEEK(oAriaApplication.ActiveCompanyID,'SYCCOMP') ;
*!*	   .AND. SEEK(SYCCOMP.cCont_Code,'SYCINT'))
*!*	  laAddress[1,1] = SYCINT.nPart1Ord
*!*	  laAddress[1,2] = EVAL(lcAlias+'.cAddress1'+lcAddGrp)
*!*	  laAddress[1,3] = SYCINT.nPart1LEN
*!*	  laAddress[2,1] = SYCINT.nPart2Ord
*!*	  laAddress[2,2] = EVAL(lcAlias+'.cAddress2'+lcAddGrp) 
*!*	  laAddress[2,3] = SYCINT.nPart2LEN
*!*	  laAddress[3,1] = SYCINT.nPart3Ord
*!*	  laAddress[3,2] = EVAL(lcAlias+'.cAddress3'+lcAddGrp)
*!*	  laAddress[3,3] = SYCINT.nPart3LEN      
*!*	  laAddress[4,1] = SYCINT.nPart4Ord
*!*	  laAddress[4,2] = EVAL(lcAlias+'.cAddress4'+lcAddGrp)
*!*	  laAddress[4,3] = SYCINT.nPart4LEN      
*!*	  laAddress[5,1] = SYCINT.nPart5Ord
*!*	  laAddress[5,2] = EVAL(lcAlias+'.cAddress5'+lcAddGrp)
*!*	  laAddress[5,3] = SYCINT.nPart5LEN      
*!*	  laAddress[6,1] = SYCINT.nPart6Ord
*!*	  laAddress[6,2] = EVAL(lcAlias+'.cAddress6'+lcAddGrp)
*!*	  laAddress[6,3] = SYCINT.nPart6LEN
*!*	  IF TYPE("lcCurrCode") = 'C'
*!*	    &lcCurrCode = SYCINT.cCurrCode
*!*	    *SHOW GET (lcCurrCode)
*!*	  ENDIF  
*!*	  =ASORT(laAddress,1)
*!*	  lcRetVal=''
*!*	  *E301278,1 lnLineNo may be a numeric or an array hold addresses (Passed by reference) [begin]
*!*	  *FOR lnCount = 1 TO ALEN(laAddress,1)
*!*	  *  IF laAddress[lnCount,1] = lnLineNo
*!*	  *    *lcRetVal=lcRetVal+IIF(EMPTY(lcRetVal),'',',')+PADR(SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3]),laAddress[lnCount,3])
*!*	  *    lcAddPart = ALLTRIM(SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3]))
*!*	  *    lcRetVal  = lcRetVal+IIF(EMPTY(lcRetVal) .OR. RIGHT(lcRetVal,1) = ',' ,'',', ') + lcAddPart
*!*	  *   ENDIF
*!*	  *ENDFOR
*!*	  *-- if it is array
*!*	  IF llRetArray
*!*	    PRIVATE lnArrLen
*!*	    lnArrLen = 0
*!*	    FOR lnArrLen = 1 TO ALEN(lnLineNo,1)
*!*	      FOR lnCount = 1 TO ALEN(laAddress,1)
*!*	        IF laAddress[lnCount,1] = lnArrLen
*!*	          lcAddPart = ALLTRIM(SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3]))
*!*	          lnLineNo[lnArrLen] = lnLineNo[lnArrLen]+;
*!*	                               IIF(EMPTY(lnLineNo[lnArrLen]) .OR. ;
*!*	                               RIGHT(lnLineNo[lnArrLen],1) = ',' ,'',', ') + lcAddPart
*!*	         ENDIF
*!*	      ENDFOR
*!*	    ENDFOR
*!*	  ELSE  && else numeric value.

*!*	    FOR lnCount = 1 TO ALEN(laAddress,1)
*!*	      IF laAddress[lnCount,1] = lnLineNo
*!*	        lnCounter = lnCounter + 1
*!*	        lcAddPart = ALLTRIM(SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3]))
*!*	        lcRetVal  = lcRetVal+IIF(EMPTY(lcRetVal) .OR. RIGHT(lcRetVal,1) = ',' ,'',;
*!*	                             IIF(ALLTRIM(gcContCode) = "USA" AND lnCounter > 2 ,'  ' ,', ')) + lcAddPart
*!*	       ENDIF
*!*	    ENDFOR
*!*	  ENDIF
*!*	  *E301278,1 lnLineNo may be a numeric or an array hold addresses (Passed by reference) [end  ]
*!*	  
*!*	ELSE
*!*	  *E301278,1 lnLineNo may be a numeric or an array hold addresses (Passed by reference) [begin]
*!*	  *lcRetVal= EVAL(lcAlias+'.cAddress'+STR(lnLineNo,1)+lcAddGrp)
*!*	  *-- if it is an array
*!*	  IF llRetArray
*!*	    PRIVATE lnArrLen
*!*	    lnArrLen = 0
*!*	    FOR lnArrLen = 1 TO ALEN(lnLineNo,1)
*!*	      lnLineNo[lnArrLen]= EVAL(lcAlias+'.cAddress'+STR(lnArrLen,1)+lcAddGrp)
*!*	    ENDFOR
*!*	  ELSE  && else it is numeric value.
*!*	    lcRetVal= EVAL(lcAlias+'.cAddress'+STR(lnLineNo,1)+lcAddGrp)
*!*	  ENDIF  
*!*	  *E301278,1 lnLineNo may be a numeric or an array hold addresses (Passed by reference) [end  ]
*!*	ENDIF  

*!*	IF USED('SYCCOMP')
*!*	  IF llOpenCmp 
*!*	    USE IN SYCCOMP
*!*	  ELSE
*!*	    SET ORDER TO lnSavCmpTg IN SYCCOMP  
*!*	    IF BETWEEN(lnCompRec,1,RECCOUNT("SYCCOMP"))
*!*	      GOTO lnCompRec IN SYCCOMP
*!*	    ENDIF  
*!*	  ENDIF
*!*	ENDIF
*!*	  
*!*	IF USED('SYCINT')
*!*	  IF llOpenInt
*!*	    USE IN SYCINT
*!*	  ELSE
*!*	    SET ORDER TO lnSavIntTg IN SYCINT
*!*	  ENDIF  
*!*	ENDIF
lnRemResult = oAriaApplication.remotesystemdata.execute("Select * from SYCINT where ccont_code='"+lcAdrCode+"'",'',"SYCINTTMP","",oAriaApplication.SystemConnectionString,3,"",SET("DATAS"))
IF lnRemResult=1
    LOCATE
	IF !FOUND()  
  	  lnRemResult = oAriaApplication.remotesystemdata.execute("Select * from SYCINT where ccont_code='"+oAriaApplication.DefaultCountry+"'",'',"SYCINTTMP","",oAriaApplication.SystemConnectionString,3,"",SET("DATAS"))
  	  *N000300,1 HBG 05/17/2004 Locate to fix bug of EOF()
  	  LOCATE
  	  *N000300,1 [End]
	ENDIF
	IF lnRemResult>=1 AND FOUND()
	  laAddress[1,1] = SYCINTTMP.nPart1Ord
  	laAddress[1,2] = EVAL(lcAlias+'.cAddress1'+lcAddGrp)
	  laAddress[1,3] = SYCINTTMP.nPart1LEN
  	laAddress[2,1] = SYCINTTMP.nPart2Ord
	  laAddress[2,2] = EVAL(lcAlias+'.cAddress2'+lcAddGrp) 
  	laAddress[2,3] = SYCINTTMP.nPart2LEN
	  laAddress[3,1] = SYCINTTMP.nPart3Ord
  	laAddress[3,2] = EVAL(lcAlias+'.cAddress3'+lcAddGrp)
	  laAddress[3,3] = SYCINTTMP.nPart3LEN      
	  laAddress[4,1] = SYCINTTMP.nPart4Ord
  	laAddress[4,2] = EVAL(lcAlias+'.cAddress4'+lcAddGrp)
	  laAddress[4,3] = SYCINTTMP.nPart4LEN      
  	laAddress[5,1] = SYCINTTMP.nPart5Ord
	  laAddress[5,2] = EVAL(lcAlias+'.cAddress5'+lcAddGrp)
    laAddress[5,3] = SYCINTTMP.nPart5LEN      
  	laAddress[6,1] = SYCINTTMP.nPart6Ord
	  laAddress[6,2] = EVAL(lcAlias+'.cAddress6'+lcAddGrp)
 	  laAddress[6,3] = SYCINTTMP.nPart6LEN
  	IF TYPE("lcCurrCode") = 'C'
   	 &lcCurrCode = SYCINTTMP.cCurrCode
	  ENDIF  
 	 =ASORT(laAddress,1)
	  lcRetVal=''
  *-- if it is array
	  IF llRetArray
 	   PRIVATE lnArrLen
  	  lnArrLen = 0
   	 FOR lnArrLen = 1 TO ALEN(lnLineNo,1)
    	  FOR lnCount = 1 TO ALEN(laAddress,1)
     	   IF laAddress[lnCount,1] = lnArrLen
      	    lcAddPart = ALLTRIM(SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3]))
       	   lnLineNo[lnArrLen] = lnLineNo[lnArrLen]+;
        	                       IIF(EMPTY(lnLineNo[lnArrLen]) .OR. ;
         	                      RIGHT(lnLineNo[lnArrLen],1) = ',' ,'',', ') + lcAddPart
	         ENDIF
  	    ENDFOR
    	ENDFOR
	  ELSE  && else numeric value.

  	  FOR lnCount = 1 TO ALEN(laAddress,1)
    	  IF laAddress[lnCount,1] = lnLineNo
      	  lnCounter = lnCounter + 1
        	lcAddPart = ALLTRIM(SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3]))
	        lcRetVal  = lcRetVal+IIF(EMPTY(lcRetVal) .OR. RIGHT(lcRetVal,1) = ',' ,'',;
  	                           IIF(ALLTRIM(oAriaApplication.DefaultCountry) = "USA" AND lnCounter > 2 ,'  ' ,', ')) + lcAddPart
    	   ENDIF
	    ENDFOR
	  ENDIF
  *E301278,1 lnLineNo may be a numeric or an array hold addresses (Passed by reference) [end  ]
  
	ELSE
	  *E301278,1 lnLineNo may be a numeric or an array hold addresses (Passed by reference) [begin]
 	 *lcRetVal= EVAL(lcAlias+'.cAddress'+STR(lnLineNo,1)+lcAddGrp)
  *-- if it is an array
	  IF llRetArray
 	   PRIVATE lnArrLen
  	  lnArrLen = 0
   	 FOR lnArrLen = 1 TO ALEN(lnLineNo,1)
    	  lnLineNo[lnArrLen]= EVAL(lcAlias+'.cAddress'+STR(lnArrLen,1)+lcAddGrp)
    	ENDFOR
	  ELSE  && else it is numeric value.
 	   lcRetVal= EVAL(lcAlias+'.cAddress'+STR(lnLineNo,1)+lcAddGrp)
  	ENDIF  
	  *E301278,1 lnLineNo may be a numeric or an array hold addresses (Passed by reference) [end  ]
	ENDIF  
ENDIF

IF USED("SYCINTTMP")
	USE IN SYCINTTMP
ENDIF
*-- Hesham (End)
SET ORDER TO lnOldTag IN (lcAlias)
SELECT IIF(!EMPTY(lcCurAlias),(lcCurAlias),0)  

RETURN lcRetVal

*!*************************************************************
*! Name      : gfGetVld
*! Developer : Malak Hanna Aziz
*! Date      : 1993-1995 
*! Purpose   : 
*!*************************************************************
*! Calls     : 
*!          Calls: GFSUBSTR()               (function  in ARIA3.PRG)
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : 
*!*************************************************************
FUNCTION gfGetVld
PARAMETERS lcFieldNam, laArrName,llAddAll
PRIVATE lcCurrFile,lcOldTag,lcSetExct,llSydField,lnMaxLen

*** Save current environment
lcCurrFile    = SELECT()
lcSetExact    = SET('EXACT')
llSydField    = .F.
lnMaxLen      = 0

*-- Hesham (Start)
*!*	*** Check if fields system file is opened, if it is 
*!*	*** set the appropriate tag, if not open use it with
*!*	*** the appropriate tag.
*!*	*** Tag to be used is 'cFld_Name'
*!*	*** Its expression is
*!*	IF !USED('SYDFIELD')
*!*	  SELECT 0
*!*	  USE (oAriaApplication.SysPath+"SYDFIELD") ORDER TAG cFld_Name
*!*	  llSydField       = .T.
*!*	ELSE
*!*	  SELECT SYDFIELD
*!*	  lcOldTag         = SYS(22)
*!*	  SET ORDER TO TAG cFld_Name
*!*	ENDIF

*!*	*** Search for field name
*!*	IF SEEK(UPPER(lcFieldNam), 'SYDFIELD')
*!*	  PRIVATE oSubString
*!*	  oSubString = CREATEObject("SubString", This.oForm)
*!*	  oSubString.Do(SYDFIELD.mVEntries, @laArrName, "|~")
*!*	  RELEASE oSubString
*!*	  lnMaxLen = LEN(laArrName[1,1])
*!*	  FOR lnCount = 2 TO ALEN(laArrName,1) 
*!*	    lnMaxLen = MAX(lnMaxLen,LEN(laArrName[lnCount,1]))
*!*	  ENDFOR  
*!*	ELSE
*!*	  laArrName = " "
*!*	ENDIF  

*!*	IF llAddAll
*!*	  *** Add one element on top to be used for 'All' option.     
*!*	  *** Assign a value to this element later.
*!*	  IF !EMPTY(laArrName[1,1])
*!*	    DIMENSION laArrName[ALEN(laArrName,1)+1,2]
*!*	    =AINS(laArrName, 1, 1)
*!*	  ENDIF  
*!*	  laArrName[1,1] = "All"
*!*	  laArrName[1,2] = " "
*!*	ENDIF  

*!*	*** Restore environment
*!*	IF llSydField .AND. USED('SYDFIELD')
*!*	  USE IN 'SYDFIELD'   
*!*	ELSE
*!*	  IF !EMPTY(lcOldTag)
*!*	    SET ORDER TO TAG (lcOldTag)
*!*	  ENDIF  
*!*	ENDIF

*-- select the field information from sydfield remotely
lnDataS = SET("DATASESSION")
lnRemResult = oAriaApplication.remotesystemdata.execute("Select * from SYDFIELD where cFld_Name='"+UPPER(lcFieldNam)+"'",'',"SYDFIELDTMP","",oAriaApplication.SystemConnectionString,3,"",lnDataS)
*-- if the connection succeded
IF lnRemResult=1
  LOCATE
** Check if the field was found then get the field name
	IF FOUND()
 * 	PRIVATE oSubString
	 * oSubString = CREATEObject("SubString", This.oForm)
*  	oSubString.Do(SYDFIELD.mVEntries, @laArrName, "|~")
  	=gfSubStr(SYDFIELDTMP.mVEntries, @laArrName, "|~")
*	  RELEASE oSubString
  	lnMaxLen = LEN(laArrName[1,1])
	  FOR lnCount = 2 TO ALEN(laArrName,1) 
  	  lnMaxLen = MAX(lnMaxLen,LEN(laArrName[lnCount,1]))
	  ENDFOR  
	ELSE
  	laArrName = " "
	ENDIF  
ENDIF
IF llAddAll
  *** Add one element on top to be used for 'All' option.     
 	*** Assign a value to this element later.
  IF !EMPTY(laArrName[1,1])
 	  DIMENSION laArrName[ALEN(laArrName,1)+1,2]
    =AINS(laArrName, 1, 1)
 	ENDIF  
  laArrName[1,1] = "All"
 	laArrName[1,2] = " "
ENDIF  

*** Restore environment
** if the connection cursor was created then close it
IF USED('SYDFIELDTMP')
  USE IN 'SYDFIELDTMP'   
ENDIF
SET DATASESSION TO (lnDataS)
*-- Hesham (End)
SELECT (lcCurrFile)
SET EXACT &lcSetExact
RETURN lnMaxLen

*!*************************************************************
*! Name      : gfStyPrice
*! Developer : Renee Ezzat
*! Date      : 12/20/95
*! Purpose   : Return the style/color price in a given currency.
*!*************************************************************
*! Parameters: lcStyle    : Style
*!             lcColor    : color
*!             lcStyCur   : Style currency code
*!             lcPriceLvl : price level ('A', 'B', or 'C')
*!             llNoAdd    : .T. if adding prices on the fly
*!                          is not to be allowed despite the company
*!                          setup.
*!*************************************************************
*! Calls     : gfGetMemVar
*!             STYPRICE.SPR     
*!*************************************************************
*! Returns                : Price level in passed currncy for
*!                          the passed style/color, or
*!                          -1 if it is not found and/or not
*!                          added.
*!*************************************************************
*! Example   : lnPrice = gfStyPrice(lcStyle, lcColor, lcCurrCode, 'A')
*!*************************************************************
*E300328,1 RENEE 12/21/95. 
*E300620,4 Reham On 08/05/97
*E300620,4 Remove color from the function.
*B602290,1 HSS 11/29/98 Get the price from STYLE file in the case of base
*B602290,1              currency.
*:*************************************************************************
FUNCTION gfStyPrice
PARAMETERS lcStyle, lcPriceLvl, lcStyCur, llNoAdd 
PRIVATE lcFilter, lnOldVal, lnCurAlias, lnCurTag, lnCurRec, llOpenFile, lnRetPrice

IF !gfGetMemVar('LLMULCURR',oAriaApplication.ActiveCompanyID)
*-- Change this line to use oGetMemVar [End]
  RETURN -1
ENDIF

IF EMPTY(lcStyle) .OR. EMPTY(lcPriceLvl) .OR. ATC(lcPriceLvl, 'ABC') = 0
  RETURN -1
ENDIF  

*E300328,1 If there is no currency code parameter, default with
*E300328,1 the base currency.
IF EMPTY(lcStyCur)
  *-- Change this line to use oAriaApplication.BaseCurrency
  *lcStyCur = gcBaseCurr
  lcStyCur = oAriaApplication.BaseCurrency
ENDIF  

*E300328,1 Check if STYPRICE is open,
*E300328,1 If the file is used, store current environment
lnCurAlias = SELECT()
lnOldVal   = 0
IF USED('STYPRICE')
  llOpenFile = .F.
  SELECT STYPRICE
  *E300328,1 Get current tag
  lnCurTag   = VAL(SYS(21))
  *E300328,1 Get current record
  lnCurRec   = IIF(!EOF(), RECNO(), 0)
  *E300328,1 Get current filter
  lcFilter   = FILTER() 
  SET FILTER TO
  SET ORDER TO TAG STYPRICE  
ELSE
  llOpenFile = .T.
  SELECT 0
  USE (oAriaApplication.DataDir + 'STYPRICE') ORDER TAG STYPRICE
  lnCurAlias = 0
ENDIF

*E300328,1 Adjust Customer level parameter.
lcPriceLvl = LEFT(ALLTRIM(lcPriceLvl), 1) 
IF ALLTRIM(lcStyCur) == ALLTRIM(oAriaApplication.BaseCurrency)
  PRIVATE llCloseSty , lcStyOrder , lnStyRecNo
  
  IF !USED('STYLE')
    llCloseSty = .T.
    =gfOpenFile(oAriaApplication.DataDir+ 'STYLE' , oAriaApplication.DataDir+ 'STYLE','SH')
    SELECT STYLE
  ELSE
    llCloseSty = .F.
    SELECT STYLE
    lnStyRecNo = IIF(EOF() , 0 , RECNO())
    lcStyOrder = ORDER()
    SET ORDER TO TAG STYLE
  ENDIF
  
  lcStyle = PADR(lcStyle , LEN(Style))
  IF SEEK(lcStyle)
    lnRetPrice = Price&lcPriceLvl
  ELSE
    lnRetPrice = -1
  ENDIF
  
  IF llCloseSty
    USE IN STYLE
  ELSE
    SELECT STYLE
    SET ORDER TO (lcStyOrder) IN STYLE
    IF lnStyRecNo <> 0
      GO lnStyRecNo IN STYLE
    ELSE
       GO BOTTOM IN STYLE
       SKIP IN STYLE
    ENDIF
  ENDIF
ELSE    && If not base currency
*B602290,1 Add these lines to get the style price from the STYLE file [End]
  
  IF SEEK(lcStyle + lcStyCur, 'STYPRICE')
    lnRetPrice = STYPRICE.Price&lcPriceLvl
  ELSE
    lnRetPrice = -1
    IF !llNoAdd .AND. gfGetMemVar('LLSTYPRICE',oAriaApplication.ActiveCompanyID)
      STORE 0 TO lnPriceA, lnPriceB, lnPriceC
      DO FORM (oAriaApplication.ScreenHome + 'SY\STYPRICE');
         WITH lcStyle , lcStyCur , lcPriceLvl TO lnRetPrice
    ELSE
      LOCAL lcStyleTit
      lcStyleTit = ALLTRIM(gfItemMask("HI"))
      =oAriaApplication.MessageBox("TRM00344B00000" , "DIALOG" ,;
                                   lcStyleTit + " " + ALLTRIM(lcStyle) +;
                                   "|" + ALLTRIM(lcStyCur))
    ENDIF  
  ENDIF  
ENDIF    && End of IF ALLTRIM(lcStyCur) == ALLTRIM(gcBaseCurr)

*E300328,1 Restore environment
IF !llOpenfile
  SELECT STYPRICE
  SET ORDER TO (lnCurTag)
  IF !EMPTY(lcFilter)
    SET FILTER TO (lcFilter)
  ENDIF
  IF lnCurRec > 0
    GO lnCurRec
  ENDIF  
ENDIF
SELECT (lnCurAlias)
*E300328,1 Return price
RETURN lnRetPrice


*:---------------------------------------------------------------------
*! Name      : gfIsEdtble
*! Developer : Yasser Mohammed Aly - (YMA)
*! Date      : 04/27/97
*! Purpose   : Function to tell if a specific code is editable by the
*!             user or not.
*: Job ID    : E# 300631
*:---------------------------------------------------------------------
*: Calls              : None
*:---------------------------------------------------------------------
*: Passed Parameters  : lcField  -> The code to be checked.
*:                      lnFieldW -> Pointer to a numeric variable to 
*:                                  hold the field width.
*:---------------------------------------------------------------------
*: Example            : = gfIsEdtble("TERMS", @lnWidth)
*:---------------------------------------------------------------------
FUNCTION gfIsEdtble
PARAMETERS lcPField, lnFieldW, lcActvComp
LOCAL oIsEdtble

*B607903,1 MMT 12/24/2006 Fix bug of Field width in codes screen is not read from Codeset File [Start]
*!*	oIsEdtble = CREATEOBJECT('IsEdtble')
*!*	RETURN oIsEdtble.Do(lcPField, lnFieldW, lcActvComp)
PRIVATE llRetVal, lcDataDir
gcAct_Comp = oAriaApplication.ActiveCompanyId
gcDataDir  = oAriaApplication.DataDir
gcSysHome  = oAriaApplication.SysPath

LOCAL lnCurAlias
lnCurAlias = SELECT(0)

lcActvComp = IIF(TYPE("lcActvComp")#"C", gcAct_Comp, lcActvComp)
lcDataDir  = gcDataDir

IF !(lcActvComp == gcAct_Comp)
	lnRemResult = oAriaApplication.remotesystemdata.execute("Select * from syccomp where cComp_ID='"+lcActvComp+"'",'',"CompFile","",oAriaApplication.SystemConnectionString,3,"",SET("DATAS"))
	IF lnRemResult>=1
    LOCATE
  ENDIF  
  IF lnRemResult>=1 AND FOUND()
    lcDataDir = gfGetDataDir(ALLTRIM(CompFile.cCom_DDir))
  ENDIF
  USE IN CompFile
ENDIF  

lnRemResult = oAriaApplication.remotesystemdata.execute("Select * from SYDFIELD where CFLD_NAME='"+lcPField+"'",'',"FieldFile","",oAriaApplication.SystemConnectionString,3,"",SET("DATAS"))
LOCAL lnFieldWidth
IF lnRemResult>=1
  LOCATE
ENDIF  
llRetVal = IIF(lnRemResult>=1 AND FOUND(),;
	              ("EDITABLE" $ UPPER(ALLTRIM(FieldFile.mCodeInfo))), .F.)
lnFieldWidth = IIF(lnRemResult>=1 AND FOUND(),FieldFile.nFld_Wdth, 6)
USE (lcDataDir+"CODESET" ) IN 0 ORDER Fildname  AGAIN ALIAS CodeSetF
lnFieldW = IIF(SEEK(lcPField, "CodeSetF" ), CodeSetF.nfld_wdth,lnFieldWidth)
USE IN FieldFile
USE IN CodeSetF
SELECT(lnCurAlias)


RETURN llRetVal
*B607903,1 MMT 12/24/2006 Fix bug of Field width in codes screen is not read from Codeset File [End]

*!*************************************************************
*! Name      : gfAddCurSm
*! Developer : Haytham El-Sheltawi    
*! Date      : 11/29/1998
*! Purpose   : Function to add the currency symbol to a numeric value
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 1) Currency code.
*!                     2) Amount.
*!                     3) Number of decimal places.
*!*************************************************************
*! Example	      	 : .AddCurrencySymbol('USDLR' , 100.97 , 2)
*!*************************************************************
FUNCTION gfAddCurSm
PARAMETERS lcCurrCode, lnAmount, lnDecimals
LOCAL oAddCurrencySymbol

oAddCurrencySymbol = CREATEOBJECT('AddCurrencySymbol')
RETURN oAddCurrencySymbol.Do(lcCurrCode, lnAmount, lnDecimals)

*!********************************************************************
*! Name      : gfGetExSin
*! Developer : Mohamed Hassan
*! Date      : 11/27/95
*! Purpose   : Return Exchange Rate sign
*!********************************************************************
*! Parameters: lcCurrency  && Currency to define or return exh. rate for
*!             lcBaseCurr  && Variable to define base currency. 
*!             lcUntSin    && Pointer to unit sign character.
*!********************************************************************
*! Call      : 
*!********************************************************************
*! Returns   : * OR /
*!********************************************************************
*! Example   : lcExSign = gfGetExSin(@lcUntSin,'ENG')
*!             The user can pass the currency as a parametter 
*!             or the function is going to use the base currency.
*!             The function is going to return the exchnage rate
*!             sign.
*!********************************************************************
*
FUNCTION gfGetExSin
PARAMETERS lcUntSin, lcCurrency, lcBaseCurr
LOCAL lcReturn , llClose

IF TYPE('lcUntSin') = 'C' 
  lcUntSin = '/'
ENDIF
IF TYPE('lcBaseCurr') $ 'UL'
  lcBaseCurr = oAriaApplication.BaseCurrency
ENDIF
IF lcCurrency = lcBaseCurr
  RETURN '*'
ENDIF

*-- Hesham (Start)
*-- select data from syccurr remotely and procced the addcurrency
*-- normaly
** This file should always be in use if the system is multi currency,
** Sometimes it is closed by Apparel programs, so, if it is
** not used,open the file.
llClose = .F.
IF !USED('SYCCURR')
  llClose = .T.
  USE (oAriaApplication.SysPath+"SYCCURR") IN 0 ORDER TAG CCURRCODE
ENDIF  
=SEEK(lcBaseCurr,'SYCCURR','CCURRCODE')
lcReturn = IIF((SYCCURR.cCurMeth = 'M' .AND.;
               !(ALLTRIM(lcCurrency) $ SYCCURR.mCurExcept)) .OR.;
               SYCCURR.cCurMeth = 'D' .AND.;
               (ALLTRIM(lcCurrency) $ SYCCURR.mCurExcept) , '*' , '/')
*!*	IF llClose
*!*	  USE IN SYCCURR
*!*	ENDIF

*!*	lnRemResult = oAriaApplication.remotesystemdata.execute("Select * from SYCCURR where ccurrcode='"+lcBaseCurr+"'",'',"SYCCURRTMP","",oAriaApplication.SystemConnectionString,3)
*!*	IF lnRemResult=1
*!*	  LOCATE
*!*		lcReturn = IIF((SYCCURRTMP.cCurMeth = 'M' .AND.;
*!*	               !(ALLTRIM(lcCurrency) $ SYCCURRTMP.mCurExcept)) .OR.;
*!*	               SYCCURRTMP.cCurMeth = 'D' .AND.;
*!*	               (ALLTRIM(lcCurrency) $ SYCCURRTMP.mCurExcept) , '*' , '/')
*!*	  USE IN SYCCURRTMP
*!*	ENDIF

RETURN lcReturn

*!********************************************************************
*--Get Currency Symbel.
FUNCTION gfGetCurSmbl
PARAMETERS lcCurrency
*-- Hesham (Start)
*-- Open currency table remotely and get the currency symbol
	LOCAL lcFrnSmbl,lnAlias,llOpened
	llOpened = .F.
	lnAlias = SELECT()
	IF !USED('SYCCURR')
	  USE (oAriaApplication.SysPath+"SYCCURR") IN 0 ORDER TAG CCURRCODE
	  llOpened = .T.
	ENDIF  
	SET ORDER TO TAG CCURRCODE IN SYCCURR
	lcFrnSmbl = IIF(SEEK(lcCurrency,'SycCurr'),SycCurr.cCurrSmbl,"")
	IF llOpened
	  USE IN SycCurr
	ENDIF
*-- Hesham (Start)
*-- select data from syccurr remotely and procced the addcurrency
*-- normaly
llCloseFil = .F.
IF !USED('SYCCURR')
  llCloseFil = .T.
  USE (oAriaApplication.SysPath+'SYCCURR') IN 0 ORDER TAG CCURRCODE
ENDIF

*!*	LOCAL lcFrnSmbl,lnAlias

*!*	lnAlias = SELECT()

*!*	lcFrnSmbl = ""
*!*	lnRemResult = oAriaApplication.remotesystemdata.execute("Select * from SYCCURR where ccurrcode='"+lcCurrency+"'",'',"SYCCURRTMP","",oAriaApplication.SystemConnectionString,3)
*!*	IF lnRemResult=1
*!*	  LOCATE
*!*	  lcFrnSmbl = IIF(FOUND(),SycCurrTMP.cCurrSmbl,"")
*!*		USE IN SYCCURRTMP
*!*	ENDIF	
*-- Hesham (End)
SELECT(lnAlias)
RETURN lcFrnSmbl

*!*************************************************************
*! Name      : gfChkRate
*! Developer : Hesham El-Sheltawi
*! Date      : 10/09/95
*! Purpose   : Return Exchange Rate for Currency in spec. date
*!*************************************************************
*! Parameters: lcExUnit    && hold variable name to return Currency Units
*!             lcCurrency  && Currency to define or return exh. rate for
*!             ldDate      && Date to define or return exch. rate for
*!             llDispMsg   && Display message or not
*!             lcCompID    && company id to use its settings
*!             lcBaseCurr  && The currency that you want to use as default.
*E300309,1     llNoErrMsg  && .T. if the default error message is
*E300309,1                     not to be displayed, .F. otherwise.
*!*************************************************************
*! Call      : gfGetMemVar
*!*************************************************************
*! Returns            : VALUE OF exchage rate
*!*************************************************************
*! Example   : lcVarName=gfChkRate("lcEngUnit",'ENG',DATE(),.T.)
*!             WILL return from the sycexch file the exchange rate
*!             value for the currency "ENG" at the system date 
*!             and its units in variable called lcEngUnit
*!*************************************************************
FUNCTION gfChkRate
*E300309,1 RENEE 11/15/95. Add a parameter to control the display
*E300309,1                 of the message that is displayed if a 
*E300309,1                 valid exchange rate is not found.
*E300309,1                 parameter : llNoErrMsg
*
*E300336,1 RENEE 01/08/96. Enhance performance as concerning to speed
*E300336,1                 of execution.
*E300309,1 Add parameter llNoErrMsg  
*PARAMETERS lcExUnit,lcCurrency,ldDate,llDispMsg,lcCompID,lcBaseCurr
PARAMETERS lcExUnit,lcCurrency,ldDate,llDispMsg,lcCompID,lcBaseCurr, llNoErrMsg
*E300309,1 end.

LOCAL lnCurDataSess

PRIVATE llExUsedBy,lcOldAlias,lcOldTag,lnRetRate,ldCurrDay,lcOldFlt,;
        llCurUsedBy, lnExRate
lnCurDataSess = SET("Datasession")         
lnRetRate  = 0
lcCompID   = IIF(TYPE('lcCompId') <> 'C' ,oAriaApplication.ActiveCompanyID , lcCompID)
lcBaseCurr = PADR(IIF(TYPE('lcBaseCurr') <> 'C',oAriaApplication.BaseCurrency,lcBaseCurr),3)
lcOldAlias  = SELECT()
llCurUsedBy = .F.
*!*	SET DATASESSION TO 1
IF lcCurrency = lcBaseCurr
  IF TYPE('lcExUnit') = 'C'
    &lcExUnit = 1
*!*	    SET DATASESSION TO lnCurDataSess
    RETURN 1.0000    
  ENDIF
ENDIF
IF !USED('SYCCURR')
  llExUsedBy=.T.
  SELECT 0
  USE (oAriaApplication.SysPath + 'SYCCURR') 
ELSE
  SELECT SYCCURR
ENDIF

llExUsedBy=.F.
IF !USED('SYCEXCH')
  llExUsedBy=.T.
  SELECT 0
  USE (oAriaApplication.SysPath + 'SYCEXCH') 
ELSE
  SELECT SYCEXCH
ENDIF

lcOldFlt=FILTER()
lcOldTag=TAG()
*E300336,1 Set index descendingly
*SET ORDER TO TAG CURRENCY
SET ORDER TO TAG CURRENCY DESCENDING
*E300336,1 Get current NEAR setting
lcSetNear = SET('NEAR')
SET NEAR ON 
*E300336,1 end.
SET FILTER TO
IF SEEK(lcBaseCurr+PADR(lcCurrency,3)+DTOS(ldDate))
  lnRetRate= nExRate
ELSE
  STORE .F. TO LLMULCURR,LLEXCHRATE
  STORE 0 TO LNEXRATDAY
 
  lnNoVar    = gfGetMemVar('LLMULCURR,LLEXCHRATE,LNEXRATDAY' , lcCompID)
  *E300336,1 Using set near with a descending index places the record
  *E300336,1 pointer on the next best match. Remarked the following,
  *ldCurrDay={}
  *llFound = .F.
  *lnCount = 1
  *DO WHILE !llFound AND lnCount<=lnExratDay
  *  LOCATE FOR CBASECURR+CCURRCODE+DTOS(DRATEDATE) = lcBaseCurr+PADR(lcCurrency,5)+DTOS(ldDate-lnCount)
  *  llFound = FOUND()
  *  lnCount = lnCount + 1
  *ENDDO  
  *IF llFound
  *    lnRetRate= nExRate    
  *E300336,1 Check the validity of the closest matching record
  IF cBaseCurr + cCurrCode = lcBaseCurr + PADR(lcCurrency,3) ;
    .AND. dRateDate >= ldDate - lnExRatDay
    lnRetRate = nExRate    
  *E300336,1 end. 
  ELSE
    IF llExchRate AND llDispMsg
      DO FORM SYCHRATE WITH lcCurrency, lcBaseCurr, ldDate,SET("Datasession") TO lnRetRate
      *ASM, After Enabling the Close Button in SYCHRATE.SCX we must handle the case of Closing the screen without Ok or Cancel [Start]
      lnRetRate=IIF(TYPE('lnRetRate')<>'N',0,lnRetRate)
      *ASM, After Enabling the Close Button in SYCHRATE.SCX we must handle the case of Closing the screen without Ok or Cancel [End]
    ELSE
      *E300309,1 Display the default error message only if 
      *E300309,1 llDispMsg is .T. and llNoErrMsg is .F.
      *IF llDispMsg
      IF llDispMsg .AND. !llNoErrMsg
      *E300309,1 end.
        ** Message : "The last defined excahnge rate exceeds     "
        **           "the valid number of days."+CHR(13)+CHR(10)+"
        **           "The currency will be defaulted to the base "
        **           "currency.                                  "
        **           "                       ® Ok ¯              "
        =oAriaApplication.MessageBox("TRM00249B00000","DIALOG")
      ENDIF
    ENDIF
  ENDIF  
ENDIF
IF TYPE('lcExUnit') = 'C'
  &lcExUnit = LOOKUP(SYCCURR.NCURRUNIT,lcCurrency,SYCCURR.CCURRCODE,"CCURRCODE")
ENDIF
SELECT SYCEXCH
IF !EMPTY(lcOldTag)
  SET ORDER TO TAG (lcOldTag)
ENDIF
SET FILTER TO &lcOldFlt
IF llExUsedBy
  USE IN SYCEXCH
ENDIF
IF llCurUsedBy
  USE IN SYCCURR
ENDIF

*E300336,1 Restore near settings
SET NEAR &lcSetNear
*E300336,1 end.

SELECT (lcOldAlias)
*!*	SET DATASESSION TO lnCurDataSess
RETURN lnRetRate 


*!*************************************************************************
*! Name      : gfCrtTmp
*! Developer : Hesham El-Sheltawi 
*! Date      : 12/08/97    
*! Purpose   : to Create uncomplete session temprory files
*!*************************************************************************
*: Calls       :
*!*************************************************************************      
*: Passed parameters  : lcFileStruc
*:                      lcTagExp
*:                      lcTag
*:*************************************************************************
*! Returns   :  Temprary file name
*:*************************************************************************     
*:E302130,1 Hesham El Sheltawy 03/30/2003
*:E302130,1 Make function create cursors and remove closing and then reopening the tables
FUNCTION gfCrtTmp
*-- E302130,1 Hesham (Start)
*-- Add new parameter to differnciate between creating table and cursor
*PARAMETERS lcFile,lcFileStruc,lcTagExp,lcTag
PARAMETERS lcFile,lcFileStruc,lcTagExp,lcTag,llCursor
*-- E302130,1 (End)
PRIVATE lcFileType,lcOnError,llError,laFileStruc,lcFileName,lnWorkArea,lcTagType,lnCount
*-- E302130,1 Hesham (Start)
*-- E302130,1 if the llcursor parameter is not defined or passed with wrong value
IF TYPE('llCursor')#"L"
  llCursor = .F.
ENDIF
*-- E302130,1 Hesham (End)
lnWorkArea = SELECT()
lcFileName = IIF(TYPE('lcFile')='C',lcFile,gfTempName())
lcOnError = ON('ERROR')
llError = .F.
ON ERROR llError = .T.
lcFileType = 'A'
lcFileType = IIF(TYPE("lcFileStruc[1]")#"U",'A',;
             IIF(LEFT(ALLT(lcFileStruc),1)='(','S','F'))
ON ERROR &lcOnError
DO CASE
  CASE lcFileType = 'F'
    SELECT (lcFileStruc)
    =AFIELDS(laFileStruc)
    lcFileType = 'A'
  CASE lcFileType= 'A'
    =ACOPY(lcFileStruc,laFileStruc)
    *-- E302130,1 Hesham (Start)
    *-- E302130,1 Check for version if later than or equal 8 then add new columns to the array
    *-- E302130,1 if needed
    IF VAL(LEFT(VERSION(4),2))>=8 AND ALEN(laFileStruc,2)<=16
      LOCAL lnCount,lnFCount
      DIMENSION laFileStruc[ALEN(lcFileStruc,1),18]
      STORE "" TO laFileStruc
      FOR lnCount = 1 TO ALEN(lcFileStruc,1)
        FOR lnFCount = 1 TO 4
           laFileStruc[lnCount,lnFCount] = lcFileStruc[lnCount,lnFCount]
        ENDFOR
        laFileStruc[lnCount,17] = 0
        laFileStruc[lnCount,18] = 0
      ENDFOR
    ENDIF
    *-- E302130,1 Hesham (eND)
ENDCASE
lcTagType = 'A'
lcTagType = IIF(TYPE("lcTagExp[1]")#"U",'A','S')
*-- E302130,1 Hesham (Start)
*-- E302130,1 create cursor if llCursor was passed
*IF lcFileType = 'A'
*  CREATE TABLE (oAriaApplication.WorkDir+lcFileName) FROM ARRAY laFileStruc
*ELSE
*  CREATE TABLE (oAriaApplication.WorkDir+lcFileName) &lcFileStruc
*ENDIF
IF llCursor
  IF lcFileType = 'A'
    CREATE CURSOR (lcFileName) FROM ARRAY laFileStruc
  ELSE
    CREATE CURSOR (lcFileName) &lcFileStruc
  ENDIF
ELSE
  IF lcFileType = 'A'
    CREATE TABLE (oAriaApplication.WorkDir+lcFileName) FROM ARRAY laFileStruc
  ELSE
    CREATE TABLE (oAriaApplication.WorkDir+lcFileName) &lcFileStruc
  ENDIF
ENDIF
*-- E302130,1 Hesham (End)
SELECT (lcFileName)
IF lcTagType = 'A'
  FOR lnCount = 1 TO ALEN(lcTagExp,1)
    INDEX ON &lcTagExp[lnCount,1] Tag &lcTagExp[lnCount,2]
  ENDFOR
ELSE
  IF TYPE('lcTagExp') = 'C' AND TYPE('lcTag') <> 'C'
    lcTag = Field(1)
  ENDIF
  IF TYPE('lcTagExp') = 'C' AND TYPE('lcTag') = 'C'
    INDEX ON &lcTagExp Tag &lcTag
  ENDIF
ENDIF  
*-- E302130,1 Hesham (Start)
*USE
*USE (oAriaApplication.WorkDir+lcFileName) EXCLUSIVE 
*-- E302130,1 Hesham (End)
IF lcTagType = 'A'
  SET ORDER TO TAG &lcTagExp[1,2]
ELSE
  IF TYPE('lcTagExp') = 'C' AND TYPE('lcTag') = 'C'
    SET ORDER TO TAG (lcTag)
  ENDIF
ENDIF  
SELECT (lnWorkArea)
RETURN lcFileName

*!*************************************************************
*! Name      : gfUserPriv
*! Developer : Hesham El-Sheltawi
*! Date      : 04/10/97
*! Purpose   : To get user priv. on process or subprocess.
*!             function to check the user priv. on specific
*!             process & subprocess and return the users
*!             accessebility on it
*!*************************************************************
*! Calls     : gfTempName,gfSubStr
*!*************************************************************
*! Passed Parameters  : lcModule    Module ID
*!                      lcProcess   Process ID
*!                      lcSubProc   SubProcess
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : if gfUserPriv('IC','ICSTYLE','COSTING')
*!*************************************************************
FUNCTION gfUserPriv
PARAMETERS lcModule,lcProcess,lcSubProc

IF EMPTY(lcModule) OR EMPTY(lcProcess) OR PARAMETERS()<2
  RETURN .F.
ENDIF
IF ! oAriaApplication.LoginRequired OR oAriaApplication.User_Level='A'
  RETURN .T.
ENDIF

PRIVATE lcTmpPriv,lcOldAlias,llRetrun,laUsrSubP,lcSqlStat
lcTmpPriv = gfTempName()
lcOldAlias = SELECT()
SELECT 0
*USE (oAriaApplication.SysPath+'SYUUSRPR') AGAIN ALIAS &lcTmpPriv ORDER TAG CUSER_ID

*llRetrun = SEEK(ALLTRIM(oAriaApplication.User_ID)+UPPER(lcModule+oAriaApplication.ActiveCompanyID+lcProcess)) .OR. ;
*           SEEK(ALLTRIM(oAriaApplication.User_Group)+UPPER(lcModule+oAriaApplication.ActiveCompanyID+lcProcess))

lcSqlStat = "Select * from SYUUSRPR WHERE ((cUser_ID = '"+oAriaApplication.User_ID+[' AND CGRPORUSER= "U")]
IF !EMPTY(oAriaApplication.User_Group)
lcSqlStat = lcSqlStat +" OR (cUser_ID = '"+oAriaApplication.User_Group+[' AND CGRPORUSER= "G")]
ENDIF
lcSqlStat = lcSqlStat + ") AND CAPP_ID='"+lcModule+"' AND CCOMP_ID='"+oAriaApplication.ActiveCompanyID+"'"+;
            " AND CPROSS_ID='"+lcProcess+"'"

lnRemResult = oAriaApplication.remotesystemdata.execute(lcSqlStat,'',lcTmpPriv,"",oAriaApplication.SystemConnectionString,3,"",SET("Datasession"))
IF lnRemResult =1 
  LOCATE
  *B125765 ,1 HMA  12/21/2004 assign initial value to the variable llReturn. [BEGIN]
  * llRetrun = IIF(TYPE('lcSubProc')='C',.F.,llRetrun)
  llRetrun = IIF(TYPE('lcSubProc')='C',.F.,!EOF())
  *B125765 ,1 HMA  12/21/2004 assign initial value to the variable llReturn. [END]


  *--Be sure that no empty string is in the memo field to have subprocess.
  IF !EMPTY(ALLTRIM(mSubProc)) AND TYPE('lcSubProc')='C'
     DIMENSION laUsrSubP[1,2]
     =gfSubStr(LEFT(mSubProc,ATC('|',mSubProc)-1),@laUsrSubP)
     llRetrun = ASCAN(laUsrSubP,UPPER(lcSubProc))>0
  ENDIF

  USE IN (lcTmpPriv)
ELSE
  llRetrun = .F.  
ENDIF  
SELECT (lcOldAlias)
RETURN llRetrun




*!*****************************************************************
*! Name : gfBrowWare.
*! Auth : Yasser Mohammed Aly (YMA).
*! Date : 04/17/94.
*!*****************************************************************
*! Synopsis : Browse the warehouse file.
*!*****************************************************************
*! Passed :
*!        Parameters : 
*!         llIsEscape: If the escape is allowed.
*!******************************************************************
*! Files      : WareHous.DBF should be opend.
*!******************************************************************
*! Returned : 
*!        VARIABLES : 
*!        lcWareCode : The warehouse code that will be selected
*!                     if it's empty it means that the user did
*!                     not select anything.
*!******************************************************************
*! Example :
*!        =gfBrowWare(.T.)
*!******************************************************************
FUNCTION gfBrowWare
PARAMETERS llIsEscape, lcItem, lcColor, lcWareCode, lcSrcFile,lcStyMatIn,llForCurrSite

PRIVATE lcBrWinName, lcBrTtl, lcWareCode, lcItem, lcColor, lcForCond;
        lnCurAlias, lnCurTag

*-- Include the .H file
#INCLUDE R:\ARIA4XP\PRGS\SY\WAREBROW.H
IF !USED('WareHous')
  =gfOpenFile(oAriaApplication.DataDir+'WareHous','WareHous','SH')
ENDIF
GO TOP IN WareHous
IF EOF('WareHous')
  =MESSAGEBOX(LANG_NOLOC,16,_Screen.Caption)
  RETURN SPACE(6)
ENDIF

*-- Save the current alias.
lnCurAlias = SELECT()

*!B999999,1 WSH 03/08/2005, Create a variable to hold warehouse to locate at. [Start]
LOCAL lcGoToWare
lcGoToWare = lcWareCode
*!B999999,1 WSH 03/08/2005, [End]

SELECT WAREHOUS
lnCurTag    = VAL(SYS(21))
SET ORDER TO TAG WAREHOUS

lcWareCode  = IIF(EMPTY(lcWareCode), SPACE(6), lcWareCode)
*-- If called from browsing the warehouses of a specific item/color.
IF !EMPTY(lcSrcFile)
  lcColor     = IIF(EMPTY(lcColor), SPACE(6), lcColor)
  IF UPPER(ALLTRIM(LEFT(lcSrcFile,1))) = 'S'
    lcSrcFile = 'STYDYE'
    lcItem    = IIF(EMPTY(lcItem), SPACE(12), PADR(lcItem,12))
    lcFld     = 'Style'
    lcTMsg    = LANG_STYLE
    llFound   = .F.
    IF SEEK(lcItem , lcSrcFile)
      SELECT (lcSrcFile)
      LOCATE REST FOR EMPTY(Dyelot) WHILE &lcFld = lcItem 
      llFound = FOUND()
    ENDIF  
    IF !llFound
      =MESSAGEBOX(lcTMsg + ALLTRIM(lcItem) +' '+LANG_NOTASN,64)
      SET ORDER TO lnCurTag IN WAREHOUS
      SELECT (lnCurAlias)
      RETURN SPACE(6)
    ENDIF
    SELECT WAREHOUS
    SET RELATION TO lcItem + cWareCode + SPACE(10) INTO (lcSrcFile) ADDITIVE
  ELSE
   lcSrcFile = 'FABDYE'
   lcItem    = IIF(EMPTY(lcItem), SPACE(7), PADR(lcItem,7))   
   lcFld     = 'Fabric'
   lcTMsg    = LANG_ITMCLR
   llFound   = .F.
   IF SEEK(lcItem + lcColor , lcSrcFile)
     SELECT (lcSrcFile)
     LOCATE REST FOR EMPTY(Dyelot) ;
            WHILE &lcFld + Color = lcItem + lcColor
     llFound = FOUND()
   ENDIF  
   IF !llFound
     =MESSAGEBOX( lcTMsg + ALLTRIM(lcItem) + '/' + ALLTRIM(lcColor) +LANG_NOTASN,64)
     SET ORDER TO lnCurTag IN WAREHOUS
     SELECT (lnCurAlias)
     RETURN SPACE(6)
   ENDIF
   SELECT WAREHOUS
   SET RELATION TO lcItem + lcColor + cWareCode + SPACE(10) ;
                INTO (lcSrcFile) ADDITIVE
  ENDIF   
  lcForCond   = '!EOF(lcSrcFile)'
ELSE
  lcWareCode = SPACE(6)  
  lcForCond  = ''
ENDIF  


IF TYPE('lcStyMatIn')='C' AND lcStyMatIn $ 'SM'
  lcMatOrSty = IIF(UPPER(lcStyMatIn) = 'S','lStyInv','lMatInv')
  lcForCond  = lcForCond +IIF(EMPTY(lcForCond),'',' AND ') + lcMatOrSty
ENDIF

IF llForCurrSite
  lcForCond  = lcForCond +IIF(EMPTY(lcForCond),'',' AND ') + "cSiteId = oAriaApplication.CurrentSite"
ENDIF


*B123842,1 WSH Browse Locations Using Aria Global Browse Screen. [Start]
*DO FORM (oAriaApplication.ScreenHome+"IC\Warebrow.scx") WITH lcForCond TO lcWareCode

LOCAL llWasSel
DIMENSION laWarData[1]
laWarData = ''

SELECT WAREHOUS
PRIVATE lcBrFields
lcBrFields = "cWareCode:H='"+LANG_ARIA_LOCATION+"':17,cDesc:H='"+LANG_ARIA_DESCRIPTION+"':30"

*!B999999,1 WSH 03/08/2005, Locate at the first match for lcWareCode. [Start]
IF !EMPTY(lcGoToWare)
  LOCAL lnSoftSeek
  lcGoToWare = RTRIM(lcGoToWare)
  lcGoToWare = IIF(RIGHT(lcGoToWare,1) == '?', SUBSTR(lcGoToWare, 1, LEN(lcGoToWare) - 1), lcGoToWare)

  IF !SEEK(lcGoToWare)
    lnSoftSeek = RECNO(0)
    IF lnSoftSeek > 0
      GO lnSoftSeek
    ELSE
      LOCATE
    ENDIF
  ENDIF
ENDIF
*!B999999,1 WSH 03/08/2005, [End]
  
*N120718,1 HBG 08/10/2004 FIX bug variable 'FOR' not found [Begin]
*llWasSel = ARIABROW("'' FOR "+lcForCond,LANG_ARIA_LOCTTL,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,"","","CWARECODE","laWarData")
*khm
*llWasSel = ARIABROW(lcForCond,LANG_ARIA_LOCTTL,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,"","","CWARECODE","laWarData")
llWasSel = ARIABROW(IIF(!EMPTY(lcForCond),"'' FOR "+lcForCond,''),LANG_ARIA_LOCTTL,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,"","","CWARECODE","laWarData") 
*khm
*N120718,1 [End]
IF llWasSel
  lcWareCode = laWarData[1]
ENDIF
*B123842,1 WSH [End]

IF !EMPTY(lcSrcFile)
  SET RELATION OFF INTO (lcSrcFile)
ENDIF  

SET ORDER TO lnCurTag IN WAREHOUS

SELECT (lnCurAlias)
POP KEY
lcWareCode = IIF(EMPTY(lcWareCode),SPACE(6),lcWareCode)
RETURN (lcWareCode)




*!*************************************************************
*! Name : gpAdStyWar
*! Auth : Yasser Mohammed Aly (YMA).
*! Date : 05/03/94.
*!*************************************************************
*! Synopsis : Add a new record to the StyDye file.
*!*************************************************************
*! Passed :
*!        Parameters : 
*!          lcPStyle : The style.   
*!          lcPColor : The color.
*!          lcPDyelot: The Dyelot.
*!          lcPWare  : The Warehouse.
*!        Files      : The StyDye File should be opened.
*!                     The WareHouse File should be opened.
*!*************************************************************
*! Returned : 
*!        Files      : The StyDye File after appending the new record.
*!*************************************************************
*! Example :
*!        DO gpAdStyWar WITH lcStyle,lcColor,SPACE(10),lcWareCode
*!*************************************************************
PROCEDURE gpAdStyWar

PARAMETERS lcPStyle, lcPDyelot, lcPWare
PRIVATE lcDesc, lcAlias, lnCost , lcDiscCods

lcAlias = ALIAS()
*N037578,1 KHM 09/30/2004 Open the style file if it not open before, because in 
*N037578,1                some programs we use fox files remotely [Begin]
PRIVATE llOpnStyFil
llOpnStyFil = .F.
IF !USED('STYLE')
  llOpnStyFil = .T.
 = gfOpenFile(oAriaApplication.DataDir+'STYLE','STYLE','SH')
ENDIF 
*N037578,1 KHM 09/30/2004 [End]

SELECT STYLE
IF SEEK (lcPStyle)
  lcDesc = Desc
  lnCost = Ave_Cost 
  lcDiscCods  = cDiscCode
  *!B038241,1 AKA Update the GLLink Code with DEFDEF with the default Style Link Code [Start]
  lcGlCode = Link_Code
  *!B038241,1 AKA Update the GLLink Code with DEFDEF with the default Style Link Code [Start]
ELSE
  lcDesc = SPACE(20)
  lnCost = 0 
  lcDiscCods  = cDiscCode  
  *!B038241,1 AKA Update the GLLink Code with DEFDEF with the default Style Link Code [Start]
  lcGlCode = ""
  *!B038241,1 AKA Update the GLLink Code with DEFDEF with the default Style Link Code [Start]
ENDIF

STORE .F. TO llSTYDYE,llSTYINVJL
IF !USED('STYDYE')
  llSTYDYE   = .T.
  =gfOpenFile(oAriaApplication.DataDir+'STYDYE','STYDYE','SH')
ENDIF
IF !USED('STYINVJL')
  llSTYINVJL = .T.
  =gfOpenFile(oAriaApplication.DataDir+'STYINVJL','STYINVJL','SH')
ENDIF
=gfOpenFile(oAriaApplication.DataDir+'WareHous','WareHous','SH')
*! B609677,1 MMT 10/03/2011 Fix bug of wrong scale in SO screen after adding style to location [Start]
lcStyDyRel = ''
lcStyleRel = ''
*! B609677,1 MMT 10/03/2011 Fix bug of wrong scale in SO screen after adding style to location [END]

SELECT STYDYE
*! B609677,1 MMT 10/03/2011 Fix bug of wrong scale in SO screen after adding style to location [Start]
lcStyDyRel = SET("Relation")
*! B609677,1 MMT 10/03/2011 Fix bug of wrong scale in SO screen after adding style to location [END]

SET RELATION TO
SET RELATION TO style+cwarecode INTO STYINVJL ADDITIVE

SELECT STYLE
*! B609677,1 MMT 10/03/2011 Fix bug of wrong scale in SO screen after adding style to location [Start]
lcStyleRel =SET("Relation")
*! B609677,1 MMT 10/03/2011 Fix bug of wrong scale in SO screen after adding style to location [END]

SET RELATION TO
SET RELATION TO style INTO STYDYE ADDITIVE



SELECT WareHous
*!B038241,1 AKA Update the GLLink Code with DEFDEF with the default Style Link Code [Start]
*lcGlCode = IIF(SEEK(lcPWare),GL_LINK,'DEFDEF')
lcGlCode = IIF(EMPTY(lcGlCode),'DEFDEF',lcGlCode)
*!B038241,1 AKA Update the GLLink Code with DEFDEF with the default Style Link Code [End]

SELECT StyDye
APPEND BLANK
REPLACE Style      WITH lcPStyle  ,;
        Desc       WITH lcDesc    ,;
        cDiscCode  WITH lcDiscCods,;
        Dyelot     WITH lcPDyelot ,;
        cWareCode  WITH lcPWare   ,;
        Ave_Cost   WITH IIF(EMPTY(Dyelot),lnCost,0) ,;
        GL_Link    WITH lcGlCode
=gfAdd_Info('STYDYE')

*C200171 TMI [Start] Gen Upcs in EDICATGD
*-- Run if EDI installed
IF ASCAN(oAriaApplication.laEvntTrig,PADR("GNUPCWH",10)) <> 0
IF OCCURS('NC',oAriaApplication.CompanyInstalledModules)<>0
  lcWhCode   = lcPWare
  lcSty      = lcPStyle
  llFrmAdWre = .T.
  =gfDoTriger('ICSTYLE','GNUPCWH   ')
ENDIF
ENDIF
*C200171 TMI [End  ] Gen Upcs in EDICATGD


IF !EOF('STYDYE') .AND. EOF('STYINVJL')
  SELECT STYDYE
  REPLACE REST WHILE STYLE = STYLE.STYLE ;
          FOR gfTraceKey("STYDYE",Style+cWareCode+Dyelot,"M") ;
          AVE_COST WITH IIF(EMPTY(Dyelot),STYLE.AVE_COST,0)
ENDIF                   

SELECT STYDYE
SET RELATION TO
*! B609677,1 MMT 10/03/2011 Fix bug of wrong scale in SO screen after adding style to location [Start]
IF !EMPTY(lcStyDyRel)
  SET RELATION TO &lcStyDyRel
ENDIF
*! B609677,1 MMT 10/03/2011 Fix bug of wrong scale in SO screen after adding style to location [END]

SELE STYLE
SET RELATION TO
*! B609677,1 MMT 10/03/2011 Fix bug of wrong scale in SO screen after adding style to location [Start]
IF !EMPTY(lcStyleRel)
  SET RELATION TO &lcStyleRel
ENDIF
*! B609677,1 MMT 10/03/2011 Fix bug of wrong scale in SO screen after adding style to location [END]

*-- Close files
IF llSTYDYE
  USE IN STYDYE
ENDIF
IF llSTYINVJL
  USE IN STYINVJL
ENDIF

*N037578,1 KHM 09/30/2004 Close the file if its opened in this session [Begin]
IF llOpnStyFil
  USE IN Style
ENDIF
*N037578,1 KHM 09/30/2004 [End]

IF !EMPTY(lcAlias)
  SELECT (lcAlias)
ENDIF
RETURN


*!*************************************************************
*! Name : gpAdFabWar
*! Auth : Yasser Mohammed Aly (YMA).
*! Date : 06/28/94.
*!*************************************************************
*! Synopsis : Add a new record to the FabDye file.
*!*************************************************************
*! Passed :
*!        Parameters : 
*!          lcFab  : The fabric.   
*!          lcClr  : The color.
*!          lcDye  : The Dyelot.
*!          lcWare : The Warehouse.
*!        Files    : The FabDye File should be opened.
*!                   The WareHouse File should be opened.
*!*************************************************************
*! Returned : 
*!        Files      : The FabDye File after appending the new record.
*!*************************************************************
*! Example :
*!        DO gpAdFabWar WITH lcfabric,lcColor,SPACE(10),lcWareCode
*!*************************************************************
PROCEDURE gpAdFabWar 

PARAMETERS lcFab, lcClr, lcDyel, lcWare, lcTmpScope

PRIVATE lcDesc, lnAlias, llGlLink
lnAlias = SELECT()
lcGlCode = SPACE(3)
llUsed = .F.
IF !USED('')
  llUsed = gfOpenFile(oAriaApplication.DataDir+'FABRIC','FABRIC','SH')
ENDIF
= SEEK(lcFab+lcClr,'FABRIC')

SELECT FabDye
APPEND BLANK
llLock = RLOCK()
REPLACE Fabric     WITH lcFab    ,;
        Color      WITH lcClr    ,;
        Dyelot     WITH lcDyel   ,;
        cWareCode  WITH lcWare   ,;
        GL_Link    WITH lcGlCode ,;
        nfAve_Cost WITH IIF(EMPTY(lcDyel),Fabric.nfAve_Cost,0),;
        nAveCstBuy WITH IIF(EMPTY(lcDyel),Fabric.nAveCstBuy,0)
UNLOCK        
=gfAdd_Info('FABDYE')

IF llUsed
  USE IN FABRIC
ENDIF

IF TYPE('lcTmpScope') $ 'UL'
  lcTmpScope = ''
ENDIF

IF !EMPTY(lcDyel)
  lcFab = PADR(lcFab,7)
  lcClr = PADR(lcClr,6)
  lcDyel= PADR(lcDyel,10)
  
  llDyeRel = gfOpenFile(oAriaApplication.DataDir+"Dye_Rel","Dye_Rel","SH")
  SELECT Dye_Rel
  SET ORDER TO Dye_Rel

  *-- if you did not find this fabric, color, dyelot record 
  IF !SEEK(lcFab + lcClr + lcDyel)
    PRIVATE lnNearest,lcDye_Seq
    lcDye_Seq = ''
    lnNearest = RECNO(0)
    
    *-- Add this block to adjust add records at top of file. [begin]
    IF (lnNearest # 0)
      GO lnNearest
      IF FABRIC + COLOR != lcFab + lcClr
        SET ORDER TO Dye_Rel DESCENDING
        = SEEK(lcFab + lcClr + lcDyel)
        lnNearest = RECNO(0)
        IF lnNearest # 0
          GO lnNearest
          IF (FABRIC + COLOR != lcFab + lcClr) OR (lcDyel > DYELOT)
            lnNearest = 0
          ENDIF
        ENDIF     
      ENDIF  
    ENDIF    
    *-- Add this block to adjust add records at top of file. [end]

    *-- if it is Last dyelot code.
    IF lnNearest = 0
      SET ORDER TO SEQUENCE DESCENDING
      = SEEK(lcFab + lcClr)
      lcDye_Seq = PADL(ALLTRIM(STR(VAL(cDye_Seq) + 1)),4,'0')
    ENDIF  

    SET ORDER TO
    IF lnNearest # 0
      GO lnNearest
      lcDye_Seq = cDye_Seq

      SCAN FOR FABRIC + COLOR + cDye_Seq = lcFab + lcClr AND cDye_Seq >= lcDye_Seq
        REPLACE cDye_Seq WITH PADL(ALLTRIM(STR(VAL(cDye_Seq) + 1)),4,'0')
      ENDSCAN 
    ENDIF
    
    *-- insert new dyelot line.
    SELECT DYE_REL
    INSERT INTO ('Dye_Rel')                                 ;
             (FABRIC, COLOR, DYELOT, cDye_Seq , cTmpScope ) ;
      VALUES (lcFab , lcClr, lcDyel, lcDye_Seq, lcTmpScope)     
    =gfAdd_Info('DYE_REL')
  ENDIF
  
  IF llDyeRel
    USE IN DYE_REL
  ENDIF

ENDIF

=SELECT(lnAlias)
RETURN

*!*************************************************************
*! Name      : gfSheetItem
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Generate, Modify or Delete cost sheet items for style
*!*************************************************************
*! Calls     : gfGetMemVar,gfModalGen,gfItemMask,gpAdStyWar,gpAdFabWar,lfSelDyelot
*!*************************************************************
*! Parameters: lcTranType : Transaction type   ('M'-'I'-'T')
*!             lcTicketNo : Ticket number
*!             lcLinkCode : WIP Link code
*!             lcItem     : Style/Fabric
*!             lcColor    : Color
*!             lnLineNo   : Ticket line number
*!             lcDyelot   : Dyelot
*!             lcStyWare  : Default Style Issue Warehouse
*!             lcMatWare  : Default Fabric Issue Warehouse
*!             laQty      : Quantity array
*!             lcTmpBom   : Style Cost Sheet file name
*!             lcTktSheet : Cost sheet header file name
*!             lcDetFile  : Cost sheet detailed file name
*!             lcOprHdr   : Operation header file name
*!             lcLastOpr  : Last Operation
*!             lnPrice    : Purchase Price
*!             lnEst1     : Estimated cost 1
*!             lnEst2     : Estimated cost 2
*!             lnEst3     : Estimated cost 3
*!             lnEst4     : Estimated cost 4
*!             lnEst5     : Estimated cost 5
*!             lcTmpName  : Temp name needed to arrange new fabric dyelots
*!             llBackOnly : If .T. update all records in background.
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =gfSheetItem('M','000001','DEFDEF','ITEMD001','',0,'Dyelot',;
*!                           'WARE1',@laQty,100,'BOM','CTKTBOM','BOMLINE',;
*!                           'MFGOPRHD','000001',0,0,0,0,0)
*!:************************************************************************
*!
FUNCTION gfSheetItem

*N119813,1 AMH Add parameters for SQL tables and two parameters for cost element 6 and cost element 7 [Start]
*PARAMETERS lcTranType,lcTicketNo,lcLinkCode,lcItem,lcColor,lnLineNo,lcDyelot,;
           lcStyWare,lcMatWare,laQty,lcTmpSheet,lcTktSheet,lcDetFile,lcOprHdr,;
           lcLastOpr,lnPrice,lnEst1,lnEst2,lnEst3,lnEst4,lnEst5,lcTmpName,llBackOnly

*E038150,1 AMH Pass the inventory type instead of color [Start]
*PARAMETERS lcTranType,lcTicketNo,lcLinkCode,lcItem,lcColor,lnLineNo,lcDyelot,;
           lcStyWare,lcMatWare,laQty,lcTmpSheet,lcTktSheet,lcDetFile,lcOprHdr,;
           lcLastOpr,lnPrice,lnEst1,lnEst2,lnEst3,lnEst4,lnEst5,lnEst6,lnEst7,;
           lcPosHdr,lcPosLn,lcTmpName,llBackOnly
PARAMETERS lcTranType,lcTicketNo,lcLinkCode,lcItem,lcInvType,lnLineNo,lcDyelot,;
           lcStyWare,lcMatWare,laQty,lcTmpSheet,lcTktSheet,lcDetFile,lcOprHdr,;
           lcLastOpr,lnPrice,lnEst1,lnEst2,lnEst3,lnEst4,lnEst5,lnEst6,lnEst7,;
           lcPosHdr,lcPosLn,lcTmpName,llBackOnly
*E038150,1 AMH [End]

PRIVATE llStyconfg
*N119813,1 AMH [End]

*B608223,1 TMI [Start] check if the calling function is 'LFVGENERATE'
llSheetGen = (lfGtCallFn() == 'LFVGENERATE')
*B608223,1 TMI [End  ] 

PRIVATE    lcMjrMsk,lcMjrHdr,lcNMjrMsk,laItemSeg,lcClrMsk,lnClrPos,lcItemFile,;
           llDyelot,lcScale,lcItemType,lnTranQty,lcSizes,lnCount,lcCstClr,lnUnitPri,;
           lcCstItm,llContinue,laMfgRFld,lnLastSeq,lnSizePos,lcTmpBomSh,laReq,;
           lcContCode,lcContName,lcOperSeq,llInHouse,llMfgOpr,lnLeadTime
PRIVATE    lcMfgGlAcnt,lcExSign,lcUntSin,lnAlias,lcPriceCur,lnPriceRate,;
           lnPriceUnit,lcDutyCur,lnDutyRate,lnDutyUnit, lnUntQty,lcTranLett


STORE 1   TO lnPriceRate,lnPriceUnit,lnDutyRate,lnDutyUnit
STORE '/' TO lcExSign,lcUntSin

*B607915,1 MMT 12/28/2006 bug of error in Po screen while save[Start]
STORE 0 TO lnUnitPri
*B607915,1 MMT 12/28/2006 bug of error in Po screen while save[End]


*N119813,1 AMH Consider cases of Dye order and Inter-Location PO [Start]
*lcTranLett = IIF(lcTranType $ "IMT",lcTranType ,"I")
lcTranLett = IIF(lcTranType $ "IMTDN",lcTranType ,"I")
*N119813,1 AMH [End]

IF lcTranType $ 'IN' 
  
  *N119813,1 AMH Use the cursor name of the POSHDR file [Start]
  *=SEEK('P'+lcTicketNo,'POSHDR')
  *lcPriceCur  = IIF(EMPTY(POSHDR.cPriceCur),oAriaApplication.BaseCurrency,POSHDR.cPriceCur)
  *lnPriceRate = IIF(POSHDR.nPriceRat=0,1,POSHDR.nPriceRat)
  *lnPriceUnit = IIF(POSHDR.nCurrUnit=0,1,POSHDR.nCurrUnit)
  *lcDutyCur   = IIF(EMPTY(POSHDR.cDutyCur),oAriaApplication.BaseCurrency,POSHDR.cDutyCur)
  *lnDutyRate  = IIF(POSHDR.nDutyRat=0,1,POSHDR.nDutyRat)
  *lnDutyUnit  = IIF(POSHDR.nDCurUnit=0,1,POSHDR.nDCurUnit)
  lcPriceCur  = IIF(EMPTY(EVALUATE(lcPOSHDR+'.cPriceCur')),oAriaApplication.BaseCurrency,EVALUATE(lcPOSHDR+'.cPriceCur'))
  lnPriceRate = IIF(EVALUATE(lcPOSHDR+'.nPriceRat')=0,1,EVALUATE(lcPOSHDR+'.nPriceRat'))
  lnPriceUnit = IIF(EVALUATE(lcPOSHDR+'.nCurrUnit')=0,1,EVALUATE(lcPOSHDR+'.nCurrUnit'))
  lcDutyCur   = IIF(EMPTY(EVALUATE(lcPOSHDR+'.cDutyCur')),oAriaApplication.BaseCurrency,EVALUATE(lcPOSHDR+'.cDutyCur'))
  lnDutyRate  = IIF(EVALUATE(lcPOSHDR+'.nDutyRat')=0,1,EVALUATE(lcPOSHDR+'.nDutyRat'))
  lnDutyUnit  = IIF(EVALUATE(lcPOSHDR+'.nDCurUnit')=0,1,EVALUATE(lcPOSHDR+'.nDCurUnit'))
  *N119813,1 AMH [End]
  
ENDIF
STORE ''  TO M_WAREHOUSE,M_DYELOT,M_MATDYE,M_USEEXSSC

*N119813,1 AMH Use the correct company ID [Start]
*=gfGetMemVar('M_WAREHOUSE,M_DYELOT,M_MATDYE,M_USEEXSSC',gcAct_Comp)
=gfGetMemVar('M_WAREHOUSE,M_DYELOT,M_MATDYE,M_USEEXSSC',oAriaApplication.ActiveCompanyID)
llStyconfg = (ALLTRIM(gfGetMemVar('M_STYCNFG',oAriaApplication.ActiveCompanyID))='Y')
*N119813,1 AMH [End]

*E038150,1 AMH Consider the inventory type [Start]
*lcMjrMsk  = gfItemMask("PM")
*lcMjrHdr  = gfItemMask("HM")
*lcNMjrMsk = gfItemMask("PN")
*lcItmMsk  = gfItemMask("PI")
lcMjrMsk  = gfItemMask("PM","",lcInvType)
lcMjrHdr  = gfItemMask("HM","",lcInvType)
lcNMjrMsk = gfItemMask("PN","",lcInvType)
lcItmMsk  = gfItemMask("PI","",lcInvType)
*E038150,1 AMH [End]

*N119813,1 AMH Use the cursor name of the BOM file [Start]
*=gfOpenFile(oAriaApplication.DataDir+'BOM',oAriaApplication.DataDir+'BOM','SH')
SELECT (lcTmpSheet)
*N119813,1 AMH [End]

*E038150,1 AMH Consider the inventory type [Start]
*IF !SEEK(IIF(lcTranType='T',lcItem,SUBSTR(lcItem,1,LEN(lcMjrMsk))),lcTmpSheet)
IF !SEEK(lcInvType+SUBSTR(lcItem,1,LEN(lcMjrMsk)),lcTmpSheet)
  *E300725,1 Message : 38031
  *E300725,1 Cost sheet not found for style: xxx cannot generate cutting 
  *E300725,1 ticket cost sheet
  *E300725,1 Button : 00000
  *E300725,1 Ok
  *=gfModalGen('TRM38031B00000','ALERT',IIF(lcTranType='T','Fabric: ',lcMjrHdr+':')+;
              ALLTRIM(IIF(lcTranType='T',lcItem,SUBSTR(lcItem,1,LEN(lcMjrMsk))))+;
              '|'+IIF(lcTranType='M','cutting ticket',IIF(lcTranType='I','purchase order','order')))
  =gfModalGen('TRM38031B00000','ALERT',lcMjrHdr+':'+ALLTRIM(SUBSTR(lcItem,1,LEN(lcMjrMsk)))+;
              '|'+IIF(lcTranType='M','cutting ticket',IIF(lcTranType='I','purchase order','order')))
  RETURN(.F.)
ENDIF
*E038150,1 AMH [End]

lnAlias = SELECT()
DECLARE laMfgRFld[7,2],laItemSeg[1],laReq[8]
STORE '' TO lcContCode,lcContName,lcOperSeq,llInHouse,llMfgOpr,lnLeadTime,lcMfgGlAcnt
laMfgRFld[1,1] = 'CCONTCODE'
laMfgRFld[1,2] = 'lcContCode'
laMfgRFld[2,1] = 'CCONTNAME'
laMfgRFld[2,2] = 'lcContName'
laMfgRFld[3,1] = 'COPERSEQ'
laMfgRFld[3,2] = 'lcOperSeq'
laMfgRFld[4,1] = 'LINHOUSE'
laMfgRFld[4,2] = 'llInHouse'
laMfgRFld[5,1] = 'LMFGOPR'
laMfgRFld[5,2] = 'llMfgOpr'
laMfgRFld[6,1] = 'LEADTIME'
laMfgRFld[6,2] = 'lnLeadTime'
laMfgRFld[7,1] = 'GLACCOUNT'
laMfgRFld[7,2] = 'lcMfgGlAcnt'

*E038150,1 AMH Consider the inventory type [Start]
*=gfItemMask(@laItemSeg)
=gfItemMask(@laItemSeg,"",lcInvType)
*E038150,1 AMH [End]

*B608542,1 WAM 08/14/2008 Get material cost based on latest exchange rates
llMulCurr = gfGetMemVar('llMulCurr')
*B608542,1 WAM 08/14/2008 (End)

FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lcClrMsk = laItemSeg[lnCount,3]
    lnClrPos = laItemSeg[lnCount,4]
  ENDIF
  IF laItemSeg[lnCount,1]='S'
    lnSizePos = laItemSeg[lnCount,4]
  ENDIF
ENDFOR
lnLastSeq = 0
IF !EMPTY(lcLastOpr)
  =gfRltFld(lcLastOpr,@laMfgRFld,'MFGCODE')
  lnLastSeq = VAL(LEFT(lcOperSeq,2))
ENDIF

*N119813,1 AMH Add variables for cost elements # 6,7 [Start]
*STORE 0 TO lnEst1,lnEst2,lnEst3,lnEst4,lnEst5,lnFEst1,lnFEst2,lnFEst3,lnFEst4,lnFEst5
STORE 0 TO lnEst1,lnEst2,lnEst3,lnEst4,lnEst5,lnEst6,lnEst7,lnFEst1,lnFEst2,lnFEst3,lnFEst4,lnFEst5,lnFEst6,lnFEst7
*N119813,1 AMH [End]

lcItemFile = IIF(lcTranType='T','Fabric','Style')
IF !USED('Style')
  =gfOpenFile(oAriaApplication.DataDir+'STYLE',oAriaApplication.DataDir+'STYLE','SH')
ENDIF
IF !USED('Scale')
  =gfOpenFile(oAriaApplication.DataDir+'Scale',oAriaApplication.DataDir+'Scale','SH')
ENDIF

*E038150,1 AMH Remove the lcColor parameter [Start]
*llDyelot   = SEEK(lcItem+ALLTRIM(lcColor),lcItemFile) .AND. &lcItemFile..cDye_Flg='Y'
*lcScale    = IIF(lcTranType='T','',Style.Scale)
llDyelot   = SEEK(lcItem,lcItemFile) .AND. EVALUATE(lcItemFile+'.cDye_Flg')='Y'
lcScale    = EVALUATE(lcItemFile+'.Scale')
PRIVATE lnSclCnt
lnSclCnt = IIF(SEEK('S'+lcScale,'SCALE'),SCALE.Cnt,8)
*E038150,1 AMH [End]

llContinue = .T. 

*N119813,1 AMH Don't need to open these files [Start]
*=gfOpenFile(oAriaApplication.DataDir+'Ctktbom',oAriaApplication.DataDir+'Ctktbom','SH')
*=gfOpenFile(oAriaApplication.DataDir+'BomLine',oAriaApplication.DataDir+'BomLine','SH')
*=gfOpenFile(oAriaApplication.DataDir+'MFGOPRHD',oAriaApplication.DataDir+'MFGOPRHD','SH')
*N119813,1 AMH [End]

SET ORDER TO TAG Ctktbom  IN (lcTktSheet)
SET ORDER TO TAG BomLine  IN (lcDetFile)
SET ORDER TO TAG MFGOPRHD IN (lcOprHdr)

lcTmpBomSh = gfTempName()

*N119813,1 AMH Use the cursor name of the BOM file [Start]
*SELECT BOM
SELECT (lcTmpSheet)
*N119813,1 AMH [End]

=AFIELDS(laFileStru)
CREATE TABLE (oAriaApplication.WorkDir+lcTmpBomSh) FROM ARRAY laFileStru

*E038150,1 AMH Remove IClr from lcTmpBomSh [Start]
*INDEX ON typ+item+iclr+citmmajor+citmmask TAG (lcTmpBomSh)
INDEX ON typ+cInvTypC+item+cInvType+citmmajor+citmmask TAG (lcTmpBomSh)
*E038150,1 AMH [End]
*B608180,1 TMI [Start] reverse the order so the operations are met first
*B608223,1 TMI [Start] change order asscending type when called from lfvGenerate
IF llSheetGen
  *B608223,1 TMI [End  ] 
SET ORDER TO TAG &lcTmpBomSh DESCENDING
LOCATE
  *B608223,1 TMI [Start] 
ENDIF
*B608223,1 TMI [End  ]   
*B608180,1 TMI [End  ] 

*! B609246,1 MMT 05/13/2010 Fix problem of Wrong unit Qty in Ctktbom file when PO edited[Start]
PRIVATE lcCostElem
lcCostElem = gfTempName()
CREATE CURSOR (lcCostElem) (cTrType C(1), CBOMTYP C(1), Item C(19), MFGCode C(6), cDyelot C(10), cSizes C(8))
*B609356,1 SMA 07/27/2010 remove of clause to prevent empty *.cdx files from creation.....[BEGIN]
*INDEX ON cTrType+CBOMTYP +Item+MFGCode+cDyelot TAG (lcCostElem) OF (lcCostElem)
INDEX ON cTrType+CBOMTYP +Item+MFGCode+cDyelot TAG (lcCostElem)
*B609356,1 SMA 07/27/2010 remove of clause to prevent empty *.cdx files from creation.....[END]
*! B609246,1 MMT 05/13/2010 Fix problem of Wrong unit Qty in Ctktbom file when PO edited[End]

SELECT (lcTmpSheet)

*E038150,1 AMH Consider the inventory type [Start]
*=SEEK(PADR(IIF(lcTranType='T',lcItem,SUBSTR(lcItem,1,LEN(lcMjrMsk))),19))
*SCAN REST WHILE cItmMajor = PADR(IIF(lcTranType='T',lcItem,SUBSTR(lcItem,1,LEN(lcMjrMsk))),19)
=SEEK(lcInvType+PADR(SUBSTR(lcItem,1,LEN(lcMjrMsk)),19))
SCAN REST WHILE cInvType+cItmMajor = lcInvType+PADR(SUBSTR(lcItem,1,LEN(lcMjrMsk)),19)
*E038150,1 AMH [End]

  SCATTER MEMVAR MEMO
  IF cCatGTyp='S' .AND. M_USEEXSSC .AND. !EMPTY(MSZCROSREF) .AND. ;
     SUBSTR(ALLTRIM(Item),lnSizePos) = STRTRAN(SUBSTR(lcItmMsk,lnSizePos),'X','*')
    FOR lnCount = 1 TO MEMLINES(MSZCROSREF)
      lcLine = MLINE(MSZCROSREF,lnCount)
      IF SUBSTR(lcLine,1,3) <> lcScale
        LOOP
      ENDIF
      m.Item = PADR(SUBSTR(m.Item,1,lnSizePos-1)+SUBSTR(lcLine,AT('~',lcLine)+1,3),19)
      
      *E038150,1 AMH Remove IClr from lcTmpSheet [Start]
      *IF !SEEK(m.typ+m.item+m.iclr+m.citmmajor+m.citmmask,lcTmpBomSh)
      IF !SEEK(m.typ+m.cInvTypC+m.item+m.cInvType+m.citmmajor+m.citmmask,lcTmpBomSh)
      *E038150,1 AMH [End]
      
        INSERT INTO (lcTmpBomSh) FROM MEMVAR
      ENDIF
    ENDFOR
  ELSE
    INSERT INTO (lcTmpBomSh) FROM MEMVAR
  ENDIF
ENDSCAN

IF USED('STYLE')
  SELECT STYLE
  lcStyFIlt = SET('FILTER')
  SET FILTER TO
ENDIF

lcGrade = ''

*E038150,1 AMH Consider the inventory type [Start]
*IF lcTranType <> 'T'
*  =SEEK(lcItem,'Style')
*  lcGrade = Style.cStyGrade
*ELSE
*  =SEEK(lcItem+lcColor,'Fabric')
*  lcGrade = Fabric.cFabGrade
*ENDIF
=SEEK(lcItem,lcItemFile)
lcGrade = EVALUATE(lcItemFile+'.cStyGrade')
*E038150,1 AMH [End]

*N119813,1 AMH Consider case of Inter-Location PO [Start]
*IF lcTranType = 'I' OR lcTranType = 'D'
IF lcTranType $ 'IDN'
*N119813,1 AMH [End]

  =lfUpdPOBom() 
ENDIF
llMsgDispd = .F.

SELECT (lcTmpBomSh)
SCAN
  IF EMPTY(cCatGTyp) .OR. EMPTY(Typ)
    IF !llMsgDispd
      *--One or more cost items does not have a proper cost tyoe.
      *--Message : 38171
      =gfModalgen("INM38171B00000","ALERT",cItmMask)
      llMsgDispd = .T.
    ENDIF
    LOOP
  ENDIF
  *N000587,1 WAM 12/01/2007 Get foreign and equivalent cost from PO cost sheet lines
  IF TYP = '8' OR cCatGTyp = 'N'
    LOOP
  ENDIF
  *N000587,1 WAM 12/01/2007 (End)
  
  *E038150,1 AMH Remove lcColor parameter [Start]
  *IF !LIKE(STRTRAN(IIF(lcTranType='T',LEFT(cItmMask,6),cItmMask),'*','?'),IIF(lcTranType='T',lcColor,lcItem)) .OR. ;
     (!EMPTY(MSIZES) .AND. ATCLINE(lcScale+'~',MSIZES)=0) .OR. ;
     (!EMPTY(MSZCROSREF) .AND. ATCLINE(lcScale+',',MSZCROSREF)=0)     
  IF !LIKE(STRTRAN(cItmMask,'*','?'),lcItem) .OR. (!EMPTY(MSIZES) .AND. ATCLINE(lcScale+'~',MSIZES)=0) .OR. ;
     (!EMPTY(MSZCROSREF) .AND. ATCLINE(lcScale+',',MSZCROSREF)=0)
  *E038150,1 AMH [End]
  
    LOOP
  ENDIF
  lcItemType = Typ
  IF EMPTY(MSIZES)
    lnTranQty = laQty[9]
    
    *E038150,1 AMH Get the correct sizes [Start]
    *lcSizes   = '1,2,3,4,5,6,7,8'
    lcSizes = ''
    LOCAL lnI,lcI
    FOR lnI = 1 TO lnSclCnt
      lcI = STR(lnI,1)
      lcSizes = lcSizes + IIF(lnI = 1,'',',') + lcI
    ENDFOR
    *E038150,1 AMH [End]
    
  ELSE
    lcSizes = SUBSTR(MLINE(MSIZES,ATCLINE(lcScale+'~',MSIZES)),5)
    lnTranQty = 0
    FOR lnCount = 1 TO 8
      lnTranQty = lnTranQty + IIF(STR(lnCount,1) $ lcSizes,laQty[lnCount],0)
    ENDFOR
  ENDIF
  STORE '' TO lcCstClr,lcCstItm,lcCmSizes
  lcCstItmDye = SPACE(10)
  DO CASE
    CASE cCatGTyp='S'
      lcCmSizes='12345678'
      FOR lnCount = 1 TO 8
        laReq[lnCount]=laQty[lnCount]
      ENDFOR
      lcCstItm=''
      FOR lnCount = 1 TO LEN(ITEM)
        IF SUBSTR(ITEM,lnCount,1)='*'
          lcCstItm = lcCstItm + SUBSTR(lcItem,lnCount,1)
        ELSE
          lcCstItm = lcCstItm + SUBSTR(ITEM,lnCount,1)
        ENDIF
      ENDFOR
      IF !SEEK(lcCstItm,'Style')
        LOOP
      ENDIF
      lcCsItmSc = Style.Scale
      =SEEK(lcItem,'Style')
      IF !EMPTY(MSZCROSREF)
        lcCmSizes = ''
        STORE 0 TO laReq
        FOR lnCount = 1 TO MEMLINES(MSZCROSREF)
          lcLine = MLINE(MSZCROSREF,lnCount)
          IF SUBSTR(lcLine,1,3) = lcScale .AND. SUBSTR(lcLine,7,3)=lcCsItmSc
            laReq[VAL(SUBSTR(lcLine,11,1))] = laReq[VAL(SUBSTR(lcLine,11,1))]+;
            laQty[VAL(SUBSTR(lcLine,5,1))]
            lcCmSizes = lcCmSizes + SUBSTR(lcLine,11,1)
          ENDIF
        ENDFOR
      ENDIF
      IF M_WAREHOUSE='Y' .AND. !EMPTY(lcStyWare) .AND. ;
         !SEEK(lcCstItm+lcStyWare+SPACE(10),'STYDYE')
        *E300725,1 Message : 38029
        *E300725,1 Style xxxxx is not assigned to warehouse xxxx
        *E300725,1 Button : 38001
        *E300725,1 Add Cancel
        
        *E300935,4 adjust condition to update without message if llBackOnly is .T.
        *E300935,4 IF gfModalGen('QRM38029B38001','ALERT','Style: '+ALLTRIM(lcCstItm)+'|'+lcStyWare) = 1
        IF llBackOnly OR gfModalGen('QRM38029B38001','ALERT','Style: '+ALLTRIM(lcCstItm)+'|'+lcStyWare) = 1
          DO gpAdStyWar WITH lcCstItm,SPACE(10),lcStyWare
        ENDIF   
      
      ENDIF
      IF !EMPTY(lcStyWare) AND M_DYELOT='Y' .AND. SEEK(lcCstItm,'Style') .AND. Style.cDye_Flg='Y'

        SELECT (lcDetFile)
        
        *E038150,1 AMH Remove IClr from lcDetFile [Start]
        *=SEEK(lcTranLett+'1'+lcTicketNo+STR(lnLineNo,6)+&lcTmpBomSh..Typ+PADR(lcItem,19)+;
        PADR(lcColor,6)+PADR(lcCstItm,19)+PADR(lcCstClr,6)+&lcTmpBomSh..MfgCode)
        *LOCATE REST WHILE ;
        cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode=;
        lcTranLett+'1'+lcTicketNo+STR(lnLineNo,6)+&lcTmpBomSh..Typ+PADR(lcItem,19)+;
        PADR(lcColor,6)+PADR(lcCstItm,19)+PADR(lcCstClr,6)+&lcTmpBomSh..MfgCode ;
        FOR cSizes = STRTRAN(lcSizes,',','')
        =SEEK(lcTranLett+'1'+lcTicketNo+STR(lnLineNo,6)+EVALUATE(lcTmpBomSh+'.Typ')+lcInvType+PADR(lcItem,19)+;
              EVALUATE(lcTmpBomSh+'.cInvTypC')+PADR(lcCstItm,19)+EVALUATE(lcTmpBomSh+'.MfgCode'))
        LOCATE REST WHILE ;
        CIMTYP+CTYPE+CTKTNO+STR(LINENO,6)+CBOMTYP+CINVTYPE+STYLE+CINVTYPC+ITEM+MFGCODE=;
        lcTranLett+'1'+lcTicketNo+STR(lnLineNo,6)+EVALUATE(lcTmpBomSh+'.Typ')+lcInvType+PADR(lcItem,19)+;
        EVALUATE(lcTmpBomSh+'.cInvTypC')+PADR(lcCstItm,19)+EVALUATE(lcTmpBomSh+'.MfgCode') ;
        FOR cSizes = STRTRAN(lcSizes,',','')
        *E038150,1 AMH [End]

        lcCstItmDye = IIF(FOUND(),&lcDetFile..Dyelot,lcDyelot)
        
        IF SEEK(lcCstItm+lcStyWare+SPACE(10),'STYDYE') AND ;
          (EMPTY(lcCstItmDye) OR !SEEK(lcCstItm+lcStyWare+lcCstItmDye,'StyDye'))
          *E300935,4 Adjust calling with background parameter (llBackOnly)
          *E300935,4 =gfSelDyelot(lcTranType,'S',lcItem,lcColor,lcStyWare,@lcCstItmDye,lcCstItm)
          
          *N119813,1 AMH Consider case of configuration [Start]
          *=gfSelDyelot(lcTranType,'S',lcItem,lcColor,lcStyWare,@lcCstItmDye,lcCstItm,'','',llBackOnly)
          
          *E038150,1 AMH Consider the inventory type [Start]
          *=gfSelDyelot(lcTranType,'S',lcItem,lcInvType,lcStyWare,@lcCstItmDye,lcCstItm,'','',llBackOnly,llStyConfg)
          =gfSelDyelot(lcTranType,'S',lcItem,lcInvType,lcStyWare,@lcCstItmDye,lcCstItm,'0001','',llBackOnly,llStyConfg)
          *E038150,1 AMH [End]
          
          *N119813,1 AMH [End]
          
        ENDIF
      ENDIF

    CASE INLIST(cCatGTyp,'F','T')
      
      *E038150,1 AMH Remove IClr from lcTmpBomSh [Start]
      *lcCstItm = SUBSTR(Item,1,7)
      *IF IClr = '******'
      *  lcCstClr = IIF(lcTranType='T',lcColor,SUBSTR(lcItem,lnClrPos,LEN(lcClrMsk)))
      *ELSE
      *  lcCstClr = IClr
      *ENDIF
      *IF (cCatGTyp='F' OR Trim_Invt) AND !SEEK(lcCstItm+lcCstClr,'FABRIC')
      *  LOOP
      *ENDIF
      *B607915,1 MMT 12/28/2006 bug of error in Po screen while save (Commented out)
*!*	      LOCAL lnItmClrPos,lnItmClrLen,lcItemWidth
*!*	      lcItemWidth = ''
*!*	      =gfItemMask(@laItemSeg,"",cInvTypC)
*!*	      FOR lnCount = 1 TO ALEN(laItemSeg,1)
*!*	        IF laItemSeg[lnCount,1]='C'
*!*	          lnItmClrLen = LEN(laItemSeg[lnCount,3])
*!*	          lnItmClrPos = laItemSeg[lnCount,4]
*!*	        ENDIF
*!*	      ENDFOR
*!*	      lcCmSizes='12345678'
*!*	      FOR lnCount = 1 TO 8
*!*	        laReq[lnCount]=laQty[lnCount]
*!*	      ENDFOR
*!*	      IF SUBSTR(item,lnItmClrPos,lnItmClrLen) = '*'
*!*	        lcCstItm = STUFF(item,lnItmClrPos,lnItmClrLen,SUBSTR(lcItem,lnClrPos,LEN(lcClrMsk)))
*!*	      ELSE
*!*	        lcCstItm = item
*!*	      ENDIF
      *B607915,1 MMT 12/28/2006 bug of error in Po screen while save (Commented out) End

      IF cCatGTyp='F' OR Trim_Invt
      
        *B607915,1 MMT 12/28/2006 bug of error in Po screen while save 
        LOCAL lnItmClrPos,lnItmClrLen,lcItemWidth
        lcItemWidth = ''
        =gfItemMask(@laItemSeg,"",cInvTypC)
        FOR lnCount = 1 TO ALEN(laItemSeg,1)
          IF laItemSeg[lnCount,1]='C'
            lnItmClrLen = LEN(laItemSeg[lnCount,3])
            lnItmClrPos = laItemSeg[lnCount,4]
          ENDIF
        ENDFOR
        lcCmSizes='12345678'
        FOR lnCount = 1 TO 8
          laReq[lnCount]=laQty[lnCount]
        ENDFOR
        *B609533,1 SAB 02/22/2011 erro prevents adding material to PO cost sheet when  color starts with * [Start]
        *IF SUBSTR(item,lnItmClrPos,lnItmClrLen) = '*'
        IF SUBSTR(item,lnItmClrPos,lnItmClrlen) = REPLICATE('*',lnItmClrLen)
        *B609533,1 SAB 02/22/2011 erro prevents adding material to PO cost sheet when  color starts with * [End]
          lcCstItm = STUFF(item,lnItmClrPos,lnItmClrLen,SUBSTR(lcItem,lnClrPos,LEN(lcClrMsk)))
        ELSE
          lcCstItm = item
        ENDIF
        *B607915,1 MMT 12/28/2006 bug of error in Po screen while save  (End)
        
        LOCAL lnCurAlias
        lnCurAlias = SELECT(0)
        IF gfOpnSqlFl('item',"TMPFABRIC","cinvtype = '"+cInvTypC+"' and style = '"+lcCstItm+"'","","style")
          SELECT TMPFABRIC
          LOCATE
          IF EOF()
            USE IN TMPFABRIC
            SELECT (lnCurAlias)
            LOOP
          ENDIF
          lcCsItmSc   = Scale
          lcItemWidth = CITEMFLD1
          USE IN TMPFABRIC
          SELECT (lnCurAlias)
          IF !EMPTY(MSZCROSREF)
            lcCmSizes = ''
            STORE 0 TO laReq
            FOR lnCount = 1 TO MEMLINES(MSZCROSREF)
              lcLine = MLINE(MSZCROSREF,lnCount)
              IF SUBSTR(lcLine,1,3) = lcScale .AND. SUBSTR(lcLine,7,3)=lcCsItmSc
                laReq[VAL(SUBSTR(lcLine,11,1))] = laReq[VAL(SUBSTR(lcLine,11,1))]+;
                laQty[VAL(SUBSTR(lcLine,5,1))]
                lcCmSizes = lcCmSizes + SUBSTR(lcLine,11,1)
              ENDIF
            ENDFOR
          ENDIF
        ELSE
          LOOP
        ENDIF
      ENDIF
      *IF (cCatGTyp='F' .OR. Trim_Invt) .AND. M_WAREHOUSE='Y' .AND. ;
         !EMPTY(lcMatWare) .AND. ;
         !SEEK(lcCstItm+lcCstClr+lcMatWare+SPACE(10),'FABDYE')
      *  *E300725,1 Message : 38029
      *  *E300725,1 Item/Color xxxxx/xxxx is not assigned to warehouse xxxx
      *  *E300725,1 Button : 38001
      *  *E300725,1 Add Cancel
      *  
      *  *E300935,4 adjust condition to update without message if llBackOnly is .T.
      *  *E300935,4 IF gfModalGen('QRM38029B38001','ALERT','Item/Color: '+ALLTRIM(lcCstItm)+'/'+ALLTRIM(lcCstClr)+'|'+lcMatWare) = 1
      *  IF llBackOnly OR gfModalGen('QRM38029B38001','ALERT','Item/Color: '+ALLTRIM(lcCstItm)+'/'+ALLTRIM(lcCstClr)+'|'+lcMatWare) = 1
      *    DO gpAdFabWar WITH lcCstItm,lcCstClr,SPACE(10),lcMatWare
      *  ENDIF   
      *ENDIF
      IF (cCatGTyp='F' .OR. Trim_Invt) .AND. M_WAREHOUSE='Y' .AND. !EMPTY(lcMatWare)
        LOCAL lnCurAlias
        lnCurAlias = SELECT(0)
        IF gfOpnSqlFl('itemloc',"TMPFABDYE","cinvtype = '"+cInvTypC+"' and style = '"+lcCstItm+;
                                            "' and cwarecode = '"+lcMatWare+"' and dyelot = '"+SPACE(10)+"'","","stydye")
          SELECT TMPFABDYE
          LOCATE
          IF EOF()
            SELECT (lnCurAlias)
            *E300725,1 Message : 38029
            *E300725,1 Item/Color xxxxx/xxxx is not assigned to warehouse xxxx
            *E300725,1 Button : 38001
            *E300725,1 Add Cancel
            
            *E300935,4 adjust condition to update without message if llBackOnly is .T.
            *E300935,4 IF gfModalGen('QRM38029B38001','ALERT','Item/Color: '+ALLTRIM(lcCstItm)+'/'+ALLTRIM(lcCstClr)+'|'+lcMatWare) = 1
            IF llBackOnly OR gfModalGen('QRM38029B38001','ALERT','Item: '+ALLTRIM(lcCstItm)+'|'+lcMatWare) = 1
              DO gfAdItemWar WITH cInvTypC, lcCstItm, SPACE(10), lcMatWare
            ENDIF
          ENDIF
          USE IN TMPFABDYE
          SELECT (lnCurAlias)
        ELSE
          *E300725,1 Message : 38029
          *E300725,1 Item/Color xxxxx/xxxx is not assigned to warehouse xxxx
          *E300725,1 Button : 38001
          *E300725,1 Add Cancel
          
          *E300935,4 adjust condition to update without message if llBackOnly is .T.
          *E300935,4 IF gfModalGen('QRM38029B38001','ALERT','Item/Color: '+ALLTRIM(lcCstItm)+'/'+ALLTRIM(lcCstClr)+'|'+lcMatWare) = 1
          IF llBackOnly OR gfModalGen('QRM38029B38001','ALERT','Item: '+ALLTRIM(lcCstItm)+'|'+lcMatWare) = 1
            DO gfAdItemWar WITH cInvTypC, lcCstItm, SPACE(10), lcMatWare
          ENDIF
        ENDIF
      ENDIF
      *IF !EMPTY(lcMatWare) AND (cCatGTyp='F' OR Trim_Invt) AND M_MATDYE='Y' AND ;
      *   SEEK(lcCstItm+lcCstClr,'Fabric') .AND. Fabric.cDye_Flg='Y' 
      *  
      *  SELECT (lcDetFile)
      *  
      *  *N119813,1 AMH Consider case of style use configuration [Start]
      *  IF !llStyconfg
      *  *N119813,1 AMH [End]
      *  
      *  =SEEK(lcTranLett +'1'+lcTicketNo+STR(lnLineNo,6)+&lcTmpBomSh..Typ+PADR(lcItem,19)+;
      *  PADR(lcColor,6)+PADR(lcCstItm,19)+PADR(lcCstClr,6)+&lcTmpBomSh..MfgCode)
      *  LOCATE REST WHILE ;
      *  cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode=;
      *  lcTranLett +'1'+lcTicketNo+STR(lnLineNo,6)+&lcTmpBomSh..Typ+PADR(lcItem,19)+;
      *  PADR(lcColor,6)+PADR(lcCstItm,19)+PADR(lcCstClr,6)+&lcTmpBomSh..MfgCode ;
      *  FOR cSizes = STRTRAN(lcSizes,',','')
      *
      *  lcCstItmDye = IIF(FOUND(),&lcDetFile..Dyelot,lcDyelot)
      *
      *  *N119813,1 AMH Consider case of style use configuration [Start]
      *  ENDIF
      *  *N119813,1 AMH [End]
      *
      *  IF SEEK(lcCstItm+lcCstClr+lcMatWare+SPACE(10),'FABDYE') AND ;
      *    (EMPTY(lcCstItmDye) OR !SEEK(lcCstItm+lcCstClr+lcMatWare+lcCstItmDye,'FabDye'))
      *   
      *    *E300935,4 Adjust calling with background parameter (llBackOnly)
      *    *E300935,4 =gfSelDyelot(lcTranType,'F',lcItem,lcColor,lcMatWare,@lcCstItmDye,lcCstItm,lcCstClr,lcTmpName)
      *    =gfSelDyelot(lcTranType,'F',lcItem,lcColor,lcMatWare,@lcCstItmDye,lcCstItm,lcCstClr,lcTmpName,llBackOnly)
      *  ENDIF
      *ENDIF
      IF !EMPTY(lcMatWare) AND (cCatGTyp='F' OR Trim_Invt) AND M_MATDYE='Y'
        LOCAL lnCurAlias
        lnCurAlias = SELECT(0)
        IF gfOpnSqlFl('item',"TMPFABRIC","cinvtype = '"+cInvTypC+"' and style = '"+lcCstItm+"'","","style")
          SELECT TMPFABRIC
          LOCATE
          IF !EOF() AND cDye_Flg = 'Y'
            SELECT (lcDetFile)
            IF !llStyconfg
              =SEEK(lcTranLett +'1'+lcTicketNo+STR(lnLineNo,6)+EVALUATE(lcTmpBomSh+'.Typ')+lcInvType+PADR(lcItem,19)+;
              EVALUATE(lcTmpBomSh+'.cInvTypC')+PADR(lcCstItm,19)+EVALUATE(lcTmpBomSh+'.MfgCode'))
              LOCATE REST WHILE cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+cInvType+style+cInvTypC+item+mfgcode=;
                                lcTranLett +'1'+lcTicketNo+STR(lnLineNo,6)+EVALUATE(lcTmpBomSh+'.Typ')+lcInvType+;
                                PADR(lcItem,19)+EVALUATE(lcTmpBomSh+'.cInvTypC')+PADR(lcCstItm,19)+;
                                EVALUATE(lcTmpBomSh+'.MfgCode') FOR cSizes = STRTRAN(lcSizes,',','')
              lcCstItmDye = IIF(FOUND(),Dyelot,lcDyelot)
            ENDIF
            IF gfOpnSqlFl('itemloc',"TMPFABDYE","cinvtype = '"+cInvTypC+"' and style = '"+lcCstItm+;
                                                "' and cwarecode = '"+lcMatWare+"'","","stydye")
              SELECT TMPFABDYE
              LOCATE FOR Dyelot = SPACE(10)
              IF FOUND()
                IF EMPTY(lcCstItmDye)
                  =gfSelDyelot(lcTranType,'F',lcItem,lcInvType,lcMatWare,@lcCstItmDye,lcCstItm,EVALUATE(lcTmpBomSh+'.cInvTypC'),lcTmpName,llBackOnly)
                ELSE
                  LOCATE FOR Dyelot = lcCstItmDye
                  IF !FOUND()
                    =gfSelDyelot(lcTranType,'F',lcItem,lcInvType,lcMatWare,@lcCstItmDye,lcCstItm,EVALUATE(lcTmpBomSh+'.cInvTypC'),lcTmpName,llBackOnly)
                  ENDIF
                ENDIF
              ENDIF
              USE IN TMPFABDYE
            ENDIF
          ENDIF
          USE IN TMPFABRIC
          SELECT (lnCurAlias)
        ENDIF
      ENDIF
      *E038150,1 AMH [End]
      
    CASE cCatGTyp='M'
      =gfRltFld(&lcTmpBomSh..MfgCode,@laMfgRFld,'MFGCODE')
      *C200080,1 AMM Adjust to fit the new type 'D' for dye order
      *IF llMfgOpr .AND. !SEEK(lcTranType+lcTicketNo+&lcTmpBomSh..MfgCode,lcOprHdr)
      IF llMfgOpr .AND. !SEEK(lcTranLett+lcTicketNo+&lcTmpBomSh..MfgCode,lcOprHdr)
      *C200080,1 AMM End
        lcOperSeq = LEFT(lcOperSeq,2)
        IF VAL(lcOperSeq) > lnLastSeq
          lnLastSeq = VAL(lcOperSeq)
          lcLastOpr = &lcTmpBomSh..MfgCode
        ENDIF

        INSERT INTO (lcOprHdr) (cIMTYp,cTktNo,cOprCode,cOperSeq,cContCode,cContName,lInHouse,nNxtLotNo);
        VALUES (lcTranLett,lcTicketNo,&lcTmpBomSh..MfgCode,lcOperSeq,lcContCode,lcContName,llInHouse,1)

      ENDIF
      IF lcTranType = 'I'
        lcExSign = gfGetExSin(@lcUntSin,lcDutyCur)
      ENDIF  
    CASE cCatGTyp='P'
      lcExSign = gfGetExSin(@lcUntSin,lcPriceCur)
    CASE !INLIST(cCatGTyp,'S','F','T') AND lcTranType = 'I'
      lcExSign = gfGetExSin(@lcUntSin,lcDutyCur)
  ENDCASE
  SELECT (lcTktSheet)
  
  *E038150,1 AMH Remove IClr from lcTktSheet and lcDetFile [Start]
  *IF !SEEK(lcTranLett+lcTicketNo+&lcTmpBomSh..Typ+PADR(lcCstItm,19)+;
     PADR(lcCstClr,6)+&lcTmpBomSh..MfgCode+lcCstItmDye)
  IF !SEEK(lcTranLett+lcTicketNo+EVALUATE(lcTmpBomSh+'.Typ')+EVALUATE(lcTmpBomSh+'.cInvTypC')+PADR(lcCstItm,19)+;
           EVALUATE(lcTmpBomSh+'.MfgCode')+lcCstItmDye)
    APPEND BLANK
    *REPLACE CutTkt    WITH lcTicketNo    ,;
            cIMTyp    WITH lcTranLett    ,;
            cCatGTyp  WITH &lcTmpBomSh..cCatGTyp  ,;
            TRIM_INVT WITH &lcTmpBomSh..TRIM_INVT ,;
            cWareCode WITH IIF(cCatGTyp='S',lcStyWare,IIF(cCatGTyp='F' OR ;
                           (cCatGTyp='T' AND TRIM_INVT),lcMatWare,'')) ,;
            Link_Code WITH lcLinkCode            ,;
            cOprCode  WITH &lcTmpBomSh..cOprCode ,;
            TYP       WITH &lcTmpBomSh..Typ      ,;
            ITEM      WITH lcCstItm              ,;
            ICLR      WITH lcCstClr              ,;
            MfgCode   WITH &lcTmpBomSh..MfgCode  ,;
            cOprCode  WITH &lcTmpBomSh..cOprCode ,;
            DESC      WITH &lcTmpBomSh..Desc     ,;
            Dyelot    WITH lcCstItmDye           ,;
            UOM       WITH &lcTmpBomSh..UOM      ,;
            DATE      WITH oAriaApplication.SystemDate  ,;
            WIDTH     WITH IIF(cCatGTyp='F' OR (cCatGTyp='T' AND TRIM_INVT),FABRIC.WIDTH,'') 

    *E038220,1 WSH Change UOM-Code to Relation Code [Start]
    *REPLACE CutTkt    WITH lcTicketNo    ,;
            cIMTyp    WITH lcTranLett    ,;
            cCatGTyp  WITH &lcTmpBomSh..cCatGTyp  ,;
            TRIM_INVT WITH &lcTmpBomSh..TRIM_INVT ,;
            cWareCode WITH IIF(cCatGTyp='S',lcStyWare,IIF(cCatGTyp='F' OR ;
                           (cCatGTyp='T' AND TRIM_INVT),lcMatWare,'')) ,;
            Link_Code WITH lcLinkCode            ,;
            cOprCode  WITH &lcTmpBomSh..cOprCode ,;
            TYP       WITH &lcTmpBomSh..Typ      ,;
            cInvType  WITH EVALUATE(lcTmpBomSh+'.cInvTypC'),;
            ITEM      WITH lcCstItm              ,;
            MfgCode   WITH &lcTmpBomSh..MfgCode  ,;
            cOprCode  WITH &lcTmpBomSh..cOprCode ,;
            DESC      WITH &lcTmpBomSh..Desc     ,;
            Dyelot    WITH lcCstItmDye           ,;
            UOM       WITH &lcTmpBomSh..UOM      ,;
            DATE      WITH oAriaApplication.SystemDate  ,;
            WIDTH     WITH IIF(cCatGTyp='F' OR (cCatGTyp='T' AND TRIM_INVT),FABRIC.CITEMFLD1,'') 
    REPLACE CutTkt    WITH lcTicketNo    ,;
            cIMTyp    WITH lcTranLett    ,;
            cCatGTyp  WITH &lcTmpBomSh..cCatGTyp  ,;
            TRIM_INVT WITH &lcTmpBomSh..TRIM_INVT ,;
            cWareCode WITH IIF(cCatGTyp='S',lcStyWare,IIF(cCatGTyp='F' OR ;
                           (cCatGTyp='T' AND TRIM_INVT),lcMatWare,'')) ,;
            Link_Code WITH lcLinkCode            ,;
            cOprCode  WITH &lcTmpBomSh..cOprCode ,;
            TYP       WITH &lcTmpBomSh..Typ      ,;
            cInvType  WITH EVALUATE(lcTmpBomSh+'.cInvTypC'),;
            ITEM      WITH lcCstItm              ,;
            MfgCode   WITH &lcTmpBomSh..MfgCode  ,;
            cOprCode  WITH &lcTmpBomSh..cOprCode ,;
            DESC      WITH &lcTmpBomSh..Desc     ,;
            Dyelot    WITH lcCstItmDye           ,;
            CUOMCODE  WITH &lcTmpBomSh..CUOMCODE ,;
            DATE      WITH oAriaApplication.SystemDate  ,;
            WIDTH     WITH IIF(cCatGTyp='F' OR (cCatGTyp='T' AND TRIM_INVT),lcItemWidth,'')
    *E038220,1 WSH [End]

    IF lcTranType = 'M'
      REPLACE CMARKER WITH &lcTmpBomSh..cMarker
    ENDIF
    *B608180,1 TMI [Start] if the first operation in the sequence is recorded then put it in the tktSheet temp file to be issued when the cost sheet is saved
  ELSE
    *B608223,1 TMI [Start] check if called from lfvGenerate
    IF llSheetGen
      *B608223,1 TMI [End  ] 
      LOCAL lcSvOrd
      lcSvOrd = ORDER(lcOprHdr)
      SET ORDER TO &lcOprHdr IN &lcOprHdr
      GO TOP IN (lcOprHdr)
      IF !EOF(lcOprHdr) .AND. !EMPTY(&lcTmpBomSh..cOprCode) .AND. &lcTmpBomSh..cOprCode == &lcOprHdr..cOprCode
        REPLACE cOprCode WITH &lcOprHdr..cOprCode
      ENDIF
      SET ORDER TO &lcSvOrd IN &lcOprHdr
      *B608223,1 TMI [Start] 
    ENDIF
    *B608223,1 TMI [End  ] 
    *B608180,1 TMI [End  ] 

  ENDIF

  SELECT (lcDetFile)
  *=SEEK(lcTranLett+'1'+lcTicketNo+STR(lnLineNo,6)+&lcTmpBomSh..Typ+PADR(lcItem,19)+;
        PADR(lcColor,6)+PADR(lcCstItm,19)+PADR(lcCstClr,6)+&lcTmpBomSh..MfgCode)
  *LOCATE REST WHILE ;
  cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode=;
  lcTranLett+'1'+lcTicketNo+STR(lnLineNo,6)+&lcTmpBomSh..Typ+PADR(lcItem,19)+;
  PADR(lcColor,6)+PADR(lcCstItm,19)+PADR(lcCstClr,6)+&lcTmpBomSh..MfgCode ;
  FOR cSizes = STRTRAN(lcSizes,',','')
  =SEEK(lcTranLett+'1'+lcTicketNo+STR(lnLineNo,6)+&lcTmpBomSh..Typ+lcInvType+PADR(lcItem,19)+;
        EVALUATE(lcTmpBomSh+'.cInvTypC')+PADR(lcCstItm,19)+&lcTmpBomSh..MfgCode)
  LOCATE REST WHILE ;
  cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+cInvType+style+cInvTypC+item+mfgcode=;
  lcTranLett+'1'+lcTicketNo+STR(lnLineNo,6)+&lcTmpBomSh..Typ+lcInvType+PADR(lcItem,19)+;
  EVALUATE(lcTmpBomSh+'.cInvTypC')+PADR(lcCstItm,19)+&lcTmpBomSh..MfgCode ;
  FOR cSizes = STRTRAN(lcSizes,',','')
  *E038150,1 AMH [Start]
  
  llFoundRec = FOUND()         
  SELECT (lcTktSheet)

*! B609246,1 MMT 05/13/2010 Fix problem of Wrong unit Qty in Ctktbom file when PO edited[Start]
*!*	  IF !llFoundRec
*!*	    REPLACE Pieces  WITH Pieces  + lnTranQty ,;
*!*	          Req_Qty WITH Req_Qty + lnTranQty * &lcTmpBomSh..nBomTotQty ,;
*!*	          UntQty  WITH IIF(Pieces<>0,Req_Qty/Pieces,0)
*!*	    lnUntQty = &lcTmpBomSh..nBomTotQty
*!*	  ELSE
*!*	    *B608376,1 WAM 12/09/2007 Fix calculation of CT cost sheet required quantity.
*!*	    *REPLACE Pieces  WITH Pieces  + lnTranQty ,;
*!*	          Req_Qty WITH Req_Qty + lnTranQty * &lcDetFile..UnitQty ,;
*!*	          UntQty  WITH IIF(Pieces<>0,Req_Qty/Pieces,0)
*!*	    *lnUntQty = &lcDetFile..UnitQty
*!*	    
*!*	    REPLACE Req_Qty WITH Req_Qty + lnTranQty * &lcTmpBomSh..nBomTotQty ,;
*!*	            UntQty  WITH IIF(Pieces<>0,Req_Qty/Pieces,0)
*!*	    lnUntQty = &lcTmpBomSh..nBomTotQty
*!*	    *B608376,1 WAM (End)
*!*	    
*!*	  ENDIF
  SELECT (lcCostElem)
  =SEEK('A'+&lcTmpBomSh..Typ+lcCstItm+&lcTmpBomSh..MfgCode+ lcCstItmDye)
  LOCATE REST WHILE cTrType+CBOMTYP +Item+MFGCode+cDyelot = 'A'+&lcTmpBomSh..Typ+lcCstItm+&lcTmpBomSh..MfgCode+ lcCstItmDye ;
                    FOR cSizes =STRTRAN(lcSizes,',','')
  IF !FOUND()
    REPLACE Pieces  WITH Pieces  + lnTranQty IN (lcTktSheet)
    INSERT INTO (lcCostElem) (cTrType,CBOMTYP ,Item, MFGCode,cDyelot,cSizes);
                          VALUES ('A',&lcTmpBomSh..Typ,lcCstItm,&lcTmpBomSh..MfgCode,lcCstItmDye,STRTRAN(lcSizes,',',''))
  ENDIF
  SELECT (lcTktSheet)
  REPLACE Req_Qty WITH Req_Qty + lnTranQty * &lcTmpBomSh..nBomTotQty ,;
          UntQty  WITH IIF(Pieces<>0,Req_Qty/Pieces,0)
  lnUntQty = &lcTmpBomSh..nBomTotQty
*! B609246,1 MMT 05/13/2010 Fix problem of Wrong unit Qty in Ctktbom file when PO edited[End]
  *--If tranQty was zero take it from pieces.
  *--to make sure that the cost was updated.
  *--Read lnTotCost and lnFTotCost depend on lnTTranQty insted of lnTranQty.
  lnTTranQty = lnTranQty
  IF lnTranQty = 0
    lnTTranQty = Pieces
  ENDIF 

  *B608223,1 MMT 08/16/2007 fix bug of wrong unit cost saved in Ctktbom[Start]
  IF lnTranQty = 0 AND llFoundRec
    lnTTranQty  = EVALUATE(lcDetFile+'.StyQty')
  ENDIF 
  *B608223,1 MMT 08/16/2007 fix bug of wrong unit cost saved in Ctktbom[End]

  DO CASE
    CASE cCatGTyp = 'P'
      *N000587,1 WAM 12/01/2007 Get foreign and equivalent cost from PO cost sheet lines
      *lnTotCost  = lnPrice * lnTTranQty &lcExSign lnPriceRate &lcUntSin lnPriceUnit
      *wael
      *lcExSign   = gfGetExSin(@lcUntSin, IIF(ISNULL(&lcTmpBomSh..cCurrCode) OR EMPTY(&lcTmpBomSh..cCurrCode),oAriaApplication.BaseCurrency,&lcTmpBomSh..cCurrCode))
      *lnExRate   = IIF(ISNULL(&lcTmpBomSh..nExRate) OR &lcTmpBomSh..nExRate=0,1,&lcTmpBomSh..nExRate)
      *lnCurrUnit = IIF(ISNULL(&lcTmpBomSh..nCUrrUnit) OR &lcTmpBomSh..nCUrrUnit=0,1,&lcTmpBomSh..nCurrUnit)

      lcExSign   = gfGetExSin(@lcUntSin, lcPriceCur)
      lnExRate   = lnPriceRate
      lnCurrUnit = lnPriceUnit
      *wael

      lnTotCost  = lnPrice * lnTTranQty &lcExSign lnExRate &lcUntSin lnCurrUnit
      *N000587,1 WAM 12/01/2007 (End)
      lnFTotCost = lnPrice * lnTTranQty 
    CASE &lcTmpBomSh..nPercent > 0
      lnTotCost  = lnPrice*lnTTranQty*(&lcTmpBomSh..nPercent/100) &lcExSign lnPriceRate &lcUntSin lnPriceUnit
      lnFTotCost = lnPrice*lnTTranQty*(&lcTmpBomSh..nPercent/100)
    CASE cCatGTyp = 'S'
      *! B609498,1 MMT 01/13/2010 Fix bug of wrong estimated cost for style comp. in PO Screen[Start]
      *Seek in Style file for the Comp. style record
      =SEEK(lcCstItm,'Style')
      *! B609498,1 MMT 01/13/2010 Fix bug of wrong estimated cost for style comp. in PO Screen[End]
      lnTotCost  = Style.TotCost * lnTTranQty * &lcTktSheet..UntQty        
      lnFTotCost = lnTotCost
    CASE cCatGTyp = 'F' OR (cCatGTyp='T' AND TRIM_INVT)
      
      *E038150,1 AMH Consider the inventory type [Start]
      *lnTotCost  = Fabric.CostBuy/Fabric.Conv * lnTTranQty * lnUntQty      
      LOCAL lnCurAlias
      lnCurAlias = SELECT(0)
      IF gfOpnSqlFl('item',"TMPFABRIC","cinvtype = '"+EVALUATE(lcTmpBomSh+'.cInvTypC')+"' and style = '"+lcCstItm+"'","","style")
        SELECT TMPFABRIC
        LOCATE
        IF !EOF()

          *E038220,1 WSH Get Conversion Factor from UOM SQL file. [Start]
          *lnTotCost  = TotCost * lnTTranQty * lnUntQty
          LOCAL lnConf
          lnConf    = 1
          =gfGetUOMData(EVALUATE(lcTmpBomSh + '.CUOMCODE'), '', '', @lnConf)

          *B608542,1 WAM 08/14/2008 Get material cost based on latest exchange rates
          *lnTotCost  = TotCost / lnConf * lnTTranQty * lnUntQty
          IF llMulCurr
            STORE 0 TO lnIECost1, lnIECost2, lnIECost3, lnIECost4, lnIECost5, lnIECost6, lnIECost7 
            STORE 1 TO lnCurrUnit, lnExRate
            lnExRate  = gfChkRate('lnCurrUnit',TMPFABRIC.cPriceCur,oAriaApplication.SystemDate,.T.,oAriaApplication.ActiveCompanyID,.F.,.T.)
            lcExSin2  = ''
            lcExSin1  = gfGetExSin(@lcExSin2,TMPFABRIC.cPriceCur)
            lnIECost1 = TMPFABRIC.nICost1 &lcExSin1 lnExRate &lcExSin2 lnCurrUnit

            STORE 1 TO lnCurrUnit, lnExRate
            lnExRate  = gfChkRate('lnCurrUnit',TMPFABRIC.cDutyCur,oAriaApplication.SystemDate,.T.,oAriaApplication.ActiveCompanyID,.F.,.T.)
            lcExSin2  = ''
            lcExSin1  = gfGetExSin(@lcExSin2,TMPFABRIC.cDutyCur)
             FOR lnCount = 2 TO 7
              lcCount = STR(lnCount,1)
              lnIECost&lcCount = EVALUATE('TMPFABRIC.nICost'+lcCount) &lcExSin1 lnExRate &lcExSin2 lnCurrUnit
            ENDFOR
            lnTotCost = (lnIECost1 + lnIECost2 + lnIECost3 + lnIECost4 + lnIECost5 + lnIECost6 + lnIECost7 ) / lnConf * lnTTranQty * lnUntQty
          ELSE
            lnTotCost  = TotCost / lnConf * lnTTranQty * lnUntQty
          ENDIF  
          *B608542,1 WAM 08/14/2008 (End)
          
          *E038220,1 WSH [End]
        
        ENDIF
        USE IN TMPFABRIC
        SELECT (lnCurAlias)
      ELSE
        lnTotCost = 0
      ENDIF
      *E038150,1 AMH [End]
      
      lnFTotCost = lnTotCost
    *N000587,1 HBG 2/22/2007 Specify the ccatgtyp to ignor non costing elements [Begin]
    *OTHERWISE
    CASE cCatGTyp = 'M' OR cCatGTyp='D'
    *N000587,1 [End]
      *N000587,1 WAM 12/01/2007 Get foreign and equivalent cost from PO cost sheet lines
      *lnTotCost  = &lcTmpBomSh..TotCost * lnTTranQty &lcExSign lnDutyRate &lcUntSin lnDutyUnit
      lcExSign  = gfGetExSin(@lcUntSin, IIF(ISNULL(&lcTmpBomSh..cCurrCode) OR EMPTY(&lcTmpBomSh..cCurrCode),oAriaApplication.BaseCurrency,&lcTmpBomSh..cCurrCode))
      lnExRate   = IIF(ISNULL(&lcTmpBomSh..nExRate)   OR &lcTmpBomSh..nExRate=0,1,&lcTmpBomSh..nExRate)
      lnCurrUnit = IIF(ISNULL(&lcTmpBomSh..nCUrrUnit) OR &lcTmpBomSh..nCUrrUnit=0,1,&lcTmpBomSh..nCurrUnit)
      lnTotCost  = &lcTmpBomSh..TotCost * lnTTranQty &lcExSign lnExRate &lcUntSin lnCurrUnit
      *N000587,1 WAM 12/01/2007 (End)
      lnFTotCost = &lcTmpBomSh..TotCost * lnTTranQty
  ENDCASE
  IF lnTranQty=0
    *B608223,1 MMT 08/16/2007 fix bug of wrong unit cost saved in Ctktbom[Start]
    *REPLACE Est_Cost WITH lnTotCost
    IF lnTranQty = 0 AND llFoundRec
      REPLACE Est_Cost WITH Est_Cost +lnTotCost -(EVALUATE(lcDetFile+'.StyQty')*EVALUATE(lcDetFile+'.unitcost'))
    ELSE
      REPLACE Est_Cost WITH lnTotCost  
    ENDIF  
   *B608223,1 MMT 08/16/2007 fix bug of wrong unit cost saved in Ctktbom[End]
  ELSE
    REPLACE Est_Cost WITH Est_Cost + lnTotCost
  ENDIF
  REPLACE UntCost  WITH IIF(Req_Qty<>0,Est_Cost/Req_Qty,0)

  lnEst&lcItemType = lnEst&lcItemType  + lnTotCost
  lnFEst&lcItemType= lnFEst&lcItemType + lnFTotCost
  
  *E038150,1 AMH Consider the fabric and trim since it has sizes [Start]
  *IF &lcTmpBomSh..cCatGTyp = 'S'
  IF EVALUATE(lcTmpBomSh+'.cCatGTyp') $ 'SF' OR;
    (EVALUATE(lcTmpBomSh+'.cCatGTyp') = 'T' AND EVALUATE(lcTmpBomSh+'.TRIM_INVT'))
  *E038150,1 AMH [End]
  
    FOR lnCount = 1 TO 8
      lcCount = STR(lnCount,1)
      *B608376,1 WAM 12/09/2007 Fix calculation of CT cost sheet required quantity.
      *REPLACE REQ_QTY&lcCount WITH REQ_QTY&lcCount+laReq[lnCount]*&lcTktSheet..UntQty
      REPLACE REQ_QTY&lcCount WITH REQ_QTY&lcCount+laReq[lnCount]*lnUntQty
      *B608376,1 WAM 12/09/2007 (End)
    ENDFOR
  ENDIF
  REPLACE cCompSizes WITH lcCmSizes 
  
  SELECT (lcTktSheet)
  LOCAL llEdtFld,llAddFld
  llEdtFld = (TYPE(lcTktSheet+'.cEdit_USER') <> 'U') .AND. (TYPE(lcTktSheet+'.dEdit_Date') <> 'U') .AND. (TYPE(lcTktSheet+'.cEdit_Time') <> 'U');
             AND (TYPE(lcTktSheet+'.cEdt_Ver') <> 'U')
  llAddFld = (TYPE(lcTktSheet+'.cAdd_user') <> 'U') AND (TYPE(lcTktSheet+'.dAdd_Date') <> 'U') AND (TYPE(lcTktSheet+'.cAdd_Time') <> 'U') ;
             AND (TYPE(lcTktSheet+'.cAdd_Ver') <> 'U') AND EMPTY(cAdd_user)
  
  *-- New Record
  IF llAddFld 
    *** stamp the record for this user with date and time
    REPLACE cAdd_User  WITH oAriaApplication.User_ID ,;
            dAdd_Date  WITH DATE()    ,;
            cAdd_Time  WITH gfGetTime(),;
            cAdd_Ver   WITH oAriaApplication.cShortVersion
  ELSE && Modified Record
    IF llEdtFld
       REPLACE cEdit_User  WITH oAriaApplication.User_ID ,;
               dEdit_Date  WITH DATE()    ,;
               cEdit_Time  WITH gfGetTime(),;
               cEdt_Ver    WITH oAriaApplication.cShortVersion
    ENDIF            
  ENDIF
  
  IF Pieces = 0 AND !llBackOnly
    DELETE
  ENDIF
  SELECT (lcTmpBomSh)
  DO CASE
    CASE &lcTmpBomSh..CCATGTYP = 'P'
      lnUnitPri = lnPrice
    CASE INLIST(&lcTmpBomSh..CCATGTYP,'M','D')
      IF &lcTmpBomSh..nPercent > 0
        lnUnitPri  = lnPrice*(&lcTmpBomSh..nPercent/100)
      ELSE
        lnUnitPri = &lcTmpBomSh..UntCost
      ENDIF  
    
    *E038150,1 AMH Consider the inventory type [Start]
    *CASE INLIST(&lcTmpBomSh..CCATGTYP,'F','T') .AND. SEEK(SUBSTR(lcCstItm,1,7)+lcCstClr,'Fabric')
      *lnUnitPri = Fabric.CostBuy/Fabric.Conv
    CASE INLIST(&lcTmpBomSh..CCATGTYP,'F','T')
      LOCAL lnCurAlias
      lnCurAlias = SELECT(0)
      
      *B607915,1 MMT 12/28/2006 bug of error in Po screen while save[Start]
      lnUnitPri = &lcTktSheet..UntCost
      *B607915,1 MMT 12/28/2006 bug of error in Po screen while save[End]
      
      IF gfOpnSqlFl('item',"TMPFABRIC","cinvtype = '"+EVALUATE(lcTmpBomSh+'.cInvTypC')+"' and style = '"+lcCstItm+"'","","style")
        SELECT TMPFABRIC
        LOCATE
        IF !EOF()

          *E038220,1 WSH Get Conversion Factor from UOM SQL file. [Start]
          *lnUnitPri = TotCost
          LOCAL lnConf
          lnConf    = 1
          
          =gfGetUOMData(EVALUATE(lcTmpBomSh + '.CUOMCODE'), '', '', @lnConf)

          *B608542,1 WAM 08/14/2008 Get material cost based on latest exchange rates
          *lnUnitPri = TotCost / lnConf
          IF llMulCurr
            STORE 0 TO lnIECost1, lnIECost2, lnIECost3, lnIECost4, lnIECost5, lnIECost6, lnIECost7 
            STORE 1 TO lnCurrUnit, lnExRate
            lnExRate  = gfChkRate('lnCurrUnit',TMPFABRIC.cPriceCur,oAriaApplication.SystemDate,.T.,oAriaApplication.ActiveCompanyID,.F.,.T.)
            lcExSin2  = ''
            lcExSin1  = gfGetExSin(@lcExSin2,TMPFABRIC.cPriceCur)
            lnIECost1 = TMPFABRIC.nICost1 &lcExSin1 lnExRate &lcExSin2 lnCurrUnit

            STORE 1 TO lnCurrUnit, lnExRate
            lnExRate  = gfChkRate('lnCurrUnit',TMPFABRIC.cDutyCur,oAriaApplication.SystemDate,.T.,oAriaApplication.ActiveCompanyID,.F.,.T.)
            lcExSin2  = ''
            lcExSin1  = gfGetExSin(@lcExSin2,TMPFABRIC.cDutyCur)
            FOR lnCount = 2 TO 7
              lcCount = STR(lnCount,1)
              lnIECost&lcCount = EVALUATE('TMPFABRIC.nICost'+lcCount) &lcExSin1 lnExRate &lcExSin2 lnCurrUnit
            ENDFOR
            lnUnitPri = (lnIECost1 + lnIECost2 + lnIECost3 + lnIECost4 + lnIECost5 + lnIECost6 + lnIECost7 ) / lnConf
          ELSE
            lnUnitPri = TotCost / lnConf
          ENDIF  
          *B608542,1 WAM 08/14/2008 (End)
          
          *E038220,1 WSH [End]

        ENDIF
        USE IN TMPFABRIC
        SELECT (lnCurAlias)
      ENDIF
    *E038150,1 AMH [End]
    
    *N000587,1 HBG 2/22/2007 Specify the ccatgtyp to ignor non costing elements [Begin]
    *OTHERWISE
    CASE cCatGTyp = 'S' 
    *N000587,1 [End]
      lnUnitPri = &lcTktSheet..UntCost
  ENDCASE
  SELECT (lcDetFile)
  *B608180,1 TMI [Start] define laNLineNo array to hold the max value of nLineno
  DIMENSION laNLineNo[1]
  laNLineNo = 0
  llAdd2DetFile = .F.
  *B608180,1 TMI [End  ]   
  
  *E038150,1 AMH Consider the inventory type [Start]
  *=SEEK(lcTranLett+'1'+lcTicketNo+STR(lnLineNo,6)+&lcTmpBomSh..Typ+PADR(lcItem,19)+;
        PADR(lcColor,6)+PADR(lcCstItm,19)+PADR(lcCstClr,6)+&lcTmpBomSh..MfgCode)
  *LOCATE REST WHILE ;
  cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode=;
  lcTranLett+'1'+lcTicketNo+STR(lnLineNo,6)+&lcTmpBomSh..Typ+PADR(lcItem,19)+;
  PADR(lcColor,6)+PADR(lcCstItm,19)+PADR(lcCstClr,6)+&lcTmpBomSh..MfgCode ;
  FOR cSizes = STRTRAN(lcSizes,',','')
  =SEEK(lcTranLett+'1'+lcTicketNo+STR(lnLineNo,6)+&lcTmpBomSh..Typ+lcInvType+PADR(lcItem,19)+;
        EVALUATE(lcTmpBomSh+'.cInvTypC')+PADR(lcCstItm,19)+&lcTmpBomSh..MfgCode)

  LOCATE REST WHILE ;
  cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+cInvType+style+cInvTypC+item+mfgcode=;
  lcTranLett+'1'+lcTicketNo+STR(lnLineNo,6)+&lcTmpBomSh..Typ+lcInvType+PADR(lcItem,19)+;
  EVALUATE(lcTmpBomSh+'.cInvTypC')+PADR(lcCstItm,19)+&lcTmpBomSh..MfgCode ;
  FOR cSizes = STRTRAN(lcSizes,',','')
  IF !FOUND()         
    *B608180,1 TMI [Start] if the same line is found for the same fabric but with differnt operation or totcost , then add a new line
    llAdd2DetFile = .T.
  ELSE
    *B608223,1 TMI [Start] check if called from lfvGenerate

    *B608749,1 WAM 12/02/2008 Commented out
    *IF llSheetGen
    *B608749,1 WAM 12/02/2008 (End)
    
      *B608223,1 TMI [End  ] 
 
      LOCATE REST WHILE ;
      cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+cInvType+style+cInvTypC+item+mfgcode=;
      lcTranLett+'1'+lcTicketNo+STR(lnLineNo,6)+&lcTmpBomSh..Typ+lcInvType+PADR(lcItem,19)+;
      EVALUATE(lcTmpBomSh+'.cInvTypC')+PADR(lcCstItm,19)+&lcTmpBomSh..MfgCode ;
      FOR cSizes = STRTRAN(lcSizes,',','') AND cOprCode = &lcTmpBomSh..cOprCode
      IF !FOUND()
        llAdd2DetFile = .T.
      ELSE
        *B608749,1 WAM 12/02/2008 COmpare BOM unit cost with BOM line unit cost 
        *LOCATE REST WHILE ;
        cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+cInvType+style+cInvTypC+item+mfgcode=;
        lcTranLett+'1'+lcTicketNo+STR(lnLineNo,6)+&lcTmpBomSh..Typ+lcInvType+PADR(lcItem,19)+;
        EVALUATE(lcTmpBomSh+'.cInvTypC')+PADR(lcCstItm,19)+&lcTmpBomSh..MfgCode ;
        FOR cSizes = STRTRAN(lcSizes,',','') AND cOprCode = &lcTmpBomSh..cOprCode AND ItemAmt = &lcTmpBomSh..TotCost
		
		*B608902,1 MMT 06/22/2009 Fix bugs of not updating PO Cost Sheet when edit PO[Start]
		*!*	        LOCATE REST WHILE ;
		*!*	        cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+cInvType+style+cInvTypC+item+mfgcode=;
		*!*	        lcTranLett+'1'+lcTicketNo+STR(lnLineNo,6)+&lcTmpBomSh..Typ+lcInvType+PADR(lcItem,19)+;
		*!*	        EVALUATE(lcTmpBomSh+'.cInvTypC')+PADR(lcCstItm,19)+&lcTmpBomSh..MfgCode ;
		*!*	        FOR cSizes = STRTRAN(lcSizes,',','') AND cOprCode = &lcTmpBomSh..cOprCode AND UnitCost = &lcTmpBomSh..UntCost
		*!*	        *B608749,1 WAM 12/02/2008 COmpare BOM unit cost with BOM line unit cost 

		*!*	        IF !FOUND()
		*!*	          llAdd2DetFile = .T.
		*!*	        ENDIF
   		*B608902,1 MMT 06/22/2009 Fix bugs of not updating PO Cost Sheet when edit PO[End]
      ENDIF
   
      *B608749,1 WAM 12/02/2008 Commented out
      *IF llAdd2DetFile
      *  SELECT MAX(nLineNo) FROM &lcDetFile INTO ARRAY laNLineNo
      *  laNLineNo[1] = laNLineNo[1] + 1
      *ENDIF
      *B608749,1 WAM 12/02/2008 Commented out

    *B608749,1 WAM 12/02/2008 Commented out
    *ENDIF
    *B608749,1 WAM 12/02/2008 (End)
    
    *B608223,1 TMI [Start] 
  ENDIF
  *B608223,1 TMI [End  ] 
 
  IF llAdd2DetFile
    *B608749,1 WAM 12/02/2008 Increament the NLINENO field in BOMLINE when the value of the uneque key repeated.
    SELECT (lcDetFile)
    laNLineNo[1] = 0
    SCAN FOR ;
      CIMTYP+CTYPE+CTKTNO+SHIPNO+STR(LINENO,6)+CBOMTYP+CINVTYPE+STYLE+CINVTYPC+ITEM+MFGCODE+CRSESSION+CSTYGRADE+STR(NLINENO,4)=;
      lcTranLett+'1'+lcTicketNo+SPACE(6)+STR(lnLineNo,6)+&lcTmpBomSh..Typ+lcInvType+PADR(lcItem,19)+;
      EVALUATE(lcTmpBomSh+'.cInvTypC')+PADR(lcCstItm,19)+&lcTmpBomSh..MfgCode+SPACE(6)+lcGrade

      laNLineNo[1] = MAX(IIF(ISNULL(nLineNo),0,nLineNo),laNLineNo[1])
    ENDSCAN
    laNLineNo[1] = laNLineNo[1] + 1
    *B608749,1 WAM 12/02/2008 (End)

    *B608180,1 TMI [End  ] 
    APPEND BLANK
    *REPLACE cIMTyp     WITH lcTranLett     ,;
            cTktNo     WITH lcTicketNo     ,;
            LineNo     WITH lnLineNo       ,;
            cStyGrade  WITH lcGrade        ,;
            Style      WITH lcItem         ,;
            SClr       WITH lcColor        ,;
            cBomTyp    WITH &lcTmpBomSh..Typ,;
            cType      WITH '1'            ,;
            cCatGTyp   WITH &lcTmpBomSh..cCatGTyp   ,;
            cOprCode   WITH &lcTmpBomSh..cOprCode   ,;
            UnitQty    WITH &lcTmpBomSh..nBomTotQty ,;
            UnitCost   WITH lnUnitPri      ,;
            Item       WITH lcCstItm       ,;
            IClr       WITH lcCstClr       ,;
            MfgCode    WITH &lcTmpBomSh..MfgCode    ,;
            Dyelot     WITH lcCstItmDye ,;
            cSizes     WITH STRTRAN(lcSizes,',','') ,;
            cCompSizes WITH lcCmSizes
    REPLACE cIMTyp     WITH lcTranLett     ,;
            cTktNo     WITH lcTicketNo     ,;
            LineNo     WITH lnLineNo       ,;
            cStyGrade  WITH lcGrade        ,;
            Style      WITH lcItem         ,;
            cInvType   WITH lcInvType      ,;
            cBomTyp    WITH &lcTmpBomSh..Typ,;
            cType      WITH '1'            ,;
            cCatGTyp   WITH &lcTmpBomSh..cCatGTyp   ,;
            cOprCode   WITH &lcTmpBomSh..cOprCode   ,;
            UnitQty    WITH &lcTmpBomSh..nBomTotQty ,;
            UnitCost   WITH lnUnitPri      ,;
            Item       WITH lcCstItm       ,;
            cInvTypC   WITH EVALUATE(lcTmpBomSh+'.cInvTypC'),;
            MfgCode    WITH &lcTmpBomSh..MfgCode    ,;
            Dyelot     WITH lcCstItmDye ,;
            cSizes     WITH STRTRAN(lcSizes,',','') ,;
            cCompSizes WITH lcCmSizes
    REPLACE cCostStat WITH &lcTmpBomSh..cCostStat,;
            nPercent   WITH &lcTmpBomSh..nPercent
    *B608180,1 TMI [Start] update the nLineno field 
    *B608223,1 TMI [Start] check if called from lfvGenerate

    *B608749,1 WAM 12/02/2008 Commented out
    *IF llSheetGen
    *B608749,1 WAM 12/02/2008 (End)
    
      *B608223,1 TMI [End  ] 
      REPLACE nLineno    WITH laNLineNo[1]
      *B608223,1 TMI [Start] 

    *B608749,1 WAM 12/02/2008 Commented out
    *ENDIF
    *B608749,1 WAM 12/02/2008 (End)
    
    *B608223,1 TMI [End  ] 
    *B608180,1 TMI [End  ] 
    
  ENDIF
  *E038150,1 AMH [End]

  IF lnTranQty = 0
     REPLACE ItemAmt  WITH ItemQty*lnUnitPri ,;
             UnitCost WITH IIF(ItemQty=0,0,ItemAmt/ItemQty) 
    
  ELSE
    *! B609246,1 MMT 05/13/2010 Fix problem of Wrong unit Qty in Ctktbom file when PO edited[Start]
    *REPLACE StyQty   WITH StyQty  + lnTranQty ,;
            ItemQty  WITH ItemQty + lnTranQty*UnitQty  ,;
            UnitQty  WITH IIF(StyQty=0,0,ItemQty/StyQty) ,;
            ItemAmt  WITH ItemAmt + lnTranQty*UnitQty*lnUnitPri ,;
            UnitCost WITH IIF(ItemQty=0,0,ItemAmt/ItemQty)
    SELECT (lcCostElem)
    =SEEK('B'+&lcTmpBomSh..Typ+lcCstItm+&lcTmpBomSh..MfgCode+ lcCstItmDye)
    LOCATE REST WHILE cTrType+CBOMTYP +Item+MFGCode+cDyelot = 'B'+&lcTmpBomSh..Typ+lcCstItm+&lcTmpBomSh..MfgCode+ lcCstItmDye ;
                    FOR cSizes =STRTRAN(lcSizes,',','')
    IF !FOUND()
      REPLACE StyQty WITH StyQty  + lnTranQty IN (lcDetFile)
      INSERT INTO (lcCostElem) (cTrType,CBOMTYP ,Item, MFGCode,cDyelot,cSizes);
                          VALUES ('B',&lcTmpBomSh..Typ,lcCstItm,&lcTmpBomSh..MfgCode,lcCstItmDye,STRTRAN(lcSizes,',',''))
    ENDIF
    
    SELECT (lcDetFile)
    REPLACE ItemQty  WITH ItemQty + lnTranQty*&lcTmpBomSh..nBomTotQty,;
            UnitQty  WITH IIF(StyQty=0,0,ItemQty/StyQty) ,;
            ItemAmt  WITH ItemAmt + lnTranQty*&lcTmpBomSh..nBomTotQty*lnUnitPri ,;
            UnitCost WITH IIF(ItemQty=0,0,ItemAmt/ItemQty)
    *! B609246,1 MMT 05/13/2010 Fix problem of Wrong unit Qty in Ctktbom file when PO edited[End]
  ENDIF
  
  
  *N000587,1 WAM 12/01/2007 Get foreign and equivalent cost from PO cost sheet lines
  *wael
  *REPLACE cCUrrCode WITH IIF(ISNULL(&lcTmpBomSh..cCurrCode) OR EMPTY(&lcTmpBomSh..cCurrCode),oAriaApplication.BaseCurrency,&lcTmpBomSh..cCurrCode) ,;
          nExRate   WITH IIF(ISNULL(&lcTmpBomSh..nExRate)   OR &lcTmpBomSh..nExRate=0  ,1,&lcTmpBomSh..nExRate) ,;
          nCurrUnit WITH IIF(ISNULL(&lcTmpBomSh..nCUrrUnit) OR &lcTmpBomSh..nCUrrUnit=0,1,&lcTmpBomSh..nCurrUnit)
  IF cCatGTyp = 'P'
    REPLACE cCurrCode WITH lcPriceCur ,;
            nExRate   WITH lnPriceRate ,;
            nCurrUnit WITH lnPriceUnit
  ELSE
    REPLACE cCUrrCode WITH IIF(ISNULL(&lcTmpBomSh..cCurrCode) OR EMPTY(&lcTmpBomSh..cCurrCode),oAriaApplication.BaseCurrency,&lcTmpBomSh..cCurrCode) ,;
            nExRate   WITH IIF(ISNULL(&lcTmpBomSh..nExRate)   OR &lcTmpBomSh..nExRate=0  ,1,&lcTmpBomSh..nExRate) ,;
            nCurrUnit WITH IIF(ISNULL(&lcTmpBomSh..nCUrrUnit) OR &lcTmpBomSh..nCUrrUnit=0,1,&lcTmpBomSh..nCurrUnit)
  ENDIF
  *wael
  *N000587,1 WAM 12/01/2007 (End)

  * HES B608833,1 Update the audit fields
  =gfAdd_Info(lcDetFile)
  * HES B608833

  IF StyQty = 0 AND !llBackOnly
    DELETE
  ENDIF
ENDSCAN
IF llContinue
  
  *N119813,1 AMH Update all transaction in the POSHDR file [Start]
  *DO CASE
  *  CASE lcTranType='M' .AND. SEEK(lcTicketNo,'CUTTKTH')  .AND. CUTTKTH.Status # 'H'
  *    SELECT CUTTKTH
  *    =RLOCK()
  *    REPLACE CLASTOPR   WITH lcLastOpr ,;
  *            NEST_COST1 WITH NEST_COST1 + lnEst1 ,;
  *            NEST_COST2 WITH NEST_COST2 + lnEst2 ,;
  *            NEST_COST3 WITH NEST_COST3 + lnEst3 ,;
  *            NEST_COST4 WITH NEST_COST4 + lnEst4 ,;
  *            NEST_COST5 WITH NEST_COST5 + lnEst5
  *    UNLOCK
  *  CASE lcTranType$'DI' .AND. SEEK(IIF(lcTranType='I','P','D')+lcTicketNo,'POSHDR') .AND. POSHDR.Status='O'
  *    SELECT POSHDR
  *    =RLOCK()
  *    REPLACE CLASTOPR WITH lcLastOpr ,;
  *            NICOST1  WITH NICOST1 + lnEst1 ,;
  *            NICOST2  WITH NICOST2 + lnEst2 ,;
  *            NICOST3  WITH NICOST3 + lnEst3 ,;
  *            NICOST4  WITH NICOST4 + lnEst4 ,;
  *            NICOST5  WITH NICOST5 + lnEst5 ,;
  *            NFCOST1  WITH NFCOST1 + lnFEst1 ,;
  *            NFCOST2  WITH NFCOST2 + lnFEst2 ,;
  *            NFCOST3  WITH NFCOST3 + lnFEst3 ,;
  *            NFCOST4  WITH NFCOST4 + lnFEst4 ,;
  *            NFCOST5  WITH NFCOST5 + lnFEst5
  *    UNLOCK
  *  CASE lcTranType='T' .AND. SEEK(lcTicketNo,'MMFGORDH') .AND. MMFGORDH.Status='O'
  *    SELECT MMFGORDH
  *    =RLOCK()
  *    REPLACE CLASTOPR   WITH lcLastOpr ,;
  *            NEST_COST1 WITH NEST_COST1 + lnEst1 ,;
  *            NEST_COST2 WITH NEST_COST2 + lnEst2 ,;
  *            NEST_COST3 WITH NEST_COST3 + lnEst3 ,;
  *            NEST_COST4 WITH NEST_COST4 + lnEst4
  *    UNLOCK
  *ENDCASE
  IF EVALUATE(lcPOSHDR+'.Status')='O'
    SELECT (lcPOSHDR)
    REPLACE CLASTOPR WITH lcLastOpr ,;
            NICOST1  WITH NICOST1 + lnEst1 ,;
            NICOST2  WITH NICOST2 + lnEst2 ,;
            NICOST3  WITH NICOST3 + lnEst3 ,;
            NICOST4  WITH NICOST4 + lnEst4 ,;
            NICOST5  WITH NICOST5 + lnEst5 ,;
            NICOST6  WITH NICOST6 + lnEst6 ,;
            NICOST7  WITH NICOST7 + lnEst7 ,;
            NFCOST1  WITH NFCOST1 + lnFEst1 ,;
            NFCOST2  WITH NFCOST2 + lnFEst2 ,;
            NFCOST3  WITH NFCOST3 + lnFEst3 ,;
            NFCOST4  WITH NFCOST4 + lnFEst4 ,;
            NFCOST5  WITH NFCOST5 + lnFEst5 ,;
            NFCOST6  WITH NFCOST6 + lnFEst6 ,;
            NFCOST7  WITH NFCOST7 + lnFEst7
  ENDIF
  *N119813,1 AMH [End]
  
ENDIF
IF USED('STYLE')
  SELECT STYLE
  SET FILTER TO &lcStyFIlt
ENDIF
USE IN (lcTmpBomSh)
*! B609246,1 MMT 05/13/2010 Fix problem of Wrong unit Qty in Ctktbom file when PO edited[Start]
USE IN (lcCostElem)
*! B609246,1 MMT 05/13/2010 Fix problem of Wrong unit Qty in Ctktbom file when PO edited[End]


ERASE (oAriaApplication.WorkDir+lcTmpBomSh+'.DBF')
ERASE (oAriaApplication.WorkDir+lcTmpBomSh+'.CDX')
ERASE (oAriaApplication.WorkDir+lcTmpBomSh+'.FPT')

SELECT (lnAlias)

*! B609253,1 MMT 05/17/2010 Release variables from gfSheetItem Function after usage[Start]
RELEASE llStyconfg,llSheetGen,lcMjrMsk,lcMjrHdr,lcNMjrMsk,laItemSeg,lcClrMsk,lnClrPos,lcItemFile,;
		llDyelot,lcScale,lcItemType,lnTranQty,lcSizes,lnCount,lcCstClr,lnUnitPri,;
		lcCstItm,laMfgRFld,lnLastSeq,lnSizePos,lcTmpBomSh,laReq,;
		lcContCode,lcContName,lcOperSeq,llInHouse,llMfgOpr,lnLeadTime,;
		lcMfgGlAcnt,lcExSign,lcUntSin,lnAlias,lcPriceCur,lnPriceRate,;
		lnPriceUnit,lcDutyCur,lnDutyRate,lnDutyUnit,lnUntQty,lcTranLett
RELEASE M_WAREHOUSE,M_DYELOT,M_MATDYE,M_USEEXSSC
RELEASE laMfgRFld,laItemSeg,laReq,llMulCurr,lnSclCnt,laFileStru,lcCostElem,lcLine,;
		lcStyFIlt,lcGrade,llMsgDispd,lnI,lcI,lcCstClr,lcCstItm,lcCmSizes,lcCstItmDye,lcCsItmSc,;
		lnItmClrPos,lnItmClrLen,lcItemWidth,lnCurAlias,lcSvOrd,llFoundRec ,lnTTranQty ,lnFTotCost
RELEASE lnTotCost,lnCurrUnit,lcExSign,lnExRate,lnConf,lnIECost1, lnIECost2, lnIECost3, lnIECost4,;
		lnIECost5, lnIECost6, lnIECost7 
RELEASE lcExSin2  ,lcExSin1  ,lcCount,lnEst1,lnEst2,lnEst3,lnEst4,lnEst5,lnEst6,lnEst7,lnFEst1,;
		lnFEst2,lnFEst3,lnFEst4,lnFEst5,lnFEst6,lnFEst7,llEdtFld,llAddFld,laNLineNo,llAdd2DetFile 		
*! B609253,1 MMT 05/17/2010 Release variables from gfSheetItem Function after usage[End]

RETURN(llContinue)

*!*************************************************************
*! Name      : gfSelDyelot
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Check ticket sheet item have dyelots
*!*************************************************************
*! Calls     : FDYEBROW,SDYEBROW
*!*************************************************************
*! Parameters: lcTranType : Transaction type   ('M'-'I'-'T')
*!             lcType     : 'F' Fabric / 'S' Style
*!             lcItem     : Ticket Item 
*!             lcColor    : Ticket Color
*!             lcWareCode : Warehouse
*!             lcDyelot   : Dyelot
*!             lcCstItem  : Cost sheet item
*!             lcCstItmClr: Cost sheet color
*!             lcTmpName  : Temp Name
*!             llBackOnly : Update files in background
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  
*!*************************************************************
FUNCTION gfSelDyelot
*N119813,1 AMH Consider case of configuration [Start]
*PARAMETER lcTranType,lcType,lcItem,lcColor,lcWareCode,lcDyelot,lcCstItem,;
          lcCstItmClr,lcTmpName,llBackOnly

*E038150,1 AMH Pass the inventory type instead of color [Start]
*LPARAMETERS lcTranType,lcType,lcItem,lcColor,lcWareCode,lcDyelot,lcCstItem,;
          lcCstItmClr,lcTmpName,llBackOnly,llStyConfg
LPARAMETERS lcTranType,lcType,lcItem,lcInvType,lcWareCode,lcDyelot,lcCstItem,;
          lcInvTypC,lcTmpName,llBackOnly,llStyConfg
*E038150,1 AMH [End]

*N119813,1 AMH [End]
          
PRIVATE lnAlias,lnSelect,lcMessage,lcMessage1,lcItmHdr

lnAlias = SELECT()

*E038150,1 AMH Consider the inventory type [Start]
*lcItmHdr= gfItemMask("HI")
lcItmHdr= gfItemMask("HI","",lcInvType)
lcCstHdr= gfItemMask("HI","",lcInvTypC)
*E038150,1 AMH [End]

STORE '' TO M_WAREHOUSE

*N119813,1 AMH Use the correct company ID [Start]
*=gfGetMemVar('M_WAREHOUSE',gcAct_Comp)
=gfGetMemVar('M_WAREHOUSE',oAriaApplication.ActiveCompanyID)
*N119813,1 AMH [End]

*E300935,4 [begin]
*E300935,4 adjust condition to update without message if llBackOnly is .T.
lnSelect = 1
IF !llBackOnly AND !EMPTY(lcDyelot)
  
  *E038150,1 AMH Remove the lcColor parameter [Start]
  *IF lcTranType = 'T'
  *  IF EMPTY(lcItem+lcColor)
  *    lcMessage = 'One or more Fabrics'+;
  *    IIF(M_WAREHOUSE='Y','in warehouse '+lcWareCode,'')
  *  ELSE
  *    lcMessage = 'Fabric/Color'+IIF(M_WAREHOUSE='Y','/Warehouse: ',': ')+;
  *                ALLTRIM(lcItem)+'/'+ALLTRIM(lcColor)+;
  *                IIF(M_WAREHOUSE='Y','/'+lcWareCode,'')
  *  ENDIF
  *ELSE
  *  IF EMPTY(lcItem)
  *    lcMessage = 'One or more '+ALLTRIM(lcItmHdr)+;
  *    IIF(M_WAREHOUSE='Y','in warehouse '+lcWareCode,'')
  *  ELSE
  *    lcMessage = ALLTRIM(lcItmHdr)+IIF(M_WAREHOUSE='Y','/Warehouse: ',': ')+;
  *                ALLTRIM(lcItem)+IIF(M_WAREHOUSE='Y','/'+lcWareCode,'')
  *  ENDIF
  *ENDIF
  IF EMPTY(lcItem)
    lcMessage = 'One or more '+ALLTRIM(lcItmHdr)+;
    IIF(M_WAREHOUSE='Y','in warehouse '+lcWareCode,'')
  ELSE
    lcMessage = ALLTRIM(lcItmHdr)+IIF(M_WAREHOUSE='Y','/Warehouse: ',': ')+;
                ALLTRIM(lcItem)+IIF(M_WAREHOUSE='Y','/'+lcWareCode,'')
  ENDIF
  *E038150,1 AMH [end]

  IF lcType = 'F'
    
    *E038150,1 AMH Consider the inventory type [Start]
    *lcMessage1 = 'Fabric/Color/Dyelot: '+ALLTRIM(lcCstItem)+'/'+ALLTRIM(lcCstItmClr)+;
                  '/'+ALLTRIM(lcDyelot)
    lcMessage1 = ALLTRIM(lcCstHdr)+'/Dyelot: '+ALLTRIM(lcCstItem)+'/'+ALLTRIM(lcDyelot)
    *E038150,1 AMH [End]
    
  ELSE
    
    *N119813,1 AMH Consider case of configuration [Start]
    *lcMessage1 = ALLTRIM(lcItmHdr)+'/Dyelot: '+ALLTRIM(lcCstItem)+'/'+ALLTRIM(lcDyelot)
    lcMessage1 = ALLTRIM(lcItmHdr)+'/'+IIF(llStyConfg,LANG_ARIA_CONFIGMESG,LANG_ARIA_DYELOTMESG)+': '+;
                 ALLTRIM(lcCstItem)+'/'+ALLTRIM(lcDyelot)
    *N119813,1 AMH [End]
    
  ENDIF

  *E300725,1 Message : 38090
  *E300725,1 Fabric/Color/warehouse: xxx/xxx/xxx requires fabric/color/dyelot
  *E300725,1 xxx/xxx/xxx. This dyelot is not available for the fabric
  *E300725,1 Button : 38011
  *E300725,1 Add dyelot  Select dyelot Cancel
  
  *N119813,1 AMH Consider case of configuration [Start]
  *lnSelect=gfModalGen('QRM38090B38011','ALERT',lcMessage+'|'+lcMessage1+'|'+;
                      IIF(lcType='F','Fabric/Color',ALLTRIM(lcItmHdr)))
  
  *E038150,1 AMH Consider the inventory type [Start]
  IF llStyConfg .AND. lcType='S'
    *lnSelect=gfModalGen('QRM38283B38032','ALERT',lcMessage+'|'+lcMessage1+'|'+LANG_ARIA_CONFIGMESG+'|'+;
                        IIF(lcType='F','Fabric/Color',ALLTRIM(lcItmHdr)))
    lnSelect=gfModalGen('QRM38283B38032','ALERT',lcMessage+'|'+lcMessage1+'|'+LANG_ARIA_CONFIGMESG+'|'+ALLTRIM(lcCstHdr))
  ELSE
    *lnSelect=gfModalGen('QRM38283B38011','ALERT',lcMessage+'|'+lcMessage1+'|'+LANG_ARIA_DYELOTMESG+'|'+;
                        IIF(lcType='F','Fabric/Color',ALLTRIM(lcItmHdr)))
    lnSelect=gfModalGen('QRM38283B38011','ALERT',lcMessage+'|'+lcMessage1+'|'+LANG_ARIA_DYELOTMESG+'|'+ALLTRIM(lcCstHdr))
  ENDIF
  *E038150,1 AMH [End]
  
  *N119813,1 AMH [End]
  
ENDIF  

IF EMPTY(lcDyelot)
  lnSelect = 2
ENDIF
DO CASE
  CASE lnSelect = 1
    
    *E038150,1 AMH Consider the inventory type [Start]
    *IF lcType = 'F' .AND. !SEEK(lcCstItem+lcCstItmClr+lcWareCode+lcDyelot,'FABDYE')
    *  DO gpAdFabWar WITH lcCstItem, lcCstItmClr, lcDyelot, lcWareCode,lcTmpName
    *ENDIF
    IF lcType = 'F' .AND. !SEEK(lcCstItem+lcWareCode+lcDyelot,'FABDYE')
      DO gfAdItemWar WITH lcInvTypC, lcCstItem, lcDyelot, lcWareCode, lcTmpName
    ENDIF
    *E038150,1 AMH [End]
    
    IF lcType = 'S' .AND. !SEEK(lcCstItem+lcWareCode+lcDyelot,'STYDYE')
      DO gpAdStyWar WITH lcCstItem, lcDyelot, lcWareCode
    ENDIF
  CASE lnSelect = 2
    
    *N119813,1 AMH Consider case of configuration [Start]
    *IF !IIF(lcType='F',FDYEBROW(lcCstItem,lcCstItmClr,@lcDyelot,.T.),;
                    SDYEBROW(lcCstItem,@lcDyelot,.T.)) .OR. EMPTY(lcDyelot)
    
    *E038150,1 AMH Consider the inventory type [Start]
    *IF !IIF(lcType='F',FDYEBROW(lcCstItem,lcCstItmClr,@lcDyelot,.T.),;
                    SDYEBROW(lcCstItem,@lcDyelot,.T.,.F.,.F.,.F.,.F.,.F.,llStyConfg)) .OR. EMPTY(lcDyelot)
    IF !IIF(lcType='F',gfItmDyBrw(lcInvTypC,lcCstItem,@lcDyelot,.T.),;
                    SDYEBROW(lcCstItem,@lcDyelot,.T.,.F.,.F.,.F.,.F.,.F.,llStyConfg)) .OR. EMPTY(lcDyelot)
    *E038150,1 AMH [End]
    
    *N119813,1 AMH [End]
    
      lcDyelot=SPACE(10)
      RETURN(.F.)
    ENDIF
  CASE lnSelect = 3
    lcDyelot=SPACE(10)
    RETURN(.F.)
ENDCASE
SELECT (lnAlias)
RETURN

*!*************************************************************
*! Name      : lfUpdPOBom                    
*! Developer : Timour A. K.
*! Date      : 05/30/1999
*! Purpose   : Update Temp P/O cost sheet used in Creation of 
*!             P/o cost sheet in gfSheetItem.
*!*************************************************************
*! Calls From: gfSheetItem()
*!*************************************************************
*! Parameters:  Nome
*!*************************************************************
*! Returns   :  None
*!:************************************************************
FUNCTION lfUpdPOBom
LOCAL lnI

*--Update rate per lb cost in mfg record in dye order cost sheet.
*--If dye order cost sheet.
IF lcTranType = 'D'
  *--Overwite the cost of dyeing operation by the rate per lib * weight.
  lcDyeOpr = gfGetMemvar('M_DYEOPR')
  SELECT (lcTmpBomSh)
  LOCATE FOR CCATGTYP='M' AND cItmMajor=STYLE.cStyMajor AND MfgCode=lcDyeOpr
  lcI = Typ
  
  *N119813,1 AMH Use the cursor name of the POSLN file [Start]
  *IF FOUND() AND POSLN.nCost&lcI <> 0
  *  REPLACE UntCost    WITH POSLN.nCost&lcI,;
  *          nBomTotQty WITH 1,;
  *          TotCost    WITH POSLN.nCost&lcI
  *ENDIF

  *N119813,1 KHM 08/09/2004 Use nFCost instead of nCost [Start]
  *IF FOUND() AND EVALUATE(lcPOSLN+'.nCost'+lcI) <> 0
  IF FOUND() AND EVALUATE(lcPOSLN+'.nFCost'+lcI) <> 0
  *N119813,1 KHM 08/09/2004 [End]
    REPLACE UntCost    WITH EVALUATE(lcPOSLN+'.nFCost'+lcI),;
            nBomTotQty WITH 1,;
            TotCost    WITH EVALUATE(lcPOSLN+'.nFCost'+lcI)
  ENDIF
  *N119813,1 AMH [End]

ELSE

  *--If there is More than one PPrice for the same style, create the P/O cost
  *--sheet for PPrice element only one record with the cost that in P/O.
  SELECT (lcTmpBomSh)
  =SEEK('1')
  
  *E038150,1 AMH Consider the inventory type [Start]
  *LOCATE REST WHILE typ+item+iclr+citmmajor+citmmask = '1' ;
              FOR cItmMajor=STYLE.cStyMajor
  LOCATE REST WHILE typ+cInvTypC+item+cInvType+citmmajor+citmmask = '1' ;
              FOR cItmMajor=STYLE.cStyMajor
  lcCostStat = cCostStat
  *E038150,1 AMH [End]
  
  IF FOUND()

    *N000587,1 WAM 12/01/2007 Get foreign and equivalent cost from PO cost sheet lines
    lcCUrrCode = IIF(ISNULL(cCurrCode) OR EMPTY(cCurrCode),oAriaApplication.BaseCurrency,cCurrCode)
    lnCurrUnit = IIF(ISNULL(nCUrrUnit) OR nCUrrUnit=0,1,nCUrrUnit)
    lnExRate   = IIF(ISNULL(nExRate) OR nExRate=0,1,nExrate)
    *N000587,1 WAM 12/01/2007 (End)
    
    *E038150,1 AMH Consider the inventory type [Start]
    *DELETE REST WHILE typ+item+iclr+citmmajor+citmmask = '1' ;
                FOR cItmMajor=STYLE.cStyMajor
    DELETE REST WHILE typ+cInvTypC+item+cInvType+citmmajor+citmmask = '1' ;
                FOR cItmMajor=STYLE.cStyMajor
    *E038150,1 AMH [End]

  
  IF lnPrice <> 0
    *--Delete the cost sheet created from the BOM that contail multiple 
    *--record for PPrice and create a new one from P/O with P/O price.
    APPEND BLANK
    
    *E038150,1 AMH Consider the inventory type [Start]
    *REPLACE cItmMajor  WITH STYLE.cStyMajor,;
            Typ        WITH '1',;
            cItmMask   WITH STYLE.Style,;
            MfgCode    WITH IIF(!STYLE.LDetCost,'*1',"******"),;
            Uom        WITH 'EAC',;
            nBomTotQty WITH 1,;
            UntCost    WITH lnPrice,;
            TotCost    WITH lnPrice,;
            CCatgTyp   WITH 'P'

    *E038220,1 WSH Change UOM-Code to Relation Code [Start]
    *REPLACE cItmMajor  WITH STYLE.cStyMajor,;
            cInvType   WITH "0001",;
            Typ        WITH '1',;
            cItmMask   WITH STYLE.Style,;
            MfgCode    WITH IIF(!STYLE.LDetCost,'*1',"******"),;
            Uom        WITH 'EAC',;
            nBomTotQty WITH 1,;
            UntCost    WITH lnPrice,;
            TotCost    WITH lnPrice,;
            CCatgTyp   WITH 'P'
    REPLACE cItmMajor  WITH STYLE.cStyMajor,;
            cInvType   WITH "0001",;
            Typ        WITH '1',;
            cItmMask   WITH STYLE.Style,;
            MfgCode    WITH IIF(!STYLE.LDetCost,'*1',"******"),;
            nBomTotQty WITH 1,;
            UntCost    WITH lnPrice,;
            TotCost    WITH lnPrice,;
            CCatgTyp   WITH 'P'
    *N000587,1 WAM 12/01/2007 Get foreign and equivalent cost from PO cost sheet lines

    *B608749,1 WAM 12/02/2008 Update price currency and exchange rate with PO currency and exchange rate
    *REPLACE cCurrCode WITH lcCUrrCode ,; 
            nCUrrUnit WITH lnCurrUnit ,; 
            nExrate   WITH lnExRate

    REPLACE cCurrCode WITH lcPriceCur ,; 
            nCUrrUnit WITH lnPriceRate,; 
            nExrate   WITH lnPriceUnit
    *B608749,1 WAM 12/02/2008 (End)

    *N000587,1 WAM 12/01/2007 (End)
    
    *--Get Relation for Default UOM-Code
    LOCAL lcRelCode
    lcRelCode = ''

    =gfGetUOMData(@lcRelCode, '', '', 1, .F.)
    
    REPLACE cUOMCode WITH lcRelCode
    *E038220,1 WSH [End]

    *E038150,1 AMH [End]
    
    REPLACE cCostStat WITH lcCostStat      
  ENDIF
  ENDIF

  *--If no detail costing for style and there is a cost elements entered.
  *--Create or update the P/O cost sheet for this cost element with the
  *--cost that ented in P/O.
  IF !STYLE.LDetCost
  *--Delete the cost sheet created from the BOm and create a new one from P/O.
    DELETE FOR CCATGTYP<>'P' AND cItmMajor=STYLE.cStyMajor

    *B607915,1 MMT 12/28/2006 bug of error in Po screen while save[Start]
    llTINVT= ALLTRIM(gfGetMemVar('M_TINVT',oAriaApplication.ActiveCompanyID))='Y'
    *B607915,1 MMT 12/28/2006 bug of error in Po screen while save[End]
    
    *E038150,1 AMH Consider the siven cost elements [Start]
    *FOR lnI=2 TO 5
    FOR lnI=2 TO 7
    *E038150,1 AMH [End]
    
      lcI=STR(lnI,1)
      
      *N119813,1 AMH Use the cursor name of the POSLN file [Start]
      *IF POSLN.nCost&lcI <> 0
      *  lcIType&lcI = gfGetMemVar('M_cIType'+lcI)
      *  APPEND BLANK
      *  GATHER MEMVAR
      *  REPLACE cItmMajor WITH STYLE.cStyMajor,;
      *          Typ       WITH lcI,;
      *          cItmMask  WITH STYLE.Style,;
      *          MfgCode   WITH '*'+lcI ,;
      *          Uom       WITH 'EAC',;
      *          UntCost   WITH POSLN.nCost&lcI,;
      *          nBomTotQty WITH 1,;
      *          TotCost   WITH POSLN.nCost&lcI,;
      *          CCatgTyp  WITH lcIType&lcI
      *ENDIF

      *N119813,1 KHM 08/09/2004 Use nFCost instead of nCost [Start]
      *IF EVALUATE(lcPOSLN+'.nCost'+lcI) <> 0
      IF EVALUATE(lcPOSLN+'.nFCost'+lcI) <> 0
      *N119813,1 KHM 08/09/2004 [End]      
        lcIType&lcI = gfGetMemVar('M_cIType'+lcI)
        *B609572,1 MMT 04/19/2011 Fix bug of wrong Costing elements desc. in case of detail cost No Styles[Start]
        lcCISLBL&lcI = gfGetMemVar('M_CISLBL'+lcI)
        *B609572,1 MMT 04/19/2011 Fix bug of wrong Costing elements desc. in case of detail cost No Styles[End]
        APPEND BLANK
        GATHER MEMVAR
        
        *E038150,1 AMH Consider the inventory type [Start]
        *REPLACE cItmMajor  WITH STYLE.cStyMajor,;
                Typ        WITH lcI,;
                cItmMask   WITH STYLE.Style,;
                MfgCode    WITH '*'+lcI ,;
                Uom        WITH 'EAC',;
                UntCost    WITH EVALUATE(lcPOSLN+'.nFCost'+lcI),;
                nBomTotQty WITH 1,;
                TotCost    WITH EVALUATE(lcPOSLN+'.nFCost'+lcI),;
                CCatgTyp   WITH lcIType&lcI

        *E038220,1 WSH Change UOM-Code to Relation Code [Start]
        *REPLACE cItmMajor  WITH STYLE.cStyMajor,;
                cInvType   WITH "0001",;
                Typ        WITH lcI,;
                cItmMask   WITH STYLE.Style,;
                MfgCode    WITH '*'+lcI ,;
                Uom        WITH 'EAC',;
                UntCost    WITH EVALUATE(lcPOSLN+'.nFCost'+lcI),;
                nBomTotQty WITH 1,;
                TotCost    WITH EVALUATE(lcPOSLN+'.nFCost'+lcI),;
                CCatgTyp   WITH lcIType&lcI
       *B607915,1 MMT 12/28/2006 bug of error in Po screen while save[Start]
       * REPLACE cItmMajor  WITH STYLE.cStyMajor,;
                cInvType   WITH "0001",;
                Typ        WITH lcI,;
                cItmMask   WITH STYLE.Style,;
                MfgCode    WITH '*'+lcI ,;
                UntCost    WITH EVALUATE(lcPOSLN+'.nFCost'+lcI),;
                nBomTotQty WITH 1,;
                TotCost    WITH EVALUATE(lcPOSLN+'.nFCost'+lcI),;
                CCatgTyp   WITH lcIType&lcI

        REPLACE cItmMajor  WITH STYLE.cStyMajor,;
                cInvType   WITH "0001",;
                Typ        WITH lcI,;
                cItmMask   WITH STYLE.Style,;
                MfgCode    WITH '*'+lcI ,;
                UntCost    WITH EVALUATE(lcPOSLN+'.nFCost'+lcI),;
                nBomTotQty WITH 1,;
                TotCost    WITH EVALUATE(lcPOSLN+'.nFCost'+lcI),;
                CCatgTyp   WITH lcIType&lcI,;
                CinvTypC   WITH IIF(lcIType&lcI='F' OR (lcIType&lcI='T' AND llTINVT),'0002','') ,;
                trim_invt  WITH IIF(lcIType&lcI='T',llTINVT,.F.)
        *B607915,1 MMT 12/28/2006 bug of error in Po screen while save[End]
        *--Get Relation for Default UOM-Code
        *B609572,1 MMT 04/19/2011 Fix bug of wrong Costing elements desc. in case of detail cost No Styles[Start]
        REPLACE Desc WITH lcCISLBL&lcI 
		*B609572,1 MMT 04/19/2011 Fix bug of wrong Costing elements desc. in case of detail cost No Styles[End]
        LOCAL lcRelCode
        lcRelCode = ''
        
        =gfGetUOMData(@lcRelCode, '', '', 1, .F.)
        
        REPLACE cUOMCode WITH lcRelCode
        *E038220,1 WSH [End]
        *E038150,1 AMH [End]
        *N000587,1 WAM 12/01/2007 Get foreign and equivalent cost from PO cost sheet lines
         REPLACE cCurrCode WITH oAriaApplication.BaseCurrency ,; 
                 nCUrrUnit WITH 1 ,; 
                 nExrate   WITH 1
         *N000587,1 WAM 12/01/2007 (End)
      ENDIF
      *N119813,1 AMH [End]
      
    ENDFOR
  ENDIF
ENDIF
*! B609253,1 MMT 05/17/2010 Release variables from gfSheetItem Function after usage[Start]
RELEASE lnI,lcDyeOpr,lcI,lcCUrrCode,lnCurrUnit ,lnExRate   ,lcCostStat,lcRelCode,;
		llTINVT,lcIType2,lcIType3,lcIType4,lcIType5,lcIType6,lcIType7
*! B609253,1 MMT 05/17/2010 Release variables from gfSheetItem Function after usage[End]
RETURN

*****************************************************************************
* PROG: FDYEBROW
* DESC: PROC TO BROWSE THE Dyelots for specific fabric,color 
* using the new browse
*****************************************************************************
PROCEDURE FDYEBROW
PARAMETER XFAB,XCLR,XDYELOT,llRetAlias, lcFromWare 
PRIVATE lcBrFields,lnCurAlias,llFound,laData

DECLARE laData[2]  && array to get values from browse
STORE '' TO laData
llWasSel   = .T.
llBrowse   = IIF(TYPE('llBrowse')='U',.T.,llBrowse) && variable to determine forcing browse or not
lcBrFields = [Dyelot:H="Dyelot #",Onhand:H="On Hand",Usage]

lnCurAlias = SELECT()
IF !USED('FABDYE')
  =gfOpenFile(oAriaApplication.DataDir+'FABDYE','FABDYE','SH')
ELSE
  SELECT FABDYE
  lcFDyOrder = TAG()
  SET ORDER TO TAG FabDye
ENDIF

llFound = .F.
IF SEEK(xFab+xClr+IIF( TYPE('lcFromWare') $ 'UL','',lcFromWare) )
  IF TYPE('lcFromWare')$'UL'
    LOCATE REST WHILE Fabric + Color =  xFab + xClr FOR !EMPTY(DyeLot)
  ELSE
    LOCATE REST WHILE Fabric + Color + cWareCode = xFab + xClr + lcFromWare FOR !EMPTY(DyeLot)
  ENDIF
  llFound = FOUND()
ENDIF  

IF llFound
  lnSoftSeek=RECNO(0)
  IF lnSoftSeek<>0 
    GO lnSoftSeek
  ELSE
    GO TOP
  ENDIF     

  llWasSel = ARIABROW([xFab+xClr+IIF(TYPE('lcFromWare')$ 'UL','',lcFromWare) FOR !EMPTY(DYELOT)],;
             "Item/Color "+ALLTRIM(xFab)+"/"+ALLTRIM(xClr)+" Dyelots",;
             gnBrHSRow1, gnBrHSCol1, gnBrHSRow2, gnBrHSCol2,"","",;
             "FABDYE.Dyelot","laData")
  XDYELOT  = IIF(llWasSel, laData[1], SPACE(10))
ELSE
  XDYELOT  = SPACE(10)
  LOCAL lcDilMesg
  lcDilMesg = 'There are no dyelots for item/color '+ALLTRIM(xFab)+'/'+ALLTRIM(xClr)+;
            IIF(TYPE('lcFromWare')$'UL',' on the file.',' for warehouse '+ALLTRIM(lcFromWare)+'.')
  =MESSAGEBOX(lcDilMesg,64,_Screen.Caption)
ENDIF  

IF !EMPTY(lcFDyOrder)
  SET ORDER TO TAG (lcFDyOrder)
ELSE
  SET ORDER TO  
ENDIF  
IF llRetAlias
  SELECT (lnCurAlias)
ENDIF  
RETURN llWasSel


*****************************************************************************
*! PROG: SDYEBROW
*! DESC: PROGRAM TO BROWSE THE STYLE DYELOTS TO SELECT ONE
*! PARA: STYLE/COLOR AND DYELOT
*****************************************************************************
PROCEDURE SDYEBROW
*N119812,1 HBG 16/11/2003 Add new parameter wit .T. or .F. to check if use style configuration [Begin]
*PARAMETER XSTYLE,XDYELOT,llRetAlias,lcFromWare,llIncAvail,llTop,lcBrwAlias ,lcIndTag 
PARAMETER XSTYLE,XDYELOT,llRetAlias,lcFromWare,llIncAvail,llTop,lcBrwAlias ,lcIndTag , llUseConfg
*N119812,1 [End]
PRIVATE lcBrFields,lcAlias,lcFromWare,lnStyOrd,lnAlias,laData,lcBrwAlias,lcDataDir,lcIndTag

IF TYPE("lcBrwAlias") $ "UL"
  lcBrwAlias = "STYDYE"
  lcDataDir  = oAriaApplication.DataDir
ELSE
  lcDataDir  = oAriaApplication.WorkDir
ENDIF
lcIndTag = IIF(TYPE("lcIndTag") $ "UL","STYDYE",lcIndTag)

DECLARE laData[3] && array to get values from browse
laData     = ' '
llBrowse   = IIF(TYPE('llBrowse')='U',.F.,llBrowse) && variable to determine forcing browse or not

PRIVATE lnBrHSRow1, lnBrHSCol1, lnBrHSRow2, lnBrHSCol2
IF llTop
  lnBrHSRow1 = gnBrFSRow1
  lnBrHSCol1 = gnBrFSCol1
  lnBrHSRow2 = gnBrHSRow1
  lnBrHSCol2 = gnBrHSCol1
ELSE
  lnBrHSRow1 = gnBrHSRow1
  lnBrHSCol1 = gnBrHSCol1
  lnBrHSRow2 = gnBrHSRow2
  lnBrHSCol2 = gnBrHSCol2
ENDIF
xStyle     = PADR(xStyle, 19)
lcFromWare = IIF(TYPE('lcFromWare') $ 'UL' .OR. EMPTY(lcFromWare),'', lcFromWare)

*N119812,1 HBG 16/11/2003 If use style configuration , Change Browse title and Field name [Begin]
*lcTitle    = "Style "+ALLTRIM(xStyle)+" Dyelots"
*lcBrFields = "DYELOT:R :H='Dyelot #':10,"
lcTitle    = "Style "+ALLTRIM(xStyle)+IIF(llUseConfg,LANG_ARIA_Config,LANG_ARIA_Dyelot)
lcHeader = IIF(llUseConfg,LANG_ARIA_ConfigNum,LANG_ARIA_DyelotNum)
lcBrFields = "DYELOT:R :H='"+lcHeader+"':17,"
*N119812,1 [End]


IF llIncAvail
  *N119812,1 HBG 16/11/2003 Change all string s to be variabels in Header file [Begin]
  *!*	  lcBrFields = lcBrFields +; 
  *!*		         "A1=STK1-ALO1  :R :H='Avl1':6,"+;	         	           	         
  *!*	             "A2=STK2-ALO2  :R :H='Avl2':6,"+;
  *!*		         "A3=STK3-ALO3  :R :H='Avl3':6,"+;
  *!*		         "A4=STK4-ALO4  :R :H='Avl4':6,"+;
  *!*		         "A5=STK5-ALO5  :R :H='Avl5':6,"+;
  *!*		         "A6=STK6-ALO6  :R :H='Avl6':6,"+;
  *!*		         "A7=STK7-ALO7  :R :H='Avl7':6,"+;
  *!*		         "A8=STK8-ALO8  :R :H='Avl8':6,"+;
  *!*		         "A9=TOTSTK-TOTALO :R :H='TotAvl':7,"
  *!*	  lcBrFields = lcBrFields +; 
  *!*		         "ALO1  :R :H='Alo1':6,"+;
  *!*		         "ALO2  :R :H='Alo2':6,"+;
  *!*		         "ALO3  :R :H='Alo3':6,"+;
  *!*		         "ALO4  :R :H='Alo4':6,"+;
  *!*		         "ALO5  :R :H='Alo5':6,"+;
  *!*		         "ALO6  :R :H='Alo6':6,"+;
  *!*		         "ALO7  :R :H='Alo7':6,"+;
  *!*		         "ALO8  :R :H='Alo8':6,"+;
  *!*	             "TOTALO:R :H='TotAlo':7,"+;
  *!*		         "STK1  :R :H='Stk1':6,"+;	         	           	         
  *!*	             "STK2  :R :H='Stk2':6,"+;
  *!*		         "STK3  :R :H='Stk3':6,"+;
  *!*		         "STK4  :R :H='Stk4':6,"+;
  *!*		         "STK5  :R :H='Stk5':6,"+;
  *!*		         "STK6  :R :H='Stk6':6,"+;
  *!*		         "STK7  :R :H='Stk7':6,"+;
  *!*		         "STK8  :R :H='Stk8':6,"+;
  *!*		         "TOTSTK:R :H='TotStk':7"
  lcBrFields = lcBrFields +; 
	         "A1=STK1-ALO1  :R :H='"+LANG_ARIA_Avl1+"':6,"+;	         	           	         
             "A2=STK2-ALO2  :R :H='"+LANG_ARIA_Avl2+"':6,"+;
	         "A3=STK3-ALO3  :R :H='"+LANG_ARIA_Avl3+"':6,"+;
	         "A4=STK4-ALO4  :R :H='"+LANG_ARIA_Avl4+"':6,"+;
	         "A5=STK5-ALO5  :R :H='"+LANG_ARIA_Avl5+"':6,"+;
	         "A6=STK6-ALO6  :R :H='"+LANG_ARIA_Avl6+"':6,"+;
	         "A7=STK7-ALO7  :R :H='"+LANG_ARIA_Avl7+"':6,"+;
	         "A8=STK8-ALO8  :R :H='"+LANG_ARIA_Avl8+"':6,"+;
	         "A9=TOTSTK-TOTALO :R :H='"+LANG_ARIA_TotAvl+"':7,"
  lcBrFields = lcBrFields +; 
	         "ALO1  :R :H='"+LANG_ARIA_Alo1+"':6,"+;
	         "ALO2  :R :H='"+LANG_ARIA_Alo2+"':6,"+;
	         "ALO3  :R :H='"+LANG_ARIA_Alo3+"':6,"+;
	         "ALO4  :R :H='"+LANG_ARIA_Alo4+"':6,"+;
	         "ALO5  :R :H='"+LANG_ARIA_Alo5+"':6,"+;
	         "ALO6  :R :H='"+LANG_ARIA_Alo6+"':6,"+;
	         "ALO7  :R :H='"+LANG_ARIA_Alo7+"':6,"+;
	         "ALO8  :R :H='"+LANG_ARIA_Alo8+"':6,"+;
             "TOTALO:R :H='"+LANG_ARIA_TotAlo+"':7,"+;
	         "STK1  :R :H='"+LANG_ARIA_Stk1+"':6,"+;	         	           	         
             "STK2  :R :H='"+LANG_ARIA_Stk2+"':6,"+;
	         "STK3  :R :H='"+LANG_ARIA_Stk3+"':6,"+;
	         "STK4  :R :H='"+LANG_ARIA_Stk4+"':6,"+;
	         "STK5  :R :H='"+LANG_ARIA_Stk5+"':6,"+;
	         "STK6  :R :H='"+LANG_ARIA_Stk6+"':6,"+;
	         "STK7  :R :H='"+LANG_ARIA_Stk7+"':6,"+;
	         "STK8  :R :H='"+LANG_ARIA_Stk8+"':6,"+;
	         "TOTSTK:R :H='"+LANG_ARIA_TotStk+"':7"
  *N119812,1 [End]
ELSE
  *N119812,1 HBG 16/11/2003 Change all string s to be variabels in Header file [Begin]
  *!*	  lcBrFields = lcBrFields +; 
  *!*		         "STK1  :R :H='Stk1':6,"+;	         	           	         
  *!*	             "STK2  :R :H='Stk2':6,"+;
  *!*		         "STK3  :R :H='Stk3':6,"+;
  *!*		         "STK4  :R :H='Stk4':6,"+;
  *!*		         "STK5  :R :H='Stk5':6,"+;
  *!*		         "STK6  :R :H='Stk6':6,"+;
  *!*		         "STK7  :R :H='Stk7':6,"+;
  *!*		         "STK8  :R :H='Stk8':6,"+;
  *!*		         "TOTSTK:R :H='TotStk':7,"+;
  *!*		         "ALO1  :R :H='Alo1':6,"+;
  *!*		         "ALO2  :R :H='Alo2':6,"+;
  *!*		         "ALO3  :R :H='Alo3':6,"+;
  *!*		         "ALO4  :R :H='Alo4':6,"+;
  *!*		         "ALO5  :R :H='Alo5':6,"+;
  *!*		         "ALO6  :R :H='Alo6':6,"+;
  *!*		         "ALO7  :R :H='Alo7':6,"+;
  *!*		         "ALO8  :R :H='Alo8':6,"+;
  *!*	             "TOTALO:R :H='TotAlo':7"
  lcBrFields = lcBrFields +; 
	         "STK1  :R :H='"+LANG_ARIA_Stk1+"':6,"+;	         	           	         
             "STK2  :R :H='"+LANG_ARIA_Stk2+"':6,"+;
	         "STK3  :R :H='"+LANG_ARIA_Stk3+"':6,"+;
	         "STK4  :R :H='"+LANG_ARIA_Stk4+"':6,"+;
	         "STK5  :R :H='"+LANG_ARIA_Stk5+"':6,"+;
	         "STK6  :R :H='"+LANG_ARIA_Stk6+"':6,"+;
	         "STK7  :R :H='"+LANG_ARIA_Stk7+"':6,"+;
	         "STK8  :R :H='"+LANG_ARIA_Stk8+"':6,"+;
	         "TOTSTK:R :H='"+LANG_ARIA_TotStk+"':7,"+;
	         "ALO1  :R :H='"+LANG_ARIA_Alo1+"':6,"+;
	         "ALO2  :R :H='"+LANG_ARIA_Alo2+"':6,"+;
	         "ALO3  :R :H='"+LANG_ARIA_Alo3+"':6,"+;
	         "ALO4  :R :H='"+LANG_ARIA_Alo4+"':6,"+;
	         "ALO5  :R :H='"+LANG_ARIA_Alo5+"':6,"+;
	         "ALO6  :R :H='"+LANG_ARIA_Alo6+"':6,"+;
	         "ALO7  :R :H='"+LANG_ARIA_Alo7+"':6,"+;
	         "ALO8  :R :H='"+LANG_ARIA_Alo8+"':6,"+;
             "TOTALO:R :H='"+LANG_ARIA_TotAlo+"':7"
  *N119812,1 [End]
ENDIF

lnAlias    = SELECT()
llOpenFile = gfOpenFile(lcDataDir+lcBrwAlias,lcDataDir+lcIndTag,'SH')

SELECT (lcBrwAlias)
lnStyOrd   = VAL(SYS(21))
SET ORDER TO TAG (lcIndTag)
LOCATE 
SEEK XSTYLE + lcFromWare
LOCATE REST FOR !EMPTY(DYELOT) WHILE STYLE + cWareCode = XSTYLE + lcFromWare
IF !FOUND()
  *N119812,1 HBG 16/11/2003 
  *-- Change all string s to be variabels in Header file [Begin]
  *!*	  lcTmpStr = ALLTRIM(xStyle) + IIF(EMPTY(lcFromWare) , ' on the file' , ' in location ' + ALLTRIM(lcFromWare))
  lcTmpStr = ALLTRIM(xStyle) + IIF(EMPTY(lcFromWare) , LANG_ARIA_MsgVar1 , LANG_ARIA_MsgVar2 + ALLTRIM(lcFromWare))
  *-- If use style configuration , Change the message [Begin]
  IF llUseConfg
    =gfModalGen("INM00407B00000" , "DIALOG" , lcTmpStr)
  ELSE
  *N119812,1 [End]
    =gfModalGen("INM00277B00000" , "DIALOG" , lcTmpStr)
  *N119812,1 HBG 16/11/2003 If use style configuration , Change the message [Begin]  
  ENDIF
  *N119812,1 [End]  
  llWasSel = .F.
ELSE
  llWasSel= ARIABROW('XSTYLE + lcFromWare'+[FOR !EMPTY(DYELOT)],lcTitle,;
            lnBrHSRow1, lnBrHSCol1, lnBrHSRow2, lnBrHSCol2,'','',"Style,Dyelot","laData")
  
  IF llWasSel
    XSTYLE = laData[1]
    XDYELOT= laData[2]
  ELSE
    XDYELOT= SPACE(10)
  ENDIF  
ENDIF

SET ORDER TO lnStyOrd
IF llOpenFile .AND. USED(lcBrwAlias)
  USE IN (lcBrwAlias)
ENDIF   
SELECT (lnAlias)

RETURN llWasSel

*!**************************************************************************
*! PROG: STYBROW.PRG
*! program to browse through the style file by using the new browse
*! written by Hesham El_Sheltawi 15/02/1995
*! NOTE: THIS PROGRAM ASSUMES THAT THE STYLE FILE HAS BEEN OPENED
*!       IF WE WANT TO DISPLAY NO COLORS THEN XCLR = '*'
*!       IF WE WANT TO DISPLAY ALL SEASONS XSEASON = '*'
*!**************************************************************************
*!**************************************************************************
*! Modification:
*! B038623,1 MAH 10/12/2004 Fix preference problems.
*!*****************************************************************************************
PROCEDURE STYBROW

*! B608684,1 MMT 09/10/2008 call gfStyBrw when browse style file in option grid[Start]      
*PARAMETERS XSTYLE,XCLR,XSEASON,llRetAlias,lcMajorOrNon
PARAMETERS XSTYLE,XCLR,XSEASON,llRetAlias,lcMajorOrNon,lcCursor,lcSelFld,lcValidFun 
*! B608684,1 MMT 09/10/2008 call gfStyBrw when browse style file in option grid[End]      
        
PRIVATE lcBrFields,lnCurAlias, lcStyle,laData ,lcStyOrder ,lcStyTtl ,lcTitle ,;
         lcXMjr ,lcXNMjr ,lcClrTtl ,lcStyOrder,lcMajorOrNon, lnMajLen,XSTYLE,XCLR,XSEASON

DECLARE laData[3] && array to get values from browse
STORE '' TO laData,lcScope
llBrowse = IIF(TYPE('llBrowse')='U',.T.,llBrowse) && variable to determine forcing browse or not

IF !USED('Codes')
  lcCurAlias = ALIAS()
  =gfOpenFile(oAriaApplication.Datadir+'Codes','Codes','SH')
  SELECT(lcCurAlias)
ENDIF

lcMajorOrNon = IIF(TYPE('lcMajorOrNon')$'UL',"M",lcMajorOrNon)
lcStyTtl = IIF(lcMajorOrNon='M',gfItemMask('HM'),gfItemMask('HN'))
lcTitle = IIF(lcMajorOrNon='M',gfItemMask('HM'),IIF(lcMajorOrNon="N",gfItemMask('HN'),gfItemMask('HI')))
lcXMjr  = gfItemMask('HM')
lcXNMjr = gfItemMask('HN')

lcClrTtl = lcXMjr +'/'+ lcXNMjr
lnMajLen = LEN(gfItemMask('PM'))

*-- Include the .H file
#INCLUDE R:\ARIA4XP\PRGS\SY\STYBROW.H

IF lcMajorOrNon $ 'NI'

    *E124065,1 AMH Add color name to style and material browse [Start]
    *lcBrFields = [STYLE :30 :H=ALLTRIM(lcClrTtl),DESC :20 :H="]+LANG_DESC+[",DESC1 :35 :H="]+LANG_LDESC+[",]+;
                 [lcSesDesc=gfCodDes(Season,'SEASON'):20 :H="]+LANG_SEASON+[",lcDivDesc=gfCodDes(cdivision,'CDIVISION') :20:H="]+LANG_DIV+[",]
    lcBrFields = [STYLE :30 :H=ALLTRIM(lcClrTtl),]+;
                 [lcColor=gfCodDes(SUBSTR(STYLE,lnClrStrt,lnClrWidth),'COLOR'):20 :H="]+LANG_COLOR+;
                 [",DESC :20 :H="]+LANG_DESC+[",DESC1 :35 :H="]+LANG_LDESC+[",]+;
                 [lcSesDesc=gfCodDes(Season,'SEASON'):20 :H="]+LANG_SEASON+;
                 [",lcDivDesc=gfCodDes(cdivision,'CDIVISION') :20:H="]+LANG_DIV+[",]
    *E124065,1 AMH [End]

    lcBrFields = lcBrFields+;
                 [pricea :10:H="]+LANG_PRICEA+[" , PRICEB :10:H="]+LANG_PRICEB+[",PRICEC :10:H="]+LANG_PRICEC+[",]
    
    lcBrFields = lcBrFields+;
                 [totWip:12:h="]+LANG_WIP+[",totstk:12:h="]+LANG_STOCK+[",totord:12:h="]+LANG_ORDER+[",]

    lcBrFields = lcBrFields+;
                 [OTS=(TOTWIP+TOTSTK-TOTORD):12:H="]+LANG_OTS+[",Fabric:15:h="]+LANG_FABRIC+[",]

    lcBrFields = lcBrFields+;
                 [CSTYGRADE :H="]+LANG_GRADE+[", lcRoyDesc=gfCodDes(ROYALTY,'ROYALTY') :20 :H="]+LANG_ROYAL+[" , PATTERN :H="]+LANG_PATRN+[", STATUS :H="]+LANG_STATUS+[",]

    lcBrFields = lcBrFields + ;
                 [SCALE :H="]+LANG_SCALE+[", PREPAK :H="]+LANG_PREPAK+[", CBUYPREPK :H="]+LANG_BPREPK+[", QTY_CTN :H="]+LANG_QTYCRT+[", COMMISSION :H="]+LANG_COMM+[", LINK_CODE :H="]+LANG_LNKCD+[",]+;
                 [lcMaked = IIF(MAKE,'Y','N') :H="]+LANG_MAKE+[", NMCOST1 :H="]+LANG_MCST1+[" , NMCOST2 :H="]+LANG_MCST2+[", NMCOST3 :H="]+LANG_MCST3+[", NMCOST4 :H="]+LANG_MCST4+[",NMCOST5 :H="]+LANG_MCST5+[",]

    lcBrFields = lcBrFields + ;
                 [NICOST1 :H="]+LANG_ICST1+[", NICOST2 :H="]+LANG_ICST2+[", NICOST3 :H="]+LANG_ICST3+[", NICOST4 :H="]+LANG_ICST4+[", NICOST5 :H="]+LANG_ICST5+[",]

    lcBrFields = lcBrFields+;
                 [NPRCOST2 :H="]+LANG_PCST2+[",NPRCOST3 :H="]+LANG_PCST3+[",NPRCOST4 :H="]+LANG_PCST4+[",NPRCOST5 :H="]+LANG_PCST5+[",TOTCOST :H="]+LANG_TOTCST+[",]+;
                 [AVE_COST :H="]+LANG_AVECST+[",NSTKVAL :H="]+LANG_STKVAL+[",SOLDOUT :H="]+LANG_SOLDDT+[",START :H="]+LANG_STRTDT+[",LOCATION :H="]+LANG_DBIN+[",LINVSTY :H="]+LANG_INVSTY+[",]

    lcBrFields = lcBrFields+;
                 [MARKA :H="]+LANG_MARKA+[",MARKB:H="]+LANG_MARKB+[",MARKC :H="]+LANG_MARKC+[",]+;
                 [CCONSINFO1 :H="]+LANG_CONSI1+[",CCONSINFO2 :H="]+LANG_CONSI2+["]
ELSE
    
    lcBrFields = [CSTYMAJOR:30:H=ALLTRIM(lcStyTtl),STYLE_A.DESC:20:H="]+LANG_DESC+[",STYLE_A.DESC1:35:H="]+LANG_LDESC+[",]+;
                 [lcSesDesc=gfCodDes(STYLE_A.Season,'SEASON'):20:H="]+LANG_SEASON+[",lcDivDesc=gfCodDes(STYLE_A.cdivision,'CDIVISION'):20:H="]+LANG_DIV+[",]

    lcBrFields = lcBrFields+;
                 [STYLE_A.pricea:10:h="]+LANG_PRICEA+[",STYLE_A.PRICEB:10:h="]+LANG_PRICEB+[",STYLE_A.PRICEC:10:h="]+LANG_PRICEC+[",]
    
    lcBrFields = lcBrFields+;
                 [lnTotWIP=lfSumSeasn(PADR(ALLTRIM(STYLE_A.CSTYMAJOR),lnMajLen),'TotWip'):12:h="]+LANG_WIP+[",]+;
                 [lnTotStk=lfSumSeasn(PADR(ALLTRIM(STYLE_A.CSTYMAJOR),lnMajLen),'totstk'):12:h="]+LANG_STOCK+[",]+;
                 [lnTotORD=lfSumSeasn(PADR(ALLTRIM(STYLE_A.CSTYMAJOR),lnMajLen),'totord'):12:h="]+LANG_ORDER+[",]+;
                 [lnOts   =lfSum_OTS(PADR(ALLTRIM(STYLE_A.CSTYMAJOR),lnMajLen)):12:H="]+LANG_OTS+[",]

    lcBrFields = lcBrFields+;
                 [Style_A.Fabric:12:h="]+LANG_FABRIC+[",Style_A.CSTYGRADE:H="]+LANG_GRADE+[", lcRoyDesc=gfCodDes(STYLE_A.ROYALTY,'ROYALTY') :15 :H="]+LANG_ROYAL+[" , STYLE_A.PATTERN :H="]+LANG_PATRN+[", STYLE_A.STATUS :H="]+LANG_STATUS+[",]

    lcBrFields = lcBrFields + ;
                 [Style_A.SCALE :H="]+LANG_SCALE+[", Style_A.PREPAK :H="]+LANG_PREPAK+[", Style_A.CBUYPREPK :H="]+LANG_BPREPK+[", Style_A.QTY_CTN :H="]+LANG_QTYCRT+[", Style_A.COMMISSION :H="]+LANG_COMM+[", Style_A.LINK_CODE :H="]+LANG_LNKCD+[",]+;
                 [lcMaked = IIF(Style_A.MAKE,'Y','N') :H="]+LANG_MAKE+[", Style_A.NMCOST1 :H="]+LANG_MCST1+[" , Style_A.NMCOST2 :H="]+LANG_MCST2+[", Style_A.NMCOST3 :H="]+LANG_MCST3+[", Style_A.NMCOST4 :H="]+LANG_MCST4+[",Style_A.NMCOST5 :H="]+LANG_MCST5+[",]

    lcBrFields = lcBrFields + ;
                 [Style_A.NICOST1 :H="]+LANG_ICST1+[", Style_A.NICOST2 :H="]+LANG_ICST2+[", Style_A.NICOST3 :H="]+LANG_ICST3+[", Style_A.NICOST4 :H="]+LANG_ICST4+[", Style_A.NICOST5 :H="]+LANG_ICST5+[",]

    lcBrFields = lcBrFields+;
                 [STYLE_A.NPRCOST2 :H="]+LANG_PCST2+[",STYLE_A.NPRCOST3 :H="]+LANG_PCST3+[",STYLE_A.NPRCOST4 :H="]+LANG_PCST4+[",STYLE_A.NPRCOST5 :H="]+LANG_PCST5+[",STYLE_A.TOTCOST :H="]+LANG_TOTCST+[",]+;
                 [lnAvegCost=lfCalAvCst() :H="]+LANG_AVECST+[",STYLE_A.SOLDOUT :H="]+LANG_SOLDDT+[",STYLE_A.START :H="]+LANG_STRTDT+[",STYLE_A.LOCATION :H="]+LANG_DBIN+[",STYLE_A.LINVSTY :H="]+LANG_INVSTY+[",]

    lcBrFields = lcBrFields+;
                 [STYLE_A.MARKA :H="]+LANG_MARKA+[",STYLE_A.MARKB:H="]+LANG_MARKB+[",STYLE_A.MARKC :H="]+LANG_MARKC+[",]+;
                 [STYLE_A.CCONSINFO1 :H="]+LANG_CONSI1+[",STYLE_A.CCONSINFO2 :H="]+LANG_CONSI2+["]

    
ENDIF


lnCurAlias = SELECT()
SELECT 0

PRIVATE llSTYLE_A
llSTYLE_A = .F.
IF !USED('STYLE_A')
  llSTYLE_A = .T.
  USE (oAriaApplication.Datadir+'STYLE') AGAIN ALIAS STYLE_A ORDER TAG STYLE
ENDIF

SELECT STYLE
lcStyOrder = TAG()
SET ORDER TO TAG CSTYLE
DO CASE
  CASE '*' $ XCLR AND '*' $ XSEASON
    lcBrFields = [STYLE :30 :H=ALLTRIM(lcClrTtl),DESC :45:H="]+LANG_DESC+[",]+;
                 [pricea :H="]+LANG_PRICEA+[",totWip :14 :H="]+LANG_WIP+[",totstk :14 :H="]+LANG_STOCK+[",]+;
                 [totord :14 :H="]+LANG_ORDER+[",Fabric :19 :H="]+LANG_FABRIC+["]

  CASE '*' $ XCLR
    lcBrFields = [STYLE :30 :H=ALLTRIM(lcClrTtl),DESC :45:H="]+LANG_DESC+[",]+;
                 [pricea :H="]+LANG_PRICEA+[",totWip :14 :H="]+LANG_WIP+[",totstk :14 :H="]+LANG_STOCK+[",]+;
                 [totord :14 :H="]+LANG_ORDER+[",Fabric :19 :H="]+LANG_FABRIC+["]
    
    *B128950,1 AMH Fix bug of variable not found when browse [Start]
    *lcScope = [=gfvSeason]
    lcScope = [=gfvSeason()]
    *B128950,1 AMH [End]
    
  OTHERWISE  
    IF !('*' $ XSEASON)
      
      *B128950,1 AMH Fix bug of variable not found when browse [Start]
      *lcScope = [=gfvSeason]
      lcScope = [=gfvSeason()]
      *B128950,1 AMH [End]
      
    ENDIF  
ENDCASE

*! B608684,1 MMT 09/10/2008 call gfStyBrw when browse style file in option grid[Start]      
lcScope = IIF(TYPE('lcValidFun')='C' and !EMPTY(lcValidFun),lcValidFun ,lcScope)
*! B608684,1 MMT 09/10/2008 call gfStyBrw when browse style file in option grid[End]      

SELECT STYLE

IF lcMajorOrNon = 'M'
  SET ORDER TO CSTYLE
ELSE
  SET ORDER TO STYLE
ENDIF
SET RELATION TO PADR(STYLE.CSTYMAJOR,lnMajLen) INTO STYLE_A ADDI
IF lcMajorOrNon = 'M'
  lcStyle = [XSTYLE]
ELSE
  IF lcMajorOrNon $ 'N'
    SELECT STYLE
    lcStyle = [XSTYLE]
  ENDIF
ENDIF

IF !SEEK(xStyle)
 lnSoftSeek = IIF(SEEK(ALLT(xStyle),'STYLE'),RECNO(),RECNO(0))
 IF  lnSoftSeek > 0 
   GO lnSoftSeek
 ELSE
   GO TOP
 ENDIF
ENDIF  

*! B038623,1 MAH 10/12/2004 [BEGIN]
*-- IF TYPE('lcStyle')='C'
*--   IF lcMajorOrNon = 'N'
*--     llWasSel=ARIABROW(lcStyle,lcTitle,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,lcScope,"","STYLE,STYLE_A.SEASON","laData")
*--   ELSE
*--     llWasSel=ARIABROW('',lcTitle,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,lcScope,"","STYLE,STYLE_A.SEASON","laData")
*--   ENDIF
*-- ELSE
*--   llWasSel=ARIABROW('',lcTitle,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,lcScope,"","STYLE,STYLE_A.SEASON","laData")
*-- ENDIF  
IF TYPE('lcStyle') = 'C'
  IF lcMajorOrNon = 'N'
    *! B608684,1 MMT 09/10/2008 call gfStyBrw when browse style file in option grid[Start]      
    *llWasSel = ARIABROW(lcStyle, lcTitle, gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2, lcScope, "", ;
                        "STYLE,STYLE_A.SEASON", "laData", ;
                        .F., .F., .F., .F., .F., .F., .F., .F., .F., 'A1')
    llWasSel = ARIABROW(lcStyle, lcTitle, gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2, lcScope, "", ;
                        "STYLE,STYLE_A.SEASON", "laData", ;
                        .F., .F., .F., .F., .F., .F., .F., .F., .F., 'A1',lcCursor,lcSelFld)
    *! B608684,1 MMT 09/10/2008 call gfStyBrw when browse style file in option grid[End]                              
                        
  ELSE
    *! B608684,1 MMT 09/10/2008 call gfStyBrw when browse style file in option grid[Start]      
    *llWasSel = ARIABROW('', lcTitle, gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2, lcScope, "", ;
                        "STYLE,STYLE_A.SEASON", "laData", .F., .F., .F., .F., .F., .F., .F., .F., .F., 'A2')
    llWasSel = ARIABROW('', lcTitle, gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2, lcScope, "", ;
                        "STYLE,STYLE_A.SEASON", "laData", .F., .F., .F., .F., .F., .F., .F., .F., .F., 'A2',lcCursor,lcSelFld)
    *! B608684,1 MMT 09/10/2008 call gfStyBrw when browse style file in option grid[ENd]                              
  ENDIF
ELSE
  *! B608684,1 MMT 09/10/2008 call gfStyBrw when browse style file in option grid[Start]      
  *llWasSel = ARIABROW('', lcTitle, gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2, lcScope, "", "STYLE,STYLE_A.SEASON", ;
                      "laData", .F., .F., .F., .F., .F., .F., .F., .F., .F., 'A3')
  llWasSel = ARIABROW('', lcTitle, gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2, lcScope, "", "STYLE,STYLE_A.SEASON", ;
                      "laData", .F., .F., .F., .F., .F., .F., .F., .F., .F., 'A3',lcCursor,lcSelFld)
  *! B608684,1 MMT 09/10/2008 call gfStyBrw when browse style file in option grid[End]                            
ENDIF  
*! B038623,1 MAH 10/12/2004 [END]

IF lcMajorOrNon = 'M'
  SET SKIP TO
  SET RELATION OFF INTO STYLE_A
ENDIF
IF llWasSel
  XSTYLE  = laData[1]
  
  *B128950,1 AMH Fix bug of Out side of range [Start]
  *XCLR    = laData[2]
  XCLR    = SPACE(6)
  *B128950,1 AMH [End]
  
  SET ORDER TO TAG STYLE
  SEEK xStyle
ELSE
  XSTYLE  = SPACE(12)
  XCLR    = SPACE(6)
ENDIF  

IF !EMPTY(lcStyOrder)
  SELECT STYLE
  SET ORDER TO TAG (lcStyOrder)
ELSE
  SET ORDER TO
ENDIF  

IF llSTYLE_A
  USE IN STYLE_A
ENDIF

IF llRetAlias
  SELECT (lnCurAlias)
ENDIF  

*! B608684,1 MMT 09/10/2008 call gfStyBrw when browse style file in option grid[Start]      
IF  (TYPE('lcCursor')= 'C' AND !EMPTY(lcCursor)) AND (TYPE('lcSelFld')= 'C' AND !EMPTY(lcSelFld))
  RETURN llWasSel 
ELSE
*! B608684,1 MMT 09/10/2008 call gfStyBrw when browse style file in option grid[End]      

  RETURN XSTYLE
  
*! B608684,1 MMT 09/10/2008 call gfStyBrw when browse style file in option grid[Start]      
ENDIF 
*! B608684,1 MMT 09/10/2008 call gfStyBrw when browse style file in option grid[End]      

*!**************************************************************************
*! FUNCTION gfvSeason
*! program to validate the style selected by the user in the browse
*! if it use the same season or not 
*! written by Hesham El_Sheltawi 15/02/1995
*!**************************************************************************
FUNCTION gfvSeason

IF STYLE_A.Season = XSEASON  OR ALLTRIM(STYLE_A.Season) = 'Y'
ELSE
   =MESSAGEBOX("You are restricted to styles from season "+XSEASON+"! RETRY")
ENDIF

*!**************************************************************************
*! FUNCTION lfSumSeasn
*! FUNCTION to sum a specific field for the current style in style file
*! written by Hesham El_Sheltawi 15/02/1995
*!**************************************************************************
FUNCTION lfSumSeasn
PARAMETERS lcstyle,lccomp
lnTotcomp = 0
SELECT STYLE_A
SUM &lcCOMP TO lnTotcomp WHILE STYLE=lcstyle
SELECT STYLE
*! B608684,1 MMT 09/10/2008 call gfStyBrw when browse style file in option grid[Start]      
IF BETWEEN(RECNO(),1,RECCOUNT())
*! B608684,1 MMT 09/10/2008 call gfStyBrw when browse style file in option grid[End]      
GO RECNO()
*! B608684,1 MMT 09/10/2008 call gfStyBrw when browse style file in option grid[Start]      
ENDIF 
*! B608684,1 MMT 09/10/2008 call gfStyBrw when browse style file in option grid[End]      
RETURN INT(lnTotcomp)

*!**************************************************************************
*! FUNCTION lfSum_OTS
*! FUNCTION to sum the open to sell quantities.
*! written by Reham Alallamy 04/26/95
*!**************************************************************************
FUNCTION lfSum_OTS
PARAMETERS lcstyle

lnTot_OTS = 0
SELECT STYLE_A
SUM (totWip+totStk-totOrd) TO lnTot_OTS WHILE STYLE=lcstyle
SELECT STYLE
*! B608684,1 MMT 09/10/2008 call gfStyBrw when browse style file in option grid[Start]      
IF BETWEEN(RECNO(),1,RECCOUNT())
*! B608684,1 MMT 09/10/2008 call gfStyBrw when browse style file in option grid[End]      
GO RECNO()
*! B608684,1 MMT 09/10/2008 call gfStyBrw when browse style file in option grid[Start]    
ENDIF   
*! B608684,1 MMT 09/10/2008 call gfStyBrw when browse style file in option grid[End]      
RETURN INT(lnTot_OTS)


*!*************************************************************
*! Name      : lfCalAvCst
*! Developer : Hossam El Etreby [HDM]
*! Date      : 06/06/1999
*! Purpose   : Function to Calculate Average cost for all style colors
*!*************************************************************
*! Returns            : Average cost
*!*************************************************************
FUNCTION lfCalAvCst

PRIVATE lnTotStk, lnTotStVal , lnAvCstVal
STORE 0 TO lnTotStk , lnTotStk , lnAvCstVal

lnTotStk   = lfSumSeasn(PADR(ALLTRIM(STYLE_A.CSTYMAJOR),lnMajLen),'TotStk')
lnTotStVal = lfSumSeasn(PADR(ALLTRIM(STYLE_A.CSTYMAJOR),lnMajLen),'NSTKVAL')
IF lnTotStk > 0
  lnAvCstVal = lnTotStVal / lnTotStk
ELSE
  RETURN STYLE_A.Ave_Cost
ENDIF
RETURN (lnAvCstVal)



*!***************************************************************
* PROG: POSBROW
* DESC: PROGRAM TO BROWSE THE STYLE PURCHASE ORDER
* PARA: THE PO AND VENDOR IS PASSED AS A PARAMETER
*PARAMETER :lcAType='P' for p/o,
*                   'R' for return po
*                   'C' for contract
*--------------------------------------------------------------
*  lcVen - >  will hold the source location insted of vendor.
*!***************************************************************
PROCEDURE POSBROW
PARAMETER lcPO,lcVEN,lcAType

PRIVATE lcBrFields,lcAlias,lcVenCode,laPData
DECLARE laPData[2] && array to get values from browse
laPData=' '
lcTitle  = IIF(lcAType='R','Return Purchase Orders',;
           IIF(lcAType='C','Contracts',IIF(lcAType='D','Dye Purchase Order',;
           IIF(lcAType='A','Adornment Purchase Order','Style Purchase Orders'))))
lcTitle  = IIF(lcAType='N','Inter Location Purchase Orders',lcTitle)
lcPOType = IIF(lcAType='R','Return P/O',IIF(lcAType='C','Contract','P/O '))
lcVenCode=IIF(!EMPTY(lcVen),PADR(lcVen,8)+lcAType,lcAType)

lcAlias  = ALIAS()   && Save the current alias.
IF !(lcAType $ 'NA')
  *--Open the vendor file if not opened.
  =gfOpenFile(oAriaApplication.DataDir+'APVENDOR','Vencode','SH')
  llApLink = (gfGetMemVar('M_ApLink')='Y')
  lcApDir  = ALLTRIM(gfGetMemVar('M_ApLink'))
ENDIF

lcBrFields =   [PO        :R :H=lcPOType+' #':12,]+;
	           [Status    :R :H='S':4,]+;
               [Vendor    :R :H=IIF(lcAType$'NA','Source Loc.','Vendor'):15,]+;
               [lcVnName = IIF(lcAType$'NA',WAREHOUS.cDesc,ApVendor.cVenComp) :R :H='Name':22,]+;
	           [Entered   :R :H='Entered':15,]+;
	           [Complete  :R :H='Complete':15,]+;
	           [nStyOrder :R :H='Tot.Qty.':10,]+;
               [POTotal   :R :H='Amount':15,]+;
               [Receive   :R :H='Receive':10,]+;
               [Open      :R :H='Open':10]

SELECT POSHDR
IF !EMPTY(lcVen)
  SET ORDER TO TAG POSHDRV
  IF !SEEK (lcVenCode)
    =MESSAGEBOX("There are no "+lcPOType+"s for this "+IIF(lcAType='N','location','vendor')+" on the file....!",64,_SCREEN.Caption)
    lcVEN = SPACE(8)
    lcPO  = SPACE(6) 
    GO TOP
    SET ORDER TO TAG POSHDR
    RETURN .F.
  ENDIF
ELSE
  SET ORDER TO TAG POSHDR
  IF !SEEK(lcAType)
    =MESSAGEBOX("There are no "+lcPOType+"s on the file....!",64,_SCREEN.Caption)
    lcVEN = SPACE(8)
    lcPO  = SPACE(6) 
    GO TOP
    RETURN .F.
  ENDIF
ENDIF  

SELECT POSHDR
lcPosRel = SET('RELATION')
IF lcAType $ 'NA'
  SET RELATION TO PADR(Vendor,6) INTO WareHous
ELSE
  SET RELATION TO Vendor INTO APVendor
ENDIF
*--Call Global Browse.
*llWasSel=gfBrows([lcVenCode],"Po,Vendor","laPData",lcTitle)
llWasSel=AriaBrow([lcVenCode],lcTitle,0,0,0,0,'','',"Po,Vendor","laPData",.T.,"POSHDR",.f.)

SET RELATION TO &lcPosRel
IF llWasSel
  lcPO  = laPData[1]
  lcVEN = laPData[2]
ELSE
  lcVEN = SPACE(8)
  lcPO  = SPACE(6) 
ENDIF  

SELECT POSHDR
SET ORDER TO TAG POSHDR
RETURN llWasSel

***********
*!*************************************************************
*! Name      : gfBrowPref
*! Developer : Hesham El-Sheltawi
*! Date      : 1993-1995 
*! Purpose   : Get the programm that called the browse to create the prefrence of the browse
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : Browse fields
*!                      Alias name
*!                      
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : 
*!*************************************************************
FUNCTION gfBrowPref
PARAMETERS lcBrCmdFields,lcAliasName,llCmtPrgTree
PRIVATE lcProgName,lcUsrFile,lcPref,lcFields,lcSelectAlias

*-- User resource file
lcUsrFile   = 'X'+SYS(2007,PADR(oAriaApplication.User_ID,10))
*-- Browse file
lcAliasName = IIF(TYPE('lcAliasName')='C',lcAliasName,'')
lcAliasName = IIF(EMPTY(lcAliasName),IIF(INLIST(LEFT(ALIAS(),1),'X','T'),'',ALIAS()),lcAliasName)

IF EMPTY(lcAliasName) OR !(oAriaApplication.ResourceHome $ SET('RESO',1))
  RETURN "Fields "+lcBrCmdFields
ENDIF
*-- Get calling program name
lcProgName = ''
IF !llCmtPrgTree
  lnCount = 2
  lcProgName = IIF(LEFT(PROG(1),2)='ON','',PROG(1))  
  DO WHILE !('GFBROWS' $ SYS(16,lnCount)) AND !('ARIABROW' $ SYS(16,lnCount)) AND !EMPTY(SYS(16,lnCount))
   lcProgName = lcProgName + IIF(LEFT(PROG(lnCount),2)='ON','',PROG(lnCount))
   IF !('GFBROWS' $ SYS(16,lnCount)) AND !('ARIABROW' $ SYS(16,lnCount))
     lnCount = lnCount +1
   ENDIF  
  ENDDO
  lcProgName = lcProgName + SUBSTR(SYS(16,lnCount),ATC('IN ',SYS(16,lnCount)))
ENDIF
lcPref   = 'W'+SYS(2007,PADR(lcProgName+lcAliasName,340))
lcFields = IIF(EMPTY(lcBrCmdFields),'','FIELDS '+lcBrCmdFields)
lcSelectAlias = SELECT()
IF oAriaApplication.ResourceHome $ SET('RESOURCE',1) AND ;
   SET('RESOURCE')='ON' AND FILE(oAriaApplication.ResourceHome+lcUsrFile+'.DBF')
  lcFields=''
  IF !USED(lcUsrFile)
    SELECT 0
    USE (oAriaApplication.ResourceHome+lcUsrFile) ORDER TAG cUserPref
  ENDIF  
  SELECT (lcUsrFile)
  SET ORDER TO TAG cUserPref
  =SEEK(lcPref)
  IF !FOUND() OR !(ALLTRIM(lcBrCmdFields) == ALLTRIM(mBrowField)) OR ;
                 !(ALLTRIM(mBrowField) == ALLTRIM(lcBrCmdFields))
    IF !FOUND()
      APPEND BLANK
    ELSE
      SELECT 0
      USE (SYS(2005)) AGAIN ORDER TAG 1
      IF SEEK(PADR('PREF'+'W',12)+PADR('WINDBROW',12)+PADR('B'+lcPref,24))
        REPLACE DATA WITH ''
      ENDIF
      USE
      SELECT (lcUsrFile)
    ENDIF
    REPLACE cUserPref  WITH lcPref,;
            mBrowField WITH lcBrCmdFields
    lcFields = IIF(EMPTY(lcBrCmdFields),'','FIELDS '+lcBrCmdFields)
  ENDIF
ENDIF
IF USED(lcUsrFile)
  USE IN (lcUsrFile)
ENDIF
SELECT (lcSelectAlias)
lcPref = 'PREF B'+lcPref+' '
RETURN lcPref+lcFields

*!*************************************************************
*! Name      : gfCodDes 
*! Developer : Malak Hanna Aziz
*! Date      : 1993-1995 
*! Purpose   : Get Code Description
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : lcCodeVal, lcFldName , llChkEdit
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : 
*!*************************************************************
FUNCTION gfCodDes
PARAMETERS lcCodeVal, lcFldName , llChkEdit

*-- MAB Local variables ... BEGIN
LOCAL lnCurrentAlias, lcSavOrder
*-- MAB Local variables ... END

PRIVATE lcReturnVal,lcSavFltr
lcSavFltr = ""

llRetArray = (TYPE("lcCodeVal[1,1]") = "C")
IF !llRetArray AND (TYPE("lcCodeVal") <> "C" OR TYPE("lcFldName") <> "C")
  RETURN ''
ENDIF

IF !llRetArray
  lcCodeVal   = PADR(UPPER(lcCodeVal),6)
  lcFldName   = PADR(UPPER(lcFldName),10)
ENDIF  

lcReturnVal = ""
lnCurrentAlias  = SELECT(0)      && Variable to save the currently selected file.

llUseCodes = .F.
IF !USED("CODES")
  USE (oAriaApplication.DataDir+"Codes") IN 0 AGAIN
  llUseCodes = .T.
ENDIF

SELECT CODES               && Select CODES file
lcSavOrder = SYS(22)       && Save the file order
SET ORDER TO TAG CODES     && Change the order

*-- if pass array of codes.
IF llRetArray
  PRIVATE lnArrLen , lnCodeLen
  lnCodeLen = 6
  lnArrLen  = 0
  FOR lnArrLen = 1 TO ALEN(lcCodeVal,1)
    lcCodeVal[lnArrLen,1] = PADR(UPPER(lcCodeVal[lnArrLen,1]),6)
    lcCodeVal[lnArrLen,2] = PADR(UPPER(lcCodeVal[lnArrLen,2]),10)
    IF EMPTY(lcCodeVal[lnArrLen,1]) .OR. lcCodeVal[lnArrLen,1] = "*"
      lcCurFlt = FILTER()
      lnFltRec = IIF(EOF() .OR. BOF(), 0, RECNO())
      SET FILTER TO

      IF !SEEK(SPACE(1)+LEFT(lcCodeVal[lnArrLen,1],1))
        APPEND BLANK
        REPLACE cFld_Name  WITH IIF(lcCodeVal[lnArrLen,1] = '*','ALL','N/A') ;
                cCode_No   WITH IIF(lcCodeVal[lnArrLen,1] = '*','*','')      ;
                cDiscrep   WITH IIF(lcCodeVal[lnArrLen,1] = '*','All','N/A') ;
                cRltField  WITH 'N'
      ENDIF  
      lcCodeVal[lnArrLen,3] = CODES.cDiscrep
      SET FILTER TO &lcCurFlt.
      IF BETWEEN(lnFltRec,1,RECCOUNT())
        GO lnFltRec
      ENDIF    

    ELSE
  
      IF SEEK('N' + lcCodeVal[lnArrLen,1] + "N" + lcCodeVal[lnArrLen,2])  
        lcCodeVal[lnArrLen,3] = CODES.cDiscrep
      ELSE
        lcCodeVal[lnArrLen,3] = ''       && In case of this code record is deleted
      ENDIF  

    ENDIF

    IF !EMPTY(lcCodeVal[lnArrLen,3]) AND gfIsEdtble(ALLTRIM(lcCodeVal[lnArrLen,2]) , @lnCodeLen)
       lcCodeVal[lnArrLen,3] = PADR(lcCodeVal[lnArrLen,1],lnCodeLen) + '-' + lcCodeVal[lnArrLen,3]
    ENDIF

  ENDFOR

ELSE && Pass one code only

  IF EMPTY(lcCodeVal) .OR. lcCodeVal = "*"
    lcCurFlt = FILTER()
    lnFltRec = IIF(EOF() .OR. BOF(), 0, RECNO())
    SET FILTER TO
    IF !SEEK(SPACE(1)+LEFT(lcCodeVal,1))
      APPEND BLANK
      REPLACE cFld_Name  WITH IIF(lcCodeVal = '*','ALL','N/A') ;
              cCode_No   WITH IIF(lcCodeVal = '*','*','')      ;
              cDiscrep   WITH IIF(lcCodeVal = '*','All','N/A') ;
              cRltField  WITH 'N'
    ENDIF  
    lcReturnVal = CODES.cDiscrep
    SET FILTER TO &lcCurFlt.
    IF BETWEEN(lnFltRec,1,RECCOUNT())
      GO lnFltRec
    ENDIF    
  ELSE
    lcSavFltr = FILTER()
    lnFltRec = IIF(EOF() .OR. BOF(), 0, RECNO())
    SET FILTER TO 

    IF SEEK('N' + lcCodeVal + "N" + lcFldName)  
      lcReturnVal = CODES.cDiscrep
    ELSE
      lcReturnVal = ''       && In case of this code record is deleted
    ENDIF  

    SET FILTER TO &lcSavFltr 
    IF BETWEEN(lnFltRec,1,RECCOUNT())
      GO lnFltRec
    ENDIF    
  ENDIF  

  PRIVATE lnCodeLen
  lnCodeLen = 6
  IF llChkEdit AND !EMPTY(lcReturnVal) AND gfIsEdtble(ALLTRIM(lcFldName) , @lnCodeLen)
    lcReturnVal = PADR(lcCodeVal,lnCodeLen) + '-' + lcReturnVal
  ENDIF

ENDIF

*-- MAB 12/08/2002 - Codes not the selected alias... BEGIN
SELECT CODES
*-- MAB 12/08/2002 - Codes not the selected alias... END

SET ORDER TO &lcSavOrder
*!*  IF llUseCodes
*!*    USE IN Codes
*!*  ENDIF

SELECT (lnCurrentAlias)
RETURN lcReturnVal
*-- end of gfCodDes.



*!**********************************************************************
*! Name      : lfTrnType
*! Date      : 06/02/97
*! Purpose   : Function return the transaction title descreption.
*!**********************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfTrnType
PARAMETERS lcTrType,llStyMake,lcIRType
PRIVATE lcTrnDesc
lcTrnDesc = ""
DO CASE
  CASE lcTrType $ '1I'
	lcTrnDesc='Adjustment        '
  CASE lcTrType ='2'
	lcTrnDesc='Physical Inventory'
  CASE lcTrType ='3'
	lcTrnDesc='Invoice           '
  CASE lcTrType ='4'
	lcTrnDesc='Void Invoice      '
  CASE lcTrType ='5'
	lcTrnDesc='Receive C/T       '
  CASE lcTrType ='6'
    IF llStyMake
      lcTrnDesc= IIF(lcIRType='I','Issue Adornment order','Receive Mfg. Order')
    ELSE
      lcTrnDesc= IIF(lcIRType='I','Issue','Receive')+' P/O       '
    ENDIF
  CASE lcTrType ='7'
	lcTrnDesc='Return Merchandise'
  CASE lcTrType ='8'
	lcTrnDesc='Void Credit Memo  '
  CASE lcTrType ='9'
	lcTrnDesc='Inventory Markdown'
ENDCASE
RETURN lcTrnDesc


*!*************************************************************
*! Name      : gfPrePBrow
*! Developer : MALAK - Malak Hanna
*! Date      : 05/02/1995
*! Purpose   : Function to validate entered prepack value for a 
*!             specific size scale.
*!*************************************************************
*! Calls     : ARIABROW()
*!*************************************************************
*! Passed Parameters  :  Entered prepack
*!*************************************************************
*! Returns            :  .T. --> Valid scale 
*!                       .F. --> Invalid scale
*!*************************************************************
*! Example            :  gfPrePBrow(m.Scale,@m.Prpak)
*!*************************************************************
FUNCTION gfPrePBrow
PARAMETER lcScale,lcPrePack,llBrowse

IF !USED('SCALE')
  =gfOpenFile(oAriaApplication.DataDir+'SCALE','SCALE','SH')
ENDIF
IF llBrowse OR (!EMPTY(lcPrePack) AND !SEEK('P'+lcScale+lcPrePack,'SCALE'))
  IF SEEK('P'+lcScale,'SCALE')
    lnOldAlias = SELECT()
    SELECT SCALE
    DECLARE laValues[1]  && array to get values from browse
    lcFile_Ttl = 'Prepacks'
    lcBrFields = "Prepak :H='Prepack'"
    IF SEEK('S'+lcScale,'SCALE') 
      FOR lnCounter = 1 TO SCALE.Cnt
        lcPos = STR(lnCounter,1)
        lcBrFields =lcBrFields+",PP&lcPos:H='"+Scale.Sz&lcPos+"' :6"
      ENDFOR 
    ENDIF
 
    lcBrFields = lcBrFields + ",ppTot:H='Total' :8"
    =SEEK("P"+lcScale,'Scale')
    =ARIABROW(["P"+lcScale],lcFile_Ttl,gnBrHSRow1, gnBrHSCol1, gnBrHSRow2, gnBrHSCol2,'','','PREPAK','laValues')
    lcPrePack = IIF(EMPTY(laValues[1]),SPACE(1),laValues[1])
    SELECT (lnOldAlias)
  ELSE
    =MESSAGEBOX('There is no prepak for scale '+ lcScale,64,_SCREEN.Caption)
    lcPrePack = SPACE(1)
  ENDIF  
ENDIF  

RETURN !EMPTY(lcPrePack)

*!**************************************************************************
*! PROG: CUSBROWM.PRG
*! program to browse through the Customer file by using the new browse
*! written by Hesham El_Sheltawi 15/02/1995
*! NOTE: THIS PROGRAM ASSUMES THAT THE fabric FILE HAS BEEN OPENED
*! 
*!**************************************************************************
PROCEDURE CUSBROWM
Parameters XACCOUNT,llRetAlias,lcMastStor
IF EMPTY(lcMastStor)
  RETURN CUSBROW(@XACCOUNT,'M',llRetAlias)
ELSE  
  RETURN CUSBROW(@XACCOUNT,lcMastStor,llRetAlias)
ENDIF  

*!**************************************************************************
*! PROG: CUSBROWS.PRG
*! program to browse The stores for a specific Customer by using the new browse
*! written by Hesham El_Sheltawi 15/02/1995
*! NOTE: THIS PROGRAM ASSUMES THAT THE fabric FILE HAS BEEN OPENED
*! 
*!**************************************************************************
PROCEDURE CUSBROWS
Parameters XACCOUNT,llRetAlias
PRIVATE lcStore
lcStore=XSTORE
llWasSel=CUSBROW(@XACCOUNT,'S',@LCSTORE,llRetAlias)
XSTORE =lcStore
RETURN llWasSel

*!**************************************************************************
*! PROG: CUSBROW.PRG
*! program to browse The customer file by using the new browse
*! written by Hesham El_Sheltawi 15/02/1995
*! NOTE: THIS PROGRAM ASSUMES THAT THE fabric FILE HAS BEEN OPENED
*! Called by CUSBROWM.PRG & CUSBROWS.PRG
*!**************************************************************************

PROCEDURE CUSBROW
Parameters XACCOUNT,lcKey,XSTORE,llRetAlias     && returns the account code of the customer selected
PRIVATE lcCityHed , lcCityWid , lcStatHed , lcStatWid , lnCurAlias
PRIVATE lcBrFields,laData

*-- Include the .H file
#INCLUDE R:\ARIA4XP\PRGS\SY\CUSBROW.H

lnCurAlias = SELECT(0)
*-- Hesham (Start)
*-- select country from sycint remotely
*llSycInt = gfOpenFile(oAriaApplication.SysPath+'SycInt',oAriaApplication.SysPath+'Ccontcode','SH')
lnRemResult = oAriaApplication.remotesystemdata.execute("Select * from SYCINT where ccont_code='"+oAriaApplication.DefaultCountry+"'",'',"SYCINTTMP","",oAriaApplication.SystemConnectionString,3,"",SET("DATAS"))
IF lnRemResult=1
  LOCATE
ELSE
  =gfDialog("I","Error Connecting to system database.")
  RETURN .F.
ENDIF
*!*	lnSycIntRc = RECNO('SycInt')
*!*	=SEEK(oAriaApplication.DefaultCountry,'SycInt','Ccontcode')
*!*	lcCityHed = SycInt.Cpart3Lab
*!*	lcCityWid = ALLTRIM(STR(SycInt.Npart3Len))
*!*	lcStatHed = SycInt.Cpart4Lab
*!*	lcStatWid = ALLTRIM(STR(SycInt.Npart4Len))

lcCityHed = SycIntTmp.Cpart3Lab
lcCityWid = ALLTRIM(STR(SycIntTmp.Npart3Len))
lcStatHed = SycIntTmp.Cpart4Lab
lcStatWid = ALLTRIM(STR(SycIntTmp.Npart4Len))
*-- Hesham (End)

DECLARE laData[2]  && array to get values from browse
IF !EMPTY(XACCOUNT)
  XACCOUNT = PADR(IIF(ATC("?",XACCOUNT)<>0,STUFF(XACCOUNT,ATC("?",XACCOUNT),1,""),XACCOUNT),5)
ENDIF  
STORE '' TO laData
llWasSel=.T.
llBrowse = IIF(TYPE('llBrowse')='U',.T.,llBrowse)
lcKey=IIF(TYPE('lcKey')='C',lcKey,'M')
*-- Hesham (Start)
*-- Change the sycint alias to sycinttmp
*!*	IF lcKey='S'
*!*	  lcBrFields = "STORE :h='Store',stName:23:h='Name',"+;
*!*	               "cAddress1 :H='ST '+SycInt.cPart1Lab :R :P=REPLICATE('X',SycInt.nPart1Len),"+;
*!*	               "cAddress2 :H='ST '+SycInt.cPart2Lab :R :P=REPLICATE('X',SycInt.nPart2Len),"+;
*!*	               "cAddress3 :H='ST '+SycInt.cPart3Lab :R :P=REPLICATE('X',SycInt.nPart3Len),"+;
*!*	               "cAddress4 :H='ST '+SycInt.cPart4Lab :R :P=REPLICATE('X',SycInt.nPart4Len),"+;
*!*	               "cAddress5 :H='ST '+SycInt.cPart5Lab :R :P=REPLICATE('X',SycInt.nPart5Len),"+;
*!*	               "cAddress12 :H='BT '+SycInt.cPart1Lab :R :P=REPLICATE('X',SycInt.nPart1Len),"+;
*!*	               "cAddress22 :H='BT '+SycInt.cPart2Lab :R :P=REPLICATE('X',SycInt.nPart2Len),"+;
*!*	               "cAddress32 :H='BT '+SycInt.cPart3Lab :R :P=REPLICATE('X',SycInt.nPart3Len),"+;
*!*	               "cAddress42 :H='BT '+SycInt.cPart4Lab :R :P=REPLICATE('X',SycInt.nPart4Len),"+;
*!*	               "cAddress52 :H='BT '+SycInt.cPart5Lab :R :P=REPLICATE('X',SycInt.nPart5Len),"+;
*!*	               "Phone1 :P= GFPHONETEM() :H='Phone #...',Buyer :H='Buyer',salesrep :H='Rep'"
*!*	ELSE
*!*	  IF lcKey='MSP'
*!*	     lcBrFields = "Phone1 :P=GFPHONETEM() :20 :H='Phone #',Account :H='Acct#',stName:20:h='Name',"+;
*!*	                  "cAddress1 :H='ST '+SycInt.cPart1Lab :R :P=REPLICATE('X',SycInt.nPart1Len),"+;
*!*	                  "cAddress2 :H='ST '+SycInt.cPart2Lab :R :P=REPLICATE('X',SycInt.nPart2Len),"+;
*!*	                  "cAddress3 :H='ST '+SycInt.cPart3Lab :R :P=REPLICATE('X',SycInt.nPart3Len),"+;
*!*	                  "cAddress4 :H='ST '+SycInt.cPart4Lab :R :P=REPLICATE('X',SycInt.nPart4Len),"+;
*!*	                  "cAddress5 :H='ST '+SycInt.cPart5Lab :R :P=REPLICATE('X',SycInt.nPart5Len),"+;
*!*	                  "cAddress12 :H='BT '+SycInt.cPart1Lab :R :P=REPLICATE('X',SycInt.nPart1Len),"+;
*!*	                  "cAddress22 :H='BT '+SycInt.cPart2Lab :R :P=REPLICATE('X',SycInt.nPart2Len),"+;
*!*	                  "cAddress32 :H='BT '+SycInt.cPart3Lab :R :P=REPLICATE('X',SycInt.nPart3Len),"+;
*!*	                  "cAddress42 :H='BT '+SycInt.cPart4Lab :R :P=REPLICATE('X',SycInt.nPart4Len),"+;
*!*	                  "cAddress52 :H='BT '+SycInt.cPart5Lab :R :P=REPLICATE('X',SycInt.nPart5Len),"+;
*!*	                  "Buyer :H='Buyer',salesrep :H='Rep',NetBal:11:H='Balance'"
*!*	  ELSE
*!*	    IF  lcKey='MSN'
*!*	      lcBrFields = "stName:20:h='Name',Account :H='Acct#',"+;
*!*	                   "cAddress1 :H='ST '+SycInt.cPart1Lab :R :P=REPLICATE('X',SycInt.nPart1Len),"+;
*!*	                   "cAddress2 :H='ST '+SycInt.cPart2Lab :R :P=REPLICATE('X',SycInt.nPart2Len),"+;
*!*	                   "cAddress3 :H='ST '+SycInt.cPart3Lab :R :P=REPLICATE('X',SycInt.nPart3Len),"+;
*!*	                   "cAddress4 :H='ST '+SycInt.cPart4Lab :R :P=REPLICATE('X',SycInt.nPart4Len),"+;
*!*	                   "cAddress5 :H='ST '+SycInt.cPart5Lab :R :P=REPLICATE('X',SycInt.nPart5Len),"+;
*!*	                   "cAddress12 :H='BT '+SycInt.cPart1Lab :R :P=REPLICATE('X',SycInt.nPart1Len),"+;
*!*	                   "cAddress22 :H='BT '+SycInt.cPart2Lab :R :P=REPLICATE('X',SycInt.nPart2Len),"+;
*!*	                   "cAddress32 :H='BT '+SycInt.cPart3Lab :R :P=REPLICATE('X',SycInt.nPart3Len),"+;
*!*	                   "cAddress42 :H='BT '+SycInt.cPart4Lab :R :P=REPLICATE('X',SycInt.nPart4Len),"+;
*!*	                   "cAddress52 :H='BT '+SycInt.cPart5Lab :R :P=REPLICATE('X',SycInt.nPart5Len),"+;
*!*	                   "Phone1 :P= GFPHONETEM() :H='Phone #...',Buyer :H='Buyer',salesrep :H='Rep',NetBal:11:H='Balance'"
*!*	    ELSE
*!*	      lcBrFields = "Account :H='Acct#', BtName :H='Name':R,"+;
*!*	                   "cAddress1 :H='ST '+SycInt.cPart1Lab :R :P=REPLICATE('X',SycInt.nPart1Len),"+;
*!*	                   "cAddress2 :H='ST '+SycInt.cPart2Lab :R :P=REPLICATE('X',SycInt.nPart2Len),"+;
*!*	                   "cAddress3 :H='ST '+SycInt.cPart3Lab :R :P=REPLICATE('X',SycInt.nPart3Len),"+;
*!*	                   "cAddress4 :H='ST '+SycInt.cPart4Lab :R :P=REPLICATE('X',SycInt.nPart4Len),"+;
*!*	                   "cAddress5 :H='ST '+SycInt.cPart5Lab :R :P=REPLICATE('X',SycInt.nPart5Len),"+;
*!*	                   "cAddress12 :H='BT '+SycInt.cPart1Lab :R :P=REPLICATE('X',SycInt.nPart1Len),"+;
*!*	                   "cAddress22 :H='BT '+SycInt.cPart2Lab :R :P=REPLICATE('X',SycInt.nPart2Len),"+;
*!*	                   "cAddress32 :H='BT '+SycInt.cPart3Lab :R :P=REPLICATE('X',SycInt.nPart3Len),"+;
*!*	                   "cAddress42 :H='BT '+SycInt.cPart4Lab :R :P=REPLICATE('X',SycInt.nPart4Len),"+;
*!*	                   "cAddress52 :H='BT '+SycInt.cPart5Lab :R :P=REPLICATE('X',SycInt.nPart5Len),"+;
*!*	                   "Phone1 :P= GFPHONETEM() :H='Phone #...',Buyer :H='Buyer',salesrep :H='Rep',NetBal:11:H='Balance'"
*!*	    ENDIF
*!*	  ENDIF
*!*	ENDIF               
IF lcKey='S'
  lcBrFields = "STORE :h='"+LANG_LabelStore+"',stName:23:h='"+LANG_LabelName+"',"+;
               "cAddress1 :H='ST '+SycIntTmp.cPart1Lab :R :P=REPLICATE('X',SycIntTmp.nPart1Len),"+;
               "cAddress2 :H='ST '+SycIntTmp.cPart2Lab :R :P=REPLICATE('X',SycIntTmp.nPart2Len),"+;
               "cAddress3 :H='ST '+SycIntTmp.cPart3Lab :R :P=REPLICATE('X',SycIntTmp.nPart3Len),"+;
               "cAddress4 :H='ST '+SycIntTmp.cPart4Lab :R :P=REPLICATE('X',SycIntTmp.nPart4Len),"+;
               "cAddress5 :H='ST '+SycIntTmp.cPart5Lab :R :P=REPLICATE('X',SycIntTmp.nPart5Len),"+;
               "cAddress12 :H='BT '+SycIntTmp.cPart1Lab :R :P=REPLICATE('X',SycIntTmp.nPart1Len),"+;
               "cAddress22 :H='BT '+SycIntTmp.cPart2Lab :R :P=REPLICATE('X',SycIntTmp.nPart2Len),"+;
               "cAddress32 :H='BT '+SycIntTmp.cPart3Lab :R :P=REPLICATE('X',SycIntTmp.nPart3Len),"+;
               "cAddress42 :H='BT '+SycIntTmp.cPart4Lab :R :P=REPLICATE('X',SycIntTmp.nPart4Len),"+;
               "cAddress52 :H='BT '+SycIntTmp.cPart5Lab :R :P=REPLICATE('X',SycIntTmp.nPart5Len),"+;
               "Phone1 :P= GFPHONETEM() :H='"+LANG_LabelPhone+"',Buyer :H='"+LANG_LabelBuyer+"',salesrep :H='"+LANG_LabelRep+"'"
ELSE
  IF lcKey='MSP'
     lcBrFields = "Phone1 :P=GFPHONETEM() :20 :H='"+LANG_LabelPhone+"',Account :H='"+LANG_LabelAcc+"',stName:20:h='"+LANG_LabelName+"',"+;
                  "cAddress1 :H='ST '+SycIntTmp.cPart1Lab :R :P=REPLICATE('X',SycIntTmp.nPart1Len),"+;
                  "cAddress2 :H='ST '+SycIntTmp.cPart2Lab :R :P=REPLICATE('X',SycIntTmp.nPart2Len),"+;
                  "cAddress3 :H='ST '+SycIntTmp.cPart3Lab :R :P=REPLICATE('X',SycIntTmp.nPart3Len),"+;
                  "cAddress4 :H='ST '+SycIntTmp.cPart4Lab :R :P=REPLICATE('X',SycIntTmp.nPart4Len),"+;
                  "cAddress5 :H='ST '+SycIntTmp.cPart5Lab :R :P=REPLICATE('X',SycIntTmp.nPart5Len),"+;
                  "cAddress12 :H='BT '+SycIntTmp.cPart1Lab :R :P=REPLICATE('X',SycIntTmp.nPart1Len),"+;
                  "cAddress22 :H='BT '+SycIntTmp.cPart2Lab :R :P=REPLICATE('X',SycIntTmp.nPart2Len),"+;
                  "cAddress32 :H='BT '+SycIntTmp.cPart3Lab :R :P=REPLICATE('X',SycIntTmp.nPart3Len),"+;
                  "cAddress42 :H='BT '+SycIntTmp.cPart4Lab :R :P=REPLICATE('X',SycIntTmp.nPart4Len),"+;
                  "cAddress52 :H='BT '+SycIntTmp.cPart5Lab :R :P=REPLICATE('X',SycIntTmp.nPart5Len),"+;
                  "Buyer :H='"+LANG_LabelBuyer+"',salesrep :H='"+LANG_LabelRep+"',NetBal:11:H='"+LANG_LabelBalance+"'"
  ELSE
    IF  lcKey='MSN'
      lcBrFields = "stName:20:h='"+LANG_LabelName+"',Account :H='"+LANG_LabelAcc+"',"+;
                   "cAddress1 :H='ST '+SycIntTmp.cPart1Lab :R :P=REPLICATE('X',SycIntTmp.nPart1Len),"+;
                   "cAddress2 :H='ST '+SycIntTmp.cPart2Lab :R :P=REPLICATE('X',SycIntTmp.nPart2Len),"+;
                   "cAddress3 :H='ST '+SycIntTmp.cPart3Lab :R :P=REPLICATE('X',SycIntTmp.nPart3Len),"+;
                   "cAddress4 :H='ST '+SycIntTmp.cPart4Lab :R :P=REPLICATE('X',SycIntTmp.nPart4Len),"+;
                   "cAddress5 :H='ST '+SycIntTmp.cPart5Lab :R :P=REPLICATE('X',SycIntTmp.nPart5Len),"+;
                   "cAddress12 :H='BT '+SycIntTmp.cPart1Lab :R :P=REPLICATE('X',SycIntTmp.nPart1Len),"+;
                   "cAddress22 :H='BT '+SycIntTmp.cPart2Lab :R :P=REPLICATE('X',SycIntTmp.nPart2Len),"+;
                   "cAddress32 :H='BT '+SycIntTmp.cPart3Lab :R :P=REPLICATE('X',SycIntTmp.nPart3Len),"+;
                   "cAddress42 :H='BT '+SycIntTmp.cPart4Lab :R :P=REPLICATE('X',SycIntTmp.nPart4Len),"+;
                   "cAddress52 :H='BT '+SycIntTmp.cPart5Lab :R :P=REPLICATE('X',SycIntTmp.nPart5Len),"+;
                   "Phone1 :P= GFPHONETEM() :H='"+LANG_LabelPhone+"',Buyer :H='"+LANG_LabelBuyer+"',salesrep :H='"+LANG_LabelRep+"',NetBal:11:H='"+LANG_LabelBalance+"'"
    ELSE
      lcBrFields = "Account :H='"+LANG_LabelAcc+"', BtName :H='"+LANG_LabelName+"':R,"+;
                   "cAddress1 :H='ST '+SycIntTmp.cPart1Lab :R :P=REPLICATE('X',SycIntTmp.nPart1Len),"+;
                   "cAddress2 :H='ST '+SycIntTmp.cPart2Lab :R :P=REPLICATE('X',SycIntTmp.nPart2Len),"+;
                   "cAddress3 :H='ST '+SycIntTmp.cPart3Lab :R :P=REPLICATE('X',SycIntTmp.nPart3Len),"+;
                   "cAddress4 :H='ST '+SycIntTmp.cPart4Lab :R :P=REPLICATE('X',SycIntTmp.nPart4Len),"+;
                   "cAddress5 :H='ST '+SycIntTmp.cPart5Lab :R :P=REPLICATE('X',SycIntTmp.nPart5Len),"+;
                   "cAddress12 :H='BT '+SycIntTmp.cPart1Lab :R :P=REPLICATE('X',SycIntTmp.nPart1Len),"+;
                   "cAddress22 :H='BT '+SycIntTmp.cPart2Lab :R :P=REPLICATE('X',SycIntTmp.nPart2Len),"+;
                   "cAddress32 :H='BT '+SycIntTmp.cPart3Lab :R :P=REPLICATE('X',SycIntTmp.nPart3Len),"+;
                   "cAddress42 :H='BT '+SycIntTmp.cPart4Lab :R :P=REPLICATE('X',SycIntTmp.nPart4Len),"+;
                   "cAddress52 :H='BT '+SycIntTmp.cPart5Lab :R :P=REPLICATE('X',SycIntTmp.nPart5Len),"+;
                   "Phone1 :P= GFPHONETEM() :H='"+LANG_LabelPhone+"',Buyer :H='"+LANG_LabelBuyer+"',salesrep :H='"+LANG_LabelRep+"',NetBal:11:H='"+LANG_LabelBalance+"'"
    ENDIF
  ENDIF
ENDIF               
*-- Hesham (End)
IF !USED('Customer')
=gfOpenFile(oAriaApplication.DataDir+'Customer','Customer','SH')
ENDIF
SELECT CUSTOMER
lcCustOrder = TAG()
IF lckey='MSP'
  SET ORDER TO TAG CUSTOMPH
ELSE
  IF lckey='MSN'
    SET ORDER TO TAG CUSTOMNM
  ELSE  
    SET ORDER TO TAG CUSTOMER
  ENDIF
ENDIF  
lnCusRec=0
IF  IIF(lcKey<>'MSN' AND lcKey<>'MSP',!SEEK(lckey+XACCOUNT+IIF(lcKey='S',xstore,''),'customer'),.T.) OR llBrowse
  IF lcKey<>'MSN' AND lcKey<>'MSP'
    lnSoftSeek=RECNO(0)
    IF lnSoftSeek<>0 
      GO lnSoftSeek
    ELSE
      GO TOP
    ENDIF     
  ENDIF  
  IF lcKey='S' OR lcKey='MSP' OR lcKey='MSN'
    IF lcKey='S'
      IF !SEEK('S'+xAccount) 
        =gfModalgen("INM40082B00000","ALERT",LANG_LabelStores+'|'+LANG_LabelAccount+' '+xAccount)
        IF !EMPTY(lcCustOrder)
          SET ORDER TO TAG (lcCustOrder)
        ELSE
          SET ORDER TO  
        ENDIF  
        xStore=SPACE(8)
        RETURN .F.
      ENDIF 
      IF lcKey='S'
        llWasSel=ARIABROW(["S"+XACCOUNT],LANG_LabelStTitle+XACCOUNT ,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','',"ACCOUNT,STORE","laData",.F.)  
      ELSE
        llWasSel=ARIABROW(["M"+XACCOUNT],LANG_LabelAcTitle,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','',"ACCOUNT","laData",lcKey='M')  
      ENDIF
      lnCusRec = RECNO('CUSTOMER')
    ELSE
      *ASM, going to first record if no value was seeked [Start]
      IF EMPTY(XACCOUNT)
        LOCATE
      ENDIF
      *ASM, going to first record if no value was seeked [End]
      llWasSel=ARIABROW(['M','S'],LANG_LabelAcTitle,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','',IIF(lcKey='S',"ACCOUNT,STORE","ACCOUNT,STORE"),"laData",lcKey='M')
    ENDIF      
  ELSE
    llWasSel=ARIABROW(["M"],LANG_LabelAcTitle,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','',IIF(lcKey='S',"ACCOUNT,STORE","ACCOUNT"),"laData",lcKey='M')
  ENDIF  
  IF llWasSel
    XACCOUNT  = laData[1]
    IF lcKey='S' OR lcKey='MSP' OR lcKey='MSN'
      XSTORE = laData[2]
    ENDIF
  ELSE
    XACCOUNT = SPACE(5)  
  ENDIF  
ENDIF  
IF !EMPTY(lcCustOrder)
  SET ORDER TO TAG (lcCustOrder)
ELSE
  SET ORDER TO  
ENDIF
IF llWasSel AND lnCusRec>0
  GO lnCusRec
ENDIF
IF llRetAlias
  SELECT (lnCurAlias)
ENDIF  
*-- Hesham (Start)
*-- Close the temprory alias sycinttmp
*!*	IF llSycInt 
*!*	  USE IN 'SycInt'
*!*	ELSE
*!*	  IF BETWEEN(lnSycIntRc,1,RECCOUNT('SycInt'))
*!*	    GO lnSycIntRc IN 'SycInt'
*!*	  ENDIF
*!*	ENDIF  
IF USED("SYCINTTMP")
  USE IN SYCINTTMP
ENDIF
*-- Hesham (End)

RETURN llWasSel

*!*************************************************************
*! Name      : gfPhoneTem
*! Developer : Hesham Shereef
*! Date      : 1993-1995 
*! Purpose   : Fix the phone template
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : 
*!*************************************************************
*  FUNCTION RETURN THE PHONE TEMPLATE FORMAT "PICTURE"
*:->
FUNCTION gfPhoneTem

*B132026,1 WSH 05/16/2006 Add parameter to allow getting Phone Template for any Country. [Start]
*PARAMETER lcPhoneTem
LPARAMETER lcPhoneTem, lcCont_ID
*B132026,1 WSH 05/16/2006 [End]

*IF PARAMETERS()>0
 * RETURN "@R "+STRTRAN(lcPhoneTem,'#','X')
*ELSE
 * RETURN "@R "+STRTRAN(oAriaApplication.PhoneMask,'#','X')
*ENDIF  

*B132026,1 WSH 05/16/2006 Add parameter to allow getting Phone Template for any Country. [Start]
*IF PARAMETERS() = 0
*  *If the function get called at design time, the check on oAriaApplication will prevent errors
*  *while opening a form 
*  lcPhoneTem = IIF(TYPE("oAriaApplication.PhoneMask")='U','',oAriaApplication.PhoneMask)
*ENDIF
IF PARAMETERS() = 0 OR TYPE("lcPhoneTem") # 'C' OR EMPTY(lcPhoneTem)
  lcPhoneTem = IIF(TYPE("oAriaApplication.PhoneMask") = 'U', '', oAriaApplication.PhoneMask)
ENDIF

IF TYPE("lcCont_ID") = "C" AND !EMPTY(lcCont_ID) AND lcCont_ID # PADR(oAriaApplication.DefaultCountry,6)
  LOCAL lnRemResult, lnAlias
  lnAlias     = SELECT(0)
  lnRemResult = oAriaApplication.RemoteSystemData.Execute("SELECT * FROM SYCINT WHERE cCont_Code = '" + lcCont_ID + "'",;
                        "", "TMPSYCINT", "", oAriaApplication.SystemConnectionString, 3, "", SET("Datasession"))
  
  IF lnRemResult >= 1
    SELECT TMPSYCINT
    LOCATE
    lcPhoneTem = IIF(FOUND() AND !EMPTY(cPhoneTemp), ALLTRIM(cPhoneTemp), lcPhoneTem)
    USE IN TMPSYCINT
  ENDIF
  SELECT (lnAlias)
ENDIF
*B132026,1 WSH 05/16/2006 [End]

RETURN STRTRAN(lcPhoneTem,'#','X')
*!**************************************************************************
*! PROG: REPCHK.PRG
*! program to browse through the SALESREP file by using the new browse
*! written by Hesham El_Sheltawi 15/02/1995
*! NOTE: THIS PROGRAM ASSUMES THAT THE SALESREP FILE HAS BEEN OPENED
*! 
*!**************************************************************************
FUNCTION REPCHK
PARAMETERS XREPCODE,llRetAlias

PRIVATE lcBrFields,lnCurAlias,laData

llBrowse = IIF(TYPE('llBrowse')='U',.T.,llBrowse)

IF !llBrowse .AND. SEEK(XREPCODE,'SALESREP','SALESREP')
  DATA = SUBSTR(SALESREP.NAME,1,20)
  RETURN
ENDIF
DECLARE laData[3] && array to get values from browse
STORE '' TO laData
SELECT SALESREP
lcBrFields = [repCode:H="Code",Name:H="Name",Phone :P= gfPhoneTem() :H="Phone"]
lnCurAlias = SELECT()

IF llBrowse OR !SEEK(XREPCODE,'SALESREP','SALESREP')
  DATA = SPACE(20)
  lnSoftSeek=RECNO(0)
  IF lnSoftSeek<>0 .AND. lnSoftSeek <= RECCOUNT("SALESREP")
    GO lnSoftSeek
  ELSE
    GO TOP
  ENDIF     
  IF EOF()
    =gfModalgen("TRM00052B00000","ALERT")
    XREPCODE = SPACE(3)
  ELSE
    IF ARIABROW('',"Sales Representatives",gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'',"","REPCODE","laData")
      XREPCODE  = laData[1]
      DATA = SUBSTR(SALESREP.NAME,1,20)
    ELSE
      XREPCODE = SPACE(3)
    ENDIF
  ENDIF
ENDIF  
IF llRetAlias
  SELECT (lnCurAlias)
ENDIF  
RETURN

*!*************************************************************
*! Name      : lfGetprice
*! Developer : WAM
*! Date      : 02/22/1999
*! Purpose   : Get Style Price
*!*************************************************************
*! Calls     : lfCheckPri()
*!*************************************************************
*! Passed Parameters  :  lcStyle : Style
*!                       lcLevel : Price level
*!                       lnQuantity : invoice Quantity
*!*************************************************************
*! Returns            :  Alternative price
*!*************************************************************
*! Example            :  =lfGetprice(m.Style,'A',100)
*!*************************************************************
FUNCTION gfGetprice
PARAMETERS lcStyle, lcLevel, lnQuantity, lcCuurCode
PRIVATE lnPrice, lnAlias

lnAlias = SELECT()
lcCuurCode = IIF(TYPE('lcCuurCode')='C' AND !EMPTY(lcCuurCode),lcCuurCode,oAriaApplication.BaseCurrency)
=SEEK(lcStyle,'Style')
IF lcLevel = 'Q'
  DO CASE
    *B132211,1 KHM 05/18/2006 [Begin]
    *CASE Style.nAtQtyC > 0 AND lnQuantity > Style.nAtQtyC
    CASE Style.nAtQtyC > 0 AND lnQuantity >= Style.nAtQtyC
    *B132211,1 KHM 05/18/2006 [End]
      lcLevel = 'C'
    *B132211,1 KHM 05/18/2006 [Begin]
    *CASE Style.nAtQtyB > 0 AND lnQuantity > Style.nAtQtyB
    CASE Style.nAtQtyB > 0 AND lnQuantity >= Style.nAtQtyB
    *B132211,1 KHM 05/18/2006 [End]
      lcLevel = 'B'
    OTHERWISE
      lcLevel = 'A'
  ENDCASE
ELSE
  lcLevel=IIF(INLIST(lcLevel,'A','B','C'),lcLevel,'A')
ENDIF
lnPrice = IIF(lcCuurCode=oAriaApplication.BaseCurrency,Style.Price&lcLevel,;
                      gfStyPrice(lcStyle,lcLevel,lcCuurCode))
SELECT (lnAlias)
RETURN(lnPrice)

*!*************************************************************
*! Name      : lfCheckPri
*! Developer : WAM
*! Date      : 02/22/1999
*! Purpose   : Select alternative price level
*!*************************************************************
*! Calls     : gfStyPrice(),SOSTYPRI.SPX
*!*************************************************************
*! Passed Parameters  :  lcStyle : Style
*!                       lcLevel : Price level
*!                       lcCuurCode : Currency Code
*!*************************************************************
*! Returns            :  Alternative price
*!*************************************************************
*! Example            :  =lfCheckPri('A')
*!*************************************************************
FUNCTION lfCheckPri
PARAMETERS lcStyle,lcLevel, lcCuurCode
lnPrice = 0
DO FORM (oAriaApplication.ScreenHome+"SOSTYPRI") WITH lcStyle,lcLevel, lcCuurCode TO lnPrice
RETURN(lnPrice)

*!*************************************************************
*! Name      : gfGLBrowse
*! Developer : MALAK - Malak Hanna
*! Date      : 05/18/1995
*! Purpose   : To check and browse the gl link codes from gl_Link file .
*!*************************************************************
*! Calls     : ARIABROW()
*!*************************************************************
*! Passed Parameters  : lcLinkType --> '01' for 'Sales' type
*!                                 --> '02' for 'Style Invertory' type
*!                                 --> '03' for 'Material Inverntory' type
*!                                 --> '04' for 'Work in Process' type
*!                                 --> '00' for main types
*!                                 --> ''   for all types except '00'
*!                      lcLinkCode --> Variables that holds entered link code
*!                      lcLinkDesc --> Variables that holds link descrption 
*!                      lnSalesPart--> 0 Browse the whole sales types
*!                                     1 Browse Customer sales
*!                                     2 Browse Style sales
*!*************************************************************
*! Returns            :  .T. or .F.
*!*************************************************************
*! Example            : =gfGLBrowse('02',@m.LinkCode,@lcLinkDesc)
*!*************************************************************
FUNCTION gfGLBrowse
PARAMETER lcLinkType, lcLinkCode, lcLinkDesc,lnSalesPart,llBrowse

PRIVATE   lcLinkType, lcLinkCode, lcLinkDesc,laData,lcBrFields

lnOldAlias = SELECT()
llOpnGlLnk = .F.
IF USED('GL_LINK')
  lnOldTag = ORDER('GL_LINK')
ELSE
  llOpnGlLnk = gfOpenFile(oAriaApplication.DataDir+'GL_LINK','','SH')
ENDIF

IF lcLinkType='02' .AND. lnSalesPart=2
  SET ORDER TO TAG SALES IN GL_LINK
ELSE
  SET ORDER TO TAG GL_LINK1 IN GL_LINK
ENDIF

llBrowse = IIF(TYPE('llBrowse') = 'U' ,.F., llBrowse .OR. '?' $ lcLinkCode)

*-- MAB 05/28/2003 Avoid browsing in View mode ..... BEGIN
LOCAL llValidate
llValidate = (TYPE("_screen.ActiveForm.Parent.ActiveMode") != "C") OR;
    _screen.ActiveForm.Parent.ActiveMode != "V"
*IF llBrowse OR !SEEK(lcLinkType+lcLinkCode,'GL_LINK')
IF llValidate AND (llBrowse OR !SEEK(lcLinkType+lcLinkCode,'GL_LINK'))
*-- MAB 05/28/2003 Avoid browsing in View mode ..... END
  lnPos    = IIF(lcLinkType='02' .AND. lnSalesPart=2,4,1)
  lnLength = IIF(lcLinkType='02' .AND. INLIST(lnSalesPart,1,2),3,6)

  SELECT LinkType,SUBSTR(Link_Code,lnPos,lnLength) AS LinkCode FROM GL_LINK ;
  GROUP BY LinkType,LinkCode ;
  HAVING (LinkType = lcLinkType);
  INTO CURSOR lcCodesCurs
  SET RELATION TO lcLinkType+LinkCode INTO GL_LINK

  DECLARE laValues[2]     && array to get values from browse
  
  DECLARE laLinkType[5] 
  laLinkType[1] = 'Customer           '
  laLinkType[2] = 'Sales              '
  laLinkType[3] = 'Style Invertory    '
  laLinkType[4] = 'Material Inverntory'    
  laLinkType[5] = 'Work in Process    '
  *E300592,1 (End)
    
  lcFile_Ttl    = 'GL Link Codes' + IIF(EMPTY(lcLinkType) OR lcLinkType='00' ;
                  ,'',' for '+ALLTRIM(laLinkType[VAL(lcLinkType)]))

  lcBrFields    = "LinkCode :H='Code',"        +;
                  "GL_LINK.LinkDesc  :H='Description'"  +;
                  IIF(EMPTY(lcLinkType),",lcDummy=laLinkType[VAL(LinkType)]:H='Type'",'')


  =ARIABROW('',lcFile_Ttl,gnBrHSRow1, gnBrHSCol1, gnBrHSRow2, gnBrHSCol2,'','','LinkCode,GL_LINK.LinkDesc','laValues')
                  
  IF !EMPTY(laValues[1])
    lcLinkCode = laValues[1]
    lcLinkDesc = IIF(EMPTY(laValues[2]),SPACE(30),laValues[2])
    lcLinkType = LinkType
  ELSE
    lcLinkCode = SPACE(3)
    lcLinkDesc = SPACE(30)
  ENDIF
  
  USE IN lcCodesCurs
 
ELSE
  lcLinkDesc = GL_LINK.LinkDesc
ENDIF

IF llOpnGlLnk
  USE IN GL_LINK
ELSE  
  SET ORDER TO TAG lnOldTag IN GL_LINK
ENDIF  

SELECT (lnOldAlias)
llBrowse = .F.
RETURN !EMPTY(lcLinkCode)  && end of gfGLBrowse.

*!**************************************************************************
*! PROG: ORDBROWA.PRG
*! program to browse The Order header file sorting by account by using the new browse
*! written by Hesham El_Sheltawi 15/02/1995
*! NOTE: THIS PROGRAM ASSUMES THAT THE order header FILE HAS BEEN OPENED
*! 
*!**************************************************************************
PROCEDURE ORDBROWA

Parameters XACCOUNT , llRetAlias , lcOrdrType
lcOrdrType = IIF(TYPE('lcOrdrType') <> 'C' .OR. EMPTY(lcOrdrType) , 'O' ,;
                 IIF(UPPER(lcOrdrType) = 'A' , '' , UPPER(lcOrdrType)))
*-- Include the .H file
#INCLUDE R:\ARIA4XP\PRGS\SY\ORDBROW.H

IF TYPE('XORDER')='U'
 STORE '' TO XORDER
ENDIF

IF TYPE('XPOSEARCH')='U'
  XPOSEARCH = .F.
ENDIF

IF TYPE('XSTORE') = 'U'
  XSTORE = ''
ENDIF

IF XPOSEARCH
  DECLARE laValues[1]  && array to get values from browse
  STORE '' TO laValues
  lnOldWork = SELECT()
  SELECT ORDHDR

  lnOldOrder = SYS(21)
  SET ORDER TO TAG ORDCUST
  
  =SEEK(xAccount+UPPER(xCustPO)+IIF(EMPTY(xStore),'',xStore))
  =SEEK(xAccount+UPPER(xCustPO))
  
  IF FOUND()
    DO CASE
      CASE !EMPTY(xStore) .AND. !EMPTY(lcOrdrType)
        LOCATE REST;
              WHILE Account + UPPER(CustPO) = xAccount+UPPER(xCustPO);
              FOR Store = xStore .AND. cOrdType = lcOrdrType
      
      CASE !EMPTY(xStore)
        LOCATE REST;
              WHILE Account + UPPER(CustPO) = xAccount+UPPER(xCustPO);
              FOR Store = xStore
      
      CASE !EMPTY(lcOrdrType)
        LOCATE REST;
              WHILE Account + UPPER(CustPO) = xAccount+UPPER(xCustPO);
              FOR cOrdType = lcOrdrType
      
    ENDCASE
  ENDIF
  SET ORDER TO &lnOldOrder
  lcBrFields = [Order:H="]+LANG_LabelOrder+[",status:H="]+LANG_Labelstatus+[",lcSesDesc=gfCodDes(Season,'SEASON'):H="]+LANG_LabelSeason+[",lcDivDesc=gfCodDes(cDivision,'CDIVISION'):H="]+LANG_LabelDivision+[",]+;
               [CustPo=IIF(multipo,'*Multi_PO*',custpo):H="]+LANG_LabelCustPo+[",]+;
               [ACCOUNT:H="]+LANG_LabelACCOUNT+[",store=IIF(MULTI='Y','*Multi*',STORE):H="]+LANG_Labelstore+[",Customer.stname]+;
               [:H="]+LANG_Labelname+[",Open:H="]+LANG_LabelOpen+[",OpenAmt:H="]+LANG_LabelOpenAmt+[",Ship:H="]+LANG_LabelShip+[",Shipamt:H="]+LANG_LabelShipamt+[",]+;
               [start:H="]+LANG_Labelstart+[",Complete:H="]+LANG_LabelComplete+[",Note1:H="]+LANG_LabelNote+["]

  IF FOUND()
    IF ARIABROW('FOR Account + UPPER(CustPO) + cOrdType + Order = xAccount + UPPER(xCustPO)' +;
                 IIF(EMPTY(lcOrdrType) , "" , " .AND. cOrdType + Order = lcOrdrType") ,;
                 IIF(EMPTY(xStore),""," .AND. Store = xStore") +;
                 "Orders",gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','ORDER','laValues')
      xOrder = laValues[1]
    ELSE
      STORE SPACE(6)  TO xOrder
    ENDIF
  ELSE
    =gfModalgen("TRM32118B00000","ALERT")
    *=gfDialog('I','No orders matching the selected criteria.')
    STORE SPACE(6) TO xOrder
  ENDIF  
  
  IF llRetAlias
    SELECT (lnOldWork)
  ENDIF
  
  RETURN xOrder
ELSE
  RETURN ORDBROW(@XACCOUNT , 'A' , @XORDER , llRetAlias ,;
                 IIF(EMPTY(lcOrdrType) , 'A' , lcOrdrType))
ENDIF

*!**************************************************************************
*! PROG: ORDBROWO.PRG
*! program to browse The Order header file sorting by order  by using the new browse
*! written by Hesham El_Sheltawi 15/02/1995
*! NOTE: THIS PROGRAM ASSUMES THAT THE order header FILE HAS BEEN OPENED
*! 
*!**************************************************************************
PROCEDURE ORDBROWO
PARAMETERS XORDER , llRetAlias , lcOrdrType

IF TYPE('XACCOUNT')='U'
 STORE '' TO XACCOUNT
ENDIF
RETURN ORDBROW(@XACCOUNT , 'O' , @XORDER , llRetAlias , lcOrdrType)

*!**************************************************************************
*! PROG: ORDBROW.PRG
*! program to browse The Order header file by using the new browse
*! written by Hesham El_Sheltawi 15/02/1995
*! NOTE: THIS PROGRAM ASSUMES THAT THE order header FILE HAS BEEN OPENED
*!  called by CUSBROWA.PRG & CUSBROWO.PRG
*!**************************************************************************
PROCEDURE ORDBROW
PARAMETERS XACCOUNT , lcKey , Xorder , llRetAlias , lcOrdrType    && returns the account code of the customer selected

lcOrdrType = IIF(TYPE('lcOrdrType') <> 'C' .OR. EMPTY(lcOrdrType) , 'O' ,;
                 IIF(UPPER(lcOrdrType) = 'A' , '' , UPPER(lcOrdrType)))
PRIVATE lcBrFields,lnCurAlias,laData

*-- Include the .H file
#INCLUDE R:\ARIA4XP\PRGS\SY\ORDBROW.H

DECLARE laData[2]  && array to get values from browse
DECLARE laOrdState[4,2]
laOrdState[1,1] = LANG_LabelStatusOpen
laOrdState[1,2] = 'O'
laOrdState[2,1] = LANG_LabelStatusHold
laOrdState[2,2] = 'H'
laOrdState[3,1] = LANG_LabelStatusCancel
laOrdState[3,2] = 'X'
laOrdState[4,1] = LANG_LabelStatusComplete
laOrdState[4,2] = 'C'
STORE '' TO laData
llWasSel=.T.
lnCurAlias = SELECT()
llBrowse = IIF(TYPE('llBrowse')='U',.T.,llBrowse) && variable to determine forcing browse or not

lcBrFields = [Order:H="]+LANG_LabelOrder+[",status:H="]+LANG_Labelstatus+[",lcSesDesc=gfCodDes(Season,'SEASON'):H="]+LANG_LabelSeason+[",lcDivDesc=gfCodDes(cDivision,'CDIVISION'):H="]+LANG_LabelDivision+[",]+;
             [CustPo=IIF(multipo,'*Multi_PO*',custpo):H="]+LANG_LabelCustPo+[",]+;
             [ACCOUNT:H="]+LANG_LabelACCOUNT+[",store=IIF(MULTI='Y','*Multi*',STORE):H="]+LANG_Labelstore+[",Customer.stname]+;
             [:H="]+LANG_Labelname+[",Open:H="]+LANG_LabelOpen+[",OpenAmt:H="]+LANG_LabelOpenAmt+[",Ship:H="]+LANG_LabelShip+[",Shipamt:H="]+LANG_LabelShipamt+[",]+;
             [start:H="]+LANG_Labelstart+[",Complete:H="]+LANG_LabelComplete+[",]+;
             [Note1:H="]+LANG_LabelNote+["]

IF !USED('Customer')
  =gfOpenFile(oAriaApplication.DataDir+'Customer',oAriaApplication.DataDir+'Customer','SH')
ENDIF

SELECT ORDHDR
SET RELATION TO IIF(STORE=SPACE(8),'M'+ACCOUNT,'S'+ACCOUNT+STORE) INTO CUSTOMER

*B608245,1 MMT 08/28/2007 fix bug of not browsing stores orders[Start]
lcOldIndex =  ''
IF TYPE('xSTORE') <> 'U' AND !EMPTY(xSTORE)
 lcOldIndex =  ORDER('ORDLINE')
 SET ORDER TO ORDLINST IN 'ORDLINE'   && CORDTYPE+ORDER+STORE+STYLE+STR(LINENO,6)
 SET RELATION TO lcOrdrType + Ordhdr.order + xSTORE INTO Ordline ADDITIVE
ENDIF 
*B608245,1 MMT 08/28/2007 fix bug of not browsing stores orders[End]

lcOrdOrder = TAG()
IF lcKey='A'
  SET ORDER TO TAG ORDACCT
ELSE
  SET ORDER TO TAG ORDHDR
ENDIF  
IF lckey='A'  && called from cus. prog.
  IF !SEEK(xAccount)
    =gfModalgen("INM40082B00000","ALERT",LANG_Labelorders+'|'+LANG_LabelAccount+' '+xAccount)
*    =gfDialog("I","No orders have been found for account# "+xAccount+".") 
    IF !EMPTY(lcOrdOrder)
      SET ORDER TO TAG (lcOrdOrder)
    ELSE
      SET ORDER TO  
    ENDIF  
    SET RELATION TO
    IF llRetAlias
      SELECT (lnCurAlias)
    ENDIF  
    RETURN .F.
  ENDIF
ENDIF
IF llBrowse OR !SEEK(XACCOUNT)
  lnSoftSeek=RECNO(0)
  IF lnSoftSeek<>0 .AND. lnSoftSeek <= RECCOUNT("ORDHDR")
    GO lnSoftSeek
  ELSE
    GO TOP
  ENDIF     
    lcPushB=IIF(lcKey="A",'',"Fi\<nd;;\<Descending;\<Filter;\!\<Select;\?\<Cancel")
    IF lcKey='A'
      *! B040211,1 ASM 05/10/2006 Bug in the Global Browse  [Start]
      *llWasSel=ARIABROW([XACCOUNT + lcOrdrType]+' FOR STORE = IIF(EMPTY(xSTORE),"",xStore)',;
               "Orders",gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,"",lcPushB,'ORDER,Account',"laData")

      *B608245,1 MMT 08/28/2007 fix bug of not browsing stores orders[Start]         
      *llWasSel=ARIABROW([XACCOUNT + lcOrdrType]+' FOR STORE = "'+IIF(EMPTY(xSTORE),'"',xStore+'"'),;
               "Orders",gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,"",lcPushB,'ORDER,Account',"laData")
      llWasSel=ARIABROW([XACCOUNT + lcOrdrType]+' FOR STORE = "'+IIF(EMPTY(xSTORE),'"',xStore+'" OR !EOF("ORDLINE")'),;
               "Orders",gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,"",lcPushB,'ORDER,Account',"laData")
      *B608245,1 MMT 08/28/2007 fix bug of not browsing stores orders[END]         

      *! B040211,1 ASM 05/10/2006 Bug in the Global Browse  [End]
    ELSE
      llWasSel=ARIABROW('lcOrdrType',"Orders",gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,"",lcPushB,'Order,ACCOUNT',"laData")
    ENDIF
    IF llWasSel
      XORDER    = laData[1]    
      XACCOUNT  = laData[2]
    ELSE
      XORDER = SPACE(6)
      XACCOUNT = SPACE(5)
    ENDIF  
ENDIF  

*B608245,1 MMT 08/28/2007 fix bug of not browsing stores orders[Start]
IF TYPE('xSTORE') <> 'U' AND !EMPTY(xSTORE) AND !empty(lcOldIndex)
  SET ORDER TO (lcOldIndex) IN 'ORDLINE'
ENDIF   
*B608245,1 MMT 08/28/2007 fix bug of not browsing stores orders[End]

IF !EMPTY(lcOrdOrder)
  SET ORDER TO TAG (lcOrdOrder)
ELSE
  SET ORDER TO  
ENDIF  
SET RELATION TO
IF llRetAlias
  SELECT (lnCurAlias)
ENDIF  
RETURN llWasSel

*!*************************************************************************
*! Name      : gfCrdtBrow
*! Developer : Reham Alallamy
*! Date      : 30/03/97
*! Purpose   : Func. to browse all the available credit memos.
*!*************************************************************************
*! Calls     :
*!*************************************************************************      
*! Returns   :  
*!*************************************************************************     
*
FUNCTION gfCrdtBrow
PARAMETER lcCrMemo,llRetAlias,lcAccount,lcStore
PRIVATE lcBrFields,lnCurAlias,laData
PRIVATE lcCrMOrdr 

lcAccount = IIF(TYPE('lcAccount')='C',lcAccount,'')
lcStore   = IIF(TYPE('lcStore')='C',lcStore,'')
DECLARE laData[1]  && array to get values from browse
STORE '' TO laData
llWasSel = .T.
llBrowse = IIF(TYPE('llBrowse') = 'U' , .T. , llBrowse) && variable to determine forcing browse or not
lcBrFields = [CrMemo,Account:H="Acct#",Store,CrDate,RaNo,]+;
             [Status:H="S",Reference:H="Ref.",Pieces,TotCredit:H="Amount",]+;
             [cWareCode:H="WareHouse",Invoice,Order,Reason,]+;
             [cDivision,Salesrep1,Salesrep2]
lnCurAlias = SELECT()
lcCrMOrder = ''
llOpenCrM = .F.
IF !USED("RETHDR")
  *B608139,1 WAM Change function gfCrdtBrow to browse credit memos from SQL tables not from FOX tables
  *SELECT 0
  *USE (oAriaApplication.DataDir+'RETHDR') IN 0 ORDER TAG RETHDRA
  =gfOpenTable(oAriaApplication.DataDir+'RETHDR',oAriaApplication.DataDir+'RETHDRA','SH')
  *B608139,1 WAM (End)
  llOpenCrM = .T.
ELSE
  SELECT RETHDR
  *B608139,1 WAM Change function gfCrdtBrow to browse credit memos from SQL tables not from FOX tables
  *lcCrMOrder = TAG()
  *SET ORDER TO TAG RETHDRA
  lcCrMOrder = ORDER()
  gfSETORDER("RETHDRA")
  *B608139,1 WAM (End)
ENDIF
*B608139,1 WAM Change function gfCrdtBrow to browse credit memos from SQL tables not from FOX tables
*=SEEK(lcAccount)
SELECT RETHDR
=gfSEEK(lcAccount)
*B608139,1 WAM (End)
LOCATE REST WHILE Account + CrMemo = lcAccount FOR Store = IIF(EMPTY(lcStore) , ALLTRIM(lcStore) , PADR(lcStore,8))

IF !FOUND()
  *E300455,1 Message : 40058
  *E300455,1 No Credit Memos found
  *E300455,1 Button : 00000
  *E300455,1 Ok
  =gfModalGen('TRM40058B00000','ALERT',IIF(EMPTY(lcAccount),'',;
  'for account: '+lcAccount)+IIF(EMPTY(lcStore),'',' store: '+lcStore))
  lcCrMemo = SPACE(6)
ELSE
  IF llBrowse .OR. !SEEK(lcAccount+lcCrMemo)
    lnSoftSeek = RECNO(0)
    IF lnSoftSeek > 0 .AND. lnSoftSeek <= RECCOUNT("RETHDR")
      GO lnSoftSeek
    ELSE
      GO TOP
    ENDIF
    SELECT RETHDR
    *B608139,1 WAM Change function gfCrdtBrow to browse credit memos from SQL tables not from FOX tables
    *lcCrMOrdr = TAG()
    *SET ORDER TO TAG RETHDR
    lcCrMOrdr = ORDER()
    gfSETORDER("RETHDR")
    *B608139,1 WAM (End)
    llWasSel = ARIABROW("FOR ACCOUNT=lcAccount AND STORE=IIF(EMPTY(lcStore) , ALLTRIM(lcStore) , PADR(lcStore,8))","Credit Memos",gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,"",'','CrMemo',"laData")

    *B608139,1 WAM Change function gfCrdtBrow to browse credit memos from SQL tables not from FOX tables
    *SET ORDER TO TAG (lcCrMOrdr) IN RETHDR
    gfSETORDER(lcCrMOrdr)
    *B608139,1 WAM (End)
    lcCrMemo = IIF(llWasSel , laData[1] , SPACE(6))
  ENDIF
ENDIF
IF llOpenCrM
  *B608139,1 WAM Change function gfCrdtBrow to browse credit memos from SQL tables not from FOX tables
  *USE IN RETHDR
  gfCloseTable('RETHDR')
  *B608139,1 WAM (End)
ELSE
  SELECT RETHDR
  IF !EMPTY(lcCrMOrder)
    *B608139,1 WAM Change function gfCrdtBrow to browse credit memos from SQL tables not from FOX tables
    *SET ORDER TO TAG (lcCrMOrder)
    gfSETORDER(lcCrMOrder)
    *B608139,1 WAM (End)
  ELSE
    *B608139,1 WAM Change function gfCrdtBrow to browse credit memos from SQL tables not from FOX tables
    *SET ORDER TO
    gfSETORDER()
    *B608139,1 WAM (End)
  ENDIF
ENDIF
IF llRetAlias
  SELECT (lnCurAlias)
ENDIF
RETURN (lcCrMemo)

*****************************************************************************
* PROG: RABROW
* DESC: UDF() PROGRAM TO BROWSE THE R/A FOR A SPECIFIC ACCOUNT
* using the new browse
*****************************************************************************
PROCEDURE RABROW
PARAMETER XRANO,llRetAlias
PRIVATE lcBrFields,lnCurAlias,laData

DECLARE laData[2]  && array to get values from browse
STORE '' TO laData
llWasSel=.T.
llBrowse = IIF(TYPE('llBrowse')='U',.F.,llBrowse) && variable to determine forcing browse or not

lcBrFields = [RANO:H="R/A #",RADATE:H="Issued",VOID:]+;
             [H="Void",AUTH:H="Pieces",AUTHAMT:H="Amount",]+;
             [invoice:H="Invoice",order:H="Order",]+;
             [custpo:H="Cust P/O",cartons:H="Cartons"]
lnCurAlias = SELECT()
IF TYPE('XACCOUNT')='U'
 STORE '' TO XACCOUNT
ENDIF             

IF !USED('RETAUTH')
  
  *WSH [Start]
  *=gfOpenFile(oAriaApplication.DataDir+'RETAUTH',oAriaApplication.DataDir+'RETAUTH','SH')
  =gfOpenTable(oAriaApplication.DataDir+'RETAUTH',oAriaApplication.DataDir+'RETAUTH','SH')
  *WSH [End]
  
ENDIF
SELECT RETAUTH

*WSH [Start]
*lcRatOrder = TAG()
*SET ORDER TO TAG RETAUTHA
lcRatOrder = ORDER()
gfSETORDER("RETAUTHA")
*WSH [End]

IF SEEK(xAccount + xRaNo)
  IF STATUS <> 'O'
    =gfDialog('I', 'This R/A is already complete!')
    XRANO = SPACE(6)
  ELSE
    XRANO = RANO
  ENDIF
ELSE
  
  *WSH [Start]
  *lnSoftSeek=RECNO(0)
  *IF SEEK(xAccount)
  IF gfSEEK(xAccount)
  *WSH [End]
  
    IF TYPE('XSTORE') = "U"
      XSTORE=SPACE(8)
    ENDIF
    LOCATE REST FOR STATUS='O' .AND. IIF(EMPTY(XSTORE),.T.,STORE=XSTORE) ;
                WHILE ACCOUNT=xAccount    
    IF FOUND()
      
      *WSH [Start]
      *IF lnSoftSeek<>0 
      *  GO lnSoftSeek
      *ELSE
      *  GO TOP
      *ENDIF     
      *llWasSel=ARIABROW([xaccount FOR STATUS='O' .AND.;
               IIF(EMPTY(XSTORE),.T.,STORE=XSTORE)],;
               "Return Authorizations",;
               gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,"",'','RANO',"laData")
      *XRANO = IIF(llWasSel,laData[1],SPACE(6))
      llWasSel=ARIABROW([xaccount FOR STATUS='O' .AND.;
               IIF(EMPTY(XSTORE),.T.,STORE=XSTORE)],;
               "Return Authorizations",;
               gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,"",'','RANO',"laData",.T.,"RETAUTH")
      XRANO = IIF(llWasSel,laData[1],SPACE(6))
      *WSH [End]
      
    ELSE
      =gfModalgen("INM40082B00000","ALERT","Open R/As"+'|'+"Account"+' '+xAccount)
      *=gfDialog("I",'There are no open R/As for this account')
      XRANO = SPACE(6)
    ENDIF
  ELSE
    =gfModalgen("INM40082B00000","ALERT","R/As"+'|'+"Account"+' '+xAccount)
    *=gfDialog('I', 'There are no R/As for this account ! ')
    XRANO = SPACE(6)
  ENDIF
ENDIF

IF !EMPTY(lcRatOrder)
  
  *WSH [Start]
  *SET ORDER TO TAG (lcRatOrder)
  gfSETORDER(lcRatOrder)
  *WSH [End]
  
ELSE
  
  *WSH [Start]
  *SET ORDER TO
  gfSETORDER()
  *WSH [Start]
  
ENDIF  
IF llRetAlias
  SELECT(lnCurAlias)
ENDIF
RETURN(XRANO)

*!*************************************************************
*! Name      : gfCurrBrow
*! Developer : RENEE - Renee Ezzat
*! Date      : 12/27/1995
*! Purpose   : Global currency code fields validation
*!*************************************************************
*! Calls     : gfBrow()
*!*************************************************************
*! Passed Parameters  : lcCurrCode : pointer to currency code field
*!                      lcCurrDesc : pointer to currency description
*!                                   variable (optional) 
*!*************************************************************
*! Returns            :  .T. If a valid currency code is selected,
*!                       .F. otherwise
*!*************************************************************
*! Example            :  =gfCurrBrow(@lcCurrCode, @lcCurrDesc)
*!*************************************************************
FUNCTION gfCurrBrow
PARAMETERS lcCurrCode, lcCurrDesc
LOCAL llWasSel, lcAlias
lcCurrCode = PADR(ALLTRIM(lcCurrCode),5)
DECLARE laTmpVal[2]
laTmpVal[1] = 'cCurrCode'
laTmpVal[2] = 'cCurrDesc'
*B00000,1 ASM Apply Aria Grid in the Currency Selection [Start]
*!*  IF gfBrow('SYCCURR', 'CCURRCODE', lcCurrCode, @laTmpVal,;
*!*            [cCurrCode:8:H='Currency',cCurrDesc:30:H='Description',;
*!*             nCurrUnit:5:H='Units',cCurrSmbl:7:H='Symbol'],;
*!*             'Currencies', 56)  
*!*    lcCurrCode = laTmpVal[1]
*!*    lcCurrDesc = laTmpVal[2]    
*!*  ELSE
*!*    lcCurrCode = SPACE(5)
*!*    lcCurrDesc = SPACE(30)
*!*  ENDIF
lcAlias = Alias()
SELECT SYCCURR
lcBrFields = "cCurrCode:8:H='Currency',"+;
             "cCurrDesc:30:H='Description',"+;                                   
             "nCurrUnit:5:H='Units',"+;
             "cCurrSmbl:7:H='Symbol'"

*B999999,1 AMH Fix bug of no records to display [Start]
*llWasSel= ARIABROW(IIF(EMPTY(lcCurrCode),.F.,[lcCurrCode]),'Currencies',gnBrFSRow1, ;
   gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,"","","cCurrCode,cCurrDesc","laTmpVal")
llWasSel= ARIABROW(.F.,'Currencies',gnBrFSRow1,gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,"","","cCurrCode,cCurrDesc","laTmpVal")
*B999999,1 AMH [End]

IF llWasSel
  lcCurrCode = laTmpVal[1]
  lcCurrDesc = laTmpVal[2]    
ELSE
  lcCurrCode = SPACE(5)
  lcCurrDesc = SPACE(30)
ENDIF
IF !EMPTY(lcAlias)
  SELECT (lcAlias)
ENDIF

RETURN llWasSel
*B00000,1 [End]


*!*************************************************************
*! Name      : gfBrow
*! Developer : RENEE - Renee Ezzat
*! Date      : 12/27/1995
*! Purpose   : No button global browse
*!*************************************************************
*! Calls     : lcCurrCode
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  IF gfBrow('SYCCURR', 'CCURRCODE',;
*!                                  lcCurrCode, @laTmpVal,;
*!                                  lcBrFlds, lcBrTtl, 15, 54)  
*!*************************************************************
*: Modifications : 
*! E037885,2 MAH 12/03/2004 Separate screen in different session.
*:************************************************************************
FUNCTION gfBrow
PARAMETERS lcSeekfile, lcSeekTag, lcSeekExp, laRetVal, lcBrFields, lcBrTitle,lnBrCols
PRIVATE lcCurFlt, lcBrWinName, lnCurAlias, lnCurTag, lnCurRec, llRetVal,;
        llEntered, lcExpToSeek, laKeyExp 

lnCurAlias = SELECT()
SELECT (lcSeekFile)
lcCurFlt = FILTER()
lnCurTag = VAL(SYS(21))
IF DELETED()
  SKIP 
ENDIF  
lnCurRec = IIF(EOF() .OR. BOF(), 0, RECNO())

SET FILTER TO
SET ORDER TO TAG (lcSeekTag)

llBrowse =  IIF(TYPE('llBrowse') = 'U', .F., llBrowse) .OR. '?' $ lcSeekExp
GO TOP
llEntered = .F.
IF EOF()
  =gfDialog('I', 'There are no records to browse.')
ELSE
  IF !llBrowse 
    IF !SEEK(lcSeekExp)
      IF BETWEEN(RECNO(0), 1, RECCOUNT())
        GO RECNO(0)
      ELSE
        GO TOP
      ENDIF    
      llBrowse = .T.
    ELSE
      llEntered = .T.
    ENDIF  
  ENDIF
  IF llBrowse 
    PUSH KEY
    =gfClearKey()
    DIMENSION laKeyExp[1]
    STORE 0 TO lnX,lnY,lnCurR,lnTimelimt,lnSeeklimt,lnSelRec
    STORE .F. TO llClick
    STORE [""] TO laKeyExp
    
    lcBrTitle   = IIF(EMPTY(lcBrTitle), PROPER(ALIAS()), lcBrTitle)
    lcBrWinName = gfTempName()

    *! E037885,2 MAH 12/03/2004 Deine in top level window [BEGIN]
    *-- DEFINE WINDOW (lcBrWinName);
    *--        AT 0,0 SIZE 2*SROW()/3, lnBrCols;
    *--        FONT "Tahoma", 9 ;
    *--        FLOAT ;
    *--        NOCLOSE ;
    *--        SHADOW ;
    *--        NOMINIMIZE ;
    *--        SYSTEM ;
    *--        COLOR SCHEME 10  	            
    LOCAL loHostForm
    loHostForm = gfGetTopLevelFrom()
    
    IF TYPE('loHostForm.Name') # 'C' .OR. UPPER(loHostForm.Name) == 'SCREEN'
      DEFINE WINDOW (lcBrWinName);
             AT 0,0 SIZE 2*SROW()/3, lnBrCols ;
             FONT "Tahoma", 9 ;
             FLOAT ;
             NOCLOSE ;
             SHADOW ;
             NOMINIMIZE ;
             SYSTEM ;
             COLOR SCHEME 10  	            
    ELSE
      DEFINE WINDOW (lcBrWinName);
             AT 0,0 SIZE 2*SROW()/3, lnBrCols ;
             IN (loHostForm.Name) ;
             FONT "Tahoma", 9 ;
             FLOAT ;
             NOCLOSE ;
             SHADOW ;
             NOMINIMIZE ;
             SYSTEM ;
             COLOR SCHEME 10  	            
    ENDIF
    *! E037885,2 MAH 12/03/2004 [END]

    MOVE WINDOW (lcBrWinName) CENTER   
  
    ON KEY LABEL ENTER DO lpSelOnEnt  WITH lcBrTitle, llEntered    
    ON KEY LABEL LEFTMOUSE  DO lfCdChkDcl WITH lcBrTitle, llEntered   
    lcExpToSeek="" 
    lcOrdExpr   = SYS(14, EVAL(SYS(21)))
    IF TYPE(lcOrdExpr) = 'N'
      lnStartTrap = 48
      lnEndTrap   = 57
    ELSE
      lnStartTrap = 32
      lnEndTrap   = 126
    ENDIF  
    FOR lnChrToTrap = lnStartTrap TO lnEndTrap
      ON KEY LABEL (CHR(lnChrToTrap)) DO lfcdchIncS
    ENDFOR
    
    *! E037885,2 MAH 12/03/2004 Deine in top level window [BEGIN]
    *-- BROWSE FIELDS &lcBrFields;
    *--        WINDOW (lcBrWinName);
    *--        LOCK 0;
    *--        NOMENU;         
    *--        NOAPPEND;
    *--        NOEDIT;
    *--        NODELETE;
    *--        TITLE lcBrTitle 
    IF TYPE('loHostForm.Name') # 'C' .OR. UPPER(loHostForm.Name) == 'SCREEN'
      BROWSE FIELDS &lcBrFields;
             WINDOW (lcBrWinName);
             LOCK 0;
             NOMENU;         
             NOAPPEND;
             NOEDIT;
             NODELETE;
             TITLE lcBrTitle
    ELSE
      BROWSE FIELDS &lcBrFields;
             WINDOW (lcBrWinName);
             LOCK 0;
             NOMENU;         
             NOAPPEND;
             NOEDIT;
             NODELETE;
             TITLE lcBrTitle IN WINDOW (loHostForm.Name)
    ENDIF
    *! E037885,2 MAH 12/03/2004 [END]
    POP KEY

    IF llEntered .AND. BETWEEN(lnSelRec, 1, RECCOUNT())
      GO lnSelRec
    ENDIF
    WAIT CLEAR
    RELEASE WINDOW (lcBrWinName)
  ENDIF
ENDIF

*E300329,1 If the browse is called from the last field before a menu
*E300329,1 created by MENU TO command, and a selection is done using
*E300329,1 a double mouse click, an extra mouse click needs to be 
*E300329,1 consumed so that the cursor waits for a menu selection
=inkey(0.001)

*E300329,1 If selected,
IF llEntered
  FOR lnCount = 1 TO ALEN(laRetVal)
    laRetVal[lnCount] = EVALUATE(lcSeekFile + '.' + laRetVal[lnCount])
  ENDFOR  
ENDIF
llBrowse = .F.
*E300329,1 Restore environmet

SELECT (lcSeekFile)
SET ORDER TO TAG (lnCurTag)
IF !EMPTY(lcCurFlt)
  SET FILTER TO
ENDIF
IF lnCurRec <> 0
  GO lnCurRec
ELSE
  GO TOP
ENDIF  
SELECT (lnCurAlias)
RETURN llEntered

*!*************************************************************
*! Name      : gfClearKey
*! Developer : RENEE - Renee Ezzat
*! Date      : 05/25/1995
*! Purpose   : Resets any keys then sets global key traps.
*!*************************************************************
*! Example            :  =gfClearKey()
*!*************************************************************
FUNCTION gfClearKey
ON KEY
ON KEY LABEL F4 KEYBOARD '?'+'{ENTER}'
ON KEY LABEL F1 DO lpPressF1

*:************************************************************************
*: Program file  : lpSelOnEnt
*: Program desc. : Selects a code upon presssing ENTER in CodeChk browse
*: For screen    :
*:         System: 
*:         Module: Aria Apparel System
*:      Developer: Renee Ezzat
*:************************************************************************
*: Example :
*:        ON KEY LABEL ENTER DO lpSelOnEnt
*:*************************************************************
PROCEDURE lpSelOnEnt
PARAMETERS lcBrName, llExit

ON KEY

*B600480,1 YI on 06/20/95 save the selected record number
IF TYPE('lnSelRec')='N'
  lnSelRec = RECNO()
ENDIF

DEACTIVATE WINDOW  (lcBrName)
llExit = .T.


*!******************************************************************
*!
*!              Function: lfCdChkDcl
*!
*!******************************************************************
*
FUNCTION lfCdChkDcl
PARAMETERS lcBrName,llExit
lnX =INT(MROW())
lnY =INT(MCOL())
*B600322 if the user pressed double click with the mouse in any of the
*B600322 scroll bars "Horz.,Vert." ignore it and dont select the active
*B600322 record, we added the checking if the MROW AND THE MCOL inside
*B600322 the browse window with out taking the position of the scroll
*B600322 bars in out consideration

IF  (MROW(lcBrName)<>-1) AND (MCOL(lcBrName)<>-1) AND (MROW(lcBrName)>IIF(_DOS OR _UNIX,2,1.77));
     AND BETWEEN(MCOL(lcBrName),2,WCOL(lcBrName)-IIF(_DOS OR _UNIX,2,1.77));
     AND BETWEEN(MROW(lcBrName),2,WROWS(lcBrName)-IIF(_DOS OR _UNIX,2,1.77))

*    AND MCOL(xTitle)<(WCOLS(xTitle)+WLCOL(xTitle)-IIF(_DOS OR _UNIX,2,3.77));
    AND MROW(xTitle)<(WROWS(xTitle)+WLROW(xTitle)-IIF(_DOS OR _UNIX,2,1.77))

  * if leftmouse wasn't pressed  previously 
  IF !llClick
    lnTimelimt = SECONDS()  
    lnCurR     = lnX
    llClick    = .T.
  ELSE
  * else if leftmouse was pressed  previously   
    * if the second click of the mouse is in the time limit and clicking
    * on the same row
    IF SECONDS() < lnTimelimt + _DBLCLICK .AND. lnCurR  = INT(MROW()); 
       AND  lnX=lnCurR 
      
      lnSelRec   = RECNO()
      llClick    = .F.

      ON KEY
      DEACTIVATE WINDOW  (lcBrName)
      llExit = .T.
   
      *KEYBOARD lcKeyLable CLEAR  
    ENDIF
    IF lnCurR    = INT(MROW())
      lnTimelimt = SECONDS()  
    ELSE
      llClick    = .F.
    ENDIF   
  ENDIF
ELSE
  llClick    = .F.
ENDIF

*!**************************************************************************
*!
*!              Function: lfcdchIncS
*!
*!**************************************************************************
*
*!B600363,1 function added for inc. search.
FUNCTION lfcdchIncS
PRIVATE lnBrRecNO
* if the last key pressed time = 0 get the new time
IF lnSeeklimt = 0
  lnSeeklimt = SECONDS()  
ENDIF  
* if time in the limit of the last key pressed time and the dblclick time
IF SECONDS() < lnSeeklimt + _DBLCLICK 
  lnSeeklimt = SECONDS()  
  lnBrRecNO = IIF(RECNO()>RECCOUNT(),0,RECNO())
  * loop through the key used for the browse
  FOR lnCount = 1 TO ALEN(laKeyExp,1)
    * if seek of key+keypressed
    IF SEEK(&lakeyExp[lnCount]+lcExpToSeek+UPPER(CHR(LASTKEY())))
      * add the keypressed to the exptoseek
      lcExpToSeek = lcExpToSeek+UPPER(CHR(LASTKEY()))
      WAIT lcExpToSeek WINDOW NOWAIT                
      RETURN
    ELSE
    *else if !seek of key+keypressed return to the current record
      IF lnBrRecNO>0
        GO lnBrRecNO
      ENDIF  
    ENDIF
  ENDFOR
ELSE
* else if time not in the limit of the last key pressed time and the dblclick time
  lnSeeklimt = 0
  lcExpToSeek=''
  =lfcdchIncS()
ENDIF


*!**********************************************************************
*! Name      : gfOTSDisp
*! Developer : WAB - WALID A. WAHAB
*! Date      : 11/24/1999
*! Purpose   : Function to show the open Qty To Sell.
*!**********************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : 1--> lcStyle	   style code 
*!                      2--> lcWareHouse   ware house code
*!                      3--> llAllWareHs ----(all ware house,warehouse)
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
*N000408,1 Reham 07/2002 Convert to work under Aria 4 version
*B000000,1 HBG 01/30/2005 Add parameter to Get the OTS on Configuration level if use configuration 
*!************************************************************************
FUNCTION gfOTSDisp

*B038253,1 AMH Add parameter for dyelot [Start]
*PARAMETER lcStyle,lcWareHouse,llAllWareHs
*B000000,1 HBG 01/30/2005 Add parameter to Get the OTS on Configuration level if use configuration [Begin]
*PARAMETER lcStyle,lcWareHouse,llAllWareHs,lcDyelot

*B132139,1 WSH 05/18/2006 Add extended Scale parameter to get Open To Sell for it. [Start]
PARAMETER lcStyle,lcWareHouse,llAllWareHs,lcDyelot,llUseConfig
*PARAMETER lcStyle,lcWareHouse,llAllWareHs,lcDyelot,llUseConfig,lcEXScale
*B132139,1 WSH 05/18/2006 [End]

IF TYPE('llUseConfig') = 'U'
  llUseConfig = .F.
ENDIF
*B000000,1 [End]
lcDyelot = IIF(TYPE('lcDyelot')#'C' OR EMPTY(lcDyelot),SPACE(10),lcDyelot)
*B038253,1 AMH [End]

PRIVATE lcBrFields,lnAlias,llMFInstld,llSOInstld,llPOInstld,;
        lctmpOTS,lcEngland,llOtsinfo,lcexkey,lcwcondt,lcNMjrTl,lcMjrPct,;
        lcNMjrPt,lnstylewid,lncolorwid,llAllClrs

*-- Include the .H file
#INCLUDE R:\ARIA4XP\PRGS\SY\gfOTSDisp.H

*-- Get the "O.T.S. period ranges" setup if weekly, 2 weeks or monthly
lcOtsPrd   = ""
lcOtsPrd   = gfGetMemVar('M_OTSPRIOD')

*-- Get the setting of OTS based on exact transaction date "J&L"
llOTSbasTr = .F.
llOTSbasTr = gfGetMemVar('M_OTSBASTR',gcAct_Comp)
llOTSbasTr = IIF(TYPE("llOTSbasTr")$"UC" , .F. , llOTSbasTr)

*-- Get the style code info.
lcMjrPct   = gfItemMask('PM')  && Major Picture
lcNMjrPt   = gfItemMask('PN')  && Non-Major Picture
lcNMjrTl   = gfItemMask('HN')  && Non-Major Title
lnstylewid = LEN(lcMjrPct)     && Major Length
lncolorwid = LEN(lcNMjrPt)     && Non-Major Length
llAllClrs  = EMPTY(SUBSTR(lcStyle,lnStyleWid+2)) && If display all colors

STORE .F. TO llMFInstld , llSOInstld , llPOInstld
*-- Flag hold if the MF module is installed or not
llMFInstld = (OCCURS('MF',oAriaApplication.CompanyInstalledModules)<>0)
*-- Flag hold if the SO module is installed or not
llSOInstld = (OCCURS('SO',oAriaApplication.CompanyInstalledModules)<>0)
*-- Flag hold if the PO module is installed or not
llPOInstld = (OCCURS('PO',oAriaApplication.CompanyInstalledModules)<>0)

*-- Assign temp names to variables
lctmpOTS   = gftempname()  && Temp name hold the OTS qty. cursor.
lcPoHdrTmp = gftempname()  && Temp name hold the PO header alias.
lcPolinTmp = gftempname()  && Temp name hold the PO line alias.
lcShpHdTmp = gftempname()  && Temp name hold the shipment header alias.
lcCutHdTmp = gftempname()  && Temp name hold the cutting ticket header alias.
lcCutlnTmp = gftempname()  && Temp name hold the cutting ticket line alias.
lcOrdHdTmp = gftempname()  && Temp name hold the order header alias.
lcOrdLnTmp = gftempname()  && Temp name hold the order line alias.
lcStyleTmp = gftempname()  && Temp name hold the style alias.
lcStyDyTmp = gftempname()  && Temp name hold the style dyelot alias.
lcScaleTmp = gftempname()  && Temp name hold the scale alias.

lcEngland  = 'ENG'

*-- Hold the for condition based on warehouse setup "Multiple or single"
lcfcondt = IIF(!llAllWareHs,'cWareCode = lcWareHouse','.T.')

*-- Check if the  PO, MF & SO are not installed, return from this function.
IF !llPOInstld AND !llMFInstld AND !llSOInstld
  RETURN
ENDIF

*-- Save current work area
lnAlias=SELECT()

*-- Assign flag to know if style file opened in this function or not
llOpnStyle = .F.

*B132139,1 WSH 05/18/2006 [Start]
*llOpnStyle = gfOpenFile(oAriaApplication.DataDir+"STYLE", "STYLE", "SH",@lcStyleTmp,.T.)
llOpnStyle = gfOpenTable(oAriaApplication.DataDir+"STYLE", "STYLE", "SH",@lcStyleTmp,.T.)
*B132139,1 WSH 05/18/2006 [End]

*-- Checking existance of OTS information.

*B132139,1 WSH 05/18/2006 [Start]
*=SEEK(lcStyle) 
SELECT (lcStyleTmp)
=gfSEEK(lcStyle)
*B132139,1 WSH 05/18/2006 [End]

llOtsinfo = .F.  && Assign flag to know if there is OTS data to be displayed

*-- Check if only one color has data, stop checking.
SCAN WHILE Style = lcStyle
  FOR I=1 TO 8
    Z=STR(I,1)
    *-- Check if there is data for the current style "Stock, WIP or Order" qty.
    IF Stk&z<>0 OR Wip&z<>0 OR Ord&z<>0
      *-- Set the OTS info. flag to true & exit from this loop
	  llOtsinfo = .T.
  	EXIT
    ENDIF
  ENDFOR
  *-- If only one color has data, stop checking.
  IF llOtsinfo
    EXIT
  ENDIF
ENDSCAN

*-- Check the OTS info. flag, if no data to display, return from this function
IF !llOtsinfo
  *-- No Open to sell information found.
  =gfModalGen('TRM42045B42001','DIALOG')
  *-- Close the style file if opened in this function.
  IF llOpnStyle
    
    *B132139,1 WSH 05/18/2006 [Start]
    *USE IN (lcStyleTmp)
    gfCloseTable(lcStyleTmp)
    *B132139,1 WSH 05/18/2006 [End]
    
  ENDIF
  RETURN
ENDIF

*-- Assign flag to know if open files in this program or not
STORE .F. TO llOpnScale, llOpnStyDye, llOpnPoHdr, llOpnPoLin, llOpnShpHdr, llOpnCutHdr, llOpnCutLin, llOpnOrdHdr, llOpnOrdLin

*-- Open the needed files & set the opened flags to true if opened in this function

*B132139,1 WSH 05/18/2006 [Start]
*llOpnScale = gfOpenFile(oAriaApplication.DataDir+"SCALE", "SCALE", "SH",@lcScaleTmp,.T.)
*llOpnStyDye = gfOpenFile(oAriaApplication.DataDir+"STYDYE", "STYDYE", "SH",@lcStyDyTmp,.T.)
*IF llPOInstld 
*  llOpnPoHdr  = gfOpenFile(oAriaApplication.DataDir+'POShdr','POSHdr'  ,'SH',@lcPoHdrTmp,.T.)
*  llOpnPoLin  = gfOpenFile(oAriaApplication.DataDir+'POSLN' ,'POSLNS'  ,'SH',@lcPolinTmp,.T.)
*  llOpnShpHdr = gfOpenFile(oAriaApplication.DataDir+'Shpmthdr','Shpmthdr','SH',@lcShpHdTmp,.T.)
*ENDIF
*IF llMFInstld 
*  llOpnCutHdr = gfOpenFile(oAriaApplication.DataDir+"CUTTKTH", "CUTTKTH", "SH",@lcCutHdTmp,.T.)
*  llOpnCutLin = gfOpenFile(oAriaApplication.DataDir+"CUTTKTL", "CUTTKTLS", "SH",@lcCutlnTmp,.T.)
*ENDIF
*IF llSOInstld
*  llOpnOrdHdr = gfOpenFile(oAriaApplication.DataDir+"ORDHDR", "ORDHDR", "SH",@lcOrdHdTmp,.T.)
*  llOpnOrdLin = gfOpenFile(oAriaApplication.DataDir+"ORDLINE", "ORDLINES", "SH",@lcOrdLnTmp,.T.)
*ENDIF
llOpnScale  = gfOpenTable(oAriaApplication.DataDir+"SCALE", "SCALE", "SH",@lcScaleTmp,.T.)
llOpnStyDye = gfOpenTable(oAriaApplication.DataDir+"STYDYE", "STYDYE", "SH",@lcStyDyTmp,.T.)
IF llPOInstld OR llMFInstld
  llOpnPoHdr  = gfOpenTable(oAriaApplication.DataDir+'POShdr','POSHdr'  ,'SH',@lcPoHdrTmp,.T.)
  llOpnPoLin  = gfOpenTable(oAriaApplication.DataDir+'POSLN' ,'POSLNS'  ,'SH',@lcPolinTmp,.T.)
  IF llPOInstld
    llOpnShpHdr = gfOpenTable(oAriaApplication.DataDir+'Shpmthdr','Shpmthdr','SH',@lcShpHdTmp,.T.)
  ENDIF
ENDIF
IF llSOInstld
  llOpnOrdHdr = gfOpenTable(oAriaApplication.DataDir+"ORDHDR", "ORDHDR", "SH",@lcOrdHdTmp,.T.)
  llOpnOrdLin = gfOpenTable(oAriaApplication.DataDir+"ORDLINE", "ORDLINES", "SH",@lcOrdLnTmp,.T.)
ENDIF
*B132139,1 WSH 05/18/2006 [End]

*-- restore the work area
SELECT (lnAlias)
*--Save current pointers since this function will change it.
lnCrSav1=IIF(!EOF(),RECNO(),0)

WAIT WINDOW 'Collecting Open to Sell information...' NOWAIT

*-- Create cursor hold the OTS qty. that will be displayed.
CREATE CURSOR (lctmpots) (Style C(19),SZCnt C(1) ,Size C(5) ,;
                          nqty1 N(7),nqty2 N(7),nqty3 N(7),nqty4 N(7),;
                          nqty5 N(7),nqty6 N(7),nqty7 N(7),nqty8 N(7),;
                          nqty9 N(7),nqty10 N(7),nqty11 N(7),nqty12 N(7))

*B038253,1 AMH Create index to use it for privent dublicate records [Start]
*B609356,1 SMA 07/27/2010 remove of clause to prevent empty *.cdx files from creation.....[BEGIN]
*INDEX ON Style+Size TAG (lcTmpOTS) OF (lcTmpOTS)
INDEX ON Style+Size TAG (lcTmpOTS)
*B609356,1 SMA 07/27/2010 remove of clause to prevent empty *.cdx files from creation.....[END]
*B038253,1 AMH [End]

*-- If display all warehouses, display data from Style file, if display specific 
*-- warehouse, display data from style dyelot file for this warehouse.
SELECT IIF(llAllWareHs,lcStyleTmp,lcStyDyTmp)

*-- Set while clause condition
lcWCondt = 'Style = lcStyle'

*-- Set for clause condition
*! N119687,1 WSH [Start] 04/27/2004 Fix Bug in Open To Sell Browse gfOTSDisp()
*lcFCondt = IIF(!llAllWareHs,'cWareCode = lcWareHouse','.T.')

*B038253,1 AMH Consider the dyelot [Start]
*lcFCondt = IIF(!llAllWareHs,'cWareCode = lcWareHouse AND EMPTY(dyelot)','.T.')
lcFCondt = IIF(!llAllWareHs,'cWareCode = lcWareHouse AND dyelot = lcDyelot','.T.')
*B038253,1 AMH [End]

*! N119687,1 WSH [End] 04/27/2004 Fix Bug in Open To Sell Browse gfOTSDisp()

*-- Scan for the current style to get all the style records 

*B132139,1 WSH 05/18/2006 [Start]
*=SEEK(lcStyle)
=gfSEEK(lcStyle)
*B132139,1 WSH 05/18/2006 [End]

SCAN WHILE &lcwcondt FOR &lcfcondt
	IF !llAllWareHs
	  =SEEK(&lcStyDyTmp..Style,lcStyleTmp)
	ENDIF
	*-- Get the style non-major for the current style.
  lcColor = SUBSTR(Style,lnStyleWid+1,lnColorWid+1)
  *-- Get the scale record for the current style.
  
  *B132139,1 WSH 05/18/2006 [Start]
  *=SEEK('S'+&lcStyleTmp..Scale,lcScaleTmp)
  =gfSEEK('S'+&lcStyleTmp..Scale,lcScaleTmp)
  *B132139,1 WSH 05/18/2006 [End]
  
	*-- Add record for each style /size in the OTS cursor.
	SELECT (lcTmpOts)
	FOR I = 1 TO &lcScaleTmp..cnt
		z = STR(I,1)
    
    *B038253,1 AMH Check if the record exist [Start]
    IF SEEK(EVALUATE(lcStyleTmp+'.Style')+EVALUATE(lcScaleTmp+'.sz'+z))
      LOOP
    ENDIF
    *B038253,1 AMH [End]
    
		APPEND BLANK
		REPLACE Style WITH &lcStyleTmp..Style,;
                SZCnt WITH Z,;
			    Size  WITH &lcScaleTmp..sz&z
	ENDFOR
ENDSCAN
*-- Create index on style + size in the OTS cursor
SELECT (lcTmpOts)
*B609356,1 SMA 07/27/2010 remove of clause to prevent empty *.cdx files from creation.....[BEGIN]
*INDEX ON Style+SZCnt TAG (lctmpots) OF (lctmpots)
INDEX ON Style+SZCnt TAG (lctmpots)
*B609356,1 SMA 07/27/2010 remove of clause to prevent empty *.cdx files from creation.....[END]

*-- Initialize the necessary global variables.
ldToday = oAriaApplication.SystemDate
lcThisM = ALLTRIM(STR(MONTH(ldToday)))  && Hold the current system date month
lcThisY = ALLTRIM(STR(YEAR(ldToday)))   && Hold the current system date year
lcThisD = ALLTRIM(STR(DAY(ldToday)))    && Hold the current system date day

*-- Array hold no. of days in each month.
DECLARE  laNoOfDays[12]
laNoOfDays = '31'
*-- Store 30 days in the following months "April, June, September & November"
STORE '30' TO laNoOfDays[4],laNoOfDays[6],laNoOfDays[9],laNoOfDays[11]

*-- Array hold "From - To" dates up to 10 periods.
DECLARE laDtPeriod[10,3]
laDtPeriod = {}                 && Set value of empty date type in the period array
lcPrdMonth = lcThisM            && Store Current month value in the period month
lnPrdMonth = INT(VAL(lcThisM))  && Store no. of the current month in the period month #
lcPrdYear  = lcThisY            && Store Current yesr value in the period yesr

*-- Read date format & change it to England if the country is England or date setting is British.
llEngDate = (ALLTRIM(oAriaApplication.DefaultCountry) = lcEngland .OR. SET('DATE')='BRITISH' )

*-- First default for the no. of periods will be 10
lnPrdNum = 10
*-- IF the OTS range setting is "2 Weeks" & the current day at the first half of the month
IF lcOtsPrd='E' AND VAL(lcThisD)<=15  
  *-- Set no. of periods to 11
  lnPrdNum = 11
ENDIF

*-- If the OTS range setting is "Weekly"
IF lcOtsPrd = 'W'
  DO CASE
    *-- If the current date day in the second week of the month.
    CASE BETWEEN(VAL(lcThisD),8,15)
      lnPrdNum = 11
    *-- If the current date day in the third week of the month.
    CASE BETWEEN(VAL(lcThisD),16,22)
      lnPrdNum = 12
    *-- If the current date day in the forth week of the month.
    CASE BETWEEN(VAL(lcThisD),22,31)
      lnPrdNum = 13
  ENDCASE
ENDIF

*-- Define the Period array with the final no. of periods & define type empty date to its value.
DECLARE laDtPeriod[lnPrdNum,3]
laDtPeriod = {}

*-- Loop from first period to the total no. of periods
FOR I = 1 TO lnPrdNum
  *-- Variable hold the string of the current period no.
  lcPrdCnt = PADL(ALLTRIM(STR(I,2)),2,"0")
  DO CASE
    *-- Case OTS range is every 2 weeks.
    CASE lcOtsPrd = 'E'
     IF lcPrdCnt $ '01-03-05-07-09-11'  && First part of month.
       *-- If date setting is England, use the british format
       IF llEngDate
         laDtPeriod[I,1] = CTOD('01/'+lcPrdMonth+'/'+lcPrdYear)           && Hold Period start date
         laDtPeriod[I,2] = CTOD('15/'+lcPrdMonth+'/'+lcPrdYear)           && Hold period end date
       *-- If date setting is not England, use the normal format
       ELSE
         laDtPeriod[I,1] = CTOD(lcPrdMonth+'/01/'+lcPrdYear)              && Hold Period start date
         laDtPeriod[I,2] = CTOD(lcPrdMonth+'/15/'+lcPrdYear)              && Hold Period end date
       ENDIF
       *-- Hold the title of the period "ex.: JAN. 01-15"
       laDtPeriod[I,3] = SUBSTR(CMONTH(laDtPeriod[I,1]),1,3)+'. 01-15'
     *-- Case the value of lcPrdCnt $ '02-04-06-08-10-12'
     ELSE
       *-- If date setting is England, use the british format
       IF llEngDate
         laDtPeriod[I,1] = CTOD('16/'+lcPrdMonth+'/'+lcPrdYear)              && Hold the period start date
         laNoOfDays[2]   = IIF(MOD(YEAR(laDtPeriod[I,1]),4)=0 ,'29' ,'28' )  && Adjust Feb. no. of days depend on the current year value.
         laDtPeriod[I,2] = CTOD(ALLTRIM(laNoOfDays[lnPrdMonth])+'/'+lcPrdMonth+'/'+lcPrdYear)
       *-- If date setting is not England, use the normal format
       ELSE
         laDtPeriod[I,1] = CTOD(lcPrdMonth+'/16/'+lcPrdYear)                 && Hold the period start date
         laNoOfDays[2]   = IIF(MOD(YEAR(laDtPeriod[I,1]),4)=0 ,'29' ,'28' )  && Adjust Feb. no. of days depend on the current year value.
         laDtPeriod[I,2] = CTOD(lcPrdMonth+'/'+ALLTRIM(laNoOfDays[lnPrdMonth])+'/'+lcPrdYear)
       ENDIF
       *-- Hold the title of the period "ex.: FEB. 16-31"
       laDtPeriod[I,3] = SUBSTR(CMONTH(laDtPeriod[I,1]),1,3)+'. 16-'+ALLTRIM(laNoOfDays[lnPrdMonth])
       *-- Assign new date value by adding one day to the current period end date
       ldNewDate  = (laDtPeriod[I,2]+1)
       lcPrdMonth = ALLTRIM(STR(MONTH(ldNewDate))) && Get period month name
       lnPrdMonth = INT(VAL(lcPrdMonth))           && Get period month #
       lcPrdYear  = ALLTRIM(STR(YEAR(ldNewDate)))  && Get period year
     ENDIF
    *-- Case OTS range is weekly.
    CASE lcOtsPrd = 'W'
      DO CASE
        CASE lcPrdCnt $ '01-05-09-13'  && First week.
         *-- If date setting is England, use the british format
         IF llEngDate
           laDtPeriod[I,1] = CTOD('01/'+lcPrdMonth+'/'+lcPrdYear)  && Hold the period start date
           laDtPeriod[I,2] = CTOD('07/'+lcPrdMonth+'/'+lcPrdYear)  && Hold the period end date
         *-- If date setting is not England, use the normal format
         ELSE
           laDtPeriod[I,1] = CTOD(lcPrdMonth+'/01/'+lcPrdYear)     && Hold the period start date
           laDtPeriod[I,2] = CTOD(lcPrdMonth+'/07/'+lcPrdYear)     && Hold the period end date
         ENDIF
         *-- Hold the title of the period "ex.: FEB. 01-07"
         laDtPeriod[I,3] = SUBSTR(CMONTH(laDtPeriod[I,1]),1,3)+'. 01-07'
        CASE lcPrdCnt $ '02-06-10'  && Second Week
          *-- If date setting is England, use the british format
          IF llEngDate
            laDtPeriod[I,1] = CTOD('08/'+lcPrdMonth+'/'+lcPrdYear) && Hold the period start date
            laDtPeriod[I,2] = CTOD('15/'+lcPrdMonth+'/'+lcPrdYear) && Hold the period end date
          *-- If date setting is not England, use the normal format
          ELSE
            laDtPeriod[I,1] = CTOD(lcPrdMonth+'/08/'+lcPrdYear)    && Hold the period start date
            laDtPeriod[I,2] = CTOD(lcPrdMonth+'/15/'+lcPrdYear)    && Hold the period end date
          ENDIF
          *-- Hold the title of the period "ex.: FEB. 08-15"
          laDtPeriod[I,3] = SUBSTR(CMONTH(laDtPeriod[I,1]),1,3)+'. 08-15'
        CASE lcPrdCnt $ '03-07-11'  && Third Week
          *-- If date setting is England, use the british format
          IF llEngDate
            laDtPeriod[I,1] = CTOD('16/'+lcPrdMonth+'/'+lcPrdYear)  && Hold the period start date
            laDtPeriod[I,2] = CTOD('22/'+lcPrdMonth+'/'+lcPrdYear)  && Hold the period end date
          *-- If date setting is not England, use the normal format
          ELSE
            laDtPeriod[I,1] = CTOD(lcPrdMonth+'/16/'+lcPrdYear)     && Hold the period start date
            laDtPeriod[I,2] = CTOD(lcPrdMonth+'/22/'+lcPrdYear)     && Hold the period end date
          ENDIF
          *-- Hold the title of the period "ex.: FEB. 16-22"
          laDtPeriod[I,3] = SUBSTR(CMONTH(laDtPeriod[I,1]),1,3)+'. 16-22'
        CASE lcPrdCnt $ '04-08-12'  && Forth week
          *-- If date setting is England, use the british format
          IF llEngDate
            laDtPeriod[I,1] = CTOD('23/'+lcPrdMonth+'/'+lcPrdYear)  && Hold the period start date
            laNoOfDays[2]   = IIF(MOD(YEAR(laDtPeriod[I,1]),4)=0 ,'29' ,'28' )  && Adjust Feb. no. of days depend on the current year value.
            laDtPeriod[I,2] = CTOD(ALLTRIM(laNoOfDays[lnPrdMonth])+'/'+lcPrdMonth+'/'+lcPrdYear)  && Hold the period end date
          *-- If date setting is not England, use the normal format
          ELSE
            laDtPeriod[I,1] = CTOD(lcPrdMonth+'/23/'+lcPrdYear)     && Hold the period start date
            laNoOfDays[2]   = IIF(MOD(YEAR(laDtPeriod[I,1]),4)=0 ,'29' ,'28' )  && Adjust Feb. no. of days depend on the current year value.
            laDtPeriod[I,2] = CTOD(lcPrdMonth+'/'+ALLTRIM(laNoOfDays[lnPrdMonth])+'/'+lcPrdYear)  && Hold the period end date
          ENDIF
          *-- Hold the title of the period "ex.: FEB. 23-31"
          laDtPeriod[I,3] = SUBSTR(CMONTH(laDtPeriod[I,1]),1,3)+'. 23-'+ALLTRIM(laNoOfDays[lnPrdMonth])
          *-- Assign new date value by adding one day to the current period end date
          ldNewDate  = (laDtPeriod[I,2]+1)
          lcPrdMonth = ALLTRIM(STR(MONTH(ldNewDate)))   && Get period month name
          lnPrdMonth = INT(VAL(lcPrdMonth))             && Get period month #
          lcPrdYear  = ALLTRIM(STR(YEAR(ldNewDate)))    && Get period year
      ENDCASE
    *-- Case OTS range is monthly.
    CASE lcOtsPrd = 'M'
      *-- If date setting is England, use the british format
      IF llEngDate
        laDtPeriod[I,1] = CTOD('01/'+lcPrdMonth+'/'+lcPrdYear)               && Hold the period start date
        laNoOfDays[2]   = IIF(MOD(YEAR(laDtPeriod[I,1]),4)=0 ,'29' ,'28' )   && Adjust Feb. no. of days depend on the current year value.
        laDtPeriod[I,2] = CTOD(ALLTRIM(laNoOfDays[lnPrdMonth])+'/'+lcPrdMonth+'/'+lcPrdYear)  && Hold the period end date
      *-- If date setting is not England, use the normal format
      ELSE
        laDtPeriod[I,1] = CTOD(lcPrdMonth+'/01/'+lcPrdYear)                  && Hold the period start date
        laNoOfDays[2]   = IIF(MOD(YEAR(laDtPeriod[I,1]),4)=0 ,'29' ,'28' )   && Adjust Feb. no. of days depend on the current year value.
        laDtPeriod[I,2] = CTOD(lcPrdMonth+'/'+ALLTRIM(laNoOfDays[lnPrdMonth])+'/'+lcPrdYear)  && Hold the period end date
      ENDIF
      *-- Hold the title of the period "ex.: JANUARY"
      laDtPeriod[I,3] = PADR(CMONTH(laDtPeriod[I,1]),10)
      *-- Assign new date value by adding one day to the current period end date
      ldNewDate  = (laDtPeriod[I,2]+1)
      lcPrdMonth = ALLTRIM(STR(MONTH(ldNewDate)))      && Get period month name
      lnPrdMonth = INT(VAL(lcPrdMonth))                && Get period month #
      lcPrdYear  = ALLTRIM(STR(YEAR(ldNewDate)))       && Get period year
  ENDCASE
ENDFOR

*-- Copy the period array we built to another array
=ACOPY(laDtPeriod,laDtTmpPrd)
*-- Assign variable hold the row # that has to be removed from this array
lnRemvCol = 0
*-- Loop to check if there is any period date range is less than the system date.
FOR I = 1 TO ALEN(laDtTmpPrd,1)
  IF oAriaApplication.SystemDate > laDtTmpPrd[I,1] AND oAriaApplication.SystemDate > laDtTmpPrd[I,2]
    *-- Define the row # that need to be removed from the period array
    lnRemvCol = I
    LOOP
  ENDIF
ENDFOR

*-- Restore the laDtPeriod array after removing the out of date range period row that is less than system date
=ACOPY(laDtTmpPrd,laDtPeriod,(lnRemvCol*3)+1)

FOR I = 1 TO ALEN(laDtPeriod,1)
  lcPrdCnt = ALLTRIM(STR(I,2))
  *-- Define heading to the first period title "Current"
  IF I = 1
    laDtPeriod[I,3] = 'Current'
  ENDIF

  *--Browse fields heading
  lcMn&lcPrdCnt = laDtPeriod[I,3]
ENDFOR

*-- Filling the main array.
SELECT (lcStyleTmp)

*B132139,1 WSH 05/18/2006 [Start]
*=SEEK(lcStyle)
*lnSize_cnt = IIF(SEEK('S'+&lcStyleTmp..Scale,lcScaleTmp), &lcScaleTmp..cnt , 1)
=SEEK(lcStyle) && no need to gfSeek again
lnSize_cnt = IIF(gfSEEK('S'+&lcStyleTmp..Scale,lcScaleTmp), &lcScaleTmp..cnt, 1)
*B132139,1 WSH 05/18/2006 [End]

*-- Define OTS info array with dimention of its size scale
DIMENSION laOTCInfo[lnSize_cnt,12]
laOTCInfo = 0

*-- If display all warehouses, display data from Style file, if display specific 
*-- warehouse, display data from style dyelot file for this warehouse.
SELECT IIF(llAllWareHs,lcStyleTmp,lcStyDyTmp)

*B132139,1 WSH 05/18/2006 [Start]
*=SEEK( lcStyle )
=SEEK( lcStyle ) && No need to gfSeek again
*B132139,1 WSH 05/18/2006 [End]

*-- Scan for the current style to get all the style records 
SCAN WHILE &lcwcondt FOR &lcfcondt
  lcPStyle  = Style
  laOTCInfo = 0.00
  *-- Loop with the style size count
  FOR lnSz_no = 1 TO lnSize_cnt
    lcSz_no = STR(lnSz_no,1)
	laOTCInfo[lnSz_no,01] = STK&lcSz_no                && Get the stock value
	laOTCInfo[lnSz_no,12] = ORD&lcSz_no + SHP&lcSz_no  && Get the order + shipped value
  ENDFOR
  
  *B132139,1 WSH 05/18/2006 [Start]
  **-- If the MF module is installed & style is MAKE "Manufactured"
  *IF llMFInstld AND &lcStyleTmp..MAKE
  *  *-- Procedure to compute the OTS quantites for the manufactured styles (MAKE : YES)
  *  DO lpOTSmYes WITH lcPStyle
  *ENDIF
  *
  **-- If the PO module is installed & style is not MAKE "Imported"
  *IF llPOInstld AND !&lcStyleTmp..MAKE
  *  *-- Procedure to compute the OTS quantites for the imported styles (MAKE : NO)
  *  DO lpOTSmNo  WITH lcPStyle
  *ENDIF
  
  *-- If the PO OR MF modules is installed
  IF llPOInstld OR llMFInstld
    *-- Procedure to compute the OTS quantites for the styles
    DO lpOTSmNo  WITH '0001' + lcPStyle
  ENDIF
  *B132139,1 WSH 05/18/2006 [End]
  
  *-- If the SO module is installed.
  IF llSOInstld
    *-- Procedure to substract the order line quantities from the OTS quantities.
    DO lpOrdQSub WITH lcPStyle
  ENDIF
  
  *C102567,1 HBG IF Get the setting of OTS based on exact transaction date = 'No'
  *C102567,1     the calculation will remain the same [Begin]
  IF !llOTSbasTr
  *C102567,1 [End]
    *-- To post the negative values to the nearst positive value.
    DO lppstnvvlu
  *C102567,1 HBG IF Get the setting of OTS based on exact transaction date = 'Yes'
  *C102567,1     get the OTS for each period [Begin]
  ELSE
    *-- Update OTS qty. in each period
    DO lpUpdOTS
  ENDIF
  *C102567,1 [End]
  
  *-- Replace the OTS Quantities.
  DO lpotslins WITH lcPStyle
ENDSCAN

*-- Restore old work area
SELECT(lnAlias)
*-- Restore record pointer
IF lnCrSav1 <> 0
  GOTO lnCrSav1
ENDIF

*-- Calculate all the table columns to variables
SELECT (lcTmpOTS)
SUM nQty1,nQty2,nQty3,nQty4,nQty5,nQty6,nQty7,nQty8,nQty9,nQty10,nQty11,nQty12 TO ;
    lnQty1,lnQty2,lnQty3,lnQty4,lnQty5,lnQty6,lnQty7,lnQty8,lnQty9,lnQty10,lnQty11,lnQty12

*-- Add new record in the OTS cursor with for columns & rows totals

*B038253,1 AMH Add total word [Start]
*INSERT INTO (lcTmpOTS) (STYLE,SZCnt,nQty1,nQty2,nQty3,nQty4,nQty5,nQty6,nQty7,nQty8,nQty9,nQty10,nQty11,nQty12) ;
	 VALUES (REPL(CHR(255),LEN(ALLTRIM(SUBSTR(lcStyle,1,lnStyleWid)))),'W',lnQty1,lnQty2,lnQty3,lnQty4,lnQty5,lnQty6,lnQty7,lnQty8,lnQty9,lnQty10,lnQty11,lnQty12)
INSERT INTO (lcTmpOTS) (STYLE,SZCnt,Size,nQty1,nQty2,nQty3,nQty4,nQty5,nQty6,nQty7,nQty8,nQty9,nQty10,nQty11,nQty12) ;
   VALUES (REPL(CHR(255),LEN(ALLTRIM(SUBSTR(lcStyle,1,lnStyleWid)))),'W','Total',lnQty1,lnQty2,lnQty3,lnQty4,lnQty5,lnQty6,lnQty7,lnQty8,lnQty9,lnQty10,lnQty11,lnQty12)
*B038253,1 AMH [End]

*-- Change the calling of the trigger to be under condition if exist.
IF ASCAN(oAriaApplication.laEvntTrig,PADR("ACCUMOTS",10)) <> 0
  *--C102237,1 TMI [START] A new line will be added right below each style sizw shown in the OTS 
  *--C102237,1 TMI         screen. This line will show the accumulative OTS balance
  =gfDoTriger('ICSTYLE',PADR('ACCUMOTS',10))
  *--C102237,1 TMI [END  ] 
ENDIF

*-- Define variable hold the browse fields.
lcBrFields = IIF(llAllClrs,"lcSty=SUBSTR(Style,lnStyleWid+2,lnColorWid) :H=lcNMjrTl,","")+;
            "Size ," +;
         	"nQty1  :H=lcMn1  :P='999999',"+;
        	"nQty2  :H=lcMn2  :P='999999',"+;
         	"nQty3  :H=lcMn3  :P='999999',"+;
         	"nQty4  :H=lcMn4  :P='999999',"+;
        	"nQty5  :H=lcMn5  :P='999999',"+;
        	"nQty6  :H=lcMn6  :P='999999',"+;
        	"nQty7  :H=lcMn7  :P='999999',"+;
        	"nQty8  :H=lcMn8  :P='999999',"+;
        	"nQty9  :H=lcMn9  :P='999999',"+;
        	"nQty10 :H='"+LANG_HdFuture+"' :P='999999',"+;
        	"nQty11 :H='"+LANG_HdTotAvl+"' :P='999999',"+;
        	"nQty12 :H='"+LANG_HdTotSld+"' :P='999999'"

*-- Clear the wait message to execute the browse function
WAIT CLEAR

*-- Call global function browse to display the OTS info.
GOTO TOP

=ARIABROW('',LANG_BrowTitl,gnbrhsrow1, gnbrhscol1, gnbrhsrow2, gnbrhscol2,'',;
         	'Fi\<nd;Or\<der by;\<Descending;Fi\<lter;;\!\?\<Ok')

*-- Check the opened flag to close the files that have been opened in this function

*B132139,1 WSH 05/18/2006 [Start]
*IF llOpnScale
*  USE IN (lcScaleTmp)
*ENDIF
*IF llOpnStyle
*  USE IN (lcStyleTmp)
*ENDIF
*IF llOpnPoHdr
*  USE IN (lcPoHdrTmp)
*ENDIF
*IF llOpnPoLin
*  USE IN (lcPolinTmp)
*ENDIF
*IF llOpnShpHdr
*  USE IN (lcShpHdTmp)
*ENDIF
*IF llOpnCutHdr
*  USE IN (lcCutHdTmp)
*ENDIF
*IF llOpnCutLin
*  USE IN (lcCutlnTmp)
*ENDIF
*IF llOpnOrdHdr
*  USE IN (lcOrdHdTmp)
*ENDIF
*IF llOpnOrdLin
*  USE IN (lcOrdLnTmp)
*ENDIF
IF llOpnScale
  =gfCloseTable(lcScaleTmp)
ENDIF
IF llOpnStyle
  =gfCloseTable(lcStyleTmp)
ENDIF
IF llOpnPoHdr
  =gfCloseTable(lcPoHdrTmp)
ENDIF
IF llOpnPoLin
  =gfCloseTable(lcPolinTmp)
ENDIF
IF llOpnShpHdr
  =gfCloseTable(lcShpHdTmp)
ENDIF
IF llOpnOrdHdr
  =gfCloseTable(lcOrdHdTmp)
ENDIF
IF llOpnOrdLin
  =gfCloseTable(lcOrdLnTmp)
ENDIF
*B132139,1 WSH 05/18/2006 [End]

*-- Restore work area
SELECT(lnAlias)
RETURN


*:******************************************************************
*! PROG      : lpOTSmYes
*! Developer : WAB - WALID A. WAHAB
*! Date      : 11/24/1999
*! DESC      : Procedure to compute the OTS quantites for the
*!             manufactured styles (MAKE : YES)
*:******************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : lcOtsKey->OTS key like style as ex.
*!*************************************************************
*! Returns   : ............
*!*************************************************************
*! Example   : Do ..
*!*************************************************************
*E500304,1 WAB
*!************************************************************************
PROCEDURE lpOtsMYes
PARAMETERS lcOtsKey

*-- Scan in the cutting ticket line with the current style & warehouse
SELECT (lcCutlnTmp)
SEEK lcOtsKey
SCAN WHILE Style = lcOtsKey FOR (llAllWareHs OR cWareCode=lcWareHouse)
  *-- For the founded line, check if its Cutting ticket # is "Open, On Hold or Active" in the cutting ticket header
  =SEEK(Cuttkt,lcCutHdTmp)
  IF &lcCutHdTmp..Status $ 'OAH'
    *-- Call procedure to accumulate the cutting ticket lines quantity to the OTS info.
    *C102567,1 HBG Pass the parameter of add to current by .T. to get the OTS in the correct period [Begin]
    DO acum_ots WITH lcCutlnTmp , &lcCutHdTmp..complete , IIF(Trancd='1',1,-1) , .T. , .T.
    *C102567,1 [End]
  ENDIF
ENDSCAN
RETURN

*:******************************************************************
*! Name      : lpOTSmNo
*! Developer : WAB - WALID A. WAHAB
*! Date      : 11/24/1999
*! Purpose   : Procedure to compute the OTS quantites for the imported
*!             styles (MAKE : NO)
*:******************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : lcOtsKey->OTS key like style as ex.
*!*************************************************************
*! Returns   : ............
*!*************************************************************
*! Example   : Do ..
*!*************************************************************
PROCEDURE lpOtsMNo
PARAMETERS lcOtsKey

PRIVATE lcPo_no

*-- Define aray with dimention of style size count
DIMENSION lnQty[lnSize_cnt]
lnQty   = 0

*B132139,1 WSH 05/18/2006 [Start]
*lcPo_no = ''
lcPo_no = SPACE(8)
*B132139,1 WSH 05/18/2006 [End]

*-- Define 2 arrays to check if there is over rescive or not[Begin]
DIMENSION laPOVlu[lnSize_cnt],laOverResv[lnSize_cnt]
laPOVlu    = 0
laOverResv = .F.

*-- Scan in the PO lines with the current style & warehouse
SELECT (lcPolinTmp)

*B132139,1 WSH 05/18/2006 [Start]
*SEEK lcOtsKey
*SCAN WHILE Style = lcOtsKey ;
*     FOR cStyType<>'C' AND (llAllWareHs OR cWareCode=lcWareHouse) AND TranCd<>'6'
*  *-- For the founded line, check if its PO # is "Open, On Hold" in the PO header
*  =SEEK(&lcPolinTmp..cStyType+&lcPolinTmp..PO,lcPoHdrTmp)
=gfSEEK(lcOtsKey)
SCAN WHILE cinvtype+style+cbusdocu+cstytype+po+STR(lineno,6)+trancd = lcOtsKey ;
     FOR cStyType $ "ADPUN" AND (llAllWareHs OR cWareCode=lcWareHouse) AND TranCd <> '6'
  *-- For the founded line, check if its PO # is "Open, On Hold" in the PO header
  =gfSEEK(cBusDocu+cStyType+PO, lcPoHdrTmp)
*B132139,1 WSH 05/18/2006 [End]

  *B608884,1 WAM 06/02/2009 Include Actualized PO in OTS calculation
  *IF !(&lcPoHdrTmp..Status $ 'OH')
  IF !(&lcPoHdrTmp..Status $ 'OHA')
  *B608884,1 WAM 06/02/2009 (End)

    LOOP
  ENDIF

  *-- Define the transaction sign depend on the line type & PO type.
  
  *B132139,1 WSH 05/18/2006 [Start]
  *lcTrSign = IIF(Trancd='1',1,-1) * IIF(cStyType $ 'ADPN',1,-1)
  lcTrSign = IIF(Trancd='1',1,-1) * IIF(cBusDocu $ 'R', -1, 1)
  *B132139,1 WSH 05/18/2006 [End]
  
  *-- Get transaction date
  ldTranDate = IIF (EMPTY(&lcPoHdrTmp..available),&lcPoHdrTmp..complete ,&lcPoHdrTmp..available)
  
  llArrQty = .F.
  *-- If finishing looping in the PO line & the PO has changed
  
  *B132139,1 WSH 05/18/2006 [Start]
  *IF lcPo_no = PO
  IF lcPo_no = cBusDocu+cStyType+PO
  *B132139,1 WSH 05/18/2006 [End]
  
    *-- Check For reciving and damaged and canceled Qty to fix bug of over reciecve
    IF Trancd $ '245'
      llArrQty = .T.
      *-- Loop to check the over receive qty.
      FOR lnCount = 1 TO lnSize_cnt
        lcCount = STR(lnCount,1)
        *-- If there is over receive in the current qty., add zero to the qty. array.
        IF laOverResv[lnCount]
          lnQty[lnCount] = 0
        *-- If not over receive the current qty.
        ELSE
          *-- If the current qty. greater than the budget qty. & the budget qty. greater than zero
          IF Qty&lcCount. > laPOVlu[lnCount] AND laPOVlu[lnCount] > 0
            *-- Save budget qty.
            lnQty[lnCount]      = laPOVlu[lnCount]
            *-- Set the flag of over recive to .T.
            laOverResv[lnCount] = .T.
          ELSE
            *-- Save current Budget qty.
            lnQty[lnCount]      = Qty&lcCount. 
            laPOVlu[lnCount]    = laPOVlu[lnCount] - Qty&lcCount. 
            IF laPOVlu[lnCount] = 0
              *-- Set the flag of over recive to .T.
              laOverResv[lnCount] = .T.
            ENDIF
          ENDIF 
        ENDIF  
      ENDFOR
    ENDIF
  *-- If the PO has been changed
  ELSE
    *-- Assign the PO # to the PO variable
    
    *B132139,1 WSH 05/18/2006 [Start]
    *lcPo_no = PO
    lcPo_no = cBusDocu+cStyType+PO
    *B132139,1 WSH 05/18/2006 [End]
    
    FOR lnCount = 1 TO lnSize_cnt
      lcCount = STR(lnCount,1)
      *-- Save the current qty. in the qty. array.
      lnQty[lnCount] = Qty&lcCount.
      *-- Save the budget qty. of the PO line.
      laPOVlu[lnCount] = Qty&lcCount.
      *-- Set the flag of over recive to .F.
      laOverResv = .F.
    ENDFOR
  ENDIF
  *-- Reham 
  *-- Change the checking of the trigger name, Change the calling of the trigger, instead calling the standard
  *-- code, The commented line has to be moved to customer main program in a new function with the name of the trigger
  
  *IF ASCAN(oAriaApplication.laEvntTrig , PADR('ACCUMOTS', 10)) <> 0 
    *DO acum_ots WITH lcPolinTmp , ldTranDate , lcTrSign , .T. , .F. , llArrQty
  *-- To accumulate the quantity in process.
  *-- Update Open & hold PO's if it is prior to date range in First period not the current based on JL trigger
  IF ASCAN(oAriaApplication.laEvntTrig , PADR('UPDPO1', 10)) <> 0 
    =gfDoTriger('ICSTYLE',PADR('UPDPO1',10))
  *-- Reham
  ELSE
    DO acum_ots WITH lcPolinTmp , ldTranDate , lcTrSign , .T. , .T. , llArrQty
  ENDIF

  *-- If there is shipped qty.
  
  *B132139,1 WSH 05/18/2006 [Start]
  *IF Trancd = '3'
  IF Trancd = '3' AND llPOInstld
  *B132139,1 WSH 05/18/2006 [End]
  
    *-- Get the shipment info.
    
    *B132139,1 WSH 05/18/2006 [Start]
    *=SEEK(&lcPolinTmp..ShipNo,lcShpHdTmp)
    *B609507,1 TMI 01/20/2011 [Start] replace the SEEK command with gfSeek for the shpmthdr file as it is a sql file
    *=SEEK(&lcPolinTmp..cBusDocu + &lcPolinTmp..cStyType + &lcPolinTmp..ShipNo, lcShpHdTmp)
    =gfSEEK(&lcPolinTmp..cBusDocu + &lcPolinTmp..cStyType + &lcPolinTmp..ShipNo, lcShpHdTmp)
    *B609507,1 TMI 01/20/2011 [End  ] 
    *B132139,1 WSH 05/18/2006 [End]
    
    *-- Reham
    *-- Change the checking of the trigger name, Change the calling of the trigger, instead calling the standard
    *-- code, The commented line has to be moved to customer main program in a new function with the name of the trigger
    
    *IF ASCAN(oAriaApplication.laEvntTrig , PADR('ACCUMOTS', 10)) <> 0 
    *  DO acum_ots WITH lcPolinTmp , &lcShpHdTmp..eta , 1 , .T. , .F. , llArrQty
    *-- To accumulate the quantity in process.
    *-- Update Open & hold PO's if it is prior to date range in First period not the current based on JL trigger
    IF ASCAN(oAriaApplication.laEvntTrig , PADR('UPDPO2', 10)) <> 0 
      =gfDoTriger('ICSTYLE',PADR('UPDPO2',10))
    *-- Reham
    ELSE
      DO acum_ots WITH lcPolinTmp , &lcShpHdTmp..eta , 1 , .T. , .T. , llArrQty
    ENDIF
  ENDIF
ENDSCAN
RETURN

*:******************************************************************
*! PROG      : lpOrdQSub
*! Developer : WAB - WALID A. WAHAB
*! Date      : 11/24/1999
*! DESC      : Procedure to substract the order line quantities from the
*!             OTS quantities .
*:******************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : lcOtsKey->OTS key like style as ex.
*!*************************************************************
*! Returns    : ............
*!*************************************************************
*! Example   : Do ..
*!*************************************************************
PROCEDURE lpOrdQSub
PARAMETERS lcOtsKey

*-- Scan in the order lines with the current style & warehouse
SELECT (lcOrdLnTmp)

*B132139,1 WSH 05/18/2006 [Start]
*SEEK lcOtsKey
=gfSEEK(lcOtsKey)
*B132139,1 WSH 05/18/2006 [End]

*B000000,1 HBG 01/30/2005 Get the OTS on Configuration level if use configuration [Begin]
*SCAN WHILE Style = lcOtsKey FOR cOrdType='O'
SCAN WHILE Style = lcOtsKey FOR cOrdType='O' AND IIF(llUseConfig,Dyelot=lcDyelot,.T.)
*B000000,1 [End]
  
  *B132139,1 WSH 05/18/2006 [Start]
  *=SEEK('O'+&lcOrdLnTmp..Order,lcOrdHdTmp)
  =gfSEEK('O'+&lcOrdLnTmp..Order,lcOrdHdTmp)
  *B132139,1 WSH 05/18/2006 [End]
  
  *-- For the founded line, check if its order # is "Open, On Hold" in the order header
  IF &lcOrdHdTmp..Status$'OH' AND (llAllWareHs OR &lcOrdHdTmp..cWareCode=lcWareHouse)
  *-- Call procedure to accumulate the order lines quantity to the OTS info.
  DO acum_ots WITH lcOrdLnTmp , start , -1 , .F. , .T.
  ENDIF
ENDSCAN
RETURN

*:******************************************************************
*! PROG      : lpPstNvVlu
*! Developer : WAB - WALID A. WAHAB
*! Date      : 11/24/1999
*! DESC      : Procedure to post the negative values in any period to the
*!             nearst positive value of any next period.
*!             - We do this by looping the two dimensional array "laOTCInfo"
*!               in case of the report format is by size , otherwise we will
*!               loop just the last row of the same array (which holds the
*!               total OTS quantities for this color) ,to do the following :
*!             - If there is a negative value , we will make it 0 and subtract
*!               it from the nearst next period has a POSITIVE value.
*:******************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns   : ............
*!*************************************************************
*! Example   : Do ..
*!*************************************************************
PROCEDURE lpPstNvVlu

*-- Define on hand variable
xon_hand = 0
*-- Loop with the no. of the style sizes
FOR lnSz_no = 1 TO lnSize_cnt
  *-- Loop in the first 10 periods
  FOR I = 1 TO 10
    *-- If the current OTS value for the current size & in the current period in negative value.
	IF laOTCInfo[lnSz_no,I] < 0
	  *-- Store this negative value to the On Hand value
  	xon_hand = laOTCInfo [lnSz_no,I]
  	*-- Set the negative value rto zero
	  laOTCInfo[lnSz_no,I] = 0
	  *-- If first period
	  IF I = 1
		DO lppostbf WITH 2,10                  && GO_BACKWORD   ->
      *-- If any other period
	  ELSE
		DO lppostbf WITH I-1,1                 && GO_BACKWORD   <-
		*-- If the On Hand value is negative & the period is not the tenth period
		IF (xon_hand < 0) AND (I < 10)
	  	  DO lppostbf WITH I+1,10              && GO_FOREWORD   ->
		ENDIF
  	ENDIF
	ENDIF   &&-> -VE NUMBER
  ENDFOR
  *-- If the On hand qty. is negative
  IF xon_hand < 0
    *-- Put the remaining negative qty. in the tenth period
	laOTCInfo[lnSz_no,10] = xon_hand
  ENDIF
  *-- Set on hand value to zero
  xon_hand = 0
ENDFOR
RETURN

*:******************************************************************
*! PROG      : lpUpdOTS
*! Developer : Hend Ghanem (HBG)
*! Date      : 03/04/2002
*! DESC : Update OTS qty. in each period
*:******************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : lcOtsKey->OTS key like style as ex.
*!*************************************************************
*! Returns   : ............
*!*************************************************************
*! Example   : Do ..
*!*************************************************************
*!C102567,1 HBG
*!*************************************************************
PROCEDURE lpUpdOTS

*-- Loop with the no. of the style sizes.
FOR lnSz_no = 1 TO lnSize_cnt
  *-- Get the OTS value for the current size
  lnCurrent = laOTCInfo[lnSz_no,1]
  *-- Loop from second period to the total no. of periods
  FOR I = 2 TO lnPrdNum
    *-- Increase the OTS value with the current size in the current period with the current value
    laOTCInfo[lnSz_no,I] = laOTCInfo[lnSz_no,I] + lnCurrent
    *-- Put the the OTS value with the current size in the current period in the current value
    lnCurrent = laOTCInfo[lnSz_no,I]
  ENDFOR
ENDFOR

*:******************************************************************
*! PROG      : lpOTSLins
*! Developer : WAB - WALID A. WAHAB
*! Date      : 11/24/1999
*! DESC      : Replace the OTS Quantities.
*:******************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : lcOtsKey->OTS key like style as ex.
*!*************************************************************
*! Returns   : ............
*!*************************************************************
*! Example   : Do ..
*!*************************************************************
*!E500304,1 WAB
*!*************************************************************
PROCEDURE lpOtsLins
PARAMETERS lcOtsKey

*-- Loop to accumulate the the OTS lines and total to the 11th col. {Period} in the array .
FOR lnSz_no = 1 TO lnSize_cnt
  *C102567,1 HBG IF Get the setting of OTS based on exact transaction date = 'No'
  *C102567,1     the calculation will remain the same
  IF !llOTSbasTr
    FOR I = 1 TO 10
	  laOTCInfo[lnSz_no,11] = laOTCInfo[lnSz_no,11] + laOTCInfo[lnSz_no,I]
    ENDFOR
  *C102567,1 HBG IF Get the setting of OTS based on exact transaction date = 'Yes'
  *C102567,1     the Total avalibale is the future qty
  ELSE
    laOTCInfo[lnSz_no,11] = laOTCInfo[lnSz_no,10]
  ENDIF
ENDFOR

*-- Loop with the no. of the style sizes to update the OTS cursor lines for each [color\size].
FOR lnSz_no = 1 TO lnSize_cnt
  lcSz_no = STR(lnSz_no,1)
  SELECT (lctmpots)
  *-- Seek the current style & the current size 
  =SEEK(lcOtsKey+lcSz_no)
  *-- Loop in the first 12 periods
  FOR I = 1 TO 12
	Z = ALLTRIM(STR(I,2))
	*-- Update the style/size qty. with the current OTS value
	REPLACE nQty&Z WITH laOTCInfo[lnSz_no,I]
  ENDFOR
ENDFOR
RETURN

*:******************************************************************
*! PROG      : ACUM_OTS
*! Developer : WAB - WALID A. WAHAB
*! Date      : 11/24/1999
*! DESC      : Procedure to accumulate the OTS quantities .
*:******************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  :
*!        1) lcMFile    : To hold the file name in process .
*!        2) ldTrDate   : To hold the date in process .
*!        3) lnSnType   : To deside whether to add or subtract the
*!                        quantity in process to or from the balance .
*!        4) llAcumOTS  : If this funtion was called to accumalats O.T.S Qty
*!                        or Subtract the order Qty .
*!        5) llAddToCur : The qty before periods add to the current coloumn.
*!        6) llArrQty   : Flag to determine if use original transaction 
*!                      : qty. or array qty.
*!*************************************************************
*! Returns   : ............
*!*************************************************************
*! Example   : Do ..
*!*************************************************************
PROCEDURE Acum_Ots
PARAMETERS lcMFile , ldTrDate , lnSnType , llAcumOTS  , llAddToCur , llArrQty
PRIVATE lnX

*-- Define if we are going to accumalate OTS qty. or subtract order qty
lnX = IIF(llAcumOTS,2,1)

*-- The columns of the array laOTCInfo are initialized
*-- By the stock Qty and this function is going to accumalats the O.T.S QTY
*-- Or Subtract the order Qty form each column , But now we need to exclude
*-- The first column of this array (i.e laOTCInfo[1,x]) form this process
*-- Because this column carry the current O.T.S Qty .    

*C102567,1 HBG IF Get the setting of OTS based on exact transaction date = 'No'
*C102567,1     the calculation will remain the same
SELECT (lcMFile)
IF !llOTSbasTr
  *-- If the current transaction date is between the defined periods date range.
  IF BETWEEN(ldTrDate , laDtPeriod[1,1] , laDtPeriod[10,2])
    *-- Loop with the no. of the style sizes
    FOR  lnSz_no = 1 TO lnSize_cnt
      lcSz_no = STR(lnSz_no,1) 
      *-- Determin if we will use lnQty Array or QtyX fields 
      lcQty = IIF(llArrQty,'lnQty['+lcSz_no+']','Qty'+lcSz_no)
      *-- Search for the right period to process .
      FOR I = lnX TO 10
        *-- Check if the current transaction date is in the range of the current period
	    IF BETWEEN(ldTrDate , laDtPeriod[I,1] , laDtPeriod[I,2])
          *-- Accumulate the OTS values.
          laOTCInfo[lnSz_no,I] = laOTCInfo[lnSz_no,I] + ( EVALUATE(lcQty) * lnSnType )
	      EXIT
        ENDIF
      ENDFOR
      
      *-- If the current transaction is less than 1st period or between first and second periods 
      *-- and flag of accumulate the OTS qty. is true
      IF (laDtPeriod[1,1] <= ldTrDate).AND.(ldTrDate < laDtPeriod[2,1]) .AND. llAcumOTS

          *-- Reham
          *-- Change the checking of the trigger name, Change the calling of the trigger, instead calling the standard
          *-- code, The commented line has to be moved to customer main program in a new function with the name of the trigger

          *C102567,1 Accumalate the OTS qty. in the correct period
          *IF ASCAN(oAriaApplication.laEvntTrig , PADR('ACCUMOTS', 10)) <> 0 
          *  laOTCInfo[lnSz_no,2]=laOTCInfo[lnSz_no,2] + ( EVALUATE(lcQty) * lnSnType )
          IF ASCAN(oAriaApplication.laEvntTrig , PADR('UPDOTS', 10)) <> 0 
            =gfDoTriger('ICSTYLE',PADR('UPDOTS',10))
          ELSE
            laOTCInfo[lnSz_no,1]=laOTCInfo[lnSz_no,1] + ( EVALUATE(lcQty) * lnSnType )
          ENDIF
      ENDIF
    ENDFOR
  *-- If the current transaction date is not between the 1st period start date & the 10th period end date
  ELSE
    *-- If the current transaction date is greater than the end date of 10th period
    IF ldTrDate > laDtPeriod[10,2]
      FOR lnSz_no = 1 TO lnSize_cnt
        lcSz_no = STR(lnSz_no,1)
        *-- Determin if we will use lnQty Array or QtyX fields 
        lcQty = IIF(llArrQty,'lnQty['+lcSz_no+']','Qty'+lcSz_no)
        *-- Accumulate the OTS values to the 10th period
        laOTCInfo[lnSz_no,10]=laOTCInfo[lnSz_no,10] + ( EVALUATE(lcQty) * lnSnType )
      ENDFOR
    *-- If the current transaction date is less than or equal the end date of 10th period
    ELSE
      FOR lnSz_no = 1 TO lnSize_cnt
        lcSz_no = STR(lnSz_no,1) 
        *-- Determin if we will use lnQty Array or QtyX fields 
        lcQty = IIF(llArrQty,'lnQty['+lcSz_no+']','Qty'+lcSz_no)
        IF llAddToCur
          *-- If llAddToCur is true add the qty to current column 
          laOTCInfo[lnSz_no,1] = laOTCInfo[lnSz_no,1] + ( EVALUATE(lcQty)* lnSnType )
        ELSE
          *-- If llAddToCur is false add the tran. qty. to the first period not the current period
          laOTCInfo[lnSz_no,2] = laOTCInfo[lnSz_no,2] + ( EVALUATE(lcQty)* lnSnType )
        ENDIF
      ENDFOR
    ENDIF
  ENDIF
*C102567,1 HBG IF Get the setting of OTS based on exact transaction date = 'Yes'
*C102567,1     Update each transaction date with the Qty of the transaction happen in it
ELSE
  *-- If the current transaction date is between the 1st period start date & the 10th period end date
  IF BETWEEN(ldTrDate , laDtPeriod[1,1] , laDtPeriod[10,2])
    *-- Loop with the no. of the style sizes
    FOR lnSz_no = 1 TO lnSize_cnt
      *-- Check if the current transaction date is in date range of 1st period
      IF BETWEEN(ldTrDate , laDtPeriod[1,1] , laDtPeriod[1,2])
        lcSz_no = STR(lnSz_no,1)
        *-- Determin if we will use lnQty Array or QtyX fields 
        lcQty   = IIF(llArrQty,'lnQty['+lcSz_no+']','Qty'+lcSz_no)
        *-- Accumulate the OTS values to the 1st period
        laOTCInfo[lnSz_no,1] = laOTCInfo[lnSz_no,1] + ( EVALUATE(lcQty) * lnSnType )
      *-- If the current transaction date is not in date range of 1st period
      ELSE
        *-- Search for the right period to process .
        FOR I = lnX TO lnPrdNum
          IF BETWEEN(ldTrDate , laDtPeriod[I,1] , laDtPeriod[I,2]) AND IIF(I > lnX , ldTrDate > laDtPeriod[I-1,2] , .T.)
            lcSz_no = STR(lnSz_no,1) 
            *-- Determin if we will use lnQty Array or QtyX fields 
            lcQty   = IIF(llArrQty,'lnQty['+lcSz_no+']','Qty'+lcSz_no)
            *-- Accumulate the OTS values to the 1st period
            laOTCInfo[lnSz_no,I] = laOTCInfo[lnSz_no,I] + ( EVALUATE(lcQty) * lnSnType )
          ENDIF
        ENDFOR
      ENDIF
    ENDFOR
  *-- If the current transaction date is not between the 1st period start date & the 10th period end date
  ELSE
    *-- If the current transaction date is greater than the end date of 10th period
    IF ldTrDate > laDtPeriod[10,2]
      FOR lnSz_no = 1 TO lnSize_cnt
        lcSz_no = STR(lnSz_no,1)
        *-- Determin if we will use lnQty Array or QtyX fields 
        lcQty   = IIF(llArrQty,'lnQty['+lcSz_no+']','Qty'+lcSz_no)
        *-- Accumulate the OTS values to the 10th period value
        laOTCInfo[lnSz_no,10] = laOTCInfo[lnSz_no,10] + ( EVALUATE(lcQty) * lnSnType )
	  ENDFOR
    *-- If the current transaction date is less than or equal the end date of 10th period
    ELSE
      FOR lnSz_no = 1 TO lnSize_cnt
        lcSz_no = STR(lnSz_no,1)
        *-- Determin if we will use lnQty Array or QtyX fields 
        lcQty   = IIF(llArrQty,'lnQty['+lcSz_no+']','Qty'+lcSz_no)
        IF llAddToCur
          *-- If llAddToCur is true add the qty to current column 
          laOTCInfo[lnSz_no,1]=laOTCInfo[lnSz_no,1] + ( EVALUATE(lcQty)* lnSnType )
        ELSE
          *-- If llAddToCur is false add the tran. qty. to the first period not the current period
          laOTCInfo[lnSz_no,2]=laOTCInfo[lnSz_no,2] + ( EVALUATE(lcQty)* lnSnType )
        ENDIF
      ENDFOR
    ENDIF
  ENDIF
ENDIF
RETURN

*:******************************************************************
*! PROG      : lpPostBf
*! Developer : WAB - WALID A. WAHAB
*! Date      : 11/24/1999
*! DESC      : To do the posting either backword or forword.
*! NOTE      : Called from lpPstNvVlu
*:******************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : lnIniPer,lnEndPer->Initial and end periods.
*!*************************************************************
*! Returns   : ............
*!*************************************************************
*! Example   : Do ..
*!*************************************************************
PROCEDURE lpPostBF
PARAMETERS lnIniPer ,lnEndPer

*-- Define the for direction "From , To , Direction {Forward/Backword}"
lcFloop = IIF(lnIniPer>lnEndPer,'lnIniPer TO lnEndPer STEP -1','lnIniPer TO lnEndPer')

FOR I = &lcFloop
  *-- Add the current OTS value to the on hand value
  xon_hand =  xon_hand + laOTCInfo[lnSz_no,I]
  *-- If the on hand value is negative or is zero
  IF xon_hand <= 0
    *-- Set the current OTS value to zero
	laOTCInfo[lnSz_no,I] = 0
  *-- If the on hand value is positive
  ELSE
    *-- Save on hand value in the current period
	laOTCInfo[lnSz_no,I] = xon_hand
	*-- Set on hand value to zero
	xon_hand = 0
	EXIT
  ENDIF
ENDFOR
RETURN


*:---------------------------------------------------------------------
*! Name      : POFBROW
*! Developer : Wael Aly Mohamed 
*! Date      : 05/04/97
*! Purpose   : Function to browse material POS
*: Job ID    : E300637,1
*:---------------------------------------------------------------------
*: Calls              : gfDialog,AriaBrow
*:---------------------------------------------------------------------
*: Passed Parameters  : lcPOType : PO Type
*:                      lcVendor : Vendor
*:                      lcPo     : PO#
*:---------------------------------------------------------------------
*: Example            : =POFBROW('P',@lcVendor,@lcPo)
*:---------------------------------------------------------------------
FUNCTION POFBROW
PARAMETER lcPOType,lcVendor,lcPo
PRIVATE lcBrFields,lcAlias,laData,llRetValue,lcPOOrder


lcAlias = ALIAS()
SELECT POFHDR
lcOldFilt = FILTER()
SET FILT TO
DO CASE
  CASE lcPOType= 'P'
    lcTitle  = 'Material Purchase Orders'
  CASE lcPOType= 'C'
    lcTitle  = 'Material Purchase Order Contracts'
  CASE lcPOType= 'R'
    lcTitle  = 'Material Purchase Order Returns'
ENDCASE
lcPOOrder = ORDER('POFHDR')
lcVendor  = IIF(TYPE('lcVendor')='C' .AND. !EMPTY(lcVendor),lcVendor,'')
lcPo      = IIF(TYPE('lcPo')='C',lcPo,SPACE(6))
SET ORDER TO TAG IIF(EMPTY(lcVendor),'POFHDR','POFHDRV') IN POFHDR
IF !SEEK(lcVendor+lcPOType,'POFHDR')
  =MESSAGEBOX('No '+lcTitle+' found'+IIF(EMPTY(lcVendor),'',' for vendor '+lcVendor)+'.')
  *=gfDialog ("I","No "+lcTitle+' found'+IIF(EMPTY(lcVendor),'',' for vendor '+lcVendor)+'.')
  lcVendor = SPACE(8)
  lcPo     = SPACE(6)
  SELECT POFHDR
  SET FILTER TO &lcOldFilt 
  SET ORDER TO TAG lcPoOrder IN POFHDR
  IF !EMPTY(lcAlias)
     SELECT (lcAlias)
  ENDIF  

  RETURN(.F.)
ENDIF
DECLARE laBrow[2]
laBrow=' '
lcBrFields = "POMAT   :R :H='P/O #':12,"+;
             "Status  :R :H='S':4,"+;	         	           	         
             "Vendor  :R :H='Vendor':15,"+;
	         "ApVendor.cVenComp :R :H='Name':22,"+;
	         "Complete:R :H='Complete':10,"+;
  	         "NFABORDER  :R :H='Tot.Qty.':10,"+;	         
             "POTotal :R :H='Amount':15,"+;
             "NFBRECEIVE :R :H='Receive':10,"+;
             "NPO_OPEN   :R :H='Open':10"
llOpnApVen = gfOpenFile(oAriaApplication.DataDir+'ApVendor',oAriaApplication.DataDir+'VenCode','SH')
CLEAR TYPEAHEAD
SELECT POFHDR
SET RELATION TO Vendor INTO ApVendor
=SEEK(lcVendor+lcPOType)

*llRetValue = gfBrows('lcVendor+lcPOType',"PoMat,Vendor","laBrow",lcTitle)
llRetValue =AriaBrow([lcVendor+lcPOType],lcTitle,0,0,0,0,'','',"PoMat,Vendor","laBrow",.T.,"POSHDR",.f.)

IF llRetValue
  lcPo     = laBrow[1]
  lcVendor = laBrow[2]
ELSE
  lcPo     = SPACE(6)
  lcVendor = SPACE(8) 
ENDIF  
IF llOpnApVen
  USE IN ApVendor
ENDIF
SET FILTER TO &lcOldFilt 
SET ORDER TO TAG lcPoOrder IN POFHDR
IF !EMPTY(lcAlias)
  SELECT (lcAlias)
ENDIF  
RETURN llRetValue
*:---------------------------------------------------------------------
*! Name      : gfGetAddLbl
*! Developer : Moamed Abdel-Salam 
*! Date      : 08/13/2003
*! Purpose   : Function to the address lables
*:---------------------------------------------------------------------
*: Calls              : oAriaApplication.remotesystemdata
*:---------------------------------------------------------------------
*: Passed Parameters  : pcCountry : Country Code
*:                      pcLables : Refernce to array for return value
*:---------------------------------------------------------------------
*: Example            : =gfGetAddLbl("USA",@laAddress)
*:---------------------------------------------------------------------
FUNCTION gfGetAddLbl
PARAMETERS pcCountry, pcLables
*-- If no parameter sent use the default country code
IF EMPTY(pcCountry)
  pcCountry = oAriaApplication.DefaultCountry
ENDIF
lcTmpCursor = gfTempName()
lcSelect = "Select * from SYCINT where ccont_code='" + pcCountry +"'"
lnRemResult = oAriaApplication.remotesystemdata.execute(lcSelect,'',lcTmpCursor,"",oAriaApplication.SystemConnectionString,3,"",SET("DATAS"))
DECLARE pcLables[6,2]
FOR lnCnt = 1 TO 6
  lcLblFld  = "CPARt"+ STR(lnCnt,1) + "LAB"
  lcWidFld = "nPARt"+ STR(lnCnt,1) + "LEN"
  pcLables[lnCnt,1] = &lcTmpCursor..&lcLblFld. 
  pcLables[lnCnt,2] = &lcTmpCursor..&lcWidFld.
ENDFOR
USE IN (lcTmpCursor)


*!*************************************************************
*! Name      : gfScalBrow
*! Developer : MALAK - Malak Hanna
*! Date      : 05/02/1995
*! Purpose   : Function to validate entered scale size value.
*!*************************************************************
*! Calls     : ARIABROW()
*!*************************************************************
*! Passed Parameters  :  Entered Scale
*!                       Multi scale 
*!*************************************************************
*! Returns            :  .T. --> Valid scale 
*!                       .F. --> Invalid scale
*!*************************************************************
*! Example            :  gfScalBrow(@m.Scale)
*!*************************************************************
FUNCTION gfScalBrow
PARAMETER lcParaScale,llMltScle,llBrowse
PRIVATE lcBrFields

lnOldAlias = SELECT()
*-- N119687,1 WSH 04/21/2004 Chnge the title of scale browse from "Scale" to "Size Scale"  [Start]
*-- Change the variable lcFile_Ttl to  "Size Scales" and read it from ARia.H file
*lcFile_Ttl = 'Scales'
*-- N119687,1 WSH 04/21/2004 Chnge the title of scale browse from "Scale" to "Size Scale"  [End]
PRIVATE laValues

*--If Extended size scale was used.
IF llMltScle
  *--Browse from Scale Header file.
  llOpndSHd = gfOpenFile(oAriaApplication.DataDir+'scalehd','Extscale','SH')

  IF llBrowse OR EMPTY(lcParaScale) OR !SEEK(lcParaScale)
    DECLARE laValues[1]  && array to get values from browse
    lcBrFields = "cExtScale :H='Scale' ,"+;
                 "cScaleDes :H='Description',"+;
                 "cDim1Desc :H='Dimention1' ,"+;
                 "cDim2Desc :H='Dimention2' ,"+;
                 "cDim3Desc :H='Dimention3' ,"+;
                 "nNoOfDim  :H='No of Dimes' "
   
    *=ARIABROW('',lcFile_Ttl,gnBrHSRow1, gnBrHSCol1, gnBrHSRow2, gnBrHSCol2,'','','cExtScale','laValues')
		=ARIABROW('',LANG_ARIA_Scale_Title,gnBrHSRow1, gnBrHSCol1, gnBrHSRow2, gnBrHSCol2,'','','cExtScale','laValues')    
    lcParaScale = IIF(EMPTY(laValues[1]),SPACE(1),laValues[1])
  ENDIF  

  IF llOpndSHd
    USE IN SCALEHD
  ENDIF

*--NO extended scales.
ELSE
  SELECT SCALE
  IF llBrowse OR EMPTY(lcParaScale) OR !SEEK('S'+lcParaScale)
    DECLARE laValues[1]  && array to get values from browse
     lcBrFields = "Scale:5:H='Scale' ,"+;
                  "cScl_DESC :24:H='Description',"+;
                  "CNT  :H='Cnt'  ,"+;
                  "SZ1  :15:H='Size 1',"+;
                  "SZ2  :15:H='Size 2',"+;
                  "SZ3  :15:H='Size 3',"+;
                  "SZ4  :15:H='Size 4',"+;
                  "SZ5  :15:H='Size 5',"+;
                  "SZ6  :15:H='Size 6',"+;
                  "SZ7  :15:H='Size 7',"+;
                  "SZ8  :15:H='Size 8'"
   
    *=ARIABROW(['S'],lcFile_Ttl,gnBrHSRow1, gnBrHSCol1, gnBrHSRow2, gnBrHSCol2,'','','SCALE','laValues')
    *B128783,1 KHM 07/12/2005 Do not display the "*" because its added by the conversion [Begin]
    *=ARIABROW(['S'],LANG_ARIA_Scale_Title,gnBrHSRow1, gnBrHSCol1, gnBrHSRow2, gnBrHSCol2,'','','SCALE','laValues')
    =ARIABROW(['S' FOR SCALE <> '*'],LANG_ARIA_Scale_Title,gnBrHSRow1, gnBrHSCol1, gnBrHSRow2, gnBrHSCol2,'','','SCALE','laValues')    
    *B128783,1 KHM 07/12/2005 [End]
    lcParaScale = IIF(EMPTY(laValues[1]),SPACE(1),laValues[1])
  ENDIF  
ENDIF

SELECT (lnOldAlias)
RETURN !EMPTY(lcParaScale)



*!*************************************************************
*! Name      : gfObj_Lock
*! Developer : Hesham El_Sheltawi
*! Date      : 08/14/2002
*! Purpose   : Function to logicaly lock a record
*!*************************************************************
*! Calls     : gfModalGen
*!*************************************************************
*! Passed Parameters  :  Lock or unlock
*!                       
*!*************************************************************
*! Returns            :  .T. --> succeded
*!                       .F. --> unsuccess
*!*************************************************************
*! Example            :  gfObj_Lock(.T.)
*!*************************************************************
FUNCTION gfObj_Lock
PARAMETERS lLok_Set
PRIVATE lnRecNo,lRet_Flag
lnWorkArea = ALIAS()
IF EMPTY(lnWorkArea)
  RETURN
ENDIF
PRIVATE lnOldrpSt
SELECT (lnWorkArea)
lnDataSession = SET("DATASESSION")
lnAlias = SELECT()
lRet_Flag = .F.
lLok_It   = .F.
llLocked  = .F.
*** Go to the same record to get a fresh copy in the buffer
lnRecNo = RECNO()

DO WHILE .T.
  SELECT (lnWorkArea)
  IF lnRecNo <= RECCOUNT()
    GO lnRecNo
   llLocked = RLOCK() 
   UNLOCK RECORD lnRecNo
   IF llLocked
     TABLEREVERT(.F.)
   ENDIF 
   IF DELETED()
     =gfModalGen('INM00095B00000','ALERT')
     SELECT (lnAlias)
     *THIS.CHangemode("S")
     RETURN .F.
   ENDIF
  ENDIF  

  *** Chek if the record is in use by another user
  IF lLok_Set 
    *** Chek if the field cLok_User in the structur
    IF !lLok_Stat .AND. llLocked
      *** Record is not locked you may lock it
      lLok_It   = .T.
    ELSE
      lcLok_User = cLok_User
      IF !EMPTY(lcLok_User)
        IF ALLTRIM(lcLok_User) = ALLTRIM(oAriaApplication.User_ID)
          * Messaging the user that he cannot edit the same record
          * from more than one session and permit him from editing
          * the same record
          IF gfModalGen("INM00240B00006","ALERT")=2
            lLok_It    = .F.
            lRet_Flag  = .F.
          ELSE      
            lLok_It    = .T.
          ENDIF
        ELSE

          *We save old value of reprocess first.[START]
          lnOldrpSt = SET('REPROCESS')
          SET REPROCESS TO 1
          
					SET DATASESSION TO 1
					llLoop = .F.
					SELECT syuStatc
          IF SEEK ('INI'+'OLDVARS'+lcLok_User,'syuStatc') 
            LOCAL lnStatcRec
            SCAN REST WHILE cobj_typ+ALLTRIM(cobj_name)+cuser_id = 'INI'+'OLDVARS'+lcLok_User
                lnStatcRec = RECNO()
	            IF RLOCK('syuStatc')
  	            UNLOCK RECORD lnStatcRec IN  syuStatc 
        	      lLok_It    = .T.  	            
*!*	  	            lnStatcRec = RECNO()
*!*	    	          GO (oAriaApplication.UserStaticRecord) IN syuStatc 
*!*	      	        =RLOCK('syuStatc')
*!*	        	      GO lnStatcRec
          	  ELSE
            	  UNLOCK
              	 GO (oAriaApplication.UserStaticRecord) IN syuStatc 
              	=RLOCK('syuStatc') 
              	*** Display the message "Record is in use by user AAAA"
              	lcLok_User = oAriaApplication.getUserName(lcLok_User)
              	*** Record is in use by user ????    
              	SET DATASESSION TO (lnDataSession)
              	IF  gfModalGen("INM00028B00015","ALERT",lcLok_User) = 1
                	llLoop = .T.
              	ENDIF  
              	lLok_It    = .F.
              	lRet_Flag  = .F.
              	EXIT 
            	ENDIF
           ENDSCAN 	
          ELSE
            lLok_It    = .T. 
          ENDIF
          * Return the old value of reprocess.
          SET REPROCESS TO  lnOldrpSt
					SET DATASESSION TO (lnDataSession)
          IF llLoop
            LOOP 
          ENDIF 

        ENDIF
      ELSE
        *** Display the message "Record is in use by another"
        SET DATASESSION TO (lnDataSession)
        IF gfModalGen("INM00029B00015","ALERT") = 1
          LOOP
        ENDIF  
        lLok_It    = .F.
        lRet_Flag  = .F.
      ENDIF   
    ENDIF

  ELSE
    *** Chek if these three field in the file structur
    IF TYPE ('cLok_User') <> "U" .AND. ;
       TYPE ('dLok_Date') <> "U" .AND. ;
       TYPE ('cLok_Time') <> "U" 

      *** Unlock the record
      REPLACE lLok_Stat WITH .F. , ;   
              cLok_User WITH ""  , ;
              dLok_Date WITH {}  , ;
              cLok_Time WITH ""
			=TABLEUPDATE(0,.T.)              
      lRet_Flag  = .T.
    ENDIF  
  ENDIF

  EXIT
ENDDO

*** Chek if you have to lock the record or not
SET DATASESSION TO (lnDataSession)
IF lLok_It  
  *** Chek if these three field in the file structur
  IF TYPE ('cLok_User') <> "U" .AND. ;
     TYPE ('dLok_Date') <> "U" .AND. ;
     TYPE ('cLok_Time') <> "U" 
    *** Lock the record for this user with date and time
    REPLACE lLok_Stat WITH .T.       , ;   
             cLok_User WITH oAriaApplication.User_ID , ;
             dLok_Date WITH DATE()    , ;
             cLok_Time WITH gfGetTime()
    =TABLEUPDATE(0,.T.)
    lRet_Flag  = .T.    
  ENDIF
ENDIF
SELECT (lnWorkArea)
UNLOCK
SELECT (lnAlias)

RETURN lRet_Flag



*!**************************************************************************
*! PROG: FABROW.PRG
*! program to browse through the fabric file by using the new browse
*! written by Hesham El_Sheltawi 15/02/1995
*! NOTE: THIS PROGRAM ASSUMES THAT THE fabric FILE HAS BEEN OPENED
*!       IF WE WANT TO DISPLAY NO COLORS THEN XCLR = '*'
*!**************************************************************************
PROCEDURE FABROW

PARAMETERS XFAB,XCLR,llRetAlias,lcFabForEx
PRIVATE lcBrFields,lnCurAlias,lcStyle,laData

DECLARE laData[2]  && array to get values from browse
STORE '' TO laData
llBrowse = IIF(TYPE('llBrowse')='U',.T.,llBrowse) && variable to determine forcing browse or not
lcTitle = "Item"+IIF('*' $ XCLR,'',' color')  && variable to hold Browse title
llWasSel=.T.
lcStyle=IIF(XCLR=CHR(240),XFAB,'')
lnCurAlias = SELECT()

SELECT 0
USE (oAriaApplication.DataDir+'FABRIC') AGAIN ALIAS FABRIC_A ORDER TAG FABRIC
SELECT FABRIC
lcFabOrder = TAG()
IF '*' $ XCLR
  *--'ITEM_TYPE'
  lcItmType = gfCodDes(item_Type,'ITEM_TYPE')
  lcBrFields = [Fabric:14:h="Item",Color="******" :h="Color" :12,Desc:h="Description":35,]+;
               [lcItmType = gfCodDes(item_Type,'ITEM_TYPE') :h="Type",loc:14:h="Location",Vendor:16,]+;
               [Pattern:20,Onhand=lfSumFab(Fabric,'onhand'):14:h=]+;
               ["On Hand",OnOrder=lfSumFab(Fabric,'onorder'):14:h="On Order"]
ELSE
  lcBrFields = [Fabric:14:h="Item",Fabric_A.Color:12,Fabric_A.Desc:h="Description":35,]+;
               [lcItmType = gfCodDes(Fabric_A.item_Type,'ITEM_TYPE'):h="Type",Fabric_A.loc:14:h="Location",Fabric_A.Vendor:16,]+;
               [Fabric_A.Pattern:20,Fabric_A.Onhand:14:h="On Hand",Fabric_A.OnOrder:14:h="On Order"]
ENDIF  
SET ORDER TO TAG CFABRIC
SET RELATION TO FABRIC.FABRIC INTO FABRIC_A
IF !('*' $ XCLR)
  SET SKIP TO FABRIC_A
ENDIF  
IF llBrowse OR !SEEK(XFAB+XCLR,'FABRIC_A')
  lnSoftSeek=RECNO(0)
  IF lnSoftSeek<>0 AND BETWEEN(lnSoftSeek,1,RECCOUNT())
    GO lnSoftSeek
  ELSE
    GO TOP
  ENDIF
  IF EMPTY(lcFabForEx)
    llWasSel= ARIABROW([lcStyle],lcTitle,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,"","","FABRIC,FABRIC_A.COLOR","laData")
  ELSE
    SELECT FABRIC
    LOCATE &lcFabForEx
    IF FOUND()
      llWasSel= ARIABROW(lcFabForEx,lcTitle,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,"","","FABRIC,FABRIC_A.COLOR","laData")
    ELSE
      =gfModalGen('TRM00342B00000','DIALOG')
    ENDIF
  ENDIF
  IF llWasSel
    XFAB  = laData[1]
    XCLR  = laData[2]  
    SET ORDER TO TAG FABRIC
    SEEK xFab + xClr
  ELSE
    XFAB  = SPACE(7)
    XCLR  = SPACE(6)
  ENDIF  
ENDIF  

IF !EMPTY(lcFabOrder)
  SET ORDER TO TAG (lcFabOrder)
ELSE
  SET ORDER TO  
ENDIF  
SET SKIP TO
SET RELATION TO
USE IN FABRIC_A
IF llRetAlias
  SELECT (lnCurAlias)
ENDIF  
RETURN llWasSel

*!**************************************************************************
*! FUNCTION lfSumFab
*! FUNCTION to sum a specific field for the current style in style file
*! written by Hesham El_Sheltawi 15/02/1995
*!**************************************************************************
FUNCTION lfSumFab
PARAMETERS lcFab,lccomp
lnTotcomp = 0
SELECT Fabric_A
SUM &lcCOMP TO lnTotcomp WHILE Fabric=lcFab
SELECT Fabric
GO RECNO()
RETURN INT(lnTotcomp)


*!*************************************************************
*! Name : gfAPVnBrow.
*! Auth : Hisham Ramsis Philips (HISH).
*! Date : 13/03/95.
*!*************************************************************
*! Synopsis : Browse vendors from the AP files if system is seted to
*!            use AP link.
*!*************************************************************
*! Passed :
*!        Parameters : Vendor code.
*!*************************************************************
*! Returned : 
*!        Variables  : True if the user select or false if the user
*!                     ESC. from the browse.
*!*************************************************************
*! Example :
*!        =gfAPVnBrow( @lcVenCode )
*!*************************************************************
*! Screen layout :
*!    ....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....
*!03  ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
*!04  º VENDOR    .............NAME.............  .....PHONE......  OUR ACCOUNT     º
*!05  ÇÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¶
*!06  º ±±±±±±±±  ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±  ±±±±±±±±±±±±±±±±  ±±±±±±±±±±±±±±± º
*!..  º 12345678  123456789012345678901234567890  1234567890123456  123456789012345 º
*!22  ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
*!*************************************************************
FUNCTION gfAPVnBrow

PARAMETERS lcVenCode,llRetAlias , lcSubType
PRIVATE lcBrFields,lcAlias,laData , lcForExpr

DECLARE laData[1] && array to get values from browse
laData=' '
llOpnApVen=.F.
llBrowse = IIF(TYPE('llBrowse')='U',.T.,llBrowse) && variable to determine forcing browse or not
lcTitle  = 'A/P Vendors'

gcPhnFrmt  =gfPhoneTem()
lcBrFields = "cVendCode :R :H='Vendor':20,"+;
             "cVenComp  :R :H='Name':45,"+;
             "cPhoneNo  :R :P= gcPhnFrmt  :H='Phone':28,"+;
             "cVenOurAc :R :H='Our Account':28"

lcAlias = ALIAS()   && Save the current alias.

IF !USED( 'APVendor' )
  SELECT 0
  =gfOpenFile(oAriaApplication.DataDir+'ApVendor','VenCode','SH')
  llOpnApVen=.T.
ELSE  
  llOpnApVen=.F.
ENDIF
SELECT APVendor
GO TOP
IF EOF()
  = MESSAGEBOX('Vendor file is empty',16,_SCREEN.Caption)
  lcVenCode  = SPACE(08)
  IF llOpnApVen
    USE
  ENDIF
  IF llRetAlias
    SELECT (lcAlias)
  ENDIF  
  RETURN 
ELSE
  IF !SEEK (lcVenCode)
    IF RECNO(0)=0
      GO TOP
    ELSE
      GO RECNO(0)
    ENDIF
  ENDIF
ENDIF
IF TYPE('lcSubType') <> 'C'
  lcSubType = ''
ENDIF
STORE '' TO lcForExpr
IF !EMPTY(lcSubType)
  FOR lnForLoop = 1 TO LEN(ALLTRIM(lcSubType))
    IF lnForLoop > 1
      lcForExpr = lcForExpr + ' OR '
    ENDIF
    lcForExpr = lcForExpr + "'" + SUBSTR(ALLTRIM(lcSubType),lnForLoop,1) +;
                "'" + ' $ ' + 'cVenSupTyp'
  ENDFOR
ENDIF
IF EMPTY(lcForExpr)
  llWasSel  =ARIABROW('',lcTitle,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','',"cVendCode","laData")
ELSE
  llWasSel  =ARIABROW([FOR &lcForExpr],lcTitle,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','',"cVendCode","laData")
ENDIF

lcVenCode =IIF(llWasSel,laData[1],SPACE(8))
IF  llOpnApVen
  USE
ENDIF
IF llRetAlias
  SELECT (lcAlias)
ENDIF  
RETURN llWasSel
*-- EOF( gfAPVnBrow )


*!*************************************************************
*! Name : GETOBJ
*! Auth : Hesham El_Sheltawi
*! Date : 27/08/2002.
*!*************************************************************
*! Synopsis : call the object link screen
*!*!*************************************************************
*! Passed :
*!        Parameters : Object Type, Object ID
*!*************************************************************
*! Returned : 
*!        Variables  : 
*!*************************************************************
*! Example :
*!        =GetObj( "S","WOMCL-RED" )
*!*************************************************************
FUNCTION gfGetObj
PARAMETERS lcObjType,lcObjectID

* MAH OBJECT LINK START
*DO FORM (oAriaApplication.ScreenHome+"sy\OBJECTS") WITH lcObjType,lcObjectID
LOCAL loObject
loObject = CREATEOBJECT("AriaObjectBrowser.Loader")
loObject.LoadForm(oAriaApplication, _screen.ActiveForm.Parent, CREATEOBJECT('InterOperability'), oAriaApplication.carianativedatafilesconstr, lcObjType, lcObjectID)
* MAH End

RETURN
*!*************************************************************
*:************************************************************************
*: Program file  : GFBROWSE.PRG
*: Program desc. : 
*: For screen    :
*:         System: Aria advantage series
*:         Module: Main system 
*:      Developer: 
*:************************************************************************
*: Calls : 
*:         Procedures :
*:         Functions  : 
*:************************************************************************
*: Passed Parameters  : 
*:************************************************************************
*B600383,1 Hesham On 06/07/95
*B600383,1 Clear the key field if browse and there is no records in 
*B600383,1 the file.
*E600823,1 Hesham El-Sheltawi 05/16/96
*E600823,1 Create prefrence for each browse for any file according
*E600823,1 to the name of the function that is browsing the file
*E600823,1 and the file name
*B601183,1 Hesham El-Sheltawi On 07/29/96
*B601183,1 Change the position of the trapping for then enter and
*B601183,1 escape after the checking for the emptines of the file
*B800703,1 Hesham 08/21/96 
*B800703,1 the for condition is clearing after the Enhancment 
*B800703,1 made for the incrementel search 
*B601437,1 make the browse for command browse rest for
*B601456,1 Hesham El-Sheltawi 12/05/96
*B601456,1 Shut down the trapping of the enter and esc buttons
*B601660,1 Hesham El-Sheltawi 12/03/97
*B601660,1 Make the Global Browse Use one read Level
*B601660,1 Change the validation of the push button in the global browse
*B601660,1 window that branch to another screen to Terminate the read
*B601660,1 of the browse
*! E038142,2 MAH 09/16/2004 Full support for run forms with SQL with high Performance.
*! B038623,1 MAH 10/12/2004 Fix preference problems.
*:************************************************************************
FUNCTION GFBROWS
*! E038142,2 MAH 09/16/2004 Add addtional parameters [BEGIN]
*-- PARAMETER lcFltrExp,lcFieldsNam,lcArrName,lcBrowTitle,llIsFilter,lcAliasName,llGetTree
*! B038623,1 MAH 10/12/2004 Add Addtional Parmaeters [BEGIN]
*-- PARAMETER lcFltrExp, lcFieldsNam, lcArrName, lcBrowTitle, llIsFilter, lcAliasName, llGetTree, ;
*--           lcBrowseFileName, lcBrowseTableDBEngine, lcBrowsePKIndexName, lcBrowseIndexName, lcSeekToKey
PARAMETER lcFltrExp, lcFieldsNam, lcArrName, lcBrowTitle, llIsFilter, lcAliasName, llGetTree, ;
          lcBrowseFileName, lcBrowseTableDBEngine, lcBrowsePKIndexName, lcBrowseIndexName, lcSeekToKey, ;
          lcPreferenceKey, lcPreferenceSubKey
*! B038623,1 MAH 10/12/2004 [END]
*! E038142,2 MAH 09/16/2004 [END]

*! E038142,2 MAH 09/16/2004 Add addtional parameters [BEGIN]
*-- RETURN ARIABROW(lcFltrExp,lcBrowTitle,.F.,.F.,.F.,.F.,.F.,.F.,lcFieldsNam,lcArrName,.F.,lcAliasName,llGetTree)
*! B038623,1 MAH 10/12/2004 Add Addtional Parmaeters [BEGIN]
*-- RETURN ARIABROW(lcFltrExp, lcBrowTitle, .F., .F., .F., .F., .F., .F., ;
*--                 lcFieldsNam, lcArrName, .F., lcAliasName, llGetTree, ;
*--                 lcBrowseFileName, lcBrowseTableDBEngine, lcBrowsePKIndexName, lcBrowseIndexName, lcSeekToKey)
RETURN ARIABROW(lcFltrExp, lcBrowTitle, .F., .F., .F., .F., .F., .F., ;
                lcFieldsNam, lcArrName, .F., lcAliasName, llGetTree, ;
                lcBrowseFileName, lcBrowseTableDBEngine, lcBrowsePKIndexName, lcBrowseIndexName, lcSeekToKey, ;
                lcPreferenceKey, lcPreferenceSubKey)
*! B038623,1 MAH 10/12/2004 [END]                
*! E038142,2 MAH 09/16/2004 [END]

FUNCTION GFEXPONOF
lnBar = BAR()
lcPop = POPUP()
oAriaApplication.DisplayExplorer = !oAriaApplication.DisplayExplorer
SET MARK OF BAR lnBar OF (lcPop) oAriaApplication.DisplayExplorer


FUNCTION GFEXPTOP
lnBar = BAR()
lcPop = POPUP()
oAriaApplication.ExplorerOnTop = !oAriaApplication.ExplorerOnTop
SET MARK OF BAR lnBar OF (lcPop) oAriaApplication.ExplorerOnTop


*!*************************************************************
*! Name      : gfADel
*! Developer : Yasser Saad Ibrahime
*! Date      : 1993-1995 
*! Purpose   : To delete a colum or row from array
*!*************************************************************
*! Calls     : 
*!      Called by: GFUPDATSYS()             (function  in ARIA3.PRG)
*!*************************************************************
*! Passed Parameters  : array name
*!                      number of colum or row to be deleted
*!                      1 -> row  / 2 -> colum   
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : 
*!*************************************************************
*:->
FUNCTION gfADel
PARAMETERS laDel,lnDelWch,lnSubScrp

*B601481,1 This line was added by HS [Begin]
PRIVATE laTemp
*B601481,1 This line was added by HS [End]

lnSubScrp=IIF(TYPE("lnSubScrp") $ "UL",1,lnSubScrp)
DO CASE
  CASE lnSubScrp=1
    =ADEL(laDel,lnDelWch)
    IF ALEN(laDel,2)<>0
      IF ALEN(laDel,1)>1
        DIMENSION laDel[ALEN(laDel,1)-1,ALEN(laDel,2)]
      ENDIF  
    ELSE
      IF ALEN(laDel,1)>1
        DIMENSION laDel[ALEN(laDel,1)-1,1]
      ENDIF  
    ENDIF
    RETURN .T.
  CASE lnSubScrp=2
    DIMENSION laTemp[ALEN(laDel,1),ALEN(laDel,2)-1]
    FOR I=0 TO ALEN(laDel,1)-1
      IF lnDelWch>1 .AND. lnDelWch<ALEN(laDel,2)
        =ACOPY(laDel,laTemp,i*ALEN(laDel,2)+1,lnDelWch-1,i*ALEN(laTemp,2)+1)
        =ACOPY(laDel,laTemp,i*ALEN(laDel,2)+lnDelWch+1,ALEN(laDel,2)-lnDelWch,;
              i*ALEN(laTemp,2)+lnDelWch)
      ELSE
        IF lnDelWch=1
          =ACOPY(laDel,laTemp,i*ALEN(laDel,2)+2,ALEN(laDel,2)-1,i*ALEN(laTemp,2)+1)
        ELSE
          =ACOPY(laDel,laTemp,i*ALEN(laDel,2)+1,Alen(laDel,2)-1,i*ALEN(laTemp,2)+1)
        ENDIF        
      ENDIF        
    ENDFOR
   DIMENSION laDel[ALEN(laTemp,1),ALEN(laTemp,2)]
   RETURN (ACOPY(laTemp,laDel)>0)
ENDCASE


*************************************************************************
*FUNCTION FErrInfo
*DESC: Function get error information, which depends on transaction type. 
*NOTE: Called from Function Checkprd
*DATE: 02/28/1994
*AUTH: Wael Aly Mohamed 
*PARA: lcType   : Transaction type.
*    : lcDKind  : Date kind
*    : lcErrMes1: Message line #1
*    : lcErrMes2: Message line #2
*MODI:
*E100219,9 WAM 07/04/95 Add new type codes for receiving materials & styles
*E100219,9              from operation
*N000016,6 WAM 07/04/95 Add new type code for M.F.G. order receivin.
*E300324,6 RENEE 12/28/95 function fErrInfo() : Change text of type 'IN'
*E300324,6                AND 'V1' from 'System date' to 'Invoice date',
*E300324,6                and remove the checking message.
*************************************************************************
FUNCTION FErrInfo
PARAMETERS lcType,lcDKind,lcErrMes1,lcErrMes2

DO CASE
  CASE lcType = 'IN'                 && Invoice
    *E300324,6 Change message text.
    *&lcDKind   = 'System date '
    &lcDKind   = 'Invoice date '
    *E300324,6 end.
    
    &lcErrMes1 = 'Not allowed to create invoices for this date. '
    
    *E300324,6 Remove checking message text.
    *&lcErrMes2 = 'Please check the system date.'
    &lcErrMes2 = ''
    *E300324,6 end.  
    
  CASE lcType = 'VI1'                 && Void invoice 1st.
    &lcDKind   = 'System date '
    &lcErrMes1 = 'Not allowed to void invoices for this date. '
    &lcErrMes2 = 'Please check the system date.'
    
  CASE lcType = 'VI2'                 && Void Invoice 2nd.
    &lcDKind   = 'Invoice date '
    &lcErrMes1 = 'Not allowed to void invoices from prior periods.'
    &lcErrMes2 = ''
      
  CASE lcType = 'IA'                 && Inventory Adjustment
    &lcDKind   = 'Transaction date '
    &lcErrMes1 = 'Not allowed to enter inventory adjustment for this date.'
    &lcErrMes2 = ''
      
  CASE lcType = 'IP'                 && Inventory Physical
    &lcDKind   = 'Transaction date '
    &lcErrMes1 = 'Not allowed to enter physical inventory for this date.'
    &lcErrMes2 = ''
    
  CASE lcType = 'ZE'                 && Zero out Stock
    &lcDKind   = 'System date '
    &lcErrMes1 = 'Not allowed to zero out stock for this date. '
    &lcErrMes2 = 'Please check the system date.'
      
  CASE lcType = 'PO'                 && Receive P/Os
    &lcDKind   = 'System date '
    &lcErrMes1 = 'Not allowed to enter P/O receivings for this date. '
    &lcErrMes2 = 'Please check the system date.'
      
  CASE lcType = 'CT'                 && Receive C/Ts
    &lcDKind   = 'System date '
    &lcErrMes1 = 'Not allowed to enter C/T receivings for this date. '
    &lcErrMes2 = 'Please check the system date.'

  CASE lcType = 'RM'                 && Return merchandise
    &lcDKind   = 'System date '
    &lcErrMes1 = 'Not allowed to receive returns for this date. '
    &lcErrMes2 = 'Please check the system date.'
    
  CASE lcType = 'VR1'                 &&  Void Return 1st
    &lcDKind   = 'System date '
    &lcErrMes1 = 'Not allowed to void credit memo for this date. '
    &lcErrMes2 = 'Please check the system date.'
    
  CASE lcType = 'VR2'                 &&  Void Return 2nd
    &lcDKind   = 'Return date '
    &lcErrMes1 = 'Not allowed to void credit memo from prior periods.'
    &lcErrMes2 = ''
    
  CASE lcType = 'CR'                 &&  Customer Payment
    &lcDKind   = 'Batch date '
    &lcErrMes1 = 'Not allowed to enter payments for this batch date.'
    &lcErrMes2 = ''

  CASE lcType = 'AJ'                 &&  Adjustment
    &lcDKind   = 'Batch date '
    &lcErrMes1 = 'Not allowed to enter adjustments for this batch date.'
    &lcErrMes2 = ''
    
  CASE lcType = 'KO'                 &&  Key Off
    &lcDKind   = 'Key off date '
    &lcErrMes1 = 'Not allowed to make key off for this date.'
    &lcErrMes2 = ''

  CASE lcType = 'RO'                 &&  Receive from material operation
    &lcDKind   = 'Material operation receiving date '
    &lcErrMes1 = 'Not allowed to receive from material operation for this date.'
    &lcErrMes2 = ''

  CASE lcType = 'RS'                 &&  Receive from style operation
    &lcDKind   = 'Style operation receiving date '
    &lcErrMes1 = 'Not allowed to receive from style operation for this date.'
    &lcErrMes2 = ''

  CASE lcType = 'MM'                 &&  Receive M.F.G. order
    &lcDKind   = 'M.F.G. order receiving date '
    &lcErrMes1 = 'Not allowed to receive M.F.G. order for this date.'
    &lcErrMes2 = ''
  OTHERWISE
    &lcDKind   = 'Transaction date '
    &lcErrMes1 = ''
    &lcErrMes2 = ''
ENDCASE   

RETURN(.T.)

FUNCTION lfTrnsStr
PARAMETERS lcValueStr,lcDataType,lcDirection
DO CASE
  CASE lcDataType $ 'CM'
     RETURN ALLT(lcValueStr)
  CASE lcDataType = 'N'
      RETURN VAL(lcValueStr)
  CASE lcDataType='D'
     RETURN CTOD(lcValueStr)
  CASE lcDataType = 'L'
     RETURN IIF(UPPER(ALLTRIM(lcValueStr))='.F.',.F.,.T.)
ENDCASE


*!*************************************************************
*! Name      : gfToglErr
*! Developer : Hesham El-Sheltawi
*! Date      : 11/25/1998
*! Purpose   : function to togle error handler enabling
*!*************************************************************
*! Called from : Main Menu
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Example	      	 : gfToglErr()
*!*************************************************************
*E300797,1 Hesham (Start)
*E300797,1 function to enable and disable error handler
FUNCTION gfToglErr
glErrorHan = !glErrorHan
IF glErrorHan
*!*	    ON ERROR DO gfEHan WITH ERROR(), MESSAGE(), MESSAGE(1), ;
*!*	        SYS(16), LINENO(), SYS(102), SYS(100), SYS(101), LASTKEY(), ;
*!*	        ALIAS(), SYS(18), SYS(5), SYS(12), SYS(6), SYS(2003), WONTOP(), ;
*!*	        SYS(2011), SYS(2018), SET("CURSOR")
  oAriaApplication.SetError()
ELSE
  WAIT WINDOW "Clearing Error Handler" nowait
  ON ERROR
ENDIF
IF !EMPTY(BAR()) AND !EMPTY(POPUP())
  SET MARK OF BAR BAR() OF (POPUP()) glErrorHan
ENDIF
*E300797,1 Hesham (End)











*-- Added by Badran in 10/13/2002 ... BEGIN
*:************************************************************************
*: Program file  : GFGENFLT.PRG
*: Program desc. : 
*: For screen    :
*:         System: Aria advantage series
*:         Module: Main system 
*:      Developer: 
*:************************************************************************
*: Calls : 
*:         Procedures :
*:         Functions  : 
*:************************************************************************
*: Passed Parameters  : 
*!************************************************************************
*! Modifications :
*! E038142, 2 MAH 08/31/2004 Full support for run forms with SQL with high Performance.
*! E037237,2 MAH 10/01/2004 Enhance the interface of color scheme.
*! E038650,1 MAH 10/27/2004 Enhance the Filter.
*!************************************************************************
*:
FUNCTION gfGenFlt
*! E038142,2 MAH 08/31/2004 Add new paramter to pass the each table engine lcDBEngine [BEGIN]
*-- * N038038,1 SMM Add a parameter laFilterArray that will hold the expressions[Start]
*-- *--	PARAMETERS lcArray , llFilter , llUseArray
*-- PARAMETERS lcArray , llFilter , llUseArray ,laFilterArray 
*-- * N038038,1 SMM [End]

*! E038650,1 MAH 10/27/2004 Add new parameters: remove case sensitive and another to return english filter string [BEGIN] 
*-- PARAMETERS lcArray, llFilter, llUseArray, laFilterArray, lcDBEngine
PARAMETERS lcArray, llFilter, llUseArray, laFilterArray, lcDBEngine, llCaseInsensitive, llEnglishQuery
*! E038650,1 MAH 10/27/2004 [END] 
*! E038650,1 MAH 10/27/2004 Enhance the Filter.
lcDBEngine = IIF(TYPE('lcDBEngine') = 'C', lcDBEngine, oAriaApplication.cNativeDBID)

*! E038142,2 MAH 08/31/2004 [END]

llUseArray = IIF(TYPE('llUseArray') <> 'L' , .F. , llUseArray)

* N038038,1 SMM To check if the parameter was send or not[Start]
llUseFilterArray = IIF(Type('laFilterArray') <> 'C', .F. , .T.)
* N038038,1 SMM [End]

lcquery=''
lcElmSep='|'
lcValSep='~'

lcLineFeed=' ' &&+CHR(10)+CHR(13)
lnFltStart=1
DO WHILE (&lcArray[lnFltStart,1]='.OR.' OR EMPTY(&lcArray[lnFltStart,1]));
        AND !lnFltStart=ALEN(&lcArray,1)
  lnFltStart=lnFltStart+1
ENDDO 
lnFltEnd=ALEN(&lcArray,1)
DO WHILE (&lcArray[lnFltEnd,1]='.OR.' OR EMPTY(&lcArray[lnFltEnd,1]));
         AND lnFltEnd>1
  lnFltEnd=lnFltEnd-1
ENDDO 

lnOr=0       
IF lnFltStart>ALEN(&lcArray,1)
  RETURN ''
ENDIF
IF lnFltEnd=ALEN(&lcArray,1)
   lcWhichElm=lcArray+'['+ALLTRIM(STR(ALEN(&lcArray,1)))+',1]'
  IF TYPE(lcWhichElm)<>'C'
    RETURN ''
  ENDIF
  IF &lcArray[ALEN(&lcArray,1),1]='.OR.'
    RETURN ''
  ENDIF
ENDIF
lnCount=lnFltStart

DO WHILE  lnCount<=lnFltEnd
  *! E037237,2 MAH 10/01/2004 Remove the VAL becasue it condseder 0 is empty [BEGIN]
  *-- IF &lcArray[lnCount,3]='N' AND EMPTY(VAL(&lcArray[lnCount,6])) ;
  *--     AND &lcArray[lnCount,7]='V'
  *--   lnCount=lnCount+1
  *--   LOOP
  *-- ENDIF
  IF &lcArray[lnCount,3]='N' AND EMPTY(&lcArray[lnCount,6]) ;
      AND &lcArray[lnCount,7]='V'
    lnCount=lnCount+1
    LOOP
  ENDIF
  *! E037237,2 MAH 10/01/2004 [END]
  
  IF IIF(TYPE(ALLTRIM(lcArray) + "[lnCount,6]") = 'C' ,;
         !EMPTY(ALLTRIM(STRTRAN(STRTRAN(&lcArray[lnCount,6] , lcElmSep ,;
         '') , IIF(&lcArray[lnCount,3] = 'D' ,'/' , '') , ''))) ,;
         !EMPTY(&lcArray[lnCount,6])) .OR. &lcArray[lnCount,1] = '.OR.'
  
    IF !EMPTY(&lcArray[lnCount,1])
      IF &lcArray[lnCount,1]<>'.OR.'
        *! E038142,2 MAH 08/31/2004 Pass lcDBEngine to lfGetQCond [BEGIN]
        *-- lcQuery=lcQuery+lfGetQCond(lnCount,lcArray,llFilter)
        *! E038650,1 MAH 10/27/2004 apply new parameters remove case sensitive and return english filter string [BEGIN]
        *-- lcQuery = lcQuery + lfGetQCond(lnCount, lcArray, llFilter, lcDBEngine)
        lcQuery = lcQuery + lfGetQCond(lnCount, lcArray, llFilter, lcDBEngine, llCaseInsensitive, llEnglishQuery)
        *! E038650,1 MAH 10/27/2004 [END]         
        *! E038142,2 MAH 08/31/2004 [END]

    * N038038,1 SMM Filling laFilterArray with the filters one filter on the record[Start]
    IF llUseFilterArray && If laFilterArray parameter was passed
      *-- Fill the laFilterArray column 2 with the filter expression after removing | .AND. |   
      *! E038142,2 MAH 08/31/2004 Pass lcDBEngine to lfGetQCond [BEGIN]
      *-- laFilterArray[lnCount,2] = SUBSTR(lfGetQCond(lnCount,lcArray,llFilter),1,LEN(lfGetQCond(lnCount,lcArray,llFilter))-9)
      laFilterArray[lnCount, 2] = SUBSTR(lfGetQCond(lnCount, lcArray, llFilter, lcDBEngine), ;
                                         1, ;
                                         LEN(lfGetQCond(lnCount, lcArray, llFilter, lcDBEngine)) - 9)
      *! E038142,2 MAH 08/31/2004 [END]

      IF AT("SUBSTR",&lcArray[lnCount,1])>0
        lnTableFirst = AT("(",&lcArray[lnCount,1])+1 && get the position of "("
        lnTableEnd = AT(".",&lcArray[lnCount,1]) && get the position of "."
      ELSE  
        lnTableFirst = 1 
        lnTableEnd = AT(".",&lcArray[lnCount,1]) && get the position of "."
      ENDIF
      *-- Fill the laFilterArray column 1 with the Table Name
      laFilterArray[lnCount,1] = SUBSTR(&lcArray[lnCount,1],lnTableFirst,lnTableEnd-lnTableFirst)  
    ENDIF
    * N038038,1 SMM [END]
      ELSE
        lcQuery= IIF(RIGHT(lcQuery,9)=lcElmSep+' .AND. '+lcElmSep,SUBSTR(lcQuery,1,LEN(lcQuery)-9),lcQuery)
        IF lnOr>0
         lcQuery=lcQuery+' ) '
         lnOr=0
        ENDIF

        DO WHILE lnCount<lnFltEnd-1 AND (EMPTY(ALLTRIM(STRTRAN(STRTRAN(&lcArray[lnCount+1,6],lcElmSep,'');
           ,IIF(&lcArray[lnCount+1,3]='D','/  /',''),''))) OR &lcArray[lnCount+1,1]='.OR.')
          lnCount=lnCount+1
        ENDDO
        IF !EMPTY(ALLTRIM(STRTRAN(&lcArray[lnCount+1,6],lcElmSep,''))) AND !EMPTY(ALLTRIM(lcQuery))
           lcQuery=ALLTRIM(lcQuery)+' '+lcElmSep+' OR '+lcElmSep+'( '
          lnOr=1
        ENDIF   
      ENDIF  
    ENDIF 
  ENDIF
  lnCount=lnCount+1   
ENDDO
lcQuery= IIF(RIGHT(lcQuery,9)=lcElmSep+' .AND. '+lcElmSep,SUBSTR(lcQuery,1,LEN(lcQuery)-9),lcQuery)
 IF lnOr>0
   lcQuery=lcQuery+' ) '
 ENDIF    
 lcQuery=STRTRAN(lcQuery,lcElmSep+' .AND. '+lcElmSep,lcLineFeed+' AND '+lcLineFeed)
 lcQuery=STRTRAN(lcQuery,lcElmSep+' OR '+lcElmSep,lcLineFeed+' OR '+lcLineFeed) 
RETURN lcQuery
*-- end of gfGenFlt.

*!*****************************************************************************************
*! Name      : lfGetQCond
*! Developer : 
*! Date      : 10/13/2002 10:40:09 AM
*! Purpose   : Get Filter condition
*! Entry no. : N000398,1 - Build Aria3 Option Grid
*!*****************************************************************************************
*! Parameters: 
*!****************************************************************************************
*! Returns   : 
*!****************************************************************************************
*! Runs as following:
*!
*!************************************************************************
*! Modifications :
*! E038142,2 MAH 08/31/2004 Full support for run forms with SQL with high Performance.
*! E038650,1 MAH 10/27/2004 Enhance the Filter.
*! B999999,1 MAH 03/01/2005 Fix apply filter problem.
*!************************************************************************
FUNCTION lfGetQCond
*! E038142,2 MAH 08/31/2004 Add new paramter to pass the each table engine lcDBEngine [BEGIN]
*-- PARAMETERS lnCount,lcArray,llFilter

*! E038650,1 MAH 10/27/2004 Add new parameters: remove case sensitive and another to return english filter string [BEGIN] 
*-- PARAMETERS lcArray, llFilter, llUseArray, laFilterArray, lcDBEngine
PARAMETERS lnCount, lcArray, llFilter, lcDBEngine, llCaseInsensitive, llEnglishQuery
*! E038650,1 MAH 10/27/2004 [END] 

lcDBEngine = IIF(TYPE('lcDBEngine') = 'C', lcDBEngine, oAriaApplication.cNativeDBID)
*! E038142,2 MAH 08/31/2004 [END]

*! E038142,2 MAH 08/31/2004 Convert left side according DB engine [BEGIN]
LOCAL lcLeftSide
*! E038650,1 MAH 01/18/2005 [BEGIN]
*-- lcLeftSide = lfGetConditionLeftSide(&lcArray[lnCount, 1], lcDBEngine)
lcLeftSide = lfGetConditionLeftSide(&lcArray[lnCount, 1], lcDBEngine)
lcLeftSide = lcLeftSide + ' ' 
*! E038650,1 MAH 01/18/2005 [END]
*! E038142,2 MAH 08/31/2004 [END]

lcFiltExp=''
DO CASE
  CASE &lcArray[lnCount,5] = 'Contains'
    *! E038142,2 MAH 08/31/2004 Build condition according to DB Engine [BEGIN]
    DO CASE
      CASE ALLTRIM(lcDBEngine) == oAriaApplication.cNativeDBID
        *! E038142,2 MAH 08/31/2004 [END]
        
        *! E038650,1 MAH 10/27/2004 Apply case Insensitive and english query if required [BEGIN] 
        *-- lcFiltExp=IIF(&lcArray[lnCount,4],'','!(')+;
        *--           lfrightGet(&lcArray[lnCount,6],&lcArray[lnCount,3],;
        *--           &lcArray[lnCount,5],lcElmSep,&lcArray[lnCount,7])+;
        *--           ' $ '+ALLTRIM(&lcArray[lnCount,1])+' '+;
        *--           IIF(&lcArray[lnCount,4],'',' ) ')+lcElmSep+' .AND. '+lcElmSep
        IF llEnglishQuery
          lcFiltExp = ALLTRIM(&lcArray.[lnCount, 1]) + ;
                      IIF(&lcArray.[lnCount, 4], ' contains ', ' not contains ') + ;
                      lfRightGet(&lcArray.[lnCount, 6], ;
                                 &lcArray.[lnCount, 3], ;
                                 &lcArray.[lnCount, 5], ;
                                 lcElmSep, ;
                                 &lcArray.[lnCount, 7], ;
                                 lcDBEngine, ;
                                 llCaseInsensitive, ;
                                 .T.) + ;
                      lcElmSep + ' .AND. ' + lcElmSep
        ELSE
          lcFiltExp = IIF(&lcArray.[lnCount, 4], '',  '!(') + ;
                          lfRightGet(&lcArray.[lnCount, 6], ;
                                     &lcArray.[lnCount, 3], ;
                                     &lcArray.[lnCount, 5], ;
                                     lcElmSep, ;
                                     &lcArray.[lnCount, 7], ;
                                     lcDBEngine, ;
                                     llCaseInsensitive, ;
                                     llEnglishQuery) + ;
                          ' $ ' + ;
                          IIF(llCaseInsensitive, 'RTRIM(UPPER(', '') + ;
                          ALLTRIM(&lcArray.[lnCount, 1]) + ' ' + ;
                          IIF(llCaseInsensitive, '))', '') + ;
                          IIF(&lcArray.[lnCount, 4], '', ' )') + ;
                          lcElmSep + ' .AND. ' + lcElmSep
        ENDIF
        *! E038650,1 MAH 10/27/2004 [END]

        *! E038142,2 MAH 08/31/2004 Get DB Engine type [BEGIN]
      CASE ALLTRIM(lcDBEngine) == oAriaApplication.cSQLDBID
        *! E038650,1 MAH 01/18/2005 [BEGIN]
        *-- lcFiltExp = lcLeftSide + ;
                    IIF(&lcArray[lnCount, 4], "LIKE '" , "NOT LIKE '%") + ;
                    lfRightGet(&lcArray[lnCount, 6], ;
                               &lcArray[lnCount, 3], ;
                               &lcArray[lnCount, 5], ;
                               lcElmSep, ;
                               &lcArray[lnCount, 7], ;
                               lcDBEngine) + '% ' + ;
                    lcElmSep + ' .AND. ' + lcElmSep
        LOCAL lcRight
        lcRight = lfRightGet(&lcArray[lnCount, 6], ;
                             &lcArray[lnCount, 3], ;
                             &lcArray[lnCount, 5], ;
                             lcElmSep, ;
                             &lcArray[lnCount, 7], ;
                             lcDBEngine)
        lcRight = ALLTRIM(lcRight)              
        lcRight = SUBSTR(lcRight, 2, LEN(lcRight) - 2)
        *! B999999,1 MAH 03/01/2005 [BEGIN]
        *-- IF ALLTRIM(lcArray[lnCount, 7]) == 'F'
        IF ALLTRIM(&lcArray[lnCount, 7]) == 'F'
        *! B999999,1 MAH 03/01/2005 [END]
          lcRight = SUBSTR(lcRight, 2, LEN(lcRight) - 2)
        ENDIF
        lcFiltExp = lcLeftSide + ;
                    IIF(&lcArray[lnCount, 4], "LIKE '%" , "NOT LIKE '%") + ;
                    lcRight + "%' " + ;
                    lcElmSep + ' .AND. ' + lcElmSep
        *! E038650,1 MAH 01/18/2005 [END]
    ENDCASE
    *! E038142,2 MAH 08/31/2004 [END]
                 
  CASE &lcArray[lnCount,5] = 'Like' OR &lcArray[lnCount,5] = 'Exactly Like'
    *! E038142,2 MAH 08/31/2004 Build condition according to DB Engine [BEGIN]
    DO CASE
      CASE ALLTRIM(lcDBEngine) == oAriaApplication.cNativeDBID
        *! E038142,2 MAH 08/31/2004 [END]
        
        *! E038650,1 MAH 10/27/2004 Apply case Insensitive and english query if required [BEGIN]
        *-- lcFiltExp=IIF(&lcArray[lnCount,3]='D',"DTOS(",'')+ALLTRIM(&lcArray[lnCount,1]);
        *--           +IIF(&lcArray[lnCount,3]='D',")",'')+' '+IIF(&lcArray[lnCount,4],;
        *--            IIF(&lcArray[lnCount,5] = 'Like','=','=='),'<>')+' '+;
        *--           lfrightGet(&lcArray[lnCount,6],&lcArray[lnCount,3],;
        *--          &lcArray[lnCount,5],lcElmSep,&lcArray[lnCount,7])+lcElmSep+' .AND. '+lcElmSep
        lcFiltExp = IIF(llEnglishQuery, '', IIF(&lcArray.[lnCount, 3] = 'D', 'DTOS(', '')) + ;
                    IIF(llCaseInsensitive .AND. &lcArray.[lnCount, 3] $ 'CM', IIF(&lcArray.[lnCount, 4], '', '!('), '') + ;
                    IIF(llCaseInsensitive .AND. &lcArray.[lnCount, 3] $ 'CM', 'UPPER(RTRIM(', '') + ;
                    ALLTRIM(&lcArray.[lnCount, 1]) + ;
                    IIF(llCaseInsensitive .AND. &lcArray.[lnCount, 3] $ 'CM', '))', '') + ;
                    IIF(llEnglishQuery, '', IIF(&lcArray.[lnCount, 3] = 'D', ')', '')) + ;
                    IIF(llEnglishQuery, ;
                        IIF(&lcArray.[lnCount, 4], ' like ', ' not like '), ;
                        IIF(llCaseInsensitive  .AND. &lcArray.[lnCount, 3] $ 'CM', ;
                            ' == ', ;
                            IIF(&lcArray.[lnCount, 4], ;
                                IIF(&lcArray.[lnCount, 5] = 'Like', ' = ', ' == '), ;
                                ' <> '))) + ;
                    lfRightGet(&lcArray.[lnCount, 6], ;
                               &lcArray.[lnCount, 3], ;
                               &lcArray.[lnCount, 5], ;
                               lcElmSep, ;
                               &lcArray.[lnCount, 7], ;
                               lcDBEngine, ;
                               llCaseInsensitive, ;
                               llEnglishQuery) + ;
                    IIF(llCaseInsensitive .AND. &lcArray.[lnCount, 3] $ 'CM', IIF(&lcArray.[lnCount, 4], '', ')'), '') + ;
                    lcElmSep + ' .AND. ' + lcElmSep
        *! E038650,1 MAH 10/27/2004 [END]

        *! E038142,2 MAH 08/31/2004 Get DB Engine type [BEGIN]
      CASE ALLTRIM(lcDBEngine) == oAriaApplication.cSQLDBID
        lcFiltExp = IIF(&lcArray[lnCount, 3] = 'D', 'CONVERT(CHAR(8), ', ' ') + ;
                    lcLeftSide + ;
                    IIF(&lcArray[lnCount, 3] = 'D', ', 112) ', ' ') + ;
                    IIF(&lcArray[lnCount, 4], '= ', '<> ') + ;
                    lfRightGet(&lcArray[lnCount, 6], ;
                               &lcArray[lnCount, 3], ;
                               &lcArray[lnCount, 5], ;
                               lcElmSep, ;
                               &lcArray[lnCount, 7], ;
                               lcDBEngine) + ' ' + ;
                               lcElmSep + ' .AND. ' + lcElmSep
    ENDCASE
    *! E038142,2 MAH 08/31/2004 [END]
               
  CASE INLIST(&lcArray[lnCount,5],'Greater Than','Less Than','Greater Or Equal',;
              'Less Or Equal')     
    *! E038142,2 MAH 08/31/2004 Build condition according to DB Engine [BEGIN]
    DO CASE
      CASE ALLTRIM(lcDBEngine) == oAriaApplication.cNativeDBID
        *! E038142,2 MAH 08/31/2004 [END]
        
        *! E038650,1 MAH 10/27/2004 Apply case Insensitive and english query if required [BEGIN]
        *-- lcOperator=lfGetOper(ALLTRIM(&lcArray[lnCount,5]),&lcArray[lnCount,4])              
        *-- lcFiltExp=IIF(&lcArray[lnCount,4],'','!(')+;
        *--         IIF(&lcArray[lnCount,3]='D',"DTOS(",'')+ALLTRIM(&lcArray[lnCount,1]);
        *--           +IIF(&lcArray[lnCount,3]='D',")",'')+' '+lcOperator+' '+;
        *--           lfrightGet(&lcArray[lnCount,6],&lcArray[lnCount,3],;
        *--          &lcArray[lnCount,5],lcElmSep,&lcArray[lnCount,7])+IIF(&lcArray[lnCount,4],'',' ) ')+;
        *--          lcElmSep+' .AND. '+lcElmSep
        lcOperator = lfGetOper(ALLTRIM(&lcArray.[lnCount, 5]), &lcArray[lnCount, 4], llEnglishQuery)
        lcFiltExp  = IIF(llEnglishQuery, '', IIF(&lcArray.[lnCount, 4], '', '!(')) + ;
                     IIF(llEnglishQuery, '', IIF(&lcArray.[lnCount, 3] = 'D', 'DTOS(', '')) + ;
                     IIF(llCaseInsensitive .AND. &lcArray.[lnCount, 3] $ 'CM', 'UPPER(RTRIM(', '') + ;
                     ALLTRIM(&lcArray.[lnCount, 1]) + ;
                     IIF(llCaseInsensitive .AND. &lcArray.[lnCount, 3] $ 'CM', '))', '') + ;
                     IIF(llEnglishQuery, '', IIF(&lcArray.[lnCount, 3] = 'D',')', '')) + ;
                     ' ' + lcOperator + ' ' + ;
                     lfRightGet(&lcArray.[lnCount, 6], ;
                                &lcArray.[lnCount, 3], ;
                                &lcArray.[lnCount, 5], ;
                                lcElmSep, ;
                                &lcArray.[lnCount, 7], ;
                                lcDBEngine, ;
                                llCaseInsensitive, ;
                                llEnglishQuery) + ;
                     IIF(llEnglishQuery, '', IIF(&lcArray.[lnCount, 4], '', ' )')) + ;
                     lcElmSep + ' .AND. ' + lcElmSep
        *! E038650,1 MAH 10/27/2004 [END]

        *! E038142,2 MAH 08/31/2004 Get DB Engine type [BEGIN]
      CASE ALLTRIM(lcDBEngine) == oAriaApplication.cSQLDBID
        lcOperator = lfGetOper(ALLTRIM(&lcArray[lnCount, 5]), &lcArray[lnCount, 4])
        lcFiltExp  = IIF(&lcArray[lnCount, 4], ' ' , 'NOT (') + ;
                     IIF(&lcArray[lnCount, 3]='D', 'CONVERT(CHAR(8), ' ,' ') + ;
                     lcLeftSide + ;
                     IIF(&lcArray[lnCount, 3] = 'D', ', 112) ', ' ') + ;
                     lcOperator + ' ' + ;
                     lfRightGet(&lcArray[lnCount, 6], ;
                                &lcArray[lnCount, 3], ;
                                &lcArray[lnCount, 5], ;
                                lcElmSep, ;
                                &lcArray[lnCount, 7], ;
                                lcDBEngine) + ;
                      IIF(&lcArray[lnCount, 4], ' ', ') ') + ;
                     lcElmSep + ' .AND. ' + lcElmSep                              
    ENDCASE
    *! E038142,2 MAH 08/31/2004 [END]

  CASE &lcArray[lnCount,5] = 'Between'
    IF llFilter
      *! E038650,1 MAH 10/27/2004 Apply case Insensitive if required [BEGIN]
      *-- lcFiltExp=IIF(&lcArray[lnCount,4],'BETWEEN(','!BETWEEN(')+;
      *--         IIF(&lcArray[lnCount,3]='D',"DTOS(",'')+ALLTRIM(&lcArray[lnCount,1]);
      *--             +IIF(&lcArray[lnCount,3]='D',")",'')+','+;
      *--             lfrightGet(&lcArray[lnCount,6],&lcArray[lnCount,3],;
      *--             &lcArray[lnCount,5],lcElmSep,&lcArray[lnCount,7])+;
      *--             ')'+lcElmSep+' .AND. '+lcElmSep
      lcFiltExp = IIF(&lcArray.[lnCount, 4], 'BETWEEN(', '!BETWEEN(') + ;
                  IIF(&lcArray.[lnCount, 3] = 'D', 'DTOS(', '') + ;
                  IIF(llCaseInsensitive .AND. &lcArray.[lnCount, 3] $ 'CM', 'UPPER(RTRIM(', '') + ;
                  ALLTRIM(&lcArray.[lnCount, 1]) + ;
                  IIF(llCaseInsensitive .AND. &lcArray.[lnCount, 3] $ 'CM', '))', '') + ;
                  IIF(&lcArray.[lnCount, 3] = 'D', ')', '') + ',' + ;
                  lfRightGet(&lcArray.[lnCount, 6], ;
                             &lcArray.[lnCount, 3], ;
                             &lcArray.[lnCount, 5], ;
                             lcElmSep, ;
                             &lcArray.[lnCount, 7], ;
                             lcDBEngine, ;                             
                             llCaseInsensitive, ;
                             llEnglishQuery) + ;
                  ')' + lcElmSep + ' .AND. ' + lcElmSep
      *! E038650,1 MAH 10/27/2004 [END]
    ELSE
      *! E038142,2 MAH 08/31/2004 Build condition according to DB Engine [BEGIN]
      DO CASE
        CASE ALLTRIM(lcDBEngine) == oAriaApplication.cNativeDBID
          *! E038142,2 MAH 08/31/2004 [END]
          
          *! E038650,1 MAH 10/27/2004 Apply case Insensitive and english query if required [BEGIN]
          *-- lcFiltExp = IIF(&lcArray[lnCount,4],'','!(')+;
          *--             IIF(&lcArray[lnCount,3]='D',"DTOS(",'')+ALLTRIM(&lcArray[lnCount,1]);
          *--             +IIF(&lcArray[lnCount,3]='D',")",'')+' BETWEEN '+;
          *--             lfrightGet(&lcArray[lnCount,6],&lcArray[lnCount,3],;
          *--             &lcArray[lnCount,5],lcElmSep,&lcArray[lnCount,7])+;
          *--             IIF(&lcArray[lnCount,4],'',')')+lcElmSep+' .AND. '+lcElmSep    
          lcFiltExp = IIF(llEnglishQuery, '', IIF(&lcArray.[lnCount, 4], '', '!(')) + ;
                      IIF(llEnglishQuery, '', IIF(&lcArray.[lnCount, 3] = 'D', 'DTOS(', '')) + ;
                      IIF(llCaseInsensitive .AND. &lcArray.[lnCount, 3] $ 'CM', 'UPPER(RTRIM(', '') + ;
                      ALLTRIM(&lcArray.[lnCount, 1]) + ;
                      IIF(llCaseInsensitive .AND. &lcArray.[lnCount, 3] $ 'CM', '))', '') + ;
                      IIF(llEnglishQuery, '', IIF(&lcArray.[lnCount, 3] = 'D', ')', '')) + ;
                      IIF(llEnglishQuery, ; 
                          IIF(&lcArray.[lnCount, 4], ' between ', ' not between '), ;
                          ' BETWEEN ') + ;
                      lfRightGet(&lcArray.[lnCount, 6], ;
                                 &lcArray.[lnCount, 3], ;
                                 &lcArray.[lnCount, 5], ;
                                 lcElmSep, ;
                                 &lcArray.[lnCount, 7], ;
                                 lcDBEngine, ;
                                 llCaseInsensitive, ;
                                 llEnglishQuery) + ;
                      IIF(llEnglishQuery,  '', IIF(&lcArray.[lnCount, 4], '', ')')) + ;
                      lcElmSep + ' .AND. ' + lcElmSep    
          *! E038650,1 MAH 10/27/2004 [END]

          *! E038142,2 MAH 08/31/2004 Get DB Engine type [BEGIN]
        CASE ALLTRIM(lcDBEngine) == oAriaApplication.cSQLDBID
          lcFiltExp = IIF(&lcArray[lnCount, 4], ' ' , 'NOT ( ') + ;
                      IIF(&lcArray[lnCount, 3]='D', 'CONVERT(CHAR(8), ', ' ') + ;
                      lcLeftSide + ;
                      IIF(&lcArray[lnCount, 3] = 'D', ', 112) ', ' ') + ;
                      ' BETWEEN '+ ;
                      lfrightGet(&lcArray[lnCount, 6], ;
                                 &lcArray[lnCount, 3], ;
                                 &lcArray[lnCount, 5], ;
                                 lcElmSep, ;
                                 &lcArray[lnCount, 7], ;
                                 lcDBEngine) + ;
                      IIF(&lcArray[lnCount, 4], ' ', ') ') + ;
                      lcElmSep + ' .AND. ' + lcElmSep    
      ENDCASE
      *! E038142,2 MAH 08/31/2004 [END]
    ENDIF  
  CASE &lcArray[lnCount,5] = 'In List'
    IF &lcArray[lnCount,7] <> 'R'
      IF !llUseArray OR OCCURS("|",&lcArray[lnCount,6]) < 25 
        IF llFilter
          lcFiltExp=IIF(&lcArray[lnCount,4],'INLIST(','!INLIST(')+;
                    IIF(&lcArray[lnCount,3]='D',"DTOS(",'')+ALLTRIM(&lcArray[lnCount,1]);
                       +IIF(&lcArray[lnCount,3]='D',")",'')+','+;
                       lfrightGet(&lcArray[lnCount,6],&lcArray[lnCount,3],;
                       &lcArray[lnCount,5],lcElmSep,&lcArray[lnCount,7])+;
                       ')'+lcElmSep+' .AND. '+lcElmSep
        ELSE
          lcFiltExp= IIF(&lcArray[lnCount,4],'','!(')+;
                        IIF(&lcArray[lnCount,3]='D',"DTOS(",'')+ALLTRIM(&lcArray[lnCount,1]);
                       +IIF(&lcArray[lnCount,3]='D',")",'')+' IN('+;
                       lfrightGet(&lcArray[lnCount,6],&lcArray[lnCount,3],;
                       &lcArray[lnCount,5],lcElmSep,&lcArray[lnCount,7])+')'+;
                       IIF(&lcArray[lnCount,4],'',')')+lcElmSep+' .AND. '+lcElmSep    
        ENDIF    
      ELSE    && Else [If the function can use the filter array in the filter expression]
        DO CASE
          
          CASE &lcArray[lnCount,3] = 'N'
            *-- Variable to hold the Numeric field Picture
            lcPict    = IIF(ATC(lcElmSep , &lcArray[lnCount,6]) = 0 ,;
                            &lcArray[lnCount,6] ,;
                            SUBSTR(&lcArray[lnCount,6] , 1 ,;
                                   ATC(lcElmSep , &lcArray[lnCount,6]) - 1))
            
            lcPict    = STUF(REPLICATE('9' , LEN(lcPict)) ,;
                             ATC('.' , lcPict) ,;
                             IIF(ATC('.' , lcPict) = 0 , 0 , 1) ,;
                             IIF(ATC('.' , lcPict) = 0 , '' , '.'))
            
            *-- Variable to hold the Numeric field Length (In character
            *-- string)
            lcLength  = ALLTRIM(STR(LEN(lcPict)))
            
            *-- Variable to hold the Numeric field Decimal Length (In
            *-- character string)
            lcDecimal = ALLTRIM(STR(IIF(AT('.' , lcPict) = 0 , 0 ,;
                                        LEN(lcPict) - AT('.' , lcPict))))
            
            lcFiltExp = IIF(&lcArray[lnCount,4] , '(' , '!(') +;
                        'STR(' + ALLTRIM(&lcArray[lnCount,1]) +;
                        ' , ' + lcLength + ' , ' + lcDecimal + ')' +;
                        ' $ ' + ALLTRIM(lcArray) +;
                        '[' + ALLTRIM(STR(lnCount)) + ',6]' + ')' +;
                        lcElmSep + ' .AND. ' + lcElmSep
            
          CASE &lcArray[lnCount,3] = 'D'
            lcFiltExp = IIF(&lcArray[lnCount,4] , '(' , '!(') +;
                        'DTOC(' + ALLTRIM(&lcArray[lnCount,1]) + ')' +;
                        ' $ ' + ALLTRIM(lcArray) +;
                        '[' + ALLTRIM(STR(lnCount)) + ',6]' + ')' +;
                        lcElmSep + ' .AND. ' + lcElmSep
            
          OTHERWISE
            lcFiltExp = IIF(&lcArray[lnCount,4] , '(' , '!(') +;
                        ALLTRIM(&lcArray[lnCount,1]) +;
                        ' $ '+ ALLTRIM(lcArray) +;
                        '[' + ALLTRIM(STR(lnCount)) + ',6]' + ')' +;
                        lcElmSep + ' .AND. ' + lcElmSep
            
        ENDCASE
      ENDIF    && End of IF !llUseArray
      
    *In list operator [Begin]
    ELSE    && Else [If this filter option is using the In range screen]
      
      IF USED(&lcArray[lnCount,6]) .AND. SEEK('' , &lcArray[lnCount,6])
        PRIVAT lnSavAls , lnSelCnt , lcInlstExp , lcOptExp , lcKey
        lnSavAls   = SELECT(0)        && Save the old alias.
        lnSelCnt   = 0                && Variable to be used to count the user selection
        lcInlstExp = ""               && Variable to hold the user selection
        lcKey      = ''
        
        *-- Get an optimize-able expression, if applicable.
        lcOptExp   = lfGetOptEx(lcArray , lnCount , @lcKey)
        
        SELECT (&lcArray[lnCount,6])
        
        *-- Scan loop to count and get the user selection as far as it
        *-- doesn't exceeds 24 selection.
        SCAN WHILE lnSelCnt <= 25
          *-- count the user selection
          lnSelCnt   = lnSelCnt   + 1
          *-- Accumulate the key - if there is one - and the user selection
		  *B609328,1 MMT 07/01/2010 material master file listing crashes when fabric has "'"[Start]          
          *lcInlstExp = lcInlstExp + ',"' + lcKey + EVAL(KEY()) + '"'		  
          lcInlstExp = lcInlstExp + ",'" + lcKey + EVAL(KEY()) + "'"
		  *B609328,1 MMT 07/01/2010 material master file listing crashes when fabric has "'"[End]          
        ENDSCAN    && End of SCAN WHILE lnSelCnt <= 25
        
        *-- If the user selected more than 24 selection
        IF lnSelCnt > 24
          
          *-- Use BETWEEN() function, if applicable.
          IF &lcArray[lnCount,4]
            GO TOP
  		    *B609328,1 MMT 07/01/2010 material master file listing crashes when fabric has "'"[Start]                      
*!*	            lcFiltExp = 'BETWEEN(' + lcOptExp +;
*!*	                        ',"' + lcKey + EVAL(KEY()) + '",'
            lcFiltExp = 'BETWEEN(' + lcOptExp +;
                        ",'" + lcKey + EVAL(KEY()) + "',"
	  	    *B609328,1 MMT 07/01/2010 material master file listing crashes when fabric has "'"[End]                      
            GO BOTTOM   
  		    *B609328,1 MMT 07/01/2010 material master file listing crashes when fabric has "'"[Start]                                  
*!*	            lcFiltExp = lcFiltExp + '"' + lcKey + EVAL(KEY()) + '")' +;
*!*	                        lcElmSep + ' .AND. ' + lcElmSep
            lcFiltExp = lcFiltExp + "'" + lcKey + EVAL(KEY()) + "')" +;
                        lcElmSep + ' .AND. ' + lcElmSep
  		    *B609328,1 MMT 07/01/2010 material master file listing crashes when fabric has "'"[End]                                              
            
          ENDIF    && End of IF &lcArray[lnCount,4]
          
          *-- Use the SEEK() function
          lcFiltExp = lcFiltExp + IIF(&lcArray[lnCount,4] , '' , '!') + 'SEEK(' + ;
                      ALLTRIM(&lcArray[lnCount,1]) + ' , "' + ;
                      &lcArray[lnCount,6] + '")' +;
                      lcElmSep + ' .AND. ' + lcElmSep
        
        *-- Use INLIST when the selection less than 24              
        ELSE    && Else (If the user selected 24 selection or less.)
          lcFiltExp = 'INLIST(' + lcOptExp + lcInlstExp + ;
                      ')' + lcElmSep + ' .AND. ' + lcElmSep
        ENDIF                         
        SELECT (lnSavAls)
        
      ELSE    && Else [If the In Range cursor is not opened or if it has no records]
        lcFiltExp = ''
      ENDIF    && End of IF USED(&lcArray[lnCount,6]) .AND. SEEK('' , &lcArray[lnCount,6])
    ENDIF    && End of IF &lcArray[lnCount,7] <> 'R'
    
ENDCASE

*-- BADRAN (N000398,1 - Build Aria3 Option Grid)
*-- IF it's 8 dimension array, save the filter expression to column 8...BEGIN
IF ALEN(&lcArray.,2) = 8
  LOCAL lnAtElmSep
  lnAtElmSep = RAT(lcElmSep,lcFiltExp,2)
  &lcArray.[lnCount,8] = LEFT(lcFiltExp,lnAtElmSep-1)
ENDIF 
*-- IF it's 8 dimension array, save the filter expression to column 8...END
RETURN lcFiltExp && 
*--  end of lfGetQCond.

*!*****************************************************************************************
*! Name      : lfGetOper
*! Developer : 
*! Date      : 10/13/2002 10:41:02 AM
*! Purpose   : Get Filter operator
*! Entry no. : N000398,1 - Build Aria3 Option Grid
*!*****************************************************************************************
*! Parameters: 
*!****************************************************************************************
*! Returns   : 
*!****************************************************************************************
*! Runs as following:
*!
*!****************************************************************************************
*! Modifications :
*! E038650,1 MAH 10/27/2004 Enhance the Filter.
*!****************************************************************************************
FUNCTION lfGetOper

*! E038650,1 MAH 10/27/2004 Add new parameter to return english filter string [BEGIN] 
*-- PARAMETER lcOperator,llisnot
PARAMETERS lcOperator, llisnot, llEnglishQuery
*! E038650,1 MAH 10/27/2004 [END] 

*! E038650,1 MAH 10/27/2004 Apply return english query if required [BEGIN]
IF llEnglishQuery
  DO CASE
    CASE lcOperator = 'Greater Than'
       RETURN IIF(llIsNot, '', 'not ') + 'greater than'
       
    CASE lcOperator = 'Less Than'
       RETURN IIF(llIsNot, '', 'not ') + 'less than'
       
    CASE lcOperator = 'Greater Or Equal'
      RETURN IIF(llIsNot, '', 'not ') + 'greater or equal'
      
    CASE lcOperator = 'Less Or Equal'
      RETURN IIF(llIsNot, '', 'not ') + 'less or equal'
  ENDCASE  
ELSE
  *! E038650,1 MAH 10/27/2004 [END]
  DO CASE
    CASE lcOperator = 'Greater Than'
       RETURN '>'
    CASE lcOperator = 'Less Than'
       RETURN '<'
    CASE lcOperator = 'Greater Or Equal'
      RETURN '>='
    CASE lcOperator = 'Less Or Equal'
      RETURN '<='
  ENDCASE  
  *! E038650,1 MAH 10/27/2004 Apply return english query if required [BEGIN]
ENDIF
*! E038650,1 MAH 10/27/2004 [END]

*-- end of lfGetOper.

*!*****************************************************************************************
*! Name      : lfRightGet
*! Developer : 
*! Date      : 10/13/2002 10:41:47 AM
*! Purpose   : Get Right value
*! Entry no. : N000398,1 - Build Aria3 Option Grid
*!*****************************************************************************************
*! Parameters: 
*!****************************************************************************************
*! Returns   : 
*!****************************************************************************************
*! Runs as following:
*!
*!****************************************************************************************
*! Modifications :
*! 08/31/2004 Full support for run forms with SQL with high Performance.
*! E038650,1 MAH 10/27/2004 Enhance the Filter.
*! E037885,2 MAH 12/03/2004 Separate screen in different session.
*! B127002,1 MAH 04/05/2005 Greater than condition not returning correct values.
*! B039435,1 MAH Fix bug of there is a quote in the field value.
*! B129154,1 MAH 07/28/2005 Error in PO Report.
*!****************************************************************************************

FUNCTION lfRightGet
*! E038142,2 MAH 08/31/2004 Add new paramter to pass the each table engine lcDBEngine [BEGIN]
*-- PARAMETERS mRightHead,cLeftType,cOperator,lcElmSep,cRightType

*! E038650,1 MAH 10/27/2004 Add new parameters: remove case sensitive and another to return english filter string [BEGIN] 
*-- PARAMETERS mRightHead, cLeftType, cOperator, lcElmSep, cRightType, lcDBEngine
PARAMETERS mRightHead, cLeftType, cOperator, lcElmSep, cRightType, lcDBEngine, llCaseInsensitive, llEnglishQuery
*! E038650,1 MAH 10/27/2004 [END] 

lcDBEngine = IIF(TYPE('lcDBEngine') = 'C', lcDBEngine, oAriaApplication.cNativeDBID)
*! E038142,2 MAH 08/31/2004 [END]

lcRetVal=mRightHead
DO CASE
  CASE cRightType='V'
    DO CASE      
      CASE cLeftType $ 'CM'
        IF INLIST(COPERATOR,'Between','In List')
          *! E038142,2 MAH 08/31/2004 Apply return english query parameters [BEGIN]
          *-- lcSeper=IIF(!llFilter AND cOperator='Between',' AND ',',')
          lcSeper = IIF(!llFilter AND cOperator = 'Between', IIF(llEnglishQuery, ' and ', ' AND '), ',')
          *! E038142,2 MAH 08/31/2004 [END]
          
          *! E038142,2 MAH 08/31/2004 Apply remove case sensitive  parameters [BEGIN]
          *-- lcRetVal='"'+STRTRAN(mRightHead,lcElmSep,'"'+lcSeper+'"')+'"'
          
          *! B039435,1 MAH Fix bug of there is a quote in the field value [BEGIN]
          *-- IF llCaseInsensitive
          *--   IF !EMPTY(mRightHead)
          *--     LOCAL lcRight
          *--     lcRight = UPPER(mRightHead)
          *--     
          *--     LOCAL laRight[1]
          *--     =gfSubStr(lcRight, @laRight, lcElmSep)
          *--     
          *--     lcRight = ''
          *--     LOCAL lnIndex
          *--     FOR lnIndex = 1 TO ALEN(laRight, 1)
          *--       lcRight = lcRight + laRight[lnIndex] + lcElmSep
          *--     ENDFOR
          *--     lcRight = SUBSTR(lcRight, 1, LEN(lcRight) - 1)
          *--   ENDIF
          *--   
          *--   lcRetVal = '"' + STRTRAN(UPPER(lcRight), lcElmSep, '"' + lcSeper + '"') + '"'
          *-- ELSE
          *--   lcRetVal = '"' + STRTRAN(mRightHead, lcElmSep, '"' + lcSeper + '"') + '"'
          *-- ENDIF
          
          IF !EMPTY(mRightHead)
            LOCAL lcRight
            
            IF llCaseInsensitive
              lcRight = UPPER(mRightHead)
            *! B129154,1 MAH 07/28/2005 [BEGIN] 
            ELSE
              lcRight = mRightHead
            *! B129154,1 MAH 07/28/2005 [END] 
            ENDIF
            
            LOCAL laRight[1]
            =gfSubStr(lcRight, @laRight, lcElmSep)
            
            lcRight = ''
            LOCAL lnIndex
            FOR lnIndex = 1 TO ALEN(laRight, 1)
              lcRight = lcRight + gfStrToExp(laRight[lnIndex], lcDBEngine, llEnglishQuery) + lcElmSep
            ENDFOR
            
            lcRight = SUBSTR(lcRight, 1, LEN(lcRight) - 1)
          ENDIF
            
          lcRetVal = STRTRAN(UPPER(lcRight), lcElmSep, lcSeper)
          *! B039435,1 MAH [END]
          *! E038142,2 MAH 08/31/2004 [END]
        ELSE
          *! E038142,2 MAH 08/31/2004 Apply remove case sensitive  parameters [BEGIN]
          *-- RETURN '"'+IIF(cOperator='Contains',ALLTRIM(mrightHead),mrightHead)+'"'

          *! B127002,1 MAH 04/05/2005 Greater than condition not returning correct values [BEGIN]
          IF cOperator = 'Greater Than'
            IF llCaseInsensitive
              *! B039435,1 MAH Fix bug of there is a quote in the field value [BEGIN]
              *-- RETURN '"' + RTRIM(UPPER(mrightHead)) + ' "'
              RETURN gfStrToExp(RTRIM(UPPER(mrightHead)), lcDBEngine, llEnglishQuery)
              *! B039435,1 MAH [END]
            ELSE
              *! B039435,1 MAH Fix bug of there is a quote in the field value [BEGIN]
              *-- RETURN '"' + RTRIM(mrightHead) + ' "'
              RETURN gfStrToExp(RTRIM(mrightHead), lcDBEngine, llEnglishQuery)
              *! B039435,1 MAH [END]
            ENDIF
          ELSE
          *! B127002,1 MAH 04/05/2005[END]
            IF llCaseInsensitive
              *! E037885,2 MAH 12/03/2004 [BEGIN]
              *-- RETURN '"' + IIF(cOperator = 'Contains', UPPER(RTRIM(mrightHead)), UPPER(mrightHead)) + '"'
              *! B039435,1 MAH Fix bug of there is a quote in the field value [BEGIN]
              *-- RETURN '"' + IIF(cOperator = 'Contains', UPPER(RTRIM(mrightHead)), RTRIM(UPPER(mrightHead))) + '"'
              RETURN gfStrToExp(IIF(cOperator = 'Contains', UPPER(RTRIM(mrightHead)), RTRIM(UPPER(mrightHead))), lcDBEngine, llEnglishQuery)
              *! B039435,1 MAH [END]
              *! E037885,2 MAH 12/03/2004 [END]
            ELSE
              *! E037885,2 MAH 12/03/2004 [BEGIN]
              *-- RETURN '"' + IIF(cOperator = 'Contains', ALLTRIM(mrightHead), mrightHead) + '"'
              *! B039435,1 MAH Fix bug of there is a quote in the field value [BEGIN]
              *-- RETURN '"' + IIF(cOperator = 'Contains', ALLTRIM(mrightHead), RTRIM(mrightHead)) + '"'
              RETURN gfStrToExp(IIF(cOperator = 'Contains', ALLTRIM(mrightHead), RTRIM(mrightHead)), lcDBEngine, llEnglishQuery)
              *! B039435,1 MAH [END]
              *! E037885,2 MAH 12/03/2004 [END]
            ENDIF
            *! E038142,2 MAH 08/31/2004 [END]

          *! B127002,1 MAH 04/05/2005 Greater than condition not returning correct values [BEGIN]
          ENDIF
          *! B127002,1 MAH 04/05/2005[END]

        ENDIF
      CASE cLeftType = 'N'
        *! E038142,2 MAH 08/31/2004 Apply return english query parameters [BEGIN]
        *-- lcSeper=IIF(COPERATOR='Between' AND !llFilter,' AND ',',')
        lcSeper = IIF(COPERATOR='Between' AND !llFilter, IIF(llEnglishQuery, ' and ', ' AND '), ',')
         *! E038142,2 MAH 08/31/2004 [END]

        lcRetVal=STRTRAN(mRightHead,lcElmSep,lcSeper)
        IF EMPTY(lcRetVal)
          lcRetVal='0'
        ENDIF

      CASE cLeftType = 'D'
        lcOldCen = SET('CENT')
        SET CENTURY ON
        IF INLIST(COPERATOR,'Between','In List')
          LOCAL lnDates, lcRightHand, lnDate, lcDate
          lcRightHand = lcElmSep + ALLTRIM(mRightHead) + lcElmSep
          lnDates = OCCURS(lcElmSep,lcRightHand) - 1
          lcRetVal = ""
          FOR lnDate = 1 TO lnDates
            lcDate = STREXTRACT(lcRightHand,lcElmSep,lcElmSep,lnDate)
      
      
            *! E038650,1 MAH 01/18/2005 [BEGIN]
            *-- lcRetVal = lcRetVal + ",'" + CDateToString(lcDate) + "'"
            IF !llFilter AND cOperator = 'Between'
              IF llEnglishQuery
                IF EMPTY(lcRetVal)
                  lcRetVal = lcRetVal + " '" + CDateToString(lcDate) + "'"
                ELSE
                  lcRetVal = lcRetVal + " and '" + CDateToString(lcDate) + "'"
                ENDIF
              ELSE
                IF EMPTY(lcRetVal)
                  lcRetVal = lcRetVal + " '" + CDateToString(lcDate) + "'"
                ELSE
                  lcRetVal = lcRetVal + " AND '" + CDateToString(lcDate) + "'"
                ENDIF
              ENDIF
            ELSE
              lcRetVal = lcRetVal + ",'" + CDateToString(lcDate) + "'"
            ENDIF
            *! E038650,1 MAH 01/18/2005 [END]
          ENDFOR 
          lcRetVal = SUBSTR(lcRetVal,2)
          
*--             lcSeper=IIF(!llFilter AND cOperator='Between',' AND ALLTRIM(DTOS(',',ALLTRIM(DTOS(')            
*--              lcRetVal='ALLTRIM(DTOS({  '+STRTRAN(ALLTRIM(mRightHead),lcElmSep,'  }))'+lcSeper+'{  ')+'  }))'
        ELSE
          IF BETWEEN(YEAR(CTOD(MRIGHTHEAD)) ,1900,1950)
            lcTmpYear  = ALLTRIM(STR(YEAR(CTOD(MRIGHTHEAD)) + 100))
            MRIGHTHEAD = SUBSTR(MRIGHTHEAD,1,6)+lcTmpYear
          ENDIF
          *lcRetVal='ALLTRIM(DTOS({  '+ALLTRIM(MRIGHTHEAD)+'  }))'
          *! E038142,2 MAH 08/31/2004 Correct the code [BEGIN]
          *-- lcRetVal = CDateToString(mRightHead)
          lcRetVal = "'" + CDateToString(mRightHead) + "'"
          *! E038142,2 MAH 08/31/2004 [END]
        ENDIF   
        SET CENTURY &lcOldCen
      
      CASE cLeftType = 'L'
        *! E038142,2 MAH 08/31/2004 Return value condition according to DB Engine [BEGIN]
        *-- RETURN ' '+lcRetVal+' '
        DO CASE
          CASE ALLTRIM(lcDBEngine) == oAriaApplication.cNativeDBID
            *B609526,1 MMT 02/17/2011 Error in Automatic allocation screen OG[T20110201.0005][Start]
            *RETURN ' ' + lcRetVal + ' '
            RETURN ' ' + IIF(TYPE('lcRetVal')='L', IIF(lcRetVal = .T.,".T.",".F."),lcRetVal)+ ' '
            *B609526,1 MMT 02/17/2011 Error in Automatic allocation screen OG[T20110201.0005][End]
            
          CASE ALLTRIM(lcDBEngine) == oAriaApplication.cSQLDBID
            *B608900,1 WAM 06/21/2009 Fix expression
            *RETURN ' ' + IIF(UPPER(ALLTRIM(lcRetVal)) = '.T.' .OR. UPPER(ALLTRIM(lcRetVal)) = 'T', 1, 0) + ' '
            RETURN ' ' + IIF(UPPER(ALLTRIM(lcRetVal)) = '.T.' .OR. UPPER(ALLTRIM(lcRetVal)) = 'T', '1','0') + ' '
            *B608900,1 WAM 06/21/2009 (End)
        ENDCASE
        *! E038142,2 MAH 08/31/2004 [END]
    ENDCASE  

  CASE cRightType='F'
    *! E038142,2 MAH 08/31/2004 Apply remove case sensitive  parameters [BEGIN]
    *-- lcRetVal=STRTRAN(IIF(cOperator='Contains',ALLTRIM(mrightHead),mrightHead),lcElmSep,',')
    *! E038650,1 MAH 01/18/2005 [BEGIN]
    DO CASE
      CASE ALLTRIM(lcDBEngine) == oAriaApplication.cNativeDBID
        IF cLeftType $ 'MC'
          IF llCaseInsensitive
            lcRetVal = 'UPPER(RTRIM(' + ALLTRIM(mrightHead) + '))'
          ELSE
            lcRetVal = STRTRAN(IIF(cOperator = 'Contains', ALLTRIM(mrightHead) , mrightHead), lcElmSep, ',')
          ENDIF
        ENDIF
        
        IF cLeftType $ 'NL'
          lcRetVal = ALLTRIM(mrightHead)
        ENDIF
        
        IF cLeftType $ 'D'
          lcRetVal = 'DTOS(' + ALLTRIM(mrightHead) + ')'
        ENDIF
      
      CASE ALLTRIM(lcDBEngine) == oAriaApplication.cSQLDBID
        lcRetVal = '[' + ALLTRIM(mrightHead) + ']'
        IF cLeftType = 'D'
          lcRetVal = ' CONVERT(CHAR(8),' + lcRetVal + ', 112) '
        ENDIF
    ENDCASE
    *! E038650,1 MAH 01/18/2005 [END]
    *! E038142,2 MAH 08/31/2004 [END]
ENDCASE  

IF INLIST(cOperator,'Between','In List') AND EMPTY(ALLTRIM(mRightHead))
  *! E038142,2 MAH 08/31/2004 Apply return english query parameters [BEGIN]
  *-- lcSeper=IIF(!llFilter AND cOperator='Between',' AND ',',')
  lcSeper = IIF(!llFilter AND cOperator='Between', IIF(llEnglishQuery, ' and ', ' AND '), ',')
  *! E038142,2 MAH 08/31/2004 [END]

  lcRetVal=lcRetVal+lcSeper+lcRetVal
ENDIF
RETURN lcRetVal
*-- end of lfRightGet. DTOS

*!*************************************************************
*! Name      : lfGetOptEx
*! Developer : HS (Haytham El_Sheltawi)
*! Date      : 05/25/1999
*! Purpose   : create an optimize-able expression - if applicable -
*!             for the In Range options.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfGetOptEx()
*!*************************************************************
*B602931,4 this function was added by HS for the bug entry B602931,4.
*!*************************************************************
FUNCTION lfGetOptEx

PARAMETERS lcArray , lnRow , lcKey

lcKey = ''

PRIVATE lnArrayRow , lcFile , lcField , lcKeyExpr , lcOptExp , lnSelect ,;
        lnCount , laOptExp
 
lnArrayRow = ASCAN(laRangeInfo, UPPER(ALLTRIM(&lcArray.[lnRow,1])))

*-- If the filter option is of type expression.
IF lnArrayRow = 0
  RETURN ALLTRIM(&lcArray[lnRow,1])
ENDIF
lnArrayRow = ASUBSCRIPT(laRangeInfo, lnArrayRow , 1)


*-- Get the file of the filter option
lcFile = SUBSTR(ALLTRIM(&lcArray[lnRow,1]) ,;
                1 , AT('.' , ALLTRIM(&lcArray[lnRow,1])) - 1)

*-- If the file of the filter option is not open, we will not be able to
*-- get the indexes
IF !USED(lcFile)
  RETURN ALLTRIM(&lcArray[lnRow,1])
ENDIF

*-- Get the field of the filter option
lcField = UPPER(SUBSTR(ALLTRIM(&lcArray[lnRow,1]) ,;
                       AT('.' , ALLTRIM(&lcArray[lnRow,1])) + 1))

*-- Get the key expression that was passed to the In Range function
lcKeyExpr = IIF(AT('FOR' , laRangeInfo[lnArrayRow,2]) = 0 ,;
                laRangeInfo[lnArrayRow,2] ,;
                SUBSTR(laRangeInfo[lnArrayRow,2] , 1 ,;
                       AT('FOR' , laRangeInfo[lnArrayRow,2]) - 1))

lnSelect = SELECT(0)        && Save the alias
SELECT (lcFile)

lcOptExp = ''
lnCount  = 0

*-- DO WHILE loop to search for an index that starts with the field of the
*-- filter option.
DO WHILE lcOptExp <> lcField
  lnCount  = lnCount + 1
  lcOptExp = KEY(lnCount)
  IF EMPTY(lcOptExp)
    EXIT
  ENDIF
ENDDO    && End of DO WHILE lcOptExp <> lcField

*-- If we didn't find an index that starts with the field of the filter
*-- option we are going to search for one that starts with the same key
*-- used with the In Range function
IF EMPTY(lcOptExp) .AND. TYPE(lcKeyExpr) = 'C'
  
  IF LEN(&lcKeyExpr) = 0
    lnCount  = 0
    DO WHILE lcOptExp <> lcField
      lnCount  = lnCount + 1
      lcOptExp = KEY(lnCount)
      IF EMPTY(lcOptExp)
        EXIT
      ENDIF    && End of IF EMPTY(lcOptExp)
    ENDDO    && End of DO WHILE lcOptExp <> laBrFldFlt[lnArrayRow,4] + ...
    
    *-- If we found an index that can be used
    IF !EMPTY(lcOptExp)
      *-- Get the key that will be used with this index expression
      lcKey = &lcKeyExpr
    ENDIF    && End of IF !EMPTY(lcOptExp)
  ENDIF    && End of IF LEN(&lcKeyExpr) = LEN(&laBrFldFlt[lnArrayRow,4])
ENDIF    && End of EMPTY(lcOptExp) .AND. TYPE(laBrFldFlt[lnArrayRow,4]) ...

SELECT (lnSelect)

*-- If we did not find an index that can be used
IF EMPTY(lcOptExp)
  RETURN ALLTRIM(&lcArray[lnRow,1])
ELSE    && Else (If we found an index that can be used)
  DIMENSION laOptExp[1,1]
  =gfSubStr(lcOptExp , @laOptExp , '+')
  lcOptExp = ''
  
  *-- FOR loop to add the alias to the index expression
  FOR lnCount = 1 TO ALEN(laOptExp , 1)
    lcOptExp = lcOptExp + IIF(EMPTY(lcOptExp) , '' , '+') +;
               IIF(OCCURS('(' , laOptExp[lnCount,1]) = 0 ,;
                   lcFile + '.' + laOptExp[lnCount,1] ,;
                   STUFF(laOptExp[lnCount,1] ,;
                         RAT('(' , laOptExp[lnCount,1]) + 1 , 0 ,;
                         lcFile + '.'))
    
  ENDFOR    && End of FOR lnCount = 1 TO ALEN(laOptExp , 1)
  RETURN lcOptExp
ENDIF    && End of IF EMPTY(lcOptExp)
*-- end of lfGetOptEx.
*-- Added by Badran in 10/13/2002 ... END

*!************************************************************************************
*! Name : GetExpr.
*!************************************************************************************
*! Synopsis : Build an expression 
*!************************************************************************************
*! Passed :
*!        Parameters : 
*!           lcExp       : The Default Expression
*!           lcFile      : The File to use it's fields
*!           lcExpPrmpt  : The Title of the expression builder
*!           la_Field    : The name of the fields header array
*!           laField     : The phiscal fields array
*!           llAddAlias  : add alias to the expression or not
*!           lcType      : the type of expression that will return 
*!           laFiles     : files is used
*!           la_Var      : array that holds variable that can be use in expression
*!************************************************************************************
*! Returned : Expression String
*!************************************************************************************
*! Example :
*!        
*!************************************************************************************
FUNCTION GetExpr
PARAMETER lcExp,lcFile,lcExpPrmpt,la_Field,laField,llAddAlias,lcType,laFiles,la_Var
*SET CLASSLIB TO r:\aria4\classes\utility.vcx
PRIVATE oExpr
oExpr=CREATEOBJECT("ariaexpression",lcExp,lcFile,lcExpPrmpt,la_Field,@laField,llAddAlias,lcType,laFiles,la_Var)

IF !EMPTY(oExpr.laField) AND !('U' $ oExpr.lcType)
  FOR lnCount = 1 TO ALEN(oExpr.laField)
    lcFldName = oExpr.laField[lnCount]
    DO CASE
      CASE LEFT(oExpr.laField[lnCount],1)="M" OR LEFT(oExpr.laField[lnCount],1)="C"
        &lcFldName = " "
      CASE LEFT(oExpr.laField[lnCount],1)="N"
        &lcFldName  = 1
      CASE LEFT(oExpr.laField[lnCount],1)="D"
        &lcFldName = DATE()
    ENDCASE
  ENDFOR
ELSE
   IF TYPE('oExpr.laFiles') $ 'UL' AND EMPTY(oExpr.laField)
     oExpr.laField = " "
   ENDIF  
ENDIF
oExpr.Show 
lcRetu = oExpr.mvExp
RETURN lcRetu



*!************************************************************************************
*! Name : gpStDyBrow.
*!************************************************************************************
*! Synopsis : Browse the dyelots for a specific Style/Color at a specific warehouse. 
*!************************************************************************************
*! Passed :
*!        Parameters : 
*!           lcStyle : The active style.
*!           lcColor : The active color.
*!           lcWare  : The active warehouse.
*!           lcDyelot: The dyelot.
*!        Files      : StyDye,Style file should be opened.
*!************************************************************************************
*! Returned : 
*!        Parameters : Style, Location & Selected Dyelot
*!************************************************************************************
*! Example :
*!        DO gpStDyBrow WITH lcStyle,lcFromWare,lcDyelot
*!************************************************************************************
PROCEDURE gpStDyBrow
*N119812,1 HBG 16/11/2003 Add new parameter wit .T. or .F. to check if use style configuration [Begin]
*PARAMETER lcStyle,lcWare,lcDyelot
PARAMETER lcStyle,lcWare,lcDyelot,llUseConfg
*N119812,1 [End]
lnAlias=SELECT()
llMultiWH  = gfGetMemVar('M_WareHouse')='Y'
SELECT STYLE
lnXSvRec=RECNO()
SELECT STYDYE
IF !SEEK(PADR(lcStyle,19)+lcWare+lcDyelot)
  IF llMultiWH
	*N119812,1 HBG 16/11/2003 Change all text to be variables [Begin]  
    *lcmesgx = 'Style\Warehouse :'+ALLTRIM(lcStyle)+'\'+ALLTRIM(lcWare)
    lcmesgx = LANG_ARIA_MsgSgx1+ALLTRIM(lcStyle)+'\'+ALLTRIM(lcWare)
    *N119812,1 [End]  
  ELSE
    *N119812,1 HBG 16/11/2003 Change all text to be variables [Begin]  
    *lcmesgx = 'Style :'+ALLTRIM(lcStyle)
    lcmesgx = LANG_ARIA_MsgSgx2+ALLTRIM(lcStyle)
    *N119812,1 [End]
  ENDIF
  *N119812,1 HBG 16/11/2003 If use style configuration , Change the message [Begin]
  IF llUseConfg
    lnChoice=gfModalGen('TRM42254B42003','DIALOG',ALLTRIM(lcStyle)+'|'+ALLTRIM(lcDyelot)+'|'+lcWare)
  ELSE
  *N119812,1 [End]
    lnChoice=gfModalGen('TRM42062B42003','DIALOG',ALLTRIM(lcStyle)+'|'+ALLTRIM(lcDyelot)+'|'+lcWare)
  *N119812,1 HBG 16/11/2003 End if use style configuration , Change the message [Begin]
  ENDIF
  *N119812,1 [End]
  IF lnChoice = 1
    IF !SEEK(PADR(lcStyle,19)+lcWare+SPACE(10),'STYDYE')
      DO gpAdStyWar WITH lcStyle,SPACE(10),lcWare
    ENDIF
    DO gpAdStyWar WITH lcStyle,lcDyelot,lcWare
    RETURN
  
  ELSE   
    IF lnChoice = 2
      IF SEEK(PADR(lcStyle,19)+lcWare+SPACE(10),'STYDYE')
        LOCATE REST WHILE Style+cWareCode = PADR(lcStyle,19)+lcWare FOR !EMPTY(Dyelot)
        IF !FOUND()
          *N119812,1 HBG 16/11/2003 If use style configuration , Change the message [Begin]
		  IF llUseConfg
		    =gfModalGen('TRM42253B42001','DIALOG',lcmesgx)
		  ELSE
	      *N119812,1 [End]
		    =gfModalGen('TRM42053B42001','DIALOG',lcmesgx)
		  *N119812,1 HBG 16/11/2003 If use style configuration , Change the message [Begin]  
		  ENDIF
 		  *N119812,1 [End]
          lcDyelot = SPACE(10)
          RETURN
        ENDIF
      ENDIF
    ELSE
      lcDyelot = SPACE(10)
      RETURN
    ENDIF
  ENDIF
ENDIF

IF STYLE.Style <> STYDYE.Style
  =SEEK(STYDYE.Style,'STYLE')
ENDIF
=SEEK('S'+STYLE.Scale,'SCALE')

*N119812,1 HBG 16/11/2003 If use style configuration , Change the field name [Begin]
*lcBrfields = "Dyelot  :H='Dyelot#' ,"
lcHeader = IIF(llUseConfg,LANG_ARIA_ConfigNum,LANG_ARIA_DyelotNum)
lcBrfields = "Dyelot  :H='"+lcHeader+"' ,"

*--- Change all text to variables

*!*	FOR I=1 TO SCALE.Cnt
*!*	  Z=STR(I,1)
*!*	  lcString = "Stk"+Z+" :H='Stock&Z' ,"
*!*	  lcBrfields = lcBrfields + lcString
*!*	ENDFOR
*!*	lcBrfields = lcBrfields + "TotStk :H='TotStk',"

*!*	FOR I=1 TO SCALE.Cnt
*!*	  Z=STR(I,1)
*!*	  lcString = "Alo"+Z+" :H='Aloc.&Z',"
*!*	  lcBrfields = lcBrfields + lcString
*!*	ENDFOR
*!*	lcBrfields = lcBrfields + "TotAlo :H='TotAlo'"
lcBrFields = lcBrFields +; 
 	        "STK1  :R :H='"+LANG_ARIA_Stk1+"':6,"+;	         	           	         
            "STK2  :R :H='"+LANG_ARIA_Stk2+"':6,"+;
	        "STK3  :R :H='"+LANG_ARIA_Stk3+"':6,"+;
	        "STK4  :R :H='"+LANG_ARIA_Stk4+"':6,"+;
	        "STK5  :R :H='"+LANG_ARIA_Stk5+"':6,"+;
	        "STK6  :R :H='"+LANG_ARIA_Stk6+"':6,"+;
	        "STK7  :R :H='"+LANG_ARIA_Stk7+"':6,"+;
	        "STK8  :R :H='"+LANG_ARIA_Stk8+"',"+;
	        "TOTSTK:R :H='"+LANG_ARIA_TotStk+"':7,"+;
	        "ALO1  :R :H='"+LANG_ARIA_Alo1+"':6,"+;
	        "ALO2  :R :H='"+LANG_ARIA_Alo2+"':6,"+;
	        "ALO3  :R :H='"+LANG_ARIA_Alo3+"':6,"+;
	        "ALO4  :R :H='"+LANG_ARIA_Alo4+"':6,"+;
	        "ALO5  :R :H='"+LANG_ARIA_Alo5+"':6,"+;
	        "ALO6  :R :H='"+LANG_ARIA_Alo6+"':6,"+;
	        "ALO7  :R :H='"+LANG_ARIA_Alo7+"':6,"+;
	        "ALO8  :R :H='"+LANG_ARIA_Alo8+"':6,"+;
            "TOTALO:R :H='"+LANG_ARIA_ToTAlo+"':7"
*N119812,1 [End]

lcDyelot = SPACE(10)
lcDyeKey = PADR(lcStyle,19)+lcWare
*N119812,1 HBG 16/11/2003 If use style configuration , Change the browse title [Begin]
*IF ARIABROW([lcDyeKey FOR !EMPTY(Dyelot) REST],' Style Dyelots ', gnBrHSRow1, gnBrHSCol1, gnBrHSRow2, gnBrHSCol2)
IF ARIABROW([lcDyeKey FOR !EMPTY(Dyelot) REST],IIF(llUseConfg,LANG_ARIA_StyConfg,LANG_ARIA_StyDyelot), gnBrHSRow1, gnBrHSCol1, gnBrHSRow2, gnBrHSCol2)
*N119812,1 [End]
  lcDyelot = Dyelot
ENDIF

SELECT STYLE
GOTO lnXSvRec
SELECT(lnAlias)
RETURN


*!************************************************************************************
*! Name : sygraph.
*!************************************************************************************
*! Synopsis : display graphs for specific data 
*!************************************************************************************
*! Passed :
*!        Parameters : 
*!           lcTempCurs  : The cursor to be used in graph
*!           lnGraphType : The Graph type to display (pie,bar,...) value from 1 to 14
*!           lcFields    : The fields that the graph will be generated for
*!           llByRow     : display graph by row if true else by column
*!************************************************************************************
*! Returned : none
*!************************************************************************************
*! Example :
*!        
*!************************************************************************************
FUNCTION syGraph
PARAMETERS lcTempCurs,lnGraphType,lcFields,llByRow
DO FORM (oAriaApplication.ScreenHome+"SY\SYGRAPH") WITH lcTempCurs,lnGraphType,lcFields,llByRow
RETURN

*!*************************************************************
*! Name      : gfWait
*! Developer : Reham Al-Allamy
*! Date      : 2002
*! Purpose   : To display wait message from the dialog dectionary
*!*************************************************************
*! Calls     : 
*!          Calls: GFSUBSTR()
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : 
*!*************************************************************
*
FUNCTION gfWait
PARAMETERS lcMessagNo,lcNoWait,lcVarsStr

lcCurrDbf   = ALIAS()
lcNoWait    = IIF(TYPE('lcNoWait' )='C',lcNoWait ,'')
lcVarsStr   = IIF(TYPE('lcVarsStr')='C',lcVarsStr,'')

*-- Create cursor temp. name to hold the message info.
lcTmpCurs = gfTempName()
lnResult  = oAriaApplication.remotesystemdata.execute("Select * From syddlobj WHERE cdlobjtyp = 'M' AND cdlobjid = '"+lcMessagNo+"'", ;
            "",lcTmpCurs,"", oAriaApplication.SystemConnectionString,3 ,"",SET("DATASESSION"))
IF lnResult = 1
  SELECT (lcTmpCurs)
  LOCATE 
  IF FOUND() AND !EMPTY(cdlobjtyp+cdlobjid)
    lcMesagtxt = ALLTRIM(MDLOBJ)
    
    IF AT("ð",lcMesagtxt) > 0
      DECLARE laVarsStr [1]
      *** Collect variables to be replaced from string to array
      =gfSubStr(lcVarsStr,@laVarsStr,'|')
      
      *** Replace each ð mark with variabe sent
      FOR lnVarsStr = 1  TO ALEN(laVarsStr,1)
        lcMesagtxt = STRTRAN(lcMesagtxt,'ð',laVarsStr[lnVarsStr],1,1)
      ENDFOR  
    ENDIF
    WAIT lcMesagtxt WINDOW &lcNoWait
  ENDIF 
  USE IN (lcTmpCurs)
ENDIF

IF !EMPTY(lcCurrDbf)
  SELECT(lcCurrDbf)
ENDIF

*!*************************************************************
*! Name      : gfTmp2Mast
*! Developer : Reham Al-Allamy
*! Date      : 23-10-2002
*! Purpose   : To update master file from a temp one
*!*************************************************************
*! Passed Parameters  : Master file name
*!                      Temp file name 
*!                      Fixed message in thermo
*!                      variable message in thermo   
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : 
*!*************************************************************
* Program to update records of master file from temp file 
* Temp file has to have 2 additional fields nRecNo and cStatus to hold
* the phisical records no and the action status done to this record
*
*:->
FUNCTION gfTmp2Mast
PARAMETERS lcMastFile,lcTempFile,lcFxdMsg,lcVarMsg
PRIVATE    lcMastFile,lcTempFile,lcSavAlias,lcFxdMsg,lcVarMsg

loToolBarWindow = oAriaApplication.oToolBar.oWindParent 
*-- Include the .H file
#INCLUDE R:\ARIA4XP\PRGS\SY\gfTmp2Mast.H

lcSavAlias = SELECT(0)
lcFxdMsg   = IIF(TYPE('lcFxdMsg')<>'C',LANG_Save,lcFxdMsg ) 
lcVarMsg   = IIF(TYPE('lcVarMsg')<>'C',' ',lcVarMsg) 

lcSaveDel = SET ('DELETE')
SET DELETE OFF

*-- Initialize the progress bar needed variables.
oProgress = NEWOBJECT('ariaprogressbar',oAriaApplication.classdir+'utility.vcx')
oProgress.TotalProgress = RECCOUNT(lcTempFile)
oProgress.lblFirstLabel.Caption = lcFxdMsg
oProgress.Show()
lnCurRecord  = 1

SELECT (lcTempFile)
GO TOP
*-- Scan through all the Added,Modified or Deleted records
SCAN FOR cStatus <> 'S' 
  *-- Call the progress bar.
  oProgress.lblSecondLabel.Caption = lcVarMsg
  oProgress.CurrentProgress(lnCurRecord)
  lnCurRecord = lnCurRecord + 1

  DO CASE 
    *-- New added record   
    CASE cStatus = 'A'                    
      SCATTER MEMVAR MEMO
      SELECT  (lcMastFile)  
      IF SEEK(' ')                        && Chek if there is empty 
        RECALL                            && Deleted records to recall
        GATHER MEMVAR MEMO
      ELSE  
        INSERT INTO &lcMastFile FROM MEMVAR 
      ENDIF  

    *-- Record was modified
    CASE cStatus = 'M'                    
      SCATTER MEMVAR MEMO                 && Collect data from temp
      SELECT  (lcMastFile)  
      GO &lcTempFile..nRecNo
      GATHER  MEMVAR MEMO                 && Replace master data 

    *-- Record was deleted
    CASE cStatus = 'D' .AND.  DELETED() 
      SELECT  (lcMastFile)  
      GO &lcTempFile..nRecNo
      SCATTER MEMVAR MEMO BLANK           && Empty the record befor
      GATHER  MEMVAR MEMO                 && delete it
      DELETE                              && Delete recored not in temp
  ENDCASE

  SELECT  (lcTempFile)
  REPLACE cStatus WITH "S"
ENDSCAN  
*-- Terminate the progress bar
oProgress=NULL
oAriaApplication.oToolBar.oWindParent = loToolBarWindow

SET DELETE &lcSaveDel
SELECT (lcSavAlias)


*!*************************************************************
*! Name      : gfGetBrF
*! Developer : Amin Khodary 
*! Date      : 30-10-2002
*! Purpose   : To get the browse field header string and file title.
*!*************************************************************
*! Passed Parameters  : lcFileTitl  -- Reference to the file title 
*!                      lcBrowFlds  -- Reference to browse fields string
*!                      lcSrchFile  -- Which file you want to get its title amd field
*!                      lcFields    -- For which fields you want to get get its header. 
*!*************************************************************
*! Called from Factor field validation, called from AP programs
*!*************************************************************
*! Returns            : lcFileTitl  -- Reference to the file title 
*!                    : lcBrowFlds  -- Reference to browse fields string
*!*************************************************************
*! Example   : gfGetBrF(@lcFileTitl, @lcBrowFlds, 'APVENDOR',"cVendCode, cVendComp")
*!*************************************************************
*! Note      :  This fucntion called lfGetBrF in Aria27
*!*************************************************************

FUNCTION gfGetBrF
PARAMETERS lcFileTitl, lcBrowFlds, lcSrchFile, lcFields

LOCAL llMayProceed , lcCurAlias , lnFieldNum
llMayProceed = .F. 

PRIVATE  laFields
STORE " " TO lcFileTitl, lcBrowFlds 
  
*-- Use the file that passed as a parameter, otherwise use the current work area
lcCurAlias = ALIAS()
lcSrchFile   = IIF(TYPE('lcSrchFile') <> 'C' .OR.;
                   EMPTY(lcSrchFile), lcCurAlias, lcSrchFile)
IF !EMPTY(lcSrchFile)
  llMayProceed = .T. 
	=oAriaApplication.remotesystemdata.execute("Select * from SYDFILES WHERE cFile_Nam = '"+UPPER(lcSrchFile)+"'",'',"SYDFILES","",oAriaApplication.SystemConnectionString,3,"",SET("Datasession"))
	=oAriaApplication.remotesystemdata.execute("Select * from SYDFIELD",'',"SYDFIELD","",oAriaApplication.SystemConnectionString,3,"",SET("Datasession"))
	DECLARE laFields[1,1]
	laFields   = " "
	lcFileTitl = ALLTRIM(LOOKUP(SYDFILES.cFile_Ttl, UPPER(ALLTRIM(lcSrchFile)),;
	                             SYDFILES.cFile_Nam, 'cFile_Nam'))
	lcFields   = IIF(TYPE('lcFields') <> 'C' .OR. EMPTY(lcFields),;
	                     STRTRAN(SYDFILES.mBrow_Fld, '|', ','), lcFields)
	    
	=gfSubStr(lcFields, @laFields, ",")   
	FOR lnFieldNum = 1 TO ALEN(laFields,1)
		lcBrowFlds = lcBrowFlds + laFields[lnFieldNum]+;
								[:H=']+ALLTRIM(LOOKUP(SYDFIELD.cFld_Head,;
		               UPPER(ALLTRIM(laFields[lnFieldNum])),SYDFIELD.cFld_Name,'cFld_Name'))+[',]
	ENDFOR
	lcBrowFlds  = SUBSTR(lcBrowFlds, 1, LEN(lcBrowFlds)-1)
  SELECT IIF(!EMPTY(lcCurAlias),(lcCurAlias),0)  
ENDIF

RETURN llMayProceed

**************************************************************************************

*!*************************************************************
*! Name      : gpFbDyBrow
*! Developer : Samah Wilson Kirollos (SWK)
*! Date      : 09/01/96.
*! Purpose   : Browse the Dyelots for a certain Fabric/Clr 
*!             in a warehouse.
*!*************************************************************
*! Calls     : Functions  : ARIABROW
*!*************************************************************
*! Passed :
*!        Parameters : 
*!            lcWare   : The Warehouse.
*!            lcFab    : The Fabric.   
*!            lcClr    : The color.
*!            lcDyelot : The Dyelot
*!*************************************************************
*! Returns             : Dyelot
*!***********************************************************************
FUNCTION gpFbDyBrow
PARAMETER lcFab,lcClr,lcDye,lcWare

PRIVATE laData,lcBrFields
lnAlias = SELECT()

*-- Array to get values from browse
DECLARE laData[3]
laData = SPACE(1)

*--Variable to check if there is any dyelot
llFound = .F.

*--Check if the system setup is Multi or Single WareHouse
llMultiWH = ALLTRIM(gfGetMemVar('M_WareHouse'))= 'Y'

*-- Title for the browse
lcTitle = IIF (llMultiWH, 'ITEM\WAREHOUSE DYELOTS', 'ITEM\DYELOTS')

*-- Variable to check if we select from the browse or not
llWasSel = .T.

lcKey = "lcFab+lcClr+lcWare"

IF USED ('FabDye')
  SELECT FabDye 
  lcOldTFad = TAG() 
ENDIF
llOpenFad =gfOpenFile(oAriaApplication.DataDir+'FabDye','FABDYE','SH')
SET ORDER TO TAG FABDYE IN FabDye

IF SEEK (lcFab+lcClr+lcWare)
  SCAN WHILE Fabric+Color+cWareCode = lcFab+lcClr+lcWare FOR !EMPTY(Dyelot)
    llFound = .T.
    EXIT
  ENDSCAN
ENDIF
IF llFound 
  lcBrFields = [FabDye.Fabric:14:h="Item",FabDye.Color:9:H="Color",FabDye.Dyelot:h="Dyelot":15,FabDye.OnHand:h="OnHand":12]

  llWasSel = ARIABROW(lcKey+[FOR !EMPTY(DYELOT)],;
             lcTitle,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,"","","Fabric,Color,Dyelot","laData")
  lcFab    = IIF(llWasSel, laData[1], SPACE(7)) 
  lcClr    = IIF(llWasSel, laData[2], SPACE(6))
  lcDye    = IIF(llWasSel, laData[3], SPACE(10))
ELSE
  =gfModalGen('TRM36045B36000','ALERT',ALLTRIM(lcFab)+'\'+ALLTRIM(lcClr) +;
              IIF(llMultiWH, ' in warehouse: '+ ALLTRIM(lcWare)+'.'  ,;
              ' on the file.' ))
  lcDye = SPACE(10)            
ENDIF

IF llOpenFad
  SET ORDER TO TAG (lcOldTFad) IN FabDye
  USE IN FabDye
ENDIF
SELECT(lnAlias)
RETURN lcDye

*!*************************************************************
*! Name      : gfFlock
*! Developer : Yasser Saad Ibrahime
*! Convert By: Reham on 10/31/2002
*! Date      : 1993-1995 
*! Purpose   : To lock entir file or files
*!*************************************************************
*! Calls     : 
*!          Calls: GFSUBSTR()               (function  in ARIA3.PRG)
*!          Calls: GFMODALGEN()             (function  in ARIA3.PRG)
*!*************************************************************
*! Passed Parameters  : Name of files
*!                      lock or unlock
*!                      Numvber of attemps 
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : 
*!*************************************************************
* This function will atempt to lock multiple files and let the user to retry
* agian till he select to cancel the operation
* parameters : Files list to lock
*              Flag to lock or unlock the files
*
*:->
FUNCTION gfFlock
PARAMETERS lcFls2lock,llLokUnlok,lnTry

lcFls2lock = IIF(TYPE('lcFls2lock')<>'C',ALIAS(),lcFls2lock)
llLokUnlok = IIF(TYPE('llLokUnlok')='U',.F.,llLokUnlok)
lnTry      = IIF(TYPE('lnTry')     ='N',lnTry,-2) 
llRetFlag  = .F.
llRetry    = .F.

*** If no files was sent return with .F.
IF EMPTY(lcFls2lock)
  RETURN llRetFlag
ENDIF

*** Put files to be locked in array
DECLARE laFls2lock[1]

IF ',' $ lcFls2lock
  =gfSubStr(lcFls2lock,@laFls2lock,',')
ELSE
  laFls2lock[1] = lcFls2lock
ENDIF   

SET REPROCESS TO lnTry

*** Lock one or multiple files
IF llLokUnlok
  *** Keep tring till the user decide to cancle
  DO WHILE .T.
    *** Loop to lock all files to be locked
    FOR lnFCount =  1 TO ALEN(laFls2lock,1)
      *** If files was locked all return with .T.
      IF FLOCK(laFls2lock[lnFCount])
        llRetFlag = .T.
      ELSE
        *** Give the user message to retry or cancel
        *** Files is in use by another user
        IF gfModalgen("QRM00108B00015","ALERT",UPPER(ALLTRIM(laFls2lock[lnFCount]))) = 1
          llRetFlag = .F.
          llRetry   = .T.
          *** Exit from the for loop and retry again
          EXIT
        ELSE
          llRetFlag = .F.
          llRetry   = .F.
          *** Exit from the for loop and quit function
          EXIT
        ENDIF
      ENDIF
    ENDFOR
    *** If at least one files was not locked
    IF !llRetFlag 
      *** If the user select to retry loop again
      IF llRetry 
        LOOP
      *** If not quit the function with .F.
      ELSE
        *** If cancel unlock in all alias 
        FOR lnFCount =  1 TO ALEN(laFls2lock,1)
          UNLOCK IN (laFls2lock[lnFCount])
        ENDFOR
        llRetFlag = .F.    
        EXIT
      ENDIF
    ELSE
      *** If all lockes went ok terminat the loop
      EXIT
    ENDIF  
  ENDDO

*** Unlock multiple files 
ELSE
  FOR lnFCount =  1 TO ALEN(laFls2lock,1)
    UNLOCK IN (laFls2lock[lnFCount])
  ENDFOR
  llRetFlag = .T.
ENDIF

SET REPROCESS TO -2

RETURN llRetFlag

*!*************************************************************
*! Name      : gfRlock
*! Developer : Yasser Saad Ibrahime
*! Date      : 1993-1995 
*! Purpose   : Phisycal lock to record in one or more files
*!*************************************************************
*! Calls     : 
*!          Calls: GFSUBSTR()               (function  in ARIA3.PRG)
*!          Calls: GFMODALGEN()             (function  in ARIA3.PRG)
*!*************************************************************
*! Passed Parameters  : Name of files
*!                      Lock or unlock 
*!                      Number of attempts    
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : 
*!*************************************************************
* This function will atempt to lock multiple files and let the user to retry
* agian till he select to cancel the operation
* parameters : Files list to lock
*              Flag to lock or unlock the files
*
*:->
FUNCTION gfRlock
PARAMETERS lcFls2lock,llLokUnlok,lnTry

lcFls2lock = IIF(TYPE('lcFls2lock')<>'C',ALIAS(),lcFls2lock)
llLokUnlok = IIF(TYPE('llLokUnlok')='U',.F.,llLokUnlok)
lnTry      = IIF(TYPE('lnTry')     ='N',lnTry,gnLockTry) 
llRetFlag  = .F.
llRetry    = .F.

*** If no files was sent return with .F.
IF EMPTY(lcFls2lock)
  RETURN llRetFlag
ENDIF

*** Put files to be record locked in array
DECLARE laFls2lock[1]
IF ',' $ lcFls2lock 
  =gfSubStr(lcFls2lock,@laFls2lock,',')
ELSE
  laFls2lock[1] = lcFls2lock
ENDIF  


SET REPROCESS TO lnTry

*** Lock one or multiple files
IF llLokUnlok
  *** Keep tring till the user decide to cancle
  DO WHILE .T.
    *** Loop to lock all files to be locked
    FOR lnFCount =  1 TO ALEN(laFls2lock,1)
      *** If files was locked all return with .T.
      IF RLOCK(laFls2lock[lnFCount])
        llRetFlag = .T.
      ELSE
        *** Give the user message to retry or cancel
        *** Files is in use by another user
        IF gfModalgen("QRM00109B00015","ALERT",UPPER(ALLTRIM(laFls2lock[lnFCount]))) = 1
          llRetFlag = .F.
          llRetry   = .T.
          *** Exit from the for loop and retry again
          EXIT
        ELSE
          llRetFlag = .F.
          llRetry   = .F.
          *** Exit from the for loop and quit function
          EXIT
        ENDIF
      ENDIF
    ENDFOR
    *** If at least one files was not locked
    IF !llRetFlag 
      *** If the user select to retry loop again
      IF llRetry 
        LOOP
      *** If not quit the function with .F.
      ELSE
        *** If cancel unlock in all alias 
        FOR lnFCount =  1 TO ALEN(laFls2lock,1)
          UNLOCK IN (laFls2lock[lnFCount])
        ENDFOR
        llRetFlag = .F.    
        EXIT
      ENDIF
    ELSE
      *** If all lockes went ok terminat the loop
      EXIT
    ENDIF  
  ENDDO

*** Unlock multiple files 
ELSE
  FOR lnFCount =  1 TO ALEN(laFls2lock,1)
    UNLOCK IN (laFls2lock[lnFCount])
  ENDFOR
  llRetFlag = .T.
ENDIF

SET REPROCESS TO gnLockTry

RETURN llRetFlag


*!*************************************************************
*! Name      : gfObj_Lock
*! Developer : Yasser Saad Ibrahime 
*! Date      : 1993-1995 
*! Modified  : Hesham El_Sheltawi
*! Date      : 11/12/2002
*! Purpose   : To object lock any record in any file
*!*************************************************************
*! Calls     : 
*!          Calls: GFMODALGEN()             (function  in ARIA3.PRG)
*!          Calls: GFGETTIME()              (function  in ARIA3.PRG)
*!*************************************************************
*! Passed Parameters  : flage to lock or unlock
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : 
*!*************************************************************
*:->
FUNCTION gfObj_Lock
PARAMETERS lLok_Set
PRIVATE lnRecNo,lRet_Flag,lnOldrpSt
LOCAL lcSelAlias,lnDataSession,llBufferMode
lnDataSession = SET("DATASESSION")
SET DATASESSION TO (lnDataSession)
lcSelAlias = ALIAS()
IF EMPTY(lcSelAlias)
  RETURN
ENDIF

SELECT (lcSelAlias)
llBufferMode = CURSORGETPROP("Buffering") >= 4

lRet_Flag = .F.
lLok_It   = .F.
llLocked  = .F.
*** Go to the same record to get a fresh copy in the buffer
lnRecNo = RECNO()

DO WHILE .T.
  SELECT (lcSelAlias)
  IF lnRecNo <= RECCOUNT()
    GO lnRecNo
   llLocked = RLOCK() 
   IF llLocked AND llBufferMode
     TABLEREVERT(.F.)
   ENDIF 
   IF DELETED()
     UNLOCK
     =gfModalGen('INM00095B00000','ALERT')
     SELECT (lcSelAlias)

     RETURN .F.
   ENDIF
  ENDIF  

  *** Chek if the record is in use by another user
  IF lLok_Set 
    *** Chek if the field cLok_User in the structur
    IF !lLok_Stat .AND. llLocked
      *** Record is not locked you may lock it
      lLok_It   = .T.
    ELSE
      lcLok_User = cLok_User
      IF !EMPTY(lcLok_User)
        IF ALLTRIM(lcLok_User) = ALLTRIM(oAriaApplication.User_ID)
          * Messaging the user that he cannot edit the same record
          * from more than one session and permit him from editing
          * the same record
          IF gfModalGen("INM00240B00006","ALERT")=2
            lLok_It    = .F.
            lRet_Flag  = .F.
          ELSE      
            lLok_It    = .T.
          ENDIF
        ELSE

          *We save old value of reprocess first.[START]
          lnOldrpSt = SET('REPROCESS')
          SET REPROCESS TO 1
          
					SET DATASESSION TO 1
					llLoop = .F.
					SELECT syuStatc
          IF SEEK ('INI'+'OLDVARS'+lcLok_User,'syuStatc') 
            SCAN REST WHILE cobj_typ+ALLTRIM(cobj_name)+cuser_id = 'INI'+'OLDVARS'+lcLok_User
	            IF RLOCK('syuStatc')
  	            UNLOCK IN  syuStatc 
    	          GO (oAriaApplication.UserStaticRecord) IN syuStatc 
      	        =RLOCK('syuStatc')
        	      lLok_It    = .T.
          	  ELSE
            	  UNLOCK
              	 GO (oAriaApplication.UserStaticRecord) IN syuStatc 
              	=RLOCK('syuStatc') 
              	*** Display the message "Record is in use by user AAAA"
              	lcLok_User = oAriaApplication.getUserName(lcLok_User)
              	*** Record is in use by user ????    
              	SET DATASESSION TO (lnDataSession)
              	IF  gfModalGen("INM00028B00015","ALERT",lcLok_User) = 1
                	llLoop = .T.
              	ENDIF  
              	lLok_It    = .F.
              	lRet_Flag  = .F.
              	EXIT 
            	ENDIF
           ENDSCAN 	
          ELSE
            lLok_It    = .T. 
          ENDIF
          * Return the old value of reprocess.
          SET REPROCESS TO  lnOldrpSt
					SET DATASESSION TO (lnDataSession)
          IF llLoop
            LOOP 
          ENDIF 

        ENDIF
      ELSE
        *** Display the message "Record is in use by another"
        SET DATASESSION TO (lnDataSession)
        IF gfModalGen("INM00029B00015","ALERT") = 1
          LOOP
        ENDIF  
        lLok_It    = .F.
        lRet_Flag  = .F.
      ENDIF   
    ENDIF

  ELSE
    *** Chek if these three field in the file structur
    IF TYPE ('cLok_User') <> "U" .AND. ;
       TYPE ('dLok_Date') <> "U" .AND. ;
       TYPE ('cLok_Time') <> "U" 

      *** Unlock the record
      REPLACE lLok_Stat WITH .F. , ;   
              cLok_User WITH ""  , ;
              dLok_Date WITH {}  , ;
              cLok_Time WITH ""
            IF llBufferMode  
			  =TABLEUPDATE(0,.T.)              
			ENDIF   
      lRet_Flag  = .T.
    ENDIF  
  ENDIF

  EXIT
ENDDO

*** Chek if you have to lock the record or not
SET DATASESSION TO (lnDataSession)
IF lLok_It  
  *** Chek if these three field in the file structur
  IF TYPE ('cLok_User') <> "U" .AND. ;
     TYPE ('dLok_Date') <> "U" .AND. ;
     TYPE ('cLok_Time') <> "U" 
    *** Lock the record for this user with date and time
    REPLACE lLok_Stat WITH .T.       , ;   
             cLok_User WITH oAriaApplication.User_ID , ;
             dLok_Date WITH DATE()    , ;
             cLok_Time WITH gfGetTime()
    IF llBufferMode
      =TABLEUPDATE(0,.T.)
    ENDIF   
    lRet_Flag  = .T.    
  ENDIF
ENDIF
SELECT (lcSelAlias)
UNLOCK


RETURN lRet_Flag

*!*****************************************************************************************
*! Name      : CDateToString
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 11/26/2002 01:45:04 AM
*! Purpose   : Changes the char date to string date.
*! Entry no. : 
*!*****************************************************************************************
*! Parameters: 
*!****************************************************************************************
*! Returns   : 
*!****************************************************************************************
*! Runs as following:
*!
*!****************************************************************************************
*!
FUNCTION CDateToString
  LPARAMETERS cCharDate
  IF (VARTYPE(cCharDate) != "C") OR EMPTY(cCharDate)
    RETURN ""
  ENDIF 
  LOCAL ldDate, llError, lcOldError
  lcOldError = ON("ERROR")
  ON ERROR llError = .T.
  ldDate = CTOD(ALLTRIM(cCharDate))
  ON ERROR &lcOldError.
  IF llError OR (VARTYPE(ldDate) != "D")
    RETURN ""
  ENDIF
  RETURN DTOS(ldDate)
ENDFUNC 

*!*************************************************************
*! Name      : gfFillPop
*! Developer : Yasser Saad Ibrahime
*! Date      : 1993-1995 
*! Purpose   : To fill any popup from an array
*!*************************************************************
*! Calls     : 
*!      Called by: GFACTPOP()               (function  in ARIA3.PRG)
*!*************************************************************
*! Passed Parameters  : Popup name
*!                      Array name
*!                      Colums number to be used in filling  
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : 
*!*************************************************************
*:->
FUNCTION gfFillPop
PARAMETERS lcPopNam,laPopAray,lnColNum
lnColNum =IIF(TYPE('lnColNum')<>'N',1,lnColNum)

RELEASE BAR ALL OF &lcPopNam


FOR lnCount = 1 to ALEN(&laPopAray,1)
  DEFINE BAR lnCount OF &lcPopNam ;
         PROMPT (ALLTRIM(&laPopAray[lnCount,lnColNum]))
ENDFOR


*!*************************************************************
*! Name      : gfPopArang
*! Developer : Yasser Saad Ibrahime
*! Date      : 1993-1995 
*! Purpose   : To rearrange array from any popup
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : Popup name
*!                      array name
*!                      array colum to use in sort
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : 
*!*************************************************************
*  Collect new arrangement of popup
*
*:->
FUNCTION gfPopArang
PARAMETERS lcPopNam,laArray,lnPopUpRow

lnPopUpRow = IIF(TYPE('lnPopUpRow')<>'N',1,lnPopUpRow)

IF ALEN(laArray,2) <= 1 
  DECLARE laArray[CNTBAR(lcPopNam),1]

  FOR lnCount = 1 TO CNTBAR(lcPopNam)
    laArray[lnCount]=PRMBAR(lcPopNam,GETBAR(lcPopNam,lnCount))
  ENDFOR
ELSE  
  DECLARE laTmpAry [ALEN(laArray,1),ALEN(laArray,2)]
  =ACOPY(laArray,laTmpAry)

  FOR lnCount = 1 TO CNTBAR(lcPopNam)
    
    lnSourcRow = ASCAN(laTmpAry,PRMBAR(lcPopNam,GETBAR(lcPopNam,lnCount))) 

    IF lnSourcRow > 0
      lnSourcRow = ASUBSCRIPT(laTmpAry,lnSourcRow,1)
 
      FOR  lnColum = 1 TO ALEN(laTmpAry,2)
        laArray[lnCount,lnColum] = laTmpAry[lnSourcRow,lnColum] 
      ENDFOR
 
    ENDIF

  ENDFOR

ENDIF  

*!*****************************************************************************************
*! Name      : DateRng
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 11/26/2002 01:45:04 AM
*! Purpose   : Date Range Functionality
*! Entry no. : 
*!*****************************************************************************************
*!
FUNCTION DateRng
  DO FORM DateRng
  RETURN .T.
ENDFUNC 
*-- end of DateRng.

*--E000000,1 Hesham (Start)
*!*****************************************************************************************
*! Name      : gfLockSys
*! Developer : Hesham - Hesham El Sheltawi
*! Date      : 04/15/2003 
*! Purpose   : Lock the system to prevent any user to log into the system
*! Entry no. : E000000,1
*!*****************************************************************************************
*!
FUNCTION gfLockSys
  LOCAL llLockSys,lcCurAlias,llSuccess
  llLockSys = !glLockSys
  lcCurAlias = ALIAS()
  llSuccess = .F.
  lnBar = BAR()
  lcPop = POPUP()
  SELECT sycinst 
  IF llLockSys
    IF RLOCK("SYCINST")
      REPLACE lLockSys WITH .T.
      FLUSH()
      llSuccess = .T.
    ELSE
      MESSAGEBOX(LANG_LogMSG,16,this.systemname)  
    ENDIF
  ELSE
    IF RLOCK("SYCINST")
      REPLACE sycinst.lLockSys WITH .F.
      UNLOCK IN SYCINST
      llSuccess = .T.
    ENDIF
  ENDIF   
  SELECT (lcCurAlias)
  IF llSuccess
    glLockSys = sycinst.lLockSys
    SET MARK OF BAR lnBar OF (lcPop) glLockSys
  ENDIF   
*--E000000,1 Hesham (End)




*!********************************************************************
*! Name      : gfAmntDisp
*! Developer : Mohamed Hassan
*! Date      : 12/26/95
*! Purpose   : Return the amount according to the display condition.
*!********************************************************************
*! Parameters: lnAmount     && The amount that you want to display.
*!           : lcRpDispCur  && The way to display the amount.
*!           : ldExRateDt   && If you are going to display the amount
*!           :                 with an exchange rate of a specific date.
*!           : lcTmepFile   && The temp file name that hold the temp. 
*!           :                 exchange rates.
*!           : llAprvCurr   && If you are using the Approved currency.
*!           : lcGetFile    detect which alias we get currency values from it.
*!           : lcGetField   detect which Field we get currency values from it.
*!********************************************************************
*! Call      : From all the AP reports that is using the currency display
*!           : feature.
*!********************************************************************
*! Returns   : lnAmount
*!********************************************************************
*! Example   : gfAmntDisp(APINVHDR.NINVAMNT,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.).
*!********************************************************************
*E301214,1   : Transfer this function from AP Module and AR Module to be a global function.
**********
FUNCTION gfAmntDisp
PARAMETER lnAmount,lcRpDispCur,ldExRateDt,lcTmepFile,llAprvCurr,lcGetFile,lcGetField
PRIVATE lnAmount,lcRpDispCur,ldExRateDt,lcTmepFil,llAprvCurr,lcExSin1,lcExSin2,lnSavAlias,lcGetField

lnAmount    = IIF(TYPE('lnAmount') = 'N',lnAmount,0)
lcRpDispCur = IIF(TYPE('lcRpDispCur') ='C',lcRpDispCur,'')
ldExRateDt  = IIF(TYPE('ldExRateDt') = 'D',ldExRateDt,{})
lcTmepFile  = IIF(TYPE('lcTmepFile') = 'C',lcTmepFile,'')
llAprvCurr  = IIF(TYPE('llAprvCurr') = 'L',llAprvCurr,.F.)

lcGetFile   = IIF(TYPE('lcGetFile')$"UL",'',lcGetFile)
lcGetField  = IIF(TYPE('lcGetField') ='C',lcGetField,'')

lcExSin1    = ''       && Variable to hold the first sign in the equation.
lcExSin2    = ''       && Variable to hold the second sign in the equation.
lnSavAlias  = SELECT(0)  && Variable to save the alias.
DO CASE
  CASE lcRpDispCur = 'F'

  CASE lcRpDispCur = 'O'
    IF EMPTY(lcGetFile)
      lcCurrCode = IIF(llAprvCurr,CAPRCURCOD,CCURRCODE)
    ELSE
      *Get filed currency if send . 
       lcCurrCode = IIF(llAprvCurr,&lcGetFile..CAPRCURCOD,;
                   IIF(EMPTY(lcGetField),&lcGetFile..CCURRCODE,EVAL(lcGetFile+'.'+lcGetField)))
    ENDIF  
    lcExSin2   = ' '
    lcExSin1   = gfGetExSin(@lcExSin2,lcCurrCode)
    lnExRate = 0
    IF EMPTY(lcGetFile)
      lnUnit = NCURRUNIT
      lnExRate = IIF(llAprvCurr , gfChkRate('lnUnit' , lcCurrCode , DINVDATE , .F.) , NEXRATE)
    ELSE
      lnUnit = &lcGetFile..NCURRUNIT
      lnExRate = IIF(llAprvCurr , gfChkRate('lnUnit' , lcCurrCode , &lcGetFile..DINVDATE , .F.) , &lcGetFile..NEXRATE)
    ENDIF  
    lnExRate = IIF(lnExRate <> 0 , lnExRate , 1)
    lnUnit = IIF(lnUnit <> 0 , lnUnit , 1)
    lnAmount   = ROUND(lnAmount &lcExSin1 lnExRate &lcExSin2 lnUnit , 2)

  CASE lcRpDispCur = 'D'
    lnExRate   = 0
    lnUnit     = 0
    IF EMPTY(lcGetFile)
      lcCurrCode = IIF(llAprvCurr,CAPRCURCOD,CCURRCODE)
    ELSE
      lcCurrCode = IIF(llAprvCurr,&lcGetFile..CAPRCURCOD,;
                   IIF(EMPTY(lcGetField),&lcGetFile..CCURRCODE,EVAL(lcGetFile+'.'+lcGetField)))
    ENDIF
    IF lcCurrCode = oAriaApplication.BaseCurrency
      lnExRate = 1
      lnUnit   = 1
    ELSE
      lnExRate   = gfChkRate('lnUnit',lcCurrCode,ldExRateDt,.F.)
    ENDIF
    lnExRate = IIF(lnExRate <> 0 , lnExRate , 1)
    lnUnit = IIF(lnUnit <> 0 , lnUnit , 1)
    lcExSin2   = ' '
    lcExSin1   = gfGetExSin(@lcExSin2,lcCurrCode)
    lnAmount   = ROUND(lnAmount &lcExSin1 lnExRate &lcExSin2 lnUnit,2)

  CASE lcRpDispCur = 'N'
    lnExRate   = 0
    lnUnit     = 0
    IF EMPTY(lcGetFile)
      lcCurrCode = IIF(llAprvCurr,CAPRCURCOD,CCURRCODE)
    ELSE
      lcCurrCode = IIF(llAprvCurr,&lcGetFile..CAPRCURCOD,;
                   IIF(EMPTY(lcGetField),&lcGetFile..CCURRCODE,EVAL(lcGetFile+'.'+lcGetField)))
    ENDIF  
    IF lcCurrCode = oAriaApplication.BaseCurrency
      lnExRate = 1
      lnUnit   = 1
    ELSE
      IF SEEK(lcCurrCode,lcTmepFile)
        lnExRate = &lcTmepFile..NEXRATE
        lnUnit   = &lcTmepFile..NCURRUNIT
      ENDIF
    ENDIF
    lnExRate = IIF(lnExRate <> 0 , lnExRate , 1)
    lnUnit = IIF(lnUnit <> 0 , lnUnit , 1)
    lcExSin2   = ' '
    lcExSin1   = gfGetExSin(@lcExSin2,lcCurrCode)
    lnAmount   = ROUND(lnAmount &lcExSin1 lnExRate &lcExSin2 lnUnit,2)
ENDCASE
SELECT (lnSavAlias)
RETURN lnAmount

*-- end of gfAmntDisp.

*!***************************************************************************
*! Name      : gfGetSeq
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/26/2003
*! Purpose   : Get the next sequence number.
*!***************************************************************************
*! Example   : =gfGetSeq()
*!***************************************************************************
*! Notes     : This function was written by MAN.
*!***************************************************************************
*B606902,1
FUNCTION gfGetSeq
PARAMETER lnSeq,cChr
PRIVATE lcSPrefix , lnCharASCI , lnI , lcAlias
lcAlias = ALIAS()
SELECT Sequence
lcSPrefix = ""
IF !(cChr = CHR(0))
  lnCharASCI = ASC(cChr)
  IF MOD(lnCharASCI,26) = 0
    lcChar = "Z"
    lnCharPos = lnCharASCI/26
  ELSE
    lnCharPos = INT(lnCharASCI/26)+1
    lcChar =  chr(mod(lnCharASCI,26)+64)
  ENDIF  
  FOR lnI = 1 TO lnCharPos - 1
    lcSPrefix = lcSPrefix + "Z"
  ENDFOR
  lcSPrefix = lcSPrefix + lcChar

ELSE
 lcChar =""
ENDIF
IF lnSeq = VAL(REPLICATE("9",lnRetLen-LEN(lcSPrefix)))
  IF EMPTY(cChr)
    lcSPrefix = "A"
    *REPLACE cSeq_Chr WITH CHR(1)
    lcChrToUpd = CHR(1)
  ELSE
    IF lcChar = "Z"
      lcSPrefix = lcSPrefix + "A"
    ELSE
      lcSPrefix = LEFT(lcSPrefix,LEN(lcSPrefix)-1) + CHR(ASC(lcChar)+1)
    ENDIF       
    *REPLACE cSeq_Chr WITH CHR(ASC(cSeq_Chr)+1)
    lcChrToUpd = CHR(ASC(cSeq_Chr)+1)
  ENDIF  
  lnSeq = 0
ELSE
  lnSeq = lnSeq + 1
ENDIF

*lnSeq = lnSeq + 1
*REPLACE nSeq_No with lnSeq
*? lcSPrefix+PADL(lnSeq,lnRetLen-LEN(lcSPrefix),"0")

lnRetVal = lnSeq
lcExtraStr = lcSPrefix

IF !EMPTY(lcAlias)
  SELECT (lcAlias)
ENDIF
*-- End of gfGetSeq.

*!*************************************************************
*! Name      : IsValidMail
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 07/10/2003
*! Purpose   : Validate an email string.
*!*************************************************************
*! Passed Parameters  : strMail   (E-Mail String)
*!*************************************************************
*! Return             : True if valid email.
*!*************************************************************
*!
FUNCTION IsValidMail
  LPARAMETERS strMail
  IF VARTYPE(strMail) != "C"
    RETURN .F.
  ENDIF

  strMail = ALLTRIM(strMail)
  *-- Empty or has a space.
  If (Len(strMail) = 0) Or (ATC(" ",strMail) > 0) Then
    RETURN .F.
  EndIf

  LOCAL intAt As Integer
  intAt = ATC("@", strMail)

  *-- If Not found or it's the first character or it's the last character.
  If (intAt <= 1) Or (intAt = Len(strMail)) Then
    RETURN .F.
  EndIf

  *! B127465,1 SMM 05/08/2005 Invalid Email [START]
  *-- If the dot is before the @
*!*	  If ATC(".", strMail) < intAt Then
*!*	    RETURN .F.
*!*	  EndIf
  *! B127465,1 SMM 05/08/2005 Invalid Email [End]
  strMail = SUBSTR(strMail, intAt + 1)
    
  *-- If still have another @
  If ATC("@", strMail) > 0 Then
    RETURN .F.
  EndIf
   
  intAt = ATC(".", strMail)
  *-- If It's the first character after @ or not exists or it's the last character.
  If (intAt <= 1) Or (intAt = Len(strMail)) Then
    RETURN .F.
  EndIf

  *-- Validate the last characters.
  strMail = SUBSTR(strMail,RAT(".",strMail)+1)
  IF !BETWEEN(LEN(strMail),2,3)  && (.Com, .Org, or .eg)
    RETURN .F.
  ENDIF
  RETURN .T.  && Pass the validation.
ENDFUNC     && end of IsValidMail.

*!*************************************************************
*! Name       : gfCstShtBrow
*! Developer  : AHMED MAHER (AMH)
*! Date       : 01/04/2004
*! Purpose    : Browse the cost sheet ID.
*!*************************************************************
*! Parameters : lcKeyValue = Filter Key
*!              lcFile_Ttl = Browse Title
*!              lcCstShtID = Variable to Hold the selected cost sheet ID
*!              lcBomHeadr = Alias name of the BOMHEADR cursor
*!              lnCstHeadr = 1 for Imported
*!                           2 for Manufactured
*!                           3 for Imported and Manufactured
*!                           4 for Material
*!*************************************************************
*! Return     : .F. if there is no cost sheet selected or .T. if there is a selected Cost sheet ID
*!*************************************************************
*! N037377,1 AMH
FUNCTION gfCstShtBrow
LPARAMETERS lcKeyValue,lcFile_Ttl,lcCstShtID,lcBomHeadr,lnCstHeadr

LOCAL llRet,lnAlias,lnCount,lcCount
lnAlias = SELECT(0)
SELECT (lcBomHeadr)

DIMENSION laCstLbl[7]

IF BITAND(1,lnCstHeadr) # 0
  *--- Array hold the memory variables that need to be load from the company setup file.
  DECLARE laMainSetp[7,2]
  FOR lnCount = 1 TO 7
    lcCount = STR(lnCount,1)
    laMainSetp[lnCount,1] = 'M_CISLBL'+lcCount
  ENDFOR
  =gfGetMemVar(@laMainSetp, oAriaApplication.ActiveCompanyID)
  FOR lnCount = 1 TO 7
    laCstLbl[lnCount] = laMainSetp[lnCount,2]
  ENDFOR
ENDIF

IF BITAND(2,lnCstHeadr) # 0
  *--- Array hold the memory variables that need to be load from the company setup file.
  DECLARE laMainSetp[7,2]
  FOR lnCount = 1 TO 7
    lcCount = STR(lnCount,1)
    laMainSetp[lnCount,1] = 'M_CMSLBL'+lcCount
  ENDFOR
  =gfGetMemVar(@laMainSetp, oAriaApplication.ActiveCompanyID)
  FOR lnCount = 1 TO 7
    laCstLbl[lnCount] = IIF(EMPTY(laCstLbl[lnCount]),'',laCstLbl[lnCount]+CHR(10)) + laMainSetp[lnCount,2]
  ENDFOR
ENDIF

IF BITAND(4,lnCstHeadr) # 0
  *--- Array hold the memory variables that need to be load from the company setup file.
  DECLARE laMainSetp[4,2]
  FOR lnCount = 1 TO 4
    lcCount = STR(lnCount,1)
    laMainSetp[lnCount,1] = 'M_CTSLBL'+lcCount
  ENDFOR
  =gfGetMemVar(@laMainSetp, oAriaApplication.ActiveCompanyID)
  FOR lnCount = 1 TO 4
    laCstLbl[lnCount] = laMainSetp[lnCount,2]
  ENDFOR
ENDIF

DIMENSION laTempData[1]
STORE '' TO laTempData

lcBrFields =                     "CITMMAJOR  :H='"+LANG_ARIA_CITMMAJOR+"'  ," +;
                                 "CCSTSHT_ID :H='"+LANG_ARIA_CCSTSHT_ID+"' ," +;
                                 "CCSTSHTDSC :H='"+LANG_ARIA_CCSTSHTDSC+"' ," +;
                                 "LDEFCSTSHT :H='"+LANG_ARIA_LDEFCSTSHT+"' ," +;
                                 "CSTATUS    :H='"+LANG_ARIA_CSTATUS+"'    ," +;
                                 "LBASONSIZ  :H='"+LANG_ARIA_LBASONSIZ+"'  ," +;
                                 "NCOST1     :H='"+ALLTRIM(laCstLbl[1])+"' ," +;
                                 "NCOST2     :H='"+ALLTRIM(laCstLbl[2])+"' ," +;
                                 "NCOST3     :H='"+ALLTRIM(laCstLbl[3])+"' ," +;
                                 "NCOST4     :H='"+ALLTRIM(laCstLbl[4])+"' ," +;
             IIF(lnCstHeadr=4,"","NCOST5     :H='"+ALLTRIM(laCstLbl[5])+"' ,")+;
             IIF(lnCstHeadr=4,"","NCOST6     :H='"+ALLTRIM(laCstLbl[6])+"' ,")+;
             IIF(lnCstHeadr=4,"","NCOST7     :H='"+ALLTRIM(laCstLbl[7])+"' ,")+;
                                 "TOTCOST    :H='"+LANG_ARIA_TOTCOST+"'    ," +;
                                 "CCSTSHTTYP :H='"+LANG_ARIA_CCSTSHTTYP+"'"

llRet = gfBrows(lcKeyValue,'CCSTSHT_ID','laTempData',lcFile_Ttl,.F.)
IF llRet
  lcCstShtID = laTempData[1]
ENDIF
SELECT (lnAlias)

RETURN llRet
*-- end of gfCstShtBrow.

*!*************************************************************
*! Name : gfOpnSqlFl (037923)
*! Auth : Khalid Mohi El-Din Mohamed (KHM)
*! Date : 04/01/2004.
*!*************************************************************
*! Synopsis : To open the SQL files.
*!*************************************************************
*! Passed :
*!        Parameters : 
*!		  lcTable     -> SQL table
*!		  lcCursor    -> Cursor to hold the SQL table
*!		  lcWhereCond -> Where condition to be used in the select statement
*!		  laIndex     -> Index on the cursor
*!		  lcTagName   -> Tag name
*!*************************************************************
*! Returned : 
*!*************************************************************
*! Example :
*!        DO gfOpnSqlFl WITH "ITEM",lcTmpFile,@laIndex,"Style"
*!*************************************************************
*!*	FUNCTION gfOpnSqlFl
*!*	LPARAMETERS lcTable,lcCursor,lcWhereCond,laIndex,lcTagName

*!*	LOCAL lnConnectionHandlar, lcODBC, lcUserName, lcPassWord, lnBuffering, lcSqlStatment

*!*	lcSqlStatment = "SELECT * FROM " + lcTable + " (INDEX="+lcTagName+")" + " WHERE "+lcWhereCond
*!*	lcODBC        = 'DS'+oAriaApplication.ActiveCompanyID
*!*	lcUserName    = 'sa'
*!*	lcPassWord    = ''

*!*	lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlrun(lcSqlStatment,lcCursor,lcTable,oAriaApplication.ActiveCompanyConStr,3,;
*!*	                                      'SAVE',SET("DATASESSION"))
*!*	IF lnConnectionHandlar = 1
*!*	  lnBuffering = CURSORGETPROP("Buffering",lcCursor)
*!*	  =CURSORSETPROP("Buffering",3,lcCursor)
*!*	  IF !EMPTY(laIndex)
*!*	    FOR lnCntr = 1 TO ALEN(laIndex,1)
*!*	      lcIndex = laIndex[lnCntr,1]
*!*	      lcTag   = laIndex[lnCntr,2]
*!*	      lcUnique = laIndex[lnCntr,3]
*!*	      IF TYPE("lcUnique") = "C"
*!*	        INDEX ON &lcIndex. &lcUnique. TAG (lcTag) OF (lcCursor)
*!*	      ELSE
*!*	        INDEX ON &lcIndex. TAG (lcTag) OF (lcCursor)
*!*	      ENDIF
*!*	    ENDFOR
*!*	    lcTag = laIndex[1,2]
*!*	    SET ORDER TO TAG (lcTag)
*!*	  ENDIF
*!*	  =CURSORSETPROP("Buffering",lnBuffering,lcCursor)
*!*	ELSE
*!*	  =oAriaApplication.RemoteCompanyData.CheckRetResult("sqlrun",lnConnectionHandlar,.T.)
*!*	  RETURN .F.
*!*	ENDIF

FUNCTION gfOpnSqlFl

*!B999999,1 WSH 02/07/2005, Add Parameter to allow opening Native Files Remotely. [Start]
*LPARAMETERS lcTable,lcCursor,lcWhereCond,laIndex,lcTagName
LPARAMETERS lcTable, lcCursor, lcWhereCond, laIndex, lcTagName, llNative
*!B999999,1 WSH 02/07/2005, [End]

*!B999999,1 WSH 02/07/2005, Add Variable to allow opening Native Files Remotely. [Start]
*--Get Connection String Type
IF llNative
  lcConnString = oAriaApplication.cAriaNativeDataFilesConStr
ELSE
  lcConnString = oAriaApplication.ActiveCompanyConStr
ENDIF
*!B999999,1 WSH 02/07/2005, [End]

LOCAL lnConnectionHandlar, lcODBC, lcUserName, lcPassWord, lnBuffering, lcSqlStatment

*!B999999,1 WSH 02/07/2005, Don't use index if it is not passed. [Start]
*lcSqlStatment = "SELECT * FROM " + lcTable + " (INDEX="+lcTagName+")" + " WHERE "+lcWhereCond

*B131608,1 WSH 05/03/2006 No "Index" if it is native. [Start]
*lcSqlStatment = "SELECT * FROM " + lcTable + IIF(TYPE("lcTagName") = 'C' AND !EMPTY(lcTagName), " (INDEX="+lcTagName+")", "") + " WHERE "+ lcWhereCond
lcSqlStatment = "SELECT * FROM " + lcTable + IIF(!llNative AND TYPE("lcTagName") = 'C' AND !EMPTY(lcTagName), " (INDEX="+lcTagName+")", "") + " WHERE "+ lcWhereCond
*B131608,1 WSH 05/03/2006 [End]

*!B999999,1 WSH 02/07/2005, [End]

lcODBC        = 'DS'+oAriaApplication.ActiveCompanyID
lcUserName    = 'sa'
lcPassWord    = ''

*!B999999,1 WSH 02/07/2005, Add Variable to allow opening Native Files Remotely. [Start]
*lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlrun(lcSqlStatment,lcCursor,lcTable,oAriaApplication.ActiveCompanyConStr,3,;
                                      'SAVE',SET("DATASESSION"))

*B131608,1 WSH 05/03/2006 Pass the Table Name to SQLRun method. [Start]
*lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlrun(lcSqlStatment,lcCursor,"",lcConnString,3,;
                                      'SAVE',SET("DATASESSION"))
lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlrun(lcSqlStatment,lcCursor,lcTable,lcConnString,3,;
                                      'SAVE',SET("DATASESSION"))
*B131608,1 WSH 05/03/2006 [Start]

*!B999999,1 WSH 02/07/2005, [End]

IF lnConnectionHandlar = 1
  lnBuffering = CURSORGETPROP("Buffering",lcCursor)
  =CURSORSETPROP("Buffering",3,lcCursor)
  IF !EMPTY(laIndex)
    FOR lnCntr = 1 TO ALEN(laIndex,1)
      lcIndex = laIndex[lnCntr,1]
      lcTag   = laIndex[lnCntr,2]
      lcUnique = laIndex[lnCntr,3]
      IF TYPE("lcUnique") = "C"
        *! B609207,1 MMT 04/12/2010 Error when more than one user try to open Rolls screen from PO Rec. Screen[Start]
        *INDEX ON &lcIndex. &lcUnique. TAG (lcTag) OF (lcCursor)
        INDEX ON &lcIndex. &lcUnique. TAG (lcTag) 
        *! B609207,1 MMT 04/12/2010 Error when more than one user try to open Rolls screen from PO Rec. Screen[End]
      ELSE
        *! B609207,1 MMT 04/12/2010 Error when more than one user try to open Rolls screen from PO Rec. Screen[Start]      
        *INDEX ON &lcIndex. TAG (lcTag) OF (lcCursor)        
        INDEX ON &lcIndex. TAG (lcTag) 
        *! B609207,1 MMT 04/12/2010 Error when more than one user try to open Rolls screen from PO Rec. Screen[ENd]        
      ENDIF
    ENDFOR
    lcTag = laIndex[1,2]
    SET ORDER TO TAG (lcTag)
  ENDIF
  =CURSORSETPROP("Buffering",lnBuffering,lcCursor)
ELSE
  =oAriaApplication.RemoteCompanyData.CheckRetResult("sqlrun",lnConnectionHandlar,.T.)
  RETURN .F.
ENDIF


*!*************************************************************
*! Name : gfUpdSqlFl (037923)
*! Auth : Khalid Mohi El-Din Mohamed (KHM)
*! Date : 04/01/2004.
*!*************************************************************
*! Synopsis : To update the SQL tables
*!*************************************************************
*! Passed :
*!        Parameters : 
*!        lcTable 		   -> SQL table
*!        LcPrimaryKeyList -> List of the index fields.
*!        lcSQLTable 	   -> SQL table
*!*************************************************************
*! Example :
*!        DO gfUpdSqlFl WITH "ITEM","cInvType,Style"
*!*************************************************************
FUNCTION gfUpdSqlFl
LPARAMETERS lcTable,LcPrimaryKeyList, lcSQLTable

LOCAL lnConnectionHandlar, lcTranCode, lcODBC, lcUserName, lcPassWord, llReturn

lcODBC     = 'DS'+oAriaApplication.ActiveCompanyID
lcUserName = 'sa'
lcPassWord = ''
llReturn   = .T.

lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr,3,'')

IF TYPE('lcTranCode') = 'N'
  =oAriaApplication.RemoteCompanyData.CheckRetResult("BeginTran",lcTranCode,.T.)
  IF BETWEEN(lnRecNo,1,RECCOUNT(lcTable))
    GOTO lnRecNo IN (lcTable)
  ENDIF
  RETURN .F.
ENDIF

lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate(lcTable,lcTranCode,SET("DATASESSION"),LcPrimaryKeyList,lcSQLTable)
IF lnConnectionHandlar # 1 .AND. lnConnectionHandlar # 2
  =oAriaApplication.RemoteCompanyData.CheckRetResult("sqlupdate",lnConnectionHandlar,.T.)
  llReturn = .F.
ENDIF

lnConnectionHandlar = oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode)
IF lnConnectionHandlar # 1
  =oAriaApplication.RemoteCompanyData.CheckRetResult("CommitTran",lnConnectionHandlar,.T.)
  llReturn = .F.
ENDIF
RETURN llReturn

*!*************************************************************
*! Name : gfAdItemWar  (037923)
*! Auth : Khalid Mohi El-Din Mohamed (KHM)
*! Date : 04/01/2004.
*!*************************************************************
*! Synopsis : This function will add a record to the new ITEMLOC
*!*************************************************************
*! Passed :
*!        Parameters : 
*!        lcInvtype -> "0001" for Style, "0002" for Fabric (For example)
*!        lcItem -> Fabric/Color or Style code
*!        lcDyelot -> Dye lot
*!        lcWareH -> Warehouse
*!        lcTmpScope -> To be replaced in the DYE_REL. cTmpScope. 
*!*************************************************************
*! Returned : 
*!*************************************************************
*! Example :
*!        DO gfAdItemWar WITH lcStyle,SPACE(10),lcWareCode
*!*************************************************************
FUNCTION gfAdItemWar
LPARAMETERS lcInvtype, lcItem, lcDyelot, lcWareH, lcTmpScope
PRIVATE lcDesc, lcAlias, lnCost , lcDiscCods, lcGlCode, lcItemTran
lcDesc = ""
lcAlias = ALIAS()
STORE 0 TO lnCost , lcDiscCods
IF TYPE('lcTmpScope') $ 'UL'
  lcTmpScope = ''
ENDIF

IF TYPE("lcTmpItem") $ "UL" OR !USED(lcTmpItem)
  PRIVATE lcTmpItem
  lcTmpItem = gfTempName()
ENDIF  
IF TYPE("lcTmpItemLoc") $ "UL" OR !USED(lcTmpItemLoc)
  PRIVATE lcTmpItemLoc
  lcTmpItemLoc = gfTempName()
ENDIF

*-- To get the item description, average cost and discount code to replace it in the new 
*-- added record in the ItemLoc file.
=gfOpnSqlFl('ITEM',lcTmpItem,;
             "CINVTYPE = '" + lcInvtype + "'" + " AND Style ='" + lcItem + "'","","Style")
SELECT (lcTmpItem)
LOCATE
lcDesc     = Desc
lnCost     = Ave_Cost
lcDiscCods = cDiscCode
lcGlCode   = Link_Code
*N119813,1 AMH Open the warehouse file if not opened yet [Start]
LOCAL llOpenWare
llOpenWare = .F.
IF !USED('WareHous')
  llOpenWare = gfOpenFile(oAriaApplication.DataDir+'WareHous','WareHous','SH')
ENDIF
*N119813,1 AMH [End]

*lcGlCode   = IIF(SEEK(PADR(lcWareH,6),'WareHous'),WareHous.GL_LINK,'DEFDEF')

*-- Amin, Enhance the performance of the Where Condition. [Start] 
*-- To get a temporary cursor of the ItemLoc file to add the record and update the master file
*=gfOpnSqlFl('ITEMLOC',lcTmpItemLoc,;
             "cInvType = '" + "    " + "'" + " AND Style ='" + "        " + "'","","StyDye")

*N119813,1 AMH Close the warehouse file if opened in this sission then open the itemloc file with correct SQL statment [Start]
*=gfOpnSqlFl('ITEMLOC',lcTmpItemLoc,"FALSE","","StyDye")
IF llOpenWare AND USED('WareHous')
  USE IN WareHous
ENDIF

LOCAL lnConnectionHandlar, lcSqlStatment

lcSqlStatment = "SELECT TOP 0 * FROM ITEMLOC"
lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlrun(lcSqlStatment,lcTmpItemLoc,"ITEMLOC",oAriaApplication.ActiveCompanyConStr,3,;
                                      'SAVE',SET("DATASESSION"))
IF lnConnectionHandlar <> 1
  =oAriaApplication.RemoteCompanyData.CheckRetResult("sqlrun",lnConnectionHandlar,.T.)
ENDIF
*N119813,1 AMH [End]
*-- Amin, Enhance the performance of the Where Condition. [End]


INSERT INTO (lcTmpItemLoc) (cInvType, Style, Desc, cDiscCode, Dyelot, cWareCode, Ave_Cost,;
              nAveCstBuy, GL_Link);
          VALUES (lcInvtype, lcItem, lcDesc, lcDiscCods, lcDyelot, lcWareH,;
               IIF(EMPTY(lcDyelot),lnCost,0), IIF(EMPTY(lcDyelot),lnCost,0),;
               lcGlCode)

*B131608,1 WSH 05/03/2006 [Start]
=gfAdd_info(lcTmpItemLoc)
*B131608,1 WSH 05/03/2006 [End]

*-- Updating the master ItemLoc file
SELECT(lcTmpItemLoc)
IF gfUpdSqlFl(lcTmpItemLoc,'Style,WareHous,Dyelot')
  =TABLEUPDATE(.T.,.T.)
ELSE
  =TABLEREVERT(.T.)
ENDIF

*-- [Khalid] This trigger will be called for all types of inventory that are classified for
*-- Sell (if the "S" $ cItemTran)
*C200171 TMI [Start] Gen Upcs in EDICATGD
*-- Run if EDI installed
*!*  IF ASCAN(oAriaApplication.laEvntTrig,PADR("GNUPCWH",10)) <> 0
*!*    IF OCCURS('NC',oAriaApplication.CompanyInstalledModules)<>0
*!*      lcWhCode   = lcPWare
*!*      lcSty      = lcPStyle
*!*      llFrmAdWre = .T.
*!*      =gfDoTriger('ICSTYLE','GNUPCWH   ')
*!*    ENDIF
*!*  ENDIF
*C200171 TMI [End  ] Gen Upcs in EDICATGD

*-- Arrange the dyelots for all types of inventory that are calssified to be "Used".
*-- (if the "U" $ cItemTran)
IF TYPE("lcInvTypeF") $ "UL" OR !USED(lcInvTypeF)
  PRIVATE lcInvTypeF
  lcInvTypeF = gfTempName()
ENDIF  
*-- To get the cItemTran
=gfOpnSqlFl('INVTYPE',lcInvTypeF,;
             "CINVTYPE = '" + lcInvtype + "'" ,"","CINVTYPE")
SELECT(lcInvTypeF)
LOCATE
lcItemTran = cItemTran

IF 'U' $ lcItemTran AND !EMPTY(lcDyelot)
  IF TYPE("lcTmpDyRel") $ "UL" OR !USED(lcTmpDyRel)
    lcTmpDyRel = gfTempName()
  ENDIF
 
  DIMENSION laIndex[2,3]
  laIndex[1,1] = "cInvType+cItem+Dyelot"
  laIndex[1,2] = lcTmpDyRel
  laIndex[2,1] = "cInvType+cItem+cDye_Seq"
  laIndex[2,2] = "SEQUENCE"

  =gfOpnSqlFl('Dye_Rel',lcTmpDyRel,;
             "cInvType = '" + lcInvtype + "'" + " AND cItem ='" + lcItem + "'",@laIndex,"Dye_Rel")

  SELECT (lcTmpDyRel)
  *-- if you did not find this item, dyelot record 
  IF !SEEK(lcInvtype+lcItem+lcDyelot)
    PRIVATE lnNearest,lcDye_Seq
    lcDye_Seq = ''
    lnNearest = RECNO(0)
    
    *-- Add this block to adjust add records at top of file.
    IF (lnNearest # 0)
      GO lnNearest
      IF cItem <> lcItem
        SET ORDER TO (lcTmpDyRel) DESCENDING
        = SEEK(lcInvType+lcItem+lcDyelot)
        lnNearest = RECNO(0)
        IF lnNearest # 0
          GO lnNearest
          IF (cItem <> lcItem) OR (Dyelot <> lcDyelot)
            lnNearest = 0
          ENDIF
        ENDIF     
      ENDIF  
    ENDIF    

    *-- if it is the Last dyelot code.
    IF lnNearest = 0
      SELECT (lcTmpDyRel)
      SET ORDER TO TAG SEQUENCE DESCENDING
      = SEEK(lcInvType+lcItem)
      lcDye_Seq = PADL(ALLTRIM(STR(VAL(cDye_Seq) + 1)),4,'0')
    ENDIF  

    SET ORDER TO
    IF lnNearest # 0
      GO lnNearest
      lcDye_Seq = cDye_Seq

      SCAN FOR cItem+cDye_Seq = lcItem AND cDye_Seq >= lcDye_Seq
        REPLACE cDye_Seq WITH PADL(ALLTRIM(STR(VAL(cDye_Seq) + 1)),4,'0')
        
        *B131608,1 WSH 05/03/2006 [Start]
        =gfAdd_info(lcTmpDyRel)
        *B131608,1 WSH 05/03/2006 [End]
        
      ENDSCAN 
    ENDIF
    
    *-- insert new dyelot line.
    INSERT INTO (lcTmpDyRel)                      ;
             (cInvType, cItem, Dyelot, cDye_Seq , cTmpScope ) ;
      VALUES (lcInvType, lcItem, lcDyelot, lcDye_Seq, lcTmpScope)     
    
    *B131608,1 WSH 05/03/2006 [Start]
    =gfAdd_info(lcTmpDyRel)
    *B131608,1 WSH 05/03/2006 [End]
    
    *-- Updating the master Dye_Rel file
    SELECT(lcTmpDyRel)
    SET ORDER TO (lcTmpDyRel)
    IF gfUpdSqlFl(lcTmpDyRel,'cInvType,cItem,Dyelot')
      =TABLEUPDATE(.T.,.T.)
    ELSE
      =TABLEREVERT(.T.)
    ENDIF
  ENDIF 
ENDIF

*!*************************************************************
*! Name : gfItmDyBrw (037923)
*! Auth : Khalid Mohi El-Din Mohamed (KHM)
*! Date : 04/01/2004.
*!*************************************************************
*! Synopsis : To browse the item's dyelots
*!*************************************************************
*! Passed :
*!        Parameters : 
*!        lcInvtype -> "0001" for Style, "0002" for Fabric
*!        lcItem -> Item code (style or fabric-color)
*!        lcDyelot -> Dye lot code
*!        lcWareCode -> Warehouse Code
*!        llRetAlias -> To return the selected alias or not
*!        llIncAvail -> To display the browse fields according to this parameter
*!        llTop -> To determine the top of the browse.
*!        lcBrwAlias -> Alias to be browsed
*!        lcIndTag -> ITEMLOC tag.
*!        llUseConfg -> To check if using configuration or dye lot.
*!*************************************************************
*! Returned : 
*!*************************************************************
*! Example :
*!        DO gfItmDyBrw WITH lcStyle,SPACE(10),lcWareCode
*!*************************************************************
FUNCTION gfItmDyBrw
LPARAMETERS lcInvtype, lcItem, lcDyelot, lcWareCode, llRetAlias, llIncAvail, llTop,;
            lcBrwAlias, lcIndTag, llUseConfg

PRIVATE lcBrFields,lcAlias,lnStyOrd,lcFromWare,lnAlias,laData,lcBrwAlias,lcDataDir,lcIndTag
lcFromWare = lcWareCode
llOpenFile = .F.
lnAlias    = SELECT()

IF TYPE("lcTmpItemLoc") $ "UL" OR !USED(lcTmpItemLoc)
  PRIVATE lcTmpItemLoc
  lcTmpItemLoc = gfTempName()
ENDIF

IF TYPE("lcBrwAlias") $ "UL"
  DIMENSION laIndex [1,3]
  laIndex[1,1] = "Style+cWareCode+Dyelot"  
  laIndex[1,2] = lcTmpItemLoc
  *-- To get a temporary cursor of the ItemLoc file to add the record and update the master file
  
  *N119813,1 AMH Consider case of warehouse code not passed [Start]
  *=gfOpnSqlFl('ITEMLOC',lcTmpItemLoc,;
          "cInvType = '" + lcInvtype + "'" + " AND Style ='" + lcItem + "'"+;
          " AND cWareCode ='" + lcWareCode + "'" + " AND Dyelot<>'" + "          "+"'",@laIndex,"StyDye")
  =gfOpnSqlFl('ITEMLOC',lcTmpItemLoc,;
          "cInvType = '" + lcInvtype + "'" + " AND Style ='" + lcItem + "'"+;
          IIF(TYPE('lcWareCode')$'UL',""," AND cWareCode ='" + lcWareCode + "'")+;
          " AND Dyelot<>'" + "          "+"'",@laIndex,"StyDye")
  *N119813,1 AMH [End]

  lcBrwAlias = lcTmpItemLoc
ELSE
  lcDataDir  = oAriaApplication.WorkDir
  llOpenFile = gfOpenFile(lcDataDir+lcBrwAlias,lcDataDir+lcIndTag,'SH')
ENDIF
lcIndTag   = IIF(TYPE("lcIndTag") $ "UL",lcTmpItemLoc,lcIndTag)

*-- array to get values from browse
DECLARE laData[3]
laData     = ' '
*-- variable to determine forcing browse or not
llBrowse   = IIF(TYPE('llBrowse')='U',.F.,llBrowse)

PRIVATE lnBrHSRow1, lnBrHSCol1, lnBrHSRow2, lnBrHSCol2
IF llTop
  lnBrHSRow1 = gnBrFSRow1
  lnBrHSCol1 = gnBrFSCol1
  lnBrHSRow2 = gnBrHSRow1
  lnBrHSCol2 = gnBrHSCol1
ELSE
  lnBrHSRow1 = gnBrHSRow1
  lnBrHSCol1 = gnBrHSCol1
  lnBrHSRow2 = gnBrHSRow2
  lnBrHSCol2 = gnBrHSCol2
ENDIF
lcItem     = PADR(lcItem, 19)
lcFromWare = IIF(TYPE('lcFromWare') $ 'UL' .OR. EMPTY(lcFromWare),'', lcFromWare)
lcTitle    = "Style "+ALLTRIM(lcItem)+IIF(llUseConfg,LANG_ARIA_Config,LANG_ARIA_Dyelot)
lcHeader   = IIF(llUseConfg,LANG_ARIA_ConfigNum,LANG_ARIA_DyelotNum)
lcBrFields = "DYELOT:R :H='"+lcHeader+"':17,"

IF llIncAvail
  lcBrFields = lcBrFields +; 
           "A1=STK1-ALO1  :R :H='"+LANG_ARIA_Avl1+"':12,"+;                                   
             "A2=STK2-ALO2  :R :H='"+LANG_ARIA_Avl2+"':12,"+;
           "A3=STK3-ALO3  :R :H='"+LANG_ARIA_Avl3+"':12,"+;
           "A4=STK4-ALO4  :R :H='"+LANG_ARIA_Avl4+"':12,"+;
           "A5=STK5-ALO5  :R :H='"+LANG_ARIA_Avl5+"':12,"+;
           "A6=STK6-ALO6  :R :H='"+LANG_ARIA_Avl6+"':12,"+;
           "A7=STK7-ALO7  :R :H='"+LANG_ARIA_Avl7+"':12,"+;
           "A8=STK8-ALO8  :R :H='"+LANG_ARIA_Avl8+"':12,"+;
           "A9=TOTSTK-TOTALO :R :H='"+LANG_ARIA_TotAvl+"':13,"
  lcBrFields = lcBrFields +; 
           "ALO1  :R :H='"+LANG_ARIA_Alo1+"':12,"+;
           "ALO2  :R :H='"+LANG_ARIA_Alo2+"':12,"+;
           "ALO3  :R :H='"+LANG_ARIA_Alo3+"':12,"+;
           "ALO4  :R :H='"+LANG_ARIA_Alo4+"':12,"+;
           "ALO5  :R :H='"+LANG_ARIA_Alo5+"':12,"+;
           "ALO6  :R :H='"+LANG_ARIA_Alo6+"':12,"+;
           "ALO7  :R :H='"+LANG_ARIA_Alo7+"':12,"+;
           "ALO8  :R :H='"+LANG_ARIA_Alo8+"':12,"+;
             "TOTALO:R :H='"+LANG_ARIA_TotAlo+"':13,"+;
           "STK1  :R :H='"+LANG_ARIA_Stk1+"':12,"+;                                   
             "STK2  :R :H='"+LANG_ARIA_Stk2+"':12,"+;
           "STK3  :R :H='"+LANG_ARIA_Stk3+"':12,"+;
           "STK4  :R :H='"+LANG_ARIA_Stk4+"':12,"+;
           "STK5  :R :H='"+LANG_ARIA_Stk5+"':12,"+;
           "STK6  :R :H='"+LANG_ARIA_Stk6+"':12,"+;
           "STK7  :R :H='"+LANG_ARIA_Stk7+"':12,"+;
           "STK8  :R :H='"+LANG_ARIA_Stk8+"':12,"+;
           "TOTSTK:R :H='"+LANG_ARIA_TotStk+"':13"
ELSE
  lcBrFields = lcBrFields +; 
           "STK1  :R :H='"+LANG_ARIA_Stk1+"':12,"+;                                   
             "STK2  :R :H='"+LANG_ARIA_Stk2+"':12,"+;
           "STK3  :R :H='"+LANG_ARIA_Stk3+"':12,"+;
           "STK4  :R :H='"+LANG_ARIA_Stk4+"':12,"+;
           "STK5  :R :H='"+LANG_ARIA_Stk5+"':12,"+;
           "STK6  :R :H='"+LANG_ARIA_Stk6+"':12,"+;
           "STK7  :R :H='"+LANG_ARIA_Stk7+"':12,"+;
           "STK8  :R :H='"+LANG_ARIA_Stk8+"':12,"+;
           "TOTSTK:R :H='"+LANG_ARIA_TotStk+"':13,"+;
           "ALO1  :R :H='"+LANG_ARIA_Alo1+"':12,"+;
           "ALO2  :R :H='"+LANG_ARIA_Alo2+"':12,"+;
           "ALO3  :R :H='"+LANG_ARIA_Alo3+"':12,"+;
           "ALO4  :R :H='"+LANG_ARIA_Alo4+"':12,"+;
           "ALO5  :R :H='"+LANG_ARIA_Alo5+"':12,"+;
           "ALO6  :R :H='"+LANG_ARIA_Alo6+"':12,"+;
           "ALO7  :R :H='"+LANG_ARIA_Alo7+"':12,"+;
           "ALO8  :R :H='"+LANG_ARIA_Alo8+"':12,"+;
             "TOTALO:R :H='"+LANG_ARIA_TotAlo+"':13"
ENDIF


SELECT (lcBrwAlias)
lnStyOrd   = VAL(SYS(21))
SET ORDER TO TAG (lcIndTag)
SEEK lcItem + lcFromWare
LOCATE REST WHILE Style+cWareCode+Dyelot = lcItem+lcFromWare;
            FOR !EMPTY(DYELOT)  
IF !FOUND()
  lcTmpStr = ALLTRIM(lcItem) + IIF(EMPTY(lcFromWare) , LANG_ARIA_MsgVar1 , LANG_ARIA_MsgVar2 + ALLTRIM(lcFromWare))
  IF llUseConfg
    =gfModalGen("INM00407B00000" , "DIALOG" , lcTmpStr)
  ELSE
    =gfModalGen("INM00277B00000" , "DIALOG" , lcTmpStr)
  ENDIF
  llWasSel = .F.
ELSE
  SELECT (lcBrwAlias)
  llWasSel= ARIABROW("'"+lcItem+lcFromWare+"' FOR !EMPTY(DYELOT)",lcTitle,;
            lnBrHSRow1, lnBrHSCol1, lnBrHSRow2, lnBrHSCol2,'','',"Style,Dyelot","laData")

  
  IF llWasSel
    lcItem  = laData[1]
    lcDyelot = laData[2]
  ELSE
    lcDyelot = SPACE(10)
  ENDIF  
ENDIF

SET ORDER TO lnStyOrd
IF llOpenFile .AND. USED(lcBrwAlias)
  USE IN (lcBrwAlias)
ENDIF   
SELECT (lnAlias)

RETURN llWasSel

*!*************************************************************
*! Name : gfBrowWarH (037923)
*! Auth : Khalid Mohi El-Din Mohamed (KHM)
*! Date : 04/01/2004.
*!*************************************************************
*! Synopsis : This function will browse the wharehouses
*!*************************************************************
*! Passed Parameters : 
*!                   llIsEscape -> If the escape is allowed
*!                   lcInvtype -> For example "0001" for Style, "0002" for Fabric 
*!                   lcItem  -> Fabric or Style Code
*!                   lcWareCode -> Warehouse Code
*!                   lcStyMatIn -> "S" for finished good inventory, "M" for material inventory
*!                   llForCurrSite -> for site id in case of NC is installed.
*!*************************************************************
*! Returned : lcWareCode
*!*************************************************************
*! Example :
*!        DO gfBrowWarH WITH .F.,,,,'S',''
*!*************************************************************
FUNCTION gfBrowWarH
LPARAMETERS llIsEscape, lcInvType, lcItem, lcWareCode, lcStyMatIn, llForCurrSite

PRIVATE lcWareCode, lnCurAlias, lnCurTag
lcForCond = ''

IF TYPE("lcTmpItemLoc") $ "UL" OR !USED(lcTmpItemLoc)
  PRIVATE lcTmpItemLoc
  lcTmpItemLoc = gfTempName()
ENDIF

*-- Include the .H file
#INCLUDE R:\ARIA4XP\PRGS\SY\WAREBROW.H

GO TOP IN WareHous
IF EOF('WareHous')
  =MESSAGEBOX("No locations found.",16,_Screen.Caption)
  RETURN SPACE(6)
ENDIF

*-- Save the current alias.
lnCurAlias = SELECT()

SELECT WAREHOUS
lnCurTag    = VAL(SYS(21))
SET ORDER TO TAG WAREHOUS

lcWareCode  = IIF(EMPTY(lcWareCode), SPACE(6), lcWareCode)
*-- If called from browsing the warehouses of a specific item/color.
IF !EMPTY(lcItem)
  DIMENSION laIndex[1,3]
  laIndex[1,1] = "cWareCode"
  laIndex[1,2] = lcTmpItemLoc

  =gfOpnSqlFl('ITEMLOC',lcTmpItemLoc,;
   				"cInvType = '" + lcInvType + "'" +;
   				" AND Style ='" + lcItem + "'" + " AND Dyelot='"+ "          "+"'",@laIndex,"StyDye")
  SELECT(lcTmpItemLoc)
  LOCATE
  llFound   = !EOF()
  IF !llFound
    =MESSAGEBOX("Item: " + ALLTRIM(lcItem) + " is not assigned to any location.",64)
    SET ORDER TO lnCurTag IN WAREHOUS
    SELECT (lnCurAlias)
    RETURN SPACE(6)
  ENDIF
  SELECT WAREHOUS
  SET RELATION TO  cWareCode ;
               INTO (lcTmpItemLoc) ADDITIVE
  lcForCond   = '!EOF(lcTmpItemLoc)'
ELSE
  lcWareCode = SPACE(6)  
ENDIF  


IF TYPE('lcStyMatIn')='C' AND lcStyMatIn $ 'SM'
  lcMatOrSty = IIF(UPPER(lcStyMatIn) = 'S','lStyInv','lMatInv')
  lcForCond  = lcForCond +IIF(EMPTY(lcForCond),'',' AND ') + lcMatOrSty
ENDIF

IF llForCurrSite
  lcForCond  = lcForCond +IIF(EMPTY(lcForCond),'',' AND ') + "cSiteId = oAriaApplication.CurrentSite"
ENDIF

DO FORM (oAriaApplication.ScreenHome+"IC\Warebrow.scx") WITH lcForCond TO lcWareCode

SET ORDER TO lnCurTag IN WAREHOUS

SELECT (lnCurAlias)
POP KEY
lcWareCode = IIF(EMPTY(lcWareCode),SPACE(6),lcWareCode)
RETURN (lcWareCode)

*!*************************************************************
*! Name : gfAddBrwDy (037923)
*! Auth : Khalid Mohi El-Din Mohamed (KHM)
*! Date : 04/01/2004.
*!*************************************************************
*! Synopsis : To browse/add dyelots for a specific item
*!*************************************************************
*! Passed :
*!        Parameters : 
*!		  lcInvtype -> "0001" for Style, "0002" for Fabric
*!		  lcItem -> Item code (style or fabric-color)
*!		  lcDyelot -> Dye lot code
*!		  lcWareCode -> Warehouse Code
*!		  llUseConfg -> If the system uses configuration or dye lot
*!*************************************************************
*! Returned : 
*!*************************************************************
*! Example :
*!        DO gfAddBrwDy WITH "0001",lcStyle,SPACE(10),lcWareCode
*!*************************************************************
FUNCTION gfAddBrwDy
LPARAMETERS lcInvtype, lcItem, lcDyelot, lcWareCode, llUseConfg
PRIVATE lnAlias, llMultiWH

lnAlias=SELECT()
llMultiWH  = ALLTRIM(UPPER(gfGetMemVar('M_WareHouse')))='Y'

IF TYPE("lcTmpItemLoc") $ "UL" OR !USED(lcTmpItemLoc)
  lcTmpItemLoc = gfTempName()
ENDIF

DIMENSION laIndex [1,3]
laIndex[1,1] = "Style+cWareCode+Dyelot"  
laIndex[1,2] = lcTmpItemLoc
*-- To get a temporary cursor of the ItemLoc file to add the record and update the master file
=gfOpnSqlFl('ITEMLOC',lcTmpItemLoc,;
    			"cInvType = '" + lcInvtype + "'" + " AND Style ='" + lcItem + "'"+;
    			" AND cWareCode = '" + lcWareCode +"'" + " AND Dyelot='" + lcDyelot + "'",@laIndex,"StyDye")

SELECT (lcTmpItemLoc)
LOCATE
IF EOF()
  IF llMultiWH
    *lcmesgx = 'Style\Warehouse :'+ALLTRIM(lcStyle)+'\'+ALLTRIM(lcWare)
    lcmesgx = LANG_ARIA_MsgSgx1+ALLTRIM(lcItem)+'\'+ALLTRIM(lcWareCode)
  ELSE
    *lcmesgx = 'Item  :'+ALLTRIM(lcStyle)
    lcmesgx = LANG_ARIA_MsgSgx2+ALLTRIM(lcItem)
  ENDIF
  IF llUseConfg
    lnChoice=gfModalGen('TRM42254B42003','DIALOG',ALLTRIM(lcItem)+'|'+ALLTRIM(lcDyelot)+'|'+lcWareCode)
  ELSE
    lnChoice=gfModalGen('TRM42062B42003','DIALOG',ALLTRIM(lcItem)+'|'+ALLTRIM(lcDyelot)+'|'+lcWareCode)
  ENDIF
  IF lnChoice = 1
    =gfOpnSqlFl('ITEMLOC',lcTmpItemLoc,;
       			  "cInvType = '" + lcInvtype + "'" + " AND Style ='" + lcItem + "'"+;
    			  " AND cWareCode = '" + lcWareCode + "'" ,@laIndex,"StyDye")
    SELECT (lcTmpItemLoc)
    LOCATE
    IF EOF()
      =gfAdItemWar(lcInvType, lcItem, SPACE(10), lcWareCode )
    ENDIF
      =gfAdItemWar(lcInvType, lcItem, lcDyelot, lcWareCode )
    SELECT(lnAlias)  
    RETURN  
  ELSE   
    IF lnChoice = 2
      =gfOpnSqlFl('ITEMLOC',lcTmpItemLoc,;
       			  "cInvType = '" + lcInvtype + "'" + " AND Style ='" + lcItem + "'"+;
    			  " AND cWareCode = '" + lcWareCode + "'",@laIndex,"StyDye")
      SELECT(lcTmpItemLoc)
      LOCATE
      IF !EOF()
        LOCATE REST WHILE Style+cWareCode+Dyelot = PADR(lcItem,19)+lcWareCode;
        		    FOR !EMPTY(Dyelot)
        IF !FOUND()
		  IF llUseConfg
		    =gfModalGen('TRM42253B42001','DIALOG',lcmesgx)
		  ELSE
		    =gfModalGen('TRM42053B42001','DIALOG',lcmesgx)
		  ENDIF
          lcDyelot = SPACE(10)
          SELECT(lnAlias)
          RETURN
        ELSE
          lcHeader   = IIF(llUseConfg,LANG_ARIA_ConfigNum,LANG_ARIA_DyelotNum)
		  lcBrfields = "Dyelot  :H='"+lcHeader+"' ,"
		  lcBrFields = lcBrFields +; 
 	      			   "STK1  :R :H='"+LANG_ARIA_Stk1+"':12,"+;
		               "STK2  :R :H='"+LANG_ARIA_Stk2+"':12,"+;
	       			   "STK3  :R :H='"+LANG_ARIA_Stk3+"':12,"+;
	  	 	           "STK4  :R :H='"+LANG_ARIA_Stk4+"':12,"+;
			           "STK5  :R :H='"+LANG_ARIA_Stk5+"':12,"+;
		  	           "STK6  :R :H='"+LANG_ARIA_Stk6+"':12,"+;
			           "STK7  :R :H='"+LANG_ARIA_Stk7+"':12,"+;
	        		   "STK8  :R :H='"+LANG_ARIA_Stk8+"':12,"+;
		  	           "TOTSTK:R :H='"+LANG_ARIA_TotStk+"':13,"+;
		 	           "ALO1  :R :H='"+LANG_ARIA_Alo1+"':12,"+;
		 	           "ALO2  :R :H='"+LANG_ARIA_Alo2+"':12,"+;
			           "ALO3  :R :H='"+LANG_ARIA_Alo3+"':12,"+;
	        		   "ALO4  :R :H='"+LANG_ARIA_Alo4+"':12,"+;
			           "ALO5  :R :H='"+LANG_ARIA_Alo5+"':12,"+;
	        		   "ALO6  :R :H='"+LANG_ARIA_Alo6+"':12,"+;
	        		   "ALO7  :R :H='"+LANG_ARIA_Alo7+"':12,"+;
			           "ALO8  :R :H='"+LANG_ARIA_Alo8+"':12,"+;
		               "TOTALO:R :H='"+LANG_ARIA_ToTAlo+"':13"
					   lcDyelot = SPACE(10)
			SELECT(lcTmpItemLoc)
			IF ARIABROW([FOR !EMPTY(Dyelot)],;
			           IIF(llUseConfg,LANG_ARIA_StyConfg,LANG_ARIA_StyDyelot),;
			               gnBrHSRow1, gnBrHSCol1, gnBrHSRow2, gnBrHSCol2)
			  lcDyelot = Dyelot
			ENDIF

        ENDIF
      ENDIF
    ELSE
      lcDyelot = SPACE(10)
      SELECT(lnAlias)
      RETURN
    ENDIF
  ENDIF
ENDIF
SELECT(lnAlias)
RETURN

*!*************************************************************
*! Name : gfItemBrow (037923)
*! Auth : Khalid Mohi El-Din Mohamed (KHM)
*! Date : 04/01/2004.
*!*************************************************************
*! Synopsis : To browse styles/fabrics
*!*************************************************************
*! Passed :
*!        Parameters : 
*!      lcInvtype -> "0001" for Style, "0002" for Fabric
*!      lcItem  -> Fabric or Style Code
*!      lcColor -> Color Code
*!      lcSeason -> "*" for all season, otherwise a specific season.
*!      lcMajorOrNon -> For none-major segment.
*!      lcForExp -> If there is any specific expression.
*!      llRetAlias -> To return the selected alias before entering this function.
*!*************************************************************
*! Returned : 
*!*************************************************************
*! Example :
*!        DO gfItemBrow WITH "0001"
*!*************************************************************
FUNCTION gfItemBrow
LPARAMETERS lcInvtype, lcItem, lcColor, lcSeason, lcMajorOrNon, lcForExp, llRetAlias

PRIVATE lcBrFields,lnCurAlias, lcStyle,laData ,lcStyOrder ,lcStyTtl ,lcTitle ,;
         lcXMjr ,lcXNMjr ,lcClrTtl ,lcStyOrder,lcMajorOrNon, lnMajLen,laIndex, laIndex2


DECLARE laData[3] && array to get values from browse
STORE '' TO laData,lcScope
*-- variable to determine forcing browse or not
llBrowse = IIF(TYPE('llBrowse')='U',.T.,llBrowse)

IF !USED('Codes')
  lcCurAlias = ALIAS()
  =gfOpenFile(oAriaApplication.Datadir+'Codes','Codes','SH')
  SELECT(lcCurAlias)
ENDIF
lcSeason     = IIF(TYPE('lcSeason')$'UL',"",lcSeason)
lcMajorOrNon = IIF(TYPE('lcMajorOrNon')$'UL',"M",lcMajorOrNon)
lcStyTtl = IIF(lcMajorOrNon='M',gfItemMask('HM','',lcInvType),gfItemMask('HN','',lcInvType))
lcTitle  = IIF(lcMajorOrNon='M',gfItemMask('HM','',lcInvType),;
       IIF(lcMajorOrNon="N",gfItemMask('HN','',lcInvType),gfItemMask('HI','',lcInvType)))
lcXMjr  = gfItemMask('HM','',lcInvType)
lcXNMjr = gfItemMask('HN','',lcInvType)

*E124065,1 AMH Get the color position and width [Start]
LOCAL ARRAY laItemSeg[1,1]
=gfItemMask(@laItemSeg,'',lcInvType)
PRIVATE lnClrStrt,lnClrWidth

FOR lnI = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnI,1] = 'C'
    lnClrStrt  = laItemSeg[lnI,4]
    lnClrWidth = LEN(laItemSeg[lnI,3])
    EXIT
  ENDIF
ENDFOR
*E124065,1 AMH [End]

lcClrTtl = lcXMjr +'/'+ lcXNMjr
lnMajLen = LEN(gfItemMask('PM','',lcInvType))

*!B039082,1 WSH, 02/23/2005, Enhance Item Browse... Use new browse technique. [Start]
*!*  IF TYPE("lcTmpItem") $ "UL" OR !USED(lcTmpItem)
*!*    lcTmpItem = gfTempName()
*!*    
*!*    *N119680,1 AMH Use the specific expression [Start]
*!*    LOCAL lcWhereCond
*!*    lcWhereCond = "CINVTYPE = '" + lcInvtype + "'" + IIF(EMPTY(lcForExp),""," AND ") + lcForExp
*!*    *N119680,1 AMH [End]
*!*    
*!*    *E038245,1 WSH Enhance the way of openning SQL Item file [Start]
*!*    LOCAL lcStatement
*!*    
*!*    lcStatement = "SELECT Style, cStyMajor, Status, cDefWare, [Desc], Desc1, Season, cDivision," +;
*!*                  "       PriceA, PriceB, PriceC, Fabric, cStyGrade, Royalty, Pattern, Scale, " +;
*!*                  "       Prepak, cBuyPrepk, Qty_Ctn, Commission, Link_Code, Make, nMCost1, " +;
*!*                  "       nMCost2, nMCost3, nMCost4, nMCost5, nMCost6, nMCost7, nICost1, nICost2, " +;
*!*                  "       nICost3, nicost4, nICost5, nICost6, nICost7, nPrCost2, nPrCost3, nPrCost4, nPrCost5, nPrCost6, nPrCost7, " +;
*!*                  "       TotCost, Ave_Cost, nStkVal, SoldOut, Start, Location, lInvSty, " +;
*!*                  "       MarkA, MarkB, MarkC, cConsInfo1, cConsInfo2" +;
*!*                  "  FROM Item WITH (INDEX = STYLE)" +;
*!*                  "  WHERE " + lcWhereCond
*!*      
*!*    *--Run the Satatement
*!*    lnConnectionHandler = oAriaApplication.RemoteCompanyData.SQLRun(lcStatement, lcTmpItem, "ITEM",;
*!*                                              oAriaApplication.ActiveCompanyConStr,;
*!*                                              3, "SAVE", SET("Datasession"))

*!*    *--Check Connection Result
*!*    IF lnConnectionHandler # 1
*!*      =oAriaApplication.RemoteCompanyData.CheckRetResult("SQLRUN", lnConnectionHandler, .T.)
*!*      RETURN ""
*!*    ENDIF

*!*    LOCAL lnOldBuffMode
*!*    
*!*    lnOldBuffMode = CURSORGETPROP("Buffering", lcTmpItem)
*!*    =CURSORSETPROP("Buffering", 3, lcTmpItem)

*!*    SELECT (lcTmpItem)
*!*    *E038245,1 WSH [End]

*!*    IF lcMajorOrNon = 'M'

*!*      *E038245,1 WSH Enhance the way of openning SQL Item file [Start]
*!*      *DIMENSION laIndex[2,3]
*!*      *laIndex = " "
*!*      *laIndex[1,1] = "Style"
*!*      *laIndex[1,2] = "Style"
*!*      *laIndex[2,1] = "cStyMajor"
*!*      *laIndex[2,2] = "CStyle"
*!*      *laIndex[2,3] = "Unique"
*!*      
*!*      *N119680,1 AMH Use the specific expression [Start]
*!*      *=gfOpnSqlFl('ITEM',lcTmpItem,"CINVTYPE = '" + lcInvtype + "'",@laIndex,"Style")
*!*      *=gfOpnSqlFl('ITEM',lcTmpItem,lcWhereCond,@laIndex,"Style")
*!*      *N119680,1 AMH [End]
*!*      INDEX ON Style TAG Style
*!*      INDEX ON cStyMajor TAG cStyle UNIQUE
*!*      *E038245,1 WSH [End]

*!*    ELSE

*!*      *E038245,1 WSH Enhance the way of openning SQL Item file [Start]
*!*      *DIMENSION laIndex[1,3]
*!*      *laIndex =" "
*!*      *laIndex[1,1] = "Style"
*!*      *laIndex[1,2] = "Style"
*!*      
*!*      *N119680,1 AMH Use the specific expression [Start]
*!*      *=gfOpnSqlFl('ITEM',lcTmpItem,"CINVTYPE = '" + lcInvtype + "'",@laIndex,"Style")
*!*      *=gfOpnSqlFl('ITEM',lcTmpItem,lcWhereCond,@laIndex,"Style")
*!*      *N119680,1 AMH [End]
*!*      INDEX ON Style TAG Style
*!*      *E038245,1 WSH [End]
*!*      
*!*    ENDIF

*!*    *E038245,1 WSH Open ItemLoc file to Get Quantity Fields - No Need to Open Item file again [Start]
*!*    =CURSORSETPROP("Buffering", lnOldBuffMode, lcTmpItem)
*!*    
*!*    *-- Open the item file in another alias to calculate some fields.  

*!*    *lcTmpItem2 = gfTempName()

*!*    *DIMENSION laIndex2[1,3]
*!*    *laIndex2 = " "
*!*    *laIndex2[1,1] = "Style"
*!*    *laIndex2[1,2] = "Style"
*!*    
*!*    *N119680,1 AMH Use the specific expression [Start]
*!*    *=gfOpnSqlFl('ITEM',lcTmpItem2,"CINVTYPE = '" + lcInvtype + "'",@laIndex,"Style")
*!*    *=gfOpnSqlFl('ITEM',lcTmpItem2,lcWhereCond,@laIndex,"Style")
*!*    *N119680,1 AMH [End]

*!*    lcTmpItemLoc = gfTempName()

*!*    lcWhereCond = "cInvType = '" + lcInvtype + "' AND Dyelot = ''"
*!*    
*!*    lcStatement = "SELECT  " + IIF(lcMajorOrNon == 'M', "LEFT(Style, " + STR(lnMajLen) + ") AS ", "") + "Style, " +;
*!*                  "        SUM(totord) AS totord, SUM(totstk) AS totstk, SUM(totwip) AS totwip, SUM(nStkVal) AS nStkVal" +;
*!*                  "  FROM  ItemLoc WITH (INDEX = STYDYE)" +;
*!*                  "  WHERE " + lcWhereCond +;
*!*                  "  GROUP BY " + IIF(lcMajorOrNon == 'M', "LEFT(Style," + STR(lnMajLen) + ")", "Style")

*!*    *--Run the Satatement
*!*    lnConnectionHandler = oAriaApplication.RemoteCompanyData.SQLRun(lcStatement, lcTmpItemLoc, "ITEMLOC",;
*!*                                              oAriaApplication.ActiveCompanyConStr,;
*!*                                              3, "SAVE", SET("Datasession"))

*!*    *--Check Connection Result
*!*    IF lnConnectionHandler # 1
*!*      =oAriaApplication.RemoteCompanyData.CheckRetResult("SQLRUN", lnConnectionHandler, .T.)
*!*      RETURN ""
*!*    ENDIF
*!*    
*!*    SELECT (lcTmpItemLoc)
*!*    lnOldBuffMode = CURSORGETPROP("Buffering", lcTmpItemLoc)
*!*    =CURSORSETPROP("Buffering", 3, lcTmpItemLoc)
*!*    INDEX ON Style TAG Style
*!*    =CURSORSETPROP("Buffering", lnOldBuffMode, lcTmpItemLoc)
*!*    SET ORDER TO Style
*!*    *E038245,1 WSH [End]

*!*  ENDIF

*!*  *-- Include the .H file
*!*  #INCLUDE R:\ARIA4XP\PRGS\SY\STYBROW.H

*!*  *E038245,1 WSH No need for this condition as Item file is opened only once [Start]
*!*  *IF lcMajorOrNon $ 'NI'
*!*  *E038245,1 WSH [End]

*!*      *E038245,1 WSH Get Quantities from ItemLoc Table not from Item Table [Start]
*!*      *lcBrFields = [STYLE :30 :H=ALLTRIM(lcClrTtl),DESC :20 :H="]+LANG_DESC+[",DESC1 :35 :H="]+LANG_LDESC+[",]+;
*!*                   [lcSesDesc=gfCodDes(Season,'SEASON'):20 :H="]+LANG_SEASON+[",lcDivDesc=gfCodDes(cdivision,'CDIVISION') :20:H="]+LANG_DIV+[",]

*!*      *E124065,1 AMH Add color name to style and material browse [Start]
*!*      *lcBrFields = IIF(lcMajorOrNon $ 'NI', [STYLE :30 :H=ALLTRIM(lcClrTtl)],[cStyMajor :30 :H=ALLTRIM(lcStyTtl)]) + [,DESC :20 :H="]+LANG_DESC+[",DESC1 :35 :H="]+LANG_LDESC+[",]+;
*!*                   [lcSesDesc=gfCodDes(Season,'SEASON'):20 :H="]+LANG_SEASON+[",lcDivDesc=gfCodDes(cdivision,'CDIVISION') :20:H="]+LANG_DIV+[",]
*!*      lcBrFields = IIF(lcMajorOrNon $ 'NI', [STYLE :30 :H=ALLTRIM(lcClrTtl)]+;
*!*                   [,lcColor=gfCodDes(SUBSTR(STYLE,lnClrStrt,lnClrWidth),'COLOR'):20 :H="]+LANG_COLOR+["],;
*!*                   [cStyMajor :30 :H=ALLTRIM(lcStyTtl)]) +;
*!*                   [,DESC :20 :H="]+LANG_DESC+[",DESC1 :35 :H="]+LANG_LDESC+[",]+;
*!*                   [lcSesDesc=gfCodDes(Season,'SEASON'):20 :H="]+LANG_SEASON+;
*!*                   [",lcDivDesc=gfCodDes(cdivision,'CDIVISION') :20:H="]+LANG_DIV+[",]
*!*      *E124065,1 AMH [End]

*!*      *E038245,1 WSH [End]

*!*      lcBrFields = lcBrFields+;
*!*                   [pricea :10:H="]+LANG_PRICEA+[" , PRICEB :10:H="]+LANG_PRICEB+[",PRICEC :10:H="]+LANG_PRICEC+[",]
*!*      
*!*      *E038245,1 WSH Get Quantities from ItemLoc Table not from Item Table [Start]
*!*      *lcBrFields = lcBrFields+;
*!*                   [totWip:12:h="]+LANG_WIP+[",totstk:12:h="]+LANG_STOCK+[",totord:12:h="]+LANG_ORDER+[",]

*!*      lcBrFields = lcBrFields+;
*!*                   [&lcTmpItemLoc..TotWip: 12: H="]+LANG_WIP+[",]+;
*!*                   [&lcTmpItemLoc..TotStk: 12: H="]+LANG_STOCK+[",]+;
*!*                   [&lcTmpItemLoc..TotOrd: 12: H="]+LANG_ORDER+[",]

*!*      *lcBrFields = lcBrFields+;
*!*                   [OTS=(TOTWIP+TOTSTK-TOTORD):12:H="]+LANG_OTS+[",Fabric:15:h="]+LANG_FABRIC+[",]

*!*      lcBrFields = lcBrFields+;
*!*                   [OTS=&lcTmpItemLoc..totWip+&lcTmpItemLoc..totStk-&lcTmpItemLoc..totOrd: 12: H="]+LANG_OTS+[",Fabric:15:h="]+LANG_FABRIC+[",]
*!*      *E038245,1 WSH [End]

*!*      lcBrFields = lcBrFields+;
*!*                   [CSTYGRADE :H="]+LANG_GRADE+[", lcRoyDesc=gfCodDes(ROYALTY,'ROYALTY') :20 :H="]+LANG_ROYAL+[" , PATTERN :H="]+LANG_PATRN+[", STATUS :H="]+LANG_STATUS+[",]

*!*      lcBrFields = lcBrFields + ;
*!*                   [SCALE :H="]+LANG_SCALE+[", PREPAK :H="]+LANG_PREPAK+[", CBUYPREPK :H="]+LANG_BPREPK+[", QTY_CTN :H="]+LANG_QTYCRT+[", COMMISSION :H="]+LANG_COMM+[", LINK_CODE :H="]+LANG_LNKCD+[",]+;
*!*                   [lcMaked = IIF(MAKE,'Y','N') :H="]+LANG_MAKE+[", NMCOST1 :H="]+LANG_MCST1+[" , NMCOST2 :H="]+LANG_MCST2+[", NMCOST3 :H="]+LANG_MCST3+[", NMCOST4 :H="]+LANG_MCST4+[",NMCOST5 :H="]+LANG_MCST5+[",]

*!*      lcBrFields = lcBrFields + ;
*!*                   [NICOST1 :H="]+LANG_ICST1+[", NICOST2 :H="]+LANG_ICST2+[", NICOST3 :H="]+LANG_ICST3+[", NICOST4 :H="]+LANG_ICST4+[", NICOST5 :H="]+LANG_ICST5+[",]

*!*      *E038245,1 WSH Calculate Average Cost from ItemLoc file not from Item file [Start]
*!*      *lcBrFields = lcBrFields+;
*!*                   [NPRCOST2 :H="]+LANG_PCST2+[",NPRCOST3 :H="]+LANG_PCST3+[",NPRCOST4 :H="]+LANG_PCST4+[",NPRCOST5 :H="]+LANG_PCST5+[",TOTCOST :H="]+LANG_TOTCST+[",]+;
*!*                   [AVE_COST :H="]+LANG_AVECST+[",NSTKVAL :H="]+LANG_STKVAL+[",SOLDOUT :H="]+LANG_SOLDDT+[",START :H="]+LANG_STRTDT+[",LOCATION :H="]+LANG_DBIN+[",LINVSTY :H="]+LANG_INVSTY+[",]
*!*      lcBrFields = lcBrFields+;
*!*                   [NPRCOST2 :H="]+LANG_PCST2+[",NPRCOST3 :H="]+LANG_PCST3+[",NPRCOST4 :H="]+LANG_PCST4+[",NPRCOST5 :H="]+LANG_PCST5+[",TOTCOST :H="]+LANG_TOTCST+[",]+;
*!*                   [lnAvegCost=gfCalAvCst() :H="]+LANG_AVECST+IIF(lcMajorOrNon $ 'NI', "", [",NSTKVAL :H="]+LANG_STKVAL)+[",SOLDOUT :H="]+LANG_SOLDDT+[",START :H="]+LANG_STRTDT+[",LOCATION :H="]+LANG_DBIN+[",LINVSTY :H="]+LANG_INVSTY+[",]
*!*      *E038245,1 WSH [End]

*!*      lcBrFields = lcBrFields+;
*!*                   [MARKA :H="]+LANG_MARKA+[",MARKB:H="]+LANG_MARKB+[",MARKC :H="]+LANG_MARKC+[",]+;
*!*                   [CCONSINFO1 :H="]+LANG_CONSI1+[",CCONSINFO2 :H="]+LANG_CONSI2+["]

*!*  *E038245,1 WSH No need for this condition as Item file is opened only once [Start]

*!*  *!*  ELSE
*!*  *!*      
*!*  *!*      lcBrFields = [CSTYMAJOR:30:H=ALLTRIM(lcStyTtl),&lcTmpItem2..DESC:20:H="]+LANG_DESC+[",&lcTmpItem2..DESC1:35:H="]+LANG_LDESC+[",]+;
*!*  *!*                   [lcSesDesc=gfCodDes(&lcTmpItem2..Season,'SEASON'):20:H="]+LANG_SEASON+[",lcDivDesc=gfCodDes(&lcTmpItem2..cdivision,'CDIVISION'):20:H="]+LANG_DIV+[",]

*!*  *!*      lcBrFields = lcBrFields+;
*!*  *!*                   [&lcTmpItem2..pricea:10:h="]+LANG_PRICEA+[",&lcTmpItem2..PRICEB:10:h="]+LANG_PRICEB+[",&lcTmpItem2..PRICEC:10:h="]+LANG_PRICEC+[",]
*!*  *!*      
*!*  *!*      lcBrFields = lcBrFields+;
*!*  *!*                   [lnTotWIP=gfSumItem(PADR(ALLTRIM(&lcTmpItem2..CSTYMAJOR),lnMajLen),'TotWip',lcTmpItem2):12:h="]+LANG_WIP+[",]+;
*!*  *!*                   [lnTotStk=gfSumItem(PADR(ALLTRIM(&lcTmpItem2..CSTYMAJOR),lnMajLen),'totstk',lcTmpItem2):12:h="]+LANG_STOCK+[",]+;
*!*  *!*                   [lnTotORD=gfSumItem(PADR(ALLTRIM(&lcTmpItem2..CSTYMAJOR),lnMajLen),'totord',lcTmpItem2):12:h="]+LANG_ORDER+[",]+;
*!*  *!*                   [lnOts   =gfSumItem(PADR(ALLTRIM(&lcTmpItem2..CSTYMAJOR),lnMajLen),'totWip+totStk-totOrd',lcTmpItem2):12:H="]+LANG_OTS+[",]

*!*  *!*      lcBrFields = lcBrFields+;
*!*  *!*                   [&lcTmpItem2..Fabric:12:h="]+LANG_FABRIC+[",&lcTmpItem2..CSTYGRADE:H="]+LANG_GRADE+[", lcRoyDesc=gfCodDes(&lcTmpItem2..ROYALTY,'ROYALTY') :15 :H="]+LANG_ROYAL+[" , &lcTmpItem2..PATTERN :H="]+LANG_PATRN+[", &lcTmpItem2..STATUS :H="]+LANG_STATUS+[",]

*!*  *!*      lcBrFields = lcBrFields + ;
*!*  *!*                   [&lcTmpItem2..SCALE :H="]+LANG_SCALE+[", &lcTmpItem2..PREPAK :H="]+LANG_PREPAK+[", &lcTmpItem2..CBUYPREPK :H="]+LANG_BPREPK+[", &lcTmpItem2..QTY_CTN :H="]+LANG_QTYCRT+[", &lcTmpItem2..COMMISSION :H="]+LANG_COMM+[", &lcTmpItem2..LINK_CODE :H="]+LANG_LNKCD+[",]+;
*!*  *!*                   [lcMaked = IIF(&lcTmpItem2..MAKE,'Y','N') :H="]+LANG_MAKE+[", &lcTmpItem2..NMCOST1 :H="]+LANG_MCST1+[" , &lcTmpItem2..NMCOST2 :H="]+LANG_MCST2+[", &lcTmpItem2..NMCOST3 :H="]+LANG_MCST3+[", &lcTmpItem2..NMCOST4 :H="]+LANG_MCST4+[",&lcTmpItem2..NMCOST5 :H="]+LANG_MCST5+[",]

*!*  *!*      lcBrFields = lcBrFields + ;
*!*  *!*                   [&lcTmpItem2..NICOST1 :H="]+LANG_ICST1+[", &lcTmpItem2..NICOST2 :H="]+LANG_ICST2+[", &lcTmpItem2..NICOST3 :H="]+LANG_ICST3+[", &lcTmpItem2..NICOST4 :H="]+LANG_ICST4+[", &lcTmpItem2..NICOST5 :H="]+LANG_ICST5+[",]

*!*  *!*      lcBrFields = lcBrFields+;
*!*  *!*                   [&lcTmpItem2..NPRCOST2 :H="]+LANG_PCST2+[",&lcTmpItem2..NPRCOST3 :H="]+LANG_PCST3+[",&lcTmpItem2..NPRCOST4 :H="]+LANG_PCST4+[",&lcTmpItem2..NPRCOST5 :H="]+LANG_PCST5+[",&lcTmpItem2..TOTCOST :H="]+LANG_TOTCST+[",]+;
*!*  *!*                   [lnAvegCost=gfCalAvCst() :H="]+LANG_AVECST+[",&lcTmpItem2..SOLDOUT :H="]+LANG_SOLDDT+[",&lcTmpItem2..START :H="]+LANG_STRTDT+[",&lcTmpItem2..LOCATION :H="]+LANG_DBIN+[",&lcTmpItem2..LINVSTY :H="]+LANG_INVSTY+[",]

*!*  *!*      lcBrFields = lcBrFields+;
*!*  *!*                   [&lcTmpItem2..MARKA :H="]+LANG_MARKA+[",&lcTmpItem2..MARKB:H="]+LANG_MARKB+[",&lcTmpItem2..MARKC :H="]+LANG_MARKC+[",]+;
*!*  *!*                   [&lcTmpItem2..CCONSINFO1 :H="]+LANG_CONSI1+[",&lcTmpItem2..CCONSINFO2 :H="]+LANG_CONSI2+["]

*!*  *!*  ENDIF

*!*  *E038245,1 WSH [End]

*!*  lnCurAlias = SELECT()
*!*  SELECT (lcTmpItem)
*!*  lcStyOrder = TAG()
*!*  *SET ORDER TO TAG CSTYLE

*!*  DO CASE
*!*    CASE '*' $ lcColor AND '*' $ lcSeason

*!*      *E038245,1 WSH Get Quantities from ItemLoc Table not from Item Table [Start]
*!*      *lcBrFields = [STYLE :30 :H=ALLTRIM(lcClrTtl),DESC :45:H="]+LANG_DESC+[",]+;
*!*                   [pricea :H="]+LANG_PRICEA+[",totWip :14 :H="]+LANG_WIP+[",totstk :14 :H="]+LANG_STOCK+[",]+;
*!*                   [totord :14 :H="]+LANG_ORDER+[",Fabric :19 :H="]+LANG_FABRIC+["]
*!*      lcBrFields = [STYLE :30 :H=ALLTRIM(lcClrTtl),DESC :45:H="]+LANG_DESC+[",]+;
*!*                   [pricea :H="]+LANG_PRICEA+[",&lcTmpItemLoc..TotWip :14 :H="]+LANG_WIP+[",&lcTmpItemLoc..TotStk :14 :H="]+LANG_STOCK+[",]+;
*!*                   [&lcTmpItemLoc..TotOrd :14 :H="]+LANG_ORDER+[",Fabric :19 :H="]+LANG_FABRIC+["]
*!*      *E038245,1 WSH [End]

*!*    CASE '*' $ lcColor

*!*      *E038245,1 WSH Get Quantities from ItemLoc Table not from Item Table [Start]
*!*      *lcBrFields = [STYLE :30 :H=ALLTRIM(lcClrTtl),DESC :45:H="]+LANG_DESC+[",]+;
*!*                   [pricea :H="]+LANG_PRICEA+[",totWip :14 :H="]+LANG_WIP+[",totstk :14 :H="]+LANG_STOCK+[",]+;
*!*                   [totord :14 :H="]+LANG_ORDER+[",Fabric :19 :H="]+LANG_FABRIC+["]
*!*      lcBrFields = [STYLE :30 :H=ALLTRIM(lcClrTtl),DESC :45:H="]+LANG_DESC+[",]+;
*!*                   [pricea :H="]+LANG_PRICEA+[",&lcTmpItemLoc..TotWip :14 :H="]+LANG_WIP+[",&lcTmpItemLoc..TotStk :14 :H="]+LANG_STOCK+[",]+;
*!*                   [&lcTmpItemLoc..TotOrd :14 :H="]+LANG_ORDER+[",Fabric :19 :H="]+LANG_FABRIC+["]
*!*      *E038245,1 WSH [End]

*!*      lcScope = [=gfvSeasonC]
*!*      
*!*    OTHERWISE  
*!*      IF !('*' $ lcSeason)
*!*        lcScope = [=gfvSeasonC]
*!*      ENDIF  
*!*  ENDCASE

*!*  SELECT (lcTmpItem)

*!*  *E038245,1 WSH Set relation between Item and ItemLoc files [Start]

*!*  IF lcMajorOrNon = 'M'
*!*    SET ORDER TO TAG CSTYLE
*!*    SET RELATION TO LEFT(cStyMajor, lnMajLen) INTO (lcTmpItemLoc) ADDITIVE
*!*  ELSE
*!*    SET ORDER TO TAG STYLE
*!*    SET RELATION TO Style INTO (lcTmpItemLoc) ADDITIVE
*!*  ENDIF

*!*  *SET RELATION TO PADR(&lcTmpItem..CSTYMAJOR,lnMajLen) INTO (lcTmpItem2) ADDITIVE

*!*  *E038245,1 WSH [End]

*!*  *E121005,1 WSH Remove the '?' from the "lcItem" to locaate right [Start]
*!*  LOCAL lcBrowChr
*!*  lcItem    = RTRIM(lcItem)
*!*  lcBrowChr = RIGHT(lcItem, 1)
*!*  lcItem    = IIF(lcBrowChr == '?', SUBSTR(lcItem, 1, LEN(lcItem) - 1) , lcItem)
*!*  *E121005,1 WSH [End]

*!*  IF lcMajorOrNon = 'M'
*!*    lcStyle = [lcItem]
*!*  ELSE
*!*    IF lcMajorOrNon $ 'N'
*!*      SELECT (lcTmpItem)
*!*      
*!*      *N119680,1 AMH Use the correct Expration [Start]
*!*      *lcStyle = [lcItem]
*!*      lcStyle = "'"+PADR(lcItem,lnMajLen)+"'"
*!*      *N119680,1 AMH [End]
*!*      
*!*    ENDIF
*!*  ENDIF

*!*  IF !SEEK(lcItem)
*!*   lnSoftSeek = IIF(SEEK(RTRIM(lcItem),lcTmpItem),RECNO(),RECNO(0))
*!*   IF  lnSoftSeek > 0 
*!*     GO lnSoftSeek
*!*   ELSE
*!*     GO TOP
*!*   ENDIF
*!*  ENDIF  

*!*  IF TYPE('lcStyle')='C'
*!*    IF lcMajorOrNon = 'N'
*!*      *E038245,1 WSH Remove Second Item Cursor and use the main one [Start]
*!*      *llWasSel=ARIABROW(lcStyle,lcTitle,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,lcScope,"","STYLE,&lcTmpItem2..SEASON","laData")
*!*      llWasSel=ARIABROW(lcStyle,lcTitle,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,lcScope,"","STYLE,SEASON","laData")
*!*      *E038245,1 WSH [Start]
*!*    ELSE
*!*      *E038245,1 WSH Remove Second Item Cursor and use the main one [Start]
*!*      *llWasSel=ARIABROW('',lcTitle,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,lcScope,"","STYLE,&lcTmpItem2..SEASON","laData")
*!*      llWasSel=ARIABROW('',lcTitle,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,lcScope,"","STYLE,SEASON","laData")
*!*      *E038245,1 WSH [Start]
*!*    ENDIF
*!*  ELSE
*!*    *E038245,1 WSH Remove Second Item Cursor and use the main one [Start]
*!*    *llWasSel=ARIABROW('',lcTitle,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,lcScope,"","STYLE,&lcTmpItem2..SEASON","laData")
*!*    llWasSel=ARIABROW('',lcTitle,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,lcScope,"","STYLE,SEASON","laData")
*!*    *E038245,1 WSH [Start]
*!*  ENDIF  

*!*  *E038245,1 WSH Remove Second Item Cursor as it is not needed and clear ItemLoc relation [Start]
*!*  *IF lcMajorOrNon = 'M'
*!*    *SET SKIP TO

*!*    *SET RELATION OFF INTO (lcTmpItem2)
*!*  *ENDIF

*!*  SET RELATION OFF INTO (lcTmpItemLoc)
*!*  *E038245,1 WSH [Start]

*!*  IF llWasSel
*!*    lcItem  = laData[1]
*!*    lcColor = laData[2]
*!*  ELSE
*!*    lcItem  = SPACE(12)
*!*    lcColor = SPACE(6)
*!*  ENDIF  

*B128949,1 WSH 08/07/2005 Read Total Quantity Fields from Item File. [Start]
*IF TYPE("lcTmpItem") $ "UL" OR !USED(lcTmpItem) OR TYPE("lcTmpItemLoc") $ "UL" OR !USED(lcTmpItemLoc)
IF TYPE("lcTmpItem") $ "UL" OR !USED(lcTmpItem) OR TYPE("lcTmpItem2") $ "UL" OR !USED(lcTmpItem2)
*B128949,1 WSH 08/07/2005 [End]

  lcTmpItem = gfTempName()
  
  *--Create Temp Item Cursor
  lnConnectionHandler = oAriaApplication.RemoteCompanyData.SQLRun("SELECT TOP 0 * FROM ITEM (INDEX = STYLE)", lcTmpItem, "ITEM",;
                                            oAriaApplication.ActiveCompanyConStr,;
                                            3, "SAVE", SET("Datasession"))

  *--Check Connection Result
  IF lnConnectionHandler # 1
    =oAriaApplication.RemoteCompanyData.CheckRetResult("SQLRUN", lnConnectionHandler, .T.)
    RETURN ""
  ENDIF

  *--Open Temp ItemLoc Cursor to Get needed Quantity Fields
  *B128949,1 WSH 08/07/2005 Read Total Quantity Fields from Item File. [Start]
  *SELECT (lcTmpItem)
  *lnOldBuffMode = CURSORGETPROP("Buffering", lcTmpItem)
  *=CURSORSETPROP("Buffering", 3, lcTmpItem)
  *INDEX ON cInvType + Style TAG Style
  *=CURSORSETPROP("Buffering", lnOldBuffMode, lcTmpItem)
  *SET ORDER TO Style
  **--Open Temp ItemLoc Cursor to Get needed Quantity Fields
  *lcTmpItemLoc = gfTempName()
  *lcWhereCond  = "cInvType = '" + lcInvtype + "' AND Dyelot = ''"
  *
  *lcStatement  = "SELECT  Style, " +;
  *               "        totord, totstk, totwip, nStkVal" +;
  *               "  FROM  ItemLoc WITH (INDEX = STYDYE)" +;
  *               "  WHERE " + lcWhereCond
  *
  **--Run the Satatement
  *lnConnectionHandler = oAriaApplication.RemoteCompanyData.SQLRun(lcStatement, lcTmpItemLoc, "ITEMLOC",;
  *                                          oAriaApplication.ActiveCompanyConStr,;
  *                                          3, "SAVE", SET("Datasession"))
  *
  **--Check Connection Result
  *IF lnConnectionHandler # 1
  *  =oAriaApplication.RemoteCompanyData.CheckRetResult("SQLRUN", lnConnectionHandler, .T.)
  *  RETURN ""
  *ENDIF
  *
  *SELECT (lcTmpItemLoc)
  *lnOldBuffMode = CURSORGETPROP("Buffering", lcTmpItemLoc)
  *=CURSORSETPROP("Buffering", 3, lcTmpItemLoc)
  *INDEX ON Style TAG Style
  *=CURSORSETPROP("Buffering", lnOldBuffMode, lcTmpItemLoc)
  *SET ORDER TO Style
  IF lcMajorOrNon = 'M'
    LOCAL lcStat
    lcTmpItem2 = gfTempName()
    
    lcStat = "SELECT cStyMajor, SUM(TotStk) AS TotStk, SUM(TotOrd) AS TotOrd, SUM(TotWip) AS TotWip, SUM(nStkVal) AS nStkVal" +;
             "  FROM ITEM (INDEX=CSTYLE) " +;
             " WHERE cInvType = '" + lcInvtype + "'" +;
             " GROUP BY cStyMajor" +;
             " ORDER By cStyMajor"
    
    *--Create Temp Item Cursor
    lnConnectionHandler = oAriaApplication.RemoteCompanyData.SQLRun(lcStat, lcTmpItem2, "ITEM",;
                                              oAriaApplication.ActiveCompanyConStr,;
                                              3, "SAVE", SET("Datasession"))
    
    *--Check Connection Result
    IF lnConnectionHandler # 1
      =oAriaApplication.RemoteCompanyData.CheckRetResult("SQLRUN", lnConnectionHandler, .T.)
      RETURN ""
    ENDIF
    
    lnOldBuffMode = CURSORGETPROP("Buffering", lcTmpItem2)
    =CURSORSETPROP("Buffering", 3, lcTmpItem2)
    INDEX ON cStyMajor TAG Style
    =CURSORSETPROP("Buffering", lnOldBuffMode, lcTmpItem2)
    SET ORDER TO Style
  ENDIF
  *B128949,1 WSH 08/07/2005 [End]
ENDIF

*-- Include the .H file
#INCLUDE R:\ARIA4XP\PRGS\SY\STYBROW.H

PRIVATE lcToDo, llWasSel, lcBrFields
llWasSel = .F.
lcBrFields = ''
  
lcBrFields = IIF(lcMajorOrNon $ 'NI', [STYLE :30 :H=ALLTRIM(lcClrTtl)]+;
             [,lcColor=gfCodDes(SUBSTR(STYLE,lnClrStrt,lnClrWidth),'COLOR'):20 :H="]+LANG_COLOR+["],;
             [cStyMajor :30 :H=ALLTRIM(lcStyTtl)]) +;
             [,DESC :20 :H="]+LANG_DESC+[",DESC1 :35 :H="]+LANG_LDESC+[",]+;
             [lcSesDesc=gfCodDes(Season,'SEASON'):20 :H="]+LANG_SEASON+;
             [",lcDivDesc=gfCodDes(cdivision,'CDIVISION') :20:H="]+LANG_DIV+[",]

lcBrFields = lcBrFields+;
             [pricea :10:H="]+LANG_PRICEA+[" , PRICEB :10:H="]+LANG_PRICEB+[",PRICEC :10:H="]+LANG_PRICEC+[",]

IF lcMajorOrNon = 'M'

  *B128949,1 WSH 08/07/2005 Read Total Quantity Fields from Item File. [Start]
  *lcBrFields = lcBrFields+;
               [lnTotWIP=gfSumItem(PADR(ALLTRIM(CSTYMAJOR),lnMajLen),'TotWip',lcTmpItemLoc):12:h="]+LANG_WIP+[",]+;
               [lnTotStk=gfSumItem(PADR(ALLTRIM(CSTYMAJOR),lnMajLen),'totstk',lcTmpItemLoc):12:h="]+LANG_STOCK+[",]+;
               [lnTotORD=gfSumItem(PADR(ALLTRIM(CSTYMAJOR),lnMajLen),'totord',lcTmpItemLoc):12:h="]+LANG_ORDER+[",]+;
               [lnOts   =gfSumItem(PADR(ALLTRIM(CSTYMAJOR),lnMajLen),'totWip+totStk-totOrd',lcTmpItemLoc):12:H="]+LANG_OTS+[",]
  lcBrFields = lcBrFields+;
               [lnTotWIP=gfSumItem(CSTYMAJOR,'TotWip',lcTmpItem2):12:h="]+LANG_WIP+[",]+;
               [lnTotStk=gfSumItem(CSTYMAJOR,'totstk',lcTmpItem2):12:h="]+LANG_STOCK+[",]+;
               [lnTotORD=gfSumItem(CSTYMAJOR,'totord',lcTmpItem2):12:h="]+LANG_ORDER+[",]+;
               [lnOts   =gfSumItem(CSTYMAJOR,'totWip+totStk-totOrd',lcTmpItem2):12:H="]+LANG_OTS+[",]
  *B128949,1 WSH 08/07/2005 [End]

ELSE

  *B128949,1 WSH 08/07/2005 Read Total Quantity Fields from Item File. [Start]
  *lcBrFields = lcBrFields+;
               [lnTotWIP=gfSumItem(Style,'TotWip',lcTmpItemLoc):12:h="]+LANG_WIP+[",]+;
               [lnTotStk=gfSumItem(Style,'totstk',lcTmpItemLoc):12:h="]+LANG_STOCK+[",]+;
               [lnTotORD=gfSumItem(Style,'totord',lcTmpItemLoc):12:h="]+LANG_ORDER+[",]+;
               [lnOts   =gfSumItem(Style,'totWip+totStk-totOrd',lcTmpItemLoc):12:H="]+LANG_OTS+[",]
  lcBrFields = lcBrFields+;
               [TotWip:12:h="]+LANG_WIP+[",]+;
               [totstk:12:h="]+LANG_STOCK+[",]+;
               [totord:12:h="]+LANG_ORDER+[",]+;
               [lnOts = totWip+totStk-totOrd :12:H="]+LANG_OTS+[",]
  *B128949,1 WSH 08/07/2005 [End]

ENDIF

lcBrFields = lcBrFields+;
             [CSTYGRADE :H="]+LANG_GRADE+[", lcRoyDesc=gfCodDes(ROYALTY,'ROYALTY') :20 :H="]+LANG_ROYAL+[" , PATTERN :H="]+LANG_PATRN+[", STATUS :H="]+LANG_STATUS+[",]

lcBrFields = lcBrFields + ;
             [SCALE :H="]+LANG_SCALE+[", PREPAK :H="]+LANG_PREPAK+[", CBUYPREPK :H="]+LANG_BPREPK+[", QTY_CTN :H="]+LANG_QTYCRT+[", COMMISSION :H="]+LANG_COMM+[", LINK_CODE :H="]+LANG_LNKCD+[",]+;
             [lcMaked = IIF(MAKE,'Y','N') :H="]+LANG_MAKE+[", NMCOST1 :H="]+LANG_MCST1+[" , NMCOST2 :H="]+LANG_MCST2+[", NMCOST3 :H="]+LANG_MCST3+[", NMCOST4 :H="]+LANG_MCST4+[",NMCOST5 :H="]+LANG_MCST5+[",]

lcBrFields = lcBrFields + ;
             [NICOST1 :H="]+LANG_ICST1+[", NICOST2 :H="]+LANG_ICST2+[", NICOST3 :H="]+LANG_ICST3+[", NICOST4 :H="]+LANG_ICST4+[", NICOST5 :H="]+LANG_ICST5+[",]

*B128949,1 WSH 08/07/2005 Read Total Quantity Fields from Item File. [Start]
*lcBrFields = lcBrFields+;
             [NPRCOST2 :H="]+LANG_PCST2+[",NPRCOST3 :H="]+LANG_PCST3+[",NPRCOST4 :H="]+LANG_PCST4+[",NPRCOST5 :H="]+LANG_PCST5+[",TOTCOST :H="]+LANG_TOTCST+[",]+;
             [lnAvegCost=gfCalAvCst() :H="]+LANG_AVECST+IIF(lcMajorOrNon $ 'NI', "", [",NSTKVAL :H="]+LANG_STKVAL)+[",SOLDOUT :H="]+LANG_SOLDDT+[",START :H="]+LANG_STRTDT+[",LOCATION :H="]+LANG_DBIN+[",LINVSTY :H="]+LANG_INVSTY+[",]
lcBrFields = lcBrFields+;
             [NPRCOST2 :H="]+LANG_PCST2+[",NPRCOST3 :H="]+LANG_PCST3+[",NPRCOST4 :H="]+LANG_PCST4+[",NPRCOST5 :H="]+LANG_PCST5+[",TOTCOST :H="]+LANG_TOTCST+[",]+;
             [lnAvegCost=gfCalAvCst(']+lcMajorOrNon+['):H="]+LANG_AVECST+IIF(lcMajorOrNon $ 'NI', [",NSTKVAL], [",lnTotVal=gfSumItem(CSTYMAJOR,'NSTKVAL',lcTmpItem2)])+[ :H="]+LANG_STKVAL+[",SOLDOUT :H="]+LANG_SOLDDT+[",START :H="]+LANG_STRTDT+[",LOCATION :H="]+LANG_DBIN+[",LINVSTY :H="]+LANG_INVSTY+[",]
*B128949,1 WSH 08/07/2005 [End]

lcBrFields = lcBrFields+;
             [MARKA :H="]+LANG_MARKA+[",MARKB:H="]+LANG_MARKB+[",MARKC :H="]+LANG_MARKC+[",]+;
             [CCONSINFO1 :H="]+LANG_CONSI1+[",CCONSINFO2 :H="]+LANG_CONSI2+["]

lnCurAlias = SELECT()
SELECT (lcTmpItem)
lcStyOrder = TAG()

DO CASE
  CASE '*' $ lcColor AND '*' $ lcSeason

    *B128949,1 WSH 08/07/2005 Read Total Quantity Fields from Item File. [Start]
    *lcBrFields = [STYLE :30 :H=ALLTRIM(lcClrTtl),DESC :45:H="]+LANG_DESC+[",]+;
                 [pricea :H="]+LANG_PRICEA+[",]+;
                 [lnTotWIP=gfSumItem(Style,'TotWip',lcTmpItemLoc):12:h="]+LANG_WIP+[",]+;
                 [lnTotStk=gfSumItem(Style,'totstk',lcTmpItemLoc):12:h="]+LANG_STOCK+[",]+;
                 [lnTotORD=gfSumItem(Style,'totord',lcTmpItemLoc):12:h="]+LANG_ORDER+[",]+;
                 [lnOts   =gfSumItem(Style,'totWip+totStk-totOrd',lcTmpItemLoc):12:H="]+LANG_OTS+[",]+;
                 [Fabric :19 :H="]+LANG_FABRIC+["]
    lcBrFields = [STYLE :30 :H=ALLTRIM(lcClrTtl),DESC :45:H="]+LANG_DESC+[",]+;
                 [pricea :H="]+LANG_PRICEA+[",]+;
                 [TotWip:12:h="]+LANG_WIP+[",]+;
                 [totstk:12:h="]+LANG_STOCK+[",]+;
                 [totord:12:h="]+LANG_ORDER+[",]+;
                 [lnOts = totWip+totStk-totOrd :12:H="]+LANG_OTS+[",]+;
                 [Fabric :19 :H="]+LANG_FABRIC+["]
    *B128949,1 WSH 08/07/2005 [End]

  CASE '*' $ lcColor

    *B128949,1 WSH 08/07/2005 Read Total Quantity Fields from Item File. [Start]
    *lcBrFields = [STYLE :30 :H=ALLTRIM(lcClrTtl),DESC :45:H="]+LANG_DESC+[",]+;
                 [pricea :H="]+LANG_PRICEA+[",]+;
                 [lnTotWIP=gfSumItem(Style,'TotWip',lcTmpItemLoc):12:h="]+LANG_WIP+[",]+;
                 [lnTotStk=gfSumItem(Style,'totstk',lcTmpItemLoc):12:h="]+LANG_STOCK+[",]+;
                 [lnTotORD=gfSumItem(Style,'totord',lcTmpItemLoc):12:h="]+LANG_ORDER+[",]+;
                 [lnOts   =gfSumItem(Style,'totWip+totStk-totOrd',lcTmpItemLoc):12:H="]+LANG_OTS+[",]+;
                 [Fabric :19 :H="]+LANG_FABRIC+["]
    lcBrFields = [STYLE :30 :H=ALLTRIM(lcClrTtl),DESC :45:H="]+LANG_DESC+[",]+;
                 [pricea :H="]+LANG_PRICEA+[",]+;
                 [TotWip:12:h="]+LANG_WIP+[",]+;
                 [totstk:12:h="]+LANG_STOCK+[",]+;
                 [totord:12:h="]+LANG_ORDER+[",]+;
                 [lnOts = totWip+totStk-totOrd :12:H="]+LANG_OTS+[",]+;
                 [Fabric :19 :H="]+LANG_FABRIC+["]
    *B128949,1 WSH 08/07/2005 [End]
    
    *B128950,1 AMH Fix bug of variable not found when browse [Start]
    *lcScope = [=gfvSeasonC]
    lcScope = [=gfvSeasonC()]
    *B128950,1 AMH [End]
    
  OTHERWISE  
    IF !('*' $ lcSeason)
      
      *B128950,1 AMH Fix bug of variable not found when browse [Start]
      *lcScope = [=gfvSeasonC]
      lcScope = [=gfvSeasonC()]
      *B128950,1 AMH [End]
      
    ENDIF  
ENDCASE

SELECT (lcTmpItem)

LOCAL lcBrowChr
lcItem    = RTRIM(lcItem)
lcBrowChr = RIGHT(lcItem, 1)
lcItem    = IIF(lcBrowChr == '?', SUBSTR(lcItem, 1, LEN(lcItem) - 1) , lcItem)

*B039435,1 AMH Fix bug of there is quat in the item value [Start]
*lcForExp  = IIF(lcMajorOrNon = "N", "Style LIKE '" + PADR(lcItem, lnMajLen) + "%'" + IIF(EMPTY(lcForExp), "", " AND " + lcForExp), lcForExp)
PUBLIC lcItemMjr
lcItemMjr = PADR(lcItem, lnMajLen) + "%"
lcForExp  = IIF(lcMajorOrNon = "N", "Style LIKE ?m.lcItemMjr" + IIF(EMPTY(lcForExp), "", " AND " + lcForExp), lcForExp)
*B039435,1 AMH [End]

SELECT (lcTmpItem)

*! B128044,1 WSH 05/18/2005, Fix bug of displaying only one color while browsing Fabrics from Material PO. [Start]
*lcToDo = 'DO FORM BROWSE ' +;
         'WITH lcBrFields,"' + lcTitle + '",' +;
         '["' + PADR(ALLTRIM(lcInvtype), 4) + '"],"' + IIF(EMPTY(lcForExp), '', lcForExp) + '",' +;
         '.F., .T., .F., .F., .F., .F., .F., .F., .F., .F., .F., .F., .F.,"ITEM",' +;
         '"' + oAriaApplication.cSQLDBID + '",' +;
         '"STYLE", "' + IIF(lcMajorOrNon = "N", "STYLE", "CSTYLE") + '", "' + PADR(ALLTRIM(lcInvType),4) + RTRIM(lcItem) + '"' +;
         ' TO llWasSel'

*B039435,1 AMH Fix bug of there is quat in the item value [Start]
*lcToDo = 'DO FORM BROWSE ' +;
         'WITH lcBrFields,"' + lcTitle + '",' +;
         '["' + PADR(ALLTRIM(lcInvtype), 4) + '"],"' + IIF(EMPTY(lcForExp), '', lcForExp) + '",' +;
         '.F., .T., .F., .F., .F., .F., .F., .F., .F., .F., .F., .F., .F.,"ITEM",' +;
         '"' + oAriaApplication.cSQLDBID + '",' +;
         '"STYLE", "' + IIF(lcMajorOrNon $ "IN", "STYLE", "CSTYLE") + '", "' + PADR(ALLTRIM(lcInvType),4) + RTRIM(lcItem) + '"' +;
         ' TO llWasSel'
lcToDo = 'DO FORM BROWSE ' +;
         'WITH lcBrFields,"' + lcTitle + '",' +;
         '["' + PADR(ALLTRIM(lcInvtype), 4) + '"],"' + IIF(EMPTY(lcForExp), '', lcForExp) + '",' +;
         '.F., .T., .F., .F., .F., .F., .F., .F., .F., .F., .F., .F., .F.,"ITEM",' +;
         '"' + oAriaApplication.cSQLDBID + '",' +;
         '"STYLE", "' + IIF(lcMajorOrNon $ "IN", "STYLE", "CSTYLE") + '", [' + PADR(ALLTRIM(lcInvType),4) + RTRIM(lcItem) + ']' +;
         ' TO llWasSel'
*B039435,1 AMH [End]

*! B128044,1 WSH 05/18/2005, [End]


=gfExecute(lcToDo)

IF llWasSel
  lcItem  = EVALUATE(lcTmpItem + '.Style')
  lcColor = EVALUATE(lcTmpItem + '.Season')
ELSE
  lcItem  = SPACE(12)
  lcColor = SPACE(6)
ENDIF  
*!B039082,1 WSH, 02/23/2005, [End]

IF llRetAlias
  SELECT (lnCurAlias)
ENDIF  

RETURN lcItem

*!*************************************************************
*! Name : gfSumItem (037923)
*! Auth : Khalid Mohi El-Din Mohamed (KHM)
*! Date : 04/01/2004.
*!*************************************************************
*! Synopsis : To sum a specific field from a specific file
*!*************************************************************
*! Passed Parameters : 
*!      lcItem  -> Fabric or Style Code
*!      lcFieldNam -> Field name to be summed
*!        lcFile    -> Alias to be selected 
*!*************************************************************
*! Example :
*!        =gfSumItem("0001","STYLE     -BLACK",Season,lcTmpFile)
*!*************************************************************
FUNCTION gfSumItem
LPARAMETERS lcItem, lcFieldNam, lcFile

PRIVATE lnTotFld, lcAlias
lcAlias  = SELECT()
lnTotFld = 0

SELECT (lcFile)

*B039082,1 WSH 02/21/2005, Seek as there is no relation in gfItemBrow. [Sart]
=SEEK(lcItem)
*B039082,1 WSH 02/21/2005, [End]

*B128949,1 WSH 08/07/2005 Read Total Quantity Fields from Item File. [Start]
*SUM &lcFieldNam TO lnTotFld REST WHILE Style = lcItem
lnTotFld = EVALUATE(lcFile + '.' + lcFieldNam)
*B128949,1 WSH 08/07/2005 [End]


SELECT (lcAlias)

*B039082,1 WSH 02/21/2005, No need to locate. [Sart]
*GO RECNO()
*B039082,1 WSH 02/21/2005, [End]

RETURN INT(lnTotFld)

*!*************************************************************
*! Name : gfCalAvCst
*! Auth : Khalid Mohi El-Din Mohamed (KHM)
*! Date : 04/01/2004.
*!*************************************************************
*! Synopsis : To calculate the average cost for each style
*!*************************************************************
*! Example :
*!        =gfCalAvCst()
*!*************************************************************
FUNCTION gfCalAvCst

*B128949,1 WSH 08/07/2005 Read Total Quantity Fields from Item File. [Start]
LPARAMETERS lcMajorOrNon
*B128949,1 WSH 08/07/2005 [End]

PRIVATE lnTotStk, lnTotStVal , lnAvCstVal
STORE 0 TO lnTotStk , lnTotStk , lnAvCstVal

*E038245,1 WSH Remove Second Item Cursor as it is not needed and Calculate from ItemLoc File [Start]
*lnTotStk   = gfSumItem(PADR(ALLTRIM(&lcTmpItem2..CSTYMAJOR),lnMajLen),'TotStk',lcTmpItem2)
*lnTotStVal = gfSumItem(PADR(ALLTRIM(&lcTmpItem2..CSTYMAJOR),lnMajLen),'NSTKVAL',lcTmpItem2)

*B128949,1 WSH 08/07/2005 Read Total Quantity Fields from Item File. [Start]
*lnTotStk   = &lcTmpItemLoc..TotStk
*lnTotStVal = &lcTmpItemLoc..NSTKVAL
IF lcMajorOrNon = 'M'
  lnTotStk   = gfSumItem(EVALUATE(lcTmpItem + '.CSTYMAJOR'), 'TotStk', lcTmpItem2)
  lnTotStVal = gfSumItem(EVALUATE(lcTmpItem + '.CSTYMAJOR'), 'NSTKVAL', lcTmpItem2)
ELSE
  lnTotStk   = EVALUATE(lcTmpItem + '.TotStk')
  lnTotStVal = EVALUATE(lcTmpItem + '.NSTKVAL')
ENDIF
*B128949,1 WSH 08/07/2005 [End]

*E038245,1 WSH [End]

IF lnTotStk > 0
  lnAvCstVal = lnTotStVal / lnTotStk
ELSE
  *E038245,1 WSH Remove Second Item Cursor as it is not needed and Calculate from ItemLoc File [Start]
  *RETURN &lcTmpItem2..Ave_Cost
  RETURN Ave_Cost
  *E038245,1 WSH [End]
ENDIF
RETURN (lnAvCstVal)

*!*************************************************************
*! Name : gfvSeasonC (037923)
*! Auth : Khalid Mohi El-Din Mohamed (KHM)
*! Date : 04/01/2004.
*!*************************************************************
*! Synopsis : To validate the style selected by the user in the browse
*!         if it use the same season or not 
*!*************************************************************
*! Example :
*!        =gfvSeasonC()
*!*************************************************************
FUNCTION gfvSeasonC

*E038245,1 WSH Remove Second Item Cursor as it is not needed [Start]
*IF &lcTmpItem2..Season = lcSeason  OR ALLTRIM(&lcTmpItem2..Season) = 'Y'
IF Season = lcSeason  OR ALLTRIM(Season) = 'Y'
*E038245,1 WSH [End]

ELSE
   =MESSAGEBOX("You are restricted to styles from season "+lcSeason+"! RETRY")
ENDIF

*!*************************************************************
*! Name       : gfDispRep 
*! Developer  : Saeed Mostafa (SMM)
*! Date       : 07/12/2004.
*! Purpose    : To run a report from a screen without using OG
*! Entry      : B038282,1 Missing function
*!*************************************************************
*! PARAMETERS : 
*!     lcRprtNam : The report to run
*!*************************************************************
FUNCTION gfDispRep
PARAMETERS  lcRprtNam

LOCAL lcReportHome
lcReportHome = oAriaApplication.ReportHome

DO CASE
  *!B038457,1 SMM 09/01/2004 Solve If there are spaces in the path an error occured [START]
	*!*	  CASE oAriaApplication.gcDevice = "SCREEN"
	*!*	     IF oAriaApplication.glHeader
	*!*	       REPORT FORM &lcReportHome.&lcRprtNam PREVIEW 
	*!*	     ELSE
	*!*	       REPORT FORM &lcReportHome.&lcRprtNam PREVIEW PLAIN
	*!*	     ENDIF  
	*!*	  CASE oAriaApplication.gcDevice = "PRINTER"
	*!*	    IF oAriaApplication.glHeader           
	*!*	      REPORT FORM &lcReportHome.&lcRprtNam TO PRINTER NOCONSOLE NOEJECT 
	*!*	    ELSE
	*!*	      REPORT FORM &lcReportHome.&lcRprtNam TO PRINTER NOEJECT NOCONSOLE PLAIN
	*!*      ENDIF  
  
  CASE oAriaApplication.gcDevice = "SCREEN"    
    IF oAriaApplication.glHeader
      REPORT FORM "&lcReportHome.&lcRprtNam" PREVIEW 
    ELSE
      REPORT FORM "&lcReportHome.&lcRprtNam" PREVIEW PLAIN
    ENDIF  
  CASE oAriaApplication.gcDevice = "PRINTER"
    IF oAriaApplication.glHeader           
      REPORT FORM "&lcReportHome.&lcRprtNam" TO PRINTER NOCONSOLE NOEJECT 
    ELSE
      REPORT FORM "&lcReportHome.&lcRprtNam" TO PRINTER NOEJECT NOCONSOLE PLAIN
    ENDIF  
  *!B038457,1 SMM 09/01/2004 [END]
ENDCASE  

*!*************************************************************
*! Name       : gfSoftSeeK
*! Developer  : Saeed Mostafa (SMM)
*! Date       : 08/09/2004.
*! Purpose    : To run Soft Seek from IN Range class
*! Entry      : B123704,1
*!*************************************************************
*! PARAMETERS : 
*! lnKeyCode  : The last pressed Key
*! lcField    : The field which is searched 
*!*************************************************************
FUNCTION gfSoftSeek
PARAMETERS  lnKeyCode, lcField
LOCAL lcSearcKey
lcSearchKey = ''
DO FORM SOFTSEEK WITH ;
		CHR(lnKeyCode), LEN(&lcField) TO lcSearchKey
RETURN lcSearchKey


*!*	************************************************************************
*!*	Descrption    : Configuration Browse screen
*!*	Developer     : Hend Ghanem (HBG)
*!*	Date          : 11/02/2004  
*!*	Entry #       : N037401
*!*	************************************************************************
*!* P A S S E D    P A R A M E T E R S
*!* 01)-- lcStyle        : Current selected Style
*!* 02)-- lcWareCode     : Current selected Warehouse
*!* 03)-- llCloseButton  : Flag to display Close button or not.
*!* 04)-- lcValue		: Entered Configuratin value
*!* 05)-- lcMode	     : The mode of the screen
*!*	************************************************************************
*!*	Modification  :
*!*	************************************************************************
FUNCTION gfConfgBrw
*! C201210,1 MMT 01/11/2010 Add New Parameters to Style Configuration screen[Start]
*LPARAMETERS lcStyle,lcWareCode,llCloseButton,llSetFoucs,lcValue,lcMode
*! C201210,2 MMT 01/11/2010 Add New Parameters to Style Configuration screen for all Scale case[Start]
LPARAMETERS lcStyle,lcWareCode,llCloseButton,llSetFoucs,lcValue,lcMode,llAllClr,llSelectBut,llAllScl
*! C201210,2 MMT 01/11/2010 Add New Parameters to Style Configuration screen for all Scale case[End]
*! C201210,1 MMT 01/11/2010 Add New Parameters to Style Configuration screen[End]

LOCAL lcReturnValue
lcReturnValue= ''
*! C201210,1 MMT 01/11/2010 Add New Parameters to Style Configuration screen[Start]
*DO FORM ConfgBrw WITH lcStyle,lcWareCode,llCloseButton,llSetFoucs,lcValue,lcMode TO lcReturnValue
*! C201210,2 MMT 01/11/2010 Add New Parameters to Style Configuration screen for all Scale case[Start]
*DO FORM ConfgBrw WITH lcStyle,lcWareCode,llCloseButton,llSetFoucs,lcValue,lcMode,llAllClr,llSelectBut TO lcReturnValue
DO FORM ConfgBrw WITH lcStyle,lcWareCode,llCloseButton,llSetFoucs,lcValue,lcMode,llAllClr,llSelectBut,llAllScl TO lcReturnValue
*! C201210,2 MMT 01/11/2010 Add New Parameters to Style Configuration screen for all Scale case[End]
*! C201210,1 MMT 01/11/2010 Add New Parameters to Style Configuration screen[End]
RETURN lcReturnValue

*!*	************************************************************************
*!*	Descrption    : Browse screen
*!*	Developer     : Hend Ghanem (HBG)
*!*	Date          : 11/02/2004  
*!*	Entry #       : 
*!*	************************************************************************
*-- P A S S E D    P A R A M E T E R S
*01)-- tcBrowseFields : Browse fields
*02)-- tcBrowseTitle  : Browse title
*03)-- tcKey          : Browse key Expression
*04)-- tcFor          : Browse Filter Expression
*05)-- tcOptions      : string holds enable status of the 4 bars in the default shortcut
*   --                  Find, Filter, Quick find and Order
*06)-- tlSelect       : Show Select Button or not
*07)-- tcSelectObj    : for single select, referecance to select button method
*07)-- tcSelectMethod : for single select, name of method to call when click select button
*09)-- tcUserShortCut : address of three diminsional array that holds additional customized bars
*   --                  in the browse shortcut. 
*                       1: bar prompt
*   --                  2: Reference to object 
*                       3: Method name to run when select this bar
*10)-- tcTempFile     : Name of temporary file that will have multiple selections
*11)-- tcSelField     : Name of field to be returned in multiple selections
*12)-- llHalfHeight   : Browse full height or half height
*16)-- lcFieldsNam    : Returned fields
*17)-- lcArrName      : Returned Array
*!*	************************************************************************
*!*	Modification  :
*!*	************************************************************************
FUNCTION gfBrowScr
LPARAMETERS tcBrowseFields,tcBrowseTitle ,tcKey,tcFor   ,tcOptions ,tlSelect  ,;
           toSelectObj   ,tcSelectMethod,tcUserShortCut,tcTempFile,tcSelField,;
           llHalfHeight,lcFieldsNam,lcArrName,llChckKey,lcAliasName,llGetTree, ;
           lcBrowseFileName
           
LOCAL lcReturnValue
lcReturnValue= ''
DO FORM BROWSE WITH tcBrowseFields, tcBrowseTitle , tcKey, tcFor , tcOptions , tlSelect , toSelectObj TO lcReturnValue
RETURN lcReturnValue

*!*************************************************************
*! Name : gfGetUOMData
*! Auth : Wael M. Abo-Shawareb (WSH)
*! Date : 07/28/2004.
*!*************************************************************
*! Synopsis : To Get UOM Data from UOM Table
*!*************************************************************
*! Passed Parameters : 
*!      lcRelCode -> UOM-Relation Code     "By Reference"
*!      lcUOMFrom -> Convert from UOM-Code "By Reference"
*!      lcUOMTo   -> Convert to UOM-Code   "By Reference"
*!      lnConv    -> Conversion Factor     "By Reference"
*!      llDispMsg -> Flag to Display "Add, Browse, Reenter" message
*!                   or not if Relation not found. "By Value"
*!*************************************************************
*! Returned : 1 -> The User Select Add But Relation not Added.
*!            2 -> Relation Added, Found, or the User Browse and select Relation.
*!            3 -> The User Choose Reenter or Relation not found and not added
*!*************************************************************
*! Example :
*!        =gfGetUOMData(@lcRelCode, @lcUOMBuy, @lcUOMUse, @lnConv, .T.)
*!*************************************************************
*! E038220,1 WSH
FUNCTION gfGetUOMData
LPARAMETERS lcRelCode, lcUOMFrom, lcUOMTo, lnConv, llDispMsg

LOCAL lnRetVal, llCodesOpened, lcOldAlias, lcWhereCond, lcDefUOM, lnHandler, llAddRel, llBrowseRel
llCodesOpened = .F.
lnRetVal      = 3
lcOldAlias    = SELECT()
lcDefUOM      = SPACE(6)
llAddRel      = .F.
llBrowseRel   = .F.

IF EMPTY(lcRelCode) AND (lcUOMFrom == lcUOMTo) AND !EMPTY(lnConv) AND (lnConv <> 1)
  *--Can not create a Relation between a UOM Code and itself.
  =gfModalGen('QRM00414B42001', 'DIALOG')
  SELECT (lcOldAlias)
  RETURN 3
ENDIF

LOCAL lcUOMCurs
lcUOMCurs = gfTempName()

DIMENSION laUOMData[4]
laUOMData = ''

LOCAL lcUOMCode, lcUOM_B, lcUOM_V, lnConf
lcUOMCode = lcRelCode
lcUOM_B   = lcUOMFrom
lcUOM_V   = lcUOMTo
lnConf    = IIF(EMPTY(lcRelCode), lnConv, 0)
llDispMsg = IIF(EMPTY(lcRelCode), llDispMsg, .F.)

IF !USED('Codes')
  llCodesOpened = gfOpenFile(oAriaApplication.Datadir + 'CODES', 'CCODE_NO', 'SH')
ENDIF
lcDefUOM = IIF(SEEK("DCUNTOFMGR", "CODES", "CCODE_NO"), CODES.cCode_No, "EAC   ")

*--Open UOM Cursor based on passed parameters.
LOCAL lcUOMTag

IF !EMPTY(lcUOMCode)
  lcWhereCond = "CUOMCODE = '" + lcUOMCode + "'"
  lcUOMTag = 'UOMCODE'
ELSE
  lcUOM_B = IIF(!EMPTY(lcUOM_B), lcUOM_B, lcDefUOM)
  lcUOM_V = IIF(!EMPTY(lcUOM_V), lcUOM_V, lcDefUOM)
  lcWhereCond = "CUOM_B = '" + lcUOM_B + "'" +;
                " AND CUOM_V = '" + lcUOM_V + "'" +;
                  IIF(!EMPTY(lnConf), " AND NCONF = " + STR(lnConf), "")
  lcUOMTag = 'UOM'
ENDIF
IF !gfOpnSqlFl("UOM", lcUOMCurs, lcWhereCond, '', "UOMCODE")
  SELECT (lcOldAlias)
  RETURN 3
ENDIF

*--Case of one Relation found
IF RECCOUNT(lcUOMCurs) = 1
  lcRelCode = EVALUATE(lcUOMCurs + '.cUOMCode')
  lcUOMFrom = EVALUATE(lcUOMCurs + '.cUOM_B')
  lcUOMTo   = EVALUATE(lcUOMCurs + '.cUOM_V')
  lnConv    = EVALUATE(lcUOMCurs + '.nConF')
  lnRetVal  = 2
ELSE
  IF llDispMsg
    *--Display Query Message based on Current Situation.
    LOCAL lnSelect, llWasSel
    
    IF EOF(lcUOMCurs)
      IF EMPTY(lnConf)
        *--No Relations Found Between <UOM_B> and <UOM_V>
        *--  "Add,Reenter"
        lnSelect = gfModalGen('TRM00411B42006', 'DIALOG', RTRIM(lcUOM_B) + '|' + RTRIM(lcUOM_V))
        IF lnSelect = 2   && Reenter
          lnSelect = 3
        ENDIF
      ELSE
        *--No Relations Found Between <UOM_B> and <UOM_V> with Conversion <lnConf> 
        *--  "Add,Browse,Reenter"
        lnSelect = gfModalGen('TRM00412B42003', 'DIALOG', RTRIM(lcUOM_B) + '|' + RTRIM(lcUOM_V) + '|' + ALLTRIM(STR(lnConf)))
      ENDIF
    ELSE
      *--More than one Relation Found Between <UOM_B> and <UOM_V>
      *--  "Add,Browse,Reenter"
      lnSelect = gfModalGen('TRM00413B42003', 'DIALOG', RTRIM(lcUOM_B) + '|' + RTRIM(lcUOM_V))
    ENDIF
    
    DO CASE
      *--Case Select Add
      CASE lnSelect = 1
        llAddRel = .T.
      *--Case Select Browse
      CASE lnSelect = 2
        lcWhereCond = "CUOM_B = '" + lcUOM_B + "'" +;
                      " AND CUOM_V = '" + lcUOM_V + "'"
        
        IF EMPTY(lnConf) OR gfOpnSqlFl("UOM", lcUOMCurs, lcWhereCond, '', "UOM")
          llBrowseRel = .T.
        ENDIF
    ENDCASE
  ELSE
    IF EOF(lcUOMCurs)
      IF EMPTY(lnConf)
        *--Nothing to ADD or Browse for.
        *IF !EMPTY(lcUOMCode)
        IF EMPTY(lcUOMCode)
          *=gfModalGen('TRM00415B42001', 'DIALOG')
        *ELSE
          =gfModalGen('TRM00411B42001', 'DIALOG', RTRIM(lcUOM_B) + '|' + RTRIM(lcUOM_V))
        ENDIF
      ELSE
        llAddRel = .T.
      ENDIF
    ELSE
      IF EMPTY(lcUOMCode) AND EMPTY(lnConf)
        llBrowseRel = .T.
      ENDIF
    ENDIF
  ENDIF
ENDIF

*--Relation not found and not added then browse.
IF llBrowseRel
  IF !EOF()
    PRIVATE lcBrFields
    
    *--Browse UOMs
    *lcBrFields = "CUOMCODE:H='"+LANG_ARIA_UOM_REL+"':22,lcUOM_B=gfCodDes(CUOM_B,'CUNTOFMGR'):H='"+LANG_ARIA_UOM_B+"':22,"+;
                 "lcUOM_V=gfCodDes(CUOM_V,'CUNTOFMGR'):H='"+LANG_ARIA_UOM_V+"':22,NCONF:H='"+LANG_ARIA_UOM_CONF+"':22"
    lcBrFields = "lcUOM_B=gfCodDes(CUOM_B,'CUNTOFMGR'):H='"+LANG_ARIA_UOM_B+"':22,"+;
                 "lcUOM_V=gfCodDes(CUOM_V,'CUNTOFMGR'):H='"+LANG_ARIA_UOM_V+"':22,"+;
                 "NCONF:H='"+LANG_ARIA_UOM_CONF+"':25"

    SELECT (lcUOMCurs)
    llWasSel = ARIABROW('', LANG_ARIA_UOM, .F., .F., .F., .F., '', '', 'CUOMCODE,CUOM_B,CUOM_V,NCONF', 'laUOMData')
    
    *--Fill UOM Variables if Relation Code Selected
    IF llWasSel
      lcRelCode = laUOMData[1]
      lcUOMFrom = laUOMData[2]
      lcUOMTo   = laUOMData[3]
      lnConv    = laUOMData[4]
      lnRetVal  = 2
    ENDIF
  ELSE
    *--Nothing to ADD or Browse for.
    =gfModalGen('TRM00415B42001', 'DIALOG')
  ENDIF
ENDIF

IF llAddRel
  LOCAL lcTranCode
  
  *--Add Record to UOM Table if we have a conversion factor.
  IF !EMPTY(lnConf)
    LOCAL lcStatement, lcTempCurs, lcTopRel
    lcTempCurs = gfTempName()
    
    *--Get Last Relation Code Value to Set New One.
    lcStatement = "SELECT TOP 1 CUOMCODE FROM UOM (INDEX=UOMCODE) ORDER BY CUOMCODE DESC"
    lnHandlar = oAriaApplication.RemoteCompanyData.SQLRun(lcStatement, lcTempCurs, "UOM",;
                oAriaApplication.ActiveCompanyConStr, 3, 'SAVE', SET("Datasession"))

    IF lnHandlar = 1
      *--Set the Highest Relation Code in UOM file
      lcTopRel = STRTRAN(STR(VAL(EVALUATE(lcTempCurs + '.CUOMCODE')) + 1, 6, 0), ' ', '0')
      
      *--Add record in UOM table for new Relation.
      INSERT INTO (lcUOMCurs) (CUOMCODE, CUOM_B, CUOM_V, NCONF) VALUES (lcTopRel, lcUOM_B, lcUOM_V, lnConf)
      =gfAdd_info(lcUOMCurs)
      
      IF gfUpdSqlFl(lcUOMCurs, 'CUOMCODE', 'UOM')
        lcRelCode = lcTopRel
        lcUOMFrom = lcUOM_B
        lcUOMTo   = lcUOM_V
        lnConv    = lnConf
        lnRetVal  = 2
      ENDIF
    ELSE
      =oAriaApplication.RemoteCompanyData.CheckRetResult("SQLRUN", lnHandlar, .T.)
    ENDIF
          
    USE IN (lcTempCurs)
  ELSE
    lnRetVal = 1
  ENDIF
ENDIF

USE IN (lcUOMCurs)
IF llCodesOpened
  USE IN CODES
ENDIF

SELECT (lcOldAlias)
RETURN lnRetVal


*!*************************************************************
*! Name      	: lfGetSysColors
*! Developer 	: Mahmoud Said (MAH) 
*! Date      	: 08/23/2004
*! Purpose   	: Return the nuber of colors used by the system.
*! Tracking #   : E038428,1 MAH 08/23/2004 The color background is not clear.
*!*************************************************************
*! Returns 	: Number of color used by the system
*!*************************************************************
FUNCTION lfGetSysColors
*-- Get handle to desktop device context
DECLARE INTEGER GetDC IN WIN32API INTEGER
LOCAL lnDCHandle
lnDCHandle = GetDC(0)

*-- Get device info
DECLARE INTEGER GetDeviceCaps IN WIN32API INTEGER, INTEGER 
LOCAL lnPlanes, lnBitsPixel
lnPlanes    = GetDeviceCaps(lnDCHandle, 14)
lnBitsPixel = GetDeviceCaps(lnDCHandle, 12)

*-- Returns the number of colors used by the driver
LOCAL lnNumColors
lnNumColors = 2 ^ (lnPlanes * lnBitsPixel)

*-- The following code releases the GetDC handle
DECLARE INTEGER ReleaseDC IN Win32API INTEGER, INTEGER
=ReleaseDC(0, lnDCHandle)

RETURN lnNumColors



*!*****************************************************************************************
*! Name       : gfConvertDataType
*! Developer  : SMM - Saeed Mohammed Mostafa
*! Date       : 09/05/2004
*! Purpose    : Convert data type from Fox Format to SQL Format
*! Entry no.  : N038470
*! Parameters : lcFoxStruct : Comma separated string or array 
*!*****************************************************************************************
FUNCTION gfConvertDataType
LPARAMETERS lcFoxStruct


LOCAL lcSQLStruct,lcCommaPos,lcSpacePos,lcField,lcStruct, lnCount,lcStr
LOCAL lcOpenBracketPos, lcCloseBracketPos

lcFileType = IIF(TYPE("lcFoxStruct[1]") ='C', 'A', 'C')
IF lcFileType = 'C' && Structure passed as comma separated string
  LOCAL lcRetStruct
  lcStr = ALLTRIM(lcFoxStruct) + ","
  lcRetStruct = ""
  DO WHILE LEN(lcStr) > 1	
  lcSpacePos = AT(SPACE(1),lcStr)
	lcOpenBracketPos  = AT('(',lcStr)
	lcCloseBracketPos = AT(')',lcStr)
	lcCommaPos = IIF(lcOpenBracketPos < AT(',',lcStr) AND lcCloseBracketPos > ;
					AT(',',lcStr),AT(',',lcStr,2),AT(',',lcStr))
    lcField  = SUBSTR(lcStr,1,lcSpacePos-1)
    lcStruct = ALLTRIM(SUBSTR(lcStr,lcSpacePos,lcCommaPos-lcSpacePos))
    DO CASE && Check For the connection Driver
      CASE oAriaApplication.ConnectionDriver = 'SQL'
      DO CASE 
	    CASE LEFT(lcStruct,1) = 'C'
		      lcSQLStruct = "Char" + SUBSTR(lcStruct,2,LEN(lcStruct)-1)
        CASE LEFT(lcStruct,1) = 'N'
		      lcSQLStruct = "Numeric"+ SUBSTR(lcStruct,2,LEN(lcStruct)-1)
        CASE lcStruct = 'L'
	  	    lcSQLStruct = "bit"
        CASE lcStruct = 'D'
	    	  lcSQLStruct = "datetime"
        CASE lcStruct = 'T'
	      	lcSQLStruct = "datetime"
        CASE lcStruct = 'MEMO'
		      lcSQLStruct = "Text"
        CASE lcStruct = 'M'
		      lcSQLStruct = "Text"
        OTHERWISE
   	      lcSQLStruct = ""
      ENDCASE
    ENDCASE
	IF EMPTY(lcRetStruct) 
  	  lcRetStruct = lcField + SPACE(1) + lcSQLStruct    		    
	ELSE
	  lcRetStruct = lcRetStruct + "," + lcField + SPACE(1) + lcSQLStruct    		    
	ENDIF
	lcStr = ALLTRIM(SUBSTR(lcSTR,lcCommaPos+1,LEN(lcSTR)-lcCommaPos))
  ENDDO	
  RETURN lcRetStruct

ELSE && AFIELDS
  FOR lnCount = 1 TO ALEN(lcFoxStruct,1)
    * C = Character, D = Date, L = Logical, M = Memo, N = Numeric, F = Float, I = Integer
    DO CASE && Check For the connection Driver
      CASE oAriaApplication.ConnectionDriver = 'SQL'
      DO CASE 
	    CASE lcFoxStruct[lnCount,2] = 'C'
	  	    lcSQLStruct = "Char"
        CASE lcFoxStruct[lnCount,2] = 'N'
	    	  lcSQLStruct = "Numeric"
        CASE lcFoxStruct[lnCount,2] = 'L'
	      	lcSQLStruct = "bit"
        CASE lcFoxStruct[lnCount,2]	= 'D'
		      lcSQLStruct = "datetime"
        CASE lcFoxStruct[lnCount,2]	= 'T'
		      lcSQLStruct = "datetime"
       CASE lcFoxStruct[lnCount,2]	= 'M'
		      lcSQLStruct = "Text"
       CASE lcFoxStruct[lnCount,2]	= 'MEMO'
		      lcSQLStruct = "Text"
        CASE lcFoxStruct[lnCount,2]	= 'F'
		      lcSQLStruct = "Float"
        CASE lcFoxStruct[lnCount,2]	= 'I'
		      lcSQLStruct = "Integer"
        OTHERWISE
   	      lcSQLStruct = ""
      ENDCASE
    ENDCASE
	lcFoxStruct[lnCount,2] = lcSQLStruct
  ENDFOR
ENDIF





*!***********************************************************************************************************************
*! Name      	: lfGetExpTableName
*! Developer 	: Mahmoud Said [MAH]
*! Date      	: 08/31/2004
*! Purpose   	: Get the table name, which is used in any expression.
*! Tracking   	: E038142,2 MAH 08/31/2004 Full support for run forms with SQL with high Performance.
*!***********************************************************************************************************************
*! Parameters  	: 
*!  lcExpression : Expression
*!***********************************************************************************************************************
*! Returns 	: Table name
*!***********************************************************************************************************************
FUNCTION lfGetExpTableName
LPARAMETERS lcExpression

LOCAL lcTableName
lcTableName = ''

IF AT('(', lcExpression) = 0 
  lcTableName = ALLTRIM(LEFT(lcExpression, AT('.', lcExpression)))
ELSE
  *-- Get table name
  IF AT('.', lcExpression) = 0 
    FOR lnIndex = AT('.', lcExpression) - 1 TO 1 STEP -1
      IF lnIndex = 1
        lcTableName = ALLTRIM(LEFT(lcExpression, AT('.', lcExpression)))
        EXIT
      ENDIF

      IF !ISALPHA(SUBSTR(lcExpression, lnIndex, 1))
        lcTableName = ALLTRIM(SUBSTR(lcExpression, lnIndex + 1, AT('.', lcExpression) - lnIndex - 1))
        EXIT
      ENDIF
    ENDFOR
  ENDIF
ENDIF

RETURN lcTableName


*!***********************************************************************************************************************
*! Name      	: lfGetConditionLeftSide
*! Developer 	: Mahmoud Said [MAH]
*! Date      	: 08/31/2004
*! Purpose   	: Get the left side name, which is used in any expression.
*! Tracking   	: E038142,2 MAH 08/31/2004 Full support for run forms with SQL with high Performance.
*!***********************************************************************************************************************
*! Parameters  	: 
*!  lcExpression : Expression
*!  lcDBEngine   : DB engine ID
*!***********************************************************************************************************************
*! Returns 	: Table name
*!***********************************************************************************************************************
FUNCTION lfGetConditionLeftSide
LPARAMETERS lcExpression, lcDBEngine

lcDBEngine = IIF(TYPE('lcDBEngine') = 'C', lcDBEngine, oAriaApplication.cNativeDBID)

LOCAL lcToRetExpression
lcToRetExpression = ''

DO CASE
  CASE ALLTRIM(lcDBEngine) == oAriaApplication.cNativeDBID
    RETURN lcExpression
    
  CASE ALLTRIM(lcDBEngine) == oAriaApplication.cSQLDBID
    IF AT('(', lcExpression) = 0
      lcToRetExpression = lcExpression
    ELSE
      *-- Convert SUBSTR   , PADR, UPPER, DTOS   , STR, ALLTRIM, LEFT, LOWER to the new list
      *-- Convert SUBSTRING,     ,      , CONVERT, STR, LTRIM  , LEFT, 
      LOCAL lnIndex 
      FOR lnIndex = 1 TO LEN(lcExpression)
        IF SUBSTR(lcExpression, lnIndex, 1) == "("
        
          *-- Get Command
          LOCAL lnInternalIndex
          FOR lnInternalIndex = lnIndex - 1 TO 1 STEP -1
            IF lnInternalIndex = 1
              EXIT
            ENDIF

            IF !ISALPHA(SUBSTR(lcExpression, lnInternalIndex, 1))
              EXIT
            ENDIF
          ENDFOR
      
          LOCAL lcCommand, lcReplace
          lcReplace = ''
      
          *-- Replace command
          lcCommand = UPPER(ALLTRIM(SUBSTR(lcExpression, lnInternalIndex + 1, lnIndex - lnInternalIndex - 1)))
          DO CASE 
            CASE lcCommand == 'SUBSTR'
              lcReplace = 'SUBSTRING'
              
            CASE lcCommand == 'PADR'
              lcReplace = ''
            
            CASE lcCommand == 'UPPER'
              lcReplace = ''
            
            CASE lcCommand == 'LOWER'
              lcReplace = ''
            
            CASE lcCommand == 'ALLTRIM'
              lcReplace = 'LTRIM'
            
            CASE lcCommand == 'DTOS'
              lcReplace = 'CONVERT(CHAR(8), '
          ENDCASE
          
          lcToRetExpression = LEFT(lcToRetExpression, LEN(lcToRetExpression) - LEN(lcCommand))
          lcToRetExpression = lcToRetExpression + lcReplace
          
          *-- Handle DTOS
          IF lcCommand == 'DTOS'
            LOCAL lnBrackets
            lnBrackets = 1
            
            FOR lnInternalIndex = lnIndex + 1 TO LEN(lcExpression)
              LOCAL lcCurrentChar
              lcCurrentChar = SUBSTR(lcExpression, lnInternalIndex, 1)
              lnBrackets = IIF(lcCurrentChar = ')', -1, IIF(lcCurrentChar = '(', 1, 0))
              
              IF lnBrackets = 0 
                lcExpression = LEFT(lcExpression, lnInternalIndex - 1) + ;
                               '|' + ;
                               SUBSTR(lcExpression, lnInternalIndex + 1)
              ENDIF
            ENDFOR
          ENDIF
        ELSE
          IF SUBSTR(lcExpression, lnIndex, 1) = "|"
            lcToRetExpression = lcToRetExpression + ', 112)'
          ELSE
            lcToRetExpression = lcToRetExpression + SUBSTR(lcExpression, lnIndex, 1)
          ENDIF
        ENDIF  
      ENDFOR
    ENDIF

    *-- Replace fields names with [FieldName]
    LOCAL lnDotNo, lnDotPos
    lnDotNo = 1
    
    lnDotPos = AT('.', lcToRetExpression, lnDotNo)
    DO WHILE lnDotPos > 0
      LOCAL lnIndex
      FOR lnIndex = lnDotPos + 1 TO LEN(lcToRetExpression)
        IF lnIndex = LEN(lcToRetExpression)
            lcToRetExpression = SUBSTR(lcToRetExpression, 1, lnDotPos) + ;
                                '[' + ;
                                SUBSTR(lcToRetExpression, lnDotPos + 1, lnIndex - lnDotPos) + ;
                                ']'
        ELSE
          * MAH T20080331.0008 20 May 2008 Check Also for underscor
          *-- IF !ISALPHA(SUBSTR(lcToRetExpression, lnIndex, 1))
          IF !ISALPHA(SUBSTR(lcToRetExpression, lnIndex, 1)) .AND. !(SUBSTR(lcToRetExpression, lnIndex, 1) = "_")
          * MAH T20080331.0008 20 May 2008 Check Also for underscor
            lcToRetExpression = SUBSTR(lcToRetExpression, 1, lnDotPos) + ;
                                '[' + ;
                                SUBSTR(lcToRetExpression, lnDotPos + 1, lnIndex - lnDotPos - 1) + ;
                                ']' + ;
                                SUBSTR(lcToRetExpression, lnIndex)
            EXIT
          ENDIF
        ENDIF
      ENDFOR
      
      lnDotNo  = lnDotNo + 1
      lnDotPos = AT('.', lcToRetExpression, lnDotNo)
    ENDDO
    
    RETURN lcToRetExpression
ENDCASE

*!***********************************************************************************************************************
*! Name      	: gfGetTableInfo
*! Developer 	: Mahmoud Said [MAH]
*! Date      	: 09/27/2004
*! Purpose   	: Get information about any table
*! Tracking   	: E038142,2 MAH 08/31/2004 Full support for run forms with SQL with high Performance.
*!***********************************************************************************************************************
*! Parameters  	: 
*! lcTableName             : (R) Table name
*! lcDBEngineType          : (W) Table database engine
*! lcUniqueIndexName       : (W) Unique index name in case of oAriaApplication.cSQLDBID
*! lcUniqueIndexExpression : (W) Unique index expression in case of oAriaApplication.cSQLDBID
*! llUniqueIndexOrder      : (W) Unique index order in case of oAriaApplication.cSQLDBID
*! llServerError           : (W) Error while connecting to server 
*!***********************************************************************************************************************
*! Returns 	: 
*!***********************************************************************************************************************
FUNCTION gfGetTableInfo

PARAMETERS lcTableName, lcDBEngineType, lcUniqueIndexName, lcUniqueIndexExpression, llUniqueIndexOrder, llServerError

LOCAL lnSelect
lnSelect = SELECT()

llServerError = .F.

LOCAL lcTempName
lcTempName = gfTempName()

LOCAL lnFldResult
lnFldResult = oAriaApplication.RemoteSystemData.Execute( ;
                                        "SELECT cFile_Nam FROM SydFiles WHERE cFile_Nam = '" + ;
                                        PADR(lcTableName, 8) + "'", ;
                                        '', ;
                                        lcTempName, ;
                                        '', ;
                                        oAriaApplication.cAria4Sysfiles, ;
                                        3, ;
                                        '', ; 
                                        SET("Datasession"))


IF lnFldResult = 1
	*-- If RECCOUNT(lcTempName) = 1 means this table from type SQL 
	*-- If RECCOUNT(lcTempName) = 0  means this table from type FOX 
  IF RECCOUNT(lcTempName) > 0 
    lcDBEngineType = oAriaApplication.cSQLDBID
    
    LOCAL lnFldResult
    lnFldResult = oAriaApplication.RemoteSystemData.Execute( ;
                                           "SELECT cfile_tag, cindx_exp, lascend FROM sydindex WHERE cFile_nam = '" + ;
                                           PADR(lcTableName, 8) + "'" + ;
                                           ' AND lunique', ;
                                           '', ;
                                           lcTempName, ;
                                           '', ;
                                           oAriaApplication.cAria4Sysfiles, ;
                                           3, ;
                                           '', ; 
                                           SET("Datasession"))
    IF lnFldResult = 1
      lcUniqueIndexName       = ALLTRIM(&lcTempName..cFile_Tag)
      lcUniqueIndexExpression = ALLTRIM(&lcTempName..cIndx_Exp)
      llUniqueIndexOrder      = &lcTempName..lAscend
    ELSE
     oAriaApplication.RemoteSystemData.CheckRetResult('Execute', lnFldResult, .T.)
     llServerError = .T.
    ENDIF
  ELSE
    lcDBEngineType = oAriaApplication.cFOXDBID
  ENDIF
  
  USE IN (lcTempName)
ELSE
  oAriaApplication.RemoteSystemData.CheckRetResult('Execute', lnFldResult, .T.)
  llServerError = .T.
ENDIF

SELECT(lnSelect)

*!*************************************************************
*! Name      : gfValdPath
*! Developer : Mariam Mazhar
*! Date      : 17/08/2004
*! Purpose   : To validate any path if right or wrong
*!*************************************************************
*! Calls     : 
*!      Called by: Called from custom style summary report 
*!*************************************************************
*! Passed Parameters  : Path to be validated
*!*************************************************************
*! Returns            : Flag to tell valid or not
*!*************************************************************
*! Example   : 
*!*************************************************************
FUNCTION gfValdPath
PARAMETERS lcPath

llRetVal = .T.
lcDefault=FULLPATH(SET('DEFAULT'))
lcOnError=ON('ERROR')

ON ERROR llError = .T.
llError          = .F.


SET DEFAULT TO (lcPath)

IF llError
  llRetVal = .F.
ENDIF

ON ERROR &lcOnError

SET DEFAULT TO (lcDefault)

RETURN llRetVal



*!*************************************************************
*! Name      : gfVldUsrFld
*! Developer : Saeed Mohammed Mostafa
*! Date      : 09/30/2004
*! Purpose   : Function to Validate User define fields Values
*! Tracking  : N037249
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : laPrUsrFields Array
*!*************************************************************
*! Returns            : Logical 
*!*************************************************************
*! Called Form        : gfOpGrid , Control panel save
*!*************************************************************
*! Example            : = gfVldUsrFld(@laUsrFields)
*!*************************************************************
*! Due To C101459,1
*!*************************************************************
FUNCTION gfVldUsrFld
PARAMETERS laPrUsrFields

*N037773,1 WSH 05/17/2005 Declare variables as local as they may cause an infinite loop. [Start]
*PRIVATE lnCount,llValid,lnAlias
LOCAL lnCount,llValid,lnAlias,lcCurrCntrl
*N037773,1 WSH 05/17/2005 [End]

lnAlias = SELECT()
SELECT 0
llValid = .T.
FOR lnCount = 1 TO ALEN(laPrUsrFields,1)
  IF !EMPTY(laPrUsrFields[lnCount,8])

    *N037773,1 WSH 05/17/2005 Assign the Focus Control of the Option Grid to be the validated field, to use in valid functions. [Start]
    lcCurrCntrl = loOGScroll.GetObjectRef('laOGFxFlt[' + ALLTRIM(STR(lnCount)) + ',1]')
    lcCurrCntrl.Parent.Parent.FocusControl = lcCurrCntrl.Parent.Name
    lcCurrCntrl.Parent.FocusControl        = lcCurrCntrl.Name
    *N037773,1 WSH 05/17/2005 [End]

    lcFldName = laPrUsrFields[lnCount,1]
    PRIVATE &lcFldName
    &lcFldName = laPrUsrFields[lnCount,6]
    lnWhenPos = AT('WHEN',laPrUsrFields[lnCount,8])
    lcValid = SUBSTR(laPrUsrFields[lnCount,8],1,IIF(lnWhenPos=0,LEN(laPrUsrFields[lnCount,8]),lnWhenPos-1))

    *! SMM to cancel error [START]
    LOCAL lcOldErr
    lcOldErr = ON("Error")
    ON ERROR llValid = .T.
    *! SMM to cancel error [END]

    llValid = EVAL(ALLT(STRTRAN(lcValid,ALLT(laPrUsrFields[lnCount,1]),'m.'+ALLT(laPrUsrFields[lnCount,1]))))
	IF TYPE("llValid") # "L"
  	  llValid  = .T.
	ENDIF
    *! SMM to cancel error [START]
    ON ERROR &lcOldErr
    *! SMM to cancel error [END]

    IF !llValid
      *N037773,1 WSH 10/11/2005 Check the existance of the array element before using it. [Start]
      IF TYPE("laPrUsrFields[lnCount,9]") # 'U' AND !EMPTY(laPrUsrFields[lnCount,9])
      *N037773,1 WSH 10/11/2005 [End]

        lcErrMessage = "User defined field ("+ALLT(laPrUsrFields[lnCount,9])+") is not valid."+CHR(13)+CHR(10)+;
                       "Value must be "+ALLT(STRTRAN(laPrUsrFields[lnCount,8],laPrUsrFields[lnCount,1],ALLT(laPrUsrFields[lnCount,9])))
        =gfModalGen(.F.,.F.,.F.,.F.,lcErrMessage)

      *N037773,1 WSH 10/11/2005 Check the existance of the array element before using it. [Start]
      ENDIF
      *N037773,1 WSH 10/11/2005 [End]
      
      EXIT
    ENDIF
  ENDIF
ENDFOR
SELECT (lnAlias)
RETURN llValid


*!***********************************************************************************************************************
*! Name        : gfExecute
*! Developer   : Ahmad Shoukry Mohammed (ASM)
*! Date        : 10/10/2004
*! Purpose     : Executes a command to overcome the problem of calling forms include in the EXE
*!***********************************************************************************************************************
*! Parameters  : The string to execute
*!***********************************************************************************************************************
*! Returns     : None
*!***********************************************************************************************************************
FUNCTION gfExecute
LPARAMETERS lcStr
  &lcStr
RETURN

*!*************************************************************
*! Name      : gfUsrVldFn
*! Developer : Ahmed Mohamed Ibrahim
*! Date      : 01/31/2001
*! Purpose   : Function to run any function withen its program
*!*************************************************************
*! Called from : Global all over the system
*!*************************************************************
*! Calls       : passed function as a parameter
*!*************************************************************
*! Passed Parameters : lcFncNam,lcFncLoc,lcParam
*!                     lcFncNam  : Variable hold the name function 
*!                     lcFncLoc  : Variable hold the function path
*!                     lcParam   : Variable hold the parameters 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = gfUsrVldFn()
*!*************************************************************
FUNCTION gfUsrVldFn
PARAMETERS lcFncNam,lcFncLoc,lcParam
*-- lcRetrn variable to hold the return value
PRIVATE lcRetrn

IF !EMPTY(lcFncNam)
  lcRetrn  = SPACE(0)
  *! B609526,1 MMT 02/17/2011 Check the client main file on X in case of SAAS[T20101109.0012][Start]
  llClientFile = .F.
  IF oAriaApplication.multiinst 
    lcRootDr = STRTRAN(UPPER(oAriaApplication.clientapplicationhome),'PRGS\','')
    IF !EMPTY(lcFncLoc)  
      IF  FILE(lcRootDr+lcFncLoc+'.FXP')
        lcFncLoc = lcRootDr+lcFncLoc
        llClientFile = .T.
      ELSE
        lcFncLoc= IIF(FILE(oAriaApplication.clientapplicationhome+lcFncLoc+'.FXP'),;
                       oAriaApplication.clientapplicationhome+lcFncLoc,;
  	  			       IIF(FILE(oAriaApplication.clientapplicationhome+;
  			       			oAriaApplication.ActiveModuleID+'\'+lcFncLoc+'.FXP'),;
  				            oAriaApplication.clientapplicationhome+;
  				            oAriaApplication.ActiveModuleID+'\'+lcFncLoc,;
  				            oAriaApplication.clientapplicationhome+LEFT(lcFncLoc,2)+'\'+lcFncLoc))
  		  IF FILE(lcFncLoc+'.FXP')
  		    llClientFile = .T.
  		  ENDIF	            
      ENDIF
    ENDIF
  ENDIF 
  IF !llClientFile 
  *! B609526,1 MMT 02/17/2011 Check the client main file on X in case of SAAS[T20101109.0012][End]
  lcRootDr = STRTRAN(UPPER(oAriaApplication.ApplicationHome),'PRGS\','')
  * If lcFncLoc parm. is not empty that measns the lcFncNam function will be called from 
  * main program lcFncLoc 
  IF !EMPTY(lcFncLoc)  
    IF  FILE(lcRootDr+lcFncLoc+'.FXP')
      lcFncLoc = lcRootDr+lcFncLoc
    ELSE
      lcFncLoc= IIF(FILE(oAriaApplication.ApplicationHome+lcFncLoc+'.FXP'),;
                       oAriaApplication.ApplicationHome+lcFncLoc,;
  	  			       IIF(FILE(oAriaApplication.ApplicationHome+;
  			       			oAriaApplication.ActiveModuleID+'\'+lcFncLoc+'.FXP'),;
  				            oAriaApplication.ApplicationHome+;
  				            oAriaApplication.ActiveModuleID+'\'+lcFncLoc,;
  				            oAriaApplication.ApplicationHome+LEFT(lcFncLoc,2)+'\'+lcFncLoc))
    ENDIF
  ELSE
     * If lcFncLoc is empty that means the lcFncNam function is not embaeded into prg bit its a 
     * separate program. 
  ENDIF  
  *! SMM to cancel error [START]
  *! B609526,1 MMT 02/17/2011 Check the client main file on X in case of SAAS[T20101109.0012][Start]
  ENDIF
  *! B609526,1 MMT 02/17/2011 Check the client main file on X in case of SAAS[T20101109.0012][End]
  LOCAL lcOldErr
  lcOldErr = ON("Error")
  ON ERROR RETURN .T.
  *! SMM to cancel error [END]
  IF EMPTY(lcParam)
    DO (lcFncNam) IN (lcFncLoc) WITH lcRetrn
  ELSE
    DO (lcFncNam) IN (lcFncLoc) WITH lcRetrn,&lcParam
  ENDIF
  *! SMM to cancel error [START]
  ON ERROR &lcOldErr
  *! SMM to cancel error [END]
  RETURN lcRetrn
ENDIF
*!*************************************************************
*! Name       : gfPDFViewer
*! Developer  : Saeed Mostafa (SMM)
*! Date       : 10/28/2004.
*! Purpose    : To run PDF viewer
*! Entry      : N038424,1
*!*************************************************************
*! PARAMETERS : 
*! lcCaption  : Title
*! lcFilePath : The Path of the PDF File
*!*************************************************************
FUNCTION gfPDFViewer
PARAMETERS  lcCaption, lcFilePath
DO FORM PDFViewer WITH lcCaption, lcFilePath
RETURN .T.

****************************************************************************
*! Name      : GetScale()
*! Developer : Heba Fathi (HFK)
*! Date      : 10/20/2004
*! Purpose   : Function that will return a string with the size scale
****************************************************************************
*! Parameters: 1 =>  Size scale to be displayed
*!             2 =>  Spacing between each size
****************************************************************************
PROCEDURE GetScale
PARAMETERS XSC,XSPACE
PRIVATE XOLDALIAS,X,XSTRING,XSC,XSPACE,Z
XOLDALIAS= ALIAS()
=gfOpenFile(gcDataDir+'SCALE',gcDataDir+'SCALE','SH')
SELE SCALE
SEEK 'S'+XSC
X       = 1
XSTRING = ''
DO WHILE FOUND() .AND. X<=CNT
  Z = STR(X,1)
  XSTRING = XSTRING + SZ&Z + IIF(X=CNT,'',XSPACE)
  X= X + 1
ENDDO
IF .NOT. FOUND()
  XSTRING ='* * * E R R O R * * *'
ENDIF
IF LEN(TRIM(XOLDALIAS)) > 0
  SELE &XOLDALIAS
ENDIF

RETURN(XSTRING)

*!*************************************************************
*! Name      	: gfGetTopLevelFrom
*! Developer 	: Mahmoud Said (MAH) 
*! Date      	: 12/03/2004
*! Purpose   	: Return refrence to top level form
*! Tracking #   : E037885,2 MAH 12/03/2004 Separate screen in different session.
****************************************************************************
*! Parameters : 
*!  loScreen : Handle to screen we need to get parent for
****************************************************************************
*! Returns 	: Return refrence to top level form or .NULL.
*:************************************************************************
*: Modifications : 
*! B039071,1 MAH 02/24/2005 Close Group Problem.
*:************************************************************************

FUNCTION gfGetTopLevelFrom
LPARAMETERS loScreen

DECLARE INTEGER GetParent IN WIN32API INTEGER

LOCAL loForm

IF TYPE('loScreen') = 'O' .AND. !ISNULL(loScreen)
  loForm = loScreen
ELSE
  IF TYPE('_SCREEN.ActiveForm') # 'O' .OR. ISNULL(_SCREEN.ActiveForm)
    RETURN .NULL.
  ELSE
    loForm = _SCREEN.ActiveForm
  ENDIF
ENDIF


IF TYPE('loForm') # 'O' .OR. ISNULL(loForm)
  RETURN .NULL.
ELSE
  DO WHILE !(UPPER(loForm.BaseClass) == 'FORM') .AND. TYPE('loForm.Parent') = 'O' .AND. !ISNULL(loForm.Parent)
    
    
    * T20080429.0009 MAH 18 / 6 / 2008 BEGIN
    IF TYPE('loForm.Parent') != 'O' .OR. ISNULL(loForm.Parent)
      RETURN .NULL.
    ENDIF
   * T20080429.0009 MAH 18 / 6 / 2008 END
   
   loForm = loForm.Parent
    
  ENDDO
  
  LOCAL lnTopLevelHwnd
  lnTopLevelHwnd = loForm.HWnd

  DO WHILE lnTopLevelHwnd # 0
    IF lnTopLevelHwnd = 0
      RETURN .NULL.
    ELSE
      IF _SCREEN.HWnd = lnTopLevelHwnd
        RETURN _SCREEN
      ENDIF
      
      LOCAL lnIndex
      FOR lnIndex = 1 TO _SCREEN.FormCount
      	*! B039071,1 MAH 02/24/2005 [BEGIN]
      	IF TYPE('_SCREEN.Forms(lnIndex).Name') = 'O' .AND. !ISNULL(_SCREEN.Forms(lnIndex).Name)
      	*! B039071,1 MAH 02/24/2005 [END]
        
          IF !EMPTY(_SCREEN.Forms(lnIndex).Name) .AND. TYPE('_SCREEN.Forms(lnIndex).HWnd') = 'N' .AND. ;
             _SCREEN.Forms(lnIndex).HWnd = lnTopLevelHwnd .AND. _SCREEN.Forms(lnIndex).ShowWindow = 2
            RETURN _SCREEN.Forms(lnIndex)
          ENDIF
      	
      	*! B039071,1 MAH 02/24/2005 [BEGIN]
      	ENDIF
      	*! B039071,1 MAH 02/24/2005 [END]
      ENDFOR
    ENDIF
    
    lnTopLevelHwnd = GetParent(lnTopLevelHwnd)
  ENDDO
    
  RETURN .NULL.
ENDIF

*!*************************************************************
*! Name        : gfFormIsActive
*! Developer   : Ahmad Shoukry Mohammed (ASM)
*! Date        : 12/07/2004
*! Purpose     : To Check if the Form is the active form of the FoxPro
*! Parameters  : The cHostFormName property of the FormSet
*!*************************************************************
*! Returns     : True / False
*!*************************************************************
FUNCTION gfFormIsActive
LPARAMETERS lchfn
LOCAL llRetVal
llRetVal = TYPE("_screen.ActiveForm.Parent") = "O" AND ;
           TYPE("_screen.ActiveForm.Parent.cHostFormName") = "C" and ;
           _screen.ActiveForm.Parent.cHostFormName == lchfn
RETURN llRetVal

*!*************************************************************
*! Name      	: gfFireValidEvent
*! Developer 	: Mahmoud Said (MAH) 
*! Date      	: 05/17/2005
*! Purpose   	: Prevent fire valid when we switch from form to another
*! Tracking #   : E037885,4 MAH 02/17/2005 Remove the host form.
****************************************************************************
*! Parameters : 
*!  loThisForm : Handle to current Form
****************************************************************************
*! Returns 	: .T. if we need to fire the event otherwise .F.
*!*************************************************************
FUNCTION gfFireValidEvent

LPARAMETERS loThisForm

LOCAL loScreen
loScreen = _SCREEN.ActiveForm 

IF TYPE('loScreen') = 'O' .AND. !ISNULL(loScreen) .AND. loScreen.HWnd = loThisForm.HWnd
  RETURN .T.
ELSE
  RETURN .F.
ENDIF


*!*************************************************************
*! Name      	: gfGetIndexColumn
*! Developer 	: Mahmoud Said (MAH) 
*! Date      	: 04/17/2005
*! Purpose   	: Return string to use by FOX index (Asc, Desc) in order to create the same SQL collating order.
*! Tracking #   : E037241,2 MAH 04/17/2005 Browse User Defined Sort.
****************************************************************************
*! Parameters : 
*!  lcValue     : String
*!  llDirection : .T. for Asc and .F. for Desc
****************************************************************************
*! Returns 	: String
*:************************************************************************
*: Modifications : 
*:************************************************************************

FUNCTION gfGetIndexColumn
LPARAMETERS lcValue, llDirection

LOCAL lcReturn
PUBLIC laAscii[256]
IF EMPTY(laAscii)
  laAscii[001] = 001
  laAscii[002] = 002
  laAscii[003] = 003
  laAscii[004] = 004
  laAscii[005] = 005
  laAscii[006] = 006
  laAscii[007] = 007
  laAscii[008] = 008
  laAscii[009] = 009
  laAscii[010] = 010
  laAscii[011] = 011
  laAscii[012] = 012
  laAscii[013] = 013
  laAscii[014] = 014
  laAscii[015] = 015
  laAscii[016] = 016
  laAscii[017] = 017
  laAscii[018] = 018
  laAscii[019] = 019
  laAscii[020] = 020
  laAscii[021] = 021
  laAscii[022] = 022
  laAscii[023] = 023
  laAscii[024] = 024
  laAscii[025] = 025
  laAscii[026] = 026
  laAscii[027] = 027
  laAscii[028] = 028
  laAscii[029] = 028
  laAscii[030] = 030
  laAscii[031] = 031
  laAscii[032] = 032
  laAscii[033] = 033
  laAscii[034] = 034
  laAscii[035] = 035
  laAscii[036] = 036
  laAscii[037] = 037
  laAscii[038] = 038
  laAscii[039] = 039
  laAscii[040] = 040
  laAscii[041] = 041
  laAscii[042] = 042
  laAscii[043] = 043
  laAscii[044] = 044
  laAscii[045] = 045
  laAscii[046] = 046
  laAscii[047] = 047
  laAscii[048] = 048
  laAscii[049] = 133
  laAscii[050] = 134
  laAscii[051] = 135
  laAscii[052] = 136
  laAscii[053] = 137
  laAscii[054] = 138
  laAscii[055] = 139
  laAscii[056] = 140
  laAscii[057] = 141
  laAscii[058] = 142
  laAscii[059] = 049
  laAscii[060] = 050
  laAscii[061] = 051
  laAscii[062] = 052
  laAscii[063] = 053
  laAscii[064] = 054
  laAscii[065] = 055
  laAscii[066] = 143
  laAscii[067] = 159
  laAscii[068] = 162
  laAscii[069] = 166
  laAscii[070] = 167
  laAscii[071] = 178
  laAscii[072] = 179
  laAscii[073] = 182
  laAscii[074] = 183
  laAscii[075] = 194
  laAscii[076] = 195
  laAscii[077] = 198
  laAscii[078] = 199
  laAscii[079] = 202
  laAscii[080] = 206
  laAscii[081] = 220
  laAscii[082] = 221
  laAscii[083] = 224
  laAscii[084] = 225
  laAscii[085] = 229
  laAscii[086] = 230
  laAscii[087] = 241
  laAscii[088] = 242
  laAscii[089] = 245
  laAscii[090] = 246
  laAscii[091] = 252
  laAscii[092] = 056
  laAscii[093] = 057
  laAscii[094] = 058
  laAscii[095] = 059
  laAscii[096] = 060
  laAscii[097] = 061
  laAscii[098] = 144
  laAscii[099] = 160
  laAscii[100] = 161
  
  laAscii[101] = 165
  laAscii[102] = 168
  laAscii[103] = 177
  laAscii[104] = 180
  laAscii[105] = 181
  laAscii[106] = 184
  laAscii[107] = 193
  laAscii[108] = 196
  laAscii[109] = 197
  laAscii[110] = 200
  laAscii[111] = 201
  laAscii[112] = 205
  laAscii[113] = 219
  laAscii[114] = 222
  laAscii[115] = 223
  laAscii[116] = 226
  laAscii[117] = 228
  laAscii[118] = 231
  laAscii[119] = 240
  laAscii[120] = 243
  laAscii[121] = 244
  laAscii[122] = 247
  laAscii[123] = 251
  laAscii[124] = 062
  laAscii[125] = 063
  laAscii[126] = 064
  laAscii[127] = 065
  laAscii[128] = 066
  laAscii[129] = 067
  laAscii[130] = 068
  laAscii[131] = 069
  laAscii[132] = 070
  laAscii[133] = 071
  laAscii[134] = 072
  laAscii[135] = 073
  laAscii[136] = 074
  laAscii[137] = 075
  laAscii[138] = 076
  laAscii[139] = 077
  laAscii[140] = 078
  laAscii[141] = 079
  laAscii[142] = 080
  laAscii[143] = 081
  laAscii[144] = 082
  laAscii[145] = 083
  laAscii[146] = 084
  laAscii[147] = 085
  laAscii[148] = 086
  laAscii[149] = 087
  laAscii[150] = 088
  laAscii[151] = 089
  laAscii[152] = 090
  laAscii[153] = 091
  laAscii[154] = 092
  laAscii[155] = 093
  laAscii[156] = 094
  laAscii[157] = 095
  laAscii[158] = 096
  laAscii[159] = 097
  laAscii[160] = 098
  laAscii[161] = 099
  laAscii[162] = 100
  laAscii[163] = 101
  laAscii[164] = 102
  laAscii[165] = 103
  laAscii[166] = 104
  laAscii[167] = 105
  laAscii[168] = 106
  laAscii[169] = 107
  laAscii[170] = 108
  laAscii[171] = 109
  laAscii[172] = 110
  laAscii[173] = 111
  laAscii[174] = 112
  laAscii[175] = 113
  laAscii[176] = 114
  laAscii[177] = 115
  laAscii[178] = 116
  laAscii[179] = 117
  laAscii[180] = 118
  laAscii[181] = 119
  laAscii[182] = 120
  laAscii[183] = 121
  laAscii[184] = 122
  laAscii[185] = 123
  laAscii[186] = 124
  laAscii[187] = 125
  laAscii[188] = 126
  laAscii[189] = 127
  laAscii[190] = 128
  laAscii[191] = 129
  laAscii[192] = 130
  laAscii[193] = 146
  laAscii[194] = 147
  laAscii[195] = 150
  laAscii[196] = 151
  laAscii[197] = 154
  laAscii[198] = 155
  laAscii[199] = 158
  laAscii[200] = 163
  
  laAscii[201] = 170
  laAscii[202] = 171
  laAscii[203] = 174
  laAscii[204] = 175
  laAscii[205] = 186
  laAscii[206] = 187
  laAscii[207] = 190
  laAscii[208] = 191
  laAscii[209] = 253
  laAscii[210] = 203
  laAscii[211] = 208
  laAscii[212] = 209
  laAscii[213] = 212
  laAscii[214] = 213
  laAscii[215] = 216
  laAscii[216] = 131
  laAscii[217] = 217
  laAscii[218] = 232
  laAscii[219] = 235
  laAscii[220] = 236
  laAscii[221] = 239
  laAscii[222] = 248
  laAscii[223] = 255
  laAscii[224] = 227
  laAscii[225] = 145
  laAscii[226] = 148
  laAscii[227] = 149
  laAscii[228] = 152
  laAscii[229] = 153
  laAscii[230] = 156
  laAscii[231] = 157
  laAscii[232] = 164
  laAscii[233] = 169
  laAscii[234] = 172
  laAscii[235] = 173
  laAscii[236] = 176
  laAscii[237] = 185
  laAscii[238] = 188
  laAscii[239] = 189
  laAscii[240] = 192
  laAscii[241] = 254
  laAscii[242] = 204
  laAscii[243] = 207
  laAscii[244] = 210
  laAscii[245] = 211
  laAscii[246] = 214
  laAscii[247] = 215
  laAscii[248] = 132
  laAscii[249] = 218
  laAscii[250] = 233
  laAscii[251] = 234
  laAscii[252] = 237
  laAscii[253] = 238
  laAscii[254] = 249
  laAscii[255] = 256
  laAscii[256] = 250
ENDIF

LOCAL lnChar

lcReturn = ''
FOR lnChar = 1 TO LEN(lcValue)
  IF llDirection
    lcReturn = lcReturn + CHR(laAscii[ASC(SUBSTR(lcValue, lnChar, 1)) + 1] - 1)
  ELSE
    *! E037241,2 MAH 07/17/2005 [BEGIN]
    *-- lcReturn = lcReturn + CHR(laAscii[256 - laAscii[ASC(SUBSTR(lcValue, lnChar, 1)) + 1] + 1] - 1)
    lcReturn = lcReturn + CHR(255 - laAscii[ASC(SUBSTR(lcValue, lnChar, 1)) + 1] - 1)
    *! E037241,2 MAH 07/17/2005 [END]
  ENDIF
ENDFOR

RETURN lcReturn

*!*************************************************************
*! Name      	: gfStrToExp
*! Developer 	: Mahmoud Said (MAH) 
*! Date      	: 04/17/2005
*! Purpose   	: 
*! Tracking #   : B039435,1 MAH Fix bug of there is a quote in the field value.
****************************************************************************
*! Parameters : 
*!  lcValue        : String
*!  lcDBEngine     : Database engine type
*!  llEnglishQuery : Show expression in englis query
****************************************************************************
*! Returns 	: Expression
*:************************************************************************
*: Modifications : 
*:************************************************************************
FUNCTION gfStrToExp
LPARAMETERS lcValue, lcDBEngine, llEnglishQuery

IF llEnglishQuery
  lcValue =  '"' + lcValue + '"'
ELSE
  lcDBEngine = IIF(TYPE('lcDBEngine') = 'C', lcDBEngine, oAriaApplication.cNativeDBID)
  
  *-- Note: not supported case: there is ', " and ]
  
  DO CASE  
    CASE ALLTRIM(lcDBEngine) == oAriaApplication.cNativeDBID
      IF !("'" $ lcValue)
        lcValue =  "'" + lcValue + "'"
      ELSE
        IF !('"' $ lcValue)
          lcValue =  '"' + lcValue + '"'
        ELSE
          lcValue =  "[" + lcValue + "]"
        ENDIF
      ENDIF

    CASE ALLTRIM(lcDBEngine) == oAriaApplication.cSQLDBID
      lcValue = "'" + STRTRAN(lcValue, "'", "''") + "'"
  ENDCASE
ENDIF

RETURN lcValue

*!*************************************************************
*! Name        : gfOpenTable
*! Developer   : Ahmad Shoukry Mohammed (ASM)
*! Date        : 03/28/2005
*! Purpose     : Create a Remote Table Object if Trying to open Fox or Sql Table Defined in the Sysem Files
*! Tracking #  : N039155,1 ASM 03/28/2005 Converting Programs and Screens to work With SQL.
****************************************************************************
*! Parameters  : File Name, Index Tag, Open Mode ("EX" or "SH"), Alias Name, Force Open
*!*************************************************************
*! Returns            :  True  ----> If passed file is open by this function
*!                       False ----> If passed file is already open.
*!*************************************************************
FUNCTION gfOpenTable
LPARAMETERS NFILE,lcIndex,MODE,lcAliasNam,llForceOp,lcCompId,llFastTable
LOCAL lnTable, lnL

lcAliasNam = IIF(TYPE('lcAliasNam')#'C' OR EMPTY(lcAliasNam),JUSTSTEM(NFILE),lcAliasNam)
lnTable = gfGetRemoteTable(SET("Datasession"),lcAliasNam)

IF lnTable=0 && No Remote Table Object was Found

  lnL = ALEN(oAriaApplication.laRemoteTable)
  IF TYPE('oAriaApplication.laRemoteTable[lnL]')='O'
    lnL = lnL + 1 
    DIMENSION oAriaApplication.laRemoteTable[lnL]
  ENDIF
  *WAIT WINDOW NFILE+', '+lcIndex+', '+PADR(SET("Datasession"),10) TIMEOUT 0.5
  lcIndex = IIF(EMPTY(lcIndex),'',JUSTSTEM(lcIndex))
  oAriaApplication.laRemoteTable[lnL] = CREATEOBJECT("RemoteTable",JUSTSTEM(NFILE),lcIndex,lcAliasNam,SET("Datasession"),lcCompId,llFastTable)
  IF TYPE('oAriaApplication.laRemoteTable[lnL]')<>'O'
    lnL = MAX(lnL - 1,1)
    DIMENSION oAriaApplication.laRemoteTable[lnL]
    RETURN gfOpenFile(NFILE,lcIndex,MODE,lcAliasNam,llForceOp)
  ENDIF

ELSE && a Remote Table Object Already Exist

  IF !EMPTY(lcIndex)
    *WAIT WINDOW lcIndex TIMEOUT 0.5
    oAriaApplication.laRemoteTable[lnTable].SetOrder(JUSTSTEM(lcIndex))
  ENDIF

ENDIF

RETURN 
*--end of gfOpenTable

*!*************************************************************
*! Name        : gfGetRemoteTable
*! Developer   : Ahmad Shoukry Mohammed (ASM)
*! Date        : 03/28/2005
*! Purpose     : Searches the AriaApplication laRemoteTable Array for an Existing Object
*! Tracking #  : N039155,1 ASM 03/28/2005 Converting Programs and Screens to work With SQL.
****************************************************************************
*! Parameters  : DataSession, Alias Name
*!*************************************************************
*! Returns            :  0  ----> If Remote Table Object Not Found in the laRemoteTable Array
*!                       n  ----> Element No. of laRemoteTable in which the Remote Table Exists
*!*************************************************************
FUNCTION gfGetRemoteTable
LPARAMETERS lnDataSessionID, lcAlias
LOCAL lnC, lnL, lnV

lnV=0
lnL = ALEN(oAriaApplication.laRemoteTable)
FOR lnC=1 TO lnL
  IF TYPE('oAriaApplication.laRemoteTable[lnC]')='O' AND ;
      oAriaApplication.laRemoteTable[lnC].lnDataSession == lnDataSessionID AND ;
      UPPER(oAriaApplication.laRemoteTable[lnC].lcCursorView)==UPPER(lcAlias)
     lnV = lnC
     EXIT
  ENDIF
NEXT

RETURN lnV
*--end of gfGetRemoteTable

*!*************************************************************
*! Name        : gfGetRemoteProp
*! Developer   : Ahmad Shoukry Mohammed (ASM)
*! Date        : 03/28/2005
*! Purpose     : Searches the AriaApplication laRemoteTable Array for an Existing Object
*!               And if found Returns the value of a Remote Table Property
*! Tracking #  : N039155,1 ASM 03/28/2005 Converting Programs and Screens to work With SQL.
****************************************************************************
*! Parameters  : Property Name, Alias Name
*!*************************************************************
*! Returns            :  0  ----> If Remote Table Object Not Found in the laRemoteTable Array
*!                       n  ----> Element No. of laRemoteTable in which the Remote Table Exists
*!*************************************************************
FUNCTION gfGetRemoteProp
LPARAMETERS lcProp, lcAlias
LOCAL lcMacro, lRetValue

lcAlias = IIF(TYPE('lcAlias')='C',lcAlias,ALIAS())
lnTable = gfGetRemoteTable(SET("Datasession"),lcAlias)

IF lnTable<>0 && Remote Table Object was Found
  lcMacro = 'oAriaApplication.laRemoteTable[lnTable].'+lcProp
  lRetValue = &lcMacro
ELSE
  lRetValue = .F.
ENDIF

RETURN lRetValue
*--end of gfGetRemoteProp


*!*************************************************************
*! Name        : gfGetAlias
*! Developer   : Ahmad Shoukry Mohammed (ASM)
*! Date        : 03/28/2005
*! Purpose     : Extracts Alias Name from a Statement
*! Tracking #  : N039155,1 ASM 03/28/2005 Converting Programs and Screens to work With SQL.
****************************************************************************
*! Parameters  : Statment
*!*************************************************************
*! Returns     :  Alias Name
*!*************************************************************
FUNCTION gfGetAlias
LPARAMETERS lcStat
LOCAL lcAlias
DO CASE
  CASE TYPE('lcStat')='C' AND (' IN ' $ UPPER(lcStat) OR UPPER(lcStat)='IN ')
    *! B040241,1 ASM 05/18/2006 Bug in gfReplace, fixed in gfGetAlias [Start]
    *lcAlias = ALLTRIM(SUBSTR(lcStat,AT(' IN ',UPPER(lcStat))+4))
    lcAlias = ALLTRIM(SUBSTR(lcStat,RAT(' IN ',UPPER(lcStat))+4))
    *lcStat = LEFT(lcStat,AT(' IN ',UPPER(lcStat))-1)
    IF "'" $ lcAlias or '"' $ lcAlias or "]" $ lcAlias
       lcAlias = ALIAS()
    ELSE
      lcStat = LEFT(lcStat,RAT(' IN ',UPPER(lcStat))-1)
    ENDIF
    *! B040241,1 ASM 05/18/2006 Bug in gfReplace, fixed in gfGetAlias [End]
  OTHERWISE
    lcAlias = ALIAS()
ENDCASE

RETURN lcAlias
*--end of gfGetAlias

*!*************************************************************
*! Name        : gfSetOrder
*! Developer   : Ahmad Shoukry Mohammed (ASM)
*! Date        : 03/28/2005
*! Purpose     : Called Instead of the Set Order To Command
*! Tracking #  : N039155,1 ASM 03/28/2005 Converting Programs and Screens to work With SQL.
****************************************************************************
*! Parameters  : Index Tag
*!*************************************************************
*! Returns     :  None
*!*************************************************************
FUNCTION gfSetOrder
LPARAMETERS lcStat
LOCAL lnTable, lcAlias, lcOldAlias

lcOldAlias = ALIAS()
lcAlias = gfGetAlias(@lcStat)
lnTable = gfGetRemoteTable(SET("Datasession"),lcAlias)

IF lnTable<>0 && Remote Table Object was Found
  oAriaApplication.laRemoteTable[lnTable].SetOrder(lcStat)
ELSE
  SELECT (lcAlias)
  IF TYPE('lcStat')='C' AND !EMPTY(lcStat)
    SET ORDER TO &lcStat
  ELSE
    SET ORDER TO
  ENDIF
ENDIF

IF !EMPTY(lcOldAlias)
  SELECT (lcOldAlias)
ENDIF

RETURN
*--end of gfSetOrder

*!*************************************************************
*! Name        : gfSeek
*! Developer   : Ahmad Shoukry Mohammed (ASM)
*! Date        : 03/28/2005
*! Purpose     : Called Instead of the Seek Function
*! Tracking #  : N039155,1 ASM 03/28/2005 Converting Programs and Screens to work With SQL.
****************************************************************************
*! Parameters  : Expression, Alias, Tag Name, Fast Seek
*!*************************************************************
*! Returns     :  Seek Result
*!*************************************************************
FUNCTION gfSeek
LPARAMETERS lcExp, lcAlias, lcTagName, llFastSeek
LOCAL lnTable

lcAlias = IIF(TYPE('lcAlias')='C',lcAlias,ALIAS())
lnTable = gfGetRemoteTable(SET("Datasession"),lcAlias)

IF lnTable<>0 && Remote Table Object was Found
  RETURN oAriaApplication.laRemoteTable[lnTable].Seek(lcExp,lcTagName,llFastSeek)
ELSE
  LOCAL lcParam
  lcParam = 'lcExp' + IIF(!EMPTY(lcAlias), ',lcAlias' + IIF(!EMPTY(lcTagName), ',lcTagName', ''), '')
  
  RETURN SEEK(&lcParam.)
ENDIF

RETURN
*--end of gfSeek


*!*************************************************************
*! Name        : gfRelation
*! Developer   : Ahmad Shoukry Mohammed (ASM)
*! Date        : 03/28/2005
*! Purpose     : Called Within the Set Relation To command
*! Tracking #  : N039155,1 ASM 03/28/2005 Converting Programs and Screens to work With SQL.
****************************************************************************
*! Parameters  : Expression, Alias, Additive
*!*************************************************************
*! Returns     :  The Expression Paramater Value
*!*************************************************************
FUNCTION gfRelation
LPARAMETERS lcExp, lcAlias, llAdditive
LOCAL lnTable

lnTable = gfGetRemoteTable(SET("Datasession"),lcAlias)
IF lnTable<>0 && Remote Table Object was Found
  oAriaApplication.laRemoteTable[lnTable].Seek(EVALUATE(lcExp))
  IF llAdditive
    SET RELATION TO &lcExp INTO &lcAlias ADDITIVE
  ELSE
    SET RELATION TO &lcExp INTO &lcAlias
  ENDIF
ENDIF

RETURN EVALUATE(lcExp)
*--end of gfRelation


*!*************************************************************
*! Name        : gfSqlRun
*! Developer   : Ahmad Shoukry Mohammed (ASM)
*! Date        : 03/28/2005
*! Purpose     : Called Instead of the Locate Command
*! Tracking #  : N039155,1 ASM 03/28/2005 Converting Programs and Screens to work With SQL.
****************************************************************************
*! Parameters  : SQL Select Statement, Cursor Receiving Results, Ignore Creating Index
*!*************************************************************
*! Returns     :  False if Falied to Create the Cursor
*!*************************************************************
FUNCTION gfSqlRun
LPARAMETERS lcSqlStatement, lcAlias, llNoIndex, lcTempAlias
LOCAL lnTable

lcAlias = IIF(TYPE('lcAlias')='C',lcAlias,ALIAS())
lcTempAlias = IIF(TYPE('lcTempAlias')='C',lcTempAlias,lcAlias)
lnTable = gfGetRemoteTable(SET("Datasession"),lcAlias)

IF lnTable<>0 && Remote Table Object was Found
  RETURN oAriaApplication.laRemoteTable[lnTable].SqlRun(lcSqlStatement, lcTempAlias, llNoIndex)
ELSE
  RETURN .F.
ENDIF

RETURN
*--end of SqlRun

*!*************************************************************
*! Name        : gfRecNo
*! Developer   : Ahmad Shoukry Mohammed (ASM)
*! Date        : 03/28/2005
*! Purpose     : Called Instead of the Recno Function
*! Tracking #  : N039155,1 ASM 03/28/2005 Converting Programs and Screens to work With SQL.
****************************************************************************
*! Parameters  : Alias
*!*************************************************************
*! Returns     :  RecNo Result
*!*************************************************************
FUNCTION gfRecNo
LPARAMETERS lcAlias
LOCAL lnTable

lcAlias = IIF(TYPE('lcAlias')='C',lcAlias,ALIAS())
lnTable = gfGetRemoteTable(SET("Datasession"),lcAlias)

IF lnTable<>0 && Remote Table Object was Found
  RETURN oAriaApplication.laRemoteTable[lnTable].RecNo()
ELSE
  RETURN RECNO(lcAlias)
ENDIF

RETURN
*--end of gfRecNo


*!*************************************************************
*! Name        : gfGoRec
*! Developer   : Ahmad Shoukry Mohammed (ASM)
*! Date        : 03/28/2005
*! Purpose     : Called Instead of the Go / Goto Statement
*! Tracking #  : N039155,1 ASM 03/28/2005 Converting Programs and Screens to work With SQL.
****************************************************************************
*! Parameters  : Statement
*!*************************************************************
*! Returns     : None
*!*************************************************************
FUNCTION gfGoRec
LPARAMETERS lcStat
LOCAL lnTable, lcAlias, lcOldAlias, lnRecNum

lcOldAlias = ALIAS()
lcAlias = gfGetAlias(@lcStat)
lnTable = gfGetRemoteTable(SET("Datasession"),lcAlias)

IF lnTable<>0 && Remote Table Object was Found
*khm1
  *oAriaApplication.laRemoteTable[lnTable].GoRec(&lcStat)
  oAriaApplication.laRemoteTable[lnTable].GoRec(lcStat)
*khm1  
ELSE
  SELECT (lcAlias)
  lnRecNum = IIF(TYPE('lcStat')='N',lcStat,VAL(lcStat))
  GO lnRecNum
ENDIF

IF !EMPTY(lcOldAlias)
  SELECT (lcOldAlias)
ENDIF

RETURN
*--end of gfGoRec


*!*************************************************************
*! Name        : gfGoTop
*! Developer   : Ahmad Shoukry Mohammed (ASM)
*! Date        : 03/28/2005
*! Purpose     : Called Instead of the Go Top Statement
*! Tracking #  : N039155,1 ASM 03/28/2005 Converting Programs and Screens to work With SQL.
****************************************************************************
*! Parameters  : Statemnet
*!*************************************************************
*! Returns     : None (or True/False if Remote Table)
*!*************************************************************
FUNCTION gfGoTop
LPARAMETERS lcStat
LOCAL lnTable, lcAlias, lcOldAlias, llRetVal

llRetVal = .T.
lcOldAlias = ALIAS()
lcAlias = gfGetAlias(@lcStat)
lnTable = gfGetRemoteTable(SET("Datasession"),lcAlias)

IF lnTable<>0 && Remote Table Object was Found
  llRetVal = oAriaApplication.laRemoteTable[lnTable].GoTop()
ELSE
  SELECT (lcAlias)
  GO TOP
ENDIF

IF !EMPTY(lcOldAlias)
  SELECT (lcOldAlias)
ENDIF

RETURN llRetVal
*--end of gfGoTop


*!*************************************************************
*! Name        : gfGoBottom
*! Developer   : Ahmad Shoukry Mohammed (ASM)
*! Date        : 03/28/2005
*! Purpose     : Called Instead of the Go Bottom Statement
*! Tracking #  : N039155,1 ASM 03/28/2005 Converting Programs and Screens to work With SQL.
****************************************************************************
*! Parameters  : Statemnet
*!*************************************************************
*! Returns     : None (or True/False if Remote Table)
*!*************************************************************
FUNCTION gfGoBottom
LPARAMETERS lcStat
LOCAL lnTable, lcAlias, lcOldAlias

llRetVal = .T.
lcOldAlias = ALIAS()
lcAlias = gfGetAlias(@lcStat)
lnTable = gfGetRemoteTable(SET("Datasession"),lcAlias)

IF lnTable<>0 && Remote Table Object was Found
  llRetVal = oAriaApplication.laRemoteTable[lnTable].GoBottom()
ELSE
  SELECT (lcAlias)
  GO BOTTOM
ENDIF

IF !EMPTY(lcOldAlias)
  SELECT (lcOldAlias)
ENDIF

RETURN llRetVal
*--end of gfGoBottom


*!*************************************************************
*! Name        : gfGoNext
*! Developer   : Ahmad Shoukry Mohammed (ASM)
*! Date        : 03/28/2005
*! Purpose     : Called Instead of Skip Command
*! Tracking #  : N039155,1 ASM 03/28/2005 Converting Programs and Screens to work With SQL.
****************************************************************************
*! Parameters  : Statemnet
*!*************************************************************
*! Returns     : None (or True/False if Remote Table)
*!*************************************************************
FUNCTION gfGoNext
LPARAMETERS lcStat
LOCAL lnTable, lcAlias, lcOldAlias, llRetVal

llRetVal = .T.
lcOldAlias = ALIAS()
lcAlias = gfGetAlias(@lcStat)
lnTable = gfGetRemoteTable(SET("Datasession"),lcAlias)

IF lnTable<>0 && Remote Table Object was Found
  llRetVal = oAriaApplication.laRemoteTable[lnTable].GoNext()
ELSE
  SELECT (lcAlias)
  SKIP
ENDIF

IF !EMPTY(lcOldAlias)
  SELECT (lcOldAlias)
ENDIF

RETURN llRetVal
*--end of gfGoNext


*!*************************************************************
*! Name        : gfGoPrevious
*! Developer   : Ahmad Shoukry Mohammed (ASM)
*! Date        : 03/28/2005
*! Purpose     : Called Instead of Skip -1 Command
*! Tracking #  : N039155,1 ASM 03/28/2005 Converting Programs and Screens to work With SQL.
****************************************************************************
*! Parameters  : Statemnet
*!*************************************************************
*! Returns     : None (or True/False if Remote Table)
*!*************************************************************
FUNCTION gfGoPrevious
LPARAMETERS lcStat
LOCAL lnTable, lcAlias, lcOldAlias, llRetVal

llRetVal = .T.
lcOldAlias = ALIAS()
lcAlias = gfGetAlias(@lcStat)
lnTable = gfGetRemoteTable(SET("Datasession"),lcAlias)

IF lnTable<>0 && Remote Table Object was Found
  llRetVal = oAriaApplication.laRemoteTable[lnTable].GoPrevious()
ELSE
  SELECT (lcAlias)
  SKIP -1
ENDIF

IF !EMPTY(lcOldAlias)
  SELECT (lcOldAlias)
ENDIF

RETURN llRetVal
*--end of gfGoPrevious


*!*************************************************************
*! Name        : gfAppend
*! Developer   : Ahmad Shoukry Mohammed (ASM)
*! Date        : 03/28/2005
*! Purpose     : Called Instead of the Append Blank Statement
*! Tracking #  : N039155,1 ASM 03/28/2005 Converting Programs and Screens to work With SQL.
****************************************************************************
*! Parameters  : None
*!*************************************************************
*! Returns     : None
*!*************************************************************
FUNCTION gfAppend

*N039550,1 WSH 09/18/2005 add new parameter to append from memvar. [Start]
*LPARAMETERS lcStat
LPARAMETERS lcStat, llFormMemVar
*N039550,1 WSH 09/18/2005 [End]

LOCAL lnTable, lcAlias, lcOldAlias

lcOldAlias = ALIAS()
lcAlias = gfGetAlias(@lcStat)
lnTable = gfGetRemoteTable(SET("Datasession"),lcAlias)

IF lnTable<>0 AND !oAriaApplication.laRemoteTable[lnTable].llNative && Remote Table Object was Found and the Table is SQL
  SELECT (oAriaApplication.laRemoteTable[lnTable].lcCursorUpdate)
  APPEND BLANK
  
  *N039550,1 WSH 09/18/2005 add new parameter to append from memvar. [Start]
  IF llFormMemVar
    GATHER MEMVAR MEMO
  ENDIF
  *N039550,1 WSH 09/18/2005 [End]
  
ENDIF
SELECT (lcAlias)
APPEND BLANK 

*N039550,1 WSH 09/18/2005 add new parameter to append from memvar. [Start]
IF llFormMemVar
  GATHER MEMVAR MEMO
ENDIF
*N039550,1 WSH 09/18/2005 [End]

IF !EMPTY(lcOldAlias)
  SELECT (lcOldAlias)
ENDIF

RETURN
*--end of gfAppend


*!*************************************************************
*! Name        : gfReplace
*! Developer   : Ahmad Shoukry Mohammed (ASM)
*! Date        : 03/28/2005
*! Purpose     : Called Instead of the Replace Statement
*! Tracking #  : N039155,1 ASM 03/28/2005 Converting Programs and Screens to work With SQL.
****************************************************************************
*! Parameters  : Replace Statement
*!*************************************************************
*! Returns     : None
*!*************************************************************
FUNCTION gfReplace
LPARAMETERS lcStat
LOCAL lnTable, lcAlias, lcOldAlias

lcOldAlias = ALIAS()
lcAlias = gfGetAlias(@lcStat)
lnTable = gfGetRemoteTable(SET("Datasession"),lcAlias)

IF lnTable<>0 AND !oAriaApplication.laRemoteTable[lnTable].llNative && Remote Table Object was Found and the Table is SQL
  IF TYPE('lcStat')='C' AND !EMPTY(lcStat)
    oAriaApplication.laRemoteTable[lnTable].Replace(lcStat)
  ELSE
    oAriaApplication.laRemoteTable[lnTable].Replace()
  ENDIF
ENDIF
SELECT (lcAlias)

IF TYPE('lcStat')='C' AND !EMPTY(lcStat)
  REPLACE &lcStat
ENDIF

IF !EMPTY(lcOldAlias)
  SELECT (lcOldAlias)
ENDIF

RETURN
*--end of gfReplace


*!*************************************************************
*! Name        : gfDelete
*! Developer   : Ahmad Shoukry Mohammed (ASM)
*! Date        : 03/28/2005
*! Purpose     : Called Instead of the Replace Statement
*! Tracking #  : N039155,1 ASM 03/28/2005 Converting Programs and Screens to work With SQL.
****************************************************************************
*! Parameters  : Delete Statement
*!*************************************************************
*! Returns     : None
*!*************************************************************
FUNCTION gfDelete
LPARAMETERS lcStat
LOCAL lnTable, lcAlias, lcOldAlias

lcOldAlias = ALIAS()
lcAlias = gfGetAlias(@lcStat)
lnTable = gfGetRemoteTable(SET("Datasession"),lcAlias)

SELECT (lcAlias)
IF lnTable<>0 AND !oAriaApplication.laRemoteTable[lnTable].llNative && Remote Table Object was Found and the Table is SQL
  IF TYPE('lcStat')='C' AND !EMPTY(lcStat)
    SCAN &lcStat
      oAriaApplication.laRemoteTable[lnTable].Delete()
    ENDSCAN
  ELSE
    oAriaApplication.laRemoteTable[lnTable].Delete()
  ENDIF
ENDIF

IF TYPE('lcStat')='C' AND !EMPTY(lcStat)
  DELETE &lcStat
ELSE
  DELETE
ENDIF

IF !EMPTY(lcOldAlias)
  SELECT (lcOldAlias)
ENDIF

RETURN
*--end of gfDelete


*!*************************************************************
*! Name        : gfTableUpdate
*! Developer   : Ahmad Shoukry Mohammed (ASM)
*! Date        : 03/28/2005
*! Purpose     : Called Instead of the TableUpdate Function
*! Tracking #  : N039155,1 ASM 03/28/2005 Converting Programs and Screens to work With SQL.
****************************************************************************
*! Parameters  : Paremeters of the TableUpdate Function
*!*************************************************************
*! Returns     : TableUpdate Function Result
*!*************************************************************
FUNCTION gfTableUpdate
LPARAMETERS llForce, lcAlias, lcError
LOCAL lnTable, lcOldAlias, lcTranCode, llRetValue

lcOldAlias = ALIAS()
lcAlias = IIF(TYPE('lcAlias')='C',lcAlias,ALIAS())
lnTable = gfGetRemoteTable(SET("Datasession"),lcAlias)

llRetValue = .T.
IF lnTable<>0 && Remote Table Object was Found
  IF TYPE('llForce')<>'C'
    lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr,3,'')
    IF TYPE('lcTranCode') = 'N'
      llRetValue = .F.
    ENDIF
  ELSE
    lcTranCode = llForce
  ENDIF
  llRetValue = llRetValue AND oAriaApplication.laRemoteTable[lnTable].TableUpdate(lcTranCode)
  IF TYPE('llForce')<>'C' AND TYPE('lcTranCode')='C'
    IF llRetValue
      llRetValue = (oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode)=1)  AND llRetValue
    ELSE
      llRetValue = (oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)=1)
    ENDIF
  ENDIF
ELSE
  LOCAL lcParam
  *! B609140,1 MMT 02/10/2010 Fix bug of error while cancelling Sales order [Start]  
  *lcParam = IIF(TYPE('llForce') = 'L', PADR(llForce,3), '.T.') + ', '+;
            IIF(!EMPTY(lcAlias), lcAlias ,' .F.')+ ', '+;
            IIF(!EMPTY(lcError), lcError,' .F.')
  *llRetValue = TableUpdate(&lcParam.)            
  IF CURSORGETPROP("Buffering",lcAlias) <> 1              
	lcParam = '.F.,'          
	lcParam = lcParam +  IIF(TYPE('llForce') = 'L', IIF(llForce,'.T.','.F.'), '.T.')+ ', '+;
			IIF(!EMPTY(lcAlias), "'"+lcAlias+"'" ,' .F.')+ ', '+;
			IIF(!EMPTY(lcError), lcError,' .F.')
	llRetValue = TableUpdate(&lcParam.)			
  ENDIF	
  *! B609140,1 MMT 02/10/2010 Fix bug of error while cancelling Sales order [End]    
  

ENDIF

IF !EMPTY(lcOldAlias)
  SELECT (lcOldAlias)
ENDIF

RETURN llRetValue
*--end of gfTableUpdate


*!*************************************************************
*! Name        : gfCloseTable
*! Developer   : Ahmad Shoukry Mohammed (ASM)
*! Date        : 03/28/2005
*! Purpose     : Called Instead of the Use / Use in statemnet
*! Tracking #  : N039155,1 ASM 03/28/2005 Converting Programs and Screens to work With SQL.
****************************************************************************
*! Parameters  : Alias
*!*************************************************************
*! Returns     : None
*!*************************************************************
FUNCTION gfCloseTable
LPARAMETERS lcAlias
LOCAL lnTable, lnLen

IF TYPE('lcAlias')='C' AND 'IN ' $ lcAlias
  lcAlias = gfGetAlias(@lcAlias)
ELSE
  lcAlias = IIF(TYPE('lcAlias')='C',lcAlias,ALIAS())
ENDIF
lnTable = gfGetRemoteTable(SET("Datasession"),lcAlias)

IF lnTable<>0 && Remote Table Object was Found
  oAriaApplication.laRemoteTable[lnTable] = Null
  ADEL(oAriaApplication.laRemoteTable,lnTable)
  lnLen = MAX(ALEN(oAriaApplication.laRemoteTable)-1,1)
  DIMENSION oAriaApplication.laRemoteTable[lnLen]
ENDIF

RETURN
*--end of gfCloseTable 


FUNCTION gfGetTaskListID
LOCAL lnSelected
lnSelected = SELECT()

LOCAL lcReturn 
lcReturn = ''

DO CASE
  CASE syschdul.cContType = 'O' .OR. syschdul.cContType = 'T'
    LOCAL lcKey
    lcKey = ALLTRIM(syschdul.cContType) + ALLTRIM(syschdul.cseqnumber) 
    gfOpenFile(oAriaApplication.DataDir+'ORDHDR','ORDHDR',"SH")
    = SEEK(lcKey, 'ORDHDR', 'ORDHDR')
    lcReturn = ORDHDR.Account
    
  CASE syschdul.cContType = 'P'
    LOCAL lcWhere, lcSQL, lcTempCursorName
    lcWhere = "WHERE [cbusdocu] = '" + ALLTRIM(syschdul.cContType) + "' and [cstytype] = 'P' and [po] = '" + ALLTRIM(syschdul.cseqnumber) + "'"
    lcSQL   = "SELECT vendor FROM poshdr WITH (INDEX = poshdr) " + lcWhere
    
    lcTempCursorName = gfTempName()
    LOCAL lnSQLRunResult
    lnSQLRunResult = oAriaApplication.RemoteCompanyData.SQLRun(lcSQL, lcTempCursorName, ;
                     '', oAriaApplication.ActiveCompanyConStr, 3, 'SAVE', SET("Datasession"))

    IF lnSQLRunResult # 1
      *-- No need due to the error apears while refreshing forever
      *-- oAriaApplication.RemoteCompanyData.CheckRetResult('SQLRun', lnSQLRunResult, .T.)
    ELSE
      lcReturn = &lcTempCursorName..vendor
    ENDIF
  
  OTHERWISE
    lcReturn = syschdul.ccont_id
ENDCASE

SELECT(lnSelected)
RETURN lcReturn 


FUNCTION gfGetTaskListName
LOCAL lnSelected
lnSelected = SELECT()

LOCAL lcReturn 
lcReturn = ''

DO CASE
  CASE syschdul.cContType = 'O' .OR. syschdul.cContType = 'T'
    LOCAL lcKey
    lcKey = ALLTRIM(syschdul.cContType) + ALLTRIM(syschdul.cseqnumber) 
    gfOpenFile(oAriaApplication.DataDir+'ORDHDR','ORDHDR',"SH")
    = SEEK(lcKey, 'ORDHDR', 'ORDHDR')
    lcReturn = ORDHDR.Account
    lcReturn = LOOKUP(CUSTOMER.BtName,"M"+ALLTRIM(lcReturn),CUSTOMER.ACCOUNT,"CUSTOMER")
    
  CASE syschdul.cContType = 'P'
    LOCAL lcWhere, lcSQL, lcTempCursorName
    lcWhere = "WHERE [cbusdocu] = '" + ALLTRIM(syschdul.cContType) + "' and [cstytype] = 'P' and [po] = '" + ALLTRIM(syschdul.cseqnumber) + "'"
    lcSQL   = "SELECT vendor FROM poshdr WITH (INDEX = poshdr) " + lcWhere
    
    lcTempCursorName = gfTempName()
    LOCAL lnSQLRunResult
    lnSQLRunResult = oAriaApplication.RemoteCompanyData.SQLRun(lcSQL, lcTempCursorName, ;
                     '', oAriaApplication.ActiveCompanyConStr, 3, 'SAVE', SET("Datasession"))

    IF lnSQLRunResult # 1
      *-- No need due to the error apears while refreshing forever
      *-- oAriaApplication.RemoteCompanyData.CheckRetResult('SQLRun', lnSQLRunResult, .T.)
    ELSE
      lcReturn = &lcTempCursorName..vendor
      gfOpenFile(oAriaApplication.DataDir+'APVENDOR','VENCODE',"SH")
      lcReturn = LOOKUP(APVENDOR.cvencomp,ALLTRIM(lcReturn),APVENDOR.CVENDCODE,"VENCODE")
    ENDIF
  
  OTHERWISE
   lcReturn = LOOKUP(CUSTOMER.BtName,"M"+syschdul.cCont_Id,CUSTOMER.ACCOUNT,"CUSTOMER")
ENDCASE

SELECT(lnSelected)
RETURN lcReturn

*!*************************************************************
*! Name        : gfToolTipAssign
*! Developer   : Ahmad Shoukry Mohammed (ASM)
*! Date        : 05/01/2006
*! Purpose     : Checks the 
****************************************************************************
*! Parameters  : The Control, The Value
*!*************************************************************
*! Returns     : None
*!*************************************************************
*040206 ASM,1 Fix the Problem of ToolTip text shifting
FUNCTION gfToolTipAssign
LPARAMETERS loCtrl, m.vNewVal

loCtrl.xToolTip = m.vNewVal
loCtrl.ToolTipText = ''

RETURN
*--end of gfToolTipAssign

*!*************************************************************
*! Name        : gfToolTipShift
*! Developer   : Ahmad Shoukry Mohammed (ASM)
*! Date        : 05/01/2006
*! Purpose     : Checks the Docking Effect of the ToolBar on the TollTips
****************************************************************************
*! Parameters  : None
*!*************************************************************
*! Returns     : Numeric
*!*************************************************************
*040206 ASM,1 Fix the Problem of ToolTip text shifting
FUNCTION gfToolTipShift
LOCAL lnRetValue

lnRetValue = IIF(TYPE('oAriaApplication.oToolBar.oWindParent.AriaForm1.Name')='C' AND ;
             _screen.ActiveForm.Name==oAriaApplication.oToolBar.oWindParent.AriaForm1.Name, ;
              oAriaApplication.oToolBar.DockPosition,2)
RETURN lnRetValue
*--end of gfToolTipShift

*!*************************************************************
*! Name        : gfMouseEnter
*! Developer   : Ahmad Shoukry Mohammed (ASM)
*! Date        : 05/01/2006
*! Purpose     : Solve the Problem of ToolTipText When the ToolBar is Docked Top or Left. Called From Mouse Enter Event of the Control
****************************************************************************
*! Parameters  : The Control
*!*************************************************************
*! Returns     : None
*!*************************************************************
*040206 ASM,1 Fix the Problem of ToolTip text shifting
FUNCTION gfMouseEnter
LPARAMETERS loCtrl
LOCAL lnDockPos, loForm, lnAbsTop, lnAbsLeft

* T20080429.0009 MAH 18 / 6 / 2008 BEGIN
IF TYPE('loForm') != 'O' .OR. ISNULL(loForm)
  RETURN
ENDIF
* T20080429.0009 MAH 18 / 6 / 2008 END

loForm = _screen.ActiveForm
IF TYPE('loForm.lblToolTip')<>'O'
  loForm.AddObject('lblToolTip','ariatooltip')
ENDIF

loForm.lblToolTip.visible = .F.
lnDockPos = gfToolTipShift()
lnAbsTop = gfAbsolutePos('Top',loCtrl)
lnAbsLeft = gfAbsolutePos('Left',loCtrl)
DO CASE
  CASE lnDockPos = 0 && Top Docking
    loForm.lblToolTip.Top = lnAbsTop - oAriaApplication.oToolBar.Height
    loForm.lblToolTip.Left = lnAbsLeft
  CASE lnDockPos = 1 && Left Docking
    loForm.lblToolTip.Top = lnAbsTop
    loForm.lblToolTip.Left = lnAbsLeft - oAriaApplication.oToolBar.Width
  OTHERWISE
    loForm.lblToolTip.Top = lnAbsTop
    loForm.lblToolTip.Left = lnAbsLeft
ENDCASE
loForm.lblToolTip.height = loCtrl.Height
loForm.lblToolTip.width = loCtrl.Width

IF loCtrl.MousePointer<>0
  loForm.lblToolTip.MousePointer = loCtrl.MousePointer
ELSE
  DO CASE
    CASE LOWER(loCtrl.BaseClass) = 'commandbutton' OR !loCtrl.Enabled
      loForm.lblToolTip.MousePointer = 1
    CASE LOWER(loCtrl.BaseClass) = 'combobox' AND loCtrl.Enabled AND loCtrl.Style=2
      loForm.lblToolTip.MousePointer = 1
    CASE loCtrl.Enabled
      loForm.lblToolTip.MousePointer = 3
  ENDCASE
ENDIF
loForm.lblToolTip.ToolTipText = IIF(TYPE('loCtrl.xToolTip')='C',loCtrl.xToolTip,loForm.lblToolTip.ToolTipText)
loForm.lblToolTip.loControl = loCtrl
loForm.lblToolTip.visible = .t.

RETURN
*--end of gfMouseEnter

*!*************************************************************
*! Name        : gfAbsolutePos
*! Developer   : Ahmad Shoukry Mohammed (ASM)
*! Date        : 05/01/2006
*! Purpose     : To Calculate the Absolute Top or Left of a control in a Container
****************************************************************************
*! Parameters  : The Property, The Control
*!*************************************************************
*! Returns     : Numeric
*!*************************************************************
*040206 ASM,1 Fix the Problem of ToolTip text shifting
FUNCTION gfAbsolutePos
LPARAMETERS lcProp, loCtrl
LOCAL lnValue
lnValue = IIF(UPPER(lcProp)="TOP" and LOWER(loCtrl.Parent.BaseClass)<>'form',25,0) + EVALUATE('loCtrl.'+lcProp)

loCtrl = loCtrl.Parent
DO WHILE LOWER(loCtrl.BaseClass)<>'form'
  IF TYPE('loCtrl.'+lcProp)='N'
    lnValue = lnValue +  EVALUATE('loCtrl.'+lcProp)
  ENDIF
 loCtrl = loCtrl.Parent
ENDDO

RETURN lnValue

*:**************************************************************************
*:* Name        : lfGtCallFn
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/19/2007
*:* Purpose     : Get calling function name
*:***************************************************************************
*B608223,1 TMI
FUNCTION lfGtCallFn
LOCAL lnI,lcRet
lnI = 0
DO WHILE !EMPTY(PROGRAM(lnI))
  lnI = lnI + 1
ENDDO
lcRet = PROGRAM(lnI-3)
*! B609253,1 MMT 05/17/2010 Release variables from gfSheetItem Function after usage[Start]
RELEASE lnI
*! B609253,1 MMT 05/17/2010 Release variables from gfSheetItem Function after usage[End]
RETURN lcRet
*-- end of lfGtCallFn.

*E302567,1 MMT 01/12/2009 Change paths for SAAS [sTART]
*!*************************************************************
*! Name      : gfCallForm
*! Developer : MARIAM MAZHAR [MMT]
*! Date      : 01/12/2009
*! Purpose   : Function to call forms instead of DO FORM
*!*************************************************************
*! Parameters:
*! lcFormName : The form to be called Name
*! lcModuleID : Module that form existing in
*! lcWithParam: WITH Paramters
*! lcToParam  : TO Paramters
*!*************************************************************
FUNCTION gfCallForm
PARAMETERS lcFormName,lcModuleID,lcWithParam,lcToParam



IF TYPE('lcModuleID') = 'C'
  IF oAriaApplication.multiinst AND FILE(oAriaApplication.ClientScreenHome+lcModuleID+'\'+lcFormName+'.SCX')
    IF TYPE('lcWithParam') = 'C'
      IF  TYPE('lcToParam') = 'C'
        DO FORM oAriaApplication.ClientScreenHome +lcModuleID+'\'+lcFormName+'.SCX' WITH &lcWithParam TO &lcToParam
      ELSE
        DO FORM oAriaApplication.ClientScreenHome +lcModuleID+'\'+lcFormName+'.SCX' WITH &lcWithParam 
      ENDIF   
    ELSE
      IF  TYPE('lcToParam') = 'C'
        DO FORM oAriaApplication.ClientScreenHome +lcModuleID+'\'+lcFormName+'.SCX' TO &lcToParam
      ELSE
        DO FORM oAriaApplication.ClientScreenHome +lcModuleID+'\'+lcFormName+'.SCX'       
      ENDIF   
    ENDIF    
  ELSE
    IF TYPE('lcWithParam') = 'C'
      IF  TYPE('lcToParam') = 'C'
        DO FORM oAriaApplication.ScreenHome+lcModuleID+'\'+lcFormName+'.SCX' WITH &lcWithParam TO &lcToParam
      ELSE
        DO FORM oAriaApplication.ScreenHome+lcModuleID+'\'+lcFormName+'.SCX' WITH &lcWithParam 
      ENDIF   
    ELSE
      IF  TYPE('lcToParam') = 'C'
        DO FORM oAriaApplication.ScreenHome+lcModuleID+'\'+lcFormName+'.SCX' TO &lcToParam
      ELSE
        DO FORM oAriaApplication.ScreenHome+lcModuleID+'\'+lcFormName+'.SCX'      
      ENDIF   
    ENDIF    
  ENDIF   
ELSE
 IF oAriaApplication.multiinst AND FILE(oAriaApplication.ClientScreenHome+lcFormName+'.SCX')
    IF TYPE('lcWithParam') = 'C'
      IF  TYPE('lcToParam') = 'C'
        DO FORM oAriaApplication.ClientScreenHome +lcFormName+'.SCX' WITH &lcWithParam TO &lcToParam
      ELSE
        DO FORM oAriaApplication.ClientScreenHome +lcFormName+'.SCX' WITH &lcWithParam 
      ENDIF   
    ELSE
      IF  TYPE('lcToParam') = 'C'
        DO FORM oAriaApplication.ClientScreenHome +lcFormName+'.SCX' TO &lcToParam
      ELSE
        DO FORM oAriaApplication.ClientScreenHome +lcFormName+'.SCX'       
      ENDIF   
    ENDIF    
  ELSE
    IF TYPE('lcWithParam') = 'C'
      IF  TYPE('lcToParam') = 'C'
        DO FORM oAriaApplication.ScreenHome+lcFormName+'.SCX' WITH &lcWithParam TO &lcToParam
      ELSE
        DO FORM oAriaApplication.ScreenHome+lcFormName+'.SCX' WITH &lcWithParam 
      ENDIF   
    ELSE
      IF  TYPE('lcToParam') = 'C'
        DO FORM oAriaApplication.ScreenHome+lcFormName+'.SCX' TO &lcToParam
      ELSE
        DO FORM oAriaApplication.ScreenHome+lcFormName+'.SCX'      
      ENDIF   
    ENDIF    
  ENDIF    
ENDIF 
*E302567,1 MMT 01/12/2009 Change paths for SAAS [eND]
*:******************************************************************
*! Function  : gfDispSpack
*! Developer : Wael Ali Mohamed
*! Date      : 01/01/2011
*! DESC      : function to display All installed tracking entries that was 
*:******************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =gfDispSpack()
*!*************************************************************
*! B609487,1 WAM 01/23/2011 Display System updates [T20110109.0001]
*!*************************************************************
Function gfDispSpac

lcSQLDICPATH =  oAriaApplication.cAria4SysPath
SELECT PADR(IIF(sydattach.centrytype='C',"Custom","System"),10) AS cType, sydattach.crelease ,sydattach.csrvpack,sydattach.cbuild,sydattach.centrytype,sydattach.centryid,sydexes.ticket,sydexes.ctrksdsc,;
sydexes.dgendate, sydexes.dinstdate,sydattach.cname,sydattach.ckey,sydattach.ctag ;
FROM (lcSQLDICPATH +"SYDATTACH") INNER JOIN (lcSQLDICPATH+"SYDEXES") ;
ON sydattach.crelease+sydattach.csrvpack+sydattach.cbuild+sydattach.centrytype+sydattach.centryid =  sydexeS.crelease+sydexes.csrvpack+sydexes.cbuild+sydexes.centrytype+sydexes.ctrackno ;
UNION ;
(SELECT "Temporary " AS cType, clients_apps.crelease ,clients_apps.csrvpack,SPACE(3) AS cbuild,clients_apps.centrytype,clients_apps.centryid,clients_apps.Ticket,clients_apps.ctrksdsc,;
TTOD(dgendate) AS dgendate,TTOD(dinstdate) AS dinstdate,clients_apps.cname,clients_apps.ckey,clients_apps.ctag ;
FROM (lcSQLDICPATH+"CLIENTS_APPS") ) ;
ORDER BY  clients_apps.crelease,clients_apps.csrvpack,cbuild,clients_apps.centrytype,clients_apps.centryid INTO CURSOR SysUpdates

SELECT SysUpdates

lcBrFields =[crelease:H="Release#",csrvpack:H="Service Pack#",cbuild:H="Build#",centrytype:H="Type",centryid:H="Entry ID",ticket:H="Ticket#",ctrksdsc:H="Description",]
lcBrFields =lcBrFields +[dgendate:H="Generate Date", dinstdate:H="Install Date",cname:H="File name",ckey:H="Key",ctag:H="Tag Name",cType:H="Update Type"] 

=ARIABROW(.F.,"System Updates")  

USE IN 'SysUpdates'
USE IN 'SYDATTACH'
USE IN 'SYDEXES'
USE IN 'CLIENTS_APPS'


* T20101118.0003 - Attach Objects by Style /Colour MAH [BEGIN]
FUNCTION gfGetStyleImageInfo
LPARAMETERS lcReturnType, lcStyleKey
IF ALLTRIM(lcReturnType) == "K"
  DIMENSION laSegInfo[1,1]
  LOCAL loGetItemMask
  loGetItemMask = CREATEOBJECT('GetItemMask')
  loGetItemMask.Do(@laSegInfo, '', "0001")
  
  IF ALEN(laSegInfo, 2) > 1
    IF EMPTY(SUBSTR(lcStyleKey, laSegInfo[2, 4], LEN(laSegInfo[2, 3])))
      lcStyleKey = SUBSTR(lcStyleKey, laSegInfo[1, 4], LEN(laSegInfo[1, 3]))
    ELSE
      lcStyleKey = SUBSTR(lcStyleKey, laSegInfo[1, 4], LEN(laSegInfo[1, 3])) + laSegInfo[1, 6] + ;
                   SUBSTR(lcStyleKey, laSegInfo[2, 4], LEN(laSegInfo[2, 3]))
    ENDIF
  ENDIF

  LOCAL lnMajLen, lcRetValue
  
  lcStyleKey = PADR(lcStyleKey, 19)

  IF !USED('OBJLINK')
    =gfOpenTable('OBJLINK','OBJLNKTY')
  ENDIF
  
  IF gfSeek('D' + 'S' + lcStyleKey, 'OBJLINK', 'OBJDEFA') .OR. gfSeek('S' + lcStyleKey, 'OBJLINK', 'OBJLNKTY')
   lcRetValue = lcStyleKey
  ELSE
    lnMajLen    = LEN(gfItemMask('PM'))
    lcRetValue  = SUBSTR(lcStyleKey, 1, lnMajLen)
  ENDIF
  
  RETURN PADR(lcRetValue, 19)
ENDIF

RETURN ""

FUNCTION gfGetItemImageInfo
LPARAMETERS lcReturnType, lcStyleKey

IF ALLTRIM(lcReturnType) == "K"
  DIMENSION laSegInfo[1,1]
  LOCAL loGetItemMask
  loGetItemMask = CREATEOBJECT('GetItemMask')
  loGetItemMask.Do(@laSegInfo, '', "0002")
  
  IF ALEN(laSegInfo, 2) > 1
    IF EMPTY(SUBSTR(lcStyleKey, laSegInfo[2, 4], LEN(laSegInfo[2, 3])))
      lcStyleKey = SUBSTR(lcStyleKey, laSegInfo[1, 4], LEN(laSegInfo[1, 3]))
    ELSE
      lcStyleKey = SUBSTR(lcStyleKey, laSegInfo[1, 4], LEN(laSegInfo[1, 3])) + laSegInfo[1, 6] + ;
                   SUBSTR(lcStyleKey, laSegInfo[2, 4], LEN(laSegInfo[2, 3]))
    ENDIF
  ENDIF

  LOCAL lnMajLen, lcRetValue
  
  lcStyleKey  = PADR(lcStyleKey, 19)

  IF !USED('OBJLINK')
    =gfOpenTable('OBJLINK','OBJLNKTY')
  ENDIF
  IF gfSeek('D' + 'M' + lcStyleKey, 'OBJLINK', 'OBJDEFA') .OR. gfSeek('M' + lcStyleKey, 'OBJLINK', 'OBJLNKTY')
   lcRetValue = lcStyleKey
  ELSE
    lnMajLen    = LEN(gfItemMask('PM', '', '0002'))
    lcRetValue  = SUBSTR(lcStyleKey, 1, lnMajLen)
  ENDIF
  
  RETURN PADR(lcRetValue, 19)
ENDIF

RETURN ""
* T20101118.0003 - Attach Objects by Style /Colour MAH [END]