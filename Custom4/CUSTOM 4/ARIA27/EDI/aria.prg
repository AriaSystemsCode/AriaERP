CLEAR
on error do gfErrorTrap

DEACTIVATE WINDOW "Project Manager"
CLEAR ALL CLASSES
_VFP.Visible = .T.

*-- All public vars will be released as soon as the application
*-- object is created. 

PUBLIC gcBaseWind, gnProgCopy, glInitMenu,gnMaxUsers,gcPrmtMdls,gcAct_Key,laCtrStat
*B607093,1 Adding active application varible HASSAN 07/14/2003 [BEGIN]
*PUBLIC glToolActv,glMsgREm,glUser_tsk,glAutoAdd,GlLog_Requ,glSys_log,gcAct_Comp
 PUBLIC glToolActv,glMsgREm,glUser_tsk,glAutoAdd,GlLog_Requ,glSys_log,gcAct_Comp,gcAct_Appl
*B607093,1 Adding active application varible HASSAN 07/14/2003 [END  ]

PUBLIC oAriaApplication,gcPlatForm,gcLicence,gcCompName,lcProgName,glErrorHan
STORE .F. TO glToolActv,glMsgREm,glUser_tsk,glAutoAdd,GlLog_Requ,glSys_Log,glErrorHan
*B607093,1 Adding active application varible HASSAN 07/14/2003 [BEGIN]
*STORE "  " TO gcAct_Comp,gcLicence,gcCompName
 STORE "  " TO gcAct_Comp,gcLicence,gcCompName,gcAct_Appl
*B607093,1 Adding active application varible HASSAN 07/14/2003 [END  ]
gnMaxUsers = 0

lcCurrentProcedure = UPPER(SYS(16,1))
lcCurrentProcedure = STRTRAN(lcCurrentProcedure, "\PRGS", "")
lnPathStart        = AT(":",lcCurrentProcedure)- 1
lnLenOfPath        = RAT("\", lcCurrentProcedure) - (lnPathStart)
lcCurPath          = SUBSTR(lcCurrentProcedure, lnPathStart, lnLenofPath)
SET DEFAULT TO (lcCurPath)
SET CLASSLIB TO

lcClasPath  = lcCurPath + "\CLASSES\"
lnClasses   = ADIR(laClasses, lcClasPath + "*.*", "D")
FOR lnClass = 1 TO lnClasses
  IF "D" $ laClasses[lnClass,5] AND !INLIST(laClasses[lnClass,1], ".", "..")
    = lpSetLibs (lcClasPath + laClasses[lnClass,1])
  ELSE
    = lpSetFile (lcClasPath, laClasses[lnClass,1])
  ENDIF
ENDFOR
RELEASE lcClass, lnClasses, lnClass, lcClasPath, lnLenOfPath, lnPathStart, lcCurrentProcedure

oAriaApplication = CREATEOBJECT("AriaApplication")
IF TYPE('oAriaApplication') = "O"
  oAriaApplication.Do()
  oAriaApplication.oToolBar = null
  oAriaApplication = null
ENDIF

SET CLASSLIB TO
CLEAR CLASSLIB MAIN
CLEAR DLLS
RELEASE ALL EXTENDED
CLEAR ALL

* ------------------------------------------------------------------------------

PROCEDURE lpSetLibs
LPARAMETERS lcPathToSearch
PRIVATE lnClasses, lnClass, laClasses

lcPathToSearch = IIF(RIGHT(lcPathToSearch,1)="\", lcPathToSearch, lcPathToSearch+"\")
lnClasses   = ADIR(laClasses, lcPathToSearch + "*.VCX")
FOR lnClass = 1 TO lnClasses
  = lpSetFile (lcPathToSearch, laClasses[lnClass,1])
ENDFOR

* ------------------------------------------------------------------------------

PROCEDURE lpSetFile
LPARAMETERS lcPath, lcClassFileName 
PRIVATE lcClass, lcCommand

IF UPPER(RIGHT(lcClassFileName, 3)) = "VCX"
  lcClass   = lcPath + lcClassFileName 
  lcCommand = SPACE(0)
  IF INLIST(UPPER(lcClassFileName), "MAIN.VCX", "GLOBALS.VCX", "UTILITY.VCX")
    lcCommand = "IN ARIA.EXE"
  ENDIF
  *B605053,1 Hassan [Begin]
  *IF !(lcClassFileName $ SET("CLASSLIB"))
  IF !("\"+lcClassFileName $ SET("CLASSLIB"))
    SET CLASSLIB TO (lcClass) &lcCommand ADDITIVE
  ENDIF
  *B605053,1 Hassan [End]
ENDIF

*!*************************************************************
*! Name      : gfTempName
*! Developer : Yasser Saad Ibrahime
*! Date      : 1993-1995 
*! Purpose   : Creat temp file name
*!*************************************************************
*! Calls     : 
*!      Called by: ARIA3.PRG                
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : 
*!*************************************************************
*:->
FUNCTION gfTempName
RETURN ("X"+SUBSTR(SYS(2015),4))




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




FUNCTION GPEXIT
CLEAR WINDOWS
IF _SCREEN.FORMCOUNT <= 1
  oAriaApplication.Sysexit()
ENDIF  



FUNCTION gfUserList
PARAMETERS llGetCount
PRIVATE ALL LIKE  L*
*SET DATASESSION TO 1
lcOldRep = SET('REPROCESS')
lnDataSession = set('datas')
SET DATASESSION TO 1
SET REPROCESS TO 1

llGetCount = IIF(TYPE('llGetCount')="U",.F.,llGetCount)

DECLARE laUserList[1]
llUserUsed = USED('SYUUSER')
lcCurrFile = ALIAS()
laUserList = " "
lnUsrRec   = 0
SELECT SYUSTATC
IF !llUserUsed
  USE (oAriaApplication.SysPath+'SYUUSER') IN 0 ORDER TAG cuser_id
ENDIF
IF USED("SYUUSER")
  lnUsrRec = RECNO("SYUUSER")
ENDIF

SELECT IIF(SYUSTATC.CUSER_ID = oAriaApplication.User_ID AND;
           SYUSTATC.cstation = oAriaApplication.Station,;
           "» ","  ")+;
           PADR(LFGETUSRNM(SYUSTATC.CUSER_ID),35) ;
     FROM (oAriaApplication.SysPath+"SYUSTATC");
     WHERE COBJ_TYP+ALLTRIM(COBJ_NAME)+SYUSTATC.CUSER_ID+CSTATION=;
            'INI'+'OLDVARS' ;
       .AND.;
             gfCheckUser(SYUSTATC.CUSER_ID,CSTATION) ;
       INTO ARRAY  laUserList     

IF oAriaApplication.UserStaticRecord < RECCOUNT('syuStatc')
  GO oAriaApplication.UserStaticRecord  IN syuStatc 
  SET REPROCESS TO 1
  =RLOCK('syuStatc')
ENDIF  
SET REPROCESS TO lcOldRep
IF !llGetCount
  oAriaApplication.DoFormRetVal("syusrlst")
ENDIF  


IF !EMPTY(lcCurrFile)
  SELECT (lcCurrFile)
ENDIF  

IF lnUsrRec > 0 .AND. USED("SYUUSER")
  IF lnUsrRec <= RECCOUNT("SYUUSER")
    GO lnUsrRec IN SYUUSER
  ENDIF
ENDIF
IF !llUserUsed
  USE IN SYUUSER
ENDIF
SET DATASESSION TO lnDataSession
IF llGetCount
  RETURN _TALLY 
ENDIF



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
RETURN llRetFlag .OR. (lcUserID+lcStation = oAriaApplication.User_ID+oAriaApplication.Station)



FUNCTION gfStation

lcStation = ALLTRIM(GETENV('P_STATION'))

IF !EMPTY(lcStation)
  lcStation = ALLTRIM(SYS(2007,lcStation))
ELSE
  lcStation = SUBSTR(SYS(3),4) 
ENDIF

RETURN (lcStation)


FUNCTION lfGetUsrNm
PARAMETER lcUser_ID
SELECT SYUUSER
IF SEEK(lcUser_ID)
   RETURN cUsr_Name
ELSE
    RETURN lcUser_ID
ENDIF

FUNCTION GPRELOGIN
IF _SCREEN.FORMCOUNT <= 1 
  oAriaApplication.Login()
  oAriaApplication.LogUser(.T.)
  OAriaApplication.SetMenu(OAriaApplication.ActiveModuleID,IIF(OAriaApplication.ActiveModuleID='SY','S','A'))
ELSE
  =MessageBox("You have to close all programs before login in with new use id",16)  
ENDIF



FUNCTION gfPrintSet
=SYS(1037)

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
*
FUNCTION gfBrowse
*wab
*lParameters tcBrowseFields,tcBrowseTitle,tcAlias,tcKey,tcFor,tcOptions,tlSelect
lParameters tcBrowseFields,tcBrowseTitle,tcAlias,tcKey,tcFor,tcOptions,tlSelect ,;
            toSelectObj,tcSelectMethod,tcUserShortCut,tcTempFile,tcSelField
*wab
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
*wab
DO FORM BROWSE WITH tcBrowseFields,tcBrowseTitle,tcKey,tcFor,tcOptions,tlSelect;
   TO llReturnValue
*DO FORM BROWSE WITH tcBrowseFields,tcBrowseTitle,tcKey,tcFor,tcOptions,tlSelect,;
*       toSelectObj,tcSelectMethod,tcUserShortCut,tcTempFile,tcSelField   TO llReturnValue
*wab
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
    lcTableName = CURSORGETPROP('Sourcename',lcAlias)
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
FUNCTION GFGENFLT
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


FUNCTION lfGetQCond
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


FUNCTION lfGetOper              
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


FUNCTION lfRightGet
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
oAriaApplication.oToolBar.cmdprint.Click

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
*
FUNCTION GPRECHIST

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

FUNCTION GFTBARONOF
oAriaApplication.oToolBar.Visible = !oAriaApplication.oToolBar.Visible

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


FUNCTION UPDDATE
DO Form syUpDate

*!*************************************************************
*! Name      : gfAdd_Info
*! Developer : Yasser Saad Ibrahime
*! Date      : 1993-1995 
*! Purpose   : To add  audit information to any file
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

LOCAL oAddInfo

oAddInfo = CREATEOBJECT('AddUserInfo')

RETURN oAddInfo.Do(lcFileName , oForm)



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

*!************************************************************!*************************************************************
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
llIsOpen = .F.
IF !USED('SYCTRIGG')
    SELECT 0
    *** Open the
    USE (oARiaApplication.SysPath+"SYCTRIGG")
    SET ORDER TO 1
ENDIF

SELECT SYCTRIGG

*-- If there is triggers for this Object/Event
IF SEEK(PADR(lcProgName , 10) + PADR(lcEvent , 10))
  
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
    IF !EMPTY(SYCTRIGG.mParmExpr)
      
      *-- Get the parameter expressions
      DIMENSION laParamExp[OCCURS('~' , SYCTRIGG.mParmExpr) + 1]
      =gfSubStr(SYCTRIGG.mParmExpr , @laParamExp , '~')
      
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
    
    *-- If custom process
    *Hassan [Begin]
    lcOldPath = FullPath('')
    lcNewPath = SubStr(oAriaApplication.ApplicationHome,1,Rat("\",oAriaApplication.ApplicationHome,2))
    CD (lcNewPath) 
    *Hassan [End]
    IF SYCTRIGG.cActvTyp = 'C'
      *-- Call the program and get the returned value
      llReturn = &lcProgToDo(&lcParmStr)
    ENDIF    && End of IF SYCTRIGG.cActvTyp = 'C'
    *Hassan [Begin]
    CD (lcOldPath) 
    *Hassan [End]
    SELECT SYCTRIGG
  ENDSCAN    && End of SCAN REST WHILE cAPObjNam + cEvent_ID = ...
  


ELSE    &&  *In case the process doesn't exist.[START]
  llReturn = .F.
  
ENDIF    && End of IF SEEK(PADR(lcProgName , 10) + PADR(lcEvent , 10))

*-- Restore the old alias
SELECT (lnOldAlias)

RETURN (llReturn)
