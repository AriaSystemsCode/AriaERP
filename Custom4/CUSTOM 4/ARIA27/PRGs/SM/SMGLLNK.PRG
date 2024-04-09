*:************************************************************
*: Program file  : SMGLLNK
*: Program desc. : Create G/L. Link Code
*: For screen    : SMGLLNK
*:        System : Aria Advantage Series.
*:        Module : System Manager (SM).
*:     Developer : Ahmed Amer (AHM)
*:************************************************************
*: Calls : 
*:         Procedures : lpShow,lpSavscr
*:         Functions  : gfSetup,gfOpenFile,lfvData_2,
*:                    : lfvData_3,lfTrap,lfClrTrap  
*:************************************************************
*: Passed Parameters  : lcSetupCom,lcSULnkVer
*:************************************************************
*: Example : DO SMGLLNK
*:************************************************************
*: Modifications :
*E301098,1 HESHAM 12/16/98 Get company data path using gfGetDataDir(..)
*E801897,1 AAMER  01/24/99 Validate that a G/L link code is always 6 characters.
*E301077,62 AAMER 02/16/99 Open files with gfOpenFile and close it with function
*                          gfCloseFile
*E301173,1 AHM As a result of removing gl linked company if we are linked to Aria
*E301349,1 AHM 11/18/1999 Adding new category keys in SyGLCateg file without
*E301349,1                Displaying these category keys in the category key browse
*:************************************************************

*-- lcSetupCom Parameter that hold the passed company if the program
*--                      called from setup and has no value if it is
*--                      called from the menu
*-- lcSULnkVer Parameter that hold the passed G/L. linking version if the program
*--                      called from setup and has no value if it is
*--                      called from the menu
PARAMETERS lcSetupCom,lcSULnkVer

*-- llSetup Variable that showes if the program called from setup or from menu
llSetup = !EMPTY(lcSetupCom)

*-- laData      array that hold the data of the key screens and header folder
*-- laScrMode   array that hold the screen mode
*-- laKeyField  array that hold the key of the screen 
*-- laType      array that hold the link types
*-- laComp      Array that hold companies list

DIMENSION laData[1],laScrMode[4],laKeyField[2,4],laType[5],laComp[1,3]

*-- lcTypeDesc  variable that hold link type description
*-- lcGLAcc     variable that hold G/L. Account
*-- lcGLDesc    variable that hold G/L. description
*-- lcGLChrTag  variable that hold the name of the account chart tag
*-- lcGLDir     variable that hold the directory of the GL_Link file
*-- lcFile      variable that hold the name of the account chart file
*-- lcGLCo      variable that hold the name of the G/L. company
*-- lcCodeFld   variable that hold the name of the GL account code filed
*               in account chart file
*-- lcDescFld   variable that hold the name of the GL account description 
*               filed in account chart file
*-- lcSysDir    variable that hold the system files directory of the used
*               G/L. version
*-- lcGLVer     variable that hold the version of the G/L. Link
*-- lcGLCo      variable that hold Company ID which the system is linked
*               to its GL.
*-- lcOldVal    variable that hold the original value of any field befor modify it
*-- lcPrntCo    variable that hold the parent company ID
*-- lcPrSysDir  variable that hold the system files directory of the parent
*               company 
*-- lcPrGLVer   variable that hold the version of the parent company G/L. Link
*-- lcPrGLCo    variable that hold parent Company ID which the system is linked
*               to its GL.
*-- lcPrGLDir   variable that hold the directory of the GL_Link file of the 
*               parent company
*-- lcPrFile    variable that hold the name of the account chart file of the 
*               parent company
*-- lcCpyLink   variable that hold the source GL link code to copy information
*               from it
*-- lcGlLink1   variable that hold temp name to be used for defining the
*               key fields window
*-- lcGlLink2   variable that hold temp name to be used for defining the
*               browse window
*-- lcGlLink3   variable that hold temp name to be used for defining the
*               edit rigion window
*-- lcKeyCat    variable that hold the temp name of dbf file which used for 
*               holding link code category keys
*-- lcBaseGL    variable that hold the name of the alias which GL_Link file
*               will be used with it and showes the detail records only
*               (do not include records that have linktype = '00')
*-- lcAllGL     variable that hold the name of the alias which GL_Link file
*               will be used with it and showes all file records
*-- lcGLChart   variable that hold the name of the alias which chart of 
*               Account GL_Link file will be used with it
*-- lcGLMask    variable that hold the name of the alias which Account code
*--             file will be used with it
*-- lcGLAccStr  variable that hold the name of the alias which Account structure 
*--             file will be used with it
*-- lcEmptyMas  Variable that hold the account picture with the seperator
*--             only (no picture characters)

STORE SPACE(0) TO lcTypeDesc,lcGLAcc,lcGLDesc,laComp,laType,lcGLChrTag,;
                  lcGLDir,lcFile,lcGLCo,lcCodeFld,lcDescFld,lcSysDir,;
                  lcGLVer,lcGLCo,lcOldVal,lcPrntCo,lcPrSysDir,;
                  lcPrGLVer,lcPrGLCo,lcPrGLDir,lpopcPrFile,lcCpyLink,lcGlLink1,;
                  lcGlLink2,lcGlLink3,lcKeyCat,lcBaseGL,lcAllGL,lcGLChart,lcAcMask,;
                  lcGLMask,lcGLAccStr,lcEmptyMas
                  
*-- lcGLDscStat variable that hold the eanabling status of the G/L. description
*               field
*-- lcTypeStat  variable that hold the eanabling status of the link type field
*-- lcLinkStat  variable that hold the eanabling status of the link code field
STORE "DISABLE" TO lcGLDscStat,lcTypeStat,lcLinkStat

*-- llOtherVer  variable that showes if the system is linked to ARIA,SBT or
*               Others GL versions to be used in the screen SMGLLNK3 for
*               GL Account field and its push button (#OBJDISP llOtherVer)
*-- llChldComp  variable that showes if the company is a chield company
*-- llBrowse    variable that used to show if any browse button is clicked
*-- llNoShow    variable that allow screen to call lpshow procedure once 
*--             the program is run

STORE .F. TO llOtherVer,llChldComp,llBrowse

*-- lnBrRecNo   variable that hold the current record of the category keys
*               browse
*-- lnComp      variable that hold the number of the cuurent companies
*               popup value
*-- lnLinkTxt   variable that hold the number of the cuurent Link type
*               popup value
STORE 0 To lnBrRecNo,lnComp,lnLinkTxt,lnAcLen

*-- llSMMOD     variable that showes if the parogram called from SM module
*               or any other module
llSMMOD = gcAct_Appl == "SM"

*-- lcTypDesc   To hold the description of the GL Link Category Type + Code 
*				Description.
lcTypDesc = "Description"

laDefproc[09] = .F.                 && This is to Enable local save
laDefproc[07] = .F.                 && This is to Enable local delete

*-- lcBrTtl variable that hold the browse title
lcBrTtl = 'Key Categories'
 
laKeyField[1,1] = "laData[1]"
laKeyField[1,2] = .F.
laKeyField[1,3] = "GL_Link1"
laKeyField[1,4] = 1

laKeyField[2,1] = "laData[2]"
laKeyField[2,2] = .T.
laKeyField[2,3] = "GL_Link1"
laKeyField[2,4] = 2

IF !gfSetup()
  RETURN
ENDIF  

*-- this is to open the SMGLLNK Screen as modal window if the program
*   is called from setup
IF llSetUp
  lcModal = 'WITH (GCBASEWIND), (lcBrTtl)'
ELSE
  lcModal = ''
ENDIF     &&IF llSetUp

IF !WEXIST(gcBaseWind)
  *-- This is to get the GL_Link file browse fields 
  lcBrFields = gfDbfField('GL_LINK')
  *-- This is to get the GL_Link browse title
  
  *--E301077,62 (Start)
  PRIVATE llSydFOpn
  llSydFOpn = gfOpenFile(gcSysHome+'SYDFILES','Cfile_nam','SH')
  *--E301077,62 (End)
  
  lcFile_Ttl = IIF(SEEK('GL_LINK', 'SYDFILES'),SYDFILES.cFile_Ttl, 'G/L Link codes') 
  
  *--E301077,62 (Start)
  = gfCloseFile('SYDFILES')
  *--E301077,62 (End)
  
  lcGlLink1  = gfTempName()
  lcGlLink2  = gfTempName()
  lcGlLink3  = gfTempName()  
    
  lcKeyCat   = gfTempName()         
  
  lcBaseGL   = gfTempName()         
  lcAllGL    = gfTempName()         
  lcGLChart  = gfTempName()         
  lcSysTmp   = gfTempName()
  lcGLMask   = gfTempName()
  lcGLAccStr = gfTempName()

  CREATE TABLE (gcWorkDir + lcKeyCat);
   (Link_Code C(6),LinkType C(2),LinkDesc C(30),CatgKey C(3),CatgDesc C(30),;
    GLAcnt C(24),GLAccDesc C(40),nRecNo N(4),cStatus C(1))

  *-- lfTypeArr() is a function to prepare the Link Type arry according
  *   to the calling module
  llNoThing = lfTypeArr()
  *-- lfGLFiles() is a function to open the GL_Link file account chart
  *   file according to the GL version(ARIA,SBT,Others) and to the active
  *   company

  IF !lfGLFiles()
    glQuitting = .T.
    RETURN
  ENDIF
  
  *-- This is to switch screen mode to add mode if the program is called
  *   from the setup
  IF llSetUp
    STORE .F. TO laScrMode
    laScrMode[4] = .T.
    llNoShow = .F.
  ENDIF    &&IF llSetUp
  
ENDIF    &&IF !WEXIST(gcBaseWind)

PUSH KEY                                      && To save the the current on key label
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrTtl)  && To activate the browse screen when pressing ALT+B
DO (gcScrDir + gcWinAppl + '\SMGLLNK.SPR')                 && calling SMGLLNK screen
RELEASE WINDOW (lcBrTtl)                      && Release the Browse Window
POP KEY                                       && To Restore the previous assignments for on key label

*-- If we realy quitting the screen 

*--E301077,62 (Start)
*--Comment this part of code since all opened files is close
*--when quitting the program and temp. files is being erased.
*IF glQuitting
*  IF USED(lcKeyCat)
*    USE IN (lcKeyCat)
*  ENDIF    &&IF USED(lcKeyCat)
*  ERASE (gcWorkDir+lcKeyCat +".DBF")
*  ERASE (gcWorkDir+lcKeyCat +".CDX")
  
*  *--E301077,62 (Start)
*  *IF USED(lcGLChart)
*  *  USE IN (lcGLChart)
*  *ENDIF    &&IF USED(lcGLChart)
*  =gfCloseFile(lcGLChart)
*  *--E301077,62 (End)
     
*  IF USED(lcBaseGL)
*    SELECT (lcBaseGL)
*    SET RELATION TO
*    *--E301077,62 (Start)
*    *USE IN (lcBaseGL)
*    *--E301077,62 (End)
*  ENDIF    &&IF USED(lcBaseGL)
*  *--E301077,62 (Start)
*  =gfCloseFile(lcBaseGL)
*  *--E301077,62 (End)

*  *--E301077,62 (Start)
*  *IF USED(lcALLGL)
*  *  USE IN (lcALLGL)
*  *ENDIF    &&IF USED(lcALLGL)
*  =gfCloseFile(lcAllGL)
*  *--E301077,62 (End)
  
*ENDIF     &&IF glQuitting
*--E301077,62 (End)

*!*************************************************************
*! Name      : lfvComp
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Validat the selected company
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvComp()
*!*************************************************************

FUNCTION lfvComp

IF laComp[lnComp,2] <> 'NOCOMP'
  llNothing = lfGLFiles(ALLTRIM(laComp[lnComp,2]))
ENDIF  

*!*************************************************************
*! Name      : lfvLinkTxt
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Validat the link type .
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvLinkTxt()
*!*************************************************************

FUNCTION lfvLinkTxt

laData[1] = laType[lnLinkTxt,2]

*-- this means that 'Select Link type' is found in the popup
*   and it is not the selected item
IF ASCAN(laType,'00') <> 0 AND !(laType[lnLinkTxt,2] == '00')
  DIMENSION laType[ALEN(laType,1)-1,2]
  SHOW GET lnLinkTxt
ENDIF   &&IF ASCAN(laType,'00') <......

IF llSetUp
  SELECT(lcKeyCat)
  SET FILTER TO LinkType = laData[1]
  SHOW GET pbGlAcc ENABLE
  SHOW GET lcGLAcc ENABLE
  GO TOP
  SHOW WINDOW (lcBrTtl) REFRESH SAME
ENDIF     &&IF llSetUp

*!*************************************************************
*! Name      : lfvData_2
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Validat the screen key laData[2] .
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfTypeArr(),gfSeekRec(),lfGLLinkBrw()
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvData_2()
*!*************************************************************

FUNCTION lfvData_2

PRIVATE llOption,lcLinkType,lcFltExp

llOption = .F. 
lcLinkType = ''

IF !MDOWN()
  *E801897,1 Validate that a link code is not less than 6 characters
  IF LEN(ALLTRIM(laData[2])) < 6 AND !llBrowse AND !("?" $ laData[2])
    *-- "GL Link code coud not be less than 6 characters"
    *-- <Browse> , <Reenter>
    IF gfModalGen("QRM00346B00014","Dialog") = 1
      llBrowse = .T.
    ELSE
      laData[2] = ''
      _CUROBJ = _CUROBJ
    ENDIF
  ENDIF       
  *E801897,1 end
  IF llBrowse OR '?' $ laData[2] OR (!EMPTY(laData[2]) AND laType[lnLinkTxt,2]=='00')
    llNothing = lfGLLinkBrw()
    laData[1] = IIF(llNothing,&lcBaseGL..LinkType ,laData[1])  
    laData[2] = IIF(llNothing,&lcBaseGL..Link_Code,'')
    llBrowse  = .F.
    IF !llNothing
      _CUROBJ = _CUROBJ
    ENDIF
  ENDIF       &&IF llBrowse OR '?' $ laData[2] OR......

  lcLinkType = laData[1]

  IF !EMPTY(laData[2])
    llOption = gfSeekRec()
  ENDIF     &&!EMPTY(laData[2])

  lnLastKey = LASTKEY()

  IF (EMPTY(laData[2]) AND lnLastKey = 13) OR llOption 

    laData[1] = lcLinkType

    IF EMPTY(laData[2])
      IF EMPTY(gcAct_Comp)
        lnComp = ALEN(laComp,1)
        STORE 'DISABLE' TO lcTypeStat,lcLinkStat
      ELSE
        STORE 'ENABLE' TO lcTypeStat,lcLinkStat
      ENDIF      &&IF EMPTY(gcAct_Comp)

      SHOW GET lnLinkTxt &lcTypeStat
      SHOW GET pbLink    &lcLinkStat
      SHOW GET laData[2] &lcLinkStat
  
      _CUROBJ = OBJNUM(laData[2])
    ELSE
      _CUROBJ = OBJNUM(lcTypeDesc)
    ENDIF
  ENDIF       &&IF EMPTY(laData[2]) AND LASTKEY() = 13
 
  IF gcAct_Appl = "SM"
    SHOW GET lnComp
  ENDIF
ENDIF
lcTypDesc = laType[lnLinkTxt,1] + ' ' + ALLTRIM(laData[2])
=lfRefresh(lcGlLink1)

*!*************************************************************
*! Name      : lfGLCpyScr
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Copying information from another GL.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfGLCpyScr()
*!*************************************************************

FUNCTION lfGLCpyScr

*-- lcWName  variable that hold the SMGLLNKC Screen name
lcWName  = "SMGLCPY"
*-- lcWTitl  variable that hold the SMGLLNKC Screen Title
lcWTitl  = "Copy Information From G/L Link code"
llBrowse = .F.

PUSH KEY     
DO (gcScrDir + gcWinAppl + '\SMGLLNKC.SPR')                                  
POP KEY                                        

*!*************************************************************
*! Name      : lfvCpyLink
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Validate the Gl Link code (lcGLCpyLink Field)
*!             in Copying information screen 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvCpyLink()
*!*************************************************************

FUNCTION lfvCpyLink

lnCurAlias = SELECT(0)
IF llBrowse OR !SEEK(laData[1]+lcCpyLink,lcBaseGL)
  llPckCopy = .T.
  llBrowse  = .F.
  lcCpyLink = IIF(lfGLLinkBrw(),&lcBaseGL..Link_Code,SPACE(6))
ENDIF      &&IF llBrowse OR !SEEK(laData[1]+.........
SELECT(lnCurAlias)

*!*************************************************************
*! Name      : lfGLCpyAct
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Trap the keys in screen SMGLLNKC
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfGLCpyAct()
*!*************************************************************

FUNCTION lfGLCpyAct

*-- This function called in activate snippet for screen packScr
*-- which use to copy data from another pack id

ON KEY LABEL ESCAPE DO lfGLCpyEsc

*!*************************************************************
*! Name      : lfGLCpyEsc
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Trap the Esacpe Key in screen SMGLLNKC
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfGLCpyEsc()
*!*************************************************************

FUNCTION lfGLCpyEsc

ON KEY LABEL ESCAPE

_CUROBJ = OBJNUM(pbCancel)
KEYBOARD "{ENTER}" CLEAR PLAIN

*!*************************************************************
*! Name      : lfGLLinkBrw
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Browse  existing link codes
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfGLLinkBrw()
*!*************************************************************

FUNCTION lfGLLinkBrw

PRIVATE lcFields,laBrow,lnCurAlias,lcCurTag,llReturn,lcTag,lcBrFields,lcFile_Ttl
DIMENSION laBrow[1]
STORE SPACE(0) TO lcFields,laBrow
llReturn = .F.

lnCurAlias = SELECT(0)

lcFields    = "linktype,link_code"

lcBrFields = "Type = IIF(ASCAN(laType,&lcBaseGL..LinkType)>0,laType[ASUBSCRIPT(laType,ASCAN(laType,&lcBaseGL..LinkType),1),1],'') :H='Type':15,"+;
             "Link_Code    :H='Link code',"+;
             "Description=&lcBaseGL..LinkDesc :H='Description'"

lcFile_Ttl  = 'Link codes'

SELECT (lcBaseGL)
LOCATE
DECLARE laTemp[1]

DO CASE
  CASE gcAct_Appl = "AR"
    lcBrFor = "FOR INLIST(LinkType,'01','02') AND !DELETED()"
  CASE gcAct_Appl = "SO"
    lcBrFor = "FOR INLIST(LinkType,'01','02') AND !DELETED()"
  CASE gcAct_Appl = "PO"
    lcBrFor = "FOR INLIST(LinkType,'05') AND !DELETED()"
  CASE gcAct_Appl = "MA"
    lcBrFor = "FOR INLIST(LinkType,'04','05') AND !DELETED()"
  CASE gcAct_Appl = "MF"
    lcBrFor = "FOR INLIST(LinkType,'05') AND !DELETED()"
  CASE gcAct_Appl = "IC"
    lcBrFor = "FOR INLIST(LinkType,'03') AND !DELETED()"
  CASE gcAct_Appl = "SM"
    lcBrFor = "FOR INLIST(LinkType,'01','02','03','04','05') AND !DELETED()"
ENDCASE    

llReturn  = gfBrows(IIF(laData[1]='00' OR EMPTY(laData[1]),lcBrFor,"laData[1]"), 'linktype', 'laTemp')

SELECT(lnCurAlias)

RETURN llReturn

*!*************************************************************
*! Name      : lfvTypeDesc
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Validate the link type Account description
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfvTypeDesc()
*!*************************************************************

FUNCTION lfvTypeDesc

PRIVATE lnRecNo

SELECT(lcKeyCat)
lnRecNo = RECNO(lcKeyCat)
REPLACE ALL LinkDesc WITH lcTypeDesc,;
            cStatus  WITH IIF(nRecNo=0,'A','M')

IF lnRecNo<=RECCOUNT(lcKeyCat)
  GOTO lnRecNo
ENDIF  

*!*********************************************************
*! Name      : lfvGLAcc
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Validate the GL Account
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfvGLAcc()
*!*************************************************************

FUNCTION lfvGLAcc

IF !llOtherVer AND (laScrMode[3] OR laScrMode[4])
  lcGLAcc = &lcKeyCat..GLAcnt

  llEmptyAcc = lcEmptyMas = lcGLAcc
  IF llEmptyAcc
    lcGLAcc = ''
  ENDIF

  IF !llOtherVer AND !llEmptyAcc AND !SEEK(ALLTRIM(lcGLAcc),lcGLChart)
    llBrowse  = .F.
    SELECT (lcGLChart)
    GO TOP
    IF EOF()
      *-- There are no records to browse.
      *-- OK
      = gfModalGen("INM44032B00000","Dialog")  
      _CUROBJ = _CUROBJ
    ELSE
      llNoThing = lfGLChrBrw()
      IF !llNothing 
        SELECT (lcKeyCat)
        lcGLAcc = lcOldVal
        REPLACE GLAcnt WITH lcOldVal
        _CUROBJ = _CUROBJ
      ENDIF
    ENDIF
  
  ENDIF    && IF !llOtherVer AND (llBrowse OR (!EMPTY(lcGLAcc) AND.........

  IF !(lcGLAcc == lcOldVal)
    = gfUpdate()
      SELECT (lcKeyCat)
      IF &lcKeyCat..nRecNo = 0
        REPLACE &lcKeyCat..cStatus   WITH 'A'      ,;
                &lcKeyCat..GLAcnt    WITH lcGLAcc  ,;
                &lcKeyCat..LinkDesc  WITH lcTypeDesc
      ELSE
        REPLACE &lcKeyCat..cStatus   WITH 'M',;
                &lcKeyCat..GLAcnt    WITH lcGLAcc
      ENDIF

      SHOW WINDOW (lcBrTtl) REFRESH SAME
  ENDIF  &&IF !(lcGLAcc == lcOldVal)
ELSE

  SELECT (lcKeyCat)
  IF &lcKeyCat..nRecNo = 0
    REPLACE &lcKeyCat..cStatus   WITH 'A'
  ELSE
    REPLACE &lcKeyCat..cStatus   WITH 'M'
  ENDIF

ENDIF &&IF !llOtherVer  

*!*************************************************************
*! Name      : lfGLChrBrw
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Browse account chart
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfGLChrBrw()
*!*************************************************************

FUNCTION lfGLChrBrw

PRIVATE lcFields,laBrow,lnCurAlias,lcCurTag,llReturn,lcTag,lcBrFields,lcFile_Ttl
DIMENSION laBrow[1]
STORE SPACE(0) TO lcFields,laBrow
llReturn = .F.
DIMENSION laTemp[1]

lnCurAlias = SELECT(0)

lcFields    = lcCodeFld+','+lcDescFld

lcBrFields  = lcCodeFld + ":H='GL Account'," + lcDescFld + ":H='Description'"

lcFile_Ttl  = 'GL Accounts'

SELECT (lcGLChart)

llReturn = AriaBrow(.F.,lcFile_Ttl,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,.F.,.F.,lcFields,"laBrow",.F.,lcGLChart,.F.)

IF llReturn
  lcGLAcc = laBrow[1,1]
  lcGlDesc= laBrow[1,2]
ENDIF

SELECT(lnCurAlias)

RETURN llReturn

*!*************************************************************
*! Name      : lfCatBrow
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Category keys for a link code
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfCatBrow()
*!*************************************************************

FUNCTION lfCatBrow

lnCurAlias = SELECT(0)
SELECT (lcKeyCat)
GO TOP

lnBrRecNo = RECNO()

IF llOtherVer
  BROWSE FIELDS cMarker =IIF(RECNO()=lnBrRecNo,'>',' ')  :H=' ':R:1:W=.F.,;
                CatgKey  :W=.F.:R   :H="Category Key",;
                CatgDesc :W=.F.:R:28:H="Description" ,;
                Glacnt   :W=(laScrMode[3] OR laScrMode[4]) AND lfOldValue():P=lcAcMask:H="GL. Account" :V=lfvGLAcc() :F,;
                GlaccDesc:W=(laScrMode[3] OR laScrMode[4]) AND lcGLVer= 'O':40:H="Description"  ;
         SAVE NOWAIT NOAPPEND NODELETE NOMENU NOCLEAR ;
         TITLE(lcBrTtl) WHEN lfwBrow() VALID :F lfVBrow()    ;         
         WINDOW (lcGlLink2) IN WINDOW (gcBaseWind)         
ELSE
  BROWSE FIELDS cMarker =IIF(RECNO()=lnBrRecNo,'>',' ')  :H=' ':R:1:W=.F.,;
                CatgKey  :W=.F.:R   :H="Category Key",;
                CatgDesc :W=.F.:R:28:H="Description" ,;
                Glacnt   :W=(laScrMode[3] OR laScrMode[4]) AND lfOldValue():P=lcAcMask:H="GL. Account" :V=lfvGLAcc() :F,;
                GLAccDsc=IIF(laScrMode[1],SPACE(40),IIF(SEEK(&lcKeyCat..Glacnt,lcGLChart),;
                             EVAL(lcGLChart+'.'+lcDescFld),''));
                             :R:40:H="Description" ;
         SAVE NOWAIT NOAPPEND NODELETE NOMENU NOCLEAR ;
         TITLE(lcBrTtl) WHEN lfwBrow() VALID :F lfVBrow()    ;         
         WINDOW (lcGlLink2) IN WINDOW (gcBaseWind)         
ENDIF

= lfwBrow()

*!*************************************************************
*! Name      : lfOldValue
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : To save the old value befor modifying it
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfOldValue()
*!*************************************************************

FUNCTION lfOldValue

lcOldVal = EVAL(VARREAD())

*!*************************************************************
*! Name      : lpShow
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Handling the screen mode
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfWHBrow,lfwBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpShow
*!*************************************************************

PROCEDURE lpShow

PRIVATE lnCurAlias,lcFilToUse,lcFltExp
lcFilToUse = IIF(laScrMode[2],lcBaseGL, lcKeyCat)

lnCurAlias = SELECT(0)

DO CASE
  *-- Select mode
  CASE laScrMode[1]
    IF llSetUp
      glQuitting = .T.
      CLEAR READ
    ELSE
      CREATE TABLE (gcWorkDir + lcKeyCat);
       (Link_Code C(6),LinkType C(2),LinkDesc C(30),CatgKey C(3),;
        CatgDesc C(30),GLAcnt C(24),GLAccDesc C(40),nRecNo N(4),cStatus C(1))
    
      STORE SPACE(0) TO lcTypeDesc,lcGLAcc,lcGLDesc,lcCpyLink
      STORE "DISABLE" TO lcGLDscStat,lcTypeStat,lcLinkStat
      STORE .F. TO llChldComp
      STORE 0 To lnBrRecNo
      *-- MAN
      lcTypDesc = 'Description'
      =lfRefresh(lcGlLink1)

      IF EMPTY(gcAct_Comp)
        STORE 'DISABLE' TO lcTypeStat,lcLinkStat
      ELSE
        STORE 'ENABLE' TO lcTypeStat,lcLinkStat
      ENDIF    &&IF EMPTY(gcAct_Comp)
    
      = lfTypeArr()
      IF EMPTY(gcAct_Comp)
        = lfGLFiles()
      ENDIF   &&IF EMPTY(gcAct_Comp) OR llSetUp
    ENDIF   &&IF llSetUp
    ACTIVATE WINDOW (lcGlLink1)
    IF gcAct_Appl = 'SM'
      IF llSetUp
        _CUROBJ = OBJNUM(lnLinkTxt)
      ELSE
        _CUROBJ = OBJNUM(lnComp)
      ENDIF
    ELSE
      _CUROBJ = OBJNUM(lnLinkTxt)
    ENDIF

  *-- View mode
  CASE laScrMode[2]
    SELECT (lcBaseGL)
    SCATTER FIELDS &lcScFields TO laData
    lcTypeDesc = laData[3]
    laData[3]  = &lcALLGL..LinkDesc
	lnLinkTxt  = ASUBSCRIPT(laType,ASCAN(laType,laData[1]),1)
    STORE "DISABLE" TO lcGLDscStat,lcTypeStat,lcLinkStat
    
  *-- Edit mode
  CASE laScrMode[3]
    lcTypeDesc = laData[3]
    laData[3]  = &lcALLGL..LinkDesc
    lcGLDscStat  = IIF(lcGLVer=='O','ENABLE','DISABLE')
    STORE "DISABLE" TO lcTypeStat,lcLinkStat

  *-- Add mode
  CASE laScrMode[4]
    IF llSetup
      laData[2] = 'DEFDEF'
      STORE 'Default G/L. link code' TO laData[3],lcTypeDesc
      STORE "ENABLE"  TO lcTypeStat
      STORE "DISABLE" TO lcLinkStat
    ELSE
      STORE "DISABLE" TO lcTypeStat,lcLinkStat

      SELECT (lcBaseGL)
      lcFltExp = SET('FILTER')
      SET FILTER TO 
      IF SEEK('00'+laData[2])
        laData[3]  = &lcALLGL..LinkDesc
        SHOW GET laData[3] DISABLE
      ENDIF
      SET FILTER TO &lcFltExp

    ENDIF   &&IF llSetup
    
    *-- "Do you wish to copy from another G/L. Link code."
    *-- <YES>, <NO>
    IF !llSetUp AND SEEK(laData[1],lcBaseGL) AND gfModalGen("INM00295B00006","Dialog") = 1
      = lfGLCpyScr()
    ENDIF
    
    lcGLDscStat  = IIF(lcGLVer=='O','ENABLE','DISABLE')

ENDCASE

SHOW GET lnLinkTxt &lcTypeStat
SHOW GET pbLink    &lcLinkStat
SHOW GET laData[2] &lcLinkStat

IF laScrMode[2] OR laScrMode[4]
  IF !llSetUp
    lcLinkCode  = IIF(EMPTY(lcCpyLink),laData[2],lcCpyLink)
    lcSelCode   = IIF(EMPTY(lcCpyLink),'&lcBaseGL..Link_Code','laData[2]')
    lcRecNoExp  = IIF(EMPTY(lcCpyLink),'RECNO()','0')
    lcStatusExp = IIF(EMPTY(lcCpyLink),"'S'","'A'")
    SET ORDER TO 0 IN (lcBaseGL)
    
    *E301349,1 (Start)
    *SELECT EVAL(lcSelCode) AS Link_Code,LinkType,LinkDesc,CatgKey,CatgDesc,;
           GLAcnt,GLAccDesc,EVAL(lcRecNoExp) AS nRecNo,EVAL(lcStatusExp) AS cStatus ;
      FROM (lcBaseGL) INTO DBF (gcWorkDir + lcKeyCat);
      WHERE LinkType+Link_Code = laData[1]+lcLinkCode;
    UNION ;
    SELECT laData[2],laData[1],lcTypeDesc,CatgKey,CatgDesc,;
           SPACE(24),SPACE(40),0,'S';
      FROM SyGlCatg;
      WHERE CatgType = laData[1] AND CatgKey NOT IN;
      (SELECT CatgKey FROM (lcBaseGL);
       WHERE LinkType+Link_Code = laData[1]+lcLinkCode)
    
    SELECT EVAL(lcSelCode) AS Link_Code,LinkType,LinkDesc,CatgKey,CatgDesc,;
           GLAcnt,GLAccDesc,EVAL(lcRecNoExp) AS nRecNo,EVAL(lcStatusExp) AS cStatus ;
      FROM (lcBaseGL) INTO DBF (gcWorkDir + lcKeyCat);
      WHERE LinkType+Link_Code = laData[1]+lcLinkCode;
    UNION ;
    SELECT laData[2],laData[1],lcTypeDesc,CatgKey,CatgDesc,;
           SPACE(24),SPACE(40),0,'S';
      FROM SyGlCatg;
      WHERE !EMPTY(CatgType) ;
      AND   CatgType = laData[1] AND CatgKey NOT IN;
            (SELECT CatgKey FROM (lcBaseGL);
             WHERE LinkType+Link_Code = laData[1]+lcLinkCode)
    *E301349,1 (End)
  ELSE
    *E301349,1 (Start)
    *SELECT laData[2] AS Link_Code,CatgType AS LinkType,;
           lcTypeDesc AS LinkDesc,CatgKey,CatgDesc,;
           SPACE(24) AS GLAcnt,SPACE(40) AS GLAccDesc,0 AS nRecNo,;
           'S' AS cStatus ;
      FROM SyGLCatg INTO DBF (gcWorkDir + lcKeyCat)

    SELECT laData[2] AS Link_Code,CatgType AS LinkType,;
           lcTypeDesc AS LinkDesc,CatgKey,CatgDesc,;
           SPACE(24) AS GLAcnt,SPACE(40) AS GLAccDesc,0 AS nRecNo,;
           'S' AS cStatus ;
      FROM SyGLCatg INTO DBF (gcWorkDir + lcKeyCat) ;
      WHERE !EMPTY(CatgType)
    *E301349,1 (End)
    SELECT (lcKeyCat)
    SET FILTER TO LinkType = laData[1]
    SHOW GET pbGlAcc DISABLE
    SHOW GET lcGLAcc DISABLE
  ENDIF      &&IF !llSetUp
  SET ORDER TO GL_Link1 IN (lcBaseGL)
ENDIF   &&IF laScrMode[2] OR laScrMode[4]

SELECT(lcKeyCat)

= lfCatBrow()

SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lpSavscr
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : To make local save.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : DO lpSavscr
*!*************************************************************

PROCEDURE lpSavscr

PRIVATE llContinue

llContinue = .T.

SELECT(lcKeyCat)
SET FILTER TO

SCAN 
  IF EMPTY(GLAcnt) AND ;
     (LinkType <> '05' OR (LinkType = '05' AND !(CatgKey $ '022,023,024,025,026')))
    llContinue = .F.
    *-- GL Account for any category can not be left empty
    *-- <OK>
    = gfModalGen("INM00294B00000","Dialog")  
    laData[1] = &lcKeyCat..LinkType
    SHOW GET laData[1]
    lnLinkTxt = ASUBSCRIPT(laType,ASCAN(laType,laData[1]),1)
    SET FILTER TO LinkType = laData[1]
    EXIT
  ENDIF
  IF !llOtherVer AND !SEEK(ALLTRIM(GLAcnt),lcGLChart)
    llContinue = .F.
    *-- Account x not found in chart of account.
    *-- <OK>
    = gfModalGen("INM00316B00000","Dialog",ALLTRIM(GLAcnt))
    laData[1] = &lcKeyCat..LinkType
    SHOW GET laData[1]
    lnLinkTxt = ASUBSCRIPT(laType,ASCAN(laType,laData[1]),1)
    SET FILTER TO LinkType = laData[1]
    EXIT
  ENDIF
ENDSCAN

IF llContinue
  SELECT (lcBaseGL)
  lcActFltr = SET('FILTER')
  SET FILTER TO
  SET ORDER TO GL_Link1 IN (lcBaseGL)
  IF !SEEK('00'+laData[2],lcBaseGL)
    APPEND BLANK
  ENDIF
  GATHER FROM laData FIELDS &lcScFields
  REPLACE LinkType WITH "00"

  SET ORDER TO GL_Link IN (lcBaseGL)
  =gfTmp2Mast(lcBaseGL,lcKeyCat,'','')
  SET ORDER TO GL_Link1 IN (lcBaseGL)
  =SEEK(laData[1]+laData[2],lcBaseGL)
  SET FILTER TO &lcActFltr.
ENDIF  &&IF llContinue

llCSave = llContinue

*!*************************************************************
*! Name      : lpDelScr
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : To make local delete.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : DO lpDelScr
*!*************************************************************

PROCEDURE lpDelScr

PRIVATE lnCurAlias,lcTag,lnCurRec,llOpen,lnI,llContinue

llContinue = .T.

lnCurAlias = SELECT(0)

IF ALLTRIM(laData[2]) == 'DEFDEF'
  *-- Cannot delete the default link code.
  *-- <OK>
  = gfModalGen("INM00300B00000","Dialog")  
  llContinue = .F.
ELSE
  *-- this part for checking the existence of the link code in the
  *   mentioned files in laGLFiles array

  *-- Deleting this code requires checking its checking its existence
  *-- on some files. This process may take a few minutes.
  *--<Check>,<Ignore cheking>
  IF gfModalGen("INM00296B00032","Dialog") = 1
    DIMENSION laGLFiles[16]
    STORE SPACE(0) TO laGLFiles
    laGLFiles[01] = 'Customer'
    laGLFiles[02] = 'Style'
    laGLFiles[03] = 'Fabric'
    laGLFiles[04] = 'Debit'
    laGLFiles[05] = 'Credit'
    laGLFiles[06] = 'InvHdr'
    laGLFiles[07] = 'InvtAdj'
    laGLFiles[08] = 'CtktBOM'
    laGLFiles[09] = 'CutTktH'
    laGLFiles[10] = 'ARHist'
    laGLFiles[11] = 'RetHdr'
    laGLFiles[12] = 'StyDye'
    laGLFiles[13] = 'FabDye'
    laGLFiles[14] = 'PosHdr'
    laGLFiles[15] = 'FInvtAdj'
    laGLFiles[16] = 'MMfgOrdH'
    FOR lnI = 1 TO ALEN(laGLFiles)
      IF FILE(gcDataDir+laGLFiles[lnI]+'.DBF')
        llOpen = gfOpenFile(gcDataDir+laGLFiles[lnI])
        *SELECT(laGLFiles[lnI])
        IF INLIST(UPPER(laGLFiles[lnI]),'STYDYE','FABDYE','WAREHOUS')
          LOCATE FOR GL_Link   = laData[2]
        ELSE
          LOCATE FOR Link_Code = laData[2]
        ENDIF
        
        IF FOUND(laGLFiles[lnI])
          
          *--E301077,62 (Start)
          *IF llOpen
          *  USE IN laGLFiles[lnI]
          *ENDIF
          = gfCloseFile(laGLFiles[lnI])
          *--E301077,62 (End)


          *-- This Link code is used in one or more of the data files,
          *-- cannot delete.
          *-- <OK>
          = gfModalGen("INM00297B00000","Dialog")  
          llContinue = .F.
          EXIT
        ELSE
          *--E301077,62 (Start)
          *IF llOpen
          *  USE IN laGLFiles[lnI]
          *ENDIF       &&IF llOpen
          = gfCloseFile(laGLFiles[lnI])
          *--E301077,62 (End)

        ENDIF      &&IF FOUND(laGLFiles[lnI])
      ENDIF
    ENDFOR     &&FOR lnI = 1 TO ALEN(laGLFiles)
  ENDIF    &&IF gfModalGen("INM00296B00032","Dialog") = 1
ENDIF

IF llContinue
  SELECT (lcBaseGL)
  lcTag = ORDER(lcBaseGL)

  SET ORDER TO 0 
  lnCurRec = RECNO(lcBaseGL)
  SCAN FOR LinkType+Link_Code = laData[1]+laData[2]
    BLANK  
    DELETE
  ENDSCAN
  IF lnCurRec<=RECCOUNT(lcBaseGL)
    GOTO lnCurRec
  ENDIF    &&IF lnCurRec<=RECCOUNT(lcBaseGL)

  SET ORDER TO GL_Link IN (lcBaseGL)
  IF SEEK(laData[2],lcBaseGL)
    LOCATE REST WHILE Link_Code = laData[2] FOR !(LinkType=='00')
    IF !FOUND(lcBaseGL)
      = SEEK(laData[2],lcBaseGL)
      BLANK
      DELETE
    ENDIF
  ENDIF 
  SET ORDER TO lcTag IN (lcBaseGL)
  SELECT(lcKeyCat)
  DELETE ALL
  SHOW WINDOW (lcBrTtl) REFRESH SAME
  *-- Return to "SELECT" mode
  laScrMode        = .F.
  laScrMode[1]     = .T.
  SELECT(lnCurAlias)
ENDIF  &&IF llContinue

SELECT (lcBaseFile)

*!*************************************************************
*! Name      : lfTrap
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : TO Assign functions to some keys to not affect the browse
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfBrTab,lfBrBack
*!*************************************************************
*! Passed Parameters  : NONE 
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfTrap()
*!*************************************************************

FUNCTION lfTrap

*-- THIS is function is called in deactivate snippet of the screen
*-- if the screen on top is the browse screen assign fuction to the key

IF WONTOP()  = lcBrTtl
  glFromBrow = .T.
  ON KEY LABEL TAB     DO lfBrTab
  ON KEY LABEL BACKTAB DO lfBrBack
ENDIF

*!*************************************************************
*! Name      : lfClrTrap
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Clearing the previous trapping
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfClrTrap()
*!*************************************************************

FUNCTION lfClrTrap

*-- THIS is function is called in activate snippet of the screen
*-- if the screen on top is not the browse screen restore 
*-- the previous on key label 

IF glFromBrow
  =gfStopBrow()
ENDIF  

ON KEY LABEL TAB
ON KEY LABEL BACKTAB

*!*************************************************************
*! Name      : lfBrTab
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Trap the Tab Key
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfBrTab()
*!*************************************************************

FUNCTION lfBrTab

ON KEY LABEL TAB
ACTIVATE WINDOW ('gwcContrl1')
_CUROBJ = OBJNUM(pbTop)

*!*************************************************************
*! Name      : lfBrBack
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Trap the BackTab Key
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfBrBack()
*!*************************************************************

FUNCTION lfBrBack

ON KEY LABEL BACKTAB

DO CASE
  CASE laScrMode[1]
    ACTIVATE WINDOW (lcGlLink1)
    IF llSMMOD AND gcAct_Appl = "SM"
      _CUROBJ = OBJNUM(lnComp)
    ELSE  
      _CUROBJ = OBJNUM(laData[2])
    ENDIF  
  CASE laScrMode[2]
    ACTIVATE WINDOW ('gwcContrl1')
    _CUROBJ = OBJNUM(pbCls)
  CASE laScrMode[3]
    ACTIVATE WINDOW (lcGlLink1)
    IF llSMMOD AND gcAct_Appl = "SM"
      _CUROBJ = OBJNUM(lnComp)
    ELSE  
      _CUROBJ = OBJNUM(laData[3])
    ENDIF  
  CASE laScrMode[4]
    ACTIVATE WINDOW (lcGlLink1)
    IF llSMMOD AND gcAct_Appl = "SM"
      _CUROBJ = OBJNUM(lnComp)
    ELSE  
      _CUROBJ = OBJNUM(laData[3])
    ENDIF  
ENDCASE    

*!*************************************************************
*! Name      : lfwBrow
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : adjust the label of pbsel button
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfwBrow()
*!*************************************************************

FUNCTION lfwBrow

lnBrRecNo = RECNO(lcKeyCat)
SHOW WINDOW (lcBrTtl) REFRESH SAME

*!*************************************************************
*! Name      : lfvBrow
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : TO CHECK IF comming from browse to call gfStopBrow() function
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvBrow()
*!*************************************************************

FUNCTION lfvBrow

IF WONTOP() # (lcBrTtl)
  =gfStopBrow()
ENDIF

*!*************************************************************
*! Name      : lfTypeArr
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : prepare the Link Type arry according to the calling module
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfTypeArr()
*!*************************************************************

FUNCTION lfTypeArr

DO CASE
  CASE gcAct_Appl = "AR"
    DIMENSION laType[3,2]
    laType[1,1] = "Customer"
    laType[1,2] = "01"
    laType[2,1] = "Sales"
    laType[2,2] = "02"
    laType[3,1] = "Select link type"
    laType[3,2] = "00"
  CASE gcAct_Appl = "SO"
    DIMENSION laType[3,2]
    laType[1,1] = "Customer"
    laType[1,2] = "01"
    laType[2,1] = "Sales"
    laType[2,2] = "02"
    laType[3,1] = "Select link type"
    laType[3,2] = "00"
  CASE gcAct_Appl = "PO"
    DIMENSION laType[2,2]
    laType[1,1] = "Work in process"
    laType[1,2] = "05"    
    laType[2,1] = "Select link type"
    laType[2,2] = "00"
  CASE gcAct_Appl = "MA"
    DIMENSION laType[3,2]
    laType[1,1] = "Material"
    laType[1,2] = "04"    
    laType[2,1] = "Work in process"
    laType[2,2] = "05"    
    laType[3,1] = "Select link type"
    laType[3,2] = "00"
  CASE gcAct_Appl = "MF"
    DIMENSION laType[2,2]
    laType[1,1] = "Work in process"
    laType[1,2] = "05"    
    laType[2,1] = "Select link type"
    laType[2,2] = "00"
  CASE gcAct_Appl = "IC"
    DIMENSION laType[2,2]
    laType[1,1] = "Style"
    laType[1,2] = "03"    
    laType[2,1] = "Select link type"
    laType[2,2] = "00"
  CASE gcAct_Appl = "SM"
    DIMENSION laType[6,2]
    laType[1,1] = "Customer"
    laType[1,2] = "01"    
    laType[2,1] = "Sales"
    laType[2,2] = "02"
    laType[3,1] = "Style"        
    laType[3,2] = "03"            
    laType[4,1] = "Material"            
    laType[4,2] = "04"                
    laType[5,1] = "Work in process"    
    laType[5,2] = "05"        
    laType[6,1] = "Select link type"
    laType[6,2] = "00"
ENDCASE

lnLinkTxt = ASUBSCRIPT(laType,ASCAN(laType,'00'),1)
SHOW GET lnLinkTxt &lcTypeStat

*!*************************************************************
*! Name      : lfGLFiles
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : open the GL_Link file account chart file according to 
*!             the GL version(ARIA,SBT,Others) and to the active company
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfGLFiles()
*!*************************************************************

FUNCTION lfGLFiles
PARAMETERS lcSelComp

PRIVATE llContinue,lcComp,lcSelComp

llContinue = .T.

*--E301077,68 (Start)
*IF USED(lcGLChart)
*  USE IN (lcGLChart)
*ENDIF
=gfCloseFile(lcGLChart)
*--E301077,69 (End)

*--E301077,62 (Start)
*IF USED(lcBaseGL)
*  USE IN (lcBaseGL)
*ENDIF
=gfCloseFile(lcBaseGl)
*--E301077,62 (End)

*--E301077,62 (Start)
*IF USED(lcALLGL)
*  USE IN (lcALLGL)
*ENDIF
=gfCloseFile(lcAllGL)
*--E301077,62 (End)
  
IF llSetUp
  STORE 'DISABLE' TO lcTypeStat,lcLinkStat
  laCtrStat[10]  =  "DISABLE" 
  SHOW GET pbBrws  DISABLE
ELSE
  
  IF EMPTY(gcAct_comp)
    STORE 'DISABLE' TO lcTypeStat,lcLinkStat,lcpbBrwsSt
  ELSE
    STORE 'ENABLE' TO lcTypeStat,lcLinkStat,lcpbBrwsSt
  ENDIF
  laCtrStat[10]  = lcpbBrwsSt 
  SHOW GET pbBrws  &lcpbBrwsSt
  
ENDIF  &&IF EMPTY(lcCompID) OR !llGL_Link

lcComp  = IIF(TYPE('lcSelComp')=='C',lcSelComp,IIF(llSetUp,lcSetupCom,gcAct_Comp))

IF gcAct_Appl == 'SM' 
  SELECT SycComp.cComp_ID+"-"+SycComp.cCom_Name,SycComp.cComp_ID;
   FROM SycComp INTO ARRAY laComp

  DIMENSION laComp[ALEN(laComp,1)+1,2]
  laComp[ALEN(laComp,1),1] = 'Select a company'
  laComp[ALEN(laComp,1),2] = 'NOCOMP'
ENDIF  &&IF gcAct_Appl == 'SM' AND !llSetUp

IF !EMPTY(lcComp)
  llGL_Link  = ALLTRIM(UPPER(gfGetMemVar('M_Link_GL',lcComp)))   = 'Y'
  IF !llSetUp AND !llGL_Link 
    *-- System has not been linked to gl_link yet
    *-- <OK>
    = gfModalGen("INM00292B00000","Dialog")  
    llContinue = .F.
    lcBaseFile = ''
    IF gcAct_Appl == 'SM' 
      lnComp = ASUBSCRIPT(laComp,ASCAN(laComp,'NOCOMP'),1)
      SHOW GET lnComp
      STORE 'DISABLE' TO lcTypeStat,lcLinkStat
      SHOW GET lnLinkTxt &lcTypeStat
      SHOW GET pbLink    &lcLinkStat
      SHOW GET laData[2] &lcLinkStat
    ENDIF
  ELSE
    STORE 'ENABLE' TO lcTypeStat,lcLinkStat
    SHOW GET lnLinkTxt &lcTypeStat
    SHOW GET pbLink    &lcLinkStat
    SHOW GET laData[2] &lcLinkStat
  ENDIF
ELSE
  DIMENSION laData[3]
  STORE SPACE(0) TO laData
  IF gcAct_Appl == 'SM'
    lnComp = ASUBSCRIPT(laComp,ASCAN(laComp,'NOCOMP'),1)
  ENDIF
ENDIF     &&IF !EMPTY(gcAct_Comp)

IF llContinue AND !EMPTY(lcComp)
  
  llChldComp = SEEK(lcComp,'SycComp') AND !EMPTY(SycComp.cCompPrnt)
  *E301098,1 Hesham (Start)
  *lcDataDir  = IIF(SEEK(lcComp,'SycComp'),ALLTRIM(SycComp.cCom_dDir),'')
  lcDataDir  = IIF(SEEK(lcComp,'SycComp'),gfGetDataDir(ALLTRIM(SycComp.cCom_dDir)),'')
  *E301098,1 Hesham (End)
  lcPrntCo   = IIF(llChldComp,ALLTRIM(SycComp.cCompPrnt),'')
  lcGLVer    = ALLTRIM(UPPER(gfGetMemVar('M_GL_VERS',lcComp)))
  lcGLCo     = ALLTRIM(UPPER(gfGetMemVar('M_GL_CO',lcComp)))
  
  *E301173,1 (Start)
  IF lcGLVer <> 'S'
    lcGLCo = IIF(llChldComp,ALLTRIM(SyCComp.cCompPrnt),lcComp)
  ENDIF
  *E301173,1 (Start)

  lcGLVer = IIF(llSetUp,lcSULnkVer,lcGLVer)
  
  DO CASE
    *-- GL Version is SBT
    CASE lcGLVer = 'S'
      lcSBTGLDir = ALLTRIM(UPPER(gfGetMemVar('M_SYS_DIR',lcComp)))
      
      *--E301077,62 (Start)
      *USE (lcSBTGLDir+'SYSDATA') IN 0 AGAIN ALIAS (lcSysTmp)
      =gfOpenFile(lcSBTGLDir+'SYSDATA','','SH',@lcSysTmp,.T.)
      *--E301077,62 (End)
        
      SELECT (lcSysTmp)
      LOCATE FOR SYSID = "GL" + lcGLCo
      IF !FOUND()
        *--lcInfoMsg = 'Company not found !!!'
        =gfModalGen('INM00269B00000','DIALOG')
        llContinue = .F.
      ELSE  &&FOUND
        *-- Get path for gl data and company name
        lcGLDir    = ALLTRIM(SUBSTR(DRIVE,61,30))         && DATA DIRECTORY PATH
        lcFile     = "GLACNT"+lcGLCo
        lcPrGLDir  = lcGLDir
      ENDIF
      
      *--E301077,62 (Start)
      *USE IN (lcSysTmp)
      =gfCloseFile(lcSysTmp)
      *--E301077,62 (End)
      
      lcCodeFld   = 'GLACNT'       
      lcDescFld   = 'GLDESC'
      lcGLDscStat = "DISABLE"
      llOtherVer  = .F.

    *-- GL Version is ARIA
    CASE lcGLVer  = 'A'
      *E301098,1 Hesham (Start)
      *lcGLDir     = IIF(SEEK(IIF(llChldComp,lcPrntCo,lcComp),'SycComp'),ALLTRIM(SycComp.cCom_dDir),'')
      lcGLDir     = IIF(SEEK(IIF(llChldComp,lcPrntCo,lcComp),'SycComp'),gfGetDataDir(ALLTRIM(SycComp.cCom_dDir)),'')
      *E301098,1 Hesham (End)
      lcFile      = "GLACCHAR"
      *E301098,1 Hesham (Start)
      *lcPrGLDir   = IIF(SEEK(lcPrntCo,'SycComp'),ALLTRIM(SycComp.cCom_dDir),'')
      lcPrGLDir   = IIF(SEEK(lcPrntCo,'SycComp'),gfGetDataDir(ALLTRIM(SycComp.cCom_dDir)),'')
      *E301098,1 Hesham (End)
      lcCodeFld   = 'CACCTCODE'       
      lcDescFld   = 'CACCNLDES'
      lcGLDscStat = "DISABLE"
      llOtherVer  = .F.

    *-- Other type of GL version
    OTHERWISE
      *E301098,1 Hesham (Start)
      *lcGLDir     = IIF(SEEK(IIF(llChldComp,lcPrntCo,lcComp),'SycComp'),ALLTRIM(SycComp.cCom_dDir),'')
      lcGLDir     = IIF(SEEK(IIF(llChldComp,lcPrntCo,lcComp),'SycComp'),gfGetDataDir(ALLTRIM(SycComp.cCom_dDir)),'')
      *E301098,1 Hesham (End)
      lcFile      = ''      
      lcCodeFld   = ''       
      lcDescFld   = ''
      lcGLDscStat = "ENABLE"
      llOtherVer  = .T.

  ENDCASE

  IF lcGLVer <> 'O'
    IF !FILE(IIF(llChldComp,lcPrGLDir,lcGLDir)+lcFile+'.DBF')
      *-- Chart of account file for this company not found !!!
      *-- <OK>
      = gfModalGen("INM00293B00000","Dialog")  
    ELSE
      *--E301077,68 (Start)
      *USE (IIF(llChldComp,lcPrGLDir,lcGLDir)+lcFile) IN 0 AGAIN ALIAS (lcGLChart) SHARED
      PRIVATE lcChrtFile
      lcChrtFile = IIF(llChldComp,lcPrGLDir,lcGLDir)+lcFile
      =gfOpenFile(lcChrtFile,IIF(lcGLVer = 'S','GLACNT',IIF(lcGLVer = 'A','ACCTCODE','')),'SH',@lcGLChart,.T.)
      *--E301077,62 (End)
      DO CASE
        CASE lcGLVer = 'S'
          SET ORDER TO GLACNT IN (lcGLChart)
        CASE lcGLVer = 'A'
          SET ORDER TO ACCTCODE IN (lcGLChart)        
      ENDCASE
    ENDIF    &&IF !FILE(IIF(llChldComp,lcPrGLDir,lcGLDir)+......
  ENDIF   &&IF lcGLVer <> 'O'
  
  *--E301077,62 (Start)
  *USE (lcDataDir+'GL_Link') IN 0 AGAIN ALIAS (lcBaseGL) ORDER GL_Link1 SHARED
  =gfOpenFile(lcDataDir+'GL_Link','GL_Link1','SH',@lcBaseGL,.T.)
  *--E301077,62 (End)
  
  SELECT (lcBaseGL)
  
  lcArr = ""
  FOR lnI = 1 TO ALEN(laType,1)
    IF laType[lnI,2] <> '00'
      lcArr = lcArr + IIF(EMPTY(lcArr),"'"+laType[lnI,2]+"'",','+"'"+laType[lnI,2]+"'")
    ENDIF
  ENDFOR
  lcFltExp = "INLIST(LinkType," + lcArr + ")"

  SET FILTER TO &lcFltExp.
    
  *--E301077,62 (Start)
  *USE (lcDataDir+'GL_Link') IN 0 AGAIN ALIAS (lcALLGL) ORDER GL_Link1 SHARED
  =gfOpenFile(lcDataDir+'GL_Link','GL_Link1','SH',@lcALLGL,.T.)
  *--E301077,62 (End)
  
  SELECT (lcBaseGL)
  SET RELATION TO '00'+Link_Code INTO (lcALLGL)

  lcBaseFile = lcBaseGL
  lcScFields = "LinkType,Link_Code,LinkDesc"
  SCATTER FIELDS &lcScFields TO laData BLANK
  SHOW GET pbLink    &lcLinkStat
  SHOW GET laData[2] &lcLinkStat

  IF lcGLVer $ 'AO'
    *--E301077,62 (Start)
    *USE (lcGLDir+"ACCOD") IN 0 AGAIN ALIAS GLMask ORDER AccSegNo
    =gfOpenFile(lcGLDir+"ACCOD","AccSegNo",'SH',@lcGLMask,.T.)
    *--E301077,62 (End)
    SELECT (lcGLMask)
    GOTO TOP
    lcRep     = IIF(lcGLVer = "A", "9", "X")
    lcAcMask  = "X" + SUBSTR(STRTRAN(ALLTRIM(cAcsMask),"#",lcRep),2)
    lnAcLen   = LEN(ALLTRIM(lcAcMask))
    *--E301077,62 (Start)
    *USE IN (lcGLMask)
    =gfCloseFile(lcGLMask)
    *--E301077,62 (End)
  ELSE
    lcAcntStrc = lcSBTGLDir + "\GLDATA\GLSTRU" + lcGLCo + ".DBF"
    lcAcMask   = SPACE(0)
    
    *--E301077,62 (Start)
    *USE (lcAcntStrc) IN 0 AGAIN ALIAS (lcGLAccStr) ORDER SegID
    =gfOpenFile(lcAcntStrc,'','SH',@lcGLAccStr,.T.)
    SET ORDER TO SegID IN (lcGLAccStr)
    *--E301077,62 (End)
    
    SELECT (lcGLAccStr)
    SCAN FOR SegLen > 0
      lcAcMask = lcAcMask + IIF(EMPTY(lcAcMask),"","-") + ALLTRIM(SegMask)
    ENDSCAN
    lnAcLen   = LEN(ALLTRIM(lcAcMask))
    *--E301077,62 (Start)
    *USE IN (lcGLAccStr)
    =gfCloseFile(lcGLAccStr)
    *--301077,62 (End)
  ENDIF

  lcEmptyMas = lcAcMask
  lcEmptyMas = STRTRAN(lcEmptyMas,'A',' ')
  lcEmptyMas = STRTRAN(lcEmptyMas,'X',' ')
  lcEmptyMas = STRTRAN(lcEmptyMas,'9',' ')
  lcEmptyMas = STRTRAN(lcEmptyMas,'#',' ')
  lcEmptyMas = STRTRAN(lcEmptyMas,'!',' ')
  lcEmptyMas = STRTRAN(lcEmptyMas,'*',' ')
  lcEmptyMas = STRTRAN(lcEmptyMas,'.',' ')
  lcEmptyMas = STRTRAN(lcEmptyMas,';',' ')
  *-- Padding with 24 char. refers to glaccount field lenght
  lcEmptyMas = PADR(lcEmptyMas,24)

  IF llSetUp
    lnComp = ASUBSCRIPT(laComp,ASCAN(laComp,lcComp),1)
  ELSE
    IF gcAct_Appl == 'SM'
      lnComp = ASUBSCRIPT(laComp,IIF(EMPTY(lcComp),;
                                     ASCAN(laComp,'NOCOMP'),;
                                     ASCAN(laComp,lcComp)),1)
    ENDIF
  ENDIF
  
  IF TYPE("lcSelComp") = 'C'
    = lfCatBrow()
  ENDIF

ENDIF

RETURN llContinue
