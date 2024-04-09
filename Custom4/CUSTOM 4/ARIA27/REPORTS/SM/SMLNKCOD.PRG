*:***************************************************************************
*: Program file  : SMLNKCOD
*: Program desc. : Print link codes
*: For Report    : 
*: System        : Aria Advantage Series.
*: Module        : SYSTEM MANAGER (SM)
*: Developer     : AHMED AMER (AHM)
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO SMLNKCOD
*:***************************************************************************

SET ORDER TO 0 IN GL_LINK

*-- lcFileName Variable that hold account chart file name
*-- lcFieldNam Variable that hold account description field name
*-- lcGLVer    Variable that hold G/L Link version

XFNAME    = lcFileName
XDFIELD   = lcFieldNam
X_GL_VERS = lcGLVer

LinkTmp   = gfTEMPNAME()

lcLinkType = lcRPLnkTyp
XLINK_CODE = lcRPLnkCod

SELECT GL_LINK
GO TOP
***PRINT THE G.L. LINKS FILE
lcCondition = "LinkType <> '00'"
DO CASE
CASE EMPTY(XLINK_CODE) .AND. !EMPTY(lcLinkType)
 lcCondition = lcCondition + " .AND. LINKTYPE = lcLinkType"
CASE EMPTY(LCLINKTYPE) .AND. !EMPTY(XLINK_CODE)
 lcCondition = lcCondition + " .AND. LINK_CODE = XLINK_CODE"
CASE !EMPTY(LCLINKTYPE) .AND. !EMPTY(XLINK_CODE)
 lcCondition = lcCondition + " .AND. LINK_CODE+LINKTYPE = XLINK_CODE+lcLinkType"
ENDCASE
SELECT * FROM Gl_Link ;
  WHERE &lcCondition ;
  ORDER BY Link_Code,LinkType ;
INTO DBF (gcWorkDir+LinkTmp)

PAGENO  = 0
XTIME   = TIME()
XREPORT = ' '
XTITLE  = lcRPTitle
R_WIDTH = 'W'
R_TITLE = 'GENERAL LEDGER LINK FILE'
ROW = 99

SET DEVICE TO PRINT
DO lpPrint
DO ENDREPORT
SET DEVICE TO SCREEN

*!*************************************************************
*! Name      : lpPrint
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : Print report
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpPrint
*!*************************************************************

PROCEDURE lpPrint

lcLinkCode = SPACE(3)

DO WHILE !EOF()
  IF ROW > 50
    ROW = 1
    PAGENO = PAGENO + 1
    DO RPT_HDR WITH XREPORT,XTITLE,R_WIDTH
    ROW = 5
  ENDIF
  IF lcLinkCode <> Link_Code
    ROW = ROW + 1
    lcLinkCode = Link_Code
    lcLinkType = SPACE(2)
    @ ROW,01 SAY 'LINK CODE :'
    @ ROW,13 SAY LINK_CODE
    @ ROW,22 SAY 'DESCRIPTION :'
    @ ROW,37 SAY LINKDESC
    ROW = ROW + 1
    @ ROW,00 SAY REPLICATE('-',132)
    ROW = ROW + 1
    @ ROW,01 SAY 'KEY ARIA TRANSACTION               G/L ACCOUNT              DESCRIPTION'
    ROW = ROW + 1
    @ ROW,00 SAY REPLICATE('-',132)
    ROW = ROW + 1
  ENDIF
  IF lcLinkType <> LinkType
    lcLinkType = LinkType
    lcTypeDesc = laAllType[ASUBSCRIPT(laAllType,ASCAN(laAllType,lcLinkType),1),1]
    @ ROW+1,01 SAY 'TYPE : ' + lcTypeDesc
    ROW = ROW + 3
  ENDIF
  SCAN WHILE lcLinkCode+LCLINKTYPE = LINK_CODE+LINKTYPE .AND. ROW <= 50
    @ ROW,01 SAY CATGKEY
    @ ROW,05 SAY CATGDESC
    @ ROW,36 SAY GLACNT
    IF X_GL_VERS <> 'O'
      SELECT &XFName
      IF SEEK(&LinkTmp..GLACNT)
        XDESC1 = &XDField
      ELSE
        XDESC1 = SPACE(53)
      ENDIF
    ELSE
      XDESC1 = GLACCDESC
    ENDIF
    @ ROW,61 SAY XDESC1
    ROW = ROW + 1
  ENDSCAN
ENDDO

*!*************************************************************
*! Name      : lfvLnkCod
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : Validate link code
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfvLnkCod()
*!*************************************************************

FUNCTION lfvLnkCod

lcCodeVar = VARREAD()
lcCodeVal = &lcCodeVar

IF !EMPTY(lcCodeVal)
  IF !SEEK(IIF(EMPTY(lcRPLnkTyp),'00',lcRPLnkTyp)+ALLTRIM(lcCodeVal)),'GL_Link')
    llNothing  = lfGLLinkBrw()
    &lcCodeVar = IIF(llNothing,GL_LINK.Link_Code,'')
  ENDIF
ENDIF  

*!*************************************************************
*! Name      : lfGLLinkBrw
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : Browse link codes
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfGLLinkBrw()
*!*************************************************************

FUNCTION lfGLLinkBrw

PRIVATE lcFields,laBrow,lnCurAlias,lcCurTag,llReturn,lcTag,lcBrFields,lcFile_Ttl
DIMENSION laBrow[1]
STORE SPACE(0) TO lcFields,laBrow
llReturn = .F.

lnCurAlias = SELECT(0)
lcTag      = ORDER()

lcFields    = "linktype,link_code"

lcBrFields = "Type = IIF(ASCAN(laAllType,GL_LINK.LinkType)>0,laAllType[ASUBSCRIPT(laAllType,ASCAN(laAllType,GL_LINK.LinkType),1),1],'') :H='Type':15,"+;
             "Link_Code    :H='Link code',"+;
             "Description=GL_LINK.LinkDesc :H='Description'"

lcFile_Ttl  = 'Link codes'

SELECT GL_LINK
SET ORDER TO GL_LINK1
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
    lcBrFor = "FOR LinkType <> '00' AND !DELETED()"
ENDCASE    

llReturn  = gfBrows(IIF(EMPTY(lcRPLnkTyp),lcBrFor,"lcRPLnkTyp"), 'linktype', 'laTemp')

SELECT(lnCurAlias)
SET ORDER TO (lcTag)

RETURN llReturn

*!*************************************************************
*! Name      : lfAllTypes
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : Prepare types arrays at option grid calling
*!             according to the module that is called from
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfAllTypes()
*!*************************************************************

FUNCTION lfAllTypes

DO CASE
  CASE gcAct_Appl = "AR"
    DIMENSION laType[3,1]
    DIMENSION laTypeRet[3,1]
    laType[1,1]    = "ALL"
    laTypeRet[1,1] = ""
    laType[2,1]    = "Customer"
    laTypeRet[2,1] = "01"
    laType[3,1]    = "Sales"
    laTypeRet[3,1] = "02"
  CASE gcAct_Appl = "SO"
    DIMENSION laType[3,1]
    DIMENSION laTypeRet[3,1]
    laType[1,1]    = "ALL"
    laTypeRet[1,1] = ""
    laType[2,1]    = "Customer"
    laTypeRet[2,1] = "01"
    laType[3,1]    = "Sales"
    laTypeRet[3,1] = "02"
  CASE gcAct_Appl = "PO"
    DIMENSION laType[2,1]
    DIMENSION laTypeRet[2,1]
    laType[1,1]    = "ALL"
    laTypeRet[1,1] = ""
    laType[2,1]    = "Work in process"
    laTypeRet[2,1] = "05"    
  CASE gcAct_Appl = "MA"
    DIMENSION laType[3,1]
    DIMENSION laTypeRet[3,1]
    laType[1,1]    = "ALL"
    laTypeRet[1,1] = ""
    laType[2,1]    = "Material"
    laTypeRet[2,1] = "04"    
    laType[3,1]    = "Work in process"
    laTypeRet[3,1] = "05"    
  CASE gcAct_Appl = "MF"
    DIMENSION laType[2,1]
    DIMENSION laTypeRet[2,1]
    laType[1,1]    = "ALL"
    laTypeRet[1,1] = ""
    laType[2,1]    = "Work in process"
    laTypeRet[2,1] = "05"    
  CASE gcAct_Appl = "IC"
    DIMENSION laType[2,1]
    DIMENSION laTypeRet[2,1]
    laType[1,1]    = "ALL"
    laTypeRet[1,1] = ""
    laType[2,1]    = "Style"
    laTypeRet[2,1] = "03"    
  CASE gcAct_Appl = "SM"
    DIMENSION laType[6,1]
    DIMENSION laTypeRet[6,1]
    laType[1,1]    = "ALL"
    laTypeRet[1,1] = ""
    laType[2,1]    = "Customer"
    laTypeRet[2,1] = "01"    
    laType[3,1]    = "Sales"
    laTypeRet[3,1] = "02"
    laType[4,1]    = "Style"        
    laTypeRet[4,1] = "03"            
    laType[5,1]    = "Material"            
    laTypeRet[5,1] = "04"                
    laType[6,1]    = "Work in process"    
    laTypeRet[6,1] = "05"        
ENDCASE

DIMENSION laAllType[5,2]
laAllType[1,1] = "Customer"
laAllType[1,2] = "01"    
laAllType[2,1] = "Sales"
laAllType[2,2] = "02"
laAllType[3,1] = "Style"        
laAllType[3,2] = "03"            
laAllType[4,1] = "Material"            
laAllType[4,2] = "04"                
laAllType[5,1] = "Work in process"    
laAllType[5,2] = "05"        

= lfGLFiles()

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

STORE SPACE(0) TO lcTypeDesc,lcGLAcc,lcGLDesc,laComp,lcGLChrTag,;
                  lcGLDir,lcFile,lcGLCo,lcCodeFld,lcDescFld,lcSysDir,;
                  lcGLVer,lcGLCo,lcOldVal,lcPrntCo,lcPrSysDir,;
                  lcPrGLVer,lcPrGLCo,lcPrGLDir,lpopcPrFile,;
                  lcGLChart,lcAcMask
                  
STORE .F. TO llOtherVer,llChldComp

STORE 0 To lnAcLen

PRIVATE llContinue

llContinue = .T.

lcComp  = gcAct_Comp

IF EMPTY(lcComp)
  llContinue = .F.
  *-- You have to select company first
  *-- <OK>
  = gfModalGen("INM00192B00000","Dialog")  
  llContinue = .F.
ENDIF

IF llContinue
  llGL_Link  = ALLTRIM(UPPER(gfGetMemVar('M_Link_GL',lcComp)))   = 'Y'
  IF !llGL_Link 
    *-- System has not been linked to gl_link yet
    *-- <OK>
    = gfModalGen("INM00292B00000","Dialog")  
    llContinue = .F.
  ENDIF
ENDIF

IF llContinue
  
  llChldComp = SEEK(lcComp,'SycComp') AND !EMPTY(SycComp.cCompPrnt)
  lcDataDir  = IIF(SEEK(lcComp,'SycComp'),ALLTRIM(SycComp.cCom_dDir),'')
  lcPrntCo   = IIF(llChldComp,ALLTRIM(SycComp.cCompPrnt),'')
  lcGLVer    = ALLTRIM(UPPER(gfGetMemVar('M_GL_VERS',lcComp)))
  lcGLCo     = ALLTRIM(UPPER(gfGetMemVar('M_GL_CO',lcComp)))

  DO CASE
    *-- GL Version is SBT
    CASE lcGLVer = 'S'
      lcSBTGLDir = ALLTRIM(UPPER(gfGetMemVar('M_SYS_DIR',lcComp)))
      USE (lcSBTGLDir+'SYSDATA') IN 0 AGAIN ALIAS (lcSysTmp)
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
      USE IN (lcSysTmp)
      lcCodeFld   = 'GLACNT'       
      lcDescFld   = 'GLDESC'
      llOtherVer  = .F.

    *-- GL Version is ARIA
    CASE lcGLVer  = 'A'
      lcGLDir     = IIF(SEEK(IIF(llChldComp,lcPrntCo,lcComp),'SycComp'),ALLTRIM(SycComp.cCom_dDir),'')
      lcFile      = "GLACCHAR"
      lcPrGLDir   = IIF(SEEK(lcPrntCo,'SycComp'),ALLTRIM(SycComp.cCom_dDir),'')
      lcCodeFld   = 'CACCTCODE'       
      lcDescFld   = 'CACCNLDES'
      llOtherVer  = .F.

    *-- Other type of GL version
    OTHERWISE
      lcGLDir     = IIF(SEEK(IIF(llChldComp,lcPrntCo,lcComp),'SycComp'),ALLTRIM(SycComp.cCom_dDir),'')
      lcFile      = ''      
      lcCodeFld   = ''       
      lcDescFld   = ''
      llOtherVer  = .T.

  ENDCASE

  IF lcGLVer <> 'O'
    IF !FILE(IIF(llChldComp,lcPrGLDir,lcGLDir)+lcFile+'.DBF')
      *-- Chart of account file for this company not found !!!
      *-- <OK>
      = gfModalGen("INM00293B00000","Dialog")
      llContinue = .F.
    ELSE
      IF USED(lcFile)
        USE IN (lcFile)
      ENDIF
      USE (IIF(llChldComp,lcPrGLDir,lcGLDir)+lcFile) IN 0 AGAIN SHARED
      DO CASE
        CASE lcGLVer = 'S'
          SET ORDER TO GLACNT IN (lcFile)
        CASE lcGLVer = 'A'
          SET ORDER TO ACCTCODE IN (lcFile)        
      ENDCASE
    ENDIF    &&IF !FILE(IIF(llChldComp,lcPrGLDir,lcGLDir)+......
  ENDIF   &&IF lcGLVer <> 'O'
ENDIF  

IF llContinue
  lcFileName = lcFile
  lcFieldNam = lcDescFld
  IF USED ('GL_Link')
    USE IN GL_LINK
  ENDIF
  USE (lcDataDir+'GL_Link') IN 0 ORDER GL_Link1 SHARED 
ELSE
  llOgTrmnat = .T.
  CLEAR READ
ENDIF