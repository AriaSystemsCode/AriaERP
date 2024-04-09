*:***************************************************************************
*: Program file  : SMLNKCOD
*: Program desc. : Print link codes
*: For Report    : (N000571)
*: System        : Aria Advantage Series.4XP
*: Module        : SYSTEM MANAGER (SM)
*: Developer     : Mariam Mazhar[MMT]
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
#INCLUDE R:\ARIA4XP\REPORTS\SM\SMLNKCOD.h

IF looGscroll.llOGFltCh
  lfCrtTempFile()
  lfCollect()
ENDIF   

DIMENSION loOGScroll.laCRParams[2,2]
*--Optional Title
loOGScroll.laCRParams[1,1] = 'OpTitle'
loOGScroll.laCRParams[1,2] = lcRPTitle 
*--Report Name
loOGScroll.laCRParams[2,1] = 'ReportName'
loOGScroll.laCRParams[2,2] = LANG_Report_Name
*--Group By

SELECT(lcReportFile)&&the file will print from it

IF RECCOUNT()=0   &&if the file is empty
*--no records to display
  =gfModalGen('TRM00052B34000','ALERT')
  RETURN .F.
ENDIF  &&endif the file is not empty

SELECT(lcReportFile)
COPY TO oAriaApplication.WorkDir+lcRepFile+'.DBF' 
DIMENSION loOgScroll.lacrTABLES[1]  && array For Temp Table & pathes 
loOgScroll.lacrTABLES[1]= oAriaApplication.WorkDir+lcRepFile+'.DBF' 

loogScroll.cCROrientation = 'P'

= gfDispRe()
 
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Mariam Mazhar[MMT]
*! Date      : 01/03/2007
*! Purpose   : Report When Function
*!*************************************************************
FUNCTION lfwRepWhen


IF EMPTY(lcRPLnkTyp)
  lcBrowCond = iif(oAriaApplication.ActiveModuleID = "AR","FOR INLIST(LinkType,'01','02') AND !DELETED()",;
  			 iif(oAriaApplication.ActiveModuleID = "SO","FOR INLIST(LinkType,'01','02') AND !DELETED()",;
  			 iif(oAriaApplication.ActiveModuleID = "PO","FOR INLIST(LinkType,'05') AND !DELETED()",;
  			 iif(oAriaApplication.ActiveModuleID = "MA","FOR INLIST(LinkType,'04','05') AND !DELETED()",;
  			 iif(oAriaApplication.ActiveModuleID = "MF","FOR INLIST(LinkType,'05') AND !DELETED()",;
  			 iif(oAriaApplication.ActiveModuleID = "IC","FOR INLIST(LinkType,'03') AND !DELETED()",;
  			 iif(oAriaApplication.ActiveModuleID = "SM","FOR LinkType <> '00' AND !DELETED()",.T.)))))))
ELSE
  lcBrowCond = "FOR LinkType = '"+lcRPLnkTyp+"'"
ENDIF       

lcComp  = oAriaApplication.ActiveCompanyID 

llGL_Link  = ALLTRIM(UPPER(gfGetMemVar('M_Link_GL',lcComp)))   = 'Y'
IF !llGL_Link 
  RETURN .F.
ELSE
  RETURN .T.
ENDIF   



*!*************************************************************
*! Name      : lfAllTypes
*! Developer : Mariam Mazhar[MMT]
*! Date      : 01/03/2007
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
  CASE oAriaApplication.ActiveModuleID = "AR"
    DIMENSION laType[3,1]
    DIMENSION laTypeRet[3,1]
    laType[1,1]    = LANG_ALL
    laTypeRet[1,1] = ""
    laType[2,1]    = LANG_Customer
    laTypeRet[2,1] = "01"
    laType[3,1]    = LANG_Sales
    laTypeRet[3,1] = "02"
    
  CASE oAriaApplication.ActiveModuleID = "SO"
    DIMENSION laType[3,1]
    DIMENSION laTypeRet[3,1]
    laType[1,1]    = LANG_ALL
    laTypeRet[1,1] = ""
    laType[2,1]    = LANG_Customer
    laTypeRet[2,1] = "01"
    laType[3,1]    = LANG_Sales
    laTypeRet[3,1] = "02"
    
  CASE oAriaApplication.ActiveModuleID  = "PO"
    DIMENSION laType[2,1]
    DIMENSION laTypeRet[2,1]
    laType[1,1]    = LANG_ALL
    laTypeRet[1,1] = ""
    laType[2,1]    = LANG_WIP
    laTypeRet[2,1] = "05"    
    
  CASE oAriaApplication.ActiveModuleID = "MA"
    DIMENSION laType[3,1]
    DIMENSION laTypeRet[3,1]
    laType[1,1]    = LANG_ALL
    laTypeRet[1,1] = ""
    laType[2,1]    = LANG_MaTerial
    laTypeRet[2,1] = "04"    
    laType[3,1]    = LANG_WIP
    laTypeRet[3,1] = "05"    
    
  CASE oAriaApplication.ActiveModuleID  = "MF"
    DIMENSION laType[2,1]
    DIMENSION laTypeRet[2,1]
    laType[1,1]    = LANG_ALL
    laTypeRet[1,1] = ""
    laType[2,1]    = LANG_WIP
    laTypeRet[2,1] = "05"    
    
  CASE oAriaApplication.ActiveModuleID  = "IC"
    DIMENSION laType[2,1]
    DIMENSION laTypeRet[2,1]
    laType[1,1]    = LANG_ALL
    laTypeRet[1,1] = ""
    laType[2,1]    = LANG_Style       
    laTypeRet[2,1] = "03"    
    
  CASE oAriaApplication.ActiveModuleID = "SM"
    DIMENSION laType[6,1]
    DIMENSION laTypeRet[6,1]
    laType[1,1]    = LANG_ALL
    laTypeRet[1,1] = ""
    laType[2,1]    = LANG_Customer
    laTypeRet[2,1] = "01"    
    laType[3,1]    = LANG_Sales
    laTypeRet[3,1] = "02"
    laType[4,1]    = LANG_Style       
    laTypeRet[4,1] = "03"            
    laType[5,1]    = LANG_MaTerial         
    laTypeRet[5,1] = "04"                
    laType[6,1]    = LANG_WIP    
    laTypeRet[6,1] = "05"        
ENDCASE

DIMENSION laAllType[5,2]
laAllType[1,1] = LANG_Customer
laAllType[1,2] = "01"    
laAllType[2,1] = LANG_Sales
laAllType[2,2] = "02"
laAllType[3,1] = LANG_Style       
laAllType[3,2] = "03"            
laAllType[4,1] = LANG_MaTerial           
laAllType[4,2] = "04"                
laAllType[5,1] = LANG_WIP   
laAllType[5,2] = "05"        

= lfGLFiles()

*!*************************************************************
*! Name      : lfGLFiles
*! Developer : Mariam Mazhar[MMT]
*! Date      : 01/03/2007
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

lcComp  = oAriaApplication.ActiveCompanyID 

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
      = gfOpenTable(lcSBTGLDir+'SYSDATA','','SH',lcSysTmp)
     *  USE (lcSBTGLDir+'SYSDATA') IN 0 AGAIN ALIAS (lcSysTmp)
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
      = gfOpenTable(IIF(llChldComp,lcPrGLDir,lcGLDir)+ lcFile,'','SH')
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
  = gfOpenTable(oAriaApplication.DataDir + 'GL_Link' ,'GL_Link1','SH')
ELSE
  llOgTrmnat = .T.
  RETURN .F.
ENDIF

*!*************************************************************
*! Name      : lfvLnkCod
*! Developer : Mariam Mazhar[MMT]
*! Date      : 01/03/2007
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


IF EMPTY(lcRPLnkTyp)
  lcBrowCond = iif(oAriaApplication.ActiveModuleID = "AR","FOR INLIST(LinkType,'01','02') AND !DELETED()",;
  			 iif(oAriaApplication.ActiveModuleID = "SO","FOR INLIST(LinkType,'01','02') AND !DELETED()",;
  			 iif(oAriaApplication.ActiveModuleID = "PO","FOR INLIST(LinkType,'05') AND !DELETED()",;
  			 iif(oAriaApplication.ActiveModuleID = "MA","FOR INLIST(LinkType,'04','05') AND !DELETED()",;
  			 iif(oAriaApplication.ActiveModuleID = "MF","FOR INLIST(LinkType,'05') AND !DELETED()",;
  			 iif(oAriaApplication.ActiveModuleID = "IC","FOR INLIST(LinkType,'03') AND !DELETED()",;
  			 iif(oAriaApplication.ActiveModuleID = "SM","FOR LinkType <> '00' AND !DELETED()",.T.)))))))
ELSE
  lcBrowCond = "FOR LinkType = '"+lcRPLnkTyp+"'"
ENDIF          


lnPos = ASCAN(loOGScroll.laogFxflt,'lcRPLnkCod')
IF lnPos <> 0 
  lnPos = ASUBSCRIPT(loOGScroll.laogFxflt,lnPos,1)
  lcSelected = IIF(EMPTY(loOGScroll.laogFxflt[lnPos,6]),'',loOGScroll.laogFxflt[lnPos,6])
  IF !EMPTY(lcSelected) AND USED(lcSelected)
    SELECT (lcSelected)
    ZAP 
  ENDIF 
ENDIF   

*!*************************************************************
*! Name      : lfCollect
*! Developer : Mariam Mazhar[MMT]
*! Date      : 01/03/2007
*! Purpose   : collect report data 
*!*************************************************************
FUNCTION lfCollect
lcCondition = "LinkType <> '00'"
llSelected = .F.
lcSelected  = ''
lnPos = ASCAN(loOGScroll.laogFxflt,'lcRPLnkCod')
IF lnPos <> 0 
  lnPos = ASUBSCRIPT(loOGScroll.laogFxflt,lnPos,1)
  lcSelected = IIF(EMPTY(loOGScroll.laogFxflt[lnPos,6]),'',loOGScroll.laogFxflt[lnPos,6])
  IF !EMPTY(lcSelected) AND USED(lcSelected)
    SELECT(lcSelected)
    LOCATE 
    IF !EOF()
      llSelected = .T.
    ENDIF 
  ENDIF   
ENDIF 

IF llSelected
  SELECT(lcSelected)
  SCAN 
    IF gfSeek(KEYEXP,'Gl_Link','Gl_Link1')
      SELECT Gl_Link
      gfSetorder('GL_LINK')
      gfSeek('')
      SCAN FOR LINKTYPE+LINK_CODE = &lcSelected..KEYEXP AND  LinkType <> '00'
        SCATTER MEMO MEMVAR 
        INSERT INTO (lcReportFile) FROM MEMVAR 
        IF lcGLVer <> 'O'
          SELECT (lcFileName)
          IF gfSEEK(&lcReportFile..GLACNT)
             REPLACE GLACCDESC WITH eval(lcFileName+'.&lcFieldNam')  IN (lcReportFile) 
     	  ELSE
         	REPLACE GLACCDESC WITH  SPACE(53) IN (lcReportFile) 
	     ENDIF
	   ENDIF    
      ENDSCAN 
    ENDIF 
  ENDSCAN 
ELSE
  IF !EMPTY(lcRPLnkTyp) 
    SELECT Gl_Link
    IF gfSeek(lcRPLnkTyp,'Gl_Link','Gl_Link1')  
      SELECT Gl_Link
      gfSetorder('GL_LINK')
      gfSeek('')
      SCAN FOR  LINKTYPE+LINK_CODE = lcRPLnkTyp AND  LinkType <> '00'
        SCATTER MEMO MEMVAR 
        INSERT INTO (lcReportFile) FROM MEMVAR 
        IF lcGLVer <> 'O'
          SELECT (lcFileName)
          IF gfSEEK(&lcReportFile..GLACNT)
             REPLACE GLACCDESC WITH eval(lcFileName+'.&lcFieldNam') IN (lcReportFile) 
     	  ELSE
         	REPLACE GLACCDESC WITH  SPACE(53) IN (lcReportFile) 
	     ENDIF
	   ENDIF    
      ENDSCAN 
    ENDIF 
  ELSE
    SELECT Gl_Link
    gfSetorder('GL_LINK')
    gfSeek('')    
    SCAN FOR LinkType <> '00'
      SCATTER MEMO MEMVAR 
      INSERT INTO (lcReportFile) FROM MEMVAR 
      IF lcGLVer <> 'O'
          SELECT (lcFileName)
          IF gfSEEK(&lcReportFile..GLACNT)
             REPLACE GLACCDESC WITH eval(lcFileName+'.&lcFieldNam')  IN (lcReportFile) 
     	  ELSE
         	REPLACE GLACCDESC WITH  SPACE(53) IN (lcReportFile) 
	     ENDIF
	   ENDIF    

    ENDSCAN 
  ENDIF 
ENDIF 
SELECT Gl_Link
gfSetorder('GL_LINK1')

*!*************************************************************
*! Name      : lfCrtTempFile
*! Developer : Mariam Mazhar[MMT]
*! Date      : 01/03/2007
*! Purpose   : create report temp file 
*!*************************************************************
FUNCTION lfCrtTempFile

IF USED(lcReportFile)
  SELECT(lcReportFile)
  ZAP 
ENDIF 

SELECT Gl_Link
DIMENSION laFileStruct[1,18]
AFIELDS(laFileStruc)
=gfCrtTmp(lcReportFile,@laFileStruc,"LINKTYPE+LINK_CODE",lcReportFile,.T.)

