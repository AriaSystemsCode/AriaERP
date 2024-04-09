*!********************************************************************
*: Program file  : IcDyeFx.prg
*: Program desc. : fixing 
*: For screen    : None.
*: System        : ARIA APPAREL SERIES
*: Module        : Ic
*: Developer     : Walid A. Wahab (WAB)
*: Refere To     : B#606938
*!********************************************************************
*:Modifications  :
*:*************************************************************************

lcOldPath = SPACE(40)
IF !gfSetup()
  RETURN
ENDIF  


*-- Opening the Sysrates file in order to update it with the new Rates.
*-- Initializing the necessary variables.
lcPathName = SPACE(40)
lcFileName = SPACE(12)
lcStyDyTmp = gfTempName()
lcwarehous = gfTempName()
lcWareCode = SPACE(6) 

lcStyle = PADR('MEMO',12)
lcColor = '000001'
=gfvWareHs('S', lcStyle, lcColor, @lcWareCode)  

DO (gcScrDir+"IC\IcDyeFx.SPX")


*!*************************************************************
*! Name      : lfvProceed
*! Developer : Walid A. Wahab (WAB)
*! Date      : 02/16/2002
*! Purpose   : 
*!*************************************************************
*! Example            :  lfvProceed()
*!*************************************************************
FUNCTION lfvProceed

IF EMPTY(lcPathName)
  *Path name can not be empty.
  =gfModalGen('TRM04074B00000','DIALOG','Path name')
  _CUROBJ = OBJNUM(lcPathName)
  RETURN
ENDIF

lcPathName  = lcOldPath
lnPathName = LEN(lcPathName)
lcPathName = ALLTRIM(SUBSTR(lcPathName,1,RAT('\',lcPathName)))


IF LEN(ALLTRIM(lcPathName)) = 1
  lcPathName = ALLTRIM(lcPathName) + ':'
ENDIF

*wab
lcFileNAme = 'STYDYE.DBF'
IF !FILE(lcPathName+lcFileName)   
  *---"File does not exist. Cannot proceed."
  =gfModalGen('TRM00273B00000','DIALOG')
  _CUROBJ = OBJNUM(lcFileName)
  RETURN
ENDIF
IF lfOpenFiles()
  =lfFxStyDye()
  =gfModalGen('TRM42219B00000','DIALOG','Fixing')
  USE IN ('STYLE')
  USE IN ('STYDYE')
  USE IN ('warehous')
  USE IN (lcStyDyTmp)
  USE IN (lcwarehous)
 
ENDIF

*:*************************************************************
*: Name      : lfvgetFile
*: Developer : Walid A. Wahab - (WAB)
*: Date      : 02/16/2003
*: Purpose   : Function to get the Aria26 data file
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : = lfvgetFile ()
*:*************************************************************
FUNCTION lfvgetFile
PARAMETERS lcVarNam

lcPathName  = GETDIR('','Aria26 company Path ')

lcOldPath = lcPathName

IF EMPTY(lcPathName)
  SHOW GET PbProceed DISABLE
ELSE
  SHOW GET PbProceed ENABLE
ENDIF
SHOW GET lcPathName

*-- End Of lfvgetFile

*:*************************************************************
*: Name      : lfOpenFiles
*: Developer : Walid A. Wahab - (WAB)
*: Date      : 02/16/2003
*: Purpose   : Function to open files
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : = lfOpenFiles ()
*:*************************************************************
FUNCTION lfOpenFiles
llReturn =gfOpenFile(lcPathName+'StyDye','StyDye','SH',lcStyDyTmp,.T.)
llReturn =gfOpenFile(lcPathName+'warehous','warehous','SH',lcwarehous,.T.)

llReturn =gfOpenFile(gcDataDir+'Style','Style','SH')
llReturn =gfOpenFile(gcDataDir+'StyDye','StyDye','SH')
llReturn =gfOpenFile(gcDataDir+'warehous','warehous','SH')
RETURN llReturn


*:*************************************************************
*: Name      : lfFxStyDye
*: Developer : Walid A. Wahab - (WAB)
*: Date      : 02/16/2003
*: Purpose   : Function to fix the stydye records
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : = lfFxStyDye ()
*:*************************************************************
FUNCTION lfFxStyDye

SELECT (lcwarehous)
SCAN
  IF !SEEK(cWareCode,'warehous')
    SCATTER MEMVAR MEMO
    SELECT warehous
    APPEND BLANK
    *--Update Differencies
    =RLOCK() 
    REPLACE CWARECODE  WITH m.cWareCode,;
            CDESC      WITH m.cDesc ,;
            CPHONE     WITH m.cPhone,;
            UPS        WITH m.Ups,;
            CADDRESS1  WITH m.cAddr1  ,;
            CADDRESS2  WITH m.cAddr2  ,;
    	    CADDRESS3  WITH m.cCity   ,;
            CADDRESS4  WITH m.cState  ,;
            CADDRESS5  WITH m.cZip    ,;
            CADDRESS6  WITH m.cCountry,;
            CCONT_CODE WITH gcContCode,;
            LMATINV    WITH .T.,;
            LSTYINV    WITH .T.,;
            GL_LINK    WITH IIF(EMPTY(m.Gl_Link),'DEFDEF',m.Gl_link+IIF(m.Gl_link='DEF','DEF',''))
    UNLOCK
  ENDIF
ENDSCAN
SELECT (lcStyDyTmp)
SCAN
  WAIT WINDOW 'Style : ' + Style+'-'+Color NOWAIT
  IF !EMPTY(cWareCode)
    IF !SEEK(Style+'-'+Color+cwareCode+Dyelot,'StyDye')
      SCATTER MEMVAR
      SELECT StyDye
      APPEND BLANK
      GATHER MEMVAR
      *--Update Differencies
      =RLOCK() 
      REPLACE STYLE   WITH m.Style+'-'+m.Color,;
              GL_LINK WITH IIF(EMPTY(m.GL_Link) OR m.GL_Link='DEF','DEFDEF',m.GL_Link),;
              NSTKVAL WITH m.TotStk*m.Ave_Cost,;
              CAdd_Time WITH IIF(EMPTY(m.Mod_Time),m.Add_Time,m.Mod_Time),;
              Dadd_Date WITH IIF(EMPTY(m.Mod_Date),m.Add_Date,m.Mod_Date),;
              CAdd_User WITH IIF(EMPTY(m.Mod_Userid),m.Add_Userid,m.Mod_Userid)            
      UNLOCK
      SELECT (lcStyDyTmp)
    ENDIF
  ENDIF
ENDSCAN
SELECT WAREHOUS
LOCATE FOR  lStyInv 
IF FOUND()
  lcwarecode=cWareCode
ENDIF
SELECT STYLE
SCAN
  WAIT WINDOW 'Style : ' + Style NOWAIT
  IF EMPTY(cdefware) OR !SEEK(Style,'StyDye')
    lcStyle = Style 
    IF SEEK(lcStyle,'StyDye')
      REPLACE cdefware WITH StyDye.cWareCode
    ELSE
      REPLACE cdefware WITH lcWareCode
      DO gpAdStyWar WITH lcStyle, SPACE(10), lcWareCode      
      =SEEK(lcStyle)
    ENDIF
  ENDIF
ENDSCAN
