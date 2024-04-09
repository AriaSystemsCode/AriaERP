*:************************************************************************
*: Program file  : SMFRE20.PRG
*: Program desc. : Copying Codes from company to another.
*:                 The program is a conversion from 2.6 ==> 2.7
*: System        : Aria Apparel Series. Ver 2.7
*: Module        : SM
*: Date          : 06/13/2000
*: Developer     : Wael Mohammed Anwar (WMA)
*: Refer to      : C101899
*:************************************************************************
*: Calls : 
*:         Functions  : lfSelComp()
*:                    : lfDefComp()
*:                    : lfvSelComp()
*:                    : lfDataDir()
*:                    : lfSelComT()
*:                    : lfDefComT()
*:                    : lfvSelComT()
*:                    : lfDataDirT()
*:                    : lfChkRp()
*:                    : lfCloseFile()
*:         Procedures : None
*:************************************************************************
*:Passed Parameters : None
*:************************************************************************
*:Example           : Do SMFRE20
*:************************************************************************
*:Modifications     :
*:************************************************************************

STORE .F. TO llClear
STORE '' TO laCompDesc ,laCompCode ,laCompDest ,laCompCodT
STORE '' TO lcRpSelCom ,lcCompArrs ,lcDataDir ,lcRpSelCot ,lcCompArrT ,lcDataDirT

lcExpr = gfOpGrid('SMFRE200' , .T.)  && Run selection grid.

IF EMPTY(lcExpr) 
  IF lcExpr = .F.
    RETURN
  ENDIF
ENDIF

STORE '' TO M_POST_PPRD,M_SYS_DIR,M_GL_VERS,M_GL_CO
= gfGetMemVar('M_POST_PPRD,M_SYS_DIR,M_GL_VERS,M_GL_CO',lcRPSelCom)

= gfOpenFile(lcDataDir+'CODES',lcDataDir+'Ccode_no','SH')
= gfOpenFile(gcSysHome+'SYDFIELD',gcSysHome+'cfld_name','SH')

SELECT 0
USE &lcDataDirT.CODES ALIAS CODE1
SET ORDER TO TAG Ccode_no

SELECT SYDFIELD
SCAN FOR &lcExpr
  SELECT CODES
  IF SEEK("D" + SYDFIELD.Cfld_name,'CODES')
    SCATTER MEMVAR
    IF !SEEK("D" + m.Cfld_name + m.Ccode_no + m.Cdiscrep + m.Crltd_nam,'CODE1')
      WAIT WINDOW 'Copying code : ' + ALLTRIM(m.Cdiscrep) NOWAIT
      SELECT CODE1
      APPEND BLANK
      GATHER MEMVAR
    ELSE
      WAIT WINDOW 'Selected code : ' + ALLTRIM(m.Cdiscrep) + ' , already exist in target Co.' NOWAIT
    ENDIF
  ELSE
    WAIT WINDOW 'Selected code : ' + ALLTRIM(SYDFIELD.CFLD_HEAD) + ' , not found in source Co.' NOWAIT  
  ENDIF
  
  SELECT CODES
  IF SEEK("N" + SYDFIELD.Cfld_name,'CODES')
    SCAN WHILE SYDFIELD.Cfld_name = CODES.Cfld_name  
      SCATTER MEMVAR
      IF !SEEK("N" + m.Cfld_name + m.Ccode_no + m.Cdiscrep + m.Crltd_nam,'CODE1') 
        WAIT WINDOW 'Copying code : ' + ALLTRIM(m.Cdiscrep) NOWAIT
        SELECT CODE1
        APPEND BLANK
        GATHER MEMVAR
      ELSE
        WAIT WINDOW 'Selected code : ' + ALLTRIM(m.Cdiscrep) + ' , already exist in target Co.' NOWAIT
      ENDIF
    ENDSCAN
  ELSE
    WAIT WINDOW 'Selected code : ' + ALLTRIM(SYDFIELD.CFLD_HEAD) + ' , not found in source Co.' NOWAIT  
  ENDIF
ENDSCAN
= gfModalGen(.f.,.f.,.f.,.f.,'Process completed successfully.')
= lfCloseFile()
WAIT CLEAR 
*-- End Of SMFRE20.PRG



*!****************************************************************************
*! Name      : lfSelComp
*! Developer : Wael Mohammed Anwar (WMA)
*! Date      : 06/13/2000
*! Purpose   : To Select Source Co.
*!****************************************************************************
*! Calls              : None
*!****************************************************************************
*! Passed Parameters  : None
*!****************************************************************************
*! Returns            : None 
*!****************************************************************************
*! Example            : =lfSelComp()
*!****************************************************************************
FUNCTION lfSelComp
PRIVATE lnI,lnCurAlias,lcCurTag

*IF TYPE('laCompDesc[1,1]') $ "UL" OR ;
*   (ALEN(laCompDesc,1) = 1 AND EMPTY(laCompDesc[1,1]))
  
  DIMENSION laCompDesc[1,1],laCompCode[1,1]
  STORE SPACE(0) TO laCompDesc[1,1],laCompCode[1,1]
*ENDIF

lnCurAlias = SELECT(0)
SELECT SycComp
lcCurTag = ORDER()
SET ORDER TO Ccomp_id
PRIVATE lnI
lnI = 1
SCAN
  lnI = ALEN(laCompDesc,1) + IIF(EMPTY(laCompDesc[lnI]),0,1)
  DIMENSION laCompDesc[lnI,1],laCompCode[lnI,1]
  laCompDesc[lnI,1] = SycComp.cComp_ID+"-"+SycComp.cCom_Name
  laCompCode[lnI,1] = SycComp.cComp_ID
ENDSCAN

SET ORDER TO &lcCurTag

SELECT (lnCurAlias)
*-- End Of lfSelComp.


*!****************************************************************************
*! Name      : lfDefComp
*! Developer : Wael Mohammed Anwar (WMA)
*! Date      : 06/13/2000
*! Purpose   : Select Default Source Co.
*!****************************************************************************
*! Calls              : None
*!****************************************************************************
*! Passed Parameters  : None
*!****************************************************************************
*! Returns            : lcRet
*!****************************************************************************
*! Example            : =lfDefComp()
*!****************************************************************************
FUNCTION lfDefComp
PRIVATE lcRet

IF EMPTY(gcAct_Comp)
  DIMENSION laCompDesc[ALEN(laCompDesc,1)+1,1],laCompCode[ALEN(laCompCode,1)+1,1]
  = AINS(laCompDesc , 1)
  = AINS(laCompCode , 1)
  laCompDesc[1,1] = "Select source company"
  laCompCode[1,1] = "NoComp"
  lcRet = "NoComp"
ELSE
  lcRet = gcAct_Comp
  = lfDataDir(lcRet)
ENDIF

RETURN lcRet
*-- End Of lfDefComp.


*!****************************************************************************
*! Name      : lfvSelComp
*! Developer : Wael Mohammed Anwar (WMA)
*! Date      : 06/13/2000
*! Purpose   : Open Code File For Selected Co.
*!****************************************************************************
*! Calls              : None
*!****************************************************************************
*! Passed Parameters  : None
*!****************************************************************************
*! Returns            : None
*!****************************************************************************
*! Example            : =lfvSelComp
*!****************************************************************************
FUNCTION lfvSelComp

= lfDataDir(lcRPSelCom)
*-- End Of lfvSelComp.


*!****************************************************************************
*! Name      : lfDataDir
*! Developer : Wael Mohammed Anwar (WMA)
*! Date      : 06/13/2000
*! Purpose   : Get Data Dir.
*!****************************************************************************
*! Calls              : None
*!****************************************************************************
*! Passed Parameters  : lcComp
*!****************************************************************************
*! Returns            : None
*!****************************************************************************
*! Example            : =lfDataDir()
*!****************************************************************************
FUNCTION lfDataDir
PARAMETERS lcComp
PRIVATE lcComp,lnCurAlias,lcCurTag

lnCurAlias = SELECT(0)
SELECT SycComp
lcCurTag = ORDER()
SET ORDER TO Ccomp_id
IF SEEK(lcComp)
  lcDataDir = gfGetDataDir(ALLTRIM(SycComp.cCom_dDir))
ENDIF  
SET ORDER TO &lcCurTag
SELECT (lnCurAlias)
*-- End Of lfDataDir.



*!****************************************************************************
*! Name      : lfSelComT
*! Developer : Wael Mohammed Anwar (WMA)
*! Date      : 06/13/2000
*! Purpose   : To Select Target Co.
*!****************************************************************************
*! Calls              : None
*!****************************************************************************
*! Passed Parameters  : None
*!****************************************************************************
*! Returns            : None 
*!****************************************************************************
*! Example            : =lfSelComT()
*!****************************************************************************
FUNCTION lfSelComT
PRIVATE lnI,lnCurAlias,lcCurTag

*IF TYPE('laCompDesT[1,1]') $ "UL" OR ;
*   (ALEN(laCompDesT,1) = 1 AND EMPTY(laCompDesT[1,1]))
  
  DIMENSION laCompDesT[1,1],laCompCodT[1,1]
  STORE SPACE(0) TO laCompDesT[1,1],laCompCodT[1,1]
*ENDIF

lnCurAlias = SELECT(0)
SELECT SycComp
lcCurTag = ORDER()
SET ORDER TO Ccomp_id
PRIVATE lnI
lnI = 1
SCAN
  lnI = ALEN(laCompDesT,1) + IIF(EMPTY(laCompDesT[lnI]),0,1)
  DIMENSION laCompDesT[lnI,1],laCompCodT[lnI,1]
  laCompDesT[lnI,1] = SycComp.cComp_ID+"-"+SycComp.cCom_Name
  laCompCodT[lnI,1] = SycComp.cComp_ID
ENDSCAN

SET ORDER TO &lcCurTag

SELECT (lnCurAlias)
*-- End Of lfSelComp.


*!****************************************************************************
*! Name      : lfDefComT
*! Developer : Wael Mohammed Anwar (WMA)
*! Date      : 06/13/2000
*! Purpose   : Select Default Target Co.
*!****************************************************************************
*! Calls              : None
*!****************************************************************************
*! Passed Parameters  : None
*!****************************************************************************
*! Returns            : lcRet
*!****************************************************************************
*! Example            : =lfDefComT()
*!****************************************************************************
FUNCTION lfDefComT
PRIVATE lcRet

DIMENSION laCompDesT[ALEN(laCompDesT,1)+1,1],laCompCodT[ALEN(laCompCodT,1)+1,1]
= AINS(laCompDesT , 1)
= AINS(laCompCodT , 1)
laCompDesT[1,1] = "Select Target Company"
laCompCodT[1,1] = "NoComp"
lcRet = "NoComp"

RETURN lcRet
*-- End Of lfDefComT.


*!****************************************************************************
*! Name      : lfvSelComT
*! Developer : Wael Mohammed Anwar (WMA)
*! Date      : 06/13/2000
*! Purpose   : Call Function to Get Data Dir.
*!****************************************************************************
*! Calls              : None
*!****************************************************************************
*! Passed Parameters  : None
*!****************************************************************************
*! Returns            : None
*!****************************************************************************
*! Example            : =lfvSelComT
*!****************************************************************************
FUNCTION lfvSelComT
PRIVATE lnCurAlias

= lfDataDirT(lcRPSelCoT)
*-- End Of lfvSelComT.


*!****************************************************************************
*! Name      : lfDataDirT
*! Developer : Wael Mohammed Anwar (WMA)
*! Date      : 06/13/2000
*! Purpose   : Get Data Dir.
*!****************************************************************************
*! Calls              : None
*!****************************************************************************
*! Passed Parameters  : lcComp
*!****************************************************************************
*! Returns            : None
*!****************************************************************************
*! Example            : =lfDataDirT()
*!****************************************************************************
FUNCTION lfDataDirT
PARAMETERS lcComp
PRIVATE lcComp,lnCurAlias,lcCurTag

lnCurAlias = SELECT(0)
SELECT SycComp
lcCurTag = ORDER()
SET ORDER TO Ccomp_id
IF SEEK(lcComp)
  lcDataDirT = gfGetDataDir(ALLTRIM(SycComp.cCom_dDir))
ENDIF  
SET ORDER TO &lcCurTag
SELECT (lnCurAlias)
*-- End Of lfDataDir.


*!****************************************************************************
*! Name      : lfChkRp
*! Developer : Wael Mohammed Anwar (WMA)
*! Date      : 06/13/2000
*! Purpose   : Chek if no target company selected 
*!             & if Target company is source company
*!****************************************************************************
*! Calls              : None
*!****************************************************************************
*! Passed Parameters  : 
*!****************************************************************************
*! Returns            : None
*!****************************************************************************
*! Example            : =lfChkRp()
*!****************************************************************************
FUNCTION lfChkRp

IF lcRpSelCom = "NoComp"
  = gfModalGen(.f.,.f.,.f.,.f.,'No source company selected.')
  RETURN .F.
ELSE
  IF lcRpSelCoT = "NoComp"
    = gfModalGen(.f.,.f.,.f.,.f.,'No target company selected.')
    RETURN .F.
  ELSE
    IF lcRpSelCoT = lcRPSelCom
      = gfModalGen(.f.,.f.,.f.,.f.,'Target company cannot be the source company.')
      RETURN .F.
    ENDIF
  ENDIF
ENDIF
*-- End Of lfChkRp.


*!****************************************************************************
*! Name      : lfCloseFile
*! Developer : Wael Mohammed Anwar (WMA)
*! Date      : 06/13/2000
*! Purpose   : Close Files
*!****************************************************************************
*! Calls              : None
*!****************************************************************************
*! Passed Parameters  : 
*!****************************************************************************
*! Returns            : None
*!****************************************************************************
*! Example            : =lfCloseFile()
*!****************************************************************************
FUNCTION lfCloseFile

IF USED('CODES')
  SELECT CODES
  USE
ENDIF 
IF USED('CODE1')
  SELECT CODE1
  USE
ENDIF 
IF USED('SYDFIELD')
  SELECT SYDFIELD
  USE
ENDIF 
*-- End Of lfCloseFile.