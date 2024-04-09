*:****************************************************************
*: Program file  : SMvf2f26.PRG
*: Program desc. : Convert Visual tables From Sbt  to Table Fox pro 26 
*: System        : Aria Apparel System - Version 2.7.
*: Module        : System Manager (SM)
*: Developer     : ABDOU ELGENDI -  (ABD)
*: Date          : 08/06/2003
*: Tracking Job Number : E#302208,1
*:****************************************************************
*: Calls         : 
*:               : FUNCTIONS  :  lpError , g_retsdx.
*:               -----------------------------------------------
*:               : PROCEDURE  : None.
*:****************************************************************
*: Calls From   : SMvf2f26.PRG
*:****************************************************************
*: Passed Parameters  : None.
*:****************************************************************
*:E#302208,1.
*:****************************************************************
*: Note :-
*:      This program wite in Visual Fox Pro to Convert Visual Fox
*:      Pro Tables to Fox Pro 2.6 tables.
*:****************************************************************
*:Modifications  :
*:****************************************************************
*:
*-- lcSbtCompy :- Variable hold the SBT Company code.
*-- lcSbtPath  :- Variable hold the SBT directory.
*-- lcSbtWork  :- Variable Hold The Work Directory.

LPARAMETERS lcMemoFile
IF TYPE('lcMemoFile') # 'C'
	RETURN
ENDIF
RESTORE FROM (lcMemoFile) ADDI

PRIVATE lcOldCurnet_Path , lcOldSafty , lcErrStr
lcOldCurnet_Path = FULLPATH('')
lcOldSafty       = SET("Safety")
SET SAFETY OFF 
lcErrStr = ON('ERROR')
ON ERROR DO lpError


*-- Copy Main Customer File.
IF File(lcSbtPath+'ARCUST'+lcSbtCompy+'.DBF') 
  USE  (lcSbtPath+'ARCUST'+lcSbtCompy+'.DBF') SHARED IN 0
  COPY TO (lcSbtWork+'lcARCUST'+'.DBF') FOX2X
  USE
ENDIF

*-- Copy Store  Customer File.
IF File(lcSbtPath+'ARCADR'+lcSbtCompy+'.DBF')
  USE  (lcSbtPath+'ARCADR'+lcSbtCompy+'.DBF') SHARED IN 0
  COPY TO (lcSbtWork+'lcARCADR'+'.DBF') FOX2X
  USE
ENDIF

*-- Copy Style File.
IF File(lcSbtPath+'ICITEM'+lcSbtCompy+'.DBF')
  USE  (lcSbtPath+'ICITEM'+lcSbtCompy+'.DBF') SHARED IN 0
  COPY TO (lcSbtWork+'lcICITEM'+'.DBF') FOX2X
  USE
ENDIF

IF File(lcSbtPath+'ARMAST'+lcSbtCompy+'.DBF')
  USE  (lcSbtPath+'ARMAST'+lcSbtCompy+'.DBF') SHARED IN 0
  COPY TO (lcSbtWork+'lcARMAST'+'.DBF') FOX2X
  USE
ENDIF

CD (lcOldCurnet_Path)
SET SAFETY &lcOldSafty
ON ERROR &lcErrStr

*-- End OF Code.
*:*************************************************************
*: Name      : lpError
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 08/06/2003
*: Purpose   : Function to handle error.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Called from : Prog.
*:*************************************************************
*: Example   : Do  lpError
*:*************************************************************
*:
PROCEDURE  lpError
RETURN .T.

*--End of lpError
*:*************************************************************
*: Name      : g_retsdx
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 08/06/2003
*: Purpose   : Dummy Function to handle index expration.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Called from : Prog.
*:*************************************************************
*: Example   : = g_retsdx(Company)
*:*************************************************************
*:
FUNCTION g_retsdx
LPARAMETERS lcparam
  
RETURN ''
*:****************************************************************