*:***************************************************************************
*: Program file  : ICSLCRT
*: Program desc. : Scales Report
*: For Report    : (ICSLCRTE.FRX,ICSLCRTN.FRX)
*: System        : Aria Advantage Series.4XP (NEW Framework)
*: Module        : Inventory Control (IC)
*: Developer     : Mariam MAzhar (MMT)
*: Date          : 09/28/2008
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : gfDispRe,gfModalGen,gfRange,lfwOgWhen,lfGetForm.
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO ICSLCRT
*:***************************************************************************
*: This Report Program is due to E301117 ...
*:***************************************************************************
*: Modifications :
*:***************************************************************************
*: B609000,1 AHS Calling the program from server reports folder to collect data and display report it not SAAS[T20090805.0001]
*: E302857,1 HES 02/10/2011 Avoid 'X:\Aria4xp\SRVRPTS' Fixed Path [T20110206.0017]
*:***************************************************************************
lcStTime = TIME()
DIMENSION laPrePaks[1,1]
loogscroll.cCROrientation = 'P'

*B609000,1 AHS Calling the program from server reports folder to collect data and display report it not SAAS[Start]
IF oAriaApplication.MULTIINST
  *: E302857,1 HES 02/10/2011 Avoid Fixed Path ------- BEGIN 
*!*	  SET PROCEDURE TO X:\ARIA4XP\SRVRPTS\IC\ICSCLRT.FXP ADDITIVE
*!*	  DO X:\ARIA4XP\SRVRPTS\IC\ICSCLRT.FXP 
  SET PROCEDURE TO oAriaApplication.CLIENTSRVREPORTHOME+"IC\ICSCLRT.FXP" ADDITIVE
  DO oAriaApplication.CLIENTSRVREPORTHOME+"IC\ICSCLRT.FXP"   
  *: E302857,1 HES 02/10/2011 Avoid Fixed Path ------- END      
ELSE
  lcSrvRpt = STRTRAN(UPPER(oAriaApplication.ReportHome),'REPORTS','SRVRPTS')
  DO lcSrvRpt+"IC\ICSCLRT.FXP" WITH .F.,.F.
ENDIF 
*B609000,1 AHS Calling the program from server reports folder to collect data and display report it not SAAS[End]

*-- end of program code.

*!*************************************************************
*! Name      : lfwOgWhen
*! Developer : Mariam MAzhar (MMT)
*! Date      : 09/28/2008
*! Purpose   : OG when function
*!*************************************************************
*! Called from : OG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : ....
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwOgWhen()
*!*************************************************************
*!
FUNCTION lfwOgWhen
= gfOpenTable(oAriaApplication.DataDir + 'SCALE' ,'SCALE','SH')
= gfOpenTable(oAriaApplication.DataDir + 'SCALEHD' ,'Extscale','SH')
*-- end of lfwOgWhen.

*!*************************************************************
*! Name      : lfFilMmVar
*: Developer     : Mariam MAzhar (MMT)
*: Date          : 09/28/2008
*! Purpose   : Fill Memory variables.
*!*************************************************************
*! Passed Parameters : ....
*!*************************************************************
*! Return      : Null
*!*************************************************************
*! Example     : =lfFilMmVar()
*!*************************************************************
*!
FUNCTION lfFilMmVar
PRIVATE laSetups

DIMENSION laSetups[2,2]
laSetups[1,1] = 'M_USEEXSSC'
laSetups[2,1] = 'M_EXTWIDTH'
=gfGetMemVar(@laSetups)
llExtSize  = laSetups[1,2]
lnExtWidth = laSetups[2,2]
lcRpForm   = IIF(llExtSize,'ICSCLRTE','ICSCLRTN')
*-- end of lfFilMmVar.


*!*************************************************************
*! Name      : lfSScale
*: Developer     : Mariam MAzhar (MMT)
*: Date          : 09/28/2008
*! Purpose   : Scale In Range set function.
*!*************************************************************
*! Passed Parameters : Dummy Parameter for set only.
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : =lfSScale()
*!*************************************************************
*!
FUNCTION lfSScale
PARAMETERS lcDummyPrm
IF llExtSize
  GO TOP IN SCALEHD
ELSE
  GO TOP IN SCALE
ENDIF
*-- end of lfSScale.
*!*************************************************************
*! Name      : lfPackFlag
*: Developer     : Mariam MAzhar (MMT)
*: Date          : 09/28/2008
*! Purpose   : Rising Prepak flag.
*!*************************************************************
*! Passed Parameters : ....
*!*************************************************************
*! Return      : Null
*!*************************************************************
*! Example     : = lfPackFlag()
*!*************************************************************
*!
FUNCTION lfPackFlag
STORE (Type = 'S') TO llPackFlag,llNrfFlag
RETURN ''
*-- end of lfPackFlag.

*!*************************************************************
*! Name      : lfHavePp
*: Developer     : Mariam MAzhar (MMT)
*: Date          : 09/28/2008
*! Purpose   : Rising Prepak flag.
*!*************************************************************
*! Passed Parameters : ....
*!*************************************************************
*! Return      : Null
*!*************************************************************
*! Example     : =lfHavePp()
*!*************************************************************
*!
FUNCTION lfHavePp
llHavePp = .F.
FOR lnI = 1 TO 8
  lcI = STR(lnI,1)
  IF pP&lcI > 0
    llHavePp = .T.
    EXIT
  ENDIF
ENDFOR
RETURN llHavePp
*-- end of lfHavePp.

*!*************************************************************
*! Name      : lfInitVal
*: Developer     : Mariam MAzhar (MMT)
*: Date          : 09/28/2008
*! Purpose   : Initial prepak value.
*!*************************************************************
*! Passed Parameters : ....
*!*************************************************************
*! Return      : Null
*!*************************************************************
*! Example     : =lfInitVal()
*!*************************************************************
*!
FUNCTION lfInitVal
lcPrepak = ''
RETURN lcPrepak
*-- end of lfInitVal.

*!*************************************************************
*! Name      : lfCurrVal
*: Developer     : Mariam MAzhar (MMT)
*: Date          : 09/28/2008
*! Purpose   : Current prepak value.
*!*************************************************************
*! Passed Parameters : ....
*!*************************************************************
*! Return      : Null
*!*************************************************************
*! Example     : =lfInitVal()
*!*************************************************************
*!
FUNCTION lfCurrVal
lcPrepak = PREPAK
RETURN ''
*-- end of lfCurrVal.
