*!*****************************************************************************************
*! Name      : RMCRDRVU.PRG
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 07/02/2008
*! Purpose   : CUSTOMIZED DAILY RETURN REGISTER FOR REVUE.
*! Entry no. : C201055
*!*****************************************************************************************
*! Modification
*!*****************************************************************************************

*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
IF oAriaApplication.MULTIINST 
  SET PROCEDURE TO X:\ARIA4XP\SRVRPTS\RM\rmcrdrv.FXP ADDITIVE
  DO X:\ARIA4XP\SRVRPTS\RM\rmcrdrv.FXP  
  
ELSE
  lcSrvRpt = STRTRAN(UPPER(oAriaApplication.ReportHome),'REPORTS','SRVRPTS')
  DO lcSrvRpt+"RM\rmcrdrv.FXP" WITH .F.,.F.
ENDIF   
RETURN 
*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]

llRnFrmOg = IIF(TYPE('llRnFrmOg')='U',.F.,llRnFrmOg)
loogScroll.cCROrientation = 'L'
lcStTime = DATE()
lfCmpExpr()

*:***************************************************************************
*: Name              : lfvArray
*! Developer         : Mariam Mazhar Tawfik [MMT]
*! Date              : 07/02/2008
*: Purpose           : Function to initiate the target and sourece arrays
*:                     in Option Grid
*:***************************************************************************
*: Called from       : SYREPUVR
*:***************************************************************************
*: Calls             : None
*:***************************************************************************
*: Passed Parameters : None
*:***************************************************************************
*: Return		     : None
*:***************************************************************************
*: Example           : = lfvArray()
*:***************************************************************************

FUNCTION lfvArray
PARAMETERS llFromMover


IF !llFromMover
  DIMENSION laRpCompS[1],laRpCompT[1]
  STORE '' TO laRpCompS , laRpCompT
ENDIF 

SELECT SYCCOMP
SELECT DISTINCT Ccomp_id+' - '+Ccom_name ;
       FROM SYCCOMP INTO ARRAY laRpCompS
       
*:***************************************************************************
*: Name              : lfvComp
*! Developer         : Mariam Mazhar Tawfik [MMT]
*! Date              : 07/02/2008
*: Purpose           : Function to call the Mover Window with list ogf all 
*:                     the companies
*:***************************************************************************
*: Called from       : SYREPUVR
*:***************************************************************************
*: Calls             : None
*:***************************************************************************
*: Passed Parameters : None
*:***************************************************************************
*: Return		     : None
*:***************************************************************************
*: Example           : = lfvComp()
*:***************************************************************************

FUNCTION lfvComp

IF EMPTY(laRpCompS[1])
  lfvArray(.T.)
ENDIF 


= lfOGMover(@laRpCompS,@laRpCompT,'Companies',.T.,'')
loogscroll.llOGFltCh = .T.
lfCmpExpr()

*!************************************************************
*! Name      : RefreshStatus
*: Developer : Mariam Mazhar(MMT)
*: Date      : 02/16/2006
*! Purpose   : function to refresh the status option
*!************************************************************
*! Parameters: None
*!************************************************************
*! Returns   : None
*!************************************************************
*!
FUNCTION RefreshStatus
LOCAL lcStatusStr, lnTarget
  lcStatusStr = ""
  IF !EMPTY(laRpCompT)
    FOR lnTarget = 1 TO ALEN(laRpCompT,1)
      lcStatusStr = lcStatusStr + ", " + laRpCompT[lnTarget]
    ENDFOR 
    lcStatusStr = SUBSTR(lcStatusStr,3)
  ENDIF   
  RETURN lcStatusStr
ENDFUNC 

*!***************************************************************************
*! Name      : lfCmpExpr
*: Developer : Adel Mohmmed El Gazzar (ADEL)
*: Date      : 02/12/2002
*! Purpose   : - Evaluate Company expression.
*!***************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!***************************************************************************
*! Called from : lfvCategory(),lfvCompany(),lfwRepWhen()
*!***************************************************************************
*! Passed Parameters  : ....
*!***************************************************************************
*! Returns            : ....
*!***************************************************************************
*! Example   : = lfCmpExpr()
*!***************************************************************************
*E301294,1  AKA 08/23/99  
*!***************************************************************************

FUNCTION lfCmpExpr
PRIVATE laTarget

*laRpCompS , laRpCompT
IF EMPTY(laRpCompT)
  = ACOPY(laRpCompS ,laTarget)
ELSE
  = ACOPY(laRpCompT,laTarget)
ENDIF
= ASORT(laTarget)
lcOldComp = lcRpCmpExp
lcRpCmpExp = ''
FOR lnI = 1 TO ALEN(laTarget,1)
  lcRpCmpExp = IIF(EMPTY(lcRpCmpExp),PADR(laTarget[lnI],2),;
                    lcRpCmpExp + ','+PADR(laTarget[lnI],2))
ENDFOR

IF LEN(lcRpCmpExp) > 2
  llNComp = .T.
ELSE
  llNComp = .F.
ENDIF
CLEARREAD()
*-- end of lfCmpExpr.