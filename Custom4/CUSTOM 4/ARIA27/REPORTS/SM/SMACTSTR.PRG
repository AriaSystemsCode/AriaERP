*:************************************************************************
*:
*: Procedure file: SMACTSTR.PRG
*:
*:         System: ARIA BUSINESS SYSTEM
*:         Module: System Manager
*:         Author: Reham Alallami
*:      Copyright (c)
*:  Last modified:01/04/95
*:
*:  Procs & Funcs: 
*:
*:************************************************************************
*MOD: 
*E301053,1 AMM 11/04/98 use DBFS instead of the old SYSFILES in S/M reports
*E300789,1 Hesham 03/04/99
*E300789,1 remove field ccomp_id from files ACCOD,FISHD,FSPRD,FSHLD,CODES
*E300789,1 Change Any seeking in these files
*B603758,1 RAMY 24/07/2000 Disable the Advanced button in the option grid
*						   Note: the modification is in the file SYDREPRT.DBF only 
*:************************************************************************
SELECT SYCCOMP

SET ORDER TO TAG CCOMP_ID
*E301053,1 AMM START, Open the file of the required company not in the option grid
=SEEK(lcRpCompId)
*E300789,1 Hesham (Start)
*llACCOD = gfOpenFile(ALLTRIM(SYCCOMP.cCom_DDir) + 'ACCOD','COMPID','SH')

*--Ramy thin modification without entry and is done while fixing new relase in 07/17/2000 [START]
IF !USED('ACCOD')
  llACCOD = gfOpenFile(ALLTRIM(SYCCOMP.cCom_DDir) + 'ACCOD' , 'Accsegno' , 'SH')
ELSE
  llACCOD = .T.
  SELECT ACCOD
ENDIF
*--Ramy [end]

*E300789,1 Hesham (End)
*E301053,1 AMM Comment out.
*SELECT SYCACCOD
*SET ORDER TO TAG COMPID
*E301053,1 AMM END
*E300789,1 Hesham (Start)
*SET RELATION TO CCOMP_ID INTO SYCCOMP ADDITIVE
*E300789,1 Hesham (End)

DO gfDispRe WITH EVAL('lcRpForm'),IIF(!EMPTY(ALLTRIM(lcRpExp)),'FOR ','')+lcRpExp

*E301053,1 AMM start, Close the file.
*SELECT SYCACCOD
*SET RELATION TO
IF llACCOD .AND. USED('llACCOD')
  USE IN ACCOD
ENDIF
*E301053,1 AMM end


*!************************************************************************
*!
*!      Function lfClearRep
*!
*!************************************************************************
*
FUNCTION lfClearRep

IF USED(lcRpTargt)
  USE IN ALIAS(lcRpTargt)
ENDIF

*!*************************************************************
*! Name      : lfDefComp                              
*! Developer : Ahmed Mohamed Ibrahim
*! Date      : 11/05/1998
*! Purpose   : Function to get the default value of the company id
*! Ref       : *E301053,1 AMM
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Called From  : Option grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  Company ID
*!*************************************************************
*! Example            :  =lfDefComp()
*!*************************************************************
*
FUNCTION lfDefComp

*IF Statment to check if there is an active company
IF !EMPTY(gcAct_Comp)
  RETURN gcAct_Comp
ELSE    && Else
  SELECT SYCCOMP
  GO TOP
  RETURN SYCCOMP.cComp_Id
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfwCompId                              
*! Developer : Ahmed Mohamed Ibrahim
*! Date      : 11/05/1998
*! Purpose   : When function for the field Company ID
*! Ref       : *E301053,1 AMM
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Called From  : Option grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfwCompId()
*!*************************************************************
*E300643,1 This function was added by HS for the Enhancement
*!*************************************************************
*
FUNCTION lfwCompId

lcRpOldCom = EVALUATE(SYS(18))     && Varible to hold the old Company ID

*!*************************************************************
*! Name      : lfvCompId                              
*! Developer : Ahmed Mohamed Ibrahim
*! Date      : 11/05/1998
*! Purpose   : Valid function for the field Company ID
* Ref        : *E301053,1 AMM
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Called From  : Option grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvCompId()
*!*************************************************************
*E300643,1 This function was added by HS for the Enhancement
*!*************************************************************
*
FUNCTION lfvCompId

lcObjName = SYS(18)  && Varible to hold the name of the current object varible

*IF Statment to check if the Company ID was empty
IF EMPTY(EVALUATE(SYS(18)))
  &lcObjName = lcRpOldCom     && Restore the old value
ENDIF    && End of IF


