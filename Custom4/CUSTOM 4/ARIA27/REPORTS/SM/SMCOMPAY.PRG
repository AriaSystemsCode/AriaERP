*:************************************************************************
*:
*: Procedure file: SMCOMPAY.RPR
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
*E300266,6 Reham On 08/07/95    
*E300266,6 1- In (SMCOMPAD.FRX) Call gfGetAdr() from the FRX in the 
*E300266,6 address lines.
*E300266,6 2- In (SYDREPRT.DBF) in the report rec. put :_
*E300266,6    _ lfCompVald() as a valid function for the report.
*E300266,6    _ lfCompShow() as a show function for the report.
*E301053,1 AMM 11/04/98 use DBFS instead of the old SYSFILES in S/M reports
*:************************************************************************

*E301053,1 AMM Add the filter of the company to the report filter
IF !EMPTY(lcRpCompId)
  lcRpExp = IIF(EMPTY(lcRpExp),lcRpExp,"(" + lcRpExp + ") .AND." )+;
            "(SYCCOMP.cComp_id = '"+lcRpCompId+ "')"
ENDIF
*E301053,1 AMM end
DO CASE
  CASE lcRpForm = "SMCOMPAS"
    
    SELECT SYCCOMP
    SET ORDER TO TAG CCOMP_ID
    SET FILTER TO EMPTY(ccompprnt)
    SET RELATION TO SYCCOMP.cComp_id INTO SYCCOMP_A ADDITIVE
    SET SKIP TO SYCCOMP_A
    
    GO TOP
    DO gfDispRe WITH EVAL('lcRpForm'),IIF(!EMPTY(ALLTRIM(lcRpExp)),"FOR ",'')+lcRpExp
    
    SELECT SYCCOMP
    SET FILTER TO
    SET RELATION TO
  CASE lcRpForm = "SMCOMPAD"
    SELECT SYCCOMP
    GO TOP
    DO gfDispRe WITH EVAL('lcRpForm'),IIF(!EMPTY(ALLTRIM(lcRpExp)),"FOR ",'')+lcRpExp
ENDCASE

*!************************************************************************
*!
*!      FUNCTION lfCompShow
*!
*!************************************************************************
*
FUNCTION lfCompShow

SELECT 0
*E301053,1 AMM Open the file if not opened
IF !USED('SYCCOMP_A')
*E301053,1 AMM end
  USE (gcSysHome+"SYCCOMP") AGAIN ALIAS SYCCOMP_A
  INDEX ON ccompprnt TO &gcWorkDir.&lcRpIndex
  SET ORDER TO TAG &lcRpIndex
ENDIF

*!************************************************************************
*!
*!      FUNCTION lfCompVald
*!
*!************************************************************************
*
FUNCTION lfCompVald

IF USED("SYCCOMP_A")
  SELECT SYCCOMP_A
  USE 
ENDIF
IF USED("SYCCOMP")
  SELECT SYCCOMP
  USE 
ENDIF

ERASE &gcWorkDir.&lcRpIndex+".IDX"

USE (gcSysHome+"SYCCOMP") ALIAS SYCCOMP
SET ORDER TO CCOMP_ID

*!************************************************************************
*!
*!      FUNCTION lfClearRep
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
*! Ref.      : *E301053,1 AMM
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
*! Name      : lfvComp                              
*! Developer : Ahmed Mohamed Ibrahim
*! Date      : 11/05/1998
*! Purpose   : Function to get the default value of the company id
*! Ref.      : *E301053,1 AMM
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Called From  : Option grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  Company ID
*!*************************************************************
*! Example            :  =lfvComp()
*!*************************************************************

FUNCTION lfvComp
DECLARE laRpRetFld(1)
lcOldBrFld    = lcBrFields
lcBrFields    = 'cComp_ID:H="Company ID",cCom_Name:H="Company Name"'
laRpRetFld[1] = ''

&& Check If year field is empty
lcRpCurFld = VARREAD()
IF .NOT. EMPTY(&lcRpCurFld)  
  lcOldAlias = SELECT()    && Save the current alias
  llUesdBefo = .F.        && Check if used before or this the first time
  IF NOT USED("SYCCOMP") 
    SELECT 0
    USE (gcSysHome+'SYCCOMP') ORDER TAG Ccomp_id
    llUesdBefo = .T.
  ENDIF
  SELECT SYCCOMP
  *** Search for the current company+year
    IF ('?' $ &lcRpCurFld. .OR. !SEEK(&lcRpCurFld)) 
        =gfBrows(.F.,'cComp_ID',"laRpRetFld",'Company information',.F.)
      &lcRpCurFld. = laRpRetFld[1]
      SHOW GET (lcRpCurFld)
    ENDIF
  
  IF llUesdBefo       && .F.- this file used by the system
    USE IN SYCFISHD
  ENDIF
  SELECT (lcOldAlias)
ENDIF
RETURN 
