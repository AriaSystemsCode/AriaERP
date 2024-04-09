*:************************************************************************
*:
*: Procedure file: SMFISCY.RPR
*:
*:         System: ARIA BUSINESS SYSTEM
*:         Module: General Ledger
*:         Author: Albert Raif
*:      Copyright (c) 
*:  Last modified:14/04/94
*:
*:  Procs & Funcs: 
*:
*:************************************************************************
*MOD: 
*E301053,1 AMM 11/04/98 use DBFS instead of the old SYSFILES in S/M reports
*E300789,1 Hesham 03/04/99
*E300789,1 remove field ccomp_id from files ACCOD,FISHD,FSPRD,FSHLD,CODES
*E300789,1 Change Any seeking in these files
*:************************************************************************
*** Report Setup
DECLARE laRpDays[7]
laRpDays[1] = 'Mon.'
laRpDays[2] = 'Tue.'
laRpDays[3] = 'Wed.'
laRpDays[4] = 'Thu.'
laRpDays[5] = 'Fri.'
laRpDays[6] = 'Sat.'
laRpDays[7] = 'Sun.'
*lcRpFiles  = "SYCCOMP,SYCFISHD,SYCFSHLD,SYCFSPRD"  && Get slected files name
*E301053,1 AMM start, Open the files of the selected company here not by 
*E301053,1 AMM        the option grid.
SELECT SYCCOMP
*E300789,1 Hesham (Start)
SET ORDER TO TAG CCOMP_ID
*E300789,1 Hesham (End)
=SEEK(lcRpCompId)

llFISHD = gfOpenFile(ALLTRIM(SYCCOMP.cCom_DDir) + 'FISHD','','SH')
llFSPRD = gfOpenFile(ALLTRIM(SYCCOMP.cCom_DDir) + 'FSPRD','','SH')
llFSHLD = gfOpenFile(ALLTRIM(SYCCOMP.cCom_DDir) + 'FSHLD','','SH')
*E301053,1 AMM end
DO CASE
  CASE lcRpForm = 'SMFISCYH'
    *E301053,1 AMM start, change file name to the new one
    *lcFilName = 'SYCFSHLD'
    lcFilName = 'FSHLD'
    *E301053,1 AMM end
  CASE lcRpForm = 'SMFISCYS'
    *E301053,1 AMM start, change file name to the new one
    *lcFilName = 'SYCFISHD'
    lcFilName = 'FISHD'
    *E301053,1 AMM end
  CASE lcRpForm = 'SMFISCYP'
    *E301053,1 AMM start, change file name to the new one
    *lcFilName = 'SYCFSPRD'
    lcFilName = 'FSPRD'
    *E301053,1 AMM end
ENDCASE



SELECT (lcFilName)
SET ORDER TO 1
*E300789,1 Hesham (Start)
*SET RELATION TO &lcFilName..ccomp_id INTO SYCCOMP ADDITIVE
*E300789,1 Hesham (End)
DO gfDispRe WITH EVAL('lcRpForm'),IIF(!EMPTY(lcRpExp),'For ','')+lcRpExp
SET RELATION TO

*E301053,1 AMM start, Close files
IF llFISHD .AND. USED('FISHD') 
  USE IN FISHD
ENDIF
IF llFSPRD .AND. USED('FSPRD') 
  USE IN FSPRD
ENDIF
IF llFSHLD .AND. USED('FSHLD') 
  USE IN FSHLD
ENDIF
*E301053,1 AMM end

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
* EOF


*!************************************************************************
*!
*!      FUNCTION lfvComp
*!
*!************************************************************************
*
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

*!*************************************************************
*! Name      : lfDefComp                              
*! Developer : Ahmed Mohamed Ibrahim
*! Date      : 11/04/1998
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

*E301053,1 AMM IF Statment to check if there is an active company
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
*! Date      : 11/04/1998
*! Purpose   : When function for the field Company ID
*! Ref.      : *E301053,1 AMM
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
*
FUNCTION lfwCompId

lcRpOldCom = EVALUATE(SYS(18))     && Varible to hold the old Company ID

*!*************************************************************
*! Name      : lfvCompId                              
*! Developer : Ahmed Mohamed Ibrahim
*! Date      : 11/04/1998
*! Purpose   : Valid function for the field Company ID
*! Ref.      : *E301053,1 AMM
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
*
FUNCTION lfvCompId

lcObjName = SYS(18)  && Varible to hold the name of the current object varible

*E301053,1 AMM IF Statment to check if the Company ID was empty
IF EMPTY(EVALUATE(SYS(18)))
  &lcObjName = lcRpOldCom     && Restore the old value
ENDIF    && End of IF

