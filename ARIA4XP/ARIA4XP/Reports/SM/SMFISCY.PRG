*:*********************************************************************************
*: Program file  : SMFISCY.PRG
*: Program desc. : Print Fiscal Calendar Report
*:        System : Aria4 XP.
*:        Module : SM (System Manager).
*:     Developer : Saber A Razek (SAB)
*:     Entry     : E303336
*:*********************************************************************************

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

SELECT SYCCOMP
SET ORDER TO TAG CCOMP_ID
=SEEK(lcRpCompId)

llFISHD = gfOpenFile(ALLTRIM(SYCCOMP.cCom_DDir) + 'FISHD','','SH')
llFSPRD = gfOpenFile(ALLTRIM(SYCCOMP.cCom_DDir) + 'FSPRD','','SH')
llFSHLD = gfOpenFile(ALLTRIM(SYCCOMP.cCom_DDir) + 'FSHLD','','SH')

DO CASE
  CASE lcRpForm = 'SMFISCYH'
    lcFilName = 'FSHLD'
    
  CASE lcRpForm = 'SMFISCYS'
    lcFilName = 'FISHD'
    
  CASE lcRpForm = 'SMFISCYP'
    lcFilName = 'FSPRD'
        
ENDCASE

SELECT (lcFilName)
SET ORDER TO 1

DO gfDispRe WITH EVAL('lcRpForm'),IIF(!EMPTY(lcRpExp),'For ','')+lcRpExp
SET RELATION TO

IF llFISHD .AND. USED('FISHD') 
  USE IN FISHD
ENDIF
IF llFSPRD .AND. USED('FSPRD') 
  USE IN FSPRD
ENDIF
IF llFSHLD .AND. USED('FSHLD') 
  USE IN FSHLD
ENDIF


*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Saber A Rzek (SAB)
*! Date      : 12/30/2012
*! Purpose   : The when function of the option grid
*!*************************************************************
FUNCTION lfwRepWhen

DECLARE laRpCmpDsc[1,1] , laRpCmpNo[1,1] &&, laRpCmpCod[1,5]
STORE SPACE(0) TO laRpCmpDsc , laRpCmpNo &&, laRpCmpCod

*- Fill Company Array.
=lfGetComps()


ENDFUNC
*- End of 


*!*************************************************************
*! Name      : lfvCompID
*! Developer : Saber A Rzek (SAB)
*! Date      : 12/30/2012
*! Purpose   : Valid function for the field Company ID
*!*************************************************************
FUNCTION lfvCompID


ENDFUNC
*-End of lfvCompID


*!*************************************************************
*! Name      : lfGetComps
*! Developer : Saber A Rzek (SAB)
*! Date      : 12/30/2012
*! Purpose   : Get company array
*!*************************************************************
FUNCTION lfGetComps

LOCAL lnComNo , lnI
*- Collect all companies
SELECT cComp_ID+" - "+cCom_Name FROM SYCCOMP INTO ARRAY laRpCmpCod ORDER BY 1

lnComNo = _TALLY

DECLARE laRpCmpDsc[lnComNo, 1] , laRpCmpNo[lnComNo, 1]
IF lnComNo > 0
  lnI = 1
  FOR lnI = 1 TO lnComNo
    laRpCmpDsc[lnI, 1] = laRpCmpCod[lnI, 1]
    laRpCmpNo[lnI, 1]  = LEFT(laRpCmpCod[lnI, 1], 2)
  ENDFOR
ENDIF

ENDFUNC
*--- end of lfGetComps.


*!*************************************************************
*! Name      : lfDefComp
*! Developer : Saber A Rzek (SAB)
*! Date      : 12/30/2012
*! Purpose   : Function to get the default value of the company id
*!*************************************************************
FUNCTION lfDefComp

*IF Statment to check if there is an active company
IF !EMPTY(oAriaApplication.ActiveCompanyID) &&!EMPTY(gcAct_Comp)
  RETURN oAriaApplication.ActiveCompanyID &&gcAct_Comp
ELSE
  LOCAL lnAlias
  lnAlias = SELECT(0)
  SELECT SYCCOMP
  GO TOP
  SELECT (lnAlias)
  RETURN SYCCOMP.cComp_Id
ENDIF

ENDFUNC
*-End of lfDefComp