*:******************************************************************************************
*:Program file : ApAgCath.PRG
*:Program desc. : A/P Aging report by due date
*:System : AP
*:Developer : Ahmed Abdel Naby (AAN)
*:Date : 14/3/2001
*:******************************************************************************************
*:Calls: 
*:		Functions: 
*:******************************************************************************************
*:Passed Parameters:
*:******************************************************************************************
*:Example : Do ApAgCath
*:******************************************************************************************
*-- Due to C200178,1
*:******************************************************************************************
*B604464,1 AAN 05/08/2001 Change the company name in the report from hard coded to variable
*B604464,1 AAN            Showing the current active company.        
*B804202,1 AME 05/21/2001 print the second report in all cases.
*B804202,1 AME            There's also some changes in ApAgCat2.frx report.
*:******************************************************************************************

lcCent = SET("CENTURY")
SET CENTURY OFF
STORE {} TO ldOStDate, ldOEdDate, ldORpDueDat
lcRpCompNm = ' '    && Varible to hold the active company name
IF SEEK(gcAct_Comp , 'SYCCOMP')
  lcRpCompNm = ALLTRIM(SYCCOMP.cCom_Name)    && Varible to hold the active company name
ENDIF    && End of IF


lcTmpCursr = gfTempName()    && Varible to hold a temp. name for the cursor

IF !'APVENDOR.CVENGRP1' $ UPPER(lcRpExp)
  SELECT *;
        FROM APINVHDR;
        INTO CURSOR &lcTmpCursr;
        WHERE &lcRpExp;
        ORDER BY APINVHDR.cVendCode , dInvDuDat

ELSE
  SELECT APINVHDR.*;
        FROM APINVHDR , APVENDOR;
        INTO CURSOR &lcTmpCursr;
        WHERE &lcRpExp .AND. APINVHDR.cVendCode + APINVHDR.cInvNo = APVENDOR.cVendCode;
        ORDER BY APINVHDR.cVendCode , dInvDuDat

ENDIF

SELECT (lcTmpCursr)
SET RELATION TO cVendCode INTO APVENDOR
SET RELATION TO cVendCode+cInvNo INTO APINVHDR ADDITIVE

lcOGPlatForm = "DOS"

DO gfDispRe WITH EVAL('LCRPFORM'),.F.,.T.

IF gcDevice = 'SCREEN' OR gcDevice = 'FILE'
  gcOutFile = ALLT(STRTRAN(gcOutFile,'ADDITIVE'))+' ADDITIVE'
ENDIF
llFound=EOF()

*B804202,1 AME [Start] Rename Variable name 
*lcExpr = [BETWEEN(dInvDuDat,ldRpduedat,ldRpduedat+6)]

lcScExpr = [BETWEEN(dInvDuDat,ldRpduedat,ldRpduedat+6)]

 
SET FILTER TO &lcScExpr
LOCATE 
IF EOF()
  lcTmpCur1 = gfTempName()    && Varible to hold a temp. name for the cursor
  COPY STRUCTURE TO (gcWorkDir+lcTmpCur1)
  =gfOpenFile(gcWorkDir+lcTmpCur1,'','SH')
  APPEND BLANK
  SELECT (lcTmpCur1)
  lcScExpr  = ""
ENDIF
*B804202,1 AME [End]

*B804202,1 AME [Start] Remov the if Statment, to print second report in all cases
*LOCATE FOR &lcExpr
*IF llfound AND FOUND()
*B804202,1 AME [End]

lnPageNo=_PAGENO
*B804202,1 AME [Start] Remov the if Statment, to print second report in all cases
*DO gfDispRe WITH 'APAGCAT2',lcExpr &&'FOR '+lcRpExp +[ AND BETWEEN(APINVHDR.dinvdudat,ldRpduedat,ldRpduedat+6)]
DO gfDispRe WITH 'APAGCAT2'

*ELSE
*  IF _TALLY <> 0
*    =gfEndPrn()
*  ENDIF
*ENDIF
*B804202,1 AME [End]

USE IN (lcTmpCursr)

IF EMPTY(lcScExpr)
  SELECT (lcTmpCur1)
  USE 
ENDIF  

SET CENTURY &lccent

*!*************************************************************
*! Name      : lfwStDate
*! Developer : Ahmed Abdel Naby (AAN)
*! Date : 14/3/2001
*! Purpose   : got old date.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : 
*!*************************************************************
FUNCTION lfwStDate

ldOStDate = laoGfxflt[1,6]

*!*************************************************************
*! Name      : lfvStDate
*! Developer : Ahmed Abdel Naby (AAN)
*! Date : 14/3/2001
*! Purpose   : validate start date.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : 
*!*************************************************************
FUNCTION lfvStDate

IF !EMPTY(laoGfxflt[2,6])
  IF laoGfxflt[1,6] > laoGfxflt[2,6]  
    laoGfxflt[1,6] = ldOStDate    
    WAIT 'Strating date should be less or equal than ending date.' WINDOW
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfwEdDate
*! Developer : Ahmed Abdel Naby (AAN)
*! Date      : 14/3/2001
*! Purpose   : get old date.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : 
*!*************************************************************
*C100528,1 HISH 11/23/95
FUNCTION lfwEdDate

ldOEdDate = laoGfxflt[2,6]


*!*************************************************************
*! Name      : lfvEdDate
*! Developer : Ahmed Abdel Naby (AAN)
*! Date      : 14/3/2001
*! Purpose   : validate end date.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : 
*!*************************************************************
FUNCTION lfvEdDate

IF !EMPTY(laoGfxflt[1,6])
  IF laoGfxflt[1,6] > laoGfxflt[2,6]  
    laoGfxflt[2,6]=ldOEdDate    
    WAIT 'Ending date should be greater or equal than starting date.' WINDOW
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfdDueDat
*! Developer : Ahmed Abdel Naby (AAN)
*! Date      : 14/3/2001
*! Purpose   : Get default due date.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : 
*!*************************************************************
*
FUNCTION lfdDueDat

ldRpDueDat = gdSysDate+6-DOW(gdSysDate)

RETURN ldRpDueDat
*!*************************************************************
*! Name      : lfwDueDat
*! Developer : Ahmed Abdel Naby (AAN)
*! Date      : 14/3/2001
*! Purpose   : Validate due date.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : 
*!*************************************************************
*
FUNCTION lfwDueDat

ldORpDueDat = ldRpDueDat
*!*************************************************************
*! Name      : lfvDueDat
*! Developer : Ahmed Abdel Naby (AAN)
*! Date      : 14/3/2001
*! Purpose   : Validate due date.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : 
*!*************************************************************
*
FUNCTION lfvDueDat

ldRpDueDat = ldRpDueDat+6-DOW(ldRpDueDat)
ldRpDueDat=IIF(ldRpDueDat>=DATE(),ldRpDueDat,ldORpDueDat)
