*:***************************************************************************
*: Program file  : FixPick
*: Program desc. : Fix cutpick file
*: For screen    : FixPick
*:        System : Aria Advantage Series.
*:        Module : Allocation AL
*:     Developer : Mohamed Atia Badran (MAB)
*      Date      : 04/08/1999
*:***************************************************************************
*: Calls : 
*:     Functions  : lfIntrFace,lfFindFile,lfvProc,lfUpdtFile
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO FixPick
*:***************************************************************************
*
PARAMETERS lcSysDir

PRIVATE llCompPath
llCompPath = .F.

DO WHILE .T.
  
  IF TYPE('lcSysDir') $ 'UL'
    lcSysDir = GETDIR('','Aria Advantage Series')
  ELSE
    llCompPath = .T.
  ENDIF  

  *-- if user press ESC or Cancel.
  IF EMPTY(lcSysDir)

    EXIT

  ELSE  && else user select directory.

    *-- if it's aria advantage directory.
    IF llCompPath OR FILE(lcSysDir+'SYCCOMP.DBF')  

      IF llCompPath
      
        = lfUpdtFile(0,lcSysDir) && Update Current company file.
        EXIT
      
      ELSE

        *-- open company file.
        USE (lcSysDir+'SYCCOMP.DBF') ORDER Ccomp_id IN 0 SHARED
        SELECT SYCCOMP
        GO TOP

        *-- if no companies created for this system.
        IF EOF()
          WAIT WINDOW 'Current directory does not have companies' TIMEOUT 3
          USE IN SYCCOMP
          RELEASE lcSysDir 
          LOOP
      
        ELSE  && else this directory have companies.
          *-- Run interface for company select.
          PRIVATE llFindFile
          llFindFile = .F.
          =lfIntrFace()

          *-- Close Company file, and then exit loop
          USE IN SYCCOMP

          *-- if allocation module is installed for at least one company.
          IF llFindFile
            EXIT
          ELSE
            RELEASE lcSysDir 
            LOOP        
          ENDIF              
      
        ENDIF  
      
      ENDIF     && end if no companies created for this system.
      
    ELSE  && not aria advantage series directory.
      
      WAIT WINDOW 'Current directory is not an Aria Advantage Series directory'  TIMEOUT 3
      RELEASE lcSysDir 
      LOOP
    
    ENDIF        && end if it's aria advantage directory.
  ENDIF          && end if user press ESC or Cancel.
ENDDO  
WAIT CLEAR
*-- end of program code.

*-- call interface function.
FUNCTION lfIntrFace
PRIVATE laCompany,lcCompDir,lcCompStat
DECLARE laCompany[1,3]
STORE '' TO laCompany,lcCompDir

SELECT SYCCOMP
*-- Collect all companies
SELECT ccomp_id+" - "+cCom_Name,cCom_dDir,'N' ;
  FROM SYCCOMP ;
  INTO ARRAY laCompany ;
  ORDER BY 1

FOR lnI = 1 TO _TALLY
  =lfFindFile(lnI,ALLTRIM(laCompany[lnI,2]))
ENDFOR

DO WHILE .T.
  lnDelPos = ASCAN(laCompany,'DELETE')
  IF lnDelPos = 0
    EXIT
  ELSE
    lnDelPos = ASUBSCRIPT(laCompany,lnDelPos,1)
  ENDIF

  =ADEL(laCompany,lnDelPos,1)
  IF ALEN(laCompany,1) > 1
    DIMENSION laCompany[ALEN(laCompany,1)-1,3]
  ENDIF
ENDDO

IF llFindFile
  
  IF ALEN(laCompany,1) > 1

    DECLARE laCompany [ALEN(laCompany,1)+1,3] 

    =AINS('laCompany',1)
    laCompany[1,1] = "                         All Companies "
    laCompany[1,2] = " "
    puCompany      = 1

  ENDIF
  DO FixPick.SPX

ELSE

  WAIT WINDOW 'Allocation Module not installed for all companies' TIMEOUT 2
ENDIF  

*-- end of lfIntrFace.

*-- Check foundation of cutpick file
FUNCTION lfFindFile
PARAMETERS lnCompPos,lcDataDir 
IF FILE(lcDataDir+'CUTPICK.DBF')
  llFindFile = .T.

ELSE

  WAIT WINDOW 'Allocation Module not installed for company :' +;
              SUBSTR(laCompany[lnCompPos,1],5) TIMEOUT 2
  laCompany[lnCompPos,3] = 'DELETE'
ENDIF
*-- end of lfFindFile.

*-- Fix Function
FUNCTION lfvProc

IF EMPTY(PADR(laCompany[puCompany,1],2))
  FOR lnI = 2 TO ALEN(laCompany,1)
    =lfUpdtFile(lnI,ALLTRIM(laCompany[lnI,2]))  
  ENDFOR
ELSE
  =lfUpdtFile(puCompany,ALLTRIM(laCompany[puCompany,2]))  
ENDIF
*-- end of lfvProc.

*--
FUNCTION lfUpdtFile
PARAMETERS lnCompPos,lcDataDir
USE (lcDataDir+'CUTPICK.DBF') ORDER Cutpick IN 0 SHARED
SELECT CUTPICK
GO TOP
IF EOF() OR TYPE('ctktlineno') $ 'UL'
  IF lnCompPos > 0
    lcMessage = IIF(EOF(),'No allocation transactions was found for company:  ',;
                          'You need update cutpick structure in company:')
    WAIT WINDOW lcMessage + SUBSTR(laCompany[lnCompPos,1],5) TIMEOUT 2
  ENDIF  
ELSE                

  IF FILE(lcDataDir+'POSLN.DBF')
    USE (lcDataDir+'POSLN.DBF') ORDER POSLNS IN 0 SHARED
  ENDIF  

  IF FILE(lcDataDir+'cuttktl.DBF')
    USE (lcDataDir+'cuttktl.DBF') ORDER cuttktlS IN 0 SHARED
  ENDIF  

  lcLastKey  = ' '
  lnLastNo   = 0

  SELECT CUTPICK
  SCAN

    WAIT WINDOW 'Update Order : '   + Order + ' Line# ' + cOrdLine + ' Style = ' + Style NOWAIT
    IF !EMPTY(ctktlineno)
      LOOP
    ENDIF
      
    llUpdt     = .F.
    
    IF TRANCD = '1'
      lcUsedFile = 'cuttktl'
      lcPickExpr = [Style+cTktNo]
      lcUsedExpr = [cuttktl.style+cuttktl.cuttkt]
    ELSE
      lcUsedFile = 'POSLN'
      lcPickExpr = [Style+'P'+cTktNo]
      lcUsedExpr = [POSLN.Style+POSLN.cstytype+POSLN.po]
    ENDIF

    IF SEEK(EVALUATE(lcPickExpr),lcUsedFile)
      IF EVALUATE(lcPickExpr) == lcLastKey
        SKIP lnLastNo IN (lcUsedFile)
        lnLastNo = lnLastNo + 1
        llUpdt = (EVALUATE(lcPickExpr) = EVALUATE(lcUsedExpr))
      ELSE
        lnLastNo = 1
        llUpdt   = .T.
      ENDIF

      IF llUpdt
        REPLACE ctktlineno WITH STR(&lcUsedFile..LineNo,6)
      ENDIF
    
    ENDIF
    lcLastKey = EVALUATE(lcPickExpr)
  ENDSCAN

ENDIF

IF USED('CUTPICK')
  USE IN CUTPICK
ENDIF

IF USED('POSLN')
  USE IN POSLN
ENDIF

IF USED('cuttktl')
  USE IN cuttktl
ENDIF
*-- end of lfUpdtFile.