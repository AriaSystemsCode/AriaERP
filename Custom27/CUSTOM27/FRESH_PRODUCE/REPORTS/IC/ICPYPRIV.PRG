****************************************************************************
*: Program file      : ICPYPRIV.PRG
*: Program desc.     : Physical to Perpetual Inventory Variance     
*: System            : Aria Apparel System (A27).
*: Module            : Inventory Control (IC)
*: Developer         : TAK
*: Date              : 10/10/2000
*:**************************************************************************

*--ckeck on warehouse.
lcWareCode = ""
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'WAREHOUS.CWARECODE'),1)
IF lnPosition > 0
  IF EMPTY(laOGFxFlt[lnPosition,6])
    =gfModalGen('TRM34020B34000','DIALOG','Warehouse'+'|'+' ')      
    SET DEVI TO SCREEN
    RETURN
  ENDIF
  lcWareCode = laOGFxFlt[lnPosition,6]
ENDIF



lcPhyPath= ""
IF ! FILE(gcDataDir+'PHY_COMP.MEM')
  lcPhyPath = GETDIR('','Phys. Invt. Co. Data dir')
  IF EMPTY(lcPhyPath)
    WAIT WINDOW 'Physical Inventory Data Directory should be selected, Connot Proceed!'
    SET DEVICE TO SCREEN
    RETURN
  ENDIF
  SAVE ALL LIKE lcPhyPat* TO gcDataDir+'PHY_COMP.MEM'
ELSE
  RESTORE FROM gcDataDir+'PHY_COMP.MEM' ADDI
ENDIF


*--Check for groups
lcGroups = ""
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'STYLE.CSTYGROUP'),1)
IF lnPosition > 0
  lcGroups = laOGFxFlt[lnPosition,6]
ENDIF


*-- To check if there is any selected Accounts.
llStySel = .F.
*-- To hold the file of the selected style if any
lcStyFile = ''
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'STYLE.CSTYMAJOR'),1)
IF lnPosition > 0
  lcStyFile = laOGFxFlt[lnPosition,6]
  IF !EMPTY(lcStyFile) AND USED(lcStyFile)
    SELECT (lcStyFile)
    GOTO TOP
    llStySel = !EOF()
  ENDIF  
ENDIF


SELECT 0
USE (lcPhyPath+'STYDYE.dbf') ALIAS 'ASTYDYE'
SET ORDER TO TAG Stydye


SELECT STYLE
SET ORDER TO TAG Style
lcTmpStyle = gfTempName()
COPY STRU TO (gcWorkDir+lcTmpStyle)
=gfOpenFile(gcWorkDir+lcTmpStyle,'','EX')
INDEX ON Pattern+Style TAG (lcTmpStyle)


IF llStySel
  SELECT (lcStyFile)
  SCAN
    =SEEK(PADR(cStyMajor,12),'STYLE')
    SELECT STYLE
    SCAN WHILE Style = PADR(&lcStyFile..cStyMajor,12)
      IF !EMPTY(lcGroups) AND !(STYLE.CSTYGROUP $ lcGroups)
         LOOP
      ENDIF
      WAIT WINDOW 'Collecting Styles :'+STYLE.Style NOWAIT
  
      =lfUpdTSty()
    ENDSCAN
  ENDSCAN
ELSE
  SELECT STYLE
  SCAN
    IF !EMPTY(lcGroups) AND !(STYLE.CSTYGROUP $ lcGroups)
       LOOP
    ENDIF
    WAIT WINDOW 'Collecting Styles :'+STYLE.Style NOWAIT
  
    =lfUpdTSty()

  ENDSCAN
ENDIF  


SELECT (lcTmpStyle)
IF llShowDiff
  SET FILTER TO TotStk<>TotOrd OR Stk1<>Ord1 OR Stk2<>Ord2 OR Stk3<>Ord3 OR Stk4<>Ord4
  GO TOP  
ENDIF
IF !EOF()
  GO BOTT
  lcLastKey = Pattern+Style
  GO TOP  

  = gfDispRe(lcRpForm)

ELSE
  =gfModalGen('TRM00052B00000','DIALOG')
ENDIF

IF USED(lcTmpStyle)
  SELECT (lcTmpStyle)
  USE
  ERASE(gcWorkDir+lcTmpStyle+'.DBF')
  ERASE(gcWorkDir+lcTmpStyle+'.CDX')
ENDIF

SELECT STYLE
SET ORDER TO TAG Cstyle

USE IN ASTYDYE

SET DEVICE TO SCREEN
RETURN



*****************************************************
FUNCTION lfWhenRep

********************************************************
FUNCTION lfvWare
PRIVATE lcWarFld,lcWarhous,lnSelFile,lcWareTag

lcWarFld   = VARREAD()
lcWareCode = EVAL(lcWarFld)

lnSelFile = SELECT(0)
SELECT WAREHOUS
lcWareTag = ORDER('WAREHOUS') 
SET ORDER TO TAG WAREHOUS IN WAREHOUS
IF !EMPTY(lcWareCode) .AND. ('?' $ lcWareCode .OR. !SEEK(lcWareCode , 'WAREHOUS'))
  lcWareCode =gfBrowWare(.T.)  
ENDIF
&lcWarFld = lcWareCode
SET ORDER TO lcWareTag
SELECT (lnSelFile)
RETURN

**********************************************************
FUNCTION lfUpdTSty

IF SEEK(STYLE.Style+lcWareCode,'STYDYE')
  =SEEK(STYLE.Style+lcWareCode,'ASTYDYE')
  SELECT (lcTmpStyle)
  APPEND BLANK
  REPLACE Style     WITH STYLE.Style,;
          cStyMajor WITH STYLE.cStyMajor,;
          Pattern   WITH IIF(lcSortBy='G',STYLE.cStyGroup,SUBSTR(STYLE.Style,14,6)),;
          TotCost   WITH STYLE.TotCost,;
          Stk1      WITH STYDYE.Stk1,;
          Stk2      WITH STYDYE.Stk2,;
          Stk3      WITH STYDYE.Stk3,;
          Stk4      WITH STYDYE.Stk4,;
          TotStk    WITH STYDYE.Stk1+STYDYE.Stk2+STYDYE.Stk3+STYDYE.Stk4,;
          Ord1      WITH ASTYDYE.Stk1,;
          Ord2      WITH ASTYDYE.Stk2,;
          Ord3      WITH ASTYDYE.Stk3,;
          Ord4      WITH ASTYDYE.Stk4,;
          TotOrd    WITH ASTYDYE.Stk1+ASTYDYE.Stk2+ASTYDYE.Stk3+ASTYDYE.Stk4
ENDIF       
RETURN
