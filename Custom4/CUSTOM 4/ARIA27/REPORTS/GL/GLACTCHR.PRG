*:************************************************************************
*:
*: Procedure file: GLACTCHR.RPR
*:               : Chart of accounts
*:
*:         System: ARIA BUSINESS SYSTEM
*:         Module: General Ledger
*:         Author: Albert Raif
*:      Copyright (c) 
*:  Last modified:12/05/94
*:
*:  Procs & Fncts: 
*:                lfChngCond    
*:                
*:
*:      Documented   /  /
*:************************************************************************
*
*B600344,1 Reham On 05/28/95 Fix the upper case for : cash flow 
*B600344,1 code and let it browse.

*B600338,1 Reham On 05/28/95 Increase the date width to 10 ch.
*B600339,1 Reham On 05/28/95 "Added By" new object in Option Grid
*B600358,1 Reham On 05/30/95
*B600358,1 _ Change the heading "Account number" to "Account code"
*B600358,1 _ In the detail form, increase the status object 1 ch.
*B600358,1 _ Add validation for acc. group code.

*B600366,1 Reham On 06/01/95 Fix the acc. code picture in the dictionary
*B600366,1 to prevent browsing if acc. exists.

*B600344,4 Reham On 06/11/95 Fix the validation of the ratio group object.

*B800073,1 ORABY 08/15/95 If there is no group in the accounts group file the user 
*B800073,1                cannot exit the Acount group field in the option grid
*E300789,7 WALID (WAM) 03/05/99 Make all modifications needed due to
*E300789,7       removing ccomp_id field .
*B803801,1 ABD 11/08/2000 Fix Bug Table is not ordered.
*B301551,1 KAM 30/01/2001 convert glactchs.frx,glactchd.frx from text format to graphic fromat
*:************************************************************************
*** Report Setup
   
lcRpFiles  = IIF(EMPTY(laOGHDFlt[1,1]),"GLACCHAR","GLACCHAR,GLGRPDT")  && Get slected files name

IF llOGFltCh OR glEscPrs
*  lcRpExp = lcRpExp + ' '
       
  *** Save escape setting
  lcSaveDel = SET('DELETE')
  lcSaveEsc = SET('ESCAP')
  lcSaveOnEs = ON('ESCAPE')
*  SET DELETE OFF
  SET ESCAP ON
  ON ESCAP DO gpSQLBrak
        
  *** Intialize the varliable that count rows selected
  _TALLY = 0
      
  *** Activate the system select therom.
  SET TALK ON
  
  WAIT 'Collecting data...' WINDOW NOWAIT
  
  ***   Create select  statment
  SELECT   &lcRpFields;
    FROM  &lcRpFiles ;
    WHERE  &lcRpExp ;
     &lcRpOrder.;
    INTO CURSOR (lcRpTargt)
    
  *** Restore all enviroment 
  WAIT CLEAR
  SET TALK OFF
  SET DELETE &lcSaveDel
  ON ESCAPE  &lcSaveOnEs
  SET ESCAPE &lcSaveEsc
  
  *** Display the report to screen , file or printer
  *** and check if there is any record or not
  *** before that check if press Escape or not
  IF _TALLY = 0        && No records collected
    ** NO recoeds hove been collected
    =gfModalGen("INM00052B00000","DIALOG")
  ELSE
    DO gfDispRe WITH EVAL('lcRpForm')
  ENDIF
ELSE
  SELECT (lcRpTargt)
  DO gfDispRe WITH EVAL('lcRpForm')
ENDIF 
glEscPrs = .F.

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

*!************************************************************************
*!
*!      FUNCTION lfvGrpCode
*!
*!************************************************************************
*
FUNCTION lfvGrpCode

DECLARE laRpRetFld(1)
lcOldBrFld    = lcBrFields
lcBrFields    = 'CGrpCode:H="Code",CGrplnhed:H="Description"'
laRpRetFld[1] = ''
lcRpCurFld      = VARREAD()

SELECT GLGRPHD
SET ORDER TO TAG grpcode
IF '?' $ &lcRpCurFld. .OR. !SEEK(&lcRpCurFld)
  =gfBrows([""],'CGrpCode',"laRpRetFld",'Codes File',.F.)
  &lcRpCurFld = laRpRetFld[1]
  SHOW GET (lcRpCurFld)
ENDIF
lcBrFields = lcOldBrFld
IF EMPTY(&lcRpCurFld)
  STORE '' TO laOGHdFlt
  *B800073,1 MOVE TO THE NEXT OBJECT
  _CUROBJ = _CUROBJ + 1
ELSE
  laOGHDFlt[1,1]='GLACCHAR.CACCTCODE'
  laOGHDFlt[1,2]='F'
  laOGHDFlt[1,3]='C'
  laOGHDFlt[1,4]=.T.      
  laOGHDFlt[1,5]='Like'
  laOGHDFlt[1,6]='GLGRPDT.CACCTCODE'
  laOGHDFlt[1,7]='F'      
ENDIF
SET ORDER TO

*!************************************************************************
*!
*!      FUNCTION lfwOldVal
*!
*!************************************************************************
*
FUNCTION lfwOldVal

lcRpOld=EVAL(VARREAD())

*!************************************************************************
*!
*!      FUNCTION lfGetPic
*!
*!*************************************************************************
*
FUNCTION lfGetPic

lcOldAlias = ALIAS()    && Save the current alias
llUesdBefo = .F.        && Check if used before or this the first time

*HAYTHAR Change these lines to Use ACCOD instead of SYCACCOD [Begin]

*IF NOT USED("SYCACCOD") 
*  SELECT 0
*  USE &gcSysHome.SYCACCOD ORDER TAG COMPID
*  llUesdBefo = .T.
*ENDIF
*SELECT SYCACCOD
IF NOT USED("ACCOD") 
  SELECT 0
  *E300789,7 WALID (WAM) [BEGIN] Open with no index
  *USE &gcDataDir.ACCOD ORDER TAG COMPID
  USE &gcDataDir.ACCOD 
  *E300789,7 WALID (WAM) [END..]
  llUesdBefo = .T.
ENDIF
SELECT ACCOD

*HAYTHAR Change these lines to Use ACCOD instead of SYCACCOD [End]
*E300789,7 WALID (WAM) [BEGIN]
GO TOP
IF !EOF()
*IF SEEK(gcAct_Comp)
*E300789,7 WALID (WAM) [END..]
  *HAYTHAR Change these lines to Use ACCOD instead of SYCACCOD [Begin]
  
  *lcRpSegMas = ALLTRIM(SYCACCOD.cacsmask)
  lcRpSegMas = ALLTRIM(ACCOD.cacsmask)
  
  *HAYTHAR Change these lines to Use ACCOD instead of SYCACCOD [End]
  
  lcRpSegMas = STRTRAN(lcRpSegMas, '#', '9',2) 
  lcRpSegMas = STRTRAN(lcRpSegMas, '#', 'X',1,1) 
ELSE
  lcRpSegMas = " "
ENDIF
IF llUesdBefo       && .F.- this file used by the system
  
  *HAYTHAR Change these lines to Use ACCOD instead of SYCACCOD [Begin]
  
  *USE IN SYCACCOD
  USE IN ACCOD
  
  *HAYTHAR Change these lines to Use ACCOD instead of SYCACCOD [End]
  
ENDIF
IF NOT EMPTY(lcOldAlias)
  SELECT (lcOldAlias)
ENDIF    
RETURN lcRpSegMas

*!************************************************************************
*!
*!      FUNCTION lfvRatGrp
*!
*!************************************************************************
*
FUNCTION lfvRatGrp

lcRpCurFld = EVALUATE(VARREAD())

SELECT GLRACOD
LOCATE FOR GLRACOD.csegratio == lcRpCurFld
IF !FOUND()
  *B600344,4 Fix the validation because it was accepting the 
  *B600344,4 type if it's entered
  DECLARE laRpRetFld(1)
  lcOldBrFld    = lcBrFields
  lcBrFields    = 'CSEGRATIO:H="Ratio group code",CRATDESC:H="Ratio group description"'
  laRpRetFld[1] = ''
  
  lcRpCurFld = VARREAD()
  IF !EMPTY(&lcRpCurFld)
    SELECT GLRACOD
    =gfBrows([],'CSEGRATIO',"laRpRetFld",'Ratio Group File',.F.)
    &lcRpCurFld = laRpRetFld[1]
    SHOW GET (lcRpCurFld)
  ENDIF
  lcBrFields = lcOldBrFld
ENDIF

SELECT GLACCHAR

*!************************************************************************
*!
*!      FUNCTION lfvActCod
*!
*!************************************************************************
*
FUNCTION lfvActCod
*B803801,1 ABD Fix Bug Table is not ordered. [Begin]
PRIVATE lcOldOrder
lcOldOrder = ''
*B803801,1 ABD 

*B600366,1 Reham On 06/01/95 Add this function to browse if acc. does not
*B600366,1 exist in the file.

DECLARE laRpRetFld(1)
lcOldBrFld    = lcBrFields
lcBrFields    = 'CACCTCODE:H="Account code",CACCNLDES:H="Long report description"'
laRpRetFld[1] = ''
lcRpCurFld    = VARREAD()

IF !EMPTY(&lcRpCurFld)
  SELECT GLACCHAR
  *B803801,1 ABD Fix Bug Table is not ordered save old order and set order to acctcode. [Begin]  
  lcOldOrder = ORDER()
  SET ORDER TO ACCTCODE
  *B803801,1 ABD   [End]
  IF "?" $ &lcRpCurFld .OR. !SEEK(&lcRpCurFld)
    =gfBrows([],'CACCTCODE',"laRpRetFld",'Accounts File',.F.)
    &lcRpCurFld = laRpRetFld[1]
    SHOW GET (lcRpCurFld)
  ENDIF
  *B803801,1 ABD Return the old order. [Begin]
  SET ORDER TO &lcOldOrder  
  *B803801,1 ABD   [End]  
ENDIF

lcBrFields = lcOldBrFld
