*:************************************************************************
*:
*: Procedure file: GLUNBATS.RPR
*:               : Unposted batches
*:
*:         System: ARIA BUSINESS SYSTEM
*:         Module: General Ledger
*:         Author: Hany Waide
*:      Copyright (c) 
*:  Last modified:03/04/94
*:
*:  Procs & Fncts: 
*:                lfChngCond    
*:                lfEditList
*:                lfRpPost
*:                lfRpNum
*:                lfClearRep
*:
*:      Documented 03/11/1994
*:************************************************************************
*
*B600344,1 Reham On 05/28/95 Fix the upper case for : S.M. 
*B600346,1 Reham On 05/29/95 Spelling mistakes.
*B600339,1 Reham On 05/28/95 "Added By" new object in Option Grid

*B600344,4 Reham On 06/11/95 Change the batch no. pic in the SYREPUVR file
*B600344,4 from "999999" to be "@! X99999"
*B600516,1 ORABY 07/03/1995  Ordering the SYCFISHD file
*E300789,6 MAB 03/06/1999 Deal with files have CompId Fields.
*B603800,1 SSE 08/13/2000 Enh# of Updating GlBatch File fields cBatElUsr , dBatElDat , cBatElTim
*B603800,1                After Previewing or Running Report
*B602244,1 KAM 09/05/2000 Change the Total Debit , Total Credit and Total Audit to 
*B602244,1                Righ Justify in GLUNBATS.FRX
*:************************************************************************
*** Report Setup

*HAYTHAR Change this line to Use FISHD instead of SYCFISHD [Begin]
*SET ORDER TO TAG COMPFYEAR IN SYCFISHD 
SET ORDER TO TAG COMPFYEAR IN FISHD 
*HAYTHAR Change this line to Use FISHD instead of SYCFISHD [Begin]

SELECT GLBATCH

*HAYTHAR Change this line to Use FISHD instead of SYCFISHD [Begin]
*SET RELATION TO gcAct_comp+ glbatch.cbatpyr INTO SYCFISHD ADDITIVE

*E300789,6 File does not have company Id [Begin
*SET RELATION TO gcAct_comp+ glbatch.cbatpyr INTO FISHD ADDITIVE
SET RELATION TO glbatch.cbatpyr INTO FISHD ADDITIVE
*E300789,6 File does not have company Id [End

*HAYTHAR Change this line to Use FISHD instead of SYCFISHD [End]

GO TOP
DO gfDispRe WITH EVAL('lcRpForm'),IIF(!EMPTY(ALLTRIM(lcRpExp)),'FOR ','')+lcRpExp

SELECT GLBATCH
SET RELATION TO 

*B603800,1 [Begin]
IF !EMPTY(ALLTRIM(lcRpExp))
  SCAN FOR &lcRpExp
    =RLOCK()
    REPLACE cbatElUsr WITH gcUser_ID ,;
            dBatElDat WITH DATE()    ,;
            cBatElTim WITH gfGetTime() 
    UNLOCK
  ENDSCAN
ENDIF
*B603800,1 [End]

*!************************************************************************
*!
*!      Function lfEditList
*!
*!************************************************************************
*
FUNCTION lfEditList

RETURN  ( DTOS(glBatch.dbaTelDat)+glBatch.cbAtelTim >;
                                  DTOS(glBatch.daDd_Date)+glBatch.caDd_Time )

*!************************************************************************
*!
*!      Function lfvRpPost
*!
*!************************************************************************
*
FUNCTION lfvRpPost

DECLARE laRpRetFld(1)
lcOldBrFld    = lcBrFields
lcBrFields    = 'CFisFYear:H="Year",DFisBgDat:H="Begin date",DFisEnDat:H="End date"'
laRpRetFld[1] = ''

&& Check If year field is empty
lcRpCurFld = VARREAD()
IF .NOT.EMPTY(&lcRpCurFld.)  
  lcOldAlias = ALIAS()    && Save the current alias
  llUesdBefo = .F.        && Check if used before or this the first time
  
  *HAYTHAR Change these lines to Use FISHD instead of SYCFISHD [Begin]
  
  *IF NOT USED("SYCFISHD") 
  *  SELECT 0
  *  USE &gcSysHome.SYCFISHD ORDER TAG compfyear
  *  llUesdBefo = .T.
  *ENDIF
  *SELECT SYCFISHD
  IF NOT USED("FISHD") 
    SELECT 0
    USE &gcSysHome.FISHD ORDER TAG compfyear
    llUesdBefo = .T.
  ENDIF
  SELECT FISHD
  
  *HAYTHAR Change these lines to Use FISHD instead of SYCFISHD [End]
  
  *B600516,1  Ordering the SYCFISHD file
  SET ORDER TO TAG COMPFYEAR
  *** Search for the current company+year

    *E300789,6 File does not have company Id [Begin
    *E IF ('?' $ &lcRpCurFld. .OR.;  
    *E  !SEEK(gcPrnt_Cmp+ALLTRIM(&lcRpCurFld.))) .AND. !EMPTY(ALLTRIM(&lcRpCurFld.))
    IF ('?' $ &lcRpCurFld. .OR.;  
      !SEEK(ALLTRIM(&lcRpCurFld.))) .AND. !EMPTY(ALLTRIM(&lcRpCurFld.))

        *E =gfBrows([gcPrnt_Cmp],'CFisFyear',"laRpRetFld",'Transaction Codes ',.F.)
        =gfBrows('','CFisFyear',"laRpRetFld",'Transaction Codes ',.F.)
        
      &lcRpCurFld = laRpRetFld[1]
      SHOW GET (lcRpCurFld)
    ENDIF
    *E300789,6 File does not have company Id [End
  
  IF llUesdBefo       && .F.- this file used by the system
    
    *HAYTHAR Change this line to Use FISHD instead of SYCFISHD [Begin]
    *USE IN SYCFISHD
    USE IN FISHD
    *HAYTHAR Change this line to Use FISHD instead of SYCFISHD [End]
    
  ENDIF
  IF NOT EMPTY(lcOldAlias)
    SELECT (lcOldAlias)
  ENDIF    
ENDIF
RETURN 

*!************************************************************************
*!
*!      Function lfRpNum
*!
*!************************************************************************
*
FUNCTION lfRpNum

EXTERNAL ARRAY laOgFxFlt
DECLARE laRetInfo[1]
lcRpBatNo=VARREAD()
IF !EMPTY(EVAL(lcRpBatNo))
  &lcRpBatNo=trans(VAL(EVAL(lcRpBatNo)),'@l 999999')
  SHOW GET &lcRpBatNo
ENDIF  
SELECT GLBATCH
lcOldTag=SYS(22)
SET ORDER TO TAG BATCHNO

*lcSaveBrow       = lcBrFields
lcBrFields       = 'cbatchno:H="Batch number",cbatdesc:H="Batch description"'
laRetInfo[1]     = laOgFxFlt[1,6]
*lcSavFTitl       = lcFile_Ttl
lcFile_Ttl       = 'Batches' 

*** first  parameter : index expression
*** second parameter : key field
*** third  parameter : array to return in
*** forth  parameter : flag to set user filter in brows on/off

IF !EMPTY(ALLTRIM(EVAL(VARREAD()))) AND ! SEEK(EVAL(lcRpBatNo)) .AND. LASTKEY() = 13
  IF !EMPTY(laOgFxFlt[1,6])
    =gfBrows('"'+laOgFxFlt[1,6]+laOgFxFlt[2,6]+'"',"cbatchno",'laRetInfo',.F.)  
  ELSE
    SET ORDER TO TAG BATSTAT
    =gfBrows('"'+laOgFxFlt[2,6]+'"',"cbatchno",'laRetInfo',.F.)  
  ENDIF
&lcRpBatNo=laRetInfo[1]
SHOW GET &lcRpBatNo
ENDIF

*lcBrFields       = lcSaveBrow
*lcFile_Ttl       = lcSavFTitl
SET ORDER TO TAG &lcOldTag

*!************************************************************************
*!
*!      FUNCTION lfvSorMod
*!
*!************************************************************************
*
FUNCTION lfvSorMod

*mab
PRIVATE llApplFile
llApplFile = .F.

DECLARE laRpRetFld(1)
IF EMPTY(EVAL(VARREAD()))
  RETURN
ENDIF
lcOldBrFld    = lcBrFields
lcBrFields    = 'Capp_id:H="Modul code",Capp_name:H="Modul name"'
laRpRetFld[1] = ''
lcRpCurFld      = VARREAD()
IF !USED('SYDAPPL')
  *mab
  llApplFile = .T.
  SELECT 0
  USE (gcSysHome+'SYDAPPL')
ENDIF
SELECT sydAPPL
SET ORDER TO TAG CAPP_ID
IF !SEEK(EVAL(lcRpCurFld)) AND LASTKEY()=13
  =gfBrows([FOR !INLIST(CAPP_ID,'SY','SM')],'CAPP_ID',"laRpRetFld",'Moduls',.F.)
  &lcRpCurFld = laRpRetFld[1]
ELSE
  IF !SEEK(EVAL(lcRpCurFld))    
    &lcRpCurFld=""
  ENDIF
ENDIF
SHOW GET (lcRpCurFld)
lcBrFields = lcOldBrFld
SET ORDER TO

*MAB
IF llApplFile
  USE IN SYDAPPL
ENDIF


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
