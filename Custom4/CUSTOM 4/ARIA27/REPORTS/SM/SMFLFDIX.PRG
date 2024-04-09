*:************************************************************************
*:
*: Procedure file: SMFIELD.RPR
*:
*:         System: ARIA BUSINESS SYSTEM
*:         Module: General Ledger
*:         Author: Hany Waide
*:      Copyright (c) 
*:  Last modified:03/04/94
*:
*:  Procs & Funcs: 
*:
*:************************************************************************
*B600842,1 Hesham El-Sheltawi 03/31/96
*B600842,1 change the select sql fields to get the SYDINDEX.CFILE_TAG
*B600842,1 instead of SYDFILES.CFILE_TAG
*B602935,1 BWA 05/26/1999 Fix the bug of "SELECTS ARE NOT UNION COMPATIBLE" 
*B602935,1 BWA            in the detail mode of the report
*:************************************************************************
*** Report Setup

DO CASE
  CASE lcRpForm='SMFLFDIX' 
    SELECT SYDFILES
    GO TOP
    DO gfDispRe WITH EVAL('lcRpForm'),IIF(!EMPTY(ALLTRIM(lcRpExp)),'FOR ','')+lcRpExp

  CASE lcRpForm='SMFILFLD' .OR. lcRpForm='SMFILIND' 
    lcDummy = ''
    SELECT SYDFILES
    GO TOP
    lcRpFiles  = IIF(lcRpForm='SMFILFLD',"SYDFILES,SYDFLFLD,SMFIELD",;
                              "SYDFILES,SYDINDEX")
    ***   Get the field order ****
    lnCount    = -1
    lnTotal    = RECCOUNT('SYDFILES')   
    
    ***   Create select  statment
    =lfTrapEsc(.T.,-1)
    lcSelFlds = lcrpflfld()
    lcRpTExp  = IIF(EMPTY(lcRpExp),'','AND '+lcRpExp)
*B600842,1 Hesham (Start)    
*B600842,1 Remark the next line and change the select sql fields
*B600842,1 to get the field called SYDINDEX.CFILE_TAG instead of
*B600842,1 SYDFILES.CFILE_TAG
*    SELECT &lcSelFlds;
     FROM  SYDFILES,SYDFLFLD,SYDFIELD;
     WHERE (SYDFILES.CFILE_NAM=SYDFLFLD.CFILE_NAM;
           .AND. SYDFLFLD.CFLD_NAME=SYDFIELD.CFLD_NAME);
     &lcRpTExp;
     UNION SELECT SYDFILES.CFILE_NAM,CFILE_TTL,NREC_LEN,;
                  SYDFILES.CUPGRDLVL,LSYS_DATA,SPACE(10) AS 'CFLD_NAME',;
                  SPACE(1) AS 'CDATA_TYP',000 AS 'NFLD_WDTH',;
                  000 AS 'NFLD_DEC',SPACE(25) AS 'CFLD_HEAD',;
                  SPACE(50) AS 'CFLD_MSG',CINDX_NAM,CINDX_EXP,;
                  NINDX_NO,SYDFILES.CFILE_TAG,'I' AS 'cStatus';
            FROM  SYDFILES,SYDINDEX;
            WHERE (SYDFILES.CFILE_NAM=SYDINDEX.CFILE_NAM);
            &lcRpTExp;
            .AND. lfRpThermo(lnTotal,'lnCount');
            INTO CURSOR  &lcRpTargt;
            ORDER BY 1,16,4,7
            
     SELECT &lcSelFlds;
     FROM  SYDFILES,SYDFLFLD,SYDFIELD;
     WHERE (SYDFILES.CFILE_NAM=SYDFLFLD.CFILE_NAM;
           .AND. SYDFLFLD.CFLD_NAME=SYDFIELD.CFLD_NAME);
     &lcRpTExp;
     UNION SELECT SYDFILES.CFILE_NAM,CFILE_TTL,NREC_LEN,;
                  SYDFILES.CUPGRDLVL,LSYS_DATA,SPACE(10) AS 'CFLD_NAME',;
                  SPACE(1) AS 'CDATA_TYP',000 AS 'NFLD_WDTH',;
                  000 AS 'NFLD_DEC',SPACE(25) AS 'CFLD_HEAD',;
                  SPACE(50) AS 'CFLD_MSG',CINDX_NAM,CINDX_EXP,;
                  NINDX_NO,SYDINDEX.CFILE_TAG,'I' AS 'cStatus';
            FROM  SYDFILES,SYDINDEX;
            WHERE (SYDFILES.CFILE_NAM=SYDINDEX.CFILE_NAM);
            &lcRpTExp;
            .AND. lfRpThermo(lnTotal,'lnCount');
            INTO CURSOR  &lcRpTargt;
            ORDER BY 1,16,4,7
*B600842,1 Hesham (End)    
    =lfTrapEsc(.F.)        
    IF !llOGEscPrs  
      IF lnCount < lnTotal
        =lfClosThrm(lnTotal,lnCount,.f.,INT(ABS(lnTotal-_tally)))
      ENDIF
      *** Display the report to screen , file or printer
      IF _TALLY = 0        && No records collected
        *** NO recoeds hove been collected
        =gfModalGen("INM00052B00000","DIALOG")
      ENDIF
      SELECT (lcRpTargt)
      DO gfDispRe WITH EVAL('lcRpForm')
      *** Clear all relations each time you come out of the report program
      *** So when you run the program again you can restablish the relation
      *** With no problem
    ELSE
      IF lnCount < lnTotal
        lnCount=lnTotal-1   
        =lfClosThrm(lnTotal,lnCount)
      ENDIF  
   ENDIF
ENDCASE

*!************************************************************************
*!
*!      Function lfChngCond
*!
*!************************************************************************
*
***  Function to switch between two FRX reports

FUNCTION lfChngCond

llOGFilter=IIF(lcRpForm="SMFLFDIX",.T.,.F.)

*DO CASE
*  CASE lcRpForm =   "SMFLFDIX"
*       =lfChangeGrid('SMFLFDIX')
*  CASE lcRpForm =   "SMFILFLD"
*       =lfChangeGrid('SMFILFLD')  
*ENDCASE  

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

*!************************************************************************
*!
*!      Function lcrpflfld
*!
*!************************************************************************
*
FUNCTION lcrpflfld

*B602935,1 BWA 05/26/1999 Change the width of the CINDX_EXP field from 60 To 120 Characters
*RETURN [SYDFILES.CFILE_NAM,CFILE_TTL,NREC_LEN,]+;
       [SYDFILES.CUPGRDLVL,SYDFILES.LSYS_DATA,]+;
       [SYDFIELD.CFLD_NAME,CDATA_TYP,NFLD_WDTH,]+;
       [NFLD_DEC,CFLD_HEAD,]+;
       [CFLD_MSG,SPACE(12) AS 'CINDX_NAM',SPACE(60) AS 'CINDX_EXP',]+;
       [NINDX_NO,SYDFILES.CFILE_TAG,'F' AS 'cStatus']

RETURN [SYDFILES.CFILE_NAM,CFILE_TTL,NREC_LEN,]+;
       [SYDFILES.CUPGRDLVL,SYDFILES.LSYS_DATA,]+;
       [SYDFIELD.CFLD_NAME,CDATA_TYP,NFLD_WDTH,]+;
       [NFLD_DEC,CFLD_HEAD,]+;
       [CFLD_MSG,SPACE(12) AS 'CINDX_NAM',SPACE(120) AS 'CINDX_EXP',]+;
       [NINDX_NO,SYDFILES.CFILE_TAG,'F' AS 'cStatus']
       
*B602935,1 BWA [END]
 
       
