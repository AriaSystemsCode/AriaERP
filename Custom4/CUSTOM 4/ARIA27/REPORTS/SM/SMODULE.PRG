*:************************************************************************
*:
*: Procedure file: GLBATUDS.RPR
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
*
*** Report Setup
DO CASE
  CASE lcRpForm = "SMODULS"
    SELECT SYDAPPL
    GO TOP
    DO gfDispRe WITH EVAL('lcRpForm'),IIF(!EMPTY(ALLTRIM(lcRpExp)),'FOR ','')+lcRpExp
  CASE lcRpForm ="SMODULD" 
    SELECT SYDAPPL
    GO TOP
    lcRpFiles  = "SYDAPPL,SYDOBJECT"  && Get slected files name
    ***   Get the field order ****
    lnCount    = -1 
    lnTotal    = RECCOUNT('SYDAPPL')   
     ***   Create select  statment
     =lfTrapEsc(.T.,-1)
    CREATE CURSOR &lcRpTEMP. (MTEMP M)
    APPEND BLANK
    lcRpTFields=lfRpFiles()
    lcRpTExp= IIF(lcRpExp='.T.','','AND '+lcRpExp)

    SELECT &lcRpTFields;
      FROM  SYDAPPL,SYDFILES,&lcRpTEMP.;
      WHERE SYDAPPL.CAPP_ID $ ALLTRIM(SYDFILES.MFILE_APP);
      &lcRpTExp.;
      UNION SELECT SYDAPPL.CAPP_ID,CAPP_NAME,CAPP_VER,LSETREQ,;
                   SYDAPPL.CUPGRDLVL,CAPOBJNAM,CPRGLNAME,CDVLP_NAM,;
                   DLAST_MOD,SYDOBJCT.LSINGUSR,CAPOBJTYP,;
                   SYDOBJCT.CUPGRDLVL AS 'CUPGRDLV',MPRGNAMES,;
                   &lcRpTEMP..MTEMP AS 'MFILE_APP',;
                   SPACE(34) AS 'CFILE_TTL',00000 AS 'NREC_LEN',;
                   000 AS 'NFLD_NO',00 AS 'NINDX_NO','O' AS 'cStatus';
             FROM  SYDAPPL,SYDOBJCT,&lcRpTEMP.;
             WHERE SYDAPPL.CAPP_ID=SYDOBJCT.CAPP_ID .AND. ;
                   lfRpThermo(lnTotal,'lnCount');
             &lcRpTExp.;
             INTO CURSOR &lcRpTargt.;
             ORDER BY 1,19,15
    =lfTrapEsc(.F.)        
    IF !llOGEscPrs  
      IF lnCount < lnTotal
        =lfClosThrm(lnTotal,lnCount,.f.,ABS(lnTotal-_tally))
      ENDIF
     *** Display the report to screen , file or printer
     IF _TALLY = 0        && No records collected
       *** NO recoeds hove been collected
       =gfModalGen("INM00052B00000","DIALOG")
       RETURN
     ENDIF
    SELECT (lcRpTargt)
    DO gfDispRe WITH EVAL('lcRpForm') 
    *** Clear all relations each time you comm out of the report program
    *** So when you run the program again you can restablish the relation
    *** With no problem
  ELSE
    IF lnCount < lnTotal
      lnCount=lnTotal-1   
      =lfClosThrm(lnTotal,lnCount)
      RETURN
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
llOGFilter=IIF(lcRpForm="SMODULS",.F.,.T.)
DO CASE
  CASE lcRpForm =   "SMODULS"
       =lfChangeGrid('SMODULE')
  CASE lcRpForm =   "SMODULD"
       =lfChangeGrid('SMODULD')  
ENDCASE  

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
IF USED(lcRpTEMP)
  USE IN ALIAS(lcRpTEMP)
ENDIF 
** EOF

*!************************************************************************
*!
*!      Function lfRpFiles
*!
*!************************************************************************
*
FUNCTION lfRpFiles
RETURN [SYDAPPL.CAPP_ID,CAPP_NAME,CAPP_VER,LSETREQ,SYDAPPL.CUPGRDLVL,]+;
  [SPACE(12) AS 'CAPOBJNAM',SPACE(40) AS 'CPRGLNAME',]+;
  [SPACE(15) AS 'CDVLP_NAM',{} AS 'DLAST_MOD',.F. AS 'LSINGUSR',]+;
  [SPACE(1) AS 'CAPOBJTYP',SPACE(1) AS 'CUPGRDLV',]+;
  [&lcRpTEMP..MTEMP AS 'MPRGNAMES',MFILE_APP,CFILE_TTL,NREC_LEN,]+;
  [NFLD_NO,NINDX_NO,'F' AS 'cStatus']
