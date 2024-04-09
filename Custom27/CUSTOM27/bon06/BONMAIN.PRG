*!**************************************************************************
*! Name      : BONMAIN.PRG
*! Developer : TMI - TAREK MOHAMMED IBRAHIM
*! Date      : 08/18/2004
*! Purpose   : BONG HWA Custom Process Program .
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : Logical value.       C200597,1
*!**************************************************************************
*
PARAMETER lcEvntFun,lcFunPars

lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLTRIM(lcEvntFun)+'('+lcFunPars+')'

*-- Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue
*-- End of Program.

*:**************************************************************************
*:* Name        : lfTraceKey
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/18/2004
*:* Purpose     : A copy of the global function gfTraceKey , customized for Bon Wha 
*                 to work based on a setup option regardless if CM module is installed 
*                 or not , 
*                 It also is restricted to work for the files CUSTOMER,
*                                                             STYLE   ,
*                                                             CODES   ,
*                                                             SCALE   ,
*                                                             SALESREP
*                 Actually no condition will be put in the code to check what is the incoming file
*                 but the restriction is by adding appropriate triggers.
*                 Also restricted for the codes SHIPVIA   ,
*                                               CTERMCODE ,
*                                               CORDERCAT ,
*                                               SEASON    ,
*                                               CDIVISION ,
*                                               COLOR     ,
*                                               CSTYGROUP 
*:***************************************************************************
*C200597,1 
FUNCTION lfTraceKey
PARAMETERS lcFileName,lcKeyExpr,lcEventOccr,lcUpdtDir,lcUpdtModl

IF TYPE('lcUpdtDir') $ 'UL' OR EMPTY(lcUpdtDir) OR TYPE('lcUpdtModl') $ 'UL' OR EMPTY(lcUpdtModl)
  lcUpdtDir  = gcDataDir
  lcUpdtModl = gcComp_Mdl
ENDIF

** If the communication module is installed for the active company then 
** return , and the global function will work

IF ('CM' $ lcUpdtModl)
  RETURN
ELSE
  *- check the setup parameter "Force to use Trace Key"
  IF !gfGetMemVar('M_LUSETRCE')
    RETURN  
  ENDIF
  IF PADR(UPPER(lcFileName),8) = 'CODES   '
    *- take only these codes in considration
    IF !UPPER(SUBSTR(lcKeyExpr,2,6)) $ 'SHIPVIA   |CTERMCODE |CORDERCAT |SEASON    |CDIVISION |COLOR     |CSTYGROUP '
      RETURN
    ENDIF
  ENDIF
ENDIF

PRIVATE lnAlias
lnAlias = SELECT()            && save the active work area
USE (lcUpdtDir+'CMTRACE') IN 0 ORDER TAG CTRANS AGAIN ALIAS TRACE_ALIS

lcFileName = PADR(UPPER(lcFileName),8)  && make the file name 8 char width
lcKeyExpr = lcKeyExpr+CHR(250)
lcKeyExpr = PADR(lcKeyExpr,LEN(TRACE_ALIS.CKEYEXPR))  && make the key expr 80 char width

SELECT TRACE_ALIS
SET ORDER TO TAG CTRANS

*- work only with the listed files
*IF lcFileName $ 'CUSTOMER|STYLE   |CODES   |SCALE   |SALESREP|'
  ** check the event occured on the record
  DO CASE
    ** if the event was add new record then add the record directly in
    ** the trans. tracing file
    CASE lcEventOccr = 'A'
      
     INSERT INTO TRACE_ALIS (CFILE_NAM,CKEYEXPR,CSTATUS);
                    VALUES  (lcFileName,lcKeyExpr,lcEventOccr)

    ** otherwise if the event was delete or modify then             
    OTHERWISE
      ** check if there was a record added for the same file+key in the
      ** trans. tracing file
      IF SEEK(lcFileName+lcKeyExpr)
        ** if there was record for the file+key then
        DO CASE
          ** if the event was deleting a record and the record in the
          ** trans. tracing file was just added the delete the record
          ** from the trans. tracing file
          CASE lcEventOccr = 'D' AND CSTATUS = 'A'
            BLANK
            DELETE
          ** if the event was deleting a record and the record in the
          ** trans. tracing file was modied then change the status
          ** of the record in the trans. tracing file to deleted
          CASE lcEventOccr = 'D' AND CSTATUS = 'M'        
            REPLACE CSTATUS WITH 'D'
        ENDCASE
      ** else if there was no record for file+key in the trans. tracing
      ** file then add new record with status like the event passed for
      ** the function
      ELSE
        INSERT INTO TRACE_ALIS (CFILE_NAM,CKEYEXPR,CSTATUS);
                       VALUES  (lcFileName,lcKeyExpr,lcEventOccr)
      ENDIF
  ENDCASE  
*ENDIF
** clear relation on the site views file

USE IN TRACE_ALIS

** restore the current work area
SELECT (lnAlias)
*-- end of gfTraceKey.
