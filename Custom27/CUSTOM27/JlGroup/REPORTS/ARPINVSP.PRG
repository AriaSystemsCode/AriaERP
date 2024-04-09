*:***************************************************************************
*: Program file  : ARPINVSP.PRG
*: Program desc. : CUSTOMIZED INVOICE FOR SOPE CREEK.
*: Date          : 05/28/2001
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT ERNEST (BWA)
*: Tracking Job Number: C102287
*: 
*:***************************************************************************
*: Calls : 						
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARPINVSP
*:***************************************************************************
*B605672,1 SSE 03/10/2002 Fix bug of not printing all lines of invoice more than 7 lines.

DECLARE laNamchrg[10] , laValchrg[10]
STORE SPACE(0) TO laNamchrg
STORE 0 TO laValchrg

*B605672,1 Open SydField here. [Begin]
IF USED('SYDFIELD')
  =gfOpenFile(gcSysHome+'SYDFIELD',gcSysHome+'CFLD_NAME','SH')
ENDIF
*B605672,1 Open SydField here. [End]

IF !USED('JLCHARGS')
  =gfOpenFile(gcDataDir+'JLCHARGS',gcDataDir+'INVOICE','SH')
ENDIF

SELECT INVHDR
SET RELATION TO INVOICE + ACCOUNT + ORDER + STORE + PIKTKT INTO JLCHARGS ADDITIVE

                       *-- End of the Program --*

*!*************************************************************
*! Name      : lfChargs
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 06/17/2001
*! Purpose   : Function to Get the charges.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       :
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfChargs
PARAMETER lcChargs

DECLARE laNamchrg[10] , laValchrg[10]
STORE SPACE(0) TO laNamchrg
STORE 0 TO laValchrg

*B605672,1 Commented out. [Begin]
*IF USED('SYDFIELD')
*  =gfOpenFile(gcSysHome+'SYDFIELD',gcSysHome+'CFLD_NAME','SH')
*ENDIF
*B605672,1 Commented out. [End]

FOR lnLoop = 1 TO 10
    lcLop = ALLTRIM(STR(lnLoop))
    laNamchrg[&lcLop] = ALLTRIM(IIF(SEEK('NCHRG'+lcLop , 'SYDFIELD') , SYDFIELD.CFLD_HEAD , ''))
ENDFOR

FOR lnLoop = 1 TO 10
    lcLop = ALLTRIM(STR(lnLoop))
    laValchrg[&lcLop] = JLCHARGS.NCHRG&lcLop
ENDFOR

FOR lnLoop = 1 TO 10
    lcLop = ALLTRIM(STR(lnLoop))
    IF laValchrg[&lcLop] <= 0
      laNamchrg[&lcLop] = SPACE(0)
    ENDIF
ENDFOR

FOR lnArray = 1 TO 2
  IF lnArray = 1
    lcArrayNam = 'laNamchrg'
  ELSE
    lcArrayNam = 'laValchrg'
  ENDIF

  FOR lnCount = 1 TO 10
    IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
      EMPTY(&lcArrayNam.[lnCount])
      =ADEL(&lcArrayNam , lnCount)
      lnCount = lnCount - 1
    ENDIF
  ENDFOR

  FOR lnCount = 1 TO 10
    IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "N" .AND.;
      EVAL(lcarraynam + "[" + str(lncount,1) + "]" ) <= 0
      =ADEL(&lcArrayNam , lnCount)
      lnCount = lnCount - 1
    ENDIF
  ENDFOR

ENDFOR

FOR lnLoop = 1 TO 10
    IF TYPE('laValchrg[lnLoop]') = "L"
      STORE 0 TO laValchrg[lnLoop]
    ENDIF
    
    IF TYPE('laNamchrg[lnLoop]') = "L"
      STORE SPACE(0) TO laNamchrg[lnLoop]
    ENDIF
ENDFOR

RETURN ''
*-- End of lfChargs.
