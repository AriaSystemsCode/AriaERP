*!**************************************************************************
*! Name      : FALMAIN.PRG
*! Developer : AHMED MAHER (AMH)
*! Date      : 05/05/2003
*! Purpose   : Falcon Bay Custom Process Program .
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : Logical value.       C102810,1
*!**************************************************************************

PARAMETER lcEvntFun,lcFunPars

lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLTRIM(lcEvntFun)+'('+lcFunPars+')'

*-- Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue
*-- End of Program.

*!**************************************************************************
*! Name      : lfStyFlter
*! Developer : AHMED MAHER (AMH)
*! Date      : 05/05/2003
*! Purpose   : Set filter for style file.
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Notes : 
*!**************************************************************************
*! C102810
FUNCTION lfStyFlter

PRIVATE lnAlias,lcTmpRejct,lcFilter,lcOrdHdr,lcOrdLine,lcInvHdr,lcInvLine
lnAlias = SELECT(0)
=lfClrFlter()

lcTmpRejct = gfTempName()
CREATE CURSOR (lcTmpRejct) (STYLE C(19))
INDEX ON STYLE TAG (lcTmprejct) OF (lcTmpRejct)

lcOrdHdr  = gfTempName()
lcOrdLine = gfTempName()
lcInvHdr  = gfTempName()
lcInvLine = gfTempName()

=gfOpenFile(gcDataDir+'ORDHDR','ORDACCT','SH',@lcOrdHdr,.T.)
=gfOpenFile(gcDataDir+'ORDLINE','ORDLINE','SH',@lcOrdLine,.T.)
=gfOpenFile(gcDataDir+'INVHDR','INVHDRA','SH',@lcInvHdr,.T.)
=gfOpenFile(gcDataDir+'INVLINE','INVLINE','SH',@lcInvLine,.T.)

IF SEEK(laData[2],lcInvHdr)
  SELECT (lcInvHdr)
  SCAN REST WHILE ACCOUNT+INVOICE = laData[2] FOR STATUS = 'C'
    IF SEEK(INVOICE,lcInvLine)
      SELECT (lcInvLine)
      SCAN REST WHILE INVOICE+STR(LINENO,6) = EVALUATE(lcInvHdr+'.INVOICE');
                  FOR IIF(ORDHDR.SEASON='*',.T.,SEASON = ORDHDR.SEASON) .AND.;
                      !SEEK(STYLE,lcTmpRejct)
        SELECT (lcTmpRejct)
        APPEND BLANK
        REPLACE STYLE WITH EVALUATE(lcInvLine+'.STYLE')
      ENDSCAN
    ENDIF
  ENDSCAN
ENDIF

IF SEEK(laData[2],lcOrdHdr)
  SELECT (lcOrdHdr)
  SCAN REST WHILE ACCOUNT+CORDTYPE+ORDER = laData[2]+'O' FOR STATUS $ 'OH'
    IF SEEK(CORDTYPE+ORDER,lcOrdLine)
      SELECT (lcOrdLine)
      SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = 'O'+EVALUATE(lcOrdHdr+'.ORDER');
                  FOR IIF(ORDHDR.SEASON='*',.T.,SEASON = ORDHDR.SEASON) .AND.;
                      !SEEK(STYLE,lcTmpRejct)
        SELECT (lcTmpRejct)
        APPEND BLANK
        REPLACE STYLE WITH EVALUATE(lcOrdLine+'.STYLE')
      ENDSCAN
    ENDIF
  ENDSCAN
ENDIF

USE IN (lcOrdHdr)
USE IN (lcOrdLine)
USE IN (lcInvHdr)
USE IN (lcInvLine)

SELECT STYLE
lcFilter = "!SEEK(STYLE,'"+lcTmpRejct+"')"
SET FILTER TO &lcFilter.
SELECT (lnAlias)
*--end of lfStyFlter.

*!**************************************************************************
*! Name      : lfClrFlter
*! Developer : AHMED MAHER (AMH)
*! Date      : 05/05/2003
*! Purpose   : Clear filter for style file.
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Notes : 
*!**************************************************************************
*! C102810
FUNCTION lfClrFlter

PRIVATE lnAlias,lcFilter,lnStrPos,lnEndPos,lcTmpRejct
lnAlias = SELECT(0)
SELECT STYLE
lcFilter   = FILTER()
SET FILTER TO
lnStrPos   = AT(',',lcFilter)
lnEndPos   = AT(')',lcFilter)
IF MIN(lnStrPos,lnEndPos) # 0
  lcTmpRejct = EVALUATE(SUBSTR(lcFilter,lnStrPos+1,lnEndPos-lnStrPos-1))
  USE IN (lcTmpRejct)
ENDIF
SELECT (lnAlias)
*--end of lfClrFlter.