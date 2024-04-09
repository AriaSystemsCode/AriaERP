*!*************************************************************
*! Name        : lfEndLine
*! Developer   : BASSEM RAFAT ERNEST (BWA)
*! Date        : 04/05/2001
*! Purpose     : To get the Record count of the lcTmpTrans dbwf
*!*************************************************************
*! Called from : ARCSTMB.FRX
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Refer to    : 
*!*************************************************************
*! Example     : lfEndLine
*!*************************************************************
FUNCTION lfEndLine
PARAMETERS LCDUMMY

PRIVATE lcAlias , lcSeek , lcOrder

*lnKeyRecNo      && Variable to hold the number of the last Line per
                 && GroupKey and the variable in Syrepuvr.dbf

lcAlias = ALIAS()
STORE SPACE(0) TO lcSeek
lcSeek = IIF(llMulCurr AND lcRpCurr = 'F', cGroupKey + STR(EVALUATE(lcTmpTrans+'.nGroup'),6) + EVALUATE(lcTmpTrans+'.cCurrCode') , cGroupKey )
SELECT (lcTmpTrn_A)
lcOrder = ORDER()
SET ORDER TO TAG lcTmpTrans DESCENDING

=SEEK(lcSeek,lcTmpTrn_A)
lnKeyRecNo = IIF(EOF(lcTmpTrn_A) , 0 , RECNO(lcTmpTrn_A))

SET ORDER TO TAG lcTmpTrans ASCENDING
SELECT (lcAlias)

LCDUMMY = .T.
RETURN LCDUMMY

*--End of lfEndLine.
