LPARAMETERS lcErrState,lcErrTarg,lcErrMsg

lnSAlias = SELECT()
SELECT TBLError 
APPEND BLANK
REPLACE CfUNCTION WITH lcErrState,;
        TARGET    WITH lcErrTarg,;
        WCOND     WITH lcErrMsg
SELECT(lnSAlias)
RETURN
