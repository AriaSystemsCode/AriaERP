
**  Utility to provide a currency description for the order confirmation.

lcFile = ALIAS()
STORE '' TO lcCurrDesc
IF .NOT. USED('SYCCURR')
	SELECT 0
	USE (gcSysHome + 'SYCCURR')
ENDIF
SELECT SYCCURR
SET ORDER TO CCURRCODE
IF SEEK(ORDHDR.CCURRCODE),6)+'N'+'COLOR')
	STORE alltrim(CCURRDESC) TO lcCurrDesc
ENDIF	
SELECT &lcFile
RETURN 
