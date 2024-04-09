**********************************************************************************************
* Parameter Description: 
*           lcBankCode : Bank Code
*           lctxtBankCode : Bank code
*           lctxtBankName : Variable holds the bank name
*						lctxtChkActCode : Checking account code
*						lctxtChkActName : Checkign account name
*						oCalledFormSet  : FormSet 
**********************************************************************************************

PARAMETERS lcBankCode , lctxtBankCode, lctxtBankName, lctxtChkActCode, lctxtChkActName, llBrowse, oCalledFormSet

IF TYPE("lctxtBankName") = 'C'
	lcBankNameValue  = lctxtBankName   + ".Value"
	lcBankNameOldValue  = lctxtBankName   + ".OldValue"
ENDIF 

IF TYPE("lctxtChkActName") = 'C'
	lcChkActNameValue= lctxtChkActName + ".Value"
	lcChkActNameOldValue= lctxtChkActName + ".OldValue"
ENDIF 

lcBankCodeValue  = lctxtBankCode   + ".Value"
*lcBankNameValue  = lctxtBankName   + ".Value"
lcChkActCodeValue= lctxtChkActCode + ".Value"
*lcChkActNameValue= lctxtChkActName + ".Value"

lcBankCodeOldValue  = lctxtBankCode   + ".OldValue"
*lcBankNameOldValue  = lctxtBankName   + ".OldValue"
lcChkActCodeOldValue= lctxtChkActCode + ".OldValue"
*lcChkActNameOldValue= lctxtChkActName + ".OldValue"


llChkActCodeEnabled = lctxtChkActCode + ".Enabled"

*-- Variable holds the returned value 
llValidValue = .F. 

*-- No need to invoke the browse screen if the user didn't clicked on cmdKey OR 
*-- old value is same as the current value 
*IF !oCalledFormSet.selectedfrombrowse  AND (oCalledFormSet.&lcBankCodeValue  = oCalledFormSet.&lcBankCodeOldValue)
IF !llBrowse .AND. (oCalledFormSet.&lcBankCodeValue  = oCalledFormSet.&lcBankCodeOldValue)
	RETURN .T.
ENDIF

*-- Save the current file.
lcCurrent = SELECT ()

*- Select the master file for the chart of account 
SELECT APBANKS

DO CASE
  	 
	CASE EMPTY(lcBankCode) AND !llBrowse 
		*-- Refresh the values of bank, bank name, checking account and checking name. 
		oCalledFormSet.&lcBankCodeValue   = "" 
		IF TYPE("lctxtBankName") = 'C'
			oCalledFormSet.&lcBankNameValue   = ""
		ENDIF 
		oCalledFormSet.&lcChkActCodeValue = ""
		IF TYPE("lctxtChkActName") = 'C'
			oCalledFormSet.&lcChkActNameValue = ""
		ENDIF 
		
		*-- Return .F. indicated that the passed bank value is not valid 
		llValidValue = .F. 
		
	CASE llBrowse OR ;
			(!EMPTY(lcBankCode) AND !SEEK(lcBankCode, "APBANKS") ) OR ; 
			LEFT(lcBankCode,1) = "$"  

			DECLARE laRpRetFld(1)
			*lcBrFields    = 'cBnkCode:H="Bank code",cBnkLnDes:H="Long description"'
		    lcBrFields    = 'cBnkCode:H=LANG_Bank_code,cBnkLnDes:H=LANG_Long_description '
			laRpRetFld[1] = ''		
			=gfBrows([],'cBnkCode',"laRpRetFld",'APBANKS',.F.)
			IF EMPTY(laRpRetFld[1]) 
				*-- If the user didn't select bank code. return with the old value 
				oCalledFormSet.&lcBankCodeValue   = oCalledFormSet.&lcBankCodeOldValue   
				llValidValue = .F. 
			ELSE	
			  *-- Refresh the account code and the name with the new values.
				oCalledFormSet.&lcBankCodeValue  = laRpRetFld[1]
				IF TYPE("lctxtBankName") = 'C'
	 				oCalledFormSet.&lcBankNameValue  =  APBANKS.cBnkShDes
	 			ENDIF 
	 			llValidValue = .T. 
			ENDIF  		  

	CASE !EMPTY(lcBankCode) AND SEEK(lcBankCode, "APBANKS") 
		oCalledFormSet.&lcBankCodeValue  =  lcBankCode
		IF TYPE("lctxtBankName") = 'C'
			oCalledFormSet.&lcBankNameValue  =  APBANKS.cBnkShDes
		ENDIF 	
		llValidValue = .T. 
ENDCASE 

*-- Get the Check account information

IF llValidValue AND SEEK(oCalledFormSet.&lcBankCodeValue, "APCHECKS")
	SELECT APCHECKS
  * Display the checking account # and name 	
	oCalledFormSet.&lcChkActCodeValue= APCHECKS.cChkAcct
	IF TYPE("lctxtChkActName") = 'C'
		oCalledFormSet.&lcChkActNameValue= APCHECKS.cChkShDes
	ENDIF 
	
	*-- Enable the checking account 
	oCalledFormSet.&llChkActCodeEnabled = .T.
ELSE
	oCalledFormSet.&lcChkActCodeOldValue = ""
	oCalledFormSet.&llChkActCodeEnabled = .F.	
	IF TYPE("lctxtChkActName") = 'C'
		oCalledFormSet.&lcChkActNameOldValue = ""
	ENDIF 
ENDIF


*-- Back to the current file.
SELECT (lcCurrent) 
RETURN llValidValue 
