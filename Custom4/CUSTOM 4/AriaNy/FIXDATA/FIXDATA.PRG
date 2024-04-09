*!**************************************************************************
*! Name      : FIXDATA.PRG
*! Purpose   : Rplace all fields that have NULL values with default values
*! Auth      : Amin Khdoary  (AKA)   
*! Date 		 : 05/15/2004
*!**************************************************************************
*! Modifications :
*!**********************************************************************************
PRIVATE lcConnStr, lnHandleConnect

SET Safety OFF
lcConnStr = lfReadConStr()
lnHandleConnect = SQLSTRINGCONNECT(lcConnStr)           


IF lnHandleConnect  < 1 
	WAIT WINDOW "Not able to connect to the Database!"
	RETURN 
ENDIF 	


=ValidateNull ("customertransaction")
=ValidateNull ("cstsoftw")
=ValidateNull ("custprof")
=ValidateNull ("suappmod")
=ValidateNull ("aria_app")
=ValidateNull ("trackingentries")
=ValidateNull ("suactkey")
=ValidateNull ("custconn")

=SQLDISCONNECT(lnHandleConnect)

RETURN 



FUNCTION ValidateNull 
PARAMETERS lcFile 

=SQLEXEC(lnHandleConnect  , "SELECT * FROM &lcFile ", '&lcFile')

SELECT &lcFile
COPY STRUCTURE TO C:\ARIANY\&lcFile
USE C:\ARIANY\&lcFile IN 0  ALIAS TEMPCURSOR 

SELECT &lcFile
=AFIELDS(laFileField)
lnFileField = ALEN(laFileField,1)

SCAN   
	lAddNewRec = .T. 
	lcStr =""
  lcPrimKey = " rowguid ='" +&lcFile..rowguid +"'"
	FOR nLoop = 1 TO lnFileField 
		SELECT &lcFile
		lcField= ALLTRIM(laFileField[nLoop,1])
		IF ISNULL(EVALUATE("&lcFile"+".&lcField"))
			*-- Buidl String 
			IF EMPTY(lcStr)
				lcStr ="Set "
			ELSE
				lcStr =lcStr + ","
			ENDIF 	
			DO CASE
				CASE laFileField[nLoop,2] $ 'CM'
					*REPLACE &lcField WITH ""
				  lcStr = lcStr +"["+lcField+"]="+ "''"
				CASE laFileField[nLoop,2] $ 'IBN'
					*REPLACE &lcField WITH 0
					lcStr = lcStr +"["+lcField+"]="+ "0"
				CASE laFileField[nLoop,2] = 'L'
					*REPLACE &lcField WITH .F.				
					lcStr = lcStr +"["+lcField+"]="+ "0"
					
				CASE laFileField[nLoop,2] = 'T'
					*REPLACE &lcField WITH CTOD("01/01/1900")
					lcStr = lcStr +"["+lcField+"]='"+ "1/1/1900" + "'"
			ENDCASE
		ENDIF 
	ENDFOR 	

  IF LEN(lcStr) > 4  
		
		lnExResult = SQLEXEC(lnHandleConnect  ,"UPDATE "+lcFile+" "+lcStr+" WHERE "+lcPrimKey)
		IF lnExResult > 0 
			SCATTER MEMVAR MEMO 
			SELECT TEMPCURSOR 
			IF lAddNewRec = .T. 
			 APPEND BLANK 
			ENDIF 	
			lAddNewRec = .F. 
			GATHER MEMVAR MEMO 
		ELSE
			WAIT WINDOW "Not able to update "+ lcFile + "!!!"
		ENDIF	
	ENDIF 	
	SELECT &lcFile	
ENDSCAN

SELECT (lcFile)
USE

SELECT TEMPCURSOR 
USE 
RETURN 




*!**************************************************************************
*! Name      : lfReadConStr
*! Purpose   : Read SQL connection string. (get it if not defined).
*!**************************************************************************
FUNCTION lfReadConStr

*--Read connection string.
lcConnFile = ""

*-- Cairo Connection Inforamtion for ARIANY DATAbase
CCONSERVER = 'WEBSERVER2'
CCONDBNAME = 'NYDB01'
CCONUSERID = 'ARIA4'
CCONPASWRD = 'ARIA4'

  
  *lcConnStr = "driver={SQL Server};server=ariaweb;DATABASE=db01;uid=msp;pwd=aria"  &&Egypt connection string.
   *-- For ARIAny DATA in WEBSERVER2 in Cairo 
	lcConnStr = "driver={SQL Server};server=WEBSERVER2;DATABASE=NYDB01;uid=ARIA4;pwd=ARIA4"
  *-- For ARIANY DATA in ARIASRV1 in NY 
	*!*lcConnStr = "driver={SQL Server};server=ariasrv1;DATABASE=DB01;uid=web;pwd=webaria"
  
  lcConnStr = INPUTBOX("Enter connection string","SQL Connection string",lcConnStr)
  IF EMPTY(lcConnStr)
    MESSAGEBOX("No server connection information are defined, unable to proceed!",16,_SCREEN.Caption)
  ELSE
    =STRTOFILE(ALLTRIM(lcConnStr),lcConnFile,0)
  ENDIF

RETURN lcConnStr
