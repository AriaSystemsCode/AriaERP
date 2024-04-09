*!*************************************************************************
*! 
*! Validation function for date by giving messages.
*! 
*! Parameters :
*!
*! 1- The company that you want to validate from its periods.
*!
*! 2- Logical parameter to accept the date in a locked period.
*!
*! This function call the LFVLDATE() with the same parameters.
*!
*!*************************************************************************
*
PARAMETERS lcCompany,lcCrPrd,lcCrYer,ldDateTVal,llLockPer

PRIVATE llLockStat,lcCompYer,lcCompPrd
PRIVATE lnYear,lcYerPer,lcMessage,lcMessgSel
PRIVATE lcCurYear,lcCurPrd,llVaildDate

lcCompYer  = ' '   && Variable to hlod the current year of the company.
lcCompPrd  = ' '   && Variable to hlod the current period of the company.

lcCurYear  = ' '
lcCurPrd   = ' '

llVaildDate= .F.   && Variable to indicate if the date is valid or not

** Variable to hold the year of the period.
lcCrYer    = IIF(TYPE('lcCrYer') <> 'C',' ',lcCrYer)

** Varibale to hold the period No.
lcCrPrd    = IIF(TYPE('lcCrPrd') <> 'C',' ',lcCrPrd)

llLockPer  = IIF(llLockPer,llLockPer,.F.)

lnYear     = 0     && Variable to hold the year No.

llLockStat = .F.   && Variable to hold the lock stat of the period. 

IF lfVlDate(lcCompany,lcCrPrd,lcCrYer,ldDateTVal)  && Call the date validation function.

  ** Variable to hold the period / year combined.
  lcCrPrd  = lcCurPrd
  lcCrYer  = lcCurYear 
  lcYerPer = lcCurPrd+'-'+lcCurYear 

  ** In this variable we are going to subtract the company current year 
  ** from the year of the period and add 3 to this variable.

  lnYear   = (INT(VAL(lcCurYear))-INT(VAL(lcCompYer))) + 3

  ** We are going to check if the variable is less than zero so we are
  ** going to assign it to 1 else we are going to live it as it is.
  
  lnYear   = IIF(lnYear <= 0,1,lnYear)
  
  *-- MAN, Start Validating if the period is current or previous
  *--      in cas that the year is current. 
  IF lnYear = 3
    lnYear  = lnYear + SIGN(VAL(lcCrPrd) - VAL(lcCurr_prd))
  ENDIF  && End Period Validation
  
  DO CASE
    ** If the period lock stat is .T. and the parameter is .T.                   
    CASE llLockStat 
   
      ** MESSAGE : " This date falls in a locked period ð.    "
      **           "                    ® Ok ¯                "

      =gfModalGen("TRM00134B00000","DIALOG",lcYerPer)
      llVaildDate = .F.
      
    ** If the year variable is greater than 0.
    CASE lnYear > 0 

      ** In this variable we include all the year status.
      lcMessage  = 'history prior   current  future  '

      ** We take a substring from the lcMessage by multiply 
      ** the year variable by 8 and subtract 7.
      lcMessgSel = ALLTRIM(SUBSTR(lcMessage,(lnYear*8)-7,8))

      ** MESSAGE : " This date falls in ð period ð. "
      **           "                    ® Ok ¯          "
      
      ** Dont display the message if the date in the current period
      IF lnYear <> 3    
        =gfModalGen("TRM00136B00000","DIALOG",lcMessgSel+"|"+lcYerPer)
      ENDIF
      
      IF lnYear = 1   && If the year = 1 we are not going to accept.
        llVaildDate = .F.
      ELSE
        llVaildDate = .T.  
      ENDIF  
  ENDCASE
ELSE
  ** If the period year or the period no is empty

  **MESSAGE : " This date does not belong to any period  "
  **          "                    ® Ok ¯                "

  =gfModalGen("TRM00133B00000","DIALOG")
    
  llVaildDate = .F.
ENDIF

RETURN llVaildDate