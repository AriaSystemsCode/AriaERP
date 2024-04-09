*:******************************************************************************
*: Program file  : AP1099F.PRG
*: Program desc. : Print 
*: System        : Aria Apparel System (A27)
*: Module        : Accounts Payable    (AP )
*: Developer     : ABDOU ELGENDI     - (ABD) 
*: Date          : 01/25/2000
*:******************************************************************************
*: Calls : 
*:    FUNCTIONS  : lfCler1009 , lfLokRec , lfUnLokRec , lfRpWhen , lfRpSetUp
*:               : lfAdrShift , lfRecLock,lfsrvForm
*:    --------------------------------------------------------------------------
*:    PROCEDURES : 
*:******************************************************************************
*: Passed Parameters : None
*:******************************************************************************
*:Modifications  :
*:******************************************************************************
*B603522,1 SSE 03/19/2000 Convert all objects in FRX to TRANSPARENT
*B131119,1 Oms 02/22/2006 
*          1-add Option In OPtion Grid to enable the use to select 
*            the layeout tow or three form per apage and hold the resuli in LCRPFORMAT variable
*          2-add Function lfsrvForm to set the Form to Print Two or The Per Page.
*          3-add Another FRX Called AP10992.frx in {\reports\AP\} path.
*-- Variable declarations [ Begin]
STORE .F. TO llUsApSetUp 
*-- Variable declarations [ End ]
*-- IF Statment to check if the user will not clear the vendors 1099 balance
*-- or if the report output device is screen 
IF !llRpClerVB 
  SELECT APVENDOR
  DO gfDispRe WITH EVAL('lcRpForm') , 'FOR '+lcRpExp
ELSE    && Else

  lcCurName = gfTempName()

  SELECT * ;
    FROM APVENDOR;
    WHERE &lcRpExp;
    INTO CURSOR &lcCurName

  SELECT (lcCurName)
  SET RELATION TO cVendCode INTO APVENDOR
  ***************************************************************************
  *-- IF Statment to check if the selected records was locked or the user --*
  *-- selected to continue without clearing the vendors 1099 balance .    --*
  ***************************************************************************
    
  *-- IF Statment to check if the selected records was not locked
  IF !lfLokRec()
    SET RELATION OFF INTO APVENDOR
    RETURN
  ELSE    &Else if the selected records was locked or the user selected to continue without clearing the vendors 1099 balance
    *-- IF Statment to check if we are not going to clear the vendors 1099 balance
    IF !llRpClerVB
      SET RELATION OFF INTO APVENDOR
      SELECT APVENDOR
      DO gfDispRe WITH EVAL('lcRpForm') , 'FOR '+lcRpExp
    ELSE   && Else
      SELECT (lcCurName)
      DO gfDispRe WITH EVAL('lcRpForm')
      IF !EOF()      
        =lfCler1009()
      ENDIF
    ENDIF   && End of IF
  ENDIF   && End of IF
ENDIF   && End of IF
**B131119,1 Oms print two or three form pre page [START]
*lcRpForm=lcactiveFrom
**B131119,1 Oms print two or three form pre page [END]
*-- End Print The Report.
*!******************************************************************************
*-- Functions and Procedures :
*!******************************************************************************
*! Name         : lfCler1009
*! Developer    : ABDOU ELGENDI - (ABD)
*! Date         : 01/25/2000
*! Purpose      : Function to clear the field [APVENDOR.nVen1099B] 
*!******************************************************************************
*! Calls        : gfModalGen()
*!******************************************************************************
*! Parameters   : None
*!******************************************************************************
*! Returns      :  None
*!******************************************************************************
*! Example      :  =lfCler1009()
*!******************************************************************************
*
FUNCTION lfCler1009

*** Message: " Are you sure you want to clear vendors 1099 balance "
*** Choices: "               < Yes >        < No >                 "  
=lfUnLokRec(gfModalGen("QRM40169B00006","DIALOG",'clear vendors 1099 balance.'))

*-- End Of lfCler1009.
*!*************************************************************
*! Name         : lfLokRec
*! Developer    : ABDOU ELGENDI - (ABD)
*! Date         : 01/25/2000
*! Purpose      : Function to lock the selected records
*!*************************************************************
*! Calls        : gfModalGen() , gfObj_lock()
*!*************************************************************
*! Parameters   : None
*!*************************************************************
*! Returns      : 1).T. if it was able to lock the records Or
*!                  if the user selected to continue after all
*!                2)Else it return .F. 
*!*************************************************************
*! Example      : =lfLokRec()
*!*************************************************************
*
FUNCTION lfLokRec

PRIVATE lnContinue
lnContinue = 1

SELECT (lcCurName)
*-- SCAN Loop to scan the cursor (lcCurName).
SCAN 
  SELECT APVENDOR
  *-- IF Statment to check if the record was not locked.
  IF !gfRlock()
    llRpClerVB = .F.    && Set the flag of clear vendors balance to .F.

    *** Message: " One or more records is in use by another user. "
    *** Message: " Print the form without clearing vendor balances"
    *** Choices: "          < Yes >        < No >                 "  
    
    lnContinue = gfModalGen("QRM04195B00006","DIALOG")
    =lfUnLokRec(2)
    EXIT
  ENDIF     && End of IF
  SELECT (lcCurName)
ENDSCAN   && End of Scan loop

RETURN IIF(lnContinue = 1 , .T. , .F.)
*-- End Of lfLokRec.
*!*************************************************************
*! Name         : lfUnLokRec
*! Developer    : ABDOU ELGENDI - (ABD)
*! Date         : 01/25/2000
*! Purpose      : Function to unlock the selected records
*!*************************************************************
*! Calls        : gfObj_lock()
*!*************************************************************
*! Called From  : lfLokRec() , lfCler1009()
*!*************************************************************
*! Parameters   : lnClear
*!*************************************************************
*! Returns      : None 
*!*************************************************************
*! Example      :  =lfUnLokRec(1)
*!*************************************************************
*
FUNCTION lfUnLokRec

PARAMETERS lnClear

SELECT (lcCurName)
GOTO TOP

IF lnClear = 1
  *-- SCAN Loop to scan the cursor (lcCurName).
  SCAN 
    SELECT APVENDOR
    REPLACE nVen1099B WITH 0
    =gfObj_lock(.F.)
    SELECT (lcCurName)
  ENDSCAN   && End of SCAN Loop
ELSE   && Else
  *-- SCAN Loop to scan the cursor (lcCurName).
  SCAN
    SELECT APVENDOR
    =gfObj_lock(.F.)
    SELECT (lcCurName)
  ENDSCAN   && End of SCAN Loop
ENDIF   && End of IF
*-- End Of lfUnLokRec.
*!********************************************************************************
*! Name         : lfRpWhen
*! Developer    : ABDOU ELGENDI - (ABD)
*! Date         : 01/25/2000
*! Purpose      : When Function of the Option Grid
*!********************************************************************************
*! Calls        : gfModalGen()
*!********************************************************************************
*! Called From  : Option Grig
*!********************************************************************************
*!  Parameters  : None
*!********************************************************************************
*! Returns      : .T. or .F. 
*!********************************************************************************
*! Example      :  =lfRpWhen()
*!********************************************************************************
*
FUNCTION lfRpWhen

*-- IF Statment to check if the APSETUP file was opened before.
DIMENSION laCompAdd[5,1]
laCompAdd = ''           && Array to hold the Company address.

lc1099CmNu  = ' '    && Varible to hold the State/Payer's state number.

*-- IF Statment to check if we was able to Seek the Active Company in the
*-- SYCCOMP file.
*-- Get company Address [begin].
SELECT SYCCOMP
= SEEK (gcAct_Comp)
lcRpCompNm = cCom_Name             && Company Name.
= gfGetAdr('SYCCOMP' , '' , '' , '' , @laCompAdd)
= lfAdrShift('laCompAdd')    && Shift Company address if there is empty line.

lcRpCompA1 =laCompAdd[1]
lcRpCompA2 =laCompAdd[2]
lcRpCompA3 =laCompAdd[3]
lcRpCompA4 =laCompAdd[4]
*-- Get company Address [End].

lnAlias = SELECT (0)
SELECT APSETUP
lc1099CmNu = c1099spNu
SELECT(lnAlias)

*--End Of lfRpWhen.
*!********************************************************************************
*! Name         : lfRpSetUp
*! Developer    : ABDOU ELGENDI - (ABD)
*! Date         : 01/25/2000
*! Purpose      : Chek  if the 1099 processing is Suppresed.
*!********************************************************************************
*! Calls        : gfModalGen()
*!********************************************************************************
*! Called From  :  Default Function In SYREPUVR.
*!********************************************************************************
*!  Parameters  : None
*!********************************************************************************
*! Returns      : .T. or .F. 
*!********************************************************************************
*! Example      :  =lfRpSetUp()
*!********************************************************************************
*
FUNCTION lfRpSetUp
*-- IF Statment to check if the APSETUP file is not in use.
IF !USED('APSETUP')
  USE APSETUP IN 0
ENDIF    && End of IF

SELECT APSETUP
*-- IF Statment to check if the 1099 processing is Suppresed.
IF lApS1099

  *** Message: " 1099 calculation is not implemented from your      "
  *** Message: " Accounts Payable setup. Form 1099 is not available."
  *** Choices: "                     < Ok >                         "  
  =gfModalGen("INM04193B00000","DIALOG")

  *-- IF Statment to check if the APSETUP file was opened by this function
  *IF llUsApSetUp
  *  USE
  *ENDIF    && End of IF
  llOGTrmnat = .T.
  CLEAR READ
  RETURN
ENDIF

*-- IF Statment to check if the State/Payer's state number is empty
IF EMPTY(c1099spNu)
  
  *-- IF Statment to check if the user want to fill the State/Payer's state
  *-- number from the Accounts Payable setup screen first
  *** Message: " The State/Payer's state number is not available."
  *** Message: " Do you wish to set it up from the Accounts      "
  *** Message: " Payable setup before printing the form.         "
  *** Choices: "            < Yes >         < No >               "  
  IF gfModalGen("QRM04194B00006","DIALOG") = 1
    llOGTrmnat = .T.
    CLEAR READ
    RETURN .F.
  ENDIF    && End of IF
ENDIF   && End of IF  

*--- End of lfRpSetUp.
*!********************************************************************************
*! Name      : lfAdrShift
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 01/25/2000
*! Purpose   : used to shift the laCompAdd  array.
*!********************************************************************************
*! Passed Parameters  : lcArrayNam : Array hold The address.
*!********************************************************************************
*! Returns            : None
*!********************************************************************************
*! Example   : = lfAdrShift()
*!********************************************************************************
*
FUNCTION lfAdrShift
PARAMETERS lcArrayNam

*-- FOR Loop to loop the Address Array
FOR lnCount = 1 TO 6
  
  *-- IF The current Array element is of type character and empty
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
     EMPTY(&lcArrayNam.[lnCount])
    
    =ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

*-- FOR Loop to loop the Address Array
FOR lnCount = 1 TO ALEN(&lcArrayNam)
  *-- IF The current Array element is not of type character
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop
*-- End Of lfAdrShift
*!********************************************************************************
*! Name      : lfRecLock
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 12/22/1999
*! Purpose   : Record Lock.
*!********************************************************************************
*! Calls     : None.
*!********************************************************************************
*! Parameters: lcFile->Locked file.
*!********************************************************************************
*! Returns   : None.
*!********************************************************************************
*! Example   : =lfRecLock()
*!********************************************************************************
*
FUNCTION lfRecLock
PARAMETERS lcFile

SET REPROCESS TO 5 SECONDS 
DO WHILE .T.
  *-This record is in use by another user !','\!\<Retry;\<Cancel'
  lnChoice=gfModalGen('INM00029B00015','DIALOG')
  IF lnChoice = 1 
    IF !RLOCK(lcFile)
      LOOP
    ELSE
      lnRet = .T.
      EXIT
    ENDIF 
  ELSE
    lnRet = .F.
    EXIT   
  ENDIF
ENDDO
SET REPROCESS TO 0
RETURN (lnRet)

*-- End Of lfRecLock.
*!********************************************************************************
*! Name      : lfsrvForm
*! Developer :Omar Shaban (Oms)
*! Date      : 02/22/2006
*! Purpose   : used to Set the active Form to Variable lcRpForm
*!********************************************************************************
FUNCTION lfsrvForm

lcRpForm=IIF(lcRpFormat="3","AP1099F","AP10992")

*-- End Of lfsrvForm.