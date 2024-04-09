*:***************************************************************************
*: Program file  : AP1099F.PRG
*: Program desc. : 1099 Form
*: System        : ARIA4XP
*: Module        : Accounts Payable(AP)
*: Developer     : AHMED MOUSTAFA (AHS)
*: Date          : 09/13/2009    
*: Entry#        : E302639
*:***************************************************************************
* Modifications
*:***************************************************************************
*E302639,3 TMI 08/07/2011 allow the report to work in A4xp
*:***************************************************************************
*-- Variable declarations
STORE .F. TO llUsApSetUp 
STORE '' TO lcTempFile

*-- Getting temp name
lcTempFile = LoOGScroll.gfTempName()

IF !USED('APVENDOR') 
  =gfOpenTABLE('APVENDOR','VENCODE','SH')
  SELECT APVENDOR
  =gfSeek('')
ENDIF
IF !USED('APSETUP') 
  =gfOpenTABLE('APSETUP','APSETUP','SH')
  SELECT APSETUP
  =gfSeek('')
ENDIF

=lfvCrTemp()     && To create Temp File

SELECT APVENDOR
  SCAN 
    SCATTER MEMVAR memo 
    SELECT &lcTempFile
    APPEND BLANK 
    GATHER MEMVAR memo
  ENDSCAN 



*-- If 1099 balance is not cleared
IF !llRpClerVB 
  SELECT &lcTempFile
  *AHS
  lcRpExp = "EMPTY(&lcTempFile..cven1099t)= .F. AND &lcTempFile..nven1099B >= 0.01 = .T."
  *AHS
  DO gfDispRe WITH EVAL('lcRpForm') , 'FOR '+lcRpExp  
ELSE     
  *-- If 1099 balance is not cleared and selected records wasn't locked
  IF !lfLokRec()
    RETURN
  ELSE    &&Else if the selected records was locked or the user selected to continue without clearing the vendors 1099 balance
    *-- If Statment to check if we are not going to clear the vendors 1099 balance
    IF !llRpClerVB
      SELECT &lcTempFile
      *AHS
      lcRpExp = "EMPTY(&lcTempFile..cven1099t)= .F. AND &lcTempFile..nven1099B >= 0.01 = .T."
      *AHS
      DO gfDispRe WITH EVAL('lcRpForm') , 'FOR '+lcRpExp
    ELSE   
      SELECT &lcTempFile      
      *AHS
      lcRpExp = "EMPTY(&lcTempFile..cven1099t)= .F. AND &lcTempFile..nven1099B >= 0.01 = .T."
      *AHS
      DO gfDispRe WITH EVAL('lcRpForm') , 'FOR '+lcRpExp
      IF !EOF()      
        =lfCler1009()
      ENDIF
    ENDIF   
  ENDIF   
ENDIF   


*!*	*-- End Print The Report.
*!**************************************************************************
*-- Functions and Procedures :
*!**************************************************************************
*! Function      : lfvCrTemp
*! Purpose       : Creating Temp file  
*! Developer     : AHMED MOUSTAFA (AHS)     
*! Date          : 09/08/2009
*!**************************************************************************
FUNCTION lfvCrTemp
DIMENSION laTempStru[21,4]

  laTempstru[1,1]='CVENTAXID'
  laTempstru[1,2]='C'
  laTempstru[1,3]= 15
  laTempstru[1,4]= 0
   
  laTempstru[2,1]='CVENCOMP'
  laTempstru[2,2]='C'
  laTempstru[2,3]= 30
  laTempstru[2,4]= 0

  laTempstru[3,1]='NVEN1099B'
  laTempstru[3,2]='N'
  laTempstru[3,3]= 15
  laTempstru[3,4]= 2

  laTempstru[4,1]='CADDRESS1'
  laTempstru[4,2]='C'
  laTempstru[4,3]= 30
  laTempstru[4,4]= 0
  
  laTempstru[5,1]='CADDRESS2'
  laTempstru[5,2]='C'
  laTempstru[5,3]= 30
  laTempstru[5,4]= 0

  laTempstru[6,1]='CADDRESS3'
  laTempstru[6,2]='C'
  laTempstru[6,3]= 30
  laTempstru[6,4]= 0
        
  laTempstru[7,1]='CVENDCODE'
  laTempstru[7,2]='C'
  laTempstru[7,3]= 8
  laTempstru[7,4]= 0
   
  laTempstru[8,1]='CCOM_NAME'
  laTempstru[8,2]='C'
  laTempstru[8,3]= 30
  laTempstru[8,4]= 0
  
  laTempstru[9,1]='LLOK_STAT'
  laTempstru[9,2]='L'
  laTempstru[9,3]= 1
  laTempstru[9,4]= 0
  
  laTempstru[10,1]='CLOK_USER'
  laTempstru[10,2]='C'
  laTempstru[10,3]= 8
  laTempstru[10,4]= 0
  
  laTempstru[11,1]='DLOK_DATE'
  laTempstru[11,2]='D'
  laTempstru[11,3]= 10
  laTempstru[11,4]= 0
  
  laTempstru[12,1]='CLOK_TIME'
  laTempstru[12,2]='C'
  laTempstru[12,3]= 8
  laTempstru[12,4]= 0
  
  laTempstru[13,1]='CADD_TIME'
  laTempstru[13,2]='C'
  laTempstru[13,3]= 11
  laTempstru[13,4]= 0
  
  laTempstru[14,1]='CADD_USER'
  laTempstru[14,2]='C'
  laTempstru[14,3]= 10
  laTempstru[14,4]= 0
    
  laTempstru[15,1]='CADD_VER'
  laTempstru[15,2]='C'
  laTempstru[15,3]= 3
  laTempstru[15,4]= 0
  
  laTempstru[16,1]='CEDIT_TIME'
  laTempstru[16,2]='C'
  laTempstru[16,3]= 11
  laTempstru[16,4]= 0
  
  laTempstru[17,1]='CEDIT_USER'
  laTempstru[17,2]='C'
  laTempstru[17,3]= 10
  laTempstru[17,4]= 0
  
  laTempstru[18,1]='CEDT_VER'
  laTempstru[18,2]='C'
  laTempstru[18,3]= 3
  laTempstru[18,4]= 0

  laTempstru[19,1]='CVEN1099T'
  laTempstru[19,2]='C'
  laTempstru[19,3]= 6
  laTempstru[19,4]= 0
  
  laTempstru[20,1]='CADDRESS4'
  laTempstru[20,2]='C'
  laTempstru[20,3]= 30
  laTempstru[20,4]= 0
  
  laTempstru[21,1]='CADDRESS5'
  laTempstru[21,2]='C'
  laTempstru[21,3]= 30
  laTempstru[21,4]= 0
             
IF USED(lcTempFile)
  USE IN (lcTempFile)
ENDIF 

=gfCrtTmp(lcTempFile,@laTempStru,"CVENDCODE",lcTempFile,.T.)
*--End of function
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

SELECT &lcTempFile
*-- SCAN Loop to scan the cursor (lcCurName).
SCAN 
  *SELECT APVENDOR
  *-- IF Statment to check if the record was not locked.
  IF !gfRlock()
    llRpClerVB = .F.    && Set the flag of clear vendors balance to .F.

    *** Message: " One or more records is in use by another user. "
    *** Message: " Print the form without clearing vendor balances"
    *** Choices: "          < Yes >        < No >                 "  
    
    lnContinue = gfModalGen("QRM04195B00006","DIALOG")
    =lfUnLokRec(2)
    EXIT
  ENDIF    
ENDSCAN   

RETURN IIF(lnContinue = 1 , .T. , .F.)
*-- End Of lfLokRec.
*!*************************************************************
*! Name         : lfUnLokRec
*! Developer    : AHMED MOUSTAFA - (AHS)
*! Date         : 09/28/2009
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
FUNCTION lfUnLokRec

PARAMETERS lnClear

SELECT &lcTempFile
GOTO TOP

IF lnClear = 1
  SELECT APVENDOR
  SCAN FOR EMPTY(APVENDOR.cven1099t)= .F. AND APVENDOR.nven1099B >= 0.01 = .T.
    =gfReplace("nVen1099B WITH 0")
    =gfTableUpdate(.T.,'APVENDOR')   
  ENDSCAN   
ELSE   
 * SCAN FOR EMPTY(APVENDOR.cven1099t)= .F. AND APVENDOR.nven1099B >= 0.01 = .T.
    SELECT APVENDOR
   =gfObj_lock(.F.)
    SELECT &lcTempFile
 * ENDSCAN   && End of SCAN Loop
ENDIF  
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

*E302639,3 TMI 08/07/2011 [Start] do not run the report if the setup is not ok for 1099 form
IF !llSetupDum
  RETURN .F.
ENDIF
*E302639,3 TMI 08/07/2011 [End  ] 

*-- IF Statment to check if the APSETUP file was opened before.
DIMENSION laCompAdd[5,1]
laCompAdd = ''           && Array to hold the Company address.

lc1099CmNu  = ' '    && Varible to hold the State/Payer's state number.

*-- IF Statment to check if we was able to Seek the Active Company in the
*-- SYCCOMP file.
*-- Get company Address [begin].
SELECT SYCCOMP
*AHS
*= SEEK (gcAct_Comp)
=gfseek(oAriaApplication.ActiveCompanyID)
*AHS
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
*!*	*-- IF Statment to check if the APSETUP file is not in use.
*!*	IF !USED('APSETUP')
*!*	  USE APSETUP IN 0
*!*	ENDIF    && End of IF
IF !USED('APSETUP') 
  =gfOpenTABLE('APSETUP','APSETUP','SH')
  SELECT APSETUP
  =gfSeek('')
ENDIF

SELECT APSETUP
*-- IF Statment to check if the 1099 processing is Suppresed.
IF FILE('C:\X.X')
  ON ERROR
  SET STEP ON 
  _SCREEN.Visible=.T.
ENDIF


IF lApS1099

  *** Message: " 1099 calculation is not implemented from your      "
  *** Message: " Accounts Payable setup. Form 1099 is not available."
  *** Choices: "                     < Ok >                         "  
  =gfModalGen("INM04193B00000","DIALOG")
  *E302639,3 TMI 08/07/2011 [Start] set the variable to false 
  *RETURN 
  RETURN .F.
  *E302639,3 TMI 08/07/2011 [End  ] 
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
*! Developer : Omar Shaban (Oms)
*! Date      : 02/22/2006
*! Purpose   : used to Set the active Form to Variable lcRpForm
*!********************************************************************************
FUNCTION lfsrvForm

lcRpForm = IIF(lcRpFormat="3","AP1099F","AP10992")

*-- End Of lfsrvForm.