*:************************************************************************
*:
*: Procedure file: APDIVIS.PRG
*:
*:         System: ARIA ADVANTAGE SERIES
*:         Module: Accounts Payable
*:         Author: Hisham Ramsis Philips
*:      Copyright (c) 
*:  Last modified: 11/06/94
*:
*:  Procs & Fncts: lpShow       ** Show proc. for home screen ***
*:               : lfwOldVals   ** Stores old values
*:               : lfvDivDes    ** Get a description of a certain code.***
*:               : lfvData_2    ** Get available bank code and return its description.***
*:               : lfvData_3    ** Get available checks and return its description.***
*:               : lfvData_4    ** Get expense accounts codes and description.***
*:               : lfvData_5    ** get AP accounts codes and descriptions.***
*:               : lfDispAcsDes ** Get Account description calling from view mode.
*:               : lfwAccount   ** When function for all the accounts.
*:               : lfvAccount   ** Valid function for all the accounts.*:
*:               : lpSavScr     ** Local save procedure, added by Renee
*:
*:     Documented 00/00/1994
*:************************************************************************
*B600313,1 Reham On 06/12/95
*B600313,1 Just move the Purchase variance account objects from the screen
*B600313,1 But I didn't edit anything in the screen object file or this PRG.
*600444,1 ORABY 07/26/95 Refresh the screen with clean data in the ADD mode
*C600444,1 HISH 11/26/95 Made the division popup disable in the view mode.
*E300643,1  HS 04/14/97 1)Make some changes for we have change the file
*E300643,1              SYCCODES name to CODES and make it a data file
*E300643,1              [Change its dir. from SYSFILES to DBFS]
*E300643,1              2)Make some changes for we have change the function
*E300643,1              [gfCodDes]
*E300683,1 AHMED 06/04/97 Add screens directory path to the calling of SPRS
*E301077,80 IHB  02/03/1999 Enhance opening and closing files
*E300789,4 IHB  07/03/1999 Remove company ID from ACCOD, FISHD, FSPRD, FSHLD, CODES
*:************************************************************************
*
EXTERNAL ARRAY laData,laKeyField,laScrMode, laDefProc
PRIVATE laBankObjs 
DECLARE laKeyField [1,4],laDivDes[1,1],laDivCod[1,1],laBankObjs[2,3]

** lcOldVal     Variable to hold old bank code.
** lcBankCheck  Variable to hold the check of the bank.
** lcOldAcc     Variable to hold the old Account.
** lcValComp    Variable to hold 
** lcPopCol     Variable to hold 
** lcDivision   Variable to hold devision description in dos condition.

laKeyField[1,1] = 'IBDIVISION'
laKeyField[1,2] = .T.
laKeyField[1,3] = 'CDIVISION'
laKeyField[1,4] = 1

STORE 'Select division' TO lcTBlnkCode
STORE lcTBlnkCode       TO lcDivision  , puDiv
STORE .F.               TO llBrowse    , laDefProc[9]
STORE ' '               TO lcOldVal    , lcBankCheck, lcOldAcc,;
                           lcValComp   , lcPopCol
STORE "DISABLE"         TO lcChkStat

IF !gfSetup()
  RETURN
ENDIF

*** If division file opened for the first time. ***
IF !WEXIST(gcBaseWind)
  SCATTER FIELDS &lcScFields MEMO TO laData BLANK
  STORE lcEmptyAcc TO laData[4] , laData[5] , laData[6] ,;
                      laData[7] , laData[8] , laData[9]

  *** Prepare an array to hold bank objects to be used for
  *** global bank and checking accounts validations as follows :
  *** One row for every object, such that 
  *** row no. 1 holds bank object names,
  *** row no. 2 holds checking account object names
  *** row no. 3 holds the corresponding G/L account object names,
  *** Columns are ordered as follows :
  *** Column no. 1 : invisible button name for corresponding object
  *** Column no. 2 : object name (e.g. bank object name)
  *** Column no. 3 : object description name(if required)
  laBankObjs  = ' '
  laBankObjs[1,1] = 'ibBank'         && Bank code invisible button
  laBankObjs[1,2] = 'laData[2]'     && Bank code 
  laBankObjs[2,1] = 'ibChecks'       && Checking account invisible button
  laBankObjs[2,2] = 'laData[3]'     && Checking account 
ENDIF

IF _WINDOWS

  *E300643,1 Change this line for the changes we have made 
  *          to SYCCODES [Begin]
  *DEFINE POPUP puDivision PROMPT field SYCCODES.cdiscrep scroll;
  *  FROM 3.42,2.88 TO 8.42,33.15 ;
  *  MESSAGE gfObj_msg()

  DEFINE POPUP puDivision PROMPT field CODES.cdiscrep scroll;
    FROM 3.42,2.88 TO 8.42,33.15 ;
    MESSAGE gfObj_msg()

  *E300643,1 Change this line [End]
  
  ON SELECTION POPUP puDivision DO lfvDivision
ENDIF  

*E300643,1 Change this line for the changes we have made to SYCCODES [Begin]
*SELECT SYCCODES
SELECT CODES
*E300643,1 Change this line for the changes we have made to SYCCODES [End]

*E300789,4 IHB  07/03/1999 Remove company ID [start]
*SET FILTER TO CCOMP_ID+CRLTFIELD+CFLD_NAME = gcAct_Comp+'N' + 'CDIVISION '
SET FILTER TO CDEFCODE+CRLTFIELD+CFLD_NAME = 'N'+'N' + 'CDIVISION '
*E300789,4 IHB  [end]

LOCATE

IF FOUND()  && If there is any division in the codes file.
  SELECT APDIV
  *E300683,1 Call *.SPR from screens directory
  * DO Apdivis.SPR 
  DO (gcScrDir + gcWinAppl + '\Apdivis.SPR')
  *E300683,1 end          
ELSE
  *** Codes not found in the Code File ***
  *** Message: " You have to enter your company division through" 
  ***          " the codes program in the system manager.       "    
  *** Choices: "                         < Ok >                 "  
  =gfModalGen("TRM04000B00000","DIALOG") 
  glQuitting=.T.
ENDIF

*E300643,1 Change this line for the changes we have made to SYCCODES [Begin]
*SELECT SYCCODES
SELECT CODES
*E300643,1 Change this line for the changes we have made to SYCCODES [End]
SET FILTER TO 

RELEASE POPUPS puDivision

*!**************************************************************************
*!
*!      Procedure: lpShow
*!
*!**************************************************************************
*
PROCEDURE lpShow

SELECT APDIV

DO CASE
  CASE laScrMode[1] && In case of view mode
    lcDivision = lcTBlnkCode  
    puDiv      = lcTBlnkCode
    STORE lcEmptyAcc TO laData[4],laData[5],laData[6],;
                        laData[7],laData[8],laData[9]
    SHOW GET puDiv ENABLE
  OTHERWISE
    IF laScrMode[2] .OR. laScrMode[3]
      FOR lnCount = 4 TO 9
        laData[lnCount] = IIF(EMPTY(laData[lnCount]),lcEmptyAcc, laData[lnCount])
      ENDFOR

      *E300643,1 Change this line for the changes we have made 
      *          to (gfCodDes) [Begin]
      *lcDivision = gfCodDes(laData[1])
      lcDivision = gfCodDes(laData[1] , 'CDIVISION')
      *E300643,1 Change this line [End]
      
      puDiv      = lcDivision
    *600444,1 Refreshing the screen in case of add mode
    ELSE
      *600444,1 START
      *600444,1 Save the key field value
      lcData_1  = laData[1]
      *600444,1 Clear the ladata array
      SCATTER FIELDS &lcScFields MEMO TO laData BLANK
      *600444,1 store zeroes in the accounts numbers
      STORE lcEmptyAcc TO laData[4] , laData[5] , laData[6] ,;
                          laData[7] , laData[8] , laData[9]     
      *600444,1 Reload the key field value
      laData[1] = lcData_1
      *600444,1 Refresh the screen
      SHOW GETS ONLY
      *600444,1 END
    ENDIF  
    SHOW GET puDiv DISABLE  
ENDCASE    

lcChkStat = IIF(!EMPTY(laData[2]) .AND. (laScrMode[3] .OR. laScrMode[4]),;
                "ENABLE", "DISABLE") 
SHOW GET laData[3] &lcChkStat
SHOW GET ibChecks  &lcChkStat

*IF EMPTY(laData[2])
*  SHOW GET laData[3] DISABLE
*  SHOW GET ibChecks  DISABLE
*ELSE
*  IF !laScrMode[2]
*    SHOW GET laData[3] DISABLE
*    SHOW GET ibChecks  DISABLE
*  ENDIF  
*ENDIF

lcPopCol      = IIF(laScrMode[1] ,SCHEME(1,2), SCHEME(1,10))

IF laScrMode[2] 
  *C600444,1 HISH 11/26/95 Made the division popup disable in the view mode (Begin)
  *SHOW GET puDivision ENABLE
  *SHOW GET ibDivision ENABLE
  SHOW GET puDivision DISABLE
  SHOW GET ibDivision DISABLE
  *C600444,1 (End)
ENDIF

=lfRefresh()
SELECT APDIV

*!**************************************************************************
*!
*!      Function: lfvDivision
*!
*!**************************************************************************
* Valid function of the division popup.
*
FUNCTION lfvDivision
 
DO CASE
  CASE _DOS

    *E300643,1 Change this line for the changes we have made 
    *          to SYCCODES [Begin]
    *laData[1] = gfActPop(0,14,6,47,'SYCCODES','cCode_No','cDiscrep',@lcDivision)
    laData[1] = gfActPop(0,14,6,47,'CODES','cCode_No','cDiscrep',@lcDivision)
    *E300643,1 Change this line [End]
    
    =lfRefresh()
  CASE _WINDOWS

    *E300643,1 Change this lines for the changes we have made 
    *          to SYCCODES [Begin]
    *puDiv = SYCCODES.cdiscrep
    *laData[1] = SYCCODES.cCode_No
    puDiv = CODES.cdiscrep
    laData[1] = CODES.cCode_No
    *E300643,1 Change this lines [End]

    SHOW GET puDiv
    =gfUpdate()
ENDCASE

IF _WINDOWS
  HIDE POPUP puDivision
ENDIF

IF !EMPTY(laData[1])

  SELECT APDIV
  *** Get the information of this code.***
  IF SEEK(laData[1])
    SCATTER FIELDS &lcScFields MEMO TO laData
    FOR lnCount = 4 TO 9
      laData[lnCount] = IIF(EMPTY(laData[lnCount]),lcEmptyAcc, laData[lnCount])
    ENDFOR
    laScrMode=.F.
    laScrMode[2]=.T.
    SHOW GETS 
  ELSE
    *** Codes not found in the Code File *** 
    lnOption = gfModalGen('INM00001B00001','DIALOG',IIF(_DOS,ALLTRIM(lcDivision),ALLTRIM(puDiv)))
    *** Make choices in the message.***
    DO CASE
      CASE lnOption = 1
        =gfBrows()

      CASE lnOption = 2
        laScrMode    = .F.
        laScrMode[4] = .T.
        SHOW GETS

      CASE lnOption = 3
        laScrMode    = .F.
        laScrMode[1] = .T.
        lcDivision   = ' '
        SHOW GETS
    ENDCASE  
  ENDIF  
ENDIF

SELECT APDIV

IF _WINDOWS
  DEACTIVATE POPUP puDivision
ENDIF

=lfRefresh()

*!**************************************************************************
*!
*!      Function: lfwOldVals
*!
*!**************************************************************************
*
FUNCTION lfwOldVals
lcOldVal = EVALUATE(SYS(18))

*!**************************************************************************
*!
*!      Function: lfvBnkChk
*!
*!**************************************************************************
* Valid function for get fields laData[2] (bank code), and laData[3] 
* (checking account code)
*
FUNCTION lfvBnkChk
IF EVALUATE(SYS(18)) <> lcOldVal
  =gfUpdate()
ENDIF
IF !lfBnkChk(@laBankObjs, lcOldVal, @llBrowse, @lcChkStat)
  SELECT APDIV
  RETURN 1
ENDIF  
SELECT APDIV

*!**************************************************************************
*!
*!      Function: lfvData_2
*!
*!**************************************************************************
* Validation of bank.
*
FUNCTION lfvData_2

** If the old variable = new variable there is no need for the validation.
IF !llBrowse .AND. lcOldBank = laData[2]
  RETURN
ENDIF  

SELECT APBANKS              && Select the bank file.
lcSavOrder = SET('ORDER')   && Save old order.
SET ORDER TO TAG BANKCODE   && Set order to bank code.

** Validate if browse or not empty of ladata[2].

IF llBrowse .OR. !SEEK(laData[2]) .OR. ATC("?",laData[2]) > 0
  DIMENSION laTemp[1]  && We are going to declare an array to hold the browse result.
  laTemp = ''          && Filling the array.

  lcSavBrFld=lcBrFields  && Save the browse fields in this variable.
  lcSavTitle=lcFile_Ttl  && Save the browse title.

  ** Assign the variable of the browse fields by the new fields.
  lcBrFields="CBNKCODE  :H= 'Bank Code',;
              CBNKSHDES :H= 'Bank description'"
  lcFile_Ttl="Banks"  && Assign the title variable with file title.
  
  =gfBrows(.F.,'CBNKCODE,CBNKSHDES','laTemp')  && Call the browse function.
    
  lcBrFields=lcSavBrFld   && Restore the old fields.
  lcFile_Ttl=lcSavTitle   && Restore the old title.
    
  IF !EMPTY(laTemp[1])    && If the user select from the browse.
    laData[2] = laTemp[1] && Assign the array element with the browse array.
    laData[3] = LOOKUP(APCHECKS.cChkAcct,laData[2],APCHECKS.cChkAcct,'BANKCHECK')
  ELSE
    laData[2] = lcOldBank  
  ENDIF
ELSE
  laData[3] = LOOKUP(APCHECKS.cChkAcct,laData[2],APCHECKS.cChkAcct,'BANKCHECK')
ENDIF

SHOW GET laData[2]

IF !EMPTY(laData[2])
  SHOW GET laData[3] ENABLE
  SHOW GET ibChecks  ENABLE
ELSE 
  laData[3] = SPACE(12)
  SHOW GET laData[3] DISABLE
  SHOW GET ibChecks  DISABLE
ENDIF  

IF !EMPTY(lcSavOrder)
  SELECT APBANKS
  SET ORDER TO &lcSavOrder
ENDIF

llBrowse = .F.

SELECT APDIV

*!**************************************************************************
*!
*!      Function: lfvData_3
*!
*!**************************************************************************
*
FUNCTION lfvData_3

IF !llBrowse .AND. lcOldCheck = laData[3]
  RETURN
ENDIF  

SELECT APCHECKS             && Select the check file.
lcSavOrder = SET('ORDER')   && Save old order
SET ORDER TO TAG BANKCHECK  && Set order to Check code.

** Validate if browse or not empty of ladata[2]+ladata[3].
IF llBrowse .OR. !SEEK(laData[2]+laData[3]) .OR. ATC("?",laData[3]) > 0

  llBrowse = .F.
  DIMENSION laTemp[2] && We are going to declare an array to hold the browse result.

  laTemp = '' && Filling the array.
   
  lcSavBrFld=lcBrFields   && Save the browse fields in this variable.
  lcSavTitle=lcFile_Ttl   && Save the browse title.

  ** Assign the variable of the browse fields by the new fields.
  lcBrFields="CBNKCODE :H= 'Bank Code',;
              CCHKACCT  :H= 'Check Code'"

  lcFile_Ttl= "Checks" && Assign the title variable with file title.
  lcBank = laData[2]
  ** Call the browse function.
  =gfBrows([FOR CBNKCODE = lcbank],'CBNKCODE,CCHKACCT,CCHKGLACC','laTemp')
    
  lcBrFields=lcSavBrFld && Restore the old fields.
  lcFile_Ttl=lcSavTitle && Restore the old title.
    
  IF !EMPTY(laTemp[2])     && If the user select from the browse.
    laData[3] = laTemp[2]  && Assign the array element with the browse array.
  ELSE
    laData[3] = SPACE(12)  && Clear the array element.
  ENDIF
ENDIF

SHOW GET laData[2]
SHOW GET laData[3]

IF !EMPTY(lcSavOrder)
  SELECT APCHECKS
  SET ORDER TO &lcSavOrder
ENDIF

SELECT APDIV

*!**************************************************************************
*!
*!      Function: lfwAccount
*!
*!**************************************************************************
* This fuction store old accounts values
FUNCTION lfwAccount
PARAMETERS lcOldAcct

lcOldAcc = lcOldAcct

*!**************************************************************************
*!
*!      Function: lfvAccount
*!
*!**************************************************************************
*
FUNCTION lfvAccount
PARAMETERS lnObjNum

lcAccount = SYS(18)

IF EMPTY(STRTRAN(STRTRAN(&lcAccount,'-'),'0'))
  &lcAccount  = lcEmptyAcc
  SHOW GET (lcAccount)
ENDIF

** if mouse clicked or not empty of account. **
IF llBrowse .OR. !EMPTY(STRTRAN(STRTRAN(&lcAccount,'-'),'0'))
  =lfApAcs(.F.,llBrowse) && activate accounts browse.
  llBrowse = .F.  && return false to browse flag.
  IF EMPTY(STRTRAN(STRTRAN(&lcAccount,'-'),'0')) && if empty account.
    DO CASE
      CASE lnObjNum = 1
        laData[4]  = lcOldAcc && restore old account
        SHOW GET laData[4]    
      CASE lnObjNum = 2
        laData[5] = lcOldAcc  && restore old account
        SHOW GET laData[5]
      CASE lnObjNum = 3
        laData[6] = lcOldAcc  && restore old account
        SHOW GET laData[6]
      CASE lnObjNum = 4
        laData[7] = lcOldAcc  && restore old account
        SHOW GET laData[7]
      CASE lnObjNum = 5
        laData[8] = lcOldAcc  && restore old account
        SHOW GET laData[8]
      CASE lnObjNum = 6
        laData[9] = lcOldAcc  && restore old account
        SHOW GET laData[9]
    ENDCASE  
  ENDIF
  =gfUpdate()
ENDIF

SELECT APDIV
=lfRefresh()

*!**************************************************************************
*!
*!      Procedure: lpSavScr
*!
*!**************************************************************************
*  Local save procedure
*
PROCEDURE lpSavScr
FOR lnCount = 4 TO 9
  laData[lnCount] = IIF(EMPTY(STRTRAN(STRTRAN(laData[lnCount],'-'),'0')),;
                        SPACE(lnApsAcLen), laData[lnCount])
ENDFOR

SELECT APDIV

IF laScrMode[4]
  APPEND BLANK
ENDIF

*** Store laData values in the current record
GATHER FROM laData FIELDS &lcScFields