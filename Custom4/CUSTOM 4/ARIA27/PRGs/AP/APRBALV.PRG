*:************************************************************************
*:
*: Procedure file: APRBALV.PRG
*:
*:         System: ARIA ADVANTAGE SERIES
*:         Module: Accounts Payable
*:         Author: Haytham El_Sheltawi
*:      Copyright (c) 
*:  Last modified:  /  /
*:
*:  Procs & Fncts: lfOpenFile()
*:               : lfwOldVal()
*:               : lfvVendCod()
*:               : lfvRBalnce()
*:               : lfvClose()
*:               : lfvUpDate()
*:               : lfvReport()
*:               : lfRbalVend()
*:               : lfErsVBalF()
*:               : 
*:
*:      Documented  02/12/1998
*:************************************************************************
*N100003,1 HS  02/03/1998 This program was coded by HS for this entry
*E301258,1 AKA 06/01/1999 Implement the program in 2.7
*B038431,1 NNA 09/06/2004 Fix bug that when you open any window after Openning [Recalculate Vendor Balance]
*B038431,1 NNA            you'll find that [Recalculate vendor balance] hang up
*:************************************************************************
*

llBrowse = .F.                  && Flag to know if the User selected to Browse [From the Browse button]
laOldVal = ''                   && Variable to hold the Old value
lcVendCode = ''                 && Variable to hold the Vendor code
lcCompany = ''                  && Variable to hold the Vendor company
lcUpDateSt = 'ENABLE'           && Varible to hold the Update and the Report Push Bottons status

*E301258,1 AKA  Start
lcExSin1   = ''                 && Varible to hold the first sign 
lcExSin2   = ''                 && Varible to hold the second sign 
lnInvAmt   = 0                  && Varible to hold the invoice amount
lnDiscTakn = 0                  && Varible to hold the total discount taken per invoice
lnDiscOffr = 0                  && Varible to hold the discount offeramount per invoice
lnAdjtAmt  = 0                  && Varible to hold the total adjustment amount per invoice
lnAmtPaid  = 0                  && Varible to hold the total paid amount per invoice
lnPurchAmt = 0                  && Varible to hold the purchase amount per invoice
lcInvNo    = ''                 && Varible to hold the A/P invoice no.

*DECLARE laUsedFile[2,6] , laRepoFile[2,6] 

DECLARE laUsedFile[2,6] , laRepoFile[2,6]  , laMstrFile[5,6]

*E301258,1 AKA End



**laUsedFile Array to be used by the Function [lfOpenFile] to open Vendor
**rebalance file and Vendor History rebalance file

laUsedFile[1,1] = 'APRBALV'
laUsedFile[1,2] = 'VENCODE'
laUsedFile[1,3] = 'APRBALV'
laUsedFile[1,4] = ''

laUsedFile[2,1] = 'APRBALVH'
laUsedFile[2,2] = 'VENDYEAR'
laUsedFile[2,3] = 'APRBALVH'
laUsedFile[2,4] = ''


**laRepoFile Array to be used by the Function [lfOpenFile] to open the
**needed files for the report

laRepoFile[1,1] = 'APRBALV'
laRepoFile[1,2] = 'VENCODE'
laRepoFile[1,3]  = 'APVENDOR'
laRepoFile[1,4] = ''

laRepoFile[2,1] = 'APRBALVH'
laRepoFile[2,2] = 'VENDYEAR'
laRepoFile[2,3] = 'APVENHST'
laRepoFile[2,4] = ''

*E301258,1 AKA Start
**laMstrFile Array to be used by the Function [lfOpenFile] to open the
**needed master files for process
laMstrFile[1,1] = 'APVENDOR'
laMstrFile[1,2] = 'VENCODE'
laMstrFile[1,3] = 'APVENDOR'
laMstrFile[1,4] = ''

laMstrFile[2,1] = 'FISHD'
laMstrFile[2,2] = 'COMPFYEAR'
laMstrFile[2,3] = 'FISHD'
laMstrFile[2,4] = ''

laMstrFile[3,1] = 'APDIST'
laMstrFile[3,2] = 'INVVEND'
laMstrFile[3,3] = 'APDIST'
laMstrFile[3,4] = ''

laMstrFile[4,1] = 'APINVHDR'
laMstrFile[4,2] = 'VENDINV'
laMstrFile[4,3] = 'APINVHDR'
laMstrFile[4,4] = ''

laMstrFile[5,1] = 'APVENHST'
laMstrFile[5,2] = 'VENDYEAR'
laMstrFile[5,3] = 'APVENHST'
laMstrFile[5,4] = ''
*E301258,1 AKA End



*If The Function [lfOpenFile] was not able to open the Vendor rebalance
*file and Vendor History rebalance file (For 1 of the files [or both]
*do not exist)


IF lfOpenFile(.T. , 'laUsedFile') < 2
  lcUpDateSt = 'DISABLE'
  =lfOpenFile(.F. , 'laUsedFile')
  =lfErsVBalF()
ENDIF    && End of IF The Function [lfOpenFile] was not able to open the Vendor rebalance file




IF !gfSetup()
  RETURN
ENDIF

*E301258,1 AKA Start 
=lfOpenFile(.T. , 'laMstrFile')
*E301258,1 AKA End



*E301258,1 AKA End
*E301258,1 *** Change way of the program calling 
*DO APRBALV.SPR
DO (gcScrDir + gcWinAppl + "\APRBALV.SPR")
*E301258,1 AKA End


=lfOpenFile(.F. , 'laUsedFile')

*E301258,1 AKA Start
*E301258,1 Close master files
=lfOpenFile(.F. , 'laMstrFile')
*E301258,1 AKA End

*B038431,1 NNA 09/06/2004 (Begin) Stop this Command because it Erase Window if we are in a multi Session
*glQuitting = .T.
*B038431,1 NNA (End)

*!*************************************************************
*! Name      : lfOpenFile
*! Developer : Haytham El_Sheltawi
*! Date      : 02/10/1998
*! Purpose   : Function to open and close the needed files
*!*************************************************************
*! Called from : APRBALV.PRG , lfvReport()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 1) .T. [To open the files] ,
*!                        .F. [To close the files]
*!                     2) The name of the Array that holds the
*!                        files to be opened
*!*************************************************************
*! Return      : The number of files that was opened.
*!*************************************************************
*
FUNCTION lfOpenFile

PARAMETERS llOpen , lcArray

PRIVATE lnCount , lnOpened , lcDataDir , lcPathStat

lcPathStat = SET('FULLPATH ')     && Varible to save the SET FULLPATH status
SET FULLPATH ON
lnOpened = 0                      && Variable to hold the number of files that was opened

*If The function is to open the files
IF llOpen
  
  *FOR Loop to loop the Array (lcArray)
  FOR lnCount = 1 TO ALEN(&lcArray , 1)
    
    lcDataDir = IIF('\' $ &lcArray.[lnCount , 1] , '' , gcDataDir)    && Variable to hold the file path
    
    *If The file to be Opened exist
    IF FILE(lcDataDir + &lcArray.[lnCount , 1] + '.DBF')
    
      lnOpened = lnOpened + 1
      &lcArray.[lnCount , 6] = .T.
      
      *If The file is not open
      IF !USED(&lcArray.[lnCount , 3])
        SELECT 0
        USE (lcDataDir + &lcArray.[lnCount , 1]) AGAIN ALIAS (&lcArray.[lnCount , 3])
        
        *If The file is to be opened with an Index Tag
        IF !EMPTY(&lcArray.[lnCount , 2])
          SET ORDER TO TAG (&lcArray.[lnCount , 2])
        ENDIF    && End of IF The file is to be opened with an Index Tag
        
        &lcArray.[lnCount , 5] = .T.
        
      ELSE    && Else [IF The file is open]
        
        SELECT (&lcArray.[lnCount , 3])
        
        *If we are going to open another file with this Alias
        IF &lcArray.[lnCount , 1] <> &lcArray.[lnCount , 3]
          &lcArray.[lnCount , 4] = DBF()
          USE (lcDataDir + &lcArray.[lnCount , 1]) AGAIN ALIAS (&lcArray.[lnCount , 3])
        ENDIF    && End of IF we are going to open another file with this Alias
        
        *If The file is to be opened with an Index Tag
        IF !EMPTY(&lcArray.[lnCount , 2])
          SET ORDER TO TAG (&lcArray.[lnCount , 2])
        ELSE    && Else [IF The file is to be opened with no Index Tag active]
          SET ORDER TO
        ENDIF    && End of IF The file is to be opened with an Index Tag
        
        SET FILTER TO 
        &lcArray.[lnCount , 5] = .F.
      ENDIF    && End of IF The file is not open
      GO TOP
    
    ENDIF    && End of IF The file to be Opened exist
  ENDFOR    && End of FOR Loop to loop the Array (lcArray)

ELSE    && Else [IF The function is to close the files]

  *FOR Loop to loop the array (lcArray)
  FOR lnCount = 1 TO ALEN(&lcArray , 1)
    
    *If The file exist
    IF &lcArray.[lnCount , 6] = .T.
    
      *If The file was not open befor
      IF &lcArray.[lnCount , 5]
        USE IN (&lcArray.[lnCount , 3])
      ELSE    && Else [IF The file was open befor]
        
        *If The file was opened with the Alias of another file
        IF &lcArray.[lnCount , 1] <> &lcArray.[lnCount , 3]
          USE (&lcArray.[lnCount , 4]) AGAIN IN (&lcArray.[lnCount , 3])
        ENDIF    && End of IF The file was opened with the Alias of another file
      ENDIF    && End of IF The file was not open befor
      
      &lcArray.[lnCount , 4] = ''
      &lcArray.[lnCount , 6] = .F.
    
    ENDIF    && End of IF The file exist
  ENDFOR    && End of FOR Loop to loop the array (lcArray)
ENDIF    && End of IF The function is to open the files

SET FULLPATH &lcPathStat

RETURN lnOpened

*!*************************************************************
*! Name      : lfwOldVal
*! Developer : Haytham El_Sheltawi
*! Date      : 02/03/1998
*! Purpose   : When function to get the Old value
*!*************************************************************
*! Called from : The Vendor code Get field
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfwOldVal

laOldVal = EVALUATE(SYS(18))      && Varible to hold the old value

*!*************************************************************
*! Name      : lfvVendCod
*! Developer : Haytham El_Sheltawi
*! Date      : 02/03/1998
*! Purpose   : Valid function for the Get field Vendor code
*!*************************************************************
*! Called from : Screen APRBALV
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvVendCod

SELECT APVENDOR

*If The user selected to browse or if the Vendor code he entered is
*not in the file
IF llBrowse .OR. (!EMPTY(lcVendCode) .AND. ;
   (ATC("?" , lcVendCode) > 0 .OR. !SEEK(lcVendCode)))
  
  DIMENSION laTemp[2]
  laTemp = ''                      && Array to hold to hold the Vendor code and Company [That the user selected from the browse]
  lcSavBrFld = lcBrFields          && Variable to save the old browse fields
  lcSavTitle = lcFile_Ttl          && Variable to save the old browse title
  
  lcBrFields = "CVENDCODE :H= 'Vendor Code',;
                CVENCOMP :H= 'Company',;
                CPHONENO :H= 'Phone'"
  
  lcFile_Ttl = "Vendor"
  
  =gfBrows(.F. , 'CVENDCODE , CVENCOMP' , 'laTemp')
  
  lcBrFields = lcSavBrFld          && Restore the old browse fields
  lcFile_Ttl = lcSavTitle          && Restore the old browse title
  
  *If The user has selected a vendor from the browse
  IF !EMPTY(laTemp[1])
    lcVendCode = laTemp[1]
    lcCompany = laTemp[2]
    =lfRefresh()
  ELSE    && Else [IF The user did not select a vendor from the browse]
    lcVendCode = laOldVal              && Restore the old value
  ENDIF    && End of IF The user has selected a vendor from the browse
ELSE    && Else [IF The Vendor code is Empty or Valid]
  lcCompany = IIF(EMPTY(lcVendCode) , '' , cVenComp)
  =lfRefresh()
ENDIF    && End of IF The user selected to browse or if the Vendor code he entered is not in the file

llBrowse = .F.

*!*************************************************************
*! Name      : lfvRBalnce
*! Developer : Haytham El_Sheltawi
*! Date      : 02/03/1998
*! Purpose   : Valid function for the Push Button Rebalance
*!*************************************************************
*! Called from : Screen APRBALV
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvRBalnce

PRIVATE laStructur , lnCount

*If The vendor code get field is empty give the user a message to confirm
*that he is going to recalculate the balances for all vendors
***   Message    : 'Recalculate balances for all vendors.        '
***   Buttons    : '         <   Yes   >       <    No   >       '

*E301258,1 AKA Start 
* Assign new message no. in Aria 2.7 instead of message no. in Adv1 
*IF EMPTY(lcVendCode) .AND. gfModalGen('QRM04171B00006' , 'Dialog') = 2
*  RETURN
*ENDIF    && End of IF The vendor code get field is empty

IF EMPTY(lcVendCode) .AND. gfModalGen('QRM04189B00006' , 'Dialog') = 2
  RETURN
ENDIF    && End of IF The vendor code get field is empty
*E301258,1 AKA End


DECLARE laStructur[1,4]

*If The Vendor code get field is not empty and the there is one or more
*vendor with recalculated balances that is not updated
IF !EMPTY(lcVendCode) .AND. USED('APRBALV')
  
  *E301258,1 AKA Start   
  * Assign new message no. in Aria 2.7 instead of message no. in Adv1   
  ***   Message    : 'One or more vendor(s) balances have not been updated.'
  ***   Buttons    : '     < Overwrite >   <  Append  >   <  Cancel  >     '
  *lnUserSel = gfModalGen('QRM04173B00010' , 'Dialog')
  lnUserSel  = gfModalGen('QRM04190B00010' , 'Dialog')
  *E301258,1 AKA End 
  *If Overwrite
  IF lnUserSel = 1
    =lfErsVBalF()
  ELSE    && Else [IF Append or Cancel]
    
    *If Cancel
    IF lnUserSel = 3
      RETURN
    ENDIF    && End of IF Cancel
  ENDIF    && End of IF Overwrite
ENDIF    && End of IF The Vendor code get field is not empty and the there is one or more vendor with recalculated balances that is not updated

*If The Vendor rebalance file is not opened
IF !USED('APRBALV')
  SELECT APVENDOR
  =AFIELDS(laStructur)
  CREATE TABLE (gcDataDir + 'APRBALV') FROM ARRAY laStructur
  INDEX ON cVendCode TAG VENCODE
  laUsedFile[1,5] = .T.
  laUsedFile[1,6] = .T.
ENDIF    && End of IF The Vendor rebalance file is not opened

*If The Vendor History rebalance file is not opened
IF !USED('APRBALVH')
  SELECT APVENHST
  =AFIELDS(laStructur)
  CREATE TABLE (gcDataDir + 'APRBALVH') FROM ARRAY laStructur
  INDEX ON cVendCode + cFisFYear TAG VENDYEAR
  laUsedFile[2,5] = .T.
  laUsedFile[2,6] = .T.
ENDIF    && End of IF The Vendor History rebalance file is not opened

*Set the Order Tags for all needed files
SET ORDER TO TAG VENCODE  IN APRBALV
SET ORDER TO TAG VENDYEAR IN APRBALVH
SET ORDER TO TAG VENCODE  IN APVENDOR
SET ORDER TO TAG VENDYEAR IN APVENHST
SET ORDER TO TAG VENDINV  IN APINVHDR
SET ORDER TO TAG INVVEND  IN APDIST

***********************************
*E301258,1 AKA Start 
*E301258,1 *** After having each company it's own chart of account and data file,
*E301258,1 *** so it's important to open the files from data directory instead from system directory.
*E301258,1 *** Note :  Files in version 2.6 changed it's name in version 2.7
*E301258,1 ***         Old File Name ( Version 2.6)      New File Name (Version 2.7)
*E301258,1 ***         ..\sysfiles\SYCFISHD              ..\gcDataDir\FISHD
*E301258,1 ***         ..\sysfiles\SYCFSPRD              ..\gcDataDir\FSPRD
*SET ORDER TO TAG COMPFYEAR IN SYCFISHD
*E301258,1 AKA End

SELECT APVENDOR

*If We are going to recalculate the balances for all Vendors
IF EMPTY(lcVendCode)
  lnTotal = RECCOUNT()              && These variable is for the thermometer
  lnCurrent = 0                     && These variable is for the thermometer
  
  *SCAN Loop to scan the Vendor file
  SCAN
    =lfRbalVend()

    lnCurrent = lnCurrent  + 1
    =gfThermo(lnTotal , lnCurrent , 'Recalculating balances...' ,;
              'Vendor : ' + ALLTRIM(APVENDOR.cVendCode))
  
    SELECT APVENDOR
  ENDSCAN    && End of SCAN Loop to scan the Vendor file
  
  *If The thermometer is not finished
  IF lnCurrent < lnTotal
    
    *FOR Loop to finish the thermometer
    FOR lnCount = lnCurrent TO lnTotal
      =gfThermo(lnTotal , lnCount , 'Recalculating balances...')
    ENDFOR    && End of FOR Loop to finish the thermometer
  ENDIF    && End of IF The thermometer is not finished
ELSE    && Else [IF We are going to recalculate the balances for one Vendor]
  
  *If Statment to replace the record pointer in the correct record in
  *the Vendor file
  IF SEEK(lcVendCode)
    =lfRbalVend()
  ENDIF    && End of IF Statment to replace the record pointer in the correct record
ENDIF    && End of IF We are going to recalculate the balances for all Vendors

lcUpDateSt = 'ENABLE'
SHOW GET pbReport &lcUpDateSt
SHOW GET pbUpDate &lcUpDateSt

*!*************************************************************
*! Name      : lfvClose
*! Developer : Haytham El_Sheltawi
*! Date      : 02/03/1998
*! Purpose   : Valid function for the Push Button Close
*!*************************************************************
*! Called from : Screen APRBALV
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvClose

CLEAR READ

*!*************************************************************
*! Name      : lfvUpDate
*! Developer : Haytham El_Sheltawi
*! Date      : 02/03/1998
*! Purpose   : Valid function for the Push Button Update
*!*************************************************************
*! Called from : Screen APRBALV
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvUpDate

PRIVATE llNotUpDat , lcVendor , lnCount

*Set the Order Tags for all needed files
SET ORDER TO TAG VENCODE IN APRBALV
SET ORDER TO TAG VENDYEAR IN APRBALVH
SET ORDER TO TAG VENCODE IN APVENDOR
SET ORDER TO TAG VENDYEAR IN APVENHST
SET ORDER TO TAG VENDINV IN APINVHDR
SET ORDER TO TAG INVVEND IN APDIST


*E301258,1 AKA Start
*E301258,1 *** After having each company it's own chart of account and data file,
*E301258,1 *** so it's important to change the system directory to be data directory.
*E301258,1 *** Note :  Files in version 2.6 changed it's name in version 2.7
*E301258,1 ***         Old File Name ( Version 2.6)      New File Name (Version 2.7)
*E301258,1 ***         ..\sysfiles\SYCFISHD              ..\gcDataDir\FISHD
*E301258,1 ***         ..\sysfiles\SYCFSPRD              ..\gcDataDir\FSPRD

*SELECT SYCFISHD
*SET ORDER TO TAG COMPFYEAR
*COUNT FOR cComp_Id + cFisFYear = gcAct_Comp TO lnFisYerNo    && Variable to hold the number of fiscal years for the current company
*** To get the number of records in calender year table withoud deleted records
SELECT FISHD
COUNT TO lnFisYerNo FOR !DELETED()  &&  Get no. of calender years
*E301258,1 AKA End

llNotUpDat = .F.                             && Flag to know if one or more Vendors was not updated
SELECT APRBALV
lnTotal = RECCOUNT() * lnFisYerNo            && These variable is for the Thermometer
lnCurrent = 0                                && These variable is for the Thermometer

*SCAN Loop to scan the Vendor rebalance file
SCAN
  *If The Vendor time stamp is not changed
  IF SEEK(cVendCode , 'APVENDOR') .AND. ;
     (dAdd_Date = APVENDOR.dAdd_Date .AND. cAdd_Time = APVENDOR.cAdd_Time)

    SELECT APVENHST
    
    *If Statment to replace the record pointer in the correct record in
    *the Vendor History file
    *IF SEEK(APVENDOR.cVendCode)
      
      *SCAN Loop to scan the Vendor records in the Vendor History file
      SCAN FOR cVendCode + cFisFYear = APVENDOR.cVendCode
      
        BLANK
        DELETE
      ENDSCAN    && End of SCAN Loop to scan the Vendor records in the Vendor History file
    *ENDIF    && End of IF Statment to replace the record pointer in the correct record
    
    SET DELETED OFF
    
    *E301258,1 AKA Start
    *SELECT SYCFISHD
    *=SEEK (gcAct_Comp)
    SELECT FISHD 
    GO TOP
    *E301258,1 AKA End
    
    
    *SCAN Loop to scan the Fiscal calendar for this company
    *[!DELETED() For we have set the DELETED Off]

    *E301258,1 AKA Start
    *SCAN REST ;
        WHILE cComp_Id = gcAct_Comp ;
          FOR !DELETED()
    SCAN FOR !DELETED()
    *E301258,1 AKA End
      
      SELECT APRBALVH
      *If There is a record for this Fiscal Year and Vendor in the
      *Vendor History rebalance file
      
      *E301258,1 AKA Start
      *IF SEEK(APVENDOR.cVendCode + SYCFISHD.cFisFYear)
      IF SEEK(APVENDOR.cVendCode + FISHD.cFisFYear)
      *E301258,1 AKA End
      
        SCATTER MEMVAR MEMO
      ELSE    && Else [IF There is no records for this Fiscal Year and Vendor]
        SCATTER MEMVAR MEMO BLANK
        M.cVendCode = APVENDOR.cVendCode
        
        *E301258,1 AKA Start
        *M.cFisFYear = SYCFISHD.cFisFYear
         M.cFisFYear = FISHD.cFisFYear
        *E301258,1 AKA End
        
      ENDIF    && End of IF There is a record for this Fiscal Year and Vendor
      
      SELECT APVENHST
      
      *If There is a deleted record in the Vendor History file
      IF SEEK(SPACE(12))
        RECALL
      ELSE    && Else [IF There is no deleted record in the Vendor History file]
        APPEND BLANK
      ENDIF    && End of IF There is a deleted record in the Vendor History file
      
      GATHER MEMVAR MEMO
      =gfAdd_Info('APVENHST')
      
      lnCurrent = lnCurrent  + 1
      =gfThermo(lnTotal , lnCurrent , 'Updating balances...' ,;
                'Vendor : ' + ALLTRIM(APRBALV.cVendCode))
      
      *E301258,1 AKA Start
      *SELECT SYCFISHD
      SELECT FISHD
      *E301258,1 AKA End
      
      
    ENDSCAN    && End of SCAN Loop to scan the Fiscal calendar for this company
    
    SET DELETED ON
    
    SELECT APRBALV
    SCATTER MEMVAR MEMO
    SELECT APVENDOR
    GATHER MEMVAR MEMO
    =gfAdd_Info('APVENDOR')
    SELECT APRBALV
    DELETE
  ELSE    && Else [IF The Vendor time stamp was changed]
    llNotUpDat = .T.
    lcVendor = ALLTRIM(APVENDOR.cVendCode)          && Variable to hold the current Vendor code
  ENDIF    && End of IF The Vendor time stamp is not changed
  SELECT APRBALV
ENDSCAN    && End of SCAN Loop to scan the Vendor rebalance file

*If The thermometer is not finished
IF lnCurrent < lnTotal
  
  *FOR Loop to finish the thermometer
  FOR lnCount = lnCurrent TO lnTotal
    =gfThermo(lnTotal , lnCount , 'Updating balances...')
  ENDFOR    && End of FOR Loop to finish the thermometer
ENDIF    && End of IF The thermometer is not finished

*E301258,1 AKA Start   
* Assign new message no. in Aria 2.7 instead of message no. in Adv1 
*If There is one or more Vendors that was not updated
*IF llNotUpDat
*  =gfModalGen('QRM04172B00000' , 'Dialog' , IIF(RECCOUNT('APRBALV') > 1 ,;
*              'One or more vendor(s)' , 'Vendor ' + lcVendor))
*ENDIF    && End of IF There is one or more Vendors that was not updated
IF llNotUpDat
  =gfModalGen('QRM04191B00000' , 'Dialog' , IIF(RECCOUNT('APRBALV') > 1 ,;
              'One or more vendor(s)' , 'Vendor ' + lcVendor))
ENDIF    && End of IF There is one or more Vendors that was not updated
*E301258,1 AKA End


=lfErsVBalF()
lcUpDateSt = 'DISABLE'
SHOW GET pbReport &lcUpDateSt
SHOW GET pbUpDate &lcUpDateSt

*!*************************************************************
*! Name      : lfvReport
*! Developer : Haytham El_Sheltawi
*! Date      : 02/03/1998
*! Purpose   : Valid function for the Push Button Report
*!*************************************************************
*! Called from : Screen APRBALV
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvReport

*E301258,1 AKA Start
* Close master files b4 running the report
=lfOpenFile(.F. , 'laMstrFile')
*E301258,1 AKA End

*Open the needed files for the report
=lfOpenFile(.T. , 'laRepoFile')

DO (gcRepHome + 'APREPORT.APP') WITH 'APVNHSTS'

*Close the report files
=lfOpenFile(.F. , 'laRepoFile')

*E301258,1 AKA Start 
=lfOpenFile(.T. , 'laMstrFile')
*E301258,1 AKA End
*!*************************************************************
*! Name      : lfRbalVend
*! Developer : Haytham El_Sheltawi
*! Date      : 02/10/1998
*! Purpose   : Function to recalculate the Vendor blances
*!*************************************************************
*! Called from : lfvRBalnce()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfRbalVend

PRIVATE lcPeriod , lcField

* Initialize OpenDebit variables
lnInvAmnt = 0       
lnInvPaid = 0 
lnInvDisTk= 0 
lnInvAdj  = 0 


SCATTER MEMVAR MEMO
M.NVENBAL = 0
M.NVENCPUR = 0
M.NVENCPAY = 0
M.NVENOPNDR = 0

SELECT APRBALV

*If The Vendor code dose not exist in the Vendor rebalance file
IF !SEEK(M.cVendCode)
  APPEND BLANK
ENDIF    && End of IF The Vendor code dose not exist in the Vendor rebalance file
GATHER MEMVAR MEMO

SELECT APVENHST
SCATTER MEMVAR MEMO BLANK
M.cVendCode = APVENDOR.cVendCode

*E301258,1 AKA Start 
*SELECT SYCFISHD
*=SEEK(gcAct_Comp)
SELECT FISHD
GO TOP
*E301258,1 AKA End



*SCAN Loop to scan the Fiscal Calendar file for the current company

*E301258,1 AKA Start 
*SCAN REST ;
    WHILE cComp_Id = gcAct_Comp

SCAN 
*E301258,1 AKA End

  M.cFisFYear = cFisFYear
  SELECT APRBALVH
  
  *If The Vendor code and Fiscal year dose not exist in the Vendor History
  *rebalance file
  IF !SEEK(M.cVendCode + M.cFisFYear)
    APPEND BLANK
  ENDIF    && End of IF The Vendor code and Fiscal year dose not exist
  GATHER MEMVAR MEMO
  
  *E301258,1 AKA Start 
  *SELECT SYCFISHD
  SELECT FISHD
  *E301258,1 AKA End
  
ENDSCAN    && End of SCAN Loop to scan the Fiscal Calendar file for the current company

SELECT APINVHDR

*If Statment to replace the record pointer in the first record for this
*Vendor in the Invoice Header file
IF SEEK(M.cVendCode)
  
  *SCAN Loop To scan the Vendor invoices in the Invoice Header file
  *with status <> Void
  SCAN REST;
      WHILE cVendCode = M.cVendCode;
        FOR cInvStat <> 'V'
    
    *E301258,1 AKA Start 
    lnDiscOffr = lfInvInfo(APINVHDR.cInvNo , @lnInvAmt , @lnDiscTakn , lnDiscOffr , @lnAdjtAmt  , @lnAmtPaid , @lnPurchAmt)  
    
    *E301258,1 AKA End

    
    *If Not af advance payment, update cumulative purchases
    IF APINVHDR.cInvStat <> 'A'
      *Update Vendor file
      SELECT APRBALV

      *E301258,1 AKA Start 
      *REPLACE nVenCPur WITH nVenCPur + APINVHDR.nInvAmnt
      REPLACE nVenCPur WITH nVenCPur + lnInvAmt
      *E301258,1 AKA End
      
      *Update Vendor History file for APINVHDR.cFisfYear + APINVHDR.cFspPrdId
      SELECT APRBALVH
      
      *If Statment to replace the record pointer in the correct record for
      *this Vendor and Fiscal Year in the Vendor History file
      IF SEEK(M.cVendCode + APINVHDR.cFisfYear)
        lcPeriod = ALLTRIM(STR(VAL(APINVHDR.cFspPrdId)))
        *E301258,1 AKA Start 
        *REPLACE nVnhPurch        WITH nVnhPurch + APINVHDR.nInvAmnt,;
        *        nVnhPur&lcPeriod WITH nVnhPur&lcPeriod + APINVHDR.nInvAmnt

        REPLACE nVnhPurch        WITH nVnhPurch + lnPurchAmt ,;
                nVnhPur&lcPeriod WITH nVnhPur&lcPeriod + lnPurchAmt 
        *E301258,1 AKA End             
        
      ENDIF    && End of IF Statment to replace the record pointer in the correct record for this Vendor and Fiscal Year in the Vendor History file
  
    ELSE
      
      *E301258,1 AKA Start
      * Calculate Open debit 
      lnInvAmnt = 0       
      lnInvPaid = 0 
      lnInvDisTk= 0 
      lnInvAdj  = 0 
      lcExSin2   = ''
      lcExSin1   = gfGetExSin(@lcExSin2, APINVHDR.cCurrCode ) 
      lnInvAmnt = lnInvAmnt + ROUND(APINVHDR.nInvAmnt   &lcExSin1 APINVHDR.nExRate &lcExSin2 APINVHDR.nCurrUnit ,2)
      lnInvPaid = lnInvPaid + ROUND(APINVHDR.nInvPaid   &lcExSin1 APINVHDR.nExRate &lcExSin2 APINVHDR.nCurrUnit ,2)
      lnInvDisTk= lnInvDisTk+ ROUND(APINVHDR.nInvDisTk  &lcExSin1 APINVHDR.nExRate &lcExSin2 APINVHDR.nCurrUnit ,2)      
      lnInvAdj  = lnInvAdj  + ROUND(APINVHDR.nInvAdj    &lcExSin1 APINVHDR.nExRate &lcExSin2 APINVHDR.nCurrUnit ,2)            
      *E301258,1 AKA End              
      
    ENDIF    && End of IF Not an advance payment, update cumulative purchases
  
    *Update vendor balances if the invoice is not a credit card payment
    *If Not credit card payment
    IF APINVHDR.cVenPMeth <> 'C'       
      
      *Update Vendor file
      SELECT APRBALV
      *E301258,1 AKA Start 
      *REPLACE nVenBal WITH nVenBal + APINVHDR.nInvAmnt - APINVHDR.nInvPaid ;
      *                             - APINVHDR.nInvDisTk - APINVHDR.nInvAdj 


      REPLACE nVenBal WITH nVenBal + lnInvAmt - lnAmtPaid;
                                   - lnDiscTakn - lnAdjtAmt 
      *E301258,1 AKA End
      
      *Update Vendor History file
      SELECT APRBALVH
      *If Statment to replace the record pointer in the correct record for
      *this Vendor and Fiscal Year in the Vendor History file
      

      IF SEEK(M.cVendCode + APINVHDR.cFisfYear)
        *E301258,1 AKA Start 
        *REPLACE nVnhDisOf  WITH nVnhDisOf + APINVHDR.nInvDisOf ,;
                nVnhDisTkn WITH nVnhDisTkn + APINVHDR.nInvDistK ,;
                nVnhAdj    WITH nVnhAdj + APINVHDR.nInvAdj

        REPLACE nVnhDisOf  WITH nVnhDisOf +  lnDiscOffr ,;
                nVnhDisTkn WITH nVnhDisTkn + lnDiscTakn ,;
                nVnhAdj    WITH nVnhAdj + lnAdjtAmt


                

        *E301258,1 AKA End
      ENDIF    && End of IF Statment to replace the record pointer in the correct record for this Vendor and Fiscal Year in the Vendor History file
      
      
      *Update payments from APDIST (including advance payments)
      SELECT APDIST
      =SEEK(APINVHDR.cInvNo + M.cVendCode)
      
      *SCAN Loop To scan the Vendor invoices lines in the Dist. file
      *with status <> Void
      SCAN REST;
          WHILE cInvNo + cVendCode = APINVHDR.cInvNo + M.cVendCode ;
            FOR cAPdStat<> 'V'
        *If the record is a payment record (cApdActId = 'C') 
        IF cApdActId = 'C'
          *Update Vendor file
          SELECT APRBALV
          *E301258,1 AKA Start 
          *REPLACE nVenCPay WITH nVenCPay - APDIST.nApdAmnt
          REPLACE nVenCPay WITH nVenCPay -  APDIST.nEqvAmnt 
          *E301258,1 AKA End 
          
          *Update Vendor History file for APDIST.cFisfYear + APDIST.cFspPrdId
          SELECT APRBALVH
          
          *If Statment to replace the record pointer in the correct record
          *for this Vendor and Fiscal Year in the Vendor History file
          IF SEEK(M.cVendCode + APINVHDR.cFisfYear)
            lcPeriod = ALLTRIM(STR(VAL(APDIST.cFspPrdId)))
            lcField = IIF(APDIST.cApdTrTyp = 'P' , 'nVnhPChkP' ,;
                      IIF(APDIST.cApdTrTyp = 'M' , 'nVnhMChkP' ,;
                      IIF(APDIST.cApdTrTyp = 'N' , 'nVnhNChkP' ,;
                          'nVnhCashP' )))
            *E301258,1 AKA Start 
            *REPLACE nVnhTotPa        WITH nVnhTotPa - APDIST.nApdAmnt,;
            *       &lcField         WITH &lcField   - APDIST.nApdAmnt,;
            *        nVnhPay&lcPeriod WITH nVnhPay&lcPeriod - APDIST.nApdAmnt
           

           
            REPLACE nVnhTotPa        WITH nVnhTotPa - APDIST.nEqvAmnt  ,;
                    &lcField         WITH &lcField  - APDIST.nEqvAmnt ,;
                    nVnhPay&lcPeriod WITH nVnhPay&lcPeriod -  APDIST.nEqvAmnt 
                    
            *E301258,1 AKA End 
          ENDIF    && End of IF Statment to replace the record pointer in the correct record for this Vendor and Fiscal Year in the Vendor History file
        ENDIF    && End IF cApdActId = 'C'
        
        *If the record is an application record (applied bdebit memo)
        IF APDIST.cApdTrTyp = 'A' .AND. cApdActID = 'A' .AND. nApdAmnt < 0
          
          *Update Vendor History file for APDIST.cFisfYear
          SELECT APRBALVH
          
          *If Statment to replace the record pointer in the correct record
          *for this Vendor and Fiscal Year in the Vendor History file
          IF SEEK(M.cVendCode + APINVHDR.cFisfYear)
            *E301258,1 AKA End
            *REPLACE nVnhDMApP WITH nVnhDMApP +  APDIST.nApdAmnt ,;
            *        nVnHAdj   WITH nVnHAdj   - APDIST.nApdAmnt  
          
            REPLACE nVnhDMApP WITH nVnhDMApP + - APDIST.nEqvAmnt ,;
                    nVnHAdj   WITH nVnHAdj   +  lnAdjtAmt 
                    
            *E301258,1 AKA End 
          ENDIF    && End of IF Statment to replace the record pointer in the correct record for this Vendor and Fiscal Year in the Vendor History file
        ENDIF    && End IF APDIST.cApdTrTyp = 'A' ,,,
        
        SELECT APDIST
      ENDSCAN    && End of SCAN Loop To scan the Vendor invoices lines in the Dist. file with status <> Void
    
    ELSE      && IF Credit card payment, update payments
    
      *Update Vendor file
      SELECT APRBALV
      *E301258,1 AKA Start 
      *REPLACE nVenCPay WITH nVenCPay + APINVHDR.nInvAmnt
      REPLACE nVenCPay WITH nVenCPay + lnInvAmt 
      *E301258,1 AKA End 
  
      
      *Update Vendor History file
      SELECT APRBALVH
      
      *If Statment to replace the record pointer in the correct record
      *for this Vendor and Fiscal Year in the Vendor History file
      IF SEEK(M.cVendCode + APINVHDR.cFisfYear)
        lcPeriod = ALLTRIM(STR(VAL(APINVHDR.cFspPrdId)))
        *E301258,1 AKA Start 
        *REPLACE nVnhTotPa        WITH nVnhTotPa + APINVHDR.nInvAmnt ,;
        *        nVnhCCPay        WITH nVnhCCPay + APINVHDR.nInvAmnt ,;
        *        nVnhPay&lcPeriod WITH nVnhPay&lcPeriod + APINVHDR.nInvAmnt
        
        REPLACE nVnhTotPa        WITH nVnhTotPa + lnInvAmt ,;
                nVnhCCPay        WITH nVnhCCPay + lnInvAmt ,;
                nVnhPay&lcPeriod WITH nVnhPay&lcPeriod + lnInvAmt
        *E301258,1 AKA End 
      ENDIF    && End of IF Statment to replace the record pointer in the correct record for this Vendor and Fiscal Year in the Vendor History file
    ENDIF    && End of IF Not credit card payment
    
    *Update Vendor open debits (for advance payments and debit memos)
    *If Advance payment or Debit memo
    *E301258,1 AKA Start 
    *IF APINVHDR.nInvAmnt < 0
      *Update Vendor file
      *SELECT APRBALV
      *REPLACE nVenOpnDR WITH nVenOpnDR - APINVHDR.nInvAmnt+;
      *                       APINVHDR.nInvPaid + APINVHDR.nInvDisTk +;
      *                       APINVHDR.nInvAdj
    *ENDIF    && End of IF Advance payment or Debit memo
     *E301258,1 AKA (End)
    SELECT APINVHDR
  ENDSCAN    && End of SCAN Loop To scan the Vendor invoices in the Invoice Header file
  
ENDIF    && End of IF Statment to replace the record pointer in the first record for this Vendor in the Invoice Header file


*E301258,1  (Start)
** Update OpenDebit value in Vendor Master
SELECT APRBALV      

IF SEEK(M.cVendCode)
   REPLACE nVenOpnDR WITH -lnInvAmnt  + lnInvPaid + ;
                          lnInvDisTk + lnInvAdj  
ENDIF  
* Initialize OpenDebit variables
lnInvAmnt = 0       
lnInvPaid = 0 
lnInvDisTk= 0 
lnInvAdj  = 0 

SELECT APINVHDR
*E301258,1 AKA End 



*!*************************************************************
*! Name      : lfErsVBalF
*! Developer : Haytham El_Sheltawi
*! Date      : 02/11/1998
*! Purpose   : Function to Erases the Vendor rebalance file
*!             and the Vendor History rebalance file
*!*************************************************************
*! Called from : lfvRBalnce()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfErsVBalF


laUsedFile[1,4] = ''
laUsedFile[1,6] = .F.
laUsedFile[2,4] = ''
laUsedFile[2,6] = .F.

*If The Vendor rebalance file is open
IF USED('APRBALV')
  USE IN APRBALV
ENDIF    && End of IF The Vendor rebalance file is open

*If The APRBALV.DBF file exist
IF FILE(gcDataDir + 'APRBALV.DBF')
  ERASE (gcDataDir + 'APRBALV.DBF')
ENDIF    && End of IF The APRBALV.DBF file exist

*If The APRBALV.FPT file exist
IF FILE(gcDataDir + 'APRBALV.FPT')
  ERASE (gcDataDir + 'APRBALV.FPT')
ENDIF    && End of IF The APRBALV.FPT file exist

*If The APRBALV.CDX file exist
IF FILE(gcDataDir + 'APRBALV.CDX')
  ERASE (gcDataDir + 'APRBALV.CDX')
ENDIF    && End of IF The APRBALV.CDX file exist


*If The Vendor History rebalance file is open
IF USED('APRBALVH')
  USE IN APRBALVH
ENDIF    && End of IF The Vendor History rebalance file is open

*If The APRBALVH.DBF file exist
IF FILE(gcDataDir + 'APRBALVH.DBF')
  ERASE (gcDataDir + 'APRBALVH.DBF')
ENDIF    && End of IF The APRBALVH.DBF file exist

*If The APRBALVH.FPT file exist
IF FILE(gcDataDir + 'APRBALVH.FPT')
  ERASE (gcDataDir + 'APRBALVH.FPT')
ENDIF    && End of IF The APRBALVH.FPT file exist

*If The APRBALVH.CDX file exist
IF FILE(gcDataDir + 'APRBALVH.CDX')
  ERASE (gcDataDir + 'APRBALVH.CDX')
ENDIF    && End of IF The APRBALVH.CDX file exist



*!*************************************************************
*! Name      : lfInvInfo
*! Developer : Amin Khodary 
*! Date      : 06/01/1999
*! Purpose   : Function to calculate and return the values of A/P invoice
*!             amount , total discount taken , total adj. amount , discount offer
*!             as well as the amount paid from invoice distribution file.
*!*************************************************************
*! Called from : lfRbalVend()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 1 ) lcInvNo    
*!                     2 ) lnInvAmt   
*!					   3 ) lnDiscTakn 
*!					   4 ) lnDiscOffr 
*!					   5 ) lnAdjtAmt  
*!                     6 ) lnAmtPaid
*!*************************************************************
*! Return      : 1 ) lnInvAmt holds invoice amount. 
*!               2 ) lnDiscTakn holds total discount taken.
*!               3 ) lnDiscOffr holds discount offer amount.
*!               4 ) lnAdjtAmt holds total adj. amount.  
*!               5 ) lnAmtPaid holds total paid amount. 
*!               6 ) lnPurchAmt holds purchase amount per invoiuce. 
*!*************************************************************
FUNCTION lfInvInfo
PARAMETERS  lcInvNo , lnInvAmt , lnDiscTakn , lnDiscOffr , lnAdjtAmt  , lnAmtPaid  , lnPurchAmt 

*Save the current area
lcCurArea  = SELECT()          && Varible to save the number of the current work area

* Select the A/P invoice distribution
SELECT APDIST

* Define private variable to save the current record no. of invoice distribution
PRIVATE  lnSaveRec
lnSaveRec = RECNO('APDIST')

* Initialize variables 
lnInvAmt   = 0                  
lnDiscTakn = 0                  
lnDiscOffr = 0                  
lnAdjtAmt  = 0                  
lnAmtPaid  = 0 
lnPurchAmt  = 0 
*If condition not found set the record pointer to the saved record no. then retrun
IF !SEEK( lcInvNo + M.cVendCode  , 'APDIST') 
  IF lnSaveRec > 0  AND lnSaveRec <= RECCOUNT('APDIST') 
  
    GOTO (lnSaveRec) IN APDIST
  ENDIF
  SELECT (lcCurArea)
  RETURN lnDiscOffr 
ENDIF 
 
* Loop the required records and calcluate the values.
SCAN REST WHILE APDIST.cVendCode + APDIST.cInvNo = M.cVendCode + lcInvNo ;
          FOR ( !DELETED() AND  APDIST.cApdStat <> 'V' ) 
  
  * Get equivelent value of A/P invoice amount or debit memo amount. 
  * Note: It must only one record per invoice.  
  IF APDIST.cApDtrTyp = 'I' AND APDIST.cApdActId = 'A'  
    lnInvAmt   = - APDIST.nEqvAmnt 
    lnPurchAmt = - APDIST.nEqvAmnt 
  ENDIF 

  * Notes
  * APDIST.cApDtrTyp contains 'A' means debit memo
  * APDIST.cApDtrTyp contains 'H' means cash payments 
  * APDIST.cApDtrTyp contains 'M' means manuals checks payments 
  * APDIST.cApDtrTyp contains 'N' means non checks payments 
  * APDIST.cApDtrTyp contains 'P' means printed checks
  
  * Get and sum the equivelent value of paid amount 
  IF APDIST.cApDtrTyp $ 'HMNP' AND APDIST.cApdActId = 'C'
    lnAmtPaid = lnAmtPaid -  APDIST.nEqvAmnt 
  ENDIF 

  * Get and sum the equivelent value of discount taken 
  IF APDIST.cApDtrTyp $ 'AHMNP' AND APDIST.cApdActId = 'S'
    lnDiscTakn = lnDiscTakn - APDIST.nEqvAmnt 
  ENDIF 

  
  * Get and sum the equivelent value of adj. amount.
  IF APDIST.cApDtrTyp $ 'HMNP' AND APDIST.cApdActId = 'J'
    lnAdjtAmt = lnAdjtAmt - APDIST.nEqvAmnt
  ENDIF
  
ENDSCAN  && End of Loop the required records and calcluate the values.       

IF APINVHDR.nInvAmnt > 0 && Payable Invoice
  SUM IIF(cApdActId ="A" and nEqvAmnt<0,-nEqvAmnt,0) , IIF ( cApdActId ="S",-nEqvAmnt,0) to lnp,lndis;
  FOR cApdTrTyp+cBnkCode+cChkAcct+cApdRef+cInvNo+cApdActId = "A"+SPACE(20)+PADR("lcInvNo",10)
  lnAmtPaid  = lnAmtPaid   + lnp
  lnDiscTakn = lnDiscTakn  + lndis
ENDIF  

* Get and calculate the value of discount offer from invoice header file.
lcExSin2   = ''
lcExSin1   = gfGetExSin(@lcExSin2, APINVHDR.cCurrCode ) 
lnDiscOffr = ROUND(APINVHDR.nInvDisOf  &lcExSin1 APINVHDR.nExRate &lcExSin2 APINVHDR.nCurrUnit ,2)


* Back to the initial record
SELECT APDIST
IF lnSaveRec > 0  AND lnSaveRec <= RECCOUNT()
  GOTO (lnSaveRec) IN APDIST
ENDIF  

* Back to the initial file and record no.
SELECT (lcCurArea)
RETURN lnDiscOffr 
