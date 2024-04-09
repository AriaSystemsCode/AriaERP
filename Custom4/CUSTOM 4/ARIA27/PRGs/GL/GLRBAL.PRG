*B601855,1 MAN 09/09/97 1. Windows screen disappears when rebalancing 
*B601855,1 				   (fix GLRBAL.scx)
*B601855,1 				2. Closing balance initialization   
*B602126,1 RENEE 12/16/98 Do not post closing entries
*E301257,1 AMIN  05/23/99 Implement the program in 2.7
*:************************************************************************
*



llBalnBfor = .F.         && Flag to know if the Rebalance process was done befor in another session without Updating the master file
llUsdAcBal = .F.         && Flag to know if the GLACBALS file was open befor
llUsdTrnHd = .F.         && Flag to know if the GLPTRNHD file was open befor
llUsdTrnDt = .F.         && Flag to know if the GLPTRNDT file was open befor

lcOnEsc = ''                    && Varible to hold the ON KEY of the Escape key
lcUpDateSt = 'DISABLE'          && Varible to hold the Update and the Trial balance Push Bottons status

*IF The memory variable file exist
IF FILE(gcDataDir + 'GLRBAL.MEM')
  RESTORE FROM (gcDataDir + 'GLRBAL.MEM') ADDITIVE
ENDIF    && End of IF


lcYear = IIF(TYPE('mvYear') = 'C' .AND. !EMPTY(mvYear) ,;
             mvYear , lcCurr_yer)    && Varible to hold the Starting from year

puYear = lcYear        && Varible to create the Popup in the Windows platform
lcRBalYear = lcYear    && Varible to hold the Starting year of the Rebalance process
lnPostTran = IIF(TYPE('mvPostTran') <> 'N' , 0 , mvPostTran)    && Varible to hold the number of transactions that was Reposted in the Rebalance process

RELEASE ALL LIKE mv*

DECLARE laUsedFile[5,5] , laYear[1,1]

laUsedFile[1,1] = 'GLBATCH'
laUsedFile[1,2] = 'BATSTAT'
laUsedFile[1,3] = 'GLBATCH'

laUsedFile[2,1] = 'GLPTRNHD'
laUsedFile[2,2] = ''
laUsedFile[2,3] = 'GLTRNSHD'

laUsedFile[3,1] = 'GLPTRNDT'
laUsedFile[3,2] = 'BATCHTRN'
laUsedFile[3,3] = 'GLTRNSDT'

laUsedFile[4,1] = 'GLACBALS'
laUsedFile[4,2] = 'ACCYRPRD'
laUsedFile[4,3] = 'GLTMPBAL'

laUsedFile[5,1] = 'GLACCHAR'
laUsedFile[5,2] = 'ACCTCODE'
laUsedFile[5,3] = 'GLACCHAR'

lcTmpBBtch  = gfTempName()        && Varible to hold a Temp. name for the file that will hold the Beginning batches to be Reposted
lcTmpJBtch  = gfTempName()        && Varible to hold a Temp. name for the file that will hold the Normal batches to be Reposted
lcTmpCBtch  = gfTempName()        && Varible to hold a Temp. name for the file that will hold the Closing batches to be Reposted
lcTmpTran   = gfTempName()        && Varible to hold a Temp. name for the file that will hold the Single transactions to be Reposted
lcTmpGLTHd  = gfTempName()        && Varible to hold a Temp. name for a Temp. file to be used as a dum GLPTRNHD file
lcTmpGLTDt  = gfTempName()        && Varible to hold a Temp. name for a Temp. file to be used as a dum GLTRNSDT file



lcPathStat = SET('FULLPATH ')     && Varible to save the SET FULLPATH status
SET  FULLPATH ON


=lfOpenFile(.T.)

SELECT GLTRNSHD
COPY STRUCTURE TO (gcWorkDir + lcTmpGLTHd) WITH CDX
SELECT GLTRNSDT
COPY STRUCTURE TO (gcWorkDir + lcTmpGLTDt) WITH CDX




*IF There is any file that is opened with the Alias GLPTRNHD
IF USED('GLPTRNHD')
  llUsdTrnHd = .T.
  SELECT GLPTRNHD
  lcUsdTrnHd = DBF()          && Varible to hold the name and path of the file that was opened with the Alias GLPTRNHD
  lnUsdTrnHd = SELECT(0)      && Varible to hold the number of the work area that has the Alias GLPTRNHD
  USE
ENDIF    && End of IF

*IF There is any file that is opened with the Alias GLPTRNDT
IF USED('GLPTRNDT')
  llUsdTrnDt = .T.
  SELECT GLPTRNDT
  lcUsdTrnDt = DBF()          && Varible to hold the name and path of the file that was opened with the Alias GLPTRNDT
  lnUsdTrnDt = SELECT(0)      && Varible to hold the number of the work area that has the Alias GLPTRNDT
  USE
ENDIF    && End of IF

USE (gcWorkDir + lcTmpGLTHd) ALIAS GLPTRNHD IN 0
USE (gcWorkDir + lcTmpGLTDt) ALIAS GLPTRNDT IN 0

*E301257,1  Start
*E301257,1 After having each company it's own chart of account and data file,
*E301257,1 so it's important to open the files from data directory instead from system directory.
*E301257,1 *** Note :  Files in version 2.6 changed it's name in version 2.7
*E301257,1 ***         Old File Name ( Version 2.6)      New File Name (Version 2.7)
*E301257,1 ***         ..\sysfiles\SYCFISHD              ..\gcDataDir\FISHD
*E301257,1 ***         ..\sysfiles\SYCFSPRD              ..\gcDataDir\FSPRD

*SELECT cFisFYear ;
*  FROM (gcSysHome + 'SYCFISHD');
*  INTO ARRAY laYear ;
* WHERE cComp_Id + cFisFYear = gcAct_Comp .AND. cFisYStat <> 'H'

SELECT cFisFYear ;
  FROM (gcDataDir + 'FISHD');
  INTO ARRAY laYear ;
 WHERE cFisYStat <> 'H'
*E301257,1 End


*IF There is any file that is opened with the Alias GLACBALS
IF USED('GLACBALS')
  llUsdAcBal = .T.
  SELECT GLACBALS
  lnUsdAcBal = SELECT(0)      && Varible to hold the number of the work area that has the Alias GLACBALS
  lcUsdAcBal = DBF()          && Varible to hold the name and path of the file that was opened with the Alias GLACBALS
  USE
ENDIF    && End of IF

*IF The file GLTMPBAL exist
IF FILE(gcDataDir + 'GLTMPBAL.DBF')
  llBalnBfor = .T.
  lcUpDateSt = 'ENABLE'
  USE (gcDataDir + 'GLTMPBAL.DBF') ALIAS GLACBALS IN 0 EXCLUSIVE
ENDIF    && End of IF

SET FULLPATH &lcPathStat

IF !gfSetup()
  RETURN
ENDIF

*E301257,1 Start
*** Change way of the program calling 
*DO GLRBAL.SPR
DO (gcScrDir + gcWinAppl + "\GLRBAL.SPR")
*E301257,1 End


=lfOpenFile(.F.)

*IF The Alias GLACBALS exist [That was used for the Temp. balance file]
IF USED('GLACBALS')
  USE IN GLACBALS
ENDIF    && End of IF

*IF There was any file that was opened with the Alias GLACBALS
IF llUsdAcBal
  USE (lcUsdAcBal) IN (lnUsdAcBal)
ENDIF    && End of IF

*IF There was any file that was opened with the Alias GLPTRNHD
IF llUsdTrnHd
  USE (lcUsdTrnHd) IN (lnUsdTrnHd)
ENDIF    && End of IF

*IF There was any file that was opened with the Alias GLPTRNDT
IF llUsdTrnDt
  USE (lcUsdTrnDt) IN (lnUsdTrnDt)
ENDIF    && End of IF

glQuitting = .T.



*IF The Temp. file that we used as a dum GLPTRNHD file exist

IF FILE(gcWorkDir + lcTmpGLTHd + '.DBF')
  *E301257,1 Start
  IF USED('GLPTRNHD')
    USE IN GLPTRNHD
  ENDIF 
  *E301257,1 End
  
  ERASE (gcWorkDir + lcTmpGLTHd + '.DBF')
ENDIF    && End of IF

*IF The CDX file of the Temp. file that we used as a dum GLPTRNHD file exist
IF FILE(gcWorkDir + lcTmpGLTHd + '.CDX')
  ERASE (gcWorkDir + lcTmpGLTHd + '.CDX')
ENDIF    && End of IF

*IF The Temp. file that we used as a dum GLPTRNDT file exist
IF FILE(gcWorkDir + lcTmpGLTDt + '.DBF')
  *E301257,1 Start
  IF USED('GLPTRNDT')
    USE IN GLPTRNDT 
  ENDIF 
  *E301257,1 End

  ERASE (gcWorkDir + lcTmpGLTDt + '.DBF')
ENDIF    && End of IF

*IF The CDX file of the Temp. file that we used as a dum GLPTRNDT file exist
IF FILE(gcWorkDir + lcTmpGLTDt + '.CDX')
  ERASE (gcWorkDir + lcTmpGLTDt + '.CDX')
ENDIF    && End of IF

*B602126,1 Close and erase temporary files
IF USED(lcTmpBBtch)
  USE IN (lcTmpBBtch)
ENDIF    && End of IF
ERASE (gcWorkDir + lcTmpBBtch + '.DBF')  
ERASE (gcWorkDir + lcTmpBBtch + '.CDX')  

IF USED(lcTmpJBtch)
  USE IN (lcTmpJBtch)
ENDIF    && End of IF
ERASE (gcWorkDir + lcTmpJBtch + '.DBF')  
ERASE (gcWorkDir + lcTmpJBtch + '.CDX')  

IF USED(lcTmpCBtch)
  USE IN (lcTmpCBtch)
ENDIF    && End of IF
ERASE (gcWorkDir + lcTmpCBtch + '.DBF')  
ERASE (gcWorkDir + lcTmpCBtch + '.CDX')  

IF USED(lcTmpTran)
  USE IN (lcTmpTran)
ENDIF    && End of IF
ERASE (gcWorkDir + lcTmpTran + '.DBF')  
ERASE (gcWorkDir + lcTmpTran + '.CDX')  
*B602126,1 end

*!*************************************************************
*! Name      : lfOpenFile
*! Developer : Haytham El_Sheltawi
*! Date      : 08/13/1997
*! Purpose   : Function to open and close the needed files
*!*************************************************************
*! Called from : GLRBAL.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : .T. [To open the files] ,
*!                     .F. [To close the files]
*!*************************************************************
*
FUNCTION lfOpenFile


PARAMETERS llOpen
PRIVATE lnCount , lcPathStat

*IF The function is to open the files
IF llOpen
  
  *FOR Loop to loop the array laUsedFile
  FOR lnCount = 1 TO ALEN(laUsedFile , 1)
    
    *IF The file is not open
    IF !USED(laUsedFile[lnCount , 3])
      SELECT 0
      USE (gcDataDir + laUsedFile[lnCount , 1]) AGAIN ALIAS &laUsedFile[lnCount , 3]
      
      *IF The file is to be opened with an Index Tag
      IF !EMPTY(laUsedFile[lnCount , 2])
        SET ORDER TO TAG (laUsedFile[lnCount , 2])
      ENDIF    && End of IF
      laUsedFile[lnCount , 5] = .T.

    ELSE    && Else

      SELECT (laUsedFile[lnCount , 3])

      *IF we are going to open another file with this Alias
      IF laUsedFile[lnCount , 1] <> laUsedFile[lnCount , 3]
        laUsedFile[lnCount , 4] = DBF()
        USE (gcDataDir + laUsedFile[lnCount , 1]) AGAIN ALIAS &laUsedFile[lnCount , 3]
      ENDIF    && End of IF
      
      *IF The file is to be opened with an Index Tag
      IF !EMPTY(laUsedFile[lnCount , 2])
        SET ORDER TO TAG (laUsedFile[lnCount , 2])
      ELSE    && Else
        SET ORDER TO
      ENDIF    && End of IF
      SET FILTER TO 
      laUsedFile[lnCount , 5] = .F.
    ENDIF    && End of IF
    GO TOP
  ENDFOR    && End of FOR Loop

ELSE    && Else

  *FOR Loop to loop the array laUsedFile
  FOR lnCount = 1 TO ALEN(laUsedFile , 1)

    *IF The file was not open befor
    IF laUsedFile[lnCount , 5]
      USE IN (laUsedFile[lnCount , 3])
    ELSE    && Else
      *IF The file was opened with the Alias of another file
      IF laUsedFile[lnCount , 1] <> laUsedFile[lnCount , 3]
        USE (laUsedFile[lnCount , 4]) AGAIN IN &laUsedFile[lnCount , 3]
      ENDIF    && End of IF
    ENDIF    && End of IF
  ENDFOR    && End of FOR Loop
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfwPopup
*! Developer : Haytham El_Sheltawi
*! Date      : 08/13/1997
*! Purpose   : When function for the Popup Starting year
*!*************************************************************
*! Called from : Screen GLRBAL
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfwPopup

*Show get the hand made popups [for Dos platform] with the appropirate color
SHOW GET ibPopYear COLOR ,,,,,&lcSelCont,,,&lcEnbCont,&lcDisCont

*IF The platform is Windows
IF _WINDOWS
  lcOnEsc = ON('KEY', 'ESC')
  ON KEY LABEL ESC llDum = .T.
  ON KEY LABEL ENTER llDum = .T.
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfvPopup
*! Developer : Haytham El_Sheltawi
*! Date      : 08/14/1997
*! Purpose   : Valid function for the Popup Starting year
*!*************************************************************
*! Called from : Screen GLRBAL
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvPopup

*IF The platform is Dos
IF _DOS
  = gfActPop(1,35,6,43,'laYear',1,1,@lcYear)

  *Show get the hand made popups [for Dos platform] with the appropirate color
  SHOW GET ibPopYear  COLOR ,,,,,&lcSelCont,,,&lcEnbCont,&lcDisCont
  SHOW GETS
ELSE    && Else
  lcYear = puYear
  ON KEY LABEL ESC &lcOnEsc
  ON KEY LABEL ENTER
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfvRBalnce
*! Developer : Haytham El_Sheltawi
*! Date      : 08/14/1997
*! Purpose   : Valid function for the Push Button Rebalance
*!*************************************************************
*! Called from : Screen GLRBAL
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvRBalnce

lcRBalYear = lcYear
llBalnBfor = .F.

* Meesage : "Recalculate all accounts starting from ð ?"
* Buttons :           <Proceed> <Cacel>
*IF The user selected to Cancel

*E301257,1 Start 
* Assign new message no. in Aria 2.7 instead of message no. in Adv1 
*IF gfModalGen("TRM02247B00012" , "DIALOG" , lcYear) = 2
*  RETURN
*ENDIF    && End of IF
IF gfModalGen("TRM02249B00012" , "DIALOG" , lcYear) = 2
  RETURN
ENDIF    && End of IF
*E301257,1 End



*IF The Starting year is the Previous year
IF VAL(lcYear) < VAL(lcCurr_yer)
  
  * Meesage : "You have selected to rebalance starting previous year (ð). "
  *           "The system allows purging files of the previous year.      "
  *           "Rebalancing starting from previous year wan't be accurate  "
  *           "if you purged the files through this year.                 "
  * Buttons :           <Proceed> <Cacel>
  *IF The user selected to Cancel
  *E301257,1 Start 
  * Assign new message no. in Aria 2.7 instead of message no. in Adv1 
  *IF gfModalGen("TRM02252B00012" , "DIALOG" , lcYear) = 2
  *  RETURN
  *ENDIF    && End of IF
  IF gfModalGen("TRM02250B00012" , "DIALOG" , lcYear) = 2
    RETURN
  ENDIF    && End of IF
  *E301257,1 End
  
ENDIF    && End of IF

=lfFixBatch()

*IF There is any Batches or Transactions to be Reposted
IF lfSelData()
  
  *IF There is Single transactions to be Reposted and not all of them was  
  *Reposted succesfully
  *B602126,1 Use dummy posting by passing the fourth parameter to lftbpost
  *IF RECCOUNT(lcTmpTran) <> 0 ;
     .AND. RECCOUNT(lcTmpTran) <> lfTBPost("Transactions" , lcTmpTran , '')
  IF RECCOUNT(lcTmpTran) <> 0 ;
     .AND. RECCOUNT(lcTmpTran) <> lfTBPost("Transactions" , lcTmpTran , '', "GLACBALS")
  *B602126,1 end  
    * Meesage : "Some of the ð were not succesfully reposted to the "
    *           "balances file , Unable to proceed with rebalancing."
    * Buttons :                          <Ok>

    *E301257,1 Start 
    * Assign new message no. in Aria 2.7 instead of message no. in Adv1 
    *=gfModalGen("TRM02250B00000" , "DIALOG" , "transactions")
    =gfModalGen("TRM02251B00000" , "DIALOG" , "transactions")
    *E301257,1 End

    =lfRmTmpBal()
    RETURN
  ENDIF    && End of IF
  
  *IF There is Beginning batches to be Reposted and not all of them was  
  *Reposted succesfully
  *B602126,1 Use dummy posting by passing the fourth parameter to lftbpost
  *IF RECCOUNT(lcTmpBBtch) <> 0 ;
     .AND. RECCOUNT(lcTmpBBtch) <> lfTBPost("Batch" , lcTmpBBtch , "Beginning")
  IF RECCOUNT(lcTmpBBtch) <> 0 ;
     .AND. RECCOUNT(lcTmpBBtch) <> lfTBPost("Batch" , lcTmpBBtch , "Beginning","GLACBALS")
  *B602126,1 end
    * Meesage : "Some of the ð were not succesfully reposted to the "
    *           "balances file , Unable to proceed with rebalancing."
    * Buttons :           <Ok>
    *E301257,1 Start 
    * Assign new message no. in Aria 2.7 instead of message no. in Adv1 
    *=gfModalGen("TRM02250B00000" , "DIALOG" , "beginning batches")
    =gfModalGen("TRM02251B00000" , "DIALOG" , "beginning batches")    
    *E301257,1 End
    =lfRmTmpBal()
    RETURN
  ENDIF    && End of IF
  
  *IF There is Normal batches to be Reposted and not all of them was  
  *Reposted succesfully
  *B602126,1 Use dummy posting by passing the fourth parameter to lftbpost
  *IF RECCOUNT(lcTmpJBtch) <> 0 ;
     .AND. RECCOUNT(lcTmpJBtch) <> lfTBPost("Batch" , lcTmpJBtch , "Nonbeginning")
  IF RECCOUNT(lcTmpJBtch) <> 0 ;
     .AND. RECCOUNT(lcTmpJBtch) <> lfTBPost("Batch" , lcTmpJBtch , "Nonbeginning","GLACBALS")
  *B602126,1 end
  
    * Meesage : "Some of the ð were not succesfully reposted to the "
    *           "balances file , Unable to proceed with rebalancing."
    * Buttons :           <Ok>
    *E301257,1 Start 
    * Assign new message no. in Aria 2.7 instead of message no. in Adv1 
    *=gfModalGen("TRM02250B00000" , "DIALOG" , "batches")
    =gfModalGen("TRM02251B00000" , "DIALOG" , "batches")
    *E301257,1 End
    =lfRmTmpBal()
    RETURN
  ENDIF    && End of IF
  
  *IF There is Closing batches to be Reposted and not all of them was  
  *Reposted succesfully
  *B602126,1 Do not post closing batches. They are already created
  *B602126,1 by the posting program
  *IF RECCOUNT(lcTmpCBtch) <> 0 ;
  *   .AND. RECCOUNT(lcTmpCBtch) <> lfTBPost("Batch" , lcTmpCBtch , "Closing")
  
    * Meesage : "Some of the ð were not succesfully reposted to the "
    *           "balances file , Unable to proceed with rebalancing."
    * Buttons :           <Ok>
  *  =gfModalGen("TRM02250B00000" , "DIALOG" , "closing batches")
  *  =lfRmTmpBal()
  *  RETURN
  *ENDIF    && End of IF
  *B602126,1 end
  
  mvYear = lcYear
  mvPostTran = lnPostTran
  SAVE TO (gcDataDir + 'GLRBAL.MEM') ALL LIKE mv*
  SHOW GET pbReport ENABLE
  SHOW GET pbUpDate ENABLE
ELSE    && Else
  
  * Meesage : "No butche's or transaction's were reposted."
  * Buttons :                <Ok>
  *E301257,1 Start 
  * Assign new message no. in Aria 2.7 instead of message no. in Adv1 
  *=gfModalGen("TRM02248B00000" , "DIALOG")
  =gfModalGen("TRM02252B00000" , "DIALOG")  
  *E301257,1 End
  
  mvYear = lcYear
  mvPostTran = lnPostTran
  SAVE TO (gcDataDir + 'GLRBAL.MEM') ALL LIKE mv*
  SHOW GET pbReport ENABLE
  SHOW GET pbUpDate ENABLE
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfvClose
*! Developer : Haytham El_Sheltawi
*! Date      : 08/14/1997
*! Purpose   : Valid function for the Push Button Close
*!*************************************************************
*! Called from : Screen GLRBAL
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
*! Name      : lfSelData
*! Developer : Haytham El_Sheltawi
*! Date      : 08/14/1997
*! Purpose   : Function to get the Single transactions ,
*!             Beginning batches , Normal batches and Closing batches
*!             to be Reposted and repare the Temp. Balance file for
*!             Reposting
*!*************************************************************
*! Called from : Screen GLRBAL
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : .T. if ther is any Batches or Transactions to be
*!               Reposted , .F. Otherwise
*!*************************************************************
*
FUNCTION lfSelData

PRIVATE lnPBatTran , lnTotal , lnCurrent , lcAccount , llBalShtAc

lnPBatTran = 0        && Varible to hold the number of Transactions in the Batches to be Reposted
lnPostTran = 0

*B602126,1 Create tables instead of cursors. No need to close them
*IF The Cursor [lcTmpBBtch] exist
*IF USED(lcTmpBBtch)
*  USE IN (lcTmpBBtch)
*ENDIF    && End of IF

*IF The Cursor [lcTmpJBtch] exist
*IF USED(lcTmpJBtch)
*  USE IN (lcTmpJBtch)
*ENDIF    && End of IF

*IF The Cursor [lcTmpCBtch] exist
*IF USED(lcTmpCBtch)
*  USE IN (lcTmpCBtch)
*ENDIF    && End of IF

*IF The Cursor [lcTmpTran] exist
*IF USED(lcTmpTran)
*  USE IN (lcTmpTran)
*ENDIF    && End of IF
*B602126,1 end

*B602126,1 Create table instead of a cursor
*CREATE CURSOR &lcTmpBBtch (cBatchNo C(6) , cBatStat C(1))
CREATE TABLE (gcWorkDir + lcTmpBBtch) (cBatchNo C(6) , cBatStat C(1))
*B602126,1 end
INDEX ON cBatchNo TAG CBATCHNO 
SET ORDER TO CBATCHNO      

*B602126,1 Create table instead of a cursor
*CREATE CURSOR &lcTmpJBtch (cBatchNo C(6) , cBatStat C(1))
CREATE TABLE (gcWorkDir + lcTmpJBtch) (cBatchNo C(6) , cBatStat C(1))
*B602126,1 end
INDEX ON cBatchNo TAG CBATCHNO 
SET ORDER TO CBATCHNO      

*B602126,1 Create table instead of a cursor
*CREATE CURSOR &lcTmpCBtch (cBatchNo C(6) , cBatStat C(1))
CREATE TABLE (gcWorkDir + lcTmpCBtch) (cBatchNo C(6) , cBatStat C(1))
INDEX ON cBatchNo TAG CBATCHNO 
SET ORDER TO CBATCHNO      

*B602126,1 Create table instead of a cursor
*CREATE CURSOR &lcTmpTran (cTranNo C(8) , cTrnStat C(1))
CREATE TABLE (gcWorkDir + lcTmpTran) (cTranNo C(8) , cTrnStat C(1))
INDEX ON cTranNo TAG CTRANNO 
*SET ORDER TO CTRANNO

SELECT GLBATCH

*IF There is a posted Batches
IF SEEK('P')
  
  *SCAN Loop to scan the GLBATCH file [The Batches file] FOR the posted
  *Batches with cBatPYr > or = the Starting year
  SCAN REST;
      WHILE cBatStat = 'P';
        FOR cBatPYr >= lcYear
    
    *DO CASE Statment
    DO CASE
      *Case of Closing Batches
      CASE cBatType = 'C'
        INSERT INTO &lcTmpCBtch (cBatchNo , cBatStat) VALUES;
                                (GLBATCH.cBatchNo , GLBATCH.cBatStat)
      
      *Case of Beginning Batches
      CASE cBatType = 'B'
        INSERT INTO &lcTmpBBtch (cBatchNo , cBatStat) VALUES;
                                (GLBATCH.cBatchNo , GLBATCH.cBatStat)
      
      *Otherwise
      OTHERWISE
        INSERT INTO &lcTmpJBtch (cBatchNo , cBatStat) VALUES;
                                (GLBATCH.cBatchNo , GLBATCH.cBatStat)
    
    ENDCASE    && End of DO CASE Statment
    
  ENDSCAN    && End of SCAN Loop
ENDIF    && End of IF

SELECT GLTRNSHD

*SCAN Loop to scan the GLTRNSHD file [The Posted Transactios header file
*opened with the Alias GLTRNSHD] FOR Batch # = 0 and cTrnPYr > or = the
*Starting year
SCAN FOR cBatchNo + cTranNo = '000000' .AND. cTrnPYr >= lcYear
  lnPostTran = lnPostTran + 1
  INSERT INTO &lcTmpTran (cTranNo , cTrnStat) VALUES;
                         (GLTRNSHD.cTranNo , GLTRNSHD.cTrnStat)
  
ENDSCAN    && End of SCAN Loop

COUNT FOR cBatchNo + cTranNo <> '000000';
     .AND. cTrnPYr >= lcYear;
       TO lnPBatTran

lnPostTran = lnPostTran + lnPBatTran

*IF The Temp. Balance file [Opened with the Alias GLACBALS] exist
IF USED('GLACBALS')
  USE IN GLACBALS
  
  *IF The file GLTMPBAL [Temp. Balance file] exist
  IF FILE(gcDataDir + 'GLTMPBAL.DBF')
    ERASE (gcDataDir + 'GLTMPBAL.DBF')
  ENDIF    && End of IF
  
  *IF The CDX file of GLTMPBAL [Temp. Balance file] exist
  IF FILE(gcDataDir + 'GLTMPBAL.CDX')
    ERASE (gcDataDir + 'GLTMPBAL.CDX')
  ENDIF    && End of IF
ENDIF    && End of IF

SELECT GLTMPBAL
COPY STRUCTURE TO (gcDataDir + 'GLTMPBAL.DBF') WITH CDX
USE (gcDataDir + 'GLTMPBAL.DBF') ALIAS GLACBALS IN 0

SELECT GLTMPBAL
lnTotal = RECCOUNT()        && Varible to hold the number of Account Balances file records [Opened with the Alias GLTMPBAL]
lnCurrent = 0               && Varible to hold the number of records that was finished

GO TOP

*SCAN Loop to Scan the Account Balances file 
SCAN
  lcAccount = cAcctCode        && Varible to hold the current account code
  =SEEK(cAcctCode , 'GLACCHAR')
  llBalShtAc = IIF(LEFT(GLACCHAR.cTypeCode , 1) $ 'ALQY' , .T. , .F.)    && Flag to know if the account is one of the Balance shet accounts
  
  *SCAN Loop to Scan the Account Balances file for the current account
  *and Fiscal year < the Starting from year
  SCAN REST;
      WHILE cAcctCode = lcAccount .AND. cFisFYear < lcYear
     
    lnCurrent = lnCurrent + 1
    SCATTER MEMVAR MEMO
    INSERT INTO GLACBALS FROM MEMVAR
    =gfThermo(lnTotal , lnCurrent , 'Fixing Account balance file')
  ENDSCAN    && End of SCAN Loop
  
  SKIP -1
  *B601855,1 Start
  *lnClosBal = nAcbClBal        && Varible to hold the closing balance of the account for the last Period in the last Fiscal year < the Starting from year
  IF lcAccount <> cAcctCode
    lnClosBal = 0
  ELSE  
    lnClosBal = nAcbClBal
  ENDIF
  *B601855,1 End
  SCATTER FIELDS LIKE N* MEMVAR BLANK
  
  *IF We are at the Begin of the Account Balances file
  IF BOF()
    GO RECNO()
  ELSE    && Else
    SKIP 1
  ENDIF    && End of IF
  
  *SCAN Loop to Scan the Account Balances file for the current account
  *and Fiscal year > or = the Starting from year
  SCAN REST;
      WHILE cAcctCode = lcAccount
    
    lnCurrent = lnCurrent + 1
    SCATTER FIELDS EXCEPT N* MEMVAR MEMO
    INSERT INTO GLACBALS FROM MEMVAR
    
    *IF the account is one of the Balance shet accounts
    IF llBalShtAc
      REPLACE GLACBALS.nAcbOpBal WITH lnClosBal;
              GLACBALS.nAcbClBal WITH lnClosBal
    ENDIF    && End of IF
    =gfThermo(lnTotal , lnCurrent , 'Fixing Account balance file')
  ENDSCAN    && End of SCAN Loop
  SKIP -1  
ENDSCAN    && End of SCAN Loop
=gfThermo(lnTotal , lnTotal , 'Fixing Account balance file')

RETURN lnPostTran <> 0

*!*************************************************************
*! Name      : lfvUpDate
*! Developer : Haytham El_Sheltawi
*! Date      : 08/15/1997
*! Purpose   : Valid function for the Push Button Update
*!*************************************************************
*! Called from : Screen GLRBAL
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvUpDate

PRIVATE lnCurPost
lnCurPost = 0          && Varible to hold the number of Transactions posted with cTrnPYr > or = Starting year

*IF the Rebalance process was done befor in another session without Updating
*the master file
IF llBalnBfor
  SELECT GLTRNSHD
  COUNT FOR cTrnPYr >= lcYear TO lnCurPost
  
  *IF the number of Transactions posted with cTrnPYr > or = Starting year
  *dose not equal the number of transactions that was Reposted in the
  *Rebalance process
  IF lnCurPost <> lnPostTran
    
    * Meesage : "One or more transaction were posted since the last "
    *           "recaculation of balances. You need to run the      "
    *           "recalculation process again.                       " 
    * Buttons :                     <Ok>
    *E301257,1 Start 
    * Assign new message no. in Aria 2.7 instead of message no. in Adv1 
    *=gfModalGen("TRM02249B00000" , "DIALOG")
    =gfModalGen("TRM02253B00000" , "DIALOG")    
    *E301257,1 End
    
    =lfRmTmpBal()
    RETURN
  ENDIF    && End of IF
  
  SELECT GLACBALS
  SET ORDER TO TAG ACCYRPRD
  SET RELATION TO cAcctCode INTO GLACCHAR
  LOCATE FOR EOF('GLACCHAR')
  
  *IF There is an account that exist in the Temp. balance file and dose
  *not exist in the GLACCHAR file [Chart of accounts file]
  IF FOUND()
    SET RELATION TO
    
    * Meesage : "Found mismatch between the current chart of account's, and"
    *           "the recalculated balances file. You need to run the       "
    *           "recalculation process again.                              "
    * Buttons :                         <Ok>
    *E301257,1 Start 
    * Assign new message no. in Aria 2.7 instead of message no. in Adv1 
    *=gfModalGen("TRM02251B00000" , "DIALOG")
    =gfModalGen("TRM02254B00000" , "DIALOG")
    *E301257,1 End
    
    =lfRmTmpBal()
    RETURN
  ENDIF    && End of IF
  
  SET RELATION TO
  SELECT GLACCHAR
  SET RELATION TO cAcctCode INTO GLACBALS
  LOCATE FOR EOF('GLACBALS')
  
  *IF There is an account that exist in the GLACCHAR file [Chart of accounts
  *file] and dose not exist in the Temp. balance file
  IF FOUND()
    SET RELATION TO
    
    * Meesage : "Found mismatch between the current chart of account's, and"
    *           "the recalculated balances file. You need to run the       "
    *           "recalculation process again.                              "
    * Buttons :                     <Ok>
    *E301257,1 Start 
    * Assign new message no. in Aria 2.7 instead of message no. in Adv1 
    *=gfModalGen("TRM02251B00000" , "DIALOG")
    =gfModalGen("TRM02254B00000" , "DIALOG")    
    *E301257,1 End
    
    =lfRmTmpBal()
    RETURN
  ENDIF    && End of IF
  SET RELATION TO
ENDIF    && End of IF

* Meesage : "We strongly recommend that a backup is done for company ð data"
*           "files immediately prior to the ð process.  Do you wish to     "
*           "continue?                                                     "
* Buttons :                     <Proceed>    <Cancel>
*IF The user selected to cancel

*E301257,1 Start 
* Assign new message no. in Aria 2.7 instead of message no. in Adv1 
*IF gfModalGen("TRM02253B00012" , "ALERT" , gcAct_Comp + '|Update') = 2
*  RETURN
*ENDIF    && End of IF
IF gfModalGen("TRM02255B00012" , "ALERT" , gcAct_Comp + '|Update') = 2
  RETURN
ENDIF    && End of IF
*E301257,1 End

*IF The Starting year value in the Popup is not the same that was used
*in the Rebalance process
IF lcRBalYear <> lcYear
  lcYear = lcRBalYear
  puYear = lcYear
  SHOW GETS
ENDIF    && End of IF

USE IN GLACBALS
USE IN GLTMPBAL
RENAME (gcDataDir + 'GLACBALS.DBF') TO (gcDataDir + 'TMPNAME.DBF')
RENAME (gcDataDir + 'GLACBALS.CDX') TO (gcDataDir + 'TMPNAME.CDX')
RENAME (gcDataDir + 'GLTMPBAL.DBF') TO (gcDataDir + 'GLACBALS.DBF')
RENAME (gcDataDir + 'GLTMPBAL.CDX') TO (gcDataDir + 'GLACBALS.CDX')

ERASE (gcDataDir + 'TMPNAME.DBF')
ERASE (gcDataDir + 'TMPNAME.CDX')

*IF The memory variable file exist
IF FILE(gcDataDir + 'GLRBAL.MEM')
  ERASE (gcDataDir + 'GLRBAL.MEM')
ENDIF    && End of IF

USE (gcDataDir + 'GLACBALS.DBF') ALIAS GLTMPBAL ORDER TAG ACCYRPRD IN 0
SHOW GET pbReport DISABLE
SHOW GET pbUpDate DISABLE

*!*************************************************************
*! Name      : lfvReport
*! Developer : Haytham El_Sheltawi
*! Date      : 08/15/1997
*! Purpose   : Valid function for the Push Button Trial balance
*!*************************************************************
*! Called from : Screen GLRBAL
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvReport

DO (gcRepHome + 'GLREPORT.APP') WITH 'GLTRABAL'

*!*************************************************************
*! Name      : lfRmTmpBal
*! Developer : Haytham El_Sheltawi
*! Date      : 08/15/1997
*! Purpose   : Function to delete the GLTMPBAL file and the
*!             GLRBAL.MEM memory variable file
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
FUNCTION lfRmTmpBal

lcRBalYear = lcYear
lnPostTran = 0

*IF The Alias GLACBALS exist [That was used for the Temp. balance file]
IF USED('GLACBALS')
  USE IN GLACBALS
ENDIF    && End of IF

*IF The file GLTMPBAL exist [The Temp. balance file]
IF FILE(gcDataDir + 'GLTMPBAL.DBF')
  ERASE (gcDataDir + 'GLTMPBAL.DBF')
ENDIF    && End of IF

*IF The CDX file of GLTMPBAL [Temp. Balance file] exist
IF FILE(gcDataDir + 'GLTMPBAL.CDX')
  ERASE (gcDataDir + 'GLTMPBAL.CDX')
ENDIF    && End of IF

*IF The memory variable file exist
IF FILE(gcDataDir + 'GLRBAL.MEM')
  ERASE (gcDataDir + 'GLRBAL.MEM')
ENDIF    && End of IF

SHOW GET pbReport DISABLE
SHOW GET pbUpDate DISABLE

*!*************************************************************
*! Name      : lfFixBatch
*! Developer : Haytham El_Sheltawi
*! Date      : 08/15/1997
*! Purpose   : Function to Fix the GLBATCH file
*!*************************************************************
*! Called from : lfvRBalnce()
*!*************************************************************
*! Calls       : lfPreData() , gfThermo()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfFixBatch

PRIVATE lcSetDelSt , llFixSom

llFixSom = .F.                  && Flag to know if we have fixed any records
lcSetDelSt = SET('DELETED')     && Varible to save the SET DELETED status
SET DELETED OFF

lcBatType = ''          && Varible to hold the Batch type
ldBatPBeg = {}          && Varible to hold the Batch beging poste date
ldBatPEnd = {}          && Varible to hold the Batch ending poste date
lnBatCnTot = 0          && Varible to hold the Batch Total Dibet

SET ORDER TO TAG BATCHNO  IN GLBATCH
SELECT GLTRNSHD
SET ORDER TO TAG BATCHTRN
SET RELATION TO cBatchNo INTO GLBATCH
GO TOP

lnCurRec = 0                && Varible to use for the thermometer
lnTotRec = RECCOUNT()       && Varible to use for the thermometer

*SCAN Loop to scan the Posted transaction header for the records that
*has Batch number and this Batch number dose not exist in the GLBATCH
*file or was deleted
SCAN FOR cBatchNo + cTranNo <> '000000' ;
    .AND. (EOF('GLBATCH') .OR. DELETED('GLBATCH'))

  llFixSom = .T.
  
  *IF The Batch number was deleted
  IF DELETED('GLBATCH')
    lnCurRec = lnCurRec + 1
    SELECT GLBATCH
    RECALL
    SELECT GLTRNSHD
  ELSE    && Else
    =lfPreData()
    SKIP -1
    INSERT INTO GLBATCH (cBatchNo , cBatStat , lBatInd , cBatType ,;
                         cBatPyr , dBatPBeg , dBatPEnd , nBatCnTot ,;
                         nBaTotDR , nBaTotCR , cSrcModul , cComp_ID ,;
                         cPostSess , cPostProg , cPostUser , dPostDate ,;
                         cPostTime , cAdd_User , dAdd_Date , cAdd_Time);
         VALUES (GLTRNSHD.cBatchNo , GLTRNSHD.cTrnStat , .F. , lcBatType ,;
                 GLTRNSHD.cTrnPYr , ldBatPBeg , ldBatPEnd , lnBatCnTot ,;
                 lnBatCnTot , lnBatCnTot , GLTRNSHD.cSrcModul ,;
                 GLTRNSHD.cComp_ID , GLTRNSHD.cPostSess , GLTRNSHD.cPostProg ,;
                 GLTRNSHD.cPostUser , GLTRNSHD.dPostDate , GLTRNSHD.cPostTime ,;
                 GLTRNSHD.cAdd_User , GLTRNSHD.dAdd_Date , GLTRNSHD.cAdd_Time)
  ENDIF    && End of IF
  =gfThermo(lnTotRec , lnCurRec , 'Fixing Batches file')
ENDSCAN    && End of Scan Loop

*IF we had Fix any records
IF llFixSom
  *FOR Loop to complete the thermometer
  FOR lnRst = lnCurRec TO lnTotRec
    =gfThermo(lnTotRec , lnRst , 'Fixing Batches file')
  ENDFOR    && End of FOR Loop
ENDIF    && End of IF

SET RELATION TO
SET ORDER TO
SET ORDER TO TAG BATSTAT IN GLBATCH

SET DELETED &lcSetDelSt

*!*************************************************************
*! Name      : lfPreData
*! Developer : Haytham El_Sheltawi
*! Date      : 08/15/1997
*! Purpose   : Function to get the values for cBatType , nBatCnTot
*!             dBatPBeg , dBatPEnd
*!*************************************************************
*! Called from : lfFixBatch()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfPreData

PRIVATE lcFisYear , lcMinPrd , lcMaxPrd , lcBatchNo

*DO CASE Statment to get the Batch type
DO CASE
  *Case The transaction type is 'Y'
  CASE GLTRNSHD.cTrnType = 'Y'
    lcBatType = 'C'
  
  *Case The transaction type is 'B'
  CASE GLTRNSHD.cTrnType = 'B'
    lcBatType = 'B'
  
  *Otherwise
  OTHERWISE
    
    *IF the accounts in the Transactions lines for this Batch is
    *Statystical account
    IF SEEK(GLTRNSHD.cBatchNo , 'GLTRNSDT') .AND.;
       SEEK(GLTRNSDT.cAcctCode , 'GLACCHAR') .AND.;
       LEFT(GLACCHAR.cTypeCode , 1) = 'Y'
    
      lcBatType = 'S'
    ELSE    && Else
      lcBatType = 'N'
    ENDIF    && End of IF  
ENDCASE    && End of CASE Statment

lcBatchNo = cBatchNo      && Varible to hold the Batch number
lcFisYear = cTrnPYr       && Varible to hold the Fiscal year
lcMinPrd = cTrnPPrd       && Varible to hold the Min. Prioud
lcMaxPrd = cTrnPPrd       && Varible to hold the Max. Prioud
lnBatCnTot = 0

*SCAN Loop to scan the Posted transaction header for the current Batch
SCAN REST;
    WHILE cBatchNo = lcBatchNo
  lnCurRec = lnCurRec + 1
  lcMinPrd = MIN(lcMinPrd , cTrnPPrd)
  lcMaxPrd = MAX(lcMaxPrd , cTrnPPrd)
  lnBatCnTot = lnBatCnTot + nTrnTotDR
ENDSCAN    && End of SCAN Loop


*E301257,1 Start
** Use new file name (FSPRD) instead of old file name (SYCFSPRD)
*ldBatPBeg = IIF(SEEK(gcAct_Comp + lcFisYear + lcMinPrd , 'SYCFSPRD') ,;
                SYCFSPRD.dFspPBgDt , {})
*ldBatPEnd = IIF(SEEK(gcAct_Comp + lcFisYear + lcMaxPrd , 'SYCFSPRD') ,;
                SYCFSPRD.dFspPEnDt , {})

ldBatPBeg = IIF(SEEK(lcFisYear + lcMinPrd , 'FSPRD') ,;
                FSPRD.dFspPBgDt , {})

ldBatPEnd = IIF(SEEK(lcFisYear + lcMaxPrd , 'FSPRD') ,;
                FSPRD.dFspPEnDt , {})
*E301257,1 End
