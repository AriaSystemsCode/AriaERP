*:************************************************************************
*:
*: Procedure file: GLCASHF.PRG
*:
*:         System: ARIA BUSINESS SYSTEM
*:         Module: General Ledger
*:         Author: Albert Raif
*:      Copyright (c) 
*:  Last modified:  /  /
*:
*:  Procs & Fncts: 
*:               
*:      Documented   /  /
*:************************************************************************
*B600377,1 Reham On 06/03/95 Restore the right data if leave the screen
*B600377,1 & back again for the cash flow group popup & the radio button.
*
*B600435,1 _ Fix the error that was cut the files names in the array & search
*B600435,1   in the SQL for the file name with its extention.
*B600435,1 _ Change the way of searching in the files for cash flow we want
*B600435,1   to delete.
*E300683,5 AHMED 06/10/97 Add prgs directory path to the calling of programs
*E301077,71 (WAM) Walid Abou El-Magd 03/03/99 Open SYDFLFLD with gfSysOpen() 
*              and close it by gfCloseFile()
*              
*:************************************************************************
*
EXTERNAL ARRAY laData,laKeyField,laScrMode,laDefProc
DECLARE laKeyField[1,4] , laCfGroup[3,2] , laPhIName[1]

laKeyField[1,1] = 'laData[1]'
laKeyField[1,2] =.T.
laKeyField[1,3] = 'CFICODE'
laKeyField[1,4] = 1

*** Array to hold the activity groups name 
laCfGroup[1,1] = 'Operating activities'
laCfGroup[1,2] = 'O'
laCfGroup[2,1] = 'Investment activities'
laCfGroup[2,2] = 'I'
laCfGroup[3,1] = 'Financing activities'
laCfGroup[3,2] = 'F'

laDefProc      = .T.
laDefProc[9]   = .F.
laDefProc[7]   = .F.

*** group character  _ the first character of cash flow id
lcCfGroup      = 'O'

IF !gfSetup()
  RETURN
ENDIF

rbAdj = 1

IF !WEXIST(gcBaseWind)
  SCATTER FIELDS &lcScFields MEMO TO laData BLANK
ENDIF

*B600377,1 Reham On 06/03/95 Restore the right data if leave the screen
*B600377,1 and back again.
rbAdj     = IIF(laData[4] = "D" , 1 , 2 )
puCfgroup = IIF(AT(lcCfGroup,"OIF") > 0 , AT(lcCfGroup,"OIF"),0)

*E300683,5 Call *.SPR from screens directory
* DO glCashf.SPR 
DO (gcScrDir + gcWinAppl + '\glCashf.SPR')
*E300683,5 end   
*!**************************************************************************
*!
*!      Procedure: lpShow
*!
*!**************************************************************************
*
PROCEDURE lpShow

DO CASE
  *** Select mode
  CASE laScrMode[1]
    puCfGroup = 1            && IIF(BETWEEN(puCfGroup,1,3),puCfGroup,1)
    lcCfGroup = "O"          && laCfGroup[puCfGroup,2]
    laData[4] = "D"
    rbAdj     = 1
    SHOW GET rbAdj DISABLE
  *** View mode
  CASE laScrMode[2] .OR. laScrMode[3]
    rbAdj     = IIF(laData[4] = "D" , 1 , 2 )
    laData[1] = glcfitem.csegcaflo  && store the Original ID into laData[1]
    lcCfGroup = LEFT(laData[1],1)   && Divid the Original ID into Group character
    laData[1] = SUBSTR(laData[1],2) && And the number
    puCfGroup = 0                   && rest the popup bottom
    
    ***  Search for the group character to deispaly the popup bottom
    FOR lnCount  = 1 TO ALEN(laCfGroup,1)
      IF laCfGroup [lnCount,2] == lcCfGroup 
        puCfGroup = lnCount
        EXIT  
      ENDIF
    ENDFOR
    IF laScrMode[2]
      SHOW GET rbAdj DISABLE
    ELSE
      IF VAL(laData[1]) <= 50
        SHOW GET rbAdj DISABLE
      ELSE
        SHOW GET rbAdj ENABLE
      ENDIF
    ENDIF
  CASE laScrMode[4]
    IF VAL(SUBSTR(laData[1],2)) <= 50
      SHOW GET rbAdj DISABLE
    ELSE
      SHOW GET rbAdj ENABLE
    ENDIF
ENDCASE

*** check if the ID Smaller than 50  -- From 1 to 50 reseved by the system
IF VAL(laData[1]) <= 50
  SHOW GET pbDlt DISABLE    && disable the delete bottom
ELSE
  IF laScrMode[2]
    SHOW GET pbDlt ENABLE   && Enable the delete bottom
  ENDIF
ENDIF

*!**************************************************************************
*!
*!      Function : lfvCfGroup
*!
*!**************************************************************************
* Done after popup bottom pushed to control the group character

FUNCTION lfvCfGroup

lcCfGroup = laCfGroup[puCfGroup,2]

SHOW GET lcCfGroup

_CUROBJ = OBJNUM(laData[1])

*!**************************************************************************
*!
*!      Function: lfvData_1
*!
*!**************************************************************************
*
FUNCTION lfvData_1

IF !EMPTY(laData[1]) AND LASTKEY()= 13
  IF ATC("?",laData[1]) > 0 
    =gfBrows()
  ELSE
    IF ISDIGIT(laData[1])
      laData[1] = ALLTRIM(lcCfGroup+laData[1]) && make the whole Id
      =gfSeekRec()
      IF laScrMode[4]
        ** Check if the Id between 1 and 50
        IF !BETWEEN(VAL(SUBSTR(laData[1],2)),51,99)
          laData[1] = SPACE(FSIZE(csegcaflo))
          SHOW GET laData[1]
          *** Available lines for the ð activities section from 51 to 99
          =gfModalGen("INM02170B00000",'DIALOG',;
                      LOWER(laCfGroup[puCfGroup,1])+"|"+"51"+"|"+"99") 
          laScrMode    = .F.
          laScrMode[1] = .T.
          SHOW GETS
        ELSE
          laData[1] = SUBSTR(laData[1],2)
          SHOW GET laData[1]
          SHOW GET lcCfGroup DISABLE
        ENDIF
      ENDIF  
    ELSE
      ** You have to enter the item line number
      =gfModalGen("INM02186B00000",'DIALOG')   		
      laData[1] = " "
      _CUROBJ   = OBJNUM(laData[1])
    ENDIF
  ENDIF
ENDIF

*!**************************************************************************
*!
*!      PROCEDURE lfvAdj
*!
*!**************************************************************************
* 
FUNCTION lfvAdj

laData[4] = IIF(rbAdj = 1 , "D" , "A" )

*!**************************************************************************
*!
*!      PROCEDURE lpSavScr
*!
*!**************************************************************************
* Save the informaation
PROCEDURE lpSavScr

IF EMPTY(laData[2])
  *** you have to enter ð description
  =gfModalGen("INM02171B00000","DIALOG",IIF(puCFGroup=0,''," "+laCfGroup[puCfGroup,1])+" ")
  llcSave = .F.
  _CUROBJ = OBJNUM(laData[2])
  RETURN 
ENDIF

*** delete any space character
laData[1] = ALLTRIM(lcCfGroup+laData[1])
  
IF laScrMode[4]
  APPEND BLANK
ENDIF  

GATHER FROM laData FIELDS &lcScFields MEMO
=gfAdd_Info()

*!**************************************************************************
*!
*!      PROCEDURE: lpDelScr
*!
*!**************************************************************************
*
PROCEDURE lpDelScr

llUsed     = .F.
lnNOfFiles = 0
IF NOT USED("SYDFLFLD")
  SELECT 0
  *E301077,71 (WAM) Walid  [BEGIN] use gfSysOpen() instead of USE command
  *And get the value of llUsed as return value from the function . 
  *USE &gcSysHome.SYDFLFLD 
  llUsed = gfSysOpen(gcSysHome+'SYDFLFLD','','SH')
  *llUsed = .T.
  *E301077,71 (WAM) Walid  [END..]
ENDIF
SELECT SYDFLFLD

*** Select the files that used this field from sydflfld into array
*B600435,1 Remark this line because this SQL assumed that file name
*B600435,1 saved with it extention in file field dictionary.
*SELECT cFile_nam FROM SYDFLFLD WHERE ;
       cFld_name = "CSEGCAFLO";
       AND cFile_nam <> "GLCFITEM.DBF" ;
       GROUP BY cFile_nam ;
       INTO ARRAY laFiles

*B600435,1 Put the 2 cases in the SQL :_ file with exetension or 
*B600435,1 file without extension.
SELECT cFile_nam , .F. AS ln FROM SYDFLFLD WHERE ;
       cFld_name = "CSEGCAFLO" AND ;
       cFile_nam <> "GLCFITEM.DBF" AND ;
       cFile_nam <> "GLCFITEM" ;
       GROUP BY cFile_nam ;
       INTO ARRAY laFiles
 
*** Store the number of array  
lnNOfRec = _TALLY

IF llUsed
  *E301077 (WAM) Walid Abou El-Magd [BEGIN]
  * Close the file with gfCloseFile() instead of  USE IN command
  *USE IN   SYDFLFLD && if yes the close this file
  = gfCloseFile('SYDFLFLD')
  *E301077 (WAM) Walid Abou El-Magd [END..]
  llUsed = .F.
ENDIF

*** Check if the record information used by another files
FOR lnCount = 1 TO lnNOfRec
  IF EMPTY(laFiles[lnCount])
    LOOP
  ENDIF
  *** Store the alias of the file 
  laFiles[lnCount] = ALLTRIM(laFiles[lnCount])  
  *B600435,1 Remark this line because this command assumed that file name
  *B600435,1 saved with it extention in file field dictionary.
  *laFiles[lnCount] = STUFF(laFiles[lnCount],LEN(laFiles[lnCount])-3,4,'')
  laFiles[lnCount] = IIF(AT(".",laFiles[lnCount])>0,;
                     SUBSTR(laFiles[lnCount],1,AT(".",laFiles[lnCount])-1),;
                     laFiles[lnCount])

  IF NOT USED(laFiles[lnCount])
    SELECT 0
    *E301077 (WAM) Walid Abou El-Magd [BEGIN]
    *USE gcDataDir+laFiles[lnCount]
    IF LEFT(laFiles[lnCount],2) = 'SY'
      llUsed = gfSysOpen(gcSysHome+laFiles[lnCount],'','SH')
    ELSE
      llUsed = gfOpenFile(gcDataDir+laFiles[lnCount],'','')
    ENDIF
    *llUsed = .T.
    *E301077 (WAM) Walid Abou El-Magd [END..]
    SELECT (laFiles[lnCount])
  ENDIF
  
  SELECT (laFiles[lnCount])
  
  *** Search for the record which match the record information
  LOCATE FOR CSEGCAFLO = ALLTRIM(lcCfGroup+laData[1])
  *B600435,1 If the code found in the file.
  IF FOUND()
    lnNOfFiles = lnNOfFiles + 1  && the number of files that share info.
    
    *B600435,1 If the code found in the chart of accounts files.
    IF laFiles[lnCount] = "GLACCHAR"
      *** Store the account code that used the cash flow code
      *B600435,1 Concatinate the variable to display it in the message.
      lcAccNum =laFiles[lnCount]+".CACCTCODE"
      lcAccNum = "account "+ALLTRIM(&lcAccNum)+" in the chart of account"
    ELSE
      *B600435,1 If the code exist in any other file except chart of accounts.
      ***  Store the logical file name
      *B600435,1 Set flag to know if the file dictionary opend or not.
      llUsedFil = .F.
      *B600435,1 If file dictionary is not used open it, & set flag for that.
      IF NOT USED("SYDFILES")
        SELECT 0
        USE &gcSysHome.SYDFILES
        llUsedFil = .T.
      ENDIF
      
      SELECT SYDFILES
      DECLARE laPhIName[1]
      *B600435,1 Get the logical name for the file that have the cash 
      *B600435,1 flow code you want to delete.
      SELECT CFILE_TTL ;
      FROM SYDFILES ;
      WHERE CFILE_NAM = (laFiles[lnCount]);
      INTO ARRAY laPhiName
      
      *B600435,1 If the flag was .T. , it means that the file was not used before.
      IF llUsedFil
        SELECT SYDFILES
        USE
      ENDIF
      
      *B600435,1 Concatinate the variable to display it in the message.
      lcAccNum = "file "+laPhiName[1]
    ENDIF
    EXIT
  ENDIF
  *B600435,1 If the file was not used from before, close it.
  IF llUsed
    USE IN laFiles[lnCount]
    llUsed = .F.
  ENDIF
ENDFOR

IF NOT USED("GLCFITEM")
  USE &gcDataDir.GLCFITEM IN SELECT(1)
ENDIF

SELECT GLCFITEM   && select the original file
*** Check if there is any file share information
IF lnNOfFiles > 0
  *** Cash flow code ð is used by account ð in the chart of accounts file
  =gfModalGen("INM02173B00000","DIALOG",lcCfGroup+laData[1]+"|"+ALLTRIM(lcAccNum ))
  laScrMode    = .F.
  laScrMode[2] = .T.
  SHOW GETS
ELSE
  DELETE  
  laScrMode    = .F.
  laScrMode[1] = .T.
  SHOW GETS
ENDIF
