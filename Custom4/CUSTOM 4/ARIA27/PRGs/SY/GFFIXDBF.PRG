*!***************************************************************************
*! Name      : gfFixDbf
*! Developer : Walid Abou El-Magd  (WAM)
*! Date      : 09/24/1999
*! Purpose   : Function to automaticlly fix any corrupted DBF in ARIA27 system.
*! Job ID    : E# 301316,1
*!***************************************************************************
*! Calls     : None
*!***************************************************************************
*! Return    : None
*!***************************************************************************
*! Passed Parameters  : laFixInfo  : Pointer to the fix information array.
*! Notes  : This charachter array consistes of 10 cells 
*!        : The programmer fill the first 5 , And the function return the rest.
*!        : laFixInfo[1] = The full path of the file to be fixed...
*!
*!        : laFixInfo[2] = What do you want the function to do 
*!                         "F"-->Auto Fix & "A"-->Ask befor fixing & "D" -> Don't fix
*!
*!                         [if not passed "A" will be the default]
*!
*!        : laFixInfo[3] = Do you want to get detail information about the problems found in this file?
*!                         "T"=Yes & "F"=No [if not passed "F" will be the default]
*!
*!        : laFixInfo[4] = Do you want to make a backup for this file befor fixing?
*!                         "T"=Yes & "F"=No & "A"=Ask befor backup
*!                         [if not passed "A" will be the default]
*!
*!        : laFixInfo[5] = Do you want to overwrite the last backup?
*!                         "T"=Yes & "F"=No & "A"=Ask[if not passed "A" will be the default]
*!
*!        : This function return its information in the next elementes of array 
*!
*!        laFixInfo[6]  -> File Check State one of the following strings
*!                         "FILE NOT EXIST"    , "FILE ACCESS DENIED"
*!                         "NO PROBLEMS FOUND" , "PROBLEMS FOUND"
*!        laFixInfo[7]  -> Disk Space report
*!                         " ","Insufficient Disk Space"
*!        laFixInfo[8]  -> Backup report
*!                         " ","Canceled By User","<File name of backup>"
*!        laFixInfo[9]  -> Problems report [all problems found in the file]
*!        laFixInfo[10] -> File report [Fixed] or [Not Fixed]
*!***************************************************************************
*! Example   : DIMENSION laFixInfo[10]
*!             laFixInfo=" "
*!             laFixInfo[1]=lcDataDir+lcFileName
*!             laFixInfo[2]="F"        &&--Auto Fix
*!             laFixInfo[3]="T"        &&--Details
*!             laFixInfo[4]="A"        &&--Ask befor backup
*!             laFixInfo[5]="A"        &&--Ask if overwrite the last backup?
*!             = gfFixDbf(@laFixInfo)
*!***************************************************************************
*! *B603943,1 HBG 10/12/2000 The free disk space without using windows API [Begin]
*!***************************************************************************
FUNCTION gfFixDbf
PARAMETERS laFixInfo
PRIVATE llAutoFix,llDontFix,llGetInfo,llBackup,llBackAsk,llBackOver,llOverAsk,lnChkVal
STORE .F. TO llAutoFix,llDontFix,llGetInfo,llBackup,llBackAsk,llBackOver,llOverAsk            
STORE  0  TO lnChkVal
*-- 
IF TYPE("laFixInfo")# "C" OR EMPTY(laFixInfo[1]) OR !(UPPER(LEFT(ALLTRIM(laFixInfo[2]),1))$"FAD")  
  RETURN 
ENDIF
*-- Set the required libraries
*B603943,1 HBG 10/12/2000 Set the libarary if it not setting [Begin]
*SET LIBRARY TO FULLPATH("")+"FOXTOOLS.FLL" ADDITIVE
*SET LIBRARY TO FULLPATH("")+"FFIX4_25.FLL" ADDITIVE
PRIVATE lcSetLibr
lcSetLibr = SET("LIBRARY")
IF !(SYS(2004)+"FOXTOOLS.FLL" $ lcSetLibr) 
  SET LIBRARY TO (SYS(2004)+"FOXTOOLS.FLL") ADDITIVE
ENDIF
IF !(SYS(2004)+"FOXTOOLS.FLL" $ lcSetLibr) 
  SET LIBRARY TO (SYS(2004)+"FFIX4_25.FLL") ADDITIVE
ENDIF
*B603943,1 [End]

FOR lnX= 1 TO 10
  IF lnX <=5
    STORE UPPER(ALLTRIM(laFixInfo[lnX])) TO laFixInfo[lnX]
  ELSE
    STORE SPACE(0) TO laFixInfo[lnX]
  ENDIF  
ENDFOR

*-- Add the extension (.DBF) to the full file name .
*-- It is not important If this file name passed with or without extension.
laFixInfo[1]=FORCEEXT(laFixInfo[1],"DBF")

*-- Check if the file is phyiscally exist on the disk
IF !FILE(laFixInfo[1])
  laFixInfo[6]="FILE NOT EXIST"
  *B603943,1 HBG 10/12/2000 Release the libarary if it not setting by another prog.[Begin]
  *RELEASE LIBRARY FULLPATH("")+"FOXTOOLS.FLL" 
  *RELEASE LIBRARY FULLPATH("")+"FFIX4_25.FLL" 
  = LfRelLibr()
  *B603943,1 [End]
  RETURN
ENDIF

*-- Set the control variable llAutoFix if not passed default it to Ask
IF !LEFT(laFixInfo[2],1)$"FAD"
  laFixInfo[2]="A"
  llAutoFix=.F.

ELSE
  llAutoFix=(LEFT(laFixInfo[2],1)="F")
  llDontFix=(LEFT(laFixInfo[2],1)="D")
ENDIF


*-- If detail parameter is not passed default it to "F"=NO.
IF !LEFT(laFixInfo[3],1)$"TF"
  laFixInfo[3]="F"
ELSE
  llGetInfo=(LEFT(laFixInfo[3],1) = "T" )
ENDIF

*-- If backup parameter is not passed default it to "A"=Ask befor backup 

IF !LEFT(laFixInfo[4],1)$"TFA"
  laFixInfo[4]= "A"
  STORE .T. TO llBackAsk
ELSE
  llBackup    = LEFT(laFixInfo[4],1) = "T" 
  llBackAsk   = LEFT(laFixInfo[4],1) = "A" 
ENDIF

*-- If what about last backup parameter is not passed default it to "A"=Ask.
IF !LEFT(laFixInfo[5],1)$"TFA" OR LEFT(laFixInfo[5],1)$"A"
  *-- In this case the variable llBackOver will be set or reset by the user
  laFixInfo[5]="A"
  llOverAsk   =.T.
ELSE
  llBackOver=LEFT(laFixInfo[5],1) = "T" 
ENDIF

*-- Now it is the time to check 
lnChkVal=FIXDBF(laFixInfo[1],0)

DO CASE
  CASE lnChkVal <= 0
    laFixInfo[6]=IIF(lnChkVal=0,"NO PROBLEMS FOUND","FILE ACCESS DENIED")
    *B603943,1 HBG 10/12/2000 Release the libarary if it not setting by another prog.[Begin]
    *RELEASE LIBRARY FULLPATH("")+"FOXTOOLS.FLL" 
    *RELEASE LIBRARY FULLPATH("")+"FFIX4_25.FLL" 
    = LfRelLibr()
    *B603943,1 [End]
    RETURN    
  CASE lnChkVal > 0
    laFixInfo[6]="PROBLEMS FOUND"
ENDCASE

*-- if detail information required
IF llGetInfo
  DO WHILE lnChkVal > 0
    DO CASE 
	  CASE lnChkVal >= 64
	    laFixInfo[9]=laFixInfo[9]+'   * Memo Non-Standard Block size |'
	    lnChkVal = lnChkVal - 64
	  CASE lnChkVal >= 32
	    laFixInfo[9]=laFixInfo[9]+'   * Memo Next Available Block incorrect |'
        lnChkVal = lnChkVal - 32
	  CASE lnChkVal >= 16
	    laFixInfo[9]=laFixInfo[9]+'   * CDX flag incorrect |'
	    lnChkVal = lnChkVal - 16
      CASE lnChkVal >= 8
	    laFixInfo[9]=laFixInfo[9]+'   * DBF type ID incorrect |'
	    lnChkVal = lnChkVal - 8
	  CASE lnChkVal >= 4
	    laFixInfo[9]=laFixInfo[9]+'   * Record count incorrect |'
	    lnChkVal = lnChkVal - 4
	  CASE lnChkVal >= 2
	    laFixInfo[9]=laFixInfo[9]+'   * Record length incorrect |'
	    lnChkVal = lnChkVal - 2  
	  CASE lnChkVal >= 1
	    laFixInfo[9]=laFixInfo[9]+'   * Header length incorrect |'
	    lnChkVal = lnChkVal - 1
    ENDCASE
  ENDDO		
ENDIF
*-- If must ask fix or not....
*-- Do you want to fix the file (XXX)?
IF llDontFix OR (!llAutoFix AND gfModalGen('QRM00357B00006','Dialog',laFixInfo[1])=2)
  laFixInfo[10] = "Not Fixed"
  *B603943,1 HBG 10/12/2000 Release the libarary if it not setting by another prog.[Begin]
  *RELEASE LIBRARY FULLPATH("")+"FOXTOOLS.FLL" 
  *RELEASE LIBRARY FULLPATH("")+"FFIX4_25.FLL" 
  = LfRelLibr()
  *B603943,1 [End]
  RETURN    
ENDIF
*--the next code must be in case of fix,Either forced or user-choice .


IF llBackAsk 
  *-- All the next calculations must be done before the message since we
  *-- Need the size in this message.
  PRIVATE lnFileSize
  lnFileSize=0
  =ADIR(laFileSize,laFixInfo[1])    &&-- size of .DBF
  lnFileSize=laFileSize[1,2]
  laFixInfo[1]=FORCEEXT(laFixInfo[1],"CDX")
  IF FILE(laFixInfo[1])
    =ADIR(laFileSize,laFixInfo[1])    &&-- size of .CDX
    lnFileSize=lnFileSize+laFileSize[1,2]
  ENDIF  
  laFixInfo[1]=FORCEEXT(laFixInfo[1],"FPT")
  IF FILE(laFixInfo[1])
    =ADIR(laFileSize,laFixInfo[1])    &&-- size of .CDX
    lnFileSize=lnFileSize+laFileSize[1,2]
  ENDIF  
  *-- Restore the original extension.
  laFixInfo[1]=FORCEEXT(laFixInfo[1],"DBF") 
  *-- Make a backup for XXX ?
  *-- Total Size nnnnnnn Byets
  *-- <Yes> <No>
  llBackup = IIF(gfModalGen('QRM00358B00006','Dialog',laFixInfo[1]+'|'+STR(lnFileSize))=2,.F.,.T.)
  *-- Report the backup information...
  laFixInfo[8]=IIF(llBackup,"","Canceled By User")
ENDIF

*-- If backup required
IF llBackup 
  *B603943,1 HBG 10/12/2000 Add Variabel "lcFreeSpac" to get the free disk space [Begin]
  *PRIVATE lcBakupExt,lcLastBakup,lcExt,lnOption,lnFileSize,;
          laFileSize,lnDrivSize,laBackSize,lnBackSize,;
          lnSecPClus,lnBytPSect,lnFreeClus,lnTotClus
  *STORE SPACE(0) TO lcBakupExt,lcLastBakup

  PRIVATE lcBakupExt,lcLastBakup,lcExt,lnOption,lnFileSize,;
          laFileSize,lnDrivSize,laBackSize,lnBackSize,;
          lnSecPClus,lnBytPSect,lnFreeClus,lnTotClus,lcFreeSpac,lcConsSet  

  STORE SPACE(0) TO lcBakupExt,lcLastBakup,lcFreeSpac,lcConsSet
  *B603943,1 [End]

  STORE  0       TO lnFileSize,lnBackSize,lnDrivSize,lnSecPClus,lnBytPSect,lnFreeClus,lnTotClus
  STORE  1       TO lnOption
  
  *-- Get the file size...[Start]

  *-- Note that: we can't use wildcard characters (* and ?) to get all needed
  *-- Information in only one call to ADIR() to avoid selection old backups 
  *-- For this file .
  =ADIR(laFileSize,laFixInfo[1])    &&-- size of .DBF
  lnFileSize=laFileSize[1,2]
  laFixInfo[1]=FORCEEXT(laFixInfo[1],"CDX")
  IF FILE(laFixInfo[1])
    =ADIR(laFileSize,laFixInfo[1])    &&-- size of .CDX
    lnFileSize=lnFileSize+laFileSize[1,2]
  ENDIF  
  laFixInfo[1]=FORCEEXT(laFixInfo[1],"FPT")
  IF FILE(laFixInfo[1])
    =ADIR(laFileSize,laFixInfo[1])    &&-- size of .FPT
    lnFileSize=lnFileSize+laFileSize[1,2]
  ENDIF  
  *-- Restore the original extension.
  laFixInfo[1]=FORCEEXT(laFixInfo[1],"DBF") 
  
  *-- Get the file size...[End..]
  
  *-- Get the disk free space [Start]
  *-- Fox Help Said That :
  *-- On some networks, the value returned by the fox function DISKSPACE()  
  *-- may not be accurate for large network drives.
  *-- So I will use the windows API to get the free disk space.
  
  *B603943,1 HBG 10/12/2000 Get The free disk space without using windows API [Begin]
  *lfGetFree=REGFN("GetDiskFreeSpace",'C@L@L@L@L','')
  *=CALLFN(lfGetFree,JUSTDRIVE(laFixInfo[1]),@lnSecPClus,@lnBytPSect,@lnFreeClus,@lnTotClus)
  *lnDrivSize=lnSecPClus*lnBytPSect*lnFreeClus

  lcConsSet = SET('CONS')
  SET CONS OFF
  DIR LIKE *.XXX TO FILE (gcWorkDir+'Tmpspace.TXT')
  SET CONS &lcConsSet
  CREATE CURSOR TMPSPCE (mTxt M)
  APPEND BLANK
  APPEND MEMO TMPSPCE.mTxt FROM (gcWorkDir+'Tmpspace.TXT')
  lcFreeSpac = SUBSTR(MLINE(TMPSPCE.mTxt,;
  ATCLINE("BYTES REMAINING",UPPER(TMPSPCE.mTxt))),1,;
  ATC("BYTES REMAINING",MLINE(TMPSPCE.mTxt,ATCLINE("BYTES REMAINING",UPPER(TMPSPCE.mTxt))))-1)
  lnDrivSize= ROUND((VAL(lcFreeSpac)/1024/1024),1)
  *B603943,1 [End]
  
  laFixInfo[7]=IIF(lnDrivSize > lnFileSize," ","Insufficient Disk Space")
  *-- Get the disk free space [End..]
  
  *-- Get a valid new backup extention [Start]
  
  FOR lnX = 1 to 99
    IF !FILE(FORCEEXT(laFixInfo[1],"D"+IIF(lnX < 10 ,'0','')+ALLTRIM(STR(lnX))))
      lcBakupExt="D"+IIF(lnX < 10 ,'0','')+ALLTRIM(STR(lnX))
      EXIT
    ENDIF
  ENDFOR

  DO CASE
    *-- Case Maximum number of backups encountered
    CASE EMPTY(lcBakupExt)      
      *-- If llBackOver it's ok , if not I will inform the user that I can't
      *-- Make a new backup <Overwrite>,<Fix without backup>,<Cancel>
      IF llOverAsk OR !llBackOver 
        PRIVATE lnDummy
        *-- Get information about the ".?99" 
        =ADIR(laBackSize,FORCEEXT(laFixInfo[1],'?99'))
        *-- Max number of backups encounterd
        *-- Name :XXX.D99
        *-- Date :XX\XX\XX
        *-- <Overwrite>,<Skip backup>,<Cancel> NEW B
        lnDummy = gfModalGen('QRM00361B00036','Dialog','Maximum number of backups encountered'+'|'+FORCEEXT(laFixInfo[1],'D99')+'|'+DTOC(laBackSize[1,3]))
        DO CASE
          CASE lnDummy=1
            lcBakupExt="D99"
          CASE lnDummy=2                 
            *-- Report the backup state
            laFixInfo[8] ="Canceled By User"          
            lcBakupExt=" "
          CASE lnDummy=3
            *B603943,1 HBG 10/12/2000 Release the libarary if it not setting by another prog.[Begin]  
            *RELEASE LIBRARY FULLPATH("")+"FOXTOOLS.FLL" 
            *RELEASE LIBRARY FULLPATH("")+"FFIX4_25.FLL"
            = LfRelLibr()
            *B603943,1 [End] 
            *-- Report the backup state
            laFixInfo[8] ="Canceled By User"
            *-- Report the File state
            laFixInfo[10]="Not Fixed"
            RETURN    
        ENDCASE
      ELSE  &&-- this else means Auto-overwrite is the passed parameter
        *-- Adjust the the backup name we will overwrite it from empty to "D99"
        lcBakupExt="D99"
      ENDIF         
    CASE lcBakupExt # "D01"      
      *-- To get a date for the message
      =ADIR(laBackSize,FORCEEXT(laFixInfo[1],"D"+IIF(VAL(SUBSTR(lcBakupExt,2))-1 < 10 ,'0',SPACE(0))+ALLTRIM(STR(INT(VAL(SUBSTR(lcBakupExt,2))-1),2))))
      IF llOverAsk
        *-- Over write the last backup (XXX) ?
        *-- Date  : XX/XX/XX     <Yes>,<No>
        llBackOver=IIF(gfModalGen('QRM00359B00006','Dialog',laBackSize[1,1]+'|'+DTOC(laBackSize[1,3]))=1,.T.,.F.)
      ENDIF
      IF llBackOver 
        *-- This line give me the ext of the last backup 
        lcBakupExt = "D"+IIF(VAL(SUBSTR(lcBakupExt,2))-1 < 10 ,'0',SPACE(0)) +ALLTRIM(STR(INT(VAL(SUBSTR(lcBakupExt,2))-1),2))
      ENDIF
  ENDCASE

  *-- Now we have a lcBakupExt maybe old (to overwrite) maybe new (to creat)
  *-- We don't know till now , But we can .
  IF !EMPTY(lcBakupExt)  &&-- May go empty in CASE lnDummy=2 from case EMPTY(lcBakupExt)
    IF FILE(FORCEEXT(laFixInfo[1],lcBakupExt)) &&-- this an overwrite case
      =ADIR(laBackSize,FORCEEXT(laFixInfo[1],STRTRAN(lcBakupExt,'D','?')))
      FOR I=1 TO ALEN(laBackSize,1)
        lnBackSize=lnBackSize+laBackSize[I,2]
      ENDFOR
      *-- Insufficient Disk Space , Fix without backup? <Fix>,<Cancel>
      IF lnDrivSize+lnBackSize < lnFileSize AND gfModalGen('QRM00360B00037','Dialog')=2
        laFixInfo[7] ="Insufficient Disk Space"
        laFixInfo[10]="Not Fixed"
        *B603943,1 HBG 10/12/2000 Release the libarary if it not setting by another prog.[Begin]
        *RELEASE LIBRARY FULLPATH("")+"FOXTOOLS.FLL" 
        *RELEASE LIBRARY FULLPATH("")+"FFIX4_25.FLL" 
        = LfRelLibr()
        *B603943,1 [End]
        RETURN              
      ENDIF
    ELSE &&-- Creat new backup
      IF lnDrivSize < lnFileSize 
        =ADIR(laBackSize,FORCEEXT(laFixInfo[1],"D"+IIF(VAL(SUBSTR(lcBakupExt,2))-1 < 10 ,'0',SPACE(0))+ALLTRIM(STR(INT(VAL(SUBSTR(lcBakupExt,2))-1),2))))
        FOR I=1 TO ALEN(laBackSize,1)
          lnBackSize=lnBackSize+laBackSize[I,2]
        ENDFOR        
        IF lnDrivSize+lnBackSize < lnFileSize
          *-- Insufficient Disk Space , Fix without backup? <Fix>,<Cancel>
          IF gfModalGen('QRM00360B00037','Dialog')=2
            laFixInfo[7] ="Insufficient Disk Space"
            laFixInfo[10]="Not Fixed"
            *B603943,1 HBG 10/12/2000 Release the libarary if it not setting by another prog.[Begin]
            *RELEASE LIBRARY FULLPATH("")+"FOXTOOLS.FLL" 
            *RELEASE LIBRARY FULLPATH("")+"FFIX4_25.FLL" 
            = LfRelLibr()
            *B603943,1 [end]
            RETURN                          
          ENDIF
        ELSE
          *--Insufficient Disk Space,Overwrite the last backup?
          *-- Name : XXX
          *-- Date : XX/XX/XX   <Overwrite>,<Skip>,<Cancel>
          lnDummy=gfModalGen('QRM00361B00036','Dialog','Insufficient Disk Space,Overwrite the last backup?'+'|'+laBackSize[1,1]+'|'+DTOC(laBackSize[1,3]))
          DO CASE
            CASE lnDummy=1
              lcBakupExt = "D"+IIF(VAL(SUBSTR(lcBakupExt,2))-1 < 10 ,'0',SPACE(0)) +ALLTRIM(STR(INT(VAL(SUBSTR(lcBakupExt,2))-1),2))
            CASE lnDummy=2
              lcBakupExt = " "
              laFixInfo[8]="Canceled By User" 
            CASE lcBakupExt = 3
              laFixInfo[7] ="Insufficient Disk Space"
              laFixInfo[10]="Not Fixed"
              *B603943,1 HBG 10/12/2000 Release the libarary if it not setting by another prog.[Begin]
              *RELEASE LIBRARY FULLPATH("")+"FOXTOOLS.FLL" 
              *RELEASE LIBRARY FULLPATH("")+"FFIX4_25.FLL" 
              = LfRelLibr()
              *B603943,1 [End]
              RETURN                                        
          ENDCASE
        ENDIF
      ENDIF
    ENDIF
  ENDIF

  IF !EMPTY(lcBakupExt)
    laFixInfo[8]=FORCEEXT(laFixInfo[1],lcBakupExt)
    COPY FILE &laFixInfo[1] TO &laFixInfo[8]
    laFixInfo[1]=FORCEEXT(laFixInfo[1],"CDX")
    laFixInfo[8]=STRTRAN(laFixInfo[8],'.D','.C')
    IF FILE(laFixInfo[1])
      COPY FILE &laFixInfo[1] TO &laFixInfo[8]
    ENDIF  
    laFixInfo[1]=FORCEEXT(laFixInfo[1],"FPT")
    laFixInfo[8]=STRTRAN(laFixInfo[8],'.C','.F')
    IF FILE(laFixInfo[1])
      COPY FILE &laFixInfo[1] TO &laFixInfo[8]
    ENDIF  
    laFixInfo[1]=FORCEEXT(laFixInfo[1],"DBF")
    laFixInfo[8]=STRTRAN(laFixInfo[8],'.F','.D')  
  ENDIF
  
ENDIF &&-- llBackup   
=FIXDBF(laFixInfo[1],1,64)
SELECT 0
USE &laFixInfo[1] EXCLUSIVE
REINDEX
USE
laFixInfo[10]="Fixed"
*B603943,1 HBG 10/12/2000 Release the libarary if it not setting by another prog.[Begin]
*RELEASE LIBRARY FULLPATH("")+"FOXTOOLS.FLL" 
*RELEASE LIBRARY FULLPATH("")+"FFIX4_25.FLL" 
= LfRelLibr()
*B603943,1 [End]
RETURN                                      



*!*************************************************************
*! Name      : LfRelLibr
*! Developer : Hend Ghanem (HBG)
*! Date      : 10/12/2000
*! Purpose   : Release the libarary if it not setting by another prog.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : = LfRelLibr()
*!*************************************************************
FUNCTION LfRelLibr

IF !(SYS(2004)+"FOXTOOLS.FLL" $ lcSetLibr)
  RELEASE LIBRARY (SYS(2004)+"FOXTOOLS.FLL")
ENDIF  
IF !(SYS(2004)+"FOXTOOLS.FLL" $ lcSetLibr)
  RELEASE LIBRARY (SYS(2004)+"FFIX4_25.FLL" )
ENDIF
*-- End of LfRelLibr