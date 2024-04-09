*!********************************************************************
*: Program file  : SMCRATE.PRG
*: Program desc. : IMPORTING EXCEL FILES TO SYSRATES FILE
*: For screen    : None.
*: System        : ARIA APPAREL SERIES
*: Module        : SYSTEM MANAGER
*: Developer     : KHALID MOHI EL-DIN . KHM
*: Converted from Aria26 By Nader Anis. (NAD)
*: Refere To     : E301365    
*!********************************************************************
*Steps to convert FrtRates files:
*1- Rename the Excel file to its corresponding ship type name. For example
*   1DA.CSV     will be  USUPSN.CSV
*   1DASAVER    will be  USUPST.CSV
*   2DAM        will be  USUPSE.CSV
*   2DA         will be  USUPS2.CSV
*   2DS         will be  USUPS3.CSV
*   GnComm      will be  USUPSG.CSV
*2- Delete all the rows in the Excel file till you find the row of the zone 
*   where the word "EXCEED" is found. Then if you find a word "Letter" 
*   delete its row. In other words the first row of the Excel file Should be
*   the row where the "Exceed" word is found (DO NOT CHANGE THIS ROW AT ALL)
*   then the second line should be the first weight without any dollar sign 
*   in front of each value ($ remove it if its found).
*3- Enter the path where the files are located => W:\Aria26\Work\		
*4-Enter the file name = > USUPSG
*5- We count the number of values between comma (,) then we check the files
*   that are equal in number of values between comma then we assign
*    lnMaxField to their numbers of values between comma
*6- We create a cursor with 10 fields as the maximum number of values 
*   between comma in the files that you sent is 10. So we create a 
*   cursor with 10 fields.
*:*************************************************************************
*:Modifications  :
* B603445,1 ASH 02/10/2000 Fix the bug of wrong saving weight.
* B605429,1 ABD 02-27-2002 Change the format of the ups files while converting the 
* B605429,1 ABD            Rates the Ups chnaged their downloaded excel files .
* B605429,1 ABD            we need to let our conver rate program to remove the 
* B605429,1 ABD            word "ZONE" from the line where the word "EXCEED" if found .
* E302135,1 AMH 03/31/2003 Update the nIniRate field.
*:*************************************************************************

* B605429,1 ABD - The files was change [Begin]
*-- UPS Ground         --> GNDCOMM.CSV  = USUPSG.csv
*-- UPS Next day       --> 1DA.CSV      = USUPSN.csv
*-- UPS Next day Saver --> 1DASAVER.CSV = USUPST.csv
*-- UPS Second day     --> 2DA.CSV      = USUPS2.csv
*-- UPS Second day AM  --> 2DAM.CSV     = USUPSE.csv
*-- UPS Third day      --> 3DSCOMM.CSV  = USUPS3.csv
* B605429,1 ABD - [End]

* B605429,1 ABD  [Begin]
lcOldPath = SPACE(40)
IF !gfSetup()
  RETURN
ENDIF  
* B605429,1 ABD [End]

IF _DOS
   CLEAR
   SET COLOR TO (qClrNrm)
   DO HEADER WITH 'IMPRRATES','IMPORTING FILES'
ENDIF



*-- Opening the Sysrates file in order to update it with the new Rates.
= gfOpenFile (gcSysHome+'SysRates','FrtRates','SH')
*-- Initializing the necessary variables.
lcPathName = SPACE(40)
lcFileName = SPACE(12)
lcTmpZone  = gfTempName()

IF _DOS OR _UNIX
   =gfSetClrSet()
ENDIF 

DO (gcScrDir+"SM\SMCRATE.SPX")


*!*************************************************************
*! Name      : lfvProceed
*! Developer : KHALID MOHI EL-DIN
*! Date      : 02/10/1999
*! Purpose   : To import the files in the SysRates file.
*!*************************************************************
*! Example            :  lfvProceed()
*!*************************************************************
FUNCTION lfvProceed

IF EMPTY(lcPathName)
  *Path name can not be empty.
  =gfModalGen('TRM04074B00000','DIALOG','Path name')
  _CUROBJ = OBJNUM(lcPathName)
  RETURN
ENDIF

* B605429,1 ABD - No Need to check on this variable. [Begin]
*IF EMPTY(lcFileName)
*  * File name can not be empty.
*  =gfModalGen('TRM04074B00000','DIALOG','File name')
*  _CUROBJ = OBJNUM(lcFileName)
*  RETURN
*ENDIF

lcPathName  = lcOldPath
*-- Check if File Is .Csv file.
IF !(".CSV" $ lcPathName)
    = gfModalGen('INM00389B00000','F','ALERT')
    RETURN
ENDIF

lnPathName = LEN(lcPathName)
lcFileName = ALLTRIM(SUBSTR(lcPathName_f,RAT('\',lcPathName_f)+1,lnPathName))
lcFileName = STRTRAN(lcFileName,".CSV","")
lcPathName = ALLTRIM(SUBSTR(lcPathName,1,RAT('\',lcPathName)))
* B605429,1 ABD - [End]


IF LEN(ALLTRIM(lcPathName)) = 1
  lcPathName = ALLTRIM(lcPathName) + ':'
ENDIF


lcPathName = ALLTRIM(lcPathName)
lcShipType = ALLTRIM(lcFileName)
lcFileName = ALLTRIM(lcFileName) + ".CSV"
lcPathName = IIF(RIGHT(lcPathName,1)<>"\",lcPathName+"\",lcPathName)



*-- To determine the number of fields in the file.
IF LEFT(lcFileName,6) = "USUPS2" OR LEFT(lcFileName,6) = "USUPSN"
  lnMaxField = 10
ELSE
  lnMaxField = 7
ENDIF  

IF !FILE(lcPathName+lcFileName)   
  * "File does not exist. Cannot proceed."
  =gfModalGen('TRM00273B00000','DIALOG')
  _CUROBJ = OBJNUM(lcFileName)
  RETURN
ENDIF

*-- Creating a cursor that will hold the converted data from the 
*-- excel file in order to append it in the master database file.
* B605429,1 ABD - The files was change so I will change the field weight only
* B605429,1 ABD - Increase the Field Weight. [Begin]

*CREATE CURSOR (lcTmpZone);
   (cWeight C(6), cRate1 C(6), cRate2 C(6), cRate3 C(6),;
    cRate4 C(6), cRate5 C(6), cRate6 C(6), cRate7 C(6), cRate8 C(6),;
    cRate9 C(6), cRate10 C(6))

CREATE CURSOR (lcTmpZone);
   (cWeight C(10), cRate1 C(10), cRate2 C(10), cRate3 C(10),;
    cRate4 C(10), cRate5 C(10), cRate6 C(10), cRate7 C(10), cRate8 C(10),;
    cRate9 C(10), cRate10 C(10))

* B605429,1 ABD - Increase the Field Weight. [End]
  
*-- Copying the excel file to the cursor.
APPEND FROM (lcPathName+lcFileName) DELIMITED WITH ,


* B605429,1 ABD - dlete all recored in the header file.[Begin]


*-- delete the same type if found.
SELECT SysRates
LOCATE 
IF !EOF() .AND. SEEK(lcShipType)
  DELETE REST WHILE shiptype+zone+cshpweight = lcShipType
ENDIF

*-- Delete the header of the Excel sheet.
llEXIT = .T.  && Variable Hold false if found needed character.
SELECT (lcTmpZone)
LOCATE
SCAN WHILE llEXIT
  *-- More check to don't delete all record if arrive to first recored 
  *--go out from this loop.
  IF ALLTRIM(cWeight) = '1'
    EXIT
  ENDIF
  
  *-- Delete all recored untill found word Weight.
  llEXIT = IIF('WEIGHT' $ UPPER(cWeight) ,.F.,llEXIT)
  IF llEXIT
    DELETE
  ENDIF
ENDSCAN
* B605429,1 ABD - [End]


SELECT (lcTmpZone)
GOTO TOP
FOR lnCounter = 1 TO lnMaxField
  lcCounter = ALLTRIM(STR(lnCounter,2))
  * B605429,1 ABD - Remove word Zone from the line where the word "WEIGHT" if found .  [Begin]
  *lcZone&lcCounter = cRate&lcCounter
  lcZone&lcCounter = ALLTRIM(STRTRAN(UPPER(cRate&lcCounter ),'ZONE',''))
  * B605429,1 ABD - [End]
ENDFOR


* B605429,1 ABD - dlete all recored in the header file.[Begin]
*-- Delete the header of the Excel sheet.
*SKIP
*SCAN REST
*- Then we will delete row hold word letter.
llEXIT = .T.
SELECT (lcTmpZone)

SCAN REST WHILE llEXIT
  *-- More check to don't delete all record if arrive to first recored 
  *--go out from this loop.
  IF ALLTRIM(cWeight) = '1'
    EXIT
  ENDIF

  IF ('LETTER' $ UPPER(cWeight))
    DELETE
    EXIT
  ELSE
    DELETE
  ENDIF
ENDSCAN
SELECT (lcTmpZone)
GOTO TOP

SCAN FOR !EMPTY(cWeight)
  * B605429,1 ABD - [End]

  FOR lnCount = 1 TO lnMaxField
    lcCount = ALLTRIM(STR(lnCount,2))
    WAIT WINDOW "Weight : "+ALLTRIM(&lcTmpZone..cWeight)+" Zone : " +;
                ALLTRIM(lcZone&lcCount) + " Rate : " +;
                ALLTRIM(&lcTmpZone..cRate&lcCount) NOWAIT
  
    SELECT Sysrates
    APPEND BLANK
    *B603445,1 ASH 02/10/2000 (Begin) Fix the bug of wrong saving weight.
    *REPLACE ShipType     WITH lcShipType                  ,;
            Zone         WITH ALLTRIM(lcZone&lcCount)     ,;
            cShpWeight   WITH ALLTRIM(&lcTmpZone..cWeight),;
            cFrtRate     WITH VAL(&lcTmpZone..cRate&lcCount)
    
    *E302135,1 AMH Update the nIniRate field [Start]
    *REPLACE ShipType     WITH lcShipType                  ,;
            Zone         WITH ALLTRIM(lcZone&lcCount)     ,;
            cShpWeight   WITH PADL(ALLTRIM(&lcTmpZone..cWeight),6,' '),;
            cFrtRate     WITH VAL(&lcTmpZone..cRate&lcCount)
    REPLACE ShipType     WITH lcShipType                  ,;
            Zone         WITH ALLTRIM(lcZone&lcCount)     ,;
            cShpWeight   WITH PADL(ALLTRIM(&lcTmpZone..cWeight),6,' '),;
            cFrtRate     WITH VAL(&lcTmpZone..cRate&lcCount),;
            nIniRate     WITH cFrtRate
    *E302135,1 AMH [End]
    
    *B603445,1 ASH 02/10/2000 (End) 
  ENDFOR
ENDSCAN
WAIT CLEAR
ERASE(lcTmpZone)
=gfModalGen('TRM42219B00000','DIALOG','Converte')
*:*************************************************************
*: Name      : lfvgetFile
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 02/27/2002
*: Purpose   : Function to get the Excel file data Dir & File.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : lcVarNam = ladata [13]
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : = lfvApData ()
*:*************************************************************
* B605429,1 ABD - [Begin]
FUNCTION lfvgetFile
PARAMETERS lcVarNam

lcPathName  = GETFILE('CSV', 'Excel sheet Path : ','Select') 

lcOldPath = lcPathName

IF EMPTY(lcPathName)
  SHOW GET PbProceed DISABLE
ELSE
  SHOW GET PbProceed ENABLE
ENDIF
SHOW GET lcPathName

*-- End Of lfvgetFile
* B605429,1 ABD - [End]
*:*************************************************************
