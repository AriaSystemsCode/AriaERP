*!********************************************************************
*: Program file  : SMFRTZ.PRG 
*: Program desc. : IMPORTING EXCEL FILES TO SysZones FILE
*: System: ARIA APPAREL SERIES
*: Module: SYSTEM MANAGER
*: Developer: Nader Anis Mitry   CONVERTED FROM 26 IMPFRTZN.PRG
*:Refere To : E301365 
*!********************************************************************


*-- Opening the SysZones file in order to update it with the new Zones.


= gfOpenFile (gcSysHome+'SysZones',"Frtzones",'SH')


*-- Initializing the necessary variables.
lcPathName = SPACE(40)
lcPathNam  = SPACE(40)
lcFrom     = '004'
lcTo       = '994'
llDone     = .F.

*-- Declaring array to hold the name of the zones.
DECLARE laZonesNam [6]
laZonesNam[1] = 'USUPSG '   && Ground
laZonesNam[2] = 'USUPS3 '   && 3 Day Select
laZonesNam[3] = 'USUPS2 '   && 2nd Day Air
laZonesNam[4] = 'USUPSE '   && 2nd Day Air A.M. 
laZonesNam[5] = 'USUPST '   && Next Day Air Saver
laZonesNam[6] = 'USUPSN '   && Next Day Air

lcTmpZone=gfTempName()

DO (gcScrDir+"SM\SMCFRTZ.SPX")
*-- Close the databases.
IF USED ('SysZones')
  USE IN SysZones
ENDIF  
*!*************************************************************
*! Name      : lfvProceed
*! Developer : KHALD MOHI EL-DIN
*! Date      : 03/05/1998
*! Purpose   : To convert the excel files into SysZones DataBase file.
*!********************************************************************
*! Example            :  lfvProceed()
*!********************************************************************
FUNCTION lfvProceed
IF EMPTY(lcPathName)
  =lfvPath()
  RETURN
ENDIF  
*-- If one of the range was empty.
IF EMPTY(lcFrom) OR EMPTY(lcTo)

  *--- Range cannot be empty 
  =gfModalGen('TRM00368B00000','DIALOG')
  IF EMPTY(lcFrom)
    _CUROBJ = OBJNUM(lcFrom)
  ELSE
    _CUROBJ = OBJNUM(lcTo)
  ENDIF  
  RETURN
ENDIF

*-- If the low range is greater than the high range.
IF lcFrom > lcTo
  *-- Shipper Code range error. Cannot proceed.
  =gfModalGen('TRM00369B00000','DIALOG')
  _CUROBJ = OBJNUM(lcFrom)
  RETURN
ENDIF

IF LEN(ALLTRIM(lcPathName)) = 1
  lcPathName = ALLTRIM(lcPathName) + ':'
ENDIF

IF RIGHT(ALLTRIM(lcPathName),1) <> '\'
  lcPathName = ALLTRIM(lcPathName) + '\'
ENDIF                                       

lnSFrom = INT(VAL(lcFrom))
lnSTo   = INT(VAL(lcTo))

*-- Loop for handling all the EXCEL files.
FOR lnFilCount = lnSFrom TO lnSTo

  llOk = .T.
  lcFileName = PADL(lnFilCount,3,'0')
  lcFile     = lcFileName 
  lcFileName = lcFileName + '.CSV'
  lcFileName = ALLTRIM(lcPathName) + lcFileName
  
  *-- Checking if the file exists or not.
  IF FILE(lcFileName)
    llDone = .T.
    *-- Creating a cursor that will hold the converted data from the 
    *-- excel file in order to append it in the master database file.
    CREATE CURSOR &lcTmpZone;
       (lcShipTo C(15), lcZone1 C(3), lcZone2 C(3), lcZone3 C(3),;
        lcZone4 C(3), lcZone5 C(3), lcZone6 C(3))
  
    *-- Copying the excel file to the cursor.
    APPEND FROM &lcFileName DELIMITED WITH ,
  
    *-- Setting this filter in order to ignor unnecessary records.
    SET FILTER TO !EMPTY(lcShipTo)
  
    *-- Looping on the cursor and adding the record in the semi-master database.
    SCAN
      
      *-- Ignoring all the records before the one that its lcShipTo 
      *-- field = 'Dest. ZIP' Or the records that have an empty lcShipTo.
      IF llOk
        IF ALLTRIM(lcShipTo) <> 'Dest. ZIP'
          LOOP
        ELSE
          llOk = .F.  
        ENDIF
      ENDIF
      
      IF ALLTRIM(lcShipTo) <> 'Dest. ZIP'

        *-- If the there is a range such as (004-005) then split it
        *-- into two records 1 record for 004 and the other for 005.
        IF OCCURS('-',lcShipTo) > 0
          lnFrom = INT(VAL(LEFT(ALLTRIM(lcShipTo),3)))
          lnTo   = INT(VAL(RIGHT(ALLTRIM(lcShipTo),3)))
        
          *-- Outer loop for the lcShipTo when its a range (004-005).
          FOR lnCounter = lnFrom TO lnTo
            lcCounter   = PADL(lnCounter,3,'0')
            WAIT WINDOW 'ShipTo Zip Code : '+lcCounter NOWAIT
            *-- Inner loop for the number of columns. 1 record for 
            *-- each column.
            FOR lnCount = 1 TO 6
              lcCount = STR(lnCount,1)
              IF !EMPTY(&lcTmpZone..lcZone&lcCount) AND VAL(&lcTmpZone..lcZone&lcCount) > 0
                SELECT Syszones
                IF !SEEK(laZonesNam[lnCount]+lcFile+lcCounter)
                  APPEND BLANK
                  REPLACE ShipType WITH laZonesNam[lnCount],;
                          ShipFrom WITH lcFile             ,;
                          ShipTo   WITH lcCounter          ,;
                          Zone     WITH IIF(laZonesNam[lnCount]='USUPST ',;
                                        'T'+RIGHT(&lcTmpZone..lcZone&lcCount,2),;
                                        &lcTmpZone..lcZone&lcCount)
                ELSE
                  REPLACE Zone WITH IIF(laZonesNam[lnCount]='USUPST ',;
                                        'T'+RIGHT(&lcTmpZone..lcZone&lcCount,2),;
                                        &lcTmpZone..lcZone&lcCount)
                ENDIF
              ENDIF          
            ENDFOR
          ENDFOR
        
        *-- If there is no range in the lcShipTo such as (004)  
        ELSE          
          WAIT WINDOW 'ShipTo Zip Code : '+&lcTmpZone..lcShipTo NOWAIT        
          *-- Loop for the number of columns. 1 record for 
          *-- each column.          
          FOR lnCount = 1 TO 6
            lcCount   = STR(lnCount,1)
            lcCounter = PADL(lnCount,3,'0')
            lcShip    = PADL(ALLTRIM(&lcTmpZone..lcShipTo),3,'0')
            IF !EMPTY(&lcTmpZone..lcZone&lcCount) AND VAL(&lcTmpZone..lcZone&lcCount) > 0
              SELECT SysZones
              IF !SEEK(laZonesNam[lnCount]+lcFile+lcShip)
                APPEND BLANK
                REPLACE ShipType WITH laZonesNam[lnCount],;
                        ShipFrom WITH lcFile             ,;
                        ShipTo   WITH lcShip             ,;
                        Zone     WITH IIF(laZonesNam[lnCount]='USUPST ',;
                                      'T'+RIGHT(&lcTmpZone..lcZone&lcCount,2),;
                                      &lcTmpZone..lcZone&lcCount)
              ELSE
                REPLACE Zone WITH IIF(laZonesNam[lnCount]='USUPST ',;
                                      'T'+RIGHT(&lcTmpZone..lcZone&lcCount,2),;
                                      &lcTmpZone..lcZone&lcCount)
              ENDIF          
            ENDIF          
          ENDFOR
        ENDIF
      ENDIF  
      
      SELECT &lcTmpZone      
      *-- The last record in the file.

      IF RIGHT(ALLTRIM(lcShipTo),3) = '999'
        EXIT
      ENDIF
      
    ENDSCAN
  ENDIF
ENDFOR
IF llDone
  *--All files have been converted successfully.
  =gfModalGen('TRM00370B00000','DIALOG')
ELSE
  *--No files have been converted.
  =gfModalGen('TRM00371B00000','DIALOG')
ENDIF
CLEAR READ

*!*************************************************************
*! Name      : lfvPath
*! Developer : KHALD MOHI EL-DIN
*! Date      : 03/05/1998
*! Purpose   : To validate path
*!*************************************************************
*! Example            :  lfvPath()
*!*************************************************************
FUNCTION lfvPath
IF EMPTY(lcPathName)
  RETURN
ENDIF  
lcPathNam = lcPathName
lcPathNam = ALLTRIM(lcPathNam)

*-- Initializing the variable to hold the error. If the path and
*-- file name were correct then set the path to the new path in order
*-- to be able to create the TXT file. Otherwise display a message
*-- informing that there was an error occured.
llError = .F.
ON ERROR llError = .T.
lcOldPath = FULLPATH("")
SET DEFAUL TO (lcPathNam)

ON ERROR 
SET DEFAULT TO (lcOldPath)
IF llError OR EMPTY(lcPathName)
  lcPathName=GetDir('','Select source directory')
  IF EMPTY(lcPathName)
    lcPathName = SPACE(40)
    SHOW GET pbProceed DISABLE
    _CUROBJ = _CUROBJ
    RETURN
  ENDIF
  SET DEFAULT TO (lcOldPath)
  SHOW GET pbProceed ENABLE
ELSE
  SHOW GET pbProceed ENABLE
ENDIF
