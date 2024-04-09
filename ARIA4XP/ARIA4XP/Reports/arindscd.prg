*-- Start of converting by MAB (BADRAN 03/02/2003)
*-- N000505 : BADRAN (Individual Schedule Report.)
*:***************************************************************************
*: Program file  : ARINDSCD.PRG
*: Program desc. : Individual Schedules Report.
*:            The program is a conversion from 2.6 ..CONT930.PRG
*! Date          : 07/09/2000
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT (BWA)      
*: Tracking Job Number: New Program (000152)
*!***************************************************************************
*! Modification:
*! B123663,1 SMM 07/13/2004 Change gfMover to lfOGMover.
*! B609356,1 SMA 07/21/2010 Fix bug of creating empty *.cdx files [T20091027.0093]
*!***************************************************************************
*!
*--Get the full picture of the date.
lcCentury = SET('CENTURY')
SET CENTURY ON

lcDevice = SET('DEVICE')
SET DEVICE TO SCREEN

*--Call the mover function to choose the users.
DIMENSION laSource[1],laTarget[1]
STORE '' TO laSource , laTarget

SELECT CUSER_ID FROM SYUUSER INTO ARRAY laSource DISTINCT

* B123663,1 SMM Change gfMover to lfOGMover
*-- = gfMover(@laSource,@laTarget,'Users ',.T.,'lfvUsers()')
= lfOGMover(@laSource,@laTarget,'Users ',.T.,'lfvUsers()')
* B123663,1 SMM End

IF EMPTY(laTarget[1])
  RETURN .F.
ENDIF 

*-- Get the original setting of the device.
SET DEVICE TO &lcDevice.

*--The Users.
PRIVATE lcUsers
lcUsers   = ""
FOR lnInd = 1 TO ALEN(laTarget)
  lcUsers = lcUsers + "|" + PADR(laTarget[lnInd],10)
ENDFOR
lcUsers = ALLTRIM(lcUsers)
lcUsers = SUBSTR(lcUsers,2)
WAIT WINDOW "Collecting Data"  NOWAIT

*-- Get the Schedule date (Remotely) ..... BEGIN
LOCAL lnRemoteResult, lcSelectCommand, lcWhereCond, llNoData, lcScdTmp
lcSelectCommand = [SELECT CUSER_ID,CCONT_ID,CTRANTYPE,DTRANDATE,CTRANTIME,] +;
                  [NESTDUR,IIF(SUBSTR(CTRANTIME,1,2)='12',SUBSTR(CTRANTIME,1,6)+'AM',CTRANTIME) AS cTmpTime] +;
                  [ FROM SYSCHDUL]
*lcWhereCond = GetWhereCond()
lcWhereCond = ""
IF !EMPTY(lcWhereCond)
  lcSelectCommand = lcSelectCommand + " WHERE " + lcWhereCond
ENDIF 

lcScdTmp = gfTempName()
lnRemoteResult = loOGScroll.SQLExecute("SYSCHDUL", lcSelectCommand,"",lcScdTmp,"",;
    oAriaApplication.SystemConnectionString,3,"")
IF lnRemoteResult >= 1 
  SELECT (lcScdTmp)
  LOCATE 
  llNoData = EOF()
ELSE
  llNoData = .T.
ENDIF 

IF llNoData
  WAIT WINDOW "No opened activities." TIMEOUT 2
  SET CENTURY &lcCentury.
  RETURN .F.
ENDIF 

*-- Create the Temporary table.
*-- Create an editable table instead of a read-only cursor..... BEGIN
lcTemp = gfTempName()
LOCAL laSchuleStru[1,18]
=AFIELDS(laSchuleStru)
CREATE TABLE (gcWorkDir+lcTemp+".DBF") FROM ARRAY laSchuleStru
*B609356,1 SMA 07/21/2010 remove of clause to prevent empty *.cdx files from creation.....[BEGIN]
*INDEX ON DTOS(DTRANDATE)+IIF('A' $ CTMPTIME ,'1','2')+CTMPTIME TAG TIMEIND OF (gcWorkDir+lcTemp+".CDX")
*INDEX ON DTOS(DTRANDATE)+IIF('A' $ CTRANTIME ,'1','2')+CTRANTIME TAG DUEIND OF (gcWorkDir+lcTemp+".CDX")
INDEX ON DTOS(DTRANDATE)+IIF('A' $ CTMPTIME ,'1','2')+CTMPTIME TAG TIMEIND 
INDEX ON DTOS(DTRANDATE)+IIF('A' $ CTRANTIME ,'1','2')+CTRANTIME TAG DUEIND
*B609356,1 SMA 07/21/2010 remove of clause to prevent empty *.cdx files from creation.....[END]
*-- Create an editable table instead of a read-only cursor..... END

*-- Get the Schedule date (Remotely) ..... END
DIMENSION TIMARR(23)
TIMARR(1)="08:00 AM   "
TIMARR(2)="08:30 AM   "
TIMARR(3)="09:00 AM   "
TIMARR(4)="09:30 AM   "
TIMARR(5)="10:00 AM   "
TIMARR(6)="10:30 AM   "
TIMARR(7)="11:00 AM   "
TIMARR(8)="11:30 AM   "
TIMARR(9)="01:00 PM   "
TIMARR(10)="01:30 PM   "
TIMARR(11)="02:00 PM   "
TIMARR(12)="02:30 PM   "
TIMARR(13)="03:00 PM   "
TIMARR(14)="03:30 PM   "
TIMARR(15)="04:00 PM   "
TIMARR(16)="04:30 PM   "
TIMARR(17)="05:00 PM   "
TIMARR(18)="05:30 PM   "
TIMARR(19)="06:00 PM   "
TIMARR(20)="06:30 PM   "
TIMARR(21)="07:00 PM   "
TIMARR(22)="12:00 PM   "
TIMARR(23)="12:30 PM   "

SELECT (lcScdTmp)
SCAN 
  ldTempDate = DTRANDATE
  lcUser = CUSER_ID
  FOR I = 1 TO 23
    IF !SEEK(DTOS(ldTempDate)+IIF('A' $ TIMARR(I),'1','2') + TIMARR(I),lcTemp)
      INSERT INTO (lcTemp) (DTRANDATE, CTRANTIME, CTMPTIME, CUSER_ID) ;
                  VALUES (ldTempDate, TIMARR(I),;
                          IIF(SUBSTR(CTRANTIME,1,2)='12',SUBSTR(CTRANTIME,1,6)+'AM',CTRANTIME),;
                          lcUser)
    ENDIF
  ENDFOR
ENDSCAN 

*--   INDEX ON DTOS(DTRANDATE)+IIF('A' $ CTMPTIME ,'1','2')+CTMPTIME TAG LCINDEX
SELECT (lcTemp)
SET ORDER TO TIMEIND
APPEND FROM FULLPATH(DBF(lcScdTmp))
USE IN (lcScdTmp)

SELECT (lcTemp)
LOCATE

*WSH 10/19/2005 [Start]
WAIT CLEAR
*WSH 10/19/2005 [End]
lcRpForm = "ARINDSCD"
DO gfDispRe WITH EVALUATE('lcRpForm')
SET CENTURY &lcCentury
*-- end of main report code.

*!*************************************************************
*! Name      : lfvUsers
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 07/04/2000
*! Purpose   : Function to check the number of the users selected.
*!*************************************************************
*! Called from : ARINDSCD.PRG
*!*************************************************************
*! Calls       :
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvUsers()
*!*************************************************************
FUNCTION lfvUsers
LPARAMETERS lnPressedButton
IF INLIST(lnPressedButton,3,4)  && Remove and RemoveAll
  RETURN .T.
ENDIF 

IF ALEN(latarget) > 4 then
    = gfModalGen('TRB00000','F',' ',' ','You can`t enter more than 5 users.')
   RETURN .F.
ENDIF
*-- End of lfvUsers.

*!*************************************************************
*! Name      : lfvDateRng
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 07/04/2000
*! Purpose   : Showes date range screen
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : DateRng.spr
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvDateRng()
*!*************************************************************
FUNCTION lfvDateRng
PRIVATE ldFrom,ldTo

ldFrom = LCDATE1
LDTO   = LCDATE2
lcDateType = 'D'

lcTitle = 'Due Date Range'

*B603955,1 ABD -Call DateRng & ObjRng Screen from one place to solve
*B603955,1 ABD -Failed to convert 00 to 2000, it converts it to 1900. [Begin]
*DO (gcRepHome + gcAct_Appl + '\DateRng.SPR')
DO DateRng
*B603955,1 ABD - [End]

*-- End of lfvDateRng.
*!*************************************************************
*! Name      : lfvEntrd
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 07/04/2000
*! Purpose   : Showes add date range screen
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : DateRng.spr
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvEntrd()
*!*************************************************************
FUNCTION lfvEntrd
PRIVATE ldFrom,ldTo

ldFrom = ldFAdd
LDTO   = ldTAdd
lcDateType = 'E'

lcTitle = 'Entered Date Range'

*B603955,1 ABD -Call DateRng & ObjRng Screen from one place to solve
*B603955,1 ABD -Failed to convert 00 to 2000, it converts it to 1900. [Begin]
*DO (gcRepHome + gcAct_Appl + '\DateRng.SPR')
DO DateRng
*B603955,1 ABD - [End]



*-- End of lfvEntrd.
*!*************************************************************
*! Name      : lfvComplt
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 07/04/2000
*! Purpose   : Showes Complete date range screen
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : DateRng.spr
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvComplt()
*!*************************************************************
FUNCTION lfvComplt
PRIVATE ldFrom,ldTo

ldFrom = ldFCmplt
LDTO   = ldTCmplt
lcDateType = 'C'

lcTitle = 'Complete Date Range'

*B603955,1 ABD -Call DateRng & ObjRng Screen from one place to solve
*B603955,1 ABD -Failed to convert 00 to 2000, it converts it to 1900. [Begin]
*DO (gcRepHome + gcAct_Appl + '\DateRng.SPR')
DO DateRng
*B603955,1 ABD - [End]



*-- End of lfvDateRng.
*!*************************************************************
*! Name      : lfvpbok
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 07/04/2000
*! Purpose   : Validate date range screen's OK button.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvpbok()
*!*************************************************************
FUNCTION lfvpbok

IF ldFrom > ldTo
   WAIT WINDOW ["From" value must be less than or equal to "To" value] NOWAIT
    *-- _CUROBJ = OBJNUM(ldFrom)
ELSE
  DO CASE
    CASE lcDateType = 'D'
      LCDATE1 = ldFrom
      LCDATE2 = ldTo
    CASE lcDateType = 'E'
      ldFAdd = ldFrom
      ldTAdd = LDTO
    CASE lcDateType = 'C'
      ldFCmplt = ldFrom
      ldTCmplt = LDTO
  ENDCASE
ENDIF
*-- End of lfvpbok.

*-- End of converting by MAB (BADRAN 03/02/2003)
*-- N000505 : BADRAN (Individual Schedule Report.)


*!*****************************************************************************************
*! Name      : AfterOpenDataFiles
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 02/27/2003 02:20:34 PM
*! Purpose   : Event occurs after open data files.
*!*****************************************************************************************
*! Parameters: laOGTables - Data Tables array passed By Ref (@)
*!****************************************************************************************
*!
FUNCTION AfterOpenDataFiles
  LPARAMETERS laOGTables

LOCAL lnConnectionHandlar, lcSqlStatment , loSqlConnection

lcSqlStatment   = [SELECT DISTINCT cUser_ID, cUsr_Name FROM SYUUSER ORDER BY cUser_ID]
loSqlConnection = CREATEOBJECT('remotedataaccess')

lnConnectionHandlar = loSqlConnection.sqlrun(lcSqlStatment,"SYUUSER","SYUUSER",oAriaApplication.SystemConnectionString,3,;
                                      'SAVE')
IF lnConnectionHandlar = 1
  =CURSORSETPROP("Buffering",3,"SYUUSER")
  *-- To initialize the indecis that will be created for each file
  lcUserTable    = gfTempName()
  *B609356,1 SMA 07/21/2010 remove of clause to prevent empty *.cdx files from creation.....[BEGIN]
  *INDEX ON cUser_ID TAG cUser_ID OF (oAriaApplication.WorkDir+lcUserTable+".CDX")
  INDEX ON cUser_ID TAG cUser_ID 
  *B609356,1 SMA 07/21/2010 remove of clause to prevent empty *.cdx files from creation.....[END]
  loSqlConnection = NULL
ELSE
  =loSqlConnection.CheckRetResult("sqlrun",lnConnectionHandlar,.T.)
  loSqlConnection = NULL
ENDIF
RETURN .F.  

*-- end of AfterOpenDataFiles.

*!*****************************************************************************************
*! Name      : lfwOGWhen
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 03/02/2003 02:20:34 PM
*! Purpose   : Option Grid when function
*!*****************************************************************************************
*!
FUNCTION lfwOGWhen

  *-- Adjust date range captions.
  LOCAL loDateObj
  *-- Schedule Date.
  loDateObj = GetObjectRef("lnDumIndv")
  IF ATC("Between",loDateObj.Caption) = 0
    loDateObj.Caption = "Between ..."
    loDateObj = .NULL.

    *-- Entered Date.
    loDateObj = GetObjectRef("lnDumAdd")
    loDateObj.Caption = "Between ..."
    loDateObj = .NULL.

    *-- Complete Date.
    loDateObj = GetObjectRef("lnDumCmpl")
    loDateObj.Caption = "Between ..."
    loDateObj = .NULL.
  ELSE
    loDateObj = .NULL.
  ENDIF 
  
ENDFUNC 
*-- end of lfwOGWhen.

*!*****************************************************************************************
*! Name      : RefSchdule
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 03/02/2003 11:18:46 AM
*! Purpose   : 
*! Entry no. : 
*!*****************************************************************************************
*!
FUNCTION RefSchdule
  LPARAMETERS cDateType
  LOCAL lcSelDate, ldFromDate, ldToDate, lcFromDate, lcToDate
  DO CASE
    CASE cDateType = 'D'
      ldFromDate = LCDATE1
      ldToDate   = LCDATE2
    CASE cDateType = 'E'
      ldFromDate = ldFAdd
      ldToDate   = ldTAdd
    CASE cDateType = 'C'
      ldFromDate = ldFCmplt
      ldToDate   = ldTCmplt
  ENDCASE

  lcFromDate = ALLTRIM(DTOC(ldFromDate))
  lcToDate   = ALLTRIM(DTOC(ldToDate))
  IF lcFromDate == lcToDate
    IF EMPTY(ldToDate)
      lcSelDate = ""
    ELSE
      lcSelDate = lcFromDate
    ENDIF   
  ELSE
    lcSelDate = lcFromDate + ", " + lcToDate
  ENDIF 
  RETURN lcSelDate
ENDFUNC 
*-- End of RefSchdule.

*!*****************************************************************************************
*! Name      : GetWhereCond
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 03/02/2003 11:18:46 AM
*! Purpose   : 
*! Entry no. : 
*!*****************************************************************************************
*!
FUNCTION GetWhereCond
  LOCAL lcWhereCond
  lcWhereCond = ""
  IF !EMPTY(lcUsers)
    IF OCCURS("|",lcUsers) = 0
      lcWhereCond = lcWhereCond + 'CUSER_ID = "' + lcUsers + '"'
    ELSE
      lcWhereCond = lcWhereCond + 'CUSER_ID $ "' + lcUsers + '"'
    ENDIF   
  ENDIF
  
  IF lcRpTask  != 'L'
    lcWhereCond = lcWhereCond + IIF(EMPTY(lcWhereCond),""," AND ") +;
         'CTRANTYPE = "' + lcRpTask + '"'
  ENDIF 

  IF lcRpStats != 'B'
    lcWhereCond = lcWhereCond + IIF(EMPTY(lcWhereCond),""," AND ") +;
         'CCOMPLETED = "' + lcRpStats + '"'
  ENDIF 

  IF !EMPTY(LCDATE1) OR !EMPTY(LCDATE2)
    lcWhereCond = lcWhereCond + IIF(EMPTY(lcWhereCond),""," AND ") +;
         'BETWEEN(DTOS(DTRANDATE),"' + DTOS(LCDATE1) + '","' + DTOS(LCDATE2) + '")'
  ENDIF 

  IF !EMPTY(ldFAdd) OR !EMPTY(ldTAdd)
    lcWhereCond = lcWhereCond + IIF(EMPTY(lcWhereCond),""," AND ") +;
         'BETWEEN(DTOS(DADD_DATE),"' + DTOS(ldFAdd) + '","' + DTOS(ldTAdd) + '")'
  ENDIF 

  IF !EMPTY(ldFCmplt) OR !EMPTY(ldTCmplt)
    lcWhereCond = lcWhereCond + IIF(EMPTY(lcWhereCond),""," AND ") +;
         'BETWEEN(DTOS(DcmpltDATE),"' + DTOS(ldFCmplt) + '","' + DTOS(ldTCmplt) + '")'
  ENDIF 
  
  *-- BADRAN Include the lcRpExp in this report (Feature Flexiablity)
  IF UPPER(ALLTRIM(lcRpExp)) != ".T."
    lcWhereCond = lcWhereCond + IIF(EMPTY(lcWhereCond),""," AND ") + lcRpExp
  ENDIF 

  RETURN lcWhereCond 
ENDFUNC 
*-- end of GetWhereCond.

*!*************************************************************
*! Name      : lfClearRep
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/1998
*! Purpose   : Function that we call when Close the option grid.
*!*************************************************************
FUNCTION lfClearRep
  LOCAL lcUserTable
  lcUserTable = UPPER(ALLTRIM(FULLPATH(DBF("SYUUSER"))))
  USE IN SYUUSER
  ERASE (lcUserTable)
  lcUserTable = STRTRAN(lcUserTable,".DBF",".CDX")
  ERASE (lcUserTable)
ENDFUNC 
*-- end of lfClearRep.
