*:***************************************************************************
*: Program file  : ARINDSCD.PRG
*: Program desc. : Individual Schedules Report.
*: 				   The program is a conversion from 2.6 ..CONT930.PRG
*! Date          : 07/09/2000
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT (BWA)			
*: Tracking Job Number: New Program (000152)
*: 
*:***************************************************************************
*: Calls : 						
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARINDSCD
*:***************************************************************************
*: Modifications
*B603955,1 ABD 10/18/2000 Call DateRng & ObjRng Screen from one place to solve
*B603955,1 ABD            Failed to convert 00 to 2000, it converts it to 1900.
*:***************************************************************************

*--Get the full picture of the date.
lcCentury = SET('CENTURY')
SET CENTURY ON

lcDevice = SET('DEVICE')
SET DEVICE TO SCREEN

*--Call the mover function to choose the users.
DIMENSION laSource[1],laTarget[1]
STORE '' TO laSource , laTarget
SELECT CUSER_ID FROM SYUUSER INTO ARRAY laSource
= gfMover(@laSource,@laTarget,'Users ',.T.,'lfvUsers()')

*-- Get the original setting of the device.
SET DEVICE TO &lcDevice

*--The Users.
STORE SPACE(0) TO lcUsers
FOR lnInd = 1 TO ALEN(laTarget)
  lcUsers = lcUsers + PADR(laTarget[lnInd],6) + ' | '
ENDFOR
lcUsers = IIF(ALLTRIM(lcUsers) = '|','',lcUsers)

WAIT WINDOW "Collecting Data"  NOWAIT
SELECT SYSCHDUL
SELECT CUSER_ID,CCONT_ID,CTRANTYPE,DTRANDATE,CTRANTIME,;
       NESTDUR,IIF(SUBSTR(CTRANTIME,1,2)='12',SUBSTR(CTRANTIME,1,6)+'AM',CTRANTIME) AS cTmpTime ;
       FROM SYSCHDUL INTO TABLE (GCWORKDIR+lcTemp);
       WHERE IIF(LCDATE1={} .AND. LCDATE2= {} , DTRANDATE  = DTRANDATE ,;
             IIF(LCDATE1={} .AND. LCDATE2<>{} , DTRANDATE <= LCDATE2 , BETWEEN(DTRANDATE,LCDATE1,LCDATE2)));
       .AND. IIF(ldFAdd  ={} .AND. ldTAdd  = {} , DADD_DATE   = DADD_DATE ,;
             IIF(ldFAdd  ={} .AND. ldTAdd  <>{} , DADD_DATE  <= ldTAdd    , BETWEEN(DADD_DATE,ldFAdd,ldTAdd)));
       .AND. IIF(ldFCmplt={} .AND. ldTCmplt= {} , DcmpltDATE  = DcmpltDATE,;
             IIF(ldFCmplt={} .AND. ldTCmplt<>{} , DcmpltDATE <= ldTCmplt , BETWEEN(DcmpltDATE,ldFCmplt,ldTCmplt)));
       .AND. IIF(lcRpTask  = 'L' , CTRANTYPE = "" , CTRANTYPE = lcRpTask);
       .AND. IIF(lcRpStats = 'B' , CCOMPLETED = "" , CCOMPLETED = lcRpStats);
       .AND. ALLTRIM(CUSER_ID) $ lcUsers
WAIT CLEAR

SELECT (lcTemp)
IF _TALLY = 0
  WAIT WINDOW "No opened activities." NOWAIT
  RETURN
ENDIF

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

INDEX ON DTOS(DTRANDATE)+IIF('A' $ CTRANTIME ,'1','2')+CTRANTIME TAG LCINDEX
LOCATE
DO WHILE .T.
  ldTempDate = DTRANDATE
  lcUser = CUSER_ID
  FOR I = 1 TO 23
	IF !SEEK(DTOS(ldTempDate)+IIF('A' $ TIMARR(I),'1','2') + TIMARR(I))
      APPEND BLANK
	  REPLACE DTRANDATE WITH ldTempDate,;
              CTRANTIME WITH TIMARR(I),;
              CTMPTIME  WITH IIF(SUBSTR(CTRANTIME,1,2)='12',SUBSTR(CTRANTIME,1,6)+'AM',CTRANTIME),;
        	  CUSER_ID  WITH lcUser
	ENDIF
  ENDFOR
  SKIP
  IF EOF()
	EXIT 
  ELSE
	ldTempDate=DTRANDATE
	LOOP
	ENDIF
ENDDO

SELECT (lcTemp)
INDEX ON DTOS(DTRANDATE)+IIF('A' $ CTMPTIME ,'1','2')+CTMPTIME TAG LCINDEX
LOCATE

DO gfDispRe WITH EVALUATE('lcRpForm')
SET CENTURY &lcCentury

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

lcTitle = 'Date range'

*B603955,1 ABD -Call DateRng & ObjRng Screen from one place to solve
*B603955,1 ABD -Failed to convert 00 to 2000, it converts it to 1900. [Begin]
*DO (gcRepHome + gcAct_Appl + '\DateRng.SPR')
DO DateRng.Spx
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

lcTitle = 'Entered Date range'

*B603955,1 ABD -Call DateRng & ObjRng Screen from one place to solve
*B603955,1 ABD -Failed to convert 00 to 2000, it converts it to 1900. [Begin]
*DO (gcRepHome + gcAct_Appl + '\DateRng.SPR')
DO DateRng.Spx
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

lcTitle = 'Complete Date range'

*B603955,1 ABD -Call DateRng & ObjRng Screen from one place to solve
*B603955,1 ABD -Failed to convert 00 to 2000, it converts it to 1900. [Begin]
*DO (gcRepHome + gcAct_Appl + '\DateRng.SPR')
DO DateRng.Spx
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
  _CUROBJ = OBJNUM(ldFrom)
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
  CLEAR READ
ENDIF

*-- End of lfvpbok.

*!*************************************************************
*! Name      : gfMover
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 07/04/2000
*! Purpose   : The mover function.
*!*************************************************************
*! Called from : ARINDSCD.PRG
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = gfMover()
*!*************************************************************
FUNCTION gfMover
PARAMETERS laSource,laTarget,lcMovTitle,llOpenAll,lcVldFunc,llMV1By1,llReMV1By1

DIMENSION laSource[ALEN(laSource,1),1],laTarget[ALEN(laTarget,1),1]

lnLoop    = 0

*** This for the mover title. ***
lcMovTitle = IIF(TYPE("lcMovTitle") $ "UL" , "Mover" , lcMovTitle)
lcMovTitle = IIF(EMPTY(lcMovTitle),"Mover" ,lcMovTitle)

lnWidTit  = IIF(LEN(ALLTRIM(lcMovTitle)) > 74,74,LEN(ALLTRIM(lcMovTitle)))
lnWidTit  = IIF(lnWidTit = 0 , 1 , lnWidTit )
llOpenAll = IIF(TYPE("llOpenAll")="U",.F.,llOpenAll)

IF llOpenAll
  llCanVald = .T.
ELSE
  llCanVald = (laScrMode[3] .OR. laScrMode[4]) 
ENDIF  

lnMaxList = 1

FOR lnCount = 1 TO ALEN(laSource,1)
  IF TXTWIDTH(laSource[lnCount],'MS Sans Serif',9,'N') > lnMaxList
    lnMaxList = TXTWIDTH(laSource[lnCount],'MS Sans Serif',9,'N')
  ENDIF
ENDFOR

lnMaxList = IIF(_DOS,IIF(lnMaxList+4 > 31 , 31 , lnMaxList+4 ),;
                     IIF(lnMaxList+7 > 27 , 27 , lnMaxList+7 ))
lnMaxScrn = IIF(_DOS,MAX(lnWidTit,(lnMaxList * 2) + 16),(lnMaxList*2)+31)

lnColPos = IIF(LEN(ALLTRIM(lcMovTitle)) > lnMaxScrn,0,;
           INT((lnMaxScrn - LEN(ALLTRIM(lcMovTitle)))/2))

EXTERNAL ARRAY laSource,laTarget
lnOldDim =ALEN(laTarget,1)
DECLARE laOldTarg[lnOldDim]

=ACOPY(laTarget,laOldTarg)

lnOldSour =ALEN(laSource,1)
DECLARE laOldSour[lnOldSour]
=ACOPY(laSource,laOldSour)

IF ALEN(laTarget,1) = 1 .AND. TYPE('laTarget[1]')="L"
  laTarget =' '
ENDIF  

FOR lnCount = 1 TO ALEN('laSource',1)
  IF ASCAN('laTarget',ALLTRIM(laSource[lnCount])) > 0
    laSource[lnCount,1] = '\'+laSource[lnCount,1]
  ENDIF
ENDFOR

lsSource = IIF(LEFT(laSource[1,1] ,1) = "\",0,1)
lsTarget = 1

PUSH KEY
ON KEY
ON KEY LABEL ESC DO lfCancel
DO SYMover.SPR
POP KEY

*!*************************************************************
*! Name      : lfShow
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 07/04/2000
*! Purpose   : The lfShow function.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfShow()
*!*************************************************************
FUNCTION lfShow

IF llCanVald 
  IF ALEN('laTarget',1) = ALEN('laSource',1)  AND !EMPTY(laTarget[1]);
    OR EMPTY(laSource[1])
    SHOW GET lsSource     DISABLE
    SHOW GET pbMove       DISABLE
    SHOW GET pbAll        DISABLE
  ELSE
    SHOW GET lsSource     ENABLE
    SHOW GET pbMove       ENABLE
    SHOW GET pbAll        ENABLE
  ENDIF  
  
  IF EMPTY(laTarget[1])
    SHOW GET lsTarget    DISABLE
    SHOW GET pbRemove    DISABLE
    SHOW GET pbRAll      DISABLE
  ELSE
    SHOW GET lsTarget    ENABLE
    SHOW GET pbRemove    ENABLE
    SHOW GET pbRAll      ENABLE
  ENDIF  
ELSE
    SHOW GET pbRemove    DISABLE
    SHOW GET pbRAll      DISABLE    
    SHOW GET pbMove      DISABLE
    SHOW GET pbAll       DISABLE    
    SHOW GET pbCancel    DISABLE
ENDIF  
SHOW GET pbAll        DISABLE

*!*************************************************************
*! Name      : lfvSource
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 07/04/2000
*! Purpose   : The lfvSource function.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvSource()
*!*************************************************************
FUNCTION lfvSource

IF TYPE('lcVldFunc') = 'C' .AND. !EMPTY(lcVldFunc) 
  IF !&lcVldFunc.(1)
    RETURN .F.
  ENDIF  
ENDIF  

IF lsSource = 0
  RETURN .F.
ENDIF

IF lsSource <= ALEN('laSource',1) AND lsSource <> 0
  IF !EMPTY(laTarget[1]) 
    DIMENSION laTarget[ALEN(laTarget)+1]
  ENDIF
  laTarget[ALEN(laTarget)]= ALLTRIM(laSource[lsSource])
    laSource[lsSource] = "\"+laSource[lsSource]
    IF lsSource < ALEN(laSource)
      DO WHILE LEFT(laSource[lsSource],1) = "\" 
        lsSource = lsSource + 1
        IF lsSource = ALEN(laSource)
          EXIT
        ENDIF
      ENDDO  
    ELSE
      lsSource = 1  
    ENDIF
ENDIF  

IF lsSource > ALEN(laSource,1)
  lsSource = ALEN(laSource,1)
ENDIF

_CUROBJ = OBJNUM(lsSource)
SHOW GETS

*!*************************************************************
*! Name      : lfvTarget
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 07/04/2000
*! Purpose   : The lfvTarget function.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvTarget()
*!*************************************************************
FUNCTION lfvTarget
IF TYPE('lcVldFunc') = 'C' .AND. !EMPTY(lcVldFunc) 
  IF !&lcVldFunc.(3)
    *B601886,1 Add this line to loop in latarget in another item.
    IF lnLoop > 0
      lnLoop = lnLoop + 1
    ENDIF  
    RETURN .F.
  ENDIF  
ENDIF  

lcExactSav = SET("EXACT")
SET EXACT ON

IF lsTarget <= ALEN('laTarget',1) .AND.;
   lsTarget <> 0 .AND.;
   ASCAN('laSource',"\"+laTarget[lsTarget]) > 0

  lsSource  = ASUBSCRIPT('laSource',;
                   ASCAN('laSource',"\"+laTarget[lsTarget]),1)
  *-- Adding this line in order to enable the element in the source list 
  *-- when removing it from the target one.
  laSource[lsSource] = SUBSTR(laSource[lsSource],2,LEN(laSource[lsSource]))

  =ADEL(laTarget,lsTarget)
  IF ALEN(laTarget,1) > 1
    DIMENSION laTarget[ALEN(laTarget)-1]
  ELSE
    laTarget[1] =' '
  ENDIF  
ENDIF


IF lsTarget = 0
  lsTarget = 1
ENDIF  

IF lsTarget > ALEN(laTarget,1)
  lsTarget = ALEN(laTarget,1)
ENDIF

_CUROBJ = OBJNUM(lsTarget)
SHOW GETS

SET EXACT &lcExactSav

*!*************************************************************
*! Name      : lfvMovmnts
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 07/04/2000
*! Purpose   : The lfvMovmnts function.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvMovmnts()
*!*************************************************************
FUNCTION lfvMovmnts

PARAMETERS lnMovmnts

DO CASE
  CASE lnMovmnts = 1
    =lfvSource()
  CASE lnMovmnts = 2
    IF TYPE('lcVldFunc') = 'C' .AND. !EMPTY(lcVldFunc) .AND. llMV1By1
      FOR lnI = 1 TO ALEN(laSource,1)
        lsSource = lnI 
        =lfvSource()
      ENDFOR
    ELSE  
      IF TYPE('lcVldFunc') = 'C' .AND. !EMPTY(lcVldFunc)
        IF !&lcVldFunc.(lnMovmnts)
          RETURN
        ENDIF  
      ENDIF
      DECLARE laTarget[ALEN('laSource',1)]
      FOR lnCounter = 1 TO ALEN(laSource,1)
        lnPos = IIF(OCCURS("\",laSource[lnCounter])>0,2,1)
        laTarget[lnCounter] = SUBSTR(laSource[lnCounter],lnPos,LEN(laSource[lnCounter]))
        laSource[lnCounter] = IIF(OCCURS("\",laSource[lnCounter])>0,"","\") + laSource[lnCounter]
      ENDFOR
      SHOW GETS
    ENDIF
  CASE lnMovmnts = 3
    =lfvTarget()
  CASE lnMovmnts = 4
    IF TYPE('lcVldFunc') = 'C' .AND. !EMPTY(lcVldFunc) .AND. llReMV1By1
      lnLoop    = 1
      DO WHILE lnLoop <= ALEN(laTarget) AND ;
               ! (ALEN(laTarget,1) = 1 AND EMPTY(laTarget[1]))   
        lsTarget = lnLoop 
        =lfvTarget()
      ENDDO 				&& Ending of DO loop
      lnLoop    = 0
    ELSE
      IF TYPE('lcVldFunc') = 'C' .AND. !EMPTY(lcVldFunc)
        IF !&lcVldFunc.(lnMovmnts)
          RETURN
        ENDIF  
      ENDIF
      FOR lnCounter = 1 TO ALEN(laSource,1)
        lnPos = IIF(OCCURS("\",laSource[lnCounter])>0,2,1)
        laSource[lnCounter] = SUBSTR(laSource[lnCounter],lnPos,LEN(laSource[lnCounter]))
      ENDFOR
      DECLARE laTarget[1]
      laTarget =' '
      SHOW GETS
    ENDIF
ENDCASE


*!*************************************************************
*! Name      : lfvCancel
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 07/04/2000
*! Purpose   : The lfvCancel function.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvCancel()
*!*************************************************************
FUNCTION lfvCancel

DECLARE laTarget[lnOldDim]
=ACOPY(laOldTarg,laTarget)
DECLARE laSource[lnOldSour]
=ACOPY(laOldSour,laSource)


FUNCTION lfCancel
=lfvCancel()
CLEAR READ

*-- End of gfMover.

*-- 03/11/2003 MAB ARIA4 COMPATABILITY ..... BEGIN
FUNCTION lfwOGWhen
*-- end of lfwOGWhen.
*-- 03/11/2003 MAB ARIA4 COMPATABILITY ..... END
