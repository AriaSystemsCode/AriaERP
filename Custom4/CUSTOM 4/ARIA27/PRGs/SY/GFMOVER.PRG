*:************************************************************************
*: Program file  : GFMOVER.PRG
*: Program desc. : 
*: For screen    :
*:         System: Aria advantage series
*:         Module: Main system 
*:      Developer: Yasser Saad Ibrahime
*:************************************************************************
*: Calls : 
*:         Procedures :
*:         Functions  : 
*:************************************************************************
*: Passed Parameters  : 
*:************************************************************************
*B601886,1 MAB  10/27/97  Fix two bugs, in windows version.
*B601886,1                1- The title is "Mover" at all. 
*B601886,1                2- The mover transfer data in view mode.
*B601886,1                   To solve this problem, add the following   
*B601886,1                   Code in lists validation in the screen
*B601886,1                   IIF(llCanVald,lfvSource(),.T.) for source
*B601886,1                   IIF(llCanVald,lfvTarget(),.T.) for target
*B601886,1                3- The mover do the valid Fn passed to it
*B601886,1                   Twice if we press Move or ReMove.
*B601886,1                   To solve this problem, make valid in lists only.   
*B601886,1 (New Feature)  4- In some cases we want to validate MoveAll and ReMoveAll
*B601886,1                   as we validate Move and Remove, one by one  
*B601886,1                   To make this , add another two parameters 
*B601886,1                   i.  llMV1By1 pass .T. if you want making times of your <Move> Valid code
*B601886,1                   ii. llReMV1By1 pass .T. if you want making times of your <ReMove> Valid code
*E300825,1 MAB 03/04/98 Adjust Symover screen variables because we change screen font.
*B801980,1 KHM 03/24/99 Fixed the bug of not displaying all the elements of
*B801980,1              the source list in the gfMovr() function when the 
*B801980,1              elements excceeded 2500 elements.
*B602758,1 MAB 04/08/99 Fix invalid subscripte refrence bug.
*B602922,1 MAB 05/24/99 If first source item was selected before lsSource must equal zero.
*B602922,1              i.e. Avoid multiple selection for first source item.
*B6036796,1 ADEL 08/06/2000 Don't move previously-moved elemnts in the mover when pressing
*B6036796,1                 Move All button again or after Move button.
********************************************************************************
FUNCTION gfMover
PARAMETERS laSource,laTarget,lcMovTitle,llOpenAll,lcVldFunc,llMV1By1,llReMV1By1

*B602758,1 Redimension source and target array adding column dimension [Begin]
DIMENSION laSource[ALEN(laSource,1),1],laTarget[ALEN(laTarget,1),1]
*B602758,1 Redimension source and target array adding column dimension [End  ]

*B601886,1 Add this variable to hold number of item to be valid 
*B601886,1 in <ReMoveAll> case because changes occur to laTarget.
lnLoop    = 0
*B601886,1  (End)

*** This for the mover title. ***
lcMovTitle = IIF(TYPE("lcMovTitle") $ "UL" , "Mover" , lcMovTitle)

*B601886,1 (#1) Assure that the passed title not empty
lcMovTitle = IIF(EMPTY(lcMovTitle),"Mover" ,lcMovTitle)
*B601886,1 (#1) (End)

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
  *E300825,1 Change block1 to block2 to fill lnMaxList with approparate width 
  *E300825,1 corressponding to actual font size
  *E300825,1 block1 [begin]
  *IF LEN(laSource[lnCount]) > lnMaxList
    *lnMaxList = LEN(laSource[lnCount])
  *ENDIF
  *E300825,1 block1 [end]
  
  *E300825,1 block2 [begin]
  IF TXTWIDTH(laSource[lnCount],'MS Sans Serif',9,'N') > lnMaxList
    lnMaxList = TXTWIDTH(laSource[lnCount],'MS Sans Serif',9,'N')
  ENDIF
  *E300825,1 block2 [end]
ENDFOR

*E300825,1 Change lnMaxList expression [begin] 
*lnMaxList = IIF(_DOS,IIF(lnMaxList+4 > 31 , 31 , lnMaxList+4 ),;
                     IIF(lnMaxList+4 > 27 , 27 , lnMaxList+4 ))

lnMaxList = IIF(_DOS,IIF(lnMaxList+4 > 31 , 31 , lnMaxList+4 ),;
                     IIF(lnMaxList+7 > 27 , 27 , lnMaxList+7 ))
*E300825,1 Change lnMaxList expression [end] 


*E300825,1 Change lnMaxScrn expression [begin] 
*lnMaxScrn = IIF(_DOS,MAX(lnWidTit,(lnMaxList * 2) + 16),(lnMaxList*2)+23)
lnMaxScrn = IIF(_DOS,MAX(lnWidTit,(lnMaxList * 2) + 16),(lnMaxList*2)+31)
*E300825,1 Change lnMaxScrn expression [end] 

lnColPos = IIF(LEN(ALLTRIM(lcMovTitle)) > lnMaxScrn,0,;
           INT((lnMaxScrn - LEN(ALLTRIM(lcMovTitle)))/2))


EXTERNAL ARRAY laSource,laTarget
lnOldDim =ALEN(laTarget,1)
DECLARE laOldTarg[lnOldDim]

=ACOPY(laTarget,laOldTarg)

*B801980,1 KHM 03/24/99 (Begin) Copying the source array into laOldSour
*B801980,1            in order to be able to reuse it in the cancel command.
lnOldSour =ALEN(laSource,1)
DECLARE laOldSour[lnOldSour]
=ACOPY(laSource,laOldSour)
*B801980,1 KHM 03/24/99 (End)

IF ALEN(laTarget,1) = 1 .AND. TYPE('laTarget[1]')="L"
  laTarget =' '
ENDIF  

*B801980,1 KHM 03/24/99 (Begin) The following line is commented out
*B801980,1              because we are no longer using the popup.
*DEFINE POPUP puSource  MARGIN RELATIVE SCROLL MARK CHR(16)
*B801980,1 KHM 03/24/99 (End)

FOR lnCount = 1 TO ALEN('laSource',1)
  *B801980,1 KHM 03/24/99 (Begin) The following line is commented out.
  *DEFINE BAR lnCount OF puSource PROMPT (ALLTRIM(laSource[lnCount]))
  *B801980,1 KHM 03/24/99 (End)
  IF ASCAN('laTarget',ALLTRIM(laSource[lnCount])) > 0
    *B801980,1 KHM 03/24/99 (Begin) Commenting the following line because
    *B801980,1                      we are not using the popup.
    *SET SKIP OF BAR lnCount OF puSource .T.
    
    laSource[lnCount,1] = '\'+laSource[lnCount,1]
    *laSource[lnCount] = '\'+laSource[lnCount]
    *B801980,1 KHM 03/24/99 (End)
  ENDIF
ENDFOR

*B602922,1 If first source item was selected before lsSource must equal zero. [Begin]
*B602922,1 i.e. you can not select it again.
*STORE 1  TO lsSource,lsTarget
lsSource = IIF(LEFT(laSource[1,1] ,1) = "\",0,1)
lsTarget = 1
*B602922,1 If first source item was selected before lsSource must equal zero. [End  ]

PUSH KEY
ON KEY
ON KEY LABEL ESC DO lfCancel
DO SYMover.SPR
POP KEY

*!*************************************************************************
*!
*!              Function: lfShow
*!
*!*************************************************************************
*
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

*!*************************************************************************
*!
*!              Function: lfvSource
*!
*!*************************************************************************
*
FUNCTION lfvSource

*B6036796,1 (Begin) Move the condition here. If this element has previously selected, Ignore it.
IF lsSource = 0 OR LEFT(laSource[lsSource],1) = "\"
  RETURN .F.
ENDIF
*B6036796,1 (End)

IF TYPE('lcVldFunc') = 'C' .AND. !EMPTY(lcVldFunc) 
  IF !&lcVldFunc.(1)
    RETURN .F.
  ENDIF  
ENDIF  
*B601886,1 (MAB) When bar is skipped, and try to move it [BEGIN] 
*B6036796,1 (Begin) Remark the following lines and move it up to most top of this function with a new condition.
*IF lsSource = 0
*  RETURN .F.
*ENDIF
*B6036796,1 (End)
*B601886,1 (MAB) When bar is skipped, it try to move it [END] 

IF lsSource <= ALEN('laSource',1) AND lsSource <> 0
  *B801980,1 KHM 03/24/99 (Begin) The following line is commented out
  *B801980,1              because we are not using the popup anymore 
  *B801980,1              and we are using an array instead of popup.    
  *SET SKIP OF BAR lsSource OF puSource .T.
  *B801980,1 KHM 03/24/99 (End)
  
  IF !EMPTY(laTarget[1]) 
    DIMENSION laTarget[ALEN(laTarget)+1]
  ENDIF
  laTarget[ALEN(laTarget)]= ALLTRIM(laSource[lsSource])
    *B801980,1 KHM 03/24/99 (Begin) When moving a record we replace "\" 
    *B801980,1              before the moved element in order to disable it.
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
    *B801980,1 KHM 03/24/99 (End)
ENDIF  

*B801980,1 KHM 03/24/99 (Begin) The following code is commented out because
*B801980,1              we are not using the popup anymore as we are using
*B801980,1              an array instead of the popup.
*lnStart  = lsSource
*lsSource = 0

*FOR lnCount = lnStart TO CNTBAR('puSource')
*  IF !SKPBAR('puSource',lnCount)
*    lsSource = lnCount 
*    EXIT
*  ENDIF  
*ENDFOR

*IF lsSource = 0
*  FOR lnCount = 1 TO CNTBAR('puSource')
*    IF !SKPBAR('puSource',lnCount)
*      lsSource = lnCount 
*      EXIT
*    ENDIF  
*  ENDFOR
*ENDIF  
*B801980,1 KHM 03/24/99 (End)

*B601886,1 (MAB) Adjust List Pointer [BEGIN]
IF lsSource > ALEN(laSource,1)
  lsSource = ALEN(laSource,1)
ENDIF
*B601886,1 (MAB) Adjust List Pointer [END]


_CUROBJ = OBJNUM(lsSource)
SHOW GETS

*!*************************************************************************
*!
*!              Function: lfvTarget
*!
*!*************************************************************************
*
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

*B801980,1 KHM 03/24/99 (Begin) Changing the ASCAN in the IF command
*B801980,1              by adding a "\" to the element of the target 
*B801980,1              that we are looking for in the source in order to 
*B801980,1              remove it from the target and enable it in the source.
*IF lsTarget <= ALEN('laTarget',1) .AND.;
   lsTarget <> 0 .AND.;
   ASCAN('laSource',laTarget[lsTarget]) > 0

  *lsSource  = ASUBSCRIPT('laSource',;
                   ASCAN('laSource',laTarget[lsTarget]),1)
  *SET MARK OF POPUP puSource .F.
  *SET SKIP OF BAR lsSource OF puSource .F.
IF lsTarget <= ALEN('laTarget',1) .AND.;
   lsTarget <> 0 .AND.;
   ASCAN('laSource',"\"+laTarget[lsTarget]) > 0

  lsSource  = ASUBSCRIPT('laSource',;
                   ASCAN('laSource',"\"+laTarget[lsTarget]),1)
  *-- Adding this line in order to enable the element in the source list 
  *-- when removing it from the target one.
  laSource[lsSource] = SUBSTR(laSource[lsSource],2,LEN(laSource[lsSource]))
*B801980,1 KHM 03/24/99 (End)

  =ADEL(laTarget,lsTarget)
  IF ALEN(laTarget,1) > 1
    DIMENSION laTarget[ALEN(laTarget)-1]
  ELSE
    laTarget[1] =' '
  ENDIF  
ENDIF


*B601886,1 (MAB) Adjust List Pointer [BEGIN]
IF lsTarget = 0
  lsTarget = 1
ENDIF  

*B601886,1 Adjust List Pointer
IF lsTarget > ALEN(laTarget,1)
  lsTarget = ALEN(laTarget,1)
ENDIF
*B601886,1 (MAB) Adjust List Pointer [END]

_CUROBJ = OBJNUM(lsTarget)
SHOW GETS

SET EXACT &lcExactSav

*!*************************************************************************
*!
*!              Function: lfvMovmnts
*!
*!*************************************************************************
*
FUNCTION lfvMovmnts

PARAMETERS lnMovmnts

*B601886,1  (#3) Remove this code and prevent twice doing, 
*B601886,1  also it serve when valid for MoveAll and RemoveAll 
*IF TYPE('lcVldFunc') = 'C' .AND. !EMPTY(lcVldFunc) 
*  IF !&lcVldFunc.(lnMovmnts)
*    RETURN
*  ENDIF  
*ENDIF  

DO CASE
  CASE lnMovmnts = 1
    *B601886,1   New code is clear and faster than old code because of
    *B601886,1   direct access to valid function. 
    *_CUROBJ = OBJNUM(lsSource)
    *KEYBOARD "{ENTER}"
    =lfvSource()
  CASE lnMovmnts = 2
    *B601886,1  For one by one items in Source list  
    IF TYPE('lcVldFunc') = 'C' .AND. !EMPTY(lcVldFunc) .AND. llMV1By1
      FOR lnI = 1 TO ALEN(laSource,1)
        *B601886,1  This code is to assign new item to be moved
        lsSource = lnI 
        =lfvSource()
      ENDFOR
      *B6036796,1 (Begin) This variable will be used by any program to determine that the meassage teliing
      *B6036796,1         that any color cannot be deleted will be dispalyed once
      *B6036796,1         i.e IF llDispMess ... THEN MESSAGE AND llDispMess = .F.
      llDispMess = .T.
      *B6036796,1 (eND)
    ELSE  
      *B601886,1  Normal MoveAll   
      IF TYPE('lcVldFunc') = 'C' .AND. !EMPTY(lcVldFunc)
        IF !&lcVldFunc.(lnMovmnts)
          RETURN
        ENDIF  
      ENDIF
      DECLARE laTarget[ALEN('laSource',1)]
      *B801980,1 KHM 03/24/99 (Begin) The following code is added when using
      *B801980,1              the move all button and its used to move all the 
      *B801980,1              elements from the source list to the target and 
      *B801980,1              checking if the element is move to the target list
      *B801980,1              before using the move all command in order to do 
      *B801980,1              not move it with a disabled status.
      *=ACOPY(laSource,laTarget)
      *SET SKIP OF POPUP puSource .T.
      FOR lnCounter = 1 TO ALEN(laSource,1)
        lnPos = IIF(OCCURS("\",laSource[lnCounter])>0,2,1)
        laTarget[lnCounter] = SUBSTR(laSource[lnCounter],lnPos,LEN(laSource[lnCounter]))
        laSource[lnCounter] = IIF(OCCURS("\",laSource[lnCounter])>0,"","\") + laSource[lnCounter]
      ENDFOR
      *B801980,1 KHM 03/24/99 (End)
      SHOW GETS
    ENDIF
  CASE lnMovmnts = 3
    *_CUROBJ = OBJNUM(lsTarget)
    *KEYBOARD "{ENTER}"
    *B601886,1   This code is clear and faster than old code because of
    *B601886,1   direct access to valid function. 
    =lfvTarget()
  CASE lnMovmnts = 4
    *B601886,1  For one by one items in Target list  
    *B601886,1  WE have to use while because when removing
    *B601886,1  the size of latarget changed.
    IF TYPE('lcVldFunc') = 'C' .AND. !EMPTY(lcVldFunc) .AND. llReMV1By1
      lnLoop    = 1
      DO WHILE lnLoop <= ALEN(laTarget) AND ;
               ! (ALEN(laTarget,1) = 1 AND EMPTY(laTarget[1]))   
        *B601886,1  This code is to assign new item to be removed
        lsTarget = lnLoop 
        =lfvTarget()
      ENDDO 				&& Ending of DO loop
      lnLoop    = 0
    ELSE
      *B601886,1  Normal ReMoveAll   
      IF TYPE('lcVldFunc') = 'C' .AND. !EMPTY(lcVldFunc)
        IF !&lcVldFunc.(lnMovmnts)
          RETURN
        ENDIF  
      ENDIF
      *B801980,1 KHM 03/24/99 (Begin) Looping through the laSource array and
      *B801980,1              enabling all the elements of laSource array
      FOR lnCounter = 1 TO ALEN(laSource,1)
        lnPos = IIF(OCCURS("\",laSource[lnCounter])>0,2,1)
        laSource[lnCounter] = SUBSTR(laSource[lnCounter],lnPos,LEN(laSource[lnCounter]))
      ENDFOR
      *B602040,1 KHM 06/24/98 (End)
      DECLARE laTarget[1]
      laTarget =' '
      *B801980,1 KHM 03/24/99 (Begin) Commenting the following line as we
      *B801980,1              are not using the popup anymore.
      *SET SKIP OF POPUP puSource .F.
      *B801980,1 KHM 03/24/99 (End)
      SHOW GETS
    ENDIF
ENDCASE


*!*************************************************************************
*!
*!              Function: lfvCancel
*!
*!*************************************************************************
*
FUNCTION lfvCancel

DECLARE laTarget[lnOldDim]
=ACOPY(laOldTarg,laTarget)
*B801980 KHM 03/24/99 (Begin) Copying from the laOldSour to the laSource
DECLARE laSource[lnOldSour]
=ACOPY(laOldSour,laSource)
*B801980 KHM 03/24/99 (End)


FUNCTION lfCancel
=lfvCancel()
CLEAR READ