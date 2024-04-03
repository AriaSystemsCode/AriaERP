*:************************************************************************
*: Program file  : GFMOVER.PRG
*: Program desc. : Global Mover Screen
*:         System: Aria advantage series
*:         Module: Main system
*:************************************************************************
*: In oCalledFormSet parameter you should pass ThisFormSet
*: it will be used to validate the screen mode and also to run the valid
*: Method for select and remove mover items.
*:************************************************************************
FUNCTION gfMover

*E039820,1 WSH Enable adding new codes on the fly from the Mover Screen. [Start]
*PARAMETERS laSource,laTarget,lcMovTitle,llEditable,lcVldFunc,llMV1By1,llReMV1By1,oCalledFormSet
PARAMETERS laSource,laTarget,lcMovTitle,llEditable,lcVldFunc,llMV1By1,llReMV1By1,oCalledFormSet,cCodesField
*E039820,1 WSH [End]

*-Make the viriables private to be seen on form.
PRIVATE laSource,laTarget,laOldSour,laOldTarg,lnOldDim,lnOldSour,;
        lcVldFunc,llMV1By1,llReMV1By1,lnLoop,llFormReturn

llFormReturn = .T.  &&Default from return is .T. , .F. for Cancel.
*- Redimension source and target array adding column dimension
DIMENSION laSource[ALEN(laSource,1),1],laTarget[ALEN(laTarget,1),1]

*** This for the mover title. ***
lcMovTitle = IIF(TYPE("lcMovTitle") $ "UL" , "Mover" , IIF(EMPTY(lcMovTitle),"Mover" ,lcMovTitle))

*TTTT
LOCAL oCalledFromForm
llEditable = IIF(TYPE("llEditable") = "U" ,.F., llEditable)
IF llEditable
  llCanVald = .T.
ELSE
* llEditable = (laScrMode[3] .OR. laScrMode[4]) OR Forced to be .T. all the Time.
  llCanVald = (oCalledFormSet.ActiveMode $ "EA")
ENDIF

*Add this variable to hold number of item to be valid in <ReMoveAll> case because changes occur to laTarget.
lnLoop = 0

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


*E039820,1 WSH Enable adding new codes on the fly from the Mover Screen. [Start]
*DO FORM (oAriaApplication.ScreenHome+"sy\symover.scx") WITH lcMovTitle,llCanVald,oCalledFormSet
DO FORM (oAriaApplication.ScreenHome+"sy\symover.scx") WITH lcMovTitle,llCanVald,oCalledFormSet,cCodesField
*E039820,1 WSH [End]

RETURN llFormReturn
