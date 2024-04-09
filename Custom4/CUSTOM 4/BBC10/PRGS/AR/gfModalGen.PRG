**:***********************************************************************
*:  Program file : gfModalGen.PRG
*:  Program desc.: Cusom messaging function for BBC10
*:         System: Aria 4XP
*:      Developer: Mariam Mazhar[MMT]
*:           Date: 09/15/2021
*:      Reference: C202430,1
*:************************************************************************
*************************************************************************************
*FUNCTION  gfModalGen 
  LPARAMETER lcDlgID,lcDlgTyp,lcVarsStr,lcDlgValid,lcDlgMessg
  IF TYPE("oAriaApplication.oToolBar") = 'O' .AND. !ISNULL(oAriaApplication.oToolBar)
    oAriaApplication.oToolBar.Enabled = .F.
  ENDIF

  LOCAL oMessageBox, llActiveFormLocked, llFormExist
  oMessageBox = NEWOBJECT("AriaMessageBox",ADDBS(oAriaApplication.ClassDir)+"Utility.vcx")
  IF VARTYPE(oMessageBox) != "O"

    IF TYPE("oAriaApplication.oToolBar") = 'O' .AND. !ISNULL(oAriaApplication.oToolBar)
      oAriaApplication.oToolBar.Enabled = .T.
    ENDIF

    RETURN 0
  ENDIF 
  
  *-- Get the dialog and buttons from the dictionary.
  IF !oMessageBox.GetMessage(lcDlgID,lcDlgTyp,lcVarsStr,lcDlgValid,lcDlgMessg)
    IF TYPE("oAriaApplication.oToolBar") = 'O' .AND. !ISNULL(oAriaApplication.oToolBar)
      oAriaApplication.oToolBar.Enabled = .T.
    ENDIF

    RETURN 0
  ENDIF 

  IF (TYPE("_SCREEN.ActiveForm.LockScreen") = "L") AND !EMPTY(TYPE("_SCREEN.ActiveForm.LockScreen")) AND TYPE('_SCREEN.ActiveForm') = 'O' AND !ISNULL(_SCREEN.ActiveForm)
    llFormExist = .T.
    llActiveFormLocked = _SCREEN.ActiveForm.LockScreen
    _SCREEN.ActiveForm.LockScreen = .F.
  ENDIF 

  PRIVATE lnMessageChoice
  lnMessageChoice = lnChoice
  RETURN lnMessageChoice  && Return the response.
ENDFUNC 