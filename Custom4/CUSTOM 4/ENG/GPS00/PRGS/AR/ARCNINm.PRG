***********************************************************************
*:  Program file : ARCNINM.PRG
*:  Program desc.: Add option to Direct Invoice screen to scan style barcode
*:         System: Aria 4XP
*:      Developer: Mariam Mazhar[MMT]
*:           Date: 12/19/2012
*:      Reference: C201543.Exe,C201542.EXE[T20121218.0008]
*:************************************************************************
FUNCTION  gfModalGen 
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
  oAriaApplication.oToolBar.Enabled = .T.
  RELEASE oMessageBox
  PRIVATE lnMessageChoice
  lnMessageChoice = lnChoice
  RETURN lnMessageChoice  && Return the response.
ENDFUNC 

