***********************************************************************
*:  Program file : ARCNINC.PRG
*:  Program desc.: Auto create concession invoices and credit notes
*:         System: Aria 4XP
*:      Developer: Mariam Mazhar[MMT]
*:           Date: 09/18/2011
*:      Reference: C201384.Exe,C201383.122[T20110621.0057]
*:************************************************************************
*:C201384,1 MMT 11/03/2011 Fix the Problem of Error while saving CM[T20110621.0057]
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

  PRIVATE lnMessageChoice
  lnMessageChoice = lnChoice
  RETURN lnMessageChoice  && Return the response.
ENDFUNC 

