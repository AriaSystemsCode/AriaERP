*:----------------------------------------------------------------
*: Program file        : SMWMAIN.PRG
*: Program description : Company Wizard
*: For System          : Aria Advantage Series - Version 4.0
*: For Module          : System Manager - (SM)
*: Developer Name      : Tarek Mohammed Ibrahim
*: Tracking Job Number : T20080804.0011
*:----------------------------------------------------------------

DO FORM (oAriaApplication.ScreenHome+'SM\SMWMAIN.SCX') WITH 'SMWMAIN' NAME oSMWMAIN

*:**************************************************************************
*:* Name        : lfGetProp
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/07/2008
*:* Purpose     : Get property value from the wizard class
*:***************************************************************************
*:* Called from : SMWMAIN.SCX,INIT
*:***************************************************************************
*:* Parameters : lcPropName
*:*              lcRetType 
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfGetProp()
*:***************************************************************************
FUNCTION lfGetProp
LPARAMETERS lcPropName,lcRetType 
IF EMPTY(lcPropName)
  RETURN ''
ENDIF  
lcPropName = UPPER(lcPropName)
lcRetType = IIF(EMPTY(lcRetType),'C',lcRetType)
lcRetType = UPPER(lcRetType)

LOCAL lcUserFldText,lnLnNo,lcLine,lcPropValue,lcRet
lcUserFldText = UPPER(USER)

lcRet = ''
lnLnNo = ATLINE(lcPropName,lcUserFldText)
IF lnLnNo>0
lcLine = MLINE(lcUserFldText,lnLnNo)
lcPropValue = SUBSTR(lcLine,AT('=',lcLine)+1)

DO CASE
CASE lcRetType = 'C'
lcRet = lcPropValue
CASE lcRetType = 'L'
lcRet = EVALUATE(lcPropValue)
CASE lcRetType = 'N'
lcRet = Val(lcPropValue)
ENDCASE
ENDIF
RETURN lcRet
*-- end of lfGetProp.


*:**************************************************************************
*:* Name        : lfvFitScroll
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 09/21/2008
*:* Purpose     : fits the control when it is in unseen area
*:***************************************************************************
*:* Called from : page3
*:***************************************************************************
FUNCTION lfvFitScroll
PARAMETERS oCont

LOCAL lcName,lcJ
oCont.Parent.parent.parent.FocusControl = 'Outercontainer.Innercontainer.'+oCont.Name

*- Update my own property variable to save the current focus control
*04.0011 TMI 01/18/2009 [Start] 
*oCont.Parent.parent.parent.parent.myFocusControl = 'Outercontainer.Innercontainer.'+oCont.Name
lcJ = RIGHT(oCont.Parent.parent.parent.Name,1)
oCont.Parent.parent.parent.parent.FocusCntScroll&lcJ = 'Outercontainer.Innercontainer.'+oCont.Name


*- Set the color of labels to the default
oCont.Parent.SETALL('BACKCOLOR',RGB(236,233,216),'lblsetupwzlabel')

*- Set the current backcolor of the label to different color
oLabel = EVALUATE('oCont.Parent.lbl'+oCont.Name)
oLabel.BackColor = RGB(192,192,192)

*- Get the edtStepDesc refrence path
* this control.Innercontainer.Outercontainer.Ariascroll1.page3.Ariaform1.edtStepDesc
oStepDesc = oCont.Parent.parent.parent.parent.parent.edtStepDesc
*- Set the help of the current item


IF gfSeek(PADR(oCont.cModuleId,3)+oCont.Name,'SYWHELP')
  oStepDesc.Value = SYWHELP.mHelp
ELSE
  oStepDesc.Value = 'No help is associated to this item'
ENDIF


*-- end of lfvFitScroll.
*TAREK [end]


*:***************************************************************************
* Below the functions that are validation for the options used in the setup
*:***************************************************************************



*:**************************************************************************
*:* Name        : lfvUseExt
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 09/22/2008
*:* Purpose     : Valid setup fn. for the variable M_USEEXSSC
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvUseExt()
*:***************************************************************************
FUNCTION lfvUseExt
LOCAL loFormSet,lcVal
loFormSet = oAriaApplication.oToolBar.oWindParent
lcVal = _Screen.ActiveForm.Activecontrol.Value
loFormSet.Ariaform1.Page5.cboStru.Value = IIF(lcVal='.T.','Extended Size Scale','Standard Size Scale')
loFormSet.Ariaform1.Page5.cboStru.Refresh

loFormSet.Ariaform1.Page5.cboStru.InteractiveChange()

*-- end of lfvUseExt.

*:**************************************************************************
*:* Name        : lfvClrAstm
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 09/22/2008
*:* Purpose     : Valid setup fn. for the variable MCLRASSORT
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvClrAstm()
*:***************************************************************************
FUNCTION lfvClrAstm
*- MCLRASSORT

*-- end of lfvClrAstm.

*:**************************************************************************
*:* Name        : lfvmaCsM
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 09/22/2008
*:* Purpose     : Valid setup fn. for the variable M_MATCSTMT
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvmaCsM()
*:***************************************************************************
FUNCTION lfvmaCsM
*- M_MATCSTMT

*-- end of lfvmaCsM.

*:**************************************************************************
*:* Name        : lfvTrkRolls
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 09/22/2008
*:* Purpose     : Valid setup fn. for the variable M_TRKROLLS
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvTrkRolls()
*:***************************************************************************
FUNCTION lfvTrkRolls
*- M_TRKROLLS

*-- end of lfvTrkRolls.

*:**************************************************************************
*:* Name        : lfvGenRolId
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 09/22/2008
*:* Purpose     : Valid setup fn. for the variable M_GENROLID
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvGenRolId()
*:***************************************************************************
FUNCTION lfvGenRolId
*- M_GENROLID

*-- end of lfvGenRolId.

*:**************************************************************************
*:* Name        : lfvAtomInt
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 09/22/2008
*:* Purpose     : Valid setup fn. for the variable M_USEATOM 
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvAtomInt()
*:***************************************************************************
FUNCTION lfvAtomInt
*- M_USEATOM 

*-- end of lfvAtomInt.

*:**************************************************************************
*:* Name        : lfvAtomDir
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 09/22/2008
*:* Purpose     : Valid setup fn. for the variable M_ATOMDIR 
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvAtomDir()
*:***************************************************************************
FUNCTION lfvAtomDir
*- M_ATOMDIR 

*-- end of lfvAtomDir.

*:**************************************************************************
*:* Name        : lfvMulCurr
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 09/22/2008
*:* Purpose     : Valid setup fn. for the variable LLMULCURR 
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvMulCurr()
*:***************************************************************************
FUNCTION lfvMulCurr
*- LLMULCURR 

*-- end of lfvMulCurr.
*:**************************************************************************
*:* Name        : lfvTaxSt
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 09/22/2008
*:* Purpose     : Valid setup fn. for the variable M_TAX     
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvTaxSt()
*:***************************************************************************
FUNCTION lfvTaxSt
*- M_TAX     

*-- end of lfvTaxSt.


*:**************************************************************************
*:* Name        : lfvLtoGL
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 09/22/2008
*:* Purpose     : Valid setup fn. for the variable M_LINK_GL 
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvLtoGL()
*:***************************************************************************
FUNCTION lfvLtoGL
*- M_LINK_GL 

*-- end of lfvLtoGL.

*:**************************************************************************
*:* Name        : lfvGLVers
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 09/22/2008
*:* Purpose     : Valid setup fn. for the variable M_GL_VERS 
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvGLVers()
*:***************************************************************************
FUNCTION lfvGLVers
*- M_GL_VERS 

*-- end of lfvGLVers.

*:**************************************************************************
*:* Name        : lfvSysDir
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 09/22/2008
*:* Purpose     : Valid setup fn. for the variable M_SYS_DIR 
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvSysDir()
*:***************************************************************************
FUNCTION lfvSysDir
*- M_SYS_DIR 

*-- end of lfvSysDir.

*:**************************************************************************
*:* Name        : lfvGLCo
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 09/22/2008
*:* Purpose     : Valid setup fn. for the variable M_GL_CO   
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvGLCo()
*:***************************************************************************
FUNCTION lfvGLCo
*- M_GL_CO   

*-- end of lfvGLCo.


FUNCTION lfvM_DOVW
RETURN VAL(M_DOVW)>0

*-- end of lfvM_DOVW.

FUNCTION lfvM_AOVW
RETURN VAL(M_AOVW)>0
*-- end of lfvM_AOVW.


FUNCTION lfvM_TAX_RATE
RETURN VAL(M_TAX_RATE)>0
*-- end of lfvM_TAX_RATE.


*:**************************************************************************
*:* Name        : lfvAOVW
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/12/2009
*:* Purpose     : Adornment Order Vis. wind
*:***************************************************************************
FUNCTION lfvAOVW
* VALIDATION HAVE TO BE DONE HERE

*-- end of lfvAOVW.