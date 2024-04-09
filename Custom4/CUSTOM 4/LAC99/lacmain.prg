*!**************************************************************************
*! Name      : LACMAIN.PRG
*! Developer : Mariam Mazhar[MMT]
*! Date      : 01/15/2020
*! Purpose   : La Cera Custom Process Program.
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : Logical value.       C201235,1
*!**************************************************************************
*! Modifications
*E612028,1 MMT 01/15/2020 Add trigger to calculate package weight based on styles sizes weight[T20191223.0003]
*!**************************************************************************
PARAMETER loFormSet,lcEvntFun,lcFunPars

lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLT(lcEvntFun)+'('+lcFunPars+')'

*--Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue 



FUNCTION LFDFNLINRESN

Local lnCntBar
  lcHostFormName = '[' + loFormSet.cHostFormName + ']'
  lnCntBar = Cntbar('_INQURYPOP')+1
  Define Bar lnCntBar Of _INQURYPOP Prompt 'Edit Adjustment Reason' Skip For ;
gfFormIsActive(&lcHostFormName) .And. ((!_Screen.ActiveForm.Parent.ActiveMode$'EV') OR _Screen.ActiveForm.Parent.Ariaform1.Ariapageframe1.ActivePage<>2)

  On Selection Bar lnCntBar Of _INQURYPOP Do lfcallAdjReasonForm

  loFormSet.NextOptionBar = loFormSet.NextOptionBar + 1




FUNCTION lfcallAdjReasonForm

loFormSet = _Screen.ActiveForm.Parent
 
IF loFormSet.ActiveMode $ 'SA'
  RETURN 
ENDIF
lcOrdHDr = IIF(loFormSet.ActiveMode ='E',loFormSet.OFORMENVIRONMENT.lcOrdHdr,'ORDHDR')
lnDataSessV = SET("Datasession")
SET DATASESSION TO loFormSet.DataSessionID
IF &lcOrdHDr..cOrdType ='O' AND &lcOrdHDr..LEDIORDER
   IF !USED('EDIACPRT')
     =gfOpenTable('EDIACPRT','ACCFACT'  ,'SH')
   ENDIF
   IF !USED('EDIPD')
     =gfOpenTable('EDIPD'   ,'PARTTRANS','SH')
   ENDIF   
ELSE
  RETURN    
ENDIF   
llEdiAct = gfSEEK('A'+&lcOrdHDr..Account,'EDIACPRT') AND gfSEEK(EDIACPRT.cpartcode + '865','EDIPD')
IF llEdiAct 
  DO FORM (oAriaApplication.ClientScreenHome+"\SO\SOEDTADJR.SCX") WITH loFormSet 
ENDIF
SET DATASESSION TO lnDataSessV 


*E612028,1 MMT 01/15/2020 Add trigger to calculate package weight based on styles sizes weight[T20191223.0003][Start]
FUNCTION lfGETSZWGHT 
lnOldAlsSel = ALIAS()
IF !USED('STYLESIZEATTRIBUTES')
  =gfOpenTable('STYLESIZEATTRIBUTES','STYSATTR')
ENDIF
IF !USED('SCALE_EX')
  =gfOpenTable('SCALE','SCALE','SH','SCALE_EX')
ENDIF

IF gfSeek(Ordline.Style,'STYLESIZEATTRIBUTES','STYSATTR')
  lnScaleCnt = IIF(gfSeek('S'+ORDLINE.SCALE,'SCALE_EX'),SCALE_EX.CNT,8)
  FOR lnSzCnt = 1 TO lnScaleCnt 
    lcSzNo = STR(lnSzCnt,1)
    IF gfSeek(Ordline.Style+lcSzNo ,'STYLESIZEATTRIBUTES','STYSATTR')
      IF STYLESIZEATTRIBUTES.NSTYWEIGHT > 0
        lnTotWght = lnTotWght + (IIF(!EMPTY(Ordline.Piktkt),ORdline.Pik&lcSzNo.,ORDLINE.QTy&lcSzNo.) * STYLESIZEATTRIBUTES.NSTYWEIGHT)  
      ELSE
        lnTotWght = lnTotWght + (IIF(!EMPTY(Ordline.Piktkt),ORdline.Pik&lcSzNo.,ORDLINE.QTy&lcSzNo.) * style.nstyweight)    
      ENDIF
    ENDIF
  ENDFOR
ELSE
  lnTotWght = lnTotWght + (IIF(!EMPTY(Ordline.Piktkt),ORdline.TotPik,ORDLINE.TOTQTy) * style.nstyweight)  
ENDIF
SELECT(lnOldAlsSel)

FUNCTION lfUPSZWGHT
 lnoldalssel = ALIAS()
 IF  !USED('STYLESIZEATTRIBUTES')
    = gfopentable('STYLESIZEATTRIBUTES', 'STYSATTR')
 ENDIF
 SELECT (loformset.lcpcklin)
 LOCATE
 SCAN FOR  !DELETED()
    IF gfseek(EVALUATE(loformset.lcpcklin+'.Style')+EVALUATE(loformset.lcpcklin+'.cSizeNo'), 'STYLESIZEATTRIBUTES', 'STYSATTR')
       IF stylesizeattributes.nstyweight>0
          REPLACE stywgh WITH stylesizeattributes.nstyweight
       ENDIF
    ENDIF
 ENDSCAN
 SELECT (lnoldalssel)
*E612028,1 MMT 01/15/2020 Add trigger to calculate package weight based on styles sizes weight[T20191223.0003][End]