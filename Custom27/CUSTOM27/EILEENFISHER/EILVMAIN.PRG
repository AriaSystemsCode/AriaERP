*!**************************************************************************
*! Name      : EILVMAIN.PRG
*! Developer : WALEED HAMED (WLD)
*! Date      : 12/19/2004
*! Purpose   : EILEEN FISHER  Custom Process Program .
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : Logical value.  
*!**************************************************************************
*C125077,1 WLD 12/19/2004 Put 3 digit of Store No. instread of 000 .
*!**************************************************************************

PARAMETER lcEvntFun,lcFunPars

lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLTRIM(lcEvntFun)+'('+lcFunPars+')'

*-- Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue
*-- End of Program.

*!*************************************************************************
*! Name      : lfSTOREBOX
*! Developer : Waleed Hamed (WLD)
*! Date      : 12/19/2004
*! Purpose   : Put 3 digit of Store No. instread of 000 in the BOX Carton
*!**************************************************************************
*! Called from : Class "Send856"
*!**************************************************************************
*! Passed Parameters : ---
*!**************************************************************************
*C125077,1
FUNCTION lfSTOREBOX
*C125077,1 Passing the carton sequence to Variable, AEH [Start]
IF Sycedish.cMapset == 'KWI'
  Select CRTINFO
  =Seek (MPACKNO+MACCOUNT+PADL(ALLTRIM(STR(lnCarton,5)),5),'CRTINFO','CRTINFOP')
  XBOX_SER = RIGHT(ALLTRIM(MStore),3)+CRTINFO.CCARTSEQ
ENDIF 
*C125077,1 Passing the carton sequence to Variable, AEH [End]
*XBOX_SER   = RIGHT(ALLTRIM(MStore),3)+Substr(XBOX_SER,4,17)
ENDFUNC

*-- end of lfSTOREBOX.

*!**************************************************************************
*! Name      : lfOpenCRTINF
*! Developer : Amgad EL Halawany (AEH)
*! Date      : 06/05/2005
*! Purpose   : Open The CRTINFO File
*!**************************************************************************
*! Called from : Class "OpenFile"
*!**************************************************************************
*! Passed Parameters : ---
*!**************************************************************************

Function lfOpenCRTINF
    =lcobj.OPEN(oAriaApplication.DataDir,"CRTINFO","CRTINFOP")
ENDFun

*--End of lfOpenCRTINFO