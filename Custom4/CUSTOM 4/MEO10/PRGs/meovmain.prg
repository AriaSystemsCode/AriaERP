*:****************************************************************************************
*: Name      : Main Program for Visual Foxpro modules
*: Developer : Waleed Hamed Zekr Allah (WLD)
*: Date      : 04/18/2006
*: Purpose   : Main program for EDI Triggers
******************************************************************************************
*: Return      : None
******************************************************************************************
*: Program desc. : Main Program.
*:****************************************************************************************
*C201114,1 WLD 03/05/2009 Send warehouse BOL instead of Aria BOL
*B608859,1 WLD 05/03/2009 ASN's Missing Pack Detail - Restore BOL not correct
*:****************************************************************************************
PARAMETER lcEvntFun,lcFunPars
lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLTRIM(lcEvntFun)+'('+lcFunPars+')'

*-- Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue
*-- End of Program.

*:****************************************************************************************
*! Name      : lfWHBOL
*! Developer : Walid Hamed (WLD)
*! Date      : 03/05/2009
*! Purpose   : Send warehouse BOL instead of Aria BOL
*:****************************************************************************************
*! Called from : class Send810\Send856 PrnLabel
*:****************************************************************************************
*! Calls       :
*:****************************************************************************************
*! Return      : None
*C201114,1 WLD 03/05/2009 Send warehouse BOL instead of Aria BOL
*:****************************************************************************************
FUNCTION lfWHBOL
*B608859,1 WLD ASN's Missing Pack Detail - Restore BOL not correct 05/03/2009  [Begin]
Public lcTmpBOL_MEO,lcTmpMUCB_MEO
*B608859,1 WLD ASN's Missing Pack Detail - Restore BOL not correct 05/03/2009  [End]

  *B608859,1 WLD ASN's Missing Pack Detail - Restore BOL not correct 05/03/2009  [Begin]
  *lcTmpBOL = IIF(EDITrans.cEdiTrnTyp = '810',MBOL,MBOL_NO)
  *lcTmpMUCB = MUCB
  lcTmpBOL_MEO = IIF(EDITrans.cEdiTrnTyp = '810',MBOL,MBOL_NO)
  lcTmpMUCB_MEO = MUCB
  *B608859,1 WLD ASN's Missing Pack Detail - Restore BOL not correct 05/03/2009  [End]
  

  IF 'AS' $ OAriaApplication.CompanyInstalledModules

    IF EDITrans.cEdiTrnTyp = '810'
      MBOL = IIF(!EMPTY(BOL_HDR.cFactBOL),BOL_HDR.cFactBOL,MBOL)
    ELSE
      MBOL_NO = IIF(!EMPTY(BOL_HDR.cFactBOL),BOL_HDR.cFactBOL,MBOL_NO)
    ENDIF
    MUCB  = IIF(!EMPTY(BOL_HDR.cFactBOL),BOL_HDR.cFactBOL,MUCB)
  ENDIF

ENDFUNC
*:****************************************************************************************
*! Name      : lfRESTBOL
*! Developer : Walid Hamed (WLD)
*! Date      : 03/05/2009
*! Purpose   : Restore Aria BOL instead of warehouse BOL
*:****************************************************************************************
*! Called from : class Send810\Send856 PrnLabel
*:****************************************************************************************
*! Calls       :
*:****************************************************************************************
*! Return      : None
*C201114,1 WLD 03/05/2009 Send warehouse BOL instead of Aria BOL
*:****************************************************************************************
FUNCTION lfRESTBOL
*B608859,1 WLD ASN's Missing Pack Detail - comment out 05/03/2009  [Begin]
*  lcTmpBOL= IIF(EDITrans.cEdiTrnTyp = '810',MBOL,MBOL_NO)
*  lcTmpMUCB = MUCB
*  IF 'AS' $ OAriaApplication.CompanyInstalledModules
*    IF EDITrans.cEdiTrnTyp = '810'
*      MBOL = lcTmpBOL
*    ELSE
*      MBOL_NO = lcTmpBOL
*    ENDIF
*    MUCB  = lcTmpMUCB
*  ENDIF
  IF 'AS' $ OAriaApplication.CompanyInstalledModules
    IF EDITrans.cEdiTrnTyp = '810'
      MBOL = lcTmpBOL_MEO
    ELSE
      MBOL_NO = lcTmpBOL_MEO
    ENDIF
    MUCB  = lcTmpMUCB_MEO
  ENDIF
 
Release lcTmpBOL_MEO,lcTmpMUCB_MEO
*B608859,1 WLD ASN's Missing Pack Detail - Restore BOL not correct 05/03/2009  [End]

ENDFUNC
