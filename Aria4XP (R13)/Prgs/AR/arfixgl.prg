lcTmpGLD = gfTempName()
=gfOpenFile(oAriaApplication.DataDir+'Customer','Customer','SH')
=gfOpenFile(oAriaApplication.DataDir+'ArHist','ArHistT','SH')
=gfOpenFile(oAriaApplication.DataDir+'ApPaymnt','','SH')
=gfOpenFile(oAriaApplication.DataDir+'GLDist',oAriaApplication.DataDir+'Gldistno','SH')
=gfOpenFile(oAriaApplication.DataDir+'Credit',oAriaApplication.DataDir+'CrTran','SH')

SELECT GLDIST
COPY STRUCTURE TO (oAriaApplication.WorkDir+lcTmpGLD)
= gfOpenFile(oAriaApplication.WorkDir+lcTmpGLD ,"","EX")


SELECT Credit
SEEK '4'
SCAN REST WHILE TranType+Tran = '4'
  lcTranNo = Tran
  =SEEK(IIF(EMPTY(Store),'M'+Account,'S'+Account+Store),'Customer')

  IF !SEEK(lcTranNo+'CR','GLDIST')
    WAIT WINDOW 'Updating GL for transaction # : ' + lcTranNo NOWAIT

    STORE '' TO lcGLFYear, lcGLPeriod

    = CheckPrd(Credit.TranDate,"lcGLFYear","lcGLPeriod",'CR', .T.)

    DO GLDIST WITH Credit.Link_Code,'002',ABS(Credit.Amount)      ,;
      'CR',Credit.TRAN,Credit.TranDate,lcGLFYear,lcGLPeriod,'&lcTmpGLD',;
       Credit.cAdjAcct,Credit.cCurrCode,Credit.nCurrUnit,Credit.nExRate

    DO GLDIST WITH Credit.Link_Code,'001',-ABS(Credit.Amount)     ,;
      'CR',Credit.TRAN,Credit.TranDate,lcGLFYear,lcGLPeriod,'&lcTmpGLD',;
      Credit.cARGlAcc,Credit.cCurrCode,Credit.nCurrUnit,Credit.nExRate

    SELECT ApPaymnt
    APPEND BLANK
    REPLACE cPayType  WITH "A"                  ,;
      cPayMeth  WITH IIF(Credit.lNonAr,'N','A') ,;
      cBnkCode  WITH Credit.cBnkCode            ,;
      cChkAcct  WITH Credit.cChkAcct            ,;
      dPayDate  WITH Credit.TranDate            ,;
      cFisFYear WITH lcGLFYear                  ,;
      cFspprdid WITH lcGLPeriod                 ,;
      cPayDocNo WITH Credit.STORE               ,;
      cPayClNo  WITH Credit.Account             ,;
      cPayComp  WITH Customer.BTName            ,;
      nPayAmnt  WITH Credit.Amount              ,;
      cCurrCode WITH Credit.cCurrCode           ,;
      nExRate   WITH Credit.nExRate             ,;
      nCurrUnit WITH Credit.nCurrUnit           ,;
      cPayRecSt WITH "O"                        ,;
      BATCH     WITH Credit.BATCH               ,;
      cOwner    WITH 'By Aria CR'

      =gfAdd_Info('ApPaymnt')
  ENDIF
ENDSCAN

SELECT Customer
SEEK 'M'
SCAN REST WHILE Type+Account+Store = 'M'

  IF SEEK(Account,'ArHist')
    SELECT ArHist
    SCAN REST WHILE Account+Tran+cInstalNo = Customer.Account ;
              FOR TranType = '4'
      lcTranNo = Tran
      IF !SEEK(lcTranNo+'CR','GLDIST')
        WAIT WINDOW 'Updating GL for transaction # : ' + lcTranNo NOWAIT

        STORE '' TO lcGLFYear, lcGLPeriod

        = CheckPrd(Credit.TranDate,"lcGLFYear","lcGLPeriod",'CR', .T.)

        DO GLDIST WITH ArHist.Link_Code,'002',ABS(ArHist.Amount)      ,;
          'CR',ArHist.TRAN,ArHist.TranDate,lcGLFYear,lcGLPeriod,'&lcTmpGLD',;
          ArHist.cAdjAcct,ArHist.cCurrCode,ArHist.nCurrUnit,ArHist.nExRate

        DO GLDIST WITH ArHist.Link_Code,'001',-ABS(ArHist.Amount)     ,;
           'CR',ArHist.TRAN,ArHist.TranDate,lcGLFYear,lcGLPeriod,'&lcTmpGLD',;
           ArHist.cARGlAcc,ArHist.cCurrCode,ArHist.nCurrUnit,ArHist.nExRate
        SELECT ApPaymnt
        APPEND BLANK
        REPLACE cPayType  WITH "A"                        ,;
                cPayMeth  WITH 'A'                        ,;
                cBnkCode  WITH ArHist.cBnkCode            ,;
                cChkAcct  WITH ArHist.cChkAcct            ,;
                dPayDate  WITH ArHist.TranDate            ,;
                cFisFYear WITH lcGLFYear                  ,;
                cFspprdid WITH lcGLPeriod                 ,;
                cPayDocNo WITH ArHist.STORE               ,;
                cPayClNo  WITH ArHist.Account             ,;
                cPayComp  WITH Customer.BTName            ,;
                nPayAmnt  WITH ArHist.Amount              ,;
                cCurrCode WITH ArHist.cCurrCode           ,;
                nExRate   WITH ArHist.nExRate             ,;
                nCurrUnit WITH ArHist.nCurrUnit           ,;
                cPayRecSt WITH "O"                        ,;
                BATCH     WITH ArHist.BATCH               ,;
                cOwner    WITH 'By Aria CR'
      ENDIF
    ENDSCAN
  ENDIF

ENDSCAN


lcGLSeqNo = gfSequence("GLSESSION", oAriaApplication.ActiveCompanyId)
SELECT (lcTmpGLD)
REPLACE ALL GLSESSION WITH lcGLSeqNo,;
            cOwner    WITH 'By Aria CR'
USE IN (lcTmpGLD)

SELECT GLDIST
APPEND FROM (oAriaApplication.WorkDir+lcTmpGLD)

IF FILE(oAriaApplication.WorkDir+lcTmpGLD)
  ERASE oAriaApplication.WorkDir+lcTmpGLD
ENDIF

WAIT WINDOW 'Updating GL has been completed successfully. Press any key to continue...'
