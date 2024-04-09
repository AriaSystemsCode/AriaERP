*!*************************************************************
*! Name      : APAGEINV.PRG
*! Developer : Hesham Elmasry (HES)
*! Date      : 07/07/2009
*! Tracking  : E302628
*!*************************************************************
*! Modifications :
*E302628,3 TMI 08/09/2011 To enable the report in A4xp
*E302975,1 AP Conv.Proj. Attaching all files the phase to TMI 10/23/2011
*B610013,1 MMT 07/18/2012 AP aged report displays incorrect data[T20120711.0043]
*B610013,2 MMT 07/19/2012 AP aged report displays incorrect data[T20120711.0043]
*B610088,1 SAB 09/17/2012 Remove the AP account from lcRpExp when the AP account is empty [T20120711.0043]
*B610129,1 HIA 10/23/2012 Modify fields names, when export to excel [T20121019.0002]
*B610168,1 SAB 12/05/2012 Fix problem of xls and xlsx export are not the same [T20121129.0004]
*E303613,1 MMT 10/25/2015 Add Invoice date to Aged Payble report export to excel[T20151008.0005]
*:************************************************************************
*B610129,1 HIA 10/23/2012 Modify fields names, when export to excel [T20121019.0002][Begin]
#INCLUDE R:\ARIA4XP\REPORTS\AP\APAGEINV.H
*B610129,1 HIA 10/23/2012 Modify fields names, when export to excel [T20121019.0002][End]

llUseMulCr = gfGetMemVar('LLMULCURR')

IF !USED('APDISTDS')
  =gfOpenTABLE(oAriaApplication.DATADIR+'APDIST','INVVEND','SH','APDISTDS',.T.)   && CINVNO+CVENDCODE+CAPDTRTYP
ENDIF
SELECT APDISTDS
SET ORDER TO INVVEND DESC

llShowGT = IIF(lcRpCurr="F",IIF(EMPTY(laogfxflt[1,6]),.F.,.T.),.T.)
lcExSin1 = ' '
lcExSin2 = ' '

*B610129,1 HIA 10/23/2012 Modify fields names, when export to excel [T20121019.0002][Begin]
*IF loOgScroll.llOGFltCh 
IF loOgScroll.llOGFltCh OR (TYPE(lcPriTmp+'.nbeforDetd')='U')
*B610129,1 HIA 10/23/2012 Modify fields names, when export to excel [T20121019.0002][End
  lnTotPaymnt = 0
  llCanPrint  = .F.

  =lfCrtTemp()

  IF !lfColData()
    RETURN
  ENDIF
ENDIF

SELECT (lcPriTmp)
IF EMPTY(laOGFxFlt[1,6]) .AND. lcRpCurr <> 'F'
  SET ORDER TO cVenCurTag
ELSE
  SET ORDER TO CURVENINV
ENDIF
*If user wants to display all invoice regardless of the currency code, change
* the index to be Vendor Code  + Currency + invoice No.

*B610129,1 HIA 10/23/2012 Modify fields names, when export to excel [T20121019.0002][Begin]
*B610168,1 SAB 12/05/2012 Fix problem of xls and xlsx export are not the same [Start]
*IF loogscroll.cTextRepType == "EXCEL"
IF loOGScroll.cTextRepType = "EXCEL"
*B610168,1 SAB 12/05/2012 Fix problem of xls and xlsx export are not the same [End]
  SELECT (lcPriTmp)
  lcLANG_TOTALPAYLABEL = LANG_TOTALPAYLABEL
  lcLANG_DISCOFFLABEL  = LANG_DISCOFFLABEL
  lcBefore = LANG_BEFORELABEL+'_'+ALLTRIM(STRTRAN(cBefore,'/','_'))
  lcBefore = STRTRAN(lcBefore,' ','')
  lcBefore = STRTRAN(lcBefore,'-','_')  
  lcPrd4   = IIF(!EMPTY(alltrim(cprd4ha)), LANG_FROMLABEL+'_'+ALLTRIM(STRTRAN(cprd4ha,'/','_'))+'_'+LANG_TOLABEL,LANG_PERIODLABEL)+'_'+ALLTRIM(STRTRAN(cprd4hb,'/','_'))
  lcPrd4   = STRTRAN(lcPrd4,' ','')
  lcPrd4   = STRTRAN(lcPrd4,'-','_')  
  lcPrd3   = IIF(!EMPTY(alltrim(cprd3ha)),LANG_FROMLABEL+'_'+ALLTRIM(STRTRAN(cprd3ha,'/','_'))+'_'+LANG_TOLABEL,LANG_PERIODLABEL)+'_'+ALLTRIM(STRTRAN(cprd3hb,'/','_'))
  lcPrd3   = STRTRAN(lcPrd3,' ','')
  lcPrd3   = STRTRAN(lcPrd3,'-','_')  
  lcPrd2   = IIF(!EMPTY(alltrim(cprd2ha)),LANG_FROMLABEL+'_'+ALLTRIM(STRTRAN(cprd2ha,'/','_'))+'_'+LANG_TOLABEL,LANG_PERIODLABEL)+'_'+ALLTRIM(STRTRAN(cprd2hb,'/','_'))
  lcPrd2   = STRTRAN(lcPrd2,' ','')
  lcPrd2   = STRTRAN(lcPrd2,'-','_')  
  lcPrd1   = IIF(!EMPTY(alltrim(cprd1ha)),LANG_FROMLABEL+'_'+ALLTRIM(STRTRAN(cprd1ha,'/','_'))+'_'+LANG_TOLABEL,LANG_PERIODLABEL)+'_'+ALLTRIM(STRTRAN(cprd1hb,'/','_'))
  lcPrd1   = STRTRAN(lcPrd1,' ','')
  lcPrd1   = STRTRAN(lcPrd1,'-','_')  
  lcAfter  = IIF(!EMPTY(alltrim(caftera)),ALLTRIM(STRTRAN(caftera,'/','_')) +'_','')+ALLTRIM(STRTRAN(cafterb,'/','_'))
  lcAfter  = STRTRAN(lcAfter,' ','')
  lcAfter  = STRTRAN(lcAfter,'-','_')

  ALTER TABLE (lcPriTmp) RENAME nbeforDetd TO (lcBefore) RENAME nprd4detd  TO (lcPrd4  ) RENAME nprd3detd  TO (lcPrd3  ) RENAME nprd2detd  TO (lcPrd2  ) ;
    RENAME nprd1detd  TO (lcPrd1  ) ;
    RENAME nafterdetd TO (lcAfter ) RENAME nTotPyblsd TO (lcLANG_TOTALPAYLABEL) rename nDisOfrdd to (lcLANG_DISCOFFLABEL) 

  *SELECT nbeforDetd as (lcBefore),nprd4detd as (lcPrd4),nprd3detd as (lcPrd3),nprd2detd as (lcPrd2),nprd1detd as (lcPrd1),nafterdetd as (lcAfter), * FROM (lcPriTmp) INTO CURSOR Cur_Temp
  *SELECT Cur_Temp

ENDIF

*B610129,1 HIA 10/23/2012 Modify fields names, when export to excel [T20121019.0002][End]

DO gfDispRe WITH EVAL('lcRpForm')


*B610129,1 HIA 10/23/2012 Modify fields names, when export to excel [T20121019.0002][Begin]
*B610168,1 SAB 12/05/2012 Fix problem of xls and xlsx export are not the same [Start]
*IF loogscroll.cTextRepType == "EXCEL"
IF loOGScroll.cTextRepType = "EXCEL"
*B610168,1 SAB 12/05/2012 Fix problem of xls and xlsx export are not the same [End]
  *USE IN Cur_Temp
  SELECT (lcPriTmp)
  loOgScroll.llOGFltCh = .T.
  loogscroll.cTextRepType     = ""

ENDIF
*B610129,1 HIA 10/23/2012 Modify fields names, when export to excel [T20121019.0002][End]

*!**************************************************************************
*! Name      : lfColData
*! Developer : Hesham Elmasry
*! Date      : 07/07/2009
*! Purpose   : Is used to get all the AP invoices according to the
*!             selected criteria and insert it in a temporary file to
*!             be used in the print process.
*!**************************************************************************
*! Example   :  =lfColData()
*!**************************************************************************
FUNCTION lfColData
PRIVATE lnTotRec

PRIVATE lcVenPmthd,lcCurrCod,lcVenCode,lcDivision,lnVendrs,lcFltExpr,lcApAccnt,ldPostDat,lnInvAmnt

lcVenPmthd = lfCheckFilter(3,"APINVHDR.CVENPMETH")
lcVenCode  = lfCheckFilter(3,"APINVHDR.CVENDCODE")
lcDivision = lfCheckFilter(3,"APINVHDR.CDIVISION")
ldPostDat  = lfCheckFilter(3,"APINVHDR.DPOSTDATE")
lcCurrCod  = lfCheckFilter(1,"APINVHDR.CCURRCODE")
lcApAccnt  = lfCheckFilter(1,"APINVHDR.CAPACCT")

ldPostDat = IIF(TYPE('ldPostDat')='C',CTOD(ldPostDat),ldPostDat)

STORE 0 TO lnVendrs
IF !EMPTY(lcVenCode)
  SELECT(lcVenCode)
  *- go top first
  LOCATE
  COUNT FOR !DELETED() TO lnVendrs
ENDIF

*- as there are hidden filters then the variable lcRpExp always not empty
=lfPolishExp(@lcRpExp,"APINVHDR.CVENDCODE")
*E302628,4 Aged Payable TMI 10/20/2011 [Start] remove the CDIVISION filter and replace it with a one linked with a varialbe to removed the error 'Too many arguments'
=lfPolishExp(@lcRpExp,"(APINVHDR.CDIVISION")
lcRpExp = lcRpExp + IIF(EMPTY(lcDivision),'',IIF(!EMPTY(lcRpExp)," AND ","")+"APINVHDR.CDIVISION "+IIF(LEN(lcDivision)>7,"$","=")+" lcDivision")
*E302628,4 Aged Payable TMI 10/20/2011 [End  ]
lcRpExp = lcRpExp + IIF(EMPTY(lcRpRefnce),"","AND LIKE('"+STRTRAN(lcRpRefnce,' ','?')+"',APINVHDR.cinvref)" )
*E302628,4 Aged Payable TMI 10/23/2011 [Start] change ldRpCurDat to static value to not being evaluated each line
*lcRpExp = lcRpExp + IIF(EMPTY(lcRpExp),""," AND ") + [IIF(cInvStat='V',;
IIF(SEEK(cInvNo + cVendCode,'APDISTDS') AND ;
DTOS(ApDistDs.dApdTrDat) > DTOS(ldRpCurDat),.T.,.F.),.T.)]
lcRpExp = lcRpExp + IIF(EMPTY(lcRpExp),""," AND ") + ;
  "(cInvStat<>'V' OR (SEEK(cInvNo+cVendCode,'APDISTDS') AND DTOS(ApDistDs.dApdTrDat)>'"+DTOS(ldRpCurDat)+"'))"
*E302628,4 Aged Payable TMI 10/23/2011 [End  ]

*- check llRpConPay
IF llRpConPay
  lcRpExp = "ABS(apinvhdr.ninvamnt) > ABS(apinvhdr.ninvpaid) + ABS(apinvhdr.ninvadj) + ABS(apinvhdr.ninvdistk) AND " + lcRpExp
ELSE
  SET ORDER TO TAG INVVEND IN APDIST DESC
ENDIF


*B610088,1 SAB 09/17/2012 Remove the AP account from lcRpExp when the AP account is empty [Start]
*- Remove AP/Account from filter expression if its values is empty
lnPrtCnt = OCCURS(' AND ', lcRpExp)+1
lcNewExp = ''
FOR lnCnt = 1 TO lnPrtCnt
  lnStrPos = IIF(lnCnt = 1, 1, AT(' AND ', lcRpExp, lnCnt-1))
  lnEndPos = IIF(lnCnt <> lnPrtCnt, AT(' AND ', lcRpExp, lnCnt),LEN(lcRpExp)+1)
  lcExpPrt = STRTRAN(SUBSTR(lcRpExp, lnStrPos, lnEndPos - lnStrPos), ' AND ', '')
  IF 'APINVHDR.CAPACCT' $ lcExpPrt
    lcAccPrt = STRTRAN(lcExpPrt, 'APINVHDR.CAPACCT', '')
    lcAccPrt = STRTRAN(lcAccPrt, '=', '')
    lcAccPrt = STRTRAN(lcAccPrt, '-', '')
    lcAccPrt = STRTRAN(lcAccPrt, "'", '')
    lcAccPrt = ALLTRIM(lcAccPrt)
    IF !EMPTY(lcAccPrt)
      lcNewExp = lcNewExp + IIF(EMPTY(lcNewExp), '', ' AND ') + lcExpPrt
    ENDIF
  ELSE
    lcNewExp = lcNewExp + IIF(EMPTY(lcNewExp), '', ' AND ') + lcExpPrt
  ENDIF
ENDFOR
lcRpExp = lcNewExp
*B610088,1 SAB 09/17/2012 Remove the AP account from lcRpExp when the AP account is empty [End]

SELECT APINVHDR

*E302628,4 Aged Payable TMI 10/20/2011 [Start] add the progress bar indicator to the report
oProgress = NEWOBJECT('ariaprogressbar',oAriaApplication.classdir+'utility.vcx')
oProgress.TotalProgress = RECCOUNT('APVENDOR')
oProgress.lblFirstLabel.CAPTION = 'Aged Payable Invoices'
lcLastVend = ' '
lnThermNo = 0
lnTotCnt = MAX(RECCOUNT('APVENDOR'),1)
oProgress.SHOW()
*E302628,4 Aged Payable TMI 10/20/2011 [End  ]

IF lnVendrs > 0
  SELECT(lcVenCode)
  *- be sure you go top
  LOCATE
  SCAN FOR !EOF()
    SELECT APINVHDR
    IF SEEK(&lcVenCode..cvendcode)
      =lfAddLn(&lcVenCode..cVendCode) && Add lines for this specific Vendor in the primary temp.
    ENDIF
  ENDSCAN
ELSE
  SELECT APINVHDR
  =SEEK('')
  =lfAddLn() && Add lines for All Vendors in the primary temp
ENDIF

*E302628,4 Aged Payable TMI 10/20/2011 [Start] remove the prgress bar
oProgress = NULL
*E302628,4 Aged Payable TMI 10/20/2011 [End  ]

WAIT CLEAR

*-- Check if the temporary file was empty then display a notification
*-- message that no record was selected.
SELECT (lcPriTmp)
LOCATE
IF EOF()
  =gfModalGen('TRM00052B00000','DIALOG' )
  SET DEVICE TO SCREEN
  RETURN .F.
ENDIF

*!*************************************************************
*! Name      : lfAddLn
*! Developer : Hesham Elmasry(HES)
*! Date      : 07/21/2009
*! Purpose   : Add line for vendors to the primary temp file
*!*************************************************************
FUNCTION lfAddLn
PARAMETERS lcVendCode
lcVendCode = IIF(EMPTY(lcVendCode),'',lcVendCode)

SELECT (lcPriTmp)
SET ORDER TO

SELECT APINVHDR

lnTotRec = RECCOUNT()

SCAN REST WHILE CVENDCODE+CINVNO = lcVendCode ;
    FOR &lcRpExp

  *E302628,4 Aged Payable TMI 10/20/2011 [Start] remove this message and use progress bar instead
  *WAIT WINDOW NOWAIT APINVHDR.CVENDCODE + APINVHDR.CINVNO
  *E302628,4 Aged Payable TMI 10/20/2011 [End  ]

  *E302628,4 Aged Payable TMI 10/20/2011 [Start] move this down
  *!*	  SELECT APDIST
  *!*	  =SEEK(APINVHDR.CINVNO+APINVHDR.CVENDCODE)
  *E302628,4 Aged Payable TMI 10/20/2011 [End  ]

  SELECT APPAYMNT
  =SEEK("P"+APDIST.CAPDTRTYP+PADR(RTRIM(APDIST.CAPDREF),8," ")+APDIST.CBNKCODE+APDIST.CCHKACCT)

  SELECT APINVHDR
  *E302628,4 Aged Payable TMI 10/20/2011 [Start]
  *!*	  IF ApDist.dAPDtrdat <= ldRpCurDat
  *!*	    lnAmtToPay = nInvAmnt-nInvPaid-nInvDistk-nInvAdj
  *!*	    *-- if the aging date < Payment date I get the lnAmtToPay by Decreasing the invoice amount
  *!*	    *-- with any changed on the invoice but multiply the Ex_rate in the ApinvHdr file to Get
  *!*	    *-- the Paid Amount by the Base Currency that Because I get lnTotPaymnt From the (lfSumALL) Function
  *!*	    *-- By the Base Currency , in this case the Two Amount will be equal and lost them with Each +/-
  *!*	    *-- so I get the invoice amount as net
  *!*	  ELSE
  *E302628,4 Aged Payable TMI 10/20/2011 [End  ]
  lnAmtToPay = APINVHDR.nInvAmnt-APINVHDR.nInvPaid-APINVHDR.nInvDistk-APINVHDR.nInvAdj
  *E302628,4 Aged Payable TMI 10/20/2011 [Start]
  *!*	  ENDIF
  *E302628,4 Aged Payable TMI 10/20/2011 [End  ]


  *E302628,4 Aged Payable TMI 10/20/2011 [Start]
  *lnAmtToPay = lnAmtToPay - lfViodPayt(cVendCode,cInvNo)
  lnTotPaymnt = 0
  llCanPrint  = .F.
  SELECT APDIST
  =SEEK(APINVHDR.CINVNO+APINVHDR.CVENDCODE)
  IF APPAYMNT.lPayAdvAn
    SUM REST nAPdAmnt WHILE cInvNo+cVendCode+cApdTrTyp = APINVHDR.CINVNO+APINVHDR.CVENDCODE;
      FOR dAPDtrdat <= ldRpCurDat AND cApdStat  = 'V'  AND cApdActID = "A" AND capdtrtyp $ "MNHP" AND nAPdAmnt < 0 ;
      TO lnTotPaymnt
  ELSE
    SUM REST nAPdAmnt WHILE cInvNo+cVendCode+cApdTrTyp = APINVHDR.CINVNO+APINVHDR.CVENDCODE;
      FOR  dAPDtrdat <= ldRpCurDat AND cApdStat  = 'V'  AND cApdActID = "A" AND capdtrtyp $ "MNHP" ;
      TO lnTotPaymnt
  ENDIF
  *E302628,4 Aged Payable TMI 10/20/2011 [End  ]
  *E302628,4 Aged Payable TMI 10/20/2011 [End  ]

  *E302628,4 Aged Payable TMI 10/20/2011 [Start]
  IF apinvhdr.ninvpaid + apinvhdr.ninvdistk + apinvhdr.ninvadj  = 0
    llCanPrint  = .T.
    lnTotPaymnt = 0
  ELSE
    *E302628,4 Aged Payable TMI 10/20/2011 [End  ]

    *-- Calling lfSumAll() to get the open amount of the invoice within
    *-- the date that was entered in the Option Grid.
    *B610013,1 MMT 07/18/2012 AP aged report displays incorrect data[T20120711.0043][Start]
    *lnAmtToPay = lnAmtToPay + lfSumall(cVendCode,cInvNo)
    lnAmtToPay = lnAmtToPay + lfSumall(apinvhdr.cVendCode,apinvhdr.cInvNo)
    *B610013,1 MMT 07/18/2012 AP aged report displays incorrect data[T20120711.0043][End]
    *E302628,4 Aged Payable TMI 10/20/2011 [Start]
  ENDIF
  *E302628,4 Aged Payable TMI 10/20/2011 [End  ]

  *E302628,4 Aged Payable TMI 10/20/2011 [Start] use the progress bar instead
  IF lcLastVend <> APINVHDR.CVENDCODE
    lnThermNo = lnThermNo + 1
    lcLastVend = APINVHDR.CVENDCODE
    oProgress.CurrentProgress(lnThermNo)
    oProgress.lblFirstLabel.CAPTION = APINVHDR.CVENDCODE
  ENDIF
  *E302628,4 Aged Payable TMI 10/20/2011 [End  ]

  *-- Check if there is an open amount within the selected date then add
  *-- a new record to the temporary file.
  *E302628,4 Aged Payable TMI 10/20/2011 [Start]
  *IF lnAmtToPay <> 0
  IF lnAmtToPay <> 0 AND llCanPrint
    *E302628,4 Aged Payable TMI 10/20/2011 [End  ]
    SELECT APINVHDR
    SCATTER MEMVAR MEMO
    =SEEK(APINVHDR.CVENDCODE,'APVENDOR')

    m.nTotPay    = lnAmtToPay
    m.lCanPrint  = llCanPrint
    m.CVENCOMP   = APVENDOR.CVENCOMP
    m.DINVDUDATd = IIF(m.lCanPrint,DINVDUDAT,'')
    m.CINVNOd    = IIF(m.lCanPrint,CINVNO,'')
    m.cbefore    = IIF(llRpBucket,ldRpC4F,PADL(INT(lnRpPrd4),10,' '))
    m.cprd1ha    = IIF(llRpBucket,ldRpC1F,'')
    m.cprd1hb    = IIF(llRpBucket,ldRpC1T,PADL('0 - '+ALLTRIM(STR(INT(lnRpPrd1),3)),10,' '))
    m.cprd2ha    = IIF(llRpBucket,ldRpC2F,'')
    m.cprd2hb    = IIF(llRpBucket,ldRpC2T,PADL(ALLTRIM(STR(INT(lnRpPrd1),3))+' - '+ALLTRIM(STR(INT(lnRpPrd2),3)),10,' '))
    m.cprd3ha    = IIF(llRpBucket,ldRpC3F,'')
    m.cprd3hb    = IIF(llRpBucket,ldRpC3T,PADL(ALLTRIM(STR(INT(lnRpPrd2),3))+' - '+ALLTRIM(STR(INT(lnRpPrd3),3)),10,' '))
    m.cprd4ha    = IIF(llRpBucket,ldRpC4F,'')
    m.cprd4hb    = IIF(llRpBucket,ldRpC4T,PADL(ALLTRIM(STR(INT(lnRpPrd3),3))+' - '+ALLTRIM(STR(INT(lnRpPrd4),3)),10,' '))
    m.caftera    = IIF(llRpBucket,' After','')
    m.cafterb    = IIF(llRpBucket,ldRpC1T,'    Future')
    m.nInvAmnt   = gfAmntDisp(NINVAMNT, lcRpCurr , ldRpExDate , lcRpTmpNam)
    m.nInvAmntd  = IIF(m.lCanPrint,gfAmntDisp(NINVAMNT, lcRpCurr , ldRpExDate , lcRpTmpNam),0)
    m.nDisOfrd   = gfAmntDisp(NINVDISOF, lcRpCurr , ldRpExDate , lcRpTmpNam)
    m.nDisOfrdd  = IIF(m.lCanPrint, gfAmntDisp(NINVDISOF, lcRpCurr , ldRpExDate , lcRpTmpNam),0)
    m.nbeforDet  = IIF(apinvhdr.DINVDUDAT<ldRpC4F,gfAmntDisp(apinvhdr.ninvamnt-apinvhdr.ninvpaid-apinvhdr.ninvdistk-apinvhdr.ninvadj, lcRpCurr , ldRpExDate , lcRpTmpNam),0.0)
    m.nbeforDetd = IIF(m.lCanPrint,IIF(DINVDUDAT<ldRpC4F,gfAmntDisp(nTotPay, lcRpCurr , ldRpExDate , lcRpTmpNam),0),0)
    m.nprd4det   = IIF(BETWEEN(apinvhdr.DINVDUDAT,ldRpC4F,ldRpC4T),gfAmntDisp(apinvhdr.ninvamnt - apinvhdr.ninvpaid - apinvhdr.ninvdistk - apinvhdr.ninvadj, lcRpCurr , ldRpExDate , lcRpTmpNam),0.0)
    m.nprd4detd  = IIF(m.lCanPrint,IIF(BETWEEN(DINVDUDAT,ldRpC4F,ldRpC4T),gfAmntDisp(nTotPay, lcRpCurr , ldRpExDate , lcRpTmpNam),0.00),0)
    m.nprd3det   = IIF(BETWEEN(apinvhdr.DINVDUDAT,ldRpC3F,ldRpC3T),gfAmntDisp(apinvhdr.ninvamnt - apinvhdr.ninvpaid - apinvhdr.ninvdistk - apinvhdr.ninvadj, lcRpCurr , ldRpExDate , lcRpTmpNam) ,0.0)
    m.nprd3detd  = IIF(m.lCanPrint,IIF(BETWEEN(DINVDUDAT,ldRpC3F,ldRpC3T),gfAmntDisp(nTotPay, lcRpCurr , ldRpExDate , lcRpTmpNam) ,0.00),0)
    m.nPrd2det   = IIF(BETWEEN(apinvhdr.DINVDUDAT,ldRpC2F,ldRpC2T),gfAmntDisp(apinvhdr.ninvamnt - apinvhdr.ninvpaid - apinvhdr.ninvdistk - apinvhdr.ninvadj, lcRpCurr , ldRpExDate , lcRpTmpNam),0.0)
    m.nprd2detd  = IIF(m.lCanPrint,IIF(BETWEEN(DINVDUDAT,ldRpC2F,ldRpC2T),gfAmntDisp(nTotPay, lcRpCurr , ldRpExDate , lcRpTmpNam),0.00),0)
    m.nPrd1det   = IIF(BETWEEN(apinvhdr.DINVDUDAT,ldRpC1F,ldRpC1T),gfAmntDisp(apinvhdr.ninvamnt - apinvhdr.ninvpaid - apinvhdr.ninvdistk - apinvhdr.ninvadj, lcRpCurr , ldRpExDate , lcRpTmpNam),0.0)
    m.nprd1detd  = IIF(m.lCanPrint,IIF(BETWEEN(apinvhdr.DINVDUDAT,ldRpC1F,ldRpC1T),gfAmntDisp(nTotPay, lcRpCurr , ldRpExDate , lcRpTmpNam),0.00),0)
    m.nafterdet  = IIF(apinvhdr.DINVDUDAT>ldRpC1T,gfAmntDisp(apinvhdr.ninvamnt - apinvhdr.ninvpaid - apinvhdr.ninvdistk -apinvhdr.ninvadj, lcRpCurr , ldRpExDate , lcRpTmpNam),0.0)
    m.nafterdetd = IIF(m.lCanPrint,IIF(DINVDUDAT>ldRpC1T,gfAmntDisp(nTotPay, lcRpCurr , ldRpExDate , lcRpTmpNam)  ,0.0),0)
    m.ntotpybls  = gfAmntDisp(apinvhdr.ninvamnt-apinvhdr.ninvpaid-apinvhdr.ninvdistk-apinvhdr.ninvadj, lcRpCurr , ldRpExDate , lcRpTmpNam)
    m.ntotpyblsd = IIF(m.lCanPrint,gfAmntDisp(nTotPay, lcRpCurr , ldRpExDate , lcRpTmpNam),0.00)

    *E302628,4 Aged Payable TMI 10/20/2011 [Start]
    *IF llCanPrint
    *E302628,4 Aged Payable TMI 10/20/2011 [End  ]

    SELECT (lcPriTmp)
    APPEND BLANK
    GATHER MEMVAR MEMO

    *E302628,4 Aged Payable TMI 10/20/2011 [Start]
    *ENDIF
    *E302628,4 Aged Payable TMI 10/20/2011 [End  ]
  ENDIF
ENDSCAN

*!**************************************************************************
*!
*!      Function: lfRepShow
*!
*!**************************************************************************
*
FUNCTION lfRepShow

laOGFxFlt[1,6]= oAriaApplication.BaseCurrency
laOGObjCnt[11] = gfGetMemVar('LLMULCURR')
=lfOGShowGet("lnRepCurr")


*!**************************************************************************
*!
*!      Function: lfvCurDat
*!
*!**************************************************************************

FUNCTION lfvCurDat

lcRpVar = loOgScroll.ACTIVECONTROL
IF EMPTY(ldRpCurDat)
  =gfModalGen("TRM04066B00000","DIALOG",'report date')
  loOgScroll.ACTIVECONTROL.VALUE = loOgScroll.ACTIVECONTROL.OldValue
  ldRpCurDat = loOgScroll.ACTIVECONTROL.OldValue
ELSE
  IF ldRpCurDat < gdSysDate
    llRpConPay = .F.
    =lfOgShowGet("llRpConPay")
    =lfvConPaymnt()
  ENDIF
  ldRpC1F=ldRpCurDat - lnRpPrd1
  ldRpC1T=ldRpCurDat
  ldRpC2F=ldRpCurDat - lnRpPrd2
  ldRpC2T=ldRpC1F - 1
  ldRpC3F=ldRpCurDat - lnRpPrd3
  ldRpC3T=ldRpC2F - 1
  ldRpC4F=ldRpCurDat - lnRpPrd4
  ldRpC4T=ldRpC3F - 1
ENDIF

*!**************************************************************************
*!
*!      Function: lfvConPaymnt
*!
*!**************************************************************************
*
FUNCTION lfvConPaymnt

IF !llRpConPay
  lcRpForm = IIF(lcRpCType='S',"APAGEDS","APAGEDD")
  laOGHDFlt[2,1] = "APINVHDR.DINVDUDAT"
  laOGHDFlt[2,3] = "D"
  laOGHDFlt[2,5] = "Between"
  laOGHDFlt[2,6] = ""
  laOGHDFlt[2,7] = "V"
ELSE
  lcRpForm = IIF(lcRpCType='S',"APAGECS","APAGECD")
  laOGHDFlt[2,1] = "ABS(apinvhdr.ninvamnt)"
  laOGHDFlt[2,3] = "N"
  laOGHDFlt[2,5] = "Greater Than"
  laOGHDFlt[2,6] = "ABS(apinvhdr.ninvpaid) + ABS(apinvhdr.ninvadj) + ABS(apinvhdr.ninvdistk)"
  laOGHDFlt[2,7] = "E"
ENDIF

*!**************************************************************************
*!
*!      Function: lfvRepForm
*!
*!**************************************************************************
*
FUNCTION lfvRepForm
IF !llRpConPay
  lcRpForm = IIF(lcRpCType='S',"APAGEDS","APAGEDD")
ELSE
  lcRpForm = IIF(lcRpCType='S',"APAGECS","APAGECD")
ENDIF
*!**************************************************************************
*!
*!      Function: lfvPrd
*!
*!**************************************************************************
*
FUNCTION lfvPrd

LOCAL loFld, llError, lnNo
llError = .F.
loFld = loOgScroll.ACTIVECONTROL

lcControl  = loOGScroll.FocusControl
lnNo = IIF('1'$lcControl,1,IIF('2'$lcControl,2,IIF('3'$lcControl,3,4)))

DO CASE
CASE lnRpPrd1 <= 0
  =gfModalGen("TRM04072B00000","DIALOG",'No. of days for the 1st period|zero')
  llError = .T.
CASE lnRpPrd1 >= lnRpPrd2
  =gfModalGen("TRM04072B00000","DIALOG",'No. of days for the 2nd period|no. of days for the 1st period')
  llError = .T.
CASE lnRpPrd2 >= lnRpPrd3
  =gfModalGen("TRM04072B00000","DIALOG",'No. of days for the 3rd period|no. of days for the 2nd period')
  llError = .T.
CASE lnRpPrd3 >= lnRpPrd4
  =gfModalGen("TRM04072B00000","DIALOG",'No. of days for the 4th period|no. of days for the 3rd period')
  llError = .T.
OTHERWISE
  llError = .F.
  =lfvCurDat()
  RETURN .T.
ENDCASE

IF llError
  loFld.VALUE = loFld.OldValue
  loFld.REFRESH
  lcNo = ALLTRIM(STR(lnNo))
  lnRpPrd&lcNo = loFld.OldValue
ENDIF
*!**************************************************************************
*!
*!      Function: lfvCurDisp
*!
*!**************************************************************************
FUNCTION lfvCurDisp
llRpProced = gfRepCur(.T., @lcRpCurr,@ldRpExDate,lcRpTmpNam)

LOCAL lnPos
lnPos = ASCAN(loOgScroll.laOgFxFlt,'APINVHDR.CCURRCODE')
lnPos = ASUBSCRIPT(loOgScroll.laOgFxFlt,lnPos,2)
IF lcRpCurr = 'F' .AND. EMPTY(laOGFxFlt[lnPos,6])
  loOgScroll.laOGFxFlt[lnPos,6]= oAriaApplication.BaseCurrency
ENDIF


*!*************************************************************
*! Name      : lfvCurCode
*! Developer : Tarek Mohamed Ibrahim
*! Date      : 08/15/2011
*! Purpose   : This function called from the currency field to
*!             validate the currency.
*!*************************************************************
FUNCTION lfvCurCode

LOCAL lnPos,loFld,lnSlct
lnSlct = SELECT(0)
lnPos = ASUBSCRIPT(loOgScroll.laOGFxFlt,ASCAN(loOgScroll.laOGFxFlt,'APINVHDR.CCURRCODE'),1)
IF EMPTY(loOgScroll.laOGFxFlt[lnPos,6])
  llAllCurr  = .T.
  RETURN
ENDIF
llAllCurr  = .F.

loFld = loOgScroll.ACTIVECONTROL

IF !SEEK(loFld.VALUE,'SYCCURR') .OR. ATC("?",loFld.VALUE) > 0
  SELECT SYCCURR
  DIMENSION laTemp[1]
  laTemp     = ''
  lcFile_Ttl = "Currency"
  lcBrFields = "CCURRCODE :R :H= 'Currency code'," +;
    "CCURRDESC :R :H= 'Description',  " +;
    "CCURRSMBL :R :H= 'Symbol'"
  =gfBrows('','CCURRCODE','laTemp')
  IF EMPTY(laTemp[1])
    loOgScroll.laOGFxFlt[lnPos,6] = loFld.OldValue
  ELSE
    loOgScroll.laOGFxFlt[lnPos,6] = laTemp[1]
  ENDIF
ENDIF
SELECT (lnSlct)

*!**************************************************************************
*! Name      : lfvApAcCod
*! Developer : Tarek Mohamed Ibrahim
*! Date      : 08/15/2011
*! Purpose   : This function is to validate the accounts from
*!             the chart of account of the active company or the
*!             chart of account of another company.
*!**************************************************************************
*! Called from : The Option Grid
*!**************************************************************************
FUNCTION lfvApAcCod
PRIVATE lcSavAlias,lcFieldCont

LOCAL loFld
loFld = loOgScroll.ACTIVECONTROL
lcSavAlias  = ALIAS()  && Variable to save the selected alias.
lcFieldCont = loOgScroll.ACTIVECONTROL.VALUE   && Assign the content of the field to the variable.

*** Variable hold an empty account to compair with. ***
lcEmptyAcs = REPLICATE('0',lnApsAcLen)

*Prevent executing the browse if the account code is empty.
IF !EMPTY(STRTRAN(lcFieldCont,"-",""))

  IF llApGlLink .AND. lcFieldCont <> lcEmptyAcs

    IF !USED('GLACCHAR')
      =gfOpenTABLE(oAriaApplication.DATADIR+'GLACCHAR',oAriaApplication.DATADIR+'ACCTCODE','SH')
    ENDIF

    SELECT GLACCHAR

    IF !SEEK(lcFieldCont) .OR. ATC('?',lcFieldCont) > 0
      LOCATE
      DIMENSION laTemp[2]
      laTemp = ''
      lcSavBrFld=lcBrfields
      lcSavTitle=lcFile_Ttl

      lcBrfields="CACCTCODE :H= 'Account Code',;
                  CACCNLDES :H= 'Account Description'"

      lcFile_Ttl="Chart of accounts"

      =gfbrows(' ','CACCTCODE,CACCNLDES','laTemp')

      lcFile_Ttl=lcSavTitle
      lcBrfields=lcSavBrFld

      lnPOS = ASCAN(loOgScroll.laOGFxFlt,"APINVHDR.CAPACCT")
      IF lnPos > 0
        lnPOS    = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnPos,1)
      ENDIF
      IF !EMPTY(laTemp[1])
        lcFieldCont = ALLTRIM(laTemp[1])
        loOgScroll.laOGFxFlt[lnPOS,6] = laTemp[1]
      ELSE
        lcFieldCont = REPLICATE('0',lnApsAcLen)
        loOgScroll.laOGFxFlt[lnPOS,6] = loFld.OldValue
        loFld.VALUE = loFld.OldValue
      ENDIF
    ENDIF

    IF !EMPTY(lcSavAlias)
      SELECT(lcSavAlias)
    ENDIF

    *Blank the account code if the value equal 0.
    loOgScroll.ACTIVECONTROL.VALUE = IIF(VAL(lcFieldCont) = 0 , "" , lcFieldCont)

  ENDIF
ENDIF

*Blank the account code if the value equal 0 and there is no GL link.
IF VAL(lcFieldCont) = 0 .AND. llApGlLink
  *lcVarName  = SYS(18)
  *&lcVarName = ""
  loFld.VALUE = ''
  loOgScroll.laOGFxFlt[lnPOS,6] = ''
ENDIF

IF !EMPTY(lcSavAlias)
  SELECT(lcSavAlias)
ENDIF
*-- End of lfvApAcCod.


*!**************************************************************************
*! Name      : lfViodPayt
*! Developer : Tarek Mohamed Ibrahim
*! Date      : 08/15/2011
*! Purpose   : To calculate the voided payments that are less or equal to the aging date.
*!**************************************************************************
*! Example   :  =lfColData()
*!**************************************************************************
FUNCTION lfViodPayt
PARAMETERS lcVend,lcInv

lnTotPaymnt = 0
llCanPrint  = .F.
lnRPAlias = SELECT()

SELECT APDIST
=SEEK(lcInv+lcVend)

IF APPAYMNT.lPayAdvAn
  SUM REST nAPdAmnt WHILE cInvNo+cVendCode+cApdTrTyp = lcInv+lcVend;
    FOR ( capdtrtyp $ "MNHP" AND cApdActID = "A" .AND. nAPdAmnt < 0 ) ;
    AND dAPDtrdat <= ldRpCurDat ;
    AND cApdStat  = 'V' TO lnTotPaymnt
ELSE
  SUM REST nAPdAmnt WHILE cInvNo+cVendCode+cApdTrTyp = lcInv+lcVend;
    FOR ( capdtrtyp $ "MNHP" AND cApdActID = "A" ) ;
    AND dAPDtrdat <= ldRpCurDat ;
    AND cApdStat  = 'V' TO lnTotPaymnt

ENDIF

SELECT (lnRpAlias)
GO RECNO()
RETURN lnTotPaymnt

*!**************************************************************************
*! Name      : lfGetLstRat
*! Developer : Tarek Mohamed Ibrahim
*! Date      : 08/15/2011
*! Purpose   : Return the Last exchange rate before the given date if there is no
*!             Rate in the given date
*!********************************************************************************
*! Example   :  =lfGetLstRat()
*!**************************************************************************

FUNCTION lfGetLstRat
PARAMETER lcToCurr, lcFromCurr, ldExRate
lcOldAlias = ALIAS()
IF !USED('Sycexch')
  =gfOpenTable(oAriaApplication.DATADIR+'Sycexch',oAriaApplication.DATADIR+'','SH')
ENDIF

SELECT Sycexch.nexrate, MAX(Sycexch.dratedate);
  FROM Sycexch;
  WHERE Sycexch.dratedate <= ldExRate ;
  AND Sycexch.cbasecurr = lcFromCurr;
  AND Sycexch.ccurrcode = lcToCurr  ;
  INTO ARRAY laExRate

SELECT (lcOldAlias)
RETURN IIF(_TALLY >=1 ,laExRate[1,1],1)

*!**************************************************************************
*! Name      :lfSumALL
*! Developer : Tarek Mohamed Ibrahim
*! Date      : 08/15/2011
*! Purpose   : Get amount in correct way for All payment cases
*!********************************************************************************
FUNCTION lfSumALL
PARAMETERS lcVend,lcInv

*E302628,4 Aged Payable TMI 10/20/2011 [Start] move this part to the main block to enhance
*!*	IF apinvhdr.ninvpaid + apinvhdr.ninvdistk + apinvhdr.ninvadj  = 0
*!*	  llCanPrint  = .T.
*!*	  lnTotPaymnt = 0
*!*	  RETURN 0
*!*	ENDIF
*E302628,4 Aged Payable TMI 10/20/2011 [End  ]

lnTotPaymnt = 0
llCanPrint  = .F.

llGetEq = .F.

*E302628,4 Aged Payable TMI 10/23/2011 [Start] no need for this line
*!*	lnRPAlias=SELECT()
*E302628,4 Aged Payable TMI 10/23/2011 [End  ]

SELECT APDIST

IF llUseMulCr
  SEEK lcInv+lcVend
  SCAN REST WHILE CINVNO+CVENDCODE = lcInv+lcVend;
      FOR ((capdtrtyp $ "MNHP" AND capdactID $ "CSJ") .OR. ( capdtrtyp = "A" AND nApdAmnt < 0));
      AND dAPDtrdat > ldRpCurDat;
      AND cApdStat  <> 'V'
    DO CASE
    CASE APDIST.CAPDACTID = 'C'
      lcExSin2 = ' '

      IF APINVHDR.CCURRCODE = APPAYMNT.CCURRCODE
        IF APINVHDR.CCURRCODE = oAriaApplication.BaseCurrency
          lnTotPaymnt = lnTotPaymnt-ROUND(APDIST.NAPDAMNT,2)
        ELSE
          lcExSin1   = gfGetExSin(@lcExSin2,APDIST.CCURRCODE,oAriaApplication.BaseCurrency)
          lnExRate   = APDIST.NEXRATE
          lnTotPaymnt = lnTotPaymnt - ROUND(APDIST.NAPDAMNT &lcExSin1 lnExRate ,2)
          llGetEq = .T.
        ENDIF
      ELSE

        llGetEq = .T.
        IF (APINVHDR.CCURRCODE <> oAriaApplication.BaseCurrency) AND (APPAYMNT.CCURRCODE <> oAriaApplication.BaseCurrency)
          lcExSin1   = gfGetExSin(@lcExSin2,APDIST.CCURRCODE,oAriaApplication.BaseCurrency)
          lnExRate   = APDIST.NEXRATE
          lnTotPaymnt = lnTotPaymnt - ROUND(APDIST.NAPDAMNT &lcExSin1 lnExRate ,2)
        ELSE
          lcExSin1   = gfGetExSin(@lcExSin2,APINVHDR.CCURRCODE,APPAYMNT.CCURRCODE)

          lnUnit     = APINVHDR.NCURRUNIT
          lcInvCurr  = APINVHDR.CCURRCODE
          lcPayCurr  = APPAYMNT.CCURRCODE
          ldExRateDt = APPAYMNT.DPAYDATE

          lnExRate   = APDIST.NEXRATE
          lnExRate = IIF(lnExRate = 0 ,lfGetLstRat(lcInvCurr,lcPayCurr,ldExRateDt),lnExRate)

          lnTotPaymnt = lnTotPaymnt-ROUND(APDIST.NEQVAMNT,2)
        ENDIF

      ENDIF
    CASE APDIST.CAPDACTID $ 'S'

      IF APINVHDR.CCURRCODE = oAriaApplication.BaseCurrency
        lnTotPaymnt = lnTotPaymnt - APDIST.nApdAmnt
      ELSE
        lcExSin1   = gfGetExSin(@lcExSin2,APDIST.CCURRCODE,oAriaApplication.BaseCurrency)
        lnExRate   = APDIST.NEXRATE
        lnTotPaymnt = lnTotPaymnt - ROUND(APDIST.NAPDAMNT &lcExSin1 lnExRate ,2)
        llGetEq = .T.
      ENDIF

    CASE APDIST.CAPDACTID $ 'J' AND APDIST.cApdTrTyp <> "A"

      lcExSin1   = gfGetExSin(@lcExSin2,APINVHDR.CCURRCODE,APPAYMNT.CCURRCODE)
      lcExSin1   = IIF(lcExSin1 = '*' , '/' , '*')
      lcExSin2   = IIF(lcExSin2 = '*' , '/' , '*')
      lnUnit     = APINVHDR.NCURRUNIT
      lcInvCurr  = APINVHDR.CCURRCODE
      lcPayCurr  = APPAYMNT.CCURRCODE
      ldExRateDt = APPAYMNT.DPAYDATE

      lnExRate   = APDIST.NEXRATE

      lcExSin1   = gfGetExSin(@lcExSin2,APDIST.CCURRCODE,oAriaApplication.BaseCurrency)
      lnExRate   = APDIST.NEXRATE
      lnTotPaymnt = lnTotPaymnt - ROUND(APDIST.NAPDAMNT &lcExSin1 lnExRate ,2)

    CASE APDIST.cApdTrTyp = "A" AND APDIST.CAPDACTID $ 'A'

      lnTotPaymnt = lnTotPaymnt+APDIST.NAPDAMNT
    ENDCASE
  ENDSCAN

  IF llGetEq
    lcExSin2 = ' '
    lcExSin1   = gfGetExSin(@lcExSin2,APINVHDR.CCURRCODE)
    lcExSin1   = IIF(lcExSin1 = '*' , '/' , '*')
    lcExSin2   = IIF(lcExSin2 = '*' , '/' , '*')
    lnExRate   = APINVHDR.NEXRATE
    lnTotPaymnt= ROUND(lnTotPaymnt &lcExSin1 lnExRate ,2)
  ENDIF

ELSE
  *B610013,2 MMT 07/19/2012 AP aged report displays incorrect data[T20120711.0043][Start]
  SEEK lcInv+lcVend
  *B610013,2 MMT 07/19/2012 AP aged report displays incorrect data[T20120711.0043][End]
  *E302628,4 Aged Payable TMI 10/20/2011 [Start] enhance the code by rearranging the places of evaluated expressions
  *!*	  SUM REST nAPdAmnt WHILE CINVNO+CVENDCODE = lcInv+lcVend;
  *!*	      FOR ((capdtrtyp $ "MNHP" AND capdactID = "A") .OR. ( capdtrtyp = "A" AND nApdAmnt < 0));
  *!*	      AND dAPDtrdat > ldRpCurDat AND cApdStat  <> 'V' TO lnTotPaymnt
  SUM REST nAPdAmnt WHILE CINVNO+CVENDCODE+CAPDTRTYP = lcInv+lcVend ;
    FOR dAPDtrdat > ldRpCurDat AND cApdStat  <> 'V' AND ;
    ((capdtrtyp $ "MNHP" AND capdactID = "A") .OR. ( capdtrtyp = "A" AND nApdAmnt < 0));
    TO lnTotPaymnt
  *E302628,4 Aged Payable TMI 10/20/2011 [End  ]


ENDIF

*E302628,4 Aged Payable TMI 10/20/2011 [Start]  enhance the code
*IF apinvhdr.nInvAmnt > 0 AND lnTotPaymnt <> apinvhdr.ninvpaid + apinvhdr.ninvdistk + apinvhdr.ninvadj
IF apinvhdr.nInvAmnt > 0 AND lnTotPaymnt <> apinvhdr.ninvpaid + apinvhdr.ninvdistk + apinvhdr.ninvadj ;
    AND SEEK("A                    "+lcInv,'APDIST','PAYMNTS')

  *E302628,4 Aged Payable TMI 10/20/2011 [End  ]
  SELECT APDIST
  lcOldTag = ORDER()
  *E302628,4 Aged Payable TMI 10/20/2011 [Start]  enhance the code
  *gfSetOrder('PAYMNTS')
  SET ORDER TO 'PAYMNTS'
  *E302628,4 Aged Payable TMI 10/20/2011 [End  ]

  *E302628,4 Aged Payable TMI 10/20/2011 [Start]  enhance the code
  *SEEK "A"+REPL(" ",FSIZE("CBNKCODE"))+REPL(" ",FSIZE("CCHKACCT"))+lcInv
  *SUM REST nAPdAmnt WHILE CAPDTRTYP+CBNKCODE+CCHKACCT+CAPDREF="A"+REPL(" ",FSIZE("CBNKCODE"))+;
  REPL(" ",FSIZE("CCHKACCT"))+lcInv ;
  FOR cVendCode = lcVend ;
  AND dAPDtrdat > ldRpCurDat ;
  AND cApdStat  <> 'V';
  AND (cApdActId = 'A' AND nApdAmnt > 0);
  TO lnTotDebit
  *SEEK "A                    "+lcInv
  lnTotDebit = 0
  SUM REST nAPdAmnt WHILE CAPDTRTYP+CBNKCODE+CCHKACCT+CAPDREF="A                    "+lcInv ;
    FOR cVendCode = lcVend ;
    AND cApdActId = 'A' AND nApdAmnt > 0;
    AND dAPDtrdat > ldRpCurDat ;
    AND cApdStat  <> 'V';
    TO lnTotDebit
  *E302628,4 Aged Payable TMI 10/20/2011 [End  ]
  lnTotPaymnt = lnTotPaymnt + lnTotDebit
  *E302628,4 Aged Payable TMI 10/20/2011 [Start]
  *gfSetOrder(lcOldTag)
  *!*	  SET ORDER TO (lcOldTag)
  *E302628,4 Aged Payable TMI 10/20/2011 [End  ]
  *E302628,4 Aged Payable TMI 10/06/2011 [Start]
  IF !llRpConPay
    SET ORDER TO TAG INVVEND IN APDIST DESC
  ELSE
    SET ORDER TO (lcOldTag)
  ENDIF
  *E302628,4 Aged Payable TMI 10/06/2011 [End  ]
ENDIF

*E302628,4 Aged Payable TMI 10/20/2011 [Start]
*!*	SELECT (lnRpAlias)
*!*	GO RECNO()
*E302628,4 Aged Payable TMI 10/20/2011 [End  ]
llCanPrint  = (apinvhdr.ninvamnt - apinvhdr.ninvpaid - apinvhdr.ninvdistk - apinvhdr.ninvadj+lnTotPaymnt <> 0)
RETURN IIF(llCanPrint,lnTotPaymnt,0)

*!*************************************************************
*! Name      : lfCrtTemp
*! Developer : Hesham Elmasry(HES)
*! Date      : 07/20/2009
*! Purpose   : Create the Temp used as a base file for the
*!             report and the export
*!*************************************************************
FUNCTION lfCrtTemp

*B610129,1 HIA 10/23/2012 Modify fields names, when export to excel [T20121019.0002][Begin]
*!*	IF USED(lcPriTmp) AND RECCOUNT(lcPriTmp) > 0
*!*	  SELECT (lcPriTmp)
*!*	  ZAP
*!*	ENDIF
*!*	*-- Create File
*!*	IF !USED(lcPriTmp)

IF USED(lcPriTmp) 
  SELECT (lcPriTmp)
  USE 
ENDIF
*B610129,1 HIA 10/23/2012 Modify fields names, when export to excel [T20121019.0002][End]

  *E303613,1 MMT 10/25/2015 Add Invoice date to Aged Payble report export to excel[T20151008.0005][Start]
  *DIMENSION laTempStru1[38,4]
  DIMENSION laTempStru1[39,4]  
  *E303613,1 MMT 10/25/2015 Add Invoice date to Aged Payble report export to excel[T20151008.0005][End]
  lnI = 0
  
  lnI = lnI + 1
  laTempStru1[lnI,1] = 'CVENDCODE'
  laTempStru1[lnI,2] = 'C'
  laTempStru1[lnI,3] = 8
  laTempStru1[lnI,4] = 0
  
  lnI = lnI + 1
  laTempStru1[lnI,1] = 'CVENCOMP'
  laTempStru1[lnI,2] = 'C'
  laTempStru1[lnI,3] = 30
  laTempStru1[lnI,4] = 0
  
   lnI = lnI + 1
  laTempStru1[lnI,1] = 'CCURRCODE'
  laTempStru1[lnI,2] = 'C'
  laTempStru1[lnI,3] = 3
  laTempStru1[lnI,4] = 0
  
  lnI = lnI + 1
  laTempStru1[lnI,1] = 'CINVNO'
  laTempStru1[lnI,2] = 'C'
  laTempStru1[lnI,3] = 12
  laTempStru1[lnI,4] = 0
  
  *E303613,1 MMT 10/25/2015 Add Invoice date to Aged Payble report export to excel[T20151008.0005][Start]
  lnI = lnI + 1
  laTempStru1[lnI,1] = 'DINVDATE'
  laTempStru1[lnI,2] = 'D'
  laTempStru1[lnI,3] = 8
  laTempStru1[lnI,4] = 0
  *E303613,1 MMT 10/25/2015 Add Invoice date to Aged Payble report export to excel[T20151008.0005][End]
 
  lnI = lnI + 1
  laTempStru1[lnI,1] = 'DINVDUDAT'
  laTempStru1[lnI,2] = 'D'
  laTempStru1[lnI,3] = 8
  laTempStru1[lnI,4] = 0
  
  lnI = lnI + 1
  laTempStru1[lnI,1] = 'nInvAmnt'
  laTempStru1[lnI,2] = 'N'
  laTempStru1[lnI,3] = 15
  laTempStru1[lnI,4] = 2
  

  
  lnI = lnI + 1
  laTempStru1[lnI,1] = 'nDisOfrdd'
  laTempStru1[lnI,2] = 'N'
  laTempStru1[lnI,3] = 15
  laTempStru1[lnI,4] = 2
  

  
  lnI = lnI + 1
  laTempStru1[lnI,1] = 'nbefordetd'
  laTempStru1[lnI,2] = 'N'
  laTempStru1[lnI,3] = 15
  laTempStru1[lnI,4] = 2
  
  lnI = lnI + 1
  laTempStru1[lnI,1] = 'nprd4detd'
  laTempStru1[lnI,2] = 'N'
  laTempStru1[lnI,3] = 15
  laTempStru1[lnI,4] = 2  

  lnI = lnI + 1
  laTempStru1[lnI,1] = 'nprd3detd'
  laTempStru1[lnI,2] = 'N'
  laTempStru1[lnI,3] = 15
  laTempStru1[lnI,4] = 2  
  
    lnI = lnI + 1
  laTempStru1[lnI,1] = 'nprd2detd'
  laTempStru1[lnI,2] = 'N'
  laTempStru1[lnI,3] = 15
  laTempStru1[lnI,4] = 2
  
  lnI = lnI + 1
  laTempStru1[lnI,1] = 'nprd1detd'
  laTempStru1[lnI,2] = 'N'
  laTempStru1[lnI,3] = 15
  laTempStru1[lnI,4] = 2
  
  lnI = lnI + 1
  laTempStru1[lnI,1] = 'nafterdetd'
  laTempStru1[lnI,2] = 'N'
  laTempStru1[lnI,3] = 15
  laTempStru1[lnI,4] = 2  


  lnI = lnI + 1
  laTempStru1[lnI,1] = 'ntotpyblsd'
  laTempStru1[lnI,2] = 'N'
  laTempStru1[lnI,3] = 15
  laTempStru1[lnI,4] = 2    
  
  
  lnI = lnI + 1
  laTempStru1[lnI,1] = 'cbefore'
  laTempStru1[lnI,2] = 'C'
  laTempStru1[lnI,3] = 10
  laTempStru1[lnI,4] = 0
  
  lnI = lnI + 1
  laTempStru1[lnI,1] = 'nbefordet'
  laTempStru1[lnI,2] = 'N'
  laTempStru1[lnI,3] = 15
  laTempStru1[lnI,4] = 2
  
  lnI = lnI + 1
  laTempStru1[lnI,1] = 'DINVDUDATD'
  laTempStru1[lnI,2] = 'D'
  laTempStru1[lnI,3] = 8
  laTempStru1[lnI,4] = 0  

  
  lnI = lnI + 1
  laTempStru1[lnI,1] = 'cprd1ha'
  laTempStru1[lnI,2] = 'C'
  laTempStru1[lnI,3] = 10
  laTempStru1[lnI,4] = 0
  
  lnI = lnI + 1
  laTempStru1[lnI,1] = 'cprd1hb'
  laTempStru1[lnI,2] = 'C'
  laTempStru1[lnI,3] = 10
  laTempStru1[lnI,4] = 0
  
  lnI = lnI + 1
  laTempStru1[lnI,1] = 'nprd1det'
  laTempStru1[lnI,2] = 'N'
  laTempStru1[lnI,3] = 15
  laTempStru1[lnI,4] = 2
  
     lnI = lnI + 1
  laTempStru1[lnI,1] = 'CINVNOD'
  laTempStru1[lnI,2] = 'C'
  laTempStru1[lnI,3] = 12
  laTempStru1[lnI,4] = 0
  
  
  lnI = lnI + 1
  laTempStru1[lnI,1] = 'nInvAmntd'
  laTempStru1[lnI,2] = 'N'
  laTempStru1[lnI,3] = 15
  laTempStru1[lnI,4] = 2
  
  
  
  lnI = lnI + 1
  laTempStru1[lnI,1] = 'cprd2ha'
  laTempStru1[lnI,2] = 'C'
  laTempStru1[lnI,3] = 10
  laTempStru1[lnI,4] = 0
  
  lnI = lnI + 1
  laTempStru1[lnI,1] = 'cprd2hb'
  laTempStru1[lnI,2] = 'C'
  laTempStru1[lnI,3] = 10
  laTempStru1[lnI,4] = 0
  
  lnI = lnI + 1
  laTempStru1[lnI,1] = 'nprd2det'
  laTempStru1[lnI,2] = 'N'
  laTempStru1[lnI,3] = 15
  laTempStru1[lnI,4] = 2
  

  
  lnI = lnI + 1
  laTempStru1[lnI,1] = 'cprd3ha'
  laTempStru1[lnI,2] = 'C'
  laTempStru1[lnI,3] = 10
  laTempStru1[lnI,4] = 0
  
  lnI = lnI + 1
  laTempStru1[lnI,1] = 'cprd3hb'
  laTempStru1[lnI,2] = 'C'
  laTempStru1[lnI,3] = 10
  laTempStru1[lnI,4] = 0
  
  lnI = lnI + 1
  laTempStru1[lnI,1] = 'nprd3det'
  laTempStru1[lnI,2] = 'N'
  laTempStru1[lnI,3] = 15
  laTempStru1[lnI,4] = 2
  

  
  lnI = lnI + 1
  laTempStru1[lnI,1] = 'cprd4ha'
  laTempStru1[lnI,2] = 'C'
  laTempStru1[lnI,3] = 10
  laTempStru1[lnI,4] = 0
  
  lnI = lnI + 1
  laTempStru1[lnI,1] = 'cprd4hb'
  laTempStru1[lnI,2] = 'C'
  laTempStru1[lnI,3] = 10
  laTempStru1[lnI,4] = 0
  
  lnI = lnI + 1
  laTempStru1[lnI,1] = 'nprd4det'
  laTempStru1[lnI,2] = 'N'
  laTempStru1[lnI,3] = 15
  laTempStru1[lnI,4] = 2
  
  
  lnI = lnI + 1
  laTempStru1[lnI,1] = 'caftera'
  laTempStru1[lnI,2] = 'C'
  laTempStru1[lnI,3] = 6
  laTempStru1[lnI,4] = 0
  
  lnI = lnI + 1
  laTempStru1[lnI,1] = 'cafterb'
  laTempStru1[lnI,2] = 'C'
  laTempStru1[lnI,3] = 10
  laTempStru1[lnI,4] = 0
  
  lnI = lnI + 1
  laTempStru1[lnI,1] = 'nafterdet'
  laTempStru1[lnI,2] = 'N'
  laTempStru1[lnI,3] = 15
  laTempStru1[lnI,4] = 2
  
  

  
  lnI = lnI + 1
  laTempStru1[lnI,1] = 'nDisOfrd'
  laTempStru1[lnI,2] = 'N'
  laTempStru1[lnI,3] = 15
  laTempStru1[lnI,4] = 2
  

  
  lnI = lnI + 1
  laTempStru1[lnI,1] = 'ntotpybls'
  laTempStru1[lnI,2] = 'N'
  laTempStru1[lnI,3] = 15
  laTempStru1[lnI,4] = 2
  
  
  lnI = lnI + 1
  laTempStru1[lnI,1] = 'nTotPay'
  laTempStru1[lnI,2] = 'N'
  laTempStru1[lnI,3] = 14
  laTempStru1[lnI,4] = 2
  
  lnI = lnI + 1
  laTempStru1[lnI,1] = 'lCanPrint'
  laTempStru1[lnI,2] = 'L'
  laTempStru1[lnI,3] = 1
  laTempStru1[lnI,4] = 0
  

DECLARE LAIndeces[2,2]
LAIndeces[1,1] = 'cvendcode+ccurrcode+cinvno'
LAIndeces[1,2] = 'cVenCurTag'
LAIndeces[2,1] = 'cCurrCode+cVendCode+cInvNo'
LAIndeces[2,2] = 'CURVENINV'

gfCrtTmp(lcPriTmp,@laTempStru1,@LAIndeces,lcPriTmp,.T.)

*B610129,1 HIA 10/23/2012 Modify fields names, when export to excel [T20121019.0002][Begin]
*ENDIF
*B610129,1 HIA 10/23/2012 Modify fields names, when export to excel [T20121019.0002][End]
* End of lfCrtTemp

*!*************************************************************
*! Name      : lfCheckFilter
*! Developer : Hesham Elmasry(HES)
*! Date      : 07/21/2009
*! Purpose   : Get filter expression values
*!*************************************************************
FUNCTION lfCheckFilter
LPARAMETERS lnArrayType, lcFilter

LOCAL lcReturn, lnPOS
DO CASE
CASE lnArrayType = 1
  lnPOS = ASCAN(loOgScroll.laOGFxFlt,lcFilter)
  IF lnPos > 0
    lnPOS    = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnPos,1)
    lcReturn = loOgScroll.laOGFxFlt[lnPOS,6]
  ELSE
    lcReturn = ""
  ENDIF
CASE lnArrayType = 2
  lnPOS = ASCAN(loOgScroll.laOGHDFlt,lcFilter)
  IF lnPos > 0
    lnPOS    = ASUBSCRIPT(loOgScroll.laOGHDFlt,lnPos,1)
    lcReturn = loOgScroll.laOGHDFlt[lnPOS,6]
  ELSE
    lcReturn = ""
  ENDIF
CASE lnArrayType = 3
  lnPOS = ASCAN(loOgScroll.laOGvrFlt,lcFilter)
  IF lnPos > 0
    lnPOS    = ASUBSCRIPT(loOgScroll.laOGvrFlt,lnPos,1)
    lcReturn = loOgScroll.laOGvrFlt[lnPOS,6]
  ELSE
    lcReturn = ""
  ENDIF
OTHERWISE
  lcReturn = ""
ENDCASE

RETURN lcReturn
* End of lfCheckFilter

*!**************************************************************************
*!
*!      Function: lfGlCkActD
*!
*!**************************************************************************
FUNCTION lfGlCkActD
RETURN lcRpActPic

*!**************************************************************************
*!
*!      Function: lfBnkCode
*!
*!**************************************************************************
FUNCTION lfBnkCode
PRIVATE lcRetVal

lcRetVal = ' '

lcOldAlias = ALIAS()    && Save the current alias
SELECT APSETUP
lcRetVal   = APSETUP.CBNKCODE

llRpGlLink = IIF(APSETUP.CAPSGLLINK='Y',.T.,.F.)
lcRpActPic = IIF(llRpGlLink,STRTRAN(ALLTRIM(STRTRAN(lcApsAcMas,'#','X',1)),'X','9',2),;
  ALLTRIM(STRTRAN(lcApsAcMas,'#','9',1)))
IF llRpGlLink
  SELECT SYCCOMP
  =SEEK(oAriaApplication.ActiveCompanyID)
  lcParent   = SYCCOMP.CCOMPPRNT
  IF EMPTY(lcParent)
    lcRpParDir = oAriaApplication.DataDir
  ELSE
    =SEEK(lcParent)
    lcRpParDir = SYCCOMP.CCOM_DDIR
    =SEEK(oAriaApplication.ActiveCompanyID)
  ENDIF
ENDIF

SELECT (lcOldAlias)
IF EMPTY(&lcOGVarName)
  loOgScroll.&lcOGVarName=lcRetVal
ENDIF

RETURN REPLI('X',8)
*!**************************************************************************
*!
*!      Function: lfChkAct
*!
*!**************************************************************************
FUNCTION lfChkAct
PARAMETERS llFirsTime

PRIVATE lcRetVal

lcRetVal = ' '

lcOldAlias = ALIAS()    && Save the current alias

IF llFirsTime
  SELECT APSETUP
  lcRetVal = APSETUP.CCHKACCT
ELSE
  SELECT APCHECKS
  *E302628,4 Aged Payable TMI 10/20/2011 [Start]
  *gfSetOrder('BANKCHECK')
  SET ORDER TO 'BANKCHECK'
  *E302628,4 Aged Payable TMI 10/20/2011 [End  ]
  =SEEK(laOGFxFlt[2,6])
  lcRetVal = APCHECKS.CCHKACCT
ENDIF

SELECT (lcOldAlias)
IF EMPTY(&lcOGVarName) AND llFirsTime
  &lcOGVarName=lcRetVal
ENDIF
RETURN IIF(llFirsTime,REPL('X',12),lcRetVAl)

*!**************************************************************************
*!
*!      Function: lfvBank
*!
*!**************************************************************************
FUNCTION lfvBank

LOCAL loFld
loFld = loOgScroll.ACTIVECONTROL

IF loFld.OldValue = loFld.VALUE
  RETURN
ENDIF

DECLARE laRpRetFld(1)
lcBrFields    = 'CBnkCode:H="Code",CBNKLNDES:H="Description"'
laRpRetFld[1] = ''

lcOldAlias = ALIAS()    && Save the current alias
llUesdBefo = .F.        && Check if used before or this the first time

SELECT APBANKS
*E302628,4 Aged Payable TMI 10/20/2011 [Start]
*gfSetOrder('BANKCODE')
SET ORDER TO 'BANKCODE'
*E302628,4 Aged Payable TMI 10/20/2011 [End  ]

IF loFld.OldValue <> loFld.VALUE
  *** Search for the current Group code
  IF '?' $ loFld.VALUE .OR. (!EMPTY(loFld.VALUE) AND !SEEK(loFld.VALUE))
    =gfBrows([],'CBnkCode',"laRpRetFld",'Bank Codes ',.F.)
    lnPOS = ASCAN(loOgScroll.laOGFxFlt,"APINVHDR.CBNKCODE")
    IF lnPos > 0
      lnPOS = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnPos,1)
    ENDIF
    IF !EMPTY(laRpRetFld[1])
      loFld.VALUE = laRpRetFld[1]
      loOgScroll.laOGFxFlt[lnPOS,6] = laRpRetFld[1]
    ELSE
      loFld.VALUE = loFld.OldValue
      loOgScroll.laOgFxFlt[lnPOS,6] = loFld.OldValue
    ENDIF
    loFld.REFRESH
  ENDIF

  lnPOS = ASCAN(loOgScroll.laOGFxFlt,"APINVHDR.CCHKACCT")
  IF lnPos > 0
    lnPOS    = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnPos,1)
    loOgScroll.laOGFxFlt[lnPOS,6] = lfchkact()
  ENDIF
ENDIF

IF NOT EMPTY(lcOldAlias)
  SELECT (lcOldAlias)
ENDIF
*!**************************************************************************
*!
*!      Function: lfvChkAct
*!
*!**************************************************************************
FUNCTION lfvChkAct

IF loOgScroll.ACTIVECONTROL.OldValue = loOgScroll.ACTIVECONTROL.VALUE
  RETURN
ENDIF
DECLARE laRpRetFld(1)
lcBrFields    = 'CBnkCode:H="Bank Code",CChkAcct:H="Checking account"'
laRpRetFld[1] = ''

lcOldAlias = ALIAS()    && Save the current alias
llUesdBefo = .F.        && Check if used before or this the first time

SELECT APCHECKS
*E302628,4 Aged Payable TMI 10/20/2011 [Start]
*gfSetOrder('BANKCHECK')
SET ORDER TO 'BANKCHECK'
*E302628,4 Aged Payable TMI 10/20/2011 [End  ]

LOCAL lcCurVal
lcCurVal = loOgScroll.ACTIVECONTROL.VALUE
&& Check If year field is empty
IF loOgScroll.ACTIVECONTROL.OldValue <> loOgScroll.ACTIVECONTROL.VALUE
  *** Search for the current Group code
  LOCAL lnPos
  lnPOS = ASCAN(loOgScroll.laOGFxFlt,"APINVHDR.CCHKACCT")
  IF lnPos > 0
    lnPOS = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnPos,1)
  ENDIF
  IF '?' $ loOgScroll.ACTIVECONTROL.VALUE .OR. (!EMPTY(lcCurVal) AND !SEEK(laOGFxFlt[2,6]+lcCurVal))
    lcKey = IIF(EMPTY(laOGFxFlt[2,6]),'','"'+laOGFxFlt[2,6]+'"')
    =gfBrows(lcKey,'CBnkCode,CChkAcct',"laRpRetFld",'Bank & Check Accounts ',.F.)
    IF !EMPTY(laRpRetFld[1]) AND loOgScroll.ACTIVECONTROL.VALUE <> laRpRetFld[2]
      loOgScroll.laOGFxFlt[2,6] = laRpRetFld[1]
      loOgScroll.ACTIVECONTROL.VALUE = laRpRetFld[2]
      loOgScroll.laOGFxFlt[lnPOS,6] = laRpRetFld[2]
    ELSE
      loOgScroll.ACTIVECONTROL.VALUE = loOgScroll.ACTIVECONTROL.OldValue
      loOgScroll.laOGFxFlt[lnPOS,6] = loOgScroll.ACTIVECONTROL.OldValue
    ENDIF
    loOgScroll.ACTIVECONTROL.REFRESH
  ENDIF
ENDIF
IF NOT EMPTY(lcOldAlias)
  SELECT (lcOldAlias)
ENDIF

*!**************************************************************************
*!
*!      Function: lfGlLink
*!
*!**************************************************************************
FUNCTION lfGlLink

IF !USED('APSETUP')
  =gfOpenTABLE(oAriaApplication.DATADIR+'APSETUP',oAriaApplication.DATADIR+'APSETUP','SH')
  SELECT APSETUP
  SEEK('')
ENDIF

IF !USED('ACCOD')
  =gfOpenTABLE(oAriaApplication.DATADIR+'ACCOD',oAriaApplication.DATADIR+'ACCSEGNO','SH')
  SELECT ACCOD
  SEEK('')
ENDIF

IF !USED('SYCCOMP')
  =gfOpenTABLE(oAriaApplication.SysPath+'SYCCOMP',oAriaApplication.DATADIR+'CCOMP_ID','SH')
  SELECT SYCCOMP
  SEEK('')
ENDIF

SELECT ACCOD
lcApsAcMas = ACCOD.cAcsMask
lcApsAcMas = STRTRAN(lcApsAcMas,'#',IIF(APSETUP.cApsgllink='Y','9','X'))
lcApsAcMas = ALLTRIM("X"+SUBSTR(lcApsAcMas,2))

lcApTmpAlias=ALIAS()
SELECT APSETUP
GO TOP
llRpGlLink = capsgllink = 'Y'
lcRpActPic = IIF(llRpGlLink,STRTRAN(ALLTRIM(STRTRAN(lcApsAcMas,'#','X',1)),'X','9',2),;
  ALLTRIM(STRTRAN(lcApsAcMas,'#','9',1)))
IF llRpGlLink
  SELECT SYCCOMP
  =SEEK(oAriaApplication.ActiveCompanyID)
  lcParent   = SYCCOMP.CCOMPPRNT
  IF EMPTY(lcParent)
    lcRpParDir = oAriaApplication.DataDir
  ELSE
    =SEEK(lcParent)
    lcRpParDir = SYCCOMP.CCOM_DDIR
    =SEEK(oAriaApplication.ActiveCompanyID)
  ENDIF
ENDIF

SELECT (lcApTmpAlias)

*!**************************************************************************
*!
*!      Function: lfvGlAct
*!
*!**************************************************************************
FUNCTION lfvGlAct

LOCAL loFld
loFld = loOgScroll.ACTIVECONTROL
IF loFld.OldValue = loFld.VALUE
  RETURN
ENDIF

DECLARE laRpRetFld(1)
lcBrFields    = 'CACCTCODE:H="Code",CACCNLDES:H="Description"'
laRpRetFld[1] = ''
IF llRpGlLink
  lcOldAlias = ALIAS()    && Save the current alias

  SELECT GLACCHAR

  && Check If year field is empty
  IF .NOT. EMPTY(loFld.VALUE)
    *** Search for the current Group code
    lnPOS = ASCAN(loOgScroll.laOGFxFlt,"APINVHDR.CCHKGLACC")
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnPos,1)
    ENDIF

    IF ('?' $ loFld.VALUE .OR.!SEEK(loFld.VALUE))
      =gfBrows([],'CACCTCODE',"laRpRetFld",'Accoonts ',.F.)
      IF !EMPTY(laRpRetFld[1])
        loFld.VALUE = laRpRetFld[1]
        loOgScroll.laOGFxFlt[lnPOS,6] = laRpRetFld[1]
      ELSE
        loFld.VALUE = loFld.oldvalue
        loOgScroll.laOGFxFlt[lnPOS,6] = loFld.oldvalue
      ENDIF
      loFld.REFRESH
    ENDIF
  ENDIF

  IF NOT EMPTY(lcOldAlias)
    SELECT (lcOldAlias)
  ENDIF
ENDIF


***************************************************************************************
* Name        : lfvPostingDate
* Developer   : Tarek Mohammed Ibrahim - TMI
* Date        : 09/29/2011
* Purpose     : to be sure that the laOgVrFlt element APINVHDR.DPOSTDATE's is written as a string
* Called from : OG
***************************************************************************************
FUNCTION lfvPostingDate
LOCAL lnPos,lcVal
lnPos = ASUBSCRIPT(loOgScroll.laOgVrFlt,ASCAN(loOgScroll.laOgVrFlt,'APINVHDR.DPOSTDATE'),1)
lcVal = loOgScroll.laOgVrFlt[lnPos,6]

IF TYPE('lcVal') = 'D'
  loOgScroll.laOgVrFlt[lnPos,6] = DTOC(lcVal)
ENDIF
*-  end of lfvPostingDate

************************************************************************************************
* Name        : lfPolishExp
* Developer   : Tarek Mohammed Ibrahim - TMI
* Date        : 10/03/2011
* Purpose     : to remove a part of the filter from the lcRpExp
************************************************************************************************
FUNCTION lfPolishExp
PARAMETERS lcExp,lcRmv
LOCAL lnPos
lnPos = AT(lcRmv,lcExp)
DO WHILE lnPos>0
  lnAndPos = RAT(' AND ',SUBSTR(lcExp,1,lnPos))
  lcLeftStr = LEFT(lcExp,lnAndPos-1)
  lnPranth = AT(')',SUBSTR(lcExp,lnAndPos))
  lcRightStr = SUBSTR(lcExp,lnAndPos+lnPranth)
  lcExp = lcLeftStr+lcRightStr
  lnPos = AT(lcRmv,lcExp)
ENDDO
