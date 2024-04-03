*:***************************************************************************
*: Program file  : APPYINVD
*: Program desc. : Invoice Detail Report
*: System        : ARIA 4XP
*: Module        : Accounts Payable(AP)
*: Developer     : AHMED MOUSTAFA (AHS)
*: Date          : 08/09/2009
*: Tracking #    : E302940
*:***************************************************************************
* Modifications
*B609556,1 TMI 10/04/2011 [Start]
*E302940,5 Payable Invoice Detail TMI 10/04/2011, fix the criteria of the fiscal year to be on the APDIST not on the APINVHDR
*E302975,1 AP Conv.Proj. Attaching all files the phase to TMI 10/23/2011
*B609987,1 MMT 07/04/2012 Fix bug of incorrect data in case of selecting Period range in OG[T20120703.0025]
*:************************************************************************
*N000682,1 MMT 02/05/2013 Globalization changes[Start]
#INCLUDE  r:\aria4xp\reports\ap\appyinvd.H
*N000682,1 MMT 02/05/2013 Globalization changes[End]
*-- To check if the filters are changed, then collect the data again
IF loOgScroll.llOGFltCh

  =lfvCrTemp()        && To create Temp File
  =lfvColData()       && Data Collection

  SELECT &lcTempFile  && To delete the lines when line number is 0
  LOCATE
  DO WHILE !EOF(lcTempFile)
    lcKey = CVENDCODE+CCURRCODE+CINVNO
    SKIP
    DELETE REST WHILE CVENDCODE+CCURRCODE+CINVNO = lcKey ;
                  FOR NAPDLINNO = 0
  ENDDO
ENDIF

*- select the temp file to browse from
SELECT &lcTempFile

DO gfDispRe WITH EVAL('lcRpForm')

*- Check if temp file is closed then maybe the export to excel that closed it,
*- so reset collecting data again
IF !USED(lcTempFile)
  loOgScroll.llOGFltCh = .T.
ENDIF

*!**************************************************************************
*! Function      : lfvColData
*! Purpose       : Collection of data
*! Developer     : AHMED MOUSTAFA (AHS)
*! Date          : 08/09/2009
*!**************************************************************************
FUNCTION lfvColData

lnVendPos = lfGetPos('APDIST.CVENDCODE','laOgVrFlt')
lnCurrPos = lfGetPos('APINVHDR.CCURRCODE','laOgFxFlt')
lnMethPos = lfGetPos('APINVHDR.CVENPMETH','laOgVrFlt')
lnDivPos = lfGetPos('APINVHDR.CDIVISION','laOgVrFlt')
lnPrdPos = lfGetPos("APDIST.CFISFYEAR+'-'+APDIST.CFSPPRDID",'laOgVrFlt')
lcCurr = loOGScroll.laOGfxFlt[lnCurrPos,6]
lcVend = loOGScroll.laOGvrFlt[lnVendPos,6]
lcMeth = loOGScroll.laOGvrFlt[lnMethPos,6]
lcDiv = loOGScroll.laOGvrFlt[lnDivPos,6]
lcPrd = loOGScroll.laOGvrFlt[lnPrdPos,6]

STORE '' TO lcPrdFrom,lcPrdTo
IF !EMPTY(lcPrd) AND USED(lcPrd)
  SELECT &lcPrd
  LOCATE
  lcPrdFrom = KeyExp
  GO BOTTOM
  lcPrdTo   = KeyExp
ENDIF

llVenSlct = .F.
IF !EMPTY(lcVend) AND USED(lcVend)
  SELECT &lcVend
  LOCATE
  llVenSlct = !EOF()
ENDIF


*N000682 ,1 Thabet Handle globalization issues [Start]
*WAIT WINDOW NOWAIT 'Collecting Data ...[ESC to abort]'
WAIT WINDOW NOWAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_COLLECT_DATA,oAriaApplication.GetHeaderText("LANG_COLLECT_DATA",AHEADERFILE))
*N000682 ,1 Thabet Handle globalization issues [END]

*E302940,5 Payable Invoice Detail TMI 10/17/2011 [Start] get the extra added filters from the lcRpExp to allow the
* advanced option to work
lcRpExp = STRTRAN(lcRpExp,"  "," ")
lcRpExp = '.T. AND '+lcRpExp
=lfReplExp(@lcRpExp,"AND APINVHDR.CCURRCODE",'')
=lfPolishExp(@lcRpExp,"(APDIST.CVENDCODE")
=lfPolishExp(@lcRpExp,"(APINVHDR.CDIVISION")
=lfPolishExp(@lcRpExp,"(APINVHDR.CVENPMETH")
*B609987,1 MMT 07/04/2012 Fix bug of incorrect data in case of selecting Period range in OG[T20120703.0025][Start]
*=lfPolishExp(@lcRpExp,"(APDIST.CFISFYEAR")
=lfPolishExp(@lcRpExp,"(APDIST.CFISFYEAR+'-'+APDIST.CFSPPRDID")
*B609987,1 MMT 07/04/2012 Fix bug of incorrect data in case of selecting Period range in OG[T20120703.0025][End]
lcRpExp = ALLTRIM(lcRpExp)
lcRpExp = IIF( lcRpExp=='.T.','',SUBSTR(lcRpExp,5))
*E302940,5 Payable Invoice Detail TMI 10/17/2011 [End  ]

*E302940,5 Payable Invoice Detail TMI 10/04/2011 [Start] fix the criteria of the fiscal year to be on the APDIST not on the APINVHDR
*E302940,5 Payable Invoice Detail TMI 10/17/2011 [Start]
*lcFisPrd = "FOR " + IIF(!EMPTY(lcPrdFrom),"BETWEEN(APDIST.cfisfyear+'-'+APDIST.cfspprdid,'&lcPrdFrom','&lcPrdTo')","")
lcFisPrd = IIF(!EMPTY(lcPrdFrom),"FOR BETWEEN(APDIST.cfisfyear+'-'+APDIST.cfspprdid,'&lcPrdFrom','&lcPrdTo')","")
*E302940,5 Payable Invoice Detail TMI 10/17/2011 [End  ]
*E302940,5 Payable Invoice Detail TMI 10/04/2011 [End  ]

IF llVenSlct
  *--activate the lcVend temp alias
  SELECT &lcVend
  SCAN
    gfseek(&lcVend..cVendCode,'APINVHDR')
    * send the vendor code as a parameter
    =lfInsLine(&lcVend..cVendCode)
  ENDSCAN
ELSE
  gfseek('','APINVHDR')
  =lfInsLine()
ENDIF

WAIT CLEAR


*!*************************************************************
*! Name      : lfReplExp
*! Developer : Tarek Mohamed Ibrahim
*! Date      : 10/17/2011
*! Purpose   : I created this function as I faced the following problem
* The lcRpExp is created with expression contains
*                      APINVHDR.CBNKCODE = 'CHASE'
* which it should be
*                      APINVHDR.CBNKCODE = 'CHASE   '
* But this would cause problems especially in case if there are more than one bank code with same initials
*!*************************************************************
FUNCTION lfReplExp
LPARAMETERS lcExp,lcFind,lcRepl
LOCAL lnPos,lnAndPos
lnPos = AT(lcFind,lcExp)
IF lnPos>0
  lnAndPos = AT(' AND ',SUBSTR(lcExp,lnPos))
  lnAndPos = IIF(lnAndPos<>0,lnAndPos-1,LEN(SUBSTR(lcExp,lnPos)))
  lcExp = STUFF(lcExp,lnPos,lnAndPos,lcRepl)
ENDIF
*- end of lfReplExp

************************************************************************************************
* Name        : lfPolishExp
* Developer   : Tarek Mohammed Ibrahim - TMI
* Date        : 10/03/2011
* Purpose     : to remove a part of the filter from the lcRpExp
************************************************************************************************
FUNCTION lfPolishExp
PARAMETERS lcExp,lcRmv
LOCAL lnPos,lcRight
lcRight = ")"
lnPos = AT(lcRmv,lcExp)
DO WHILE lnPos>0
  lnAndPos = RAT(' AND ',SUBSTR(lcExp,1,lnPos))
  lcLeftStr = LEFT(lcExp,lnAndPos-1)
  lnPranth = AT(lcRight,SUBSTR(lcExp,lnAndPos))
  lcRightStr = SUBSTR(lcExp,lnAndPos+lnPranth+LEN(lcRight)-1)
  lcExp = lcLeftStr+lcRightStr
  lnPos = AT(lcRmv,lcExp)
ENDDO

*!**************************************************************************
*! Function      : lfInsLine
*! Purpose       : Inserting lines in Temp File
*! Developer     : AHMED MOUSTAFA (AHS)
*! Date          : 08/09/2009
*!**************************************************************************
FUNCTION lfInsLine
PARAMETERS lcVendCode
lcVendCode = IIF(EMPTY(lcVendCode),'',lcVendCode)

SELECT APINVHDR
*E302940,5 Payable Invoice Detail TMI 10/04/2011 [Start] fix the criteria of the fiscal year to be on the APDIST not on the APINVHDR
*SCAN REST WHILE CVENDCODE+CINVNO = lcVendCode AND INKEY()<>27
*  IF IIF(!EMPTY(loOGScroll.laOGfxFlt[lnCurrPos,6]),APINVHDR.CCURRCODE $ loOGScroll.laOGfxFlt[lnCurrPos,6] , .T. ) AND ;
*    IIF(!EMPTY(loOGScroll.laOGvrFlt[lnMethPos,6]),APINVHDR.CVENPMETH $ loOGScroll.laOGvrFlt[lnMethPos,6],.T.) AND ;
*    IIF(!EMPTY(loOGScroll.laOGvrFlt[lnDivPos,6]),APINVHDR.CDIVISION $ loOGScroll.laOGvrFlt[lnDivPos,6],.T.) AND ;
*    IIF(llRpIncInv,.T.,APINVHDR.CINVSTAT <> 'V') AND ;
*    IIF(lcRpStatus='A',.T.,IIF(lcRpStatus='O',;
*       ABS(APINVHDR.NINVAMNT) > ABS(APINVHDR.NINVPAID)+ABS(APINVHDR.NINVADJ)+ABS(APINVHDR.NINVDISTK),;
*       ABS(APINVHDR.NINVAMNT) = ABS(APINVHDR.NINVPAID)+ABS(APINVHDR.NINVADJ)+ABS(APINVHDR.NINVDISTK))) AND ;
*    IIF(!EMPTY(lcRpRefnce),LIKE(STRTRAN(lcRpRefnce,' ','?'),APINVHDR.cinvref),.T.) AND ;
*    IIF(!EMPTY(lcRpSess),lfFltSess(),.T.) AND ;
*    IIF(!EMPTY(lcPrdFrom),BETWEEN(apinvhdr.cfisfyear+'-'+apinvhdr.cfspprdid,lcPrdFrom,lcPrdTo),.T.)
*E302940,5 Payable Invoice Detail TMI 10/17/2011 [Start]
*!*	SCAN REST WHILE CVENDCODE+CINVNO = lcVendCode AND INKEY()<>27
*!*	  IF IIF(!EMPTY(loOGScroll.laOGfxFlt[lnCurrPos,6]),APINVHDR.CCURRCODE $ loOGScroll.laOGfxFlt[lnCurrPos,6] , .T. ) AND ;
*!*	    IIF(!EMPTY(loOGScroll.laOGvrFlt[lnMethPos,6]),APINVHDR.CVENPMETH $ loOGScroll.laOGvrFlt[lnMethPos,6],.T.) AND ;
*!*	    IIF(!EMPTY(loOGScroll.laOGvrFlt[lnDivPos,6]),APINVHDR.CDIVISION $ loOGScroll.laOGvrFlt[lnDivPos,6],.T.) AND ;
*!*	    IIF(llRpIncInv,.T.,APINVHDR.CINVSTAT <> 'V') AND ;
*!*	    IIF(lcRpStatus='A',.T.,IIF(lcRpStatus='O',;
*!*	       ABS(APINVHDR.NINVAMNT) > ABS(APINVHDR.NINVPAID)+ABS(APINVHDR.NINVADJ)+ABS(APINVHDR.NINVDISTK),;
*!*	       ABS(APINVHDR.NINVAMNT) = ABS(APINVHDR.NINVPAID)+ABS(APINVHDR.NINVADJ)+ABS(APINVHDR.NINVDISTK))) AND ;
*!*	    IIF(!EMPTY(lcRpRefnce),LIKE(STRTRAN(lcRpRefnce,' ','?'),APINVHDR.cinvref),.T.) AND ;
*!*	    IIF(!EMPTY(lcRpSess),lfFltSess(),.T.)
SCAN REST WHILE CVENDCODE+CINVNO = lcVendCode AND INKEY()<>27
  IF IIF(!EMPTY(loOGScroll.laOGfxFlt[lnCurrPos,6]),APINVHDR.CCURRCODE $ loOGScroll.laOGfxFlt[lnCurrPos,6] , .T. ) AND ;
    IIF(!EMPTY(loOGScroll.laOGvrFlt[lnMethPos,6]),APINVHDR.CVENPMETH $ loOGScroll.laOGvrFlt[lnMethPos,6],.T.) AND ;
    IIF(!EMPTY(loOGScroll.laOGvrFlt[lnDivPos,6]),APINVHDR.CDIVISION $ loOGScroll.laOGvrFlt[lnDivPos,6],.T.) AND ;
    IIF(llRpIncInv,.T.,APINVHDR.CINVSTAT <> 'V') AND ;
    IIF(lcRpStatus='A',.T.,IIF(lcRpStatus='O',;
       ABS(APINVHDR.NINVAMNT) > ABS(APINVHDR.NINVPAID)+ABS(APINVHDR.NINVADJ)+ABS(APINVHDR.NINVDISTK),;
       ABS(APINVHDR.NINVAMNT) = ABS(APINVHDR.NINVPAID)+ABS(APINVHDR.NINVADJ)+ABS(APINVHDR.NINVDISTK))) AND ;
    IIF(!EMPTY(lcRpRefnce),LIKE(STRTRAN(lcRpRefnce,' ','?'),APINVHDR.cinvref),.T.) AND ;
    IIF(!EMPTY(lcRpSess),lfFltSess(),.T.) ;
    &lcRpExp
  *E302940,5 Payable Invoice Detail TMI 10/17/2011 [End  ]
  *E302940,5 Payable Invoice Detail TMI 10/04/2011 [End  ]

    SCATTER MEMVAR memo

    gfseek(APINVHDR.CINVNO+APINVHDR.CVENDCODE,'APDIST')
    SELECT APDIST
    *E302940,5 Payable Invoice Detail TMI 10/04/2011 [Start] fix the criteria of the fiscal year to be on the APDIST not on the APINVHDR
    *SCAN REST WHILE CINVNO+CVENDCODE+CAPDTRTYP = APINVHDR.CINVNO+APINVHDR.CVENDCODE
    SCAN REST WHILE CINVNO+CVENDCODE+CAPDTRTYP = APINVHDR.CINVNO+APINVHDR.CVENDCODE ;
         &lcFisPrd
      *E302940,5 Payable Invoice Detail TMI 10/04/2011 [End  ]
      M.CAPDSTAT = APDIST.CAPDSTAT
      M.CAPDACTID = APDIST.CAPDACTID
      M.CAPDGLACT = APDIST.CAPDGLACT

      SELECT GLACCHAR
      gfseek(APDIST.CAPDGLACT,'GLACCHAR')

      SELECT &lcTempFile
      M.CVENPMETH = SUBSTR(lcRpVldEnt,ATC('~',lcRpVldEnt,ATC(LEFT(m.cvenpmeth,1),lcRpVldVal))+1,(ATC('~',lcRpVldEnt,ATC(LEFT(m.cvenpmeth,1),lcRpVldVal)+1)-1)-(ATC('~',lcRpVldEnt,ATC(LEFT(m.cvenpmeth,1),lcRpVldVal))))
      M.INVAMNT1 = gfAmntDisp(APINVHDR.NINVAMNT,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.)
      M.PAIDAMNT1 = IIF(APINVHDR.CINVSTAT='V',0,gfAmntDisp(APINVHDR.NINVPAID,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.))
      M.DIS_TAK1 = gfAmntDisp(APINVHDR.NINVDISTK,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.)
      M.ADJ_AMNT1 = gfAmntDisp(APINVHDR.NINVADJ,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.)
      M.OPEN_BAL1 = gfAmntDisp(APINVHDR.NINVAmnt-APINVHDR.NINVPAID-APINVHDR.NINVDISTK-APINVHDR.NINVADJ,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.)
      M.NAPDAMNT  = IIF(APDIST.CAPDSTAT<>'V' AND APDIST.CAPDACTID='D',gfAmntDisp(APDIST.NAPDAMNT,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.),0)
      M.NAPDLINNO = IIF(APDIST.CAPDSTAT<>'V' AND APDIST.CAPDACTID='D',APDIST.NAPDLINNO,0)
      M.CGLACCNT = IIF(APDIST.CAPDSTAT<>'V' AND APDIST.CAPDACTID='D',ALLT(APDIST.CAPDGLACT)+' '+GLACCHAR.CACCNLDES,'')
      INSERT INTO &lcTempFile FROM MEMVAR
    ENDSCAN
  ENDIF
ENDSCAN

*!**************************************************************************
*! Function      : lfGetPos
*! Purpose       : Getting the number of element from array
*! Developer     : AHMED MOUSTAFA (AHS)
*! Date          : 08/03/2009
*!**************************************************************************
FUNCTION lfGetPos
PARAMETERS lcOpt,lcArray
LOCAL lnPos
lnPos = ASCAN(loOGScroll.&lcArray,lcOpt)
lnPos = ASUBSCRIPT(loOGScroll.&lcArray,lnPos,1)
RETURN lnPos
*!**************************************************************************
*! Function      : lfvCrTemp
*! Purpose       : Creating Temp file
*! Developer     : AHMED MOUSTAFA (AHS)
*! Date          : 08/09/2009
*!**************************************************************************
FUNCTION lfvCrTemp

IF USED(lcTempFile) AND RECCOUNT(lcTempFile) > 0
  SELECT (lcTempFile)
  ZAP
ENDIF
*-- Create File
IF !USED(lcTempFile)
  DIMENSION laTempStru[23,4]
  laTempstru[1,1]='CVENDCODE'
  laTempstru[1,2]='C'
  laTempstru[1,3]= 8
  laTempstru[1,4]= 0

  laTempstru[2,1]='COUTCOMP'
  laTempstru[2,2]='C'
  laTempstru[2,3]= 40
  laTempstru[2,4]= 0

  laTempstru[3,1]='CINVNO'
  laTempstru[3,2]='C'
  laTempstru[3,3]= 12
  laTempstru[3,4]= 0

  laTempstru[4,1]='CINVREF'
  laTempstru[4,2]='C'
  laTempstru[4,3]= 15
  laTempstru[4,4]= 0

  laTempstru[5,1]='DINVDATE'
  laTempstru[5,2]='D'
  laTempstru[5,3]= 10
  laTempstru[5,4]= 0

  laTempstru[6,1]='DINVDUDAT'
  laTempstru[6,2]='D'
  laTempstru[6,3]= 10
  laTempstru[6,4]= 0

  laTempstru[7,1]='CVENPRIOR'
  laTempstru[7,2]='C'
  laTempstru[7,3]= 1
  laTempstru[7,4]= 0

  laTempstru[8,1]='CVENPMETH'
  laTempstru[8,2]='C'
  laTempstru[8,3]= 30
  laTempstru[8,4]= 0

  laTempstru[9,1]='INVAMNT1'
  laTempstru[9,2]='N'
  laTempstru[9,3]= 15
  laTempstru[9,4]= 2

  laTempstru[10,1]='PAIDAMNT1'
  laTempstru[10,2]='N'
  laTempstru[10,3]= 15
  laTempstru[10,4]= 2

  laTempstru[11,1]='DIS_TAK1'
  laTempstru[11,2]='N'
  laTempstru[11,3]= 15
  laTempstru[11,4]= 2

  laTempstru[12,1]='ADJ_AMNT1'
  laTempstru[12,2]='N'
  laTempstru[12,3]= 15
  laTempstru[12,4]= 2

  laTempstru[13,1]='OPEN_BAL1'
  laTempstru[13,2]='N'
  laTempstru[13,3]= 15
  laTempstru[13,4]= 2

  laTempstru[14,1]='CCURRCODE'
  laTempstru[14,2]='C'
  laTempstru[14,3]= 3
  laTempstru[14,4]= 0

  laTempstru[15,1]='CINVSTAT'
  laTempstru[15,2]='C'
  laTempstru[15,3]= 8
  laTempstru[15,4]= 0

  laTempstru[16,1]='CAPDSTAT'
  laTempstru[16,2]='C'
  laTempstru[16,3]= 10
  laTempstru[16,4]= 0

  laTempstru[17,1]='CAPDACTID'
  laTempstru[17,2]='C'
  laTempstru[17,3]= 10
  laTempstru[17,4]= 0

  laTempstru[18,1]='NAPDAMNT'
  laTempstru[18,2]='N'
  laTempstru[18,3]= 15
  laTempstru[18,4]= 2

  laTempstru[19,1]='CAPDGLACT'
  laTempstru[19,2]='C'
  laTempstru[19,3]= 24
  laTempstru[19,4]= 0

  laTempstru[20,1]='NAPDLINNO'
  laTempstru[20,2]='N'
  laTempstru[20,3]= 8
  laTempstru[20,4]= 0

  laTempstru[21,1]='CACCNLDES'
  laTempstru[21,2]='C'
  laTempstru[21,3]= 65
  laTempstru[21,4]= 0

  laTempstru[22,1]='CGLACCNT'
  laTempstru[22,2]='C'
  laTempstru[22,3]= 90
  laTempstru[22,4]= 0

  laTempstru[23,1]='CAPACCT'
  laTempstru[23,2]='C'
  laTempstru[23,3]= 24
  laTempstru[23,4]= 0

  =gfCrtTmp(lcTempFile,@laTempStru,"CVENDCODE+CCURRCODE+CINVNO",lcTempFile,.T.)
ENDIF

*--End of function

*!**************************************************************************
*! Function      : lfwRepWhen
*! Purpose       : When function
*! Developer     : AHMED MOUSTAFA (AHS)
*! Date          : 08/03/2009
*!**************************************************************************
FUNCTION lfwRepWhen
loOgScroll.laOGFxFlt[1,6] = oAriaApplication.basecurrency

SELECT FSPRD
=gfseek('')

SELECT APSETUP
=gfSeek('')

*!**************************************************************************
*!
*!      Function: lfRepShow
*!
*!**************************************************************************
*
FUNCTION lfRepShow
laOGObjCnt[6] = gfGetMemVar('LLMULCURR')

*!**************************************************************************
*!
*!      Function: lfvSess
*!
*!**************************************************************************
FUNCTION lfvSess

PRIVATE ALL LIKE lcOldAl*

IF EMPTY(lcRpSess)
  RETURN
ENDIF

lcRpSess= PADL(ALLTRIM(lcRpSess),8,'0')
LOCAL loFld
loFld = loOgScroll.ActiveControl

IF gfSEEK('CAPSESSNO ','SEQUENCE')
  IF !BETWEEN(VAL(lcRpSess),1,SEQUENCE.nSeq_No)
    =gfModalGen("TRM04158B00000","DIALOG",ALLTRIM(STR(SEQUENCE.nSeq_No)))
    lcRpSess = ''

    RETURN
  ENDIF
ENDIF

*************************************************************************
*!
*!      Function: lfFltSess
*!
*************************************************************************

FUNCTION lfFltSess

lcOldFl = ALIAS()
SELECT APDIST
=gfSeek(APINVHDR.CINVNO + APINVHDR.CVENDCODE)

LOCATE REST WHILE CINVNO+CVENDCODE+CAPDTRTYP = APINVHDR.CINVNO + APINVHDR.CVENDCODE ;
            FOR   cApSessNo = lcRpSess

IF EMPTY(lcOldFl)
  SELECT 0
ELSE
  SELECT (lcOldFl)
ENDIF
RETURN FOUND("APDIST")

*!**************************************************************************
*!
*!      Function: lfvCurDisp
*!
*!**************************************************************************
*- Add the currency to the AP reports.
FUNCTION lfvCurDisp

llRpProced = gfRepCur(.T., @lcRpCurr,@ldRpExDate,lcRpTmpNam)

*!*************************************************************
*! Name      : lfwCurCode
*! Developer : Tarek Mohamed Ibrahim
*! Date      : 08/15/2011
*! Purpose   : This function called from the currency field to
*!             validate the currency.
*!*************************************************************
FUNCTION lfwCurCode

lcOldCurr = laOGFxFlt[1,6]

*!*************************************************************
*! Name      : lfvCurCode
*! Developer : Hesham Elmasry
*! Date      : 02/28/2010
*! Purpose   : This function called from the currency field to
*!             validate the currency.
*!*************************************************************
FUNCTION lfvCurCode

LOCAL loFld
loFld = loOgScroll.ActiveControl

IF EMPTY(loFld.Value)
  llAllCurr  = .T.
  RETURN
ENDIF
llAllCurr  = .F.

IF !SEEK(loFld.Value,'SYCCURR') .OR. ATC("?",loFld.Value) > 0
  SELECT SYCCURR
  DIMENSION laTemp[1]
  laTemp     = ''
  lcSavBrFld = lcBrFields
  lcSavTitle = lcFile_Ttl

  * N000682 ,1 Thabet Handle globalization issues [Start]
  *  lcFile_Ttl = "Currency"
  *  lcBrFields = "CCURRCODE :R :H= 'Currency code'," +;
                 "CCURRDESC :R :H= 'Description',  " +;
                 "CCURRSMBL :R :H= 'Symbol'"
  lcFile_Ttl = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CURRENCY,oAriaApplication.GetHeaderText("LANG_CURRENCY",AHEADERFILE))
  lcBrFields = "CCURRCODE :R :H= '"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CURRENCY_CODE,oAriaApplication.GetHeaderText("LANG_CURRENCY_CODE",AHEADERFILE))+"'," +;
               "CCURRDESC :R :H= '"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_DESCRIPTION,oAriaApplication.GetHeaderText("LANG_DESCRIPTION",AHEADERFILE))+"',  " +;
        		 	"CCURRSMBL :R :H= '"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SYMBOL,oAriaApplication.GetHeaderText("LANG_SYMBOL",AHEADERFILE))+"'"
  * N000682 ,1 Thabet Handle globalization issues [END]
  =gfBrows('','CCURRCODE','laTemp')
  lcBrFields = lcSavBrFld
  lcFile_Ttl = lcSavTitle
  loFld.Value = laTemp[1]
  lnPOS = ASCAN(loOgScroll.laOGFxFlt,"APINVHDR.CCURRCODE")
  IF lnPos > 0
    lnPOS    = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnPos,1)
    loOgScroll.laOGFxFlt[lnPOS,6] = laTemp[1]
  ENDIF
ENDIF

SELECT APINVHDR

*!*************************************************************
*! Name      : lfvPayMeth
*! Developer : Mariam MAzhar
*! Date      : 10/19/2009
*! Purpose   : Validate Payment Method
*!*************************************************************
FUNCTION lfvPayMeth
PRIVATE lcOldStat,lcCurrChr

lcOldStat = lcRpVenPay
*N000682,1 MMT 02/05/2013 Globalization changes[Start]
*= lfOGMover(@laRpSource,@laRpTarget,'Select Customer Status',.T.,'')  && call mover function.
=lfOGMover(@laRpSource,@laRpTarget,;
           IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SELECTPAY,oAriaApplication.GetHeaderText("LANG_SELECTPAY",AHEADERFILE)),.T.,'')  && call mover function.
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 MMT 02/05/2013 Globalization changes[Start]
lcRpVenPay = ' '
*-- Loop to make Status expression.
FOR lnI = 1 TO ALEN(laRpTarget,1)
  *N000682,1 MMT 02/05/2013 Globalization changes[Start]
*!*	  lcRpVenPay= lcRpVenPay+ IIF(laRpTarget[lnI] = 'Printed Checks',"'P'",;
*!*	                            IIF(laRpTarget[lnI] = 'Manual Checks',",'M'",;
*!*	                            IIF(laRpTarget[lnI] = 'Non Check Payments',",'N'",;
*!*	                            IIF(laRpTarget[lnI] = 'Credit Card',",'C'",IIF(laRpTarget[lnI] = 'Cash Payments' ,",'H'",'')))))
  lcRpVenPay= lcRpVenPay+ IIF(laRpTarget[lnI] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_PRINTEDCHECK,oAriaApplication.GetHeaderText("LANG_PRINTEDCHECK",AHEADERFILE)),"'P'",;
                          IIF(laRpTarget[lnI] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MANUALCHECK,oAriaApplication.GetHeaderText("LANG_MANUALCHECK",AHEADERFILE)),",'M'",;
                          IIF(laRpTarget[lnI] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_NONCHECK,oAriaApplication.GetHeaderText("LANG_NONCHECK",AHEADERFILE)),",'N'",;
                          IIF(laRpTarget[lnI] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CREDITCARD,oAriaApplication.GetHeaderText("LANG_CREDITCARD",AHEADERFILE)),",'C'",;
                          IIF(laRpTarget[lnI] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CASHPAY,oAriaApplication.GetHeaderText("LANG_CASHPAY",AHEADERFILE)) ,",'H'",'')))))
  *N000682,1 MMT 02/05/2013 Globalization changes[END]
ENDFOR  && end Loop to make Status expression.
lcRpVenPay= IIF(EMPTY(lcRpVenPay),lcRpVenPay,ALLTRIM(lcRpVenPay))

*-- Compare current selected status with old value  [begin]
*-- to rise change status flag.

*-- if length of current selected status differ from previous length
IF LEN(lcOldStat) != LEN(lcRpVenPay)
  llChStatus = .T.

ELSE  && else if length of current selected status equal previous length
  *-- loop to check if it's the same selected status or not.
  FOR lnJ = 1 TO LEN(lcOldStat)
    lcCurrChr = SUBSTR(lcOldStat,lnJ,lnJ)
    IF !(lcCurrChr $ lcRpVenPay)
      llChStatus = .T.
      EXIT
    ENDIF
  ENDFOR  && end loop to check if it's the same selected status or not.
ENDIF
*-- Compare current selected status with old value  [end]
*-- End of lfvOStatus.


*!*************************************************************
*! Name      : RefreshStatus
*! Developer : MARIAM MAZHAR (MMT)
*! Date      : 10/19/2009
*! Purpose   : Return the selected Payment Method in the ReadBox
*!*************************************************************
FUNCTION RefreshStatus
  LOCAL lcStatusStr, lnTarget
  lcStatusStr = ""
  IF !EMPTY(laRpTarget)
    FOR lnTarget = 1 TO ALEN(laRpTarget,1)
      lcStatusStr = lcStatusStr + ", " + laRpTarget[lnTarget]
    ENDFOR
    lcStatusStr = SUBSTR(lcStatusStr,3)
  ENDIF
  RETURN lcStatusStr
ENDFUNC


**********************************************************************
*
*             lfvInvoice
*
**********************************************************************
FUNCTION lfvInvoice

LOCAL lnRpIncInv
llRpIncInv=IIF(lcRpStatus=='P',.F.,llRpIncInv)

lnRpIncInv = ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,'LLRPINCINV'),1)
laOGObjCnt[lnRpIncInv] = lcRpStatus <> 'P'
= lfOGShowGet('LLRPINCINV')


*:**************************************************************************
*:* Name        : lfSRVPerd
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/30/2010
*:* Purpose     : Set-Reset-Valid function for the periods selected
*:***************************************************************************
*:* Called from : OG
*:***************************************************************************
FUNCTION lfSRVPerd
PARAMETERS lcSRV
LOCAL lnPos,lnSlct,lcSlcPrd,lnCnt
lnSlct = SELECT()

DO CASE
CASE lcSRV = 'R'
  *- be sure that only two id's are selected at most
  lnPrdPos = lfGetPos("APDIST.CFISFYEAR+'-'+APDIST.CFSPPRDID",'laOgVrFlt')
  SELECT (loOgScroll.laOGVrFlt[lnPrdPos,6])
  LOCATE
  COUNT TO lnCnt
  IF lnCnt>2
    LOCATE
    FOR lnI = 2 TO lnCnt-1
      SKIP
      DELETE
    ENDFOR
  ENDIF
ENDCASE

SELECT(lnSlct)
*-- end of lfSRVPerd.
