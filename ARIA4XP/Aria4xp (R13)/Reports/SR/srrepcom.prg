*:***************************************************************************
*: Program file  : SRREPCOM
*: Program desc. : Sales Representative commission statement Report
*: System        : Aria4XP.
*: Module        : Sales Representative (SR )
*: Developer     : AYMAN MAHMOUD AHMED  (AYM)
*:TRACK NO       : N040291
*:***************************************************************************
*: Calls :
*:    Procedures : ENDREPORT
*:    Functions  : gfModalGen,gfGetAdr,lfwRepWhen,lfFltState,lfAdrShift
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO SRREPCOM
*:***************************************************************************
*: Modifications:
*: B608695,1 MMT 09/23/2008 Fix bug of wrong amount in Mutli currency case [T20080909.0056]
*B609554,1 TMI 03/22/2011 fix a problem file must be opened exclusively [T20110321.0007]
*B609642,1 WAM 07/12/2011 Change sorting of REPCOMM file so that the payment comes at the end of the period [T20110707.0034]
*B610054,1 MMT 08/23/2012 Cannot export Sales rep. Commission Statement report to PDF[T20120820.0001]
*E303286,1 MMT 10/29/2012 Change commission statement report to print the amount foreign currency[T20120418.0014]
*:***************************************************************************
#Include r:\aria4xp\reports\SR\srrepcom.h
IF llOgFltCh
llDonprnt=.F.
lcFilter = '.T.'

DECLARE laCompAdd[5]
STORE '' TO laCompAdd
*E303286,1 MMT 10/29/2012 Change commission statement report to print the amount foreign currency[Start]
lcCurrPost = "LEFT"                              && Default Value.
*E303286,1 MMT 10/29/2012 Change commission statement report to print the amount foreign currency[End]
=lfFilcomp()
lnDatePos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'REPCOMM.DATE'),1)
LDATE = SUBSTR(laOGFxFlt[lnDatePos,6],1,ATC('|',laOGFxFlt[lnDatePos,6])-1)
HDATE = SUBSTR(laOGFxFlt[lnDatePos,6],  ATC('|',laOGFxFlt[lnDatePos,6])+1)

lnPayDatePos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'REPCOMM.PAYDATE'),1)
XLPAYDATE  =SUBSTR(laOGFxFlt[lnPayDatePos,6],1,ATC('|',laOGFxFlt[lnPayDatePos,6])-1)
XHPAYDATE  = SUBSTR(laOGFxFlt[lnPayDatePos,6],  ATC('|',laOGFxFlt[lnPayDatePos,6])+1)
TODAY      = DATE()
DCA = lcRpBal

STORE '' TO lcRepFltr
* SalesRep Filter
lcRepFltr= lfCheckFilter(1, 'SALESREP.REPCODE')
llRepFltr   = !EMPTY(lcRepFltr) AND USED(lcRepFltr) AND RECCOUNT(lcRepFltr) > 0
IF llRepFltr
  SELECT (lcRepFltr)
  INDEX ON REPCODE TAG (lcRepFltr)
ELSE
  IF TYPE("lcRepFltr") = "C" AND USED(lcRepFltr)
    USE IN (lcRepFltr)
  ENDIF
  lcRepFltr= ''
ENDIF

* CURRENCY Filter
IF llMultCurr
  lcCurFltr= lfCheckFilter(1, 'REPCOMM.CCURRCODE')
  llCurFltr   = !EMPTY(lcCurFltr) AND USED(lcCurFltr) AND RECCOUNT(lcCurFltr) > 0
  IF llCurFltr
    SELECT (lcCurFltr)
    INDEX ON CCURRCODE TAG (lcCurFltr)
    lcFilter=" SEEK(CCURRCODE ,'"+lcCurFltr+"')"
  ELSE
    IF TYPE("lcCurFltr") = "C" AND USED(lcCurFltr)
      USE IN (lcCurFltr)
    ENDIF
    lcCurFltr= ''
  ENDIF
ENDIF



lcJoin=''
IF llRepFltr
  lcJoin=lcJoin + " inner join " +lcRepFltr+" TmpRep on SALESREP.REPCODE=TmpRep.REPCODE "
ENDIF

lcWhere=' WHERE .T.'
lcWhere=lcWhere+IIF(DCA = "A","",IIF(DCA = "C"," AND SALESREP.BALANCE < 0"," AND SALESREP.BALANCE> 0"))

lcexpR=lcFilter
IF !EMPTY(LDATE)
  lcexpR=lcexpR+" AND BETWEEN(DATE,CTOD('"+LDATE +"'),CTOD('"+HDATE+"'))"
ELSE
  IF  !EMPTY(HDATE)
    lcexpR=lcexpR+" AND DATE<=CTOD('"+HDATE+"')"
  ENDIF
ENDIF

lcexpM=lcFilter +' AND  STATUS="P" '
IF !EMPTY(XLPAYDATE )
  lcexpM=lcexpM+" AND BETWEEN(PAYDATE,CTOD('"+XLPAYDATE+"'),CTOD('"+XHPAYDATE+"'))"
ELSE
  IF  !EMPTY(XHPAYDATE)
    lcexpM=lcexpM+" AND PAYDATE<=CTOD('"+XHPAYDATE+"')"
  ENDIF
ENDIF

*!* GETS REQUIRED sALESREPS
lcFields=          " SALESREP.repcode ,SALESREP.name ,SALESREP.pay_type ,SALESREP.FAX,SALESREP.PHONE,SALESREP.SSN ,"
lcFields=lcFields+ " SALESREP.caddress1 ,SALESREP.caddress2 ,SALESREP.caddress3 ,SALESREP.caddress4 , "
lcFields=lcFields+ " SALESREP.caddress5 ,SALESREP.caddress6 "
lcFrom=" FROM SALESREP  "+lcJoin

lcTmpRep   =loOgScroll.gfTempName()
lcREPCOMM  =loOgScroll.gfTempName()
lcTempFile =loOgScroll.gfTempName()
lcWorkfile =loOgScroll.gfTempName()
lcBgBAL  =loOgScroll.gfTempName()
lcLogotmp=loOgScroll.gfTempName()

SELECT &lcFields. &lcFrom. &lcWhere INTO CURSOR (lcTmpRep)

SELECT (lcTmpRep)
IF !RECCOUNT()>0
    =gfModalGen('TRM00052B40011','ALERT')
  llDonprnt=.T.
  RETURN
ENDIF
=CURSORSETPROP("Buffering" ,3)
INDEX ON REPCODE TAG (lcTmpRep) OF   (gcWorkDir+lcTmpRep+'.CDX')


SELECT REPCOMM
*B609642,1 WAM 07/12/2011 Change sorting of REPCOMM file so that the payment comes at the end of the period
*!*	IF llMultCurr
*!*	  INDEX ON REPCODE+CCURRCODE+DTOS(DATE)+TRAN+TRANTYPE TAG (lcREPCOMM) OF   (gcWorkDir+lcREPCOMM+'.CDX')
*!*	ENDIF
IF llMultCurr
  INDEX ON REPCODE+CCURRCODE+DTOS(DATE) TAG (lcREPCOMM) OF (gcWorkDir+lcREPCOMM+'.CDX')
ELSE
  INDEX ON REPCODE+DTOS(DATE) TAG (lcREPCOMM) OF (gcWorkDir+lcREPCOMM+'.CDX')
ENDIF
*B609642,1 WAM 07/12/2011 (End)

=lfBuildTmp()  && BUILD WORK FILE
=lfBgBlanc ()  && GETS GEGIN GbALAMCE OF SELECTED SALESREP


DIMENSION  laRepData[6]
STORE '' TO laRepData


RUNBAL=0
lnBgbal=0
SELECT (lcTmpRep)
SET RELATION TO REPCODE INTO SALESREP
SCAN FOR pay_type="R"
  XSALESREP=REPCODE
  SELECT REPCOMM
  IF SEEK(XSALESREP)
* N000862 ,1 Thabet Handle globalization issues [Start]
*  WAIT WINDOW 'Collecting statements for sales representative ' + XSALESREP NOWAIT
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW Lang_Collecting_statements + XSALESREP NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",Lang_Collecting_statements,oAriaApplication.GetHeaderText("Lang_Collecting_statements",AHEADERFILE)) + XSALESREP NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]

  * N000862 ,1 Thabet Handle globalization issues [END]
    =lfFilArray()
    IF llMultCurr AND lcRpCurr = "F"
      lcCurrtCur =ccurrcode
      lnBgbal=IIF(SEEK(repcode+lcCurrtCur ,lcbgbal),&lcbgbal..amnts,0)
      RUNBAL=lnBgbal
    ELSE
      lnBgbal=IIF(SEEK(repcode,lcbgbal),&lcbgbal..amnts,0)
      RUNBAL=lnBgbal
    ENDIF
    SCAN WHILE repcode=XSALESREP FOR &lcexpR
      IF llMultCurr AND lcRpCurr = "F"  AND (lcCurrtCur<>ccurrcode)
        lcCurrtCur =ccurrcode
        lnBgbal=IIF(SEEK(repcode+lcCurrtCur ,lcbgbal),&lcbgbal..amnts,0)
        RUNBAL=lnBgbal
      ENDIF
      SCATTER MEMVAR
      =FILLTEMP()
    ENDSCAN
  ENDIF
ENDSCAN

RUNBAL=0
lnBgbal=0
SELECT (lcTmpRep)
SCAN FOR pay_type="M"
  RUNBAL=0
  XSALESREP=REPCODE
  SELECT REPCOMM
  IF SEEK(XSALESREP)
    * N000862 ,1 Thabet Handle globalization issues [Start]
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW Lang_Collecting_sales_representative + XSALESREP NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",Lang_Collecting_sales_representative,oAriaApplication.GetHeaderText("Lang_Collecting_sales_representative",AHEADERFILE)) + XSALESREP NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]

    * N000862 ,1 Thabet Handle globalization issues [Start]
    =lfFilArray()
    IF llMultCurr AND lcRpCurr = "F"
      lcCurrtCur =ccurrcode
    ENDIF
    SCAN WHILE repcode=XSALESREP FOR &lcexpM
      IF llMultCurr AND lcRpCurr = "F"  AND (lcCurrtCur<>ccurrcode)
        lcCurrtCur =ccurrcode
        RUNBAL=0
      ENDIF
      SCATTER MEMVAR
      =FILLTEMP()
    ENDSCAN
  ENDIF
ENDSCAN

*B609642,1 WAM 07/12/2011 Change sorting of REPCOMM file so that the payment comes at the end of the period
*!*	IF llMultCurr
*!*	  SELECT REPCOMM
*!*	  CLOSE INDEXES
*!*	  ERASE(gcWorkDir+lcRepComm+'.CDX')
*!*	ENDIF
SELECT REPCOMM
CLOSE INDEXES
ERASE(gcWorkDir+lcRepComm+'.CDX')
*B609642,1 WAM 07/12/2011 (End)

SELECT (lcWorkfile )
IF !RECCOUNT()>0
  llDonprnt=.T.
  *-- Message : There are no records to display...!
  *--                < Ok >
    =gfModalGen('TRM00052B40011','ALERT')
  RETURN
ENDIF
GOTO TOP
COPY REST TO oAriaApplication.WorkDir + lcTempFile+ ".DBF"
*E303286,1 MMT 10/29/2012 Change commission statement report to print the amount foreign currency[Start]
*USE oAriaApplication.WorkDir + lcTempFile+ ".DBF" IN 0 SHARED
USE oAriaApplication.WorkDir + lcTempFile+ ".DBF" IN 0 EXCL
*E303286,1 MMT 10/29/2012 Change commission statement report to print the amount foreign currency[End]
SELECT (lcTempFile)
INDEX ON REPCODE+CCURRCODE+DTOC(DATE) TAG (lcTempFile)
=lfAdjustCRSettings()
IF USED(lcTempFile)
  USE IN (lcTempFile)
ENDIF

IF USED(lcLogotmp)
    USE IN (lcLogotmp)
ENDIF

=gfDispRe()
ELSE
  IF llDonprnt
    *-- Message : There are no records to display...!
    *--                < Ok >
    =gfModalGen('TRM00052B40011','ALERT')
    RETURN
  ELSE
    =gfDispRe()
  ENDIF
ENDIF  &&FILTER CHANGE

*------- Functions section ---------
*----------------------------------
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/13/1998
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : lfAdrShift
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen

lnDatePos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'REPCOMM.DATE'),1)
lnPayDatePos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'REPCOMM.PAYDATE'),1)

SELECT SALESREP
SET ORDER TO
LOCATE FOR REPCODE = '' AND PAY_TYPE = 'M'
IF FOUND()
  IF EMPTY(laOGFxFlt[lnPayDatePos,6])
    laOGFxFlt[lnPayDatePos,6] = DTOC(DATE()) + '|' + DTOC(DATE())
  ENDIF
ELSE
  = lfFltState(lnPayDatePos,'D')  && Disable Pay Date.
ENDIF
SET ORDER TO SALESREP IN SALESREP  && To use it to validate REP# in option grid.



IF llMultCurr
  lnCurrPos  = lfItmPos('REPCOMM.CCURRCODE')
ELSE
  lcRpCurr = "O"
ENDIF

*-- End of lfwRepWhen.
*!*************************************************************
*! Name      : lfwOldVal
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/13/1998
*! Purpose   : When function to get the Old value
*!*************************************************************
*! Called from : Some of the Option Grid fields
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwOldVal()
*!*************************************************************
FUNCTION lfwOldVal
laOldVal = EVALUATE(SYS(18))      && Varible to hold the old value
*-- end of lfwOldVal.


*!*************************************************************
*! Name      : lfFltState
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/15/98
*! Purpose   : Enable and disable selected objects.
*!*************************************************************
*! Called from : lfwRepWhen
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : lfOGShowGet
*!*************************************************************
*! Passed Parameters  : 1- Filter Object Number
*!                    : 2- 'E' for enable, 'D' for disable
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfFltState()
*!*************************************************************
FUNCTION lfFltState
PARAMETERS lnObjNum,lcObjState
PRIVATE lcFxFltPos,lnPos

IF lcObjState = 'D' AND !EMPTY(laOGFxFlt[lnObjNum,6])
  laOGFxFlt[lnObjNum,6] = ''
ENDIF

lcFxFltPos = 'laOGFxFlt[' + ALLTRIM(STR(lnObjNum)) + ',6]'
lnPos = ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,lcFxFltPos),1)

laOGObjCnt[lnPos] = (lcObjState = 'E')
= lfOGShowGet(lcFxFltPos)  && Enable / Disable Object .
*-- end of lfFltState.

*!*************************************************************
*! Name      : lfAdrShift
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 07/15/1998
*! Purpose   : Function to Shift the Address array if there is any
*!             empty lines in the address
*!*************************************************************
*! Called from : lfwRepWhen
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : Array name
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfAdrShift()
*!*************************************************************
FUNCTION lfAdrShift
PARAMETERS lcArrayNam
FOR lnCount = 1 TO ALEN(&lcArrayNam)
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
     EMPTY(&lcArrayNam.[lnCount])

    =ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

FOR lnCount = 1 TO ALEN(&lcArrayNam)
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop
*-- end of lfAdrShift.


*!*************************************************************
*! Name      : lfItmPos
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 07/01/2000
*! Purpose   : Evaluate fixed filter position within array.
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*! Returns            : Position
*!*************************************************************
*! Example   : = lfItmPos()
*!*************************************************************
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos

*-- End Of lfItmPos.
*!*************************************************************
*! Name      : lfBaseAmt
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 07/01/2000
*! Purpose   : Compute base amount
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*! Returns            : Position
*!*************************************************************
*! Example   : = lfBaseAmt()
*!*************************************************************
FUNCTION lfBaseAmt
PARAMETERS lnAmntCurr
PRIVATE lnBaseAmt

lnBaseAmt = lnAmntCurr
*-- if Multi currency and user want to print in base currency and
*-- currency not the base currency.
IF llMultCurr AND lcRpCurr <> "F" AND lnBaseAmt <> 0
  lnBaseAmt = gfAmntDisp(lnBaseAmt,lcRpCurr,ldRpExDate,lcRpTmpNam)
ENDIF
RETURN lnBaseAmt

*-- End of lfBaseAmt.

*!*************************************************************
*! Name      : lfBgBlanc
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 07/01/2000
*! Purpose   : Accumlate the Beginning Balane for evry sales rep
*!           : Deff. Currency.
*!*************************************************************
*! Passed Parameters  : Non.
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : = lfBgBlanc()
*!*************************************************************
FUNCTION lfBgBlanc
IF llMultCurr
  IF  lcRpCurr = "F"
    lcSqlStat=" SELECT MIN(REPCODE)AS REPCODE,MIN(CCURRCODE) AS CCURRCODE,SUM(IIF(nforamnt = 0 , AMOUNT , lfBaseAmt(nforamnt))) AS AMNTS "
    *B609554,1 TMI 03/22/2011 [Start]  allow the created cursor to be readwrite
    *lcSqlStat=lcSqlStat+" FROM REPCOMM GROUP BY REPCOMM.repcode,REPCOMM.ccurrcode " + IIF(!EMPTY(LDATE)," WHERE  DATE<CTOD('"+LDATE+"')"," WHERE .F.  ")+"  INTO CURSOR "+ lcBgBAL
    lcSqlStat=lcSqlStat+" FROM REPCOMM GROUP BY REPCOMM.repcode,REPCOMM.ccurrcode " + IIF(!EMPTY(LDATE)," WHERE  DATE<CTOD('"+LDATE+"')"," WHERE .F.  ")+"  INTO CURSOR "+ lcBgBAL +' READWRITE'
    *B609554,1 TMI 03/22/2011 [End  ]
  ELSE
    lcSqlStat=" SELECT MIN(REPCODE)AS REPCODE,SUM(IIF(nforamnt = 0 , AMOUNT , lfBaseAmt(nforamnt))) AS AMNTS "
    *B609554,1 TMI 03/22/2011 [Start]  allow the created cursor to be readwrite
    *lcSqlStat=lcSqlStat+" FROM REPCOMM GROUP BY REPCOMM.repcode " + IIF(!EMPTY(LDATE)," WHERE  DATE<CTOD('"+LDATE+"')"," WHERE .F. ")+"  INTO CURSOR "+ lcBgBAL
    lcSqlStat=lcSqlStat+" FROM REPCOMM GROUP BY REPCOMM.repcode " + IIF(!EMPTY(LDATE)," WHERE  DATE<CTOD('"+LDATE+"')"," WHERE .F. ")+"  INTO CURSOR "+ lcBgBAL   +' READWRITE'
    *B609554,1 TMI 03/22/2011 [End  ]
  ENDIF
ELSE
  lcSqlStat=" SELECT MIN(REPCODE)AS REPCODE,SUM(AMOUNT ) AS AMNTS "
  *B609554,1 TMI 03/22/2011 [Start] allow the created cursor to be readwrite
  *lcSqlStat=lcSqlStat+" FROM REPCOMM GROUP BY REPCOMM.repcode  " +  IIF(!EMPTY(LDATE)," WHERE  DATE<CTOD('"+LDATE+"')"," WHERE .F. ")+"  INTO CURSOR "+ lcBgBAL
  lcSqlStat=lcSqlStat+" FROM REPCOMM GROUP BY REPCOMM.repcode  " +  IIF(!EMPTY(LDATE)," WHERE  DATE<CTOD('"+LDATE+"')"," WHERE .F. ")+"  INTO CURSOR "+ lcBgBAL   +' READWRITE'
  *B609554,1 TMI 03/22/2011 [End  ]
ENDIF
&lcSqlStat.
SELECT (lcBgBAL)
=CURSORSETPROP("Buffering" ,3)
IF llMultCurr AND lcRpCurr = "F"
  INDEX ON REPCODE+CCURRCODE TAG (lcBgBAL)
ELSE
  INDEX ON REPCODE TAG (lcBgBAL)
ENDIF
*-- End Of lfBgBlanc.
*************************************************************
*! Name      : lfAdjustCRSettings
*! Developer : AYMAN MAHMOUD AHMED (SMM)
*! Date      : 06/26/2006
*! Purpose   : To set the report data files and parameters
*!*************************************************************
FUNCTION lfAdjustCRSettings

DIMENSION loOgScroll.laCRTables[2]
*B610054,1 MMT 08/23/2012 Cannot export Sales rep. Commission Statement report to PDF[Start]
*DIMENSION loOgScroll.laCRParams[10,2]
*E303286,1 MMT 10/29/2012 Change commission statement report to print the amount foreign currency[Start]
*DIMENSION loOgScroll.laCRParams[11,2]
DIMENSION loOgScroll.laCRParams[12,2]
*E303286,1 MMT 10/29/2012 Change commission statement report to print the amount foreign currency[End]
*B610054,1 MMT 08/23/2012 Cannot export Sales rep. Commission Statement report to PDF[End]
loOgScroll.lcOGLastForm ='SRREPCOM'
loOGScroll.cCROrientation='P'
loOgScroll.laCRTables[1] = oAriaApplication.WorkDir +  lcTempFile+ ".DBF"
loOgScroll.laCRTables[2] = oAriaApplication.WorkDir +  lcLogotmp+ ".DBF"


loOgScroll.laCRParams[1,1] = 'ReportName'
*N000682,1 MMT 02/11/2013 Globalization changes[Start]
*loOgScroll.laCRParams[1,2]= 'C o m m i s s i o n s  S t a t e m e n t'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loOgScroll.laCRParams[1,2]= Lang_REPORTTTL
loOgScroll.laCRParams[1,2]= IIF(oAriaApplication.oActivelang.cLang_ID = "EN",Lang_REPORTTTL,oAriaApplication.GetHeaderText("Lang_REPORTTTL",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 MMT 02/11/2013 Globalization changes[End]
loOgScroll.laCRParams[2,1] = 'TUDATE'
loOgScroll.laCRParams[2,2]= IIF(EMPTY(LDATE),'  /  /',DTOC(CTOD(LDATE)-1))

loOgScroll.laCRParams[3,1] = 'HDATE'
loOgScroll.laCRParams[3,2]= IIF(EMPTY(HDATE),'  /  /  ',DTOC(CTOD(HDATE)))

loOgScroll.laCRParams[4,1] = 'DECIMAL'
loOgScroll.laCRParams[4,2]= IIF(lcRpDeciml='Y','Y','N')

loOgScroll.laCRParams[5,1] = 'COMP1'
loOgScroll.laCRParams[5,2]= laCompAdd[1]

loOgScroll.laCRParams[6,1] = 'COMP2'
loOgScroll.laCRParams[6,2]= laCompAdd[2]

loOgScroll.laCRParams[7,1] = 'COMP3'
loOgScroll.laCRParams[7,2]= laCompAdd[3]

loOgScroll.laCRParams[8,1] = 'COMP4'
loOgScroll.laCRParams[8,2]= laCompAdd[4]

loOgScroll.laCRParams[9,1] = 'COMP5'
loOgScroll.laCRParams[9,2]= laCompAdd[5]

loOgScroll.laCRParams[10,1] = 'LCLOGO'
loOgScroll.laCRParams[10,2]= loogscroll.lclogopath

*B610054,1 MMT 08/23/2012 Cannot export Sales rep. Commission Statement report to PDF[Start]
loOgScroll.laCRParams[11,1] = 'HASLOGO'
loOgScroll.laCRParams[11,2]= IIF(TYPE("loogscroll.lcLogoPath") = 'C' .AND. !EMPTY(loogscroll.lcLogoPath) .AND. !ISNULL(loogscroll.lcLogoPath) .AND.  (UPPER(RIGHT(loogscroll.lcLogoPath,3)) == 'BMP') and ;
loogscroll.FileExist(loogscroll.lcLogoPath) .AND. loogscroll.llShowLogo,'Y','N')
*B610054,1 MMT 08/23/2012 Cannot export Sales rep. Commission Statement report to PDF[END]
*E303286,1 MMT 10/29/2012 Change commission statement report to print the amount foreign currency[Start]
loOgScroll.laCRParams[12,1] = 'ShowFrgnCol'
loOgScroll.laCRParams[12,2]= IIF(llMultCurr and lcRpCurr <> 'F','Y','N')
*E303286,1 MMT 10/29/2012 Change commission statement report to print the amount foreign currency[End]
*************************************************************
*! Name      : lfBuildTmp
*! Developer : AYMAN MAHMOUD AHMED (AYM)
*! Date      : 05/30/2006
*! Purpose   :
*!*************************************************************
FUNCTION lfBuildTmp

DIMENSION laTempStru[27,18] ,laTempREP[1,18],laTempCOM[1,18]
PRIVATE lnFileCnt , lnFldRow
STORE '' TO laTempStru,laTempREP,laTempCOM
lcExcStat = SET('EXACT')
SET EXACT ON
SELECT REPCOMM
=OGAFIELDS(@laTempCOM)
laTempStru[1,1]  = 'REPCODE'
laTempStru[2,1]  = 'ACCOUNT'
laTempStru[3,1]  = 'CUSTPO'
laTempStru[4,1]  = 'DATE'
laTempStru[5,1]  = 'STATUS '
laTempStru[6,1]  = 'ORDER'
laTempStru[7,1]  = 'AMOUNT'
laTempStru[8,1]  = 'BALANCE'
laTempStru[9,1]  = 'DESC'
laTempStru[10,1] = 'PAYDATE'
laTempStru[11,1] = 'STORE'
laTempStru[12,1] = 'TRAN'
laTempStru[13,1] = 'TRANTYPE'
laTempStru[14,1] = 'CCURRCODE'
*-- Loop to get other dimensions of transaction included fields (Like master file)
FOR lnFileCnt = 1 TO 14
  lnFldRow = ASCAN(laTempCOM,laTempStru[lnFileCnt,1])
  IF lnFldRow > 0
    lnFldRow = ASUBSCRIPT(laTempCOM,lnFldRow,1)
    laTempStru[lnFileCnt , 2 ] = laTempCOM[lnFldRow , 2 ]
    laTempStru[lnFileCnt , 3 ] = laTempCOM[lnFldRow , 3 ]
    laTempStru[lnFileCnt , 4 ] = laTempCOM[lnFldRow , 4 ]
  ENDIF
ENDFOR  && end Loop to get other dimensions of transaction included fields (Like master file)
SELECT SALESREP
=OGAFIELDS(@laTempREP)
laTempStru[15,1]  = 'NAME'
laTempStru[16,1]  = 'PAY_TYPE'
laTempStru[17,1]  = 'PHONE'
laTempStru[18,1]  = 'FAX'
laTempStru[19,1]  = 'SSN'
laTempStru[20,1]  = 'CADDRESS1'
laTempStru[21,1]  = 'CADDRESS2'
laTempStru[22,1]  = 'CADDRESS3'
laTempStru[23,1]  = 'CADDRESS4'
laTempStru[24,1]  = 'CADDRESS5'
laTempStru[25,1]  = 'CADDRESS6'

FOR lnFileCnt = 15 TO 25
  lnFldRow = ASCAN(laTempREP,laTempStru[lnFileCnt,1])
  IF lnFldRow > 0
    lnFldRow = ASUBSCRIPT(laTempCOM,lnFldRow,1)
    laTempStru[lnFileCnt , 2 ] = laTempREP[lnFldRow , 2 ]
    laTempStru[lnFileCnt , 3 ] = laTempREP[lnFldRow , 3 ]
    laTempStru[lnFileCnt , 4 ] = laTempREP[lnFldRow , 4 ]
  ENDIF
ENDFOR
laTempStru[26,1] = 'BGBALANCE'
laTempStru[26,2] = 'N'
laTempStru[26,3] = 10
laTempStru[26,4] = 2
laTempStru[27,1] = 'CUSTOMER'
laTempStru[27,2] = 'C'
laTempStru[27,3] = 50
laTempStru[27,4] = 0
*E303286,1 MMT 10/29/2012 Change commission statement report to print the amount foreign currency[Start]
DIMENSION laTempStru[30,18]
laTempStru[28,1] = 'NFORAMNT'
laTempStru[28,2] = 'N'
laTempStru[28,3] = 13
laTempStru[28,4] = 2
laTempStru[29,1] = 'CURSYMB'
laTempStru[29,2] = 'C'
laTempStru[29,3] = 3
laTempStru[29,4] = 0
laTempStru[30,1] = 'CurrOrnt'
laTempStru[30,2] = 'C'
laTempStru[30,3] = 1
laTempStru[30,4] = 0
FOR lnCount = 28 TO 30
  STORE '' TO laTempStru[lnCount,7],laTempStru[lnCount,8],laTempStru[lnCount,9],;
              laTempStru[lnCount,10],laTempStru[lnCount,11],laTempStru[lnCount,12],;
              laTempStru[lnCount,13],laTempStru[lnCount,14],laTempStru[lnCount,15],;
              laTempStru[lnCount,16]
  STORE 0 TO  laTempStru[lnCount,17],laTempStru[lnCount,18]
ENDFOR
*E303286,1 MMT 10/29/2012 Change commission statement report to print the amount foreign currency[End]
=gfCrtTmp(lcWorkfile ,@laTempstru,,"",.f.)
SET EXACT &lcExcStat


lcPath=oAriaApplication.WorkDir +  lcLogotmp+ ".DBF"

CREATE TABLE (lcPath) (ST C(8),mygenfield G)
SELECT (lcLogotmp)
APPEND BLANK
REPLACE ST  WITH 'LOGO'
IF TYPE("loogscroll.lcLogoPath") = 'C' .AND. !EMPTY(loogscroll.lcLogoPath) .AND. !ISNULL(loogscroll.lcLogoPath) .AND.  (UPPER(RIGHT(loogscroll.lcLogoPath,3)) == 'BMP')
  IF loogscroll.FileExist(loogscroll.lcLogoPath) .AND. loogscroll.llShowLogo
      APPEND GENERAL mygenfield FROM (loogscroll.lcLogoPath)
  ENDIF
ENDIF




*************************************************************
*! Name      : lfCheckFilter
*! Developer : Saeed Mohammed (SMM)
*! Date      : 09/07/2004
*! Purpose   : Check if the filter was selected
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

*************************************************************
*! Name      : FILLTEMP
*! Developer : AYMAN MAHMOUD (AYM)
*! Date      : 06/25/2006
*! Purpose   : FILL TEMPRORY FILE
*!*************************************************************
FUNCTION FILLTEMP

IF llMultCurr
  lnAmount = IIF(nforamnt = 0 , AMOUNT , lfBaseAmt(nforamnt))
  RUNBAL = RUNBAL + IIF(AMOUNT > 0 , lnAmount , IIF(lnAmount < 0 , lnAmount , lnAmount*-1))
  M.BALANCE= RUNBAL
  M.CCURRCODE=IIF(lcRpCurr = "F" ,CCURRCODE,gcBaseCurr)

  *: B608695,1 MMT 09/23/2008 Fix bug of wrong amount in Mutli currency case [Start]
  M.AMOUNT= lnAmount
  *: B608695,1 MMT 09/23/2008 Fix bug of wrong amount in Mutli currency case [End]
  *E303286,1 MMT 10/29/2012 Change commission statement report to print the amount foreign currency[Start]
  m.CURSYMB = gfGetCurSmbl(ccurrcode)
  m.NFORAMNT = nforamnt
  m.CurrOrnt = IIF(lcCurrPost ='LEFT','L','R')
  *E303286,1 MMT 10/29/2012 Change commission statement report to print the amount foreign currency[End]
ELSE
  M.AMOUNT= AMOUNT
  RUNBAL = RUNBAL + AMOUNT
  M.BALANCE= RUNBAL
ENDIF
m.BGBALANCE=lnBgbal
m.DATE= DATE
m.TRAN= TRAN
m.DESC= PADR(DESC,18)
M.ORDER= ORDER
M.CUSTPO=PADR(CUSTPO,13)
M.NAME=&lcTmpRep..NAME
M.PAY_TYPE=&lcTmpRep..PAY_TYPE
M.PHONE=&lcTmpRep..PHONE
M.FAX=&lcTmpRep..FAX
M.SSN=&lcTmpRep..SSN
M.STORE='LOGO'
M.CADDRESS1=ALLTRIM(laRepData[1])
M.CADDRESS2=ALLTRIM(laRepData[2])
M.CADDRESS3=ALLTRIM(laRepData[3])
M.CADDRESS4=ALLTRIM(laRepData[4])


IF !EMPTY(ACCOUNT) .AND. SEEK('M'+REPCOMM.ACCOUNT,'CUSTOMER')
 M.CUSTOMER= CUSTOMER.BTNAME
ENDIF
INSERT INTO (lcWorkfile ) FROM MEMVAR


*************************************************************
*! Name      : lfFilArray
*! Developer : AYMAN MAHMOUD (AYM)
*! Date      : 06/25/2006
*! Purpose   : FILL TEMPRORY ARRAY HOLDS REP ADDRESSES
*!*************************************************************
FUNCTION lfFilArray

*!*  laRepData[1]=&lcTmpRep..CADDRESS1
*!*  laRepData[2]=&lcTmpRep..CADDRESS2
*!*  laRepData[3]=&lcTmpRep..CADDRESS3
*!*  laRepData[4]=&lcTmpRep..CADDRESS4
*!*  laRepData[5]=&lcTmpRep..CADDRESS5
*!*  laRepData[6]=&lcTmpRep..CADDRESS6
*!*  = lfAdrShift('laRepData')
lcAlias=ALIAS()

SELECT SALESREP

*-- Evaluate sales rep. address [begin]
laRepData[1] = CADDRESS1
laRepData[2] = CADDRESS2
laRepData[3] = gfGetAdr('SALESREP' , '' , '' , '' , 4) + ' --- ' +    gfGetAdr('SALESREP' , '' , '' , '' , 3)
laRepData[4] = IIF(EMPTY(Phone),'',TRANSFORM(Phone ,'@R '+ gfPhoneTem()))

= lfAdrShift('laRepData')    && Shift SALESREP address if there is empty line.
*-- Evaluate sales rep. address [end]
SELECT &lcAlias

*************************************************************
*! Name      : lfFilcomp
*! Developer : AYMAN MAHMOUD (AYM)
*! Date      : 06/25/2006
*! Purpose   : FILL company ADDRESSES
*!*************************************************************
FUNCTION lfFilcomp
IF !USED('SYCCOMP')
    USE (gcSysHome+'SYCCOMP') ORDER TAG Ccomp_id IN 0 SHARED
ENDIF

SELECT SYCCOMP
SEEK oariaapplication.activecompanyid
laCompAdd[1] = cCom_Name
laCompAdd[2] = gfGetAdr('SYCCOMP' , '' , '' , '' , 1)
laCompAdd[3] = gfGetAdr('SYCCOMP' , '' , '' , '' , 2)
laCompAdd[4] = gfGetAdr('SYCCOMP' , '' , '' , '' , 4) + ' --- ' +  gfGetAdr('SYCCOMP' , '' , '' , '' , 3)
laCompAdd[5] = IIF(EMPTY(cCom_Phon),"",TRANSFORM(cCom_Phon ,'@R'+ gfPhoneTem()))
= lfAdrShift('laCompAdd')
*E303286,1 MMT 10/29/2012 Change commission statement report to print the amount foreign currency[Start]
IF llMultCurr
  lcWorkArea = SELECT()
  PRIVATE  lcSqlCommand , lnResult
  lcSelectCommand = [SELECT cCurrency, cCurrencyI FROM SYCINT WHERE ccont_code = '] + SycComp.cCont_Code + [']
  lnRemoteResult = loOGScroll.SQLExecute("SYCINT", lcSelectCommand,"","SYCINT","",;
     oAriaApplication.SystemConnectionString,3,"")
  IF lnRemoteResult >= 1
    SELECT SYCINT
    LOCATE
    IF FOUND()
      lcCurrPost = SycInt.cCurrency
    ENDIF
  ENDIF
  SELECT(lcWorkArea)
ENDIF
*E303286,1 MMT 10/29/2012 Change commission statement report to print the amount foreign currency[End]
