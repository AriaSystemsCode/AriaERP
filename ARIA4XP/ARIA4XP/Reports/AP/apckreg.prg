*:***************************************************************************
*: Program file  : apckreg.prg
*: Program desc. : Check register report
*: System        : Aria Advantage Series.(Aria4XP)
*: Module        : Accounts Payable (AP)
*: Developer     : Mariam Mazhar(MMT)
*! Date          : 03/16/2011
*! Entry No.     : 037482 - [T20101213.0029]
*!***************************************************************************
*: Modifications:
*: B609975,1 MMT 06/25/2012 incorrect records selcted from APDIST file[T20120607.0033]
*********************************************************************************************
#INCLUDE R:\Aria4xp\reports\ap\apckreg.H
loogScroll.cCROrientation = 'P'
IF llOGFltCh
  lcOrderApp = ""
  IF SUBSTR(UPPER(lcrpgrp),1,8) = 'CBNKCODE'
    lcOrderApp  = "CBNKCODE+CCHKACCT+CPAYTYPE+CPAYMETH+CPAYDOCNO"
  ELSE
    lcOrderApp  = "CPAYTYPE+CPAYCLNO"
  ENDIF
  IF !USED('APPAYMNT')
    =gfOpenTable('APPAYMNT','TYPCLNO')
  ENDIF
  SELECT APPAYMNT
  DIMENSION laAppStru[1,18]
  lnAppCnt = AFIELDS(laAppStru)
  DIMENSION laAppStru[lnAppCnt +8,18]
  laAppStru[lnAppCnt +1,1]= 'capsessno'
	laAppStru[lnAppCnt +1,2]= 'C'
	laAppStru[lnAppCnt +1,3]= 8
	laAppStru[lnAppCnt +1,4]= 0

	laAppStru[lnAppCnt +2,1]= 'napdamnt'
	laAppStru[lnAppCnt +2,2]= 'N'
	laAppStru[lnAppCnt +2,3]= 15
	laAppStru[lnAppCnt +2,4]= 2

	laAppStru[lnAppCnt +3,1]= 'cchkglacc'
	laAppStru[lnAppCnt +3,2]= 'C'
	laAppStru[lnAppCnt +3,3]= 24
	laAppStru[lnAppCnt +3,4]= 0


	laAppStru[lnAppCnt +4,1]= 'cinvno'
	laAppStru[lnAppCnt +4,2]= 'C'
	laAppStru[lnAppCnt +4,3]= 12
	laAppStru[lnAppCnt +4,4]= 0


	laAppStru[lnAppCnt +5,1]= 'cinvref'
	laAppStru[lnAppCnt +5,2]= 'C'
	laAppStru[lnAppCnt +5,3]= 15
	laAppStru[lnAppCnt +5,4]= 0

	laAppStru[lnAppCnt +6,1]= 'cInvStat'
	laAppStru[lnAppCnt +6,2]= 'C'
	laAppStru[lnAppCnt +6,3]= 1
	laAppStru[lnAppCnt +6,4]= 0


	laAppStru[lnAppCnt +6,1]= 'cInvStat'
	laAppStru[lnAppCnt +6,2]= 'C'
	laAppStru[lnAppCnt +6,3]= 1
	laAppStru[lnAppCnt +6,4]= 0

	laAppStru[lnAppCnt +7,1]= 'DISCCURR'
	laAppStru[lnAppCnt +7,2]= 'C'
	laAppStru[lnAppCnt +7,3]= 3
	laAppStru[lnAppCnt +7,4]= 0

	laAppStru[lnAppCnt +8,1]= 'nDISRate'
	laAppStru[lnAppCnt +8,2]= 'N'
	laAppStru[lnAppCnt +8,3]= 9
	laAppStru[lnAppCnt +8,4]= 4


	FOR lnLoop = 1 to  8
	  STORE ' ' TO  laAppStru[lnAppCnt +lnLoop,7],laAppStru[lnAppCnt+lnLoop,8],;
	          laAppStru[lnAppCnt+lnLoop,9],laAppStru[lnAppCnt+lnLoop,10],;
	          laAppStru[lnAppCnt+lnLoop,11],laAppStru[lnAppCnt+lnLoop,12],;
	          laAppStru[lnAppCnt+lnLoop,13],laAppStru[lnAppCnt+lnLoop,14],;
	          laAppStru[lnAppCnt+lnLoop,15],laAppStru[lnAppCnt+lnLoop,16]
	  STORE 0 TO    laAppStru[lnAppCnt+lnLoop,17] ,laAppStru[lnAppCnt+lnLoop,18]
	ENDFOR

	=gfCrtTmp(lcAPPTmp,@laAppStru,lcOrderApp,lcAPPTmp,.T.)

	llPayNo = .F.
	lcPaySel = ''
	lnPosPay = ASCAN(loOgScroll.laOgFXFlt,"lcTrnNo")
	IF lnPosPay > 0 
	  lnPosPay= ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosPay,1)
	  lcPaySel =IIF(!EMPTY(loOgScroll.laOgFxFlt[lnPosPay,6]),loOgScroll.laOgFxFlt[lnPosPay,6],'')
	  IF !EMPTY(lcPaySel) AND USED(lcPaySel)
	    SELECT(lcPaySel)
	    LOCATE
	    IF !EOF()
	      llPayNo = .T.
	    ENDIF
	  ENDIF  
	ENDIF 



	llVendor = .F.
	lcVenSel = ''
	lnPosVen = ASCAN(loOgScroll.laOgFXFlt,"APPAYMNT.CPAYCLNO")
	IF lnPosVen> 0 
	  lnPosVen= ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosVen,1)
	  lcVenSel=IIF(!EMPTY(loOgScroll.laOgFxFlt[lnPosVen,6]),loOgScroll.laOgFxFlt[lnPosVen,6],'')
	  IF !EMPTY(lcVenSel) AND USED(lcVenSel)
	    SELECT(lcVenSel)
	    LOCATE
	    IF !EOF()
	      llVendor= .T.
	    ENDIF
	  ENDIF  
	ENDIF 

	llPayDate = .F.
	ldSPDate = {}
	ldEPDate = {}
	lnPosPyDate = ASCAN(loOgScroll.laOgFXFlt,"APPAYMNT.DPAYDATE")
	IF lnPosPyDate> 0 
	  lnPosPyDate  =  ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosPyDate,1)
	  lcPayDate =loOGScroll.laOgFxFlt[lnPosPyDate,6]
	  IF !EMPTY(lcPayDate)
	    ldSPDate = IIF(EMPTY(SUBSTR(loOGScroll.laOGFxFlt[lnPosPyDate,6],1,10)),CTOD(""),CTOD(SUBSTR(loOGScroll.laOGFxFlt[lnPosPyDate,6],1,10)))
	    ldEPDate = IIF(EMPTY(SUBSTR(loOGScroll.laOGFxFlt[lnPosPyDate,6],12,21)),CTOD(""),CTOD(SUBSTR(loOGScroll.laOGFxFlt[lnPosPyDate,6],12,21)))
	    llPayDate = .T.
	  ENDIF  
	ENDIF


	SELECT APDIST
	=gfSetOrder('PAYMNTS')
	IF !USED('APINVHDR')
	  =gfOPenTable('APINVHDR','INVVEND')
	ELSE
	  SELECT APINVHDR
	  =gfSetOrder('INVVEND')
	ENDIF  
	IF !USED('APCHECKS')
	  =gfOPenTable('APCHECKS','BANKCHECK')
	ELSE
	  SELECT APCHECKS
	  =gfSetOrder('BANKCHECK')
	ENDIF  
	SELECT APPAYMNT
	=gfSetOrder('TYPCLNO')
	=gfSeek('P')
  WAIT WINDOW LANG_apckreg_COLLECT NOWAIT 
	SCAN REST WHILE CPAYTYPE+CPAYCLNO = "P" FOR IIF(!EMPTY(lcRpBnkCod),cbnkcode = lcRpBnkCod ,.T.) AND;
								  IIF(!EMPTY(lcRpChkAct),cchkacct = lcRpChkAct ,.T.) AND ;
								  IIF(EMPTY(lcRpVenPay),CPAYMETH $ "PMN",CPAYMETH $ lcRpVenPay) AND ;
								  IIF(llRpIncVPy , .T. , cPayStat <> 'V') AND IIF(llVendor ,SEEK(APPAYMNT.CPAYCLNO,lcVenSel),.T.) AND ;
								  IIF(llPayDate ,BETWEEN(APPAYMNT.DPAYDATE,ldSPDate,ldEPDate),.T.)
								  
		=gfSeek(appaymnt.cpaymeth+ appaymnt.cbnkcode+ appaymnt.cchkacct+ appaymnt.cpaydocno,'APDIST','PAYMNTS')
		=gfSeek(appaymnt.cbnkcode+ appaymnt.cchkacct ,'APCHECKS','BANKCHECK')
		
		SELECT APDIST
		*: B609975,1 MMT 06/25/2012 incorrect records selcted from APDIST file[T20120607.0033]
		*SCAN REST WHILE CAPDTRTYP+CBNKCODE+CCHKACCT+CAPDREF+CINVNO+CAPDACTID =;
		                appaymnt.cpaymeth+ appaymnt.cbnkcode+ appaymnt.cchkacct+ appaymnt.cpaydocno FOR;
		                IIF(llPayNo,SEEK(APDIST.cApdRef,lcPaySel),.T.)  AND APDIST.CAPDACTID = 'C' AND ;
		                APDIST.CAPDTRTYP $ "P|M|N"
		SCAN REST WHILE CAPDTRTYP+CBNKCODE+CCHKACCT+CAPDREF+CINVNO+CAPDACTID =;
		                appaymnt.cpaymeth+ appaymnt.cbnkcode+ appaymnt.cchkacct+ appaymnt.cpaydocno FOR;
		                IIF(llPayNo,SEEK(APDIST.cApdRef,lcPaySel),.T.)  AND APDIST.CAPDACTID = 'C' AND ;
		                APDIST.CAPDTRTYP $ "P|M|N" AND apdist.cvendcode = APPAYMNT.CPAYCLNO
        *: B609975,1 MMT 06/25/2012 incorrect records selcted from APDIST file[End]		                
	    =gfSeek(apdist.cinvno+ apdist.cvendcode,'APINVHDR','INVVEND')							  							   
	    IF IIF(APPAYMNT.cPayStat = 'V' , IIF(APINVHDR.nInvAmnt > 0 .OR. APPAYMNT.lPayAdvan , APDIST.nApdAmnt , -APDIST.nApdAmnt) < 0 , .T.)
	      SELECT APPAYMNT
	      SCATTER MEMO MEMVAR 
	      m.capsessno =APDIST.capsessno
	      m.napdamnt = APDIST.nApdAmnt
	      m.cchkglacc =APCHECKS.cchkglacc
	      m.cinvno = apinvhdr.cinvno
				m.cinvref = apinvhdr.cinvref
				m.cInvStat = APINVHDR.cInvStat
				m.DISCCURR =APDIST.CCURRCODE
				m.nDISRate= APDIST.nExRate
	      INSERT into (lcAPPTmp) FROM MEMVAR 
	    ENDIF  
	  ENDSCAN  
	ENDSCAN 
ENDIF
SELECT (lcAPPTmp)
LOCATE 
DO gfDispRe WITH EVAL('LCRPFORM')

*!**************************************************************************
*! Name      : lfRepShow
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 03/16/2011
*! Purpose   : Report Show 
*!**************************************************************************
FUNCTION lfRepShow
laOGObjCnt[3] = gfGetMemVar('LLMULCURR')
=lfOGShowGet("lnRepCurr")

*!**************************************************************************
*! Name      : lfvCurDisp
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 03/16/2011
*! Purpose   : Currency Disp. Validation 
*!**************************************************************************
FUNCTION lfvCurDisp
llRpProced = gfRepCur(.T., @lcRpCurr,@ldRpExDate,lcRpTmpNam)
*!**************************************************************************
*! Name      : lfvCurDisp
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 03/16/2011
*! Purpose   : This function is to save the array element number
*!             of the payment method
*!*************************************************************
*! Called from : The Option Grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
FUNCTION lfwOpGrid


DECLARE laRpSource[3],laRpTarget[1]
STORE LANG_apckreg_PRINTEDCHKS   TO laRpSource[1] 
STORE ''               TO laRpTarget[1]
STORE LANG_apckreg_MANUALCHKS    TO laRpSource[2] &&,laRpTarget[2]
STORE LANG_apckreg_NONCHKS TO laRpSource[3]
lcRpVenPay= "'P','M','N'"
*!*************************************************************
*! Name      : lfAmntDisp
*! Developer : Mariam Mazhar
*! Date      : 03/16/2011
*! Purpose   : To display the payment amount according to the
*!             currency selected.
*!*************************************************************
*! Called from : The Option Grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
FUNCTION lfAmntDisp
PARAMETER lnAmount,lcRpDispCur,ldExRateDt,lcTmepFile,llAprvCurr,lcGetFile

PRIVATE lnAmount,lcRpDispCur,ldExRateDt,lcTmepFil,llAprvCurr,lcExSin1,lcExSin2,lnSavAlias

lnAmount    = IIF(TYPE('lnAmount') = 'N',lnAmount,0)
lcRpDispCur = IIF(TYPE('lcRpDispCur') ='C',lcRpDispCur,'')
ldExRateDt  = IIF(TYPE('ldExRateDt') = 'D',ldExRateDt,{})
lcTmepFile  = IIF(TYPE('lcTmepFile') = 'C',lcTmepFile,'')
llAprvCurr  = IIF(TYPE('llAprvCurr') = 'L',llAprvCurr,.F.)

lcExSin1    = ''       && Variable to hold the first sign in the equation.
lcExSin2    = ''       && Variable to hold the second sign in the equation.

lnSavAlias  = SELECT(0)  && Variable to save the alias.
lcGetFile   = IIF(TYPE('lcGetFile')$"UL",'',lcGetFile)

DO CASE
  CASE lcRpDispCur = 'F'

  CASE lcRpDispCur = 'O'
    IF CCURRCODE = oAriaApplication.BaseCurrency
      SELECT (lnSavAlias)
      RETURN lnAmount  
    ENDIF
    IF EMPTY(lcGetFile)
      lcCurrCode = IIF(llAprvCurr,CAPRCURCOD,CCURRCODE)
    ELSE
      lcCurrCode = IIF(llAprvCurr,&lcGetFile..CAPRCURCOD,&lcGetFile..CCURRCODE)
    ENDIF  

    
    lcExSin2   = ' '
    lcExSin1   = gfGetExSin(@lcExSin2,lcCurrCode)
        
    
    lnExRate = 0

    IF EMPTY(lcGetFile)
      lnUnit = NCURRUNIT
      lnExRate = IIF(llAprvCurr , gfChkRate('lnUnit' , lcCurrCode , DINVDATE , .F.) , NEXRATE)
    ELSE
      lnUnit = &lcGetFile..NCURRUNIT
      lnExRate = IIF(llAprvCurr , gfChkRate('lnUnit' , lcCurrCode , &lcGetFile..DINVDATE , .F.) , &lcGetFile..NEXRATE)
    ENDIF         
  
   
    IF lcCurrCode = DISCCURR
   	  lnExRate  = nDISRate
    ENDIF
    
   
    
    lnExRate = IIF(lnExRate <> 0 , lnExRate , 1)    
    
    lnUnit = IIF(lnExRate <> 0 , lnUnit , 1)
    lnAmount   = ROUND(lnAmount &lcExSin1 lnExRate &lcExSin2 lnUnit , 2)

  CASE lcRpDispCur = 'D'
    lnExRate   = 0
    lnUnit     = 0
    
    IF EMPTY(lcGetFile)
      lcCurrCode = IIF(llAprvCurr,CAPRCURCOD,CCURRCODE)
    ELSE
      lcCurrCode = IIF(llAprvCurr,&lcGetFile..CAPRCURCOD,&lcGetFile..CCURRCODE)
    ENDIF
      
    IF lcCurrCode = oAriaApplication.BaseCurrency
      lnExRate = 1
      lnUnit   = 1
    ELSE
      lnExRate   = gfChkRate('lnUnit',lcCurrCode,ldExRateDt,.F.)
    ENDIF

    lnExRate = IIF(lnExRate <> 0 , lnExRate , 1)
    lnUnit = IIF(lnExRate <> 0 , lnUnit , 1)

    lcExSin2   = ' '
    lcExSin1   = gfGetExSin(@lcExSin2,lcCurrCode)
    lnAmount   = ROUND(lnAmount &lcExSin1 lnExRate &lcExSin2 lnUnit,2)

  CASE lcRpDispCur = 'U'

    lnExRate   = 0
    lnUnit     = 0
    
    IF EMPTY(lcGetFile)
      lcCurrCode = IIF(llAprvCurr,CAPRCURCOD,CCURRCODE)
    ELSE
      lcCurrCode = IIF(llAprvCurr,&lcGetFile..CAPRCURCOD,&lcGetFile..CCURRCODE)
    ENDIF  

    IF lcCurrCode = oAriaApplication.BaseCurrency
      lnExRate = 1
      lnUnit   = 1
    ELSE
      IF SEEK(lcCurrCode,lcTmepFile)
        lnExRate = &lcTmepFile..NEXRATE
        lnUnit   = &lcTmepFile..NCURRUNIT
      ENDIF
    ENDIF

    lnExRate = IIF(lnExRate <> 0 , lnExRate , 1)
    lnUnit = IIF(lnExRate <> 0 , lnUnit , 1)

    lcExSin2   = ' '
    lcExSin1   = gfGetExSin(@lcExSin2,lcCurrCode)
    lnAmount   = ROUND(lnAmount &lcExSin1 lnExRate &lcExSin2 lnUnit,2)
ENDCASE

SELECT (lnSavAlias)

RETURN lnAmount

*!*************************************************************
*! Name      : lfvClrRead
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 03/16/2011
*! Purpose   : ClearRead
*!*************************************************************
*! Called from : The Option Grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
FUNCTION lfvClrRead

CLEARREAD()
*!*************************************************************
*! Name      : RefreshStatus
*! Developer : MARIAM MAZHAR (MMT) 
*! Date      : 03/16/2011
*! Purpose   : Return the selected Payment Method in the ReadBox
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!***************************************************************************
*! Modification:
*!***************************************************************************
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
*!*************************************************************
*! Name      : lfvPayMeth
*! Developer : Mariam MAzhar
*! Date      : 03/16/2011
*! Purpose   : Validate Payment Method
*!*************************************************************
FUNCTION lfvPayMeth
PRIVATE lcOldStat,lcCurrChr

lcOldStat = lcRpVenPay
= lfOGMover(@laRpSource,@laRpTarget,LANG_apckreg_PayMSelect,.T.,'')  && call mover function.
lcRpVenPay = ' '
*-- Loop to make Status expression.
FOR lnI = 1 TO ALEN(laRpTarget,1)
  lcRpVenPay= lcRpVenPay+ IIF(laRpTarget[lnI] = LANG_apckreg_PRINTEDCHKS,"'P'",;
                            IIF(laRpTarget[lnI] = LANG_apckreg_MANUALCHKS,",'M'",;
                            IIF(laRpTarget[lnI] = LANG_apckreg_NONCHKS,",'N'",'')))

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
*! Name      : lfvBnkCode   
*! Developer : MARIAM MAZHAR (MMT) 
*! Date      :03/16/2011
*! Purpose   :Validate bank code
*!*************************************************************************
FUNCTION lfvBnkCode   

IF EMPTY(lcRpBnkCod)
  lcRpChkAct = ''
  RETURN 
ENDIF


lcControl = loOGScroll.FocusControl
lcCurVar  = loOGScroll.&lcControl.
lcOldVal = lcCurVar.oldvalue

IF !USED('APBANKS')
  =gfOpenTable('APBANKS','BANKCODE')
ENDIF

SELECT APBANKS
=gfSeek('')
IF '?' $ lcRpBnkCod .OR. !SEEK(lcRpBnkCod)
  DECLARE laRpRetFld(1)
  lcBrFields    = 'CBnkCode:H="'+LANG_apckreg_CODE+'",CBNKLNDES:H="'+LANG_apckreg_DESC+'"'
  laRpRetFld[1] = ''
  =gfBrows([],'CBnkCode',"laRpRetFld",LANG_apckreg_BNKCODE  ,.F.)
  IF EMPTY(laRpRetFld[1])  
    lcRpBnkCod = lcOldVal
  ELSE
    lcRpBnkCod = laRpRetFld[1]
  ENDIF
ENDIF
IF !USED('APCHECKS')
  =gfOpenTable('APCHECKS','BANKCHECK')
ENDIF
IF !EMPTY(lcRpBnkCod)  && in case of press cancel and Empty(lcOldVal)
  =gfSEEK(lcRpBnkCod,'APCHECKS')
  lcRpChkAct = APCHECKS.CCHKACCT
  IF !gfSEEK(lcRpBnkCod+lcRpChkAct,'APCHECKS')
    *** The bank has no checking accounts setup.
    =gfModalGen("INM04023B00000","DIALOG")
    lcRpBnkCod = lcOldVal
  ENDIF
ENDIF
        
*!*************************************************************
*! Name      : lfvChkAcct            
*! Developer : MARIAM MAZHAR (MMT) 
*! Date      : 03/16/2011
*! Purpose   : Validate Checking account
*!*************************************************************************
*  
FUNCTION lfvChkAcct            

IF !USED('APCHECKS')
  =gfOpenTable('APCHECKS','BANKCHECK')
ENDIF

lcControl = loOGScroll.FocusControl
lcCurVar  = loOGScroll.&lcControl.
lcOldVal = lcCurVar.oldvalue
SELECT APCHECKS
=gfSeek('')
IF '?' $ lcRpChkAct .OR.!SEEK(PADR(lcRpBnkCod,8)+PADR(lcRpChkAct,12))
  DECLARE laRpRetFld(1)
  lcBrFields    = 'CBnkCode:H="'+LANG_apckreg_OneBNKCODE+'",CChkAcct:H="'+LANG_apckreg_CHKACC +'"'
  laRpRetFld[1] = ''
  =gfBrows([lcRpBnkCod],'CChkAcct',"laRpRetFld",LANG_apckreg_BNKCHK ,.F.)
  IF EMPTY(laRpRetFld[1])  
    lcRpChkAct = lcOldVal
  ELSE
    lcRpChkAct = laRpRetFld[1]
    lcRpBnkCod = APCHECKS.CBnkCode
  ENDIF
ENDIF
*!*************************************************************
*! Name      : lfSRTRRN  
*! Developer : MARIAM MAZHAR (MMT) 
*! Date      : 03/16/2011
*! Purpose   : Set/Reset PayNo Browser
*!*************************************************************************
FUNCTION lfSRTRRN  
LPARAMETERS lcTrTyp
IF lcTrTyp = 'S'
  SELECT APDIST
  =gfSetOrder('Invvend')
ELSE
  SELECT APDIST
	=gfSetOrder('PAYMNTS')
ENDIF