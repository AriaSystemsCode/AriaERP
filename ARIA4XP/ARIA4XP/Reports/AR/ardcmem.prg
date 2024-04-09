*:**********************************************************************
*: Program file 		: ARDCMEM 
*: Program description	: Debit/Credit Memos Report
*: Module				: Accounts Receivable (AR)
*: Developer			: HEBA FATHI (HFK)
*: Tracking Job Number	: #037411
*: Date					: 03/02/2005
*:**********************************************************************		
*: Calls: 
*:      Programs		: 
*:      Screens			: 
*:      Global Function	:	gfModalGen,gfDispRe,gfGetMemVar,gfOpenFile,gfPhoneTem,gfGetAdr
*:**********************************************************************
*: Called From			: 
*:**********************************************************************
*: Passed Parameters	: None
*:**********************************************************************
*: Example				:	DO ARDCMEM
*:**************************************************************************
*: Modifcations:
*: B608427,1 WAM 02/11/2008 Show credit adjustment created from key-off screen
*:**********************************************************************
#INCLUDE R:\aria4xp\reports\ar\ardcmem.H
loogScroll.cCROrientation = 'P'

lcStTime   = TIME()    && Time in which we start collect data.
llEndGroup = .F.       && Flag detect that we finish group
lnTotAmt   = 0         && Amount Printed on Page footer [Dos Mode]
*-- if user change last filter .
*-- Use variable llOGFltCh that detect OG filter changes.[Begin]

IF llClearFn OR loOGScroll.llOGFltCh
  STORE .F. TO llChAcc,llChTrnTyp,llClearFn
  lcLastExpr = loOGScroll.lcRpExp

  *-- if you have previous data clear workfile then recreate it. [begin]
  IF !USED(lcWorkFile) OR (RECCOUNT(lcWorkFile) > 0)
    IF USED(lcWorkFile)
      USE IN (lcWorkFile)
    ENDIF
    =lfWorkFile()
  ENDIF
  *-- if you have previous data clear workfile then recreate it. [end]

  *-- Evaluate report expression [Begin]
  loOGScroll.lcRpExp = STRTRAN(loOGScroll.lcRpExp,'DEBIT.','')
  loOGScroll.lcRpExp = IIF(ALLTRIM(loOGScroll.lcRpExp) = '.T.','',loOGScroll.lcRpExp)  && Report Filter Expression.
  lcCBFilter = ''
  IF 'TRANDATE' $ loOGScroll.lcRpExp
	*!*	    IF 'ACCOUNT' $ loOGScroll.lcRpExp
	*!*		  lcDateExp   = SUBSTR(loOGScroll.lcRpExp,ATC('BETWEEN(DTOS(TRANDATE',loOGScroll.lcRpExp))
	*!*		  lnFinish    = IIF(ATC(' AND',lcDateExp)>0,ATC(' AND',lcDateExp),LEN(lcDateExp))
	*!*		  lcDateExpr  = SUBSTR(lcDateExp,1,lnFinish)
	*!*		  lcDateExpr  = STRTRAN(lcDateExpr,'TRANDATE','ChgBk_Date')
	*!*			*!*	      lnDateStPs  = ATC('BETWEEN(DTOS(TRANDATE',loOGScroll.lcRpExp)
	*!*			*!*	      lnDateLen   = ATC('})))',loOGScroll.lcRpExp) + 5 - lnDateStPs
	*!*			*!*	      lcDateExpr  = SUBSTR(loOGScroll.lcRpExp,lnDateStPs,lnDateLen)
	*!*			*!*	      lcDateExpr  = STRTRAN(lcDateExpr,'TRANDATE','ChgBk_Date')
	*!*	      IF !EMPTY(lcDateExpr)
	*!*	        lcCBFilter = loOGScroll.lcRpExp + [ AND ] + lcDateExpr + [ AND ]
	*!*	      ELSE
	*!*	        lcCBFilter = loOGScroll.lcRpExp + [ AND ]
	*!*	      ENDIF
	*!*	    ELSE
    lcCBFilter = STRTRAN(loOGScroll.lcRpExp,'TRANDATE','ChgBk_Date') + [ AND ]
	*!*	    ENDIF
	*!*	  ENDIF
	*!*	      *-hfk,03/02/2005, Add currency filter to the charge back filter
	*!*	      lnCurrFlt = ASCAN(loOGScroll.laOgFxFlt,'DEBIT.CCURRCODE')
	*!*	      lnCurrFlt = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnCurrFlt,1)
	*!*	      lcCurrFlt = loOGScroll.laOgFxFlt[lnCurrFlt,8]
	*!*	      IF !EMPTY(lcCurrFlt)
	*!*			lcCurrFlt = STRTRAN(lcCurrFlt,'DEBIT.','')
	*!*			lcChBkFilt = lcChBkFilt + ' AND ' + lcCurrFlt
  ELSE
	lcCBFilter = IIF(EMPTY(loOGScroll.lcRpExp),"",loOGScroll.lcRpExp+ [ AND ]) 
  ENDIF 

  *-- Evaluate report expression [End]

  *-- Collect report data [Begin]
  PRIVATE lcDebFilt,lcCreFilt,lcTranType,lcHistFilt
  STORE '' TO lcDebFilt,lcCreFilt,lcTranType,lcHistFilt
  *-- if user choice is to print charge back or all
  IF lcRpTrnTyp $ 'HA'
    USE (oAriaApplication.DataDir+'ARHIST') ORDER TAG Arhistht AGAIN ALIAS (lcHistFile) IN 0 SHARED
    lcChBkFilt = ''
    IF EMPTY(lcCBFilter)
      lcChBkFilt = [TRANTYPE $ '48']
    ELSE
      lcChBkFilt = "(" + lcCBFilter + " TRANTYPE='8') OR (" + loOGScroll.lcRpExp + " AND TRANTYPE='4')"
    ENDIF

    lcChBkFilt = [account+tran+cinstalno = '' AND ] + lcChBkFilt  && For Optimizing.
    SELECT ARHIST
    m.cTranArng = 'C'
    SCAN FOR &lcChBkFilt
      SCATTER MEMVAR MEMO
      IF (TRANTYPE = '8')
        INSERT INTO (lcWorkFile) FROM MEMVAR
      ELSE  && Transaction Type '4'
        IF SEEK(m.Account+m.History,lcHistFile)
          SELECT (lcHistFile)
          LOCATE REST WHILE account+history+trantype+tran+cinstalno = m.Account+m.History ;
                      FOR   TRANTYPE = '8'
          IF FOUND()
            INSERT INTO (lcWorkFile) FROM MEMVAR
          ENDIF
        ENDIF
      ENDIF
    ENDSCAN
    USE IN (lcHistFile)
  ENDIF  && end if user choice is to print charge back or all

  *-- if user choice is to print debit or all
  IF lcRpTrnTyp $ 'DA'
    lcTranType = '2'
    lcDebFilt  = IIF(EMPTY(loOGScroll.lcRpExp),[TRANTYPE='2'], loOGScroll.lcRpExp + [ AND TRANTYPE='2'])
    lcDebFilt  = [account+tran+cinstalno+DTOS(trandate) = '' AND ] + lcDebFilt
    m.cTranArng = 'A'
    SELECT DEBIT
    SCAN FOR &lcDebFilt
      SCATTER MEMVAR MEMO
      INSERT INTO (lcWorkFile) FROM MEMVAR
    ENDSCAN
  ENDIF  && end if user choice is to print debit or all

  *-- if user choice is to print credit or all
  IF lcRpTrnTyp $ 'CA'
*!*	        =ACOPY(loOGScroll.laOGHdFlt , lcRpHdflt )  && Array to hold the Hidden   filter array to select it back
*!*	        =ACOPY(loOGScroll.laOGFxFlt , lcRpFxflt )  && Array to hold the Fixed    filter array to select it back
*!*	        =ACOPY(loOGScroll.laOGVrFlt , lcRpVrflt )  && Array to hold the Variable filter array to select it back

*!*	        For lnCount =  1 TO ALEN(loOGScroll.laOGHdFlt,1)
*!*	            loOGScroll.laOGHdFlt[lnCount,1] = STRTRAN(loOGScroll.laOGHDFlt[lnCount,1],'DEBIT.','CREDIT.')
*!*		    ENDFOR

*!*	        For lnCount =  1 TO ALEN(loOGScroll.laOGFxFlt,1)
*!*	            loOGScroll.laOGFxFlt[lnCount,1] = STRTRAN(loOGScroll.LAOGFXFLT[lnCount,1],'DEBIT.','CREDIT.')
*!*	    	ENDFOR

*!*	        For lnCount =  1 TO ALEN(loOGScroll.laOGVrFlt,1)
*!*	            loOGScroll.laOGVrFlt[lnCount,1] = STRTRAN(loOGScroll.laOGVrFlt[lnCount,1],'DEBIT.','CREDIT.')
*!*		    ENDFOR

*!*	    	LOCAL lcOGHDFlt,lcOGFxFlt,lcOGVrFlt
*!*	        lcOGHDFlt = IIF(EMPTY(lcOGHDFlt),".T.",gfGenFlt('loOGScroll.laOGHdFlt' , llOGFilter , .T.))  && variable holf the conditoins in the laOgFdFkt
*!*	        lcOGFxFlt = IIF(EMPTY(lcOGFxFlt),".T.",gfGenFlt('loOGScroll.laOGFxFlt' , llOGFilter , .T.))  && variable holf the conditoins in the laOgFxFkt
*!*	        lcOGVrFlt = IIF(EMPTY(lcOGVrFlt),".T.",gfGenFlt('loOGScroll.laOGVrFlt' , llOGFilter , .T.))  && variable holf the conditoins in the laOgVrFkt

*!*		    loOGScroll.lcRpExp = "(" + lcOGHDFlt + ")" + ".AND." + "(" + lcOGFxFlt + ")" + ".AND." + "(" + lcOGVrFlt + ")"
*!*		    loOGScroll.lcRpExp    = STRTRAN(loOGScroll.lcRpExp,'CREDIT.','')
*!*			loOGScroll.lcRpExp    = STRTRAN(loOGScroll.lcRpExp,'DEBIT.','')

        *B608427,1 WAM 02/11/2008 Show credit adjustment created from key-off screen
	    *lcTranType = lcTranType + '5'
	    lcTranType = lcTranType + '57'
        *B608427,1 WAM 02/11/2008 (ENd)

	    lcCreFilt  = IIF(EMPTY(loOGScroll.lcRpExp),[TRANTYPE='2'], loOGScroll.lcRpExp + [ AND TRANTYPE='5'])
	    lcCreFilt  = [account+tran+DTOS(trandate) = '' AND ] + lcCreFilt
	    m.cTranArng = 'B'
	    SELECT CREDIT
	    SCAN FOR &lcCreFilt
	      SCATTER MEMVAR MEMO
	      INSERT INTO (lcWorkFile) FROM MEMVAR
	    ENDSCAN
*!*	        =ACOPY(lcRpHdflt , loOGScroll.laOGHdFlt )  && Array to hold the FIRST Hidden   filter array to select it back
*!*	        =ACOPY(lcRpFxflt , loOGScroll.laOGFxFlt )  && Array to hold the FIRST Fixed    filter array to select it back
*!*	        =ACOPY(lcRpVrflt , loOGScroll.laOGVrFlt )  && Array to hold the FIRST Variable filter array to select it back
  ENDIF  && end if user choice is to print credit or all
  *-- Collect report data [End  ]

  *-- Debit (and/or) credit history.
  IF !EMPTY(lcTranType)
    lcHistFilt = IIF(EMPTY(loOGScroll.lcRpExp),[TRANTYPE $ lcTranType],;
                    loOGScroll.lcRpExp + [ AND TRANTYPE $ lcTranType])
    lcHistFilt = [account+tran+cinstalno = '' AND ] + lcHistFilt
    SELECT ARHIST
    SCAN FOR &lcHistFilt
      m.cTranArng = IIF(TRANTYPE='2','A','B')
      SCATTER MEMVAR MEMO
      INSERT INTO (lcWorkFile) FROM MEMVAR
    ENDSCAN
  ENDIF

ENDIF


SELECT (lcWorkFile)
GO TOP
*-- Check To See If There Are Any Records Selected [begin]
IF EOF()
  PRIVATE lcMessage
  DO CASE
    CASE lcRpTrnTyp='H'
      *-lcMessage = 'Debit on account'
      lcMessage = LANG_ArDcmem_DebOnAcc
    CASE lcRpTrnTyp='D'
      *-lcMessage = 'Debit'
      lcMessage = LANG_ArDcmem_Debit      
    CASE lcRpTrnTyp='C'
      *-lcMessage = 'Credit'
      lcMessage = LANG_ArDcmem_Credit      
    OTHERWISE  && All Transaction Types.
      *-lcMessage = 'Debit/Credit/Debit on account'
      lcMessage = LANG_ArDcmem_DCOnAcc  
  ENDCASE
  *-- Message : There are no XXX Memos to display...!
  *--                < Ok >
  =gfModalGen('TRM40148B40011' , 'ALERT' , lcMessage)
  RETURN
ENDIF
*-- Check To See If There Are Any Records Selected [end  ]

lcEdTime = TIME()  && Time in which we finish collect data.
lnInterval = lfCollTime(lcStTime,lcEdTime)  && Calculate collecting data spent time.
*-WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT(lcWorkFile))) + ' Records in ' + ALLTRIM(STR(lnInterval,6,2)) + ' Seconds...' NOWAIT
WAIT WINDOW LANG_ArDcmem_Slctd + ALLTRIM(STR(RECCOUNT(lcWorkFile))) + LANG_ArDcmem_RecIn + ALLTRIM(STR(lnInterval,6,2)) + LANG_ArDcmem_Sec NOWAIT

lcLstAcc = ''

DO gfDispRe WITH EVAL('lcRpForm')
*-- Call Report [END  ]
SET RELATION TO
*-- end of core code.
*----------------------- Report Code End -----------------------------
*-- Function and procedure section...
*!
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Heba Fathi (HFK)
*! Date      : 02/11/2004
*! Purpose   : Option Grid When function
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*!
FUNCTION lfwRepWhen

*-- if it's first time to run the report.
IF TYPE('lcLastExpr') = 'N'
  DECLARE laCompAdd[6],laCustAdd[5]
  laCompAdd = ''                   && Array to hold the Company  address
  laCustAdd = ''                   && Array to hold the Customer address

  *-- Get company Address [begin].
  *-- Open Company file [Begin]
	lcSelectCommand=[SELECT * FROM SYCCOMP WHERE Ccomp_id=']+oAriaApplication.ActiveCompanyID+[']
	lnResult = oAriaApplication.remotesystemdata.execute(lcSelectCommand,"","SYCCOMP","",oAriaApplication.SystemConnectionString,3,"",SET("DATASESSION")) 
	IF lnResult >= 1 
	  lcCompName = cCom_Name             && Company Name.
	  lcCompPhon = cCom_Phon             && Company Phone.
	  lcPhonPict = gfPhoneTem()          && Company Phone Picture Format.
	  laCompAdd[1] = gfGetAdr('SYCCOMP' , '' , '' , '' , 1)
	  laCompAdd[2] = gfGetAdr('SYCCOMP' , '' , '' , '' , 2)
	  laCompAdd[3] = gfGetAdr('SYCCOMP' , '' , '' , '' , 3)
	  laCompAdd[4] = gfGetAdr('SYCCOMP' , '' , '' , '' , 4)
	  laCompAdd[5] = gfGetAdr('SYCCOMP' , '' , '' , '' , 5)
	  *-laCompAdd[6] = 'Phone# : '+TRANSFORM(lcCompPhon , lcPhonPict)
	  laCompAdd[6] = LANG_ArDcmem_Phone + TRANSFORM(lcCompPhon , lcPhonPict)  
	  = lfAdrShift('laCompAdd')    && Shift Company address if there is empty line.
  ENDIF 
  *-- Get company Address [end].

  SET ORDER TO CUSTOMER IN CUSTOMER
  SET ORDER TO Codes    IN CODES
  SET ORDER TO DEBIT    IN DEBIT
  SET ORDER TO CREDIT   IN CREDIT
  SET ORDER TO ARHIST   IN ARHIST

  IF llMultCurr
    SET ORDER TO CCURRCODE IN SYCCURR  && To VALIDATE currency code.
	lnCurrPos  = ASCAN(loOGScroll.laOGFxFlt,'DEBIT.CCURRCODE')
    IF lnCurrPos > 0
    	lnCurrPos  = ASUBSCRIPT(loOGScroll.laOGFxFlt,lnCurrPos,1)
		loOGScroll.laOGFxFlt[lnCurrPos,6] = oAriaApplication.BaseCurrency
		= LFOGSHOWGET('LAOGFXFLT[' + ALLTRIM(STR(LNCURRPOS)) + ',6]')  && SHOW GET OBJECT .
    ENDIF
  ENDIF
	
  PRIVATE lnNoOFlds
  DIMENSION laTempStru[1,18]
  laTempStru = ''
  SELECT DEBIT
  =AFIELD(laTempStru)
  lnNoOFlds=ALEN(laTempStru,1)
  DIMENSION laTempStru[lnNoOFlds+13, 18] 

  *-- Add Fields found in either credit or arhist files and not in debit file. [Begin]
  laTempStru[lnNoOFlds+1  ,1] = 'cCreditCod'  && From credit file.
  laTempStru[lnNoOFlds+1  ,2] = 'C'
  laTempStru[lnNoOFlds+1  ,3] = 6
  laTempStru[lnNoOFlds+1  ,4] = 0

  laTempStru[lnNoOFlds+2  ,1] = 'Credt_Date'  && From credit file.
  laTempStru[lnNoOFlds+2  ,2] = 'D'
  laTempStru[lnNoOFlds+2  ,3] = 8
  laTempStru[lnNoOFlds+2  ,4] = 0

  laTempStru[lnNoOFlds+3  ,1] = 'History'  && From credit file.
  laTempStru[lnNoOFlds+3  ,2] = 'C'
  laTempStru[lnNoOFlds+3  ,3] = 6
  laTempStru[lnNoOFlds+3  ,4] = 0

  laTempStru[lnNoOFlds+4  ,1] = 'lNonAR'  && From credit file.
  laTempStru[lnNoOFlds+4  ,2] = 'L'
  laTempStru[lnNoOFlds+4  ,3] = 1
  laTempStru[lnNoOFlds+4  ,4] = 0

  laTempStru[lnNoOFlds+5  ,1] = 'Balance'  && From AR History file.
  laTempStru[lnNoOFlds+5  ,2] = 'N'
  laTempStru[lnNoOFlds+5  ,3] = 11
  laTempStru[lnNoOFlds+5  ,4] = 2

  laTempStru[lnNoOFlds+6  ,1] = 'Flag'  && From AR History file.
  laTempStru[lnNoOFlds+6  ,2] = 'C'
  laTempStru[lnNoOFlds+6  ,3] = 1
  laTempStru[lnNoOFlds+6  ,4] = 0

  laTempStru[lnNoOFlds+7  ,1] = 'TotDB'  && From AR History file.
  laTempStru[lnNoOFlds+7  ,2] = 'N'
  laTempStru[lnNoOFlds+7  ,3] = 11
  laTempStru[lnNoOFlds+7  ,4] = 2

  laTempStru[lnNoOFlds+8  ,1] = 'TotCR'  && From AR History file.
  laTempStru[lnNoOFlds+8  ,2] = 'N'
  laTempStru[lnNoOFlds+8  ,3] = 11
  laTempStru[lnNoOFlds+8  ,4] = 2

  laTempStru[lnNoOFlds+9  ,1] = 'OpenAmt'  && From AR History file.
  laTempStru[lnNoOFlds+9  ,2] = 'N'
  laTempStru[lnNoOFlds+9  ,3] = 11
  laTempStru[lnNoOFlds+9  ,4] = 2

  laTempStru[lnNoOFlds+10  ,1] = 'HistDate'  && From AR History file.
  laTempStru[lnNoOFlds+10  ,2] = 'D'
  laTempStru[lnNoOFlds+10  ,3] = 8
  laTempStru[lnNoOFlds+10  ,4] = 0

  laTempStru[lnNoOFlds+11  ,1] = 'Sfs'  && From AR History file.
  laTempStru[lnNoOFlds+11  ,2] = 'C'
  laTempStru[lnNoOFlds+11  ,3] = 4
  laTempStru[lnNoOFlds+11  ,4] = 0

  laTempStru[lnNoOFlds+12  ,1] = 'Deb_Adj'  && From AR History file.
  laTempStru[lnNoOFlds+12  ,2] = 'C'
  laTempStru[lnNoOFlds+12  ,3] = 1
  laTempStru[lnNoOFlds+12  ,4] = 0

  laTempStru[lnNoOFlds+13  ,1] = 'cTranArng'  && From AR History file.
  laTempStru[lnNoOFlds+13  ,2] = 'C'
  laTempStru[lnNoOFlds+13  ,3] = 1
  laTempStru[lnNoOFlds+13  ,4] = 0
  *-- Add Fields found in either credit or arhist files and not in debit file. [Begin]
  FOR lnIndex= 1 TO 13
    FOR  lnIns=7 TO 18 
      STORE SPACE(1) TO laTempStru[lnNoOFlds+lnIndex,lnIns]
    ENDFOR 
  ENDFOR 
  =lfWorkFile()    && Create Temporary cursor.

ENDIF  && END IF you first time enter when function.
*-- end of lfwRepWhen.
*!
*!*************************************************************
*! Name      : lfWorkFile
*! Developer : Heba Fathi (HFK)
*! Date      : 02/11/2004
*! Purpose   : Create work cursor.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*!
FUNCTION lfWorkFile
gfCrtTmp(lcWorkFile,@laTempStru,"cTranArng + Account + History + TranType + Tran + DTOS(TranDate)",lcWorkFile,.T.)
*-- end of lfWorkFile.
*!
*!*************************************************************
*! Name      : lfsrAcc
*! Developer : Heba Fathi (HFK)
*! Date      : 02/11/2004
*! Purpose   : Rise change account flag, in range browse screen.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Note      : S symbol is [S,Set],R is Reset.
*!*************************************************************
*!
FUNCTION lfsrAcc
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'
    llChAcc = .T.
    GO TOP IN CUSTOMER
  CASE lcParm = 'R'
    llClearAcc = .F.
ENDCASE
*-- end of lfsrAcc.
*!
*!*************************************************************
*! Name      : lfvTrnType
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/09/1998
*! Purpose   : Rise flag to recollect data
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ....
*!*************************************************************
*!
FUNCTION lfvTrnType
llChTrnTyp = .T.
*-- end of lfvTrnType.
*!
*!*************************************************************
*! Name      : lfCollTime
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/09/1998
*! Purpose   : Calcualte spent time in data collection.
*!*************************************************************
*! Passed Parameters  : Start collection date,End collection date
*!*************************************************************
*! Returns            : Spent time.
*!*************************************************************
*!
FUNCTION lfCollTime
PARAMETERS lcStart,lcEnd
lnStHour  = IIF(VAL(LEFT(lcStart,2)) = 0,VAL(LEFT(lcStart,2))+24,VAL(LEFT(lcStart,2)))
lnEndHour = IIF(VAL(LEFT(lcEnd,2))   = 0,VAL(LEFT(lcEnd,2))  +24,VAL(LEFT(lcEnd,2)))
lnStart = 3600 * lnStHour  + 60 * VAL(SUBSTR(lcStart,4,2)) + VAL(RIGHT(lcStart,2))
lnEnd   = 3600 * lnEndHour + 60 * VAL(SUBSTR(lcEnd,4,2))   + VAL(RIGHT(lcEnd,2))
RETURN (lnEnd - lnStart)
*-- end of lfCollTime.
*!
*!*************************************************************
*! Name      : lfClearRep
*! Developer : Heba Fathi (HFK)
*! Date      : 02/11/2004
*! Purpose   : Function that we call when Close the option grid.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*!
FUNCTION lfClearRep

llClearFn = .T.    && If you run filter you must create cursor again.
*-- Close temp. opended files, if it used.
IF USED(lcWorkFile)
  USE IN (lcWorkFile)
ENDIF
*-- Close Company file [Begin]
IF USED('SYCCOMP')
  USE IN SYCCOMP
ENDIF
*-- Close Company file [End  ]

*-- Close lcLinkChar file if it was oppened.
IF (ASCAN(LoOGScroll.laSelFile,'LCLINKCHAR') = 0) AND USED('LCLINKCHAR')
  USE IN LCLINKCHAR
ENDIF

*-- Close SYCINT file if it was oppened.
IF (ASCAN(LoOGScroll.laSelFile,'SYCINT') = 0) AND USED('SYCINT')
  USE IN SYCINT
ENDIF
*-- end of lfClearRep.
*!
*!*************************************************************
*! Name      : lfHeadVar
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 12/09/1998
*! Purpose   : Get data to be printed on page header
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*!
FUNCTION lfHeadVar
IF (lcLstAcc <> ACCOUNT) AND SEEK('M'+ACCOUNT,'CUSTOMER')
  *-- Fill Customer array with its data [Begin]
  laCustAdd[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 )
  laCustAdd[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 )
  laCustAdd[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 )
  laCustAdd[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4 )
  laCustAdd[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5 )
  = lfAdrShift('laCustAdd')
ENDIF
*-- Fill Customer array with its data [End]
lcLstAcc = ACCOUNT
llEndGroup = .F.
RETURN ''
*-- end of lfHeadVar.
*!
*!*************************************************************
*! Name      : lfAdrShift
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 12/09/1998
*! Purpose   : Function to Shift the Address array if there is any
*!             empty lines in the address
*!*************************************************************
*! Passed Parameters : Array name
*!*************************************************************
*! Return      : None
*!*************************************************************
*!
FUNCTION lfAdrShift
PARAMETERS lcArrayNam

*-- FOR Loop to loop the Address Array
FOR lnCount = 1 TO ALEN(&lcArrayNam)
  *IF The current Array element is of type character and empty
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
	EMPTY(&lcArrayNam.[lnCount])
    =ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

*- FOR Loop to loop the Address Array
FOR lnCount = 1 TO ALEN(&lcArrayNam)

  *IF The current Array element is not of type character
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop
*-- end of lfAdrShift.
*!
*!*************************************************************
*! Name      : lfEndGroup
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/09/1998
*! Purpose   : Rise end group flag, assign value printed on DOS mode Footer
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ....
*!*************************************************************
*!
FUNCTION lfEndGroup
lnTotAmt   = lnGrpAmt
llEndGroup = .T.
RETURN ''
*-- end of lfEndGroup.
*!
