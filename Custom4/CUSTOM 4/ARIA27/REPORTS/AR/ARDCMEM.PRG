*:***************************************************************************
*: Program file  : ARDCMEM 
*: Program desc. : debit/Credit Memos Report
*: For Report    : (ARDCMEM.FRX)
*: System        : Aria Advantage Series.
*: Module        : Accounts Receivable (AR)
*: Developer     : Mohamed Atia Badran (MAB)
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : gfModalGen,gfDispRe,gfGetMemVar,gfOpenFile,gfPhoneTem,gfGetAdr,
*:               : lfEvalVars,lfwRepWhen,lfWorkFile,lfsrAcc,lfvTrnType,lfCollTime,
*:               : lfClearRep,lfHeadVar,lfAdrShift,lfEndGroup
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO ARDCMEM 
*:***************************************************************************
*: This Report Program is due to E301083 ...
*:E301170,1 MAB 03/15/1999 Use variable llOGFltCh that detect OG filter changes.
*:***************************************************************************
*: Modifications
*: B802341,1 BWA 06/22/99 Fix the bug of print <City ST ZIP> in the same line 
*: B802341,1 BWA          and in the ARDCMEM.FRX fix the bug of leave empty 
*: B802341,1 BWA          space between the shaded header and first transaction. 
*: B603224,1 ADEL 12/15/99 Fix the bug that if user choose to print Credit or All
*: B603224,1               the program does not get ARHIST records.
*: E301429,1 NAD 06/19/2000 Add record to the SYREPUVR to ask the user to print decimals or not. 

*E127808,1 EIH 06/02/2005 In case of england print "Tax Description - Tax Code" in right hand side of
*E127808,1 EIH            Company address6.

*:***************************************************************************
lcStTime   = TIME()    && Time in which we start collect data.
llEndGroup = .F.       && Flag detect that we finish group
lnTotAmt   = 0         && Amount Printed on Page footer [Dos Mode]
*-- if user change last filter .


*E127808,1 EIH 06/02/2005 Check if company is england or not and set lcTaxDeCod with
*E127808,1 EIH            New value [Begin].  
STORE SPACE(0) TO lcTaxDeCod
IF gcContCode = 'ENG'
  lcTaxDeCod =  ALLTRIM(gfGetMemVar('M_TAX_REFE'))
ENDIF
*E127808,1 EIH [End].




*E301170,1 MAB 03/15/1999 Use variable llOGFltCh that detect OG filter changes.[Begin
*IF llClearFn OR llChAcc OR llChTrnTyp OR (TYPE('lcLastExpr') = 'N') OR !(lcRpExp == lcLastExpr)
IF llClearFn OR llOGFltCh
*E301170,1 MAB 03/15/1999 Use variable llOGFltCh that detect OG filter changes.[End

  STORE .F. TO llChAcc,llChTrnTyp,llClearFn
  lcLastExpr = lcRpExp

  *-- if you have previous data clear workfile then recreate it. [begin]
  IF !USED(lcWorkFile) OR (RECCOUNT(lcWorkFile) > 0)
    IF USED(lcWorkFile)
      USE IN (lcWorkFile)
    ENDIF  
    =lfWorkFile()
  ENDIF
  *-- if you have previous data clear workfile then recreate it. [end]

  *-- Evaluate report expression [Begin]
  lcRpExp    = STRTRAN(lcRpExp,'DEBIT.','')  
  lcRpExp    = IIF(ALLTRIM(lcRpExp) = '.T.','',lcRpExp)  && Report Filter Expression.

  lcCBFilter = ''
  IF 'TRANDATE' $ lcRpExp
    
    IF 'ACCOUNT' $ lcRpExp
      lnDateStPs  = ATC('BETWEEN(DTOS(TRANDATE',lcRpExp) 
      lnDateLen   = ATC('})))',lcRpExp) + 5 - lnDateStPs
      lcDateExpr  = SUBSTR(lcRpExp,lnDateStPs,lnDateLen)
      lcDateExpr  = STRTRAN(lcDateExpr,'TRANDATE','ChgBk_Date')
      *B802341,1 BWA 06/22/99  Fix the bug of not change the index when selected the credit type
      *lcCBFilter = lcRpExp + [ AND ] + lcDateExpr
      IF !EMPTY(lcDateExpr)
        lcCBFilter = lcRpExp + [ AND ] + lcDateExpr + [ AND ]
      ELSE
        lcCBFilter = lcRpExp + [ AND ] 
      ENDIF
      *B802341,1 BWA [END]
    ELSE
      *B802341,1 BWA 06/22/99  Fix the bug of not change the index when selected the credit type
      *lcCBFilter = STRTRAN(lcRpExp,'TRANDATE','ChgBk_Date')
      lcCBFilter = STRTRAN(lcRpExp,'TRANDATE','ChgBk_Date') + [AND]
      *B802341,1 BWA [END]
    ENDIF
  
  ENDIF
  *-- Evaluate report expression [End  ]

  *-- Collect report data [Begin]
  PRIVATE lcDebFilt,lcCreFilt,lcTranType,lcHistFilt
  STORE '' TO lcDebFilt,lcCreFilt,lcTranType,lcHistFilt

  *-- if user choice is to print charge back or all
  IF lcRpTrnTyp $ 'HA'

    USE (gcDataDir+'ARHIST') ORDER TAG Arhistht AGAIN ALIAS (lcHistFile) IN 0 SHARED

    lcChBkFilt = ''
    IF EMPTY(lcCBFilter)
      lcChBkFilt = [TRANTYPE $ '48']
    ELSE
    *B802341,1 BWA 06/22/99  Fix the bug of not change the index when selected the credit type
      *lcChBkFilt = "(" + lcCBFilter + " AND TRANTYPE='8') OR (" +;
                            lcRpExp + " AND TRANTYPE='4')"
      lcChBkFilt = "(" + lcCBFilter + "     TRANTYPE='8') OR (" +;
                            lcRpExp + " AND TRANTYPE='4')"
    *B802341,1 BWA [END]
                            
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
    lcDebFilt  = IIF(EMPTY(lcRpExp),[TRANTYPE='2'], lcRpExp + [ AND TRANTYPE='2'])
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

    *B802341,1 BWA 06/22/99  Fix the bug of not change the index when selected the credit type
    =ACOPY(laOGHDFlt , lcRpHdflt )  && Array to hold the Hidden   filter array to select it back 
    =ACOPY(laOGFxFlt , lcRpFxflt )  && Array to hold the Fixed    filter array to select it back
    =ACOPY(laOGVrFlt , lcRpVrflt )  && Array to hold the Variable filter array to select it back
      
    For lnCount =  1 TO ALEN(laOGHDFlt,1)
      laOGHDFlt[lnCount,1] = STRTRAN(laOGHDFlt[lnCount,1],'DEBIT.','CREDIT.')
    ENDFOR

    For lnCount =  1 TO ALEN(laOGFxFlt,1)
      LAOGFXFLT[lnCount,1] = STRTRAN(LAOGFXFLT[lnCount,1],'DEBIT.','CREDIT.')
    ENDFOR
      
    For lnCount =  1 TO ALEN(laOGVrFlt,1)
      laOGVrFlt[lnCount,1] = STRTRAN(laOGVrFlt[lnCount,1],'DEBIT.','CREDIT.')      
    ENDFOR
      
    lcOGHDFlt = IIF(EMPTY(lcOGHDFlt),".T.",gfGenFlt('laOGHDFlt' , llOGFilter , .T.))  && variable holf the conditoins in the laOgFdFkt
    lcOGFxFlt = IIF(EMPTY(lcOGFxFlt),".T.",gfGenFlt('laOGFxFlt' , llOGFilter , .T.))  && variable holf the conditoins in the laOgFxFkt
    lcOGVrFlt = IIF(EMPTY(lcOGVrFlt),".T.",gfGenFlt('laOGVrFlt' , llOGFilter , .T.))  && variable holf the conditoins in the laOgVrFkt 

    lcRpExp = "(" + lcOGHDFlt + ")" + ".AND." + "(" + lcOGFxFlt + ")" + ".AND." + "(" + lcOGVrFlt + ")"
    *BB603224,1 (Begin) As lcRpExp now has "CREDIT." in it, se we will remove it for 
    *BB603224,1         rushmore when scanning CREDIT file and avoiding logical 
    *BB603224,1         bug (CREDIT.) in scnning filter when scanning ARHIST.
    lcRpExp    = STRTRAN(lcRpExp,'CREDIT.','')  
    *BB603224,1 (End)
    *BB603224,1 BWA [END]

    lcTranType = lcTranType + '5'
    lcCreFilt  = IIF(EMPTY(lcRpExp),[TRANTYPE='2'], lcRpExp + [ AND TRANTYPE='5'])
    lcCreFilt  = [account+tran+DTOS(trandate) = '' AND ] + lcCreFilt
    
    m.cTranArng = 'B'
    SELECT CREDIT
    SCAN FOR &lcCreFilt
      SCATTER MEMVAR MEMO
      INSERT INTO (lcWorkFile) FROM MEMVAR
    ENDSCAN
    *B802341,1 BWA 06/22/99  Fix the bug of not change the index when selected the credit type
    =ACOPY(lcRpHdflt , laOGHDFlt )  && Array to hold the FIRST Hidden   filter array to select it back
    =ACOPY(lcRpFxflt , laOGFxFlt )  && Array to hold the FIRST Fixed    filter array to select it back
    =ACOPY(lcRpVrflt , laOGVrFlt )  && Array to hold the FIRST Variable filter array to select it back
    *B802341,1 BWA [END]
  ENDIF  && end if user choice is to print credit or all
  *-- Collect report data [End  ]
  
  *-- Debit (and/or) credit history.
  IF !EMPTY(lcTranType)
    lcHistFilt = IIF(EMPTY(lcRpExp),[TRANTYPE $ lcTranType],;
                    lcRpExp + [ AND TRANTYPE $ lcTranType])
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
      lcMessage = 'Debit on account'
    CASE lcRpTrnTyp='D'
      lcMessage = 'Debit'
    CASE lcRpTrnTyp='C'
      lcMessage = 'Credit'
    OTHERWISE  && All Transaction Types.
      lcMessage = 'Debit/Credit/Debit on account'
  ENDCASE
  *-- Message : There are no XXX Memos to display...!
  *--                < Ok > 
  =gfModalGen('TRM40148B40011' , 'ALERT' , lcMessage)
  RETURN
ENDIF
*-- Check To See If There Are Any Records Selected [end  ]

lcEdTime = TIME()  && Time in which we finish collect data.
lnInterval = lfCollTime(lcStTime,lcEdTime)  && Calculate collecting data spent time.
WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT(lcWorkFile))) + ' Records in ' + ALLTRIM(STR(lnInterval,6,2)) + ' Seconds...' NOWAIT

lcLstAcc = ''
DO gfDispRe WITH EVAL('lcRpForm')
*-- Call Report [END  ]
SET RELATION TO
*-- end of core code.
*----------------------- Report Code End -----------------------------


*-- Function and procedure section...
*------------------------------------
*!*************************************************************
*! Name      : lfEvalVars
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/09/1998
*! Purpose   : Fill Default values used in both OG and Report.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : gfGetMemVar,gfOpenFile
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : = lfEvalVars()
*!*************************************************************
FUNCTION lfEvalVars

llMultCurr  = gfGetMemVar('llMulCurr')    && .T., if company use multi currency.
*-- if multi currency evaluate currency arrays [Begin]
IF llMultCurr
  DIMENSION laCurrVal[1,1]

  IF !USED('SYCCURR')
    =gfOpenFile(gcSysHome+'SYCCURR',gcSysHome+'Ccurrcode','SH')
  ENDIF

  SELECT DISTINCT CCURRCODE FROM SYCCURR ORDER BY CCURRCODE INTO ARRAY laCurrVal
  DIMENSION laCurrDesc[ALEN(laCurrVal,1),1]

  SELECT SYCCURR
  SET ORDER TO CCURRCODE  && To VALIDATE currency code.
  FOR lnI = 1 TO ALEN(laCurrVal,1)
    = SEEK(ALLTRIM(laCurrVal[lnI,1]))
    laCurrDesc[lnI,1] = CCURRCODE + ' - ' + ALLTRIM(CCURRDESC)
  ENDFOR
ENDIF
*-- if multi currency evaluate currency arrays [Begin]
*-- end of lfEvalVars.

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/09/1998
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : gfGetMemVar
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen

*-- if it's first time to run the report.
IF TYPE('lcLastExpr') = 'N'

  DECLARE laCompAdd[6],laCustAdd[5]
  laCompAdd = ''                   && Array to hold the Company  address
  laCustAdd = ''                   && Array to hold the Customer address

  *-- Get company Address [begin].
  *E301077,72 Open Company file [Begin]
  IF !USED('SYCCOMP')
    USE (gcSysHome+'SYCCOMP') ORDER TAG Ccomp_id IN 0 SHARED
  ENDIF
  *E301077,72 Open Company file [End  ]

  *-- Get company Address [begin].
  SELECT SYCCOMP
  SEEK gcAct_Comp
  lcCompName = cCom_Name             && Company Name.
  lcCompPhon = cCom_Phon             && Company Phone.
  lcPhonPict = gfPhoneTem()          && Company Phone Picture Format.

  laCompAdd[1] = gfGetAdr('SYCCOMP' , '' , '' , '' , 1)
  laCompAdd[2] = gfGetAdr('SYCCOMP' , '' , '' , '' , 2)
  laCompAdd[3] = gfGetAdr('SYCCOMP' , '' , '' , '' , 3)
  laCompAdd[4] = gfGetAdr('SYCCOMP' , '' , '' , '' , 4)
  laCompAdd[5] = gfGetAdr('SYCCOMP' , '' , '' , '' , 5)
  laCompAdd[6] = 'Phone# : '+TRANSFORM(lcCompPhon , lcPhonPict)
  
  = lfAdrShift('laCompAdd')    && Shift Company address if there is empty line.
  *-- Get company Address [end].
  
  SET ORDER TO CUSTOMER IN CUSTOMER
  SET ORDER TO Codes    IN CODES   
  SET ORDER TO DEBIT    IN DEBIT   
  SET ORDER TO CREDIT   IN CREDIT   
  SET ORDER TO ARHIST   IN ARHIST   

  IF llMultCurr
    SET ORDER TO CCURRCODE IN SYCCURR  && To VALIDATE currency code.

    lnCurrPos  = ASCAN(laOGFxFlt,'DEBIT.CCURRCODE')
    IF lnCurrPos > 0
      lnCurrPos  = ASUBSCRIPT(laOGFxFlt,lnCurrPos,1)
      laOGFxFlt[lnCurrPos,6] = gcBaseCurr
      = lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnCurrPos)) + ',6]')  && Show get Object .
    ENDIF
    
  ENDIF

  PRIVATE lnNoOFlds
  DIMENSION laTempStru[1,4]
  STORE '' TO laTempStru

  *-- Make array have Debit file structure.
  SELECT DEBIT
  lnNoOFlds = AFIELDS(laTempStru)

  DIMENSION laTempStru[lnNoOFlds + 13, 4]

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

  =lfWorkFile()    && Create Temporary cursor.
  
ENDIF  && END IF you first time enter when function.
*-- end of lfwRepWhen.

*!*************************************************************
*! Name      : lfWorkFile
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/09/1998
*! Purpose   : Create work cursor.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfWorkFile()
*!*************************************************************
FUNCTION lfWorkFile
CREATE CURSOR (lcWorkFile) ;
   FROM ARRAY laTempStru

SELECT (lcWorkFile)
INDEX ON cTranArng + Account + History + TranType + Tran + DTOS(TranDate) TAG (lcWorkFile)
*-- end of lfWorkFile.

*!*************************************************************
*! Name      : lfsrAcc
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/09/1998
*! Purpose   : Rise change account flag, in range browse screen.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfsrAcc()
*!*************************************************************
*! Note      : S symbol is [S,Set]
*!*************************************************************
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

*!*************************************************************
*! Name      : lfvTrnType
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/09/1998
*! Purpose   : Rise flag to recollect data
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : = lfvTrnType()
*!*************************************************************
FUNCTION lfvTrnType
llChTrnTyp = .T.
*-- end of lfvTrnType.

*!*************************************************************
*! Name      : lfCollTime
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/09/1998
*! Purpose   : Calcualte spent time in data collection.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code section.
*!*************************************************************
*! Passed Parameters  : Start collection date,End collection date
*!*************************************************************
*! Returns            : Spent time.
*!*************************************************************
*! Example   : =lfCollTime()
*!*************************************************************
FUNCTION lfCollTime
PARAMETERS lcStart,lcEnd
lnStHour  = IIF(VAL(LEFT(lcStart,2)) = 0,VAL(LEFT(lcStart,2))+24,VAL(LEFT(lcStart,2)))
lnEndHour = IIF(VAL(LEFT(lcEnd,2))   = 0,VAL(LEFT(lcEnd,2))  +24,VAL(LEFT(lcEnd,2)))
lnStart = 3600 * lnStHour  + 60 * VAL(SUBSTR(lcStart,4,2)) + VAL(RIGHT(lcStart,2))
lnEnd   = 3600 * lnEndHour + 60 * VAL(SUBSTR(lcEnd,4,2))   + VAL(RIGHT(lcEnd,2))
RETURN (lnEnd - lnStart)
*-- end of lfCollTime.

*!*************************************************************
*! Name      : lfClearRep
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/09/1998
*! Purpose   : Function that we call when Close the option grid.
*!*************************************************************
*! Called from : [Option Grid] < Close > button.
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfClearRep()
*!*************************************************************
FUNCTION lfClearRep
llClearFn = .T.    && If you run filter you must create cursor again.
*-- Close temp. opended files, if it used.
IF USED(lcWorkFile)
 USE IN (lcWorkFile)
ENDIF

*E301077,72 Close Company file [Begin]
IF USED('SYCCOMP')
  USE IN SYCCOMP
ENDIF
*E301077,72 Close Company file [End  ]

*-- Close lcLinkChar file if it was oppened.
IF (ASCAN(laSelFile,'LCLINKCHAR') = 0) AND USED('LCLINKCHAR')
  USE IN LCLINKCHAR
ENDIF

*-- Close SYCINT file if it was oppened.
IF (ASCAN(laSelFile,'SYCINT') = 0) AND USED('SYCINT')
  USE IN SYCINT
ENDIF

*-- end of lfClearRep.

*!*************************************************************
*! Name      : lfHeadVar
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 12/09/1998
*! Purpose   : Get data to be printed on page header
*!*************************************************************
*! Called from : Page header band.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfAdrShift
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfHeadVar()
*!*************************************************************
FUNCTION lfHeadVar
IF (lcLstAcc <> ACCOUNT) AND SEEK('M'+ACCOUNT,'CUSTOMER')
  *-- Fill Customer array with its data [Begin]
  *B802341,1 BWA 06/22/99 Fix the bug of print <City ST ZIP> in the same line   
  *laCustAdd[1] = CUSTOMER.CADDRESS1
  *laCustAdd[2] = CUSTOMER.CADDRESS2
  *laCustAdd[3] = CUSTOMER.CADDRESS3 
  *laCustAdd[4] = ALLTRIM(CUSTOMER.CADDRESS4) +  ' - ' + ALLTRIM(CUSTOMER.CADDRESS5)
  *laCustAdd[5] = CUSTOMER.CADDRESS6
  
  laCustAdd[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 )
  laCustAdd[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 )
  laCustAdd[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 )
  laCustAdd[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4 )
  laCustAdd[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5 )
  *B802341,1 BWA [END]
  = lfAdrShift('laCustAdd')
ENDIF  
*-- Fill Customer array with its data [End]
lcLstAcc = ACCOUNT
llEndGroup = .F.
RETURN ''
*-- end of lfHeadVar.

*!*************************************************************
*! Name      : lfAdrShift
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 12/09/1998
*! Purpose   : Function to Shift the Address array if there is any
*!             empty lines in the address
*!*************************************************************
*! Called from : MAMATPO.PRG 
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

*FOR Loop to loop the Address Array
FOR lnCount = 1 TO ALEN(&lcArrayNam)
  
  *IF The current Array element is of type character and empty
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
     EMPTY(&lcArrayNam.[lnCount])
    
    =ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

*FOR Loop to loop the Address Array
FOR lnCount = 1 TO ALEN(&lcArrayNam)
  
  *IF The current Array element is not of type character
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop
*-- end of lfAdrShift.

*!*************************************************************
*! Name      : lfEndGroup
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/09/1998
*! Purpose   : Rise end group flag, assign value printed on DOS mode Footer
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : = lfEndGroup()
*!*************************************************************
FUNCTION lfEndGroup
lnTotAmt   = lnGrpAmt
llEndGroup = .T.
RETURN ''
*-- end of lfEndGroup.