*:***************************************************************************
*: Program file       : ARASSHT
*: Program desc.      : ASSIGNMENT SHEET REPORT
*: System             : ARIA4XP
*: Module             : ACCOUNT RECEIVABLE (AR)
*: Developer          : Tarek Noaman (TNA)
*: Tracking Job Number: N037671
*: Date               : 5/21/2006
*:***************************************************************************
*: Calls         : 
*:    Functions  : lfwRunGrid, lfvInvNo, lfwOldVal, lfEvalVars, lfvForm
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example       : DO ARASSHT
*:***************************************************************************
*:                   Option grid Filter contains 
*:01-Print Form                    (S)hort/(L)ong                     lcRpType
*:02-Invoice Date                  Between                            INVHDR.INVDATE
*:03-Invoice                       In List                            INVHDR.INVOICE
*:04-Factor Code                   In List                            INVHDR.CFACCODE
*:05-Currency                      In List                            INVHDR.CCURRCODE
*-- if selected (L)ong Print Form, we add the following to the option grid :
*:06-Assignment Sheet #            IS                                 lcRpAssSht
*:07-Factor Commission %           IS                                 lnRpFactCm
*:08-Reserve With Held             IS                                 lnRpReserP
*:09-Other Charges                 IS                                 lnRpOther
*:***************************************************************************
*!*	ACTIVATE WINDOW trace
*!*	_screen.Visible =.T.
*!*	SUSPEND

DECLARE laAddress[6,3]      && only the first five addresses are needed
STORE '' TO lcBillAdd1,lcBillAdd2,lcBillAdd3,lcBillAdd4,lcBillAdd5

lcStTime   = TIME()
llDummy    = loOgScroll.llOGFltCh AND lfCollData()
lcEdTime   = TIME()
lnInterval = lfCollTime(lcStTime,lcEdTime)  && Calculate collecting data spent time.*--
SELECT FInvhdr
GO TOP
WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT())) + ' Record(s) in ' + ALLTRIM(STR(lnInterval,6,2)) + ' Seconds...' NOWAIT

IF RECCOUNT()=0
  *--There is no record to display
  =gfModalGen('TRM00052B40011','ALERT')
  RETURN .F.
ENDIF 

FactName = IIF(SEEK(lcRpSpeFac,'SYCFACT'),SYCFACT.CFACCOMP,' ')

DIMENSION loOGScroll.laCRParams[5,2]
loOGScroll.laCRParams[1,1]  = 'Layout'
loOGScroll.laCRParams[1,2]  = IIF(lcRpType='S','Short','Long')
loOGScroll.laCRParams[2,1]  = 'sortby'
loOGScroll.laCRParams[2,2]  = IIF(lcRpType='S','Invoice','Account+Invoice')
loOGScroll.laCRParams[3,1]  = 'ReportName'
loOGScroll.laCRParams[3,2]  = 'Assignment Schedule'
loOGScroll.laCRParams[4,1]  = 'AssNum'
loOGScroll.laCRParams[4,2]  = lcRpAssSht
loOGScroll.laCRParams[5,1]  = 'FactName'
loOGScroll.laCRParams[5,2]  = FactName


lcTmpFInv = loOgScroll.gfTempName()

DIMENSION loOgScroll.lacrTABLES[1]  && array For Temp Table & pathes 
loOgScroll.lacrTABLES[1] = oAriaApplication.WorkDir+lcTmpFInv+'.DBF'

SELECT FInvhdr
COPY TO oAriaApplication.WorkDir + lcTmpFInv + ".DBF"
*COPY TO oAriaApplication.WorkDir + "FInvhdr.DBF"
*!*	*!*	=gfOpenFile(oAriaApplication.WorkDir + (lcTmpFInv),'','EX')
*!*	*!*	USE IN (lcTmpFInv)  &&close the file will display from to be opened exclusive

loOgScroll.lcOGLastForm = IIF(lcRpType='S',"ARASSHTS","ARASSHTL")
loogScroll.cCROrientation = 'P'

= gfDispRe()

ERASE oAriaApplication.WorkDir+lcTmpFInv+'.DBF'


*!*************************************************************
*! Name      : lfCollData
*! Developer : Tarek Noaman	(TNA)
*! Date      : 4/18/2006
*! Purpose   : Collection of Data
*!*************************************************************
*! Called from : This Program
*!*************************************************************
FUNCTION lfCollData

CREATE CURSOR FInvHDR  (invoice C(6),account C(5),btname C(30),invdate D,terms C(30),approval C(10),factno C(10), ;
                        totalchg N(14,2),lnTradeDis N(14,2),lnFactFee N(14,2),lnReserve N(14,2),lnOthers N(14,2),lnNet N(14,2), ;
                        caddress1 C(30),caddress2 C(30),caddress3 C(30),caddress4 C(30),caddress5 C(30))

IF lcRpType='L'
  SELECT FInvhdr
  INDEX ON Account+Invoice TAG INVTEMP
ENDIF

SELECT INVHDR
SET RELATION TO IIF(EMPTY(STORE),'M'+ACCOUNT,'S'+ACCOUNT+STORE) INTO CUSTOMER

SUM TotalChg TO lnTotInv

lcRpExp = IIF(!EMPTY(lcRpSpeFac),lcRpExp+" AND INVHDR.CFACCODE='"+lcRpSpeFac+"'",lcRpExp)

SCAN FOR EVALUATE(lcRpExp)
  SCATTER MEMVAR memo
  INSERT INTO FInvhdr FROM MEMVAR
      REPLACE FInvhdr.btname   WITH customer.btname, ;
              FInvhdr.terms    WITH gfCodDes(INVHDR.CTERMCODE,'CTERMCODE'), ;
              FInvhdr.factno   WITH IIF(INVHDR.CFACCODE=CUSTOMER.CFACCODE,SUBSTR(CUSTOMER.FACTACCT,1,10),'')

  IF lcRpType = 'S'
    STORE '' TO lcBillAdd1,lcBillAdd2,lcBillAdd3,lcBillAdd4,lcBillAdd5
    =gfGetAdr('Customer','','','',1,'2')
    FOR lnCount = 1 TO ALEN(laAddress,1)
      lcCount   = STR(laAddress[lnCount,1],1)
      lcBillAdd&lcCount = lcBillAdd&lcCount + IIF(EMPTY(lcBillAdd&lcCount),'',',')+;
      SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
    ENDFOR
    REPLACE FInvhdr.caddress1 WITH lcBillAdd1, ;
            FInvhdr.caddress2 WITH lcBillAdd2, ;
            FInvhdr.caddress3 WITH lcBillAdd3, ;
            FInvhdr.caddress4 WITH lcBillAdd4, ;
            FInvhdr.caddress5 WITH lcBillAdd5
  ELSE
    lnTradeDis = ( (Trde_Disc/100)*TotalChg )
    lnFactFee  = (TotalChg - lnTradeDis)*(lnRpFactCm/100)
    lnReserve  = (TotalChg - lnTradeDis)*(lnRpReserP/100)
    lnOthers   = (TotalChg / lnTotInv ) * lnRpOther
    lnNet      = TotalChg - (lnTradeDis + lnFactFee + lnReserve + lnOthers)
    REPLACE FInvhdr.lnTradeDis WITH lnTradeDis, ;
            FInvhdr.lnFactFee  WITH lnFactFee, ;
            FInvhdr.lnReserve  WITH lnReserve, ;
            FInvhdr.lnOthers   WITH lnOthers, ;
            FInvhdr.lnNet      WITH lnNet
  ENDIF
ENDSCAN

*!*************************************************************
*! Name      : lfCollTime
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 06/04/99
*! Purpose   : Calcualte spent time in data collection.
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
*! Name      : lfwRunGrid
*! Developer : IHB
*! Date      : 11/30/1998
*! Purpose   : valid function when run grid.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfwRunGrid()
*!*************************************************************
FUNCTION lfwRunGrid
lcRpSpeFac = SYCFACT.CFACCODE
IF llMultCurr
  SET ORDER TO CCURRCODE IN SYCCURR     && To VALIDATE currency code.
  lnCurrPos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'INVHDR.CCURRCODE'),1)
  IF lnOGSeting = 1
    laOGFxFlt[lnCurrPos,6] = gcBaseCurr
    = lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnCurrPos)) + ',6]')  && Show get Object .
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfwOldVal
*! Developer : Haytham El_Sheltawi
*! Date      : 01/11/1998
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
FUNCTION lfwOldVal
laOldVal = EVALUATE(SYS(18))      && Varible to hold the old value
                                  && this is a global solution for any object type.

*!*************************************************************
*! Name      : lfEvalVars
*! Developer : IHB
*! Date      : 12/15/1998
*! Purpose   : Assign/Fill Default values used in Option Grid .
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : 
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
*-- if multi currency evaluate currency arrays [End]

*!*************************************************************
*! Name      : lfvForm
*! Developer : IHB
*! Date      : 12/06/1998
*! Purpose   : To validate the Form Type Option.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfvSortBy()
*!*************************************************************
FUNCTION lfvForm
ClearRead()

*!*************************************************************
*! Name      : lfvFactCom
*! Developer : IHB
*! Date      : 01/01/1999
*! Purpose   : Validate Factor Commission.
*!*************************************************************
*! Calls     : NONE
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvFactCom()
*!*************************************************************
FUNCTION lfvFactCom
IF lnRpFactCm >= 0
  RETURN .T.
ELSE
  RETURN .F.
ENDIF

*!*************************************************************
*! Name      : lfvReserPs
*! Developer : IHB
*! Date      : 01/01/1999
*! Purpose   : Validate Factor Reserve with Held.
*!*************************************************************
*! Calls     : NONE
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvReserPs()
*!*************************************************************
FUNCTION lfvReserPs
IF lnRpReserP >= 0
  RETURN .T.
ELSE
  RETURN .F.
ENDIF

*!*************************************************************
*! Name      : lfvOterChg
*! Developer : IHB
*! Date      : 01/01/1999
*! Purpose   : Validate Factor Otehr charges.
*!*************************************************************
*! Calls     : NONE
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvOterChg()
*!*************************************************************
FUNCTION lfvOterChg
IF lnRpOther >= 0
  RETURN .T.
ELSE
  RETURN .F.
ENDIF


*!*************************************************************
*! Name      : lfvFactor
*! Developer : IHB
*! Date      : 12/15/1998
*! Purpose   : Validate factor option.
*!*************************************************************
*! Calls     : ARIABROW()
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvFactor()
*!*************************************************************
*!*	FUNCTION lfvFactor
*!*	PRIVATE lnCurAlias,lcFactor
*!*	lnCurAlias = SELECT(0)
*!*	SELECT SycFact
*!*	SET ORDER TO TAG Cfaccode
*!*	*-- laOGFxFlt[3,6] stores the selected factor value
*!*	*-- if there's a factor value and it's not exist in the SYCFACT, browse from SYCFACT
*!*	IF !EMPTY(laOGFxFlt[3,6]) .AND. !SEEK(laOGFxFlt[3,6],'SycFact')
*!*	  lcBrFields     = [cFacCode:H='Factor',cFacComp:H='Name',cFacCont:H='Contact',cPhoneNo :P= gfPhoneTem() :H='Phone']
*!*	  SELECT SycFact
*!*	  laOGFxFlt[3,6] = IIF(ARIABROW('',"Factors",gnBrFSRow1, gnBrFSCol1,;
*!*	                  gnBrFSRow2, gnBrFSCol2,'','','cFacCode','laBrowArr'),;
*!*	                  SycFact.cFacCode,SPACE(6))

*!*	ENDIF  && end if there's a factor value and it's not exist in the SYCFACT
*!*	*-- factor code cannot be empty
*!*	IF EMPTY(laOGFxFlt[3,6])
*!*	  WAIT WINDOW 'Factor Code Can Not Be Empty! Try Again.'
*!*	  RETURN .F.
*!*	ENDIF
*!*	*--
*!*	SELECT (lnCurAlias)


FUNCTION lfvFactor
PRIVATE lnAlias,lcObjName , lcObjVal , laRetVal, lcFile_Ttl
lcObjName = OGSYS18()      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(OGSYS18())      && Varible to hold  the value of the current GET field

=gfOpenFile(oAriaApplication.SysPath+'SycFact',oAriaApplication.SysPath+'Cfaccode','SH')  

IF '?' $lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal,'SycFact'))
  lcBrFields  = [cFacCode:H='Factor',cFacComp:H='Factor/Company Name',cFacCont:H='Contact',cPhoneNo :P= gfPhoneTem() :H='Phone',cFacTitle :H='Title']
  *lcBrFields  = [cFacCode:H=LANG_Araging_Fctr ,cFacComp:H=LANG_Araging_FacCom ,cFacCont:H=LANG_Araging_Contact ,cPhoneNo :P= gfPhoneTem() :H=LANG_Araging_Phone ,cFacTitle :H=LANG_Araging_Title ]
  lcFile_Ttl= 'Factor'
*!*	  lcFile_Ttl= LANG_Araging_Fctr 
  SELECT SYCFACT
  LOCATE 
  DECLARE laRetVal[1]
  IF gfBrows('' ,'CFacCode', 'laRetVal',lcFile_Ttl)
    &lcObjName = laRetVal[1]
    lcRpSpeFac= EVALUATE(lcObjName)
  ELSE    && Else
    &lcObjName = lcObjVal
  ENDIF    && End of IF   
ENDIF  
IF EMPTY(lcRpSpeFac)
  WAIT WINDOW 'Factor Code Can Not Be Empty! Try Again.'
  RETURN .F.
ENDIF
