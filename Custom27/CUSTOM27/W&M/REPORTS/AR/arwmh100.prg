*:***************************************************************************
*: Program file  : ARAGING.PRG
*: Program desc. : A/R aging report
*: For Report    : Araging.FRX ,Araginh.FRX
*: System        : Aria Advantage Series.
*: Module        : Account Rec. (AR)
*: Date          : 07/14/1999
*: Developer     : Samah Wilson Kirollos (SWK)
*: Transport by  : Sameh Saiid (SSE)
*:***************************************************************************
*: This Program is due to C101568 ... for "W&M HeadWare" as "ARWMH100.PRG"
*:***************************************************************************
*: Modifactions
*:***************************************************************************
*
*C101568,1 SSE 07/13/1999 this Custom program is using ONLY THE SHORT FORM so everything 
*C101568,1                is left without change except only in Report Variables (SYRPUVR)   

*=================================  Begin ===============================*
*=========================  Report Program Code =========================*
*========================================================================*
*-- lcStTime   : Variable to hold the start Time
*-- llCurrGrp  : Flag .T. if multi currency and print in foreign and not sort by currency.
*-- lcReplExpr : Replace key field expression instead of use index command on huge file
*-- lcAcctFilt : Variable to hold filter on account
*-- lcSorted   : Printed Sort By expression
*-- lcGroup    : variable hold Short Form groups.
*-- lcInnGrp   : var to hold the group by in FRX
*-- lcCodeDesc : var to hold the Term code and description in one string

lcStTime   = TIME()
llCurrGrp  = llMultCur AND (lcRpCurr = 'F') AND (lcRpAgSort !='C')
STORE '' TO lcAcctFilt , lcSorted , lcGroup , lcInnGrp , lcCodeDesc
lcReplExpr = lfGetReplc()  && Get Sort by value to update cTempKey with it.

*-- if user change filter criteria then you must collect data again
IF llOGFltCh
  *-- if you have previous data clear work files then recreate it. [begin]
  IF !USED(lcTempAge) OR (RECCOUNT(lcTempAge) > 0)
    =lfWorkFile()
  ENDIF
  *-- if you have previous data clear work files then recreate it. [End  ]

  *-- llChSortBy : Flag if .T. indicate that user change sort by, but here false it
  *                Avoid Replacing Key field with proper sort by values in if...endif block.
  *                i.e. do not enter reindex block because we do it with data collection.
  llChSortBy = .F. 
  
  =lfCollect()               && Collect Aging data.
  
ENDIF  && if user change filter criteria then you must collect data again.

*-- Asking if no records (Display message) otherwise print report [Begin.]
IF RECCOUNT(lcTempAge) = 0
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
  RETURN
ENDIF
*-- Asking if no records (Display message) otherwise print report [End.]

*-- Filter Temporary Account File [Begin]

*-- if user want balance debits only.
IF lcRpBalanc = 'D'
  lcAcctFilt = IIF(EMPTY(lcAcctFilt) , '' , lcAcctFilt + [ AND ]) + [nNetBal >= lnRpBalanc]
ENDIF  && end if user want balance debits only.

*-- if you have totals filter.
IF !EMPTY(lcAcctFilt)

  SELECT (lcTempAcc)
  SET FILTER TO &lcAcctFilt

  GO TOP
  *-- Asking if no records (Display message) otherwise print report [Begin.]
  IF EOF()
    *---Text : 'No Record Selected for the report..!'
    =gfModalGen('TRM00052B00000','DIALOG')
    RETURN
  ENDIF
  *-- Asking if no records (Display message) otherwise print report [End.]

ENDIF  && end if you have totals filter
*-- Filter Temporary Account File [End  ]

*=================================  Begin ===============================*
*==========================  Have data section ==========================*
*========================================================================*
SELECT (lcTempAge)
*-- Reindexing file if user change sort by [Begin.]
IF llChSortBy
  llChSortBy = .F.  && Avoid Replacing Key field with proper sort by values again.
  REPLACE ALL cTempKey WITH EVALUATE(lcReplExpr) && Replace key field with new values.
ENDIF
*-- Reindexing file if user change sort by [End.]

*-- if print short form.
IF lcRpReport = 'H'
  *-- Create Another alias form the same line descending order to print total line 
  *-- from withen detail band of form, and this technique have the ability to print
  *-- both transaction lines and post dated cheques from one line in this band, avoiding
  *-- waste of papers.
  IF !USED(lcTempRev)
    DO lpAnothAls
  ENDIF
  
  *-- Set relation with reverse file.
  SELECT (lcTempAge)
  SET RELATION TO EVALUATE(lcReplExpr) INTO (lcTempRev)
  
  *-- lcTermData : Printed in short form if line figure transaction totals
  *-- lcPostText : Printed in short form if line figure Post dated totals
  *-- lcCurrSec  : Printed in short form currency section.

  *-- if multi currency company.
  IF llMultCur
    lcCurrSec = [cCurrCode + '-' + IIF(SEEK(cCurrCode,'SycCurr'),PADR(SycCurr.CCURRDESC,9),SPACE(9))]

  ELSE  && else single currency company.
  
    lcCurrSec = [PADR(TRANSFORM(IIF(EMPTY(CUSTOMER.PHONE2), ]      +;
                [CUSTOMER.PHONE1,CUSTOMER.PHONE2),gfPhoneTem()),13)]
  ENDIF  && end if multi currency company.

  *C101568,1 Modify lcTermData which appears in Short Format by removing SalesRep Code
  *C101568,1 and payment terms for this Custom ARAGING [Begin.]      
  *lcTermData = [IIF(RECNO() <> RECNO(lcTempRev) OR ]                      +;
  *             [(lnGrpAge=0 AND lnGrpCre=0),'',]                          +;
  *             [PADR(CUSTOMER.BTNAME,10) + ' ' + ]                        +;
  *             lcCurrSec + [ + ' ' +]                                     +;
  *             [Customer.SALESREP + ' ' + ]                               +;
  *             [PADR(gfCoddes(cTermCode,'CTERMCODE',.T.),25))]

  lcTermData = [IIF(RECNO() <> RECNO(lcTempRev) OR ]                      +;
               [(lnGrpAge=0 AND lnGrpCre=0),'',]                          +;
               [PADR(CUSTOMER.BTNAME,10) + ' ' + ]                        +;
               lcCurrSec+ [)]
               
  *C101568,1 End of removing SalesRep Code and payment terms for this Custom ARAGING [End.]      
 
  lcPostText = [IIF(RECNO() <> RECNO(lcTempRev) OR lnGrpPost=0 ,'',]      +;
               [PADR(CUSTOMER.BTNAME,10) + ]                              +;
               [',   Totals Post Dated Cheques = ' + ]                    +;
               [TRANSFORM(lnGrpPost,"99999999.99"))]

ELSE  && else print Detail or Summary forms.

  *-- Prepair to deal with Age Credits (Yes / No)
  llPrnCrdDt = llRpCrdDet AND llRPAgeCrd     && Print credit detail and age credits.

ENDIF  && end if print short form.

*-- Set relation between Temporary file and customer files.
SELECT (lcTempAge)
SET RELATION TO 'M' + ACCOUNT INTO CUSTOMER ,;
                      ACCOUNT INTO (lcTempAcc) ADDITIVE

*-- Evaluate Aging headers, (Report aging titles). [Begin]
*-- If Age by date
IF lcAgeType = 'D'

  lcAgeHd30  = 'Over ' + ALLTRIM(STR(lnRPDay1))
  lcAgeHd60  = 'Over ' + ALLTRIM(STR(lnRPDay2))
  lcAgeHd90  = 'Over ' + ALLTRIM(STR(lnRPDay3))
  lcAgeHd120 = 'Over ' + ALLTRIM(STR(lnRPDay4))

ELSE  && Else Age by terms

  lcAgeHd30  = '1 - ' + ALLTRIM(STR(lnRPDay1))
  lcAgeHd60  = ALLTRIM(STR(lnRPDay1+1)) + ' - ' + ALLTRIM(STR(lnRPDay2))
  lcAgeHd90  = ALLTRIM(STR(lnRPDay2+1)) + ' - ' + ALLTRIM(STR(lnRPDay3))
  lcAgeHd120 = 'Over ' + ALLTRIM(STR(lnRPDay3+1))

ENDIF  && end If Age by date.
*-- Evaluate Aging headers, (Report aging titles). [End  ]

*-- Calculate spent time in collecting data.
lcEdTime = TIME()  && Time in which we finish collect data.
lnInterval = lfCollTime(lcStTime,lcEdTime)  && Calculate collecting data spent time.
WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT(lcTempAcc))) + ' Customer(s), ' + ALLTRIM(STR(RECCOUNT())) + ' Transaction(s) in ' + ALLTRIM(STR(lnInterval,6,2)) + ' Seconds...' NOWAIT

*========================================================================*
*==========================  Have data section ==========================*
*=================================   End  ===============================*

*-- Print Code
GO TOP  && Refresh relation
IF EMPTY(lcAcctFilt)
  DO gfDispRe WITH EVALUATE('lcRpName')  && No totals filter.
ELSE
  DO gfDispRe WITH EVALUATE('lcRpName'), 'FOR !EOF(lcTempAcc)'  && Have totals filter.
ENDIF  

WAIT CLEAR

*-- Release relation.
SET RELATION TO

*-- Close reverse file (if print short form only)
IF USED(lcTempRev)
  USE IN (lcTempRev)
ENDIF
*-- end of report code.
*E301214,1 New Program code instead of last one [End  ]
*========================================================================*
*=========================  Report Program Code =========================*
*=================================   End  ===============================*


*========================================================================*
*==========================  Functions Section ==========================*
*========================================================================*
*!*************************************************************
*! Name      : lpAnothAls
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 02/05/1999
*! Purpose   : Open temporary cursor in another alias with reverse sorting 
*!           : which help us detect last line in each group with out using 
*!           : .FRX group to gain suppress line if blank which work only in 
*!           : .FRX detail band, also print Transaction line and post dated line
*!           : from the same .Frx detail line which save paper space.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code section.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example   : DO lpAnothAls
*!*************************************************************
*!Due to E301214,1
*
PROCEDURE lpAnothAls
PRIVATE lcFullPath , lcCurDir
lcFullPath = SET('FULLPATH')
SET FULLPATH ON 
lcCurDir   = DBF()  && Get Cursor full path.
SET FULLPATH &lcFullPath
  
USE (lcCurDir) IN 0 AGAIN ALIAS (lcTempRev)  && Use the same file in another alias
SELECT (lcTempRev)
SET ORDER TO TAG (lcTempRev)  && Set Descending order.
*-- end of lpAnothAls.

*!*************************************************************
*! Name      : lfvAcct
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : Validate the acount from the option grid
*!*************************************************************
*! Example     : = lfvAcct()
*!*************************************************************
FUNCTION lfvAcct
PRIVATE lcObjNam , lcObjVal , llObjRet

lcObjNam = SYS(18)                && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field

*-- IF The user want to Browse or if the Account he entered is not in the file
IF !EMPTY(lcObjVal) .AND. !SEEK('M'+lcObjVal , 'CUSTOMER')
  llBrowse = .T.
  xAccount = lcObjVal
  DO CUSBROWM WITH xAccount
  lcObjVal = xAccount
  llBrowse = .F.
ENDIF    && End of IF
&lcObjNam = lcObjVal
*-- end of lfvAcct.

*!*************************************************************
*! Name      : lfwGrid
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : Called from the when function of the option grid
*!*************************************************************
*! Example     : = lfwGrid()
*!*************************************************************
*!Due to E301214,1 Create arrays hold temporary files structures, then create them.
*
FUNCTION lfwGrid
*-- if first time run report [Begin]
*-- lcBuild is defined in reports generator as numeric field and after first run gone to
*-- be character.
IF TYPE('lcBuild') = 'N'
  lcBuild = 'OK'
  lcPrtFac = 'FACTORED AND NON FACTORED INVOICES'

  =lfFilTrnAr()  && Fill arrays then create files.

ENDIF  

*-- Disable Age Credits if user does not want to include credit detail [Begin]
= !llRpCrdDet AND lfvIncDet()
*-- Disable Age Credits if user does not want to include credit detail [End  ]
*-- if first time run report [End  ]
*-- End of lfvReport.

*!*************************************************************
*! Name      : lfvInv
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : Called from the option grid to set the variable
*!             of factored invoices
*!*************************************************************
*! Example     : = lfvInv()
*!*************************************************************
FUNCTION lfvInv
lcRPSpeFac = IIF((lcRPFactor = 'F'),lcRPSpeFac,SPACE(05))
CLEAR READ
*-- End of lfvInv.

*!*************************************************************
*! Name      : lfvFactor
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : to validate a specific factor
*!*************************************************************
*! Example     : = lfvFactor()
*!*************************************************************
FUNCTION lfvFactor
PRIVATE lnAlias

lnAlias = SELECT()
SELECT CUSTOMER
IF !EMPTY(lcRPSpeFac) .AND. !SEEK ('F'+lcRPSpeFac)
  llNoThing = lfGetFac (lcRPSpeFac)
ENDIF
SELECT(lnAlias)
*-- End of lfvFactor.

*!*************************************************************
*! Name      : lfvSalRep
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : to validate the salesrep field from the option grid
*!*************************************************************
*! Example     : = lfvSalRep()
*!*************************************************************
FUNCTION lfvSalRep
PRIVATE lcObjNam , lcObjVal , llObjRet

SET ORDER TO TAG SALESREP IN SALESREP
lcObjNam = SYS(18)                && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field

*-- IF The user want to Browse or if the Account he entered is not in the file
IF !EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'SALESREP')
  llBrowse = .T.
  xAccount = lcObjVal
  DO REPCHK WITH xAccount
  lcObjVal = xAccount
  llBrowse = .F.
ENDIF    && End of IF
&lcObjNam = lcObjVal
*-- End of lfvSalRep.

*!*************************************************************
*! Name      : lfvDay1
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : to validate the number of days for the 1st bucket
*!*************************************************************
*! Example     : = lfvDay1()
*!*************************************************************
FUNCTION lfvDay1

IF lnRPDay1 <= 0
  =gfModalGen('TRM40132B40011','ALERT')
  lnRPDay1 = lnOldDays
ENDIF
IF lnRPDay1 >= lnRPDay2
  =gfModalGen('TRM40133B40011','ALERT','2nd|1st')
  lnRPDay1 = lnOldDays
ENDIF
*-- End of lfvDay1.

*!*************************************************************
*! Name      : lfwDays
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : get the old value of days buckets in a variable
*!*************************************************************
*! Example     : = lfwDays()
*!*************************************************************
FUNCTION lfwDays
lnOldDays = EVALUATE(SYS(18))
*-- End of lfwDays.

*!*************************************************************
*! Name      : lfvDay2
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : to validate the number of days for the 2nd bucket
*!*************************************************************
*! Example     : = lfvDay2()
*!*************************************************************
FUNCTION lfvDay2

IF lnRPDay2 <= lnRPDay1
  =gfModalGen('TRM40133B40011','ALERT','2nd|1st')
  lnRPDay2 = lnOldDays
ENDIF
IF lnRPDay2 >= lnRPDay3
  =gfModalGen('TRM40133B40011','ALERT','3rd|2nd')
  lnRPDay2 = lnOldDays
ENDIF
*-- End of lfvDay2.

*!*************************************************************
*! Name      : lfvDay3
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : to validate the number of days for the 3rd bucket
*!*************************************************************
*! Example     : = lfvDay3()
*!*************************************************************
FUNCTION lfvDay3

IF lnRPDay3 <= lnRPDay2
  =gfModalGen('TRM40133B40011','ALERT','3rd|2nd')
  lnRPDay3 = lnOldDays
ENDIF
IF lnRPDay3 >= lnRPDay4
  =gfModalGen('TRM40133B40011','ALERT','3rd|2nd')
  lnRPDay3 = lnOldDays
ENDIF
*-- End of lfvDay3.

*!*************************************************************
*! Name      : lfvDay4
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : to validate the number of days for the 4th bucket
*!*************************************************************
*! Example     : = lfvDay4()
*!*************************************************************
FUNCTION lfvDay4

IF lnRPDay4 <= lnRPDay3
  =gfModalGen('TRM40133B40011','ALERT','4th|3rd')
  lnRPDay4 = lnOldDays
ENDIF
*-- End of lfvDay4.

*!*************************************************************
*! Name      : lfvCurDisp
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : get the description of Curr.
*!*************************************************************
*! Example     : = lfvCurDisp()
*!*************************************************************
FUNCTION lfvCurDisp
=gfRepCur(.T., @lcRpCurr,@ldRpExDate ,lcRpTmpNam)
*-- end of lfvCurDisp.

*!*************************************************************
*! Name      : lfDefCurr
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/11/1998
*! Purpose   : Return Default currency value.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : currency default value.
*!*************************************************************
*! Example     : = lfDefCurr()
*!*************************************************************
*!B801811,1 MAB
*!
FUNCTION lfDefCurr
RETURN IIF(llMultCur,'F','O')
*-- end of lfDefCurr.

*!*************************************************************
*! Name      : lfFillSort
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/11/1998
*! Purpose   : Fill sort by arrays.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : = lfFillSort()
*!*************************************************************
*!B801811,1 MAB
*!
FUNCTION lfFillSort
PRIVATE lnRows
lnRows = IIF(llMultCur,7,6)
DIMENSION laSortDesc[lnRows,1],laSortVal[lnRows,1]

laSortDesc[1,1] = 'Account'
laSortDesc[2,1] = 'Terms'
laSortDesc[3,1] = 'Salesrep'
laSortDesc[4,1] = 'Division'
laSortDesc[5,1] = 'Region'
laSortDesc[6,1] = 'Country'

laSortVal[1,1] = 'A'
laSortVal[2,1] = 'T'
laSortVal[3,1] = 'S'
laSortVal[4,1] = 'D'
laSortVal[5,1] = 'R'
laSortVal[6,1] = 'U'

*-- if company support multi currency add Currency to Sort By POPUP.
IF llMultCur
  =AINS(laSortDesc,5)
  =AINS(laSortVal,5)
  laSortDesc[5,1] = 'Currency'
  laSortVal[5,1]  = 'C'
ENDIF  && end if company support multi currency add Currency to Sort By POPUP.
*-- end of lfFillSort.

*!*************************************************************
*! Name      : lfFilTrnAr
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 02/05/1999
*! Purpose   : Create temporary cursor structure.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfWorkFile
*!*************************************************************
*! Called from : OG When function.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example   : =lfFilTrnAr()
*!*************************************************************
*!Due to E301214,1
*
FUNCTION lfFilTrnAr
PRIVATE laTempTran , laTempCust , lcExcStat
lcExcStat = SET('EXACT')
SET EXACT ON

*-- laTempStru : Array hold structure of temporary transaction + Customer fields.
*-- laTempTran : Array hold structure of transaction fields.
*-- laTempCust : Array hold structure of Customer fields.
DIMENSION laTempStru[36,4] , laTempTran[1,4] , laTempCust[1,4]
STORE '' TO laTempStru,laTempTran
PRIVATE lnFileCnt , lnFldRow

*-- Fields from Customer File.
SELECT CUSTOMER
= AFIELDS(laTempCust)
laTempStru[1,1]  = 'ACCOUNT'
laTempStru[2,1]  = 'CFACCODE'
laTempStru[3,1]  = 'SALESREP'
laTempStru[4,1]  = 'CURRENT'
laTempStru[5,1]  = 'AGE30'
laTempStru[6,1]  = 'AGE60'
laTempStru[7,1]  = 'AGE90'
laTempStru[8,1]  = 'AGE120'
laTempStru[9,1]  = 'TOTAGE'
laTempStru[10,1] = 'OPENCR'
laTempStru[11,1] = 'CHGBACK'
laTempStru[12,1] = 'NETBAL'
laTempStru[13,1] = 'REGION'
laTempStru[14,1] = 'CTERMCODE'
laTempStru[15,1] = 'CDIVISION'
laTempStru[16,1] = 'CCURRCODE'
laTempStru[17,1] = 'CCONT_CODE'

*-- Loop to get other dimensions of Customer included fields (Like master file)
FOR lnFileCnt = 1 TO 17
  lnFldRow = ASCAN(laTempCust,laTempStru[lnFileCnt,1])
  IF lnFldRow > 0
    lnFldRow = ASUBSCRIPT(laTempCust,lnFldRow,1)
    laTempStru[lnFileCnt , 2 ] = laTempCust[lnFldRow , 2 ]
    laTempStru[lnFileCnt , 3 ] = laTempCust[lnFldRow , 3 ]
    laTempStru[lnFileCnt , 4 ] = laTempCust[lnFldRow , 4 ]
  ENDIF
ENDFOR  && end Loop to get other dimensions of Customer included fields (Like master file)

*-- Fields from Debit , Credit , and History Files
SELECT ARHIST
= AFIELDS(laTempTran)
laTempStru[18,1] = 'TRANTYPE'
laTempStru[19,1] = 'TRANCODE'
laTempStru[20,1] = 'TRAN'
laTempStru[21,1] = 'BATCH'
laTempStru[22,1] = 'CINSTALNO'
laTempStru[23,1] = 'TRANDATE'
laTempStru[24,1] = 'DPOSTDATE'
laTempStru[25,1] = 'CHGBK_DATE'
laTempStru[26,1] = 'DESC'
laTempStru[27,1] = 'REFERENCE'
laTempStru[28,1] = 'AMOUNT'
laTempStru[29,1] = 'DUEDATE'
laTempStru[30,1] = 'CADJACCT'
laTempStru[31,1] = 'CARGLACC'
laTempStru[32,1] = 'NCURRUNIT'
laTempStru[33,1] = 'NEXRATE'

*-- Loop to get other dimensions of transaction included fields (Like master file)
FOR lnFileCnt = 18 TO 33
  lnFldRow = ASCAN(laTempTran,laTempStru[lnFileCnt,1])
  IF lnFldRow > 0
    lnFldRow = ASUBSCRIPT(laTempTran,lnFldRow,1)
    laTempStru[lnFileCnt , 2 ] = laTempTran[lnFldRow , 2 ]
    laTempStru[lnFileCnt , 3 ] = laTempTran[lnFldRow , 3 ]
    laTempStru[lnFileCnt , 4 ] = laTempTran[lnFldRow , 4 ]
  ENDIF
ENDFOR  && end Loop to get other dimensions of transaction included fields (Like master file)

*-- Add Fields from PostDchq File.
*-- cTranType field : Added to indicate that post dated if it's "P" else normal transaction.
laTempStru[34 ,1] = 'CTRANTYPE'
laTempStru[34 ,2] = 'C'
laTempStru[34 ,3] = 1
laTempStru[34 ,4] = 0

laTempStru[35 ,1] = 'CHEQUENO'
laTempStru[35 ,2] = 'C'
laTempStru[35 ,3] = 10
laTempStru[35 ,4] = 0

*-- Temporary Key Field.
laTempStru[36 ,1] = 'CTEMPKEY'
laTempStru[36 ,2] = 'C'
laTempStru[36 ,3] = 15
laTempStru[36 ,4] = 0

SET EXACT &lcExcStat  && Restore exact setting.

=lfWorkFile()  && Create temporary files.
*-- end of lfFilTrnAr.

*!*************************************************************
*! Name      : lfWorkFile
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 02/05/99
*! Purpose   : Create work cursors.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfOpenFile
*!*************************************************************
*! Called from : lfFilTrnAr , Report code.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfWorkFile()
*!*************************************************************
*
FUNCTION lfWorkFile

*-- if company use multi currency.
IF llMultCur

  *-- Open Currency file.
  IF !USED('SYCCURR')
    =gfOpenFile(gcSysHome+'SYCCURR',gcSysHome+'Ccurrcode','SH')
  ENDIF  

ELSE  && else company use single currency format.
  
  *-- Open Post dated cheques file.
  IF !USED('POSTDCHQ')
    USE (gcDataDir+'POSTDCHQ') ORDER TAG POSTDCHQ IN 0 SHARED
  ENDIF

ENDIF  && end if company use multi currency.

SET ORDER TO CUSTOMER IN CUSTOMER
SET ORDER TO TAG Ccontcode IN SYCINT

*-- First Close all temporary files if it was opened before [Begin]
IF USED(lcTempAge)
  USE IN (lcTempAge)
ENDIF

IF USED(lcTempRev)
  USE IN (lcTempRev)
ENDIF

IF USED(lcTempAcc)
  USE IN (lcTempAcc)
ENDIF
*-- First Close all temporary files if it was opened before [End  ]

*-- Create Totals file [Begin]
*-- nAges   : Numeric Field Hold Totals for Age30 + Age60 + Age90 + Age120 (Past Due Ages only)
*-- nNetBal : Numeric Field Hold Net balance for account in base currency.
CREATE CURSOR (lcTempAcc) (Account C(5) , nAges N(11,2) , nNetBal N(11,2))
SELECT (lcTempAcc)

*-- Fix Cursor bug [Begin]
ZAP
*-- Fix Cursor bug [End  ]
INDEX ON ACCOUNT TAG (lcTempAcc) OF (gcWorkDir+lcTempAcc+'.CDX')
*-- Create Totals file [End  ]

*-- Create transaction lines file [Begin]
CREATE CURSOR (lcTempAge) ;
   FROM ARRAY laTempStru
SELECT (lcTempAge)

*-- Fix Cursor bug [Begin]
ZAP
*-- Fix Cursor bug [End  ]

*-- First  index used in reverse sorting in short form to detect end of group .
*-- Second index used in printing according sort by .
INDEX ON cTempKey TAG (lcTempRev) OF (gcWorkDir+lcTempAge+'.CDX') DESCENDING
INDEX ON cTempKey TAG (lcTempAge) OF (gcWorkDir+lcTempAge+'.CDX') 
*-- Create transaction lines file [End  ]
*-- end of lfWorkFile.

*!*************************************************************
*! Name      : lfvSortBy
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 04/05/99
*! Purpose   : change index flag to reindex temp cursor.
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
*! Example   : =lfvSortBy()
*!*************************************************************
FUNCTION lfvSortBy
llChSortBy = .T.  && Detect that user change sorting method.
*-- end of lfvSortBy.

*!*************************************************************
*! Name      : lfGetReplc
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 06/04/99
*! Purpose   : Get Replaced expression.(According to sort by options)
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code section.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : lcExpr ---> which means (Sort by expression) 
*!*************************************************************
*! Example   : =lfGetReplc()
*!*************************************************************
*
FUNCTION lfGetReplc
PRIVATE lcExpr
DO CASE
  *-- Sort by account
  CASE lcRpAgSort = 'A'
    lcGroup  = []  && Used in short form only.
    lcInnGrp = [Account]
    lcExpr   = [ACCOUNT + cTranType]
    lcSorted = 'Account'
    
  *-- Sort by Terms
    CASE lcRpAgSort = 'T'
    lcGroup  = [cTermCode]
    lcInnGrp = [cTermCode + Account]
    lcCodeDesc = [gfCoddes(cTermCode,'CTERMCODE',.T.) + ' Totals ==>']
    lcExpr   = [CTERMCODE + Account + cTranType]
    lcSorted = 'Term Code'
    
  *-- Sort by division
  CASE lcRpAgSort = 'D'
    lcGroup  = [CDIVISION]
    lcInnGrp = [CDIVISION + Account]
    lcCodeDesc = [gfCoddes(CDIVISION,'CDIVISION',.T.) + ' Totals ==>']
    lcExpr   = [CDIVISION + Account + cTranType]
    lcSorted = 'Division'
    
  *-- Sort by currency
  CASE lcRpAgSort = 'C'
    lcGroup  = [cCurrCode]
    lcInnGrp = [cCurrCode + Account]
    lcCodeDesc = [cCurrCode + '-' + IIF(SEEK(cCurrCode,'SYCCURR'),SYCCURR.CCURRDESC,SPACE(30)) + ' Totals ==>']
    lcExpr   = [CCURRCODE + Account + cTranType]
    lcSorted = 'Currency'
    
  *-- Sort by sales rep.
  CASE lcRpAgSort = 'S'
    lcGroup  = [Salesrep]
    lcInnGrp = [Salesrep + Account]
    lcCodeDesc = [IIF(SEEK(Salesrep,'Salesrep'),Salesrep.Name,SPACE(24)) + ' Totals ==>']
    lcExpr   = [Salesrep + Account + cTranType]
    lcSorted = 'Sales Representative'

  *-- Sort by region
  CASE lcRpAgSort = 'R'
    lcGroup  = [Region]
    lcInnGrp = [Region + Account]
    lcCodeDesc = [gfCoddes(REGION,'REGION',.T.) + ' Totals ==>']
    lcExpr   = [Region + Account + cTranType]
    lcSorted = 'Region'
    
  *-- Sort by country.
  CASE lcRpAgSort = 'U'
    lcGroup  = [cCont_Code]
    lcInnGrp = [cCont_Code + Account]
    lcCodeDesc = [cCont_Code + '-' + IIF(SEEK(cCont_Code,'SYCINT'),SYCINT.CCONT_DESC,SPACE(30)) + ' Totals ==>']
    lcExpr   = [cCont_Code + Account + cTranType]
    lcSorted = 'Country'

ENDCASE

*-- if multi currency and print in foreign and not sort by currency.
*-- include currency in your groups, and index filter.
IF llCurrGrp AND (LEFT(lcExpr,9) != 'CCURRCODE')
  lcExpr   = lcExpr   + [ + CCURRCODE]
  lcGroup  = lcGroup  + [ + CCURRCODE]
  lcInnGrp = lcInnGrp + [ + CCURRCODE]
ENDIF  && end if multi currency and print in foreign and not sort by currency.

RETURN lcExpr
*-- end of lfGetReplc

*!*************************************************************
*! Name      : lfCollTime
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 06/04/99
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
*
FUNCTION lfCollTime
PARAMETERS lcStart,lcEnd
lnStHour  = IIF(VAL(LEFT(lcStart,2)) = 0,VAL(LEFT(lcStart,2))+24,VAL(LEFT(lcStart,2)))
lnEndHour = IIF(VAL(LEFT(lcEnd,2))   = 0,VAL(LEFT(lcEnd,2))  +24,VAL(LEFT(lcEnd,2)))
lnStart = 3600 * lnStHour  + 60 * VAL(SUBSTR(lcStart,4,2)) + VAL(RIGHT(lcStart,2))
lnEnd   = 3600 * lnEndHour + 60 * VAL(SUBSTR(lcEnd,4,2))   + VAL(RIGHT(lcEnd,2))
RETURN (lnEnd - lnStart)
*-- end of lfCollTime.

*!*************************************************************
*! Name      : lfCollect
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 02/05/1999
*! Purpose   : Collect new data in two temporary cursor One is Transaction cursor
*!           : and another if for totals in base currency to handle due past only
*!           : and Over/Under Filters...
*!           : Note, Technique used here is to scan transcation files to get there transcations
*!           :       If achive customer filter in customer file.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfInsPstCh , lfInsDebit , lfInsCredt , lfInsHisto
*!*************************************************************
*! Called from : Report code section.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example   : DO lfCollect
*!*************************************************************
*
FUNCTION lfCollect
*-- Define collect variables [Begin]
*-- lcFacFlt : Variable Hold Factor/Non-Factor/Both Filter.
*-- lcDebFlt : Variable Hold Debit main Filter.
*-- lcCreFlt : Variable Hold Credit main Filter.
*-- lcHstFlt : Variable Hold History main Filter.
PRIVATE lcDebFlt, lcCreFlt, lcHstFlt , lcFacFlt, lcGlAccFlt
STORE '' TO lcDebFlt, lcCreFlt, lcHstFlt, lcFacFlt
*-- Define collect variables [End  ]

*-- if print Factored only
IF lcRpFactor = 'F'

  lcFacFlt = IIF(EMPTY(lcRPSpeFac),[!EMPTY(cFacCode)],[cFacCode=lcRPSpeFac])

  lcDebFlt = [TRANDATE<=ldRpEndDat AND ]                          +;
             [IIF( TRANTYPE='3',CHGBK_DATE<=ldRpEndDat,.T.)]

  lcCreFlt = [TRANDATE<=ldRPEndDat AND ]                          +;
             [IIF( TRANTYPE='6',CREDT_DATE<=ldRPEndDat,.T.)]

ELSE && Factored (and/or) non Factored

  IF lcRpFactor='N'
    lcFacFlt = [EMPTY(cFacCode)]
  ENDIF

  lcDebFlt = [IIF( TRANTYPE='3',]                                +; 
             [TRANDATE<=ldRpEndDat AND CHGBK_DATE<=ldRpEndDat ,] +;
             [TRANDATE<=ldRpEndDat )]

  lcCreFlt = [IIF( TRANTYPE='6',]                                +;
             [TRANDATE<=ldRPEndDat AND CREDT_DATE<=ldRPEndDat ,] +;
             [TRANDATE<=ldRPEndDat )]

ENDIF  && end if print Factored only.

lcHstFlt = [HISTDATE > ldRPEndDat AND TRANDATE <= ldRPEndDat]  && History (Keyed) filter

*-- Add Factor Filter to Main Files Filter.
IF !EMPTY(lcFacFlt)
  lcDebFlt = lcDebFlt + [ AND ] + lcFacFlt
  lcCreFlt = lcCreFlt + [ AND ] + lcFacFlt
  lcHstFlt = lcHstFlt + [ AND ] + lcFacFlt
ENDIF  

*-- Add GlAccount Filter to Main Files Filter.
IF !EMPTY(STRTRAN(lcRpGlAcc,'-',''))
  lcDebFlt = lcDebFlt + [ AND cArGlAcc = lcRpGlAcc]
  lcCreFlt = lcCreFlt + [ AND cArGlAcc = lcRpGlAcc]
  lcHstFlt = lcHstFlt + [ AND cArGlAcc = lcRpGlAcc]
ENDIF

*-- Declare Customer file memory variables [Begin]
SELECT CUSTOMER 
GO TOP
SCATTER MEMVAR MEMO BLANK  && Define aging variables
*-- Declare Customer file memory variables [End  ]

*-- insert post date cheques Transactions if not multi currency company.
IF !llMultCur
  =lfInsPstCh()
ENDIF  

*-- Insert Debit Transactions ...
=lfInsDebit()

*-- Insert Credit Transactions ...
=lfInsCredt()

*-- If user want to include key off transactions.
IF llRpKeyOff
  *-- Insert keyed Transactions ...
  =lfInsHisto()
ENDIF  
*-- end of lfCollect.

*!*************************************************************
*! Name      : lfInsPstCh
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 02/05/1999
*! Purpose   : Insert post dated cheques records to Transaction File.
*!*************************************************************
*! Calls     : 
*!             Procedures : lpInsRecrd
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code section.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example   : =lfInsPstCh()
*!*************************************************************
*
FUNCTION lfInsPstCh
*-- Set relation with customer file.
SELECT POSTDCHQ
SET RELATION TO 'M' + ACCOUNT INTO CUSTOMER  && Relation with customer.

*-- Scan All post dated checks achieve customer filter.
SCAN FOR account+DTOS(paydate) = '' AND CUSTOMER.TYPE = 'M' AND &lcRpExp AND ;
         Amount <> 0

  SCATTER MEMVAR
  m.TranDate  = POSTDCHQ.PAYDATE       && check pay date
  m.Desc      = "PostDated Checks"     && Description
  m.TranType  = ' '                    && Empty transaction
  m.cTranType = 'P'  && This field Added to indicate that post dated if it's "P" else normal transaction.
  
  DO lpInsRecrd  && Insert New Record in Transaction Temporary File.

ENDSCAN  && end Scan All post dated checks achieve customer filter.

SET RELATION TO  && Rest relation.
*-- end of lfInsPstCh.

*!*************************************************************
*! Name      : lfInsDebit
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 02/05/1999
*! Purpose   : Insert Debit records to Transaction and total Files.
*!*************************************************************
*! Calls     : 
*!             Procedures : lpDebDeal , lpInsRecrd
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code section.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example   : =lfInsDebit()
*!*************************************************************
*
FUNCTION lfInsDebit
*-- Set relation with customer file.
SELECT DEBIT
SET RELATION TO 'M' + ACCOUNT INTO CUSTOMER  && Relation with customer.


*-- Scan Debit records achieve both customer and debit filters.
SCAN FOR account+tran+cinstalno+DTOS(trandate) = '' AND &lcDebFlt AND ;
         CUSTOMER.TYPE = 'M' AND &lcRpExp
  
  *-- if user does not want to include charge back transaction and it is Charge back.
  IF (!llRPPrnCh .AND. TranType = '3' )
    LOOP
  ENDIF

  SCATTER MEMVAR
  DO lpDebDeal   && Calculate Ages and fill memory variables (Totage and NetBal)
  DO lpInsRecrd  && Insert New Record in Transaction Temporary File and Totals Temporary file.

ENDSCAN  && end Scan Debit records achieve both customer and debit filters.

SET RELATION TO  && Rest relation.
*-- end of lfInsDebit.

*!*************************************************************
*! Name      : lfInsCredt
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 02/05/1999
*! Purpose   : Insert Credit records to Transaction and total Files.
*!*************************************************************
*! Calls     : 
*!             Procedures : lpCreDeal , lpInsRecrd
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code section.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example   : =lfInsCredt()
*!*************************************************************
*
FUNCTION lfInsCredt
*-- Set relation with customer file.
SELECT CREDIT
SET RELATION TO 'M' + ACCOUNT INTO CUSTOMER  && Relation with customer.

*-- Scan Credit records achieve both customer and Credit filters.
SCAN FOR account+tran+DTOS(trandate) = '' AND &lcCreFlt AND ;
         CUSTOMER.TYPE = 'M' AND &lcRpExp

  SCATTER MEMVAR
  DO lpCreDeal   && Calculate Ages and fill memory variables (OpenCr and NetBal)
  DO lpInsRecrd  && Insert New Record in Transaction Temporary File and Totals Temporary file.


ENDSCAN  && end Scan Credit records achieve both customer and Credit filters.

SET RELATION TO  && Rest relation.

*-- end of lfInsCredt.

*!*************************************************************
*! Name      : lfInsHisto
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 02/05/1999
*! Purpose   : Insert History records to Transaction and total Files.
*!*************************************************************
*! Calls     : 
*!             Procedures : lpDebDeal , lpCreDeal , lpInsRecrd
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code section.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example   : =lfInsHisto()
*!*************************************************************
*
FUNCTION lfInsHisto
*-- Set relation with customer file.
SELECT ARHIST
SET RELATION TO 'M' + ACCOUNT INTO CUSTOMER  && Relation with customer.

*-- Scan History records achieve both customer and History filters.
SCAN FOR account+tran+cinstalno = '' AND &lcHstFlt AND ;
         CUSTOMER.TYPE = 'M' AND &lcRpExp
         
  *-- IF user does not want to include charge back transaction and it is Charge back (Type=3),
  *-- OR charge back date greater than ending period OR it is credit on account transaction
  *-- (Type=6) AND credit date greater than ending period, then
  *-- Cancel this transaction [i.e. LOOP it]
  IF (!llRPPrnCh .AND. TranType = '3' )            OR ;
     (TRANTYPE = '3' .AND. CHGBK_DATE>ldRPEndDat)  OR ;
     (TRANTYPE = '6' .AND. CREDT_DATE>ldRPEndDat)  OR ;
     !(TRANTYPE $ "0123456")
    LOOP
  ENDIF

  SCATTER MEMVAR

  *====> Debit Transaction  <====*
  *-- if Invoice(Type=1), Debit Adjustment(Type=2) OR Charge back(Type=3) .
  IF TRANTYPE $ '123'
    DO lpDebDeal    && Calculate Ages and fill memory variables (Totage and NetBal)
  ENDIF

  *====> Credit Transaction  <====*
  *-- if Return Invoice(Type=0), Payment(Type=4), Credit Adjustment(Type=5)
  *-- , OR Credit on account(Type=6).
  IF TRANTYPE $ '0456')
    DO lpCreDeal   && Calculate Ages and fill memory variables (OpenCr and NetBal)
  ENDIF 

  DO lpInsRecrd  && Insert New Record in Transaction Temporary File and Totals Temporary file.

ENDSCAN  && end Scan History records achieve both customer and History filters.
SET RELATION TO  && Rest relation.
*-- end of lfInsHisto.

*!*************************************************************
*! Name      : lpDebDeal
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 02/05/1999
*! Purpose   : Age debits according to aging type (By Date OR By Terms) also fill memory variables.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfUpdtAge
*!*************************************************************
*! Called from : Report code section.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example   : DO lpDebDeal
*!*************************************************************
*
PROCEDURE lpDebDeal

*-- if Age By Date
IF lcAgeType = 'D'
  =lfUpdtAge('D',ldRpEndDat - TRANDATE)  && Update Date ages

ELSE  && else if Age by Terms

   ldDueDate = IIF(EMPTY(DUEDATE),TRANDATE+lnRPDay1,DUEDATE)
  =lfUpdtAge('D',ldRpEndDat - ldDueDate)  && Update Term ages
ENDIF  && end if Age By Date

*-- if charge back Charge back equal transaction amount
IF TRANTYPE= '3'
  m.ChgBack = Amount
ENDIF

STORE AMOUNT TO m.TotAge , m.NetBal  && Save total age and net balance per transaction

*-- if it is invoice and find it.
IF TRANTYPE = '1' AND SEEK(DEBIT.TRAN,'INVHDR')
  m.cDivision = INVHDR.CDIVISION  && invoice division.
ENDIF  
*-- end of lpDebDeal.

*!*************************************************************
*! Name      : lpCreDeal
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 02/05/1999
*! Purpose   : Age Credits also fill memory variables.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfUpdtAge
*!*************************************************************
*! Called from : Report code section.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example   : DO lpCreDeal
*!*************************************************************
*
PROCEDURE lpCreDeal
=lfUpdtAge('C',gdSysDate - TRANDATE) && Update Date ages
STORE AMOUNT TO m.OpenCr , m.NetBal  && Save open credit and net balance per transaction

*-- if return invoice and return merchandise module installed and find this return.
IF TRANTYPE = '0' AND ('RM' $ gcCmpModules) AND SEEK(CREDIT.TRAN,'RETHDR')
  m.cDivision = RETHDR.CDIVISION  && Return division.
ENDIF  
*-- end of lpCreDeal.

*!*************************************************************
*! Name      : lfUpdtAge
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 02/05/1999
*! Purpose   : Age Credits also fill memory variables.
*! Note      : I use Age fields (Current, Age30 , 60 , 90 , 120) to age both
*!           : by date or by terms to save disk usage and also simplify print
*!           : forms to print from one field instead of use command like 
*!           : IIF(lcAgeType='D',Age30,TerAge30) , and we must all know that two situations
*!           : does not occur at the same time.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : lpDebDeal , lpCreDeal
*!*************************************************************
*! Passed Parameters  : 1- Age Debits(D)/Credits(C)
*!                    : 2- Age Days
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example   : =lfUpdtAge()
*!*************************************************************
*
FUNCTION lfUpdtAge
PARAMETERS lcUpdtTyp , lnAgeDays

*-- Intializing variables that calculates data from transaction files. [begin]
STORE 0.00 TO m.Current , m.Age30 , m.Age60 , m.Age90 , m.Age120 ,;
              m.OpenCr,m.ChgBack , m.Totage , m.NetBal , m.Age00
STORE '' TO m.Region , m.cDivision , m.SalesRep , m.cTermCode , m.cPastOnly
*-- Intializing variables that calculates data from transaction files. [End  ]

*-- if no parameter passed (i.e. want to intializing only) then return
*-- does occur in this program but it may be.
IF TYPE('lcUpdtTyp') $ 'UL'
  RETURN
ENDIF

*-- lcAges : Varaible Hold Field description (Age00, 30 , 60 , 90 , and 120)
*-- if Debit / History
IF lcUpdtTyp = 'D'

  *-- if Age By Date.
  IF lcAgeType = 'D'
    lcAges = 'm.Age'                                                    +;
      IIF(lnAgeDays >= lnRpDay4,'120',IIF(lnAgeDays >= lnRpDay3 ,'90'   ,;
      IIF(lnAgeDays >= lnRpDay2 ,'60' ,IIF(lnAgeDays >= lnRpDay1 ,'30','00')))) 


  ELSE  && else if Age By Terms.

    lcAges = 'm.Age'                                                      +;
      IIF(lnAgeDays >= lnRpDay3+1,'120',IIF(lnAgeDays >= lnRpDay2+1 ,'90' ,;
      IIF(lnAgeDays >= lnRpDay1+1 ,'60' ,IIF(lnAgeDays >= 1 ,'30','00')))) 
  
  ENDIF  && end if Age By Date.

ELSE  && else if Credit / History

  lcAges = 'm.Age'                                                      +;
    IIF(lnAgeDays >= 120,'120',IIF(lnAgeDays >= 90 ,'90' ,;
    IIF(lnAgeDays >= 60 ,'60' ,IIF(lnAgeDays >= 30 ,'30','00')))) 


ENDIF  && end if Debit / History

&lcAges   = Amount       && Fill Age variable
m.Current = IIF(m.Age00 = 0 , 0 , m.Age00)  && because there is no Age00 field (it's Current)
*-- end of lfUpdtAge.

*!*************************************************************
*! Name      : lpInsRecrd
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 02/05/1999
*! Purpose   : Insert both transaction and total records.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfBaseVal
*!*************************************************************
*! Called from : lpDebDeal , lpCreDeal
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example   : DO lpInsRecrd
*!*************************************************************
*
PROCEDURE lpInsRecrd
PRIVATE lnPastAges , lnNetBal
STORE 0 TO lnPastAges , lnNetBal

*-- Evaluate Customer file memory variables. [Begin]
m.Region     = Customer.Region 
m.cTermCode  = Customer.cTermCode
m.cCont_Code = Customer.cCont_Code
m.SalesRep   = Customer.SalesRep
*-- Evaluate Customer file memory variables. [Begin]

INSERT INTO (lcTempAge) FROM MEMVAR         && Insert Transaction Record
SELECT (lcTempAge)
REPLACE cTempKey WITH EVALUATE(lcReplExpr)  && Replace key field with approparate value.

*-- if Debit, Credit, or History record only (Not Post dated checks record).
IF EMPTY(cTranType)

  *-- if Multi currency and currency does not equal base currency.
  IF llMultCur AND (cCurrCode <> gcBaseCurr)

    = lfBaseVal(@lnPastAges , @lnNetBal)  && Evaluate Past Ages and net balance in base currency.

  ELSE  && else if Single currency or currency equal base currency.

    lnPastAges = IIF(TotAge > 0 AND Current = 0,TotAge,0)  && Past age
    lnNetBal   = NetBal                                    && Net balance

  ENDIF  && end if Multi currency and currency does not equal base currency.

  *-- if Find this account in Temporary totals file.
  IF SEEK(Account,lcTempAcc)

    *-- Accomulate both past due ages and net balance in total file.
    REPLACE &lcTempAcc..nAges WITH   &lcTempAcc..nAges + lnPastAges ,;
            &lcTempAcc..nNetBal WITH &lcTempAcc..nNetBal + lnNetBal

  ELSE  && else if this account not found before.

    *-- Add new record in temporary total files.
    WAIT WINDOW 'Collect data for Customer ' + Account NOWAIT
    m.Account = Account
    m.nAges   = lnPastAges
    m.nNetBal = lnNetBal
    INSERT INTO (lcTempAcc) FROM MEMVAR  && insert new record in account totals file.
  
  ENDIF  && end if Find this account in Temporary totals file.

ENDIF  && end if Debit, Credit, or History record.
*-- end of lpInsRecrd.

*!*************************************************************
*! Name      : lfBaseVal
*! Developer : Mohamed Badran (MAB)
*! Date      : 02/17/98
*! Purpose   : Calculate Currency unit, rate, signs and Evaluate amount values
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfGetExSin,gfChkRate
*!*************************************************************
*! Passed Parameters  : 1- First  number (Due Ages Amount)
*! By Reference       : 2- Second number (Net Balance Amount)
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfBaseVal()
*!*************************************************************
*
FUNCTION lfBaseVal
PARAMETERS lnBaseVal1 , lnBaseVal2
PRIVATE lcAExRSin , lcAUntSin , lnCurrRate , lnCurrUnit

lcAUntSin   = '*'        && Variable to hold unit sign.
lcAExRSin   = '*'        && Variable to hold exchange rate sign.
lcAExRSin  = gfGetExSin(@lcAUntSin, cCurrCode)  && Currency sign.
lnCurrRate  = nExRate          && Variable to hold current rate
lnCurrUnit  = nCurrUnit        && Variable to hold current unit
lnBaseVal1 = IIF(TotAge > 0 AND Current = 0,TotAge,0) &lcAExRSin lnCurrRate &lcAUntSin lnCurrUnit
lnBaseVal2 = NetBal &lcAExRSin lnCurrRate &lcAUntSin lnCurrUnit
*-- end of lfBaseVal.

*!*************************************************************
*! Name      : lfClearRep
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 03/20/1999
*! Purpose   : Close any opened files if user press OG <Close> Button
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : = lfClearRep()
*!*************************************************************
*!
FUNCTION lfClearRep
*-- Rise llOGFltCh flag to recollect data next time preview or run because this fn called 
*-- if user press <Reset> or Clear Read.
llOGFltCh = .T.  

*-- Close post dated checks file.
IF USED('POSTDCHQ')
  USE IN POSTDCHQ
ENDIF

*-- Close Currency file.
IF USED('SYCCURR')
  USE IN SYCCURR
ENDIF

*-- Close Temporary Cursors [Begin]
IF USED(lcTempAge)
  USE IN (lcTempAge)
ENDIF

IF USED(lcTempRev)
  USE IN (lcTempRev)
ENDIF

IF USED(lcTempAcc)
  USE IN (lcTempAcc)
ENDIF
*-- Close Temporary Cursors [End  ]
*-- end of lfClearRep.