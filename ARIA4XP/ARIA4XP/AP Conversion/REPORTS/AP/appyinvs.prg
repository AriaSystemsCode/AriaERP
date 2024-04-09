*:***************************************************************************
*: Program file  : APPYINVS
*: Program desc. : Invoice Summary Report
*: System        : ARIA 4XP
*: Module        : Accounts Payable(AP)
*: Developer     : AHMED MOUSTAFA (AHS)
*: Date          : 07/29/2009                                                                                                                                                 
*:***************************************************************************
*E302938,3 TMI 08/04/2011 Fix problems to enable the report to run in A4xp with Fox tables
*E302975,1 AP Conv.Proj. Attaching all files the phase to TMI 10/23/2011 
*:************************************************************************

IF loOgScroll.llOGFltCh

  =lfvCrTemp()     && To create Temp File
  =lfvColData()    && Data Collection

ENDIF   

SELECT &lcTempFile
DO gfDispRe WITH EVAL('lcRpForm')

*!**************************************************************************
*! Function      : lfvColData
*! Purpose       : Collection of data 
*! Developer     : AHMED MOUSTAFA (AHS)     
*! Date          : 07/19/2009
*!**************************************************************************
FUNCTION lfvColData

lnVendPos = lfGetPos('APINVHDR.CVENDCODE','laOgVrFlt')
lnCurrPos = lfGetPos('APINVHDR.CCURRCODE','laOgFxFlt')
lnMethPos = lfGetPos('APINVHDR.CVENPMETH','laOgVrFlt')
lnDivPos = lfGetPos('APINVHDR.CDIVISION','laOgVrFlt')
lnPriorPos = lfGetPos('APINVHDR.CVENPRIOR','laOgVrFlt')
lcCurr = loOGScroll.laOGfxFlt[lnCurrPos,6]
lcVend = loOGScroll.laOGvrFlt[lnVendPos,6]
lcMeth = loOGScroll.laOGvrFlt[lnMethPos,6]
lcDiv = loOGScroll.laOGvrFlt[lnDivPos,6]

lcPrior = loOGScroll.laOGvrFlt[lnPriorPos,6]
lcPriorFrom = SUBSTR(lcPrior,1,1)
lnTo = AT('|',lcPrior)+1
lcPriorTo =SUBSTR(lcPrior,lnTo,1)

lnPrdPos = lfGetPos("APINVHDR.CFISFYEAR+'-'+APINVHDR.CFSPPRDID",'laOgVrFlt')
lcPrdSlctd = loOGScroll.laOGvrFlt[lnPrdPos,6]

STORE '' TO lcPrdFrom,lcPrdTo
IF !EMPTY(lcPrdSlctd) AND USED(lcPrdSlctd)
  SELECT &lcPrdSlctd
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

*E302940,5 Payable Invoice Detail TMI 10/17/2011 [Start] get the extra added filters from the lcRpExp to allow the 
* advanced option to work
lcRpExp = STRTRAN(lcRpExp,"  "," ")
lcRpExp = '.T. AND '+lcRpExp
=lfReplExp(@lcRpExp,"AND APINVHDR.CCURRCODE",'')
=lfPolishExp(@lcRpExp,"(APINVHDR.CVENDCODE")
=lfPolishExp(@lcRpExp,"(APINVHDR.CDIVISION")
=lfPolishExp(@lcRpExp,"(APINVHDR.CVENPMETH")
=lfPolishExp(@lcRpExp,"(APINVHDR.CVENPRIOR")
=lfPolishExp(@lcRpExp,"(APDIST.CFISFYEAR")
lcRpExp = ALLTRIM(lcRpExp)
lcRpExp = IIF( lcRpExp=='.T.','',SUBSTR(lcRpExp,5))
*E302940,5 Payable Invoice Detail TMI 10/17/2011 [End  ] 

WAIT WINDOW NOWAIT 'Collecting Data ...'

IF llVenSlct
  SCAN
    gfseek(&lcVend..cVendCode,'APINVHDR')
    SELECT APINVHDR
    =lfInsLine(&lcVend..cVendCode)
  ENDSCAN  
ELSE 
  gfseek('','APINVHDR')
  SELECT APINVHDR
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
*! Date          : 08/03/2009
*!**************************************************************************
FUNCTION lfInsLine
PARAMETERS lcVendCode         &&pass vendor code as a parameter
lcVendCode = IIF(EMPTY(lcVendCode),'',lcVendCode)

SCAN REST WHILE CVENDCODE+CINVNO = lcVendCode
  *E302938,4 Payable Invoice Summary TMI 10/17/2011 [Start] 
*!*	  IF IIF(!EMPTY(loOGScroll.laOGfxFlt[lnCurrPos,6]),APINVHDR.CCURRCODE $ loOGScroll.laOGfxFlt[lnCurrPos,6] , .T. ) AND ;
*!*	    IIF(!EMPTY(loOGScroll.laOGvrFlt[lnMethPos,6]),APINVHDR.CVENPMETH $ loOGScroll.laOGvrFlt[lnMethPos,6],.T.) AND ;
*!*	    IIF(!EMPTY(loOGScroll.laOGvrFlt[lnDivPos,6]),APINVHDR.CDIVISION $ loOGScroll.laOGvrFlt[lnDivPos,6],.T.) AND ;
*!*	    IIF(!EMPTY(loOGScroll.laOGvrFlt[lnPriorPos,6]),BETWEEN(APINVHDR.CVENPRIOR,lcPriorFrom,lcPriorTo),.T.) AND ;
*!*	    IIF(llRpIncInv,.T.,APINVHDR.CINVSTAT<>'V') AND ;
*!*	    IIF(lcRpStatus='A',.T.,;
*!*	     IIF(lcRpStatus='O',ABS(APINVHDR.NINVAMNT) > ABS(APINVHDR.NINVPAID)+ABS(APINVHDR.NINVADJ)+ABS(APINVHDR.NINVDISTK),;
*!*	                        ABS(APINVHDR.NINVAMNT) = ABS(APINVHDR.NINVPAID)+ABS(APINVHDR.NINVADJ)+ABS(APINVHDR.NINVDISTK))) AND ;
*!*	    IIF(!EMPTY(lcRpRefnce),LIKE(STRTRAN(lcRpRefnce,' ','?'),APINVHDR.cinvref),.T.) AND ;
*!*	    IIF(!EMPTY(lcRpSess),lfFltSess(),.T.) AND ;
*!*	    IIF(!EMPTY(lcPrdFrom),BETWEEN(apinvhdr.cfisfyear +'-'+ apinvhdr.cfspprdid,lcPrdFrom,lcPrdTo),.T.) 
  IF IIF(!EMPTY(loOGScroll.laOGfxFlt[lnCurrPos,6]),APINVHDR.CCURRCODE $ loOGScroll.laOGfxFlt[lnCurrPos,6] , .T. ) AND ;
    IIF(!EMPTY(loOGScroll.laOGvrFlt[lnMethPos,6]),APINVHDR.CVENPMETH $ loOGScroll.laOGvrFlt[lnMethPos,6],.T.) AND ;
    IIF(!EMPTY(loOGScroll.laOGvrFlt[lnDivPos,6]),APINVHDR.CDIVISION $ loOGScroll.laOGvrFlt[lnDivPos,6],.T.) AND ;
    IIF(!EMPTY(loOGScroll.laOGvrFlt[lnPriorPos,6]),BETWEEN(APINVHDR.CVENPRIOR,lcPriorFrom,lcPriorTo),.T.) AND ;
    IIF(llRpIncInv,.T.,APINVHDR.CINVSTAT<>'V') AND ;
    IIF(lcRpStatus='A',.T.,;
     IIF(lcRpStatus='O',ABS(APINVHDR.NINVAMNT) > ABS(APINVHDR.NINVPAID)+ABS(APINVHDR.NINVADJ)+ABS(APINVHDR.NINVDISTK),;
                        ABS(APINVHDR.NINVAMNT) = ABS(APINVHDR.NINVPAID)+ABS(APINVHDR.NINVADJ)+ABS(APINVHDR.NINVDISTK))) AND ;
    IIF(!EMPTY(lcRpRefnce),LIKE(STRTRAN(lcRpRefnce,' ','?'),APINVHDR.cinvref),.T.) AND ;
    IIF(!EMPTY(lcRpSess),lfFltSess(),.T.) AND ;
    IIF(!EMPTY(lcPrdFrom),BETWEEN(apinvhdr.cfisfyear +'-'+ apinvhdr.cfspprdid,lcPrdFrom,lcPrdTo),.T.) ;
    &lcRpExp     

    SCATTER MEMVAR memo 
    =gfSeek(APINVHDR.CVENDCODE,'APVENDOR')
    m.CVENCOMP = APVENDOR.CVENCOMP
    INSERT INTO &lcTempFile FROM MEMVAR 
    SELECT &lcTempFile
    REPLACE CVENPMETH WITH SUBSTR(lcRpVldEnt,ATC('~',lcRpVldEnt,ATC(LEFT(cvenpmeth,1),lcRpVldVal))+1,(ATC('~',lcRpVldEnt,ATC(LEFT(cvenpmeth,1),lcRpVldVal)+1)-1)-(ATC('~',lcRpVldEnt,ATC(LEFT(cvenpmeth,1),lcRpVldVal)))) ,;
            NAGE      WITH IIF((oAriaApplication.SystemDate-DINVDUDAT)+1<0,(oAriaApplication.SystemDate-DINVDUDAT)-1,(oAriaApplication.SystemDate-DINVDUDAT)+1) ,;
            INVAMNT1  WITH gfAmntDisp(APINVHDR.NINVAMNT,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.) ,;
            PAIDAMNT1 WITH IIF(APINVHDR.CINVSTAT='V',0,gfAmntDisp(APINVHDR.NINVPAID,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.)),;
            DIS_TAK1  WITH gfAmntDisp(APINVHDR.NINVDISTK,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.),;
            ADJ_AMNT1 WITH gfAmntDisp(APINVHDR.NINVADJ,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.),;
            OPEN_BAL1 WITH gfAmntDisp(APINVHDR.NINVAmnt-APINVHDR.NINVPAID-APINVHDR.NINVDISTK-APINVHDR.NINVADJ,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.)
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
*! Date          : 08/03/2009
*!**************************************************************************
FUNCTION lfvCrTemp
LOCAL lnI

IF USED(lcTempFile)
  SELECT (lcTempFile)
  ZAP
  RETURN
ENDIF

DIMENSION laTempStru[17,18]

lnI = 0
lnI = lnI + 1
laTempstru[lnI,1]='CVENDCODE'
laTempstru[lnI,2]='C'
laTempstru[lnI,3]= 8
laTempstru[lnI,4]= 0

lnI = lnI + 1
laTempstru[lnI,1]='CVENCOMP'
laTempstru[lnI,2]='C'
laTempstru[lnI,3]= 30
laTempstru[lnI,4]= 0

lnI = lnI + 1
laTempstru[lnI,1]='CINVNO'
laTempstru[lnI,2]='C'
laTempstru[lnI,3]= 12
laTempstru[lnI,4]= 0

lnI = lnI + 1
laTempstru[lnI,1]='CINVREF'
laTempstru[lnI,2]='C'
laTempstru[lnI,3]= 15
laTempstru[lnI,4]= 0
  
lnI = lnI + 1
laTempstru[lnI,1]='DINVDATE'
laTempstru[lnI,2]='D'
laTempstru[lnI,3]= 10
laTempstru[lnI,4]= 0

lnI = lnI + 1
laTempstru[lnI,1]='DINVDUDAT'
laTempstru[lnI,2]='D'
laTempstru[lnI,3]= 10
laTempstru[lnI,4]= 0
    
lnI = lnI + 1
laTempstru[lnI,1]='CFACCODE'
laTempstru[lnI,2]='C'
laTempstru[lnI,3]= 8
laTempstru[lnI,4]= 0
    
lnI = lnI + 1
laTempstru[lnI,1]='CVENPRIOR'
laTempstru[lnI,2]='C'
laTempstru[lnI,3]= 1
laTempstru[lnI,4]= 0
   
lnI = lnI + 1
laTempstru[lnI,1]='CVENPMETH'
laTempstru[lnI,2]='C'
laTempstru[lnI,3]= 30
laTempstru[lnI,4]= 0
  
lnI = lnI + 1
laTempstru[lnI,1]='nAGE'
laTempstru[lnI,2]='N'
laTempstru[lnI,3]= 8
laTempstru[lnI,4]= 0
  
lnI = lnI + 1
laTempstru[lnI,1]='INVAMNT1'
laTempstru[lnI,2]='N'
laTempstru[lnI,3]= 15
laTempstru[lnI,4]= 2
  
lnI = lnI + 1
laTempstru[lnI,1]='PAIDAMNT1'
laTempstru[lnI,2]='N'
laTempstru[lnI,3]= 15
laTempstru[lnI,4]= 2
  
lnI = lnI + 1
laTempstru[lnI,1]='DIS_TAK1'
laTempstru[lnI,2]='N'
laTempstru[lnI,3]= 15
laTempstru[lnI,4]= 2
  
lnI = lnI + 1
laTempstru[lnI,1]='ADJ_AMNT1'
laTempstru[lnI,2]='N'
laTempstru[lnI,3]= 15
laTempstru[lnI,4]= 2
  
lnI = lnI + 1
laTempstru[lnI,1]='OPEN_BAL1'
laTempstru[lnI,2]='N'
laTempstru[lnI,3]= 15
laTempstru[lnI,4]= 2
  
lnI = lnI + 1
laTempstru[lnI,1]='CCURRCODE'
laTempstru[lnI,2]='C'
laTempstru[lnI,3]= 3
laTempstru[lnI,4]= 0
  
lnI = lnI + 1
laTempstru[lnI,1]='CINVSTAT'
laTempstru[lnI,2]='C'
laTempstru[lnI,3]= 8
laTempstru[lnI,4]= 0
     
FOR lnJ = 1 TO ALEN(laTempstru,1)
  FOR lnI = 7 TO 16
    laTempstru[lnJ,lnI] = ''
  ENDFOR
  laTempstru[lnJ,17] = 0
  laTempstru[lnJ,18] = 0
ENDFOR
=gfCrtTmp(lcTempFile,@laTempStru,"CVENDCODE+CINVNO",lcTempFile,.T.)
*--End of function

*!**************************************************************************
*! Function      : lfwRepWhen
*! Purpose       : Creating Temp file  
*! Developer     : AHMED MOUSTAFA (AHS)     
*! Date          : 08/03/2009
*!**************************************************************************
FUNCTION lfwRepWhen

loOgScroll.laOGFxFlt[1,6] = oAriaApplication.basecurrency

SELECT APVENDOR  
=gfseek('')

SELECT FSPRD
=gfseek('')

*!**************************************************************************
*!
*!      Function: lfvInvoice
*!
*!**************************************************************************
* 
FUNCTION lfvInvoice

lnRpIncInv = ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,'LLRPINCINV'),1)

laOGObjCnt[lnRpIncInv] = lcRpStatus <> 'P'
llRpIncInv=IIF(lcRpStatus=='P',.F.,llRpIncInv)

= lfOGShowGet('LLRPINCINV')

*!**************************************************************************
*!
*!      Function: lfvSess
*!
*!**************************************************************************
FUNCTION lfvSess

IF EMPTY(lcRpSess)
  RETURN
ENDIF

lcRpSess= PADL(ALLTRIM(lcRpSess),8,'0')
IF SEEK('CAPSESSNO ' , 'SEQUENCE')
  IF !BETWEEN(VAL(lcRpSess),1,SEQUENCE.nSeq_No)
    =gfModalGen("TRM04158B00000","DIALOG",ALLTRIM(STR(SEQUENCE.nSeq_No)))
    lcRpSess = ''
  ENDIF
ENDIF  

*!**************************************************************************
*!
*!      Function: lfFltSess
*!      To check if the one of distribution lines for the current
*!      invoice has the entered session number
*!**************************************************************************
FUNCTION lfFltSess

IF !USED('APDIST')
   =gfOpenTABLE(oAriaApplication.DATADIR+'APDIST',oAriaApplication.DATADIR+'INVVEND','SH')
ENDIF

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
FUNCTION lfvCurDisp

llRpProced = gfRepCur(.T., @lcRpCurr,@ldRpExDate,lcRpTmpNam)

*!*************************************************************
*! Name      : lfvCurCode
*! Developer : Walid Abou El-Magd (WAM)
*! Date      : 08/17/1999
*! Purpose   : This function called from the currency field to
*!             validate the currency.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : 
*!*************************************************************
FUNCTION lfvCurCode
lnAlias=SELECT(0)

IF EMPTY(laOGFxFlt[1,6]) 
  RETURN
ENDIF

IF !SEEK(laOGFxFlt[1,6],'SYCCURR') .OR. ATC("?",laOGFxFlt[1,6]) > 0
  SELECT SYCCURR
  DIMENSION laTemp[1]
  laTemp     = ''
  lcSavBrFld = lcBrFields
  lcSavTitle = lcFile_Ttl
  lcFile_Ttl = "Currency"
  lcBrFields = "CCURRCODE :R :H= 'Currency code'," +;
               "CCURRDESC :R :H= 'Description',  " +;
               "CCURRSMBL :R :H= 'Symbol'"
  =gfBrows('','CCURRCODE','laTemp')
  lcBrFields = lcSavBrFld
  lcFile_Ttl = lcSavTitle
  IF EMPTY(laTemp[1])
    laOGFxFlt[1,6] = loOgScroll.ActiveControl.OldValue
  ELSE
    laOGFxFlt[1,6] = laTemp[1]
  ENDIF
ENDIF

SELECT(lnAlias)


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
LOCAL lnPos,lnSlct,lcSlcPrd
lnSlct = SELECT()

DO CASE
CASE lcSRV = 'R'
  *- be sure that only two id's are selected at most
  lnPrdPos = lfGetPos("APINVHDR.CFISFYEAR+'-'+APINVHDR.CFSPPRDID",'laOgVrFlt')  
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

******************************************************************************************************
*
*   FUNCTION lfvPriority
*   check if the From Priority is no set , then force it to be '1'
******************************************************************************************************
FUNCTION lfvPriority
LOCAL loFrom,loTo
loFrom = _SCREEN.ActiveForm.ACTIVECONTROL.PARENT.OBJ_FROM
loTo   = _SCREEN.ActiveForm.ACTIVECONTROL.PARENT.OBJ_TO

IF !EMPTY(loTo.VALUE) AND EMPTY(loFrom.VALUE)
  loFrom.VALUE = '1'
ENDIF