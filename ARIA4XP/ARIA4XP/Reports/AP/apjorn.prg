*:***************************************************************************
*: Program file  : APJORN
*: Program desc. : AP JOURNAL
*: System        : ARIA4XP
*: Module        : Accounts Payable(AP)
*: Developer     : AHMED MOUSTAFA (AHS)
*: Date          : 09/28/2009                                                                                                                                                 
*:***************************************************************************
*:Modification
*E303640,5 TMI 08/15/2011 Add a test code to support new added indexes to the APDIST file
*E302975,1 AP Conv.Proj. Attaching all files the phase to TMI 10/23/2011 
*B609973,1 SAB 06/21/2012 Fix problem that appear when select transaction date or AP session no [T20120606.0025]
*E303379,1 TMI 04/17/2013 Include voided payments in the apjournal report [T20130102.0010] 
*B610677,1 TMI 02/17/2014 fix the order When report is sorted by Vendor Invoice [T20140214.0007 ] 
*B610677,4 TMI 03/13/2014 don't get the array of vendors from the lcTempFile [T20140214.0007 ] 
*E303659,1 AEG 04/07/2016 modify ap jornal to show voided invoices only [T20150126.0034 ] 
*:************************************************************************

IF loOgScroll.llOGFltCh 
  =lfCrTemp() &&To create Temp File
  =lfColData() &&Data Collection
ENDIF 

*- set the correct order before getting data
SELECT &lcTempFile
DO CASE
  CASE lcRpSort = 'G'
    lcRpForm = IIF(lcRpFormat='A','APJORN','APJORNB')
    SET ORDER TO 'ACCTYRPR'  && 'CAPDGLACT+CFISFYEAR+CFSPPRDID'
  CASE lcRpSort = 'T'
    lcRpForm = 'APJORNT'
    SET ORDER TO 'cJourTagT' && capdtrtyp+cApdRef+cInvNo
  CASE lcRpSort = 'V'
    lcRpForm = 'APJORNV'
    SET ORDER TO 'cJourTagV' && CVENDCODE+CINVNO
    *B610677,1 TMI 02/17/2014 14:42 [Start] sort index be cJourTagT in case of sort option is V or T
    IF loOgScroll.llOGFltCh 
      =lfGetOpenAmt()
      *- adjust report format by showing the invoice # then its DB application 
      SELECT &lcTempFile
      SET ORDER TO            && disable the order as the replaced field is included in the key
      LOCATE
      SCAN FOR CAPDTRTYP = 'A' AND NEQVAMNT>0
        lcInv = CINVNO 
        REPLACE CINVNO WITH CAPDREF ;
                CAPDREF WITH lcInv
      ENDSCAN
      *- replace  CAPDTRTYP with 'X' to show after the type 'I'
      LOCATE 
      REPLACE CAPDTRTYP WITH 'X'  FOR CAPDTRTYP = 'A' 
      SET ORDER TO 'cJourTagV' && CVENDCODE+CINVNO
    ENDIF    
   *B610677,1 TMI 02/17/2014 14:42 [End  ] 
ENDCASE
LOCATE

DO gfDispRe WITH EVAL('lcRpForm')

*!**************************************************************************
*! Function      : lfCrTemp
*! Purpose       : Creating Temp file  
*! Developer     : AHMED MOUSTAFA (AHS)     
*! Date          : 09/28/2009
*!**************************************************************************
FUNCTION lfCrTemp
LOCAL lnSlct
lnSlct = SELECT(0)

IF USED(lcTempFile)
  SELECT (lcTempFile)
  ZAP
  SELECT (lnSlct)
  RETURN
ENDIF 

DIMENSION laTempStru[15,18]

LOCAL lnI,lnJ
lnI = 0
lnI = lnI + 1
laTempstru[lnI,1]='CVENDCODE'
laTempstru[lnI,2]='C'
laTempstru[lnI,3]= 8
laTempstru[lnI,4]= 0
   
lnI = lnI + 1
laTempstru[lnI,1]='CAPDGLACT'
laTempstru[lnI,2]='C'
laTempstru[lnI,3]= 24
laTempstru[lnI,4]= 0

lnI = lnI + 1
laTempstru[lnI,1]='CINVNO'
laTempstru[lnI,2]='C'
laTempstru[lnI,3]= 12
laTempstru[lnI,4]= 0

lnI = lnI + 1
laTempstru[lnI,1]='CFSPPRDID'
laTempstru[lnI,2]='C'
laTempstru[lnI,3]= 2
laTempstru[lnI,4]= 0
  
lnI = lnI + 1
laTempstru[lnI,1]='DAPDTRDAT'
laTempstru[lnI,2]='D'
laTempstru[lnI,3]= 10
laTempstru[lnI,4]= 0

lnI = lnI + 1
laTempstru[lnI,1]='CAPDTRTYP'
laTempstru[lnI,2]='C'
laTempstru[lnI,3]= 1
laTempstru[lnI,4]= 0
        
lnI = lnI + 1
laTempstru[lnI,1]='CAPDTRDSC'
laTempstru[lnI,2]='C'
laTempstru[lnI,3]= 20
laTempstru[lnI,4]= 0
        
lnI = lnI + 1
laTempstru[lnI,1]='CAPDREF'
laTempstru[lnI,2]='C'
laTempstru[lnI,3]= 12
laTempstru[lnI,4]= 0
   
lnI = lnI + 1
laTempstru[lnI,1]='CAPSESSNO'
laTempstru[lnI,2]='C'
laTempstru[lnI,3]= 8
laTempstru[lnI,4]= 0
   
lnI = lnI + 1
laTempstru[lnI,1]='CBATCHNO'
laTempstru[lnI,2]='C'
laTempstru[lnI,3]= 6
laTempstru[lnI,4]= 0
  
lnI = lnI + 1
laTempstru[lnI,1]='CTRNSLEDN'
laTempstru[lnI,2]='C'
laTempstru[lnI,3]= 8
laTempstru[lnI,4]= 0
  
lnI = lnI + 1
laTempstru[lnI,1]='NEQVAMNT'
laTempstru[lnI,2]='N'
laTempstru[lnI,3]= 15
laTempstru[lnI,4]= 2
  
lnI = lnI + 1
laTempstru[lnI,1]='CVENCOMP'
laTempstru[lnI,2]='C'
laTempstru[lnI,3]= 25
laTempstru[lnI,4]= 0
  
lnI = lnI + 1
laTempstru[lnI,1]='CFISFYEAR'
laTempstru[lnI,2]='C'
laTempstru[lnI,3]= 4
laTempstru[lnI,4]= 0
  
lnI = lnI + 1
laTempstru[lnI,1]='CDESC'
laTempstru[lnI,2]='C'
laTempstru[lnI,3]= 30
laTempstru[lnI,4]= 0

*- Update other array elements 
FOR lnI = 1 TO ALEN(laTempstru,1)
  FOR lnJ = 7 TO 16
    laTempstru[lnI,lnJ] = ''
  ENDFOR
  STORE 0 TO laTempstru[lnI,17],laTempstru[lnI,18]
ENDFOR

DIMENSION laIndex[3,2]
laIndex[1,1] = 'CAPDGLACT+CFISFYEAR+CFSPPRDID'
laIndex[1,2] = 'ACCTYRPR'
*B610677,3 TMI 02/20/2014 16:24 [Start] add the CAPDTRTYP to the index
*laIndex[2,1] = 'CVENDCODE+CINVNO'
laIndex[2,1] = 'CVENDCODE+CINVNO+CAPDTRTYP'
*B610677,3 TMI 02/20/2014 16:24 [End  ] 
laIndex[2,2] = 'cJourTagV'
laIndex[3,1] = 'capdtrtyp+cApdRef+cInvNo'
laIndex[3,2] = 'cJourTagT'
=gfCrtTmp(lcTempFile,@laTempStru,@laIndex)

SELECT (lnSlct)

*--End of function
*!**************************************************************************
*! Function      : lfColData
*! Purpose       : Collection of data 
*! Developer     : AHMED MOUSTAFA (AHS)     
*! Date          : 08/09/2009
*!**************************************************************************
FUNCTION lfColData

lnVendPos = lfGetPos('APDIST.CVENDCODE','laOgVrFlt')
lnGlAcctPos = lfGetPos('APDIST.CAPDGLACT','laOgVrFlt')
lnTrTypePos = lfGetPos('APDIST.CAPDTRTYP','laOgVrFlt')
lnSessNoPos = lfGetPos('APDIST.CAPSESSNO','laOgVrFlt')
lnTrDatePos = lfGetPos('APDIST.DAPDTRDAT','laOgFxFlt')
lcVend = loOGScroll.laOGvrFlt[lnVendPos,6]
lcGlAcct = loOGScroll.laOGvrFlt[lnGlAcctPos,6]
lcTrType = loOGScroll.laOGvrFlt[lnTrTypePos,6]
lcTrDate = loOGScroll.laOGfxFlt[lnTrDatePos,6]
lcSessNo = loOGScroll.laOGvrFlt[lnSessNoPos,6]

lcPTrD = AT('|',lcTrDate)+1
lcTrDateFrom = SUBSTR(lcTrDate,1,lcPTrD-2)
lcTrDateTo = SUBSTR(lcTrDate,lcPTrD)

lcP = AT('|',lcSessNo)+1
lcSessFrom = SUBSTR(lcSessNo,1,lcP-2)
lcSessTo = SUBSTR(lcSessNo,lcP)

lnVen = 0
IF !EMPTY(lcVend) AND USED(lcVend)
  SELECT &lcVend
  LOCATE
  COUNT TO lnVen FOR !DELETED()
ENDIF   

STORE {} TO ldFrmDate , ldEnddate
IF !EMPTY(laOGFxFlt[lnTrDatePos,6])
    lnPipPos = ATC('|',laOgFxFlt[1,6]) - 1
    ldFrmDate = CTOD(SUBSTR(laogFxFlt[1,6],1,lnPipPos))
    ldEnddate = CTOD(SUBSTR(laogFxFlt[1,6],lnPipPos+ 2,LEN(laOGFxFlt[lnTrDatePos,6])))
ENDIF

lcRpExp = lcRpExp + IIF(EMPT(lcRpExp) , '' , ' AND ' )  + " APDIST.nApdAmnt <> 0  " 
*E303379,1 TMI 04/15/2013 [Start] 
*lcRpExp = lcRpExp + IIF(EMPT(lcRpExp) , '' , ' AND ' )  + ;
                    "(APDIST.capdstat <> 'V' .OR. "+;
                    "(APDIST.capdstat = 'V' .AND. APDIST.cApdTrTyp $ 'MHNP' "+;
                    IIF(!EMPTY(ldEnddate)," .AND. BETWEEN(DTOS(APDIST.DAPDTRDAT),'"+DTOS(ldFrmDate)+"','"+DTOS(ldEnddate)+"')","")+"))"
lcIncI = "MHNP" + IIF(llRpIncVdInv,"I","")
*!* E303659,1 AEG 04/07/2016 modify ap jornal to show voided invoices only [Start ] 
*!*	lcRpExp = lcRpExp + IIF(EMPT(lcRpExp) , '' , ' AND ' )  + ;
*!*	                    "(APDIST.capdstat <> 'V' .OR. "+;
*!*	                    "(APDIST.capdstat = 'V' .AND. APDIST.cApdTrTyp $ '&lcIncI' "+ ;
*!*	                    IIF(!EMPTY(ldEnddate)," .AND. BETWEEN(DTOS(APDIST.DAPDTRDAT),'"+DTOS(ldFrmDate)+"','"+DTOS(ldEnddate)+"')","")+"))"



lcRpExp = lcRpExp + IIF(EMPT(lcRpExp) , '' , ' AND ' )  + ;
                    "("+IIF(llRpShVdInv .AND. llRpIncVdInv,"","APDIST.capdstat <> 'V' .OR. ")+;
                    "(APDIST.capdstat = 'V' .AND. APDIST.cApdTrTyp $ '&lcIncI' "+ ;
                    IIF(!EMPTY(ldEnddate)," .AND. BETWEEN(DTOS(APDIST.DAPDTRDAT),'"+DTOS(ldFrmDate)+"','"+DTOS(ldEnddate)+"')","")+"))"


*!* E303659,1 AEG 04/07/2016 modify ap jornal to show voided invoices only [End ] 
*E303379,1 TMI 04/15/2013 [End  ] 
lcRpExp = lcRpExp + IIF(!llRpPrven AND lcRpSort = 'V',IIF(EMPT(lcRpExp) , '' , ' AND ' )  + " !EMPTY(APDIST.cvendcode) " ,'')
lcRpExp = lcRpExp + IIF(lcRpRel = 'B','',;
                         IIF(EMPTY(lcRpExp),'',' AND ')+IIF(lcRpRel='U','!','')+"APDIST.lApdPost")
lcRpExp = STRTRAN(lcRpExp,'.AND.',' AND ')
lcRpExp = STRTRAN(lcRpExp,'  ',' ')

SELECT APDIST
DO CASE
*- A certain transaction # is selected
CASE !EMPTY(lcTrnNo)
  SET ORDER TO INVVEND   && CINVNO+CVENDCODE+CAPDTRTYP
   lcRpExp = lcRpExp + IIF(EMPT(lcRpExp) , '' , ' AND ' ) + ;
                      " APDIST.cApdRef= '"+lcTrnNo+"'"
  lcRpExp = STRTRAN(lcRpExp,"AND APDIST.CAPDTRTYP = '"+lcTrType+"'")
  =lfPolishExp(@lcRpExp,"APDIST.CVENDCODE")
  =SEEK(lcInvNo+lcVenNo+lcTrType,'APDIST')
  lcWhile = "WHILE "+KEY()+" = '"+lcInvNo+lcVenNo+lcTrType+"'"
  =lfInsLine(lcWhile)
 *- Vendor or more is selected
CASE lnVen>0 
  IF !FILE(gcWorkDir +lcJourTmp+ '.CDX') OR !lfTagFound('cJourTagV','APDIST')
    WAIT WINDOW NOWAIT 'Please wait .. '
    INDEX ON cVendCode+capdtrtyp TAG cJourTagV OF (gcWorkDir +  lcJourTmp + '.CDX')          
    WAIT CLEAR
  ELSE
    SET ORDER TO TAG cJourTagV OF (gcWorkDir +  lcJourTmp + '.CDX')      
  ENDIF
  =lfPolishExp(@lcRpExp,"APDIST.CVENDCODE")
  lcKey = KEY()
  SELECT &lcVend
  LOCATE
  SCAN
    lcWhile = "WHILE "+lcKey+" = '"+&lcVend..CVENDCODE+"'"    
    IF SEEK(&lcVend..CVENDCODE,'APDIST')
      =lfInsLine(lcWhile)
    ENDIF
  ENDSCAN  
  
*- GL Account is selected
CASE !EMPTY(CHRTRAN(lcGlAcct,'-',''))
  SET ORDER TO ACCTYRPR
  lcRpExp = STRTRAN(lcRpExp,"AND APDIST.CAPDGLACT = '"+lcGlAcct+"'")
  lcWhile = "WHILE "+KEY()+" = '"+lcGlAcct+"'"
  IF SEEK(lcGlAcct,'APDIST')
    =lfInsLine(lcWhile)
  ENDIF
  
*- Just a transaction type is selected
CASE !EMPTY(lcTrType)
  SET ORDER TO PAYMNTS   && CAPDTRTYP+CBNKCODE+CCHKACCT+CAPDREF+CINVNO+CAPDACTID
  lcRpExp = STRTRAN(lcRpExp,"AND APDIST.CAPDTRTYP = '"+lcTrType+"'")
  lcWhile = "WHILE "+KEY()+" = '"+lcTrType+"'"    
  IF SEEK(lcTrType,'APDIST')
    =lfInsLine(lcWhile)
  ENDIF
  
CASE !EMPTY(lcTrDateTo)
  IF !FILE(gcWorkDir +lcJourTmp+ '.CDX') OR !lfTagFound('cJourTagD','APDIST')
    WAIT WINDOW NOWAIT 'Please wait .. '
    INDEX ON DTOS(DAPDTRDAT) TAG cJourTagD OF (gcWorkDir +  lcJourTmp + '.CDX')
    WAIT CLEAR
  ELSE
    SET ORDER TO TAG cJourTagD OF (gcWorkDir +  lcJourTmp + '.CDX')      
  ENDIF
  IF EMPTY(DTOS(CTOD(lcTrDateFrom)))
    LOCATE
  ELSE
    IF !SEEK(DTOS(CTOD(lcTrDateFrom)),'APDIST')
      *B609973,1 SAB 06/21/2012 Fix problem that appear when select transaction date or AP session no [Start]
      *GOTO RECNO(0)
      IF RECNO(0) > 0
        GOTO RECNO(0)
      ENDIF
      *B609973,1 SAB 06/21/2012 Fix problem that appear when select transaction date or AP session no [End]
    ENDIF
  ENDIF
  lcRpExp = STRTRAN(lcRpExp,"AND BETWEEN(DTOS(APDIST.DAPDTRDAT),'"+DTOS(CTOD(lcTrDateFrom))+"','"+DTOS(CTOD(lcTrDateTo))+"')")
  lcWhile = "WHILE DTOS(DAPDTRDAT) <= '"+DTOS(CTOD(lcTrDateTo))+"'"
  =lfInsLine(lcWhile)

*- Session ID is entered
CASE !EMPTY(lcSessTo)

  IF !FILE(gcWorkDir +lcJourTmp+ '.CDX') OR !lfTagFound('cJourTagS','APDIST')
    WAIT WINDOW NOWAIT 'Please wait .. '
    INDEX ON CAPSESSNO TAG cJourTagS OF (gcWorkDir +  lcJourTmp + '.CDX')
    WAIT CLEAR
  ELSE
    SET ORDER TO TAG cJourTagS OF (gcWorkDir +  lcJourTmp + '.CDX')      
  ENDIF
  IF EMPTY(lcSessFrom)
    LOCATE
  ELSE
    IF !SEEK(lcSessFrom,'APDIST')
      *B609973,1 SAB 06/21/2012 Fix problem that appear when select transaction date or AP session no [Start]      
      *GOTO RECNO(0)
      IF RECNO(0) > 0
        GOTO RECNO(0)
      ENDIF
      *B609973,1 SAB 06/21/2012 Fix problem that appear when select transaction date or AP session no [End]
    ENDIF
  ENDIF
  lcSessFrom = PADR(lcSessFrom,8)
  lcSessTo   = PADR(lcSessTo,8)
  lcRpExp = STRTRAN(lcRpExp,"AND BETWEEN(APDIST.CAPSESSNO,'"+lcSessFrom+"','"+lcSessTo+"')")
  lcWhile = "WHILE APDIST.CAPSESSNO <= '"+lcSessTo+"'"
  =lfInsLine(lcWhile)
 OTHERWISE

  LOCATE
  =lfInsLine('')
  
ENDCASE

*!**************************************************************************
*! Function      : lfInsLine
*! Purpose       : Inserting lines in Temp File
*! Developer     : AHMED MOUSTAFA (AHS)     
*! Date          : 08/03/2009
*!**************************************************************************
FUNCTION lfInsLine
LPARAMETERS lcWhile
lcWhile = IIF(EMPTY(lcWhile),'',lcWhile)

SELECT APDIST
SCAN REST &lcWhile ;
     FOR &lcRpExp

  SCATTER MEMVAR memo   
  m.cVenComp = IIF(SEEK(APDIST.cVendCode,'APVENDOR'),APVENDOR.cVenComp,'')
  m.CDESC    = IIF(llApGlLink,IIF(SEEK(APDIST.capdglact,'GLACCHAR','ACCTCODE'),GLACCHAR.CACCNLDES,''),'')
  m.CAPDTRDSC = IIF(APDIST.cApdTrTyp = 'A' , 'DM application' ,IIF(APDIST.cApdTrTyp = 'B' , 'Bank adjustment', IIF(APDIST.cApdTrTyp = 'H' , 'Cash payments',IIF(APDIST.cApdTrTyp = 'I' , 'Invoice', IIF(APDIST.cApdTrTyp = 'M' , 'Manual checks',IIF(APDIST.cApdTrTyp = 'N' , 'Non check payment',IIF(APDIST.cApdTrTyp = 'P' , 'Printed checks' ,'')))))))

  SELECT &lcTempFile     
  INSERT INTO &lcTempFile FROM MEMVAR      
ENDSCAN 
*!**************************************************************************
*! Function      : lfGetPos
*! Purpose       : Getting the number of element from array 
*! Developer     : AHMED MOUSTAFA (AHS)     
*! Date          : 09/29/2009
*!**************************************************************************
FUNCTION lfGetPos
PARAMETERS lcOpt,lcArray
LOCAL lnPos
lnPos = ASCAN(loOGScroll.&lcArray,lcOpt)
lnPos = ASUBSCRIPT(loOGScroll.&lcArray,lnPos,1)
RETURN lnPos
*--End of function

*!*************************************************************
*! Name      : lfvApAcCod
*! Developer : Tarek Mohamed Ibrahim
*! Date      : 08/15/2011
*! Purpose   : This function is to validate the accounts from 
*!             the chart of account of the active company or the
*!             chart of account of another company.
*!*************************************************************
FUNCTION lfvApAcCod
LOCAL loFld,lnPOS
loFld = loOgScroll.ActiveControl

lcApsAcMas = ACCOD.cAcsMask
lcApsAcMas = STRTRAN(lcApsAcMas,'#',IIF(APSETUP.cApsgllink='Y','9','X'))
lcApsAcMas = ALLTRIM("X"+SUBSTR(lcApsAcMas,2))
lnApsAcLen = LEN(ALLTRIM(lcApsAcMas))

lcSavAlias  = ALIAS()  && Variable to save the selected alias.

lcFieldCont = _screen.ActiveForm.ActiveControl.value    && Assign the content of the field to the variable.

*** Variable hold an empty account to compair with. ***
lcEmptyAcs = REPLICATE('0',lnApsAcLen)

lnPOS = ASCAN(loOgScroll.laOGVrFlt,"APDIST.CAPDGLACT") 
IF lnPos > 0
  lnPOS    = ASUBSCRIPT(loOgScroll.laOGVrFlt,lnPos,1)
ENDIF  

*- Prevent executing the browse if the account code is empty.
IF !EMPTY(STRTRAN(lcFieldCont,"-",""))

  IF llApGlLink .AND. lcFieldCont <> lcEmptyAcs

    IF !USED('GLACCHAR') 
      =gfOpenTABLE('GLACCHAR','ACCTCODE','SH')
      SELECT GLACCHAR
      =gfsetorder('ACCTCODE')
      =gfSeek('')
    ENDIF
    
    *- be sure you browse from the correct table
    SELECT GLACCHAR
    =gfsetorder('ACCTCODE')
    
    IF !SEEK(lcFieldCont) .OR. ATC('?',lcFieldCont) > 0
      DIMENSION laTemp[2]
      laTemp = ''
      lcBrfields="CACCTCODE :H= 'Account Code',;
                  CACCNLDES :H= 'Account Description'"

      lcFile_Ttl="Chart of accounts"
        
      =gfbrows(' ','CACCTCODE,CACCNLDES','laTemp')

      IF !EMPTY(laTemp[1])
        lcFieldCont = ALLTRIM(laTemp[1])
       ELSE
        lcFieldCont = REPLICATE('0',lnApsAcLen)  
      ENDIF
    ENDIF
    
    
    IF !EMPTY(laTemp[1])  
      loFld.Value = laTemp[1]
        loOgScroll.laOGVrFlt[lnPOS,6] = laTemp[1]
    ELSE
      loFld.Value = loFld.OldValue
      *- restore the old value the the laOgvrflt array itself 
      lnGlAcctPos = lfGetPos('APDIST.CAPDGLACT','laOgVrFlt')
      loOGScroll.laOGvrFlt[lnGlAcctPos,6] = loFld.OldValue
    ENDIF  
 
    IF !EMPTY(lcSavAlias)
      SELECT(lcSavAlias)
    ENDIF  

  ENDIF
ELSE
  loOgScroll.laOGVrFlt[lnPOS,6] = ""
ENDIF

IF !EMPTY(lcSavAlias)
  SELECT(lcSavAlias)
ENDIF  
****************************************************************
*! Name      : lfvSess
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 08/11/2011
*! Purpose   : Valid function to pad session #
****************************************************************
FUNCTION lfvSess
LOCAL loFld
loFld = _Screen.ActiveForm.ActiveControl
IF !EMPTY(loFld.Value)
  loFld.Value = PADL(ALLTRIM(loFld.Value),8,'0')
ENDIF
*- end of FUNCTION lfvSess


************************************************************
*! Name      : lfvSort
*! Developer : Tarek Mohamed Ibrahim
*! Date      : 08/15/2011
*! Purpose   : This function is used to order the report based on 
*!             Gl account or transaction type.
************************************************************
FUNCTION lfvSort
SELECT APDIST

DO CASE
  CASE lcRpSort = 'G'
    lcRpForm = IIF(lcRpFormat='A','APJORN','APJORNB')    
    llRpPrven = .T.
  CASE lcRpSort = 'V'
    lcRpForm = 'APJORNV'
  CASE lcRpSort = 'T'
    lcRpForm = 'APJORNT'
    llRpPrven = .T.
ENDCASE
ClearRead()

=lfToglShowTrnNo()

lcRpFormat = IIF(lcRpSort<>'G','A',lcRpFormat)
IF ASCAN(laOGObjType,UPPER('lcRpFormat')) # 0
  lnPos = ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,UPPER('lcRpFormat')),1)
  laOGObjCnt[lnPos] = (PADR(lcRpSort,1)='G')
  =lfOGShowGet(UPPER('lcRpFormat'))
ENDIF

*!*************************************************************
*! Name      : lfwApJour
*! Developer : Tarek Mohamed Ibrahim
*! Date      : 08/15/2011
*! Purpose   : This function is used as a when function for this report
*!*************************************************************
FUNCTION lfwApJour

=lfToglShowTrnNo()

IF !USED('APVENDOR') 
  =gfOpenTABLE('APVENDOR','VENCODE','SH')
  SELECT APVENDOR
  =gfsetorder('VENCODE')
  =gfseek('')
ENDIF

IF !USED('ACCOD') 
  =gfOpenTable("ACCOD",'ACCSEGNO','SH')
  SELECT ACCOD
  =gfsetorder('ACCSEGNO')
  =gfseek('')
ENDIF

IF !USED('APSETUP') 
  =gfOpenTABLE('APSETUP','APSETUP','SH')
  SELECT APSETUP
  =gfSeek('')
ENDIF

IF APSETUP.CAPSGLLINK = 'Y'
  llApGlLink = .T.
ELSE 
  llApGlLink = .F.
ENDIF     

lnTrnTyPos = ASCAN(laOGVrFlt,'APDIST.CAPDTRTYP')
IF lnTrnTyPos > 0
  lnTrnTyPos = ASUBSCRIPT(laOGVrFlt,lnTrnTyPos,1)
ENDIF

*!*************************************************************
*! Name      : lfvTrnNo
*! Developer : Tarek Mohamed Ibrahim
*! Date      : 08/15/2011
*! Purpose   : browse transaction no. from APDIST file
*!*************************************************************
FUNCTION lfvTrnNo

PRIVATE lnTrnType
LOCAL loFld
loFld = loOgScroll.ActiveControl

PRIVATE  lcPriorDbf , lcPriorCdx  , lcPriorFlt
lcPriorDbf  = SELECT(0)
lcPriorCdx  = ORDER()
lcPriorFlt  = FILTER()

PRIVATE lcFiltExp
lcFiltExp = ""

SELECT APDIST
SET ORDER TO Invvend 
LOCATE

* Assign no space to lcInvNo 
lcInvNo = ''  

IF !EMPTY(lcTrnNo) 
  IF !SEEK(lcTrnNo) .OR. ATC("?",lcTrnNo) > 0
    ** MESSAGE : " This record is not found in the  "
    **           " data file.                       "
    **           "      < Browse >   < Reenter>     "
    lnClosRec  = RECNO(0)
    DIMENSION laTemp[4]
    laTemp = ''
    
    lcBrFields = "CVENDCODE :H= 'Vendor code'    ,;
                  CINVNO    :H= 'Invoice number' ,;    
                  DAPDTRDAT :H= 'Invoice date'   ,;
                  CAPDREF   :H= 'Reference'"
                  
    lcFile_Ttl = "Invoice"
    IF BETWEEN(lnClosRec,1,RECCOUNT('APDIST'))
      GO lnClosRec
    ELSE
      GO TOP
    ENDIF
    
    * APDIST.cApdTrTyp  
    * 'A' --- DM Application    
    * 'B' --- Bank Adj.
    * 'H' --- Cash Payment
    * 'I' --- Invoice
    * 'M' --- Manual Payment
    * 'N' --- Non Manual Payment
    * 'P' --- Printed Checks
    DIMENSION  laTrnType[8]
    
    *C101831,1 SSE 03/27/2000 Adjust cases of Transaction Types [Begin]
    DO CASE
      CASE EMPTY(laOGVrFlt[lnTrnTyPos,6])
        lnTrnType = 1
      CASE laOGVrFlt[lnTrnTyPos,6] = "A"
        lnTrnType = 2
      CASE laOGVrFlt[lnTrnTyPos,6] = "B"
        lnTrnType = 3
      CASE laOGVrFlt[lnTrnTyPos,6] = "H"
        lnTrnType = 4
      CASE laOGVrFlt[lnTrnTyPos,6] = "I"
        lnTrnType = 5
      CASE laOGVrFlt[lnTrnTyPos,6] = "M"
        lnTrnType = 6
      CASE laOGVrFlt[lnTrnTyPos,6] = "N"
        lnTrnType = 7
      CASE laOGVrFlt[lnTrnTyPos,6] = "P"
        lnTrnType = 8      
    ENDCASE
    *C101831,1 SSE 03/27/2000 [End]
    
    laTrnType[1]  = [ .T. ]
    laTrnType[2]  = [ cApdTrTyp = 'A' AND cApdActId = 'A' AND nApdAmnt >  0  AND cApdStat <> 'V' ]
    laTrnType[3]  = [ cApdTrTyp = 'B' AND cApdActId = 'D' AND nApdAmnt <> 0  AND cApdStat <> 'V' ]
    laTrnType[4]  = [ cApdTrTyp = 'H' AND cApdActId = 'A' AND nApdAmnt <> 0  AND cApdStat <> 'V' ]
    laTrnType[5]  = [ cApdTrTyp = 'I' AND cApdActId = 'A' AND nApdAmnt <  0  AND cApdStat <> 'V' ]
    laTrnType[6]  = [ cApdTrTyp = 'M' AND cApdActId = 'A' AND nApdAmnt <> 0  AND cApdStat <> 'V' ]
    laTrnType[7]  = [ cApdTrTyp = 'N' AND cApdActId = 'A' AND nApdAmnt <> 0  AND cApdStat <> 'V' ]
    laTrnType[8]  = [ cApdTrTyp = 'P' AND cApdActId = 'A' AND nApdAmnt <> 0  AND cApdStat <> 'V' ]

    lcFiltExp = laTrnType(lnTrnType)   

    *- Add vendor code to filter if not bank adjustment.
    IF !EMPTY(laOGVRFLT[1,6]) AND lnTrnType <> 3 
      lcFiltExp = lcFiltExp  +  " AND  APDIST.CVENDCODE = '"+laOGVRFLT[1,6]+"'  " 
    ENDIF 
    
    =gfBrows( 'FOR' + lcFiltExp  ,'CVENDCODE,CINVNO,DAPDTRDAT,CAPDREF','laTemp')
    
    IF !EMPTY(laTemp[1])
      * Assign selected vendor no. to lcVenNo variable
      lcVenNo   = laTemp[1]
      * Assign selected Reference no. to lcTrnNo variable
      lcTrnNo   = laTemp[4]      
      * Assign invoice no to lcInvNo variable      
      lcInvNo   = laTemp[2]

      loFld.Value = laTemp[4]
    ELSE
      * Assign no space to lcTrnNo and lcInvNo 
      lcTrnNo = ''
      lcInvNo = ''  
      loFld.Value = loFld.oldValue 
    ENDIF    
    
  ENDIF
ENDIF

SELECT (lcPriorDbf)  
SET ORDER TO &lcPriorCdx
SET FILTER TO  &lcPriorFlt  
 
*!**************************************************************************
*!
*!      FUNCTION: lfRepshow
*!
*!**************************************************************************
FUNCTION lfRepshow 


*!***************************************************************************
*! Name      : lfvFormat
*! Developer : Tarek Mohamed Ibrahim
*! Date      : 08/15/2011
*! Purpose   : Valid for changing Report Format 
*!***************************************************************************
FUNCTION lfvFormat
lcRpForm = IIF(lcRpFormat='A','APJORN','APJORNB')
*-- End of lfvFormat.

*!***************************************************************************
*       FUNCTION lfVTranTyp
*!***************************************************************************
FUNCTION lfVTranTyp
=lfToglShowTrnNo()

*!**************************************************************************
*!
*!  FUNCTION lfInputMask
*!
*!**************************************************************************
FUNCTION lfInputMask

IF !USED('APSETUP') 
  =gfOpenTABLE('APSETUP','APSETUP','SH')
  SELECT APSETUP
  =gfsetorder('APSETUP')
  =gfSeek('')
ENDIF
IF !USED('ACCOD') 
  =gfOpenTable("ACCOD",'ACCSEGNO','SH')
  SELECT ACCOD
  =gfsetorder('ACCSEGNO')
  =gfseek('')
ENDIF
LOCAL lcApsAcMas
lcApsAcMas = ACCOD.cAcsMask
lcApsAcMas = STRTRAN(lcApsAcMas,'#',IIF(APSETUP.cApsgllink='Y','9','X'))
lcApsAcMas = ALLTRIM("X"+SUBSTR(lcApsAcMas,2))
lnApsAcLen = LEN(ALLTRIM(lcApsAcMas))

RETURN lcApsAcMas
*end of lfInputMask


********************************************************************************************
* Check if a value is selected
********************************************************************************************
FUNCTION lfIsSlcted
PARAMETERS lcCursor
LOCAL llIsSelected,lnSlct
lnSlct = SELECT(0)
llIsSelected = .F.
IF !EMPTY(lcCursor) AND USED(lcCursor)
  SELECT &lcCursor
  LOCATE 
  llIsSelected = !EOF()
ENDIF
SELECT (lnSlct)
RETURN llIsSelected

******************************************************************************************
*! Name      : lfToglShowTrnNo
*! Developer : Tarek Mohamed Ibrahim
*! Date      : 08/10/2011
*! Purpose   : Toggle "Tran. No" OG variable Enabled/Disabled
******************************************************************************************
FUNCTION lfToglShowTrnNo
LOCAL lnTrTypePos,lcTrType
lnTrTypePos = lfGetPos('APDIST.CAPDTRTYP','laOgVrFlt')
lcTrType = loOGScroll.laOGvrFlt[lnTrTypePos,6]
IF ASCAN(laOGObjType,'LCTRNNO') # 0
  lnPos = ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,'LCTRNNO'),1)
  laOGObjCnt[lnPos] = PADR(lcRpSort,1) <> 'G' AND !EMPTY(lcTrType)
  LCTRNNO = IIF(!EMPTY(lcTrType),LCTRNNO,'')
  =lfOGShowGet('LCTRNNO')
ENDIF


******************************************************************************************
*! Name      : lfToglShowTrnNo
*! Developer : Tarek Mohamed Ibrahim
*! Date      : 08/10/2011
*! Purpose   : Check if tag exists
******************************************************************************************
FUNCTION lfTagFound
PARAMETERS lcTag,lcAlias
LOCAL lnSlct,lnTag,llExists
lnSlct = SELECT(0)
llExists = .F.
lcTag = ALLTRIM(UPPER(lcTag))
SELECT (lcAlias)
lnI = 1
DO WHILE !EMPTY(TAG(lnI))
  IF lcTag == TAG(lnI)
    llExists = .T.
    EXIT
  ENDIF
  lnI = lnI + 1
ENDDO 

SELECT (lnSlct)
RETURN llExists

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

************************************************************
*! Name      : lfGetOpenAmt
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 02/17/2014
*! Purpose   : add a line for the open amount balance for the selected vendor
*! B610677,1
************************************************************
FUNCTION lfGetOpenAmt
LOCAL lnSlct,laVend,lnI,lcRpExp,lcBefore,lnPos
lnSlct = SELECT(0)

lnVendPos = lfGetPos('APDIST.CVENDCODE','laOgVrFlt')
lnGlAcctPos = lfGetPos('APDIST.CAPDGLACT','laOgVrFlt')
lnTrTypePos = lfGetPos('APDIST.CAPDTRTYP','laOgVrFlt')
lnSessNoPos = lfGetPos('APDIST.CAPSESSNO','laOgVrFlt')
lnTrDatePos = lfGetPos('APDIST.DAPDTRDAT','laOgFxFlt')
lcVend = loOGScroll.laOGvrFlt[lnVendPos,6]
lcGlAcct = loOGScroll.laOGvrFlt[lnGlAcctPos,6]
lcTrType = loOGScroll.laOGvrFlt[lnTrTypePos,6]
lcTrDate = loOGScroll.laOGfxFlt[lnTrDatePos,6]
lcSessNo = loOGScroll.laOGvrFlt[lnSessNoPos,6]

lcPTrD = AT('|',lcTrDate)+1
lcTrDateFrom = SUBSTR(lcTrDate,1,lcPTrD-2)
lcTrDateTo = SUBSTR(lcTrDate,lcPTrD)

lcP = AT('|',lcSessNo)+1
lcSessFrom = SUBSTR(lcSessNo,1,lcP-2)
lcSessTo = SUBSTR(lcSessNo,lcP)

lnVen = 0
IF !EMPTY(lcVend) AND USED(lcVend)
  SELECT &lcVend
  LOCATE
  COUNT TO lnVen FOR !DELETED()
ENDIF

STORE {} TO ldFrmDate , ldEnddate
IF !EMPTY(laOGFxFlt[lnTrDatePos,6])
    lnPipPos = ATC('|',laOgFxFlt[1,6]) - 1
    ldFrmDate = CTOD(SUBSTR(laogFxFlt[1,6],1,lnPipPos))
    ldEnddate = CTOD(SUBSTR(laogFxFlt[1,6],lnPipPos+ 2,LEN(laOGFxFlt[lnTrDatePos,6])))
ENDIF

lcRpExp = loOgScroll.lcRpExp

lcRpExp = lcRpExp + IIF(EMPT(lcRpExp) , '' , ' AND ' )  + " APDIST.nApdAmnt <> 0  "
lcIncI = "MHNP" + IIF(llRpIncVdInv,"I","")
*!* E303659,1 AEG 04/07/2016 modify ap jornal to show voided invoices only [Start ] 
*!*	lcRpExp = lcRpExp + IIF(EMPT(lcRpExp) , '' , ' AND ' )  + ;
*!*	                    "(APDIST.capdstat <> 'V' .OR. "+;
*!*	                    "(APDIST.capdstat = 'V' .AND. APDIST.cApdTrTyp $ '&lcIncI' ))"


lcRpExp = lcRpExp + IIF(EMPT(lcRpExp) , '' , ' AND ' )  + ;
                     "( "+IIF(llRpShVdInv .AND. llRpIncVdInv,"","APDIST.capdstat <> 'V' .OR.") +;
                    "(APDIST.capdstat = 'V' .AND. APDIST.cApdTrTyp $ '&lcIncI' ))"


*!* E303659,1 AEG 04/07/2016 modify ap jornal to show voided invoices only [END ] 

lcRpExp = lcRpExp + IIF(!llRpPrven AND lcRpSort = 'V',IIF(EMPT(lcRpExp) , '' , ' AND ' )  + " !EMPTY(APDIST.cvendcode) " ,'')
lcRpExp = lcRpExp + IIF(lcRpRel = 'B','',;
                         IIF(EMPTY(lcRpExp),'',' AND ')+IIF(lcRpRel='U','!','')+"APDIST.lApdPost")
lcRpExp = STRTRAN(lcRpExp,'.AND.',' AND ')
lcRpExp = STRTRAN(lcRpExp,'  ',' ')
IF !EMPTY(lcTrnNo)
   lcRpExp = lcRpExp + IIF(EMPT(lcRpExp) , '' , ' AND ' ) + ;
                      " APDIST.cApdRef= '"+lcTrnNo+"'"
ENDIF 

*- Remove the BETWEEN ( start date, end date )
lcBefore = SUBSTR(lcrpexp,1,AT("BETWEEN(",lcrpexp)-1)
lcrpexp = SUBSTR(lcrpexp,AT("BETWEEN(",lcrpexp))
lnPos = AT(")",lcrpexp,2)
lcRpexp = STUFF(lcRpexp ,1,lnPos," .T. ")
lcRpexp = lcBefore + lcRpexp 

*After collecting data loop through the selected vendor and sum from the apdist the neqvamt for date prior to the FROM date 
SELECT APDIST
IF !FILE(gcWorkDir +lcJourTmp+ '.CDX') OR !lfTagFound('cJourTagV','APDIST')
  INDEX ON cVendCode+capdtrtyp TAG cJourTagV OF (gcWorkDir +  lcJourTmp + '.CDX')
ELSE
  SET ORDER TO TAG cJourTagV OF (gcWorkDir +  lcJourTmp + '.CDX')
ENDIF

*- Add a new record to the temp cursor with the summed open amt such that it is located on top of the collected lines for the vendor 
*- Make the description of this line ‘Opening amount’

DIMENSION laVend[1]
*B610677,4 TMI 03/13/2014 13:33 [Start] don't get the array of vendors from the lcTempFile, it does not cover the case that there is an opening balance while 
*                                       there is no transactions for the selected criteria, get it instead from APVENDOR or LCVEND
*SELECT DISTINCT CVENDCODE FROM DBF(lcTempFile) INTO ARRAY laVend
lcVendSrc = IIF(lnVen = 0 , 'APVENDOR' , lcVend )
SELECT DISTINCT CVENDCODE FROM &lcVendSrc INTO ARRAY laVend
*B610677,4 TMI 03/13/2014 13:33 [End  ] 
SELECT &lcTempFile
SCATTER MEMVAR blank 
m.CAPDGLACT = 'Opening Balance'
FOR lnI = 1 TO ALEN(laVend,1)  
  m.NEQVAMNT = 0
  m.CVENDCODE = laVend[lnI]
  SELECT APDIST
  =SEEK(laVend[lnI])  
  llNoStartDate = EMPTY(CHRTRAN(lcTrDateFrom,'\',''))
  SUM NEQVAMNT TO m.NEQVAMNT REST WHILE cVendCode+capdtrtyp = laVend[lnI] FOR IIF(llNoStartDate,.T.,DAPDTRDAT < CTOD(lcTrDateFrom) ) AND EVALUATE(lcRpExp)
  IF m.NEQVAMNT <> 0 
    INSERT INTO &lcTempFile FROM MEMVAR 
  ENDIF 
ENDFOR

SELECT (lnSlct)
*- End of lfGetOpenAmt.