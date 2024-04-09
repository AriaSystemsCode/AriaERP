***********************************************************************
*:  Program file : ARIMPRE.PRG
*:  Program desc.: Import retail File
*:         System: Aria 4XP
*:      Developer: Mariam Mazhar[MMT]
*:           Date: 08/12/2015
*:      Reference: C201701.Exe[T20150630.0002]
*:************************************************************************
*: Modifications:
*C201701,2 MMT 09/07/2015 Modify the program as per specs change - Issue#2[T20150630.0002]
*C201701,3 MMT 09/13/2015 Modify the program as per specs change - Issue#2[T20150630.0002]
*:************************************************************************
lcExpr = gfOpGrid('ARIMPRET' , .T.)&&,.F.,.F.,.T.,.T.)  
*!*************************************************************
*! Name      : lfvAccount
*! Developer : Mariam Mazhar[MMT]
*! Date      : 08/11/2015
*! Purpose   : Validate Account
*!*************************************************************
FUNCTION lfvAccount

IF EMPTY(ALLTRIM(lcRpAcct))
  RETURN ''
ENDI

PRIVATE lcObjVal
PRIVATE lnAlsNo,lcCustOrd,lcObjName
lnAlsNo = SELECT(0)
IF !USED('CUSTOMER')
  =gfOpenTable('Customer','Customer')
ENDIF
SELECT CUSTOMER
lcCustOrd = ORDER()
gfSetOrder('CUSTOMER')
lcOldVal = lcRpAcct 
*IF The user want to Browse or if the Account he entered is not in the file
IF '?' $ lcRpAcct .OR. (!EMPTY(lcRpAcct) .AND. !gfSEEK('M' +lcRpAcct, 'CUSTOMER'))
  llObjRet = CusBrowM(@lcRpAcct, '' , 'M')
  lcRpAcct= IIF(llObjRet , lcRpAcct, '')
  IF lcRpAcct <> lcOldVal 
    lcRpStore = ''
  ENDIF   
ELSE 
  lcRpStore = ''
ENDIF

SELECT CUSTOMER
gfSETORDER(lcCustOrd)
SELECT(lnAlsNo)
*!*************************************************************
*! Name      : lfvPath
*! Developer : Mariam Mazhar[MMT]
*! Date      : 08/11/2015
*! Purpose   : Validate Input File
*!*************************************************************
FUNCTION lfvPath
IF ALLTRIM(lcRpPath)  = "?" OR (!EMPTY(lcRpPath) AND !FILE(lcRpPath))
  lcRpPath  = GETFILE('XLS','Select source file')
ENDIF
*!*************************************************************
*! Name      : lfvHist
*! Developer : Mariam Mazhar[MMT]
*! Date      : 08/11/2015
*! Purpose   : Validate History Path
*!*************************************************************
FUNCTION lfvHist
IF ALLTRIM(lcRpHist)  = "?" OR (!EMPTY(lcRpHist) aNd !DIRECTORY(lcRpHist))
  lcRpHist = GETDIR('','Select History File Location')
ENDIF
*!*************************************************************
*! Name      : lfvStore
*! Developer : Mariam Mazhar[MMT]
*! Date      : 08/11/2015
*! Purpose   : Validate Store
*!*************************************************************
FUNCTION lfvStore
IF EMPTY(ALLTRIM(lcRpStore))
  RETURN ''
ENDIf

IF EMPTY(lcRpAcct)
  =gfModalGen('TRM00000B00000',.F.,.F.,.F.,'please select customer first')
  lcRpStore = ''
  RETURN -1
ENDIF

lcCurrType = 'M'
lcCurrAccount = lcRpAcct
IF '?' $ This.Keytextbox.Value .OR. (!EMPTY(lcRpStore) .AND. !gfSeek('S'+lcRpAcct+lcRpStore, 'CUSTOMER'))
  SELECT customer
  SET KEY TO
  lcOldStore  = lcRpStore
  xStore = lcRpStore
  IF !CUSBROWS(lcCurrAccount,.T.)
    IF gfSeek(lcCurrType+lcCurrAccount+lcOldStore,'Customer')
      lcRpStore = lcOldStore
    ELSE
      lcRpStore = ''
    ENDIF
  ELSE
    lcRpStore = xStore
  ENDIF
ENDIF    
*!*************************************************************
*! Name      : lfvWarehouse
*! Developer : Mariam Mazhar[MMT]
*! Date      : 08/11/2015
*! Purpose   : Validate warehouse
*!*************************************************************
FUNCTION lfvWarehouse
IF EMPTY(ALLTRIM(lcRpLoc))
  RETURN ''
ENDIF

lcObjName = OGSYS18()      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(OGSYS18())      && Varible to hold  the value of the current GET field
lcOldVal = lcObjVal 
IF !USED('WAREHOUS')
  =gfOpenTable('WAREHOUS','WAREHOUS')
ENDIF
*-- IF The user want to Browse or if the Warehouse he entered is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !gfSEEK(lcObjVal , 'WAREHOUS'))
  lcObjVal = gfBrowWare(.T.)
  lcObjVal = IIF(EMPTY(lcObjVal) , IIF(gfSeek(lcOldVal , 'WAREHOUS'),lcOldVal ,'') , lcObjVal)
  &lcObjName. = lcObjVal
  lcRpLoc = lcObjVal 
  IF lcOldVal <> lcRpLoc 
    lcRpBinLoc= ''
  ENDIF 
ELSE
 lcRpBinLoc= ''
ENDIF    && End of IF
*!*************************************************************
*! Name      : lfvBinLoc
*! Developer : Mariam Mazhar[MMT]
*! Date      : 08/11/2015
*! Purpose   : Validate Bin location
*!*************************************************************
FUNCTION lfvBinLoc
IF EMPTY(ALLTRIM(lcRpBinLoc))
  RETURN ''
ENDIF

IF EMPTY(lcRpLoc)
  =gfModalGen('TRM00000B00000',.F.,.F.,.F.,'please select location first')
  lcRpBinLoc= ''
  RETURN -1
ENDIF
*IF !FILE(oAriaApplication.WorkDir+lcBinTemp+".DBF")
  LOCAL lcSvOrd,lcWH
  IF !USED('WHSSTYLOC')
    =gfOpenTable('WHSLOC','WHSLOCST','SH','WHSSTYLOC')
  ENDIF
  lcOldValue = lcRpBinLoc
  lcSvOrd = ORDER('WHSLOC')
  SELECT WHSSTYLOC
  SET ORDER TO WHSLOCST
  SET KEY TO SPACE(25)+lcRpLoc 
  COPY TO (oAriaApplication.WorkDir+lcBinTemp) FIELDS CWARECODE,CLOCATION
  SET KEY TO 
  SELECT 0
  USE (oAriaApplication.WorkDir+lcBinTemp) EXCLUSIVE
  INDEX ON CWARECODE+CLOCATION TAG WHSLOC  
*!*	ELSE
*!*	  IF !USED(lcBinTemp)  
*!*	    USE (oAriaApplication.WorkDir+lcBinTemp) EXCLUSIVE IN 0 ORDER 1
*!*	  ENDIF
*!*	ENDIF 
  
SELECT (lcBinTemp)
LOCATE 
DIMENSION laTempData[1]
laTempData[1]    =''
IF !AriaBrow(.F.,'Bin Locations',.F.,.F.,.F.,.F.,'',.T.,'CLOCATION','laTempData')
  lcRpBinLoc = IIF (!EMPTY(lcOldValue),IIF(gfSeek(SPACE(25)+lcRpLoc+lcOldValue,'WHSSTYLOC','WHSLOCST'),lcOldValue,''),'')
  USE IN (lcBinTemp)
  RETURN ''
ELSE
  lcRpBinLoc = laTempData[1]    
ENDIF
USE IN (lcBinTemp)
*!*************************************************************
*! Name      : lfwImpWhen
*! Developer : Mariam Mazhar[MMT]
*! Date      : 08/11/2015
*! Purpose   : OG When function
*!*************************************************************
FUNCTION lfwImpWhen
*lcRpReason
IF !USED('Codes_DEF')
  =gfOpenTable('Codes','CCODE_NO','SH','Codes_DEF')
ENDIF
lcDefaReason = ''
IF gfSeek('D'+'CADJREASON','Codes_DEF','CCODE_NO')
  lcDefaReason = Codes_DEF.CCODE_NO
ENDIF
IF !EMPTY(lcDefaReason)
  lcRpReason  = lcDefaReason
ENDIF
*!*************************************************************
*! Name      : lfCreatExp
*! Developer : Mariam Mazhar[MMT]
*! Date      : 08/11/2015
*! Purpose   : Validate input info and start processing
*!*************************************************************
FUNCTION lfCreatExp
lcGlFYear  = ''
lcGlPeriod = ''
llRet = .t.
IF (EMPTY(lcRpPath) OR !FILE(lcRpPath))
  =gfModalGen('TRM00000B00000',.F.,.F.,.F.,'Invalid source file, cannot proceed.')  
  RETURN .F.
ENDIF

IF (EMPTY(lcRpHist) OR !DIRECTORY(lcRpHist))
  =gfModalGen('TRM00000B00000',.F.,.F.,.F.,'Invalid History directory, cannot proceed.')  
  RETURN .F.
ENDIF

*!*	IF EMPTY(ALLTRIM(lcRpAcct))
*!*	  =gfModalGen('TRM00000B00000',.F.,.F.,.F.,'No customer is selected, cannot proceed.')  
*!*	  RETURN .F.
*!*	ENDIF

*!*	IF EMPTY(ALLTRIM(lcRpLoc))
*!*	  =gfModalGen('TRM00000B00000',.F.,.F.,.F.,'No warehouse is selected, cannot proceed.')  
*!*	  RETURN .F.
*!*	ENDIF


*!*	IF EMPTY(ALLTRIM(lcRpBinLoc))
*!*	  =gfModalGen('TRM00000B00000',.F.,.F.,.F.,'No Bin location is selected, cannot proceed.')  
*!*	  RETURN .F.
*!*	ENDIF

IF EMPTY(lcRpReason)
  =gfModalGen('TRM00000B00000',.F.,.F.,.F.,'No Adjustment reason is selected, cannot proceed.')  
  RETURN .F.
ENDIF

SELECT 0
IMPORT from (lcRpPath) TYPE XL5
lcImportAlias = ALIAS()
*Mt
ALTER Table (lcImportAlias) Alter COLUMN I C(25)
ZAP IN (lcImportAlias)
APPEND FROM (lcRpPath) TYPE XL5
SELECT (lcImportAlias)
*mt
LOCATE 
IF EOF() OR RECCOUNT()=1
  =gfModalGen('TRM00000B00000',.F.,.F.,.F.,'The Source file is empty, cannot proceed.')  
  RETURN .F.
ENDIF
lcOrderNumSeq  = ALLTRIM(gfGetMemVar('M_GenOrNum',oAriaApplication.ActiveCompanyID))
llFileHasProb = .F.
lfCreateTmp()
=lfCrtAdjstTmp()
lfProcessFile()
lfVerifyFileData()
IF llFileHasProb
  SELECT TMPSTR
  DO FORM (oAriaApplication.ScreenHome + 'AR\arcninl.SCX')
  USE IN TMPSTR
ELSE
  lfCreateInvCr()  
ENDIF  
IF USED(lcScreenTemp) AND RECCOUNT(lcScreenTemp)<> 0
  SELECT (lcScreenTemp)
  SCAN
    SCATTER MEMO MEMVAR
    SELECT INVTADJ
    =gfAppend('INVTADJ', .T.)  
  ENDSCAN
  SELECT INVTADJ
  =gfTableUpdate()
ENDIF


*!*************************************************************
*! Name      : lfCreateTmp
*! Developer : Mariam Mazhar[MMT]
*! Date      : 09/08/2011
*! Purpose   : Create Tmp.
*!*************************************************************
FUNCTION lfCreateTmp


DIMENSION laRangeArr[2,4]
laRangeArr[1,1] = 'TrnType'
laRangeArr[1,2] = 'C'
laRangeArr[1,3] = 1
laRangeArr[1,4] = 0
laRangeArr[2,1] = 'TrnNum'
laRangeArr[2,2] = 'C'
laRangeArr[2,3] = 6
laRangeArr[2,4] = 0
=gfCrtTmp(lcTransRange ,@laRangeArr,'TrnType+TrnNum',lcTransRange)

DIMENSION laStuctTemp[17,4]
laStuctTemp[1,1] = 'STYLE'
laStuctTemp[1,2] = 'C'
laStuctTemp[1,3] = 19
laStuctTemp[1,4] = 0

laStuctTemp[2,1] = 'PRICE'
laStuctTemp[2,2] = 'N'
laStuctTemp[2,3] = 10
laStuctTemp[2,4] = 2


laStuctTemp[3,1] = 'QTY1'
laStuctTemp[3,2] = 'N'
laStuctTemp[3,3] = 6
laStuctTemp[3,4] = 0

laStuctTemp[4,1] = 'QTY2'
laStuctTemp[4,2] = 'N'
laStuctTemp[4,3] = 6
laStuctTemp[4,4] = 0

laStuctTemp[5,1] = 'QTY3'
laStuctTemp[5,2] = 'N'
laStuctTemp[5,3] = 6
laStuctTemp[5,4] = 0

laStuctTemp[6,1] = 'QTY4'
laStuctTemp[6,2] = 'N'
laStuctTemp[6,3] = 6
laStuctTemp[6,4] = 0

laStuctTemp[7,1] = 'QTY5'
laStuctTemp[7,2] = 'N'
laStuctTemp[7,3] = 6
laStuctTemp[7,4] = 0

laStuctTemp[8,1] = 'QTY6'
laStuctTemp[8,2] = 'N'
laStuctTemp[8,3] = 6
laStuctTemp[8,4] = 0

laStuctTemp[9,1] = 'QTY7'
laStuctTemp[9,2] = 'N'
laStuctTemp[9,3] = 6
laStuctTemp[9,4] = 0

laStuctTemp[10,1] = 'QTY8'
laStuctTemp[10,2] = 'N'
laStuctTemp[10,3] = 6
laStuctTemp[10,4] = 0

laStuctTemp[11,1] = 'Date'
laStuctTemp[11,2] = 'D'
laStuctTemp[11,3] = 8
laStuctTemp[11,4] = 0

laStuctTemp[12,1] = 'Time'
laStuctTemp[12,2] = 'C'
laStuctTemp[12,3] = 10
laStuctTemp[12,4] = 0

laStuctTemp[13,1] = 'Season'
laStuctTemp[13,2] = 'C'
laStuctTemp[13,3] = 6
laStuctTemp[13,4] = 0

laStuctTemp[14,1] = 'CDivision'
laStuctTemp[14,2] = 'C'
laStuctTemp[14,3] = 6
laStuctTemp[14,4] = 0

laStuctTemp[15,1] = 'Account'
laStuctTemp[15,2] = 'C'
laStuctTemp[15,3] = 5
laStuctTemp[15,4] = 0

laStuctTemp[16,1] = 'Location'
laStuctTemp[16,2] = 'C'
laStuctTemp[16,3] = 6
laStuctTemp[16,4] = 0

laStuctTemp[17,1] = 'Refer'
laStuctTemp[17,2] = 'C'
laStuctTemp[17,3] = 30
laStuctTemp[17,4] = 0

=gfCrtTmp(lcStyleDet ,@laStuctTemp,'STYLE+Account+Location',lcStyleDet)


DIMENSION laStuctArr[11,4]

laStuctArr[1,1] = 'UPC'
laStuctArr[1,2] = 'C'
laStuctArr[1,3] = 15
laStuctArr[1,4] = 0

laStuctArr[2,1] = 'DateTime'
laStuctArr[2,2] = 'C'
laStuctArr[2,3] = 20
laStuctArr[2,4] = 0

laStuctArr[3,1] = 'QTY'
laStuctArr[3,2] = 'N'
laStuctArr[3,3] = 6
laStuctArr[3,4] = 0

laStuctArr[4,1] = 'PRICE'
laStuctArr[4,2] = 'N'
laStuctArr[4,3] = 10
laStuctArr[4,4] = 2

laStuctArr[5,1] = 'lIgnore'
laStuctArr[5,2] = 'L'
laStuctArr[5,3] = 1
laStuctArr[5,4] = 0

laStuctArr[6,1] = 'STYLE'
laStuctArr[6,2] = 'C'
laStuctArr[6,3] = 19
laStuctArr[6,4] = 0

laStuctArr[7,1] = 'SIZE'
laStuctArr[7,2] = 'C'
laStuctArr[7,3] = 2
laStuctArr[7,4] = 0


laStuctArr[8,1] = 'Location'
laStuctArr[8,2] = 'C'
laStuctArr[8,3] = 6
laStuctArr[8,4] = 0

laStuctArr[9,1] = 'Account'
laStuctArr[9,2] = 'C'
laStuctArr[9,3] = 5
laStuctArr[9,4] = 0

laStuctArr[11,1] = 'LineNo'
laStuctArr[11,2] = 'N'
laStuctArr[11,3] = 5
laStuctArr[11,4] = 0

laStuctArr[10,1] = 'Refer'
laStuctArr[10,2] = 'C'
laStuctArr[10,3] = 30
laStuctArr[10,4] = 0


=gfCrtTmp(lcFileData ,@laStuctArr,'UPC',lcFileData)
*!*	SELECT(lcFileData)
*!*	INDEX ON UPC TAG 'FileDesc' ADDITIVE
*!*	SET ORDER TO (lcFileData)

IF USED('TMPSTR')
  USE IN TMPSTR
ENDIF
CREATE CURSOR TMPSTR (mStrRep M(10))
SELECT TMPSTR
APPEND BLANK
REPLACE mStrRep WITH REPLICATE('*',68) + CHR(13) +;
  "*                                Error log                                                                 *" + CHR(13) +;
  REPLICATE('*',68) + CHR(13) + ' ' + CHR(13)
  

*!*************************************************************
*! Name      : lfProcessFile
*! Developer : Mariam Mazhar[MMT]
*! Date      : 08/11/2015
*! Purpose   : Read the Excel file 
*!*************************************************************
FUNCTION lfProcessFile

SELECT(lcImportAlias)
LOCATE 
*mt
*SCAN FOR RECNO() > 1 AND !EMPTY(ALLTRIM(&lcImportAlias..H)) AND VAL(&lcImportAlias..Q) <> 0
SCAN FOR RECNO() > 1 AND  VAL(&lcImportAlias..E) <> 0
*mt
  INSERT INTO (lcFileData) (UPC,DateTime,QTY,PRICE,lIgnore,STYLE,SIZE,Location,Account,LineNo,Refer) VALUES ;
             (ALLTRIM(&lcImportAlias..H) ,&lcImportAlias..I,VAL(&lcImportAlias..E),;
             ABS(VAL(&lcImportAlias..L)/VAL(&lcImportAlias..E)),.F.,'','',&lcImportAlias..C,&lcImportAlias..C,RECNO(lcImportAlias),&lcImportAlias..R)
ENDSCAN
SELECT (lcFileData)
LOCATE 

*!*************************************************************
*! Name      : lfVerifyFileData  
*! Developer : Mariam Mazhar[MMT]
*! Date      : 08/11/2015
*! Purpose   : Verify the data in Excel file
*!*************************************************************
FUNCTION lfVerifyFileData  
REPLACE mStrRep WITH mStrRep + CHR(13) +CHR(10)+CHR(13) +CHR(10)+ CHR(13) +CHR(10) IN  TMPSTR

IF !USED('STYLE')
  =gfOpenTable('STYLE','STYLE')
ENDIF
IF !USED('STYLEUPC')
  =gfOpenTable('STYLEUPC','STYUPCN')
ENDIF

IF !USED('Customer')
  =gfOpenTable('Customer','Customer')
ENDIF

IF !USED('WareHous')
  =gfOpenTable('WareHous','WareHous')
ENDIF
*CWARECODE+CLOCATION+STYLE+COLOR
IF !USED('WHSLOC')
  =gfOpenTable('WHSLOC','WHSLOC')
ENDIF


llHasProblem = .F.
SELECT (lcFileData)
LOCATE 
SCAN 
  IF EMPTY(ALLTRIM(&lcFileData..UPC)) OR ((!gfSeek(ALLTRIM(&lcFileData..UPC),'STYLEUPC') OR (gfSeek(ALLTRIM(&lcFileData..UPC),'STYLEUPC') AND !gfSeek(STYLEUPC.STYLE,'STYLE'))))
    REPLACE mStrRep WITH mStrRep +"Line No.:"+ALLTRIM(STR(&lcFileData..LineNo))+"   Barcode: "+&lcFileData..UPC+'   '+ALLTRIM(&lcFileData..SIZE)+" does not exist in the UPC file"+ CHR(13) +CHR(10) IN  TMPSTR
    llHasProblem = .T.
    REPLACE lIgnore WITH .T. IN (lcFileData)
  ELSE
   REPLACE STYLE WITH STYLEUPC.STYLE,;
  		   lIgnore WITH .F.    ,;
           SIZE  WITH STYLEUPC.SIZE IN (lcFileData)  
  ENDIF
  IF !gfSeek('M'+&lcFileData..Account,'Customer','Customer')
    REPLACE mStrRep WITH mStrRep +"Line No.:"+ALLTRIM(STR(&lcFileData..LineNo))+"   Customer: "+&lcFileData..Account+'   '+" does not exist in the Customer file"+ CHR(13) +CHR(10) IN  TMPSTR
    llHasProblem = .T.
    REPLACE lIgnore WITH .T. IN (lcFileData)
  ENDIF
  IF !gfSeek(&lcFileData..Location,'WareHous','WareHous')
    REPLACE mStrRep WITH mStrRep +"Line No.:"+ALLTRIM(STR(&lcFileData..LineNo))+"   Location: "+&lcFileData..Location+'   '+" does not exist in the Locations file"+ CHR(13) +CHR(10) IN  TMPSTR
    llHasProblem = .T.
    REPLACE lIgnore WITH .T. IN (lcFileData)
  ELSE
    IF !gfSeek(&lcFileData..Location+PADR("STORE",10),'WHSLOC','WHSLOC')
      REPLACE mStrRep WITH mStrRep +"Line No.:"+ALLTRIM(STR(&lcFileData..LineNo))+"   Bin: STORE is not for Location: "+&lcFileData..Location+'   '+" does not exist in the Bin Locations file"+ CHR(13) +CHR(10) IN  TMPSTR
      llHasProblem = .T.
      REPLACE lIgnore WITH .T. IN (lcFileData)
    ENDIF
  ENDIF
ENDSCAN 

IF !llFileHasProb
  llFileHasProb = llHasProblem
ENDIF
*!*************************************************************
*! Name      : lfCreateInvCr
*! Developer : Mariam Mazhar[MMT]
*! Date      : 08/11/2015
*! Purpose   : Create Invoice or CR
*!*************************************************************
FUNCTION lfCreateInvCr
llUse_GL = ALLTRIM(gfGetMemVar('M_Link_GL',oAriaApplication.ActiveCompanyID)) ='Y'
IF !USED('WAREHOUS')
  =gfOpenTAble('WAREHOUS','WAREHOUS')
ENDIF
lcAdjAcct = ' '
IF llUse_GL AND !EMPTY(lcRpReason)
  DECLARE laTrmRltFd[1,2]
  laTrmRltFd[1,1] = 'GLACCOUNT'
  laTrmRltFd[1,2] = 'lcAdjAcct'
  = gfRltFld(lcRpReason, @laTrmRltFd , "CADJREASON")
ENDIF
loDInv = ''
loCrMem = ''
opross = CREATEOBJECT('ariaprogressbar')
lcRpBinLoc	= PADR("STORE",10)
SELECT (lcFileData)  
LOCATE 
LOCATE FOR QTY < 0
IF FOUND()
  lfCrtInvCrMemo('R')
ENDIF
lfVerifyAdjQty()
SELECT (lcFileData)  
LOCATE 
LOCATE FOR QTY > 0
IF FOUND()
  lfCrtInvCrMemo('I')
ENDIF


IF TYPE('loDInv') = 'O'
  loDInv.RELEASE()
ENDIF
IF TYPE('loCrMem') = "O"
  loCrMem.RELEASE()
ENDIF
IF UPPER((oAriaApplication.ApplicationHome + 'ar\ARCNINC.FXP')) $ UPPER(SET("Procedure"))
  RELEASE PROCEDURE (oAriaApplication.ApplicationHome + 'ar\ARCNINC.FXP')
ENDIF
oPross = NULL
LOOGSCROLL.PARENT.VISIBLE = .T.
SELECT  (lcTransRange)
LOCATE
IF !EOF()
  IF USED('TMPSTR')
    USE IN 'TMPSTR'
  ENDIF
  CREATE CURSOR TMPSTR (mStrRep M(10))
  SELECT TMPSTR 
  APPEND BLANK
  REPLACE mStrRep WITH REPLICATE('*',68) + CHR(13) +;
    "*                                Processing Result                                                                 *" + CHR(13) +;
    REPLICATE('*',68) + CHR(13) + ' ' + CHR(13)

  SELECT  (lcTransRange)
  SCAN
    REPLACE mStrRep WITH  mStrRep +" "+IIF(&lcTransRange..TrnType = 'R','Credit memo: ','Invoice: ')+&lcTransRange..TrnNum+ CHR(13) +CHR(10) IN TMPSTR
  ENDSCAN
  SELECT TMPSTR
  DO FORM (oAriaApplication.ScreenHome + 'AR\arcninl.SCX')
  TRY 
    COPY FILE (lcRpPath) TO (lcRpHist+JUSTFNAME(lcRpPath))
    ERASE (lcRpPath)
  CATCH
    =gfModalGen('TRM00000B00000',.F.,.F.,.F.,'Could not move the source file to the history directory.')
  ENDTRY   
ENDIF
  
*!*************************************************************
*! Name      : lfCrtInvCrMemo
*! Developer : Mariam Mazhar[MMT]
*! Date      : 08/11/2015
*! Purpose   : Function to create Invoice or Credit memo
*!*************************************************************
FUNCTION lfCrtInvCrMemo
LPARAMETERS lnTranType

DECLARE laTaxRat[1,2]
laTaxRat[1,1] = 'NTAXRATE'
laTaxRat[1,2] = 'lnTaxRat'
lnTaxRat = 0
SELECT (lcStyleDet)
ZAP
SELECT (lcFileData)
lnRecordCnt = 0
COUNT FOR IIF(lnTranType ='R',QTY < 0,QTY > 0) TO lnRecordCnt 
IF lnRecordCnt > 0
  oPross.TotalProgress = lnRecordCnt
ENDIF

lnCntLine = 0
SCAN FOR IIF(lnTranType ='R',QTY < 0,QTY > 0)
  lnCntLine = lnCntLine + 1
  LOOGSCROLL.PARENT.VISIBLE = .F.
  oPross.lblFirstLabel.CAPTION = "Creating Transactions..."
  oPross.CurrentProgress(lnCntLine)
  oPross.AUTOCENTER = .T.
  opross.VISIBLE = .T.
  oPross.SHOW()

  lcQtyPos = ALLTRIM(&lcFileData..SIZE)
  m.Price = &lcFileData..Price
  m.STYLE = &lcFileData..STYLE
  =gfSeek(m.STYLE,'STYLE','STYLE')
  
  lnTaxRat = 0
  IF !EMPTY(STYLE.CTAXCODE)
    = gfRltFld(STYLE.CTAXCODE , @laTaxRat , 'CTAXCODE')
  ENDIF
  IF lnTaxRat > 0
    *C201701,3 MMT 09/13/2015 Modify the program as per specs change - Issue#2[T20150630.0002][Start]
    *m.Price = m.Price/(1+(lnTaxRat/100))
    *C201701,3 MMT 09/13/2015 Modify the program as per specs change - Issue#2[T20150630.0002][end]
  ENDIF
  IF !SEEK(m.STYLE+&lcFileData..Account+&lcFileData..Location,lcStyleDet)
    INSERT INTO (lcStyleDet) (STYLE,qty&lcQtyPos.,PRICE,date,Time,Season,CDIVISION,Account,Location,Refer) VALUES ;
      (m.STYLE,ABS(&lcFileData..QTY),m.Price,CTOD(SUBSTR(&lcFileData..DateTime,1,10)),SUBSTR(&lcFileData..DateTime,11),STYLE.Season,;
      STYLE.CDIVISION,&lcFileData..Account,&lcFileData..Location,&lcFileData..Refer)
  ELSE
    lnCurSelAls = SELECT()
    SELECT (lcStyleDet)
    LOCATE REST WHILE STYLE+Account+Location = m.STYLE+&lcFileData..Account+&lcFileData..Location FOR PRICE = m.Price AND ;
    date = CTOD(SUBSTR(&lcFileData..DateTime,1,10)) AND Time = SUBSTR(&lcFileData..DateTime,11)
    IF FOUND()
      REPLACE qty&lcQtyPos. WITH qty&lcQtyPos.+ ABS(&lcFileData..QTY) IN (lcStyleDet)
    ELSE
      INSERT INTO (lcStyleDet) (STYLE,qty&lcQtyPos.,PRICE,date,Time,Season,CDIVISION,Account,Location,Refer) VALUES ;
        (m.STYLE,ABS(&lcFileData..QTY),m.Price,CTOD(SUBSTR(&lcFileData..DateTime,1,10)),SUBSTR(&lcFileData..DateTime,11),;
        STYLE.Season,STYLE.CDIVISION,&lcFileData..Account,&lcFileData..Location,&lcFileData..Refer)
    ENDIF
    SELECT(lnCurSelAls)
  ENDIF
ENDSCAN


SELECT (lcStyleDet)
LOCATE
IF lnTranType = 'I'
  **Check if Auto Adjustment is required
  ***
    
  ***
  SELECT DISTINCT  SEASON,CDIVISION,Date,Time,Account,Location  FROM (lcStyleDet) INTO CURSOR  'DIFFTRAN'
ELSE
  SELECT DISTINCT  CDIVISION,Date,Time,Account,Location    FROM (lcStyleDet) INTO CURSOR  'DIFFTRAN'
ENDIF

SELECT (lcStyleDet)
LOCATE
IF !EOF()
  lcOldProcd = SET("Procedure")
  lcOldProcd  = (oAriaApplication.ApplicationHome + 'ar\ARCNINC.FXP') +IIF(!EMPTY(lcOldProcd),",",'')+lcOldProcd
  SET PROCEDURE TO &lcOldProcd.
ENDIF

lnChoice = 1  
IF lnTranType = 'I'
  SELECT DIFFTRAN
  SCAN FOR !EMPTY(SEASON) AND !EMPTY(CDIVISION) AND !EMPTY(Account) AND !EMPTY(Location)
    lcRpLoc = DIFFTRAN.Location
    lcRpAcct = DIFFTRAN.Account
    SELECT (lcStyleDet)
    IF llUse_GL
      =gfOpenFile(oAriaApplication.DataDir+'GLDist','GLDistAc','SH')
      SELECT GLDist
      lcTmpGlDt = IIF(TYPE('lcXMLFileName') <> 'C',loogScroll.gfTempName(),gfTempName())
      lcWorkDir = oAriaApplication.workdir
      COPY STRU TO (lcWorkDir+lcTmpGlDt+'.DBF')
      USE (lcWorkDir+lcTmpGlDt+'.DBF') IN 0 EXCLUSIVE
      SELECT (lcTmpGlDt)
    ENDIF
*!*	    SELECT (lcStyleDet)
*!*	    LOCATE
*!*	    SCAN 
      lnChoice = 1
      IF TYPE('loDInv') <> "O"
        DO FORM (oAriaApplication.ScreenHome+"ar\ardinv.scx") NOSHOW NAME loDInv LINKED
        loDInv.lBranchScreen=.F.
        loDInv.NAME = "AARARDINV"
        loDInv.minittriggers ()
      ENDIF
      loDInv.laSetups[18,2] = 'N'
      loDInv.mCreateTempfiles
      loDInv.DefaultWarecode = lcRpLoc
      loDInv.changemode ('A')
      WITH loDInv.AriaForm1.AriaPageframe1.Page2.InvoiceEditRegion1
        STORE 0 TO .TaxDueAmount, .Merchandisetax, .TotalCartons
      ENDWITH
      loDInv.DefaultSeason =  DIFFTRAN.SEASON 
      loDInv.DefaultDivision = DIFFTRAN.CDIVISION 
      loDInv.DefaultInvoiceDate = DIFFTRAN.Date
      loDInv.DefaultPostingDate = DIFFTRAN.Date
      
      loDInv.ariaform1.keyAccount.keytextbox.oldValue = SPACE(5)
      loDInv.ariaform1.keyAccount.keytextbox.VALUE = lcRpAcct
      loDInv.ariaform1.keyAccount.keytextbox.VALID
      loDInv.ariaform1.keySTORE.keytextbox.oldValue = SPACE(5)
      loDInv.ariaform1.keySTORE.keytextbox.VALUE = ""&&lcRpStore
      loDInv.ariaform1.keySTORE.keytextbox.VALID
      =SEEK('M'+lcRpAcct,'CUSTOMER','CUSTOMER')
      lnDisc = Customer.Disc
      lcDatse = SET("Datasession")
      SET DATASESSION TO loDInv.DATASESSIONID
      REPLACE  DiscPcnt   WITH lnDisc IN (loDInv.lcInvhdr)
      SET DATASESSION TO lcDatse
      *C201701,3 MMT 09/13/2015 Modify the program as per specs change - Issue#2[T20150630.0002][Start]
      loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.LastLineNo = 0 
      *C201701,3 MMT 09/13/2015 Modify the program as per specs change - Issue#2[T20150630.0002][End]
      SELECT (lcStyleDet)
      LOCATE      
      SCAN FOR SEASON = DIFFTRAN.SEASON AND CDIVISION = DIFFTRAN.CDIVISION AND;
               Date = DIFFTRAN.Date AND Time = DIFFTRAN.Time AND Account = DIFFTRAN.Account AND Location = DIFFTRAN.Location
        STORE '' TO M_POST_PPRD,M_SYS_DIR,M_GL_VERS,M_GL_CO
        =gfGetMemVar('M_POST_PPRD,M_SYS_DIR,M_GL_VERS,M_GL_CO',oAriaApplication.ActiveCompanyID)
        llRet = CHECKPRD(&lcStyleDet..Date,'lcGLFYear','lcGLPeriod','IN')
        loDInv.AriaForm1.AriaPageFrame1.Page1.txtNote1.Value = &lcStyleDet..Refer
*!*	        loDInv.DefaultInvoiceDate = &lcStyleDet..Date
*!*	        loDInv.DefaultPostingDate = &lcStyleDet..Date
        lnRetPriceValue = &lcStyleDet..PRICE
        loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.mResetControlSource ()
        STORE .T. TO loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.llNewline,loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.llAddLine
        loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.keyStyle.ENABLED = .T.
        loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.keyStyle.TxtItem.VALUE = &lcStyleDet..STYLE
        loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.keyStyle.VALID(.T.,0,&lcStyleDet..STYLE,SPACE(19),&lcStyleDet..STYLE)
        loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.mResetControlSource ()
        loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.txtGrossPrice.OldValue  = 0
        loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.txtGrossPrice.VALUE  = &lcStyleDet..PRICE
        loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.txtGrossPrice.Valid()
        loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.spnDiscount.oldValue = 0
        loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.spnDiscount.VALUE = 0
        loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.spnDiscount.LOSTFOCUS()
        loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.txtNetPrice.OldValue  = 0
        loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.txtNetPrice.VALUE  = &lcStyleDet..PRICE
        loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.txtNetPrice.LOSTFOCUS()
        
        loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.cntQuantity.txtQty1.CONTROLSOURCE = ''
        loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.cntQuantity.txtQty2.CONTROLSOURCE = ''
        loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.cntQuantity.txtQty3.CONTROLSOURCE = ''
        loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.cntQuantity.txtQty4.CONTROLSOURCE = ''
        loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.cntQuantity.txtQty5.CONTROLSOURCE = ''
        loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.cntQuantity.txtQty6.CONTROLSOURCE = ''
        loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.cntQuantity.txtQty7.CONTROLSOURCE = ''
        loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.cntQuantity.txtQty8.CONTROLSOURCE = ''
        loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.cntQuantity.txtTotQty.CONTROLSOURCE = ''

        FOR lnX= 1 TO 8
          lcX = STR(lnX,1)
          IF &lcStyleDet..Qty&lcX. > 0
            loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.cntQuantity.txtQty&lcX..VALUE  =  &lcStyleDet..Qty&lcX.
            loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.cntQuantity.txtQty&lcX..VALID
          ENDIF
        ENDFOR
        lnOldLevel = loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.PriceLevel 
        loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.PriceLevel  = 'A'
        lnPriceOfSty = &lcStyleDet..PRICE
        lcDatse = SET("Datasession")
        SET DATASESSION TO loDInv.DATASESSIONID
		REPLACE gros_price WITH lnPriceOfSty ,;
				disc_pcnt WITH 0,;
				price WITH lnPriceOfSty  IN (loDInv.lcInvLine)
  	    SET DATASESSION TO lcDatse
        loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.cntQuantity.LOSTFOCUS()
        loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.PriceLevel  = lnOldLevel 

        =gfAdd_Info(loDInv.lcInvLine,loDInv)
      ENDSCAN
   	  
      lcDatse = SET("Datasession")
      SET DATASESSION TO loDInv.DATASESSIONID
      REPLACE Binloc1 WITH lcRpBinLoc,;
        Binloc2 WITH lcRpBinLoc,;
        Binloc3 WITH lcRpBinLoc,;
        Binloc4 WITH lcRpBinLoc,;
        Binloc5 WITH lcRpBinLoc,;
        Binloc6 WITH lcRpBinLoc,;
        Binloc7 WITH lcRpBinLoc,;
        Binloc8 WITH lcRpBinLoc ALL IN (loDInv.lcInvLine)
      lnDetLins = RECCOUNT(loDInv.lcInvLine)
      
      IF lcOrderNumSeq = 'Y'
        REPLACE ORDER WITH gfSequence('ORDER','','',EVALUATE(loDInv.lcInvHdr+'.cDivision')) IN (loDInv.lcInvHdr)
      ENDIF

      SET DATASESSION TO lcDatse

      IF lnDetLins <= 0
        LOOP
      ENDIF

      loDInv.AriaForm1.AriaPageFrame1.Page3.ACTIVATE()
      lnChoice = 2
      DIMENSION  laInv[1]
      laInv[1] = ''

      loDInv.HIDE()
      loDInv.SaveFiles(.F.)
      INSERT INTO (lcTransRange) (TrnType,TrnNum) VALUES ('I',laInv[1])
    ENDSCAN
  ELSE
    **Credit memo Case
    SELECT DIFFTRAN
    SCAN FOR !EMPTY(CDIVISION) AND !EMPTY(Account) AND !EMPTY(Location)
      lcRpLoc = DIFFTRAN.Location
      lcRpAcct = DIFFTRAN.Account
      SELECT (lcStyleDet)
      IF llUse_GL
        =gfOpenFile(oAriaApplication.DataDir+'GLDist','GLDistAc','SH')
        SELECT GLDist
        lcTmpGlDt = IIF(TYPE('lcXMLFileName') <> 'C',loogScroll.gfTempName(),gfTempName())
        lcWorkDir = oAriaApplication.workdir
        COPY STRU TO (lcWorkDir+lcTmpGlDt+'.DBF')
        USE (lcWorkDir+lcTmpGlDt+'.DBF') IN 0 EXCLUSIVE
        SELECT (lcTmpGlDt)
      ENDIF
      SELECT (lcStyleDet)
      LOCATE
      
      lnChoice = 1
      IF TYPE('loCrMem') <> "O"
        DO FORM (oAriaApplication.ScreenHome+"RMCRMEM.scx") NOSHOW NAME loCrMem LINKED
        loCrMem.lBranchScreen=.F.
        loCrMem.NAME = "AARRMCRMEM"
        loCrMem.minittriggers ()
        lcDatse = SET("Datasession")
        SET DATASESSION TO loCrMem.DATASESSIONID
        IF  ASCAN(loCrMem.laEvntTrig,PADR('ADDRMFLD',10)) <> 0 .AND. loCrMem.mDoTrigger(PADR('ISUSEBIN',10))
          loFormSet = loCrMem
          lcCrMmStyl = loFormSet.ariaform1.rmcrmbus.lcCrMmStyl
          lcCrMmLine = loFormSet.ariaform1.rmcrmbus.lcCrMmLine
          lcCrMemLin = loFormSet.ariaform1.rmcrmbus.lcCrMemLin
          llLink_GL  = loFormSet.ariaform1.rmcrmbus.llLink_GL
          DO lfCrtUnComp IN (oAriaApplication.ApplicationHome + 'RMSave.FXP') WITH .F. , .T.
        ENDIF
        SET DATASESSION TO lcDatse
      ENDIF
      loCrMem.ChangeMode ('A')
      
      loCrMem.AriaForm1.pgfReCrmem.pgheader.cntHeader.DtCrdDate.Text1.VALUE  =DIFFTRAN.Date
      loCrMem.AriaForm1.DtPostDate.Text1.VALUE = DIFFTRAN.Date
      loCrMem.AriaForm1.KBAccount.keyTextbox.VALUE = lcRpAcct
      loCrMem.AriaForm1.KBAccount.keyTextbox.oldValue = SPACE(5)
      loCrMem.AriaForm1.KBAccount.keyTextbox.VALID
      loCrMem.ariaform1.KBSTORE.keytextbox.oldValue = SPACE(5)
      loCrMem.ariaform1.KBSTORE.keytextbox.VALUE = ""
      loCrMem.ariaform1.KBSTORE.keytextbox.VALID
      loCrMem.AriaForm1.pgfReCrmem.pgHEADER.cntHeader.cboDivision.VALUE =  DIFFTRAN.CDIVISION
      loCrMem.AriaForm1.pgfReCrmem.pgHEADER.cntHeader.cboReason.VALUE =  loCrMem.AriaForm1.pgfReCrmem.pgHEADER.cntHeader.cboReason.CodeDefaultValue
      loCrMem.AriaForm1.pgfReCrmem.pgHEADER.cntHeader.cboLocation.VALUE = lcRpLoc
      loCrMem.ariaForm1.rmCRMBUS.mUpdHdrFl(loCrMem.ariaForm1.rmCRMBUS.lcCrMemHdr)
      =SEEK('M'+lcRpAcct,'CUSTOMER','CUSTOMER')
      lnDisc = Customer.Disc
      SELECT (lcStyleDet)
      SCAN FOR  CDIVISION = DIFFTRAN.CDIVISION AND Date = DIFFTRAN.Date AND Time = DIFFTRAN.Time ;
                AND Account = DIFFTRAN.Account AND Location = DIFFTRAN.Location
        loCrMem.AriaForm1.pgfrecrmem.pgheader.cntheader.txtref.Value =   &lcStyleDet..Refer
        loCrMem.AriaForm1.pgfrecrmem.pgheader.cntheader.txtref.Valid()        
        lnRetPriceValue = &lcStyleDet..PRICE
        loCrMem.ariaForm1.rmCRMBUS.llAddLine = .T.
        WITH loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail
          .cboReason.VALUE = loCrMem.AriaForm1.pgfReCrmem.pgHEADER.cntHeader.cboReason.VALUE
          .cboReason.REQUERY()
          .cboQuality.VALUE = loCrMem.laStyGrade[1,2]
          .cboQuality.REQUERY()
          STORE " " TO .KbStyle.VALUE , .txtStyDesc.VALUE , loCrMem.ariaForm1.rmCRMBUS.lcCurLine ,;
            loCrMem.ariaForm1.rmCRMBUS.lcTranCd , .KbStyRetTo.VALUE ,;
            loCrMem.ariaForm1.rmCRMBUS.lcScale  , .kbConfig.KeyTExtBOx.VALUE , .kbDyelot.KeyTExtBOx.VALUE
          STORE 0   TO .txtTotAlQty.VALUE , .txtPrice.VALUE , .txtTotAmount.VALUE , .txtGrsPrice.VALUE , .txtDiscount.VALUE ,;
            .txtHstRat.VALUE , .txtPstRat.VALUE ,;
            loCrMem.ariaForm1.rmCRMBUS.lnScaleCnt , loCrMem.ariaForm1.rmCRMBUS.lnCost , loCrMem.ariaForm1.rmCRMBUS.lnDisc_Amt , loCrMem.ariaForm1.rmCRMBUS.lnTrde_Amt ,;
            loCrMem.ariaForm1.rmCRMBUS.lnPstTotal , loCrMem.ariaForm1.rmCRMBUS.lnDiscPcnt
          STORE '' TO .cboEmpl.VALUE
          STORE 0 TO .txtCost.VALUE
          .SbrkBackToStk.SCALE = ""
          FOR lnI = 1 TO 8
            lcI = STR(lnI,1)
            .SbrkBackToStk.txtQty&lcI..VALUE = 0
            .SbrkBackToStk.txtsizelbl&lcI..VALUE = ""
          ENDFOR
          .SbrkBackToStk.txtTotQty.VALUE =  0
          .SbrkBackToStk.txtTotQty.ENABLED = .F.
        ENDWITH

        loCrMem.AriaForm1.llLinStat  = .T.
        STORE .F. TO loCrMem.AriaForm1.llRetStat  ,loCrMem.AriaForm1.llDyeStat  , loCrMem.AriaForm1.llsizestat
        loCrMem.AriaForm1.mShowObj()
        loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.KbStyle.VALUE = " "
        loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.KbStyle.TxtItem.VALUE =&lcStyleDet..STYLE
        llOldCst = loCrMem.Ariaform1.RMCRMBUS.llStdCost
        loCrMem.Ariaform1.RMCRMBUS.llStdCost = .T.

        loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.KbStyle.VALID(.T.,0,&lcStyleDet..STYLE,SPACE(19),&lcStyleDet..STYLE)
        loCrMem.Ariaform1.RMCRMBUS.llStdCost = llOldCst
        *lnCstValue = (&lcStyleDet..Price*lnDefCost/100)
        lcDatse = SET("Datasession")
        SET DATASESSION TO loCrMem.DATASESSIONID
        lcCrMemLin = loCrMem.Ariaform1.RMCRMBUS.lcCrMemLin
        loCrMem.Ariaform1.RMCRMBUS.lnCost = loCrMem.Ariaform1.RMCRMBUS.mcostassign(.T.)
        *loCrMem.Ariaform1.RMCRMBUS.lnCost = IIF(llDefaultSty ,lnCstValue ,loCrMem.Ariaform1.RMCRMBUS.lnCost)
        REPLACE COST WITH loCrMem.Ariaform1.RMCRMBUS.lnCost IN (loCrMem.Ariaform1.RMCRMBUS.lcCrMemlin)
        SET DATASESSION TO lcDatse
        loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.cboReason.VALUE = loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.cboReason.CodeDefaultValue
        loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.cboReason.VALID()
        loCrMem.AriaForm1.pgfReCrmem.pgHEADER.cntHeader.cboReason.VALUE= loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.cboReason.CodeDefaultValue
        loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.txtGrsPrice.OldValue = 0
        loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.txtGrsPrice.VALUE = &lcStyleDet..PRICE
        loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.txtGrsPrice.VALID

        IF TYPE('loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.txtDiscount.OldValue') <> 'N'
          loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.txtDiscount.OldValue = 0
        ENDIF
        loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.txtDiscount.VALUE = lnDisc
        loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.txtDiscount.VALID

        FOR lnX= 1 TO 8
          lcX = STR(lnX,1)
          IF &lcStyleDet..Qty&lcX. > 0
            loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.SbrkBackToStk.txtQty&lcX..OldValue  = 0
            loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.SbrkBackToStk.txtQty&lcX..VALUE  =  &lcStyleDet..Qty&lcX.
            loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.SbrkBackToStk.txtQty&lcX..VALID
          ENDIF
        ENDFOR
      ENDSCAN

      lcDatse = SET("Datasession")
      SET DATASESSION TO loCrMem.DATASESSIONID
      REPLACE Binloc1 WITH lcRpBinLoc,;
        Binloc2 WITH lcRpBinLoc,;
        Binloc3 WITH lcRpBinLoc,;
        Binloc4 WITH lcRpBinLoc,;
        Binloc5 WITH lcRpBinLoc,;
        Binloc6 WITH lcRpBinLoc,;
        Binloc7 WITH lcRpBinLoc,;
        Binloc8 WITH lcRpBinLoc ALL IN (loCrMem.Ariaform1.RMCRMBUS.lcCrMemlin)
        
      lnDetLins2 = RECCOUNT(loCrMem.Ariaform1.RMCRMBUS.lcCrMemlin)

      IF !USED('WHSLOC')
        =gfOpenTable('WHSLOC','WHSLOC')
      ENDIF

      SET DATASESSION TO lcDatse
      
      IF lnDetLins2 <= 0
        LOOP
      ENDIF
      
      lnChoice = 2
      lcCrMemo = SPACE(6)

      loCrMem.SaveFiles(.F.)
      ***Update[Start]
      IF !EMPTY(lcCrMemo)
        SET DATASESSION TO loCrMem.DATASESSIONID
        loCrMem.Ariaform1.RMCRMBUS.RETHDR.SEEK("******")
        SET DATASESSION TO lcDatse
      ENDIF
      INSERT INTO (lcTransRange) (TrnType,TrnNum) VALUES ('R',lcCrMemo)
      SELECT (lcStyleDet)
    ENDSCAN
  ENDIF


*:**************************************************************************
*:* Name        : lfVerifyAdjQty
*!* Developer   : Mariam Mazhar[MMT]
*!* Date        : 08/12/2015
*:* Purpose     : Check XML Qty
*:***************************************************************************
FUNCTION lfVerifyAdjQty
SELECT (lcFileData)
LOCATE
DO WHILE !EOF()
  lcKeyExp = UPC
  lcLocation = Location
  =SEEK(lcKeyExp)
  lnQtyOld = 0
  SCAN REST WHILE UPC  = lcKeyExp FOR !lIgnore AND &lcFileData..Qty > 0 AND Location = lcLocation 
    lcQtyPos = ALLTRIM(&lcFileData..SIZE)
    m.STYLE = &lcFileData..STYLE
    lcSelDefaultWare  = lcLocation 
    lcSelDefBin = lcRpBinLoc
    IF !USED('WHBINLOC_A')
      =gfOpenTable('WHBINLOC','WHBINLOC','SH','WHBINLOC_A')
    ENDIF
    IF !gfSeek(PADR(lcSelDefaultWare,6)+PADR(lcSelDefBin ,10)+m.STYLE ,'WHBINLOC_A')
        lcCurrAlias = SELECT(0)
        lnAdjCost    = lfGetAdjustCost(m.STYLE, lcSelDefaultWare)
        =lfCreateAutoAdj(lcSelDefaultWare, lcSelDefBin, m.STYLE, VAL(&lcFileData..SIZE), lnAdjCost,ABS(&lcFileData..Qty))
        lnQtyOld = lnQtyOld + ABS(&lcFileData..Qty)
        SELECT (lcCurrAlias)
    ELSE
      IF  WHBINLOC_A.Qty&lcQtyPos. < lnQtyOld+ABS(&lcFileData..Qty)
        lnAdjCost    = lfGetAdjustCost(m.STYLE, lcSelDefaultWare)
        =lfCreateAutoAdj(lcSelDefaultWare, lcSelDefBin, m.STYLE, VAL(&lcFileData..SIZE), lnAdjCost,lnQtyOld+&lcFileData..Qty - WHBINLOC_A.Qty&lcQtyPos.)
        lnQtyOld = lnQtyOld + ABS(&lcFileData..Qty)
      ELSE
        lnQtyOld = lnQtyOld + ABS(&lcFileData..Qty)
      ENDIF
    ENDIF
  ENDSCAN
ENDDO
*!*************************************************************
*! Name      : lfGetAdjustCost
*! Developer : Mariam Mazhar[MMT]
*! Date      : 08/12/2015
*! Purpose   : Get Style cost for the auto adjustment
*!*************************************************************
FUNCTION lfGetAdjustCost
LPARAMETERS lcStyle, lcWareCode

LOCAL lnAdjCost, lnRecCnt, lcAlias
lnAdjCost = 0
lnRecCnt  = 0
lcAlias = SELECT()

IF !USED('STYDYE_CST')
  *=gfOpenTable('STYDYE','STYDYE','SH','STYDYE_CST')  && STYLE+CWARECODE+DYELOT
  USE (oAriaApplication.DataDir+'STYDYE') IN 0 SHARED AGAIN ALIAS STYDYE_CST ORDER STYDYE  && STYLE+CWARECODE+DYELOT
ENDIF

*IF gfSeek(lcStyle+lcWareCode,'STYDYE_CST')
IF SEEK(lcStyle+lcWareCode, 'STYDYE_CST')
  SELECT STYDYE_CST
  SCAN REST WHILE STYLE+CWARECODE+DYELOT = lcStyle+lcWareCode
    lnAdjCost = lnAdjCost + STYDYE_CST.Ave_Cost
    lnRecCnt  = lnRecCnt + 1
  ENDSCAN
ENDIF

lnAdjCost = lnAdjCost / lnRecCnt

*!B610637,1 MMT 12/26/2013 Concession invoicing program calculates wrong style ave. cost[T20131212.0012][Start]
IF !SEEK(lcStyle+lcWareCode, 'STYDYE_CST')
  IF !USED('STYLE_CST')
    =gfOpenTable('STYLE','STYLE','SH','STYLE_CST')
  ENDIF  
  IF gfSeek(lcStyle,'STYLE_CST')
    lnAdjCost = STYLE_CST.Ave_Cost
  ENDIF
ENDIF
*!B610637,1 MMT 12/26/2013 Concession invoicing program calculates wrong style ave. cost[T20131212.0012][End]

SELECT (lcAlias)

RETURN lnAdjCost
*!*************************************************************
*! Name      : lfCreateAutoAdj
*! Developer : Mariam Mazhar[MMT]
*! Date      : 08/12/2015
*! Purpose   : Create Auto Inventory Adjustment
*!*************************************************************
FUNCTION lfCreateAutoAdj
LPARAMETERS lcAdjWare, lcAdjBin, lcAdjStyle, lnStySz, lnAdjCost,lnQtyToAdj

STORE '' TO M_POST_PPRD,M_SYS_DIR,M_GL_VERS,M_GL_CO
=gfGetMemVar('M_POST_PPRD,M_SYS_DIR,M_GL_VERS,M_GL_CO',oAriaApplication.ActiveCompanyID)
llRet = CHECKPRD(CTOD(SUBSTR(&lcFileData..DateTime,1,10)),'lcGLFYear','lcGLPeriod','IN',.T.)


DIMENSION laAdjust[9]
laAdjust          = 0
laAdjust[lnStySz] = lnQtyToAdj
laAdjust[9]       = lnQtyToAdj
lcLinkCode = IIF(llUse_GL , IIF(!EMPTY(STYLE.Link_Code), STYLE.Link_Code, 'DEFDEF'), "")

lcAdjReason = lcRpReason
lcAdjAcct = ' '
IF llUse_GL AND !EMPTY(lcAdjReason)
  DECLARE laTrmRltFd[1,2]
  laTrmRltFd[1,1] = 'GLACCOUNT'
  laTrmRltFd[1,2] = 'lcAdjAcct'
  = gfRltFld(lcAdjReason , @laTrmRltFd , "CADJREASON")
ELSE
  lcAdjReason = ' '
ENDIF

IF llUse_GL
  =gfOpenFile(oAriaApplication.DataDir+'GLDist','GLDistAc','SH')
  SELECT GLDist
  lcTmpGlDt = IIF(TYPE('lcXMLFileName') <> 'C',loogScroll.gfTempName(),gfTempName())
  lcWorkDir = oAriaApplication.workdir
  COPY STRU TO (lcWorkDir+lcTmpGlDt+'.DBF')
  USE (lcWorkDir+lcTmpGlDt+'.DBF') IN 0 EXCLUSIVE
  SELECT (lcTmpGlDt)
ENDIF

IF llUse_GL
  DECLARE laGLDistAr[2,13]
  laGLDistAr[1,1] = lcLinkCode
  laGLDistAr[2,1] = lcLinkCode
  laGLDistAr[1,2] = '006'
  laGLDistAr[2,2] = '007'
  laGLDistAr[1,3] = 1
  laGLDistAr[2,3] = -1
  STORE 'IA' TO laGLDistAr[1,4],laGLDistAr[2,4]
  STORE ''        TO laGLDistAr[1,5],laGLDistAr[2,5]
  STORE CTOD(SUBSTR(&lcFileData..DateTime,1,10)) TO laGLDistAr[1,6],laGLDistAr[2,6]
  STORE lcGlFYear  TO laGLDistAr[1,7],laGLDistAr[2,7]
  STORE lcGlPeriod TO laGLDistAr[1,8],laGLDistAr[2,8]
  STORE lcTmpGlDt TO laGLDistAr[1,9],laGLDistAr[2,9]
  laGLDistAr[2,10] = lcAdjAcct
ELSE
  DIME laGLDistAr[1,1]
  laGLDistAr = ''
ENDIF


IF !USED('STYDYE_AB')
  =gfOpenTable('STYDYE','STYDYE','SH','STYDYE_AB')
ENDIF
IF  !gfSEEK(lcAdjStyle+lcAdjWare+SPACE(10),'STYDYE_AB')
  DO gpAdStyWar WITH lcAdjStyle, SPACE(10), lcAdjWare
ENDIF

lnRet=gfStyCrl('1', lcAdjStyle, lcAdjWare, SPACE(10), CTOD(SUBSTR(&lcFileData..DateTime,1,10)), '', @laAdjust, lnAdjCost,;
               'INV_'+lcAdjWare, .T., 'INV_'+lcAdjWare, 0, '', '', @laGLDistAr, 0, '', '')

IF llUse_GL 
  lcWorkDir = oAriaApplication.workdir
	SELECT (lcTmpGlDt)
	*-- Generate a unique session number.
	lcGlSess = gfSequence('GLSESSION')
	REPLACE ALL GLSESSION WITH lcGlSess
	USE
	SELECT GLDIST
	APPEND FROM (lcWorkDir+lcTmpGlDt+'.DBF')
	ERASE (lcWorkDir+lcTmpGlDt+'.DBF')
	SELECT STYLE
	=gfTableUpdate()
ENDIF
          
lfUpdateBinLoctionFiles(lcAdjStyle, lcAdjWare, lcAdjBin)

*- Get GL Variables value
llRet = .T.
lcGlFYear  = ''
lcGlPeriod = ''
STORE '' TO M_POST_PPRD,M_SYS_DIR,M_GL_VERS,M_GL_CO
=gfGetMemVar('M_POST_PPRD,M_SYS_DIR,M_GL_VERS,M_GL_CO',oAriaApplication.ActiveCompanyID)
llRet = CHECKPRD(CTOD(SUBSTR(&lcFileData..DateTime,1,10)),'lcGLFYear','lcGLPeriod','IN',.T.)

*- Fill Adjustment temp file
lcCurAlisSel = SELECT()
IF !USED('StyInvjl_Adj')
  =gfOpenTable('StyInvjl','STYDATE','SH','StyInvjl_Adj')
ENDIF

SELECT 'StyInvjl_Adj'
SET ORDER TO STYINVJL  DESCENDING 
=gfSeek(lcAdjStyle+lcAdjWare)
SCATTER MEMO MEMVAR
SELECT (lcScreenTemp)
APPEND BLANK
m.cReason   = 'INV_'+lcAdjWare
*!B610382,2 SAB 06/26/2013 Fix concession issu# 2 [T20120108.0010][Issue2][End]
m.Date      = StyInvjl_Adj.dTrDate
m.dPostDate = StyInvjl_Adj.dTrDate
m.Type      = 'P'
m.Unt_Cost  = StyInvjl_Adj.nCost
m.Old_Cost  = StyInvjl_Adj.nCost
m.GLFYear   = lcGlFYear
m.GLPeriod  = lcGlPeriod
m.cFromWare = StyInvjl_Adj.cWareCode

FOR lnNum = 1 TO 8
  lcNum = ALLTRIM(STR(lnNum))
  m.Adj&lcNum.    = StyInvjl_Adj.nStk&lcNum.
  m.OldQty&lcNum. = StyInvjl_Adj.nStk&lcNum.
  m.nOldTo&lcNum. = StyInvjl_Adj.nStk&lcNum.
ENDFOR

m.TotAdj    = StyInvjl_Adj.nTotStk
m.TotOld    = StyInvjl_Adj.nTotStk
m.nTotOldTo = StyInvjl_Adj.nTotStk
GATHER MEMO MEMVAR
ENDFUNC 
*-End Of lfCreateAutAdj
*:**************************************************************************
*:* Name        : lfUpdateBinLoctionFiles
*:* Developer   : Mariam Mazhar[MMT]
*:* Date        : 08/12/2015
*:* Purpose     : update BININVJL table
*:***************************************************************************
FUNCTION lfUpdateBinLoctionFiles
LPARAMETERS lcStyle,lcLocation,lcBin
lcCurAlisSel = SELECT()
IF !USED('StyInvjl_bin')
  =gfOpenTable('StyInvjl','STYDATE','SH','StyInvjl_bin')
ENDIF
IF !USED('BININVJL_A')
  =gfOpenTable('BININVJL','WHBINSTY','SH','BININVJL_A')
ENDIF
IF !USED('WHBINLOC_Bin')
  =gfOpenTable('WHBINLOC','WHBINLOC','SH','WHBINLOC_Bin')
ENDIF

IF !USED('WHSLOC_Bin')
  =gfOpenTable('WHSLOC','WHSLOC','SH','WHSLOC_Bin')
ENDIF
LOCAL lnMaxLnNo
lnMaxLnNo = 0

SELECT 'StyInvjl_bin'
SET ORDER TO 'STYDATE' DESCENDING
=gfSeek(lcStyle+lcLocation)
lcSessionID = StyInvjl_bin.CSESSION
SET ORDER TO 'STYDATE' ASCENDING
=gfSeek(lcStyle+lcLocation)
SCAN WHILE STYLE+CWARECODE+DTOS(DTRDATE)+CSESSION+CIRTYPE = lcStyle+lcLocation FOR CSESSION = lcSessionID
  SCATT MEMVAR MEMO
  m.clocation = lcBin
  SELECT BININVJL_A
  IF !gfSeek(m.CWARECODE+m.CLOCATION+m.STYLE+m.CTRCODE+STR(m.LINENO,6) +m.cirtype+m.CSESSION+m.crsession+DTOS(m.DTRDATE),'BININVJL_A','WHBINSTY')
    =lfIncLineNO(@lnMaxLnNo)
    =gfAppend('BININVJL',.T.)
  ENDIF
ENDSCAN


SELECT WHBINLOC_Bin
lcReplace = ''
IF !gfSEEK(lcLocation +lcBin+lcStyle,'WHBINLOC_Bin')
  SELECT WHSLOC_Bin
  =gfSEEK(lcLocation  +lcBin +SPACE(19),'WHSLOC_Bin')
  SELECT WHBINLOC_Bin
  =gfAppend()
  lcReplace = lcReplace + ;
    'STYLE      WITH ['+lcStyle  +'] '+;
    'CWARECODE  WITH ['+lcLocation  +'] '+;
    'clocation  WITH ['+lcBin+'] '

  lcReplace = lcReplace +;
    'cBlkPck   WITH ['+WHSLOC_Bin.cBlkPck   +'] '+;
    'cSection  WITH ['+WHSLOC_Bin.cSection  +'] '+;
    'cBinClass WITH ['+WHSLOC_Bin.cBinClass +'] '

ENDIF

LOCAL lcKey,lnAdj1,lnAdj2,lnAdj3,lnAdj4,lnAdj5,lnAdj6,lnAdj7,lnAdj8
STORE 0 TO lnAdj1,lnAdj2,lnAdj3,lnAdj4,lnAdj5,lnAdj6,lnAdj7,lnAdj8

lnAdj1 = laAdjust[1]
lnAdj2 = laAdjust[2]
lnAdj3 = laAdjust[3]
lnAdj4 = laAdjust[4]
lnAdj5 = laAdjust[5]
lnAdj6 = laAdjust[6]
lnAdj7 = laAdjust[7]
lnAdj8 = laAdjust[8]

SELECT WHBINLOC_Bin
lcReplace = lcReplace + ;
  'Qty1       WITH '+ STR( Qty1 + lnAdj1 ) +' '+;
  'Qty2       WITH '+ STR( Qty2 + lnAdj2 ) +' '+;
  'Qty3       WITH '+ STR( Qty3 + lnAdj3 ) +' '+;
  'Qty4       WITH '+ STR( Qty4 + lnAdj4 ) +' '+;
  'Qty5       WITH '+ STR( Qty5 + lnAdj5 ) +' '+;
  'Qty6       WITH '+ STR( Qty6 + lnAdj6 ) +' '+;
  'Qty7       WITH '+ STR( Qty7 + lnAdj7 ) +' '+;
  'Qty8       WITH '+ STR( Qty8 + lnAdj8 )
=gfReplace(lcReplace)

lcReplace = 'TotQty     WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8 '
=gfReplace(lcReplace)

IF WHBINLOC_Bin.Qty1 = 0 .AND. WHBINLOC_Bin.Qty2 = 0 .AND. WHBINLOC_Bin.Qty3 = 0 .AND. WHBINLOC_Bin.Qty4 = 0 .AND. ;
    WHBINLOC_Bin.Qty5 = 0 .AND. WHBINLOC_Bin.Qty6 = 0 .AND. WHBINLOC_Bin.Qty7 = 0 .AND. WHBINLOC_Bin.Qty8 = 0 .AND. ;
    gfGetMemVar('M_DELZRBNR')
  =gfDelete()
ENDIF

SELECT WHBINLOC_Bin
=gfTableUpdate()
SELECT  BININVJL_A
=gfTableUpdate()
=gfCloseTable('BININVJL_A')
=gfCloseTable('StyInvjl_bin')
=gfCloseTable('WHBINLOC_Bin')
=gfCloseTable('WHSLOC_Bin')
SELECT(lcCurAlisSel)
*:**************************************************************************
*:* Name        : lfIncLineNO
*:* Developer   : Mariam Mazhar[MMT]
*:* Date        : 08/12/2015
*:* Purpose     : used to increment the m.LINENO variable to keep the key of BININVJL
*:***************************************************************************
FUNCTION lfIncLineNO
LPARAMETERS lnMaxLnNo
SELECT BININVJL_A
=gfSeek(m.CWARECODE+m.CLOCATION+m.STYLE)
LOCATE
SCAN
  IF CWARECODE+CLOCATION+STYLE+CTRCODE+cirtype+CSESSION+crsession+DTOS(DTRDATE) = ;
      m.CWARECODE+m.CLOCATION+m.STYLE+m.CTRCODE+m.cirtype+m.CSESSION+m.crsession+DTOS(m.DTRDATE)
    lnMaxLnNo = MAX(LINENO,lnMaxLnNo)
    m.LineNo = lnMaxLnNo+1
  ENDIF
ENDSCAN

*!*************************************************************
*! Name      : lfCrtAdjstTmp
*! Developer : Mariam Mazhar[MMT]
*! Date      : 08/11/2015
*! Purpose   : Create Adjustment Temp. file
*!*************************************************************
FUNCTION lfCrtAdjstTmp
=gfOpenTable('INVTADJ','INVTADJ')   && STYLE

*-- Create tmp adjustment file.
SELECT INVTADJ
=AFIELDS(laFStru)
lnNo1=ASCAN(laFStru, 'UNT_COST')
lnNo2=ASCAN(laFStru, 'OLD_COST')

*--Make the lenth of this two fields as ave_cost field.
STORE 15 TO laFStru(lnNo1+2),laFStru(lnNo2+2)
STORE  7 TO laFStru(lnNo1+3),laFStru(lnNo2+3)
lnFStru = ALEN(laFStru,1)

DIMENSION laFStru[lnFStru+3, 18]

laFStru[lnFStru+1, 1] = 'cAdjReason'
laFStru[lnFStru+1, 2] = 'C'
laFStru[lnFStru+1, 3] = 6
laFStru[lnFStru+1, 4] = 0

laFStru[lnFStru+2, 1] = 'cRefer'
laFStru[lnFStru+2, 2] = 'C'
laFStru[lnFStru+2, 3] = 6
laFStru[lnFStru+2, 4] = 0

laFStru[lnFStru+3, 1] = 'cIssSess'
laFStru[lnFStru+3, 2] = 'C'
laFStru[lnFStru+3, 3] = 6
laFStru[lnFStru+3, 4] = 0

STORE .F. TO laFStru[lnFStru+1, 5],laFStru[lnFStru+1, 6],;
             laFStru[lnFStru+2, 5],laFStru[lnFStru+2, 6],;
             laFStru[lnFStru+3, 5],laFStru[lnFStru+3, 6]
STORE ""  TO laFStru[lnFStru+1, 7],laFStru[lnFStru+1, 8],laFStru[lnFStru+1, 9],laFStru[lnFStru+1,10],laFStru[lnFStru+1,11],;
             laFStru[lnFStru+2, 7],laFStru[lnFStru+2, 8],laFStru[lnFStru+2, 9],laFStru[lnFStru+2,10],laFStru[lnFStru+2,11],;
             laFStru[lnFStru+3, 7],laFStru[lnFStru+3, 8],laFStru[lnFStru+3, 9],laFStru[lnFStru+3,10],laFStru[lnFStru+3,11]
STORE ""  TO laFStru[lnFStru+1,12],laFStru[lnFStru+1,13],laFStru[lnFStru+1,14],laFStru[lnFStru+1,15],laFStru[lnFStru+1,16],;
             laFStru[lnFStru+2,12],laFStru[lnFStru+2,13],laFStru[lnFStru+2,14],laFStru[lnFStru+2,15],laFStru[lnFStru+2,16],;
             laFStru[lnFStru+3,12],laFStru[lnFStru+3,13],laFStru[lnFStru+3,14],laFStru[lnFStru+3,15],laFStru[lnFStru+3,16]
STORE 0   TO laFStru[lnFStru+1,17],laFStru[lnFStru+1,18],;
             laFStru[lnFStru+2,17],laFStru[lnFStru+2,18],;
             laFStru[lnFStru+3,17],laFStru[lnFStru+3,18]

CREATE DBF (oAriaApplication.WorkDir+lcScreenTemp) FROM ARRAY laFStru
USE
SELECT 0

USE (oAriaApplication.WorkDir+lcScreenTemp) EXCLUSIVE ALIAS (lcScreenTemp)

ENDFUNC 
