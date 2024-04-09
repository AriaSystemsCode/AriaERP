*:**************************************************************************
*: Program file  : ARAMI10
*: Program desc. : Quartely Sales Report.
*: System        : Aria Advantage Series.
*: Module        : Accounts Receivable
*: Developer     : Sameh Saiid Ezzat (SSE)
*: Date          : 12/18/2001
*: Reference     : C102409
*:**************************************************************************
*: Procedures    : lpCreatFil , lpGenExpr , lpCollData
*: Functions     : lfwRepWhen , lfItmPos , lfPreRun
*:**************************************************************************
*: Passed Parameters : None
*:**************************************************************************
*

*-- Check if there is a style group entered.
SELECT Codes
SET ORDER TO TAG cCode_No
IF !SEEK("NCSTYGROUP")
  *-- Message <This company has no style group codes, Cannot proceed.>
  *-- Buttons <                          OK                          >
  =gfModalGen("TRM000000B00000","DIALOG",'','',"This company has no style group codes, Cannot proceed.")
  RETURN  
ENDIF

*-- Check if user didn't select Style Group.
IF EMPTY(laOGFxFlt[lnStyGrPos,6])
  *-- Message <You have to select a Style Group.>
  *-- Buttons <               OK                >
  =gfModalGen("TRM000000B00000","DIALOG",'','',"You have to select a Style Group.")
  RETURN    
ENDIF

*-- IF filter change collect data again.
IF llOGFltCh
  PRIVATE lcInvExp , lcRetExp , lcDateType
  STORE '.T.' TO lcInvExp , lcRetExp
  
  DO lpCreatFil
  
  lcDateType = SET('DATE')
  DO lpGenExpr           && get the Date expression

  SELECT RetLine
  SET RELATION TO 'M' + Account INTO Customer ADDITIVE
  SET RELATION TO CrMemo INTO RetHdr ADDITIVE
  SET RELATION TO style INTO Style ADDITIVE

  SELECT InvLine
  SET RELATION TO 'M' + Account INTO Customer ADDITIVE
  SET RELATION TO Invoice INTO InvHdr ADDITIVE
  SET RELATION TO style INTO Style ADDITIVE

  DO lpCollData          && Collect the data for report.
  
  SET DATE TO &lcDateType.

  SELECT RetLine
  SET RELATION TO
    
  SELECT InvLine
  SET RELATION TO
ENDIF
*-- Endif of Filter change.  

SELECT (lcWorkFile)
LOCATE
IF EOF()
  *-- Message <There are no records to display>
  *-- Buttons <               OK              >
  = gfModalGen('TRM00052B00000','DIALOG' )
  SET DEVICE TO SCREEN
  RETURN
ELSE
  *-- Creating the text file which will be used for printing
  PRIVATE lcOutPut , lnHandle , lcFileName , lcStyGrDes
  SELECT Codes
  SET ORDER TO TAG Ccode_no
  lcStyGrDes = IIF(SEEK('N'+PADR('CSTYGROUP',10)+ALLTRIM(laOGFxFlt[lnStyGrPos,6]),'Codes'),ALLTRIM(cDiscRep),'')
  
  *lcFileName = ALLTRIM(SycComp.cCom_Name) + "_" + ALLTRIM(FisHd.cFisFYear) +"_" + "Q" + lcRpQuarter + "_" + gcBaseCurr + "_" + "file1"
  lcFileName = ALLTRIM(lcRpYear) +"_" + "Q" + lcRpQuarter
  lcOutput = lcFileName+".TXT"
  lnHandle = FCREATE(lcOutput)
  IF lnHandle < 0
    *-- Message <Cannot open output file.>
    *-- Buttons <           OK           >
    =gfModalGen("TRM000000B00000","DIALOG",'','','Cannot open output file.')
    RETURN
  ENDIF
  
  SELECT (lcWorkFile)
  SCAN
    WAIT WINDOW "Writing to text file Account : " + Account NOWAIT
    =FPUTS(lnHandle,["]+ALLTRIM(cBtName)+[","]+ALLTRIM(cStCountry)+[","]+Account+[","]+lcStyGrDes+[","]+ALLTRIM(lcRpCatgry)+[","]+lcRpGender+[","]+cDate+[","]+ALLTRIM(STR(nUnits,15))+[","]+ALLTRIM(TRANSFORM(nGross,'999999999999.99'))+[","]+ALLTRIM(TRANSFORM(nNet,'999999999999.99'))+["])
  ENDSCAN
  WAIT CLEAR
  =FCLOSE(lnHandle)  
ENDIF
*-- End of Report.

*!**************************************************************************
*! Name      : lfwRepWhen
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 12/18/2001
*! Purpose   : Report When Function.
*!**************************************************************************
*! Example   : = lfwRepWhen()
*!**************************************************************************
*
FUNCTION lfwRepWhen
SHOW GET pbOGPrevie DISABLE             && Disable the preview button.
=SEEK(gcAct_Comp,'SycComp')
lnStyGrPos = lfItmPos('STYLE.CSTYGROUP') && get Season filter Position.

lcRpYear = STR(YEAR(gdSysDate),4)
*-- End of lfwRepWhen.

*!**************************************************************************
*! Name      : lfItmPos
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 12/18/2001
*! Purpose   : To get the position of the fixed filter in OG.
*!**************************************************************************
*! Called from : OG When Function 
*!**************************************************************************
*! Example   : = lfItmPos()
*!**************************************************************************
*
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos
*-- End of lfItmPos.

*!**************************************************************************
*! Name      : lpCreatFil
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 12/18/2001
*! Purpose   : Create work File.
*!**************************************************************************
*! Example   : DO lpCreatFil.
*!**************************************************************************
*
PROCEDURE lpCreatFil
*-- Create a temporary table to hold data which will be displayed in report.
CREATE TABLE (gcWorkDir+lcWorkFile) (cDate C(6) , cBtName C(30) , ;
              cStCountry C(30) , Account C(5) , ;
              nUnits N(13) , nGross N(15,2) , nNet N(15,2) , cMonth C(2))
INDEX ON Account + cMonth TAG (lcWorkFile)
*-- End of lpCreatFil.

*!**************************************************************************
*! Name      : lpCollData
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 12/18/2001
*! Purpose   : To collect data for the report.
*!**************************************************************************
*! Example   : DO lpCollData
*!**************************************************************************
*
PROCEDURE lpCollData
SELECT InvLine

SCAN FOR &lcInvExp AND &lcRpExp AND InvHdr.Status <> 'V'
  WAIT WINDOW 'Collecting data for Invoice # ' + Invoice NOWAIT
  IF SEEK(Account+PADL(ALLTRIM(STR(MONTH(InvDate),2)),2,'0'),(lcWorkFile))
    SELECT (lcWorkFile)
    REPLACE nUnits WITH nUnits + InvLine.TotQty , ;
            nGross WITH nGross + (InvLine.Price * InvLine.TotQty) , ;
            nNet   WITH nNet + (InvLine.Price * InvLine.TotQty)
  ELSE
    m.cMonth = PADL(ALLTRIM(STR(MONTH(InvDate),2)),2,'0')
    m.cDate = SUBSTR(UPPER(CMONTH(InvDate)),1,3)+"-"+SUBSTR(STR(YEAR(InvDate),4),3)
    m.cBtName = Customer.BtName
    m.cStCountry = Customer.cCont_Code
    m.Account = Account
    m.nUnits = TotQty
    m.nGross = Price * TotQty
    m.nNet = m.nGross
    INSERT INTO (lcWorkFile) FROM MEMVAR 
  ENDIF        
ENDSCAN

SELECT RetLine
SCAN FOR &lcRetExp AND  &lcRpExp AND RetHdr.Status <> 'V'
  WAIT WINDOW 'Collecting data for Credit Memo # ' + CrMemo NOWAIT
  IF SEEK(Account+PADL(ALLTRIM(STR(MONTH(CrDate),2)),2,'0'),(lcWorkFile))  
    SELECT (lcWorkFile)
    *++NAD
    IF SEEK  (Retline.invoice,'INVHDR')
       REPLACE nNet WITH nNet - (RetLine.Gros_Price * RetLine.TotQty)
    ELSE
       REPLACE nNet WITH nNet - RetLine.Amount
    ENDIF
    *REPLACE nNet WITH nNet - (RetLine.Gros_Price * RetLine.TotQty)
    *+NAD
    
  ELSE
    m.cMonth = PADL(ALLTRIM(STR(MONTH(CrDate),2)),2,'0')
    m.cDate = SUBSTR(UPPER(CMONTH(CrDate)),1,3)+"-"+SUBSTR(STR(YEAR(CrDate),4),3)
    m.cBtName = Customer.BtName
    m.cStCountry = Customer.cCont_Code
    m.Account = Account
    *NAD   
     *m.nNet    = (RetLine.Gros_Price * RetLine.TotQty) * -1
    IF SEEK  (Retline.invoice,'INVHDR')
      m.nNet    = (RetLine.Gros_Price * RetLine.TotQty) * -1
    ELSE
      m.nNet    = (RetLine.Amount) * -1
    ENDIF
     STORE 0 TO m.nGross , m.nUnits
    INSERT INTO (lcWorkFile) FROM MEMVAR 
  ENDIF        
ENDSCAN 
*-- End of lpCollData.

*!**************************************************************************
*! Name      : lfPreRun
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 12/18/2001
*! Purpose   : Pre_Preview Function To call Temporary .FRX again
*!           : to evaluate #OBJDISP objects again.
*!**************************************************************************
*! Called from : Report Generator when press <Preview> or <Run>
*!**************************************************************************
*! Example   : = lfPreRun()
*!**************************************************************************
*
FUNCTION lfPreRun
llOGEscPrsd = .F.
lnOGHD=IIF(EMPTY(laOGHdFlt[1,1]),0,ALEN(laOGHdFlt,1))
lnOGFX=IIF(EMPTY(laOGFxFlt[1,1]),0,ALEN(laOGFxFlt,1))
lnOGvr=IIF(EMPTY(laOGVrFlt[1,1]),0,ALEN(laOGVrFlt,1))
lcOGSelect=SELECT()
=lfInitValue('laOGVrFlt','T')
=lfInitValue('laOGFXFlt','T')
  
lcOGHDFlt=gfGenFlt('laOGHDFlt' , llOGFilter , .T.)
lcOGFxFlt=gfGenFlt('laOGFxFlt' , llOGFilter , .T.)
lcOGVrFlt=gfGenFlt('laOGVrFlt' , llOGFilter , .T.)
  
lnOGHD=IIF(EMPTY(lcOGHDFlt),0,ALEN(laOGHdFlt,1))
lnOGFX=IIF(EMPTY(lcOGFxFlt),0,ALEN(laOGFxFlt,1))
lnOGvr=IIF(EMPTY(lcOGVrFlt),0,ALEN(laOGVrFlt,1))  
=lfInitValue('laOGVrFlt','C')
=lfInitValue('laOGFXFlt','C')

lcRpExp = lcOGHDFlt+IIF((lnOGFx+lnOGVr>0) AND lnOGHd>0,' AND (','')+;
          lcOGFxFlt+IIF(lnOGVr>0,IIF(lnOgFx>0,' AND ',''),'')+lcOGVrFlt+;
          IIF((lnOGFx+lnOGVr>0) AND lnOGHd>0,')','')

lcRpExp=IIF(EMPTY(lcRpExp),'.T.',lcRpExp)
lcogExc=SET('EXACT')        
SET EXACT ON
llOGFltCh = IIF(llOGFltCh,llOGFltCh,lcogfltr<>lcrpexp)        

SET EXACT &lcOGExc
IF llOGFltCh
  lcOGFltr=lcRpExp
ENDIF
SET CONSOLE OFF

IF !EMPTY(lcOGFormV)
  IF !(lcOGLastForm==&lcOGFormV)
      
    lcOGFormArr='laRpObj'+SUBSTR(lcOGPlatForm,1,3)
      
    lcOGLastForm=&lcOGFormV
    llOGStyleCh=.T.
  ENDIF  

  IF llOGRefForm .OR. lcRepAvlM = 'B' .OR. llOGStyleCh
    =gfCrtFrm(EVAL(lcOGFormV),lcOGFormArr,llOGRefForm)
  ENDIF

ENDIF  

DO (gcRepHome+gcAct_Appl+'REPORT.APP') WITH lcOGPrgName, .T.

SET CONSOLE ON

IF !VARREAD() == "PBRUN")
  gcDevice=lcOGDevice
  gcOutFile=lcOGOutFile
  glHeader = llOGHeader
ENDIF  

llOGFltCh = .F.

RETURN .F.
*-- End of lfPreRun.

*!***************************************************************************
*! Name      : lpGenExpr
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 12/18/2001
*! Purpose   : Generate Expression
*!***************************************************************************
*! Example   : DO lpGenExpr
*!***************************************************************************
*
PROCEDURE lpGenExpr
PRIVATE lcAlias , lnX
lcAlias = ALIAS()

*-- Copy all laOGFxFlt to another array to save the old value.
DIMENSION laTempExpr[1]
=ACOPY(laOGFxFlt,laTempExpr)         && Copy Fixed Filter Array to Temp Array.

*-- Define new Fixed filter array to hold one expression only.
DIMENSION laOGFxFlt[1,7]
laOGFxFlt = ""          

*-- we need to generate a new Month Range Expression based on INVLINE.INVDATE.
PRIVATE lcSetCent , lcPerFrom , lcPerTo
lcSetCent = SET('CENT')
SET CENT ON

laOGFxFlt[1,1] = PADR("INVLINE.INVDATE",18)
laOGFxFlt[1,2] = "F"
laOGFxFlt[1,3] = "D"
laOGFxFlt[1,4] = .T.
laOGFxFlt[1,5] = PADR('Between',20)

DO CASE
  CASE lcRpQuarter = '1'
    lcPerFrom = "01/01/" + ALLTRIM(lcRpYear)
    lcPerTo = "03/31/" + ALLTRIM(lcRpYear)
  CASE lcRpQuarter = '2'
    lcPerFrom = "04/01/" + ALLTRIM(lcRpYear)
    lcPerTo = "06/30/" + ALLTRIM(lcRpYear)
  CASE lcRpQuarter = '3'
    lcPerFrom = "07/01/" + ALLTRIM(lcRpYear)
    lcPerTo = "09/30/" + ALLTRIM(lcRpYear)
  CASE lcRpQuarter = '4'
    lcPerFrom = "10/01/" + ALLTRIM(lcRpYear)
    lcPerTo = "12/31/" + ALLTRIM(lcRpYear)
ENDCASE

laOGFxFlt[1,6] = lcPerFrom+"|"+lcPerTo
laOGFxFlt[1,7] = "V"

*-- Generate expression for Month Range.
lcInvExp = gfGenFlt('laOGFxFlt',.T.,.T.)
lcRetExp = STRTRAN(lcInvExp,"INVLINE.INVDATE","RETLINE.CRDATE")
SET CENT &lcSetCent

*-- Restore original laOGFxFlt.
DIMENSION laOGFxFlt[1]
=ACOPY(laTempExpr,laOGFxFlt)         && Restore Fixed Filter Array to Temp Array.

SELECT (lcAlias)
*-- End of lpGenExpr.

*!***************************************************************************
*! Name      : lfQuarter
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 12/18/2001
*! Purpose   : Set the default quarter.
*!***************************************************************************
*! Example   : =lfQuarter()
*!***************************************************************************
*
FUNCTION lfQuarter
PRIVATE lnMonth
lnMonth = MONTH(gdSysDate)
DO CASE
  CASE BETWEEN(lnMonth,1,3)
    lcDefQrter = '1'  
  CASE BETWEEN(lnMonth,4,6)
    lcDefQrter = '2'
  CASE BETWEEN(lnMonth,7,9)
    lcDefQrter = '3'
  CASE BETWEEN(lnMonth,10,12)
    lcDefQrter = '4'
ENDCASE
*-- End of lfQuarter.