***********************************************************************
*:  Program file : SOIMPSO.PRG
*:  Program desc.: Import Sales orders from CSV files
*:         System: Aria 4XP
*:      Developer: Mariam Mazhar[MMT]
*:           Date: 11/14/2011
*:      Reference: C201419.Exe,C201412.122[T20110506.0009]
**************************************************************************
*: Modifications**********************************************************
*! B610231,1 HIA 02/10/2013 Order Maintenance - first thing in the morning - always get this string of errors [T20121231.0025]
*! B610306,1 HIA 04/16/2013 T20121231.0025 - Order Maintenance - first thing in the morning - always get this string of errors
*! B610625,1 TMI 12/15/2013 replace ARIA27 with ARIA4XP to locate the correct classes path [T20131204.0013]
*! B610902,1 MMT 11/05/2014 Import Web orders not working with company with style major length <> 8[T20141007.0009]
*! B611010,1 MMT 06/02/2015 Custom Import Sales program gives error at SHA13[T20150427.0016]
*!C202073 MHM Cannot Import Web Orders using template Sport Haley Orders
*:************************************************************************
lcExpr = gfOpGrid('SOIMPSOR' , .T.)
*!*************************************************************
*! Name      : lfWhenImp
*! Developer : Mariam Mazhar(MMT)
*! Date      : 11/14/2011
*! Purpose   : When Function
*!*************************************************************
FUNCTION lfWhenImp
IF EMPTY(lcRpHist) AND FILE(ADDBS(ALLTRIM(oAriaApplication.ResourceHome))+ ADDBS(ALLTRIM(oAriaApplication.User_ID))+'\IMPORD.MEM')
  RESTORE FROM (ADDBS(ALLTRIM(oAriaApplication.ResourceHome))+ ADDBS(ALLTRIM(oAriaApplication.User_ID))+'\IMPORD.MEM') ADDITIVE
ENDIF

*!*************************************************************
*! Name      : lfvHist
*! Developer : Mariam Mazhar(MMT)
*! Date      : 11/14/2011
*! Purpose   : Validate History Path
*!*************************************************************
FUNCTION lfvHist
*lcRpHist
IF (!EMPTY(lcRpHist) AND !DIRE(lcRpHist)) OR '?' $ lcRpHist
  lcRpHist = GETDIR('','History Directory')
ENDIF

*!*************************************************************
*! Name      : lfvHFile
*! Developer : Mariam Mazhar(MMT)
*! Date      : 11/14/2011
*! Purpose   : Validate Header file Path
*!*************************************************************
FUNCTION lfvHFile
*lcRpHFile
IF (!EMPTY(lcRpHFile) AND !FILE(lcRpHFile)) OR '?' $ lcRpHFile
  lcRpHFile = GETFILE('CSV','Header File','Select',2)
ENDIF
*!*************************************************************
*! Name      : lfvDFile
*! Developer : Mariam Mazhar(MMT)
*! Date      : 11/14/2011
*! Purpose   : Validate Detail file Path
*!*************************************************************
FUNCTION lfvDFile
IF (!EMPTY(lcRpDFile) AND !FILE(lcRpDFile)) OR '?' $ lcRpDFile
  lcRpDFile= GETFILE('CSV','Detail File','Select',2)
ENDIF
*!*************************************************************
*! Name      : lfImport
*! Developer : Mariam Mazhar(MMT)
*! Date      : 11/14/2011
*! Purpose   : Import function
*!*************************************************************
FUNCTION lfImport
IF (!EMPTY(lcRpHist) AND !DIRE(lcRpHist)) OR '?' $ lcRpHist OR EMPTY(lcRpHist)
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Invalid History Directory')
  RETURN .F.
ENDIF

IF (!EMPTY(lcRpHFile) AND !FILE(lcRpHFile)) OR '?' $ lcRpHFile OR EMPTY(lcRpHFile) OR UPPER(JUSTEXT(lcRpHFile)) <> 'CSV'
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Invalid Header file name')
  RETURN .F.
ENDIF
IF (!EMPTY(lcRpDFile) AND !FILE(lcRpDFile)) OR '?' $ lcRpDFile OR EMPTY(lcRpDFile) OR UPPER(JUSTEXT(lcRpDFile)) <> 'CSV'
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Invalid Detail file name')
  RETURN .F.
ENDIF
IF (ADDBS(JUSTPATH(lcRpDFile)) == ADDBS(lcRpHist)) OR (ADDBS(JUSTPATH(lcRpHFile)) == ADDBS(lcRpHist))
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Invalid History Directory')
  RETURN .F.
ENDIF
IF lcRpDFile == lcRpHFile
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Invalid Header File or detail File')
  RETURN .F.
ENDIF

lcRpHist = ADDBS(ALLTRIM(lcRpHist))
lcRpDFile  = ALLTRIM(lcRpDFile)
lcRpHFile = ALLTRIM(lcRpHFile)
IF !EMPTY(lcRpHist)
  SAVE ALL LIKE lcRpHist* TO (ADDBS(ALLTRIM(oAriaApplication.ResourceHome))+ ADDBS(ALLTRIM(oAriaApplication.User_ID))+'\IMPORD.MEM')
ENDIF

***Start Processing the file
*************** Step1: Calling the Dot Net Translator to create Aria XML File ******************************
* This step is waiting for Mostafa to create the Dot Net Dll
*lcAriaXmlFile  = "C:\Users\mmt\Desktop\AriaXMl3t54kb4p3od.xml"
*C202073 MHM Cannot Import Web Orders using template Sport Haley Orders [start]
CREATE CURSOR test ( workOrder c(50) ,startdate Date,Enddate Date,accountNo c(50))
append from  (lcRpHFile) type csv
SELECT Distinct AccountNo FROM test 
SELECT test 
SCAN 
IF !USED('EDIPH')
  =gfOpenTable('EDIPH','PARTNER','SH')
ENDIF
IF !SEEK(ALLTRIM(test.accountNo),'EDIPH','PARTNER')  
SELECT EDIPH
APPEND BLANK
replace cpartcode WITH ALLTRIM(test.accountNo)
replace cpartname WITH ALLTRIM(test.accountNo)
replace cnetwork  WITH 'WEB'
replace cFieldSep WITH ","
=TABLEUPDATE(.T.)
ENDIF
IF !USED('EDIPD')
  =gfOpenTable('EDIPD','PARTTRANS','SH')
ENDIF
IF !SEEK(ALLTRIM(test.accountNo),'EDIPD','PARTTRANS')
SELECT EDIPD
APPEND BLANK
replace cpartcode WITH ALLTRIM(test.accountNo)
replace ltrade WITH .T.
replace ceditrntyp WITH '850'
replace ctranactv WITH 'P'
=TABLEUPDATE(.T.)
ENDIF 
ENDSCAN 
*C202073 MHM Cannot Import Web Orders using template Sport Haley Orders
lcAriaXmlFile  = oAriaApplication.WorkDir+loogscroll.gfTempName()+'.XML'
loTranslator = CREATEOBJECT("CSVOrder.CSVOrder")
*! B610902,1 MMT 11/05/2014 Import Web orders not working with company with style major length <> 8[T20141007.0009][Start]
STORE  0 TO lnMajorLength,lnColorLength
STORE '' TO lcMajSep,lcColorSep
DECLARE laItemSeg[1]
PRIVATE lnCount 
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='F'
    lnMajorLength = LEN(laItemSeg[lnCount,3])
    lcMajSep = ALLT(laItemSeg[lnCount,6])
  ENDIF
  IF laItemSeg[lnCount,1]='C'
    lnColorLength = LEN(laItemSeg[lnCount,3])
    lcColorSep = ALLT(laItemSeg[lnCount,6])
  ENDIF
ENDFOR
loTranslator.nMajorLength = lnMajorLength 
loTranslator.cMajSep = lcMajSep
loTranslator.nColorLength = lnColorLength 
loTranslator.cColorSep = lcColorSep 
*! B610902,1 MMT 11/05/2014 Import Web orders not working with company with style major length <> 8[T20141007.0009][End]
loTranslator.IMPORT(lcRpHFile, lcRpDFile ,lcAriaXmlFile)

IF !FILE(lcAriaXmlFile)
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,"The DLL Doesn't Registered!, can't proceed.")
  RETURN .F.
ENDIF
************************************************************************************************************


*************** Step2: Calling the Transaction core all to update PO Entity with ******************************
** Get the Connection info. from Syccomp table
lnRemResult = oAriaApplication.RemoteSystemData.Execute("SELECT * FROM SYCCOMP WHERE CCOMP_ID='"+oAriaApplication.ActiveCompanyID+"'", '', 'SYCCOMP_IMP', "", oAriaApplication.SystemConnectionString, 3, "", SET("Datasession"))
IF lnRemResult <> 1
  RETURN .F.
ENDIF
lcServer = SYCCOMP_IMP.cconserver
lcDatabaseName = SYCCOMP_IMP.ccondbname
lcUsrNam = SYCCOMP_IMP.cconuserid
lcPasword = SYCCOMP_IMP.cconpaswrd
TRY
  *Create Object from the Transaction core dll
  IMPDLL = CREATEOBJECT("TransactionsCore.TransactionsCore")

  *Call the Import Method
  IMPDLL.IMPORT('850',lcServer,lcDatabaseName ,lcUsrNam,lcPasword,lcAriaXmlFile)
  RELEASE IMPDLL
CATCH
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,"The DLL Doesn't Registered!, can't proceed.")
ENDTRY
*** as per Mr.Badran we skipped the step of calling the transaction core again to get the XML from the PO database as we already have the same XML

*********************** Step3: Call the Adaptor that will create the Sales orders from XML file ********************************
* Get Default location from WareHouse Table
lcDefLocation = ''
IF !USED('WareHous')
  =gfOpenTable('WareHous','WareHous','SH')
ENDIF
SELECT WareHous
=gfSeek('')
LOCATE FOR ldefware
IF FOUND()
  lcDefLocation = WareHous.CWareCode
ELSE
  LOCATE
  lcDefLocation = WareHous.CWareCode
ENDIF

lcEDI3_lcEdiPath      = ADDBS(oAriaApplication.ediinstallationpath)
lcA27_lcA27Path   =    UPPER(ADDBS(oAriaApplication.SysPath))
*B610625,1 TMI 12/15/2013 13:00 [Start] replace ARIA27 with ARIA4XP to locate the correct classes path
*lcClientEDIPath   = STRTRAN(lcA27_lcA27Path,"ARIA27\SYSFILES\","ARIA3EDI\")
lcClientEDIPath   = STRTRAN(lcA27_lcA27Path,"ARIA4XP\SYSFILES\","ARIA3EDI\")
*B610625,1 TMI 12/15/2013 13:00 [End  ] 
IF FILE(lcClientEDIPath+'CLASSES\'+"EDIPO.VCX")
  lcEDI3ClassDir = lcClientEDIPath+'CLASSES\'
ELSE
  lcEDI3ClassDir = lcEDI3_lcEdiPath+  'CLASSES\'
ENDIF

*MT
llTotalyProcessed = .F.
*MT

*! B611010,1 MMT 06/02/2015 Custom Import Sales program gives error at SHA13[T20150427.0016][Start]
IF TYPE('oAriaApplication.isRemoteComp') ='U'
  oAriaApplication.AddProperty('isRemoteComp',.F.)
ENDIF
*C202073 MHM Cannot Import Web Orders using template Sport Haley Orders
oAriaApplication.isRemoteComp = .T.
*C202073 MHM Cannot Import Web Orders using template Sport Haley Orders
*! B611010,1 MMT 06/02/2015 Custom Import Sales program gives error at SHA13[T20150427.0016][End]

*! B610231,1 HIA 02/10/2013 Order Maintenance - first thing in the morning - always get this string of errors [T20121231.0025][Start]
lcOnError = ''
lcSetProcedure = ''
TRY
  lcOnError = ON('ERROR')
  lcSetProcedure = SET("Procedure")
CATCH
ENDTRY
*! B610231,1 HIA 02/10/2013 Order Maintenance - first thing in the morning - always get this string of errors [T20121231.0025][End]

lcEDI3PrgDir        = lcEDI3_lcEdiPath+  'PRGS\'
lcOldApp =oAriaApplication.ApplicationHome
oAriaApplication.ApplicationHome = lcEDI3PrgDir
*oAriaApplication.ApplicationHome = lcEDI3PrgDir

*! B610306,1 HIA 04/16/2013 T20121231.0025 - Order Maintenance - first thing in the morning - always get this string of errors [Start]
lcProcSet = 'Set Procedure To '+ ADDBS(lcEDI3_lcEdiPath) +'PRGS\EB\SOUPDATE.FXP Additive '
&lcProcSet.
*! B610306,1 HIA 04/16/2013 T20121231.0025 - Order Maintenance - first thing in the morning - always get this string of errors [End]

loEDIPO = NEWOBJECT('adpprocesspo',lcEDI3ClassDir+'EDIPO.VCX')

* HES
*!*	loEDIPO.ProcessPO(.T.,"",'',lcAriaXmlFile ,lcDefLocation,.T.)
lcErorFile = loEDIPO.ProcessPO(.T.,"",'',lcAriaXmlFile ,lcDefLocation,.T.)
* HES
*! B610306,1 HIA 04/16/2013 T20121231.0025 - Order Maintenance - first thing in the morning - always get this string of errors [Start]
lcRelease = 'RELEASE PROCEDURE '+ ADDBS(lcEDI3_lcEdiPath) +'PRGS\EB\SOUPDATE.FXP'
&lcRelease.
*! B610306,1 HIA 04/16/2013 T20121231.0025 - Order Maintenance - first thing in the morning - always get this string of errors [End]

*MT
llTotalyProcessed = loEDIPO.lmovesource
*MT
oAriaApplication.ApplicationHome  = lcOldApp

* HES
*!*	lcErorFile = oAriaApplication.WorkDir + loEDIPO.ErrorFile + '.TXT'
* HES

IF FILE(lcErorFile)
  lcErrorTxt = FILETOSTR(lcErorFile)
  IF !EMPTY(lcErrorTxt)
    CREATE CURSOR TMPSTR (mStrRep M(10))
    SELECT TMPSTR
    APPEND BLANK

    * HES, as it shows both accepted and rejected log
    *!*	    REPLACE mStrRep WITH REPLICATE('*',68) + CHR(13) +;
    *!*	                       "*                                Error log                                                                 *" + CHR(13) +;
    *!*	                       REPLICATE('*',68) + CHR(13) + ' ' + CHR(13)
    REPLACE mStrRep WITH REPLICATE('*',68) + CHR(13) +;
      "*                                Status log                                                                 *" + CHR(13) +;
      REPLICATE('*',68) + CHR(13) + ' ' + CHR(13)
    * HES

    REPLACE mStrRep WITH mStrRep+lcErrorTxt  IN TMPSTR
    SELECT TMPSTR
    DO FORM (oAriaApplication.clientscreenhome+ 'SO\soimplog.SCX')
    USE IN TMPSTR
  ENDIF
ENDIF

IF llTotalyProcessed
  *** Move CSV files to the History file after it has been processed
  *COPY FILE (lcRpHFile) TO (lcRpHist+JUSTSTEM(lcRpHFile)+'.'+JUSTEXT(lcRpHFile))
  COPY FILE (lcRpHFile) TO (lcRpHist+JUSTSTEM(lcRpHFile)+ALLTRIM(DTOS(DATE()))+ALLTRIM(STR(HOUR(CTOT(TIME()))))+ALLTRIM(STR(MINUTE(CTOT(TIME()))))+ALLTRIM(STR(SEC(CTOT(TIME()))))+'.'+JUSTEXT(lcRpHFile))
  ERASE (lcRpHFile)

  *COPY FILE (lcRpDFile) TO (lcRpHist+JUSTSTEM(lcRpDFile)+'.'+JUSTEXT(lcRpDFile))
  COPY FILE (lcRpDFile) TO (lcRpHist+JUSTSTEM(lcRpDFile)+ALLTRIM(DTOS(DATE()))+ALLTRIM(STR(HOUR(CTOT(TIME()))))+ALLTRIM(STR(MINUTE(CTOT(TIME()))))+ALLTRIM(STR(SEC(CTOT(TIME()))))+'.'+JUSTEXT(lcRpDFile))
  ERASE (lcRpDFile)
ENDIF

*! B610231,1 HIA 02/10/2013 Order Maintenance - first thing in the morning - always get this string of errors [T20121231.0025][Start]
TRY
  ON ERROR &lcOnError.
CATCH
ENDTRY
TRY
  SET PROCEDURE TO  &lcSetProcedure.
CATCH
ENDTRY
*! B610231,1 HIA 02/10/2013 Order Maintenance - first thing in the morning - always get this string of errors [T20121231.0025][End]

*!*************************************************************
*! Name      : lfOpen_SQL_File
*! Developer : Mariam Mazhar(MMT)
*! Date      : 11/14/2011
*! Purpose   : Open Sql Function to be used by EDI class instead the one in EDI Globals prg
*!*************************************************************
FUNCTION lfOpen_SQL_File
LPARAMETERS lcSelString,lcFile_name,lcIndex_expr,lcTagName

lnConnectionHandlar = 0
IF  !EMPTY(lcSelString)
  lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlrun(lcSelString ,lcFile_name,lcFile_name,;
    oAriaApplication.ActiveCompanyConStr,3,'BROWSE',SET("DATASESSION"))

  IF lnConnectionHandlar = 1
    SELECT (lcFile_name)
    =CURSORSETPROP("Buffering" ,3,lcFile_name)
    IF !EMPTY(lcIndex_expr)
      IF TYPE('lcTagName') = 'C' AND !EMPTY(lcTagName)
        lcIndex_expr = 'INDEX ON '+lcIndex_expr + ' TAG '+ lcTagName
      ELSE
        lcIndex_expr = 'INDEX ON '+lcIndex_expr + ' TAG '+lcFile_name
      ENDIF
      &lcIndex_expr.
    ENDIF
    =CURSORSETPROP("Buffering" ,5,lcFile_name)
  ELSE
    lcResult = oAriaApplication.RemoteCompanyData.CheckRetResult('Execute',lnConnectionHandlar,.F.)
  ENDIF
ENDIF
RETURN ALLTRIM(STR(lnConnectionHandlar,2))
ENDFUNC
***************************
