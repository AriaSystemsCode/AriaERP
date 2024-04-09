*:***************************************************************************
*: Program file  : SMCCODE
*: Program desc. : Codes report
*: For Report    : (SMCCODE.FRX)
*: System        : Aria Advantage Series.4XP (NEW Framework)
*: Module        : System Manager (SM)
*: Developer     : Mariam MAzhar (MMT)
*: Date          : 09/28/2008
*:***************************************************************************
*: Modifications:
*:***************************************************************************
*: N000622 AHS 09/08/2009 Calling request builder fxp to collect data[T20090805.0001]
*:***************************************************************************
*T20100512.0026 Hassan 2010 05 23 [BEGIN]
*PARAMETERS lcRequestID, lcXMLFileName
PARAMETERS lcRequestID, lcXMLFileName, ClientId
*ClientId = "GMA10"
*lcRequestID="af799c08-0e43-4f91-9bf3-442f11870587"
*lcXMLFileName = "\\ARIA-HASSAN\C\ARIA4XP\OUTPUT\XA0TSSPY.xml"

*T20100512.0026 Hassan 2010 05 23 [END]


*N000622 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
  *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[End]
  PRIVATE loAgent
  loAgent = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.AriaRequestAgent")

  PRIVATE loProgress
  loProgress = CREATEOBJECT("Aria.DataTypes.RequestHandler.AriaRequestProgress")

  loProgress.Percent = 0
  loProgress.DESCRIPTION = "Opening Data Files..."
  *T20100512.0026 Hassan 2010 05 23 [BEGIN]
  *loAgent.UpdateObjectProgress(lcRequestID, loProgress)

  loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientId)
  *T20100512.0026 Hassan 2010 05 23 [END]

  LOCAL loEnvironment
  *T20100512.0026 Hassan 2010 05 23 [BEGIN]
  loEnvironment = CREATEOBJECT("Aria.Environment.AriaEnviromentVariables")
  loEnvironment.ClientId = ClientId
  loEnvironment.ConnectionsRefresh()
  loEnvironment.GetAria27CompanyDataConnectionString(loAgent.GetRequestCompany(lcRequestID, ClientId))

  *T20100512.0026 Hassan 2010 05 23 [END]
  LOCAL lcCurrentProcedure
  lcCurrentProcedure = STRTRAN(UPPER(loEnvironment.Aria40SystemFilesPath), UPPER("SQLDictionary\"), "", -1, 1, 1)
  lcCurrentProcedure = STRTRAN(UPPER(loEnvironment.Aria40SystemFilesPath), UPPER("SQLDictionary"), "", -1, 1, 1)

  *T20100512.0026 Hassan 2010 05 23 [BEGIN]
  *DO (lcCurrentProcedure + "SRVPRGS\SY\ariamain.fxp") WITH loAgent.GetRequestCompany(lcRequestID)
  DO (lcCurrentProcedure + "SRVPRGS\SY\ariamain.fxp") WITH loAgent.GetRequestCompany(lcRequestID, ClientId), ClientId
  *T20100512.0026 Hassan 2010 05 23 [END]


  oAriaEnvironment.XML.RestoreFromXML(FILETOSTR(lcXMLFileName),.T.)

  oAriaEnvironment.REPORT.gcAct_Appl = lcActiveMod
  oAriaEnvironment.activeModuleID = 'SM'

  PUBLIC gcAct_Appl
  gcAct_Appl = lcActiveMod

  IF LEFT(gcDevice, 7) = "PRINTER"
    oAriaEnvironment.gcDevice = "PRINTER"
  ELSE
    oAriaEnvironment.gcDevice = "FILE"
  ENDIF
  *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
ENDIF
*N000622 AHS 09/08/2009 Calling request builder fxp to collect data[End]


IF USED('SYCCOMP')
  USE IN SYCCOMP
ENDIF
*N000622 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
  *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[End]
  oAriaEnvironment.gcDevice = 'FILE'

  oAriaEnvironment.remotetableaccess.OPENTABLE(oAriaEnvironment.systemfilespath+'SydField' ,'CFLD_NAME','SH',.F.,.F.)
  oAriaEnvironment.remotetableaccess.OPENTABLE(oAriaEnvironment.systemfilespath+'SYCCOMP' ,'CCOMP_ID','SH',.F.,.F.)

  lcPrintHdr = oAriaEnvironment.CURSORS.GetCursorTempName()        && Temp Cursor For the Header of codes and Related fields to be print
  lcHeadTemp = oAriaEnvironment.CURSORS.GetCursorTempName()        && Temp Cursor For the Header of codes and Related fields
  lcDetalTmp = oAriaEnvironment.CURSORS.GetCursorTempName()        && Temp Cursor For the Details of codes and Related fields to be print
  lcHdrIndex = oAriaEnvironment.CURSORS.GetCursorTempName()        && Temp Name for Header index used in print only.
  *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
ELSE
  =gfOpenFile(oAriaApplication.syspath+'SYDFIELD','CFLD_NAME','SH',.F.,.F.)
  =gfOpenFile(oAriaApplication.syspath+'SYCCOMP','CCOMP_ID','SH',.F.,.F.)
  lcPrintHdr = gfTempName()        && Temp Cursor For the Header of codes and Related fields to be print
  lcHeadTemp = gfTempName()        && Temp Cursor For the Header of codes and Related fields
  lcDetalTmp = gfTempName()        && Temp Cursor For the Details of codes and Related fields to be print
  lcHdrIndex = gfTempName()        && Temp Name for Header index used in print only.
ENDIF
*N000622 AHS 09/08/2009 Calling request builder fxp to collect data[End]

lcStTime = TIME()
lfFillCode()

IF TYPE("laRltFdInf[1,1]")$"UL"
  =lfFillRelt()              
        && Fill Related fields array.
ENDIF

*-- if user change filter expression.(We Collect the Data of the report again)
*-- lnSepWidth : Variable to hold the separator between the Headers of the realated fields.
PRIVATE lnSepWidth
lnSepWidth = 3

=lfCollect()  && Collect report data.


*-- Asking if no records (Display message) otherwise print report [Begin.]
IF RECCOUNT(lcPrintHdr) = 0
  *---Text : 'No Record Selected for the report..!'
  * =gfModalGen('TRM00052B00000','DIALOG')
  RETURN
ENDIF
*-- Asking if no records (Display message) otherwise print report [End.]

PRIVATE lcCurrFld , lnCompPos , lcCompName
*-- lcCurrFld : This Variable will be used in .FRX to detect if we are at start
*--             of new code group or not (To print headers).
lcCurrFld = SPACE(10)

*-- Get the ID and the Name of the selected company to be print
lnCompPos = ASCAN(laCompDesc,lcRpComp)  && Scan the array laCompDesc which hold Description of
&& company (ID , Name) by the selected company
IF lnCompPos > 0
  lnCompPos  = ASUBSCRIPT(laCompDesc,lnCompPos,1)
  lcCompName = laCompDesc[lnCompPos]          && Get the Id & the name of selected company
ENDIF

*-- Set relation to header print table
SELECT (lcPrintHdr)
SET ORDER TO (lcHdrIndex)
SET RELATION TO cFld_name INTO (lcDetalTmp)
SET SKIP TO (lcDetalTmp)


llnewPage=.T.   && Detect if it new page

*N000622 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
  *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[End]
  oAriaEnvironment.REPORT.OGLastForm = lcRpName

  loProgress.Percent = 0.9
  loProgress.DESCRIPTION = "Printing Report..."
  *T20100512.0026 Hassan 2010 05 23 [BEGIN]
  *loAgent.UpdateObjectProgress(lcRequestID, loProgress)
  loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientId)
  *T20100512.0026 Hassan 2010 05 23 [END]

  PRIVATE loProxy
  loProxy = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy")
  *T20100512.0026 Hassan 2010 05 23 [BEGIN]
  *IF loProxy.GetRequest(lcRequestID).Status = 3
  IF loProxy.GetRequest(lcRequestID, ClientId).STATUS = 3
    *T20100512.0026 Hassan 2010 05 23 [END]
    oAriaEnvironment.REPORT.PRINT(oAriaEnvironment.REPORT.OGLastForm)

    loProgress.Percent = 1.0
    loProgress.DESCRIPTION = "Printing Report..."
    *T20100512.0026 Hassan 2010 05 23 [BEGIN]
    *loAgent.UpdateObjectProgress(lcRequestID, loProgress)
    loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientId)
    *T20100512.0026 Hassan 2010 05 23 [END]
  ENDIF
  *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
ELSE
  DO gfdispre WITH EVALUATE('lcRpName')
ENDIF
*N000622 AHS 09/08/2009 Calling request builder fxp to collect data[End]
*-- Remove relation
SET RELATION TO



*!*************************************************************
*! Name      : lfCollect
*: Developer     : Mariam MAzhar (MMT)
*: Date          : 09/28/2008
*! Purpose   : Collect the report Data
*!*************************************************************
*N000120,1
FUNCTION lfCollect
PRIVATE lnLines,lnI,lnWidth ,lnFound, lcDesc , lcCurrCode , lcCodes , lnCnt
STORE "" TO lcDesc , lcCurrCode

=lfNewSeson()      && Open Codes for selected company and creates the needed temp


&& cursors (lcPrintHdr, lcHeadTemp, lcDetalTmp)

SELECT (lcTempCode)   && Select the temp cursor which is a copy of "SydField"
COUNT FOR cFld_name = lcRpCode AND !DELETED() TO lnCodeCnt

SELECT (lcTempCode)   && Select the temp cursor which is a copy of "SydField"
LOCATE
*-- Scan the temp cursor which holds all the codes for the selected code(s).
SCAN FOR cFld_name = lcRpCode

  *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
  IF TYPE('lcXMLFileName') = 'C'
    *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[End]
    lnPerCent = RECNO()/lnCodeCnt
    IF MOD(RECNO(),CEILING(lnCodeCnt / 10)) = 0
      loProgress.Percent = lnPerCent * 0.9
      loProgress.DESCRIPTION = "Collecting Data..."
      *T20100512.0026 Hassan 2010 05 23 [BEGIN]
      *loAgent.UpdateObjectProgress(lcRequestID, loProgress)
      loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientId)
      *T20100512.0026 Hassan 2010 05 23 [END]
    ENDIF

    *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
  ENDIF
  *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[End]

  =lfFillMast()  && Fill Header and details for master code records.

  *-- if there is a related fields for this code
  IF &lcTempCode..lrltFields

    =lfHdrRelat()  && Fill related Memo/Records in Header File
    =lfDetRelat()  && Fill related Memo/Records in Detail File

  ENDIF  && end if there is a related fields for this code

ENDSCAN   && end Scan the temp cursor for the selected code or codes
*-- end of lfCollect.

*!*************************************************************
*! Name      : lfFillMast
*: Developer     : Mariam MAzhar (MMT)
*: Date          : 09/28/2008
*! Purpose   : Fill The Header file and the Detail file for
*!           : master records (which is the codes itself)
*!*************************************************************
*: Called From : Function (lfCollect) , within scaning the temp
*!             : cursor which holds all the codes
*!*************************************************************

FUNCTION lfFillMast

*-- Note : Selected Alias is Temp. SydField which is (lcTempCode)
*-- Fill Header for master records [Begin]
SCATTER MEMVAR
INSERT INTO (lcHeadTemp) FROM MEMVAR     && Fill Header for master records (which is the codes itself)
INSERT INTO (lcPrintHdr) FROM MEMVAR     && Fill Header for master records to be Print (which is the codes itself)
*-- Fill Header for master records [End  ]

SELECT Codes
*-- Fill Detail for master records(which is the value of the code itself) [Begin]
*N000622 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
  *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[End]
  oAriaEnvironment.remotetableaccess.setorderto('Idrltfname')
  *SET ORDER TO Idrltfname IN CODES    && order codes file on index "Idrltfname"

  && which is (cdefcode+crltfield+cfld_name)
  *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
ELSE
  gfsetorder('Idrltfname')
ENDIF
*N000622 AHS 09/08/2009 Calling request builder fxp to collect data[End]

*-- if Seek in codes file for not defaulted records
*-- and it is the header record for active code


*N000622 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
  *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[End]
  IF oAriaEnvironment.remotetableaccess.SEEKRECORD("NN"+&lcTempCode..cFld_name  ,"CODES")
    *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
    SELECT Codes

    *-- Scan codes table for not defaulted records and it is the header record for active code
    *-- 1St N : Stands for : not defaulted records
    *-- 2Nd N : Stands for : header record (not a releated record)
    SCAN WHILE cdefcode+crltfield+cFld_name =;
        "NN" + &lcTempCode..cFld_name
      *-- If this code is editable the header of it will be "code # - discrption"
      *-- if not editable the header will be "discrption"
      lcDesc = IIF("EDITABLE" $ &lcTempCode..mcodeinfo, ccode_no+"-" , "") + cdiscrep

      INSERT INTO (lcDetalTmp) (cFld_name ,ccode_no, cFld_Desc) VALUES;
        (Codes.cFld_name ,Codes.ccode_no, lcDesc)

    ENDSCAN  && end Scan codes table for not defaulted records and it is the header
    && record for active code

    lcCurrCode = &lcDetalTmp..cFld_name   && save the active code in the global variavle "lcCurrcode"
    *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
  ENDIF
  *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[End]
ENDIF  && end if Seek in codes file for not defaulted records
&& and it is the header record for active code
*-- Fill Detail for master records [End  ]




IF TYPE('lcXMLFileName') != 'C'
  IF gfseek("NN"+&lcTempCode..cFld_name  ,"CODES")
    *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[End]

    SELECT Codes

    *-- Scan codes table for not defaulted records and it is the header record for active code
    *-- 1St N : Stands for : not defaulted records
    *-- 2Nd N : Stands for : header record (not a releated record)
    SCAN WHILE cdefcode+crltfield+cFld_name =;
        "NN" + &lcTempCode..cFld_name
      *-- If this code is editable the header of it will be "code # - discrption"
      *-- if not editable the header will be "discrption"
      lcDesc = IIF("EDITABLE" $ &lcTempCode..mcodeinfo, ccode_no+"-" , "") + cdiscrep

      INSERT INTO (lcDetalTmp) (cFld_name ,ccode_no, cFld_Desc) VALUES;
        (Codes.cFld_name ,Codes.ccode_no, lcDesc)

    ENDSCAN  && end Scan codes table for not defaulted records and it is the header
    && record for active code

    lcCurrCode = &lcDetalTmp..cFld_name   && save the active code in the global variavle "lcCurrcode"
    *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
  ENDIF
  *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[End]
ENDIF  && end if Seek in codes file for not defaulted records
&& and it is the header record for active code
*-- Fill Detail for master records [End  ]

*-- end of lfFillMast.

*!*************************************************************
*! Name      : lfHdrRelat
*: Developer     : Mariam MAzhar (MMT)
*: Date          : 09/28/2008
*! Purpose   : Fill related Memo/Records in Header File
*!*************************************************************
*! Called From : Function (lfCollect) , within scaning the temp
*!             : cursor which holds all the codes if there is
*!             : releated fields
*!*************************************************************
*N000120,1
FUNCTION lfHdrRelat


SELECT (lcTempCode)   && Select the temp cursor which holds all codes

DIMENSION laRLtFLd[1]     && array to hold the relaeted fields for the active code
lcRelated = STRTRAN(ALLTRIM(mrltFields),"$","")
lcRelated = STRTRAN(ALLTRIM(lcRelated),"~","")
=gfSubStr(lcRelated,@laRLtFLd,"|")

lnWidth = 90     && the width for the header of the releated fields in the report

FOR lnI=1 TO ALEN(laRLtFLd)
  lnFound = ASCAN(laRltFdInf,ALLTRIM(laRLtFLd[lnI]))   && scan the array which holds all the releated fields for all codes
  && for the releated fields for the active code
  *-- if the releated fields for the active found in "laRltFdInf"
  IF lnFound > 0
    lnFound = ASUBSCRIPT(laRltFdInf,lnFound,1)
    *-- call function lfFillMemo() to fill the memo field of the header file by the valus found
    *-- in "laRltFdInf" for the active code
    =lfFillMemo(laRltFdInf[lnFound,1],laRltFdInf[lnFound,2],;
      laRltFdInf[lnFound,3])
  ELSE
    *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
    IF TYPE('lcXMLFileName') = 'C'
      *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[End]
      *-- if this field is found in sydfield
      IF oAriaEnvironment.remotetableaccess.SEEKRECORD(PADR(laRLtFLd[lnI],10),"SydField")
        *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
      ELSE
        IF gfseek(PADR(laRLtFLd[lnI],10),"SydField")
        ENDIF
        *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[End]

        *-- Add new Items to Array laRltFdInf
        DIMENSION laRltFdInf[ALEN(laRltFdInf,1) + 1 , ALEN(laRltFdInf,2)]
        laRltFdInf[ALEN(laRltFdInf,1),1] = SydField.cFld_name
        laRltFdInf[ALEN(laRltFdInf,1),2] = ALLTRIM(SydField.cfld_head)
        laRltFdInf[ALEN(laRltFdInf,1),3] = SydField.nfld_wdth
        *-- call function lfFillMemo() to fill the memo field of the header file by the valus found
        *-- in "SydField"  for the active code
        =lfFillMemo(SydField.cFld_name,SydField.cfld_head,;
          SydField.nfld_wdth)
      ENDIF  && if this field is found in sydfield
    ENDIF  && if the releated fields for the active found in "laRltFdInf"
    *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
  ENDIF
  *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[End]

ENDFOR
*-- end of lfHdrRelat.

*!*************************************************************
*! Name      : lfDetRelat
*: Developer     : Mariam MAzhar (MMT)
*: Date          : 09/28/2008
*! Purpose   : Fill related Memo/Records in Detail File
*!*************************************************************
*! Called From : Function (lfCollect) , within scaning the temp
*!             : cursor which holds all the codes if there is
*!             : releated fields
*!*************************************************************
*N000120,1
FUNCTION lfDetRelat
PRIVATE lnNoOfRec , lcDesc

*-- if the active code found in the detail file we get it
IF SEEK(lcCurrCode,lcDetalTmp)
  SELECT (lcDetalTmp)          && Select the detail file

  *-- scan the detail file for the active code
  SCAN FOR cFld_name = lcCurrCode
    SCATTER MEMVAR
    *-- If the crosponding record to the active code found in the Header file we get it
    IF SEEK (cFld_name,lcHeadTemp)
      lnNoOfRec = 1       && variable to hold # of records for the active code in the
      && Header file

      SELECT (lcHeadTemp) && Select the Header file
      *-- scan the Header file for the active code
      SCAN FOR cFld_name = &lcDetalTmp..cFld_name
        *-- if The # of records for the active code is more than one
        *-- we select the detail file and add another record for the same active code
        IF lnNoOfRec > 1
          SELECT (lcDetalTmp)
          APPEND BLANK
          REPLACE cFld_name WITH m.cFld_name ,;
            ccode_no  WITH m.ccode_no
          =lfFlDetMem()   && Call function to fill the memo of the detail file for the active code
        ELSE
          *-- Update Detail memo field
          =lfFlDetMem()  && Call function to fill the memo of the detail file for the active code
          lnNoOfRec = lnNoOfRec + 1  && incrementing the # of records

        ENDIF && if The # of records for the active code is more than one

      ENDSCAN  && scan the Header file for the active code
    ENDIF && If the crosponding record to the active code found in the Header file we get it

  ENDSCAN  && scan the detail file for the active code

ENDIF  && if the active code found in the detail file
*-- end of lfDetRelat.

*!****************************************************************************
*! Name      : lfFillMemo
*: Developer     : Mariam MAzhar (MMT)
*: Date          : 09/28/2008
*! Purpose   : Fill memo field of Header Temp files for the
*!           : active code
*!****************************************************************************
*! Called From : Function (lfHdrRelat)
*!****************************************************************************
*! Parameters  : lcFldcode : hold the code of the related field
*!             :           : from either the array (laRltFdInf) or (Sydfield)
*!             : lcFldHead : hold the Header of the related field
*!             :           : from either the array (laRltFdInf) or (Sydfield)
*!             : lnFldWidth: hold the width of the related field
*!             :           : from either the array (laRltFdInf) or (Sydfield)
*!****************************************************************************
*N000120,1
FUNCTION lfFillMemo
PARAMETERS lcFldcode,lcFldHead, lnFldWidth
PRIVATE lcFldcode,lcFldHead, lnFldWidth , lcSepartor

lcSepartor= "|" + SPACE(lnSepWidth-1)   && variable to hold the separator between
&& related fields in the memo

lcFldHead = ALLTRIM(lcFldHead)
lnFldWidth = MAX(lnFldWidth,LEN(lcFldHead))  && Get the maxmam width of the related fields
&& the width of it from ("laRltFdInf" or "Sydfield")
&& or the lenth of the header

*-- if the lenth of the header less than the width for the header of the
*-- releated fields in the report save it in the memo field , otherwise
*-- insert new record in the detail file for the same code
IF LEN(lcFldHead) <= lnWidth
  *-- Accomulate Related Fields for Temp. Header File
  SELECT (lcHeadTemp)
  REPLACE mRlt_Code WITH mRlt_Code + IIF(EMPTY(mRlt_Code),"",lcSepartor) +;
    PADR(lcFldcode,lnFldWidth),;
    mRel_fld WITH mRel_fld + IIF(EMPTY(mRel_fld),"",SPACE(lnSepWidth)) +;
    PADR(lcFldHead,lnFldWidth)

  *-- Accomulate Related Fields for Temp. Print Header File
  SELECT (lcPrintHdr)
  REPLACE mRel_fld WITH mRel_fld + IIF(EMPTY(mRel_fld),"",SPACE(lnSepWidth)) +;
    PADR(lcFldHead,lnFldWidth)

ELSE

  *-- Insert New Record into Temp. Related Header
  INSERT INTO (lcHeadTemp) (cFld_name , mRlt_Code, mRel_fld );
    VALUES                  (&lcTempCode..cFld_name, PADR(lcFldcode,lnFldWidth),;
    PADR(lcFldHead,lnFldWidth))

  *-- Accomulate Related Fields for Temp. Print Header File
  SELECT (lcPrintHdr)
  REPLACE mRel_fld WITH mRel_fld + IIF(EMPTY(mRel_fld),"",CHR(13)) +;
    PADR(lcFldHead,lnFldWidth)

  lnWidth = 90
ENDIF  && if the lenth of the header less than the width for the header of the
&& releated fields in the report

lnWidth = lnWidth - ( lnFldWidth + lnSepWidth)  && decrement the width for the header of the
&& releated fields in the report by the width
&& of the header of the related field
*-- end of lfFillMemo

*!*************************************************************
*! Name      : lfFlDetMem
*: Developer     : Mariam MAzhar (MMT)
*: Date          : 09/28/2008
*! Purpose   : Fill memo field of Detal Temp files
*!*************************************************************
*! Called From : Function (lfDetRelat)
*!**************************************************************

FUNCTION lfFlDetMem
PRIVATE lnI , lnPos , lnRltPos , lcRltd_Name , lcMemoStr , lnSpaceLen
lcMemoStr = ""     && String variable to hold the values of the related
&& fiels and the separator between them

DIMENSION laRltcode[1]    && array to hold the codes of the related fiels from
&& memo field which hold them in the header file
STORE "" TO laRltcode



SELECT Codes
*N000622 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
  *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[End]
  oAriaEnvironment.remotetableaccess.setorderto('CODES')
  oAriaEnvironment.Strings.getSubstring(&lcHeadTemp..mRlt_Code,@laRltcode,"|")
  *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
ELSE
  gfsetorder('CODES')
  gfSubStr(&lcHeadTemp..mRlt_Code,@laRltcode,"|")
ENDIF
*N000622 AHS 09/08/2009 Calling request builder fxp to collect data[End]






DIMENSION laTemp[ALEN(laRltCode,1),3]  && temp array to hold the values of the related fiels
&& and the start position to be print from in the report
*B605226,1 RAE [END]

laRltcode[1]  = SPACE(2) + laRltcode[1]

SELECT Codes
*-- if Seek in codes file for not defaulted records
*-- and it is the related field for active code and active code # get it
*cdefcode+ccode_no+crltfield+cfld_name
*N000622 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
  *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[End]
  IF oAriaEnvironment.remotetableaccess.SEEKRECORD("N" + PADR(&lcDetalTmp..ccode_no,6) + "Y" + PADR(&lcDetalTmp..cFld_name,10))
    *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
  ELSE
    IF gfseek("N" + PADR(&lcDetalTmp..ccode_no,6) + "Y" + PADR(&lcDetalTmp..cFld_name,10))
    ENDIF
    *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[End]

    *-- Scan codes table for not defaulted records and it is he related field for active code
    *-- and active code #
    *-- N : Stands for : not defaulted records
    *-- Y : Stands for : releated record

    SCAN REST WHILE  cdefcode+ccode_no+crltfield+cFld_name = "N" + PADR(&lcDetalTmp..ccode_no,6) +;
        "Y" + PADR(&lcDetalTmp..cFld_name,10)
      lcRltd_Name = SPACE(lnSepWidth-1) + ALLTRIM(Codes.crltd_nam)   && variable to hold the code
      && of the related field
      lnPos = ASCAN(laRltcode,lcRltd_Name)             && scan the array which hold the codes of
      && the related fiels by lcRltd_Name
      *-- if found calculate the start position of it ,to be print in the report
      IF lnPos > 0
        lnPos = ASUBSCRIPT(laRltcode,lnPos,1)
        lnRltPos = 0
        *-- if the start position not (1) calculate it
        IF lnPos > 1
          FOR lnI=1 TO lnPos-1
            lnRltPos = lnRltPos + IIF(lnI=1 , LEN(laRltcode[lnI]), LEN(laRltcode[lnI])-(lnSepWidth-1));
              + lnSepWidth
          ENDFOR
        ELSE
          lnRltPos = 1
        ENDIF    && end if the start position not (1) calculate it

        *-- fill the temp array with the value of the related field and its position
        laTemp[lnPos,1] = ALLTRIM(Codes.crltd_vlu)
        laTemp[lnPos,2] = lnRltPos

        *B605226,1 RAE [START]
        laTemp[lnPos,3] = ALLTRIM(Codes.crltd_nam)
        *B605226,1 RAE [END]

      ENDIF && end if found calculate the start position of it ,to be print in the report

    ENDSCAN  && end Scan codes table for not defaulted records and it is he related field for active code
    && and active code #
    *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
  ENDIF
ENDIF
*N000622 AHS 09/08/2009 Calling request builder fxp to collect data[End]

*WAB AS per Amin
*we have add the next lines of code to insert the Obseleted value to the report instead of creating a fix program
*to add the related field obeselete to the color code.
*N000622 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
  *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[End]
  IF ASCAN(laTemp,'LLOBSOLETE') = 0 .AND. oAriaEnvironment.remotetableaccess.SEEKRECORD('LLOBSOLETE','SYDFIELD')
    *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
  ELSE
    IF ASCAN(laTemp,'LLOBSOLETE') = 0 .AND. gfseek('LLOBSOLETE','SYDFIELD')
    ENDIF
    *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[End]

    lcRltd_Name = SPACE(lnSepWidth-1) + ALLTRIM('LLOBSOLETE')
    lnPos = ASCAN(laRltcode,lcRltd_Name)             && scan the array which hold the codes of
    && the related fiels by lcRltd_Name
    *-- if found calculate the start position of it ,to be print in the report
    IF lnPos > 0
      lnPos = ASUBSCRIPT(laRltcode,lnPos,1)
      lnRltPos = 0
      *-- if the start position not (1) calculate it
      IF lnPos > 1
        FOR lnI=1 TO lnPos-1
          lnRltPos = lnRltPos + IIF(lnI=1 , LEN(laRltcode[lnI]), LEN(laRltcode[lnI])-(lnSepWidth-1));
            + lnSepWidth
        ENDFOR
      ELSE
        lnRltPos = 1
      ENDIF    && end if the start position not (1) calculate it

      *-- fill the temp array with the value of the related field and its position
      laTemp[lnPos,1] = 'N'
      laTemp[lnPos,2] = lnRltPos
      laTemp[lnPos,3] = 'LLOBSOLETE'
    ENDIF
  ENDIF
  *wab

  *-- fill the string which holds the values of related fields and order them
  *-- by their positions
  FOR lnI = 1 TO ALEN(laTemp,1)
    *-- if this element in the array not define store null to the value and 0 to the position
    IF TYPE("laTemp[lnI,2]") $ "UL"
      laTemp[lnI,1] = ""
      laTemp[lnI,2] = 0
    ENDIF     && end if this element in the array not define store
    && null to the value and 0 to the position

    *-- if this element is logical store "yes" insted of "T" and "No" insted of "F" to the value

    *B605226,1 RAE [START]
    *IF TYPE("laTemp[lnI,1]") $ "L"
    *laTemp[lnI,1] = IIF(laTemp[lnI,1],"Yes","No")
    *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
    IF TYPE('lcXMLFileName') = 'C'
      *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[End]
      IF TYPE('laTemp[lnI,3]') = "C" .AND. oAriaEnvironment.remotetableaccess.SEEKRECORD(ALLTRIM(laTemp[lnI,3]),'SYDFIELD') .AND. SydField.cData_TYP = 'L'
        *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
      ELSE
        IF TYPE('laTemp[lnI,3]') = "C" .AND. gfseek(ALLTRIM(laTemp[lnI,3]),'SYDFIELD') .AND. SydField.cData_TYP = 'L'
        ENDIF
        *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
        laTemp[lnI,1] = IIF(laTemp[lnI,1] = 'T' ,"Yes","No")
      ENDIF  && end if this element is logical store "yes" insted of "T"
      && and "No" insted of "F" to the value
      *B605226,1 RAE [END]
      *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
    ENDIF
    *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[End]
    lnSpaceLen = IIF(lnI=1 OR (laTemp[lnI,2]=0),0, laTemp[lnI,2] - LEN(lcMemoStr) - 2)
    lcMemoStr = lcMemoStr + SPACE(lnSpaceLen) + laTemp[lnI,1]
  ENDFOR   && end fill the string which holds the values of related fields and order them
  && by their positions
  *-- Fill the memo field
  REPLACE &lcDetalTmp..cRel_vlu WITH lcMemoStr

ENDIF  && end if Seek in codes file for not defaulted records
&& and it is the related field for active code and active code #

*-- end of lfFlDetMem

*!*************************************************************
*! Name      : lfNewSeson
*: Developer     : Mariam MAzhar (MMT)
*: Date          : 09/28/2008
*! Purpose   : open and creats files to be used in the report
*!*************************************************************

FUNCTION lfNewSeson

PRIVATE lcDataDir

*-- Close Codes file if opened from another company
IF USED("CODES")
  *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
  IF TYPE('lcXMLFileName') = 'C'
    *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[End]
    oAriaEnvironment.remotetableaccess.CLOSETABLE("CODES")
    *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
  ELSE
    gfCloseTable("CODES")
  ENDIF
  *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[End]
ENDIF

*-- if the selected company found in "SYCCOMP" file get the directory of it
*N000622 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
  *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[End]
  IF oAriaEnvironment.remotetableaccess.SEEKRECORD(lcRpComp,"SYCCOMP",'CCOMP_ID')
    *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
  ELSE
    IF gfseek(lcRpComp,"SYCCOMP",'CCOMP_ID')
    ENDIF
    *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[End]
    *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
    IF TYPE('lcXMLFileName') = 'C'
      *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[End]
      lcDataDir = ALLTRIM(oAriaEnvironment.getdatadirectory(ALLTRIM(SYCCOMP.cCom_dDir)))
      lcDataDir = lcDataDir + IIF(RIGHT(lcDataDir,1)="\","","\")
    ENDIF   && end if the selected company found in "SYCCOMP" file get the directory of it
    *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
  ENDIF
  *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[End]

  *-- open the codes file for this company
  *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
     

  
  IF TYPE('lcXMLFileName') = 'C'
    *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[End]
    lcOldComp = oAriaEnvironment.ActiveCompanyId

  
  
    oAriaEnvironment.GetCompanyInformation(lcRpComp)
    

  
  
    oAriaEnvironment.remotetableaccess.OPENTABLE(oAriaEnvironment.datadir + 'CODES' ,'Ccode_no','SH',.F.,.F.,lcRpComp)
    *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
  ELSE
    lcOldComp = oAriaApplication.ActiveCompanyId
    oAriaApplication.GetCompanyInformation(lcRpComp)
    gfOpenTable(oAriaApplication.datadir + 'CODES' ,'Ccode_no','SH',.F.,.F.,lcRpComp)
  ENDIF
  

  
  *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[End]
  SELECT Codes
  *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
  IF TYPE('lcXMLFileName') = 'C'
    *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[End]
    oAriaEnvironment.remotetableaccess.setorderto('Ccode_no')
    oAriaEnvironment.ActiveCompanyId = lcOldComp
    oAriaEnvironment.GetCompanyInformation(lcOldComp)
      

    *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
  ELSE
    gfsetorder('Ccode_no')
    oAriaApplication.ActiveCompanyId = lcOldComp
    oAriaApplication.GetCompanyInformation(lcOldComp)
  ENDIF
  *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[End]
  *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
ENDIF
*N000622 AHS 09/08/2009 Calling request builder fxp to collect data[End]

=gfOpenFile(oAriaApplication.datadir+'CODES','Ccode_no','SH')  && Reopen Codes file from selected company

*-- Create temp. tables used by this report to print layout [Begin]
*-- Create Temp. Header Cursor
IF USED(lcHeadTemp)
  USE IN (lcHeadTemp)
ENDIF
CREATE CURSOR (lcHeadTemp) (cFld_name c(10), cfld_head c(30), mRlt_Code M(10), mRel_fld M(10))
ZAP
INDEX ON cFld_name TAG (lcHeadTemp)&& OF (goAriaEnvironment.WorkDir+lcHeadTemp+".CDX")

*-- Create Temp. Header Print File
IF USED(lcPrintHdr)
  USE IN (lcPrintHdr)
ENDIF
CREATE CURSOR (lcPrintHdr) (cFld_name c(10), cfld_head c(30), mRel_fld M(10))
ZAP
INDEX ON cfld_head TAG (lcHdrIndex) &&OF (goAriaEnvironment.WorkDir+lcPrintHdr+".CDX")
INDEX ON cFld_name TAG (lcPrintHdr) &&OF (goAriaEnvironment.WorkDir+lcPrintHdr+".CDX")
SET ORDER TO (lcPrintHdr)

*-- Create Temp. Detail File
IF USED(lcDetalTmp)
  USE IN (lcDetalTmp)
ENDIF
CREATE CURSOR (lcDetalTmp) (cFld_name c(10),ccode_no c(10) ,cFld_Desc c(41), cRel_vlu c(90))
ZAP
INDEX ON cFld_name + ccode_no TAG (lcDetalTmp) &&OF (goAriaEnvironment.WorkDir+lcDetalTmp+".CDX")
*-- Create temp. tables used by this report to print layout [End  ]

*-- end of lfNewSeson

*!************************************************************************
*! Name      : lfFillCode
*: Developer     : Mariam MAzhar (MMT)
*: Date          : 09/28/2008
*! Purpose   : Fill The arrays of companys information & code information
*!************************************************************************
*N000120,1
FUNCTION lfFillCode
PRIVATE lnI

*-- Fill Company Array
SELECT SYCCOMP
lnI = 1
*-- scaning "SYCCOMP" to get the companys information
SCAN
  DIMENSION laCompDesc[lnI,1] , laCompVal[lnI,1]
  laCompDesc[lnI,1] = cComp_Id + "-" + cCom_Name    && array to hold the companys information
  && (ID & Name)

  laCompVal[lnI,1]  = cComp_Id                      && array to hold the companys ID
  lnI = lnI + 1
ENDSCAN   && end scaning "SYCCOMP" to get the companys information

*-- Fill codes arrays.
*N000622 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
  *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[End]
  lcTempCode = oAriaEnvironment.CURSORS.GetCursorTempName()  && temp cursor to hold codes
  *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
ELSE
  lcTempCode = gfTempName()
ENDIF
*N000622 AHS 09/08/2009 Calling request builder fxp to collect data[End]
*-- select the needed information for codes from "Sydfield" and save it in the temp cuesor
*-- lcTempCode
SELECT  cFld_name , cfld_head ,mrltFields ,lrltFields ,mcodeinfo;
  FROM SydField;
  WHERE lvldentry ;
  ORDER BY cfld_head ;
  INTO CURSOR (lcTempCode)

*-- if there is no codes found and saved in (lcTempCode)
*-- terminate the Option Grid , else fill codes array
IF _TALLY = 0
  WAIT WINDOW "No Codes found"
  llOgTrmnat = .T.
  RETURN .F.
ELSE
  DIMENSION laCodeDesc[_TALLY + 1,1] , laCodeRet[_TALLY + 1,1]  && array to hold codes information
  laCodeDesc[1] = "All"
  laCodeRet[1]  = ""

  SELECT (lcTempCode)
  lnI = 2
  *-- scan temp cursor to fill the codes array
  SCAN
    laCodeDesc[lnI,1] = ALLTRIM(cfld_head)
    laCodeRet [lnI,1] = cFld_name
    lnI = lnI + 1
  ENDSCAN  && scan temp cursor to fill the codes array

ENDIF  && if there is no codes found and saved in (lcTempCode)

SELECT (lcTempCode)
INDEX ON cFld_name TAG (lcTempCode) &&OF (goAriaEnvironment.WorkDir+lcTempCode+".CDX")


*-- end of lfFillCode.

*!*************************************************************
*! Name      : lfFillRelt
*: Developer     : Mariam MAzhar (MMT)
*: Date          : 09/28/2008
*! Purpose   : Fill Related fields array
*!*************************************************************
*N000120,1
FUNCTION lfFillRelt

*-- Array to Hold headers of releated fields
*B605226,1 RAE [START]
DIMENSION laRltFdInf[45,3]
*DIMENSION laRltFdInf[46,3]
*B605226,1 RAE [END]
laRltFdInf[1,1]="CADJACCT"
laRltFdInf[1,2]="Adj. Account"
laRltFdInf[1,3]=24
laRltFdInf[2,1]="CLRLNAME"
laRltFdInf[2,2]="Long Name"
laRltFdInf[2,3]=30
laRltFdInf[3,1]="CNRFCODE"
laRltFdInf[3,2]="NRF Code"
laRltFdInf[3,3]=5
laRltFdInf[4,1]="ALLOW_TYPE"
laRltFdInf[4,2]="Allowance Type"
laRltFdInf[4,3]=1
laRltFdInf[5,1]="CBNKCODE"
laRltFdInf[5,2]="Bank Code"
laRltFdInf[5,3]=8
laRltFdInf[6,1]="CCHKACCT"
laRltFdInf[6,2]="Bank Checking Acc."
laRltFdInf[6,3]=12
laRltFdInf[7,1]="DISCPCNT"
laRltFdInf[7,2]="Discount Type"
laRltFdInf[7,3]=6
laRltFdInf[8,1]="START"
laRltFdInf[8,2]="Start Date"
laRltFdInf[8,3]=8
laRltFdInf[9,1]="DENDATE"
laRltFdInf[9,2]="End Date"
laRltFdInf[9,3]=8
laRltFdInf[10,1]="DIVLNAME"
laRltFdInf[10,2]="Division Long Name"
laRltFdInf[10,3]=30
laRltFdInf[11,1]="LINK_CODE"
laRltFdInf[11,2]="GL Link Code"
laRltFdInf[11,3]=6
laRltFdInf[12,1]="CSLSGLLINK"
laRltFdInf[12,2]="GL Sales Link Code"
laRltFdInf[12,3]=3
laRltFdInf[13,1]="DIVGROUP"
laRltFdInf[13,2]="Division Group"
laRltFdInf[13,3]=3
laRltFdInf[14,1]="CUPCMAN"
laRltFdInf[14,2]="U.C.C. Manufacture ID"
laRltFdInf[14,3]=6
laRltFdInf[15,1]="CUPCGENTYP"
laRltFdInf[15,2]="Eropune UPC"
laRltFdInf[15,3]=1
laRltFdInf[16,1]="GLACCOUNT"
laRltFdInf[16,2]="GL Account"
laRltFdInf[16,3]=24
laRltFdInf[17,1]="COPERSEQ"
laRltFdInf[17,2]="Operation Seq."
laRltFdInf[17,3]=2
laRltFdInf[18,1]="LINHOUSE"
laRltFdInf[18,2]="In House (Y/N)"
laRltFdInf[18,3]=1
laRltFdInf[19,1]="CCONTCODE"
laRltFdInf[19,2]="Contractore/Department"
laRltFdInf[19,3]=8
laRltFdInf[20,1]="CCONTNAME"
laRltFdInf[20,2]="Contractore Name"
laRltFdInf[20,3]=30
laRltFdInf[21,1]="LMFGOPR"
laRltFdInf[21,2]="Consider As Operation(Y/N)"
laRltFdInf[21,3]=1
laRltFdInf[22,1]="LEADTIME"
laRltFdInf[22,2]="Lead Time"
laRltFdInf[22,3]=3
laRltFdInf[23,1]="CFRGTACNT"
laRltFdInf[23,2]="GL Freight Account"
laRltFdInf[23,3]=24
laRltFdInf[24,1]="CTAXCODE"
laRltFdInf[24,2]="Tax Code"
laRltFdInf[24,3]=6
laRltFdInf[25,1]="CARGLACC"
laRltFdInf[25,2]="AR/Non AR Account"
laRltFdInf[25,3]=24
laRltFdInf[26,1]="NTERDUED"
laRltFdInf[26,2]="Net Due Days"
laRltFdInf[26,3]=3
laRltFdInf[27,1]="NTERDISCD"
laRltFdInf[27,2]="Discount Days"
laRltFdInf[27,3]=3
laRltFdInf[28,1]="NTERDISCR"
laRltFdInf[28,2]="Discount Percent"
laRltFdInf[28,3]=6
laRltFdInf[29,1]="EOM"
laRltFdInf[29,2]="E.O.M (Y/N)"
laRltFdInf[29,3]=1
laRltFdInf[30,1]="EOMDAY"
laRltFdInf[30,2]="End Of Month Day"
laRltFdInf[30,3]=2
laRltFdInf[31,1]="CODYN"
laRltFdInf[31,2]="C.O.D (Y/N)"
laRltFdInf[31,3]=1
laRltFdInf[32,1]="LINSTALLM"
laRltFdInf[32,2]="Use Installments (Y/N)"
laRltFdInf[32,3]=1
laRltFdInf[33,1]="LLCASH"
laRltFdInf[33,2]="Cash Payment (Y/N)"
laRltFdInf[33,3]=1
laRltFdInf[34,1]="NRYLRATE"
laRltFdInf[34,2]="Royalty Rate"
laRltFdInf[34,3]=6
laRltFdInf[35,1]="CARRIERCOD"
laRltFdInf[35,2]="Carrier Code"
laRltFdInf[35,3]=4
laRltFdInf[36,1]="CUPC"
laRltFdInf[36,2]="UPC Type"
laRltFdInf[36,3]=13
laRltFdInf[37,1]="NCODCHARGE"
laRltFdInf[37,2]="COD Charge"
laRltFdInf[37,3]=5
laRltFdInf[38,1]="NFXDPRCNT"
laRltFdInf[38,2]="Merchandise Charge"
laRltFdInf[38,3]=5
laRltFdInf[39,1]="NINSCHARGE"
laRltFdInf[39,2]="Insurance Charge/100$"
laRltFdInf[39,3]=5
laRltFdInf[40,1]="NTAXRATE"
laRltFdInf[40,2]="Tax Rate"
laRltFdInf[40,3]=6
laRltFdInf[41,1]="CTAXRULE"
laRltFdInf[41,2]="Tax Rule"
laRltFdInf[41,3]=2
laRltFdInf[42,1]="CGLINPACCT"
laRltFdInf[42,2]="GL Input Account"
laRltFdInf[42,3]=24
laRltFdInf[43,1]="CGLOUTACCT"
laRltFdInf[43,2]="GL Output Account"
laRltFdInf[43,3]=24
laRltFdInf[44,1]="C1099CODE"
laRltFdInf[44,2]="1099 Code(Rent,Royalties)"
laRltFdInf[44,3]=2

*B605226,1 RAE [START]
laRltFdInf[45,1]="LLOBSOLETE"
laRltFdInf[45,2]="Obsolete"
laRltFdInf[45,3]=10
*B605226,1 RAE [END]
*-- end of lfFillRelt.
*!*************************************************************
*! Name      : lfCurrFld
*! Developer : Hend Ghanem
*! Date      : 05/25/2000
*! Purpose   : Control printing headers of codes and related
*!           : fields in the .frx
*!*************************************************************
*! Called From :  .FRX detail band
*!*************************************************************

FUNCTION lfCurrFld
lcCurrFld = &lcPrintHdr..cFld_name
llnewPage= .F.
RETURN ""

*-- end of lfCurrFld
*!*************************************************************
*! Name      : lfDummy
*: Developer     : Mariam MAzhar (MMT)
*: Date          : 09/28/2008
*! Purpose   : Control printing headers if it new page
*!           : in the .frx
*!*************************************************************
*! Called From :  .FRX detail band
*!*************************************************************

FUNCTION lfDummy
llnewPage= .T.
*-- end if lfDummy
*!*************************************************************
*! Name      : lfClearRep
*: Developer     : Mariam MAzhar (MMT)
*: Date          : 09/28/2008
*! Purpose   : Clear report data before exit
*!*************************************************************

FUNCTION lfClearRep
llOGFltCh = .T.
IF USED("CODES")
  USE IN Codes
ENDIF
