*!**************************************************************************
*! Name      : GLFMAIN.PRG
*! Developer : Mariam Mazhar
*! Date      : 05/25/2017
*! Purpose   : Custom triggers for GLF10 {P20170503.0001} (C202027.exe)
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Modifications: 
*! C202040,1 MMT 07/03/2017 Change custom import PO lines to add royalty codes instead of divison codes[P20170503.0001]
*! B611356,1 MMT 07/11/2017 Custom Import PO lines for GLF10 imports blank colors[P20170503.0001- Issue#1]
*! B611476,1 MMT 11/16/2017 Custom PO import program lines price is changed after import[T20171103.0003]
*!**************************************************************************
PARAMETER loFormSet,lcEvntFun,lcFunPars
lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLT(lcEvntFun)+'('+lcFunPars+')'

*--Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue

*!**************************************************************************
*! Name       : lfADDIMPOP
*! Developer  : Mariam Mazhar
*! Date       : 05/25/2017
*! Purpose    : Add Import PO lines option to PO screen
*!**************************************************************************
FUNCTION lfADDIMPOP
lcHostFormName = '[' + loFormSet.cHostFormName + ']'
If loFormSet.ariaform1.mainWorkOrder.cWorkOrdTyp  = 'PP'
  Local lnCntBar
  lnCntBar = Cntbar('_OPTIONPOP')+1
  Define Bar lnCntBar Of _OPTIONPOP Prompt 'Import Lines From Excel'  Skip For  ;
    gfFormIsActive(&lcHostFormName) And ;
    ((Type('_screen.ActiveForm.PgfPOStyle') = "O" AND (EMPTY(_screen.ActiveForm.cntPoHeader.kbVendor.keytextbox.Value) OR EMPTY(_screen.ActiveForm.cntPoHeader.kbWareHouse.keytextbox.Value)  ) ) .Or. (_Screen.ActiveForm.Parent.ActiveMode $ 'SV'))
  On Selection Bar lnCntBar  Of _OPTIONPOP Do lfOpnImport 
ENDIF
*!**************************************************************************
*! Name       : lfOpnImport
*! Developer  : Mariam Mazhar
*! Date       : 05/25/2017
*! Purpose    : Open Import Screen
*!**************************************************************************
FUNCTION lfOpnImport
loFormSet = _Screen.ActiveForm.parent 
=gfCallForm('POIMPRT','PO')
loFormSet.Activate()

*!**************************************************************************
*! Name       : lfvGetFile
*! Developer  : Mariam Mazhar
*! Date       : 05/25/2017
*! Purpose    : Get File Path
*!**************************************************************************
FUNCTION lfvGetFile
PRIVATE lcOld_Path
lcOld_Path = FULLPATH('')
lcPathName  = GETFILE('XLS', 'Excel sheet Path : ','Select') 
SET DEFAULT TO &lcOld_Path

IF EMPTY(lcPathName)
  RETURN ''  	
ELSE
  RETURN lcPathName 
ENDIF

*!**************************************************************************
*! Name       : lfvProceed
*! Developer  : Mariam Mazhar
*! Date       : 05/25/2017
*! Purpose    : Proceed importing file
*!**************************************************************************
FUNCTION lfvProceed
LPARAMETERS lcSelectedFile,loFormSetObj

IF LEN(ALLTRIM(lcSelectedFile)) = 1
  lcSelectedFile = ALLTRIM(lcSelectedFile)
ENDIF
IF EMPTY(lcSelectedFile)
  *Path name can not be empty.
  =gfModalGen('TRM04074B00000','DIALOG','Path name')
  RETURN
ENDIF

*-- Check if File Is .XLS file.
IF !(".XLS" $ lcSelectedFile)
    = gfModalGen('INM00389B00000','F','ALERT')
    RETURN
ENDIF

IF !FILE(lcSelectedFile)   
  * "File does not exist. Cannot proceed."
  =gfModalGen('TRM00273B00000','DIALOG')
  RETURN
ENDIF

lcXls = 'SHEET'
If !Used(lcXLS)
  Select 0
  WAIT WINDOW NOWAIT 'Opening Excel file....'                               
  lcErrStr = ON('ERROR')
  llImpErr = .F.
  ON ERROR llImpErr = .T.
  lcDefDir = FULLPATH('')               && Save current default dir
  SET DEFAULT TO (oAriaApplication.WorkDir)

  lcPathNameNew = ADDBS(oAriaApplication.WorkDir) + JUSTFNAME(lcSelectedFile)
  COPY FILE  (lcSelectedFile) TO (lcPathNameNew)
  lcSelectedFile = lcPathNameNew
  
  IMPORT FROM (lcSelectedFile) TYPE XLS
  lcXLS = DBF()
  lcAlias = ALIAS()  
  Use (lcXLS) In 0 Again ALIAS 'SHEET'
  Select (lcALIAS)
  Use 
  lcXLS = 'SHEET'
  SELECT (lcXLS)
  SET DEFAULT TO &lcDefDir
  ON ERROR &lcErrStr
  WAIT CLEAR
  IF llImpErr
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Invalid Excel file format!')      
    IF USED(lcXls)
  	  USE IN (lcXls)
  	ENDIF
   	ERASE (oAriaApplication.WorkDir+lcXls+'.DBF')
    IF USED('TMPSTR')
      USE IN 'TMPSTR'
    ENDIF
    loFormSetObj.Release()
    RETURN
  ENDIF
ELSE
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'There is another file with the same name of '+;
                                           'the selected Excel file , Please rename.')
  IF USED(lcXls)
    USE IN (lcXls)
  ENDIF
  IF USED('TMPSTR')
    USE IN 'TMPSTR'
  ENDIF

  ERASE (oAriaApplication.WorkDir+lcXls+'.DBF')
  loFormSetObj.Release()
  RETURN
ENDIF
SELECT (lcXLS) 
GO TOP 
DELETE && Delete the first row which hold the Fields headings
DELETE ALL FOR EMPTY(A) && Delete all empty records

*************************** 1. Vendor validation step ***************************************************************
Select DISTINCT UPPER(ALLTRIM(A)) FROM (lcXLS) WHERE  !DELETED() INTO ARRAY laVendArr


IF _TALLY = 0
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'There is no valid lines in the selected Excel file. Cannot proceed.')
  IF USED(lcXls)
    USE IN (lcXls)
  ENDIF
  IF USED('TMPSTR')
    USE IN 'TMPSTR'
  ENDIF

  ERASE (oAriaApplication.WorkDir+lcXls+'.DBF')
  loFormSetObj.Release()
  RETURN 
ENDIF

IF _TALLY > 1
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'There is more than one vendor in the selected Excel file. Cannot proceed.')
  IF USED(lcXls)
    USE IN (lcXls)
  ENDIF
  IF USED('TMPSTR')
    USE IN 'TMPSTR'
  ENDIF

  ERASE (oAriaApplication.WorkDir+lcXls+'.DBF')
  loFormSetObj.Release()
  RETURN 
ENDIF
IF !USED('Vendors_Fil') 
  =gfOpenTable('APVENDOR','VENCODE','SH','Vendors_Fil')
ENDIF
lcFileVendor = ''
IF !gfSeek(PADR(UPPER(ALLTRIM(laVendArr[1])),8),'Vendors_Fil','VENCODE') AND !gfSeek(ALLTRIM(UPPER(laVendArr[1])),'Vendors_Fil','VENCOMP')
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'The vendor in the selected Excel file is not found in Vendors file. Cannot proceed.')
  IF USED(lcXls)
    USE IN (lcXls)
  ENDIF
  IF USED('TMPSTR')
    USE IN 'TMPSTR'
  ENDIF

  ERASE (oAriaApplication.WorkDir+lcXls+'.DBF')
  loFormSetObj.Release()
  RETURN
ELSE
  lcFileVendor = Vendors_Fil.CVENDCODE 
ENDIF

IF ALLTRIM(UPPER(loFormSet.AriaForm1.cntPoHeader.kbVendor.keytextbox.Value)) <> ALLTRIM(UPPER(lcFileVendor))
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'The vendor in the selected Excel file does not match the selected vendor. Cannot proceed.')
  IF USED(lcXls)
    USE IN (lcXls)
  ENDIF
  IF USED('TMPSTR')
    USE IN 'TMPSTR'
  ENDIF

  ERASE (oAriaApplication.WorkDir+lcXls+'.DBF')
  loFormSetObj.Release()
  RETURN 
ENDIF
*! C202040,1 MMT 07/03/2017 Change custom import PO lines to add royalty codes instead of divison codes[Start]
*!*  Select DISTINCT UPPER(ALLTRIM(B)) FROM (lcXLS) WHERE  !DELETED() INTO ARRAY laDivArr
*!*  IF _TALLY = 0
*!*    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'There is no valid lines in the selected Excel file. Cannot proceed.')
*!*    IF USED(lcXls)
*!*      USE IN (lcXls)
*!*    ENDIF
*!*    IF USED('TMPSTR')
*!*      USE IN 'TMPSTR'
*!*    ENDIF

*!*    ERASE (oAriaApplication.WorkDir+lcXls+'.DBF')
*!*    loFormSetObj.Release()
*!*    RETURN 
*!*  ENDIF

*!*  IF _TALLY > 1
*!*    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'There is more than one division in the selected Excel file. Cannot proceed.')
*!*    IF USED(lcXls)
*!*      USE IN (lcXls)
*!*    ENDIF
*!*    IF USED('TMPSTR')
*!*      USE IN 'TMPSTR'
*!*    ENDIF

*!*    ERASE (oAriaApplication.WorkDir+lcXls+'.DBF')
*!*    loFormSetObj.Release()
*!*    RETURN 
*!*  ENDIF
*! C202040,1 MMT 07/03/2017 Change custom import PO lines to add royalty codes instead of divison codes[End]
*************************** 2. Scale and sizes validation step ***************************************************************
IF !USED('Scale_Fil') 
  =gfOpenTable('SCALE','SCALE','SH','Scale_Fil')
ENDIF

IF !USED('STYLE_Fil') 
  =gfOpenTable('STYLE','STYLE','SH','STYLE_Fil')
ENDIF

IF !USED('STYLEUPC_Fil') 
  =gfOpenTable('STYLEUPC','STYUPCN','SH','STYLEUPC_Fil')
ENDIF


SELECT (lcXLS) 
SELECT DISTINCT M,N FROM (lcXLS) WHERE !DELETED() INTO CURSOR 'SizeScale'
DIMENSION laCurField[1,4]
laCurField[1,1] = 'mStrRep'
laCurField[1,2] = 'M'
laCurField[1,3] = 10
laCurField[1,4] = 0
=gfCrtTmp('TMPSTR',@laCurField,.F.)

SELECT 'SizeScale'
GO TOP
SCAN
  IF !gfSeek('S'+ALLTRIM(SizeScale.N),'Scale_Fil','Scale')
    SELECT 'TMPSTR'
    LOCATE 
    IF EOF()
      APPEND BLANK
    ENDIF
    Replace mStrRep WITH mStrRep+ "Scale: "+ALLTRIM(SizeScale.N)+" not found"+CHR(13)+CHR(10) IN 'TMPSTR'
  ELSE
    lnSizeCnt = 0
    llSizeFound = .F.
    SELECT Scale_Fil
    SCAN REST WHILE TYPE+SCALE+PREPAK = 'S'+ALLTRIM(SizeScale.N)
      FOR lnC = 1 TO Scale_Fil.cnt
        lcC=STR(lnC,1)
        IF ALLTRIM(UPPER(Scale_Fil.Sz&lcC.))== ALLTRIM(UPPER(SizeScale.M))
          lnSizeCnt  = lnC 
          llSizeFound = .T.
          EXIT 
        ENDIF
      ENDFOR
      IF llSizeFound 
        EXIT
      ENDIF
    ENDSCAN  
    IF !llSizeFound 
      SELECT 'TMPSTR'
      LOCATE 
      IF EOF()
        APPEND BLANK
      ENDIF
      REPLACE mStrRep WITH mStrRep+ "Size: "+ALLTRIM(SizeScale.M)+" not found in Scale: "+ALLTRIM(SizeScale.N)+CHR(13)+CHR(10) IN 'TMPSTR'
    ELSE
      
    ENDIF
  ENDIF
ENDSCAN
***Check UPCs
SELECT distinct &lcXLS..F,&lcXLS..O FROM (lcXLS) WHERE !EMPTY(&lcXLS..O) INTO CURSOR 'STYVUPC'
SELECT 'STYVUPC'
LOCATE 
IF !EOF()
  SCAN
    lcUpc =O
    lcVenSty = SUBSTR(F,1,19)
    IF gfSeek(ALLTRIM(lcUpc),'STYLEUPC_Fil','STYUPCN')
      IF gfSeek(STYLEUPC_Fil.STYLE,'STYLE_Fil','STYLE') 
        IF STYLE_Fil.cVenSty <> lcVenSty 
          SELECT 'TMPSTR'
          LOCATE 
          IF EOF()
            APPEND BLANK
          ENDIF
          Replace mStrRep WITH mStrRep+ "UPC: "+ALLTRIM(lcUpc)+" is assigned to Vendor Style#:"+ALLTRIM(STYLE_Fil.cVenSty)+CHR(13)+CHR(10) IN 'TMPSTR'
        ENDIF
      ENDIF
    ENDIF
  ENDSCAN
ENDIF
****

DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)
lnNonMajSt = 0
*-- Loop Around Non Major elements.
STORE '' TO lcNonMajPi ,lcNonMajPos
FOR lnI =  1 TO ALEN(laMajSegs,1)
  IF laMajSegs[lnI,1] = 'C'
    lnNonMajSt = laMajSegs[lnI,4]      && This item hold seg. start position.
    lcNonMajPi = IIF(EMPTY(lcNonMajPi) .OR. laMajSegs[lnI,1]='C',;
                 laMajSegs[lnI,3],;
                 lcNonMajPi + laMajSegs[lnI-1,6] + laMajSegs[lnI,3])
    lcNonMajPos = laMajSegs[lnI,4]                 
    EXIT
  ENDIF
ENDFOR

STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
CREATE CURSOR 'POLines' (Style C(19), cDivision C(6), cStyGroup C(6),Qty1 N(7),Qty2 N(7),;
                        Qty3 N(7),Qty4 N(7),Qty5 N(7),Qty6 N(7),Qty7 N(7),Qty8 N(7),TotQty N(8),nfCost1 N(13,2),lImported L)
SELECT 'POLines'                         
INDEX ON  Style TAG 'POLines'

CREATE CURSOR 'ImpStyle' (Vendor C(8),cVenSty C(19),Color C(6),cClrDesc C(60),Season C(6), cDivision C(6),;
                          cStyGroup C(6) ,nsugRetPri N(13,2),nfCost1 N(13,2),Desc C(30),MatComps C(100), ;
                          CounMdIn C(2),MIDCode C(30),HTSCode C(30),Scale C(3),Size C(5), UPC C(13),CITES L,;
                          FUR L,WILDFARM C(1), CountOrg C(15),ScentNam C(30),Qty N(7),Style C(19),cSizeNo C(1),lNoVenSty L,royalty C(6)) 
SELECT 'ImpStyle' 
INDEX ON cVenSty+COLOR+SCALE+SIZE TAG 'ImpStyle' 
*! C202040,1 MMT 07/03/2017 Change custom import PO lines to add royalty codes instead of divison codes[Start]
IF !USED('Codes_Fil')
  =gfOpenTable("Codes",'cCode_No','SH','Codes_Fil')
ENDIF
*! C202040,1 MMT 07/03/2017 Change custom import PO lines to add royalty codes instead of divison codes[End]
SELECT (lcXLS)
LOCATE
SCAN
  lcSizeCnt = 0
  llSizeFound = .F.
  SELECT Scale_Fil
  =gfSeek('S'+ALLTRIM(&lcXLS..N),'Scale_Fil','Scale')
  SCAN REST WHILE TYPE+SCALE+PREPAK = 'S' + ALLTRIM(&lcXLS..N)
    FOR lnC = 1 TO Scale_Fil.cnt
      lcC=STR(lnC,1)
      IF ALLTRIM(UPPER(Scale_Fil.Sz&lcC.))== UPPER(ALLTRIM(&lcXLS..M))
        lcSizeCnt = lcC
        llSizeFound = .T.
        EXIT 
      ENDIF
    ENDFOR
    IF llSizeFound 
      EXIT
    ENDIF
  ENDSCAN  
  SELECT STYLE_Fil
  *B611356,1 MMT 07/11/2017 Custom Import PO lines for GLF10 imports blank colors[P20170503.0001- Issue#1][Start]
  *LOCATE FOR cvensty = SUBSTR(ALLTRIM(&lcXLS..F),1,19) AND SUBSTR(Style ,lnNonMajSt,lnColorLen) = PADR(&lcXLS..G,lnColorLen) AND IIF(llSizeFound ,Scale = Scale_Fil.Scale,.T.)  
  LOCATE FOR cvensty = SUBSTR(ALLTRIM(&lcXLS..F),1,19) AND SUBSTR(Style ,lnNonMajSt,lnColorLen) = PADR(ALLTRIM(&lcXLS..G),lnColorLen) AND IIF(llSizeFound ,Scale = Scale_Fil.Scale,.T.)
  *B611356,1 MMT 07/11/2017 Custom Import PO lines for GLF10 imports blank colors[P20170503.0001- Issue#1][End]
  IF FOUND()
    llInValid = .F.
    *! C202040,1 MMT 07/03/2017 Change custom import PO lines to add royalty codes instead of divison codes[Start]
    *IF STYLE_Fil.cDivision <> SUBSTR(&lcXLS..B,1,6)
    lcRoyCode = SPACE(6)
    SELECT 'Codes_Fil'
    =gfSetOrder('cCode_No')
    IF  gfSEEK("N"+'ROYALTY   ','Codes_Fil','cCode_No')
      LOCATE REST WHILE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM = "N"+'ROYALTY   ' FOR UPPER(ALLTRIM(CDISCREP)) = UPPER(ALLTRIM(&lcXLS..B))
      IF FOUND()
        lcRoyCode = Codes_Fil.CCODE_NO
      ENDIF
    ENDIF
    IF !EMPTY(ALLTRIM(&lcXLS..B)) AND STYLE_Fil.royalty <> lcRoyCode
    *! C202040,1 MMT 07/03/2017 Change custom import PO lines to add royalty codes instead of divison codes[End]
      SELECT 'TMPSTR'
      LOCATE 
      IF EOF()
        APPEND BLANK
      ENDIF
      *! C202040,1 MMT 07/03/2017 Change custom import PO lines to add royalty codes instead of divison codes[Start]
      *REPLACE mStrRep WITH mStrRep+ "Style : "+SUBSTR(&lcXLS..F,1,19)+" has Division mismatch"+CHR(13)+CHR(10) IN 'TMPSTR'
      REPLACE mStrRep WITH mStrRep+ "Style : "+SUBSTR(&lcXLS..F,1,19)+" has Royalty mismatch"+CHR(13)+CHR(10) IN 'TMPSTR'
      *! C202040,1 MMT 07/03/2017 Change custom import PO lines to add royalty codes instead of divison codes[End]
      llInValid = .T.
    ENDIF

    IF STYLE_Fil.Season <> SUBSTR(ALLTRIM(&lcXLS..C),1,6)
      SELECT 'TMPSTR'
      LOCATE 
      IF EOF()
        APPEND BLANK
      ENDIF
      REPLACE mStrRep WITH mStrRep+ "Style : "+SUBSTR(&lcXLS..F,1,19)+" has Season mismatch"+CHR(13)+CHR(10) IN 'TMPSTR'
      llInValid = .T.
    ENDIF

    IF STYLE_Fil.cStyGroup <> SUBSTR(ALLTRIM(&lcXLS..D),1,6)
      SELECT 'TMPSTR'
      LOCATE 
      IF EOF()
        APPEND BLANK
      ENDIF
      REPLACE mStrRep WITH mStrRep+ "Style : "+SUBSTR(&lcXLS..F,1,19)+" has Style group mismatch"+CHR(13)+CHR(10) IN 'TMPSTR'
      llInValid = .T.
    ENDIF
    
    IF SUBSTR(STYLE_Fil.Scale,1,LEN(ALLTRIM(&lcXLS..N))) <> ALLTRIM(&lcXLS..N)      
      SELECT 'TMPSTR'
      LOCATE 
      IF EOF()
        APPEND BLANK
      ENDIF
      REPLACE mStrRep WITH mStrRep+ "Style : "+SUBSTR(&lcXLS..F,1,19)+" has Scale mismatch"+CHR(13)+CHR(10) IN 'TMPSTR'
      llInValid = .T.
    ENDIF

    IF SUBSTR(STYLE_Fil.Style ,lnNonMajSt,lnColorLen) <> PADR(ALLTRIM(&lcXLS..G),lnColorLen)
      SELECT 'TMPSTR'
      LOCATE 
      IF EOF()
        APPEND BLANK
      ENDIF
      REPLACE mStrRep WITH mStrRep+ "Style : "+SUBSTR(&lcXLS..F,1,19)+" has Color mismatch"+CHR(13)+CHR(10) IN 'TMPSTR'
      llInValid = .T.
    ENDIF 

    IF !EMPTY(&lcXLS..O) AND llSizeFound 
	    IF gfSeek(ALLTRIM(&lcXLS..O),'STYLEUPC_Fil','STYUPCN')
	      IF STYLEUPC_Fil.STYLE = STYLE_FIL.STYLE 
	        IF lcSizeCnt <> ALLTRIM(STYLEUPC_Fil.Size)
	          SELECT 'TMPSTR'
	          LOCATE 
	          IF EOF()
	            APPEND BLANK
	          ENDIF
	          REPLACE mStrRep WITH mStrRep+  "Style : "+SUBSTR(&lcXLS..F,1,19)+" has UPC mismatch"+CHR(13)+CHR(10) IN 'TMPSTR'
	          llInValid = .T.
	        ENDIF
	      ELSE
	        SELECT 'TMPSTR'
	        LOCATE 
	        IF EOF()
	          APPEND BLANK
	        ENDIF
	        REPLACE mStrRep WITH mStrRep+  "Style : "+SUBSTR(&lcXLS..F,1,19)+" has UPC mismatch"+CHR(13)+CHR(10) IN 'TMPSTR'
	        llInValid = .T.
	      ENDIF
	      *FF
*!*		    ELSE
*!*		      SELECT 'TMPSTR'
*!*		      LOCATE 
*!*		      IF EOF()
*!*		        APPEND BLANK
*!*		      ENDIF
*!*		      REPLACE mStrRep WITH mStrRep+  "Style : "+SUBSTR(&lcXLS..F,1,19)+", UPC:"+ALLTRIM(&lcXLS..O)+" is not assigned to Style/Size"+CHR(13)+CHR(10) IN 'TMPSTR'
*!*		      llInValid = .T.
*FF
	    ENDIF
	  ENDIF

      
    IF !llInValid
      *** Updating UDFs [Start]
      lcStyleID = STYLE_FIL.STYLE
      lcStyleColr = SUBSTR(STYLE_FIL.STYLE,1,lnNonMajSt+lnColorLen-1)
      IF gfSeek(lcStyleColr,'STYLE_FIL','STYLE')
        *! C202040,1 MMT 07/03/2017 Change custom import PO lines to add royalty codes instead of divison codes[Start]
        IF loFormSet.AriaForm1.pgfPOStyle.page1.cntheaderFolder.cboDivision.Value <> STYLE_FIL.cDivision
          REPLACE cDivision WITH loFormSet.AriaForm1.pgfPOStyle.page1.cntheaderFolder.cboDivision.Value IN STYLE_FIL REST WHILE STYLE = lcStyleColr
        ENDIF
        =gfSeek(lcStyleColr,'STYLE_FIL','STYLE')
        *! C202040,1 MMT 07/03/2017 Change custom import PO lines to add royalty codes instead of divison codes[End]
        IF STYLE_FIL.GLFCITES <> IIF(UPPER(&lcXLS..P)='YES',.T.,.F.)
          REPLACE GLFCITES WITH IIF(UPPER(&lcXLS..P)='YES',.T.,.F.) IN STYLE_FIL REST WHILE STYLE = lcStyleColr
        ENDIF
        =gfSeek(lcStyleColr,'STYLE_FIL','STYLE')
        IF STYLE_FIL.GLFCOUNTRY <> &lcXLS..V AND !EMPTY(ALLTRIM(&lcXLS..V))  
          REPLACE GLFCOUNTRY WITH &lcXLS..V IN STYLE_FIL REST WHILE STYLE = lcStyleColr
        ENDIF
        =gfSeek(lcStyleColr,'STYLE_FIL','STYLE')
        IF STYLE_FIL.GLFMID    <> &lcXLS..K AND !EMPTY(ALLTRIM(&lcXLS..K))  
          REPLACE GLFMID    WITH &lcXLS..K IN STYLE_FIL REST WHILE STYLE = lcStyleColr
        ENDIF
        =gfSeek(lcStyleColr,'STYLE_FIL','STYLE')
        IF STYLE_FIL.HTSCODE <> &lcXLS..L AND !EMPTY(ALLTRIM(&lcXLS..L))  
          REPLACE HTSCODE WITH &lcXLS..L IN STYLE_FIL REST WHILE STYLE = lcStyleColr
        ENDIF
        =gfSeek(lcStyleColr,'STYLE_FIL','STYLE')
        IF STYLE_FIL.FUR <> IIF(UPPER(&lcXLS..Q)='YES',.T.,.F.)
          REPLACE FUR WITH IIF(UPPER(&lcXLS..Q)='YES',.T.,.F.) IN STYLE_FIL REST WHILE STYLE = lcStyleColr
        ENDIF
        =gfSeek(lcStyleColr,'STYLE_FIL','STYLE')
        IF STYLE_FIL.CTRYMADE <> &lcXLS..J AND !EMPTY(ALLTRIM(&lcXLS..J))  
          REPLACE CTRYMADE WITH &lcXLS..J IN STYLE_FIL REST WHILE STYLE = lcStyleColr
        ENDIF
        =gfSeek(lcStyleColr,'STYLE_FIL','STYLE')
        IF STYLE_FIL.MatComps <> &lcXLS..I  AND !EMPTY(ALLTRIM(&lcXLS..I))  
          REPLACE MatComps WITH &lcXLS..I IN STYLE_FIL REST WHILE STYLE = lcStyleColr
        ENDIF
        =gfSeek(lcStyleColr,'STYLE_FIL','STYLE')
        IF STYLE_FIL.SCNTNAME <> &lcXLS..W AND !EMPTY(ALLTRIM(&lcXLS..W))  
          REPLACE SCNTNAME  WITH &lcXLS..W IN STYLE_FIL REST WHILE STYLE = lcStyleColr
        ENDIF
        =gfSeek(lcStyleColr,'STYLE_FIL','STYLE')
        IF STYLE_FIL.WILDFARM <> &lcXLS..U AND !EMPTY(ALLTRIM(&lcXLS..U))
          REPLACE WILDFARM WITH &lcXLS..U IN STYLE_FIL REST WHILE STYLE = lcStyleColr
        ENDIF
        SELECT STYLE_FIL
        =gfReplace('')
        =gfTableUpdate()
      ENDIF  
      =gfSeek(lcStyleID,'STYLE_FIL','STYLE')
      *** {End}
      IF !SEEK(STYLE_Fil.Style,'POLines')
        SELECT 'POLines'
        APPEND BLANK 
        REPLACE Style           WITH STYLE_Fil.Style,;
                cDivision       WITH STYLE_Fil.cDivision ,;
                cStyGroup       WITH STYLE_Fil.cStyGroup ,;
                Qty&lcSizeCnt.  WITH VAL(&lcXLS..T),;
                nfCost1         WITH VAL(&lcXLS..R),;
                TotQty          WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
      ELSE
        SELECT 'POLines'
        LOCATE REST WHILE Style =STYLE_Fil.Style FOR nfCost1 = VAL(&lcXLS..R)
        IF FOUND()
          REPLACE Qty&lcSizeCnt.  WITH Qty&lcSizeCnt. + VAL(&lcXLS..T),;           
                  TotQty          WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8       
        ELSE
          SELECT 'POLines'
          APPEND BLANK 
          REPLACE Style           WITH STYLE_Fil.Style,;
                  cDivision       WITH STYLE_Fil.cDivision ,;
                  cStyGroup       WITH STYLE_Fil.cStyGroup ,;
                  Qty&lcSizeCnt.  WITH VAL(&lcXLS..T),;
                  nfCost1         WITH VAL(&lcXLS..R),;
                  TotQty          WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
        ENDIF          
      ENDIF
    ENDIF 
  ELSE
    IF llSizeFound
      *B611356,1 MMT 07/11/2017 Custom Import PO lines for GLF10 imports blank colors[P20170503.0001- Issue#1][Start]
      *IF !SEEK(&lcXLS..F+&lcXLS..G+Scale_Fil.Scale+&lcXLS..M,'ImpStyle') 
      IF !SEEK(&lcXLS..F+PADR(ALLTRIM(&lcXLS..G),6)+Scale_Fil.Scale+&lcXLS..M,'ImpStyle') 
      *B611356,1 MMT 07/11/2017 Custom Import PO lines for GLF10 imports blank colors[P20170503.0001- Issue#1][End]
        SELECT STYLE_Fil
        llNoVenSty = .F.
        *B611356,1 MMT 07/11/2017 Custom Import PO lines for GLF10 imports blank colors[P20170503.0001- Issue#1][Start]
        *LOCATE FOR cvensty = SUBSTR(&lcXLS..F,1,19) AND SUBSTR(Style ,lnNonMajSt,lnColorLen) <> PADR(&lcXLS..G,lnColorLen) AND IIF(llSizeFound ,Scale = Scale_Fil.Scale,.T.)        
        LOCATE FOR cvensty = SUBSTR(&lcXLS..F,1,19) AND SUBSTR(Style ,lnNonMajSt,lnColorLen) <> PADR(ALLTRIM(&lcXLS..G),lnColorLen) AND IIF(llSizeFound ,Scale = Scale_Fil.Scale,.T.)
        *B611356,1 MMT 07/11/2017 Custom Import PO lines for GLF10 imports blank colors[P20170503.0001- Issue#1][End]
        IF !FOUND()
          llNoVenSty = .T.
        ENDIF
        SELECT 'ImpStyle'
        APPEND BLANK 
        REPLACE Vendor WITH  ALLTRIM(UPPER(loFormSet.AriaForm1.cntPoHeader.kbVendor.keytextbox.Value)),;
                cVenSty WITH &lcXLS..F,;
                Color WITH &lcXLS..G,;
                cClrDesc WITH &lcXLS..H,;
                MatComps WITH &lcXLS..I,;
                Season   WITH SUBSTR(ALLTRIM(&lcXLS..C),1,6),;   
                cDivision WITH SUBSTR(ALLTRIM(&lcXLS..B),1,6),;   
                cStyGroup WITH SUBSTR(ALLTRIM(&lcXLS..D),1,6),;   
                nsugRetPri WITH VAL(&lcXLS..S),;   
                nfCost1 WITH VAL(&lcXLS..R),;   
                HTSCode  WITH &lcXLS..L,;   
                CounMdIn WITH &lcXLS..J,;   
                MIDCode  WITH &lcXLS..K,;    
                Scale    WITH Scale_Fil.Scale,;   
                Size     WITH &lcXLS..M,;   
                UPC      WITH ALLTRIM(&lcXLS..O),;   
                CITES    WITH IIF(UPPER(&lcXLS..P)='YES',.T.,.F.),;   
                FUR      WITH IIF(UPPER(&lcXLS..Q)='YES',.T.,.F.),;  
                WILDFARM WITH &lcXLS..U,;  
                CountOrg WITH &lcXLS..V,;  
                ScentNam WITH &lcXLS..W,;
                Desc     WITH &lcXLS..E,;
                Qty      WITH VAL(&lcXLS..T),;
                cSizeNo  WITH lcSizeCnt ,;
                lNoVenSty WITH llNoVenSty
         *B611356,1 MMT 07/11/2017 Custom Import PO lines for GLF10 imports blank colors[P20170503.0001- Issue#1][Start]
         REPLACE Color WITH ALLTRIM(&lcXLS..G)
         *B611356,1 MMT 07/11/2017 Custom Import PO lines for GLF10 imports blank colors[P20170503.0001- Issue#1][End]       
         *! C202040,1 MMT 07/03/2017 Change custom import PO lines to add royalty codes instead of divison codes[Start]
         REPLACE Royalty WITH SUBSTR(ALLTRIM(&lcXLS..B),1,6),;   
                 cDivision WITH loFormSet.AriaForm1.pgfPOStyle.page1.cntheaderFolder.cboDivision.Value 
         *! C202040,1 MMT 07/03/2017 Change custom import PO lines to add royalty codes instead of divison codes[End]
       ELSE
         REPLACE Qty     WITH Qty + VAL(&lcXLS..T)         
       ENDIF 
     ENDIF        
  ENDIF
ENDSCAN
SELECT 'TMPSTR'
LOCATE 
IF !EOF()
  =gfCallForm('POERRLOG','PO',"'Error log'")
  IF USED(lcXls)
    USE IN (lcXls)
  ENDIF
  IF USED('TMPSTR')
    USE IN 'TMPSTR'
  ENDIF

  ERASE (oAriaApplication.WorkDir+lcXls+'.DBF')
  loFormSetObj.Release()
  RETURN
ENDIF
************************************************ Importing******************************************
* Scan the file to get the list of Styles that will be imported in the system and the list of missing codes to be added in the log file.
IF USED('MissingData')
  USE IN 'MissingData'
ENDIF
SELECT 'ImpStyle'                          
LOCATE
IF !EOF()
  IF !USED('Codes_Fil')
    =gfOpenTable("Codes",'cCode_No','SH','Codes_Fil')
  ENDIF
  CREATE CURSOR MissingData (ColumnH C(20), ErrorDesc C(60),CurrentValue C(19),AddAuto L, AnotherV C(10),cClrDesc C(60))
  SELECT Distinct Color,cClrDesc FROM  'ImpStyle' INTO CURSOR 'Colors'
  SELECT Distinct Season FROM  'ImpStyle' INTO CURSOR 'Seasons'
  *! C202040,1 MMT 07/03/2017 Change custom import PO lines to add royalty codes instead of divison codes[Start]
  *SELECT Distinct cDivision FROM  'ImpStyle' INTO CURSOR 'Divisions'
  SELECT Distinct Royalty FROM  'ImpStyle' INTO CURSOR 'RoyaltyCur'
  *! C202040,1 MMT 07/03/2017 Change custom import PO lines to add royalty codes instead of divison codes[End]
  SELECT Distinct cStyGroup FROM  'ImpStyle' INTO CURSOR 'StyGroups'
  SELECT DISTINCT cVenSty FROM  'ImpStyle' WHERE lNoVenSty INTO CURSOR 'cStyVen'
  
  SELECT cStyVen
  LOCATE
  IF !EOF()
    SCAN
      INSERT INTO MissingData VALUES  ('Vendor Style',"Vendor Style is not found",ALLTRIM(cStyVen.cVenSty),.T., '','')
    ENDSCAN
  ENDIF
    
  SELECT Colors
  LOCATE
  SCAN
    IF !gfSEEK("N"+'COLOR     '+SUBSTR(Colors.Color,1,6),'Codes_Fil','cCode_No')
      INSERT INTO MissingData VALUES  ('Color Code',"Color is not found",SUBSTR(Colors.Color,1,6),.T., '',Colors.cClrDesc)
    ENDIF 
  ENDSCAN


  SELECT Seasons
  LOCATE
  SCAN
    IF !gfSEEK("N"+'SEASON    '+SUBSTR(Seasons.Season ,1,6),'Codes_Fil','cCode_No')
      INSERT INTO MissingData VALUES  ('Season Code',"Season is not found",SUBSTR(Seasons.Season,1,6),.T., '','')
    ENDIF 
  ENDSCAN
  *! C202040,1 MMT 07/03/2017 Change custom import PO lines to add royalty codes instead of divison codes[Start]
  *SELECT Divisions
  SELECT RoyaltyCur
  *! C202040,1 MMT 07/03/2017 Change custom import PO lines to add royalty codes instead of divison codes[End]
  LOCATE
  SCAN
    *! C202040,1 MMT 07/03/2017 Change custom import PO lines to add royalty codes instead of divison codes[Start]
*!*      IF !gfSEEK("N"+'CDIVISION '+SUBSTR(Divisions.cDivision ,1,6),'Codes_Fil','cCode_No')
*!*        INSERT INTO MissingData VALUES  ('Division',"Division is not found",SUBSTR(Divisions.cDivision ,1,6),.T., '','')
         *ENDIF 
    *IF !gfSEEK("N"+'ROYALTY   '+SUBSTR(RoyaltyCur.ROYALTY,1,6),'Codes_Fil','cCode_No')
     lcRoyCode = ''
     SELECT 'Codes_Fil'
     =gfSetOrder('cCode_No')
     IF  gfSEEK("N"+'ROYALTY   ','Codes_Fil','cCode_No')
       LOCATE REST WHILE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM = "N"+'ROYALTY   ' FOR UPPER(ALLTRIM(CDISCREP)) = UPPER(ALLTRIM(RoyaltyCur.ROYALTY))
       IF FOUND()
         lcRoyCode = Codes_Fil.CCODE_NO
       ENDIF
     ENDIF
     IF EMPTY(ALLTRIM(lcRoyCode))
       INSERT INTO MissingData VALUES  ('Brand',"Royalty is not found",SUBSTR(RoyaltyCur.ROYALTY,1,6),.T., '','')
     ENDIF  
     *! C202040,1 MMT 07/03/2017 Change custom import PO lines to add royalty codes instead of divison codes[End]  
  ENDSCAN


  SELECT StyGroups
  LOCATE
  SCAN
    IF !gfSEEK("N"+'CSTYGROUP '+SUBSTR(StyGroups.cStyGroup,1,6),'Codes_Fil','cCode_No')
      INSERT INTO MissingData VALUES  ('Category/Style Group',"Style Group is not found",SUBSTR(StyGroups.cStyGroup,1,6),.T., '','')
    ENDIF 
  ENDSCAN

  IF USED('Codes_Fil')
    gfCloseTable('Codes_Fil')
  ENDIF
  
  SELECT MissingData 
  LOCATE
  IF !EOF()
    llCancelledFix = .F.
    llProceed = .F.
    =gfCallForm('POERRFIX','PO',"'MissingData'")
    IF llCancelledFix
      lfAddPOLine('POLines',.F.,loFormSet)
      SELECT POLines
      LOCATE FOR lImported
      IF FOUND()
        =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Some Styles are imported before.Cannot be re-imported.')
      ENDIF
      IF USED(lcXls)
        USE IN (lcXls)
      ENDIF
      IF USED('TMPSTR')
        USE IN 'TMPSTR'
      ENDIF

      ERASE (oAriaApplication.WorkDir+lcXls+'.DBF')
      loFormSetObj.Release()
      RETURN
    ENDIF
  ENDIF
ENDIF



SELECT 'ImpStyle'                          
LOCATE
IF !EOF() 
  *Add missing styles to style and Stydye tables
  llStylesAdded = lfAddMissingStyle('ImpStyle',ALLTRIM(UPPER(loFormSet.AriaForm1.cntPoHeader.kbVendor.keytextbox.Value)),ALLTRIM(UPPER(loFormSet.AriaForm1.cntPoHeader.kbWareHouse.keytextbox.Value)))
  IF !llStylesAdded 
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Some Styles could not be added to the system.Cannot proceed.')
    IF USED(lcXls)
      USE IN (lcXls)
    ENDIF
    IF USED('TMPSTR')
      USE IN 'TMPSTR'
    ENDIF

    ERASE (oAriaApplication.WorkDir+lcXls+'.DBF')
    RETURN .F.
  ENDIF
  ***
ENDIF
&&AddLines to PO LINES
WAIT WINDOW NOWAIT 'Adding Lines to PO....'                               
lfAddPOLine('POLines','ImpStyle',loFormSet)
SELECT POLines
LOCATE FOR lImported
IF FOUND()
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Some Styles are imported before.Cannot be re-imported.')
ENDIF
IF USED(lcXls)
  USE IN (lcXls)
ENDIF
IF USED('TMPSTR')
  USE IN 'TMPSTR'
ENDIF
ERASE (oAriaApplication.WorkDir+lcXls+'.DBF')
*** Summary report
DIMENSION laCurField[1,4]
laCurField[1,1] = 'mStrRep'
laCurField[1,2] = 'M'
laCurField[1,3] = 10
laCurField[1,4] = 0
=gfCrtTmp('TMPSTR',@laCurField,.F.)
IF USED('MissingData') 
  SELECT MissingData 
  LOCATE FOR AddAuto AND ColumnH <> 'Vendor Style'
  IF FOUND()
    SCAN FOR AddAuto AND ColumnH <> 'Vendor Style'
      SELECT 'TMPSTR'
      LOCATE 
      IF EOF()
        APPEND BLANK
      ENDIF
      Replace mStrRep WITH mStrRep+MissingData.ColumnH+": "+ALLTRIM(MissingData.CurrentValue)+" has been added to Codes file."+CHR(13)+CHR(10) IN 'TMPSTR'
    ENDSCAN
  ENDIF
ENDIF
IF USED('ImpStyle')
  SELECT 'ImpStyle'                          
  LOCATE
  SCAN FOR !EMPTY(STYLE) 
    SELECT 'TMPSTR'
    LOCATE 
    IF EOF()
      APPEND BLANK
    ENDIF
    Replace mStrRep WITH mStrRep+ "STYLE: "+ALLTRIM(ImpStyle.STYLE)+", Vendor Style:"+ALLTRIM(ImpStyle.cVenSty)+" has been added to Style file."+CHR(13)+CHR(10) IN 'TMPSTR'
  ENDSCAN
ENDIF  
SELECT 'TMPSTR'
LOCATE 
IF !EOF()
  =gfCallForm('POERRLOG','PO',"'Items Added Report'")
ENDIF  
IF USED('TMPSTR')
  USE IN 'TMPSTR'
ENDIF
*** End Summary
loFormSetObj.Release()
RETURN
*!**************************************************************************
*! Name       : lfFixError
*! Developer  : Mariam Mazhar
*! Date       : 06/05/2017
*! Purpose    : Fix errors found
*!**************************************************************************
FUNCTION lfFixError
PARAMETERS loFixErrFormSet
SELECT (loFixErrFormSet.lccursorname)
LOCATE FOR !AddAuto AND EMPTY(AnotherV)
IF FOUND()
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,"Some columns don't have another value and Add automatically is not checked. Please fix to proceed.")      
  RETURN 
ENDIF
SELECT (loFixErrFormSet.lccursorname)
LOCATE FOR AddAuto AND EMPTY(ALLTRIM(CurrentValue))
IF FOUND()
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,"Some columns don't have current value and Add automatically is checked. Please fix to proceed.")      
  RETURN 
ENDIF

SELECT(loFixErrFormSet.lccursorname)
LOCATE
SCAN FOR !AddAuto AND !EMPTY(AnotherV)
  lcFldName = ''
  DO CASE
    CASE ColumnH = 'Category/Style Group'
      lcFldName = 'cStyGroup'
    *! C202040,1 MMT 07/03/2017 Change custom import PO lines to add royalty codes instead of divison codes[Start]  
*!*      CASE ColumnH = 'Division'
*!*        lcFldName = 'cDivision'
    CASE ColumnH = 'Brand'
      lcFldName = 'ROYALTY'
    *! C202040,1 MMT 07/03/2017 Change custom import PO lines to add royalty codes instead of divison codes[End]
    CASE ColumnH = 'Season Code'
      lcFldName = 'Season'
    CASE ColumnH = 'Color Code'
      lcFldName = 'Color'
  ENDCASE
  IF !EMPTY(lcFldName) 
    SELECT 'ImpStyle'
    REPLACE &lcFldName. WITH MissingData.AnotherV FOR &lcFldName. = SUBSTR(MissingData.CurrentValue,1,6) ALL
  ENDIF
ENDSCAN

SELECT (loFixErrFormSet.lccursorname)
LOCATE FOR AddAuto AND ColumnH <> 'Vendor Style'
IF FOUND()
  IF !lfAddMissingCode(loFixErrFormSet.lccursorname)
     =gfModalGen('INM00000B00000',.F.,.F.,.F.,"Could not open Codes Table to add the missing codes. Cannot proceed.")
     llProceed =.F.
     RETURN .F.
  ELSE
    llProceed =.T.
  ENDIF
ENDIF
loFixErrFormSet.Release ()

*!**************************************************************************
*! Name       : lfAddMissingCode
*! Developer  : Mariam Mazhar
*! Date       : 06/05/2017
*! Purpose    : Add missing Codes
*!**************************************************************************
FUNCTION lfAddMissingCode
LPARAMETERS lcMissedCodes
IF !USED('SYCCOMP_Fil')
  =gfOpenTable('SYCCOMP','CCOMP_ID','SH','SYCCOMP_Fil')
ENDIF
lcTmpCodes   = gfTempName() 
lcTmpRelFlds = gfTempName()


IF lfSQLStatement("SELECT *, ' ' AS cStatus FROM CODES WHERE (1 = 2)", lcTmpRelFlds, 'CCODE_NO+CRLTD_NAM|cStatus|', 'CCODE_NO|cStatus|', lfIsNative('CODES') = 1)
  SELECT (lcTmpRelFlds)
  SET ORDER TO CCODE_NO
  =AFIELDS(laFileStru)
  lnFlds = ALEN(laFileStru,1) + 1
  DIMENSION lafilestru[lnFlds,18]

  lafilestru[lnFlds,1] = 'llDefCode'
  lafilestru[lnFlds,2] = 'L'
  lafilestru[lnFlds,3] = 1
  lafilestru[lnFlds,4] = 0
  lafilestru[lnFlds,5] = .F.
  lafilestru[lnFlds,6] = .F.
  FOR lnI = 7 TO 16
    lafilestru[lnFlds,lnI] = ''
  ENDFOR
  lafilestru[lnFlds,17] = 0
  lafilestru[lnFlds,18] = 0

  =gfCrtTmp(lcTmpCodes, @laFileStru, "CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM", "CCODE_NO")
  SELECT (lcTmpCodes)
  INDEX ON cStatus TAG cStatus
  INDEX ON cDiscrep TAG "Discrep"
  INDEX ON cCode_No+cDiscrep TAG (lcTmpCodes)
  =CURSORSETPROP("Buffering", 5)
  SET ORDER TO CCODE_NO
  SELECT DISTINCT ColumnH FROM (lcMissedCodes) WHERE AddAuto AND ColumnH <> 'Vendor Style' INTO CURSOR 'CodesToAdd'
  llSavedCorrectly = .T.
  SELECT 'CodesToAdd'
  LOCATE 
  IF !EOF()
    SCAN
      lcFldName = ''
      DO CASE 
        CASE ColumnH = 'Category/Style Group'
          lcFldName = PADR('CSTYGROUP',10)
        *! C202040,1 MMT 07/03/2017 Change custom import PO lines to add royalty codes instead of divison codes[Start]  
*!*          CASE ColumnH = 'Division'
*!*            lcFldName = PADR('CDIVISION',10)
        CASE ColumnH = 'Brand'
          lcFldName = PADR('ROYALTY',10)
        *! C202040,1 MMT 07/03/2017 Change custom import PO lines to add royalty codes instead of divison codes[End]
        CASE ColumnH = 'Season Code'
          lcFldName = PADR('SEASON',10)
        CASE ColumnH = 'Color Code'
          lcFldName =PADR('COLOR',10)
      ENDCASE  
      llHasRel = .F.
      laRelFld = ""  
      lfClearData()
      lfGetCodeData(lcFldName)
      lnCodeWdth = 0
      llIsEditable = gfIsEdtble(lcFldName , @lnCodeWdth, oAriaApplication.ActiveCompanyID)
      SELECT (lcMissedCodes)
      SCAN FOR ColumnH = CodesToAdd.ColumnH AND AddAuto
         lcInsCode = IIF(llIsEditable, PADR(&lcMissedCodes..CurrentValue,lnCodeWdth), ALLTRIM(gfSequence("cCode_No", oAriaApplication.ActiveCompanyID)))
         lcInsDesc = IIF(ColumnH = 'Color Code',&lcMissedCodes..cClrDesc,&lcMissedCodes..CurrentValue)
         IF !(IIF(llIsEditable, EMPTY(lcInsCode), EMPTY(lcInsDesc)))
            SELECT (lcTmpCodes)
		    APPEND BLANK
		    REPLACE cDefCode  WITH 'N',;
        		    cFld_Name WITH lcFldName ,;
		            cCode_No  WITH lcInsCode,;
        		    cDiscrep  WITH lcInsDesc,;
		            cRltField WITH 'N'
		    SCATTER MEMVAR MEMO
		    REPLACE cStatus WITH 'S'
            *--If it has related fields add its records.
           IF llHasRel
		     FOR lnCount3 = 1 TO ALEN(laRelFld, 1)
		       SELECT (lcTmpRelFlds)
        	   APPEND BLANK
               REPLACE cDefCode  WITH 'N',;
                       cFld_Name WITH lcFldName ,;
                       cCode_No  WITH lcInsCode,;
                       cDiscrep  WITH '',;
                       cRltField WITH 'Y',;
                       cRltd_Nam WITH IIF(LEFT(laRelFld[lnCount3,1], 1) == '$', SUBSTR(laRelFld[lnCount3,1], 2), laRelFld[lnCount3,1]),;
                       cRltd_Typ WITH laRelFld[lnCount3,3],;
                       cRltd_Vlu WITH IIF(laRelFld[lnCount3,03]='L','F','')
              REPLACE cStatus WITH 'S'
            ENDFOR
          ENDIF
        ENDIF
      ENDSCAN
      lcSetProcd =SET("Procedure")
      SET PROCEDURE TO (oAriaApplication.ApplicationHome + 'SM\SMCODES.FXP') ADDITIVE
      llSavedCorrectly = llSavedCorrectly and lfSaveCode(oAriaApplication.ActiveCompanyID,lcFldName ,lcTmpCodes,lcTmpRelFlds,'SYCCOMP_Fil',llIsEditable)
      SET PROCEDURE TO &lcSetProcd.
    ENDSCAN
  ENDIF
  RETURN llSavedCorrectly
ELSE
  RETURN .F.
ENDIF
*!*************************************************************
*! Name      : lfSQLStatement
*! Developer : Mariam Mazhar
*! Date      : 06/05/2017
*! Purpose   : Runs a SQL Server Query Statement
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters:
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : =lfSQLStatement()
*!*************************************************************
FUNCTION lfSQLStatement
LPARAMETERS lcSQLStatment, lcCursor, lcIndex, lcTages, llNative, lcConnString, lnBuffering

LOCAL lnConnectionHandlar, lcConnString

*--Get Connection String Type
IF TYPE("lcConnString") # 'C' OR EMPTY(lcConnString)
  IF llNative
    lcConnString = oAriaApplication.cAriaNativeDataFilesConStr
  ELSE
    lcConnString = oAriaApplication.ActiveCompanyConStr
  ENDIF
ENDIF

*--Loop Until Successfully Connect or Cancel by the user
DO WHILE .T.
  *--Run the Satatement
  lnConnectionHandlar = oAriaApplication.RemoteCompanyData.SQLRun(lcSqlStatment, lcCursor, '',;
                                  lcConnString, 3, 'SAVE', SET("Datasession"))

  IF lnConnectionHandlar = 1
    *--If Query Successfully executed, Create Indexes if needed for the result cursor
    lnBuffering = IIF(TYPE('lnBuffering') = 'N', lnBuffering, CURSORGETPROP("Buffering", lcCursor))

    =lfCreateIndecies(lcCursor, lcIndex, lcTages)
    RETURN .T.
  ELSE
    *--Query Execution Error
    =oAriaApplication.RemoteCompanyData.CheckRetResult("SQLRUN", lnConnectionHandlar, .T.)
    RETURN .F.
  ENDIF
ENDDO
*!*************************************************************
*! Name      : lfCreateIndecies
*! Developer : Mariam Mazhar
*! Date      : 06/05/2017
*! Purpose   : Create Indecies for a cursor
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
FUNCTION lfCreateIndecies
LPARAMETERS lcCursor, lcIndex, lcTages

LOCAL lnOldBuffMode, lcIndex1, lcTages1, lcIndExp

*--If Query Successfully executed, Create Indexes if needed for the result cursor
lnOldBuffMode = CURSORGETPROP("Buffering", lcCursor)
=CURSORSETPROP("Buffering", 3, lcCursor)

lcTages1 = lcTages
lcIndex1 = lcIndex
SELECT (lcCursor)
DO WHILE AT("|", lcIndex1,1) <> 0
  lcIndex  = SUBSTR(lcIndex1, 1, AT("|", lcIndex1, 1) - 1)
  lcIndex1 = STRTRAN(lcIndex1, lcIndex + "|", "", 1, 1)
  lcTages  = SUBSTR(lcTages1, 1, AT("|", lcTages1, 1) - 1)
  lcTages1 = STRTRAN(lcTages1, lcTages + "|", "", 1, 1)
  INDEX ON &lcIndex. TAG (lcTages)
ENDDO
=CURSORSETPROP("Buffering", IIF(TYPE("lnBuffMode") = 'N', lnBuffMode, lnOldBuffMode), lcCursor)

RETURN .T.
*--End of lfCreateIndecies()
*!*************************************************************
*! Name      : lfIsNative
*! Developer : Mariam Mazhar
*! Date      : 06/05/2017
*! Purpose   : Check if the passed table is Fox or SQL
*!*************************************************************
*! Example   : lfIsNative()
*!*************************************************************
FUNCTION lfIsNative
LPARAMETERS lcTable

LOCAL lnAlias, llNative, lnResult
lnAlias = SELECT(0)

lnResult = oAriaApplication.RemoteSystemData.Execute("SELECT sydIndex.* FROM sydIndex INNER JOIN sydFILES ON sydFILES.cfile_nam = sydIndex.cfile_nam  where sydIndex.cfile_nam = '" + PADR(lcTable, oAriaApplication.FileW) + "' AND cver = 'A40'",;
                 "", "SysTmpCursor", "", oAriaApplication.cAria4SysFiles, 3, "", SET("Datasession"))
                
IF lnResult < 1
  oAriaApplication.RemoteSystemData.CheckRetResult("Execute", lnResult, .T.)
  SELECT (lnAlias)
  RETURN 0
ENDIF

SELECT SysTmpCursor
LOCATE
llNative = EOF()

USE IN SysTmpCursor

SELECT (lnAlias)
RETURN IIF(llNative, 1, 2)

*!*************************************************************
*! Name      : lfClearData
*! Developer : Mariam Mazhar
*! Date      : 06/05/2017
*! Purpose   : Clear Temp Codes file befor collecting data
*!*************************************************************
*! Example   : lfClearData()
*!*************************************************************
FUNCTION lfClearData

SELECT (lcTmpCodes)
=TABLEREVERT(.T.)
=CURSORSETPROP("Buffering", 3)
ZAP
=CURSORSETPROP("Buffering", 5)

SELECT (lcTmpRelFlds)
=TABLEREVERT(.T.)
=CURSORSETPROP("Buffering", 3)
ZAP
=CURSORSETPROP("Buffering", 5)

*!**************************************************************************
*! Name       : lfGetCodeData
*! Developer  : Mariam Mazhar
*! Date       : 06/05/2017
*! Purpose    : Get Code Related fields/Data
*!**************************************************************************
FUNCTION lfGetCodeData
LPARAMETERS lcVldFld

IF !USED('SYDFIELD_Fil')
  =gfOpenTable('SYDFIELD','CFLD_NAME','SH','SYDFIELD_Fil')
ENDIF
=gfSeek(PADR(lcVldFld,30),'SYDFIELD_Fil','CFLD_NAME')
llHasRel = SYDFIELD_Fil.LRLTFIELDS
lcSydField = 'SYDFIELD_Fil'
lcTmpCurs = gfTempName()
IF !USED('Codes_Fil')
  =gfOpenTable('Codes','CCODE_NO','SH','Codes_Fil')
ENDIF
IF gfSEEK('N'+PADR(lcVldFld, 10),'Codes_Fil')
  *-- Get Codes
  SELECT * FROM 'Codes_Fil';
   WHERE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM = 'N'+PADR(lcVldFld, 10) AND CRLTFIELD = 'N';
    INTO TABLE (oAriaApplication.WorkDir + lcTmpCurs + '.DBF')

  SELECT (lcTmpCodes)
  APPEND FROM (oAriaApplication.WorkDir + lcTmpCurs + '.DBF')

  *-- Get Related Fields
  SELECT * FROM 'Codes_Fil';
   WHERE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM = 'N'+PADR(lcVldFld, 10) AND CRLTFIELD = 'Y';
    INTO TABLE (oAriaApplication.WorkDir + lcTmpCurs + '.DBF')

  SELECT (lcTmpRelFlds)
  APPEND FROM (oAriaApplication.WorkDir + lcTmpCurs + '.DBF')

  *-- Delete Temp File
  IF USED(lcTmpCurs)
    USE IN (lcTmpCurs)
  ENDIF
  IF FILE(oAriaApplication.WorkDir + lcTmpCurs + '.DBF')
    DELETE FILE (oAriaApplication.WorkDir + lcTmpCurs + '.DBF')
  ENDIF
ENDIF
*--Get Default code
IF gfSEEK('D' + SUBSTR(lcVldFld,1,10),'Codes_Fil')
  SELECT 'Codes_Fil'
  IF SEEK('N'+CFLD_NAME+CCODE_NO, lcTmpCodes, 'CCODE_NO')
    SELECT (lcTmpCodes)
    REPLACE llDefCode WITH .T.,;
            cDefCode  WITH 'D'
  ELSE
    SCATTER MEMVAR MEMO
    SELECT (lcTmpCodes)
    APPEND BLANK
    GATHER MEMVAR MEMO
    REPLACE llDefCode WITH .T.
  ENDIF
ENDIF

*--Update Temp Codes file...
SELECT (lcTmpCodes)
=TABLEUPDATE(.T.)
LOCATE

SELECT (lcTmpRelFlds)
=TABLEUPDATE(.T.)
LOCATE
IF llHasRel 
  lcRltField = "|" + lfGetRFStr(lcVldFld) + "|"
  lcRltField = STRTRAN(lcRltField, SPACE(1))
  lcORltFld  = lcRltField
  lnOccur = OCCUR('~', lcRltField)
  IF lnOccur > 0
    FOR lnC = 1 TO lnOccur
      lnPos      = ATC("~", lcRltField, lnC)
      lnOccur1   = OCCUR('|', SUBSTR(lcRltField, 1, lnPos))
      lnPos1     = ATC('|', lcRltField, lnOccur1 + 1)
      lcRltField = STRTRAN(lcRltField, SUBSTR(lcRltField, lnPos, lnPos1 - lnpos), '')
    ENDFOR
  ENDIF

  LOCAL lctTmpRelFld, lcFldName, laTmpArray, lnFld
  lctTmpRelFld = STRTRAN(lcRltField, '|', '', 1, 1)
  lnFld        = 0
  *--Create the Related Fields Array...
  DO WHILE AT("|", lctTmpRelFld, 1) <> 0
    lcFldName    = SUBSTR(lctTmpRelFld, 1, AT("|", lctTmpRelFld, 1) - 1)
    lctTmpRelFld = STRTRAN(lctTmpRelFld, lcFldName + "|", "", 1, 1)
    lcFldName    = ALLTRIM(lcFldName)
    lnFld        = lnFld + 1
    IF gfSEEK(IIF(LEFt(lcFldName, 1) == '$', PADR(SUBSTR(lcFldName, 2), oAriaApplication.FieldW), PADR(lcFldName, oAriaApplication.FieldW)),lcSydField, 'CFLD_NAME')
      SELECT (lcSydField)
      LOCATE REST WHILE cfld_name = IIF(LEFt(lcFldName, 1) == '$', PADR(SUBSTR(lcFldName, 2), oAriaApplication.FieldW), PADR(lcFldName, oAriaApplication.FieldW)) FOR (CVER='A27' OR EMPTY(CVER))
      IF FOUND()
        DIMENSION laRelFld(lnFld, 14)
        laRelFld[lnFld,01] = lcFldName
        laRelFld[lnFld,02] = PADR(IIF(!EMPTY(cFld_head), cFld_head, cFld_name + SPACE(15)), 25)
        laRelFld[lnFld,03] = cData_typ
        laRelFld[lnFld,04] = nFld_Wdth
        laRelFld[lnFld,05] = nFld_dec
        laRelFld[lnFld,06] = ''
        laRelFld[lnFld,07] = ''
        laRelFld[lnFld,08] = PADR(ALLTRIM(mVald_str), 250)
        laRelFld[lnFld,09] = cVldfnloc
        laRelFld[lnFld,10] = ''
        laRelFld[lnFld,11] = lVldEntry
        laRelFld[lnFld,12] = PADR(ALLTRIM(mVEntries), 250)
        laRelFld[lnFld,13] = .F.
        laRelFld[lnFld,14] = ''
      ENDIF
    ENDIF
  ENDDO

  FOR lnArayCont = 1 TO ALEN(laRelFld, 1)
    lnPos = ATC(ALLTRIM(laRelFld[lnArayCont,1]), lcRltField)
    IF SUBSTR(lcRltField, lnPos - 1, 1) = '$'
      laRelFld[lnArayCont,13] = .T.
      IF SUBSTR(lcORltFld , lnPos + LEN(ALLTRIM(laRelFld[lnArayCont,1])), 1) = '~'
        lnOccur = OCCUR('|', SUBSTR(lcORltFld, 1, lnPos))
        lnPos1  = ATC("|", lcORltFld, lnOccur + 1)
        loFormSet.laRelFld[lnArayCont,14] = SUBSTR(lcORltFld, lnPos + LEN(ALLTRIM(laRelFld[lnArayCont, 1])) + 1, ;
           lnPos1 - (lnPos + LEN(ALLTRIM(laRelFld[lnArayCont,1]))) - 1)
      ENDIF
    ENDIF
  ENDFOR

  FOR lnArayCont = 1 TO ALEN(laRelFld, 1) - 1
    FOR I = lnArayCont + 1 TO ALEN(laRelFld, 1)
      IF ATC(ALLTRIM(laRelFld[lnArayCont,1]), lcRltField) > ATC(ALLTRIM(laRelFld[I,1]), lcRltField)
        =lfXChng(@laRelFld, lnArayCont, I)
      ENDIF
    ENDFOR
  ENDFOR

  llHasRel = !EMPTY(laRelFld[1,1])
ENDIF

*!**************************************************************************
*! Name       : lfGetRFStr
*! Developer  : Mariam Mazhar
*! Date       : 06/05/2017
*! Purpose    : Get Code Extra Related fields
*!**************************************************************************
FUNCTION lfGetRFStr
LPARAMETERS lcCodField 

lcActComp = oAriaApplication.ActiveCompanyID
LOCAL lcRet, llOpen, lcTag, lnPos, lnAlias, llApInst
lnAlias = SELECT(0)

IF TYPE("lcCodField") # "C" OR TYPE("lcActComp") # "C"
  lcRet = SPACE(0)
ELSE
  lcRet      = SPACE(0)
  lcCodField = UPPER(ALLTRIM(lcCodField))
  lcActComp  = ALLTRIM(lcActComp)
  llGlLink   = (gfGetMemVar("M_LINK_GL", lcActComp) = "Y")
  lcCountry  = SPACE(0)
  lcSysComp ='SYCCOMP_Fil'
  SELECT (lcSysComp)
  IF gfSEEK(lcActComp)
    lcCountry = UPPER(ALLTRIM(EVALUATE(lcSysComp + '.cCont_Code')))
    llApInst  = 'AP' $ UPPER(EVALUATE(lcSysComp + '.mModlSet'))
  ENDIF

  IF !EMPTY(lcCountry)
    DO CASE
      CASE lcCodField = "STATE"
		lcRet = "NTAXRATE" + IIF(lcCountry = "CANADA", "|CTAXRULE|LHSTTAX", "")
      OTHERWISE
        
        =gfSEEK(lcCodField,'SYDFIELD_Fil', 'CFLD_NAME')
        SELECT(lcSydField)
        LOCATE REST WHILE cfld_name = lcCodField FOR (CVER='A27' OR EMPTY(CVER))
        IF FOUND()
          lcRet = UPPER(ALLTRIM(EVALUATE(lcSydField + '.mRltFields')))
          IF !EMPTY(lcRet)
            DO CASE
              CASE lcCodField = "CDIVISION"
                IF !(llGlLink .AND. gfGetMemVar("M_DIV_LINK", lcActComp) = 'Y')
                  lnPos = ATC('LINK_CODE', lcRet)
                  IF lnPos > 0
                    lnMand = IIF(SUBSTR(lcRet, lnPos - 1, 1) = '$', 2, 1)
                    lcRet  = STUFF(lcRet, lnPos - lnMand, LEN('LINK_CODE') + lnMand, '')
                  ENDIF
                  lnPos = ATC('CSLSGLLINK', lcRet)
                  IF lnPos > 0
                    lnMand = IIF(SUBSTR(lcRet, lnPos - 1, 1) = '$', 2, 1)
                    lcRet  = STUFF(lcRet, lnPos - lnMand, LEN('CSLSGLLINK') + lnMand, '')
                  ENDIF
                ENDIF

                IF gfGetMemVar("M_DIV_SEQ", lcActComp) # 'Y'
                  lnPos = ATC('DIVGROUP', lcRet)
                  IF lnPos > 0
                    lnMand = IIF(SUBSTR(lcRet, lnPos - 1, 1) = '$', 2, 1)
                    lcRet  = STUFF(lcRet, lnPos - lnMand,LEN('DIVGROUP') + lnMand, '')
                  ENDIF
                ENDIF
                IF gfGetMemVar("M_UCCDIV", lcActComp) = 'Y' OR ;
                   gfGetMemVar("M_UPC_USE", lcActComp) <> 'Y'
                  lnPos = ATC('CUPCMAN', lcRet)
                  IF lnPos > 0
                    lnMand = IIF(SUBSTR(lcRet, lnPos - 1, 1) = '$', 2, 1)
                    lcRet  = STUFF(lcRet, lnPos - lnMand, LEN('CUPCMAN') + lnMand, '')
                  ENDIF
                  lnPos = ATC('CUPCGENTYP', lcRet)
                  IF lnPos > 0
                    lnMand = IIF(SUBSTR(lcRet, lnPos - 1, 1) = '$', 2, 1)
                    lcRet  = STUFF(lcRet, lnPos - lnMand, LEN('CUPCGENTYP') + lnMand, '')
                  ENDIF
                ENDIF
              CASE INLIST(lcCodField, "TRANCODE", "CCREDITCOD")
                IF !llApInst
                  lnPos = ATC('CBNKCODE', lcRet)
                  IF lnPos > 0
                    lnMand = IIF(SUBSTR(lcRet, lnPos - 1, 1) = '$', 2, 1)
                    lcRet  = STUFF(lcRet, lnPos - lnMand, LEN('CBNKCODE') + lnMand, '')
                  ENDIF
                  lnPos = ATC('CCHKACCT', lcRet)
                  IF lnPos > 0
                    lnMand = IIF(SUBSTR(lcRet, lnPos - 1, 1) = '$', 2, 1)
                    lcRet  = STUFF(lcRet, lnPos - lnMand, LEN('CCHKACCT') + lnMand, '')
                  ENDIF
                ENDIF
              CASE lcCodField = "SHIPVIA" .AND. lcCountry = "ENG"
                 lnPos = ATC('NCODCHARGE', lcRet)
                 IF lnPos > 0
                   lnMand = IIF(SUBSTR(lcRet, lnPos - 1, 1) = '$', 2, 1)

                   *--Remove the related field from the related string.
                   lcRet  = STUFF(lcRet, lnPos - lnMand, LEN('NCODCHARGE') + lnMand, '')
                 ENDIF
                 lnPos = ATC('NINSCHARGE', lcRet)
                 IF lnPos > 0
                   lnMand = IIF(SUBSTR(lcRet, lnPos - 1, 1) = '$', 2, 1)
                   lcRet  = STUFF(lcRet, lnPos - lnMand,LEN('NINSCHARGE') + lnMand, '')
                 ENDIF
                 lnPos = ATC('NFXDPRCNT', lcRet)
                 IF lnPos > 0
                   lnMand = IIF(SUBSTR(lcRet, lnPos - 1, 1) = '$', 2, 1)
                   lcRet  = STUFF(lcRet, lnPos - lnMand, LEN('NFXDPRCNT') + lnMand, '')
                 ENDIF
            ENDCASE
          ENDIF && !EMPTY(lcRet)
        ENDIF
    ENDCASE
  ENDIF
ENDIF

SELECT (lnAlias)
RETURN (lcRet)
*!*************************************************************
*! Name      : lfXChng
*! Developer : Mariam Mazhar
*! Date      : 06/05/2017
*! Purpose   : Exchange two Rows of an array
*!*************************************************************
*! Example   :  =lfxChng(@array,row1,row2)
*!*************************************************************
FUNCTION lfXchng
LPARAMETERS lcArray, lnBeg, lnEnd

LOCAL laTemp(1)

=ACOPY(lcArray, laTemp, 1 + (lnBeg - 1) * ALEN(lcArray, 2), ALEN(lcArray, 2))
=ACOPY(lcArray, lcArray, 1 + (lnEnd - 1) * ALEN(lcArray, 2), ALEN(lcArray, 2), 1 + (lnBeg - 1) * ALEN(lcArray, 2))
=ACOPY(lcTemp, lcArray, 1, ALEN(lcArray, 2), 1 + (lnEnd - 1) * ALEN(lcArray, 2))
RETURN


*!**************************************************************************
*! Name       : lfAddMissingStyle
*! Developer  : Mariam Mazhar
*! Date       : 06/05/2017
*! Purpose    : Add missing Style
*!**************************************************************************
FUNCTION lfAddMissingStyle
LPARAMETERS lcImpStyle,lcVendor,lcWareHouse
IF !USED('UPCSEQ')
  =gfOpenTable('UPCSEQ','UPCSEQ')
ENDIF
STORE 0 TO lnMajoln,lnColorLn,lnSclLen,lnSclPos 
STORE '' TO lcMajSep,lnColorSep 

DIMENSION laItemSeg[1]
  =gfItemMask(@laItemSeg)
  FOR lnCount = 1 TO ALEN(laItemSeg,1)
    DO CASE
    CASE laItemSeg[lnCount,1]='F'
      lnMajoln = LEN(laItemSeg[lnCount,3])
      lcMajSep = ALLT(laItemSeg[lnCount,6])    
      
    CASE laItemSeg[lnCount,1]='C'
      lnColorLn = LEN(laItemSeg[lnCount,3])
      lnClrPos = laItemSeg[lnCount,4]
      lnColorSep = ALLT(laItemSeg[lnCount,6])    
      
    CASE  laItemSeg[lnCount,1]='S'
      lnSclLen = LEN(laItemSeg[lnCount,3])
      lnSclPos = laItemSeg[lnCount,4]
      
    ENDCASE  
  ENDFOR
IF !USED('Codes_Fil')
  =gfOpenTable("Codes",'cCode_No','SH','Codes_Fil')
ENDIF
llOpnedSty = .F.
IF !USED('STYLE')
  =gfOpenTable("STYLE",'STYLE','SH')
  llOpnedSty = .T.
ENDIF

IF gfSeek('D'+'ROYALTY   ','Codes_Fil','cCode_No')
  lcDefRoy = Codes_Fil.CCODE_NO
ENDIF  

IF gfSeek('D'+'CDISCCODE ','Codes_Fil','cCode_No')
  lcDefDisc = Codes_Fil.CCODE_NO
ENDIF  

IF gfSeek('D'+'CPURCODE  ','Codes_Fil','cCode_No')
  lcDefPurGrp = Codes_Fil.CCODE_NO
ENDIF  
lcbomheadr = gfTempName()
lcBom = gfTempName()
lcTmpStyPr =  gfTempName()
lcprofilevalue = gfTempName()
lcProfileList =gfTempName()
lnScaleWidth =  gfGetMemVar('M_EXTWIDTH') 
lcColorFile =gfTempName()
lcWareFil  = gfTempName()     
DIMENSION laSetups[42,2]
lnX = 1
FOR lnI=1 TO 7
  lcI=STR(lnI,1)
  laSetups[lnX,1]   = 'M_cMType'+lcI
  laSetups[lnX+1,1] = 'M_cMSLbl'+lcI
  laSetups[lnX+2,1] = 'M_CMCOST'+lcI
  laSetups[lnX+3,1] = 'M_cIType'+lcI
  laSetups[lnX+4,1] = 'M_cISLbl'+lcI
  laSetups[lnX+5,1] = 'M_CICOST'+lcI
  lnX = lnX + 6
ENDFOR
=gfGetMemVar(@laSetups)

DIMENSION laICost[7,3],laMCost[7,3]
lnX = 1
FOR lnI = 1 TO 7
  lcI=STR(lnI,1)
  laMCost[lnI,1] = laSetups[lnX,2]
  laMCost[lnI,2] = laSetups[lnX+1,2]
  laMCost[lnI,3] = laSetups[lnX+2,2]

  laICost[lnI,1] = laSetups[lnX+3,2]
  laICost[lnI,2] = laSetups[lnX+4,2]
  laICost[lnI,3] = laSetups[lnX+5,2]
  lnX = lnX + 6
ENDFOR

*! C202040,1 MMT 07/03/2017 Change custom import PO lines to add royalty codes instead of divison codes[Start]
*SELECT DISTINCT cvensty,cDivision FROM (lcImpStyle) WHERE lNoVenSty INTO CURSOR 'VendorStyles'
SELECT DISTINCT cvensty FROM (lcImpStyle) WHERE lNoVenSty INTO CURSOR 'VendorStyles'
*! C202040,1 MMT 07/03/2017 Change custom import PO lines to add royalty codes instead of divison codes[End]
SELECT 'VendorStyles'
LOCATE
IF !EOF()
  SCAN
    llAddMajor = .T.
    SELECT STYLE_FIL
    LOCATE FOR cvensty = SUBSTR(VendorStyles.cvensty,1,19)
    IF FOUND()
      llAddMajor = .F.
    ENDIF
    
    IF llAddMajor && New Style Major with Colors
      lfCreateStyFil()
      =SEEK(VendorStyles.cVenSty,lcImpStyle,'ImpStyle') 
      lnLastSeq = 0
      IF !SEEK('STYLEMSEQ','UPCSEQ','UPCSEQ')   && 
        SELECT 'UPCSEQ'
        APPEND BLANK 
        REPLACE CUPCMAN WITH 'STYLEMSEQ',;
                cupclast WITH '0'
        =gfReplace('')        
        =gfTableUpdate()
      ENDIF
      lnLastSeq = VAL(UPCSEQ.cupclast)
      *! C202040,1 MMT 07/03/2017 Change custom import PO lines to add royalty codes instead of divison codes[Start]
      *lcStyleKey = PADR(SUBSTR(&lcImpStyle..cDivision,1,2)+PADL(ALLTRIM(STR(lnLastSeq)),6,'0'),lnMajoln)+lcMajSep      
      lcStyleKey = PADR(SUBSTR(&lcImpStyle..ROYALTY,1,2)+PADL(ALLTRIM(STR(lnLastSeq)),6,'0'),lnMajoln)+lcMajSep      
      *! C202040,1 MMT 07/03/2017 Change custom import PO lines to add royalty codes instead of divison codes[End]
      SELECT 'UPCSEQ'
      REPLACE cupclast WITH STR(lnLastSeq+1,6) 
      =gfReplace('')        
      =gfTableUpdate()
      
      DO WHILE gfSeek(lcStyleKey,'STYLE_Fil','STYLE')
        SELECT 'UPCSEQ'
        REPLACE cupclast WITH STR(lnLastSeq+1,6) 
        lnLastSeq = VAL(UPCSEQ.cupclast)
        lcStyleKey = PADR(SUBSTR(&lcImpStyle..cDivision,1,2)+PADL(ALLTRIM(STR(lnLastSeq)),6,'0'),lnMajoln)+lcMajSep
        =gfReplace('')        
        =gfTableUpdate()
      ENDDO 
      *! C202040,1 MMT 07/03/2017 Change custom import PO lines to add royalty codes instead of divison codes[Start] 
      *lcStyMajor = PADR(SUBSTR(&lcImpStyle..cDivision,1,2)+PADL(ALLTRIM(STR(lnLastSeq)),6,'0'),lnMajoln)
      lcStyMajor = PADR(SUBSTR(&lcImpStyle..Royalty,1,2)+PADL(ALLTRIM(STR(lnLastSeq)),6,'0'),lnMajoln)
      *! C202040,1 MMT 07/03/2017 Change custom import PO lines to add royalty codes instead of divison codes[End]
      lcNonMaj = REPLICATE('*',lnColorLn)+lnColorSep+REPLICATE('*',lnSclLen)
      SELECT  (lcImpStyle)
      =SEEK(VendorStyles.cVenSty,lcImpStyle,'ImpStyle') 
      SCAN REST WHILE cVenSty+COLOR+SCALE+SIZE = VendorStyles.cVenSty
        IF !EMPTY(&lcImpStyle..Style)
          LOOP
        ENDIF
        lcEmptyKey=&lcImpStyle..cVenSty+&lcImpStyle..COLOR+&lcImpStyle..SCALE+&lcImpStyle..SIZE
        lcStyleCode = lcStyleKey+ SUBSTR(&lcImpStyle..Color,1,lnColorLn)+lnColorSep+&lcImpStyle..Scale
        IF SEEK(lcStyleCode,lcColorFile) 
          REPLACE Style WITH lcStyleCode IN (lcImpStyle)
          LOOP
        ENDIF
        SELECT Scale_FIL
        lcScaleCode = SUBSTR(&lcImpStyle..Scale,1,lnScaleWidth)
        =SEEK('S'+lcScaleCode)
        SCAN REST WHILE type+scale+prepak ='S'+lcScaleCode
          lcStyleCode = lcStyleKey+ SUBSTR(&lcImpStyle..Color,1,lnColorLn)+lnColorSep+Scale_FIL.Scale
          lcColorCurr = &lcImpStyle..Color
          IF SEEK(VendorStyles.cVenSty+lcColorCurr+Scale_FIL.Scale,lcImpStyle)
            REPLACE Style WITH lcStyleCode FOR cVenSty+Color+Scale+Size = VendorStyles.cVenSty+lcColorCurr+Scale_FIL.Scale IN (lcImpStyle)
          ENDIF
           =SEEK(lcEmptyKey,lcImpStyle)
           IF !SEEK(lcStyleCode,lcColorFile)  
             *! C202040,1 MMT 07/03/2017 Change custom import PO lines to add royalty codes instead of divison codes[Start]
             lcRoyCode = ''
             SELECT 'Codes_Fil'
             =gfSetOrder('cCode_No')
             IF  gfSEEK("N"+'ROYALTY   ','Codes_Fil','cCode_No')
               LOCATE REST WHILE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM = "N"+'ROYALTY   ' FOR UPPER(ALLTRIM(CDISCREP)) = UPPER(ALLTRIM(&lcImpStyle..ROYALTY))
               IF FOUND()
                 lcRoyCode = Codes_Fil.CCODE_NO
               ENDIF
             ENDIF
             *! C202040,1 MMT 07/03/2017 Change custom import PO lines to add royalty codes instead of divison codes[End]
             SELECT (lcColorFile)
             APPEND BLANK 
             *! C202040,1 MMT 07/03/2017 Change custom import PO lines to add royalty codes instead of divison codes[Start]
*!*              REPLACE Style WITH lcStyleCode,;
*!*                      Scale WITH Scale_FIL.Scale,;
*!*                      cStyMajor WITH  lcStyMajor ,;
*!*                      Season WITH  &lcImpStyle..Season,;
*!*                      cDivision WITH  &lcImpStyle..cDivision ,;
*!*                      cStyGroup WITH  &lcImpStyle..cStyGroup ,;
*!*                      Royalty  WITH lcDefRoy,;
*!*                      CDISCCODE WITH lcDefDisc ,;
*!*                      cpurcode WITH lcDefPurGrp ,;
*!*                      cdefware  WITH lcWareHouse,;
*!*                      vendor  WITH lcVendor,;
*!*                      cvensty WITH &lcImpStyle..cvensty ,;
*!*                      cdutycur WITH oAriaApplication.BaseCurrency,;
*!*                      cpricecur WITH oAriaApplication.BaseCurrency,;
*!*                      cstygrade WITH '1',;
*!*                      status    WITH 'A',;
*!*                      desc  WITH &lcImpStyle..Desc ,;
*!*                      desc1 WITH &lcImpStyle..Desc ,;
*!*                      linvsty WITH .T.,;
*!*                      location WITH  lcWareHouse,;
*!*                      make  WITH .F.,;
*!*                      ncurrrate WITH 1,;
*!*                      nicost1  WITH &lcImpStyle..nfCost1,;
*!*                      gros_price WITH &lcImpStyle..nFCost1,;
*!*                      nsugretpri WITH &lcImpStyle..nsugRetPri,;
*!*                      totcost    WITH  &lcImpStyle..nfCost1,;
*!*                      Ave_Cost WITH  &lcImpStyle..nfCost1,;
*!*                      Link_Code WITH   "DEFDEF",;
*!*                      cSlsGLLink WITH  "DEF",;
*!*                      cDye_Flg  WITH   'N',;
*!*                      lDetCost  WITH  .F.,;
*!*                      lTaxable  WITH  .T.,;
*!*                      Commission WITH  .T.,;
*!*                      cStatus    WITH 'A',;
*!*                      GLFCITES   WITH &lcImpStyle..CITES,;
*!*                      GLFCOUNTRY WITH &lcImpStyle..CountOrg,;
*!*                      GLFMID    WITH   &lcImpStyle..MIDCode  ,;
*!*                      HTSCODE WITH &lcImpStyle..HTSCode,;
*!*                      FUR    WITH &lcImpStyle..FUR,;
*!*                      CTRYMADE WITH &lcImpStyle..CounMdIn,;
*!*                      MATCOMPS WITH &lcImpStyle..MatComps,;
*!*                      SCNTNAME WITH &lcImpStyle..ScentNam,;
*!*                      WILDFARM WITH &lcImpStyle..WILDFARM
            
            REPLACE Style WITH lcStyleCode,;
                    Scale WITH Scale_FIL.Scale,;
                    cStyMajor WITH  lcStyMajor ,;
                    Season WITH  &lcImpStyle..Season,;
                    cDivision WITH  &lcImpStyle..cDivision ,;
                    cStyGroup WITH  &lcImpStyle..cStyGroup ,;
                    Royalty  WITH IIF(!EMPTY(ALLTRIM(lcRoyCode)),lcRoyCode ,lcDefRoy) ,;
                    CDISCCODE WITH lcDefDisc ,;
                    cpurcode WITH lcDefPurGrp ,;
                    cdefware  WITH lcWareHouse,;
                    vendor  WITH lcVendor,;
                    cvensty WITH &lcImpStyle..cvensty ,;
                    cdutycur WITH oAriaApplication.BaseCurrency,;
                    cpricecur WITH oAriaApplication.BaseCurrency,;
                    cstygrade WITH '1',;
                    status    WITH 'A',;
                    desc  WITH &lcImpStyle..Desc ,;
                    desc1 WITH &lcImpStyle..Desc ,;
                    linvsty WITH .T.,;
                    location WITH  lcWareHouse,;
                    make  WITH .F.,;
                    ncurrrate WITH 1,;
                    nicost1  WITH &lcImpStyle..nfCost1,;
                    gros_price WITH &lcImpStyle..nFCost1,;
                    nsugretpri WITH &lcImpStyle..nsugRetPri,;
                    totcost    WITH  &lcImpStyle..nfCost1,;
                    Ave_Cost WITH  &lcImpStyle..nfCost1,;
                    Link_Code WITH   "DEFDEF",;
                    cSlsGLLink WITH  "DEF",;
                    cDye_Flg  WITH   'N',;
                    lDetCost  WITH  .F.,;
                    lTaxable  WITH  .T.,;
                    Commission WITH  .T.,;
                    cStatus    WITH 'A',;
                    GLFCITES   WITH &lcImpStyle..CITES,;
                    GLFCOUNTRY WITH &lcImpStyle..CountOrg,;
                    GLFMID    WITH   &lcImpStyle..MIDCode  ,;
                    HTSCODE WITH &lcImpStyle..HTSCode,;
                    FUR    WITH &lcImpStyle..FUR,;
                    CTRYMADE WITH &lcImpStyle..CounMdIn,;
                    MATCOMPS WITH &lcImpStyle..MatComps,;
                    SCNTNAME WITH &lcImpStyle..ScentNam,;
                    WILDFARM WITH &lcImpStyle..WILDFARM
            *! C202040,1 MMT 07/03/2017 Change custom import PO lines to add royalty codes instead of divison codes[End]        
            SCATTER MEMO MEMVAR
            IF !SEEK(lcStyleKey ,'STYLE','STYLE')
              gfAppend(' IN Style', .T.)
            ELSE
              SELECT STYLE
            ENDIF          
            SELECT STYLE 
            GATHER MEMO MEMVAR
            gfReplace('')
            REPLACE Style WITH EVALUATE(lcColorFile+ '.Style')
            LOCAL lcCursUpd
            lcCursUpd = ''
            lnStyPos = gfGetRemoteTable(SET("Datasession"), 'Style')
            IF lnStyPos > 0
              lcCursUpd = oAriaApplication.laRemoteTable[lnStyPos].lcCursorUpdate
            ENDIF  
            SELECT (lcCursUpd)
            REPLACE Style WITH EVALUATE(lcColorFile+ '.Style')
  
            SELECT(lcWareFil)           
            APPEND BLANK
            REPLACE STYLE WITH lcStyleCode,;
                    dyelot    WITH SPACE(10),;
                    cwarecode WITH lcWareHouse,;
                    Desc      WITH &lcImpStyle..Desc ,;
                    gl_link   WITH 'DEFDEF'  ,;
                    cstatus   WITH 'A'       ,; 
                    ave_cost  WITH &lcImpStyle..nfCost1,;
                    cDiscCode WITH lcDefDisc 
          ENDIF
        ENDSCAN
        
      
      ENDSCAN
      lcTranCode = ''
      llSusses = .F.
      lcSetProcd =SET("Procedure")
      SET PROCEDURE TO (oAriaApplication.ApplicationHome + 'IC\ICSTYSAV.FXP') ADDITIVE
      lfSaveStyle(lcStyleKey,lcStyMajor,lcNonMaj,lcStyleKey,lcColorFile,.T.,.T.,.F.,lcBomHeadr,lcBOM,lcTmpStyPr,lcprofilevalue,lcProfileList ,;
      oAriaApplication.BaseCurrency,oAriaApplication.BaseCurrency,1,1,@laMCost,@laICost,1,1,lcWareFil,0,.F.,.F.,.F.)
      SET PROCEDURE TO &lcSetProcd.
      IF llSusses
        llSusses = oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode) = 1
      ENDIF

      SELECT STYLE
      =GFTABLEUPDATE()
      IF USED('ICSTYHST')
        =gfCloseTable('ICSTYHST')
      ENDIF
    ELSE  && adding missing colors from existing Style Major
    
    ENDIF
  ENDSCAN
ENDIF 
*Adding missing Colors
SELECT DISTINCT cvensty,Color FROM (lcImpStyle) WHERE !lNoVenSty INTO CURSOR 'VenStyClr'
SELECT 'VenStyClr'
LOCATE
IF !EOF()
  lfCreateStyFil()
  SELECT 'VenStyClr'
  SCAN
    =SEEK(VenStyClr.cVenSty+VenStyClr.Color,lcImpStyle,'ImpStyle')
    =gfSeek('S'+&lcImpStyle..Scale,'Scale_Fil','Scale')
    SELECT STYLE_Fil
    LOCATE FOR cvensty = SUBSTR(VenStyClr.cvensty,1,19) AND Scale = Scale_Fil.Scale
    IF FOUND()
      lcStyleSample = SUBSTR(STYLE_Fil.STYLE,1,lnMajoln)
      =gfSeek(lcStyleSample ,'STYLE','STYLE')
      SELECT STYLE_Fil
      =gfSeek(lcStyleSample ,'STYLE_Fil','STYLE')
      SCAN REST WHILE STYLE = lcStyleSample 
        SCATTER MEMO MEMVAR
        IF !SEEK(STYLE_Fil.STYLE,lcColorFile)  
          SELECT (lcColorFile)
          m.cStatus ='S'
          APPEND BLANK 
          GATHER MEMO MEMVAR 
        ENDIF           
      ENDSCAN  
      =SEEK(VenStyClr.cVenSty+VenStyClr.Color,lcImpStyle,'ImpStyle') 
      lcStyleKey = lcStyleSample +lcMajSep
      lcStyMajor = lcStyleSample 
      lcNonMaj = REPLICATE('*',lnColorLn)+lnColorSep+REPLICATE('*',lnSclLen)
      SELECT Scale_FIL
      lcScaleCode = SUBSTR(&lcImpStyle..Scale,1,lnScaleWidth)
      =SEEK('S'+lcScaleCode,'Scale_FIL','Scale')
      SCAN REST WHILE TYPE+SCALE ='S'+lcScaleCode
        lcNewSty =  lcStyleSample+lcMajSep  + SUBSTR(VenStyClr.Color,1,lnColorLn)+lnColorSep+ Scale_FIL.Scale
        REPLACE STYLE WITH lcNewSty FOR cVenSty = VenStyClr.cVenSty AND Color =  VenStyClr.Color AND Scale = Scale_FIL.Scale IN (lcImpStyle)
        IF !SEEK(VenStyClr.cVenSty+VenStyClr.Color+ Scale_FIL.Scale,lcImpStyle,'ImpStyle') 
          =SEEK(VenStyClr.cVenSty+VenStyClr.Color,lcImpStyle,'ImpStyle') 
        ENDIF

        IF !SEEK(lcNewSty,lcColorFile)  
          SELECT (lcColorFile)
          m.Style = lcNewSty
          m.Scale = Scale_FIL.Scale
          m.cStatus ='A'
          m.GLFCITES   = &lcImpStyle..CITES
          m.GLFCOUNTRY = &lcImpStyle..CountOrg
          m.GLFMID    =&lcImpStyle..MIDCode  
          m.HTSCODE = &lcImpStyle..HTSCode
          m.FUR    = &lcImpStyle..FUR
          m.CTRYMADE = &lcImpStyle..CounMdIn
          m.MATCOMPS = &lcImpStyle..MatComps
          m.SCNTNAME  = &lcImpStyle..ScentNam
          m.WILDFARM = &lcImpStyle..WILDFARM
          m.gros_price = &lcImpStyle..nFCost1
          m.niCost1 = &lcImpStyle..nFCost1
          *! C202040,1 MMT 07/03/2017 Change custom import PO lines to add royalty codes instead of divison codes[Start]
          m.cDivision = &lcImpStyle..cDivision
          lcRoyCode = ''
          SELECT 'Codes_Fil'
          =gfSetOrder('cCode_No')
          IF  gfSEEK("N"+'ROYALTY   ','Codes_Fil','cCode_No')
            LOCATE REST WHILE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM = "N"+'ROYALTY   ' FOR UPPER(ALLTRIM(CDISCREP)) = UPPER(ALLTRIM(&lcImpStyle..ROYALTY))
            IF FOUND()
              lcRoyCode = Codes_Fil.CCODE_NO
            ENDIF
          ENDIF
          m.Royalty = IIF(!EMPTY(ALLTRIM(lcRoyCode)),lcRoyCode ,lcDefRoy)
          SELECT (lcColorFile)
          *! C202040,1 MMT 07/03/2017 Change custom import PO lines to add royalty codes instead of divison codes[End]
          APPEND BLANK 
          GATHER MEMO MEMVAR 
          SELECT(lcWareFil)           
          APPEND BLANK
          REPLACE STYLE WITH lcNewSty,;
                  dyelot    WITH SPACE(10),;
                  cwarecode WITH lcWareHouse,;
                  Desc      WITH &lcImpStyle..Desc ,;
                  gl_link   WITH 'DEFDEF'  ,;
                  cstatus   WITH 'A'       ,; 
                  ave_cost  WITH &lcImpStyle..nfCost1,;
                  cDiscCode WITH m.cDiscCode 
        ENDIF           
        
      ENDSCAN
      lcSetProcd =SET("Procedure")
      loStyFormSet = CREATEOBJECT("CUSTOM")
      loStyFormSet.AddProperty("ActiveMode",'E')
      loStyFormSet.AddProperty("lcFromStyle",'')
      loStyFormSet.AddProperty("llCopyContinue", .F.)
      loStyFormSet.AddProperty("LLDONTSAVEPROFILE", .F.)
      loStyFormSet.AddProperty("cBrowseTableDBEngine", "NATIVE")
      loStyFormSet.AddProperty("laEvntTrig[1,1]", "")
      lcOnError = ON('Error')
      lcTranCode = ''
      llSusses = .F.
      ON ERROR llStyEr = .T.
      SET PROCEDURE TO (oAriaApplication.ApplicationHome + 'IC\ICSTYSAV.FXP') ADDITIVE
      lfSaveStyle(lcStyleKey,lcStyMajor,lcNonMaj,lcStyleKey,lcColorFile,.T.,.T.,.F.,lcBomHeadr,lcBOM,lcTmpStyPr,lcprofilevalue,lcProfileList ,;
      oAriaApplication.BaseCurrency,oAriaApplication.BaseCurrency,1,1,@laMCost,@laICost,1,1,lcWareFil,0,.F.,.F.,loStyFormSet)
      ON ERROR &lcOnError.
      SET PROCEDURE TO &lcSetProcd.
      IF llSusses
        llSusses = oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode) = 1
      ENDIF
      SELECT STYLE
      =GFTABLEUPDATE()
      IF USED('ICSTYHST')
        =gfCloseTable('ICSTYHST')
      ENDIF
    ENDIF
  ENDSCAN
ENDIF


*Saving UPCs
SELECT (lcImpStyle)
LOCATE FOR !EMPTY(Style) AND !EMPTY(cSizeNo) AND !EMPTY(UPC)
IF FOUND()
  IF !USED('STYLEUPC_Fil') 
    =gfOpenTable('STYLEUPC','STYUPCN','SH','STYLEUPC_Fil')
  ENDIF
  SELECT (lcImpStyle)
  SCAN FOR !EMPTY(Style) AND !EMPTY(cSizeNo) AND !EMPTY(UPC)
    IF !gfSeek(&lcImpStyle..Style+&lcImpStyle..cSizeNo,'STYLEUPC_Fil','STYLEUPC') AND;
       !gfSeek(ALLTRIM(&lcImpStyle..UPC),'STYLEUPC_Fil','STYUPCN') 
          && CUPCNUM1+CUPCNUM2+CUPCNUM3
      SELECT STYLEUPC_Fil
      APPEND BLANK 
      REPLACE STYLE WITH &lcImpStyle..Style,;
              Size  WITH &lcImpStyle..cSizeNo ,;
              cUpcNum1 with SUBSTR(ALLTRIM(&lcImpStyle..UPC),1,6),;
              cUpcNum2 with SUBSTR(ALLTRIM(&lcImpStyle..UPC),7,5),;
              cUpcNum3 with SUBSTR(ALLTRIM(&lcImpStyle..UPC),12),;
              cAdd_user with oAriaApplication.User_ID,;
              dAdd_date with  DATE(),;
              cAdd_time WITH TIME()
      =gfReplace('')        
    ENDIF
  ENDSCAN
  SELECT 'STYLEUPC_Fil'
  =gfTableUpdate()
ENDIF

**Update UDFs from Excel 
SELECT (lcImpStyle)
LOCATE
SCAN FOR !EMPTY(Style)
   IF gfSeek(SUBSTR(&lcImpStyle..STYLE,1,lnSclPos-1),'STYLE_FIL','STYLE')
     IF STYLE_FIL.GLFCITES <> &lcImpStyle..CITES 
       REPLACE GLFCITES WITH &lcImpStyle..CITES IN STYLE_FIL REST WHILE STYLE = SUBSTR(&lcImpStyle..STYLE,1,lnSclPos-1)
     ENDIF
     =gfSeek(SUBSTR(&lcImpStyle..STYLE,1,lnSclPos-1),'STYLE_FIL','STYLE')
     IF STYLE_FIL.GLFCOUNTRY <> &lcImpStyle..CountOrg AND !EMPTY(ALLTRIM(&lcImpStyle..CountOrg))  
       REPLACE GLFCOUNTRY WITH &lcImpStyle..CountOrg IN STYLE_FIL REST WHILE STYLE = SUBSTR(&lcImpStyle..STYLE,1,lnSclPos-1)
     ENDIF
     =gfSeek(SUBSTR(&lcImpStyle..STYLE,1,lnSclPos-1),'STYLE_FIL','STYLE')
     IF STYLE_FIL.GLFMID    <> &lcImpStyle..MIDCode AND !EMPTY(ALLTRIM(&lcImpStyle..MIDCode))  
       REPLACE GLFMID    WITH &lcImpStyle..MIDCode  IN STYLE_FIL REST WHILE STYLE = SUBSTR(&lcImpStyle..STYLE,1,lnSclPos-1)
     ENDIF
     =gfSeek(SUBSTR(&lcImpStyle..STYLE,1,lnSclPos-1),'STYLE_FIL','STYLE')
     IF STYLE_FIL.HTSCODE <> &lcImpStyle..HTSCode AND !EMPTY(ALLTRIM(&lcImpStyle..HTSCode))  
       REPLACE HTSCODE WITH &lcImpStyle..HTSCode IN STYLE_FIL REST WHILE STYLE = SUBSTR(&lcImpStyle..STYLE,1,lnSclPos-1)
     ENDIF
     =gfSeek(SUBSTR(&lcImpStyle..STYLE,1,lnSclPos-1),'STYLE_FIL','STYLE')
     IF STYLE_FIL.FUR <> &lcImpStyle..FUR    
       REPLACE FUR WITH &lcImpStyle..FUR IN STYLE_FIL REST WHILE STYLE = SUBSTR(&lcImpStyle..STYLE,1,lnSclPos-1)
     ENDIF
     =gfSeek(SUBSTR(&lcImpStyle..STYLE,1,lnSclPos-1),'STYLE_FIL','STYLE')
     IF STYLE_FIL.CTRYMADE <> &lcImpStyle..CounMdIn AND !EMPTY(ALLTRIM(&lcImpStyle..CounMdIn))  
       REPLACE CTRYMADE WITH &lcImpStyle..CounMdIn IN STYLE_FIL REST WHILE STYLE = SUBSTR(&lcImpStyle..STYLE,1,lnSclPos-1)
     ENDIF
     =gfSeek(SUBSTR(&lcImpStyle..STYLE,1,lnSclPos-1),'STYLE_FIL','STYLE')
     IF STYLE_FIL.MatComps <> &lcImpStyle..MatComps  AND !EMPTY(ALLTRIM(&lcImpStyle..MatComps))  
       REPLACE MatComps WITH &lcImpStyle..MatComps IN STYLE_FIL REST WHILE STYLE = SUBSTR(&lcImpStyle..STYLE,1,lnSclPos-1)
     ENDIF
     =gfSeek(SUBSTR(&lcImpStyle..STYLE,1,lnSclPos-1),'STYLE_FIL','STYLE')
     IF STYLE_FIL.SCNTNAME <> &lcImpStyle..ScentNam AND !EMPTY(ALLTRIM(&lcImpStyle..ScentNam))  
       REPLACE SCNTNAME  WITH &lcImpStyle..ScentNam IN STYLE_FIL REST WHILE STYLE = SUBSTR(&lcImpStyle..STYLE,1,lnSclPos-1)
     ENDIF
     =gfSeek(SUBSTR(&lcImpStyle..STYLE,1,lnSclPos-1),'STYLE_FIL','STYLE')
     IF STYLE_FIL.WILDFARM <> &lcImpStyle..WILDFARM AND !EMPTY(ALLTRIM(&lcImpStyle..WILDFARM))
       REPLACE WILDFARM WITH &lcImpStyle..WILDFARM IN STYLE_FIL REST WHILE STYLE = SUBSTR(&lcImpStyle..STYLE,1,lnSclPos-1)
     ENDIF

     *! C202040,1 MMT 07/03/2017 Change custom import PO lines to add royalty codes instead of divison codes[Start]
     lcRoyCode = ''
     SELECT 'Codes_Fil'
     =gfSetOrder('cCode_No')
     IF  gfSEEK("N"+'ROYALTY   ','Codes_Fil','cCode_No')
       LOCATE REST WHILE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM = "N"+'ROYALTY   ' FOR UPPER(ALLTRIM(CDISCREP)) = UPPER(ALLTRIM(&lcImpStyle..ROYALTY))
       IF FOUND()
         lcRoyCode = Codes_Fil.CCODE_NO
       ENDIF
     ENDIF
     =gfSeek(SUBSTR(&lcImpStyle..STYLE,1,lnSclPos-1),'STYLE_FIL','STYLE')
     IF !EMPTY(ALLTRIM(lcRoyCode)) AND STYLE_FIL.Royalty <> lcRoyCode
       REPLACE Royalty WITH lcRoyCode IN STYLE_FIL REST WHILE STYLE = SUBSTR(&lcImpStyle..STYLE,1,lnSclPos-1)
     ENDIF
     =gfSeek(SUBSTR(&lcImpStyle..STYLE,1,lnSclPos-1),'STYLE_FIL','STYLE')
     IF &lcImpStyle..cDivision <>  STYLE_FIL.cDivision
       REPLACE cDivision WITH &lcImpStyle..cDivision IN STYLE_FIL REST WHILE STYLE = SUBSTR(&lcImpStyle..STYLE,1,lnSclPos-1)
     ENDIF
     *! C202040,1 MMT 07/03/2017 Change custom import PO lines to add royalty codes instead of divison codes[End]
    
     SELECT STYLE_FIL
     =gfReplace('')
   ENDIF  
ENDSCAN
SELECT STYLE_FIL
=gfTableUpdate()
IF llOpnedSty
  =gfCloseTable("STYLE")
ENDIF
SELECT(lcImpStyle) 
LOCATE FOR EMPTY(STYLE)
IF !FOUND()
  RETURN .T.
ELSE
  RETURN .F.  
ENDIF
*!**************************************************************************
*! Name       : lfCreateStyFil
*! Developer  : Mariam Mazhar
*! Date       : 06/05/2017
*! Purpose    : Create Style temp. files
*!**************************************************************************
FUNCTION lfCreateStyFil
IF USED(lcColorFile)
  USE IN (lcColorFile)
ENDIF
SELECT STYLE_FIL
=AFIELDS(laFileStru)
lnFFlds = ALEN(lafilestru,1)
DIMENSION lafilestru[lnFFlds+5,18]
lafilestru[lnFFlds+1,1] = 'CSTATUS'
lafilestru[lnFFlds+1,2] = 'C'
lafilestru[lnFFlds+1,3] = 1
lafilestru[lnFFlds+2,1] = 'CCOLOR'
lafilestru[lnFFlds+2,2] = 'C'
lafilestru[lnFFlds+2,3] = IIF(lnColorLn#0,lnColorLn,6)
lafilestru[lnFFlds+3,1] = 'CCLRRGB'
lafilestru[lnFFlds+3,2] = 'C'
lafilestru[lnFFlds+3,3] = 40
lafilestru[lnFFlds+4,1] = 'CCLRSDS'
lafilestru[lnFFlds+4,2] = 'C'
lafilestru[lnFFlds+4,3] = 20
lafilestru[lnFFlds+5,1] = 'CCLRLDS'
lafilestru[lnFFlds+5,2] = 'C'
lafilestru[lnFFlds+5,3] = 40

STORE 0 TO lafilestru[lnFFlds+1,4],lafilestru[lnFFlds+2,4],lafilestru[lnFFlds+3,4],lafilestru[lnFFlds+4,4],lafilestru[lnFFlds+5,4]
STORE .F. TO lafilestru[lnFFlds+1,5],lafilestru[lnFFlds+1,6],lafilestru[lnFFlds+2,5],lafilestru[lnFFlds+2,6],lafilestru[lnFFlds+3,5],;
             lafilestru[lnFFlds+3,6],lafilestru[lnFFlds+4,5],lafilestru[lnFFlds+4,6],lafilestru[lnFFlds+5,5],lafilestru[lnFFlds+5,6]
FOR lnFCount = 1 TO 5
STORE "" TO lafilestru[lnFFlds+lnFCount,7],;
lafilestru[lnFFlds+lnFCount,8],;
lafilestru[lnFFlds+lnFCount,9],;
lafilestru[lnFFlds+lnFCount,10],;
lafilestru[lnFFlds+lnFCount,11],;
            lafilestru[lnFFlds+lnFCount,12],;
            lafilestru[lnFFlds+lnFCount,13],;
            lafilestru[lnFFlds+lnFCount,14],;
            lafilestru[lnFFlds+lnFCount,15],;
            lafilestru[lnFFlds+lnFCount,16]
            
  STORE 0 TO laFilestru[lnfflds+lnFCount,17],laFilestru[lnfflds+lnFCount,18]
ENDFOR
=gfCrtTmp(lcColorFile,@laFileStru,"Style","colororder")


IF USED(lcbom)
  USE IN (lcbom)
ENDIF
IF USED(lcbomheadr)
  USE IN (lcbomheadr)
ENDIF

IF !USED('BOMHEADR_FIL')
  =gfOpenTable('BOMHEADR','BOMHEADR','SH','BOMHEADR_FIL')
ENDIF
IF !USED('BOM_FIL')
  =gfOpenTable('BOM','MULTIBOM','SH','BOM_FIL')
ENDIF
SELECT BOMHEADR_FIL
lnFld = AFIELDS(laBomHStru)
=gfCrtTmp(lcbomheadr,@laBomHStru,"CITMMAJOR+CCSTSHTTYP+CCSTSHT_ID",lcbomheadr)
SELECT BOM_FIL
lnFld = AFIELDS(laBomStru)
=gfCrtTmp(lcbom,@laBomStru,"CITMMAJOR+TYP+CITMMASK+MFGCODE+ITEM",'BOM')
SELECT (lcbom)
INDEX on typ+item+citmmajor+citmmask TAG 'Bomitem' ADDITIVE 

IF USED(lcWareFil)
  USE IN (lcWareFil)
ENDIF

IF !USED('StyDye_Fil')
  =gfOpenTable('StyDye','STYDYE','SH','STYDYE_FIL')
ENDIF

SELECT StyDye_Fil
=AFIELDS(lafilestru)
lnFFlds = ALEN(lafilestru,1)
DIMENSION lafilestru[lnFFlds+1,18]
lafilestru[lnFFlds+1,1] = 'CSTATUS'
lafilestru[lnFFlds+1,2] = 'C'
lafilestru[lnFFlds+1,3] = 1
lafilestru[lnFFlds+1,4] = 0
lafilestru[lnFFlds+1,5] = .T.
lafilestru[lnFFlds+1,6] = .T.
STORE "" TO lafilestru[lnFFlds+1,7],lafilestru[lnFFlds+1,8],lafilestru[lnFFlds+1,9],lafilestru[lnFFlds+1,10],lafilestru[lnFFlds+1,11],;
            lafilestru[lnFFlds+1,12],lafilestru[lnFFlds+1,13],lafilestru[lnFFlds+1,14],lafilestru[lnFFlds+1,15],lafilestru[lnFFlds+1,16]
STORE 0 TO lafilestru[lnFFlds+1,17],lafilestru[lnFFlds+1,18]
=gfCrtTmp(lcwarefil,@lafilestru)
SELECT (lcWareFil)
INDEX ON cwarecode TAG cwarecode Unique
SET ORDER TO       TAG cwarecode 

*!**************************************************************************
*! Name       : lfAddPOLine
*! Developer  : Mariam Mazhar
*! Date       : 06/05/2017
*! Purpose    : Add PO lines
*!**************************************************************************
FUNCTION lfAddPOLine
PARAMETERS lcPOLines,lcImpStyle,loPOFormSet
IF TYPE('lcImpStyle') ='C' AND USED(lcImpStyle)
  SELECT(lcImpStyle)
  LOCATE
  SCAN 
    lcSizeCnt = ALLTRIM(&lcImpStyle..cSizeNo)
    IF !SEEK(&lcImpStyle..Style,'POLines')
      SELECT (lcPOLines)
      APPEND BLANK 
      REPLACE Style           WITH &lcImpStyle..Style,;
              cDivision       WITH &lcImpStyle..cDivision ,;
              cStyGroup       WITH &lcImpStyle..cStyGroup ,;
              Qty&lcSizeCnt.  WITH &lcImpStyle..Qty,;
              nfCost1         WITH &lcImpStyle..nFCost1,;
              TotQty          WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
    ELSE
      SELECT (lcPOLines)
      LOCATE REST WHILE Style = &lcImpStyle..Style FOR nfCost1 = &lcImpStyle..nFCost1
      IF FOUND()
        REPLACE Qty&lcSizeCnt.  WITH Qty&lcSizeCnt. + &lcImpStyle..Qty,;           
                    TotQty          WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8       
      ELSE
        SELECT (lcPOLines)
        APPEND BLANK 
        REPLACE Style           WITH &lcImpStyle..Style,;
                cDivision       WITH &lcImpStyle..cDivision ,;
                cStyGroup       WITH &lcImpStyle..cStyGroup ,;
                Qty&lcSizeCnt.  WITH &lcImpStyle..Qty,;
                nfCost1         WITH &lcImpStyle..nFCost1,;
                TotQty          WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
      ENDIF          
    ENDIF
  ENDSCAN
ENDIF 

SELECT(lcPOLines)
Select DISTINCT UPPER(ALLTRIM(cDivision)) FROM (lcPOLines) WHERE  !DELETED() INTO ARRAY laDivArr
IF _TALLY > 1
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Styles have different divisions. Cannot proceed.')
  IF USED(lcXls)
    USE IN (lcXls)
  ENDIF
  IF USED('TMPSTR')
    USE IN 'TMPSTR'
  ENDIF

  ERASE (oAriaApplication.WorkDir+lcXls+'.DBF')
  loFormSetObj.Release()
  RETURN
ELSE
  *B611356,1 MMT 07/11/2017 Custom Import PO lines for GLF10 imports blank colors[P20170503.0001- Issue#1][Start]
  IF _TALLY > 0
  *B611356,1 MMT 07/11/2017 Custom Import PO lines for GLF10 imports blank colors[P20170503.0001- Issue#1][End]
    loPOFormSet.AriaForm1.pgfPOStyle.page1.cntheaderFolder.cboDivision.oldValue = loPOFormSet.AriaForm1.pgfPOStyle.page1.cntheaderFolder.cboDivision.Value
    loPOFormSet.AriaForm1.pgfPOStyle.page1.cntheaderFolder.cboDivision.GetCodes()
    loPOFormSet.AriaForm1.pgfPOStyle.page1.cntheaderFolder.cboDivision.Value = laDivArr[1]
    loPOFormSet.AriaForm1.pgfPOStyle.page1.cntheaderFolder.cboDivision.Valid()
  *B611356,1 MMT 07/11/2017 Custom Import PO lines for GLF10 imports blank colors[P20170503.0001- Issue#1][Start]  
  ENDIF
  *B611356,1 MMT 07/11/2017 Custom Import PO lines for GLF10 imports blank colors[P20170503.0001- Issue#1][End]
ENDIF
IF !USED('StyDye_Fil')
  =gfOpenTable('StyDye','STYDYE','SH','STYDYE_FIL')
ENDIF
lcPOLineTmp =  loPOFormSet.ariaForm1.mainworkorder.cPoLine
loPOFormSet.ariaFORM1.pgfPOStyle.page2.CntDetailFolder.editregion1.llimportbatch = .T.
SELECT(lcPOLines)
LOCATE
SCAN
  lcStySel = &lcPOLines..STYLE
  lnDataSess = SET("Datasession") 
  SET DATASESSION TO loPOFormSet.DataSessionID
  IF SEEK(lcStySel,lcPOLineTmp,lcPOLineTmp)
    SET DATASESSION TO lnDataSess
    REPLACE lImported WITH .T. IN (lcPOLines)
    SELECT(lcPOLines)
    LOOP
  ENDIF
  SET DATASESSION TO lnDataSess  
  IF !gfSeek(&lcPOLines..Style+ALLTRIM(UPPER(loPOFormSet.AriaForm1.cntPoHeader.kbWareHouse.keytextbox.Value)),'STYDYE_FIL','STYDYE')
    DO gpAdStyWar WITH &lcPOLines..Style,SPACE(10),ALLTRIM(UPPER(loPOFormSet.AriaForm1.cntPoHeader.kbWareHouse.keytextbox.Value))
  ENDIF
  WITH loPOFormSet.ariaFORM1.pgfPOStyle.page2.CntDetailFolder.editregion1
    .cmdNew.Click()
    .kbStyle.txtItem.value = ''
    .kbStyle.txtItem.value = &lcPOLines..Style
    .kbStyle.txtItem.valid()
  
    .txtGrossPrice.oldValue = 0
    .txtGrossPrice.Value = &lcPOLines..nfCost1
    .txtGrossPrice.Valid()
  
    .txtTotalQty.GotFocus()
    .txtTotalQty.Value = &lcPOLines..TotQty
    .txtTotalQty.Valid()
    .txtTotalQty.lostFocus()
  
    .cntSizesQty.GotFocus()
    .cntSizesQty.txtQty1.Value = &lcPOLines..Qty1
    .cntSizesQty.txtQty1.Valid
    .cntSizesQty.txtQty2.Value = &lcPOLines..Qty2
    .cntSizesQty.txtQty2.Valid
    .cntSizesQty.txtQty3.Value = &lcPOLines..Qty3
    .cntSizesQty.txtQty3.Valid
    .cntSizesQty.txtQty4.Value = &lcPOLines..Qty4
    .cntSizesQty.txtQty4.Valid  
    .cntSizesQty.txtQty5.Value = &lcPOLines..Qty5
    .cntSizesQty.txtQty5.Valid  
    .cntSizesQty.txtQty6.Value = &lcPOLines..Qty6
    .cntSizesQty.txtQty6.Valid
    .cntSizesQty.txtQty7.Value = &lcPOLines..Qty7
    .cntSizesQty.txtQty7.Valid  
    .cntSizesQty.txtQty8.Value = &lcPOLines..Qty8
    .cntSizesQty.txtQty8.Valid
    .cntSizesQty.lostFocus()
    loPOFormSet.ariaFORM1.pgfPOStyle.page2.CntDetailFolder.cNextStyle = ''
  ENDWITH
  =Seek(lcStySel,lcPOLines,'POLines')
ENDSCAN
loPOFormSet.ariaFORM1.pgfPOStyle.page2.CntDetailFolder.editregion1.llimportbatch = .F.
loPOFormSet.ariaFORM1.pgfPOStyle.page2.CntDetailFolder.mbrowsefields ("PP")
*! B611476,1 MMT 11/16/2017 Custom PO import program lines price is changed after import[T20171103.0003][Start]
loPOFormSet.ariaFORM1.pgfPOStyle.page2.CntDetailFolder.editregion1.kbCostSheet.KeyTextBox.oldValue= loPOFormSet.ariaFORM1.pgfPOStyle.page2.CntDetailFolder.editregion1.kbCostSheet.KeyTextBox.Value
loPOFormSet.ariaFORM1.pgfPOStyle.page2.CntDetailFolder.editregion1.kbCostSheet.Tag =''
*! B611476,1 MMT 11/16/2017 Custom PO import program lines price is changed after import[T20171103.0003][End]