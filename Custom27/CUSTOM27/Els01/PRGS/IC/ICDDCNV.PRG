*:----------------------------------------------------------------
*: Program file        : ICDDCNV.PRG
*: Program description : Import Style/UPC/Codes from XLS File
*: For screen          : ICDDCNV.SPR
*: For System          : Aria Advantage Series - Version 2.7
*: For Module          : Inventory Control - (IC)
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : C200151
*:----------------------------------------------------------------
*: Called From         : 1. System Menu\IC\FILES
*:----------------------------------------------------------------
*: Passed Parameters   : None
*:----------------------------------------------------------------
*: Example             : DO ICDDCNV
*:----------------------------------------------------------------
*--- This program Import style,UPC and Codes from XLS file.
*--- The XLS file must be saved in Excel 2.1 Format.
IF !gfSetup()
  RETURN
ENDIF
lnThermSiz = 32        && The default size of the thermometers.
lcCurrFile = "Overall Processing"
lcTempName = gfTempName()
lcTempRej  = gfTempName()
lcErroSrc  = gcWorkDir+"ICDDEror.TXT"
lcFPer     = "0 %"
lcFTherm   = ""
lcOSourc   = ""
lcSource   = ""

DO (gcScrDir +  gcAct_Appl + '\ICDDCNV.SPR')
IF FILE(gcWorkDir+lcTempRej+".DBF")
  SELECT (lcTempRej)
  GO TOP
  IF FILE(gcWorkDir+lcTempRej+".DBF")
    SELECT (lcTempRej)
    GO TOP
    IF EOF()
      APPEND BLANK
      REPLACE cError  WITH "Conversion process has been canceled."
    ELSE
      IF ALLTRIM(cError)  = "DONE"
        REPLACE cError  WITH "Conversion process completed successfully."
      ENDIF
    ENDIF
  ENDIF
  DO (gcScrDir +  gcAct_Appl + '\ICDDeror.SPR')
  COPY TO (lcErroSrc) TYPE DELIMITED
  IF USED(lcTempRej)
    USE IN (lcTempRej)
  ENDIF
  ERASE (gcWorkDir)+(lcTempRej)+".DBF"
ENDIF
CLEAR READ

*:----------------------------------------------------------------
*: Program file        : lfvSelfile
*: Program description : Valid select file button
*: For screen          : ICDDCNV.SPR
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : C200151
*:----------------------------------------------------------------
*: Passed Parameters   : None
*:----------------------------------------------------------------
*: Example             : =lfvSelfile()
*:----------------------------------------------------------------
FUNCTION lfvSelfile

lcSource = GETFILE('XLS','Select '+lcFileType+' File to be convert','Convert')
IF !(lcOSourc == lcSource)
  WAIT "Validating entery.. Please wait" WINDOW NOWAIT
  IF !EMPTY(lcSource) AND lfChkCon()
    SHOW GET pbProc ENABLE
  ELSE
    IF !EMPTY(lcSource)
      lcOvrWrit = "Invalid XLS file format."
      =gfModalGen('INM00000B00000',.F.,.F.,.F.,lcOvrWrit)
    ENDIF
    lcSource = lcOSourc
    _CUROBJ = _CUROBJ
  ENDIF  
  WAIT CLEAR 
  SHOW GET lcSource 
ENDIF
*:----------------------------------------------------------------
*: Program file        : lfDummy
*: Program description : Dummy function to return .F.
*: For screen          : ICDDCNV.SPR
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : C200151
*:----------------------------------------------------------------
*: Passed Parameters   : None
*:----------------------------------------------------------------
*: Example             : =lfDummy()
*:----------------------------------------------------------------
FUNCTION lfDummy

RETURN (.F.)

*:----------------------------------------------------------------
*: Program file        : lfConvert
*: Program description : Main conversion function
*: For screen          : ICDDCNV.SPR
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : C200151
*:----------------------------------------------------------------
*: Passed Parameters   : None
*:----------------------------------------------------------------
*: Example             : =lfConvert()
*:----------------------------------------------------------------
FUNCTION lfConvert
PRIVATE llCanProc , lcStyle

llCanProc = .T.
=gfOpenFile(gcDataDir + "CODES", gcDataDir + "Ccode_no")
*--- cdefcode+cfld_name+ccode_no+cdiscrep+crltd_nam
SELECT (lcTempName)
PACK
GO TOP
DO CASE
  CASE EMPTY(lcSource)
    lcOvrWrit = "You have to select a source file. Cannot proceed."
    llCanProc = .F.
  CASE EOF()
    lcOvrWrit = "Source file cannot be empty. Cannot proceed."
    llCanProc = .F.
ENDCASE
llDummy= !llCanProc .AND. gfModalGen('INM00000B00000',.F.,.F.,.F.,lcOvrWrit)
IF llCanProc
  =gfOpenFile(gcDataDir + "apvendor", gcDataDir + "Vencode")
  =gfOpenFile(gcDataDir + "ICSegval", gcDataDir + "Segval")
  IF lcFileType = 'Style'
    =gfOpenFile(gcDataDir + "STYLE", gcDataDir + "STYLE")
    =gfOpenFile(gcDataDir + "STYDYE", gcDataDir + "STYDYE")
    =gfOpenFile(gcDataDir + "StyleUPC", gcDataDir + "StyleUPC")
    =gfOpenFile(gcDataDir + "BOM", gcDataDir + "BOM")
  ELSE
    =gfOpenFile(gcDataDir + "warehous", gcDataDir + "warehous")
  ENDIF
  SELECT (lcTempName)
  lnTotRe = RECCOUNT()-1
  SCAN FOR IIF(lcFileType = 'Style',RECNO()>1,.T.)
    SCATTER MEMVAR MEMO
    lnRecNo =  lnRecNo + 1
    *--- Parameter lcMessg , lnRecNo , lcFilNam
    lcMessage = "Converting "+IIF(lcFileType = 'Style',": Styles ",": Codes")
    =lfUpdFTher(lcMessage,lnRecNo,ALIAS(),lnTotRe)
    llDummy = IIF(lcFileType = 'Style',lfConvSty(),lfConvCod())
  ENDSCAN
  SELECT (lcTempRej)
  GO TOP
  IF EOF()
    APPEND BLANK
    REPLACE cError WITH "DONE"
  ENDIF
ENDIF

*:----------------------------------------------------------------
*: Program file        : lfUpdFTher
*: Program description : Update thermo funciotn
*: For screen          : ICDDCNV.SPR
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : C200151
*:----------------------------------------------------------------
*: Passed Parameters   : None
*:----------------------------------------------------------------
*: Example             : =lfUpdFTher()
*:----------------------------------------------------------------
FUNCTION lfUpdFTher
PARAMETER lcMessg , lnRecNo , lcFilNam , lnTotRecNo

PRIVATE lnPer
lnPer = (lnRecNo/lnTotRecNo) *100
lcPerSign  = SPACE(1) + "%" + SPACE(1)
IF lnPer > 0
  lcFTherm  = REPLICATE(CHR(219),lnRecNo*(lnThermSiz/lnTotRecNo))
ELSE
  lcFTherm  = ""
ENDIF
lcPersent  = ALLTRIM(STR(lnPer,6,2))+lcPerSign
SHOW GET lcFTherm
lcCurrFile = lcMessg
SHOW GET lcCurrFile
lcFPer = lcPersent
=lfRefresh()


*:----------------------------------------------------------------
*: Program file        : lfChkCon
*: Program description : Check the XLS files and create temp file funciton
*: For screen          : ICDDCNV.SPR
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : C200151
*:----------------------------------------------------------------
*: Passed Parameters   : None
*:----------------------------------------------------------------
*: Example             : =lfChkCon()
*:----------------------------------------------------------------
FUNCTION lfChkCon
PRIVATE llToRet

llToRet = .F.
IF lfCreTmp()
  llToRet = .T.
  CREATE TABLE (gcWorkDir+lcTempRej) ;
  (cError C(100))
ENDIF
RETURN llToRet


*:----------------------------------------------------------------
*: Program file        : lfCreTmp
*: Program description : Create temp file.
*: For screen          : ICDDCNV.SPR
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : C200151
*:----------------------------------------------------------------
*: Passed Parameters   : None
*:----------------------------------------------------------------
*: Example             : =lfCreTmp()
*:----------------------------------------------------------------
FUNCTION lfCreTmp

IF lcFileType = 'Style'
  CREATE TABLE (gcWorkDir+lcTempName) ;
  (UPC C(6), BrDIV C(2), TypCat C(4), Ses C(1),;
   SEQ_M C(2), Vendor C(2),PROD_DESC C(60),PROD_SHORT C(20),;
   Cost_price N(13,3),retail_Price N(13,3),Color C(3),code_Size C(4) , cDIVISION C(05) , LOCATION C(08) )
ELSE
  CREATE TABLE (gcWorkDir+lcTempName) ;
  (Type C(2), CODE C(6), DESC C(60))
ENDIF
=lfUpdTemp()

*:----------------------------------------------------------------
*: Program file        : lfUpdTemp
*: Program description : Update temp funciotn
*: For screen          : ICDDCNV.SPR
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : C200151
*:----------------------------------------------------------------
*: Passed Parameters   : None
*:----------------------------------------------------------------
*: Example             : =lfUpdTemp()
*:----------------------------------------------------------------
FUNCTION lfUpdTemp

SELECT (lcTempName)
ZAP
WAIT "Please wait. While creating DBF file." WINDOW NOWAIT
IF lcFileType = 'Style'
  APPEND FROM &lcSource TYPE XLS
ELSE
  APPEND FROM &lcSource FOR TYPE $ "SE|VE|LC|CL|BR|TC|MS" TYPE XLS
ENDIF
WAIT CLEAR
GO TOP
RETURN(IIF(lcFileType = 'Style',TYPE("UPC")="C",TYPE("TYPE")="C"))

*:----------------------------------------------------------------
*: Program file        : lfvFile
*: Program description : Valid function for the file popup
*: For screen          : ICDDCNV.SPR
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : C200151
*:----------------------------------------------------------------
*: Passed Parameters   : None
*:----------------------------------------------------------------
*: Example             : =lfvFile()
*:----------------------------------------------------------------
FUNCTION lfvFile

lcSource = ""
SHOW GET lcSource
IF USED(lcTempName)
  SELECT (lcTempName)
  ZAP
ENDIF

*:----------------------------------------------------------------
*: Program file        : lfChkSeg
*: Program description : Funciton to update style segment
*: For screen          : ICDDCNV.SPR
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : C200151
*:----------------------------------------------------------------
*: Passed Parameters   : None
*:----------------------------------------------------------------
*: Example             : =lfChkSeg()
*:----------------------------------------------------------------
FUNCTION lfChkSeg
PRIVATE lnOldAls

lnOldAls = SELECT(0)
*--- m.BrDIV+m.TypCat+m.Ses+m.SEQ_M+m.Vendor+"-"+m.Color+m.code_Size
SELECT ICSegval
IF !SEEK('1'+m.BrDIV)
  SELECT (lcTempRej)
  APPEND BLANK
  REPLACE cError WITH "Brand/Div: "+m.BrDIV+" - not found in segment file."
ENDIF
SELECT ICSegval
IF !SEEK('2'+m.TypCat)
  SELECT (lcTempRej)
  APPEND BLANK
  REPLACE cError WITH "Type/Category: "+m.TypCat+" - not found in segment file."
ENDIF
SELECT ICSegval
IF !SEEK('4'+m.SEQ_M)
  SELECT (lcTempRej)
  APPEND BLANK
  REPLACE cError WITH "Manual SEQ.: "+m.SEQ_M+" - not found in segment file."
ENDIF
SELECT ICSegval
IF !SEEK('5'+m.Vendor)
  SELECT (lcTempRej)
  APPEND BLANK
  REPLACE cError WITH "Vendor: "+m.Vendor+" - not found in segment file."
ENDIF
SELECT(lnOldAls)

*:----------------------------------------------------------------
*: Program file        : lfConvSty
*: Program description : Funciton to convert style.
*: For screen          : ICDDCNV.SPR
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : C200151
*:----------------------------------------------------------------
*: Passed Parameters   : None
*:----------------------------------------------------------------
*: Example             : =lfConvSty()
*:----------------------------------------------------------------
FUNCTION lfConvSty

lcStyle = PADR(m.BrDIV,2)+PADR(m.TypCat,4)+PADR(m.Ses,1)+;
          PADR(m.SEQ_M,2)+PADR(m.Vendor,2)+"-"+PADR(m.Color,3)+"-"+PADR(m.code_Size,3)
*--- cdefcode+cfld_name+ccode_no+cdiscrep+crltd_nam
SELECT CODES
IF SEEK('N'+"SEASON    "+m.Ses) .AND. SEEK('N'+"COLOR     "+PADR(m.Color,3))
  =lfChkSeg()
  SELECT STYLE
  IF !SEEK(lcStyle)
    APPEND BLANK
  ENDIF
*cDivision  WITH PADR(m.BrDIV,2)
  GATHER MEMVAR MEMO
  REPLACE STYLE      WITH lcStyle,;
          DESC1      WITH m.PROD_DESC,;
          DESC       WITH m.PROD_SHORT,;
          nICost1    WITH m.Cost_price,;
          Vendor     WITH m.Vendor,;
          nSugRetPri WITH m.retail_Price,;
          Pricea     WITH m.retail_Price,;
          Ave_Cost   WITH m.Cost_price,;
          SCALE      WITH PADR(m.code_Size,3),;
          cDefWare   WITH m.Location,;
          Season     WITH m.Ses,;
          Status     WITH "A",;
          Link_Code  WITH "DEFDEF",;
          cDye_Flg   WITH "N",;
          cStyGrade  WITH "1",;
          cPriceCur  WITH gcBaseCurr,;
          cdutyCur   WITH gcBaseCurr,;
          lInvSty    WITH .T.,;
          lDetCost   WITH .F.,;
          MAKE       WITH .F.,;
          cStyMajor  WITH PADR(m.BrDIV,2)+PADR(m.TypCat,4)+PADR(m.Ses,1)+PADR(m.SEQ_M,2)+PADR(m.Vendor,2)
  SELECT STYDYE
  IF !EMPTY(m.Location) .AND. !SEEK(lcStyle+m.Location)
    APPEND BLANK
    REPLACE STYLE     WITH lcStyle,;
            Desc      WITH m.PROD_SHORT,;
            cWareCode WITH ALLTRIM(m.Location),;
            Gl_Link   WITH "DEFDEF",;
            Ave_Cost  WITH m.Cost_price
  ENDIF
  =lfUpdDiv(m.cDivision)
  =lfUpdUpc()
  =lfUpBom()
ELSE
  SELECT CODES
  IF !SEEK('N'+"SEASON    "+m.Ses)
    SELECT (lcTempRej)
    APPEND BLANK
    REPLACE cError WITH lcStyle+": Season code not found in CODES file"
  ELSE
    SELECT (lcTempRej)
    APPEND BLANK
    REPLACE cError WITH lcStyle+": Color code not found in CODES file"
  ENDIF
ENDIF

*:----------------------------------------------------------------
*: Program file        : lfConvCod
*: Program description : Funciton to convert Codes.
*: For screen          : ICDDCNV.SPR
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : C200151
*:----------------------------------------------------------------
*: Passed Parameters   : None
*:----------------------------------------------------------------
*: Example             : =lfConvCod()
*:----------------------------------------------------------------
FUNCTION lfConvCod

DO CASE
  CASE TYPE = "SE"  &&-- Season Code
    =lfUpdCode("SE")
  CASE TYPE = "CL"  &&-- Color Code
    =lfUpdCode("CL")
  CASE TYPE = "LC"  &&-- Location
    =lfUpdLoc()
  OTHERWISE
    =lfUpdSeg(TYPE,m.Code,m.Desc)
ENDCASE

*:----------------------------------------------------------------
*: Program file        : lfUpdSeg
*: Program description : Update segment.
*: For screen          : ICDDCNV.SPR
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : C200151
*:----------------------------------------------------------------
*: Passed Parameters   : None
*:----------------------------------------------------------------
*: Example             : =lfUpdSeg()
*:----------------------------------------------------------------
FUNCTION lfUpdSeg
PARAMETER lcTyp , lcCod , lcDes
PRIVATE lnOldAls

lnOldAls = SELECT(0)
SELECT ICSegval
DO CASE
  CASE lcTyp = "BR"  &&-- Segment  1
    IF !SEEK('1'+lcCod)
      APPEND BLANK
    ENDIF
    REPLACE cIsegNo  WITH '1',;
            cIsegVal WITH lcCod,;
            cisgvalsd WITH lcDes
  SELECT CODES
  *--- cdefcode+cfld_name+ccode_no+cdiscrep+crltd_nam
  *IF !SEEK('N'+"CDIVISION "+lcCod)
  *  APPEND BLANK  
  *  REPLACE CDEFCODE  WITH 'N',;
  *          CFLD_NAME WITH "CDIVISION ",;
  *          cDiscRep  WITH lcDes,;
  *          CCODE_NO  WITH lcCod,;
  *          CRLTFIELD WITH 'N',;
  *          CRLTD_NAM WITH "",;
  *          CRLTD_TYP WITH ""
  *  IF !SEEK("D"+"CDIVISION ")
  *    APPEND BLANK  
  *    REPLACE CDEFCODE  WITH 'D',;
  *            CFLD_NAME WITH "CDIVISION ",;
  *            cDiscRep  WITH lcDes,;
  *            CCODE_NO  WITH lcCod,;
  *            CRLTFIELD WITH 'N',;
  *            CRLTD_NAM WITH "",;
  *            CRLTD_TYP WITH ""
  *  ENDIF
  *  *--- RELATED FIELDS
  *  APPEND BLANK
  *  REPLACE CDEFCODE  WITH 'N',;
  *          CFLD_NAME WITH "CDIVISION ",;
  *          cDiscRep  WITH lcDes,;
  *          CCODE_NO  WITH lcCod,;
  *          CRLTFIELD WITH 'Y',;
  *          CRLTD_NAM WITH "CUPCMAN",;
  *          CRLTD_TYP WITH "C"
  *  APPEND BLANK
  *  REPLACE CDEFCODE  WITH 'N',;
  *          CFLD_NAME WITH "CDIVISION ",;
  *          cDiscRep  WITH lcDes,;
  *          CCODE_NO  WITH lcCod,;
  *          CRLTFIELD WITH 'Y',;
  *          CRLTD_NAM WITH "DIVGROUP",;
  *          CRLTD_TYP WITH "C"
  *  APPEND BLANK
  *  REPLACE CDEFCODE  WITH 'N',;
  *          CFLD_NAME WITH "CDIVISION ",;
  *          cDiscRep  WITH lcDes,;
  *          CCODE_NO  WITH lcCod,;
  *          CRLTFIELD WITH 'Y',;
  *          CRLTD_NAM WITH "DIVLNAME",;
  *          CRLTD_TYP WITH "C"
  *ENDIF
  CASE lcTyp = "TC"  &&-- Segment  2
    IF !SEEK('2'+lcCod)
      APPEND BLANK
    ENDIF
    REPLACE cIsegNo  WITH '2',;
            cIsegVal WITH lcCod,;
            cisgvalsd WITH lcDes
  CASE lcTyp = "MS"  &&-- Segment  4
    IF !SEEK('4'+lcCod)
      APPEND BLANK
    ENDIF
    REPLACE cIsegNo  WITH '4',;
            cIsegVal WITH lcCod,;
            cisgvalsd WITH lcDes
  CASE lcTyp = "VE"  &&-- Segment  5
    IF !SEEK('5'+lcCod)
      APPEND BLANK
    ENDIF
    REPLACE cIsegNo  WITH '5',;
            cIsegVal WITH lcCod,;
            cisgvalsd WITH lcDes
    =lfUpdVend(lcCod,lcDes)
ENDCASE
SELECT(lnOldAls)

*:----------------------------------------------------------------
*: Program file        : lfUpdCode
*: Program description : Funciton to convert Codes file.
*: For screen          : ICDDCNV.SPR
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : C200151
*:----------------------------------------------------------------
*: Passed Parameters   : None
*:----------------------------------------------------------------
*: Example             : =lfUpdCode()
*:----------------------------------------------------------------
FUNCTION lfUpdCode
PARAMETER lcCodTyp

PRIVATE lnOldAls
lnOldAls = SELECT(0)
SELECT CODES
*--- cdefcode+cfld_name+ccode_no+cdiscrep+crltd_nam
IF !SEEK('N'+IIF(lcCodTyp="SE","SEASON    ","COLOR     ")+m.Code)
  APPEND BLANK  
  REPLACE CDEFCODE  WITH 'N',;
          CFLD_NAME WITH IIF(lcCodTyp="SE","SEASON","COLOR"),;
          cDiscRep  WITH m.DESC,;
          CCODE_NO  WITH m.Code,;
          CRLTFIELD WITH 'N',;
          CRLTD_NAM WITH "",;
          CRLTD_TYP WITH ""
  IF !SEEK("D"+IIF(lcCodTyp="SE","SEASON    ","COLOR     "))
    APPEND BLANK  
    REPLACE CDEFCODE  WITH 'D',;
            CFLD_NAME WITH IIF(lcCodTyp="SE","SEASON","COLOR"),;
            cDiscRep  WITH m.DESC,;
            CCODE_NO  WITH m.Code,;
            CRLTFIELD WITH 'N',;
            CRLTD_NAM WITH "",;
            CRLTD_TYP WITH ""
  ENDIF
  IF lcCodTyp = "CL"
  *--- CLRLNAME  , CNRFCODE  
    APPEND BLANK
    REPLACE CDEFCODE  WITH 'N',;
            CFLD_NAME WITH "COLOR",;
            cDiscRep  WITH m.DESC,;
            CCODE_NO  WITH m.Code,;
            CRLTFIELD WITH 'Y',;
            CRLTD_NAM WITH "CLRLNAME",;
            CRLTD_TYP WITH "C"
    APPEND BLANK
    REPLACE CDEFCODE  WITH 'N',;
            CFLD_NAME WITH "COLOR",;
            cDiscRep  WITH m.DESC,;
            CCODE_NO  WITH m.Code,;
            CRLTFIELD WITH 'Y',;
            CRLTD_NAM WITH "CNRFCODE",;
            cRltd_Vlu WITH m.Code,;
            CRLTD_TYP WITH "C"
    IF !SEEK("D"+IIF(lcCodTyp="SE","SEASON    ","COLOR     "))
      APPEND BLANK
      REPLACE CDEFCODE  WITH 'D',;
              CFLD_NAME WITH "COLOR",;
              cDiscRep  WITH m.DESC,;
              CCODE_NO  WITH m.Code,;
              CRLTFIELD WITH 'Y',;
              CRLTD_NAM WITH "CLRLNAME",;
              CRLTD_TYP WITH "C"
      APPEND BLANK
      REPLACE CDEFCODE  WITH 'D',;
              CFLD_NAME WITH "COLOR",;
              cDiscRep  WITH m.DESC,;
              CCODE_NO  WITH m.Code,;
              CRLTFIELD WITH 'Y',;
              CRLTD_NAM WITH "CNRFCODE",;
              cRltd_Vlu WITH m.Code,;
              CRLTD_TYP WITH "C"
    ENDIF
    SELECT(lnOldAls)
  ENDIF
ENDIF
SELECT(lnOldAls)

*:----------------------------------------------------------------
*: Program file        : lfUpdLoc
*: Program description : Funciton to convert warehouse file.
*: For screen          : ICDDCNV.SPR
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : C200151
*:----------------------------------------------------------------
*: Passed Parameters   : None
*:----------------------------------------------------------------
*: Example             : =lfUpdLoc()
*:----------------------------------------------------------------
FUNCTION lfUpdLoc
PRIVATE lnOldAls

lnOldAls = SELECT(0)
SELECT warehous
IF !SEEK(m.Code)
  APPEND BLANK
  REPLACE cwarecode  WITH m.Code,;
          cDesc      WITH m.Desc,;
          cCont_Code WITH gcContCode,;
          lMatInv    WITH .T.,;
          lStyInv    WITH .T.,;
          gl_Link    WITH "DEFDEF"
ENDIF
SELECT(lnOldAls)

*:----------------------------------------------------------------
*: Program file        : lfUpdUpc
*: Program description : Funciton to convert StyleUPC file.
*: For screen          : ICDDCNV.SPR
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : C200151
*:----------------------------------------------------------------
*: Passed Parameters   : None
*:----------------------------------------------------------------
*: Example             : =lfUpdUpc()
*:----------------------------------------------------------------
FUNCTION lfUpdUpc

SELECT StyleUPC
*--- style+size
IF !SEEK(lcStyle+SUBSTR(m.code_Size,4,1))
  APPEND BLANK
ENDIF
REPLACE STYLE    WITH lcStyle,;
        Size     WITH SUBSTR(m.code_Size,4,1),;
        cUPCNUM1 WITH UPC

*:----------------------------------------------------------------
*: Program file        : lfUpBom
*: Program description : Funciton to update bom file.
*: For screen          : ICDDCNV.SPR
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : C200151
*:----------------------------------------------------------------
*: Passed Parameters   : None
*:----------------------------------------------------------------
*: Example             : =lfUpBom()
*:----------------------------------------------------------------
FUNCTION lfUpBom

PRIVATE lnOldAls , lcItemMAjor

lnOldAls = SELECT(0)
*--- citmmajor+typ+citmmask+mfgcode+item+iclr
SELECT BOM
lcItemMAjor = PADR(PADR(m.BrDIV,2)+PADR(m.TypCat,4)+PADR(m.Ses,1)+PADR(m.SEQ_M,2)+PADR(m.Vendor,2),20)
IF !SEEK(lcItemMAjor+'1'+PADR(lcStyle,20)+"*1")
  APPEND BLANK
ENDIF
REPLACE citmmajor  WITH lcItemMAjor,;
        Typ        WITH '1',;
        citmmask   WITH lcStyle,;
        mfgCode    WITH "*1",;
        UOM        WITH "EAC",;
        untCost    WITH m.Cost_price,;
        nBomTotQty WITH 1,;
        TotCost    WITH m.Cost_price,;
        cCatgTyp   WITH "P"
SELECT(lnOldAls)

*:----------------------------------------------------------------
*: Program file        : lfUpdVend
*: Program description : Funciton to convert vendor file.
*: For screen          : ICDDCNV.SPR
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : C200151
*:----------------------------------------------------------------
*: Passed Parameters   : None
*:----------------------------------------------------------------
*: Example             : =lfUpdVend()
*:----------------------------------------------------------------
FUNCTION lfUpdVend
PARAMETER lcCode , lcDesc
PRIVATE lnOldAls
lnOldAls = SELECT(0)
SELECT APVendor

IF !SEEK(lcCode)
  APPEND BLANK
  REPLACE cVendCode WITH lcCode,;
          cVenComp  WITH lcDesc
ENDIF
SELECT(lnOldAls)

FUNCTION lfUpdDiv
PARAMETER lcDivision
PRIVATE lnOldAls

lnOldAls = SELECT(0)
SELECT CODES
IF !SEEK('N'+"CDIVISION "+lcDivision)
  APPEND BLANK
  REPLACE CDEFCODE  WITH 'N',;
          CFLD_NAME WITH "CDIVISION",;
          cDiscRep  WITH "Division: "+lcDivision,;
          CCODE_NO  WITH lcDivision,;
          CRLTFIELD WITH 'N',;
          CRLTD_NAM WITH "",;
          CRLTD_TYP WITH ""
  *--- CUPCMAN   , DIVGROUP  ,DIVLNAME
  APPEND BLANK
  REPLACE CDEFCODE  WITH 'N',;
          CFLD_NAME WITH "CDIVISION",;
          cDiscRep  WITH "Division: "+lcDivision,;
          CCODE_NO  WITH lcDivision,;
          CRLTFIELD WITH 'Y',;
          CRLTD_NAM WITH "CUPCMAN",;
          CRLTD_TYP WITH "C"
  APPEND BLANK
  REPLACE CDEFCODE  WITH 'N',;
          CFLD_NAME WITH "CDIVISION",;
          cDiscRep  WITH "Division: "+lcDivision,;
          CCODE_NO  WITH lcDivision,;
          CRLTFIELD WITH 'Y',;
          CRLTD_NAM WITH "DIVGROUP",;
          CRLTD_TYP WITH "C"

  APPEND BLANK
  REPLACE CDEFCODE  WITH 'N',;
          CFLD_NAME WITH "CDIVISION",;
          cDiscRep  WITH "Division: "+lcDivision,;
          CCODE_NO  WITH lcDivision,;
          CRLTFIELD WITH 'Y',;
          CRLTD_NAM WITH "DIVLNAME",;
          CRLTD_TYP WITH "C"

ENDIF
IF !SEEK('D'+"CDIVISION ")
  APPEND BLANK
  REPLACE CDEFCODE  WITH 'D',;
          CFLD_NAME WITH "CDIVISION",;
          cDiscRep  WITH "Division: "+lcDivision,;
          CCODE_NO  WITH lcDivision,;
          CRLTFIELD WITH 'N',;
          CRLTD_NAM WITH "",;
          CRLTD_TYP WITH ""
ENDIF
SELECT(lnOldAls)