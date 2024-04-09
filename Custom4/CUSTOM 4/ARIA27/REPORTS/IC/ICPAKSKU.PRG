*:**************************************************************************
*: Program file  : ICPAKSKU
*: Program desc. : PACK/SKU REPORT
*: Date          : 05/09/1999
*: System        : Aria Advantage Series.
*: Module        : INVENTORY CONTROL (IC)
*: Developer     : Sameh (SSE)
*:**************************************************************************
*: Calls : 
*:    Procedures : CUSBROWM , gfDispRe
*:    Functions  : lfvAcct() , lfAdd2Tmp() , lfGetTmpl() , lfQtyStr() ,
*:                 
*:**************************************************************************
*: Passed Parameters  : None
*:**************************************************************************
*: Notes   : .... 
*:**************************************************************************
*: Example : DO ICPAKSKU
*:**************************************************************************
*: This Program is due to  E301218,1...
*:**************************************************************************
*B603549,1 HESHAM 04/04/2000 
*B127041,1 EIH    04/20/2005 The Pack report doesn't print the pack ID
*:**************************************************************************

lcAccount = laOGFxFlt[1,6]  && Variable to store 1st Fixed Filter of OG

*-- Check existance of pack/sku for the selected account in SPCK_LIN File.
SELECT SPCK_LIN
*-- 
IF !EMPTY(lcAccount) .AND. !(SEEK('S'+lcAccount) .OR. SEEK('P'+lcAccount))
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
  RETURN

ELSE  &&  Else If (lcAccount is empty) or (lcAccount not empty and Seek
      &&  was done successfully) Build tmp file. and start to collect data                      
  COPY STRUCTURE TO (gcWorkDir+lcTempFile)
  =gfOpenFile(gcWorkDir+lcTempFile,'','EX')

  *-- make relation from SPCK_LIN to STYLE and from STYLE to SCALE 
  *-- to get the proper data and fill the Temp File

  *--e.g. SPCK_LIN
  *              |__
  *                 STYLE	
  *					    |__	
  *                        SCALE
  
  *-- make the relation [Begin.]
  SELECT STYLE
  SET RELATION TO 'S'+Scale INTO SCALE ADDITIVE

  SELECT SPCK_LIN
  SET RELATION TO STYLE INTO STYLE ADDITIVE
  *-- make the relation [End.]

  *-- if Account is empty in SPCK_LIN scan for all the file (loop) 
  IF EMPTY(lcAccount)
    SCAN  
      =lfAdd2Tmp()  && fills temp. file with data matching criteria.
    ENDSCAN
  ELSE  && Else If there is Account in SPCK_LIN

    *-- use full index expression 
    SEEK 'P'+lcAccount
    SCAN WHILE Type+Account+Pack_id+Style='P'+lcAccount
      =lfAdd2Tmp()  && fills temp. file with data matching criteria.
    ENDSCAN

    *-- use full index expression 
    SEEK 'S'+lcAccount
    SCAN WHILE Type+Account+Pack_id+Style='S'+lcAccount
      =lfAdd2Tmp()  && fills temp. file with data matching criteria.
    ENDSCAN

  ENDIF
  *-- End If of scan SPCK_LIN for Specified Account or ALL Accounts(empty)

  *-- break relation between SPCK_LIN and STYLE only leaving relation 
  *-- between STYLE and SCALE as it is
  SET RELATION TO
  SELECT (lcTempFile)

  *-- creating new index in the Temp. File for Report 
  INDEX ON Account+IIF(Type='S','1','2')+Style TAG (lcTempFile)
  GO TOP

  *--Start the report.
  SELECT (lcTempFile)
  *-- Set the relations for the report between the Temp. File and 
  *-- Customer , Style , Scale as follows
  *-- e.g. (lcTempFile)
  *--                 |____
  *--                 |    Customer  
  *--                 |____
  *--                      Style
  *--                          |____
  *--                               Scale
      
  SET RELATION TO 'M'+Account INTO CUSTOMER ADDITIVE
  SET RELATION TO Style INTO STYLE  ADDITIVE
  GO TOP

  *-- Main Report variables
  R_TITLE = ''      && Variable to store Report Title
  R_WIDTH = 'N'     && Variable to store Report Width (Narrow)
  PageNo   = 0      && Variable to store Report Page No.
  xTotPack = 0      && Variable to store Report Total QTYs for every Style

  DO gfDispRe WITH EVAL('lcRpName')

  *-- close The Temporary file  [Begin.]
  IF USED  (lcTempFile)  
    SELECT (lcTempFile)
    USE 
  ENDIF
  *-- close The Temporary file  [End.]

  *-- Erase The Temporary file created [Begin.]
  ERASE (gcWorkDir+lcTempFile+'.DBF')
  ERASE (gcWorkDir+lcTempFile,'.CDX')  
  *-- Erase The Temporary file created [End.]

ENDIF
*-- end of report

*!**************************************************************************
*! Name      : lfAdd2Tmp
*! Developer : Sameh (SSE)
*! Date      : 05/09/1999
*! Purpose   : Add new record in tmp file.
*!**************************************************************************
*! Called from : Main PRG
*!**************************************************************************
*! Calls       : 
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : =lfAdd2Tmp()
*!**************************************************************************
*
FUNCTION lfAdd2Tmp

SCATTER TO laFields   && lafields Array to store the SPCK_LIN Fields
SELECT (lcTempFile)
APPEND BLANK
GATHER FROM laFields  && adding lafields Array to Temp File

*-- If Type 'S' (Sku) 
IF TYPE = 'S'
  *-- If Prepak is empty (no Prepak)
  IF EMPTY(PrePak)
    *-- 
    FOR I = 1 TO 8
      Z=STR(I,1)
      IF QTY&Z = 1
        EXIT
      ENDIF
    ENDFOR
    REPLACE Sku WITH SCALE.Sz&Z
  ELSE
    REPLACE Sku WITH 'PREPACK'
  ENDIF
ENDIF  && End If Type 'S'
*-- end of lfAdd2Tmp

*!**************************************************************************
*! Name      : lfGetTmpl
*! Developer : Sameh (SSE)
*! Date      : 05/09/1999
*! Purpose   : Returns the template pack id from SKUTMPL File. 
*!**************************************************************************
*! Called from : IcpakSku.FRX
*!**************************************************************************
*! Calls       : 
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : =lfGetTmpl()
*!**************************************************************************
*
FUNCTION lfGetTmpl

xtotpack = 0          && Variable to store Report Total QTYs for every Style
lcSPackID = ''        && var. to store the pack id Title  
lcAlias   = SELECT()  && var. to store the current Alias

*-- get pack_id title from SKUTMPL File
SELECT SKUTMPL

*-- if pack (Type='P') and customer has SkuTmpl   
IF SEEK('P'+CUSTOMER.SkuTmpl)  
  lcFld1=ALLTRIM(Field1)
  lcFld2=ALLTRIM(Field2)
  
  *B603549,1 Hesham (Start)  
  *  lcSpackid=&lcFld1+' : '+SUBSTR(&lcTempFile..Sku,1,Len1)+'    '+;
  *B127041,1 EIH    04/20/2005 The Pack report doesn't print the pack ID [BEGIN]
  *IIF(Len2<>0,&lcFld2+' : '+SUBSTR(&lcTempFile..Sku,Len1+1,Len2),'')
  *lcSpackid= lcFld1+' : '+SUBSTR(&lcTempFile..Sku,1,Len1)+'    '+;
  *IIF(Len2<>0,lcFld2+' : '+SUBSTR(&lcTempFile..Sku,Len1+1,Len2),'')

  lcSpackid= lcFld1+' : '+SUBSTR(&lcTempFile..PACK_ID,1,Len1)+'    '+;
  IIF(Len2<>0,lcFld2+' : '+SUBSTR(&lcTempFile..PACK_ID,Len1+1,Len2),'')

  *B127041,1 EIH    04/20/2005 [END]
  *B603549,1 Hesham (End)  
  
ELSE  && Else If customer does not have SkuTmpl 
  *B127041,1 EIH    04/20/2005 The Pack report doesn't print the pack ID [BEGIN]
  *lcSPackID = 'Pack id : ' + &lcTempFile..Sku
  lcSPackID = 'Pack id : ' + &lcTempFile..PACK_ID
  *B127041,1 EIH    04/20/2005 [END]
  
ENDIF

SELECT(lcAlias)  && restore Saved Alias

RETURN lcSPackID
*-- end of lfGetTmpl.

*!**************************************************************************
*! Name      : lfQtyStr
*! Developer : Sameh (SSE)
*! Date      : 05/09/1999
*! Purpose   : Returns QTY line string in report. 
*!**************************************************************************
*! Called from : IcpakSku.FRX
*!**************************************************************************
*! Calls       : 
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : =lfQtyStr()
*!**************************************************************************
*
FUNCTION lfQtyStr
lcQtyTxt = ''  && variable to store the Scale

*-- this loop collects all Scales in one string variable separated by SPACES
FOR I=1 TO SCALE.Cnt
  Z=STR(I,1)
  lcQtyTxt=lcQtyTxt+STR(Qty&Z,6)
ENDFOR
*-- end of for loop

lcQtyTxt = lcQtyTxt + SPACE(48-LEN(lcQtyTxt)) + STR(TotQty,7)
xTotPack = xTotPack + TotQty
RETURN (lcQtyTxt)
*-- end of lfQtyStr.


*!**************************************************************************
*! Name      : lfvAcct
*! Developer : Sameh (SSE)
*! Date      : 05/09/1999
*! Purpose   : Validation function for the Customer Account field
*!**************************************************************************
*! Called from : Customer Account field [Option Grid]
*!**************************************************************************
*! Calls       : CusBrowM()
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : =lfvAcct()
*!**************************************************************************
*
FUNCTION lfvAcct
PRIVATE lcObjNam , lcObjVal

lcObjNam = SYS(18)   && Var. store the name of the memory variable
                     && used to create the current GET field
lcObjVal = EVAL(SYS(18)) && Var. store the value of the current GET field

*-- IF The user want to Browse or if the Account entered is not in the file
IF !EMPTY(lcObjVal) .AND. !SEEK('M'+lcObjVal , 'CUSTOMER')
  llBrowse = .T.
  lcAccount = lcObjVal
  DO CUSBROWM WITH lcAccount
  lcObjVal = lcAccount
  llBrowse = .F.
ENDIF    

&lcObjNam = lcObjVal
*-- end of lfvAcct.
