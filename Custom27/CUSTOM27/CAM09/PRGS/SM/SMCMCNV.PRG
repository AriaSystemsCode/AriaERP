*:****************************************************************************************
*: Program file  : SMCMCNV.PRG
*: Program desc. : Custom program to Convert the data from MAS90 to Aria for CAM09 ( Camp David )
*: Date          : 10/02/2005
*: System        : Aria Advantage Series.
*: Module        : SM2
*: Developer     : TMI - TAREK MOHAMED IBRAHIM
*: Purpose       : Convert the data from MAS09 to Aria.
*: Entry #       : C # 126871,1
*:****************************************************************************************
*: Modifications : 
*: C126871,2 WSH 03/22/2006 Add other Codes to the Codes file and modify some Master files.
*T20060920.0009,1 TMI 09/21/2006 adding the import customer contacts
*                                fixing import ventor contacts
*:****************************************************************************************

*!* 
*!* Program steps
*!* 
*!* 
*!* open the option grid
*!* select the files to import data from 
*!* 
*!* processing
*!*   check the files are saved in xls 4 format
*!*   note : if the file has more than 32000 lines then separate it to two files or more as required
*!*   
*!*   read the data
*!*     if the data can not be imported report a log file with all the errors encountered
*!*     if the data imported , report a log with errors in converted data if any
*!*     

*- Check if this is a valid Co.
IF !lfValidCo()
  RETURN
ENDIF

*- Initialize needed variables
STORE '' TO lcClrSpr,lcLogFile,lcSvMem

* Samah
* STORE '' TO lcRpCust,lcImCust  ,;
            lcRpCstShp,lcImCstShp,;
            lcRpAriCst,lcImAriCst,;
            lcRpSlsrep,lcImSlsrep,;
            lcRpColors,lcImColors,;
            lcRpTerms ,lcImTerms ,;
            lcRpWareH ,lcImWareH ,;
            lcRpPrdlin,lcImPrdlin,;
            lcRpVendor,lcImVendor,;
            lcRpItmVnd,lcImItmVnd,;
            lcRpWhDMtx,lcImWhDMtx,;
            lcRpInvMst,lcImInvMst,;
            lcRpMstMrx,lcImMstMrx

STORE '' TO lcRpCust  ,lcImCust  ,;
            lcRpCSRep, lcImCSRep ,;
            lcRpCstCnt,lcImCstCnt,;
            lcRpCstShp,lcImCstShp,;
            lcRpAriCst,lcImAriCst,;
            lcRpSlsrep,lcImSlsrep,;
            lcRpColors,lcImColors,;
            lcRpTerms ,lcImTerms ,;
            lcRpWareH ,lcImWareH ,;
            lcRpPrdlin,lcImPrdlin,;
            lcRpVendor,lcImVendor,;
            lcRpItmVnd,lcImItmVnd,;
            lcRpWhDMtx,lcImWhDMtx,;
            lcRpInvMst,lcImInvMst,;
            lcRpMstMrx,lcImMstMrx
*Samah

*C126871,2 WSH [Start]
STORE '' TO lcRpTrmRef,lcImTrmRef,;
            lcRPShpRef,lcImShpRef,;
            lcRPRetRef,lcImRetRef,;
            lcRpVenCnt,lcImVenCnt,;
            lcRpVenMem,lcImVenMem,;
            lcRpRepCom,lcImRepCom,;
            lcRpDivisn,lcImDivisn
*C126871,2 WSH [End]

*WSH
STORE '' TO lcRpCstMem,lcImCstMem
*WSH

STORE .F. TO llZap

*- Restore variables from temp file saved with the current user and active company
STORE '' TO lcVar1,lcVar2,lcVar3,lcVar4,lcVar6,lcVar7,lcVar8,lcVar9,lcVar10,lcVar11,lcVar12,lcVar13,lcVar14
STORE .F. TO lcVar5

*C126871,2 WSH [Start]
STORE '' TO lcVar15, lcVar16, lcVar17, lcVar18, lcVar19, lcVar20, lcVar21
STORE '' TO lcMas90Fld

lcMas90Fld = "M.CMAS90CODE"
*C126871,2 WSH [End]

*WSH
STORE '' TO lcVar22, lcVar23, lcVar24
*WSH

DIMENSION laTmpFl[1]  && An array that holds temp file names to erase them after end the program
laTmpFl[1] = ''
STORE 0 TO lnTmpCnt  && Temp counter

*- Define temp names variables
lcLogFile  = lfTmpFlNm()

*- Create needed temp files
=lfCrtTmps()

*-Open needed files for convert
=lfOpenFiles()  

IF lfError()
  =lfLogReprt()  && *- Show final log report  
  =lfErase()   && *- Erase not needed temp files
  RETURN
ENDIF 

*- select the file variables names in an array to deny selecting a file twice
DIMENSION laVars[1]
laVars[1] = ''
SELECT SUBSTR(MFLD_NAME,1,10) ;
   FROM (gcSysHome+'SYREPUVR') ;
   WHERE CREP_ID = 'SMCMCNV' AND SUBSTR(MFLD_NAME,1,10) = 'lcRp' ;
   INTO ARRAY laVars

lnTryAgain = 1
DO WHILE lnTryAgain > 0

  DIMENSION laTmpFl[1]  && An array that holds temp file names to erase them after end the program
  laTmpFl[1] = ''
  STORE 0 TO lnTmpCnt  && Temp counter
  
  *- Define temp names variables
  lcLogFile  = lfTmpFlNm()
  
  *- Create needed temp files
  =lfCrtTmps()
  
  lcSvMem = gcWorkDir + ALLTRIM(gcACT_COMP) + 'Vars' + '.MEM'
  IF FILE(lcSvMem)
    RESTORE FROM (lcSvMem) ADDITIVE  
    
    lcVar1 = lcRpCust
    lcVar2 = lcRpCstShp
    lcVar3 = lcRpAriCst
    lcVar4 = lcRpSlsrep
    lcVar6 = lcRpColors
    lcVar7 = lcRpWareH
    lcVar8 = lcRpPrdlin
    lcVar9 = lcRpVendor
    lcVar10 = lcRpItmVnd
    lcVar11 = lcRpWhDMtx
    lcVar12 = lcRpInvMst
    lcVar13 = lcRpMstMrx
    lcVar14 = lcRpTerms
    
    *C126871,2 WSH [Start]
    lcVar15 = lcRpTrmRef
    lcVar16 = lcRpShpRef
    lcVar17 = lcRpRetRef
    lcVar18 = lcRpVenCnt
    lcVar19 = lcRpVenMem
    lcVar20 = lcRpRepCom
    lcVar21 = lcRpDivisn
    *C126871,2 WSH [End]
    
    *Samah
    lcVar22 = lcRpCSRep
    lcVar23 = lcRpCstCnt
    *Samah
    
    *WSH
    lcVar24 = lcRpCstMem
    *WSH
    
  ENDIF  
  
  SELECT &lcLogFile
  ZAP
  
  *-Call Option Grid
  lcExpr = gfOpGrid('SMCMCNV' , .T.)
  
  *- Save variables
  SAVE TO (lcSvMem) ALL LIKE l?Rp*
  
  IF EMPTY(lcExpr)
    lnTryAgain = 0
    LOOP
  ENDIF

  =IIF( gfGetMemvar('M_REP_COMM')<>'D' , lfLogData('Sales rep commission does not base on divisions',0,.T.) , '' )    

  =lfChkImp('lcRpCust'   ,'"Customer master"')
  =lfChkImp('lcRpCstShp' ,'"Customer Ship to Addresses"')
  =lfChkImp('lcRpAriCst' ,'"Aria Customer List"')
  *Samah
  =lfChkImp('lcRpCSRep' , '"Customer UDF"')
  =lfChkImp('lcRpCstCnt', '"Customer Contacts"')
  *Samah
  =lfChkImp('lcRpSlsrep' ,'"Salesperson Master"')
  =lfChkImp('lcRpItmVnd' ,'"Inventory Item Vendor"')
  =lfChkImp('lcRpWhDMtx' ,'"Inventory Item WH Detail Matrix"')
  =lfChkImp('lcRpInvMst' ,'"Inventory Master"')
*  =lfChkImp('lcRpMstMrx' ,'"Inventory Master Matrix"')
  =lfChkImp('lcRpPrdlin' ,'"Scales (Product Line and Size Scale)"') 
  =lfChkImp('lcRpColors' ,'"Colors"') 
  =lfChkImp('lcRpTerms'  ,'"Terms Code Master"') 
  =lfChkImp('lcRpWareH'  ,'"Warehouses"') 
  =lfChkImp('lcRpVendor' ,'"Vendor Master"') 
  
  *C126871,2 WSH Add new files and check all wrong files once... [Start] 
  *=IIF(!lfError() , lfImp('lcRpCust'   ,'Customer master','ConsumerUID',16)                  , '' )
  *=IIF(!lfError() , lfImp('lcRpCstShp' ,'Customer Ship to Addresses','ShipToCountryCode',12) , '' )
  *=IIF(!lfError() , lfImp('lcRpAriCst' ,'Aria Customer List','Aria Customer Number',4)       , '' )
  *=IIF(!lfError() , lfImp('lcRpSlsrep' ,'Salesperson Master','SalespersonNumber',2)          , '' )
  *=IIF(!lfError() , lfImp('lcRpColors' ,'Colors','50/50 Crew Neck Fleece',2)                 , '' )
  *=IIF(!lfError() , lfImp('lcRpTerms'  ,'Terms Code Master','DaysBeforeDiscDue',5)           , '' )
  *=IIF(!lfError() , lfImp('lcRpWareH'  ,'Warehouses','WhseDescription',2)                    , '' )
  *=IIF(!lfError() , lfImp('lcRpPrdlin' ,'Scales (Product Line and Size Scale)','Size Scale ( up to 8 sizes across) - 1 alpha char',1) , '' )
  *=IIF(!lfError() , lfImp('lcRpVendor' ,'Vendor Master','VendorName',3)                      , '' )
  *=IIF(!lfError() , lfImp('lcRpItmVnd' ,'Inventory Item Vendor','VendorWarrantyCode',9)      , '' )
  *=IIF(!lfError() , lfImp('lcRpWhDMtx' ,'Inventory Item WH Detail Matrix','BinLocation',5)   , '' )
  *=IIF(!lfError() , lfImp('lcRpInvMst' ,'Inventory. Master','AverageBackOrderFillDays',63)   , '' )
*****  =IIF(!lfError() , lfImp('lcRpMstMrx' ,'Inventory Master Matrix','ColumnLabelNRFCode',8)    , '' )
  =lfChkImp('lcRpDivisn' , '"Division Code Master"')
  =lfChkImp('lcRpTrmRef' , '"Terms Code Reference"')
  =lfChkImp('lcRpShpRef' , '"Ship Via Code Reference"')
  =lfChkImp('lcRpRetRef' , '"Return Auth. Code Reference"')
  =lfChkImp('lcRpRetRef' , '"Vendor Contacts"')
  =lfChkImp('lcRpVenMem' , '"Vendor Memo Maintenance"')
  *=lfChkImp('lcRpRepCom' ,'"Sales Representative Commission"')
  
  *WSH
  =lfChkImp('lcRpCstMem' , '"Customer Memo Maintenance"')
  *WSH
  
  IF !lfError()
    =lfImp('lcRpCust'   , 'Customer master', 'ConsumerUID', 16, 'P')
    =lfImp('lcRpCstShp' , 'Customer Ship to Addresses', 'ShipToCountryCode', 12, 'L')
    =lfImp('lcRpAriCst' , 'Aria Customer List', 'Aria Customer Number', 4, 'D')
    *Samah
    =lfImp('lcRpCSRep' , 'Customer UDF', 'Csrep', 3, 'C')
    *=lfImp('lcRpCstCnt'  , 'Aria Customer List', 'Aria Customer Number', 4, 'D')
    *Samah
    *T20060920.0009 [Start] 09/19/2006 import customer contacts
    =lfImp('lcRpCstCnt'  , 'Customer Contacts', 'ContactCode', 3 , 'C')
    *T20060920.0009 [END]
    =lfImp('lcRpSlsrep' , 'Salesperson Master', 'SalespersonNumber', 2, 'B')
    =lfImp('lcRpColors' , 'Colors', 'RowLabelDescription', 2, 'B')
    =lfImp('lcRpTerms'  , 'Terms Code Master', 'DaysBeforeDiscDue', 5, 'E')
    =lfImp('lcRpWareH'  , 'Warehouses', 'WhseDescription', 2)
    =lfImp('lcRpPrdlin' , 'Scales (Product Line and Size Scale)', 'Size Scale ( up to 8 sizes across) - 1 alpha char', 1, 'A')
    =lfImp('lcRpVendor' , 'Vendor Master', 'VendorName', 3, 'C')
    =lfImp('lcRpItmVnd' , 'Inventory Item Vendor', 'VendorWarrantyCode', 9, 'I')
    =lfImp('lcRpWhDMtx' , 'Inventory Item WH Detail Matrix', 'BinLocation', 5, 'E')
    =lfImp('lcRpInvMst' , 'Inventory. Master', 'AverageBackOrderFillDays', 63, 'BK')
    =lfImp('lcRpDivisn' , 'Division Code Master', 'Division', 1, 'A')
    =lfImp('lcRpTrmRef' , 'Terms Code Reference', 'Old AR Terms Description', 5, 'F')
    =lfImp('lcRpShpRef' , 'Ship Via Code Reference', 'Ship Via Code', 5, 'I')
    =lfImp('lcRpRetRef' , 'Return Reasons Code Reference', 'Return Auth Codes', 4, 'E')
    =lfImp('lcRpVenCnt' , 'Vendor Contacts', 'ContactCode', 3, 'C')
    =lfImp('lcRpVenMem' , 'Vendor Memo Maintenance', 'MemoText', 10, 'J')
    
    *WSH
    =lfImp('lcRpCstMem' , 'Customer Memo Maintenance', 'MemoSubject', 5, 'E')
    *WSH
    
  ENDIF
  *C126871,2 WSH [End]
  
  *-----------------* Proceed in conversion *-----------------* 
  IF !lfError()
    llZap = ( gfModalGen('INM00000B00006',.F.,.F.,.F.,'Overwrite existing data?') = 1 )
    IF llZap
      SELECT CODES
      ZAP
      SELECT SALESREP
      ZAP
      SELECT CUSTOMER
      ZAP
      SELECT ARCUSHST
      ZAP
      SELECT WAREHOUS
      ZAP
      SELECT SCALE
      ZAP
      SELECT SCALEHD
      ZAP
      SELECT STYLE
      ZAP
      SELECT STYDYE
      ZAP
      SELECT ICSTYHST 
      ZAP
      SELECT ICSEGVAL
      ZAP
      SELECT APVENDOR
      ZAP
      
      *C126871,2 WSH [Start]
      SELECT CONTACT
      ZAP
      SELECT NOTEPAD
      ZAP
      *SELECT REPCOMM
      *ZAP
      *C126871,2 WSH [End]
      
    ENDIF
    
    =lfCodeFile()  
    =lfColors()  
    =lfWareHous()
    =lfScale()
    =lfStyle()
    =lfSalesrps()  
    =lfCustomers()
    =lfVendor()
    =lfLogData('--------------------------------------')
  ENDIF  
  
  *-- Show final log report
  
  *C126871,2 WSH [Start]
  WAIT WINDOW "Deleting unneeded temporary files. Please wait..." NOWAIT
  *C126871,2 WSH [End]
  
  =lfLogReprt()
  lnTryAgain = lnTryAgain + 1
  
  *-- Close and Erase not needed temp files
  =lfErase()
ENDDO 

*:**************************************************************************
*:* Name        : lfOpenFiles
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/18/2004
*:* Purpose     : Open needed files
*:***************************************************************************
FUNCTION lfOpenFiles

=lfOpnExclsv(gcDataDir+'APVENDOR','VENCODE' )
=lfOpnExclsv(gcDataDir+'CUSTOMER','CUSTOMER')
=lfOpnExclsv(gcDataDir+'ARCUSHST','ACTHST'  )
=lfOpnExclsv(gcDataDir+'CODES'   ,'CCODE_NO')
=lfOpnExclsv(gcDataDir+'SALESREP','SALESREP')
=lfOpnExclsv(gcDataDir+'REP_DIV' ,'REP_DIV' )
=lfOpnExclsv(gcDataDir+'WAREHOUS','WAREHOUS')
=lfOpnExclsv(gcDataDir+'SCALE'   ,'SCALE'   )
=lfOpnExclsv(gcDataDir+'SCALEHD' ,'EXTSCALE')

*C126871,2 WSH [Start]
=lfOpnExclsv(gcDataDir+'CONTACT' ,'CONTACT')
=lfOpnExclsv(gcDataDir+'NOTEPAD' ,'NOTEPAD')
*=lfOpnExclsv(gcDataDir+'REPCOMM' ,'REPCOMM')
*C126871,2 WSH [End]

=lfOpnExclsv(gcDataDir+'STYLE'   ,'STYLE'   )
=lfOpnExclsv(gcDataDir+'STYDYE'  ,'STYDYE'  )
=lfOpnExclsv(gcDataDir+'SCALE'   ,'SCALE'   )
=lfOpnExclsv(gcDataDir+'SCALEHD' ,'EXTSCALE')
=lfOpnExclsv(gcDataDir+'ICSTYHST','STYHST'  )
=lfOpnExclsv(gcDataDir+'ICSEGVAL','SEGVAL'  )

=lfOpnExclsv(gcDataDir+'WAREHOUS','WAREHOUS')
=lfOpnExclsv(gcDataDir+'APVENHST','VENDYEAR')

=gfOpenFile(gcSysHome+'SYDFIELD','CFLD_NAME','SH')  && CFLD_NAME

PRIVATE lcSvOrd
IF USED('CODES')
  SELECT CODES
  lcSvOrd = ORDER()
  INDEX ON CDEFCODE+CFLD_NAME+CDISCREP TO (gcWorkDir+'CODEDESC')
  SET ORDER TO &lcSvOrd
ENDIF  

*-- end of lfOpenFiles.

*:**************************************************************************
*:* Name        : lfChkExclsv
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/12/2005
*:* Purpose     : Check if the file can be opened exclusivly, if not abort
*:***************************************************************************
FUNCTION lfOpnExclsv
PARAMETERS lcFile,lcIndex
PRIVATE lcSvError,llErr
llErr = .F.
lcSvError = ON('ERROR')

ON ERROR llErr = .T.
=gfOpenFile(lcFile,lcIndex,'EX')
ON ERROR &lcSvError

IF llErr
  =lfLogData('The file &lcFile can not be opened exclusivly.',0,.T.)
ENDIF

*-- end of lfChkExclsv.

*:**************************************************************************
*:* Name        : lfOpn
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/23/2004
*:* Purpose     : Open files
*:***************************************************************************
FUNCTION lfOpn
PARAMETERS lcFl,lcOrder,llSysfile
lcDir = IIF(llSysfile,gcSysHome,gcDataDir)
IF FILE(lcDir+lcFl+'.DBF')
  *-- Open files exclusivly
  PRIVATE llOpen
  llOpen=gfOpenFile(lcDir+lcFl,lcOrder,'EX')
ELSE
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'File '+lcDir+lcFl+'.DBF does not exist.')
  llRet = .F.
ENDIF  

*-- end of lfOpn.

*:**************************************************************************
*:* Name        : lfValidCo
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/23/2004
*:* Purpose     : Check if this valid co.
*:***************************************************************************
FUNCTION lfValidCo
PRIVATE llValid,llStrufl,lcModules

*!* This function check this company to be 
*!*   none extended 
*!*   style with style code structure  12 - 6 

STORE .F. TO llStrufl
STORE .T. TO llValid
IF FILE(gcDataDir+'icistru.DBF')
  llStrufl = gfOpenFile(gcDataDir+'icistru' ,'SEGNO'   ,'EX')
ENDIF  

lcModules = IIF(!'IC'$gcCmpModul,'IC, ','')+;
            IIF(!'AR'$gcCmpModul,'AR, ','')+;
            IIF(!'AP'$gcCmpModul,'AP, ','')+;
            IIF(!'SR'$gcCmpModul,'SR, ','')+;
            IIF(!'PO'$gcCmpModul,'PO, ','')
DO CASE

  CASE !EMPTY(lcModules)
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'The following modules are not installed &lcModules can not proceed.')
    llValid = .F.

  CASE gfGetMemvar('M_USEEXSSC')
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,;
    'This is an extended size scale company, can not proceed.')
    llValid = .F.

  CASE gfGetMemvar('M_WAREHOUS') = 'N'
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,;
    'The company must use multiple locations.')
    llValid = .F.

  *- If the item structure is not set yet
  CASE llStrufl .AND. !SEEK('U','ICISTRU')
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,;
    'The Style Code Structure is not set yet, can not proceed.')    
    llValid = .F.

  CASE !lfStyStru()
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,;
    'The Style Code Structure must be (Style=12, Color=6).')
    llValid = .F.

ENDCASE

IF USED('ICISTRU')
  USE IN 'ICISTRU'
ENDIF  

RETURN llValid

*-- end of lfValidCo.

*:**************************************************************************
*:* Name        : lfStyStru
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/23/2004
*:* Purpose     : Get style structure information
*:***************************************************************************
FUNCTION lfStyStru
PRIVATE lnSlct, laItemSeg, lnCount
lnSlct = Select()
DECLARE laItemSeg[1]

=gfItemMask(@laItemSeg)

FOR lnCount = 1 TO ALEN(laItemSeg,1)
  DO CASE
    CASE laItemSeg[lnCount,1]='F'
      IF LEN(laItemSeg[lnCount,3]) <> 12
        SELECT (lnSlct)
        RETURN .F.
      ENDIF
      lcClrSpr = ALLTRIM(laItemSeg[lnCount,6])
      
    CASE laItemSeg[lnCount,1]='C'
      IF LEN(laItemSeg[lnCount,3]) <> 6
        SELECT (lnSlct)
        RETURN .F.
      ENDIF
  ENDCASE
ENDFOR

SELECT (lnSlct)
*-- end of lfStyStru.

*:**************************************************************************
*:* Name        : lfvSlctfl
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/18/2004
*:* Purpose     : Select Excel file
*:***************************************************************************
*:* Called from : OG
*:***************************************************************************
FUNCTION lfvSlctfl
=gfMover(@laRpSource,@laRpTarget,"Select file to update Aria with.",.T.)
lcRpSlcted = ''
FOR lnI = 1 TO ALEN(laRpTarget)  
  IF !EMPTY(laRpTarget[lnI]) 
    lcRpSlcted = lcRpSlcted + '|' + laRpTarget[lnI]
  ENDIF
ENDFOR
CLEAR READ
*-- end of lfvSlctfl.

*:**************************************************************************
*:* Name        : lfwRepWhen
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/22/2004
*:* Purpose     : OG When funcion 
*:***************************************************************************
FUNCTION lfwRepWhen

*- Restore the optin grid variables from the temp variable list
lcRpCust   = lcVar1
lcRpCstShp = lcVar2
lcRpAriCst = lcVar3
lcRpSlsrep = lcVar4
lcRpColors = lcVar6
lcRpWareH  = lcVar7
lcRpPrdlin = lcVar8
lcRpVendor = lcVar9 
lcRpItmVnd = lcVar10 
lcRpWhDMtx = lcVar11 
lcRpInvMst = lcVar12 
lcRpMstMrx = lcVar13 
lcRpTerms  = lcVar14

*C126871,2 WSH [Start]
lcRpTrmRef = lcVar15
lcRpShpRef = lcVar16
lcRpRetRef = lcVar17
lcRpVenCnt = lcVar18
lcRpVenMem = lcVar19
lcRpRepCom = lcVar20
lcRpDivisn = lcVar21
*C126871,2 WSH [End]

*WSH
lcRpCSRep  = lcVar22
lcRpCStCnt = lcVar23
lcRpCstMem = lcVar24
*WSH

*- Disable save options button
SHOW GET pbOGSave  DISABLE
SHOW GET pbOGReset DISABLE
*-- end of lfwRepWhen.

*:**************************************************************************
*:* Name        : lfvGtFl
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/18/2004
*:* Purpose     : Select Excel file to update Aria with 
*:***************************************************************************
*:* Called from : OG
*:***************************************************************************
FUNCTION lfvGtFl
PARAMETERS lcVarNm,lcExt
PRIVATE lnI,lcFile,lcFileEx,llOk
llOk = .F.   

&lcVarNm = UPPER(ALLTRIM(&lcVarNm))
IF !EMPTY(&lcVarNm) .AND. (!FILE(&lcVarNm) .OR. lfSlctdBfr(lcVarNm))
  DO WHILE !llOk
    &lcVarNm = GETFILE(lcExt)  
    
    IF !EMPTY(&lcVarNm)
      *- Ceck the file is entered only once
      llOk = !lfSlctdBfr(lcVarNm)
    ELSE
      llOk = .T.
    ENDIF
  ENDDO
ENDIF  
*-- end of lfvGtFl.


*:**************************************************************************
*:* Name        : lfSlctdBfr
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/18/2005
*:* Purpose     : Check that this file is seleced befor in the option grid
*:***************************************************************************
FUNCTION lfSlctdBfr
PARAMETERS lcVarNm
PRIVATE lnI,lcFile,lcFileEx
FOR lnI = 1 TO ALEN(laVars)
  IF lcVarNm <> laVars[lnI]
    lcFile = UPPER(ALLTRIM(SUBSTR(&lcVarNm,RAT('\',&lcVarNm)+1)))
    lcFileEx = UPPER(ALLTRIM(SUBSTR(&laVars[lnI],RAT('\',&laVars[lnI])+1)))
    IF lcFile == lcFileEx
      WAIT WINDOW NOWAIT 'This file name has been already selected.'
      &lcVarNm = ''
      EXIT
    ENDIF          
  ENDIF
ENDFOR
RETURN EMPTY(&lcVarNm)
*-- end of lfSlctdBfr.

*:**************************************************************************
*:* Name        : lfUpdcodes
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/21/2004
*:* Purpose     : Update codes 
*:***************************************************************************
FUNCTION lfUpdcodes
PARAMETERS lcCode,lcCodVr,lcID,lcDesc

PRIVATE lnSlct,lcSvOrd

lnSlct = SELECT()

IF TYPE('&lcCodVr') = 'U'
  &lcCodVr = ''
ENDIF
  
IF !EMPTY(lcID)
  &lcCodVr = lcID
ENDIF  
IF EMPTY(lcDesc)
  lcDesc = &lcCodVr
ENDIF  

SELECT CODES
IF !EMPTY(&lcCodVr)
  IF SEEK(PADR(lcCode,10),'SYDFIELD')
    IF !'EDITABLE' $ SYDFIELD.MCODEINFO
      lcSvOrd = ORDER()
      SET ORDER TO CODEDESC  && CDEFCODE+CFLD_NAME+CDISCREP
      lcDesc   = &lcCodVr
      IF !SEEK('N'+PADR(lcCode,10)+PADR(ALLTRIM(&lcCodVr),30), 'CODES')
        &lcCodVr = ALLTRIM(gfSequence("cCode_No",gcAct_Comp))
      ELSE
        &lcCodVr = CODES.CCODE_NO  
      ENDIF
      SET ORDER TO &lcSvOrd
    ENDIF
  ENDIF
  
  && CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM
  IF !SEEK('N'+PADR(lcCode,10)+PADR(&lcCodVr,6),'CODES')    
    APPEND BLANK
    REPLACE CDEFCODE   WITH 'N' ;
            CFLD_NAME  WITH lcCode ;
            CCODE_NO   WITH &lcCodVr ;
            CDISCREP   WITH lcDesc ;
            CRLTFIELD  WITH 'N' 
    =lfAdUsrInf()

    *- Add related fields data if any
    =lfAddRltFl(PADR(lcCode,10),&lcCodVr)
    
  ENDIF
  *Samah    
 * IF !SEEK('D'+PADR(lcCode,10),'CODES')
    APPEND BLANK
    REPLACE CDEFCODE   WITH 'D' ;
            CFLD_NAME  WITH lcCode ;
            CCODE_NO   WITH &lcCodVr ;
            CDISCREP   WITH lcDesc ;
            CRLTFIELD  WITH 'N' 
    =lfAdUsrInf()
  *ENDIF

   IF !SEEK('D'+PADR(lcCode,10),'CODES')
     DO CASE
     
     Case lcCode  = 'SHIPVIA' AND &lcCodVr = UPPER("UPS Ground")
      APPEND BLANK
      REPLACE CDEFCODE   WITH 'D' ;
              CFLD_NAME  WITH lcCode ;
              CCODE_NO   WITH &lcCodVr ;
              CDISCREP   WITH lcDesc ;
              CRLTFIELD  WITH 'N' 
      =lfAdUsrInf()
     Case lcCode  <> 'SHIPVIA'      
      APPEND BLANK
      REPLACE CDEFCODE   WITH 'D' ;
              CFLD_NAME  WITH lcCode ;
              CCODE_NO   WITH &lcCodVr ;
              CDISCREP   WITH lcDesc ;
              CRLTFIELD  WITH 'N' 
      =lfAdUsrInf()
    ENDCASE  
  ENDIF
  *SAMAH

ELSE
  IF SEEK('D'+PADR(lcCode,6),'CODES')
    &lcCodVr = CODES.CCODE_NO
  ENDIF

ENDIF  

SELECT (lnSlct)
*-- end of lfUpdcodes.

*:**************************************************************************
*:* Name        : lfAddRltFl
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/18/2005
*:* Purpose     : Add related fields data
*:***************************************************************************
FUNCTION lfAddRltFl
PARAMETERS lcCode,lcCodVal
   
    *- Add related field
    DO CASE

    CASE lcCode = 'CSTYGROUP '

      APPEND BLANK
      REPLACE CCODE_NO  WITH lcCodVal ;
              CDEFCODE  WITH 'N' ;
              CRLTFIELD WITH 'Y' ;
              CFLD_NAME WITH lcCode ;
              CRLTD_NAM WITH 'CGRPDESC2' ;
              CRLTD_TYP WITH 'C' ;
              CRLTD_VLU WITH '' 
      =lfAdUsrInf()
    
    *C126871,2 WSH [Start]
    *CASE lcCode = 'CDIVISION '
    *
    *  APPEND BLANK
    *  REPLACE CCODE_NO  WITH lcCodVal ;
    *          CDEFCODE  WITH 'N' ;
    *          CRLTFIELD WITH 'Y' ;
    *          CFLD_NAME WITH lcCode ;
    *          CRLTD_NAM WITH 'DIVLNAME  ' ;
    *          CRLTD_TYP WITH 'C' ;
    *          CRLTD_VLU WITH '' 
    *  =lfAdUsrInf()
    *
    *  APPEND BLANK
    *  REPLACE CCODE_NO  WITH lcCodVal ;
    *          CDEFCODE  WITH 'N' ;
    *          CRLTFIELD WITH 'Y' ;
    *          CFLD_NAME WITH lcCode ;
    *          CRLTD_NAM WITH 'CDIVTYPE  ' ;
    *          CRLTD_TYP WITH 'C' ;
    *          CRLTD_VLU WITH 'A' 
    *  =lfAdUsrInf()
    *C126871,2 WSH [End]
    
    CASE lcCode = 'SHIPVIA   '

      APPEND BLANK
      REPLACE CCODE_NO  WITH lcCodVal ;
              CDEFCODE  WITH 'N' ;
              CRLTFIELD WITH 'Y' ;
              CFLD_NAME WITH lcCode ;
              CRLTD_NAM WITH 'CUPS      ' ;
              CRLTD_TYP WITH 'C' ;
              CRLTD_VLU WITH '' 
      =lfAdUsrInf()

      APPEND BLANK
      REPLACE CCODE_NO  WITH lcCodVal ;
              CDEFCODE  WITH 'N' ;
              CRLTFIELD WITH 'Y' ;
              CFLD_NAME WITH lcCode ;
              CRLTD_NAM WITH 'NCODCHARGE' ;
              CRLTD_TYP WITH 'N' ;
              CRLTD_VLU WITH '0.0' 
      =lfAdUsrInf()

      APPEND BLANK
      REPLACE CCODE_NO  WITH lcCodVal ;
              CDEFCODE  WITH 'N' ;
              CRLTFIELD WITH 'Y' ;
              CFLD_NAME WITH lcCode ;
              CRLTD_NAM WITH 'NINSCHARGE' ;
              CRLTD_TYP WITH 'N' ;
              CRLTD_VLU WITH '0.0' 
      =lfAdUsrInf()

      APPEND BLANK
      REPLACE CCODE_NO  WITH lcCodVal ;
              CDEFCODE  WITH 'N' ;
              CRLTFIELD WITH 'Y' ;
              CFLD_NAME WITH lcCode ;
              CRLTD_NAM WITH 'NFXDPRCNT ' ;
              CRLTD_TYP WITH 'N' ;
              CRLTD_VLU WITH '0.0' 
      =lfAdUsrInf()

      APPEND BLANK
      REPLACE CCODE_NO  WITH lcCodVal ;
              CDEFCODE  WITH 'N' ;
              CRLTFIELD WITH 'Y' ;
              CFLD_NAME WITH lcCode ;
              CRLTD_NAM WITH 'CARRIERCOD' ;
              CRLTD_TYP WITH 'C' ;
              CRLTD_VLU WITH '' 
      =lfAdUsrInf()

      APPEND BLANK
      REPLACE CCODE_NO  WITH lcCodVal ;
              CDEFCODE  WITH 'N' ;
              CRLTFIELD WITH 'Y' ;
              CFLD_NAME WITH lcCode ;
              CRLTD_NAM WITH 'CTRANMTHD ' ;
              CRLTD_TYP WITH 'C' ;
              CRLTD_VLU WITH '@M' 
      =lfAdUsrInf()
    
    CASE lcCode = 'CVEN1099T '

      APPEND BLANK
      REPLACE CCODE_NO  WITH lcCodVal ;
              CDEFCODE  WITH 'N' ;
              CRLTFIELD WITH 'Y' ;
              CFLD_NAME WITH lcCode ;
              CRLTD_NAM WITH 'C1099CODE ' ;
              CRLTD_TYP WITH 'C' ;
              CRLTD_VLU WITH '01' 
      =lfAdUsrInf()
    
    CASE lcCode = 'COLOR     '

      APPEND BLANK
      REPLACE CCODE_NO  WITH lcCodVal ;
              CDEFCODE  WITH 'N' ;
              CRLTFIELD WITH 'Y' ;
              CFLD_NAME WITH lcCode ;
              CRLTD_NAM WITH 'CLRLNAME  ' ;
              CRLTD_TYP WITH 'C' ;
              CRLTD_VLU WITH '' 
      =lfAdUsrInf()

      APPEND BLANK
      REPLACE CCODE_NO  WITH lcCodVal ;
              CDEFCODE  WITH 'N' ;
              CRLTFIELD WITH 'Y' ;
              CFLD_NAME WITH lcCode ;
              CRLTD_NAM WITH 'CNRFCODE  ' ;
              CRLTD_TYP WITH 'C' ;
              CRLTD_VLU WITH '' 
      =lfAdUsrInf()

      APPEND BLANK
      REPLACE CCODE_NO  WITH lcCodVal ;
              CDEFCODE  WITH 'N' ;
              CRLTFIELD WITH 'Y' ;
              CFLD_NAME WITH lcCode ;
              CRLTD_NAM WITH 'LLOBSOLETE' ;
              CRLTD_TYP WITH 'L' ;
              CRLTD_VLU WITH 'F' 
      =lfAdUsrInf()

      APPEND BLANK
      REPLACE CCODE_NO  WITH lcCodVal ;
              CDEFCODE  WITH 'N' ;
              CRLTFIELD WITH 'Y' ;
              CFLD_NAME WITH lcCode ;
              CRLTD_NAM WITH 'CPAINT    ' ;
              CRLTD_TYP WITH 'C' ;
              CRLTD_VLU WITH '' 
      =lfAdUsrInf()
      
      

    ENDCASE

*-- end of lfAddRltFl.

*:**************************************************************************
*:* Name        : lfAdUsrInf
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/23/2004
*:* Purpose     : Add user information 
*:***************************************************************************
FUNCTION lfAdUsrInf
PARAMETERS llEdit,lcAlias
PRIVATE lnSlct
lnSlct = SELECT()

IF !EMPTY(lcAlias)
  SELECT &lcAlias
ENDIF
  
IF llEdit

  REPLACE CEDIT_USER  WITH gcUser_id ;
          DEDIT_DATE  WITH gdSysDate ;
          CEDIT_TIME  WITH TIME() ;
          CEDT_VER    WITH 'A27'

ELSE

  REPLACE CADD_USER  WITH gcUser_id ;
          DADD_DATE  WITH gdSysDate ;
          CADD_TIME  WITH TIME() ;
          CADD_VER   WITH 'A27'

ENDIF

SELECT (lnSlct)
*-- end of lfAdUsrInf.
*:**************************************************************************
*:* Name        : lfErase
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/23/2004
*:* Purpose     : *- Erase not needed temp files
*:***************************************************************************
FUNCTION lfErase
FOR lnTmpCnt = 1 TO ALEN(laTmpFl)
  IF !EMPTY(laTmpFl[lnTmpCnt])
    IF USED(laTmpFl[lnTmpCnt])
      USE IN (laTmpFl[lnTmpCnt])
    ENDIF
    ERASE (gcWorkDir+laTmpFl[lnTmpCnt]+'.DBF')
    ERASE (gcWorkDir+laTmpFl[lnTmpCnt]+'.CDX'))
    ERASE (gcWorkDir+laTmpFl[lnTmpCnt]+'.FPT'))
  ENDIF
ENDFOR
*-- end of lfErase.

*:**************************************************************************
*:* Name        : lfTmpFlNm
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/23/2004
*:* Purpose     : Create a temp file name and add it to temp names array 
*:***************************************************************************
FUNCTION lfTmpFlNm
lnTmpCnt = lnTmpCnt + 1
DIMENSION laTmpFl[lnTmpCnt]
laTmpFl[lnTmpCnt] = gfTempName()
RETURN laTmpFl[lnTmpCnt]
*-- end of lfTmpFlNm.

*:**************************************************************************
*:* Name        : lfGetDir
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 04/05/2004
*:* Purpose     : Get color files dir
*:***************************************************************************
FUNCTION lfGetDir
IF EMPTY(lcRpClrDir)
  lcRpClrDir =  GETDIR()
ENDIF  
*-- end of lfGetDir.

*:**************************************************************************
*:* Name        : lfStr2Arr
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 04/15/2004
*:* Purpose     : Convert a string to array 
*:***************************************************************************
FUNCTION lfStr2Arr
PARAMETERS lcStr,laArr,lcSepr
PRIVATE lnK
FOR lnK = 1 TO OCCURS(lcSepr,lcStr) - 1
  DIMENSION laArr[lnK]
  laArr[lnK] = SUBSTR(lcStr,AT(lcSepr,lcStr,1)+1,AT(lcSepr,lcStr,2)-2)
  lcStr = SUBSTR(lcStr,AT(lcSepr,lcStr,2))
ENDFOR

*-- end of lfStr2Arr.

*:**************************************************************************
*:* Name        : lfCrtTmps
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/12/2005
*:* Purpose     : Create temp files
*:***************************************************************************
FUNCTION lfCrtTmps
PRIVATE laFileStru,lnFileStru,laIndex,lnAlias
lnAlias = SELECT()

*-- Create Temp file that will hold any error occurs while importing.
CREATE TABLE (gcWorkDir+lcLogFile) (nRecNo N(5) , cError C(150) , LERROR L , CVAR C(50) )
INDEX ON CVAR+STR(NRECNO,5) TAG CVAR
INDEX ON cError TAG cError

SELECT(lnAlias)

*-- End of Function lfCrtTmps.


*:**************************************************************************
*:* Name        : lfLogData
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/03/2005
*!* Purpose     : Insert record in the log file with the error occurred
*!*             : or generate an error massage
*:***************************************************************************
FUNCTION lfLogData
PARAMETERS lcLogMsg,lnRecno,llError,lcVar
lnRecno = IIF(EMPTY(lnRecno),0,lnRecno)
lcVar = IIF(EMPTY(lcVar),'',lcVar)
*==*IF !SEEK(PADR(lcLogMsg,150)),lcLogFile)
  INSERT INTO (lcLogFile) (CERROR,NRECNO,LERROR,CVAR) ;
                  VALUES (lcLogMsg,lnRecno,llError,lcVar)
*==*ENDIF                  
*-- End of Function lfLogData.

*:**************************************************************************
*:* Name        : lfLogReprt
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/03/2005
*:* Purpose     : Show a log report
*:***************************************************************************
FUNCTION lfLogReprt
PRIVATE lnLogLnNo 
lcWinTitl  = "Log Report"
CREATE CURSOR TMPSTR (mStrRep M(120))
SELECT TMPSTR 
APPEND BLANK  

IF !lfError()
Replace mStrRep WITH mStrRep + '--------------------------------------------------------------------'+Chr(13)+;
                               'Importing data from excel files .'+Chr(13)+;
                               'Started at Date '+DTOC(DATE())+' Time '+TIME()+Chr(13)+;
                               '--------------------------------------------------------------------'
ENDIF
SELECT (lcLogFile)
SET ORDER TO CVAR
LOCATE
lnLogLnNo = 0
SCAN
  SELECT TMPSTR 
  IF &lcLogFile..NRECNO <> 0
    lnLogLnNo = lnLogLnNo + 1
*    Replace mStrRep WITH mStrRep + Chr(10) + Chr(13)+ ALLTRIM(STR(lnLogLnNo)) + ' - ' + ALLTRIM(&lcLogFile..cError)
    Replace mStrRep WITH mStrRep + Chr(13)+ ALLTRIM(STR(lnLogLnNo)) + ' - ' + ALLTRIM(&lcLogFile..cError)
  ELSE
*    Replace mStrRep WITH mStrRep + Chr(10) + Chr(13)+ ALLTRIM(&lcLogFile..cError)
    Replace mStrRep WITH mStrRep + Chr(13)+ ALLTRIM(&lcLogFile..cError)
  ENDIF
  SELECT (lcLogFile)
ENDSCAN
SELECT TMPSTR

*C126871,2 WSH [Start]
WAIT CLEAR
*C126871,2 WSH [End]

DO (gcScrDir + 'SM\SMSTRREP.SPR')
*DO (gcScrDir + 'SM\SMLOG.SPR')

*-- end of lfLogReprt.

*!**************************************************************************
*!* Name      : lfPrnt
*!* Developer : NNA - NADER NABIL ABD-ALMONAM
*!* Date      : 07/18/2005
*!* Purpose   : Print the 2nd Rep commission difference report.
*!**************************************************************************
*!* Example   : = lfPrnt()
*!**************************************************************************
FUNCTION lfvPrnt
IF pSetup(.T.)
  gcOutFile = gcWorkDir+gfTempName()+'.TXT'
  COPY MEMO TMPSTR.mStrRep TO &gcOutFile
  gcDevice = 'PRINTER'
  DO ENDREPORT
  gcDevice = 'SCREEN'
ENDIF

*:**************************************************************************
*:* Name        : lfChkImp
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/11/2005
*:* Purpose     : Check imported file variable is not empty
*:***************************************************************************
FUNCTION lfChkImp
PARAMETERS lcVar,lcMsg
  
  IF EMPTY(&lcVar)
    lcMsg = 'You have not selected the &lcMsg file.'
    =lfLogData(lcMsg,0,.T.)    
  ENDIF 
  
*-- end of lfChkImp.
*:**************************************************************************
*:* Name        : lfImp
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/04/2005
*:* Purpose     : call the lfImport function .
*:***************************************************************************
FUNCTION lfImp

*C126871,2 WSH [Start]
*PARAMETERS lcVar,lcMsg,lcString,lnFldNo
PARAMETERS lcVar, lcMsg, lcString, lnFldNo, lcFldHdr
*C126871,2 WSH [End]

PRIVATE lcRetVar,lcCurDir,lcSvError,llError,lcAlias,lcImpFile

*C126871,2 WSH [Start]
PRIVATE lnAlias
lnAlias = SELECT(0)
*C126871,2 WSH [End]

lcString = IIF(EMPTY(lcString) , '' , lcString )
lnFldNo  = IIF(EMPTY(lnFldNo ) , 1  , lnFldNo )
  
lcRetVar = STRTRAN(lcVar,'lcRp','lcIm')

IF !EMPTY(&lcVar)

  &lcRetVar = ''
  lcCurDir = FULLPATH('')               && Save current default dir
  SET DEFAULT TO (gcWorkDir)
  lcImpFile = &lcVar
  
  lcSvError = ON('ERROR')
  WAIT WINDOW NOWAIT 'Importing the excel file :'+lcImpFile
  SELECT 0

  llError = .F.
  ON ERROR llError = .T.
  IMPORT FROM (lcImpFile) TYPE XLS
  ON ERROR &lcSvError

  WAIT CLEAR
  
  IF llError
    lcMsg = 'The &lcMsg file (' + ALLTRIM(lcImpFile) +') is not a xls 4.0 format.'
    =lfLogData(lcMsg,0,.T.)
  ELSE
    DO CASE
    CASE RECCOUNT() = 0
      lcMsg = 'The &lcMsg file is empty.'
      =lfLogData(lcMsg,0,.T.)
    
    *C126871,2 WSH [Start]
    *CASE EMPTY(FIELD(lnFldNo)) .OR. !lcString $ EVAL(FIELD(lnFldNo))
    CASE (!EMPTY(lcFldHdr) .AND. !lcString $ EVALUATE(lcFldHdr)) .OR. EMPTY(FIELD(lnFldNo)) .OR. !lcString $ EVALUATE(FIELD(lnFldNo))
    *C126871,2 WSH [End]
    
      lcMsg = 'The &lcMsg file is wrong.'      
      =lfLogData(lcMsg,0,.T.)
      
      *- Remove the wrong selected and update the memo field   and close the file
      &lcVar = ''
      SAVE TO (lcSvMem) ALL LIKE l?Rp*      
      USE 

    OTHERWISE
      &lcRetVar = lfTmpFlNm()
      USE DBF() ALIAS (&lcRetVar)
      GO TOP
      DELETE NEXT 1
      
      GO TOP
      IF EOF()
        lcMsg = 'The &lcMsg file has no data records.'
        =lfLogData(lcMsg,0,.T.)
      ENDIF
    ENDCASE

  ENDIF
  
  *C126871,2 WSH [Start]
  IF !lfError()
    DO CASE
      CASE lcRetVar = "lcImTrmRef"
        SELECT (lcImTrmRef)
        INDEX ON A TAG A
        INDEX ON E TAG E
      CASE lcRetVar = "lcImShpRef"
        SELECT (lcImShpRef)
        INDEX ON A TAG A
        INDEX ON E TAG E
        INDEX ON I TAG I
      CASE lcRetVar = "lcImRetRef"
        SELECT (lcImRetRef)
        INDEX ON A TAG A
        INDEX ON D TAG D
    ENDCASE
  ENDIF
  *C126871,2 WSH [End]
  
  SET DEFAULT TO &lcCurDir
  
ELSE

  lcMsg = 'You have not selected the &lcMsg file.'
  =lfLogData(lcMsg,0,.T.)    

ENDIF 

*C126871,2 WSH [Start]
SELECT (lnAlias)
*C126871,2 WSH [End]

*-- end of lfImp.

*:**************************************************************************
*:* Name        : lfError
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/04/2005
*:* Purpose     : Check if there is an error lines in log file
*:***************************************************************************
FUNCTION lfError
PRIVATE lnSlct,llErr
lnSlct = SELECT()

SELECT &lcLogFile
GO TOP
LOCATE FOR LERROR
llErr = FOUND()

SELECT (lnSlct)
RETURN llErr
*-- end of lfError.

*:**************************************************************************
*:* Name        : lfCodeFile
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/05/2005
*:* Purpose     : Update codes
*:***************************************************************************
FUNCTION lfCodeFile

*C126871,2 WSH [Start]
**- Update division codes from the "division master file"'
*=lfUpdcodes('CDIVISION ','M.CDIVISION','00','MAIN DIVISION')
*=lfUpdcodes('CDIVISION ','M.CDIVISION','10','CORPORATE DIVISION')
*=lfUpdcodes('CDIVISION ','M.CDIVISION','20','RESORT DIVISION')
*=lfUpdcodes('CDIVISION ','M.CDIVISION','30','CAMP DAVID SAMPLES')
*=lfUpdcodes('CDIVISION ','M.CDIVISION','40','COLLEGIATE DIVISION')
*=lfUpdcodes('CDIVISION ','M.CDIVISION','50','CATALOG SALES')
*=lfUpdcodes('CDIVISION ','M.CDIVISION','60','CLOSEOUT DIVISION')
*=lfUpdcodes('CDIVISION ','M.CDIVISION','70','GOLF DIVISION')
*=lfUpdcodes('CDIVISION ','M.CDIVISION','90','ATTITUDE 101')
*=lfUpdcodes('CDIVISION ','M.CDIVISION','95','BRANDED EMBLEM')
=lfDivision()
*C126871,2 WSH [End]

*- Update Groups codes '
=lfUpdcodes('CSTYGROUP','M.CSTYGROUP','ACCE','Accessories')
=lfUpdcodes('CSTYGROUP','M.CSTYGROUP','ACTV','Active Wear')
=lfUpdcodes('CSTYGROUP','M.CSTYGROUP','CAPS','Caps')
=lfUpdcodes('CSTYGROUP','M.CSTYGROUP','CLOS','Close-Outs')
=lfUpdcodes('CSTYGROUP','M.CSTYGROUP','DI  ','Direct Import')
=lfUpdcodes('CSTYGROUP','M.CSTYGROUP','EMBL','Imported Emblem')
=lfUpdcodes('CSTYGROUP','M.CSTYGROUP','FABR','Fabric')
=lfUpdcodes('CSTYGROUP','M.CSTYGROUP','FLCE','Fleece')
=lfUpdcodes('CSTYGROUP','M.CSTYGROUP','HNLY','Henley')
=lfUpdcodes('CSTYGROUP','M.CSTYGROUP','JCKT','Jackets')
=lfUpdcodes('CSTYGROUP','M.CSTYGROUP','JRS ','CDX Juniors')
=lfUpdcodes('CSTYGROUP','M.CSTYGROUP','KIDS','Kids')
=lfUpdcodes('CSTYGROUP','M.CSTYGROUP','LADI','Ladies')
=lfUpdcodes('CSTYGROUP','M.CSTYGROUP','NEWI','New Item')
=lfUpdcodes('CSTYGROUP','M.CSTYGROUP','PIN ','Imported Pin & Misc')
=lfUpdcodes('CSTYGROUP','M.CSTYGROUP','POLO','Polo Shirts')
=lfUpdcodes('CSTYGROUP','M.CSTYGROUP','PRC ','Private Reserver Collection')
=lfUpdcodes('CSTYGROUP','M.CSTYGROUP','ROY ','Royalties')
=lfUpdcodes('CSTYGROUP','M.CSTYGROUP','SHRT','Shorts')
=lfUpdcodes('CSTYGROUP','M.CSTYGROUP','THRD','Thread')
=lfUpdcodes('CSTYGROUP','M.CSTYGROUP','TSHT','T-shirts')
=lfUpdcodes('CSTYGROUP','M.CSTYGROUP','WOVN','Woven')

*- No need for Region Code
=lfUpdcodes('REGION'   ,'M.REGION'   , 'N/A','N/A')

*- Add one season called All Seasons
M.SEASON = ''
=lfUpdcodes('SEASON'   ,'M.SEASON'   , 'ALLSES','ALL SEASONS')

*- Get terms'
=lfTerms()

*C126871,2 WSH [Start]
*- Get Ship Via Codes
=lfShipVia()
=lfReturns()
*C126871,2 WSH [End]

*- Get other codes from customer file'
SELECT &lcImCust
GO TOP
SCAN FOR !EMPTY(&lcImCust..Q+&lcImCust..AB+&lcImCust..W)
  
  *C126871,2 WSH [Start]
  *M.SHIPVIA = ALLTRIM(&lcImCust..Q)  && ShipMethod         
  *=lfUpdcodes('SHIPVIA   ','M.SHIPVIA')
  *WAIT WINDOW NOWAIT M.SHIPVIA
  *C126871,2 WSH [End]
  
  M.CLASS   = ALLTRIM(&lcImCust..AB) && CustomerType  
  =lfUpdcodes('CLASS     ','M.CLASS')
  WAIT WINDOW NOWAIT M.CLASS
  
  M.SPCINST = ALLTRIM(&lcImCust..W)   && SortField   
  =lfUpdcodes('SPCINST'   ,'M.SPCINST')
  WAIT WINDOW NOWAIT M.SPCINST    
  
ENDSCAN  

*- Get more other codes from vendor file'
SELECT &lcImVendor
GO TOP
SCAN FOR !EMPTY(&lcImVendor..Y)

  M.CVEN1099T  = ALLTRIM(&lcImVendor..Y)
  =lfUpdcodes('CVEN1099T','M.CVEN1099T')

ENDSCAN

=lfLogData('Codes data Updated successfully')

*-- end of lfCodeFile.

*:**************************************************************************
*:* Name        : lfSalesrps
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/15/2005
*:* Purpose     : sales reps processing
*:***************************************************************************
FUNCTION lfSalesrps

*- Processing salesreps'
PRIVATE lcDiv

*C126871,2 WSH [Start]
*SELECT (lcImRepCom)
*INDEX ON B+DTOS(CTOD(G))+E+F TAG BGEF
*SET ORDER TO BGEF
*C126871,2 WSH [End]

SELECT &lcImSlsrep
GO TOP
SCAN 
  IF SUBSTR(&lcImSlsrep..B,1,1) <> '0'
    lcMsg = 'Sales Rep file: the repcode ' + ALLTRIM(&lcImSlsrep..B) + ' does not start with "0" , skip this record.'
    =lfLogData( lcMsg , RECNO(lcImSlsrep) ,.F.,lcImSlsrep)
    LOOP
  ENDIF
  
  SELECT SALESREP
  SCATTER MEMVAR BLANK
  
  M.REPCODE    = ALLTRIM(SUBSTR(&lcImSlsrep..B,2))
  
  *C126871,2 WSH [Start]
  &lcMas90Fld. = &lcImSlsrep..B
  *C126871,2 WSH [End]
  
  WAIT WINDOW NOWAIT 'Repcode:'+M.REPCODE
  M.NAME       = ALLTRIM(&lcImSlsrep..C)
  M.PHONE      = CHRTRAN( ALLTRIM(&lcImSlsrep..L) , ' -()' , '' )
  M.PAY_TYPE   = 'R'
  M.CADDRESS1  = ALLTRIM(&lcImSlsrep..D)
  M.CADDRESS2  = ALLTRIM(&lcImSlsrep..E)
  M.CADDRESS3  = ALLTRIM(&lcImSlsrep..F)
  M.CADDRESS4  = ALLTRIM(&lcImSlsrep..G)
  M.CADDRESS5  = ALLTRIM(&lcImSlsrep..H)
  M.CADDRESS6  = ALLTRIM(&lcImSlsrep..N)
  M.CEMAIL_ADD = ALLTRIM(&lcImSlsrep..O)
  M.COMM       = ALLTRIM(&lcImSlsrep..P)  
  
  M.CCONT_CODE = 'USA'
  
  =lfUpdcodes('CTERMCODE','M.CTERMCODE')
  =lfUpdcodes('REGION'   ,'M.REGION')
  
  IF !SEEK(M.REPCODE,'SALESREP')  
    APPEND BLANK
    GATHER MEMVAR
    =lfAdUsrInf()
  ENDIF
  
  *- Add REP_DIV data
  lcDiv = PADR(ALLTRIM(&lcImSlsrep..A),6)
  IF !SEEK(M.REPCODE+lcDiv,'REP_DIV')
    SELECT REP_DIV
    APPEND BLANK
    REPLACE REPCODE   WITH M.REPCODE ;
            CDIVISION WITH lcDiv ;
            COMM_RATE WITH VAL(ALLTRIM(&lcImSlsrep..P))
    =lfAdUsrInf()
  ENDIF
  
ENDSCAN
WAIT CLEAR
=lfLogData('Sales Reps data converted successfully',0)

*-- end of lfSalesrps.

*:**************************************************************************
*:* Name        : lfCustomers
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/04/2005
*:* Purpose     : Processing customers data
*:***************************************************************************
FUNCTION lfCustomers

WAIT WINDOW NOWAIT 'Indexing...'
SELECT &lcImCust
INDEX ON A+B TAG AB
LOCATE

SELECT &lcImAriCst
REPLACE D WITH ALLT(D) ALL
INDEX ON D TAG D
LOCATE

SELECT &lcImCstShp
INDEX ON A+B TAG AB
LOCATE

*C126871,2 WSH [Start]
SELECT (lcImShpRef)
SET ORDER TO E

SELECT (lcImTrmRef)
SET ORDER TO E
*C126871,2 WSH [End]

*WSH
SELECT (lcImCstMem)
INDEX ON A+B+C TAG ABC
SET ORDER TO ABC

SELECT (lcImCSRep)
INDEX ON A+B TAG AB
SET ORDER TO AB
*WSH

*T20060920.0009 09/19/2006 Customer contacts file [START]
SELECT (lcImCstCnt)
INDEX ON A+B TAG AB
SET ORDER TO AB
*T20060920.0009 09/19/2006 [END]

SELECT CUSTOMER
lcSvOrd = ORDER()
INDEX ON TYPE+ACCOUNT+STNAME TO (gcWorkDir+'STNAMETG')
SET ORDER TO &lcSvOrd

SELECT &lcImAriCst

SCAN
  IF EMPTY(B) OR EMPTY(D)
    LOOP
  ENDIF
  
  SELECT CUSTOMER
  SCATTER MEMVAR BLANK
  
  IF SEEK(&lcImAriCst..A+&lcImAriCst..B, lcImCust)
    M.ACCOUNT = PADR(ALLTRIM(&lcImAriCst..D),5)
    M.STNAME  = PADR( IIF(!EMPTY(&lcImAriCst..C) , ALLTRIM(&lcImAriCst..C) , M.ACCOUNT ) , 30 )
    M.BTNAME  = IIF(!EMPTY(&lcImAriCst..C) , ALLTRIM(&lcImAriCst..C) , M.ACCOUNT )
  ELSE
    =lfLogData('Account '+&lcImAriCst..B+' Exists in the Aria Customer List file but not found in custmer master file.')
    LOOP
  ENDIF
  
  *C126871,2 WSH [Start]
  &lcMas90Fld. = &lcImCust..B
  *C126871,2 WSH [End]
  
  WAIT WINDOW NOWAIT 'Account : '+M.ACCOUNT
  
  M.TYPE        = "M"  
  M.BILLTO      = "M"  
  M.STATUS      = "A"
  M.PHONE1      = CHRTRAN( ALLTRIM(&lcImCust..I) , ' -()' , '' ) + ALLTRIM(&lcImCust..J)
  M.FAX         = CHRTRAN( ALLTRIM(&lcImCust..AQ), ' -()' , '' ) 
  M.CADDRESS1   = ALLTRIM(&lcImCust..D) 
  M.CADDRESS12  = ALLTRIM(&lcImCust..D) 
  M.CADDRESS2   = ALLTRIM(&lcImCust..E) 
  M.CADDRESS22  = ALLTRIM(&lcImCust..E) 
  M.CADDRESS3   = ALLTRIM(&lcImCust..F) 
  M.CADDRESS32  = ALLTRIM(&lcImCust..F) 
  M.CADDRESS4   = ALLTRIM(&lcImCust..G) 
  M.CADDRESS42  = ALLTRIM(&lcImCust..G) 
  M.CADDRESS5   = ALLTRIM(&lcImCust..H) 
  M.CADDRESS52  = ALLTRIM(&lcImCust..H) 
  
  *WSH
  *M.CEMAI_ADD   = ALLTRIM(&lcImCust..AR) 
  M.CEMAIL_ADD  = ALLTRIM(&lcImCust..AR) 
  *WSH
  
  M.PRNT_STATM  = "Y"
  M.LINK_CODE   = "DEFDEF"
  M.CONSOL      = "N"
  M.PRIORITY    = "5  "
  M.CINSUR      = "Y"
  M.PRICELVL    = "A"
  M.CTAXRULE    = " 1"  
  
  M.CCONT_CODE  = IIF(ALLTRIM(&lcImCust..H) = 'CAN' , 'CANADA' , ;
                  IIF(ALLTRIM(&lcImCust..H) = 'BCS' , 'BCS'    , 'USA' ))
  M.CCONT_COD2  = M.CCONT_CODE  
  M.CADDRESS6   = M.CCONT_CODE  
  M.CADDRESS62  = M.CCONT_CODE  
  
  M.CCURRCODE   = IIF(ALLTRIM(&lcImCust..H) = 'CAN' , 'CAD' , ;
                  IIF(ALLTRIM(&lcImCust..H) = 'BCS' , 'BCS'    , 'USD' ))
  
  M.CBACKORD    = "A"  
  M.CSLSGLLINK  = "DEF"
  
  M.NOTE        = ALLTRIM(&lcImCust..V)
  M.CRLIMIT     = VAL(ALLTRIM(&lcImCust..AX))
  
  M.SALESREP    = ALLTRIM(SUBSTR(&lcImCust..U,2))
  IF !EMPTY(M.SALESREP) .AND. !SEEK(M.SALESREP, 'SALESREP')
*==*    =lfLogData('The repcode '+M.SALESREP+' not added to the salesrep file yet.')
    M.SALESREP = ''
  ENDIF
  
  *- Update code fields  
  M.SHIPVIA     = ALLTRIM(&lcImCust..Q)
  
  *C126871,2 WSH [Start]
  IF SEEK(PADR(M.SHIPVIA, LEN(EVALUATE(lcImShpRef + '.E'))), lcImShpRef)
    M.SHIPVIA = ALLTRIM(EVALUATE(lcImShpRef + '.G'))
  ELSE
    M.SHIPVIA = ''
  ENDIF
  *C126871,2 WSH [End]
  
  =lfUpdcodes('SHIPVIA   ','M.SHIPVIA')
  
  M.CLASS       = ALLTRIM(&lcImCust..AB)
  =lfUpdcodes('CLASS     ','M.CLASS')
  
  *C126871,2 WSH [Start]
  *=SEEK(ALLTRIM(&lcImCust..T), lcImTerms)
  *M.CTERMCODE  = ALLTRIM(&lcImTerms..H)
  M.CTERMCODE  = ALLTRIM(&lcImCust..T)
  
  IF !EMPTY(M.CTERMCODE)
    IF SEEK(PADR(M.CTERMCODE, LEN(EVALUATE(lcImTrmRef + '.E'))), lcImTrmRef)
      M.CTERMCODE = ALLTRIM(EVALUATE(lcImTrmRef + '.G'))
      
      IF SEEK(PADR(M.CTERMCODE, LEN(EVALUATE(lcImTerms + '.A'))), lcImTerms)
        M.CTERMCODE = ALLTRIM(EVALUATE(lcImTerms + '.H'))
      ELSE
        M.CTERMCODE = ''
      ENDIF
    ELSE
      M.CTERMCODE = ''
    ENDIF
  ENDIF
  *C126871,2 WSH [End]
  
  M.SPCINST    = ALLTRIM(&lcImCust..W)
  =lfUpdcodes('SPCINST'   ,'M.SPCINST')
  
  M.CDIVISION  = ALLTRIM(&lcImCust..A)
  =lfUpdcodes('CDIVISION ','M.CDIVISION')
  
  =lfUpdcodes('REGION    ','M.REGION')
  
  M.SKUTMPL    = 'DEF'
  
  *WSH
  SELECT (lcImCSRep)
  IF SEEK(&lcImCust..A+&lcImCust..B)
    m.cSRep      = ALLTRIM(C)
    m.CustType   = ALLTRIM(D)
    m.CustPrty   = ALLTRIM(E)
    m.SalesGoal  = ALLTRIM(F)
    m.SalesGlLyr = ALLTRIM(G)
  ELSE
    m.cSRep      = ''
    m.CustType   = ''
    m.CustPrty   = ''
    m.SalesGoal  = ''
    m.SalesGlLyr = ''
  ENDIF
  *WSH
  
  SELECT CUSTOMER
  IF !SEEK('M'+M.ACCOUNT,'CUSTOMER')
    INSERT INTO CUSTOMER FROM MEMVAR
    =lfAdUsrInf()
  ELSE
    IF CUSTOMER.STNAME <> M.STNAME
      lcSvOrd = ORDER('CUSTOMER')
      SET ORDER TO STNAMETG
      
      IF !SEEK('S'+M.ACCOUNT+M.STNAME,'CUSTOMER')
        M.TYPE  = 'S'
        IF !SEEK(&lcImCust..A+&lcImCust..B, lcImCstShp) .OR. PADR(&lcImCust..C,30)<>PADR(&lcImCstShp..D,30)
          M.STORE = &lcImCust..B
          INSERT INTO CUSTOMER FROM MEMVAR
          =lfAdUsrInf()
        ELSE
          M.STORE = &lcImCstShp..C
          INSERT INTO CUSTOMER FROM MEMVAR
          =lfAdUsrInf()
        ENDIF
      ENDIF
      SET ORDER TO &lcSvOrd
    
    *WSH [Start]
    ELSE
      IF (!INLIST(CUSTOMER.CDIVISION, '20') AND M.CDIVISION = '20') OR ;
         (!INLIST(CUSTOMER.CDIVISION, '20', '40') AND M.CDIVISION = '40') OR ;
         (!INLIST(CUSTOMER.CDIVISION, '20', '40', '90') AND M.CDIVISION = '90') OR ;
         (!INLIST(CUSTOMER.CDIVISION, '20', '40', '90', '95') AND M.CDIVISION = '95')
        SELECT CUSTOMER
        GATHER MEMVAR
      ENDIF
    *WSH [End]
    
    ENDIF
  ENDIF
  
  IF !SEEK(M.ACCOUNT+gcCurrYear, 'ARCUSHST')
    SELECT ARCUSHST
    FOR lnFsYear = -1 TO 1
      APPEND BLANK
      REPLACE ACCOUNT   WITH M.ACCOUNT ;
              CFISFYEAR WITH STR(VAL(gcCurrYear)+lnFsYear,4)
      =lfAdUsrInf()
    ENDFOR 
  ENDIF
  
  *- Updating stores if any ( here if the id is repeated over the mastre customer file )
  SELECT &lcImCstShp
  =SEEK(&lcImCust..A+&lcImCust..B, lcImCstShp)
  SCAN REST WHILE &lcImCstShp..A+&lcImCstShp..B = &lcImCust..A+&lcImCust..B
    SELECT CUSTOMER
    SCATTER MEMVAR       
    
    M.TYPE        = 'S'
    M.STORE       = PADR(ALLTRIM(&lcImCstShp..C),8)
    M.STNAME      = ALLTRIM(&lcImCstShp..D)
    
    M.CADDRESS1   = ALLTRIM(&lcImCstShp..E) 
    *M.CADDRESS12  = ALLTRIM(&lcImCstShp..E) 
    M.CADDRESS2   = ALLTRIM(&lcImCstShp..F) 
    *M.CADDRESS22  = ALLTRIM(&lcImCstShp..F) 
    M.CADDRESS3   = ALLTRIM(&lcImCstShp..G) 
    *M.CADDRESS32  = ALLTRIM(&lcImCstShp..G) 
    M.CADDRESS4   = ALLTRIM(&lcImCstShp..H) 
    *M.CADDRESS42  = ALLTRIM(&lcImCstShp..H) 
    M.CADDRESS5   = ALLTRIM(&lcImCstShp..I) 
    *M.CADDRESS52  = ALLTRIM(&lcImCstShp..I) 
    
    M.PHONE1      = CHRTRAN(ALLTRIM(&lcImCstShp..Q), ' -()', '')
    M.FAX         = CHRTRAN(ALLTRIM(&lcImCstShp..R), ' -()', '')
    
    M.SALESREP    = ALLTRIM( SUBSTR(&lcImCstShp..O,2) )
    IF !EMPTY(M.SALESREP) .AND. !SEEK(M.SALESREP,'SALESREP')
      M.SALESREP = ''
    ENDIF
    
    SELECT CUSTOMER
    IF !SEEK('S'+M.ACCOUNT+M.STORE,'CUSTOMER')
      INSERT INTO CUSTOMER FROM MEMVAR
      =lfAdUsrInf()
    ELSE
      IF CUSTOMER.STNAME <> PADR(&lcImCstShp..D,30)
        
        *WSH [Start]
        *IF CUSTOMER.CDIVISION <> PADR(&lcImCstShp..A,6) 
        *  M.STORE = ALLTRIM(&lcImCstShp..C)+'/'+ALLTRIM(&lcImCstShp..A)
        *ELSE
        *  M.STORE = ALLTRIM(&lcImCstShp..C)+'/'+ALLTRIM(&lcImCstShp..B)
        *ENDIF
        lnSeqNo = 1
        M.STORE = PADR(ALLTRIM(&lcImCstShp..C) + '_' + ALLTRIM(STR(lnSeqNo)), 8)
        
        DO WHILE SEEK('S'+M.ACCOUNT+M.STORE,'CUSTOMER')
          lnSeqNo = lnSeqNo + 1
          M.STORE = PADR(ALLTRIM(&lcImCstShp..C) + '_' + ALLTRIM(STR(lnSeqNo)), 8)
        ENDDO
        *WSH [End]
        
        INSERT INTO CUSTOMER FROM MEMVAR
        =lfAdUsrInf()
      ENDIF
    ENDIF
  ENDSCAN
  
  *--mhm
  IF SEEK(&lcImCust..A+&lcImCust..B, lcImCstMem) AND !SEEK("A" + M.ACCOUNT, "NotePad")
    SELECT NOTEPAD
    SCATTER MEMVAR MEMO BLANK
    
    M.TYPE   = "A"
    M.KEY    = M.ACCOUNT
    M.CDESC  = "Notes For Account Number : " + M.ACCOUNT
    
    *WSH
    lcCurrHearder = &lcImCstMem..C
    M.MNOTES = SPACE(2) + REPLICATE("*", 10) + SPACE(2) + ALLTRIM(&lcImCstMem..C) +;
               IIF(EMPTY(&lcImCstMem..E), "", ' "' + ALLTRIM(&lcImCstMem..E) + '"') + SPACE(2) + "**" + SPACE(2) +;
               ALLTRIM(&lcImCstMem..F) + SPACE(2) + "**" + SPACE(2) +;
               ALLTRIM(&lcImCust..B) + " - " + ALLTRIM(&lcImCust..A) + SPACE(2) + REPLICATE("*", 10) + CHR(13) + CHR(13)
    *WSH
    
    SELECT (lcImCstMem)
    SKIP
    SCAN REST WHILE A+B+C = &lcImCust..A+&lcImCust..B
      
      *WSH
      IF C # lcCurrHearder
        lcCurrHearder = C
        M.MNOTES = M.MNOTES + CHR(13) + CHR(13) + SPACE(2) + REPLICATE("*", 10) + SPACE(2) + ALLTRIM(&lcImCstMem..C) +;
                   IIF(EMPTY(&lcImCstMem..E), "", ' "' + ALLTRIM(&lcImCstMem..E) + '"') + SPACE(2) + "**" + SPACE(2) +;
                   ALLTRIM(&lcImCstMem..F) + SPACE(2) + "**" + SPACE(2) +;
                   ALLTRIM(&lcImCust..B) + " - " + ALLTRIM(&lcImCust..A) + SPACE(2) + REPLICATE("*", 10) + CHR(13) + CHR(13)
      ENDIF
      *WSH
      
      M.MNOTES = M.MNOTES + ALLTRIM(&lcImCstMem..J)
    ENDSCAN
    
    SELECT NOTEPAD
    APPEND BLANK
    GATHER MEMVAR MEMO
    =lfAdUsrInf()
  ENDIF
  *--mhm

  *T20060920.0009 09/19/2006 [START] Add customer contacts  
  SELECT &lcImCstCnt
  =SEEK(&lcImCust..A+&lcImCust..B, lcImCstCnt )
  SCAN REST WHILE &lcImCstCnt..A+&lcImCstCnt..B = &lcImCust..A+&lcImCust..B
  
    IF !SEEK("C" + PADR(M.ACCOUNT,8) + SPACE(8) + PADR(ALLT(&lcImCust..C),30), 'CONTACT')
      SELECT Contact
      SCATTER MEMVAR MEMO BLANK
    
      M.CCONTTYPE  = "C"
      M.CCONT_ID   = M.ACCOUNT
      M.CCONT_CODE = ALLTRIM(&lcImCstCnt..C)
      M.CONTACT    = IIF(EMPTY(&lcImCstCnt..D), M.CCONT_CODE, ALLTRIM(&lcImCstCnt..D))
      M.PHONE      = ALLTRIM(&lcImCstCnt..N)
      M.FAX        = ALLTRIM(&lcImCstCnt..R)
      M.CCONTTTL   = ALLTRIM(&lcImCstCnt..M)
      M.CCONTSALUT = ALLTRIM(&lcImCstCnt..L)
      M.CEMAIL_ADD = ALLTRIM(&lcImCstCnt..S)
      M.CADDRESS1  = ALLTRIM(&lcImCstCnt..E)
      M.CADDRESS2  = ALLTRIM(&lcImCstCnt..F) + IIF(!EMPTY(&lcImCstCnt..G + &lcImCstCnt..F), ", ", "") + ALLTRIM(&lcImCstCnt..G)
      M.CADDRESS3  = ALLTRIM(&lcImCstCnt..H)
      M.CADDRESS4  = ALLTRIM(&lcImCstCnt..I)
      M.CADDRESS5  = ALLTRIM(&lcImCstCnt..J)
      M.CADDRESS6  = ALLTRIM(&lcImCstCnt..K)
      M.PHONE1     = ALLTRIM(&lcImCstCnt..P)
      M.MNOTE      = ALLTRIM(&lcImCstCnt..T)
      m.CEMAIL_ADD = ALLTRIM(&lcImCstCnt..S)
      
      IF LEN(M.CADDRESS2) > LEN(Contact.CADDRESS2)
        =lfLogData('Contact ' + PADR(M.CCONT_ID, LEN(CONTACT.CCONT_ID)) + ' Address Line 3 has been trancated.', RECNO(lcImCstCnt) ,.F., lcImCstCnt)
      ENDIF
    
      APPEND BLANK
      GATHER MEMVAR MEMO
      =lfAdUsrInf()
    ENDIF
  ENDSCAN
  *T20060920.0009 09/19/2006 [END  ] Add customer contacts  
  
ENDSCAN
WAIT CLEAR

=lfLogData('Customers data converted successfully')

*-- end of lfCustomers.

*:**************************************************************************
*:* Name        : lfColors
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/10/2005
*:* Purpose     : Import colors file
*:***************************************************************************
FUNCTION lfColors

*-Import colors file'
SELECT &lcImColors
REPLACE ALL A WITH ALLTRIM(A) ;
            B WITH IIF(EMPTY(B) , A , B)
GO TOP
SCAN FOR !EMPTY(&lcImColors..B)
  WAIT WINDOW NOWAIT 'Color :' + &lcImColors..A
  =lfUpdcodes('COLOR','M.COLOR',ALLTRIM(A),ALLTRIM(B))
ENDSCAN
WAIT CLEAR
=lfLogData('Colors data converted successfully')
*-- end of lfColors.

*:**************************************************************************
*:* Name        : lfTerms
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/10/2005
*:* Purpose     : Import Terms file
*:***************************************************************************
FUNCTION lfTerms

SELECT &lcImTerms
REPLACE H WITH '' ALL
GO TOP
SCAN
  
  *C126871,2 WSH [Start]
  IF EMPTY(B)
    LOOP
  ENDIF
  *C126871,2 WSH [End]
  
  WAIT WINDOW NOWAIT 'Term :'+&lcImTerms..A + '-' + &lcImTerms..B
  
  M.CTERMCODE = ALLTRIM(B)
  
  =lfUpdcodes('CTERMCODE ','M.CTERMCODE ')
  REPLACE H WITH M.CTERMCODE
  
  SELECT CODES
  SCATTER MEMVAR BLANK
  
  M.CCODE_NO  = M.CTERMCODE
  M.CDEFCODE  = 'N'
  M.CRLTFIELD = 'Y' 
  M.CFLD_NAME = 'CTERMCODE'
  M.ADD_USER = gcUser_ID
  M.ADD_DATE = gdSysDate
  M.ADD_TIME = TIME()
  
  *C126871,2 WSH [Start]
  IF !SEEK('N'+PADR('CTERMCODE',10)+PADR(M.CTERMCODE,6)+SPACE(30)+'NTERDUED ', 'CODES', 'CCODE_NO')
  *C126871,2 WSH [End]
  
    M.CRLTD_NAM = 'NTERDUED'
    M.CRLTD_TYP = 'N'
    M.CRLTD_VLU = ALLTRIM(&lcImTerms..C)
    INSERT INTO CODES FROM MEMVAR
  
  *C126871,2 WSH [Start]
    =lfAdUsrInf()
  ENDIF
  
  IF !SEEK('N'+PADR('CTERMCODE',10)+PADR(M.CTERMCODE,6)+SPACE(30)+'NTERDISCD', 'CODES', 'CCODE_NO')
  *C126871,2 WSH [End]
  
    M.CRLTD_NAM = 'NTERDISCD'
    M.CRLTD_TYP = 'N'
    M.CRLTD_VLU = ALLTRIM(&lcImTerms..E)
    INSERT INTO CODES FROM MEMVAR
  
  *C126871,2 WSH [Start]
    =lfAdUsrInf()
  ENDIF
  
  IF !SEEK('N'+PADR('CTERMCODE',10)+PADR(M.CTERMCODE,6)+SPACE(30)+'NTERDISCR', 'CODES', 'CCODE_NO')
  *C126871,2 WSH [End]
  
    M.CRLTD_NAM = 'NTERDISCR'
    M.CRLTD_TYP = 'N'
    M.CRLTD_VLU = ALLTRIM(&lcImTerms..J)
    INSERT INTO CODES FROM MEMVAR
  
  *C126871,2 WSH [Start]
    =lfAdUsrInf()
  ENDIF
  
  IF !SEEK('N'+PADR('CTERMCODE',10)+PADR(M.CTERMCODE,6)+SPACE(30)+'EOM ', 'CODES', 'CCODE_NO')
  *C126871,2 WSH [End]
  
    M.CRLTD_NAM = 'EOM'
    M.CRLTD_TYP = 'C'
    M.CRLTD_VLU = IIF( ALLTRIM(&lcImTerms..D) = 'D' , 'Y' , 'N' )
    INSERT INTO CODES FROM MEMVAR
  
  *C126871,2 WSH [Start]
    =lfAdUsrInf()
  ENDIF
  
  IF !SEEK('N'+PADR('CTERMCODE',10)+PADR(M.CTERMCODE,6)+SPACE(30)+'EOMDAY ', 'CODES', 'CCODE_NO')
  *C126871,2 WSH [End]
  
    M.CRLTD_NAM = 'EOMDAY'
    M.CRLTD_TYP = 'N'
    M.CRLTD_VLU = IIF( ALLTRIM(&lcImTerms..D) = 'D' , '21' , '0' )
    INSERT INTO CODES FROM MEMVAR
  
  *C126871,2 WSH [Start]
    =lfAdUsrInf()
  ENDIF
  
  IF !SEEK('N'+PADR('CTERMCODE',10)+PADR(M.CTERMCODE,6)+SPACE(30)+'CODYN ', 'CODES', 'CCODE_NO')
  *C126871,2 WSH [End]
  
    M.CRLTD_NAM = 'CODYN'
    M.CRLTD_TYP = 'C'
    M.CRLTD_VLU = 'N'
    INSERT INTO CODES FROM MEMVAR
  
  *C126871,2 WSH [Start]
    =lfAdUsrInf()
  ENDIF
  
  IF !SEEK('N'+PADR('CTERMCODE',10)+PADR(M.CTERMCODE,6)+SPACE(30)+'LINSTALLM', 'CODES', 'CCODE_NO')
  *C126871,2 WSH [End]
  
    M.CRLTD_NAM = 'LINSTALLM'
    M.CRLTD_TYP = 'L'
    M.CRLTD_VLU = 'F'
    INSERT INTO CODES FROM MEMVAR
  
  *C126871,2 WSH [Start]
    =lfAdUsrInf()
  ENDIF
  
  IF !SEEK('N'+PADR('CTERMCODE',10)+PADR(M.CTERMCODE,6)+SPACE(30)+'CASH_NON ', 'CODES', 'CCODE_NO')
  *C126871,2 WSH [End]
  
    M.CRLTD_NAM = 'CASH_NON' 
    M.CRLTD_TYP = 'C'
    M.CRLTD_VLU = 'C'
    INSERT INTO CODES FROM MEMVAR
  
  *C126871,2 WSH [Start]
    =lfAdUsrInf()
  ENDIF
  *C126871,2 WSH [End]
  
ENDSCAN
WAIT CLEAR
=lfLogData('Terms data converted successfully')

*- Use this in rest of the program to update terms codes
SELECT &lcImTerms
INDEX ON A TAG A

*-- end of lfColors.

*:**************************************************************************
*:* Name        : lfWarehous
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/10/2005
*:* Purpose     : Import WH data
*:***************************************************************************
FUNCTION lfWarehous
*-Import WH data'
SELECT &lcImWareH
SCAN
  SELECT WAREHOUS
  SCATTER MEMVAR BLANK
  
  WAIT WINDOW NOWAIT 'WH :'+&lcImWareH..A
  
  M.CWARECODE = ALLTRIM(&lcImWareH..A)
  M.CDESC     = ALLTRIM(&lcImWareH..B)
  M.CADDRESS1 = ALLTRIM(&lcImWareH..D)
  M.CADDRESS3 = SUBSTR( &lcImWareH..E , 1 , AT(',' , &lcImWareH..E )-1 )
  M.CADDRESS4 = LEFT( ALLTRIM(SUBSTR( &lcImWareH..E , AT(',' , &lcImWareH..E )+1 )) , 2 )
  M.CADDRESS4 = IIF( RIGHT(ALLTRIM(&lcImWareH..E), 5 )='     ',&lcImWareH..E, RIGHT(ALLTRIM(&lcImWareH..E), 5 ) )
  M.CADDRESS6 = 'USA'
  M.CCONT_CODE= 'USA'
  
  M.LSTYINV   = .T.
  
  *C126871,2 WSH [Start] 
  &lcMas90Fld. = ALLTRIM(&lcImWareH..A)
  *C126871,2 WSH [End]
  
  IF !SEEK(M.CWARECODE,'WAREHOUS')
    INSERT INTO WAREHOUS FROM MEMVAR
    =lfAdUsrInf(.F.,'WAREHOUS')
  ENDIF
  
ENDSCAN
WAIT CLEAR

=lfLogData('Warehouses data converted successfully')

*-- end of lfWarehous.


*:**************************************************************************
*:* Name        : lfScale
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/10/2005
*:* Purpose     : Update scale file
*:***************************************************************************
FUNCTION lfScale

*-Update scale file'
SELECT &lcImPrdlin
SCAN FOR &lcImPrdlin..A = ' '
  
  WAIT WINDOW NOWAIT 'Scale:'+&lcImPrdlin..A
  
  SELECT SCALE 
  SCATTER MEMVAR BLANK
  M.TYPE = 'S'
  M.SCALE = ALLTRIM(&lcImPrdlin..A)
  M.CSCL_DESC = ALLTRIM(&lcImPrdlin..B)
  M.SZ1 = ALLTRIM(PADR(&lcImPrdlin..C,20))
  M.SZ2 = ALLTRIM(PADR(&lcImPrdlin..D,20))
  M.SZ3 = ALLTRIM(PADR(&lcImPrdlin..E,20))
  M.SZ4 = ALLTRIM(PADR(&lcImPrdlin..F,20))
  M.SZ5 = ALLTRIM(PADR(&lcImPrdlin..G,20))
  M.SZ6 = ALLTRIM(PADR(&lcImPrdlin..H,20))
  M.SZ7 = Strtran( ALLTRIM(PADR(&lcImPrdlin..I,20)) , '0' )
  M.SZ8 = Strtran( ALLTRIM(PADR(&lcImPrdlin..J,20)) , '0' )
  M.CNT = IIF(!EMPTY(M.SZ1),1,0)+IIF(!EMPTY(M.SZ2),1,0)+IIF(!EMPTY(M.SZ3),1,0)+IIF(!EMPTY(M.SZ4),1,0)+;
          IIF(!EMPTY(M.SZ5),1,0)+IIF(!EMPTY(M.SZ6),1,0)+IIF(!EMPTY(M.SZ7),1,0)+IIF(!EMPTY(M.SZ8),1,0)
  
  *C126871,2 WSH [Start]
  &lcMas90Fld. = ALLTRIM(&lcImPrdlin..A)
  *C126871,2 WSH [End]
  
  IF !SEEK('S'+M.SCALE,'SCALE')
    APPEND BLANK
    GATHER MEMVAR
    =lfAdUsrInf()
  ENDIF          
ENDSCAN
WAIT CLEAR
SELECT SCALEHD
LOCATE
IF EOF()
  APPEND BLANK
ENDIF

=lfLogData('Scales data converted successfully')

*-- end of lfScale.

*:**************************************************************************
*:* Name        : lfStyle
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/10/2005
*:* Purpose     : Append style data
*:***************************************************************************
FUNCTION lfStyle
PRIVATE lcDefWH 
SELECT &lcImPrdlin

*-Append style data'

WAIT WINDOW NOWAIT 'Indexing...'
INDEX ON UPPER(PADR(A,6)) TAG A

*SELECT &lcImMstMrx

SELECT &lcImItmVnd
INDEX ON PADR(A,19) TAG A

SELECT &lcImWhDMtx
INDEX ON PADR(A,19)+B+C TAG ABC

GO TOP IN WAREHOUS
lcDefWH = WAREHOUS.CWARECODE

SELECT &lcImInvMst
GO TOP
SCAN
  
  
  *- Check stymajor width
  IF EMPTY(&lcImInvMst..A)
    LOOP
  ENDIF
  
  IF LEN(ALLTRIM(&lcImInvMst..A)) > 12
    =lfLogData('Style '+ PADR(ALLTRIM(&lcImInvMst..A),19) + ' has major width > 12 characters.',RECNO(lcImInvMst) ,.F.,lcImInvMst)
    LOOP
  ENDIF

  &&seek to get the color
  IF !SEEK(PADR(&lcImInvMst..A,19),lcImWhDMtx)
    =lfLogData('Style '+ PADR(ALLTRIM(&lcImInvMst..A),19) + ' has no colors.',RECNO(lcImInvMst) ,.F.,lcImInvMst)
    LOOP
  ENDIF

  SELECT &lcImWhDMtx
  SCAN REST WHILE PADR(A,19)+B+C = PADR(&lcImInvMst..A,19)
  
    SELECT STYLE
    SCATTER MEMVAR BLANK
    
    SELECT &lcImInvMst
  
    M.CSTYMAJOR  = ALLTRIM(&lcImInvMst..A)
    IF EMPTY(M.CSTYMAJOR)
      LOOP
    ENDIF
    
    *C126871,2 WSH [Start]
    &lcMas90Fld. = ALLTRIM(&lcImInvMst..A)
    *C126871,2 WSH [End]
    
    WAIT WINDOW NOWAIT 'Style :'+M.CSTYMAJOR
    
    M.STYLE      = PADR(M.CSTYMAJOR,12) + '-' + PADR(&lcImWhDMtx..C,6)
      
    M.CDYE_FLG   = 'N'
    M.PATTERN    = ''
    M.STATUS     = 'A'
    M.CSTYGROUP  = ALLTRIM(&lcImInvMst..C)    && product line field
    
    *- seek in product line file to get scale  
    IF !SEEK(PADR(ALLT(&lcImInvMst..C),6),lcImPrdlin)  
      =lfLogData('Style '+ PADR(ALLTRIM(&lcImInvMst..A),19) + ' has no scale defined.',RECNO(lcImInvMst) ,.F.,lcImInvMst)
      LOOP
    ENDIF
    M.SCALE      = ALLTRIM(&lcImPrdlin..C)
    
    M.PREPAK     = ''
    M.DESC       = ALLTRIM(&lcImInvMst..B)
    M.DESC1      = ALLTRIM(&lcImInvMst..B) + '-' + gfCodDes(PADR(&lcImWhDMtx..C,6) , 'COLOR' )
    
    *- Assign the default season
    =lfUpdcodes('SEASON'   ,'M.SEASON') 
    
    M.CSTYGRADE  = '1'
    M.QTY_CTN    = 0
    M.COMMISSION = .T.
    M.LINK_CODE  = 'DEFDEF'
    M.MAKE       = .F.
    M.CPRICECUR  = 'USD'
    M.CDUTYCUR   = 'USD'
    M.LINVSTY    = ( ALLTRIM(&lcImInvMst..I) == 'Y' )
    M.PRICEA     = VAL(&lcImInvMst..BE)
    M.NCURRRATE  = 0
    M.NFRGNPRICE = 0
    M.LUPCYN     = .F.
    M.NSUGRETPRI = Val(&lcImInvMst..BD) 
    M.CDEFWARE   = IIF(!EMPTY(&lcImInvMst..N), ALLTRIM(&lcImInvMst..N) , lcDefWH ) 
    M.AVE_COST   = VAL(&lcImInvMst..BH)
    M.NTAXBREAK  = 1
    
    M.VENDOR     = ALLTRIM(&lcImInvMst..Q)
    *- seek to get the vendor code
    IF EMPTY(M.VENDOR) .AND. SEEK(PADR(&lcImInvMst..A,19),lcImItmVnd)
      M.VENDOR     = ALLTRIM(&lcImItmVnd..C)
      M.LEADTIME   = VAL(&lcImItmVnd..N)
    ENDIF
    
    M.CSLSGLLINK = 'DEF'
    M.LDETCOST   = .T.
  
    =lfUpdcodes('CDIVISION ','M.CDIVISION')
    
    IF !SEEK(M.STYLE,'STYLE')
      INSERT INTO STYLE FROM MEMVAR
      =lfAdUsrInf(.F.,'STYLE')
    ENDIF

    *:****************************  ICSTYHST  ****************************:*

    SELECT ICSTYHST
    IF !SEEK(M.STYLE + gcCurrYear,'ICSTYHST')
      FOR lnFsYear = -1 TO 1
        APPEND BLANK
        REPLACE Style     WITH M.STYLE ;
                cFisFYear WITH STR(VAL(gcCurrYear)+lnFsYear,4)
        =lfAdUsrInf()
      ENDFOR
    ENDIF

    *:****************************  ICSEGVAL  ****************************:*

    SELECT ICSEGVAL
    IF !SEEK('1'+M.STYLE,'ICSEGVAL')
      APPEND BLANK
      REPLACE CISEGNO   WITH '1' ;
              CISEGVAL  WITH M.CSTYMAJOR ;
              CISGVALSD WITH STYLE.DESC  ;
              CISGVALLD WITH STYLE.DESC1
      =lfAdUsrInf()
    ENDIF   
   
    *:****************************  STYDYE  ****************************:*    
   
    M.CWARECODE  = PADR(ALLTRIM(&lcImWhDMtx..B),6)
    M.CWARECODE  = IIF(EMPTY(M.CWARECODE),M.CDEFWARE,M.CWARECODE)  && if warehous code is empty add the default warehous code
    IF !SEEK(M.CWARECODE,'WAREHOUS')
      =lfLogData('Style '+ PADR(ALLTRIM(&lcImInvMst..A),19) + ' Warehous code not found in warehous file.',RECNO(lcImInvMst) ,.T.,lcImInvMst)
      LOOP
    ENDIF
    M.GL_LINK    = 'DEFDEF'
    M.AVE_COST   = VAL(&lcImWhDMtx..AQ)    && CurrentAverageCost    

    IF !SEEK(M.STYLE+M.CWARECODE+'          ','STYDYE')
      INSERT INTO STYDYE FROM MEMVAR
      =lfAdUsrInf(.F.,'STYDYE')
    ENDIF
  ENDSCAN
ENDSCAN  
WAIT CLEAR
  *-- M.end of lfStyle.
  
*:**************************************************************************
*:* Name        : lfVendor
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/10/2005
*:* Purpose     : Append vendor data
*:***************************************************************************
FUNCTION lfVendor
PRIVATE lnFsYear

*C126871,2 WSH [Start]
SELECT (lcImTrmRef)
SET ORDER TO A

SELECT (lcImVenCnt)
INDEX ON B TAG B
SET ORDER TO B

SELECT (lcImVenMem)
INDEX ON B+C+D TAG BCD
SET ORDER TO BCD
*C126871,2 WSH [End]

*-Append vendor data'
SELECT &lcImVendor

WAIT WINDOW NOWAIT 'Indexing...'
INDEX ON B TAG B

SCAN

  WAIT WINDOW NOWAIT 'Vendor :'+&lcImVendor..B
  
  SELECT APVENDOR 
  SCATTER MEMVAR BLANK
  
  *C126871,2 WSH [Start]
  *M.CVENDCODE  = ALLTRIM(&lcImVendor..B)
  M.CVENDCODE  = PADR(ALLTRIM(&lcImVendor..B), 8)
  
  &lcMas90Fld. = &lcImVendor..B
  *C126871,2 WSH [End]
  
  M.CVENCOMP   = IIF(!EMPTY(&lcImVendor..C) , ALLTRIM(&lcImVendor..C) , M.CVENDCODE )
  M.LVENMISC   = .F.
  M.CADDRESS1  = ALLTRIM(&lcImVendor..D)
  M.CADDRESS2  = ALLTRIM(&lcImVendor..E)
  M.CADDRESS3  = ALLTRIM(&lcImVendor..F)
  M.CADDRESS4  = ALLTRIM(&lcImVendor..G)
  M.CADDRESS5  = ALLTRIM(&lcImVendor..H)
  M.CADDRESS6  = ALLTRIM(&lcImVendor..M)
  M.CCONT_CODE = 'USA'
  
  M.CPHONENO   = CHRTRAN( ALLTRIM(&lcImVendor..I) , ' -()' , '' ) + ALLTRIM(&lcImVendor..J)
  M.CFAXNO     = CHRTRAN( ALLTRIM(&lcImVendor..AL), ' -()' , '' )
  M.CDIVISION  = ALLTRIM(&lcImVendor..A)
  
  M.CVENTAXID  = ALLTRIM(&lcImVendor..Z)
  
  *C126871,2 WSH [Start]
  *=SEEK(ALLTRIM(&lcImCust..T) , lcImTerms )
  *M.CTERMCODE  = ALLTRIM(&lcImTerms..H)
  M.CTERMCODE  = ALLTRIM(&lcImVendor..T)
  
  IF !EMPTY(M.CTERMCODE)
    IF SEEK(PADR(M.CTERMCODE, LEN(EVALUATE(lcImTrmRef + '.E'))), lcImTrmRef)
      M.CTERMCODE = ALLTRIM(EVALUATE(lcImTrmRef + '.G'))
      
      IF SEEK(PADR(M.CTERMCODE, LEN(EVALUATE(lcImTerms + '.A'))), lcImTerms)
        M.CTERMCODE = ALLTRIM(EVALUATE(lcImTerms + '.H'))
      ELSE
        M.CTERMCODE = ''
      ENDIF
    ELSE
      M.CTERMCODE = ''
    ENDIF
  ENDIF
  *C126871,2 WSH [End]
  
  M.DVENLPAYD  = ALLTRIM(&lcImVendor..AB)
  M.DVENLPORD  = ALLTRIM(&lcImVendor..AA)
  M.CVENOURAC  = ALLTRIM(&lcImVendor..R)
  
  M.CVENCOMNT  = ALLTRIM(&lcImVendor..V)
  
  M.CVEN1099T  = ALLTRIM(&lcImVendor..Y)
  =lfUpdcodes('CVEN1099T','M.CVEN1099T')
  
  M.CVENPRIOR  = '1'
  M.CVENPMETH  = 'P'
  M.CTAXTYPE   = 'T'
  
  M.CCURRCODE  = 'USD'
  M.CVENSUPTYP = 'S'
  
  IF !SEEK(M.CVENDCODE,'APVENDOR')
    APPEND BLANK
    GATHER MEMVAR
    
    *C126871,2 WSH [Start]
    =lfAdUsrInf()
    *C126871,2 WSH [End]
    
    *- Vendor history file
    && VENDYEAR : CVENDCODE+CFISFYEAR
    IF !SEEK(M.CVENDCODE+gcCurrYear,'APVENHST')
      SELECT APVENHST
      FOR lnFsYear = -1 TO 1
        APPEND BLANK
        REPLACE CVENDCODE WITH M.CVENDCODE ;
                CFISFYEAR WITH STR(VAL(gcCurrYear)+lnFsYear,4)
        =lfAdUsrInf()
      ENDFOR 
    ENDIF 
    
    *C126871,2 WSH [Start]
    IF SEEK(M.CVENDCODE, lcImVenCnt) AND !SEEK("V" + M.CVENDCODE + SPACE(8) + ALLTRIM(&lcImVenCnt..D), "Contact")
      SELECT Contact
      SCATTER MEMVAR MEMO BLANK
      
      *T20060920.0009 09/19/2006 loop to append vendor contacts [Start]
      SELECT &lcImVenCnt
      SCAN REST WHILE PADR(ALLTRIM(&lcImVendor..B), 8) = M.CVENDCODE
        *T20060920.0009 09/19/2006 [end]
        
        M.CCONTTYPE  = "V"
        M.CCONT_ID   = M.CVENDCODE
        M.CCONT_CODE = ALLTRIM(&lcImVenCnt..C)
        M.CONTACT    = IIF(EMPTY(&lcImVenCnt..D), M.CCONT_CODE, ALLTRIM(&lcImVenCnt..D))
        M.PHONE      = ALLTRIM(&lcImVenCnt..N)
        M.FAX        = ALLTRIM(&lcImVenCnt..R)
        M.CCONTTTL   = ALLTRIM(&lcImVenCnt..M)
        M.CCONTSALUT = ALLTRIM(&lcImVenCnt..L)
        M.CEMAIL_ADD = ALLTRIM(&lcImVenCnt..S)
        M.CADDRESS1  = ALLTRIM(&lcImVenCnt..E)
        M.CADDRESS2  = ALLTRIM(&lcImVenCnt..F) + IIF(!EMPTY(&lcImVenCnt..G + &lcImVenCnt..F), ", ", "") + ALLTRIM(&lcImVenCnt..G)
        M.CADDRESS3  = ALLTRIM(&lcImVenCnt..H)
        M.CADDRESS4  = ALLTRIM(&lcImVenCnt..I)
        M.CADDRESS5  = ALLTRIM(&lcImVenCnt..J)
        M.CADDRESS6  = ALLTRIM(&lcImVenCnt..K)
        M.PHONE1     = ALLTRIM(&lcImVenCnt..P)
        M.MNOTE      = ALLTRIM(&lcImVenCnt..T)
        *T20060920.0009 09/19/2006 [start] adding contact email 
        m.CEMAIL_ADD = ALLTRIM(&lcImVenCnt..S)
        *T20060920.0009 [end]
        IF LEN(M.CADDRESS2) > LEN(Contact.CADDRESS2)
          =lfLogData('Contact ' + PADR(M.CCONT_ID, LEN(CONTACT.CCONT_ID)) + ' Address Line 3 has been trancated.', RECNO(lcImVenCnt) ,.F., lcImVenCnt)
        ENDIF
        
        SELECT Contact
        APPEND BLANK
        GATHER MEMVAR MEMO
        =lfAdUsrInf()
        
        *T20060920.0009 09/19/2006 [start] 
      ENDSCAN
      *T20060920.0009 09/19/2006
   
    ENDIF
    
    IF SEEK(M.CVENDCODE, lcImVenMem) AND !SEEK("H" + M.CVENDCODE, "NotePad")
      SELECT NOTEPAD
      SCATTER MEMVAR MEMO BLANK
      
      M.TYPE   = "H"
      M.KEY    = M.CVENDCODE
      M.CDESC  = "Notes For Vendor Number : " + M.CVENDCODE
      
      lcCurrHearder = &lcImCstMem..C
      M.MNOTES = SPACE(2) + REPLICATE("*", 10) + SPACE(2) + ALLTRIM(&lcImVenMem..C) +;
                 IIF(EMPTY(&lcImVenMem..E), "", ' "' + ALLTRIM(&lcImVenMem..E) + '"') + SPACE(2) + "**" + SPACE(2) +;
                 ALLTRIM(&lcImVenMem..F) + SPACE(2) + "**" + SPACE(2) +;
                 ALLTRIM(&lcImVendor..B) + SPACE(2) + REPLICATE("*", 10) + CHR(13) + CHR(13)
      
      ** M.MNOTES = SPACE(2) + IIF(EMPTY(&lcImVenMem..E), ALLTRIM(&lcImVenMem..C), ALLTRIM(&lcImVenMem..E)) + SPACE(1) + ALLTRIM(&lcImVenMem..F)
      
      SELECT (lcImVenMem)
      SKIP
      SCAN REST WHILE B = M.CVENDCODE
        IF C # lcCurrHearder
          lcCurrHearder = C
          M.MNOTES = M.MNOTES + CHR(13) + CHR(13) + SPACE(2) + REPLICATE("*", 10) + SPACE(2) + ALLTRIM(&lcImVenMem..C) +;
                     IIF(EMPTY(&lcImVenMem..E), "", ' "' + ALLTRIM(&lcImVenMem..E) + '"') + SPACE(2) + "**" + SPACE(2) +;
                     ALLTRIM(&lcImVenMem..F) + SPACE(2) + "**" + SPACE(2) +;
                     ALLTRIM(&lcImVendor..B) + SPACE(2) + REPLICATE("*", 10) + CHR(13) + CHR(13)
        ENDIF
        
        M.MNOTES = M.MNOTES + ALLTRIM(&lcImVenMem..J)
      ENDSCAN
      
      SELECT NOTEPAD
      APPEND BLANK
      GATHER MEMVAR MEMO
      =lfAdUsrInf()
    ENDIF
    *C126871,2 WSH [End]
    
  ENDIF
  
ENDSCAN
WAIT CLEAR
*-- end of lfVendor.

*:**************************************************************************
*:* Name        : lfvHelp
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/13/2005
*:* Purpose     : Show instruction screen
*:***************************************************************************
FUNCTION lfvHelp
PRIVATE lnSlct,lcHlp,lcCr
lnSlct = SELECT()

*=gfModalGen('INM00000B00000',.F.,.F.,.F.,'Show instruction screen')

lcCr = CHR(13)
lcHlp = '' 
lcHlp = lcHlp + 'Convert instructions' + lcCr
lcHlp = lcHlp + '' + lcCr 
lcHlp = lcHlp + '' + lcCr 
lcHlp = lcHlp + '' + lcCr 
lcHlp = lcHlp + '' + lcCr 
lcHlp = lcHlp + '' + lcCr 
lcHlp = lcHlp + '-----------------------' + lcCr


lcWinTitl  = "Instruction screen"
CREATE CURSOR TMPSTR (mStrRep M(120))
APPEND BLANK  
REPLACE mStrRep WITH lcHlp

*- Call Log screen
DO (gcScrDir + 'SM\SMSTRREP.SPR')
*DO (gcScrDir + 'SM\SMLOG.SPR')

USE IN TMPSTR
SELECT (lnSlct)
*-- end of lfvHelp.

*:**************************************************************************
*:* Name        : lfShipVia
*:* Developer   : Wael M. Abo-Shawareb (WSH)
*:* Date        : 03/22/2006
*:* Purpose     : Import Ship Via Codes
*:***************************************************************************
*C126871,2
FUNCTION lfShipVia

PRIVATE lnAlias
lnAlias = SELECT(0)

SELECT (lcImShpRef)
SET ORDER TO I

LOCATE
SCAN
  WAIT WINDOW NOWAIT 'Ship Via: ' + &lcImShpRef..I
  
  M.SHIPVIA = ALLTRIM(I)
  =lfUpdcodes('SHIPVIA ','M.SHIPVIA')
ENDSCAN

WAIT CLEAR
=lfLogData('Ship Via data converted successfully')

SELECT (lnAlias)
RETURN

*:**************************************************************************
*:* Name        : lfReturns
*:* Developer   : Wael M. Abo-Shawareb (WSH)
*:* Date        : 03/22/2006
*:* Purpose     : Import Return Reasons Codes
*:***************************************************************************
*C126871,2
FUNCTION lfReturns

PRIVATE lnAlias
lnAlias = SELECT(0)

SELECT (lcImRetRef)
SET ORDER TO A

LOCATE
SCAN
  WAIT WINDOW NOWAIT 'Return Reason: ' + B
  
  M.REASON = ALLTRIM(B)
  =lfUpdcodes('REASON ','M.REASON')
  
  WAIT WINDOW NOWAIT 'Return Reason: ' + E
  
  M.REASON = ALLTRIM(E)
  =lfUpdcodes('REASON ','M.REASON')
ENDSCAN

WAIT CLEAR
=lfLogData('Return Reasons data converted successfully')

SELECT (lnAlias)
RETURN

*:**************************************************************************
*:* Name        : lfDivision
*:* Developer   : Wael M. Abo-Shawareb (WSH)
*:* Date        : 03/22/2006
*:* Purpose     : Import Divisions file
*:***************************************************************************
*C126871,2
FUNCTION lfDivision

SELECT &lcImDivisn
SCAN
  IF EMPTY(A)
    LOOP
  ENDIF
  
  WAIT WINDOW NOWAIT 'Division :' + &lcImDivisn..A + '-' + &lcImDivisn..B
  
  M.CDIVISION = ALLTRIM(A)
  =lfUpdcodes('CDIVISION ', 'M.CDIVISION', M.CDIVISION, &lcImDivisn..B)
  
  SELECT CODES
  SCATTER MEMVAR BLANK
  
  M.CCODE_NO  = M.CDIVISION
  M.CDEFCODE  = 'N'
  M.CRLTFIELD = 'Y' 
  M.CFLD_NAME = 'CDIVISION'
  M.ADD_USER  = gcUser_ID
  M.ADD_DATE  = gdSysDate
  M.ADD_TIME  = TIME()
  
  IF !SEEK('N'+PADR('CDIVISION',10)+PADR(M.CDIVISION,6)+SPACE(30)+'DIVLNAME', 'CODES', 'CCODE_NO')
    M.CRLTD_NAM = 'DIVLNAME'
    M.CRLTD_TYP = 'C'
    M.CRLTD_VLU = ''
    INSERT INTO CODES FROM MEMVAR
    =lfAdUsrInf()
  ENDIF
  
  IF !SEEK('N'+PADR('CDIVISION',10)+PADR(M.CDIVISION,6)+SPACE(30)+'LINK_CODE', 'CODES', 'CCODE_NO')
    M.CRLTD_NAM = 'LINK_CODE'
    M.CRLTD_TYP = 'C'
    M.CRLTD_VLU = ''
    INSERT INTO CODES FROM MEMVAR
    =lfAdUsrInf()
  ENDIF
  
  IF !SEEK('N'+PADR('CDIVISION',10)+PADR(M.CDIVISION,6)+SPACE(30)+'CSLSGLLINK', 'CODES', 'CCODE_NO')
    M.CRLTD_NAM = 'CSLSGLLINK'
    M.CRLTD_TYP = 'C'
    M.CRLTD_VLU = ''
    INSERT INTO CODES FROM MEMVAR
    =lfAdUsrInf()
  ENDIF
  
  IF !SEEK('N'+PADR('CDIVISION',10)+PADR(M.CDIVISION,6)+SPACE(30)+'DIVGROUP ', 'CODES', 'CCODE_NO')
    M.CRLTD_NAM = 'DIVGROUP'
    M.CRLTD_TYP = 'C'
    M.CRLTD_VLU = ''
    INSERT INTO CODES FROM MEMVAR
    =lfAdUsrInf()
  ENDIF
  
  IF !SEEK('N'+PADR('CDIVISION',10)+PADR(M.CDIVISION,6)+SPACE(30)+'CUPCMAN ', 'CODES', 'CCODE_NO')
    M.CRLTD_NAM = 'CUPCMAN'
    M.CRLTD_TYP = 'C'
    M.CRLTD_VLU = ''
    INSERT INTO CODES FROM MEMVAR
    =lfAdUsrInf()
  ENDIF
  
  IF !SEEK('N'+PADR('CDIVISION',10)+PADR(M.CDIVISION,6)+SPACE(30)+'CUPCGENTYP', 'CODES', 'CCODE_NO')
    M.CRLTD_NAM = 'CUPCGENTYP'
    M.CRLTD_TYP = 'C'
    M.CRLTD_VLU = ''
    INSERT INTO CODES FROM MEMVAR
    =lfAdUsrInf()
  ENDIF
  
  IF !SEEK('N'+PADR('CDIVISION',10)+PADR(M.CDIVISION,6)+SPACE(30)+'CDIVTYPE ', 'CODES', 'CCODE_NO')
    M.CRLTD_NAM = 'CDIVTYPE'
    M.CRLTD_TYP = 'C'
    M.CRLTD_VLU = 'A'
    INSERT INTO CODES FROM MEMVAR
    =lfAdUsrInf()
  ENDIF
  
  IF !SEEK('N'+PADR('CDIVISION',10)+PADR(M.CDIVISION,6)+SPACE(30)+'CIMGPATH ', 'CODES', 'CCODE_NO')
    M.CRLTD_NAM = 'CIMGPATH'
    M.CRLTD_TYP = 'C'
    M.CRLTD_VLU = ''
    INSERT INTO CODES FROM MEMVAR
    =lfAdUsrInf()
  ENDIF
ENDSCAN

WAIT CLEAR
=lfLogData('Divisions data converted successfully')

RETURN
*-- end of lfColors.
*:***************************************************************************
