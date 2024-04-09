*:****************************************************************
*: Program file  : ALIMPLW
*: Program desc. : Import M&S File.
*: System        : Aria Apparel System - Version 2.7.
*: Module        : Sales Order Allocation [AL]
*: Developer     : ABDOU ELGENDI -  [ABD]
*: Date          : 08/28/2003
*: Tracking Job Number : C200572,1
*:****************************************************************
*: Calls         : G.FUNCTIONS: gfSetup
*:               : FUNCTIONS  : lfProceed , lfGetFile , lfvMs3File
*:               :            : lfUpPikQty, lfOpenFls , lfClosFls
*:               :            : lfErrLog  , lfvPrint  , lfUpdtPick
*:               :            : lfUpdtPck2, lfgetStyle,lfGenPiktk
*:               :            : lfEscap.
*:               -----------------------------------------------
*:               : PROCEDURE  : lpSaveOrd.
*:****************************************************************
*: Passed Parameters  : None.
*:****************************************************************
*: Aria4 tracking: C200572,1
*: Developer     : Ahmed Salah
*: Date          : 02/10/2007
*:******************************************************************************************************
*:Modifications  :
*:B608250,1 SSH 02/09/2007 Get warehouse code form MS3 file and use it to search for the Proper Sales Order
*:C200851,1 HIA 09/10/2007 Processing Discrepancy files
*:C200863,1 MMT 10/03/2007 Update Original Qty fields in ALcaloff Table[T20070822.0012]
*:C200863,2 MMT 09/22/2008 Change Hist. File Name[T20070822.0012]
*:C201105,1 MMT 02/16/2009 Add packs to M&S import call off program for low profile [T20081208.0001]
*:B608881,1 AHS 05/29/2009 M&S import program at Low Profile creating invalid Call Off No
*:******************************************************************************************************
*:

*-- lcTempMs3   :- Temp File hold the MS3 Data File.
*-- laOpenFile  :- Array Hold the Opened Files.
*-- lnLineNo    :- Variable to hold the printed line number at error file.
*-- lcOrdline   :- Variable to hold the order line file.
*-- lcTmpError  :- Variable to hold the all the errors occures while import the data.

DO FORM (oAriaApplication.ScreenHome+'AL\ALIMPLW')


*-- END of Code.
*:*************************************************************
*: Name      : lfProceed
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 08/28/2003
*: Purpose   : Funtion to collect the data From text file.
*:*************************************************************
*: Calls     :
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfProceed ()
*:*************************************************************
*:
FUNCTION lfProceed
  LPARAMETERS lctxtfile,oForm
  *C200851.exe HIA, 2007/09/10 Processing Discrepancy files [BEGIN]
  *DIMENSION laOpenFile[08,3]
  DIMENSION laOpenFile[10,3]

  *--THE COLOR LENGTH
  STORE 0 TO lncolorlen, lnColorpos
  DECLARE laItemSeg[1]
  =gfItemMask(@laItemSeg)
  FOR lnCount = 1 TO ALEN(laItemSeg,1)
    IF laItemSeg[lnCount,1]='C'
      lncolorlen = LEN(laItemSeg[lnCount,3])
      lnColorpos = laItemSeg[lnCount,4]
      EXIT
    ENDIF
  ENDFOR

  STORE 'PICHDR' TO lcFILE_type
  STORE 0  TO lnWeek, lnYear, lnStore, lnCallOff, lnSKU, lnPack_Qty, lnRcv_QTY, lnAdv_QTY, lnAdv_No
  STORE '' TO lcFact,lcStyMajor,lcClr_DESC, lcSz_DESC, lcDept
  *C200851.exe HIA, 2007/09/10 Processing Discrepancy files [END]


  STORE ''  TO lcTempMs3 , M.style , M.Order,lcTempMs4
  *B608250,1 SSH 02/09/2007 [Start]Initiate warehosue code variable.
  lcWareCode = ""
  *B608250,1 SSH 02/09/2007 [END]
  STORE .F. TO llOpenRep , llOpenFils
  STORE 0   TO lnFilHandl , lnLineNo , M.LineNo

  lcTempMs3 = gfTempName()
  lcTempMs4 = gfTempName()
  lcOrdline = gfTempName()
  lcTmpError= gfTempName()
  =lfOpenFls()
  oAriaApplication.otoolbar.cmdPrint.ENABLED = .F.
  oAriaApplication.otoolbar.cmdPrint.ControlEnable = 0

  PRIVATE llLoop , lcLine , lcConvLine , lcSku_No , lcLastpldN
  *-- Give User message in case the ms3 file variable is empty.
  *lcTxtFile = ThisFormSet.lcTxtFile
  IF EMPTY(lctxtfile)
    =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,;
    'You must select an MS3 file.')
    RETURN
  ENDIF
  SET MEMOWIDTH TO 600

  *-- Create a Table

  CREATE TABLE (oAriaApplication.WorkDir+lcTempMs3) (MEMO M(10))
  APPEN BLANK
  APPEND MEMO MEMO FROM (lctxtfile) OVERWRITE

  =lfConvFormat()
  *C200851.exe HIA, 2007/09/10 Processing Discrepancy files [BEGIN]  
  oForm.opross = CREATEOBJECT('ariaprogressbar')  
  oForm.Refresh()  

  oForm.oPross.lblFirstLabel.Caption = 'Import M&S Files .... '
  oForm.oPross.TotalProgress = MEMLINES(MEMO)
  oForm.oPross.AutoCenter = .T.
  oForm.oPross.Show()
  oForm.oPross.CurrentProgress(1)
  *oForm.oPross.Hide()
  *C200851.exe HIA, 2007/09/10 Processing Discrepancy files [END]
  
  PRIVATE lnLineNo
  lnLineNo = OCCUR("'",MEMO)
  llLoop = .F.
  
  
  *:C201105,1 MMT 02/16/2009 Add packs to M&S import call off program for low profile [Start]
  llByPack = .F.
  lcDep  = ''
  *:C201105,1 MMT 02/16/2009 Add packs to M&S import call off program for low profile [End]
  
  FOR lnI = 1 TO MEMLINES(MEMO) &&lnLineNo &&MEMLINES(MEMO)
    lcLine     = MLINE(MEMO,lnI)
    lcLine     = STRTRAN(lcLine,['])
    lcConvLine = PADL(lcLine,3)
    *C200851.exe HIA, 2007/09/10 Processing Discrepancy files [Begin]
    oForm.oPross.CurrentProgress(lnI)
    *C200851.exe HIA, 2007/09/10 Processing Discrepancy files [END]

    *C200851.exe HIA, 2007/09/10 Processing Discrepancy files [BEGIN]
    *-- Detect the file type
    IF lcConvLine = 'STX'
      lcFILE_type = RIGHT(lcLine,6)
    ENDIF
    IF lcFILE_type <> 'DLCHDR'
      *C200851.exe HIA, 2007/09/10 Processing Discrepancy files [END]


      *-- Check for the first 5 recored not needed while convert ms3 file.  'PIC',

      *B608250,1 SSH 02/09/2007 [Start] Get warehouse code from STX line position 59 with length 4.
      IF lcConvLine = "STX"
        *lcWareCode = '02'+SUBSTR(lcLine,59,4)
        lcWareCode = ALLTRIM(SUBSTR(lcLine,59,4))
        
        *:C201105,1 MMT 02/16/2009 Add packs to M&S import call off program for low profile [Start]
        lcDep  = ALLTRIM(SUBSTR(lcLine,54,3))
        *:C201105,1 MMT 02/16/2009 Add packs to M&S import call off program for low profile [End]
        
      ENDIF
      *B608250,1 SSH 02/09/2007 [End]
      IF INLIST(lcConvLine,'Y50','TYP','SDT','CDT','FIL','PFT','DIN')
        LOOP
      ENDIF
      IF lcConvLine = 'ORD'
        
        *:C201105,1 MMT 02/16/2009 Add packs to M&S import call off program for low profile [Start]
        IF SUBSTR(lcLine,5,5) =  '00001'
          llByPack = .T.
          llLoop = .F.
          LOOP
        ELSE
        *:C201105,1 MMT 02/16/2009 Add packs to M&S import call off program for low profile [End]
       
        IF SUBSTR(lcLine,5,4) =  '0000'
          *-- if ord set to 0000 then loop until the next ord.
          llLoop = .T.
          LOOP
        ELSE
        
		  
          llLoop = .F.
          
        ENDIF
      
      *:C201105,1 MMT 02/16/2009 Add packs to M&S import call off program for low profile [Start]
      ENDIF 
      *:C201105,1 MMT 02/16/2009 Add packs to M&S import call off program for low profile [End]
      
      ENDIF
      *-- Get The Sku#
      IF !llLoop .AND. lcConvLine = 'PLD'
        
        *:C201105,1 MMT 02/16/2009 Add packs to M&S import call off program for low profile [Start]
        IF !llByPack
        *:C201105,1 MMT 02/16/2009 Add packs to M&S import call off program for low profile [End]
        
          lcSku_No   = ALLTRIM(SUBSTR(lcLine,8,8))  &&ALLTRIM(SUBSTR(lcLine,10,8))
        *:B608881,1 AHS 05/29/2009 M&S import program at Low Profile creating invalid Call Off No [Start] 
         * lcLastpldN = SUBSTR(lcLine,22,6)   &&SUBSTR(lcLine,25,6)
           lcLastpldN = RIGHT(lcLine,6)
        *:B608881,1 AHS 05/29/2009 M&S import program at Low Profile creating invalid Call Off No [End]   
          lcCallOffNo=lcLastpldN
          WAIT WINDOW 'Please wait ... data collecting for Sku# : '+lcSku_No NOWAI
        
        *:C201105,1 MMT 02/16/2009 Add packs to M&S import call off program for low profile [Start]
        ELSE
          lnEndPos = ATC('+',lcLine,2)
          lcSku_No   = ALLTRIM(SUBSTR(lcLine,8,lnEndPos - 8))  &&ALLTRIM(SUBSTR(lcLine,10,8))
          lcLastpldN = RIGHT(lcLine,6)   &&SUBSTR(lcLine,25,6)
          lcCallOffNo=lcLastpldN
          WAIT WINDOW 'Please wait ... data collecting for Pack# : '+lcSku_No NOWAI
        ENDIF 
        *:C201105,1 MMT 02/16/2009 Add packs to M&S import call off program for low profile [End]
        
      ENDIF

      *Get the stores.
      IF !llLoop .AND. lcConvLine = 'PIC'
        lnadd=0
        IF SUBSTR(lcLine,10,1)=":"
          lnadd=1
        ENDIF
        lcStore = SUBSTR(lcLine,10+lnadd,4) &&SUBSTR(lcLine,15,4)
        lcDepot = SUBSTR(lcLine,17+lnadd,2) &&SUBSTR(lcLine,51,2)
        lnQty = EVAL(ALLTRIM(SUBSTR(lcLine,20))) &&Eval(ALLTRIM(SUBSTR(lcLine,62)))
        =lfUpPikQty(lnQty,lcSku_No,lcLastpldN,lcStore,lcDepot,lctxtfile)
      ENDIF

      *-- Get the Qty.
      IF !llLoop .AND. lcConvLine = 'PTR'
      ENDIF

      *C200851.exe HIA, 2007/09/10 Processing Discrepancy files [BEGIN]
    ELSE
      *-- Process the DCLHDR files
      *PRIVATE lcFact,lnWeek,lnYear, lnStore, lnCallOff, ldCancel, lnSKU, lnPack_Qty, lnRcv_QTY, lnAdv_QTY, lnAdv_No, lnDept,lcStyMajor,lcClr_DESC, lcSz_DESC
      IF lcConvLine = 'SDT'
        lcFact = RIGHT(lcLine,4)
      ENDIF
      IF lcConvLine = 'DNA'
        lnWeek = RIGHT(lcLine,2)
        IF !EMPTY(lnWeek )
          lnWeek  = VAL(lnWeek )
        ENDIF
      ENDIF
      IF lcConvLine = 'FIL'
        lnYear = RIGHT(lcLine,6)
        lnYear = '20' + LEFT(lnYear,2)
        IF !EMPTY(lnYear )
          lnYear = VAL(lnYear )
        ENDIF
        =lfCHKDSCLn()        
      ENDIF
      IF lcConvLine = 'CLO'
        lnStore = RIGHT(lcLine,4)
        IF !EMPTY(lnStore )
          lnStore = VAL(lnStore )
        ENDIF
      ENDIF
      IF lcConvLine = 'ODD'
        lnCallOff = SUBSTR(lcLine,AT('+',lcLine,1)+1,AT('+',lcLine,2)-AT('+',lcLine,1)-1)
        IF !EMPTY(lnCallOff )
          lnCallOff = VAL(lnCallOff )
        ENDIF
      ENDIF

      IF lcConvLine = 'ODD'
        IF AT('+',lcLine,3) > 0
          ldCancel = SUBSTR(lcLine,AT('+',lcLine,2)+1,AT('+',lcLine,3)-AT('+',lcLine,2)-1)
        ELSE
          ldCancel = SUBSTR(lcLine,AT('+',lcLine,2)+1,100)
        ENDIF

        IF !EMPTY(ldCancel)
          ldCancel = RIGHT(ldCancel ,6)
          ldCancel = EVAL('DATE(20'+SUBSTR(ldCancel,1,2)+','+SUBSTR(ldCancel,3,2)+','+SUBSTR(ldCancel,5,2)+')')
        ENDIF
      ENDIF

      IF lcConvLine = 'DCD'
        *lnSKU, lnPack_Qty, lnRcv_QTY, lnAdv_QTY, lnAdv_No, lnDept,lcStyMajor,lcClr_DESC, lcSz_DESC
        lnSKU      = SUBSTR(lcLine,AT('+',lcLine,1)+1,AT('+',lcLine,2)-AT('+',lcLine,1)-1)
        lnSKU      = VAL(SUBSTR(lnSKU,AT(':',lnSKU)+1,100))
        
        *lnPack_Qty = VAL(SUBSTR(lcLine,AT('+',lcLine,2)+1,AT('+',lcLine,3)-AT('+',lcLine,2)-1))
        *lnRcv_QTY  = VAL(SUBSTR(lcLine,AT('+',lcLine,3)+1,AT('+',lcLine,4)-AT('+',lcLine,3)-1))
        *lnAdv_QTY  = VAL(SUBSTR(lcLine,AT('+',lcLine,4)+1,AT('+',lcLine,5)-AT('+',lcLine,4)-1))
        *lnAdv_No   = VAL(SUBSTR(lcLine,AT('+',lcLine,5)+1,AT('+',lcLine,6)-AT('+',lcLine,5)-1))
        
        lnPack_Qty = VAL(SUBSTR(lcLine,AT('+',lcLine,4)+1,AT('+',lcLine,5)-AT('+',lcLine,4)-1))
        lnRcv_QTY  = VAL(SUBSTR(lcLine,AT('+',lcLine,5)+1,AT('+',lcLine,6)-AT('+',lcLine,5)-1))
        lnAdv_QTY  = VAL(SUBSTR(lcLine,AT('+',lcLine,9)+1,AT('+',lcLine,10)-AT('+',lcLine,9)-1))
        lnAdv_No   = VAL(SUBSTR(lcLine,AT('+',lcLine,10)+1,10))        
        
        lcSize = ''
        lcStyle = ''
        IF lfgetStyle(ALLTRIM(STR(lnSKU)))  AND SEEK(lcStyle,'Style')
          lcDept     = SUBSTR(gfCodDes(STYLE.CPURCODE, 'CPURCODE'),1,4)
          lcStyMajor = ALLTRIM(STYLE.CSTYMAJOR)

          lcClr_DESC =gfCodDes(SUBSTR(STYLE.STYLE,lnColorpos,lncolorlen),'COLOR')

          IF SEEK('S'+STYLE.SCALE,'SCALE')
            lcSz_DESC = EVAL('SCALE.SZ'+ALLTRIM(lcSize) )
          ENDIF
        ENDIF
        =lfAddDSCLn()
      ENDIF


    ENDIF
    *C200851.exe HIA, 2007/09/10 Processing Discrepancy files [END]


  ENDFOR

  *:C200863,2 MMT 09/22/2008 Change Hist. File Name[Start]
  lcCallOffNUm = ALCalOff.ccalloff
  lcDeptNo  = ALCalOff.cdept
  lcDate = DTOS(oAriaApplication.SystemDate)
  *:C200863,2 MMT 09/22/2008 Change Hist. File Name[End]
  

  IF llOpenRep
    llOpenRep = .F.
    SELECT ALCalOff
    =TABLEREVERT(.T.)
    *-- Must close the error file before append this erro into memo field.
    = FCLOSE(lnFilHandl)
    CREATE TABLE (oAriaApplication.WorkDir+lcTmpError) (MEMO M(10))
    APPEN BLANK
    APPEND MEMO MEMO FROM (oAriaApplication.WorkDir+'ErrLog.txt') OVERWRITE

    *B608250,1 SSH 02/09/2007 [Start] Add the warehosue code to the error log file.
    *oForm.AriaForm1.TxtError.Value="Imported From "+lctxtfile+CHR(13)+MEMO
    oForm.AriaForm1.TxtError.VALUE="Imported From "+lctxtfile+CHR(13)+"WareHouse Code : "+lcWareCode+CHR(13)+MEMO
    *B608250,1 SSH 02/09/2007 [END]

    *--Call Error screen.
    *DO (gcScrDir+gcWinAppl+"\ALDISPER.SPX")
    oAriaApplication.otoolbar.cmdPrint.ENABLED = .T.
    oAriaApplication.otoolbar.cmdPrint.ControlEnable = 1
  ELSE
    *C200851.exe HIA, 2007/09/10 Processing Discrepancy files [Begin]
    IF lcFILE_type = 'DLCHDR'
      SELECT ALDSCRP
      =TABLEUPDATE(.T.,.T.,"ALDSCRP")

    ELSE
      *C200851.exe HIA, 2007/09/10 Processing Discrepancy files [End]

      *!**-- Function to generate and save the order and pick ticket.
      DO lpSaveOrd
      SELECT ALCalOff
      
      =TABLEUPDATE(.T.,.T.,"ALCalOff")
      *C200851.exe HIA, 2007/09/10 Processing Discrepancy files [Begin]
    ENDIF
    *C200851.exe HIA, 2007/09/10 Processing Discrepancy files [End]
    *- Message Text   :- Import SBT files complete successfully.
    *- Message No.    :- 000000.
    *- Buttom Message :- Ok
    *- Buttom Number  :- 00000.
    lcMessage = 'Data was Imported successfully from the MS3 file to Aria4XP system.'
    oForm.AriaForm1.TxtError.VALUE=lcMessage
    = gfModalGen('INM00000B00000','F','ALERT',' ',lcMessage)
    = FCLOSE(lnFilHandl)
    =lfMSHistory(lctxtfile)
  ENDIF

  *C200851.exe HIA, 2007/09/10 Processing Discrepancy files [Begin]
  oForm.oPross.Hide()
  *C200851.exe HIA, 2007/09/10 Processing Discrepancy files [END]
  *-- End Of lfvMs3File.
  *:*************************************************************
  *: Name      : lfUpPikQty
  *: Developer : ABDOU ELGENDI -  (ABD)
  *: Date      : 08/28/2003
  *: Purpose   : Funtion to update the piktkt Qty.
  *:*************************************************************
  *: Calls     :
  *:             Procedures : ....
  *:             Functions  : ....
  *:*************************************************************
  *: Passed Parameters  : lnQty   :- Variable hold the Piked Qty.
  *:                    : lcSku_No:- Variable hold the Sku No.
  *:                    : lcLastpldN:- Hold 3 Charcter from the MS3 file.
  *:*************************************************************
  *: Returns            : ............
  *:*************************************************************
  *: Example   : =lfUpPikQty()
  *:*************************************************************
  *:
FUNCTION lfUpPikQty
  PARAMETERS lnSndPkQty, lcSndSkuNo, lclast_Pld,lcStore,lcDepot,lcImportFrom

  PRIVATE lnPrvAlias &&, lcStyle , lcSize
  STORE '' TO lcStyle , lcSize,lnChart
  lnPrvAlias = SELECT (0)
  IF TYPE('lnSndPkQty')= 'U' .OR. TYPE('lcSndSkuNo') = 'U' .OR. ;
    TYPE('lclast_Pld')= 'U'
    RETURN
  ENDIF

  lnLen = LEN(lclast_Pld)
  IF lnLen < 3
    FOR I = lnLen TO 3
      lnChart = lnChart + '0'
    ENDFOR
    lclast_Pld = lnChart + lclast_Pld
  ENDIF

  *-- First Check for the Sku #.
  IF !EMPTY(lcSndSkuNo)
    *-- Function to Check if the SKU # is exist or not if found return
    *-- the Style and the size.
    IF lfgetStyle(lcSndSkuNo)
      *-- Check if this Style is exist at the order line file.
      IF SEEK(lcStyle,'ORDLINE')

        SELECT ORDLINE
        *B608250,1 SSH 02/09/2007 [Start] Commented out to to add the warehouse code to the condition.
        *!*	      LOCATE REST WHILE Style+DTOS(complete)+cordtype+order+store+STR(lineno,6) = lcStyle ;
        *!*	                  FOR  EMPTY(Piktkt) .AND. Ordhdr.Status = 'O'
        LOCATE REST WHILE STYLE+DTOS(COMPLETE)+cordtype+ORDER+STORE+STR(LINENO,6) = lcStyle ;
        FOR  EMPTY(Piktkt) .AND. Ordhdr.STATUS = 'O' AND lcWareCode==SUBSTR(Ordhdr.cWareCode,3,4)
        *B608250,1 SSH 02/09/2007 [End]

        IF !FOUND()
          =SEEK(lcStyle,'ORDLINE')
          *B608250,1 SSH 02/09/2007 [Start] Commented out to to add the warehouse code to the condition.
          *!*	        LOCATE REST WHILE Style+DTOS(complete)+cordtype+order+store+STR(lineno,6) = lcStyle ;
          *!*	                    FOR  !EMPTY(Piktkt) .AND. Ordhdr.Status = 'O'

          LOCATE REST WHILE STYLE+DTOS(COMPLETE)+cordtype+ORDER+STORE+STR(LINENO,6) = lcStyle ;
          FOR  !EMPTY(Piktkt) .AND. Ordhdr.STATUS = 'O' AND lcWareCode==SUBSTR(Ordhdr.cWareCode,3,4)

          *B608250,1 SSH 02/09/2007 [End]
          && Check for Previous Call off for the same order line.
          IF FOUND()
             
             *:C201105,1 MMT 02/16/2009 Add packs to M&S import call off program for low profile [Start]
             IF !llByPack 
             *:C201105,1 MMT 02/16/2009 Add packs to M&S import call off program for low profile [End]
             
               = lfAddNewOrdLine(lnSndPkQty)
             
             *:C201105,1 MMT 02/16/2009 Add packs to M&S import call off program for low profile [Start]
             ELSE
               lcAlias = SELECT()
               FOR lnK = 1 TO 8
                 lcSize = STR(lnK,1)
                 lnQtySz = spck_lin.qty&lcSize. * lnSndPkQty
                 lnPkQty = lnSndPkQty
                 lnSndPkQty = lnQtySz 
                 = lfAddNewOrdLine(lnQtySz)
                 lnSndPkQty = lnPkQty 
               ENDFOR 
               SELECT(lcAlias)
             ENDIF 
             *:C201105,1 MMT 02/16/2009 Add packs to M&S import call off program for low profile [End]
             
          ELSE
            *-- Send Erro to Erro File in case no ordr open for cuuernt style.
            *lcError = 'There is no order open for style# / Sku# :'+ lcStyle +' / '+ lcSndSkuNo +'.'
            *= lfErrLog (lcError)
          ENDIF
        ENDIF

        SELECT ORDLINE
        =SEEK(lcStyle,'ORDLINE')
        *B608250,1 SSH 02/09/2007 [Start] Commented out to add the warehouse code to the condition.

        *!*LOCATE REST WHILE Style+DTOS(Complete)+cordtype+order+store+STR(lineno,6) = lcStyle ;
        *!* FOR (EMPTY(Piktkt) OR Piktkt="N") .AND. Ordhdr.Status = 'O'
        LOCATE REST WHILE STYLE+DTOS(COMPLETE)+cordtype+ORDER+STORE+STR(LINENO,6) = lcStyle ;
        FOR (EMPTY(Piktkt) OR Piktkt="N") .AND. Ordhdr.STATUS = 'O' AND lcWareCode==SUBSTR(Ordhdr.cWareCode,3,4)

        *B608250,1 SSH 02/09/2007 [End]
        IF FOUND()
          *-- Check if this record was picked before that.
           
           *:C201105,1 MMT 02/16/2009 Add packs to M&S import call off program for low profile [Start]
           IF !llByPack 
           *:C201105,1 MMT 02/16/2009 Add packs to M&S import call off program for low profile [EnD]
          =lfUpdtPick(lcStore,lclast_Pld)

          *:C201105,1 MMT 02/16/2009 Add packs to M&S import call off program for low profile [Start]
          ELSE
           lcAlias = SELECT()
           FOR lnK = 1 TO 8
             lcSize = STR(lnK,1)
             lnQtySz = spck_lin.qty&lcSize. * lnSndPkQty
             lnPkQty = lnSndPkQty
             lnSndPkQty = lnQtySz 
              =lfUpdtPick(lcStore,lclast_Pld)
             lnSndPkQty = lnPkQty 
           ENDFOR 
           SELECT(lcAlias)
          ENDIF 
          *:C201105,1 MMT 02/16/2009 Add packs to M&S import call off program for low profile [End]
          
        ELSE
          *-- Send Erro to Erro File in case no ordr open for cuuernt style.
          lcError = 'There is no order open for style# / Sku# :'+ lcStyle +' / '+ lcSndSkuNo +'.'
          =lfErrLog (lcError,lcImportFrom)
        ENDIF
        *ENDIF
      ELSE
        *-- Send Erro to Erro File in case not found the style into order line file.
        lcError = 'Style# / Sku# :'+ lcStyle +' / '+ lcSndSkuNo +' not found into order line file.'
        = lfErrLog (lcError,lcImportFrom)
      ENDIF
    ELSE
      *-- Send Erro to Erro File in case not found the Sku.
      *:C201105,1 MMT 02/16/2009 Add packs to M&S import call off program for low profile [Start]
      IF !llByPack 
      *:C201105,1 MMT 02/16/2009 Add packs to M&S import call off program for low profile [End]
      
        lcError = 'Sku # :' + lcSndSkuNo +' not found into the database.'

      *:C201105,1 MMT 02/16/2009 Add packs to M&S import call off program for low profile [Start]
      ELSE
        lcError = 'Pack No :'+lcSndSkuNo +' does not exist - please check with M&S department '+lcDep  
      ENDIF
      *:C201105,1 MMT 02/16/2009 Add packs to M&S import call off program for low profile [End]
      = lfErrLog (lcError,lcImportFrom)
    ENDIF
  ENDIF

  SELECT (lnPrvAlias)
  *-- End OF lfUpPikQty
  *:*************************************************************
  *: Name      : lfOpenFls
  *: Developer : ABDOU ELGENDI -  (ABD)
  *: Date      : 08/28/2003
  *: Purpose   : Function to open needed files.
  *:*************************************************************
  *: Calls     :
  *:             Procedures : ....
  *:             Functions  : ....
  *:*************************************************************
  *: Passed Parameters  : ............
  *:*************************************************************
  *: Returns            : ............
  *:*************************************************************
  *: Example   : =lfOpenFls ()
  *:*************************************************************
  *:
FUNCTION lfOpenFls
  PRIVATE lnPrvAlias

  lnPrvAlias = SELECT(0)

  *--- Array laOpenFile column 1 :- Hold the name of the file.
  *--- Array laOpenFile column 2 :- Hold the name of the index file.
  *--- Array laOpenFile column 3 :- Hold true in case open the file .
  *-- Aria4XP Tables.
  llOpenFils = .T.
  laOpenFile[1,1] = 'spck_lin'
  laOpenFile[1,2] = 'Spck_lin'

  laOpenFile[2,1] = 'STYLE'
  laOpenFile[2,2] = 'STYLE'

  laOpenFile[3,1] = 'STYDYE'
  laOpenFile[3,2] = 'STYDYE'

  laOpenFile[4,1] = 'ordline'
  laOpenFile[4,2] = 'Ordlines'

  laOpenFile[5,1] = 'ordhdr'
  laOpenFile[5,2] = 'ordhdr'

  laOpenFile[6,1] = 'spck_hdr'
  laOpenFile[6,2] = 'spck_hdr'

  laOpenFile[7,1] = 'PIKTKT'
  laOpenFile[7,2] = 'PIKTKT'

  laOpenFile[8,1] = 'ALCalOff'
  laOpenFile[8,2] = 'ALCalOff'

  *C200851.exe HIA, 2007/09/10 Processing Discrepancy files [BEGIN]
  laOpenFile[9,1] = 'ALDSCRP'
  laOpenFile[9,2] = 'ALDSCRP'

  laOpenFile[10,1] = 'SCALE'
  laOpenFile[10,2] = 'SCALE'
  *C200851.exe HIA, 2007/09/10 Processing Discrepancy files [END]

  FOR I = 1 TO ALEN(laOpenFile,1)
    IF !USED(laOpenFile[I,1])
      laOpenFile[I,3] = gfOpenFile(oAriaApplication.DataDir+laOpenFile[I,1],laOpenFile[I,2], "SH")
      *laOpenFile[I,3] = gfOpenTable(oAriaApplication.DataDir + laOpenFile[I,1], laOpenFile[I,2], 'SH')
    ENDIF
  ENDFOR

  SELECT ALCalOff
  =CURSORSETPROP("Buffering",5,'ALCalOff')

  *C200851.exe HIA, 2007/09/10 Processing Discrepancy files [BEGIN]
  SELECT ALDSCRP
  =CURSORSETPROP("Buffering",5,'ALDSCRP')
  *C200851.exe HIA, 2007/09/10 Processing Discrepancy files [END]

  SELECT ORDLINE
  *:C201105,1 MMT 02/18/2009 Add packs to M&S import call off program for low profile [Start]
  *SET ORDER TO Ordlines DESC
  SET ORDER TO Ordlines 
  *:C201105,1 MMT 02/18/2009 Add packs to M&S import call off program for low profile [End]
  SET RELATION TO cordtype+ORDER INTO Ordhdr

  lnFileStru = AFIELDS(laFileStru)

  DIMENSION laFileStru[lnFileStru+2,18]

  laFileStru[lnFileStru+1,1] = 'cLastpldNo'
  laFileStru[lnFileStru+1,2] = 'C'
  laFileStru[lnFileStru+1,3] = 6
  laFileStru[lnFileStru+1,4] = 0
  FOR lnI = 7 TO 16
    STORE SPACE(1) TO laFileStru[lnFileStru+1,lnI]
  ENDFOR

  laFileStru[lnFileStru+2,1] = 'CDEPOT'
  laFileStru[lnFileStru+2,2] = 'C'
  laFileStru[lnFileStru+2,3] = 2
  laFileStru[lnFileStru+2,4] = 0
  FOR lnI = 7 TO 16
    STORE SPACE(1) TO laFileStru[lnFileStru+2,lnI]
  ENDFOR
  DECLARE laIndex[2,2]
  laIndex[1,1] = 'Style+Order+STR(Lineno,6)+ Store'
  laIndex[1,2] = 'Ordline'
  laIndex[2,1] = 'Order + Store + cWareCode '
  laIndex[2,2] = 'Ordlins'
  =gfCrtTmp(lcOrdline,@laFileStru,@laIndex)
  SELECT(lnPrvAlias)

  *-- End of lfOpenFls.
  *:*************************************************************
  *: Name      : lfClosFls
  *: Developer : ABDOU ELGENDI -  (ABD)
  *: Date      : 08/28/2003
  *: Purpose   : Function to close opened files.
  *:*************************************************************
  *: Calls     :
  *:             Procedures : ....
  *:             Functions  : ....
  *:*************************************************************
  *: Called from : Prog.
  *:*************************************************************
  *: Example   : = lfClosFls ()
  *:*************************************************************
  *:
FUNCTION lfClosFls

  IF llOpenFils
    FOR I = 1 TO ALEN(laOpenFile,1)
      IF USED(laOpenFile[I,1]) .AND.  laOpenFile[I,3]
        = gfCloseFile(laOpenFile[I,1])
      ENDIF
    ENDFOR
  ENDIF

  IF USED(lcTempMs3)
    SELECT (lcTempMs3)
    USE
    ERASE (oAriaApplication.WorkDir+'lcTempMs3'+".DBF")
    ERASE (oAriaApplication.WorkDir+'lcTempMs3'+".FPT")
  ENDIF

  IF USED(lcTempMs4)
    SELECT (lcTempMs4)
    USE
    ERASE (oAriaApplication.WorkDir+'lcTempMs4'+".DBF")
    ERASE (oAriaApplication.WorkDir+'lcTempMs4'+".FPT")
  ENDIF



  IF USED(lcOrdline)
    SELECT (lcOrdline)
    USE
    ERASE (oAriaApplication.WorkDir+'lcOrdline'+".DBF")
    ERASE (oAriaApplication.WorkDir+'lcOrdline'+".FPT")
  ENDIF

  IF USED(lcTmpError)
    SELECT (lcTmpError)
    USE
    ERASE (oAriaApplication.WorkDir+'lcTmpError'+".DBF")
    ERASE (oAriaApplication.WorkDir+'lcTmpError'+".FPT")
  ENDIF

  *-- End of lfClosFls
  *:*************************************************************
  *: Name      : lfErrLog
  *: Developer : ABDOU ELGENDI -  (ABD)
  *: Date      : 08/28/2003
  *: Purpose   : Function to write the error into log file.
  *:*************************************************************
  *: Calls     :
  *:             Procedures : ....
  *:             Functions  : ....
  *:*************************************************************
  *: Called from : Prog.
  *:*************************************************************
  *: Example   : = lfErrLog ()
  *:*************************************************************
  *:
FUNCTION lfErrLog
  PARAMETER lcErr_Mesage,lcfromFile

  PRIVATE lnAlias
  lnAlias = SELECT(0)
  lnLineNo = lnLineNo + 1
  IF !llOpenRep
    llOpenRep = .T.
    lnFilHandl = FCREAT(oAriaApplication.WorkDir+'ErrLog.txt')
    =FPUTS(lnFilHandl,REPLICATE('*',65))
    =FPUTS(lnFilHandl,"*     Errors occurred while importing the MS3 file ["+lcfromFile+"]  to Aria4XP *")
    =FPUTS(lnFilHandl,REPLICATE('*',65))
    =FPUTS(lnFilHandl,' ')
    =FPUTS(lnFilHandl,' ')
  ENDIF

  =FPUTS(lnFilHandl, ALLTRIM(STR(lnLineNo)) + ' - '+ lcErr_Mesage)

  SELECT(lnAlias)
  RETURN

  *-- END OF lfErrLog
  *:*************************************************************
  *: Name      : lfvPrint
  *: Developer : ABDOU ELGENDI -  (ABD)
  *: Date      : 08/28/2003
  *: Purpose   : Function to print error file.
  *:*************************************************************
  *: Calls     :
  *:             Procedures : ....
  *:             Functions  : ....
  *:*************************************************************
  *: Called from : Prog.
  *:*************************************************************
  *: Example   : = lfvPrint()
  *:*************************************************************
  *:
FUNCTION lfvPrint

  *IF PSETUP(.T.)
  SET DEVICE TO PRINT
  SET CONSOLE OFF
  TYPE  oAriaApplication.WorkDir+'ErrLog.txt' TO PRINTER
  SET CONSOLE ON
  lcOGPlatForm='DOS'
  DO GFENDPRN
  SET DEVICE TO SCREEN
  *ENDIF
  RETURN

  *-- End OF lfvPrint
  *:*************************************************************
  *: Name      : lfUpdtPick
  *: Developer : ABDOU ELGENDI -  (ABD)
  *: Date      : 08/28/2003
  *: Purpose   : Function to print error file.
  *:*************************************************************
  *: Calls     :
  *:             Procedures : ....
  *:             Functions  : ....
  *:*************************************************************
  *: Called from : Prog.
  *:*************************************************************
  *: Example   : = lfUpdtPick()
  *:*************************************************************
  *:
FUNCTION lfUpdtPick
  LPARAMETERS lcStore,lclast_Pld
  PRIVATE lnAlias
  lnAlias = SELECT(0)
  ***lnSndPkQty
  SELECT ORDLINE
  *PRIVATE lnQty2Pik,llUpdate2
  llUpdate2=.F.
  lnQty2Pik = 0
  *-- Check if this line was picked before
  IF SEEK(ORDLINE.STYLE+ORDLINE.ORDER+STR(ORDLINE.LINENO,6),lcOrdline)
    IF &lcOrdline..PIK&lcSize < &lcOrdline..Qty&lcSize
      SELECT (lcOrdline)

      lnQty2Pik = MAX(MIN(lnSndPkQty,Qty&lcSize - PIK&lcSize),0)
      REPLACE PIK&lcSize WITH PIK&lcSize + MAX(MIN(lnSndPkQty,Qty&lcSize - PIK&lcSize),0),;
      TotPik     WITH Pik1+Pik2+Pik3+Pik4+Pik5+Pik6+Pik7+Pik8                    ,;
      cLastpldNo WITH lclast_Pld
      *---SSH: Function to update Call off transcatin file [Begin]
      =lfUpdCallOff(lnSndPkQty,lclast_Pld,lcStore,lcDepot)
      *---SSH: Function to update Call off transcatin file [End]
      
      
        lnSndPkQty = lnSndPkQty - lnQty2Pik &&PIK&lcSize
      

    ELSE
      *-- Call function to update the pick Qty.
      = lfUpdtPck2 ()
      llUpdate2=.T.
    ENDIF
  ELSE
    *-- Check if the Open Qty is Cover the picked Qty.
    IF Qty&lcSize >= lnSndPkQty && if the Open Qty Cover.
      SELECT ORDLINE
      SCATTER MEMVAR MEMO
      SELECT (lcOrdline)
      APPEND BLANK
      GATHER MEMVAR MEMO
      REPLACE PIK&lcSize WITH MAX(MIN(lnSndPkQty,Qty&lcSize),0)      ,;
      TotPik     WITH Pik1+Pik2+Pik3+Pik4+Pik5+Pik6+Pik7+Pik8,;
      cLastpldNo WITH lclast_Pld

      *---SSH: Function to update Call off transcatin file [Begin]
      =lfUpdCallOff(lnSndPkQty,cLastpldNo,lcStore,lcDepot)
      *---SSH: Function to update Call off transcatin file [End]
      
      
      
      lnSndPkQty = lnSndPkQty - PIK&lcSize
      

      
    ELSE && IF the Qty not Cover the Picked Qty.
      *-- Call function to update the pick Qty.
      = lfUpdtPck2 ()
      llUpdate2=.T.
    ENDIF
  ENDIF
  *-- Check if I pick all the Qty or not
  
  

  
  IF lnSndPkQty > 0

    SELECT (lcOrdline)

    =SEEK(ORDLINE.STYLE+ORDLINE.ORDER+STR(ORDLINE.LINENO,6),lcOrdline)
    REPLACE PIK&lcSize  WITH PIK&lcSize + lnSndPkQty,;
    TotPik      WITH Pik1+Pik2+Pik3+Pik4+Pik5+Pik6+Pik7+Pik8

    *---SSH: Function to update Call off transcatin file [Begin]
    IF lnQty2Pik+lnSndPkQty<>0
      lnQty2Pik = lnQty2Pik+lnSndPkQty
      =lfUpdCallOff(lnQty2Pik,cLastpldNo,lcStore,lcDepot)
    ENDIF
    *---SSH: Function to update Call off transcatin file [End]

    lnSndPkQty = 0
  ELSE
    IF llUpdate2 AND lnQty2Pik<>0
      llUpdate2=.F.
      SELECT (lcOrdline)
      =lfUpdCallOff(lnQty2Pik,cLastpldNo,lcStore,lcDepot)
    ENDIF
  ENDIF
  
  
  SELECT(lnAlias)

  *-- End OF lfUpdtPick
  *:*************************************************************
  *: Name      : lfUpdtPck2
  *: Developer : ABDOU ELGENDI -  (ABD)
  *: Date      : 08/28/2003
  *: Purpose   : Function to Update the Pick Qty .
  *:*************************************************************
  *: Calls     :
  *:             Procedures : ....
  *:             Functions  : ....
  *:*************************************************************
  *: Called from : Prog.
  *:*************************************************************
  *: Example   : = lfUpdtPck2()
  *:*************************************************************
  *:
FUNCTION lfUpdtPck2


  PRIVATE lnAlias
  lnAlias = SELECT(0)
  SELECT ORDLINE
  lnOldRec=RECNO()

  SCAN REST WHILE STYLE+DTOS(COMPLETE)+cordtype+ORDER+STORE+STR(LINENO,6) = ;
    lcStyle FOR EMPTY(Piktkt) AND lcWareCode==SUBSTR(Ordhdr.cWareCode,3,4).AND. Ordhdr.STATUS = 'O' AND lnSndPkQty>0

    IF SEEK(ORDLINE.STYLE+ORDLINE.ORDER+STR(ORDLINE.LINENO,6),lcOrdline)
      SELECT (lcOrdline)
      *- Check if this order still have avilable pick Qty.
      IF PIK&lcSize < Qty&lcSize
        *PRIVATE lnQty2Pik
        lnQty2Pik = lnQty2Pik+MAX(MIN(lnSndPkQty,Qty&lcSize - PIK&lcSize),0)
        ln2Pik = MAX(MIN(lnSndPkQty,Qty&lcSize - PIK&lcSize),0)
        *lnSndPkQty = MAX(MIN(lnSndPkQty,Qty&lcSize - PIK&lcSize),0)
        REPLACE PIK&lcSize WITH PIK&lcSize + ln2Pik  ,; &&MAX(MIN(lnSndPkQty,Qty&lcSize - PIK&lcSize),0)
        TotPik     WITH Pik1+Pik2+Pik3+Pik4+Pik5+Pik6+Pik7+Pik8                         ,;
        cLastpldNo WITH lclast_Pld
        lnSndPkQty = lnSndPkQty - ln2Pik
        SCATTER MEMVAR MEMO
      ELSE
        LOOP
      ENDIF
    ELSE
      SCATTER MEMVAR MEMO
      SELECT (lcOrdline)
      IF MAX(MIN(lnSndPkQty,Qty&lcSize),0)>0 OR Qty&lcSize=0
        APPEND BLANK
        GATHER MEMVAR MEMO
        lnQty2Pik=lnQty2Pik+IIF(Qty&lcSize<=0,lnSndPkQty,MAX(MIN(lnSndPkQty,Qty&lcSize),0))
        REPLACE PIK&lcSize WITH IIF(Qty&lcSize<=0,lnSndPkQty,MAX(MIN(lnSndPkQty,Qty&lcSize),0)),;
        TotPik     WITH Pik1+Pik2+Pik3+Pik4+Pik5+Pik6+Pik7+Pik8 ,;
        cLastpldNo WITH lclast_Pld
        lnSndPkQty = lnSndPkQty - PIK&lcSize
        SELECT ORDLINE
      ENDIF
    ENDIF
  ENDSCAN
  GOTO lnOldRec
  SELECT(lnAlias)

  *-- End of lfUpdtPck2
  *:*************************************************************
  *: Name      : lfgetStyle
  *: Developer : ABDOU ELGENDI -  (ABD)
  *: Date      : 08/28/2003
  *: Purpose   : Function to get the style from the spck_lin file.
  *:*************************************************************
  *: Calls     :
  *:             Procedures : ....
  *:             Functions  : ....
  *:*************************************************************
  *: Called from : Prog.
  *:*************************************************************
  *: Example   : = lfgetStyle()
  *:*************************************************************
  *:
FUNCTION lfgetStyle
  LPARAMETERS lcSndPckId

  PRIVATE lnAlias , llgetStyle
  llgetStyle = .F.
  IF TYPE('lcSndPckId') = 'U'
    RETURN
  ENDIF
  lnAlias = SELECT(0)
  
  SELECT Spck_lin
  
  *:C201105,1 MMT 02/16/2009 Add packs to M&S import call off program for low profile [Start]
  IF !llByPack 
  *:C201105,1 MMT 02/16/2009 Add packs to M&S import call off program for low profile [End]
  
    IF SEEK('S')
      SCAN REST WHILE TYPE+account+pack_id = 'S' FOR lcSndPckId $ ALLTRIM(pack_id)
        lcStyle = Spck_lin.STYLE
        FOR I = 1 TO 8
          *-- Hold the size scale.
          lcSize = STR(I,1)
          IF Spck_lin.Qty&lcSize = 1
            EXIT
          ENDIF
        ENDFOR
        llgetStyle = .T.
        EXIT
      ENDSCAN
    ENDIF
  
  *:C201105,1 MMT 02/16/2009 Add packs to M&S import call off program for low profile [Start]
  ELSE
    IF SEEK('P')
      LOCATE REST WHILE TYPE+account+pack_id = 'P' FOR pack_id = ALLTRIM(lcSndPckId)
      IF FOUND()
        lcStyle = Spck_lin.STYLE
        llgetStyle = .T.
      ENDIF 
    ENDIF 
  ENDIF 
  *:C201105,1 MMT 02/16/2009 Add packs to M&S import call off program for low profile [End]
  SELECT(lnAlias)
  RETURN llgetStyle

  *-- End Of lfgetStyle
  *:*************************************************************
  *: Name      : lpSaveOrd
  *: Developer : ABDOU ELGENDI -  (ABD)
  *: Date      : 08/28/2003
  *: Purpose   : Function to get the style from the spck_lin file.
  *:*************************************************************
  *: Calls     :
  *:             Procedures : ....
  *:             Functions  : ....
  *:*************************************************************
  *: Called from : Prog.
  *:*************************************************************
  *: Example   : DO lpSaveOrd
  *:*************************************************************
  *:
FUNCTION lpSaveOrd
  PRIVATE lnAlias , laOverPik ,  lUpdOrdQty , lnStart , lcStart,;
  lnDiffernt , lnTotQty
  DIMENSION laOverPik [8]
  STORE .F. TO  lUpdOrdQty
  STORE ''  TO lcStart
  STORE 0 TO lnStart , laOverPik , lnDiffernt, lnTotQty
  lnAlias = SELECT (0)
  *-- Function to Generate piktkt for the order lines.
 
  
  SELECT (lcOrdline)
  = lfGenPiktk ()

  SELECT ORDLINE
  SET RELATION TO
  SET ORDER TO TAG Ordlines  ASCENDING
  SELECT (lcOrdline)
  SCAN FOR TotQty<>0 OR TotPik<>0
    SCATTER MEMVAR MEMO
    *-- Index key into Order line file.
    *--style+DTOS(complete)+cordtype+order+store+STR(lineno,6)
    IF SEEK(M.style+ DTOS(M.Complete) + M.cordtype+ M.Order+M.store+STR(M.LineNo,6),'ORDLINE')
      IF ORDLINE.Piktkt="N"
        SELECT ORDLINE
        GATHER MEMVAR MEMO
      ENDIF
      SELECT ALCalOff
      REPLACE ALL  Piktkt WITH m.Piktkt FOR ORDER+STYLE+STR(LINENO,6) = M.Order+M.style+STR(M.LineNo,6) AND (EMPTY(Piktkt) OR Piktkt="N")
      SELECT ORDLINE
      =RLOCK()
      *-- Update the Picked fields.
      REPLACE Pik1    WITH M.Pik1   ,;
      Pik2    WITH M.Pik2   ,;
      Pik3    WITH M.Pik3   ,;
      Pik4    WITH M.Pik4   ,;
      Pik5    WITH M.Pik5   ,;
      Pik6    WITH M.Pik6   ,;
      Pik7    WITH M.Pik7   ,;
      Pik8    WITH M.Pik8   ,;
      TotPik  WITH M.TotPik ,;
      Piktkt  WITH M.Piktkt ,;
      PikDate WITH M.PikDate,;
      Picked  WITH .T.

      *:C200863,2 MMT 09/22/2008 Change Hist. File Name[Start]
      REPLACE ccalloff WITH ALCalOff.ccalloff
      *:C200863,2 MMT 09/22/2008 Change Hist. File Name[End]
		
      UNLOCK

      *-- Get the Over allocated Qty.
      laOverPik[1] = MAX(Pik1 - Qty1,0)
      laOverPik[2] = MAX(Pik2 - Qty2,0)
      laOverPik[3] = MAX(Pik3 - Qty3,0)
      laOverPik[4] = MAX(Pik4 - Qty4,0)
      laOverPik[5] = MAX(Pik5 - Qty5,0)
      laOverPik[6] = MAX(Pik6 - Qty6,0)
      laOverPik[7] = MAX(Pik7 - Qty7,0)
      laOverPik[8] = MAX(Pik8 - Qty8,0)

      *-- Check if we have any size over allocated.
      lUpdOrdQty = .F.
      FOR lnStart = 1 TO 8
        lcStart = STR(lnStart,1)
        IF PIK&lcStart > Qty&lcStart
          lUpdOrdQty = .T.
          EXIT
        ENDIF
      ENDFOR
      *-- Update the Open And Book Qty in case over allocate.
      IF lUpdOrdQty
        lnOvrAlQty = TotPik - TotQty
        lnTotQty   = Qty1 + Qty2 + Qty3 + Qty4 + Qty5 + Qty6 + Qty7 + Qty8
        =RLOCK()
        *-- Update the Open Qty fields.
        REPLACE Qty1    WITH MAX(M.Pik1,Qty1)   ,;
        Qty2    WITH MAX(M.Pik2,Qty2)   ,;
        Qty3    WITH MAX(M.Pik3,Qty3)   ,;
        Qty4    WITH MAX(M.Pik4,Qty4)   ,;
        Qty5    WITH MAX(M.Pik5,Qty5)   ,;
        Qty6    WITH MAX(M.Pik6,Qty6)   ,;
        Qty7    WITH MAX(M.Pik7,Qty7)   ,;
        Qty8    WITH MAX(M.Pik8,Qty8)   ,;
        TotQty  WITH MAX(M.TotPik,Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8),;
        Prepak  WITH '' ,;
        PpQty   WITH 0
        UNLOCK

        =RLOCK()
        *-- Update the Book Qty fields.
        REPLACE Book1    WITH MAX(M.Pik1,Book1) ,;
        Book2    WITH MAX(M.Pik2,Book2) ,;
        Book3    WITH MAX(M.Pik3,Book3) ,;
        Book4    WITH MAX(M.Pik4,Book4) ,;
        Book5    WITH MAX(M.Pik5,Book5) ,;
        Book6    WITH MAX(M.Pik6,Book6) ,;
        Book7    WITH MAX(M.Pik7,Book7) ,;
        Book8    WITH MAX(M.Pik8,Book8) ,;
        TotBook  WITH MAX(M.TotPik,Book1+Book2+Book3+Book4+Book5+Book6+Book7+Book8)
        UNLOCK

        *-- Update the order header file.
        lnDiffernt = MAX(TotQty - lnTotQty,0 )
        SELECT Ordhdr
        IF SEEK(M.cordtype+ M.Order)
          =RLOCK()
          REPLACE Book    WITH Book    + lnDiffernt            ,;
          OPEN    WITH OPEN    + lnDiffernt            ,;
          BookAmt WITH BookAmt + (lnDiffernt * M.Price),;
          OpenAmt WITH OpenAmt + (lnDiffernt * M.Price)
        ENDIF
        UNLOCK
      ENDIF
    ENDIF

    *-- Update the Style File.
    IF SEEK(M.style,'STYLE')
      SELECT STYLE
      =RLOCK()
      REPLACE Alo1       WITH Alo1   + M.Pik1      ,;
              Alo2       WITH Alo2   + M.Pik2      ,;
              Alo3       WITH Alo3   + M.Pik3      ,;
              Alo4       WITH Alo4   + M.Pik4      ,;
              Alo5       WITH Alo5   + M.Pik5      ,;
              Alo6       WITH Alo6   + M.Pik6      ,;
              Alo7       WITH Alo7   + M.Pik7      ,;
              Alo8       WITH Alo8   + M.Pik8      ,;
              TotAlo     WITH TotAlo + M.TotPik    ,;
              Ord1       WITH Ord1   + laOverPik[1],;
              Ord2       WITH Ord2   + laOverPik[2],;
              Ord3       WITH Ord3   + laOverPik[3],;
              Ord4       WITH Ord4   + laOverPik[4],;
              Ord5       WITH Ord5   + laOverPik[5],;
              Ord6       WITH Ord6   + laOverPik[6],;
              Ord7       WITH Ord7   + laOverPik[7],;
              Ord8       WITH Ord8   + laOverPik[8],;
              TotOrd     WITH Ord1 + Ord2 + Ord3 + Ord4 + Ord5 + Ord6 + Ord7 + Ord8
              *      cLastpldNo WITH &lcOrdline..cLastpldNo
              *SSHNEW
              SELECT ALCalOff
              LOCATE FOR CCALLOFF+cmsStore+CDEPOT+ORDER+STYLE+STR(LINENO,6) = &lcOrdline..cLastpldNo
              *lcfist3 = SUBSTR(STR(ALLTRIM(YEAR(dadd_date))),1,3)
              lcfist3 = SUBSTR(ALLTRIM(STR(YEAR(dadd_date))),2)
              lcLast3 = SUBSTR(ALLTRIM(&lcOrdline..cLastpldNo),1,3)
              SELECT Style
              REPLACE cLastPldNo WITH lcfist3+lcLast3
              *SSHNEW
      UNLOCK
    ENDIF

    *-- Update the Stydye file.
    IF SEEK(M.style+M.cWareCode,'STYDYE')
      SELECT STYDYE
      =RLOCK()
      REPLACE Alo1       WITH Alo1   + M.Pik1      ,;
      Alo2       WITH Alo2   + M.Pik2      ,;
      Alo3       WITH Alo3   + M.Pik3      ,;
      Alo4       WITH Alo4   + M.Pik4      ,;
      Alo5       WITH Alo5   + M.Pik5      ,;
      Alo6       WITH Alo6   + M.Pik6      ,;
      Alo7       WITH Alo7   + M.Pik7      ,;
      Alo8       WITH Alo8   + M.Pik8      ,;
      TotAlo     WITH TotAlo + M.TotPik    ,;
      Ord1       WITH Ord1   + laOverPik[1],;
      Ord2       WITH Ord2   + laOverPik[2],;
      Ord3       WITH Ord3   + laOverPik[3],;
      Ord4       WITH Ord4   + laOverPik[4],;
      Ord5       WITH Ord5   + laOverPik[5],;
      Ord6       WITH Ord6   + laOverPik[6],;
      Ord7       WITH Ord7   + laOverPik[7],;
      Ord8       WITH Ord8   + laOverPik[8],;
      TotOrd     WITH Ord1 + Ord2 + Ord3 + Ord4 + Ord5 + Ord6 + Ord7 + Ord8

      UNLOCK
    ENDIF

  ENDSCAN


  SELECT ORDLINE
  REPLACE Piktkt WITH "" FOR Piktkt="N"
  *:C201105,1 MMT 02/16/2009 Add packs to M&S import call off program for low profile [Start]
  *SET ORDER TO Ordlines DESC
  SET ORDER TO Ordlines 
  *:C201105,1 MMT 02/16/2009 Add packs to M&S import call off program for low profile [End]
  SET RELATION TO cordtype+ORDER INTO Ordhdr

  SELECT (lnAlias)
  *-- End OF lpSaveOrd
  *:*************************************************************
  *: Name      : lfGenPiktk
  *: Developer : ABDOU ELGENDI -  (ABD)
  *: Date      : 08/28/2003
  *: Purpose   : Function to get the style from the spck_lin file.
  *:*************************************************************
  *: Calls     :
  *:             Procedures : ....
  *:             Functions  : ....
  *:*************************************************************
  *: Called from : Prog.
  *:*************************************************************
  *: Example   :  = lfGenPiktk ()
  *:*************************************************************
  *:
FUNCTION lfGenPiktk

  oAlObj = CREATEOBJECT("AL")
  PRIVATE lnAlias , lcChgPkTKt , lcPiktkNo
  STORE '' TO lcPiktkNo , lcChgPkTKt
  lnAlias = SELECT (0)
  SELECT (lcOrdline)
  SET ORDER TO Ordlins
  LOCATE
  SCAN
    SCATTER MEMVAR MEMO
    llDummu = (m.Piktkt = "N") AND lfUpdOrdLin(m.Order)
    SCATTER MEMVAR MEMO
    IF lcChgPkTKt <> m.Order + m.store + m.cWareCode
      lcChgPkTKt = m.Order + m.store + m.cWareCode
      lcPikTktNo = oAlObj.lfGetPkTkt(m.Order , Ordhdr.cDivision , m.store , m.cWareCode,2,"N")
      *lcPikTktNo = lfGetPkTkt(m.Order, ORDHDR.cDivision, m.Store, m.cWareCode)
      SELECT Piktkt
      APPEN BLANK
      REPLACE Piktkt    WITH lcPikTktNo ,;
      account   WITH M.account  ,;
      STORE     WITH M.store    ,;
      ORDER     WITH M.Order    ,;
      DATE      WITH oAriaApplication.SystemDate,;
      cWareCode WITH M.cWareCode,;
      STATUS    WITH 'O'

      REPLACE cAdd_USer WITH oAriaApplication.User_ID,;
      cAdd_Time WITH gfGettime(),;
      dAdd_Date WITH oAriaApplication.SystemDate

    ENDIF
    SELECT (lcOrdline)
    REPLACE Piktkt  WITH lcPikTktNo ,;
    PikDate WITH oAriaApplication.SystemDate

  ENDSCAN
  SELECT(lnAlias)

  *:*************************************************************
  *: Name      : lfUpdCallOff
  *: Developer : Ahmed Salah -  (SSH)
  *: Date      : 08/28/2007
  *: Purpose   : Update CallOff File.
  *:*************************************************************
  *: Passed Parameters  : lnQty,lcCallOfNo,lcStore,lcDepot
  *:*************************************************************
  *: Returns            : ............
  *:*************************************************************
  *: Example   : =lfUpdCallOff()
  *:*************************************************************
  *:
FUNCTION lfUpdCallOff
  LPARAMETERS lnQty,lcCallOfNo,lcStore,lcDepot

  PRIVATE lnOldAls
  IF !EMPTY(lcCallOfNo)
    lnOldAls = SELECT(0)
    SELECT ALCalOff
    LOCATE FOR CCALLOFF+cmsStore+CDEPOT+ORDER+STYLE+STR(LINENO,6) = ;
    lcCallOfNo+lcStore+lcDepot+&lcOrdline..ORDER+ORDLINE.STYLE
    IF !FOUND()
      APPEND BLANK
      REPLACE cAdd_USer WITH oAriaApplication.User_ID,;
      cAdd_Time WITH gfGettime(),;
      dAdd_Date WITH oAriaApplication.SystemDate
    ENDIF
    REPLACE Qty&lcSize WITH lnQty,;
    TotQty     WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8,;
    CCALLOFF   WITH lcCallOfNo,;
    cmsStore   WITH lcStore,;
    CDEPOT     WITH lcDepot,;
    STYLE      WITH &lcOrdline..STYLE,;
    Piktkt     WITH &lcOrdline..Piktkt,;
    ORDER      WITH &lcOrdline..ORDER,;
    LINENO     WITH &lcOrdline..LINENO,;
    CDEPT      WITH lfGetStyGroup(ORDLINE.STYLE)

    REPLACE cWareCode WITH &lcOrdline..cWareCode
    
    *C200863,1 MMT 10/03/2007 Update Original Qty fields in ALcaloff Table[Start]
    REPLACE norigqty&lcSize WITH Qty&lcSize,;
    		norgtotqty      WITH norigqty1+norigqty2+norigqty3+norigqty4+norigqty5+norigqty6+norigqty7+norigqty8

    *C200863,1 MMT 10/03/2007 Update Original Qty fields in ALcaloff Table[End]

    *:C201105,1 MMT 02/16/2009 Add packs to M&S import call off program for low profile [Start]
    IF llByPack
      REPLACE Pack_id WITH lcSndSkuNo,;
              NPACKNO WITH lnPkQty
    ENDIF         
    *:C201105,1 MMT 02/16/2009 Add packs to M&S import call off program for low profile [End]


    SELECT(lnOldAls)
  ENDIF

  *:*************************************************************
  *: Name      : lfAddNewOrdLine
  *: Developer : Ahmed Salah -  (SSH)
  *: Date      : 08/28/2007
  *: Purpose   : Add new ordline.
  *:*************************************************************
  *: Passed Parameters  : lnSndPkQty,llNoOpen
  *:*************************************************************
  *: Returns            : ............
  *:*************************************************************
  *: Example   : =lfAddNewOrdLine()
  *:*************************************************************
  *:
FUNCTION lfAddNewOrdLine
  LPARAMETERS lnSndPkQty,llNoOpen

  PRIVATE lnOldAls,lnTempQty
  lnOldAls = SELECT(0)
  SELECT ORDLINE
  lnTempQty = Qty&lcSize.
  SCATTER MEMVAR MEMO
  IF SUBSTR(m.Piktkt,1,1) = "N"
  ELSE
    SELECT Ordhdr
    LOCATE FOR cordtype+ORDER="O"+ORDLINE.ORDER
    IF FOUND()
      lnOrdLine = LastLine
      REPLACE LastLine WITH LastLine+1
    ENDIF
    SELECT ORDLINE
    APPEND BLANK
    GATHER MEMVAR MEMO
    REPLACE cAdd_USer WITH oAriaApplication.User_ID,;
    cAdd_Time WITH gfGettime(),;
    dAdd_Date WITH oAriaApplication.SystemDate

    REPLACE LINENO WITH lnOrdLine+1,;
    TotPik WITH 0,;
    Pik1   WITH 0,;
    Pik2   WITH 0,;
    Pik3   WITH 0,;
    Pik4   WITH 0,;
    Pik5   WITH 0,;
    Pik6   WITH 0,;
    Pik7   WITH 0,;
    Pik8   WITH 0,;
    TotPik WITH 0,;
    Piktkt WITH "N"+ALLTRIM(STR(m.LineNo))

    REPLACE Qty1   WITH 0,;
    Qty2   WITH 0,;
    Qty3   WITH 0,;
    Qty4   WITH 0,;
    Qty5   WITH 0,;
    Qty6   WITH 0,;
    Qty7   WITH 0,;
    Qty8   WITH 0,;
    TotQty WITH 0

  ENDIF
  REPLACE Qty&lcSize. WITH Qty&lcSize.+lnSndPkQty,;
  TotQty      WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
  IF SEEK(m.style+m.Order+STR(m.LineNo,6),lcOrdline)
    SELECT(lcOrdline)
    REPLACE Qty&lcSize. WITH Qty&lcSize.+lnSndPkQty,;
    TotQty      WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
  ENDIF
  SELECT (lnOldAls)

  *:*************************************************************
  *: Name      : lfSRInit
  *: Developer : Ahmed Salah -  (SSH)
  *: Date      : 08/28/2007
  *: Purpose   : Init function.
  *:*************************************************************
  *: Passed Parameters  : lcOrder
  *:*************************************************************
  *: Returns            : ............
  *:*************************************************************
  *: Example   : =lfSRInit()
  *:*************************************************************
  *:
FUNCTION lfSRInit
  LPARAMETERS loFormSet

  DECLARE loFormSet.laPanelObj[1,6]
  STORE '' TO loFormSet.laPanelObj
  loFormSet.laPanelObj[1,1] = 'pbScop'
  loFormSet.laPanelObj[1,2] = oAriaApplication.BitMapHome+"GENERAT.BMP"
  loFormSet.laPanelObj[1,3] = 'mProceed'
  loFormSet.laPanelObj[1,4] = 'Proceed'
  loFormSet.laPanelObj[1,5] = 'Proceed'
  loFormSet.laPanelObj[1,6] = 'S'


  *:*************************************************************
  *: Name      : lfUpdOrdLin
  *: Developer : Ahmed Salah -  (SSH)
  *: Date      : 08/28/2007
  *: Purpose   : Update OrdLine.
  *:*************************************************************
  *: Passed Parameters  : lcOrder
  *:*************************************************************
  *: Returns            : ............
  *:*************************************************************
  *: Example   : =lfUpdOrdLin()
  *:*************************************************************
  *:
FUNCTION lfUpdOrdLin
  LPARAMETERS lcOrder

  PRIVATE lnOldAls,lnOrdLineNo
  lnOldAls = SELECT(0)
  SCATTER MEMVAR MEMO
  SELECT (lcOrdline)
  REPLACE Qty1    WITH Pik1,;
  Qty2    WITH Pik2,;
  Qty3    WITH Pik3,;
  Qty4    WITH Pik4,;
  Qty5    WITH Pik5,;
  Qty6    WITH Pik6,;
  Qty7    WITH Pik7,;
  Qty8    WITH Pik8,;
  TotQty  WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8,;
  Book1   WITH Qty1,;
  Book2   WITH Qty2,;
  Book3   WITH Qty3,;
  Book4   WITH Qty4,;
  Book5   WITH Qty5,;
  Book6   WITH Qty6,;
  Book7   WITH Qty7,;
  Book8   WITH Qty8,;
  TotBook WITH TotQty

  SCATTER MEMVAR MEMO
  lnOrdLineNo = VAL(ALLTRIM(SUBSTR(m.Piktkt,2)))
  SELECT ORDLINE
  LOCATE FOR cordtype+ORDER+STR(LINENO,6) = "O"+lcOrder+STR(lnOrdLineNo,6) AND !EMPTY(Piktkt) AND Piktkt<>"N"
  IF FOUND()
    REPLACE Qty1    WITH IIF(Qty1-m.Qty1>0,Qty1-m.Qty1,Qty1),;
    Qty2    WITH IIF(Qty2-m.Qty2>0,Qty2-m.Qty2,Qty2),;
    Qty3    WITH IIF(Qty3-m.Qty3>0,Qty3-m.Qty3,Qty3),;
    Qty4    WITH IIF(Qty4-m.Qty4>0,Qty4-m.Qty4,Qty4),;
    Qty5    WITH IIF(Qty5-m.Qty5>0,Qty5-m.Qty5,Qty5),;
    Qty6    WITH IIF(Qty6-m.Qty6>0,Qty6-m.Qty6,Qty6),;
    Qty7    WITH IIF(Qty7-m.Qty7>0,Qty7-m.Qty7,Qty7),;
    Qty8    WITH IIF(Qty8-m.Qty8>0,Qty8-m.Qty8,Qty8),;
    TotQty  WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8,;
    Book1   WITH Qty1,;
    Book2   WITH Qty2,;
    Book3   WITH Qty3,;
    Book4   WITH Qty4,;
    Book5   WITH Qty5,;
    Book6   WITH Qty6,;
    Book7   WITH Qty7,;
    Book8   WITH Qty8,;
    TotBook WITH TotQty

  ELSE
    REPLACE Qty1    WITH m.Qty1,;
    Qty2    WITH m.Qty2,;
    Qty3    WITH m.Qty3,;
    Qty4    WITH m.Qty4,;
    Qty5    WITH m.Qty5,;
    Qty6    WITH m.Qty6,;
    Qty7    WITH m.Qty7,;
    Qty8    WITH m.Qty8,;
    TotQty  WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8,;
    Book1   WITH Qty1,;
    Book2   WITH Qty2,;
    Book3   WITH Qty3,;
    Book4   WITH Qty4,;
    Book5   WITH Qty5,;
    Book6   WITH Qty6,;
    Book7   WITH Qty7,;
    Book8   WITH Qty8,;
    TotBook WITH TotQty

  ENDIF
  SELECT(lnOldAls)


  *:*************************************************************
  *: Name      : lfConvFormat
  *: Developer : Ahmed Salah -  (SSH)
  *: Date      : 08/28/2007
  *: Purpose   : Convert MS3 file to the old format.
  *:*************************************************************
  *: Passed Parameters  : ............
  *:*************************************************************
  *: Returns            : ............
  *:*************************************************************
  *: Example   : =lfConvFormat()
  *:*************************************************************
  *:
FUNCTION lfConvFormat

  CREATE TABLE (oAriaApplication.WorkDir+lcTempMs4) (MEMO M(10))
  APPEN BLANK
  SELECT(lcTempMs3)
  REPLACE MEMO WITH STRTRAN(MEMO,CHR(13))
  lnLineNo = OCCUR("'",MEMO)
  FOR lnI = 1 TO lnLineNo
    SELECT(lcTempMs3)
    lnEndPos = AT("'",MEMO)
    lcLine1 = SUBSTR(MEMO,1,lnEndPos-1)
    lcLine2 = SUBSTR(MEMO,1,lnEndPos)
    SELECT(lcTempMs3)
    REPLACE MEMO WITH ALLTRIM(STRTRAN(MEMO,lcLine2,"",1,1))
    SELECT(lcTempMs4)
    REPLACE MEMO WITH MEMO+STRTRAN(ALLTRIM(lcLine2),CHR(13))+CHR(13)
  ENDFOR
  SELECT (lcTempMs3)
  REPLACE MEMO WITH &lcTempMs4..MEMO



  *:*************************************************************
  *: Name      : lfMSHistory
  *: Developer : Ahmed Salah -  (SSH)
  *: Date      : 08/28/2007
  *: Purpose   : Move imported Call of file to teh history path.
  *:*************************************************************
  *: Passed Parameters  : ............
  *:*************************************************************
  *: Returns            : ............
  *:*************************************************************
  *: Example   : =lfMSHistory()
  *:*************************************************************
  *:
FUNCTION lfMSHistory
  LPARAMETERS lcOldFile

  lcNewFile = lfGetHistPath()
  lcFileName = lfGetFileName(lcOldFile)
  *C200851.exe HIA, 2007/09/10 Processing Discrepancy files [Begin]
  *COPY FILE (lcOldFile) TO (lcNewFile+lcFileName+".OLD")
  IF lcFILE_type = 'DLCHDR'  
    COPY FILE (lcOldFile) TO (lcNewFile+lcFileName+".DLD")
  ELSE
    *:C200863,2 MMT 09/22/2008 Change Hist. File Name[Start]
    lcFileName = ALLTRIM(lcCallOffNUm)+ALLTRIM(lcDeptNo)+ALLTRIM(lcDate)
    *:C200863,2 MMT 09/22/2008 Change Hist. File Name[End]
    
    COPY FILE (lcOldFile) TO (lcNewFile+lcFileName+".OLD")
  ENDIF   
  *C200851.exe HIA, 2007/09/10 Processing Discrepancy files [END]  
  ERASE (lcOldFile)

  *:*************************************************************
  *: Name      : lfGetHistPath
  *: Developer : Ahmed Salah -  (SSH)
  *: Date      : 08/28/2007
  *: Purpose   : Get history file path.
  *:*************************************************************
  *: Passed Parameters  : ............
  *:*************************************************************
  *: Returns            : ............
  *:*************************************************************
  *: Example   : =lfGetHistPath()
  *:*************************************************************
  *:
FUNCTION lfGetHistPath

  lc2Return = gfGetMemVar('M_HISTMS' , oAriaApplication.ActiveCompanyId)
  IF SUBSTR(lc2Return,LEN(ALLTRIM(lc2Return)))="\"
  ELSE
    lc2Return = lc2Return+"\"
  ENDIF
  RETURN(lc2Return)

FUNCTION lfGetFileName
  LPARAMETERS cFileName
  cFileName = SUBSTR(cFileName,AT("\",cFileName,OCCURS("\",cFileName))+1)
  RETURN(SUBSTR(cFileName,1,LEN(ALLTRIM(cFileName))-4))


  *:*************************************************************
  *: Name      : lfGetStyGroup
  *: Developer : Ahmed Salah -  (SSH)
  *: Date      : 08/28/2007
  *: Purpose   : Get Style Group.
  *:*************************************************************
  *: Passed Parameters  : cStyle
  *:*************************************************************
  *: Returns            : ............
  *:*************************************************************
  *: Example   : =lfGetStyGroup ()
  *:*************************************************************
  *:
FUNCTION lfGetStyGroup
  LPARAMETERS cStyle

  PRIVATE lnOldAls
  lnOldAls = SELECT(0)
  SELECT STYLE
  =gfSeek(cStyle)
  SELECT(lnOldAls)
  RETURN(SUBSTR(gfCodDes(STYLE.CPURCODE, 'CPURCODE'),1,3))
  *:*************************************************************
  *: Name      : lfCHKDSCLn
  *: Developer : Hassan Ibrahim Ali -  (HIA)
  *: Date      : 09/10/2007
  *: Purpose   : Funtion to check existance of the transaction;
  *:             Before at the custom discrepancy file.
  *:*************************************************************
  *: Calls     :
  *:             Procedures : ....
  *:             Functions  : ....
  *:*************************************************************
  *: Passed Parameters  :
  *:*************************************************************
  *: Returns            : ............
  *:*************************************************************
  *: Example   : =lfCHKDSCLn()
  *:*************************************************************
FUNCTION lfCHKDSCLn
  SELECT ALDSCRP
  IF SEEK(lcFACT+STR(lnWEEK,2)+STR(lnYEAR,4)) &&ALDSCRP   && CFACT+STR(NWEEK,2)+STR(NYEAR,2)+STR(NSTORE,4)+STR(NCALLOFF,6) 
   DELETE FOR CFACT+STR(NWEEK,2)+STR(NYEAR,4)+STR(NSTORE,4)+STR(NCALLOFF,6) = lcFACT+STR(lnWEEK,2)+STR(lnYEAR,4)
  ENDIF  
  SELECT (lcTempMs3)
ENDFUNC
*:*************************************************************
  *:*************************************************************
  *: Name      : lfAddDSCLn
  *: Developer : Hassan Ibrahim Ali -  (HIA)
  *: Date      : 09/10/2007
  *: Purpose   : Funtion to update the custom discrepancy file.
  *:*************************************************************
  *: Calls     :
  *:             Procedures : ....
  *:             Functions  : ....
  *:*************************************************************
  *: Passed Parameters  :
  *:*************************************************************
  *: Returns            : ............
  *:*************************************************************
  *: Example   : =lfAddDSCLn()
  *:*************************************************************
FUNCTION lfAddDSCLn
  SELECT ALDSCRP

  INSERT INTO ALDSCRP ;
      (cFact,nWeek,nYear, nStore, nCallOff, dCancel, nSKU, nPack_Qty, nRcv_QTY, nAdv_QTY, nAdv_No, cDept,CSTYMAJOR,cClr_DESC, cSz_DESC);
      VALUES (lcFact,lnWeek,lnYear, lnStore, lnCallOff, ldCancel, lnSKU, lnPack_Qty, lnRcv_QTY, lnAdv_QTY, lnAdv_No, lcDept,lcStyMajor,lcClr_DESC, lcSz_DESC)
  REPLACE cAdd_USer WITH oAriaApplication.User_ID,;
            cAdd_Time WITH gfGettime(),;
            dAdd_Date WITH oAriaApplication.SystemDate
  SELECT (lcTempMs3)
ENDFUNC
*:*************************************************************