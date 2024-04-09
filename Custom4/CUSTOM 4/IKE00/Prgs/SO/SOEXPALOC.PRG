*!*****************************************************************************************
*! Name      : SOEXPALOC.PRG
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 12/20/2015 
*! Purpose   : Program to Export order info. to XLS
*! Entry no. : C201755 - [T20151014.0017]
*:************************************************************************
*: Modifications:
*: B611114,1 MMT 02/10/2016 Issue#3 - Custom Export Allocation program exports only 65000 records[T20151014.0017]
*:************************************************************************

*!*************************************************************
*! Name      : lfInit
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 12/20/2015 
*! Purpose   : Init Function for the screen
*!*************************************************************
FUNCTION lfInit
PARAMETERS loFormSet
  loFormSet.lcTempFileHdr = gfTempName()
  loFormSet.lcDetailFile = gfTempName()
  loFormSet.lcexporttemp = gfTempName()
  SET MULTILOCKS ON
  gfOpenTable('Ordhdr','Ordhdr')
  gfOpenTable('ordline','ORDLINST')
  gfOpenTable('STYLE','STYLE')
  gfOpenTable('SCALE','SCALE')
   
   
  DIMENSION laFileStruD[13,4] 
  
  laFileStruD[1,1]  = 'Order'
  laFileStruD[1,2]  = 'C'
  laFileStruD[1,3]  = 6
  laFileStruD[1,4]  =  0
  
  laFileStruD[2,1]  = 'Store'
  laFileStruD[2,2]  = 'C'
  laFileStruD[2,3]  = 8
  laFileStruD[2,4]  =  0
  
  laFileStruD[3,1]  = 'Style'
  laFileStruD[3,2]  = 'C'
  laFileStruD[3,3]  = 19
  laFileStruD[3,4]  =  0
  
  laFileStruD[4,1]  = 'TotQty'
  laFileStruD[4,2]  = 'N'
  laFileStruD[4,3]  = 10
  laFileStruD[4,4]  =  0
  
  laFileStruD[5,1]  = 'DC'
  laFileStruD[5,2]  = 'C'
  laFileStruD[5,3]  = 8
  laFileStruD[5,4]  =  0
  
  laFileStruD[6,1]  = 'QTY1'
  laFileStruD[6,2]  = 'N'
  laFileStruD[6,3]  = 9
  laFileStruD[6,4]  =  0
  
  laFileStruD[7,1]  = 'QTY2'
  laFileStruD[7,2]  = 'N'
  laFileStruD[7,3]  = 9
  laFileStruD[7,4]  =  0


  laFileStruD[8,1]  = 'QTY3'
  laFileStruD[8,2]  = 'N'
  laFileStruD[8,3]  = 9
  laFileStruD[8,4]  =  0
  
  laFileStruD[9,1]  = 'QTY4'
  laFileStruD[9,2]  = 'N'
  laFileStruD[9,3]  = 9
  laFileStruD[9,4]  =  0
  
  laFileStruD[10,1]  = 'QTY5'
  laFileStruD[10,2]  = 'N'
  laFileStruD[10,3]  = 9
  laFileStruD[10,4]  =  0
  
  laFileStruD[11,1]  = 'QTY6'
  laFileStruD[11,2]  = 'N'
  laFileStruD[11,3]  = 9
  laFileStruD[11,4]  =  0
  
  laFileStruD[12,1]  = 'QTY7'
  laFileStruD[12,2]  = 'N'
  laFileStruD[12,3]  = 9
  laFileStruD[12,4]  =  0
  
  laFileStruD[13,1]  = 'QTY8'
  laFileStruD[13,2]  = 'N'
  laFileStruD[13,3]  = 9
  laFileStruD[13,4]  =  0

  =gfCrtTmp(loFormSet.lcDetailFile ,@laFileStruD,"ORDER+STORE+STYLE",  loFormSet.lcDetailFile ,.T.)
   
  
  DIMENSION laFileStru[6,4]

  laFileStru[1,1] = 'LLSEL'
  laFileStru[1,2] = 'L'
  laFileStru[1,3] = 1
  laFileStru[1,4] = 0

  laFileStru[2,1] = 'Order'
  laFileStru[2,2] = 'C'
  laFileStru[2,3] = 6
  laFileStru[2,4] = 0

  laFileStru[3,1] = 'Account'
  laFileStru[3,2] = 'C'
  laFileStru[3,3] = 5
  laFileStru[3,4] = 0
  
  laFileStru[4,1] = 'CWARECODE'
  laFileStru[4,2] = 'C'
  laFileStru[4,3] = 6
  laFileStru[4,4] = 0


  laFileStru[5,1] = 'Open'
  laFileStru[5,2] = 'N'
  laFileStru[5,3] = 8
  laFileStru[5,4] = 0

  laFileStru[6,1] = 'OpenAmt'
  laFileStru[6,2] = 'N'
  laFileStru[6,3] = 13
  laFileStru[6,4] = 2
  
  =gfCrtTmp(loFormSet.lcTempFileHdr ,@laFileStru,"ORDER",  loFormSet.lcTempFileHdr ,.T.)
  lfAddControlSource(loFormSet)

  WITH loFormSet
    .nWorkArea        = 'ORDHDR'
    .DataEnvironment.InitialSelectedAlias = 'ORDHDR'
    .cbrowsetabledbengine   = 'NATIVE'
  ENDWITH

  loFormSet.llCallScop = .F.   &&Flag to hold the first time of the session
  loFormSet.llFrstTime = .T.       && Flag to know if we are going to call lpShow for the first time
ENDFUNC


*!*************************************************************
*! Name      : lfvScope
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 12/20/2015 
*! Purpose   : Scope function to call selection criteria OG
*!*************************************************************
FUNCTION  lfvScope
PARAMETERS loFormSet
 lndataSessPre = SET("Datasession" )
 IF loFormSet.llCalledFromOp
    loFormSet.llCallOption = loFormSet.llCalledFromOp
 ENDIF
 loFormSet.llCallScop = .F.             && Screen Already Initialized
 lcDataSessI = SET("Datasession" )&&THIS.loFormSet.DatasessionID
 DIMENSION laOgSelect[1]
 laOgSelect =''
 lcRpGroup = 'S'
 lcRpFPath = ""
 lcExpr = gfOpGrid('SOEXPALL' , .T.)&&,.F.,.F.,.T.,.T.)
 loFormSet.lcexportPath =  lcRpFPath 
 loFormSet.lcRpGroup = lcRpGroup
 SET DATASESSION TO lcDataSessI
 IF lcExpr <> ".F."  
   
   SELECT (loFormSet.lcTempFileHdr) 
   SET RELATION OFF INTO (loFormSet.lcDetailFile) 
   ZAP IN (loFormSet.lcTempFileHdr) 
   *!*	ORDER     
   llSeleOrder= .F. && flag to indicate if there is any piktkt selected
   lnPosOrder= ASCAN(laOgSelect,"ORDHDR.ORDER")
   IF lnPosOrder> 0
	 lnPosOrder= ASUBSCRIPT(laOgSelect,lnPosOrder,1)
	 lcFileOrder=IIF(!EMPTY(laOgSelect[lnPosOrder,6]),laOgSelect[lnPosOrder,6],'')
	 IF !EMPTY(lcFileOrder) AND USED(lcFileOrder) AND RECCOUNT(lcFileOrder)> 0
	   SELECT(lcFileOrder)
	   LOCATE For !DELETED()
	   IF !EOF()
	     llSeleOrder= .T.
	   ENDIF  
	 ENDIF
   ENDIF
   
   *!*	ACCOUNT   
   llSeleAcc= .F. && flag to indicate if there is any piktkt selected
   lnPosAcc= ASCAN(laOgSelect,"ORDHDR.ACCOUNT")
   IF lnPosAcc> 0
     lnPosAcc= ASUBSCRIPT(laOgSelect,lnPosAcc,1)
     lcFileAccount=IIF(!EMPTY(laOgSelect[lnPosAcc,6]),laOgSelect[lnPosAcc,6],'')
     IF !EMPTY(lcFileAccount) AND USED(lcFileAccount) AND RECCOUNT(lcFileAccount)> 0
       SELECT(lcFileAccount)
       LOCATE For !DELETED()
       IF !EOF()
         llSeleAcc= .T.
       ENDIF  
     ENDIF
   ENDIF
       
   *!* Entered    
  ldEntStart= {}
  ldEntEnd = {}
  llSeleEntDate= .F. && flag to indicate if there is any piktkt selected
  lnDatePos= ASCAN(laOgSelect,"ORDHDR.ENTERED")
  IF lnDatePos> 0
    lnDatePos= ASUBSCRIPT(laOgSelect,lnDatePos,1)
    llSeleEntDate = !EMPTY(laOgSelect[lnDatePos,6])
    ldEntStart = CTOD(SUBSTR(laOgSelect[lnDatePos,6],1,10))
    ldEntEnd = CTOD(SUBSTR(laOgSelect[lnDatePos,6],12,21))
  ENDIF
 

   *!* Complete 
  ldCompStart= {}
  ldCompEnd = {}
  llSeleCompDate= .F. && flag to indicate if there is any piktkt selected
  lnDatePos= ASCAN(laOgSelect,"ORDHDR.COMPLETE")
  IF lnDatePos> 0
    lnDatePos= ASUBSCRIPT(laOgSelect,lnDatePos,1)
    llSeleCompDate = !EMPTY(laOgSelect[lnDatePos,6])
    ldCompStart = CTOD(SUBSTR(laOgSelect[lnDatePos,6],1,10))
    ldCompEnd  = CTOD(SUBSTR(laOgSelect[lnDatePos,6],12,21))
  ENDIF
  
  IF !USED('ORDHDR')
    =gfOpenTable('ORDHDR','ORDHDR')
  ENDIF    
  IF !USED('ORDLINE')
    =gfOpenTable('ORDLINE','ORDLINE')
  ENDIF    
       
  IF !USED('Customer')
    =gfOpenTable('Customer','Customer')
  ENDIF    

       
   DO CASE 
     CASE llSeleOrder && Order is Selected
       SELECT ORDHDR     
       =gfSetOrder("ORDHDR")    
       SELECT(lcFileOrder)
       LOCATE 
       SCAN FOR !DELETED()
         lcOrder = Order
         SELECT ORDHDR
         IF gfSeek("O"+lcOrder) AND IIF(llSeleAcc ,SEEK(Account,lcFileAccount),.T.) AND ;
           IIF(llSeleCompDate,BETWEEN(ORDHDR.COMPLETE,ldCompStart,ldCompEnd ),.T.) AND ;
           IIF(llSeleEntDate,BETWEEN(Ordhdr.Entered,ldEntStart,ldEntEnd),.T.) AND !(Ordhdr.Status  $ 'CX') AND ORDHDR.MULTI = 'Y'
           SCATTER MEMO MEMVAR 
           m.LLSEL = .T.
           INSERT INTO  (loFormSet.lcTempFileHdr) FROM MEMVAR     
           SELECT Ordline 
           =gfSeek('O'+lcOrder)
           SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = "O"+lcOrder FOR TOTQTY > 0
             SCATTER MEMO MEMVAR 
             m.DC = ""
             IF !EMPTY(m.Store) AND gfSeek('S'+m.Account+m.Store,'Customer','Customer') AND !EMPTY(Customer.Dist_CTR)
               m.DC = Customer.Dist_CTR 
             ENDIF  
             IF !SEEK(m.Order+m.Store+m.Style,loFormSet.lcDetailFile)
               INSERT INTO (loFormSet.lcDetailFile) FROM MEMVAR     
             ELSE
               REPLACE TotQty WITH TotQty + Ordline.TOTQTY
             ENDIF  
           ENDSCAN
         ENDIF  
         SELECT(lcFileOrder)  
       ENDSCAN     
     CASE llSeleAcc && Account is Selected
        SELECT ORDHDR
        =gfSetOrder("ORDACCT") && ACCOUNT+CORDTYPE+ORDER
        SELECT(lcFileAccount)
        LOCATE
        SCAN FOR !DELETED()
          lcAccnt = Account
          SELECT ORDHDR
          =gfSeek(lcAccnt+"O")
          SCAN REST WHILE  ACCOUNT+CORDTYPE+ORDER = lcAccnt+"O" FOR IIF(llSeleCompDate,BETWEEN(ORDHDR.COMPLETE,ldCompStart,ldCompEnd),.T.) AND ;
             IIF(llSeleEntDate,BETWEEN(Ordhdr.Entered,ldEntStart,ldEntEnd),.T.) AND !(Ordhdr.Status  $ 'CX')AND ORDHDR.MULTI = 'Y'
             SCATTER MEMO MEMVAR 
             m.LLSEL = .T.
             INSERT INTO (loFormSet.lcTempFileHdr) FROM MEMVAR 
             lcOrder = m.Order
             SELECT Ordline 
             =gfSeek('O'+lcOrder)
             SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = "O"+lcOrder FOR TOTQTY > 0
               SCATTER MEMO MEMVAR 
               m.DC = ""
               IF !EMPTY(m.Store) AND gfSeek('S'+m.Account+m.Store,'Customer','Customer') AND !EMPTY(Customer.Dist_CTR)
                 m.DC = Customer.Dist_CTR 
               ENDIF  
               IF !SEEK(m.Order+m.Store+m.Style,loFormSet.lcDetailFile)
                 INSERT INTO (loFormSet.lcDetailFile) FROM MEMVAR     
               ELSE
                 REPLACE TotQty WITH TotQty + Ordline.TOTQTY
			   ENDIF
             ENDSCAN
          ENDSCAN
          SELECT(lcFileAccount) 
        ENDSCAN 
    
     OTHERWISE 
       SELECT ORDHDR  
       =gfSetOrder("ORDHDR")  && CORDTYPE+ORDER 
       =gfSeek("O")
       SCAN REST WHILE CORDTYPE+ORDER = "O"  FOR IIF(llSeleCompDate,BETWEEN(ORDHDR.COMPLETE,ldCompStart,ldCompEnd),.T.) AND ;
             IIF(llSeleEntDate,BETWEEN(Ordhdr.Entered,ldEntStart,ldEntEnd),.T.) AND !(Ordhdr.Status  $ 'CX')AND ORDHDR.MULTI = 'Y'
         SCATTER MEMO MEMVAR 
         m.LLSEL = .T.
         INSERT INTO (loFormSet.lcTempFileHdr) FROM MEMVAR   
         lcOrder = m.Order
         SELECT Ordline 
         =gfSeek('O'+lcOrder)
         SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = "O"+lcOrder FOR TOTQTY > 0
            SCATTER MEMO MEMVAR 
            m.DC = ""
            IF !EMPTY(m.Store) AND gfSeek('S'+m.Account+m.Store,'Customer','Customer') AND !EMPTY(Customer.Dist_CTR)
              m.DC = Customer.Dist_CTR 
            ENDIF  
            IF !SEEK(m.Order+m.Store+m.Style,loFormSet.lcDetailFile)
              INSERT INTO (loFormSet.lcDetailFile) FROM MEMVAR     
            ELSE
              REPLACE TotQty WITH TotQty + Ordline.TOTQTY
            ENDIF  
         ENDSCAN        
   	   ENDSCAN
   ENDCASE 


 
   SELECT (loFormSet.lcTempFileHdr)
   SET RELATION TO Order INTO (loFormSet.lcDetailFile) ADDITIVE 
   LOCATE

   lfAddControlSource(loFormSet)
   loFormSet.lnSelRec   = RECCOUNT()
   loFormSet.lnDelRec   = 0
   loFormSet.llenablerel = .T.
   loFormSet.lnUnSelRec = 0
   IF !EOF()
     loFormSet.llEnableInvert = .T.
     loFormSet.llEnableSelect = .T.
     loFormSet.llEnableSelectall = .F.
     loFormSet.llEnableSelectnone = .T.
   ELSE    && Else
     loFormSet.llEnableInvert = .F.
     loFormSet.llEnableSelect = .F.
     loFormSet.llEnableSelectAll = .F.
     loFormSet.llEnableSelectNone = .F.
   ENDIF
   IF !loFormSet.llEnableRel
     loFormSet.oToolBar.ChangeButtonStatus('cmdExport','DISABLED')
   ELSE
     loFormSet.oToolBar.ChangeButtonStatus('cmdExport','ENABLED')
   ENDIF 
    && End of IF
 ELSE
 RETURN
ENDIF
*!*****************************************************************************************
*! Name      : lfsrOrder
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 12/20/2015 
*! Purpose   : Reset/Set order browser
*!*****************************************************************************************
FUNCTION lfsrOrder
PARAMETERS lcParm
IF lcParm = "S"
  SELECT ORDHDR
  LOCATE
ENDIF
*!*************************************************************
*! Name      : lfAddControlSource
*! Developer : Mariam Mazhar (MMT)
*! Date      : 12/20/2015
*! Purpose   : Adjust grid control source
*!*************************************************************
FUNCTION lfAddControlSource
PARAMETERS loFormSet
  WITH loFormSet.ariaform1.grdOrders.grdMultiSelectionGrid
    .RecordSource = ""
    .RecordSource = loFormSet.lcTempFileHdr 
    .Column1.Header1.Caption = ""
    .Column1.CurrentControl = "AriaCheckBox1"
    .column1.ControlSource ='Thisformset.mgetValueLogic()'
    .column2.ControlSource = loFormSet.lcTempFileHdr +'.Order'
    .column3.ControlSource = loFormSet.lcTempFileHdr +'.ACCOUNT'
    .column4.ControlSource = loFormSet.lcTempFileHdr +'.CWARECODE'
    .column5.ControlSource = loFormSet.lcTempFileHdr +'.Open'
    .column6.ControlSource  = loFormSet.lcTempFileHdr +'.OpenAmt'
   
    .Column1.Enabled = .T.
    .SETALL('ReadOnly',.T.,'COLUMN')
    .Column1.readonly = .F.
	.Enabled = .T.
    .Column1.AriaCheckBox1.Enabled = .T.
    .refresh()
  ENDWITH
  WITH loFormSet.ariaform1.grdDetails
    .RecordSource = ""
    .RecordSource = loFormSet.lcDetailFile 
    .column1.ControlSource = loFormSet.lcDetailFile +'.Store'
    .column2.ControlSource = loFormSet.lcDetailFile +'.Style'
    .column3.ControlSource = loFormSet.lcDetailFile +'.TotQty'
    .column4.ControlSource = loFormSet.lcDetailFile +'.DC'
    .SETALL('ReadOnly',.T.,'COLUMN')
    .Enabled = .T.
    .refresh()
  ENDWITH
ENDFUNC
*!*************************************************************
*! Name      : lfvSelAll
*! Developer : Mariam Mazhar [MMT]
*! Date      : 12/20/2015
*! Purpose   : Valid function of push button Select all
*!*************************************************************
*! Called from : Scrren ALRELPIK
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvSelAll
PARAMETERS loFormSet
SELECT(loFormSet.lcTempFileHdr)
lnRecCurrn = RECNO()

REPLACE ALL LLSEL WITH .T.
loFormSet.lnSelRec   = RECCOUNT() - loFormSet.lnDelRec
loFormSet.lnUnSelRec = 0
IF BETWEEN(lnRecCurrn,1,RECCOUNT())
  GO lnRecCurrn
ENDIF

loFormSet.lcCaptionSel = "UnSelect"

loFormSet.llenableselectall = .F.
loFormSet.llenableselectnone = .T.
loFormSet.llenablerel = .T.
*!*************************************************************
*! Name      : lfvSelNon
*! Developer : Mariam Mazhar [MMT]
*! Date      : 12/20/2015
*! Purpose   : Valid function of push button Select none
*!*************************************************************
*! Called from : Scrren ALRELPIK
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvSelNon
PARAMETERS loFormSet
SELECT(loFormSet.lcTempFileHdr)

lnRecCurr = RECNO()

REPLACE ALL LLSEL WITH .F.

loFormSet.lnSelRec   = 0
loFormSet.lnUnSelRec = RECCOUNT() - loFormSet.lnDelRec
IF BETWEEN(lnRecCurr,1,RECCOUNT())
  GO lnRecCurr
ENDIF
loFormSet.lcCaptionSel = "Select"

loFormSet.llEnableSelectAll  = .T.
loFormSet.llEnableSelectNone = .F.
loFormSet.llEnableRel = .F.
*!*************************************************************
*! Name      : lfvInvert
*! Developer : Mariam Mazhar [MMT]
*! Date      : 12/20/2015
*! Purpose   : Valid function of push button Invert
*!*************************************************************
*! Called from : Scrren ALRELPIK
*!*************************************************************
*! Calls       : lfvpbSel()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvInvert
PARAMETERS loFormSet
SELECT(loFormSet.lcTempFileHdr)
lnRecNOCurr = RECNO()
REPLACE ALL LLSEL WITH !LLSEL
IF BETWEEN(lnRecNOCurr,1,RECCOUNT())
  GO lnRecNOCurr
ENDIF  

lfvpbSel(loFormSet)

loFormSet.lnUnSelRec = loFormSet.lnSelRec
loFormSet.lnSelRec   = RECCOUNT() - loFormSet.lnDelRec - loFormSet.lnSelRec

*there is no selected records
IF loFormSet.lnSelRec = 0
  loFormSet.llenableselectall = .T.
  loFormSet.llenableselectnone = .F.
  loFormSet.llenablerel = .F.
ELSE
  loFormSet.llenableselectnone = .T.
  loFormSet.llenablerel = .T.

  *--All the records were selected
  IF loFormSet.lnUnSelRec = 0
    loFormSet.llenableselectall = .F.
  ENDIF
ENDIF
*!*************************************************************
*! Name      : lfvSelect
*! Developer : Mariam Mazhar [MMT]
*! Date      : 12/20/2015
*! Purpose   : Valid function of push button Select
*!*************************************************************
*! Called from : Scrren ALRELPIK
*!*************************************************************
*! Calls       : lfvpbSel()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvSelect
PARAMETERS loFormSet
SELECT(loFormSet.lcTempFileHdr)
REPLACE LLSEL WITH !LLSEL
lfvpbSel(loFormSet)

loFormSet.lnSelRec   = IIF(llSel , loFormSet.lnSelRec + 1 , loFormSet.lnSelRec - 1)
loFormSet.lnUnSelRec = IIF(llSel , loFormSet.lnUnSelRec - 1 , loFormSet.lnUnSelRec + 1)

*No records was selected
IF loFormSet.lnSelRec = 0
  loFormSet.llenableinvert = .T.
  loFormSet.llenableselect = .T.
  loFormSet.llenableselectall = .T.
  loFormSet.llenableselectnone = .F.
  loFormSet.llenablerel = .F.
ELSE    && Else
  loFormSet.llenableselectnone = .T.
  loFormSet.llenablerel = .T.

  *-- All the records were selected
  IF loFormSet.lnUnSelRec = 0
    loFormSet.llenableselectall = .F.
  ELSE
    loFormSet.llenableselectall = .T.
  ENDIF
ENDIF
*!*************************************************************
*! Name      : lfvpbSel
*! Developer : Mariam Mazhar [MMT]
*! Date      : 12/20/2015
*! Purpose   : Function to arange the push button select prompt
*!*************************************************************
*! Called from : lfvSelect() , lfvInvert() , The Browse [lcPickBrow]
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : .T.
*!*************************************************************
FUNCTION lfvpbSel
PARAMETERS loFormSet
IF LLSEL
  loFormSet.lcCaptionSel = "UnSelect"
ELSE
  loFormSet.lcCaptionSel = "Select"
ENDIF

RETURN .T.
*!*************************************************************
*! Name      : lfOGWhen
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 12/20/2015 
*! Purpose   : When function for the Option Grid
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfOGWhen
*!**************************************************************************
*! Name      : lfvShipAddress1
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 12/20/2015 
*! Purpose   : the Order Browse customer shipping address 1
*!**************************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : Valid Shipping Address 1
*!**************************************************************************
*! Example   : =lfvShipAddress1("MA100","HOB")
*!**************************************************************************
*!
FUNCTION lfvShipAddress1
LPARAMETERS lcAccount, lcStore
LOCAL lcAddress1, lnSelect
lnSelect = SELECT(0)
IF !USED("Customer")
  gfOpenTable("Customer","Customer")
ENDIF
IF EMPTY(lcStore)
  =gfSEEK("M"+lcAccount,"CUSTOMER","CUSTOMER")
ELSE
  =gfSEEK("S"+lcAccount+lcStore,"CUSTOMER","CUSTOMER")
ENDIF
lcAddress1 = Customer.cAddress1

SELECT(lnSelect)
RETURN lcAddress1
ENDFUNC

*!*************************************************************
*! Name      : lfCopyCriteria
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 12/20/2015 
*! Purpose   : Function to Copy OG criteria and validate OG options
*!*************************************************************
FUNCTION lfCopyCriteria
IF EMPTY(ALLTRIM(lcRpPath)) OR !DIRECTORY(lcRpPath)
  =gfModalgen("TRM00000B00000","DIALOG",.F.,.F.,"Invalid Output File Path")
  RETURN .F.
ENDIF
lcRpFPath  = lcRpPath
ACOPY(loogscroll.laogFxflt,laOgSelect)
lcRpGroup  = lcRpGrBy

*!*************************************************************
*! Name      : lfExportXLS
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 12/20/2015 
*! Purpose   : Function to Export selected orders to XLS
*!*************************************************************
FUNCTION lfExportXLS
PARAMETERS loFormSet
lcRpGroup =loFormSet.lcRpGroup
IF USED(loFormSet.lcexporttemp)
  USE IN (loFormSet.lcexporttemp)
ENDIF

IF !USED('SPCK_LIN')
  =gfOpenTable('SPCK_LIN','SPCKLINS','SH')   && TYPE+ACCOUNT+STYLE+PACK_ID
ELSE
  SELECT SPCK_LIN
  gfSetOrder('SPCKLINS')  
ENDIF

  DIMENSION laFileExp[3,4]
  IF lcRpGroup = 'S'
    laFileExp[1,1]  = 'STORE_ID'
  ELSE
    laFileExp[1,1]  = 'DC'
  ENDIF  
  laFileExp[1,2] = 'C'
  laFileExp[1,3] = 8
  laFileExp[1,4] = 0
  
  laFileExp[2,1] = 'SKU'
  laFileExp[2,2] = 'C'
  laFileExp[2,3] = 16
  laFileExp[2,4] = 0
  
  laFileExp[3,1] = 'Quantity'
  laFileExp[3,2] = 'N'
  laFileExp[3,3] = 15
  laFileExp[3,4] = 0
  
   =gfCrtTmp(loFormSet.lcexporttemp,@laFileExp,IIF(lcRpGroup = 'S','STORE_ID','DC')+"+SKU",  loFormSet.lcexporttemp,.T.)
  
  SELECT (loFormSet.lcTempFileHdr)
  LOCATE 
  SCAN FOR llSel
    lcOrder = Order
    WAIT WINDOW "Collecting data for Order# " + lcOrder NOWAIT 
    lcAccount = Account
    SELECT (loFormSet.lcDetailFile)
    =SEEK(lcOrder)
    SCAN REST WHILE ORDER+STORE+STYLE = lcOrder
      lcStyle = Style
      =gfSeek(lcStyle,'Style','Style')
      =gfSeek('S'+Style.Scale,'Scale','Scale')
      FOR lnQcnt = 1 TO Scale.cnt
        lcQcnt = STR(lnQcnt,1)
        IF EVALUATE(loFormSet.lcDetailFile+".Qty"+lcQcnt) > 0
          SELECT SPCK_LIN
          =gfSeek('S'+lcAccount+lcStyle,'SPCK_LIN','SPCKLINS')
          LOCATE REST WHILE TYPE+ACCOUNT+STYLE+PACK_ID = 'S'+lcAccount+lcStyle FOR Qty&lcQcnt. = 1
          IF FOUND()
            IF !SEEK(IIF(lcRpGroup = 'S',EVALUATE(loFormSet.lcDetailFile+".Store"),EVALUATE(loFormSet.lcDetailFile+".DC"))+SPCK_LIN.PACK_ID,loFormSet.lcexporttemp)
              INSERT INTO (loFormSet.lcexporttemp) VALUES (IIF(lcRpGroup = 'S',EVALUATE(loFormSet.lcDetailFile+".Store"),EVALUATE(loFormSet.lcDetailFile+".DC")),;
                SPCK_LIN.PACK_ID,EVALUATE(loFormSet.lcDetailFile+".Qty"+lcQcnt))
            ELSE
              REPLACE Quantity WITH Quantity+EVALUATE(loFormSet.lcDetailFile+".Qty"+lcQcnt) IN  (loFormSet.lcexporttemp)
            ENDIF
          ENDIF
        ENDIF
      ENDFOR 
    ENDSCAN
  ENDSCAN
  WAIT CLEAR 
loFormSet.lcexportPath = ADDBS(ALLTRIM(loFormSet.lcexportPath))
*: B611114,1 MMT 02/10/2016 Issue#3 - Custom Export Allocation program exports only 65000 records[T20151014.0017][Start]
*lcFileName = loFormSet.lcexportPath +lcRpGroup+"A-"+ALLTRIM(STR(YEAR(DATE())))+ALLTRIM(STR(MONTH(DATE())))+ALLTRIM(STR(DAY(DATE())))+".XLS"
*IF FILE(lcFileName+"X")
*  IF gfModalgen("QRM00000B00006","DIALOG",.F.,.F.,'File: '+lcFileName+"X"+' already exists. Would you like to overwrite it?') = 2 &&NO
lcFileName = loFormSet.lcexportPath +lcRpGroup+"A-"+ALLTRIM(STR(YEAR(DATE())))+ALLTRIM(STR(MONTH(DATE())))+ALLTRIM(STR(DAY(DATE())))+".XLSX"
IF FILE(lcFileName)
  IF gfModalgen("QRM00000B00006","DIALOG",.F.,.F.,'File: '+lcFileName+' already exists. Would you like to overwrite it?') = 2 &&NO
*: B611114,1 MMT 02/10/2016 Issue#3 - Custom Export Allocation program exports only 65000 records[T20151014.0017][End]
    RETURN
  *: B611114,1 MMT 02/10/2016 Issue#3 - Custom Export Allocation program exports only 65000 records[T20151014.0017][Start]
  ELSE
    ERASE (lcFileName)
  *: B611114,1 MMT 02/10/2016 Issue#3 - Custom Export Allocation program exports only 65000 records[T20151014.0017][End]
  ENDIF
ENDIF

SELECT  (loFormSet.lcexporttemp)
LOCATE 
IF !EOF()
   *EXPORT TO (lcFileName) TYPE XLS  
   *: B611114,1 MMT 02/10/2016 Issue#3 - Custom Export Allocation program exports only 65000 records[T20151014.0017][Start]
   *lcDataFile = ADDBS(JUSTPATH(lcFileName)) + "TOTAL COMBINED.XLS"
   lcDataFile = ADDBS(JUSTPATH(lcFileName)) + "TOTAL COMBINED.CSV"
   lcFileName = FORCEEXT(lcFileName ,'CSV')
   *: B611114,1 MMT 02/10/2016 Issue#3 - Custom Export Allocation program exports only 65000 records[T20151014.0017][End]
   IF FILE(lcDataFile)
     ERASE (lcDataFile)
   ENDIF
   *: B611114,1 MMT 02/10/2016 Issue#3 - Custom Export Allocation program exports only 65000 records[T20151014.0017][Start] 
   *COPY TO (lcDataFile) TYPE XL5   
   *IF FILE(lcFileName+"X") 
   *  ERASE (lcFileName+"X")
   COPY TO (lcDataFile) TYPE CSV
   IF FILE(lcFileName) 
     ERASE (lcFileName)
   *: B611114,1 MMT 02/10/2016 Issue#3 - Custom Export Allocation program exports only 65000 records[T20151014.0017][End]     
   ENDIF
   IF !FILE(lcDataFile)
     WAIT CLEAR 
     *--
     *-- Excel connection failed.
     =gfModalGen("INM00406B00000","DIALOGE")  
     RETURN .F.
   ENDIF
   *: B611114,1 MMT 02/10/2016 Issue#3 - Custom Export Allocation program exports only 65000 records[T20151014.0017][Start]
   *RENAME (lcDataFile) TO (lcFileName)
   RENAME (lcDataFile) TO (lcFileName)
   *: B611114,1 MMT 02/10/2016 Issue#3 - Custom Export Allocation program exports only 65000 records[T20151014.0017][End]
   LOCAL oExcel, lnActiveAlias, lnRowRef, lcCelRef, lcCellValue
   lnActiveAlias = SELECT(0)
   oExcel = CreateOBJECT("Table2Excel",lcFileName)
   IF VARTYPE(oExcel) != "O" OR ISNULL(oExcel)
     WAIT CLEAR 
     =gfModalGen("INM00406B00000","DIALOGE")  
     RETURN .F.
   ENDIF
   oExcel.XLSFORMAT = 'XLSX'
   oExcel.Save()                 && Save this sheet.
   oExcel.Close()                && Close the sheet.
   oExcel = .NULL.
   *: B611114,1 MMT 02/10/2016 Issue#3 - Custom Export Allocation program exports only 65000 records[T20151014.0017][Start]
   *IF FILE(lcFileName+"X")
   *=gfModalgen("INM00000B00000","DIALOG",.F.,.F.,"File: "+lcFileName+"X"+" has been created successfully")
   lcFileName = FORCEEXT(lcFileName ,'XLSX')
   IF FILE(lcFileName)
     =gfModalgen("INM00000B00000","DIALOG",.F.,.F.,"File: "+lcFileName+" has been created successfully")
   *: B611114,1 MMT 02/10/2016 Issue#3 - Custom Export Allocation program exports only 65000 records[T20151014.0017][End]
   ENDIF
ELSE
  =gfModalgen("TRM00000B00000","DIALOG",.F.,.F.,"None of the selected orders styles has UPC#. cannot export")
ENDIF
*!*************************************************************
*! Name      : lfvOutPth
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 12/20/2015 
*! Purpose   : Validate file Path
*!*************************************************************
FUNCTION lfvOutPth
IF EMPTY(ALLTRIM(lcRpPath)) OR !DIRECTORY(lcRpPath)
  lcRpPath = GETDIR('',"Select Output File Path")
ENDIF