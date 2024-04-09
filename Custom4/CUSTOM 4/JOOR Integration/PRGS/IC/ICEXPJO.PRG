**:***********************************************************************
*:  Name         : ICEXPJO.PRG
*:  Program desc.: Style Inventory Bulk Update
*:         System: Aria 4XP
*:      Developer: Mariam Mazhar
*:           Date: 01/05/2016
*:      Entry no.: C201759 [T20151217.0010]
*:************************************************************************
*: Modifications:
*:************************************************************************
*: C201759,2 MMT 02/01/2016 Add Royalty option, Don't Export style has no UPC, and Export Zero Stock Style[T20151217.0010]
*: C201759,3 MMT 02/07/2016 Change the date field from date to string[T20151217.0010]
*!*************************************************************
*! Name      : lfvScope
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 01/06/2016 
*! Purpose   : Scope function to call selection criteria OG
*!*************************************************************
FUNCTION lfvScope
PARAMETERS loFormSet
lndataSessPre = SET("Datasession" )
IF loFormSet.llCalledFromOp
  loFormSet.llCallOption = loFormSet.llCalledFromOp
ENDIF
loFormSet.llCallScop = .F.             && Screen Already Initialized
lcDataSessI = SET("Datasession" )&&THIS.loFormSet.DatasessionID
DIMENSION laOgSelect[1]
laOgSelect =''

lcExpr = gfOpGrid('ICEXPRJR' , .T.)&&,.F.,.F.,.T.,.T.)
SET DATASESSION TO lcDataSessI


IF lcExpr <> ".F."
   SELECT (loFormSet.lcTempFileHdr) 
   ZAP
   *!*	Style
   llSeleStyle= .F. && flag to indicate if there is any piktkt selected
   lnPosStyle = ASCAN(laOgSelect,"STYLE.STYLE")
   IF lnPosStyle > 0
     lnPosStyle = ASUBSCRIPT(laOgSelect,lnPosStyle,1)
     lcFileStyle =IIF(!EMPTY(laOgSelect[lnPosStyle,6]),laOgSelect[lnPosStyle,6],'')
     IF !EMPTY(lcFileStyle) AND USED(lcFileStyle) AND RECCOUNT(lcFileStyle)> 0
       SELECT(lcFileStyle)
       LOCATE For !DELETED()
       IF !EOF()
         llSeleStyle = .T.
       ENDIF  
     ENDIF
   ENDIF
   
   *!*	Pattern
   lcPattern= ''
   lnPosPatt= ASCAN(laOgSelect,"STYLE.PATTERN")
   IF lnPosPatt > 0
     lnPosPatt = ASUBSCRIPT(laOgSelect,lnPosPatt,1)
     lcPattern =IIF(!EMPTY(laOgSelect[lnPosPatt,6]),laOgSelect[lnPosPatt,6],'')
   ENDIF
   
   *!*	CWARECODE 
   llSeleWare= .F. && flag to indicate if there is any piktkt selected
   lnPosWare= ASCAN(laOgSelect,"STYDYE.CWARECODE")
   IF lnPosWare > 0
     lnPosWare= ASUBSCRIPT(laOgSelect,lnPosWare,1)
     lcFileWare=IIF(!EMPTY(laOgSelect[lnPosWare,6]),laOgSelect[lnPosWare,6],'')
     IF !EMPTY(lcFileWare) AND USED(lcFileWare) AND RECCOUNT(lcFileWare)> 0
       SELECT(lcFileWare)
       LOCATE For !DELETED()
       IF !EOF()
         llSeleWare= .T.
       ENDIF  
     ENDIF
   ENDIF
   loFormSet.llSeleWare  = llSeleWare  
   *SEASON
   llUseSeason  = .F.
   lnSeaPos = ASCAN(laOgSelect,"STYLE.SEASON")
   IF lnSeaPos > 0
	 lnSeaPos = ASUBSCRIPT(laOgSelect,lnSeaPos,1)
	 lcSeaSel =IIF(!EMPTY(laOgSelect[lnSeaPos,6]),laOgSelect[lnSeaPos,6],'')
	 IF !EMPTY(lcSeaSel)
	   lcSeaFile = gfTempName()
	   llUseSeason = IIF(LEN(lcSeaSel)>0,.T.,.F.) AND lfConvertToCursor(lcSeaSel,'SEASON',lcSeaFile)
	 ENDIF
   ENDIF

	*DIVISION
	llUseDiv  = .F.
	lnDivPos = ASCAN(laOgSelect,"STYLE.CDIVISION")
	IF lnDivPos > 0
	  lnDivPos = ASUBSCRIPT(laOgSelect,lnDivPos,1)
	  lcDivSel =IIF(!EMPTY(laOgSelect[lnDivPos,6]),laOgSelect[lnDivPos,6],'')
	  IF !EMPTY(lcDivSel)
	    lcDivFile = gfTempName()
	    llUseDiv = IIF(LEN(lcDivSel)>0,.T.,.F.) AND lfConvertToCursor(lcDivSel,'CDIVISION',lcDivFile)
	  ENDIF
	ENDIF


	*Style Group
	llUseGrp  = .F.
	lnGrpPos = ASCAN(laOgSelect,"STYLE.CSTYGROUP")
	IF lnGrpPos  > 0
	  lnGrpPos  = ASUBSCRIPT(laOgSelect,lnGrpPos ,1)
	  lcGrpSel =IIF(!EMPTY(laOgSelect[lnGrpPos ,6]),laOgSelect[lnGrpPos ,6],'')
	  IF !EMPTY(lcGrpSel)
	    lcGrpFile = gfTempName()
	    llUseGrp = IIF(LEN(lcGrpSel)>0,.T.,.F.) AND lfConvertToCursor(lcGrpSel,'CSTYGRP',lcGrpFile)
	  ENDIF
	ENDIF
	
	*Status
    lcStatusVal =""
    lnStatPos = ASCAN(laOgSelect,"STYLE.STATUS")
    IF lnStatPos > 0
      lnStatPos = ASUBSCRIPT(laOgSelect,lnStatPos ,1)
      IF !Empt(laOgSelect[lnStatPos ,6])
        lcStatusVal ="'"+STRTRAN(laOgSelect[lnStatPos ,6],"|","','")+"'"
      ENDIF  
    ENDIF      
    * C201759,2 Add Royalty option, Don't Export style has no UPC, and Export Zero Stock Style[T20151217.0010][Start]
    llUseRoyal  = .F.
	lnRoyPos = ASCAN(laOgSelect,"STYLE.ROYALTY")
	IF lnRoyPos   > 0
	  lnRoyPos  = ASUBSCRIPT(laOgSelect,lnRoyPos  ,1)
	  lcRoySel =IIF(!EMPTY(laOgSelect[lnRoyPos  ,6]),laOgSelect[lnRoyPos ,6],'')
	  IF !EMPTY(lcRoySel)
	    lcRoyFile = gfTempName()
	    llUseRoyal = IIF(LEN(lcRoySel)>0,.T.,.F.) AND lfConvertToCursor(lcRoySel ,'ROYALTY',lcRoyFile)
	  ENDIF
	ENDIF
    * C201759,2 Add Royalty option, Don't Export style has no UPC, and Export Zero Stock Style[T20151217.0010][End]
	
   IF llSeleWare  
     SELECT STYDYE
     =gfSetOrder('STYDYEW')
     SELECT (lcFileWare)
     LOCATE
     SCAN 
       SELECT STYDYE
       =gfSeek(&lcFileWare..cWareCode)  
       *: C201759,2 Add Royalty option, Don't Export style has no UPC, and Export Zero Stock Style[T20151217.0010][Start]
*!*	       SCAN REST WHILE CWARECODE+STYLE+DYELOT = &lcFileWare..cWareCode FOR EMPTY(Dyelot) AND ;
*!*	            IIF(llSeleStyle,SEEK(STYDYE.Style,lcFileStyle),.T.) AND gfSeek(STYDYE.Style,'Style','Style')  AND;
*!*	            IIF(!EMPTY(lcStatusVal),Style.Status $ lcStatusVal,.T.) AND IIF(!EMPTY(lcPattern),StyLe.pattern =lcPattern,.T.) AND ;
*!*	            IIF(llUseSeason,SEEK(Style.Season,lcSeaFile),.T.) AND IIF(llUseDiv,SEEK(Style.cDivision,lcDivFile),.T.) AND ;
*!*	            IIF(llUseGrp,SEEK(Style.CSTYGROUP,lcGrpFile),.T.) 
       SCAN REST WHILE CWARECODE+STYLE+DYELOT = &lcFileWare..cWareCode FOR EMPTY(Dyelot) AND ;
            IIF(llSeleStyle,SEEK(STYDYE.Style,lcFileStyle),.T.) AND gfSeek(STYDYE.Style,'Style','Style')  AND;
            IIF(!EMPTY(lcStatusVal),Style.Status $ lcStatusVal,.T.) AND IIF(!EMPTY(lcPattern),StyLe.pattern =lcPattern,.T.) AND ;
            IIF(llUseSeason,SEEK(Style.Season,lcSeaFile),.T.) AND IIF(llUseDiv,SEEK(Style.cDivision,lcDivFile),.T.) AND ;
            IIF(llUseGrp,SEEK(Style.CSTYGROUP,lcGrpFile),.T.) AND IIF(llUseRoyal, SEEK(STYLE.ROYALTY,lcRoyFile),.T.)
       *: C201759,2 Add Royalty option, Don't Export style has no UPC, and Export Zero Stock Style[T20151217.0010][End]     
         WAIT WINDOW 'Collecting data for Style: '+ STYDYE.STYLE  NOWAIT    
         =gfSeek('S'+STyle.Scale,'Scale','Scale')  
         =gfSeek(&lcFileWare..cWareCode,'WAREHOUS')
         lfGetWorkInProcess(&lcFileWare..cWareCode,STYDYE.Style)
         FOR lnSCnt = 1 To Scale.cnt
           lcSCnt = STR(lnSCnt ,1)
           *: C201759,2 Add Royalty option, Don't Export style has no UPC, and Export Zero Stock Style[T20151217.0010][Start]
           *IF STYDYE.STK&lcSCnt. > 0
           *  =gfSeek(STYDYE.Style+lcSCnt ,'STYLEUPC','STYLEUPC')
           IF STYDYE.STK&lcSCnt. >= 0 AND gfSeek(STYDYE.Style+lcSCnt ,'STYLEUPC','STYLEUPC')
           *: C201759,2 Add Royalty option, Don't Export style has no UPC, and Export Zero Stock Style[T20151217.0010][End]
                 *: C201759,3 MMT 02/07/2016 Change the date field from date to string[T20151217.0010][Start]
*!*	             INSERT INTO (loFormSet.lcTempFileHdr) (llSel,WarehouseID,Style,SIZE,Quantity,UPC,Warehouse,Date) VALUES ;
*!*	             (.T.,STYDYE.cWareCode,STYDYE.Style,Scale.SZ&lcSCnt.,STYDYE.STK&lcSCnt.,STYLEUPC.CUPCNUM1+STYLEUPC.CUPCNUM2+STYLEUPC.CUPCNUM3,WAREHOUS.cdesc,{})
             INSERT INTO (loFormSet.lcTempFileHdr) (llSel,WarehouseID,Style,SIZE,Quantity,UPC,Warehouse,Date) VALUES ;
             (.T.,STYDYE.cWareCode,STYDYE.Style,Scale.SZ&lcSCnt.,STYDYE.STK&lcSCnt.,STYLEUPC.CUPCNUM1+STYLEUPC.CUPCNUM2+STYLEUPC.CUPCNUM3,WAREHOUS.cdesc,'')
             *: C201759,3 MMT 02/07/2016 Change the date field from date to string[T20151217.0010][End]
           ENDIF
         ENDFOR  
       ENDSCAN
     ENDSCAN 
   ELSE
     SELECT STYDYE
     =gfSetOrder('STYDYE')
     IF llSeleStyle
       SELECT STYLE
       =gfSetOrder('Style')
       SELECT (lcFileStyle)
       LOCATE
       SCAN
         SELECT STYLE 
         =gfSeek(&lcFileStyle..STYLE)
         *: C201759,2 Add Royalty option, Don't Export style has no UPC, and Export Zero Stock Style[T20151217.0010][Start]
*!*	         IF IIF(!EMPTY(lcStatusVal),Style.Status $ lcStatusVal,.T.) AND IIF(!EMPTY(lcPattern),StyLe.pattern =lcPattern,.T.) AND ;
*!*	            IIF(llUseSeason,SEEK(Style.Season,lcSeaFile),.T.) AND IIF(llUseDiv,SEEK(Style.cDivision,lcDivFile),.T.) AND ;
*!*	            IIF(llUseGrp,SEEK(Style.CSTYGROUP,lcGrpFile),.T.) 
         IF IIF(!EMPTY(lcStatusVal),Style.Status $ lcStatusVal,.T.) AND IIF(!EMPTY(lcPattern),StyLe.pattern =lcPattern,.T.) AND ;
            IIF(llUseSeason,SEEK(Style.Season,lcSeaFile),.T.) AND IIF(llUseDiv,SEEK(Style.cDivision,lcDivFile),.T.) AND ;
            IIF(llUseGrp,SEEK(Style.CSTYGROUP,lcGrpFile),.T.) AND IIF(llUseRoyal, SEEK(STYLE.ROYALTY,lcRoyFile),.T.)
        *: C201759,2 Add Royalty option, Don't Export style has no UPC, and Export Zero Stock Style[T20151217.0010][End]
           =gfSeek('S'+STyle.Scale,'Scale','Scale')  
           SELECT STYDYE
           =gfSeek(&lcFileStyle..STYLE)
           SCAN REST WHILE STYLE+CWARECODE+DYELOT = &lcFileStyle..STYLE FOR EMPTY(Dyelot) AND !EMPTY(ALLTRIM(cWareCode))
             WAIT WINDOW 'Collecting data for Style: '+ &lcFileStyle..STYLE  NOWAIT 
             =gfSeek(STYDYE.cWareCode,'WAREHOUS')
             lfGetWorkInProcess(STYDYE.cWareCode,STYLE.Style)
             FOR lnSCnt = 1 To Scale.cnt
               lcSCnt = STR(lnSCnt ,1)
               *: C201759,2 Add Royalty option, Don't Export style has no UPC, and Export Zero Stock Style[T20151217.0010][Start]
               *IF STYDYE.STK&lcSCnt. > 0
               *  =gfSeek(STYLE.Style+lcSCnt ,'STYLEUPC','STYLEUPC')
               IF STYDYE.STK&lcSCnt. >= 0 AND gfSeek(STYLE.Style+lcSCnt ,'STYLEUPC','STYLEUPC')
               *: C201759,2 Add Royalty option, Don't Export style has no UPC, and Export Zero Stock Style[T20151217.0010][End]
                 *: C201759,3 MMT 02/07/2016 Change the date field from date to string[T20151217.0010][Start]
*!*	                 INSERT INTO (loFormSet.lcTempFileHdr) (llSel,WarehouseID,Style,SIZE,Quantity,UPC,Warehouse,Date) VALUES ;
*!*	                 (.T.,STYDYE.CWARECODE,STYLE.Style,Scale.SZ&lcSCnt.,STYDYE.STK&lcSCnt.,STYLEUPC.CUPCNUM1+STYLEUPC.CUPCNUM2+STYLEUPC.CUPCNUM3,WAREHOUS.cdesc,{})
                 INSERT INTO (loFormSet.lcTempFileHdr) (llSel,WarehouseID,Style,SIZE,Quantity,UPC,Warehouse,Date) VALUES ;
                 (.T.,STYDYE.CWARECODE,STYLE.Style,Scale.SZ&lcSCnt.,STYDYE.STK&lcSCnt.,STYLEUPC.CUPCNUM1+STYLEUPC.CUPCNUM2+STYLEUPC.CUPCNUM3,WAREHOUS.cdesc,'')
                 *: C201759,3 MMT 02/07/2016 Change the date field from date to string[T20151217.0010][End]
               ENDIF
             ENDFOR  
           ENDSCAN   
         ENDIF  
       ENDSCAN
     ELSE
       SELECT STYLE
       =gfSetOrder('Style')
       LOCATE
       *: C201759,2 Add Royalty option, Don't Export style has no UPC, and Export Zero Stock Style[T20151217.0010][Start]
*!*	       SCAN FOR IIF(!EMPTY(lcStatusVal),Style.Status $ lcStatusVal,.T.) AND IIF(!EMPTY(lcPattern),StyLe.pattern =lcPattern,.T.) AND ;
*!*	            IIF(llUseSeason,SEEK(Style.Season,lcSeaFile),.T.) AND IIF(llUseDiv,SEEK(Style.cDivision,lcDivFile),.T.) AND ;
*!*	            IIF(llUseGrp,SEEK(Style.CSTYGROUP,lcGrpFile),.T.) 
       SCAN FOR IIF(!EMPTY(lcStatusVal),Style.Status $ lcStatusVal,.T.) AND IIF(!EMPTY(lcPattern),StyLe.pattern =lcPattern,.T.) AND ;
            IIF(llUseSeason,SEEK(Style.Season,lcSeaFile),.T.) AND IIF(llUseDiv,SEEK(Style.cDivision,lcDivFile),.T.) AND ;
            IIF(llUseGrp,SEEK(Style.CSTYGROUP,lcGrpFile),.T.) AND IIF(llUseRoyal, SEEK(STYLE.ROYALTY,lcRoyFile),.T.)
       *: C201759,2 Add Royalty option, Don't Export style has no UPC, and Export Zero Stock Style[T20151217.0010][End]
         =gfSeek('S'+STyle.Scale,'Scale','Scale')  
         SELECT STYDYE
         =gfSeek(STYLE.STYLE)
         SCAN REST WHILE STYLE+CWARECODE+DYELOT = STYLE.STYLE FOR EMPTY(Dyelot) AND !EMPTY(ALLTRIM(cWareCode))
           WAIT WINDOW 'Collecting data for Style: '+ Style.STYLE  NOWAIT
           =gfSeek(STYDYE.cWareCode,'WAREHOUS') 
           lfGetWorkInProcess(STYDYE.cWareCode,STYLE.Style)
           FOR lnSCnt = 1 To Scale.cnt
             lcSCnt = STR(lnSCnt ,1)
             *: C201759,2 Add Royalty option, Don't Export style has no UPC, and Export Zero Stock Style[T20151217.0010][Start]
             *IF STYDYE.STK&lcSCnt. > 0
             *  =gfSeek(STYLE.Style+lcSCnt ,'STYLEUPC','STYLEUPC')
             IF STYDYE.STK&lcSCnt. >= 0 AND gfSeek(STYLE.Style+lcSCnt ,'STYLEUPC','STYLEUPC')  
             *: C201759,2 Add Royalty option, Don't Export style has no UPC, and Export Zero Stock Style[T20151217.0010][End]
               *: C201759,3 MMT 02/07/2016 Change the date field from date to string[T20151217.0010][Start]
*!*	               INSERT INTO (loFormSet.lcTempFileHdr) (llSel,WarehouseID,Style,SIZE,Quantity,UPC,Warehouse,Date) ;
*!*	                      VALUES (.T.,STYDYE.CWARECODE,STYLE.Style,Scale.SZ&lcSCnt.,STYDYE.STK&lcSCnt.,STYLEUPC.CUPCNUM1+STYLEUPC.CUPCNUM2+STYLEUPC.CUPCNUM3,WAREHOUS.cdesc,{})
               INSERT INTO (loFormSet.lcTempFileHdr) (llSel,WarehouseID,Style,SIZE,Quantity,UPC,Warehouse,Date) ;
                      VALUES (.T.,STYDYE.CWARECODE,STYLE.Style,Scale.SZ&lcSCnt.,STYDYE.STK&lcSCnt.,STYLEUPC.CUPCNUM1+STYLEUPC.CUPCNUM2+STYLEUPC.CUPCNUM3,WAREHOUS.cdesc,'')
               *: C201759,3 MMT 02/07/2016 Change the date field from date to string[T20151217.0010][End]
             ENDIF
           ENDFOR  
         ENDSCAN     
       ENDSCAN         
     ENDIF
   ENDIF  
   WAIT CLEAR  
   SELECT (loFormSet.lcTempFileHdr)
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
     =gfModalgen("TRM00000B00000","DIALOG",.F.,.F.,"No records to display.")
     loFormSet.llEnableInvert = .F.
     loFormSet.llEnableSelect = .F.
     loFormSet.llEnableSelectAll = .F.
     loFormSet.llEnableSelectNone = .F.
     loFormSet.llenablerel = .F.
   ENDIF    && End of IF
   IF !loFormSet.llEnableRel
     loFormSet.oToolBar.ChangeButtonStatus('cmdExport','DISABLED')
   ELSE
     loFormSet.oToolBar.ChangeButtonStatus('cmdExport','ENABLED')
   ENDIF 
 ELSE
 RETURN
ENDIF
*!*************************************************************
*! Name      : lfInit
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 01/06/2016
*! Purpose   : init function of th from
*!*************************************************************
FUNCTION lfInit
  LPARAMETERS loFormSet
  loFormSet.lcTempFileHdr = gfTempName()
  SET MULTILOCKS ON
  gfOpenTable('Style','Style')
  gfOpenTable('Stydye','Stydye')
  gfOpenTable('StyleUpc','StyleUpc')
  gfOpenTable('Scale','Scale')
  gfOpenTable('Warehous','WAREHOUS')
  DIMENSION laFileStru[8,4]

  laFileStru[1,1] = 'LLSEL'
  laFileStru[1,2] = 'L'
  laFileStru[1,3] = 1
  laFileStru[1,4] = 0

  laFileStru[2,1] = 'WarehouseID'
  laFileStru[2,2] = 'C'
  laFileStru[2,3] = 6
  laFileStru[2,4] = 0

  laFileStru[3,1] = 'Style'
  laFileStru[3,2] = 'C'
  laFileStru[3,3] = 19
  laFileStru[3,4] = 0
  
  laFileStru[4,1] = 'Size'
  laFileStru[4,2] = 'C'
  laFileStru[4,3] = 5
  laFileStru[4,4] = 0

  laFileStru[5,1] = 'Quantity'
  laFileStru[5,2] = 'N'
  laFileStru[5,3] = 12
  laFileStru[5,4] = 0

  laFileStru[6,1] = 'UPC'
  laFileStru[6,2] = 'C'
  laFileStru[6,3] = 13
  laFileStru[6,4] = 0
  
  laFileStru[7,1] = 'Warehouse'
  laFileStru[7,2] = 'C'
  laFileStru[7,3] = 30
  laFileStru[7,4] = 0
  *: C201759,3 MMT 02/07/2016 Change the date field from date to string[T20151217.0010][Start]
*!*	  laFileStru[8,1] = 'Date'
*!*	  laFileStru[8,2] = 'D'
*!*	  laFileStru[8,3] = 8
  laFileStru[8,1] = 'Date'
  laFileStru[8,2] = 'C'
  laFileStru[8,3] = 10
  *: C201759,3 MMT 02/07/2016 Change the date field from date to string[T20151217.0010][End]
  laFileStru[8,4] = 0
  *: C201759,3 MMT 02/07/2016 Change the date field from date to string[T20151217.0010][Start]
  *=gfCrtTmp(loFormSet.lcTempFileHdr ,@laFileStru,"UPC+Warehouse+DTOS(DATE)",  loFormSet.lcTempFileHdr ,.T.)
  =gfCrtTmp(loFormSet.lcTempFileHdr ,@laFileStru,"UPC+Warehouse+DATE",  loFormSet.lcTempFileHdr ,.T.)
  *: C201759,3 MMT 02/07/2016 Change the date field from date to string[T20151217.0010][End]
  lfAddControlSource(loFormSet)

  WITH loFormSet
    .nWorkArea        = 'STYLE'
    .DataEnvironment.InitialSelectedAlias = 'STYLE'
    .cbrowsetabledbengine   = 'NATIVE'
  ENDWITH

  loFormSet.llMultiWH  = (ALLTRIM(gfGetMemVar('M_WareHouse')) = 'Y')

  loFormSet.llCallScop = .F.   &&Flag to hold the first time of the session
  loFormSet.llFrstTime = .T.       && Flag to know if we are going to call lpShow for the first time
ENDFUNC
*!*************************************************************
*! Name      : lfAddControlSource
*! Developer : Mariam Mazhar (MMT)
*! Date      : 01/06/2016
*! Purpose   : Adjust grid control source
*!*************************************************************
FUNCTION lfAddControlSource
PARAMETERS loFormSet
WITH loFormSet.ariaform1.grdStyles.grdMultiSelectionGrid
    .RecordSource = ""
    .RecordSource = loFormSet.lcTempFileHdr 
    .Column1.Header1.Caption = ""
    .Column1.CurrentControl = "AriaCheckBox1"
    .column1.ControlSource ='Thisformset.mgetValueLogic()'
    .column2.ControlSource = loFormSet.lcTempFileHdr +'.WarehouseID'
*    .column2.VISIBLE = (loFormSet.llMultiWH and loFormSet.llSeleWare)
    .column3.ControlSource = loFormSet.lcTempFileHdr +'.Style'
    .column4.ControlSource = loFormSet.lcTempFileHdr +'.Size'
    .column5.ControlSource = loFormSet.lcTempFileHdr +'.Quantity'
    .column6.ControlSource = loFormSet.lcTempFileHdr +'.UPC'
    .column7.ControlSource = loFormSet.lcTempFileHdr +'.Date'
   
    .Column1.Enabled = .T.
    .SETALL('ReadOnly',.T.,'COLUMN')
    .Column1.readonly = .F.
	.Enabled = .T.
    .Column1.AriaCheckBox1.Enabled = .T.
    .refresh()
  ENDWITH
ENDFUNC
*!*************************************************************
*! Name      : lfvSelAll
*! Developer : Mariam Mazhar [MMT]
*! Date      : 01/06/2016
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
GO lnRecCurrn

loFormSet.lcCaptionSel = "UnSelect"

loFormSet.llenableselectall = .F.
loFormSet.llenableselectnone = .T.
loFormSet.llenablerel = .T.
*!*************************************************************
*! Name      : lfvSelNon
*! Developer : Mariam Mazhar [MMT]
*! Date      : 01/06/2016
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
GO lnRecCurr
loFormSet.lcCaptionSel = "Select"

loFormSet.llEnableSelectAll  = .T.
loFormSet.llEnableSelectNone = .F.
loFormSet.llEnableRel = .F.
*!*************************************************************
*! Name      : lfvInvert
*! Developer : Mariam Mazhar [MMT]
*! Date      : 01/06/2016
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
GO lnRecNOCurr

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
*! Date      : 01/06/2016
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
*! Date      : 01/06/2016
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
*! Name      : lfCopyCriteria
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 01/06/2016 
*! Purpose   : Copy OG criteria
*!*************************************************************
FUNCTION lfCopyCriteria
ACOPY(loogscroll.laogFxflt,laOgSelect)  

*!*************************************************************
*! Name      : lfConvertToCursor
*: Developer : Mariam Mazhar (MMT)
*: Date      : 01/06/2016
*! Purpose   : Convert a list of values into a cusrsor
*!*************************************************************
*!B608130
FUNCTION lfConvertToCursor
PARAMETERS lcStrToConv,lcFieldName ,lcNewFile
lcCursorTemp = lcNewFile &&Cursor Hold Selected values
DIMENSION laTempacstru[1,4]
laTempacstru[1,1] = lcFieldName

DO CASE

CASE   ALLTRIM(lcFieldName) = 'SEASON'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6
  laTempacstru[1,4]= 0

CASE   ALLTRIM(lcFieldName) = 'CDIVISION'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6
  laTempacstru[1,4]= 0

CASE   ALLTRIM(lcFieldName) = 'CSTYGRP'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6
  laTempacstru[1,4]= 0

*: C201759,2 Add Royalty option, Don't Export style has no UPC, and Export Zero Stock Style[T20151217.0010][Start]
CASE ALLTRIM(lcFieldName) = 'ROYALTY'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6
  laTempacstru[1,4]= 0
*: C201759,2 Add Royalty option, Don't Export style has no UPC, and Export Zero Stock Style[T20151217.0010][End]
  
ENDCASE

 = gfCrtTmp(lcCursorTemp ,@laTempacstru,lcFieldName ,lcCursorTemp ,.T.)
lcValuesToConvert = lcStrToConv
IF !EMPTY(lcValuesToConvert)
  lnStart=1
  lnEnd=AT('|',lcValuesToConvert )
  DO WHILE lnEnd <> 0
    SELECT(lcCursorTemp )
    APPEND BLANK
    REPLACE &lcFieldName  WITH SUBSTR(lcValuesToConvert,lnStart,lnEnd-1)
    lcValuesToConvert = STUFF(lcValuesToConvert ,lnStart,lnEnd,"")
    lnEnd=AT('|',lcValuesToConvert )
  ENDDO
  IF lnEnd = 0
    SELECT(lcCursorTemp )
    APPEND BLANK
    REPLACE &lcFieldName  WITH lcValuesToConvert
  ENDIF
ENDIF
RETURN .T.
*!*************************************************************
*! Name      : lfwrepwhen
*: Developer : Mariam Mazhar (MMT)
*: Date      : 01/06/2016
*! Purpose   : When of OG 
*!*************************************************************
FUNCTION lfwrepwhen
*!*************************************************************
*! Name      : lfExport
*: Developer : Mariam Mazhar (MMT)
*: Date      : 01/06/2016
*! Purpose   : Export function
*!*************************************************************
FUNCTION lfExport
PARAMETERS loFormSet
SELECT (loFormSet.lcTempFileHdr) 
LOCATE FOR llSel
IF !FOUND()
  =gfModalgen("TRM00000B00000","DIALOG",.F.,.F.,"No selected lines to update, cannot proceed.")
  RETURN .F.
ENDIF
WAIT WINDOW 'Exporting file to Excel.....' NOWAIT 
lcExcelFile = "INVAVL"+DTOS(oAriaApplication.SystemDate)
lcFilePath = ADDBS(STRTRAN(UPPER(ALLTRIM(oAriaApplication.EDIPATH)),"ARIA4XP",'ARIA3EDI'))+"OUTBOX\"+lcExcelFile+".XLS"
COPY TO (lcFilePath) TYPE XLS FIELDS UPC,Quantity,WareHouseID,Warehouse,Date FOR llsel
IF FILE(lcFilePath) 
  =gfModalgen("INM00000B00000","DIALOG",.F.,.F.,"File: "+lcFilePath+" has been created successfully")
*!*	  IF lfUploadFile(lcExcelFile)
*!*	    =gfModalGen('INM00000B00000',.F.,.F.,.F.,"File has been uploaded successfully")
*!*	    WAIT CLEAR 
*!*	  ELSE
*!*	    =gfModalGen('INM00000B00000',.F.,.F.,.F.,"File could not be uploaded")
*!*	    WAIT CLEAR 
*!*	    RETURN .F.
*!*	  ENDIF
ENDIF

*Ftp functions
*!*************************************************************
*! Name      : lfUploadFile
*: Developer : Mariam Mazhar (MMT)
*: Date      : 01/06/2016
*! Purpose   : Upload function
*!*************************************************************
FUNCTION lfUploadFile
PARAMETERS lcFileName

#DEFINE GENERIC_READ 2147483648 && &H80000000
#DEFINE GENERIC_WRITE 1073741824 && &H40000000

Local m.ftpServer, m.ftpUserName, m.ftpUserPass

PUBLIC hOpen, hFtpSession
DECLARE INTEGER InternetOpen IN wininet.dll;
STRING sAgent,;
INTEGER lAccessType,;
STRING sProxyName,;
STRING sProxyBypass,;
STRING lFlags

DECLARE INTEGER InternetCloseHandle IN wininet.dll;
INTEGER hInet

DECLARE INTEGER InternetConnect IN wininet.dll;
INTEGER hInternetSession,;
STRING sServerName,;
INTEGER nServerPort,;
STRING sUsername,;
STRING sPassword,;
INTEGER lService,;
INTEGER lFlags,;
INTEGER lContext

DECLARE INTEGER FtpOpenFile IN wininet.dll;
INTEGER hFtpSession,;
STRING sFileName,;
INTEGER lAccess,;
INTEGER lFlags,;
INTEGER lContext

DECLARE INTEGER InternetWriteFile IN wininet.dll;
INTEGER hFile,;
STRING @ sBuffer,;
INTEGER lNumBytesToWrite,;
INTEGER @ dwNumberOfBytesWritten
#DEFINE FTP_TRANSFER_TYPE_BINARY       2

*!*	m.ftpServer="klingon"
*!*	m.ftpServer="172.10.1.3"
*!*	m.ftpUserName="e2userdp"
*!*	m.ftpUserPass="e2user"
m.ftpUserName = "aria\mariam.m" && ftp user Name
m.ftpUserPass = "aria_123456789" &&Ftp Password
m.ftpServer   = "174.143.166.180"
lcTargetPath = "/ariaftp/"
*!*	lcFirstFileToFind = ""
*!*	IF connect2ftp (m.ftpServer, m.ftpUserName, m.ftpUserPass)
*!*	  lcFirstFile = lcFileName
*!*	  llFoundFile = .F.
*!*	  lnCnt = 0
*!*	  DO WHILE !llFoundFile 
*!*	    lcCnt = STR(lnCnt,2)
*!*	    lcCnt = PADL(ALLTRIM(lcCnt),2,"0")
*!*	    lcFirstFileToFind = lcFirstFile+"-"+lcCnt 
*!*	    hFile = FtpOpenFile(hFtpSession, lcTargetPath +lcFirstFileToFind+".XLS",;
*!*	                GENERIC_READ, FTP_TRANSFER_TYPE_BINARY, 0)

*!*	    IF hFile =< 0
*!*	      llFoundFile =.T.
*!*	      EXIT 
*!*	    ELSE
*!*	      llFoundFile = .F.
*!*	    ENDIF
*!*	    lnCnt = lnCnt + 1 
*!*	  ENDDO
*!*	  = InternetCloseHandle (hFile)
*!*	ELSE
*!*	  RETURN .F.  
*!*	ENDIF  

IF connect2ftp (m.ftpServer, m.ftpUserName, m.ftpUserPass)
  lcSourcePath = oAriaApplication.WorkDir && local folder   && remote folder (ftp server)
  *lnFiles = ADIR (arr, lcRpOFile)

*!*	  FOR lnCnt=1 TO lnFiles
    lcSource = lcSourcePath + lcFileName+".XLS"
    lcTarget = lcTargetPath +lcFileName+".XLS"
    *lcTargetPath + LOWER (arr [lnCnt, 1])
    * lcSource + " -> " + lcTarget
    WAIT WINDOW 'Uploading file.....' NOWAIT 
    local2ftp (hFtpSession, lcSource, lcTarget)
    WAIT CLEAR
*!*	  ENDFOR
  = InternetCloseHandle (hFtpSession)
  = InternetCloseHandle (hOpen)
ELSE
  RETURN .F.  
ENDIF
RETURN .T.

*!*************************************************************
*! Name      : connect2ftp
*: Developer : Mariam Mazhar (MMT)
*: Date      : 01/06/2016
*! Purpose   : connect to ftp
*!*************************************************************
FUNCTION connect2ftp (strHost, strUser, strPwd)
#DEFINE INTERNET_FLAG_PASSIVE 0x08000000
** Open the access
sProxyName = CHR(0) &&... no proxy
sProxyBypass = CHR(0) &&... nothing to bypass
lFlags = 0 &&... no flags used
sAgent = 'vfp'
hOpen = InternetOpen (sAgent, 1,sProxyName, sProxyBypass, lFlags)

IF hOpen = 0
  RETURN .F.
ENDIF

** Connect to FTP.
hFtpSession = InternetConnect (hOpen, strHost, 0, strUser, strPwd, 1, INTERNET_FLAG_PASSIVE,0)


IF hFtpSession = 0
** Close
= InternetCloseHandle (hOpen)
*? "FTP " + strHost + " not ready"
RETURN .F.
ELSE
*? "Connected to " + strHost + " as: [" + strUser + ", *****]"
ENDIF
RETURN .T.

*!*************************************************************
*! Name      : local2ftp 
*: Developer : Mariam Mazhar (MMT)
*: Date      : 01/06/2016
*! Purpose   : Copy to ftp
*!*************************************************************
FUNCTION local2ftp (hConnect, lcSource, lcTarget)
** Upload local file to ftp server
hSource = FOPEN (lcSource)
IF (hSource = -1)
RETURN -1
ENDIF

** New file in ftp server
hTarget = FtpOpenFile(hConnect, lcTarget, GENERIC_WRITE, 2, 0)
IF hTarget = 0
= FCLOSE (hSource)
RETURN -2
ENDIF

lnBytesWritten = 0
lnChunkSize = 512 && 128, 512
DO WHILE Not FEOF(hSource)
lcBuffer = FREAD (hSource, lnChunkSize)
lnLength = Len(lcBuffer)
IF lnLength > 0
IF InternetWriteFile (hTarget, @lcBuffer, lnLength, @lnLength) =1
lnBytesWritten = lnBytesWritten + lnLength
*? lnBytesWritten
** Show Progress
ELSE
EXIT
ENDIF
ELSE
EXIT
ENDIF
ENDDO

= InternetCloseHandle (hTarget)
= FCLOSE (hSource)

RETURN lnBytesWritten 

*!*************************************************************
*! Name      : lfGetWorkInProcess
*: Developer : Mariam Mazhar (MMT)
*: Date      : 01/21/2016
*! Purpose   : Get Styles WIP
*!*************************************************************
FUNCTION lfGetWorkInProcess
PARAMETERS lcWareHouseCode,lcSelectedStyle
lcSelectStatment = "Select poshdr.complete ,SUM(CASE "+; 
    " WHEN trancd in ('1') THEN Qty1 "+;
    " WHEN trancd in ('2','4','5')  THEN -1 * qty1 End) as Qty1,"+;
    " SUM(CASE "+;
    " WHEN trancd in ('1') THEN Qty2 "+;
    " WHEN trancd in ('2','4','5')  THEN -1 * qty2 End) as Qty2,"+;
    " SUM(CASE "+;
    " WHEN trancd in ('1') THEN Qty3 "+;
    " WHEN trancd in ('2','4','5')  THEN -1 * qty3 End) as Qty3,"+;
    " SUM(CASE "+;
    " WHEN trancd in ('1') THEN Qty4"+;
    " WHEN trancd in ('2','4','5')  THEN -1 * qty4 End) as Qty4,"+;
    " SUM(CASE "+;
    " WHEN trancd in ('1') THEN Qty5"+;
    " WHEN trancd in ('2','4','5')  THEN -1 * qty5 End) as Qty5,"+;
    " SUM(CASE "+;
    " WHEN trancd in ('1') THEN Qty6"+;
    " WHEN trancd in ('2','4','5')  THEN -1 * qty6 End) as Qty6,"+;
    " SUM(CASE "+;
    " WHEN trancd in ('1') THEN Qty7"+;
    " WHEN trancd in ('2','4','5')  THEN -1 * qty7 End) as Qty7,"+;
    " SUM(CASE "+;
    " WHEN trancd in ('1') THEN Qty8"+;
    " WHEN trancd in ('2','4','5')  THEN -1 * qty8 End) as Qty8 "+;
    " from posln inner join poshdr on POSHDR.cstytype +POSHDR.cbusdocu+ poshdr.po = posln.cstytype +posln.cbusdocu+ posln.po "+;
    " where (posln.cStytype ='U' or posln.cstytype ='P')"+;
    " and posln.cbusdocu ='P' and poshdr.status in ('H','O') and posln.trancd <>'3' "+;
    " and POSLN.Style ='"+STRTRAN(lcSelectedStyle,"'","''")+"' and POSLN.cWareCode ='"+STRTRAN(lcWareHouseCode,"'","''")+"' group by poshdr.complete"

lnResult=oAriaApplication.remotecompanydata.SqlRun(lcSelectStatment ,'WIPTMP', ;
                 "POSLN",oariaapplication.activecompanyconstr,3,'SAVE',SET("Datasession"))
IF lnResult<>1
   oAriaApplication.remotecompanydata.CheckRetResult("sqlrun",lnResult,.T.)  
   WAIT WINDOW This.lccursorview+'.SqlRun()'
   llContinue = .F.
   EXIT 
ELSE
  SELECT 'WIPTMP'
  LOCATE 
  IF !EOF()
    SCAN FOR Qty1 > 0 OR Qty2 > 0 OR Qty3 > 0 OR Qty4 > 0 OR Qty5 > 0 OR Qty6 > 0 OR Qty7 > 0 OR Qty8 > 0
      FOR lnSCnt = 1 To Scale.cnt
        lcSCnt = STR(lnSCnt ,1)
        IF WIPTMP.QTY&lcSCnt. > 0
          *: C201759,2 Add Royalty option, Don't Export style has no UPC, and Export Zero Stock Style[T20151217.0010][Start]
          *=gfSeek(lcSelectedStyle+lcSCnt ,'STYLEUPC','STYLEUPC')
          IF gfSeek(lcSelectedStyle+lcSCnt ,'STYLEUPC','STYLEUPC')
          *: C201759,2 Add Royalty option, Don't Export style has no UPC, and Export Zero Stock Style[T20151217.0010][End]
            *: C201759,3 MMT 02/07/2016 Change the date field from date to string[T20151217.0010][Start]
*!*	            INSERT INTO (loFormSet.lcTempFileHdr) (llSel,WarehouseID,Style,SIZE,Quantity,UPC,Warehouse,Date) VALUES ;
*!*	              (.T.,lcWareHouseCode,lcSelectedStyle,Scale.SZ&lcSCnt.,WIPTMP.QTY&lcSCnt.,STYLEUPC.CUPCNUM1+STYLEUPC.CUPCNUM2+STYLEUPC.CUPCNUM3,WAREHOUS.cdesc,WIPTMP.Complete)
            INSERT INTO (loFormSet.lcTempFileHdr) (llSel,WarehouseID,Style,SIZE,Quantity,UPC,Warehouse,Date) VALUES ;
              (.T.,lcWareHouseCode,lcSelectedStyle,Scale.SZ&lcSCnt.,WIPTMP.QTY&lcSCnt.,STYLEUPC.CUPCNUM1+STYLEUPC.CUPCNUM2+STYLEUPC.CUPCNUM3,WAREHOUS.cdesc,DTOC(WIPTMP.Complete))
            *: C201759,3 MMT 02/07/2016 Change the date field from date to string[T20151217.0010][End]
          *: C201759,2 Add Royalty option, Don't Export style has no UPC, and Export Zero Stock Style[T20151217.0010][Start]
          ENDIF
          *: C201759,2 Add Royalty option, Don't Export style has no UPC, and Export Zero Stock Style[T20151217.0010][End]
        ENDIF
      ENDFOR  
    ENDSCAN
  ENDIF  
ENDIF  



  