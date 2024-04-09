*:***********************************************************************************************
*:  Program Name : MFINTRNS.PRG
*:         System: ARIA 2.7
*:         Module: Manufactoring (MF).
*:  Program desc.: Cutting Ticket In-Transit
*:         Author: Hend Ghanem (HBG)
*:      Copyright (c) 
*:  Last modified: 13/02/2001
*:***********************************************************************************************
*: For Custom# C102184
*:***********************************************************************************************
*Modifications:
*B604463,1 HGB 05/07/2001 Fix the bug of pressing twice in the save button.
*B604463,1                And changing the screen title.
*B604631,1 KHM 07/03/2001 Fix the bug of file access denied.
*B605913,1 AMH 05/05/2001 Fix the bug of cancel shipment option disabled.
*:***********************************************************************************************

DIMENSION laShpVia[1,2]
DIMENSION laShpType[3,2]
laShpType[1,1] = 'Regulars '
laShpType[1,2] = 'R'
laShpType[2,1] = 'Seconds  '
laShpType[2,2] = 'S'
laShpType[3,1] = 'Balances '
laShpType[3,2] = 'B'

STORE "" TO laCodInfo,laShpVia,lcCodeType

EXTERNAL ARRAY laData , laKeyField
DECLARE lafoldwinds[2,2],laScrMode[4],laKeyField[1,4]

laKeyField[1,1] = 'laData[1]'
laKeyField[1,2] =.T.
laKeyField[1,3] = 'TRANSHDR'
laKeyField[1,4] = 1

STORE ' ' TO lcWinCh0,lcWinCh1,lcWinCh2,lcWinC21,lcWinC22,lcWinC23,lcWinC24,lcWinCh3
STORE ' ' TO lcScFields,lcfolder,lcfoldprnt,lcStatus,ldOldDate,lcBrTtl1,lcStyHdr,lcStylePct
STORE ' ' TO lcSz1,lcSz2,lcSz3,lcSz4,lcSz5,lcSz6,lcSz7,lcSz8,lcStyDesc,lcVenNam
STORE ''  TO lcBrD1fld, lcTranLine, lcTranLine
STORE 0   TO lnStyleWid
STORE 1   TO lnActFolder,lnMarker,lnShptype
STORE .F. TO llBrowse , ll2Brow  
STORE .T. TO llAlowNew,llAddLine,llCUpdate,llRecalTot,llShow,llNoShow
lcEscTrap  = ON("KEY","ESC")
lcWindTitl = ''

IF !gfSetup()
  RETURN
ENDIF

*--Initialize the Folders array.
lnFolderCEnd = 102.50
lnFolderREnd =   2.00
lnNoFld      =   2
lcwfoldchng  = '=lfActFolder()'  && function to control shows after change the folder
lcfoldpush   = 'pbFolders'       && push button name for the next folder
laDefProc[9]  = .F.              && Disable the control panel save proc.  (lpSavScr)

*B605913,1 AMH Add local cancel proc. [Start]
laDefProc[7]  = .F.              && Disable the control panel delete proc.(lpDelScr)
*B605913,1 AMH [End]

=gfOpenFile(gcDataDir+'TransHdr',gcDataDir+'TransHdr','SH')
=gfOpenFile(gcDataDir+'CUTTKTL',gcDataDir+'CUTTKTLI','SH')
=gfOpenFile(gcDataDir+'CUTTKTH',gcDataDir+'CUTTKTH','SH')
=gfOpenFile(gcDataDir+'APVENDOR',gcDataDir+'VENCODE','SH')
=gfOpenFile(gcDataDir+'CODES',gcDataDir+'Ccode_no','SH')
=gfOpenFile(gcDataDir+'STYLE',gcDataDir+'STYLE','SH')
=gfOpenFile(gcDataDir+'SCALE',gcDataDir+'SCALE','SH')

SELECT TransHdr
SET RELATION TO ccutven+cship_type+ccutinv INTO CUTTKTL

*-- Define the variable of the system
lcCancel  = gcBmpHome + "trash.bmp"
lcUncan   = gcBmpHome + "untrash.bmp"
lcPromp   = lcCancel
lcDelMesag= 'cancel'

IF !WEXIST(gcBaseWind)
  lcStylePct = gfItemMask("PI")
  lnStyleWid = LEN(lcStylePct)
  lcStyHdr   = gfItemMask('HI')
  lcOldValue = SPACE(0)
  lcBrTtl1   = 'Detail Lines'

  lcScFields ='CCUTVEN   ,CSHIP_TYPE,CCUTINV ,STATUS   ,Entered ,cartons  ,CCONTANR,'+;
              'ETA       ,cVessel   ,Reference  ,DCUSTOM   ,DTRDATE,'+;
              'DWAREHOUS ,SHIPVIA   ,NOTE,TOTQTY'    
             
  SELECT TransHdr
  SCATTER FIELDS &lcScFields MEMO TO laData BLANK           
  
  *--Program screen windows.
  lcWinCh0    = gfTempName()
  lcWinCh1    = gfTempName()
  lcWinCh2    = gfTempName()
  lcWinC21    = gfTempName()
  lcWinC22    = gfTempName()
  lcWinC23    = gfTempName()
  lcWinC24    = gfTempName()
  lcfolder    = gfTempName()        && Folder Window Name
  lcfoldprnt  = gcBaseWind          && window parent name for the folder
  lnActFolder = 1                   && active folder

  lafoldwinds[1,1] = 'Header'
  lafoldwinds[1,2] = lcWinCh1
  lafoldwinds[2,1] = 'Details'
  lafoldwinds[2,2] = lcWinCh2
  
  lcTranLine = gfTempName()
  lcLotLines = gfTempName()
    
  *-- Array to Hold the Code information For ShipVia [Begin]  
  DIMENSION laCodInfo[1,10]
  laCodInfo[1,1]  = "SHIPVIA"
  laCodInfo[1,2]  = "laShpVia"
  laCodInfo[1,3]  = "lnShpVia"
  laCodInfo[1,4]  = ""
  laCodInfo[1,5]  = .F.    
  laCodInfo[1,6]  = .F.     
  laCodInfo[1,7]  = "TRANSHDR"     
  laCodInfo[1,8]  = "TRANSHDR"     
  laCodInfo[1,9]  = "laData[1]"
  laCodInfo[1,10] = "ShipVia" 
  
  *--Create temp files.    
  SELECT CUTTKTL
  =AFIELDS(laFileStru)
  lnI = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnI,4]
  laFileStru[lnI,1] = 'Desc'
  laFileStru[lnI,2] = 'C'
  laFileStru[lnI,3] = 20
  laFileStru[lnI,4] = 0
  
  lnI = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnI,4]
  laFileStru[lnI,1] = 'Scale'
  laFileStru[lnI,2] = 'C'
  laFileStru[lnI,3] = 3
  laFileStru[lnI,4] = 0
  
  lnI = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnI,4]
  laFileStru[lnI,1] = 'Check'
  laFileStru[lnI,2] = 'C'
  laFileStru[lnI,3] = 1
  laFileStru[lnI,4] = 0
  DIMENSION laIndx[1,2]
  laIndx[1,1] = "ccutven+cship_type+ccutinv+style+STR(nlot_no,4)+trancd+STR(RECNO(),7)"
  laIndx[1,2] = lcTranLine
  lcOldExcl = SET('EXCL')
  SET EXCLUSIVE ON
  =gfCrtTmp(lcTranLine,@laFileStru,@laIndx)  
  
  lnI = 1
  DIMENSION laFileStru[lnI,4]
  laFileStru[lnI,1] = 'CutTkt'
  laFileStru[lnI,2] = 'C'
  laFileStru[lnI,3] = 6
  laFileStru[lnI,4] = 0
  
  lnI = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnI,4]
  laFileStru[lnI,1] = 'Style'
  laFileStru[lnI,2] = 'C'
  laFileStru[lnI,3] = 19
  laFileStru[lnI,4] = 0
  
  lnI = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnI,4]
  laFileStru[lnI,1] = 'nLot_no'
  laFileStru[lnI,2] = 'N'
  laFileStru[lnI,3] = 4
  laFileStru[lnI,4] = 0
  
  lnI = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnI,4]
  laFileStru[lnI,1] = 'Qty1'
  laFileStru[lnI,2] = 'N'
  laFileStru[lnI,3] = 6
  laFileStru[lnI,4] = 0
  
  lnI = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnI,4]
  laFileStru[lnI,1] = 'Qty2'
  laFileStru[lnI,2] = 'N'
  laFileStru[lnI,3] = 6
  laFileStru[lnI,4] = 0
  
  lnI = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnI,4]
  laFileStru[lnI,1] = 'Qty3'
  laFileStru[lnI,2] = 'N'
  laFileStru[lnI,3] = 6
  laFileStru[lnI,4] = 0
  
  lnI = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnI,4]
  laFileStru[lnI,1] = 'Qty4'
  laFileStru[lnI,2] = 'N'
  laFileStru[lnI,3] = 6
  laFileStru[lnI,4] = 0
  
  lnI = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnI,4]
  laFileStru[lnI,1] = 'Qty5'
  laFileStru[lnI,2] = 'N'
  laFileStru[lnI,3] = 6
  laFileStru[lnI,4] = 0
  
  lnI = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnI,4]
  laFileStru[lnI,1] = 'Qty6'
  laFileStru[lnI,2] = 'N'
  laFileStru[lnI,3] = 6
  laFileStru[lnI,4] = 0
  
  lnI = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnI,4]
  laFileStru[lnI,1] = 'Qty7'
  laFileStru[lnI,2] = 'N'
  laFileStru[lnI,3] = 6
  laFileStru[lnI,4] = 0
  
  lnI = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnI,4]
  laFileStru[lnI,1] = 'Qty8'
  laFileStru[lnI,2] = 'N'
  laFileStru[lnI,3] = 6
  laFileStru[lnI,4] = 0
  
  lnI = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnI,4]
  laFileStru[lnI,1] = 'TotQty'
  laFileStru[lnI,2] = 'N'
  laFileStru[lnI,3] = 7
  laFileStru[lnI,4] = 0
  
  DIMENSION laIndx[1,2]
  laIndx[1,1] = "style+CutTkt+STR(nlot_no,4)"
  laIndx[1,2] = lcLotLines
  
  *B604631,1 KHM 07/03/2001 (Begin) Commenting the following line.
  *lcOldExcl = SET('EXCL')
  *B604631,1 KHM 07/03/2001 (End)
  
  =gfCrtTmp(lcLotLines,@laFileStru,@laIndx)  
  
  SET EXCLUSIVE &lcOldExcl
ENDIF

*SELECT TransHdr
SELECT (lcBaseFile)
SET ORDER TO TAG TransHdr

*B604463,1 HGB 05/07/2001 (Begin) Changing the title.
lcWindTitl = 'Cutting Ticket In-Transit'
*B604463,1 HGB 05/07/2001 
DO (gcScrDir+gcWinAppl+"\MFINTRNS.SPX")


IF glQuitting
  SELECT (lcTranLine)
  USE
  ERASE (gcWorkDir+lcTranLine+'.DBF')
  ERASE (gcWorkDir+lcTranLine+'.CDX')
ENDIF


*:*************************************************************
*! Name      : lfActFolder
*! Developer : Hend Ghanem (HBG)
*! Date      : 14/03/2001
*! Purpose   : Validating the active folder
*:*************************************************************
FUNCTION lfActFolder
IF lnActFolder = 1
  SHOW WINDOW (lcWinCh1) TOP
  ACTIVATE WINDOW (lcWinCh1)
  
  IF laScrMode[2]
    SHOW GETS WINDOW (lcWinCh1) DISABLE ONLY
  ELSE 
    IF !EMPTY(laData[1]) AND !EMPTY(laData[3])
      SELECT (lcTranLine)
      GO TOP
      laData[16] = 0
      SCAN
        laData[16] = laData[16] + &lcTranLine..TotQty
      ENDSCAN
      SHOW GETS WINDOW (lcWinCh1) ENABLE  ONLY
      SHOW GET lcStatus DISABLE
    ENDIF  
  ENDIF
  SHOW GETS WINDOW (lcWinC22) DISABLE ONLY
ELSE
  ON KEY LABEL ALT+B DO lfwBrow1  
  
  IF laScrMode[3] OR laScrMode[4]
    IF lfCkEOF(lcTranLine)
      SHOW GETS WINDOW (lcWinC22) DISABLE ONLY  
      
      IF !(laData[4] $ 'XC')
        IF !EMPTY(laData[3])
          SHOW GET PBNEW ENABLE
        ENDIF  

        IF !EOF(lcTranLine)
          SHOW GET pbRemove ENABLE 
        ELSE
          SHOW GET pbRemove DISABLE
        ENDIF

    	_CUROBJ=OBJNUM(pbNew)
      ENDIF
    ELSE
      IF !(laData[4] $ 'XC')
        SHOW GETS WINDOW (lcWinC22) ENABLE ONLY  
        SHOW GET m.TotQty DISABLE      
      ELSE
        SHOW GETS WINDOW (lcWinC22) DISABLE ONLY          
      ENDIF
    ENDIF
  ELSE
    SHOW GETS WINDOW (lcWinC22) DISABLE ONLY    
  ENDIF
  SHOW GETS WINDOW (lcWinCh1) DISABLE ONLY
  IF !lfCkEOF(lcTranLine)
    =lfwBrow1()
  ENDIF

  IF !laScrMode[1]
    SHOW GET ibTrap1 ENABLE
  ENDIF
    SHOW WINDOW (lcWinCh2) TOP
  ACTIVATE WINDOW (lcWinCh2)
ENDIF


*!*************************************************************
*! Name      : lfCkEOF
*! Developer : Hend Ghanem (HBG)
*! Date      : 14/03/2001
*! Purpose   : Function if there is lines .
*!*************************************************************
FUNCTION lfCkEOF
PARA lcCkFile
lnAlias = SELECT()
SELECT (lcCkFile)
GO TOP

lnRet = EOF()

SELECT(lnAlias)
RETURN lnRet


*!*************************************************************
*! Name      : lfActBrow1
*! Developer : Hend Ghanem (HBG)
*! Date      : 14/03/2001
*! Purpose   : Original Browse function.
*!*************************************************************
FUNCTION lfActBrow1

lcSelect = SELECT()

SELECT (lcTranLine)
GO TOP
lnMarker = RECNO()

lcBrD1fld = "cMarker =IIF(RECNO()=lnMarker ,'>',' '):H=' ':R:1:W=.F.,"+;
            "Style     :H='Style#':R:25,"+;
            "Qty1      :H='Size1':6:R,"+;
            "Qty2      :H='Size2':6:R,"+;
            "Qty3      :H='Size3':6:R,"+;
            "Qty4      :H='Size4':6:R,"+;
            "Qty5      :H='Size5':6:R,"+;
            "Qty6      :H='Size6':6:R,"+;
            "Qty7      :H='Size7':6:R,"+;
            "Qty8      :H='Size8':6:R,"+;
            "TotQty    :H='Total Qty':10:R,"+;
            "nLot_No   :H='Lot #':6:R"


BROWSE FIELDS &lcBrD1fld ;
        	    LOCK 0   ;
                NOEDIT   ;
                NOAPPEND ;
        	    NOCLEAR  ;
                NODELETE ;
                NOMENU   ;
  	            NOWAIT   ;
	            SAVE     ;
  	     TITLE (lcBrTtl1);
	    WINDOW (lcWinC21);
	 IN WINDOW (lcWinCh2);
          WHEN lfwBrow1()

SELECT (lcSelect)	



*!*************************************************************
*! Name      : lfwBrow1
*! Developer : Hend Ghanem (HBG)
*! Date      : 14/03/2001
*! Purpose   : When Browse function.
*!*************************************************************
FUNCTION lfwBrow1
glFromBrow = .T.
lnMarker = RECNO()
SHOW WINDOW (lcBrTtl1) REFRESH

SELECT (lcTranLine)

SCATTER MEMVAR MEMO

llFoundScl=SEEK('S'+&lcTranLine..Scale,'Scale')
lcSz1=IIF(llFoundScl,SCALE.SZ1,'')
lcSz2=IIF(llFoundScl,SCALE.SZ2,'')
lcSz3=IIF(llFoundScl,SCALE.SZ3,'')
lcSz4=IIF(llFoundScl,SCALE.SZ4,'')
lcSz5=IIF(llFoundScl,SCALE.SZ5,'')
lcSz6=IIF(llFoundScl,SCALE.SZ6,'')
lcSz7=IIF(llFoundScl,SCALE.SZ7,'')
lcSz8=IIF(llFoundScl,SCALE.SZ8,'')

lcStyle   = m.Style
lcStyDesc = m.Desc
lnLot     = PADL(m.nLot_no,4,' ')

IF laScrMode[4] OR laScrMode[3]
  IF !EOF(lcTranLine) 
    SHOW GET pbRemove ENABLE
  ELSE
    SHOW GET pbRemove DISABLE
  ENDIF
ENDIF  

SHOW GETS WINDOW (lcWinC22) ONLY
=lfRefresh(lcWinC22)

SHOW GET pbKey ENABLE


*:*************************************************************
*! Name      : lpShow
*! Developer : Hend Ghanem (HBG)
*! Date      : 14/03/2001
*! Purpose   : Doing whatever necessary when changing screen modes
*:*************************************************************
PROCEDURE lpShow

*B605913,1 AMH Comment the next line [Start]
*SHOW GET pbDlt DISABLE
*B605913,1 AMH [End]

DO CASE

  *** S E L E C T  M O D E ***      
  CASE laScrMode[1]
    
       
    lnLastFold  = lnActFolder
    lnActFolder = 1
    = lfChngFolder(lnActFolder)

    SHOW GET ibFolder(2) DISABLE
    lcStatus  = ' '

    SELECT (lcTranLine)
    ZAP
    =lfActBrow1()
    
    lcVenNam = ""
    SHOW GET lcVenNam 
    lnShptype = 1
    IF EMPTY(laData[1])
      SHOW GETS WINDOW (lcWinCh1) DISABLE ONLY
      =lfRefresh(lcWinCh1)
      SHOW GET lnShpType DISABLE
      SHOW GET laData[3] DISABLE
    ELSE
      SHOW GET lnShpType ENABLE
      SHOW GET laData[3] ENABLE
    ENDIF  
    
    SELECT TransHdr
    llNewShp = .F.
    
    SHOW GET pbDlt,1 PROMPT lcPromp DISABLE
    laCtrStat[8]  =  "DISABLE"
    
    SHOW GETS WINDOW (lcWinC22) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh1) DISABLE ONLY
    SHOW GET ibTrap1 DISABLE
    SHOW GET pbUsrFields DISABLE

    lcCodeType = "N"

    
  *** V I E W   M O D E ***      
  CASE laScrMode[2]
    
    SHOW GETS WINDOW (lcWinC22) DISABLE ONLY    
    
    *--- Refresh the Browse with the accepted data
    SELECT (lcTranLine)
    ZAP
    
    SELECT CUTTKTL
    lcOrder = TAG()
    GO TOP
    SET ORDER TO Cuttktli
    IF SEEK(laData[1]+laData[2]+laData[3])
      SCAN REST WHILE ccutven+cship_type+ccutinv+style+STR(nlot_no,4)+trancd+;
                      STR(RECNO(),7) = laData[1]+laData[2]+laData[3] 
        IF trancd = '5' OR trancd = '6'
          SCATTER MEMVAR MEMO
          INSERT INTO (lcTranLine) FROM MEMVAR
          =SEEK(&lcTranLine..Style,'STYLE')
          REPLACE &lcTranLine..Scale WITH STYLE.Scale,;
                  &lcTranLine..Desc  WITH STYLE.Desc ,;
                  &lcTranLine..Check WITH 'O'
        ENDIF  
      ENDSCAN
    ENDIF
    SET ORDER TO lcOrder 
    
    
    DO CASE
      CASE laData[2] = 'R'
        lnShpType = 1
      CASE laData[2] = 'S'
        lnShpType = 2
      CASE laData[2] = 'B'
        lnShpType = 3
    ENDCASE
    
    SHOW GET lnShpType DISABLE
    
    SELECT (lcTranLine)
    GO TOP

    =lfGetInfo(@lcStatus)
    =lfActBrow1()
    llNewShp = .F.
    IF lcStatus <> 'OPEN'
      SHOW GET pbDlt,1 PROMPT lcPromp DISABLE
      laCtrStat[8]  =  "DISABLE"
    ELSE
      SHOW GET pbDlt,1 PROMPT lcPromp ENABLE
      laCtrStat[8]  =  "ENABLE"
    ENDIF
    
    SHOW GET pbUsrFields DISABLE
    SHOW GET ibTrap1 DISABLE
    SHOW GET laData[16]

    SHOW GET ibFolder(2) ENABLE

    lcCodeType = "T"
 
  *** E D I T   M O D E ***      
  CASE laScrMode[3]
    STORE .T. TO llCUpdate,llUpdated
    STORE .F. TO llCSave,llShow

    =lfActBrow1()
    
    SHOW GET pbUsrFields DISABLE
    
    *B605913,1 AMH Privent edit for completed and canceled invoices [Start]
    *IF lcStatus = 'COMPLETE'
    IF lcStatus <> 'OPEN'
      *=gfModalGen('TRM00000B00000','DIALOG',;
                  .F.,.F.,'This invoice has already been completed. Cannot proceed.')
      =gfModalGen('TRM00000B00000','DIALOG',.F.,.F.,;
                  'This invoice has already been '+ALLTRIM(LOWER(lcStatus))+'. Cannot proceed.')
    *B605913,1 AMH [End]
    
      STORE .F. TO laScrMode  
      laScrMode[2] = .T.
      SHOW GETS
    ELSE
      SHOW GETS WINDOW (lcWinCh1) ENABLE ONLY
      SHOW GET ibFolder(2) ENABLE
      _CUROBJ = OBJNUM(laData[5])
      =lfChngFolder(lnActFolder)
      llNewShp = .F.
      SHOW GET lcStyle   DISABLE
      SHOW GET ibStyle   DISABLE
      SHOW GET lcStyDesc DISABLE
      SHOW GET lnLot     DISABLE            
      SHOW GET ibLot     DISABLE                  
      SHOW GET ibTrap1   DISABLE
      SHOW GET lnLot     DISABLE
      SHOW GET m.TotQty  DISABLE
      SHOW GET pbNew    ENABLE
      SHOW GET pbRemove ENABLE
      =lfRefresh(lcWinC22)
      lcCodeType = "T"
    ENDIF  
    
        
  *** A D D   M O D E ***      
  CASE laScrMode[4]
  
    STORE .T. TO llCUpdate,llUpdated
    STORE .F. TO llCSave,llShow
   
 
    SHOW GET pbUsrFields DISABLE
    SHOW GET m.TotQty DISABLE
    SHOW GET lnLot    DISABLE

    IF EMPTY(laData[1])
     
      llNewShp = .T.
      lnShptype  = 1 
      lcVenNam  = ""
      SHOW GET laData[1] ENABLE 
      SHOW GET ibFactor ENABLE 
      SHOW GET lnShpType DISABLE
      SHOW GET laData[3] DISABLE
      SHOW GET lcVenNam  Enable
      _CUROBJ = OBJNUM(laData[1])
      
      lnLastFold  = lnActFolder
      lnActFolder = 1
      =lfChngFolder(lnActFolder)
      SHOW GET ibFolder(1) ENABLE
      SELECT (lcTranLine)
      ZAP
      =lfActBrow1()
      SCATTER MEMVAR BLANK
      STORE "" TO lcStyle, lcStyDesc, lnLot
      SELECT TransHdr
      SHOW GET pbDlt,1 PROMPT lcPromp DISABLE
      laCtrStat[8]  =  "DISABLE"
      SHOW GETS WINDOW (lcWinC22) DISABLE ONLY
      SHOW GETS WINDOW (lcWinCh1) DISABLE ONLY

      SHOW GET pbNew    DISABLE 
      SHOW GET pbRemove DISABLE 
      SHOW GET ibTrap1  DISABLE

    ELSE
      SHOW GET ibFolder(2) ENABLE
      SHOW GET ibFactor DISABLE
      lcStatus   = 'OPEN'
      laData[4]  = 'O'
      laData[5]  = gdSysDate
      laData[8]  = gdSysDate + 14
      laData[6]  = 1
      laData[16] = 0

      SHOW GET laData[5]
      SHOW GET laData[6]
      SHOW GET laData[8]
      SHOW GET laData[16]
      SHOW GET lcStatus

      
      =lfRefresh(lcWinCh1)
      
      SHOW GET pbNew    ENABLE
      SHOW GET pbRemove ENABLE
      
      IF EMPTY(lnLot)
        SELECT (lcTranLine)
        ZAP
        =lfActBrow1()
      ENDIF  
      
    ENDIF
    
    =lfChngFolder(lnActFolder)
    SHOW GET ibTrap1 DISABLE

    lcCodeType = "D"
ENDCASE

 laData[2]=laShpType[lnShpType,2]
 = gfwCodePop ( @laCodInfo, "SHIPVIA"   ,lcCodeType )
 laData[14]=laShpVia[lnShpVia,2]


=LFREFRESH(lcWinCh0)


*:*************************************************************
*! Name      : lfGetInfo
*! Developer : Hend Ghanem (HBG)
*! Date      : 14/03/2001
*! Purpose   : Getting the necessary information after entering 
*!             a valid shipment number
*:*************************************************************
FUNCTION lfGetInfo
PARAMETER lcStatus

SELECT TransHdr
SCATTER FIELDS &lcScFields MEMO TO laData
DO CASE
  CASE laData[4] = 'O'
    lcStatus = 'OPEN'
  CASE laData[4] = 'C'
    lcStatus = 'COMPLETE'
  CASE laData[4] = 'X'
    lcStatus = 'CANCELED'
ENDCASE

*!*************************************************************
*! Name      : lfwMajor
*! Developer : Hend Ghanem (HBG)
*! Date      : 14/03/2001
*! Purpose   : When function to get the current style field.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*
FUNCTION lfwMajor

lcOldValue = lcStyle
RETURN

*-- end of lfwMajor.

*:*************************************************************
*! Name      : lfTrap
*! Developer : Hend Ghanem (HBG)
*! Date      : 14/03/2001
*! Purpose   : Traping necessary keys
*:*************************************************************
FUNCTION lfTrap
IF WONTOP() = lcBrTtl1
  ON KEY LABEL TAB        DO lfTab
  ON KEY LABEL BACKTAB    DO lfShiftTab
  ON KEY LABEL ESC        DO gfCPClose
  ON KEY LABEL Ctrl+ENTER  lnDummy = 1
  ON KEY LABEL Ctrl+HOME   lnDummy = 1
  ON KEY LABEL Ctrl+W      lnDummy = 1
  ON KEY LABEL Ctrl+END    lnDummy = 1
  glFromBrow = .T.
ENDIF  


*:*************************************************************
*! Name      : lfUnTrap
*! Developer : Hend Ghanem (HBG)
*! Date      : 14/03/2001
*! Purpose   : Untrap the trapped keys
*:*************************************************************
FUNCTION lfUnTrap

*B605913,1 AMH Comment the next line [Start]
*SHOW GET pbDlt DISABLE
*B605913,1 AMH [End]

IF WONTOP() # lcBrTtl1
  ON KEY LABEL TAB
  ON KEY LABEL BACKTAB
  ON KEY LABEL Ctrl+ENTER
  ON KEY LABEL Ctrl+HOME
  ON KEY LABEL Ctrl+END
  ON KEY LABEL Ctrl+W
  ON KEY LABEL ESC &lcEscTrap
  glFromBrow = .F.
ENDIF  

*:*************************************************************
*! Name      : lfTab
*! Developer : Hend Ghanem (HBG)
*! Date      : 14/03/2001
*! Purpose   : Traping the TAB key
*:*************************************************************
FUNCTION lfTab

ACTIVATE WINDOW (lcWinC22)
_CUROBJ=OBJNUM(lcStyle)

RETURN


*:*************************************************************
*! Name      : lfShiftTab
*! Developer : Hend Ghanem (HBG)
*! Date      : 14/03/2001
*! Purpose   : Traping the SHIFT+TAB key
*:*************************************************************
FUNCTION lfShiftTab

ACTIVATE WINDOW (lcfolder)
_CUROBJ = OBJNUM(ibFolder(2))

RETURN


*!**************************************************************************
*!
*!      Function: lfvData_1
*!
*!**************************************************************************
*
FUNCTION lfvData_1


lcObj = PADR(ALLTRIM(laData[1]),8)

SELECT APVENDOR
SET ORDER TO TAG VenCode 
*--IF Statment to check if we are going to Browse
IF (EMPTY(lcObj) AND llBrowse) .OR. '?' $ lcObj .OR. (!EMPTY(lcObj) AND !SEEK(lcObj , 'APVENDOR'))
  =gfApVnBrow(@lcObj)
  llBrowse  = .F.
  IF !EMPTY(lcObj) 
    laData[1] = lcObj      && Update the field
  ELSE
    laData[1] = SPACE(8)
    _CUROBJ = OBJNUM(laData[1])
  ENDIF
ENDIF

IF !EMPTY(laData[1])
  =SEEK(laData[1] , 'APVENDOR')
  lcVenNam  = APVENDOR.cvencomp
  SHOW GET lnShpType ENABLE
  SHOW GET laData[3] ENABLE
  SHOW GET lcVenNam  
  _CUROBJ = OBJNUM(lnShpType)
  SHOW GETS WINDOW (lcWinCh0) ENABLE  ONLY
  SHOW GETS WINDOW (lcWinCh1) DISABLE ONLY
  SHOW GETS WINDOW (lcWinC22) DISABLE ONLY
  =lfRefresh(lcWinCh0)
ENDIF




*!*************************************************************
*! Name      : lfvInvoice
*! Developer : Hend Ghanem (HBG)
*! Date      : 14/03/2001
*! Purpose   : Validate Invoice number
*!*************************************************************
*! Calls     : lfInvBrow,gfSeekRec
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  .f.
*!*************************************************************
*! Example   :  =lfvInvoice()
*!*************************************************************
FUNCTION lfvInvoice

IF EMPTY(laData[3])
  =gfModalGen('TRM00000B00000','DIALOG',.F.,.F.,'You have to enter an invoice #.') 
  _CUROBJ=OBJNUM(laData[3])
  RETURN
ENDIF

laData[2]=laShpType[lnShpType,2]

SELECT TRANSHDR
GOTO TOP

DO CASE
  CASE laScrMode[4] AND SEEK(laData[1]+laData[2]+laData[3],'TransHdr')
    =gfModalGen('TRM00000B00000','DIALOG',.F.,.F.,'This Invoice number exists for the same factory before. Cannot proceed.')
    laData[3] = SPACE(6)
    _CUROBJ=OBJNUM(laData[3])
    RETURN
  CASE laScrMode[1] AND SEEK(laData[1]+laData[2]+laData[3],'TransHdr')
    STORE .F. TO laScrMode
    laScrMode[2] = .T.
    = lfGetData()

  CASE laScrMode[1] AND !SEEK(laData[1]+laData[2]+laData[3],'TransHdr')
   
    DIMENSION laTempData[1]
    STORE '' TO laTempData
    lcKey = laData[1]+laData[2]
    SELECT TRANSHDR
    LOCATE
    IF SEEK(lcKey)
      lcPrvField = lcBrFields
      lcBrFields = "ccutinv       :H ='Invoice #' :15:R,"+;
                   "lcShipTy=IIF(cShip_type='R','Regular',IIF(cShip_type='S','Second','Balance')) :R :H ='Ship Type' :15,"+;
                   "cCutVen       :H ='Factory' :15:R,"+;
                   "Status        :H ='Status' :10:R"
                   

      IF !AriaBrow([lcKey],"IN-TRANSIT FACTORY/INVOICE BROWSE",;
                    gnBrFSRow1,gnBrFSCol1,gnBrFSRow2,gnBrFSCol2,;
                   .F.,.F.,"cCutInv","laTempData")
        lcBrFields = lcPrvField
        laData[3] = laTempData[1]
        RETURN
      ENDIF
      lcBrFields = lcPrvField
      laData[3] = laTempData[1]
    
      STORE .F. TO laScrMode
      laScrMode[2] = .T.
      = lfGetData()
    ELSE                                     
      =gfModalGen('TRM00000B00000','DIALOG',.F.,.F.,'No invoice has been found. Cannot proceed.') 
      STORE SPACE(8) TO laData[1]
      STORE SPACE(6) TO laData[3]
      STORE .F. TO laScrMode
      laScrMode[1] = .T.
    ENDIF  
ENDCASE

SHOW GETS


*:*************************************************************
*! Name      : lfvDate
*! Developer : Hend Ghanem (HBG)
*! Date      : 14/03/2001
*! Purpose   : Validating Entered & ETA dates
*:*************************************************************
FUNCTION lfvDate
PRIVATE llInvalid,lcDatNam
llInvalid = .F.

IF EMPTY(laData[5]) .OR. EMPTY(laData[8])
  llInvalid = .T.

  lcDatNam = IIF(EMPTY(laData[5]),'entered','E.T.A.')
  *-You cannot leave E.T.A. / entered date empty.
  =gfModalGen('INM34005B00000','DIALOG',lcDatNam)
ENDIF


IF laData[5] > laData[8] AND !llInvalid
  llInvalid = .T.

  *-E.T.A. date cannot be prior to entered date.
  =gfModalGen('INM34003B00000','DIALOG','E.T.A.')
ENDIF


IF llInvalid
  lcOldVal  = VARREAD()
  &lcOldVal = ldOldDate
  _CUROBJ = _CUROBJ
ENDIF


*!*************************************************************
*! Name      : lfGetData
*! Developer : Hend Ghanem (HBG)
*! Date      : 14/03/2001
*! Purpose   : Validate the line information.
*!*************************************************************
FUNCTION lfGetData


laData[4]   = TRANSHDR.status
DO CASE
  CASE laData[4] = 'O'
    lcStatus = 'OPEN'
  CASE laData[4] = 'C'
    lcStatus = 'COMPLETE'
  CASE laData[4] = 'X'
    lcStatus = 'CANCELED'
ENDCASE
laData[5]  = TRANSHDR.entered
laData[6]  = TRANSHDR.cartons
laData[7]  = TRANSHDR.ccontanr
laData[8]  = TRANSHDR.eta
laData[9]  = TRANSHDR.cvessel
laData[10] = TRANSHDR.reference
laData[11] = TRANSHDR.dcustom
laData[12] = TRANSHDR.dtrdate
laData[13] = TRANSHDR.dwarehous
laData[15] = TRANSHDR.note
laData[16] = TRANSHDR.totqty

laShpVia[lnShpVia,2] = TRANSHDR.shipvia
laData[14] = laShpVia[lnShpVia,2]

SELECT CUTTKTL
lcOrder = TAG()
SET ORDER TO Cuttktli
IF SEEK(laData[1]+laData[2]+laData[3])
  SCAN REST WHILE ccutven+cship_type+ccutinv+style+STR(nlot_no,4)+trancd+;
                  STR(RECNO(),7) = laData[1]+laData[2]+laData[3] 
    IF trancd = '5' OR trancd = '6'
      SCATTER MEMVAR MEMO
      INSERT INTO (lcTranLine) FROM MEMVAR
      =SEEK(&lcTranLine..Style,'STYLE') 
      REPLACE &lcTranLine..Scale WITH STYLE.Scale ,;
              &lcTranLine..Desc  WITH STYLE.Desc ,;
              &lcTranLine..Check WITH 'O'
      =lfActBrow1()
    ENDIF  
  ENDSCAN
ENDIF
SET ORDER TO lcOrder 



*!*************************************************************
*! Name      : lfvNew
*! Developer : Hend Ghanem (HBG)
*! Date      : 14/03/2001
*! Purpose   : New shipment line
*!*************************************************************
FUNCTION lfvNew

SELECT (lcTranLine)
SCATTER MEMVAR BLANK
STORE ' ' TO lcStyDesc

SHOW GETS WINDOW (lcWinC22) DISABLE ONLY
STORE "" TO lcStyle, lcStyDesc, lnLot
SHOW GETS WINDOW (lcWinC22) DISABLE ONLY 

SHOW GET lcStyle  ENABLE 
SHOW GET ibStyle ENABLE 
=lfRefresh(lcWinC22)
_CUROBJ = OBJNUM(lcStyle)
RETURN


*!*************************************************************
*! Name      : lfvRemove
*! Developer : Hend Ghanem (HBG)
*! Date      : 14/03/2001
*! Purpose   : Remove shipment line.
*!*************************************************************
FUNCTION lfvRemove

IF gfModalGen('QRM34036B34001','DIALOG') = 2
  *-Are you sure you want to delete this line?
  RETURN
ENDIF


SELECT (lcTranLine)
DELETE
PACK
GO TOP
lcDispDet = IIF(EOF(),'DISABLE','ENABLE')
SCATTER MEMVAR
=lfActBrow1()
IF EOF()
  STORE "" TO lcStyDesc , lcStyle, lnLot
ENDIF
SHOW GETS WINDOW (lcWinC22) &lcDispDet ONLY
SHOW GET pbNew ENABLE 

RETURN

*!*************************************************************
*! Name      : lfvSizeQty
*! Developer : Hend Ghanem (HBG)
*! Date      : 14/03/2001
*! Purpose   : Validate Style size quantity
*!*************************************************************
FUNCTION lfvSizeQty
PARAMETERS lnSize

lcSz = STR(lnSize,1)

IF m.Qty&lcSz<0
  *-Cannot accept -ve quantity.
  =gfModalGen('TRM34051B34000','DIALOG')
  m.Qty&lcSz = &lcTranLine..Qty&lcSz
  RETURN .F.
ENDIF

SELECT (lcTranLine)
REPLACE Qty&lcSz WITH m.Qty&lcSz ,;
        TotQty   WITh Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8 

m.TotQty   = &lcTranLine..TotQty


SHOW GETS WINDOW (lcWinC22) ONLY
SHOW WINDOW (lcBrTtl1) REFRESH SAME


*:*************************************************************
*! Name     : lpSavScr
*! Developer : Hend Ghanem (HBG)
*! Date      : 14/03/2001
*! Purpose  : Save/Update shimpent intransit
*:*************************************************************
PROCEDURE lpSavScr

*-- Save lines in CUTTKTL file
SELECT CUTTKTL
SET ORDER TO CUTTKTLI

SELECT (lcTranLine)

LOCATE FOR TotQty=0
IF FOUND()
  *-One or more lines has zero quantity,Ignore this lines.
  IF gfModalGen('TRM34052B34001','DIALOG')=1
    DELETE REST FOR TotQty=0
    =lfwBrow1()
  ELSE
    STORE .F. TO llShow,llCSave
    RETURN .F.
  ENDIF
ENDIF

GO TOP


IF EOF()
  =gfModalGen('TRM00000B00000','DIALOG',;
                  .F.,.F.,'The lines for this invoice are missing. Cannot update.')
  STORE .F. TO llShow,llCSave
  RETURN 
ENDIF

SELECT CUTTKTL
GO TOP
IF SEEK(laData[1]+laData[2]+laData[3])
  DELETE REST FOR CCUTVEN+CSHIP_TYPE+CCUTINV = laData[1]+laData[2]+laData[3]
ENDIF  

SELECT (lcTranLine)
GO TOP
laData[16] = 0
SCAN
  SCATTER MEMVAR MEMO

  m.CCUTVEN    = laData[1]
  m.CSHIP_TYPE = laData[2]
  m.CCUTINV    = laData[3]
  DO CASE
    CASE laData[2] = 'R'  
      m.Trancd = '5'
    CASE laData[2] = 'S'
      m.Trancd = '6'
      m.cStyGrade = '2'
    CASE laData[2] = 'B'
      m.Trancd = '5'      
  ENDCASE

  *SELECT CUTTKTL
  *GO TOP
  *IF SEEK(m.CCUTVEN+m.CSHIP_TYPE+m.CCUTINV+m.STYLE+STR(m.nLot_No,4)+m.Trancd)
   * SELECT CUTTKTL
   * GATHER MEMVAR MEMO
  *ELSE
    INSERT INTO CUTTKTL FROM MEMVAR
  *ENDIF

  laData[16] = laData[16] + m.TotQty

  *SELECT (lcTranLine)
ENDSCAN


*-- Save header in TRANSHDR file
SELECT TRANSHDR
IF !SEEK(m.CCUTVEN+m.CSHIP_TYPE+m.CCUTINV)
  APPEND BLANK
ENDIF  

REPLACE CCUTVEN    WITH laData[1],;
        CSHIP_TYPE WITH laData[2],;
        CCUTINV    WITH laData[3],;
        STATUS     WITH laData[4],;
        Entered    WITH laData[5],;
        cartons    WITH laData[6],;
        CCONTANR   WITH laData[7],;
        ETA        WITH laData[8],;
        cVessel    WITH laData[9],;
        reference  WITH laData[10],; 
        DCUSTOM    WITH laData[11],;
        DTRDATE    WITH laData[12],;
        DWAREHOUS  WITH laData[13],;
        SHIPVIA    WITH laData[14],;
        NOTE       WITH laData[15],;
        TOTQTY     WITH laData[16]
        
=lfUpdUsrFl('CCUTVEN',laData[1])
=lfUpdUsrFl('CSHIP_TYPE',laData[2])
=lfUpdUsrFl('CCUTINV',laData[3])
=lfUpdUsrFl('CCONTANR',laData[7])

*B604463,1 HGB 05/07/2001 (Begin) Returning the screen mode to the select mode.
STORE .F. TO laScrMode  
laScrMode[1] = .T.
SHOW GETS
*B604463,1 HGB 05/07/2001 (End)

*!**************************************************************************
*! Name      : lfUpdUsrFl
*! Developer : Hend Ghanem (HBG)
*! Date      : 14/03/2001
*! Purpose   : Updating User defined fields.
*!**************************************************************************
*! Example   : =lfUpdUsrFl()
*!**************************************************************************
*
FUNCTION lfUpdUsrFl
PARAMETERS lcFldName,lcFldValue

lnFldPos = ASUBSCRIPT(laUsrFields,ASCAN(laUsrFields,lcFldName),1)
laUsrFields[lnFldPos,6] = lcFldValue
*-- end of lfUpdUsrFl.

*!**************************************************************************
*! Name      : lfCkOpnQty
*! Developer : Hend Ghanem (HBG)
*! Date      : 14/03/2001
*! Purpose   : Get the open Qty of the Lot #
*!**************************************************************************
*! Example   : =lfUpdUsrFl()
*!**************************************************************************
*
FUNCTION lfCkOpnQty

SELECT CUTTKTL
GO TOP
lnNewLot = 0

IF SEEK(lcStyle)
  SCAN REST WHILE style+cuttkt+trancd = lcStyle 
    *khalid
    IF SEEK(CutTkt,'CutTktH') AND CutTktH.Status = "X"
      LOOP
    ENDIF
    *khalid
    IF lnNewLot <> nLot_No
      INSERT INTO (lcLotLines) (cuttkt , Style , nLot_No);
                        VALUES (CUTTKTL.Cuttkt,lcStyle,CUTTKTL.nLot_No)
      lnNewLot = nLot_No                  
    ENDIF                    
    SELECT (lcLotLines)
    FOR lnI = 1 To 8
      lcI = STR(lnI,1)
      REPLACE &lcLotLines..Qty&lcI WITH IIF(CUTTKTL.TRANCD='1', &lcLotLines..Qty&lcI+CUTTKTL.Qty&lcI,;
                                                            MAX(&lcLotLines..Qty&lcI-CUTTKTL.Qty&lcI,0))
    ENDFOR
    SELECT CUTTKTL
  ENDSCAN
ENDIF


SELECT (lcTranLine)
GO TOP
IF SEEK(laData[1]+laData[2]+laData[3]+lcStyle)
  SCAN REST WHILE ccutven+cship_type+ccutinv+style+;
                      STR(nlot_no,4)+trancd+STR(RECNO(),7) = laData[1]+laData[2]+laData[3]+lcStyle
    =SEEK(&lcTranLine..Style+&lcTranLine..cuttkt,lcLotLines)
    SELECT (lcLotLines)
    REPLACE cuttkt  WITH &lcTranLine..cuttkt,;
            Style   WITH &lcTranLine..Style,;
            nLot_No WITH &lcTranLine..nLot_no

    SELECT (lcLotLines)
    FOR lnI = 1 To 8
      lcI = STR(lnI,1)
      SELECT (lcLotLines)      
      REPLACE &lcLotLines..Qty&lcI WITH MAX(&lcLotLines..Qty&lcI -  &lcTranLine..Qty&lcI,0)
    ENDFOR
    SELECT (lcTranLine)
  ENDSCAN
  
ENDIF  

SELECT (lcLotLines)
GO TOP
SCAN
  FOR lnI = 1 To 8
    lcI = STR(lnI,1)
    REPLACE &lcLotLines..TotQty WITH &lcLotLines..TotQty + &lcLotLines..Qty&lcI 
  ENDFOR  
ENDSCAN



*!**************************************************************************
*! Name      : lfChecks
*! Developer : Hend Ghanem (HBG)
*! Date      : 14/03/2001
*! Purpose   : 
*!**************************************************************************
*! Example   : =lfChecks()
*!**************************************************************************
*
FUNCTION lfChecks

SELECT CUTTKTL
SET ORDER TO Cuttktli
lnRecNo = RECNO()

IF SEEK(laData[1]+laData[2]+laData[3]+lcStyle+PADL(ALLTRIM(lnLot),4,' '),'CUTTKTL') OR;
   SEEK(laData[1]+laData[2]+laData[3]+lcStyle+PADL(ALLTRIM(lnLot),4,' '),lcTranLine)
   =gfModalGen('TRM00000B00000','DIALOG',;
                  .F.,.F.,'Style/Lot '+lcStyle+'/'+ALLTRIM(lnLot)+' has been entered before. Cannot proceed')

   STORE "" TO lcStyle, lcStyDesc, lnLot
   
   SHOW GETS WINDOW (lcWinC22) DISABLE ONLY 
   =lfRefresh(lcWinC22)
   IF !EOF(lcTranLine)
     SHOW GET pbRemove ENABLE 
   ENDIF
   SHOW GET pbNew ENABLE 
   _CUROBJ=OBJNUM(pbNew)  
   SELECT CUTTKTL
   SET ORDER TO lcOrder 
   SELECT (lnAlias)             
   RETURN .F.
ENDIF

IF CUTTKTH.Status $ 'HO'
  lnChoice =gfModalGen('TRM00000B36001','DIALOG',;
                .F.,.F.,'This cutting ticket has not been actualized, Apply quantity?')
  IF lnChoice = 2
    STORE "" TO lcStyle, lcStyDesc, lnLot
    SHOW GETS WINDOW (lcWinC22) DISABLE ONLY

    IF !EOF(lcTranLine)
      SHOW GET pbRemove ENABLE 
    ELSE
      SHOW GET pbRemove DISABLE
    ENDIF
    
    SHOW GET pbNew ENABLE 
    _CUROBJ=OBJNUM(pbNew)
    SELECT CUTTKTL
    SET ORDER TO lcOrder 
    SELECT (lnAlias)             
    RETURN .F.
  ENDIF    
ENDIF

IF CUTTKTH.Status $ 'CX'
  lcMesg = 'This cut ticket has been '+IIF(CUTTKTH.Status="C",;
                                    'completely received. Cannot proceed.',;
                                    'Cancelled. Cannot proceed.')
  
  =gfModalGen('TRM00000B00000','DIALOG',;
              .F.,.F.,lcMesg)
  STORE "" TO lcStyle, lcStyDesc, lnLot
  SHOW GETS WINDOW (lcWinC22) DISABLE ONLY


  IF !EOF(lcTranLine)
    SHOW GET pbRemove ENABLE 
  ELSE
    SHOW GET pbRemove DISABLE
  ENDIF

  SHOW GET pbNew ENABLE 
  _CUROBJ=OBJNUM(pbNew)
  SELECT CUTTKTL
  SET ORDER TO lcOrder 
  SELECT (lnAlias)             
  RETURN .F.
ENDIF  
RETURN .T.



*!*************************************************************
*! Name      : lfvMajor
*! Developer : Hend Ghanem (HBG)
*! Date      : 12/21/2000
*! Purpose   : Valid function to validate style field.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*
FUNCTION lfvMajor

PRIVATE llOpenSty, lnAlias, lcOrder
STORE .F. TO  llOpenSty

IF !llbrowse AND LASTKEY() <> 13
  RETURN
ENDIF
IF !llbrowse AND EMPTY(SUBSTR(lcStyle,1,3)) AND EMPTY(SUBSTR(lcStyle,5,LEN(ALLTRIM(lcStyle)))) AND;
                 EMPTY(RIGHT(lcstyle,6))
  RETURN
ENDIF
SELECT Style
LOCATE
IF llbrowse OR (!EMPTY(lcStyle) AND !SEEK(lcStyle,'STYLE'))
  llbrowse = .F.
  lcStyle = gfStyBrw('I',lcStyle,"",.F.)
  IF EMPTY(lcStyle)
    SHOW GETS WINDOW (lcWinC22) DISABLE ONLY 
    IF !EOF(lcTranLine)
      SHOW GET pbRemove ENABLE 
    ELSE
      SHOW GET pbRemove DISABLE
    ENDIF

    SHOW GET pbNew ENABLE 
    _CUROBJ=OBJNUM(pbNew)
    RETURN
  ENDIF
ENDIF

*-- Check if there is a lots for this style or not
lnAlias = SELECT(0)
SELECT CUTTKTL
lcOrder = TAG()
SET ORDER TO Cuttktls
IF !EMPTY(lcStyle) AND !SEEK(lcStyle)
  =gfModalGen('TRM00000B00000','DIALOG',;
                 .F.,.F.,'No lots found for this ' + ALLTRIM(lcStyle)+'.')
  lcStyle   = ""
  lcStyDesc = ""
  SHOW GETS WINDOW (lcWinC22) DISABLE ONLY 
  =lfRefresh(lcWinC22)
  

  IF !EOF(lcTranLine)
    SHOW GET pbRemove ENABLE 
  ELSE
    SHOW GET pbRemove DISABLE
  ENDIF

  SHOW GET pbNew ENABLE 
  _CUROBJ=OBJNUM(pbNew)  
  RETURN
ENDIF

IF EMPTY(lcStyle)
  SHOW GETS WINDOW (lcWinC22) DISABLE ONLY 
  SHOW GET pbNew ENABLE 
  _CUROBJ=OBJNUM(pbNew)
  RETURN
ELSE
  lcStyDesc = STYLE.Desc
  SHOW GET lcStyDesc
  SHOW GET lnLot ENABLE
  SHOW GET ibLot ENABLE
  _CUROBJ=OBJNUM(lnLot)

ENDIF

SELECT CUTTKTL
SET ORDER TO lcOrder 
SELECT (lnAlias)

RETURN
*-- end of lfvMajor.

*!*************************************************************
*! Name      : lfvLot()
*! Developer : Hend Ghanem (HBG)
*! Date      : 14/03/2001
*! Purpose   : Valid function to validate Lot field.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*
FUNCTION lfvLot

PRIVATE lnAlias, lcOrder, llFound, lcFile_Ttl, lcKeyVal
STORE .F. TO llFound
DIMENSION laTempData[2]
STORE '' TO laTempData

IF !llbrowse AND LASTKEY() <> 13 
  RETURN
ENDIF

IF EMPTY(lnLot) .AND. !ll2Brow  &&AND !llBrowse
  RETURN
ENDIF


lnAlias = SELECT(0)
SELECT CUTTKTL
lcOrder = TAG()
SET ORDER TO Cuttktls

SELECT (lcLotLines)  
ZAP
=lfCkOpnQty()
  
SELECT (lcLotLines)
SET RELATION TO CUTTKT INTO CUTTKTH
  
lcKeyVal   = lcStyle
SELECT (lcLotLines)  


lcFile_Ttl = 'Cutting Tickets'
lcPrvField = lcBrFields
lcBrFields = "Cuttkt           :H='Cuttkt'  ,"+;
             "Style            :H='Style'   ,"+;
             "nLot_No          :H='Lot #'   ,"+;
             "Qty1             :H='Qty1'    ,"+;
             "Qty2             :H='Qty2'    ,"+;
             "Qty3             :H='Qty3'    ,"+;
             "Qty4             :H='Qty4'    ,"+;
             "Qty5             :H='Qty5'    ,"+;
             "Qty6             :H='Qty6'    ,"+;
             "Qty7             :H='Qty7'    ,"+;
             "Qty8             :H='Qty8'    ,"+;
             "TotQty           :H='TotQty'" 

llLotFound = .F.
IF SEEK (lcStyle)
  LOCATE REST WHILE style+CutTkt+STR(nlot_no,4) = lcStyle FOR STR(nlot_no,4) = PADL(ALLTRIM(lnLot),4,' ')
  llLotFound = FOUND()
ENDIF

IF llbrowse OR ATC('?',lnLot) <> 0 OR !llLotFound           
  llbrowse = .F.
  ll2Brow  = .F.
  IF !AriaBrow([lcKeyVal],lcFile_Ttl,gnBrFSRow1,gnBrFSCol1,gnBrFSRow2,gnBrFSCol2,.F.,.F.,'nLot_No,Cuttkt','laTempData')
    STORE "" TO lcStyle, lcStyDesc, lnLot
    SHOW GETS WINDOW (lcWinC22) DISABLE ONLY 
    =lfRefresh(lcWinC22)

    IF !EOF(lcTranLine)
      SHOW GET pbRemove ENABLE 
    ELSE
      SHOW GET pbRemove DISABLE
    ENDIF
    lcPrvField = lcBrFields
    SHOW GET pbNew ENABLE 
    _CUROBJ=OBJNUM(pbNew)  
    RETURN
  ENDIF
ENDIF
lcBrFields =lcPrvField 

lnLot    = PADL(&lcLotLines..nLot_No,4,' ')
lcCuttkt = &lcLotLines..CutTkt
SHOW GET lnLot
llContenu =lfChecks()
IF !llContenu 
  RETURN
ENDIF

SHOW GETS WINDOW (lcWinC22) ENABLE ONLY 
SHOW GET m.TotQty DISABLE
SHOW GET lnLot    DISABLE
SHOW GET lcStyle  DISABLE
SHOW GET ibLot    DISABLE
SHOW GET ibStyle  DISABLE

  
SELECT CUTTKTL
SET ORDER TO Cuttktls
=SEEK(lcStyle+lcCuttkt+'1')
SCATTER MEMVAR MEMO
SELECT (lcTranLine)
APPEND BLANK
 
=SEEK(lcStyle,'STYLE') 
=SEEK('S'+STYLE.Scale,'SCALE')
  
FOR lnI = 1 TO SCALE.Cnt
  lcI = STR(lnI,1)
  m.Qty&lcI = 0
ENDFOR
m.TOTQTY = 0
  
  
GATHER MEMVAR MEMO
REPLACE &lcTranLine..ccutven    WITH laData[1],;
        &lcTranLine..cship_type WITH laData[2],;
        &lcTranLine..ccutinv    WITH laData[3],;
        &lcTranLine..Style      WITH lcStyle  ,;
        &lcTranLine..Desc       WITH lcStyDesc,;
        &lcTranLine..Scale      WITH STYLE.Scale ,;
        &lcTranLine..nLot_No    WITH INT(VAL(lnLot)),;
        &lcTranLine..Check      WITH 'N'

_CUROBJ=OBJNUM(m.Qty1) 
  
SHOW WINDOW (lcBrTtl1) REFRESH SAME

=lfwBrow1()

RETURN
*-- end of lfvLot.

*****************

*!*************************************************************
*! Name      : lpDelScr
*! Developer : Ahmed Maher 
*! Date      : 05/05/2002
*! Purpose   : Valid procedure to cancel the invoice
*!*************************************************************
*B605913,1 AMH Add local delete proc. [Start]
*!*************************************************************

PROCEDURE lpDelScr

IF TRANSHDR.Status <> 'O'
  =gfModalGen('TRM00000B00000','DIALOG',.F.,.F.,;
              'Invoice status is '+ALLTRIM(LOWER(lcStatus))+'. Cannot proceed.')
  RETURN
ENDIF

SET ORDER TO CUTTKTLI IN CUTTKTL
IF SEEK(laData[1]+laData[2]+laData[3],'CUTTKTL')
  SELECT CUTTKTL
  DELETE REST WHILE cCutVen+cShip_Type+cCutInv+Style+STR(nLot_No,4)+TranCd+STR(RECNO(),7) =;
                    laData[1]+laData[2]+laData[3] FOR TranCd $ '56'
ENDIF
SET ORDER TO CUTTKTL IN CUTTKTL

IF SEEK(laData[1]+laData[2]+laData[3],'TRANSHDR')
  SELECT TRANSHDR
  REPLACE Status WITH 'X'
ENDIF
*B605913,1 AMH [End]