*:************************************************************************
*: Procedure file: POSHP.PRG                                            :*
*:         System: ARIA APPAREL SYSTEM 2.7                              :*
*:         Module: Purchase orders                                      :*
*:         Author: Amr Abdel Raouf Hegazy                               :*
*:           Date: 05/25/97                                             :* 
*:************************************************************************
*: Modifications :                                                      :*
*:            HDM 10/28/1998 Adding Inquier Parameter                   :*
*:B802468,1   WAB 08/07/1999 after changed the Prepak code ,the Tot.Qty 
*:B802468,1       and size Qty doesn't get updated according to the new changes.
*:B802468,1	      the screen freesing at the edit & view mode
*C101704,1 SAM 01/17/2000 Allow alpha numeric PO# manualy
*:E301159,1   WMA Add Fields in Shipment Header File (SHPMTHDR) 
*:E301159,1       and in Shipment Screen in Header Folder
*:B603749,1 ADEL 07/16/2000 Enhance the performace of the screen.
*:B603793,1 ABD 08/07/2000 Fix bug when create new shipment and select PO 
*:B603793,1 ABD            that parially received the shipment qty should be 
*:B603793,1 ABD            the open on that PO line and not the ordered.
*:B803568,1 ABD 08/07/2000 Fix bug that the system allow to assign the same PO# in 
*:B803568,1 ABD            more than one shipment,Even you assign all the PO Qty.
*:B603839,1 ABD 08/22/2000 Fix the following bug :-
*:B603839,1 ABD            1) Fit the browse screen to see all the scrren.
*:B603839,1 ABD            2) Browse Zero in filed OpenQty If Less than 
*:B603839,1 ABD               Zero upon the user over recive the Qty.
*:B603839,1 ABD            3) Speed up the Performance of the screen.
*:B603882,1 RAMY 09/10/2000 fix the bug of wrong calculation in the open qty
*:B803680,1 ABD  09/24/2000 give user message when get the lines for the Selected po.
*:B603936,1 KHM  10/02/2000 Optimize the speed.
*:C102160,1 AMH  02/08/2001 Adding a new option to the menu to allow the user
*:C102160,1                 to view shipment lines after receiving as a custom
*:C102160,1                 process for GMA.
*:C102160,4 AMH  02/27/2001 Fix some bugs in Entry C102160.
*:B804301,1 MHM  07/16/2001 Fix the bug of not allowing to add a PO to a
*:B804301,1                 shipment even if there were open quantities
*:B804301,1                 in the PO.
*:B604737,1 KHM 07/30/2001  Fix the bug of not displaying the notification
*:B604737,1                 message in case you press Esc while you are in 
*:B604737,1                 the Add or Edit mode.
*:B604836,1 KHM 08/20/2001  Fix the bug of wrong calculation of total qty.
*:B605208,1 AMH 12/27/2001  Add field to show the total quantaty of the whole shipment.
*:E301797,1 ADEL 01/20/02   Apply shipment upon Mateial also.
*:B607280,1 ABD 06/04/2003  The Shipment screen Stock, Damaged, Canceled and Total Qty fields 
*:B607280,1 ABD Cannot accept more than 999.999 (it shows **** if more). 
*:B607280,1 ABD The fields need to be enlarged to at least: 9999999.999 or 
*:B607280,1 ABD The maximum saved in the database.
*:B607107,1 TMI 10/12/2003 Fix the bug that when opening another screen the temp file to add lines is zapped.
*:B119930,1 ABD 10/19/2003 Fix bug Wrong Open Qty on Material PO Shipment.
*:B120628,1 ASH 11/16/2003 Fix bug of not saving header fields user defined field in case of completed shipments.
*:B038431,1 MHM 08/23/2004 Fix bug of SHPTHDR File Not Found and Message not found
*:C123616,1 TMI 15/09/2004 Define a menu pad for the shipment screen, but its bars will be defined via triggers
*:B125864,1 NNA 01/02/2005 Fix bug that happen when you open the material screen for a fabric who
*:B125864,1 NNA            has a shipment Qty. then press In Transit button you'll get an error
*:B125864,1 NNA            'Variable 'PO' not found'
*:B126771,1 NNA 04/13/2005 fix bug that you cann't Edit in the shipment Details
*:B040280,1 NNA 06/21/2006 Generate 943 EDI transaction for PO shipment directed to 3PL providers.
*:************************************************************************

*--HDM[START]
*E301797,1 (Begin) Add a new papameter for checking if coming from material screen.
*PARAMETERS lcShpMntNo
PARAMETERS lcShpMntNo,lcCalldFrom
llFromMA = (TYPE('lcCalldFrom') = 'C' AND lcCalldFrom = 'M') OR (TYPE('llFromMat') = 'L' AND llFromMat)
*E301797,1 (End)
*--HDM[END]
*E301159,1  (WMA) Define variables [Begin]
DIMENSION laShpVia[1,2]
STORE "" TO laCodInfo,laShpVia,lcCodeType
*E301159,1  (WMA)  [End]


EXTERNAL ARRAY laData , laDefProc
DECLARE lafoldwinds[2,2],laScrMode[5],laKeyField[1,4],laWareH[1]
*--HDM[START]
llExCalled = (PARAMETERS()=1) OR TYPE('lcShpMntNo')='C'
*--HDM[END]

laKeyField[1,1] = 'laData[1]'
laKeyField[1,2] =.T.
laKeyField[1,3] = 'SHPMTHDR'
laKeyField[1,4] = 1
*:B038431,1 MHM 08/23/2004 [Start]
IF llFromMA
  laKeyField[1,3] = 'MATSHPHD'
ENDIF  
*:B038431,1 MHM  [End]



*E301797,1 (Begin) Adjust for Material shiprment.
IF llFromMA
  lcBaseFile  = 'MATSHPHD'
  laKeyField[1,3] = 'MATSHPHD'
  laKeyField[1,4] = 1
ENDIF
DECLARE laWareM[1]
lcDesc   = ' '
lcWare   = ' '
*E301797,1 (End)

STORE ' ' TO lcWinCh0,lcWinCh1,lcWinCh2,lcWinC21,lcWinC22,lcWinC23,lcWinC24,lcWinCh3
STORE ' ' TO lcScFields,lcfolder,lcfoldprnt,lcStatus,ldOldDate,lcBrTtl1,lcShpLine,lcTmpPoLn,lcStyHdr,lcEmptySty,lcStylePct
STORE ' ' TO lcSz1,lcSz2,lcSz3,lcSz4,lcSz5,lcSz6,lcSz7,lcSz8,lcStyDesc,lcLastStyle,lcOldWare,lcDelMesag
STORE 0   TO lnStyleWid,lnRecvStk,lnRecvDam,lnRecvCan

*C102160,1 AMH Custom variable to hold the total in transit of shipment [Start]
lnInTranst = 0
*C102160,1 AMH [End]

STORE 1   TO lnActFolder,lnMarker
*C101704,1 [Start] variable for setup to know generate PO# manually or not  
*STORE .F. TO llBrowse,llWareHous,llNewShp 
STORE .F. TO llBrowse,llWareHous,llNewShp ,llGenOrNum
*C101704,1 [End]
STORE .T. TO llAlowNew
*lcEscTrap  = ON("KEY","ESC")
lcRedClr   = "RGB(,,,255,0,0)"
llNoShow = .F.            && Flag to make the screen call the PROCEDURE lpShow evry time it run
*lcTrpStat  = "ENABLE"
*SHOW GET ibTrap1 &lcTrpStat

*B802468,1 WAB - Logic Variable fo Check if user change the prepack  && for disp objct
*B802468,1 WAB - START
llPrepak = .F.
lcObjDisp = 'DISABLE' 			&& for enable or disabled object
*B802468,1 WAB - END

*B603793,1 ABD Add logical variable to cheak if open Qty > 0 .[Begin]
lcOpenQty = 'DISABLE'
*B603793,1 ABD [End]

*B604737,1 KHM 07/30/2001 (Begin) Initializing the following variables.
STORE .T. TO llCUpdate
STORE .F. TO llUpdated
*B604737,1 KHM 07/30/2001 (End)

*B605208,1 AMH Add variable to hold the total quantity of the whole shipment [Start]
STORE 0 TO lnTotQty
DECLARE laOldQty[8]
STORE 0 TO laOldQty
*B605208,1 AMH [End]

*B607107    TMI [Start] Variable to save last mode 
lnLastMode = 1
*B607107    TMI [End  ] 
IF !gfSetup()
  RETURN
ENDIF

*B125864,1 NNA 01/02/2005 (Begin) if it from the material module then I empty Array laUsrFields that
*B125864,1 NNA            holds the fields for SHPMTHDR file because in this case our base file is 'MATSHPHD'
IF llFromMA
 STORE '' TO laUsrFields
ENDIF
*B125864,1 NNA (End)

*--Initialize the Folders array.
lnFolderCEnd = 102.50
lnFolderREnd =   2.00
lnNoFld      =   2
lcwfoldchng  = '=lfActFolder()'  && function to control shows after change the folder
lcfoldpush   = 'pbFolders'       && push button name for the next folder
laDefProc[7]  = .F.              && Disable the control panel delete proc.(lpDelScr)
laDefProc[9]  = .F.              && Disable the control panel save proc.  (lpSavScr)
*laDefProc[10] = .F.              && Disable the control panel close proc. (lpClsScr)


*--Open needed files.
*:B038431,1 MHM 08/23/2004 Check if called from MA [Start]
IF !llFromMA
*:B038431,1 MHM  [End]

=gfOpenFile(gcDataDir+'ShpmtHdr','ShpmtHdr','SH')

*:B038431,1 MHM 08/23/2004 Check if called from MA [Start]
ENDIF
*:B038431,1  [End]

*E301797,1 (Begin) Adjust for Material shiprment.
IF llFromMA
  *:B038431,1 MHM 08/23/2004 Get Correct Title[Start]
  lcFile_ttl = 'Material PO Shipments'
  *:B038431,1 MHM 08/23/2004  [Start]

  =gfOpenFile(gcDataDir+'MATSHPHD','MATSHPHD','SH')
  =gfOpenFile(gcDataDir+'MATSHPDT','MATSHPDT','SH')
  =gfOpenFile(gcDataDir+'FABRIC','FABRIC','SH')
  =gfOpenFile(gcDataDir+'FabDye','FabDye','SH')
  =gfOpenFile(gcDataDir+'POFHDR','POFHDR','SH')
  =gfOpenFile(gcDataDir+'POFLN','POFLN','SH')
  lcBaseFile  = 'MATSHPHD'
ENDIF  
*E301797,1 (End)

=gfOpenFile(gcDataDir+'Style','Style','SH')
=gfOpenFile(gcDataDir+'StyDye','StyDye','SH')
=gfOpenFile(gcDataDir+'WareHous','WareHous','SH')
=gfOpenFile(gcDataDir+'PosHdr','PosHdr','SH')

*E301159,1  (WMA) Open Codes File For Get ShipVia by Function gfwCodePop() [Begin]
=gfOpenFile(gcDataDir+'Codes','Ccode_no','SH')
*E301159,1  (WMA)  [End]

*-- Define the variable of the system
lcCancel  = gcBmpHome + "trash.bmp"
lcUncan   = gcBmpHome + "untrash.bmp"
lcPromp   = lcCancel
lcDelMesag= 'cancel'


IF !WEXIST(gcBaseWind)
  lcEmptySty = STRTRAN(GFITEMMASK("PI"),'X',' ')
  lcStyHdr   = gfItemMask('HI')
  lcStylePct = gfItemMask("PI")
  lnStyleWid = LEN(lcStylePct)

  *C101704,1 [Satrt] get setup to know generate PO# manually or not
  llGenOrNum = ALLTRIM(gfGetMemVar('M_GenStOrN')) = 'Y'
  *C101704,1 [End]
  lcBrTtl1 = 'Detail Lines'
  
  *E301159,1  (WMA) Add Fields in Browse Screen [Begin]
  *lcBrFields = "ShipNo    :R :H='Shipment #':8,"+;
                "Status    :R :H='S':2,"+;
                "Entered   :R :H='Entered':8,"+;
  	            "Cartons   :R :H='Cartons':5,"+;
	            "AirWayB   :R :H='Air-Way Bill #':15,"+;
    	        "ETA       :R :H='E.T.A.':8,"+;
    	        "TotQty    :R :H='In-Transit':7,"+;
    	        "Recv_Stk  :R :H='Received':6,"+;
    	        "Recv_Dam  :R :H='Damaged':6,"+;
    	        "Recv_Can  :R :H='Canceled':6,"+;
    	        "cVessel   :R :H='Airline/Vessel':30,"+;
    	        "Reference :R :H='Reference':30"
 
  lcBrFields = "ShipNo     :R :H='Shipment #':8,"+;
               "Status     :R :H='S':2,"+;
               "Entered    :R :H='Entered':8,"+;
  	           "Cartons    :R :H='Cartons':5,"+;
	           "AirWayB    :R :H='Air-Way Bill #':15,"+;
    	       "ETA        :R :H='E.T.A.':8,"+;
    	       "TotQty     :R :H='In-Transit':7,"+;
    	       "Recv_Stk   :R :H='Received':6,"+;
    	       "Recv_Dam   :R :H='Damaged':6,"+;
    	       "Recv_Can   :R :H='Canceled':6,"+;
    	       "cVessel    :R :H='Airline/Vessel':30,"+;
    	       "Reference  :R :H='Reference':30,"+;
               "Dcustom    :R :H='Customs':8,"+;
               "Dtrdate    :R :H='Trucker':8,"+;
               "Dwarehous  :R :H='WareHouse':8,"+;
               "Shipvia    :R :H='ShipVia':6,"+;                               
               "Cship_adv  :R :H='Ship/Advise':10,"+;
               "Note       :R :H='Note':30"                             
  *E301159,1  (WMA) [End]
  *E301797,1 (Begin) Adjust screen title
  IF llFromMA
    lcWindTitle = 'Material PO Shipment'
    lcBrFields  = STRTRAN(lcBrFields,'TotQty','NFaBTOTQTY')
    lcBrFields  = STRTRAN(lcBrFields,'Recv_Stk','RecvStk')
    lcBrFields  = STRTRAN(lcBrFields,'Recv_Dam','RecvDam')
    lcBrFields  = STRTRAN(lcBrFields,'Recv_Can','RecvCan')
  ENDIF
  *E301797,1 (End)
  
  
  *************----1-----*----2-----*----3-----*----4-----*----5-----****
  *E301159,1  (WMA) Add Screen Fields [Begin]
  *lcScFields ='ShipNo    ,Status    ,Entered   ,Cartons   ,AirWayB   ,'+;
              'ETA       ,cVessel   ,Reference' 
   lcScFields ='ShipNo    ,Status    ,Entered   ,Cartons   ,AirWayB   ,'+;
              'ETA       ,cVessel   ,Reference  ,DCUSTOM   ,DTRDATE   ,'+;
              'DWAREHOUS ,SHIPVIA   ,CSHIP_ADV  ,NOTE      '    
  *E301159,1  (WMA) [End]
  ***********************************************************************

  *E301797,1 (Begin) Adjust for Material shiprment.
  *SELECT ShpmtHdr
  IF llFromMA
    SELECT MATSHPHD
  ELSE
    SELECT ShpmtHdr
  ENDIF  
  *E301797,1 (End)

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

  lcShpLine  = gfTempName()
  lcTmpPoLn  = gfTempName()
  llWareHous = gfGetMemVar('M_WareHouse')='Y'

  *E301159,1  (WMA) Array to Hold the Code information For ShipVia [Begin]  
  DIMENSION laCodInfo [1,10]
  laCodInfo[1,1]  = "SHIPVIA"
  laCodInfo[1,2]  = "laShpVia"
  laCodInfo[1,3]  = "lnShpVia"
  laCodInfo[1,4]  = ""
  laCodInfo[1,5]  = .F.    
  laCodInfo[1,6]  = .F.     
  laCodInfo[1,7]  = "SHPMTHDR"     
  laCodInfo[1,8]  = "SHPMTHDR"
  *E301797,1 (Begin) Adjust for Material shiprment.
  IF llFromMA
    laCodInfo[1,7]  = "MATSHPHD"     
    laCodInfo[1,8]  = "MATSHPHD"
  ENDIF  
  *E301797,1 (End)
  laCodInfo[1,9]  = "laData[1]"
  laCodInfo[1,10] = "ShipVia"     
  *E301159,1  (WMA) [End]
  
  *B038431,1 MHM 08/23/2004 no need for initializing warehouse[Start]
  *IF llWareHous
  *  DIME laWareH[1]
  *  laWareH[1] = 'N/A'
  *  lnWHNo=1 
  *ENDIF
  *B038431,1 [End]
  
  *E301797,1 (Begin) Set temp files for material shipment.
  IF llFromMA
    SELECT MATSHPDT 
    =AFIELDS(laFStru)
    DIMENSION laFStru[ALEN(laFStru,1)+1,4]
    laFStru[ALEN(laFStru,1),1] = 'lNewRec'
    laFStru[ALEN(laFStru,1),2] = 'L'
    laFStru[ALEN(laFStru,1),3] = 1
    laFStru[ALEN(laFStru,1),4] = 0
    *--Open Temp files exclusive.
    lcOldExcl = SET('EXCL')
    SET EXCLUSIVE ON
    =gfCrtTmp(lcShpLine,@laFStru,'pomat+fabric+color+STR(LineNo,6)',lcShpLine)
    lnFStru = ALEN(laFStru,1)
    DIMENSION laFStru[lnFStru+5 ,4]
    laFStru[lnFStru+1,1] = 'cMarker'
    laFStru[lnFStru+1,2] = 'C'
    laFStru[lnFStru+1,3] = 1
    laFStru[lnFStru+1,4] = 0
    laFStru[lnFStru+2,1] = 'cStatus'
    laFStru[lnFStru+2,2] = 'C'
    laFStru[lnFStru+2,3] = 1
    laFStru[lnFStru+2,4] = 0
    laFStru[lnFStru+3,1] = 'nOpenQty'
    laFStru[lnFStru+3,2] = 'N'
    laFStru[lnFStru+3,3] = 6
    laFStru[lnFStru+3,4] = 0
    laFStru[lnFStru+4,1] = 'nRecevQty'
    laFStru[lnFStru+4,2] = 'N'
    laFStru[lnFStru+4,3] = 6
    laFStru[lnFStru+4,4] = 0
    laFStru[lnFStru+5,1] = 'ntransQty'
    laFStru[lnFStru+5,2] = 'N'
    laFStru[lnFStru+5,3] = 6
    laFStru[lnFStru+5,4] = 0
    =gfCrtTmp(lcTmpPoLn,@laFStru,'cmattype+pomat+fabric+color+trancd+STR(LineNo,6)',lcTmpPoLn)
    SET EXCLUSIVE &lcOldExcl
  ELSE
  *E301797,1 (End)
    *--Create temp files.
    *--HDM Check For File Usage First[start]
    IF !USED('POSLN') 
      USE(gcDataDir+'POSLN') IN 0 SHARED
    ENDIF
    *--HDM Check For File Usage First[end]
    SELECT POSLN
    *B603749,1 (Begin) Change the way the temp files created as it take a lot of time.
    *  SELECT *,.F. AS lNewRec FROM POSLN WHERE .F. INTO DBF (gcWorkDir+lcShpLine)
    *SELECT (lcShpLine)
    *INDEX ON PO+Style+STR(LineNo,6) TAG lcShpLine
    *SELECT *,'' AS cMarker,'' AS cStatus FROM POSLN;
     WHERE .F. INTO DBF (gcWorkDir+lcTmpPoLn)
    =AFIELDS(laFStru)
    DIMENSION laFStru[ALEN(laFStru,1)+1,4]
    laFStru[ALEN(laFStru,1),1] = 'lNewRec'
    laFStru[ALEN(laFStru,1),2] = 'L'
    laFStru[ALEN(laFStru,1),3] = 1
    laFStru[ALEN(laFStru,1),4] = 0
    *--Open Temp files exclusive.
    lcOldExcl = SET('EXCL')
    SET EXCLUSIVE ON
    =gfCrtTmp(lcShpLine,@laFStru,'PO+Style+STR(LineNo,6)',lcShpLine)
    *B603936,1 KHM 10/02/2000 (Begin) Adding the follwoing fields in the
    *B603936,1                temporary file: nOpenQty,nRecevQty, ntransQty.
    *DIMENSION laFStru[ALEN(laFStru,1)+1,4]
    *laFStru[ALEN(laFStru,1) -1,1] = 'cMarker'
    *laFStru[ALEN(laFStru,1) -1,2] = 'C'
    *laFStru[ALEN(laFStru,1) -1,3] = 1
    *laFStru[ALEN(laFStru,1) -1,4] = 0
    *laFStru[ALEN(laFStru,1) ,1] = 'cStatus'
    *laFStru[ALEN(laFStru,1) ,2] = 'C'
    *laFStru[ALEN(laFStru,1) ,3] = 1
    *laFStru[ALEN(laFStru,1) ,4] = 0
    lnFStru = ALEN(laFStru,1)
    DIMENSION laFStru[lnFStru+5 ,4]
    laFStru[lnFStru+1,1] = 'cMarker'
    laFStru[lnFStru+1,2] = 'C'
    laFStru[lnFStru+1,3] = 1
    laFStru[lnFStru+1,4] = 0
    laFStru[lnFStru+2,1] = 'cStatus'
    laFStru[lnFStru+2,2] = 'C'
    laFStru[lnFStru+2,3] = 1
    laFStru[lnFStru+2,4] = 0
    laFStru[lnFStru+3,1] = 'nOpenQty'
    laFStru[lnFStru+3,2] = 'N'
    laFStru[lnFStru+3,3] = 6
    laFStru[lnFStru+3,4] = 0
    laFStru[lnFStru+4,1] = 'nRecevQty'
    laFStru[lnFStru+4,2] = 'N'
    laFStru[lnFStru+4,3] = 6
    laFStru[lnFStru+4,4] = 0
    laFStru[lnFStru+5,1] = 'ntransQty'
    laFStru[lnFStru+5,2] = 'N'
    laFStru[lnFStru+5,3] = 6
    laFStru[lnFStru+5,4] = 0
    *=gfCrtTmp(lcTmpPoLn,@laFStru)
    =gfCrtTmp(lcTmpPoLn,@laFStru,'cStyType+PO+Style+Trancd+STR(LineNo,6)+STR(RECNO(),7)',lcTmpPoLn)
    *B603936,1 KHM 10/02/2000 (End)
    SET EXCLUSIVE &lcOldExcl
    *B603749,1 (End)  
  *E301797,1 (Begin)
  ENDIF
  *E301797,1 (End)
ENDIF

*--Activate Options pad.
*=lfActPad()
*C123616,1  TMI [Start] Define a menu pad
=lfActPad()
*C123616,1  TMI [End  ] 

*--Call screen.
*E301797,1 (Begin) Adjust for Material shiprment.
*SELECT ShpmtHdr
*SET ORDER TO TAG ShpmtHdr
IF llFromMA
  SELECT MATSHPHD
  SET ORDER TO TAG MATSHPHD
ELSE
  SELECT ShpmtHdr
  SET ORDER TO TAG ShpmtHdr
ENDIF  
*E301797,1 (End)

SELECT (lcBaseFile)
PUSH KEY
PUSH MENU _MSYSMENU
lnSavBarNo = lfGtSavBar()
DEFINE BAR 100 OF P01PU01 PROMPT "Browse" KEY ALT+B

ON KEY LABEL ALT+B

ON SELECTION BAR 100 OF P01PU01 ACTIVATE WINDOW (lcBrTtl1)
*--HDM[START]
IF llExCalled
  llNoShow = .F.
  laScrMode=.F.
  laScrMode[2]=.T.
  *E301797,1 (Begin) Adjust for Material shiprment.
  *SELECT SHPMTHDR
  *SET ORDER TO TAG Shpmthdr
  IF llFromMA
    SELECT MATSHPHD
    SET ORDER TO TAG MATSHPHD
  ELSE
    SELECT SHPMTHDR
    SET ORDER TO TAG Shpmthdr
  ENDIF  
  *E301797,1 (End)
  =SEEK(lcShpMntNo)
  SCATTER FIELDS &lcScFields TO laData
ENDIF
*--HDM[END]

*C102160,1 AMH Custom process for ability to view shipment lines after receiving [Start]
*IF ASCAN(laEvntTrig,PADR("ADOPTGMA",10)) <> 0
*ash1
IF !llFromMA AND ASCAN(laEvntTrig,PADR("ADOPTGMA",10)) <> 0
  =gfDoTriger("POSHP",PADR("ADOPTGMA",10))
  lcShowMode = 'ORG'
  llInTranst = .T.
ELSE
  llInTranst = .F.
ENDIF  
*C102160,1 AMH [End]

*E301797,1 (Begin) Move the screen to tha main screen folder.
*DO (gcScrDir+gcWinAppl+"\POSHP.SPX")

DO (gcScrDir+"POSHP.SPX")
*E301797,1 (Begin) Reintialize this variable for not be used from Style po shipment.
llFromMat = .F.
*E301797,1 (End)

*E301797,1 (End)

POP KEY
POP MENU _MSYSMENU
RELEASE WINDOW (lcBrTtl1)
RELEASE BAR 100 OF P01PU01

*--Open needed files.


*RELEASE PAD _Option OF _MSYSMENU
*C123616,1  TMI [Start] Release Pad 
RELEASE PAD _Option OF _MSYSMENU
ON KEY LABEL ALT+P                && Remove the definition of Alt+P key
*C123616,1  TMI [End  ] 

*--Normal exit.
IF glQuitting
  SELECT (lcShpLine)
  USE
  SELECT (lcTmpPoLn)
  USE
  ERASE (gcWorkDir+lcShpLine+'.DBF')
  ERASE (gcWorkDir+lcShpLine+'.CDX')
  ERASE (gcWorkDir+lcTmpPoLn+'.DBF')
  ERASE (gcWorkDir+lcTmpPoLn+'.CDX')
ENDIF

*C123616,1  TMI [Start] Close temp carton file for MEMO
IF ASCAN(laEvntTrig,PADR("CLOSFILS",10)) <> 0
  =gfDoTriger("POSHP",PADR("CLOSFILS",10))
ENDIF
*C123616,1  TMI [End  ] 

RETURN
*-End...

*:*************************************************************
*! Name      : lfActFolder
*! Purpose   : Validating the active folder
*:*************************************************************
FUNCTION lfActFolder

IF lnActFolder = 1
  SHOW WINDOW (lcWinCh1) TOP
  IF laScrMode[2]
    SHOW GETS WINDOW (lcWinCh1) DISABLE ONLY
  ELSE
    SHOW GETS WINDOW (lcWinCh1) ENABLE  ONLY
    SHOW GET lcStatus DISABLE
  ENDIF
  SHOW GETS WINDOW (lcWinC22) DISABLE ONLY
ELSE
  *ON KEY LABEL ALT+B DO lfwBrow1
  SHOW WINDOW (lcWinCh2) TOP
  IF laScrMode[3] OR laScrMode[4]
    IF lfCkEOF(lcShpLine)
      SHOW GETS WINDOW (lcWinC22) DISABLE ONLY  
      IF !(laData[2] $ 'XC')
        SHOW GET PBNEW ENABLE
    	_CUROBJ=OBJNUM(pbNew)
      ENDIF
    ELSE
      IF !(laData[2] $ 'XC')
        SHOW GETS WINDOW (lcWinC22) ENABLE ONLY        
      ELSE
        SHOW GETS WINDOW (lcWinC22) DISABLE ONLY          
      ENDIF
      IF !llWareHous
        SHOW GET lnWHNo DISABLE
      ENDIF
    ENDIF
  ELSE
    SHOW GETS WINDOW (lcWinC22) DISABLE ONLY    
  ENDIF
  SHOW GETS WINDOW (lcWinCh1) DISABLE ONLY
  IF !lfCkEOF(lcShpLine)
    =lfGetLine()
  ENDIF

  SHOW GET ibPO     DISABLE
  SHOW GET m.PO     DISABLE
  SHOW GET ibStyle  DISABLE

  *E301797,1 (Begin) Adjust for Material shiprment.
  *SHOW GET m.Style  DISABLE
  IF llFromMA
    SHOW GET lcFabric DISABLE
    SHOW GET lcColor  DISABLE
    SHOW GET lcDesc   DISABLE
    SHOW GET pbFabric DISABLE
    SHOW GET pbColor  DISABLE
  ELSE
    SHOW GET m.Style  DISABLE
  ENDIF
  *E301797,1 (End)

  SHOW GET lnWHNo   DISABLE
  IF !laScrMode[1]
    SHOW GET ibTrap1 ENABLE
  ENDIF
  *E301797,1 (Begin) Get warehouse for Material shiprment.
  IF EMPTY(laWareM)
    SELECT cWareCode     ;  
    FROM   WAREHOUS      ;                                         
    INTO ARRAY laWareM
    SHOW GET lnMatWh
    lcWare = laWareM[1]
  ENDIF  
  *E301797,1 (End)

  *B126771,1 NNA 04/13/2005 (Begin) Show Window (lcWinC22) on top
  SHOW WINDOW (lcWinC22) TOP
  *B126771,1 NNA (End)
ENDIF

*:*************************************************************
*! Name      : lpShow
*! Purpose   : Doing whatever necessary when changing screen modes
*:*************************************************************
PROCEDURE lpShow


DO CASE

  *** S E L E C T  M O D E ***      
  CASE laScrMode[1]
  
    *C123616,1  TMI [Start] empty temp files used for Nik Nak
    IF ASCAN(laEvntTrig,PADR("EMPTMP",10)) <> 0
      =gfDoTriger("POSHP",PADR("EMPTMP",10))
    ENDIF
    *C123616,1  TMI [End  ] 

    lnLastFold  = lnActFolder
    lnActFolder = 1
    = lfChngFolder(lnActFolder)

    SHOW GET ibFolder(2) DISABLE
    lcStatus  = ' '
    lnRecvStk = 0
    lnRecvDam = 0
    lnRecvCan = 0

    *C102160,1 AMH Custom variable to hold the total in transit of shipment [Start]
    IF ASCAN(laEvntTrig,PADR("ADOPTGMA",10)) <> 0
      lnInTranst = 0
    ENDIF  
    *C102160,1 AMH [End]

    SELECT (lcShpLine)
    ZAP
    
    *B605208,1 AMH Ini. the total quantity of the whole of shipment [Start]
    STORE 0 TO lnTotQty
    *B605208,1 AMH [End]
    
    =lfActBrow1()
    *E301797,1 (Begin) Adjust for Material shiprment.
    *SELECT ShpmtHdr
    IF llFromMA
      SELECT MATSHPHD
      STORE '' TO lcFabric,lcColor,lcDesc,m.PO
      SHOW GET lcFabric
      SHOW GET lcColor
      SHOW GET lcDesc
    ELSE
      SELECT ShpmtHdr
    ENDIF  
    *E301797,1 (End)

    llNewShp = .F.
    SHOW GET pbDlt,1 PROMPT lcPromp DISABLE

    SHOW GETS WINDOW (lcWinC22) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh1) DISABLE ONLY
    SHOW GET ibTrap1 DISABLE
    
    *E301159,1  (WMA) To Fill The Array For ShipVia With "N/A" In Select Mode [Begin]
    lcCodeType = "N"
    *E301159,1  (WMA) [End]
    
  *** V I E W   M O D E ***      
  CASE laScrMode[2]
 
    SHOW GET ibFolder(2) ENABLE
    SELECT (lcShpLine)
    ZAP
    *:B038431,1 MHM 08/23/2004 Check if called from MA [Start]
    IF !llFromMA
    *:B038431,1 MHM  [End]
      lnRecvStk = ShpmtHdr.Recv_Stk
      lnRecvDam = ShpmtHdr.Recv_Dam
      lnRecvCan = ShpmtHdr.Recv_Can
    
    *:B038431,1 MHM 08/23/2004 Check if called from MA [Start]
    ELSE
      lnRecvStk = MATSHPHD.RecvStk
      lnRecvDam = MATSHPHD.RecvDam
      lnRecvCan = MATSHPHD.RecvCan
    
    ENDIF
    *:B038431,1 MHM  [End]

    *B605208,1 AMH Ini. the total quantity of the whole of shipment [Start]
    STORE 0 TO lnTotQty
    *B605208,1 AMH [End]
    *E301797,1 (Begin) Get Material shipment lines.
    IF llFromMA
      SELECT matshpdt
      SET ORDER TO TAG shipno
      IF SEEK(laData[1])
        SCAN WHILE ShipNo = laData[1] FOR TranCd = '3'
          SCATTER MEMVAR
          SELECT (lcShpLine)
          APPEND BLANK
          GATHER MEMVAR
          lnTotQty = lnTotQty + nFabTotQty
        ENDSCAN
      ENDIF
      lnRecvStk = MATSHPHD.RecvStk
      lnRecvDam = MATSHPHD.RecvDam
      lnRecvCan = MATSHPHD.RecvCan
    ELSE
    *E301797,1 (End)
      SELECT POsLn
      SET ORDER TO TAG POsLnSh
      IF SEEK(laData[1])
        *C102160,1 AMH Custom variable to hold the total in transit of shipment [Start]
        IF ASCAN(laEvntTrig,PADR("ADOPTGMA",10)) <> 0
          lnInTranst = 0
        ENDIF  
        *C102160,1 AMH [End]
        SCAN WHILE ShipNo = laData[1] FOR TranCd = '3'
          *C102160,1 AMH Custom variable to hold the total in transit of shipment [Start]
          IF ASCAN(laEvntTrig,PADR("ADOPTGMA",10)) <> 0
            lnInTranst = lnInTranst + TotQty
          ENDIF  
          *C102160,1 AMH [End]
          SCATTER MEMVAR
          SELECT (lcShpLine)
          APPEND BLANK
          GATHER MEMVAR
          *B605208,1 AMH calculate the total quantity of the whole shipment [Start]
          lnTotQty = lnTotQty + TotQty
          *B605208,1 AMH [End]
        ENDSCAN
      ENDIF
    *E301797,1 (Begin) Adjust for Material shiprment.
    ENDIF
    *E301797,1 (End)
      

    *C102160,4 AMH Fix incorrect data if select shipment in show summary 
    *C102160,4     or show recived lines [Start]

    *B125864,1 NNA 01/02/2005 (Begin) do this trigger in case not from Material Mo.
    *IF ASCAN(laEvntTrig,PADR("ADOPTGMA",10)) <> 0
    IF !llFromMA AND ASCAN(laEvntTrig,PADR("ADOPTGMA",10)) <> 0
    *B125864,1 NNA (End)
    
      =gfDoTriger("POSHP",PADR("ACTBAR",10))
    ENDIF  
    *C102160,4 AMH [End]

    SELECT (lcShpLine)
    GO TOP
    *B038431,1 MHM 08/23/2004 initialize array with 1 [Start]
    *lnWHNo=2
    lnWHNo=1
    *B038431  [End]
    SELECT Warehous
    SCAN
      DIME laWareH[lnWHNo]
      laWareH[lnWHNo] = cWareCode
      lnWHNo=lnWHNo+1 
    ENDSCAN
    lnWHNo=ASCAN(laWareH,cWareCode)
    lnWHNo= IIF(lnWHNo = 0,1,lnWHNo)
    SHOW GET lnWHNo DISABLE

    SELECT (lcShpLine)

    =lfGetInfo(@lcStatus)
    =lfActBrow1()
    llNewShp = .F.
    IF lcStatus <> 'O'
      SHOW GET pbDlt,1 PROMPT lcPromp DISABLE
    ELSE
      SHOW GET pbDlt,1 PROMPT lcPromp ENABLE
    ENDIF
    *:B802468,1 WAB ibtrap1 must be disable
    *:B802468,1 WAB - START
    *SHOW GET ibTrap1 ENABLE

    SHOW GET ibTrap1 DISABLE

    *:B802468,1 WAB - END

    *E301159,1  (WMA) To Fill The Array For ShipVia With "List" In View Mode [Begin]
    lcCodeType = "T"
    *E301159,1  (WMA) [End]

  *** E D I T   M O D E ***      
  CASE laScrMode[3]

    *B604737,1 KHM 07/30/2001 (Begin) Initializing the following variables
    STORE .T. TO llCUpdate,llUpdated
    *B604737,1 KHM 07/30/2001 (End)

    *C102160,1 AMH recollecting lines of lcShpLine if not come from show original lines [Start]
    *ash1
    *IF ASCAN(laEvntTrig,PADR("ADOPTGMA",10)) <> 0 .AND. lcShowMode <> 'ORG'
    IF !llFromMA AND ASCAN(laEvntTrig,PADR("ADOPTGMA",10)) <> 0 .AND. lcShowMode <> 'ORG'
      lcShowMode = 'ORG'
      SET MARK OF BAR 1 OF _OPTIONPOP TO .T.
      SET MARK OF BAR 2 OF _OPTIONPOP TO .F.
      SET MARK OF BAR 3 OF _OPTIONPOP TO .F.
      SELECT (lcShpLine)
      ZAP
      SELECT POsLn
      SET ORDER TO TAG POsLnSh
      IF SEEK(laData[1])
        SCAN WHILE ShipNo = laData[1] FOR TranCd = '3'
          SCATTER MEMVAR
          SELECT (lcShpLine)
          APPEND BLANK
          GATHER MEMVAR
        ENDSCAN
      ENDIF

      SELECT (lcShpLine)
      GO TOP
      =lfActBrow1()
      *C102160,4 AMH Refresh Screen fields [Start]
      =lfwBrow1()
      *C102160,4 AMH [End]
    ENDIF  
    *C102160,1 AMH [End]
    SHOW GETS WINDOW (lcWinCh1) ENABLE ONLY
    SHOW GET ibFolder(2) ENABLE
    _CUROBJ = OBJNUM(laData[3])
    =lfChngFolder(lnActFolder)
    llNewShp = .F.
    *:B802468,1 WAB ibtrap1 must be disable
    *:B802468,1 WAB - START
    *SHOW GET ibTrap1 ENABLE
    
    SHOW GET ibTrap1 DISABLE

    *:B802468,1 WAB - END
    
    *E301159,1  (WMA) To Fill The Array For ShipVia With "List" In Edit Mode [Begin]
    lcCodeType = "T"
    *E301159,1  (WMA) [End]
        
  *** A D D   M O D E ***      
  CASE laScrMode[4]

   *B604737,1 KHM 07/30/2001 (Begin) Initializing the following variables
   STORE .T. TO llCUpdate,llUpdated
   *B604737,1 KHM 07/30/2001 (End)

   *B605208,1 AMH Ini. the total quantity of the whole of shipment [Start]
   STORE 0 TO lnTotQty
   *B605208,1 AMH [End]

   *C102160,1 AMH restore show original lines [Start]
   *ash1
   *IF ASCAN(laEvntTrig,PADR("ADOPTGMA",10)) <> 0 .AND. lcShowMode <> 'ORG'
   IF !llFromMA AND ASCAN(laEvntTrig,PADR("ADOPTGMA",10)) <> 0 .AND. lcShowMode <> 'ORG'
     lcShowMode = 'ORG'
     SET MARK OF BAR 1 OF _OPTIONPOP TO .T.
     SET MARK OF BAR 2 OF _OPTIONPOP TO .F.
     SET MARK OF BAR 3 OF _OPTIONPOP TO .F.
   ENDIF  
   *C102160,1 AMH [End]
   IF EMPTY(laData[1])
 
      llNewShp = .T.
      SHOW GET laData[1] Enable
      _CUROBJ = OBJNUM(laData[1])
      SHOW GETS WINDOW (lcWinCh1) DISABLE ONLY
      SHOW GETS WINDOW (lcWinC22) DISABLE ONLY
      *wab
       lnLastFold  = lnActFolder
       lnActFolder = 1
       =lfChngFolder(lnActFolder)
       SHOW GET ibFolder(1) ENABLE
       SELECT (lcShpLine)
       *B607107    TMI [Start] zap only if lastmode is not Add mode
       IF lnLastMode <> 4
         ZAP         
       ENDIF
       *B607107    TMI [End  ] 
       =lfActBrow1()
       SCATTER MEMVAR BLANK
       STORE ' ' TO lcStyDesc
       
       *:B038431,1 MHM 08/23/2004 Check if called from MA [Start]
       *SELECT ShpmtHdr
       IF llFromMA
        SELECT MATSHPHD
       ELSE
         SELECT ShpmtHdr
       ENDIF  
       *:B038431,1 MHM  [End]
       
       SHOW GET pbDlt,1 PROMPT lcPromp DISABLE
       SHOW GETS WINDOW (lcWinC22) DISABLE ONLY
       SHOW GETS WINDOW (lcWinCh1) DISABLE ONLY
       SHOW GET ibTrap1 DISABLE
      *wab

    ELSE
      SHOW GET ibFolder(2) ENABLE
      lcStatus = 'OPEN'
      laData[2] = 'O'
      laData[3] = gdSysDate
      laData[6] = gdSysDate + 14
      laData[4] = 1
      SHOW GET laData[3]
      SHOW GET laData[4]
      SHOW GET laData[6]
      SHOW GET lcStatus
      SELECT (lcShpLine)
      *B607107    TMI [Start] zap only if lastmode is not Add mode
      IF lnLastMode <> 4
        ZAP
      ENDIF
      *B607107    TMI [End  ] 
      =lfActBrow1()

      IF llWareHous
        SELECT (lcShpLine)
        GO TOP
        
        *B038431,1 MHM 08/23/2004 initialize array with 1 [Start]
        *lnWHNo=2
        lnWHNo=1
        *B038431,1 MHM [End]
        
        SELECT Warehous
        SCAN
          DIME laWareH[lnWHNo]
          laWareH[lnWHNo] = cWareCode
          lnWHNo=lnWHNo+1 
        ENDSCAN
        lnWHNo=IIF(lnWHNo=0,1,lnWHNo)
        SHOW GET lnWHNo DISABLE
      ENDIF
    ENDIF
    
    *:B802468,1 WAB ibtrap1 must be disable & call lfChngFolder()
    *:B802468,1 WAB - START
    *SHOW GET ibTrap1 ENABLE
    =lfChngFolder(lnActFolder)
    SHOW GET ibTrap1 DISABLE

    *:B802468,1 WAB - END
        
    *E301159,1  (WMA) To Fill The Array For ShipVia With "Default" In Add Mode [Begin]
    lcCodeType = "D"
    *E301159,1  (WMA) [End]
     
ENDCASE

*E301159,1  (WMA) Call Function gfwCodePop() To Fill Code Array With Values [Begin]
= gfwCodePop ( @laCodInfo, "SHIPVIA"   ,lcCodeType )
laData[12]=laShpVia[lnShpVia,2]
*E301159,1  (WMA) [End]
=LFREFRESH(lcWinCh0)


*B607107    TMI [Start] Save last mode
lnLastMode = ASCAN(laScrMode,.T.)
*B607107    TMI [End  ] 


*:*************************************************************
*! Name      : lfvShpmnt
*! Purpose   : Validating the shipment number
*:*************************************************************
FUNCTION lfvShpmnt

llBrDone = .F.
IF !laScrMode[4]
  *E301797,1 (Begin) Adjust for Material shiprment.
  *IF llBrowse OR ( !EMPTY(laData[1]) AND !SEEK(laData[1],'ShpmtHdr') )
  *  lcShipNo=laData[1]
  *  SELECT ShpmtHdr
  IF llBrowse OR (!EMPTY(laData[1]) AND !SEEK(laData[1],IIF(llFromMA,'MATSHPHD','ShpmtHdr')))
    lcShipNo=laData[1]
    SELECT IIF(llFromMA,'MATSHPHD','ShpmtHdr')
  *E301797,1 (End)
    DO ShipBrow WITH lcShipNo
    llBrowse = .F.
    llBrDone = .T.
  ENDIF 

  IF !llBrDone .AND. !EMPTY(laData[1])
    laScrMode=.F.
    laScrMode[2]=.T.
    *E301797,1 (Begin) Make MATSHPHD the base file for browsing for Material shiprment.
    lcBaseFile  = ALIAS()
    *E301797,1 (End)
    =gfSeekRec()
    =lfGetInfo(@lcStatus)
    =lfGetLine()
    =lfRefresh()
  ENDIF
ELSE
  *:B802468,1   WAB - iF The LADATA[1] IS EMPTY SHOW ALL FILED DISABLED and 
  *:B802468,1   WAB - no need to let the ladata[1] is the current record
  *:B802468,1   WAB - start
  *IF EMPTY(laData[1]) .AND. LASTKEY() = 13
  *_CUROBJ  = _CUROBJ
  IF EMPTY(laData[1]) 
  
    SHOW GET laData[3] DISABLE
    SHOW GET laData[4] DISABLE
    SHOW GET laData[5] DISABLE
    SHOW GET laData[6] DISABLE
    SHOW GET laData[7] DISABLE
    SHOW GET laData[8] DISABLE
    *E301159,1  (WMA) Disable Fields [Begin]
    SHOW GET lnShpVia   DISABLE
    SHOW GET laData[9]  DISABLE
    SHOW GET laData[10] DISABLE
    SHOW GET laData[11] DISABLE
    SHOW GET laData[12] DISABLE
    SHOW GET laData[13] DISABLE
    SHOW GET laData[14] DISABLE
    *E301159,1  (WMA) [End]
    
  *:B802468,1   WAB - END

  ELSE

    *:B802468,1   WAB - No need for check if its not empty cause it already not empty
    *:B802468,1   WAB - start

    *IF !EMPTY(laData[1])

    *:B802468,1   WAB - END

     *E301797,1 (Begin)
     *IF SEEK(laData[1],'ShpmtHdr')
     IF SEEK(laData[1],IIF(llFromMA,'MATSHPHD','ShpmtHdr'))
     *E301797,1 (End) 
     
        =gfModalGen('INM34050B34000','DIALOG')

        *B802468,1 WAB - empty ladata[1] and return without SHOW GETS
        *B802468,1 WAB - no need to let the ladata[1] is the current record
        *B802468,1 - START
        *_CUROBJ  = _CUROBJ
        laData[1] = ''
        SHOW GET laData[1] ENABLE
        *B802468,1 - END        
      ELSE
        SHOW GET laData[1] Disable
        *:B802468,1   WAB - LADATA[1] not Found, SHOW ALL FILED DISABLED
        *:B802468,1   WAB - start
        
        SHOW GET laData[3] ENABLE
        SHOW GET laData[4] ENABLE
        SHOW GET laData[5] ENABLE
        SHOW GET laData[6] ENABLE
        SHOW GET laData[7] ENABLE
        SHOW GET laData[8] ENABLE

        *E301159,1  (WMA) Enable Fields [Begin]
        SHOW GET lnShpVia   ENABLE
        SHOW GET laData[9]  ENABLE
        SHOW GET laData[10] ENABLE
        SHOW GET laData[11] ENABLE
        SHOW GET laData[12] ENABLE
        SHOW GET laData[13] ENABLE
        SHOW GET laData[14] ENABLE
        *E301159,1  (WMA) [End]
        
        *:B802468,1   WAB - END
                
      ENDIF
      SHOW GETS

    *:B802468,1   WAB - No need for Endif cause we commet the if statment
    *:B802468,1   WAB - start

    *ENDIF

    *:B802468,1   WAB - END

  ENDIF
ENDIF


*:*************************************************************
*! Name      : lfGetInfo
*! Purpose   : Getting the necessary information after entering 
*!             a valid shipment number
*:*************************************************************
FUNCTION lfGetInfo
PARAMETER lcStatus

*E301797,1 (Begin) Select the proper file.
*SELECT ShpmtHdr
SELECT IIF(llFromMA,'MATSHPHD','ShpmtHdr')
*E301797,1 (End)

SCATTER FIELDS &lcScFields MEMO TO laData
DO CASE
  CASE laData[2] = 'O'
    lcStatus = 'OPEN'
  CASE laData[2] = 'C'
    lcStatus = 'COMPLETE'
  CASE laData[2] = 'X'
    lcStatus = 'CANCELED'
ENDCASE



*:*************************************************************
*! Name      : lfvDate
*! Purpose   : Validating Entered & ETA dates
*:*************************************************************
FUNCTION lfvDate
PRIVATE llInvalid,lcDatNam
llInvalid = .F.

IF EMPTY(laData[3]) .OR. EMPTY(laData[6])
  llInvalid = .T.

  lcDatNam = IIF(EMPTY(laData[3]),'entered','E.T.A.')
  *-You cannot leave E.T.A. / entered date empty.
  =gfModalGen('INM34005B00000','DIALOG',lcDatNam)
ENDIF


IF laData[3] > laData[6] AND !llInvalid
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
*! Name      : lfActBrow1
*! Purpose   : Original Browse function.
*!*************************************************************
FUNCTION lfActBrow1


lcSelect = SELECT()
SELECT (lcShpLine)
GO TOP
lnMarker = RECNO()

*C102160,1 AMH Custom process for ability to view shipment lines after receiving [Start]
*ash1
*IF ASCAN(laEvntTrig,PADR("ADOPTGMA",10)) <> 0 .AND. lcShowMode <> 'ORG'
IF !llFromMA AND ASCAN(laEvntTrig,PADR("ADOPTGMA",10)) <> 0 .AND. lcShowMode <> 'ORG'
  IF lcShowMode = 'SUM'
    lcBrD1fld= [cMarker =IIF(RECNO()=lnMarker ,'>',' '):H=' ':R:1:W=.F.,]+;
               [Style     :H='Style':R:29,]+;
               [TotQty    :H='Open Qty':R:14,]+;
               [TotOrd    :H='Received Qty':R:14]
  ELSE
    lcBrD1fld= [cMarker =IIF(RECNO()=lnMarker ,'>',' '):H=' ':R:1:W=.F.,]+;
               [Style     :H='Style':R:24,]+;
               [Qty1      :H='Size1':8:R,]+;
               [Qty2      :H='Size2':8:R,]+;
               [Qty3      :H='Size3':8:R,]+;
               [Qty4      :H='Size4':8:R,]+;
               [Qty5      :H='Size5':8:R,]+;
               [Qty6      :H='Size6':8:R,]+;
               [Qty7      :H='Size7':8:R,]+;
               [Qty8      :H='Size8':8:R,]+;
               [TotQty    :H='Total Quantity':R:10]
  ENDIF
ELSE
  *E301797,1 (Begin) Remove size breakdown in case of Material shipment.
  IF llFromMA  
    lcBrD1fld= [cMarker =IIF(RECNO()=lnMarker ,'>',' '):H=' ':R:1:W=.F.,]+;
               [POMAT     :H='PO #':R:8,]+;
               [Vendor    :H='Vendor':R:8,]+;
               [Fabric    :H='Fabric':R:7,]+;
               [Color     :H='Color' :R:6,]+;
               [lcDummy=IIF(SEEK(Fabric,'Fabric'),Fabric.Desc,SPACE(20)) :H='Fabric Description':R:20,]+;
               [Reference :H='Reference':R,]+;
               [nFabTotQty:H='Total Quantity':R:7,]+;
               [cWareCode :H='Warehouse':R:6]
  ELSE
  *E301797,1 (End)  
    lcBrD1fld= [cMarker =IIF(RECNO()=lnMarker ,'>',' '):H=' ':R:1:W=.F.,]+;
             [PO        :H='PO #':R:8,]+;
             [Vendor    :H='Vendor':R:8,]+;
             [Style     :H='Style#':R:12,]+;
             [lcDummy=IIF(SEEK(Style,'Style'),Style.Desc,SPACE(20)) :H='Style Description':R:20,]+;
             [Reference :H='Reference':R,]+;
             [Qty1      :H='Size1':5:R,]+;
             [Qty2      :H='Size2':5:R,]+;
             [Qty3      :H='Size3':5:R,]+;
             [Qty4      :H='Size4':5:R,]+;
             [Qty5      :H='Size5':5:R,]+;
             [Qty6      :H='Size6':5:R,]+;
             [Qty7      :H='Size7':5:R,]+;
             [Qty8      :H='Size8':5:R,]+;
             [TotQty    :H='Total Quantity':R:7,]+;
             [cWareCode :H='Warehouse':R:6,]+;
             [Prepak    :H='PP':2:R,]+;
             [PPQty     :H='PPQt':R]
  *E301797,1 (Begin) Remove size breakdown in case of Material shipment.           
  ENDIF
  *E301797,1 (End)
ENDIF  
*C102160,1 AMH [End]

*lcBWinTl=IIF(llZoom,lcWinC24,lcWinC21)

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
*! Name      : lfwBrowse
*! Purpose   : When Browse function.
*!*************************************************************
FUNCTION lfwBrow1
glFromBrow = .T.

*B605208,1 AMH Ini. the old value [Start]
IF lnMarker # RECNO()
  PRIVATE llUpdTot,lnI
  llUpdTot = .F.
  FOR lnI = 1 TO 8
    IF laOldQty[lnI] # 0
      llUpdTot = .T.
      EXIT
    ENDIF
  ENDFOR
  IF llUpdTot
    PRIVATE lcI
    FOR lnI = 1 TO 8
      lcI = STR(lnI,1)
      lnTotQty = lnTotQty + EVALUATE('m.Qty'+lcI) - laOldQty[lnI]
    ENDFOR
    SHOW GET lnTotQty
  ENDIF
  STORE 0 TO laOldQty
ENDIF
*B605208,1 AMH [End]

lnMarker = RECNO()
SHOW WINDOW (lcBrTtl1) REFRESH
=lfGetLine()

IF laScrMode[4] OR laScrMode[3]
  SHOW GET ibPO     DISABLE
  SHOW GET m.PO     DISABLE
  SHOW GET pbNew    ENABLE
  SHOW GET pbRemove ENABLE
  IF !lNewRec
    SHOW GET lnWHNo DISABLE
  ENDIF
ENDIF


*!*************************************************************
*! Name      : lfGetLine
*! Purpose   : Validate the line information.
*!*************************************************************
FUNCTION lfGetLine

SELECT (lcShpLine)
SCATTER MEMVAR

*C102160,4 AMH Get blank data for show summary mode [Start]

*B125864,1 NNA 01/02/2005 (Begin) do this trigger in case not from Material Mo.
*IF ASCAN(laEvntTrig,PADR("ADOPTGMA",10)) <> 0
IF !llFromMA AND ASCAN(laEvntTrig,PADR("ADOPTGMA",10)) <> 0
*B125864,1 NNA (End)

  IF lcShowMode = 'SUM'
    SCATTER MEMVAR BLANK
  ENDIF
ENDIF  
*C102160,4 AMH [End]
*E301797,1 (Begin) Adjust for Material shiprment.
IF llFromMA
  lcDesc  = IIF(SEEK(m.Fabric+m.Color,'Fabric'),Fabric.Desc,SPACE(20))
  lcFabric= m.Fabric
  lcColor = m.Color
  m.po    = pomat
ELSE
*E301797,1 (End)
  lcStyDesc = IIF(SEEK(m.Style,'STYLE'),STYLE.Desc,SPACE(20))
  llFoundScl=SEEK('S'+m.Scale,'Scale')
  lcSz1=IIF(llFoundScl,SCALE.SZ1,'')
  lcSz2=IIF(llFoundScl,SCALE.SZ2,'')
  lcSz3=IIF(llFoundScl,SCALE.SZ3,'')
  lcSz4=IIF(llFoundScl,SCALE.SZ4,'')
  lcSz5=IIF(llFoundScl,SCALE.SZ5,'')
  lcSz6=IIF(llFoundScl,SCALE.SZ6,'')
  lcSz7=IIF(llFoundScl,SCALE.SZ7,'')
  lcSz8=IIF(llFoundScl,SCALE.SZ8,'')
*C102160,4 AMH [End]
*E301797,1 (Begin) Adjust for Material shiprment.
ENDIF
*E301797,1 (End)

SHOW GETS WINDOW (lcWinC22) ONLY
IF llWareHous
  *E301797,1 (Begin) Adjust for Material shiprment.
  IF llFromMA
    lnMatWh = ASCAN(laWareH,m.cWareCode)
    lnMatWh = IIF(lnMatWh= 0,1,lnMatWh)
    SHOW GET lnMatWh
  ELSE
  *E301797,1 (End)
    lnWHNo = ASCAN(laWareH,m.cWareCode)
    *B603749,1 (Begin) Fix Invalid subcript reference.
    lnWHNo= IIF(lnWHNo = 0,1,lnWHNo)
    *B603749,1 (End)
    SHOW GET lnWHNo
  *E301797,1 (Begin) Adjust for Material shiprment.
  ENDIF
  *E301797,1 (End)
  
ENDIF
SHOW GET pbKey ENABLE
lnMarker = RECNO()
SHOW WINDOW (lcBrTtl1) REFRESH SAME
=lfRefresh(lcWinC22)

*:*************************************************************
*! Name      : lfwWHous
*! Purpose   : Force the validation of the line warehouse.
*:*************************************************************
FUNCTION lfwWHous
lcOldWare = laWareH[lnWHNo]



*:*************************************************************
*! Name      : lfvWHous
*! Purpose   : Valid line warehouse.
*:*************************************************************
FUNCTION lfvWHous
m.cWareCode = laWareH[lnWHNo]
IF !SEEK(m.Style+m.cWareCode+SPACE(10),'STYDYE')
  *-Style: xxx/xxx is not assigned to warehouse: xxx. "\<Add;\<Reenter"
  IF gfModalGen('QRM34048B34004','DIALOG',ALLTRIM(m.Style)+'|'+m.cWareCode) = 1
    DO gpAdStyWar WITH m.Style,SPACE(10),m.cWareCode
  ELSE
    m.cWareCode=lcOldWare
  ENDIF
ENDIF
REPLACE &lcShpLine..cWareCode WITH m.cWareCode
lnWHNo = ASCAN(laWareH,m.cWareCode)
SHOW GET lnWHNo ENABLE

SHOW WINDOW (lcBrTtl1) REFRESH SAME

*:*************************************************************
*! Name      : lfTrap
*! Purpose   : Traping necessary keys
*:*************************************************************
FUNCTION lfTrap
IF WONTOP() = lcBrTtl1

  ON KEY LABEL TAB        DO lfTab
  ON KEY LABEL BACKTAB    DO lfShiftTab
  *ON KEY LABEL ESC        DO gfCPClose
  ON KEY LABEL Ctrl+ENTER lnDummy = 1
  ON KEY LABEL Ctrl+HOME  lnDummy = 1
  ON KEY LABEL Ctrl+W     lnDummy = 1
  ON KEY LABEL Ctrl+END   lnDummy = 1
  glFromBrow = .T.
ENDIF  


*:*************************************************************
*! Name      : lfUnTrap
*! Purpose   : Untrap the trapped keys
*:*************************************************************
FUNCTION lfUnTrap
IF WONTOP() # lcBrTtl1
  ON KEY LABEL TAB
  ON KEY LABEL BACKTAB
  ON KEY LABEL Ctrl+ENTER
  ON KEY LABEL Ctrl+HOME
  ON KEY LABEL Ctrl+END
  ON KEY LABEL Ctrl+W
  *ON KEY LABEL ESC &lcEscTrap
  glFromBrow = .F.
ENDIF  

*C123616,1  TMI [Start] After we come from pocrhdr screen we need to refreh the shipment detail browse
IF ASCAN(laEvntTrig,'REFBRW') <> 0 AND FILE(gcWorkDir+'TEMPNAME.TMP')
  =gfDoTriger('POSHP','REFBRW')
ENDIF
*C123616,1  TMI [End  ] 

*:*************************************************************
*! Name      : lfTab
*! Purpose   : Traping the TAB key
*:*************************************************************
FUNCTION lfTab
*IF WONTOP(lcBrTtl1)
  *ACTIVATE WINDOW gwcContrl1
  ACTIVATE WINDOW (lcWinC22)
  _CUROBJ=OBJNUM(ibPO)
*ELSE
*  ACTIVATE WINDOW (lcBrTtl1)
*ENDIF
RETURN

*:*************************************************************
*! Name      : lfShiftTab
*! Purpose   : Traping the SHIFT+TAB key
*:*************************************************************
FUNCTION lfShiftTab
*IF WONTOP(lcBrTtl1)
  ACTIVATE WINDOW (lcfolder)
  _CUROBJ = OBJNUM(ibFolder(2))
*ELSE
*  ACTIVATE WINDOW (lcBrTtl1)
*ENDIF
RETURN


*!*************************************************************
*! Name      : lfCkEOF
*! Purpose   : Function if there is lines for P/O.
*!*************************************************************
FUNCTION lfCkEOF
PARA lcCkFile
lnAlias=SELECT()
SELECT (lcCkFile)
IF EOF()
  GO TOP
  lnRet = EOF()
ELSE
  lnRet = .F.
ENDIF
SELECT(lnAlias)
RETURN lnRet

*!*************************************************************
*! Name      : lfvPO
*! Purpose   : Function to validate the entered PO #
*!*************************************************************
FUNCTION lfvPO
PRIVATE llInvalid,llPresOk,llFirst
STORE .F. TO llPresOk,llInvalid
llFirst   = .T.
IF llBrowse OR ( !EMPTY(m.PO) AND !SEEK('P'+m.PO,'POSHDR') )
  lcPo=m.PO
  SELECT POSHDR
  SET FILTER TO cStyType='P'
  DO POSBrow WITH lcPo,'        ','P'
  m.PO=lcPo
  llBrowse = .F.
ENDIF 

IF EMPTY(m.PO) 
  llInvalid = .T.
ENDIF

IF !llInvalid AND PosHdr.Status $ 'CSX'
  *--PO status is either complete, canceled, or closed. Cannot proceed.
  =gfModalGen('TRM34131B00000','DIALOG')
  llInvalid = .T.
ENDIF

SET ORDER TO TAG PosLn IN PosLn
IF !llInvalid AND !SEEK('P'+m.PO,'PosLn')
  *--The lines for this PO have not been found. Cannot proceed.
  =gfModalGen('TRM34132B00000','DIALOG')
  llInvalid = .T.
ENDIF

IF llInvalid 
  RETURN
ENDIF

lcLin_Ttl = "PO Lines"

*B803680,1 ABD Remove the order from the POSLN FILE to speed up the performance. [Begin]
SELECT POSLN
lcOldOrder = ORDER()

*B603936,1 KHM 10/02/2000 (Begin) Commenting the following lines and changing
*B603936,1                the select SQL to be SEEK and scan for more optimization.
*SET ORDER TO
*B803680,1 ABD [End]

*B603793,1 ABD Add fileds (nOpenQty,nRecevQty,ntransQty) to browse the remainder from the PO. [Begin]
*SELECT *,'' AS cMarker,'' AS cStatus FROM POSLN;
*  WHERE cStyType+PO+Style+STR(LineNo,6)+TranCd+STR(RECNO(),7) = ;
*        'P'+m.PO+Style+STR(LineNo,6)+'1'+STR(RECNO(),7);
*  INTO DBF (gcWorkDir+lcTmpPoLn)
*B603793,1 ABD [End]

*SELECT *,'' AS cMarker,'' AS cStatus,POSLN.TotQty AS nOpenQty ,;
   0000000 AS nRecevQty,0000000 AS ntransQty FROM POSLN;
  WHERE cStyType+PO+Style+STR(LineNo,6)+TranCd+STR(RECNO(),7) = ;
        'P'+m.PO+Style+STR(LineNo,6)+'1'+STR(RECNO(),7);
  INTO DBF (gcWorkDir+lcTmpPoLn)

SELECT (lcTmpPoLn)
ZAP
SELECT POSLN
=SEEK('P'+m.PO)
SCAN REST WHILE cStyType+PO+Style+STR(LineNo,6)+TranCd = 'P'+m.PO FOR TranCd = '1'
  WAIT WINDOW 'Collecting P/O lines :'+POSLN.Style NOWAIT
  SCATTER MEMVAR 
  SELECT (lcTmpPoLn)
  APPEND BLANK
  GATHER MEMVAR  
  REPLACE nOpenQty WITH POSLN.TotQty
ENDSCAN
WAIT CLEAR

*IF !FILE(gcWorkDir+lcTmpPoLn)
  *B603793,1 ABD Add lineNo Filed to the index to speed the search . [Begin]
  *INDEX ON cStyType+PO+Style+Trancd+STR(RECNO(),7) TAG (lcTmpPoLn) OF (gcWorkDir+lcTmpPoLn)
*  INDEX ON cStyType+PO+Style+Trancd+STR(LineNo,6)+STR(RECNO(),7) TAG (lcTmpPoLn) OF (gcWorkDir+lcTmpPoLn)  
  *B603793,1 ABD [End]
*ENDIF
SELECT (lcTmpPoLn)
*B603936,1 KHM 10/02/2000 (End)

GO TOP
*B603793,1 ABD Accumulate the remainder from the PO in filed nOpenQty to browse it when 
*B603793,1 ABD Select the lines from browse screen. [Begin]
SELECT POSLN

*B803680,1 ABD Return the old order to the POSLN FILE. [Begin]
SET ORDER TO &lcOldOrder
*--Till user message that he select lines for this PO. [Begin]
WAIT WINDOW 'Please wait .... Collecting data' NOWAIT
*B803680,1 ABD [End]


*B603839,1 ABD Speed up the performance when select the po .[Begin]
*SCAN FOR cStyType+PO+Style+STR(LineNo,6)+TranCd+STR(RECNO(),7) = 'P'+m.PO 
IF SEEK('P'+m.PO)
  SCAN REST WHILE cStyType+PO+Style+STR(LineNo,6)+TranCd+STR(RECNO(),7) = 'P'+m.PO
    *B603839,1 ABD [END]
    IF Trancd # '1'
      DO CASE
         CASE Trancd = '2' .OR. Trancd = '4' .OR. Trancd = '5'
           IF SEEK('P'+m.PO+Style+'1'+STR(LineNo,6),lcTmpPoln)
             SELECT (lcTmpPoln)
             REPLACE nRecevQty With nRecevQty + Posln.Totqty
           ENDIF
         CASE Trancd = '3'
           IF SEEK('P'+m.PO+Style+'1'+STR(LineNo,6),lcTmpPoln)
             SELECT (lcTmpPoln)
             REPLACE ntransQty With ntransQty + Posln.Totqty
           ENDIF
       ENDCASE

       *B804301,4 MHM  07/24/2001 Check for open Qty [Start]
       *REPLACE nOpenQty  With nOpenQty  - Posln.Totqty
       FOR lnCntr = 1 TO 8
         lcCntr = STR(lnCntr,1)
         REPLACE Qty&lcCntr WITH MAX(Qty&lcCntr - PosLn.Qty&lcCntr,0)
       ENDFOR
       REPLACE nOpenQty  With Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
       *B804301,4 MHM  07/24/2001 [End]

       SELECT POSLN 
    ENDIF    
  ENDSCAN
  *B603839,1 End for if statement.[Begin]
  
  *B803680,1 (Begin) Clear the wait window
  WAIT CLEAR
  *B803680,1 (End)
ENDIF  
*B603839,1 [End]
*B603793,1 ABD [End]


*B803568,1 ABD accumulate the open Qty from the po. [Begin]
SELECT (lcTmpPoln)
*B804301,1 MHM  Changing the check of the open quantity [START]   
*lnShpQty = 0
*SUM nOpenQty To lnShpQty For cStyType+PO+Style+STR(LineNo,6)+TranCd+STR(RECNO(),7) = 'P'+m.PO
*IF lnShpQty <= 0
SCAN
  IF nOpenQty <= 0
    DELETE
  ENDIF
ENDSCAN
LOCATE
IF EOF()
*B804301,1 MHM  07/16/2001[End]
  *B802614,1 Message        : xx ,Cannot proceed.
  *B802614,1 Message No.    : 42080.
  *B802614,1 Button Message : OK
  *B802614,1 Button         : 00000
  = gfModalGen('TRM42080B00000','DIALOG','This PO was shipped before.')
  m.PO = ''
  SHOW GET m.PO
  _CUROBJ=OBJNUM(m.po)
  RETURN
ENDIF  
*B803568,1 [End]

*E301797,1 (Begin) Move the screen to tha main screen folder.
*DO (gcScrDir+gcWinAppl+"\POSEL.SPX")
DO (gcScrDir+"POSEL.SPX")
*E301797,1 (End)


IF llPresOk
  SHOW WINDOW (lcBrTtl1) REFRESH

  *B605208,1 AMH refresh the total quantity of the whole of shipment [Start]
  SHOW GET lnTotQty
  *B605208,1 AMH [End]
    
  IF !llFirst
    IF llWareHous
      lnWHNo = ASCAN(laWareH,&lcShpLine..cWareCode)
    ENDIF
    SHOW GETS WINDOW (lcWinC22) ENABLE ONLY
    =lfGetLine()
    
    SHOW GET ibPO    DISABLE
    SHOW GET m.PO    DISABLE
    SHOW GET ibStyle DISABLE
    SHOW GET m.Style DISABLE

    _CUROBJ = IIF(llWareHous,OBJNUM(lnWHNo),OBJNUM(m.Prepak))

  ENDIF

  
ENDIF



*!*************************************************************
*! Name      : lfvSelec
*! Purpose   : Select a record or select all records
*!*************************************************************
FUNCTION lfvSelec
PARAMETER llAll
PRIVATE lnNewRec 

lnAlias = SELECT()
SELECT (lcTmpPoLn)

*B603793,1 ABD Remark the next line .[Begin]
*lnNewRec = RECNO()
*B603793,1 ABD [End]

IF llAll
  *B603793,1 ABD Select all recored that open Qty > 0  only.[Begin]
  *REPLACE ALL cStatus WITH '»'
  *B603839,1 ABD Speed up the performance. [Begin]
  *SCAN
  *  IF nOpenQty # 0
  *    REPLACE cStatus WITH '»'
  *  ENDIF
  *ENDSCAN 
  *B603793,1 ABD Replace all record that nOpenQty > 0 to speed up the Performance.  [Begin]
  REPLACE ALL cStatus WITH '»' FOR nOpenQty > 0
  *B603839,1 ABD [End]
ELSE
  REPLACE cStatus WITH '»'
ENDIF
SHOW WINDOW (lcLin_Ttl) REFRESH

SHOW GET pbUSele  ENABLE 
SHOW GET pbUSeleA ENABLE 
SHOW GET pbSele  DISABLE 
SHOW GET pbSeleA DISABLE 

*B603793,1 ABD Remark the next line and go to the first recored selected. [Begin]
*GO lnNewRec
Locate FOR !EMPTY(cStatus)
*B603793,1 ABD [End]
SELECT (lnAlias)


*!*************************************************************
*! Name      : lfvUSelec
*! Purpose   : Unselect a record or select all records
*!*************************************************************
FUNCTION lfvUSelec
PARAMETER llAll
PRIVATE lnNewRec 

lnAlias = SELECT()
SELECT (lcTmpPoLn)
lnNewRec = RECNO()
IF llAll
  REPLACE ALL cStatus WITH SPACE(01)
ELSE
  REPLACE cStatus WITH SPACE(01)
ENDIF
SHOW WINDOW (lcLin_Ttl) REFRESH
SHOW GET pbSele  ENABLE 
SHOW GET pbSeleA ENABLE 
SHOW GET pbUSele  DISABLE 
SHOW GET pbUSeleA DISABLE 
SCAN
  IF !EMPTY(cStatus)
    SHOW GET pbUSele  ENABLE 
    SHOW GET pbUSeleA ENABLE 
    EXIT
  ENDIF
ENDSCAN
GO lnNewRec
SELECT (lnAlias)
RETURN


*!*************************************************************
*! Name      : lfSelLins
*! Purpose   : Browse the selected PO lines to choose the shipment lines
*!*************************************************************
FUNCTION lfSelLins
lnAlias = SELECT()

lnMarker = RECNO()
*B603793,1 ABD Browse the remainder Qty from the PO line Qty in filed nOpenQty when 
*B603793,1 ABD Select the lines from browse screen. [Begin]
*lcBrflds =   [cMarker :H=' ':R:1:W=.F.,]+;
             [cStatus :H='',]+;
             [Style   :H='Style':R,]+;
             IIF(llWareHous,[cWareCode :H='Ware.':R,],[])+;
             [Reference :H='Reference':R,]+;
             [TOTQTY:7:H='Quantity':P='99999999':R,] +;
             [lnAmt=(TotQty*nCost1):11:H='Amount':P='999999999.999':R]
*B603839,1 ABD Fit the browse screen to see all the scrren. 
*B603839,1 ABD & Browse Zero in filed OpenQty If Less than 
*B603839,1 ABD Zero upon the user over recive the Qty. [Begin]
*lcBrflds =   [cMarker :H=' ':R:1:W=.F.,]+;
             [cStatus :H='',]+;
             [Style   :H='Style':R,]+;
             IIF(llWareHous,[cWareCode :H='Ware.':R,],[])+;
             [Reference :H='Reference':R,]+;
             [nOpenQty  :6:H='Open'      :P='99999999':R,] +;
             [nRecevQty :9:H='Received'  :P='99999999':R,] +;
             [ntransQty :11:H='In Transit':P='99999999':R,] +;
             [TOTQTY    :9:H='Original'  :P='99999999':R,] +;
             [lnAmt=(TotQty*nCost1):11:H='Amount':P='999999999.999':R]

*E301797,1 (Begin) Remove size breakdown in case of Material shipment.
IF llFromMA  
  lcBrflds =   [cMarker :H=' ':R:1:W=.F.,]+;
               [cStatus :H='',]+;
               [Fabric  :H='Fabric':R,]+;
               [Color   :H='Color':R,]+;             
               IIF(llWareHous,[cWareCode :H='Ware.':R,],[])+;
               [Reference :20:H='Reference':R,]+;
               [nFabTotQty:8:H='Original'  :P='999999999.999':R,] +;             
               [lnOpenQty = MAX(nOpenQty,0) :5:H='Open'      :P='99999999':R,] +;
               [nRecevQty :8:H='Received'  :P='99999999':R,] +;
               [ntransQty :10:H='In Transit':P='99999999':R,] +;
               [lnAmt=(nFabTotQty*nCost1):11:H='Amount':P='999999999.999':R]
ELSE
*E301797,1 (End)  
  lcBrflds =   [cMarker :H=' ':R:1:W=.F.,]+;
               [cStatus :H='',]+;
               [Style   :H='Style':R,]+;
               IIF(llWareHous,[cWareCode :H='Ware.':R,],[])+;
               [Reference :20:H='Reference':R,]+;
               [TOTQTY    :8:H='Original'  :P='99999999':R,] +;             
               [lnOpenQty = MAX(nOpenQty,0) :5:H='Open'      :P='99999999':R,] +;
               [nRecevQty :8:H='Received'  :P='99999999':R,] +;
               [ntransQty :10:H='In Transit':P='99999999':R,] +;
               [lnAmt=(TotQty*nCost1):11:H='Amount':P='999999999.999':R]
*E301797,1 (Begin) Remove size breakdown in case of Material shipment.           
ENDIF
*E301797,1 (End)

*B603839,1 ABD [End]             
*B603793,1 ABD [End]
SELECT(lcTmpPoLn)
GO TOP
*:B802468,1 WAB - If there is no record disable (select & select all ) buttons
*:B802468,1 WAB - START
IF RECCOUNT() > 0 
  lcObjDisp = 'ENABLE'
ELSE
  lcObjDisp = 'DISABLE'
ENDIF
*:B802468,1 WAB - END


*B603793,1 ABD Enable the buttom select in case of RECCOUNT() Andopen Qty > 0 .[Begin]
IF  RECCOUNT() > 0 .AND. nOpenQty > 0 
  lcOpenQty = 'ENABLE'
ELSE
  lcOpenQty = 'DISABLE'
ENDIF
*B603793,1 ABD [End]

BROWSE FIELDS &lcBrflds ;
       LOCK 0   ;
       NOAPPEND ;
       NOCLEAR  ;
       NOEDIT   ;       
       NODELETE ;
       NOMENU   ;
       NOWAIT   ;
       SAVE     ;
       WHEN lfwPOl();
       TITLE lcLin_Ttl    ;
       WINDOW POSel1 IN WINDOW POSel

ACTIVATE WINDOW (lcLin_Ttl) 

SELECT (lnAlias)



*!*************************************************************
*! Name      : lfvTotQty
*! Purpose   : Total quantity validation
*!*************************************************************
FUNCTION lfvTotQty

*E301797,1 (Begin) Adjust for Material shiprment.
IF llFromMA
  lcKey = EVAL(KEY())
  REPLACE NFabTotQty WITH m.NFabTotQty
  SUM NFabTotQty TO lnTotQty 
  =SEEK(lcKey)
  REPLACE TOTQTY     WITH m.NFabTotQty,;
          nFabTOTQTY WITH m.NFabTotQty
          
  SHOW GETS WINDOW (lcWinC22) ONLY
  SHOW WINDOW (lcBrTtl1) REFRESH SAME
  =lfRefresh(lcWinC22)
  SHOW GET lnTotQty
  FLUSH
  RETURN
ENDIF
*E301797,1 (End)

m.Scale = &lcShpLine..Scale
IF EMPTY(m.Prepak)
  RETURN
ENDIF
IF !EMPTY(m.Prepak) .AND. SEEK('P'+m.Scale+m.Prepak,'Scale')
  IF MOD(m.TotQty,Scale.PPTot) <> 0 
    *-Total qty is not evenly divisible by the prepak qty 9999.
    =gfModalGen('TRM34044B34000','DIALOG',STR(Scale.PPTot,4) )         
    =SEEK('S'+m.Scale,'Scale')
    _CUROBJ = OBJNUM(m.Prepak)
    RETURN
  ENDIF

  *B605208,1 AMH Use lfvPPakQty function to update the qty fields correctly [Start]
  *m.Qty1 = m.TotQty*Scale.PP1/Scale.PPTot
  *m.Qty2 = m.TotQty*Scale.PP2/Scale.PPTot
  *m.Qty3 = m.TotQty*Scale.PP3/Scale.PPTot
  *m.Qty4 = m.TotQty*Scale.PP4/Scale.PPTot
  *m.Qty5 = m.TotQty*Scale.PP5/Scale.PPTot
  *m.Qty6 = m.TotQty*Scale.PP6/Scale.PPTot
  *m.Qty7 = m.TotQty*Scale.PP7/Scale.PPTot
  *m.Qty8 = m.TotQty*Scale.PP8/Scale.PPTot
  m.PPQty = m.TotQty/Scale.PPTot
  =lfvPPakQty()
  *B605208,1 AMH [End]
  
ENDIF
=SEEK('S'+m.Scale,'Scale')
*IF m.Qty1=&lcShpLine..Qty1 AND m.Qty2=&lcShpLine..Qty2 AND m.Qty3=&lcShpLine..Qty3 AND m.Qty4=&lcShpLine..Qty4 AND ;
*   m.Qty5=&lcShpLine..Qty5 AND m.Qty6=&lcShpLine..Qty6 AND m.Qty7=&lcShpLine..Qty7 AND m.Qty8=&lcShpLine..Qty8 AND m.TotQty=&lcShpLine..TotQty
*  RETURN
*ENDIF

FLUSH


*!*************************************************************
*! Name      : FUNCTION lfvSizeQty
*! Purpose   : Validate Style size quantity
*!*************************************************************
FUNCTION lfvSizeQty
PARAMETERS lnSize
lcSz = STR(lnSize,1)


IF m.Qty&lcSz<0
  *-Cannot accept -ve quantity.
  =gfModalGen('TRM34051B34000','DIALOG')
  m.Qty&lcSz = &lcShpLine..Qty&lcSz
  RETURN .F.
ENDIF
IF m.Qty&lcSz = &lcShpLine..Qty&lcSz
  IF lnSize = Scale.Cnt 
    IF m.TotQty <> &lcShpLine..TotQty .AND. ;
      gfModalGen('QRM34045B34001','DIALOG',STR(m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8,5)) = 2
      *-Quantities out of balance! 9999 Pieces ok?','\<Yes;\?\<No'
      
      *B605208,1 AMH Restore the Old Data [Start]
      =lfRestOld()
      *B605208,1 AMH [End]
      
      _CUROBJ = OBJNUM(m.Prepak)
    ELSE
    
      *B605208,1 AMH Ini. the old value [Start]
      PRIVATE llUpdTot,lnI
      llUpdTot = .F.
      FOR lnI = 1 TO 8
        IF laOldQty[lnI] # 0
          llUpdTot = .T.
          EXIT
        ENDIF
      ENDFOR
      IF llUpdTot
        PRIVATE lcI
        FOR lnI = 1 TO 8
          lcI = STR(lnI,1)
          lnTotQty = lnTotQty + EVALUATE(lcShpLine+'.QTY'+lcI) - laOldQty[lnI]
        ENDFOR
        SHOW GET lnTotQty
      ENDIF
      STORE 0 TO laOldQty
      *B605208,1 AMH [End]
      
      m.TotQty = &lcShpLine..TotQty
      SHOW GET m.TotQty
    ENDIF
  ENDIF
  RETURN
ENDIF

SELECT (lcShpLine)

*B605208,1 AMH Save the old data [Start]
=lfSaveOld()
*B605208,1 AMH [End]

REPLACE Qty&lcSz WITH m.Qty&lcSz ,;
        TotQty   WITh Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8 

IF lnSize = Scale.Cnt 
  IF m.TotQty <> &lcShpLine..TotQty .AND. ;
      gfModalGen('QRM34045B34001','DIALOG',STR(m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8,5)) = 2
      *-Quantities out of balance! 9999 Pieces ok?','\<Yes;\?\<No'
    
    *B605208,1 AMH Restore the Old Data [Start]
    =lfRestOld()
    *B605208,1 AMH [End]
    
    _CUROBJ = OBJNUM(m.Prepak)
  ELSE
  
    *B605208,1 AMH Ini. the old value [Start]
    PRIVATE llUpdTot,lnI
    llUpdTot = .F.
    FOR lnI = 1 TO 8
      IF laOldQty[lnI] # 0
        llUpdTot = .T.
        EXIT
      ENDIF
    ENDFOR
    IF llUpdTot
      PRIVATE lcI
      FOR lnI = 1 TO 8
        lcI = STR(lnI,1)
        lnTotQty = lnTotQty + EVALUATE(lcShpLine+'.QTY'+lcI) - laOldQty[lnI]
      ENDFOR
      SHOW GET lnTotQty
    ENDIF
    STORE 0 TO laOldQty
    *B605208,1 AMH [End]
    
    m.TotQty = &lcShpLine..TotQty
    SHOW GET m.TotQty
  ENDIF
ENDIF
SHOW WINDOW (lcBrTtl1) REFRESH SAME



*!*************************************************************
*! Name      : FUNCTION lfvPrePak
*! Purpose   : Validate Style prepak
*!*************************************************************
FUNCTION lfvPrePak

m.Scale = &lcShpLine..Scale
IF (!EMPTY(m.Prepak) .AND. !SEEK('P'+m.Scale+m.Prepak,'Scale'))
  =gfPrePBrow(m.Scale,@m.Prepak)
  
  *B605208,1 AMH Comment the next line [Start]
  *=SEEK('S'+m.Scale,'Scale')
  *B605208,1 AMH [End]

ENDIF

*B605208,1 AMH restore the correct record in scale file [Start]
=SEEK('S'+m.Scale,'Scale')
*B605208,1 AMH [End]

IF &lcShpLine..Prepak <> m.Prepak

*B802468,1 WAB - Logic Variable True if user change the prepack 
*B802468,1 WAB - START
llPrePak = .T.
=lfvPPakQty()
*B802468,1 WAB - END

  REPLACE &lcShpLine..Prepak WITH m.Prepak
  IF EMPTY(&lcShpLine..Prepak)
    REPLACE &lcShpLine..PPQty WITH 0
    m.PPQTY = 0
    SHOW GET m.PPQty
  ELSE
    SHOW GET m.Prepak ENABLE
  ENDIF
  SHOW GETS WINDOW (lcWinC22) ONLY
  SHOW WINDOW (lcBrTtl1) REFRESH SAME
  FLUSH

*B802468,1 WAB - Logic Variable False Check if user didn't change the prepack 
*B802468,1 WAB - START
ELSE
  llPrePak = .F.
*B802468,1 WAB - END
  
ENDIF

*!*************************************************************
*! Name      : FUNCTION lfvPPakQty
*! Purpose   : Validate Style prepak quantity
*!*************************************************************
FUNCTION lfvPPakQty

m.Scale = &lcShpLine..Scale

*B802468,1 WAB - if user didn't change the prepack and the prepack qty
*B802468,1 WAB - START
*IF m.PPQty = &lcShpLine..PPQty 

IF m.PPQty = &lcShpLine..PPQty  and !llPrePak

*B802468,1 WAB - END

  RETURN
ENDIF

IF !EMPTY(m.Prepak) AND m.PPQty <> 0
  =SEEK('P'+m.Scale+m.Prepak,'Scale')
  m.Qty1   = m.PPQty*Scale.PP1
  m.Qty2   = m.PPQty*Scale.PP2
  m.Qty3   = m.PPQty*Scale.PP3
  m.Qty4   = m.PPQty*Scale.PP4
  m.Qty5   = m.PPQty*Scale.PP5
  m.Qty6   = m.PPQty*Scale.PP6
  m.Qty7   = m.PPQty*Scale.PP7
  m.Qty8   = m.PPQty*Scale.PP8
  m.TotQty = m.PPQty*Scale.PPTot
  SHOW GET m.TotQty
  =SEEK('S'+m.Scale,'Scale')

  *B605208,1 AMH Ini. the old value [Start]
  STORE 0 TO laOldQty
  *B605208,1 AMH [End]

  *B802468,1 WAB - replace ppqty in temp file with new value & call lfvsizeQty()
  *B802468,1 - START

  REPLACE Ppqty WITH m.PpQty
  FOR lnI = 1 To Scale.Cnt
    =lfvSizeQty(lnI)
  ENDFOR

  *B802468,1 - END

ELSE
  STORE 0 TO m.TotQty
  SHOW GET m.TotQty
ENDIF

SHOW GETS WINDOW (lcWinC22) ONLY
SHOW WINDOW (lcBrTtl1) REFRESH SAME
=lfRefresh(lcWinC22)
FLUSH
RETURN



*!*************************************************************
*! Name      : lfvNew
*! Purpose   : New shipment line
*!*************************************************************
FUNCTION lfvNew

SELECT (lcShpLine)
SCATTER MEMVAR BLANK
STORE ' ' TO lcStyDesc
*E301797,1 (Begin) Adjust for Material shiprment.
*SHOW GET m.Style  DISABLE
IF llFromMA
  STORE '' TO lcFabric,lcColor,lcDesc,m.PO
  SHOW GET lcFabric
  SHOW GET lcColor
  SHOW GET lcDesc
ENDIF
*E301797,1 (End)

SHOW GETS WINDOW (lcWinC22) DISABLE ONLY
SHOW GET ibPO    ENABLE
SHOW GET m.PO    ENABLE
=lfRefresh(lcWinC22)
_CUROBJ = OBJNUM(m.PO)
RETURN


*!*************************************************************
*! Name      : lfvRemove
*! Purpose   : Remove shipment line.
*!*************************************************************
FUNCTION lfvRemove
IF gfModalGen('QRM34036B34001','DIALOG') = 2
  *-Are you sure you want to delete this line?
  RETURN
ENDIF

SELECT (lcShpLine)

*B605208,1 AMH recalculate the total quantity of the whole of shipment [Start]
lnTotQty = lnTotQty - TotQty
SHOW GET lnTotQty
*B605208,1 AMH [End]

DELETE
PACK
GO TOP
lcDispDet = IIF(EOF(),'DISABLE','')
SCATTER MEMVAR
*E301797,1 (Begin) Adjust for Material shiprment.
IF llFromMA
  lcDesc  = IIF(SEEK(m.Fabric+m.Color,'Fabric'),Fabric.Desc,SPACE(20))
  lcFabric= m.Fabric
  lcColor = m.Color
  m.PO = ''
ENDIF
*E301797,1 (End)
=lfActBrow1()
IF EOF()
  STORE ' ' TO lcStyDesc
ENDIF

SHOW GETS WINDOW (lcWinC22) &lcDispDet ONLY
IF EOF()
  SHOW GET pbNew ENABLE 
ENDIF
RETURN


*!*************************************************************
*! Name      : lfwPOl
*! Purpose   : Preparing the PO lines browse 
*!*************************************************************
FUNCTION lfwPOl
PRIVATE lnNewRec 
lnAlias = SELECT()
SELECT (lcTmpPoLn)
lnNewRec = RECNO()

REPLACE ALL cMarker WITH ' '
GO lnNewRec
REPLACE cMarker WITH '>'
IF cStatus = '»'
  SHOW GET pbUSele  ENABLE 
  SHOW GET pbUSeleA ENABLE 
  SHOW GET pbSele  DISABLE 
  SHOW GET pbSeleA DISABLE 
ELSE
  SHOW GET pbUSele  DISABLE
  SHOW GET pbUSeleA DISABLE 
  *B603793,1 ABD Enable Select Buttom if open Qty > 0  only & diable if open Qty <= 0. [Begin]
  IF nOpenQty > 0 
    SHOW GET pbSele   ENABLE
  ELSE
    SHOW GET pbSele   DISABLE
  ENDIF
  *B603793,1 ABD [End]
  SHOW GET pbSeleA  ENABLE
ENDIF

SHOW WINDOW (lcLin_Ttl) REFRESH
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfVCancel
*! Purpose   : Ignore the user selection and terminate 
*!             the select PO lines browse screen
*!*************************************************************
FUNCTION lfVCancel
m.PO = SPACE(6)
_CUROBJ = OBJNUM(m.PO)
RETURN


*!*************************************************************
*! Name      : lfvOk
*! Purpose   : Append the appropriate records in the shipment lines file
*!*************************************************************
FUNCTION lfvOk

SELECT (lcTmpPoLn)
GO TOP
llFirst   = .T.
*B803680,1 ABD Till user message that he select lines for this PO. [Begin]  
WAIT WINDOW 'Please wait .... Collecting data' NOWAIT
*B803680,1 ABD [End]

*E301797,1 (Begin) Adjust for Material shiprment.
IF llFromMA
  SCAN FOR !EMPTY(cStatus)
    SCATTER MEMVAR MEMO
    SELECT (lcShpLine)
    IF SEEK(&lcTmpPoLn..pomat+&lcTmpPoLn..fabric+&lcTmpPoLn..color+STR(&lcTmpPoLn..LineNo,6))
      *--PO\Fabric\color was entered from before in this shipment. The line will be ignored
      =gfModalGen('INM36180B00000','DIALOG',&lcTmpPoLn..POMAT+'|'+&lcTmpPoLn..fabric+'|'+&lcTmpPoLn..color)
      LOOP
    ENDIF
    SELECT (lcShpLine)
    
    *B119930,1 ABD - Ship the open qty from the seleced record. [Begin]
    M.nFabTotQty = MAX(M.nFabTotQty - (nRecevQty + ntransQty),0)
    *B119930,1 ABD - [End]
    
    APPEND BLANK
    GATHER MEMVAR MEMO
    lnTotQty = lnTotQty + nFabTotQty
    REPLACE ShipNo  WITH laData[1],;
            TranCd  WITH '3'      ,;
            lNewRec WITH .T.      ,;
            TOTQTY  WITH nFabTotQty
    SELECT (lcTmpPoLn)
    IF llFirst
      llFirst = .F.
    ENDIF
  ENDSCAN
  WAIT CLEAR
  ZAP
  llPresOk = .T.
  RETURN
ENDIF
*E301797,1 (End)

SCAN FOR !EMPTY(cStatus)
  SCATTER MEMVAR MEMO
  SELECT (lcShpLine)
  IF SEEK(&lcTmpPoLn..PO+&lcTmpPoLn..Style+STR(&lcTmpPoLn..LineNo,6))
    *--PO\Style &lcTmpPoLn..PO+'\'+&lcTmpPoLn..Style was entered from before in this shipment. The line will be ignored
    =gfModalGen('INM34133B00000','DIALOG',&lcTmpPoLn..PO+'|'+&lcTmpPoLn..Style)
    LOOP
  ENDIF

  *B603793,1 ABD Accumulate the remainder from the PO to browse it when 
  *B603793,1 ABD Select the lines from browse screen. [Begin]
  *--B603882,1 RAMY [start]
  *SELECT (lcTmpPoln)
  *lnSaveRec = RecNo()
  *--B603882,1 RAMY [end
  SELECT POSLN
  *B603839,1 ABD Speed up the performance when select the po .[Begin]  
  *SCAN FOR cStyType+PO+Style+STR(LineNo,6)+TranCd+STR(RECNO(),7) = 'P'+m.PO
  *--Koko
  *IF SEEK('P'+m.PO)
  *B804301,4 MHM  07/16/2001 comment the following code that checks for open Qty [Start]
  *IF SEEK('P'+m.PO+m.Style)
  *--Koko
    *--B603882,1 RAMY 1- Add the FOR condition to the SCAN statement 
    *--B603882,1 RAMY 2- Remove the IF SEEK line [start]
    
    *SCAN REST WHILE cStyType+PO+Style+STR(LineNo,6)+TranCd+STR(RECNO(),7) = 'P'+m.PO
    *B603839,1 ABD [End] 
    *IF Trancd # '1'
    *IF SEEK('P'+m.PO+m.Style+'1'+STR(LineNo,6),lcTmpPoln)
  *  SCAN REST WHILE cStyType+PO+Style+STR(LineNo,6)+TranCd+STR(RECNO(),7) = 'P'+m.PO+M.Style ;
              FOR Trancd # '1'
     
    *--B603882,1 RAMY [end]
      *B603839,1 ABD update the Qty upon the scale sizes. [Begin]
        *FOR I = 1 To 8 
  *      SELECT STYLE
        *--B603882,1 RAMY Change the alias and fix the seek condition [start]
        *IF SEEK (&lcTmpPoln..STYLE)
  *      IF SEEK(POSLN.STYLE)
        *--B603882,1 RAMY [end]
  *        SELECT SCALE
  *        =SEEK ('S'+ STYLE.SCALE)
  *      ENDIF
  *      SELECT POSLN
  *      FOR I = 1 To SCALE.CNT      
         *B603839,1 ABD [End]           
  *        lcCount = STR(I,1)
  *        m.Qty&lcCount = m.Qty&lcCount - Posln.Qty&lcCount
  *      ENDFOR  
  *      m.TotQty      = m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8
      *--B603882,1 RAMY [start]
      *ENDIF
      *ENDIF
      *--B603882,1 RAMY [end]
  *  ENDSCAN
    *B603839,1 End for if statement.[Begin]
  *ENDIF
  *B804301,4 MHM  07/24/2001 [End]

  *B603839,1 ABD [End]    
  
  *--B603882,1 RAMY [start]
  *SELECT (lcTmpPoln)
  *GOTO lnSaveRec
  *--B603882,1 RAMY [end]
  SELECT (lcShpLine)
  *B603793,1 ABD [End]
  APPEND BLANK
  *B604836,1 08/20/2001 KHM (Begin) Recalculating the Total quantity
  m.TotQty      = m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8
  *B604836,1 AMH (End)

  GATHER MEMVAR MEMO
  
  *B605208,1 AMH recalculating the total quantity of the whole of shipment [Start]
  lnTotQty = lnTotQty + TotQty
  *B605208,1 AMH [End]
  
  REPLACE ShipNo  WITH laData[1],;
          TranCd  WITH '3'      ,;
         lNewRec WITH .T.
  SELECT (lcTmpPoLn)
  IF llFirst
    llFirst = .F.
  ENDIF
ENDSCAN
*B803680,1 Releasing the wait window
WAIT CLEAR
*B803680,1 ABD [End]

ZAP
llPresOk = .T.

*:*************************************************************
*! Name     : lpSavScr
*! Purpose  : Save/Update shimpent intransit
*:*************************************************************
PROCEDURE lpSavScr
PRIVATE lnTotShp

lcShpType = 'shipment'
SELECT (lcShpLine)
LOCATE FOR TotQty=0
IF FOUND()
  *-One or more lines has zero quantity,Ignore this lines.
  IF gfModalGen('TRM34052B34001','DIALOG')=1
    DELETE REST FOR TotQty=0
  ELSE
    STORE .F. TO llShow,llCSave
    RETURN .F.
  ENDIF
ENDIF

*C123616,1  TMI [Start] if carton qty is different than qty in the main shipment screen then add a warning
IF ASCAN(laEvntTrig,'SHPWRN')<>0
  IF !gfDoTriger('POSHP','SHPWRN')
    RETURN .F.
  ENDIF
ENDIF
*C123616,1  TMI [End  ] 

GO TOP

*E301797,1 (Begin) Adjust for Material shiprment.
IF llFromMA

  IF lfCkEOF(lcShpLine)
    *-The lines for this '+lcShpType+' are missing ! cannot update.
    =gfModalGen('TRM34017B34000','DIALOG',lcShpType)
    SELECT (lcShpLine)
    PACK
    GO TOP
    STORE '' TO lcFabric,lcColor,lcDesc,m.PO
    SHOW GET lcFabric
    SHOW GET lcColor
    SHOW GET lcDesc
    =lfActBrow1()
    SHOW GETS WINDOW (lcWinC22) DISABLE ONLY
    SHOW GET pbNew ENABLE 
    
    *:B038431,1 MHM 08/23/2004 Check if called from MA [Start]
    *SELECT ShpmtHdr
    IF llFromMA
      SELECT MATSHPHD
    ELSE
      SELECT ShpmtHdr
    ENDIF  
    *:B038431,1 MHM [End]
    
    SHOW GET pbRemove DISABLE
    STORE .F. TO llShow,llCSave
    RETURN .F.
  ENDIF
  SELECT (lcShpLine)
  SUM ALL nFabTotQty TO lnTotShp
  
  IF laScrMode[4]
    SELECT MATSHPDT
    APPEND FROM (gcWorkDir+lcShpLine)
    SET ORDER TO TAG FabDye IN FabDye
    SET ORDER TO TAG Fabric IN Fabric
    SELECT (lcShpLine)
    SET RELATION TO Fabric + Color+cWareCode+SPACE(10) INTO FabDye ADDI
    SET RELATION TO Fabric + Color INTO Fabric  ADDITIVE
    SCAN
      IF !EOF('FabDye')
        SELECT FabDye
        REPLACE Matotintr WITH Matotintr + &lcShpLine..nFabTotQty*Fabric.CONV
      ENDIF
      IF !EOF('Fabric')
        SELECT Fabric
        REPLACE Matotintr WITH Matotintr + &lcShpLine..nFabTotQty*Fabric.CONV
        
      ENDIF
      SELECT (lcShpLine)
    ENDSCAN
    SET RELATION TO
    SELECT MATSHPHD
    APPEND BLANK
  ELSE
    SELECT MATSHPDT
    SET ORDER TO TAG Shipno
    SET RELATION TO Fabric + Color+cWareCode+SPACE(10) INTO FabDye ADDI
    SET RELATION TO Fabric + Color INTO Fabric  ADDITIVE
    IF SEEK(laData[1])
      SCAN REST WHILE ShipNo = laData[1] FOR TranCd = '3'
        IF !SEEK(pomat+fabric+color+STR(LineNo,6),lcShpLine)
          REPLACE lFlag WITH .T.
          IF !EOF('FabDye')
            SELECT FabDye
            REPLACE Matotintr WITH max(Matotintr - MATSHPDT.nFabTotQty,0)*Fabric.CONV
          ENDIF
          IF !EOF('Fabric')
            SELECT Fabric
            REPLACE Matotintr WITH max(Matotintr - MATSHPDT.nFabTotQty,0)*Fabric.CONV
          ENDIF
         SELECT MATSHPDT
        ENDIF
      ENDSCAN
      =SEEK(laData[1])
      DELETE REST WHILE ShipNo = laData[1] FOR TranCd = '3' AND lFlag = .T.
    ENDIF

    *E301797,1 (Begin) Adjust for Material shiprment.
    SELECT MATSHPDT
    SET RELATION TO
    SET ORDER TO TAG Shipno
    SET RELATION TO Fabric + Color+cWareCode+SPACE(10) INTO FabDye ADDI
    SET RELATION TO Fabric + Color INTO Fabric  ADDITIVE
    
    SELECT (lcShpLine)
    SCAN
      SCATTER MEMVAR MEMO 
      IF SEEK(laData[1]+'P'+pomat+crsession+fabric+color+'3','MATSHPDT')
        SELECT MATSHPDT
          IF !EOF('FabDye')
            SELECT FabDye
            REPLACE Matotintr WITH max(Matotintr - &lcShpLine..nFabTotQty,0)*Fabric.CONV
          ENDIF
          IF !EOF('Fabric')
            SELECT Fabric
            REPLACE Matotintr WITH max(Matotintr - &lcShpLine..nFabTotQty,0)*Fabric.CONV
          ENDIF
        SELECT MATSHPDT
      ELSE
        SELECT MATSHPDT
        APPEND BLANK
      ENDIF
      GATHER MEMVAR MEMO
      IF !EOF('FabDye')
        SELECT FabDye
        REPLACE Matotintr WITH Matotintr + &lcShpLine..nFabTotQty*Fabric.CONV
      ENDIF
      IF !EOF('Fabric')
        SELECT Fabric
        REPLACE Matotintr WITH Matotintr + &lcShpLine..nFabTotQty*Fabric.CONV
      ENDIF
    ENDSCAN
    SELECT MATSHPDT
    SET RELATION TO
    SELECT MATSHPHD
  ENDIF
  REPLACE ShipNo     WITH laData[1],;
          Status     WITH laData[2],;
          Entered    WITH laData[3],;
          nFabTotQty WITH lnTotShp ,;
          Cartons    WITH laData[4],;
          AirWayB    WITH laData[5],;
          ETA        WITH laData[6],;
          cVessel    WITH laData[7],;
          Reference  WITH laData[8],;
          Dcustom    WITH laData[9],;
          Dtrdate    WITH laData[10],; 
          Dwarehous  WITH laData[11],;
          Shipvia    WITH laData[12],;
          Cship_adv  WITH laData[13],;
          Note       WITH laData[14]
  RETURN
ENDIF
*E301797,1 (End)

*B120628,1 ASH 11/16/2003 (Begin) Fix bug of not saving header fields user defined field in case of completed shipments.
*IF lfCkEOF(lcShpLine)
IF lfCkEOF(lcShpLine) AND laData[2] <> 'C'
*B120628,1 ASH 11/16/2003 (End)
  *-The lines for this '+lcShpType+' are missing ! cannot update.
  =gfModalGen('TRM34017B34000','DIALOG',lcShpType)
  SELECT (lcShpLine)
  PACK
  GO TOP
  SCATTER MEMVAR BLANK
  =lfActBrow1()    
  STORE ' ' TO lcStyDesc
  SHOW GETS WINDOW (lcWinC22) DISABLE ONLY
  SHOW GET pbNew ENABLE 
  SELECT ShpmtHdr

  SHOW GET pbRemove DISABLE
  STORE .F. TO llShow,llCSave
  RETURN .F.
ENDIF


SELECT (lcShpLine)
SUM ALL TotQty TO lnTotShp

IF laScrMode[4]
  SELECT PosLn
  APPEND FROM (gcWorkDir+lcShpLine)
  
  SET ORDER TO TAG StyDye IN StyDye
  SET ORDER TO TAG Style  IN Style
  SELECT (lcShpLine)
  SET RELATION TO Style+cWareCode+SPACE(10) INTO StyDye
  SET RELATION TO Style INTO Style ADDITIVE
  SCAN
    IF !EOF('StyDye')
      SELECT StyDye
      REPLACE InTrans1 WITH InTrans1 + &lcShpLine..Qty1,;
              InTrans2 WITH InTrans2 + &lcShpLine..Qty2,;
              InTrans3 WITH InTrans3 + &lcShpLine..Qty3,;
              InTrans4 WITH InTrans4 + &lcShpLine..Qty4,;
              InTrans5 WITH InTrans5 + &lcShpLine..Qty5,;
              InTrans6 WITH InTrans6 + &lcShpLine..Qty6,;
              InTrans7 WITH InTrans7 + &lcShpLine..Qty7,;
              InTrans8 WITH InTrans8 + &lcShpLine..Qty8,;
              TotInTrn WITH TotInTrn + &lcShpLine..TotQty
    ENDIF
    IF !EOF('Style')
      SELECT Style
      REPLACE InTrans1 WITH InTrans1 + &lcShpLine..Qty1,;
              InTrans2 WITH InTrans2 + &lcShpLine..Qty2,;
              InTrans3 WITH InTrans3 + &lcShpLine..Qty3,;
              InTrans4 WITH InTrans4 + &lcShpLine..Qty4,;
              InTrans5 WITH InTrans5 + &lcShpLine..Qty5,;
              InTrans6 WITH InTrans6 + &lcShpLine..Qty6,;
              InTrans7 WITH InTrans7 + &lcShpLine..Qty7,;
              InTrans8 WITH InTrans8 + &lcShpLine..Qty8,;
              TotInTrn WITH TotInTrn + &lcShpLine..TotQty
    ENDIF
    *B040280,1 NNA 06/21/2006 (Begin) Generate 943 EDI transaction for PO shipment directed to 3PL providers.
    =lfEdiTrans('ADD')
    *B040280,1 NNA (End)
    
    SELECT (lcShpLine)
  ENDSCAN
  SET RELATION TO

  SELECT ShpmtHdr
  APPEND BLANK
  
ELSE
  
  SELECT Posln
  SET ORDER TO TAG POsLnSh
  SET RELATION TO Style+cWareCode+SPACE(10) INTO StyDye
  SET RELATION TO Style INTO Style ADDITIVE

  IF SEEK(laData[1])
    SCAN REST WHILE ShipNo = laData[1] FOR TranCd = '3'
      IF !SEEK(PO+Style+STR(LineNo,6),lcShpLine)
        REPLACE lFlag WITH .T.
        IF !EOF('StyDye')
          SELECT StyDye
          REPLACE InTrans1 WITH MAX(InTrans1 - PosLn.Qty1,0),;
                  InTrans2 WITH MAX(InTrans2 - PosLn.Qty2,0),;
                  InTrans3 WITH MAX(InTrans3 - PosLn.Qty3,0),;
                  InTrans4 WITH MAX(InTrans4 - PosLn.Qty4,0),;
                  InTrans5 WITH MAX(InTrans5 - PosLn.Qty5,0),;
                  InTrans6 WITH MAX(InTrans6 - PosLn.Qty6,0),;
                  InTrans7 WITH MAX(InTrans7 - PosLn.Qty7,0),;
                  InTrans8 WITH MAX(InTrans8 - PosLn.Qty8,0),;
                  TotInTrn WITH MAX(TotInTrn - PosLn.TotQty,0)
        ENDIF
        IF !EOF('Style')
          SELECT Style
          REPLACE InTrans1 WITH MAX(InTrans1 - PosLn.Qty1,0),;
                  InTrans2 WITH MAX(InTrans2 - PosLn.Qty2,0),;
                  InTrans3 WITH MAX(InTrans3 - PosLn.Qty3,0),;
                  InTrans4 WITH MAX(InTrans4 - PosLn.Qty4,0),;
                  InTrans5 WITH MAX(InTrans5 - PosLn.Qty5,0),;
                  InTrans6 WITH MAX(InTrans6 - PosLn.Qty6,0),;
                  InTrans7 WITH MAX(InTrans7 - PosLn.Qty7,0),;
                  InTrans8 WITH MAX(InTrans8 - PosLn.Qty8,0),;
                  TotInTrn WITH MAX(TotInTrn - PosLn.TotQty,0)
        ENDIF
        SELECT Posln
      ENDIF
    ENDSCAN

    *B040280,1 NNA 06/21/2006 (Begin) Delete 943 EDI transaction for PO shipment in case of Edit mode.
    =lfEdiTrans('DELETE')
    SELECT Posln
    *B040280,1 NNA (End)

    =SEEK(laData[1])
    DELETE REST WHILE ShipNo = laData[1] FOR TranCd = '3' AND lFlag = .T.
  ENDIF
  SELECT (lcShpLine)
  SCAN
    SCATTER MEMVAR MEMO
    IF SEEK(laData[1]+'P'+PO+Style+STR(LineNo,6)+'3','Posln')
      SELECT PosLn
      IF !EOF('StyDye')
        SELECT StyDye
        REPLACE InTrans1 WITH MAX(InTrans1 - PosLn.Qty1,0),;
                InTrans2 WITH MAX(InTrans2 - PosLn.Qty2,0),;
                InTrans3 WITH MAX(InTrans3 - PosLn.Qty3,0),;
                InTrans4 WITH MAX(InTrans4 - PosLn.Qty4,0),;
                InTrans5 WITH MAX(InTrans5 - PosLn.Qty5,0),;
                InTrans6 WITH MAX(InTrans6 - PosLn.Qty6,0),;
                InTrans7 WITH MAX(InTrans7 - PosLn.Qty7,0),;
                InTrans8 WITH MAX(InTrans8 - PosLn.Qty8,0),;
                TotInTrn WITH MAX(TotInTrn - PosLn.TotQty,0)
      ENDIF
      IF !EOF('Style')
        SELECT Style
        REPLACE InTrans1 WITH MAX(InTrans1 - PosLn.Qty1,0),;
                InTrans2 WITH MAX(InTrans2 - PosLn.Qty2,0),;
                InTrans3 WITH MAX(InTrans3 - PosLn.Qty3,0),;
                InTrans4 WITH MAX(InTrans4 - PosLn.Qty4,0),;
                InTrans5 WITH MAX(InTrans5 - PosLn.Qty5,0),;
                InTrans6 WITH MAX(InTrans6 - PosLn.Qty6,0),;
                InTrans7 WITH MAX(InTrans7 - PosLn.Qty7,0),;
                InTrans8 WITH MAX(InTrans8 - PosLn.Qty8,0),;
                TotInTrn WITH MAX(TotInTrn - PosLn.TotQty,0)
      ENDIF
      
      SELECT PosLn
    ELSE
      SELECT Posln
      APPEND BLANK
    ENDIF

    GATHER MEMVAR MEMO
    
    IF !EOF('StyDye')
      SELECT StyDye
      REPLACE InTrans1 WITH InTrans1 + &lcShpLine..Qty1,;
              InTrans2 WITH InTrans2 + &lcShpLine..Qty2,;
              InTrans3 WITH InTrans3 + &lcShpLine..Qty3,;
              InTrans4 WITH InTrans4 + &lcShpLine..Qty4,;
              InTrans5 WITH InTrans5 + &lcShpLine..Qty5,;
              InTrans6 WITH InTrans6 + &lcShpLine..Qty6,;
              InTrans7 WITH InTrans7 + &lcShpLine..Qty7,;
              InTrans8 WITH InTrans8 + &lcShpLine..Qty8,;
              TotInTrn WITH TotInTrn + &lcShpLine..TotQty
    ENDIF
    IF !EOF('Style')
      SELECT Style
      REPLACE InTrans1 WITH InTrans1 + &lcShpLine..Qty1,;
              InTrans2 WITH InTrans2 + &lcShpLine..Qty2,;
              InTrans3 WITH InTrans3 + &lcShpLine..Qty3,;
              InTrans4 WITH InTrans4 + &lcShpLine..Qty4,;
              InTrans5 WITH InTrans5 + &lcShpLine..Qty5,;
              InTrans6 WITH InTrans6 + &lcShpLine..Qty6,;
              InTrans7 WITH InTrans7 + &lcShpLine..Qty7,;
              InTrans8 WITH InTrans8 + &lcShpLine..Qty8,;
              TotInTrn WITH TotInTrn + &lcShpLine..TotQty
    ENDIF

    *B040280,1 NNA 06/21/2006 (Begin) Generate 943 EDI transaction for PO shipment directed to 3PL providers.
    =lfEdiTrans('ADD')
    *B040280,1 NNA (End)

  ENDSCAN
  
  SELECT Posln
  SET RELATION TO
  SELECT ShpmtHdr
  
ENDIF

*E301159,1  (WMA) Save Added Fields [Begin]
*REPLACE ShipNo    WITH laData[1],;
         Status    WITH laData[2],;
         Entered   WITH laData[3],;
         TotQty    WITH lnTotShp ,;
         Cartons   WITH laData[4],;
         AirWayB   WITH laData[5],;
         ETA       WITH laData[6],;
         cVessel   WITH laData[7],;
         Reference WITH laData[8]

REPLACE ShipNo    WITH laData[1],;
        Status    WITH laData[2],;
        Entered   WITH laData[3],;
        TotQty    WITH lnTotShp ,;
        Cartons   WITH laData[4],;
        AirWayB   WITH laData[5],;
        ETA       WITH laData[6],;
        cVessel   WITH laData[7],;
        Reference WITH laData[8],;
        Dcustom   WITH laData[9],;
        Dtrdate   WITH laData[10],; 
        Dwarehous WITH laData[11],;
        Shipvia   WITH laData[12],;
        Cship_adv WITH laData[13],;
        Note      WITH laData[14]
*E301159,1  (WMA) [End]
*C123616,1  TMI [Start] Save data for SHPCRTMF for Nik Nak
IF ASCAN(laEvntTrig,PADR("SVSHP",10)) <> 0
  =gfDoTriger("POSHP",PADR("SVSHP",10))
ENDIF  
*C123616,1  TMI [End  ] 

*:*************************************************************
*! Name     : lpDelScr
*! Purpose  : Delete shipment intransit
*:*************************************************************
PROCEDURE lpDelScr

*E301797,1 (Begin) Delete Material shiprment.
IF llFromMA
  IF MATSHPHD.Status = 'X'
    *--Shipment status is canceled. Cannot proceed.
    =gfModalGen('INM34134B00000','DIALOG','canceled')
    RETURN
  ENDIF

  IF MATSHPHD.Status = 'C'
    *--Shipment status is complete. Cannot proceed.
    =gfModalGen('INM34134B00000','DIALOG','complete')
    RETURN
  ENDIF

  SET ORDER TO TAG FabDye IN FabDye
  SET ORDER TO TAG shipno IN matshpdt
  
  IF SEEK(laData[1],'matshpdt')
    SELECT matshpdt
    SET RELATION TO Fabric + Color+cWareCode+SPACE(10) INTO FabDye ADDI
    SET RELATION TO Fabric + Color INTO Fabric  ADDITIVE
    SCAN REST WHILE ShipNo = laData[1] FOR TranCd = '3'
      IF !EOF('FabDye')
        SELECT FabDye
        REPLACE Matotintr WITH MAX(Matotintr - matshpdt.TotQty,0)
      ENDIF
      IF !EOF('Fabric')
        SELECT Fabric
        REPLACE Matotintr WITH MAX(Matotintr - matshpdt.TotQty,0)
      ENDIF
    ENDSCAN
    SET RELATION TO
    =SEEK(laData[1])
    DELETE REST WHILE ShipNo = laData[1] FOR TranCd = '3'
  ENDIF
  IF SEEK(laData[1],'MATSHPHD')
    SELECT MATSHPHD
    REPLACE Status WITH 'X'
  ENDIF
  STORE "" TO lcFabric,lcColor,lcDesc,m.Po
  STORE 0  TO m.TotQty
  SHOW GET lcFabric DISABLE
  SHOW GET lcColor  DISABLE
  SHOW GET lcDesc   DISABLE
  SHOW GET m.Po     DISABLE
  SHOW GET m.TotQty DISABLE
  RETURN
ENDIF
*E301797,1 (End)

IF ShpmtHdr.Status = 'X'
  *--Shipment status is canceled. Cannot proceed.
  =gfModalGen('INM34134B00000','DIALOG','canceled')
  RETURN
ENDIF

IF ShpmtHdr.Status = 'C'
  *--Shipment status is complete. Cannot proceed.
  =gfModalGen('INM34134B00000','DIALOG','complete')
  RETURN
ENDIF

SET ORDER TO TAG StyDye IN StyDye
SET ORDER TO TAG POsLnSh IN PosLn
IF SEEK(laData[1],'PosLn')
  SELECT PosLn
  SET RELATION TO Style+cWareCode+SPACE(10) INTO StyDye
  SET RELATION TO Style INTO Style ADDITIVE
  SCAN REST WHILE ShipNo = laData[1] FOR TranCd = '3'
    IF !EOF('StyDye')
      SELECT StyDye
      REPLACE InTrans1 WITH MAX(InTrans1 - PosLn.Qty1,0),;
              InTrans2 WITH MAX(InTrans2 - PosLn.Qty2,0),;
              InTrans3 WITH MAX(InTrans3 - PosLn.Qty3,0),;
              InTrans4 WITH MAX(InTrans4 - PosLn.Qty4,0),;
              InTrans5 WITH MAX(InTrans5 - PosLn.Qty5,0),;
              InTrans6 WITH MAX(InTrans6 - PosLn.Qty6,0),;
              InTrans7 WITH MAX(InTrans7 - PosLn.Qty7,0),;
              InTrans8 WITH MAX(InTrans8 - PosLn.Qty8,0),;
              TotInTrn WITH MAX(TotInTrn - PosLn.TotQty,0)
    ENDIF
    IF !EOF('Style')
      SELECT Style
      REPLACE InTrans1 WITH MAX(InTrans1 - PosLn.Qty1,0),;
              InTrans2 WITH MAX(InTrans2 - PosLn.Qty2,0),;
              InTrans3 WITH MAX(InTrans3 - PosLn.Qty3,0),;
              InTrans4 WITH MAX(InTrans4 - PosLn.Qty4,0),;
              InTrans5 WITH MAX(InTrans5 - PosLn.Qty5,0),;
              InTrans6 WITH MAX(InTrans6 - PosLn.Qty6,0),;
              InTrans7 WITH MAX(InTrans7 - PosLn.Qty7,0),;
              InTrans8 WITH MAX(InTrans8 - PosLn.Qty8,0),;
              TotInTrn WITH MAX(TotInTrn - PosLn.TotQty,0)
    ENDIF
  ENDSCAN
  SET RELATION TO
  =SEEK(laData[1])
  DELETE REST WHILE ShipNo = laData[1] FOR TranCd = '3'
  
ENDIF

IF SEEK(laData[1],'ShpmtHdr')
  SELECT ShpmtHdr
  REPLACE Status WITH 'X'
ENDIF

*B040280,1 NNA 06/21/2006 (Begin) Delete 943 EDI transaction for PO shipment directed to 3PL providers.
=lfEdiTrans('DELALL')
*B040280,1 NNA (End)

*C123616,1  TMI [Start] remove shipment cartons from custom file SHPCRTMF for niknak
IF ASCAN(laEvntTrig,'RMVSHP')<>0
  =gfDoTriger('POSHP','RMVSHP')
ENDIF
*C123616,1  TMI [End  ] 
* ----------------------------------------------------------------

FUNCTION lfGtSavBar
USE (gcSysHome+"SYCMenu") IN 0 ORDER Pross_ID AGAIN ALIAS MenuFile
lnRetVal = IIF(SEEK("GFCPSAVE", "MenuFile"), VAL(MenuFile.cBar_Pos), 51)
USE IN MenuFile
RETURN (lnRetVal)



*C101704,1 [Start]
*!**************************************************************************
*! Name      : lfPicture  
*! Developer : Sameh Aldesouki 
*! Date      : 01/17/2000
*! Purpose   : Change Picture of PO.
*!**************************************************************************
FUNCTION lfPicture

IF llGenOrNum
  RETURN "@! XXXXXX" 
ELSE
  RETURN "@! X99999"
ENDIF  
*C101704,1 [End]

*B605208,1 AMH Save the Old Data [Start]
*!**************************************************************************
*! Name      : lfSaveOld
*! Developer : AHMED MAHER (AMH)
*! Date      : 12/30/2001
*! Purpose   : Save the old Data
*!**************************************************************************
FUNCTION lfSaveOld

PRIVATE lnI,llSaveOld
llSaveOld = .T.
FOR lnI = 1 TO 8
  IF laOldQty[lnI] # 0
    llSaveOld = .F.
    EXIT
  ENDIF
ENDFOR
IF llSaveOld
  SCATTER FIELDS QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8 TO laOldQty
ENDIF
*B605208,1 AMH [End]

*B605208,1 AMH Restore the Old Data [Start]
*!**************************************************************************
*! Name      : lfRestOld
*! Developer : AHMED MAHER (AMH)
*! Date      : 12/30/2001
*! Purpose   : Restore the old Data
*!**************************************************************************
FUNCTION lfRestOld

PRIVATE lnAlias,lnI,lcI
lnAlias = SELECT(0)
SELECT (lcShpLine)
REPLACE TOTQTY WITH 0
FOR lnI = 1 TO 8
  lcI = STR(lnI,1)
  REPLACE ('QTY'+lcI) WITH laOldQty[lnI]
  REPLACE TOTQTY WITH TOTQTY + laOldQty[lnI]
ENDFOR
SCATTER MEMVAR
SELECT (lnAlias)
SHOW GETS WINDOW (lcWinC22) ONLY
SHOW WINDOW (lcBrTtl1) REFRESH SAME
=lfRefresh(lcWinC22)
FLUSH
RETURN
*B605208,1 AMH [End]

*!*************************************************************
*! Name      : lfvMaPO
*! Developer : Adel Mohammed El Gazzar (Adel)
*! Date      : 21/01/02
*! Purpose   : validate the entered Ma Po number
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*!..E301797
FUNCTION lfvMaPO

PRIVATE lnalias,llRetValue,lcBrTitle,lcBrFields
PRIVATE llInvalid,llPresOk,llFirst
STORE .F. TO llPresOk,llInvalid
llFirst   = .T.

IF MDOWN()
  RETURN
ENDIF
lnalias = SELECT()

*-- browse all the P/O's if the enterd po is not found in the pofhdr
IF !SEEK('P'+m.PO,'POFHDR') AND !EMPTY(m.PO)
  =POFBROW ('P','',@m.PO)
ENDIF

IF EMPTY(m.PO)
  _CUROBJ = OBJNUM(m.PO)
  RETURN
ENDIF

IF POFHDR.Status $ 'CSX'
  *--PO status is either complete, canceled, or closed. Cannot proceed.
  =gfModalGen('TRM34131B00000','DIALOG')
  _CUROBJ = OBJNUM(m.PO)
  RETURN
ENDIF
SET ORDER TO Matln IN MATSHPDT
IF !SEEK('P'+m.PO,'POFLN')
  *--The lines for this PO have not been found. Cannot proceed.
  =gfModalGen('TRM34132B00000','DIALOG')
  _CUROBJ = OBJNUM(m.PO)  
  RETURN
ENDIF
lnMatWh   = ASCAN(laWareM,POFHDR.cwarecode)
lcWare    = laWareM[lnMatWh] 
lcLin_Ttl = "PO Lines"
SELECT MATSHPDT 
lcOldOrder = ORDER()
SELECT (lcTmpPoLn)
ZAP
SELECT POFLN
=SEEK('P'+m.PO)
SCAN REST WHILE cmattype+pomat+fabric+color+trancd = 'P'+m.PO FOR TranCd = '1'
  WAIT WINDOW 'Collecting P/O lines :'+POFLN.Fabric NOWAIT
  SCATTER MEMVAR 
  SELECT (lcTmpPoLn)
  APPEND BLANK
  GATHER MEMVAR  
  REPLACE nOpenQty WITH POFLN.nFabTotQty
ENDSCAN
WAIT CLEAR
SELECT (lcTmpPoLn)
GO TOP

*B119930,1 ABD - Deduct the previous receiving sessions done against the selected line. [Begin]
SELECT POFLN
IF SEEK('P'+m.PO)
  SCAN REST WHILE cmattype+pomat+fabric+color+trancd = 'P'+m.PO FOR !(TranCd = '1')
    DO CASE
      CASE Trancd = '2' .OR. Trancd = '4' .OR. Trancd = '5'
        IF SEEK('P'+m.PO+fabric+color+'1'+STR(LineNo,6),lcTmpPoln)
          SELECT (lcTmpPoln)
          REPLACE nRecevQty With nRecevQty + POFLN.nFabTotqty
        ENDIF
      CASE Trancd = '3'
        IF SEEK('P'+m.PO+fabric+color+'1'+STR(LineNo,6),lcTmpPoln)
          SELECT (lcTmpPoln)
          REPLACE ntransQty With ntransQty + POFLN.nFabTotqty
        ENDIF
    ENDCASE
    SELECT (lcTmpPoln)
    REPLACE nOpenQty    With MAX(nOpenQty   - POFLN.nFabTotqty,0)
  ENDSCAN
  WAIT CLEAR
ENDIF  
SELECT (lcTmpPoLn)
GO TOP
*B119930,1 ABD - [End]

SELECT MATSHPDT
SET ORDER TO &lcOldOrder
*--Till user message that he select lines for this PO. [Begin]
WAIT WINDOW 'Please wait .... Collecting data' NOWAIT
IF SEEK('P'+m.PO)
  *B119930,1 ABD - Get the intransit from the current file only. [Begin]
  *SCAN REST WHILE cmattype+pomat+fabric+color+trancd = 'P'+m.PO
  *  IF Trancd # '1'
  *    DO CASE
  *       CASE Trancd = '2' .OR. Trancd = '4' .OR. Trancd = '5'
  *         IF SEEK('P'+m.PO+fabric+color+'1'+STR(LineNo,6),lcTmpPoln)
  *           SELECT (lcTmpPoln)
  *           REPLACE nRecevQty With nRecevQty + MATSHPDT.nFabTotqty
  *         ENDIF
  *       CASE Trancd = '3'
  *         IF SEEK('P'+m.PO+fabric+color+'1'+STR(LineNo,6),lcTmpPoln)
  *           SELECT (lcTmpPoln)
  *           REPLACE ntransQty With ntransQty + MATSHPDT.nFabTotqty
  *         ENDIF
  *     ENDCASE
  *     SELECT (lcTmpPoln)
  *     REPLACE nOpenQty  With MAX(nOpenQty - MATSHPDT.nFabTotqty,0)
  *     SELECT MATSHPDT
  *  ENDIF    
  SCAN REST WHILE cmattype+pomat+fabric+color+trancd = 'P'+m.PO for Trancd = '3'
     IF SEEK('P'+m.PO+fabric+color+'1'+STR(LineNo,6),lcTmpPoln)
       SELECT (lcTmpPoln)
       REPLACE ntransQty With ntransQty + MATSHPDT.nFabTotqty
     ENDIF
     SELECT (lcTmpPoln)
     REPLACE nOpenQty  With MAX(nOpenQty - MATSHPDT.nFabTotqty,0)
     SELECT MATSHPDT
  *B119930,1 ABD - [End]
  
  ENDSCAN
  WAIT CLEAR
ENDIF  

SELECT (lcTmpPoln)
SCAN
  IF nOpenQty <= 0
    DELETE
  ENDIF
ENDSCAN
LOCATE
IF EOF()
  = gfModalGen('TRM42080B00000','DIALOG','This PO was shipped before.')
  m.PO = ''
  SHOW GET m.PO
  _CUROBJ=OBJNUM(m.po)
  RETURN
ENDIF
ON KEY LABEL TAB
DO (gcScrDir+"POSEL.SPX")
*-- Reintialize this variable for not be used from Style po shipment.
llFromMat = .F.
ON KEY LABEL TAB        DO lfTab
IF llPresOk
  SHOW WINDOW (lcBrTtl1) REFRESH
  SHOW GET lnTotQty
  IF !llFirst
    IF llWareHous
      lnMatWh = ASCAN(laWareM,&lcShpLine..cWareCode)
    ENDIF
    SHOW GETS WINDOW (lcWinC22) ENABLE ONLY
    lcKey = EVAL(KEY())
    =lfGetLine()
    LOCATE
    IF !EOF()
      SHOW GET ibPO    DISABLE
      SHOW GET m.PO    DISABLE
    ENDIF 
    =SEEK(lcKey)
    *E301797,1 (Begin) Adjust for Material shiprment.
    IF llFromMA
      SHOW GET lcDesc   DISABLE
      SHOW GET lcFabric DISABLE
      SHOW GET lcColor  DISABLE
      SHOW GET pbFabric DISABLE
      SHOW GET pbColor  DISABLE
    ELSE
    *E301797,1 (End)
      SHOW GET ibStyle DISABLE
      SHOW GET m.Style DISABLE
    *E301797,1 (Begin) Adjust for Material shiprment.
    ENDIF
    *E301797,1 (End)
  ENDIF
ELSE
  SHOW GET m.PO  ENABLE
  SHOW GET ibPO  ENABLE
ENDIF
RETURN

*!*************************************************************
*! Name      : lfwWare
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : get the old warehouse in a variable
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!***************************************************
FUNCTION lfwWare

lcOldWare = lcWare

*!*************************************************************
*! Name      : lfvWare
*! Developer : Adel Mohammed El Gazzar (Adel)
*! Date      : 21/01/02
*! Purpose   : validate the warehouse field
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvWare
PRIVATE lnAlias,lcSeekKey

lnAlias   = SELECT()
lcSeekKey = SPACE(01)
lcWare    = laWareH[lnMatWh] 
IF !EMPTY(lcFabric) AND !EMPTY(lcColor)
  IF !lfFabInWar(lcFabric,lcColor,lcWare)
    _CUROBJ = _CUROBJ
    lnMatWh  = ASCAN(laWareh,cWareCode)
    SHOW GET lnMatWh
    RETURN
  ENDIF
  IF !EMPTY(lcFabric) AND !EMPTY(lcColor) AND !SEEK(lcFabric+lcColor+lcWare,'FABDYE')
    DO gpAdFabWar WITH lcFabric,lcColor,SPACE(10),lcWare
  ENDIF  
  REPLACE cWareCode WITH lcWare
  SHOW GETS WINDOW (lcWinC22) ONLY
  SHOW WINDOW (lcBrTtl1) REFRESH SAME
  =lfRefresh(lcWinC22)
ENDIF  
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfFabInWar
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : check if the fabric/color is assigned to the warehouse
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfFabInWar
PARAMETER lcfFabric,lcfColor,lcfWare

IF !SEEK(lcfFabric+lcfColor+lcfWare,'FabDye')
  lnChoice=gfModalGen('TRM36049B36001','ALERT',ALLTRIM(lcfFabric)+'/'+ALLTRIM(lcfColor)+'|'+ALLTRIM(lcfWare))
  IF lnChoice = 1
    DO gpAdFabWar WITH lcfFabric, lcfColor,SPACE(10), lcfWare
  ELSE
    RETURN .F.
  ENDIF
ENDIF
RETURN .T.

*:**************************************************************************
*:* Name        : lfActPad
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 15/09/2004
*! Purpose      : Bulid a new menu pad [Options]
*:***************************************************************************
*C123616,1
FUNCTION lfActPad

DEFINE PAD _Option OF _MSYSMENU PROMPT 'O\<ptions'
ON PAD _Option OF _msysmenu ACTIVATE POPUP _lPopOpt

DEFINE POPUP _lPopOpt MARGIN SHADOW

*-If there is standard options add them in the following lines
*..
*..


******  Custom Bars  *******
*C123616,1  TMI [Start] Add custom optins for Memo. to open "Carton Detail" Screen
IF ASCAN(laEvntTrig,PADR('ADCTNBAR',10)) <> 0
  =gfDoTriger('POSHP','ADCTNBAR')
ENDIF

*C123616,1  TMI [End  ] 
SET SKIP OF PAD _Option OF _MSYSMENU CNTBAR('_lPopOpt')=0
ON SELECTION POPUP _lPopOpt DO lpOptions
ON KEY LABEL ALT+P ACTIVATE POPUP _lPopOpt

*:**************************************************************************
*:* Name        : lpOptions
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 15/09/2004
*:* Purpose     : Options procedure
*:***************************************************************************
*C123616,1
PROCEDURE lpOptions

*-- end of lpOptions.
*!***************************************************************************
*!* Name        : lfEdiTrans
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 06/21/2006
*!* Purpose     : Generate 943 EDI transaction for PO shipment directed to 3PL providers.
*!***************************************************************************
*!* Called from : POSHP.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfEdiTrans()
*!***************************************************************************
FUNCTION lfEdiTrans
PARAMETERS lcAction
PRIVATE lnOldAlias,llChanged
STORE 0 TO lnOldAlias
STORE .F. TO llChanged
IF 'AS' $ gcCmpModules 
  lnOldAlias = SELECT(0)  
  IF !USED('EDITRANS') 
    =gfOpenFile(gcDataDir+'EDITRANS',gcDataDir+'TYPEKEY','SH')
  ENDIF
  IF !USED('EDIACPRT')
    =gfOpenFile(gcDataDir+'EDIACPRT',gcDataDir+'ACCFACT','SH')
  ENDIF
  IF !USED('EDIPD')
    =gfOpenFile(gcDataDir+'EDIPD',gcDataDir+'PARTTRANS','SH')
  ENDIF
  IF laScrMode[4] AND lcAction = 'ADD'
    IF SEEK(&lcShpLine..cWareCode,'WareHous') AND  SEEK('W'+WareHous.cThrdPLPr,'EDIACPRT') AND ;
      SEEK(EDIACPRT.cPartCode+'943','EDIPD')
      SELECT EDITRANS
      IF !SEEK('943'+PADR(ALLTRIM(laData[1]),40)+'W'+WareHous.cThrdPLPr)
        INSERT INTO 'EDITRANS' (CEDITRNTYP,KEY,TYPE,CSTATUS,CPARTNER,lInterComp) VALUES ;
        ('943',ALLTRIM(&lcShpLine..ShipNo),'W','N',WareHous.cThrdPLPr,.F.)
        =gfAdd_Info('EDITRANS')
      ENDIF
    ENDIF
  ELSE
    IF lcAction = 'DELETE'
      SELECT POSLN
      IF SEEK(laData[1])
        SCAN REST WHILE SHIPNO+CSTYTYPE+PO+STYLE+STR(LINENO,6)+TRANCD = laData[1] FOR TranCd = '3' AND lFlag = .T.
          llChanged = .T.
        ENDSCAN
        IF llChanged  		&& IF any changed happend in the shipment lines
          SELECT EDITRANS
          IF SEEK('943'+PADR(ALLTRIM(laData[1]),40)+'W')
            DELETE REST WHILE CEDITRNTYP+KEY+TYPE+CPARTNER = '943'+PADR(ALLTRIM(laData[1]),40)+'W'
          ENDIF
        ENDIF
      ENDIF
    ENDIF
    IF lcAction = 'ADD'
      IF SEEK(&lcShpLine..cWareCode,'WareHous') AND  SEEK('W'+WareHous.cThrdPLPr,'EDIACPRT') AND ;
        SEEK(EDIACPRT.cPartCode+'943','EDIPD')
        SELECT EDITRANS
        IF !SEEK('943'+PADR(ALLTRIM(laData[1]),40)+'W'+WareHous.cThrdPLPr)
          INSERT INTO 'EDITRANS' (CEDITRNTYP,KEY,TYPE,CSTATUS,CPARTNER,lInterComp) VALUES ;
          ('943',ALLTRIM(&lcShpLine..ShipNo),'W','N',WareHous.cThrdPLPr,.F.)
          =gfAdd_Info('EDITRANS')
        ENDIF
      ENDIF
    ENDIF
    IF lcAction = 'DELALL'    && in case of delete shipment
      SELECT EDITRANS
      IF SEEK('943'+PADR(ALLTRIM(laData[1]),40)+'W')
        DELETE REST WHILE CEDITRNTYP+KEY+TYPE+CPARTNER = '943'+PADR(ALLTRIM(laData[1]),40)+'W'
      ENDIF
    ENDIF
  ENDIF
  SELECT(lnOldAlias)
ENDIF
*-- End of Function lfEdiTrans.
