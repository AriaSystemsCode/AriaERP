*:************************************************************************
*:           File: ICInvtA.prg                                          :*
*:         System: ARIA APPAREL SYSTEM 2.7                              :*
*:         Module: Inventory Control                                    :*
*:        Program: Physical, adjustments, and Transfer inventory.       :*
*:         Author: Timour Abdalla Khalil                                :*
*:           Date: 07/22/97                                             :* 
*:************************************************************************
*: Passed Parameters  :                                                 :* 
*:    lnPrgTyp : To endicate the program type, which will be one of the :*  
*:               following numbers :                                    :* 
*:                1 = Adjustments.                                      :* 
*:                2 = Physical inventory.                               :* 
*:                3 = Transfer inventory.                               :* 
*:************************************************************************
*: Modifications :
*E301074,1  MAB 11/29/1998 Calling screen report in the end of Save procedure.
*E301077,12 TAK 01/04/1999 Redeuce the number of opened files at the start
*E301077,12                 running this program and open it with an object.
*B602457,1  MAB 01/25/1999 Open Temp. Files using Normal open instead of
*B602457,1                 using gfOpenFile, which make bug of alias not found
*B602457,1                 when select another module.
*B602614,1 MAB 03/25/1999 Disable < New > button until user validate posting date
*B602614,1                if single location.
*B802159,1 MAB 04/15/1999 Enable < New > button if system use single location and does not 
*B802159,1                link to GL.
*E301192,1 AHM 04/19/1999 Update reference field in styinvjl with descriptive text
*B602921,1 TAK 05/20/1999 Fixed Alias not found when pressing ESC. 
*B603028,1 TAK 05/20/1999 Fixed wrong updating of TotAdj field.
*B603049,4 ARH 07/06/1999 Added the old cost & the new cost to the main 
*B603049,4                browse in case of physical or adjustment (Function LFACTBROW).
*B802503,1 Reham On 08/08/1999
*B802503,1 Save the reference field in the style inventory journal.
*B802687,1 RAMY 10/25/1999 Fix the disabling of the command button in the printer setup when 
*B802687,1                 you choose 'YES' to print the Adj journal
*B802944,1 SAM 02/03/2000  Fix that a message (Are you sure you want to loose 
*B802944,1                 changes in this session) appears after saving when exit
*B802944,1                 despite the data have been saved correctly 
*B803150,1 KHM 05/16/2000  Fix the bug when you enter a style and browse the
*B803150,1                 pointer goes always to the top of the file not to 
*B803150,1                 the first appearance of the style of the nearest one.
*E301558,1 AHM 02/07/2001  Exclude the saving process from the lpSave Procedure into a seperate
*E301558,1                 Program to be called from the ICInvtA program and the Product Activity 
*E301558,1                 class in the EDI
*B604753,1 ASH 08/01/2001  Fix the bug of allowing Physical, adjustments, and Transfer inventory in locked periods.
*B605018,1 MHM 10/10/2001  Fix the bug of Wrong period Update in Physical, adjustments, and Transfer inventory in locked periods.
*B605167,1 ASH 11/27/2001  Fix the bug of wrong adjusting the stock in case of FIFO costing method.
*B804152,1 ADEL 08/01/02   Prompt the user to Rebalance stock when wrong one before Physical Adjusment.       
*B605176,1 BWA 08/20/2002 Fix the bug of Inv. Adj. Accept negative Qty.more than the stk in FIFO and LIFO Cost.
*E302005,1 ADEL 09/23/02  When making an adjustment, and going into negative stock, ask the user whether
*E302005,1                he wants to proceed of not. 
*B606489,1 TMI 10/01/2002 Show the long description in the browse
*B606564,1 TMI 10/29/2002 Reset the relation if not set (this is a completion of B606489)
*C037816,1 MHM 04/06/2004 Custom Bin Location For David Luke
*C123853,1 MHM 01/15/2005 modification in Custom Bin Location For David Luke
*B039107,1 EIH 03/03/2005 Fix bug of disable the NewCost field in case of standard cost method .
*B126587,1 EIH 03/16/2005 In 'IC->Transfere Inventory' The whole inventory amount allowed . but it must Inventory less Allocated.
*B039094,1 NNA 04/13/2005 Fix bug that in the inventory Adj. the Avg. Cost updates with the Last Cost
*B039094,1 NNA            not the Avg. (if the Stock cost=9 and you Adjust Stock with(+) Qty with cost 11
*B039094,1 NNA            you'll find that it updated with 11 not 10)
*B039660,1 NNA 09/04/2005 Make some Enhancements on the Bin location System (C037816,C123853) AND 
*B039660,1 NNA            Transfer all bin location Programs from Davmain.prg to Binamain.prg and make some changes
*B608299,1 NNA 10/01/2007 (T20070713.0001) fix Bug that inventory control programs deal with non inventory styles
*:*****************************************************************************************************
PARAMETERS lnPrgTyp

lcType = SUBSTR('APT',lnPrgTyp,1)
*--Program variables initialization.
DIMENSION laTStk[9],laOTStk[9],laAdjCode[1],laCodInfo[1,10],laOpFile[3]
STORE 0   TO laTStk,laOTStk,lnNewCost,lnOldCost
STORE ' ' TO lcWinCh0,lcWinCh1,lcWinCh2,lcWinCh3,;
             lcTmpAdj,lcTmpGlDt,lcTemLoc
STORE ''  TO lcFromWare,lcToWare,lcWFromDsc,lcWToDsc,lcReason,;
             lcDyelot,lcGlFYear,lcGlPeriod,lcWareAny,lcDefWareh,;
             lcAReason,lcRefer,lcOldValue
STORE .F. TO llDyelot,llAvrCost,llWareLoc,llMultiWH,llGlLink
STORE .F. TO llModiMod,llBrowse,llDyeLvl,llGoOut,llLoc,llCUpdate,llExtMFlg
STORE ' ' TO lcStyHdr,lcStyPict,lcStyDesc,laAdjCode,laCodInfo,lcCostMth
STORE ' ' TO lcHldTab,lcHldBtb,lcHldEsc,lcHldAtB,lcBtMode,lcBtNMod
STORE 0   TO lnstylewid,lnMjrWid
STORE gdSysDate TO ldPstDate,ldDate
lnAdjCode =  1
llChkDate = .T.
*E302005,1 (Begin) Initialize this variable to be true if the user wants to go into negative stock.
llNetAll    = .F.

*E302005,1 (End)
IF !gfSetup()
  RETURN
ENDIF

IF !WEXIST(gcBaseWind)
  *-- Get the setup variables from the memory files.
  llDyelot   = gfGetMemVar('M_Dyelot')   ='Y'
  lcCostMth  = gfGetMemVar('M_Cost_Meth')
  llWareLoc  = gfGetMemVar('M_WareLoc')  ='Y'
  llMultiWH  = gfGetMemVar('M_WareHouse')='Y'
  llGlLink   = gfGetMemVar('M_Link_GL')  ='Y'
  
  lcStyHdr   = gfItemMask('HI')
  lcStyPict  = gfItemMask('PI')
  lnstylewid = LEN(lcStyPict)
  lnMjrWid   = LEN(gfItemMask('PM'))
  
  *-- Do not allow the user to do the transfer program if the system
  *-- is set to multiple warehouses no.
  IF lnPrgTyp = 3
    IF !llMultiWH
      *--The system has not been setup to use multiple locations. Cannot proceed.
      =gfModalGen('TRM42054B42001','DIALOG')
      RETURN
    ENDIF
  ELSE
    IF llGlLink
      SELECT CODES
      lcCodTag=TAG()
      SET ORDER TO TAG cCode_no
      *IF !SEEK(gcAct_comp+'CADJREASON','CODES')
      IF !SEEK("N"+'CADJREASON','CODES')
        *--You have to edit the Adjustment reasons codes first, Cannot proceed.
        =gfModalGen('TRM42111B42001','DIALOG')
        SET ORDER TO TAG &lcCodTag
        SELECT INVTADJ
        RETURN 
      ENDIF  
      SET ORDER TO TAG &lcCodTag
    ENDIF
  ENDIF

  *--Program screen windows.
  lcWinCh0    = gfTempName()
  lcWinCh1    = gfTempName()
  lcWinCh2    = gfTempName()
  lcWinCh3    = gfTempName()
  SELECT STYLE
  SET ORDER TO TAG Style
  *E301077,12 comment out.
  *=gfOpenFile(gcDataDir+"StyInvJl","StyInvJl","SH")

  SELECT INVTADJ
  *-- Create tmp adjustment file.
  lcTmpAdj = gfTempName()
  =AFIELDS(laFStru)
  lnNo1=ASCAN(laFStru,'UNT_COST')
  lnNo2=ASCAN(laFStru,'OLD_COST')
  *--Make the lenth of this two fields as ave_cost field.
  STORE 15 TO laFStru(lnNo1+2),laFStru(lnNo2+2)
  STORE  7 TO laFStru(lnNo1+3),laFStru(lnNo2+3)
  lnFStru = ALEN(laFStru,1)
  DIMENSION laFStru[lnFStru+2,4]
  laFStru[lnFStru+1,1] = 'cAdjReason'
  laFStru[lnFStru+1,2] = 'C'
  laFStru[lnFStru+1,3] = 6
  laFStru[lnFStru+1,4] = 0
  laFStru[lnFStru+2,1] = 'cRefer'
  laFStru[lnFStru+2,2] = 'C'
  laFStru[lnFStru+2,3] = 6
  laFStru[lnFStru+2,4] = 0
  CREATE DBF (gcWorkDir+lcTmpAdj) FROM ARRAY laFStru
  
  *B602457 MAB Comment out the following line because [Begin
  *B602457 Temporay file opend normally without use of gfOpenFile.
  *=gfOpenFile(gcWorkDir+lcTmpAdj,' ','EX')
  *B602457 MAB Comment out the following line because [End..

  lcExpr=IIF(llDyelot,'Style+Dyelot+STR(RECNO(),6)','Style+STR(RECNO(),6)')
  INDEX ON &lcExpr TAG &lcTmpAdj
  SET RELATION TO Style INTO STYLE

  *-Open file ,prepare arrays and creat cursor and set the index. 
  IF llWareLoc 
    DIMENSION laSource[1],laTarget[1]
    *E301077,12 open only if not opened.
    IF !laOpFile[1]
      laOpFile[1]=gfOpenFile(gcDataDir+'WhsLoc','WhsLocSt','SH')
    ENDIF
    SELECT WhsLoc
    =AFIELDS(laStru)
    lcTemLoc  = gfTempName()   
    
    CREATE DBF (gcWorkDir+lcTemLoc) FROM ARRAY laStru
    INDEX ON STYLE+CWARECODE+CLOCATION TAG (lcTemLoc) OF (gcWorkDir+lcTemLoc)
    lcWareAny = SPACE(06)
  ENDIF  

  IF !llMultiWH
    *--Read the default warehouse.
    GO TOP IN WAREHOUS
    lcDefWareh = WAREHOUS.cWareCode
  ENDIF
    
  lcFromWare = IIF(llMultiWH, SPACE(6), lcDefWareh )

  IF lcType $ 'AT'
    DIMENSION laTStk[9],laOTStk[9]
    laTStk  = 0
    laOTStk = 0
  ENDIF

  IF llGlLink
    laCodInfo[1,01] = "CADJREASON"
    laCodInfo[1,02] = "laAdjCode"
    laCodInfo[1,03] = "lnAdjCode"
    laCodInfo[1,04] = ""
    laCodInfo[1,05] = .F.
    laCodInfo[1,06] = .F.
    laCodInfo[1,07] = "&lcTmpAdj"
    laCodInfo[1,08] = "&lcTmpAdj"
    IF llDyelot
      laCodInfo[1,09] = "m.Style + m.Dyelot"
    ELSE
      laCodInfo[1,09] = "m.Style"
    ENDIF
    laCodInfo[1,10] = "cAdjReason"
  ENDIF 

  llNoShow = .F.

ENDIF

EXTERNAL ARRAY laDefProc
laDefProc[9]  = .F.      && Disable the control panel save proc.  (lpSavScr)
laDefProc[10] = .F.      && Disable the control panel close proc. (lpClsScr)

*-- Declare the globle variables.
lcBrow_Ttl = "Transaction lines"
lcLevel    = IIF(!llDyelot AND !llMultiWH,'Style','StyDye')

*!C037816,1 MHM 04/06/2004 Define New variables[Start]
IF ASCAN(laEvntTrig,PADR("INITVAR",10)) <> 0 
  *C123853,1 MHM 01/15/2005 handle trigger in physical program too [Start]
  *=gfDoTriger("ICSTYAD",PADR("INITVAR",10))
  IF lcProgName = "ICSTYAD" 
    =gfDoTriger("ICSTYAD",PADR("INITVAR",10))
  ELSE
    =gfDoTriger("ICSTYPH",PADR("INITVAR",10))
  ENDIF
  *C123853,1 MHM 01/15/2005  [End]
  
ENDIF
*!C037816,1 MHM [End]

SELECT (lcTmpAdj)
lnCurrRec = RECNO()

PUSH KEY
*B602921,1 Comment out. 
*ON KEY
*B602921 End.
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrow_Ttl)
DO (gcScrDir+gcWinAppl+"\ICINVT.SPX")
RELEASE WINDOW (lcBrow_Ttl)
POP KEY


*--Normal exit.
IF glQuitting
  SELECT (lcTmpAdj)
  USE
  ERASE (gcWorkDir+lcTmpAdj+'.DBF')
  ERASE (gcWorkDir+lcTmpAdj+'.CDX')
  IF USED(lcTmpGlDt)
    USE IN (lcTmpGlDt)
    ERASE (gcWorkDir+lcTmpGlDt+'.DBF')
  ENDIF
  IF llWareLoc 
    SELECT (lcTemLoc)
    USE
    ERASE (gcWorkDir+lcTemLoc+'.DBF')
    ERASE (gcWorkDir+lcTemLoc+'.CDX')
  ENDIF
  *E301077,12 Close files if opened by this program.
  IF laOpFile[1] AND USED('WhsLoc')
    *USE IN WhsLoc
    =gfCloseFile('WhsLoc')
  ENDIF  
  IF laOpFile[2] AND USED('GLDist')
    *USE IN GLDist
    =gfCloseFile('GLDist')
  ENDIF  
  IF laOpFile[3] AND USED('StyInvJl')
    *USE IN StyInvJl
    =gfCloseFile('StyInvJl')
  ENDIF  
ENDIF
RETURN
*End...



*!*************************************************************
*! Name      : lpShow
*! Developer : Timour A. K.
*! Date      : 07/22/97
*! Purpose   : Show objects function.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lpShow()
*!*************************************************************
FUNCTION lpShow
laCtrStat[10] = 'DISABLE'
SHOW GET pbBrws DISABLE

DO CASE
  CASE laScrMode[1]
    *B802159,1 Enable < New > button if system use single location and does not [Begin]
    *B802159,1                link to GL.
    *B602614,1 Now lcBtNMod be disable in Select mode [Begin]
    IF !llMultiWH AND !llGlLink
      lcBtNMod = 'ENABLE'
    ELSE
      lcBtNMod = 'DISABLE'
    ENDIF
    
    *lcBtNMod = 'DISABLE'
    *B602614,1 Now lcBtNMod be disable in Select mode [End  ]
    *B802159,1 Enable < New > button if system use single location and does not [End  ]

    IF llMultiWH
      SHOW GET lcFromWare ENABLE
      SHOW GET ibWCode1   ENABLE
      IF lcType = 'T'
        SHOW GET lcToWare ENABLE
        SHOW GET ibWCode2 ENABLE
      ENDIF
      
      *B602614,1 Comment out next line because intially declared
      *lcBtNMod = 'DISABLE'
      
      IF !llGlLink
        _CUROBJ = OBJNUM(lcFromWare)
      ENDIF
    ELSE
      
      *B602614,1 Comment out next line because intially declared
      *lcBtNMod = 'ENABLE'
      
      IF !llGlLink
        _CUROBJ = OBJNUM(pbNew)
      ENDIF
    ENDIF
    IF llGlLink
       SHOW GET ldPstDate  ENABLE    
       IF llChkDate
        _CUROBJ = OBJNUM(ldPstDate)
       ELSE
         IF llMultiWH
          _CUROBJ = OBJNUM(lcFromWare)
         ELSE
          _CUROBJ = OBJNUM(pbNew)
         ENDIF
       ENDIF
    ENDIF
    SHOW GET pbKey      DISABLE
    lcBtMode = 'DISABLE'

  CASE laScrMode[4]  
    SELECT (lcTmpAdj)
    lnSavRec=IIF(!EOF(),RECNO(),1)
    LOCATE
    IF !FOUND()
      laCtrStat[11] = 'DISABLE'
      SHOW GET pbSav      DISABLE
      SHOW GET lcFromWare ENABLE
      SHOW GET ibWCode1   ENABLE
      SHOW GET ldPstDate  ENABLE    
      SHOW GET pbKey      DISABLE
      lcBtMode = 'DISABLE'
      IF lcType = 'T'
        SHOW GET ibWCode2 ENABLE
        IF !EMPTY(lcFromWare) AND !EMPTY(lcToWare)
          lcBtNMod = 'ENABLE'
        ELSE
          lcBtNMod = 'DISABLE'
        ENDIF
      ELSE
        IF !EMPTY(lcFromWare)
          lcBtNMod = 'ENABLE'
        ELSE
          lcBtNMod = 'DISABLE'
        ENDIF
      ENDIF
    ELSE
      laCtrStat[11] = 'ENABLE'
      SHOW GET pbSav      ENABLE
      SHOW GET lcFromWare DISABLE
      SHOW GET ibWCode1   DISABLE
      SHOW GET ldPstDate  DISABLE
      SHOW GET pbKey      ENABLE
      IF lcType = 'T'
        SHOW GET lcToWare DISABLE
      SHOW GET ibWCode2   DISABLE
      ENDIF
      lcBtNMod = 'ENABLE'
      lcBtMode = 'ENABLE'
      IF lnSavRec<>1
        GOTO lnSavRec
      ENDIF
    ENDIF  
ENDCASE
SHOW GET pbNew    &lcBtNMod
SHOW GET pbModify &lcBtMode
SHOW GET pbRemove &lcBtMode

RETURN

*!*************************************************************
*! Name      : lfActBrow()
*! Developer : Timour A. K.
*! Date      : 07/22/97
*! Purpose   : Function to activate the browse window.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfActBrow()
*!*************************************************************
FUNCTION lfActBrow

SELECT (lcTmpAdj)
IF llDyelot
  IF lcType = 'T'
    *B606489,1 TMI [Start] Show the long description in the browse
    *lcBfields = [cDummi=IIF(RECNO()=lnCurrRec,'>',' ') :1 :H=' ',]+;
                 "STYLE       :30 :H=lcStyHdr :R ,"+;
                 "STYLE.Desc  :24 :H='Description':10 :R,"+;
                 "Dyelot      :19 :H='Dyelot':R,"+;
                 "TotOld      :10 :H=' '+lcFromWare :R,"+;
                 "TotAdj      :10 :H='Transf.':R,"+;
                 "Total=(IIF(SEEK(Style+lcToWare+Dyelot,'STYDYE'),STYDYE.TotStk,0)+TotAdj) :10 :P='9999999' :H=' '+lcToWare :R "
    lcBfields = [cDummi=IIF(RECNO()=lnCurrRec,'>',' ') :1 :H=' ',]+;
                 "STYLE       :30 :H=lcStyHdr :R ,"+;
                 "STYLE.DESC1 :24 :H='Description':10 :R,"+;
                 "Dyelot      :19 :H='Dyelot':R,"+;
                 "TotOld      :10 :H=' '+lcFromWare :R,"+;
                 "TotAdj      :10 :H='Transf.':R,"+;
                 "Total=(IIF(SEEK(Style+lcToWare+Dyelot,'STYDYE'),STYDYE.TotStk,0)+TotAdj) :10 :P='9999999' :H=' '+lcToWare :R "
    *B606489,1 TMI [End  ] 
  ELSE
    *B603049,4 ARH 07/06/1999 (Begin) Added the old cost & the new cost to 
    *B603049,4                the main browse in case of physical or adjustment.
    *lcBfields = [cDummi=IIF(RECNO()=lnCurrRec,'>',' ') :1 :H=' ',]+;
                 "STYLE        :30 :H=lcStyHdr :R ,"+;
                 "STYLE.Desc   :24 :H='Description' :R,"+;
                 "Dyelot       :19 :H='Dyelot':R,"+;
                 "TotOld       :10  :H='Existing':R,"+;
                 "TotAdj       :10  :H=IIF(lcType='P','New Inv','Adj +\-'):R,"+;
                 "Total=IIF(lcType='P',TotAdj,(TotOld+TotAdj)) :10:P='9999999' :H='  Total':R "
    *B606489,1 TMI [Start] 
    *lcBfields = [cDummi=IIF(RECNO()=lnCurrRec,'>',' ') :1 :H=' ',]+;
                 "STYLE        :30 :H=lcStyHdr :R ,"+;
                 "STYLE.Desc   :24 :H='Description' :R,"+;
                 "Dyelot       :19 :H='Dyelot':R,"+;
                 "TotOld       :10  :H='Existing':R,"+;
                 "Old_Cost     :09:P='999999.99'  :H='Old Cost':R,"+;
                 "TotAdj       :10  :H=IIF(lcType='P','New Inv','Adj +\-'):R,"+;
                 "Unt_Cost     :09:P='999999.99'  :H='New Cost':R,"+;
                 "Total=IIF(lcType='P',TotAdj,(TotOld+TotAdj)) :10:P='9999999' :H='  Total':R "
    lcBfields = [cDummi=IIF(RECNO()=lnCurrRec,'>',' ') :1 :H=' ',]+;
                 "STYLE        :30 :H=lcStyHdr :R ,"+;
                 "STYLE.DESC1  :24 :H='Description' :R,"+;
                 "Dyelot       :19 :H='Dyelot':R,"+;
                 "TotOld       :10  :H='Existing':R,"+;
                 "Old_Cost     :09:P='999999.99'  :H='Old Cost':R,"+;
                 "TotAdj       :10  :H=IIF(lcType='P','New Inv','Adj +\-'):R,"+;
                 "Unt_Cost     :09:P='999999.99'  :H='New Cost':R,"+;
                 "Total=IIF(lcType='P',TotAdj,(TotOld+TotAdj)) :10:P='9999999' :H='  Total':R "
    *B606489,1 TMI [End  ] 
    *B603049,4 ARH 07/06/1999 (End)

  ENDIF
ELSE
  IF lcType = 'T'
    *B606489,1 TMI [Start] 
    *lcBfields = [cDummi=IIF(RECNO()=lnCurrRec,'>',' ') :1 :H=' ',]+;
                 "STYLE       :30 :H=lcStyHdr :R,"+;
                 "STYLE.Desc  :24 :H='Description':R,"+;
                 "TotOld      :12 :H=' '+lcFromWare :R,"+;
                 "TotAdj      :12 :H='Transfer':R,"+;
                 "nTotOldTo   :12 :P='9999999' :H=' '+lcToWare :R "
    lcBfields = [cDummi=IIF(RECNO()=lnCurrRec,'>',' ') :1 :H=' ',]+;
                 "STYLE       :30 :H=lcStyHdr :R,"+;
                 "STYLE.DESC1 :24 :H='Description':R,"+;
                 "TotOld      :12 :H=' '+lcFromWare :R,"+;
                 "TotAdj      :12 :H='Transfer':R,"+;
                 "nTotOldTo   :12 :P='9999999' :H=' '+lcToWare :R "
    *B606489,1 TMI [End  ] 
  ELSE
    *B603049,4 ARH 07/06/1999 (Begin) Added the old cost & the new cost to 
    *B603049,4                the main browse in case of physical or adjustment.
    *lcBfields = [cDummi=IIF(RECNO()=lnCurrRec,'>',' ') :1 :H=' ',]+;
                 "STYLE       :30 :H=lcStyHdr :R,"+;
                 "STYLE.Desc  :24 :H='Description':R,"+;
                 "TotOld      :12 :H='Existing':R,"+;
                 "TotAdj      :12 :H=IIF(lcType='P','New Inv','Adj +\-'):R,"+;
                 "Total=IIF(lcType='P',TotAdj,(TotOld+TotAdj)) :12 :P='9999999' :H='  Total':R "
    *B606489,1 TMI [Start] 
    *lcBfields = [cDummi=IIF(RECNO()=lnCurrRec,'>',' ') :1 :H=' ',]+;
                 "STYLE       :30 :H=lcStyHdr :R,"+;
                 "STYLE.Desc  :24 :H='Description':R,"+;
                 "TotOld      :12 :H='Existing':R,"+;
                 "Old_Cost    :09:P='999999.99' :H='Old Cost':R,"+;
                 "TotAdj      :12 :H=IIF(lcType='P','New Inv','Adj +\-'):R,"+;
                 "Unt_Cost    :09:P='999999.99' :H='New Cost':R,"+;
                 "Total=IIF(lcType='P',TotAdj,(TotOld+TotAdj)) :12 :P='9999999' :H='  Total':R "
    lcBfields = [cDummi=IIF(RECNO()=lnCurrRec,'>',' ') :1 :H=' ',]+;
                 "STYLE       :30 :H=lcStyHdr :R,"+;
                 "STYLE.DESC1 :24 :H='Description':R,"+;
                 "TotOld      :12 :H='Existing':R,"+;
                 "Old_Cost    :09:P='999999.99' :H='Old Cost':R,"+;
                 "TotAdj      :12 :H=IIF(lcType='P','New Inv','Adj +\-'):R,"+;
                 "Unt_Cost    :09:P='999999.99' :H='New Cost':R,"+;
                 "Total=IIF(lcType='P',TotAdj,(TotOld+TotAdj)) :12 :P='9999999' :H='  Total':R "
    *B606489,1 TMI [End  ]                   
    *B603049,4 ARH 07/06/1999 (End)

  ENDIF
ENDIF
*!C037816,1 MHM 04/06/2004 Validate Browse[Start]
IF ASCAN(laEvntTrig,PADR("DLMODBRW",10)) <> 0 
  =gfDoTriger("ICSTYTR",PADR("DLMODBRW",10))
ENDIF
*!C037816,1 [END]

lcBrowWind=IIF(!llMultiWH AND !llGlLink,lcWinCh3,lcWinCh1)
BROWSE FIELDS &lcBfields;
       NOAPPEND ;
       NOCLEAR  ;
       NODELETE ;
       NOMENU   ;
       NOWAIT   ;
       SAVE     ;
       TITLE (lcBrow_Ttl) ;
       WHEN lfwBrow() ;
       VALID :F lfvBrows();
       WINDOW (lcBrowWind);
       IN WINDOW (gcBaseWind)
RETURN

*!*************************************************************
*! Name      : lfwBrow
*! Developer : Timour A. K.
*! Date      : 07/22/97
*! Purpose   : When Browse function.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfwBrow()
*!*************************************************************
FUNCTION lfwBrow
lnCurrRec = RECNO()
SHOW WINDOW (lcBrow_Ttl) REFRESH
RETURN

*!*************************************************************
*! Name      : lfvBrows
*! Developer : Timour A. K.
*! Date      : 07/22/97
*! Purpose   : Valid Browse function.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvBrows()
*!*************************************************************
FUNCTION lfvBrows

IF WONTOP() # (lcBrow_Ttl)
  = gfStopBrow()
ENDIF

*!*************************************************************
*! Name      : lfReadAct
*! Developer : Timour A. K.
*! Date      : 07/22/97
*! Purpose   : Read Activate function clear traping.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfReadAct()
*!*************************************************************
FUNCTION lfReadAct

ON KEY LABEL TAB
ON KEY LABEL BACKTAB
*B606564,1 TMI [Start] Reset the relation if released for any reason (e.g. open style screen)
IF !'STYLE' $ RELATION(1,lcTmpAdj)
  PRIVATE lnSlct
  lnSlct = SELECT()
  SELECT (lcTmpAdj)
  SET RELATION TO STYLE INTO STYLE ADDITIVE IN (lcTmpAdj)
  =lfwBrow()
  SELECT (lnSlct)
ENDIF  
*B606564,1 TMI [End  ] Reset the relation if released for any reason 


*:*************************************************************
*! Name      : lfTrapKy
*! Developer : Timour A. K.
*! Date      : 07/22/97
*! Purpose   : Trap key.
*:*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfTrapKy()
*!*************************************************************
FUNCTION lfTrapKy

IF WONTOP() = lcBrow_Ttl
  ON KEY LABEL TAB     DO lptab
  ON KEY LABEL backtab DO lpshifttab
ENDIF  
RETURN

*!*************************************************************
*! Name      : lfUnTrapKeys
*! Developer : Timour A. K.
*! Date      : 07/22/97
*! Purpose   : Releases Key traps necessary for browse.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfUnTrapKeys()
*!*************************************************************
FUNCTION lfUnTrapKeys

ON KEY LABEL TAB        
ON KEY LABEL BACKTAB    
ON KEY LABEL ESC DO lpDetEsc
RETURN


*!*************************************************************
*! Name      : lfActBrWin
*! Developer : Timour A. K.
*! Date      : 07/22/97
*! Purpose   : key trapping for activate browse.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfActBrWin()
*!*************************************************************
FUNCTION lfActBrWin 

IF !WONTOP(lcBrow_Ttl)
  ACTIVATE WINDOW (lcBrow_Ttl)
ENDIF
RETURN


*!*************************************************************
*! Name      : lpTab
*! Developer : Timour A. K.
*! Date      : 07/22/97
*! Purpose   : Tab key trapping.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  DO lpTab
*!*************************************************************
PROCEDURE lpTab 

IF WONTOP(lcBrow_Ttl)
  _CUROBJ=OBJNUM(pbNew)
  ACTIVATE WINDOW (lcWinCh2)
ELSE
  _CUROBJ=_CUROBJ+1
ENDIF
RETURN


*!*************************************************************
*! Name      : lpShiftTab
*! Developer : Timour A. K.
*! Date      : 07/22/97
*! Purpose   : Shift Tab key trapping.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  DO lpShiftTab
*!*************************************************************
PROCEDURE lpShiftTab 

IF WONTOP(lcBrow_Ttl)
  ACTIVATE WINDOW ("gwccontrl1")
  _CUROBJ=OBJNUM(pbCls)
ENDIF
RETURN


*!*************************************************************
*! Name      : lfOldValue
*! Developer : Timour A. K.
*! Date      : 07/22/97
*! Purpose   : Function to store old value of the current filed.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfoldvalue()
*!*************************************************************
FUNCTION lfoldvalue

lcOldValue = EVALUATE(SYS(18))
RETURN


*!*************************************************************
*! Name      : lfvWareH()
*! Developer : Timour A. K.
*! Date      : 07/22/97
*! Purpose   : Validate the warehouses.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: lcWareH  -> Warehouse code.
*!             lnWareNm -> Which warehouse ,1st or 2nd.
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvWareH()
*!*************************************************************
FUNCTION lfvWareH
PARA lcWareH,lnWareNm

*B802944,1 [Start] Return after saving without enter function when press exit
IF MDOWN()
  RETURN
ENDIF
*B802944,1 [End] 

PRIVATE lcWareH,lcWareDesc



IF llBrowse .OR. (EMPTY(lcWareH) AND laScrMode[4] ) .OR.;
                 (!EMPTY(lcWareH) AND !SEEK(lcWareH,'WareHous') )
  = gfBrowWare(.F.)
  lcWareH = WareHous.cWareCode
ENDIF
lcWareDesc = IIF(!EMPTY(lcWareH),WareHous.cDesc,' ')
llBrowse = .F.

IF lnWareNm = 1
  lcFromWare = lcWareH
  lcWFromDsc = lcWareDesc
ELSE
  lcToWare   = lcWareH
  lcWToDsc   = lcWareDesc
ENDIF

IF lcType = 'T'
  IF !EMPTY(lcFromWare) AND !EMPTY(lcToWare)
  
    *!C037816,1 MHM 04/06/2004 Check for same bin location or not[Start]
    *IF lcFromWare = lcToWare
    *  *--You cannot transfer from the same warehouse.
    *  =gfModalGen('TRM42055B42001','DIALOG')
    *  RETURN .F.
    *ELSE
    *  IF !laScrMode[4]
    *    laScrMode = .F.
    *    laScrMode[4] = .T.
    *    =lfActBrow()
    *    llCUpdate = .T.
    *    SHOW GETS   
    *  ENDIF
    *ENDIF
    *B039660,1 NNA 09/04/2005 (Begin) Decrease the length of the triger to be less or equal 10 Chr.
    *IF ASCAN(laEvntTrig,PADR("DLCHKADJTR",10)) <> 0 
    *  =gfDoTriger("ICSTYTR",PADR("DLCHKADJTR",10))
    *ELSE
    IF ASCAN(laEvntTrig,PADR("DLCHKTR",10)) <> 0 
      =gfDoTriger("ICSTYTR",PADR("DLCHKTR",10))
    ELSE
    *B039660,1 NNA (End)
    *!C037816,1 MHM [End]
      IF lcFromWare = lcToWare
        *--You cannot transfer from the same warehouse.
        =gfModalGen('TRM42055B42001','DIALOG')
        RETURN .F.
      ELSE
        IF !laScrMode[4]
          laScrMode = .F.
          laScrMode[4] = .T.
          =lfActBrow()
          llCUpdate = .T.
          SHOW GETS   
        ENDIF
      ENDIF
    ENDIF  
  *!C037816,1 MHM 04/06/2004 Check for same bin location or not[Start]
  ENDIF 
  *!C037816,1 MHM [End]
  
ELSE
  *C037816,1 MHM 04/06/2004 Check for same bin location or not[Start]
  *B039660,1 NNA 09/04/2005 (Begin) Decrease the length of the triger to be less or equal 10 Chr.
  *IF ASCAN(laEvntTrig,PADR("DLCHKADJAD",10)) <> 0 
  *  *C123853,1 MHM 01/15/2005 handle trigger in physical program too [Start]
  *  *=gfDoTriger("ICSTYAD",PADR("DLCHKADJAD",10))
  *  IF lcProgName = "ICSTYAD" 
  *    =gfDoTriger("ICSTYAD",PADR("DLCHKADJAD",10))
  *  ELSE
  *    =gfDoTriger("ICSTYPH",PADR("DLCHKADJAD",10))
  *  ENDIF
  *  *C123853,1 MHM 01/15/2005  [End]
  *ELSE
  IF ASCAN(laEvntTrig,PADR("DLCHKADJ",10)) <> 0 
    IF lcProgName = "ICSTYAD" 
      =gfDoTriger("ICSTYAD",PADR("DLCHKADJ",10))
    ELSE
      =gfDoTriger("ICSTYPH",PADR("DLCHKADJ",10))
    ENDIF
  ELSE
  *B039660,1 NNA (End)

  *C037816,1 MHM [End]

    IF !EMPTY(lcFromWare)
      laScrMode[1] = .F.
      laScrMode[4] = .T.
      llCUpdate = .T.
      SHOW GETS   
   ENDIF

  *!C037816,1 MHM 04/06/2004 Check for same bin location or not[Start]
  ENDIF
  *!C037816,1 MHM [End]
ENDIF

*C037816,1 MHM 04/06/2004 Get Bin Location[Start]
IF ASCAN(laEvntTrig,PADR("DLGETBIN",10)) <> 0 
  *C123853,1 MHM 01/15/2005 handle trigger in physical program too [Start]
  *=gfDoTriger("ICSTYAD",PADR("DLGETBIN",10))
  IF lcProgName = "ICSTYAD" 
    =gfDoTriger("ICSTYAD",PADR("DLGETBIN",10))
  ELSE
    =gfDoTriger("ICSTYPH",PADR("DLGETBIN",10))
  ENDIF  
  *C123853,1 MHM 01/15/2005 [End]
ENDIF
*C037816,1 MHM [End]

=lfRefresh()
RETURN

*:*************************************************************
*! Name    : lfPstDate
*! Developer : Timour A. K.
*! Date      : 07/22/97
*! Purpose : Valid Posting date.
*:*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfPstDate()
*!*************************************************************
FUNCTION lfPstDate

*-- If the user pressed "Enter" and did not use the mouse..
*-- We usually use this condition to validate all the key fields..

*B602614,1 04/19/1999 If user press tab no validation done [Begin]
*IF LASTKEY() = 13 AND !MDOWN()
IF !MDOWN()
*B602614,1 04/19/1999 If user press tab no validation done [End  ]
  IF ldPstDate<>lcOldValue
    llChkDate=.T.
  ENDIF
  IF llChkDate AND !CHECKPRD(ldPstDate,'lcGLFYear','lcGLPeriod',IIF(lcType='P','IP','IA'))
    *B604753,1 ASH 08/01/2001  (Begin) Return the system date if the entered date is invalid.
    ldPstDate = gdSysDate
    *B605018,1 MHM 10/10/2001  Fix the bug of Wrong period Update [Start]
    =CHECKPRD(ldPstDate,'lcGLFYear','lcGLPeriod',IIF(lcType='P','IP','IA'),.T.)
    *B605018,1 MHM 10/10/2001  [End]
    
    *B604753,1 ASH 08/01/2001  (End)
    _CUROBJ = OBJNUM(ldPstDate)
    RETURN
    
  *B602614,1 Add the following ELSE condition to enable New button [Begin]
  ELSE

    IF !llMultiWH
      lcBtNMod = 'ENABLE'
      SHOW GET pbNew    &lcBtNMod
    ENDIF  

  *B602614,1 Add the following ELSE condition to enable New button [End  ]
  ENDIF
  llChkDate = .F.
ENDIF
RETURN


*:*************************************************************
*! Name    : lfVldDate
*! Developer : Timour A. K.
*! Date      : 07/22/97
*! Purpose : Valid Transaction date.
*:*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfVldDate()
*!*************************************************************
FUNCTION lfVldDate

IF llGlLink AND ldDate>ldPstDate
  *--The xxxx date cannot be after than the posting date.
  = gfModalGen('TRM42106B42000','DIALOG','adjustment')
  _CUROBJ=OBJNUM(ldDate)
  RETURN
ENDIF


*:*************************************************************
*! Name    : lfHoldKey
*! Developer : Timour A. K.
*! Date      : 07/22/97
*! Purpose : Save and restore traping.
*:*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: lcKyOper -> Operation Save or Restore
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfHoldKey()
*!*************************************************************
FUNCTION lfHoldKey
PARA lcKyOper

DO CASE
  CASE lcKyOper='S'
    lcHldTab = ON('KEY','TAB')
    lcHldBtb = ON('KEY','BACKTAB')    
    lcHldEsc = ON('KEY','ESC')
    lcHldAtB = ON('KEY','ALT+B')
  CASE lcKyOper='R'
    ON KEY LABEL TAB     &lcHldTab
    ON KEY LABEL BACKTAB &lcHldBtb
    ON KEY LABEL ESC     &lcHldEsc
    ON KEY LABEL ALT+B   &lcHldAtB
ENDCASE


*!*************************************************************
*! Name      : lpClsScr
*! Developer : Timour A. K.
*! Date      : 07/22/97
*! Purpose   : Cancel P/O.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lpClsScr()
*!*************************************************************
FUNCTION lpClsScr

SELECT (lcTmpAdj)
GO TOP
IF !EOF()
  ZAP
  lnCurrRec=RECNO()
  GO TOP
  =lfActBrow()
ENDIF
STORE ' ' TO lcToWare,lcWFromDsc,lcWToDsc,lcReason,lcDyelot,;
             lcGlFYear,lcGlPeriod,lcWareAny,lcAReason,lcRefer
STORE 0   TO laTStk,laOTStk,lnNewCost,lnOldCost

lcFromWare = IIF(llMultiWH, SPACE(6), lcDefWareh )
llLoc  = .F.
laScrMode = .F.
laScrMode[1] = .T.

llCUpdate = .F.
SHOW GETS
=lfRefresh()
RETURN



*!*************************************************************
*! Name      : lfvNew()
*! Developer : Timour A. K.
*! Date      : 07/22/97
*! Purpose   : Add new lines.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvNew()
*!*************************************************************
FUNCTION lfvNew

*B039660,1 NNA 12/20/2005 (Begin) Replace the Next Codes with only 2 lines that because
*B039660,1 NNA            Icinvta.prg deals only with ICSTYAD,ICSTYPH,ICSTYTR
*C037816,1 MHM 04/06/2004 Check for Bin Location [Start]
*IF ASCAN(laEvntTrig,PADR("DLCKSVIC",10)) <> 0
*  *IF gfDoTriger("ICSTYAD",PADR("DLCKSVIC",10))
*  IF lcProgName = "ICSTYAD" AND  gfDoTriger("ICSTYAD",PADR("DLCKSVIC",10))
*    RETURN
*  ENDIF
*  *C123853,1 MHM 01/15/2005 handle trigger in physical program too [Start]
*  IF lcProgName = "ICSTYPH" AND  gfDoTriger("ICSTYPH",PADR("DLCKSVIC",10))
*    RETURN
*  ENDIF
*  *C123853,1 MHM 01/15/2005 [End]
*ENDIF
**C037816,1 [End]
**C037816,1 MHM 04/06/2004 Check for Bin Location [Start]
*IF ASCAN(laEvntTrig,PADR("DLCKSVIC",10)) <> 0
*  *IF gfDoTriger("ICSTYTR",PADR("DLCKSVIC",10))
*  IF lcProgName = "ICSTYTR" AND gfDoTriger("ICSTYTR",PADR("DLCKSVIC",10))
*    RETURN
*  ENDIF
*ENDIF
*C037816,1 [End]
IF ASCAN(laEvntTrig,PADR("DLCKSVIC",10)) <> 0
  IF gfDoTriger(lcProgName,PADR("DLCKSVIC",10))
    RETURN
  ENDIF
ENDIF
*B039660,1 NNA (End)

llModiMod = .F.
llExtMFlg = .F.
SELECT (lcTmpAdj)
lcScFields = 'Style,Dyelot,Adj1,Adj2,Adj3,Adj4,Adj5,Adj6,Adj7,Adj8,TotAdj'
SCATTER FIELDS &lcScFields MEMO MEMVAR BLANK
STORE 0 TO m.Stk1,m.Stk2,m.Stk3,m.Stk4,m.Stk5,m.Stk6,m.Stk7,m.Stk8,m.TotStk
IF lcType = 'T'
  laTStk  = 0
  laOTStk = 0
ENDIF
IF lcType = 'A'
  laTStk  = 0
ENDIF

m.Style = SPACE(19)
ldDate  = gdSysDate
lcStyDesc = ' '
llLoc = .F.
*E302005,1 (Begin) False it for each new style.
llNetAll    = .F.
*E302005,1 (End)
*PUSH KEY
=lfHoldKey('S')
=lfUnTrapKeys()
DO (gcScrDir+gcWinAppl+"\ICINVT_A.SPX")
*POP KEY
=lfHoldKey('R')

SELECT (lcTmpAdj)
IF EOF()
  GO TOP
ENDIF
lnCurrRec = RECNO()
GO TOP
=lfActBrow()

IF !EOF()
  GOTO lnCurrRec
  _CUROBJ = OBJNUM(pbNew)

  IF !llMultiWH
    laScrMode = .F.
    laScrMode[4] = .T.
    llCUpdate = .T.
  ENDIF

ELSE
  
  IF !llMultiWH
    laScrMode = .F.
    laScrMode[1] = .T.
    llCUpdate = .F.
  ENDIF

ENDIF
SHOW GETS

*B602614,1 Show current object is post date object if select mode [Begin]
IF !llMultiWH AND laScrMode[1]
  _CUROBJ = OBJNUM(ldPstDate)
ENDIF  
*B602614,1 Show current object is post date object if select mode [End  ]
RETURN

*!*************************************************************
*! Name      : lpDetEsc
*! Developer : Timour A. K.
*! Date      : 07/22/97
*! Purpose   : Trap Esc for lines entry.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  DO lpDetEsc
*!*************************************************************
PROCEDURE lpDetEsc
_CUROBJ = OBJNUM(pbNClose)
KEYBOARD '{ENTER}'
RETURN

*!*************************************************************
*! Name      : lfvStyle()
*! Developer : Timour A. K.
*! Date      : 07/22/97
*! Purpose   : Valid function to validate style field.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvStyle()
*!*************************************************************
FUNCTION lfvStyle

IF MDOWN() OR (!llBrowse AND EMPTY(SUBSTR(m.Style,1,lnMjrWid)) )
  RETURN
ENDIF

SELECT STYLE 
m.Style=IIF(llBrowse ,'?',m.Style)
IF llBrowse OR (!EMPTY(m.Style) AND !SEEK(m.Style,'STYLE'))
  lcHdEsc1= ON('KEY','ESC')
  ON KEY LABEL ESC 
  
  *B803150,1 KHM 05/16/2000 (Begin) Passing the entered Style to gfStyBrw
  *B803150,1                in order to point to the first appearance of 
  *B803150,1                the style or the nearest record.
  *m.Style = gfStyBrw('I','','',.F.)
  m.Style = gfStyBrw('I',m.Style,'',.F.)
  *B803150,1 KHM 05/16/2000 (End)
  
  ON KEY LABEL ESC &lcHdEsc1
  llbrowse = .F.
  IF EMPTY(m.Style)
    _CUROBJ = OBJNUM(m.Style)
    RETURN
  ENDIF
ENDIF

IF EMPTY(m.Style)
  =lfClrInfo()
  *_CUROBJ = OBJNUM(pbNClose)
  RETURN
ENDIF

*B608299,1 NNA 10/01/2007 (Begin) if this is a non inventory style then show a message and not proceed
IF !STYLE.LINVSTY
  =gfModalGen('TRM42266B00000','DIALOG')
  RETURN 
ENDIF
*B608299,1 NNA (End)

*C037816,1 MHM 04/06/2004 Validate Style [Start]
IF ASCAN(laEvntTrig,PADR("DLVLDSTY",10)) <> 0 
  *=gfDoTriger("ICSTYAD",PADR("DLVLDSTY",10))
  IF lcProgName = "ICSTYAD" AND gfDoTriger("ICSTYAD",PADR("DLVLDSTY",10))
    RETURN
  ENDIF

  *C123853,1 MHM 01/15/2005 handle trigger in physical program too [Start]
  IF lcProgName = "ICSTYPH" AND gfDoTriger("ICSTYPH",PADR("DLVLDSTY",10))
    RETURN
  ENDIF
  *C123853,1 MHM 01/15/2005  [Start]
  
ENDIF
IF ASCAN(laEvntTrig,PADR("TRVLDSTY",10)) <> 0 
  *=gfDoTriger("ICSTYTR",PADR("TRVLDSTY",10))
  IF lcProgName = "ICSTYTR" AND gfDoTriger("ICSTYTR",PADR("TRVLDSTY",10))
    RETURN
  ENDIF
ENDIF
*!C037816,1 MHM [End]

IF !llDyelot OR Style.cDye_Flg<>'Y'
  =lfGetInfo()
ELSE
  lcStyDesc = Desc1
  SHOW GET ibDye    ENABLE
  SHOW GET m.Dyelot ENABLE
  =lfRefresh('ICINVT_A')
ENDIF
RETURN


*!*************************************************************
*! Name      : lfvDyelot()
*! Developer : Timour A. K.
*! Date      : 07/22/97
*! Purpose   : Valid function to validate the dyelot.
*!*************************************************************
*! Calls     : DO gpStDyBrow WITH lcStyle,lcWareHous,lcDyelot
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvDyelot()
*!*************************************************************
FUNCTION lfvDyelot

IF MDOWN() OR (!llBrowse AND EMPTY(m.Dyelot) )
  RETURN
ENDIF

lcDyelot=IIF(llBrowse ,'?',m.Dyelot)
IF llBrowse OR (!EMPTY(m.lcDyelot) AND !SEEK(PADR(m.Style,19)+lcFromWare+lcDyelot,'STYDYE'))
   DO gpStDyBrow WITH m.Style,lcFromWare,lcDyelot
ENDIF

llBrowse = .F.
IF EMPTY(lcDyelot)
  m.Dyelot = ' '
  SHOW GET ibDye    DISABLE
  SHOW GET m.Dyelot DISABLE
  _CUROBJ = OBJNUM(m.Style)
  RETURN
ENDIF

m.Dyelot=lcDyelot
IF lcType='T' AND !SEEK(PADR(m.Style,19)+lcToWare+lcDyelot,'STYDYE')
  *--Style/Dyelot :XXX/XXX is not assigned to warehouse : XXX \<Add;\<Reenter
  lnChoice=gfModalGen('TRM42062B42006','DIALOG',ALLTRIM(m.Style)+'|'+ALLTRIM(lcDyelot)+'|'+lcToWare)
  IF lnChoice = 1
    IF !SEEK(PADR(m.Style,19)+lcToWare+SPACE(10),'STYDYE')
      DO gpAdStyWar WITH m.Style,SPACE(10),lcToWare
    ENDIF
    DO gpAdStyWar WITH m.Style,lcDyelot,lcToWare
  ELSE   
    _CUROBJ = OBJNUM(m.Dyelot)
    RETURN
  ENDIF
ENDIF
=SEEK(PADR(m.Style,19)+lcFromWare+lcDyelot,'STYDYE')
laScrMode=.F.
laScrMode[4]=.T.

=lfGetInfo()
_CUROBJ = OBJNUM(ldDate)
=lfRefresh('ICINVT_A')
SHOW GET ibDye    DISABLE
SHOW GET m.Dyelot DISABLE

RETURN


*!*************************************************************
*! Name      : lfGetInfo()
*! Developer : Timour A. K.
*! Date      : 07/22/97
*! Purpose   : Get the style key accepted information.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfGetInfo()
*!*************************************************************
FUNCTION lfGetInfo

SELECT STYLE
SEEK m.Style
lcSScale  = Scale
lcStyDesc = Desc1
SELECT (lcLevel)
IF lcLevel<>'Style'
    
  IF !SEEK(PADR(m.Style,19)+lcFromWare+SPACE(10) ) .AND. llMultiWH
    IF lcType = 'P'
      *-Style: xxx is not assigned to warehouse: xxx. "\<Add;\<Reenter"
      lnChoice=gfModalGen('TRM42056B42006','DIALOG',ALLTRIM(m.Style)+'|'+lcFromWare)
    ELSE
      *-Style: xxx is not assigned to warehouse: xxx,"\<Reenter"
      =gfModalGen('TRM42056B42005','DIALOG',ALLTRIM(m.Style)+'|'+lcFromWare)
      lnChoice = 2
    ENDIF
    IF lnChoice = 1
      DO gpAdStyWar WITH m.Style,SPACE(10),lcFromWare
    ELSE
      m.Style = Style
      SHOW GET m.Style ENABLE
      SHOW GET ibStyle ENABLE
      =lfClrInfo()
      _CUROBJ = OBJNUM(m.Style)
      RETURN 
    ENDIF
  ENDIF
ENDIF

SELECT (lcLevel)
IF lcType='T'
  = SEEK(PADR(m.Style,19)+lcFromWare+lcDyelot) 
  llTransfer = .F.
  FOR I = 1 TO 8
    lcCount = STR(I,1)
    IF STYDYE.STK&lcCount > 0
      llTransfer = .T.
      EXIT
    ENDIF 
  ENDFOR

  IF TotStk <= 0 AND !llTransfer
    *--No stock available to transfer.
    = gfModalGen('TRM42057B42001','DIALOG')
    m.Style = Style
    SHOW GET m.Style ENABLE
    SHOW GET ibStyle ENABLE
    =lfClrInfo()
    _CUROBJ = OBJNUM(m.Style)
    RETURN 
  ENDIF

  IF !SEEK(PADR(m.Style,19)+lcToWare+SPACE(10))
    *-Style: xxx is not assigned to warehouse: xxx. "\<Add;\<Reenter"
    lnChoice=gfModalGen('TRM42056B42006','DIALOG',ALLTRIM(m.Style)+'|'+lcToWare)
    
    IF lnChoice = 1
      DO gpAdStyWar WITH m.Style,SPACE(10),lcToWare
      IF llDyelot AND Style.cDye_Flg='Y'
        DO gpAdStyWar WITH m.Style,lcDyelot,lcToWare
      ENDIF
    ELSE
      m.Style = Style
      SHOW GET m.Style ENABLE
      SHOW GET ibStyle ENABLE
      =lfClrInfo()
      _CUROBJ = OBJNUM(m.Style)
      RETURN 
    ENDIF
  ENDIF
  *--To Warehouse stock.
  SELECT (lcLevel)
  =SEEK(PADR(m.Style,19)+lcToWare+lcDyelot )
  SCATTER FIELDS Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8,TotStk TO laTStk
  SCATTER FIELDS Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8,TotStk TO laOTStk
ENDIF
IF lcType='A'
  SCATTER FIELDS Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8,TotStk TO laTStk
ENDIF

SHOW GET m.Style DISABLE
SHOW GET ibStyle DISABLE


llContK = .F.
SELECT (lcLevel)
IF llDyelot AND llMultiWH AND lcCostMth<>'S' AND !llModiMod
  = SEEK(PADR(m.Style,19)+lcFromWare) 
  lnOldCost = Ave_Cost
  llContK = .T.
ENDIF
IF llMultiWH
  = SEEK(PADR(m.Style,19)+lcFromWare+lcDyelot) 
ELSE
  IF llDyelot
    = SEEK(PADR(m.Style,19)+lcDefWareh+lcDyelot) 
  ELSE
    = SEEK(m.Style) 
  ENDIF
ENDIF
*B126587,1 EIH 03/16/2005 It must Inventory less Allocated. " note:  stk# = stk# - alo# " [Begin]
*SCATTER FIELDS Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8,TotStk  MEMVAR
SCATTER FIELDS Stk1,alo1,Stk2,alo2,Stk3,alo3,Stk4,alo4,Stk5,alo5,Stk6,alo6,Stk7,alo7,Stk8,alo8,TotStk,totalo MEMVAR
m.Stk1 = m.Stk1 - m.alo1
m.Stk2 = m.Stk2 - m.alo2 
m.Stk3 = m.Stk3 - m.alo3
m.Stk4 = m.Stk4 - m.alo4
m.Stk5 = m.Stk5 - m.alo5 
m.Stk6 = m.Stk6 - m.alo6
m.Stk7 = m.Stk7 - m.alo7
m.Stk8 = m.Stk8 - m.alo8
TotStk = TotStk - totalo 
*B126587,1 EIH 03/16/2005 [End]
IF !llModiMod
  IF !llContK
    IF llDyelot 
      lnOldCost = IIF(lcCostMth<>'S',STYLE.Ave_Cost,STYLE.TotCost)
    ELSE
      lnOldCost = IIF(lcCostMth<>'S',&lcLevel->Ave_Cost,STYLE.TotCost)  
    ENDIF
  ENDIF 
  lnNewCost = lnOldCost
  IF lcType = 'P'
    FOR I=1 TO 8    
      Z=STR(I,1)
      m.Adj&Z=m.Stk&Z
    ENDFOR
  ENDIF
ENDIF

IF llGlLink
  =gfwCodePop(@laCodInfo, "CADJREASON", "D") 
  lcAReason = laAdjCode[lnAdjCode,2]
ENDIF

=SEEK('S'+lcSScale,'SCALE')
SELECT (lcTmpAdj)
SHOW GET pbNewOk    ENABLE
SHOW GET pbNClose,1 PROMPT gcBmpHome+"can.bmp"
IF llDyelot AND Style.cDye_Flg='Y'
  SHOW GET ibDye    ENABLE
  SHOW GET m.Dyelot ENABLE
ELSE
  SHOW GET ibDye    DISABLE
  SHOW GET m.Dyelot DISABLE
ENDIF 
SHOW GET ldDate     ENABLE
SHOW GET lcReason   ENABLE  
SHOW GET lcRefer    ENABLE
SHOW GET lnAdjCode  ENABLE  
SHOW GET llLoc      ENABLE
SHOW GET lnOldCost  DISABLE
SHOW GET m.Adj1     ENABLE
FOR I=2 TO 8
  Z=STR(I,1)
  IF !EMPTY(SCALE.Sz&Z)
    SHOW GET m.Adj&Z ENABLE
  ELSE
    m.Adj&Z = 0
    SHOW GET m.Adj&Z DISABLE
  ENDIF
ENDFOR
SHOW GET m.TotAdj DISABLE
IF lcType='A' .AND. m.TotAdj <= 0
  lnNewCost=lnOldCost
  SHOW GET lnNewCost DISABLE
ELSE
  *B039107,1 EIH 03/03/2005 Fix bug of disable the NewCost field in case of standard cost method [start]
  *SHOW GET lnNewCost ENABLE
  IF !(ALLTRIM(lcCostMth) == 'S')
    SHOW GET lnNewCost ENABLE
  ENDIF
  *B039107,1 EIH 03/03/2005 [end]
ENDIF
llExtMFlg = .T.
llCupdate=.T.
=lfRefresh('ICINVT_A')
RETURN


*!*************************************************************
*! Name      : lfClrInfo()
*! Developer : Timour A. K.
*! Date      : 07/22/97
*! Purpose   : Get the style key accepted information.
*!             or initial information.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfClrInfo()
*!*************************************************************
FUNCTION lfClrInfo

STORE 0 TO m.Stk1,m.Stk2,m.Stk3,m.Stk4,m.Stk5,m.Stk6,m.Stk7,m.Stk8,m.TotStk
STORE 0 TO m.unt_cost,m.Adj1,m.Adj2,m.Adj3,m.Adj4,m.Adj5,m.Adj6,m.Adj7,m.Adj8,m.TotAdj
STORE 0 TO lnNewCost,lnOldCost
lcStyDesc = ' '
lnAdjCode = 0
llLoc = .F.
llExtMFlg = .F.
IF lcType $ 'AT'
  laTStk  = 0
  laOTStk = 0
ENDIF
=SEEK('P','SCALE')              &&To show empty sizes.
SHOW GET pbNewOk    DISABLE
SHOW GET pbNClose,1 PROMPT gcBmpHome+"cls.bmp"
IF llDyelot
  STORE SPACE(10) TO m.Dyelot
  SHOW GET ibDye    DISABLE
  SHOW GET m.Dyelot DISABLE
ENDIF 
SHOW GET ldDate     DISABLE
SHOW GET lcReason   DISABLE
SHOW GET lcRefer    DISABLE
SHOW GET lnAdjCode  DISABLE  
SHOW GET llLoc      DISABLE
SHOW GET lnOldCost  DISABLE
SHOW GET lnNewCost  DISABLE
SHOW GET m.Adj1     DISABLE
SHOW GET m.Adj2     DISABLE
SHOW GET m.Adj3     DISABLE
SHOW GET m.Adj4     DISABLE
SHOW GET m.Adj5     DISABLE
SHOW GET m.Adj6     DISABLE
SHOW GET m.Adj7     DISABLE
SHOW GET m.Adj8     DISABLE
SHOW GET m.TotAdj   DISABLE
SHOW GET lnNewCost  DISABLE
=lfRefresh('ICINVT_A')
RETURN

*!*************************************************************
*! Name      : lfvAdj()
*! Developer : Timour A. K.
*! Date      : 07/22/97
*! Purpose   : Calculate the total adjustment.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: lcCont->No of size adjusted.
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvAdj()
*!*************************************************************
FUNCTION lfvAdj
PARA lcCont

I=INT(VAL(lcCont))
IF lcType <> 'A' .AND. m.Adj&lcCont < 0
  *--Cannot accept negative inventory.
  = gfModalGen('TRM42059B42001','DIALOG')
  RETURN .F.
ENDIF
IF lcType='T' AND m.Adj&lcCont > m.Stk&lcCont
  RETURN .F.
ENDIF

*--Unable to make -ve adj more than stk. in LIFO and FIFO methods.
IF lcCostMth $ 'LF' AND lcType='A' AND ;
   m.Adj&lcCont<0 AND ABS(m.Adj&lcCont) > m.Stk&lcCont
  RETURN .F.
ENDIF
IF lcType='T'
  *!C037816,1 MHM 04/06/2004 Check for same bin location or not[Start]
  IF ASCAN(laEvntTrig,PADR("DLCHKTRN",10)) <> 0 
    =gfDoTriger("ICSTYTR",PADR("DLCHKTRN",10))
  ELSE
  *!C037816,1 MHM [End]
  
    IF !llModiMod
      laTStk[I] = laOTStk[I] + m.Adj&lcCont
    ELSE
      laTStk[I] = ( laOTStk[I] - &lcTmpAdj->Adj&lcCont ) + m.Adj&lcCont  
    ENDIF
    laTStk[9] = laTStk[1]+laTStk[2]+laTStk[3]+laTStk[4]+laTStk[5]+laTStk[6]+laTStk[7]+laTStk[8]

  *!C037816,1 MHM 04/06/2004 Check for same bin location or not[Start]
  ENDIF 
  *!C037816,1 MHM [End]
 
ENDIF
IF lcType='A'
  
  *B039660,1 NNA 09/04/2005 (BEGIN) Check Bin Location Properties as style Properties (Flag/Hang - Class)
  IF ASCAN(laEvntTrig,PADR("DLCHKTRN",10)) <> 0 
    =gfDoTriger("ICSTYAD",PADR("DLCHKTRN",10))
  ENDIF
  *B039660,1 NNA (End)
  
  *E302005,1 (Begin) Check if the stock becomes negative.
  IF (m.Stk&lcCont + m.Adj&lcCont) < 0 AND !llNetAll
    *-------------------------------------------------------------------
    *--MSG : Your stock balance will be negative. Do you want to proceed?
    *--BUT : \!\<Yes ;Yes to \<All ;\?\<No
    *-------------------------------------------------------------------    
    lnChoice=gfModalGen('TRM42229B44002','DIALOG')
    IF lnChoice = 3
      RETURN .F.
    ENDIF
    llNetAll    = (lnChoice = 2)
  ENDIF
  *E302005,1 (End)
  laTStk[I] = m.Stk&lcCont + m.Adj&lcCont
  laTStk[9] = laTStk[1]+laTStk[2]+laTStk[3]+laTStk[4]+laTStk[5]+laTStk[6]+laTStk[7]+laTStk[8]
ENDIF

*B039660,1 NNA 09/04/2005 (BEGIN) Check Bin Location Properties as style Properties (Flag/Hang - Class)
IF lcType='P' AND ASCAN(laEvntTrig,PADR("DLCHKTRN",10)) <> 0 
  =gfDoTriger("ICSTYPH",PADR("DLCHKTRN",10))
ENDIF
*B039660,1 NNA (End)

m.TotAdj=m.Adj1+m.Adj2+m.Adj3+m.Adj4+m.Adj5+m.Adj6+m.Adj7+m.Adj8
SHOW GET m.TotAdj DISABLE
IF lcType='A' .AND. m.TotAdj <= 0
  lnNewCost=lnOldCost
  SHOW GET lnNewCost DISABLE
ELSE
  *B039107,1 EIH 03/03/2005 Fix bug of disable the NewCost field in case of standard cost method [start]
  *SHOW GET lnNewCost ENABLE
  IF !(ALLTRIM(lcCostMth) == 'S')
    SHOW GET lnNewCost ENABLE
  ENDIF
  *B039107,1 EIH 03/03/2005 [end]
ENDIF
=lfRefresh('ICINVT_A')
RETURN


*!*************************************************************
*! Name      : lfvTrnOk()
*! Developer : Timour A. K.
*! Date      : 07/22/97
*! Purpose   : Valid function for Transaction lines fields.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvTrnOk()
*!*************************************************************
FUNCTION lfvTrnOk

IF !llModiMod
  IF lcType <> 'P' .AND. m.Adj1=0 .AND. m.Adj2=0 .AND. m.Adj3=0 .AND.;
       m.Adj4=0 .AND. m.Adj5=0 .AND. m.Adj6=0 .AND. m.Adj7=0 .AND. m.Adj8=0
    *--All adjustments becomes zero.
    =gfModalGen('INM42060B42001','DIALOG')
  ELSE
    *B603028,1 Update totadj with sum of all adjustments.
    *REPLACE TotAdj WITH m.TotAdj
    SELECT (lcTmpAdj)  
    APPEND BLANK
    REPLACE Style      WITH m.Style    ,;
            Dyelot     WITH m.Dyelot   ,;
            cFromWare  WITH lcFromWare ,;
            cToWare    WITH lcToWare   ,;
            cReason    WITH lcReason   ,;
            cRefer     WITH lcRefer    ,;
            cAdjReason WITH lcAReason  ,;
            Date       WITH ldDate     ,;
            Type       WITH lcType     ,; 
            Unt_Cost   WITH lnNewCost  ,;
            Old_Cost   WITH lnOldCost
    REPLACE Adj1       WITH m.Adj1     ,;
            Adj2       WITH m.Adj2     ,;
            Adj3       WITH m.Adj3     ,;
            Adj4       WITH m.Adj4     ,;
            Adj5       WITH m.Adj5     ,;
            Adj6       WITH m.Adj6     ,;
            Adj7       WITH m.Adj7     ,;
            Adj8       WITH m.Adj8     ,;                                                            
            TotAdj     WITH Adj1+Adj2+Adj3+Adj4+Adj5+Adj6+Adj7+Adj8,;
            TotOld     WITH m.TotStk   ,; 
            dAdd_Date  WITH gdSysDate  ,;
            cAdd_Time  WITH TIME()     ,;
            cAdd_User  WITH gcUser_id  ,;
            GlFYear    WITH lcGlFyear  ,;
            GlPeriod   WITH lcGlPeriod

    *B039094,1 NNA 04/13/2005 (Begin) Update the new cost field with the Avg_cost.
    *--mhm Nader modify in GFSTYCNTRL
    *IF lcCostMth $ 'AFL' AND TOTOLD<>0
    *  REPLACE Unt_Cost   WITH ((TotAdj*lnNewCost) + (TotOld*lnOldCost))/(TotOld+TotAdj)
    *ENDIF
    *B039094,1 NNA (End)

    *B605167,1 ASH 11/27/2001 (Begin) If the line contains both -ve and +ve qtys, then divide them into 2 lines, 
    *B605167,1                one for the -ve qty and the second for the +ve.
    IF (TotAdj < 0 AND (Adj1>0 OR Adj2>0 OR Adj3>0 OR Adj4>0 OR Adj5>0 OR Adj6>0 OR Adj7>0 OR Adj8>0)) OR ;
       (TotAdj > 0 AND (Adj1<0 OR Adj2<0 OR Adj3<0 OR Adj4<0 OR Adj5<0 OR Adj6<0 OR Adj7<0 OR Adj8<0)) OR ;
       (TotAdj = 0 AND (Adj1<>0 OR Adj2<>0 OR Adj3<>0 OR Adj4<>0 OR Adj5<>0 OR Adj6<>0 OR Adj7<>0 OR Adj8<>0))
      FOR I = 1 TO 8
        Z=STR(I,1)
        IF Adj&Z > 0
          lnPsAdj&Z = Adj&Z
          REPLACE Adj&Z WITH 0
        ELSE
          lnPsAdj&Z = 0
        ENDIF
      ENDFOR
      REPLACE TotAdj WITH Adj1+Adj2+Adj3+Adj4+Adj5+Adj6+Adj7+Adj8
      SCATTER MEMVAR MEMO
      APPEND BLANK
      GATHER MEMVAR MEMO
      FOR I = 1 TO 8
        Z=STR(I,1)
        IF lnPsAdj&Z > 0
          REPLACE Adj&Z WITH lnPsAdj&Z 
        ELSE 
          REPLACE Adj&Z WITH 0
        ENDIF
      ENDFOR
      REPLACE TotAdj WITH Adj1+Adj2+Adj3+Adj4+Adj5+Adj6+Adj7+Adj8
    ENDIF
    *B605167,1 ASH 11/27/2001 (End)
    IF lcType='T'
      REPLACE nTotOldTo WITH laTStk[9]
    ENDIF
    SELECT (lcLevel)
    IF lcLevel='Style' .OR. lcType='P'

      *B039660,1 NNA 09/04/2005 (BEGIN) Rfresh the relation between the temnp file and the style because in some case it give
      *B039660,1 NNA            an error when skip
      GOTO RECNO(lcTmpAdj) IN (lcTmpAdj)  
      *B039660,1 NNA (END)
      SELECT STYLE
      SKIP
      IF EOF()
        SKIP -1
      ENDIF
    ELSE
      SET ORDER TO TAG STYDYEW
      DO WHILE cWareCode = lcFromWare
        SKIP
        IF EOF() .OR. cWareCode <> lcFromWare
          SKIP -1
          EXIT
        ELSE
          IF EMPTY(Dyelot)
            EXIT
          ENDIF 
        ENDIF  
      ENDDO
      SET ORDER TO TAG STYDYE
    ENDIF
  ENDIF
  m.Style = Style
  SHOW GET m.Style ENABLE
  SHOW GET ibStyle ENABLE
  SELECT (lcLevel)
  =lfClrInfo()
  _CUROBJ = OBJNUM(m.Style)
  *E302005,1 (Begin) False it for each new style.
  llNetAll    = .F.
  *E302005,1 (End)
ELSE  && llModiMod

  IF lcType='A' .AND. m.Adj1=0 .AND. m.Adj2=0 .AND. m.Adj3=0 .AND.;
     m.Adj4=0 .AND. m.Adj5=0 .AND. m.Adj6=0 .AND. m.Adj7=0 .AND. m.Adj8=0
    *--All adjustments becomes zero,Cannot modify.
    = gfModalGen('INM42061B42001','DIALOG')
  ELSE
    *B603028,1 Update totadj with sum of all adjustments insted of m.TotAdj.
    SELECT (lcTmpAdj)  
    REPLACE cReason WITH lcReason,;
            cAdjReason WITH lcAReason ,;
            cRefer  WITH lcRefer,;
            Date    WITH ldDate,;
            Unt_Cost WITH lnNewCost,;
            Adj1    WITH m.Adj1,;
            Adj2    WITH m.Adj2,;
            Adj3    WITH m.Adj3,;
            Adj4    WITH m.Adj4,;
            Adj5    WITH m.Adj5,;
            Adj6    WITH m.Adj6,;
            Adj7    WITH m.Adj7,;
            Adj8    WITH m.Adj8,;                                                            
            TotAdj  WITH Adj1+Adj2+Adj3+Adj4+Adj5+Adj6+Adj7+Adj8
  ENDIF
  IF lcType='T'
    REPLACE nTotOldTo WITH laTStk[9]
  ENDIF
  CLEAR READ
ENDIF
RETURN


*!*************************************************************
*! Name      : lfvTrnClos()
*! Developer : Timour A. K.
*! Date      : 07/22/97
*! Purpose   : Valid function new/modify close.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvTrnClos()
*!*************************************************************
FUNCTION lfvTrnClos

IF llExtMFlg
  *--Are you sure ? You will lose your XXXX ,\!\<Yes;\?\<No
  IF gfModalGen('TRM42063B42002','DIALOG',IIF(llModiMod,'changes','current entry')) = 2
    _CUROBJ = OBJNUM(ldDate)
    SHOW GET ldDate ENABLE

    *B605176,1 BWA 08/20/2002 Fix the bug of Inv. Adj. Accept negative Qty.more than the stk in FIFO and LIFO Cost.[START]
    IF lcCostMth $ 'LF' OR lcType ='P'
      FOR lnCont = 1 TO 8
        lcCont = ALLTRIM(STR(lnCont))
        IF m.Adj&lcCont<0 AND ABS(m.Adj&lcCont) > m.Stk&lcCont
          STORE 0 TO m.Adj&lcCont
          SHOW GET m.Adj&lcCont
        ENDIF
      ENDFOR
    ENDIF
    *B605176,1 BWA 08/20/2002.[END]

    RETURN
  ELSE
    =lfClrInfo()
  ENDIF
ENDIF
lcReason  = SPACE(25)
lcAReason = SPACE(6)
CLEAR READ
RETURN


*!*************************************************************
*! Name      : lfvLocat()
*! Developer : Timour A. K.
*! Date      : 07/22/97
*! Purpose   : Do Functions mover to select locations.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvLocat()
*!*************************************************************
FUNCTION lfvLocat
*C037816,1 MHM 04/06/2004 Validate Location[Start]
IF ASCAN(laEvntTrig,PADR("DLVLDBIN",10)) <> 0 
  *C123853,1 MHM 01/15/2005 handle trigger in physical program too [Start]
  *=gfDoTriger("ICSTYAD",PADR("DLVLDBIN",10))
  IF lcProgName = "ICSTYAD" 
    =gfDoTriger("ICSTYAD",PADR("DLVLDBIN",10))
  ELSE
    =gfDoTriger("ICSTYPH",PADR("DLVLDBIN",10))
  ENDIF
  *C123853,1 MHM 01/15/2005  [End]
  RETURN
ENDIF
*C037816,1 MHM [End]

SELECT WHSLOC 
lcWareAny = IIF(lcType='T',lcToWare,lcFromWare)
IF !SEEK(SPACE(19)+SPACE(6)+lcWareAny) 
  *--No locations have been assigned to warehouse '+ALLTRIM(lcWareAny)+' .'
  = gfModalGen('TRM42058B42001','DIALOG',ALLTRIM(lcWareAny))
  llLoc = .F.
  SHOW GET llLoc
  RETURN
ENDIF

DIMENSION laSource[1],laTarget[1]
STORE ' ' TO laSource,laTarget
lsSource = 1
SELECT cLocation FROM WHSLOC ;
  WHERE Style+Color+cWareCode == SPACE(19)+SPACE(6)+lcWareAny ;
  INTO ARRAY laSource

SELECT cLocation FROM (lcTemLoc) ;
  WHERE Style+cWareCode+cLocation=PADR(m.Style,19)+lcWareAny;
  INTO ARRAY laTarget

=gfMover(@laSource,@laTarget,"Assign Locations",.T.,'lfvLoc')
SELECT (lcTemLoc)
DELETE FOR Style+cWareCode+cLocation = PADR(m.Style,19)+lcWareAny

IF !EMPTY(laTarget[1])
  FOR I = 1 TO ALEN(laTarget)
     APPEND BLANK
     REPLACE Style     WITH m.Style;
             cWareCode WITH lcWareAny;
             cLocation WITH laTarget[I]
  ENDFOR
ENDIF

SELECT (lcTemLoc)
GO TOP
llLoc = !EOF()
SHOW GET llLoc
RETURN


*!*************************************************************
*! Name      : lfvLoc
*! Developer : Timour A. K.
*! Date      : 07/22/97
*! Purpose   : Validate locations.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvLoc()
*!*************************************************************
FUNCTION lfvLoc
PARAMETERS lnOption

DO CASE 
  CASE lnOption=1
    lcLocatin=laSource[lsSource]
    IF !SEEK(PADR(m.Style,19)+SPACE(6)+lcWareAny+lcLocatin,"WhsLoc")
      *--Location XXX is not assigned to '+'style XXX in warehouse XXX,'\!\<Assign;\<Cancel
      IF gfModalGen('TRM42064B42007','DIALOG',ALLTRIM(lcLocatin)+'|'+ALLTRIM(m.Style)+'|'+ALLTRIM(lcWareAny)) = 1
        RETURN .T.
      ELSE
        RETURN .F. 
      ENDIF
    ELSE
      RETURN .T.
    ENDIF
  CASE lnOption=2
    RETURN .F.
  CASE (lnOption=3 OR lnOption=4)
    RETURN .T.
ENDCASE
      
****


*!*************************************************************
*! Name      : lfvModify()
*! Developer : Timour A. K.
*! Date      : 07/22/97
*! Purpose   : Modify added transaction.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvModify()
*!*************************************************************
FUNCTION lfvModify

llModiMod = .T.
SELECT (lcTmpAdj)
lnCurrRec = IIF(!EOF(),RECNO(),1)
lcScFields = 'Style,Dyelot,Adj1,Adj2,Adj3,Adj4,Adj5,Adj6,Adj7,Adj8,TotAdj'
SCATTER FIELDS &lcScFields MEMO MEMVAR
STORE 0 TO m.Stk1,m.Stk2,m.Stk3,m.Stk4,m.Stk5,m.Stk6,m.Stk7,m.Stk8,m.TotStk
IF lcType = 'AT'
  laTStk  = 0
  laOTStk = 0
ENDIF

ldDate    = Date
lnOldCost = Old_Cost
lnNewCost = Unt_Cost
lcReason  = cReason
lcAReason = cAdjReason
lcRefer   = cRefer
IF llGlLink AND !EMPTY(lcAReason)
  =gfwCodePop(@laCodInfo, "CADJREASON", "T") 
ENDIF

*PUSH KEY
=lfHoldKey('S')
=lfUnTrapKeys()
DO (gcScrDir+gcWinAppl+"\ICINVT_A.SPX")
*POP KEY
=lfHoldKey('R')

SELECT (lcTmpAdj)
GOTO lnCurrRec
SHOW WINDOW (lcBrow_Ttl) REFRESH
SHOW GETS
_CUROBJ = OBJNUM(pbModify)
RETURN


*!*************************************************************
*! Name      : lfOpnInfo()
*! Developer : Timour A. K.
*! Date      : 07/22/97
*! Purpose   : Show gets in modify mode.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfOpnInfo()
*!*************************************************************
FUNCTION lfOpnInfo

IF !llModiMod
  RETURN
ENDIF

SELECT STYLE
=SEEK(m.Style)
lcSScale=Scale
lcStyDesc=Desc1

SELECT (lcLevel)
IF lcLevel<>'Style'
  IF lcType='T'
    =SEEK(PADR(m.Style,19)+lcToWare+lcDyelot )
    SCATTER FIELDS Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8,TotStk TO laTStk
    FOR I=1 TO 8
      Z=STR(I,1)
      laTStk[I]  = laTStk[I] + &lcTmpAdj->Adj&Z
      laOTStk[I] = laTStk[I]
    ENDFOR
    laTStk[9]  = laTStk[1]+laTStk[2]+laTStk[3]+laTStk[4]+laTStk[5]+laTStk[6]+laTStk[7]+laTStk[8]
    laOTStk[9] = laTStk[9]
  ENDIF
  IF llMultiWH
    = SEEK(PADR(m.Style,19)+lcFromWare+m.Dyelot) 
  ELSE
    IF llDyelot
      = SEEK(PADR(m.Style,19)+lcDefWareh+m.Dyelot) 
    ELSE
      = SEEK(m.Style) 
    ENDIF
  ENDIF
ENDIF
*B126587,1 EIH 03/16/2005 It must Inventory less Allocated. " note:  stk# = stk# - alo# " [Begin]
*SCATTER FIELDS Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8,TotStk  MEMVAR
SCATTER FIELDS Stk1,alo1,Stk2,alo2,Stk3,alo3,Stk4,alo4,Stk5,alo5,Stk6,alo6,Stk7,alo7,Stk8,alo8,TotStk,totalo MEMVAR
m.Stk1 = m.Stk1 - m.alo1
m.Stk2 = m.Stk2 - m.alo2 
m.Stk3 = m.Stk3 - m.alo3
m.Stk4 = m.Stk4 - m.alo4
m.Stk5 = m.Stk5 - m.alo5 
m.Stk6 = m.Stk6 - m.alo6
m.Stk7 = m.Stk7 - m.alo7
m.Stk8 = m.Stk8 - m.alo8
TotStk = TotStk - totalo 
*B126587,1 EIH 03/16/2005 [End]

*C123853,1 MHM 01/15/2005 handle trigger in physical program too [Start]
IF ASCAN(laEvntTrig,PADR("DLMDYSTY",10)) <> 0 
  IF lcProgName = "ICSTYAD" 
    =gfDoTriger("ICSTYAD",PADR("DLMDYSTY",10))
  ENDIF
  IF lcProgName = "ICSTYPH" 
    =gfDoTriger("ICSTYPH",PADR("DLMDYSTY",10))
  ENDIF
ENDIF
IF ASCAN(laEvntTrig,PADR("TRMDSTY",10)) <> 0 
    =gfDoTriger("ICSTYTR",PADR("TRMDSTY",10))
ENDIF
*C123853,1 MHM 01/15/2005  [End]


IF lcType = 'A'
  FOR I=1 TO 8
    z=STR(I,1)
    laTStk[I] = m.Stk&z+m.Adj&z
  ENDFOR
ENDIF

=SEEK('S'+lcSScale,'SCALE')
SELECT (lcTmpAdj)
SHOW GET pbNewOk    ENABLE
SHOW GET pbNClose,1 PROMPT gcBmpHome+"can.bmp"
SHOW GET ibStyle    DISABLE
SHOW GET ibDye      DISABLE
SHOW GET ldDate     ENABLE
SHOW GET lcReason   ENABLE  
SHOW GET lcRefer    ENABLE
SHOW GET lnAdjCode  ENABLE  
SHOW GET llLoc      ENABLE
*B039107,1 EIH 03/03/2005 Fix bug of disable the NewCost field in case of standard cost method [start]
*SHOW GET lnNewCost  ENABLE
IF !(ALLTRIM(lcCostMth) == 'S')
  SHOW GET lnNewCost  ENABLE
ENDIF
*B039107,1 EIH 03/03/2005 [end]
SHOW GET lnOldCost  DISABLE

SHOW GET m.Adj1     ENABLE
FOR I=2 TO 8
  Z=STR(I,1)
  IF !EMPTY(SCALE.Sz&Z)
    SHOW GET m.Adj&Z ENABLE
  ELSE
    m.Adj&Z = 0
    SHOW GET m.Adj&Z DISABLE
  ENDIF
ENDFOR
SHOW GET m.TotAdj DISABLE
IF lcType='A' .AND. m.TotAdj <= 0
  lnNewCost=lnOldCost
  SHOW GET lnNewCost DISABLE
ELSE
  *B039107,1 EIH 03/03/2005 Fix bug of disable the NewCost field in case of standard cost method [start]
  *SHOW GET lnNewCost ENABLE
  IF !(ALLTRIM(lcCostMth) == 'S')
    SHOW GET lnNewCost ENABLE
  ENDIF
  *B039107,1 EIH 03/03/2005 [end]
ENDIF
=lfRefresh('ICINVT_A')
_CUROBJ = OBJNUM(ldDate)
RETURN


*!*************************************************************
*! Name      : lfvRemove()
*! Developer : Timour A. K.
*! Date      : 07/22/97
*! Purpose   : Remove added transaction.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvRemove()
*!*************************************************************
FUNCTION lfvRemove

*--The Style :XXX will be removed?','\<Yes;\<No
IF gfModalGen('TRM42065B42002','DIALOG',ALLTRIM(Style)) = 1
  SELECT (lcTmpAdj)  
  DELETE
  GO TOP
  IF !llMultiWH AND EOF()
    laScrMode = .F.
    laScrMode[1] = .T.
    llCUpdate = .F.
  ENDIF
  lnCurrRec=RECNO()
  =lfActBrow()
ENDIF
SHOW GETS
RETURN



*!*************************************************************
*! Name      : lpUnLock
*! Developer : Timour A. K.
*! Date      : 07/22/97
*! Purpose   : Unlock the saved locked records.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  DO lpUnLock
*!*************************************************************
PROCEDURE lpUnLock
IF llDyeLvl
  SET MULTILOCKS OFF
ELSE
  UNLOCK ALL
ENDIF

*!*************************************************************
*! Name      : lfRecLock
*! Developer : Timour A. K.
*! Date      : 07/22/97
*! Purpose   : Record Lock.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: lcFile->Locked file.
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfRecLock()
*!*************************************************************

FUNCTION lfRecLock
PARAMETERS lcFile

SET REPROCESS TO 5 SECONDS 
DO WHILE .T.
  *-This record is in use by another user !','\!\<Retry;\<Cancel'
  lnChoice=gfModalGen('INM00029B00015','DIALOG')
  IF lnChoice = 1 
    IF !RLOCK(lcFile)
      LOOP
    ELSE
      lnRet = .T.
      EXIT
    ENDIF 
  ELSE
    lnRet = .F.
    EXIT   
  ENDIF
ENDDO
SET REPROCESS TO 0
RETURN (lnRet)



*!*************************************************************
*! Name      : lpSavScr()
*! Developer : Timour A. K.
*! Date      : 07/22/97
*! Purpose   : Update the master files.
*!*************************************************************
*! Calls     : None.
*:  Procs & Fncts: lpUnLock
*:                 lfRecLock()
*:                 lfInvUpdt()
*:                 lfVldDate()
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lpSavScr()
*!*************************************************************
FUNCTION lpSavScr

*E301558,1 AHM 02/07/2001 Comment this part and call the lfSave in the ICInvSav Program
*E301558,1                for the saving process (Start)
*!*	*-- Open a temp file to be used in calling gl distributer proc.
*!*	IF llGlLink
*!*	  *E301077,12 open only if not opened.
*!*	  IF !laOpFile[2]
*!*	    laOpFile[2]=gfOpenFile(gcDataDir+'GLDist','GLDistAc','SH')
*!*	  ENDIF
*!*	  SELECT GLDist
*!*	  lcTmpGlDt = gfTempName()
*!*	  COPY STRU TO &gcWorkDir.&lcTmpGlDt
*!*	  *B602457 MAB The following line was converted to normally open [Begin
*!*	  *B602457 because we does not open temp. files using gfopenfile.
*!*	  *=gfOpenFile(gcWorkDir+lcTmpGlDt,' ','EX')
*!*	  USE (gcWorkDir+lcTmpGlDt) IN 0 EXCLUSIVE
*!*	  SELECT (lcTmpGlDt)
*!*	  *B602457 MAB The following line was converted to normally open [End..
*!*	ENDIF  

*!*	*E301077,12 open styinvjl file needed for saving.
*!*	*E301077,12 open only if not opened.
*!*	IF !laOpFile[3]
*!*	  laOpFile[3]=gfOpenFile(gcDataDir+"StyInvJl","StyInvJl","SH")
*!*	ENDIF

*!*	*--Save to master files.
*!*	DIMENSION laToSave[8],laTemp[8]
*!*	SELECT (lcTmpAdj)
*!*	SCAN

*!*	  *--Check if nothing to adjust or transfer (all Zero).
*!*	  IF lcType $ 'AT'
*!*	    SCATTER FIELDS Adj1,Adj2,Adj3,Adj4,Adj5,Adj6,Adj7,Adj8 TO laToSave
*!*	    llZeroAdj = .T.
*!*	    FOR lnI = 1 To 8 
*!*	      llZeroAdj = IIF(laToSave[lnI]=0, .T., .F.)
*!*	      IF !llZeroAdj
*!*	        EXIT
*!*	      ENDIF
*!*	    ENDFOR
*!*	    IF llZeroAdj
*!*	      DELETE
*!*	      LOOP
*!*	    ENDIF
*!*	  ENDIF
*!*	 
*!*	  = SEEK( Style,'STYLE')
*!*	  = SEEK( Style + cFromWare + SPACE(10),'STYDYE')

*!*	  llDyeLvl = ( llDyelot AND STYLE.cDye_Flg = 'Y' )
*!*	  IF llDyeLvl
*!*	    SET MULTILOCKS ON
*!*	  ENDIF 
*!*	  llGoOn = (RLOCK('STYDYE') OR lfRecLock('STYDYE')) AND ;
*!*	           (RLOCK('STYLE' ) OR lfRecLock('STYLE' ))
*!*	  IF llDyeLvl AND llGoOn
*!*	    =SEEK(Style+cFromWare+Dyelot,'STYDYE')
*!*	    llGoOn = (RLOCK('STYDYE') OR lfRecLock('STYDYE'))
*!*	  ENDIF  

*!*	  SELECT (lcTmpAdj)
*!*	  IF !llGoOn
*!*	    *-Style XDX: XXX/XXX is in use by another user, Unable to update.
*!*	    =gfModalGen('TRM42067B42001','DIALOG',+'|'+IIF(llDyelvl,'/Dyelot','')+'|'+ALLTRIM(STYLE)+'|'+IIF(llDyelvl,'/'+Dyelot+'',''))
*!*	    DELETE
*!*	    DO lpUnLock
*!*	    LOOP
*!*	  ENDIF

*!*	  *-- Store Style old cost, Old stock, And link code.
*!*	  SELECT Style
*!*	  lnOldStk   = TotStk
*!*	  lnOldCost  = IIF(lcCostMth<>'S',Ave_Cost,TotCost)  
*!*	  lcLinkCode = IIF(llGlLink ,IIF(!EMPTY(Link_Code),Link_Code,'DEFDEF'),"")
*!*	  SELECT (lcTmpAdj)


*!*	  *--Saving the old quantity in the master file
*!*	  *--and the checking of the FROM stock.
*!*	  IF lcType = 'T'
*!*	    *-- Store the stock in the TO warehouse before updating the master
*!*	    *-- file.
*!*	    = SEEK( Style + cToWare +IIF(llDyeLvl, Dyelot, ''),'STYDYE')
*!*	    SELECT StyDye
*!*	    SCATTER FIELDS Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8 TO laToSave
*!*	    lnTotStk = TotStk     
*!*	    *--If transfer save the link code for to warehouse.
*!*	    lcToLink = IIF(llGlLink ,IIF(!EMPTY(Gl_Link),Gl_Link,lcLinkCode),"")

*!*	    SELECT (lcTmpAdj)
*!*	    GATHER FROM laToSave FIELDS nOldTo1,nOldTo2,nOldTo3,nOldTo4,nOldTo5,nOldTo6,nOldTo7,nOldTo8
*!*	    REPLACE nTotOldTo WITH lnTotStk

*!*	    = SEEK( Style + cFromWare +IIF(llDyeLvl, Dyelot, ''),'STYDYE')
*!*	    *-- We should be sure that the stock in the FROM warehouse if enougth
*!*	    *-- to be transfered to the TO warehouse.
*!*	    SELECT StyDye
*!*	    SCATTER FIELDS Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8 TO laToSave
*!*	    lnTotStk = TotStk     
*!*	    
*!*	    llZeroStk = .F.
*!*	    FOR lnI = 1 TO 8 
*!*	      llZeroStk = IIF(laToSave[lnI] = 0, .T., .F.)
*!*	      IF !llZeroStk
*!*	        EXIT
*!*	      ENDIF
*!*	    ENDFOR

*!*	    SELECT (lcTmpAdj)
*!*	    IF llZeroStk
*!*	      *-Style/xDx : xxx/xxx is out of stock in warehouse: xxx, Cannot transfer.
*!*	      =gfModalGen('TRM42068B42001','DIALOG',IIF(llDyelvl,'/Dyelot: ',': ')+'|'+ALLTRIM(Style)+'|'+IIF(llDyelvl,'/'+ALLTRIM(Dyelot)+' ',' ')+'|'+ALLTRIM(lcFromWare))
*!*	      DELETE REST WHILE Style+cFromWare+Dyelot = STYDYE.Style+STYDYE.cWareCode+STYDYE.Dyelot
*!*	      DO lpUnLock
*!*	      LOOP                  
*!*	    ENDIF
*!*	    
*!*	    *-- Check the stock per size.
*!*	    llVldTran = .T.
*!*	    FOR lnI = 1 TO 8 
*!*	      lcAdjNum = 'Adj' + STR(lnI, 1)
*!*	      llVldTran = IIF(laToSave[lnI] >= &lcAdjNum OR &lcAdjNum = 0, .T., .F.)
*!*	      IF !llVldTran 
*!*	        EXIT
*!*	      ENDIF
*!*	    ENDFOR
*!*	    
*!*	    *-- If there is any size that does not have enougth stock to
*!*	    *-- be transfered then please till this to the user and ask him
*!*	    *-- if he wants to transfer all the stock that exists in the FROM
*!*	    *-- warehouse to the TO warehouse.
*!*	    IF !llVldTran
*!*	      *--The stock level has changed for style/xdx:xxx/xxx in warehouse: xxx Transfer what is available ? \<Yes;\<No.
*!*	      IF gfModalGen('TRM42069B42002','DIALOG',IIF(llDyelvl,'/Dyelot: ',': ')+'|'+ALLTRIM(Style)+'|'+IIF(llDyelvl,'/'+ALLTRIM(Dyelot)+' ',' ')+'|'+ALLTRIM(lcFromWare)) = 1
*!*	        *-- If the user wants to transfer the stock, all we have to
*!*	        *-- do is to save the FROM warehouse stock in the temp. file
*!*	        *-- as the adjustments quantity.
*!*	        GATHER FROM laToSave FIELDS Adj1,Adj2,Adj3,Adj4,Adj5,Adj6,Adj7,Adj8
*!*	        REPLACE TotAdj WITH lnTotStk
*!*	      ELSE
*!*	        DELETE
*!*	        DO lpUnLock
*!*	        LOOP
*!*	      ENDIF
*!*	    ENDIF
*!*	  ENDIF  && End if Transfer.
*!*	    

*!*	  SELECT IIF(!llMultiWH .AND. !llDyeLvl, 'Style', 'StyDye')
*!*	  SCATTER FIELDS Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8 TO laToSave
*!*	  lnTotStk = TotStk
*!*	  SELECT (lcTmpAdj)
*!*	  GATHER FROM laToSave FIELDS OldQty1,OldQty2,OldQty3,OldQty4 ,;
*!*	                              OldQty5,OldQty6,OldQty7,OldQty8
*!*	  REPLACE TotOld    WITH lnTotStk
*!*	  REPLACE dPostDate WITH ldPstDate
*!*	  
*!*	  WAIT WINDOW 'Start updating => '+ALLTRIM(Style)+ ;
*!*	              IIF(llDyelvl,'/'+&lcTmpAdj->Dyelot,'') NOWAIT


*!*	  *-- Store WAREHOUSE link code. If warehouse link code is empty default
*!*	  *-- warehouse link code to style link code. If the last one is empty
*!*	  *-- default to 'DEF' link code.
*!*	  lcLinkCode = IIF(llGlLink ,IIF(!EMPTY(STYDYE.GL_Link),STYDYE.GL_Link,lcLinkCode), "")

*!*	  *--Start Updating.
*!*	  IF ! lfInvUpdt()
*!*	    SELECT (lcTmpAdj)
*!*	    LOOP
*!*	  ENDIF
*!*	    
*!*	  
*!*	  SELECT (lcTmpAdj)
*!*	  REPLACE cSession WITH STYINVJL.cSession

*!*	  *--Update Master file for locations.
*!*	  IF llWareLoc 
*!*	    SELECT (lcTemLoc)
*!*	    SCAN FOR !EMPTY(cLocation)
*!*	      IF !SEEK(Style+SPACE(6)+cWareCode+cLocation,'WHSLOC')
*!*	        SCATTER MEMVAR
*!*	        INSERT INTO WhsLoc FROM MEMVAR
*!*	      ENDIF
*!*	    ENDSCAN
*!*	  ENDIF 

*!*	  SELECT (lcTmpAdj)
*!*	  DO lpUnLock
*!*	ENDSCAN


*!*	*--Update master adjustment file.
*!*	WAIT WINDOW ' Updating the master adjustment file.' NOWAIT
*!*	USE IN (lcTmpAdj)
*!*	SELECT InvtAdj
*!*	APPEND FROM &gcWorkDir.&lcTmpAdj FOR !DELETED()

*!*	*B602457 MAB The following line was converted to normally open [Begin
*!*	*B602457 because we does not open temp. files using gfopenfile.
*!*	*=gfOpenFile(gcWorkDir+lcTmpAdj,' ','EX')
*!*	USE (gcWorkDir+lcTmpAdj) IN 0 EXCLUSIVE
*!*	SELECT (lcTmpAdj)
*!*	*B602457 MAB The following line was converted to normally open [End..
*!*	SET ORDER TO TAG &lcTmpAdj
*!*	SELECT (lcTmpAdj)
*!*	ZAP

*!*	*-- Update distripution master file
*!*	IF llGlLink 
*!*	  WAIT WINDOW 'Updating the general ledger distribution file.' NOWAIT
*!*	  SELECT (lcTmpGlDt)
*!*	  *-- Generate a unique session number.
*!*	  lcGlSess = gfsequence('GLSESSION')
*!*	  REPLACE ALL GLSESSION WITH lcGlSess
*!*	  USE
*!*	  SELECT GLDIST  
*!*	  APPEND FROM &gcWorkDir.&lcTmpGlDt     
*!*	  ERASE (gcWorkDir+lcTmpGlDt+'.DBF')
*!*	ENDIF  
*!*	WAIT CLEAR

*!*	*B802687,1 RAMY [Begin]
*!*	*lcFromWare = IIF( llMultiWH, SPACE(6), lcDefWareh)
*!*	*lcToWare   = SPACE(06)
*!*	*lcWFromDsc = SPACE(30)
*!*	*lcWToDsc   = SPACE(30)
*!*	*SELECT (lcTmpAdj) 
*!*	*GO TOP
*!*	*=lfActBrow()
*!*	*laScrMode = .F.
*!*	*laScrMode[1] = .T.
*!*	*SHOW GETS  
*!*	*RETURN
*!*	*B802687,1 RAMY [end]

*B804152,1 (Begin) Check Stock balance in case of Physical Inventory.
IF lcType = 'P'
  llReBalance = .F.
  DIMENSION laStyStk[10]
  IF !lfBalanced()
    *--If not balanced (ie: The calculated stock from StyInvJl is not equal to the one in Style, StyDye
    *--MSg : Location  ð has one or more styles with incorrect total inventory quantity; 
    *--MSg : it is highly recommended that you perform a rebalance for this location. 
    *--MSg : Do you want to proceed without rebalancing?
    *--MSg : <Proceed> <Rebalance> <Cancel>
    lnChoice = gfModalGen('TRM42227B42019','DIALOG',lcFromWare)
    DO CASE
      CASE lnChoice = 3
        *--Cancel
        llcSave  = .F.
        RETURN
      CASE lnChoice = 1
        *--Proceed
        llReBalance = .F.
    ENDCASE
  ENDIF
ENDIF  
*B804152,1 (End)


DO lfSave IN (gcapphome+gcwinappl+'\ICInvSav.PRG') WITH .F.

*E301558,1 (End)

llAskPrint = IIF(TYPE('llAskPrint') $ 'UL',.T.,.F.)

*--Call report print only if this variable is defined.
*--It used as true in the programs that call externaly the saving function only. 
IF llAskPrint

  *--Would you like to print the Inventory Adjustment journal ?
  *B802687,1 RAMY Change these lines to store the inveroment before calling the OG [start]
  IF gfModalGen('TRM42066B42002','DIALOG') = 1
    *E301074,1 MAB Calling report if user want to print it [Begin]
    *PRIVATE lcRunRep,lcRepsDir
    *lcRunRep  = 'ICSTKJL'
    *lcRepsDir = gcRepHome+gcAct_Appl+'REPORT'  && TO DO ICREPORT WITH 'ICSTKJL'
    *DO &lcRepsDir WITH lcRunRep

    lcFromWare = IIF( llMultiWH, SPACE(6), lcDefWareh)
    lcToWare   = SPACE(06)
    lcWFromDsc = SPACE(30)
    lcWToDsc   = SPACE(30)
    SELECT (lcTmpAdj) 
    GO TOP
    =lfActBrow()
    laScrMode = .F.
    laScrMode[1] = .T.
    SHOW GETS  
    LNCUROBJ = OBJNUM(lcFromWare)

    =gfStatic()                   && Save inviroment before terminate
    CLEAR READ                    && Terminat old program
    =gfClrBro()                   && Call Function to release browse windows
    IF glFromBrow 
      glFromBrow = .F.
      ON KEY LABEL CTRL+Q
      KEYBOARD "{CTRL+Q}" PLAIN CLEAR
    ENDIF  
    gcNextRepo = "ICSTKJL"
    **E301074,1 MAB Calling report if user want to print it [End  ]

  ELSE

    *B802944,1 [Start] Refresh Brows and Screen after saving
    lcFromWare = IIF( llMultiWH, SPACE(6), lcDefWareh)
    lcToWare   = SPACE(06)
    lcWFromDsc = SPACE(30)
    lcWToDsc   = SPACE(30)
    SELECT (lcTmpAdj) 
    GO TOP
    =lfActBrow()
    *laScrMode = .F.
    *laScrMode[1] = .T.
    *SHOW GETS  
    ACTIVATE WINDOW (lcWinCh0) TOP
    _CUROBJ = OBJNUM(lcFromWare)
    *=lfRefresh(lcWinCh0)
    *B802944,1 [End  ] Refresh Brows and Screen after saving

  ENDIF  

ENDIF  &&llAskPrint
*-- End of lfActBrow.

*!*************************************************************
*! Name      : lfInvUpdt()
*! Developer : Timour A. K.
*! Date      : 07/22/97
*! Purpose   : Update inventory.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfInvUpdt()
*!*************************************************************
FUNCTION lfInvUpdt

SELECT (lcTmpAdj)
*--Gl adjustment account.
lcAdjAcct = ' '
IF llGlLink AND !EMPTY(cAdjReason)
  lcAdjReason = cAdjReason
  DECLARE laTrmRltFd[1,2]
  laTrmRltFd[1,1] = 'GLACCOUNT'
  laTrmRltFd[1,2] = 'lcAdjAcct'
  = gfRltFld(lcAdjReason , @laTrmRltFd , "CADJREASON")
ELSE
  lcAdjReason = ' '
ENDIF


*--G/L Array difinition and initialization.
IF llGlLink
  DECLARE laGLDistAr[2,13]
  laGLDistAr[1,1] = lcLinkCode
  laGLDistAr[2,1] = lcLinkCode
  laGLDistAr[1,2] = '006'
  laGLDistAr[2,2] = '007'
  laGLDistAr[1,3] = 1
  laGLDistAr[2,3] = -1
  STORE IIF(lcType = 'P','IP','IA') TO laGLDistAr[1,4],laGLDistAr[2,4]
  STORE ''        TO laGLDistAr[1,5],laGLDistAr[2,5]
  STORE ldPstDate TO laGLDistAr[1,6],laGLDistAr[2,6]
  STORE GLFYear   TO laGLDistAr[1,7],laGLDistAr[2,7]
  STORE GLPeriod  TO laGLDistAr[1,8],laGLDistAr[2,8]
  STORE lcTmpGlDt TO laGLDistAr[1,9],laGLDistAr[2,9]
  laGLDistAr[2,10] = lcAdjAcct
ELSE
  DIME laGLDistAr[1,1]
  laGLDistAr = ''
ENDIF


SELECT (lcTmpAdj)
*--Adjustment quantity array TRANSFER Case.
IF lcType = 'T'
  DECLARE laAdjust[9]
  FOR I = 1 TO 8
    Z=STR(I,1)
    laAdjust[I] = -Adj&Z
  ENDFOR
  laAdjust[9] = -TotAdj

  *--Call the global function for update style inventory control.
  *-- Fixing the bug of wrong updating of ctrcode field in styinvjl (Start) AAMER 04/19/98
  *lnRet=gfStyCrl('1',Style,cFromWare,Dyelot,Date,cRefer,@laAdjust,0,;
            cReason,.T.,cAdjReason,0,'','',@laGLDistAr)
  *E301192 (Start)
  PRIVATE lcRefer 
  *lnRet=gfStyCrl('1',Style,cFromWare,Dyelot,Date,'',@laAdjust,0,;
            cReason,.T.,cAdjReason,0,'','',@laGLDistAr)
  lcRefer = "To location " + lcToWare
  
  *B802503,1 Reham On 08/10/1999   *** Begin ***
  *B802503,1 Send the adjustment reference in case of inventory adjustment.
  lcAdjRef = &lcTmpAdj..cRefer
  *lnRet=gfStyCrl('1',Style,cFromWare,Dyelot,Date,'',@laAdjust,0,;
            lcRefer,.T.,cAdjReason,0,'','',@laGLDistAr)
  lnRet=gfStyCrl('1',Style,cFromWare,Dyelot,Date,'',@laAdjust,0,;
            lcRefer,.T.,cAdjReason,0,'','',@laGLDistAr,0,"",lcAdjRef)
  *B802503,1 Reham On 08/10/1999   *** End   ***
  
  *E301192 (End)
  *-- Fixing the bug of wrong updating of ctrcode field in styinvjl (End)

  *--Read an issue seesion no to use it in receive if LIFO or FIFO.
  lcIsuSessNo = STYINVJL.cSession
  *--Return with no save if function return fulse.
  IF lnRet = 0
    RETURN .F.
  ENDIF  
  *--Next Adjustment will use the link code for TO Warehouse.
  IF llGlLink
    laGLDistAr[1,1] = lcToLink
    laGLDistAr[2,1] = lcToLink
  ENDIF 
ENDIF

*--Warehouse Code.
lcAdjWareH = IIF(lcType = 'T',&lcTmpAdj..cToWare,&lcTmpAdj..cFromWare )

*--If transfer and costing method LIFO or FIFO receive by cost of issue.
IF lcType = 'T' AND lcCostMth $ 'FL' 

  SELECT STYINVJL 
  SEEK Style+&lcTmpAdj..cFromWare+lcIsuSessNo
  SCAN WHILE Style+cWareCode+cSession = Style+&lcTmpAdj..cFromWare+lcIsuSessNo
    lnSavRcNo = RECNO()
    *--Adjustment quantity array.
    DECLARE laAdjust[9]
    FOR I = 1 TO 8
      Z=STR(I,1)
      laAdjust[I] = ABS(nStk&Z)
    ENDFOR
    laAdjust[9] = ABS(nTotStk)

    *--Call the global function for update style inventory control.
    *-- Fixing the bug of wrong updating of ctrcode field in styinvjl (Start)
    *lnRet=gfStyCrl('1',Style,lcAdjWareH,cDyelot,dTrDate,Reference,@laAdjust,;
                   nCost,cTrCode,.T.,cAdjReason,0,'','',@laGLDistAr)
    *E301192 (Start)
    PRIVATE lcRefer 
    *lnRet=gfStyCrl('1',Style,lcAdjWareH,cDyelot,dTrDate,'',@laAdjust,;
                   nCost,Reference,.T.,cAdjReason,0,'','',@laGLDistAr)
    lcRefer = "From location "+ lcFromWare
    
    *B802503,1 Reham On 08/10/1999   *** Begin ***
    *B802503,1 Send the adjustment reference in case of inventory adjustment.
    lcAdjRef = &lcTmpAdj..cRefer
    *lnRet=gfStyCrl('1',Style,lcAdjWareH,cDyelot,dTrDate,'',@laAdjust,;
                   nCost,lcRefer,.T.,cAdjReason,0,'','',@laGLDistAr)
    lnRet=gfStyCrl('1',Style,lcAdjWareH,cDyelot,dTrDate,'',@laAdjust,;
                   nCost,lcRefer,.T.,cAdjReason,0,'','',@laGLDistAr,0,"",lcAdjRef)
    *B802503,1 Reham On 08/10/1999   *** End   ***
    
    *E301192 (End)
    *-- Fixing the bug of wrong updating of ctrcode field in styinvjl (End)
    SELECT STYINVJL
    GOTO lnSavRcNo   
  ENDSCAN 

ELSE
  *--Adjustment quantity array.
  DECLARE laAdjust[9]
  SCATTER FIELDS Adj1,Adj2,Adj3,Adj4,Adj5,Adj6,Adj7,Adj8,TotAdj TO laAdjust

  *--Adjustment cost.
  lnACost = IIF(TotAdj>0,Unt_Cost,Old_Cost)
  *--Type of the adjustment.
  lcAdjTyp = IIF(lcType = 'P','2','1')

  *--Call the global function for update style inventory control.
  *-- Fixing the bug of wrong updating of ctrcode field in styinvjl (Start)
  *lnRet=gfStyCrl(lcAdjTyp,Style,lcAdjWareH,Dyelot,Date,cRefer,@laAdjust,lnACost,;
                 cReason,.T.,cAdjReason,0,'','',@laGLDistAr)
  *E301192 (Start)
  PRIVATE lcRefer 
  *lnRet=gfStyCrl(lcAdjTyp,Style,lcAdjWareH,Dyelot,Date,'',@laAdjust,lnACost,;
                 cReason,.T.,cAdjReason,0,'','',@laGLDistAr)
  IF lcType = 'T'
    lcRefer = "From location " + lcFromWare
  ELSE
    lcRefer = IIF(EMPTY(cReason),gfCodDes(cAdjReason,'CADJREASON'),cReason)
  ENDIF
  
  *B802503,1 Reham On 08/10/1999   *** Begin ***
  *B802503,1 Send the adjustment reference in case of inventory adjustment.
  lcAdjRef = &lcTmpAdj..cRefer
  *lnRet=gfStyCrl(lcAdjTyp,Style,lcAdjWareH,Dyelot,Date,'',@laAdjust,lnACost,;
                 lcRefer,.T.,cAdjReason,0,'','',@laGLDistAr)  
  lnRet=gfStyCrl(lcAdjTyp,Style,lcAdjWareH,Dyelot,Date,'',@laAdjust,lnACost,;
                 lcRefer,.T.,cAdjReason,0,'','',@laGLDistAr,0,"",lcAdjRef)
  *B802503,1 Reham On 08/10/1999   *** End   ***
  *-- Fixing the bug of wrong updating of ctrcode field in styinvjl (End)
ENDIF

*--Return with no save if function return fulse.
IF lnRet = 0
  RETURN .F.
ENDIF  
RETURN



*!**************************************************************************
*! Name      : lfBalanced
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 08/01/02
*! Purpose   : Check Stock.
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Notes : 
*!**************************************************************************
*--B804152,1
FUNCTION lfBalanced
PRIVATE lnAlias

lnAlias = ALIAS()

IF !USED('StyInvJl')
  =gfOpenFile(gcDataDir+"StyInvJl","StyInvJl","SH")
ENDIF
STORE 0 TO laStyStk
SELECT (lcTmpAdj)
SCAN
 STORE 0 TO laStyStk
 llDyeLvl = ( llDyelot AND STYLE.cDye_Flg = 'Y' )
 lcKey    = IIF(!llMultiWH,STyle,Style+cFromWare)
 lcDyeLot = DyeLot
 lcWorkAls= IIF(!llMultiWH .AND. !llDyeLvl, 'Style', 'StyDye')
 IF SEEK(lcKey,'StyInvJl')
   SELECT StyInvJl
   SUM REST WHILE style+cwarecode+csession+DTOS(dtrdate)+ctrcode+STR(lineno,6) =lcKey;
            FOR IIF(llDyelot AND STYLE.cDye_Flg = 'Y',cDyeLot = lcDyeLot,.T.);
                  nStk1,nStk2,nStk3,nStk4,nStk5,nStk6,nStk7,nStk8,nTotStk,nStkVal;
       TO laStyStk[1],laStyStk[2],laStyStk[3],laStyStk[4],laStyStk[5],laStyStk[6],laStyStk[7],;
          laStyStk[8],laStyStk[9],laStyStk[10]
          
   IF SEEK(lcKey+IIF(llDyeLvl,lcDyeLot,''),lcWorkAls) AND ;
        (&lcWorkAls..Stk1<>laStyStk[1] ;
      OR &lcWorkAls..Stk2<>laStyStk[2] ;
      OR &lcWorkAls..Stk3<>laStyStk[3] ;
      OR &lcWorkAls..Stk4<>laStyStk[4] ; 
      OR &lcWorkAls..Stk5<>laStyStk[5] ;
      OR &lcWorkAls..Stk6<>laStyStk[6] ;
      OR &lcWorkAls..Stk7<>laStyStk[7] ;
      OR &lcWorkAls..Stk8<>laStyStk[8] ;
      OR &lcWorkAls..TotStk<>laStyStk[9]; 
      OR &lcWorkAls..nStkVal<>laStyStk[10])
     llReBalance = .T.
     RETURN .F.
   ENDIF 
 ENDIF
ENDSCAN

SELECT (lnAlias)

*!*************************************************************
*! Name      : lfCstICAPH
*! Developer : Ehab Ismail Hamed  - (EIH)
*! Date      : 03/03/05
*! Purpose   : In screen ICINVT_A disable the Newcost field 
*!           : In case of standard cost method .
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Calls              :  gfGetMemVar
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            : =lfCstICAPH()
*!*************************************************************
*B039107,1 EIH 03/03/2005 Fix bug of disable the NewCost field 
*B039107,1                in case of standard cost method . 
FUNCTION  lfCstICAPH
IF (ALLTRIM(lcCostMth) == 'S')
 SHOW GET lnNewCost DISABLE
ENDIF
*B039107,1 EIH 03/03/2005 [end]