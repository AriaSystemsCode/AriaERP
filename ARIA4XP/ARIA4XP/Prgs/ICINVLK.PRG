*****************************************************************************
*: Program file  : ICINVLK.PRG (T20060908.0003)
*: Program desc. : Style Invintory And MarkDown.
*: Module        : IC (Inventory Locking)
*: System		     : Aria Apparel System (A4xp)
*: Developer	   : Mariam Mazhar [MMT] N000548
*****************************************************************************
*: Passed Parameters  : lcModulPara:
*:                       '1' ->  # For Style
*:                       '2' ->  # For Material
*****************************************************************************
*:Modifications :
*:B608303 NNA 04/10/2007 (TT20070713.0001) fix Bug that inventory control programs deal with non inventory styles
*:B608453,1 AKA 25/2/2008 [T20071205.0007] Fix numeric overflow error while posting  
*:B608564,1 MOS 05/20/2008[T20080423.0053] FIX the problem of importing from extended style excel sheet 
*:B608573,1 MMT 05/28/2008 Fix bug of error if MA module is not installed	[T20080415.0040]
*:C200876,1 TMI 05/02/2008 Adding the bin location triggers
*:B608785,1 TMI 01/22/2009 initilaize lcAdjReason,show the progress bar
*:B608844,1 TMI 04/11/2009 Get the "lnColorLen","lnCrlPo" correct value and Send to the GLDIS the POSTDATE not the BATCH date [T20090115.0001]
*:B608941,1 TMI 07/16/2009 add the progress bar
*!B609335,1 MMT 07/06/2010 Validate Style Sizes Cnt and locking date in Excel file[T20100318.0002]
*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[T20100730.0001]
*!B609607,1 MMT 06/08/2011 Fix bug of repeated adj. record while Posting if Bin Location is used(T20100129.0008) 
*!E303058,1 MMT 02/05/2012 Allow user to import excel file after creating batch in locking screen[T20120111.0036]
*!B609845,1 MMT 02/23/2012 Fix error in invnetory locking screen after click on scope [T20120216.0104]
*!E303079,1 MMT 03/11/2012 Fixing Media issues[T20120304.0004]
*!B609845,2 MMT 03/15/2012 Fix error in invnetory locking screen after click on scope if use Bin location is YES[T20120308.0006]
*!B610699,1 TMI 03/17/2014 check the backup folder before taking a backup, Also remove '!' from MD command[T20140226.0011 ] 
*****************************************************************************

#INCLUDE R:\ARIA4XP\SCREENS\ICINVLCK.H
PARAMETERS lcModulPara
lcModulPara = IIF(TYPE('lcModulPara')='U',SPACE(01),lcModulPara)
lcPara   = STR(lcModulPara,1)
lcParamStr = "12"

IF (TYPE('lcPara') <> "C" OR EMPTY(lcPara) OR !(lcPara $ lcParamStr))
  WAIT WINDOW LANG_Msg_Cannot_Run
  RETURN
ENDIF

llMatModul = IIF(STR(lcModulPara,1) = '2',.T.,.F.)

DO FORM (oAriaApplication.ScreenHome+'\ICINVLCK.SCX') WITH llMatModul 
***********************************************************  ******************
*: Program file  : lfOpenFiles
*: Program desc. : Open nessasary Files.
*:      Developer: Mariam Mazhar [MMT]
*! Date      : 11/19/2006
*****************************************************************************
*! Called from :  Procedures : ICINVLK.PRG
*****************************************************************************
*: Passed Parameters  : lcPara
*****************************************************************************
*:EXAMPLE : DO lpOpenFiles
*****************************************************************************
FUNCTION lfOpenFiles
PARAMETERS loFormSet
SET MULTILOCKS ON
=gfOpenTable(oAriaApplication.DataDir+'MDINVNTL',oAriaApplication.DataDir+'MDINVNTL','SH')
=gfOpenTable(oAriaApplication.DataDir+'MDINVNTH',oAriaApplication.DataDir+'MDINVNTH','SH')
=gfOpenTable(oAriaApplication.DataDir+'GLDIST','','SH')
=gfOpenTable(oAriaApplication.DataDir+'WareHous',oAriaApplication.DataDir+'WareHous','SH')
=gfOpenTable(oAriaApplication.DataDir+'Scale',oAriaApplication.DataDir+'Scale','SH')
=gfOpenTable(oAriaApplication.DataDir+'CODES',oAriaApplication.DataDir+'CODES','SH')

*:B608573,1 MMT 05/28/2008 Fix bug of error if MA module is not installed[Start]
IF ("MA" $ oAriaApplication.CompanyInstalledModules) AND loFormSet.llmatmodul  
  =gfOpenTable(oAriaApplication.DataDir+'ROLLS',oAriaApplication.DataDir+'ROLLS','SH')
ENDIF   
*:B608573,1 MMT 05/28/2008 Fix bug of error if MA module is not installed[End]

SELECT Codes
gfSetOrder('cCode_no')
=gfSEEK("D"+'CADJREASON','CODES')
lcDefAdjCd = Codes.Ccode_No 
IF loFormSet.llLoc
  =gfOpenTable(oAriaApplication.DataDir+'WhsLoc',oAriaApplication.DataDir+'WhsLoc','SH')
ENDIF
IF loFormSet.llMatModul
  =gfOpenTable(oAriaApplication.DataDir+'ITEM' ,'cStyle','SH')
  =gfOpenTable(oAriaApplication.DataDir+'ITEMLOC' ,'stydye','SH')
  =gfOpenTable(oAriaApplication.DataDir+'ITEMJRNL' ,'styinvjl','SH')
  =gfOpenTable(oAriaApplication.DataDir+'UOM' ,'UOMCODE','SH')
  

ELSE
  =gfOpenTable(oAriaApplication.DataDir+'Style' ,'Style','SH')
  =gfOpenTable(oAriaApplication.DataDir+'StyDye' ,'StyDye','SH')
  =gfOpenTable(oAriaApplication.DataDir+'StyInvJl' ,'StyInvJl','SH')
ENDIF
RETURN


*!*************************************************************
*! Name      : lfVScope
*! Developer : Mariam Mazhar- (MMT)
*! Date      : 11/19/2006
*! Purpose   : Functio to valid scope but on menu bar.
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            : =lfVScope()
*!*************************************************************
*!
FUNCTION lfVScope
PARAMETERS loFormSet

*-------
IF EMPTY(loFormset.ariaform1.kblocation.keytextbox.value)
  =gfModalGen('TRM42150B00000','DIALOG')
  loFormset.ariaform1.kblocation.keytextbox.SetFocus()
  RETURN .F.
ENDIF 
*---------
DIMENSION laScopExpr[1,2] , laNormExpr[1,2]
llCallFromScr = .F.
laScopExpr = ''
laNormExpr = ''

*lcStyTitle  = gfItemMask('HI')
lcStylePic  = "@! " + loFormSet.lcmajpict
*gfItemMask('PM')
lcStyMajor  = loFormSet.lcStyMajor
*= gfItemMask('HM')
lnMajorLen  = LEN(lcStylePic)

STORE .F. TO lcExpr1,lcExpr2
STORE ''  TO  lcExpr,XVENDOR,lcRPVendor,lcRPFrmFab,lcRPToFab,lcRPTitle,lcRPSortBy,;
              lcRPItmTyp,lcRPForm,LFAB,HFAB,XTITLE,XSORT,XITEM_TYP,XFILTER,;
              lcSLoc,lcRpDoMimP,lcRpDoMimD,lcMy,lcStyle,;
              lcStat,lcDyelot,lcOldDye,lcReason,m.cLkBatch,lcGlSess
STORE ''  TO  lcDyeTlt,lcInvType,lcFabDye
STORE .F. TO llRPOnHand,llRPPrnDye,llRPPntW
STORE 0   TO lnMajorLen,lnRpStatus,lnColorlen
DIMENSION laRpSource[1],laRpTarget[1]
llAvrCost = loFormSet.llAvrCost
llWareHus = loFormSet.llWareHus
llDyelot  = loFormSet.llDyelot 
llLinkGL  = loFormSet.llLinkGL 
llLoc     = loFormSet.llLoc    
STORE 0 TO lnTotStk,lnCost,lnColorLen,lnClrPo,lnLNewTot,lnMCost,lnOldCost
*B608844,1 TMI 04/09/2009 [Start] get the "lnColorLen" from properties
=lfGetClrPo()
*B608844,1 TMI 04/09/2009 [End  ] 

DECLARE laFxExpr[1],laVrExpr[1]
*--- Start Call Option Grid
lcDataSessI = SET("Datasession" )
lcOldPrc = SET("PROCEDURE")
IF loFormSet.llmatmodul  
  *!E303058,1 MMT 02/05/2012 Allow user to import excel file after creating batch in locking screen[T20120111.0036][Start]
  *SET PROCEDURE TO oAriaApplication.ReportHome+"MA\MAMATLST.FXP"  
  *!B609845,2 MMT 03/15/2012 Fix error in invnetory locking screen after click on scope if use Bin location is YES[Start]
  *SET PROCEDURE TO oAriaApplication.ReportHome+"MA\MAMATLST.FXP" ADDITIVE 
  lcSetProc = SET("Procedure")
  lcSetProc = oAriaApplication.ReportHome+"MA\MAMATLST.FXP"+IIF(!EMPTY(lcSetProc),",","")+lcSetProc   
  SET PROCEDURE TO &lcSetProc.
  *!B609845,2 MMT 03/15/2012 Fix error in invnetory locking screen after click on scope if use Bin location is YES[END]

  *!E303058,1 MMT 02/05/2012 Allow user to import excel file after creating batch in locking screen[T20120111.0036][END]
  lcExpr = gfOpGrid('MAMATLST',.T.)&&,.F.,.F.,.T.,.T.)  
  SET PROCEDURE TO &lcOldPrc
ELSE
  STORE '' TO lcMajTtl,lcMajPic,lcColorTt
  STORE '' TO lnO_T_S
  STORE .F. TO llRpPrnPic
  *!E303058,1 MMT 02/05/2012 Allow user to import excel file after creating batch in locking screen[T20120111.0036][Start]
  *SET PROCEDURE TO oAriaApplication.ReportHome+"IC\ICSTYLST.FXP"  
  *!B609845,2 MMT 03/15/2012 Fix error in invnetory locking screen after click on scope if use Bin location is YES[START]
  *SET PROCEDURE TO oAriaApplication.ReportHome+"IC\ICSTYLST.FXP" ADDITIVE 
  lcSetProc = SET("Procedure")
  lcSetProc = oAriaApplication.ReportHome+"IC\ICSTYLST.FXP"+IIF(!EMPTY(lcSetProc),",","")+lcSetProc   
  SET PROCEDURE TO &lcSetProc.
  *!B609845,2 MMT 03/15/2012 Fix error in invnetory locking screen after click on scope if use Bin location is YES[END]
  *T20071102.0018,10/C200876 TMI [Start] Calls the lfvScope fucntion with parameters changed
  IF ASCAN(loFormSet.laEvntTrig,PADR('SHWSCOPE',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0 ;
    .AND. loFormSet.mDoTrigger(PADR('ISUSEBIN',10))
    =loFormSet.mDoTrigger(PADR('SHWSCOPE',10))
  ELSE
    *T20071102.0018,10/C200876 TMI [End  ] 

    lcExpr = gfOpGrid('ICSTYLST',.T.)&&,.F.,.F.,.T.,.T.)  
    *T20071102.0018,10/C200876 TMI [Start] close the above if 
  ENDIF
  *T20071102.0018,10/C200876 TMI [End  ] 
  SET PROCEDURE TO &lcOldPrc
ENDIF
*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
IF loFormSet.llmatmodul
  SELECT ITEM
  =GFSETORDER('STYLE')
ENDIF
*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
SET DATASESSION TO lcDataSessI 
IF lcExpr <> ".F."
  =lfSelData()
ENDIF 
*!*************************************************************
*! Name      : lfSelData
*! Developer : Mariam Mazhar [MMT]
*! Date      : 11/19/2006
*! Purpose   : Select Data.
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            : =lfSelData()
*!*************************************************************
*!
FUNCTION lfSelData
PARAMETERS llLogic

SELECT(loFormset.lcTmpQuery)
DELETE ALL
SELECT (loFormset.lcBatLin)
DELETE ALL
SELECT (loFormset.lcBatHdr)
DELETE ALL
=lfSelMRec()
SELECT(loFormset.lcTmpQuery)
LOCATE 
IF EOF()
  =gfModalGen('TRM00342B00000','DIALOG' )
  loFormset.oToolBar.cmdadd.Enabled = .F.
  loFormset.ariaform1.cmdEdit.Enabled = .F.
  loFormset.ariaform1.cmdZero.Enabled = .F.
  loFormset.ariaform1.kblocation.enabled = .T.
  loFormset.ariaform1.kblocation.keytextbox.SetFocus()
ELSE
  loFormset.ariaform1.kblocation.Enabled = .F.
  loFormset.oToolBar.cmdadd.Enabled = .T.
  loFormset.ariaform1.cmdEdit.Enabled = .T.
  loFormset.ariaform1.cmdZero.Enabled = .T.
ENDIF
SELECT (loFormSet.lcBatLin)
IF loFormSet.llMatModul
  *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
  *LOCATE FOR IIF(loFormSet.llDyelot AND lfStyDye(SUBSTR(Style,1,7),Color),!EMPTY(EVALUATE(loFormSet.lcBatLin+'.DYELOT')),.T.)
  LOCATE FOR IIF(loFormSet.llDyelot AND lfStyDye(Style),!EMPTY(EVALUATE(loFormSet.lcBatLin+'.DYELOT')),.T.)
  *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
ELSE
  *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
  *LOCATE FOR IIF(loFormSet.llDyelot AND lfStyDye(Style ,Color),!EMPTY(EVALUATE(loFormSet.lcBatLin+'.DYELOT')),.T.)    
  LOCATE FOR IIF(loFormSet.llDyelot AND lfStyDye(Style),!EMPTY(EVALUATE(loFormSet.lcBatLin+'.DYELOT')),.T.)  
  *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
ENDIF
IF !FOUND()
  loFormset.oToolBar.cmdadd.Enabled = .F.
ELSE
  loFormset.oToolBar.cmdadd.Enabled = .T.
ENDIF
SELECT(loFormset.lcTmpQuery)
LOCATE 
RETURN

*!*************************************************************
*! Name      : lfVBatchBrow
*! Developer : Mariam Mazhar [MMT]
*! Date      : 11/19/2006
*! Purpose   : Browse batches
*!*************************************************************
FUNCTION lfVBatchBrow
PARAMETERS loFormset
LOCAL lcBatch
lcBatch = PADR(loFormset.ariaform1.kbBatchNo.keytextbox.Value,6)
lcKey = IIF(loFormSet.llmatmodul,'M','S')
SELECT MDINVNTH
IF (EMPTY(lcBatch) AND loFormset.ariaform1.kbBatchNo.selectedfrombrowse) OR ;
  (AT('?',ALLTRIM(lcBatch),1) > 0) OR;
  (!EMPTY(lcBatch) AND !gfSEEK(lcKey +lcBatch)) OR ;
  loFormset.ariaform1.kbBatchNo.selectedfrombrowse
  
  lcBrFields = [clkBatch :R :H= LANG_Batch , date :R :H= LANG_Lck_date, Dpostdate :R :H= LANG_Post_Date,] +;
                   [CWARECODE  :R :H= LANG_Location,Content :R :H= LANG_Notes ]
                             
  IF !loFormset.ariaform1.kbBatchNo.selectedfrombrowse
    loFormset.ariaform1.kbBatchNo.selectedfrombrowse = .T.
  ENDIF 

  IF loFormset.ariaform1.kbBatchNo.selectedfrombrowse
    lcIndexSel = "MDINVNTH" 	
    lcFilter = IIF(loFormset.llmatmodul ," FOR cbattype = 'M'"," FOR cbattype = 'S'")
    lcBatch  = IIF(ARIABROW(lcFilter,LANG_Batch,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, ;
                         gnBrFSCol2,'','','clkBatch','laBrowArr', .F.,'',.F.,'MDINVNTH','','',lcIndexSel),MDINVNTH.clkBatch,SPACE(6))


    loFormset.AriaForm1.kbBatchNo.keytextbox.Value = lcBatch  
    loFormset.ariaform1.kbBatchNo.selectedfrombrowse = .F.
    RETURN .T.
  ENDIF  
ELSE
  IF gfSEEK(lcKey +lcBatch)
    loFormset.ariaform1.kbBatchNo.selectedfrombrowse = .F.
    RETURN .T. 	     
  ENDIF  
ENDIF 
*!*************************************************************
*! Name      : lfSelMRec
*! Developer : Mariam Mazhar [MMT]
*! Date      : 11/19/2006
*! Purpose   : Select Style in multi wareHouse.
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            : =lfSelMRec()
*!*************************************************************
FUNCTION lfSelMRec
m.Color = SPACE(06)
WAIT WINDOW LANG_Select_Records NOWAIT

lcStyPic  = loFormSet.lcstytitle
lcMajPict = loFormSet.lcmajpict
lnMajLen  = LEN(lcMajPict)
lcSepar   = SUBSTR(lcstypic,lnMAjlen+1,1)


IF loFormSet.llmatmodul
  lcCostMeth = gfGetMemVar('M_MATCSTMT')
ELSE
  lcCostMeth = gfGetMemVar('M_COST_MET')
ENDIF

IF llLogic && Import from Excel
  SELECT (loFormSet.lcInport)
 *!B608564,1 MOS 05/20/2008 FIX the problem of importing from extended style excel sheet [start] 
  *SCAN FOR RECNO() >=5  AND !EMPTY(COLOR)
   SCAN FOR RECNO() >=5 && AND !EMPTY(COLOR)
 *!B608564,1 MOS 05/20/2008 FIX the problem of importing from extended style excel sheet[end]
  
  SELECT STYDYE
 *!B608564,1 MOS 05/20/2008 FIX the problem of importing from extended style excel sheet[start] 
   * IF gfSEEK(EVALUATE(loFormSet.lcInport+'.STYLE') + lcSepar + EVALUATE(loFormSet.lcInport+'.COLOR') + loFormSet.lcWare , 'STYDYE')
     IF gfSEEK(EVALUATE(loFormSet.lcInport+'.STYLE') + loFormSet.lcWare , 'STYDYE')
        *SCAN REST WHILE STYLE + CWARECODE + DYELOT = EVAL(loFormSet.lcInport+'.STYLE') + lcSepar + EVAL(loFormSet.lcInport+'.COLOR') AND IIF(loFormSet.llWareHus,CwareCode = PADR(loFormset.ariaform1.kblocation.keytextbox.Value,6) ,.T.)
        SCAN REST WHILE STYLE + CWARECODE + DYELOT = EVAL(loFormSet.lcInport+'.STYLE') AND IIF(loFormSet.llWareHus,CwareCode = PADR(loFormset.ariaform1.kblocation.keytextbox.Value,6) ,.T.)
 *!B608564,1 MOS 05/20/2008 FIX the problem of importing from extended style excel sheet[end] 
  
        SCATTER MEMVAR MEMO
        STORE 0 TO lnTotStk
        FOR lnLop = 1 To 8
          lcStk  = 'Stk' + ALLTRIM(STR(lnLop))
          m.&lcStk = EVALUATE(loFormSet.lcInport+'.&lcStk')
          lnTotStk = lnTotStk + EVALUATE(loFormSet.lcInport+'.&lcStk')
        ENDFOR
        m.TotStk = lnTotStk
        m.StyDesc = Style.Desc1
        DO CASE
          CASE  lcCostMeth = 'S'  && Standard
            m.Cost    = STYLE.TotCost
            m.oCost   = STYLE.TotCost
          OTHERWISE       && 'A' Average
            m.Cost    = Ave_Cost
            m.oCost   = Ave_Cost
        ENDCASE
        m.ITEM    = ''

        *!B609335,1 MMT 07/06/2010 Validate Style Sizes Cnt and locking date in Excel file[Start]
        llInvalidStyle = .F.
        =gfSeek(EVALUATE(loFormSet.lcInport+'.STYLE'),'STYLE','STYLE')
        =gfSeek('S'+Style.Scale,'Scale','Scale')
        FOR lnZ = Scale.cnt + 1 TO 8
          lcZ = STR(lnZ,1)
          IF m.Stk&lcZ. <> 0
            llInvalidStyle = .T.
            SELECT (loFormSet.lcRejSty)  
            APPEND BLANK
            REPLACE Style    WITH EVALUATE(loFormSet.lcInport+'.STYLE') ,;
              WareCode WITH loFormSet.lcWare ,;
              Reason   WITH LANG_SCALECNFLCT      ,;
              cdesc    WITH IIF(gfSEEK(PADR(loFormSet.lcWare,6),"WAREHOUS"),Warehous.cdesc,"")          
            EXIT   
          ENDIF 
        ENDFOR 
        IF llInvalidStyle 
          EXIT 
        ENDIF 
        *!B609335,1 MMT 07/06/2010 Validate Style Sizes Cnt and locking date in Excel file[End]
        
     *!B608564,1 MOS 05/20/2008 FIX the problem of importing from extended style excel sheet[start] 
       *m.Color =  EVALUATE(loFormSet.lcInport+'.COLOR')
        m.Color =  SPACE(6)
     *!B608564,1 MOS 05/20/2008 FIX the problem of importing from extended style excel sheet[end] 
        
        m.STOCK   = lnTotStk 
        m.oSTOCK  = TotStk
        m.nStkVal = nStkVal
        m.Cadd_user = oAriaApplication.User_Name
        m.Dadd_date = DATE()
        m.Cadd_time = gfGetTime()
        IF lfChekBin()
          SELECT MDINVNTL
          gfSetOrder('MDINVNTLS')
          WAIT WINDOW LANG_Select_Records NOWAIT
          *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
          *=lfAddLTemp(m.Style,m.Color)          
          =lfAddLTemp(m.Style)
          *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
          INSERT INTO (loFormSet.lcBatLin) FROM MEMVAR
          SELECT (loFormSet.lcBatLin)
          Replace Dyelot   WITH StyDye.Dyelot   ,;
                  cBatType WITH 'S'             ,;
                  Scale    WITH Style.Scale     ,;
                  OldCost  WITH StyDye.Ave_Cost ,;
                  OldStk1  WITH StyDye.Stk1     ,;
                  OldStk2  WITH StyDye.Stk2     ,;
                  OldStk3  WITH StyDye.Stk3     ,;
                  OldStk4  WITH StyDye.Stk4     ,;
                  OldStk5  WITH StyDye.Stk5     ,;
                  OldStk6  WITH StyDye.Stk6     ,;
                  OldStk7  WITH StyDye.Stk7     ,;
                  OldStk8  WITH StyDye.Stk8     ,;
                  OldTotStk WITH StyDye.TotStk
        ENDIF
      ENDSCAN
    ELSE
      SELECT (loFormSet.lcRejSty)
      APPEND BLANK
      REPLACE Style    WITH EVALUATE(loFormSet.lcInport+'.STYLE') ,;
              WareCode WITH loFormSet.lcWare ,;
              Reason   WITH LANG_Imprt_Not_Fnd_Msg + lcStyPic + LANG_Imprt_Not_Fnd,;
              cdesc    WITH IIF(gfSEEK(PADR(loFormSet.lcWare,6),"WAREHOUS"),Warehous.cdesc,"")
    ENDI
  ENDSCAN

ELSE
*!*	  IF ASCAN(laEvntTrig,PADR("DLSELREC",10)) <> 0 
*!*	    =gfDoTriger("ICINVLK",PADR("DLSELREC",10))
*!*	  ELSE

  IF loFormSet.llmatmodul &&Material
   *--Vendor
   llVendSelect = .F.
   lcVendFile = ''
   lnVendPos = ASCAN(laFxExpr,"APVENDOR.CVENDCODE")
   IF lnVendPos > 0 
     lnVendPos = ASUBSCRIPT(laFxExpr,lnVendPos,1)
     lcVendFile = laFxExpr[lnVendPos,6]
   ENDIF  
   IF !EMPTY(lcVendFile)  AND USED(lcVendFile)
     SELECT(lcVendFile)
     LOCATE 
     IF !EOF()
       llVendSelect = .T.
     ENDIF 
   ENDIF 
   
   *--Item 
   llItemSelect = .F.
   lcItemFile = ''
   lnItemPos = ASCAN(laFxExpr,"ITEM.CSTYMAJOR")
   IF lnItemPos> 0 
     lnItemPos = ASUBSCRIPT(laFxExpr,lnItemPos,1)
     lcItemFile = laFxExpr[lnItemPos,6]
   ENDIF  
   IF !EMPTY(lcItemFile)  AND USED(lcItemFile)
     SELECT(lcItemFile)
     LOCATE 
     IF !EOF()
       llItemSelect = .T.
     ENDIF 
   ENDIF 

   *Location
   llLocSelect = .F.
   lcLocFile = ''
   lnLocPos = ASCAN(laVrExpr,"WAREHOUS.CWARECODE")
   IF lnLocPos > 0 
     lnLocPos  = ASUBSCRIPT(laVrExpr,lnLocPos,1)
     lcLocFile = laVrExpr[lnLocPos,6]
   ENDIF  
   IF !EMPTY(lcLocFile)  AND USED(lcLocFile)
     SELECT(lcLocFile)
     LOCATE 
     IF !EOF()
       llLocSelect = .T.
     ENDIF 
   ENDIF 

   *Status
   lcStuExp  = ''
   lnStuPos = ASCAN(laVrExpr,"ITEM.STATUS")
   IF lnStuPos > 0 
     lnStuPos  = ASUBSCRIPT(laVrExpr,lnStuPos,1)
     lcStuExp = laVrExpr[lnStuPos,6]
   ENDIF  
   
   IF !EMPTY(lcStuExp)
     lcStuExp = "Inlist(ITEM.STATUS,'"+STRTRAN(lcStuExp,'|',"','")+"')"
   ENDIF 
   
   *--Item Type
   llItmTypSelect = .F.
   lcItmTypFile   = ''
   lnItmTypPos = ASUBSCRIPT(laVrExpr,ASCAN(laVrExpr,"ITEM.ITEM_TYPE"),1)
   IF lnItmTypPos> 0
     lcTypeStr   = laVrExpr[lnItmTypPos,6]
     IF !EMPTY(lcTypeStr)
       lcItmTypFile = gfTempName()  
       llItmTypSelect = IIF(LEN(lcTypeStr)>0,.T.,.F.) AND lfConvertToCursor(lcTypeStr,'CSTY_TYPE',lcItmTypFile)
     ENDIF 
   ENDIF 
   
   *COLOR
   llItmClrSelect = .F.
   lcItmClrFile   = ''
   lnItmClrPos = ASUBSCRIPT(laVrExpr,ASCAN(laVrExpr,"RIGHT(ITEM.STYLE,lnColorLen)"),1)

   IF lnItmClrPos > 0
     lcClrStr   = laVrExpr[lnItmClrPos,6]
     IF !EMPTY(lcClrStr)
       lcItmClrFile = gfTempName()  
       llItmClrSelect = IIF(LEN(lcClrStr)>0,.T.,.F.) AND lfConvertToCursor(lcClrStr,'COLOR',lcItmClrFile)
     ENDIF 
   ENDIF 

   *CSTYGRP
   llItmGrpSelect = .F.
   lcItmGrpFile   = ''
   lnItmGrpPos = ASUBSCRIPT(laVrExpr,ASCAN(laVrExpr,"ITEM.CSTYGROUP"),1)
   IF lnItmGrpPos > 0
     lcGrpStr   = laVrExpr[lnItmGrpPos,6]
     IF !EMPTY(lcGrpStr)
       lcItmGrpFile = gfTempName()  
       llItmGrpSelect = IIF(LEN(lcGrpStr)>0,.T.,.F.) AND lfConvertToCursor(lcGrpStr,'CSTYGRP',lcItmGrpFile)
     ENDIF 
   ENDIF 

   *SEASON
   llItmSeaSelect = .F.
   lcItmSeaFile   = ''
   lnItmSeaPos = ASUBSCRIPT(laVrExpr,ASCAN(laVrExpr,"ITEM.SEASON"),1)
   IF lnItmSeaPos > 0
     lcSeaStr   = laVrExpr[lnItmSeaPos,6]
     IF !EMPTY(lcSeaStr)
       lcItmSeaFile = gfTempName()  
       llItmSeaSelect = IIF(LEN(lcSeaStr)>0,.T.,.F.) AND lfConvertToCursor(lcSeaStr,'SEASON',lcItmSeaFile)
     ENDIF 
   ENDIF 
   
   *CDIVISION
   llItmDivSelect = .F.
   lcItmDivFile   = ''
   lnItmDivPos = ASUBSCRIPT(laVrExpr,ASCAN(laVrExpr,"ITEM.CDIVISION"),1)
   IF lnItmDivPos > 0
     lcDivStr   = laVrExpr[lnItmDivPos,6]
     IF !EMPTY(lcDivStr)
       lcItmDivFile = gfTempName()  
       llItmDivSelect = IIF(LEN(lcDivStr)>0,.T.,.F.) AND lfConvertToCursor(lcDivStr,'CDIVISION',lcItmDivFile)
     ENDIF 
   ENDIF 
   lenClrlen = loFormset.lenclrlen
   *LEN(gfitemmask("PN", "", "0002"))
   lnMajLength =loFormset.lnmajlength
   *=LEN(gfItemMask('PM','', "0002"))


   IF llItemSelect
     SELECT(lcItemFile)
     SCAN 
          *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
*!*          IF gfSqlRun("SELECT * FROM Itemloc(INDEX = Stydye) WHERE cinvtype = '0002' AND  style  LIKE '"+;
*!*                    SUBSTR(&lcItemFile..CSTYMAJOR,1,lnMajLength)+"%'"+;
*!*                    IIF(llWareHus," AND cwarecode='"+PADR(loFormset.ariaform1.kblocation.keytextbox.Value,6)+"'","") ,'ITEMLOC')
        IF gfSqlRun("SELECT * FROM Itemloc(INDEX = Stydye) WHERE cinvtype = '0002' AND  style  LIKE '"+;
                  SUBSTR(&lcItemFile..CSTYMAJOR,1,lnMajLength)+"%'"+;
                  IIF(llWareHus," AND cwarecode='"+STRTRAN(PADR(loFormset.ariaform1.kblocation.keytextbox.Value,6),"'","''")+"'","") ,'ITEMLOC')
        *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
         SELECT Itemloc
         *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
         *SCAN REST WHILE cinvtype+ style+ cwarecode+dyelot= '0002' FOR IIF(llWareHus,Itemloc.cwarecode = PADR(loFormset.ariaform1.kblocation.keytextbox.Value,6),.T.) AND ;
             gfSeek('0002'+Itemloc.Style,'Item','Style') AND IIF(llVendSelect,SEEK(Item.vendor,lcVendFile),.T.) AND ;
             IIF(llItmTypSelect,SEEK(ITEM.ITEM_TYPE,lcItmTypFile),.T.) AND ;
             IIF(!EMPTY(lcStuExp),EVALUATE(lcStuExp),.T.) AND ;
             IIF(llItmSeaSelect,SEEK(ITEM.SEASON,lcItmSeaFile),.T.) AND ;
             IIF(llItmGrpSelect,Seek(ITEM.CSTYGROUP,lcItmGrpFile),.T.) AND ;
             IIF(llItmDivSelect ,SEEK(ITEM.CDIVISION,lcItmDivFile),.T.) AND ;
             IIF(llItmClrSelect,SEEK(RIGHT(ITEM.STYLE,lnColorlen),lcItmClrFile),.T.) AND;
             IIF(lcRpDomImp='D',ITEM.MAKE,IIF(lcRpDomImp='I',!ITEM.MAKE,.T.))     
         SCAN REST WHILE cinvtype+ style+ cwarecode+dyelot= '0002' FOR IIF(llWareHus,Itemloc.cwarecode = PADR(loFormset.ariaform1.kblocation.keytextbox.Value,6),.T.) AND ;
             gfSeek('0002'+Itemloc.Style,'Item','Style') AND IIF(llVendSelect,SEEK(Item.vendor,lcVendFile),.T.) AND ;
             IIF(llItmTypSelect,SEEK(ITEM.ITEM_TYPE,lcItmTypFile),.T.) AND ;
             IIF(!EMPTY(lcStuExp),EVALUATE(lcStuExp),.T.) AND ;
             IIF(llItmSeaSelect,SEEK(ITEM.SEASON,lcItmSeaFile),.T.) AND ;
             IIF(llItmGrpSelect,Seek(ITEM.CSTYGROUP,lcItmGrpFile),.T.) AND ;
             IIF(llItmDivSelect ,SEEK(ITEM.CDIVISION,lcItmDivFile),.T.) AND ;
             IIF(llItmClrSelect,SEEK(SUBSTR(ITEM.STYLE,lnClrPo,lnColorlen),lcItmClrFile),.T.) AND;
             IIF(lcRpDomImp='D',ITEM.MAKE,IIF(lcRpDomImp='I',!ITEM.MAKE,.T.))     
        *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
           SCATTER MEMVAR MEMO
          *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
          *m.Color = RIGHT(Item.Style,lenClrlen)
           m.Color = SUBSTR(Item.Style,lnClrPo,lenClrlen)
           *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
           m.FabDesc = Item.Desc
           m.CLRDESC = lfGetDesc(m.COLOR)&&,'COLOR')

           DO CASE
             
             CASE  lcCostMeth = 'S'  && Standard
               m.Cost    = IIF(gfSeek(Item.cConvBuy,'UOM'),Item.TOTCOST/ UOM.nconf,0)
               m.oCost   = IIF(gfSeek(Item.cConvBuy,'UOM'),Item.TOTCOST/ UOM.nconf,0)
             OTHERWISE       && 'A' Average
               m.Cost    = AVE_COST
               m.oCost   = AVE_COST
           ENDCASE
           *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
           *m.item = LEFT(Style,lnMajLength )
           m.item = Style
           *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
           m.STOCK   = TOTSTK
           m.oSTOCK  = TOTSTK
           m.nStkVal = nStkVal
           IF lfChekBin()
             SELECT MDINVNTL
             gfSetOrder('MDINVNTLS')
             WAIT WINDOW LANG_Select_Records NOWAIT
             *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
             *=lfAddLTemp(PADR(m.ITEM,19),m.Color)
             =lfAddLTemp(PADR(m.ITEM,19))
              *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
           ENDIF
         ENDSCAN 
       ENDIF 
     ENDSCAN 
   ELSE
     && If user Dosn't select item range
     *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start] 
*!*       IF gfSqlRun("SELECT * FROM Itemloc(INDEX = Stydye) WHERE cinvtype = '0002' "+ ;
*!*                   IIF(llWareHus," AND cwarecode='"+PADR(loFormset.ariaform1.kblocation.keytextbox.Value,6)+"'","") ,'ITEMLOC')
     IF gfSqlRun("SELECT * FROM Itemloc(INDEX = Stydye) WHERE cinvtype = '0002' "+ ;
                 IIF(llWareHus," AND cwarecode='"+STRTRAN(PADR(loFormset.ariaform1.kblocation.keytextbox.Value,6),"'","''")+"'","") ,'ITEMLOC')
  *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]

     *gfSeek('0002','itemLoc','Stydye')
       SELECT Itemloc
       *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
       *SCAN REST WHILE cinvtype+ style+ cwarecode+dyelot= '0002' FOR IIF(llWareHus,Itemloc.cwarecode = PADR(loFormset.ariaform1.kblocation.keytextbox.Value,6),.T.) AND ;
          gfSeek('0002'+Itemloc.Style,'Item','Style') AND IIF(llVendSelect,SEEK(Item.vendor,lcVendFile),.T.) AND ;
          IIF(llItmTypSelect,SEEK(ITEM.ITEM_TYPE,lcItmTypFile),.T.) AND ;
          IIF(!EMPTY(lcStuExp),EVALUATE(lcStuExp),.T.) AND ;
          IIF(llItmSeaSelect,SEEK(ITEM.SEASON,lcItmSeaFile),.T.) AND ;
          IIF(llItmGrpSelect,Seek(ITEM.CSTYGROUP,lcItmGrpFile),.T.) AND ;
          IIF(llItmDivSelect ,SEEK(ITEM.CDIVISION,lcItmDivFile),.T.) AND ;
          IIF(llItmClrSelect,SEEK(RIGHT(ITEM.STYLE,lnColorlen),lcItmClrFile),.T.) AND ;
          IIF(lcRpDomImp='D',ITEM.MAKE,IIF(lcRpDomImp='I',!ITEM.MAKE,.T.))     
             
       SCAN REST WHILE cinvtype+ style+ cwarecode+dyelot= '0002' FOR IIF(llWareHus,Itemloc.cwarecode = PADR(loFormset.ariaform1.kblocation.keytextbox.Value,6),.T.) AND ;
          gfSeek('0002'+Itemloc.Style,'Item','Style') AND IIF(llVendSelect,SEEK(Item.vendor,lcVendFile),.T.) AND ;
          IIF(llItmTypSelect,SEEK(ITEM.ITEM_TYPE,lcItmTypFile),.T.) AND ;
          IIF(!EMPTY(lcStuExp),EVALUATE(lcStuExp),.T.) AND ;
          IIF(llItmSeaSelect,SEEK(ITEM.SEASON,lcItmSeaFile),.T.) AND ;
          IIF(llItmGrpSelect,Seek(ITEM.CSTYGROUP,lcItmGrpFile),.T.) AND ;
          IIF(llItmDivSelect ,SEEK(ITEM.CDIVISION,lcItmDivFile),.T.) AND ;
          IIF(llItmClrSelect,SEEK(SUBSTR(ITEM.STYLE,lnClrPo,lnColorlen),lcItmClrFile),.T.) AND ;
          IIF(lcRpDomImp='D',ITEM.MAKE,IIF(lcRpDomImp='I',!ITEM.MAKE,.T.))     
       *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
           SCATTER MEMVAR MEMO
           *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
           *m.Color = RIGHT(Item.Style,lenClrlen)
           m.Color = SUBSTR(Item.Style,lnClrPo,lenClrlen)
           *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
           m.FabDesc = Item.Desc
           m.CLRDESC = lfGetDesc(m.COLOR)&&,'COLOR')
           DO CASE
             CASE  lcCostMeth = 'S'  && Standard
               m.Cost    = IIF(gfSeek(Item.cConvBuy,'UOM'),Item.TOTCOST/ UOM.nconf,0)
               m.oCost   = IIF(gfSeek(Item.cConvBuy,'UOM'),Item.TOTCOST/ UOM.nconf,0)
             OTHERWISE       && 'A' Average
               m.Cost    = AVE_COST
               m.oCost   = AVE_COST
           ENDCASE
           *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
           *m.item = LEFT(Style,lnMajLength )
           m.item = Style
           *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
           m.STOCK   = TOTSTK
           m.oSTOCK  = TOTSTK
           m.nStkVal = nStkVal
           IF lfChekBin()
             SELECT MDINVNTL
             gfSetOrder('MDINVNTLS')
             WAIT WINDOW LANG_Select_Records NOWAIT
             *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
             *=lfAddLTemp(PADR(m.ITEM,19),m.Color)             
             =lfAddLTemp(PADR(m.ITEM,19))
             *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
           ENDIF
         ENDSCAN 
      ENDIF 
    ENDIF
  ELSE

   *Status
   lcStuExpSty  = ''
   lnStuPos = ASCAN(laVrExpr,"STYLE.STATUS")
   IF lnStuPos > 0 
     lnStuPos  = ASUBSCRIPT(laVrExpr,lnStuPos,1)
     lcStuExpSty = laVrExpr[lnStuPos,6]
   ENDIF  
   
   IF !EMPTY(lcStuExpSty)
     lcStuExpSty = "INLIST(STYLE.STATUS,'"+STRTRAN(lcStuExpSty,'|',"','")+"')"
   ENDIF 
  
    *COLOR
    llStyClrSelect = .F.
    lcStyClrFile   = ''
    lnStyClrPos = ASUBSCRIPT(laVrExpr,ASCAN(laVrExpr,"SUBSTR(STYLE.Style,lnClrPo,lnColorLen)"),1)
    IF lnStyClrPos > 0
      lcClrStr   = laVrExpr[lnStyClrPos,6]
      IF !EMPTY(lcClrStr)
        lcStyClrFile = gfTempName()  
        llStyClrSelect = IIF(LEN(lcClrStr)>0,.T.,.F.) AND lfConvertToCursor(lcClrStr,'COLOR',lcStyClrFile)
      ENDIF 
    ENDIF 

    *CSTYGRP
    llStyGrpSelect = .F.
    lcStyGrpFile   = ''
    lnStyGrpPos = ASUBSCRIPT(laVrExpr,ASCAN(laVrExpr,"STYLE.CSTYGROUP"),1)
    IF lnStyGrpPos  > 0
      lcGrpStr   = laVrExpr[lnStyGrpPos ,6]
      IF !EMPTY(lcGrpStr)
        lcStyGrpFile = gfTempName()  
        llStyGrpSelect = IIF(LEN(lcGrpStr)>0,.T.,.F.) AND lfConvertToCursor(lcGrpStr,'CSTYGRP',lcStyGrpFile)
      ENDIF 
    ENDIF 

    *SEASON
    llStySeaSelect = .F.
    lcStySeaFile   = ''
    lnStySeaPos = ASUBSCRIPT(laVrExpr,ASCAN(laVrExpr,"STYLE.SEASON"),1)
    IF lnStySeaPos > 0
      lcSeaStr   = laVrExpr[lnStySeaPos,6]
      IF !EMPTY(lcSeaStr)
        lcStySeaFile = gfTempName()  
        llStySeaSelect  = IIF(LEN(lcSeaStr)>0,.T.,.F.) AND lfConvertToCursor(lcSeaStr,'SEASON',lcStySeaFile)
      ENDIF 
    ENDIF 
   
    *CDIVISION
    llStyDivSelect = .F.
    lcStyDivFile = ''
    lnStyDivPos = ASUBSCRIPT(laVrExpr,ASCAN(laVrExpr,"STYLE.CDIVISION"),1)
    IF lnStyDivPos  > 0
      lcDivStr   = laVrExpr[lnStyDivPos ,6]
      IF !EMPTY(lcDivStr)
        lcStyDivFile = gfTempName()  
        llStyDivSelect = IIF(LEN(lcDivStr)>0,.T.,.F.) AND lfConvertToCursor(lcDivStr,'CDIVISION',lcStyDivFile )
      ENDIF 
    ENDIF 
    
   *--Fabric
   llFabSelect = .F.
   lcFabFile = ''
   lnFabPos  = ASCAN(laVrExpr,"STYLE.FABRIC")
   IF lnFabPos > 0 
     lnFabPos  = ASUBSCRIPT(laVrExpr,lnFabPos,1)
     lcFabFile = laVrExpr[lnFabPos ,6]
   ENDIF  
   IF !EMPTY(lcFabFile)  AND USED(lcFabFile)
     SELECT(lcFabFile)
     LOCATE 
     IF !EOF()
       llFabSelect = .T.
     ENDIF 
   ENDIF 

   *--Style
   llStySelect = .F.
   lcStyFile = ''
   lnStyPos = ASCAN(laVrExpr,"STYLE.CSTYMAJOR")
   IF lnStyPos > 0 
     lnStyPos  = ASUBSCRIPT(laVrExpr,lnStyPos,1)
     lcStyFile = laVrExpr[lnStyPos ,6]
   ENDIF  
   IF !EMPTY(lcStyFile)  AND USED(lcStyFile)
     SELECT(lcStyFile)
     LOCATE 
     IF !EOF()
       llStySelect = .T.
     ENDIF 
   ENDIF 
   *lcStyMajor  = gfItemMask('HM')
   lcStyMajor  = loFormSet.lcStyMajor
   lnMajorLen  = LEN(lcStylePic)

   IF llStySelect
     SELECT(lcStyFile)
     SCAN
       IF gfSeek(SUBSTR(&lcStyFile..CStyMajor,1,lnMajLen ),'STYDYE','STYDYE')
         SELECT STYDYE
         *B608303 NNA 04/10/2007 (Begin) Check For inventory Style only
         *SCAN REST WHILE STYLE+CWARECODE+DYELOT = SUBSTR(&lcStyFile..CStyMajor,1,lnMajLen );
           FOR IIF(loFormset.llWareHus,CwareCode = PADR(loFormset.ariaform1.kblocation.keytextbox.Value,6) ,.T.) AND ;
             gfSeek(Stydye.Style,'Style') AND ;
             IIF(!EMPTY(lcStuExpSty),EVALUATE(lcStuExpSty),.T.) AND ;
             IIF(llStyClrSelect,SEEK(SUBSTR(STYLE.Style,lnClrPo,lnColorLen),lcStyClrFile),.T.) AND ;
             IIF(llStyGrpSelect,SEEK(STYLE.CSTYGROUP,lcStyGrpFile),.T.) AND ;
             IIF(llStySeaSelect,SEEK(STYLE.SEASON,lcStySeaFile),.T.) AND ;
             IIF(llStyDivSelect,SEEK(STYLE.CDIVISION,lcStyDivFile),.T.) AND ;
             IIF(llFabSelect,SEEK(STYLE.FABRIC,lcFabFile),.T.) AND ;
             IIF(lcRPDomImp ='D',Style.MAKE,IIF(lcRpDomImp='I',!Style.MAKE,.T.))     

         SCAN REST WHILE STYLE+CWARECODE+DYELOT = SUBSTR(&lcStyFile..CStyMajor,1,lnMajLen );
           FOR IIF(loFormset.llWareHus,CwareCode = PADR(loFormset.ariaform1.kblocation.keytextbox.Value,6) ,.T.) AND ;
             gfSeek(Stydye.Style,'Style') AND STYLE.LINVSTY AND;
             IIF(!EMPTY(lcStuExpSty),EVALUATE(lcStuExpSty),.T.) AND ;
             IIF(llStyClrSelect,SEEK(SUBSTR(STYLE.Style,lnClrPo,lnColorLen),lcStyClrFile),.T.) AND ;
             IIF(llStyGrpSelect,SEEK(STYLE.CSTYGROUP,lcStyGrpFile),.T.) AND ;
             IIF(llStySeaSelect,SEEK(STYLE.SEASON,lcStySeaFile),.T.) AND ;
             IIF(llStyDivSelect,SEEK(STYLE.CDIVISION,lcStyDivFile),.T.) AND ;
             IIF(llFabSelect,SEEK(STYLE.FABRIC,lcFabFile),.T.) AND ;
             IIF(lcRPDomImp ='D',Style.MAKE,IIF(lcRpDomImp='I',!Style.MAKE,.T.))     
         *B608303 NNA 04/10/2007 (End)

           SCATTER MEMVAR MEMO
           m.StyDesc = Style.Desc1
           DO CASE
             CASE  lcCostMeth = 'S'  && Standard
               m.Cost    = STYLE.TotCost
               m.oCost   = STYLE.TotCost
             OTHERWISE       && 'A' Average
               m.Cost    = Ave_Cost
               m.oCost   = Ave_Cost
             ENDCASE
             m.ITEM    = ''
             m.STOCK   = TotStk
             m.oSTOCK  = TotStk
             m.nStkVal = nStkVal
             *T20060818.0001(C200876) TMI [Start] Add lines to the temp file to lock - BIN LOCATION             
             IF TYPE('loFormSet.llIsUseBin')='L' .AND. loFormSet.llIsUseBin
               =loFormSet.mDoTrigger(PADR('DLSELREC',10))
             ELSE
               *T20060818.0001(C200876) TMI [End  ]      
               IF lfChekBin()
                 SELECT MDINVNTL
                 gfSetOrder('MDINVNTLS')
                 WAIT WINDOW LANG_Select_Records NOWAIT
                 *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
                 *=lfAddLTemp(m.Style,m.Color)                 
                 =lfAddLTemp(m.Style)
                 *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
               ENDIF
             *T20071102.0018,10/C200876 TMI [Start] close the above if
           ENDIF
           *T20071102.0018,10/C200876 TMI [End  ] 
         ENDSCAN 
       ENDIF 
     ENDSCAN 
   ELSE
     SELECT STYDYE
     *B608303 NNA 04/10/2007 (Begin) Check For inventory Style only
     *SCAN FOR IIF(loFormset.llWareHus,CwareCode = PADR(loFormset.ariaform1.kblocation.keytextbox.Value,6) ,.T.) AND ;
         gfSeek(Stydye.Style,'Style') AND ;
         IIF(!EMPTY(lcStuExpSty),EVALUATE(lcStuExpSty),.T.) AND ;
         IIF(llStyClrSelect,SEEK(SUBSTR(STYLE.Style,lnClrPo,lnColorLen),lcStyClrFile),.T.) AND ;
         IIF(llStyGrpSelect,SEEK(STYLE.CSTYGROUP,lcStyGrpFile),.T.) AND ;
         IIF(llStySeaSelect,SEEK(STYLE.SEASON,lcStySeaFile),.T.) AND ;
         IIF(llStyDivSelect,SEEK(STYLE.CDIVISION,lcStyDivFile),.T.) AND ;
         IIF(llFabSelect,SEEK(STYLE.FABRIC,lcFabFile),.T.) AND ;
         IIF(lcRPDomImp ='D',Style.MAKE,IIF(lcRpDomImp='I',!Style.MAKE,.T.))     
     SCAN FOR IIF(loFormset.llWareHus,CwareCode = PADR(loFormset.ariaform1.kblocation.keytextbox.Value,6) ,.T.) AND ;
         gfSeek(Stydye.Style,'Style') AND STYLE.LINVSTY AND ;
         IIF(!EMPTY(lcStuExpSty),EVALUATE(lcStuExpSty),.T.) AND ;
         IIF(llStyClrSelect,SEEK(SUBSTR(STYLE.Style,lnClrPo,lnColorLen),lcStyClrFile),.T.) AND ;
         IIF(llStyGrpSelect,SEEK(STYLE.CSTYGROUP,lcStyGrpFile),.T.) AND ;
         IIF(llStySeaSelect,SEEK(STYLE.SEASON,lcStySeaFile),.T.) AND ;
         IIF(llStyDivSelect,SEEK(STYLE.CDIVISION,lcStyDivFile),.T.) AND ;
         IIF(llFabSelect,SEEK(STYLE.FABRIC,lcFabFile),.T.) AND ;
         IIF(lcRPDomImp ='D',Style.MAKE,IIF(lcRpDomImp='I',!Style.MAKE,.T.))     
      *B608303 NNA 04/10/2007 (End)
       SCATTER MEMVAR MEMO
       m.StyDesc = Style.Desc1
       DO CASE
         CASE  lcCostMeth = 'S'  && Standard
           m.Cost    = STYLE.TotCost
           m.oCost   = STYLE.TotCost
         OTHERWISE       && 'A' Average
           m.Cost    = Ave_Cost
           m.oCost   = Ave_Cost
         ENDCASE
         m.ITEM    = ''
         m.STOCK   = TotStk
         m.oSTOCK  = TotStk
         m.nStkVal = nStkVal
         *T20060818.0001(C200876) TMI [Start] Add lines to the temp file to lock - BIN LOCATION
         IF TYPE('loFormSet.llIsUseBin')='L' .AND. loFormSet.llIsUseBin
           =loFormSet.mDoTrigger(PADR('DLSELREC',10))
         ELSE
           *T20060818.0001(C200876) TMI [End  ]      
           IF lfChekBin()
             SELECT MDINVNTL
             gfSetOrder('MDINVNTLS')
             WAIT WINDOW LANG_Select_Records NOWAIT
             *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
             *=lfAddLTemp(m.Style,m.Color)             
             =lfAddLTemp(m.Style)
             *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
           ENDIF
           *T20071102.0018,10/C200876 TMI [Start] 
         ENDIF
         *T20071102.0018,10/C200876 TMI [End  ] 
      ENDSCAN 
    ENDIF 
  ENDIF 
ENDIF 

lcStyPic  = loFormSet.lcstytitle
*= gfItemMask("HI")

IF llLogic
  PRIVATE lcAlias
  lcAlias = SELECT(0)
  SELECT (loFormSet.lcRejSty)
  LOCATE 
  IF !EOF()
     lnAnswer = gfModalGen("QRM00000B42017","DIALOG",.F.,.F.,LANG_One_More + lcStyPic +LANG_Rej_Sty)
     =lfPrnRep()
  ENDIF
  SELECT(lcAlias)
ENDIF

*---Function To Get Batch Line And Insert Into Temp File
=lfTmpBtchL()

SELECT IIF(loFormSet.llMatModul,'ITEMLOC','StyDye')
WAIT CLEAR
loFormSet.ariaFORM1.grditems.refresh


*!*************************************************************
*! Name      : lfGetHdData
*! Developer : Mariam Mazhar [MMT]
*! Date      : 11/19/2006
*! Purpose   : Function to get header data.
*!*************************************************************
FUNCTION lfGetHdData
PARAMETERS loFormset

 SELECT (loFormset.lcTmpQuery)
 DELETE ALL 
 SELECT (loFormset.lcBatLin)
 DELETE ALL 
 SELECT (loFormset.lcBatHdr)
 DELETE ALL 

*IF gfSEEK(IIF(loFormSet.llmatmodul,'M','S')+loFormset.ariaform1.kbBatchNo.keytextbox.Value,'MDINVNTH')
  SELECT MDINVNTH
  loFormset.ariaform1.kbBatchNo.keytextbox.Value = clkBatch 
  loFormset.ariaform1.dtpLock.Value = Date
  loFormset.ariaform1.dtpPost.Value = IIF(!EMPTY(DPostDate),DPostDate,{})
  loFormset.ariaform1.kbLocation.keytextbox.Value = cWareCode
  loFormset.ariaform1.txtNotes.Value = content
  loFormset.lcStatus = Type
  IF MDINVNTH.Type <> 'P'
    loFormset.ariaform1.txtStatus.Value     = IIF(loFormset.lcStatus = 'H','Hold','Open')
    loFormset.AriaForm1.txtStatus.Ariacolor = IIF(loFormset.lcStatus = 'H',"R" ,"G" )
    IF loFormset.activemode <> 'V'
      loFormset.ariaform1.cmdZero.Enabled     = .T. 
    ENDIF 
    IF MDINVNTH.Type = 'L'
      loFormset.ariaform1.dtpPost.Enabled = .F.
      loFormset.oToolBar.ChangeButtonStatus('cmdPost','DISABLED')
      loFormset.oToolBar.cmdDelete.Enabled = .T.
    ELSE
     IF loFormset.activemode <> 'V'
       loFormset.ariaform1.dtpPost.Enabled = .T.
     ENDIF   
     *loFormset.ariaform1.dtpPost.Value = oariaapplication.systemdate
  	 loFormset.oToolBar.cmdDelete.Enabled = .F.    
    ENDIF 
    loFormset.AriaForm1.txtStatus.Ariacolor = "G" 
  ELSE
    loFormset.oToolBar.ChangeButtonStatus('cmdPost','DISABLED')
    loFormset.ariaform1.txtStatus.Value     = 'Posted'
    loFormset.ariaform1.cmdZero.Enabled     = .F. 
    loFormset.AriaForm1.txtStatus.Ariacolor = "R" 
    loFormset.oToolBar.cmdDelete.Enabled = .F.    
    loFormset.oToolBar.cmdEdit.Enabled = .F.
  ENDIF 
  IF MDINVNTH.Type = 'H'
    loFormset.AriaForm1.txtStatus.Ariacolor = "R" 
    loFormset.ariaform1.cmdZero.Enabled     = .F. 
  ENDIF
=lfGetData()
loFormSet.ariaFORM1.grditems.refresh
loFormSet.mactivatepad 
*ENDIF 

*!*************************************************************
*! Name      : lfGetData
*! Developer : Mariam Mazhar [MMT]
*! Date      : 11/19/2006
*! Purpose   : Function to get data.
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            : =lfGetData()
*!*************************************************************
*!
FUNCTION lfGetData

STORE 0 TO lnCountLn , lnRest , loFormSet.lnTotRec
lcStyHdr = loFormSet.lcStyMajor
*gfItemMask('HM')
lcKey = IIF(loFormSet.llmatmodul,'M','S')

SELECT MDINVNTH
*!*  =gfSEEK(lcKey +loFormset.ariaform1.kbBatchNo.keytextbox.Value)
SCATTER MEMVAR MEMO
INSERT INTO (loFormset.lcBatHdr) FROM MEMVAR
SELECT MDINVNTL
gfSetOrder('MDINVNTL')
=gfSEEK(lcKey +MDINVNTH.cLkBatch)

lcStrtTime = SECONDS()
*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
*!*  SCAN REST WHILE cBatType+cLkBatch = lcKey +PADR(loFormset.ariaform1.kbBatchNo.keytextbox.Value,6) &&;
*!*                  FOR IIF(loFormSet.llDyelot  AND lfStyDye(Style,Color),!EMPTY(Dyelot),.T.)
SCAN REST WHILE cBatType+cLkBatch = lcKey +PADR(loFormset.ariaform1.kbBatchNo.keytextbox.Value,6) ;
                FOR IIF(loFormSet.llDyelot  AND lfStyDye(Style),!EMPTY(Dyelot),.T.)
*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
  WAIT WINDOW 'Selecting ' + IIF(loFormSet.llMatModul,'ITEMS',lcStyHdr)+ ' : ' + Style NOWAIT
  loFormSet.lnTotRec = loFormSet.lnTotRec+ 1
  
  SCATTER MEMVAR MEMO
  *(loFormSet.llDyelot  AND lfStyDye(Style,Color) AND !EMPTY(Dyelot)) OR
*!*    IF   !loFormSet.llDyelot  ; 
*!*      .OR. (loFormSet.llDyelot  AND !lfStyDye(Style,Color))
    
    IF loFormset.llMatModul
      *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
      *m.FabDesc = IIF(gfSEEK('0002'+ALLTRIM(m.Style),'Item'),Item.Desc,'')
      m.FabDesc = IIF(gfSEEK('0002'+ALLTRIM(m.Style),'Item','STYLE'),Item.Desc,'')
      *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
      m.CLRDESC = lfGetDesc(COLOR)&&,'COLOR')
    ELSE
      m.StyDesc = IIF(gfSEEK(m.Style,'Style'),Style.Desc1,'')
    ENDIF
    
    m.ITEM    = IIF(loFormset.llMatModul,STYLE,'')
    m.Stock   = IIF(loFormset.llMatModul,OnHand,TotStk)
    m.Cost    = COST
    m.OStock  = IIF(loFormset.llMatModul,OldQty,OldTotStk)
    m.OCost   = OldCOST

*!*      IF ASCAN(laEvntTrig,PADR("DLASSBIN",10)) <> 0 
*!*        =gfDoTriger("ICINVLK",PADR("DLASSBIN",10))
*!*      ELSE
    *T20060818.0001(C200876) TMI [Start] 
    IF ASCAN(loFormSet.laEvntTrig,PADR('DLASSBIN',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0 ;
      .AND. gfDoTriger('POSTREC',PADR('ISUSEBIN',10))
      =loFormSet.mDoTrigger(PADR('DLASSBIN',10)) 
    ELSE
    *T20060818.0001(C200876) TMI [End  ] 
    *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
    *IF SEEK(m.STYLE+m.Color,loFormSet.lcTmpQuery)    
    IF SEEK(m.STYLE,loFormSet.lcTmpQuery)
    *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
        SELECT (loFormSet.lcTmpQuery)
        REPLACE Stock WITH Stock + m.Stock;
                oStock WITH oStock + m.oStock
    ELSE
      INSERT INTO (loFormSet.lcTmpQuery) FROM MEMVAR
    ENDIF
*!*    ENDIF 

    INSERT INTO (loFormSet.lcBatLin) FROM MEMVAR

    *T20060818.0001(C200876) TMI [Start] 
    ENDIF
    *T20060818.0001(C200876) TMI [End  ] 
ENDSCAN
*WAIT WINDOW SECONDS()-lcStrtTime 
WAIT CLEAR
SELECT(loFormSet.lcTmpQuery)
LOCATE 

*T20071102.0018,10/C200876 TMI 06/03/2008 [Start] 
IF ASCAN(loFormSet.laEvntTrig,PADR('ISUSEBIN',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0 ;
   .AND. gfDoTriger('POSTREC',PADR('ISUSEBIN',10))
  llLogic = .F.
  lcCostMeth = gfGetMemVar('M_COST_MET') 
  =lfTmpBtchL()
ENDIF  
*T20071102.0018,10/C200876 TMI 06/03/2008 [End  ] 

*!*************************************************************
*! Name      : lfTempFile
*! Developer : Mariam Mazhar [MMT]
*! Date      : 11/19/2006
*! Purpose   : Function to Create Temp File.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Called from : 
*!         Procedures : ICINVLK.PRG
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfTempFile()
*!*************************************************************
*!
FUNCTION lfTempFile
PARAMETERS loFormSet


IF loFormSet.llMatModul
	DIMENSION laFileStru[13, 4]

	laFileStru[1,1] = 'ITEM'
	laFileStru[1,2] = 'C'
	laFileStru[1,3] = 19
	laFileStru[1,4] = 0

  laFileStru[2,1] = 'COLOR'
  laFileStru[2,2] = 'C'
  laFileStru[2,3] = 6
  laFileStru[2,4] = 0

  laFileStru[3,1] = 'FabDesc'
  laFileStru[3,2] = 'C'
  laFileStru[3,3] = 20
  laFileStru[3,4] = 0

  laFileStru[4,1] = 'CLRDESC'
  laFileStru[4,2] = 'C'
  laFileStru[4,3] = 30
  laFileStru[4,4] = 0

  laFileStru[5,1] = 'STOCK'
  laFileStru[5,2] = 'N'
  laFileStru[5,3] = 12
  laFileStru[5,4] = 3

  laFileStru[6,1] = 'OSTOCK'
  laFileStru[6,2] = 'N'
  laFileStru[6,3] = 12
  laFileStru[6,4] = 3

  laFileStru[7,1] = 'nStkVal'
  laFileStru[7,2] = 'N'
  laFileStru[7,3] = 12
  laFileStru[7,4] = 2

  laFileStru[8,1] = 'BIN'
  laFileStru[8,2] = 'C'
  laFileStru[8,3] = 10
  laFileStru[8,4] = 0

  laFileStru[9,1] = 'Cost'
  laFileStru[9,2] = 'N'
  laFileStru[9,3] = 10
  laFileStru[9,4] = 3

  laFileStru[10,1] = 'OCost'
  laFileStru[10,2] = 'N'
  laFileStru[10,3] = 10
  laFileStru[10,4] = 3

  laFileStru[11,1] = 'CADD_USER'
  laFileStru[11,2] = 'C'
  laFileStru[11,3] = 10
  laFileStru[11,4] = 0

  laFileStru[12,1] = 'CADD_TIME'
  laFileStru[12,2] = 'C'
  laFileStru[12,3] = 11
  laFileStru[12,4] = 0
  
  laFileStru[13,1] = 'DADD_DATE '
  laFileStru[13,2] = 'D'
  laFileStru[13,3] = 8
  laFileStru[13,4] = 0
  *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
  *=gfCrtTmp(loFormSet.lcTmpQuery,@laFileStru,'ITEM+Color',loFormSet.lcTmpQuery)  
  =gfCrtTmp(loFormSet.lcTmpQuery,@laFileStru,'ITEM',loFormSet.lcTmpQuery)
  *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
ELSE
  DIMENSION laFileStru[12, 4]

  laFileStru[1,1] = 'STYLE'
  laFileStru[1,2] = 'C'
  laFileStru[1,3] = 19
  laFileStru[1,4] = 0

  laFileStru[2,1] = 'COLOR'
  laFileStru[2,2] = 'C'
  laFileStru[2,3] = 6
  laFileStru[2,4] = 0

  laFileStru[3,1] = 'StyDesc'
  laFileStru[3,2] = 'C'
  laFileStru[3,3] = 60
  laFileStru[3,4] = 0


  laFileStru[4,1] = 'STOCK'
  laFileStru[4,2] = 'N'
  laFileStru[4,3] = 10
  laFileStru[4,4] = 0

  laFileStru[5,1] = 'OSTOCK'
  laFileStru[5,2] = 'N'
  laFileStru[5,3] = 10
  laFileStru[5,4] = 0

  laFileStru[6,1] = 'nStkVal'
  laFileStru[6,2] = 'N'
  laFileStru[6,3] = 12
  laFileStru[6,4] = 2

  laFileStru[7,1] = 'BIN'
  laFileStru[7,2] = 'C'
  laFileStru[7,3] = 10
  laFileStru[7,4] = 0

  laFileStru[8,1] = 'Cost'
  laFileStru[8,2] = 'N'
  laFileStru[8,3] = 10
  laFileStru[8,4] = 2

  laFileStru[9,1] = 'OCost'
  laFileStru[9,2] = 'N'
  laFileStru[9,3] = 10
  laFileStru[9,4] = 2

  laFileStru[10,1] = 'CADD_USER'
  laFileStru[10,2] = 'C'
  laFileStru[10,3] = 10
  laFileStru[10,4] = 0

  laFileStru[11,1] = 'CADD_TIME'
  laFileStru[11,2] = 'C'
  laFileStru[11,3] = 11
  laFileStru[11,4] = 0
  
  laFileStru[12,1] = 'DADD_DATE '
  laFileStru[12,2] = 'D'
  laFileStru[12,3] = 8
  laFileStru[12,4] = 0
  
    *T20071102.0018,10/C200876 TMI 05/29/2008 [Start] Add the bin field to the created temp index forlcTmpQuery
  IF ASCAN(loFormSet.laEvntTrig,PADR('ISUSEBIN',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0 ;
    .AND. gfDoTriger('POSTREC',PADR('ISUSEBIN',10))
    *T20071102.0018,10/C200876 TMI 07/07/2008 [Start] let lcTmpQuery be shown by bin+style+color
    *=gfCrtTmp(loFormSet.lcTmpQuery,@laFileStru,'Style+Color+BIN',loFormSet.lcTmpQuery)
    =gfCrtTmp(loFormSet.lcTmpQuery,@laFileStru,'BIN+Style+Color',loFormSet.lcTmpQuery)
    *T20071102.0018,10/C200876 TMI 07/07/2008 [End  ] 
  ELSE
    *T20071102.0018,10/C200876 TMI 05/29/2008 [End  ] 
    *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
    *=gfCrtTmp(loFormSet.lcTmpQuery,@laFileStru,'Style+Color',loFormSet.lcTmpQuery)    
    =gfCrtTmp(loFormSet.lcTmpQuery,@laFileStru,'Style',loFormSet.lcTmpQuery)
    *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
    *T20071102.0018,10/C200876 TMI 05/29/2008 [Start] 
  ENDIF
  *T20071102.0018,10/C200876 TMI 05/29/2008 [End  ] 
  
ENDIF
*--- Create Temp Header file
SELECT MDINVNTH
=AFIELDS(laTpFlds)
lnTpFlds = ALEN(laTpFlds,1)
DIMENSION  laTpFlds[lnTpFlds+1,18]
laTpFlds[lnTpFlds+1,1] = 'nSteps'
laTpFlds[lnTpFlds+1,2] = 'N'
laTpFlds[lnTpFlds+1,3] = 2
laTpFlds[lnTpFlds+1,4] = 0

  STORE ' ' TO  laTpFlds[lnTpFlds+1,7],laTpFlds[lnTpFlds+1,8],;
                laTpFlds[lnTpFlds+1,9],laTpFlds[lnTpFlds+1,10],;
                laTpFlds[lnTpFlds+1,11],laTpFlds[lnTpFlds+1,12],;
                laTpFlds[lnTpFlds+1,13],laTpFlds[lnTpFlds+1,14],;
                laTpFlds[lnTpFlds+1,15],laTpFlds[lnTpFlds+1,16]
  STORE 0 TO    laTpFlds[lnTpFlds+1,17] ,laTpFlds[lnTpFlds+1,18]

=gfCrtTmp(loFormSet.lcBatHdr,@laTpFlds,[cbattype+cLkBatch],loFormSet.lcBatHdr)


SELECT (loFormSet.lcBatHdr)

*--- Create Temp Line file
SELECT MDINVNTL
=AFIELDS(laTpFlds)
DIMENSION laIndexArr[2,2]
*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
*laIndexArr[1,1] = [cbattype+cLkBatch+style+color+dyelot+clocation]
laIndexArr[1,1] = [cbattype+cLkBatch+style+dyelot+clocation]
*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
laIndexArr[1,2] = loFormSet.lcBatInd
*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
*laIndexArr[2,1] = [style+color+dyelot+clocation+cbattype+cLkBatch]
laIndexArr[2,1] = [style+dyelot+clocation+cbattype+cLkBatch]
*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
laIndexArr[2,2] = loFormSet.lcStyInd

=gfCrtTmp(loFormSet.lcBatLin,@laTpFlds,@laIndexArr,loFormSet.lcBatLin)

*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
IF loFormSet.llMatModul
  loFormSet.ariaFORM1.Caption = STRTRAN(loFormSet.Ariaform1.Caption,LANG_STYINVLOCK,LANG_MATINVLOCK)
ENDIF   
*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]


*!*************************************************************
*! Name      : lfStyDye
*! Developer : Mariam Mazhar [MMT]
*! Date      : 11/19/2006
*! Purpose   : Functio to check if style/item is dyelot.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Called from : 
*!         Procedures : ICINVLK.PRG
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfStyDye()
*!*************************************************************
*!
FUNCTION lfStyDye
*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
*PARAMETER lcSty1,lcColr1
PARAMETER lcSty1
*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
llRet = .F.
IF loFormSet.llMatModul
 lcCurrAlias = ALIAS()
 lenClrlen = loFormSet.lenclrlen
 *= LEN(gfitemmask("PN", "", "0002"))
 *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
  *= gfSqlRun("SELECT * FROM Item(INDEX = cStyle) WHERE cinvtype = '0002' AND  style  LIKE '"+;
                  SUBSTR(lcSty1,1,7)+"%'"+;
                  IIF(EMPTY(lcColr1),''," And Right(style,"+ALLTRIM(STR(lenClrlen))+") like '"+lcColr1+"'") ,'ITEM')
  = gfSqlRun("SELECT * FROM Item(INDEX = cStyle) WHERE cinvtype = '0002' AND  style  LIKE '"+;
                  lcSty1+"%'",'ITEM')
  *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]

 * =gfSEEK('0002'+SUBSTR(lcSty1,1,7)+lcColr1,'ITEM')
  llRet = (ITEM.cDye_Flg='Y')
  SELECT(lcCurrAlias)
ELSE
  =gfSEEK(lcSty1,'Style')
  llRet = (Style.cDye_Flg='Y')
ENDIF
llStyDye = llRet
RETURN (llRet)

*!*************************************************************
*! Name      : lfAddControlSource
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 11/19/2006
*! Purpose   : Add Control Source for main screens grid
*!*************************************************************
FUNCTION lfAddControlSource
PARAMETERS loFormSet

lcStyPic  = loFormSet.lcstytitle
*gfItemMask("HI")

lcMajPict = loFormSet.lcmajpict
*= gfItemMask("PM")
WITH loFormSet.ariaFORM1.grditems

  .RecordSource = ''
  .RecordSource = loFormSet.lcTmpQuery
  IF loFormSet.llMatModul
    .ColumnCount = 9
    
    .column1.ControlSource = loFormSet.lcTmpQuery+'.ITEM'
    .column1.header1.Caption = LANG_Item_Title
    *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
    *.coLUMN1.Width = 80    
    .COLUMN1.Width = 140
    *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
    .column2.ControlSource = loFormSet.lcTmpQuery+'.COLOR'
    .column2.header1.Caption = LANG_Clr_Title
    .coLUMN2.Width = 60
    *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
    .coLUMN2.Visible =.F.
    *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
    

    .column3.ControlSource = loFormSet.lcTmpQuery+'.FabDesc'
    .column3.header1.Caption = LANG_Itm_Desc
    .coLUMN3.Width = 150
    
    .column4.ControlSource = loFormSet.lcTmpQuery+'.CLRDESC'
    .column4.header1.Caption = LANG_Clr_Desc
    .coLUMN4.Width = 100
    
    
    .column5.ControlSource = loFormSet.lcTmpQuery+'.OStock'
    .column5.header1.Caption = LANG_Old_onHand
    .coLUMN5.Width = 60
    
    .column6.ControlSource = loFormSet.lcTmpQuery+'.OCost'
    .column6.header1.Caption = LANG_Old_Cost
    .coLUMN6.Width = 60
    
    .column7.ControlSource = loFormSet.lcTmpQuery+'.Stock'
    .column7.header1.Caption = LANG_New_onHand
    .coLUMN7.Width = 80
         
    .column8.ControlSource = loFormSet.lcTmpQuery+'.Cost'
    .column8.header1.Caption = LANG_New_Cost
    .coLUMN8.Width = 60

    .column9.visible = .F.    
  ELSE
    .ColumnCount = 7
    .column1.ControlSource = loFormSet.lcTmpQuery+'.Style'
    .column1.header1.Caption = lcStyPic
    .coLUMN1.Width = 120
    
    .column2.ControlSource = loFormSet.lcTmpQuery+'.STYDESC'
    .column2.header1.Caption = LANG_Desc
    .coLUMN2.Width = 200

    .column3.ControlSource = loFormSet.lcTmpQuery+'.OStock'
    .column3.header1.Caption = LANG_Old_Stk
    
    .column4.ControlSource = loFormSet.lcTmpQuery+'.OCost'
    .column4.header1.Caption = LANG_Old_Cost 
    
    .column5.ControlSource = loFormSet.lcTmpQuery+'.Stock'
    .column5.header1.Caption = LANG_New_Stk
    
    .column6.ControlSource = loFormSet.lcTmpQuery+'.Cost'
    .column6.header1.Caption = LANG_New_Cost
    .column7.visible = .F.

    *T20060818.0001(C200876) TMI [Start] Add a column labeled 'BIN' to the grid browse in the ICINVLOC screen BIN LOCATION 
    IF ASCAN(loFormSet.laEvntTrig,PADR('DLBRWBIN',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0
      =loFormSet.mDoTrigger(PADR('DLBRWBIN',10)) 
    ENDIF
    *T20060818.0001(C200876) TMI [End  ] 

  ENDIF 
  .SETALL('ReadOnly',.T.,'COLUMN')
ENDWITH 



*!*************************************************************
*! Name      : lfVMkDwn
*! Developer : Mariam Mazhar [MMT]
*! Date      : 11/19/2006
*! Purpose   : Valid Function for MkDwn buton.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : lfShowBtns
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfVMkDwn()
*!*************************************************************
*!
FUNCTION lfVMkDwn
PARAMETERS loFormSet
lcKey = IIF(loFormSet.llmatmodul,'M','S')
IF loFormSet.llLinkGL
  lnoldals = SELECT(0)
  SELECT CODES
  lcCodTag=TAG()
  gfSetOrder('cCode_no')
  IF !gfSEEK("N"+'CADJREASON','CODES')
    *--You have to edit the Adjustment reasons codes first, Cannot proceed.
    =gfModalGen('TRM42111B42001','DIALOG')
    gfSetOrder(lcCodTag)
    SELECT &lnoldals
    RETURN 
  ELSE
    =gfSEEK("D"+'CADJREASON','CODES')
    lcDefAdjCd = Codes.Ccode_No 
  ENDIF  
  gfSetOrder(lcCodTag)
  SELECT(lnoldals)
ENDIF

SELECT (loFormSet.lcBatLin)
SET ORDER TO (loFormSet.lcBatInd)
IF loFormSet.llMatModul
*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
*!*    =SEEK(lcKey +PADR(loFormset.ariaform1.kbBatchNo.keytextbox.Value,6) +;
*!*     EVALUATE(loFormset.lcTmpQuery+'.Item')+EVALUATE(loFormset.lcTmpQuery+'.Color'))
  =SEEK(lcKey +PADR(loFormset.ariaform1.kbBatchNo.keytextbox.Value,6) +;
   EVALUATE(loFormset.lcTmpQuery+'.Item'))
*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
ELSE
  *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
  *=SEEK(lcKey +PADR(loFormset.ariaform1.kbBatchNo.keytextbox.Value,6)+EVALUATE(loFormset.lcTmpQuery+'.Style')+EVALUATE(loFormset.lcTmpQuery+'.Color'))
  =SEEK(lcKey +PADR(loFormset.ariaform1.kbBatchNo.keytextbox.Value,6)+EVALUATE(loFormset.lcTmpQuery+'.Style'))
  *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
ENDIF
lcSty = Style  
lcClr = Color

IF loFormset.llMatModul
  *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
  *=gfSEEK('0002'+SUBSTR(lcSty,1,7)+lcClr,'ITEM')
  =gfSEEK('0002'+lcSty,'ITEM')
  *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
  llStyDye = (ITEM.cDye_Flg='Y')
ELSE
  =gfSEEK(lcSty,'Style')
  llStyDye = (Style.cDye_Flg='Y')
ENDIF
SELECT (loFormset.lcBatLin)
SET ORDER TO (loFormset.lcBatInd)
*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
*=SEEK(lcKey+PADR(loFormset.ariaform1.kbBatchNo.keytextbox.Value,6)+lcSty+lcClr)
=SEEK(lcKey+PADR(loFormset.ariaform1.kbBatchNo.keytextbox.Value,6)+lcSty)
*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
IF loFormset.llDyelot AND llStyDye
*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
*!*    LOCATE REST WHILE cbattype+cLkBatch+style+color+DYELOT =;
*!*                      lcKey+PADR(loFormset.ariaform1.kbBatchNo.keytextbox.Value,6)+lcSty+lcClr;
*!*                 FOR !EMPTY(DYELOT)
    LOCATE REST WHILE cbattype+cLkBatch+style+DYELOT =;
                    lcKey+PADR(loFormset.ariaform1.kbBatchNo.keytextbox.Value,6)+lcSty;
               FOR !EMPTY(DYELOT)
*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
  IF FOUND()
    lcDyelot = DYELOT
  ELSE
    *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
   * =SEEK(lcKey+PADR(loFormset.ariaform1.kbBatchNo.keytextbox.Value,6)+lcSty+lcClr)    
    =SEEK(lcKey+PADR(loFormset.ariaform1.kbBatchNo.keytextbox.Value,6)+lcSty)
    *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
  ENDIF
ENDIF
*DECLARE laMloc[1]
IF loFormSet.llLoc
 =lfFillBin()
ENDIF
SELECT (loFormSet.lcBatLin)
lnLoc = IIF(ASCAN(loFormSet.laMloc,cLocation)<>0,ASCAN(loFormSet.laMloc,cLocation),1)
IF loFormset.llMatModul
*mmt2
*!*    FOR I = 1 TO 8
*!*      Z = STR (I,1)
*!*      lcSz&Z = ''
*!*      lnStk&Z = 0
*!*    ENDFOR
  IF gfSEEK('S'+Scale,'Scale')
    FOR I = 1 TO 8
      Z = STR (I,1)
      lcSz&Z = Scale.Sz&Z
    ENDFOR
  ENDIF
  FOR I = 1 TO 8
    Z = STR (I,1)
    lnStk&Z = Stk&Z
  ENDFOR
*mmt2
ELSE
  IF gfSEEK('S'+Scale,'Scale')
    FOR I = 1 TO 8
      Z = STR (I,1)
      lcSz&Z = Scale.Sz&Z
    ENDFOR
  ENDIF
  FOR I = 1 TO 8
    Z = STR (I,1)
    lnStk&Z = Stk&Z
  ENDFOR
ENDIF
IF loFormSet.llMatModul
  lnTOTSTK   = OnHand
  lnMMCost   = COST
  lnMOldCost = OldCOST
  lcMReason  = cReason
  *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
  *lcMatDesc  = IIF(gfSEEK('0002'+SUBSTR(lcSty,1,7)+lcClr,'Item'),ITEM.Desc,'')
  lcMatDesc  = IIF(gfSEEK('0002'+lcSty,'Item'),ITEM.Desc,'')
  *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]

  lcClrDes   = lfGetDesc(lcClr)&&,'COLOR')
  lnMNewTot  = lnTOTSTK
ELSE
  lnMCost   = COST
  lnOldCost = OldCOST
  lcReason  = cReason
  lcStyDesc = IIF(gfSEEK(lcSty,'Style'),Style.Desc1,'')
  lnNewTot  = lnStk1+lnStk2+lnStk3+lnStk4+lnStk5+lnStk6+lnStk7+lnStk8
ENDIF
lcMat     = lcSty
lcMatClr  = lcClr

IF loFormSet.llMatModul
*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
*!*    SELECT * FROM (loFormSet.lcBatLin) WHERE Style+Color = lcSty + lcClr ;
*!*                               AND IIF(loFormSet.llDyelot AND lfStyDye(lcSty ,lcClr),!EMPTY(DYELOT),.T.) INTO DBF (loFormSet.lcDetLin)
*!*    INDEX ON Style+color+dyelot+clocation TAG (loFormSet.lcDetLin)
  SELECT * FROM (loFormSet.lcBatLin) WHERE Style= lcSty ;
                             AND IIF(loFormSet.llDyelot AND lfStyDye(lcSty),!EMPTY(DYELOT),.T.) INTO DBF (loFormSet.lcDetLin)
  INDEX ON Style+dyelot+clocation TAG (loFormSet.lcDetLin)                             
*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
  
  IF _TALLY = 0
    *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
    *lcMes = 'Item - Color ' + lcSty+' - '+lcClr    
    lcMes = 'Item - Color ' + lcSty
    *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
    =gfModalGen('TRM42210B00000','DIALOG',lcMes)
    RETURN
  ENDIF
ELSE


*!*    IF ASCAN(laEvntTrig,PADR("DLSELTMP",10)) <> 0 
*!*      =gfDoTriger("ICINVLK",PADR("DLSELTMP",10))
*!*    ELSE
  *T20060818.0001(C200876) TMI [Start] select Records according to the bin location in case of pushing Edit Button in the Inventory Locking Screen.
  IF ASCAN(loFormSet.laEvntTrig,PADR('DLSELTMP',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0 ;
    .AND. gfDoTriger('POSTREC',PADR('ISUSEBIN',10))
    =loFormSet.mDoTrigger(PADR('DLSELTMP',10)) 
  ELSE  
  *T20060818.0001(C200876) TMI [End  ] 

    SELECT * FROM (loFormSet.lcBatLin) WHERE Style = lcSty  ;
                               AND IIF(loFormSet.llDyelot AND lfStyDye(lcSty),!EMPTY(EVALUATE(loFormSet.lcBatLin+'.DYELOT')),.T.) INTO DBF (loFormSet.lcDetLin)
 
*!*    ENDIF
  *T20060818.0001(C200876) TMI [Start] 
  ENDIF
  *T20060818.0001(C200876) TMI [End  ]  
 
  *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
  *INDEX ON style+color+dyelot+clocation TAG (loFormSet.lcDetLin)
  INDEX ON style+dyelot+clocation TAG (loFormSet.lcDetLin)
  *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
  IF _TALLY = 0
    lcStyPic  = loFormSet.lcstytitle
    *gfItemMask("HI")
    lcMes = lcStyPic +' '+ lcSty
    =gfModalGen('TRM42210B00000','DIALOG',lcMes)
    RETURN
  ENDIF
ENDIF


lnBin = 0



IF loFormSet.llMatModul
 *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start] 
 *IF (loFormSet.llDyelot AND lfStyDye(lcSty ,lcClr)) OR (loFormSet.llLoc AND ALEN(loFormSet.laMLoc) > 1) 
 IF (loFormSet.llDyelot AND lfStyDye(lcSty)) OR (loFormSet.llLoc AND ALEN(loFormSet.laMLoc) > 1)
  *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
   IF !EMPTY(cLocation)
     lnBin = 1
   ENDIF
   
   DO FORM  oAriaApplication.ScreenHome+("ICINVMLM.SCX") WITH loFormSet
 ELSE
   DO FORM  oAriaApplication.ScreenHome+("ICINVMM.Scx") WITH loFormSet
 ENDIF
ELSE
 *T20071102.0018,10/C200876 TMI 07/03/2008 [Start] open the 'ICINVBN' screen
 IF ASCAN(loFormSet.laEvntTrig,PADR('ICINVBN',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0 ;
   AND loFormSet.mDoTrigger(PADR('ISUSEBIN',10))
   =loFormSet.mDoTrigger(PADR('ICINVBN',10))
 ELSE
   *T20071102.0018,10/C200876 TMI 07/03/2008 [End  ] 
   IF (loFormSet.llDyelot AND lfStyDye(lcSty)) OR (loFormSet.llLoc AND ALEN(loFormSet.laMLoc) > 1)
     IF !EMPTY(cLocation)
       lnBin = 1
     ENDIF
     DO FORM  oAriaApplication.ScreenHome+("ICINVMLS.Scx") WITH loFormSet
   ELSE
     DO FORM  oAriaApplication.ScreenHome+("ICINVSS.Scx") WITH loFormSet
   ENDIF
   *T20071102.0018,10/C200876 TMI 07/03/2008 [Start] 
 ENDIF
 *T20071102.0018,10/C200876 TMI 07/03/2008 [End  ] 
ENDIF

IF loFormset.lcStatus = 'L'
  loFormset.ariaform1.dtpPost.Enabled = .F.
  loFormset.oToolBar.ChangeButtonStatus('cmdPost','DISABLED')
  loFormset.oToolBar.cmdDelete.Enabled = .F.
ELSE
  loFormset.oToolBar.cmdDelete.Enabled = .F.
  IF loFormset.lcStatus  = 'M'
    loFormset.ariaform1.dtpPost.Enabled = .T.
  ELSE
    loFormset.ariaform1.dtpPost.Enabled = .F.
  ENDIF 
ENDIF 
loFormset.oToolBar.ChangeButtonStatus('cmdScope','ENABLED')

*!*************************************************************
*! Name      : lfFillBin
*! Developer : Mariam Mazhar [MMT]
*! Date      : 11/19/2006
*! Purpose   : Function to colect bins.
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfFillBin()
*!*************************************************************
*!
FUNCTION lfFillBin
*!B609845,1 MMT 02/23/2012 Fix error in invnetory locking screen after click on scope [Start]
IF !loFormSet.llLoc
  RETURN 
ENDIF
*!B609845,1 MMT 02/23/2012 Fix error in invnetory locking screen after click on scope [End]
*E303079,1 MMT 03/11/2012 Fixing Media issues[T20120304.0004][Start]
*!B609845,2 MMT 03/15/2012 Fix error in invnetory locking screen after click on scope if use Bin location is YES[START]
*!*	IF TYPE('lcSty') <> 'C' OR TYPE('lcClr') <> 'C'
*!*	  RETURN 
*!*	ENDIF
*I=1
*!B609845,2 MMT 03/15/2012 Fix error in invnetory locking screen after click on scope if use Bin location is YES[END]
IF !USED('WhsLoc')
  =gfOpenTable(oAriaApplication.DataDir+'WhsLoc',oAriaApplication.DataDir+'WhsLoc','SH')
ENDIF
I=0
*E303079,1 MMT 03/11/2012 Fixing Media issues[T20120304.0004][END]

loFormSet.laMloc[1]  = ''
SELECT WhsLoc
gfSetOrder('Whslocst')
IF gfSeek(lcSty+lcClr+EVALUATE(loFormSet.lcBatLin+'.cWareCode'))
  SCAN REST WHILE Style+Color+cWareCode=;
    lcSty+lcClr+EVALUATE(loFormSet.lcBatLin+'.cWareCode')
    I=I+1
    DECLARE loFormSet.laMloc[I]
    loFormSet.laMloc[I]  = clocation
  ENDSCAN
ELSE
  DECLARE loFormSet.laMloc[1]
  loFormSet.laMloc = ''
  loFormSet.laMloc[1]  = 'All'
ENDIF

*!*************************************************************
*! Name      : lfVMOk
*! Developer : Mariam Mazhar [MMT]
*! Date      : 11/19/2006
*! Purpose   : Valid Function for Save button in mdWn screen buton.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : lfShowBtns
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfVMOk()
*!*************************************************************
*!
FUNCTION lfVMOk
PARAMETERS loMainForm,lnValue

SELECT (loMainForm.lcDetLin)
LOCATE 

lcKey = IIF(loMainForm.llmatmodul,'M','S')
SUM REST IIF(loMainForm.llMatModul,OnHand,TotStk) TO lnLNewTot

LOCATE 
SELECT(loMainForm.lcTmpQuery)

REPLACE STOCK WITH lnLNewTot;
        Cost  WITH lnValue
SELECT (loMainForm.lcBatLin)
GOTO TOP

*C123853,1 MHM 01/15/2005 delete all record for David luck [Start]
*!*  IF ASCAN(laEvntTrig,PADR("DLDELALL",10)) <> 0 
*!*    =gfDoTriger("ICINVLK",PADR("DLDELALL",10))
*!*  ELSE
*C123853,1 MHM 01/15/2005  [End]

lcBatchno = PADR(loMainForm.ariaform1.kbBatchNo.keytextbox.Value,6)
*T20060818.0001(C200876) TMI [Start] Delete all Records from the Temp File in the inventory Locking Screen when Saving using Bin Location.
IF ASCAN(loFormSet.laEvntTrig,PADR('DLDELALL',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0 ;
  .AND. gfDoTriger('POSTREC',PADR('ISUSEBIN',10))
  =loFormSet.mDoTrigger(PADR('DLDELALL',10)) 
ELSE
  *T20060818.0001(C200876) TMI [End  ] 
*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
*!*    DELETE ALL FOR cbattype+cLkBatch+style+color+DYELOT =;
*!*                   lcKey+lcBatchno +EVALUATE(loMainForm.lcDetLin+'.Style')+EVALUATE(loMainForm.lcDetLin+'.Color');
*!*                   AND IIF(loMainForm.llDyelot AND lfStyDye(EVALUATE(loMainForm.lcDetLin+'.Style'),EVALUATE(loMainForm.lcDetLin+'.Color')),!EMPTY(EVALUATE(loMainForm.lcDetLin+'.DYELOT')),.T.)
  DELETE ALL FOR cbattype+cLkBatch+style+DYELOT =;
                 lcKey+lcBatchno +EVALUATE(loMainForm.lcDetLin+'.Style') ;
                 AND IIF(loMainForm.llDyelot AND lfStyDye(EVALUATE(loMainForm.lcDetLin+'.Style')),;
                 !EMPTY(EVALUATE(loMainForm.lcDetLin+'.DYELOT')),.T.)
*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
  *T20060818.0001(C200876) TMI [Start] 
ENDIF
*T20060818.0001(C200876) TMI [End  ] 

*C123853,1 MHM 01/15/2005 [Start]
*!*  ENDIF               
*C123853,1 MHM 01/15/2005 [End]

*T20071102.0018,10/C200876 TMI 05/21/2008 [Start] get the data from the dbf() of the temp alias loMainForm.lcDetLin
*APPEND FROM (loMainForm.lcDetLin)
APPEND FROM (DBF(loMainForm.lcDetLin))
*T20071102.0018,10/C200876 TMI 05/21/2008 [End  ] 
SELECT (loMainForm.lcDetLin)
ZAP

IF loMainForm.ariaform1.txtStatus.Value     <> 'Hold'    
  loMainForm.ariaform1.txtStatus.Value     = 'Open'    
ENDIF


SELECT (loMainForm.lcBatHdr)
LOCATE 
loMainForm.lcStatus = IIF(loMainForm.lcStatus  = 'H','H','M')
REPLACE Type WITH  loMainForm.lcStatus 

loMainForm.ariaFORM1.grditems.refresh()

*!*************************************************************
*! Name      : lfVQty
*! Developer : Mariam Mazhar [MMT]
*! Date      : 11/19/2006
*! Purpose   : Valid Function for Quantity.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : lfShowBtns
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfVQty()
*!*************************************************************
*!
FUNCTION lfVQtyValue
PARAMETERS loFormMain,loChldForm
*B608844,1 TMI 04/11/2009 [Start] only update if there is a real change
IF _SCREEN.ActiveForm.ActiveControl.Value = _SCREEN.ActiveForm.ActiveControl.OldValue
  RETURN
ENDIF
loChldForm.AriaForm1.cmdOk.Enabled = .T.

*B608844,1 TMI 04/11/2009 [End  ] 

SELECT (loFormMain.lcDetLin)
*lnNewTot = IIF(loFormMain.llMatModul,OnHand,TOTSTK)
*mmt2
*IF !loFormMain.llMatModul
*mmt2
  FOR lnInd = 1 TO 8
    lcInd = STR(lnInd,1)
    REPLACE STK&lcInd WITH loChldForm.ariaForm1.cntNewStk.txtQty&lcInd..Value
  ENDFOR
*  lnNewTot = lnStk1+lnStk2+lnStk3+lnStk4+lnStk5+lnStk6+lnStk7+lnStk8
  REPLACE TOTSTK WITH loChldForm.ariaForm1.cntNewStk.txtTotQty.Value 
*mmt2 
*ELSE
IF loFormMain.llMatModul
*  REPLACE OnHand WITH lnNewTot
  REPLACE OnHand WITH loChldForm.ariaForm1.cntNewStk.txtTotQty.Value 
ENDIF   
*mmt2  
*ENDIF
*mmt2


IF (loFormMain.llDyelot AND llStyDye) OR (loFormMain.llLoc AND ALEN(loFormMain.laMLoc) > 1)
  loChldForm.ariaForm1.grdDetails.Refresh
ENDIF


*!*************************************************************
*! Name      : lfAddCntrlSrcDet
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 10/22/2006
*! Purpose   : Add Control Source for branch screens grids
*!*************************************************************
FUNCTION lfAddCntrlSrcDet
PARAMETERS loFormMain,loChldForm

lcStyPic  = loFormMain.lcstytitle
*gfItemMask("HI")

IF !loFormMain.llMatModul
  WITH loChldForm.ariaForm1.grdDetails
    IF loFormMain.llDyelot
      .columnCount = 5
      .RecordSource = ''
      .RecordSource = loFormMain.lcDetLin

      .column1.ControlSource   = 'Style'
      .column1.header1.Caption = lcStyPic  

      .column2.ControlSource   = 'Dyelot'
      .column2.header1.Caption = LANG_Dye_title
      
      .column3.ControlSource   = 'cLocation'
      .column3.header1.Caption = LANG_Bin_title
      
      .column4.ControlSource   = 'OldTotStk'
      .column4.header1.Caption = LANG_Old_onHand
      
      .column5.ControlSource   = 'TotStk'
      .column5.header1.Caption = LANG_New_onHand
      
    ELSE
      .columnCount = 4

      .RecordSource = ''
      .RecordSource = loFormMain.lcDetLin

      .column1.ControlSource   = 'Style'
      .column1.header1.Caption = lcStyPic  

      .column2.ControlSource   = 'cLocation'
      .column2.header1.Caption = LANG_Bin_title
      
      .column3.ControlSource   = 'OldTotStk'
      .column3.header1.Caption = LANG_Old_onHand
      
      .column4.ControlSource   = 'TotStk'
      .column4.header1.Caption = LANG_New_onHand

    ENDIF
  ENDWITH   
ELSE
  WITH loChldForm.ariaForm1.GRDDETAILS 
    IF loFormMain.llDyelot
      .columnCount = 6

      .RecordSource = ''
      .RecordSource = loFormMain.lcDetLin

      .column1.ControlSource   = 'Style'
      .column1.header1.Caption = LANG_Item_Title        

      .column2.ControlSource   = 'Color'
      .column2.header1.Caption = LANG_Clr_Title

      .column3.ControlSource   = 'Dyelot'
      .column3.header1.Caption = LANG_Dye_title        
      
      .column4.ControlSource   = 'cLocation'
      .column4.header1.Caption = LANG_Bin_title
      
      .column5.ControlSource   = 'OldQty'
      .column5.header1.Caption = LANG_Old_onHand
      
      .column6.ControlSource   = 'OnHand'
      .column6.header1.Caption = LANG_New_onHand
      
    ELSE
      .columnCount = 5

      .RecordSource = ''
      .RecordSource = loFormMain.lcDetLin

      .column1.ControlSource   = 'Style'
      .column1.header1.Caption = LANG_Item_Title

      .column2.ControlSource   = 'Color'
      .column2.header1.Caption = LANG_Clr_Title
      
      .column3.ControlSource   = 'cLocation'
      loChldForm.ariaForm1.GRDDETAILS.column3.header1.Caption = LANG_Bin_title
      
      .column4.ControlSource   = 'OldQty'
      .column4.header1.Caption = LANG_Old_onHand
      
      .column5.ControlSource   = 'OnHand'
      .column5.header1.Caption = LANG_New_onHand
    ENDIF
  ENDWITH   
ENDIF   

*!*************************************************************
*! Name      : lfVBin
*! Developer : Mariam Mazhar [MMT]
*! Date      : 11/19/2006
*! Purpose   : Valid function for bins popup.
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfVBin()
*!*************************************************************
*!
FUNCTION lfVBin
PARAMETERS loFormMain,loChldForm
*--- To fill the array laMloc
=lfFillBin()
IF loChldForm.ariaForm1.chKBBins.Value = .T.
  =lfAddBin()
ELSE
  =lfRemBin()
ENDIF
SELECT (loFormMain.lcDetLin)
LOCATE 
*!*************************************************************
*! Name      : lfAddBin
*! Developer : Mariam Mazhar [MMT]
*! Date      : 11/19/2006
*! Purpose   : Valid function for bins popup.
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfAddBin()
*!*************************************************************
*!
FUNCTION lfAddBin

*--- Bin array = laMloc
IF ALEN(loFormMain.laMloc) < 2
  RETURN
ENDIF
SELECT (loFormMain.lcDetLin)
LOCATE 
SCAN
  SCAT MEMVAR MEMO
  FOR lnInd1 = 2 TO ALEN(loFormMain.laMloc)
    m.cLocation = loFormMain.laMloc[lnInd1]
    IF lnInd1 > 2
      INSERT INTO (loFormMain.lcDetLin) FROM MEMVAR
      IF !loFormMain.llMatModul
        FOR lnInd2 = 1 TO 8
          lcInd2 = STR(lnInd2 ,1 )
          REPLACE Stk&lcInd2 WITH INT(Stk&lcInd2/(ALEN(loFormMain.laMloc)-1))
          REPLACE OldStk&lcInd2 WITH INT(OldStk&lcInd2/(ALEN(loFormMain.laMloc)-1))
        ENDFOR

        REPLACE TotStk WITH INT(TotStk/(ALEN(loFormMain.laMloc)-1))
        REPLACE OldTotStk WITH INT(OldTotStk/(ALEN(loFormMain.laMloc)-1))
      ELSE
        REPLACE OnHand WITH INT(OnHand/(ALEN(loFormMain.laMloc)-1))
        REPLACE OldQty WITH INT(OldQty/(ALEN(loFormMain.laMloc)-1))
      ENDIF
    ELSE
      GATHER MEMVAR MEMO
      IF !loFormMain.llMatModul
        FOR lnInd2 = 1 TO 8
          lcInd2 = STR(lnInd2 ,1 )
          REPLACE Stk&lcInd2 WITH INT((Stk&lcInd2/(ALEN(loFormMain.laMloc)-1)) )+ (IIF(Stk&lcInd2<0,-1,1)*MOD(ABS(Stk&lcInd2),(ALEN(loFormMain.laMloc)-1)))
          REPLACE OldStk&lcInd2 WITH INT((OldStk&lcInd2/(ALEN(loFormMain.laMloc)-1)) )+ (IIF(OldStk&lcInd2<0,-1,1)*MOD(ABS(OldStk&lcInd2),(ALEN(loFormMain.laMloc)-1)))
        ENDFOR
        REPLACE TotStk WITH INT((TotStk/(ALEN(loFormMain.laMloc)-1)) )+ (IIF(TotStk<0,-1,1)*MOD(ABS(TotStk),(ALEN(loFormMain.laMloc)-1)))
        REPLACE OldTotStk WITH INT((OldTotStk/(ALEN(loFormMain.laMloc)-1)) )+ (IIF(OldTotStk<0,-1,1)*MOD(ABS(OldTotStk),(ALEN(loFormMain.laMloc)-1)))
      ELSE
        REPLACE OnHand WITH INT((OnHand/(ALEN(loFormMain.laMloc)-1)) )+ (IIF(OnHand<0,-1,1)*MOD(ABS(OnHand),(ALEN(loFormMain.laMloc)-1)))
        REPLACE OldQty WITH INT((OldQty/(ALEN(loFormMain.laMloc)-1)) )+ (IIF(OldQty<0,-1,1)*MOD(ABS(OldQty),(ALEN(loFormMain.laMloc)-1)))
      ENDIF
    ENDIF
  ENDFOR
ENDSCAN

*!*************************************************************
*! Name      : lfRemBin
*! Developer : Mariam Mazhar [MMT]
*! Date      : 11/19/2006
*! Purpose   : Valid function for bins popup.
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfRemBin()
*!*************************************************************
*!
FUNCTION lfRemBin
*--- Bin array = laMloc

IF ALEN(loFormMain.laMloc) < 2
  RETURN
ENDIF
SELECT (loFormMain.lcDetLin)
LOCATE  
DO WHILE !EOF()
  lcS1 = Style
  lcC1 = Color
  lcD1 = Dyelot
  =SEEK(lcS1+lcC1+lcD1)
  SCAT MEMVAR MEMO
  IF !loFormMain.llMatModul
    FOR lnInd = 1 TO 8
      lcInd = STR(lnInd ,1 )
      m.Stk&lcInd = 0
      m.OldStk&lcInd = 0
    ENDFOR
    m.TotStk = 0
    m.OldTotStk = 0
  ELSE
    m.OnHand = 0
    m.OldQty = 0
  ENDIF
*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
*!*    SCAN REST WHILE Style+color+dyelot+clocation = ;
*!*                    lcS1+lcC1+lcD1 FOR !EMPTY(clocation)
  SCAN REST WHILE Style+dyelot+clocation = ;
                  lcS1+lcD1 FOR !EMPTY(clocation)
*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
    m.cLocation  = ''
    IF !loFormMain.llMatModul
      FOR lnInd = 1 TO 8
        lcInd = STR(lnInd ,1 )
        m.Stk&lcInd = m.Stk&lcInd+Stk&lcInd
        m.OldStk&lcInd = m.OldStk&lcInd+OldStk&lcInd
      ENDFOR
      m.TotStk = m.TotStk+TotStk
      m.OldTotStk = m.OldTotStk+OldTotStk
    ELSE
      m.OnHand = m.OnHand +OnHand 
      m.OldQty = m.OldQty +OldQty 
    ENDIF
  ENDSCAN
  lcS1 = Style
  lcC1 = Color
  lcD1 = Dyelot
  INSERT INTO (loFormMain.lcDetLin) FROM MEMVAR
  *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
  *=SEEK(lcS1+lcC1+lcD1)  
  =SEEK(lcS1+lcD1)
  *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
ENDDO
DELETE ALL FOR !EMPTY(cLocation)

*!*************************************************************
*! Name      : lfvNewCs
*! Developer : Mariam Mazhar [MMT]
*! Date      : 11/19/2006
*! Purpose   : Valid function for new Cost in detail screen.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Called from : 
*!         Procedures : ICINVLK.PRG
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvNewCs()
*!*************************************************************
*!
FUNCTION lfvNewCs
PARAMETERS loFormMain,lnValue


SELECT (loFormMain.lcDetLin)
REPLACE ALL COST WITH lnValue
LOCATE 
loFormMain.ariaFORM1.grditems.refresh

*!*************************************************************
*! Name      : lfVZeroStk
*! Developer : Mariam Mazhar [MMT]
*! Date      : 11/19/2006
*! Purpose   : zero inventory
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Called from : 
*!         Procedures : ICINVLK.PRG
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfVZeroStk()
*!*************************************************************
*!
FUNCTION lfVZeroStk
PARAMETERS loFormSet
PRIVATE lnOld

lnOld = SELECT(0)
SELECT (loFormSet.lcTmpQuery)
REPLACE ALL STOCK WITH 0
LOCATE 
SELECT(loFormSet.lcBatLin)
FOR lnInd = 1 TO 8
  lcStr = STR(lnInd,1)
  REPLACE ALL Stk&lcStr WITH 0
ENDFOR
IF loFormSet.llMatModul
  REPLACE ALL OnHand WITH 0
ELSE
  REPLACE ALL TotStk WITH 0
ENDIF
LOCATE 
SELECT (loFormSet.lcBatHdr)
REPLACE TYPE  WITH 'M'
loFormSet.lcstatus  = 'M'
loFormSet.ariaform1.txtStatus.Value     = 'Open'    
SELECT(lnOld)
*--- End Inventory Locking - (SSH)

*!*************************************************************
*! Name      : lfvAdj
*! Developer : Mariam Mazhar [MMT]
*! Date      : 11/19/2006
*! Purpose   : Valid function for Adj reason code in detail screen.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Called from : 
*!         Procedures : ICINVLK.PRG
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvAdj()
*!*************************************************************
*!
FUNCTION lfvAdj
PARAMETERS loFormSet,lcValue
SELECT (loFormSet.lcDetLin)
REPLACE cAdjReason WITH lcValue
*!*************************************************************
*! Name      : lfvRes
*! Developer : Mariam Mazhar [MMT]
*! Date      : 11/19/2006
*! Purpose   : Valid function for reason in detail screen.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Called from : 
*!         Procedures : ICINVLK.PRG
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvRes()
*!*************************************************************
*!
FUNCTION lfvRes
PARAMETERS loFormSet,lcValue
SELECT (loFormSet.lcDetLin)
REPLACE cReason WITH lcValue

*!*************************************************************
*! Name      : lfvNewOn
*! Developer : Mariam Mazhar [MMT]
*! Date      : 11/19/2006
*! Purpose   : Valid function for new on hand in material detail screen.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Called from : 
*!         Procedures : ICINVLK.PRG
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvNewOn()
*!*************************************************************
*!
FUNCTION lfvNewOn
PARAMETERS loFormSet,lcValue
SELECT (loFormSet.lcDetLin)
REPLACE OnHand WITH lcValue

*!*************************************************************
*! Name      : lfAfterRowColMat
*! Developer : Mariam Mazhar [MMT]
*! Date      : 11/19/2006
*! Purpose   : Valid function for new on hand in material detail screen.
*!*************************************************************
FUNCTION lfAfterRowColMat
PARAMETERS loFormSet,loChldformset
loChldformset.ariaForm1.txtreason.Value  = EVALUATE(loFormSet.lcDetLin+'.cReason')
*mmt2
*loChldformset.ariaForm1.txtnewOnhand.Value  = EVALUATE(loFormSet.lcDetLin+'.OnHand')
WITH loChldformset.ariaForm1.cntNewStk
  .txtQty1.Value = EVALUATE(loFormSet.lcDetLin+'.STK1')
  .txtQty2.Value = EVALUATE(loFormSet.lcDetLin+'.STK2')
  .txtQty3.Value = EVALUATE(loFormSet.lcDetLin+'.STK3')
  .txtQty4.Value = EVALUATE(loFormSet.lcDetLin+'.STK4')
  .txtQty5.Value = EVALUATE(loFormSet.lcDetLin+'.STK5')
  .txtQty6.Value = EVALUATE(loFormSet.lcDetLin+'.STK6')
  .txtQty7.Value = EVALUATE(loFormSet.lcDetLin+'.STK7')
  .txtQty8.Value = EVALUATE(loFormSet.lcDetLin+'.STK8')
  .txtTotQty.Value = EVALUATE(loFormSet.lcDetLin+'.TOTSTK')
ENDWITH 

WITH loChldformset.ariaForm1.cntoldStk
  .txtQty1.Value = EVALUATE(loFormSet.lcDetLin+'.OldStk1')
  .txtQty2.Value = EVALUATE(loFormSet.lcDetLin+'.OldStk2')
  .txtQty3.Value = EVALUATE(loFormSet.lcDetLin+'.OldStk3')
  .txtQty4.Value = EVALUATE(loFormSet.lcDetLin+'.OldStk4')
  .txtQty5.Value = EVALUATE(loFormSet.lcDetLin+'.OldStk5')
  .txtQty6.Value = EVALUATE(loFormSet.lcDetLin+'.OldStk6')
  .txtQty7.Value = EVALUATE(loFormSet.lcDetLin+'.OldStk7')
  .txtQty8.Value = EVALUATE(loFormSet.lcDetLin+'.OldStk8')
  .txtTotQty.Value = EVALUATE(loFormSet.lcDetLin+'.OldStk1')+;
                                             EVALUATE(loFormSet.lcDetLin+'.OldStk2')+;
                                             EVALUATE(loFormSet.lcDetLin+'.OldStk3')+;
                                             EVALUATE(loFormSet.lcDetLin+'.OldStk4')+;
                                             EVALUATE(loFormSet.lcDetLin+'.OldStk5')+;
                                             EVALUATE(loFormSet.lcDetLin+'.OldStk6')+;
                                             EVALUATE(loFormSet.lcDetLin+'.OldStk7')+;
                                             EVALUATE(loFormSet.lcDetLin+'.OldStk8')

ENDWITH                                              

*mmt2
loChldformset.ariaForm1.cboAdj.Value = EVALUATE(loFormSet.lcDetLin+'.cAdjReason')
*!*************************************************************
*! Name      : lfAfterRowColStyle
*! Developer : Mariam Mazhar [MMT]
*! Date      : 11/19/2006
*! Purpose   : Valid function for new on hand in material detail screen.
*!*************************************************************
FUNCTION lfAfterRowColStyle
PARAMETERS loFormSet,loChldformset

WITH loChldformset.ariaForm1.cntNewStk
  .txtQty1.Value = EVALUATE(loFormSet.lcDetLin+'.STK1')
  .txtQty2.Value = EVALUATE(loFormSet.lcDetLin+'.STK2')
  .txtQty3.Value = EVALUATE(loFormSet.lcDetLin+'.STK3')
  .txtQty4.Value = EVALUATE(loFormSet.lcDetLin+'.STK4')
  .txtQty5.Value = EVALUATE(loFormSet.lcDetLin+'.STK5')
  .txtQty6.Value = EVALUATE(loFormSet.lcDetLin+'.STK6')
  .txtQty7.Value = EVALUATE(loFormSet.lcDetLin+'.STK7')
  .txtQty8.Value = EVALUATE(loFormSet.lcDetLin+'.STK8')
  .txtTotQty.Value = EVALUATE(loFormSet.lcDetLin+'.TOTSTK')
ENDWITH 

WITH loChldformset.ariaForm1.cntoldStk
  .txtQty1.Value = EVALUATE(loFormSet.lcDetLin+'.OldStk1')
  .txtQty2.Value = EVALUATE(loFormSet.lcDetLin+'.OldStk2')
  .txtQty3.Value = EVALUATE(loFormSet.lcDetLin+'.OldStk3')
  .txtQty4.Value = EVALUATE(loFormSet.lcDetLin+'.OldStk4')
  .txtQty5.Value = EVALUATE(loFormSet.lcDetLin+'.OldStk5')
  .txtQty6.Value = EVALUATE(loFormSet.lcDetLin+'.OldStk6')
  .txtQty7.Value = EVALUATE(loFormSet.lcDetLin+'.OldStk7')
  .txtQty8.Value = EVALUATE(loFormSet.lcDetLin+'.OldStk8')
  .txtTotQty.Value = EVALUATE(loFormSet.lcDetLin+'.OldStk1')+;
                                             EVALUATE(loFormSet.lcDetLin+'.OldStk2')+;
                                             EVALUATE(loFormSet.lcDetLin+'.OldStk3')+;
                                             EVALUATE(loFormSet.lcDetLin+'.OldStk4')+;
                                             EVALUATE(loFormSet.lcDetLin+'.OldStk5')+;
                                             EVALUATE(loFormSet.lcDetLin+'.OldStk6')+;
                                             EVALUATE(loFormSet.lcDetLin+'.OldStk7')+;
                                             EVALUATE(loFormSet.lcDetLin+'.OldStk8')

ENDWITH                                              
loChldformset.ariaForm1.txtreason.Value  = EVALUATE(loFormSet.lcDetLin+'.cReason')
loChldformset.ariaForm1.cboAdjReason.Value = EVALUATE(loFormSet.lcDetLin+'.cAdjReason')

*!*************************************************************
*! Name      : lfConvertToCursor
*: Developer : MAriam Mazhar (MMT)
*! Date      : 11/19/2006
*! Purpose   : Convert a list of values into a cusrsor
*!*************************************************************
*!
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
CASE   ALLTRIM(lcFieldName) = 'COLOR'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6 
  laTempacstru[1,4]= 0

CASE   ALLTRIM(lcFieldName) = 'CSTYGRP'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6 
  laTempacstru[1,4]= 0
  
CASE   ALLTRIM(lcFieldName) = 'CSTY_TYPE'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6 
  laTempacstru[1,4]= 0
  
*                                           
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
*! Name      : lfChekBin
*! Developer : Mariam Mazhar [MMT]
*! Date      : 11/19/2006
*! Purpose   : Functio to Check bin.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Called from : 
*!         Procedures : ICINVLK.PRG
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfChekBin()
*!*************************************************************
*!
FUNCTION lfChekBin

PRIVATE llToRet

llToRet = .T. 
IF loFormset.llLoc AND  IIF(llLogic,.T.,!EMPTY(laRpTarget))
  lnOldAls = SELECT(0)
  SELECT WhsLoc
  gfSetOrder('WhsLocSt')
*  SET ORDER TO WhsLocSt
  IF  loFormset.llWareHus
    =IIF(loFormset.llMatModul,gfSEEK(PADR(m.ITEM,19)+m.Color+PADR(loFormset.ariaform1.kblocation.keytextbox.Value,6)),gfSEEK(m.Style+m.Color+PADR(loFormset.ariaform1.kblocation.keytextbox.Value,6)))
  ELSE
    =IIF(loFormset.llMatModul,gfSEEK(PADR(m.ITEM,19)+m.Color),gfSEEK(m.Style+m.Color))
  ENDIF
  llToRet = .F.
  SCAN REST WHILE style+color+cwarecode+clocation = ;
                      IIF(loFormset.llMatModul,PADR(m.ITEM,19),m.Style)+m.Color+IIF(loFormset.llWareHus,PADR(loFormset.ariaform1.kblocation.keytextbox.Value,6),'');
            FOR IIF(llLogic,.T.,ASCAN(laRpTarget,ALLTRIM(clocation)) <> 0)
    llToRet = .T.
    EXIT
  ENDSCAN
  gfSetOrder('WhsLoc')
  SELECT (lnOldAls)
ENDIF

RETURN(llToRet)

*!*************************************************************
*! Name      : lfAddLTemp
*! Developer : Mariam Mazhar [MMT]
*! Date      : 11/19/2006
*! Purpose   : Add lie to temp file.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : PARAMETER lcItem,lcColor
*!*************************************************************
*! Called from : 
*!         Procedures : ICINVLK.PRG
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfAddLTemp()
*!*************************************************************
*!
FUNCTION lfAddLTemp
*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
*PARAMETER lcItem,lcColor
PARAMETER lcItem
*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
  
PRIVATE llToRet , lcOldAls , lcOldInd

lcOldAls = SELECT(0)

*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
*IF SEEK(lcItem+lcColor,loFormSet.lcTmpQuery)
IF SEEK(lcItem,loFormSet.lcTmpQuery)
*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
  llToRet = .F.
ENDIF
*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
*!*  IF  !SEEK(lcItem+lcColor,loFormSet.lcTmpQuery) AND lfCheckSty(lcItem,lcColor)
IF  !SEEK(lcItem,loFormSet.lcTmpQuery) AND lfCheckSty(lcItem)
*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
    INSERT INTO (loFormSet.lcTmpQuery) FROM MEMVAR
    SELECT(loFormSet.lcTmpQuery)
    REPLACE nStkVal WITH m.nStkVal
ENDIF
SELECT(lcOldAls)

*!*************************************************************
*! Name      : lfCheckSty
*! Developer : Mariam Mazhar [MMT]
*! Date      : 11/19/2006
*! Purpose   : Check if style locked before.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : PARAMETER lcItem,lcColor
*!*************************************************************
*! Called from : 
*!         Procedures : ICINVLK.PRG
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfCheckSty()
*!*************************************************************
*!
FUNCTION lfCheckSty
*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
*PARAMETER lcCSty,lcCClr
PARAMETER lcCSty
*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
PRIVATE lcOldAls , lcOldInd
lcKey = IIF(loFormSet.llmatmodul,'M','S')
lcOldAls = SELECT(0)
SELECT MDINVNTL
gfSetOrder('MDINVNTLS')
*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
*!*  =gfSEEK(lcCSty+lcCClr)
*!*  SCAN REST WHILE style+color+cbattype+cLkBatch = ;
*!*                  lcCSty+lcCClr ;
*!*                  FOR IIF(loFormset.llWareHus,cWareCode = PADR(loFormset.ariaform1.kblocation.keytextbox.Value,6),.T.)
=gfSEEK(lcCSty)
SCAN REST WHILE style+cbattype+cLkBatch =lcCSty;
                FOR IIF(loFormset.llWareHus,cWareCode = PADR(loFormset.ariaform1.kblocation.keytextbox.Value,6),.T.)
*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
  IF gfSEEK(lcKey+MDINVNTL.cLkBatch,'MDINVNTH') AND MDINVNTH.Type $ 'HML'
    RETURN(.F.)
  ENDIF
ENDSCAN
SELECT (lcOldAls)

*!*************************************************************
*! Name      : lfPrnRep.PRG  (E# 301851 dueto C#101624)
*! Developer : Mariam Mazhar [MMT]
*! Date      : 11/19/2006
*! Purpose   : Function To Print the rejected styles.
*!*************************************************************
*! Called from : ININVLK.PRG
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Example   : =lfPrnRep()
*!*************************************************************
FUNCTION lfPrnRep

LOCAL lnDataSess, lcDevice, lcClassDir, oOptionGrid

lcRpForm = 'ICREJST'

DO CASE
  CASE  lnanswer =  1
    lcDevice   = oAriaApplication.gcDevice
    oAriaApplication.gcDevice = "SCREEN"
    SET DEVICE TO SCREEN
  CASE  lnanswer =  2
    lcDevice   = oAriaApplication.gcDevice
    oAriaApplication.gcDevice = 'PRINTER'
  CASE  lnanswer =  3
    llCancel = .T. 
    SET DEVICE TO SCREEN
    RETURN
ENDCASE
lcDefa = FULLPATH('')

PRIVATE loOGScroll

*-- Create a Dummy Object of the Option Grid...
lnDataSess = SET("Datasession")
lcClassDir   = ADDBS(oAriaApplication.ClassDir)
oOptionGrid  = NEWOBJECT("optiongrid",lcClassDir+"optiongrid.vcx")
loOGScroll   = oOptionGrid.OptionGrid.oHost
lcOGPlatForm = ''
loOgScroll.lcOGPlatForm  = ''
loOgScroll.lcOGLastForm  = 'ICREJST'
loOGScroll.llPrintPDF = .F.
LoOGScroll.llCrystal = .T.
loOGScroll.cCROrientation='P'
loOGScroll.lUsePDFViewer  = .F.
loogscroll.lcogwintitl = 'Inventory Locking'

DIMENSION loOGScroll.laSelFile[1,3]
loOGScroll.laSelFile = ''

SELECT (loFormSet.lcRejSty)
LOCATE 
USE IN (loFormSet.lcRejSty)

DIMENSION loOgScroll.laCRTables[1]
DIMENSION loOgScroll.laCRParams[2,2]

loOgScroll.laCRTables[1] = oAriaApplication.WorkDir +  loFormSet.lcRejSty + ".DBF"
  
loOgScroll.laCRParams[1,1] = 'ReportName'
loOgScroll.laCRParams[1,2]= 'A Status Report'

loOgScroll.laCRParams[2,1] = 'lcStyPic'
loOgScroll.laCRParams[2,2]= loFormSet.lcstytitle
*gfItemMask("HI")


=gfDispRe('ICREJST')

SET DATASESSION TO (lnDataSess)

oOptionGrid = .NULL.
LoOGScroll = null

SET DEFAULT TO (lcDefa)
SET DEVICE TO SCREEN
RETURN 
*!*************************************************************
*! Name      : lfTmpBtchL
*! Developer : Mariam Mazhar [MMT]
*! Date      : 11/19/2006
*! Purpose   : Function to Update mdwn line and hewder.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Called from : 
*!         Procedures : ICINVLK.PRG
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfTmpBtchL()
*!*************************************************************
*!
FUNCTION lfTmpBtchL

lcKey = IIF(loFormSet.llmatmodul,'M','S')
m.Date      = loFormset.ariaform1.dtpLock.Value
m.cWareCode = PADR(loFormset.ariaform1.kblocation.keytextbox.Value,6)
m.Type      = loFormSet.lcStatus
m.cBatType  = lcKey
m.content   = loFormset.ariaform1.txtNotes.value

m.cLkBatch    = PADR(loFormset.ariaform1.kbBatchNo.keytextbox.Value,6)

INSERT INTO (loFormset.lcBatHdr) FROM MEMVAR

*!*  IF ASCAN(laEvntTrig,PADR("DLTMPBAT",10)) <> 0 
*!*    =gfDoTriger("ICINVLK",PADR("DLTMPBAT",10))
*!*  ELSE
  SELECT(loFormset.lcTmpQuery)
  LOCATE 
  SCAN
    SCATTER MEMVAR MEMO
    SELECT IIF(loFormset.llMatModul,'ITEMLOC','STYDYE')
    gfREPLACE([dLlokDate WITH loFormset.ariaform1.dtpLock.Value])
    m.Style     = IIF(loFormset.llMatModul,m.ITEM,m.Style)
    m.Creason   = IIF(loFormset.llMatModul,'MATERIAL LOCK INVENTORY     ' ,;
                                 'STYLE LOCK INVENTORY     ')
    m.cWareCode = PADR(loFormset.ariaform1.kblocation.keytextbox.Value,6)
    
    IF loFormset.llMatModul
    *--- Parameters : 1-Item ,2-Color ,3-MultiMare
     *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
      *=lfAddLin(SUBSTR(m.ITEM,1,7),m.Color)
      =lfAddLin(m.ITEM)
     *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
    ELSE
     *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
     * =lfAddLin(m.style,SPACE(06))     
      =lfAddLin(m.style)
     *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
    ENDIF
  ENDSCAN
*!*  ENDIF

*!*************************************************************
*! Name      : lfAddLin
*! Developer : Mariam Mazhar [MMT]
*! Date      : 11/19/2006
*! Purpose   : Function to add line to mdinvntkl.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : PARAMETER lcItem,lcColor
*!*************************************************************
*! Called from : 
*!         Procedures : ICINVLK.PRG
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfAddLin()
*!*************************************************************
*!
FUNCTION lfAddLin
*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
*PARAMETER lcItem,lcColr
PARAMETER lcItem
*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
PRIVATE lnAlias


lnAlias = SELECT(0)

SELECT IIF(loFormSet.llMatModul,'ITEMLOC','STYDYE')

IF loFormSet.llMatModul
  lnMajLength =loFormset.lnmajlength
  *LEN(gfItemMask('PM','', "0002"))
  lenClrlen = loFormSet.lenclrlen
  *LEN(gfitemmask("PN", "", "0002"))
  *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
  *= gfSqlRun("SELECT * FROM Itemloc(INDEX = Stydye) WHERE cinvtype = '0002' AND  style  LIKE '"+;
                  SUBSTR(lcItem,1,lnMajLength)+"%'"+;
                  IIF(loFormset.llWareHus," AND cwarecode='"+PADR(loFormset.ariaform1.kblocation.keytextbox.Value,6)+"'","")+;
                  IIF(EMPTY(lcColr),'',"And Right(style,"+ALLTRIM(STR(lenClrlen))+") like '"+lcColr+"'") ,'ITEMLOC')
  = gfSqlRun("SELECT * FROM Itemloc(INDEX = Stydye) WHERE cinvtype = '0002' AND  style  LIKE '"+lcItem+"%'"+;
                  IIF(loFormset.llWareHus," AND cwarecode='"+STRTRAN(PADR(loFormset.ariaform1.kblocation.keytextbox.Value,6),"'","''")+"'",""),'ITEMLOC')
  *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]

ELSE
*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
*!*    =gfSEEK(lcItem+IIF(EMPTY(lcColr),'',lcColr)+;
*!*               IIF(loFormset.llWareHus,PADR(loFormset.ariaform1.kblocation.keytextbox.Value,6),''))
  =gfSEEK(PADR(lcItem,19)+;
             IIF(loFormset.llWareHus,PADR(loFormset.ariaform1.kblocation.keytextbox.Value,6),''))
*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
ENDIF 

DO CASE
  CASE  lcCostMeth = 'S'  && Standard
    m.Cost    = IIF(loFormset.llMatModul,IIF(gfSeek('0002'+Itemloc.Style,'Item','Style') and  gfSeek(Item.cConvBuy,'UOM'),Item.TOTCOST/ UOM.nconf,0),STYLE.TotCost)
    m.OldCost = IIF(loFormset.llMatModul,IIF(gfSeek('0002'+Itemloc.Style,'Item','Style') and  gfSeek(Item.cConvBuy,'UOM'),Item.TOTCOST/ UOM.nconf,0),STYLE.TotCost)
  OTHERWISE       && 'A' Average
    m.Cost    = IIF(loFormset.llMAtModul,ITEMLOC.Ave_Cost,STYDYE.Ave_Cost)
    m.OldCost = IIF(loFormset.llMAtModul,ITEMLOC.Ave_Cost,STYDYE.Ave_Cost)
ENDCASE


SCAN REST WHILE IIF(loFormset.llMatModul,cinvtype+ style+ cwarecode+ dyelot= "0002",;
                               style+cwarecode+dyelot = m.Style+IIF(loFormset.llWareHus,PADR(loFormset.ariaform1.kblocation.keytextbox.Value,6),''))

  IF !loFormset.llMatModul
    FOR lnInd = 1 TO 8
      Index = STR(lnInd,1)
      m.OldStk&Index = STYDYE.Stk&Index
      IF !llLogic
        m.Stk&Index    = STYDYE.Stk&Index
      ENDIF

    ENDFOR
  *mmt2  
  ELSE
    FOR lnInd = 1 TO 8
      Index = STR(lnInd,1)
      m.OldStk&Index = Itemloc.Stk&Index
      IF !llLogic
        m.Stk&Index    = Itemloc.Stk&Index
      ENDIF
    ENDFOR
  *MMT2    
  ENDIF
 
IF !llLogic
    IF loFormset.llMatModul
      m.OldQty   = Itemloc.TOTSTK
      m.OnHand   = Itemloc.TOTSTK
      m.Dyelot   = Itemloc.Dyelot
      *mmt2
*      m.Scale    = 'F'
      m.Scale    = item.Scale
      *mmt
    ELSE
      =gfSeek(m.Style,'Style')
      m.OldTotStk = STYDYE.TotStk
      m.TotStk    = STYDYE.TotStk
      m.Dyelot    = STYDYE.Dyelot
      m.Scale     = Style.Scale
    ENDIF

    *T20060818.0001(C200876) TMI [Start] Get temp batch per bin location
    IF ASCAN(loFormSet.laEvntTrig,PADR('DLTMPBAT',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0 ;
      .AND. loFormSet.mDoTrigger(PADR('DLTMPBAT',10)) 
      ** - I already have called the trigger in just the previous line
    ELSE
      *T20060818.0001(C200876) TMI [End  ] 
    
      INSERT INTO (loFormset.lcBatLin) FROM MEMVAR
    
      *T20071102.0018,10/C200876 TMI 05/21/2008 [Start] 
    ENDIF
    *T20071102.0018,10/C200876 TMI 05/21/2008 [End  ] 
  ENDIF
ENDSCAN
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfvPathSt.PRG  (E# 301851 dueto C#101624)
*! Developer : Mariam Mazhar [MMT]
*! Date      : 11/19/2006
*! Purpose   : Valid Function For Path Button.
*!*************************************************************
*! Called from : ININVLK2.SPX
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Example   : =lfvPathSt()
*!*************************************************************
FUNCTION lfvPathSt
PARAMETERS loFormSet


lcMultWare = PADR(loFormset.ariaform1.kblocation.keytextbox.Value,6)
DIMENSION laSFile[1,1]
STORE ' ' TO laSFile
*!E303058,1 MMT 02/05/2012 Allow user to import excel file after creating batch in locking screen[T20120111.0036][Start]
*DIMENSION laFileStru[12, 4]
DIMENSION laFileStru[11, 4]
*!E303058,1 MMT 02/05/2012 Allow user to import excel file after creating batch in locking screen[T20120111.0036][END]
 laFileStru[1,1] = 'STYLE'
 laFileStru[1,2] = 'C'
*!B608564,1 MOS 05/20/2008 FIX the problem of importing from extended style excel sheet[start] 
*laFileStru[1,3] = 12
 laFileStru[1,3] = 19
*!B608564,1 MOS 05/20/2008 FIX the problem of importing from extended style excel sheet[end] 
 laFileStru[1,4] = 0
*!E303058,1 MMT 02/05/2012 Allow user to import excel file after creating batch in locking screen[T20120111.0036][Start]
*!*	 laFileStru[2,1] = 'Color'
*!*	 laFileStru[2,2] = 'C'
*!*	 laFileStru[2,3] = 6
*!*	 laFileStru[2,4] = 0

*!*	 laFileStru[3,1] = 'Desc'
*!*	 laFileStru[3,2] = 'C'
*!*	 laFileStru[3,3] = 20
*!*	 laFileStru[3,4] = 0
*!*	 

*!*	 laFileStru[4,1] = 'Stk1'
*!*	 laFileStru[4,2] = 'N'
*!*	 laFileStru[4,3] = 10
*!*	 laFileStru[4,4] = 0


*!*	 laFileStru[5,1] = 'Stk2'
*!*	 laFileStru[5,2] = 'N'
*!*	 laFileStru[5,3] = 10
*!*	 laFileStru[5,4] = 0


*!*	 laFileStru[6,1] = 'Stk3'
*!*	 laFileStru[6,2] = 'N'
*!*	 laFileStru[6,3] = 10
*!*	 laFileStru[6,4] = 0


*!*	 laFileStru[7,1] = 'Stk4'
*!*	 laFileStru[7,2] = 'N'
*!*	 laFileStru[7,3] = 10
*!*	 laFileStru[7,4] = 0

*!*	 laFileStru[8,1] = 'Stk5'
*!*	 laFileStru[8,2] = 'N'
*!*	 laFileStru[8,3] = 10
*!*	 laFileStru[8,4] = 0


*!*	 laFileStru[9,1] = 'Stk6'
*!*	 laFileStru[9,2] = 'N'
*!*	 laFileStru[9,3] = 10
*!*	 laFileStru[9,4] = 0

*!*	 laFileStru[10,1] = 'Stk7'
*!*	 laFileStru[10,2] = 'N'
*!*	 laFileStru[10,3] = 10
*!*	 laFileStru[10,4] = 0

*!*	 laFileStru[11,1] = 'Stk8'
*!*	 laFileStru[11,2] = 'N'
*!*	 laFileStru[11,3] = 10
*!*	 laFileStru[11,4] = 0

*!*	 laFileStru[12,1] = 'totStk'
*!*	 laFileStru[12,2] = 'N'
*!*	 laFileStru[12,3] = 7
*!*	 laFileStru[12,4] = 0
 laFileStru[2,1] = 'Desc'
 laFileStru[2,2] = 'C'
 laFileStru[2,3] = 20
 laFileStru[2,4] = 0
 

 laFileStru[3,1] = 'Stk1'
 laFileStru[3,2] = 'N'
 laFileStru[3,3] = 10
 laFileStru[3,4] = 0


 laFileStru[4,1] = 'Stk2'
 laFileStru[4,2] = 'N'
 laFileStru[4,3] = 10
 laFileStru[4,4] = 0


 laFileStru[5,1] = 'Stk3'
 laFileStru[5,2] = 'N'
 laFileStru[5,3] = 10
 laFileStru[5,4] = 0

 laFileStru[6,1] = 'Stk4'
 laFileStru[6,2] = 'N'
 laFileStru[6,3] = 10
 laFileStru[6,4] = 0

 laFileStru[7,1] = 'Stk5'
 laFileStru[7,2] = 'N'
 laFileStru[7,3] = 10
 laFileStru[7,4] = 0


 laFileStru[8,1] = 'Stk6'
 laFileStru[8,2] = 'N'
 laFileStru[8,3] = 10
 laFileStru[8,4] = 0

 laFileStru[9,1] = 'Stk7'
 laFileStru[9,2] = 'N'
 laFileStru[9,3] = 10
 laFileStru[9,4] = 0

 laFileStru[10,1] = 'Stk8'
 laFileStru[10,2] = 'N'
 laFileStru[10,3] = 10
 laFileStru[10,4] = 0

 laFileStru[11,1] = 'totStk'
 laFileStru[11,2] = 'N'
 laFileStru[11,3] = 7
 laFileStru[11,4] = 0
*!E303058,1 MMT 02/05/2012 Allow user to import excel file after creating batch in locking screen[T20120111.0036][END]
=gfCrtTmp(loFormSet.lcInport,@laFileStru,.F.)

DIMENSION laFileStru[4, 4]
laFileStru[1,1] = 'WARECODE'
laFileStru[1,2] = 'C'
laFileStru[1,3] = 6
laFileStru[1,4] = 0

laFileStru[2,1] = 'STYLE'
laFileStru[2,2] = 'C'
laFileStru[2,3] = 19
laFileStru[2,4] = 0

laFileStru[3,1] = 'REASON'
laFileStru[3,2] = 'C'
laFileStru[3,3] = 60
laFileStru[3,4] = 0

laFileStru[4,1] = 'cdesc'
laFileStru[4,2] = 'C'
laFileStru[4,3] = 35
laFileStru[4,4] = 0

=gfCrtTmp(loFormSet.lcRejSty,@laFileStru,'WARECODE + STYLE',loFormSet.lcRejSty,.F.)

lcOldPath  = FULLPATH("")
lcPathName = GETFILE('CSV', LANG_Imp_Brow, LANG_Import,1) 
lcFilNamMu = SUBSTR(lcPathName , RAT( "\" , lcPathName) + 1)
lcPathName = LEFT(lcPathName , RAT( "\" , lcPathName) - 1 )

IF EMPTY(lcPathName)
  STORE SPACE(40) TO lcPathName
  SET DEFAULT TO (lcOldPath)
  RETURN
ENDIF
SET DEFAULT TO (lcPathName)

lcDot = ATC("." , lcFilNamMu , 1)
IF SUBSTR(lcFilNamMu , lcDot + 1 , 3 ) # "CSV"
  SET DEFAULT TO (lcOldPath)
  *--- "This file cannot be selected. You must select a file of type CSV"
  =gfModalGen('TRM00000B00000','DIALOG',.F.,.F.,LANG_Imprt_Error)
  lcPathName = SPACE(40)
  RETURN
ENDIF
SET DEFAULT TO (lcOldPath)


SELECT (loFormSet.lcInport)
ZAP
APPEND FROM (ALLT(lcpathname)+IIF(RIGHT(lcPathName,1) <> "\","\","")+;
             lcFilNamMu) DELIMITED
GO  2
ldDate = CTOD(STYLE)
SKIP
loFormSet.lcWare = PADR(ALLT(STYLE),6)
loFormset.ariaform1.kblocation.keytextbox.Value = loFormSet.lcWare
loFormset.ariaform1.dtpLock.Value = ldDate
llwarhosM = .T.
loFormset.ariaform1.dtpLock.enabled = .T.
loFormset.ariaform1.kblocation.keytextbox.enabled = .T.
*!B609335,1 MMT 07/06/2010 Validate Style Sizes Cnt and locking date in Excel file[Start]
IF EMPTY(ldDate)
  loFormset.ariaform1.kblocation.keytextbox.Value = IIF(llwarHosM,SPACE(6),lcMultWare)
  loFormset.ariaform1.dtpLock.Value  = Oariaapplication.systemdate
  loFormset.ariaform1.dtpLock.enabled = .T.
  loFormset.ariaform1.kblocation.keytextbox.enabled = .T.
  = gfModalGen("TRM42278B00000","DIALOG")
  RETURN 
ENDIF 
*!B609335,1 MMT 07/06/2010 Validate Style Sizes Cnt and locking date in Excel file[End]

IF gfSEEK(PADR(loFormset.ariaform1.kblocation.keytextbox.Value,6) , 'WareHous')
  =lfSelData(.T.)
ELSE
   *---Text : 'No Record Selected for the report..!'
  loFormset.ariaform1.kblocation.keytextbox.Value = IIF(llwarHosM,SPACE(6),lcMultWare)
  loFormset.ariaform1.dtpLock.Value  = Oariaapplication.systemdate
  loFormset.ariaform1.dtpLock.enabled = .T.
  loFormset.ariaform1.kblocation.keytextbox.enabled = .T.
  =gfModalGen('TRM00052B00000','DIALOG')  
  RETURN
ENDIF

IF llwarHosM AND RECCOUNT(loFormset.lctmpquery) = 0
  loFormset.ariaform1.dtpLock.Value   =  Oariaapplication.systemdate
  loFormset.ariaform1.dtpLock.enabled = .T.
  IF loFormSet.llWareHus
    loFormset.ariaform1.kblocation.keytextbox.Value = SPACE(6)
    loFormset.ariaform1.kblocation.keytextbox.enabled = .T. 
  ENDIF
ENDIF

*--End of lfvPathSt.
*!*************************************************************
*! Name      : lfvPost
*! Developer : Mariam Mazhar [MMT]
*! Date      : 11/19/2006
*! Purpose   : Valid function for post batch .
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvPost()
*!*************************************************************
*!
FUNCTION lfvPost
PARAMETERS loFormSet
STORE 0 TO lnCountLn , lnRest 


lcGLFYear  = SPACE(04)      && Fiscal Year
lcGLPeriod = SPACE(02)      && Period

STORE '' TO lcGlSess
IF loFormSet.lcStatus = 'H'
  =gfModalGen('TRM42209B00000','DIALOG')
  RETURN
ENDIF

IF loFormset.ariaform1.dtpPost.Value  = {} OR loFormset.ariaform1.dtpLock.Value  > loFormset.ariaform1.dtpPost.Value 
  =gfModalGen('TRM42192B00000','DIALOG')
  loFormset.ariaform1.dtpPost.enabled = .T.
  RETURN
ENDIF
*---Conferm missage 'You will not be able to modify this batch after posting .Post now?'
IF gfModalGen('QRM42206B00006','DIALOG') = 2
  RETURN
ENDIF

*---Warning message 'This process affects all transaction files. It is HIGHLY RECOMMENDED to create backup before posting . Would you like to backup?'
IF gfModalGen('QRM42211B00006','DIALOG') = 1
  *--- Function to BackUp files.
  *B610699,1 TMI 03/17/2014 17:21 [Start] if not backuped then don't proceed
  *=lfBackUp()
  IF !lfBackUp()
    RETURN .F.
  ENDIF 
  *B610699,1 TMI 03/17/2014 17:21 [End  ] 
ENDIF

*---VIP
IF !CHECKPRD(loFormset.ariaform1.dtpLock.Value,'lcGLFYear','lcGLPeriod','IA')
  RETURN
ENDIF

IF loFormset.llLinkGL AND !USED(loFormset.lctmpGlDis)
  SELECT GLDIST
  *-- OPEN A TEMP FILE TO BE USED IN CALLING 'GLDIST' PROCEDURE.
  COPY STRUCTURE TO (oAriaApplication.WorkDir+loFormset.lctmpGlDis)
  SELECT 0
  USE (oAriaApplication.WorkDir+loFormset.lctmpGlDis) EXCLUSIVE
ENDIF

WAIT WINDOW  LANG_Update_Style_real NOWAIT
SELECT (loFormset.lcBatHdr)
REPLACE TYPE      WITH 'P';
        dPostDate WITH loFormset.ariaform1.dtpPost.Value 

SELECT MDINVNTH
lcKey = IIF(loFormSet.llmatmodul,'M','S')
=gfSEEK(lcKey+PADR(loFormset.ariaform1.kbBatchNo.keytextbox.Value,6))
gfREPLACE([TYPE      WITH 'P',]+;
        [dPostDate WITH loFormset.ariaform1.dtpPost.Value ])

ldlock = date
loFormset.ariaform1.dtpPost.Value  = dPostDate
loFormSet.lcStatus = 'P'
loFormset.ariaform1.txtStatus.Value     = 'Posted'
loFormset.AriaForm1.txtStatus.Ariacolor = "R" 

SELECT MDINVNTL
lcOldOrd = ORDER()
gfSetorder('MDINVNTL')
lcPType  = IIF(loFormset.llMatModul,'M','S')
IF !EMPTY(loFormset.lcPostScVar)
  =gfSEEK(loFormset.lcPostScVar)
ELSE
  =gfSEEK(lcPType+MDINVNTH.cLkBatch)
ENDIF
lcPBatch = MDINVNTH.cLkBatch

lcLinkCode = 'DEFDEF'
*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
loFormSet.oProgress.TotalProgress = loFormSet.lnTotRec
loFormSet.oProgress.lblFirstLabel.Caption = LANG_Posting_Title
loFormSet.oProgress.Show()
*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]


*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
*!*  DO WHILE !EOF() AND cbattype+cLkBatch+style+color+dyelot+clocation;
                    = lcPType+MDINVNTH.cLkBatch
  *loFormset.lcPostScVar = cbattype+cLkBatch+style+color  
DO WHILE !EOF() AND cbattype+cLkBatch+style+dyelot+clocation;
                    = lcPType+MDINVNTH.cLkBatch
  loFormset.lcPostScVar = cbattype+cLkBatch+style
*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
*  = gfSavSess(lcUnCmPgId, lcFiles, @laVariables,lcSession) 
  lcPStyle = Style
  lcPColor = Color
  llAdjYNo = .F.
  
  lenClrlen = loFormset.lenclrlen
  *LEN(gfitemmask("PN", "", "0002"))
  IF  loFormset.llMatModul

*    lnMajLength =LEN(gfItemMask('PM','', "0002"))
*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
*    = gfSqlRun("SELECT * FROM Item(INDEX = cStyle) WHERE cinvtype = '0002' AND  style  LIKE '"+;
                  PADR(lcPStyle,7)+"%'"+;
                  IIF(EMPTY(lcPColor),''," And Right(style,"+ALLTRIM(STR(lenClrlen))+") like '"+lcPColor+"'") ,'ITEM')
 *     = gfSqlRun("SELECT * FROM Itemloc(INDEX = Stydye) WHERE cinvtype = '0002' AND  style  LIKE '"+;
                  PADR(lcPStyle,7)+"%'"+;
                  IIF(loFormset.llWareHus," AND cwarecode='"+PADR(loFormset.ariaform1.kblocation.keytextbox.Value,6)+"'","")+;
                  IIF(EMPTY(lcPColor),'',"And Right(style,"+ALLTRIM(STR(lenClrlen))+") like '"+lcPColor+"'") ,'ITEMLOC')                

    = gfSqlRun("SELECT * FROM Item(INDEX = cStyle) WHERE cinvtype = '0002' AND  style  LIKE '"+;
                  lcPStyle+"%'",'ITEM')
    = gfSqlRun("SELECT * FROM Itemloc(INDEX = Stydye) WHERE cinvtype = '0002' AND  style  LIKE '"+;
                  lcPStyle+"%'"+;
                  IIF(loFormset.llWareHus," AND cwarecode='"+STRTRAN(PADR(loFormset.ariaform1.kblocation.keytextbox.Value,6),"'","''")+"'",""),'ITEMLOC')
*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]

  ELSE
   =gfSEEK(lcPStyle,'STYLE')
   =gfSeek(lcPStyle+IIF(loFormset.llWareHus,PADR(loFormset.ariaform1.kblocation.keytextbox.Value,6),''),'STYDYE')
  ENDIF 
 
 
  *--- Get the link Code.
  lcLinkCode = IIF(loFormset.llWareHus,IIF(loFormset.llMatModul,ITEMLOC.GL_LINK,StyDye.GL_LINK),;
                             IIF(loFormset.llMatModul,ITEM.Link_Code,Style.Link_Code))
  DECLARE laAdjust[10]
  DECLARE laLocInf[11]
  DIMENSION laOldStk[12]
  laOldStk[12] = 0
  STORE 0 TO laAdjust,laOldStk,laLocInf
  DIME laGLDistAr[1,1]
  laGLDistAr = ''
  lcAdjAcct  = ''
 *B608453,1 AKA 25/2/2008 Fix numeric overflow error while posting [start]
  *lnTempCst  = IIF(EMPTY(MDINVNTL.Dyelot),MDINVNTL.nStkVal/IIF(loFormset.llMatModul,MDINVNTL.OldQty,MDINVNTL.OldTotStk),lnTempCst)    
  lnTempCst= 0 
  IF EMPTY(MDINVNTL.Dyelot)
    IF loFormset.llMatModul
	  lnTempCst= IIF(MDINVNTL.OldQty<>0   , MDINVNTL.nStkVal/MDINVNTL.OldQty,0)
    ELSE
      lnTempCst= IIF(MDINVNTL.OldTotStk<>0, MDINVNTL.nStkVal/MDINVNTL.OldTotStk,0)
    ENDIF    
  ENDIF 
  *B608453,1 AKA 25/2/2008 Fix numeric overflow error while posting [End]
 
  IF loFormset.llMatModul
    **!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
    *= gfSqlRun("SELECT * FROM Item(INDEX = cStyle) WHERE cinvtype = '0002' AND  style  LIKE '"+;
                  SUBSTR(lcPStyle,1,7)+"%'"+;
                  IIF(EMPTY(lcPColor),''," And Right(style,"+ALLTRIM(STR(lenClrlen))+") like '"+lcPColor+"'") ,'ITEM')
    = gfSqlRun("SELECT * FROM Item(INDEX = cStyle) WHERE cinvtype = '0002' AND  style  LIKE '"+;
                  lcPStyle+"%'",'ITEM')
    *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
    
    llStyDye = (ITEM.cDye_Flg='Y')
  ELSE
    =gfSEEK(lcPStyle,'Style')
    llStyDye = (Style.cDye_Flg='Y')
  ENDIF
  SELECT MDINVNTL
  gfSetOrder('MDINVNTL')
  IF loFormset.llDyelot AND llStyDye
*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
*!*      LOCATE REST WHILE cbattype+cLkBatch+style+color+DYELOT =;
*!*                        lcKey+PADR(loFormset.ariaform1.kbBatchNo.keytextbox.Value,6)+lcPStyle+lcPColor;
*!*                   FOR !EMPTY(DYELOT)
    LOCATE REST WHILE cbattype+cLkBatch+style+DYELOT =;
                      lcKey+PADR(loFormset.ariaform1.kbBatchNo.keytextbox.Value,6)+lcPStyle;
                 FOR !EMPTY(DYELOT)
*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
  ENDIF
  lcDye1 = Dyelot
*!*    IF ASCAN(laEvntTrig,PADR("DLPSTLCK",10)) <> 0 
*!*      =gfDoTriger("ICINVLK",PADR("DLPSTLCK",10))
*!*    ELSE

  *B608785 TMI 01/22/2009 [Start] moved from below , progress bar show
*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
*!*    loFormSet.oProgress.TotalProgress = loFormSet.lnTotRec
*!*    loFormSet.oProgress.lblFirstLabel.Caption = LANG_Posting_Title
*!*    loFormSet.oProgress.Show()
*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
  *B608785 TMI 01/22/2009 [End  ] 

  *T20060818.0001(C200876) TMI [Start] Valid function for posting batch
  IF ASCAN(loFormSet.laEvntTrig,PADR('DLPSTLCK',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0 ;
    .AND. gfDoTriger('POSTREC',PADR('ISUSEBIN',10))
    *B608785 TMI 01/19/2009 [Start] initilaize lcAdjReason
    STORE '' TO lcBinLoc,lcAdjReason 
    *B608785 TMI 01/19/2009 [End  ] 
    =loFormSet.mDoTrigger(PADR('DLPSTLCK',10)) 
  ELSE
  *T20060818.0001(C200876) TMI [End  ] 

  *B608785 TMI 01/22/2009 [Start] move this before the trigger
*!*	  loFormSet.oProgress.TotalProgress = loFormSet.lnTotRec
*!*	  loFormSet.oProgress.lblFirstLabel.Caption = LANG_Posting_Title
*!*	  loFormSet.oProgress.Show()
  *B608785 TMI 01/22/2009 [End  ] 

   lcStyPic  = loFormSet.lcstytitle
   * gfItemMask("HI")
*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
*!*     SCAN REST WHILE cbattype+cLkBatch+style+color+DYELOT =;
*!*                      lcPType +lcPBatch+lcPStyle+lcPColor+lcDye1;
*!*                FOR   ((OldTotStk<>0 OR OLDSTK1<>0 OR OLDSTK2<>0 OR ;
*!*                                        OLDSTK3<>0 OR OLDSTK4<>0 OR ;
*!*                                        OLDSTK5<>0 OR OLDSTK6<>0 OR ;
*!*                                        OLDSTK7<>0 OR OLDSTK8<>0)   ;
*!*                                        OR TotStk <> 0 OR OnHand <> 0 OR OldQty <> 0 OR nStkVal <> 0)
   SCAN REST WHILE cbattype+cLkBatch+style+DYELOT =;
                    lcPType +lcPBatch+lcPStyle+lcDye1;
              FOR   ((OldTotStk<>0 OR OLDSTK1<>0 OR OLDSTK2<>0 OR ;
                                      OLDSTK3<>0 OR OLDSTK4<>0 OR ;
                                      OLDSTK5<>0 OR OLDSTK6<>0 OR ;
                                      OLDSTK7<>0 OR OLDSTK8<>0)   ;
                                      OR TotStk <> 0 OR OnHand <> 0 OR OldQty <> 0 OR nStkVal <> 0)

 *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
      *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
      *WAIT WINDOW LANG_Posting + MDINVNTH.cLkBatch + ;
                                      IIF(loFormset.llMatModul,'  Item   - Color \ ' + PADR(STYLE,7)+'-'+COLOR ,;
                                      lcStyPic +[\ ]+Style)  NOWAIT
      WAIT WINDOW LANG_Posting + MDINVNTH.cLkBatch + ;
                                      IIF(loFormset.llMatModul,'  Item   - Color \ ' + STYLE,lcStyPic +[\ ]+Style)  NOWAIT
      *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
*!*        loFormSet.oProgress.CurrentProgress(lnCountLn)    
*!*        lnCountLn = lnCountLn + 1
*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
*!*        =gfThermo(loFormSet.lnTotRec,lnCountLn,LANG_Posting_Title)

      *----Start Calculate Stk for posting
      lcAdjReason = MDINVNTL.cAdjReason
      IF !loFormset.llMatModul
        FOR lnIndex = 1 TO 8
          lcSub = STR(lnIndex,1)
          laAdjust[lnIndex] = laAdjust[lnIndex] + MDINVNTL.Stk&lcSub
          laOldStk[lnIndex] = laOldStk[lnIndex] + MDINVNTL.OldStk&lcSub
        ENDFOR
      ELSE
      *--MMt test prob.
      *--MMT
        laAdjust[1]  = IIF(loFormset.llMatModul,MDINVNTL.OnHand,MDINVNTL.TOTSTK)
        laLocInf[1]  = -1 * MDINVNTL.OldQty
        laOldStk[2] =  MDINVNTL.nStkVal
      ENDIF
      IF loFormset.llMatModul
        laLocInf[9]  = -1 * MDINVNTL.OldQty
        laLocInf[10] = -1 * (MDINVNTL.OldQty * mdinvntl.oldcost)
      ELSE
        laLocInf[9]  = -1 * MDINVNTL.OldTotStk
        laLocInf[10] = -1 * (MDINVNTL.OldTotStk * mdinvntl.oldcost)
      ENDIF
      laLocInf[11]  = loFormset.ariaform1.dtpLock.Value
      laAdjust[9]  = laAdjust[9] + IIF(loFormset.llMatModul,MDINVNTL.OnHand,MDINVNTL.TOTSTK)
      laOldStk[9]  = laOldStk[9] + IIF(loFormset.llMatModul,MDINVNTL.OldQty,MDINVNTL.OldTOTSTK)
      laAdjust[10] = MDINVNTL.COST
  ENDSCAN
*!*    ENDIF 
  *T20060818.0001(C200876) TMI [Start] 
  ENDIF
  *T20060818.0001(C200876) TMI [End  ] 
*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
*!*    IF cbattype+cLkBatch+style+color+DYELOT <>;
*!*                    lcPType +lcPBatch+lcPStyle+lcPColor
  IF cbattype+cLkBatch+style+DYELOT <>;
                  lcPType +lcPBatch+lcPStyle
*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
    llAdjYNo = .T.
    laOldStk[12] = 1
  ENDIF
  
  *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
  loFormSet.oProgress.CurrentProgress(lnCountLn)    
  lnCountLn = lnCountLn + 1
  *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
  
  
  IF (laAdjust[9]<>0 OR laOldStk[9]<>0 OR laOldStk[1]<>0 OR laOldStk[2]<>0 OR laOldStk[3]<>0 OR laOldStk[4]<>0 OR laOldStk[5]<>0 OR laOldStk[6]<>0 OR laOldStk[7]<>0 OR laOldStk[8]<>0)

    laOldStk[10] = laOldStk[9]*lnTempCst
    laOldStk[11] = loFormset.ariaform1.dtpPost.Value 
    lnMarkQty    = laOldStk[9]
    lnMarkVal    = laOldStk[9]*lnTempCst
    IF loFormset.llMatModul
      =lfPostMat()
      *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
 *    llDumy = (llAdjYNo AND lfLkStyInv(SUBSTR(lcPStyle,1,7),lcPColor,.F.))
      llDumy = (llAdjYNo AND lfLkStyInv(lcPStyle,.F.))
      *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
    ELSE
      =lfPostSty()
      *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
      *lcDummy = (llAdjYNo AND lfLkStyInv(lcPStyle,lcPColor,.F.))      
      lcDummy = (llAdjYNo AND lfLkStyInv(lcPStyle,.F.))
      *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[ENd]
    ENDIF
  ELSE
    IF loFormset.llMatModul
      *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
      *llDumy = (llAdjYNo AND lfLkStyInv(SUBSTR(lcPStyle,1,7),lcPColor,.F.))
      llDumy = (llAdjYNo AND lfLkStyInv(lcPStyle,.F.))
      *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
    ELSE
      *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
      *lcDummy = ( llAdjYNo AND lfLkStyInv(lcPStyle,lcPColor,.F.))
      lcDummy = ( llAdjYNo AND lfLkStyInv(lcPStyle,.F.))
      *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[ENd]
    ENDIF
  ENDIF
  SELECT MDINVNTL
ENDDO

FOR lnRest = lnCountLn TO loFormSet.lnTotRec
  loFormSet.oProgress.CurrentProgress(lnRest)    
*  =gfThermo(loFormSet.lnTotRec , lnRest , 'Posting')
ENDFOR    && End of FOR Loop


SELECT MDINVNTL
=gfSEEK(lcKey+PADR(loFormset.ariaform1.kbBatchNo.keytextbox.Value,6))
*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
*!*  SCAN REST WHILE cbattype+cLkBatch+style+color+dyelot+clocation;
*!*                                   = lcPType+MDINVNTH.cLkBatch
SCAN REST WHILE cbattype+cLkBatch+style+dyelot+clocation;
                                 = lcPType+MDINVNTH.cLkBatch
*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
   gfReplace([nSteps WITH 0 ])

ENDSCAN                                  
*gfREPLACE REST nSteps WITH 0 WHILE cbattype+cLkBatch+style+color+dyelot+clocation;
                                 = lcPType+MDINVNTH.cLkBatch

gfSetOrder(lcOldOrd)

loFormset.oToolBar.ChangeButtonStatus('cmdPost','DISABLED')
loFormset.ariaform1.dtpPost.enabled = .F.

lnAlias = SELECT()
IF loFormset.llLinkGL
  SELECT (loFormset.lctmpGlDis)
  IF EMPTY(lcGlSess)
    lcGlSess   = gfSequence('GLSession')
  ENDIF
  REPLACE ALL GlSession WITH lcGlSess
  *-- If temp. file not empty
  IF RECCOUNT() <> 0
    SELECT GLDIST
    SELECT(loFormset.lctmpGlDis)
    SCAN 
      SCATTER MEMO MEMVAR 
      SELECT GLDIST
      APPEND BLANK 
      GATHER MEMO MEMVAR 
      gfreplace()  
    ENDSCAN 
    *APPEND FROM (oAriaApplication.WorkDir+lctmpGlDis)
    
  ENDIF
  USE IN (loFormset.lctmpGlDis)
  ERASE (oAriaApplication.WorkDir+loFormset.lctmpGlDis+".DBF")
ENDIF

IF loFormset.llMatModul AND !USED('Item')
  = gfSqlRun("SELECT * FROM Item(INDEX = cStyle) WHERE cinvtype = '0002' AND  style  =''" ,'ITEM')
ENDIF 
*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
WAIT CLEAR 
*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
lfSaveFile()
loFormset.lcPostScVar = ''
SELECT(lnAlias)


*!*************************************************************
*! Name      : lpSavScr
*! Developer : Mariam Mazhar [MMT]
*! Date      : 11/19/2006
*! Purpose   : Valid function for save Button.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : lfShowBtns
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lpSavScr()
*!*************************************************************
*!
FUNCTION lfSavScr
PARAMETERS loFormSet

*--Start locking...
SELECT(loFormSet.lcTmpQuery)
LOCATE 
IF EOF()
  RETURN .F.
ENDIF

DO CASE
*--- Edit Mode
  CASE loFormset.activemode = 'E'
    =lfSavEdt()
    lfSaveFile()
  CASE loFormset.activemode = 'A'
    =lfSavNew()
ENDCASE
SELECT(loFormSet.lcTmpQuery)
LOCATE 
RETURN .T.

*!*************************************************************
*! Name      : lfSavEdt
*! Developer : Mariam Mazhar [MMT]
*! Date      : 11/19/2006
*! Purpose   : Function to save in edit mode.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Called from : 
*!         Procedures : ICINVLK.PRG
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfSavEdt()
*!*************************************************************
*!
FUNCTION lfSavEdt
SELECT (loFormSet.lcBatHdr)
SCATTER MEMVAR MEMO
SELECT MDINVNTH
m.Content = loFormSet.ariaform1.txtNotes.Value
m.Type    = loFormset.lcStatus
GATHER MEMVAR MEMO


*B608941.1 TMI 07/16/2009 12:57:25 PM [Start] define lnCountLn,initialize loFormSet.lnTotRec
LOCAL lnCountLn,lnRest
loFormSet.lnTotRec = 0
*B608941.1 TMI 07/16/2009 12:57:25 PM [End  ] 

SELECT MDINVNTL
gfSetorder('Mdinvntl')
lcKey = IIF(loFormSet.llmatmodul,'M','S')
=gfSEEK(lcKey+PADR(loFormset.ariaform1.kbBatchNo.keytextbox.Value,6))
*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
*!*  SCAN REST WHILE cbattype+cLkBatch+style+color+dyelot+clocation = ;
*!*                    lcKey+PADR(loFormset.ariaform1.kbBatchNo.keytextbox.Value,6) ;
*!*                    FOR IIF(loFormset.llDyelot AND ;
*!*                    lfStyDye(Style,Color),!EMPTY(DYELOT),.T.)
SCAN REST WHILE cbattype+cLkBatch+style+dyelot+clocation = ;
                  lcKey+PADR(loFormset.ariaform1.kbBatchNo.keytextbox.Value,6) ;
                  FOR IIF(loFormset.llDyelot AND ;
                  lfStyDye(Style),!EMPTY(DYELOT),.T.)
*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
  gfDelete()                  
  *B608941.1 TMI 07/16/2009 12:56:46 PM [Start] increment lnCountLn
  loFormSet.lnTotRec = loFormSet.lnTotRec + 1
  *B608941.1 TMI 07/16/2009 12:56:47 PM [End  ] 
ENDSCAN                                      
*DELETE REST WHILE cbattype+cLkBatch+style+color+dyelot+clocation = ;
                  lcKey+loFormset.ariaform1.kbBatchNo.keytextbox.Value ;
                  FOR IIF(loFormset.llDyelot AND ;
                  lfStyDye(Style,Color),!EMPTY(DYELOT),.T.)


*B608941.1 TMI 07/16/2009 12:55:56 PM [Start] show the progress bar
loFormSet.oProgress.TotalProgress = loFormSet.lnTotRec
loFormSet.oProgress.lblFirstLabel.Caption = LANG_Saving_Title
loFormSet.oProgress.Show()
lnCountLn = 0
*B608941.1 TMI 07/16/2009 12:55:56 PM [End  ] 

SELECT (loFormset.lcBatLin)
LOCATE 
*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
*SCAN FOR IIF(loFormset.llDyelot AND lfStyDye(Style,Color),!EMPTY(DYELOT),.T.)
SCAN FOR IIF(loFormset.llDyelot AND lfStyDye(Style),!EMPTY(DYELOT),.T.)
*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
  SCATTER MEMVAR MEMO
  m.Cadd_user = oAriaApplication.User_Name
  m.Dadd_date = DATE()
  m.Cadd_time = gfGetTime()
  m.cAdjReason = m.cAdjReason
  INSERT INTO MDINVNTL FROM MEMVAR
  SELECT MDINVNTL 
  gfReplace()
  *B608941.1 TMI 07/16/2009 12:59:27 PM [Start] update the progress bar
  lnCountLn = lnCountLn + 1
  loFormSet.oProgress.CurrentProgress(lnCountLn)    
  *B608941.1 TMI 07/16/2009 12:59:29 PM [End  ] 
ENDSCAN

*B608941.1 TMI 07/16/2009 01:03:48 PM [Start] close progress
FOR lnRest = lnCountLn TO loFormSet.lnTotRec
  loFormSet.oProgress.CurrentProgress(lnRest)    
ENDFOR    && End of FOR Loop
*B608941.1 TMI 07/16/2009 01:03:50 PM [End  ] 

*!*************************************************************
*! Name      : lfSavNew
*! Developer : Mariam Mazhar [MMT]
*! Date      : 11/19/2006
*! Purpose   : Function to save in New mode.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Called from : 
*!         Procedures : ICINVLK.PRG
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfSavNew()
*!*************************************************************
*!
FUNCTION lfSavNew


lcKey = IIF(loFormSet.llmatmodul,'M','S')
lcBatchNo = gfsequence('cLkBatch')

SELECT (loFormset.lcBatHdr)
*!*	LOCATE 
*!*	IF EOF()
*!*	  =gfModalGen('TRM32044B00000','DIALOG')
*!*	  RETURN .F.
*!*	ENDIF 
REPLACE ALL cLkBatch WITH lcBatchNo,cBatType WITH lcKey

SELECT (loFormSet.lcBatLin)
REPLACE ALL cLkBatch WITH lcBatchNo,cBatType WITH lcKey
SELECT (loFormset.lcBatHdr)
LOCATE 
SCATTER MEMVAR MEMO

IF nSteps < 1
  SELECT MDINVNTH
  lcOldFlt = SET("Filter")
  SET FILTER TO 
  LOCATE FOR EMPTY(mdinvnth.cbattype) AND EMPTY(mdinvnth.cLkBatch)
  IF FOUND()
    GATHER MEMVAR MEMO
    = gfAdd_Info('MDINVNTH')
  ELSE
    APPEND BLANK 
    GATHER MEMVAR MEMO
    = gfAdd_Info('MDINVNTH')
  ENDIF 
  gfReplace()
  SET FILTER TO &lcOldFlt
  SELECT (loFormset.lcBatHdr)
  =RLOCK()
  REPLACE nSteps WITH 1
  UNLOCK
ENDIF

*B608941.1 TMI 07/16/2009 12:55:56 PM [Start] show the progress bar
loFormSet.lnTotRec = RECCOUNT(loFormset.lcBatLin)
loFormSet.oProgress.TotalProgress = loFormSet.lnTotRec 
loFormSet.oProgress.lblFirstLabel.Caption = LANG_Saving_Title
loFormSet.oProgress.Show()
LOCAL lnCountLn,lnRest
lnCountLn = 0
*B608941.1 TMI 07/16/2009 12:55:56 PM [End  ] 


SELECT (loFormset.lcBatLin)

SCAN
  SCATTER MEMVAR MEMO
  m.Cadd_user = oAriaApplication.User_Name
  m.Dadd_date = DATE()
  m.Cadd_time = gfGetTime()
  IF nSteps < 1
    m.cAdjReason = m.cAdjReason
    INSERT INTO MDINVNTL FROM MEMVAR
    SELECT MDINVNTL 
    gfReplace()
    IF loFormset.llMatModul
      *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
      *=lfLkStyInv(SUBSTR(M.STYLE,1,7),M.COLOR,.T.)
      =lfLkStyInv(M.STYLE,.T.)
      *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
    ELSE
      *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
      *=lfLkStyInv(M.STYLE,M.COLOR,.T.)      
      =lfLkStyInv(M.STYLE,.T.)
      *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
    ENDIF
    SELECT (loFormset.lcBatLin)
    =RLOCK()
    REPLACE nSteps WITH 1
    UNLOCK
  ENDIF
  *B608941.1 TMI 07/16/2009 12:59:27 PM [Start] update the progress bar
  lnCountLn = lnCountLn + 1
  loFormSet.oProgress.CurrentProgress(lnCountLn)    
  *B608941.1 TMI 07/16/2009 12:59:29 PM [End  ] 
ENDSCAN

*B608941.1 TMI 07/16/2009 01:03:48 PM [Start] close progress
FOR lnRest = lnCountLn TO loFormSet.lnTotRec
  loFormSet.oProgress.CurrentProgress(lnRest)    
ENDFOR    && End of FOR Loop
*B608941.1 TMI 07/16/2009 01:03:50 PM [End  ] 

lfSaveFile()
=gfModalGen('TRM42180B00000','DIALOG',lcBatchNo)

*!*************************************************************
*! Name      : lfLkStyInv
*! Developer : Mariam Mazhar {MMT}
*! Date      : 11/19/2006
*! Purpose   : Functio to lock style or material in journal file.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : lcItem,lcColor,llFalg
*!*************************************************************
*! Called from : 
*!         Procedures : ICINVLK.PRG
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfLkStyInv()
*!*************************************************************
*!
FUNCTION lfLkStyInv
*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
*PARAMETER lcItem,lcColor,llFalg
PARAMETER lcItem,llFalg
*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
PRIVATE lnAlias
lnAlias = SELECT(0)

lnMajLength = loFormset.lnmajlength
*LEN(gfItemMask('PM','', "0002"))
lenClrlen = loFormset.lenclrlen
*LEN(gfitemmask("PN", "", "0002"))   

IF loFormset.llMatModul
*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
*  = gfSqlRun("SELECT * FROM Itemloc(INDEX = Stydye) WHERE cinvtype = '0002' AND  style  LIKE '"+;
                  SUBSTR(lcItem,1,lnMajLength)+"%'"+;
                  IIF(loFormSet.llWareHus," AND cwarecode='"+PADR(loFormset.ariaform1.kblocation.keytextbox.Value,6)+"'","")+;
                  IIF(EMPTY(lcColor),'',"And Right(style,"+ALLTRIM(STR(lenClrlen))+") like '"+lcColor+"'") ,'ITEMLOC')
  = gfSqlRun("SELECT * FROM Itemloc(INDEX = Stydye) WHERE cinvtype = '0002' AND  style  LIKE '"+;
                  lcItem+"%'"+;
                  IIF(loFormSet.llWareHus," AND cwarecode='"+STRTRAN(PADR(loFormset.ariaform1.kblocation.keytextbox.Value,6),"'","''")+"'",""),'ITEMLOC')
*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
  SELECT ITEMLOC
*  =SEEK(lcItem+lcColor+IIF(llWareHus,ladata[8],''))
  gfREPLACE([dLlokDate WITH loFormset.ariaform1.dtpLock.Value,]+;
            [dLPost    WITH loFormset.ariaform1.dtpPost.Value])

  
*  gfSetorder('STYINVJL')
*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
*  = gfSqlRun("SELECT * FROM ITEMJRNL(INDEX = STYINVJL) WHERE cinvtype = '0002' AND  style  LIKE '"+;
                  SUBSTR(lcItem,1,lnMajLength)+"%'"+;
                  IIF(loFormSet.llWareHus," AND cwarecode='"+PADR(loFormset.ariaform1.kblocation.keytextbox.Value,6)+"'","")+;
                  IIF(EMPTY(lcColor),'',"And Right(style,"+ALLTRIM(STR(lenClrlen))+") like '"+lcColor+"'") ,'ITEMJRNL')
  = gfSqlRun("SELECT * FROM ITEMJRNL(INDEX = STYINVJL) WHERE cinvtype = '0002' AND  style  LIKE '"+;
                  lcItem+"%'"+;
                  IIF(loFormSet.llWareHus," AND cwarecode='"+STRTRAN(PADR(loFormset.ariaform1.kblocation.keytextbox.Value,6),"'","''")+"'",""),'ITEMJRNL')
*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
  SELECT ITEMJRNL

*  =gfSEEK('0002'+lcItem+lcColor+IIF(loFormSet.llWareHus,loFormset.ariaform1.kblocation.keytextbox.Value,''))
  SCAN 
  *REST WHILE cfabric+ccolor+cwarecode+cdyelot+crsession+cisession = ;
                  lcItem+lcColor+IIF(loFormSet.llWareHus,loFormset.ariaform1.kblocation.keytextbox.Value,'')
    gfREPLACE([lLockFlg WITH llFalg])
  ENDSCAN
ELSE
  SELECT StyDye
  =gfSEEK(lcItem+IIF(loFormSet.llWareHus,PADR(loFormset.ariaform1.kblocation.keytextbox.Value,6),''))
  gfREPLACE([dLlokDate WITH loFormset.ariaform1.dtpLock.Value,]+;
          [dLPost    WITH loFormset.ariaform1.dtpPost.Value])

 
  *!B609607,1 MMT 06/08/2011 Fix bug of repeated adj. record while Posting if Bin Location is used(Start)
  IF TYPE('loFormSet') ='O' AND ASCAN(loFormSet.laEvntTrig,PADR('LKBININV',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0 ;
	 .AND. gfDoTriger('POSTREC',PADR('ISUSEBIN',10))
	=loFormSet.mDoTrigger(PADR('LKBININV',10)) 
    RETURN    
  ENDIF
  *!B609607,1 MMT 06/08/2011 Fix bug of repeated adj. record while Posting if Bin Location is used(End) 

  SELECT STYINVJL
  gfSetorder('STYINVJL')
  =gfSEEK(lcItem+IIF(loFormSet.llWareHus,PADR(loFormset.ariaform1.kblocation.keytextbox.Value,6),''))
  SCAN REST WHILE style+cwarecode+csession+DTOS(dtrdate)+ctrcode = ;
                  lcItem+IIF(loFormSet.llWareHus,PADR(loFormset.ariaform1.kblocation.keytextbox.Value,6),'')
    gfREPLACE([lLockFlg WITH llFalg])
  ENDSCAN
  SELECT MDINVNTL
ENDIF
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfSaveFile
*! Developer : Mariam Mazhar
*! Date      : 10/23/2006
*! Purpose   : function to save files
*!*************************************************************
FUNCTION lfSaveFile
*-- Call default save
lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr,3,'')
IF TYPE('lcTranCode') = 'N'
  =oAriaApplication.RemoteCompanyData.CheckRetResult("BeginTran",lcTranCode,.T.)
   RETURN .F.
ENDIF
lnUpdated = 0
lnAryLen = ALEN(oAriaApplication.laRemoteTable)
FOR lnCounter=1 TO lnAryLen
  IF oAriaApplication.laRemoteTable[lnCounter].lnDataSession == loFormSet.DataSessionId
    IF !oAriaApplication.laRemoteTable[lnCounter].TableUpdate(lcTranCode)
      lnUpdated=lnCounter
      exit
    ENDIF
  ENDIF
NEXT
IF lnUpdated>0
  oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
  RETURN
ELSE
  oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode)
ENDIF
*!*************************************************************
*! Name      : lfBackUp
*! Developer : Mariam Mazhar [MMT]
*! Date      : 11/19/2006
*! Purpose   : Functio to BackUp Data.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Called from : 
*!         Procedures : ICINVLK.PRG
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfBackUp()
*!*************************************************************
*! Commentes : This function save files in the ARIA27\Work\InvLBkup\
*!*************************************************************
*!
FUNCTION lfBackUp

PRIVATE lcPat 
lcPat = FULLPATH('')
*B610699,1 TMI 03/17/2014 17:27 [Start] check if the folder oAriaApplication.WorkDir+'InvLBkup' is there, if not create it
*IF IIF(loFormset.llMatModul,!FILE(oAriaApplication.WorkDir+'InvLBkup'+'\'+'Fabric.DBF'),;
                   !FILE(oAriaApplication.WorkDir+'InvLBkup'+'\'+'Style.DBF'))
IF !DIRECTORY(oAriaApplication.WorkDir+'InvLBkup')
  *B610699,1 TMI 03/17/2014 17:28 [End  ]
  
  SET DEFAULT  TO (oAriaApplication.WorkDir)
  
  *B610699,1 TMI 03/17/2014 16:54 [Start]
  *!MD InvLBkup
  MD InvLBkup
  *B610699,1 TMI 03/17/2014 16:54 [End  ] 
ELSE
  SET DEFAULT  TO (oAriaApplication.WorkDir+'InvLBkup')
ENDIF


SET DEFAULT  TO (oAriaApplication.DataDir)

*B610699,1 TMI 03/17/2014 16:56 [Start] if the backup folder has not been created then return 
IF !DIRECTORY(oAriaApplication.WorkDir+'InvLBkup')
  =gfModalGen('INM00206B00000','DIALOG')
  SET DEFAULT TO  &lcPat
  RETURN .F.
ENDIF  
*B610699,1 TMI 03/17/2014 16:56 [End  ] 

SELECT MDINVNTH 
COPY TO ADDBS(oAriaApplication.WorkDir+[InvLBkup])+'MDINVNTH.DBF' 

SELECT MDINVNTL
COPY TO ADDBS(oAriaApplication.WorkDir+[InvLBkup])+'MDINVNTL.DBF'

SELECT GLDIST
COPY TO ADDBS(oAriaApplication.WorkDir+[InvLBkup])+'GLDIST.DBF'


IF loFormset.llMatModul
  SELECT Item
  gfSeek('')
  COPY TO ADDBS(oAriaApplication.WorkDir+[InvLBkup])+'Item.DBF'
  
  SELECT ItemLoc
  gfSeek('')
  COPY TO ADDBS(oAriaApplication.WorkDir+[InvLBkup])+'ItemLoc.DBF'

  SELECT ITEMJRNL
  gfSeek('')
  COPY TO ADDBS(oAriaApplication.WorkDir+[InvLBkup])+'ITEMJRNL.DBF'
  
ELSE
  SELECT STYLE
  COPY TO ADDBS(oAriaApplication.WorkDir+[InvLBkup])+'STYLE.DBF'

  SELECT STYDYE
  COPY TO ADDBS(oAriaApplication.WorkDir+[InvLBkup])+'STYDYE.DBF'

  SELECT STYINVJL
  COPY TO ADDBS(oAriaApplication.WorkDir+[InvLBkup])+'STYINVJL.DBF'
ENDIF
SET DEFAULT TO  &lcPat
=gfModalGen('INM46045B00000','DIALOG' ,oAriaApplication.WorkDir+[InvLBkup])
RETURN 
*!*************************************************************
*! Name      : lfPostMat
*! Developer : Mariam Mazhar [MMT]
*! Date      : 11/19/2006
*! Purpose   : Functio to Post material.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Called from : 
*!         Procedures : ICINVLK.PRG
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfPostMat()
*!*************************************************************
*!
FUNCTION lfPostMat


IF loFormSet.llLinkGL
  lcGlSess    = gfSequence('GLSession')
  lnAdjCode   = 1
  DECLARE laTrmRltFd[1,2]
  laTrmRltFd[1,1] = 'GLACCOUNT'
  laTrmRltFd[1,2] = 'lcAdjAcct'
  =gfRltFld(lcAdjReason , @laTrmRltFd , "CADJREASON")
  DECLARE laGLDistAr[2,13]
  laGLDistAr[1,1] = IIF(EMPTY(lcLinkCode),'DEFDEF',lcLinkCode)
  laGLDistAr[2,1] = IIF(EMPTY(lcLinkCode),'DEFDEF',lcLinkCode)
  laGLDistAr[1,2] = '015'
  laGLDistAr[2,2] = '016'
  laGLDistAr[1,3] = 1
  laGLDistAr[2,3] = -1
  STORE 'MA'                              TO laGLDistAr[1,4],laGLDistAr[2,4]
  STORE ''                                TO laGLDistAr[1,5],laGLDistAr[2,5]
  STORE loFormset.ariaform1.dtpLock.Value TO laGLDistAr[1,6],laGLDistAr[2,6]
  STORE lcGLFYear                         TO laGLDistAr[1,7],laGLDistAr[2,7]
  STORE lcGLPeriod                        TO laGLDistAr[1,8],laGLDistAr[2,8]
  STORE loFormSet.lcTmpGlDis                        TO laGLDistAr[1,9],laGLDistAr[2,9]
  laGLDistAr[2,10] = lcAdjAcct
ELSE
  DIME laGLDistAr[1,1]
  laGLDistAr = ''
ENDIF

DIME laToSend[5,2]
laToSend[1,1] = "lnMarkQty"
laToSend[1,2] = lnMarkQty
laToSend[2,1] = "lnMarkVal"
laToSend[2,2] = lnMarkVal
laToSend[3,1] = "lnLineNo"
laToSend[3,2] = 0
laToSend[4,1] = "llAdjYes"
laToSend[4,2] = llAdjYNo
laToSend[5,1] = "lnMStkVl"
laToSend[5,2] = laOldStk[2]

lenClrlen = loFormset.lenclrlen  
lnMajLength =loFormset.lnmajlength
lcStyPic  = loFormset.lcitmpic
lcSepar   = SUBSTR(lcStyPic,lnMajLength+1,1)

*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
*lnRet = gfItemCrl('8','0002',PADR(lcPStyle,lnMajLength)+lcSepar+PADR(lcPColor,lenClrlen),;
                     PADR(loFormset.ariaform1.kblocation.keytextbox.Value,6),lcDye1,;
                     loFormset.ariaform1.dtpLock.Value,loFormset.ariaform1.dtpPost.Value,SPACE(6),;
                      @laAdjust,laAdjust[10],;
                     'MATERIAL LOCKING',lcAdjReason,;
                     '',@laGLDistAr,lcGlSess,'','','','','','',@laLocInf,'',.F.,@laToSend)
lnRet = gfItemCrl('8','0002',lcPStyle,;
                     PADR(loFormset.ariaform1.kblocation.keytextbox.Value,6),lcDye1,;
                     loFormset.ariaform1.dtpLock.Value,loFormset.ariaform1.dtpPost.Value,SPACE(6),;
                      @laAdjust,laAdjust[10],;
                     'MATERIAL LOCKING',lcAdjReason,;
                     '',@laGLDistAr,lcGlSess,'','','','','','',@laLocInf,'',.F.,@laToSend)
*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
*!*************************************************************
*! Name      : lfPostSty
*! Developer : Mariam Mazhar [MMT]
*! Date      : 11/19/2006
*! Purpose   : Functio to Post style.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Called from : 
*!         Procedures : ICINVLK.PRG
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfPostSty()
*!*************************************************************
*!
FUNCTION lfPostSty

*--G/L Array difinition and initialization.
IF loFormSet.llLinkGL
  lcGlSess   = gfSequence('GLSession')
  lnAdjCode  = 1
  DECLARE laTrmRltFd[1,2]
  laTrmRltFd[1,1] = 'GLACCOUNT'
  laTrmRltFd[1,2] = 'lcAdjAcct'
  =gfRltFld(lcAdjReason , @laTrmRltFd , "CADJREASON")
  DECLARE laGLDistAr[2,13]
  laGLDistAr[1,1] = IIF(EMPTY(lcLinkCode),'DEFDEF',lcLinkCode)
  laGLDistAr[2,1] = IIF(EMPTY(lcLinkCode),'DEFDEF',lcLinkCode)
  laGLDistAr[1,2] = '006'
  laGLDistAr[2,2] = '007'
  laGLDistAr[1,3] = 1
  laGLDistAr[2,3] = -1
  
  STORE 'LK' TO laGLDistAr[1,4],laGLDistAr[2,4]
  
  STORE ''                                TO laGLDistAr[1,5],laGLDistAr[2,5]
  *B608844,1 TMI 04/11/2009 [Start] send the Post date to the GLDIST
  *STORE loFormset.ariaform1.dtpLock.Value TO laGLDistAr[1,6],laGLDistAr[2,6]
  STORE loFormset.ariaform1.dtpPost.Value TO laGLDistAr[1,6],laGLDistAr[2,6]
  *B608844,1 TMI 04/11/2009 [End  ] 
  STORE lcGLFYear                         TO laGLDistAr[1,7],laGLDistAr[2,7]
  STORE lcGLPeriod                        TO laGLDistAr[1,8],laGLDistAr[2,8]
  STORE loFormset.lcTmpGlDis              TO laGLDistAr[1,9],laGLDistAr[2,9]
  laGLDistAr[2,10] = lcAdjAcct
ELSE
  DIME laGLDistAr[1,1]
  laGLDistAr = ''
ENDIF

*!*  IF ASCAN(laEvntTrig,PADR("DLSTYCRL",10)) <> 0 
*!*    =gfDoTriger("ICINVLK",PADR("DLSTYCRL",10))
*!*  ELSE
*T20060818.0001(C200876) TMI [Start] calls the glstycrl from bn4main
IF ASCAN(loFormSet.laEvntTrig,PADR('DLSTYCRL',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0 ;
  .AND. gfDoTriger('POSTREC',PADR('ISUSEBIN',10))
  =loFormSet.mDoTrigger(PADR('DLSTYCRL',10)) 
ELSE
*T20060818.0001(C200876) TMI [End  ] 

  *B608844,1 TMI 04/08/2009 [Start] update the STYINVJL with the POSTING DATE not with the Batch Date
  *lnRet = gfStyCrl('9',lcPStyle,PADR(loFormset.ariaform1.kblocation.keytextbox.Value,6),lcDye1,loFormset.ariaform1.dtpLock.Value,'',@laAdjust,laAdjust[10],;
                         '',.T.,lcAdjReason,1,'MDINVNTL','NSTEPS',@laGLDistAr,0,'','',@laOldStk)

  lnRet = gfStyCrl('9',lcPStyle,PADR(loFormset.ariaform1.kblocation.keytextbox.Value,6),lcDye1,loFormset.ariaform1.dtpPost.Value,'',@laAdjust,laAdjust[10],;
                       '',.T.,lcAdjReason,1,'MDINVNTL','NSTEPS',@laGLDistAr,0,'','',@laOldStk)

  *B608844,1 TMI 04/08/2009 [End  ]                        

  *!*  ENDIF
  *T20060818.0001(C200876) TMI [Start] 
ENDIF
*T20060818.0001(C200876) TMI [End  ] 
*!*************************************************************
*! Name      : lpDelScr
*! Developer : Mariam Mazhar [MMT]
*! Date      : 11/19/2006
*! Purpose   : Function for delete batch.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : lfShowBtns
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lpDelScr()
*!*************************************************************
*!
FUNCTION lpDelScr
PARAMETERS loFormSet
lcKey = IIF(loFormSet.llmatmodul,'M','S')
SELECT MDINVNTH
=gfSEEK(lcKey+PADR(loFormset.ariaform1.kbBatchNo.keytextbox.Value,6))
SCAN REST WHILE cBattype+cLkBatch = lcKey+PADR(loFormset.ariaform1.kbBatchNo.keytextbox.Value,6)
  gfDelete()
ENDSCAN 

SELECT MDINVNTL
gfSetOrder('MDINVNTL')
=gfSEEK(lcKey+PADR(loFormset.ariaform1.kbBatchNo.keytextbox.Value,6))
SCAN REST WHILE cBattype+cLkBatch = lcKey+PADR(loFormset.ariaform1.kbBatchNo.keytextbox.Value,6)
  SCATTER MEMVAR MEMO
  IF loFormset.llMatModul
    *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
    *=lfLkStyInv(SUBSTR(m.Style,1,7),m.Color,.F.)
    =lfLkStyInv(m.Style,.F.)
    *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
  ELSE
    *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
    *=lfLkStyInv(m.Style,m.Color,.F.)    
    =lfLkStyInv(m.Style,.F.)
    *!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
  ENDIF
  SELECT MDINVNTL
  gfDELETE()
ENDSCAN
lfSaveFile()
RETURN .T.                     


*!*************************************************************
*! Name      : lfHoldP
*! Developer : Mariam Mazhar [MMT]
*! Date      : 11/19/2006
*! Purpose   : VAlid Functio to hold batch.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Called from : 
*!         Procedures : ICINVLK.PRG
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lpClsScr()
*!*************************************************************
*!
FUNCTION lfHoldP
PARAMETERS loFormSet
lnOld = SELECT(0)
SELECT (loFormSet.lcBatHdr)
IF loFormSet.lcStatus = 'H'
  loFormSet.lcStatus = 'M'
  REPLACE TYPE WITH 'M'
  loFormset.ariaform1.txtStatus.Value     = 'Open'
  loFormset.AriaForm1.txtStatus.Ariacolor = "G" 
ELSE
  loFormSet.lcStatus = IIF(loFormSet.lcStatus <>'P','H','P')
  REPLACE TYPE WITH 'H'
  loFormset.ariaform1.txtStatus.Value     = IIF(loFormSet.lcStatus ='P','Posted',IIF(loFormSet.lcStatus ='H',[Hold],"Open"))
  loFormset.AriaForm1.txtStatus.Ariacolor = "R" 
ENDIF
SELECT (lnOld)
*!*************************************************************
*! Name      : lfvCont
*! Developer : Mariam Mazhar [MMT]
*! Date      : 11/19/2006
*! Purpose   : Valid function for contents field.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Called from : 
*!         Procedures : ICINVLK.PRG
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvCont()
*!*************************************************************
*!
FUNCTION lfvCont
PARAMETERS loFormSet
PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT (loFormSet.lcBatHdr)
REPLACE Content WITH loFormSet.ariaform1.txtNotes.Value 
SELECT(lnAlias)
*!*************************************************************
*! Name      : lfvPostDat
*! Developer : Mariam Mazhar [MMT]
*! Date      : 11/19/2006
*! Purpose   : Valid function for Post date.
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvPostDat()
*!*************************************************************
*!
FUNCTION lfvPostDat
PARAMETERS loFormSet
PRIVATE lnAlias
lnAlias = SELECT(0)
IF loFormset.ariaform1.dtpPost.Value  <> {} AND loFormset.ariaform1.dtpLock.Value > loFormset.ariaform1.dtpPost.Value 
  =gfModalGen('TRM42192B00000','DIALOG')
  RETURN
ENDIF
IF loFormset.ariaform1.dtpPost.Value   = {}
  RETURN
ENDIF
SELECT (loFormSet.lcBatHdr)
REPLACE dPostDate WITH loFormset.ariaform1.dtpPost.Value 
SELECT(lnAlias)
*!*************************************************************
*! Name      : lfGetDesc
*! Developer : Mariam Mazhar [MMT]
*! Date      : 11/19/2006
*! Purpose   : get color descripition
*!*************************************************************
FUNCTION lfGetDesc
PARAMETERS lcMFCode
lcOldAlias = SELECT(0)
SELECT Codes
lcMfName = ""
gfSetorder('Codes')
IF gfSEEK('N'+lcMFCode+'N'+'COLOR     ')
  lcMfName = codes.cdiscrep
ENDIF
SELECT(lcOldAlias)
RETURN lcMfName
  


*:**************************************************************************
*:* Name        : lfGetClrPo
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 04/09/2009
*:* Purpose     : Get color Position in the style 
*:***************************************************************************
*B608844,1
FUNCTION lfGetClrPo
PRIVATE laItemSeg
LOCAL lnCount
lnClrPo = 0
DECLARE laItemSeg[1]
*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[Start]
*=gfItemMask(@laItemSeg)
=gfItemMask(@laItemSeg,'',IIF(loFormSet.llmatmodul,'0002','0001'))
*!B609369,1 MMT 08/04/2010 Save Full Materail code in style field in table MDINVNTL[End]
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnColorLen = LEN(laItemSeg[lnCount,3])  
    lnClrPo    = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR
*-- end of lfGetClrPo.



*!E303058,1 MMT 02/05/2012 Allow user to import excel file after creating batch in locking screen[T20120111.0036][Start]
*:**************************************************************************
*:* Name        : lfvImpQtyPth
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 02/06/2012
*:* Purpose     : Import New Qty from CSV File
*:***************************************************************************
FUNCTION lfvImpQtyPth
LPARAMETERS loParentForm

lcOldPath = FULLPATH('')
lcFileName = GETFILE('CSV', LANG_Imp_Brow, LANG_Import,1) 
SET DEFAULT TO (lcOldPath)
IF UPPER(JUSTEXT(lcFileName)) # "CSV"
  SET DEFAULT TO (lcOldPath)
  *--- "This file cannot be selected. You must select a file of type CSV"
  =gfModalGen('TRM00000B00000','DIALOG',.F.,.F.,LANG_Imprt_Error)
  lcFileName  = SPACE(40)
  RETURN
ENDIF

DIMENSION laSFile[1,1]
STORE ' ' TO laSFile
DIMENSION laFileStru[11, 4]

 laFileStru[1,1] = 'STYLE'
 laFileStru[1,2] = 'C'
 laFileStru[1,3] = 19
 laFileStru[1,4] = 0

 laFileStru[2,1] = 'Desc'
 laFileStru[2,2] = 'C'
 laFileStru[2,3] = 20
 laFileStru[2,4] = 0
 
 laFileStru[3,1] = 'Stk1'
 laFileStru[3,2] = 'N'
 laFileStru[3,3] = 10
 laFileStru[3,4] = 0

 laFileStru[4,1] = 'Stk2'
 laFileStru[4,2] = 'N'
 laFileStru[4,3] = 10
 laFileStru[4,4] = 0

 laFileStru[5,1] = 'Stk3'
 laFileStru[5,2] = 'N'
 laFileStru[5,3] = 10
 laFileStru[5,4] = 0

 laFileStru[6,1] = 'Stk4'
 laFileStru[6,2] = 'N'
 laFileStru[6,3] = 10
 laFileStru[6,4] = 0

 laFileStru[7,1] = 'Stk5'
 laFileStru[7,2] = 'N'
 laFileStru[7,3] = 10
 laFileStru[7,4] = 0

 laFileStru[8,1] = 'Stk6'
 laFileStru[8,2] = 'N'
 laFileStru[8,3] = 10
 laFileStru[8,4] = 0

 laFileStru[9,1] = 'Stk7'
 laFileStru[9,2] = 'N'
 laFileStru[9,3] = 10
 laFileStru[9,4] = 0

 laFileStru[10,1] = 'Stk8'
 laFileStru[10,2] = 'N'
 laFileStru[10,3] = 10
 laFileStru[10,4] = 0

 laFileStru[11,1] = 'totStk'
 laFileStru[11,2] = 'N'
 laFileStru[11,3] = 7
 laFileStru[11,4] = 0

=gfCrtTmp(loParentForm.lcInport,@laFileStru,.F.)

DIMENSION laFileStru[4, 4]
laFileStru[1,1] = 'WARECODE'
laFileStru[1,2] = 'C'
laFileStru[1,3] = 6
laFileStru[1,4] = 0

laFileStru[2,1] = 'STYLE'
laFileStru[2,2] = 'C'
laFileStru[2,3] = 19
laFileStru[2,4] = 0

laFileStru[3,1] = 'REASON'
laFileStru[3,2] = 'C'
laFileStru[3,3] = 60
laFileStru[3,4] = 0

laFileStru[4,1] = 'cdesc'
laFileStru[4,2] = 'C'
laFileStru[4,3] = 35
laFileStru[4,4] = 0
=gfCrtTmp(loParentForm.lcRejSty,@laFileStru,'WARECODE + STYLE',loParentForm.lcRejSty,.F.)
SELECT (loParentForm.lcInport)
lcImportFile  = loParentForm.lcInport
lcBatchno = PADR(loParentForm.ariaform1.kbBatchNo.keytextbox.Value,6)
lcStyPic  = loParentForm.lcstytitle
lcKey = IIF(loParentForm.llmatmodul,'M','S')
ZAP
APPEND FROM (lcFileName) DELIMITED
llUpdated = .F.
SCAN FOR RECNO() > 1

  llInvalidStyle = .F.
  IF gfSeek(&lcImportFile..STYLE,'STYLE','STYLE')
    =gfSeek('S'+Style.Scale,'Scale','Scale')
    IF Scale.cnt < 8
      FOR lnZ = Scale.cnt + 1 TO 8
        lcZ = STR(lnZ,1)
        IF &lcImportFile..Stk&lcZ. <> 0
          llInvalidStyle = .T.
          SELECT (loParentForm.lcRejSty)  
          APPEND BLANK
          REPLACE Style    WITH EVALUATE(loParentForm.lcInport+'.STYLE') ,;
		          WareCode WITH PADR(loParentForm.ariaform1.kblocation.keytextbox.Value,6) ,;
			      Reason   WITH LANG_SCALECNFLCT      ,;
          	      cdesc    WITH IIF(gfSEEK(PADR(loParentForm.ariaform1.kblocation.keytextbox.Value,6),"WAREHOUS"),Warehous.cdesc,"")       	    
           EXIT   
         ENDIF 
      ENDFOR 
    ENDIF     
  ELSE
    llInvalidStyle = .T.
    SELECT (loParentForm.lcRejSty)  
    APPEND BLANK
    REPLACE Style    WITH EVALUATE(loParentForm.lcInport+'.STYLE') ,;
	        WareCode WITH PADR(loParentForm.ariaform1.kblocation.keytextbox.Value,6) ,;
	        Reason   WITH IIF(loParentForm.llMatModul,LANG_Item_Title,LANG_LOCKING_STYLETITLE)+LANG_LOCKING_NOTINARIA,;
            cdesc    WITH IIF(gfSEEK(PADR(loParentForm.ariaform1.kblocation.keytextbox.Value,6),"WAREHOUS"),Warehous.cdesc,"")       	    
    
  ENDIF    
  IF llInvalidStyle 
    LOOP
  ENDIF 
  IF SEEK(lcKey+lcBatchno+&lcImportFile..STYLE,loParentForm.lcBatLin,loParentForm.lcBatInd)
    llUpdated = .T.
    FOR lnCnt =1 TO Scale.cnt
      lcCnt = STR(lnCnt,1)
      REPLACE STK&lcCnt. WITH &lcImportFile..STK&lcCnt. IN (loParentForm.lcBatLin)
    ENDFOR   
    IF loParentForm.llMatModul
      REPLACE OnHand WITH STK1+STK2+STK3+STK4+STK5+STK6+STK7+STK8 IN (loParentForm.lcBatLin)
    ELSE  
      REPLACE TOTSTK WITH STK1+STK2+STK3+STK4+STK5+STK6+STK7+STK8 IN (loParentForm.lcBatLin)
    ENDIF 
    IF SEEK(&lcImportFile..STYLE,loParentForm.lcTmpQuery) 
      REPLACE STOCK WITH EVALUATE(loParentForm.lcBatLin+'.'+IIF(loParentForm.llMatModul,'OnHand','TOTSTK')) IN (loParentForm.lcTmpQuery)
    ENDIF
  ELSE
    SELECT (loParentForm.lcRejSty)
    APPEND BLANK 
    REPLACE STYLE    WITH &lcImportFile..STYLE,;
		    REASON   WITH IIF(loParentForm.llMatModul,LANG_Item_Title,LANG_LOCKING_STYLETITLE)+ LANG_LOCKING_NOTINBATCH,;
		    WARECODE WITH PADR(loParentForm.ariaform1.kblocation.keytextbox.Value,6),;
			CDESC    WITH IIF(gfSEEK(PADR(loParentForm.ariaform1.kblocation.keytextbox.Value,6),"WAREHOUS"),Warehous.cdesc,"")       	    
  ENDIF
ENDSCAN 

SELECT (loParentForm.lcRejSty)
LOCATE 
IF !EOF()
  loFormSet = loParentForm
  lnAnswer = gfModalGen("QRM00000B42017","DIALOG",.F.,.F.,LANG_One_More + lcStyPic +LANG_Rej_Sty)
  =lfPrnRep()
ENDIF
IF llUpdated 
  IF loParentForm.ariaform1.txtStatus.Value     <> 'Hold'    
    loParentForm.ariaform1.txtStatus.Value     = 'Open'    
  ENDIF
  SELECT (loParentForm.lcBatHdr)
  LOCATE 
  loParentForm.lcStatus = IIF(loParentForm.lcStatus  = 'H','H','M')
  REPLACE Type WITH  loParentForm.lcStatus 
  loParentForm.ariaFORM1.grditems.refresh()
ENDIF  
*:**************************************************************************
*:* Name        : lfExporttoExcel
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 02/06/2012
*:* Purpose     : Export Batch Detail to Excel File
*:**************************************************************************
FUNCTION lfExporttoExcel
LPARAMETERS loParentForm
lnCurAlis = SELECT()
lcFileOut = GETFILE('XLS','',LANG_LOCKING_SAVEFILE)
SELECT (loParentForm.lcBatLin)
IF !EMPTY(lcFileOut) AND DIRECTORY(JUSTPATH(lcFileOut))
  COPY TO (lcFileOut) TYPE XLS
  =gfModalGen('INM00000B00000','DIALOG',.F.,.F.,LANG_LOCKING_EXPORTFILE)  
ELSE
  =gfModalGen('TRM00000B00000','DIALOG',.F.,.F.,LANG_LOCKING_INVALIDFILE)  
ENDIF  
SELECT(lnCurAlis)
*!E303058,1 MMT 02/05/2012 Allow user to import excel file after creating batch in locking screen[T20120111.0036][END]