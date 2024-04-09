*:*************************************************************
*! Name     : POUPDATE
*! Purpose  : Save/Update P/O. Save function for POSTREC
*:*************************************************************
*Modifications:
*B604151,1 KHM 01/29/2001 Fix the bug of not calculating the Costs in case
*B604151,1                of changing the gross_price
*E301544,1 KHM 02/01/2001 New sequence number for Return and Int Loc.
*C200162,1 MHM 03/08/2001 Adding custom trigger.
*B604283,1 KHM 03/18/2001 Fix the bug of not allowing to cancel a PO that
*B604283,1                is generated from SO when the SO has a pick ticket.
*C200152,4 AMH 03/20/2001 custom trigger to check if the inter location PO found in 
*C200152,4                open or approved batch .
*B604319,1 KHM 04/03/2001 Fix the bug of Posting wrong GL when the user
*B604319,1                choose to receive automatically without any 
*B604319,1                additional steps.
*C200170,1 AMH 04/08/2001 Issue Inter-Location P/O Batch.
*B604502,1 MHM 06/11/2001 Fix Bug Of chaning the PO status to Complete,if 
*B604502,1                PO was partially received in case Of inter-location
*B604502,1                and calling from PS.
*C102358,4 AMH 06/09/2001 Add issue adornment order for customer JL.
*C102358,5 AMH 06/13/2001 Add trigger to custom issue adornment order for JL.
*C102357,1 AMH 06/21/2001 If Customer J&L save data to BomVar & Custom thread color file.
*C200208,1 AMH 07/26/2001 Save labelling instructions Data for STU.
*C102469,1 AMH 11/04/2001 Save the last landed cost for PO or C/T.
*B605105,1 AMH 11/18/2001 Fix the bug of cancel P/O with open shipment.
*B605456,1 KHM 02/03/2002 Rounding the landed cost by 3.
*B605678,1 AMH 03/12/2002 Fix the bug of uncancel P/O don't update wip.
*B605411,1 AMH 04/01/2002 Fix the bug of not update nmatwip in fabric & fabdye files when recv.
*C102563,1 AMH 04/18/2002 Update custom fields for Studio Direct.
*B606028,1 AMH 06/04/2002 Fix the bug of not update nmatwip in fabric & fabdye files when recv.
*C200391,1 KHM 07/30/2002 Not dispalying a message of saving the PO # when coming form NC.
*C200376,1 AMH 07/28/2002 Trigger for customer PUFFA to update the style critical path file.
*B606516,1 KHM 10/03/2002 Fix the bug displaying the wrong printers screen.
*B606579,1 AMH 10/29/2002 Fix the bug of infinit loop while receiving C/T.
*B606076,1 RAE 11/19/2002 Open BOMLINE file in case of Post
*B606700,1 KHM 12/03/2002 Fix the bug of alias not found if the MA not installed.
*E301969,1 AMH 12/22/2002 Release the allocation in case of a PO line has no receiving in case
*E301969,1                of open PO
*C200444,1 ALB 12/23/2002 Add GL entries when recieving PO (Customization for UK))
*B606760,1 KHM 01/09/2003 Fix the problem of performance (speed).
*B606743,1 WAB 04/07/2003 WO and WIP figures in Cut and Sold using dyelots    
*B607187,1 KHM 05/28/2003 Fix the bug of not being able to add note pad in case
*B607187,1                of generate po cost sheet setup is automatic and always.
*E302213,1 WAM 08/04/2003 Add 850 record for PO sent to EDI Vendor
*B120365,1 ABD 12/10/2003 Fix Bug that Changes do not save in Style PO after edit and save for 2 times.
*B121328,1 NNA 01/28/2004 Fix Bug That Complete style PO does not update posln.date when we have a style PO 
*B121328,1                and some quantities have been received. then we canceled the open quantities
*B121328,1                POSLN.DATE is not updated for this cancellation.
*E037853,1 HBG 16/02/2004 Change the width of Key field in EDITRANS to 40 char
*C037345,1 ABD 02/24/2004 Trigger for customer PUFFA to update the style critical path file.
*B121348,1 ABD 02/24/2004 Add trigger to delete the related record for C200147.
*C037816,1 MHM 04/06/2004 Custom Bin Location For David Luke
*C038291,1 TMI 08/05/2004 Save a line in POINS.DBF and in POSPEC.DBF file
*B124614,1 NNA 10/24/2004 Fix bug that when adding shipment to a batch and approve that batch then
*B124614,1 NNA            reciving it.completly the shipment status does not change to complete.
*B038753,1 KHM 12/18/2004 Use the style TotCost instead of transaction cost in case of style
*B038753,1 KHM            costing method is standard 'S'
*C123853,1 MHM 01/15/2005 modification in Custom Bin Location For David Luke
*C128583,4 TMI 09/01/2005 Include Batch type from shipment
*C127341,14TMI 09/26/2005 Update the lcWarecode variable due to a problem caused by the entry "B039484"
*B128128,1 HBG 11/20/2005 Update GLDist with the variance if the cost is changed while create the PO and cost method is standard
*B607795,1 TMI 10/08/2006 Fix a bug that when receive a CT to multiple WH the open qty is wrong calculated in MFRCVQ screen (T20061003.0005)
*B608056,1 TMI 04/22/2007 Cannot over receive for the interlocation PO                (T20070413.0018)
*B608077,1 NNA 05/11/2007 Fix bug that Cancelling a Style PO which has some sizes overreceived updates 
*B608077,1 NNA            Style WIP incorrectly                                       (T20070430.0014)
*B608121,1 TMI 06/12/2007 fix a bug that "Syntax Error on Posting Receiving" (T20070605.0001)
*:*********************************************************************************************

*:*************************************************************
*! Name     : gfSavePo
*! Purpose  : Save/Update P/O.
*:*************************************************************
FUNCTION gfSavePo
*E301484,1 WAM Add new parameter setup with .T. if called from EDI

*C200391,1 KHM 07/30/2002 (Begin) Adding a new parameter to do not display the message when
*C200391,1                saving the inter-location PO and coming from NC module.
*PARAMETERS llFromEdi
PARAMETERS llFromEdi,llDispMsg
*C200391,1 KHM 07/30/2002 (End)
*B122770,1 Inialize the neede variables for the lfvequcost [Begin]
IF llFromEdi
  lcIType1   = gfGetMemVar('M_cIType1')
  lcIType2   = gfGetMemVar('M_cIType2')
  lcIType3   = gfGetMemVar('M_cIType3')
  lcIType4   = gfGetMemVar('M_cIType4')
  lcIType5   = gfGetMemVar('M_cIType5')      
  lcCur1  = POSHDR.cPriceCur
  lcCur2  = POSHDR.cDutyCur
ENDIF 
*B122770,1 Inialize the neede variables for the lfvequcost [End  ]

STORE .T. TO llCSave,llCUpdate,llUpdated,llShow
IF EMPTY(laData[2])
  *-You have to enter the xxx code.
  *E301291,1 AMM Consider the case of adornment order
  *=gfModalGen('TRM34020B34000','DIALOG',IIF(lcAType='N','source location','vendor')+'|'+' ')
  =gfModalGen('TRM34020B34000','DIALOG',IIF(lcAType$'NA','source location','vendor')+'|'+' ')
  *E301291,1 AMM end
  _CUROBJ = OBJNUM(laData[2])
  STORE .F. TO llShow,llCSave
  RETURN
ENDIF

IF !llMultiWare AND ( (lnShpLoc=1 AND EMPTY(laData[19])) OR ;
                      (lnShpLoc=2 AND EMPTY(laData[69])) )
  *-You have to enter the xxx code.
  =gfModalGen('TRM34020B34000','DIALOG','ship to location or customer'+'|'+' ')
  _CUROBJ = IIF(lnShpLoc=1,OBJNUM(laData[19]),OBJNUM(laData[69]))
  STORE .F. TO llShow,llCSave
  RETURN
ENDIF

SELECT (lcPoLine)
LOCATE FOR TotQty=0
IF FOUND()
  *-One or more lines has zero quantity,Ignore this lines.
  IF gfModalGen('TRM34052B34001','DIALOG')=1
    DELETE REST FOR TotQty=0
  ELSE
    SELECT POSHDR
    STORE .F. TO llShow,llCSave
    RETURN .F.
  ENDIF
ENDIF
GO TOP
IF EOF()
  *-The lines for this '+lcPOType+' are missing ! cannot update.
  =gfModalGen('TRM34017B34000','DIALOG',lcPOType)
  SELECT POSHDR
  STORE .F. TO llShow,llCSave
  RETURN .F.
ENDIF

*B120365,1 ABD - Replace field nSteps with Zero to save any changes after edit and save the new modification again. [Begin]
REPLACE ALL &lcPOLine..nSteps WITH 0
LOCATE
*B120365,1 ABD - [End]

*C200162,1 MHM 03/08/2001 (Begin) Check if the balance qty is not equal to
*C200162,1                the plan qty then do not save the inter-location PO
IF lcAType = 'N'
  IF !llFromEdi AND ASCAN(laEvntTrig , PADR('CHECKQTY',10)) <> 0 AND llCalFrmOp
    llRetrn = .F.
    =gfDoTriger('POINTRC',PADR('CHECKQTY',10))
    IF llRetrn 
      STORE .F. TO llShow,llCSave ,llRetrn
      RETURN
    ELSE
      STORE .T. TO llShow,llCSave
    ENDIF
  ENDIF     
ENDIF
*C200162,1 MHM 03/08/2001 (End)


IF llMultiWare
  IF llWarehous
    LOCATE FOR EMPTY(cWareCode)
  ELSE 
    LOCATE FOR EMPTY(cWareCode) AND !EMPTY(Account)
  ENDIF
  IF FOUND()
    *--Some lines has an empty ship to,Please enter the
    *--ship to Location/Customer before saving.
    =gfModalGen('TRM34111B34000','DIALOG')
    SELECT POSHDR
    STORE .F. TO llShow,llCSave
    RETURN .F.
  ENDIF
  *--If shipto customer and not multi replace warehouse with DropShip ware.
  SELECT (lcPoLine)
  IF llWarehous
    REPLACE ALL cWareCode WITH lcDropLoc FOR !EMPTY(Account)
  ELSE
    REPLACE ALL cWareCode WITH lcDropLoc
  ENDIF
ELSE
  *--If shipto customer and not multi replace warehouse with DropShip ware.
  IF lnShpLoc=2
    laData[19] = lcDropLoc
    SELECT (lcPOhdr)
    REPLACE cWareCode WITH lcDropLoc
    SELECT (lcPoLine)
    REPLACE ALL cWareCode WITH lcDropLoc
  ENDIF
ENDIF

IF !llWareHous
  *--Single warehouse setup, header warehouse allways default. 
  laData[19] = lcDropLoc
  SELECT (lcPOhdr)
  REPLACE cWareCode WITH lcDropLoc
ENDIF

*--Do you wish to check Production against Distribution?
*--             [Yes/No ]
*E301291,1 AMM Consider the case of adornment order
*IF lcAType = 'N' AND !EMPTY(laData[71]) AND ;
       gfModalGen('QRM34113B34001','DIALOG') = 1
IF lcAType $ 'NA' AND !EMPTY(laData[71]) AND ;
       gfModalGen('QRM34113B34001','DIALOG') = 1
*E301291,1 AMM end
  IF lfChkProd()
    SELECT POSHDR
    STORE .F. TO llShow,llCSave
    RETURN .F.
  ENDIF
ENDIF

lcSign  =IIF(lcAType='R','-','+')
lcOpSign=IIF(lcAType='R','+','-')

DO CASE
  *-- Add New P/O.
  CASE laScrMode[4]
    IF llRecalTot
      =lfRecalc()
    ENDIF

    *--Sequence based on division.
    *E301484,1 WAM Do not generae new number if called from EDI
    *lcSeqDiv  = IIF(gfGetMemVar('M_DIV_SEQ')='Y',laData[4],'') 
    lcSeqDiv  = IIF(llFromEdi,lcSeqDiv,IIF(gfGetMemVar('M_DIV_SEQ')='Y',laData[4],'') )
    *E301484,1 (End)
    
    IF llContinue AND !EMPTY(lcSvdPO)
       laData[1] = lcSvdPO
    ELSE    
       *C101929,1 SSH 06/09/00  [Start Generate separate sequance no for adornment order,Dyeorder or PO.
       *laData[1] = IIF(!llGenOrNum,gfSequence('PO','','',lcSeqDiv),lfGtOrder())
       DO CASE
         CASE  lcAType = "D"  && Dye Order
           laData[1] = IIF(!llGenOrNum,gfSequence('PODY','','',lcSeqDiv),lfGtOrder())
         CASE  lcAType = "A"  && Adornment order
           laData[1] = IIF(!llGenOrNum,gfSequence('POAD','','',lcSeqDiv),lfGtOrder())

         *E301544,1 KHM 02/01/2001 (Begin) Adding a new sequence for the Retrun
         *E301544,1                and Inter location PO.
         CASE  lcAType = "R"  && Return Purchase Order
           laData[1] = IIF(!llGenOrNum,gfSequence('CPORET','','',lcSeqDiv),lfGtOrder())

         CASE  lcAType = "N"  && Inter Location Purchase Order
           laData[1] = IIF(llFromEdi,laData[1],IIF(!llGenOrNum,gfSequence('CPOINTRLOC','','',lcSeqDiv),lfGtOrder()))
         *E301544,1 KHM 02/01/2001 (End)
      
         
         OTHERWISE
           *E301484,1 Do not generate new sequence # if called from EDI
           *laData[1] = IIF(!llGenOrNum,gfSequence('PO','','',lcSeqDiv),lfGtOrder())           
           laData[1] = IIF(llFromEdi AND lcAType = "N",laData[1],IIF(!llGenOrNum,gfSequence('PO','','',lcSeqDiv),lfGtOrder()))
       ENDCASE
       lcSvdPO   = laData[1]
       *C101929,1 SSH 06/09/00  END]
    ENDIF
    *--Create uncmsess record for Saving.
    *E301484,1 WAM Do not call uncompleted session function if called from EDI
    *=lfCrUnSess('pbSav','O') 
    =IIF(llFromEdi,.T.,lfCrUnSess('pbSav','O') )
    *E301484,1 (End)
    
    *E301291,1 AMM update the temporary file with the generated PO number
    IF lcAType = 'A'
      SELECT (lcT_BomVar)
      REPLACE ALL cCost_ID  WITH laData[1]
    ENDIF
    *E301291,1 AMM end
    
    SHOW GET laData[1] DISABLE
    SELECT (lcPOhdr)
    GATHER FROM laData FIELDS &lcScFields MEMO

    SELECT (lcPOline)
    GO TOP
    lnOpnPo = 0 
    IF lcAType='C' OR laData[3]='B'
      REPLACE ALL PO WITH laData[1]
    ENDIF
    DO WHILE !EOF() AND lcAType<>'C' AND laData[3]<>'B'
      WAIT WINDOW 'Updating the WIP for style:'+STYLE NOWAIT
      REPLACE PO WITH laData[1]

      SELECT STYLE

      *C102357,1 AMH If Customer J&L don't update wip for adornment order 
      *C102357,1 that has been created from Adornment order screen [Start]
      *IF lcAType <> 'N' AND SEEK( &lcPOline..STYLE )
      IF lcAType <> 'N' AND SEEK( &lcPOline..STYLE ) AND ;
         IIF(!llFromEdi AND lcAType='A' AND ASCAN(laEvntTrig , PADR('DOSCRN',10)) <> 0,.F.,.T.)
      *C102357,1 AMH [End]
        IF &lcPOLine..nSteps < 1
          = RLOCK()
          REPLACE WIP1 WITH WIP1 &lcSign &lcPOline..QTY1,WIP2 WITH WIP2 &lcSign &lcPOline..QTY2,;
                  WIP3 WITH WIP3 &lcSign &lcPOline..QTY3,WIP4 WITH WIP4 &lcSign &lcPOline..QTY4,;
                  WIP5 WITH WIP5 &lcSign &lcPOline..QTY5,WIP6 WITH WIP6 &lcSign &lcPOline..QTY6,;
                  WIP7 WITH WIP7 &lcSign &lcPOline..QTY7,WIP8 WITH WIP8 &lcSign &lcPOline..QTY8,;
                  TotWip WITH Wip1+Wip2+Wip3+Wip4+Wip5+Wip6+Wip7+Wip8
          UNLOCK
          SELECT (lcPOLine)
          REPLACE nSteps WITH 1
      
          *B606760,1 KHM 01/09/2003 (Begin) Commented out because it slows the program.
          *FLUSH
          *B606760,1 KHM 01/09/2003 (End)
      
        ENDIF
        IF &lcPOLine..nSteps < 2 AND lcAType <> 'A'
          SELECT STYLE
          =RLOCK()
          REPLACE nWO1 WITH nWO1 &lcSign &lcPOline..QTY1,nWO2 WITH nWO2 &lcSign &lcPOline..QTY2,;
                  nWO3 WITH nWO3 &lcSign &lcPOline..QTY3,nWO4 WITH nWO4 &lcSign &lcPOline..QTY4,;
                  nWO5 WITH nWO5 &lcSign &lcPOline..QTY5,nWO6 WITH nWO6 &lcSign &lcPOline..QTY6,;
                  nWO7 WITH nWO7 &lcSign &lcPOline..QTY7,nWO8 WITH nWO8 &lcSign &lcPOline..QTY8,;
                  nTotWO WITH nWO1+nWO2+nWO3+nWO4+nWO5+nWO6+nWO7+nWO8
          UNLOCK
          SELECT (lcPOLine)
          REPLACE nSteps WITH 2
      
          *B606760,1 KHM 01/09/2003 (Begin) Commented out because it slows the program.
          *FLUSH
          *B606760,1 KHM 01/09/2003 (End)
      
        ENDIF
        *--Call TraceKey global function.
        =gfTraceKey('STYLE',STYLE.Style,'M')
      ENDIF
      
     
      IF llMultiWare
        lcSeekCond = "&lcPOline..STYLE+&lcPOline..CWARECODE+SPACE(10)"
      ELSE
        lcSeekCond = "&lcPOline..STYLE+laData[19]+SPACE(10)"
      ENDIF
      SELECT STYDYE
      *C102357,1 AMH If Customer J&L don't update wip for adornment order [Start]
      *IF SEEK(&lcSeekCond)
      IF SEEK(&lcSeekCond) AND;
         IIF(!llFromEdi AND lcAType='A' AND ASCAN(laEvntTrig , PADR('DOSCRN',10)) <> 0,.F.,.T.)
      *C102357,1 AMH [End]
        IF &lcPOLine..nSteps < 3
          = RLOCK()
          REPLACE WIP1 WITH WIP1 &lcSign &lcPOline..QTY1,WIP2 WITH WIP2 &lcSign &lcPOline..QTY2,;
                  WIP3 WITH WIP3 &lcSign &lcPOline..QTY3,WIP4 WITH WIP4 &lcSign &lcPOline..QTY4,;
                  WIP5 WITH WIP5 &lcSign &lcPOline..QTY5,WIP6 WITH WIP6 &lcSign &lcPOline..QTY6,;
                  WIP7 WITH WIP7 &lcSign &lcPOline..QTY7,WIP8 WITH WIP8 &lcSign &lcPOline..QTY8,;
                  TotWip WITH Wip1+Wip2+Wip3+Wip4+Wip5+Wip6+Wip7+Wip8
          UNLOCK
          SELECT (lcPOLine)
          REPLACE nSteps WITH 3
          
          *B606760,1 KHM 01/09/2003 (Begin) Commented out because it slows the program.
          *FLUSH
          *B606760,1 KHM 01/09/2003 (End)
          
        ENDIF
        IF &lcPOLine..nSteps < 4 AND lcAType <> 'A'
          SELECT STYDYE
          =RLOCK()
          REPLACE nWO1 WITH nWO1 &lcSign &lcPOline..QTY1,nWO2 WITH nWO2 &lcSign &lcPOline..QTY2,;
                  nWO3 WITH nWO3 &lcSign &lcPOline..QTY3,nWO4 WITH nWO4 &lcSign &lcPOline..QTY4,;
                  nWO5 WITH nWO5 &lcSign &lcPOline..QTY5,nWO6 WITH nWO6 &lcSign &lcPOline..QTY6,;
                  nWO7 WITH nWO7 &lcSign &lcPOline..QTY7,nWO8 WITH nWO8 &lcSign &lcPOline..QTY8,;
                  nTotWO WITH nWO1+nWO2+nWO3+nWO4+nWO5+nWO6+nWO7+nWO8
          UNLOCK
          SELECT (lcPOLine)
          REPLACE nSteps WITH 4
      
          *B606760,1 KHM 01/09/2003 (Begin) Commented out because it slows the program.
          *FLUSH
          *B606760,1 KHM 01/09/2003 (End)
      
        ENDIF
        *--Call TraceKey global function.
        =gfTraceKey('STYDYE',STYDYE.style+STYDYE.cwarecode+STYDYE.dyelot,'M')
      ENDIF

      *--Update WIP and WO for Target location as -ve.
      SELECT STYDYE
      IF lcAType = 'N' AND SEEK(&lcPOline..STYLE+PADR(laData[2],6)+SPACE(10))
        IF &lcPOLine..nSteps < 5
          = RLOCK()
          REPLACE WIP1 WITH WIP1 - &lcPOline..QTY1,WIP2 WITH WIP2 - &lcPOline..QTY2,;
                  WIP3 WITH WIP3 - &lcPOline..QTY3,WIP4 WITH WIP4 - &lcPOline..QTY4,;
                  WIP5 WITH WIP5 - &lcPOline..QTY5,WIP6 WITH WIP6 - &lcPOline..QTY6,;
                  WIP7 WITH WIP7 - &lcPOline..QTY7,WIP8 WITH WIP8 - &lcPOline..QTY8,;
                  TotWip WITH Wip1+Wip2+Wip3+Wip4+Wip5+Wip6+Wip7+Wip8
          UNLOCK
          SELECT (lcPOLine)
          REPLACE nSteps WITH 5
      
          *B606760,1 KHM 01/09/2003 (Begin) Commented out because it slows the program.
          *FLUSH
          *B606760,1 KHM 01/09/2003 (End)
      
        ENDIF
        IF &lcPOLine..nSteps < 6
          SELECT STYDYE
          =RLOCK()
          REPLACE nWO1 WITH nWO1 - &lcPOline..QTY1,nWO2 WITH nWO2 - &lcPOline..QTY2,;
                  nWO3 WITH nWO3 - &lcPOline..QTY3,nWO4 WITH nWO4 - &lcPOline..QTY4,;
                  nWO5 WITH nWO5 - &lcPOline..QTY5,nWO6 WITH nWO6 - &lcPOline..QTY6,;
                  nWO7 WITH nWO7 - &lcPOline..QTY7,nWO8 WITH nWO8 - &lcPOline..QTY8,;
                  nTotWO WITH nWO1+nWO2+nWO3+nWO4+nWO5+nWO6+nWO7+nWO8
          UNLOCK
          SELECT (lcPOLine)
          REPLACE nSteps WITH 6
      
          *B606760,1 KHM 01/09/2003 (Begin) Commented out because it slows the program.
          *FLUSH
          *B606760,1 KHM 01/09/2003 (End)
      
        ENDIF
        *--Call TraceKey global function.
        =gfTraceKey('STYDYE',STYDYE.style+STYDYE.cwarecode+STYDYE.dyelot,'M')
      ENDIF

      
      SELECT (lcPOline)
      lnOpnPo = lnOpnPo  &lcSign (TOTQTY*neCost1)
      SKIP
    ENDDO

    WAIT WINDOW "Updating "+lcPOType+" information..." NOWAIT

    lnLines=0
    *--Update POSLN file. 
    SELECT (lcPOline)
    SCAN
      lnLines=lnLines+1
      SCATTER MEMVAR
      SELECT POSLN

      *B602657,1 Update cStyGrade field [Begin
      *B802130,1 Comment out.
      *m.LineNo = lnLines
      *B802130,1 End.
      m.cStyGrade  = Style.cStyGrade
      *B602657,1 Update cStyGrade field [End

      APPEND BLANK
      GATHER MEMVAR

      **B602657,1 Update cStyGrade field [Begin
      *MAB Comment out the following lines.
      *REPLACE LineNo     WITH lnLines,;
      *        cAdd_User  WITH gcUser_id,;
      *        cAdd_Time  WITH TIME(),;
      *        dAdd_Date  WITH gdSysDate
      *B602657,1 Update cStyGrade field [End
      =gfAdd_Info('POSLN')

      *E301077,8 Don't update odded fields and call global function.[End

      *B602657,1 Update cStyGrade field [End
      *--Call TraceKey global function.
      *=gfTraceKey('POSLN',POSLN.cstytype+POSLN.po+POSLN.style+STR(POSLN.lineno,6)+POSLN.trancd,'A')
      SELECT POSLN
      =gfTraceKey('POSLN',cStyType+Po+cRsession+Shipno+Style+STR(Lineno,6)+Trancd,'A')
    ENDSCAN

    *--Update POSHDR file. 
    IF &lcPOHdr..nSteps < 1
      SELECT POSHDR
      APPEND BLANK 
      GATHER FROM laData FIELDS &lcScFields MEMO

      *E301077,8 Don't update odded fields and call global function.[Begin
      *Odded REPLACE cStyType   WITH lcAType ,;
      *Odded         LastLine   WITH lnLines ,;
      *Odded         lMultiWare WITH llMultiWare,;
      *Odded         cLCStatus  WITH IIF(!EMPTY(laData[13]),'S',' '),;
      *Odded         nCurrUnit  WITH IIF(llMulCurr,lnCurrUnt1,1),;
      *Odded         nDCurUnit  WITH IIF(llMulCurr,lnCurrUnt2,1),;
      *Odded         cAdd_User  WITH gcUser_id,;
      *Odded         cAdd_Time  WITH TIME(),;
      *Odded         dAdd_Date  WITH gdSysDate

      REPLACE cStyType   WITH lcAType ,;
              LastLine   WITH lnLines ,;
              lMultiWare WITH llMultiWare,;
              cLCStatus  WITH IIF(!EMPTY(laData[13]),'S',' '),;
              nCurrUnit  WITH IIF(llMulCurr,lnCurrUnt1,1),;
              nDCurUnit  WITH IIF(llMulCurr,lnCurrUnt2,1)

      =gfAdd_Info('POSHDR')
      *E301077,8 Don't update odded fields and call global function.[End

      SELECT (lcPOHdr)
      REPLACE nSteps WITH 1

      *B606760,1 KHM 01/09/2003 (Begin) Commented out because it slows the program.
      *FLUSH
      *B606760,1 KHM 01/09/2003 (End)

      *--Call TraceKey global function.
      =gfTraceKey('POSHDR',POSHDR.cstytype+POSHDR.po,'A')
    ENDIF 
    *--Update Vendor file. 
    *C200080,1 AMM Add the case of Dye Order
    *IF llApIstall AND lcAType $ 'PR' AND laData[3]<>'B'
    IF llApIstall AND lcAType $ 'PRD' AND laData[3]<>'B'
    *C200080,1 AMM end
      IF &lcPOHdr..nSteps < 2
        SELECT ApVendor
        =RLOCK()
        REPLACE nVenOpnPO WITH nVenOpnPO + lnOpnPo
        UNLOCK
        SELECT (lcPOHdr)
        REPLACE nSteps WITH 2

        *B606760,1 KHM 01/09/2003 (Begin) Commented out because it slows the program.
        *FLUSH
        *B606760,1 KHM 01/09/2003 (End)

        *--Call TraceKey global function.
        =gfTraceKey('APVENDOR',APVENDOR.cvendcode,'M')
      ENDIF
    ENDIF

    WAIT CLEAR
    *B602753,1 HDM 04/07/1999[Start] Stop Calling NotePad Program In lpSavScr as the global save
    *                                Will Call it
    *IF gfModalGen('QRM00271B34001','DIALOG') = 1
      IF &lcPOHdr..nSteps < 3
        *=lfvNote()
        
        *B607187,1 KHM 05/28/2003 (Begin) Using the local note pad function in case of
        *B607187,1                generate PO cost sheet setup is automatic and always.
        *-- "A" -> Always, "T" -> Automatic
        IF lcAType$'PD' AND lcCrtCSH $ "AT" AND gfModalGen('QRM00271B34001','DIALOG') = 1        
          =lfvNote()
        ENDIF  
        *B607187,1 KHM 05/28/2003 (End)
        
        SELECT (lcPOHdr)
        REPLACE nSteps WITH 3

        *B606760,1 KHM 01/09/2003 (Begin) Commented out because it slows the program.
        *FLUSH
        *B606760,1 KHM 01/09/2003 (End)

      ENDIF
    *ENDIF 
    *B602753,1 HDM 04/07/1999[End]
    
    *C200391,1 KHM 07/30/2002 (Begin) Do not dispaly the message if you come from NC and llDispMsg=.T.
    *IF !llGenOrNum
    IF !llGenOrNum AND !llDispMsg
    *C200391,1 KHM 07/30/2002 (End)

      *E301291,1 AMM Consider the case of adornment order
      *lcMsgTxt=IIF(lcAType='C','Contract',IIF(lcAType='R','Return purchase order','Purchase order'))
      lcMsgTxt=IIF(lcAType='C','Contract',IIF(lcAType='R','Return purchase order',;
               IIF(lcAType='D','Dye order',IIF(lcAType='A','Adornment order','Purchase order'))))
      *E301291,1 AMM end
      =gfModalGen('INM42085B42000','DIALOG',lcMsgTxt+'|'+laData[1])
    ENDIF

    *E301484,1 WAM Send Inter Location PO or Purchase orders to sites
    =lfSendEdi(lcAType,'850')
    *E301484,1 WAM (End)

  *-- Modify P/O.
  CASE laScrMode[3]  
    WAIT WINDOW "Updating "+lcPOType+" information..." NOWAIT

    *--Create uncmsess record for Saving.
    *=lfCrUnSess('pbSav','O') 
    =IIF(llFromEdi,.T.,lfCrUnSess('pbSav','O'))

    SELECT POSHDR
    =SEEK(lcAType+laData[1])
    llChgStatus=IIF(POSHDR.Status='B' AND laData[3]<>'B' ,.T.,.F.)
    lnOpnAmt=0

    *B603298,1 Start, define Gl Variables since removing the line will
    *B603298,1 that has issue qty will put the qty back to stock.
    IF lcAType $ 'NA' AND llLinkToGl
      STORE '' TO lcGLFYear,lcGLPeriod
      lcGlDist = gfTempName()
      = gfOpenFile(gcDataDir+'GLDIST','GlDistAc','SH')
      SELECT GLDist
      =AFIELDS(laFStru)
      =gfCrtTmp(lcGlDist,@laFStru)
    ENDIF
    lcGlSession = ''
    *B603298,1 End.

    *--Update POSLN file. 
    *-Deleted lines
    SELECT POSLN
    =SEEK(lcAType+laData[1])
    SCAN WHILE cStyType+PO = lcAType+laData[1]
      
      *B606760,1 KHM 01/09/2003 (Begin) Added
      WAIT WINDOW 'Updating style:'+STYLE NOWAIT
      *B606760,1 KHM 01/09/2003 (End)
      
      *--Check existance of the record.
      IF SEEK(POSLN.Style+STR(POSLN.Lineno,6),lcPOLine)
        LOOP
      ENDIF  
      
      
      *B603298,1 Start, If delete an issued line Inter location or Adornment order.
      *--Delete line has issed qty. 
      IF lcAType $ 'NA' AND POSLN.TranCd = '6' AND POSLN.TotQty<>0
        *--Return issued stock back to inventory.
        IF llLinkToGl AND EMPTY(lcGLFYear) AND !CHECKPRD(gdSysDate,'lcGLFYear','lcGLPeriod','PO')
          LOOP
        ENDIF 
        *--G/L Array difinition and initialization.
        IF llLinkToGl
          DECLARE laGLDistAr[2,13]
          laGLDistAr[1,1] = IIF(SEEK(Style+PADR(Vendor,6)+SPACE(10),'STYDYE') AND ;
                                !EMPTY(STYDYE.Gl_link),STYDYE.Gl_link,'DEFDEF')
          laGLDistAr[2,1] = IIF(!EMPTY(POSHDR.Link_Code),POSHDR.Link_Code,'DEFDEF')
          laGLDistAr[1,2] = '006'
          laGLDistAr[2,2] = '013'
          laGLDistAr[1,3] = -1
          laGLDistAr[2,3] = 1
          STORE 'IA' TO laGLDistAr[1,4],laGLDistAr[2,4]
          STORE POSLN.PO   TO laGLDistAr[1,5],laGLDistAr[2,5]
          STORE POSLN.Date TO laGLDistAr[1,6],laGLDistAr[2,6]
          STORE gdSysDate  TO laGLDistAr[1,6],laGLDistAr[2,6]
          STORE lcGLFYear  TO laGLDistAr[1,7],laGLDistAr[2,7]
          STORE lcGlPeriod TO laGLDistAr[1,8],laGLDistAr[2,8]
          STORE lcGlDist   TO laGLDistAr[1,9],laGLDistAr[2,9]
          laGLDistAr[2,10] = ''
        ELSE
          DIME laGLDistAr[1,1]
          laGLDistAr = ''
        ENDIF
        lcGlSession = IIF(EMPTY(lcGlSession),gfsequence('GLSESSION'),lcGlSession)
     
        SELECT POSLN
        DECLARE laAdjust[9]
        SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty TO laAdjust

        =gfStyCrl('1',Style,PADR(Vendor,6),Dyelot,gdSysDate,PO,@laAdjust,nCost1,;
                 'Reverse issued P/O',lcGlSession,'',0,'','',@laGLDistAr)

        *C102357,1 AMH trigger to custom remove issued line from adornment order for JL [Start]
        IF !llFromEdi AND lcAType='A' AND ASCAN(laEvntTrig , PADR('REMLINE',10)) <> 0
          =gfDoTriger('POADPO',PADR('REMLINE',10))
        ENDIF
        *C102357,1 AMH [End]

        IF lcAType = 'A'
          *--Delete the issued line too.
          =RLOCK()
          REPLACE Reference WITH 'DELETED'
          UNLOCK
          *--Call TraceKey global function.
          =gfTraceKey('POSLN',cStyType+Po+cRsession+Shipno+Style+STR(Lineno,6)+Trancd,'D')
          LOOP      
        ENDIF
      ENDIF
      *B603298,1 End.
     
      *--Use cLok_time field in POSLN insted of nSteps in lcPOLine
      *--for uncomplete session since the lcPOLine line was deleted.
      
      *--Continue for Seek and Deleted.
      IF lcAType<>'C' AND laData[3]<>'B' AND !llChgStatus
        lnOpnAmt=lnOpnAmt &lcOpSign (POSLN.TotQty * POSLN.neCost1)
        *C200080,1 AMM Add the case of Dye Order
        *IF lcAType='P' AND laData[3]='O' AND VAL(POSLN.cLok_time) < 1
        IF lcAType$'PD' AND laData[3]='O' AND VAL(POSLN.cLok_time) < 1
        *C200080,1 AMM end
          = lfUpdBom('D')
          SELECT POSLN
          =RLOCK()
          REPLACE POSLN.cLok_time WITH '1'
          UNLOCK
        ENDIF

        SELECT STYLE
        IF lcAType <> 'N' AND SEEK( POSLN.STYLE )
          IF VAL(POSLN.cLok_time) < 2
            = RLOCK()
            REPLACE WIP1 WITH WIP1 &lcOpSign POSLN.QTY1,WIP2 WITH WIP2 &lcOpSign POSLN.QTY2,;
                    WIP3 WITH WIP3 &lcOpSign POSLN.QTY3,WIP4 WITH WIP4 &lcOpSign POSLN.QTY4,;
                    WIP5 WITH WIP5 &lcOpSign POSLN.QTY5,WIP6 WITH WIP6 &lcOpSign POSLN.QTY6,;
                    WIP7 WITH WIP7 &lcOpSign POSLN.QTY7,WIP8 WITH WIP8 &lcOpSign POSLN.QTY8,;
                    TotWip WITH Wip1+Wip2+Wip3+Wip4+Wip5+Wip6+Wip7+Wip8
            UNLOCK
            SELECT POSLN
            = RLOCK()
            REPLACE POSLN.cLok_time WITH '2'
            UNLOCK
          ENDIF
          IF VAL(POSLN.cLok_time) < 3 AND lcAType <> 'A'
            SELECT STYLE
            = RLOCK()
            REPLACE nWO1 WITH nWO1 &lcOpSign POSLN.QTY1,nWO2 WITH nWO2 &lcOpSign POSLN.QTY2,;
                    nWO3 WITH nWO3 &lcOpSign POSLN.QTY3,nWO4 WITH nWO4 &lcOpSign POSLN.QTY4,;
                    nWO5 WITH nWO5 &lcOpSign POSLN.QTY5,nWO6 WITH nWO6 &lcOpSign POSLN.QTY6,;
                    nWO7 WITH nWO7 &lcOpSign POSLN.QTY7,nWO8 WITH nWO8 &lcOpSign POSLN.QTY8,;
                    nTotWO WITH nWO1+nWO2+nWO3+nWO4+nWO5+nWO6+nWO7+nWO8
            UNLOCK
            SELECT POSLN
            = RLOCK()
            REPLACE POSLN.cLok_time WITH '3'
            UNLOCK
          ENDIF
          *--Call TraceKey global function.
          =gfTraceKey('STYLE',STYLE.Style,'M')
        ENDIF
        
        
        IF llMultiWare
          lcSeekCond = "POSLN.STYLE+POSLN.CWARECODE+SPACE(10)"
        ELSE
          lcSeekCond = "POSLN.STYLE+laData[19]+SPACE(10)"
        ENDIF
        SELECT STYDYE
        *B603298,1 Don't reduce the WIP in case of issued line (trancd='6')
        *B603298,1 since it was already reduced in trancd = '1'.
        *IF SEEK(&lcSeekCond)
        IF SEEK(&lcSeekCond) AND POSLN.TranCd <> '6'
        *B603298,1 End.
          IF VAL(POSLN.cLok_time) < 4
            =RLOCK()
            REPLACE WIP1 WITH WIP1 &lcOpSign POSLN.QTY1,WIP2 WITH WIP2 &lcOpSign POSLN.QTY2,;
                    WIP3 WITH WIP3 &lcOpSign POSLN.QTY3,WIP4 WITH WIP4 &lcOpSign POSLN.QTY4,;
                    WIP5 WITH WIP5 &lcOpSign POSLN.QTY5,WIP6 WITH WIP6 &lcOpSign POSLN.QTY6,;
                    WIP7 WITH WIP7 &lcOpSign POSLN.QTY7,WIP8 WITH WIP8 &lcOpSign POSLN.QTY8,;
                    TotWip WITH Wip1+Wip2+Wip3+Wip4+Wip5+Wip6+Wip7+Wip8
            UNLOCK
            SELECT POSLN
            = RLOCK()
            REPLACE POSLN.cLok_time WITH '4'
            UNLOCK
          ENDIF
          IF VAL(POSLN.cLok_time) < 5 AND lcAType <> 'A'
            SELECT STYDYE
            =RLOCK()
            REPLACE nWO1 WITH nWO1 &lcOpSign POSLN.QTY1,nWO2 WITH nWO2 &lcOpSign POSLN.QTY2,;
                    nWO3 WITH nWO3 &lcOpSign POSLN.QTY3,nWO4 WITH nWO4 &lcOpSign POSLN.QTY4,;
                    nWO5 WITH nWO5 &lcOpSign POSLN.QTY5,nWO6 WITH nWO6 &lcOpSign POSLN.QTY6,;
                    nWO7 WITH nWO7 &lcOpSign POSLN.QTY7,nWO8 WITH nWO8 &lcOpSign POSLN.QTY8,;
                    nTotWO WITH nWO1+nWO2+nWO3+nWO4+nWO5+nWO6+nWO7+nWO8
            UNLOCK
            SELECT POSLN
            =RLOCK()
            REPLACE POSLN.cLok_time WITH '5'
            UNLOCK
          ENDIF
          *--Call TraceKey global function.
          =gfTraceKey('STYDYE',STYDYE.style+STYDYE.cwarecode+STYDYE.dyelot,'M')
        ENDIF

        SELECT STYDYE
        IF lcAType = 'N' AND SEEK(POSLN.STYLE+PADR(laData[2],6)+SPACE(10))
          *B603298,1 Reduce the WIP from source location if line is issued.
          lcActSign = IIF(POSLN.TranCd='6','-','+')
          *B603298,1 End.
          IF VAL(POSLN.cLok_time) < 6
            =RLOCK()
            REPLACE WIP1 WITH WIP1 &lcActSign POSLN.QTY1,WIP2 WITH WIP2 &lcActSign POSLN.QTY2,;
                    WIP3 WITH WIP3 &lcActSign POSLN.QTY3,WIP4 WITH WIP4 &lcActSign POSLN.QTY4,;
                    WIP5 WITH WIP5 &lcActSign POSLN.QTY5,WIP6 WITH WIP6 &lcActSign POSLN.QTY6,;
                    WIP7 WITH WIP7 &lcActSign POSLN.QTY7,WIP8 WITH WIP8 &lcActSign POSLN.QTY8,;
                    TotWip WITH Wip1+Wip2+Wip3+Wip4+Wip5+Wip6+Wip7+Wip8
            UNLOCK
            SELECT POSLN
            =RLOCK()
            REPLACE POSLN.cLok_time WITH '6'
            UNLOCK
          ENDIF
          IF VAL(POSLN.cLok_time) < 7
            SELECT STYDYE
            =RLOCK()
            REPLACE nWO1 WITH nWO1 &lcActSign POSLN.QTY1,nWO2 WITH nWO2 &lcActSign POSLN.QTY2,;
                    nWO3 WITH nWO3 &lcActSign POSLN.QTY3,nWO4 WITH nWO4 &lcActSign POSLN.QTY4,;
                    nWO5 WITH nWO5 &lcActSign POSLN.QTY5,nWO6 WITH nWO6 &lcActSign POSLN.QTY6,;
                    nWO7 WITH nWO7 &lcActSign POSLN.QTY7,nWO8 WITH nWO8 &lcActSign POSLN.QTY8,;
                    nTotWO WITH nWO1+nWO2+nWO3+nWO4+nWO5+nWO6+nWO7+nWO8
            UNLOCK
            SELECT POSLN
            =RLOCK()
            REPLACE POSLN.cLok_time WITH '7'
            UNLOCK
          ENDIF
          *--Call TraceKey global function.
          =gfTraceKey('STYDYE',STYDYE.style+STYDYE.cwarecode+STYDYE.dyelot,'M')
        ENDIF

      ENDIF 

      *--Clear allocation for deleted style.
      *B603298,1 start, added case of Adornmet order too.
      *IF llSOInstld AND lcAType='P' AND laData[3] $ 'OH' AND VAL(POSLN.cLok_time) < 8 AND SEEK('2'+laData[1]+POSLN.Style,'CUTPICK')
      IF llSOInstld AND lcAType $ 'PA' AND laData[3] $ 'OH' AND VAL(POSLN.cLok_time) < 8 AND ;
           SEEK('2'+laData[1]+POSLN.Style,'CUTPICK')
      *B603298,1 End.

        *E301077,8 open ordhdr only if not opened.
        IF !laOpFile[4]
          laOpFile[4]= gfOpenFile(gcDataDir+'ORDHDR','ORDHDR','SH')
        ENDIF
        *E301077,8 End.
        
        SELECT CUTPICK
        SCAN REST WHILE TranCd+cTktNo+Style='2'+laData[1]+POSLN.Style
          =SEEK('O'+CUTPICK.Order,'ORDHDR')
          IF SEEK('O'+CUTPICK.Order+STR(INT(VAL(CUTPICK.cOrdLine)),6),'ORDLINE')
            FOR lnSizeNo = 1 TO 8
              lcSz = STR(lnSizeNo,1)
              lcRelQty = Qty&lcSz
              *-Update values so as to be reflected on all involved cutting ticktes.
              REPLACE ORDHDR.TotCut    WITH ORDHDR.TotCut    - lcRelQty
              REPLACE ORDLINE.TotCut   WITH ORDLINE.TotCut   - lcRelQty ,;
                      ORDLINE.Cut&lcSz WITH ORDLINE.Cut&lcSz - lcRelQty
              REPLACE CUTPICK.TotQty   WITH CUTPICK.TotQty   - lcRelQty ,;
                      CUTPICK.Qty&lcSz WITH 0
            ENDFOR

            *--Call TraceKey global function.
            SELECT CUTPICK

            *E301182,7 Adjust trace key master tag [Begin]
            *=gfTraceKey('CUTPICK',Trancd+cTktNo+Order+Style+cOrdline,'M')    
            =lfTraceTag('CUTPICK','M')    
            *E301182,7 Adjust trace key master tag [End  ]

            =gfTraceKey('ORDHDR','O'+CUTPICK.Order,'M')
            =gfTraceKey('ORDLINE','O'+CUTPICK.Order+STR(INT(VAL(CUTPICK.cOrdLine)),6),'M')
          ENDIF
        ENDSCAN        
        =SEEK('2'+laData[1]+POSLN.Style)

        *C102357,1 AMH trigger to custom remove issued line from adornment order for JL [Start]
        IF !llFromEdi AND lcAType='A' AND ASCAN(laEvntTrig , PADR('REMALOLN',10)) <> 0
          =gfDoTriger('POADPO',PADR('REMALOLN',10))
        ENDIF
        *C102357,1 AMH [End]

        *E301182,7 Adjust trace key master tag [Begin]
        *DELETE REST WHILE TranCd+cTktNo+Style='2'+laData[1]+POSLN.Style ;
        *    FOR TotQty = 0 AND gfTraceKey('CUTPICK',Trancd+cTktNo+Order+Style+cOrdline,'D')

        DELETE REST WHILE TranCd+cTktNo+Style='2'+laData[1]+POSLN.Style ;
            FOR TotQty = 0 AND lfTraceTag('CUTPICK','D')

        *E301182,7 Adjust trace key master tag [Begin]

        SELECT POSLN
        =RLOCK()
        REPLACE POSLN.cLok_time WITH '8'
        UNLOCK
      ENDIF

      SELECT POSLN
      IF VAL(POSLN.cLok_time) < 9
        *B602996,1 Control the deletion from the reference field.
        =RLOCK()
        REPLACE Reference       WITH 'DELETED'
        REPLACE POSLN.cLok_time WITH '9'
        UNLOCK
        *--Call TraceKey global function.
        =gfTraceKey('POSLN',cStyType+Po+cRsession+Shipno+Style+STR(Lineno,6)+Trancd,'D')
      ENDIF
    
    ENDSCAN

    *B603298,1 Update and close the gl file.
    IF lcAType $ 'NA' AND llLinkToGl
      SELECT (lcGlDist)
      REPLACE ALL GlSession with lcGlSession
      USE
      SELECT GlDist
      APPEND FROM (gcWorkDir+lcGlDist)
      ERASE (gcWorkDir+lcGLDist+'.DBF')
      ERASE (gcWorkDir+lcGLDist+'.CDX')
    ENDIF
    *B603298,1 End.
    
    *B121348,1 ABD - Add trigger to delete the related record for C200147. [Begin]
    *-- Delete the related record for C200147.
    IF lcAType = 'P' .AND. ASCAN(laEvntTrig , PADR('DELMAPOA',10)) <> 0
      =gfDoTriger('POSTY',PADR('DELMAPOA',10))
    ENDIF
    *B121348,1 ABD -  [End]
    
    *B602996,1 Start.
    SELECT POSLN
    IF SEEK(lcAType+laData[1]) 
      DELETE REST WHILE cStyType+Po = lcAType+laData[1] FOR ALLT(Reference) = 'DELETED'
    ENDIF
    *B602996,1 End.



    SELECT (lcPOline)
    SCAN

      *B606760,1 KHM 01/09/2003 (Begin) Added
      WAIT WINDOW 'Updating style:'+STYLE NOWAIT
      *B606760,1 KHM 01/09/2003 (End)

      SCATTER MEMVAR
      *--New line
      IF !SEEK(lcAType+laData[1]+&lcPOline..Style+STR(&lcPOline..Lineno,6),'POSLN')

        *-- Insert in the PO BOM.
        *C200080,1 AMM Add the case of Dye Order
        *IF lcAType='P' AND laData[3] = 'O' AND &lcPOLine..nSteps < 10
        IF lcAType$'PD' AND laData[3] = 'O' AND &lcPOLine..nSteps < 10
        *C200080,1 AMM end
          IF lfUpdBom('A')
            SELECT (lcPOLine)
            REPLACE nSteps WITH 10
            
            *B606760,1 KHM 01/09/2003 (Begin) Commented out because it slows the program.
            *FLUSH
            *B606760,1 KHM 01/09/2003 (End)
            
          ELSE
            SELECT (lcPOLine)
            REPLACE cRSession WITH 'XXXXXX'
            LOOP
          ENDIF
        ENDIF

        IF lcAType<>'C' AND laData[3]<>'B'
          SELECT STYLE
          IF lcAType <> 'N' AND SEEK( &lcPOline..STYLE)
            IF &lcPOLine..nSteps < 11
              = RLOCK()
              REPLACE WIP1 WITH WIP1 &lcSign &lcPOline..QTY1,WIP2 WITH WIP2 &lcSign &lcPOline..QTY2,;
                      WIP3 WITH WIP3 &lcSign &lcPOline..QTY3,WIP4 WITH WIP4 &lcSign &lcPOline..QTY4,;
                      WIP5 WITH WIP5 &lcSign &lcPOline..QTY5,WIP6 WITH WIP6 &lcSign &lcPOline..QTY6,;
                      WIP7 WITH WIP7 &lcSign &lcPOline..QTY7,WIP8 WITH WIP8 &lcSign &lcPOline..QTY8,;
                      TotWip WITH Wip1+Wip2+Wip3+Wip4+Wip5+Wip6+Wip7+Wip8
              UNLOCK
              SELECT (lcPOLine)
              REPLACE nSteps WITH 11
              
              *B606760,1 KHM 01/09/2003 (Begin) Commented out because it slows the program.
              *FLUSH
              *B606760,1 KHM 01/09/2003 (End)
              
            ENDIF
            IF &lcPOLine..nSteps < 12 AND lcAType <> 'A'
              SELECT STYLE
              = RLOCK()
              REPLACE nWO1 WITH nWO1 &lcSign &lcPOline..QTY1,nWO2 WITH nWO2 &lcSign &lcPOline..QTY2,;
                      nWO3 WITH nWO3 &lcSign &lcPOline..QTY3,nWO4 WITH nWO4 &lcSign &lcPOline..QTY4,;
                      nWO5 WITH nWO5 &lcSign &lcPOline..QTY5,nWO6 WITH nWO6 &lcSign &lcPOline..QTY6,;
                      nWO7 WITH nWO7 &lcSign &lcPOline..QTY7,nWO8 WITH nWO8 &lcSign &lcPOline..QTY8,;
                      nTotWO WITH nWO1+nWO2+nWO3+nWO4+nWO5+nWO6+nWO7+nWO8
              UNLOCK
              SELECT (lcPOLine)
              REPLACE nSteps WITH 12
              
              *B606760,1 KHM 01/09/2003 (Begin) Commented out because it slows the program.
              *FLUSH
              *B606760,1 KHM 01/09/2003 (End)
              
            ENDIF
            *--Call TraceKey global function.
            =gfTraceKey('STYLE',STYLE.Style,'M')
          ENDIF

          IF llMultiWare
            lcSeekCond = "&lcPOline..STYLE+&lcPOline..CWARECODE+SPACE(10)"
          ELSE
            lcSeekCond = "&lcPOline..STYLE+laData[19]+SPACE(10)"
          ENDIF
          SELECT STYDYE
          IF SEEK(&lcSeekCond)
            IF &lcPOLine..nSteps < 13
              =RLOCK()
              REPLACE WIP1 WITH WIP1 &lcSign &lcPOline..QTY1,WIP2 WITH WIP2 &lcSign &lcPOline..QTY2,;
                      WIP3 WITH WIP3 &lcSign &lcPOline..QTY3,WIP4 WITH WIP4 &lcSign &lcPOline..QTY4,;
                      WIP5 WITH WIP5 &lcSign &lcPOline..QTY5,WIP6 WITH WIP6 &lcSign &lcPOline..QTY6,;
                      WIP7 WITH WIP7 &lcSign &lcPOline..QTY7,WIP8 WITH WIP8 &lcSign &lcPOline..QTY8,;
                      TotWip WITH Wip1+Wip2+Wip3+Wip4+Wip5+Wip6+Wip7+Wip8
              UNLOCK
              SELECT (lcPOLine)
              REPLACE nSteps WITH 13
              
              *B606760,1 KHM 01/09/2003 (Begin) Commented out because it slows the program.
              *FLUSH
              *B606760,1 KHM 01/09/2003 (End)
              
            ENDIF
            IF &lcPOLine..nSteps < 14 AND lcAType <> 'A'
              SELECT STYDYE
              = RLOCK()
              REPLACE nWO1 WITH nWO1 &lcSign &lcPOline..QTY1,nWO2 WITH nWO2 &lcSign &lcPOline..QTY2,;
                      nWO3 WITH nWO3 &lcSign &lcPOline..QTY3,nWO4 WITH nWO4 &lcSign &lcPOline..QTY4,;
                      nWO5 WITH nWO5 &lcSign &lcPOline..QTY5,nWO6 WITH nWO6 &lcSign &lcPOline..QTY6,;
                      nWO7 WITH nWO7 &lcSign &lcPOline..QTY7,nWO8 WITH nWO8 &lcSign &lcPOline..QTY8,;
                      nTotWO WITH nWO1+nWO2+nWO3+nWO4+nWO5+nWO6+nWO7+nWO8
              UNLOCK
              SELECT (lcPOLine)
              REPLACE nSteps WITH 14
      
              *B606760,1 KHM 01/09/2003 (Begin) Commented out because it slows the program.
              *FLUSH
              *B606760,1 KHM 01/09/2003 (End)
              
            ENDIF
            *--Call TraceKey global function.
            =gfTraceKey('STYDYE',STYDYE.style+STYDYE.cwarecode+STYDYE.dyelot,'M')
          ENDIF

          SELECT STYDYE
          IF lcAType = 'N' AND SEEK(&lcPOline..STYLE+PADR(laData[2],6)+SPACE(10))
            IF &lcPOLine..nSteps < 15
              =RLOCK()
              REPLACE WIP1 WITH WIP1 - &lcPOline..QTY1,WIP2 WITH WIP2 - &lcPOline..QTY2,;
                      WIP3 WITH WIP3 - &lcPOline..QTY3,WIP4 WITH WIP4 - &lcPOline..QTY4,;
                      WIP5 WITH WIP5 - &lcPOline..QTY5,WIP6 WITH WIP6 - &lcPOline..QTY6,;
                      WIP7 WITH WIP7 - &lcPOline..QTY7,WIP8 WITH WIP8 - &lcPOline..QTY8,;
                      TotWip WITH Wip1+Wip2+Wip3+Wip4+Wip5+Wip6+Wip7+Wip8
              UNLOCK
              SELECT (lcPOLine)
              REPLACE nSteps WITH 15
      
              *B606760,1 KHM 01/09/2003 (Begin) Commented out because it slows the program.
              *FLUSH
              *B606760,1 KHM 01/09/2003 (End)
      
            ENDIF
            IF &lcPOLine..nSteps < 16
              SELECT STYDYE
              = RLOCK()
              REPLACE nWO1 WITH nWO1 - &lcPOline..QTY1,nWO2 WITH nWO2 - &lcPOline..QTY2,;
                      nWO3 WITH nWO3 - &lcPOline..QTY3,nWO4 WITH nWO4 - &lcPOline..QTY4,;
                      nWO5 WITH nWO5 - &lcPOline..QTY5,nWO6 WITH nWO6 - &lcPOline..QTY6,;
                      nWO7 WITH nWO7 - &lcPOline..QTY7,nWO8 WITH nWO8 - &lcPOline..QTY8,;
                      nTotWO WITH nWO1+nWO2+nWO3+nWO4+nWO5+nWO6+nWO7+nWO8
              UNLOCK
              SELECT (lcPOLine)
              REPLACE nSteps WITH 16
      
              *B606760,1 KHM 01/09/2003 (Begin) Commented out because it slows the program.
              *FLUSH
              *B606760,1 KHM 01/09/2003 (End)
              
            ENDIF
            *--Call TraceKey global function.
            =gfTraceKey('STYDYE',STYDYE.style+STYDYE.cwarecode+STYDYE.dyelot,'M')
          ENDIF


          lnOpnAmt=lnOpnAmt &lcSign (&lcPOline..TotQty * &lcPOline..neCost1)
        ENDIF

        SELECT POSLN
        IF &lcPOLine..nSteps < 17
          APPEND BLANK

          *B602657,1 Update cStyGrade field [Begin
          m.cStyGrade = Style.cStyGrade
          *B602657,1 Update cStyGrade field [End

          GATHER MEMVAR

          *E301077,8 Don't update odded fields and call global function.[Begin
          *Odded REPLACE cAdd_User  WITH gcUser_id,;
          *Odded         cAdd_Time  WITH TIME(),;
          *Odded         dAdd_Date  WITH gdSysDate

          =gfAdd_Info('POSLN')

          *C102563,1 AMH Update custom fields for Studio Direct [Start]
          IF !llFromEdi .AND. ASCAN(laEvntTrig , PADR('SVMODFLD',10)) <> 0
            REPLACE LMOD      WITH .T.,;
                    CMOD_USER WITH CADD_USER,;
                    CMOD_DATE WITH DADD_DATE
          ENDIF
          *C102563,1 AMH [End]
      
          *E301077,8 Don't update odded fields and call global function.[End

          SELECT (lcPOLine)
          REPLACE nSteps WITH 17
       
          *B606760,1 KHM 01/09/2003 (Begin) Commented out because it slows the program.
          *FLUSH
          *B606760,1 KHM 01/09/2003 (End)
       
          *--Call TraceKey global function.
          *=gfTraceKey('POSLN',POSLN.cstytype+POSLN.po+POSLN.style+STR(POSLN.lineno,6)+POSLN.trancd,'A')
          SELECT POSLN
          =gfTraceKey('POSLN',cStyType+Po+cRsession+Shipno+Style+STR(Lineno,6)+Trancd,'A')
        ENDIF

      *--Modified line.
      ELSE
        *-- Modify the PO BOM
        *C200080,1 AMM Add the case of Dye Order
        *IF lcAType='P' AND laData[3] = 'O' AND &lcPOLine..nSteps < 10
        IF lcAType$'PD' AND laData[3] = 'O' AND &lcPOLine..nSteps < 10
        *C200080,1 AMM end
          IF lfUpdBom('M')
            
            *B604151,1 KHM 01/29/2001 (Begin) Adding a new function to 
            *B604151,1 KHM 01/29/2001 recalculate the costs.
            =lfReClCost()
            *B604151,1 KHM 01/29/2001 (End)
            
            SELECT (lcPOLine)
            REPLACE nSteps WITH 10
   
            *B606760,1 KHM 01/09/2003 (Begin) Commented out because it slows the program.
            *FLUSH
            *B606760,1 KHM 01/09/2003 (End)
         
          ELSE
            SELECT (lcPOLine)
            REPLACE cRSession WITH 'XXXXXX'
            LOOP
          ENDIF
        ENDIF

        IF lcAType<>'C' AND laData[3]<>'B'
          SELECT STYLE
          IF lcAType <> 'N' AND SEEK( &lcPOline..STYLE)
            IF &lcPOLine..nSteps < 11
              = RLOCK()
              IF llChgStatus
                REPLACE WIP1 WITH WIP1 &lcSign &lcPOline..QTY1,WIP2 WITH WIP2 &lcSign &lcPOline..QTY2,;
                        WIP3 WITH WIP3 &lcSign &lcPOline..QTY3,WIP4 WITH WIP4 &lcSign &lcPOline..QTY4,;
                        WIP5 WITH WIP5 &lcSign &lcPOline..QTY5,WIP6 WITH WIP6 &lcSign &lcPOline..QTY6,;
                        WIP7 WITH WIP7 &lcSign &lcPOline..QTY7,WIP8 WITH WIP8 &lcSign &lcPOline..QTY8,;
                        TotWip WITH Wip1+Wip2+Wip3+Wip4+Wip5+Wip6+Wip7+Wip8
              ELSE 
                REPLACE WIP1 WITH WIP1 &lcSign &lcPOline..QTY1 &lcOpSign POSLN.QTY1,WIP2 WITH WIP2 &lcSign &lcPOline..QTY2 &lcOpSign POSLN.QTY2,;
                        WIP3 WITH WIP3 &lcSign &lcPOline..QTY3 &lcOpSign POSLN.QTY3,WIP4 WITH WIP4 &lcSign &lcPOline..QTY4 &lcOpSign POSLN.QTY4,;
                        WIP5 WITH WIP5 &lcSign &lcPOline..QTY5 &lcOpSign POSLN.QTY5,WIP6 WITH WIP6 &lcSign &lcPOline..QTY6 &lcOpSign POSLN.QTY6,;
                        WIP7 WITH WIP7 &lcSign &lcPOline..QTY7 &lcOpSign POSLN.QTY7,WIP8 WITH WIP8 &lcSign &lcPOline..QTY8 &lcOpSign POSLN.QTY8,;
                        TotWip WITH Wip1+Wip2+Wip3+Wip4+Wip5+Wip6+Wip7+Wip8
              ENDIF
              UNLOCK
              SELECT (lcPOLine)
              REPLACE nSteps WITH 11
      
              *B606760,1 KHM 01/09/2003 (Begin) Commented out because it slows the program.
              *FLUSH
              *B606760,1 KHM 01/09/2003 (End)
      
            ENDIF
            IF &lcPOLine..nSteps < 12 AND lcAType <> 'A'
              SELECT STYLE
              = RLOCK()
              IF llChgStatus
                REPLACE nWO1 WITH nWO1 &lcSign &lcPOline..QTY1,nWO2 WITH nWO2 &lcSign &lcPOline..QTY2,;
                        nWO3 WITH nWO3 &lcSign &lcPOline..QTY3,nWO4 WITH nWO4 &lcSign &lcPOline..QTY4,;
                        nWO5 WITH nWO5 &lcSign &lcPOline..QTY5,nWO6 WITH nWO6 &lcSign &lcPOline..QTY6,;
                        nWO7 WITH nWO7 &lcSign &lcPOline..QTY7,nWO8 WITH nWO8 &lcSign &lcPOline..QTY8,;
                        nTotWO WITH nWO1+nWO2+nWO3+nWO4+nWO5+nWO6+nWO7+nWO8
              ELSE
                REPLACE nWO1 WITH nWO1 &lcSign &lcPOline..QTY1 &lcOpSign POSLN.QTY1,nWO2 WITH nWO2 &lcSign &lcPOline..QTY2 &lcOpSign POSLN.QTY2,;
                        nWO3 WITH nWO3 &lcSign &lcPOline..QTY3 &lcOpSign POSLN.QTY3,nWO4 WITH nWO4 &lcSign &lcPOline..QTY4 &lcOpSign POSLN.QTY4,;
                        nWO5 WITH nWO5 &lcSign &lcPOline..QTY5 &lcOpSign POSLN.QTY5,nWO6 WITH nWO6 &lcSign &lcPOline..QTY6 &lcOpSign POSLN.QTY6,;
                        nWO7 WITH nWO7 &lcSign &lcPOline..QTY7 &lcOpSign POSLN.QTY7,nWO8 WITH nWO8 &lcSign &lcPOline..QTY8 &lcOpSign POSLN.QTY8,;
                        nTotWO WITH nWO1+nWO2+nWO3+nWO4+nWO5+nWO6+nWO7+nWO8
              ENDIF
              UNLOCK
              SELECT (lcPOLine)
              REPLACE nSteps WITH 12
      
              *B606760,1 KHM 01/09/2003 (Begin) Commented out because it slows the program.
              *FLUSH
              *B606760,1 KHM 01/09/2003 (End)
      
            ENDIF
            *--Call TraceKey global function.
            =gfTraceKey('STYLE',STYLE.Style,'M')
          ENDIF

          IF llMultiWare
            lcSeekCond = "&lcPOline..STYLE+&lcPOline..CWARECODE+SPACE(10)"
            lcOldSkKey = "&lcPOline..STYLE+POSLN.cWareCode+SPACE(10)"
          ELSE
            lcSeekCond = "&lcPOline..STYLE+laData[19]+SPACE(10)"
            lcOldSkKey = "&lcPOline..STYLE+ POSHDR.cWareCode+SPACE(10)"
          ENDIF
          SELECT STYDYE
          IF SEEK(&lcSeekCond)
            IF &lcPOLine..nSteps < 13            
              =RLOCK()
              IF llChgStatus OR EVAL(lcSeekCond)<>EVAL(lcOldSkKey)
                REPLACE WIP1 WITH WIP1 &lcSign &lcPOline..QTY1,WIP2 WITH WIP2 &lcSign &lcPOline..QTY2,;
                        WIP3 WITH WIP3 &lcSign &lcPOline..QTY3,WIP4 WITH WIP4 &lcSign &lcPOline..QTY4,;
                        WIP5 WITH WIP5 &lcSign &lcPOline..QTY5,WIP6 WITH WIP6 &lcSign &lcPOline..QTY6,;
                        WIP7 WITH WIP7 &lcSign &lcPOline..QTY7,WIP8 WITH WIP8 &lcSign &lcPOline..QTY8,;
                        TotWip WITH Wip1+Wip2+Wip3+Wip4+Wip5+Wip6+Wip7+Wip8
              ELSE
                REPLACE WIP1 WITH WIP1 &lcSign &lcPOline..QTY1 &lcOpSign POSLN.QTY1,WIP2 WITH WIP2 &lcSign &lcPOline..QTY2 &lcOpSign POSLN.QTY2,;
                        WIP3 WITH WIP3 &lcSign &lcPOline..QTY3 &lcOpSign POSLN.QTY3,WIP4 WITH WIP4 &lcSign &lcPOline..QTY4 &lcOpSign POSLN.QTY4,;
                        WIP5 WITH WIP5 &lcSign &lcPOline..QTY5 &lcOpSign POSLN.QTY5,WIP6 WITH WIP6 &lcSign &lcPOline..QTY6 &lcOpSign POSLN.QTY6,;
                        WIP7 WITH WIP7 &lcSign &lcPOline..QTY7 &lcOpSign POSLN.QTY7,WIP8 WITH WIP8 &lcSign &lcPOline..QTY8 &lcOpSign POSLN.QTY8,;
                        TotWip WITH Wip1+Wip2+Wip3+Wip4+Wip5+Wip6+Wip7+Wip8
              ENDIF
              UNLOCK
              SELECT (lcPOLine)
              REPLACE nSteps WITH 13
      
              *B606760,1 KHM 01/09/2003 (Begin) Commented out because it slows the program.
              *FLUSH
              *B606760,1 KHM 01/09/2003 (End)
      
            ENDIF
            IF &lcPOLine..nSteps < 14  AND lcAType <> 'A'
              SELECT STYDYE
              =RLOCK()
              IF llChgStatus OR EVAL(lcSeekCond)<>EVAL(lcOldSkKey)
                REPLACE nWO1 WITH nWO1 &lcSign &lcPOline..QTY1,nWO2 WITH nWO2 &lcSign &lcPOline..QTY2,;
                        nWO3 WITH nWO3 &lcSign &lcPOline..QTY3,nWO4 WITH nWO4 &lcSign &lcPOline..QTY4,;
                        nWO5 WITH nWO5 &lcSign &lcPOline..QTY5,nWO6 WITH nWO6 &lcSign &lcPOline..QTY6,;
                        nWO7 WITH nWO7 &lcSign &lcPOline..QTY7,nWO8 WITH nWO8 &lcSign &lcPOline..QTY8,;
                        nTotWO WITH nWO1+nWO2+nWO3+nWO4+nWO5+nWO6+nWO7+nWO8
              ELSE
                REPLACE nWO1 WITH nWO1 &lcSign &lcPOline..QTY1 &lcOpSign POSLN.QTY1,nWO2 WITH nWO2 &lcSign &lcPOline..QTY2 &lcOpSign POSLN.QTY2,;
                        nWO3 WITH nWO3 &lcSign &lcPOline..QTY3 &lcOpSign POSLN.QTY2,nWO4 WITH nWO4 &lcSign &lcPOline..QTY4 &lcOpSign POSLN.QTY4,;
                        nWO5 WITH nWO5 &lcSign &lcPOline..QTY5 &lcOpSign POSLN.QTY3,nWO6 WITH nWO6 &lcSign &lcPOline..QTY6 &lcOpSign POSLN.QTY6,;
                        nWO7 WITH nWO7 &lcSign &lcPOline..QTY7 &lcOpSign POSLN.QTY4,nWO8 WITH nWO8 &lcSign &lcPOline..QTY8 &lcOpSign POSLN.QTY8,;
                        nTotWO WITH nWO1+nWO2+nWO3+nWO4+nWO5+nWO6+nWO7+nWO8
              ENDIF
              UNLOCK
              *--Call TraceKey global function.
              =gfTraceKey('STYDYE',STYDYE.style+STYDYE.cwarecode+STYDYE.dyelot,'M')

              IF EVAL(lcSeekCond)<>EVAL(lcOldSkKey)
                =SEEK(&lcOldSkKey)
                =RLOCK() 
                REPLACE WIP1 WITH WIP1 &lcOpSign POSLN.QTY1,WIP2 WITH WIP2 &lcOpSign POSLN.QTY2,;
                        WIP3 WITH WIP3 &lcOpSign POSLN.QTY3,WIP4 WITH WIP4 &lcOpSign POSLN.QTY4,;
                        WIP5 WITH WIP5 &lcOpSign POSLN.QTY5,WIP6 WITH WIP6 &lcOpSign POSLN.QTY6,;
                        WIP7 WITH WIP7 &lcOpSign POSLN.QTY7,WIP8 WITH WIP8 &lcOpSign POSLN.QTY8,;
                        TotWip WITH Wip1+Wip2+Wip3+Wip4+Wip5+Wip6+Wip7+Wip8
                IF lcAType <> 'A'
                  REPLACE nWO1 WITH nWO1 &lcOpSign POSLN.QTY1,nWO2 WITH nWO2 &lcOpSign POSLN.QTY2,;
                          nWO3 WITH nWO3 &lcOpSign POSLN.QTY3,nWO4 WITH nWO4 &lcOpSign POSLN.QTY4,;
                          nWO5 WITH nWO5 &lcOpSign POSLN.QTY5,nWO6 WITH nWO6 &lcOpSign POSLN.QTY6,;
                          nWO7 WITH nWO7 &lcOpSign POSLN.QTY7,nWO8 WITH nWO8 &lcOpSign POSLN.QTY8,;
                          nTotWO WITH nWO1+nWO2+nWO3+nWO4+nWO5+nWO6+nWO7+nWO8
                ENDIF
                UNLOCK
                *--Call TraceKey global function.
                =gfTraceKey('STYDYE',STYDYE.style+STYDYE.cwarecode+STYDYE.dyelot,'M')
              ENDIF
              SELECT (lcPOLine)
              REPLACE nSteps WITH 14
     
             *B606760,1 KHM 01/09/2003 (Begin) Commented out because it slows the program.
             *FLUSH
             *B606760,1 KHM 01/09/2003 (End)
     
            ENDIF
          ENDIF

          SELECT STYDYE
          IF lcAType = 'N' AND SEEK(&lcPOline..STYLE+PADR(laData[2],6)+SPACE(10))
            IF &lcPOLine..nSteps < 15
              =RLOCK()
              REPLACE WIP1 WITH WIP1 - &lcPOline..QTY1 + POSLN.QTY1,WIP2 WITH WIP2 - &lcPOline..QTY2 + POSLN.QTY2,;
                      WIP3 WITH WIP3 - &lcPOline..QTY3 + POSLN.QTY3,WIP4 WITH WIP4 - &lcPOline..QTY4 + POSLN.QTY4,;
                      WIP5 WITH WIP5 - &lcPOline..QTY5 + POSLN.QTY5,WIP6 WITH WIP6 - &lcPOline..QTY6 + POSLN.QTY6,;
                      WIP7 WITH WIP7 - &lcPOline..QTY7 + POSLN.QTY7,WIP8 WITH WIP8 - &lcPOline..QTY8 + POSLN.QTY8,;
                      TotWip WITH Wip1+Wip2+Wip3+Wip4+Wip5+Wip6+Wip7+Wip8
              UNLOCK
              SELECT (lcPOLine)
              REPLACE nSteps WITH 15
              
              *B606760,1 KHM 01/09/2003 (Begin) Commented out because it slows the program.
              *FLUSH
              *B606760,1 KHM 01/09/2003 (End)
              
            ENDIF
            IF &lcPOLine..nSteps < 16
              SELECT STYDYE
              =RLOCK()
              REPLACE nWO1 WITH nWO1 - &lcPOline..QTY1 + POSLN.QTY1,nWO2 WITH nWO2 - &lcPOline..QTY2 + POSLN.QTY2,;
                      nWO3 WITH nWO3 - &lcPOline..QTY3 + POSLN.QTY2,nWO4 WITH nWO4 - &lcPOline..QTY4 + POSLN.QTY4,;
                      nWO5 WITH nWO5 - &lcPOline..QTY5 + POSLN.QTY3,nWO6 WITH nWO6 - &lcPOline..QTY6 + POSLN.QTY6,;
                      nWO7 WITH nWO7 - &lcPOline..QTY7 + POSLN.QTY4,nWO8 WITH nWO8 - &lcPOline..QTY8 + POSLN.QTY8,;
                      nTotWO WITH nWO1+nWO2+nWO3+nWO4+nWO5+nWO6+nWO7+nWO8
              UNLOCK
              SELECT (lcPOLine)
              REPLACE nSteps WITH 16
      
              *B606760,1 KHM 01/09/2003 (Begin) Commented out because it slows the program.
              *FLUSH
              *B606760,1 KHM 01/09/2003 (End)
              
            ENDIF
            *--Call TraceKey global function.
            =gfTraceKey('STYDYE',STYDYE.style+STYDYE.cwarecode+STYDYE.dyelot,'M')
          ENDIF

          lnOpnAmt=lnOpnAmt &lcSign (&lcPOline..TotQty * &lcPOline..neCost1) &lcOpSign (POSLN.TotQty * POSLN.neCost1)

        ENDIF

        
        IF &lcPOLine..nSteps < 17
          SELECT POSLN

          *B602657,1 Update cStyGrade field [Begin
          m.cStyGrade = Style.cStyGrade
          *B602657,1 Update cStyGrade field [End

          GATHER MEMVAR

          *E301077,8 Don't update odded fields and call global function.[Begin
          *Odded REPLACE cAdd_User  WITH gcUser_id,;
          *Odded         cAdd_Time  WITH TIME(),;
          *Odded         dAdd_Date  WITH gdSysDate

          =gfAdd_Info('POSLN')
          
          *C102563,1 AMH Update custom fields for Studio Direct [Start]
          IF !llFromEdi .AND. ASCAN(laEvntTrig , PADR('SVMODFLD',10)) <> 0
            REPLACE LMOD      WITH .T.,;
                    CMOD_USER WITH CADD_USER,;
                    CMOD_DATE WITH DADD_DATE
          ENDIF
          *C102563,1 AMH [End]
          
          *E301077,8 Don't update odded fields and call global function.[End

          SELECT (lcPOLine)
          REPLACE nSteps WITH 17
      
          *B606760,1 KHM 01/09/2003 (Begin) Commented out because it slows the program.
          *FLUSH
          *B606760,1 KHM 01/09/2003 (End)
      
          *--Call TraceKey global function.
          *=gfTraceKey('POSLN',POSLN.cstytype+POSLN.po+POSLN.style+STR(POSLN.lineno,6)+POSLN.trancd,'M')
          SELECT POSLN
          =gfTraceKey('POSLN',cStyType+Po+cRsession+Shipno+Style+STR(Lineno,6)+Trancd,'M')
        ENDIF

        *--Clear allocation if released when modify.
        *B603298,1 start, added case of Adornmet order too.
        *IF llSOInstld AND lcAType='P' AND laData[3] $ 'OH' AND &lcPOLine..nSteps < 18 AND USED(lcTmpCtPk)
        IF llSOInstld AND lcAType$'PA' AND laData[3] $ 'OH' AND ;
           &lcPOLine..nSteps < 18 AND USED(lcTmpCtPk)
        *B603298,1 End.
   
          *E301077,8 open ordhdr only if not opened.
          IF !laOpFile[4]
            laOpFile[4]= gfOpenFile(gcDataDir+'ORDHDR','ORDHDR','SH')
          ENDIF
          *E301077,8 End.
          
          SELECT CUTPICK
          IF SEEK('2'+laData[1]+&lcPOLine..Style)
            SELECT (lcPOLine)
            SCATTER FIELDS Ord1,Ord2,Ord3,Ord4,Ord5,Ord6,Ord7,Ord8,TotOrd TO laNewOrd
            SELECT POSLN
            GATHER FROM laNewOrd FIELDS Ord1,Ord2,Ord3,Ord4,Ord5,Ord6,Ord7,Ord8,TotOrd
            SELECT (lcTmpCtPk)
            SCAN FOR Trancd+cTktNo+Style = '2'+laData[1]+&lcPOLine..Style
              lcCpkky=Trancd+cTktNo+Style
              SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty TO laNewOrd
              *ram
              *SCATTER FIELDS nCurPck1,nCurPck2,nCurPck3,nCurPck4,nCurPck5,nCurPck6,nCurPck7,nCurPck8,nCurPck9 TO laOldOrd
              SCATTER FIELDS nCurPck1,nCurPck2,nCurPck3,nCurPck4,nCurPck5,nCurPck6,nCurPck7,nCurPck8,nTotCur TO laOldOrd
              SELECT CUTPICK
              SEEK lcCpkky
              LOCATE REST WHILE Trancd+cTktNo+Style=lcCpkky ;
                      FOR Order=&lcTmpCtPk..Order AND cOrdLine=&lcTmpCtPk..cOrdLine
              IF FOUND()
                GATHER FROM laNewOrd FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty
                =SEEK('O'+CUTPICK.Order,'ORDHDR')
                =SEEK('O'+CUTPICK.Order+STR(INT(VAL(CUTPICK.cOrdLine)),6),'ORDLINE')    
                FOR lnSizeNo = 1 TO 8
                  lcSz = STR(lnSizeNo,1)
                  REPLACE ORDHDR.TotCut    WITH ORDHDR.TotCut    - laOldOrd[lnSizeNo] + laNewOrd[lnSizeNo]
                  REPLACE ORDLINE.TotCut   WITH ORDLINE.TotCut   - laOldOrd[lnSizeNo] + laNewOrd[lnSizeNo],;
                          ORDLINE.Cut&lcSz WITH ORDLINE.Cut&lcSz - laOldOrd[lnSizeNo] + laNewOrd[lnSizeNo]
                ENDFOR
                *--Call TraceKey global function.
                SELECT CUTPICK
                
               *E301182,7 Adjust trace key master tag [Begin]
                *=gfTraceKey('CUTPICK',Trancd+cTktNo+Order+Style+cOrdline,'M')    
                =lfTraceTag('CUTPICK','M')    
               *E301182,7 Adjust trace key master tag [End  ]
                
                =gfTraceKey('ORDHDR','O'+CUTPICK.Order,'M')
                =gfTraceKey('ORDLINE','O'+CUTPICK.Order+STR(INT(VAL(CUTPICK.cOrdLine)),6),'M')

                IF CUTPICK.TotQty = 0
                  SELECT CUTPICK
                  
                 *E301182,7 Adjust trace key master tag [Begin]
                  *=gfTraceKey('CUTPICK',Trancd+cTktNo+Order+Style+cOrdline,'D')
                  =lfTraceTag('CUTPICK','D')
                 *E301182,7 Adjust trace key master tag [End  ]
                  
                  =RLOCK()
                  BLANK
                  DELETE
                  UNLOCK
                ENDIF  
              ENDIF
            ENDSCAN
          ENDIF
          SELECT (lcPOLine)
          REPLACE nSteps WITH 18
      
          *B606760,1 KHM 01/09/2003 (Begin) Commented out because it slows the program.
          *FLUSH
          *B606760,1 KHM 01/09/2003 (End)
      
        ENDIF
       
      ENDIF     

    ENDSCAN


    *--Update the last line in the Header.
    lnLines=0
    SELECT POSLN
    =SEEK(lcAType+laData[1])
    SCAN WHILE cStyType+PO=lcAType+laData[1]  FOR TranCd = '1'
      lnLines=MAX(LineNo,lnLines)
    ENDSCAN

    *--Delete Uncorrect P/o bom lines.
    SELECT (lcPOLine)
    DELETE ALL FOR cRSession = 'XXXXXX'

    =lfRecalc()

    *--Update POSHDR file. 
    SELECT POSHDR
    =SEEK(lcAType+laData[1])
    GATHER FROM laData FIELDS &lcScFields MEMO

    *E301077,8 Don't update odded fields and call global function.[Begin
    *Odded REPLACE LastLine   WITH lnLines ,;
    *Odded         Status     WITH IIF(Open=0,IIF(Receive>0,'C','X'),Status),;
    *Odded         lMultiWare WITH llMultiWare,;
    *Odded         nCurrUnit  WITH IIF(llMulCurr,lnCurrUnt1,1),;
    *Odded         nDCurUnit  WITH IIF(llMulCurr,lnCurrUnt2,1),;
    *Odded         Mod_UserId WITH gcUser_id,;
    *Odded         Mod_Time   WITH TIME(),;
    *Odded         Mod_Date   WITH gdSysDate,;
    *Odded         Mod_Action WITH 'MODIFIED'

    REPLACE LastLine   WITH lnLines ,;
            Status     WITH IIF(Open=0,IIF(Receive>0,'C','X'),Status),;
            lMultiWare WITH llMultiWare,;
            nCurrUnit  WITH IIF(llMulCurr,lnCurrUnt1,1),;
            nDCurUnit  WITH IIF(llMulCurr,lnCurrUnt2,1)

    =gfAdd_Info('POSHDR')
    *E301077,8 Don't update odded fields and call global function.[End

    *--Call TraceKey global function.
    =gfTraceKey('POSHDR',POSHDR.cstytype+POSHDR.po,'M')

    *--Update Letter of credit file.      
    IF lcAType='P' AND !EMPTY(laData[13]) AND POSHDR.cLCStatus='A' AND POSHDR.nICost1<>laData[31]
      IF &lcPOHdr..nSteps < 1
        =lfLCPO('M',laData[13],(laData[31]-PosHdr.nICost1))  
        SELECT (lcPOHdr)
        REPLACE nSteps WITH 1

        *B606760,1 KHM 01/09/2003 (Begin) Commented out because it slows the program.
        *FLUSH
        *B606760,1 KHM 01/09/2003 (End)

      ENDIF
    ENDIF

    *--Update Vendor file. 
    *C200080,1 AMM Add the case of dye order
    *IF llApIstall AND lcAType = 'PR' AND laData[3]<>'B'
    IF llApIstall AND lcAType = 'PRD' AND laData[3]<>'B'
    *C200080,1 AMM end
      IF &lcPOHdr..nSteps < 2
        SELECT ApVendor
        =RLOCK()
        REPLACE nVenOpnPO WITH nVenOpnPO +lnOpnAmt
        UNLOCK
        SELECT (lcPOHdr)
        REPLACE nSteps WITH 2
        
        *B606760,1 KHM 01/09/2003 (Begin) Commented out because it slows the program.
        *FLUSH
        *B606760,1 KHM 01/09/2003 (End)
      ENDIF
      *--Call TraceKey global function.
      =gfTraceKey('APVENDOR',APVENDOR.cvendcode,'M')
    ENDIF
    *B604136,1 KHM 22/01/2001 (Begin) Checking if the material module is
    *B604136,1                installed and system is setup to use dyelot 
    *B604136,1                and the material uses dyelot.
    *=gfArDyRl('' , '' , lcWinCh0,.T.)
    IF 'MA' $ gcComp_mdl AND llDyelot
      =gfArDyRl('' , '' , lcWinCh0,.T.)
    ENDIF
    *B604136,1 KHM 22/01/2001 (End)

    *E301484,1 WAM Send Inter Location PO or Purchase orders to sites
    =lfSendEdi(lcAType,'850')
    *E301484,1 WAM (End)

ENDCASE
WAIT CLEAR
*E301291,1 AMM save adornemt costs
IF lcAType = 'A'

  *C102357,1 AMH If Customer J&L save data to BomVar & Custom thread color file. [Start]
  *E301291,1 AMM Call global function to update the bomvar file.
  *=gfTmp2Mast('BOMVAR' , lcT_BomVar , 'Update the style positions for the order lines...')
  IF !llFromEdi AND ASCAN(laEvntTrig , PADR('DOSCRN',10)) <> 0  
    PRIVATE lcDelete 
    lcDelete = SET('DELETE')  
    SET DELETE ON 
    =gfDoTriger('POADPO',PADR('DOSCRN',10))
    SET DELETE &lcDelete
  ELSE
    *-- The following line is used to save to Standard BomVar file.
    =gfTmp2Mast('BOMVAR' , lcT_BomVar , 'Update the style positions for the order lines...')
  ENDIF  
  *C102357,1 AMH [End]

ENDIF
*E301291,1 AMM end

*C200208,1 AMH Save labelling instructions Data for STU [Start]
IF !llFromEdi AND lcAType $ 'P' AND ASCAN(laEvntTrig , PADR('SAVLBLIN',10)) <> 0
  =gfDoTriger('POSTY',PADR('SAVLBLIN',10))
ENDIF
*C200208,1 AMH [End]

*--Complete unCmSess record ,Session completed.
*=lfCrUnSess(' ','C') 
=IIF(llFromEdi,.T.,lfCrUnSess(' ','C'))

llAddMode = laScrMode[4]

*C102087,1 AMH Add record to POTOSEND when saving for GMA [Start]
IF !llFromEdi AND ASCAN(laEvntTrig , PADR('POTOSND',10)) <> 0
  =gfDoTriger('POSTY',PADR('POTOSND',10))
ENDIF
*C102087,1 AMH [End]

*C038291,1  TMI [Start] Save a line in POINS.DBF file
IF lcAType = 'P' .AND. ASCAN(laEvntTrig , PADR('POINSSV',10)) <> 0
  =gfDoTriger('POSTY',PADR('POINSSV',10))
  =gfDoTriger('POSTY',PADR('SVMSRMNT',10))
ENDIF
*C038291,1  TMI [End  ] 

*--Generate P/O cost sheet.
*E301085,1 Generate cost sheet automatically
*IF lcAType='P' AND lcCrtCSH $ 'AI' AND laData[3]='H' AND llAddMode AND !llContinue
*C200080,1 AMM Add the case of Dye Order
*IF lcAType='P' AND lcCrtCSH $ 'AIT' AND laData[3]='H' AND llAddMode AND !llContinue
IF lcAType$'PD' AND lcCrtCSH $ 'AIT' AND laData[3]='H' AND llAddMode AND !llContinue
*C200080,1 AMM end
*E301085,1 (End)

  *E301077,8 open files only if not opened, needed in cost sheet program.
  llBomCOpn = gfOpenFile(gcDataDir+'BOMCOST','BOMCOST','SH')
  llSJrlOpn = gfOpenFile(gcDataDir+'STYINVJL','MFGOPR','SH')

  =lfvGenCstSH()
 
  *E301077,8 Close the following files if open in this session.
  IF llBomCOpn AND USED('BOMCOST')
    USE IN BOMCOST
  ENDIF
  IF llSJrlOpn AND USED('STYINVJL')
    USE IN STYINVJL
  ENDIF
  *E301077,8 End.

ENDIF

*B602619,1 Erase temp lines file after saving [Begin]
*-- USE IN (lcPOLine)
*-- ERASE (gcWorkDir+lcPOLine+'.DBF')
*-- ERASE (gcWorkDir+lcPOLine+'.CDX')
llContinue = .F.
*B602619,1 Erase temp lines file after saving [End  ]

SELECT POSHDR
RETURN


*:*************************************************************
*! Name     : lpSavScr
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose  : Post receinving (Save and Update).
*:*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lpSavScr()
*!*************************************************************
*! Modifications :
*!E301180,1 Modificated to call POSTYREC report befor posting
*!E301182,7 MAB 03/31/1999 Trace cutpick file using master key.
*E301206,1 Reham On 04/21/99
*E301206,1 Add parameter if called from bar code programs
*!*************************************************************
*!
FUNCTION gfSaveRec
*E301206,1 Reham On 04/21/99   *** Begin ***
*E301206,1 True if called from any of the bar code programs.
*E301484,1 WAM Add new parameter setup with .T. if called from EDI
PARAMETERS llBarCode,llFromEdi


*E301206,1 Reham On 04/21/99   *** End   ***
*E301180,1
PRIVATE lnCurAlias , lcPosOrd , lnPoshPOS , lnPoslPos , lnStyPos , lnTmpPos ;
                   , lcTmpOrd , lnApvenPos
R_Width = ''       && variabel defined in the report                   

*jacki
*E301484,1 WAM Send Inter Location PO or Purchase orders to sites
IF INLIST(lcPType,'O','L') AND !llFromEDI AND 'NC' $ gcCmpModules
  =gfOpenFile(gcDataDir+'EDIACPRT',gcDataDir+'ACCFACT','SH')
  =gfOpenFile(gcDataDir+'EDIPD',gcDataDir+'PARTTRANS','SH')
  =gfOpenFile(gcDataDir+'EDITRANS',gcDataDir+'TYPEKEY','SH')
ENDIF
*jacki

SELECT (lcTmpLine)
lnCrRec = IIF(EOF(),0,RECNO())
LOCATE FOR TranCd<>'1'
IF !FOUND()
  *--No [Receiving Lines/Issuing lines] was done,Cannot update.
  = gfModalGen('INM34061B42000','DIALOG',lcBrDtTtl)
  STORE .F. TO llShow,llCSave
  
  *E300935,4 Check of phisical record before go to it.
  *IF lnCrRec<>0
  IF BETWEEN(lnCrRec,1,RECCOUNT())
    GOTO lnCrRec
  ENDIF
  
  RETURN .F.
ENDIF

*E301480,1 NAD (Start)  Add the Inter location Po batch to the condition.
*IF lcPType='B'
IF lcPType $ 'BL'  
*E301480,1 NAD (End)

  LOCATE FOR nLanPrRat=0 OR nLanDuRat=0
  IF FOUND()
    *--You cannot receive batch with zero rates.
    = gfModalGen('TRM34082B42000','DIALOG')
    STORE .F. TO llShow,llCSave
  
    *E300935,4 Check of phisical record before go to it.
    *IF lnCrRec<>0
    IF BETWEEN(lnCrRec,1,RECCOUNT())
      GOTO lnCrRec
    ENDIF
    RETURN .F.
  ENDIF
ENDIF

*E301180,1
IF ! llContinue
  DO CASE 
    *-- case this report is called to Issue Return P/o , Receive C/T , Receive C/T Batch   
    *-- this case do the old code without modification .
    *C200080,1 AMM Add the dye order type 'D'
    *E301291,4 AMM Add Issue/Receive Adornment order types 'AE'
    *CASE lcPType ='R' OR lcPType ='M' OR lcPType ='T' 
    CASE lcPType $ 'RMTDAE'
    *C200080,1 AMM end
      lnSel = gfModalGen('QRM34076B42010','DIALOG')   
      IF lnSel=2    
        STORE .F. TO llShow,llCSave
        RETURN .F.
      ELSE
        lnSel=3
      ENDIF
      
    *E301480,1 NAD (Start) Add the Inter location Po batch to the condition.
    *CASE lcPType ='I' OR lcPType ='S' OR lcPType ='B' OR lcPType ='N' OR lcPType ='O'      

    *C200170,1 AMH Add case of issue inter-location P/O Batch [Start]
    *CASE lcPType ='I' OR lcPType ='S' OR lcPType ='B' OR lcPType ='N' OR lcPType ='O' OR lcPType ='L'
    CASE lcPType ='I' OR lcPType ='S' OR lcPType ='B' OR lcPType ='N' OR lcPType ='O' OR lcPType ='L' OR lcPType ='H'
    *C200170,1 AMH [End]

    *E301480,1 NAD (End)
    
    *--Print reciepts log.
    
    *--Do you wish to print/view [reciepts/issues] log before posting all transactions?,'Print Log,Post,Cancel'
    *-- lnSel=1 View log
    *-- lnSel=2 print log
    *-- lnSel=3 post
    *-- lnSel=4 cancel

    *E301484,1 WAM Add new parameter setup with .T. if called from EDI        
    *lnSel = 0
    lnSel = IIF(llFromEdi,3,0)
    *E301484,1 (End)
    
    *IF lnSel=1 OR lnSel=3
    DO WHILE lnSel < 3 &&-- after view/print do the same screen for print/view 
                       &&-- or post or cancel .
    
      *E301291,4 AMM Add Issue adornment PO case
      *lnSel = gfModalGen('QRM34075B42009','DIALOG',IIF(lcPType='N','issues','recepts'))
      *B803590,1  MHM 09/10/2000 
      *lnSel = gfModalGen('QRM34075B42009','DIALOG',IIF(lcPType$'NA','issues','reciepts'))

      *C200170,1 AMH Add case of issue inter-location P/O Batch [Start]
      *lnSel = gfModalGen('QRM34075B42009','DIALOG',IIF(lcPType$'NA','issues','receipts'))
      lnSel = gfModalGen('QRM34075B42009','DIALOG',IIF(lcPType$'NAH','issues','receipts'))
      *C200170,1 AMH [End]

      *B803590,1  MHM 09/10/2000 
      *E301291,4 AMM end
      IF lnSel=1 OR lnSel=2 &&-- if preview/print only
        *--  
        *-- Start all modifications needed to call POSTYREC report
        *-- 1- Save the current alias
        *-- 2- Save the record pointer of STYLE  file
        *-- 3- Save the record pointer of POSHDR file
        *-- 4- Save the record pointer of POSLN  file
        *--    And save it's order then close it .
        *-- 5- Save the record pointer of TMPLINE file
        *--    And save it's order 
        *-- 6- Creat the requierd index the POSTYREC report need for
        *--    this file [cstytype+po+style+STR(lineno,6)+trancd]
        *--    then use it again with alias POSLN
        *-- 7- define all variabels needed for POSTYREC report .
        *-- 8- Run the POSTYREC report .
        *-- 9- Restore all .
 
        lcRPTitle   = ''
        llMultiWh   = llWareHous
        llRPCostDt  = .F.
        llRPFrnCur  = .F.
        *B803594,1  MHM 09/10/2000 initialize variable [Start]
        llRpDec     = .T.
        *B803594,1  MHM 09/10/2000 [End]
        lcRpSortBy  = IIF(lcPType='S','H','P')
        *-- if this program is called by parameter 'N' Issue Inter-Location P/o  
        *-- or 'O' Receive Inter-Location P/o
        lcRPPoType  = IIF(lcPType$"ON","N",'A')
        lcRPPrice   = 'C'
        lcRpExp     = '.T.'
        Qcompany    = gcCom_Name
        lcMajTtl    = gfItemMask("HM")     
        lcOGWinTitl = 'P/O Receipts Log'
        lnCurAlias = SELECT(0)              
        *B803337,1 WMA Open BOMLINE File[Begin]
        =gfOpenFile(gcDataDir+'BOMLINE','BOMLINE','SH')
        *B803337,1 WMA [End]

        lnStyPos = RECNO('STYLE')
        lnPoshPOS= RECNO('POSHDR')
        lnPoslPos= RECNO('POSLN')
        lcPosOrd = ORDER('POSLN')
        USE IN POSLN
        lnTmpPos = RECNO(lcTmpLine)
        lcTmpOrd = ORDER(lcTmpLine)
        lnApvenPos= RECNO('APVENDOR')
        USE (gcWorkDir+lcTmpLine) IN 0 AGAIN ORDER TAG POSLN ALIAS POSLN 
        *B802902,1 SSH 03/05/2000 if we select to preview log report, 
        *B802902,1 SSH            the reports show the estimated p.price along 
        *B802902,1 SSH            (with each style/color being received) 
        *B802902,1 SSH            instead of the landed p.price. 
        lcBomPTyp = IIF(lcPType='S','S','I')
        
        *E301480,1 NAD (Start)  Add the Inter location Po batch to the condition.
        *IF (lcPType ='I' OR lcPType ='S' OR lcPType ='B' OR lcPType ='O')
        IF (lcPType ='I' OR lcPType ='S' OR lcPType ='B' OR lcPType ='O' OR lcPType ='L')
        *E301480,1 NAD (End)
          SELECT POSLN
          SCAN FOR  TranCd $ '42' AND TotQty <> 0;
            .AND. lfChekAdj(lcBomPTyp,Po+STR(LineNo,6),Style,IIF(lcPType='S',ShipNo,''),;
                       '',cStyGrade)
            IF !llImpCost AND llEditLCst
            ELSE
              =lfPrnLanded()
            ENDIF
          ENDSCAN
        ENDIF
        *B802902,1 SSH [E]
        llDoRep = .T.
       
        *-- Run Report
        DECLARE laWareCodes[1]
        STORE "" TO laWareCodes

        IF lnSel=2 
          gcDevice     = "PRINTER"
          lcOGPlatForm = "DOS"
          
          *B606516,1 KHM 10/03/2002 (Begin) Initializing OGPlatForm = "DOS" in order
          *B606516,1                to get the correct printer device.
          OGPlatForm = "DOS"
          *B606516,1 KHM 10/03/2002 (End)
          
          *E301206,1 Reham On 04/21/99   *** Begin ***
          *E301206,1 If called from any of the bar code programs.
          IF !llBarCode
          *E301206,1 Reham On 04/21/99   *** End   ***
            RELEASE WINDOW (lcBrDtTtl)
          *E301206,1 Reham On 04/21/99   *** Begin ***
          ENDIF
          *E301206,1 Reham On 04/21/99   *** End   ***
          
          IF lcPType = "O"    
          
            SELECT DIST cWareCode FROM POSLN INTO ARRAY laWareCodes
          ENDIF  
          IF !pSetup(.T.,.F.)
            llDoRep = .F.
            lnSel   = 4
          ENDIF  
        ELSE
          gcDevice = "SCREEN"
        ENDIF
 
        IF llDoRep
          FOR lnNo = 1 TO ALEN(laWareCodes,1)
            IF lcPType  = "O" AND lnSel=2 
              lcRpExp   = "cWareCode = '"+ laWareCodes[lnNo] + "'"
              lcRPTitle = "Issue Inter Location To " + laWareCodes[lnNo]
            ENDIF  
            =gfSetDevice()
            
            DO (gcRepHome+'\POSTYREC') &&- Report postyrec was moved to report root directory
          ENDFOR
        ENDIF
  
        SELECT STYLE
        IF BETWEEN(lnStyPos,1,RECCOUNT("STYLE"))
          GO lnStyPos    
        ENDIF  
 
        SELECT POSHDR
        IF BETWEEN(lnPoshPOS,1,RECCOUNT("POSHDR"))      
          GO lnPoshPOS    
        ENDIF  

        USE IN POSLN
        USE (gcDatadir+'POSLN') IN 0 SHARED
        SELECT POSLN
        SET ORDER TO TAG &lcPosOrd
        IF BETWEEN(lnPoslPos,1,RECCOUNT("POSLN"))      
          GO lnPoslPos
        ENDIF  
        SELECT APVENDOR
        IF BETWEEN(lnApvenPos,1,RECCOUNT("APVENDOR"))            
          GO lnApvenPos
        ENDIF  
        SELECT(lnCurAlias)
        IF BETWEEN(lnTmpPos,1,RECCOUNT(lnCurAlias))            
          GO lnTmpPos
        ENDIF  
        *E301206,1 Reham On 04/21/99   *** Begin ***
        *E301206,1 If called from any of the bar code programs.
        IF !llBarCode
        *E301206,1 Reham On 04/21/99   *** End   ***
          =lfActBrow()
        *E301206,1 Reham On 04/21/99   *** Begin ***
        ENDIF
        *E301206,1 Reham On 04/21/99   *** End   ***
      
        *--Confirm action.,'\<Post;\<Cancel'

      ENDIF  &&-- endif lnSel=1 OR lnSel=2

      *B603204,1 Start, don't allow to reprint log.
      *--Post after printing the issue or receiving log    
      IF lcPType $ 'NO' AND lnSel=2 AND gcAct_Appl='PS' AND (gfGetMemVar('LLPOSTRCV') = 'Y')
        EXIT
      ENDIF
      *B603204,1 End.

    ENDDO

    IF lnSel = 4  &&-- <Canel>
      STORE .F. TO llShow,llCSave
      RETURN .F.
    ENDIF  
  ENDCASE
ENDIF &&-- endif ! llContinue
*-----------------------------------------------------------------------
*--add New C/t line,only it you use dyelots and dyelot changed.

IF llDyelot AND llMFCall
  *--Check new lines dyelots case of batch.
  SELECT (lcTmpLine)    
  SCAN FOR !EMPTY(Dyelot)
    IF TranCd = '1'
      REPLACE lNewLn WITH .T.
    ELSE
      IF lcPType='T' AND !SEEK(Cuttkt+Style+Dyelot,'CUTTKTL')
        REPLACE lNewLUpd WITH .T.
      ENDIF
    ENDIF
  ENDSCAN

  llUnblProc = .F.

  = lfDyeOvrRcv()
  *--Unable to proceed due to
  *--The over received quantity is not allocated completely.
  IF llUnblProc
    SELECT (lcTmpLine)
    SET ORDER TO TAG TmpLine1
    GO TOP
    STORE .F. TO llShow,llCSave
    RETURN .F.
  ENDIF
ENDIF

WAIT WINDOW 'Posting all transactions ...' NOWAIT
STORE .T. TO llCSave,llCUpdate,llUpdated,llShow

*E301077,11 Open fabrics files.
*B602485,1 This code is not useful here because we open this two files[Begin
*B602485,1 under condition in the start of program.
*IF !laOpFile[4]
*  laOpFile[4] = gfOpenFile(gcDataDir+"FabDye","FabDye","SH")
*ENDIF
*IF !laOpFile[5]
*  laOpFile[5] = gfOpenFile(gcDataDir+"Fabric","Fabric","SH")
*ENDIF 
*B602485,1 This code is not useful here because we open this two files[End..
= gfOpenFile(gcDataDir+"APVENDOR","Vencode","SH")

****

*--Open GLDIST file to call G/L Distribution Procedure.
IF llLinkToGl AND ! llContinue
  *IF !laOpFile[9]
    = gfOpenFile(gcDataDir+'GLDIST','GlDistAc','SH')
  *ENDIF
  SELECT GLDist
  =AFIELDS(laFStru)
  =gfCrtTmp(lcGlDist,@laFStru)
ENDIF

*--Session no. variable.
IF ! llContinue
*E301480,1 NAD (Start) Not to Generate a new session number for the Inter Location Po batch.
  *E301484,1 WAM Do not generate new session number if called from EDI
  *lcGlSession = gfsequence('GLSESSION')
  *lcGlSession = IIF(llFromEdi,lcGlSession,gfsequence('GLSESSION'))
  *E301484,1 WAM (End)
  *lcGlSession =IIF(lcPType='L',lcBatch,IIF(llFromEdi,lcGlSession,gfsequence('GLSESSION')))
  lcGlSession = gfsequence('GLSESSION')
*E301480,1 NAD (End)
ENDIF

*--Update unCmSess record.
IF ! llContinue
  *=lfCrUnSess('pbSav','',.F.) 
  *C200170,1 AMH Don't update uncmsess record in case of issue int. loc. PO by barcode [Start]
  *=IIF(llFromEdi,.T.,lfCrUnSess('pbSav','',.F.))
  =IIF(llFromEdi .OR. (ASCAN(laEvntTrig,PADR('NOTUNSES',10))<>0),.T.,lfCrUnSess('pbSav','',.F.))
  *C200170,1 AMH [End]
ENDIF

*--Prepare before Post.

*--Update lineNo with original lineNo on p/o in case of batch.
*--To join the cartons lines when receive batch.
*E301480,1 NAD (Start) Add the Inter location Po batch to the condition.

*IF (lcPType $ 'BT') AND llByCarton
IF (lcPType $ 'BTL') AND llByCarton
*E301480,1 NAD (End)

  SELECT (lcTmpLine)
  REPLACE ALL LineNo WITH nLineNo
ENDIF

*--If new line add to P/o and update bom. Or
*C200080,1 AMM Add the dye order type 'D'
*IF (lcPType $ 'ISR')
IF (lcPType $ 'ISRD')
*C200080,1 AMM end
  =lfAddNewLn()
ENDIF

*--Issue Inter Location P/O Updates.
*E301291,4 AMM Consider case of issue adornment PO
*IF lcPType='N'

*C200170,1 AMH Add case of issue inter-location P/O Batch [Start]
*IF lcPType $ 'NA'
IF lcPType $ 'NAH'
*C200170,1 AMH [End]

*E301291,4 AMM
  *--Issue stock for Inter Location P/o Styles and create intransit records.
  =lfUpdIntCmp()

  *--Complete unCmSess record ,Session completed.
  *C200170,1 AMH Don't update uncmsess record in case of issue int. loc. PO by barcode [Start]
  *=lfCrUnSess('','C',.F.) 
  *=IIF(llFromEdi,.T.,lfCrUnSess('','C',.F.))
  =IIF(llFromEdi .OR. (ASCAN(laEvntTrig,PADR('NOTUNSES',10))<>0),.T.,lfCrUnSess('','C',.F.))
  *C200170,1 AMH [End]

  *C200170,1 AMH Update Inter Location P/O Status [Start]
  IF lcPType = 'H'
    IF SEEK('N'+lcBatch,'CTKTRCVH')
      SELECT CTKTRCVH
      REPLACE CSTATUS WITH 'I'
    ENDIF
  ENDIF
  *C200170,1 AMH [End]

  WAIT CLEAR
  SELECT (lcTmpLine)
 
  SET ORDER TO TAG TmpLine1
  DELETE ALL
  *E301206,1 Reham On 04/21/99   *** Begin ***
  *E301206,1 If called from any of the bar code programs.
  *E301484,1 WAM Do not generate new session number if called from EDI
  *IF !llBarCode
  IF !llBarCode AND !llFromEdi
  *E301484,1 WAM (End)

  *E301206,1 Reham On 04/21/99   *** End   ***
    =lfClearInfo()
  *E301206,1 Reham On 04/21/99   *** Begin ***
  ENDIF
  *E301206,1 Reham On 04/21/99   *** End   ***
  
  RETURN
ENDIF

*--Prepare date for receiving custom process.
*B603237,1 Custom process to Receive Adorn Order.
IF  lcPType='E' AND llSpecHndl
  =gfDoTriger('MFRCVAR',PADR('UPDRCEVAD',10))
ENDIF
*B603237,1 End.


*--Srart Update lines except Issue inter Location P/o.
DIME laOpnQty[8]
STORE 0   TO lnLstPoRAm,lnOpnPoAmt,laOpnQty
STORE ' ' TO lcLstTrn,lcPoVend,lcOrjWareH
STORE ' ' TO lcSvCode,lcSvStyle
lcSTKSgn= IIF( lcPType $ 'R' , '-' , '+' )
lcLotNo = SPACE(2)
lnNxtStp= 2

*C102469,1 AMH Create cursor to save the last landed cost [Start]
*B606076,1 RAE Open BOMLINE file in case of Post. [start]
*SELECT BOMLINE
IF !USED('BOMLINE')
  =gfOpenFile(gcDataDir+'BOMLINE','BOMLINE','SH')
ENDIF
SELECT BOMLINE
*B606076,1 RAE [end]
PRIVATE laBomLineF, TmpBomLine
TmpBomLine = gfTempName()

DECLARE laBomLineF[1,4]
=AFIELDS(laBomLineF)
CREATE CURSOR (TmpBomLine) FROM ARRAY laBomLineF
*C102469,1 AMH [End]
*ash
llStkLine = .F.
*--Update lines for Cutting Ticket or Purchase Order.
SELECT (lcTmpLine)
SET ORDER TO TAG TmpLine3
GO TOP
SCAN FOR TranCd <> '1' AND TotQty <> 0
  WAIT WINDOW 'Posting all transactions ... Style :'+Style NOWAIT
  

  *--Stock line is receive to Main Stock or receive to Others Stock.
  llStkLine = ( TranCd='2' OR TranCd = lcOthrTrCd )

  *--Read Saved variables.
  lcSvCode  = IIF(llMFCall ,Cuttkt ,Po )
  lcHdFile  = IIF(llMFCall,'CUTTKTH','POSHDR')
  *E301291,4 AMM Add the Receive adornment order type 'E'
  *C200080,1 AMM Add dye order type 'D'
  *lcSvTpe   = IIF(llMFCall,'',IIF(lcPType $ 'RO',IIF(lcPType='O','N','R'),'P'))    
  
  *E301480,1 NAD (Start) Add the Inter location Po batch to the condition.
  *lcSvTpe   = IIF(llMFCall,'',IIF(lcPType $ 'RDOE',IIF(lcPType='O','N',IIF(lcPType='E','A',lcPType)),'P'))    
  lcSvTpe   = IIF(llMFCall,'',IIF(lcPType $ 'RDOEL',IIF(lcPType$'OL','N',IIF(lcPType='E','A',lcPType)),'P'))    
  *E301480,1 NAD (End) 
  
  *E301291,4 AMM end

  *--Compute previous open balance on style P/o or C/t.----------------
  lcMainKy = IIF(llMfCall,Cuttkt+Style+Dyelot,;
                          lcSvTpe+PO+Style+STR(LineNo,6))

  *E300935,4 Add new logical parameter in calling of this function 
  *E300935,4 to avoid subtract block

  *C127341,14 TMI [Start] Update the lcWarecode variable 
  lcWareCode = &lcTmpLine..cWareCode
  *C127341,14 TMI [End  ] 

  *T20061003.0005 TMI [Start] update the variable m.Lineno to be used in saving process
  M.LINENO = &lcTmpLine..LINENO
  *T20061003.0005 TMI [End  ] 
  
  lcOrjWareH=lfGetOpen( lcMainKy , .T.)
  *--If this is a new line therefor no open qty.
  IF &lctmpline..lnewln
    laOpnQty = 0
  ENDIF 

  *--Update BomLine (Step 1)--------------------------------------------
  SELECT (lcTmpLine)


  *B602554,1 Add landed cost records in the bomline even for if the system is 
  *B602554,1 not setup to use detail costing for importing styles.
  *IF llStkLine AND (llMFCall OR (llImpCost AND !(lcPType $'RO')) )
  *E301291,4 AMM Add Receive adornment PO type 'E'
  *IF llStkLine AND (llMFCall OR !(lcPType $'RO') )
  
  *E301480,1 NAD (Start) Add the Inter location Po batch to the condition.
  *IF llStkLine AND (llMFCall OR !(lcPType $'ROE') )
  IF llStkLine AND (llMFCall OR !(lcPType $'ROEL') )
  *E301480,1 NAD (End)
  
  *E301291,4 AMM end
  *B602554,1 (End)
    IF llMFCall
      lcBomPTyp =IIF(llMFCall,'M',IIF(lcPType='S','S','I'))
      lcBomPKey = Cuttkt
    ELSE
      *khm1
      *lcBomPTyp = IIF(lcPType='S','S','I')
      lcBomPTyp = IIF(lcPType='S','S',IIF(lcPType='D','D','I'))
      *khm1
      
      lcBomPKey = Po+STR(LineNo,6)
    ENDIF
    *--Create new records in BOMLINE file with type '2' if it is not found.
    IF &lcTmpLine..nSteps < 1 

      *B803337,1 WMA [Begin]
      *E301077,11 MAB Open BOMLINE file here [Begin
      *IF !llOpnBomLn
      *  llOpnBomLn = lfOpn_Rest(gcDatadir,'BOMLINE','BOMLINE')
      *ENDIF  
      *E301077,11 MAB Open BOMLINE file here [End..
      *B803337,1 WMA [End]
      
      DO gpCrtBom WITH lcBomPTyp,lcBomPKey,Style,IIF(lcPType='S',ShipNo,''),;
                       lcGlSession,&lcTmpLine..cStyGrade
      SELECT (lcTmpLine)
      =RLOCK()
      REPLACE nSteps WITH 1
      UNLOCK
    ENDIF 

    *--Calculate landed costs case of detail costing. 
    *B803030,1 TAK 02/08/2000 (Begin) If you are not in the Material Module
    *B803030,1                and not detail costing and you are editing 
    *B803030,1                the costs then take it from the entered one.
    IF !llMfCall AND !llImpCost AND llEditLCst
    *B803030,1 TAK 02/08/2000 (End)
      *--Take from entered landed cost.
    ELSE
      =lfGetLanded()
    ENDIF

  ENDIF

  *--New Receiving Cost.
  SELECT (lcTmpLine)
  IF llMFCall
    lnNewCost = nLan_Cst1 +nLan_Cst2 +nLan_Cst3 +nLan_Cst4 +nLan_Cst5
  ELSE
    *B602861,1
    lnELanded = nELanCost1
    IF lnELanded = ROUND(Gros_Price*(1-Disc_Pcnt/100),3)
       lnELanded = Gros_Price*(1-Disc_Pcnt/100)
    ENDIF

    lnNewCost = lnELanded+nELanCost2+nELanCost3+nELanCost4+nELanCost5
  ENDIF
  
  *B038753,1 KHM 12/18/2004 Save the transaction cost in lnOrgCost to to be used in the gfStyCrl
  *B038753,1                function in order to save it in StyInvJl.nTranCost in ase of 
  *B038753,1                standard costing method [Begin]
  lnOrgCost = lnNewCost
  *B038753,1 KHM 12/18/2004 [End]

 
  *B604319,1 KHM 04/03/2001 (Begin) Seek for the correct record in the style file.
  =SEEK(&lcTmpLine..Style,'STYLE')
  *B604319,1 KHM 04/03/2001 (End)

  *--Read the G/L link code that will be used to create GL entres.
  IF llStkLine
    
    *B038753,1 KHM 12/18/2004 Use the Style.TotCost in case of standard costing method [Begin]
    IF lcCostMth = 'S'
      lnNewCost = Style.TotCost
    ENDIF
    *B038753,1 KHM 12/18/2004 [End]
    
    IF &lcTmpLine..Trancd = lcOthrTrCd
      =SEEK(&lcTmpLine..cRetSty,'STYLE')
      
      *B038753,1 KHM 12/18/2004 Use the Style.TotCost in case of standard costing method [Begin]
      IF lcCostMth = 'S'
        lnNewCost = Style.TotCost
      ENDIF
      *B038753,1 KHM 12/18/2004 [End]

    ENDIF  
    lclinkCode = IIF(!EMPTY(STYLE.link_code),STYLE.link_code,'DEFDEF')
    IF &lcTmpLine..Trancd = '2'
      =SEEK(&lcTmpLine..Style  +&lcTmpLine..cWareCode+SPACE(10),'STYDYE')
    ELSE        && Trancd = lcOthrTrCd
      =SEEK(&lcTmpLine..cRetSty+&lcTmpLine..cWareCode+SPACE(10),'STYDYE')
    ENDIF  
    lclinkCode=IIF(!EMPTY(STYDYE.Gl_link),STYDYE.Gl_link,lclinkcode)
  ENDIF
  lcGLPLkC=IIF(SEEK(lcSvTpe+lcSvCode,lcHdFile),&lcHdFile..Link_Code,'DEFDEF')

  lcCurSty = STYLE.STYLE
  = SEEK(&lcTmpLine..Style,'STYLE')

  *E300935,4 Update stydye file in all cases except Inter Location case.
  *--Create StyDye Dyelot record in C/t or PO if new for receiving warehouse.------
  *IF llMFCall AND llStkLine AND llDyelot AND Style.cDye_Flg='Y'
  *E301291,4 AMM Add issue/receive adornment order types 'AE'
  *IF !(lcPType $ 'RNO') AND llStkLine AND llDyelot AND Style.cDye_Flg = 'Y'
  
  *E301480,1 NAD (Start) Add the Inter location Po batch to the condition.
  *IF !(lcPType $ 'RNOAE') AND llStkLine AND llDyelot AND Style.cDye_Flg = 'Y'
  IF !(lcPType $ 'RNOLAE') AND llStkLine AND llDyelot AND Style.cDye_Flg = 'Y'
  *E301480,1 NAD (End)
  
  *E301291,4 AMM end
    IF &lcTmpLine..Trancd = '2'
      llFound=SEEK(&lcTmpLine..Style  +&lcTmpLine..cWareCode+&lcTmpLine..Dyelot,'STYDYE')
    ELSE    && Trancd = lcOthrTrCd
      llFound=SEEK(&lcTmpLine..cRetSty+&lcTmpLine..cWareCode+&lcTmpLine..Dyelot,'STYDYE')
    ENDIF  

    IF ! llFound
      SELECT STYDYE
      APPEND BLANK
      REPLACE Style     WITH IIF(&lcTmpLine..Trancd='2',&lcTmpLine..Style,&lcTmpLine..cRetSty),;
              cWareCode WITH &lcTmpLine..cWareCode,;
              Dyelot    WITH &lcTmpLine..Dyelot
      =gfAdd_Info('STYDYE')
      *--Call TraceKey global function.
     =gfTraceKey('STYDYE',STYDYE.style+STYDYE.cwarecode+STYDYE.dyelot,'A')
    ENDIF
  ENDIF

  SELECT (lcTmpLine)

  *E300935,4 ===> New block begin 
  *--Create FabDye Dyelot record in P/O if new for receiving warehouse.------
  *E300935,4 Create FabDye Dyelot record in C/t or P/O if new receiving dyelot.[begin]
  *E300935,4 This new block is By MAB To Update Fabdye and Dye_Rel Records
  *E300935,4 In all cases instead of Inter location records or new lines
  *E300935,4 I detect new Lines By logical Field lCostMade which I add
  *E300935,4 to temporary file [lcTmpName].
  *E301291,4 AMM Add issue/receive adornment PO types 'AE'
  *IF !(lcPType $ 'RNO') AND !lCostMade AND llStkLine AND llDyelot AND Style.cDye_Flg = 'Y'
  
  *E301480,1 NAD (Start) Add the Inter location Po batch to the condition.
  *IF !(lcPType $ 'RNOAE') AND !lCostMade AND llStkLine AND llDyelot AND Style.cDye_Flg = 'Y'
  IF !(lcPType $ 'RNOAEL') AND !lCostMade AND llStkLine AND llDyelot AND Style.cDye_Flg = 'Y'
  *E301480,1 NAD (End)
  
  *E301291,4 AMM end
    lcUpDatFab = ''
    IF &lcTmpLine..Trancd = '2'
      lcFabColor = PADR(lfGetColor(&lcTmpLine..Style),6)    && Get Fabric color
      
      *B606700,1 KHM 12/03/2002 (Begin) Check if the MA installed before calling.
      IF 'MA' $ gcComp_mdl
      *B606700,1 KHM 12/03/2002 (End)
        = lfUpDatFab(lcUpDatFab,lcFabColor,&lcTmpLine..Dyelot,&lcTmpLine..cWareCode)
      *B606700,1 KHM 12/03/2002 (Begin)
      ENDIF
      *B606700,1 KHM 12/03/2002 (End)  
    ELSE    && Trancd = lcOthrTrCd
      lcFabColor = PADR(lfGetColor(&lcTmpLine..cRetSty),6)   && Get Fabric color
      
      *B606700,1 KHM 12/03/2002 (Begin) Check if the MA installed before calling.
      IF 'MA' $ gcComp_mdl
      *B606700,1 KHM 12/03/2002 (End)
        = lfUpDatFab(lcUpDatFab,lcFabColor,&lcTmpLine..Dyelot,&lcTmpLine..cWareCode)
      *B606700,1 KHM 12/03/2002 (Begin)
      ENDIF
      *B606700,1 KHM 12/03/2002 (End)  

    ENDIF  
  ENDIF
  = SEEK(lcCurSty,'STYLE')
  *E300935,4 ===> New block end. 

  *--Update Style Inventory (Start step 2 & Next Step lnNxtStp)---------------
  lnNxtStp = 2
  *IF llStkLine
  *  *--G/L Array difinition and initialization.
  *  *-- Update general ledger entreis in gfStyCrl()
  *  IF llLinkToGl
  *    DECLARE laGLDistAr[2,13]
  *    laGLDistAr[1,1] = lcLinkCode
  *    laGLDistAr[2,1] = lcGLPLkC
  *    laGLDistAr[1,2] = '006'
  *    laGLDistAr[2,2] = '013'
  *    laGLDistAr[1,3] =  1
  *    laGLDistAr[2,3] = -1
  *    STORE IIF(llMFCall,'CT','PO') TO laGLDistAr[1,4],laGLDistAr[2,4]
  *    STORE lcSvCode   TO laGLDistAr[1,5],laGLDistAr[2,5]
  *    STORE ldTrDate   TO laGLDistAr[1,6],laGLDistAr[2,6]
  *    STORE lcGLFYear  TO laGLDistAr[1,7],laGLDistAr[2,7]
  *    STORE lcGlPeriod TO laGLDistAr[1,8],laGLDistAr[2,8]
  *    STORE lcGlDist   TO laGLDistAr[1,9],laGLDistAr[2,9]
  *  ELSE
  *    DIME laGLDistAr[1,1]
  *    laGLDistAr = ''
  *  ENDIF

  *  SELECT (lcTmpLine)
  *  lcJTType = IIF(llMFCall,'5','6') 
  *  lcJrlSty = IIF(TranCd = lcOthrTrCd,cRetSty,Style)
  *  *E300935,4 Now Imported styles support dyelots.
  *  *lcJDyelt = IIF(llMFCall AND llDyelot,Dyelot,SPACE(10)) 
  *  lcJDyelt = IIF(llDyelot,Dyelot,SPACE(10))     
    
  *  lnJSgn   = IIF(lcPType $ 'R',-1,1)
  *  DECLARE laAdjust[9]
  *  FOR I = 1 TO 8
  *    Z=STR(I,1)
  *    laAdjust[I] = (lnJSgn*Qty&Z)
  *  ENDFOR
  *  laAdjust[9] = (lnJSgn*TotQty)

  *  *--Call the global function for update style inventory control.
  *  *E301192 (Start)
  *  *lnNxtStp = gfStyCrl(lcJTType,lcJrlSty,cWareCode,lcJDyelt,ldRcvDate,;
                   lcSvCode,@laAdjust,lnNewCost,Reference,lcGlSession,'',;
                   2,lcTmpLine,'nSteps',@laGLDistAr)
    *E301192 (End)
  *ENDIF

  *--Update Style (Step lnNxtStp)-----------------------------------------------
  =SEEK(&lcTmpLine..Style,'STYLE')
  IF &lcTmpLine..nSteps < lnNxtStp  AND ! &lcTmpLine..lNewLUpd
    lcWipSgn = IIF(lcPType $ 'R','+','-')
    SELECT STYLE
    =RLOCK()
    REPLACE Wip1   WITH Wip1 &lcWipSgn MIN(&lcTmpLine..Qty1, laOpnQty[1]) ,;
            Wip2   WITH Wip2 &lcWipSgn MIN(&lcTmpLine..Qty2, laOpnQty[2]) ,;
            Wip3   WITH Wip3 &lcWipSgn MIN(&lcTmpLine..Qty3, laOpnQty[3]) ,;
            Wip4   WITH Wip4 &lcWipSgn MIN(&lcTmpLine..Qty4, laOpnQty[4]) ,;
            Wip5   WITH Wip5 &lcWipSgn MIN(&lcTmpLine..Qty5, laOpnQty[5]) ,;
            Wip6   WITH Wip6 &lcWipSgn MIN(&lcTmpLine..Qty6, laOpnQty[6]) ,;
            Wip7   WITH Wip7 &lcWipSgn MIN(&lcTmpLine..Qty7, laOpnQty[7]) ,;
            Wip8   WITH Wip8 &lcWipSgn MIN(&lcTmpLine..Qty8, laOpnQty[8]) ,;
            TotWip WITH Wip1+Wip2+Wip3+Wip4+Wip5+Wip6+Wip7+Wip8
    UNLOCK 
    
    *B605411,1 AMH Fix the bug of not update nmatwip in fabric & fabdye files when recv. [Start]
    =lfUpdWip()
    *B605411,1 AMH [End]
    
    SELECT (lcTmpLine)
    =RLOCK()
    REPLACE nSteps WITH lnNxtStp
    UNLOCK
    *--Call TraceKey global function.
    =gfTraceKey('STYLE',STYLE.Style,'M')
  ENDIF 


  *--Update StyDye Warehouse record (Step lnNxtStp+1)-------------------------------
  SELECT STYDYE
  =SEEK(&lcTmpLine..Style+lcOrjWareH+SPACE(10))
  IF &lcTmpLine..nSteps < lnNxtStp+1  AND ! &lcTmpLine..lNewLUpd
    lcWipSgn = IIF(lcPType $ 'R','+','-')
    =RLOCK()
    REPLACE Wip1   WITH Wip1 &lcWipSgn MIN(&lcTmpLine..Qty1, laOpnQty[1]),;
            Wip2   WITH Wip2 &lcWipSgn MIN(&lcTmpLine..Qty2, laOpnQty[2]),;
            Wip3   WITH Wip3 &lcWipSgn MIN(&lcTmpLine..Qty3, laOpnQty[3]),;
            Wip4   WITH Wip4 &lcWipSgn MIN(&lcTmpLine..Qty4, laOpnQty[4]),;
            Wip5   WITH Wip5 &lcWipSgn MIN(&lcTmpLine..Qty5, laOpnQty[5]),;
            Wip6   WITH Wip6 &lcWipSgn MIN(&lcTmpLine..Qty6, laOpnQty[6]),;
            Wip7   WITH Wip7 &lcWipSgn MIN(&lcTmpLine..Qty7, laOpnQty[7]),;
            Wip8   WITH Wip8 &lcWipSgn MIN(&lcTmpLine..Qty8, laOpnQty[8]),;
            TotWip WITH Wip1+Wip2+Wip3+Wip4+Wip5+Wip6+Wip7+Wip8
    UNLOCK 
    
    SELECT (lcTmpLine)
    =RLOCK()
    REPLACE nSteps WITH lnNxtStp+1
    UNLOCK
    *--Call TraceKey global function.
   =gfTraceKey('STYDYE',STYDYE.style+STYDYE.cwarecode+STYDYE.dyelot,'M')
  ENDIF

  *-Update APVendor (Step lnNxtStp+2)-------------------------------
  *E301291,4 AMM Add Receive adornment PO type 'E'
  *IF !llMFCall AND lcPType<>'O' AND SEEK(&lcTmpLine..Vendor,'APVENDOR') 
  
  *E301480,1 NAD (Start) Add the Inter location Po batch to the condition.
  *IF !llMFCall AND !(lcPType$'OE') AND SEEK(&lcTmpLine..Vendor,'APVENDOR') 
  IF !llMFCall AND !(lcPType$'OEL') AND SEEK(&lcTmpLine..Vendor,'APVENDOR') 
  *E301480,1 NAD (End)
  
  *E301291,4 AMM end
    SELECT (lcTmpLine)
    IF lcLstTrn <> Po AND lcPoVend <> Vendor
      STORE 0 TO lnLstPoRAm,lnOpnPoAmt
      lcLstTrn  = Po
      lcPoVend = Vendor
    ENDIF
    lnLstPoRAm = lnLstPoRAm + (TotQty * nELanCost1)
    *C200080,1 AMM Add the dye order type 'D'
    *E301291,4 AMM Add Issue adornment PO type 'A'
    *=SEEK(IIF(lcPType $ 'R','R','P')+Po+Style+STR(LineNo,6)+'1','POSLn')
    =SEEK(IIF(lcPType $ 'RDA',lcPType,'P')+Po+Style+STR(LineNo,6)+'1','POSLn')
    *C200080,1 AMM end
    lnOpnPoAmt = ( MIN((TotQty * nELanCost1),(POSLn.TotQty*POSLn.nECost1)) )
 
    IF &lcTmpLine..nSteps < lnNxtStp+2
      SELECT APVENDOR
      =RLOCK()
      IF lcPType='R'
        REPLACE nVenOpnPo with nVenOpnPo - lnOpnPoAmt
      ELSE
        REPLACE dVenLPoRD WITH ldRcvDate  ,;
                nVenLPoRA with lnLstPoRAm ,;
                nVenOpnPo with nVenOpnPo - lnOpnPoAmt
      ENDIF
      UNLOCK
      SELECT (lcTmpLine)
      =RLOCK()
      REPLACE nSteps WITH lnNxtStp+2
      UNLOCK    
      *--Call TraceKey global function.
      =gfTraceKey('APVENDOR',APVENDOR.cvendcode,'M')
    ENDIF
  ENDIF


  *--Update Header file CuttktH or POSHDR (Step lnNxtStp+3) --------------------------
  lcHdFile=IIF(llMFCall,'CUTTKTH','POSHDR')
  SELECT (lcHdFile)
  =SEEK(lcSvTpe+lcSvCode)
  lcGLPLkC = Link_Code

  lnOpnSub=0
  FOR I=1 TO 8
    lcCnt=STR(I,1)
    lnOpnSub = lnOpnSub + MIN( laOpnQty[I] , &lcTmpLine..Qty&lcCnt )
  ENDFOR
  
  *E301223,1 Read session no for P/O that return P/o was from to be sent in gfstycrl.
  lcFrstSess = IIF(lcPType='R' AND !EMPTY(POSHDR.cPONo),&lcTmpLine..CRSESSION,'')
  *E301223,1 End.
    
  IF &lcTmpLine..nSteps < lnNxtStp+3
    =RLOCK()
    DO CASE
      CASE llStkLine
        IF &lcTmpLine..Trancd = '2'
          IF llMFCall 
            REPLACE Pcs_Rec WITH Pcs_Rec + &lcTmpLine..TotQty,;
                    Pcs_Opn WITH MAX(Pcs_Opn - lnOpnSub,0)
          ELSE
            REPLACE Receive WITH Receive + &lcTmpLine..TotQty,;
                    Open    WITH MAX(Open - lnOpnSub,0)
          ENDIF
        ELSE     &&Trancd = lcOthrTrCd
          IF llMFCall 
            REPLACE Pcs_Dam WITH Pcs_Dam + &lcTmpLine..TotQty,;
                    Pcs_Opn WITH MAX(Pcs_Opn - lnOpnSub,0)
          ELSE
            REPLACE Damage  WITH Damage  + &lcTmpLine..TotQty,;
                    Open    WITH MAX(Open - lnOpnSub,0)
          ENDIF
        ENDIF
        IF llMFCall 
          *-- Update costs in Ct heder.
          *B605456,1 KHM 02/03/2002 (Begin) Rounding the landed cost.
          *REPLACE nlan_cost1 WITH nlan_cost1 +(&lcTmpLine..nlan_cst1 * &lcTmpLine..TotQty),;
                  nlan_cost2 WITH nlan_cost2 +(&lcTmpLine..nlan_cst2 * &lcTmpLine..TotQty),;
                  nlan_cost3 WITH nlan_cost3 +(&lcTmpLine..nlan_cst3 * &lcTmpLine..TotQty),;
                  nlan_cost4 WITH nlan_cost4 +(&lcTmpLine..nlan_cst4 * &lcTmpLine..TotQty),;
                  nlan_cost5 WITH nlan_cost5 +(&lcTmpLine..nlan_cst5 * &lcTmpLine..TotQty)
          
          REPLACE nlan_cost1 WITH nlan_cost1 +(ROUND(&lcTmpLine..nlan_cst1 * &lcTmpLine..TotQty,3)),;
                  nlan_cost2 WITH nlan_cost2 +(ROUND(&lcTmpLine..nlan_cst2 * &lcTmpLine..TotQty,3)),;
                  nlan_cost3 WITH nlan_cost3 +(ROUND(&lcTmpLine..nlan_cst3 * &lcTmpLine..TotQty,3)),;
                  nlan_cost4 WITH nlan_cost4 +(ROUND(&lcTmpLine..nlan_cst4 * &lcTmpLine..TotQty,3)),;
                  nlan_cost5 WITH nlan_cost5 +(ROUND(&lcTmpLine..nlan_cst5 * &lcTmpLine..TotQty,3))
          *B605456,1 KHM 02/03/2002 (End)
        ELSE
          *-- Update costs in PO heder.
          *B602861,1
          *REPLACE nFlanCost1 WITH nFlanCost1 +(&lcTmpLine..nlan_cst1 * &lcTmpLine..TotQty),;
                  nFlanCost2 WITH nFlanCost2 +(&lcTmpLine..nlan_cst2 * &lcTmpLine..TotQty),;
                  nFlanCost3 WITH nFlanCost3 +(&lcTmpLine..nlan_cst3 * &lcTmpLine..TotQty),;
                  nFlanCost4 WITH nFlanCost4 +(&lcTmpLine..nlan_cst4 * &lcTmpLine..TotQty),;
                  nFlanCost5 WITH nFlanCost5 +(&lcTmpLine..nlan_cst5 * &lcTmpLine..TotQty),;
                  nlan_cost1 WITH nlan_cost1 +(&lcTmpLine..nElanCost1 * &lcTmpLine..TotQty),;
                  nlan_cost2 WITH nlan_cost2 +(&lcTmpLine..nElanCost2 * &lcTmpLine..TotQty),;
                  nlan_cost3 WITH nlan_cost3 +(&lcTmpLine..nElanCost3 * &lcTmpLine..TotQty),;
                  nlan_cost4 WITH nlan_cost4 +(&lcTmpLine..nElanCost4 * &lcTmpLine..TotQty),;
                  nlan_cost5 WITH nlan_cost5 +(&lcTmpLine..nElanCost5 * &lcTmpLine..TotQty),;
                  nTot_Cost  WITH nlan_cost1+nlan_cost2+nlan_cost3+nlan_cost4+nlan_cost5
          
          *B605456,1 KHM 02/03/2002 (Begin) Rounding the landed cost.
           *REPLACE nFlanCost2 WITH nFlanCost2 +(&lcTmpLine..nlan_cst2 * &lcTmpLine..TotQty),;
                  nFlanCost3 WITH nFlanCost3 +(&lcTmpLine..nlan_cst3 * &lcTmpLine..TotQty),;
                  nFlanCost4 WITH nFlanCost4 +(&lcTmpLine..nlan_cst4 * &lcTmpLine..TotQty),;
                  nFlanCost5 WITH nFlanCost5 +(&lcTmpLine..nlan_cst5 * &lcTmpLine..TotQty),;
                  nlan_cost2 WITH nlan_cost2 +(&lcTmpLine..nElanCost2 * &lcTmpLine..TotQty),;
                  nlan_cost3 WITH nlan_cost3 +(&lcTmpLine..nElanCost3 * &lcTmpLine..TotQty),;
                  nlan_cost4 WITH nlan_cost4 +(&lcTmpLine..nElanCost4 * &lcTmpLine..TotQty),;
                  nlan_cost5 WITH nlan_cost5 +(&lcTmpLine..nElanCost5 * &lcTmpLine..TotQty)
           REPLACE nFlanCost2 WITH nFlanCost2 +(ROUND(&lcTmpLine..nlan_cst2 * &lcTmpLine..TotQty,3)),;
                   nFlanCost3 WITH nFlanCost3 +(ROUND(&lcTmpLine..nlan_cst3 * &lcTmpLine..TotQty,3)),;
                   nFlanCost4 WITH nFlanCost4 +(ROUND(&lcTmpLine..nlan_cst4 * &lcTmpLine..TotQty,3)),;
                   nFlanCost5 WITH nFlanCost5 +(ROUND(&lcTmpLine..nlan_cst5 * &lcTmpLine..TotQty,3)),;
                   nlan_cost2 WITH nlan_cost2 +(ROUND(&lcTmpLine..nElanCost2 * &lcTmpLine..TotQty,3)),;
                   nlan_cost3 WITH nlan_cost3 +(ROUND(&lcTmpLine..nElanCost3 * &lcTmpLine..TotQty,3)),;
                   nlan_cost4 WITH nlan_cost4 +(ROUND(&lcTmpLine..nElanCost4 * &lcTmpLine..TotQty,3)),;
                   nlan_cost5 WITH nlan_cost5 +(ROUND(&lcTmpLine..nElanCost5 * &lcTmpLine..TotQty,3))
          *B605456,1 KHM 02/03/2002 (End)
          
          IF &lcTmpLine..nElanCost1 = ROUND(&lcTmpLine..Gros_Price*(1-&lcTmpLine..Disc_Pcnt/100),3)
            *B605456,1 KHM 02/03/2002 (Begin) Rounding the landed cost.
            *REPLACE nFlanCost1 WITH nFlanCost1 +(&lcTmpLine..nlan_cst1 * &lcTmpLine..TotQty),;
                    nlan_cost1 WITH nlan_cost1 +((&lcTmpLine..Gros_Price*(1-&lcTmpLine..Disc_Pcnt/100))* &lcTmpLine..TotQty)
            REPLACE nFlanCost1 WITH nFlanCost1 +(ROUND(&lcTmpLine..nlan_cst1 * &lcTmpLine..TotQty,3)),;
                    nlan_cost1 WITH nlan_cost1 +(ROUND((&lcTmpLine..Gros_Price*(1-&lcTmpLine..Disc_Pcnt/100))* &lcTmpLine..TotQty,3))
            *B605456,1 KHM 02/03/2002 (End) 
          ELSE
            *B605456,1 KHM 02/03/2002 (Begin) Rounding the landed cost.
            *REPLACE nFlanCost1 WITH nFlanCost1 +(&lcTmpLine..nlan_cst1 * &lcTmpLine..TotQty),;
                    nlan_cost1 WITH nlan_cost1 +(&lcTmpLine..nElanCost1 * &lcTmpLine..TotQty)
            REPLACE nFlanCost1 WITH nFlanCost1 +(ROUND(&lcTmpLine..nlan_cst1 * &lcTmpLine..TotQty,3)),;
                    nlan_cost1 WITH nlan_cost1 +(ROUND(&lcTmpLine..nElanCost1 * &lcTmpLine..TotQty,3))
            *B605456,1 KHM 02/03/2002 (End) 
          ENDIF
          REPLACE nTot_Cost  WITH nlan_cost1+nlan_cost2+nlan_cost3+nlan_cost4+nlan_cost5
        ENDIF 

      CASE &lcTmpLine..Trancd = lcCanlTrCd
        IF llMFCall 
          REPLACE Pcs_Can WITH Pcs_Can + &lcTmpLine..TotQty,;
                  Pcs_Opn WITH MAX((Pcs_Opn - lnOpnSub),0)
        ELSE
          REPLACE Cancel  WITH Cancel  + &lcTmpLine..TotQty,;
                  Open    WITH MAX((Open    - lnOpnSub),0)
        ENDIF
    ENDCASE

    IF ( llMFCall AND Pcs_Opn = 0 ) OR ( !llMFCall AND Open = 0 )
      REPLACE Status with 'C'
      
      *C037345,1 ABD - Remark the next few lines and move down to update the style CP in case complete PO or Not. [Begin]
      *C200376,1 AMH Trigger for customer PUFFA to update the style critical path file [Start]
      *IF lcPType='I' .AND. ASCAN(laEvntTrig , PADR('STYCRUPD',10)) <> 0
      *  =gfDoTriger('POSTREC',PADR('STYCRUPD',10))
      *ENDIF
      *C200376,1 AMH [End]
      *C037345,1 ABD - [End]
      
    ENDIF

    *C037345,1 ABD - Trigger for customer PUFFA to update the style critical path file. [BEgin]
    IF lcPType='I' .AND. ASCAN(laEvntTrig , PADR('STYCRUPD',10)) <> 0
      =gfDoTriger('POSTREC',PADR('STYCRUPD',10))
    ENDIF
    *C037345,1 ABD - [End]

    *E301077,11 Update Odded fields using gfAdd_Info Function [Begin]
    *Odded REPLACE Mod_action WITH 'RECEIVED',;
    *Odded         Mod_Date   WITH gdSysDate ,;
    *Odded         Mod_Time   WITH TIME()    ,;
    *Odded         Mod_UserId WITH gcUser_id

    =gfAdd_Info(lcHdFile)
    *E301077,11 Update Odded fields using gfAdd_Info Function [End  ]

    UNLOCK
    SELECT (lcTmpLine)
    =RLOCK()
    REPLACE nSteps WITH lnNxtStp+3
    UNLOCK    
    *--Call TraceKey global function.
    IF llMFCall 
      =gfTraceKey('CUTTKTH',CUTTKTH.Cuttkt,'M')
    ELSE
      =gfTraceKey('POSHDR',POSHDR.cStyType+POSHDR.Po,'M')
    ENDIF
  ENDIF



  *--Update Line in CuttktL or PoSLn (lnNxtStp+4)---------------------------
  SELECT (lcTmpLine)
  IF &lcTmpLine..nSteps < lnNxtStp+4
    SCATTER MEMVAR
    SELECT (lcMasterLn)
    APPEND BLANK
    GATHER MEMVAR
    REPLACE Date      WITH ldRcvDate,;
            dPostDate WITH ldTrDate ,;
            cOwner    WITH ' '
    *--Changing receiving from customer to location.
    IF !llMFCall AND cWareCode <> lcDropLoc AND !EMPTY(Account)
       REPLACE Account WITH SPACE(5),;
               Store   WITH SPACE(8)
    ENDIF
    IF llStkLine
      REPLACE cRSession WITH lcGlSession
    ENDIF
    *C128583,5  TMI [Start] Update the field shipno in case of receiving by batch for PAN21
    IF lcPType = 'B'
      REPLACE SHIPNO WITH CTKTRCVH.SHIPNO
    ENDIF  
    *C128583,5  TMI [End  ] 
    =gfAdd_Info(lcMasterLn)
    SELECT (lcTmpLine)
    =RLOCK()
    REPLACE nSteps WITH lnNxtStp+4
    UNLOCK     
    *--Call TraceKey global function.
    IF llMFCall 
      =gfTraceKey('CUTTKTL',CUTTKTL.cuttkt+CUTTKTL.style+CUTTKTL.dyelot+CUTTKTL.trancd,'A')
    ELSE
      *=gfTraceKey('POSLN',POSLN.cstytype+POSLN.po+POSLN.style+STR(POSLN.lineno,6)+POSLN.trancd,'A')
      SELECT POSLN
      =gfTraceKey('POSLN',cStyType+Po+cRsession+Shipno+Style+STR(Lineno,6)+Trancd,'A')
    ENDIF
  ENDIF


  *--Update BomLine (Step lnNxtStp+5)-------------------------------------------------
  SELECT (lcTmpLine)

  *B602554,1 Add landed cost records in the bomline even for if the system is 
  *B602554,1 not setup to use detail costing for importing styles.
  *IF llStkLine AND ( llMFCall OR (llImpCost AND !(lcPType $ 'RO')) )
  *E301291,4 AMM Add receive adornment PO type 'E'
  *IF llStkLine AND ( llMFCall OR !(lcPType $ 'RO'))
  
  *E301480,1 NAD (Start) Add the Inter location Po batch to the condition.
  *IF llStkLine AND ( llMFCall OR !(lcPType $ 'ROE'))
  IF llStkLine AND ( llMFCall OR !(lcPType $ 'ROEL'))
  *E301480,1 NAD (End)
  
  *E301291,4 AMM end
  *B602554,1 (End)
  
    IF llMFCall
      lcBomLKey = 'M2'+Cuttkt
      lcWhileCn = "cIMTyp+cType+cTktNo=lcBomLKey"
    ELSE
      *khm1
      *lcBomLKey = 'I2'+IIF(lcPType='S',Shipno,'')+Po+STR(LineNo,6)    
      lcBomLKey = IIF(lcPType='D','D','I')+'2'+IIF(lcPType='S',Shipno,'')+Po+STR(LineNo,6)    
      *khm1
      lcWhileCn = "cIMTyp+cType+IIF(lcPType='S',Shipno,'')+cTktNo+STR(LineNo,6)=lcBomLKey"
    ENDIF

    *B803337,1 WMA [Begin]
    *E301077,11 MAB Open BOMLINE file here [Begin
    *IF !llOpnBomLn
    *  llOpnBomLn = lfOpn_Rest(gcDatadir,'BOMLINE','BOMLINE')
    *ENDIF  
    *E301077,11 MAB Open BOMLINE file here [End..
    *B803337,1 WMA [End]

    SELECT BOMLINE
    IF lcPType ='S'
      SET ORDER TO TAG BomLnShp
    ELSE
      SET ORDER TO TAG BomLine
    ENDIF
    IF SEEK(lcBomLKey) AND &lcTmpLine..nSteps < lnNxtStp+5
*B602966,1 Dont update the qty because it already updated in LFGETLANDED().
*     REPLACE REST StyQty    WITH  &lcTmpLine..TotQty,;
                   ItemQty   WITH (&lcTmpLine..TotQty*UnitQty),;
                   ItemAmt   WITH (&lcTmpLine..Totqty*UnitQty)*UnitCost,;
                   cRSession WITH lcGlSession
      
      *C102469,1 AMH Save the last landed cost of PO or C/T for recieved records [Start]
      *C102469,1     by changing the Replace rest with SCAN REST 
      *REPLACE REST cRSession WITH lcGlSession ;
             WHILE &lcWhileCn ;
               FOR Style = &lcTmpLine..Style AND EMPTY(cRSession) AND cStyGrade = &lcTmpLine..cStyGrade AND ;
                gfTraceKey('BOMLINE',cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode,'M')
      SCAN REST WHILE &lcWhileCn ;
                FOR Style = &lcTmpLine..Style AND EMPTY(cRSession) AND cStyGrade = &lcTmpLine..cStyGrade AND ;
                gfTraceKey('BOMLINE',cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode,'M')
        REPLACE cRSession WITH lcGlSession
        SCATTER MEMVAR
        STORE 0 TO m.StyQty,m.ItemQty,m.ItemAmt
        m.cRSession = ''
        SELECT (TmpBomLine)
        APPEND BLANK
        GATHER MEMVAR
      ENDSCAN
      *C102469,1 AMH [End]
      
*B602966,1 End.
      SELECT (lcTmpLine)
      =RLOCK()      
      REPLACE nSteps WITH lnNxtStp+5
      UNLOCK    
    ENDIF
  ENDIF

  *--Update MfgOprDt file (Step lnNxtStp+6)--------------------------------------
  SELECT (lcTmpLine)
  lcLastOpr = clastopr
  *C200080,1 AMM Add the dye order type 'D'
  *IF !EMPTY(lcLastOpr) AND lcPType $ 'IM' AND &lcTmpLine..nSteps < lnNxtStp+6
  IF !EMPTY(lcLastOpr) AND lcPType $ 'IMD' AND &lcTmpLine..nSteps < lnNxtStp+6
  *C200080,1 AMM end
    SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty TO laRecvQty
    IF laRecvQty[9] > 0 
      *B802635,1 AMM Add the dyelot field
      *=lfUpdLot(lcSvCode,lcLastOpr,&lcTmpLine..cLotNo,'laRecvQty',&lcTmpLine..Style)
      =lfUpdLot(lcSvCode,lcLastOpr,&lcTmpLine..cLotNo,'laRecvQty',&lcTmpLine..Style+&lcTmpLine..Dyelot)
      *B802635,1 AMM end
    ENDIF
    SELECT (lcTmpLine)
    =RLOCK()
    REPLACE nSteps WITH lnNxtStp+6
    UNLOCK    
  ENDIF
  *--Update Shipment In-Transit P/o line qty. ------------
  SELECT (lcTmpLine)
  *E301220,1 Update shipment line record if receive batch by shipment.
  *IF lcPType = 'S' 
  IF lcPType = 'S' OR (lcPType = 'B' AND !EMPTY(CTKTRCVH.ShipNo) )
    IF (lcPType = 'B' AND !EMPTY(CTKTRCVH.ShipNo) )
      lcShpCode = CTKTRCVH.ShipNo
    ENDIF
  *E301220,1 End.
    lcShpLine = lcShpCode+'P'+PO+Style+STR(LineNo,6)+'3'
    SELECT POSLN
    SET ORDER TO TAG Poslnsh
    =SEEK(lcShpLine)
    SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 TO laSHPQty
    =SEEK(POSLN.Style,'STYLE')
    =SEEK(POSLN.Style+POSLN.cWareCode+SPACE(10),'STYDYE')
    IF &lcTmpLine..nSteps < lnNxtStp + 7
      =RLOCK()
      REPLACE Qty1 WITH MAX(laSHPQty[1]-&lcTmpLine..Qty1,0),;
              Qty2 WITH MAX(laSHPQty[2]-&lcTmpLine..Qty2,0),;
              Qty3 WITH MAX(laSHPQty[3]-&lcTmpLine..Qty3,0),;
              Qty4 WITH MAX(laSHPQty[4]-&lcTmpLine..Qty4,0),;
              Qty5 WITH MAX(laSHPQty[5]-&lcTmpLine..Qty5,0),;
              Qty6 WITH MAX(laSHPQty[6]-&lcTmpLine..Qty6,0),;
              Qty7 WITH MAX(laSHPQty[7]-&lcTmpLine..Qty7,0),;
              Qty8 WITH MAX(laSHPQty[8]-&lcTmpLine..Qty8,0),;
              TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
      UNLOCK
      SELECT (lcTmpLine)
      =RLOCK()
      REPLACE nSteps WITH lnNxtStp + 7
      UNLOCK    
      *--Call TraceKey global function.
      *=gfTraceKey('POSLN',POSLN.cstytype+POSLN.po+POSLN.style+STR(POSLN.lineno,6)+POSLN.trancd,'M')
      SELECT POSLN
      =gfTraceKey('POSLN',cStyType+Po+cRsession+Shipno+Style+STR(Lineno,6)+Trancd,'M')
    ENDIF
    SELECT POSLN
    SET ORDER TO TAG Posln
  ENDIF

  *--Update Inter Location In-Transit P/o line qty. ------------
  SELECT (lcTmpLine)
  *E301291,4 AMM Add receive adornment PO type 'E'
  *IF lcPType = 'O'
  
  *E301480,1 NAD (Start) Add the Inter location Po batch to the condition.
  *IF lcPType $ 'OE'
  IF lcPType $ 'OEL'
  *E301480,1 NAD (End)
  
  *E301291,4 AMM end
    SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 TO laSHPQty
    IF &lcTmpLine..nSteps < lnNxtStp + 7
      *--Get the received line qty.
      SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 TO laAcLRQy
      *E301291,4 AMM Consider receive adornment PO case
      *lcShpLine = 'N'+Po+Style+STR(Lineno,6)+'6'
      
      *E301480,1 NAD (Start) Add the Inter location Po batch to the condition.
      *lcShpLine = IIF(lcPType='O','N','A')+Po+Style+STR(Lineno,6)+'6'
      *B608056,1 TMI [Start] Add the inter location PO batch to the condition correctly
      *lcShpLine = IIF(lcPType='OL','N','A')+Po+Style+STR(Lineno,6)+'6'
      lcShpLine = IIF(lcPType $ 'OL','N','A')+Po+Style+STR(Lineno,6)+'6'
      *B608056,1 TMI [End  ] 
      *E301480,1 NAD (End)
      
      *E301291,4 AMM end
      SELECT POSLN
      =SEEK(lcShpLine)
      SCAN WHILE cStyType+Po+Style+STR(Lineno,6)+Trancd = lcShpLine FOR TotQty <> 0
        =RLOCK()
        FOR I=1 TO 8
          IF laAcLRQy[I]<=0
            LOOP 
          ENDIF
          Z=STR(I,1)
          lnQty&z = Qty&Z
          REPLACE Qty&Z WITH MAX(Qty&Z - laAcLRQy[I],0)
          laAcLRQy[I] = laAcLRQy[I] - lnQty&z
        ENDFOR
        REPLACE TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
        UNLOCK
        SELECT POSLN
        =gfTraceKey('POSLN',cStyType+Po+cRsession+Shipno+Style+STR(Lineno,6)+Trancd,'M')
      ENDSCAN 
      SELECT (lcTmpLine)
      =RLOCK()
      REPLACE nSteps WITH lnNxtStp + 7
      UNLOCK    
    ENDIF
    
  ENDIF
  
  *--Update Intransit line for Receive by Shipment or Receive inter Location P/o.
  *E301291,4 AMM Consider receive adornment order case
  *IF lcPType $ 'SO'
  
  *E301480,1 NAD (Start) Add the Inter location Po batch to the condition.
  *IF lcPType $ 'SOE'
  *IF lcPType $ 'SOEL' commented by TMI *C128583,4  09/01/2005
  *E301480,1 NAD (End)
  *C128583,4  TMI [Start] include Batch type from shipment
  IF lcPType $ 'SOEL' .OR. ( lcPType $ 'B' .AND. POSLN.TRANCD = '3' )
  *C128583,4  TMI [End  ] 
  
  *E301291,4 AMM end
    IF &lcTmpLine..nSteps < lnNxtStp + 8
      SELECT STYLE
      =SEEK(&lcTmpLine..Style,'STYLE')
      =RLOCK()
      REPLACE InTrans1 WITH InTrans1 - MIN(laSHPQty[1],&lcTmpLine..Qty1),;
              InTrans2 WITH InTrans2 - MIN(laSHPQty[2],&lcTmpLine..Qty2),;
              InTrans3 WITH InTrans3 - MIN(laSHPQty[3],&lcTmpLine..Qty3),;
              InTrans4 WITH InTrans4 - MIN(laSHPQty[4],&lcTmpLine..Qty4),;
              InTrans5 WITH InTrans5 - MIN(laSHPQty[5],&lcTmpLine..Qty5),;
              InTrans6 WITH InTrans6 - MIN(laSHPQty[6],&lcTmpLine..Qty6),;
              InTrans7 WITH InTrans7 - MIN(laSHPQty[7],&lcTmpLine..Qty7),;
              InTrans8 WITH InTrans8 - MIN(laSHPQty[8],&lcTmpLine..Qty8),;
              TotInTrn WITH InTrans1+InTrans2+InTrans3+InTrans4+InTrans5+InTrans6+InTrans7+InTrans8

      UNLOCK
      SELECT (lcTmpLine)
      =RLOCK()
      REPLACE nSteps WITH lnNxtStp + 8
      UNLOCK    
      *--Call TraceKey global function.
      =gfTraceKey('STYLE',STYLE.Style,'M')
    ENDIF
    IF &lcTmpLine..nSteps < lnNxtStp + 9
      SELECT STYDYE
      =SEEK(&lcTmpLine..Style+&lcTmpLine..cWareCode+SPACE(10))
      =RLOCK()
      REPLACE InTrans1 WITH InTrans1 - MIN(laSHPQty[1],&lcTmpLine..Qty1),;
              InTrans2 WITH InTrans2 - MIN(laSHPQty[2],&lcTmpLine..Qty2),;
              InTrans3 WITH InTrans3 - MIN(laSHPQty[3],&lcTmpLine..Qty3),;
              InTrans4 WITH InTrans4 - MIN(laSHPQty[4],&lcTmpLine..Qty4),;
              InTrans5 WITH InTrans5 - MIN(laSHPQty[5],&lcTmpLine..Qty5),;
              InTrans6 WITH InTrans6 - MIN(laSHPQty[6],&lcTmpLine..Qty6),;
              InTrans7 WITH InTrans7 - MIN(laSHPQty[7],&lcTmpLine..Qty7),;
              InTrans8 WITH InTrans8 - MIN(laSHPQty[8],&lcTmpLine..Qty8),;
              TotInTrn WITH InTrans1+InTrans2+InTrans3+InTrans4+InTrans5+InTrans6+InTrans7+InTrans8
      UNLOCK
      SELECT (lcTmpLine)
      =RLOCK()
      REPLACE nSteps WITH lnNxtStp + 9
      UNLOCK    
      *--Call TraceKey global function.
     =gfTraceKey('STYDYE',STYDYE.style+STYDYE.cwarecode+STYDYE.dyelot,'M')
    ENDIF
  ENDIF


  *--Update Style Inventory (Start step 2 & Next Step lnNxtStp)---------------
  lnNxtStp = lnNxtStp + 10
  IF llStkLine
    *--G/L Array difinition and initialization.
    *-- Update general ledger entreis in gfStyCrl()
    IF llLinkToGl
      DECLARE laGLDistAr[2,13]
      laGLDistAr[1,1] = lcLinkCode
      laGLDistAr[2,1] = lcGLPLkC
      laGLDistAr[1,2] = '006'
      laGLDistAr[2,2] = '013'
      laGLDistAr[1,3] =  1
      laGLDistAr[2,3] = -1
      STORE IIF(llMFCall,'CT','PO') TO laGLDistAr[1,4],laGLDistAr[2,4]
      STORE lcSvCode   TO laGLDistAr[1,5],laGLDistAr[2,5]
      STORE ldTrDate   TO laGLDistAr[1,6],laGLDistAr[2,6]
      STORE lcGLFYear  TO laGLDistAr[1,7],laGLDistAr[2,7]
      STORE lcGlPeriod TO laGLDistAr[1,8],laGLDistAr[2,8]
      STORE lcGlDist   TO laGLDistAr[1,9],laGLDistAr[2,9]
    ELSE
      DIME laGLDistAr[1,1]
      laGLDistAr = ''
    ENDIF

    SELECT (lcTmpLine)
    lcJTType = IIF(llMFCall,'5','6') 
    lcJrlSty = IIF(TranCd = lcOthrTrCd,cRetSty,Style)
    *E300935,4 Now Imported styles support dyelots.
    *lcJDyelt = IIF(llMFCall AND llDyelot,Dyelot,SPACE(10)) 
    lcJDyelt = IIF(llDyelot,Dyelot,SPACE(10))     
    
    lnJSgn   = IIF(lcPType $ 'R',-1,1)
    DECLARE laAdjust[9]
    FOR I = 1 TO 8
      Z=STR(I,1)
      laAdjust[I] = (lnJSgn*Qty&Z)
    ENDFOR
    laAdjust[9] = (lnJSgn*TotQty)

    *--Call the global function for update style inventory control.
    *E301192 (Start)
    PRIVATE lcRefer
    IF llMFCall
      SELECT mfgoprdt
      =SEEK('M'+lcSvCode+&lcTmpLine..clastopr+&lcTmpLine..cLotNo+'2')
      LOCATE REST WHILE cIMTyp+cTktNo+cOprCode+TranCd = ;
                 'M'+lcSvCode+&lcTmpLine..clastopr+&lcTmpLine..cLotNo+'2' ;
                  FOR Item = &lcTmpLine..Style
      ****
      lcRefer = "VEN. " + mfgoprdt.CCONTCODE+ mfgoprdt.CCONTNAME
    ELSE
      *ahmed
      *lcRefer = "VEN. " + APVendor.cVendCode + APVendor.cVenComp
      IF !(lcPType$'E')
        lcRefer = "VEN. " + APVendor.cVendCode + APVendor.cVenComp
      ELSE
        lcRefer = SPACE(0)
      ENDIF
      *ahmed end
    ENDIF

    *E301200,1 (Start).
*    lnNxtStp = gfStyCrl(lcJTType,lcJrlSty,cWareCode,lcJDyelt,ldRcvDate,;
                   lcSvCode,@laAdjust,lnNewCost,lcRefer,lcGlSession,'',;
                   lnNxtStp,lcTmpLine,'nSteps',@laGLDistAr)

    *E301223,1 Added to send a new parameter for P/o receiving session.
    *E301223,1 (Empty in all cases except issue return P/o).
*   lnNxtStp = gfStyCrl(lcJTType,lcJrlSty,cWareCode,lcJDyelt,ldRcvDate,;
                   lcSvCode,@laAdjust,lnNewCost,lcRefer,lcGlSession,'',;
                   lnNxtStp,lcTmpLine,'nSteps',@laGLDistAr,&lcTmpLine..LineNo)
    
    *B038753,1 KHM 12/18/2004 Send transaction cost "lnOrgCost" to be be saved in 
    *B038753,1                StyInvJl.nTranCost [Begin]

    *lnNxtStp = gfStyCrl(lcJTType,lcJrlSty,cWareCode,lcJDyelt,ldRcvDate,;
                   lcSvCode,@laAdjust,lnNewCost,lcRefer,lcGlSession,'',;
                   lnNxtStp,lcTmpLine,'nSteps',@laGLDistAr,;
                   &lcTmpLine..LineNo,lcFrstSess)

    lnNxtStp = gfStyCrl(lcJTType,lcJrlSty,cWareCode,lcJDyelt,ldRcvDate,;
                   lcSvCode,@laAdjust,lnNewCost,lcRefer,lcGlSession,'',;
                   lnNxtStp,lcTmpLine,'nSteps',@laGLDistAr,;
                   &lcTmpLine..LineNo,lcFrstSess,.F.,.F.,lnOrgCost)
    
    *B128128,1 HBG 11/20/2005 Update GLDist with the variance if the cost is changed [Begin]
    *B608121,1  TMI [Start] Add the check if the company is linked to GL
    *IF lnOrgCost <> lnNewCost
    IF lnOrgCost <> lnNewCost .AND. llLinkToGl
    *B608121,1  TMI [End  ]     
      = gfOpenFile(gcDataDir+'GL_LINK','Gl_link','SH')
      lcGLTrnTyp = IIF(gcAct_Appl=="MF",'CT','PO')
      =SEEK(IIF(!EMPTY(&lcHdFile..Link_Code),&lcHdFile..Link_Code,'DEFDEF')+'022','GL_LINK')
      DO GlDist WITH IIF(!EMPTY(&lcHdFile..Link_Code),&lcHdFile..Link_Code,'DEFDEF'),IIF(EMPTY(GL_LINK.GlAcnt),'019','022'),;
                      IIF(llMFCall,CUTTKTL.totQty,POSLN.totqty)*(lnOrgCost-lnNewCost),lcGLTrnTyp,lcSvCode,gdSysDate,;
                      lcGlFYear,lcGlPeriod,lcGlDist
    ENDIF
    *B128128,1 [End]
    *B038753,1 KHM 12/18/2004 [End]
    
     *C200444,1 ALB Add GL entries when recieving PO [Begin]
     IF ASCAN(laEvntTrig , PADR('UPDTRCGL',10)) <> 0
       =gfDoTriger('POSTREC',PADR('UPDTRCGL',10))
     ENDIF  
     *C200444,1 ALB Add GL entries when recieving PO [end]

     *C123853,1 MHM 01/15/2005 Validate Bin Location [Start]
     IF ASCAN(laEvntTrig,PADR("DLSBNPOR",10)) <> 0 
       =gfDoTriger("POSTREC",PADR("DLSBNPOR",10))
     ENDIF
    *C123853,1 MHM 01/15/2005 [End]

    *E301291,4 AMM (start) 
    *B603151,1 Start, Added condition to the following part.
    IF lcPType = 'E'  AND llLinkToGl
    *B603151,1 End.
      lcBomType = '3'
      *FOR lnI = 18 TO 22
      *  IF laSetups[lnI,2] = 'M'
      *    lcBomType = RIGHT(laSetups[lnI,1],1)
      *    EXIT
      *  ENDIF
      *ENDFOR

      FOR lnI = 1 TO 5
        lcI = STR(lnI,1)
        IF lcIType&lcI = 'M'
          lcBomType = lcI
          EXIT
        ENDIF
      ENDFOR

      IF !llOpnBmCst
        llOpnBmCst = lfOpn_Rest(gcDatadir,'BOMCOST','BOMCOST')
      ENDIF  

      SELECT BomCost
      APPEND BLANK
      REPLACE cTktNo       WITH lcSvCode                                 ,;
              MfgCode      WITH '**'                                     ,;
              NTotQty      WITH &lcTmpLine..TotQty                       ,;
              nUNitCst     WITH &lcTmpLine..nCost2                       ,;
              nTotCst      WITH &lcTmpLine..nCost2*&lcTmpLine..TotQty    ,;
              nUnitACst    WITH &lcTmpLine..nAct_Cst2                    ,;
              nTotACst     WITH &lcTmpLine..nAct_Cst2*&lcTmpLine..TotQty ,;
              dTranDate    WITH ldRcvDate                                ,;
              cISession    WITH lcGlSession                              ,;
              cCostType    WITH 'M'                                      ,;
              cBOMType     WITH lcBomType                                ,;
              cIMTyp       WITH 'M'                                      ,;
              Actualize    WITH 'Y'                                      

      DO GLDIST WITH lcGLPLkC,'018',-BOMCost.nTotCst,'NL',lcSvCode,ldTrDate,;
                     lcGLFYear,lcGlPeriod,lcGlDist,''
      SELECT BOMCost
      REPLACE cLbltyAcnt WITH &lcGlDist..GLAccount

      DO GLDIST WITH lcGLPLkC,'013',BOMCost.nTotCst,'NL',lcSvCode,ldTrDate,;
                     lcGLFYear,lcGlPeriod,lcGlDist,''
      SELECT BOMCost
      REPLACE cWIPAcnt WITH &lcGlDist..GLAccount
    ENDIF
    *E301291,4 AMM end

    *E301223,1 (End).
    *E301200,1 (End)
    *E301192,1 (End)
  ENDIF

  *E301484,1 WAM Send Inter Location PO or Purchase orders to sites
  IF lcPType = 'L' AND !llFromEDI AND 'NC' $ gcCmpModules
    =SEEK(PADR(&lcTmpLine..Vendor,6),'WAREHOUS')
    IF WAREHOUS.cSiteId = gcCurSite 
      =SEEK(&lcTmpLine..cwarecode,'WAREHOUS')
      SELECT EDiAcPrt
      LOCATE FOR cSiteId = WAREHOUS.cSiteId
      IF FOUND() AND EdiAcPrt.lInterComp AND SEEK(EdiAcPrt.cPartCode+'856','EdiPd')
        SELECT EdiTrans
        *E037853,1 HBG 16/02/2004 Change the width of Key field in EDITRANS to 40 char
        *IF !SEEK('856'+PADR(&lcTmpLine..Po,20)+EdiAcPrt.Type+EdiAcPrt.cPartner)
        IF !SEEK('856'+PADR(&lcTmpLine..Po,40)+EdiAcPrt.Type+EdiAcPrt.cPartner)
        *E037853,1 [End]
          INSERT INTO ('EDITRANS') (CEDITRNTYP,KEY,TYPE,CPARTNER,lInterComp) VALUES ;
                                   ('856',&lcTmpLine..Po,EdiAcPrt.Type,EdiAcPrt.cPartner,EdiAcPrt.lInterComp)
        ENDIF
        REPLACE cStatus WITH 'N'
        =gfAdd_Info('EDITRANS')
      ENDIF
    ENDIF
  ENDIF
  *E301484,1 WAM (End)

ENDSCAN


*--Update Audit Trail.
*B603237,1 Added to update an audit trail.
IF !llFromEdi AND ASCAN(laEvntTrig,PADR("RECV_ADO",10)) <> 0
  SELECT (lcTmpLine)
  SELECT DIST PO,SHIPNO,PADR(REFERENCE,6) AS ORDER FROM (lcTmpLine) INTO CURSOR 'POAudtTl'
  SELECT POAudtTl
  SCAN
    =gfDoTriger('MFRCVAR',PADR("RECV_ADO",10))
  ENDSCAN
  USE
ENDIF
*B603237,1 End.


*--Header steps will use the first line nsteps field.
SELECT (lcTmpLine)
GO TOP

*--Update GLDist (lnNxtStp + 10)-------------------------------------------------
IF llLinkToGl AND &lcTmpLine..nSteps < lnNxtStp + 10
  WAIT WINDOW 'Updating General Ledger Distribution File ' NOWAIT
  SELECT (lcGlDist)
  REPLACE ALL GlSession with lcGlSession
  USE
  SELECT GlDist
  APPEND FROM (gcWorkDir+lcGlDist)
  ERASE (gcWorkDir+lcGLDist+'.DBF')
  ERASE (gcWorkDir+lcGLDist+'.CDX')

  SELECT (lcTmpLine)
  =RLOCK()
  REPLACE nSteps WITH lnNxtStp + 10
  UNLOCK  
ENDIF
WAIT CLEAR

*--Update BomLine for empty session no (lnNxtStp + 11)-----------------------------
*Khlid start.
*IF ( llMFCall OR (llImpCost AND !(lcPType $'R'))) AND &lcTmpLine..nSteps < lnNxtStp+11
IF ( llMFCall OR !(lcPType $'ROEL')) AND &lcTmpLine..nSteps < lnNxtStp+11
*Khlid end.

  *B803337,1 WMA [Begin]
  *E301077,11 MAB Open BOMLINE file here [Begin
  *IF !llOpnBomLn
  *  llOpnBomLn = lfOpn_Rest(gcDatadir,'BOMLINE','BOMLINE')
  *ENDIF  
  *E301077,11 MAB Open BOMLINE file here [End..
  *B803337,1 WMA [End]

  *604128,1 ABD Open BOMLINE file to Fix Bug Alias Not Found. [Begin]
  =gfOpenFile(gcDataDir+'BOMLINE','BOMLINE','SH')
  *604128,1 ABD [End]

  SELECT BOMLINE
  IF lcPType='S'
    SET ORDER TO TAG BomlnShp
    IF SEEK('I2'+lcShpCode)
      
      *C102469,1 AMH Save the last landed cost of PO or C/T for not recieved records [Start]
      *DELETE REST WHILE cIMTyp+cType+ShipNo='I2'+lcShpCode ;
          FOR EMPTY(cRSession) AND gfTraceKey('BOMLINE',cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode,'D')
      SCAN REST WHILE cIMTyp+cType+ShipNo='I2'+lcShpCode ;
          FOR EMPTY(cRSession) AND gfTraceKey('BOMLINE',cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode,'D')
        SCATTER MEMVAR
        DELETE
        SELECT (TmpBomLine)
        APPEND BLANK
        GATHER MEMVAR
      ENDSCAN
      *C102469,1 AMH [End]
      
    ENDIF
  ELSE
    SET ORDER TO TAG BomLine
    SELECT (lcTmpLine)
    SCAN
      *khm1
      *lcLKey = IIF(llMFCall,'M2'+&lcTmpLine..Cuttkt,'I2'+&lcTmpLine..Po)
      lcLKey = IIF(llMFCall,'M2'+&lcTmpLine..Cuttkt,IIF(lcPType='D','D','I')+'2'+&lcTmpLine..Po)
      *khm1
      IF SEEK(lcLKey,'BOMLINE')
        SELECT BomLine
        
        *C102469,1 AMH Save the last landed cost of PO or C/T for not recieved records [Start]
        *DELETE REST WHILE cIMTyp+cType+cTktNo=lcLKey ;
          FOR EMPTY(cRSession) AND gfTraceKey('BOMLINE',cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode,'D')
        SCAN REST WHILE cIMTyp+cType+cTktNo=lcLKey ;
            FOR EMPTY(cRSession) AND gfTraceKey('BOMLINE',cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode,'D')
          SCATTER MEMVAR
          DELETE
          SELECT (TmpBomLine)
          APPEND BLANK
          GATHER MEMVAR
        ENDSCAN
        *C102469,1 AMH [End]
        
      ENDIF
    ENDSCAN
  ENDIF
  SELECT (lcTmpLine)
  GO TOP
  =RLOCK()
  REPLACE nSteps WITH lnNxtStp + 11
  UNLOCK  
ENDIF

*C102469,1 AMH Save the last landed cost for PO or C/T from the temp cursor to bomline [Start]
IF llStkLine AND ( llMFCall OR !(lcPType $ 'ROEL'))
  SELECT (TmpBomLine)
  SCAN
    SCATTER MEMVAR
    SELECT BOMLINE
    APPEND BLANK
    GATHER MEMVAR
  ENDSCAN
  USE IN (TmpBomLine)
ENDIF
*C102469,1 AMH [End]

*--Update allocation quantity if changed the link.------------------
IF llSOInstld AND !(lcPType$'R') AND USED(lcTmpCtPk)
  SELECT (lcTmpLine)
  SCAN FOR TranCd='1' AND lAloChg
    SCATTER FIELDS Ord1,Ord2,Ord3,Ord4,Ord5,Ord6,Ord7,Ord8,TotOrd TO laNewOrd
    IF llMFCall 
      =SEEK(Cuttkt+Style+Dyelot+TranCd,'CUTTKTL') 
    ELSE
      =SEEK(cStyType+Po+Style+STR(LineNo,6)+TranCd,'POSLN') 
    ENDIF 
    SELECT (lcMasterLn)
    GATHER FROM laNewOrd FIELDS Ord1,Ord2,Ord3,Ord4,Ord5,Ord6,Ord7,Ord8,TotOrd
    SELECT (lcTmpLine)
    REPLACE lAloChg WITH .F.
  ENDSCAN
  *E301077,11 Open orders files.
  
  *IF !laOpFile[7]
    = gfOpenFile(gcDataDir+"OrdHdr","OrdHdr","SH")  
  *ENDIF
  
  *IF !laOpFile[8]
    = gfOpenFile(gcDataDir+"OrdLine","OrdLine","SH")  
  *ENDIF

  SELECT (lcTmpCtPk)
  SCAN
    lcCpkky=Trancd+cTktNo+Style
    SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty TO laNewOrd
    SCATTER FIELDS nCurPck1,nCurPck2,nCurPck3,nCurPck4,nCurPck5,nCurPck6,nCurPck7,nCurPck8,nCurPck9 TO laOldOrd
    SELECT CUTPICK
    SEEK lcCpkky
    LOCATE REST WHILE Trancd+cTktNo+Style=lcCpkky FOR cOrdLine=&lcTmpCtPk..cOrdLine
    IF FOUND()
      GATHER FROM laNewOrd FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty
      =SEEK('O'+CUTPICK.Order,'ORDHDR')
      =SEEK('O'+CUTPICK.Order+STR(INT(VAL(CUTPICK.cOrdLine)),6),'ORDLINE')    
      FOR lnSizeNo = 1 TO 8
        lcSz = STR(lnSizeNo,1)
        REPLACE ORDHDR.TotCut    WITH ORDHDR.TotCut    - laOldOrd[lnSizeNo] + laNewOrd[lnSizeNo]
        REPLACE ORDLINE.TotCut   WITH ORDLINE.TotCut   - laOldOrd[lnSizeNo] + laNewOrd[lnSizeNo],;
                ORDLINE.Cut&lcSz WITH ORDLINE.Cut&lcSz - laOldOrd[lnSizeNo] + laNewOrd[lnSizeNo]
      ENDFOR
      *--Call TraceKey global function.
      =gfTraceKey('ORDHDR','O'+CUTPICK.Order,'M')
      =gfTraceKey('ORDLINE','O'+CUTPICK.Order+STR(INT(VAL(CUTPICK.cOrdLine)),6),'M')
      SELECT CUTPICK
      IF CUTPICK.TotQty = 0
        *--Call TraceKey global function DELETE.

        *E301182,7 Trace cutpick file using master key [Begin]
        *=gfTraceKey('CUTPICK',Trancd+cTktNo+Order+Style+cOrdline,'D')
        =lfTraceTag('CUTPICK','D')
        *E301182,7 Trace cutpick file using master key [End  ]

        SELECT CUTPICK
        =RLOCK()
        DELETE
        UNLOCK 
      ELSE
        *--Call TraceKey global function MODIFY.

        *E301182,7 Trace cutpick file using master key [Begin]
        *=gfTraceKey('CUTPICK',Trancd+cTktNo+Order+Style+cOrdline,'M')
        =lfTraceTag('CUTPICK','M')
        *E301182,7 Trace cutpick file using master key [End  ]

      ENDIF  
    ENDIF
  ENDSCAN
ENDIF


*--Update WhsLoc----------------------------------------------------
IF llWareLoc 
  SELECT WHSLOC
  SET ORDER TO TAG WhsLocSt
  SELECT (lcTemLoc)
  SCAN FOR !EMPTY(clocation)
    IF !SEEK(Style+SPACE(6)+cWareCode+cLocation,'WHSLOC')
      SCATTER MEMVAR
      INSERT INTO WhsLoc FROM MEMVAR
      REPLACE cOwner WITH ' '
      =gfAdd_Info('WhsLoc')
      *--Call TraceKey global function.
      =gfTraceKey('WHSLOC',WHSLOC.cwarecode+WHSLOC.clocation+WHSLOC.style,'A')
    ENDIF
  ENDSCAN
ENDIF


*--Update ShpmtHdr---------------------------------------------------

*B124614,1 NNA 10/24/2004 (Begin) if reciving by shipment or batch we have to change the ship. status
*IF lcPType='S'
IF lcPType $ 'BS'       && 'B' for Batch,'S' for shipment
  IF !USED('ShpmtHdr')
    = gfOpenFile(gcDataDir+'ShpmtHdr','ShpmtHdr','SH')
  ENDIF
*B124614,1 NNA (End)

  *--Delete the shipment lines for coplete receive lines.
  SELECT POSLN
  SET ORDER TO TAG PoSlnSh
  =SEEK(lcShpCode)
  DELETE REST WHILE ShipNo = lcShpCode FOR TranCd = '3' AND TotQty = 0 AND ;
    gfTraceKey('POSLN',cStyType+Po+cRsession+Shipno+Style+STR(Lineno,6)+Trancd,'D')

  IF SEEK(lcShpCode)
    LOCATE REST WHILE ShipNo = lcShpCode FOR TranCd = '3'
    lcShpStat = IIF(FOUND(),'O','C')
  ELSE
    lcShpStat = 'C'
  ENDIF     
  SET ORDER TO TAG Posln

  SELECT SHPMTHDR
  =SEEK(lcShpCode)
  =RLOCK()

  *B124614,1 NNA 10/24/2004 (Begin) If we're receiving by Batch then ldEta,ldEnterd Become Empty 
  *B124614,1 NNA             and it Replace SHPMTHDR.ETA,SHPMTHDR.ldEnterd with an empty Date so 
  *B124614,1 NNA            I Fill it with the Field Date itself
  ldEta    = IIF(lcPType='S',ldEta,SHPMTHDR.ETA)
  ldEnterd = IIF(lcPType='S',ldEnterd,SHPMTHDR.Entered)
  *B124614,1 NNA (End)

  *E301077,11 Update Odded fields using gfAdd_Info Function [Begin]
  *Odded REPLACE Status     WITH lcShpStat ,;
  *Odded         Recv_Stk   WITH Recv_Stk + lnTotStk  ,;
  *Odded         recv_Dam   WITH recv_Dam + lnTotDam  ,;
  *Odded         Recv_Can   WITH Recv_Can + lnTotCan  ,;
  *Odded         Reference  WITH lcShpRef  ,;
  *Odded         Entered    WITH ldEnterd  ,;
  *Odded         Cartons    WITH lnCartons ,;
  *Odded         AirWayB    WITH lcAirWayB ,;
  *Odded         Eta        WITH ldEta     ,;
  *Odded         Mod_action WITH 'RECEIVED',;
  *Odded         Mod_Date   WITH gdSysDate ,;
  *Odded         Mod_Time   WITH TIME()    ,;
  *Odded         Mod_UserId WITH gcUser_id

  REPLACE Status     WITH lcShpStat ,;
          Recv_Stk   WITH Recv_Stk + lnTotStk  ,;
          recv_Dam   WITH recv_Dam + lnTotDam  ,;
          Recv_Can   WITH Recv_Can + lnTotCan  ,;
          Reference  WITH lcShpRef  ,;
          Entered    WITH ldEnterd  ,;
          Cartons    WITH lnCartons ,;
          AirWayB    WITH lcAirWayB ,;
          Eta        WITH ldEta

  =gfAdd_Info('SHPMTHDR')
  *E301077,11 Update Odded fields using gfAdd_Info Function [End  ]

  UNLOCK
  *--Call TraceKey global function.
  =gfTraceKey('SHPMTHDR',lcShpCode,'M')

ENDIF


*--Update CtktRcvH---------------------------------------------------

*E301480,1 NAD (Start) Add the Inter location Po batch to the condition.
*IF lcPType $ 'BT'
IF lcPType $ 'BTL'
*E301480,1 NAD (End)

  SELECT CTKTRCVH
  =RLOCK()

  *E301077,11 Update Odded fields using gfAdd_Info Function [Begin]
  *Odded REPLACE cStatus    WITH 'P'     ;
  *Odded         Mod_Date   WITH gdSysDate,;
  *Odded         Mod_Time   WITH TIME(),;
  *Odded         Mod_UserId WITH gcUser_id

  REPLACE cStatus    WITH 'P'
  =gfAdd_Info('CTKTRCVH')
  *E301077,11 Update Odded fields using gfAdd_Info Function [End]

  UNLOCK
  *--Call TraceKey global function.
  =gfTraceKey('CTKTRCVH',CTKTRCVH.cType+CTKTRCVH.TmpRcvNum,'M')
ENDIF 

*--Complete unCmSess record ,Session completed.
*=lfCrUnSess('','C',.F.) 
=IIF(llFromEdi,.T.,lfCrUnSess('','C',.F.))

*E300935,4 Arrange dyelot in Dyelot relationship file if any [begin]

*B604136,1 KHM 22/01/2001 (Begin) Checking if the material module is
*B604136,1                installed and system is setup to use dyelot 
*B604136,1                and the material uses dyelot.
*IF llDyelot
IF 'MA' $ gcComp_mdl AND llDyelot
  = gfArDyRl('' , '' , lcTmDyeRel,.T.)
ENDIF
*B604136,1 KHM 22/01/2001 (End)
  
*E300935,4 Arrange dyelot in Dyelot relationship file if any [end]
*B602698,1 [BEGIN] Zap the tmp file after posting .
IF USED(lcTempFile)
  SELECT (lcTempFile)
  ZAP
  STORE ' ' TO lcTCodeOld
ENDIF  
*B602698,1 [END..] Zap the tmp file after posting .

*C037816,1 MHM 04/06/2004 Save Data to Bin location files[Start]
IF ASCAN(laEvntTrig,PADR("SAVBINS",10)) <> 0
  =gfDoTriger("POSTREC",PADR("SAVBINS",10))
ENDIF
*C037816,1 MHM [End]


SELECT (lcTmpLine)
SET ORDER TO TAG TmpLine1
DELETE ALL

*E301484,1 Send Product Activity to Back Office
IF INLIST(lcPType,'O','L') AND 'NC' $ gcCmpModules
  =gfOpenFile(gcDataDir+'CODES',gcDataDir+'CODES','SH')
  SELECT CODES
  SET ORDER TO TAG Idrltfname
  =SEEK('NYCSITEID')
  LOCATE REST WHILE cdefcode+crltfield+cfld_name = 'NYCSITEID' ;
              FOR   cRltd_Nam = 'CCMSITETYP' AND cRltd_Vlu= 'B'
  IF FOUND()
    lcSiteId = Codes.cCode_No
    SELECT EDiAcPrt
    LOCATE FOR cSiteId = lcSiteId
    IF FOUND() AND SEEK(EdiAcPrt.cPartCode+'852','EdiPd')
      lcDateKey = DTOS(ldRcvDate)+'-'+DTOS(ldRcvDate)
      SELECT EdiTrans
      *E037853,1 HBG 16/02/2004 Change the width of Key field in EDITRANS to 40 char [Begin]
      *IF !SEEK('852'+PADR(lcDateKey,20)+EdiAcPrt.Type+EdiAcPrt.cPartner)
      IF !SEEK('852'+PADR(lcDateKey,40)+EdiAcPrt.Type+EdiAcPrt.cPartner)
      *E037853,1 [End]
          INSERT INTO ('EDITRANS') (CEDITRNTYP,KEY,TYPE,CPARTNER,LINTERCOMP) VALUES ;
                                   ('852',lcDateKey,EdiAcPrt.Type,EdiAcPrt.cPartner,EdiAcPrt.lInterComp)
      ENDIF
      REPLACE cStatus WITH 'N'
      =gfAdd_Info('EDITRANS')
    ENDIF
  ENDIF
  SET ORDER TO TAG CODES IN CODES
ENDIF
*E301484,1 (End)

*E301206,1 Reham On 04/22/99   *** Begin ***
*E301206,1 If called from any of the bar code programs.
IF !llBarCode
*E301206,1 Reham On 04/22/99   *** End   ***
  =lfClearInfo()
*E301206,1 Reham On 04/22/99   *** Begin ***
ENDIF
*E301206,1 Reham On 04/22/99   *** End   ***

RETURN
*-- end of lpsavscr.

*:*************************************************************
*! Name     : lfUpdIntCmp
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose  : Update Issue Inter Location Purchase Order.
*:*************************************************************
FUNCTION lfUpdIntCmp

*E301484,1 WAM Send Inter Location PO or Purchase orders to sites
IF !llFromEDI AND 'NC' $ gcCmpModules 
  =gfOpenFile(gcDataDir+'EDIACPRT',gcDataDir+'ACCFACT','SH')
  =gfOpenFile(gcDataDir+'EDIPD',gcDataDir+'PARTTRANS','SH')
  =gfOpenFile(gcDataDir+'EDITRANS',gcDataDir+'TYPEKEY','SH')

  *E301561,1 Send PO to back office as well
  lcSysType = gfGetMemVar('M_SYSTYPE')
  IF lcSysType = 'P'
    =gfOpenFile(gcDataDir+'CODES',gcDataDir+'Idrltfname','SH')
    SELECT CODES
    =SEEK('N'+'Y'+PADR('CSITEID',10))
    LOCATE REST WHILE cDefCode+cRltField+cFld_Name = 'N'+'Y'+PADR('CSITEID',10) ;
                FOR   cRltd_Nam = 'CCMSITETYP' AND cRltd_Vlu = 'B'
    SELECT EDiAcPrt
    LOCATE FOR cSiteId = Codes.cCode_No
    lcBackSite = cSiteId
    lcBackAcc  = cPartner
  ENDIF
  *E301561,1 (End)
ENDIF
*E301484,1 WAM (End)

SELECT (lcTmpLine)
SET ORDER TO TAG TmpLine3
*--PO+Style+Dyelot+cWareCode+STR(LineNo,6)+cCarton+TranCd

*B603237,1 Custom process to Issue Adorn Order.
IF lcPType='A' AND llSpecHndl
  =gfDoTriger('MFRCVAP',PADR('UPDISSUAD',10))
ENDIF
*B603237,1 End.

*C102358,5 AMH Add trigger to custom issue adornment order for JL [Start]
IF !llFromEdi AND lcPType='A' AND ASCAN(laEvntTrig , PADR('UPDISSAD',10)) <> 0
  =gfDoTriger('PORCVAP',PADR('UPDISSAD',10))
ENDIF
*C102358,5 AMH [End]

SELECT (lcTmpLine)
GO TOP
SCAN FOR TranCd <> '1' AND TotQty <> 0
  WAIT WINDOW 'Posting all transactions ... Style :'+Style NOWAIT
  *E301291,4 AMM Comment out, consider issue adornment order when calculate
  *lnNewCost = nECost1+nECost2+nECost3+nECost4+nECost5
  *E301291,4 AMM end
  =SEEK(Style,'STYLE') 
  *E301291,4 AMM consider issue adornment order when calculate
  IF lcPType = 'A'
    =SEEK(POSLN.Style+POSLN.cWareCode+SPACE(10),'STYDYE') 
    lnNewCost  = IIF( lcCostMth='A',STYDYE.Ave_Cost , Style.TotCost )
  ELSE
    lnNewCost = nECost1+nECost2+nECost3+nECost4+nECost5
  ENDIF
  *E301291,4 AMM end

  *--Update Style Inventory (Start step 2 & Next Step lnNxtStp)---------------
  *--G/L Array difinition and initialization.
  *-- Update general ledger entreis in gfStyCrl()

  IF llLinkToGl

    *--Read the G/L link code that will be used to create GL entres.
    DECLARE laGLDistAr[2,13]
    laGLDistAr[1,1] = IIF(SEEK(Style+PADR(Vendor,6)+SPACE(10),'STYDYE') AND ;
                          !EMPTY(STYDYE.Gl_link),STYDYE.Gl_link,'DEFDEF')
    *E301291,4 AMM Adjust to fit the new added type for adornment order
    *laGLDistAr[2,1] = IIF(SEEK('N'+PO,'POSHDR') ,POSHDR.Link_Code,'DEFDEF')
    laGLDistAr[2,1] = IIF(SEEK( IIF(lcPType='A','A','N')+PO,'POSHDR') ,POSHDR.Link_Code,'DEFDEF')
    *E301291,4 AMM end

    laGLDistAr[1,2] = '006'
    laGLDistAr[2,2] = '013'
    laGLDistAr[1,3] =  1
    laGLDistAr[2,3] = -1
    STORE 'PO' TO laGLDistAr[1,4],laGLDistAr[2,4]
    STORE &lcTmpLine..PO TO laGLDistAr[1,5],laGLDistAr[2,5]
    STORE ldTrDate   TO laGLDistAr[1,6],laGLDistAr[2,6]
    STORE lcGLFYear  TO laGLDistAr[1,7],laGLDistAr[2,7]
    STORE lcGlPeriod TO laGLDistAr[1,8],laGLDistAr[2,8]
    STORE lcGlDist   TO laGLDistAr[1,9],laGLDistAr[2,9]
  ELSE
    DIME laGLDistAr[1,1]
    laGLDistAr = ''
  ENDIF

  DECLARE laAdjust[9]
  FOR I = 1 TO 8
    Z=STR(I,1)
    laAdjust[I] = -(Qty&Z)
  ENDFOR
  laAdjust[9] = -(TotQty)

  *--Call the global function for update style inventory control.
  *E300935,4 Add dyelot field to update its stock.
  *lnNxtStp = gfStyCrl('6',Style,PADR(Vendor,6),SPACE(10),ldRcvDate,;
  *               PO,@laAdjust,0,Reference,.T.,'',;
  *               1,lcTmpLine,'nSteps',@laGLDistAr)

  *E301192 (Start)
  *lnNxtStp = gfStyCrl('6',Style,PADR(Vendor,6),PADR(Dyelot,10),ldRcvDate,;
                 PO,@laAdjust,lnNewCost,Reference,lcGlSession,'',;
                 1,lcTmpLine,'nSteps',@laGLDistAr)
  PRIVATE lcRefer
  lcRefer = "VEN. " + APVendor.cVendCode + APVendor.cVenComp
  lnNxtStp = gfStyCrl('6',Style,PADR(Vendor,6),PADR(Dyelot,10),ldRcvDate,;
                 PO,@laAdjust,lnNewCost,lcRefer,lcGlSession,'',;
                 1,lcTmpLine,'nSteps',@laGLDistAr)
  *E301192 (End)
  *--Create Intransit P/O line record
  SELECT (lcTmpLine)   
  SCATTER MEMVAR
  *E301291,4 AMM  start

  *C200170,1 AMH Add case of issue inter-location P/O Batch [Start]
  *IF lcPType $ 'NA'
  IF lcPType $ 'NAH'
  *C200170,1 AMH [End]
  
    IF lcPType = 'A'
      STORE 0 TO m.nCost2,m.nLan_Cst2,m.nECost2,m.nELanCost2
    ENDIF
    =SEEK(Style+cWareCode+SPACE(10),'STYDYE') 
    m.nCost1  = IIF( lcCostMth='A',STYDYE.Ave_Cost , Style.TotCost )
    m.nECost1 = m.nCost1
  ENDIF
  *E301291,4 AMM end
  IF &lcTmpLine..nSteps < lnNxtStp
    SELECT POSLN
    APPEND BLANK
    GATHER MEMVAR
    REPLACE Date      WITH ldRcvDate,;
            TranCd    WITH '6',;
            cOwner    WITH ' ',;
            cRSession WITH lcGlSession

    =gfAdd_Info('POSLN')
    SELECT (lcTmpLine)
    =RLOCK()
    REPLACE nSteps WITH lnNxtStp
    UNLOCK
    *--Call TraceKey global function.
    *=gfTraceKey('POSLN',POSLN.cstytype+POSLN.po+POSLN.style+STR(POSLN.lineno,6)+POSLN.trancd,'A')
    SELECT POSLN
    =gfTraceKey('POSLN',cStyType+Po+cRsession+Shipno+Style+STR(Lineno,6)+Trancd,'A')
  ENDIF
  
  
  *--Update in-transit.

  *C200170,4 AMH Add case of issue inter-location P/O Batch [Start]
  *IF &lcTmpLine..nSteps < lnNxtStp + 1 AND lcPType = 'N'
  *C102358,4 AMH Add issue adornment order for customer JL [Start]
  *IF &lcTmpLine..nSteps < lnNxtStp + 1 AND lcPType $ 'NH'
  IF &lcTmpLine..nSteps < lnNxtStp + 1 AND (lcPType $ 'NH' OR;
     (!llFromEdi AND lcPType = 'A' AND ASCAN(laEvntTrig,PADR("RCVADORD",10)) <> 0))
  *C102358,4 AMH [End]
  *C200170,4 AMH [End]

    =SEEK(POSLN.Style,'STYLE') 
    SELECT STYLE
    =RLOCK()
    REPLACE InTrans1 WITH InTrans1 + &lcTmpLine..Qty1,;
            InTrans2 WITH InTrans2 + &lcTmpLine..Qty2,;
            InTrans3 WITH InTrans3 + &lcTmpLine..Qty3,;
            InTrans4 WITH InTrans4 + &lcTmpLine..Qty4,;
            InTrans5 WITH InTrans5 + &lcTmpLine..Qty5,;
            InTrans6 WITH InTrans6 + &lcTmpLine..Qty6,;
            InTrans7 WITH InTrans7 + &lcTmpLine..Qty7,;
            InTrans8 WITH InTrans8 + &lcTmpLine..Qty8,;
            TotInTrn WITH InTrans1+InTrans2+InTrans3+InTrans4+InTrans5+InTrans6+InTrans7+InTrans8
    UNLOCK
    SELECT (lcTmpLine)
    =RLOCK()
    REPLACE nSteps WITH lnNxtStp + 1
    UNLOCK
    *--Call TraceKey global function.
    =gfTraceKey('STYLE',STYLE.Style,'M')
  ENDIF

  *C200170,4 AMH Add case of issue inter-location P/O Batch [Start]
  *IF &lcTmpLine..nSteps < lnNxtStp + 2  AND lcPType = 'N'
  *C102358,4 AMH Add issue adornment order for customer JL [Start]
  *IF &lcTmpLine..nSteps < lnNxtStp + 2  AND lcPType $ 'NH'
  IF &lcTmpLine..nSteps < lnNxtStp + 2  AND (lcPType $ 'NH' OR;
     (!llFromEdi AND lcPType = 'A' AND ASCAN(laEvntTrig,PADR("RCVADORD",10)) <> 0))
  *C102358,4 AMH [End]
  *C200170,4 AMH [End]

    =SEEK(POSLN.Style+POSLN.cWareCode+SPACE(10),'STYDYE') 
    SELECT STYDYE
    =RLOCK()
    REPLACE InTrans1 WITH InTrans1 + &lcTmpLine..Qty1,;
            InTrans2 WITH InTrans2 + &lcTmpLine..Qty2,;
            InTrans3 WITH InTrans3 + &lcTmpLine..Qty3,;
            InTrans4 WITH InTrans4 + &lcTmpLine..Qty4,;
            InTrans5 WITH InTrans5 + &lcTmpLine..Qty5,;
            InTrans6 WITH InTrans6 + &lcTmpLine..Qty6,;
            InTrans7 WITH InTrans7 + &lcTmpLine..Qty7,;
            InTrans8 WITH InTrans8 + &lcTmpLine..Qty8,;
            TotInTrn WITH InTrans1+InTrans2+InTrans3+InTrans4+InTrans5+InTrans6+InTrans7+InTrans8
    UNLOCK
    SELECT (lcTmpLine)
    =RLOCK()
    REPLACE nSteps WITH lnNxtStp + 2
    UNLOCK
    *--Call TraceKey global function.
   =gfTraceKey('STYDYE',STYDYE.style+STYDYE.cwarecode+STYDYE.dyelot,'M')
  ENDIF

  *-Update Style.
  *C200170,4 AMH Add case of issue inter-location P/O Batch [Start]
  *IF SEEK(POSLN.STYLE,'STYLE') AND lcPType = 'N'
  IF SEEK(POSLN.STYLE,'STYLE') AND lcPType $ 'NH'
  *C200170,4 AMH [End]

    SELECT STYLE
    IF &lcTmpLine..nSteps < lnNxtStp + 3
      = RLOCK()
      REPLACE WIP1 WITH WIP1 + &lcTmpLine..Qty1,WIP2 WITH WIP2 + &lcTmpLine..Qty2,;
              WIP3 WITH WIP3 + &lcTmpLine..Qty3,WIP4 WITH WIP4 + &lcTmpLine..Qty4,;
              WIP5 WITH WIP5 + &lcTmpLine..Qty5,WIP6 WITH WIP6 + &lcTmpLine..Qty6,;
              WIP7 WITH WIP7 + &lcTmpLine..Qty7,WIP8 WITH WIP8 + &lcTmpLine..Qty8,;
              TotWip WITH Wip1+Wip2+Wip3+Wip4+Wip5+Wip6+Wip7+Wip8
      UNLOCK
      SELECT (lcTmpLine)
      REPLACE nSteps WITH lnNxtStp + 3
      UNLOCK
    ENDIF
    IF &lcTmpLine..nSteps < lnNxtStp + 4
      SELECT STYLE
      =RLOCK()
      REPLACE nWO1 WITH nWO1 + &lcTmpLine..Qty1,nWO2 WITH nWO2 + &lcTmpLine..Qty2,;
              nWO3 WITH nWO3 + &lcTmpLine..Qty3,nWO4 WITH nWO4 + &lcTmpLine..Qty4,;
              nWO5 WITH nWO5 + &lcTmpLine..Qty5,nWO6 WITH nWO6 + &lcTmpLine..Qty6,;
              nWO7 WITH nWO7 + &lcTmpLine..Qty7,nWO8 WITH nWO8 + &lcTmpLine..Qty8,;
              nTotWO WITH nWO1+nWO2+nWO3+nWO4+nWO5+nWO6+nWO7+nWO8
      UNLOCK
      SELECT (lcTmpLine)
      =RLOCK()
      REPLACE nSteps WITH lnNxtStp + 4
      UNLOCK
    ENDIF
    *--Call TraceKey global function.
    =gfTraceKey('STYLE',STYLE.Style,'M')
  ENDIF

  *--Update StyDye original warehouse.
  *C200170,4 AMH Add case of issue inter-location P/O Batch [Start]
  *IF SEEK(POSLN.STYLE+PADR(POSLN.Vendor,6)+SPACE(10),'STYDYE') AND lcPType = 'N'
  IF SEEK(POSLN.STYLE+PADR(POSLN.Vendor,6)+SPACE(10),'STYDYE') AND lcPType $ 'NH'
  *C200170,4 AMH [End]

    SELECT STYDYE
    IF &lcTmpLine..nSteps < lnNxtStp + 5
      = RLOCK()
      REPLACE WIP1 WITH WIP1 + &lcTmpLine..Qty1,WIP2 WITH WIP2 + &lcTmpLine..Qty2,;
              WIP3 WITH WIP3 + &lcTmpLine..Qty3,WIP4 WITH WIP4 + &lcTmpLine..Qty4,;
              WIP5 WITH WIP5 + &lcTmpLine..Qty5,WIP6 WITH WIP6 + &lcTmpLine..Qty6,;
              WIP7 WITH WIP7 + &lcTmpLine..Qty7,WIP8 WITH WIP8 + &lcTmpLine..Qty8,;
              TotWip WITH Wip1+Wip2+Wip3+Wip4+Wip5+Wip6+Wip7+Wip8
      UNLOCK
      SELECT (lcTmpLine)
      =RLOCK()
      REPLACE nSteps WITH lnNxtStp + 5
      UNLOCK
    ENDIF
    IF &lcTmpLine..nSteps < lnNxtStp + 6
      SELECT STYDYE
      =RLOCK()
      REPLACE nWO1 WITH nWO1 + &lcTmpLine..Qty1,nWO2 WITH nWO2 + &lcTmpLine..Qty2,;
              nWO3 WITH nWO3 + &lcTmpLine..Qty3,nWO4 WITH nWO4 + &lcTmpLine..Qty4,;
              nWO5 WITH nWO5 + &lcTmpLine..Qty5,nWO6 WITH nWO6 + &lcTmpLine..Qty6,;
              nWO7 WITH nWO7 + &lcTmpLine..Qty7,nWO8 WITH nWO8 + &lcTmpLine..Qty8,;
              nTotWO WITH nWO1+nWO2+nWO3+nWO4+nWO5+nWO6+nWO7+nWO8
      UNLOCK
      SELECT (lcTmpLine)
      =RLOCK()
      REPLACE nSteps WITH lnNxtStp + 6
      UNLOCK
    ENDIF
    *--Call TraceKey global function.
    =gfTraceKey('STYDYE',STYDYE.style+STYDYE.cwarecode+STYDYE.dyelot,'M')
  ENDIF

  *E301484,1 WAM Send Inter Location PO or Purchase orders to sites
  IF !llFromEDI AND 'NC' $ gcCmpModules AND lcPType = 'N'
    lcWareCode = &lcTmpLine..cwarecode
    =SEEK(lcWareCode,'WAREHOUS')
    lcSiteId = WAREHOUS.cSiteId
    SELECT EDiAcPrt
    LOCATE FOR cSiteId = lcSiteId
    IF FOUND() AND EdiAcPrt.lInterComp AND SEEK(EdiAcPrt.cPartCode+'856','EdiPd')
      SELECT EdiTrans
      *E037853,1 HBG 16/02/2004 Change the width of Key field in EDITRANS to 40 char [Begin]
      *IF !SEEK('856'+PADR(&lcTmpLine..Po,20)+EdiAcPrt.Type+EdiAcPrt.cPartner)
      IF !SEEK('856'+PADR(&lcTmpLine..Po,40)+EdiAcPrt.Type+EdiAcPrt.cPartner)
      *E037853,1 [End]
        INSERT INTO ('EDITRANS') (CEDITRNTYP,KEY,TYPE,CPARTNER,lInterComp) VALUES ;
                                 ('856',&lcTmpLine..Po,EdiAcPrt.Type,EdiAcPrt.cPartner,EdiAcPrt.lInterComp)
      ENDIF
      REPLACE cStatus WITH 'N'
      =gfAdd_Info('EDITRANS')
      *E301561,1 Send PO to back office as well
      IF lcSysType = 'P' AND lcSiteId <> lcBackSite AND !EMPTY(lcBackAcc)
        *E037853,1 HBG 16/02/2004 Change the width of Key field in EDITRANS to 40 char [Begin]
        *IF !SEEK('856'+PADR(&lcTmpLine..Po,20)+'A'+lcBackAcc)
        IF !SEEK('856'+PADR(&lcTmpLine..Po,40)+'A'+lcBackAcc)
        *E037853,1 [End]
          INSERT INTO ('EDITRANS') (CEDITRNTYP,KEY,TYPE,CPARTNER,lInterComp) VALUES ;
                                   ('856',&lcTmpLine..Po,'A',lcBackAcc,.T.)
        ENDIF
        REPLACE cStatus WITH 'N'
        =gfAdd_Info('EDITRANS')
      ENDIF
      *E301561,1 (End)
    ENDIF
  ENDIF
  *E301484,1 WAM (End)
  
ENDSCAN

*--Update GLDist -------------------------------------------------
IF llLinkToGl

  WAIT WINDOW 'Updating General Ledger Distribution File ' NOWAIT
  SELECT (lcGlDist)
  REPLACE ALL GlSession with lcGlSession
  USE
  SELECT GlDist
  APPEND FROM (gcWorkDir+lcGlDist)
  ERASE (gcWorkDir+lcGLDist+'.DBF')
  ERASE (gcWorkDir+lcGLDist+'.CDX')
ENDIF
WAIT CLEAR
RETURN

*:*************************************************************
*! Name     : lfSendEdi
*! Developer: Wael Ali Mohamed
*! Date     : 14/11/2000              
*! Purpose  : Send 850 EDI Transaction          *E301484,1
*:*************************************************************
FUNCTION lfSendEdi
PARAMETERS lcPoType,lcEdiTrnTyp
PRIVATE lcSaveOrd,lcAlias

*B803958,1 Add 850 transaction for INter location PO only
*IF !llFromEDI AND ( (lcPoType='N' AND 'NC' $ gcCmpModules) OR (lcPoType='P' AND 'EB' $ gcCmpModules))
IF !llFromEDI AND lcPoType='N' AND 'NC' $ gcCmpModules
*B803958,1 (End)
  lcAlias = SELECT()    
  =gfOpenFile(gcDataDir+'EDIACPRT',gcDataDir+'ACCFACT','SH')
  =gfOpenFile(gcDataDir+'EDIPD',gcDataDir+'PARTTRANS','SH')
  =gfOpenFile(gcDataDir+'EDITRANS',gcDataDir+'TYPEKEY','SH')
  
  *E301561,1 Send PO to back office as well
  lcSysType = gfGetMemVar('M_SYSTYPE')
  IF lcSysType = 'P'
    =gfOpenFile(gcDataDir+'CODES',gcDataDir+'Idrltfname','SH')
    SELECT CODES
    =SEEK('N'+'Y'+PADR('CSITEID',10))
    LOCATE REST WHILE cDefCode+cRltField+cFld_Name = 'N'+'Y'+PADR('CSITEID',10) ;
                FOR   cRltd_Nam = 'CCMSITETYP' AND cRltd_Vlu = 'B'
    SELECT EDiAcPrt
    LOCATE FOR cSiteId = Codes.cCode_No
    lcBackSite = cSiteId
    lcBackAcc  = cPartner
  ENDIF
  *E301561,1 (End)
  SELECT POSLN
  lcSaveOrd = ORDER()
  SET ORDER TO TAG Poslnw
  =SEEK(lcPoType+laData[1])
  DO WHILE cstytype+po+cwarecode+account+store+style+trancd = lcPoType+laData[1]
    lcWareCode = cwarecode
    =SEEK(lcWareCode,'WAREHOUS')
    lcSiteId = WAREHOUS.cSiteId
    SELECT EDiAcPrt
    LOCATE FOR cSiteId = lcSiteId
    IF FOUND() AND ((lcPoType='N' AND EdiAcPrt.lInterComp) OR ;
                    (lcPoType='P' AND !EdiAcPrt.lInterComp)) AND ;
       SEEK(EdiAcPrt.cPartCode+lcEdiTrnTyp,'EdiPd')
      SELECT EdiTrans
      *E037853,1 HBG 16/02/2004 Change the width of Key field in EDITRANS to 40 char [Begin]
      *IF lcEdiTrnTyp = '860' AND ;
      *   SEEK('850'+PADR(laData[1],20)+EdiAcPrt.Type+EdiAcPrt.cPartner) AND cStatus = 'N'
      IF lcEdiTrnTyp = '860' AND ;
         SEEK('850'+PADR(laData[1],40)+EdiAcPrt.Type+EdiAcPrt.cPartner) AND cStatus = 'N'
      *E037853,1 [End]
        DELETE
        *E301561,1 Send PO to back office as well
        *E037853,1 HBG 16/02/2004 Change the width of Key field in EDITRANS to 40 char [Begin]  
        *IF lcSysType = 'P' AND lcSiteId <> lcBackSite AND !EMPTY(lcBackAcc) AND ;
        *  SEEK('850'+PADR(laData[1],20)+'A'+lcBackAcc) AND cStatus = 'N'
        IF lcSysType = 'P' AND lcSiteId <> lcBackSite AND !EMPTY(lcBackAcc) AND ;
          SEEK('850'+PADR(laData[1],40)+'A'+lcBackAcc) AND cStatus = 'N'
        *E037853,1 [End]
          DELETE
        ENDIF
        *E301561,1 (End)
      ELSE   
        *E037853,1 HBG 16/02/2004 Change the width of Key field in EDITRANS to 40 char [Begin]  
        *IF !SEEK(lcEdiTrnTyp+PADR(laData[1],20)+EdiAcPrt.Type+EdiAcPrt.cPartner)
        IF !SEEK(lcEdiTrnTyp+PADR(laData[1],40)+EdiAcPrt.Type+EdiAcPrt.cPartner)
        *E037853,1 [End]
          INSERT INTO ('EDITRANS') (CEDITRNTYP,KEY,TYPE,CPARTNER,lInterComp) VALUES ;
                                   (lcEdiTrnTyp,laData[1],EdiAcPrt.Type,EdiAcPrt.cPartner,EdiAcPrt.lInterComp)
        ENDIF
        REPLACE cStatus WITH 'N'
        =gfAdd_Info('EDITRANS')
        *E301561,1 Send PO to back office as well
        IF lcSysType = 'P' AND lcSiteId <> lcBackSite AND !EMPTY(lcBackAcc)
          *E037853,1 HBG 16/02/2004 Change the width of Key field in EDITRANS to 40 char [Begin]  
          *IF !SEEK(lcEdiTrnTyp+PADR(laData[1],20)+'A'+lcBackAcc)
          IF !SEEK(lcEdiTrnTyp+PADR(laData[1],40)+'A'+lcBackAcc)
          *E037853,1 [End]          
            INSERT INTO ('EDITRANS') (CEDITRNTYP,KEY,TYPE,CPARTNER,lInterComp) VALUES ;
                                     (lcEdiTrnTyp,laData[1],'A',lcBackAcc,.T.)
          ENDIF
          REPLACE cStatus WITH 'N'
          =gfAdd_Info('EDITRANS')
        ENDIF
        *E301561,1 (End)
      ENDIF  
    ENDIF
    SELECT POSLN
    SCAN REST WHILE cstytype+po+cwarecode+account+store+style+trancd = lcPoType+laData[1]+lcWareCode
    ENDSCAN
  ENDDO
  SET ORDER TO TAG &lcSaveOrd
  SELECT (lcAlias)
ENDIF
*E302213,1 Add 850 transaction for vendor
IF !llFromEDI AND lcPoType='P' AND 'EB' $ gcCmpModules
  lcAlias = SELECT()    
  =gfOpenFile(gcDataDir+'EDIACPRT',gcDataDir+'ACCFACT','SH')
  =gfOpenFile(gcDataDir+'EDIPD',gcDataDir+'PARTTRANS','SH')
  =gfOpenFile(gcDataDir+'EDITRANS',gcDataDir+'TYPEKEY','SH')

  IF SEEK('V'+laData[2],'EDIACPRT') AND SEEK(EDIACPRT.cPartCode+'850','EDIPD')
    SELECT EDITRANS
    *E037853,1 HBG 16/02/2004 Change the width of Key field in EDITRANS to 40 char [Begin]  
    *IF !SEEK('850'+PADR(laData[1],20)+'V'+laData[2])
    IF !SEEK('850'+PADR(laData[1],40)+'V'+laData[2])
    *E037853,1 [End]
      INSERT INTO 'EDITRANS' (cEdiTrnTyp,Key,Type,cPartner) VALUES ;
      ('850',laData[1],'V',laData[2])
    ENDIF
    REPLACE cStatus WITH 'N'
    =gfAdd_Info('EDITRANS')
  ENDIF 
ENDIF
*E302213,1 (End)

*!*************************************************************
*! Name      : gfDelPo
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Function if there is lines for P/O.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : .T. If No line was entered or exist
*!                      .F. Not  Eof() there is a line(s) found.
*!*************************************************************
*! Example   : =lfCkEOF()
*!*************************************************************
FUNCTION gfDelPo
*E301484,1 WAM Add new parameter setup with .T. if called from EDI
PARAMETERS llFromEdi


=gfObj_lock(.F.)
IF laData[3] = 'C' AND lcPromp = lcCancel
  *-Purchase order has been received. Cannot cancel it.
  =gfModalGen('TRM34014B34000','DIALOG',lcPOType+'|'+'received')
  RETURN
ENDIF

*B605105,1 AMH Fix the bug of cancel P/O with open shipment [Start]
IF lcAType ='P' AND lcPromp = lcCancel .AND. SEEK(lcAType+laData[1],'POSLN')
  PRIVATE lnAlias
  lnAlias = SELECT(0)
  SELECT POSLN
  LOCATE REST WHILE cStyType+Po+Style+STR(LineNo,6)+TranCd = lcAType+laData[1];
              FOR TranCd = '3' AND TOTQTY > 0
  IF FOUND()
    *-This P/O included in shipment XXXXXX, You have to remove this po from it first.
    =gfModalGen('TRM34180B34000','DIALOG',SHIPNO)
    SEEK lcAType+laData[1]
    SELECT (lnAlias)
    RETURN
  ENDIF
  SELECT (lnAlias)
ENDIF
*B605105,1 AMH Fix the bug of cancel P/O with open shipment [End]

*E301077,8 MAB Open Cut Ticket BOM file here [Begin
IF !llOpnKtBom
  llOpnKtBom = lfOpn_Rest(gcDataDir+'CTKTBOM','CTKTBOM')
ENDIF  
*E301077,8 MAB Open Cut Ticket BOM file here [End..
*C200080,1 AMM add the dye order case to the normal PO
*IF lcAType='P' AND lcPromp = lcCancel AND SEEK("I"+laData[1], "CTktBom")
IF lcAType$'PD' AND lcPromp = lcCancel AND SEEK("I"+laData[1], "CTktBom")
*C200080,1 AMM end
  IF laData[37]=0 
    =gfModalGen('QRM34125B34000','DIALOG')
    *-There is a P/O cost sheet, You have to delete it first.
    RETURN
  ELSE
    IF gfModalGen('QRM34015B34001','DIALOG',lcPOType+'|'+lcPOType) = 2
      *-This P/O has a cost sheet. Do you wish to cancel this P/O?
      RETURN
    ENDIF
  ENDIF
ENDIF

IF laData[3] = 'S' AND lcPromp = lcCancel        
  *-Cannot cancel the closed P/O.
  =gfModalGen('TRM34016B34000','DIALOG',lcPOType)
  RETURN
ENDIF

*C200152,4 AMH custom trigger to check if the inter location PO
*C200152,4     found in open or approved batch [Start]
IF lcAtype = "N" .AND. !llFromEdi AND ASCAN(laEvntTrig , PADR('DELINTPO',10)) <> 0
  PRIVATE llNotCan
  llNotCan = .F.
  =gfDoTriger('POINTRC',PADR('DELINTPO',10))
  IF llNotCan
    RETURN
  ENDIF
ENDIF
*C200152,4 AMH [End]

IF lfCkEOF(lcPoLine)
  *-The lines for this '+lcPOType+' are missing ! cannot proceed.
  =gfModalGen('TRM34017B34000','DIALOG',lcPOType)
  RETURN
ENDIF

IF laData[3] = 'X'
  SELECT (lcPOLine)
  SCAN
    m.Style=Style
    IF lcAType='C'
      IF lfVldSCnr()
        *-The Style: xxx valid in another contract for the same vendor and period, Cannot proceed!
        =gfModalGen('TRM34018B34000','DIALOG',m.Style)
        RETURN
      ENDIF
    ELSE
      IF !SEEK(m.Style,'STYLE') 
        *-One or more style(s) are missing, Cannot proceed!
        =gfModalGen('TRM34063B34000','DIALOG')
        RETURN
      ENDIF
    ENDIF
  ENDSCAN
ENDIF

*-Pieces have been received, Cancel all open items on this purchase order.
*E301291,1 AMM Consider the case of adornment order
*IF lcAType<>'N' AND laData[37] > 0 AND laData[3] = 'O' AND lcPromp = lcCancel AND ;
    gfModalGen('TRM34019B34001','DIALOG',lcPOType) = 2
IF !(lcAType$'NA') AND laData[37] > 0 AND laData[3] = 'O' AND lcPromp = lcCancel AND ;
    gfModalGen('TRM34019B34001','DIALOG',lcPOType) = 2
*E301291,1 AMM end
  RETURN
ENDIF
*C102357,4 AMH trigger to custom cancel adornment order for JL [Start]
llIssued = .F.
*C102357,4 AMH [End]
*E301291,1 AMM Consider the case of adornment order
*IF lcAType='N' AND lcPromp = lcCancel
IF lcAType$'NA' AND lcPromp = lcCancel
*E301291,1 AMM end
  SELECT POSLN
  *E301291,1 AMM Consider the case of adornment order
  *=SEEK('N'+laData[1]) 
  *LOCATE REST WHILE cStyType+PO = 'N'+laData[1] FOR TranCd = '6'
  =SEEK(lcAType+laData[1]) 
  LOCATE REST WHILE cStyType+PO = lcAType+laData[1] FOR TranCd = '6'
  *E301291,1 AMM end

  IF FOUND()
    *C102357,1 AMH trigger to custom cancel adornment order for JL [Start]
    *SELECT POSHDR
    *-Purchase order has been issued. Cannot cancel it.
    *=gfModalGen('TRM34014B34000','DIALOG',lcPOType+'|'+'issued')
    *RETURN
    IF !llFromEdi AND lcAType='A' AND ASCAN(laEvntTrig , PADR('CANCELPO',10)) <> 0
      =gfDoTriger('POADPO',PADR('CANCELPO',10))
      llIssued = .T.
    ELSE
      SELECT POSHDR
      *-Purchase order has been issued. Cannot cancel it.
      =gfModalGen('TRM34014B34000','DIALOG',lcPOType+'|'+'issued')
      RETURN
    ENDIF
    *C102357,1 AMH [End]
  ENDIF
ENDIF


*-This purchase order has allocated quantity from order(s),
*-canceling the P/O will release this allocation.
*-Would you like to continue ? < Yes > < No >
llRelAllo=.F.
IF llSOInstld AND lcAType='P' AND laData[3] $ 'OH' AND SEEK('2'+laData[1],'CutPick')
    *B602474,1 if sales order have allocated or picked quantity, [begin
    *B602474,1 user must release this allocation before.

    *B604283,1 KHM 03/18/2001 (Begin) Commented the following line in order
    *B604283,1                to allow cancel a PO that generated from SO 
    *B604283,1                even if the SO has been allocated.
    *PRIVATE lnCurAlias,llCanRet
    *llCanRet = .F.
    *lnCurAlias = SELECT(0)
    *SELECT CutPick
    *SCAN FOR trancd+ctktno+style = '2'+laData[1]

    *  IF SEEK('O'+Order+STR(INT(VAL(cOrdLine)),6),'ORDLINE') AND ORDLINE.PICKED
      *-- Message :
      *-- Can't cancel this Purchase Order as it was allocated 
      *-- to sales order XXX which have allocated/picked quantity,
      *-- First you must release sales order allocation.
      *--                     < Ok > 
    *    =gfModalGen('TRM34140B34000','DIALOG','Purchase Order' + '|' + Order)
    *    llCanRet = .T.
    *    EXIT
    *  ENDIF
    *ENDSCAN
    *= SEEK('2'+laData[1])
    *SELECT (lnCurAlias)
    *IF llCanRet
    *  RETURN
    *ENDIF
    *B604283,1 KHM 03/18/2001 (End)
    *B602474,1 if sales order have allocated or picked quantity, [end..

  *E301969,1 AMH Don't display confirmation message in case of PO open and has received Qty [Start]
  *IF gfModalGen('QRM34083B34001') = 1
  IF (laData[3] = 'O' .AND. laData[37] > 0) .OR. gfModalGen('QRM34083B34001') = 1
  *E301969,1 AMH [End]
  
    llRelAllo=.T.
  ELSE
    RETURN
  ENDIF

ENDIF
*B603298,1 Start, Added, if adornment order then when cancel release the link with SO.
IF llSOInstld AND lcAType='A' AND laData[3] $ 'OH' AND SEEK('2'+laData[1],'CutPick')
  llRelAllo=.T.
ENDIF
*B603298,1 End.

SELECT (lcPOHdr)
GATHER FROM laData FIELDS &lcScFields MEMO    
IF !llContinue
  REPLACE nSteps WITH 0
  FLUSH
  SELECT (lcPOLine)
  REPLACE ALL nSteps WITH 0
  FLUSH  

  *--Create uncmsess record.
  *E301484,1 WAM Do not call uncompleted session function if called from EDI
  *=lfCrUnSess('pbDlt','O') 
  =IIF(llFromEdi , .T. , lfCrUnSess('pbDlt' , 'O'))
  *E301484,1 (End)
  
ENDIF

lcSign  =IIF(lcAType $ 'R','-','+')
lcOpSign=IIF(lcAType $ 'R','+','-')


*-- U N C A N C E L .
IF laData[3] = 'X'
  IF laData[40] = 0 AND laData[39] = 0
    laData[3]  = 'B'
    lcStatus   = 'Bid'
    laData[40] = laData[36]
  ELSE
    *B602769,1 Start.
    *laData[3]  = 'H'
    *lcStatus   = 'On Hold'
    *E301291,1 AMM Consider the case of adornment order
    *laData[3]  = IIF(lcAType='N','O','H')
    *lcStatus   = IIF(lcAType='N','Open','On Hold')
    laData[3]  = IIF(lcAType$'NA','O','H')
    lcStatus   = IIF(lcAType$'NA','Open','On Hold')
    *E301291,1 AMM end
    *B602769,1 End.
    laData[40] = laData[40] + laData[39] 
    laData[39] = 0
  ENDIF
  laStatus[1,1] = lcStatus
  laStatus[1,2] = laData[3]
  lcPromp       = lcCancel
  lcDelMesag    = 'Cancel'
  SHOW GET pbDlt,1 PROMPT lcPromp
  lcMenProm  = PROPER(lcDelMesag)
  DEFINE BAR 10 OF P03PU03 PROMPT (lcMenProm) SKIP FOR .F.

  *--If Status bid ,then no need to update the wip.
  IF laData[3]  = 'B'
    SELECT POSHDR          
    =RLOCK()

    *E301077,8 Don't update odded fields and call global function.[Begin
    *Odded REPLACE STATUS     WITH 'B',;
    *Odded         CANCEL     WITH 0,;
    *Odded         OPEN       WITH laData[40],;
    *Odded         POCANCEL   WITH {} ,;
    *Odded         MOD_ACTION WITH 'UNCANCELED',;
    *Odded         MOD_dATE   WITH gdSysDate,;
    *Odded         MOD_TIME   WITH TIME(),;
    *Odded         MOD_USERID WITH gcUser_id

    REPLACE STATUS     WITH 'B',;
            CANCEL     WITH 0,;
            OPEN       WITH laData[40],;
            POCANCEL   WITH {}

    =gfAdd_Info('POSHDR')
    *E301077,8 Don't update odded fields and call global function.[End

    *--Call TraceKey global function.
    =gfTraceKey('POSHDR',POSHDR.cstytype+POSHDR.po,'M')

    *--Complete unCmSess record ,Session completed.
    *=lfCrUnSess(' ','C') 
    =IIF(llFromEdi,.T.,lfCrUnSess(' ','C'))
    llContinue = .F.    

    SELECT POSHDR
    SHOW GETS
    RETURN
  ENDIF

  WAIT WINDOW 'Uncancelling and updating Work in Process !' NOWAIT
  Store 0 to lnNewOpn,lnNewAOpn
  SELECT POSLN
  SEEK lcAType+laData[1]
  DO WHILE cStyType+PO = lcAType+laData[1] AND !EOF() AND lcAType<>'C'
    =SEEK(POSLN.Style+STR(POSLN.LineNo,6),lcPOLine)
    
    *--CHECK IF THERE ARE ANY RECEIVED LINES FOR THIS STYLE.
    lnSvRec = IIF(!EOF(),RECNO(),0)
    DIMENSION laRECQTY(8)
    STORE 0 TO laRECQTY,lnTOTREC
    lcKey=lcAType+laData[1]+Style+STR(LineNo,6)
    IF SEEK (lcKEY)
      DO WHILE cStyType+PO+STYLE+STR(LineNo,6) = lcKey
        IF !(TranCd $ '245')
          SKIP
          LOOP
        ENDIF
        FOR lnI = 1 TO 8
          lcI = STR( lnI, 1)
          laRecQty( lnI ) = laRecQty( lnI ) + Qty&lcI
        ENDFOR  
        SKIP
      ENDDO
      lnTOTREC=laRecQty(1)+laRecQty(2)+laRecQty(3)+laRecQty(4)+laRecQty(5)+laRecQty(6)+laRecQty(7)+laRecQty(8)
    ENDIF
    IF lnSvRec<>0
      GO lnSvRec
    ENDIF  
    *C102357,1 AMH trigger to custom uncancel adornment order for JL [Start]
    *IF lcAType <> 'N' AND SEEK(POSLN.STYLE,'STYLE')
    
    *B605678,1 AMH Fix the bug of uncancel P/O don't update wip [Start]
    *IF lcAType <> 'N' AND SEEK(POSLN.STYLE,'STYLE') AND;
       IIF(!llFromEdi AND lcAType='A' AND ASCAN(laEvntTrig , PADR('CANCELPO',10)) = 0,.T.,.F.)
    IF lcAType <> 'N' AND SEEK(POSLN.STYLE,'STYLE') AND;
       IIF(!llFromEdi AND lcAType='A' AND ASCAN(laEvntTrig , PADR('CANCELPO',10)) <> 0,.F.,.T.)
    *B605678,1 AMH [End]
    
    *C102357,1 AMH [End]
      IF &lcPOLine..nSteps < 1
        SELECT STYLE
        =RLOCK()
        REPLACE WIP1 WITH WIP1 &lcSign (PosLn.Qty1 - laRecQty(1)),;
                WIP2 WITH WIP2 &lcSign (PosLn.Qty2 - laRecQty(2)),;        
                WIP3 WITH WIP3 &lcSign (PosLn.Qty3 - laRecQty(3)),;        
                WIP4 WITH WIP4 &lcSign (PosLn.Qty4 - laRecQty(4)),;        
                WIP5 WITH WIP5 &lcSign (PosLn.Qty5 - laRecQty(5)),;        
                WIP6 WITH WIP6 &lcSign (PosLn.Qty6 - laRecQty(6)),;                        
                WIP7 WITH WIP7 &lcSign (PosLn.Qty7 - laRecQty(7)),;        
                WIP8 WITH WIP8 &lcSign (PosLn.Qty8 - laRecQty(8)),;        
                TotWip WITH Wip1+Wip2+Wip3+Wip4+Wip5+Wip6+Wip7+Wip8
        UNLOCK
        SELECT (lcPOLine)
        REPLACE nSteps WITH 1
        FLUSH
      ENDIF
      IF &lcPOLine..nSteps < 2  AND lcAType <> 'A'
        SELECT STYLE
        REPLACE nWO1 WITH nWO1 &lcSign (PosLn.Qty1 - laRecQty(1)),;
                nWO2 WITH nWO2 &lcSign (PosLn.Qty2 - laRecQty(2)),;
                nWO3 WITH nWO3 &lcSign (PosLn.Qty3 - laRecQty(3)),;
                nWO4 WITH nWO4 &lcSign (PosLn.Qty4 - laRecQty(4)),;
                nWO5 WITH nWO5 &lcSign (PosLn.Qty5 - laRecQty(5)),;                
                nWO6 WITH nWO6 &lcSign (PosLn.Qty6 - laRecQty(6)),;
                nWO7 WITH nWO7 &lcSign (PosLn.Qty7 - laRecQty(7)),;
                nWO8 WITH nWO8 &lcSign (PosLn.Qty8 - laRecQty(8)),;                                
                nTotWO WITH nWO1+nWO2+nWO3+nWO4+nWO5+nWO6+nWO7+nWO8
        UNLOCK
        SELECT (lcPOLine)
        REPLACE nSteps WITH 2
        FLUSH
      ENDIF
      *--Call TraceKey global function.
      =gfTraceKey('STYLE',STYLE.Style,'M')
    ENDIF
    
    *C102357,1 AMH trigger to custom uncancel adornment order for JL [Start]
    *IF SEEK(POSLN.STYLE+POSLN.CWARECODE+SPACE(10),'STYDYE')
    
    *B605678,1 AMH Fix the bug of uncancel P/O don't update wip [Start]
    *IF SEEK(POSLN.STYLE+POSLN.CWARECODE+SPACE(10),'STYDYE') AND;
       IIF(!llFromEdi AND lcAType='A' AND ASCAN(laEvntTrig , PADR('CANCELPO',10)) = 0,.T.,.F.)
    IF SEEK(POSLN.STYLE+POSLN.CWARECODE+SPACE(10),'STYDYE') AND;
       IIF(!llFromEdi AND lcAType='A' AND ASCAN(laEvntTrig , PADR('CANCELPO',10)) <> 0,.F.,.T.)
    *B605678,1 AMH [End]
    
    *C102357,1 AMH [End]
      SELECT STYDYE
      IF &lcPOLine..nSteps < 3
        =RLOCK()
        REPLACE WIP1 WITH WIP1 &lcSign (PosLn.Qty1 - laRecQty(1)),;
                WIP2 WITH WIP2 &lcSign (PosLn.Qty2 - laRecQty(2)),;        
                WIP3 WITH WIP3 &lcSign (PosLn.Qty3 - laRecQty(3)),;        
                WIP4 WITH WIP4 &lcSign (PosLn.Qty4 - laRecQty(4)),;        
                WIP5 WITH WIP5 &lcSign (PosLn.Qty5 - laRecQty(5)),;        
                WIP6 WITH WIP6 &lcSign (PosLn.Qty6 - laRecQty(6)),;                        
                WIP7 WITH WIP7 &lcSign (PosLn.Qty7 - laRecQty(7)),;        
                WIP8 WITH WIP8 &lcSign (PosLn.Qty8 - laRecQty(8)),;        
                TotWip WITH Wip1+Wip2+Wip3+Wip4+Wip5+Wip6+Wip7+Wip8
        UNLOCK
        SELECT (lcPOLine)
        REPLACE nSteps WITH 3
        FLUSH
      ENDIF
      SELECT STYDYE
      IF &lcPOLine..nSteps < 4 AND lcAType <> 'A'
        =RLOCK()
        REPLACE nWO1 WITH nWO1 &lcSign (PosLn.Qty1 - laRecQty(1)),;
                nWO2 WITH nWO2 &lcSign (PosLn.Qty2 - laRecQty(2)),;
                nWO3 WITH nWO3 &lcSign (PosLn.Qty3 - laRecQty(3)),;                
                nWO4 WITH nWO4 &lcSign (PosLn.Qty4 - laRecQty(4)),;
                nWO5 WITH nWO5 &lcSign (PosLn.Qty5 - laRecQty(5)),;                
                nWO6 WITH nWO6 &lcSign (PosLn.Qty6 - laRecQty(6)),;
                nWO7 WITH nWO7 &lcSign (PosLn.Qty7 - laRecQty(7)),;
                nWO8 WITH nWO8 &lcSign (PosLn.Qty8 - laRecQty(8)),;                                
                nTotWO WITH nWO1+nWO2+nWO3+nWO4+nWO5+nWO6+nWO7+nWO8
        UNLOCK
        SELECT (lcPOLine)
        REPLACE nSteps WITH 4
        FLUSH
      ENDIF
      *--Call TraceKey global function.
      =gfTraceKey('STYDYE',STYDYE.style+STYDYE.cwarecode+STYDYE.dyelot,'M')
    ENDIF

    SELECT STYDYE
    IF lcAType = 'N' AND SEEK(POSLN.STYLE+PADR(POSLN.Vendor,6)+SPACE(10))
      IF &lcPOLine..nSteps < 5
        =RLOCK()
        REPLACE WIP1 WITH WIP1 &lcOpSign (PosLn.Qty1 - laRecQty(1)),;
                WIP2 WITH WIP2 &lcOpSign (PosLn.Qty2 - laRecQty(2)),;        
                WIP3 WITH WIP3 &lcOpSign (PosLn.Qty3 - laRecQty(3)),;        
                WIP4 WITH WIP4 &lcOpSign (PosLn.Qty4 - laRecQty(4)),;        
                WIP5 WITH WIP5 &lcOpSign (PosLn.Qty5 - laRecQty(5)),;        
                WIP6 WITH WIP6 &lcOpSign (PosLn.Qty6 - laRecQty(6)),;                        
                WIP7 WITH WIP7 &lcOpSign (PosLn.Qty7 - laRecQty(7)),;        
                WIP8 WITH WIP8 &lcOpSign (PosLn.Qty8 - laRecQty(8)),;        
                TotWip WITH Wip1+Wip2+Wip3+Wip4+Wip5+Wip6+Wip7+Wip8
        UNLOCK
        SELECT (lcPOLine)
        REPLACE nSteps WITH 5
        FLUSH
      ENDIF
      SELECT STYDYE
      IF &lcPOLine..nSteps < 6 AND lcAType <> 'A'
        =RLOCK()
        REPLACE nWO1 WITH nWO1 &lcOpSign (PosLn.Qty1 - laRecQty(1)),;
                nWO2 WITH nWO2 &lcOpSign (PosLn.Qty2 - laRecQty(2)),;
                nWO3 WITH nWO3 &lcOpSign (PosLn.Qty3 - laRecQty(3)),;                
                nWO4 WITH nWO4 &lcOpSign (PosLn.Qty4 - laRecQty(4)),;
                nWO5 WITH nWO5 &lcOpSign (PosLn.Qty5 - laRecQty(5)),;                
                nWO6 WITH nWO6 &lcOpSign (PosLn.Qty6 - laRecQty(6)),;
                nWO7 WITH nWO7 &lcOpSign (PosLn.Qty7 - laRecQty(7)),;
                nWO8 WITH nWO8 &lcOpSign (PosLn.Qty8 - laRecQty(8)),;                                
                nTotWO WITH nWO1+nWO2+nWO3+nWO4+nWO5+nWO6+nWO7+nWO8
        UNLOCK
        SELECT (lcPOLine)
        REPLACE nSteps WITH 6
        FLUSH
      ENDIF
      *--Call TraceKey global function.
      =gfTraceKey('STYDYE',STYDYE.style+STYDYE.cwarecode+STYDYE.dyelot,'M')
    ENDIF

    SELECT POSLN
    lnNewAOpn=lnNewAOpn &lcSign ((TOTQTY-lnTOTREC)*neCost1)
    lnNewOpn=lnNewOpn &lcSign (TOTQTY-lnTOTREC)
    SKIP
  ENDDO

  
  *--Update vendor record if the AP link is YES with the uncanceled ammount.
  *C200080,1 AMM Add the case of Dye Order
  *IF llApIstall AND &lcPOHdr..nSteps < 1 AND lcAType $ 'PR'
  IF llApIstall AND &lcPOHdr..nSteps < 1 AND lcAType $ 'PRD'
  *C200080,1 AMM end
    SELECT ApVendor
    =RLOCK()
    REPLACE nVenOpnPO WITH nVenOpnPO + lnNewAOpn
    UNLOCK IN ApVendor
    SELECT (lcPOHdr)
    REPLACE nSteps WITH 1
    FLUSH
    *--Call TraceKey global function.
    =gfTraceKey('APVENDOR',APVENDOR.cvendcode,'M')
  ENDIF
          
  IF &lcPOHdr..nSteps < 2
    SELECT POSHDR          
    SEEK lcAType+laData[1]
    =RLOCK()

    *E301077,8 Don't update odded fields and call global function.[Begin
    *Odded REPLACE STATUS     WITH 'H',;
    *Odded         CANCEL     WITH CANCEL-ABS(lnNewOpn),;
    *Odded         OPEN       WITH ABS(lnNewOpn),;
    *Odded         POCANCEL   WITH {} ,;
    *Odded         MOD_ACTION WITH 'UNCANCELED',;
    *Odded         MOD_dATE   WITH gdSysDate,;
    *Odded         MOD_TIME   WITH TIME(),;
    *Odded         MOD_USERID WITH gcUser_id

    *B602769,1 Put the status back to open insted of hold in inter location p/o.
    *REPLACE STATUS     WITH 'H'
    *E301291,1 AMM Consider the case of adornment order
    *REPLACE STATUS     WITH IIF(lcAType='N','O','H'),;
            CANCEL     WITH CANCEL-ABS(lnNewOpn),;
            OPEN       WITH ABS(lnNewOpn),;
            POCANCEL   WITH {}
    REPLACE STATUS     WITH IIF(lcAType$'NA','O','H'),;
            CANCEL     WITH CANCEL-ABS(lnNewOpn),;
            OPEN       WITH ABS(lnNewOpn),;
            POCANCEL   WITH {}
    *E301291,1 AMM end

    =gfAdd_Info('POSHDR')
    *E301077,8 Don't update odded fields and call global function.[End

    UNLOCK
    SELECT (lcPOHdr)
    REPLACE nSteps WITH 2
    FLUSH
    *--Call TraceKey global function.
    =gfTraceKey('POSHDR',POSHDR.cstytype+POSHDR.po,'M')
  ENDIF


********************
*-- C A N C E L .--*
********************
ELSE

  IF laData[3] = 'B'
    laData[40] = 0
    laData[3]  = 'X'
    lcStatus   = 'Cancelled'
    laStatus[1,1] = lcStatus
    laStatus[1,2] = laData[3]
    lcPromp       = lcUnCan
    lcDelMesag    = 'Uncancel'
    lcMenProm  = PROPER(lcDelMesag)
    DEFINE BAR 10 OF P03PU03 PROMPT (lcMenProm) SKIP FOR .F.

    SELECT POSHDR
    =RLOCK()

    *E301077,8 Don't update odded fields and call global function.[Begin
    *Odded REPLACE STATUS WITH 'X',;
    *Odded         CANCEL WITH 0,;
    *Odded         OPEN   WITH 0,;
    *Odded         POCANCEL WITH gdSysDate,;
    *Odded         MOD_ACTION WITH 'CANCELED',;
    *Odded         MOD_dATE   WITH gdSysDate,;
    *Odded         MOD_TIME   WITH TIME(),;
    *Odded         MOD_USERID WITH gcUser_id

    REPLACE STATUS WITH 'X',;
            CANCEL WITH 0,;
            OPEN   WITH 0,;
            POCANCEL WITH gdSysDate

    =gfAdd_Info('POSHDR')
    *E301077,8 Don't update odded fields and call global function.[End

    UNLOCK
    *--Call TraceKey global function.
    =gfTraceKey('POSHDR',POSHDR.cstytype+POSHDR.po,'M')

    *--Complete unCmSess record ,Session completed.
    *=lfCrUnSess(' ','C') 
    =IIF(llFromEdi,.T.,lfCrUnSess(' ','C'))

    llContinue = .F.    
    SELECT POSHDR

    SHOW GET pbDlt,1 PROMPT lcPromp
    SHOW GETS
    RETURN
  ELSE
    laData[39] = laData[39]  + laData[40]
    laData[40] = 0

    *B MAB Change status to Cancel if inter-location P/O [Begin
    *laData[3]  = IIF(laData[37]>0 OR (laData[3]='O'),'C','X')
    *lcStatus   = IIF(laData[37]>0 OR (laData[3]='O'),'Completed','Cancelled')
    *E301291,1 AMM Consider the case of adornment order
    *IF lcAType='N'
    IF lcAType$'NA'
    *E301291,1 AMM end
      laData[3]  = 'X'
      lcStatus   = 'Cancelled'
    ELSE
      
      *B605105,1 AMH Change lcStatus first [Start]
      *khalid
      *lcStatus   = IIF(laData[37]>0 OR (laData[3]='O'),'Completed','Cancelled')
      *laData[3]  = IIF(laData[37]>0 OR (laData[3]='O'),'C','X')

      lcStatus   = IIF(laData[37]>0 AND (laData[3]<>'C'),'Completed','Cancelled')
      laData[3]  = IIF(laData[37]>0 AND (laData[3]<>'C'),'C','X')
      *khalid 
      *lcStatus   = IIF(laData[37]>0 OR (laData[3]='O'),'Completed','Cancelled')
      *B605105,1 AMH [End]
      
    ENDIF
    *B MAB Change status to Cancel if inter-location P/O [Begin

    laStatus[1,1] = lcStatus
    laStatus[1,2] = laData[3]
    lcPromp       = lcUnCan
    lcDelMesag    = 'Uncancel'
    SHOW GET pbDlt,1 PROMPT lcPromp
    lcMenProm  = PROPER(lcDelMesag)
    DEFINE BAR 10 OF P03PU03 PROMPT (lcMenProm) SKIP FOR .F.
  ENDIF

  WAIT WINDOW 'Cancelling and updating work in process !' NOWAIT

  lnCanAmt=0
  SELECT POSLN
  SEEK lcAType+laData[1]
  DO WHILE cStyType+PO = lcAType+laData[1] AND !EOF() AND lcAType<>'C'
    =SEEK(POSLN.Style+STR(POSLN.LineNo,6),lcPOLine)
    
    *--CHECK IF THERE ARE ANY RECEIVED LINES FOR THIS STYLE.
    lnSvRec = IIF(!EOF(),RECNO(),0)
    DIMENSION laRECQTY(8)
    STORE 0 TO laRECQTY,lnTotRec
    lcKey=lcAType+laData[1]+Style+STR(LineNo,6)
    IF SEEK (lcKEY)
      DO WHILE cStyType+PO+STYLE+STR(LineNo,6) = lcKey
        IF !(TranCd $ '245')
          SKIP 
          LOOP 
        ENDIF
        FOR lnI = 1 TO 8
          lcI = STR( lnI, 1)
          laRecQty( lnI ) = laRecQty( lnI ) + Qty&lcI
        ENDFOR  
        SKIP
      ENDDO
      lnTotRec=laRecQty(1)+laRecQty(2)+laRecQty(3)+laRecQty(4)+laRecQty(5)+laRecQty(6)+laRecQty(7)+laRecQty(8)
    ENDIF
    IF lnSvRec<>0
      GO lnSvRec
    ENDIF  

    SELECT STYLE
    *C102357,4 AMH trigger to custom cancel adornment order for JL [Start]
    *IF lcAType <> 'N' AND SEEK(POSLN.STYLE)
    IF lcAType <> 'N' AND SEEK(POSLN.STYLE) AND;
       IIF(!llFromEdi AND lcAType='A' AND ASCAN(laEvntTrig , PADR('CANCELPO',10)) <> 0 AND !llIssued,.F.,.T.)
    *C102357,4 AMH [End]
      IF &lcPOLine..nSteps < 1
        =RLOCK()                         
        
        *B608077,1 NNA 05/11/2007(Begin) Get the MAX((PosLn.Qty1 - laRecQty(1)),0) instead of (PosLn.Qty2 - laRecQty(2))
        *B608077,1 NNA            to treat the problem of over receiving then cancel the remaining
        *REPLACE WIP1 WITH Wip1 &lcOpSign (PosLn.Qty1 - laRecQty(1)),;
                WIP2 WITH Wip2 &lcOpSign (PosLn.Qty2 - laRecQty(2)),;
                WIP3 WITH Wip3 &lcOpSign (PosLn.Qty3 - laRecQty(3)),;
                WIP4 WITH Wip4 &lcOpSign (PosLn.Qty4 - laRecQty(4)),;
                WIP5 WITH Wip5 &lcOpSign (PosLn.Qty5 - laRecQty(5)),;
                WIP6 WITH Wip6 &lcOpSign (PosLn.Qty6 - laRecQty(6)),;
                WIP7 WITH Wip7 &lcOpSign (PosLn.Qty7 - laRecQty(7)),;
                WIP8 WITH Wip8 &lcOpSign (PosLn.Qty8 - laRecQty(8)),;
                TotWip WITH Wip1+Wip2+Wip3+Wip4+Wip5+Wip6+Wip7+Wip8
        REPLACE WIP1 WITH Wip1 &lcOpSign MAX((PosLn.Qty1 - laRecQty(1)),0),;
                WIP2 WITH Wip2 &lcOpSign MAX((PosLn.Qty2 - laRecQty(2)),0),;
                WIP3 WITH Wip3 &lcOpSign MAX((PosLn.Qty3 - laRecQty(3)),0),;
                WIP4 WITH Wip4 &lcOpSign MAX((PosLn.Qty4 - laRecQty(4)),0),;
                WIP5 WITH Wip5 &lcOpSign MAX((PosLn.Qty5 - laRecQty(5)),0),;
                WIP6 WITH Wip6 &lcOpSign MAX((PosLn.Qty6 - laRecQty(6)),0),;
                WIP7 WITH Wip7 &lcOpSign MAX((PosLn.Qty7 - laRecQty(7)),0),;
                WIP8 WITH Wip8 &lcOpSign MAX((PosLn.Qty8 - laRecQty(8)),0),;
                TotWip WITH Wip1+Wip2+Wip3+Wip4+Wip5+Wip6+Wip7+Wip8
        *B608077,1 NNA(END)
        UNLOCK
        SELECT (lcPOLine)
        REPLACE nSteps WITH 1
        FLUSH
      ENDIF
      IF &lcPOLine..nSteps < 2 AND lcAType <> 'A'
        SELECT STYLE
        =RLOCK()

        *B608077,1 NNA 05/11/2007(Begin) Get the MAX((PosLn.Qty1 - laRecQty(1)),0) instead of (PosLn.Qty2 - laRecQty(2))
        *B608077,1 NNA            to treat the problem of over receiving then cancel the remaining
        *REPLACE nWO1 WITH nWO1 &lcOpSign (PosLn.Qty1 - laRecQty(1)),;
                nWO2 WITH nWO2 &lcOpSign (PosLn.Qty2 - laRecQty(2)),;
                nWO3 WITH nWO3 &lcOpSign (PosLn.Qty3 - laRecQty(3)),;
                nWO4 WITH nWO4 &lcOpSign (PosLn.Qty4 - laRecQty(4)),;
                nWO5 WITH nWO5 &lcOpSign (PosLn.Qty5 - laRecQty(5)),;
                nWO6 WITH nWO6 &lcOpSign (PosLn.Qty6 - laRecQty(6)),;
                nWO7 WITH nWO7 &lcOpSign (PosLn.Qty7 - laRecQty(7)),;
                nWO8 WITH nWO8 &lcOpSign (PosLn.Qty8 - laRecQty(8)),;
                nTotWO WITH nWO1+nWO2+nWO3+nWO4+nWO5+nWO6+nWO7+nWO8
        REPLACE nWO1 WITH nWO1 &lcOpSign MAX((PosLn.Qty1 - laRecQty(1)),0),;
                nWO2 WITH nWO2 &lcOpSign MAX((PosLn.Qty2 - laRecQty(2)),0),;
                nWO3 WITH nWO3 &lcOpSign MAX((PosLn.Qty3 - laRecQty(3)),0),;
                nWO4 WITH nWO4 &lcOpSign MAX((PosLn.Qty4 - laRecQty(4)),0),;
                nWO5 WITH nWO5 &lcOpSign MAX((PosLn.Qty5 - laRecQty(5)),0),;
                nWO6 WITH nWO6 &lcOpSign MAX((PosLn.Qty6 - laRecQty(6)),0),;
                nWO7 WITH nWO7 &lcOpSign MAX((PosLn.Qty7 - laRecQty(7)),0),;
                nWO8 WITH nWO8 &lcOpSign MAX((PosLn.Qty8 - laRecQty(8)),0),;
                nTotWO WITH nWO1+nWO2+nWO3+nWO4+nWO5+nWO6+nWO7+nWO8
        *B608077,1 NNA(END)

        UNLOCK
        SELECT (lcPOLine)
        REPLACE nSteps WITH 2
        FLUSH
      ENDIF
      *--Call TraceKey global function.
      =gfTraceKey('STYLE',STYLE.Style,'M')
    ENDIF

    SELECT STYDYE
    *C102357,4 AMH trigger to custom cancel adornment order for JL [Start]
    *IF SEEK(POSLN.STYLE+POSLN.CWARECODE+SPACE(10))
    IF SEEK(POSLN.STYLE+POSLN.CWARECODE+SPACE(10)) AND;
       IIF(!llFromEdi AND lcAType='A' AND ASCAN(laEvntTrig , PADR('CANCELPO',10)) <> 0 AND !llIssued,.F.,.T.)
    *C102357,4 AMH [End]
      IF &lcPOLine..nSteps < 3
        =RLOCK()

        *B608077,1 NNA 05/11/2007(Begin) Get the MAX((PosLn.Qty1 - laRecQty(1)),0) instead of (PosLn.Qty2 - laRecQty(2))
        *B608077,1 NNA            to treat the problem of over receiving then cancel the remaining
        *REPLACE WIP1 WITH Wip1 &lcOpSign (PosLn.Qty1 - laRecQty(1)),;
                WIP2 WITH Wip2 &lcOpSign (PosLn.Qty2 - laRecQty(2)),;
                WIP3 WITH Wip3 &lcOpSign (PosLn.Qty3 - laRecQty(3)),;
                WIP4 WITH Wip4 &lcOpSign (PosLn.Qty4 - laRecQty(4)),;
                WIP5 WITH Wip5 &lcOpSign (PosLn.Qty5 - laRecQty(5)),;
                WIP6 WITH Wip6 &lcOpSign (PosLn.Qty6 - laRecQty(6)),;
                WIP7 WITH Wip7 &lcOpSign (PosLn.Qty7 - laRecQty(7)),;
                WIP8 WITH Wip8 &lcOpSign (PosLn.Qty8 - laRecQty(8)),;
                TotWip WITH Wip1+Wip2+Wip3+Wip4+Wip5+Wip6+Wip7+Wip8
        REPLACE WIP1 WITH Wip1 &lcOpSign Max((PosLn.Qty1 - laRecQty(1)),0),;
                WIP2 WITH Wip2 &lcOpSign Max((PosLn.Qty2 - laRecQty(2)),0),;
                WIP3 WITH Wip3 &lcOpSign Max((PosLn.Qty3 - laRecQty(3)),0),;
                WIP4 WITH Wip4 &lcOpSign Max((PosLn.Qty4 - laRecQty(4)),0),;
                WIP5 WITH Wip5 &lcOpSign Max((PosLn.Qty5 - laRecQty(5)),0),;
                WIP6 WITH Wip6 &lcOpSign Max((PosLn.Qty6 - laRecQty(6)),0),;
                WIP7 WITH Wip7 &lcOpSign Max((PosLn.Qty7 - laRecQty(7)),0),;
                WIP8 WITH Wip8 &lcOpSign Max((PosLn.Qty8 - laRecQty(8)),0),;
                TotWip WITH Wip1+Wip2+Wip3+Wip4+Wip5+Wip6+Wip7+Wip8
        *B608077,1 NNA(END)

        UNLOCK
        SELECT (lcPOLine)
        REPLACE nSteps WITH 3
        FLUSH
      ENDIF
      IF &lcPOLine..nSteps < 4 AND lcAType <> 'A'
        SELECT STYDYE
        =RLOCK()

        *B608077,1 NNA 05/11/2007(Begin) Get the MAX((PosLn.Qty1 - laRecQty(1)),0) instead of (PosLn.Qty2 - laRecQty(2))
        *B608077,1 NNA            to treat the problem of over receiving then cancel the remaining
        *REPLACE nWO1 WITH nWO1 &lcOpSign (PosLn.Qty1 - laRecQty(1)),;
                nWO2 WITH nWO2 &lcOpSign (PosLn.Qty2 - laRecQty(2)),;
                nWO3 WITH nWO3 &lcOpSign (PosLn.Qty3 - laRecQty(3)),;
                nWO4 WITH nWO4 &lcOpSign (PosLn.Qty4 - laRecQty(4)),;
                nWO5 WITH nWO5 &lcOpSign (PosLn.Qty5 - laRecQty(5)),;
                nWO6 WITH nWO6 &lcOpSign (PosLn.Qty6 - laRecQty(6)),;
                nWO7 WITH nWO7 &lcOpSign (PosLn.Qty7 - laRecQty(7)),;
                nWO8 WITH nWO8 &lcOpSign (PosLn.Qty8 - laRecQty(8)),;
                nTotWO WITH nWO1+nWO2+nWO3+nWO4+nWO5+nWO6+nWO7+nWO8
        REPLACE nWO1 WITH nWO1 &lcOpSign MAX((PosLn.Qty1 - laRecQty(1)),0),;
                nWO2 WITH nWO2 &lcOpSign MAX((PosLn.Qty2 - laRecQty(2)),0),;
                nWO3 WITH nWO3 &lcOpSign MAX((PosLn.Qty3 - laRecQty(3)),0),;
                nWO4 WITH nWO4 &lcOpSign MAX((PosLn.Qty4 - laRecQty(4)),0),;
                nWO5 WITH nWO5 &lcOpSign MAX((PosLn.Qty5 - laRecQty(5)),0),;
                nWO6 WITH nWO6 &lcOpSign MAX((PosLn.Qty6 - laRecQty(6)),0),;
                nWO7 WITH nWO7 &lcOpSign MAX((PosLn.Qty7 - laRecQty(7)),0),;
                nWO8 WITH nWO8 &lcOpSign MAX((PosLn.Qty8 - laRecQty(8)),0),;
                nTotWO WITH nWO1+nWO2+nWO3+nWO4+nWO5+nWO6+nWO7+nWO8
        *B608077,1 NNA(END)
        UNLOCK
        SELECT (lcPOLine)
        REPLACE nSteps WITH 4
        FLUSH
      ENDIF
      *--Call TraceKey global function.
      =gfTraceKey('STYDYE',STYDYE.style+STYDYE.cwarecode+STYDYE.dyelot,'M')
    ENDIF

    SELECT STYDYE
    IF lcAType = 'N' AND SEEK(POSLN.STYLE+PADR(POSLN.Vendor,6)+SPACE(10))
      IF &lcPOLine..nSteps < 5
        =RLOCK()

        *B608077,1 NNA 05/11/2007(Begin) Get the MAX((PosLn.Qty1 - laRecQty(1)),0) instead of (PosLn.Qty2 - laRecQty(2))
        *B608077,1 NNA            to treat the problem of over receiving then cancel the remaining
        *REPLACE WIP1 WITH Wip1 &lcSign (PosLn.Qty1 - laRecQty(1)),;
                WIP2 WITH Wip2 &lcSign (PosLn.Qty2 - laRecQty(2)),;
                WIP3 WITH Wip3 &lcSign (PosLn.Qty3 - laRecQty(3)),;
                WIP4 WITH Wip4 &lcSign (PosLn.Qty4 - laRecQty(4)),;
                WIP5 WITH Wip5 &lcSign (PosLn.Qty5 - laRecQty(5)),;
                WIP6 WITH Wip6 &lcSign (PosLn.Qty6 - laRecQty(6)),;
                WIP7 WITH Wip7 &lcSign (PosLn.Qty7 - laRecQty(7)),;
                WIP8 WITH Wip8 &lcSign (PosLn.Qty8 - laRecQty(8)),;
                TotWip WITH Wip1+Wip2+Wip3+Wip4+Wip5+Wip6+Wip7+Wip8
        REPLACE WIP1 WITH Wip1 &lcSign MAX((PosLn.Qty1 - laRecQty(1)),0),;
                WIP2 WITH Wip2 &lcSign MAX((PosLn.Qty2 - laRecQty(2)),0),;
                WIP3 WITH Wip3 &lcSign MAX((PosLn.Qty3 - laRecQty(3)),0),;
                WIP4 WITH Wip4 &lcSign MAX((PosLn.Qty4 - laRecQty(4)),0),;
                WIP5 WITH Wip5 &lcSign MAX((PosLn.Qty5 - laRecQty(5)),0),;
                WIP6 WITH Wip6 &lcSign MAX((PosLn.Qty6 - laRecQty(6)),0),;
                WIP7 WITH Wip7 &lcSign MAX((PosLn.Qty7 - laRecQty(7)),0),;
                WIP8 WITH Wip8 &lcSign MAX((PosLn.Qty8 - laRecQty(8)),0),;
                TotWip WITH Wip1+Wip2+Wip3+Wip4+Wip5+Wip6+Wip7+Wip8
        *B608077,1 NNA(END)
        UNLOCK
        SELECT (lcPOLine)
        REPLACE nSteps WITH 5
        FLUSH
      ENDIF
      IF &lcPOLine..nSteps < 6 AND lcAType <> 'A'
        SELECT STYDYE
        =RLOCK()

        *B608077,1 NNA 05/11/2007(Begin) Get the MAX((PosLn.Qty1 - laRecQty(1)),0) instead of (PosLn.Qty2 - laRecQty(2))
        *B608077,1 NNA            to treat the problem of over receiving then cancel the remaining
        *REPLACE nWO1 WITH nWO1 &lcSign (PosLn.Qty1 - laRecQty(1)),;
                nWO2 WITH nWO2 &lcSign (PosLn.Qty2 - laRecQty(2)),;
                nWO3 WITH nWO3 &lcSign (PosLn.Qty3 - laRecQty(3)),;
                nWO4 WITH nWO4 &lcSign (PosLn.Qty4 - laRecQty(4)),;
                nWO5 WITH nWO5 &lcSign (PosLn.Qty5 - laRecQty(5)),;
                nWO6 WITH nWO6 &lcSign (PosLn.Qty6 - laRecQty(6)),;
                nWO7 WITH nWO7 &lcSign (PosLn.Qty7 - laRecQty(7)),;
                nWO8 WITH nWO8 &lcSign (PosLn.Qty8 - laRecQty(8)),;
                nTotWO WITH nWO1+nWO2+nWO3+nWO4+nWO5+nWO6+nWO7+nWO8
        REPLACE nWO1 WITH nWO1 &lcOpSign MAX((PosLn.Qty1 - laRecQty(1)),0),;
                nWO2 WITH nWO2 &lcOpSign MAX((PosLn.Qty2 - laRecQty(2)),0),;
                nWO3 WITH nWO3 &lcOpSign MAX((PosLn.Qty3 - laRecQty(3)),0),;
                nWO4 WITH nWO4 &lcOpSign MAX((PosLn.Qty4 - laRecQty(4)),0),;
                nWO5 WITH nWO5 &lcOpSign MAX((PosLn.Qty5 - laRecQty(5)),0),;
                nWO6 WITH nWO6 &lcOpSign MAX((PosLn.Qty6 - laRecQty(6)),0),;
                nWO7 WITH nWO7 &lcOpSign MAX((PosLn.Qty7 - laRecQty(7)),0),;
                nWO8 WITH nWO8 &lcOpSign MAX((PosLn.Qty8 - laRecQty(8)),0),;
                nTotWO WITH nWO1+nWO2+nWO3+nWO4+nWO5+nWO6+nWO7+nWO8
        *B608077,1 NNA(END)
        UNLOCK
        SELECT (lcPOLine)
        REPLACE nSteps WITH 6
        FLUSH
      ENDIF
      *--Call TraceKey global function.
      =gfTraceKey('STYDYE',STYDYE.style+STYDYE.cwarecode+STYDYE.dyelot,'M')
    ENDIF


    *--Remove the ordered quantities from the Cancel P/o lines if allocated.
    IF llRelAllo AND &lcPOLine..nSteps < 7
      DIME laZero[9]
      laZero=0
      SELECT POSLN
      
      *E301969,1 AMH remove the open quantity only [Start]
      *GATHER FROM laZero FIELDS Ord1,Ord2,Ord3,Ord4,Ord5,Ord6,Ord7,Ord8,TotOrd
      IF lcAType = 'P'
        lnRecNo = RECNO()
        lcOrder = SET('ORDER')
        SET ORDER TO TAG POSLN
        IF SEEK('P'+PO+STYLE+STR(LINENO,6)+'2')
          SUM REST WHILE TRANCD = '2' QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY;
                   TO ARRAY laZero
        ENDIF
        SET ORDER TO &lcOrder.
        IF BETWEEN(lnRecNo,1,RECCOUNT())
          GOTO lnRecNo
        ENDIF
        FOR lnI = 1 TO 8
          lcI = STR(lnI,1)
          REPLACE ('ORD'+lcI) WITH MIN(laZero[lnI],EVALUATE('ORD'+lcI))
        ENDFOR
        REPLACE TOTORD WITH MIN(TOTORD,laZero[9])
        IF !laOpFile[4]
          laOpFile[4]= gfOpenFile(gcDataDir+'ORDHDR','ORDHDR','SH')
        ENDIF
        SELECT CUTPICK
        lcOrder = SET('ORDER')
        SET ORDER TO TAG CUTPKORD
        =SEEK('2'+laData[1]+STR(POSLN.LINENO,6))
        DIME laRem[9]
        laRem = 0
        SCAN REST WHILE trancd+ctktno+ctktlineno+order+style+cordline =;
                        '2'+laData[1]+STR(POSLN.LINENO,6)
          FOR lnI = 1 TO 8
            lcI = STR(lnI,1)
            laRem[lnI]  = MAX(EVALUATE('QTY'+lcI)-laZero[lnI],0)
            laZero[lnI] = MAX(laZero[lnI]-EVALUATE('QTY'+lcI),0)
          ENDFOR
          laRem[9]  = MAX(TOTQTY-laZero[9],0)
          laZero[9] = MAX(laZero[9]-TOTQTY,0)
          
          IF laRem[9] > 0
            =SEEK('O'+CUTPICK.Order,'ORDHDR')
            =SEEK('O'+CUTPICK.Order+STR(INT(VAL(CUTPICK.cOrdLine)),6),'ORDLINE')
            FOR lnSizeNo = 1 TO 8
              lcSz = STR(lnSizeNo,1)
              *-Update values so as to be reflected on all involved cutting ticktes.
              REPLACE ('ORDLINE.Cut'+lcSz) WITH EVALUATE('ORDLINE.Cut'+lcSz) - laRem[lnSizeNo]
              REPLACE ('CUTPICK.Qty'+lcSz) WITH EVALUATE('CUTPICK.QTY'+lcSz) - laRem[lnSizeNo]
            ENDFOR
            REPLACE ORDHDR.TotCut  WITH ORDHDR.TotCut  - laRem[lnSizeNo]
            REPLACE ORDLINE.TotCut WITH ORDLINE.TotCut - laRem[lnSizeNo]
            REPLACE CUTPICK.TotQty WITH CUTPICK.TotQty - laRem[lnSizeNo]
            
            *--Call TraceKey global function.
            SELECT CUTPICK
            =lfTraceTag('CUTPICK','M')    
            =gfTraceKey('ORDHDR','O'+CUTPICK.Order,'M')
            =gfTraceKey('ORDLINE','O'+CUTPICK.Order+STR(INT(VAL(CUTPICK.cOrdLine)),6),'M')
          ENDIF
          
        ENDSCAN        
        =SEEK('2'+laData[1]+STR(POSLN.LINENO,6))
        DELETE REST WHILE trancd+ctktno+ctktlineno+order+style+cordline =;
                          '2'+laData[1]+STR(POSLN.LINENO,6);
                      FOR TotQty = 0 AND lfTraceTag('CUTPICK','D')
        SET ORDER TO &lcOrder.
        SELECT POSLN
      ELSE
        GATHER FROM laZero FIELDS Ord1,Ord2,Ord3,Ord4,Ord5,Ord6,Ord7,Ord8,TotOrd
      ENDIF
      *E301969,1 AMH [End]
      
      *B603298,1 Start, Added to clear the piktkt field in case of adornment P/O.
      IF lcAType = 'A'
        REPLACE ShipNo WITH ' '
      ENDIF
      *B603298,1 End.
      SELECT (lcPOLine)
      REPLACE nSteps WITH 7
      FLUSH
      *--Call TraceKey global function.
      *=gfTraceKey('POSLN',POSLN.cstytype+POSLN.po+POSLN.style+STR(POSLN.lineno,6)+POSLN.trancd,'M')
      SELECT POSLN
      =gfTraceKey('POSLN',cStyType+Po+cRsession+Shipno+Style+STR(Lineno,6)+Trancd,'M')
    ENDIF 

    SELECT POSLN
    lnCanAmt=lnCanAmt &lcSign ((TotQty-lnTotRec)*neCost1)
    SKIP
  ENDDO


  *--Update vendor record if the AP link is YES with the uncanceled ammount.
  *C200080,1 AMM Add the case of Dye Order
  *IF llApIstall AND &lcPOHdr..nSteps < 1 AND lcAType $ 'PR'
  IF llApIstall AND &lcPOHdr..nSteps < 1 AND lcAType $ 'PRD'
  *C200080,1 AMM end
    SELECT ApVendor
    =RLOCK()
    REPLACE nVenOpnPO WITH nVenOpnPO - lnCanAmt
    UNLOCK IN ApVendor
    SELECT (lcPOHdr)
    REPLACE nSteps WITH 1
    FLUSH
    *--Call TraceKey global function.
    =gfTraceKey('APVENDOR',APVENDOR.cvendcode,'M')
  ENDIF

  IF lcAType='P' AND !EMPTY(laData[13]) AND POSHDR.cLCStatus='A'  AND &lcPOHdr..nSteps < 2
    =lfLCPO('X',laData[13],lnCanAmt)  
    SELECT (lcPOHdr)
    REPLACE nSteps WITH 2
    FLUSH
  ENDIF

  *--Remove alocation for Cancelled P/O .
  
  *E301969,1 AMH Removing allocation done before this step in case of PO [Start]
  *IF llRelAllo AND &lcPOHdr..nSteps < 3
  IF llRelAllo AND lcAType <> 'P' AND &lcPOHdr..nSteps < 3
  *E301969,1 AMH [End]
  
    *E301077,8 open ordhdr only if not opened.
    IF !laOpFile[4]
      laOpFile[4]= gfOpenFile(gcDataDir+'ORDHDR','ORDHDR','SH')
    ENDIF
    *E301077,8 End.
    SELECT CUTPICK
    =SEEK('2'+laData[1])
    SCAN REST WHILE TranCd+cTktNo = '2'+laData[1]
      =SEEK('O'+CUTPICK.Order,'ORDHDR')
      =SEEK('O'+CUTPICK.Order+STR(INT(VAL(CUTPICK.cOrdLine)),6),'ORDLINE')
      FOR lnSizeNo = 1 TO 8
        lcSz = STR(lnSizeNo,1)
        lcRelQty = Qty&lcSz
        *-Update values so as to be reflected on all involved cutting ticktes.
        REPLACE ORDHDR.TotCut    WITH ORDHDR.TotCut    - lcRelQty
        REPLACE ORDLINE.TotCut   WITH ORDLINE.TotCut   - lcRelQty ,;
                ORDLINE.Cut&lcSz WITH ORDLINE.Cut&lcSz - lcRelQty
        REPLACE CUTPICK.TotQty   WITH CUTPICK.TotQty   - lcRelQty ,;
                CUTPICK.Qty&lcSz WITH 0
      ENDFOR
      *--Call TraceKey global function.
      SELECT CUTPICK
      
      *E301182,7 Adjust trace key master tag [Begin]
      *=gfTraceKey('CUTPICK',Trancd+cTktNo+Order+Style+cOrdline,'M')    
      =lfTraceTag('CUTPICK','M')    
      *E301182,7 Adjust trace key master tag [End  ]

      =gfTraceKey('ORDHDR','O'+CUTPICK.Order,'M')
      =gfTraceKey('ORDLINE','O'+CUTPICK.Order+STR(INT(VAL(CUTPICK.cOrdLine)),6),'M')
    ENDSCAN        
    
    =SEEK('2'+laData[1])
    *E301182,7 Adjust trace key master tag [Begin]
    *DELETE REST WHILE TranCd+cTktNo = '2'+laData[1] ;
    *     FOR TotQty = 0 AND gfTraceKey('CUTPICK',Trancd+cTktNo+Order+Style+cOrdline,'D')

    DELETE REST WHILE TranCd+cTktNo = '2'+laData[1] ;
         FOR TotQty = 0 AND lfTraceTag('CUTPICK','D')
    *E301182,7 Adjust trace key master tag [End  ]

    SELECT (lcPOHdr)
    REPLACE nSteps WITH 3
    FLUSH
  ENDIF


  IF &lcPOHdr..nSteps < 4
    SELECT POSHDR
    =RLOCK()

    *E301077,8 Don't update odded fields and call global function.[Begin
    *Odded REPLACE STATUS WITH laData[3],;
    *Odded         CANCEL WITH CANCEL+OPEN,;
    *Odded         OPEN   WITH 0 ,;
    *Odded         TOTORD WITH 0 ,;
    *Odded         POCANCEL   WITH gdSysDate,;
    *Odded         MOD_ACTION WITH IIF(RECEIVE > 0,'COMPLETED','CANCELED'),;
    *Odded         MOD_dATE   WITH gdSysDate,;
    *Odded         MOD_TIME   WITH TIME(),;
    *Odded         MOD_USERID WITH gcUser_id

    REPLACE STATUS WITH laData[3],;
            CANCEL WITH CANCEL+OPEN,;
            OPEN   WITH 0 ,;
            TOTORD WITH 0 ,;
            POCANCEL   WITH gdSysDate
    *B606743,1 WAB (Start) if the Po is completed , call function to add cancel(S) line 
    IF lcAType $ 'PR' AND laData[3] = 'C'
      =lfAdCnLine()
    ENDIF
    *B606743,1 WAB (End) 
            
    =gfAdd_Info('POSHDR')
    *E301077,8 Don't update odded fields and call global function.[End

    UNLOCK
    SELECT (lcPOHdr)
    REPLACE nSteps WITH 4
    FLUSH
    *--Call TraceKey global function.
    =gfTraceKey('POSHDR',POSHDR.cstytype+POSHDR.po,'M')
  ENDIF
  *E301484,1 WAM Send Inter Location PO or Purchase orders cancellation to sites
  =lfSendEdi(lcAType ,'860')
  *E301484,1 WAM (End)

ENDIF

*--Complete unCmSess record ,Session completed.
*E301484,1 WAM Do not call uncompleted session function if called from EDI
*=lfCrUnSess(' ','C') 
=IIF(llFromEdi,.T.,lfCrUnSess(' ','C'))
*E301484,1 (End)

*C102087,1 AMH Add record to POTOSEND when saving for GMA [Start]
IF !llFromEdi AND ASCAN(laEvntTrig , PADR('POTOSND',10)) <> 0
  =gfDoTriger('POSTY',PADR('POTOSND',10))
ENDIF
*C102087,1 AMH [End]

llCOntinue = .F.
SELECT POSHDR
WAIT CLEAR
SHOW GETS


*!*************************************************************
*! Name      : lfCkEOF
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Function if there is lines for P/O.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : .T. If No line was entered or exist
*!                      .F. Not  Eof() there is a line(s) found.
*!*************************************************************
*! Example   : =lfCkEOF()
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
*! Name    : lfRecalc
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose : Function to recalculate the costs in header.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfRecalc

lnAlias = SELECT()
SELECT (lcPOLine)
lnSavRc=IIF(!EOF(),RECNO(),0)
*B602861,1 Change the way of sum total estemeted to depend on gross insted of net.
*SUM TotQty,(TotQty*nECost1),(TotQty*nECost2),(TotQty*nECost3),(TotQty*nECost4),(TotQty*nECost5),;
             (TotQty*nCost1),(TotQty*nCost2),(TotQty*nCost3),(TotQty*nCost4),(TotQty*nCost5) TO ;
  laData[36],laData[31],laData[32],laData[33],laData[34],laData[35],;
             laData[51],laData[52],laData[53],laData[54],laData[55]


SUM TotQty,(TotQty*nECost2),(TotQty*nECost3),(TotQty*nECost4),(TotQty*nECost5),;
    (TotQty*IIF(Disc_Pcnt=0,nCost1,Gros_Price*(1-Disc_Pcnt/100))),(TotQty*nCost2),(TotQty*nCost3),(TotQty*nCost4),(TotQty*nCost5) TO ;
    laData[36],laData[32],laData[33],laData[34],laData[35],;
    laData[51],laData[52],laData[53],laData[54],laData[55]

*B604502,1 MHM 06/11/2001 [Start]
IF TYPE('lcTranTyp') = 'C' AND lcTranTyp $ "C"
  laData[40] = laData[36]
ENDIF
*B604502,1 MHM 06/11/2001 [END]

IF llMulCurr
  laData[31] = lfvEquCost('1',laData[51],laData[28],lnCurrUnt1)
ELSE
  laData[31] = laData[51]
ENDIF
*B602861,1 End.

laData[30]=laData[31]+laData[32]+laData[33]+laData[34]+laData[35]
*B603693,1 WMA [Begin]
*laData[40]=MAX((laData[36]-laData[37]-laData[38]-laData[39]),0)
*B603693,1 WMA [End]

IF lnSavRc<>0
  GOTO lnSavRc
ENDIF
SELECT (lcPOHdr)
GATHER FROM laData FIELDS &lcScFields
SELECT(lnAlias)
llRecalTot = .F.
RETURN



*!*************************************************************
*! Name      : lfReClCost
*! Developer : Khalid Mohi El-Din
*! Date      : 01/29/2001
*! Purpose : Function to recalculate the costs 
*!*************************************************************
*B604151,1 KHM 01/29/2001 
*!*************************************************************
FUNCTION lfReClCost
PRIVATE lnAlias,lcPExSign,lcDExSign,lcPUntSin,lcDUntSin,lnSCsAmt

STORE '/' TO lcPExSign,lcDExSign,lcPUntSin,lcDUntSin
lcPExSign = gfGetExSin(@lcPUntSin,POSHDR.cPriceCur)
lcDExSign = gfGetExSin(@lcDUntSin,POSHDR.cDutyCur)
lnSCsAmt = 0

lnAlias = SELECT(0)

SET ORDER TO TAG BOMLINE IN BOMLINE
IF SEEK("I"+'1'+laData[1],'BOMLINE')
  SELECT (lcPoLine)
  REPLACE nCost1  WITH 0,nCost2  WITH 0,nCost3  WITH 0,nCost4  WITH 0,;
          nCost5  WITH 0,nECost1 WITH 0,nECost2 WITH 0,nECost3 WITH 0,;
          nECost4 WITH 0,nECost5 WITH 0
  FOR lnCountr = IIF(lcAType = "D",2,1) TO 5

    lcCountr = STR(lnCountr,1)
    lcBomLKey = 'I1'+laData[1]+STR(&lcPoLine..LineNo,6)+lcCountr+&lcPoLine..Style
    IF SEEK(lcBomLKey,'BOMLINE')
      SELECT BOMLINE
      SUM REST (UnitCost*UnitQty)*StyQty WHILE cImTyp+cType+cTktNo+STR(LineNo,6)+cBomTyp+Style = lcBomLKey TO lnSCsAmt    

      SELECT (lcPoLine)
      REPLACE nCost&lcCountr  WITH IIF(TotQty<>0,ROUND((lnSCsAmt/TotQty),3),0),;
              nECost&lcCountr WITH (nCost&lcCountr &lcDExSign POSHDR.nDutyRat &lcDUntSin POSHDR.nDCurUnit)
    ENDIF 
  ENDFOR
ENDIF
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfUpdWip
*! Developer : AHMED MAHER (AMH)
*! Date      : 05/20/2002
*! Purpose   : Function to update the nmatwip field in fabric & fabdye files.
*!*************************************************************
*B605411,1
*B606028,1
*!*************************************************************
FUNCTION lfUpdWip

PRIVATE lcTmpWip,lcTmpWip1
lcTmpWip  = gfTempName()
lcTmpWip1 = gfTempName()
CREATE CURSOR (lcTmpWip) (CWARECODE C(6),NMATWIP N(12,3),DTRANDATE D,CISESSION C(6))
INDEX ON DTOS(DTRANDATE)+CWARECODE TAG (lcTmpWip) OF (lcTmpWip)
INDEX ON CWARECODE+CISESSION TAG (lcTmpWip1) OF (lcTmpWip)
SET ORDER TO TAG (lcTmpWip) IN (lcTmpWip)
SELECT (lcTmpLine)

*B606579,1 AMH Save the record number instead of the key [Start]
*PRIVATE lcSeekExpr,lcWhileExp,lnOpnQty,lnOpnQtySt,lnI,lcI,lcOrder,lcBomLnKey
PRIVATE lcSeekExpr,lcWhileExp,lnOpnQty,lnOpnQtySt,lnI,lcI,lcOrder,lnBomLnRec
*B606579,1 AMH [End]

PRIVATE lcFabric,lcColor,lcDyelot
IF llMFCall
  lcSeekExpr = 'M1'+Cuttkt+STR(lineno,6)
  lcWhileExp = "cIMTyp+cType+cTktNo+STR(lineno,6)=lcSeekExpr"
ELSE
  *khm1
  *lcSeekExpr = 'I1'+IIF(lcPType='S',Shipno,'')+Po+STR(LineNo,6)
  lcSeekExpr = IIF(lcPType='D','D','I')+'1'+IIF(lcPType='S',Shipno,'')+Po+STR(LineNo,6)
  *khm1
  
  lcWhileExp = "cIMTyp+cType+IIF(lcPType='S',Shipno,'')+cTktNo+STR(LineNo,6)=lcSeekExpr"
ENDIF
*khalid
*IF !USED('MATINVJL')
IF !USED('CTKTBOM')
  =gfOpenFile(gcDataDir+'CTKTBOM','CTKTBOM','SH')
ENDIF
IF SEEK(lcSeekExpr,'BOMLINE')
  SELECT BOMLINE
  lcOrder = ORDER('CTKTBOM')
  SET ORDER TO CTKTYP IN CTKTBOM
  SCAN REST WHILE &lcWhileExp. FOR CCATGTYP $ 'FT'
    =SEEK(CIMTYP+CTKTNO+ITEM+ICLR+MFGCODE+DYELOT,'CTKTBOM')
    STORE 0 TO lnOpnQty,lnOpnQtySt
    FOR lnI = 1 TO LEN(ALLTRIM(CSIZES))
      lcI = SUBSTR(ALLTRIM(CSIZES),lnI,1)
      lnOpnQty = lnOpnQty + MIN(EVALUATE(lcTmpLine+'.Qty'+lcI), laOpnQty[VAL(lcI)])
      lnOpnQtySt = lnOpnQtySt + laOpnQty[VAL(lcI)]
    ENDFOR
    
    *B606579,1 AMH Save the record number instead of the key [Start]
    *lcBomLnKey = EVALUATE(KEY())
    lnBomLnRec = RECNO()
    *B606579,1 AMH [End]
    
    lcFabric   = ITEM
    lcColor    = ICLR
    lcDyelot   = DYELOT
    IF llMFCall
      IF SEEK('M2'+EVALUATE(lcTmpLine+'.CUTTKT'))
        SUM REST WHILE cIMTyp+cType+cTktNo+STR(lineno,6)='M2'+EVALUATE(lcTmpLine+'.CUTTKT');
                 FOR ITEM = lcFabric .AND. ICLR = lcColor .AND. DYELOT = lcDyelot .AND.;
                     !EMPTY(cRSession) ITEMQTY TO lnOpnQtySt
      ENDIF
    ELSE
      *khm1
      *IF SEEK('I2'+IIF(lcPType='S',EVALUATE(lcTmpLine+'.Shipno'),'')+EVALUATE(lcTmpLine+'.Po'))
      *SUM REST WHILE cIMTyp+cType+IIF(lcPType='S',Shipno,'')+cTktNo+STR(LineNo,6)=;
                       'I2'+IIF(lcPType='S',EVALUATE(lcTmpLine+'.Shipno'),'')+EVALUATE(lcTmpLine+'.Po');
                 FOR ITEM = lcFabric .AND. ICLR = lcColor .AND. DYELOT = lcDyelot .AND.;
                     !EMPTY(cRSession) ITEMQTY TO lnOpnQtySt

      IF SEEK(IIF(lcPType='D','D','I')+'2'+IIF(lcPType='S',EVALUATE(lcTmpLine+'.Shipno'),'')+EVALUATE(lcTmpLine+'.Po'))
        SUM REST WHILE cIMTyp+cType+IIF(lcPType='S',Shipno,'')+cTktNo+STR(LineNo,6)=;
                       IIF(lcPType='D','D','I')+'2'+IIF(lcPType='S',EVALUATE(lcTmpLine+'.Shipno'),'')+EVALUATE(lcTmpLine+'.Po');
                 FOR ITEM = lcFabric .AND. ICLR = lcColor .AND. DYELOT = lcDyelot .AND.;
                     !EMPTY(cRSession) ITEMQTY TO lnOpnQtySt
      *khm1
      ENDIF
    ENDIF
    
    *B606579,1 AMH Restore the record number instead of the key [Start]
    *=SEEK(lcBomLnKey)
    IF BETWEEN(lnBomLnRec,1,RECCOUNT())
      GO lnBomLnRec
    ENDIF  
    *B606579,1 AMH [End]
    
    SELECT (lcTmpWip)
    ZAP
    IF !USED('MATINVJL')
      =gfOpenFile(gcDataDir+'MATINVJL','MATINVJL','SH')
    ENDIF
    SELECT BOMLINE
    IF SEEK(PADR(ITEM,7)+ICLR,'MATINVJL')
      SELECT MATINVJL
      SCAN REST WHILE cfabric+ccolor+cwarecode+cdyelot+crsession+cisession =;
                      PADR(BOMLINE.ITEM,7)+BOMLINE.ICLR FOR CDYELOT = CTKTBOM.DYELOT .AND.;
                      CTRANTYPE = '4' .AND. CIMTYP = CTKTBOM.CIMTYP .AND. CTKTNO = CTKTBOM.CUTTKT
        IF NISSUED > 0
          INSERT INTO (lcTmpWip) (CWARECODE,NMATWIP,DTRANDATE,CISESSION);
                          VALUES (MATINVJL.CWARECODE,MATINVJL.NISSUED,;
                                  MATINVJL.DTRANDATE,MATINVJL.CISESSION)
        ELSE
          SELECT (lcTmpWip)
          SET ORDER TO TAG (lcTmpWip1)
          IF SEEK(MATINVJL.CWARECODE+MATINVJL.CISESSION)
            REPLACE NMATWIP WITH NMATWIP - MATINVJL.NRECEIVED
          ELSE
            IF SEEK(MATINVJL.CWARECODE)
              lnRemender = MATINVJL.NRECEIVED
              SCAN REST WHILE CWARECODE+CISESSION = MATINVJL.CWARECODE
                lnMatWip = NMATWIP
                REPLACE NMATWIP WITH MAX(NMATWIP-lnRemender,0)
                lnRemender = MAX(lnRemender-lnMatWip,0)
                IF lnRemender <= 0
                  EXIT
                ENDIF
              ENDSCAN
            ENDIF
          ENDIF
          SET ORDER TO TAG (lcTmpWip)
        ENDIF
      ENDSCAN
    ENDIF
    IF RECCOUNT(lcTmpWip) > 0
      SELECT (lcTmpWip)
      LOCATE
      lnRemQty   = lnOpnQty*BOMLINE.UNITQTY
      lnRemQtySt = lnOpnQtySt
      SCAN
        IF SEEK(PADR(BOMLINE.ITEM,7)+BOMLINE.ICLR,'FABRIC')
          REPLACE FABRIC.NMATWIP WITH FABRIC.NMATWIP &lcWipSgn MAX(MIN(NMATWIP-lnRemQtySt,lnRemQty),0)
        ENDIF
        IF SEEK(PADR(BOMLINE.ITEM,7)+BOMLINE.ICLR+CWARECODE+CTKTBOM.DYELOT,'FABDYE')
          REPLACE FABDYE.NMATWIP WITH FABDYE.NMATWIP &lcWipSgn MAX(MIN(NMATWIP-lnRemQtySt,lnRemQty),0)
        ENDIF
        IF !EMPTY(CTKTBOM.DYELOT) .AND. SEEK(Fabric.Fabric+Fabric.Color+CWARECODE+SPACE(10),'FABDYE')
          REPLACE FABDYE.NMATWIP WITH FABDYE.NMATWIP &lcWipSgn MAX(MIN(NMATWIP-lnRemQtySt,lnRemQty),0)
        ENDIF
        lnRemQty   = MIN(MAX(lnRemQty-NMATWIP+lnRemQtySt,0),lnRemQty)
        lnRemQtySt = MAX(lnRemQtySt-NMATWIP,0)
      ENDSCAN
    ENDIF
  ENDSCAN
  SET ORDER TO TAG &lcOrder. IN CTKTBOM
ENDIF
USE IN (lcTmpWip)

*!*************************************************************
*! Name      : lfAdCnLine
*! Developer : Walid A. Wahab (WAB)
*! Date      : 04/07/2003
*! Purpose : Function to add cancel line to the po
*!*************************************************************
*B606743,1 
*!*************************************************************
FUNCTION lfAdCnLine
SELECT (lcPOLine)
SCAN
  DIMENSION laCanQTY(8)
  STORE 0 TO laCanQTY,lnTOTCan
  lcFields ='Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8'
  SCATTER FIELDS &lcFields TO laCanQTY
  lnTOTCan = TotQty
  IF SEEK(lcAType+laData[1]+style+STR(lineno,6),'POSLN')
    SELECT POSLN
    SCAN REST WHILE cstytype+po+style+STR(lineno,6)+trancd = ;
              lcAType+laData[1]+&lcPOLine..Style+STR(&lcPOLine..LineNo,6)  FOR Trancd $ '245'
      FOR lnI = 1 TO 8
        lcI = STR(lnI,1)
        laCanQTY[lnI] = laCanQTY[lnI] - MIN(laCanQTY[lnI],Qty&lcI)
      ENDFOR  
    ENDSCAN
    lnTOTCan=laCanQTY[1]+laCanQTY[2]+laCanQTY[3]+laCanQTY[4]+laCanQTY[5]+laCanQTY[6]+laCanQTY[7]+laCanQTY[8]
    IF lnTotCan > 0 
      SELECT (lcPOLine)
      SCATTER MEMVAR MEMO
      FOR lnCount = 1 to 8
        lcI=STR(lnCount,1)
        m.Qty&lcI = laCanQTY[lnCount]
      ENDFOR
      m.TotQty = lnTotCan
      m.TranCd='5'
      *B121328,1 NNA 01/28/2004 [Begin] Save The cancellation date
      m.date= gdSysDate
      *B121328,1 NNA [End]
      SELECT POSLN
      APPEND BLANK
      GATHER MEMVAR MEMO
    ENDIF
  ENDIF
  SELECT (lcPoLine)
ENDSCAN

*!*	*B122770,1 28/12/04 Put the Function if it was called from the EDI [Begin]
*!*	*!*************************************************************
*!*	*! Name      : lfvEquCost
*!*	*! Developer: Timour A. K. 
*!*	*! Date     : 10/10/97               
*!*	*! Purpose   : Get eguevalent cost by pathing the foreign.
*!*	*!*************************************************************
*!*	FUNCTION lfvEquCost
*!*	PARAMETERS lcCstNo,lnFrnCost,lnCurRate,lnCurUnt

*!*	lnCstType = lcIType&lcCstNo
*!*	IF lnCstType $ 'PMD'
*!*	  STORE '' TO lcPMethod,lcPUnMeth,lcDMethod,lcDUnMeth
*!*	  IF lnCstType='P'
*!*	    lcPMethod = gfGetExSin(@lcPUnMeth,lcCur1)
*!*	    lcPMethod = IIF(EMPTY(lcPMethod),'*',lcPMethod)
*!*	    lcPUnMeth = IIF(EMPTY(lcPUnMeth),'/',lcPUnMeth)
*!*	    lnEquCost = lnFrnCost &lcPMethod lnCurRate &lcPUnMeth lnCurUnt
*!*	  ELSE
*!*	    lcDMethod = gfGetExSin(@lcDUnMeth,lcCur2)
*!*	    lcDMethod = IIF(EMPTY(lcDMethod),'*',lcDMethod)
*!*	    lcDUnMeth = IIF(EMPTY(lcDUnMeth),'/',lcDUnMeth)
*!*	    lnEquCost = lnFrnCost &lcDMethod lnCurRate &lcDUnMeth lnCurUnt
*!*	  ENDIF
*!*	ELSE
*!*	  lnEquCost = lnFrnCost
*!*	ENDIF
*!*	lnEquCost = ROUND(lnEquCost,2)
*!*	RETURN (lnEquCost)
*!*	*B122770,1 28/12/04 Put the Function if it was called from the EDI [Begin]
