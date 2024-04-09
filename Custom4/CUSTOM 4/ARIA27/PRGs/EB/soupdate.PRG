*E301175,1 HS  12/30/1998 Add the capability to call the sales order Save
*E301175,1                and Delete functions from the EDI
*E301175,1                - from EDIProcessPO Class -
*B602496,1 WAM 02/18/1999 Update the order record in the uncmsess file
*B602555,1 WAM 02/19/1999 Check if any POs/C/Ts have been assigned to the 
*B602555,1                bulk order. If any, Release allocation from the 
*B602555,1                bulk order and allocate them to the newly received
*B602555,1                distribution order.
*E301077,5 WAM 02/28/1999 Inhance openning files to speed up transaction
*B602674,1 HS  03/17/1999 fix the bug "Variable llFromEDI not found"
*E301182,1 WAM 03/23/1999 Update CT/PO line number in the CUTPICK file.
*B602753,1 HDM 04/07/1999 Stop Calling NotePad Program In lpSavScr as the global save
*B602753,1 HDM 04/07/1999        Will Call it
*B602826,1 TAK 04/26/1999 Fixed wrong check on piktkt in canceling order.
*B802131,1 TAK 05/12/1999 Fix the program does not update the dates in Ordline.
*B602952,1 TAK 05/12/1999 Fix the wrong updating of Order Qty in Style and StyDye
*B602952,1                files if the style code was changed in the order line.
*C101557,1 TAK 06/08/1999 Added a new code for Order Category in summary folder.
*C200081,1 Reham On 06/29/1999  
*C200081,1 1- Add an event for the save function for the entire order process.
*C200081,1 2- Add an event to restore the allocation amount from the order header
*C200081,1    into its row in the user defined fields array.
*E301288,1 Reham On 07/20/1999
*E301288,1 1- Validate filling the style positions for all the lines.
*E301288,1 2- Save the style positions in the BOMVAR file.
*C200089,1 Reham On 07/22/1999
*C200089,1 1- Add an event if status changed to "Open"
*C200089,1 2- Add an event if status changed to "On Hold"
*C200089,1 3- Add an event if there is modifications in the sales order info.
*B802366,1 AMM 08/18/1999 Adjust the ship via POPUP in case of multi store orders
*E301305,1 AMM 08/25/1999 Make the complete date per order line due to a system setting
*E500309,1 AMM 01/23/2000 Deduct bulk order quantity from the booked quantity due to a system setting
*C200556,1 SSH Addt Trigger for GMA to Assign Sales Order # the same web order number. 
*B607436,1 TMI 07/30/2003 Overcome the error "Numeric overflow"
*E037853,1 HBG 16/02/2004 Change the width of Key field in EDITRANS to 40 char
*E038729,1 WAM 11/11/2004 Maintain SQL tables 
*E038463,1 WAM 11/15/2004 Update configuration WIP & WO 
*!*************************************************************
*! Name      : lfSavScr
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Save new or modified order
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfSavScr()
*!*************************************************************
FUNCTION lfSavScr

*E301175,1 Add this line to add a new parameter to know if the function was
*          called from EDI - from EDI transaction 860
*          (Purchase Order Modify) -. [Begin]
PARAMETERS llFromEDI

llFromEDI = IIF(TYPE('llFromEDI') = 'L' , llFromEDI , .F.)
*E301175,1 Add this line to add a new parameter to know if the function [End]

PRIVATE lnCanBulk,lnCancel,lnCancelAmt,lnLineCount,lcHdrAlo,lcDetAlo,;
        lcBulkYear,lcBulkPrd

STORE ' ' TO lcBulkYear,lcBulkPrd
*jica
*IF !SEEK(lcOrdType,lcOrdLine) .OR. laData[41] = 0
IF !INLIST(&lcOrdHdr..Mon_Flg,'G','L') AND (!SEEK(lcOrdType,lcOrdLine) .OR. laData[41] = 0)
  *E300420,1 Message : 32044
  *E300420,1 No lines entered! No updates performed.
  *E300420,1 Button : 00000 
  *E300420,1 Ok
  =gfModalGen('TRM32044B00000','DIALOG')
  llcSave = .F.
  SELECT (IIF(lnActFolder=2,lcOrdLine,lcOrdHdr))
  RETURN
ENDIF
IF laData[34] <=0
  *E300420,1 Message : 32028
  *E300420,1 The exchange rate must be greater than zero
  *E300420,1 Button : 00000 
  *E300420,1 Ok
  =gfModalGen('TRM32028B00000','ALERT')
  llcSave = .F.
  SELECT (IIF(lnActFolder=2,lcOrdLine,lcOrdHdr))
  RETURN
ENDIF
*B607436,1  TMI [Start] Overcome the error "Numeric overflow"
IF lfDivByZr()
  llcSave = .F.
  RETURN
ENDIF   
*B607436,1  TMI [End  ]
IF lcOrdType='O' .AND. laData[5]<>'B' .AND. !CHECKPRD(laData[8],'lcGlYear','lcGlPeriod','',.T.)
  STORE SPACE(4) TO lcGlYear
  STORE SPACE(2) TO lcGlPeriod
ENDIF
IF lcOrdType='O' .AND. laData[5]<>'B' .AND. laScrMode[4] .AND. SEEK(lcOrdType+&lcOrdHdr..cFromOrder,'OrdHdr')
  =CHECKPRD(OrdHdr.Entered,'lcBulkYear','lcBulkPrd','',.T.)
ENDIF
SET ORDER TO TAG 'ORDHDR' IN 'ORDHDR'
SET ORDER TO TAG 'ORDLINE' IN 'ORDLINE'

*E301288,1 Reham On 07/20/1999   *** Begin ***
IF llBomVarnt .AND. !lfStyPos()
  *** You have to fill the missing fields for the styles positions ***
  *** before saving, or you have to delete the related order line. ***
  *** <  OK  > ***
  =gfModalGen('TRM32072B00000','DIALOG')
  llCSave = .F.
  RETURN .F.
ENDIF
*E301288,1 Reham On 07/20/1999   *** End   ***

*C200081,1 Reham On 06/29/1999  *** Begin ***
*C200081,1 If there are user fields for this order, Call process Id to
*C200081,1 execute function before saving the entire order.
IF ASCAN(laEvntTrig , PADR('ORD_SAV',10)) <> 0
  llReturn = gfDoTriger('SOORD',PADR('ORD_SAV',10))
  IF !llReturn
    llCSave = .F.
    RETURN .F.
  ENDIF
ENDIF     
*C200081,1 Reham On 06/29/1999  *** End   ***

*C200081,1 Reham On 06/29/1999  *** Begin ***
*C200081,1 If there are user fields for this order, Call process Id to
*C200081,1 execute function before saving the entire order.
IF ASCAN(laEvntTrig , PADR('ALO_RES',10)) <> 0
  llReturn = gfDoTriger('SOORD',PADR('ALO_RES',10))
ENDIF
*C200081,1 Reham On 06/29/1999  *** End   ***

*C200089,1 Reham On 07/22/1999  *** Begin ***
IF laScrMode[3]
  DO CASE
    *C200089,1 If the order status changed to "Open"
    CASE laData[5] <> OrdHdr.STATUS .AND. laData[5] = "O" .AND. ASCAN(laEvntTrig , PADR('CHG_OPN',10)) <> 0
      llReturn = gfDoTriger('SOORD',PADR('CHG_OPN',10))
    *C200089,1 If the order status changed to "On Hold"
    CASE laData[5] <> OrdHdr.STATUS .AND. laData[5] = "H" .AND. ASCAN(laEvntTrig , PADR('CHG_HLD',10)) <> 0
      llReturn = gfDoTriger('SOORD',PADR('CHG_HLD',10))
    *C200089,1 If there is any modification happened on the current order header.
    OTHERWISE
      *C200089,1 Get all the order info in a temp array.
      DECLARE laOrdInfo[1]
      =gfSubStr(ALLTRIM(lcScFields) , @laOrdInfo , ',')
      *C200089,1 Flag to know that there is modifications in the current sales order.
      llModOrd = .F.
      *C200089,1 Slect the order header file.
      SELECT OrdHdr
      *C200089,1 Loop in all the order header fields to find if there is
      *C200089,1 any modifications happened on the sales order or not.
      FOR lnOrdCnt = 1 TO ALEN(laOrdInfo)
        *C200089,1 If there is difference between the field & the array to be saved.
        IF EVALUATE(laOrdInfo[lnOrdCnt]) <> laData[lnOrdCnt]
          llModOrd = .T.
          EXIT
        ENDIF
      ENDFOR
      IF llModOrd .AND. ASCAN(laEvntTrig , PADR('MOD_ORD',10)) <> 0
        llReturn = gfDoTriger('SOORD',PADR('MOD_ORD',10))
      ENDIF
  ENDCASE
ENDIF
*C200089,1 Reham On 07/22/1999  *** End   ***

*E500377,1 Generate numbers automatically for EDI orders
*laData[1] = IIF((llContinue AND !EMPTY(laData[1])) OR laScrMode[3],laData[1],;
            IIF(laSetups[6,2]='Y',lfGtOrder(),gfSequence('ORDER','','',laData[15])))

*C124452,1 WAM Use the same web order number
*laData[1] = IIF((llContinue AND !EMPTY(laData[1])) OR laScrMode[3],laData[1],;
            IIF(laSetups[6,2]='Y' AND !llFromEDI,lfGtOrder(),gfSequence('ORDER','','',laData[15])))
laData[1] = IIF( (llContinue AND !EMPTY(laData[1])) OR laScrMode[3] OR (llFromEDI AND &lcOrdHdr..lFromWeb),laData[1],;
            IIF(laSetups[6,2]='Y' AND !llFromEDI,lfGtOrder(),gfSequence('ORDER','','',laData[15])))
*C124452,1 WAM (End)

*E500377,1 (End)

*C200556,1 SSH Trigger for GMA to Assign Sales Order # the same web order number. [Begin] 
*=gfDoTriger('SOORD',PADR('ASSGNORD',10))
*C200556,1 [End]

lcCurrOrd = laData[1]
*E301288,1 Reham On 07/20/1999   *** Begin ***
*E301288,1 Replace the order # in all the adornment order lines.
IF llBomVarnt .AND. laScrMode[4]
  SELECT (lcT_BomVar)
  REPLACE ALL cCost_Id WITH laData[1]
ENDIF
*E301288,1 Reham On 07/20/1999   *** End   ***

*E301175,1 Add this line to add the capability to call this function from
*          the EDI - from EDIProcessPO Class - [Begin]

*-- If the function was not called from EDI
IF !llFromEDI
*E301175,1 Add this line to add the capability to call this function [End]

  =gfSavSess('SOORD', lcFiles, @laVariables,lcSession)
  SELECT UnCmSess
  *B602496,1 Update the order record in the uncmsess file
  =SEEK('O'+PADR('SOORD',10)+gcUser_id+lcSession)
  *B602496,1 (End)

  REPLACE cCurrObj WITH 'pbSav'
  
*E301175,1 Add this line to add the capability to call this function from
*          the EDI - from EDIProcessPO Class - [Begin]
ENDIF    && End of IF !llFromEDI
*E301175,1 Add this line to add the capability to call this function [End]

SELECT (lcOrdHdr)
lcEdiOrd = Order
=RLOCK()
REPLACE Order WITH laData[1]
UNLOCK
SHOW GET laData[1] DISABLE
llAria4xp = .T.
IF lcOrdType='O' .AND. laData[5]<>'B' .AND. laScrMode[3] .AND. USED(lcAlocated)
  SELECT (lcAlocated)
  SET ORDER TO TAG 'CUTPKORD' IN (lcAlocated)
  GO TOP
  IF !EOF()
    *E038729,1 WAM 11/11/2004 Maintain SQL tables 
    IF oAriaApplication.isRemoteComp
      lcSetClass = SET('CLASSLIB')
      SET CLASSLIB TO (oAriaApplication.lcAria4Class+"MAIN.VCX"),(oAriaApplication.lcAria4Class+"UTILITY.VCX")
      lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr,3,'',.T.)
      SET CLASSLIB TO &lcSetClass.
      IF TYPE('lcTranCode') = 'N'
        =oAriaApplication.RemoteCompanyData.CheckRetResult("BEGINTRAN",lcTranCode,.T.)
      ELSE 
        WAIT 'Updating alocated quantity...' WINDOW NOWAIT
        SELECT (lcAlocated)
        DO WHILE !EOF()
          lcTranCd = TRANCD
          lcTktNo  = CTKTNO
          lnOrdQty = 0
          lnBudQty = 0
          SCAN REST WHILE TRANCD+CTKTNO+CTKTLINENO+ORDER+STYLE+CORDLINE = lcTranCd+lcTktNo
            SCATTER MEMVAR
            *-- Get original allocated quantity
            =SEEK(m.TRANCD+m.CTKTNO+m.CTKTLINENO+m.ORDER+m.STYLE+m.CORDLINE,'CUTPICK','Cutpkord')
            lnOrdQty = lnOrdQty + m.TotQty-CUTPICK.TotQty
            IF !EMPTY(m.cUpdSizes)
              IF SEEK(OrdHdr.cOrdType+m.Order+PADL(ALLTRIM(m.cOrdLine),6),lcOrdLine,'OrdLine')
                *-- Update style WIP & WO
                IF SEEK(m.Style,'Style')
                  =RLOCK('Style')
                  FOR lnSize = 1 TO LEN(ALLTRIM(m.cUpdSizes))
                    lcSize = SUBSTR(m.cUpdSizes,lnSize,1)
                    lnBudQty = lnBudQty - CutPick.Qty&lcSize + m.Qty&lcSize              
                    REPLACE WIP&lcSize WITH WIP&lcSize - CutPick.Qty&lcSize + m.Qty&lcSize , ;
                            TOTWIP     WITH TOTWIP     - CutPick.Qty&lcSize + m.Qty&lcSize , ;
                            NWO&lcSize WITH NWO&lcSize - CutPick.Qty&lcSize + m.Qty&lcSize , ; 
                            NTOTWO     WITH NTOTWO     - CutPick.Qty&lcSize + m.Qty&lcSize IN Style
                  ENDFOR
                  UNLOCK IN Style
                ENDIF  
                *-- Update Warehouse WIP & WO 
                IF SEEK(m.Style + &lcOrdLine..cWareCode+SPACE(10) ,'StyDye')
                  =RLOCK('StyDye')
                  FOR lnSize = 1 TO LEN(ALLTRIM(m.cUpdSizes))
                    lcSize = SUBSTR(m.cUpdSizes,lnSize,1)
                    REPLACE WIP&lcSize WITH WIP&lcSize - CutPick.Qty&lcSize + m.Qty&lcSize , ;
                            TOTWIP     WITH TOTWIP     - CutPick.Qty&lcSize + m.Qty&lcSize , ;
                            NWO&lcSize WITH NWO&lcSize - CutPick.Qty&lcSize + m.Qty&lcSize , ; 
                            NTOTWO     WITH NTOTWO     - CutPick.Qty&lcSize + m.Qty&lcSize IN StyDye
                  ENDFOR
                  UNLOCK IN StyDye
                ENDIF  
                *E038463,1 WAM 11/15/2004 Update configuration WIP & WO 
                IF !EMPTY(&lcOrdLine..Dyelot) AND SEEK(m.Style + &lcOrdLine..cWareCode+&lcOrdLine..Dyelot,'StyDye')
                  =RLOCK('StyDye')
                  FOR lnSize = 1 TO LEN(ALLTRIM(m.cUpdSizes))
                    lcSize = SUBSTR(m.cUpdSizes,lnSize,1)
                    REPLACE WIP&lcSize WITH WIP&lcSize - CutPick.Qty&lcSize + m.Qty&lcSize , ;
                            TOTWIP     WITH TOTWIP     - CutPick.Qty&lcSize + m.Qty&lcSize , ;
                            NWO&lcSize WITH NWO&lcSize - CutPick.Qty&lcSize + m.Qty&lcSize , ; 
                            NTOTWO     WITH NTOTWO     - CutPick.Qty&lcSize + m.Qty&lcSize IN StyDye
                  ENDFOR
                  UNLOCK IN StyDye
                ENDIF  
                *E038463,1 WAM 11/15/2004 (End)
              ENDIF  
            ENDIF
            *-- Update ticket lines Ordered quantity
            lcSelString="UPDATE PosLn SET Ord1=Ord1+"+ALLTRIM(STR(m.Qty1-CutPick.Qty1))+",Ord2=Ord2+"+ALLTRIM(STR(m.Qty2-CutPick.Qty2))+;
                        ",Ord3=Ord3+"+ALLTRIM(STR(m.Qty3-CutPick.Qty3))+",Ord4=Ord4+"+ALLTRIM(STR(m.Qty4-CutPick.Qty4))+;
                        ",Ord5=Ord5+"+ALLTRIM(STR(m.Qty5-CutPick.Qty5))+",Ord6=Ord6+"+ALLTRIM(STR(m.Qty6-CutPick.Qty6))+;
                        ",Ord7=Ord7+"+ALLTRIM(STR(m.Qty7-CutPick.Qty7))+",Ord8=Ord8+"+ALLTRIM(STR(m.Qty8-CutPick.Qty8))+;
                        ",TotOrd=TotOrd+"+ALLTRIM(STR(m.TotQty-CutPick.TotQty))
            *-- Update ticket lines budget quantities
            IF !EMPTY(m.cUpdSizes) 
              lcSelString=lcSelString+IIF('1' $ m.cUpdSizes,",Qty1=Qty1+"+ALLTRIM(STR(m.Qty1-CUTPICK.Qty1)),'')+;
                                      IIF('2' $ m.cUpdSizes,",Qty2=Qty2+"+ALLTRIM(STR(m.Qty2-CUTPICK.Qty2)),'')+;
                                      IIF('3' $ m.cUpdSizes,",Qty3=Qty3+"+ALLTRIM(STR(m.Qty3-CUTPICK.Qty3)),'')+;
                                      IIF('4' $ m.cUpdSizes,",Qty4=Qty4+"+ALLTRIM(STR(m.Qty4-CUTPICK.Qty4)),'')+;
                                      IIF('5' $ m.cUpdSizes,",Qty5=Qty5+"+ALLTRIM(STR(m.Qty5-CUTPICK.Qty5)),'')+;
                                      IIF('6' $ m.cUpdSizes,",Qty6=Qty6+"+ALLTRIM(STR(m.Qty6-CUTPICK.Qty6)),'')+;
                                      IIF('7' $ m.cUpdSizes,",Qty7=Qty7+"+ALLTRIM(STR(m.Qty7-CUTPICK.Qty7)),'')+;
                                      IIF('8' $ m.cUpdSizes,",Qty8=Qty8+"+ALLTRIM(STR(m.Qty8-CUTPICK.Qty8)),'')+;
                                      ",TotQty=TotQty+"+ALLTRIM(STR(m.TotQty-CUTPICK.TotQty))
            ENDIF
            IF m.TranCd = '1'
              lcSelString=lcSelString+" WHERE cBusDocu+cstytype+po+cInvType+style+STR([lineno],6)+trancd='PU"+m.CTKTNO+'0001'+m.Style+m.cTktLineNo+"1'"
            ELSE
              lcSelString=lcSelString+" WHERE cBusDocu+cstytype+po+cInvType+style+STR([lineno],6)+trancd='PP"+m.CTKTNO+'0001'+m.Style+m.cTktLineNo+"1'"
            ENDIF
            lnResult = oAriaApplication.RemoteCompanyData.Execute(lcSelString,'',"SAVEFILE","",lcTranCode,4,'',SET("DATASESSION"))
            SELECT CUTPICK
            IF m.TOTQTY = 0
              DELETE
            ELSE
              GATHER MEMVAR
            ENDIF
          ENDSCAN
          *-- Update ticket total Ordered & budget quantity
          IF lcTranCd = '1'
            *-- Change Cutting quantity if order allocated quantity has been changed
            lcSelString = "UPDATE PosHdr SET TotOrd=TotOrd+"+ALLTRIM(STR(lnOrdQty))+",nStyOrder = nStyOrder + "+ALLTRIM(STR(lnBudQty))+" ,[Open] = [Open] + "+ALLTRIM(STR(lnBudQty))+" WHERE cBusDocu+cstytype+po='PU"+lcTktNo+"'"
          ELSE
            *-- Change Purchased quantity if order allocated quantity has been changed
            lcSelString = "UPDATE PosHdr SET TotOrd=TotOrd+"+ALLTRIM(STR(lnOrdQty))+",nStyOrder = nStyOrder + "+ALLTRIM(STR(lnBudQty))+" ,[Open] = [Open] + "+ALLTRIM(STR(lnBudQty))+" WHERE cBusDocu+cstytype+po='PP"+lcTktNo+"'"
          ENDIF
          lnResult = oAriaApplication.RemoteCompanyData.Execute(lcSelString,'',"SAVEFILE","",lcTranCode,4,'',SET("DATASESSION"))
          SELECT (lcAlocated)
        ENDDO
        lnResult = lfSqlUpdate('CUTPICK',lcTranCode, SET("DATASESSION"),'trancd,ctktno,ctktlineno,order,style,cordline')
        IF lnResult <=0
          =oAriaApplication.RemoteCompanyData.CheckRetResult("SQLUPDATE",lnResult,.T.)
          =oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
        ELSE 
          IF oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode,.T.) = 1
          ELSE
            =oAriaApplication.RemoteCompanyData.CheckRetResult("COMMITTRAN",lnResult,.T.)
          ENDIF
        ENDIF
        WAIT Clear
      ENDIF
      *E038729,1 WAM 11/11/2004 (End)
    ELSE
      WAIT 'Updating alocated quantity...' WINDOW NOWAIT
      IF 'PO' $ gcCmpModules
        =gfOpenFile(gcDataDir+'POSHDR',gcDataDir+'POSHDR','SH')
        =gfOpenFile(gcDataDir+'POSLN',gcDataDir+'POSLN','SH')
      ENDIF
      IF 'MF' $ gcCmpModules
        =gfOpenFile(gcDataDir+'CUTTKTH',gcDataDir+'CUTTKTH','SH')
        =gfOpenFile(gcDataDir+'CUTTKTL',gcDataDir+'CUTLIN','SH')
      ENDIF
      =gfOpenFile(gcDataDir+'CUTPICK',gcDataDir+'CUTPKORD','SH')
      SELECT (lcAlocated)
      DO WHILE !EOF()
        lcTranCd = TRANCD
        lcTktNo  = CTKTNO
        lnOrdQty = 0
        lnBudQty = 0
        SCAN REST WHILE TRANCD+CTKTNO+CTKTLINENO+ORDER+STYLE+CORDLINE = lcTranCd+lcTktNo
          SCATTER MEMVAR
          *-- Get original allocated quantity
          =SEEK(m.TRANCD+m.CTKTNO+m.CTKTLINENO+m.ORDER+m.STYLE+m.CORDLINE,'CUTPICK','Cutpkord')
          lnOrdQty = lnOrdQty + m.TotQty-CUTPICK.TotQty
          IF !EMPTY(m.cUpdSizes)
            IF SEEK(OrdHdr.cOrdType+m.Order+PADL(ALLTRIM(m.cOrdLine),6),lcOrdLine,'OrdLine')
              *-- Update style WIP & WO
              IF SEEK(m.Style,'Style')
                =RLOCK('Style')
                FOR lnSize = 1 TO LEN(ALLTRIM(m.cUpdSizes))
                  lcSize = SUBSTR(m.cUpdSizes,lnSize,1)
                  lnBudQty = lnBudQty - CutPick.Qty&lcSize + m.Qty&lcSize
                  REPLACE WIP&lcSize WITH WIP&lcSize - CutPick.Qty&lcSize + m.Qty&lcSize , ;
                          TOTWIP     WITH TOTWIP     - CutPick.Qty&lcSize + m.Qty&lcSize , ;
                          NWO&lcSize WITH NWO&lcSize - CutPick.Qty&lcSize + m.Qty&lcSize , ; 
                          NTOTWO     WITH NTOTWO     - CutPick.Qty&lcSize + m.Qty&lcSize IN Style
                ENDFOR
                UNLOCK IN Style
              ENDIF  
              *-- Update Warehouse WIP & WO 
              IF SEEK(m.Style + &lcOrdLine..cWareCode+SPACE(10) ,'StyDye')
                =RLOCK('StyDye')
                FOR lnSize = 1 TO LEN(ALLTRIM(m.cUpdSizes))
                  lcSize = SUBSTR(m.cUpdSizes,lnSize,1)
                  REPLACE WIP&lcSize WITH WIP&lcSize - CutPick.Qty&lcSize + m.Qty&lcSize , ;
                          TOTWIP     WITH TOTWIP     - CutPick.Qty&lcSize + m.Qty&lcSize , ;
                          NWO&lcSize WITH NWO&lcSize - CutPick.Qty&lcSize + m.Qty&lcSize , ; 
                          NTOTWO     WITH NTOTWO     - CutPick.Qty&lcSize + m.Qty&lcSize IN StyDye
                ENDFOR
                UNLOCK IN StyDye
              ENDIF  
              *E038463,1 WAM 11/15/2004 Update configuration WIP & WO 
              IF !EMPTY(&lcOrdLine..Dyelot) AND SEEK(m.Style + &lcOrdLine..cWareCode+&lcOrdLine..Dyelot,'StyDye')
                =RLOCK('StyDye')
                FOR lnSize = 1 TO LEN(ALLTRIM(m.cUpdSizes))
                  lcSize = SUBSTR(m.cUpdSizes,lnSize,1)
                  REPLACE WIP&lcSize WITH WIP&lcSize - CutPick.Qty&lcSize + m.Qty&lcSize , ;
                          TOTWIP     WITH TOTWIP     - CutPick.Qty&lcSize + m.Qty&lcSize , ;
                          NWO&lcSize WITH NWO&lcSize - CutPick.Qty&lcSize + m.Qty&lcSize , ; 
                          NTOTWO     WITH NTOTWO     - CutPick.Qty&lcSize + m.Qty&lcSize IN StyDye
                ENDFOR
                UNLOCK IN StyDye
              ENDIF  
              *E038463,1 WAM 11/15/2004 (End)
            ENDIF  
          ENDIF
          SELECT (lcDetAlo)
          *E301182,1 Update CT/PO line number in the CUTPICK file.
          *=SEEK(IIF(m.TranCd='1',m.CTKTNO+m.Style,'P'+m.CTKTNO+m.Style))
          =SEEK(IIF(m.TranCd='2','P','')+m.CTKTNO+m.Style+m.cTktLineNo+'1')
          *E301182,1 (End)
          =RLOCK()
          REPLACE ORD1   WITH ORD1   - CUTPICK.Qty1   + m.Qty1 ,;
                  ORD2   WITH ORD2   - CUTPICK.Qty2   + m.Qty2 ,;
                  ORD3   WITH ORD3   - CUTPICK.Qty3   + m.Qty3 ,;
                  ORD4   WITH ORD4   - CUTPICK.Qty4   + m.Qty4 ,;
                  ORD5   WITH ORD5   - CUTPICK.Qty5   + m.Qty5 ,;
                  ORD6   WITH ORD6   - CUTPICK.Qty6   + m.Qty6 ,;
                  ORD7   WITH ORD7   - CUTPICK.Qty7   + m.Qty7 ,;
                  ORD8   WITH ORD8   - CUTPICK.Qty8   + m.Qty8 ,;
                  TOTORD WITH TOTORD - CUTPICK.TotQty + m.TotQty
          IF !EMPTY(&lcAlocated..cUpdSizes)
            REPLACE Qty1   WITH IIF('1' $ &lcAlocated..cUpdSizes,Qty1-CUTPICK.Qty1+m.Qty1,Qty1) ,;
                    Qty2   WITH IIF('2' $ &lcAlocated..cUpdSizes,Qty2-CUTPICK.Qty2+m.Qty2,Qty2) ,;
                    Qty3   WITH IIF('3' $ &lcAlocated..cUpdSizes,Qty3-CUTPICK.Qty3+m.Qty3,Qty3) ,;
                    Qty4   WITH IIF('4' $ &lcAlocated..cUpdSizes,Qty4-CUTPICK.Qty4+m.Qty4,Qty4) ,;
                    Qty5   WITH IIF('5' $ &lcAlocated..cUpdSizes,Qty5-CUTPICK.Qty5+m.Qty5,Qty5) ,;
                    Qty6   WITH IIF('6' $ &lcAlocated..cUpdSizes,Qty6-CUTPICK.Qty6+m.Qty6,Qty6) ,;
                    Qty7   WITH IIF('7' $ &lcAlocated..cUpdSizes,Qty7-CUTPICK.Qty7+m.Qty7,Qty7) ,;
                    Qty8   WITH IIF('8' $ &lcAlocated..cUpdSizes,Qty8-CUTPICK.Qty8+m.Qty8,Qty8) ,;
                    TotQty WITH TotQty - CUTPICK.TotQty + m.TotQty
          ENDIF
          UNLOCK
          SELECT CUTPICK
          =RLOCK()
          IF m.TOTQTY = 0
            DELETE
          ELSE
            =RLOCK()
            GATHER MEMVAR
            UNLOCK
          ENDIF
          UNLOCK
        ENDSCAN
        lcHdrAlo = IIF(TranCd='1','CutTktH','PosHdr')
        lcDetAlo = IIF(TranCd='1','CutTktL','PosLn')
        SELECT (lcHdrAlo)
        =SEEK(IIF(lcTranCd='1',lcTktNo,'P'+lcTktNo))
        =RLOCK()
        REPLACE TotOrd WITH TotOrd + lnOrdQty
        IF m.TranCd = '1'
          REPLACE Pcs_Bud WITH Pcs_Bud + lnBudQty ,;
                  Pcs_Opn WITH Pcs_Opn + lnBudQty
        ELSE
          REPLACE nStyOrder WITH nStyOrder + lnBudQty ,;
                  Open      WITH Open  + lnBudQty
        ENDIF
        UNLOCK
      ENDDO
      *E301077,5 Inhance openning files to speed up transaction
      =IIF('PO' $ gcCmpModules,gfCloseFile('POSHDR')  AND gfCloseFile('POSLN'),.T.)
      =IIF('MF' $ gcCmpModules,gfCloseFile('CUTTKTH') AND gfCloseFile('CUTTKTL'),.T.)
      =gfCloseFile('CUTPICK')
      *E301077,5 (End)
    ENDIF
  ENDIF
ENDIF
*E301077,5 Inhance openning files to speed up transaction
=gfOpenFile(gcDataDir+'icStyHst',gcDataDir+'Styhst','SH') 
=IIF('AL' $ gcCmpModules,gfOpenFile(gcDataDir+'PikTkt',gcDataDir+'PikTkt','SH'),.T.)
=gfOpenFile(gcDataDir+'ORDCANLN',gcDataDir+'ORDCANLN','SH') 
*E301077,5 (End)

*E038729,1 WAM 11/11/2004 Maintain SQL tables 
IF oAriaApplication.isRemoteComp 
  llBlkAloTrn = .F.
  SELECT DIST cFromOrder FROM (lcOrdLine) WHERE !EMPTY(cFromOrder) INTO ARRAY laFromOrder
  IF _TALLY > 0
    LOCAL lcSetClass
    lcSetClass = SET('CLASSLIB')
    SET CLASSLIB TO (oAriaApplication.lcAria4Class+"MAIN.VCX"),(oAriaApplication.lcAria4Class+"UTILITY.VCX")
    lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr,3,'',.T.)
    SET CLASSLIB TO &lcSetClass.
    IF TYPE('lcTranCode') = 'N'
      =oAriaApplication.RemoteCompanyData.CheckRetResult("BEGINTRAN",lcTranCode,.T.)
    ELSE
      lcFromOrder = ''
      FOR lnFromOrder = 1 TO ALEN(laFromOrder)
        lcFromOrder = lcFromOrder + "'" + laFromOrder[lnFromOrder] + "',"
      ENDFOR
      lcFromOrder = LEFT(lcFromOrder,LEN(lcFromOrder)-1)
      lcSelString = "SELECT * FROM cutpick WHERE [Order] IN (" + lcFromOrder + ")"
      IF oAriaApplication.RemoteCompanyData.Execute(lcSelString ,'',"CutPick","CutPick",lcTranCode,4,'',SET("DATASESSION")) = 1
        lnCurBuff = CURSORGETPROP("Buffering")
        =CURSORSETPROP("Buffering" ,3,"CutPick")
        INDEX ON trancd+order+cordline TAG 'CUTORD'
        INDEX ON trancd+ctktno+ctktlineno+order+style+cordline TAG 'CUTPKORD' ADDITIVE
        =CURSORSETPROP("Buffering" ,lnCurBuff ,"CutPick")
        SET ORDER TO TAG 'CUTORD'
        llBlkAloTrn = .T.
      ENDIF
    ENDIF
  ENDIF
ELSE
  llBlkAloTrn = .T.
  =gfOpenFile(gcDataDir+'CUTPICK',gcDataDir+'CUTORD','SH')
ENDIF  
*E038729,1 WAM 11/11/2004 (End)

SELECT ORDLINE
SET RELATION TO
SELECT (lcOrdLine)
SET DELETE OFF
SET ORDER TO TAG 0
GO TOP
WAIT 'Updating order lines...' WINDOW NOWAIT
lcUntSin = ''
lcExRSin = gfGetExSin(@lcUntSin, laData[33])

STORE 0 TO lnCancel,lnCancelAmt,lnCanBulk
lnLineCount=IIF(SEEK(lcOrdType+laData[1],'ORDHDR'),ORDHDR.LastLine,0)
*B802131,1 Check if the dates was changed in this session and set the flag.
*E301305,1 AMM update the complete date due to the system setting (per line or order)
*llUpdDates = IIF(laData[9]<>ORDHDR.Start OR laData[10]<>ORDHDR.Complete,.T.,.F.)
llUpdDates = IIF(laData[9]<>ORDHDR.Start OR (!llCDPerL .AND. laData[10]<>ORDHDR.Complete) ,.T.,.F.)
*E301305,1 AMM end 
*B802131,1 End.

SCAN FOR IIF(OrdHdr.Status='B',.T.,Flag='M' .OR. (Flag='N' .AND. !DELETED() .AND. TotQty > 0))

  IF lcOrdType='O' .AND. laData[5] <> 'B' .AND. !DELETED() .AND. ;
     (laScrMode[4] .OR. OrdHdr.Status='B') .AND. nSteps < 1 .AND. ;
    SEEK(lcOrdType+cFromOrder+STR(BulkLineNo,6),'OrdLine')

    *-- Decrease ordered quantity of the warehouse originaly assigned to the bulk 
    *-- order being depleted
    =SEEK(OrdLine.Style+OrdLine.cWareCode+SPACE(10),'StyDye')
    SELECT StyDye
    =RLOCK('StyDye')
    REPLACE Ord1 WITH Ord1 - MIN(OrdLine.Qty1,&lcOrdLine..Qty1),;
            Ord2 WITH Ord2 - MIN(OrdLine.Qty2,&lcOrdLine..Qty2),;
            Ord3 WITH Ord3 - MIN(OrdLine.Qty3,&lcOrdLine..Qty3),;
            Ord4 WITH Ord4 - MIN(OrdLine.Qty4,&lcOrdLine..Qty4),;
            Ord5 WITH Ord5 - MIN(OrdLine.Qty5,&lcOrdLine..Qty5),;
            Ord6 WITH Ord6 - MIN(OrdLine.Qty6,&lcOrdLine..Qty6),;
            Ord7 WITH Ord7 - MIN(OrdLine.Qty7,&lcOrdLine..Qty7),;
            Ord8 WITH Ord8 - MIN(OrdLine.Qty8,&lcOrdLine..Qty8),;
            TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8
    UNLOCK IN 'StyDye'
    *E038463,1 WAM 11/09/2004 Update ordered quantity at the Configuration level
    IF !EMPTY(OrdLine.Dyelot) AND SEEK(OrdLine.Style+OrdLine.cWareCode+OrdLine.Dyelot,'StyDye')
      =RLOCK('StyDye')
      REPLACE Ord1 WITH Ord1 - MIN(OrdLine.Qty1,&lcOrdLine..Qty1),;
              Ord2 WITH Ord2 - MIN(OrdLine.Qty2,&lcOrdLine..Qty2),;
              Ord3 WITH Ord3 - MIN(OrdLine.Qty3,&lcOrdLine..Qty3),;
              Ord4 WITH Ord4 - MIN(OrdLine.Qty4,&lcOrdLine..Qty4),;
              Ord5 WITH Ord5 - MIN(OrdLine.Qty5,&lcOrdLine..Qty5),;
              Ord6 WITH Ord6 - MIN(OrdLine.Qty6,&lcOrdLine..Qty6),;
              Ord7 WITH Ord7 - MIN(OrdLine.Qty7,&lcOrdLine..Qty7),;
              Ord8 WITH Ord8 - MIN(OrdLine.Qty8,&lcOrdLine..Qty8),;
              TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8 IN StyDye
      UNLOCK IN 'StyDye'
    ENDIF
    *E038463,1 WAM 11/09/2004 (End)
    SELECT Style 
    =SEEK(OrdLine.Style)
    RLOCK('Style')
    REPLACE Ord1   WITH Ord1 - MIN(OrdLine.Qty1,&lcOrdLine..Qty1),;
            Ord2   WITH Ord2 - MIN(OrdLine.Qty2,&lcOrdLine..Qty2),;
            Ord3   WITH Ord3 - MIN(OrdLine.Qty3,&lcOrdLine..Qty3),;
            Ord4   WITH Ord4 - MIN(OrdLine.Qty4,&lcOrdLine..Qty4),;
            Ord5   WITH Ord5 - MIN(OrdLine.Qty5,&lcOrdLine..Qty5),;
            Ord6   WITH Ord6 - MIN(OrdLine.Qty6,&lcOrdLine..Qty6),;
            Ord7   WITH Ord7 - MIN(OrdLine.Qty7,&lcOrdLine..Qty7),;
            Ord8   WITH Ord8 - MIN(OrdLine.Qty8,&lcOrdLine..Qty8),;
            TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8
    UNLOCK IN 'Style'

    SELECT (lcOrdLine)
    =RLOCK()
    REPLACE nSteps WITH 1
    UNLOCK
  ENDIF
  
  IF lcOrdType='O' .AND. laData[5]<>'B' .AND. nSteps < 2 
    IF OrdHdr.Status <> 'B' .AND. SEEK(lcOrdType+Order+STR(LineNo,6),'OrdLine')
      SELECT StyDye                           
      =SEEK(OrdLine.Style+OrdLine.cWareCode+SPACE(10))
      =RLOCK('StyDye')
      REPLACE Ord1   WITH Ord1   - OrdLine.Qty1 ,;
              Ord2   WITH Ord2   - OrdLine.Qty2 ,;
              Ord3   WITH Ord3   - OrdLine.Qty3 ,;
              Ord4   WITH Ord4   - OrdLine.Qty4 ,;
              Ord5   WITH Ord5   - OrdLine.Qty5 ,;
              Ord6   WITH Ord6   - OrdLine.Qty6 ,;
              Ord7   WITH Ord7   - OrdLine.Qty7 ,;
              Ord8   WITH Ord8   - OrdLine.Qty8 ,;
              TotOrd WITH TotOrd - OrdLine.TotQty
      UNLOCK IN 'StyDye'
      *E038463,1 WAM 11/09/2004 Update ordered quantity at the Configuration level
      IF !EMPTY(OrdLine.Dyelot) AND SEEK(OrdLine.Style+OrdLine.cWareCode+OrdLine.Dyelot)
        =RLOCK('StyDye')
        REPLACE Ord1   WITH Ord1   - OrdLine.Qty1 ,;
                Ord2   WITH Ord2   - OrdLine.Qty2 ,;
                Ord3   WITH Ord3   - OrdLine.Qty3 ,;
                Ord4   WITH Ord4   - OrdLine.Qty4 ,;
                Ord5   WITH Ord5   - OrdLine.Qty5 ,;
                Ord6   WITH Ord6   - OrdLine.Qty6 ,;
                Ord7   WITH Ord7   - OrdLine.Qty7 ,;
                Ord8   WITH Ord8   - OrdLine.Qty8 ,;
                TotOrd WITH TotOrd - OrdLine.TotQty
        UNLOCK IN 'StyDye'
      ENDIF
      *E038463,1 WAM 11/09/2004 (End)
      SELECT Style
      =SEEK(OrdLine.Style)
      =RLOCK('Style')
      REPLACE Ord1   WITH Ord1   - OrdLine.Qty1 ,;
              Ord2   WITH Ord2   - OrdLine.Qty2 ,;
              Ord3   WITH Ord3   - OrdLine.Qty3 ,;
              Ord4   WITH Ord4   - OrdLine.Qty4 ,;
              Ord5   WITH Ord5   - OrdLine.Qty5 ,;
              Ord6   WITH Ord6   - OrdLine.Qty6 ,;
              Ord7   WITH Ord7   - OrdLine.Qty7 ,;
              Ord8   WITH Ord8   - OrdLine.Qty8 ,;
              TotOrd WITH TotOrd - OrdLine.TotQty
      UNLOCK IN 'Style'
    ENDIF  
    IF !DELETED(lcOrdLine)
      SELECT StyDye                           
      =SEEK(&lcOrdLine..Style+&lcOrdLine..cWareCode+SPACE(10))
      =RLOCK('StyDye')
      REPLACE Ord1   WITH Ord1   + &lcOrdLine..Qty1 ,;
              Ord2   WITH Ord2   + &lcOrdLine..Qty2 ,;
              Ord3   WITH Ord3   + &lcOrdLine..Qty3 ,;
              Ord4   WITH Ord4   + &lcOrdLine..Qty4 ,;
              Ord5   WITH Ord5   + &lcOrdLine..Qty5 ,;
              Ord6   WITH Ord6   + &lcOrdLine..Qty6 ,;
              Ord7   WITH Ord7   + &lcOrdLine..Qty7 ,;
              Ord8   WITH Ord8   + &lcOrdLine..Qty8 ,;
              TotOrd WITH TotOrd + &lcOrdLine..TotQty
      UNLOCK IN 'StyDye'
      *E038463,1 WAM 11/09/2004 Update ordered quantity at the Configuration level
      SELECT StyDye                           
      IF !EMPTY(&lcOrdLine..Dyelot) AND SEEK(&lcOrdLine..Style+&lcOrdLine..cWareCode+&lcOrdLine..Dyelot)
        =RLOCK('StyDye')
        REPLACE Ord1   WITH Ord1   + &lcOrdLine..Qty1 ,;
                Ord2   WITH Ord2   + &lcOrdLine..Qty2 ,;
                Ord3   WITH Ord3   + &lcOrdLine..Qty3 ,;
                Ord4   WITH Ord4   + &lcOrdLine..Qty4 ,;
                Ord5   WITH Ord5   + &lcOrdLine..Qty5 ,;
                Ord6   WITH Ord6   + &lcOrdLine..Qty6 ,;
                Ord7   WITH Ord7   + &lcOrdLine..Qty7 ,;
                Ord8   WITH Ord8   + &lcOrdLine..Qty8 ,;
                TotOrd WITH TotOrd + &lcOrdLine..TotQty
        UNLOCK IN 'StyDye'
      ENDIF
      *E038463,1 WAM 11/09/2004 (End)
      SELECT Style
      =SEEK(&lcOrdLine..Style)
      =RLOCK('Style')
      REPLACE Ord1   WITH Ord1   + &lcOrdLine..Qty1 ,;
              Ord2   WITH Ord2   + &lcOrdLine..Qty2 ,;
              Ord3   WITH Ord3   + &lcOrdLine..Qty3 ,;
              Ord4   WITH Ord4   + &lcOrdLine..Qty4 ,;
              Ord5   WITH Ord5   + &lcOrdLine..Qty5 ,;
              Ord6   WITH Ord6   + &lcOrdLine..Qty6 ,;
              Ord7   WITH Ord7   + &lcOrdLine..Qty7 ,;
              Ord8   WITH Ord8   + &lcOrdLine..Qty8 ,;
              TotOrd WITH TotOrd + &lcOrdLine..TotQty
      UNLOCK IN 'Style'
    ENDIF  
    SELECT (lcOrdLine)
    =RLOCK()    
    REPLACE nSteps WITH 2
    UNLOCK
  ENDIF
  
  lnCanBulk = 0
  IF lcOrdType='O' .AND. laData[5]<>'B' .AND. ;
     (laScrMode[4] .OR. OrdHdr.Status='B') .AND. !DELETED() .AND. SEEK(lcOrdType+cFromOrder,'OrdHdr') AND ;
     SEEK(lcOrdType+cFromOrder+STR(BulkLineNo,6),'OrdLine')

    =CHECKPRD(OrdHdr.Entered,'lcBulkYear','lcBulkPrd','',.T.)
    
    lnCanBulk = MIN(OrdLine.Qty1,Qty1)+MIN(OrdLine.Qty2,Qty2)+;
                MIN(OrdLine.Qty3,Qty3)+MIN(OrdLine.Qty4,Qty4)+;
                MIN(OrdLine.Qty5,Qty5)+MIN(OrdLine.Qty6,Qty6)+;
                MIN(OrdLine.Qty7,Qty7)+MIN(OrdLine.Qty8,Qty8)
    lnCancel    = lnCancel    + lnCanBulk
    lnCancelAmt = lnCancelAmt + lnCanBulk*OrdLine.Price
    lnOpmAmnt = lnCanBulk*OrdLine.Price &lcExRSin laData[34] &lcUntSin laData[50]
    IF SEEK(Style+lcBulkYear,'icStyHst') .AND. &lcOrdLine..nSteps < 5
      lnOrdAmt = lnOpmAmnt &lcExRSin laData[34] &lcUntSin laData[50]
      SELECT icStyHst
      =RLOCK()
      REPLACE nOrdQty&lcBulkPrd WITH nOrdQty&lcBulkPrd - lnCanBulk ,;
              nOrdQty           WITH nOrdQty           - lnCanBulk ,;
              nOrdAmt&lcBulkPrd WITH nOrdAmt&lcBulkPrd - lnOrdAmt  ,;
              nOrdAmt           WITH nOrdAmt           - lnOrdAmt
      UNLOCK
      SELECT (lcOrdLine)
      =RLOCK()
      REPLACE nSteps WITH 5
      UNLOCK
    ENDIF
    IF nSteps < 6
      =RLOCK('ORDLINE')
      SELECT ORDLINE
      =RLOCK()
      REPLACE Qty1   WITH MAX(Qty1-&lcOrdLine..Qty1,0),;
              Qty2   WITH MAX(Qty2-&lcOrdLine..Qty2,0),;
              Qty3   WITH MAX(Qty3-&lcOrdLine..Qty3,0),;
              Qty4   WITH MAX(Qty4-&lcOrdLine..Qty4,0),;
              Qty5   WITH MAX(Qty5-&lcOrdLine..Qty5,0),;
              Qty6   WITH MAX(Qty6-&lcOrdLine..Qty6,0),;
              Qty7   WITH MAX(Qty7-&lcOrdLine..Qty7,0),;
              Qty8   WITH MAX(Qty8-&lcOrdLine..Qty8,0),;
              Totqty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
      *E500309,1 AMM update the booked quantity in bulk order
      REPLACE Book1   WITH MAX(Book1-&lcOrdLine..Book1,0),;
              Book2   WITH MAX(Book2-&lcOrdLine..Book2,0),;
              Book3   WITH MAX(Book3-&lcOrdLine..Book3,0),;
              Book4   WITH MAX(Book4-&lcOrdLine..Book4,0),;
              Book5   WITH MAX(Book5-&lcOrdLine..Book5,0),;
              Book6   WITH MAX(Book6-&lcOrdLine..Book6,0),;
              Book7   WITH MAX(Book7-&lcOrdLine..Book7,0),;
              Book8   WITH MAX(Book8-&lcOrdLine..Book8,0),;
              TotBook WITH Book1+Book2+Book3+Book4+Book5+Book6+Book7+Book8
      *E500309,1 AMM end

      UNLOCK IN 'ORDLINE'
      SELECT (lcOrdLine)
      =RLOCK()
      REPLACE nSteps WITH 6
      UNLOCK
    ENDIF
    *E500309,1 AMM If bulk order , deduct from booked else add to canceled quantity
    SELECT ORDHDR
    =SEEK(lcOrdType+&lcOrdLine..cFromOrder)
    =RLOCK()
    REPLACE Book      WITH Book    - lnCanBulk  ,;
            BookAmt   WITH BookAmt - lnCanBulk*OrdLine.Price ,;
            Open      WITH Open    - lnCanBulk  ,;
            Openamt   WITH Openamt - lnCanBulk*OrdLine.Price ,;
            Status    WITH IIF(Open = 0 ,'X',Status)
    UNLOCK
    SELECT (lcOrdLine)
    *E500309,1 (End)
  ENDIF
  IF lcOrdType='O' .AND. laData[5]<>'B' .AND. laScrMode[3] .AND. ;
     Picked .AND. SEEK(PikTkt,'PikTkt')
    SELECT PikTkt
    =RLOCK() 
    REPLACE Store WITH &lcOrdLine..Store
    UNLOCK
    SELECT (lcOrdLine)
  ENDIF
  =SEEK(lcOrdType+Order+STR(LineNo,6),'OrdLine')
  IF lcOrdType='O' .AND. laData[5]<>'B' .AND. SEEK(Style+lcGlYear,'icStyHst') .AND. ;
     &lcOrdLine..nSteps < 7
    lnOrdAmt = ;
    IIF(DELETED(lcOrdLine),0,&lcOrdLine..TotQty*&lcOrdLine..Price &lcExRSin laData[34] &lcUntSin laData[50]) - ;
    IIF(laScrMode[4] OR OrdHdr.Status='B',0,OrdLine.TotQty*OrdLine.Price &lcExRSin OrdHdr.nExRate &lcUntSin OrdHdr.nCurrUnit)
    SELECT icStyHst
    =RLOCK()
    REPLACE nOrdQty&lcGlPeriod WITH nOrdQty&lcGlPeriod - ;
                                    IIF(laScrMode[4] OR OrdHdr.Status='B',0,OrdLine.TotQty) + ;
                                    IIF(DELETED(lcOrdLine),0,&lcOrdLine..TotQty) ,;
            nOrdQty            WITH nOrdQty- IIF(laScrMode[4] OR OrdHdr.Status='B',0,OrdLine.TotQty) + ;
                                    IIF(DELETED(lcOrdLine),0,&lcOrdLine..TotQty) ,;
            nOrdAmt&lcGlPeriod WITH nOrdAmt&lcGlPeriod + lnOrdAmt ,;
            nOrdAmt            WITH nOrdAmt + lnOrdAmt
    UNLOCK
    SELECT (lcOrdLine)
    =RLOCK()
    REPLACE nSteps WITH 7
    UNLOCK
  ENDIF
  *E300956,1 Update Order lines canceled quantity
  IF laScrMode[3] AND &lcOrdLine..nSteps < 8 AND SEEK(lcOrdType+Order+STR(LineNo,6),lcOrdCanLn)
    SELECT (lcOrdCanLn)
    SCATTER TO laCanRec
    INSERT INTO ORDCANLN FROM ARRAY laCanRec
    SELECT (lcOrdLine)
    =RLOCK()
    REPLACE nSteps WITH 8
    UNLOCK
  ENDIF
  *E300956,1 (End)

  *B602555,1 Check if any C/Ts have been assigned to the bulk order.
  *B602555,1 If any, Release allocation from the bulk order and allocate them
  *B602555,1 to the newly received distribution order.
  SELECT (lcOrdLine)
  IF lcOrdType='O' AND laScrMode[4] AND !EMPTY(&lcOrdLine..cFromOrder) .AND. SEEK('O'+&lcOrdLine..cFromOrder,'ORDHDR','ORDHDR') AND ;
     OrdHdr.TotCut> 0 AND &lcOrdLine..nSteps < 9
    SCATTER FIELDS ;
            Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty TO laDistQty
    *E038729,1 WAM 11/11/2004 Maintain SQL tables 
    IF llBlkAloTrn AND (SEEK('1'+&lcOrdLine..cFromOrder+STR(&lcOrdLine..BulkLineNo,6),'CUTPICK','CUTORD') OR ;
                        SEEK('2'+&lcOrdLine..cFromOrder+STR(&lcOrdLine..BulkLineNo,6),'CUTPICK','CUTORD'))
      lcTranCd = CUTPICK.TRANCD
      SELECT CUTPICK
      SCAN REST ;
           WHILE TRANCD+ORDER+CORDLINE = lcTranCd+&lcOrdLine..cFromOrder+STR(&lcOrdLine..BulkLineNo,6) ;
           AND laDistQty[1]+laDistQty[2]+laDistQty[3]+laDistQty[4]+;
               laDistQty[5]+laDistQty[6]+laDistQty[7]+laDistQty[8] >0
        m.CtktNo = CtktNo
        m.cTktLineNo = cTktLineNo
        m.Cut1 = Qty1
        m.Cut2 = Qty2
        m.Cut3 = Qty3
        m.Cut4 = Qty4
        m.Cut5 = Qty5
        m.Cut6 = Qty6
        m.Cut7 = Qty7
        m.Cut8 = Qty8
        =RLOCK()            
        REPLACE Qty1   WITH MAX(Qty1-laDistQty[1],0) ,;
                Qty2   WITH MAX(Qty2-laDistQty[2],0) ,;
                Qty3   WITH MAX(Qty3-laDistQty[3],0) ,;
                Qty4   WITH MAX(Qty4-laDistQty[4],0) ,;
                Qty5   WITH MAX(Qty5-laDistQty[5],0) ,;
                Qty6   WITH MAX(Qty6-laDistQty[6],0) ,;
                Qty7   WITH MAX(Qty7-laDistQty[7],0) ,;
                Qty8   WITH MAX(Qty8-laDistQty[8],0) ,;
                TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
        UNLOCK 
        IF TotQty = 0
          DELETE
        ENDIF
        m.Cut1 = MIN(m.Cut1,laDistQty[1])
        m.Cut2 = MIN(m.Cut2,laDistQty[2])
        m.Cut3 = MIN(m.Cut3,laDistQty[3])
        m.Cut4 = MIN(m.Cut4,laDistQty[4])
        m.Cut5 = MIN(m.Cut5,laDistQty[5])
        m.Cut6 = MIN(m.Cut6,laDistQty[6])
        m.Cut7 = MIN(m.Cut7,laDistQty[7])
        m.Cut8 = MIN(m.Cut8,laDistQty[8])
        m.TotCut = m.Cut1+m.Cut2+m.Cut3+m.Cut4+m.Cut5+m.Cut6+m.Cut7+m.Cut8
        IF m.TotCut > 0
          lnRecNo  = RECNO('CUTPICK')
          SELECT CUTPICK
          IF !SEEK(lcTranCd+m.CtktNo+m.cTktLineNo+laData[1]+&lcOrdLine..STYLE+STR(&lcOrdLine..LINENO,6),'CUTPICK','CUTPKORD')
            INSERT INTO 'CUTPICK' ;
            (CTKTNO,cTktLineNo,TRANCD,ORDER,CORDLINE,STYLE) VALUES ;
            (m.CTKTNO,m.cTktLineNo,lcTranCd,laData[1],STR(&lcOrdLine..LineNo,6),&lcOrdLine..STYLE)
          ENDIF   
          REPLACE Qty1   WITH Qty1   + m.Cut1 ,;
                  Qty2   WITH Qty2   + m.Cut2 ,;
                  Qty3   WITH Qty3   + m.Cut3 ,;
                  Qty4   WITH Qty4   + m.Cut4 ,;
                  Qty5   WITH Qty5   + m.Cut5 ,;
                  Qty6   WITH Qty6   + m.Cut6 ,;
                  Qty7   WITH Qty7   + m.Cut7 ,;
                  Qty8   WITH Qty8   + m.Cut8 ,;
                  TotQty WITH TotQty + m.TotCut
          GOTO lnRecNo
          SELECT (lcOrdLine)
          =RLOCK()
          REPLACE Cut1   WITH Cut1+m.Cut1 ,;
                  Cut2   WITH Cut2+m.Cut2 ,;
                  Cut3   WITH Cut3+m.Cut3 ,;
                  Cut4   WITH Cut4+m.Cut4 ,;
                  Cut5   WITH Cut5+m.Cut5 ,;
                  Cut6   WITH Cut6+m.Cut6 ,;
                  Cut7   WITH Cut7+m.Cut7 ,;
                  Cut8   WITH Cut8+m.Cut8 ,;
                  TotCut WITH Cut1+Cut2+Cut3+Cut4+Cut5+Cut6+Cut7+Cut8
          UNLOCK
          SELECT (lcOrdHdr)
          =RLOCK()
          REPLACE TotCut WITH TotCut + m.TotCut
          UNLOCK
          IF SEEK('O'+&lcOrdLine..cFromOrder+STR(&lcOrdLine..BulkLineNo,6),'ORDLINE')
            SELECT ORDLINE
            =RLOCK()
            REPLACE Cut1   WITH MAX(Cut1-m.Cut1,0) ,;
                    Cut2   WITH MAX(Cut2-m.Cut2,0) ,;
                    Cut3   WITH MAX(Cut3-m.Cut3,0) ,;
                    Cut4   WITH MAX(Cut4-m.Cut4,0) ,;
                    Cut5   WITH MAX(Cut5-m.Cut5,0) ,;
                    Cut6   WITH MAX(Cut6-m.Cut6,0) ,;
                    Cut7   WITH MAX(Cut7-m.Cut7,0) ,;
                    Cut8   WITH MAX(Cut8-m.Cut8,0) ,;
                    TotCut WITH Cut1+Cut2+Cut3+Cut4+Cut5+Cut6+Cut7+Cut8
            UNLOCK
            SELECT ORDHDR
            =RLOCK()
            REPLACE TotCut WITH MAX(TotCut - m.TotCut,0)
            UNLOCK
          ENDIF
          laDistQty[1] = laDistQty[1] - m.Cut1
          laDistQty[2] = laDistQty[2] - m.Cut2
          laDistQty[3] = laDistQty[3] - m.Cut3
          laDistQty[4] = laDistQty[4] - m.Cut4
          laDistQty[5] = laDistQty[5] - m.Cut5
          laDistQty[6] = laDistQty[6] - m.Cut6
          laDistQty[7] = laDistQty[7] - m.Cut7
          laDistQty[8] = laDistQty[8] - m.Cut8
        ENDIF
      ENDSCAN
    ENDIF
    SELECT (lcOrdLine)
    =RLOCK()
    REPLACE nSteps WITH 9
    UNLOCK
  ENDIF
  SCATTER MEMVAR MEMO
  m.Start    = laData[9]
  *E301305,1 AMM if complete date isn't per line or empty complete date, get it from header
  *m.Complete = laData[10]
  m.Complete = IIF(!llCDPerL .OR. EMPTY(m.Complete),laData[10],m.Complete )
  *E301305,1 AMM end
  m.Flag     = SPACE(1)
  *B802131,1 If the dates allready updated by the program ,no need to update them again.
  llUpdDates = .F.
  *B802131,1 End.
  m.Order    = laData[1]
  DO CASE
    CASE !llFromEDI AND !DELETED() .AND. !SEEK(lcOrdType+laData[1]+STR(LineNo,6),'OrdLine')
      lnLineCount=lnLineCount+1
      m.LineNo = lnLineCount
      INSERT INTO ORDLINE FROM MEMVAR
      IF SEEK(lcOrdType+laData[1],'ORDHDR')
        SELECT ORDHDR
        REPLACE LastLine WITH lnLineCount
      ENDIF
    CASE (llFromEDI .AND. SEEK('T'+Order+STR(LineNo,6),'OrdLine') ) .OR. ;
         (!DELETED() .AND. SEEK(lcOrdType+Order+STR(LineNo,6),'OrdLine'))
      m.cOrdType  = IIF(llFromEDI,'O',m.cOrdType)
      IF llFromEDI AND OrdLine.cOrdType='T'
        lnLineCount=lnLineCount+1
        m.LineNo = lnLineCount
      ENDIF  
      SELECT ORDLINE
      GATHER MEMVAR MEMO
      IF DELETED()
        RECALL
      ENDIF
    CASE DELETED() .AND. SEEK(lcOrdType+Order+STR(LineNo,6),'OrdLine')
      SELECT ORDLINE
      DELETE
    OTHERWISE  
      llcSave = .F.
      RETURN
  ENDCASE
  SELECT (lcOrdLine)
  =RLOCK()
  REPLACE Order  WITH laDAta[1] ,;
          LineNo WITH m.LineNo
  UNLOCK
ENDSCAN
*E038729,1 WAM 11/11/2004 Maintain SQL tables 
IF oAriaApplication.isRemoteComp AND llBlkAloTrn 
  lnResult = lfSqlUpdate('CUTPICK',lcTranCode, SET("DATASESSION"),'trancd,ctktno,ctktlineno,order,style,cordline')
  IF lnResult <=0
    =oAriaApplication.RemoteCompanyData.CheckRetResult("SQLUPDATE",lnResult,.T.)
    =oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
  ELSE 
    IF oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode,.T.) = 1
    ELSE
      =oAriaApplication.RemoteCompanyData.CheckRetResult("COMMITTRAN",lnResult,.T.)
    ENDIF
  ENDIF
  USE IN CUTPICK
ENDIF  
*E038729,1 WAM 11/11/2004 (End)

*B802131,1 If the date does not updated and its need to update.
IF llUpdDates
  SELECT ORDLINE
  IF SEEK(lcOrdType+laData[1])
    *E301305,1 AMM Update complete date with suitable date due to the system setting
    *REPLACE REST Start WITH laData[9],Complete WITH laData[10] ;
           WHILE cOrdType+Order=lcOrdType+laData[1]
    REPLACE REST Start WITH laData[9] , Complete WITH IIF(llCDPerL,Complete,laData[10]) ;
           WHILE cOrdType+Order=lcOrdType+laData[1]
    *E301305,1 AMM end
  ENDIF
ENDIF
*B802131,1 End.

*E301288,1 Reham On 07/20/1999   *** Begin ***
*E301288,1 Call global function to update the bomvar file.
IF llBomVarnt
  =gfTmp2Mast('BOMVAR' , lcT_BomVar , 'Update the style positions for the order lines...')
ENDIF
*E301288,1 Reham On 07/20/1999   *** End   ***

*E301077,5 Inhance openning files to speed up transaction
=gfCloseFile('icStyHst')
=gfCloseFile('ORDCANLN')
=IIF('AL' $ gcCmpModules,gfCloseFile('PikTkt'),.T.)
IF lcOrdType='O' AND laScrMode[4] AND USED('CUTPICK')
  =gfCloseFile('CUTPICK')
ENDIF
*E301077,5 (End)

SET DELETE ON
WAIT 'Updating order header...' WINDOW NOWAIT

*E301077,5 Inhance openning files to speed up transaction
=gfOpenFile(gcDataDir+'arCusHst',gcDataDir+'Acthst','SH')
*E301077,5 (End)
SELECT ORDHDR
IF llFromEDI .AND. SEEK('T'+lcEdiOrd)
  REPLACE cOrdType WITH 'O' ,;
          Order    WITH laData[1]
ELSE
  IF !SEEK(lcOrdType+laData[1])
    INSERT INTO ORDHDR (cOrdType,Order) VALUES (lcOrdType,laData[1])
  ENDIF
ENDIF
IF lcOrdType='O' .AND. laData[5]<>'B' .AND. SEEK(laData[2]+lcGlYear,'arCusHst') .AND. ;
   &lcOrdHdr..nSteps < 1
  lnOrdAmt = laData[42] &lcExRSin laData[34] &lcUntSin laData[50] - ;
  IIF(laScrMode[4] OR OrdHdr.Status='B',0,OrdHdr.OpenAmt &lcExRSin OrdHdr.nExRate &lcUntSin OrdHdr.nCurrUnit)
  SELECT arCusHst
  =RLOCK()
  REPLACE nOrdQty&lcGlPeriod WITH nOrdQty&lcGlPeriod -;
          IIF(laScrMode[4] OR OrdHdr.Status='B',0,OrdHdr.Open) + laData[41],;
          nOrdQty WITH nOrdQty -;
          IIF(laScrMode[4] OR OrdHdr.Status='B',0,OrdHdr.Open) + laData[41],;
          nOrdAmt&lcGlPeriod WITH nOrdAmt&lcGlPeriod + lnOrdAmt ,;
          nOrdAmt WITH nOrdAmt + lnOrdAmt
  UNLOCK
  SELECT (lcOrdHdr)
  =RLOCK()
  REPLACE nSteps WITH 1
  UNLOCK
ENDIF
*E500309,1 AMM If bulk order , deduct from booked else add to canceled quantity
*IF lcOrdType='O' .AND. laData[5]<>'B' .AND. (laScrMode[4] .OR. OrdHdr.Status='B') .AND. ;
*   SEEK(lcOrdType+&lcOrdHdr..cFromOrder,'OrdHdr')
*
*  IF &lcOrdHdr..nSteps < 2 .AND. SEEK(laData[2]+lcBulkYear,'arCusHst')
*    lnOrdAmt = lnCancelAmt &lcExRSin laData[34] &lcUntSin laData[50]
*    SELECT arCusHst
*    =RLOCK()
*    REPLACE nOrdQty&lcBulkPrd WITH nOrdQty&lcBulkPrd - lnCancel ,;
*            nOrdQty           WITH nOrdQty           - lnCancel ,;
*            nOrdAmt&lcBulkPrd WITH nOrdAmt&lcBulkPrd - lnOrdAmt ,;
*            nOrdAmt           WITH nOrdAmt           - lnOrdAmt
*    UNLOCK
*    SELECT (lcOrdHdr)
*    =RLOCK()
*    REPLACE nSteps WITH 2
*    UNLOCK
*  ENDIF
*  IF &lcOrdHdr..nSteps < 3
*    SELECT ORDHDR
*    =RLOCK('ORDHDR')
*    REPLACE Cancel    WITH Cancel    + lnCancel   ,;
*            Cancelamt WITH Cancelamt + lnCancelAmt,;
*            Open      WITH Book      - Cancel     ,;
*            Openamt   WITH Bookamt   - Cancelamt  ,;
*            Status    WITH IIF(OrdHdr.Open = 0 ,'X',OrdHdr.Status)
*    UNLOCK IN 'ORDHDR'
*    SELECT (lcOrdHdr)
*    =RLOCK()
*    REPLACE nSteps WITH 3
*    UNLOCK
*  ENDIF  
*  IF OrdHdr.Status='X' .AND. &lcOrdHdr..nSteps < 4 .AND. ;
*    SEEK('M'+OrdHdr.Account,'Customer')
*    SELECT Customer
*    =RLOCK('Customer')
*    REPLACE nBulk WITH nBulk - 1
*    UNLOCK IN 'Customer'
*    SELECT (lcOrdHdr)
*    =RLOCK()
*    REPLACE nSteps WITH 4
*    UNLOCK
*  ENDIF
*ENDIF

IF lcOrdType='O' .AND. laData[5]<>'B' .AND. (laScrMode[4] .OR. OrdHdr.Status='B')
  IF &lcOrdHdr..nSteps < 2 .AND. SEEK(laData[2]+lcBulkYear,'arCusHst')
    lnOrdAmt = lnCancelAmt &lcExRSin laData[34] &lcUntSin laData[50]
    SELECT arCusHst
    =RLOCK()
    REPLACE nOrdQty&lcBulkPrd WITH nOrdQty&lcBulkPrd - lnCancel ,;
            nOrdQty           WITH nOrdQty           - lnCancel ,;
            nOrdAmt&lcBulkPrd WITH nOrdAmt&lcBulkPrd - lnOrdAmt ,;
            nOrdAmt           WITH nOrdAmt           - lnOrdAmt
    UNLOCK
    SELECT (lcOrdHdr)
    =RLOCK()
    REPLACE nSteps WITH 2
    UNLOCK
  ENDIF
  IF OrdHdr.Status='X' .AND. &lcOrdHdr..nSteps < 4 .AND. ;
    SEEK('M'+laData[2],'Customer')
    SELECT Customer
    =RLOCK('Customer')
    REPLACE nBulk WITH nBulk - 1
    UNLOCK IN 'Customer'
    SELECT (lcOrdHdr)
    =RLOCK()
    REPLACE nSteps WITH 4
    UNLOCK
  ENDIF
ENDIF
*E500309,1 AMM end

*E301077,5 Inhance openning files to speed up transaction
=gfCloseFile('arCusHst')
*E301077,5 (End)

IF lcOrdType='O' AND laData[5]<>'B' AND (laScrMode[4] OR OrdHdr.Status='B') AND ;
   &lcOrdHdr..Bulk='Y' AND &lcOrdHdr..nSteps < 5 AND SEEK('M'+laData[2],'Customer')
  SELECT Customer
  =RLOCK()
  REPLACE nBulk WITH nBulk + 1
  UNLOCK
  SELECT (lcOrdHdr)
  =RLOCK()
  REPLACE nSteps WITH 5
  UNLOCK
ENDIF
SELECT ORDHDR
=SEEK(lcOrdType+laData[1])
=RLOCK()
GATHER FROM laData FIELDS &lcScFields
REPLACE cOrdType   WITH lcOrdType ,;
        LastLine   WITH lnLineCount  ,;
        cFromOrder WITH &lcOrdHdr..cFromOrder ,;
        StName     WITH IIF(laData[51],lcShipName,SPACE(30)) ,;
        cAddress1  WITH IIF(laData[51],lcShipAdd1,SPACE(30)) ,;
        cAddress2  WITH IIF(laData[51],lcShipAdd2,SPACE(30)) ,;
        cAddress3  WITH IIF(laData[51],lcShipAdd3,SPACE(30)) ,;
        cAddress4  WITH IIF(laData[51],lcShipAdd4,SPACE(30)) ,;
        cAddress5  WITH IIF(laData[51],lcShipAdd5,SPACE(30)) ,;
        TotCut     WITH &lcOrdHdr..TotCut
UNLOCK

*E301175,1 Add this line to add the capability to call this function from
*          the EDI - from EDIProcessPO Class - [Begin]

*-- If the function was not called from EDI
IF !llFromEDI
*E301175,1 Add this line to add the capability to call this function [End]
  
  SELECT unCmSess
  *B602496,1 Update the order record in the uncmsess file
  =SEEK('O'+PADR('SOORD',10)+gcUser_id+lcSession)
  *B602496,1 (End)
 
  REPLACE STATUS WITH 'C'
  UNLOCK
  llContinue = .F.
  UNLOCK 
  
*E301175,1 Add this line to add the capability to call this function from
*          the EDI - from EDIProcessPO Class - [Begin]
ENDIF    && End of IF !llFromEDI
*E301175,1 Add this line to add the capability to call this function [End]

WAIT CLEAR 
IF laScrMode[4] AND !llFromEDI
  *E300420,1 Message : 32045
  *E300420,1 Order has been saved as xxxxxx
  *E300420,1 Button : 00000 
  *E300420,1 Ok
  =gfModalGen('INM32045B00000','DIALOG',IIF(lcOrdType='C','Contract','Order')+'|'+laData[1])
  *--B602753,1 HDM 04/07/1999[Start] Stop Calling NotePad Program In lpSavScr as the global save
  *--B602753,1 HDM 04/07/1999        Will Call it
  *= IIF(gfModalGen("QRM00271B00006") = 1, NotePad('B',laData[1]), .T.)
  *--B602753,1 HDM 04/07/1999[End]
ENDIF
SET ORDER TO TAG 'ORDLINE' IN (lcOrdLine)

*E301086,1 Add new record in the EDI transaction file to be send
IF !llFromEDI AND lcOrdType='O' AND 'EB' $ gcCmpModules
  *E301077,5 Inhance openning files to speed up transaction
  =gfOpenFile(gcDataDir+'EDIACPRT',gcDataDir+'ACCFACT','SH')
  =gfOpenFile(gcDataDir+'EDIPD',gcDataDir+'PARTTRANS','SH')
  =gfOpenFile(gcDataDir+'EDITRANS',gcDataDir+'TYPEKEY','SH')
  *E301077,5 (End)

  IF SEEK('A'+laData[2],'EDIACPRT') AND SEEK(EDIACPRT.cPartCode+'855','EDIPD')
    SELECT EDITRANS
    *E037853,1 HBG 16/02/2004 Change the width of Key field in EDITRANS to 40 char [Begin]
    *IF !SEEK('855'+PADR(laData[1],20)+'A'+laData[2])
    IF !SEEK('855'+PADR(laData[1],40)+'A'+laData[2])
    *E037853,1 HBG 16/02/2004 Change the width of Key field in EDITRANS to 40 char [End]
      INSERT INTO 'EDITRANS' (cEdiTrnTyp,Key,Type,cPartner) VALUES ;
      ('855',laData[1],'A',laData[2])
    ENDIF
    REPLACE cStatus WITH 'N'
    =gfAdd_Info('EDITRANS')
  ENDIF 
  *E301077,5 Inhance openning files to speed up transaction
  =gfCloseFile('EDIACPRT')
  =gfCloseFile('EDIPD')
  =gfCloseFile('EDITRANS')
  *E301077,5 (End)
ENDIF
*E301086,1 (End)

llcSave = SEEK(lcOrdType+laData[1],'ORDLINE')
SELECT ORDHDR
RETURN

*!*************************************************************
*! Name      : lfDelScr
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Cancel/Uncancel order
*!*************************************************************
*! Calls     : gfModalGen,lfGetInfo
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfDelScr()
*!*************************************************************
*!Modifications
*!
*!B801945,1 WAM 02/09/1999 Check if allocation module is installed before
*!B801945,1                use piktkt file.
*!*************************************************************
FUNCTION lfDelScr

*E301175,1 Add this line to add a new parameter to know if the function was
*          called from EDI - from EDI transaction 860
*          (Purchase Order Modify) -. [Begin]
PARAMETERS llFromEDI
*E301175,1 Add this line to add a new parameter to know if the function [End]


*B602674,1 Add this line to fix the bug "Variable llFromEDI not found" [Begin]
llFromEDI = IIF(TYPE('llFromEDI') = 'L' , llFromEDI , .F.)
*B602674,1 Add this line to fix the bug "Variable llFromEDI not found" [End]

PRIVATE lnTktCnt

IF lcOrdType='O' .AND. !CHECKPRD(OrdHdr.Entered,'lcGlYear','lcGlPeriod ','',.T.)
  RETURN
ENDIF
lcUntSin = ''
lcExRSin = gfGetExSin(@lcUntSin, laData[33])

IF OrdHdr.Status='X'
  IF lcOrdType='O' .AND. !SEEK(lcOrdType+laData[1],'ORDLINE')
    *E300420,1 Message : 32000
    *E300420,1 The lines for this order are missing! Cannot update cut & sold.
    *E300420,1 Button : 00000 
    *E300420,1 Ok
    =gfModalGen('TRM32000B00000','ALERT')
    RETURN
  ENDIF
  SET ORDER TO TAG ORDLINE IN ORDLINE
  SET ORDER TO TAG ORDHDR  IN ORDHDR  
  IF !llContinue
    *E300420,1 Message : 32002
    *E300420,1 Order is canceled. Uncancel?
    *E300420,1 Button : 32000
    *E300420,1 Yes/No
    IF gfModalGen('QRM32002B32000','ALERT',IIF(lcOrdType='C','Contract','Order')) =2
      RETURN
    ENDIF
    IF OrdHdr.Cancel = 0 AND OrdHdr.Bulk <> 'Y'
      SELECT ORDHDR
      =RLOCK()
      REPLACE Status     WITH 'B' ,;
              cCancReson WITH SPACE(6) ,;
              Cancelled  WITH {} ,;
              Flag       WITH SPACE(1)
      UNLOCK
      SCATTER FIELDS &lcScFields TO laData
      =lfGetInfo()
      RETURN
    ENDIF  
    *E301077,5 Inhance openning files to speed up transaction
    *SELECT (lcOrdHdr)
    *DELETE
    *SELECT ORDHDR
    *=SEEK(lcOrdType+laData[1])
    *SCATTER MEMVAR
    *INSERT INTO (lcOrdHdr) FROM MEMVAR
    *SELECT (lcOrdLine)
    *DELETE ALL
    *SELECT ORDLINE
    *=SEEK(lcOrdType+laData[1])
    *SCAN REST WHILE cordtype+order+STR(lineno,6) = lcOrdType+laData[1]
    *  SCATTER MEMVAR
    *  INSERT INTO (lcOrdLine) FROM MEMVAR
    *ENDSCAN
    SELECT *, 00 AS nSteps FROM ORDHDR WHERE cordtype+order=lcOrdType+laData[1];
    INTO DBF (gcWorkDir+lcOrdHdr)
    SELECT *, 00 AS nSteps FROM ORDLINE WHERE cordtype+order+STR(lineno,6)=;
    lcOrdType+laData[1] INTO DBF (gcWorkDir+lcOrdLine)
    *E301077,5 (End)
    
    IF !llFromEDI
      SELECT 'UNCMSESS'
      IF SEEK('I')
        BLANK
      ELSE
        APPEND BLANK
      ENDIF
      REPLACE Status     WITH 'O'       ,;
              cUTranType WITH 'SOORD'   ,;
              cUserId    WITH gcUser_id ,;
              cSession   WITH lcSession ,;
              cProgram   WITH 'SOORD'   ,;
              cCurrScr   WITH 'SOORD'   ,;
              cCurrObj   WITH 'PBDLT'   ,;
              dTranDate  WITH gdSysDate ,;
              cTranTime  WITH TIME()
      =RLOCK()
      lcCurrOrd = laData[1]
      lcFiles = 'lcOrdHdr,'+lcOrdHdr+','+lcOrdHdr+';'+;
                'lcOrdLine,'+lcOrdLine+',lcOrdLine;'
      =gfSavSess('SOORD', lcFiles, @laVariables,lcSession)
    ENDIF  
  ENDIF
  IF lcOrdType='O'
    WAIT 'Uncancelling and updating Cut & Sold...' WINDOW NOWAIT
  ENDIF
  STORE 0 TO lnNewOpen,lnNewOAmt
  *E301077,5 Inhance openning files to speed up transaction
  =gfOpenFile(gcDataDir+'icStyHst',gcDataDir+'Styhst','SH') 
  *E301077,5 (End)
  SELECT (lcOrdLine)
  SCAN
    IF lcOrdType='O' .AND. nSteps < 1 
      SELECT StyDye
      =SEEK(&lcOrdLine..Style+&lcOrdLine..cWareCode+SPACE(10),'StyDye')
      =RLOCK()
      REPLACE ORD1   WITH ORD1 + &lcOrdLine..QTY1 ,;
              ORD2   WITH ORD2 + &lcOrdLine..QTY2 ,;
              ORD3   WITH ORD3 + &lcOrdLine..QTY3 ,;
              ORD4   WITH ORD4 + &lcOrdLine..QTY4 ,;
              ORD5   WITH ORD5 + &lcOrdLine..QTY5 ,;
              ORD6   WITH ORD6 + &lcOrdLine..QTY6 ,;
              ORD7   WITH ORD7 + &lcOrdLine..QTY7 ,;
              ORD8   WITH ORD8 + &lcOrdLine..QTY8 ,;
              TOTORD WITH ORD1+ORD2+ORD3+ORD4+ORD5+ORD6+ORD7+ORD8
      UNLOCK
      *E038463,1 WAM 11/09/2004 Update the Qty on the Configuration level
      IF !EMPTY(&lcOrdLine..Dyelot) AND SEEK(&lcOrdLine..Style+&lcOrdLine..cWareCode+&lcOrdLine..Dyelot)
        =RLOCK()
        REPLACE ORD1   WITH ORD1 + &lcOrdLine..QTY1 ,;
                ORD2   WITH ORD2 + &lcOrdLine..QTY2 ,;
                ORD3   WITH ORD3 + &lcOrdLine..QTY3 ,;
                ORD4   WITH ORD4 + &lcOrdLine..QTY4 ,;
                ORD5   WITH ORD5 + &lcOrdLine..QTY5 ,;
                ORD6   WITH ORD6 + &lcOrdLine..QTY6 ,;
                ORD7   WITH ORD7 + &lcOrdLine..QTY7 ,;
                ORD8   WITH ORD8 + &lcOrdLine..QTY8 ,;
                TOTORD WITH ORD1+ORD2+ORD3+ORD4+ORD5+ORD6+ORD7+ORD8 IN StyDye
      ENDIF
      *E038463,1 WAM 11/09/2004 (End)
      UNLOCK IN StyDye
      SELECT (lcOrdLine)
      =RLOCK()
      REPLACE nSteps WITH 1
      UNLOCK
    ENDIF
    IF lcOrdType='O' .AND. nSteps < 2 .AND. SEEK(STYLE,'Style')
      SELECT STYLE
      =RLOCK()
      REPLACE ORD1   WITH ORD1 + &lcOrdLine..QTY1 ,;
              ORD2   WITH ORD2 + &lcOrdLine..QTY2 ,;
              ORD3   WITH ORD3 + &lcOrdLine..QTY3 ,;
              ORD4   WITH ORD4 + &lcOrdLine..QTY4 ,;
              ORD5   WITH ORD5 + &lcOrdLine..QTY5 ,;
              ORD6   WITH ORD6 + &lcOrdLine..QTY6 ,;
              ORD7   WITH ORD7 + &lcOrdLine..QTY7 ,;
              ORD8   WITH ORD8 + &lcOrdLine..QTY8 ,;
              TOTORD WITH ORD1+ORD2+ORD3+ORD4+ORD5+ORD6+ORD7+ORD8
      UNLOCK
      SELECT (lcOrdLine)
      =RLOCK()
      REPLACE nSteps WITH 2
      UNLOCK
    ENDIF
    IF lcOrdType='O' .AND. nSteps < 3 .AND. SEEK(Style+lcGlYear,'icStyHst')
      lnOrdAmt = &lcOrdLine..TotQty*&lcOrdLine..Price &lcExRSin laData[34] &lcUntSin laData[50]
      SELECT icStyHst
      =RLOCK()
      REPLACE nOrdQty&lcGlPeriod WITH nOrdQty&lcGlPeriod + &lcOrdLine..TotQty ,;
              nOrdQty            WITH nOrdQty            + &lcOrdLine..TotQty ,;
              nOrdAmt&lcGlPeriod WITH nOrdAmt&lcGlPeriod + lnOrdAmt ,;
              nOrdAmt            WITH nOrdAmt            + lnOrdAmt
      UNLOCK
      SELECT (lcOrdLine)
      =RLOCK()
      REPLACE nSteps WITH 3
      UNLOCK
    ENDIF
    lnNewOpen = lnNewOpen+ TotQty
    lnNewOAmt = lnNewOAmt+ TotQty*Price
  ENDSCAN
  *E301077,5 Inhance openning files to speed up transaction
  =gfCloseFile('icStyHst')
  *E301077,5 (End)
  IF laData[23]='Y' .AND. &lcOrdHdr..nSteps < 1 .AND. SEEK('M'+laData[2],'Customer')
    SELECT Customer
    =RLOCK()
    REPLACE nBulk WITH nBulk + 1
    UNLOCK
    SELECT (lcOrdHdr)
    =RLOCK()
    REPLACE nSteps WITH 1
    UNLOCK
  ENDIF
  IF &lcOrdHdr..nSteps < 2
    SELECT ORDHDR
    =RLOCK()
    REPLACE Status     WITH 'O'      ,;
            cCancReson WITH SPACE(6) ,;
            Cancelled  WITH {} ,;
            Cancel     WITH IIF(BULK='Y',Cancel,Cancel-lnNewOpen) ,;
            CancelAmt  WITH IIF(BULK='Y',CancelAmt,CancelAmt-lnNewOAmt) ,;
            Open       WITH lnNewOpen ,;
            OpenAmt    WITH lnNewOAmt ,;
            Flag       WITH SPACE(1)
    UNLOCK
    SELECT (lcOrdHdr)
    =RLOCK()
    REPLACE nSteps WITH 2
    UNLOCK
  ENDIF
 
  IF lcOrdType='O' .AND. &lcOrdHdr..nSteps < 3 
    *E301077,5 Inhance openning files to speed up transaction
    =gfOpenFile(gcDataDir+'arCusHst',gcDataDir+'Acthst','SH')
    *E301077,5 (End)
    =SEEK(laData[2]+lcGlYear,'arCusHst')
    lnOrdAmt = lnNewOAmt &lcExRSin laData[34] &lcUntSin laData[50]
    SELECT arCusHst
    =RLOCK()
    REPLACE nOrdQty&lcGlPeriod WITH nOrdQty&lcGlPeriod + lnNewOpen ,;
            nOrdQty            WITH nOrdQty            + lnNewOpen ,;
            nOrdAmt&lcGlPeriod WITH nOrdAmt&lcGlPeriod + lnOrdAmt  ,;
            nOrdAmt            WITH nOrdAmt            + lnOrdAmt
    UNLOCK
    SELECT (lcOrdHdr)
    =RLOCK()
    REPLACE nSteps WITH 3
    UNLOCK
  ENDIF

ELSE
  IF ORDHDR.STATUS='C'
    *E300420,1 Message : 32003
    *E300420,1 Order has been shipped complete! Cannot cancel.
    *E300420,1 Button : 00000 
    *E300420,1 Ok
    =gfModalGen('TRM32003B00000','ALERT')
    RETURN
  ENDIF
  IF !SEEK(lcOrdType+laData[1],'ORDLINE')
    *E300420,1 Message : 32000
    *E300420,1 The lines for this order are missing! Cannot update cut & sold.
    *E300420,1 Button : 00000 
    *E300420,1 Ok
    =gfModalGen('TRM32000B00000','ALERT')
    RETURN
  ENDIF
  SET ORDER TO TAG ORDLINE IN ORDLINE
  SET ORDER TO TAG ORDHDR  IN ORDHDR  
  IF !llContinue
    IF ORDHDR.TotCut > 0
      *E300420,1 Message : 32054
      *E300420,1 This order has quantity allocated.
      *E300420,1 Canceling this order will release this allocation.
      *E300420,1 Would you like to continue ? 
      *E300420,1 Button : 32000
      *E300420,1 Yes  No
      IF gfModalGen('QRM32054B32000','ALERT')=2
        RETURN
      ENDIF
      *E301077,5 Inhance openning files to speed up transaction
      IF !oAriaApplication.isRemoteComp 
      IF 'PO' $ gcCmpModules
        =gfOpenFile(gcDataDir+'POSHDR',gcDataDir+'POSHDR','SH')
        =gfOpenFile(gcDataDir+'POSLN',gcDataDir+'POSLN','SH')
      ENDIF
      IF 'MF' $ gcCmpModules
        =gfOpenFile(gcDataDir+'CUTTKTH',gcDataDir+'CUTTKTH','SH')
        *E301182,1 Update CT/PO line number in the CUTPICK file.
        *=gfOpenFile(gcDataDir+'CUTTKTL',gcDataDir+'CUTTKTL','SH')
        =gfOpenFile(gcDataDir+'CUTTKTL',gcDataDir+'CUTLIN','SH')
        *E301182,1 (End)
      ENDIF
      =gfOpenFile(gcDataDir+'CUTPICK',gcDataDir+'CUTORD','SH')
      ENDIF
      *E301077,5 (End)
    ENDIF
    
    *B801945,1 Check if allocation module is installed.
    IF 'AL' $ gcCmpModules
      *E301077,5 Inhance openning files to speed up transaction
      =gfOpenFile(gcDataDir+'PIKTKT',gcDataDir+'ORDPIK','SH')
      *E301077,5 (End)

      IF SEEK(OrdHdr.Order,'PikTkt')
        *B602826,1 Added to check on Piktkt status for picked lines mesg.
        SELECT PikTkt
        LOCATE REST WHILE Order = OrdHdr.Order FOR Status $ 'OH'
        IF FOUND()
        *B602826,1 End.
          *E300420,1 Message : 32004
          *E300420,1 Some order lines hve been picked.
          *E300420,1 Button : 00000 
          *E300420,1 Ok
          =gfModalGen('INM32004B00000','ALERT')
        ENDIF
      ENDIF
      *E301077,5 Inhance openning files to speed up transaction
      =gfOpenFile(gcDataDir+'PIKLINE',gcDataDir+'PIKLINE','SH') 
      SET ORDER TO TAG 'PIKTKT' IN 'PIKTKT'
      *E301077,5 (End)
    ENDIF
      
    *E300420,1 Message : 32005
    *E300420,1 Cancel all open items on this order?
    *E300420,1 Button : 320000
    *E300420,1 Yes/No
    IF gfModalGen('QRM32005B32000','ALERT',IIF(lcOrdType='C','contract','order')) = 2
      RETURN
    ENDIF
    IF ORDHDR.STATUS='B'
      SELECT ORDHDR
      =RLOCK()
      REPLACE STATUS     WITH 'X' ,;
              cCancReson WITH lfCanReason() ,;
              Cancelled  WITH gdSysDate ,;
              Flag       WITH SPACE(1)
      UNLOCK
      SCATTER FIELDS &lcScFields TO laData
      =lfGetInfo()
      RETURN
    ENDIF
    lnTktCnt = 0
    DIMENSION laTickets[1,2]
    *E301077,5 Inhance openning files to speed up transaction
    *IF 'PO' $ gcCmpModules
    *  SET ORDER TO TAG POSLN IN POSLN
    *ENDIF
    *IF 'MF' $ gcCmpModules
    *  SET ORDER TO TAG CUTTKTL IN CUTTKTL
    *ENDIF  
    *SELECT (lcOrdHdr)
    *DELETE
    *SELECT ORDHDR
    *=SEEK(lcOrdType+laData[1])
    *SCATTER MEMVAR
    *m.cCancReson = lfCanReason()
    *INSERT INTO (lcOrdHdr) FROM MEMVAR
    *SELECT (lcOrdLine)
    *DELETE ALL
    *SELECT ORDLINE
    *=SEEK(lcOrdType+laData[1])
    *SCAN REST WHILE cordtype+order+STR(lineno,6) = lcOrdType+laData[1]
    *  SCATTER MEMVAR
    *  INSERT INTO (lcOrdLine) FROM MEMVAR
    *ENDSCAN

    SELECT *, 00 AS nSteps FROM ORDHDR WHERE cordtype+order=lcOrdType+laData[1];
    INTO DBF (gcWorkDir+lcOrdHdr)
    m.cCancReson = lfCanReason()
    REPLACE cCancReson WITH m.cCancReson
    SELECT *, 00 AS nSteps FROM ORDLINE WHERE cordtype+order+STR(lineno,6)=;
    lcOrdType+laData[1] INTO DBF (gcWorkDir+lcOrdLine)
    *E301077,5 (End)
    
    IF !llFromEDI
      SELECT 'UNCMSESS'
      IF SEEK('I')
        BLANK
      ELSE
        APPEND BLANK
      ENDIF
      REPLACE Status     WITH 'O'       ,;
              cUTranType WITH 'SOORD'   ,;
              cUserId    WITH gcUser_id ,;
              cSession   WITH lcSession ,;
              cProgram   WITH 'SOORD'   ,;
              cCurrScr   WITH 'SOORD'   ,;
              cCurrObj   WITH 'PBDLT'   ,;
              dTranDate  WITH gdSysDate ,;
              cTranTime  WITH TIME()
      =RLOCK()
      lcCurrOrd = laData[1]
      lcFiles = 'lcOrdHdr,'+lcOrdHdr+','+lcOrdHdr+';'+;
                'lcOrdLine,'+lcOrdLine+',lcOrdLine;'
      =gfSavSess('SOORD', lcFiles, @laVariables,lcSession)
    ENDIF
  ENDIF
  WAIT 'Cancelling and updating Cut & Sold...' WINDOW NOWAIT 
  *E301077,5 Inhance openning files to speed up transaction
  =gfOpenFile(gcDataDir+'icStyHst',gcDataDir+'Styhst','SH') 
  *E301077,5 (End)
  SELECT (lcOrdLine)
  STORE 0 TO lnOpen,lnOpenAmt,lnBook,lnBookAmt
  SCAN
    IF lcOrdType='O' .AND. &lcOrdLine..nSteps < 1
      IF SEEK(&lcOrdLine..Style+&lcOrdLine..cWareCode+SPACE(10),'StyDye')
        SELECT StyDye
        =RLOCK()
        REPLACE Ord1   WITH MAX(Ord1-&lcOrdLine..Qty1,0) ,;
                Ord2   WITH MAX(Ord2-&lcOrdLine..Qty2,0) ,;
                Ord3   WITH MAX(Ord3-&lcOrdLine..Qty3,0) ,;
                Ord4   WITH MAX(Ord4-&lcOrdLine..Qty4,0) ,;
                Ord5   WITH MAX(Ord5-&lcOrdLine..Qty5,0) ,;
                Ord6   WITH MAX(Ord6-&lcOrdLine..Qty6,0) ,;
                Ord7   WITH MAX(Ord7-&lcOrdLine..Qty7,0) ,;
                Ord8   WITH MAX(Ord8-&lcOrdLine..Qty8,0) ,;
                TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8
        IF &lcOrdLine..PICKED 
          REPLACE Alo1   WITH MAX(Alo1-&lcOrdLine..Pik1,0) ,;
                  Alo2   WITH MAX(Alo2-&lcOrdLine..Pik2,0) ,;
                  Alo3   WITH MAX(Alo3-&lcOrdLine..Pik3,0) ,;
                  Alo4   WITH MAX(Alo4-&lcOrdLine..Pik4,0) ,;
                  Alo5   WITH MAX(Alo5-&lcOrdLine..Pik5,0) ,;
                  Alo6   WITH MAX(Alo6-&lcOrdLine..Pik6,0) ,;
                  Alo7   WITH MAX(Alo7-&lcOrdLine..Pik7,0) ,;
                  Alo8   WITH MAX(Alo8-&lcOrdLine..Pik8,0) ,;
                  TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8
        ENDIF
        UNLOCK
        SELECT (lcOrdLine)
        =RLOCK()
        REPLACE nSteps WITH 1
        UNLOCK
      ENDIF
    ENDIF
    *E038463,1 WAM 11/09/2004 Update ordered quantity at the Configuration level
    IF lcOrdType='O' .AND. &lcOrdLine..nSteps < 2
      IF !EMPTY(&lcOrdLine..Dyelot) AND SEEK(&lcOrdLine..Style+&lcOrdLine..cWareCode+&lcOrdLine..Dyelot,'StyDye')
        SELECT StyDye
        =RLOCK()
        REPLACE Ord1   WITH MAX(Ord1-&lcOrdLine..Qty1,0) ,;
                Ord2   WITH MAX(Ord2-&lcOrdLine..Qty2,0) ,;
                Ord3   WITH MAX(Ord3-&lcOrdLine..Qty3,0) ,;
                Ord4   WITH MAX(Ord4-&lcOrdLine..Qty4,0) ,;
                Ord5   WITH MAX(Ord5-&lcOrdLine..Qty5,0) ,;
                Ord6   WITH MAX(Ord6-&lcOrdLine..Qty6,0) ,;
                Ord7   WITH MAX(Ord7-&lcOrdLine..Qty7,0) ,;
                Ord8   WITH MAX(Ord8-&lcOrdLine..Qty8,0) ,;
                TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8
        IF &lcOrdLine..PICKED
          REPLACE Alo1   WITH MAX(Alo1-&lcOrdLine..Pik1,0) ,;
                  Alo2   WITH MAX(Alo2-&lcOrdLine..Pik2,0) ,;
                  Alo3   WITH MAX(Alo3-&lcOrdLine..Pik3,0) ,;
                  Alo4   WITH MAX(Alo4-&lcOrdLine..Pik4,0) ,;
                  Alo5   WITH MAX(Alo5-&lcOrdLine..Pik5,0) ,;
                  Alo6   WITH MAX(Alo6-&lcOrdLine..Pik6,0) ,;
                  Alo7   WITH MAX(Alo7-&lcOrdLine..Pik7,0) ,;
                  Alo8   WITH MAX(Alo8-&lcOrdLine..Pik8,0) ,;
                  TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8
        ENDIF
        UNLOCK
        SELECT (lcOrdLine)
        =RLOCK()
        REPLACE nSteps WITH 2
        UNLOCK
      ENDIF
    ENDIF
    *E038463,1 WAM 11/09/2004 (End)
    
    IF lcOrdType='O' .AND. SEEK(&lcOrdLine..STYLE,'Style')
      IF &lcOrdLine..nSteps < 3
        SELECT Style
        =RLOCK()
        REPLACE Ord1   WITH MAX(Ord1-&lcOrdLine..Qty1,0) ,;
                Ord2   WITH MAX(Ord2-&lcOrdLine..Qty2,0) ,;
                Ord3   WITH MAX(Ord3-&lcOrdLine..Qty3,0) ,;
                Ord4   WITH MAX(Ord4-&lcOrdLine..Qty4,0) ,;
                Ord5   WITH MAX(Ord5-&lcOrdLine..Qty5,0) ,;
                Ord6   WITH MAX(Ord6-&lcOrdLine..Qty6,0) ,;
                Ord7   WITH MAX(Ord7-&lcOrdLine..Qty7,0) ,;
                Ord8   WITH MAX(Ord8-&lcOrdLine..Qty8,0) ,;
                TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8
        UNLOCK
        SELECT (lcOrdLine)
        =RLOCK()
        REPLACE nSteps WITH 3
        UNLOCK
      ENDIF
      IF &lcOrdLine..PICKED .AND. &lcOrdLine..nSteps < 4
        SELECT Style
        =RLOCK()
        REPLACE Alo1   WITH MAX(Alo1-&lcOrdLine..Pik1,0) ,;
                Alo2   WITH MAX(Alo2-&lcOrdLine..Pik2,0) ,;
                Alo3   WITH MAX(Alo3-&lcOrdLine..Pik3,0) ,;
                Alo4   WITH MAX(Alo4-&lcOrdLine..Pik4,0) ,;
                Alo5   WITH MAX(Alo5-&lcOrdLine..Pik5,0) ,;
                Alo6   WITH MAX(Alo6-&lcOrdLine..Pik6,0) ,;
                Alo7   WITH MAX(Alo7-&lcOrdLine..Pik7,0) ,;
                Alo8   WITH MAX(Alo8-&lcOrdLine..Pik8,0) ,;
                TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8
        UNLOCK
        SELECT (lcOrdLine)
        =RLOCK()
        REPLACE nSteps WITH 4
        UNLOCK
      ENDIF
    ENDIF
    *B602418,1 Update dyelot allocated quantity
    IF lcOrdType='O' AND &lcOrdLine..nSteps < 5 AND &lcOrdLine..PICKED AND !EMPTY(&lcOrdLine..Dyelot) AND ;
       SEEK(&lcOrdLine..Style+&lcOrdLine..cWareCode+&lcOrdLine..Dyelot,'StyDye')
      SELECT StyDye
      =RLOCK()
      REPLACE Alo1   WITH MAX(Alo1-&lcOrdLine..Pik1,0) ,;
              Alo2   WITH MAX(Alo2-&lcOrdLine..Pik2,0) ,;
              Alo3   WITH MAX(Alo3-&lcOrdLine..Pik3,0) ,;
              Alo4   WITH MAX(Alo4-&lcOrdLine..Pik4,0) ,;
              Alo5   WITH MAX(Alo5-&lcOrdLine..Pik5,0) ,;
              Alo6   WITH MAX(Alo6-&lcOrdLine..Pik6,0) ,;
              Alo7   WITH MAX(Alo7-&lcOrdLine..Pik7,0) ,;
              Alo8   WITH MAX(Alo8-&lcOrdLine..Pik8,0) ,;
              TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8
      UNLOCK
      SELECT (lcOrdLine)
      =RLOCK()
      REPLACE nSteps WITH 5
      UNLOCK
    ENDIF
    *B602418,1 (End)

    IF lcOrdType='O' .AND. &lcOrdLine..nSteps < 6 .AND. SEEK(Style+lcGlYear,'icStyHst')
      lnOrdAmt = &lcOrdLine..TotQty*&lcOrdLine..Price &lcExRSin laData[34] &lcUntSin laData[50]
      SELECT icStyHst
      =RLOCK()
      REPLACE nOrdQty&lcGlPeriod WITH nOrdQty&lcGlPeriod - &lcOrdLine..TotQty ,;
              nOrdQty            WITH nOrdQty            - &lcOrdLine..TotQty ,;
              nOrdAmt&lcGlPeriod WITH nOrdAmt&lcGlPeriod - lnOrdAmt ,;
              nOrdAmt            WITH nOrdAmt            - lnOrdAmt
      UNLOCK
      SELECT (lcOrdLine)
      =RLOCK()
      REPLACE nSteps WITH 6
      UNLOCK
    ENDIF
    =SEEK(lcOrdType+&lcOrdLine..ORDER+STR(&lcOrdLine..LINENO,6),'ORDLINE')
    IF &lcOrdLine..Picked
      IF SEEK(&lcOrdLine..PikTkt,'PIKTKT')
        SELECT PIKTKT
        =RLOCK()
        REPLACE STATUS WITH 'X'
        UNLOCK
      ENDIF
      SELECT ORDLINE
      IF !SEEK(PikTkt+Order+STR(LineNo,6),'PIKLINE')      
        SCATTER TO laPikLine
        INSERT INTO PIKLINE FROM ARRAY laPikLine
      ENDIF
      SELECT ORDLINE
      =RLOCK()
      REPLACE PIK1   WITH 0 ,;
              PIK2   WITH 0 ,;
              PIK3   WITH 0 ,;
              PIK4   WITH 0 ,;
              PIK5   WITH 0 ,;
              PIK6   WITH 0 ,;
              PIK7   WITH 0 ,;
              PIK8   WITH 0 ,;
              TOTPIK WITH 0 ,;
              PICKED WITH .F. ,;
              PIKTKT WITH SPACE(6) ,;
              PIKDATE WITH {}
      IF SEEK(AltStyle,'STYLE') .AND. SEEK('S'+STYLE.Scale,'SCALE')
        REPLACE Style     WITH AltStyle,;
                AltStyle  WITH SPACE(19),;
                Scale     WITH SCALE.Scale,;
                cWareCode WITH OrdHdr.cWareCode
        REPLACE Qty1  WITH IIF(Scale.Cnt>=1,Qty1,0)  ,;
                Book1 WITH IIF(Scale.Cnt>=1,Book1,0) ,;
                Qty2  WITH IIF(Scale.Cnt>=2,Qty2,0)  ,;
                Book2 WITH IIF(Scale.Cnt>=2,Book2,0) ,;
                Qty3  WITH IIF(Scale.Cnt>=3,Qty3,0)  ,;
                Book3 WITH IIF(Scale.Cnt>=3,Book3,0) ,;
                Qty4  WITH IIF(Scale.Cnt>=4,Qty4,0)  ,;
                Book4 WITH IIF(Scale.Cnt>=4,Book4,0) ,;
                Qty5  WITH IIF(Scale.Cnt>=5,Qty5,0)  ,;
                Book5 WITH IIF(Scale.Cnt>=5,Book5,0) ,;
                Qty6  WITH IIF(Scale.Cnt>=6,Qty6,0)  ,;
                Book6 WITH IIF(Scale.Cnt>=6,Book6,0) ,;
                Qty7  WITH IIF(Scale.Cnt>=7,Qty7,0)  ,;
                Book7 WITH IIF(Scale.Cnt>=7,Book7,0) ,;
                Qty8  WITH IIF(Scale.Cnt>=8,Qty8,0)  ,;
                Book8 WITH IIF(Scale.Cnt>=8,Book8,0) ,;
                TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8 ,;
                TotBook WITH Book1+Book2+Book3+Book4+Book5+Book6+Book7+Book8
      ENDIF
      UNLOCK
    ENDIF
    IF ORDHDR.TotCut > 0 AND !oAriaApplication.isRemoteComp
      SELECT CutPick
      DO WHILE SEEK(IIF(Style.Make,'1','2')+&lcOrdLine..Order+STR(&lcOrdLine..LineNo,6))
        SELECT IIF(CutPick.TranCd='1','CutTktH','PosHdr')
        =SEEK(IIF(CutPick.TranCd='1','','P')+CutPick.CTKTNO)
        =RLOCK()
        REPLACE TotOrd WITH MAX(TotOrd-CUTPICK.TotQty,0)
        UNLOCK
        SELECT IIF(CutPick.TranCd='1','CutTktL','PosLn')
        *E301182,1 Update CT/PO line number in the CUTPICK file.
        *=SEEK(IIF(CutPick.TranCd='1',CutPick.CTKTNO+CutPick.Style,;
                  'P'+CutPick.CTKTNO+CutPick.Style))
        =SEEK(IIF(CutPick.TranCd='2','P','')+;
                  CutPick.CTKTNO+CutPick.Style+CutPick.cTktLineNo+'1')
        *E301182,1 (End)
        =RLOCK()
        REPLACE ORD1   WITH MAX(ORD1-CUTPICK.Qty1,0) ,;
                ORD2   WITH MAX(ORD2-CUTPICK.Qty2,0) ,;
                ORD3   WITH MAX(ORD3-CUTPICK.Qty3,0) ,;
                ORD4   WITH MAX(ORD4-CUTPICK.Qty4,0) ,;
                ORD5   WITH MAX(ORD5-CUTPICK.Qty5,0) ,;
                ORD6   WITH MAX(ORD6-CUTPICK.Qty6,0) ,;
                ORD7   WITH MAX(ORD7-CUTPICK.Qty7,0) ,;
                ORD8   WITH MAX(ORD8-CUTPICK.Qty8,0) ,;
                TOTORD WITH MAX(TOTORD-CUTPICK.TotQty,0)
        UNLOCK
        *E300420,1 Message : 32006
        *E300420,1 Cuttkt# xxxxxx status is HOLD. Do you wish to update the generated 
        *E300420,1 cuttkt quantities accordingly?
        *E300420,1 Button : 32000 
        *E300420,1 Yes/No

        *E300420,1 Message : 32007
        *E300420,1 Purchase order# xxxxxx status is HOLD. Do you wish to update the generated 
        *E300420,1 Purchase order quantities accordingly?
        *E300420,1 Button : 32000 
        *E300420,1 Yes/No
  
        *E300420,1 Message : 32008
        *E300420,1 Do you wish to update Purchase order# 999999 quantities accordingly?
        *E300420,1 Button : 32000 
        *E300420,1 Yes/No
        lnTicket=ASCAN(laTickets,CutPick.CTKTNO)
        IF lnTicket = 0
          *llUpdate= (CutPick.TranCd='1' .AND. CUTTKTH.Status='H' .AND. ;
            gfModalGen('QRM32006B32000','ALERT',CutPick.CTKTNO)=1) .OR. ;
            (CutPick.TranCd='2' .AND. ( ( POSHDR.Status='H' .AND. ;
             gfModalGen('QRM32007B32000','ALERT',CutPick.CTKTNO)=1) .OR. ;
            ( POSHDR.Status $ 'OA' .AND. !laSetups[13,2] .AND. ;
              gfModalGen('QRM32008B32000','ALERT',CutPick.CTKTNO)=1) ))
          llUpdate= (CutPick.TranCd='1' .AND. CUTTKTH.Status='H' .AND. ;
            gfModalGen('QRM32006B32000','ALERT',CutPick.CTKTNO)=1) .OR. ;
                    (CutPick.TranCd='2' .AND. POSHDR.Status='H'  .AND. ;
            gfModalGen('QRM32007B32000','ALERT',CutPick.CTKTNO)=1)

          lnTktCnt=lnTktCnt+1
          DIMENSION laTickets[lnTktCnt,2]
          laTickets[lnTktCnt,1]=CutPick.CTKTNO
          laTickets[lnTktCnt,2]=IIF(llUpdate,'Y','N')
        ELSE
          llUpdate = (laTickets[lnTicket+1]='Y')
        ENDIF
        IF llUpdate
          SELECT IIF(CutPick.TranCd='1','CutTktH','PosHdr')
          =RLOCK()
          IF CutPick.TranCd = '1'
            REPLACE Pcs_Bud WITH Pcs_Bud - CUTPICK.TotQty ,;
                    Pcs_Opn WITH Pcs_Opn - CUTPICK.TotQty
          ELSE
            REPLACE nStyOrder WITH nStyOrder - CUTPICK.TotQty ,;
                    Open      WITH Open  - CUTPICK.TotQty
          ENDIF
          UNLOCK
          SELECT IIF(CutPick.TranCd='1','CutTktL','PosLn')
          =RLOCK()
          REPLACE Qty1   WITH Qty1-CUTPICK.Qty1 ,;
                  Qty2   WITH Qty2-CUTPICK.Qty2 ,;
                  Qty3   WITH Qty3-CUTPICK.Qty3 ,;
                  Qty4   WITH Qty4-CUTPICK.Qty4 ,;
                  Qty5   WITH Qty5-CUTPICK.Qty5 ,;
                  Qty6   WITH Qty6-CUTPICK.Qty6 ,;
                  Qty7   WITH Qty7-CUTPICK.Qty7 ,;
                  Qty8   WITH Qty8-CUTPICK.Qty8 ,;
                  TotQty WITH TotQty - CUTPICK.TotQty
          UNLOCK
        ENDIF
        SELECT CUTPICK
        DELETE
      ENDDO
    ENDIF
    SELECT ORDLINE
    =RLOCK()
    REPLACE Cut1   WITH 0 ,;
            Cut2   WITH 0 ,;
            Cut3   WITH 0 ,;
            Cut4   WITH 0 ,;
            Cut5   WITH 0 ,;
            Cut6   WITH 0 ,;
            Cut7   WITH 0 ,;
            Cut8   WITH 0 ,;
            TotCut WITH 0
    UNLOCK
    lnOpen    = lnOpen    + OrdLine.TotQty
    lnOpenAmt = lnOpenAmt + OrdLine.TotQty*OrdLine.Price
    lnBook    = lnBook    + OrdLine.TotBook
    lnBookAmt = lnBookAmt + OrdLine.TotBook*OrdLine.Price
  ENDSCAN
  *E038729,1 WAM 11/15/2004  Release order allocated quantity to any cutting ticket order style purchae order
  llUpdate = .F.
  IF oAriaApplication.isRemoteComp AND ORDHDR.TotCut > 0
    lcSetClass = SET('CLASSLIB')
    SET CLASSLIB TO (oAriaApplication.lcAria4Class+"MAIN.VCX"),(oAriaApplication.lcAria4Class+"UTILITY.VCX")
    lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr,3,'',.T.)
    SET CLASSLIB TO &lcSetClass.
    IF TYPE('lcTranCode') = 'N'
      =oAriaApplication.RemoteCompanyData.CheckRetResult("BEGINTRAN",lcTranCode,.T.)
    ELSE 
      lcSelString =               "SELECT CutPick.*,PosHdr.Status FROM CutPick INNER JOIN PosHdr On PosHdr.cBusDocu+PosHdr.cStyType+PosHdr.Po = 'PU'+CutPick.cTktNo WHERE CutPick.TranCd+CutPick.[Order]+CutPick.cOrdLine LIKE '1" + OrdHdr.Order + "%' UNION "
      lcSelString = lcSelString + "SELECT CutPick.*,PosHdr.Status FROM CutPick INNER JOIN PosHdr ON PosHdr.cBusDocu+PosHdr.cStyType+PosHdr.Po = 'PP'+CutPick.cTktNo WHERE CutPick.TranCd+CutPick.[Order]+CutPick.cOrdLine LIKE '2" + OrdHdr.Order + "%' ORDER BY trancd,ctktno,cTKtLineNo"
      IF oAriaApplication.RemoteCompanyData.Execute(lcSelString,'',"CutPick","CutPick ",lcTranCode,4,'',SET("DATASESSION")) = 1
        SELECT CutPick
        GO TOP
        DO WHILE !EOF()
          lcTranCd = TranCd
          lcTktNo  = cTktNo
          lnTotOrd = 0 
          *-- Message : 32006
          *-- Cuttkt# xxxxxx status is HOLD. Do you wish to update the generated 
          *-- cuttkt quantities accordingly?
          *-- Button : 32000 
          *-- Yes/No
          *-- Message : 32007
          *-- Purchase order# xxxxxx status is HOLD. Do you wish to update the generated 
          *-- Purchase order quantities accordingly?
          *-- Button : 32000 
          *-- Yes/No
          llUpdate= Status='H' .AND. ((TranCd='1' .AND. gfModalGen('QRM32006B32000','ALERT',CTKTNO)=1) .OR. ;
                                      (TranCd='2' .AND. gfModalGen('QRM32007B32000','ALERT',CTKTNO)=1) )
        
          SCAN REST WHILE trancd+ctktno+cTKtLineNo = lcTranCd+lcTktNo
          
            =SEEK('O'+Order+cOrdLine,'OrdLine','OrdLine')
        
            *-- Update ticket lines Ordered quantity
            lcSelString="UPDATE PosLn SET Ord1=Ord1-"+ALLTRIM(STR(CutPick.Qty1))+",Ord2=Ord2-"+ALLTRIM(STR(CutPick.Qty2))+;
            ",Ord3=Ord3-"+ALLTRIM(STR(CutPick.Qty3))+",Ord4=Ord4-"+ALLTRIM(STR(CutPick.Qty4))+;
            ",Ord5=Ord5-"+ALLTRIM(STR(CutPick.Qty5))+",Ord6=Ord6-"+ALLTRIM(STR(CutPick.Qty6))+;
            ",Ord7=Ord7-"+ALLTRIM(STR(CutPick.Qty7))+",Ord8=Ord8-"+ALLTRIM(STR(CutPick.Qty8))+;
            ",TotOrd=TotOrd-"+ALLTRIM(STR(CutPick.TotQty))
            *-- Update ticket lines budget quantities
            IF llUpdate
              lcSelString=lcSelString+;
              ",Qty1=Qty1-"+ALLTRIM(STR(CUTPICK.Qty1))+",Qty2=Qty2-"+ALLTRIM(STR(CUTPICK.Qty2))+;
              ",Qty3=Qty3-"+ALLTRIM(STR(CUTPICK.Qty3))+",Qty4=Qty4-"+ALLTRIM(STR(CUTPICK.Qty4))+;
              ",Qty5=Qty5-"+ALLTRIM(STR(CUTPICK.Qty5))+",Qty6=Qty6-"+ALLTRIM(STR(CUTPICK.Qty6))+;
              ",Qty7=Qty7-"+ALLTRIM(STR(CUTPICK.Qty7))+",Qty8=Qty8-"+ALLTRIM(STR(CUTPICK.Qty8))+;
              ",TotQty=TotQty-"+ALLTRIM(STR(CUTPICK.TotQty))
            ENDIF
            IF lcTranCd = '1'
              lcSelString=lcSelString+" WHERE cBusDocu+cstytype+po+cInvType+style+STR([lineno],6)+trancd='PU"+CUTPICK.CTKTNO+'0001'+CUTPICK.Style+CUTPICK.cTktLineNo+"1'"
            ELSE
              lcSelString=lcSelString+" WHERE cBusDocu+cstytype+po+cInvType+style+STR([lineno],6)+trancd='PP"+CUTPICK.CTKTNO+'0001'+CUTPICK.Style+CUTPICK.cTktLineNo+"1'"
            ENDIF
            lnResult = oAriaApplication.RemoteCompanyData.Execute(lcSelString,'',"SAVEFILE","",lcTranCode,4,'',SET("DATASESSION"))
          
            IF SEEK(OrdLine.Style+OrdLine.cWareCode+SPACE(10),'StyDye')
              =RLOCK("StyDye")
              REPLACE WIP1   WITH MAX(WIP1-OrdLine.Qty1,0) ,;
                      WIP2   WITH MAX(WIP2-OrdLine.Qty2,0) ,;
                      WIP3   WITH MAX(WIP3-OrdLine.Qty3,0) ,;
                      WIP4   WITH MAX(WIP4-OrdLine.Qty4,0) ,;
                      WIP5   WITH MAX(WIP5-OrdLine.Qty5,0) ,;
                      WIP6   WITH MAX(WIP6-OrdLine.Qty6,0) ,;
                      WIP7   WITH MAX(WIP7-OrdLine.Qty7,0) ,;
                      WIP8   WITH MAX(WIP8-OrdLine.Qty8,0) ,;
                      TotWIP WITH WIP1+WIP2+WIP3+WIP4+WIP5+WIP6+WIP7+WIP8 IN StyDye
              REPLACE NWO1   WITH MAX(NWO1-OrdLine.Qty1,0) ,;
                      NWO2   WITH MAX(NWO2-OrdLine.Qty2,0) ,;
                      NWO3   WITH MAX(NWO3-OrdLine.Qty3,0) ,;
                      NWO4   WITH MAX(NWO4-OrdLine.Qty4,0) ,;
                      NWO5   WITH MAX(NWO5-OrdLine.Qty5,0) ,;
                      NWO6   WITH MAX(NWO6-OrdLine.Qty6,0) ,;
                      NWO7   WITH MAX(NWO7-OrdLine.Qty7,0) ,;
                      NWO8   WITH MAX(NWO8-OrdLine.Qty8,0) ,;
                      NTotWO WITH NWO1+NWO2+NWO3+NWO4+NWO5+NWO6+NWO7+NWO8 IN StyDye
              UNLOCK IN StyDye
            ENDIF
            IF !EMPTY(OrdLine.Dyelot) AND SEEK(OrdLine.Style+OrdLine.cWareCode+OrdLine.Dyelot,'StyDye')
              =RLOCK("StyDye")
              REPLACE WIP1   WITH MAX(WIP1-OrdLine.Qty1,0) ,;
                      WIP2   WITH MAX(WIP2-OrdLine.Qty2,0) ,;
                      WIP3   WITH MAX(WIP3-OrdLine.Qty3,0) ,;
                      WIP4   WITH MAX(WIP4-OrdLine.Qty4,0) ,;
                      WIP5   WITH MAX(WIP5-OrdLine.Qty5,0) ,;
                      WIP6   WITH MAX(WIP6-OrdLine.Qty6,0) ,;
                      WIP7   WITH MAX(WIP7-OrdLine.Qty7,0) ,;
                      WIP8   WITH MAX(WIP8-OrdLine.Qty8,0) ,;
                      TotWIP WITH WIP1+WIP2+WIP3+WIP4+WIP5+WIP6+WIP7+WIP8 IN StyDye
              REPLACE NWO1   WITH MAX(NWO1-OrdLine.Qty1,0) ,;
                      NWO2   WITH MAX(NWO2-OrdLine.Qty2,0) ,;
                      NWO3   WITH MAX(NWO3-OrdLine.Qty3,0) ,;
                      NWO4   WITH MAX(NWO4-OrdLine.Qty4,0) ,;
                      NWO5   WITH MAX(NWO5-OrdLine.Qty5,0) ,;
                      NWO6   WITH MAX(NWO6-OrdLine.Qty6,0) ,;
                      NWO7   WITH MAX(NWO7-OrdLine.Qty7,0) ,;
                      NWO8   WITH MAX(NWO8-OrdLine.Qty8,0) ,;
                      NTotWO WITH NWO1+NWO2+NWO3+NWO4+NWO5+NWO6+NWO7+NWO8 IN StyDye
              UNLOCK IN StyDye
           ENDIF
            *-- Decrease style ordered quantity with order open quantity
            IF SEEK(OrdLine.STYLE,'Style')
              =RLOCK("Style")
              REPLACE WIP1   WITH MAX(WIP1-OrdLine.Qty1,0) ,;
                      WIP2   WITH MAX(WIP2-OrdLine.Qty2,0) ,;
                      WIP3   WITH MAX(WIP3-OrdLine.Qty3,0) ,;
                      WIP4   WITH MAX(WIP4-OrdLine.Qty4,0) ,;
                      WIP5   WITH MAX(WIP5-OrdLine.Qty5,0) ,;
                      WIP6   WITH MAX(WIP6-OrdLine.Qty6,0) ,;
                      WIP7   WITH MAX(WIP7-OrdLine.Qty7,0) ,;
                      WIP8   WITH MAX(WIP8-OrdLine.Qty8,0) ,;
                      TotWIP WITH WIP1+WIP2+WIP3+WIP4+WIP5+WIP6+WIP7+WIP8 IN Style
              REPLACE NWO1   WITH MAX(NWO1-OrdLine.Qty1,0) ,;
                      NWO2   WITH MAX(NWO2-OrdLine.Qty2,0) ,;
                      NWO3   WITH MAX(NWO3-OrdLine.Qty3,0) ,;
                      NWO4   WITH MAX(NWO4-OrdLine.Qty4,0) ,;
                      NWO5   WITH MAX(NWO5-OrdLine.Qty5,0) ,;
                      NWO6   WITH MAX(NWO6-OrdLine.Qty6,0) ,;
                      NWO7   WITH MAX(NWO7-OrdLine.Qty7,0) ,;
                      NWO8   WITH MAX(NWO8-OrdLine.Qty8,0) ,;
                      NTotWO WITH NWO1+NWO2+NWO3+NWO4+NWO5+NWO6+NWO7+NWO8 IN Style
              UNLOCK IN Style
            ENDIF
            lnTotOrd = lnTotOrd + CUTPICK.TotQty
          ENDSCAN
          *-- Update ticket total Ordered & budget quantity
          IF lcTranCd = '1'
            *-- Change Cutting quantity if order allocated quantity has been changed
            lcSelString = "UPDATE PosHdr SET TotOrd=TotOrd-"+ALLTRIM(STR(lnTotOrd))+;
                          IIF(llUpdate,",nStyOrder = nStyOrder-"+ALLTRIM(STR(lnTotOrd))+",[Open]=[Open]-"+ALLTRIM(STR(lnTotOrd)),'')+;
                          " WHERE cBusDocu+cstytype+po='PU"+lcTktNo+"'"
          ELSE
            *-- Change Purchased quantity if order allocated quantity has been changed
            lcSelString = "UPDATE PosHdr SET TotOrd=TotOrd-"+ALLTRIM(STR(lnTotOrd))+;
                          IIF(llUpdate,",nStyOrder = nStyOrder-"+ALLTRIM(STR(lnTotOrd))+",[Open]=[Open]-"+ALLTRIM(STR(lnTotOrd)),'')+;
                          " WHERE cBusDocu+cstytype+po='PP"+lcTktNo+"'"
          ENDIF
          lnResult = oAriaApplication.RemoteCompanyData.Execute(lcSelString,'',"SAVEFILE","",lcTranCode,4,'',SET("DATASESSION"))
          SELECT CUTPICK
        ENDDO
        SELECT CUTPICK
        DELETE ALL
        lnResult = lfSqlUpdate('CUTPICK',lcTranCode, SET("DATASESSION"),'trancd,ctktno,ctktlineno,order,style,cordline')
        IF lnResult <=0
          =oAriaApplication.RemoteCompanyData.CheckRetResult("SQLUPDATE",lnResult,.T.)
          =oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
        ELSE 
          IF oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode,.T.) = 1
          ELSE
            =oAriaApplication.RemoteCompanyData.CheckRetResult("COMMITTRAN",lnResult,.T.)
          ENDIF
        ENDIF
        USE IN CUTPICK
      ENDIF
    ENDIF
  ENDIF
  *E038729,1 WAM 11/15/2004 (End)
  
  *E301077,5 Inhance openning files to speed up transaction
  =gfCloseFile('icStyHst')
  *E301077,5 (End)
  IF laData[23]='Y' .AND. &lcOrdHdr..nSteps < 1 .AND. SEEK('M'+laData[2],'Customer')
    SELECT CUSTOMER
    =RLOCK()
    REPLACE nBulk WITH nBulk - 1
    UNLOCK
    SELECT (lcOrdHdr)
    =RLOCK()
    REPLACE nSteps WITH 1
    UNLOCK
  ENDIF
  IF lcOrdType='O' .AND. &lcOrdHdr..nSteps < 2 
    *E301077,5 Inhance openning files to speed up transaction
    =gfOpenFile(gcDataDir+'arCusHst',gcDataDir+'Acthst','SH')
    *E301077,5 (End)
    =SEEK(laData[2]+lcGlYear,'arCusHst')
    lnOrdAmt = OrdHdr.OpenAmt &lcExRSin laData[34] &lcUntSin laData[50]
    SELECT arCusHst
    =RLOCK()
    REPLACE nOrdQty&lcGlPeriod WITH nOrdQty&lcGlPeriod - OrdHdr.Open ,;
            nOrdQty            WITH nOrdQty            - OrdHdr.Open ,;
            nOrdAmt&lcGlPeriod WITH nOrdAmt&lcGlPeriod - lnOrdAmt    ,;
            nOrdAmt            WITH nOrdAmt            - lnOrdAmt
    UNLOCK
    SELECT (lcOrdHdr)
    =RLOCK()
    REPLACE nSteps WITH 2
    UNLOCK
  ENDIF
  *E301077,5 Inhance openning files to speed up transaction
  =IIF('AL' $ gcCmpModules,gfCloseFile('PIKLINE') AND gfCloseFile('PIKTKT'),.T.)
  IF ORDHDR.TotCut > 0 AND !oAriaApplication.isRemoteComp 
    =IIF('PO' $ gcCmpModules,gfCloseFile('POSHDR')  AND gfCloseFile('POSLN'),.T.)
    =IIF('MF' $ gcCmpModules,gfCloseFile('CUTTKTH') AND gfCloseFile('CUTTKTL'),.T.)
    =gfCloseFile('CUTPICK')
  ENDIF
  *E301077,5 (End)
  
  IF &lcOrdHdr..nSteps < 3
    SELECT ORDHDR
    =RLOCK()
    *B602410,1 Update canceled date with system date.
    REPLACE STATUS     WITH IIF(Ship > 0,'C','X') ,;
            cCancReson WITH &lcOrdHdr..cCancReson ,;
            Cancelled  WITH gdSysDate ,;
            Cancel     WITH IIF(laData[23]='Y',Cancel ,Cancel+lnOpen),;
            CancelAmt  WITH IIF(laData[23]='Y',CancelAmt ,CancelAmt+lnOpenAmt),;
            Open       WITH 0,;
            OpenAmt    WITH 0,;
            TotCut     WITH 0,;
            Book       WITH lnBook,;
            BookAmt    WITH lnBookAmt ,;
            Flag       WITH SPACE(1)
    UNLOCK
    SELECT (lcOrdHdr)
    =RLOCK()
    REPLACE nSteps WITH 3
    UNLOCK
  ENDIF
ENDIF
*E301077,5 Inhance openning files to speed up transaction
=gfCloseFile('arCusHst')
*E301077,5 (End)

*E301175,1 Add this line to add the capability to call this function from
*          the EDI - from EDIProcessPO Class - [Begin]

*-- If the function was not called from EDI
IF !llFromEDI
*E301175,1 Add this line to add the capability to call this function [End]
  
  SELECT unCmSess
  *B602496,1 Update the order record in the uncmsess file
  =SEEK('O'+PADR('SOORD',10)+gcUser_id+lcSession)
  *B602496,1 (End)
  
  REPLACE STATUS WITH 'C'
  UNLOCK
  
*E301175,1 Add this line to add the capability to call this function from
*          the EDI - from EDIProcessPO Class - [Begin]
ENDIF    && End of IF !llFromEDI
*E301175,1 Add this line to add the capability to call this function [End]

llContinue = .F.

WAIT CLEAR  
SELECT ORDHDR
SCATTER FIELDS &lcScFields TO laData
IF !llFromEDI
  =lfGetInfo()
ENDIF
RETURN

*!*************************************************************
*! Name      : lfGet_Info
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Restore invoice information
*!*************************************************************
*! Calls     : gfGetAdr
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfGet_Info()
*!*************************************************************
FUNCTION lfGet_Info

=SEEK('M'+laData[2],'Customer')
llConsAcc  = Customer.CONSOL='Y'
lcPriceLvl = IIF(!EMPTY(Customer.PriceLvl),Customer.PriceLvl,'A')
=SEEK(IIF(EMPTY(laData[3]),'M'+laData[2],'S'+laData[2]+laData[3]),'Customer')
lnShipAddr = IIF(laData[51],2,1)
STORE '' TO  lcShipName,lcShipAdd1,lcShipAdd2,lcShipAdd3,lcShipAdd4,lcShipAdd5,;
             lcBillName,lcBillAdd1,lcBillAdd2,lcBillAdd3,lcBillAdd4,lcBillAdd5
llMultiSt = (laData[6]='Y')

IF llMultiSt
  STORE 'At Store Level' TO lcBillName
ELSE
  lcBillName = Customer.BtName
  =gfGetAdr('Customer','','','',1,'2')
  FOR lnCount = 1 TO ALEN(laAddress,1)
    lcCount = STR(laAddress[lnCount,1],1)
    lcBillAdd&lcCount = lcBillAdd&lcCount + IIF(EMPTY(lcBillAdd&lcCount),'',',')+;
      SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
  ENDFOR
ENDIF
IF lnShipAddr = 2
  lcShipName = IIF(laScrMode[2],OrdHdr.StName,&lcOrdHdr..StName)
  lcShipAdd1 = IIF(laScrMode[2],OrdHdr.cAddress1,&lcOrdHdr..cAddress1)
  lcShipAdd2 = IIF(laScrMode[2],OrdHdr.cAddress2,&lcOrdHdr..cAddress2)
  lcShipAdd3 = IIF(laScrMode[2],OrdHdr.cAddress3,&lcOrdHdr..cAddress3)
  lcShipAdd4 = IIF(laScrMode[2],OrdHdr.cAddress4,&lcOrdHdr..cAddress4)
  lcShipAdd5 = IIF(laScrMode[2],OrdHdr.cAddress5,&lcOrdHdr..cAddress5)
  IF laScrMode[2]
    SHOW GET lcShipName DISABLE
    SHOW GET lcShipAdd1 DISABLE
    SHOW GET lcShipAdd2 DISABLE
    SHOW GET lcShipAdd3 DISABLE
    SHOW GET lcShipAdd4 DISABLE
    SHOW GET lcShipAdd5 DISABLE
  ELSE
    SHOW GET lcShipName ENABLE
    SHOW GET lcShipAdd1 ENABLE
    SHOW GET lcShipAdd2 ENABLE
    SHOW GET lcShipAdd3 ENABLE
    SHOW GET lcShipAdd4 ENABLE
    SHOW GET lcShipAdd5 ENABLE
  ENDIF
ELSE
  IF llMultiSt
    STORE 'At Store Level' TO lcShipName
  ELSE
    lcShipName =  IIF(EMPTY(Customer.Dba),Customer.StName,Customer.Dba)
    =gfGetAdr('CUSTOMER','','','',1,'')
    FOR lnCount = 1 TO ALEN(laAddress,1)
      lcCount = STR(laAddress[lnCount,1],1)
      lcShipAdd&lcCount = lcShipAdd&lcCount + IIF(EMPTY(lcShipAdd&lcCount),'',',')+;
      SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
    ENDFOR
  ENDIF
  SHOW GET lcShipName DISABLE
  SHOW GET lcShipAdd1 DISABLE
  SHOW GET lcShipAdd2 DISABLE
  SHOW GET lcShipAdd3 DISABLE
  SHOW GET lcShipAdd4 DISABLE
  SHOW GET lcShipAdd5 DISABLE
ENDIF  
lcDelMesag = 'cancel'
DO CASE
  CASE laData[5] = 'B'
    lnOrdStatus = 1
    SHOW GET pbDlt,1 PROMPT lcCancel
  CASE laData[5] = 'O'
    lnOrdStatus = 2
    SHOW GET pbDlt,1 PROMPT lcCancel
  CASE laData[5] = 'H'
    lnOrdStatus = 3
    SHOW GET pbDlt,1 PROMPT lcCancel
  CASE laData[5] = 'X'
    lnOrdStatus = 4
    SHOW GET pbDlt,1 PROMPT lcUnCancel
    lcDelMesag    = 'uncancel'
  CASE laData[5] = 'C'
    lnOrdStatus = 5
    SHOW GET pbDlt,1 PROMPT lcCancel
ENDCASE
llInsur   = (laData[22]='Y')
llBulk    = (laData[23]='Y')
llReorder = (laData[24]='Y')
=gfwCodePop(@laCodes,'CTERMCODE','T')
=gfwCodePop(@laCodes,'SHIPVIA','T')
*B802366,1 AMM (Start) Display "At Store Level" instead of "ALL" in ship via POPUP
IF llMultiSt
  =lfChMSHV()
ENDIF
*B802366,1 AMM end

=gfwCodePop(@laCodes,'SPCINST','T')
=gfwCodePop(@laCodes,'SEASON','T')
=gfwCodePop(@laCodes,'CDIVISION','T')
*C101557,1 Added to get order category code.
=gfwCodePop(@laCodes,'CORDERCAT','T')
*C101557,1 End.

lnWareHouse = ASCAN(laWareHouses,laData[31])
lnWareHouse = IIF(lnWareHouse=0,1,ASUBSCRIPT(laWareHouses,lnWareHouse,1))
SHOW GET lnOrdStatus
SHOW GET lnWareHouse
SHOW GET lnShipAddr
SHOW GET llMultiSt
llReBrowse = .T.


*!*************************************************************
*! Name      : lfOrdQUpd                         *B602952,1
*! Developer : Timour A. K.
*! Date      : 05/30/1999
*! Purpose   : Update style orderd quantity in case of order
*!             line style was changed.
*!*************************************************************
*! Calls     : lfSavScr
*!*************************************************************
*! Parameters: lcFlToUpd-> Alias to update (STYLE or STYDYE).
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfOrdQUpd
PARA lcFlToUpd

*--Update New Style.
SELECT (lcFlToUpd)
=RLOCK()
REPLACE Ord1   WITH Ord1   + IIF(OrdHdr.Status='B',0,&lcOrdLine..Qty1),;
        Ord2   WITH Ord2   + IIF(OrdHdr.Status='B',0,&lcOrdLine..Qty2),;
        Ord3   WITH Ord3   + IIF(OrdHdr.Status='B',0,&lcOrdLine..Qty3),;
        Ord4   WITH Ord4   + IIF(OrdHdr.Status='B',0,&lcOrdLine..Qty4),;
        Ord5   WITH Ord5   + IIF(OrdHdr.Status='B',0,&lcOrdLine..Qty5),;
        Ord6   WITH Ord6   + IIF(OrdHdr.Status='B',0,&lcOrdLine..Qty6),;
        Ord7   WITH Ord7   + IIF(OrdHdr.Status='B',0,&lcOrdLine..Qty7),;
        Ord8   WITH Ord8   + IIF(OrdHdr.Status='B',0,&lcOrdLine..Qty8),;
        TotOrd WITH TotOrd + IIF(OrdHdr.Status='B',0,&lcOrdLine..TotQty)
UNLOCK

*--Update Old Style.
=SEEK(OrdLine.Style+IIF(lcFlToUpd='STYLE','',laData[31]+SPACE(10)),lcFlToUpd)
=RLOCK()
REPLACE Ord1   WITH Ord1   - IIF(OrdHdr.Status='B',0,OrdLine.Qty1),;
        Ord2   WITH Ord2   - IIF(OrdHdr.Status='B',0,OrdLine.Qty2),;
        Ord3   WITH Ord3   - IIF(OrdHdr.Status='B',0,OrdLine.Qty3),;
        Ord4   WITH Ord4   - IIF(OrdHdr.Status='B',0,OrdLine.Qty4),;
        Ord5   WITH Ord5   - IIF(OrdHdr.Status='B',0,OrdLine.Qty5),;
        Ord6   WITH Ord6   - IIF(OrdHdr.Status='B',0,OrdLine.Qty6),;
        Ord7   WITH Ord7   - IIF(OrdHdr.Status='B',0,OrdLine.Qty7),;
        Ord8   WITH Ord8   - IIF(OrdHdr.Status='B',0,OrdLine.Qty8),;
        TotOrd WITH TotOrd - IIF(OrdHdr.Status='B',0,OrdLine.TotQty)
UNLOCK
RETURN

*!*************************************************************
*! Name      : lfStyPos
*! Developer : Reham Al-Allamy
*! Date      : 07/20/1999
*! Purpose   : Function to check if there is style position for 
*!           : any order line to be sure it has been filled.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfStyPos()
*!*************************************************************
*
FUNCTION lfStyPos
PRIVATE lcCurAlias

*-- Save the current alias.
lcCurAlias = ALIAS()

*-- Flag to know if valid to save or not.
llSvBomVar = .T.

*-- Open the style positions file to Check if there is positions for the selected style.
=gfOpenFile(gcDataDir+'IcStyPos',gcDataDir+'IcStyPos','SH')

*-- Scan all the order lines to check if there is style position entered for the lines 
*-- that have to have records in the BomVar file.
SELECT (lcOrdLine)
SCAN FOR !DELETED()
  *-- Check if there is positions for the style in the current order line.
  IF SEEK(&lcOrdLine..Style , "IcStyPos")
    *-- Check if there is style position entered for the current line.
    IF SEEK("SO" + laData[1] + STR(&lcOrdLine..LineNo,6) , lcT_BomVar)
      *-- Check that at least one of the name drop or the design code has entered for 
      *-- all the style positions of the current order line.
      SELECT (lcT_BomVar)
      SCAN REST WHILE cidtype+ccost_id+STR(lineno,6) = "SO"+laData[1] + STR(&lcOrdLine..LineNo,6)
        IF EMPTY(&lcT_BomVar..cNDrpId) .AND. EMPTY(&lcT_BomVar..cDsgnCode)
          *** You cannot leave both name-drop and design code empty for style: {&lcOrdLine..Style}. ***
          *** <  OK  > ***
          =gfModalGen("TRM32071B00000" , "DIALOG" , &lcOrdLine..Style)
          llSvBomVar = .F.
        ENDIF
      ENDSCAN
      SELECT (lcOrdLine)
    ELSE
      *** You have to enter style position for Style: {&lcOrdLine..Style}. ***
      *** <  OK  > ***
      =gfModalGen("TRM32070B00000" , "DIALOG" , &lcOrdLine..Style)
      llSvBomVar = .F.
    ENDIF
  ENDIF
ENDSCAN

*-- Restore the previous alias.
SELECT (lcCurAlias)

*-- Return with the returned flag.
RETURN llSvBomVar

*:**************************************************************************
*:* Name        : lfDivByZr                                      *B607436,1
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/30/2003
*:* Purpose     : Overcome the error "Division by 0 when updating the field nOrdAmt
* Note          : this error is difficult to generate so this function is only to be sure
*               : that this type of error will not happen.
*:***************************************************************************
FUNCTION lfDivByZr
PRIVATE llDivByZro,lnOrdAmt,lcUntSin,lcExRSin,lnSlct
lnSlct = SELECT()
llDivByZro = .F.
lcUntSin = ''
lcExRSin = gfGetExSin(@lcUntSin, laData[33])

laData[50] = IIF(laData[50] = 0 , 1 , laData[50] )
SELECT ORDHDR
IF ORDHDR.nCurrUnit = 0
  REPLACE ORDHDR.nCurrUnit WITH 1
ENDIF
IF ORDHDR.NEXRATE = 0   
  REPLACE ORDHDR.NEXRATE WITH 1
ENDIF  

SELECT &lcOrdLine
lnRecno = RECNO()
GO TOP
SCAN
  STORE 0 TO lnAmt1,lnAmt2
  lnAmt1 = &lcOrdLine..TotQty*&lcOrdLine..Price &lcExRSin laData[34]     &lcUntSin laData[50]
  IF !llFromEDI AND laScrMode[3]
    lnAmt2 = &lcOrdLine..TotQty*&lcOrdLine..Price &lcExRSin OrdHdr.nExRate &lcUntSin OrdHdr.nCurrUnit 
  ENDIF
  IF '*' $ TRANSFORM(lnAmt1+lnAmt2,REPL('9',15))
    =gfModalGen('INM32121B00000','ALERT')
    llDivByZro = .T.
    EXIT
  ENDIF
ENDSCAN

IF BETWEEN(lnRecno,1,RECCOUNT(lcOrdLine))
  GOTO (lnRecno)
ENDIF  

SELECT (lnSlct)
RETURN llDivByZro
*-- end of lfDivByZr.

*!**********************************************************************************
*! Name      : lfSQLUpdate
*! Developer : Timour A. K.
*! Date      : 07/26/2001
*! Purpose   : SQL Update Cursor, Delete and Insert records.
*! Entry no. : N000260 
*!**********************************************************************************
*! Parameters: cCursorName : Cursor Name to Update
*!             cTranCode   : Transaction update Code
*!          nDataSessionID : Private Data Session ID number, to create cursor on it.
*!**********************************************************************************
*! Returns : Numeric (Returns the number of result).
*!            1 Finished updating successfully.
*!            2 Nothing Updated.
*!           -1 Error while updating.
*!           -3 No Primary key found for this table, unable to update.
*!           -4 Unable to retrieve updating table information.
*!           -5 Connection Object does not exist
*!           -6 One or more parameter are empty, the parameter are mandatory.
*!           -7 The cursor is not in use.
*!           -8 This Transaction code does not exist (Require begin transaction)
*!           -9 This Cursor is not updateable.
*!**********************************************************************************
*! Modifications :
*! E038142,2 MAH 09/01/2004 Full support for run forms with SQL with high Performance.
*!*****************************************************************************************
*! E038142,2 MAH 09/02/2004 Add addtional parameter lcPKIndex [BEGIN]
*-- LPARAMETERS cCursorName,lcTranCode,nDataSessionID,lcPrimaryKeyList,lcTable
FUNCTION lfSQLUpdate
LPARAMETERS cCursorName, lcTranCode, nDataSessionID, lcPrimaryKeyList, lcTable, lcPKIndex
*! E038142,2 MAH 09/02/2004 [END]


*--Check and validate passed parameters.
cCursorName = IIF(TYPE('cCursorName')='C' AND !EMPTY(cCursorName),cCursorName,"")
lcTranCode   = IIF(TYPE('lcTranCode')='C'   AND !EMPTY(lcTranCode)  ,lcTranCode  ,"")
nDataSessionID = IIF(TYPE('nDataSessionID')='N',nDataSessionID ,SET("DATASESSION"))

*--Check parameters (Both are mandatory and cannot be empty)
IF EMPTY(cCursorName) OR EMPTY(lcTranCode)
  *--One or more parameter are empty, the parameter are mandatory. 
  RETURN -6
ENDIF

LOCAL lnOldSession,lnCurrentWorkAria
lnOldSession = SET("Datasession")  && Save current data session.
lnCurrentWorkAria = SELECT()       && Save current work aria pointer.

*-- Set data session.
IF nDataSessionID <> SET("DATASESSION")
  SET DATASESSION TO nDataSessionID
ENDIF

*--Check for the required cursor is opened (used).
IF !USED(cCursorName)
  *--The cursor is not used.
  RETURN -7
ENDIF
 
LOCAL lcUpdTable   && Table to update.
lcUpdTable = IIF(TYPE('lcTable')='C',lcTable,CURSORGETPROP("Tables",cCursorName))
IF EMPTY(lcUpdTable)
  *--This Cursor is not updatable
  RETURN -9
ENDIF

*--Define local variables.
LOCAL lnHandle,;          && Connection Handle. 
      lcPrimaryKeyList,;  && Primary key fields list comma delemited.
      lcPrimKey,;         && Primary key fields sql expresion.
      lcDelSetting,;      && Deletion setting.
      lnResult,;          && This method return result. 
      lnExResult,;        && SQLExecution return result.
      lnChange,;          && Changed Record number. 
      lcUpdString         && Fields to Update list string.


*--Check for existance of connection object.
IF TYPE("oAriaApplication.RemoteCompanyData.oConnectionsClass") <> 'O'
  *--Connection Object does not exist
  RETURN -5
ENDIF

*--Check for existance of this transaction and get this transaction handle.
lnHandle = oAriaApplication.RemoteCompanyData.oConnectionsClass.CheckTransaction(lcTranCode)

*! E038142,2 MAH 09/01/2004 Get database engine ID [BEGIN]
LOCAL lcDBEngine
lcDBEngine = ALLTRIM(oAriaApplication.RemoteCompanyData.mGetConnectionDBEngine(lnHandle))
*! E038142,2 MAH 09/01/2004 [END]

IF lnHandle < 1
  *--oAriaApplication.RemoteCompanyData Transaction code does not exist (Require begin transaction)
  RETURN -8
ENDIF

*--Get Pimary key fields list.
lcPrimKey = ""

IF EMPTY(lcPrimaryKeyList)
  lcPrimaryKeyList = oAriaApplication.RemoteCompanyData.GetKeyFields(lcUpdTable)
  IF EMPTY(lcPrimaryKeyList)
    *--No Primary key found for this table, unable to update.
    RETURN -3
  ENDIF
ENDIF

*! E038142,2 MAH 09/01/2004 Buid primary key expression based on engine type [BEGIN]
DO CASE
  CASE ALLTRIM(lcDBEngine) == oAriaApplication.cSQLDBID
    *! E038142,2 MAH 09/01/2004 [END]
    lcPrimaryKeyList =","+lcPrimaryKeyList+","
    lnKeysNo  = OCCURS(',',lcPrimaryKeyList)-1
    FOR lnI=1 TO lnKeysNo
      lcFld = STREXTRACT(lcPrimaryKeyList, "," , "," ,lnI )
      *wael
      lcPrimKey = lcPrimKey + "["+lcFld+"]=?m."+lcFld+IIF(lnI<>lnKeysNo," AND ","")
   *  lcPrimKey = lcPrimKey + lcFld+"=?m."+lcFld+IIF(lnI<>lnKeysNo," AND ","")
      *wael
    ENDFOR
    
    *! E038142,2 MAH 09/01/2004 Buid primary key expression based on engine type [BEGIN]
  CASE ALLTRIM(lcDBEngine) == oAriaApplication.cFOXDBID
    lcPrimKey = lcPrimaryKeyList + ' = '
    
    *-- Replace fields in the expression to ?m.Field
    LOCAL lnLexemes
    lnLexemes = 1
    
    LOCAL laLexemes[1]
    laLexemes[1] = ''
    
    LOCAL lnIndex
    FOR lnIndex = 1 TO LEN(lcPrimaryKeyList)
      LOCAL lcChar
      lcChar = SUBSTR(lcPrimaryKeyList, lnIndex, 1)
      
      IF ISALPHA(lcChar) .OR. lcChar = '_'
        IF EMPTY(laLexemes[lnLexemes]) .OR. ISALPHA(laLexemes[lnLexemes]) .OR. LEFT(laLexemes[lnLexemes], 1) = '_'
          laLexemes[lnLexemes] = laLexemes[lnLexemes] + lcChar
        ELSE
          lnLexemes = lnLexemes + 1
          LOCAL laLexemes[lnLexemes]
          laLexemes[lnLexemes] = lcChar
        ENDIF
      ELSE
        lnLexemes = lnLexemes + 1
        LOCAL laLexemes[lnLexemes]
        laLexemes[lnLexemes] = lcChar
      ENDIF
    ENDFOR
    
    FOR lnIndex = 1 TO ALEN(laLexemes, 1)
      IF ISALPHA(laLexemes[lnIndex]) .OR. LEFT(laLexemes[lnIndex], 1) = '_'
        IF UPPER(ALLTRIM(laLexemes[lnIndex])) $ 'SUBSTR|PADR|UPPER|DTOS|STR|ALLTRIM|LEFT|LOWER'
          lcPrimKey = lcPrimKey + laLexemes[lnIndex]
        ELSE
          lcPrimKey = lcPrimKey + '?m.' + ALLTRIM(laLexemes[lnIndex])
        ENDIF
      ELSE
        lcPrimKey = lcPrimKey + laLexemes[lnIndex]
      ENDIF
    ENDFOR
ENDCASE
*! E038142,2 MAH 09/01/2004 [END]

*--Get Table columns in cursor.
oAriaApplication.RemoteCompanyData.GetColumns(lcUpdTable,"TmpTblFld",lcTranCode,4,"FOXPRO",nDataSessionID)
IF !USED("TmpTblFld")
  *--Unable to retrive updating table's information.
  RETURN -4
ENDIF

*--Get Native Table columns in cursor.
oAriaApplication.RemoteCompanyData.GetColumns(lcUpdTable,"TmpNTblFld",lcTranCode,4,"NATIVE",nDataSessionID)
IF !USED("TmpNTblFld")
  *--Unable to retrive updating table's information.
  RETURN -4
ENDIF

*--Default return value is Nothing Changed (Nothing to Update).
STORE 2 TO lnResult,lnExResult

*--Loop on modified records in cursor.

lcDelSetting = SET("Deleted") 
SET DELETED OFF 
SELECT (cCursorName)

*! E038142,2 MAH 09/02/2004 In case of SQL, build update view, which will be used in update / delete [BEGIN]
IF ALLTRIM(lcDBEngine) == oAriaApplication.cSQLDBID .AND. TYPE('lcPKIndex') = 'C' .AND. !EMPTY(lcPKIndex)
  LOCAL lcUpdView

  *-- Get Temp name
  LOCAL lcTempName
  lcTempName = GFTempName()
  
  *-- Build update function command
  lcUpdFunctionSQL = 'CREATE FUNCTION ' + lcTempName + ' ('
  
  LOCAL lcVeiwParam
  lcVeiwParam = ''
  
  LOCAL lnIndex
  FOR lnIndex = 1 TO OCCURS(',', lcPrimaryKeyList) - 1
    LOCAL lcField
    lcField = STREXTRACT(lcPrimaryKeyList, ',', ',', lnIndex)
    
    lcVeiwParam = lcVeiwParam + '?m.' + ALLTRIM(lcField)
    
    lcUpdFunctionSQL = lcUpdFunctionSQL + '@' + ALLTRIM(lcField) + ' '
    
    DO CASE
      CASE TYPE(lcField) = 'C'
        lcUpdFunctionSQL = lcUpdFunctionSQL + 'CHAR(' + ALLTRIM(STR(LEN(EVALUATE(lcField)))) + ')'
  
      CASE TYPE(lcField) = 'N'
       lcUpdFunctionSQL = lcUpdFunctionSQL + 'NUMERIC'
       
      CASE TYPE(lcField) = 'L'
        lcUpdFunctionSQL = lcUpdFunctionSQL + 'BIT'
      
      CASE TYPE(lcField) = 'D' .OR. TYPE(lcField) = 'T'
        lcUpdFunctionSQL = lcUpdFunctionSQL + 'DATETIME'
    ENDCASE
  
    IF lnIndex = OCCURS(',', lcPrimaryKeyList) - 1
      lcUpdFunctionSQL = lcUpdFunctionSQL + ') '
    ELSE
      lcVeiwParam = lcVeiwParam + ', '
      
      lcUpdFunctionSQL = lcUpdFunctionSQL + ', '
    ENDIF
  ENDFOR
  
  lcUpdFunctionSQL = lcUpdFunctionSQL + 'RETURNS TABLE AS RETURN '
  
  lcUpdFunctionSQL = lcUpdFunctionSQL + ;
                     '(SELECT * FROM ' + lcTable + '(INDEX = ' + lcPKIndex + ')' + ' WHERE ' + ;
                     STRTRAN(lcPrimKey, '?m.', '@') + ')'

  lcUpdView = lcTempName + '(' + lcVeiwParam + ')'
ENDIF
*! E038142,2 MAH 09/02/2004 [END]

*! E038142,2 MAH 09/01/2004 Set Exact [BEGIN]
*-- lnChange=GETNEXTMODIFIED(0)
lnResult = 1
IF ALLTRIM(lcDBEngine) == oAriaApplication.cFOXDBID
  lnResult = SQLEXEC(lnHandle, 'SET ANSI ' + SET('Exact'))
ENDIF

IF lnResult = 1
  lnChange = GETNEXTMODIFIED(0)
ELSE
  lnChange = 0
ENDIF
*! E038142,2 MAH [END]

DO WHILE lnChange <> 0
  *--Go to modified record.
  GOTO lnChange

  SCATTER MEMVAR MEMO

  DO CASE
    *--Update -----------------------------------------------------------------
    CASE !DELETED() AND RECNO()>0
      *! E038142,2 MAH 09/02/2004 Create update function if created [BEGIN]
      IF ALLTRIM(lcDBEngine) == oAriaApplication.cSQLDBID .AND. TYPE('lcPKIndex') = 'C' .AND. !EMPTY(lcPKIndex)
        LOCAL llUpdateFuncCreated
        
        IF !llUpdateFuncCreated
          IF SQLEXEC(lnHandle, lcUpdFunctionSQL) # 1
            SET DATASESSION TO lnOldSession
            SELECT(lnCurrentWorkAria)
            
            *AMH 2/7/2005 [Start]
            oAriaApplication.RemoteCompanyData.mdebugsqlstr('',lcUpdFunctionSQL)
            *AMH 2/7/2005 [End]
            
            RETURN -10 && Error while creating update function
          ENDIF
        ENDIF
      
        llUpdateFuncCreated = .T.
      ENDIF
      *! E038142,2 MAH 09/02/2004 [END]

       *--Build Update fields SQL string.
       *! E038142,2 MAH 09/01/2004 Get database engine ID [BEGIN]
       *-- lcUpdString = This.buildupdatestring("UPDATE")
       lcUpdString = oAriaApplication.RemoteCompanyData.BuildUpdateString("UPDATE", lcDBEngine)
       *! E038142,2 MAH 09/01/2004 [END]
       
       IF !EMPTY(lcUpdString)
         *--Execute path throgh command for SQL Update.
         *--Sent as UPDATE TableName SET Column_Name1 = eExpression1 [, Column_Name2 = eExpression2 ...] WHERE FilterCondition1...]
         *! E038142,2 MAH 09/02/2004 Create update function if created [BEGIN]
         *-- lnExResult = SQLEXEC(lnHandle,"UPDATE "+lcUpdTable+" "+lcUpdString+" WHERE "+lcPrimKey)
         IF ALLTRIM(lcDBEngine) == oAriaApplication.cSQLDBID .AND. TYPE('lcPKIndex') = 'C' .AND. !EMPTY(lcPKIndex)
           lnExResult = SQLEXEC(lnHandle,'UPDATE ' +  lcUpdView + ' ' + lcUpdString)
           
           *AMH 9/26/2004 [Start]
           IF lnExResult # 1
             oAriaApplication.RemoteCompanyData.mdebugsqlstr(lcUpdFunctionSQL,'UPDATE ' +  lcUpdView + ' ' + lcUpdString)
           ENDIF
           *AMH 9/26/2004 [End]
           
         ELSE
           lnExResult = SQLEXEC(lnHandle,"UPDATE "+lcUpdTable+" "+lcUpdString+" WHERE "+lcPrimKey)
           
           *AMH 9/26/2004 [Start]
           IF lnExResult # 1
             oAriaApplication.RemoteCompanyData.mdebugsqlstr('',"UPDATE "+lcUpdTable+" "+lcUpdString+" WHERE "+lcPrimKey)
           ENDIF
           *AMH 9/26/2004 [End]
           
         ENDIF
         *! E038142,2 MAH 09/02/2004 [END]
       ENDIF

    *--Delete -----------------------------------------------------------------
    CASE DELETED() AND RECNO()>0
      *! E038142,2 MAH 09/02/2004 Create update function if created [BEGIN]
      IF ALLTRIM(lcDBEngine) == oAriaApplication.cSQLDBID .AND. TYPE('lcPKIndex') = 'C' .AND. !EMPTY(lcPKIndex)
        LOCAL llUpdateFuncCreated
        
        IF !llUpdateFuncCreated
          IF SQLEXEC(lnHandle, lcUpdFunctionSQL) # 1
            SET DATASESSION TO lnOldSession
            SELECT(lnCurrentWorkAria)
            
            *AMH 2/7/2005 [Start]
            oAriaApplication.RemoteCompanyData.mdebugsqlstr('',lcUpdFunctionSQL)
            *AMH 2/7/2005 [End]
            
            RETURN -10 && Error while creating update function
          ENDIF
        ENDIF
        
        llUpdateFuncCreated = .T.
      ENDIF
      *! E038142,2 MAH 09/02/2004 [END]

       *--Execute path throgh command for SQL Delete.

       *! E038142,2 MAH 09/02/2004 Create update function if created [BEGIN]
       *-- lnExResult = SQLEXEC(lnHandle,"DELETE FROM "+ lcUpdTable + " WHERE "+lcPrimKey)
       IF ALLTRIM(lcDBEngine) == oAriaApplication.cSQLDBID .AND. TYPE('lcPKIndex') = 'C' .AND. !EMPTY(lcPKIndex)
         lnExResult = SQLEXEC(lnHandle, 'DELETE FROM ' + lcUpdView)
         
         *AMH 9/26/2004 [Start]
         IF lnExResult # 1
           oAriaApplication.RemoteCompanyData.mdebugsqlstr(lcUpdFunctionSQL,'DELETE FROM ' + lcUpdView)
         ENDIF
         *AMH 9/26/2004 [End]
         
       ELSE
         lnExResult = SQLEXEC(lnHandle,"DELETE FROM "+ lcUpdTable + " WHERE "+lcPrimKey) 
         
         *AMH 9/26/2004 [Start]
         IF lnExResult # 1
           oAriaApplication.RemoteCompanyData.mdebugsqlstr('',"DELETE FROM "+ lcUpdTable + " WHERE "+lcPrimKey)
         ENDIF
         *AMH 9/26/2004 [End]
         
       ENDIF
       *! E038142,2 MAH 09/02/2004 [END]

    *--Insert ----------------------------------------------------------------- 
    CASE !DELETED() AND RECNO()<0  
       *--Build Insert fields SQL string.
       *! E038142,2 MAH 09/01/2004 Get database engine ID [BEGIN]
       *-- lcInsString = This.buildupdatestring("INSERT")
       lcInsString = oAriaApplication.RemoteCompanyData.BuildUpdateString("INSERT", lcDBEngine)
       *! E038142,2 MAH 09/01/2004 [END]
       
       IF !EMPTY(lcInsString)
         *--Execute path throgh command for SQL Insert.
         *--Sent as INSERT INTO dbf_name [(fname1 [, fname2, ...])] VALUES (eExpression1 [, eExpression2, ...])

         lnExResult = SQLEXEC(lnHandle,"INSERT INTO "+lcUpdTable+" "+lcInsString) 
         
         *! E038142,2 MAH 09/01/2004 In case of fox the sqlrun returns -1 with insert [BEGIN]
         *-- In case of fox 
         IF ALLTRIM(lcDBEngine) = oAriaApplication.cFOXDBID         
           lnExResult = 1
         ENDIF
         *! E038142,2 MAH 09/01/2004 [END]
         
         *AMH 9/26/2004 [Start]
         IF lnExResult # 1
           oAriaApplication.RemoteCompanyData.mdebugsqlstr('',"INSERT INTO "+lcUpdTable+" "+lcInsString)
         ENDIF
         *AMH 9/26/2004 [End]
         
       ENDIF
          
  ENDCASE
  lnResult = MIN(lnResult,lnExResult)

  *--Next modified record.
  lnChange=GETNEXTMODIFIED(lnChange)
ENDDO

SET DELETED &lcDelSetting
*--Close table fields cursor.
USE IN TmpTblFld
USE IN TmpNTblFld

*! E038142,2 MAH 09/02/2004 Drop created view [BEGIN]
IF ALLTRIM(lcDBEngine) == oAriaApplication.cSQLDBID .AND. TYPE('lcPKIndex') = 'C' .AND. !EMPTY(lcPKIndex)
  LOCAL lcDropView
  lcDropView = 'DROP FUNCTION ' + LEFT(lcUpdView, AT('(', lcUpdView) - 1)

  *-- Execute drop function command. No nedd to handle the error if occurs.
  = SQLEXEC(lnHandle, lcDropView) 
ENDIF
*! E038142,2 MAH 09/02/2004 [END]

*--Back to old work area and return the string.
SET DATASESSION TO lnOldSession
SELECT(lnCurrentWorkAria)

*--Returns 1 when it has finished executing successfully
*--or -1 if connection level error occurs or 2 if nothing changed.
RETURN lnResult

FUNCTION STREXTRACT
LPARAMETERS lcExpression, cBeginDelim, cEndDelim, lnPos
lcExpression = SUBSTR(lcExpression,AT(cBeginDelim,lcExpression,lnPos)+1)
RETURN SUBSTR(lcExpression,1,AT(cEndDelim,lcExpression)-1)

