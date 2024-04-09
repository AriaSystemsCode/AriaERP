*!****************************************************************************
*! Program file        : Soshlbbc.prg
*! Program desc.       : Custom Shipping Label [BBC10]
*! For Report          : soshlbbc.LBX
*! System              : Aria Advantage Series 4XP.
*! Module              : SO,AR,AL
*! Developer           : Walid Hamed (WLD)
*! Date                : 08/21/2007
*! Reference           : C200835
*!****************************************************************************
*! Calls :  Functions  : lfCrtTemp - lfGetData - lfManufID - lfCheckDgt
*!                       lfwOGWhen(), lfSRVLBL(), lfPikData(), lfInvData(),
*:                       lfOrdData(), lfvLabels(), lfClearRep()
*!****************************************************************************
*! Called From         : System Menu (AL --> Output --> Shipping Label)
*!****************************************************************************
*! Passed Parameters   : None
*!****************************************************************************
*! Example             :
*!****************************************************************************
*! Modification        :
*C2008835,1 WLD Using custom Programs 08/26/2007
*B608331,1 MMT 10/26/2007 fix bugs of wrong case qty and error while preview[T20071016.0010]
*B608331,2 MMT 11/21/2007 fix bugs of not prinitng qty when 2 not put whole pack in one carton[T20071016.0010]
*B608331,3 MMT 11/25/2007 fix bugs of prinitng the Same style Carton twice[T20071016.0010]
*B608331,4 MMT 11/28/2007 fix bugs of prinitng the Same style Carton twice[T20071016.0010]
*B608482,1 MMT 03/26/2008 Fix bug of not prinitng Carrier and notes in case of there is packing list[T20071016.0010]
*B610643,1 TMI 12/29/2013 fix a problem that the shipping labels freez and do not print [T20131217.0012] 
*B610955,1 MMT 03/01/2015 Custom shipping label report does not print style info. in case of mixed carton[T20150220.0028]
*C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025]
*:************************************************************************
#INCLUDE R:\Aria4xp\reports\soshlb.H

IF RECCOUNT(lclblTbl) = 0
  *-- Message 'There are no records to display...!'
  =gfModalGen('TRM00052B00000','DIALOG')
  RETURN
ENDIF

*IF loOGScroll.llOGFltCh   &&If Filter Changed
  *-- Variable to open the customer file again with it.
  lcCust_A  = loOgScroll.gfTempName()

  *-- The main temporary file we collect data in it.
  lcLblTemp = loOgScroll.gfTempName()

  *-- Open Customer file again to get the Distribution center store addresses
  *-- from , because Customer file is engaged in a relation of the main store.
  *B608331,1 MMT 10/26/2007 fix bugs of wrong case qty and error while preview[Start]
  *lolcCust_A =CreateObject("RemoteTable","CUSTOMER","CUSTOMER","LCCUST_A",SET("Datasession"))
  lolcCust_A =CreateObject("RemoteTable","CUSTOMER","CUSTOMER",LCCUST_A,SET("Datasession"))
  *B608331,1 MMT 10/26/2007 fix bugs of wrong case qty and error while preview[End]

  STORE SPACE(0) TO lcCmAdr1, lcCmAdr2, lcCmAdr3, lcCmAdr4, lcAddr1 ,lcAddr2,;
                    lcAddr3,lcActNam, lcAcct, lcMainF

  *-- Open Syccomp file remotly  (System file)
  *-- Get company Address
  PRIVATE  lcSqlCommand , lnResult
  lcSqlCommand=[SELECT cCom_Name,cCont_Code,cCom_Phon,cAddress1,cAddress2,cAddress3,cAddress4,cAddress5,cAddress6 FROM SYCCOMP WHERE Ccomp_id=']+oAriaApplication.ActiveCompanyID+[']
  lnResult  = oAriaApplication.remotesystemdata.execute(lcSqlCommand,"","SYCCOMP","",oAriaApplication.SystemConnectionString,3,"",SET("DATASESSION"))
  * lcPhonPict = gfPhoneTem()          && Variable to hold the Company Phone Format
  *-- Get the setting of print Company/warehouse addresses.

  lcPrtAdr = IIF(TYPE('lcPrtAdr') = 'C', lcPrtAdr, 'C')
  IF lnResult >= 1
    IF lcPrtAdr  = 'C'
      lcCompName = cCom_Name
      *-- Get the company addresses
      lcCmAdr1 = gfGetAdr('SYCCOMP' , '' , '' , '' , 1)
      lcCmAdr2 = gfGetAdr('SYCCOMP' , '' , '' , '' , 2)
      lcCmAdr3 = gfGetAdr('SYCCOMP' , '' , '' , '' , 3)
      lcCmAdr4 = gfGetAdr('SYCCOMP' , '' , '' , '' , 4)
      lcCmAdr5 = gfGetAdr('SYCCOMP' , '' , '' , '' , 5)
    ENDIF
  ENDIF

  DO CASE
    *-- Current module is Sales Order Allocation.
    CASE lcXTYPE = 'P'
      SELECT PIKTKT
      =AFIELDS(laFileStru)
      IF ASCAN(laFileStru,'LABELS') = 0
        lnFileStru = ALEN(laFileStru,1)
        DIMENSION laFileStru[lnFileStru+1,18]
        lnFileStru = lnFileStru+1
        laFileStru[lnFileStru,1] = 'LABELS'   &&Number of labels required
        laFileStru[lnFileStru,2] = 'N'
        laFileStru[lnFileStru,3] = 4
        laFileStru[lnFileStru,4] = 0
      ENDIF
      lnFileStru = ALEN(laFileStru,1)
      DIMENSION laFileStru[lnFileStru+1,18]
      lnFileStru = lnFileStru+1
      laFileStru[lnFileStru,1] = 'nLblsNo'    &&Label Number
      laFileStru[lnFileStru,2] = 'N'
      laFileStru[lnFileStru,3] = 4
      laFileStru[lnFileStru,4] = 0

      FOR lncnt=7 TO 16
        FOR lnInc=1 TO lnFileStru
          STORE SPACE(0) TO laFileStru[lnInc,lnCnt],laFileStru[lnInc,lnCnt]
        ENDFOR
      ENDFOR
      FOR lnInc=1 TO lnFileStru
        STORE 0 TO laFileStru[lnInc,17],laFileStru[lnInc,18]
      ENDFOR
      =gfCrtTmp(lcLblTemp,@laFileStru,'PIKTKT',lcLblTemp,.F.)

      IF !lfpikData()
        RETURN
      ENDIF

    *-- Current module is Accounts Receivable.
    CASE lcXTYPE = 'I'
      SELECT INVHDR
      =AFIELDS(laFileStru)
      IF ASCAN(laFileStru,'LABELS') = 0
        lnFileStru = ALEN(laFileStru,1)
        DIMENSION laFileStru[lnFileStru+1,18]
        lnFileStru = lnFileStru+1
        laFileStru[lnFileStru,1] = 'LABELS'    &&Number of labels required
        laFileStru[lnFileStru,2] = 'N'
        laFileStru[lnFileStru,3] = 4
        laFileStru[lnFileStru,4] = 0
      ENDIF
      lnFileStru = ALEN(laFileStru,1)
      DIMENSION laFileStru[lnFileStru+1,18]
      lnFileStru = lnFileStru+1
      laFileStru[lnFileStru,1] = 'nLblsNo'      &&Label Number
      laFileStru[lnFileStru,2] = 'N'
      laFileStru[lnFileStru,3] = 4
      laFileStru[lnFileStru,4] = 0
      FOR lncnt=7 TO 16
        FOR lnInc=1 TO lnFileStru
          STORE SPACE(0) TO laFileStru[lnInc,lnCnt],laFileStru[lnInc,lnCnt]
        ENDFOR
      ENDFOR
      FOR lnInc=1 TO lnFileStru
        STORE 0 TO laFileStru[lnInc,17],laFileStru[lnInc,18]
      ENDFOR
      =gfCrtTmp(lcLblTemp,@laFileStru,'INVOICE+STORE',lcLblTemp,.F.)

      IF !lfInvData()
        RETURN
      ENDIF

    *-- Current module is Sales Order.
    CASE lcXTYPE = 'O'
      SELECT ORDHDR
      =AFIELDS(laFileStru)
      IF ASCAN(laFileStru,'LABELS') = 0    
        lnFileStru = ALEN(laFileStru,1)
        DIMENSION laFileStru[lnFileStru+1,18]
        lnFileStru = lnFileStru+1
        laFileStru[lnFileStru,1] = 'LABELS'
        laFileStru[lnFileStru,2] = 'N'
        laFileStru[lnFileStru,3] = 4
        laFileStru[lnFileStru,4] = 0
      ENDIF
      lnFileStru = ALEN(laFileStru,1)
      DIMENSION laFileStru[lnFileStru+1,18]
      lnFileStru = lnFileStru+1
      laFileStru[lnFileStru,1] = 'nLblsNo'
      laFileStru[lnFileStru,2] = 'N'
      laFileStru[lnFileStru,3] = 4
      laFileStru[lnFileStru,4] = 0
      FOR lncnt=7 TO 16
        FOR lnInc=1 TO lnFileStru
          STORE SPACE(0) TO laFileStru[lnInc,lnCnt],laFileStru[lnInc,lnCnt]
        ENDFOR 
      ENDFOR  
      FOR lnInc=1 TO lnFileStru    
        STORE 0 TO laFileStru[lnInc,17],laFileStru[lnInc,18]
      ENDFOR 
      =gfCrtTmp(lcLblTemp,@laFileStru,'ORDER+STORE',lcLblTemp,.F.)      

      *-- Fill the temprary file with report data
      IF !lfOrdData()
        RETURN
      ENDIF
  ENDCASE

  SELECT (lcLblTemp)
  SET RELATION TO 'O'+ORDER INTO ORDHDR, IIF(STORE=SPACE(8) ,;
          'M'+ACCOUNT, 'S'+ACCOUNT+STORE) INTO CUSTOMER



  *Create a new temporary table and insert records equal to number of labels
  lcMainF = loOgScroll.gfTempName()
  *HMA
  SELECT (lcLblTemp)
  =AFIELDS(laFileStru)
  lcTmpIndex=IIF(lcXTYPE = 'O','ORDER+STORE',IIF(lcXTYPE = 'I','INVOICE+STORE','PIKTKT'))
  =gfCrtTmp(lcMainF,@laFileStru,lcTmpIndex,lcMainF,.F.)
*  COPY STRUCTURE TO (oAriaApplication.WorkDir+lcMainF) WITH CDX
*  USE (oAriaApplication.WorkDir+lcMainF) IN 0 ORDER 1
  *HMA
  SCAN FOR !EOF('ORDHDR') .AND. LABELS # 0
    WAIT WINDOW LANG_Soshlb_Printing+' '+IIF(lcXType='O',LANG_Soshlb_Order+' '+ORDHDR.Order,IIF(lcXType='I',LANG_Soshlb_invoice+' '+Invoice,LANG_Soshlb_pick_ticket+' '+PIKTKT)) NOWAIT
    *-- Print Required number of labels
    *FOR lnc=1 TO &lcLblTemp..LABELS
      SCATTER MEMVAR
      *--store the print label no to m.nlblsno
      m.nLblsNo = 1
      *lnc
      INSERT INTO (lcMainF) FROM MEMVAR
    *ENDFOR
  ENDSCAN
  
  
  SET RELATION TO
  SELECT (lcMainF)
  SET RELATION TO 'O'+ORDER INTO ORDHDR, IIF(STORE=SPACE(8) ,;
        'M'+ACCOUNT, 'S'+ACCOUNT+STORE) INTO CUSTOMER
  *Add function to call optional program.
  lcCompName = ''
  *C2008835,1 WLD Using custom Programs 08/26/2007 [Begin]
  **=lfOptProg()
  PRIVATE lnOldAls
  STORE 0 TO lnOldAls
  STORE '' TO lcDetailF
  lnOldAls = SELECT(0)
  
  lcPrtAdr = 'W'
  
  lcDetailF =  loOgScroll.gfTempName()
  lnMjrLen   = LEN(gfItemMask("PM"))             && Style major length.
  lnColorLen = LEN(gfItemMask("PN"))
  gfOpenTable('Scale','Scale','SH')
  gfOpenTable('BOL_HDR','BOL_HDR','SH')
  gfOpenTable('BOL_LIN','BOL_LIN','SH')
  gfOpenTable('CODES','CODES','SH')
  gfOpenTable('PACK_HDR','PACK_HDR','SH')
  gfOpenTable('PACK_lIN','PACK_LIN','SH')
  gfOpenTable('INVLINE','INVLINE','SH')
  gfOpenTable('ORDLINE','ORDLINE','SH')
  gfOpenTable('piktkt','piktkt','SH')
 
  *B608331,1 MMT 10/29/2007 fix bugs of wrong case qty and error while preview[Start]
  gfOpenTable('STYLE','STYLE','SH')
  gfOpenTable('INVHDR','INVHDR','SH')
  gfOpenTable('CONSINVH','CONSINVH','SH')
  gfOpenTable('CONSINVL','CONSINVL','SH')
  *B608331,1 MMT 10/29/2007 fix bugs of wrong case qty and error while preview[End]
  
  = lfCrtTemp()
  = lfGetData()

  loOGScroll.cCROrientation='L'

  *SELECT (lnOldAls)
  SELECT (lcDetailF)
  *C2008835,1 WLD Using custom Programs 08/26/2007 [End]
*ENDIF   &&If Filter Changed



=gfDispRe(lcFormName,'LABELS # 0',.F.,'L',.T.)

*--Close tables and erase temproray files
*HMA
*!*  IF USED(lcMainF)
*!*    USE IN (lcMainF)
*!*  ENDIF
*!*  ERASE (gcWorkDir+lcMainF+'.DBF')
*!*  ERASE (gcWorkDir+lcMainF+'.CDX')
*HMA

*!*************************************************************
*! Name      : lfPikData
*! Developer : Heba Mohamed Amin(HMA)
*! Date      : 05/23/2005
*! Purpose   : Collect the PikTkt data for the report
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfModalGen
*!*************************************************************
*! Called from : SOSHLB.PRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfPikData()
*!*************************************************************
FUNCTION lfPikData
*-- Don't collect PIKTKTs with status "Canceled"
loOgScroll.lcRpExp = loOgScroll.lcRpExp + IIF(EMPTY(loOgScroll.lcRpExp),'',' .AND. ') + "PIKTKT.STATUS # 'X'"
SELECT PIKTKT
SET RELATION TO PIKTKT INTO (lclblTbl) ADDITIVE
lcRpExp = loOgScroll.lcRpExp
SCAN FOR &lcRpExp .AND. &lclblTbl..LABELS > 0
  WAIT WINDOW LANG_Soshlb_Coll_Pick + PikTkt NOWAIT
  SCATTER MEMVAR
  M.LABELS = &lclblTbl..LABELS
  INSERT INTO (lcLblTemp) FROM MEMVAR
ENDSCAN

IF RECCOUNT(lcLblTemp) = 0
  *-- Message 'There are no records to display...!'
  =gfModalGen('TRM00052B00000','DIALOG')
  RETURN .F.
ENDIF
*-- End of lfPikData.

*!*************************************************************
*! Name      : lfInvData
*! Developer : Heba Mohamed Amin(HMA)
*! Date      : 05/24/2005
*! Purpose   : Collect the invoice data for the report
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfModalGen
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfInvData()
*!*************************************************************
FUNCTION lfInvData

*-- Don't collect voided invoices
loOgScroll.lcRpExp = loOgScroll.lcRpExp + IIF(EMPTY(loOgScroll.lcRpExp),'',' .AND. ') + "INVHDR.STATUS # 'V'"
SET ORDER TO ORDHDR IN ORDHDR
SELECT INVHDR
SET RELATION TO INVOICE INTO (lclblTbl) ADDITIVE
SET RELATION TO 'O'+ORDER INTO ORDHDR ADDITIVE
lcRpExp= loOgScroll.lcRpExp
LOCATE ALL FOR &lcRpExp .AND. !EOF('ORDHDR')
IF EOF()
  *-- Message 'There are no records to display...!'
  =gfModalGen('TRM00052B00000','DIALOG')
  RETURN .F.
ENDIF
SCAN FOR &lcRpExp .AND. !EOF('ORDHDR')

  *B608331,1 MMT 10/29/2007 fix bugs of wrong case qty and error while preview[Start]
  IF INVHDR.Status = 'V'
    LOOP 
  ENDIF 
  *B608331,1 MMT 10/29/2007 fix bugs of wrong case qty and error while preview[End]
  
  WAIT WINDOW LANG_Soshlb_Coll_Invoice + Invoice NOWAIT
  SCATTER MEMVAR
  *-- In consolidated invoice, Add record with the required labels for 
  *-- each store
  IF INVHDR.CONSOL = 'Y' .AND. SEEK(INVHDR.INVOICE,'CONSINVH')
    SELECT CONSINVH
    m.Store = CONSINVH.STORE    
    lnLab = 0
    SCAN WHILE INVOICE = INVHDR.INVOICE
      IF m.STORE # CONSINVH.STORE
        m.LabELS = lnLab
        INSERT INTO (lcLblTemp) FROM MEMVAR      
        m.Store  = CONSINVH.Store
        lnLab    = CONSINVH.CARTONS
      ELSE
        lnLab = lnLab + CONSINVH.CARTONS
      ENDIF
    ENDSCAN
    m.Labels = lnLab
  ELSE
    M.LABELS = &lclblTbl..LABELS
  ENDIF
  IF M.LABELS > 0
    INSERT INTO (lcLblTemp) FROM MEMVAR
  ENDIF
ENDSCAN
SET RELATION TO

IF EOF(lcLblTemp)
  *-- Message 'There are no records to display...!'
  =gfModalGen('TRM00052B00000','DIALOG')
  RETURN .F.
ENDIF
*-- End of lfInvData.

*!*************************************************************
*! Name      : lfOrdData
*! Developer : Heba Mohamed Amin(HMA)
*! Date      : 05/24/2005
*! Purpose   : Collect Order data for the report
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfModalGen
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfOrdData()
*!*************************************************************
FUNCTION lfOrdData

*-- Don't collect orders with status "CANCELED"
loOgScroll.lcRpExp = loOgScroll.lcRpExp + IIF(EMPTY(loOgScroll.lcRpExp),'',' .AND. ') + "ORDHDR.STATUS # 'X'"
IF !EOF(lcLblTemp)
  SELECT lcLblTemp
  ZAP
ENDIF
SELECT ORDLINE
SET RELATION TO STYLE INTO STYLE ADDITIVE

SELECT ORDHDR
*-- Set relation to that temporary file to get the default number of labels
SET RELATION TO ORDER INTO (lclblTbl)
lcRpExp =loOgScroll.lcRpExp
LOCATE ALL FOR &lcRpExp .AND. &lclblTbl..LABELS > 0

IF EOF()
  *Message 'There are no records to display...!'
  =gfModalGen('TRM00052B00000','DIALOG')
  RETURN .F.
ENDIF

SCAN FOR &lcRpExp .AND. &lclblTbl..LABELS > 0
  WAIT WINDOW LANG_Soshlb_Coll_Order + Order NOWAIT
  SCATTER MEMVAR
  M.LABELS = &lclblTbl..LABELS
  IF ORDHDR.Multi # 'Y'
    INSERT INTO (lcLblTemp) FROM MEMVAR
  ELSE
    *-- If multi store order, insert a record in lcLblTemp file for each 
    *-- store to print required labels for each store
    SELECT ORDLINE
    IF SEEK(ORDHDR.CORDTYPE+ORDHDR.ORDER)
      m.CustPO = ORDLINE.CustPO     &&Print ORDLINE.CustPO in case of multi PO.
      m.Store = ORDLINE.STORE
      lnLab = 0
      SCAN WHILE ORDER = ORDHDR.ORDER
        IF m.STORE # ORDLINE.STORE 
          m.Labels = lnLab
          INSERT INTO (lcLblTemp) FROM MEMVAR
          m.STORE = ORDLINE.STORE 
          m.CustPO = ORDLINE.CustPO     &&Print ORDLINE.CustPO in case of multi PO.
          lnLab = IIF(STYLE.Qty_Ctn=0,1,CEILING(ORDLINE.TotQty/STYLE.Qty_Ctn))
        ELSE
          lnLab = lnLab + IIF(STYLE.Qty_Ctn=0,1,CEILING(ORDLINE.TotQty/STYLE.Qty_Ctn))
        ENDIF
      ENDSCAN
      m.Labels = lnLab
      INSERT INTO (lcLblTemp) FROM MEMVAR
    ENDIF
  ENDIF
ENDSCAN
SET RELATION TO

SELECT ORDLINE
SET RELATION TO
*-- End of lfOrdData.                  

*!*************************************************************
*! Name      : lfwOGWhen
*! Developer : Heba Mohamed Amin(HMA)
*! Date      : 05/22/2005
*! Purpose   : The when function of the option grid
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfTempName()
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfwOGWhen()
*!*************************************************************
FUNCTION lfwOGWhen

llcbDspDlg=.T.
*-- lclblTbl Variable defined by the option grid takes temporary name
DO CASE

  *-- Current module is Sales Order
  CASE oAriaApplication.ActiveModuleID ='SO'
    lcXType = 'O'
    DIMENSION laTempLabel[2,18] 

    laTempLabel[1,1]="ORDER"
    laTempLabel[1,2]="C"
    laTempLabel[1,3]=6
    laTempLabel[1,4]=0

    laTempLabel[2,1]="LABELS"
    laTempLabel[2,2]="N"
    laTempLabel[2,3]=4
    laTempLabel[2,4]=0
    FOR lncnt=7 TO 16
      STORE SPACE(0) TO laTempLabel[1,lnCnt],laTempLabel[2,lnCnt]
    ENDFOR  
    STORE 0 TO laTempLabel[1,17],laTempLabel[1,18],laTempLabel[2,17],laTempLabel[2,18]
    =gfCrtTmp(lclblTbl,@laTempLabel,'ORDER',lclblTbl,.F.)
    SET ORDER TO ORDLINE IN ORDLINE
    SET ORDER TO STYLE IN STYLE
    SET ORDER TO ORDHDR IN ORDHDR

  *-- Current module is Account Receivable
  CASE oAriaApplication.ActiveModuleID ='AR'
    lcXType = 'I'
    DIMENSION laTempLabel[2,18] 

    laTempLabel[1,1]="INVOICE"
    laTempLabel[1,2]="C"
    laTempLabel[1,3]=6
    laTempLabel[1,4]=0

    laTempLabel[2,1]="LABELS"
    laTempLabel[2,2]="N"
    laTempLabel[2,3]=4
    laTempLabel[2,4]=0
    FOR lncnt=7 TO 16
      STORE SPACE(0) TO laTempLabel[1,lnCnt],laTempLabel[2,lnCnt]
    ENDFOR  
    STORE 0 TO laTempLabel[1,17],laTempLabel[1,18],laTempLabel[2,17],laTempLabel[2,18]
    =gfCrtTmp(lclblTbl,@laTempLabel,'INVOICE',lclblTbl,.F.)
    *=gfDoTriger('SOSHLB',PADR('CREATEFILE',10))
    SET ORDER TO INVHDR IN INVHDR
        
  *-- Current module is Sales order Allocation
  CASE oAriaApplication.ActiveModuleID='AL'
    lcXType = 'P'
    DIMENSION laTempLabel[3,18] 

    laTempLabel[1,1]="PIKTKT"
    laTempLabel[1,2]="C"
    laTempLabel[1,3]=6
    laTempLabel[1,4]=0

    laTempLabel[2,1]="ORDER"
    laTempLabel[2,2]="C"
    laTempLabel[2,3]=6
    laTempLabel[2,4]=0
    
    laTempLabel[3,1]="LABELS"
    laTempLabel[3,2]="N"
    laTempLabel[3,3]=4
    laTempLabel[3,4]=0
    FOR lncnt=7 TO 16
      STORE SPACE(0) TO laTempLabel[1,lnCnt],laTempLabel[2,lnCnt],laTempLabel[3,lnCnt]
    ENDFOR  
    STORE 0 TO laTempLabel[1,17],laTempLabel[1,18],laTempLabel[2,17],laTempLabel[2,18],;
               laTempLabel[3,17],laTempLabel[3,18]
    =gfCrtTmp(lclblTbl,@laTempLabel,'PIKTKT',lclblTbl,.F.)
    SET ORDER TO ORDLINE IN ORDLINE
    SET ORDER TO PIKLINE IN PIKLINE
    SET ORDER TO STYLE   IN STYLE
ENDCASE
*-- End of lfwOGWhen.

*!*************************************************************
*! Name      : lfSRVLBL
*! Developer : Heba Mohamed Amin(HMA)
*! Date      : 05/22/2005
*! Purpose   : control browsing (Order , Invoice or PikTkt) and validate 
*!           : selecting it in inlist function.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfModalGen
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfSRVLBL()
*!*************************************************************
*! Note      : SRV symbol is [S,Set--R,Reset--V,Valid]
*!*************************************************************
FUNCTION lfSRVLBL
PARAMETERS lcParm

PRIVATE lcAlias,llHaveSty

DO CASE
  CASE lcParm = 'S'  && Set code
    DO CASE 
      CASE lcxType = 'O'    &&transaction is Orders
        SELECT ORDHDR
        *-- Set relation to that temporary file to get the default number of labels
        SET RELATION TO ORDER INTO (lclblTbl)
        
        *-- Set relation to customer to get the account address in the INRANGE browse
        SET RELATION TO IIF(EMPTY(STORE),'M'+ACCOUNT,'S'+ACCOUNT+STORE) ;
                INTO CUSTOMER ADDITIVE

      CASE lcxType = 'I'    &&transaction is invoices
        SELECT INVHDR
        SET RELATION TO IIF(EMPTY(STORE),'M'+ACCOUNT,'S'+ACCOUNT+STORE) ;
                INTO CUSTOMER ADDITIVE
        
        SET RELATION TO INVOICE INTO (lclblTbl) ADDITIVE
        
      CASE lcxType = 'P'    &&transaction is PikTkt
        SET ORDER TO TAG CUSTOMER IN CUSTOMER
        SELECT PIKTKT
        SET RELATION TO 'O' + Order INTO ORDHDR
        SET RELATION TO IIF(EMPTY(Store) , 'M' + Account ,;
                        'S' + Account + Store) INTO CUSTOMER ADDITIVE
        SET RELATION TO PIKTKT INTO (lclblTbl) ADDITIVE
    ENDCASE
    
  CASE lcParm = 'R'  && Reset code
    DO CASE 
      CASE lcxType = 'O'
        SELECT ORDHDR
        SET RELATION TO
      CASE lcxType = 'I'
        SELECT INVHDR
        SET RELATION TO
      CASE lcxType = 'P'
        SELECT PIKTKT
        SET RELATION TO 
    ENDCASE

  OTHERWISE      && Valid code
    
    =!lfLblFound() AND lfNoOfLbls()
    DO CASE 
     
      CASE (lcxType = 'O' .OR. lcxType = 'P' ) AND !llcbDspDlg
        lnLabels = EVAL(lclblTbl+'.Labels')
        lcTrans=IIF(lcxType = 'O',&lcLblTbl..Order,&lcLblTbl..PikTkt)
        *-- Call the screen to edit the number of labels
       && DO FORM (oAriaApplication.ReportHome+"SOSHLB.SCX") WITH lnLabels  
        *IF lnLabels <> lnLabelNo
         REPLACE &lcLblTbl..LABELS WITH 1
        *ENDIF 

      CASE lcxType = 'I'  
        *-- Calculate required number of lables, however don't allow user to edit.
        SET ORDER TO CONSINVH IN CONSINVH
        lnCurAls = SELECT(0)
        lcKey = KEY()
        lcExpr = INVHDR.INVOICE
        IF INVHDR.CONSOL # 'Y'
          SELECT (lclblTbl)
          REPLACE LABELS WITH INVHDR.CARTONS
          lnLab = INVHDR.CARTONS
        ELSE
          *-- If consolidated invoice, calculate number of labels from CONSINVH
          SELECT CONSINVH
          IF SEEK(lcExpr,'CONSINVH')
            lnLab = 0
            SCAN WHILE INVOICE = INVHDR.INVOICE
                lnLab = lnLab + CONSINVH.CARTONS
            ENDSCAN
          ELSE
            lnLab = INVHDR.CARTONS
          ENDIF
          SELECT (lclblTbl)
          IF SEEK(lcExpr)
            REPLACE LABELS WITH lnLab
          ENDIF
        ENDIF
*        IF gfDoTriger('SOSHLB',PADR('LABELSINFO',10))
        lcTrans=&lcLblTbl..Invoice
        *-- Call the screen to edit the number of labels
      *  DO FORM (oAriaApplication.ReportHome+"SOSHLB.SCX") WITH lnLab
        SELECT (lclblTbl)
        *-- Update table with the new number of labels entered by user.
       * IF lnLab <> lnLabelNo
         REPLACE &lcLblTbl..LABELS WITH 1
       * ENDIF
        SELECT (lnCurAls)
        *=SEEK(lcKey)
    ENDCASE
ENDCASE

*!*************************************************************
*! Name      : lfLblFound
*! Developer : Heba Mohamed Amin(HMA)
*! Date      : 05/22/2005
*! Purpose   : Check the Existance Of the No. Of Label In the temp File.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Return    : .T. if label found else .F.
*!*************************************************************
*! Example   : =lfLblFound()
*!*************************************************************

FUNCTION lfLblFound
PRIVATE lcToSeek
lnselected=SELECT()
lnRecNo=RECNO()
lcToSeek = IIF(oAriaApplication.ActiveModuleID ='SO',[ORDHDR.ORDER],IIF(oAriaApplication.ActiveModuleID ='AR',[INVHDR.INVOICE],;
                               [PIKTKT.PIKTKT]))
GOTO lnRecNo
RETURN SEEK(&lcToSeek,lclblTbl)
*-- end of lfLblFound.

*!*************************************************************
*! Name      : lfNoOfLbls
*! Developer : Heba Mohamed Amin(HMA)
*! Date      : 05/22/2005
*! Purpose   : Calculate Number of labels in all cases [AR-AL-SO]
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Return    : .T. or .F.
*!*************************************************************
*! Example   : =lfNoOfLbls()
*!*************************************************************

FUNCTION lfNoOfLbls
PRIVATE lnCurrAls , lnNoOfLbls
lnRecNo=RECNO()
lnNoOfLbls = 0
lnCurrAls = SELECT(0)  && Save Active Alias

DO CASE
  *-- Current module is Sales Order
  CASE oAriaApplication.ActiveModuleID='SO'
    IF SEEK(OrdHdr.cOrdType+OrdHdr.Order,"ORDLINE")
      SELECT ORDLINE
      SCAN REST WHILE cordtype+order+STR(lineno,6) = OrdHdr.cOrdType+OrdHdr.Order
        lnNoOfLbls = lfCalcrtns(lnNoOfLbls)
      ENDSCAN
    ENDIF
    INSERT INTO (lclblTbl) (Order, Labels) VALUES (OrdHdr.Order,lnNoOfLbls)

  *-- Current module is Account Receivable
  CASE oAriaApplication.ActiveModuleID='AR'
    IF InvHdr.STATUS # 'V'
      INSERT INTO (lclblTbl) (INVOICE) VALUES (INVHDR.INVOICE)
    ENDIF

  *-- Current module is Allocation
  CASE oAriaApplication.ActiveModuleID='AL'
    *-- if PikTkt is not cancelled.
    IF PIKTKT.STATUS # "X"
      IF PIKTKT.STATUS = "C" AND SEEK(PIKTKT.PIKTKT,"PIKLINE")
         lcScanCond  = PikTkt.PikTkt + PikTkt.Order
         SELECT PIKLINE
         SCAN REST WHILE piktkt+order+STR(lineno,6) = lcScanCond
           lnNoOfLbls = lfCalcrtns(lnNoOfLbls)
         ENDSCAN
      ENDIF
      IF PIKTKT.STATUS $ 'OH' AND SEEK("O"+PikTkt.Order,"ORDLINE")
        SELECT ORDLINE
        SCAN REST WHILE cOrdType+ORDER+STR(LineNo,6) = 'O'+PIKTKT.ORDER
          IF PICKED AND PIKTKT = PIKTKT.PIKTKT
            lnNoOfLbls = lfCalcrtns(lnNoOfLbls)
          ENDIF
        ENDSCAN  
      ENDIF
    
    ENDIF  && end if PikTkt is not cancelled.
    INSERT INTO (lclblTbl) (PIKTKT,Order,LABELS) VALUES ;
                           (PIKTKT.PIKTKT,PIKTKT.ORDER,lnNoOfLbls)
    
ENDCASE
lnRecNo=RECNO()
SELECT (lnCurrAls)
*-- end of lfNoOfLbls.

*!*************************************************************
*! Name      : lfCalcrtns
*! Developer : Heba Mohamed Amin(HMA)
*! Date      : 05/23/2005
*! Purpose   : General formula to Calculate Number of Cartons.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Return    : No. Of Cartons.
*!*************************************************************
*! Example   : =lfCalcrtns(No. of labels)
*!*************************************************************
FUNCTION lfCalcrtns
PARAMETERS lnCalcrtns
PRIVATE lnCalcrtns
=SEEK(Style,"STYLE")
RETURN lnCalcrtns + IIF(STYLE.Qty_Ctn=0,1,CEILING(TotQty/STYLE.Qty_Ctn))
*-- end of lfCalcrtns.

*!*************************************************************
*! Name      : lfGetAddr
*! Developer : Heba Mohamed Amin(HMA)
*! Date      : 05/29/2005
*! Purpose   : Get warehouse and ship to addresses
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Label
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfGetAddr()
*!*************************************************************
FUNCTION lfGetAddr
lnAlias = SELECT(0)
lcAcct   = ORDHDR.Account+IIF(EMPTY(STORE),'',LANG_Soshlb_Store+Store)
lcDivLName = ''
DIMENSION laDivLName[1,2]
laDivLName[1,1] = 'DIVLNAME'     && Array to get the Division long name
laDivLName[1,2] = 'lcDivLName'
=gfRltFld(ORDHDR.cDivision , @laDivLName , 'CDIVISION')
lcCompName = IIF(!EMPTY(lcDivLName),lcDivLName,SUBSTR(gcCom_Name,1,29))
IF ORDHDR.ALT_SHPTO
  lcActNam = ORDHDR.STNAME
  lcAddr1  = ORDHDR.cAddress1
  lcAddr2  = ORDHDR.cAddress2
  lcAddr3  = ORDHDR.cAddress3
ELSE
  IF !EMPTY(CUSTOMER.DIST_CTR) .AND. SEEK('S'+CUSTOMER.ACCOUNT+CUSTOMER.DIST_CTR,LCCUST_A)
    lcActNam = IIF(EMPTY(&LCCUST_A..DBA),&LCCUST_A..STNAME,&LCCUST_A..DBA)
  ELSE
    lcActNam = IIF(EMPTY(CUSTOMER.DBA),CUSTOMER.STNAME,CUSTOMER.DBA)    
  ENDIF
  IF EMPTY(CUSTOMER.DIST_CTR)
    SELECT CUSTOMER
    lcAddr1 = gfGetAdr('CUSTOMER' , '' , '' , '' , 1)
    lcAddr2 = gfGetAdr('CUSTOMER' , '' , '' , '' , 2)
    lcAddr3 = gfGetAdr('CUSTOMER' , '' , '' , '' , 3)
  ELSE
    SELECT (LCCUST_A)
    lcAddr1 = gfGetAdr(LCCUST_A,'' , '' , '' , 1)
    lcAddr2 = gfGetAdr(LCCUST_A,'' , '' , '' , 2)
    lcAddr3 = gfGetAdr(LCCUST_A,'' , '' , '' , 3)
  ENDIF
ENDIF
DECLARE lasort[3]
FOR lnI = 1 TO 3
  lcI = ALLTRIM(STR(lnI))
  lasort[lnI] = lcAddr&lcI
ENDFOR
FOR lnI = 1 TO 2
  FOR lnC = 1 to 2
    IF Empty(laSort[lnC])
      laSort[lnC] = laSort[lnC+1]
      laSort[lnC+1] = ' '
    ENDIF
  ENDFOR
ENDFOR
FOR lnI = 1 TO 3
  lcI = ALLTRIM(STR(lnI))
  lcAddr&lcI =  lasort[lnI] 
ENDFOR
IF lcPrtAdr = 'W'
  DO CASE
    CASE lcXTYPE = 'P' .OR. lcXTYPE = 'I'
      =SEEK(&lcMainF..cWareCode,'WAREHOUS')
    CASE lcXTYPE = 'O'
      =SEEK(ORDHDR.cWareCode,'WAREHOUS')
  ENDCASE
  *-- Get the company addresses
  lcCmAdr1 = gfGetAdr('WAREHOUS' , '' , '' , '' , 1)
  lcCmAdr2 = gfGetAdr('WAREHOUS' , '' , '' , '' , 2)
  lcCmAdr3 = gfGetAdr('WAREHOUS' , '' , '' , '' , 3)
  lcCmAdr4 = gfGetAdr('WAREHOUS' , '' , '' , '' , 4)
  lcCmAdr5 = gfGetAdr('WAREHOUS' , '' , '' , '' , 5)
ENDIF
SELECT (lnAlias)
RETURN ''
*!**************************************************************************
*! Name      : lfBringStr
*! Developer : Heba Mohamed Amin(HMA)
*! Date      : 05/20/2005
*! Purpose   : Function to collect (Department, Cust Po, Store and ShipVia) 
*!           : as one string
*!           : to Print In Shipping Labels.
*!**************************************************************************
*! Called from : Label
*!**************************************************************************
*! Calls       : .......
*!**************************************************************************
*! Returns     : None
*!**************************************************************************
*! Example     : =lfBringStr()
*!**************************************************************************
FUNCTION lfBringStr
PARAMETER lcDummy
PRIVATE lcShipStr,lcPoStr

lcStr = ""
IF lcXTYPE = 'I'

  = SEEK(Invoice,"INVHDR")
  lcShipStr = gfCodDes(InvHdr.ShipVia,'SHIPVIA',.T.)
  lcStr = INVHDR.DEPT + '     ' + INVHDR.CUSTPO + SPACE(5) + STORE + " " 

ELSE

  IF "*" $ OrdHdr.ShipVia
    lcShipStr = Customer.ShipVia
  ELSE
    lcShipStr = OrdHdr.ShipVia
  ENDIF
  lcShipStr = gfCodDes(lcShipStr,'SHIPVIA',.T.)

  IF OrdHdr.MultiPo
    lcPoStr = LANG_Soshlb_MultiPo
  ELSE
    lcPoStr = ORDHDR.CUSTPO  
  ENDIF
  lcStr = ORDHDR.DEPT + '     ' + lcPoStr + SPACE(6)

ENDIF
lcStr = lcStr + lcShipStr
RETURN ""
*-- end of lfBringStr

*!*************************************************************
*! Name      : gfDoTriger
*! Developer : HS (Haytham El_Sheltawi)
*! Date      : 07/22/99
*! Purpose   : Function to control any triggers found in the
*!             triggers file, customized processes and workflow
*!             server requests.
*!*************************************************************
*! Calls              : None.
*!*************************************************************
*! Passed Parameters  : 1) lcProgName, Object ID.
*!                      2) lcEvent, Event ID.
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example            :  =gfDoTriger()
*!*************************************************************
*E301297,1 this function was added by HS for the entry E301297,1.
*!*************************************************************
*
FUNCTION gfDoTriger

PARAMETERS lcProgName , lcEvent

PRIVATE lnOldAlias , lcProgToDo , laParamExp , laParam , lcParmStr ,;
        lnCount    , llReturn

llReturn = .T.

*-- If any of the parameters is not passed or passed incorrectly 
IF TYPE('lcProgName') <> 'C' .OR. EMPTY(lcProgName) .OR.;
   TYPE('lcEvent') <> 'C' .OR. EMPTY(lcEvent)
  RETURN
ENDIF

*-- Save the old alias
lnOldAlias = SELECT(0)

*-- Open the Trigger file if it was not opened
IF !USED('SYCTRIGG')
  =gfOpenFile(gcSysHome + 'SYCTRIGG' , 'OBJEVENT' , 'SH')
ENDIF

SELECT SYCTRIGG

*-- If there is triggers for this Object/Event
IF SEEK(PADR(lcProgName , 10) + PADR(lcEvent , 10))
  
  *-- Scan loop to scan the Object/Event triggers
  SCAN REST;
      WHILE cAPObjNam + cEvent_ID = PADR(lcProgName , 10) +;
            PADR(lcEvent , 10)
    
    *-- Get the name of the program that should be executed
    lcProgToDo = cTrig_ID
    
    *-- Initialize the parameter string variable
    lcParmStr  = ''
    
    *-- Restore the old alias to be able to evaluate the parameter
    *-- expressions properly
    SELECT (lnOldAlias)
    
    *-- If there is one or more parameters that should be passed to the
    *-- program
    IF !EMPTY(SYCTRIGG.mParmExpr)
      
      *-- Get the parameter expressions
      DIMENSION laParamExp[OCCURS('~' , SYCTRIGG.mParmExpr) + 1]
      =gfSubStr(SYCTRIGG.mParmExpr , @laParamExp , '~')
      
      *-- Initialize the parameters array
      DIMENSION laParam[ALEN(laParamExp , 1)]
      laParam = ""
      
      *-- Get the parameters values that will be passed to the program
      FOR lnCount = 1 TO ALEN(laParamExp , 1)
        laParam[lnCount] = EVALUATE(laParamExp[lnCount])
        lcParmStr = lcParmStr + IIF(lnCount = 1 , '' , ' , ') +;
                    'laParam[' + ALLTRIM(STR(lnCount)) + ']'
        
      ENDFOR    && End of FOR lnCount = 1 TO ALEN(laParamExp , 1)
    ENDIF    && End of IF !EMPTY(SYCTRIGG.mParmExpr)
    
    *-- If custom process
    IF SYCTRIGG.cActvTyp = 'C'
      *-- Call the program and get the returned value
      llReturn = &lcProgToDo(&lcParmStr)
    ELSE    && Else [If Server request]
    ENDIF    && End of IF SYCTRIGG.cActvTyp = 'C'
  
    SELECT SYCTRIGG
  ENDSCAN    && End of SCAN REST WHILE cAPObjNam + cEvent_ID = ...
  
  *B603662,1 BWA 05/25/2000 In case the process doesn't exist.[START]
ELSE
  llReturn = .F.
  *B603662,1 [END]
  
ENDIF    && End of IF SEEK(PADR(lcProgName , 10) + PADR(lcEvent , 10))

*-- Restore the old alias
SELECT (lnOldAlias)

RETURN (llReturn)
  

*!*************************************************************
*! Name      : lfInit
*! Developer : Heba Amin
*! Date      : 05/31/2005
*! Purpose   : Init Function of the screen
*!*************************************************************
FUNCTION lfInit
PARAMETERS loFormSet
DO CASE 
  CASE lcXType = 'O'
    lcCaption=LANG_Soshlb_Order+' # '+lcTrans+ LANG_Soshlb_Labels    
  CASE lcXType = 'P'
    lcCaption=LANG_Soshlb_pick_ticket+' '+lcTrans   
  CASE lcXType = 'I'
    lcCaption= LANG_Soshlb_Invoice +' # '+lcTrans+ LANG_Soshlb_Labels       
    loFormSet.AriaForm1.check1.visible=.F.
    loFormSet.AriaForm1.Label2.visible=.F.
ENDCASE 
loFormSet.AriaForm1.Caption = lcCaption

*!*************************************************************
*! Name      : lfEditLabelNo
*! Developer : Heba Amin
*! Date      : 05/31/2005
*! Purpose   : Code Executed when click on Ok Button Of the Screen
*!*************************************************************
FUNCTION lfEditLabelNo
PARAMETERS loFormSet
lnLabelNo = loFormSet.AriaForm1.Text1.value
RETURN lnLabelNo

*!*************************************************************
*! Name      : lfEmpty
*! Developer : Heba Amin
*! Date      : 05/31/2005
*! Purpose   : Code Executed when click on Cancel Button Of the Screen
*!*************************************************************
FUNCTION lfEmpty
PARAMETERS loFormSet
lnLabelNo = IIF(lcXType = 'I',lnLab,lnLabels)

*!*************************************************************
*! Name      : lfDisplay
*! Developer : Heba Amin
*! Date      : 05/31/2005
*! Purpose   : Don't Show Function of the screen
*!*************************************************************
FUNCTION lfDisplay
PARAMETERS loFormSet
llcbDspDlg =IIF(loFormSet.ariaForm1.check1.Value=1,.T.,.F.)
*/*/*/*/*/*/*/*//*/*//**/*/*//*//*/*/*/*/*/*////////////////////////////////////////////////
*////////////////// W A L E E D    
*!*************************************************************
*! Name      : lfCrtTemp
*! Developer : Walid Hamed (WLD)
*! Date      : 08/21/2007
*! Purpose   : Create Temp File to collect Data
*!*************************************************************
*! Called from : Soshlbbc.prg
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfCrtTemp()
*!*************************************************************
FUNCTION lfCrtTemp

  *B608331,3 MMT 11/25/2007 fix bugs of prinitng the Same style Carton twice[Start]
  *DIMENSION laFileStru[30,4]
  *B608331,4 MMT 11/28/2007 fix bugs of prinitng the Same style Carton twice[Start]      
  *DIMENSION laFileStru[31,4]
  *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][Start]
  *DIMENSION laFileStru[32,4]
  DIMENSION laFileStru[34,4]
  *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][End]
  *B608331,4 MMT 11/28/2007 fix bugs of prinitng the Same style Carton twice[End]      
  *B608331,3 MMT 11/25/2007 fix bugs of prinitng the Same style Carton twice[End]

  laFileStru[ 1 , 1 ] = 'PIKTKT'
  laFileStru[ 1 , 2 ] = 'C'
  laFileStru[ 1 , 3 ] = 6
  laFileStru[ 1 , 4 ] = 0

  laFileStru[ 2 , 1 ] = 'ORDER'
  laFileStru[ 2 , 2 ] = 'C'
  laFileStru[ 2 , 3 ] = 6
  laFileStru[ 2 , 4 ] = 0

  laFileStru[ 3 , 1 ] = 'Invoice'
  laFileStru[ 3 , 2 ] = 'C'
  laFileStru[ 3 , 3 ] = 6
  laFileStru[ 3 , 4 ] = 0

  laFileStru[ 4 , 1 ] = 'Store'
  laFileStru[ 4 , 2 ] = 'C'
  laFileStru[ 4 , 3 ] = 8
  laFileStru[ 4 , 4 ] = 0


  laFileStru[ 5 , 1 ] = 'LINENO'
  laFileStru[ 5 , 2 ] = 'N'
  laFileStru[ 5 , 3 ] = 6
  laFileStru[ 5 , 4 ] = 0

  laFileStru[ 6 , 1 ] = 'STYLE'
  laFileStru[ 6 , 2 ] = 'C'
  laFileStru[ 6 , 3 ] = lnMjrLen
  laFileStru[ 6 , 4 ] = 0

  laFileStru[ 7 , 1 ] = 'COLOR'
  laFileStru[ 7 , 2 ] = 'C'
  laFileStru[ 7 , 3 ] = 30
  laFileStru[ 7 , 4 ] = 0

  FOR I=1 TO 8
    laFileStru[ I+7, 1 ] = 'SIZ' + STR(I,1)
    laFileStru[ I+7 , 2 ] = 'C'
    laFileStru[ I+7 , 3 ] = 5
    laFileStru[ I+7 , 4 ] = 0
  ENDFOR
  FOR I=1 TO 8
    laFileStru[ I+15 , 1 ] = 'RTO' + STR(I,1)
    laFileStru[ I+15 , 2 ] = 'N'
    laFileStru[ I+15 , 3 ] = 6
    laFileStru[ I+15 , 4 ] = 0
  ENDFOR

  laFileStru[ 24 , 1 ] = 'CaseQty'
  laFileStru[ 24 , 2 ] = 'N'
  laFileStru[ 24 , 3 ] = 8
  laFileStru[ 24 , 4 ] = 0

  laFileStru[ 25 , 1 ] = 'Carrier'
  laFileStru[ 25, 2 ] = 'C'
  laFileStru[ 25 , 3 ] = 40
  laFileStru[ 25 , 4 ] = 0

  laFileStru[ 26 , 1 ] = 'Bol_No'
  laFileStru[ 26 , 2 ] = 'C'
  laFileStru[ 26 , 3 ] = 17
  laFileStru[ 26 , 4 ] = 0

  laFileStru[ 27 , 1 ] = 'nLblSNo'
  laFileStru[ 27 , 2 ] = 'N'
  laFileStru[ 27 , 3 ] = 4
  laFileStru[ 27 , 4 ] = 0

  laFileStru[ 28 , 1 ] = 'No_Cart'
  laFileStru[ 28 , 2 ] = 'N'
  laFileStru[ 28 , 3 ] = 4
  laFileStru[ 28 , 4 ] = 0

  laFileStru[ 29 , 1 ] = 'Tot_Cart'
  laFileStru[ 29 , 2 ] = 'N'
  laFileStru[ 29 , 3 ] = 4
  laFileStru[ 29 , 4 ] = 0

  laFileStru[ 30 , 1 ] = 'Account'
  laFileStru[ 30 , 2 ] = 'C'
  laFileStru[ 30 , 3 ] = 5
  laFileStru[ 30 , 4 ] = 0

  *B608331,3 MMT 11/25/2007 fix bugs of prinitng the Same style Carton twice[Start]
  laFileStru[ 31 , 1 ] = 'FStyle'
  laFileStru[ 31 , 2 ] = 'C'
  laFileStru[ 31 , 3 ] = 19
  laFileStru[ 31 , 4 ] = 0
  *B608331,3 MMT 11/25/2007 fix bugs of prinitng the Same style Carton twice[End]
  
  
  
  *B608331,4 MMT 11/28/2007 fix bugs of prinitng the Same style Carton twice[Start]      
  laFileStru[ 32 , 1 ] = 'llMixed'
  laFileStru[ 32 , 2 ] = 'L'
  laFileStru[ 32 , 3 ] = 1
  laFileStru[ 32 , 4 ] = 0

  *B608331,4 MMT 11/28/2007 fix bugs of prinitng the Same style Carton twice[End]      

  *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][Start]
  laFileStru[ 33 , 1 ] = 'cMadein'
  laFileStru[ 33 , 2 ] = 'C'
  laFileStru[ 33 , 3 ] = 30
  laFileStru[ 33 , 4 ] = 0
  
  laFileStru[ 34 , 1 ] = 'CaseWght'
  laFileStru[ 34 , 2 ] = 'N'
  laFileStru[ 34 , 3 ] = 13
  laFileStru[ 34 , 4 ] = 2
  *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][End]

  lcTmpIndex= IIF(lcXTYPE = 'O','ORDER+STORE',IIF(lcXTYPE = 'I','INVOICE+STORE','PIKTKT'))
  =gfCrtTmp(lcDetailF,@laFileStru,lcTmpIndex,lcDetailF,.F.)
  
  *B608331,3 MMT 11/25/2007 fix bugs of prinitng the Same style Carton twice[Start]
  SELECT(lcDetailF)
  INDEX on PIKTKT+STR(NO_CART,4)+FStyle TAG 'PACKSTY' ADDITIVE 
  SET ORDER TO (lcDetailF) IN (lcDetailF)
  *B608331,3 MMT 11/25/2007 fix bugs of prinitng the Same style Carton twice[End]

  *-- END OF FUNCTION lfCrtTemp.
  *!*************************************************************
  *! Name      : lfGetData
  *! Developer : Walid Hamed (WLD)
  *! Date      : 08/21/2007
  *! Purpose   : Function To Collect data
  *!*************************************************************
  *! Called from : Soshlbbc.prg
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *! Example     : = lfGetData()
  *!*************************************************************
FUNCTION lfGetData

  SELECT Pack_hdr
  lcPKHOrder = ORDER('Pack_hdr')
  SET ORDER TO ORDERPCK   && ORDER+STORE+PACK_NO
  SELECT Bol_Lin
  lcBOLLOrder = ORDER('Bol_Lin')
  SET ORDER TO ORD_BOL   && ORDER+PACK_NO+BOL_NO
  SELECT BOL_Hdr
  lcBOLHOrder = ORDER('Bol_Hdr')
  SET ORDER TO BOL_HDR   && BOL_NO
  SELECT InvLine
  lcInvLOrder = ORDER('InvLine')
  SET ORDER TO INVLINEO   && ORDER+STR(LINENO,6)+INVOICE
  SELECT Pack_Lin
  lcPKLOrder = ORDER('Pack_Lin')
  SET ORDER TO PACKSTYLE   && PACK_NO+STR(NO_CART,4)+STYLE+DYELOT
  SELECT ordline
  lcSOLOrder = ORDER('OrdLine')
  SET ORDER TO ORDLACC   && ACCOUNT+STORE+ORDER+STR(LINENO,6)
  SELECT PIKTKT
  lcPIKTOrder = ORDER('PIKTKT')
  SET ORDER TO ORDPIK   && ORDER+PIKTKT 

  SELECT (lcMainF)
  GO TOP
 
  
  SCAN 
    SCATTER MEMVAR MEMO
    STORE '' TO MUCB,m.Bol_No,lcShipVia,lcCarrier
    *B608331,1 MMT 10/31/2007 fix bugs of wrong case qty and error while preview[Start]
    DO CASE
      CASE oAriaApplication.ActiveModuleID ='AL'
        M.INVOICE = ''
        IF gfseek(m.piktkt ,'PIKTKT','PIKTKT') AND PIKTKT.STATUS = 'X'
            LOOP 
        ENDIF
        IF gfseek(m.piktkt,'Pack_Hdr','PACK_HDR')
          m.Tot_Cart = Pack_Hdr.Tot_Cart
          IF gfseek(m.ORDER+Pack_Hdr.PACK_NO,'BOL_LIN')
            gfseek(BOL_LIN.BOL_NO,'BOL_HDR')
            m.Bol_No= BOL_LIN.BOL_NO
            MUCB = PADL(ALLTRIM(lfManufID(m.Bol_No)) , 7 , '0') + PADL(ALLTRIM(m.Bol_No) , 9 , '0')
            MUCB = MUCB + lfCheckDgt(MUCB,'E')
            m.Bol_No = MUCB
            lcShipVia = BOL_Hdr.ShipVia
            lcCarrier = BOL_Hdr.Carrier
          ENDIF
          =gfSeek('O'+ m.ORDER,'ORDHDR','ORDHDR')
          m.Carrier= IIF(EMPTY(lcCarrier),ALLTRIM(gfCodDes(OrdHdr.ShipVia ,'SHIPVIA')),lcCarrier)
          *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][Start]
          m.CaseWght = 0
          *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][End]
          SELECT Pack_Lin
          =gfSEEK(Pack_Hdr.Pack_No)
          SCAN REST WHILE PACK_NO+STR(NO_CART,4)+STYLE+DYELOT = Pack_Hdr.Pack_No
            
            *B608331,4 MMT 11/28/2007 fix bugs of prinitng the Same style Carton twice[Start]
            STORE '' to m.FStyle ,m.style,m.color,m.siz1,m.siz2,m.siz3,m.siz4,m.siz5,m.siz6,m.siz7,m.siz8,M.INVOICE
            STORE 0 TO m.No_Cart,m.CaseQty ,m.Rto1,m.Rto2,m.Rto3,m.Rto4,m.Rto5,m.Rto6,m.Rto7,m.Rto8
            
            IF !SEEK(m.PIKTKT+STR(Pack_Lin.NO_CART,4),lcDetailF,'PACKSTY')
              SELECT Pack_Lin
              lcCartNo = STR(Pack_Lin.NO_CART,4)
              lcStyle = Pack_Lin.STYLE
              lnRecN = RECNO('Pack_Lin')
              *B610955,1 MMT 03/01/2015 Custom shipping label report does not print style info. in case of mixed carton[T20150220.0028][Start]
              lnOrderLineNo = pack_Lin.nordlineno
              *B610955,1 MMT 03/01/2015 Custom shipping label report does not print style info. in case of mixed carton[T20150220.0028][End]
              *B610643,1 TMI 12/29/2013 12:58 [Start] Enhance the locate process by using LOCATE REST WHILE
              *LOCATE FOR PACK_NO+STR(NO_CART,4)+STYLE+DYELOT = Pack_Hdr.Pack_No+lcCartNo AND  style <> lcStyle
              lcSvOrd = ORDER('pack_lin')
              SET ORDER TO PACKSTYLE
              =SEEK(Pack_Hdr.Pack_No+lcCartNo,'pack_lin','PACKSTYLE')
              LOCATE REST WHILE PACK_NO+STR(NO_CART,4)+STYLE+DYELOT = Pack_Hdr.Pack_No+lcCartNo for style <> lcStyle             
              SET ORDER TO &lcSvOrd
              *B610643,1 TMI 12/29/2013 12:58 [End  ] 
              IF FOUND()
                  m.llMixed = .T.    
                  *B610955,1 MMT 03/01/2015 Custom shipping label report does not print style info. in case of mixed carton[T20150220.0028][Start]
                  m.FStyle = lcStyle             
                  m.No_Cart= lcCartNo 
                  m.style= SUBSTR(lcStyle,1,lnMjrLen)
                  m.color = gfCodDes(SUBSTR(lcStyle,lnMjrLen+2,lnColorLen) , 'COLOR')
                  =gfSEEK(Pack_Hdr.ACCOUNT+Pack_Hdr.STORE+Pack_Hdr.ORDER+STR(lnOrderLineNo ,6),'ORDLINE','ORDLACC')
                  =gfSEEK('S'+ORDLINE.SCALE,'scale')
                  m.siz1= SCALE.Sz1
                  m.siz2= SCALE.Sz2
                  m.siz3= SCALE.Sz3
                  m.siz4= SCALE.Sz4
                  m.siz5= SCALE.Sz5
                  m.siz6= SCALE.Sz6
                  m.siz7= SCALE.Sz7
                  m.siz8= SCALE.Sz8
                  m.CaseQty = ORDLINE.PPQty 
                  =gfSEEK('P'+ORDLINE.SCALE+ORDLINE.PREPAK,'scale')
                  m.Rto1= Pack_lin.Qty1
                  m.Rto2= Pack_lin.Qty2
                  m.Rto3= Pack_lin.Qty3
                  m.Rto4= Pack_lin.Qty4
                  m.Rto5= Pack_lin.Qty5
                  m.Rto6= Pack_lin.Qty6
                  m.Rto7= Pack_lin.Qty7
                  m.Rto8= Pack_lin.Qty8
                  m.CaseQty  = m.Rto8+m.Rto7+m.Rto6+m.Rto5+m.Rto4+m.Rto3+m.Rto2+m.Rto1
                 *B610955,1 MMT 03/01/2015 Custom shipping label report does not print style info. in case of mixed carton[T20150220.0028][End]
                =gfSEEK(Pack_Hdr.Pack_No+lcCartNo)        
                m.No_Cart= Pack_Lin.NO_CART
                SUM totQTY TO m.CaseQty REST WHILE PACK_NO+STR(NO_CART,4)+STYLE+DYELOT = Pack_Hdr.Pack_No+lcCartNo
                *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][Start]
                =gfSEEK(Pack_Hdr.Pack_No+lcCartNo)        
                SUM weight TO m.CaseWght REST WHILE PACK_NO+STR(NO_CART,4)+STYLE+DYELOT = Pack_Hdr.Pack_No+lcCartNo
                *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][End]
                SELECT INVHDR
                lcINVHOrd = ORDER()
                =gfSetOrder('INVHDRA')   
                IF gfSeek(m.Account)
                  LOCATE REST WHILE ACCOUNT+INVOICE = m.Account FOR INVHDR.ORDER = m.ORDER AND INVHDR.Status <> 'V' AND ;
                                    INVHDR.STORE = m.Store AND INVHDR.PIKTKT = m.PIKTKT
                  IF FOUND()              
                    M.INVOICE= INVHDR.INVOICE
                  ENDIF   
                ENDIF             
                SELECT INVHDR
                gfSetOrder(lcINVHOrd)
                IF EMPTY(M.INVOICE)
                  SELECT Consinvh
                  IF gfSeek(m.PIKTKT,'Consinvh','PIKTKT')
                    M.INVOICE= Consinvh.INVOICE
                  ENDIF 
                ENDIF 
                *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][Start]
                =gfSeek(m.Style,'Style','Style')
                m.cMadein = Style.cconsinfo1
                *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][End]
                INSERT INTO  (lcDetailF) FROM MEMVAR
              ELSE
                m.llMixed = .F.            
                =gfSEEK(Pack_Hdr.Pack_No+lcCartNo+lcStyle,'PACK_LIN')
                IF !SEEK(m.PIKTKT+STR(Pack_Lin.NO_CART,4)+Pack_Lin.STYLE,lcDetailF,'PACKSTY')
                  m.FStyle = pack_Lin.STYLE
                  m.No_Cart= PAck_Lin.No_Cart
                  m.style= SUBSTR(pack_Lin.STYLE,1,lnMjrLen)
                  m.color = gfCodDes(SUBSTR(pack_Lin.STYLE,lnMjrLen+2,lnColorLen) , 'COLOR')
                  =gfSEEK(Pack_Hdr.ACCOUNT+Pack_Hdr.STORE+Pack_Hdr.ORDER+STR(pack_Lin.nordlineno,6),'ORDLINE','ORDLACC')
                  =gfSEEK('S'+ORDLINE.SCALE,'scale')
                  m.siz1= SCALE.Sz1
                  m.siz2= SCALE.Sz2
                  m.siz3= SCALE.Sz3
                  m.siz4= SCALE.Sz4
                  m.siz5= SCALE.Sz5
                  m.siz6= SCALE.Sz6
                  m.siz7= SCALE.Sz7
                  m.siz8= SCALE.Sz8
                  m.CaseQty = ORDLINE.PPQty 
                  =gfSEEK('P'+ORDLINE.SCALE+ORDLINE.PREPAK,'scale')
                  m.Rto1= Pack_lin.Qty1
                  m.Rto2= Pack_lin.Qty2
                  m.Rto3= Pack_lin.Qty3
                  m.Rto4= Pack_lin.Qty4
                  m.Rto5= Pack_lin.Qty5
                  m.Rto6= Pack_lin.Qty6
                  m.Rto7= Pack_lin.Qty7
                  m.Rto8= Pack_lin.Qty8
                  m.CaseQty  = m.Rto8+m.Rto7+m.Rto6+m.Rto5+m.Rto4+m.Rto3+m.Rto2+m.Rto1
                  *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][Start]
                  m.CaseWght = Pack_lin.weight
                  *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][End]
                  SELECT INVHDR
                  lcINVHOrd = ORDER()
                  =gfSetOrder('INVHDRA')   
                  IF gfSeek(m.Account)
                    LOCATE REST WHILE ACCOUNT+INVOICE = m.Account FOR INVHDR.ORDER = m.ORDER AND INVHDR.Status <> 'V' AND ;
                                 INVHDR.STORE = m.Store AND INVHDR.PIKTKT = m.PIKTKT
                    IF FOUND()              
                      M.INVOICE= INVHDR.INVOICE
                    ENDIF   
                  ENDIF             
                  SELECT INVHDR
                  gfSetOrder(lcINVHOrd)
                  IF EMPTY(M.INVOICE)
                    SELECT Consinvh
                    IF gfSeek(m.PIKTKT,'Consinvh','PIKTKT')
                      M.INVOICE= Consinvh.INVOICE
                    ENDIF 
                  ENDIF 
                  *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][Start]
                  =gfSeek(m.Style,'Style','Style')
                  m.cMadein = Style.cconsinfo1
                  *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][End]

                  INSERT INTO  (lcDetailF) FROM MEMVAR
                ELSE
                  REPLACE Rto1 WITH  Rto1 + Pack_lin.Qty1,;
                          Rto2 With  Rto2 + Pack_lin.Qty2,;
                          Rto3 WITH  Rto3 + Pack_lin.Qty3,;
                          Rto4 WITH  Rto4 + Pack_lin.Qty4,;
                          Rto5 WITH  Rto5 + Pack_lin.Qty5,;
                          Rto6 WITH  Rto6 + Pack_lin.Qty6,;
                          Rto7 WITH  Rto7 + Pack_lin.Qty7,;
                          Rto8 WITH  Rto8 + Pack_lin.Qty8,;
                          CaseQty  WITH  Rto8+Rto7+Rto6+Rto5+Rto4+Rto3+Rto2+Rto1 IN  (lcDetailF) 
                  *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][Start]
                  REPLACE CaseWght WITH  CaseWght +Pack_lin.weight  IN  (lcDetailF) 
                  *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][End]

                ENDIF
              ENDIF 
              IF BETWEEN(lnRecN ,1,RECCOUNT('Pack_Lin'))
                GO RECORD lnRecN IN Pack_Lin
              ENDIF 
            ELSE
              IF &lcDetailF..llMixed
                LOOP
              ELSE
              *B608331,4 MMT 11/28/2007 fix bugs of prinitng the Same style Carton twice[End]      
              *B608331,3 MMT 11/25/2007 fix bugs of prinitng the Same style Carton twice[Start]
              IF !SEEK(m.PIKTKT+STR(Pack_Lin.NO_CART,4)+Pack_Lin.STYLE,lcDetailF,'PACKSTY')
                m.FStyle = pack_Lin.STYLE
                *B608331,3 MMT 11/25/2007 fix bugs of prinitng the Same style Carton twice[End]
              
                m.No_Cart= PAck_Lin.No_Cart
                m.style= SUBSTR(pack_Lin.STYLE,1,lnMjrLen)
                m.color = gfCodDes(SUBSTR(pack_Lin.STYLE,lnMjrLen+2,lnColorLen) , 'COLOR')
                =gfSEEK(Pack_Hdr.ACCOUNT+Pack_Hdr.STORE+Pack_Hdr.ORDER+STR(pack_Lin.nordlineno,6),'ORDLINE','ORDLACC')
                =gfSEEK('S'+ORDLINE.SCALE,'scale')
                m.siz1= SCALE.Sz1
                m.siz2= SCALE.Sz2
                m.siz3= SCALE.Sz3
                m.siz4= SCALE.Sz4
                m.siz5= SCALE.Sz5
                m.siz6= SCALE.Sz6
                m.siz7= SCALE.Sz7
                m.siz8= SCALE.Sz8
                m.CaseQty = ORDLINE.PPQty 
                =gfSEEK('P'+ORDLINE.SCALE+ORDLINE.PREPAK,'scale')
            
                *B608331,2 MMT 11/21/2007 fix bugs of not prinitng qty when 2 not put whole pack in one carton[Start]
                *!*              m.Rto1= SCALE.Pp1
                *!*              m.Rto2= SCALE.Pp2
                *!*              m.Rto3= SCALE.Pp3
                *!*              m.Rto4= SCALE.Pp4
                *!*              m.Rto5= SCALE.Pp5
                *!*              m.Rto6= SCALE.Pp6
                *!*              m.Rto7= SCALE.Pp7
                *!*              m.Rto8= SCALE.Pp8
                m.Rto1= Pack_lin.Qty1
                m.Rto2= Pack_lin.Qty2
                m.Rto3= Pack_lin.Qty3
                m.Rto4= Pack_lin.Qty4
                m.Rto5= Pack_lin.Qty5
                m.Rto6= Pack_lin.Qty6
                m.Rto7= Pack_lin.Qty7
                m.Rto8= Pack_lin.Qty8
                *B608331,2 MMT 11/21/2007 fix bugs of not prinitng qty when 2 not put whole pack in one carton[End]
              
                m.CaseQty  = m.Rto8+m.Rto7+m.Rto6+m.Rto5+m.Rto4+m.Rto3+m.Rto2+m.Rto1
                *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][Start]
                m.CaseWght = Pack_lin.weight
                *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][End]
                SELECT INVHDR
                lcINVHOrd = ORDER()
                =gfSetOrder('INVHDRA')   
                IF gfSeek(m.Account)
                  LOCATE REST WHILE ACCOUNT+INVOICE = m.Account FOR INVHDR.ORDER = m.ORDER AND INVHDR.Status <> 'V' AND ;
                               INVHDR.STORE = m.Store AND INVHDR.PIKTKT = m.PIKTKT
                  IF FOUND()              
                    M.INVOICE= INVHDR.INVOICE
                  ENDIF   
                ENDIF             
                SELECT INVHDR
                gfSetOrder(lcINVHOrd)
                IF EMPTY(M.INVOICE)
                  SELECT Consinvh
                  IF gfSeek(m.PIKTKT,'Consinvh','PIKTKT')
                    M.INVOICE= Consinvh.INVOICE
                  ENDIF 
                ENDIF 
                *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][Start]
                =gfSeek(m.Style,'Style','Style')
                m.cMadein = Style.cconsinfo1
                *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][End]

                INSERT INTO  (lcDetailF) FROM MEMVAR
              *B608331,3 MMT 11/25/2007 fix bugs of prinitng the Same style Carton twice[Start]
              ELSE
                REPLACE Rto1 WITH  Rto1 + Pack_lin.Qty1,;
                        Rto2 With  Rto2 + Pack_lin.Qty2,;
                        Rto3 WITH  Rto3 + Pack_lin.Qty3,;
                        Rto4 WITH  Rto4 + Pack_lin.Qty4,;
                        Rto5 WITH  Rto5 + Pack_lin.Qty5,;
                        Rto6 WITH  Rto6 + Pack_lin.Qty6,;
                        Rto7 WITH  Rto7 + Pack_lin.Qty7,;
                        Rto8 WITH  Rto8 + Pack_lin.Qty8,;
                        CaseQty  WITH  Rto8+Rto7+Rto6+Rto5+Rto4+Rto3+Rto2+Rto1 IN  (lcDetailF) 
                 *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][Start]
                 REPLACE CaseWght WITH CaseWght + Pack_lin.weight IN (lcDetailF) 
                 *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][End]

              ENDIF
              *B608331,3 MMT 11/25/2007 fix bugs of prinitng the Same style Carton twice[End]
              *B608331,4 MMT 11/28/2007 fix bugs of prinitng the Same style Carton twice[Start]
            ENDIF    
          ENDIF   
          *B608331,4 MMT 11/28/2007 fix bugs of prinitng the Same style Carton twice[End]
          ENDSCAN
        ELSE
          SELECT INVHDR
          lcINVHOrd = ORDER()
          =gfSetOrder('INVHDRA')   
          IF gfSeek(m.Account)
             SCAN REST WHILE ACCOUNT+INVOICE = m.Account FOR INVHDR.ORDER = m.ORDER AND INVHDR.Status <> 'V' AND INVHDR.consol # 'Y' ;
                             AND INVHDR.STORE = m.Store AND INVHDR.PIKTKT = m.PIKTKT
                SELECT INVLINE
                lcINVLOrd = ORDER()
                gfSetOrder('INVLINE')
                IF gfSeek(INVHDR.INVOICE)
                   m.Tot_Cart =  INVHDR.cartons 
                   lnCrtStyle = 0
                   lnLastCart = 0 
                   lnCart = m.Tot_Cart 
                   m.INVOICE  =  INVHDR.INVOICE
                   
                   *B608482,1 MMT 03/26/2008 Fix bug of not prinitng Carrier and notes in case of there is packing list[Start]
                   m.Carrier= ALLTRIM(gfCodDes(INVHDR.ShipVia ,'SHIPVIA'))
                   *B608482,1 MMT 03/26/2008 Fix bug of not prinitng Carrier and notes in case of there is packing list[End]
                   
                   SCAN REST WHILE INVOICE+STR(LINENO,6) = m.INVOICE FOR Order = m.order 
                     =gfSEEK('S'+INVLINE.SCALE,'scale')
                      m.CaseQty = INVLINE.PPQty 
                      m.siz1= SCALE.Sz1
                      m.siz2= SCALE.Sz2
                      m.siz3= SCALE.Sz3
                      m.siz4= SCALE.Sz4
                      m.siz5= SCALE.Sz5
                      m.siz6= SCALE.Sz6
                      m.siz7= SCALE.Sz7
                      m.siz8= SCALE.Sz8
                      =gfSEEK('P'+INVLINE.SCALE+INVLINE.PREPAK,'scale')
                      m.Rto1= SCALE.Pp1
                      m.Rto2= SCALE.Pp2
                      m.Rto3= SCALE.Pp3
                      m.Rto4= SCALE.Pp4
                      m.Rto5= SCALE.Pp5
                      m.Rto6= SCALE.Pp6
                      m.Rto7= SCALE.Pp7
                      m.Rto8= SCALE.Pp8
                      m.CaseQty  = m.Rto8+m.Rto7+m.Rto6+m.Rto5+m.Rto4+m.Rto3+m.Rto2+m.Rto1
                      m.style= SUBSTR(INVLINE.STYLE,1,lnMjrLen)
                      m.color = gfCodDes(SUBSTR(INVLINE.STYLE,lnMjrLen+2,lnColorLen) , 'COLOR')
                      =gfSEEK(INVLINE.Style,"STYLE")
                      lnCrtStyle = IIF(STYLE.Qty_Ctn=0,1,CEILING(TotQty/STYLE.Qty_Ctn))
                      *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][Start]
                      m.CaseWght = m.CaseQty * STYLE.NSTYWEIGHT
                      *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][End]
                      FOR lnI = 1  TO lnCrtStyle
                        m.No_Cart= lnLastCart + lnI
                        *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][Start]
                        =gfSeek(m.Style,'Style','Style')
		                m.cMadein = Style.cconsinfo1
        		        *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][End]

                        INSERT INTO  (lcDetailF) FROM MEMVAR
                      ENDFOR     
                      lnLastCart = lnLastCart + lnCrtStyle
                      lnCart = lnCart - lnCrtStyle
                   ENDSCAN 
                   IF m.Tot_Cart < lnLastCart
                      REPLACE Tot_Cart  WITH lnLastCart ALL IN (lcDetailF) FOR INVOICE = m.Invoice AND PIKTKT = m.piktkt
                   ENDIF 
                ENDIF 
              ENDSCAN 
            ENDIF 
            SELECT INVHDR
            gfSetOrder(lcINVHOrd)
            SELECT Consinvh
            lcConsInvHOrd = ORDER()
            =gfSetOrder('PIKTKT')
            IF gfSeek(m.piktkt)
              SCAN REST WHILE PIKTKT = m.piktkt FOR Order = m.order AND  Status <> 'V'
                 m.invoice  = Consinvh.INVOICE
                 
                 *B608482,1 MMT 03/26/2008 Fix bug of not prinitng Carrier and notes in case of there is packing list[Start]
                 m.Carrier= ALLTRIM(gfCodDes(Consinvh.ShipVia ,'SHIPVIA'))
                 *B608482,1 MMT 03/26/2008 Fix bug of not prinitng Carrier and notes in case of there is packing list[End]

                 
                 
                 m.Tot_Cart =  Consinvh.cartons 
                 lnCrtStyle = 0
                 lnLastCart = 0 
                 lnCart = m.Tot_Cart 
                 SELECT Consinvl &&INVOICE+STORE+ORDER+STYLE+STR(LINENO,6)
                 IF gfSeek(m.invoice+Consinvh.Store+m.ORDER)
                   SCAN REST WHILE INVOICE+STORE+ORDER+STYLE+STR(LINENO,6) = m.invoice+Consinvh.Store+m.ORDER
                      =gfSEEK('S'+Consinvl.SCALE,'scale')
                      =gfSeek(INVOICE+STR(LINENO,6),'INVLINE','INVLINE')
                      =gfSEEK('P'+INVLINE.SCALE+INVLINE.PREPAK,'scale')
                      m.Rto1= SCALE.Pp1
                      m.Rto2= SCALE.Pp2
                      m.Rto3= SCALE.Pp3
                      m.Rto4= SCALE.Pp4
                      m.Rto5= SCALE.Pp5
                      m.Rto6= SCALE.Pp6
                      m.Rto7= SCALE.Pp7
                      m.Rto8= SCALE.Pp8
                      m.CaseQty  = m.Rto8+m.Rto7+m.Rto6+m.Rto5+m.Rto4+m.Rto3+m.Rto2+m.Rto1
                      m.siz1= SCALE.Sz1
                      m.siz2= SCALE.Sz2
                      m.siz3= SCALE.Sz3
                      m.siz4= SCALE.Sz4
                      m.siz5= SCALE.Sz5
                      m.siz6= SCALE.Sz6
                      m.siz7= SCALE.Sz7
                      m.siz8= SCALE.Sz8
                      m.style= SUBSTR(Consinvl.STYLE,1,lnMjrLen)
                      m.color = gfCodDes(SUBSTR(Consinvl.STYLE,lnMjrLen+2,lnColorLen) , 'COLOR')
                      =gfSEEK(Consinvl.Style,"STYLE")
                      lnCrtStyle = IIF(STYLE.Qty_Ctn=0,1,CEILING(TotQty/STYLE.Qty_Ctn))
                      *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][Start]
                      m.CaseWght = m.CaseQty * STYLE.NSTYWEIGHT
                      *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][End]
                      FOR lnI = 1  TO lnCrtStyle
                        m.No_Cart= lnLastCart + lnI
                        *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][Start]
                        =gfSeek(m.Style,'Style','Style')
                        m.cMadein = Style.cconsinfo1
                        *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][End]

                        INSERT INTO  (lcDetailF) FROM MEMVAR
                      ENDFOR     
                      lnLastCart = lnLastCart + lnCrtStyle
                      lnCart = lnCart - lnCrtStyle
                   ENDSCAN 
                   IF m.Tot_Cart < lnLastCart
                      REPLACE Tot_Cart  WITH lnLastCart ALL IN (lcDetailF) FOR INVOICE = m.Invoice AND PIKTKT = m.piktkt
                   ENDIF 

                 ENDIF 
              ENDSCAN 
            ENDIF 
         ENDIF 
         IF !SEEK(m.piktkt,lcDetailF)
          SELECT ordline
          gfsetorder('ordline')
           =gfSeek('O'+m.order,'ordline','ordline')
          lnTotCrt = 0
          SELECT ordline
          SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = 'O'+m.order FOR PIKTKT = m.piktkt
            =gfSEEK(ORDLINE.Style,"STYLE")
            lnTotCrt = lnTotCrt  + IIF(STYLE.Qty_Ctn=0,1,CEILING(TotPIK/STYLE.Qty_Ctn))
          ENDSCAN
      
          m.Tot_Cart = lnTotCrt
          lnCrtStyle = 0
          lnCart = m.Tot_Cart 
          lnLastCart = 0

          SELECT ordline
          gfsetorder('ordline')

          =gfSeek('O'+m.Order,'ordline','ordline')
          SELECT ordline
          SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = 'O'+m.ORDER FOR PIKTKT = m.piktkt
            =gfSEEK('S'+ORDLINE.SCALE,'scale')
            m.CaseQty = ORDLINE.PPQty 
            m.siz1= SCALE.Sz1
            m.siz2= SCALE.Sz2
            m.siz3= SCALE.Sz3
            m.siz4= SCALE.Sz4
            m.siz5= SCALE.Sz5
            m.siz6= SCALE.Sz6
            m.siz7= SCALE.Sz7
            m.siz8= SCALE.Sz8
            =gfSEEK('P'+ORDLINE.SCALE+ORDLINE.PREPAK,'scale')
            m.Rto1= SCALE.Pp1
            m.Rto2= SCALE.Pp2
            m.Rto3= SCALE.Pp3
            m.Rto4= SCALE.Pp4
            m.Rto5= SCALE.Pp5
            m.Rto6= SCALE.Pp6
            m.Rto7= SCALE.Pp7
            m.Rto8= SCALE.Pp8
            m.CaseQty  = m.Rto8+m.Rto7+m.Rto6+m.Rto5+m.Rto4+m.Rto3+m.Rto2+m.Rto1
            m.style= SUBSTR(ORDLINE.STYLE,1,lnMjrLen)
            m.color = gfCodDes(SUBSTR(ORDLINE.STYLE,lnMjrLen+2,lnColorLen) , 'COLOR')
            
            =gfSEEK(ORDLINE.STYLE,"STYLE")
            lnCrtStyle = IIF(STYLE.Qty_Ctn=0,1,CEILING(TotQty/STYLE.Qty_Ctn))
            *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][Start]
            m.CaseWght = m.CaseQty * STYLE.NSTYWEIGHT
            *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][End]

            FOR lnI = 1  TO lnCrtStyle
              m.No_Cart= lnLastCart + lnI
              *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][Start]
              =gfSeek(m.Style,'Style','Style')
              m.cMadein = Style.cconsinfo1
              *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][End]

              INSERT INTO  (lcDetailF) FROM MEMVAR
            ENDFOR     
            lnLastCart = lnLastCart + lnCrtStyle
            lnCart = lnCart - lnCrtStyle
          ENDSCAN
        ENDIF 
      
      CASE oAriaApplication.ActiveModuleID ='AR'
         
      
         IF gfSeek(m.invoice,'INVHDR') AND  INVHDR.Status = 'V' 
           LOOP 
         ENDIF 
         IF INVHDR.consol # 'Y'
           IF IIF(!EMPTY(INVHDR.PIKTKT),gfSeek(INVHDR.PIKTKT,'Pack_hdr','Pack_hdr'), gfSeek(m.order+m.STORE,'Pack_hdr','ORDERPCK')) 
              m.Tot_Cart = Pack_Hdr.Tot_Cart
              m.piktkt = Pack_Hdr.pack_no
              IF gfseek(m.ORDER+Pack_Hdr.PACK_NO,'BOL_LIN')
                gfseek(BOL_LIN.BOL_NO,'BOL_HDR')
                m.Bol_No= BOL_LIN.BOL_NO
                MUCB = PADL(ALLTRIM(lfManufID(m.Bol_No)) , 7 , '0') + PADL(ALLTRIM(m.Bol_No) , 9 , '0')
                MUCB = MUCB + lfCheckDgt(MUCB,'E')
                m.Bol_No = MUCB
                lcShipVia = BOL_Hdr.ShipVia
                lcCarrier = BOL_Hdr.Carrier
              ENDIF
              =gfSeek('O'+ m.ORDER,'ORDHDR','ORDHDR')
              m.Carrier= IIF(EMPTY(lcCarrier),ALLTRIM(gfCodDes(OrdHdr.ShipVia ,'SHIPVIA')),lcCarrier)
              SELECT Pack_Lin
              =gfSEEK(Pack_Hdr.Pack_No)
              SCAN REST WHILE PACK_NO+STR(NO_CART,4)+STYLE+DYELOT = Pack_Hdr.Pack_No
                  *B608331,4 MMT 11/28/2007 fix bugs of prinitng the Same style Carton twice[Start]
                  STORE '' to m.FStyle ,m.style,m.color,m.siz1,m.siz2,m.siz3,m.siz4,m.siz5,m.siz6,m.siz7,m.siz8
                  STORE 0 TO m.No_Cart,m.CaseQty ,m.Rto1,m.Rto2,m.Rto3,m.Rto4,m.Rto5,m.Rto6,m.Rto7,m.Rto8
                  
                  IF !SEEK(m.PIKTKT+STR(Pack_Lin.NO_CART,4),lcDetailF,'PACKSTY')
                    SELECT Pack_Lin
                    lcCartNo = STR(Pack_Lin.NO_CART,4)
                    lcStyle = Pack_Lin.STYLE
                    lnRecN = RECNO('Pack_Lin')
                    
                    *B610643,1 TMI 12/29/2013 12:58 [Start]  Enhance the locate process by using LOCATE REST WHILE
                    *LOCATE FOR PACK_NO+STR(NO_CART,4)+STYLE+DYELOT = Pack_Hdr.Pack_No+lcCartNo AND  style <> lcStyle
                    lcSvOrd = ORDER('pack_lin')
                    SET ORDER TO PACKSTYLE
                    =SEEK(Pack_Hdr.Pack_No+lcCartNo,'pack_lin','PACKSTYLE')
                    LOCATE REST WHILE PACK_NO+STR(NO_CART,4)+STYLE+DYELOT = Pack_Hdr.Pack_No+lcCartNo for style <> lcStyle             
                    SET ORDER TO &lcSvOrd
                    *B610643,1 TMI 12/29/2013 12:58 [End  ] 
                    IF FOUND()
                      m.llMixed = .T.    
                      =gfSEEK(Pack_Hdr.Pack_No+lcCartNo)        
                      m.No_Cart= Pack_Lin.NO_CART
                      SUM totQTY TO m.CaseQty REST WHILE PACK_NO+STR(NO_CART,4)+STYLE+DYELOT = Pack_Hdr.Pack_No+lcCartNo
                      *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][Start]
                      =gfSEEK(Pack_Hdr.Pack_No+lcCartNo)        
                      SUM weight TO m.CaseWght REST WHILE PACK_NO+STR(NO_CART,4)+STYLE+DYELOT = Pack_Hdr.Pack_No+lcCartNo
                      =gfSeek(m.Style,'Style','Style')
                       m.cMadein = Style.cconsinfo1
                      *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][End]

                      INSERT INTO  (lcDetailF) FROM MEMVAR
                    ELSE
                      m.llMixed = .F.            
                      =gfSEEK(Pack_Hdr.Pack_No+lcCartNo+lcStyle,'PACK_LIN')
                      IF !SEEK(m.PIKTKT+STR(Pack_Lin.NO_CART,4)+Pack_Lin.STYLE,lcDetailF,'PACKSTY')
                        m.FStyle = pack_Lin.STYLE
                        m.No_Cart= PAck_Lin.No_Cart
                        m.style= SUBSTR(pack_Lin.STYLE,1,lnMjrLen)
                        m.color = gfCodDes(SUBSTR(pack_Lin.STYLE,lnMjrLen+2,lnColorLen) , 'COLOR')
                        =gfSEEK(Pack_Hdr.ACCOUNT+Pack_Hdr.STORE+Pack_Hdr.ORDER+STR(pack_Lin.nordlineno,6),'ORDLINE','ORDLACC')
                        =gfSEEK('S'+ORDLINE.SCALE,'scale')
                        m.siz1= SCALE.Sz1
                        m.siz2= SCALE.Sz2
                        m.siz3= SCALE.Sz3
                        m.siz4= SCALE.Sz4
                        m.siz5= SCALE.Sz5
                        m.siz6= SCALE.Sz6
                        m.siz7= SCALE.Sz7
                        m.siz8= SCALE.Sz8
                        m.CaseQty = ORDLINE.PPQty 
                        =gfSEEK('P'+ORDLINE.SCALE+ORDLINE.PREPAK,'scale')
                        m.Rto1= Pack_lin.Qty1
                        m.Rto2= Pack_lin.Qty2
                        m.Rto3= Pack_lin.Qty3
                        m.Rto4= Pack_lin.Qty4
                        m.Rto5= Pack_lin.Qty5
                        m.Rto6= Pack_lin.Qty6
                        m.Rto7= Pack_lin.Qty7
                        m.Rto8= Pack_lin.Qty8
                        m.CaseQty  = m.Rto8+m.Rto7+m.Rto6+m.Rto5+m.Rto4+m.Rto3+m.Rto2+m.Rto1
                  
                        *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][Start]
                        m.CaseWght = Pack_lin.weight
                        =gfSeek(m.Style,'Style','Style')
                        m.cMadein = Style.cconsinfo1
                        *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][End]

                        INSERT INTO  (lcDetailF) FROM MEMVAR
                      ELSE
                        REPLACE Rto1 WITH  Rto1 + Pack_lin.Qty1,;
                                Rto2 With  Rto2 + Pack_lin.Qty2,;
                                Rto3 WITH  Rto3 + Pack_lin.Qty3,;
                                Rto4 WITH  Rto4 + Pack_lin.Qty4,;
                                Rto5 WITH  Rto5 + Pack_lin.Qty5,;
                                Rto6 WITH  Rto6 + Pack_lin.Qty6,;
                                Rto7 WITH  Rto7 + Pack_lin.Qty7,;
                                Rto8 WITH  Rto8 + Pack_lin.Qty8,;
                                CaseQty  WITH  Rto8+Rto7+Rto6+Rto5+Rto4+Rto3+Rto2+Rto1 IN  (lcDetailF) 
                        *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][Start]
                        REPLACE CaseWght WITH CaseWght +Pack_lin.weight IN  (lcDetailF) 
                        *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][End]

                      ENDIF
                    ENDIF 
                    IF BETWEEN(lnRecN ,1,RECCOUNT('Pack_Lin'))
                      GO RECORD lnRecN IN Pack_Lin
                    ENDIF 
                  ELSE
                    IF &lcDetailF..llMixed
                      LOOP
                    ELSE
                    *B608331,4 MMT 11/28/2007 fix bugs of prinitng the Same style Carton twice[End]      
                    *B608331,3 MMT 11/25/2007 fix bugs of prinitng the Same style Carton twice[Start]
                    IF !SEEK(m.PIKTKT+STR(Pack_Lin.NO_CART,4)+Pack_Lin.STYLE,lcDetailF,'PACKSTY')
                      m.FStyle = pack_Lin.STYLE
                      *B608331,3 MMT 11/25/2007 fix bugs of prinitng the Same style Carton twice[End]
                    
                      m.No_Cart= PAck_Lin.No_Cart
                      m.style= SUBSTR(pack_Lin.STYLE,1,lnMjrLen)
                      m.color = gfCodDes(SUBSTR(pack_Lin.STYLE,lnMjrLen+2,lnColorLen) , 'COLOR')
                      =gfSEEK(Pack_Hdr.ACCOUNT+Pack_Hdr.STORE+Pack_Hdr.ORDER+STR(pack_Lin.nordlineno,6),'ORDLINE','ORDLACC')
                      =gfSEEK('S'+ORDLINE.SCALE,'scale')
                      m.siz1= SCALE.Sz1
                      m.siz2= SCALE.Sz2
                      m.siz3= SCALE.Sz3
                      m.siz4= SCALE.Sz4
                      m.siz5= SCALE.Sz5
                      m.siz6= SCALE.Sz6
                      m.siz7= SCALE.Sz7
                      m.siz8= SCALE.Sz8
                      m.CaseQty = ORDLINE.PPQty 
                      =gfSEEK('P'+ORDLINE.SCALE+ORDLINE.PREPAK,'scale')
                  
                      *B608331,2 MMT 11/21/2007 fix bugs of not prinitng qty when 2 not put whole pack in one carton[Start]
                      *!*              m.Rto1= SCALE.Pp1
                      *!*              m.Rto2= SCALE.Pp2
                      *!*              m.Rto3= SCALE.Pp3
                      *!*              m.Rto4= SCALE.Pp4
                      *!*              m.Rto5= SCALE.Pp5
                      *!*              m.Rto6= SCALE.Pp6
                      *!*              m.Rto7= SCALE.Pp7
                      *!*              m.Rto8= SCALE.Pp8
                      m.Rto1= Pack_lin.Qty1
                      m.Rto2= Pack_lin.Qty2
                      m.Rto3= Pack_lin.Qty3
                      m.Rto4= Pack_lin.Qty4
                      m.Rto5= Pack_lin.Qty5
                      m.Rto6= Pack_lin.Qty6
                      m.Rto7= Pack_lin.Qty7
                      m.Rto8= Pack_lin.Qty8
                      *B608331,2 MMT 11/21/2007 fix bugs of not prinitng qty when 2 not put whole pack in one carton[End]
                    
                      m.CaseQty  = m.Rto8+m.Rto7+m.Rto6+m.Rto5+m.Rto4+m.Rto3+m.Rto2+m.Rto1
                      *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][Start]
                      m.CaseWght =  Pack_lin.weight 
                      =gfSeek(m.Style,'Style','Style')
	                  m.cMadein = Style.cconsinfo1
      		          *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][End]

                      INSERT INTO  (lcDetailF) FROM MEMVAR
                    *B608331,3 MMT 11/25/2007 fix bugs of prinitng the Same style Carton twice[Start]
                    ELSE
                      REPLACE Rto1 WITH  Rto1 + Pack_lin.Qty1,;
                              Rto2 With  Rto2 + Pack_lin.Qty2,;
                              Rto3 WITH  Rto3 + Pack_lin.Qty3,;
                              Rto4 WITH  Rto4 + Pack_lin.Qty4,;
                              Rto5 WITH  Rto5 + Pack_lin.Qty5,;
                              Rto6 WITH  Rto6 + Pack_lin.Qty6,;
                              Rto7 WITH  Rto7 + Pack_lin.Qty7,;
                              Rto8 WITH  Rto8 + Pack_lin.Qty8,;
                              CaseQty  WITH  Rto8+Rto7+Rto6+Rto5+Rto4+Rto3+Rto2+Rto1 IN  (lcDetailF) 
                      
                      *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][Start]
                      REPLACE CaseWght WITH CaseWght + Pack_lin.weight IN (lcDetailF) 
                      *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][End]  

                    ENDIF
                    *B608331,3 MMT 11/25/2007 fix bugs of prinitng the Same style Carton twice[End]
                    *B608331,4 MMT 11/28/2007 fix bugs of prinitng the Same style Carton twice[Start]
                  ENDIF    
                ENDIF   
                *B608331,4 MMT 11/28/2007 fix bugs of prinitng the Same style Carton twice[End]
              ENDSCAN
            ENDIF 
          ELSE
            IF INVHDR.Consol = 'Y'  
              SELECT Consinvh
              lcConsInvHOrd = ORDER()
              =gfSetOrder('Consinvh')
              IF gfSeek(m.INVOICE+m.Store+m.order)
              
                 SCAN REST WHILE INVOICE+STORE+ORDER+PIKTKT = m.INVOICE+m.Store+m.order FOR Status <> 'V' AND !EMPTY(PIKTKT)
                 
                   IF gfSeek(Consinvh.PIKTKT,'PAcK_HDR','PACK_HDR') OR gfSeek(Consinvh.order+Consinvh.STORE,'Pack_hdr','ORDERPCK')
                     m.Tot_Cart = Pack_Hdr.Tot_Cart
                     m.piktkt = Pack_Hdr.pack_no
                     IF gfseek(m.ORDER+Pack_Hdr.PACK_NO,'BOL_LIN')
                       gfseek(BOL_LIN.BOL_NO,'BOL_HDR')
                       m.Bol_No= BOL_LIN.BOL_NO
                       MUCB = PADL(ALLTRIM(lfManufID(m.Bol_No)) , 7 , '0') + PADL(ALLTRIM(m.Bol_No) , 9 , '0')
                       MUCB = MUCB + lfCheckDgt(MUCB,'E')
                       m.Bol_No = MUCB
                       lcShipVia = BOL_Hdr.ShipVia
                       lcCarrier = BOL_Hdr.Carrier
                     ENDIF
                     =gfSeek('O'+ m.ORDER,'ORDHDR','ORDHDR')
                     m.Carrier= IIF(EMPTY(lcCarrier),ALLTRIM(gfCodDes(OrdHdr.ShipVia ,'SHIPVIA')),lcCarrier)
                     SELECT Pack_Lin
                     =gfSEEK(Pack_Hdr.Pack_No)
                     SCAN REST WHILE PACK_NO+STR(NO_CART,4)+STYLE+DYELOT = Pack_Hdr.Pack_No
                           
                                               *B608331,4 MMT 11/28/2007 fix bugs of prinitng the Same style Carton twice[Start]
                        STORE '' to m.FStyle ,m.style,m.color,m.siz1,m.siz2,m.siz3,m.siz4,m.siz5,m.siz6,m.siz7,m.siz8
                        STORE 0 TO m.No_Cart,m.CaseQty ,m.Rto1,m.Rto2,m.Rto3,m.Rto4,m.Rto5,m.Rto6,m.Rto7,m.Rto8
                        
                        IF !SEEK(m.PIKTKT+STR(Pack_Lin.NO_CART,4),lcDetailF,'PACKSTY')
                          SELECT Pack_Lin
                          lcCartNo = STR(Pack_Lin.NO_CART,4)
                          lcStyle = Pack_Lin.STYLE
                          lnRecN = RECNO('Pack_Lin')
                          
                          *B610643,1 TMI 12/29/2013 12:58 [Start]  Enhance the locate process by using LOCATE REST WHILE
                          *LOCATE FOR PACK_NO+STR(NO_CART,4)+STYLE+DYELOT = Pack_Hdr.Pack_No+lcCartNo AND  style <> lcStyle
                          lcSvOrd = ORDER('pack_lin')
                          SET ORDER TO PACKSTYLE
                          =SEEK(Pack_Hdr.Pack_No+lcCartNo,'pack_lin','PACKSTYLE')
                          LOCATE REST WHILE PACK_NO+STR(NO_CART,4)+STYLE+DYELOT = Pack_Hdr.Pack_No+lcCartNo for style <> lcStyle             
                          SET ORDER TO &lcSvOrd
                          *B610643,1 TMI 12/29/2013 12:58 [End  ] 
                          IF FOUND()
                            m.llMixed = .T.    
                            =gfSEEK(Pack_Hdr.Pack_No+lcCartNo)        
                            m.No_Cart= Pack_Lin.NO_CART
                            SUM totQTY TO m.CaseQty REST WHILE PACK_NO+STR(NO_CART,4)+STYLE+DYELOT = Pack_Hdr.Pack_No+lcCartNo
                            *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][Start]
                            =gfSEEK(Pack_Hdr.Pack_No+lcCartNo)        
                            SUM weight TO m.CaseWght REST WHILE PACK_NO+STR(NO_CART,4)+STYLE+DYELOT = Pack_Hdr.Pack_No+lcCartNo
                			=gfSeek(m.Style,'Style','Style')
			                m.cMadein = Style.cconsinfo1
            			    *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][End]

                            INSERT INTO  (lcDetailF) FROM MEMVAR
                          ELSE
                            m.llMixed = .F.            
                            =gfSEEK(Pack_Hdr.Pack_No+lcCartNo+lcStyle,'PACK_LIN')
                            IF !SEEK(m.PIKTKT+STR(Pack_Lin.NO_CART,4)+Pack_Lin.STYLE,lcDetailF,'PACKSTY')
                              m.FStyle = pack_Lin.STYLE
                              m.No_Cart= PAck_Lin.No_Cart
                              m.style= SUBSTR(pack_Lin.STYLE,1,lnMjrLen)
                              m.color = gfCodDes(SUBSTR(pack_Lin.STYLE,lnMjrLen+2,lnColorLen) , 'COLOR')
                              =gfSEEK(Pack_Hdr.ACCOUNT+Pack_Hdr.STORE+Pack_Hdr.ORDER+STR(pack_Lin.nordlineno,6),'ORDLINE','ORDLACC')
                              =gfSEEK('S'+ORDLINE.SCALE,'scale')
                              m.siz1= SCALE.Sz1
                              m.siz2= SCALE.Sz2
                              m.siz3= SCALE.Sz3
                              m.siz4= SCALE.Sz4
                              m.siz5= SCALE.Sz5
                              m.siz6= SCALE.Sz6
                              m.siz7= SCALE.Sz7
                              m.siz8= SCALE.Sz8
                              m.CaseQty = ORDLINE.PPQty 
                              =gfSEEK('P'+ORDLINE.SCALE+ORDLINE.PREPAK,'scale')
                              m.Rto1= Pack_lin.Qty1
                              m.Rto2= Pack_lin.Qty2
                              m.Rto3= Pack_lin.Qty3
                              m.Rto4= Pack_lin.Qty4
                              m.Rto5= Pack_lin.Qty5
                              m.Rto6= Pack_lin.Qty6
                              m.Rto7= Pack_lin.Qty7
                              m.Rto8= Pack_lin.Qty8
                              m.CaseQty  = m.Rto8+m.Rto7+m.Rto6+m.Rto5+m.Rto4+m.Rto3+m.Rto2+m.Rto1
                        
                              *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][Start]
                              m.CaseWght = Pack_lin.weight
		            		  =gfSeek(m.Style,'Style','Style')
        			          m.cMadein = Style.cconsinfo1
			                  *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][End]

                              INSERT INTO  (lcDetailF) FROM MEMVAR
                            ELSE
                              REPLACE Rto1 WITH  Rto1 + Pack_lin.Qty1,;
                                      Rto2 With  Rto2 + Pack_lin.Qty2,;
                                      Rto3 WITH  Rto3 + Pack_lin.Qty3,;
                                      Rto4 WITH  Rto4 + Pack_lin.Qty4,;
                                      Rto5 WITH  Rto5 + Pack_lin.Qty5,;
                                      Rto6 WITH  Rto6 + Pack_lin.Qty6,;
                                      Rto7 WITH  Rto7 + Pack_lin.Qty7,;
                                      Rto8 WITH  Rto8 + Pack_lin.Qty8,;
                                      CaseQty  WITH  Rto8+Rto7+Rto6+Rto5+Rto4+Rto3+Rto2+Rto1 IN  (lcDetailF) 
                               *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][Start]
                               REPLACE CaseWght WITH CaseWght + Pack_lin.weight IN  (lcDetailF) 
                               *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][End]

                            ENDIF
                          ENDIF 
                          IF BETWEEN(lnRecN ,1,RECCOUNT('Pack_Lin'))
                            GO RECORD lnRecN IN Pack_Lin
                          ENDIF 
                        ELSE
                          IF &lcDetailF..llMixed
                            LOOP
                          ELSE
                          *B608331,4 MMT 11/28/2007 fix bugs of prinitng the Same style Carton twice[End]      
                          *B608331,3 MMT 11/25/2007 fix bugs of prinitng the Same style Carton twice[Start]
                          IF !SEEK(m.PIKTKT+STR(Pack_Lin.NO_CART,4)+Pack_Lin.STYLE,lcDetailF,'PACKSTY')
                            m.FStyle = pack_Lin.STYLE
                            *B608331,3 MMT 11/25/2007 fix bugs of prinitng the Same style Carton twice[End]
                          
                            m.No_Cart= PAck_Lin.No_Cart
                            m.style= SUBSTR(pack_Lin.STYLE,1,lnMjrLen)
                            m.color = gfCodDes(SUBSTR(pack_Lin.STYLE,lnMjrLen+2,lnColorLen) , 'COLOR')
                            =gfSEEK(Pack_Hdr.ACCOUNT+Pack_Hdr.STORE+Pack_Hdr.ORDER+STR(pack_Lin.nordlineno,6),'ORDLINE','ORDLACC')
                            =gfSEEK('S'+ORDLINE.SCALE,'scale')
                            m.siz1= SCALE.Sz1
                            m.siz2= SCALE.Sz2
                            m.siz3= SCALE.Sz3
                            m.siz4= SCALE.Sz4
                            m.siz5= SCALE.Sz5
                            m.siz6= SCALE.Sz6
                            m.siz7= SCALE.Sz7
                            m.siz8= SCALE.Sz8
                            m.CaseQty = ORDLINE.PPQty 
                            =gfSEEK('P'+ORDLINE.SCALE+ORDLINE.PREPAK,'scale')
                        
                            *B608331,2 MMT 11/21/2007 fix bugs of not prinitng qty when 2 not put whole pack in one carton[Start]
                            *!*              m.Rto1= SCALE.Pp1
                            *!*              m.Rto2= SCALE.Pp2
                            *!*              m.Rto3= SCALE.Pp3
                            *!*              m.Rto4= SCALE.Pp4
                            *!*              m.Rto5= SCALE.Pp5
                            *!*              m.Rto6= SCALE.Pp6
                            *!*              m.Rto7= SCALE.Pp7
                            *!*              m.Rto8= SCALE.Pp8
                            m.Rto1= Pack_lin.Qty1
                            m.Rto2= Pack_lin.Qty2
                            m.Rto3= Pack_lin.Qty3
                            m.Rto4= Pack_lin.Qty4
                            m.Rto5= Pack_lin.Qty5
                            m.Rto6= Pack_lin.Qty6
                            m.Rto7= Pack_lin.Qty7
                            m.Rto8= Pack_lin.Qty8
                            *B608331,2 MMT 11/21/2007 fix bugs of not prinitng qty when 2 not put whole pack in one carton[End]
                          
                            m.CaseQty  = m.Rto8+m.Rto7+m.Rto6+m.Rto5+m.Rto4+m.Rto3+m.Rto2+m.Rto1
                            *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][Start]
                            m.CaseWght =  Pack_lin.weight
                			=gfSeek(m.Style,'Style','Style')
			                m.cMadein = Style.cconsinfo1
            			   *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][End]

                            INSERT INTO  (lcDetailF) FROM MEMVAR
                          *B608331,3 MMT 11/25/2007 fix bugs of prinitng the Same style Carton twice[Start]
                          ELSE
                            REPLACE Rto1 WITH  Rto1 + Pack_lin.Qty1,;
                                    Rto2 With  Rto2 + Pack_lin.Qty2,;
                                    Rto3 WITH  Rto3 + Pack_lin.Qty3,;
                                    Rto4 WITH  Rto4 + Pack_lin.Qty4,;
                                    Rto5 WITH  Rto5 + Pack_lin.Qty5,;
                                    Rto6 WITH  Rto6 + Pack_lin.Qty6,;
                                    Rto7 WITH  Rto7 + Pack_lin.Qty7,;
                                    Rto8 WITH  Rto8 + Pack_lin.Qty8,;
                                    CaseQty  WITH  Rto8+Rto7+Rto6+Rto5+Rto4+Rto3+Rto2+Rto1 IN  (lcDetailF)
                            *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][Start]
                            REPLACE CaseWght WITH CaseWght +  Pack_lin.weight IN  (lcDetailF)
                            *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][End]

                          ENDIF
                          *B608331,3 MMT 11/25/2007 fix bugs of prinitng the Same style Carton twice[End]
                          *B608331,4 MMT 11/28/2007 fix bugs of prinitng the Same style Carton twice[Start]
                        ENDIF    
                      ENDIF   
                      *B608331,4 MMT 11/28/2007 fix bugs of prinitng the Same style Carton twice[End]

                     ENDSCAN
                   ENDIF
                 ENDSCAN 
               ENDIF 
            ENDIF 
          ENDIF   
          
 
         IF !Seek(m.invoice,lcDetailF) 
           IF gfSeek(m.invoice,'INVHDR') AND  INVHDR.Status <> 'V' 
             SELECT INVLINE
             lcINVLOrd = ORDER()
             gfSetOrder('INVLINE')
             IF INVHDR.Consol # 'Y' AND gfSeek(INVHDR.INVOICE) 
                m.Tot_Cart =  INVHDR.cartons 
                lnCrtStyle = 0
                lnLastCart = 0 
                m.INVOICE  =  INVHDR.INVOICE
                
                *B608482,1 MMT 03/26/2008 Fix bug of not prinitng Carrier and notes in case of there is packing list[Start]
                m.Carrier= ALLTRIM(gfCodDes(INVHDR.ShipVia ,'SHIPVIA'))
                *B608482,1 MMT 03/26/2008 Fix bug of not prinitng Carrier and notes in case of there is packing list[End]
                
                m.piktkt = INVHDR.piktkt
                lnCart = m.Tot_Cart 
                
                *B608482,1 MMT 03/26/2008 Fix bug of not prinitng Carrier and notes in case of there is packing list[Start]
                lnCartNoRem = 0
                lnCartRemain = 0
                *B608482,1 MMT 03/26/2008 Fix bug of not prinitng Carrier and notes in case of there is packing list[End]
                
                SCAN REST WHILE INVOICE+STR(LINENO,6) = m.INVOICE FOR Order = m.order 
                  =gfSEEK('S'+INVLINE.SCALE,'scale')
                  m.CaseQty = INVLINE.PPQty 
                  m.siz1 = SCALE.Sz1
                  m.siz2 = SCALE.Sz2
                  m.siz3 = SCALE.Sz3
                  m.siz4 = SCALE.Sz4
                  m.siz5 = SCALE.Sz5
                  m.siz6 = SCALE.Sz6
                  m.siz7 = SCALE.Sz7
                  m.siz8 = SCALE.Sz8
                  =gfSEEK('P'+INVLINE.SCALE+INVLINE.PREPAK,'scale')
                  m.Rto1 = SCALE.Pp1
                  m.Rto2 = SCALE.Pp2
                  m.Rto3 = SCALE.Pp3
                  m.Rto4 = SCALE.Pp4
                  m.Rto5 = SCALE.Pp5
                  m.Rto6 = SCALE.Pp6
                  m.Rto7 = SCALE.Pp7
                  m.Rto8 = SCALE.Pp8
                  m.CaseQty  = m.Rto8+m.Rto7+m.Rto6+m.Rto5+m.Rto4+m.Rto3+m.Rto2+m.Rto1
                  m.style    = SUBSTR(INVLINE.STYLE,1,lnMjrLen)
                  m.color    = gfCodDes(SUBSTR(INVLINE.STYLE,lnMjrLen+2,lnColorLen) , 'COLOR')
                  =gfSEEK(INVLINE.Style,"STYLE")
                  

*B608482,1 MMT 03/26/2008 Fix bug of not prinitng Carrier and notes in case of there is packing list[Start]
*!*	                  lnCrtStyle = IIF(STYLE.Qty_Ctn=0,1,CEILING(TotQty/STYLE.Qty_Ctn))
*!*	                  FOR lnI = 1  TO lnCrtStyle
*!*	                    m.No_Cart= lnLastCart + lnI
*!*	                    INSERT INTO  (lcDetailF) FROM MEMVAR
*!*	                  ENDFOR     
*!*	                  lnLastCart = lnLastCart + lnCrtStyle
                    
                  lnCasePack = 0
                  
                  llFullPrePack = .F.
                  IF !EMPTY(INVLINE.PREPAK)
                    llFullPrePack = (MOD(INVLINE.TotQty,m.CaseQty) = 0)
                  ENDIF   
                  IF llFullPrePack
                    lnCasePack = m.CaseQty
                  ELSE
                    lnCasePack = IIF(m.CaseQty > 0 , m.CaseQty,IIF(style.qty_ctn > 0,style.qty_ctn,1))
                    m.Rto1 = 0
                    m.Rto2 = 0
                    m.Rto3 = 0
                    m.Rto4 = 0
                    m.Rto5 = 0
                    m.Rto6 = 0
                    m.Rto7 = 0
                    m.Rto8 = 0
                  ENDIF  
                  lnCrtStyle = CEILING(TotQty/lnCasePack)
                  lnCartSize = lnCrtStyle * lnCasePack
                  lnRemain = totqty
                  lnStyInCart = 0
                  
                  *FOR lnI = 1  TO lnCrtStyle
                  DO WHILE lnRemain > 0 
                    IF !llFullPrePack
                      IF lnCartRemain > 0
                        SELECT(lcDetailF)
                        LOCATE FOR order = m.order AND Invoice = m.invoice AND No_Cart = lnCartNoRem 
                        IF FOUND()
                          REPLACE Style   WITH "",;
                              	  Color   WITH "",;
                              	  siz1    WITH "",;
      				              siz2    WITH "",;
      				              siz3    WITH "",;
        				          siz4    WITH "",;
        				          siz5    WITH "",;
        				          siz6    WITH "",;
        				          siz7    WITH "",;
      	  			              siz8    WITH "",;
                                  Rto1    WITH 0,;
                                  Rto2    WITH 0,;
                                  Rto3    WITH 0,;
                                  Rto4    WITH 0,;
                                  Rto5    WITH 0,;
                                  Rto6    WITH 0,;
                                  Rto7    WITH 0,;
                                  Rto8    WITH 0,;
                                  CaseQty WITH  CaseQty + MIN(lnCartRemain,INVLINE.TotQty)
                          *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][Start]
                          REPLACE CaseWght WITH CaseWght + (MIN(lnCartRemain,INVLINE.TotQty) * STYLE.NSTYWEIGHT)
                          *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][End]
                          
                          lnRemain  = INVLINE.totqty - MIN(lnCartRemain,INVLINE.TotQty)
                          lnStyInCart = lnStyInCart + MIN(lnCartRemain,INVLINE.TotQty)
                          lnCartRemain = lnCasePack - CaseQty 
                          lnCartNoRem = lnCartNoRem
                          SELECT INVLINE
                          LOOP 
                        ENDIF 
                      ENDIF 
                    ENDIF 
                    
      			    m.CaseQty = MIN(lnRemain,lnCasePack)
      			    lnStyInCart = lnStyInCart + m.CaseQty
      			    lnRemain  = totqty - lnStyInCart
      			    lnCartRemain = lnCasePack - m.CaseQty
			        
                   * m.No_Cart= lnLastCart + lnI
                    lnLastCart  = lnLastCart  + 1
                    m.No_Cart = lnLastCart  
                    lnCartNoRem = m.No_Cart
                    *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][Start]
                    m.CaseWght  = m.CaseQty * STYLE.NSTYWEIGHT
 	                =gfSeek(m.Style,'Style','Style')
                    m.cMadein = Style.cconsinfo1
                    *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][End]

                    INSERT INTO  (lcDetailF) FROM MEMVAR
                  ENDDO 
                  *B608482,1 MMT 03/26/2008 Fix bug of not prinitng Carrier and notes in case of there is packing list[End]
                  
                  lnCart = lnCart - lnCrtStyle
               ENDSCAN 
               IF m.Tot_Cart < lnLastCart
                  REPLACE Tot_Cart  WITH lnLastCart ALL IN (lcDetailF) FOR INVOICE = m.Invoice AND PIKTKT = m.piktkt
               ENDIF 
            ELSE
              IF INVHDR.Consol = 'Y'  
                SELECT Consinvh
                lcConsInvHOrd = ORDER()
                =gfSetOrder('Consinvh')
                IF gfSeek(m.INVOICE+m.Store+m.order)
                  SCAN REST WHILE INVOICE+STORE+ORDER+PIKTKT = m.INVOICE+m.Store+m.order FOR Status <> 'V'
                    m.invoice  = Consinvh.INVOICE
                    
                    *B608482,1 MMT 03/26/2008 Fix bug of not prinitng Carrier and notes in case of there is packing list[Start]
	                m.Carrier= ALLTRIM(gfCodDes(Consinvh.ShipVia ,'SHIPVIA'))
    	            *B608482,1 MMT 03/26/2008 Fix bug of not prinitng Carrier and notes in case of there is packing list[End]
                    
                    m.Tot_Cart =  Consinvh.cartons 
                    m.piktkt = Consinvh.piktkt
                    lnCrtStyle = 0
                    lnLastCart = 0 
                    lnCart = m.Tot_Cart 
                    
                    *B608482,1 MMT 03/26/2008 Fix bug of not prinitng Carrier and notes in case of there is packing list[Start]
	                lnCartNoRem = 0
	                lnCartRemain = 0
	                *B608482,1 MMT 03/26/2008 Fix bug of not prinitng Carrier and notes in case of there is packing list[End]
	                
                    SELECT Consinvl &&INVOICE+STORE+ORDER+STYLE+STR(LINENO,6)
                    IF gfSeek(m.invoice+Consinvh.Store+m.ORDER)
                      SCAN REST WHILE INVOICE+STORE+ORDER+STYLE+STR(LINENO,6) = m.invoice+Consinvh.Store+m.ORDER
                        =gfSEEK('S'+Consinvl.SCALE,'scale')
                        =gfSeek(INVOICE+STR(LINENO,6),'INVLINE','INVLINE')
	                        =gfSEEK('P'+INVLINE.SCALE+INVLINE.PREPAK,'scale')
                        m.Rto1= SCALE.Pp1
                        m.Rto2= SCALE.Pp2
                        m.Rto3= SCALE.Pp3
                        m.Rto4= SCALE.Pp4
                        m.Rto5= SCALE.Pp5
                        m.Rto6= SCALE.Pp6
                        m.Rto7= SCALE.Pp7
                        m.Rto8= SCALE.Pp8
                        m.CaseQty  = m.Rto8+m.Rto7+m.Rto6+m.Rto5+m.Rto4+m.Rto3+m.Rto2+m.Rto1
                        m.siz1= SCALE.Sz1
                        m.siz2= SCALE.Sz2
                        m.siz3= SCALE.Sz3
                        m.siz4= SCALE.Sz4
                        m.siz5= SCALE.Sz5
                        m.siz6= SCALE.Sz6
                        m.siz7= SCALE.Sz7
                        m.siz8= SCALE.Sz8
                        m.style= SUBSTR(Consinvl.STYLE,1,lnMjrLen)
                        m.color = gfCodDes(SUBSTR(Consinvl.STYLE,lnMjrLen+2,lnColorLen) , 'COLOR')
                        =gfSEEK(Consinvl.Style,"STYLE")
                        *B608482,1 MMT 03/26/2008 Fix bug of not prinitng Carrier and notes in case of there is packing list[Start]
*!*	                        lnCrtStyle = IIF(STYLE.Qty_Ctn=0,1,CEILING(TotQty/STYLE.Qty_Ctn))
*!*	                        FOR lnI = 1  TO lnCrtStyle
*!*	                          m.No_Cart= lnLastCart + lnI
*!*	                          INSERT INTO  (lcDetailF) FROM MEMVAR
*!*	                        ENDFOR     
*!*	                        lnLastCart = lnLastCart + lnCrtStyle
					            lnCasePack = 0
          
                      llFullPrePack = .F.
                      IF !EMPTY(INVLINE.PREPAK)
                        llFullPrePack = (MOD(INVLINE.TotQty,m.CaseQty) = 0)
                      ENDIF   
                      IF llFullPrePack
                       lnCasePack = m.CaseQty
                      ELSE
                        lnCasePack = IIF(m.CaseQty > 0 , m.CaseQty,IIF(style.qty_ctn > 0,style.qty_ctn,1))
                        m.Rto1 = 0
                        m.Rto2 = 0
                        m.Rto3 = 0
                        m.Rto4 = 0
                        m.Rto5 = 0
                        m.Rto6 = 0
                        m.Rto7 = 0
                        m.Rto8 = 0
                       
                      ENDIF  
                  *lnCrtStyle = IIF(STYLE.Qty_Ctn=0,1,CEILING(TotQty/STYLE.Qty_Ctn))
                  lnCasePack = IIF(m.CaseQty > 0, m.CaseQty,IIF(style.qty_ctn > 0,style.qty_ctn,1))
                  lnCrtStyle = CEILING(TotQty/lnCasePack)
                  lnCartSize = lnCrtStyle * lnCasePack
                  lnRemain = totqty
                  lnStyInCart = 0
                  
                  *FOR lnI = 1  TO lnCrtStyle
                  DO WHILE lnRemain > 0
                    IF !llFullPrePack 
                      IF lnCartRemain > 0
                        SELECT(lcDetailF)
                        LOCATE FOR order = m.order AND Invoice = m.invoice AND No_Cart = lnCartNoRem 
                        IF FOUND()
                          REPLACE Style WITH "",;
                        	      	Color WITH "",;
                                  siz1 WITH "",;
                                  siz2 with "",;
                                  siz3 with "",;
                                  siz4 with "",;
                                  siz5 with "",;
                                  siz6 with "",;
                                  siz7 with "",;
                                  siz8 with "",;
                                  Rto1 WITH 0,;
                                  Rto2 WITH 0,;
                                  Rto3 WITH 0,;
                                  Rto4 WITH 0,;
                                  Rto5 WITH 0,;
                                  Rto6 WITH 0,;
                                  Rto7 WITH 0,;
                                  Rto8 WITH 0,;
                                  CaseQty WITH  CaseQty + MIN(lnCartRemain,Consinvl.TotQty)
                          *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][Start]
                          REPLACE CaseWght  WITH CaseWght  +  (MIN(lnCartRemain,Consinvl.TotQty)* STYLE.NSTYWEIGHT)
                          *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][End]
                        
                          lnRemain  = Consinvl.totqty - MIN(lnCartRemain,Consinvl.TotQty)
                        
                          lnStyInCart = lnStyInCart + MIN(lnCartRemain,Consinvl.TotQty)
                        
                          lnCartRemain = lnCasePack - CaseQty 
                          lnCartNoRem = lnCartNoRem
                          SELECT Consinvl
                          LOOP 
                        ENDIF 
                      ENDIF 
                    ENDIF
                    
			            m.CaseQty = MIN(lnRemain,lnCasePack)
      			        lnStyInCart = lnStyInCart + m.CaseQty
      			        lnRemain  = totqty - lnStyInCart
      			        lnCartRemain = lnCasePack - m.CaseQty
			        
                   * m.No_Cart= lnLastCart + lnI
                    lnLastCart  = lnLastCart  + 1
                    m.No_Cart = lnLastCart  
                    lnCartNoRem = m.No_Cart
                    *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][Start]
                    m.CaseWght = m.CaseQty * STYLE.NSTYWEIGHT
	                =gfSeek(m.Style,'Style','Style')
                    m.cMadein = Style.cconsinfo1
                    *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][End]

                    INSERT INTO  (lcDetailF) FROM MEMVAR
                  ENDDO 
                  *B608482,1 MMT 03/26/2008 Fix bug of not prinitng Carrier and notes in case of there is packing list[End]

                        lnCart = lnCart - lnCrtStyle
                      ENDSCAN 
                      IF m.Tot_Cart < lnLastCart
                        REPLACE Tot_Cart  WITH lnLastCart ALL IN (lcDetailF) FOR INVOICE = m.Invoice AND PIKTKT = m.piktkt
                     ENDIF 
                   ENDIF 
                 ENDSCAN 
               ENDIF 
             ENDIF 
           ENDIF 
         ENDIF 
       ENDIF   
        
      CASE oAriaApplication.ActiveModuleID ='SO'
        

      
        IF gfSeek('O'+m.Order,'ORDHDR','ORDHDR') AND ORDHDR.STATUS = 'X'
          LOOP
        ENDIF
        M.INVOICE = '' 
        m.piktkt =''
        IF gfSeek(m.order+m.STORE,'Pack_hdr','ORDERPCK')
          SELECT PACK_hdr
          SCAN REST WHILE ORDER+STORE+PACK_NO = m.order+m.STORE
            m.Tot_Cart = Pack_Hdr.Tot_Cart
            m.piktkt =Pack_Hdr.PACK_NO 
            IF gfseek(m.ORDER+Pack_Hdr.PACK_NO,'BOL_LIN')
              gfseek(BOL_LIN.BOL_NO,'BOL_HDR')
              m.Bol_No= BOL_LIN.BOL_NO
              MUCB = PADL(ALLTRIM(lfManufID(m.Bol_No)) , 7 , '0') + PADL(ALLTRIM(m.Bol_No) , 9 , '0')
              MUCB = MUCB + lfCheckDgt(MUCB,'E')
              m.Bol_No = MUCB
              lcShipVia = BOL_Hdr.ShipVia
              lcCarrier = BOL_Hdr.Carrier
            ENDIF
            =gfSeek('O'+ m.ORDER,'ORDHDR','ORDHDR')
            m.Carrier= IIF(EMPTY(lcCarrier),ALLTRIM(gfCodDes(OrdHdr.ShipVia ,'SHIPVIA')),lcCarrier)
            SELECT Pack_Lin
            =gfSEEK(Pack_Hdr.Pack_No)
            SCAN REST WHILE PACK_NO+STR(NO_CART,4)+STYLE+DYELOT = Pack_Hdr.Pack_No
            
            *B608331,4 MMT 11/28/2007 fix bugs of prinitng the Same style Carton twice[Start]
            STORE '' to m.FStyle ,m.style,m.color,m.siz1,m.siz2,m.siz3,m.siz4,m.siz5,m.siz6,m.siz7,m.siz8,M.INVOICE
            STORE 0 TO m.No_Cart,m.CaseQty ,m.Rto1,m.Rto2,m.Rto3,m.Rto4,m.Rto5,m.Rto6,m.Rto7,m.Rto8
            
            IF !SEEK(m.PIKTKT+STR(Pack_Lin.NO_CART,4),lcDetailF,'PACKSTY')
              SELECT Pack_Lin
              lcCartNo = STR(Pack_Lin.NO_CART,4)
              lcStyle = Pack_Lin.STYLE
              lnRecN = RECNO('Pack_Lin')
              
              *B610643,1 TMI 12/29/2013 12:58 [Start]  Enhance the locate process by using LOCATE REST WHILE
              *LOCATE FOR PACK_NO+STR(NO_CART,4)+STYLE+DYELOT = Pack_Hdr.Pack_No+lcCartNo AND  style <> lcStyle
              lcSvOrd = ORDER('pack_lin')
              SET ORDER TO PACKSTYLE
              =SEEK(Pack_Hdr.Pack_No+lcCartNo,'pack_lin','PACKSTYLE')
              LOCATE REST WHILE PACK_NO+STR(NO_CART,4)+STYLE+DYELOT = Pack_Hdr.Pack_No+lcCartNo for style <> lcStyle             
              SET ORDER TO &lcSvOrd
              *B610643,1 TMI 12/29/2013 12:58 [End  ] 
              IF FOUND()
                m.llMixed = .T.    
                =gfSEEK(Pack_Hdr.Pack_No+lcCartNo)        
                m.No_Cart= Pack_Lin.NO_CART
                SUM totQTY TO m.CaseQty REST WHILE PACK_NO+STR(NO_CART,4)+STYLE+DYELOT = Pack_Hdr.Pack_No+lcCartNo
                *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][Start]
                =gfSEEK(Pack_Hdr.Pack_No+lcCartNo) 
                SUM weight TO m.CaseWght REST WHILE PACK_NO+STR(NO_CART,4)+STYLE+DYELOT = Pack_Hdr.Pack_No+lcCartNo 
                *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][End]
                SELECT INVHDR
                lcINVHOrd = ORDER()
                =gfSetOrder('INVHDRA')   
                IF gfSeek(m.Account)
                  LOCATE REST WHILE ACCOUNT+INVOICE = m.Account FOR INVHDR.ORDER = m.ORDER AND INVHDR.Status <> 'V' AND ;
                                    INVHDR.STORE = m.Store AND INVHDR.PIKTKT = m.PIKTKT
                  IF FOUND()              
                    M.INVOICE= INVHDR.INVOICE
                  ENDIF   
                ENDIF             
                SELECT INVHDR
                gfSetOrder(lcINVHOrd)
                IF EMPTY(M.INVOICE)
                  SELECT Consinvh
                  IF gfSeek(m.PIKTKT,'Consinvh','PIKTKT')
                    M.INVOICE= Consinvh.INVOICE
                  ENDIF 
                ENDIF 
                *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][Start]
                =gfSeek(m.Style,'Style','Style')
                m.cMadein = Style.cconsinfo1
                *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][End]

                INSERT INTO  (lcDetailF) FROM MEMVAR
              ELSE
                m.llMixed = .F.            
                =gfSEEK(Pack_Hdr.Pack_No+lcCartNo+lcStyle,'PACK_LIN')
                IF !SEEK(m.PIKTKT+STR(Pack_Lin.NO_CART,4)+Pack_Lin.STYLE,lcDetailF,'PACKSTY')
                  m.FStyle = pack_Lin.STYLE
                  m.No_Cart= PAck_Lin.No_Cart
                  m.style= SUBSTR(pack_Lin.STYLE,1,lnMjrLen)
                  m.color = gfCodDes(SUBSTR(pack_Lin.STYLE,lnMjrLen+2,lnColorLen) , 'COLOR')
                  =gfSEEK(Pack_Hdr.ACCOUNT+Pack_Hdr.STORE+Pack_Hdr.ORDER+STR(pack_Lin.nordlineno,6),'ORDLINE','ORDLACC')
                  =gfSEEK('S'+ORDLINE.SCALE,'scale')
                  m.siz1= SCALE.Sz1
                  m.siz2= SCALE.Sz2
                  m.siz3= SCALE.Sz3
                  m.siz4= SCALE.Sz4
                  m.siz5= SCALE.Sz5
                  m.siz6= SCALE.Sz6
                  m.siz7= SCALE.Sz7
                  m.siz8= SCALE.Sz8
                  m.CaseQty = ORDLINE.PPQty 
                  =gfSEEK('P'+ORDLINE.SCALE+ORDLINE.PREPAK,'scale')
                  m.Rto1= Pack_lin.Qty1
                  m.Rto2= Pack_lin.Qty2
                  m.Rto3= Pack_lin.Qty3
                  m.Rto4= Pack_lin.Qty4
                  m.Rto5= Pack_lin.Qty5
                  m.Rto6= Pack_lin.Qty6
                  m.Rto7= Pack_lin.Qty7
                  m.Rto8= Pack_lin.Qty8
                  m.CaseQty  = m.Rto8+m.Rto7+m.Rto6+m.Rto5+m.Rto4+m.Rto3+m.Rto2+m.Rto1
                  *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][Start]
                  m.CaseWght = Pack_lin.weight
                  *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][End]
                  SELECT INVHDR
                  lcINVHOrd = ORDER()
                  =gfSetOrder('INVHDRA')   
                  IF gfSeek(m.Account)
                    LOCATE REST WHILE ACCOUNT+INVOICE = m.Account FOR INVHDR.ORDER = m.ORDER AND INVHDR.Status <> 'V' AND ;
                                 INVHDR.STORE = m.Store AND INVHDR.PIKTKT = m.PIKTKT
                    IF FOUND()              
                      M.INVOICE= INVHDR.INVOICE
                    ENDIF   
                  ENDIF             
                  SELECT INVHDR
                  gfSetOrder(lcINVHOrd)
                  IF EMPTY(M.INVOICE)
                    SELECT Consinvh
                    IF gfSeek(m.PIKTKT,'Consinvh','PIKTKT')
                      M.INVOICE= Consinvh.INVOICE
                    ENDIF 
                  ENDIF 
                  *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][Start]
                  =gfSeek(m.Style,'Style','Style')
                  m.cMadein = Style.cconsinfo1
                  *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][End]

                  INSERT INTO  (lcDetailF) FROM MEMVAR
                ELSE
                  REPLACE Rto1 WITH  Rto1 + Pack_lin.Qty1,;
                          Rto2 With  Rto2 + Pack_lin.Qty2,;
                          Rto3 WITH  Rto3 + Pack_lin.Qty3,;
                          Rto4 WITH  Rto4 + Pack_lin.Qty4,;
                          Rto5 WITH  Rto5 + Pack_lin.Qty5,;
                          Rto6 WITH  Rto6 + Pack_lin.Qty6,;
                          Rto7 WITH  Rto7 + Pack_lin.Qty7,;
                          Rto8 WITH  Rto8 + Pack_lin.Qty8,;
                          CaseQty  WITH  Rto8+Rto7+Rto6+Rto5+Rto4+Rto3+Rto2+Rto1 IN  (lcDetailF) 
                  *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][Start]
                  REPLACE CaseWght WITH CaseWght + Pack_lin.weight IN (lcDetailF) 
                  *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][End]

                ENDIF
              ENDIF 
              IF BETWEEN(lnRecN ,1,RECCOUNT('Pack_Lin'))
                GO RECORD lnRecN IN Pack_Lin
              ENDIF 
            ELSE
              IF &lcDetailF..llMixed
                LOOP
              ELSE
              *B608331,4 MMT 11/28/2007 fix bugs of prinitng the Same style Carton twice[End]      
              *B608331,3 MMT 11/25/2007 fix bugs of prinitng the Same style Carton twice[Start]
              IF !SEEK(m.PIKTKT+STR(Pack_Lin.NO_CART,4)+Pack_Lin.STYLE,lcDetailF,'PACKSTY')
                m.FStyle = pack_Lin.STYLE
                *B608331,3 MMT 11/25/2007 fix bugs of prinitng the Same style Carton twice[End]
              
                m.No_Cart= PAck_Lin.No_Cart
                m.style= SUBSTR(pack_Lin.STYLE,1,lnMjrLen)
                m.color = gfCodDes(SUBSTR(pack_Lin.STYLE,lnMjrLen+2,lnColorLen) , 'COLOR')
                =gfSEEK(Pack_Hdr.ACCOUNT+Pack_Hdr.STORE+Pack_Hdr.ORDER+STR(pack_Lin.nordlineno,6),'ORDLINE','ORDLACC')
                =gfSEEK('S'+ORDLINE.SCALE,'scale')
                m.siz1= SCALE.Sz1
                m.siz2= SCALE.Sz2
                m.siz3= SCALE.Sz3
                m.siz4= SCALE.Sz4
                m.siz5= SCALE.Sz5
                m.siz6= SCALE.Sz6
                m.siz7= SCALE.Sz7
                m.siz8= SCALE.Sz8
                m.CaseQty = ORDLINE.PPQty 
                =gfSEEK('P'+ORDLINE.SCALE+ORDLINE.PREPAK,'scale')
            
                *B608331,2 MMT 11/21/2007 fix bugs of not prinitng qty when 2 not put whole pack in one carton[Start]
                *!*              m.Rto1= SCALE.Pp1
                *!*              m.Rto2= SCALE.Pp2
                *!*              m.Rto3= SCALE.Pp3
                *!*              m.Rto4= SCALE.Pp4
                *!*              m.Rto5= SCALE.Pp5
                *!*              m.Rto6= SCALE.Pp6
                *!*              m.Rto7= SCALE.Pp7
                *!*              m.Rto8= SCALE.Pp8
                m.Rto1= Pack_lin.Qty1
                m.Rto2= Pack_lin.Qty2
                m.Rto3= Pack_lin.Qty3
                m.Rto4= Pack_lin.Qty4
                m.Rto5= Pack_lin.Qty5
                m.Rto6= Pack_lin.Qty6
                m.Rto7= Pack_lin.Qty7
                m.Rto8= Pack_lin.Qty8
                *B608331,2 MMT 11/21/2007 fix bugs of not prinitng qty when 2 not put whole pack in one carton[End]
               
                m.CaseQty  = m.Rto8+m.Rto7+m.Rto6+m.Rto5+m.Rto4+m.Rto3+m.Rto2+m.Rto1
                *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][Start]
                m.CaseWght = m.CaseQty  *  STYLE.NSTYWEIGHT
                *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][End]
                SELECT INVHDR
                lcINVHOrd = ORDER()
                =gfSetOrder('INVHDRA')   
                IF gfSeek(m.Account)
                  LOCATE REST WHILE ACCOUNT+INVOICE = m.Account FOR INVHDR.ORDER = m.ORDER AND INVHDR.Status <> 'V' AND ;
                               INVHDR.STORE = m.Store AND INVHDR.PIKTKT = m.PIKTKT
                  IF FOUND()              
                    M.INVOICE= INVHDR.INVOICE
                  ENDIF   
                ENDIF             
                SELECT INVHDR
                gfSetOrder(lcINVHOrd)
                IF EMPTY(M.INVOICE)
                  SELECT Consinvh
                  IF gfSeek(m.PIKTKT,'Consinvh','PIKTKT')
                    M.INVOICE= Consinvh.INVOICE
                  ENDIF 
                ENDIF 
                *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][Start]
                =gfSeek(m.Style,'Style','Style')
                m.cMadein = Style.cconsinfo1
                *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][End]

                INSERT INTO  (lcDetailF) FROM MEMVAR
              *B608331,3 MMT 11/25/2007 fix bugs of prinitng the Same style Carton twice[Start]
              ELSE
                REPLACE Rto1 WITH  Rto1 + Pack_lin.Qty1,;
                        Rto2 With  Rto2 + Pack_lin.Qty2,;
                        Rto3 WITH  Rto3 + Pack_lin.Qty3,;
                        Rto4 WITH  Rto4 + Pack_lin.Qty4,;
                        Rto5 WITH  Rto5 + Pack_lin.Qty5,;
                        Rto6 WITH  Rto6 + Pack_lin.Qty6,;
                        Rto7 WITH  Rto7 + Pack_lin.Qty7,;
                        Rto8 WITH  Rto8 + Pack_lin.Qty8,;
                        CaseQty  WITH  Rto8+Rto7+Rto6+Rto5+Rto4+Rto3+Rto2+Rto1 IN  (lcDetailF)
                *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][Start]
                REPLACE CaseWght WITH CaseWght + Pack_lin.weight IN (lcDetailF) 
                *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][End]

              ENDIF
              *B608331,3 MMT 11/25/2007 fix bugs of prinitng the Same style Carton twice[End]
              *B608331,4 MMT 11/28/2007 fix bugs of prinitng the Same style Carton twice[Start]
            ENDIF    
          ENDIF   
          *B608331,4 MMT 11/28/2007 fix bugs of prinitng the Same style Carton twice[End]
                
            ENDSCAN
          ENDSCAN 
        ENDIF 
        
        IF !SEEK(m.order,lcDetailF) AND gfSeek(m.account,'INVHDR','INVHDRA')
          SELECT INVHDR
          lcINVHOrd = ORDER()
          =gfSetOrder('INVHDRA')  
          gfSeek(m.account,'INVHDR','INVHDRA') 
          SCAN REST WHILE ACCOUNT+INVOICE = m.account FOR Order = m.order AND Store =m.store AND INVHDR.Status <> 'V' AND INVHDR.Consol # 'Y' 
            m.InVoice = INVHDR.invoice
            *B608482,1 MMT 03/26/2008 Fix bug of not prinitng Carrier and notes in case of there is packing list[Start]
            m.Carrier= ALLTRIM(gfCodDes(INVHDR.ShipVia ,'SHIPVIA'))
            *B608482,1 MMT 03/26/2008 Fix bug of not prinitng Carrier and notes in case of there is packing list[End]

            SELECT INVLINE
            lcINVLOrd = ORDER()
            gfSetOrder('INVLINE')
            IF gfSeek(INVHDR.INVOICE)
              m.Tot_Cart =  INVHDR.cartons 
              lnCrtStyle = 0
              lnLastCart = 0 
              m.INVOICE  =  INVHDR.INVOICE
              lnCart = m.Tot_Cart 
              SCAN REST WHILE INVOICE+STR(LINENO,6) = m.INVOICE FOR Order = m.order 
                =gfSEEK('S'+INVLINE.SCALE,'scale')
                m.CaseQty = INVLINE.PPQty 
                m.siz1= SCALE.Sz1
                m.siz2= SCALE.Sz2
                m.siz3= SCALE.Sz3
                m.siz4= SCALE.Sz4
                m.siz5= SCALE.Sz5
                m.siz6= SCALE.Sz6
                m.siz7= SCALE.Sz7
                m.siz8= SCALE.Sz8
                =gfSEEK('P'+INVLINE.SCALE+INVLINE.PREPAK,'scale')
                m.Rto1= SCALE.Pp1
                m.Rto2= SCALE.Pp2
                m.Rto3= SCALE.Pp3
                m.Rto4= SCALE.Pp4
                m.Rto5= SCALE.Pp5
                m.Rto6= SCALE.Pp6
                m.Rto7= SCALE.Pp7
                m.Rto8= SCALE.Pp8
                m.CaseQty  = m.Rto8+m.Rto7+m.Rto6+m.Rto5+m.Rto4+m.Rto3+m.Rto2+m.Rto1
                m.style= SUBSTR(INVLINE.STYLE,1,lnMjrLen)
                m.color = gfCodDes(SUBSTR(INVLINE.STYLE,lnMjrLen+2,lnColorLen) , 'COLOR')
                =gfSEEK(INVLINE.Style,"STYLE")
                lnCrtStyle = IIF(STYLE.Qty_Ctn=0,1,CEILING(TotQty/STYLE.Qty_Ctn))
                *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][Start]
                m.CaseWght = m.CaseQty * STYLE.NSTYWEIGHT
                *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][End]
                FOR lnI = 1  TO lnCrtStyle
                  m.No_Cart= lnLastCart + lnI
                  *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][Start]
                  =gfSeek(m.Style,'Style','Style')
                  m.cMadein = Style.cconsinfo1
                  *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][End]

                  INSERT INTO  (lcDetailF) FROM MEMVAR
                ENDFOR     
                lnLastCart = lnLastCart + lnCrtStyle
                lnCart = lnCart - lnCrtStyle
             ENDSCAN 
             IF m.Tot_Cart < lnLastCart
                REPLACE Tot_Cart  WITH lnLastCart ALL IN (lcDetailF) FOR INVOICE = m.Invoice AND PIKTKT = m.piktkt
             ENDIF 
           ENDIF 
            SELECT INVLINE
            gfSetOrder(lcINVLOrd)
         ENDSCAN 
         SELECT INVHDR
         =gfSetOrder(lcINVHOrd)   
         SELECT CONSINVH
         SCAN FOR order = m.order AND Store = m.store
            m.invoice  = Consinvh.INVOICE
            *B608482,1 MMT 03/26/2008 Fix bug of not prinitng Carrier and notes in case of there is packing list[Start]
            m.Carrier= ALLTRIM(gfCodDes(Consinvh.ShipVia ,'SHIPVIA'))
            *B608482,1 MMT 03/26/2008 Fix bug of not prinitng Carrier and notes in case of there is packing list[End]
             m.Tot_Cart =  Consinvh.cartons 
             lnCrtStyle = 0
             lnLastCart = 0 
             lnCart = m.Tot_Cart 
             SELECT Consinvl &&INVOICE+STORE+ORDER+STYLE+STR(LINENO,6)
             IF gfSeek(m.invoice+Consinvh.Store+m.ORDER)
               SCAN REST WHILE INVOICE+STORE+ORDER+STYLE+STR(LINENO,6) = m.invoice+m.Store+m.ORDER
                  =gfSEEK('S'+Consinvl.SCALE,'scale')
                  =gfSeek(INVOICE+STR(LINENO,6),'INVLINE','INVLINE')
                  =gfSEEK('P'+INVLINE.SCALE+INVLINE.PREPAK,'scale')
                  m.Rto1= SCALE.Pp1
                  m.Rto2= SCALE.Pp2
                  m.Rto3= SCALE.Pp3
                  m.Rto4= SCALE.Pp4
                  m.Rto5= SCALE.Pp5
                  m.Rto6= SCALE.Pp6
                  m.Rto7= SCALE.Pp7
                  m.Rto8= SCALE.Pp8
                  m.CaseQty  = m.Rto8+m.Rto7+m.Rto6+m.Rto5+m.Rto4+m.Rto3+m.Rto2+m.Rto1
                  m.siz1= SCALE.Sz1
                  m.siz2= SCALE.Sz2
                  m.siz3= SCALE.Sz3
                  m.siz4= SCALE.Sz4
                  m.siz5= SCALE.Sz5
                  m.siz6= SCALE.Sz6
                  m.siz7= SCALE.Sz7
                  m.siz8= SCALE.Sz8
                  m.style= SUBSTR(Consinvl.STYLE,1,lnMjrLen)
                  m.color = gfCodDes(SUBSTR(Consinvl.STYLE,lnMjrLen+2,lnColorLen) , 'COLOR')
                  =gfSEEK(Consinvl.Style,"STYLE")
                  lnCrtStyle = IIF(STYLE.Qty_Ctn=0,1,CEILING(TotQty/STYLE.Qty_Ctn))
                  *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][Start]
                  m.CaseWght = m.CaseQty * STYLE.NSTYWEIGHT
                  *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][End]
                  FOR lnI = 1  TO lnCrtStyle
                    m.No_Cart= lnLastCart + lnI
                    *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][Start]
                    =gfSeek(m.Style,'Style','Style')
                    m.cMadein = Style.cconsinfo1
                    *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][End]

                    INSERT INTO  (lcDetailF) FROM MEMVAR
                  ENDFOR     
                  lnLastCart = lnLastCart + lnCrtStyle
                  lnCart = lnCart - lnCrtStyle
               ENDSCAN 
               IF m.Tot_Cart < lnLastCart
                  REPLACE Tot_Cart  WITH lnLastCart ALL IN (lcDetailF) FOR INVOICE = m.Invoice AND PIKTKT = m.piktkt
               ENDIF 
             ENDIF 
          ENDSCAN 
       ENDIF 
       
       
       IF !SEEK(m.order,lcDetailF) 
         SELECT PIKTKT
         lcOrderPIK = ORDER()
         gfSetorder('ORDPIK')
         IF gfSeek(m.order,'PIKTKT','ORDPIK')
           SELECT PIKTKT
           SCAN REST WHILE ORDER+PIKTKT = m.order FOR Status # 'X'
             SELECT ordline
             gfsetorder('ordline')

             =gfSeek('O'+m.order,'ordline','ordline')
             m.piktkt = PIKTKT.piktkt
             lnTotCrt = 0
             SELECT ordline
             SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = 'O'+m.order FOR PIKTKT = PIKTKT.PIKTKT 

               =gfSEEK(ORDLINE.Style,"STYLE")
               lnTotCrt = lnTotCrt  + IIF(STYLE.Qty_Ctn=0,1,CEILING(IIF(TOTPIK=0,TotQTY,TOTPIK)/STYLE.Qty_Ctn))
             ENDSCAN
             m.Tot_Cart = lnTotCrt
             lnCrtStyle = 0
             lnCart = m.Tot_Cart 
             lnLastCart = 0
           
             SELECT ordline
             gfsetorder('ordline')

             =gfSeek('O'+m.order,'ordline','ordline')
             SELECT ordline
             SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = 'O'+ m.order FOR PIKTKT = PIKTKT.PIKTKT 
               =gfSEEK('S'+ORDLINE.SCALE,'scale')
               m.CaseQty = ORDLINE.PPQty 
               m.siz1= SCALE.Sz1
               m.siz2= SCALE.Sz2
               m.siz3= SCALE.Sz3
               m.siz4= SCALE.Sz4
               m.siz5= SCALE.Sz5
               m.siz6= SCALE.Sz6
               m.siz7= SCALE.Sz7
               m.siz8= SCALE.Sz8
               =gfSEEK('P'+ORDLINE.SCALE+ORDLINE.PREPAK,'scale')
               m.Rto1= SCALE.Pp1
               m.Rto2= SCALE.Pp2
               m.Rto3= SCALE.Pp3
               m.Rto4= SCALE.Pp4
               m.Rto5= SCALE.Pp5
               m.Rto6= SCALE.Pp6
               m.Rto7= SCALE.Pp7
               m.Rto8= SCALE.Pp8
               m.CaseQty  = m.Rto8+m.Rto7+m.Rto6+m.Rto5+m.Rto4+m.Rto3+m.Rto2+m.Rto1
               m.style= SUBSTR(ORDLINE.STYLE,1,lnMjrLen)
               m.color = gfCodDes(SUBSTR(ORDLINE.STYLE,lnMjrLen+2,lnColorLen) , 'COLOR')
               
               =gfSEEK(ORDLINE.STYLE,"STYLE")
               lnCrtStyle = IIF(STYLE.Qty_Ctn=0,1,CEILING(TotQty/STYLE.Qty_Ctn))
               *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][Start]
               m.CaseWght = m.CaseQty * STYLE.NSTYWEIGHT
               *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][End]
               FOR lnI = 1  TO lnCrtStyle
                 m.No_Cart= lnLastCart + lnI
                 *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][Start]
                 =gfSeek(m.Style,'Style','Style')
                 m.cMadein = Style.cconsinfo1
                 *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][End]

                 INSERT INTO  (lcDetailF) FROM MEMVAR
               ENDFOR     
               lnLastCart = lnLastCart + lnCrtStyle
               lnCart = lnCart - lnCrtStyle
             ENDSCAN
           ENDSCAN 
         ENDIF 
           m.piktkt = ''
           SELECT ordline
           gfsetorder('ordline')

           =gfSeek('O'+m.order,'ordline','ordline')
           lnTotCrt = 0
           SELECT ordline
           SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = 'O'+m.order  FOR EMPTY(PIKTKT)
             =gfSEEK(ORDLINE.Style,"STYLE")
             lnTotCrt = lnTotCrt  + IIF(STYLE.Qty_Ctn=0,1,CEILING(IIF(TOTPIK=0,TotQTY,TOTPIK)/STYLE.Qty_Ctn))
           ENDSCAN
            
           m.Tot_Cart = lnTotCrt
           lnCrtStyle = 0
           lnCart = m.Tot_Cart 
           lnLastCart = 0
         
           =gfSeek('O'+m.order,'ordline','ordline')
           SELECT ordline
           SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = 'O'+ m.order FOR EMPTY(PIKTKT)
             =gfSEEK('S'+ORDLINE.SCALE,'scale')
             m.CaseQty = ORDLINE.PPQty 
             m.siz1= SCALE.Sz1
             m.siz2= SCALE.Sz2
             m.siz3= SCALE.Sz3
             m.siz4= SCALE.Sz4
             m.siz5= SCALE.Sz5
             m.siz6= SCALE.Sz6
             m.siz7= SCALE.Sz7
             m.siz8= SCALE.Sz8
             =gfSEEK('P'+ORDLINE.SCALE+ORDLINE.PREPAK,'scale')
             m.Rto1= SCALE.Pp1
             m.Rto2= SCALE.Pp2
             m.Rto3= SCALE.Pp3
             m.Rto4= SCALE.Pp4
             m.Rto5= SCALE.Pp5
             m.Rto6= SCALE.Pp6
             m.Rto7= SCALE.Pp7
             m.Rto8= SCALE.Pp8
             m.CaseQty  = m.Rto8+m.Rto7+m.Rto6+m.Rto5+m.Rto4+m.Rto3+m.Rto2+m.Rto1
             m.style= SUBSTR(ORDLINE.STYLE,1,lnMjrLen)
             m.color = gfCodDes(SUBSTR(ORDLINE.STYLE,lnMjrLen+2,lnColorLen) , 'COLOR')
             
             =gfSEEK(ORDLINE.STYLE,"STYLE")
             lnCrtStyle = IIF(STYLE.Qty_Ctn=0,1,CEILING(TotQty/STYLE.Qty_Ctn))
             *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][Start]
             m.CaseWght = m.CaseQty * STYLE.NSTYWEIGHT
             *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][End]

             FOR lnI = 1  TO lnCrtStyle
               m.No_Cart= lnLastCart + lnI
               *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][Start]
               =gfSeek(m.Style,'Style','Style')
               m.cMadein = Style.cconsinfo1
               *C201808,1 MMT 04/13/2016 Print case weight and consumer info. in Custom Shipping label[T20150819.0025][End]

               INSERT INTO  (lcDetailF) FROM MEMVAR
             ENDFOR     
             lnLastCart = lnLastCart + lnCrtStyle
             lnCart = lnCart - lnCrtStyle
           ENDSCAN
            SELECT PIKTKT
           gfSetorder(lcOrderPIK)

           ENDIF 
       
    ENDCASE 
*B608331,1 MMT 10/31/2007 fix bugs of wrong case qty and error while preview[End]    

*B608331,1 MMT 10/31/2007 fix bugs of wrong case qty and error while preview[Start]
*!*      =gfseek(ORDER+STORE,'Pack_Hdr')
*!*      m.Tot_Cart = Pack_Hdr.Tot_Cart
*!*      
*!*      *B608331,1 MMT 10/31/2007 fix bugs of wrong case qty and error while preview[Start]
*!*      IF NOT (oAriaApplication.ActiveModuleID ='AR') OR EMPTY(m.invoice)
*!*      *B608331,1 MMT 10/31/2007 fix bugs of wrong case qty and error while preview[End]
*!*      
*!*        =gfseek(ORDER,'InvLine')
*!*        m.invoice = InvLine.Invoice
*!*        
*!*      *B608331,1 MMT 10/31/2007 fix bugs of wrong case qty and error while preview[Start]  
*!*      *IF gfseek(ORDER+Pack_Hdr.PACK_NO,'BOL_LIN')
*!*      ENDIF   
*!*      IF gfseek(ORDER+STORE,'Pack_Hdr') AND gfseek(ORDER+Pack_Hdr.PACK_NO,'BOL_LIN')
*!*      *B608331,1 MMT 10/31/2007 fix bugs of wrong case qty and error while preview[End]
*!*      
*!*        gfseek(BOL_LIN.BOL_NO,'BOL_HDR')
*!*        m.Bol_No= BOL_LIN.BOL_NO
*!*        MUCB = PADL(ALLTRIM(lfManufID(m.Bol_No)) , 7 , '0') + PADL(ALLTRIM(m.Bol_No) , 9 , '0')
*!*        MUCB = MUCB + lfCheckDgt(MUCB,'E')
*!*        m.Bol_No = MUCB
*!*        lcShipVia = BOL_Hdr.ShipVia
*!*        lcCarrier = BOL_Hdr.Carrier
*!*      ENDIF
*!*      m.Carrier= IIF(EMPTY(lcCarrier),ALLTRIM(gfCodDes(OrdHdr.ShipVia ,'SHIPVIA')),lcCarrier)
*!*      IF gfseek(Order,'PIKTKT')
*!*        m.piktkt = PIKTKT.PikTkt
*!*        
*!*        *B608331,1 MMT 10/29/2007 fix bugs of wrong case qty and error while preview[Start]
*!*        IF PIKTKT.STATUS = 'X'
*!*          LOOP 
*!*        ENDIF 
*!*        *B608331,1 MMT 10/29/2007 fix bugs of wrong case qty and error while preview[End]
*!*      ENDIF
*!*      
*!*      
*!*      *B608331,1 MMT 10/29/2007 fix bugs of wrong case qty and error while preview[Start]
*!*      IF gfseek(ORDER+STORE,'Pack_Hdr')
*!*      *B608331,1 MMT 10/29/2007 fix bugs of wrong case qty and error while preview[End]
*!*      
*!*        SELECT PAck_Lin
*!*        =gfSEEK(Pack_Hdr.Pack_No)
*!*        SCAN REST WHILE PACK_NO+STR(NO_CART,4)+STYLE+DYELOT = Pack_Hdr.Pack_No
*!*          m.No_Cart= PAck_Lin.No_Cart
*!*          m.style= SUBSTR(pack_Lin.STYLE,1,lnMjrLen)
*!*          m.color = gfCodDes(SUBSTR(pack_Lin.STYLE,lnMjrLen+2,lnColorLen) , 'COLOR')
*!*          =gfSEEK(Pack_Hdr.ACCOUNT+Pack_Hdr.STORE+Pack_Hdr.ORDER+STR(pack_Lin.nordlineno,6),'ORDLINE')
*!*          =gfSEEK('S'+ORDLINE.SCALE,'scale')
*!*          m.siz1= SCALE.Sz1
*!*          m.siz2= SCALE.Sz2
*!*          m.siz3= SCALE.Sz3
*!*          m.siz4= SCALE.Sz4
*!*          m.siz5= SCALE.Sz5
*!*          m.siz6= SCALE.Sz6
*!*          m.siz7= SCALE.Sz7
*!*          m.siz8= SCALE.Sz8
*!*          m.CaseQty = ORDLINE.PPQty 
*!*          =gfSEEK('P'+ORDLINE.SCALE+ORDLINE.PREPAK,'scale')
*!*          m.Rto1= SCALE.Pp1
*!*          m.Rto2= SCALE.Pp2
*!*          m.Rto3= SCALE.Pp3
*!*          m.Rto4= SCALE.Pp4
*!*          m.Rto5= SCALE.Pp5
*!*          m.Rto6= SCALE.Pp6
*!*          m.Rto7= SCALE.Pp7
*!*          m.Rto8= SCALE.Pp8
*!*  		
*!*          *B608331,1 MMT 10/26/2007 fix bugs of wrong case qty and error while preview[Start]
*!*          m.CaseQty  = m.Rto8+m.Rto7+m.Rto6+m.Rto5+m.Rto4+m.Rto3+m.Rto2+m.Rto1
*!*    	    *B608331,1 MMT 10/26/2007 fix bugs of wrong case qty and error while preview[End]
*!*  		
*!*          INSERT INTO  (lcDetailF) FROM MEMVAR
*!*        ENDSCAN
*!*      *B608331,1 MMT 10/29/2007 fix bugs of wrong case qty and error while preview[Start]
*!*      ENDIF
*!*      *B608331,1 MMT 10/29/2007 fix bugs of wrong case qty and error while preview[End]
*!*      
*!*      
*!*      *B608331,1 MMT 10/29/2007 fix bugs of wrong case qty and error while preview[Start]
*!*      IF !gfseek(ORDER+STORE,'Pack_Hdr') AND !EMPTY(m.invoice) AND gfSeek(m.invoice,'INVHDR') AND INVHDR.Status # 'V'
*!*        
*!*        IF INVHDR.CONSOL = 'Y' .AND. gfSEEK(INVHDR.INVOICE,'CONSINVH')
*!*  	    SELECT CONSINVH
*!*     	    lnLab = 0
*!*          SCAN WHILE INVOICE+STORE+ORDER+PIKTKT = INVHDR.INVOICE+STORE+ORDER
*!*             lnLab = lnLab + CONSINVH.CARTONS
*!*          ENDSCAN
*!*          m.Tot_Cart = lnLab
*!*        ELSE
*!*          m.Tot_Cart =  INVHDR.cartons 
*!*        ENDIF
*!*        
*!*        SELECT INVLINE
*!*        =gfsetorder('INVLINE')
*!*        =gfseek(m.invoice,'InvLine')
*!*        lnCrtStyle = 0
*!*        lnCart = m.Tot_Cart 
*!*        lnLastCart = 0
*!*        SCAN REST WHILE INVOICE+STR(LINENO,6) = m.invoice
*!*          =gfSEEK('S'+INVLINE.SCALE,'scale')
*!*          m.CaseQty = INVLINE.PPQty 
*!*          m.siz1= SCALE.Sz1
*!*          m.siz2= SCALE.Sz2
*!*          m.siz3= SCALE.Sz3
*!*          m.siz4= SCALE.Sz4
*!*          m.siz5= SCALE.Sz5
*!*          m.siz6= SCALE.Sz6
*!*          m.siz7= SCALE.Sz7
*!*          m.siz8= SCALE.Sz8
*!*          =gfSEEK('P'+INVLINE.SCALE+INVLINE.PREPAK,'scale')
*!*          m.Rto1= SCALE.Pp1
*!*          m.Rto2= SCALE.Pp2
*!*          m.Rto3= SCALE.Pp3
*!*          m.Rto4= SCALE.Pp4
*!*          m.Rto5= SCALE.Pp5
*!*          m.Rto6= SCALE.Pp6
*!*          m.Rto7= SCALE.Pp7
*!*          m.Rto8= SCALE.Pp8
*!*          m.CaseQty  = m.Rto8+m.Rto7+m.Rto6+m.Rto5+m.Rto4+m.Rto3+m.Rto2+m.Rto1
*!*          m.style= SUBSTR(INVLINE.STYLE,1,lnMjrLen)
*!*          m.color = gfCodDes(SUBSTR(INVLINE.STYLE,lnMjrLen+2,lnColorLen) , 'COLOR')

*!*          =gfSEEK(INVLINE.Style,"STYLE")
*!*          lnCrtStyle = IIF(STYLE.Qty_Ctn=0,1,CEILING(TotQty/STYLE.Qty_Ctn))
*!*          FOR lnI = 1  TO lnCrtStyle
*!*            m.No_Cart= lnLastCart + lnI
*!*            INSERT INTO  (lcDetailF) FROM MEMVAR
*!*          ENDFOR     
*!*          lnLastCart = lnLastCart + lnCrtStyle
*!*          lnCart = lnCart - lnCrtStyle
*!*          
*!*        ENDSCAN
*!*        IF m.Tot_Cart < lnLastCart
*!*          REPLACE Tot_Cart  WITH lnLastCart ALL IN (lcDetailF) FOR INVOICE = m.Invoice
*!*        ENDIF 
*!*        
*!*        SELECT INVLINE
*!*        =gfsetorder('INVLINEO')
*!*      ENDIF 
*!*      
*!*      IF !gfseek(ORDER+STORE,'Pack_Hdr') AND EMPTY(m.invoice) AND !EMPTY(M.PIKTKT)
*!*      
*!*        lcOrder = ORDER
*!*        
*!*        =gfSeek('O'+order,'ordline','ordline')
*!*        lnTotCrt = 0
*!*        SELECT ordline
*!*        SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = 'O'+lcOrder FOR PIKTKT = m.piktkt
*!*          =gfSEEK(ORDLINE.Style,"STYLE")
*!*          lnTotCrt = lnTotCrt  + IIF(STYLE.Qty_Ctn=0,1,CEILING(TotPIK/STYLE.Qty_Ctn))
*!*        ENDSCAN
*!*        
*!*        m.Tot_Cart = lnTotCrt
*!*        lnCrtStyle = 0
*!*        lnCart = m.Tot_Cart 
*!*        lnLastCart = 0

*!*        =gfSeek('O'+lcOrder,'ordline','ordline')
*!*        SELECT ordline
*!*        SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = 'O'+lcOrder FOR PIKTKT = m.piktkt
*!*          =gfSEEK('S'+ORDLINE.SCALE,'scale')
*!*          m.CaseQty = ORDLINE.PPQty 
*!*          m.siz1= SCALE.Sz1
*!*          m.siz2= SCALE.Sz2
*!*          m.siz3= SCALE.Sz3
*!*          m.siz4= SCALE.Sz4
*!*          m.siz5= SCALE.Sz5
*!*          m.siz6= SCALE.Sz6
*!*          m.siz7= SCALE.Sz7
*!*          m.siz8= SCALE.Sz8
*!*          =gfSEEK('P'+ORDLINE.SCALE+ORDLINE.PREPAK,'scale')
*!*          m.Rto1= SCALE.Pp1
*!*          m.Rto2= SCALE.Pp2
*!*          m.Rto3= SCALE.Pp3
*!*          m.Rto4= SCALE.Pp4
*!*          m.Rto5= SCALE.Pp5
*!*          m.Rto6= SCALE.Pp6
*!*          m.Rto7= SCALE.Pp7
*!*          m.Rto8= SCALE.Pp8
*!*          m.CaseQty  = m.Rto8+m.Rto7+m.Rto6+m.Rto5+m.Rto4+m.Rto3+m.Rto2+m.Rto1
*!*          m.style= SUBSTR(ORDLINE.STYLE,1,lnMjrLen)
*!*          m.color = gfCodDes(SUBSTR(ORDLINE.STYLE,lnMjrLen+2,lnColorLen) , 'COLOR')
*!*          
*!*          =gfSEEK(ORDLINE.STYLE,"STYLE")
*!*          lnCrtStyle = IIF(STYLE.Qty_Ctn=0,1,CEILING(TotQty/STYLE.Qty_Ctn))
*!*          FOR lnI = 1  TO lnCrtStyle
*!*            m.No_Cart= lnLastCart + lnI
*!*            INSERT INTO  (lcDetailF) FROM MEMVAR
*!*          ENDFOR     
*!*          lnLastCart = lnLastCart + lnCrtStyle
*!*          lnCart = lnCart - lnCrtStyle
*!*        ENDSCAN
*!*      ENDIF
*B608331,1 MMT 10/29/2007 fix bugs of wrong case qty and error while preview[End]
*B608331,1 MMT 10/31/2007 fix bugs of wrong case qty and error while preview[End]
    
  ENDSCAN

  SELECT (lcDetailF)
  GO TOP 
  lcTmpIndex= IIF(lcXTYPE = 'O','ORDER+STORE',IIF(lcXTYPE = 'I','INVOICE+STORE','PIKTKT'))
  SET RELATION TO &lcTmpIndex INTO (lcMainF) ADDITIVE

  SET ORDER TO lcBOLHOrder IN BOL_Hdr
  SET ORDER TO lcBOLLOrder IN BOL_Lin
  SET ORDER TO lcPKHOrder IN PACK_HDR
  SET ORDER TO lcPKLOrder IN PACK_LIN
  SET ORDER TO lcInvLOrder IN InvLine
  SET ORDER TO lcSOLOrder IN OrdLine
  SET ORDER TO lcPIKTOrder IN PIKTKT

  *-- END OF FUNCTION lfGetData.

  *!*************************************************************
  *! Name      : lfManufID
  *! Developer : Hend Ghanem
  *! Date      : 21/03/2005
  *! Purpose   : validate pbDtlNew button
  *!*************************************************************
  *! Example   : = lfManufID(Bill of lading no)
  *!*************************************************************
FUNCTION lfManufID
  LPARAMETERS lcBolNo
  PRIVATE lcManuf_Id,laRltdFld, MUCCLEVEL

  lcManuf_Id = gfGetMemVar('XMANUFID',oAriaApplication.ActiveCompanyID)
  MUCCLEVEL  = gfGetMemVar('M_UCCDIV',oAriaApplication.ActiveCompanyID)

  *-- Maintain UCC manufaturer ID at division level
  IF MUCCLEVEL = 'N'
    DECLARE laRltdFld[1,2]
    STORE '' TO laRltdFld,LCUPCMAN
    laRltdFld[1,1] = "CUPCMAN"
    laRltdFld[1,2] = 'LCUPCMAN'
    =gfRltFld(ORDHDR.cDivision,@laRltdFld,'CDIVISION')
    lcManuf_Id = IIF(EMPTY(LCUPCMAN),lcManuf_Id,LCUPCMAN)
  ENDIF
  RETURN ALLTRIM(lcManuf_Id)
  *-- End of lfManufID.
  *:**************************************************************************
  *:* Name        : lfCheckDgt
  *! Developer    : Hend Ghanem
  *! Date         : 21/03/2005
  *:* Purpose     :
  *:***************************************************************************
  *:* Called from :
  *:***************************************************************************
  *:* Parameters : None
  *:***************************************************************************
  *:* Return      : None
  *:***************************************************************************
  *:* Example     :  = lfCheckDgt()
  *:***************************************************************************
FUNCTION lfCheckDgt
  PARAMETER lcBOLNo, lcType
  PRIVATE lnChkDigit ,lnSumOdd  ,lnSumEven ,lnCount
  STORE 0 TO lnChkDigit ,lnSumOdd  ,lnSumEven ,lnTop

  lnTop = LEN(lcBOLNo)
  FOR lnCount = 1 TO lnTop STEP 2
    lnSumOdd  = lnSumOdd  + VAL(SUBSTR(lcBOLNo,lnCount     , 1))
    lnSumEven = lnSumEven + VAL(SUBSTR(lcBOLNo,lnCount + 1 , 1))
  ENDFOR
  IF lcType = 'O'
    lnChkDigit = MOD(lnSumOdd*3 + lnSumEven , 10)
  ELSE
    lnChkDigit = MOD(lnSumOdd + lnSumEven*3 , 10)
  ENDIF
  RETURN(IIF(lnChkDigit=0,'0',STR(INT(10-lnChkDigit),1)))
  *:***************************************************************************
