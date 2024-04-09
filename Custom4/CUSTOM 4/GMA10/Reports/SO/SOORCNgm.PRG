*:************************************************************************
*: Program file   : SOORCNGM.PRG      (C101807)
*: Program desc.  : Order confirmation Form (GM)
*: For Report     : SOORGNGM.RPT
*: System         : Aria Advantage Series. 
*: Module         : Sales Order (SO)
*: Developer      : Heba Mohamed Amin (HMA)
*: Date           : 06/20/2004
*: Reference      : C038190,1
*:************************************************************************
*: Calls :
*:    Procedures : ....
*:    Functions  : lfPrtONte , lfAddRec   , lfPrnConti , lfPrnLast
*:                 lfPrnHdr  , lfPrnNotPd , lfGetPack  , lfGetOdHdr
*:                 lfPrPakCon.
*:************************************************************************
*: Passed Parameters  : None
*:************************************************************************
*: Example : DO SOOECNGM
*:************************************************************************
*:Modifications:
*: B038190,1 HMA 11/01/2004 fix the bug of select more than 24 value in season or division.
*: E127602,1 HMA 05/09/2005 add Style Picture to report layout.
*: B127675,1 HMA add Mask of phone 
*: B128323,1 HMA 06/02/2005 Increase field width in some calculated fields 
*: B128349,1 HMA 06/05/2005 Print the report even if the selected order has no details
*: B128422,1 HMA 06/08/2005 Print Wrong Line Notes in case print Consolidating =Yes
*: B130206,1 AYM 11/28/2005 PRINT WRONG AVG PRICE WITH PACK WHEN SELECT PACK ID =YES AND PACK DETAILS=NO
*: B610752,1 MMT 06/22/2014 Custom Order Confirmation form GM prints UPC in 2 lines[T20131219.0020]
*: B610754,1 HES 06/22/2014 Let the size break down appear regardless the UPC and SKU [GMA Upgrade]
*: B610760,1 HES 06/24/2014 Fix a bug in loogscroll array [GMA uprade issue# 79]
*: C201629,1 MMT 07/06/2014 Add Pack ID filter to the Order confirmation Report Option Grid[T20131219.0020 - Issue#81]
*: B610784,1 MMT 07/29/2014 Custom Order confirmation form does not print size w/o UPC [T20140723.0012] 
*! B611039,1 MMT 08/16/2015 Custom Order Confirmation form is not priniting file#[T20150806.0042]
*! B611106,1 MMT 01/26/2016 Error while previewing Order Confirmation form GM[T20160119.0012]
*:********************************************************************************************
#INCLUDE R:\Aria4xp\reports\so\soorcngm.H
LOCAL llPPrtpak,llInStor
PUBLIC lnTotQty,lnTotOrd,lnMer_disc,lnMer_Amnt,lnNet_Amnt

*-- Set the memory variables.
*-- llRpPack    --> Print the Pack_Id (.T.-->.F.).
*-- llRpPrTCon  --> Print consolidating all stores by Style/Color (.T.-->.F.).
*-- llRpOrdLNt  --> IF The Add notes to order lines seting is set to No
*-- llSQty      --> Print Sizes Qty (.T.-->.F.).

IF loOGScroll.llOGFltCh   &&If Filter Changed
  STORE '' TO lcOrder,lcAccount,lcMulti,lcStore,laAddress,lcTermData,lcShipVia,lcDivison,;
              lcBtName
  STORE 0 TO lnNum ,lnAmount
  STORE .F. To NOTELOOP,llYes

  IF !llRpPack 
    llRpPrtPak= .T.
  ENDIF 

  DECLARE laSoldTo[5,1] , laShipTo[5,1] , laDivLName[1,2]
  laSoldTo = ''                     && Array to hold the Sold To address
  laShipTo = ''                     && Array to hold the Ship To address
  lcDivLName = ''
  laDivLName[1,1] = 'DIVLNAME'     && Array to get the Division long name
  laDivLName[1,2] = 'lcDivLName'

  *-- If No records selected.
  SELECT ORDHDR
  SET SKIP TO
  *C201629,1 MMT 07/06/2014 Add Pack ID filter to the Order confirmation Report Option Grid[T20131219.0020 - Issue#81][Start]
  IF !"ORDLINE" $ UPPER(SET("Relation"))
    SET RELATION TO cordtype+ ORDER INTO Ordline ADDITIVE
  ENDIF    
  *C201629,1 MMT 07/06/2014 Add Pack ID filter to the Order confirmation Report Option Grid[T20131219.0020 - Issue#81][End]
  LOCATE FOR &lcRpExp
  IF !FOUND()
    *--No records to display.
    = gfModalGen('TRM00052B00000','DIALOG' )
    *-- Variable to do EndReport Or Don't.
    llNoRec = .T.
    RETURN .F.
   
  ELSE
    *-- Variable to do EndReport Or Don't.
    llNoRec = .F.
  ENDIF

  *-- Check if there is only one Order selected.
  PRIVATE lnOrderPos
  lnOrderPos = lfItmPos('ORDHDR.ORDER')   && get Get Order Fixed filter Position
  IF (UPPER(loOGScroll.laOGVrFlt[lnOrderPos,5]) = LANG_Soorcngm_InList AND USED(loOGScroll.laOGVrFlt[lnOrderPos,6]) AND RECCOUNT(loOGScroll.laOGVrFlt[lnOrderPos,6])=1)
    IF lfvChkOrd()
      DO lpvGMAStor
    ENDIF
  ENDIF

*B128349,1 HMA 06/05/2005 Print the report even if the selected order has no details[Begin]
*!*    SELECT (lcTempOrd)

*!*    IF !FOUND()
*!*      *--No records to display.
*!*      = gfModalGen('TRM00052B00000','DIALOG' )
*!*      *-- Variable to do EndReport Or Don't.
*!*      llNoRec = .T.
*!*      RETURN .F.
*!*     
*!*    ELSE
*!*      *-- Variable to do EndReport Or Don't.
*!*      llNoRec = .F.
*!*    ENDIF
*B128349,1 HMA 06/05/2005 Print the report even if the selected order has no details [End]

  *-- Get the style and color
  lcMajTitl  = ALLTRIM(gfItemMask('HI')) 
  lnMjrPct   = LEN(gfItemMask('PI')) && (item mask)  Major AND nonMajor Picture 
  IF llRpPrtCon
      *--Create these 2 files to collect the data in case of consolidating by store.
    lcTmpFile = loOgScroll.gfTempName()    &&temp file for order lines information.
    lcOrdLinSt  = loOgScroll.gfTempName()    &&temp file for order header information. 

    DIMENSION laTmpFile[22,4]

  	laTmpFile[1,1]='Style'
  	laTmpFile[1,2]='C'
  	laTmpFile[1,3]=lnMjrPct
  	laTmpFile[1,4]=0

  	laTmpFile[2,1]='nTotQty'
  	laTmpFile[2,2]='N'
  	laTmpFile[2,3]=7
  	laTmpFile[2,4]=0

  	laTmpFile[3,1]='nPrice'
  	laTmpFile[3,2]='N'
  	laTmpFile[3,3]=8
  	laTmpFile[3,4]=2

  	laTmpFile[4,1]='pack'
  	laTmpFile[4,2]='M'
  	laTmpFile[4,3]=10
  	laTmpFile[4,4]=0

  	laTmpFile[5,1]='nSugRetPri'
  	laTmpFile[5,2]='N'
  	laTmpFile[5,3]=8
  	laTmpFile[5,4]=2

  	laTmpFile[6,1]='pack_id'
  	laTmpFile[6,2]='C'
  	laTmpFile[6,3]=16
  	laTmpFile[6,4]=0

  	laTmpFile[7,1]='cPkColor'
  	laTmpFile[7,2]='C'
  	laTmpFile[7,3]=6
  	laTmpFile[7,4]=0

  	laTmpFile[8,1]='cPckSize'
  	laTmpFile[8,2]='C'
  	laTmpFile[8,3]=3
  	laTmpFile[8,4]=0

  	laTmpFile[9,1]='cPkVersion'
  	laTmpFile[9,2]='C'
  	laTmpFile[9,3]=4
  	laTmpFile[9,4]=0

  	laTmpFile[10,1]='Store'
  	laTmpFile[10,2]='C'
  	laTmpFile[10,3]=8
  	laTmpFile[10,4]=0

  	laTmpFile[11,1]='nQty1'
  	laTmpFile[11,2]='N'
  	laTmpFile[11,3]=8
  	laTmpFile[11,4]=0

  	laTmpFile[12,1]='nQty2'
  	laTmpFile[12,2]='N'
  	laTmpFile[12,3]=8
  	laTmpFile[12,4]=0

  	laTmpFile[13,1]='nQty3'
  	laTmpFile[13,2]='N'
  	laTmpFile[13,3]=8
  	laTmpFile[13,4]=0

  	laTmpFile[14,1]='nQty4'
  	laTmpFile[14,2]='N'
  	laTmpFile[14,3]=8
  	laTmpFile[14,4]=0

  	laTmpFile[15,1]='nQty5'
  	laTmpFile[15,2]='N'
  	laTmpFile[15,3]=8
  	laTmpFile[15,4]=0

  	laTmpFile[16,1]='nQty6'
  	laTmpFile[16,2]='N'
  	laTmpFile[16,3]=8
  	laTmpFile[16,4]=0

  	laTmpFile[17,1]='nQty7'
  	laTmpFile[17,2]='N'
  	laTmpFile[17,3]=8
  	laTmpFile[17,4]=0

  	laTmpFile[18,1]='nQty8'
  	laTmpFile[18,2]='N'
  	laTmpFile[18,3]=8
  	laTmpFile[18,4]=0
  	                
  	laTmpFile[19,1]='cDesc'
  	laTmpFile[19,2]='C'
  	laTmpFile[19,3]=20
  	laTmpFile[19,4]=0
                            
  	laTmpFile[20,1]='cOrdType'
  	laTmpFile[20,2]='C'
  	laTmpFile[20,3]=1
  	laTmpFile[20,4]=0
  	
    laTmpFile[21,1]='Order'
  	laTmpFile[21,2]='C'
  	laTmpFile[21,3]=6
  	laTmpFile[21,4]=0
    
    laTmpFile[22,1]='Dyelot'
  	laTmpFile[22,2]='C'
  	laTmpFile[22,3]=10
  	laTmpFile[22,4]=0
  	
    IF !llRpPack
      IF !llConfig
        lcTmpFilIdx= "cOrdType+Order+Style"
      ELSE 
        lcTmpFilIdx= "cOrdType+Order+Style+Dyelot"    
      ENDIF 
    ELSE  
      IF !llConfig
        lcTmpFilIdx= " cOrdType+Order+Style + Pack_Id +cpkcolor+cpcksize+ cPkVersion"
      ELSE 
        lcTmpFilIdx= " cOrdType+Order+Style+Dyelot + Pack_Id +cpkcolor+cpcksize+ cPkVersion"
      ENDIF 
    ENDIF  

    gfCrtTmp(lcTmpFile,@laTmpFile,lcTmpFilIdx,lcTmpFile,.F.)
  *HMA,127296[BEGIN]  
  *!*    DIMENSION laLineSt[18,4]

  *!*  	laLineSt[1,1]='cOrdType'
  *!*  	laLineSt[1,2]='C'
  *!*  	laLineSt[1,3]=1
  *!*  	laLineSt[1,4]=0

  *!*  	laLineSt[2,1]='Order'
  *!*  	laLineSt[2,2]='C'
  *!*  	laLineSt[2,3]=6
  *!*  	laLineSt[2,4]=0

  *!*  	laLineSt[3,1]='Style'
  *!*  	laLineSt[3,2]='C'
  *!*  	laLineSt[3,3]=lnMjrPct
  *!*  	laLineSt[3,4]=0

  *!*  	laLineSt[4,1]='nTotQty'
  *!*  	laLineSt[4,2]='N'
  *!*  	laLineSt[4,3]=7
  *!*  	laLineSt[4,4]=0

  *!*  	laLineSt[5,1]='nPrice'
  *!*  	laLineSt[5,2]='N'
  *!*  	laLineSt[5,3]=8
  *!*  	laLineSt[5,4]=2

  *!*  	laLineSt[6,1]='cDesc'
  *!*  	laLineSt[6,2]='C'
  *!*  	laLineSt[6,3]=20
  *!*  	laLineSt[6,4]=0

  *!*  	laLineSt[7,1]='nSugRetPri'
  *!*  	laLineSt[7,2]='N'
  *!*  	laLineSt[7,3]=8
  *!*  	laLineSt[7,4]=2

  *!*  	laLineSt[8,1]='Store'
  *!*  	laLineSt[8,2]='C'
  *!*  	laLineSt[8,3]=8
  *!*  	laLineSt[8,4]=0

  *!*  	laLineSt[9,1]='Pack_id'
  *!*  	laLineSt[9,2]='C'
  *!*  	laLineSt[9,3]=16
  *!*  	laLineSt[9,4]=0

  *!*  	laLineSt[10,1]='NOte_mem'
  *!*  	laLineSt[10,2]='M'
  *!*  	laLineSt[10,3]=10
  *!*  	laLineSt[10,4]=0

  *!*  	laLineSt[11,1]='CpckSize'
  *!*  	laLineSt[11,2]='C'
  *!*  	laLineSt[11,3]=3
  *!*  	laLineSt[11,4]=0

  *!*  	laLineSt[12,1]='CpkColor'
  *!*  	laLineSt[12,2]='C'
  *!*  	laLineSt[12,3]=6
  *!*  	laLineSt[12,4]=0

  *!*  	laLineSt[13,1]='CpkVersion'
  *!*  	laLineSt[13,2]='C'
  *!*  	laLineSt[13,3]=4
  *!*  	laLineSt[13,4]=0

  *!*  	laLineSt[14,1]='lcPckPrPic'
  *!*  	laLineSt[14,2]='C'
  *!*  	laLineSt[14,3]=20
  *!*  	laLineSt[14,4]=0

  *!*  	laLineSt[15,1]='lnAmount'
  *!*  	laLineSt[15,2]='N'
  *!*  	laLineSt[15,3]=14
  *!*  	laLineSt[15,4]=2
  *!*  	
  *!*  	laLineSt[16,1]='Desc1'
  *!*      laLineSt[16,2]='C'
  *!*      laLineSt[16,3]=60
  *!*      laLineSt[16,4]=0

  *!*  	laLineSt[17,1]='llNot_Mem'
  *!*      laLineSt[17,2]='L'
  *!*      laLineSt[17,3]=1
  *!*      laLineSt[17,4]=0

  *!*  	laLineSt[18,1]='Dyelot'
  *!*      laLineSt[18,2]='C'
  *!*      laLineSt[18,3]=10
  *!*      laLineSt[18,4]=0 
  *!*      
  *!*    IF !llConfig
  *!*      lcOrdIdx="cOrdType+order+Style + Pack_Id +cpkcolor+cpcksize+ cPkVersion"
  *!*    ELSE 
  *!*      lcOrdIdx="cOrdType+order+Style+Dyelot + Pack_Id +cpkcolor+cpcksize+ cPkVersion"
  *!*    ENDIF 
  *!*      gfCrtTmp(lcOrdLinSt,@laLineSt,lcOrdIdx,lcOrdLinSt,.F.)
  *!*    llYes =.T.
* E127602,1 HMA 05/09/2005 add Style Picture to report layout [Begin].
*!*      DIMENSION laLineSt[19,4]

*!*      laLineSt[1,1]='cOrdType'
*!*      laLineSt[1,2]='C'
*!*      laLineSt[1,3]=1
*!*      laLineSt[1,4]=0

*!*      laLineSt[2,1]='Order'
*!*      laLineSt[2,2]='C'
*!*      laLineSt[2,3]=6
*!*      laLineSt[2,4]=0

*!*      laLineSt[3,1]='Style'
*!*      laLineSt[3,2]='C'
*!*      laLineSt[3,3]=lnMjrPct
*!*      laLineSt[3,4]=0

*!*      laLineSt[4,1]='nTotQty'
*!*      laLineSt[4,2]='N'
*!*      laLineSt[4,3]=7
*!*      laLineSt[4,4]=0

*!*      laLineSt[5,1]='nPrice'
*!*      laLineSt[5,2]='N'
*!*      laLineSt[5,3]=8
*!*      laLineSt[5,4]=2

*!*      laLineSt[6,1]='cDesc'
*!*      laLineSt[6,2]='C'
*!*      laLineSt[6,3]=20
*!*      laLineSt[6,4]=0

*!*      laLineSt[7,1]='nSugRetPri'
*!*      laLineSt[7,2]='N'
*!*      laLineSt[7,3]=8
*!*      laLineSt[7,4]=2

*!*      laLineSt[8,1]='Store'
*!*      laLineSt[8,2]='C'
*!*      laLineSt[8,3]=8
*!*      laLineSt[8,4]=0

*!*      laLineSt[9,1]='Pack_id'
*!*      laLineSt[9,2]='C'
*!*      laLineSt[9,3]=16
*!*      laLineSt[9,4]=0

*!*      laLineSt[10,1]='NOte_mem'
*!*      laLineSt[10,2]='M'
*!*      laLineSt[10,3]=10
*!*      laLineSt[10,4]=0

*!*      laLineSt[11,1]='CpckSize'
*!*      laLineSt[11,2]='C'
*!*      laLineSt[11,3]=3
*!*      laLineSt[11,4]=0

*!*      laLineSt[12,1]='CpkColor'
*!*      laLineSt[12,2]='C'
*!*      laLineSt[12,3]=6
*!*      laLineSt[12,4]=0

*!*      laLineSt[13,1]='CpkVersion'
*!*      laLineSt[13,2]='C'
*!*      laLineSt[13,3]=4
*!*      laLineSt[13,4]=0

*!*      laLineSt[14,1]='lcPckPrPic'
*!*      laLineSt[14,2]='C'
*!*      laLineSt[14,3]=20
*!*      laLineSt[14,4]=0

*!*      laLineSt[15,1]='lnAmount'
*!*      laLineSt[15,2]='N'
*!*      laLineSt[15,3]=14
*!*      laLineSt[15,4]=2
*!*      
*!*      laLineSt[16,1]='Desc1'
*!*      laLineSt[16,2]='C'
*!*      laLineSt[16,3]=60
*!*      laLineSt[16,4]=0

*!*      laLineSt[17,1]='llNot_Mem'
*!*      laLineSt[17,2]='L'
*!*      laLineSt[17,3]=1
*!*      laLineSt[17,4]=0

*!*      laLineSt[18,1]='Dyelot'
*!*      laLineSt[18,2]='C'
*!*      laLineSt[18,3]=10
*!*      laLineSt[18,4]=0 
*!*        
*!*      laLineSt[19,1]='LineNo'
*!*      laLineSt[19,2]='N'
*!*      laLineSt[19,3]=6
*!*      laLineSt[19,4]=0     
    DIMENSION laLineSt[20,4]

    laLineSt[1,1]='cOrdType'
    laLineSt[1,2]='C'
    laLineSt[1,3]=1
    laLineSt[1,4]=0

    laLineSt[2,1]='Order'
    laLineSt[2,2]='C'
    laLineSt[2,3]=6
    laLineSt[2,4]=0

    laLineSt[3,1]='Style'
    laLineSt[3,2]='C'
    laLineSt[3,3]=lnMjrPct
    laLineSt[3,4]=0

    laLineSt[4,1]='nTotQty'
    laLineSt[4,2]='N'
    laLineSt[4,3]=7
    laLineSt[4,4]=0

    laLineSt[5,1]='nPrice'
    laLineSt[5,2]='N'
    laLineSt[5,3]=8
    laLineSt[5,4]=2

    laLineSt[6,1]='cDesc'
    laLineSt[6,2]='C'
    laLineSt[6,3]=20
    laLineSt[6,4]=0

    laLineSt[7,1]='nSugRetPri'
    laLineSt[7,2]='N'
    laLineSt[7,3]=8
    laLineSt[7,4]=2

    laLineSt[8,1]='Store'
    laLineSt[8,2]='C'
    laLineSt[8,3]=8
    laLineSt[8,4]=0

    laLineSt[9,1]='Pack_id'
    laLineSt[9,2]='C'
    laLineSt[9,3]=16
    laLineSt[9,4]=0

    laLineSt[10,1]='NOte_mem'
    laLineSt[10,2]='M'
    laLineSt[10,3]=10
    laLineSt[10,4]=0

    laLineSt[11,1]='CpckSize'
    laLineSt[11,2]='C'
    laLineSt[11,3]=3
    laLineSt[11,4]=0

    laLineSt[12,1]='CpkColor'
    laLineSt[12,2]='C'
    laLineSt[12,3]=6
    laLineSt[12,4]=0

    laLineSt[13,1]='CpkVersion'
    laLineSt[13,2]='C'
    laLineSt[13,3]=4
    laLineSt[13,4]=0

    laLineSt[14,1]='lcPckPrPic'
    laLineSt[14,2]='C'
    laLineSt[14,3]=20
    laLineSt[14,4]=0

    laLineSt[15,1]='lnAmount'
    laLineSt[15,2]='N'
    laLineSt[15,3]=14
    laLineSt[15,4]=2
    
    laLineSt[16,1]='Desc1'
    laLineSt[16,2]='C'
    laLineSt[16,3]=60
    laLineSt[16,4]=0

    laLineSt[17,1]='llNot_Mem'
    laLineSt[17,2]='L'
    laLineSt[17,3]=1
    laLineSt[17,4]=0

    laLineSt[18,1]='Dyelot'
    laLineSt[18,2]='C'
    laLineSt[18,3]=10
    laLineSt[18,4]=0 
      
    laLineSt[19,1]='LineNo'
    laLineSt[19,2]='N'
    laLineSt[19,3]=6
    laLineSt[19,4]=0     
    
    laLineSt[20,1]='Gobject'
    laLineSt[20,2]='G'
    laLineSt[20,3]=10
    laLineSt[20,4]=0     
* E127602,1 HMA 05/09/2005 add Style Picture to report layout [End].
    IF !llConfig
      lcOrdIdx="cOrdType+order+Style + Pack_Id +cpkcolor+cpcksize+ cPkVersion"
    ELSE 
      lcOrdIdx="cOrdType+order+Style+Dyelot + Pack_Id +cpkcolor+cpcksize+ cPkVersion"
    ENDIF 
      gfCrtTmp(lcOrdLinSt,@laLineSt,lcOrdIdx,lcOrdLinSt,.F.)
    llYes =.T.
  *HMA,127296 [END]
  ENDIF 
  *--add new table to get pack_id and SKU, UPC(Hold Pack & Style details)
  lcTmpDesc = loOgScroll.gfTempName()

  *--SKU1...8 fields is related to SKU per size
  *--UPC1...8 fields is related to UPC per size

  IF !llRpPack

    IF !llRpPrtCon
      IF !llConfig
        lcTmpIndex="cOrdtype+Order+Style+STR(LINENO,6)"
      ELSE 
        lcTmpIndex="cOrdtype+Order+Style+Dyelot+STR(LINENO,6)"
      ENDIF 
    ELSE 
      IF !llConfig
        lcTmpIndex="cOrdtype+Order+Style"
      ELSE 
        lcTmpIndex="cOrdtype+Order+Style+Dyelot"    
      ENDIF 
    ENDIF 

  ELSE 

    IF !llRpPrtCon
      IF !llConfig
        lcTmpIndex="cOrdtype+Order+Pack_Id +cpkcolor+cpcksize+ cPkVersion +Style+STR(LINENO,6)"
      ELSE
        lcTmpIndex="cOrdtype+Order+Pack_Id +cpkcolor+cpcksize+ cPkVersion +Style+Dyelot+STR(LINENO,6)"
      ENDIF 
    ELSE
      IF !llConfig
        lcTmpIndex="cOrdtype+Order+Pack_Id +cpkcolor+cpcksize+ cPkVersion +Style"
      ELSE
        lcTmpIndex="cOrdtype+Order+Pack_Id +cpkcolor+cpcksize+ cPkVersion +Style+Dyelot"
      ENDIF
    ENDIF 

  ENDIF


  DIMENSION laTmpDesc[43,4]

  laTmpDesc[1,1]='Style'
  laTmpDesc[1,2]='C'
  laTmpDesc[1,3]=lnMjrPct
  laTmpDesc[1,4]=0

  laTmpDesc[2,1]='pack_id'
  laTmpDesc[2,2]='C'
  laTmpDesc[2,3]=16
  laTmpDesc[2,4]=0

  laTmpDesc[3,1]='cPkColor'
  laTmpDesc[3,2]='C'
  laTmpDesc[3,3]=6
  laTmpDesc[3,4]=0

  laTmpDesc[4,1]='cPckSize'
  laTmpDesc[4,2]='C'
  laTmpDesc[4,3]=3
  laTmpDesc[4,4]=0

  laTmpDesc[5,1]='cPkVersion'
  laTmpDesc[5,2]='C'
  laTmpDesc[5,3]=4
  laTmpDesc[5,4]=0

  laTmpDesc[6,1]='PackSKU'
  laTmpDesc[6,2]='C'
  laTmpDesc[6,3]=16
  laTmpDesc[6,4]=0

  laTmpDesc[7,1]='PackUPC'
  laTmpDesc[7,2]='C'
  laTmpDesc[7,3]=16
  laTmpDesc[7,4]=0

  laTmpDesc[8,1]='SKU1'
  laTmpDesc[8,2]='C'
  laTmpDesc[8,3]=16
  laTmpDesc[8,4]=0

  laTmpDesc[9,1]='SKU2'
  laTmpDesc[9,2]='C'
  laTmpDesc[9,3]=16
  laTmpDesc[9,4]=0

  laTmpDesc[10,1]='SKU3'
  laTmpDesc[10,2]='C'
  laTmpDesc[10,3]=16
  laTmpDesc[10,4]=0

  laTmpDesc[11,1]='SKU4'
  laTmpDesc[11,2]='C'
  laTmpDesc[11,3]=16
  laTmpDesc[11,4]=0

  laTmpDesc[12,1]='SKU5'
  laTmpDesc[12,2]='C'
  laTmpDesc[12,3]=16
  laTmpDesc[12,4]=0

  laTmpDesc[13,1]='SKU6'
  laTmpDesc[13,2]='C'
  laTmpDesc[13,3]=16
  laTmpDesc[13,4]=0

  laTmpDesc[14,1]='SKU7'
  laTmpDesc[14,2]='C'
  laTmpDesc[14,3]=16
  laTmpDesc[14,4]=0

  laTmpDesc[15,1]='SKU8'
  laTmpDesc[15,2]='C'
  laTmpDesc[15,3]=16
  laTmpDesc[15,4]=0

  laTmpDesc[16,1]='UPC1'
  laTmpDesc[16,2]='C'
  laTmpDesc[16,3]=16
  laTmpDesc[16,4]=0

  laTmpDesc[17,1]='UPC2'
  laTmpDesc[17,2]='C'
  laTmpDesc[17,3]=16
  laTmpDesc[17,4]=0

  laTmpDesc[18,1]='UPC3'
  laTmpDesc[18,2]='C'
  laTmpDesc[18,3]=16
  laTmpDesc[18,4]=0

  laTmpDesc[19,1]='UPC4'
  laTmpDesc[19,2]='C'
  laTmpDesc[19,3]=16
  laTmpDesc[19,4]=0

  laTmpDesc[20,1]='UPC5'
  laTmpDesc[20,2]='C'
  laTmpDesc[20,3]=16
  laTmpDesc[20,4]=0

  laTmpDesc[21,1]='UPC6'
  laTmpDesc[21,2]='C'
  laTmpDesc[21,3]=16
  laTmpDesc[21,4]=0

  laTmpDesc[22,1]='UPC7'
  laTmpDesc[22,2]='C'
  laTmpDesc[22,3]=16
  laTmpDesc[22,4]=0

  laTmpDesc[23,1]='UPC8'
  laTmpDesc[23,2]='C'
  laTmpDesc[23,3]=16
  laTmpDesc[23,4]=0

  laTmpDesc[24,1]='Size1'
  laTmpDesc[24,2]='C'
  laTmpDesc[24,3]=5
  laTmpDesc[24,4]=0

  laTmpDesc[25,1]='Size2'
  laTmpDesc[25,2]='C'
  laTmpDesc[25,3]=5
  laTmpDesc[25,4]=0

  laTmpDesc[26,1]='Size3'
  laTmpDesc[26,2]='C'
  laTmpDesc[26,3]=5
  laTmpDesc[26,4]=0

  laTmpDesc[27,1]='Size4'
  laTmpDesc[27,2]='C'
  laTmpDesc[27,3]=5
  laTmpDesc[27,4]=0

  laTmpDesc[28,1]='Size5'
  laTmpDesc[28,2]='C'
  laTmpDesc[28,3]=5
  laTmpDesc[28,4]=0

  laTmpDesc[29,1]='Size6'
  laTmpDesc[29,2]='C'
  laTmpDesc[29,3]=5
  laTmpDesc[29,4]=0

  laTmpDesc[30,1]='Size7'
  laTmpDesc[30,2]='C'
  laTmpDesc[30,3]=5
  laTmpDesc[30,4]=0

  laTmpDesc[31,1]='Size8'
  laTmpDesc[31,2]='C'
  laTmpDesc[31,3]=5
  laTmpDesc[31,4]=0

  laTmpDesc[32,1]='llStyUPC'
  laTmpDesc[32,2]='L'
  laTmpDesc[32,3]=1
  laTmpDesc[32,4]=0

  laTmpDesc[33,1]='llStySKU'
  laTmpDesc[33,2]='L'
  laTmpDesc[33,3]=1
  laTmpDesc[33,4]=0

  laTmpDesc[34,1]='mUPC'
  laTmpDesc[34,2]='M'
  laTmpDesc[34,3]=60
  laTmpDesc[34,4]=0

  laTmpDesc[35,1]='mSKU'
  laTmpDesc[35,2]='M'
  laTmpDesc[35,3]=60
  laTmpDesc[35,4]=0

  laTmpDesc[36,1]='cSizeDesc'
  laTmpDesc[36,2]='C'
  laTmpDesc[36,3]=10
  laTmpDesc[36,4]=0

  laTmpDesc[37,1]='cOrdType'
  laTmpDesc[37,2]='C'
  laTmpDesc[37,3]=1
  laTmpDesc[37,4]=0

  laTmpDesc[38,1]='Order'
  laTmpDesc[38,2]='C'
  laTmpDesc[38,3]=6
  laTmpDesc[38,4]=0

  laTmpDesc[39,1]='LineNo'
  laTmpDesc[39,2]='N'
  laTmpDesc[39,3]=6
  laTmpDesc[39,4]=0

  laTmpDesc[40,1]='llMupcSku'
  laTmpDesc[40,2]='L'
  laTmpDesc[40,3]=1
  laTmpDesc[40,4]=0

  laTmpDesc[41,1]='Dyelot'
  laTmpDesc[41,2]='C'
  laTmpDesc[41,3]=10
  laTmpDesc[41,4]=0

  laTmpDesc[42,1]='mQty'
  laTmpDesc[42,2]='M'
  laTmpDesc[42,3]=60
  laTmpDesc[42,4]=0

  laTmpDesc[43,1]='mSize'
  laTmpDesc[43,2]='M'
  laTmpDesc[43,3]=60
  laTmpDesc[43,4]=0

  gfCrtTmp(lcTmpDesc,@laTmpDesc,lcTmpIndex,lcTmpDesc,.F.)

  *-- add new file to check pack in case of Print pack Details="NO"
  lcTmpPack = loOgScroll.gfTempName()
  DIMENSION laTmpPack[21,4] 

  laTmpPack[1,1]='pack_id'
  laTmpPack[1,2]='C'
  laTmpPack[1,3]=16
  laTmpPack[1,4]=0

  laTmpPack[2,1]='cPkColor'
  laTmpPack[2,2]='C'
  laTmpPack[2,3]=6
  laTmpPack[2,4]=0

  laTmpPack[3,1]='cPckSize'
  laTmpPack[3,2]='C'
  laTmpPack[3,3]=3
  laTmpPack[3,4]=0

  laTmpPack[4,1]='cPkVersion'
  laTmpPack[4,2]='C'
  laTmpPack[4,3]=4
  laTmpPack[4,4]=0

  laTmpPack[5,1]='Store'
  laTmpPack[5,2]='C'
  laTmpPack[5,3]=8
  laTmpPack[5,4]=0

  laTmpPack[6,1]='nTotQty'
  laTmpPack[6,2]='N'
  laTmpPack[6,3]=7
  laTmpPack[6,4]=0

  laTmpPack[7,1]='nPackPrc'
  laTmpPack[7,2]='N'
  laTmpPack[7,3]=9
  laTmpPack[7,4]=2

  laTmpPack[8,1]='PackUPC'
  laTmpPack[8,2]='C'
  laTmpPack[8,3]=30
  laTmpPack[8,4]=0

  laTmpPack[9,1]='PackSKU'
  laTmpPack[9,2]='C'
  laTmpPack[9,3]=30
  laTmpPack[9,4]=0

  laTmpPack[10,1]='Note_Mem'
  laTmpPack[10,2]='M'
  laTmpPack[10,3]=10
  laTmpPack[10,4]=0

  laTmpPack[11,1]='nAmount'
  laTmpPack[11,2]='N'
  laTmpPack[11,3]=14
  laTmpPack[11,4]=2

  laTmpPack[12,1]='nPackSlPrc'
  laTmpPack[12,2]='N'
  laTmpPack[12,3]=8
  laTmpPack[12,4]=2

  laTmpPack[13,1]='lRange'
  laTmpPack[13,2]='L'
  laTmpPack[13,3]=1
  laTmpPack[13,4]=0

  laTmpPack[14,1]='lPckPrPiec'
  laTmpPack[14,2]='L'
  laTmpPack[14,3]=1
  laTmpPack[14,4]=0

  laTmpPack[15,1]='nPkSlPrice'
  laTmpPack[15,2]='N'
  laTmpPack[15,3]=8
  laTmpPack[15,4]=2

  laTmpPack[16,1]='nCount'
  laTmpPack[16,2]='N'
  laTmpPack[16,3]=7
  laTmpPack[16,4]=0

  laTmpPack[17,1]='nPackQty'
  laTmpPack[17,2]='N'
  laTmpPack[17,3]=7
  laTmpPack[17,4]=0

  laTmpPack[18,1]='ColPack'
  laTmpPack[18,2]='C'
  laTmpPack[18,3]=29
  laTmpPack[18,4]=0
  	
  laTmpPack[19,1]='cOrdType'
  laTmpPack[19,2]='C'
  laTmpPack[19,3]=1
  laTmpPack[19,4]=0

  laTmpPack[20,1]='Order'
  laTmpPack[20,2]='C'
  laTmpPack[20,3]=6
  laTmpPack[20,4]=0

  laTmpPack[21,1]='lcPrcPic'
  laTmpPack[21,2]='C'
  laTmpPack[21,3]=25
  laTmpPack[21,4]=0

  IF !llRpPrTCon
    lcTmpPakIdx="cOrdType+Order+Store+Pack_Id +cpkcolor+cpcksize+ cPkVersion"
  ELSE
    lcTmpPakIdx="cOrdType+Order+Pack_Id+cpkcolor+cpcksize+ cPkVersion"
  ENDIF
  gfCrtTmp(lcTmpPack,@laTmpPack,lcTmpPakIdx,lcTmpPack,.F.)



  *--  Open needed files That didn't open before.
  =gfOpenFile(oAriaApplication.DataDir+'Styleupc',oAriaApplication.DataDir+'styleupc','SH')
  =gfOpenFile(oAriaApplication.DataDir+'Spck_Lin',oAriaApplication.DataDir+'Spck_Lin','SH')
  =gfOpenFile(oAriaApplication.DataDir+'Spck_Hdr',oAriaApplication.DataDir+'Spck_Hdr','SH')

  *SSH
  =gfOpenFile(oAriaApplication.DataDir+'edilibdt',oAriaApplication.DataDir+'Gmatran','SH')
  *ceditrntyp+ceditranno
  
  *SSH
  *--  Initilize variables


  *-- We Have Temp File Called lcTempOrd Fill Of All Data.
  SELECT (lcTempOrd)


  DO CASE

    CASE lcRpSortBy = 'S'
      IF !llConfig 
        INDEX ON CORDTYPE + ORDER + STORE +STYLE + STR(LINENO,6) + Pack_Id +cpkcolor+cpcksize+cPkVersion   TAG (lcTempOrd)
      ELSE
        INDEX ON CORDTYPE + ORDER + STORE +STYLE +DYELOT+ STR(LINENO,6) + Pack_Id +cpkcolor+cpcksize+cPkVersion   TAG (lcTempOrd)    
      ENDIF 
      SET ORDER TO TAG (lcTempOrd)
      lcSortName='Style'

    CASE lcRpSortBy = 'L'
      IF !llConfig 
        INDEX ON CORDTYPE + ORDER + STORE + STR(LINENO,6) + STYLE + Pack_Id +cpkcolor+cpcksize+ cPkVersion   TAG (lcTempOrd)
      ELSE 
        INDEX ON CORDTYPE + ORDER + STORE + STR(LINENO,6) + STYLE +DYELOT + Pack_Id +cpkcolor+cpcksize+ cPkVersion   TAG (lcTempOrd)    
      ENDIF 
      SET ORDER TO TAG (lcTempOrd)
      lcSortName='Line No'

    CASE lcRpSortBy = 'P'
      IF llRpPrTCon
        IF !llConfig        
          INDEX ON CORDTYPE + ORDER + PACK_ID +cpkcolor+cpcksize+ cPkVersion  + STORE  + STYLE +STR(LINENO,6) TAG (lcTempOrd)
        ELSE 
          INDEX ON CORDTYPE + ORDER + PACK_ID +cpkcolor+cpcksize+ cPkVersion  + STORE  + STYLE +DYELOT+STR(LINENO,6) TAG (lcTempOrd)      
        ENDIF 
      ELSE
        IF !llConfig        
          INDEX ON CORDTYPE + ORDER + STORE  + Pack_Id +cpkcolor+cpcksize+ cPkVersion + STYLE+ STR(LINENO,6)  TAG (lcTempOrd)
        ELSE 
          INDEX ON CORDTYPE + ORDER + STORE  + Pack_Id +cpkcolor+cpcksize+ cPkVersion + STYLE+DYELOT+ STR(LINENO,6)  TAG (lcTempOrd)
        ENDIF 
      ENDIF
      SET ORDER TO TAG (lcTempOrd)
      lcSortName='Pack'

  ENDCASE

  *--Fill the company address variables.
  *--Center the address
  laCompAdd1 = ''
  laCompAdd2 = SPACE(ROUND((80-LEN(laCompAdd[2]))/2,0))+laCompAdd[2]
  laCompAdd3 = SPACE(ROUND((80-LEN(laCompAdd[3]))/2,0))+laCompAdd[3]
  laCompAdd4 = SPACE(ROUND((80-LEN(laCompAdd[4]))/2,0))+laCompAdd[4]

  PRIVATE lcSelectCommand,lnResult
  lcSelectCommand=[SELECT CCOM_PHON FROM SYCCOMP WHERE Ccomp_id=']+oAriaApplication.ActiveCompanyID+[']
  lnResult = oAriaApplication.remotesystemdata.execute(lcSelectCommand,"","SYCCOMP","",oAriaApplication.SystemConnectionString,3,"",SET("DATASESSION")) 

  IF lnResult >= 1 
*B127675,1 HMA add Mask of phone  [Begin]
*    laCompAdd[5] = SyCcomp.Ccom_phon
    lcPhonePict=gfPhoneTem()
    laCompAdd[5] =TRANSFORM(SyCcomp.Ccom_phon,'@R '+lcPhonePict)
*B127675,1 HMA add Mask of phone  [End]
  ENDIF 

  laCompAdd5 = SPACE(ROUND((80-LEN(ALLTRIM(laCompAdd[5])))/2,0))+laCompAdd[5]
  SELECT (lcTempOrd)
  IF llOneMulti AND !EMPTY(laStores)
    SET FILTER TO ASCAN(laStores,ALLTRIM(Store)) <> 0

    *-- Updating the Recap in case of Temp. EDI. 
    IF lcRpOrdTyp = "T"
      SELECT (lcTempRcp)
      LOCATE
      REPLACE Qty1   WITH 0 , Qty2   WITH 0 , Qty3   WITH 0 , Qty4   WITH 0 ,;
              Qty5   WITH 0 , Qty6   WITH 0 , Qty7   WITH 0 , Qty8   WITH 0 ,;
              TotQty WITH 0 , Amount WITH 0

      SELECT (lcTempOrd)
      SCAN
        SCATTER MEMVAR MEMO
        SELECT (lcTempRcp)
        IF !llConfig 
          llSeek=SEEK(m.Account+m.Style)
        ELSE
          llSeek=SEEK(m.Account+m.Style+m.Dyelot)
        ENDIF 
        IF !llSeek      
          INSERT INTO (lcTempRcp) (Account,Style,Dyelot,nSRP,Scale) VALUES ;
                                  (m.Account,m.Style,m.Dyelot,m.nSugRetPri,m.Scale)
        ENDIF
        REPLACE Qty1   WITH Qty1   + m.Qty1 ,;
                Qty2   WITH Qty2   + m.Qty2 ,;
                Qty3   WITH Qty3   + m.Qty3 ,;
                Qty4   WITH Qty4   + m.Qty4 ,;
                Qty5   WITH Qty5   + m.Qty5 ,;
                Qty6   WITH Qty6   + m.Qty6 ,;
                Qty7   WITH Qty7   + m.Qty7 ,;
                Qty8   WITH Qty8   + m.Qty8 ,;
                TotQty WITH TotQty + m.TotQty ,;
                Amount WITH Amount + m.TotQty*m.Price
      ENDSCAN
    ENDIF
  ELSE
    SET FILTER TO
  ENDIF

*B128422,1 HMA 06/08/2005 Print Wrong Line Notes in case print Consolidating =Yes [Begin]
*--remove these Unneccesary  Variables
*  STORE 1 TO lnNoteLn,lnSvNoteLn
*  llNrmPrt = .T.
*B128422,1 HMA 06/08/2005 Print Wrong Line Notes in case print Consolidating =Yes [End]

*SSH
  *DIMENSION laheader[41,4]
  DIMENSION laheader[42,4]
*SSH
  laheader[1,1]='Order'
  laheader[1,2]='C'
  laheader[1,3]=6
  laheader[1,4]=0

  laheader[2,1]='cOrdType'
  laheader[2,2]='C'
  laheader[2,3]=1
  laheader[2,4]=0

  laheader[3,1]='Store'
  laheader[3,2]='C'
  laheader[3,3]=8
  laheader[3,4]=0

  laheader[4,1]='Dept'
  laheader[4,2]='C'
  laheader[4,3]=5
  laheader[4,4]=0

  laheader[5,1]='Note1'
  laheader[5,2]='C'
  laheader[5,3]=30
  laheader[5,4]=0

  laheader[6,1]='Note2'
  laheader[6,2]='C'
  laheader[6,3]=30
  laheader[6,4]=0

  laheader[7,1]='lnMer_Disc'
  laheader[7,2]='N'
  laheader[7,3]=20
  laheader[7,4]=2

  laheader[8,1]='Entered'
  laheader[8,2]='D'
  laheader[8,3]=8
  laheader[8,4]=0

  laheader[9,1]='Start'
  laheader[9,2]='D'
  laheader[9,3]=8
  laheader[9,4]=0

  laheader[10,1]='Complete'
  laheader[10,2]='D'
  laheader[10,3]=8
  laheader[10,4]=0

  laheader[11,1]='Rep1'
  laheader[11,2]='C'
  laheader[11,3]=3
  laheader[11,4]=0

  laheader[12,1]='Rep2'
  laheader[12,2]='C'
  laheader[12,3]=3
  laheader[12,4]=0

  laheader[13,1]='Custpo'
  laheader[13,2]='C'
  laheader[13,3]=15
  laheader[13,4]=0

  laheader[14,1]='cCusVend'
  laheader[14,2]='C'
  laheader[14,3]=15
  laheader[14,4]=0

  laheader[15,1]='lccomName'
  laheader[15,2]='C'
  laheader[15,3]=30
  laheader[15,4]=0

  laheader[16,1]='lcCmpAdd1'
  laheader[16,2]='C'
  laheader[16,3]=40
  laheader[16,4]=0

  laheader[17,1]='lcCmpAdd2'
  laheader[17,2]='C'
  laheader[17,3]=40
  laheader[17,4]=0

  laheader[18,1]='lcCmpAdd3'
  laheader[18,2]='C'
  laheader[18,3]=40
  laheader[18,4]=0

  laheader[19,1]='lcCmpAdd5'
  laheader[19,2]='C'
  laheader[19,3]=40
  laheader[19,4]=0

  laheader[20,1]='BtName'
  laheader[20,2]='C'
  laheader[20,3]=80
  laheader[20,4]=0

  laheader[21,1]='ShptName'
  laheader[21,2]='C'
  laheader[21,3]=80
  laheader[21,4]=0

  laheader[22,1]='Mnotes'
  laheader[22,2]='M'
  laheader[22,3]=40
  laheader[22,4]=0

  laheader[23,1]='lcSoldTo1'
  laheader[23,2]='C'
  laheader[23,3]=40
  laheader[23,4]=0

  laheader[24,1]='lcSoldTo2'
  laheader[24,2]='C'
  laheader[24,3]=40
  laheader[24,4]=0

  laheader[25,1]='lcSoldTo3'
  laheader[25,2]='C'
  laheader[25,3]=40
  laheader[25,4]=0

  laheader[26,1]='lcShipTo1'
  laheader[26,2]='C'
  laheader[26,3]=40
  laheader[26,4]=0

  laheader[27,1]='lcShipTo2'
  laheader[27,2]='C'
  laheader[27,3]=40
  laheader[27,4]=0

  laheader[28,1]='lcShipTo3'
  laheader[28,2]='C'
  laheader[28,3]=40
  laheader[28,4]=0

  laheader[29,1]='TermData'
  laheader[29,2]='C'
  laheader[29,3]=30
  laheader[29,4]=0

  laheader[30,1]='ShipVia'
  laheader[30,2]='C'
  laheader[30,3]=30
  laheader[30,4]=0

  laheader[31,1]='Season'
  laheader[31,2]='C'
  laheader[31,3]=30
  laheader[31,4]=0

  laheader[32,1]='Account'
  laheader[32,2]='C'
  laheader[32,3]=5
  laheader[32,4]=0
* B128323,1 HMA 06/02/2005 Increase field width of some calculated fields [Begin]
*!*    laheader[33,1]='lnTotOrd'
*!*    laheader[33,2]='N'
*!*    laheader[33,3]=8
*!*    laheader[33,4]=2

*!*    laheader[34,1]='lnAmount'
*!*    laheader[34,2]='N'
*!*    laheader[34,3]=14
*!*    laheader[34,4]=2

*!*    laheader[35,1]='lnMer_Amnt'
*!*    laheader[35,2]='N'
*!*    laheader[35,3]=8
*!*    laheader[35,4]=2

*!*    laheader[36,1]='lnTotQty'
*!*    laheader[36,2]='N'
*!*    laheader[36,3]=7
*!*    laheader[36,4]=0

*!*    laheader[37,1]='lnNet_Amnt'
*!*    laheader[37,2]='N'
*!*    laheader[37,3]=8
*!*    laheader[37,4]=2

  
  laheader[33,1]='lnTotOrd'
  laheader[33,2]='N'
  laheader[33,3]=14
  laheader[33,4]=2

  laheader[34,1]='lnAmount'
  laheader[34,2]='N'
  laheader[34,3]=14
  laheader[34,4]=2

  laheader[35,1]='lnMer_Amnt'
  laheader[35,2]='N'
  laheader[35,3]=14
  laheader[35,4]=2

  laheader[36,1]='lnTotQty'
  laheader[36,2]='N'
  laheader[36,3]=10
  laheader[36,4]=0

  laheader[37,1]='lnNet_Amnt'
  laheader[37,2]='N'
  laheader[37,3]=14
  laheader[37,4]=2
* B128323,1 HMA 06/02/2005 Increase field width of some calculated fields [End]
  laheader[38,1]='llNote1'
  laheader[38,2]='L'
  laheader[38,3]=1
  laheader[38,4]=0

  laheader[39,1]='llNote2'
  laheader[39,2]='L'
  laheader[39,3]=1
  laheader[39,4]=0

  laheader[40,1]='llMnotes'
  laheader[40,2]='L'
  laheader[40,3]=1
  laheader[40,4]=0
  
  laheader[41,1]='llMulti'
  laheader[41,2]='L'
  laheader[41,3]=1
  laheader[41,4]=0

*SSH
  
  laheader[42,1]='cEdiTranNo'
  laheader[42,2]='C'
  laheader[42,3]=6
  laheader[42,4]=0

*SSH
  IF !llRpPrtCon
    lcOrdHdr= loOgScroll.gfTempName()
    gfCrtTmp(lcOrdHdr,@laheader,"cOrdType+Order+Store",lcOrdHdr,.F.)
  ELSE 
    lcOrdHdrSt= loOgScroll.gfTempName()
    gfCrtTmp(lcOrdHdrSt,@laheader,"cOrdType+Order+Store",lcOrdHdrSt,.F.)
  ENDIF 

* E127602,1 HMA 05/09/2005 add Style Picture to report layout [Begin].
*!*    DIMENSION laLine[45,4]
*!*    laLine[1,1]='Order'
*!*    laLine[1,2]='C'
*!*    laLine[1,3]=6
*!*    laLine[1,4]=0

*!*    laLine[2,1]='Style'
*!*    laLine[2,2]='C'
*!*    laLine[2,3]=lnMjrPct
*!*    laLine[2,4]=0

*!*    laLine[3,1]='StyDesc'
*!*    laLine[3,2]='C'
*!*    laLine[3,3]=20
*!*    laLine[3,4]=0

*!*    laLine[4,1]='nSugRetPri'
*!*    laLine[4,2]='N'
*!*    laLine[4,3]=8
*!*    laLine[4,4]=2

*!*    laLine[5,1]='Price'
*!*    laLine[5,2]='N'
*!*    laLine[5,3]=8
*!*    laLine[5,4]=2

*!*    laLine[6,1]='TotQty'
*!*    laLine[6,2]='N'
*!*    laLine[6,3]=7
*!*    laLine[6,4]=0

*!*    laLine[7,1]='lnTotOrd'
*!*    laLine[7,2]='N'
*!*    laLine[7,3]=8
*!*    laLine[7,4]=2

*!*    laLine[8,1]='lnAmount'
*!*    laLine[8,2]='N'
*!*    laLine[8,3]=14
*!*    laLine[8,4]=2

*!*    laLine[9,1]='lnMer_Amnt'
*!*    laLine[9,2]='N'
*!*    laLine[9,3]=8
*!*    laLine[9,4]=2

*!*    laLine[10,1]='lnTotQty'
*!*    laLine[10,2]='N'
*!*    laLine[10,3]=7
*!*    laLine[10,4]=0

*!*    laLine[11,1]='lnNet_Amnt'
*!*    laLine[11,2]='N'
*!*    laLine[11,3]=8
*!*    laLine[11,4]=2

*!*    laLine[12,1]='Desc1'
*!*    laLine[12,2]='C'
*!*    laLine[12,3]=60
*!*    laLine[12,4]=0

*!*    laLine[13,1]='Pack_id'
*!*    laLine[13,2]='C'
*!*    laLine[13,3]=16
*!*    laLine[13,4]=0

*!*    laLine[14,1]='NOte_mem'
*!*    laLine[14,2]='M'
*!*    laLine[14,3]=10
*!*    laLine[14,4]=0

*!*    laLine[15,1]='CpckSize'
*!*    laLine[15,2]='C'
*!*    laLine[15,3]=3
*!*    laLine[15,4]=0

*!*    laLine[16,1]='CpkColor'
*!*    laLine[16,2]='C'
*!*    laLine[16,3]=6
*!*    laLine[16,4]=0

*!*    laLine[17,1]='CpkVersion'
*!*    laLine[17,2]='C'
*!*    laLine[17,3]=4
*!*    laLine[17,4]=0

*!*    laLine[18,1]='nPkSiPrice'
*!*    laLine[18,2]='N'
*!*    laLine[18,3]=8
*!*    laLine[18,4]=2

*!*    laLine[19,1]='nPackQty'
*!*    laLine[19,2]='N'
*!*    laLine[19,3]=7
*!*    laLine[19,4]=0

*!*    laLine[20,1]='nAmount'
*!*    laLine[20,2]='N'
*!*    laLine[20,3]=14
*!*    laLine[20,4]=2

*!*    laLine[21,1]='lcPckPrPic'
*!*    laLine[21,2]='C'
*!*    laLine[21,3]=30
*!*    laLine[21,4]=0

*!*    laLine[22,1]='lcLocStore'
*!*    laLine[22,2]='C'
*!*    laLine[22,3]=6
*!*    laLine[22,4]=0

*!*    laLine[23,1]='lcPCSV'
*!*    laLine[23,2]='C'
*!*    laLine[23,3]=60
*!*    laLine[23,4]=0

*!*    laLine[24,1]='cOrdType'
*!*    laLine[24,2]='C'
*!*    laLine[24,3]=1
*!*    laLine[24,4]=0

*!*    laLine[25,1]='Store'
*!*    laLine[25,2]='C'
*!*    laLine[25,3]=8
*!*    laLine[25,4]=0

*!*    laLine[26,1]='LineNo'
*!*    laLine[26,2]='N'
*!*    laLine[26,3]=6
*!*    laLine[26,4]=0

*!*    laLine[27,1]='Qty1'
*!*    laLine[27,2]='N'
*!*    laLine[27,3]=6
*!*    laLine[27,4]=0

*!*    laLine[28,1]='Qty2'
*!*    laLine[28,2]='N'
*!*    laLine[28,3]=6
*!*    laLine[28,4]=0

*!*    laLine[29,1]='Qty3'
*!*    laLine[29,2]='N'
*!*    laLine[29,3]=6
*!*    laLine[29,4]=0

*!*    laLine[30,1]='Qty4'
*!*    laLine[30,2]='N'
*!*    laLine[30,3]=6
*!*    laLine[30,4]=0

*!*    laLine[31,1]='Qty5'
*!*    laLine[31,2]='N'
*!*    laLine[31,3]=6
*!*    laLine[31,4]=0

*!*    laLine[32,1]='Qty6'
*!*    laLine[32,2]='N'
*!*    laLine[32,3]=6
*!*    laLine[32,4]=0

*!*    laLine[33,1]='Qty7'
*!*    laLine[33,2]='N'
*!*    laLine[33,3]=6
*!*    laLine[33,4]=0

*!*    laLine[34,1]='Qty8'
*!*    laLine[34,2]='N'
*!*    laLine[34,3]=6
*!*    laLine[34,4]=0


*!*    laLine[35,1]='nQty1'
*!*    laLine[35,2]='N'
*!*    laLine[35,3]=6
*!*    laLine[35,4]=0

*!*    laLine[36,1]='nQty2'
*!*    laLine[36,2]='N'
*!*    laLine[36,3]=6
*!*    laLine[36,4]=0

*!*    laLine[37,1]='nQty3'
*!*    laLine[37,2]='N'
*!*    laLine[37,3]=6
*!*    laLine[37,4]=0

*!*    laLine[38,1]='nQty4'
*!*    laLine[38,2]='N'
*!*    laLine[38,3]=6
*!*    laLine[38,4]=0

*!*    laLine[39,1]='nQty5'
*!*    laLine[39,2]='N'
*!*    laLine[39,3]=6
*!*    laLine[39,4]=0

*!*    laLine[40,1]='nQty6'
*!*    laLine[40,2]='N'
*!*    laLine[40,3]=6
*!*    laLine[40,4]=0

*!*    laLine[41,1]='nQty7'
*!*    laLine[41,2]='N'
*!*    laLine[41,3]=6
*!*    laLine[41,4]=0

*!*    laLine[42,1]='nQty8'
*!*    laLine[42,2]='N'
*!*    laLine[42,3]=6
*!*    laLine[42,4]=0

*!*    laLine[43,1]='ColPack'
*!*    laLine[43,2]='C'
*!*    laLine[43,3]=29
*!*    laLine[43,4]=0

*!*    laLine[44,1]='llNot_Mem'
*!*    laLine[44,2]='L'
*!*    laLine[44,3]=1
*!*    laLine[44,4]=0

*!*    laLine[45,1]='Dyelot'
*!*    laLine[45,2]='C'
*!*    laLine[45,3]=10
*!*    laLine[45,4]=0

  DIMENSION laLine[46,4]
  laLine[1,1]='Order'
  laLine[1,2]='C'
  laLine[1,3]=6
  laLine[1,4]=0

  laLine[2,1]='Style'
  laLine[2,2]='C'
  laLine[2,3]=lnMjrPct
  laLine[2,4]=0

  laLine[3,1]='StyDesc'
  laLine[3,2]='C'
  laLine[3,3]=20
  laLine[3,4]=0

  laLine[4,1]='nSugRetPri'
  laLine[4,2]='N'
  laLine[4,3]=8
  laLine[4,4]=2

  laLine[5,1]='Price'
  laLine[5,2]='N'
  laLine[5,3]=8
  laLine[5,4]=2

  laLine[6,1]='TotQty'
  laLine[6,2]='N'
  laLine[6,3]=7
  laLine[6,4]=0
* B128323,1 HMA 06/02/2005 Increase field width of some calculated fields [Begin]
*!*    laLine[7,1]='lnTotOrd'
*!*    laLine[7,2]='N'
*!*    laLine[7,3]=8
*!*    laLine[7,4]=2

*!*    laLine[8,1]='lnAmount'
*!*    laLine[8,2]='N'
*!*    laLine[8,3]=14
*!*    laLine[8,4]=2

*!*    laLine[9,1]='lnMer_Amnt'
*!*    laLine[9,2]='N'
*!*    laLine[9,3]=8
*!*    laLine[9,4]=2

*!*    laLine[10,1]='lnTotQty'
*!*    laLine[10,2]='N'
*!*    laLine[10,3]=7
*!*    laLine[10,4]=0

*!*    laLine[11,1]='lnNet_Amnt'
*!*    laLine[11,2]='N'
*!*    laLine[11,3]=8
*!*    laLine[11,4]=2

  laLine[7,1]='lnTotOrd'
  laLine[7,2]='N'
  laLine[7,3]=14
  laLine[7,4]=2

  laLine[8,1]='lnAmount'
  laLine[8,2]='N'
  laLine[8,3]=14
  laLine[8,4]=2

  laLine[9,1]='lnMer_Amnt'
  laLine[9,2]='N'
  laLine[9,3]=14
  laLine[9,4]=2

  laLine[10,1]='lnTotQty'
  laLine[10,2]='N'
  laLine[10,3]=10
  laLine[10,4]=0

  laLine[11,1]='lnNet_Amnt'
  laLine[11,2]='N'
  laLine[11,3]=14
  laLine[11,4]=2
* B128323,1 HMA 06/02/2005 Increase field width of some calculated fields [End]
  laLine[12,1]='Desc1'
  laLine[12,2]='C'
  laLine[12,3]=60
  laLine[12,4]=0

  laLine[13,1]='Pack_id'
  laLine[13,2]='C'
  laLine[13,3]=16
  laLine[13,4]=0

  laLine[14,1]='NOte_mem'
  laLine[14,2]='M'
  laLine[14,3]=10
  laLine[14,4]=0

  laLine[15,1]='CpckSize'
  laLine[15,2]='C'
  laLine[15,3]=3
  laLine[15,4]=0

  laLine[16,1]='CpkColor'
  laLine[16,2]='C'
  laLine[16,3]=6
  laLine[16,4]=0

  laLine[17,1]='CpkVersion'
  laLine[17,2]='C'
  laLine[17,3]=4
  laLine[17,4]=0

  laLine[18,1]='nPkSiPrice'
  laLine[18,2]='N'
  laLine[18,3]=8
  laLine[18,4]=2

  laLine[19,1]='nPackQty'
  laLine[19,2]='N'
  laLine[19,3]=7
  laLine[19,4]=0

  laLine[20,1]='nAmount'
  laLine[20,2]='N'
  laLine[20,3]=14
  laLine[20,4]=2

  laLine[21,1]='lcPckPrPic'
  laLine[21,2]='C'
  laLine[21,3]=30
  laLine[21,4]=0

  laLine[22,1]='lcLocStore'
  laLine[22,2]='C'
  laLine[22,3]=6
  laLine[22,4]=0

  laLine[23,1]='lcPCSV'
  laLine[23,2]='C'
  laLine[23,3]=60
  laLine[23,4]=0

  laLine[24,1]='cOrdType'
  laLine[24,2]='C'
  laLine[24,3]=1
  laLine[24,4]=0

  laLine[25,1]='Store'
  laLine[25,2]='C'
  laLine[25,3]=8
  laLine[25,4]=0

  laLine[26,1]='LineNo'
  laLine[26,2]='N'
  laLine[26,3]=6
  laLine[26,4]=0

  laLine[27,1]='Qty1'
  laLine[27,2]='N'
  laLine[27,3]=6
  laLine[27,4]=0

  laLine[28,1]='Qty2'
  laLine[28,2]='N'
  laLine[28,3]=6
  laLine[28,4]=0

  laLine[29,1]='Qty3'
  laLine[29,2]='N'
  laLine[29,3]=6
  laLine[29,4]=0

  laLine[30,1]='Qty4'
  laLine[30,2]='N'
  laLine[30,3]=6
  laLine[30,4]=0

  laLine[31,1]='Qty5'
  laLine[31,2]='N'
  laLine[31,3]=6
  laLine[31,4]=0

  laLine[32,1]='Qty6'
  laLine[32,2]='N'
  laLine[32,3]=6
  laLine[32,4]=0

  laLine[33,1]='Qty7'
  laLine[33,2]='N'
  laLine[33,3]=6
  laLine[33,4]=0

  laLine[34,1]='Qty8'
  laLine[34,2]='N'
  laLine[34,3]=6
  laLine[34,4]=0


  laLine[35,1]='nQty1'
  laLine[35,2]='N'
  laLine[35,3]=6
  laLine[35,4]=0

  laLine[36,1]='nQty2'
  laLine[36,2]='N'
  laLine[36,3]=6
  laLine[36,4]=0

  laLine[37,1]='nQty3'
  laLine[37,2]='N'
  laLine[37,3]=6
  laLine[37,4]=0

  laLine[38,1]='nQty4'
  laLine[38,2]='N'
  laLine[38,3]=6
  laLine[38,4]=0

  laLine[39,1]='nQty5'
  laLine[39,2]='N'
  laLine[39,3]=6
  laLine[39,4]=0

  laLine[40,1]='nQty6'
  laLine[40,2]='N'
  laLine[40,3]=6
  laLine[40,4]=0

  laLine[41,1]='nQty7'
  laLine[41,2]='N'
  laLine[41,3]=6
  laLine[41,4]=0

  laLine[42,1]='nQty8'
  laLine[42,2]='N'
  laLine[42,3]=6
  laLine[42,4]=0

  laLine[43,1]='ColPack'
  laLine[43,2]='C'
  laLine[43,3]=29
  laLine[43,4]=0

  laLine[44,1]='llNot_Mem'
  laLine[44,2]='L'
  laLine[44,3]=1
  laLine[44,4]=0

  laLine[45,1]='Dyelot'
  laLine[45,2]='C'
  laLine[45,3]=10
  laLine[45,4]=0
  
  laLine[46,1]='Gobject'
  laLine[46,2]='G'
  laLine[46,3]=10
  laLine[46,4]=0
  
* E127602,1 HMA 05/09/2005 add Style Picture to report layout [End].
  lcOrdLin= loOgScroll.gfTempName()
  gfCrtTmp(lcOrdLin,@laLine,"cOrdType+Order",lcOrdLin,.F.)
   


  SELECT (lcOrdLin)

  DO CASE
    CASE lcRpSortBy = 'S'
      IF !llConfig
        INDEX ON CORDTYPE + ORDER + STORE + STYLE + STR(LINENO,6) + Pack_Id +cpkcolor+cpcksize+cPkVersion   TAG (lcOrdLin)
      ELSE 
        INDEX ON CORDTYPE + ORDER + STORE + STYLE +DYELOT +STR(LINENO,6) + Pack_Id +cpkcolor+cpcksize+cPkVersion   TAG (lcOrdLin)
      ENDIF     
    CASE lcRpSortBy = 'L'
      IF !llConfig
        INDEX ON CORDTYPE + ORDER + STORE + STR(LINENO,6) + STYLE +  Pack_Id +cpkcolor+cpcksize+ cPkVersion   TAG (lcOrdLin)
      ELSE 
        INDEX ON CORDTYPE + ORDER + STORE + STR(LINENO,6) + STYLE +DYELOT+  Pack_Id +cpkcolor+cpcksize+ cPkVersion   TAG (lcOrdLin)
      ENDIF     
     CASE lcRpSortBy = 'P'
      IF llRpPrTCon
        IF !llConfig
          INDEX ON CORDTYPE + ORDER + PACK_ID +cpkcolor+cpcksize+ cPkVersion  + STORE  + STYLE + STR(LINENO,6) TAG (lcOrdLin)
        ELSE
          INDEX ON CORDTYPE + ORDER + PACK_ID +cpkcolor+cpcksize+ cPkVersion  + STORE  + STYLE +DYELOT+STR(LINENO,6) TAG (lcOrdLin)
        ENDIF 
      ELSE
        IF !llConfig
          INDEX ON CORDTYPE + ORDER + STORE  + Pack_Id +cpkcolor+cpcksize+ cPkVersion + STYLE + STR(LINENO,6)  TAG (lcOrdLin)
        ELSE
          INDEX ON CORDTYPE + ORDER + STORE  + Pack_Id +cpkcolor+cpcksize+ cPkVersion + STYLE +DYELOT+ STR(LINENO,6)  TAG (lcOrdLin)
        ENDIF 
      ENDIF
    
  ENDCASE
  SET ORDER TO (lcOrdLin) 


  SELECT ORDHDR
  GOTO TOP

  *-- Scan order header Information
  SCAN REST WHILE INKEY()<> 32 FOR &lcRpExp
  *-- B038190,1 HMA 11/01/2004 fix the bug of select more than 24 value in season or division. [BEGIN]
    IF !EMPTY(lcDivCursor)
      IF !SEEK(CDIVISION,lcDivCursor)
        LOOP 
      ENDIF 
    ENDIF 
    IF !EMPTY(lcSeaCursor)
      IF !SEEK(SEASON,lcSeaCursor)
        LOOP 
      ENDIF 
    ENDIF  
  *-- B038190,1 HMA 11/01/2004 fix the bug of select more than 24 value in season or division. [END]
    WAIT WINDOW LANG_Soorcngm_PrtOrd +Order+LANG_Soorcngm_Abort NOWAIT
      
    *--Get the Order header information.

    =lfGetOdHdr()

    lnTotQty  = 0
    lnTotORd  = 0.00
    lnMer_Amnt =0.00
    lnMer_Disc =0.00
    lnNet_Amnt =0.00

    NOTELOOP  = .F.

    *-- Start Printing  The header.
    SELECT (lcTempOrd)

    lcStore = Store
*SSH
    =lfPrnHdr(@llPPrtpak,@llInStor)

    *--- START PRINTING THE ORDER LINES
    *-- Main loop of printing
    lnSelectedAlias=SELECT()   
    *B128349,1 HMA 06/05/2005 Print the report even if the selected order has no details [Begin]
    IF !SEEK(OrdHdr.cOrdType+OrdHdr.Order,lcTempOrd)
      INSERT INTO &lcOrdLin(cOrdType,order,Store) VALUES (ORDHDR.CORDTYPE,ORDHDR.ORDER,ORDHDR.STORE)
      IF llRpPrtCon
        INSERT INTO &lcOrdLinSt(cOrdType,order,Store) VALUES (ORDHDR.CORDTYPE,ORDHDR.ORDER,ORDHDR.STORE)
      ENDIF 
      INSERT INTO &lcTmpDesc(cOrdType,order) VALUES (ORDHDR.CORDTYPE,ORDHDR.ORDER)
      INSERT INTO &lcTmpPack(cOrdType,order,Store) VALUES (ORDHDR.CORDTYPE,ORDHDR.ORDER,ORDHDR.STORE)
    ENDIF   
    SELECT (lnSelectedAlias)
    *B128349,1 HMA 06/05/2005 Print the report even if the selected order has no details [End]
    SCAN WHILE cOrdType+Order = ORDHDR.cOrdType+lcOrder
    	DO CASE 
       CASE lcRpSortBy = 'S'
         IF !llConfig
           llSeekRec=SEEK(CORDTYPE + ORDER + STORE + STYLE + STR(LINENO,6) + Pack_Id +cpkcolor+cpcksize+cPkVersion,lcOrdLin)
         ELSE
           llSeekRec=SEEK(CORDTYPE + ORDER + STORE + STYLE +DYELOT+ STR(LINENO,6) + Pack_Id +cpkcolor+cpcksize+cPkVersion,lcOrdLin)
         ENDIF 
       CASE lcRpSortBy = 'L'
         IF !llConfig
           llSeekRec=SEEK(CORDTYPE + ORDER + STORE + STR(LINENO,6) + STYLE +  Pack_Id +cpkcolor+cpcksize+ cPkVersion,lcOrdLin)	  
         ELSE
           llSeekRec=SEEK(CORDTYPE + ORDER + STORE + STR(LINENO,6) + STYLE +DYELOT + Pack_Id +cpkcolor+cpcksize+ cPkVersion,lcOrdLin)	  
         ENDIF 
       CASE lcRpSortBy = 'P'
         IF llRpPrTCOn
           IF !llConfig
             llSeekRec=SEEK(CORDTYPE + ORDER + PACK_ID +cpkcolor+cpcksize+ cPkVersion  + STORE  + STYLE + STR(LINENO,6),lcOrdLin)
           ELSE 
             llSeekRec=SEEK(CORDTYPE + ORDER + PACK_ID +cpkcolor+cpcksize+ cPkVersion  + STORE  + STYLE +DYELOT+ STR(LINENO,6),lcOrdLin)
           ENDIF 
         ELSE 
           IF !llConfig
             llSeekRec=SEEK(CORDTYPE + ORDER + STORE  + Pack_Id +cpkcolor+cpcksize+ cPkVersion + STYLE + STR(LINENO,6),lcOrdLin)
           ELSE
             llSeekRec=SEEK(CORDTYPE + ORDER + STORE  + Pack_Id +cpkcolor+cpcksize+ cPkVersion + STYLE +DYELOT+ STR(LINENO,6),lcOrdLin)
           ENDIF 
         ENDIF 
       ENDCASE 

       IF !llSeekRec &&fill the order line files by the required data
         *--Print from this file in case of print consolidating="NO"(one line for each order line)
         * E127602,1 HMA 05/09/2005 add Style Picture to report layout [Begin].
         *INSERT INTO &lcOrdLin(cOrdType,order,Store,LineNo,Pack_id,cpkcolor,cpcksize,cPkVersion,Style,Dyelot,ColPack) ;
  	                          VALUES (&lcTempOrd..cOrdType,&lcTempOrd..Order,&lcTempOrd..Store,&lcTempOrd..LineNo,&lcTempOrd..Pack_id,&lcTempOrd..cpkcolor,;
  	                          &lcTempOrd..cpcksize,&lcTempOrd..cPkVersion,ALLTRIM(&lcTempOrd..Style),&lcTempOrd..Dyelot,&lcTempOrd..Pack_id+&lcTempOrd..cpkcolor+&lcTempOrd..cpcksize+&lcTempOrd..cPkVersion)
         INSERT INTO &lcOrdLin(cOrdType,order,Store,LineNo,Pack_id,cpkcolor,cpcksize,cPkVersion,Style,Dyelot,ColPack,Gobject) ;
                              VALUES (&lcTempOrd..cOrdType,&lcTempOrd..Order,&lcTempOrd..Store,&lcTempOrd..LineNo,&lcTempOrd..Pack_id,&lcTempOrd..cpkcolor,;
                              &lcTempOrd..cpcksize,&lcTempOrd..cPkVersion,ALLTRIM(&lcTempOrd..Style),&lcTempOrd..Dyelot,&lcTempOrd..Pack_id+&lcTempOrd..cpkcolor+&lcTempOrd..cpcksize+&lcTempOrd..cPkVersion,OBJECTS_A.gObject)
         * E127602,1 HMA 05/09/2005 add Style Picture to report layout [End].

         *--Print from this file in case of print consolidating="YES"(one line for each Style)
         IF llRpPrtCon   &&Case of consolidating by store &order is multistore.
           SELECT (lcOrdLinSt)
*B128422,1 HMA 06/08/2005 Print Wrong Line Notes in case print Consolidating =Yes [Begin]
*--correct the Seek Expression
*!*             IF !llConfig
*!*               llSeek=SEEK(&lcTempOrd..cOrdType+&lcTempOrd..Order+PADR(ALLTRIM(&lcTempOrd..Style),lnMjrPct,' ')+&lcTempOrd..Pack_id+&lcOrdLin..cpkcolor+&lcTempOrd..cpcksize+&lcTempOrd..cPkVersion ,lcOrdLinSt) 
*!*             ELSE 
*!*               llSeek=SEEK(&lcTempOrd..cOrdType+&lcTempOrd..Order+PADR(ALLTRIM(&lcTempOrd..Style),lnMjrPct,' ')+Dyelot+&lcTempOrd..Pack_id+&lcOrdLin..cpkcolor+&lcTempOrd..cpcksize+&lcTempOrd..cPkVersion ,lcOrdLinSt) 
*!*             ENDIF 
           IF !llConfig
             llSeek=SEEK(&lcTempOrd..cOrdType+&lcTempOrd..Order+PADR(ALLTRIM(&lcTempOrd..Style),lnMjrPct,' ')+&lcTempOrd..Pack_id+&lcTempOrd..cpkcolor+&lcTempOrd..cpcksize+&lcTempOrd..cPkVersion ,lcOrdLinSt) 
           ELSE 
             llSeek=SEEK(&lcTempOrd..cOrdType+&lcTempOrd..Order+PADR(ALLTRIM(&lcTempOrd..Style),lnMjrPct,' ')+&lcTempOrd..Dyelot+&lcTempOrd..Pack_id+&lcTempOrd..cpkcolor+&lcTempOrd..cpcksize+&lcTempOrd..cPkVersion ,lcOrdLinSt) 
           ENDIF 
*B128422,1 HMA 06/08/2005 Print Wrong Line Notes in case print Consolidating =Yes [End]
           IF !llSeek
  *HMA,127296
             *INSERT INTO &lcOrdLinSt(cOrdType,Order,Style,Store,Dyelot,Pack_id,cpkcolor,cpcksize,cPkVersion,cDesc,nSugRetPri);
  	                     VALUES (&lcTempOrd..cOrdType,&lcTempOrd..order,ALLTRIM(&lcTempOrd..Style),&lcTempOrd..Store,&lcTempOrd..Dyelot,&lcTempOrd..Pack_id,&lcTempOrd..cpkcolor,;
  	                     &lcTempOrd..cpcksize,&lcTempOrd..cPkVersion,SUBSTR(Style.Desc,1,19),&lcTempOrd..nSugRetPri)

             * E127602,1 HMA 05/09/2005 add Style Picture to report layout [Begin].

             *INSERT INTO &lcOrdLinSt(cOrdType,Order,Style,Store,LineNo,Dyelot,Pack_id,cpkcolor,cpcksize,cPkVersion,cDesc,nSugRetPri);
                         VALUES (&lcTempOrd..cOrdType,&lcTempOrd..order,ALLTRIM(&lcTempOrd..Style),&lcTempOrd..Store,&lcTempOrd..LineNo,&lcTempOrd..Dyelot,&lcTempOrd..Pack_id,&lcTempOrd..cpkcolor,;
                         &lcTempOrd..cpcksize,&lcTempOrd..cPkVersion,SUBSTR(Style.Desc,1,19),&lcTempOrd..nSugRetPri)   
             INSERT INTO &lcOrdLinSt(cOrdType,Order,Style,Store,LineNo,Dyelot,Pack_id,cpkcolor,cpcksize,cPkVersion,cDesc,nSugRetPri,gObject);
                       VALUES (&lcTempOrd..cOrdType,&lcTempOrd..order,ALLTRIM(&lcTempOrd..Style),&lcTempOrd..Store,&lcTempOrd..LineNo,&lcTempOrd..Dyelot,&lcTempOrd..Pack_id,&lcTempOrd..cpkcolor,;
                       &lcTempOrd..cpcksize,&lcTempOrd..cPkVersion,SUBSTR(Style.Desc,1,19),&lcTempOrd..nSugRetPri,OBJECTS_A.gObject)   

             * E127602,1 HMA 05/09/2005 add Style Picture to report layout [End].
  *HMA,127296
           ENDIF 
         ENDIF      
    
         IF !llRpPrtCon  &&Case of consolidating by store &order is multistore.

           SELECT (lcTmpPack)&& fill the pack file by the pack information without details of its container.
           lcPack_id= &lcTempOrd..Pack_id+&lcTempOrd..cpkcolor+&lcTempOrd..cpcksize+&lcTempOrd..cPkVersion 
           IF !SEEK(&lcTempOrd..cOrdType+&lcTempOrd..Order+PADR(ALLTRIM(&lcTempOrd..Store),8,' ')+lcPack_id,lcTmpPack)
             INSERT INTO &lcTmpPack (cOrdType,order,Store) VALUES (&lcTempOrd..cOrdType,&lcTempOrd..Order,&lcTempOrd..Store)
           ENDIF   
           SELECT (lcTempOrd)
           IF !SEEK(&lcTempOrd..cOrdType+&lcTempOrd..Order+PADR(ALLTRIM(&lcTempOrd..Store),8,' ')+lcPack_id,lcTmpPack)
             SELECT (lcTmpPack)      
             =SEEK(&lcTempOrd..cOrdType+&lcTempOrd..Order+PADR(ALLTRIM(&lcTempOrd..Store),8,' '),lcTmpPack)
             IF EMPTY(Pack_id) 
               REPLACE Pack_id    WITH &lcTempOrd..Pack_id,;
                       cpkcolor   WITH &lcTempOrd..cpkcolor,;
                       cpcksize   WITH &lcTempOrd..cpcksize,;
                       cPkVersion WITH &lcTempOrd..cPkVersion,;
                       STORE      WITH &lcTempOrd..Store
             ELSE 
               INSERT INTO &lcTmpPack (cOrdType,order,Store,Pack_id,cpkcolor,cpcksize,cPkVersion) ;
                                 VALUES (&lcTempOrd..cOrdType,&lcTempOrd..Order,&lcTempOrd..Store,&lcTempOrd..Pack_id,;
                                 &lcTempOrd..cpkcolor,&lcTempOrd..cpcksize,&lcTempOrd..cPkVersion)
             ENDIF
           ENDIF 

        ELSE      &&Else of llRpPrtCon
      
          SELECT (lcTmpPack)&& fill the pack file by the pack information without details of its container.
          lcPack_id= &lcTempOrd..Pack_id+&lcTempOrd..cpkcolor+&lcTempOrd..cpcksize+&lcTempOrd..cPkVersion
          IF !SEEK(&lcTempOrd..cOrdType+&lcTempOrd..Order+lcPack_id,lcTmpPack)
            INSERT INTO &lcTmpPack (cOrdType,order,Store) VALUES (&lcTempOrd..cOrdType,&lcTempOrd..Order,&lcTempOrd..Store)
          ENDIF   
          SELECT (lcOrdLin)
          IF !SEEK(&lcTempOrd..cOrdType+&lcTempOrd..Order+lcPack_id,lcTmpPack)
             SELECT (lcTmpPack)      
             =SEEK(&lcTempOrd..cOrdType+&lcTempOrd..Order,lcTmpPack)
             IF EMPTY(Pack_id) 
               REPLACE Pack_id    WITH &lcTempOrd..Pack_id,;
                       cpkcolor   WITH &lcTempOrd..cpkcolor,;
                       cpcksize   WITH &lcTempOrd..cpcksize,;
                       cPkVersion WITH &lcTempOrd..cPkVersion
             ELSE  
               INSERT INTO &lcTmpPack (cOrdType,order,Store,Pack_id,cpkcolor,cpcksize,cPkVersion) ;
                               VALUES (&lcTempOrd..cOrdType,&lcTempOrd..Order,&lcTempOrd..Store,&lcTempOrd..Pack_id,;
                               &lcTempOrd..cpkcolor,&lcTempOrd..cpcksize,&lcTempOrd..cPkVersion)
             ENDIF
          ENDIF 
       
       ENDIF  &&End Of IF 
     ENDIF         
     SELECT (lnSelectedAlias)
    
    
             
      IF lcMulti = 'Y' .AND. Store # lcStore  .AND.  !llRpPrTCon  &&  if not Print consolidating all stores by Style/Color.
        *-- Print page footer
        lnTotQty  = 0
        lnTotORd  = 0.00
        lcStore = Store
        =lfGetOdHdr()
        =lfPrnHdr(@llPPrtpak,@llInStor)
      ENDIF
      lcStyDesc = Style.Desc

      IF !llRpPrTCon  &&  Print consolidating all stores by Style/Color
   
        IF llRpPrTPak .OR. EMPTY(Pack_id) .OR. !llRpPack
          REPLACE &lcOrdLin..Style      WITH ALLTRIM(Style),;
                  &lcOrdLin..Dyelot     WITH ALLTRIM(Dyelot),;
                  &lcOrdLin..StyDesc    WITH ALLTRIM(lcStyDesc),;
          		&lcOrdLin..nSugRetPri WITH &lcTempOrd..nSugRetPri ,;
          		&lcOrdLin..Price 	  WITH Price ,;
                  &lcOrdLin..TotQty 	  WITH TotQty
          lnAmount = TotQty   * Price
          REPLACE &lcOrdLin..lnAmount WITH lnAmount
          
          IF !EMPTY(&lcTempOrd..DESC1) OR !EMPTY(STYLE.DESC1)
            lcDescrip=IIF(EMPTY(&lcTempOrd..DESC1),STYLE.DESC1,&lcTempOrd..DESC1)
            REPLACE &lcOrdLin..Desc1 WITH lcDescrip 
          ENDIF
        ENDIF      
        lnAmount = TotQty   * Price
        lnTotORd = lnTotORd + lnAmount
        lnTotQty = lnTotQty + TotQty
        =lfPrnLast()                  

     ELSE
        IF !EMPTY(&lcTempOrd..DESC1) OR !EMPTY(STYLE.DESC1)
          lcDescrip=IIF(EMPTY(&lcTempOrd..DESC1),STYLE.DESC1,&lcTempOrd..DESC1)
          REPLACE &lcOrdLinSt..Desc1 WITH lcDescrip 
        ENDIF  
       IF llRpPrTPak .OR. EMPTY(Pack_id) .OR. !llRpPack
         =lfAddRec()
       ENDIF
       lnAmount = TotQty   * Price
       lnTotORd = lnTotORd + lnAmount
       lnTotQty = lnTotQty + TotQty
       =lfPrnLast()
     
     ENDIF
     


     IF !llRpPrtCon
       lcPack = lfGetPack()
       =lfPrtpack(lcPack,'') 
     ENDIF 
     *--Print Order Notes(line notes)

      IF MEMLINES(Note_Mem) <> 0 AND llRpOrdLNt .AND. llRpPrTPak
        =lfPrtOnte()
        IF !llRpPrtCon
          *B128422,1 HMA 06/08/2005 Print Wrong Line Notes in case print Consolidating =Yes [Begin]
          REPLACE &lcOrdLin..llNot_Mem  WITH  IIF(EMPTY(&lcOrdLin..Note_Mem),.T.,.F.)
          *IIF(EMPTY(&lcOrdLin..Note_Mem),&lcOrdLin..llNot_Mem = .F.,&lcOrdLin..llNot_Mem=.T.)
          *B128422,1 HMA 06/08/2005 Print Wrong Line Notes in case print Consolidating =Yes [End]
        ELSE 
          *B128422,1 HMA 06/08/2005 Print Wrong Line Notes in case print Consolidating =Yes [Begin]
          REPLACE &lcOrdLinSt..llNot_Mem WITH IIF(EMPTY(&lcOrdLinSt..Note_Mem),.T.,.F.)
          *IIF(EMPTY(&lcOrdLinSt..Note_Mem),&lcOrdLinSt..llNot_Mem = .F.,&lcOrdLinSt..llNot_Mem=.T.)
          *B128422,1 HMA 06/08/2005 Print Wrong Line Notes in case print Consolidating =Yes [End]
        ENDIF
      ENDIF  
      *--Print Order Notes:
      *--1.short notes for order.
      IF  llRpOrdLnt .AND. !EMPTY(ORDHDR.Note1) .AND. SUBSTR(ALLTRIM(ORDHDR.Note1),1,1) <> '*'
        IF !llRpPrtCon
          REPLACE &lcOrdHdr..NOTE1 WITH ALLTRIM(ORDHDR.Note1)
        ELSE 
          REPLACE &lcOrdHdrSt..NOTE1 WITH ALLTRIM(ORDHDR.Note1)
          ENDIF 
      ENDIF
     
      IF llRpOrdLnt .AND. !EMPTY(ORDHDR.Note2) .AND. SUBSTR(ALLTRIM(ORDHDR.Note2),1,1) <> '*'
        IF !llRpPrtCon
          REPLACE &lcOrdHdr..NOTE2 WITH ALLTRIM(ORDHDR.Note2)
        ELSE 
          REPLACE &lcOrdHdrSt..NOTE2 WITH ALLTRIM(ORDHDR.Note2)      
        ENDIF  
      ENDIF
      IF !llRpPrtCon
        *B128422,1 HMA 06/08/2005 Print Wrong Line Notes in case print Consolidating =Yes [Begin]
        REPLACE &lcOrdHdr..llNote1 WITH IIF(EMPTY(&lcOrdHdr..Note1),.T.,.F.)
        REPLACE &lcOrdHdr..llNote2 WITH IIF(EMPTY(&lcOrdHdr..Note2),.T.,.F.)
        *IIF(EMPTY(&lcOrdHdr..Note1),&lcOrdHdr..llNote1 = .F.,&lcOrdHdr..llNote1= .T.)
        *IIF(EMPTY(&lcOrdHdr..Note2),&lcOrdHdr..llNote2 = .F.,&lcOrdHdr..llNote2= .T.)
        *B128422,1 HMA 06/08/2005 Print Wrong Line Notes in case print Consolidating =Yes [End]
      ELSE 
        *B128422,1 HMA 06/08/2005 Print Wrong Line Notes in case print Consolidating =Yes [Begin]
        REPLACE &lcOrdHdrSt..llNote1 WITH IIF(EMPTY(&lcOrdHdrSt..Note1),.T.,.F.)
        REPLACE &lcOrdHdrSt..llNote2 WITH IIF(EMPTY(&lcOrdHdrSt..Note2),.T.,.F.)
        *IIF(EMPTY(&lcOrdHdrSt..Note1),&lcOrdHdrSt..llNote1 = .F.,&lcOrdHdrSt..llNote1= .T.)
        *IIF(EMPTY(&lcOrdHdrSt..Note2),&lcOrdHdrSt..llNote2 = .F.,&lcOrdHdrSt..llNote2= .T.)
        *B128422,1 HMA 06/08/2005 Print Wrong Line Notes in case print Consolidating =Yes [End]
      ENDIF

    ENDSCAN
    *------------------------ END PRINT LINE LOOP ----------------------*  
 
    IF !llRpPrtPak
      =lfPrntPack() 
    ENDIF 
    IF llRpPrtCon
      =lfPrPakCon()
    ENDIF 

    *--2.Print NotePad.
    IF llRpOrdNot
      =lfPrnNotPd()
      IF !llRpPrtCon
        *B128422,1 HMA 06/08/2005 Print Wrong Line Notes in case print Consolidating =Yes [Begin]
        *IIF(EMPTY(&lcOrdHdr..Mnotes),&lcOrdHdr..llMnotes = .F.,&lcOrdHdr..llMnotes = .T.)
        REPLACE &lcOrdHdr..llMnotes WITH IIF(EMPTY(&lcOrdHdr..Mnotes),.T.,.F.)
        *B128422,1 HMA 06/08/2005 Print Wrong Line Notes in case print Consolidating =Yes [End]
      ELSE 
        *B128422,1 HMA 06/08/2005 Print Wrong Line Notes in case print Consolidating =Yes [Begin]
        *IIF(EMPTY(&lcOrdHdrSt..Mnotes),&lcOrdHdrSt..llMnotes = .F.,&lcOrdHdrSt..llMnotes = .T.)
        REPLACE &lcOrdHdrSt..llMnotes WITH IIF(EMPTY(&lcOrdHdrSt..Mnotes),.T.,.F.)
       *B128422,1 HMA 06/08/2005 Print Wrong Line Notes in case print Consolidating =Yes [End]
      ENDIF
    ENDIF

  ENDSCAN

  *: B610752,1 MMT 06/22/2014 Custom Order Confirmation form GM prints UPC in 2 lines[T20131219.0020][Start]
  IF !EMPTY(lctmpdesc) AND USED(lctmpdesc)
    SELECT(lctmpdesc)
    REPLACE mupc WITH ALLTRIM(mupc) all
  ENDIF    
  *: B610752,1 MMT 06/22/2014 Custom Order Confirmation form GM prints UPC in 2 lines[T20131219.0020][End] 
  *--Passing the temp. files &Parameters to the crystal reports
  DO CASE 
    CASE llRpPrtCon AND (llRpPrtPak OR !llRpPack)
   
      DIMENSION loOGScroll.laCRTables[3]
      loOGScroll.laCRTables[1] = oAriaApplication.WorkDir +  lcOrdHdrSt  + ".DBF"
      loOGScroll.laCRTables[2] = oAriaApplication.WorkDir +  lcOrdLinSt  + ".DBF"
      loOGScroll.laCRTables[3] = oAriaApplication.WorkDir +  lcTmpDesc  + ".DBF"
   
    CASE llRpPrtCon AND !llRpPrtPak
   
      DIMENSION loOGScroll.laCRTables[4]
      loOGScroll.laCRTables[1] = oAriaApplication.WorkDir +  lcOrdHdrSt  + ".DBF"
      loOGScroll.laCRTables[2] = oAriaApplication.WorkDir +  lcOrdLinSt  + ".DBF"
      loOGScroll.laCRTables[3] = oAriaApplication.WorkDir +  lcTmpDesc  + ".DBF"
      loOGScroll.laCRTables[4] = oAriaApplication.WorkDir +  lcTmpPack  + ".DBF"     
   
    CASE !llRpPrtCon AND (llRpPrtPak OR !llRpPack)
   
      DIMENSION loOGScroll.laCRTables[3]
      loOGScroll.laCRTables[1] = oAriaApplication.WorkDir +  lcOrdHdr  + ".DBF"
      loOGScroll.laCRTables[2] = oAriaApplication.WorkDir +  lcOrdLin  + ".DBF"
      loOGScroll.laCRTables[3] = oAriaApplication.WorkDir +  lcTmpDesc  + ".DBF"
   
    CASE !llRpPrtCon AND !llRpPrtPak
      DIMENSION loOGScroll.laCRTables[4]
      loOGScroll.laCRTables[1] = oAriaApplication.WorkDir +  lcOrdHdr  + ".DBF"
      loOGScroll.laCRTables[2] = oAriaApplication.WorkDir +  lcOrdLin  + ".DBF"
      loOGScroll.laCRTables[3] = oAriaApplication.WorkDir +  lcTmpDesc  + ".DBF"
      loOGScroll.laCRTables[4] = oAriaApplication.WorkDir +  lcTmpPack  + ".DBF"  

  ENDCASE 

*E127602,1 HMA 05/09/2005 add Style Picture to report layout [Begin].
*!*    DIMENSION loOGScroll.laCRParams[12,2]

*!*    loOGScroll.laCRParams[1,1] = 'SortBy'
*!*    loOGScroll.laCRParams[1,2] = lcSortName
*!*    loOGScroll.laCRParams[2,1] = 'ReportName'
*!*    loOGScroll.laCRParams[2,2] = 'Order Confirmation Report'
*!*    loOGScroll.laCRParams[3,1] = 'llRpPrTCon'
*!*    loOGScroll.laCRParams[3,2] = llRpPrTCon
*!*    loOGScroll.laCRParams[4,1] = 'llInStor'
*!*    loOGScroll.laCRParams[4,2] = llInStor
*!*    loOGScroll.laCRParams[5,1] = 'llPprtPak'
*!*    loOGScroll.laCRParams[5,2] = llPprtPak
*!*    loOGScroll.laCRParams[6,1] = 'lcMajTitl'
*!*    loOGScroll.laCRParams[6,2] = lcMajTitl
*!*    loOGScroll.laCRParams[7,1] = 'llRpPack'
*!*    loOGScroll.laCRParams[7,2] = llRpPack
*!*    loOGScroll.laCRParams[8,1] = 'llRpPrtPak'
*!*    loOGScroll.laCRParams[8,2] = llRpPrtPak
*!*    loOGScroll.laCRParams[9,1] = 'llRpOrdNot'
*!*    loOGScroll.laCRParams[9,2] = llRpOrdNot
*!*    loOGScroll.laCRParams[10,1] = 'llRpOrdLnt'
*!*    loOGScroll.laCRParams[10,2] = llRpOrdLnt
*!*    loOGScroll.laCRParams[11,1] = 'llConfig'
*!*    loOGScroll.laCRParams[11,2] = llConfig
*!*    loOGScroll.laCRParams[12,1] = 'lcRpSortBy'
*!*    loOGScroll.laCRParams[12,2] = lcRpSortBy
  DIMENSION loOGScroll.laCRParams[13,2]

  loOGScroll.laCRParams[1,1] = 'SortBy'
  loOGScroll.laCRParams[1,2] = lcSortName
  loOGScroll.laCRParams[2,1] = 'ReportName'
  loOGScroll.laCRParams[2,2] = 'Order Confirmation Report'
  loOGScroll.laCRParams[3,1] = 'llRpPrTCon'
  loOGScroll.laCRParams[3,2] = llRpPrTCon
  loOGScroll.laCRParams[4,1] = 'llInStor'
  loOGScroll.laCRParams[4,2] = llInStor
  loOGScroll.laCRParams[5,1] = 'llPprtPak'
  loOGScroll.laCRParams[5,2] = llPprtPak
  loOGScroll.laCRParams[6,1] = 'lcMajTitl'
  loOGScroll.laCRParams[6,2] = lcMajTitl
  loOGScroll.laCRParams[7,1] = 'llRpPack'
  loOGScroll.laCRParams[7,2] = llRpPack
  loOGScroll.laCRParams[8,1] = 'llRpPrtPak'
  loOGScroll.laCRParams[8,2] = llRpPrtPak
  loOGScroll.laCRParams[9,1] = 'llRpOrdNot'
  loOGScroll.laCRParams[9,2] = llRpOrdNot
  loOGScroll.laCRParams[10,1] = 'llRpOrdLnt'
  loOGScroll.laCRParams[10,2] = llRpOrdLnt
  loOGScroll.laCRParams[11,1] = 'llConfig'
  loOGScroll.laCRParams[11,2] = llConfig
  loOGScroll.laCRParams[12,1] = 'lcRpSortBy'
  loOGScroll.laCRParams[12,2] = lcRpSortBy
  loOGScroll.laCRParams[13,1] = 'llPicture'
  *: B610760,1 HES 06/24/2014 Fix a bug in loogscroll array [START]
*!*	  loOGScroll.laCRParams[13,2] = llPicture
  loOGScroll.laCRParams[13,2] = llrppicture
  *: B610760,1 HES 06/24/2014 Fix a bug in loogscroll array [END  ]
  
*!*	ACTIVATE WINDOW trace
*!*	SUSPEND
*!*	_screen.Visible=.t.
*E127602,1 HMA 05/09/2005 add Style Picture to report layout [End].
  DO CASE 
    CASE llRpPrtCon AND (llRpPrtPak OR !llRpPack)

      USE IN &lcOrdHdrSt
      USE IN &lcOrdLinSt
      USE IN &lcTmpDesc  
    
    CASE llRpPrtCon AND !llRpPrtPak
    
      USE IN &lcOrdHdrSt
      USE IN &lcOrdLinSt
      USE IN &lcTmpDesc  
      USE IN &lcTmpPack 
    
    CASE !llRpPrtCon AND (llRpPrtPak OR !llRpPack)
    
      USE IN &lcOrdHdr
      USE IN &lcOrdLin
      USE IN &lcTmpDesc  


    CASE !llRpPrtCon AND !llRpPrtPak  

      USE IN &lcOrdHdr
      USE IN &lcOrdLin
      USE IN &lcTmpDesc  
      USE IN &lcTmpPack   

  ENDCASE 


  DO CASE 
    CASE llRpPrtCon AND (llRpPrtPak OR !llRpPack)
     loOgScroll.lcOgLastForm= "SOORCNGMA"    &&Print Consolidating=YES , Print Pack ID=No
    CASE llRpPrtCon AND !llRpPrtPak
     loOgScroll.lcOgLastForm= "SOORCNGMC"      &&Print Consolidating=YES , Print Pack ID=YES ,Print Pack Details=NO
    CASE !llRpPrtCon AND (llRpPrtPak OR !llRpPack)
     loOgScroll.lcOgLastForm= "SOORCNGM"    &&Print Consolidating=No , Print Pack ID=No
    CASE !llRpPrtCon AND !llRpPrtPak  
     loOgScroll.lcOgLastForm= "SOORCNGMB"    &&Print Consolidating=NO , Print Pack ID=YES ,Print Pack Details=NO
  ENDCASE 
  *--Don't show Company logo in report layout
  loOgScroll.llShowLogo=.F.
  loogScroll.cCROrientation = 'P'
  *--Display the report.

  DO gfDispRe

ELSE
*B128349,1 HMA 06/05/2005 Print the report even if the selected order has no details [Begin]]
*!*    SELECT (lcTempOrd)
*!*    IF !(RECCOUNT() > 0)
*!*      *--No records to display.
*!*      = gfModalGen('TRM00052B00000','DIALOG' )
*!*      RETURN .F.
*!*    ELSE
*!*      *--Display the report.
*!*      DO gfDispRe
*!*    ENDIF
*B128349,1 HMA 06/05/2005 Print the report even if the selected order has no details [End]
  DO gfDispRe
ENDIF 

*-- End OF Print.

*!*************************************************************
*! Name      : lfPrtONte
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 08/02/2004
*! Purpose   : Function to print Order line notes.
*!*************************************************************
*! Calls     :  None.
*!*************************************************************
*! Passed Parameters  : None.
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example            : =lfPrtONte()
*!*************************************************************
*
FUNCTION lfPrtONte

*-- Set the memo width to our standard.
PRIVATE  lnOldMemW
lnOldMemW = SET("MEMOWIDTH")
SET MEMOWIDTH TO 75
IF !llRpPrtCon
*B128422,1 HMA 06/08/2005 Print Wrong Line Notes in case print Consolidating =Yes [Begin]
*  REPLACE &lcOrdLin..Note_Mem WITH Note_Mem  
  IF EMPTY(&lcOrdLin..Note_Mem)
    REPLACE &lcOrdLin..Note_Mem WITH Note_Mem  
  ENDIF 
*B128422,1 HMA 06/08/2005 Print Wrong Line Notes in case print Consolidating =Yes [End]
ELSE 
*B128422,1 HMA 06/08/2005 Print Wrong Line Notes in case print Consolidating =Yes [Begin]
*  REPLACE &lcOrdLinSt..Note_Mem WITH Note_Mem  
  IF EMPTY(&lcOrdLinSt..Note_Mem)
    REPLACE &lcOrdLinSt..Note_Mem WITH Note_Mem  
  ENDIF 
*B128422,1 HMA 06/08/2005 Print Wrong Line Notes in case print Consolidating =Yes [End]
ENDIF 
*-- Restore the old memo width.
SET MEMOWIDTH TO lnOldMemW

RETURN(.T.)

*-- End OF lfPrtONte.

*!*************************************************************
*! Name      : lfPrnLast
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 08/02/2004
*! Purpose   : Function to print the totals per each order
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Passed Parameters  : None.
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example            :  lfPrnLast()
*!*************************************************************

FUNCTION lfPrnLast

 
lnMer_Disc = ORDHDR.DISC
lnMer_Amnt = (lnMer_Disc * lnTotORd)/100
lnNet_Amnt = lnTotORd - lnMer_Amnt
IF !llRpPrtCon
  REPLACE &lcOrdHdr..lnTotOrd WITH  lnTotOrd ,;
          &lcOrdHdr..lnMer_Disc WITH  lnMer_Disc,;
          &lcOrdHdr..lnMer_Amnt WITH  lnMer_Amnt,;
          &lcOrdHdr..lnTotQty WITH  lnTotQty,;
          &lcOrdHdr..lnNet_Amnt WITH  lnNet_Amnt
ELSE 
  REPLACE &lcOrdHdrSt..lnTotOrd WITH  lnTotOrd ,;
          &lcOrdHdrSt..lnMer_Disc WITH  lnMer_Disc,;
          &lcOrdHdrSt..lnMer_Amnt WITH  lnMer_Amnt,;
          &lcOrdHdrSt..lnTotQty WITH  lnTotQty,;
          &lcOrdHdrSt..lnNet_Amnt WITH  lnNet_Amnt
ENDIF 
*-- End Of lfPrnLast.
*!*************************************************************
*! Name      : lfPrnHdr
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 08/02/2004
*! Purpose   : Function to print the header information.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Passed Parameters  : None.
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example            :  lfPrnHdr()
*!*************************************************************
*
FUNCTION lfPrnHdr
PARAMETERS llPprtPak,llInStor
*-- Print the company address.
IF EMPTY(lcDivLName)
  lcDivision = ALLTRIM(SPACE(ROUND((80-LEN(gccom_name))/2,0))+gccom_name)
ELSE
  lcDivision = ALLTRIM(SPACE(ROUND((80-LEN(ALLTRIM(lcDivLName)))/2,0)) + ALLTRIM(lcDivLName))
ENDIF

lcCustPo =ALLTRIM(IIF(OrdHdr.MultiPO,IIF(llYes,"",LEFT(&lcTempOrd..CUSTPO,15)),LEFT(OrdHdr.CUSTPO,15)))

=SEEK('M'+ACCOUNT,'CUSTOMER')
lcCustVend =ALLTRIM(Customer.cCusVend)

*SSH
*ceditrntyp+ceditranno
PRIVATE lnOldAls,lcEdiTranNo
lcEdiTranNo=""

IF SEEK("850"+OrdHdr.Order,"edilibdt","Gmatran")
  lnOldAls=SELECT(0)
  SELECT edilibdt
  *--mhm
  *LOCATE REST WHILE ceditrntyp+ceditranno="850"+OrdHdr.Order FOR cEdiFilTyp="R" 
  LOCATE REST WHILE ceditrntyp+ceditranno="850"+OrdHdr.Order FOR cEdiFilTyp="R" AND cediref = OrdHdr.custpo
  *--mhm
  IF FOUND()
    lcEdiTranNo=cFileCode
  *! B611039,1 MMT 08/16/2015 Custom Order Confirmation form is not priniting file#[T20150806.0042][Start]
  ELSE
    IF SEEK("850"+OrdHdr.Order,"edilibdt","Gmatran")
      SELECT edilibdt
      LOCATE REST WHILE ceditrntyp+ceditranno="850"+OrdHdr.Order FOR cEdiFilTyp="R" AND STRTRAN(cediref,'-','') = OrdHdr.custpo
      IF FOUND()
        lcEdiTranNo=cFileCode
      ENDIF
    ENDIF  
  *! B611039,1 MMT 08/16/2015 Custom Order Confirmation form is not priniting file#[T20150806.0042][End]
  ENDIF
  SELECT(lnOldAls)
ENDIF
*SSH

llInStor=Customer.lIn_Store
llPprtPak = (!llRpPrTPak .AND. !EMPTY(Pack_id))

IF !llRpPrtCon &&fill the lcOrdHdr by the header information
  IF !SEEK(&lcTempOrd..cOrdType+lcOrder+&lcTempOrd..Store,lcOrdHdr)
    INSERT INTO &lcOrdHdr(cordtype,Order,Entered,Rep1,Rep2,lcComName,lcCmpAdd1,lcCmpAdd2,lcCmpAdd3,lcCmpAdd5,;
                      CustPo,Account,cCusVend,Store,Dept,Btname,ShptName,;
                      lcSoldTo1,lcSoldTo2,lcSoldTo3,lcShipTo1,lcShipTo2,lcShipTo3,Start,Complete,TermData,ShipVia,Season,cEdiTranNo); 
                      VALUES (OrdHdr.cOrdType,OrdHdr.Order,;
                      OrdHdr.Entered,OrdHdr.Rep1,OrdHdr.Rep2,lcDivision,ALLTRIM(laCompAdd1),ALLTRIM(laCompAdd2),ALLTRIM(laCompAdd3),;
                      ALLTRIM(laCompAdd5),lcCustpo,OrdHdr.Account,lcCustVend,&lcTempOrd..Store,OrdHdr.Dept,;
                      ALLTRIM(lcBtName),ALLTRIM(lcShptName),ALLTRIM(laSoldTo[1]),ALLTRIM(laSoldTo[2]),ALLTRIM(laSoldTo[3]),ALLTRIM(laShipTo[1]),;
                      laShipTo[2],laShipTo[3],OrdHdr.Start,OrdHdr.Complete,lcTermData,lcShipVia,lcSeason,lcEdiTranNo)
  ENDIF 
ELSE &&fill the LcOrdHdrSt by the header information
  IF lcOrder <> &lcOrdHdrSt..Order
    INSERT INTO &lcOrdHdrSt(cordtype,Order,llMulti,Entered,Rep1,Rep2,lcComName,lcCmpAdd1,lcCmpAdd2,lcCmpAdd3,lcCmpAdd5,;
                    CustPo,Account,cCusVend,Store,Dept,Btname,ShptName,;
                    lcSoldTo1,lcSoldTo2,lcSoldTo3,lcShipTo1,lcShipTo2,lcShipTo3,Start,Complete,TermData,ShipVia,Season,cEdiTranNo); 
                    VALUES (OrdHdr.cOrdType,OrdHdr.Order,IIF(OrdHdr.Multi ='Y',.T.,.F.),;
                    OrdHdr.Entered,OrdHdr.Rep1,OrdHdr.Rep2,lcDivision,ALLTRIM(laCompAdd1),ALLTRIM(laCompAdd2),ALLTRIM(laCompAdd3),;
                    ALLTRIM(laCompAdd5),lcCustpo,OrdHdr.Account,lcCustVend,&lcTempOrd..Store,OrdHdr.Dept,;
                    ALLTRIM(lcBtName),ALLTRIM(lcShptName),ALLTRIM(laSoldTo[1]),ALLTRIM(laSoldTo[2]),ALLTRIM(laSoldTo[3]),ALLTRIM(laShipTo[1]),;
                    ALLTRIM(laShipTo[2]),ALLTRIM(laShipTo[3]),OrdHdr.Start,OrdHdr.Complete,lcTermData,lcShipVia,lcSeason,lcEdiTranNo)
  ENDIF 
ENDIF  
  


*-- End Of lfPrnHdr.

*!*************************************************************
*! Name      : lfPrnNotPd
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 08/02/2004
*! Purpose   : Function to print the notepad
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Passed Parameters  : None.
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example            :  lfPrnNotPd()
*!*************************************************************
*
FUNCTION lfPrnNotPd
PRIVATE  lnAlias,lnOldMemW, lnMemLins, lnNotLine

lnAlias = SELECT()

IF llRpOrdNot && if Print NotePade.
  lnNotLine = 1
  SELECT NotePad
  lnMemWidth = SET("MEMOWIDTH")
  SET MEMOWIDTH TO 75
  IF SEEK ('B'+lcOrder)
    IF !llRpPrtCon
      REPLACE &lcOrdHdr..MNOTES WITH MNOTES
    ELSE 
      REPLACE &lcOrdHdrSt..MNOTES WITH MNOTES
    ENDIF
  ENDIF 
  SET MEMOWIDTH TO lnMemWidth
ENDIF
SELECT(lnAlias)

*-- End OF lfPrnNotPd.
*!***************************************************************************
*! Name      : lfGetOdHdr
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 08/02/2004
*! Purpose   : Function to get the information of the ORDER header.
*!***************************************************************************
*! Called from : None.
*!***************************************************************************
*! Calls       : None.
*!***************************************************************************
*! Passed Parameters : None.
*!***************************************************************************
*! Return            : None
*!***************************************************************************
*! Example           : = lfGetOdHdr()
*!***************************************************************************
*
FUNCTION lfGetOdHdr

PRIVATE lcAlias , lcCurrKey
lcCurrKey = ""

lnPageNo = 0

lcAlias = ALIAS()
 
lcOrder = &lcTempOrd..Order

lcMulti   = OrdHdr.Multi
lcAccount = Ordhdr.Account
SELECT (lcTempOrd)
lcStore = IIF(lcMulti <> 'Y',ORDHDR.STORE,lcStore)
STORE '' TO lcBtName,lcBtAddr1,lcBtAddr2,lcBtAddr3,lcBtAddr4,lcBtAddr5,lcBtAddr6

*-- GET DESCRIPTIONS FOR CODED FIELDS
SELECT CODES
*--Terms
lcTermData =SUBSTR(gfCodDes(OrdHdr.CTERMCODE , 'CTERMCODE'),1,15)
*--ShipVia
lcShipVia = SUBSTR(gfCodDes(OrdHdr.SHIPVIA , 'SHIPVIA'),1,15)
*--Division long name.
STORE '' TO lcDivLName
=gfRltFld(OrdHdr.cDivision,@laDivLName,'CDIVISION')
*--Special instruction
lcSpcInst = SUBSTR(gfCodDes(OrdHdr.SPCINST , 'SPCINST'),1,15)
*---Season
lcSeason = SUBSTR(gfCodDes(OrdHdr.SEASON , 'SEASON'),1,10)
*--Get the first line of the company address.

IF lcMulti = "Y" .AND. !llRpPrTCon

  SELECT CUSTOMER
  =SEEK('S'+ &lcTempOrd.. Account+ &lcTempOrd..Store)
  lcBtName  = CUSTOMER.BTNAME
  laSoldTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2')
  laSoldTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2')
  laSoldTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')
  laSoldTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '2')
  laSoldTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2')

  = lfAdrShift('laSoldTo')  && Shift Sold To address if there is empty line.
  *-- SHIP_TO ADDRESS FOR THIS STORE
  STORE '' TO lcStName,lcStAddr1,lcStAddr2,lcStAddr3,lcStAddr4,lcStAddr5,lcStAddr6
  IF OrdHdr.Alt_ShpTo
    lcShptName  = OrdHdr.STNAME
    laShipTo[1] = OrdHdr.cAddress1
    laShipTo[2] = OrdHdr.cAddress2
    laShipTo[3] = OrdHdr.cAddress3+','+OrdHdr.cAddress4+','+OrdHdr.cAddress5
    IF Len(ALLTRIM(lcStAddr2)) = 0
      lcStAddr2 = lcStAddr3
    ENDIF
  ELSE
    SELECT CUSTOMER
    =SEEK('S'+ &lcTempOrd.. Account+ &lcTempOrd..Store)

    lcShpTName = IIF(EMPTY(DBA) , STName , DBA)
    = gfGetAdr('CUSTOMER','','','',@laShipTo,'')
    = lfAdrShift('laShipTo')  && Shift Sold To address if there is empty line.
  ENDIF
ELSE
  lcBtName  = CUSTOMER.BTNAME
  SELECT CUSTOMER

  = gfGetAdr('CUSTOMER','','','',@laSoldTo,'2')

  = lfAdrShift('laSoldTo')  && Shift Sold To address if there is empty line.

  *-- SHIP_TO ADDRESS FOR THIS STORE
  STORE '' TO lcStName,lcStAddr1,lcStAddr2,lcStAddr3,lcStAddr4,lcStAddr5,lcStAddr6
  IF OrdHdr.Alt_ShpTo
    lcShptName  = OrdHdr.STNAME
    laShipTo[1] = OrdHdr.cAddress1
    laShipTo[2] = OrdHdr.cAddress2
    laShipTo[3] = OrdHdr.cAddress3+','+OrdHdr.cAddress4+','+OrdHdr.cAddress5
    IF Len(ALLTRIM(lcStAddr2)) = 0
      lcStAddr2 = lcStAddr3
    ENDIF
  ELSE
    SELECT CUSTOMER

    lcShpTName = IIF(EMPTY(DBA) , STName , DBA)
    = gfGetAdr('CUSTOMER','','','',@laShipTo,'')
    = lfAdrShift('laShipTo')  && Shift Sold To address if there is empty line.
  ENDIF

ENDIF
laCompAdd1 = laCompAdd[1]

laCompAdd1 = SPACE(ROUND((80-LEN( ALLTRIM(laCompAdd1) ))/2,0))+laCompAdd1

NOTELOOP = .F.

SELECT (lcAlias)

*-- End OF lfGetOdHdr.




*!***************************************************************************
*! Name      : lfvChkOrd
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 08/02/2004
*! Purpose   : Function to check if the user selects ONE multi store order.
*!***************************************************************************
*! Calls     : None.
*!***************************************************************************
*! Passed Parameters  : None.
*!***************************************************************************
*! Returns            : None.
*!***************************************************************************
*! Example            :  =lfvChkOrd()
*!***************************************************************************
*
FUNCTION lfvChkOrd

GO TOP IN (loOGScroll.laOGVrFlt[lnOrderPos,6])
llOneMulti = (SEEK('O'+EVALUATE(loOGScroll.laOGVrFlt[lnOrderPos,6]+'.Order'),'OrdHdr') OR ;
                          SEEK('C'+EVALUATE(loOGScroll.laOGVrFlt[lnOrderPos,6]+'.Order'),'OrdHdr') OR ;
                          SEEK('T'+EVALUATE(loOGScroll.laOGVrFlt[lnOrderPos,6]+'.Order'),'OrdHdr')) AND ;
             OrdHdr.Multi = 'Y'
IF !llOneMulti
  STORE '' TO laStores , laStorTar
ENDIF

RETURN llOneMulti
*-- End of lfvChkOrd.


*!**************************************************************************
*! Name      : lfItmPos
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 08/02/2004
*! Purpose   : To get the position of Variable filter in OG
*!**************************************************************************
*! Called from : OG When Function
*!**************************************************************************
*! Example   : = lfItmPos()
*!**************************************************************************
*
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(loOGScroll.laOGVrFlt,lcItmInFlt)
IF lnItmPos > 0
    lnItmPos = ASUBSCRIPT(loOGScroll.laOGVrFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos
*-- End of lfItmPos.

*!**************************************************************************
*! Name      : lpvGMAStor
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 08/02/2004
*! Purpose   : Function to select order's stores.
*! Reference : B604784,1
*!**************************************************************************
*! Example   : =lpvGMAStor()
*!**************************************************************************

FUNCTION lpvGMAStor
PRIVATE lcOrder
DIMENSION laStores[1] , laStorScr[1]
laStorScr = ''

lcOrder   = EVALUATE(loOGScroll.laOGVrFlt[lnOrderPos,6]+'.Order')
IF (SEEK('O'+lcOrder,'OrdHdr') AND SEEK('O'+lcOrder,'OrdLine')) OR ;
  (SEEK('C'+lcOrder,'OrdHdr') AND SEEK('C'+lcOrder,'OrdLine')) OR ;
  (SEEK('T'+lcOrder,'OrdHdr') AND SEEK('T'+lcOrder,'OrdLine'))
  SELECT OrdLine
  SCAN WHILE cOrdType+Order = OrdHdr.cOrdType+lcOrder
    IF ASCAN(laStorScr,ALLTRIM(Store)) = 0
      IF !EMPTY(laStorScr[ALEN(laStorScr,1)])
        DIMENSION laStorScr[ALEN(laStorScr,1)+1]
      ENDIF
      laStorScr[ALEN(laStorScr,1)] = ALLTRIM(Store)
    ENDIF
  ENDSCAN
ENDIF

IF !EMPTY(laStorScr)
*B128422,1 HMA 06/08/2005 Print Wrong Line Notes in case print Consolidating =Yes [Begin]
*--use the new filter change variable
  IF loOGScroll.llOGFltCh   &&If Filter Changed
*  IF llOGFltCh
*B128422,1 HMA 06/08/2005 Print Wrong Line Notes in case print Consolidating =Yes [End]
    DIMENSION laStorTar[1]
    laStorTar = .F.
  ENDIF
*B128422,1 HMA 06/08/2005 Print Wrong Line Notes in case print Consolidating =Yes [Begin]
*--use the new mover function
*  = gfMover(@laStorScr,@laStorTar,'Select Stores ',.T.)
 = lfOGMover(@laStorScr,@laStorTar,'Select Stores ',.T.)
*B128422,1 HMA 06/08/2005 Print Wrong Line Notes in case print Consolidating =Yes [End]
  laStores = ""
  IF !EMPTY(laStorTar[1])
    =ACOPY(laStorTar,laStores)
  ENDIF
ENDIF
*-- End of lpvGMAStor.
*!*************************************************************
*! Name      : lfConsYsNo
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 08/02/2004
*! Purpose   : Function to Validate consolidate
*!*************************************************************
*! Calls     :  None.
*!*************************************************************
*! Passed Parameters  : None.
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example            : =lfConsYsNo()
*!*************************************************************
*
FUNCTION lfConsYsNo
PARAMETER lcDummy

lcDummy = .T.
ClearRead()

RETURN lcDummy

*!*************************************************************
*! Name      : lfPackYsNo
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 08/02/2004
*! Purpose   : Function to Validate Pack_id
*!*************************************************************
*! Calls     :  None.
*!*************************************************************
*! Passed Parameters  : None.
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example            : =lfPackYsNo()
*!*************************************************************
*
FUNCTION lfPackYsNo
PARAMETER lcDummy
lcDummy = .T.
ClearRead()

RETURN lcDummy

*!*************************************************************
*! Name      : lfAddRec
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 08/02/2004
*! Purpose   : Function to accumolate the style/color totals
*!           : per order(Case consolidating by store)
*!*************************************************************
*! Calls     :  None.
*!*************************************************************
*! Passed Parameters  : None.
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example            : =lfAddRec()
*!*************************************************************
**
FUNCTION lfAddRec
PRIVATE lnAlias
lcStyle = ''
lnAlias = SELECT()
lcStyle =PADR(ALLTRIM(&lcTempOrd..Style),lnMjrPct,' ')

*--- Select the temp file that print accumolate styles/colors totals.

SELECT (lcTmpFile)
IF !llRpPack
  IF !llConfig
    llCheck = SEEK(&lcTempOrd..cOrdType+&lcTempOrd..Order+lcStyle,lcTmpFile)  && if found the same style in the file.
  ELSE
    llCheck = SEEK(&lcTempOrd..cOrdType+&lcTempOrd..Order+lcStyle+&lcTempOrd..Dyelot,lcTmpFile)  && if found the same style in the file.
  ENDIF 
ELSE
  IF !llConfig
    llCheck = SEEK(&lcTempOrd..cOrdType+&lcTempOrd..Order+lcStyle +&lcTempOrd..Pack_id +&lcTempOrd..cpkcolor+&lcTempOrd..cpcksize+&lcTempOrd..cPkVersion ,lcTmpFile)  && if found the same style in the file.
  ELSE
    llCheck = SEEK(&lcTempOrd..cOrdType+&lcTempOrd..Order+lcStyle+&lcTempOrd..Dyelot +&lcTempOrd..Pack_id +&lcTempOrd..cpkcolor+&lcTempOrd..cpcksize+&lcTempOrd..cPkVersion ,lcTmpFile)  && if found the same style in the file.
  ENDIF 
ENDIF
IF llCheck

  lnAvgPrice  = (nPrice * nTotQty +  &lcTempOrd..TotQty * &lcTempOrd..Price)/IIF((&lcTempOrd..TotQty + nTotQty)<>0,(&lcTempOrd..TotQty + nTotQty),1)

  REPLACE nTotQty WITH nTotQty + &lcTempOrd..TotQty
  IF llRpPrTCon
    REPLACE  Nqty1 WITH &lcTempOrd..Qty1+Nqty1,;
             Nqty2 WITH &lcTempOrd..Qty2+Nqty2,;
             Nqty3 WITH &lcTempOrd..Qty3+Nqty3,;
             Nqty4 WITH &lcTempOrd..Qty4+Nqty4,;
             Nqty5 WITH &lcTempOrd..Qty5+Nqty5,;
             Nqty6 WITH &lcTempOrd..Qty6+Nqty6,;
             Nqty7 WITH &lcTempOrd..Qty7+Nqty7,;
             Nqty8 WITH &lcTempOrd..Qty8+Nqty8
   ENDIF


  REPLACE  nPrice  WITH lnAvgPrice
  IF !llConfig
    llSeek=SEEK(&lcTempOrd..cOrdType+&lcTempOrd..Order+lcStyle+&lcTempOrd..Pack_id +&lcTempOrd..cpkcolor+&lcTempOrd..cpcksize+&lcTempOrd..cPkVersion,lcOrdLinSt)
  ELSE
    llSeek=SEEK(&lcTempOrd..cOrdType+&lcTempOrd..Order+lcStyle+&lcTempOrd..Dyelot+&lcTempOrd..Pack_id +&lcTempOrd..cpkcolor+&lcTempOrd..cpcksize+&lcTempOrd..cPkVersion,lcOrdLinSt)
  ENDIF 
  IF !llSeek
    SELECT (lcOrdLinSt)
    APPEND BLANK
    IF !llRpPack
      REPLACE Style      WITH ALLTRIM(&lcTempOrd..Style)          ,;
              Dyelot     WITH &lcTempOrd..Dyelot          ,;
              nPrice     WITH &lcTempOrd..Price          ,;
              cDesc      WITH SUBSTR(Style.Desc,1,19)    ,;
              nSugRetPri WITH &lcTempOrd..nSugRetPri     ,;
              nTotQty    WITH &lcTempOrd..TotQty
    ELSE
      REPLACE Style      WITH ALLTRIM(&lcTempOrd..Style)        ,;
              Dyelot     WITH &lcTempOrd..Dyelot          ,;
              nPrice     WITH &lcTempOrd..Price          ,;
              cDesc      WITH SUBSTR(Style.Desc,1,19)    ,;
              nSugRetPri WITH &lcTempOrd..nSugRetPri     ,;
              Pack_id    WITH &lcTempOrd..Pack_id ,;
              nTotQty    WITH &lcTempOrd..TotQty

      REPLACE cpkcolor   WITH &lcTempOrd..cpkcolor ,;
              cpcksize   WITH &lcTempOrd..cpcksize ,;
              cPkVersion WITH &lcTempOrd..cPkVersion,;
              Note_Mem   WITH &lcTempOrd..Note_Mem

    ENDIF
  ELSE
    SELECT (lcOrdLinSt)
    lnAvgPrice  = (nPrice * nTotQty +  &lcTempOrd..TotQty * &lcTempOrd..Price)/IIF((&lcTempOrd..TotQty + nTotQty)<>0,(&lcTempOrd..TotQty + nTotQty),1)
    REPLACE nTotQty WITH nTotQty + &lcTempOrd..TotQty,;
            nPrice  WITH lnAvgPrice
  ENDIF
ELSE  && else if didn't found the style in the file (New Style).
  IF !llConfig
    llSeek=SEEK(&lcTempOrd..cOrdType+&lcTempOrd..Order+lcStyle+&lcTempOrd..Pack_id +&lcTempOrd..cpkcolor+&lcTempOrd..cpcksize+&lcTempOrd..cPkVersion,lcOrdLinSt)            
  ELSE
    llSeek=SEEK(&lcTempOrd..cOrdType+&lcTempOrd..Order+lcStyle+&lcTempOrd..Dyelot+&lcTempOrd..Pack_id +&lcTempOrd..cpkcolor+&lcTempOrd..cpcksize+&lcTempOrd..cPkVersion,lcOrdLinSt)              
  ENDIF
  IF !llSeek
    SELECT (lcOrdLinSt)
    APPEND BLANK
    IF !llRpPack
      REPLACE Style      WITH ALLTRIM(&lcTempOrd..Style)         ,;
              Dyelot     WITH &lcTempOrd..Dyelot          ,;
              nPrice     WITH &lcTempOrd..Price          ,;
              cDesc      WITH SUBSTR(Style.Desc,1,19)    ,;
              nSugRetPri WITH &lcTempOrd..nSugRetPri     ,;
              nTotQty    WITH &lcTempOrd..TotQty
    ELSE
      REPLACE Style      WITH ALLTRIM(&lcTempOrd..Style)         ,;
              Dyelot     WITH &lcTempOrd..Dyelot          ,;
              nPrice     WITH &lcTempOrd..Price          ,;
              cDesc      WITH SUBSTR(Style.Desc,1,19)    ,;
              nSugRetPri WITH &lcTempOrd..nSugRetPri     ,;
              Pack_id    WITH &lcTempOrd..Pack_id ,;
              nTotQty    WITH &lcTempOrd..TotQty

       REPLACE cpkcolor   WITH &lcTempOrd..cpkcolor ,;
               cpcksize   WITH &lcTempOrd..cpcksize ,;
               cPkVersion WITH &lcTempOrd..cPkVersion,;
               Note_Mem   WITH &lcTempOrd..Note_Mem


    ENDIF
  ELSE
    SELECT (lcOrdLinSt) 

    lnAvgPrice  = (nPrice * nTotQty +  &lcTempOrd..TotQty * &lcTempOrd..Price)/IIF((&lcTempOrd..TotQty + nTotQty)<>0,(&lcTempOrd..TotQty + nTotQty),1)

    REPLACE nTotQty WITH nTotQty + &lcTempOrd..TotQty,;
            nPrice  WITH lnAvgPrice
  ENDIF

  SELECT (lcTmpFile)
  APPEND BLANK

  IF !llRpPack
    REPLACE cOrdType   WITH &lcTempOrd..cOrdType          ,;
            Order      WITH &lcTempOrd..order          ,;
            Style      WITH ALLTRIM(&lcTempOrd..Style)          ,;
            Dyelot     WITH &lcTempOrd..Dyelot          ,;
            nPrice     WITH &lcTempOrd..Price          ,;
            cDesc      WITH SUBSTR(Style.Desc,1,19)    ,;
            nSugRetPri WITH &lcTempOrd..nSugRetPri     ,;
            Pack       WITH lfGetPack()                ,;
            nTotQty    WITH &lcTempOrd..TotQty         ,;
            Store      WITH &lcTempOrd..Store
  ELSE
    REPLACE cOrdType   WITH &lcTempOrd..cOrdType          ,;
            Order      WITH &lcTempOrd..order          ,;
            Style      WITH ALLTRIM(&lcTempOrd..Style)      ,;
            Dyelot     WITH &lcTempOrd..Dyelot          ,;
            nPrice     WITH &lcTempOrd..Price          ,;
            cDesc      WITH SUBSTR(Style.Desc,1,19)    ,;
            nSugRetPri WITH &lcTempOrd..nSugRetPri     ,;
            Pack       WITH lfGetPack() ,;
            Pack_id    WITH &lcTempOrd..Pack_id        ,;
            nTotQty    WITH &lcTempOrd..TotQty         ,;
            Store      WITH &lcTempOrd..Store

    REPLACE cpkcolor   WITH &lcTempOrd..cpkcolor ,;
            cpcksize   WITH &lcTempOrd..cpcksize ,;
            cPkVersion WITH &lcTempOrd..cPkVersion

  ENDIF
  REPLACE  Nqty1 WITH &lcTempOrd..Qty1,;
           Nqty2 WITH &lcTempOrd..Qty2,;
           Nqty3 WITH &lcTempOrd..Qty3,;
           Nqty4 WITH &lcTempOrd..Qty4,;
           Nqty5 WITH &lcTempOrd..Qty5,;
           Nqty6 WITH &lcTempOrd..Qty6,;
           Nqty7 WITH &lcTempOrd..Qty7,;
           Nqty8 WITH &lcTempOrd..Qty8

ENDIF
LOCATE
SELECT(lnAlias)


*-- End Of  lfAddRec.

*!*************************************************************
*! Name      : lfGetGmSz
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 08/02/2004
*! Purpose   : Function To get Size discreption
*!*************************************************************
*! Calls     :
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Called Form        :
*!*************************************************************
*! Example            : =lfGetGmSz()
*!*************************************************************
*
FUNCTION lfGetGmSz
PARAMETER lcPackSize

PRIVATE lcNombr,lnScalePos
IF !EMPTY(lcPackSize)
  *--svae Scale Positon
  lnScalePos = RECNO('SCALE')
  =SEEK('S'+LEFT(lcPackSize,1),'SCALE')
  lcNombr = RIGHT(lcPackSize,1)
  lcSize=EVAL('SCALE.SZ'+lcNombr)
  IF BETWEEN(lnScalePos,1,RECCOUNT('SCALE'))
    GOTO lnScalePos In SCALE
  ENDIF

ELSE
  lcSize ='*****'
ENDIF

RETURN lcSize

*!*************************************************************
*! Name      : lfGetPkQty
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 08/02/2004
*! Purpose   : Function To get No of Packs
*!*************************************************************
*! Calls     :
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Called Form        :
*!*************************************************************
*! Example            : =lfGetPkQty()
*!*************************************************************
*
FUNCTION lfGetPkQty

*--accourding to new modification we change getting No Of Packs

SELECT (lcTempOrd)
lnPackQty  = npackNo

RETURN lnPackQty

*!***************************************************************************
*! Name      : lfGetPack
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 08/02/2004
*! Purpose   : Update the field TMPFILE.pack with either pack_id from ORDLINE
*!           : file or the sku
*!***************************************************************************
*! Called from : None.
*!***************************************************************************
*! Calls       : None.
*!***************************************************************************
*! Passed Parameters : None.
*!***************************************************************************
*! Return            : None
*!***************************************************************************
*! Example           : = lfGetPack()
*!***************************************************************************

FUNCTION lfGetPack
PRIVATE lcOrderset ,lcPackId
PRIVATE lnAlias
lnAlias = SELECT()

Store '' TO lcOrderset , lcPackId

*--Update PackUPC&PackSKU for each pack
  
IF llRpPack AND !EMPTY(&lcTempOrd..Pack_id)  && If not empty tempfile Get the Pack_Id from it.
  lcPackId = LANG_Soorcngm_Pack + &lcTempOrd..Pack_id+'-'+ &lcTempOrd..cpkcolor+'-'+lfGetGmSz(&lcTempOrd..cpcksize)+'-'+&lcTempOrd..cPkVersion
  *--replace in new table for it
  SELECT (lcTmpDesc)
  IF !llRpPrtCon
    IF !llConfig
      llseek=SEEK(&lcTempOrd..cOrdType+&lcTempOrd..Order+&lcTempOrd..Pack_id+&lcTempOrd..cpkcolor+&lcTempOrd..cpcksize+&lcTempOrd..cPkVersion+PADR(ALLTRIM(&lcTempOrd..Style),lnMjrPct,' ')+STR(&lcTempOrd..LINENO,6),lcTmpDesc)
    ELSE
      llseek=SEEK(&lcTempOrd..cOrdType+&lcTempOrd..Order+&lcTempOrd..Pack_id+&lcTempOrd..cpkcolor+&lcTempOrd..cpcksize+&lcTempOrd..cPkVersion+PADR(ALLTRIM(&lcTempOrd..Style),lnMjrPct,' ')+&lcTempOrd..Dyelot+STR(&lcTempOrd..LINENO,6),lcTmpDesc)    
    ENDIF 
  ELSE 
    IF !llConfig
      llseek=SEEK(&lcTempOrd..cOrdType+&lcTempOrd..Order+&lcTempOrd..Pack_id+ &lcTempOrd..cpkcolor+&lcTempOrd..cpcksize+&lcTempOrd..cPkVersion+ALLTRIM(&lcTempOrd..Style),lcTmpDesc)
    ELSE
      llseek=SEEK(&lcTempOrd..cOrdType+&lcTempOrd..Order+&lcTempOrd..Pack_id+ &lcTempOrd..cpkcolor+&lcTempOrd..cpcksize+&lcTempOrd..cPkVersion+ALLTRIM(&lcTempOrd..Style)+&lcTempOrd..Dyelot,lcTmpDesc)
    ENDIF 
  ENDIF 
  IF !llSeek
    APPEND BLANK
    REPLACE cOrdType   WITH &lcTempOrd..cOrdType   ,;
            Order      WITH &lcTempOrd..Order  ,;
            Pack_id    WITH &lcTempOrd..Pack_id   ,;
            cpkcolor   WITH &lcTempOrd..cpkcolor  ,;
            cpcksize   WITH &lcTempOrd..cpcksize  ,;
            cPkVersion WITH &lcTempOrd..cPkVersion,;
            Style      WITH ALLTRIM(&lcTempOrd..Style),;
            Dyelot     WITH &lcTempOrd..Dyelot  ,;
            LineNo     WITH &lcTempOrd..LineNo   ,;
            cSizeDesc  WITH lfGetGmSz(&lcTempOrd..cpcksize)  
  ENDIF
  SELECT (lnAlias)
  PRIVATE lcStyUpcOrd
  lcStyUpcOrd = ORDER("STYLEUPC")
  SET ORDER TO TAG Gmapkupc IN STYLEUPC
  IF SEEK(&lcTempOrd..Account+PADR(&lcTempOrd..Pack_id,19)+&lcTempOrd..cpkcolor+&lcTempOrd..cPcksize,"STYLEUPC") OR;
     SEEK("*****"+PADR(&lcTempOrd..Pack_id,19)+&lcTempOrd..cpkcolor+&lcTempOrd..cPcksize,"STYLEUPC")
    IF !EMPTY(lcStyUpcOrd)
      SET ORDER TO TAG (lcStyUpcOrd) IN STYLEUPC
    ENDIF

    lcPackId = lcPackId + SPACE(2) +'|'+ LANG_Soorcngm_PckUpc +STYLEUPC.Cupcnum1+STYLEUPC.Cupcnum2+STYLEUPC.Cupcnum3
    SELECT (lcTmpDesc)
    REPLACE PackUPC WITH STYLEUPC.Cupcnum1+STYLEUPC.Cupcnum2+STYLEUPC.Cupcnum3
    SELECT (lnAlias)

  ENDIF
  IF !EMPTY(lcStyUpcOrd)
    SET ORDER TO TAG (lcStyUpcOrd) IN STYLEUPC
  ENDIF

  PRIVATE lcSpklnOrd
  lcSpklnOrd = ORDER("SPCK_LIN")
  SET ORDER TO TAG Spck_linst IN SPCK_LIN
  IF llRpPack AND (SEEK("S"+&lcTempOrd..Account+PADR(&lcTempOrd..Pack_id,19)+&lcTempOrd..cPkColor+;
                       &lcTempOrd..cPckSize,"SPCK_LIN") OR  ;
                  SEEK("S"+"*****"+PADR(&lcTempOrd..Pack_id,19)+&lcTempOrd..cPkColor+;
                       &lcTempOrd..cPckSize,"SPCK_LIN"))

    lcPackId = IIF(EMPTY(lcPackId),LANG_Soorcngm_PckSku + SPCK_LIN.Pack_id,;
                      lcPackId+'|'+LANG_Soorcngm_PckSku + SPCK_LIN.Pack_id)
    SELECT (lcTmpDesc)
    REPLACE PackSKU WITH SPCK_LIN.Pack_id
    SELECT (lnAlias)
  ENDIF
  IF !EMPTY(lcSpklnOrd)
    SET ORDER TO TAG (lcSpklnOrd) IN SPCK_LIN
  ENDIF

ENDIF

lcOrderset = ORDER("SPCK_LIN")
SET ORDER TO TAG SPCKLINS IN SPCK_LIN

*--Update UPCs & sizes for each Style line

IF SEEK(&lcTempOrd..Style,"STYLEUPC")
  IF llRpPack
    SELECT (lcTmpDesc)
    IF !llRpPrtCon
      IF !llConfig
        llSeek=SEEK(&lcTempOrd..cOrdType+&lcTempOrd..Order+&lcTempOrd..Pack_id+ &lcTempOrd..cpkcolor+&lcTempOrd..cpcksize+&lcTempOrd..cPkVersion+PADR(ALLTRIM(&lcTempOrd..Style),lnMjrPct,' ')+STR(&lcTempOrd..LINENO,6),lcTmpDesc)
      ELSE
        llSeek=SEEK(&lcTempOrd..cOrdType+&lcTempOrd..Order+&lcTempOrd..Pack_id+ &lcTempOrd..cpkcolor+&lcTempOrd..cpcksize+&lcTempOrd..cPkVersion+PADR(ALLTRIM(&lcTempOrd..Style),lnMjrPct,' ')+&lcTempOrd..Dyelot+STR(&lcTempOrd..LINENO,6),lcTmpDesc)
      ENDIF 
    ELSE 
      IF !llConfig
        llSeek=SEEK(&lcTempOrd..cOrdType+&lcTempOrd..Order+&lcTempOrd..Pack_id+ &lcTempOrd..cpkcolor+&lcTempOrd..cpcksize+&lcTempOrd..cPkVersion+ALLTRIM(&lcTempOrd..Style),lcTmpDesc)
      ELSE
        llSeek=SEEK(&lcTempOrd..cOrdType+&lcTempOrd..Order+&lcTempOrd..Pack_id+ &lcTempOrd..cpkcolor+&lcTempOrd..cpcksize+&lcTempOrd..cPkVersion+ALLTRIM(&lcTempOrd..Style)+&lcTempOrd..Dyelot,lcTmpDesc)
      ENDIF 
    ENDIF 
    IF !llSeek
      APPEND BLANK
      REPLACE cOrdType   WITH &lcTempOrd..cOrdType   ,;
              Order      WITH &lcTempOrd..Order  ,;
              Pack_id    WITH &lcTempOrd..Pack_id ,;
              cpkcolor   WITH &lcTempOrd..cpkcolor,;
              cpcksize   WITH &lcTempOrd..cpcksize,;
              cPkVersion WITH &lcTempOrd..cPkVersion,;
              Style      WITH ALLTRIM(&lcTempOrd..Style),;
              Dyelot     WITH &lcTempOrd..Dyelot  ,;
              LineNo     WITH &lcTempOrd..LineNo   ,;
              cSizeDesc  WITH lfGetGmSz(&lcTempOrd..cpcksize)  
    ENDIF
  ELSE
    SELECT (lcTmpDesc)
    IF !llRpPrtCon
      IF !llConfig
        llSeek=SEEK(&lcTempOrd..cOrdType+&lcTempOrd..Order+PADR(ALLTRIM(&lcTempOrd..Style),lnMjrPct,' ')+STR(&lcTempOrd..LINENO,6),lcTmpDesc)
      ELSE
        llSeek=SEEK(&lcTempOrd..cOrdType+&lcTempOrd..Order+PADR(ALLTRIM(&lcTempOrd..Style),lnMjrPct,' ')+&lcTempOrd..Dyelot+STR(&lcTempOrd..LINENO,6),lcTmpDesc)
      ENDIF 
    ELSE 
      IF !llConfig
        llSeek=SEEK(&lcTempOrd..cOrdType+&lcTempOrd..Order+ALLTRIM(&lcTempOrd..Style),lcTmpDesc)
      ELSE 
        llSeek=SEEK(&lcTempOrd..cOrdType+&lcTempOrd..Order+ALLTRIM(&lcTempOrd..Style)+&lcTempOrd..Dyelot,lcTmpDesc)
      ENDIF 
    ENDIF 
    IF !llSeek
      APPEND BLANK
      REPLACE cOrdType   WITH &lcTempOrd..cOrdType   ,;
              Order      WITH &lcTempOrd..Order  ,;
              Style      WITH ALLTRIM(&lcTempOrd..Style),;
              Dyelot     WITH &lcTempOrd..Dyelot  ,;
              LineNo     WITH &lcTempOrd..LineNo ,;
              Pack_id    WITH &lcTempOrd..Pack_id ,;
              cpkcolor   WITH &lcTempOrd..cpkcolor,;
              cpcksize   WITH &lcTempOrd..cpcksize,;
              cPkVersion WITH &lcTempOrd..cPkVersion                 
    ENDIF
  ENDIF

  FOR lnNum =1 TO 8
    lcNum = STR(lnNum,1)
    *B610784,1 MMT 07/29/2014 Custom Order confirmation form does not print size w/o UPC [T20140723.0012][Start]
    *IF !EMPTY(&lcTempOrd..qty&lcNum) .AND. SEEK(&lcTempOrd..Style+lcNum,"STYLEUPC")
    IF !EMPTY(&lcTempOrd..qty&lcNum) 
      IF SEEK(&lcTempOrd..Style+lcNum,"STYLEUPC")
    *B610784,1 MMT 07/29/2014 Custom Order confirmation form does not print size w/o UPC [T20140723.0012][END]
      IF llRpSQty
        lcPackId = IIF(EMPTY(lcPackId),LANG_Soorcngm_StyUpc +STYLEUPC.Cupcnum1+STYLEUPC.Cupcnum2+STYLEUPC.Cupcnum3+SPACE(2)+SCALE.SZ&lcNum+SPACE(2),;
                          lcPackId+'|'+LANG_Soorcngm_StyUpc +STYLEUPC.Cupcnum1+STYLEUPC.Cupcnum2+STYLEUPC.Cupcnum3+SPACE(2)+SCALE.SZ&lcNum+SPACE(2))
        REPLACE UPC&lcNum   WITH STYLEUPC.Cupcnum1+STYLEUPC.Cupcnum2+STYLEUPC.Cupcnum3,;
                Size&lcNum  WITH SCALE.SZ&lcNum,;
                llStyUPC    WITH .T.
      ELSE  && No to print sizes Qty.
        *----  Add UPC # to lcPackId like "Pack # line|UPC # upc1 sz1 sz2 sz3 'Qty'  TotalQty"

        lcPackId = IIF(EMPTY(lcPackId),LANG_Soorcngm_StyUpc +STYLEUPC.Cupcnum1+STYLEUPC.Cupcnum2+STYLEUPC.Cupcnum3+SPACE(2)+SCALE.SZ&lcNum,;
                          lcPackId+'|'+LANG_Soorcngm_StyUpc +STYLEUPC.Cupcnum1+STYLEUPC.Cupcnum2+STYLEUPC.Cupcnum3+SPACE(2)+SCALE.SZ&lcNum)
        REPLACE UPC&lcNum   WITH STYLEUPC.Cupcnum1+STYLEUPC.Cupcnum2+STYLEUPC.Cupcnum3,;
                Size&lcNum  WITH SCALE.SZ&lcNum,;
                llStyUPC    WITH .T.
      ENDIF
      *B610784,1 MMT 07/29/2014 Custom Order confirmation form does not print size w/o UPC [T20140723.0012][Start]
      ELSE
        REPLACE Size&lcNum  WITH SCALE.SZ&lcNum
      ENDIF
      *B610784,1 MMT 07/29/2014 Custom Order confirmation form does not print size w/o UPC [T20140723.0012][END]
    ENDIF
  ENDFOR
ENDIF

IF  SEEK("S"+&lcTempOrd..Account+&lcTempOrd..Style,"SPCK_LIN")
  IF llRpPack
    SELECT (lcTmpDesc)
    IF !llRpPrtCon
      IF !llConfig
        llSeek=SEEK(&lcTempOrd..cOrdType+&lcTempOrd..Order+&lcTempOrd..Pack_id+ &lcTempOrd..cpkcolor+&lcTempOrd..cpcksize+&lcTempOrd..cPkVersion+PADR(ALLTRIM(&lcTempOrd..Style),lnMjrPct,' ')+STR(&lcTempOrd..LINENO,6),lcTmpDesc)
      ELSE
        llSeek=SEEK(&lcTempOrd..cOrdType+&lcTempOrd..Order+&lcTempOrd..Pack_id+ &lcTempOrd..cpkcolor+&lcTempOrd..cpcksize+&lcTempOrd..cPkVersion+PADR(ALLTRIM(&lcTempOrd..Style),lnMjrPct,' ')+&lcTempOrd..Dyelot+STR(&lcTempOrd..LINENO,6),lcTmpDesc)
      ENDIF 
    ELSE 
      IF !llConfig
        llSeek=SEEK(&lcTempOrd..cOrdType+&lcTempOrd..Order+&lcTempOrd..Pack_id+ &lcTempOrd..cpkcolor+&lcTempOrd..cpcksize+&lcTempOrd..cPkVersion+ALLTRIM(&lcTempOrd..Style),lcTmpDesc)
      ELSE
        llSeek=SEEK(&lcTempOrd..cOrdType+&lcTempOrd..Order+&lcTempOrd..Pack_id+ &lcTempOrd..cpkcolor+&lcTempOrd..cpcksize+&lcTempOrd..cPkVersion+ALLTRIM(&lcTempOrd..Style)+&lcTempOrd..Dyelot,lcTmpDesc)
      ENDIF
    ENDIF 
    IF !llSeek
      APPEND BLANK
      REPLACE cOrdType   WITH &lcTempOrd..cOrdType   ,;
              Order      WITH &lcTempOrd..Order  ,;
              Pack_id    WITH &lcTempOrd..Pack_id ,;
              cpkcolor   WITH &lcTempOrd..cpkcolor,;
              cpcksize   WITH &lcTempOrd..cpcksize,;
              cPkVersion WITH &lcTempOrd..cPkVersion,;
              Style      WITH ALLTRIM(&lcTempOrd..Style),;
              Dyelot     WITH &lcTempOrd..Dyelot  ,;
              LineNo     WITH &lcTempOrd..LineNo   ,;
              cSizeDesc   WITH lfGetGmSz(&lcTempOrd..cpcksize)  
    ENDIF
  ELSE
    SELECT (lcTmpDesc)
    IF !llRpPrtCon
      IF !llConfig
        llSeek=SEEK(&lcTempOrd..cOrdType+&lcTempOrd..Order+PADR(ALLTRIM(&lcTempOrd..Style),lnMjrPct,' ')+STR(&lcTempOrd..LINENO,6),lcTmpDesc)
      ELSE
        llSeek=SEEK(&lcTempOrd..cOrdType+&lcTempOrd..Order+PADR(ALLTRIM(&lcTempOrd..Style),lnMjrPct,' ')+&lcTempOrd..Dyelot+STR(&lcTempOrd..LINENO,6),lcTmpDesc)
      ENDIF 
    ELSE 
      IF !llConfig
        llSeek=SEEK(&lcTempOrd..cOrdType+&lcTempOrd..Order+ALLTRIM(&lcTempOrd..Style),lcTmpDesc)
      ELSE
        llSeek=SEEK(&lcTempOrd..cOrdType+&lcTempOrd..Order+ALLTRIM(&lcTempOrd..Style)+&lcTempOrd..Dyelot,lcTmpDesc)
      ENDIF 
    ENDIF 
    IF !llSeek
      APPEND BLANK
      REPLACE cOrdType   WITH &lcTempOrd..cOrdType   ,;
              Order      WITH &lcTempOrd..Order  ,;
              Style      WITH ALLTRIM(&lcTempOrd..Style),;
              Dyelot     WITH &lcTempOrd..Dyelot  ,;
              LineNo     WITH &lcTempOrd..LineNo   ,;
              Pack_id    WITH &lcTempOrd..Pack_id ,;
              cpkcolor   WITH &lcTempOrd..cpkcolor,;
              cpcksize   WITH &lcTempOrd..cpcksize,;
              cPkVersion WITH &lcTempOrd..cPkVersion              
    ENDIF
  ENDIF
  lnNum = 1
  lcNum = STR(lnNum,1)
  SELECT SPCK_LIN
  SCAN REST WHILE Type+Account+Style+Pack_id = "S"+&lcTempOrd..Account+LEFT(ALLTRIM(&lcTempOrd..Style),LEN((&lcTempOrd..Style))-1)

    IF EMPTY(lcPackId)

      lcPackId = IIF(&lcTempOrd..Qty&lcNum = 0,'',LANG_Soorcngm_StySku+ SPCK_LIN.Pack_id+SCALE.SZ&lcNum+SPACE(2))
      FOR lnCount = 1 to 8
        lcCount = STR(lnCount,1)
        IF !EMPTY(SPCK_LIN.QTY&lcCount)
          SELECT (lcTmpDesc)
          REPLACE SKU&lcCount   WITH SPCK_LIN.Pack_id,;
                  Size&lcCount  WITH SCALE.SZ&lcCount,;
                  llStySku      WITH .T.
        ENDIF
      ENDFOR
    ELSE

      lcPackId = lcPackId+IIF(&lcTempOrd..Qty&lcNum=0,'','|'+LANG_Soorcngm_StySku+ SPCK_LIN.Pack_id+SCALE.SZ&lcNum+SPACE(2))
      FOR lnCount = 1 to 8
        lcCount = STR(lnCount,1)
        IF !EMPTY(SPCK_LIN.QTY&lcCount)
          SELECT (lcTmpDesc)
          REPLACE SKU&lcCount   WITH SPCK_LIN.Pack_id,;
                  Size&lcCount  WITH SCALE.SZ&lcCount,;
                  llStySku      WITH .T.
        ENDIF
      ENDFOR
    ENDIF
    lnNum = lnNum +1
    *! B611106,1 MMT 01/26/2016 Error while previewing Order Confirmation form GM[T20160119.0012][Start]
    IF lnNum > 8
      EXIT
    ENDIF
    *! B611106,1 MMT 01/26/2016 Error while previewing Order Confirmation form GM[T20160119.0012][End]
    lcNum = STR(lnNum,1)
  ENDSCAN
ENDIF

*--if not found UPC or SKU for style line
IF !SEEK("S"+&lcTempOrd..Account+&lcTempOrd..Style,"SPCK_LIN") AND !SEEK(&lcTempOrd..Style,"STYLEUPC")
  IF llRpPack
    SELECT (lcTmpDesc)
    IF !llRpPrtCon
      IF !llConfig
        llSeek=SEEK(&lcTempOrd..cOrdType+&lcTempOrd..Order+&lcTempOrd..Pack_id+ &lcTempOrd..cpkcolor+&lcTempOrd..cpcksize+&lcTempOrd..cPkVersion+PADR(ALLTRIM(&lcTempOrd..Style),lnMjrPct,' ')+STR(&lcTempOrd..LINENO,6),lcTmpDesc)
      ELSE
        llSeek=SEEK(&lcTempOrd..cOrdType+&lcTempOrd..Order+&lcTempOrd..Pack_id+ &lcTempOrd..cpkcolor+&lcTempOrd..cpcksize+&lcTempOrd..cPkVersion+PADR(ALLTRIM(&lcTempOrd..Style),lnMjrPct,' ')+&lcTempOrd..Dyelot+STR(&lcTempOrd..LINENO,6),lcTmpDesc)
      ENDIF   
    ELSE 
      IF !llConfig
        llSeek=SEEK(&lcTempOrd..cOrdType+&lcTempOrd..Order+&lcTempOrd..Pack_id+ &lcTempOrd..cpkcolor+&lcTempOrd..cpcksize+&lcTempOrd..cPkVersion+ALLTRIM(&lcTempOrd..Style),lcTmpDesc)
      ELSE
        llSeek=SEEK(&lcTempOrd..cOrdType+&lcTempOrd..Order+&lcTempOrd..Pack_id+ &lcTempOrd..cpkcolor+&lcTempOrd..cpcksize+&lcTempOrd..cPkVersion+ALLTRIM(&lcTempOrd..Style)+&lcTempOrd..Dyelot,lcTmpDesc)
      ENDIF 
    ENDIF 
    IF !llSeek
      APPEND BLANK
      REPLACE cOrdType   WITH &lcTempOrd..cOrdType   ,;
              Order      WITH &lcTempOrd..Order  ,;
              Pack_id    WITH &lcTempOrd..Pack_id ,;
              cpkcolor   WITH &lcTempOrd..cpkcolor,;
              cpcksize   WITH &lcTempOrd..cpcksize,;
              cPkVersion WITH &lcTempOrd..cPkVersion,;
              Style      WITH ALLTRIM(&lcTempOrd..Style),;
              Dyelot     WITH &lcTempOrd..Dyelot  ,;
              LineNo     WITH &lcTempOrd..LineNo ,;   
              cSizeDesc  WITH lfGetGmSz(&lcTempOrd..cpcksize)  
    ENDIF
  ELSE
    SELECT (lcTmpDesc)
    IF !llRpPrtCon
      IF !llConfig
        llSeek=SEEK(&lcTempOrd..cOrdType+&lcTempOrd..Order+PADR(ALLTRIM(&lcTempOrd..Style),lnMjrPct,' ')+STR(&lcTempOrd..LINENO,6),lcTmpDesc)
      ELSE
        llSeek=SEEK(&lcTempOrd..cOrdType+&lcTempOrd..Order+PADR(ALLTRIM(&lcTempOrd..Style),lnMjrPct,' ')+&lcTempOrd..Dyelot+STR(&lcTempOrd..LINENO,6),lcTmpDesc)
      ENDIF 
    ELSE 
      IF !llConfig
        llSeek=SEEK(&lcTempOrd..cOrdType+&lcTempOrd..Order+ALLTRIM(&lcTempOrd..Style),lcTmpDesc)
      ELSE 
        llSeek=SEEK(&lcTempOrd..cOrdType+&lcTempOrd..Order+ALLTRIM(&lcTempOrd..Style)+&lcTempOrd..Dyelot,lcTmpDesc)
      ENDIF 
    ENDIF 
    IF !llSeek
      APPEND BLANK
      REPLACE cOrdType   WITH &lcTempOrd..cOrdType   ,;
              Order      WITH &lcTempOrd..Order  ,;
              Style      WITH ALLTRIM(&lcTempOrd..Style),;
              Dyelot     WITH &lcTempOrd..Dyelot  ,;
              LineNo     WITH &lcTempOrd..LineNo   ,;
              Pack_id    WITH &lcTempOrd..Pack_id ,;
              cpkcolor   WITH &lcTempOrd..cpkcolor,;
              cpcksize   WITH &lcTempOrd..cpcksize,;
              cPkVersion WITH &lcTempOrd..cPkVersion
    ENDIF
  ENDIF

  FOR lnNum =1 TO 8
    lcNum = STR(lnNum,1)
    IF !EMPTY(&lcTempOrd..qty&lcNum)
      REPLACE Size&lcNum  WITH SCALE.SZ&lcNum
    ENDIF
  ENDFOR
ENDIF

IF !EMPTY(lcOrderset)   && restore the ordrer for spck_lin
  SET ORDER TO TAG &lcOrderset IN  SPCK_LIN
ENDIF
SELECT(lnAlias)

RETURN lcPackId

*-- End Of lfGetPack.

*!***************************************************************************
*! Name      : lfPrtpack
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 08/02/2004
*! Purpose   : Function to print field Pack,the style SKU,Style UPC,Style Qty.
*!***************************************************************************
*! Calls     : None.
*!***************************************************************************
*! Passed Parameters  : None.
*!***************************************************************************
*! Returns            : None.
*!***************************************************************************
*! Example            :  lfPrtpack(Pack)
*!***************************************************************************
*
FUNCTION lfPrtpack
PARAMETER lcPack ,lcLocStore

PRIVATE lnUPCOcc ,lnUPCPrnt
STORE 0 TO lnUPCOcc ,lnUPCPrnt

IF !llRpPack
  IF !llRpPrtCon
    IF !llConfig
      llFound = SEEK(cOrdType+Order+PADR(ALLTRIM(STYLE),lnMjrPct,' ')+STR(LINENO,6),lcTmpDesc) .AND. (EMPTY(&lctmpDesc..MUPC)) 
    ELSE 
      llFound = SEEK(cOrdType+Order+PADR(ALLTRIM(STYLE),lnMjrPct,' ')+Dyelot+STR(LINENO,6),lcTmpDesc) .AND. (EMPTY(&lctmpDesc..MUPC)) 
    ENDIF 
  ELSE   
    SELECT(lcOrdLinSt)
    IF !llConfig
      llFound = SEEK(cOrdType+Order+ALLTRIM(STYLE),lcTmpDesc) .AND. (EMPTY(&lctmpDesc..MUPC)) 
    ELSE
      llFound = SEEK(cOrdType+Order+ALLTRIM(STYLE)+Dyelot,lcTmpDesc) .AND. (EMPTY(&lctmpDesc..MUPC)) 
    ENDIF 
  ENDIF 
ELSE
  IF !llRpPrtCon
    IF !llConfig
      llFound = SEEK(cOrdType+Order+Pack_Id +cPkColor+cPckSize+ cPkVersion+PADR(ALLTRIM(STYLE),lnMjrPct,' ')+STR(LINENO,6),lcTmpDesc)  .AND. (EMPTY(&lctmpDesc..MUPC)) 
    ELSE
      llFound = SEEK(cOrdType+Order+Pack_Id +cPkColor+cPckSize+ cPkVersion+PADR(ALLTRIM(STYLE),lnMjrPct,' ')+Dyelot+STR(LINENO,6),lcTmpDesc)  .AND. (EMPTY(&lctmpDesc..MUPC)) 
    ENDIF 
  ELSE 
    SELECT(lcOrdLinSt)
    IF !llConfig
      llFound = SEEK(cOrdType+Order+Pack_Id +cPkColor+cPckSize+ cPkVersion+ALLTRIM(STYLE),lcTmpDesc)  .AND. (EMPTY(&lctmpDesc..MUPC)) 
    ELSE
      llFound = SEEK(cOrdType+Order+Pack_Id +cPkColor+cPckSize+ cPkVersion+ALLTRIM(STYLE)+Dyelot,lcTmpDesc)  .AND. (EMPTY(&lctmpDesc..MUPC)) 
    ENDIF 
  ENDIF 
ENDIF

IF llFound
  IF llRpPrTPak OR EMPTY(Pack_id) OR !llRpPrTCon
    
    IF EMPTY(Pack_id) AND !llRpPrtCon 
      REPLACE &lcOrdLin..lcLocStore WITH  LEFT(lcLocStore,6) 
    ENDIF


    lnLineNo=0  && UPC Line
    lnLineNo1=0 && Size Line
    lnLineNo2=0 && Qty Line
    lnLineNo3=0 && Sku line
    llNewLine = .T.
    llNewLine1 = .T.
    llNewLine2 = .T.    
    llNewLine3 = .T.    
    *--both SKU and UPC Found
    
    *: B610754,1 HES 06/22/2014 Let the size break down appear regardless the UPC and SKU  [START]
*!*	    IF &lcTmpDesc..llStyUPC AND &lcTmpDesc..llStySKU
    *: B610754,1 HES 06/22/2014 Let the size break down appear regardless the UPC and SKU  [END  ]
      llUpcSku=.T.
      FOR lnCount =1 to 8
        lnLineNo=IIF(llNewLine = .T.,lnLineNO+1,lnLineNO)
        lnLineNo1=IIF(llNewLine1 = .T.,lnLineNO1+1,lnLineNO1)        
        lnLineNo2=IIF(llNewLine2 = .T.,lnLineNO2+1,lnLineNO2)                
        lnLineNo3=IIF(llNewLine3 = .T.,lnLineNO3+1,lnLineNO3)
        lcCount = STR(lnCount,1)
        IF llRpPrTCon
          llChkQty = IIF(EMPTY(&lcTmpFile..nQty&lcCount),.F.,.T.)
        ELSE
          llChkQty = IIF(EMPTY(Qty&lcCount),.F.,.T.)
        ENDIF
        IF llChkQty
          REPLACE &lcTmpDesc..mUPC WITH &lcTmpDesc..mUPC + LANG_Soorcngm_StyUpc + PADR(&lcTmpDesc..UPC&lcCount ,16,' ')
          IF llRpSQty
            REPLACE &lcTmpDesc..mSize WITH &lcTmpDesc..mSize + PADR(&lcTmpDesc..Size&lcCount ,5,' ')
            IF llRpPrTCon
              REPLACE &lcOrdLin..nQty&lcCount WITH &lcTmpFile..nQty&lcCount
              REPLACE &lcTmpDesc..mQty WITH &lcTmpDesc..mQty +STR(&lcOrdLin..nQty&lcCount)
            ELSE
              REPLACE &lcOrdLin..Qty&lcCount WITH Qty&lcCount
              REPLACE &lcTmpDesc..mQty WITH &lcTmpDesc..mQty +STR(&lcOrdLin..Qty&lcCount)
            ENDIF
          ENDIF
          REPLACE &lcTmpDesc..mSKU WITH &lcTmpDesc..mSKU + LANG_Soorcngm_StySku + PADR(&lcTmpDesc..SKU&lcCount ,16,' ') 
        ENDIF
        lcUPCvalue= MLINE(&lcTmpDesc..mUPC,lnLineNo)
        lcSizeVal= MLINE(&lcTmpDesc..mSize,lnLineNo1)
        lcQtyVal= MLINE(&lcTmpDesc..mQty,lnLineNo2)        
        lcSKUvalue= MLINE(&lcTmpDesc..mSKU,lnLineNo3)
        IF !EMPTY(lcUPCvalue)
          REPLACE &lcTmpDesc..mUPC WITH &lcTmpDesc..mUPC +CHR(13)
          llNewLine = .T.
        ELSE         
          llNewLine = .F.
        ENDIF 
        IF !EMPTY(lcSizeVal)
          REPLACE &lcTmpDesc..mSize WITH &lcTmpDesc..mSize +CHR(13)
          llNewLine1 = .T.
        ELSE         
          llNewLine1 = .F.
        ENDIF 
        IF !EMPTY(lcQtyVal)
          REPLACE &lcTmpDesc..mQty WITH &lcTmpDesc..mQty +CHR(13)
          llNewLine2 = .T.
        ELSE         
          llNewLine2 = .F.
        ENDIF         
        IF !EMPTY(lcSKUvalue)
          REPLACE &lcTmpDesc..mSKU WITH &lcTmpDesc..mSKU +CHR(13)
          llNewLine3 = .T.
        ELSE         
          llNewLine3 = .F.
        ENDIF 
      ENDFOR
    *: B610754,1 HES 06/22/2014 Let the size break down appear regardless the UPC and SKU  [START
*!*	    ENDIF
    *--UPC only Found
*!*	    IF &lcTmpDesc..llStyUPC AND !&lcTmpDesc..llStySKU
*!*	      llStyUPC=.T.
*!*	      FOR lnCount =1 to 8
*!*	        lnLineNo=IIF(llNewLine = .T.,lnLineNO+1,lnLineNO)
*!*	        lnLineNo1=IIF(llNewLine1 = .T.,lnLineNO1+1,lnLineNO1)        
*!*	        lnLineNo2=IIF(llNewLine2 = .T.,lnLineNO2+1,lnLineNO2)        
*!*	        lcCount = STR(lnCount,1)
*!*	        IF llRpPrTCon
*!*	          llChkQty = IIF(EMPTY(&lcTmpFile..nQty&lcCount),.F.,.T.)
*!*	        ELSE
*!*	          llChkQty = IIF(EMPTY(Qty&lcCount),.F.,.T.)
*!*	        ENDIF
*!*	        IF llChkQty
*!*	          REPLACE &lcTmpDesc..mUPC WITH &lcTmpDesc..mUPC + LANG_Soorcngm_StyUpc + PADR(&lcTmpDesc..UPC&lcCount ,16,' ')
*!*	          IF llRpSQty
*!*	            REPLACE &lcTmpDesc..mSize WITH &lcTmpDesc..mSize + PADR(&lcTmpDesc..Size&lcCount ,5,' ')
*!*	            IF llRpPrTCon
*!*	              REPLACE &lcOrdLin..nQty&lcCount WITH &lcTmpFile..nQty&lcCount
*!*	              REPLACE &lcTmpDesc..mQty WITH &lcTmpDesc..mQty +STR(&lcOrdLin..nQty&lcCount)
*!*	            ELSE
*!*	              REPLACE &lcOrdLin..Qty&lcCount WITH Qty&lcCount
*!*	              REPLACE &lcTmpDesc..mQty WITH &lcTmpDesc..mQty +STR(&lcOrdLin..Qty&lcCount)
*!*	            ENDIF
*!*	          ENDIF
*!*	        ENDIF
*!*	        lcUPCvalue= MLINE(&lcTmpDesc..mUPC,lnLineNo)
*!*	        lcSizeVal= MLINE(&lcTmpDesc..mSize,lnLineNo1)
*!*	        lcQtyVal= MLINE(&lcTmpDesc..mQty,lnLineNo2) 
*!*	        IF !EMPTY(lcUPCvalue)
*!*	          REPLACE &lcTmpDesc..mUPC WITH &lcTmpDesc..mUPC +CHR(13)
*!*	          llNewLine = .T.
*!*	        ELSE         
*!*	          llNewLine = .F.
*!*	        ENDIF 
*!*	        IF !EMPTY(lcSizeVal)
*!*	          REPLACE &lcTmpDesc..mSize WITH &lcTmpDesc..mSize +CHR(13)
*!*	          llNewLine1 = .T.
*!*	        ELSE         
*!*	          llNewLine1 = .F.
*!*	        ENDIF 
*!*	        IF !EMPTY(lcQtyVal)
*!*	          REPLACE &lcTmpDesc..mQty WITH &lcTmpDesc..mQty +CHR(13)
*!*	          llNewLine2 = .T.
*!*	        ELSE         
*!*	          llNewLine2 = .F.
*!*	        ENDIF         
*!*	      ENDFOR
*!*	    ENDIF



*!*	    *--SKU only Found
*!*	    IF !&lcTmpDesc..llStyUPC AND &lcTmpDesc..llStySKU
*!*	      llStyUPC=.T.
*!*	      FOR lnCount =1 to 8
*!*	        lnLineNo=IIF(llNewLine = .T.,lnLineNO+1,lnLineNO)
*!*	        lnLineNo1=IIF(llNewLine1 = .T.,lnLineNO1+1,lnLineNO1)        
*!*	        lnLineNo2=IIF(llNewLine2 = .T.,lnLineNO2+1,lnLineNO2)        
*!*	        lcCount = STR(lnCount,1)
*!*	        IF llRpPrTCon
*!*	          llChkQty = IIF(EMPTY(&lcTmpFile..nQty&lcCount),.F.,.T.)
*!*	        ELSE
*!*	          llChkQty = IIF(EMPTY(Qty&lcCount),.F.,.T.)
*!*	        ENDIF
*!*	        IF llChkQty
*!*	          REPLACE &lcTmpDesc..mUPC WITH &lcTmpDesc..mUPC + LANG_Soorcngm_StySku + PADR(&lcTmpDesc..SKU&lcCount ,16,' ')
*!*	          IF llRpSQty
*!*	            REPLACE &lcTmpDesc..mSize WITH &lcTmpDesc..mSize + PADR(&lcTmpDesc..Size&lcCount ,5,' ')
*!*	            IF llRpPrTCon
*!*	              REPLACE &lcOrdLin..nQty&lcCount WITH &lcTmpFile..nQty&lcCount
*!*	              REPLACE &lcTmpDesc..mQty WITH &lcTmpDesc..mQty +STR(&lcOrdLin..nQty&lcCount)
*!*	            ELSE
*!*	              REPLACE &lcOrdLin..Qty&lcCount WITH Qty&lcCount
*!*	              REPLACE &lcTmpDesc..mQty WITH &lcTmpDesc..mQty+STR(&lcOrdLin..Qty&lcCount)
*!*	            ENDIF
*!*	          ENDIF
*!*	        ENDIF
*!*	        lcUPCvalue= MLINE(&lcTmpDesc..mUPC,lnLineNo)
*!*	        lcSizeVal= MLINE(&lcTmpDesc..mSize,lnLineNo1)
*!*	        lcQtyVal= MLINE(&lcTmpDesc..mQty,lnLineNo2) 
*!*	        IF !EMPTY(lcUPCvalue)
*!*	          REPLACE &lcTmpDesc..mUPC WITH &lcTmpDesc..mUPC +CHR(13)
*!*	          llNewLine = .T.
*!*	        ELSE         
*!*	          llNewLine = .F.
*!*	        ENDIF 
*!*	        IF !EMPTY(lcSizeVal)
*!*	          REPLACE &lcTmpDesc..mSize WITH &lcTmpDesc..mSize +CHR(13)
*!*	          llNewLine1 = .T.
*!*	        ELSE         
*!*	          llNewLine1 = .F.
*!*	        ENDIF 
*!*	        IF !EMPTY(lcQtyVal)
*!*	          REPLACE &lcTmpDesc..mQty WITH &lcTmpDesc..mQty +CHR(13)
*!*	          llNewLine2 = .T.
*!*	        ELSE         
*!*	          llNewLine2 = .F.
*!*	        ENDIF         
*!*	      ENDFOR
*!*	    ENDIF

*!*	 
*!*	 

*!*	    *--No SKU No UPC print only Size
*!*	    IF !&lcTmpDesc..llStyUPC AND !&lcTmpDesc..llStySKU AND llRpSQty
*!*	      llStyUPC=.T.
*!*	      FOR lnCount =1 to 8
*!*	        lnLineNo=IIF(llNewLine = .T.,lnLineNO+1,lnLineNO)
*!*	        lnLineNo1=IIF(llNewLine1 = .T.,lnLineNO1+1,lnLineNO1)        
*!*	        lnLineNo2=IIF(llNewLine2 = .T.,lnLineNO2+1,lnLineNO2)        
*!*	        lcCount = STR(lnCount,1)
*!*	        IF llRpPrTCon
*!*	          llChkQty = IIF(EMPTY(&lcTmpFile..nQty&lcCount),.F.,.T.)
*!*	        ELSE
*!*	          llChkQty = IIF(EMPTY(Qty&lcCount),.F.,.T.)
*!*	        ENDIF
*!*	        IF llChkQty
*!*	          REPLACE &lcTmpDesc..mSize WITH &lcTmpDesc..mSize + PADR(&lcTmpDesc..Size&lcCount ,5,' ')
*!*	          IF llRpPrTCon
*!*	            REPLACE &lcOrdLin..nQty&lcCount WITH &lcTmpFile..nQty&lcCount
*!*	            REPLACE &lcTmpDesc..mQty WITH &lcTmpDesc..mQty+ STR(&lcOrdLin..nQty&lcCount)
*!*	          ELSE
*!*	            REPLACE &lcOrdLin..Qty&lcCount WITH  Qty&lcCount
*!*	            REPLACE &lcTmpDesc..mQty WITH &lcTmpDesc..mQty + STR(&lcOrdLin..Qty&lcCount)
*!*	          ENDIF
*!*	        ENDIF
*!*	        lcSizeVal= MLINE(&lcTmpDesc..mSize,lnLineNo1)
*!*	        lcQtyVal= MLINE(&lcTmpDesc..mQty,lnLineNo2) 
*!*	        IF !EMPTY(lcSizeVal)
*!*	          REPLACE &lcTmpDesc..mSize WITH &lcTmpDesc..mSize +CHR(13)
*!*	          llNewLine1 = .T.
*!*	        ELSE         
*!*	          llNewLine1 = .F.
*!*	        ENDIF 
*!*	        IF !EMPTY(lcQtyVal)
*!*	          REPLACE &lcTmpDesc..mQty WITH &lcTmpDesc..mQty +CHR(13)
*!*	          llNewLine2 = .T.
*!*	        ELSE         
*!*	          llNewLine2 = .F.
*!*	        ENDIF         
*!*	      ENDFOR
*!*	    ENDIF
    *: B610754,1 HES 06/22/2014 Let the size break down appear regardless the UPC and SKU  [END  
    
  ENDIF

ENDIF

*--To indicate whether the memo fields are empty or filled with data.
*HMA
*IIF (EMPTY(&lcTmpDesc..Mupc),&lcTmpDesc..llMUpcSku = .F.,&lcTmpDesc..llMUpcSku = .T.)
REPLACE &lcTmpDesc..llMUpcSku WITH IIF (EMPTY(&lcTmpDesc..Mupc),.T.,.F.)
*HMA
*-- End Of lfPrtPack

*!*************************************************************
*! Name      : lfPrntPack
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 08/02/2004
*! Purpose   : Function to print Pack_id without Line details.
*!*************************************************************
*! Calls     : lfGetPack()
*!*************************************************************
*! Passed Parameters  : None.
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example            : =lfPrntPack()
*!*************************************************************

*--This function to modify the way we get UPC and SKU

FUNCTION lfPrntPack

PRIVATE lcPck_id , lnTotlQty , lnPrice , lnSugRet , lnAvgAmt , lnTotAmnt
STORE 0 TO lnTotlQty , lnPrice , lnSugRet , lnAvgAmt , lnTotAmnt
PRIVATE llScanLop,lcPackUPC,lcPackSKU,lnAlias
STORE .F. TO llScanLop
STORE '' TO lcPackUPC,lcPackSKU

lnAlias = SELECT(0)

SELECT (lcTempOrd)
GO TOP  
lnRecTo = RECNO()
SELECT (lcTmpPack)
=SEEK(ORDHDR.cOrdType+lcOrder,lcTmpPack)
SCAN 
  lclStore=&lcTmpPack..Store
  SELECT (lcTempOrd)  
  SCAN FOR cOrdType+Order = ORDHDR.cOrdType+lcOrder .AND.  IIF(!llRpPrTCon,lclStore=Store,.T.)
    IF !EMPTY(PACK_ID)
      lcPack = lfGetPack()
      lcPck_id = SUBSTR(lcPack,10,16)
      lnCountr = 0
      lnLastRec = 0
      llCheckPck = .T.

      SCAN FOR PACK_ID=lcPck_id .AND. IIF(!llRpPrTCon,lclStore=Store,.T.) .AND.(cOrdType+Order = ORDHDR.cOrdType+lcOrder)
        IF lclStore<>Store AND llRpPrTCon
          llCheckPck = .T.
          lclStore = Store        
        ENDIF
        SCATTER MEMVAR MEMO
        IF llCheckPck = .T.
          IF llRpPrTCon
            llSeek = SEEK(cOrdType+Order+Pack_Id+cPkColor+cPckSize+cPkVersion,lcTmpPack)
          ELSE
            llSeek = SEEK(cOrdType+Order+PADR(ALLTRIM(Store),8,' ')+Pack_Id+cPkColor+cPckSize+cPkVersion,lcTmpPack)
          ENDIF
          IF llSeek 
            IF &lcTempOrd..lRange OR &lcTempOrd..lPckPrPiec OR &lcTempOrd..nPkSlPrice = 0
              REPLACE &lcTmpPack..nPackQty WITH &lcTmpPack..nPackQty + &lcTempOrd..TotQty
            ELSE
              REPLACE &lcTmpPack..nPackQty WITH &lcTmpPack..nPackQty + lfGetPkQty()
            ENDIF
            IF &lcTempOrd..lRange OR &lcTempOrd..lPckPrPiec OR &lcTempOrd..nPkSlPrice = 0
              REPLACE &lcTmpPack..nPackPrc WITH &lcTmpPack..nPackPrc +;
                                               (&lcTempOrd..TotQty*&lcTempOrd..price ) 
            ENDIF  
            REPLACE &lcTmpPack..nCount  WITH &lcTmpPack..nCount + 1,;
                    &lcTmpPack..nTotQty WITH &lcTmpPack..nTotQty + &lcTempOrd..TotQty
   
          ENDIF
          llCheckPck = .F.
        ELSE 
          REPLACE &lcTmpPack..lRange WITH &lcTempOrd..lRange ,;
                  &lcTmpPack..lPckPrPiec WITH &lcTempOrd..lPckPrPiec ,;
                  &lcTmpPack..nPkSlPrice WITH &lcTempOrd..nPkSlPrice

          IF llRpPrTCon
            llSeek = SEEK(cOrdtype+Order+Pack_Id+cPkColor+cPckSize+cPkVersion,lcTmpPack)
          ELSE
            llSeek = SEEK(cOrdType+Order+PADR(ALLTRIM(Store),8,' ')+Pack_Id+cPkColor+cPckSize+cPkVersion,lcTmpPack)
          ENDIF
          IF llSeek 
            IF &lcTempOrd..lRange OR &lcTempOrd..lPckPrPiec OR &lcTempOrd..nPkSlPrice = 0
              REPLACE &lcTmpPack..nPackPrc WITH &lcTmpPack..nPackPrc +;
                                                (&lcTempOrd..TotQty*&lcTempOrd..price ) 
              REPLACE &lcTmpPack..nPackQty WITH &lcTmpPack..nPackQty + &lcTempOrd..TotQty
            ENDIF
 
            
            REPLACE &lcTmpPack..nCount  WITH &lcTmpPack..nCount + 1,;
                    &lcTmpPack..nTotQty WITH &lcTmpPack..nTotQty + &lcTempOrd..TotQty

          ENDIF  
        ENDIF
        llScanLop = .T.
        lnCountr =RECNO()
        lnLastRec = RECNO()

      ENDSCAN
      IF llScanLop
        IF !llRpPrTCon
          IF BETWEEN(lnLastRec,1,RECCOUNT())
            GOTO lnLastRec 
          ENDIF
        ELSE
          IF EOF()
            GOTO lnCountr
          ELSE
            GOTO lnCountr+1
          ENDIF
        
        ENDIF  
      ENDIF
   
      llScanLop = .F.
    ENDIF

  ENDSCAN

ENDSCAN 

IF !llRpPrTCon 
*!*	  IF EOF()
*!*	*hma
*!*	*   SELECT (lcTempOrd)
*!*	*hma
*: B130206,1 AYM 11/28/2005 ..BEGIN
    SELECT (lcTempOrd)
*: B130206,1 AYM 11/28/2005 ..END
    IF EOF()
    GOTO lnRecTo 
    SCAN REST WHILE cOrdType+Order = ORDHDR.cOrdType+lcOrder AND lclStore=Store
    ENDSCAN
    SKIP-1
  ENDIF
ENDIF  

SELECT (lcTmpPack)
=SEEK(ORDHDR.cOrdType+lcOrder,lcTmpPack)

SCAN
 IF &lcTmpPack..lRange OR &lcTmpPack..lPckPrPiec OR &lcTmpPack..nPkSlPrice = 0
*--	AYM::   modififctaion  BEGIN
*!* REPLACE &lcTmpPack..nPackSlPrc WITH &lcTmpPack..nPackPrc / IIF(&lcTmpPack..nTotQty=0,1,&lcTmpPack..nTotQty)
    REPLACE &lcTmpPack..nPkSlPrice WITH &lcTmpPack..nPackPrc / IIF(&lcTmpPack..nTotQty=0,1,&lcTmpPack..nTotQty)
       REPLACE &lcTmpPack..nAmount WITH &lcTmpPack..nPackPrc
 ELSE
*!* REPLACE &lcTmpPack..nAmount WITH &lcTmpPack..nPackSlPrc * &lcTmpPack..nPackQty  
    REPLACE &lcTmpPack..nAmount WITH &lcTmpPack..nPkSlPrice * &lcTmpPack..nPackQty  
*--	AYM::   modififctaion  END  
 ENDIF
 lcPPrcPic=IIF(lPckPrPiec,LANG_Soorcngm_PriPiec,LANG_Soorcngm_PriPack)
 REPLACE &lcTmpPack..lcPrcPic WITH lcPPrcPic
ENDSCAN
LOCATE
SELECT(lnAlias)
SELECT (lcTempOrd)




*!***************************************************************************
*! Name      : lfPrPakCon
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 09/04/2004
*! Purpose   : Function to print field Pack,Style SKU,UPC,QTY.(Consolidated) 
*!***************************************************************************
*! Calls     : None.
*!***************************************************************************
*! Passed Parameters  : None.
*!***************************************************************************
*! Returns            : None.
*!***************************************************************************
*! Example            :  lfPrPakCon()
*!***************************************************************************

FUNCTION lfPrPakCon

IF llRpPrtCon

  SELECT(lcOrdLinSt)
  =SEEK(cOrdType+Order,lcOrdLinSt)
  SCAN 
    =SEEK(cOrdType+Order,lcTempOrd)
    IF !llRpPack
      IF !llConfig
        =SEEK(cOrdType+Order+STYLE,lcTmpFile)
      ELSE
        =SEEK(cOrdType+Order+STYLE+DYELOT,lcTmpFile)
      ENDIF 
    ELSE
      IF !llConfig
        =SEEK(cOrdType+Order+STYLE+Pack_Id +cPkColor+cPckSize+ cPkVersion,lcTmpFile)
      ELSE
        =SEEK(cOrdType+Order+STYLE+DYELOT+Pack_Id +cPkColor+cPckSize+ cPkVersion,lcTmpFile)
      ENDIF 
    ENDIF

    SELECT (lcTmpFile)

    IF llRpPack 
      IF !llConfig
        lcEVExp = "cOrdType+Order+STYLE+Pack_Id +cPkColor+cPckSize+ cPkVersion =&lcOrdLinSt..cOrdType+&lcOrdLinSt..Order+&lcOrdLinSt..Style+&lcOrdLinSt..Pack_Id +&lcOrdLinSt..cPkColor+&lcOrdLinSt..cPckSize+ &lcOrdLinSt..cPkVersion"
      ELSE
        lcEVExp = "cOrdType+Order+STYLE+DYELOT+Pack_Id +cPkColor+cPckSize+ cPkVersion =&lcOrdLinSt..cOrdType+&lcOrdLinSt..Order+&lcOrdLinSt..Style+&lcOrdLinSt..Dyelot+&lcOrdLinSt..Pack_Id +&lcOrdLinSt..cPkColor+&lcOrdLinSt..cPckSize+ &lcOrdLinSt..cPkVersion"
      ENDIF 
    ELSE  
      IF !llConfig
        lcEVExp = "cOrdType+Order+STYLE =&lcOrdLinSt..cOrdType+&lcOrdLinSt..Order+ &lcOrdLinSt..Style"
      ELSE
        lcEVExp = "cOrdType+Order+STYLE+DYELOT =&lcOrdLinSt..cOrdType+&lcOrdLinSt..Order+ &lcOrdLinSt..Style+&lcOrdLinSt..Dyelot"
      ENDIF 
    ENDIF
    SCAN REST WHILE &lcEVExp
      =lfPrtpack(Pack_id,'')
    ENDSCAN 
  ENDSCAN    
     
ENDIF   


