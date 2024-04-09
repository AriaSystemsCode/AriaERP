*:************************************************************************
*:
*: Procedure file: APINSTM.PRG
*:
*:         System: ARIA ADVANTAGE SERIES
*:         Module: Accounts Payable
*:         Author: Malak Hanna
*:      Copyright (c) 
*:  Last modified:  /  /
*:
*:  Procs & Fncts: lpShow
*:               : lfReadInv
*:               : lfwOldVals
*:               : lfwActBrow
*:               : lfvActBrow
*:               : lfvData_3
*:               : lfCheckInsta
*:               : lfvData_5
*:               : lfvData_8
*:               : lfvData_9
*:               : lfvData_10
*:               : lfvData_11
*:               : lfvBnkChk
*:               : lfvData_14
*:               : lfvType
*:               : lfvDuration
*:               : lfvVendor
*:               : lpSavScr
*:
*:  Documented      /  /
*:************************************************************************
*E300296,4 RENEE 10/23/95 Add currency exchange checks (multi currency)
*E500030,1 M.H   11/02/95 Change the Duration word in the screen to be Frequency.
*E300643,1  HS 04/15/97 1)Make some changes for we have change the function
*E300643,1              [gfCodDes].
*E300683,1 AHMED 06/04/97 Add screens directory path to the calling of SPRS
*E300692,1 ESSMAT 06/29/97. Change name and path of SYCACCOD, SYCFISHD, 
*E300692,1 					SYCFSPRD, SYCFSHLD
*E301077,80 IHB 03/03/1999 Enhance opening and closing files
*E300789,4 IHB  07/03/1999 Remove company ID from ACCOD, FISHD, FSPRD, FSHLD, CODES
*:************************************************************************

EXTERNAL ARRAY laData,laKeyField
PRIVATE laBankObjs 
DECLARE laKeyField [3,4],laInstType[1,2],laDuration[1,2], ;
        laPayMeth[1,2],laRemitTo[3,2],laWndObj[8,3],laBankObjs[3,3]


** lcCompany      Variable to hold Company
** lcPhone        Variable to hold Phone
** lcType         Say field for type    
** lcDuration     Say field for duration
** lcRemitTo      Say field for remit to 
** lcFactCode     Variable to hold factor code
** lcRmtComp      Remit to company field 
** lcRmtAddr1     Remit to address1 field 
** lcRmtAddr2     Remit to address2 field 
** lcRmtAddr3     Remit to address3 field 
** lcTermCode     Say field for terms
** lcInvRef       Variable to hold 
** lcDivision     Say field for divisions
** lcPayPrio      Variable to hold 
** lcPayMeth      Say field for payment method 
** lcOldVal       Old value of character fields
** lcSavExact     Variable to hold 
** lcPopCol       Variable to hold 
** lcFactCol      Variable to hold 
** lcPrompt       Variable to hold 
** lcEmptyCode    Variable to hold 
** lcTKeyCode     Key code logical name from dictionary
** lcCurrCode     Currency code of the invoice. && *E300296,4 
**
** lnTotAppPay    Total approved amount
** lnTerDiscD     Variable to hold 
** lnInvAmnt      Variable to hold 
** lnInvDisc      Variable to hold 
** lnOldVal       Old value of numeric fields
** lnTypeLen      Types popup width
** lnDurLen       Duration popup width   
**
** ldLastGen      Variable to hold 
** ldInvDate      Variable to hold 
** ldOldVal       Old value of date fields
** ldFisBgDt      Begin date of current fiscal year 
**
** llBrowse       Browse invisible buttons flag
** llValidData    Variable to hold 
 
laKeyField[1,1] = 'laData[1]'
laKeyField[1,2] = .F.
laKeyField[1,3] = 'TVENDINV'
laKeyField[1,4] = 1

laKeyField[2,1] = 'laData[2]'
laKeyField[2,2] =.F.
laKeyField[2,3] = 'TVENDINV'
laKeyField[2,4] = 2

laKeyField[3,1] = 'laData[3]'
laKeyField[3,2] =.T.
laKeyField[3,3] = 'TVENDINV'
laKeyField[3,4] = 3

STORE .F.       TO llBrowse    , llValidData , laDefProc[9]
STORE 'DISABLE' TO lcStatDt8   , lcStatDt9   , lcStatDt11   , lcStatDt12  ,;
                   lcStatDt13  , lcStatDt14
STORE {}        TO ldLastGen   , ldInvDate   , ldOldVal     , ldFisBgDt
STORE 0         TO lnTotAppPay , lnTerDiscD  , lnInvAmnt    , lnInvDisc   ,;
                   lnOldVal    , lnTypeLen   , lnDurLen
STORE ''        TO lcCompany   , lcPhone     , lcType       , lcDuration  ,;
                   lcRemitTo   , lcFactCode  , lcRmtComp    , lcRmtAddr1  ,;
                   lcRmtAddr2  , lcRmtAddr3  , lcTermCode   , lcInvRef    ,;
                   lcDivision  , lcPayPrio   , lcPayMeth    , lcOldVal    ,;
                   lcSavExact  , lcPopCol    , lcFactCol    , lcPrompt    ,;
                   lcEmptyCode , lcTKeyCode  , lcCurrCode

*E301077,80 IHB opend needed files only [start]
*-- llSYDFIELD, llFISHD : are used to identify whether 
*-- the files are used or not
STORE .F. TO llSYDFIELD,  llFISHD
*E301077,80 IHB [end]

IF !gfSetup()
  RETURN
ENDIF

*** Array laFields is formed as follows :
*** A row for every field to be browsed
*** Column 1 : the object name corresponding to the field
*** Column 2 : the physical field name

DECLARE laFields[3,3],laInvADt[1]

laFields[1,1] = 'laData[2]'         &&'lcVendCode'
laFields[1,2] = 'cVendCode'
laFields[1,3] = 'VENCODE'
laFields[2,1] = 'lcCompany'         && 'lcVenComp'
laFields[2,2] = 'cVenComp'
laFields[2,3] = 'VENCOMP'
laFields[3,1] = 'lcPhone'           &&'lcPhoneNo'
laFields[3,2] = 'cPhoneNo'
laFields[3,3] = 'VENPHONE'

IF !WEXIST(gcBaseWind)
  *E301077,80 IHB opend needed files only [start]
  *MAN Added PRIVATE
  PRIVATE llSYDFIELD
  llSYDFIELD = gfSysOpen(gcSYSHome+'SYDFIELD' ,'CFLD_NAME','SH')

  *-- another modification is required to open FISHD of 
  *-- the parent company, so change gcDataDir to lcDataDir
  *E300789,4 IHB Remove company ID [start]
  *MAN Added PRIVATE
  PRIVATE llOpCmp,lcDataDir,llFISHD
  llOpCmp   = gfSysOpen(gcSysHome+'SYCCOMP','Ccomp_id','SH')
  lcDataDir = ALLTRIM(IIF(SEEK(gcPrnt_Cmp, 'SYCCOMP'),gfGetDataDir(ALLT(SYCCOMP.cCom_DDir)), gcDataDir))
  llFISHD   = gfSysOpen(lcDataDir+'FISHD'    ,'COMPFYEAR','SH')
  IF llOpCmp
    =gfSysClose('SYCCOMP')
  ENDIF
  *E300789,4 IHB Remove company ID [end]
  *E301077,80 IHB [end]
  
  SCATTER FIELDS &lcScFields MEMO TO laData BLANK

  *E300643,1 Change this line for the changes we have made 
  *          to (gfCodDes) [Begin]
  *lcEmptyCode = gfCodDes(' ')   
  lcEmptyCode = gfCodDes(' ' , ' ')
  *E300643,1 Change this line [End]
  
  lcTKeyCode  = ALLTRIM(LOOKUP(SYDFIELD.cFld_Head, 'CVENDCODE',;
                        SYDFIELD.cFld_Name,'CFLD_NAME')) + '\' + ;
                ALLTRIM(LOOKUP(SYDFIELD.cFld_Head, 'CINVNO',;
                        SYDFIELD.cFld_NAme,'CFLD_NAME')) + ' : '
  lcTermCode  = lcEmptyCode
  lcDivision  = lcEmptyCode
  laData[1]   = 'I'     &&
  laData[14]  = lcEmptyAcc
  lcPrompt    = 'GL acct.'      
  lcFactCol   = IIF(_DOS,'DISABLE COLOR ,,,,,,,,,N+/N*','DISABLE COLOR ,,,,,,,,,RGB(192,192,192,192,192,192)')

  *E301077,80 IHB opend needed files only [start]
  IF USED('SYDFIELD') .AND. llSYDFIELD
    =gfSysClose('SYDFIELD')
  ENDIF
  *E301077,80 IHB [end]

  
  *** Prepare an array to hold bank objects to be used for
  *** global bank and checking accounts validations as follows :
  *** One row for every object, such that 
  *** row no. 1 holds bank object names,
  *** row no. 2 holds checking account object names
  *** row no. 3 holds the corresponding G/L account object names,
  *** Columns are ordered as follows :
  *** Column no. 1 : invisible button name for corresponding object
  *** Column no. 2 : object name (e.g. bank object name)
  *** Column no. 3 : object description name(if required)
  laBankObjs  = ' '
  laBankObjs[1,1] = 'ibBank'         && Bank code invisible button
  laBankObjs[1,2] = 'laData[12]'     && Bank code 
  laBankObjs[2,1] = 'ibChecks'       && Checking account invisible button
  laBankObjs[2,2] = 'laData[13]'     && Checking account 
  laBankObjs[3,1] = 'ibGlAcc'        && G/L account invisible button  
  laBankObjs[3,2] = 'laData[14]'     && G/L account

  *** Get the current fiscal year's start and end date
  *** to be used as defaults for the invoice dates from
  *** the fiscal calendar of the parent company.
  *E300692,1 CHANGE FILE NAME FROM SYCFISHD TO FISHD
  *SELECT SYCFISHD
  SELECT FISHD
  *E300692,1 end
  
  *E300789,4 IHB Adjust to fit the new structure [start]
  *IF SEEK(gcPrnt_Cmp)
  GO TOP
  IF !EOF()
    *E300789,4 IHB Remove company ID
    *LOCATE REST WHILE cComp_ID = gcPrnt_Cmp FOR  cFisYstat = 'C'
    LOCATE REST FOR  cFisYstat = 'C'
    *E300789,4 IHB [end]
    
    ldFisBgDt = IIF(FOUND(),dFisBgDat,{})
  ENDIF
  
  *E301077,80 IHB 03/03/1999 opend needed files only [start]
  IF USED('FISHD') .AND. llFISHD
    =gfSysClose('FISHD')
  ENDIF
  *E301077,80 IHB 03/03/1999 opend needed files only [end]
  
  *** Prepare Types array from SYDFIELD and get its maximum width
  lnTypeLen  = gfGetVld('cAutMBase',@laInstType)
  lcType     = laInstType[1,1]
 
  *** Prepare Duration array from SYDFIELD and get its maximum width
  lnDurLen   = gfGetVld('cAutFUnit',@laDuration)
  lcDuration = laDuration[1,1]   

  *** Prepare Remit to array from SYDFIELD and get its maximum width
  lnRemitLen = gfGetVld('cInvRemit',@laRemitTo)
  lcRemitTo  = laRemitTo[1,1]   
  
  *** Prepare Payment method array from SYDFIELD and get its maximum width
  =gfGetVld('cVenPMeth',@laPayMeth)
ELSE
  IF ! laScrMode[1]
    =lfReadInv()  
  ENDIF
ENDIF

SELECT APINVAHD
SET FILTER   TO
SET RELATION TO
SET FILTER   TO cAutMType = "I"          
SET RELATION TO laData[3] + laData[2] INTO APINVHDR ADDITIVE
SET RELATION TO laData[2]             INTO APVENDOR ADDITIVE

*E300683,1 Call *.SPR from screens directory
* DO APInstm.SPR 
DO (gcScrDir + gcWinAppl + '\APInstm.SPR')
*E300683,1 end          

SELECT APINVAHD
SET FILTER   TO 
SET RELATION TO

*!**************************************************************************
*!
*!      Procedure: lpShow
*!
*!**************************************************************************
*
PROCEDURE lpShow

laData[1] = 'I'        && recurring invoices only.

SELECT APINVAHD
IF RECNO() <= RECCOUNT()    && to refresh relations between files.
  GO RECNO()
ELSE
  GO TOP  
ENDIF

DO CASE
  CASE laScrMode[1]
    STORE  lcEmptyAcc TO laData[14]
    lcStatDt8   = 'DISABLE'
    lcStatDt9   = 'DISABLE'
    lcStatDt11  = 'DISABLE'    
    lcStatDt12  = 'DISABLE'
    lcStatDt13  = 'DISABLE'
    lcStatDt14  = 'DISABLE'
    lcPrompt    = 'GL acct.'
    lcCompany   = SPACE(30)          && Company
    lcPhone     = SPACE(16)          && phone
    lcType      = laInstType[1,1]
    lcDuration  = laDuration[1,1]   
    lcRemitTo   = laRemitTo[1,1]
    ldLastGen   = {}
    ldInvDate   = {}
    lcFactCode  = ''
    lcFactCol   = IIF(_DOS,'DISABLE COLOR ,,,,,,,,,N+/N*','DISABLE COLOR ,,,,,,,,,RGB(192,192,192,192,192,192)')
    lcRmtComp   = '' 
    lcRmtAddr1  = '' 
    lcRmtAddr2  = '' 
    lcRmtAddr3  = '' 
    lcInvRef    = ''
    lcPayPrio   = ''
    lcPayMeth   = ''
    lnTotAppPay = 0
    lnTerDiscD  = 0 
    lnInvAmnt   = 0 
    lnInvDisc   = 0 
    lcTermCode  = lcEmptyCode
    lcDivision  = lcEmptyCode
    lcCurrCode  = ''     && *E300296,4 

  CASE laScrMode[2] .OR. laScrMode[3]

    *** Type
    lnElemNum = ASCAN(laInstType,laData[4])
    lcType    = IIF(lnElemNum > 0, laInstType[ASUBSCRIPT(laInstType,lnElemNum ,1),1]," ")    

    *** Duration
    lnElemNum  = ASCAN(laDuration,laData[6])
    lcDuration = IIF(lnElemNum > 0, laDuration[ASUBSCRIPT(laDuration,lnElemNum ,1),1]," ")    

    *** AP account 
    IF EMPTY(laData[14])
      laData[14] = lcEmptyAcc
    ENDIF  

    lcObjectSt = IIF(laScrMode[2],'DISABLE','ENABLE')
    lcStatDt8  = 'DISABLE' 
    lcStatDt9  = lcObjectSt
    lcStatDt12 = lcObjectSt
    lcStatDt13 = lcObjectSt
    lcStatDt14 = lcObjectSt
    SHOW GET laData[8] DISABLE   && in both cases view and edit modes.

    IF laScrMode[3] .AND.  ;
       (APINVHDR.nInvAmnt  - APINVHDR.NINVPAID - ; 
        APINVHDR.NINVDISTK - APINVHDR.NINVADJ   ) = 0
      *** Message :" Invoice ð for vendor ð is fully paid."
      ***                      <   OK   >
      =gfModalGen("TRM04075B00000","DIALOG",ALLTRIM(laData[3])+'|'+ALLTRIM(laData[2]))
    ENDIF
    
  CASE laScrMode[4]
    laData[4]  = 'A'
    laData[5]  = 1
    laData[6]  = 'P'
    laData[8]  = ldFisBgDt   && begin date of current fiscal year
    laData[9]  = ldFisBgDt   && begin date of current fiscal year
    ladata[13] = SPACE(12)
    ladata[14] = lcEmptyAcc
    lcStatDt8  = 'ENABLE' 
    lcStatDt9  = 'DISABLE'
    lcStatDt12 = 'ENABLE' 
    lcStatDt13 = 'ENABLE' 
    lcStatDt14 = 'ENABLE' 

    SHOW GET laData[9] DISABLE   
  
    *E301077,80 IHB 03/03/1999 opend needed files only [start]
    *MAN
    *llAPSETUP  = gfSysOpen(gcDataDir+'APSETUP' ,' ','SH')
    *llAPDIV    = gfSysOpen(gcDataDir+'APDIV' ,'DIVISION','SH')
    = gfOpenFile(gcDataDir+'APSETUP' ,' ','SH')
    = gfOpenFile(gcDataDir+'APDIV' ,'DIVISION','SH')
    
    *E301077,80 IHB 03/03/1999 opend needed files only [end]
  
    IF APINVHDR.cVenPMeth = 'H'
      IF EMPTY(APVENDOR.cCashAcct)
        IF  !EMPTY(APVENDOR.cDivision)       .AND. ;
            SEEK(APVENDOR.cDivision,'APDIV') .AND. ;
            !EMPTY(APDIV.cCashAcct)
          laData[14] = APDIV.cCashAcct
        ELSE
          laData[14] = APSETUP.cCashAcct
        ENDIF
      ELSE
        laData[14] = APVENDOR.cCashAcct
      ENDIF
    ELSE
      IF EMPTY(APVENDOR.cBnkCode)
        IF !EMPTY(APINVHDR.cDivision)       .AND. ;
          SEEK(APINVHDR.cDivision,'APDIV')  .AND. ;
          !EMPTY(APDIV.cBnkCode)
          laData[12] = APDIV.cBnkCode
          laData[13] = APDIV.cChkAcct
        ELSE
          laData[12] = APSETUP.cBnkCode
          laData[13] = APSETUP.cChkAcct
        ENDIF
      ELSE
        laData[12] = APVENDOR.cBnkCode
        laData[13] = APVENDOR.cChkAcct
      ENDIF

      IF SEEK(laData[12]+ladata[13],'APCHECKS')
        *E300296,4 Check if the checking account has the same
        *E300296,4 currency as that of the invoice, if not,
        *E300296,4 get the first checking account using
        *E300296,4 the invoice currency, if any is found,
        *E300296,4 or clear al fields if none is found.
        *laData[14] = APCHECKS.cChkGlAcc     
        IF APCHECKS.cCurrCode = APINVHDR.cCurrCode
          laData[14] = APCHECKS.cChkGlAcc     
        ELSE
          SELECT APCHECKS
          LOCATE REST WHILE cBnkCode = laData[12];
                 FOR cCurrCode = APINVHDR.cCurrCode
          IF FOUND()
            laData[13] = APCHECKS.cChkAcct
            laData[14] = APCHECKS.cChkGlAcc     
          ELSE
            ladata[12] = SPACE(8)
            ladata[13] = SPACE(12)
            ladata[14] = lcEmptyAcc
            lcStatDt13 = 'DISABLE'    
            lcStatDt14 = 'DISABLE'      
          ENDIF
        ENDIF
        *E300296,4 end.           
      ELSE
        ladata[12] = SPACE(8)
        ladata[13] = SPACE(12)
        ladata[14] = lcEmptyAcc
        lcStatDt13 = 'DISABLE'    
        lcStatDt14 = 'DISABLE'      
        *E300296,4 show objects after ENDIF
        *SHOW GET laData[13] DISABLE
        *SHOW GET laData[14] DISABLE
        *SHOW GET ibChecks   DISABLE
        *SHOW GET ibGlAcc    DISABLE
      ENDIF  
      *E300296,4 show objects 
      SHOW GET laData[12]
      SHOW GET laData[13] &lcStatDt14
      SHOW GET laData[14] &lcStatDt14
      SHOW GET ibChecks   &lcStatDt14
      SHOW GET ibGlAcc    &lcStatDt14
      *E300296,4 end.
    ENDIF

    *E301077,80 IHB 03/03/1999 opend needed files only [start]
    *IF USED('APSETUP') .AND. llAPSETUP
    *  =gfCloseFile('APSETUP')
    *ENDIF
    *IF USED('APDIV') .AND. llAPDIV
    *  =gfCloseFile('APDIV')
    *ENDIF
    *E301077,80 IHB [end]
    
ENDCASE

IF ! laScrMode[1]
  =lfReadInv()  
ENDIF

IF laScrMode[3] .OR. laScrMode[4]
  IF EMPTY(APVENDOR.cVen1099T)
    lcStatDt11 = 'DISABLE'
    SHOW GET laData[11] DISABLE   
  ELSE
    lcStatDt11 = 'ENABLE'
    SHOW GET laData[11] ENABLE
  ENDIF
ELSE  
  lcStatDt11 = 'DISABLE'
  SHOW GET laData[11] DISABLE   
ENDIF

lcPopCol  = IIF(laScrMode[1] .OR. laScrMode[2], SCHEME(1,10),SCHEME(1,2))
SHOW GET lcFactCode &lcFactCol
SELECT APINVAHD

*!**************************************************************************
*!
*!        Function : lfReadInv
*!
*!**************************************************************************
FUNCTION lfReadInv

ldLastGen    = APINVAHD.dAutLGen
ldInvDate    = APINVHDR.dInvDate
lnInvAmnt    = APINVHDR.nInvAmnt
lnInvDisc    = APINVHDR.nInvDisOf
lcInvRef     = APINVHDR.cInvRef
lcPayPrio    = APINVHDR.cVenPrior
lnTotAppPay  = APINVHDR.nInvPaid + APINVHDR.nInvAdj
lnTerDiscD   = APINVHDR.nInvDisTk
lcRmtComp    = APINVHDR.COUTCOMP
lcRmtAddr1   = APINVHDR.COUTADDR1
lcRmtAddr2   = APINVHDR.COUTADDR2
lcRmtAddr3   = APINVHDR.COUTADDR3
lcFactCode   = APINVHDR.CFACCODE
*E300296,4 Currency code is that of the invoice
lcCurrCode   = APINVHDR.cCurrCode
*E300296,4 end.

IF APINVHDR.cInvRemit = 'F'
  lcFactCol= IIF(_DOS,'ENABLE  COLOR SCHEME(1)','ENABLE  COLOR ,RGB(,,,192,192,192),,,,,,,,RGB(100,100,100,192,192,192)')
ELSE
  lcFactCol= IIF(_DOS,'DISABLE COLOR ,,,,,,,,,N+/N*','DISABLE COLOR ,,,,,,,,,RGB(192,192,192,192,192,192)')
ENDIF

IF APINVHDR.cVenPMeth = 'H'
  lcStatDt12   =     'DISABLE'
  SHOW GET ibBank     DISABLE
  SHOW GET laData[12] DISABLE
  lcStatDt13   =     'DISABLE'    
  SHOW GET ibChecks   DISABLE
  SHOW GET laData[13] DISABLE
  lcPrompt = 'Cash acc'
ELSE
  lcPrompt = 'GL acct.'
ENDIF

*E300643,1 Change this lines for the changes we have made to (gfCodDes) [Begin]
*lcTermCode = gfCodDes(APINVHDR.cTermCode)   && Term description
*lcDivision = gfCodDes(APINVHDR.cDivision)   && Division description
lcTermCode = gfCodDes(APINVHDR.cTermCode , 'CTERMCODE')   && Term description
lcDivision = gfCodDes(APINVHDR.cDivision , 'CDIVISION')   && Division description
*E300643,1 Change this lines for the changes we have made to (gfCodDes) [End]
  
*** Payment method
lnElemNum = ASCAN(laPayMeth,APINVHDR.cVenPMeth)
lcPayMeth = IIF(lnElemNum > 0, laPayMeth[ASUBSCRIPT(laPayMeth,lnElemNum ,1),1]," ")    

*** Remit to objects *
lnElemNum = ASCAN(laRemitTo,APINVHDR.cInvRemit)
lcRemitTo = IIF(lnElemNum > 0, laRemitTo[ASUBSCRIPT(laRemitTo,lnElemNum ,1),1]," ")    

SHOW GET ldLastGen
SHOW GET ldInvDate   
SHOW GET lnInvAmnt   
SHOW GET lnInvDisc   
SHOW GET lcInvRef    
SHOW GET lcPayPrio   
SHOW GET lnTotAppPay 
SHOW GET lnTerDiscD  
SHOW GET lcRmtComp   
SHOW GET lcRmtAddr1  
SHOW GET lcRmtAddr2  
SHOW GET lcRmtAddr3  
SHOW GET lcFactCode  
SHOW GET lcTermCode   
SHOW GET lcDivision   
SHOW GET lcPayMeth  
SHOW GET lcRemitTo   
*E300296,4 Show lcCurrCode
SHOW GET lcCurrCode 
*E300296,4 end.


*!**************************************************************************
*!
*!        Function : lfwOldVals
*!
*!**************************************************************************
FUNCTION lfwOldVals

PARAMETERS lcOldObjNm
&lcOldObjNm = EVALUATE(SYS(18))


*!**************************************************************************
*!
*!      Function: lfvData_3
*!
*!**************************************************************************
*
FUNCTION lfvData_3

SELECT APINVHDR
SET ORDER TO VENDINV
lcSavExact = SET('EXACT')
SET EXACT ON
DIMENSION  laTemp[2]
laTemp     = ''
lcVendor   = laData[2]
lcSavTitle = lcFile_Ttl
lcSavBrFld = lcBrFields

=lfGetBrF(@lcFile_Ttl, @lcBrFields, 'APINVHDR')

IF llBrowse .OR. !EMPTY(laData[3]) .AND. LASTKEY() = 13
  IF llBrowse .OR. ATC("?",laData[3]) > 0 .OR. EMPTY(laData[3]) .OR. EMPTY(lcVendor)
          
    IF EMPTY(lcVendor)
      =gfBrows([FOR cInvStat <> 'V'],'CVENDCODE,CINVNO','laTemp')
    ELSE
      SET EXACT &lcSavExact
      IF SEEK(lcVendor)
        =gfBrows([FOR CVENDCODE = lcVendor AND cInvStat <> 'V'],'CVENDCODE,CINVNO','laTemp')
      ELSE
        *** Message : " No ð found for vendor ð."
        ***           "                  ® Ok ¯              "
        =gfModalGen("TRM04042B00000","DIALOG","invoices|"+ALLTRIM(lcVendor))
      ENDIF  
    ENDIF  
    
    IF EMPTY(laTemp[2])
      laData[3] = lcOldVal
      SHOW GET laData[3]
      _CUROBJ = OBJNUM(laData[3])
    ELSE
      laData[2]    = laTemp[1]
      laData[3]    = laTemp[2]
      SHOW GET laData[2]      
      SHOW GET laData[3]
      =lfCheckInsta()
    ENDIF
  ELSE
    IF SEEK(lcVendor+laData[3],'APINVHDR') .AND. APINVHDR.cInvStat <> 'V'
      =lfCheckInsta()
    ELSE  
      lnClosRec = RECNO(0)
      IF BETWEEN(lnClosRec,1,RECCOUNT('APINVHDR'))
        GO lnClosRec
      ELSE
        GO TOP
      ENDIF
      *** Message :" ð is not found in the data file."
      ***        < Browse >   < Reenter >
      lnOption = gfModalGen('QRM00001B00014','Dialog',;
                             lcTKeyCode + ALLTRIM(lcVendor)+' '+ALLTRIM(laData[3]))
      DO CASE
        CASE lnOption = 1             && Browse
          SET EXACT &lcSavExact
          IF SEEK(lcVendor)
            =gfBrows([FOR CVENDCODE = lcVendor AND cInvStat <> 'V'],'CVENDCODE,CINVNO','laTemp')
    
            IF EMPTY(laTemp[1])
              laData[3] = lcOldVal
              SHOW GET laData[3]
              _CUROBJ = OBJNUM(laData[3])
            ELSE
              laData[2]    = laTemp[1]
              laData[3]    = laTemp[2]
              SHOW GET laData[2]      
              SHOW GET laData[3]
              =lfCheckInsta()
            ENDIF
          ELSE
            *** Message : " No ð for vendor ð"
            ***           "          ® Ok ¯              "
            =gfModalGen("TRM04042B00000","DIALOG","invoices|"+ALLTRIM(lcVendor))
            laData[3] = lcOldVal
            SHOW GET laData[3]
            _CUROBJ = OBJNUM(laData[3])
          ENDIF  
        
        CASE lnOption = 2       && Reenter
          laData[3] = lcOldVal
          SHOW GET laData[3]
          SHOW GET ibInvoice ENABLE
          _CUROBJ = OBJNUM(laData[3])
      ENDCASE
    ENDIF
  ENDIF
ENDIF

lcFile_Ttl = lcSavTitle 
lcBrFields = lcSavBrFld 
SET EXACT &lcSavExact
llBrowse = .F.
SET ORDER TO INVVEND IN APINVHDR
SELECT APINVAHD

*!**************************************************************************
*!
*!        Function : lfCheckInsta
*!
*!**************************************************************************
*
FUNCTION lfCheckInsta

IF APINVHDR.cVenPMeth = 'C'
  *** Message :" You cannot create an installment record for 
  ***            invoices paid by credit card."
  ***                      <   OK   >
  =gfModalGen('QRM04079B00000','Dialog')
  laData[3] = SPACE(12)
  _CUROBJ = OBJNUM(laData[3])
  RETURN
ENDIF

IF APINVHDR.nInvAmnt < 0
  *** Message :" You cannot create an installment record for debit memos."
  ***                      <   OK   >
  =gfModalGen('QRM04081B00000','Dialog')
  laData[3] = SPACE(12)
  _CUROBJ = OBJNUM(laData[3])
  RETURN
ENDIF


SELECT APINVAHD

IF SEEK('I'+laData[2]+laData[3])
  SCATTER FIELDS &lcScFields MEMO TO laData
  laScrMode    = .F.
  laScrMode[2] = .T.
  SET EXACT &lcSavExact
  SET ORDER TO INVVEND IN APINVHDR
  SHOW GETS
ELSE
  lcSavTit2 = lcFile_Ttl
  lcSavBrF2 = lcBrFields
  DIMENSION laTemp2[1]
  laTemp2   = ''

  *** Message :" There is no installment record for this invoice."
  ***          < Browse > < Add > < Reenter >
  lnOption = gfModalGen('QRM04117B00001','Dialog')
    DO CASE
      CASE lnOption = 1
        =lfGetBrF(@lcFile_Ttl, @lcBrFields, 'APINVAHD')
        =gfBrows([FOR cAutMType+CVENDCODE = "I"+lcVendor],'CINVNO','laTemp2')
        IF EMPTY(laTemp2[1])
           laData[3] = SPACE(12)
           _CUROBJ = OBJNUM(laData[3])
        ELSE
          SCATTER FIELDS &lcScFields MEMO TO laData
          laScrMode    = .F.
          laScrMode[2] = .T.
          SET EXACT &lcSavExact
          SET ORDER TO INVVEND IN APINVHDR
          SHOW GETS
        ENDIF        

      CASE lnOption  = 2
        laScrMode    = .F.
        laScrMode[4] = .T.
        SET EXACT &lcSavExact
        SET ORDER TO INVVEND IN APINVHDR
        SHOW GETS

      CASE lnOption = 3
        laData[3] = SPACE(12)
        _CUROBJ = OBJNUM(laData[3])
    ENDCASE  
 
  lcFile_Ttl = lcSavTit2
  lcBrFields = lcSavBrF2 
ENDIF  

SELECT APINVHDR

*!**************************************************************************
*!
*!        Function : lfvData_5
*!
*!**************************************************************************
*  Valid function for laData[5] 
FUNCTION lfvData_5

IF laData[5] < 1
  *** Message : "    ð should be greater than ð.  "  
  ***                      <   OK   >
  =gfModalGen("TRM04072B00000","DIALOG",'The duration|zero')
  laData[5] = lnOldVal
  _CUROBJ   = _CUROBJ
  RETURN
ENDIF

*!**************************************************************************
*!
*!      Function: lfvData_8
*!
*!**************************************************************************
*
FUNCTION lfvData_8

IF laData[8] = ldOldVal 
  RETURN
ENDIF

IF EMPTY(laData[8])
  *** Message : " You have to enter the ð."
  ***                      <   OK   >
  =gfModalGen("TRM04066B00000","DIALOG",'first installment date')
  laData[8] = ldOldVal
  _CUROBJ   = _CUROBJ
ELSE    
  IF ! lfvDtMsg(gcPrnt_Cmp)
    laData[8] = ldOldVal
    _CUROBJ   = _CUROBJ
  ENDIF
ENDIF

laData[9]  = laData[8]
SHOW GET laData[9]

*!**************************************************************************
*!
*!      Function: lfvData_9
*!
*!**************************************************************************
*
FUNCTION lfvData_9

IF laData[9] = ldOldVal 
  RETURN
ENDIF

IF EMPTY(laData[9])
  *** Message : " You have to enter the ð."
  ***                      <   OK   >
  =gfModalGen("TRM04066B00000","DIALOG",'next installment date')
  laData[9] = ldOldVal
  _CUROBJ   = _CUROBJ
ELSE  
  IF laData[9] < laData[8]
    *** Message :" The ð date cannot be less than the ð date."
    ***                      <   OK   >
    =gfModalGen("TRM04028B00000","DIALOG",'next installment|start installment')
    laData[9] = ldOldVal
    _CUROBJ   = _CUROBJ
  ELSE
    IF ! lfvDtMsg(gcPrnt_Cmp)
      laData[9] = ldOldVal
      _CUROBJ   = _CUROBJ
    ENDIF
  ENDIF  
ENDIF

*!**************************************************************************
*!
*!      Function: lfvData_10
*!
*!**************************************************************************
*
FUNCTION lfvData_10

llValidData = .T.      

IF laData[10] < 0
  *** Message : "   ð should be greater than ð.   "  
  ***                      <   OK   >
  =gfModalGen("TRM04072B00000","DIALOG",;
  'Installment '+IIF(laData[4] = 'P','percent','amount')+'|zero')
  laData[10]  = lnOldVal
  _CUROBJ     = _CUROBJ
  llValidData = .F.
  RETURN
ENDIF

IF laData[4] = 'P'
  IF laData[10] > 100 
    *** Message :  "The installment percent cannot be greater than 100."
    ***                      <   OK   >
    *** restore the old value of the field
    =gfModalGen("TRM04119B00000","DIALOG")
    laData[10]  = lnOldVal
    _CUROBJ     = _CUROBJ
    llValidData = .F.    
  ELSE
    IF ( laData[10] * APINVHDR.nInvAmnt / 100 ) >  ;
       ( APINVHDR.nInvAmnt - (APINVHDR.NINVDISTK + ; 
                              APINVHDR.NINVPAID + ;
                              APINVHDR.NINVADJ ))
      *** Message :"ð amount cannot be greater than the open ð amount."
      ***                      <   OK   >
      *** restore the old value of the field
      =gfModalGen("TRM04015B00000","DIALOG",'The installment|invoice')
      laData[10]  = lnOldVal
      _CUROBJ     = _CUROBJ
      llValidData = .F.      
    ENDIF
  ENDIF
ELSE
  IF laData[10] > ( APINVHDR.nInvAmnt - (APINVHDR.NINVDISTK + ; 
                    APINVHDR.NINVPAID + ;
                    APINVHDR.NINVADJ  ))
    *** Message :"ð amount cannot be greater than the open ð amount."
    ***                      <   OK   >
    *** restore the old value of the field
    =gfModalGen("TRM04015B00000","DIALOG",'The installment|invoice')
    laData[10]  = lnOldVal
    _CUROBJ     = _CUROBJ
    llValidData = .F.          
  ENDIF    
ENDIF

*!**************************************************************************
*!
*!      Function: lfvData_11
*!
*!**************************************************************************
*
FUNCTION lfvData_11

llValidData = .T.

IF laData[11] < 0
  *** Message : "   ð should be greater than ð.   "  
  ***                      <   OK   >
  =gfModalGen("TRM04072B00000","DIALOG",'1099 Amount|zero')
  laData[11]  = lnOldVal
  _CUROBJ     = _CUROBJ
  llValidData = .F.
  RETURN
ENDIF

IF laData[11] > laData[10]
  *** Message :"ð amount cannot be greater than the open ð amount."
  ***                      <   OK   >
  *** restore the old value of the field
  =gfModalGen("TRM04015B00000","DIALOG",'The 1099|installment')
  laData[11]  = lnOldVal
  _CUROBJ     = _CUROBJ
  llValidData = .F.  
ENDIF  

*!**************************************************************************
*!
*!      Function: lfvBnkChk
*!
*!**************************************************************************
* Valid function for get fields laData[12] (bank code), and laData[13] 
* (checking account code)
*
FUNCTION lfvBnkChk
PRIVATE lcFltrCond, lcFltrMsg
IF EVALUATE(SYS(18)) <> lcOldVal
  =gfUpdate()
ENDIF
*E300296,4 Add parameters to lfBnkChk to restrict bank\checking account
*E300296,4 selection to those using the same currency as that of the 
*E300296,4 invoice.
*IF !lfBnkChk(@laBankObjs, lcOldVal, @llBrowse, @lcStatDt13)
*lcFltrCond = [cCurrCode = '] + APINVHDR.cCurrCode + [']
*lcFltrMsg  = ['INM04155B00000','DIALOG',']+ALLTRIM(APINVHDR.cCurrCode) + ;
              [|'+ ALLTRIM(APBANKS.cBnkCode) ]   

*IF !lfBnkChk(@laBankObjs, lcOldVal, @llBrowse, @lcStatDt13,;
             '', '', @lcFltrCond, @lcFltrMsg)

IF !lfBnkChk(@laBankObjs, lcOldVal, @llBrowse, @lcStatDt13,;
             '', '', APINVHDR.cCurrCode)             
*E300296,4 end.
  lcObjectSt = 'DISABLE'
  lcStatDt14 = 'DISABLE'
  RETURN 1
ELSE
  lcObjectSt = 'ENABLE'
  lcStatDt14 = 'ENABLE'        
ENDIF  

*!**************************************************************************
*!
*!      Function: lfvData_14
*!
*!**************************************************************************
*
FUNCTION lfvData_14

lcDummy = ''
IF EMPTY(STRTRAN(laData[14],'-',' ')) .OR. ! lfApAcs(@lcDummy,llBrowse)
  laData[14] = lcOldVal
ENDIF
  
SHOW GET laData[14]
llBrowse = .F.

*!**************************************************************************
*!
*!      Function: lfvType
*!
*!**************************************************************************
*
FUNCTION lfvType

IF _DOS
  laData[4] = gfActPop(2,11,6,14+lnTypeLen,'laInstType',2,1,@lcType)
ELSE
  lcSavExact = SET('EXACT')
  SET EXACT ON
  lnElemNum = ASCAN(laInstType,lcType)
  laData[4] = IIF(lnElemNum > 0, laInstType[ASUBSCRIPT(laInstType,lnElemNum ,1),2]," ")
  SET EXACT &lcSavExact
ENDIF  

=lfRefresh()

*!**************************************************************************
*!
*!        Function : lfvDuration
*!
*!**************************************************************************
*  Valid function for Duration popup
*
FUNCTION lfvDuration

IF _DOS 
  laData[6] = gfActPop(2,37,6,40+lnDurLen,'laDuration',2,1,@lcDuration)
  =lfRefresh()
ELSE
  lcSavExact = SET('EXACT')
  SET EXACT ON
  lnElemNum = ASCAN(laDuration,lcDuration)
  laData[6] = IIF(lnElemNum > 0, laDuration[ASUBSCRIPT(laDuration,lnElemNum ,1),2]," ")    
  SET EXACT &lcSavExact
ENDIF                          

*!**************************************************************************
*!
*!        Function : lfvVendor
*!
*!**************************************************************************
FUNCTION lfvVendor

IF lfSekVnd('APVENDOR', lcOldVal, llBrowse, .T.)
  _CUROBJ = OBJNUM(laData[3])
ENDIF
llBrowse     = .F.

*!**************************************************************************
*!
*!      Procedure: lpSavScr
*!
*!**************************************************************************
*
PROCEDURE lpSavScr

GO RECNO('APINVHDR') IN APINVHDR    && 
GO RECNO('APVENDOR') IN APVENDOR    &&

lnOldVal    = laData[10]
llValidData = .F.

=lfvData_10()               && ReValidate laData[10]
IF ! llValidData
  llcSave = .F.  
  _CUROBJ = OBJNUM(laData[10])
  =lfReadInv()  
  RETURN
ENDIF  

lnOldVal    = laData[11]
llValidData = .F.

=lfvData_11()               && ReValidate laData[11]
IF ! llValidData
  llcSave = .F.  
  _CUROBJ = OBJNUM(laData[11])
  =lfReadInv()  
  RETURN
ENDIF  

IF APINVHDR.cVenPMeth = 'H' AND EMPTY(STRTRAN(STRTRAN(laData[14],'-'),'0'))
  *** Message :  "You have to enter the ð."
  ***                      <   OK   >
  =gfModalGen("TRM04066B00000","DIALOG",'cash account')
  llcSave = .F.  
  _CUROBJ = OBJNUM(laData[14])
  =lfReadInv()  
  RETURN
ENDIF

IF APINVHDR.cVenPMeth <> 'H' 
  *** If the bank code is empty, present the following message 
  *** and return to the bank code field
  IF EMPTY(laData[12])
    *** Message : "   You have to enter the ð.  "
    ***                 ® OK  ¯
    =gfModalGen("TRM04066B00000","DIALOG","the bank code")
    llcSave = .F.  
    _CUROBJ = OBJNUM(laData[12])
    =lfReadInv()  
    RETURN 
  ENDIF

  IF !SEEK(laData[12],'APBANKS')  
    *** Message :  " ð not found."
    ***                      <   OK   >
    =gfModalGen("TRM0400200000","DIALOG",'Bank code ' + ALLTRIM(laData[12]))
    llcSave = .F.  
    _CUROBJ = OBJNUM(laData[12])
    =lfReadInv()  
    RETURN
  ENDIF 

  IF !SEEK(laData[12]+ladata[13],'APCHECKS')  
    *** Message :  "ð account not valid."
    ***                      <   OK   >
    =gfModalGen("TRM04077B00000","DIALOG",'Checking')
    llcSave = .F.  
    _CUROBJ = OBJNUM(laData[13])
    =lfReadInv()  
    RETURN
  ENDIF

  IF EMPTY(STRTRAN(laData[14],'-',' '))
    *** Message :" You have to enter the ð."
    ***                      <   OK   >
    =gfModalGen("INM04066B00000","DIALOG",'G/L account')
    llcSave = .F.  
    _CUROBJ = OBJNUM(laData[14])
    =lfReadInv()  
    RETURN 
  ELSE
    IF llApGlLink .AND. ! SEEK(ALLTRIM(laData[14]),'lcLinkChar')
      *** Message :" ð not found."
      ***                      <   OK   >
      =gfModalGen("INM04002B00000","DIALOG","G/L account|"+ALLTRIM(laData[14]))
      llcSave = .F.  
      _CUROBJ = OBJNUM(laData[14])
      =lfReadInv()  
      RETURN 
    ENDIF
  ENDIF  
ENDIF

SELECT APINVAHD
IF laScrMode[4]     
  APPEND BLANK
ENDIF

GATHER FROM laData FIELDS &lcScFields MEMO
=gfAdd_Info('APINVAHD')
