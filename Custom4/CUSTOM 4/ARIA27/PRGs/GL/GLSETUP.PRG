*:************************************************************************
*:
*: Procedure file: GLSETUP.PRG
*:
*:         System: ARIA BUSINESS SYSTEM
*:         Module: General Ledger
*:         Author: Mohamed Hassan Mohamed 
*:      Copyright (c) 
*:  Last modified: 12/05/1993
*:
*:  Procs & Fncts: lpShow
*:               : lfwData_1
*:               : lfwData_2
*:               : lfvData_1
*:               : lfvData_2
*:               : lfvData_14
*:               : lfvData_15
*:               : lfBuildPic
*:               : lfvOk
*:               : lfvCancel
*:               
*:      Documented  /  /   at   :  
*:************************************************************************
*B600387,1 Reham On 06/07/95
*B600387,1 Remark the previous validations of the batch posting window 
*B600387,1 that was related to the batch posting control & make its range
*B600387,1 within 1-13 & add new when function to retrive the old value
*B600387,1 if the value entered is wrong.
*B601019,1 RENEE 04/01/96. Spellling mustake in Exch Diff account in 
*B601019,1                 GLSETUP screen, WINDOWS.
*E300683,5 AHMED 06/10/97 Add prgs directory path to the calling of programs
*E300692,1 ESSMAT 06/30/97. Change name and path of SYCACCOD, SYCFISHD, 
*E300692,1 					SYCFSPRD, SYCFSHLD
*E300789,7 Walid (WAM) 03/03/99 Make the required modifications due to removing
*E300789,7 ccomp_id field .
*B802198,1 Walid (WAM) 05/05/99 Fix (Retained earnings & Suspense) accounts
*          Are disable in edit mode .
*:************************************************************************
*
PARAMETERS llFromSM,lcCompId,lcCompDir,lcDumii

EXTERNAL ARRAY laData,laKeyField

DECLARE laKeyField [1,4],laTDatePer[2,2],laSegSize[1,3],laObjdisp[6]

laTDatePer[1,1]='Period'
laTDatePer[1,2]='P'
laTDatePer[2,1]='Dates'
laTDatePer[2,2]='D'

laCtrStat = 'DISABLE' && To disable the browse pad in the menu

laObjdisp = .F.    && Default Display all lines in  screen 
llBrowse  = .F.

puDatePer = 1
lnRitPos  = 2
lnCount   = 0
lnTotlen  = 0
lnFrsSegSz= 15

*B600387,1 Variable to hold the batch posting window old value in the when func.
lcOldBPW  = 0

lcArrow1  = ' '
lcArrow2  = ' '
lcArrow3  = ' '
lcArrow4  = ' '
lcArrow5  = ' '
lcArrow6  = ' '

lcData1   = ' '    && Variable to Hold the laData[1]
lcData2   = ' '    && Variable to Hold the laData[2]
lcData3   = ' '
lcData19  = ' '    && Variable to hold old variable of laData[19]
lcAccMask = ' '    && Variable to Hold the Account Mask

llBatType = .F.    && Variable to hold the if its the first time from the SM
llOpenBy  = .F.
llOpenSeg = .F.
llFromSM  = IIF(TYPE('llFromSM')='U',.F.,llFromSM)
*E300692,5 Add variable for ACCOD file
llOpnAccCd = .F.
*E300692,5 end

IF !gfSetup()    
  RETURN
ENDIF  


lcCompDir = IIF(TYPE('lcCompDir')='C',ALLTRIM(lcCompDir),' ') && Variable to Hold the Data Dir

IF llFromSM .AND. !EMPTY(lcCompDir)
  IF FILE(lcCompDir+'GLSEGVAL.DBF')
    IF USED('GLSEGVAL')
      SELECT GLSEGVAL
      USE 
      llOpenSeg = .F.
    ELSE
      llOpenSeg = .T.
      SELECT 0
    ENDIF
    USE &lcCompDir.GLSEGVAL 
    SET ORDER TO TAG ACSSEGVAL
  ELSE
    =gfModalGen("TRM00085B00000","Dialog")      
    glQuitting = .T.
    =lfQuit()
    RETURN
  ENDIF  

  IF FILE(lcCompDir+'GLSETUP.DBF')
    IF USED('GLSETUP')
      SELECT GLSETUP
      USE
    ELSE
      SELECT 0
    ENDIF  
    USE &lcCompDir.GLSETUP
  ELSE
    =gfModalGen("TRM00085B00000","Dialog")      
    glQuitting = .T.
    =lfQuit()
    RETURN
  ENDIF  

  IF FILE(lcCompDir+'GLBATCH.DBF')
    IF USED('GLBATCH')
      SELECT GLBATCH
      USE
    ELSE  
      SELECT 0
    ENDIF  
    USE &lcCompDir.GLBATCH
    SET ORDER TO TAG BATCHNO
  ELSE
    =gfModalGen("TRM00085B00000","Dialog")      
    glQuitting = .T.
    =lfQuit()
    RETURN
  ENDIF

  IF FILE(lcCompDir+'GLACCHAR.DBF')
    IF USED('GLACCHAR')
      SELECT GLACCHAR
      USE
    ELSE  
      SELECT 0
    ENDIF  
    USE &lcCompDir.GLACCHAR
    SET ORDER TO TAG SEGACTIV 
  ELSE
    =gfModalGen("TRM00085B00000","Dialog")      
    glQuitting = .T.
    =lfQuit()
    RETURN
  ENDIF  
  
  *E300692,1 Change file name from SYCFISHD to FISHD  
  *IF !USED('SYCFISHD')
  IF !USED('FISHD')
*E300692,1 end  
    llOpenBy = .T.
    SELECT 0
  *E300692,1 Change file name and path from SYCFISHD to FISHD    
    *USE &gcSysHome.SYCFISHD ORDER 1
    USE &lcCompDir.FISHD ORDER 1
  *E300692,1 end  
  ELSE
  *E300692,1 Change file name from SYCFISHD to FISHD  
    *SELECT SYCFISHD
    SELECT FISHD
  *E30692,1 end  
  ENDIF  
  
  *E300692,5 Open ACCOD
  
  IF !USED('ACCOD')
    llOpnAccCd = .T.
    SELECT 0
    USE &lcCompDir.ACCOD ORDER 1
  ELSE
    SELECT ACCOD
  ENDIF  
  *E30692,5 end  
ENDIF

*E300692,1 Change file name and path from SYCACCOD to ACCOD  
*SELECT SYCACCOD.NACSSEGNO,SYCACCOD.NACSSIZE,;
       SYCACCOD.CACSSHDES;
 FROM  &gcSysHome.SYCACCOD;
 WHERE NACSSEGNO <> 0 .AND. ;
       CCOMP_ID = gcAct_comp ;
 INTO  ARRAY laSegSize

*E300789,7 [BEGIN] 
*SELECT ACCOD.NACSSEGNO,ACCOD.NACSSIZE,;
       ACCOD.CACSSHDES;
 FROM  ACCOD;
 WHERE NACSSEGNO <> 0 .AND. ;
       CCOMP_ID = gcAct_comp ;
 INTO  ARRAY laSegSize
SELECT ACCOD.NACSSEGNO,ACCOD.NACSSIZE,;
       ACCOD.CACSSHDES;
 FROM  ACCOD;
 WHERE NACSSEGNO <> 0  ;
 INTO  ARRAY laSegSize 
*E300789,7 [END..] 

*E300692,1 end

IF ALEN(laSegSize) > 1 .AND. !EMPTY(laSegSize[1,1])
  *** Calculate seg. postions & sizes. ***
  FOR lnCount = 1 TO ALEN(laSegSize,1)
    lnTotlen  = lnTotlen  + laSegSize [lnCount,1] + 4
    laObjdisp[lnCount]  = .T.
  ENDFOR
  lnXSize   = 15     && Variable to Hold the X size of laData[1],laData[2] in the screen
  lnFrsSegSz= laSegSize[1,2]
ENDIF

SELECT GLBATCH
lcSavOrder = SET('ORDER')
SET ORDER TO BATTYPE
IF SEEK('B','GLBATCH')
  llBatType = .T.
ELSE
  llBatType = .F.
ENDIF  
SET ORDER TO &lcSavOrder

*E300692,1 Change file name from SYCACCOD to ACCOD  
*SELECT SYCACCOD
SELECT ACCOD
*E300692,1 end

*E300789,7 [BEGIN] The next block of code will be commented to refelect removing
*E300789,7 ccomp_id from ACCOD
*SET ORDER TO TAG COMPID
**E300692,1 Change file name from SYCACCOD to ACCOD  
**IF SEEK(gcAct_Comp,"SYCACCOD")
*IF SEEK(gcAct_Comp,"ACCOD")
IF !EOF() 
*E300789,7 [END..]
  lcAccMask  = cAcsMask
  lnAcsNoSeg = nAcsNoSeg
  SKIP 1
  lnXSize = nAcsSize
ELSE
  lnXSize = 15
ENDIF

SELECT GLSETUP

SCATTER FIELDS &lcScFields TO laData 

IF !laData[17]  &&-- Module has not setuped yet .
  laData[4]  = .F.  
  laData[5]  = .T.
  laData[6]  = .T.
  laData[7]  = .T.
  laData[8]  = .T.
  laData[9]  = IIF(EMPTY(ldFisBgDat),{},ldFisBgDat)
  laData[10] = .F.
  laData[11] = .F.
  laData[12] = .F.
  laData[13] = .T.
  laData[14] = 1
  laData[16] = .T.
  laData[15] = 'P'
  lcDatPer   = laTDatePer[1,1]
  puDateper  = 1
  laData[18] = 1
  lcArrow1   = ''
ELSE
  puDatePer  = IIF(AT(ALLTRIM(laData[15]),"PD")>0,AT(ALLTRIM(laData[15]),"PD"),1)
  lcDatPer   = laTDatePer[puDatePer,1]
  =lfPointer()
ENDIF  
 
*E300683,5 Call *.SPR from screens directory
* DO Glsetup.SPR 
DO (gcScrDir + gcAct_Appl + '\Glsetup.SPR')
*E300683,5 end   
glQuitting = .T.

=lfQuit()

*!**************************************************************************
*!
*!      Procedure: lpShow
*!
*!**************************************************************************
*
PROCEDURE lpShow
*E300692,1 Change file name from SYCACCOD to ACCOD  
*SELECT SYCACCOD
*IF !SEEK(gcAct_Comp,"SYCACCOD")
SELECT ACCOD

*B802198,1 Since ACCOD became a data file , there is no field called [Begin]
*          comp_id any more so the following IF condition must be modified .
*IF !SEEK(gcAct_Comp,"ACCOD")
**E300692,1 end
GO TOP
IF EOF()
  *B802198,1 Since ACCOD became a data file , there is no field called [End..]
  SHOW GET laData[1]  DISABLE
  SHOW GET laData[2]  DISABLE
  SHOW GET laData[3]  DISABLE
  SHOW GET laData[19] DISABLE
ELSE
  SHOW GET laData[1]  ENABLE  
  SHOW GET laData[2]  ENABLE
  SHOW GET laData[3]  ENABLE
  SHOW GET laData[19] ENABLE
ENDIF
*B802198,1 Since ACCOD became a data file , there is no field called [End  ]

IF llBatType
  SHOW GET laData[9] DISABLE
ELSE
  SHOW GET laData[9] ENABLE
ENDIF

IF laData[14] = 1
  IF _DOS
    lcDatPer   = laTDatePer[1,1]
    laData[15] = 'P'
    =lfRefresh()
    SHOW GET ibDatPer ENABLE
  ELSE
    laData[15] = 'P'
    puDateper  = 1
    SHOW GET puDateper ENABLE
  ENDIF  
ELSE
  IF _DOS
    =lfRefresh()
    SHOW GET ibDatPer ENABLE
  ELSE
    SHOW GET puDateper ENABLE
  ENDIF  
ENDIF  


IF laData[17]
  SHOW GET laData[10] DISABLE
ELSE
  SHOW GET laData[10] ENABLE
ENDIF

*IF _DOS
*  lcColor1 = SCHEME(1,6)
*  lcColor2 = SCHEME(1,2)
*  *** Show get all hand made popups with the appropirate color. ***
*  SHOW GET ibDatPer  COLOR ,,,,,&lcSelCont,,,&lcEnbCont,&lcDisCont
*ENDIF

*!**************************************************************************
*!
*!      Function: lfwData_1
*!
*!**************************************************************************
*
FUNCTION lfwData_1

lcData1 = laData[1]

*!**************************************************************************
*!
*!      Function: lfvData_1
*!
*!**************************************************************************
*
FUNCTION lfvData_1

lcRetain = ALLTRIM(laData[1])+STRTRAN(SUBSTR(lcAccMask,AT("-",lcAccMask)),"#","0")

IF llBrowse .OR. (!EMPTY(laData[1]) .AND. lcData1 <> laData[1])

  SELECT GLACCHAR
  lcSavOrder = SET('ORDER')
  SET ORDER TO ACCTCODE

  LOCATE FOR CACCTCODE = lcRetain .AND. LEFT(cTypeCode,1) = 'Q'

  IF !FOUND() .OR. ATC('?',laData[1]) > 0 .OR. llBrowse

    LOCATE FOR LEFT(cTypeCode,1) = 'Q'

    IF FOUND()

      DIMENSION laTemp[1]
      laTemp[1] = ''
      lcSavBrFld=lcBrfields
      lcSavTitle=lcFile_Ttl

      lcBrfields="CACCTCODE :H= 'Account Code',;
                  CACCNLDES :H= 'Account Description',;
                  CTYPECODE :H= 'Type Code',;
                  CSTANDARD :H= 'Standard'"

      SET ORDER TO TAG TYPACTPOS

      =gfbrows(["Q"FOR VAL(STRTRAN(SUBSTR(CACCTCODE,lnFrsSegSz+1),"-",""))=0],'CACCTCODE,CACCNLDES,CTYPECODE,CSTANDARD','laTemp')

      lcFile_Ttl=lcSavTitle
      lcBrfields=lcSavBrFld

      SET ORDER TO

      IF !EMPTY(laTemp[1])
        laData[1] = SUBSTR(laTemp[1],1,lnFrsSegSz)
        lcRetain  = ALLTRIM(laData[1])+STRTRAN(SUBSTR(lcAccMask,AT("-",lcAccMask)),"#","0")
      ELSE
        laData[1] = lcData2
        _CUROBJ=OBJNUM(laData[1])
      ENDIF   
    ELSE
      =gfModalGen("TRM00052B00000","Dialog")        
      laData[1] = SPACE(lnFrsSegSz)
    ENDIF  
  ENDIF
  SET ORDER TO &lcSavOrder
ENDIF 

laData[1]  = SUBSTR(laData[1],1,lnFrsSegSz)

llBrowse   = .F.

SHOW GET laData[1] ENABLE

SELECT GLSETUP

*!**************************************************************************
*!
*!      Function: lfwData_2
*!
*!**************************************************************************
*
FUNCTION lfwData_2

lcData2 = laData[2]

*!**************************************************************************
*!
*!      Function: lfvData_2
*!
*!**************************************************************************
*
FUNCTION lfvData_2

lcSuspen = ALLTRIM(laData[2])+STRTRAN(SUBSTR(lcAccMask,AT("-",lcAccMask)),"#","0")

IF llBrowse .OR. (!EMPTY(laData[2]) .AND. lcData2 <> laData[2])

  SELECT GLACCHAR
  lcSavOrder = SET('ORDER')
  SET ORDER TO ACCTCODE

  LOCATE FOR CACCTCODE = lcSuspen .AND. cStandard = 'Y'

  IF !FOUND() .OR. ATC('?',laData[2]) > 0 .OR. llBrowse

    LOCATE FOR cStandard = 'Y'

    IF FOUND()

      DIMENSION laTemp[1]
      laTemp[1] = ''
      lcSavBrFld=lcBrfields
      lcSavTitle=lcFile_Ttl

      lcBrfields="CACCTCODE :H= 'Account Code',;
                  CACCNLDES :H= 'Account Description',;
                  CTYPECODE :H= 'Type Code',;
                  CSTANDARD :H= 'Standard'"

      lcFile_Ttl = 'Chart of Account'

      SET ORDER TO TAG STDACTPOS

      =gfbrows(["Y"FOR VAL(STRTRAN(SUBSTR(CACCTCODE,lnFrsSegSz+1),"-",""))=0],'CACCTCODE,CACCNLDES,CTYPECODE,CSTANDARD','laTemp')

      lcFile_Ttl=lcSavTitle
      lcBrfields=lcSavBrFld

      SET ORDER TO

      IF !EMPTY(laTemp[1])
        laData[2] = SUBSTR(laTemp[1],1,lnFrsSegSz)
        lcSuspen  = ALLTRIM(laData[2])+STRTRAN(SUBSTR(lcAccMask,AT("-",lcAccMask)),"#","0")
      ELSE
        laData[2] = lcData2
        _CUROBJ=OBJNUM(laData[2])
      ENDIF   
    ELSE
      =gfModalGen("TRM00052B00000","Dialog")        
      laData[2] = SPACE(lnFrsSegSz)
      _CUROBJ=OBJNUM(laData[2])
    ENDIF  
  ENDIF
  SET ORDER TO &lcSavOrder
ENDIF

laData[2]  = SUBSTR(laData[2],1,lnFrsSegSz)

llBrowse   = .F.

SHOW GET laData[2] ENABLE

SELECT GLSETUP

*!**************************************************************************
*!
*!      Function: lfwData_3
*!
*!**************************************************************************
*
FUNCTION lfwData_3

lcData3 = laData[3]

*!**************************************************************************
*!
*!      Function: lfvData_3
*!
*!**************************************************************************
*
FUNCTION lfvData_3

lcBudCod=ALLTRIM(laData[3])

IF llBrowse .OR. (!EMPTY(laData[3]) .AND. lcData3 <> laData[3])
  SELECT GLBUDHD
  lcSavOrder = SET('ORDER')
  SET ORDER TO BDCODYR
  LOCATE FOR CBudCode = lcBudCod
  IF !FOUND() .OR. ATC('?',laData[3]) > 0 .OR. llBrowse
      DIMENSION laTemp[1]
      laTemp[1]  = ''
      lcSavBrFld = lcBrfields
      lcSavTitle = lcFile_Ttl
      lcBrfields = "CBudCode :H= 'Budget Code',;
                    CBudYear :H= 'Budget Year',;
                    CBudDes   :H= 'Budget Description'"
      lcFile_Ttl = 'Budget Codes'
      =gfbrows(.F.,'cBudCode,cBudYear,cBudDes','laTemp')
      lcFile_Ttl = lcSavTitle
      lcBrfields = lcSavBrFld
      SET ORDER TO
      IF !EMPTY(laTemp[1])
        laData[3] = laTemp[1]
      ELSE
        laData[3] = lcData3
      ENDIF
   ENDIF
   SET ORDER TO &lcSavOrder
ENDIF

SHOW GET laData[3] ENABLE

SELECT GLSETUP

llBrowse = .F.

*!*************************************************************
*! Name      : lfwData_14
*! Developer : Reham Alallamy
*! Date      : 06/07/95
*! Purpose   : This function called from the when of the batch
*!             posting window to save the old value to solve 
*!             bug  { B600387,1 }.
*!*************************************************************
*! Calls     : 
*!             Procedures : 
*!             Functions  : 
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example   : 
*!*************************************************************
*
FUNCTION lfwData_14

*B600387,1 Save the old value.
lcOldBPW = laData[14]

*!**************************************************************************
*!
*!      Function: lfvData_14
*!
*!**************************************************************************
*
FUNCTION lfvData_14

*B600387,1  Make the range within 1-13
IF !BETWEEN(laData[14],1,13)
  *Message : "The batch posting window has to fall 
  *           within the range of [1 - 13]."
  *Button  : < Ok >
  =gfModalGen("TRM02244B00000","DIALOG")
  laData[14] = lcOldBPW
ENDIF

*B600387,1  Remark the previous validations .
*IF laData[14] = 1
*  IF _DOS
*    lcDatPer   = laTDatePer[1,1]
*    laData[15] = 'P'
*    =lfRefresh()
*    SHOW GET ibDatPer ENABLE
*  ELSE
*    laData[15] = 'P'
*    puDateper  = 1
*    SHOW GET puDateper ENABLE
*  ENDIF  
*ELSE
*  IF _DOS
*    lcDatPer   = laTDatePer[2,1]
*    laData[15] = 'D'
*    =lfRefresh()
*    SHOW GET ibDatPer ENABLE
*  ELSE
*    laData[15] = 'D'
*    puDateper  = 2
*    SHOW GET puDateper ENABLE
*  ENDIF  
*ENDIF  

*!**************************************************************************
*!
*!      Function: lfvData_15
*!
*!**************************************************************************
*
FUNCTION lfvData_15

IF _DOS
  laData[15] = gfActPop(8,65,12,74,'laTDatePer',2,1,@lcDatPer)
  =lfRefresh()
  IF laData[15]='P'
    SHOW GET ibDatPer ENABLE
  ELSE
    SHOW GET ibDatPer ENABLE
  ENDIF  
ELSE
  IF puDateper =1
    laData[15] = 'P'
    SHOW GET puDateper ENABLE
  ELSE
    laData[15] = 'D'
    SHOW GET puDateper ENABLE
  ENDIF
ENDIF

*!**************************************************************************
*!
*!      Function: lfBuildPic
*!
*!**************************************************************************
*
FUNCTION lfBuildPic

RETURN "X"+REPLICATE("9",lnFrsSegSz-1)

*!**************************************************************************
*!
*!      Function: lfvOk
*!
*!**************************************************************************
*
FUNCTION lfvOk

*MAN
llFrstSet  = laData[17]

laData[17] = .T.

laData[1]  = IIF(EMPTY(laData[1]),SPACE(lnXSize),SUBSTR(laData[1],1,lnFrsSegSz))
laData[2]  = IIF(EMPTY(laData[2]),SPACE(lnXSize),SUBSTR(laData[2],1,lnFrsSegSz))
laData[19] = IIF(EMPTY(laData[19]),SPACE(lnXSize),SUBSTR(laData[19],1,lnFrsSegSz))

SELECT GLSETUP

GO TOP
IF EOF()
  APPEND BLANK
ENDIF  

GATHER FROM laData FIELDS &lcScFields 

=gfAdd_Info()

FLUSH

*** Enter the default segments with zero ***
*** values except the first one...
*MAN Added IF llFrstSet to update the default seg. val. only in the first
*MAN setup
IF !llFrstSet
  SELECT GLSEGVAL
  FOR lnCount = 2 TO ALEN(laSegSize,1)
    lcSegNo   = ALLTRIM(STR(laSegSize[lnCount,1]))
    lcSegVal  = REPLICATE('0',laSegSize[lnCount,2])
    IF !SEEK(lcSegNo+lcSegVal)
      APPEND BLANK
      REPLACE CACSSEGNO WITH lcSegNo ;
              CSEGVALUE WITH lcSegVal ;
              CSEGLNDES WITH "Default" ;
              CSEGSHDES WITH "Default"
      =gfAdd_Info()
    ENDIF
  ENDFOR
ENDIF  

SELECT GLSETUP

*!**************************************************************************
*!
*!      Function: lfvCancel
*!
*!**************************************************************************
*
FUNCTION lfvCancel



*!**************************************************************************
*!
*!      Function: lfvCost1
*!
*!**************************************************************************
*
FUNCTION lfvCost1

laData[18] = 1
=lfPointer()

*!**************************************************************************
*!
*!      Function: lfvCost2
*!
*!**************************************************************************
*
FUNCTION lfvCost2

laData[18] = 2
=lfPointer()

*!**************************************************************************
*!
*!      Function: lfvCost3
*!
*!**************************************************************************
*
FUNCTION lfvCost3

laData[18] = 3
=lfPointer()

*!**************************************************************************
*!
*!      Function: lfvCost4
*!
*!**************************************************************************
*
FUNCTION lfvCost4

laData[18] = 4
=lfPointer()

*!**************************************************************************
*!
*!      Function: lfvCost5
*!
*!**************************************************************************
*
FUNCTION lfvCost5
 
laData[18] = 5
=lfPointer()

*!**************************************************************************
*!
*!      Function: lfvCost6
*!
*!**************************************************************************
*
FUNCTION lfvCost6

laData[18] = 6
=lfPointer()

*!**************************************************************************
*!
*!      Function: lfPointer
*!
*!**************************************************************************
*
FUNCTION lfPointer

STORE " " TO lcArrow1,lcArrow2,lcArrow3,lcArrow4,lcArrow5,lcArrow6
lcArrow  = "lcArrow" + ALLTRIM(STR(laData[18]))
&lcArrow = ''

IF WEXIST(gcBaseWind)
  =lfRefresh()
ENDIF

*!**************************************************************************
*!
*!      FUNCTION : lfQuit
*!
*!**************************************************************************
*
FUNCTION lfQuit

IF llOpenSeg .AND. USED('GLSEGVAL')
  USE IN GLSEGVAL
  llOpenSeg = .F.
ENDIF

*E300692,1 Change file name from SYCFISHD to FISHD  
*IF USED('SYCFISHD') .AND. llOpenBy
IF USED('FISHD') .AND. llOpenBy
*E300692,1 end
  llOpenBy = .F.
*E300692,1 Change file name from SYCFISHD to FISHD    
  *SELECT SYCFISHD 
  *USE IN SYCFISHD 
  SELECT FISHD 
  USE IN FISHD 
*E300692,1 end  
ENDIF  

*E300692,5 close ACCOD
IF USED('ACCOD') .AND. llOpnAccCd
  llOpnAccCd = .F.
  USE IN ACCOD
ENDIF  
*E300692,5 end  

IF llFromSM .AND. !EMPTY(lcCompDir)
  IF USED('GLSETUP')
    SELECT GLSETUP
    USE
  ENDIF  
  IF USED('GLBATCH')
    SELECT GLBATCH
    USE
  ENDIF  
  IF USED('GLACCHAR')
    SELECT GLACCHAR
    USE
  ENDIF  
ENDIF
 
*!**************************************************************************
*!
*!      Function: lfwData_19
*!
*!**************************************************************************
*
FUNCTION lfwData_19

lcData19 = laData[19]

*!**************************************************************************
*!
*!      Function: lfvData_19
*!
*!**************************************************************************
*
FUNCTION lfvData_19

lcExMaj = ALLTRIM(laData[19])+STRTRAN(SUBSTR(lcAccMask,AT("-",lcAccMask)),"#","0")

IF llBrowse .OR. (!EMPTY(laData[19]) .AND. lcData19 <> laData[19])

  SELECT GLACCHAR
  lcSavOrder = SET('ORDER')
  SET ORDER TO ACCTCODE

  LOCATE FOR CACCTCODE = lcExMaj .AND. cStandard = 'Y'

  IF !FOUND() .OR. ATC('?',laData[19]) > 0 .OR. llBrowse

    LOCATE FOR cStandard = 'Y'

    IF FOUND()

      DIMENSION laTemp[1]
      laTemp[1] = ''
      lcSavBrFld=lcBrfields
      lcSavTitle=lcFile_Ttl

      lcBrfields="CACCTCODE :H= 'Account Code',;
                  CACCNLDES :H= 'Account Description',;
                  CTYPECODE :H= 'Type Code',;
                  CSTANDARD :H= 'Standard'"

      lcFile_Ttl = 'Chart of Account'

      SET ORDER TO TAG STDACTPOS

      =gfbrows(["Y"FOR VAL(STRTRAN(SUBSTR(CACCTCODE,lnFrsSegSz+1),"-",""))=0],'CACCTCODE,CACCNLDES,CTYPECODE,CSTANDARD','laTemp')

      lcFile_Ttl=lcSavTitle
      lcBrfields=lcSavBrFld

      SET ORDER TO

      IF !EMPTY(laTemp[1])
        laData[19] = SUBSTR(laTemp[1],1,lnFrsSegSz)
        lcExMaj    = ALLTRIM(laData[19])+STRTRAN(SUBSTR(lcAccMask,AT("-",lcAccMask)),"#","0")
      ELSE
        laData[19] = lcData19
        _CUROBJ=OBJNUM(laData[19])
      ENDIF   
    ELSE
      =gfModalGen("TRM00052B00000","Dialog")        
      laData[19] = SPACE(lnFrsSegSz)
      _CUROBJ=OBJNUM(laData[19])
    ENDIF  
  ENDIF
  SET ORDER TO &lcSavOrder
ENDIF

laData[19]  = SUBSTR(laData[19],1,lnFrsSegSz)

llBrowse   = .F.

SHOW GET laData[19] ENABLE

SELECT GLSETUP
