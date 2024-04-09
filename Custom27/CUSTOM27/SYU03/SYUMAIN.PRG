*!**************************************************************************
*! Name      : SYUMAIN.PRG
*! Developer : Mohamed Shokry (MHM)
*! Date      : 01/20/2002
*! Purpose   : Studio Direct Custom Process Program .
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : Logical value.       C102497,1
*!**************************************************************************
*C102564,1 BWA 04/24/2002 Add 3 fields to let know if the style has change or not.[START]
*B606309,1 TMI 08/01/2002 Update audit trail fields with editors data
*C102798,1 WAB 02/10/2003 To increment the style price by the design selling price.  
*C124347,1 NNA 09/02/2004 Add the Customer. nhand_chrg (used Define field) Value to the Freight in the Invoice SO 
*!**************************************************************************

PARAMETER lcEvntFun,lcFunPars

lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLTRIM(lcEvntFun)+'('+lcFunPars+')'

*-- Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue
*-- End of Program.

*!**************************************************************************
*! Name      : lfGetPrvat
*! Developer : Mohamed Shokry (MHM)
*! Date      : 01/20/2002
*! Purpose   : validate label Button
*!**************************************************************************
*! Passed Parameters : ldCompDate  (Complete Date from Sales Order screen)
*!**************************************************************************
*C102497,1
FUNCTION lfGetPrvat
laPanelObj[2,1] = 'pbOption'
laPanelObj[2,2] = gcBmpHome+"Genrate.bmp"
laPanelObj[2,3] = [VALID gfDoTriger('SOORD',PADR('VLABEL',10)) MESSAGE 'Private Label' ENABLE]

*!*************************************************************
*! Name      : lfvLabel
*! Developer : Mohamed Shokry (MHM)
*! Date      : 01/20/2002
*! Purpose   : to enter all production related information we need to assign
*!           : to the entered SO line 
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Called Form        : 
*!*************************************************************
*! Example            : =lfvLabel()
*!*************************************************************
*C102497,1
FUNCTION lfVLABEL


STORE 0 TO lnLabel1 ,lnLabel2 ,lnLabel3 ,lnLabel4
STORE 0 TO lnColour1 , lnColour2 ,lnColour3 ,lnColour4 

STORE '' TO lcFileNo11 , lcFileNo22 ,lcFileNo33 ,lcFileNo44
*STORE LADATA[2] TO  lcFileNo1, lcFileNo2,lcFileNo3,lcFileNo4

STORE '' TO lcFilDesc1,lcFilDesc2,lcFilDesc3,lcFilDesc4
STORE .F. TO llFilLoc1,llFilLoc2,llFilLoc3,llFilLoc4 
*--lcLocStat   ==========> to control show of object in local screen
DO CASE
  CASE laScrMode[1]  
     lcLocStat = "DISABLE"
  CASE laScrMode[2]  
     lcLocStat = "DISABLE"
  CASE laScrMode[3]  
     lcLocStat = "ENABLE"
  CASE laScrMode[4]  
     lcLocStat = "ENABLE"
ENDCASE
*--to get mover
DIMENSION laSource1[1],laSource2[1],laSource3[1],laSource4[1]
DIMENSION laTarget1[1],laTarget2[1],laTarget3[1],laTarget4[1]
STORE ' ' TO laSource1,laSource2,laSource3,laSource4
STORE ' ' TO laTarget1,laTarget2,laTarget3,laTarget4
*--to get mover

DECLARE laCodesL[8,10] ,laLabel1[1,2],laLabel2[1,2],laLabel3[1,2],laLabel4[1,2],;
        laColour1[1,2] ,laColour2[1,2] ,laColour3[1,2], laColour4[1,2]

STORE 1 TO lnLabel1,lnLabel2,lnLabel3,lnLabel4
STORE 1 TO lnColour1,lnColour2,lnColour3,lnColour4
laCodesL[1,1] = 'CPLOPTN1  '
laCodesL[1,2] = 'laLabel1'
laCodesL[1,3] = 'lnLabel1'
laCodesL[1,4] = ''
laCodesL[1,5] = .F.
laCodesL[1,6] = .F.
laCodesL[1,10] = 'CPLOPTN1  '

laCodesL[2,1] = 'CPLOPTN2  '
laCodesL[2,2] = 'laLabel2'
laCodesL[2,3] = 'lnLabel2'
laCodesL[2,4] = ''
laCodesL[2,5] = .F.
laCodesL[2,6] = .F.
laCodesL[2,10] = 'CPLOPTN2  '

laCodesL[3,1] = 'CPLOPTN3  '
laCodesL[3,2] = 'laLabel3'
laCodesL[3,3] = 'lnLabel3'
laCodesL[3,4] = ''
laCodesL[3,5] = .F.
laCodesL[3,6] = .F.
laCodesL[3,10] = 'CPLOPTN3  '

laCodesL[4,1] = 'CPLOPTN4  '
laCodesL[4,2] = 'laLabel4'
laCodesL[4,3] = 'lnLabel4'
laCodesL[4,4] = ''
laCodesL[4,5] = .F.
laCodesL[4,6] = .F.
laCodesL[4,10] = 'CPLOPTN4  '


laCodesL[5,1] = 'CPLCLR1   '
laCodesL[5,2] = 'laColour1'
laCodesL[5,3] = 'lnColour1'
laCodesL[5,4] = ''
laCodesL[5,5] = .F.
laCodesL[5,6] = .F.
laCodesL[5,10] = 'CPLCLR1   '


laCodesL[6,1] = 'CPLCLR2   '
laCodesL[6,2] = 'laColour2'
laCodesL[6,3] = 'lnColour2'
laCodesL[6,4] = ''
laCodesL[6,5] = .F.
laCodesL[6,6] = .F.
laCodesL[6,10] = 'CPLCLR2   '


laCodesL[7,1] = 'CPLCLR3   '
laCodesL[7,2] = 'laColour3'
laCodesL[7,3] = 'lnColour3'
laCodesL[7,4] = ''
laCodesL[7,5] = .F.
laCodesL[7,6] = .F.
laCodesL[7,10] = 'CPLCLR3   '

laCodesL[8,1] = 'CPLCLR4   '
laCodesL[8,2] = 'laColour4'
laCodesL[8,3] = 'lnColour4'
laCodesL[8,4] = ''
laCodesL[8,5] = .F.
laCodesL[8,6] = .F.
laCodesL[8,10] = 'CPLCLR4   '
=lfCreatTmp()
IF laScrMode[2]
  *C102573,1 (Begin) Adjust Close button prompt.
  *lcClosKey = gcBmpHome + "CAN.BMP"
  lcClosKey = gcBmpHome + "Close2.BMP"
  *C102573,1 (End)
ELSE
  *C102573,1 (Begin) Adjust Close button prompt.
  *lcClosKey = gcBmpHome + "Close2.BMP"
  lcClosKey = gcBmpHome + "CAN.BMP"
  *C102573,1 (End)
ENDIF  
*-- Clear the trapped keys.
PUSH KEY
ON KEY
ON KEY LABEL TAB  DO lplTab

*--create temp file
DO (gcScrDir+gcWinAppl+"\SOSYU10.SPX")
POP KEY


*!*************************************************************
*! Name      : lfvLabel
*! Developer : Mohamed Shokry (MHM)
*! Date      : 01/20/2002
*! Purpose   : to enter all production related information we need to assign
*!           : to the entered SO line 
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Called Form        : 
*!*************************************************************
*! Example            : =lfvLabel()
*!*************************************************************
*C102497,1
FUNCTION lfwInit

*C102573,1 (Begin) When showing screen for the first time, show defaults, othres get saved.
IF USED(lcTempLabl) AND RECCOUNT(lcTempLabl) <> 0 AND !EMPTY(&lcTempLabl..cploptn1) AND !DELETED()
*C102573,1 (End)
  =gfwCodePop(@laCodesL,'CPLOPTN1',laLabel1[lnLabel1,2])
  =gfwCodePop(@laCodesL,'CPLOPTN2',laLabel2[lnLabel2,2])
  =gfwCodePop(@laCodesL,'CPLOPTN3',laLabel3[lnLabel3,2])
  =gfwCodePop(@laCodesL,'CPLOPTN4',laLabel4[lnLabel4,2])

  =gfwCodePop(@laCodesL,'CPLCLR1   ',laColour1[lnColour1,2])
  =gfwCodePop(@laCodesL,'CPLCLR2   ',laColour2[lnColour2,2])
  =gfwCodePop(@laCodesL,'CPLCLR3   ',laColour3[lnColour3,2])
  =gfwCodePop(@laCodesL,'CPLCLR4   ',laColour4[lnColour4,2])
  
*C102573,1 (Begin) When showing screen for the first time, show defaults, othres get saved.
ELSE
  =gfwCodePop(@laCodesL,'CPLOPTN1','D')
  =gfwCodePop(@laCodesL,'CPLOPTN2','D')
  =gfwCodePop(@laCodesL,'CPLOPTN3','D')
  =gfwCodePop(@laCodesL,'CPLOPTN4','D')

  =gfwCodePop(@laCodesL,'CPLCLR1 ','D')
  =gfwCodePop(@laCodesL,'CPLCLR2 ','D')
  =gfwCodePop(@laCodesL,'CPLCLR3 ','D')
  =gfwCodePop(@laCodesL,'CPLCLR4 ','D')
  lcBrand    = IIF(&lcTempLabl..lBrapp  AND !DELETED(),'Yes','No')
  lcEmbr     = IIF(&lcTempLabl..lEmbapp AND !DELETED(),'Yes','No')
  SHOW GET lcBrand
  SHOW GET lcEmbr
ENDIF
*C102573,1 (End)


*!*************************************************************
*! Name      : lfvAttchd
*! Developer : Mohamed Shokry (MHM)
*! Date      : 01/20/2002
*! Purpose   : Validate Attached files
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Called Form        : 
*!*************************************************************
*! Example            : =lfvLabel()
*!*************************************************************
*C102497,1

FUNCTION lfvAttchd
PARAMETER lcCurrObj

DIMENSION laGetFil[1,1]
STORE ' ' TO laGetFil
lcNewpath = gcDef_path +"artwork"
SET DEFA TO &lcNewpath 
=ADIR(laGetFil)

FOR I =1 TO ALEN(laGetFil,1)
  DIMENSION laSource&lcCurrObj[I]
  laSource&lcCurrObj[I] = laGetFil[I,1]
ENDFOR

IF !llFilLoc&lcCurrObj
  FOR I = 1 TO MEMLINES(&lcTempLabl..mfileloc&lcCurrObj)
    DIMENSION laTarget&lcCurrObj[I]
    laTarget&lcCurrObj[I] =  SUBSTR(MLINE(&lcTempLabl..mfileloc&lcCurrObj, I ),LEN(gcDef_Path)+1)
  ENDFOR  
ENDIF

IF !llFilLoc&lcCurrObj
  llFilLoc&lcCurrObj = .T.
ENDIF
*--- SSH,B#605871
=gfMover(@laSource&lcCurrObj, @laTarget&lcCurrObj,"Select Files",,!laScrMode[2],'')
*--- SSH,B#605871
SET DEFA TO &gcDef_path
*!*************************************************************
*! Name      : lfCreatTmp
*! Developer : Mohamed Shokry (MHM)
*! Date      : 01/20/2002
*! Purpose   : Create temp file
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Called Form        : 
*!*************************************************************
*! Example            : =lfCreatTmp()
*!*************************************************************
*C102497,1
FUNCTION lfCreatTmp

IF !USED('PLINFO')
  =gfOpenFile(gcDataDir+'PLINFO', gcDataDir + 'PLINFO','SH')
ENDIF
IF !USED(lcTempLabl)
  DIMENSION laTempStru[1,4]
  laTempStru = ''
  SELECT PLINFO
  = AFIELDS(laTempStru)
  CREATE TABLE (gcWorkDir+lcTempLabl) ;
  FROM ARRAY laTempStru
  INDEX ON ORDER+STR(LineNO,6) Tag &lcTempLabl
  IF !laScrMode[4] AND SEEK(laData[1],'PLINFO') 
    SELECT PLINFO
    SCAN WHILE ORDER = laData[1]
      SCATT MEMVAR MEMO
      SELECT (lcTempLabl)
      APPEND BLANK
      GATHER MEMVAR MEMO
    ENDSCAN
  ENDIF

ENDIF
IF laScrMode[4] OR laScrMode[3]
  =SEEK(&lcOrdLine..Order+STR(&lcOrdLine..LineNo,6),lcTempLabl)
ENDIF
IF laScrMode[2]
  =SEEK(OrdLine.Order+STR(OrdLine.LineNo,6),lcTempLabl)
ENDIF
laLabel1[lnLabel1,2] = &lcTempLabl..cploptn1
laLabel2[lnLabel2,2] = &lcTempLabl..cploptn2
laLabel3[lnLabel3,2] = &lcTempLabl..cploptn3
laLabel4[lnLabel4,2] = &lcTempLabl..cploptn4

laColour1[lnColour1,2] = &lcTempLabl..cplclr1
laColour2[lnColour2,2] = &lcTempLabl..cplclr2
laColour3[lnColour3,2] = &lcTempLabl..cplclr3
laColour4[lnColour4,2] = &lcTempLabl..cplclr4

  *--ONLY THREE CRUCTER
*C102573,1 (Begin) Get the entire field.
*lcFileNo11 = RIGHT(&lcTempLabl..cartfno1,3)
*lcFileNo22 = RIGHT(&lcTempLabl..cartfno2,3)
*lcFileNo33 = RIGHT(&lcTempLabl..cartfno3,3)
*lcFileNo44 = RIGHT(&lcTempLabl..cartfno4,3)
lcFileNo11 = &lcTempLabl..cartfno1
lcFileNo22 = &lcTempLabl..cartfno2
lcFileNo33 = &lcTempLabl..cartfno3
lcFileNo44 = &lcTempLabl..cartfno4
*C102573,1 (End)

lcFilDesc1 = &lcTempLabl..cartfdes1
lcFilDesc2 = &lcTempLabl..cartfdes2
lcFilDesc3 = &lcTempLabl..cartfdes3
lcFilDesc4 = &lcTempLabl..cartfdes4

*C102573,1 (Begin) Get the new 2 added fields.
lcBrand    = IIF(&lcTempLabl..lBrapp,'Yes','No')
lcEmbr     = IIF(&lcTempLabl..lEmbapp,'Yes','No')
*C102573,1 (End)

*!*************************************************************
*! Name      : lfvLabSav
*! Developer : Mohame Shokry(MHM)
*! Date      : 01/22/2002
*! Purpose   : Save data
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvLabSav()
*!*************************************************************
*C102497,1
FUNCTION lfvLabSav

*--lcOrdLine ==========> temp file come from standerd
SELECT (lcTempLabl)
IF !SEEK(&lcOrdLine..Order+STR(&lcOrdLine..LineNo,6),lcTempLabl)
  APPEND BLANK
ENDIF
*C102573,1 (Begin) Update new fields.
*REPLACE cploptn1 WITH laLabel1[lnLabel1,2],;
        cploptn2 WITH laLabel2[lnLabel2,2] ,;
        cploptn3 WITH laLabel3[lnLabel3,2] ,;
        cploptn4 WITH laLabel4[lnLabel4,2] ,;
        cplclr1 WITH laColour1[lnColour1,2] ,;
        cplclr2 WITH laColour2[lnColour2,2] ,;
        cplclr3 WITH laColour3[lnColour3,2] ,;
        cplclr4 WITH laColour4[lnColour4,2] ,;
        cartfno1 WITH (LEFT(lcFileNo1,5) + lcFileNo11) ,;
        cartfno2 WITH (LEFT(lcFileNo2,5) + lcFileNo22 ),; 
        cartfno3 WITH (LEFT(lcFileNo3,5) + lcFileNo33),;
        cartfno4 WITH (LEFT(lcFileNo4,5) + lcFileNo44),;
        cartfdes1 WITH lcFilDesc1 ,;
        cartfdes2 WITH lcFilDesc2 ,;
        cartfdes3 WITH lcFilDesc3 ,;
        cartfdes4 WITH lcFilDesc4 ,;
        Order     WITH &lcOrdLine..Order,;
        LineNo    WITH &lcOrdLine..LineNo
REPLACE cploptn1  WITH laLabel1[lnLabel1,2],;
        cploptn2  WITH laLabel2[lnLabel2,2] ,;
        cploptn3  WITH laLabel3[lnLabel3,2] ,;
        cploptn4  WITH laLabel4[lnLabel4,2] ,;
        cplclr1   WITH laColour1[lnColour1,2] ,;
        cplclr2   WITH laColour2[lnColour2,2] ,;
        cplclr3   WITH laColour3[lnColour3,2] ,;
        cplclr4   WITH laColour4[lnColour4,2] ,;
        cartfno1  WITH lcFileNo11  ,;
        cartfno2  WITH lcFileNo22  ,; 
        cartfno3  WITH lcFileNo33  ,;
        cartfno4  WITH lcFileNo44  ,;
        cartfdes1 WITH lcFilDesc1 ,;
        cartfdes2 WITH lcFilDesc2 ,;
        cartfdes3 WITH lcFilDesc3 ,;
        cartfdes4 WITH lcFilDesc4 ,;
        Order     WITH &lcOrdLine..Order,;
        LineNo    WITH &lcOrdLine..LineNo,;
        lBrapp    WITH (lcBrand = 'Yes'),;
        lEmbapp   WITH (lcEmbr  = 'Yes')
lcClosKey = gcBmpHome + "Close2.BMP"
SHOW GET PBCLOS,1 PROMPT lcClosKey
SHOW GET PblSav DISABLE
*C102573,1 (End)

        
        
*--save in memo field From mfileloc1 TO mfileloc4
=lfUpdatMem('1')
=lfUpdatMem('2')
=lfUpdatMem('3')
=lfUpdatMem('4')

*!**************************************************************************
*! Name      : lfSAVLABEL
*! Developer : Mohamed Shokry (MHM)
*! Date      : 01/20/2002
*! Purpose   : Save data to master files
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*C102497,1
FUNCTION lfSAVLABEL

*--Loop on temp file and update Custom And Master File 

*-- If we did not open Custom screen so check if there is any record removed 
*-- from Order lines file and remove related from custom one
IF !USED(lcTempLabl)
  IF !USED('PLINFO')
    =gfOpenFile(gcDataDir+'PLINFO', gcDataDir + 'PLINFO','SH')
  ENDIF
  IF SEEK(laData[1],'PLINFO')
    SELECT PLINFO
    SCAN REST WHILE ORDER = laData[1]
      IF !SEEK(lcOrdType+laData[1]+STR(LineNo,6),'ORDLINE')
        =RLOCK()                  && Lock the record after saving
        DELETE
        UNLOCK
      ENDIF  
    ENDSCAN
  ENDIF
  *B606309,1 TMI [Start] Update audit trail fields with editors data
  IF laScrMode[3] 
    SELECT ORDLINE  
    lcOldOrdr = ORDER()
    SET ORDER TO ORDLINE
    =SEEK(lcOrdType+laData[1],'ORDLINE')
    SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = lcOrdType+laData[1]
      REPLACE ORDLINE.lMod      WITH  .T.,;
              ORDLINE.cMod_user WITH  gcUser_id,;
              ORDLINE.cMod_Date WITH  gdSysDate
    ENDSCAN
    SET ORDER TO &lcOldOrdr
  ENDIF
  *B606309,1 TMI [End  ] Update audit trail fields with editors data
  RETURN
ENDIF

SELECT (lcTempLabl)
REPLACE ALL ORDER WITH laData[1]

SELECT ORDLINE
lcOldOrdr = SET('ORDER')
SET ORDER TO ORDLINE
=SEEK(lcOrdType+laData[1])
SCAN REST WHILE cOrdType + ORDER = lcOrdType + laData[1]
  IF SEEK(ORDER + STR(LineNo,6),lcTempLabl)
    *--update flag with .T.
    REPLACE lplbl WITH .T.

    SELECT (lcTempLabl)
    SCATT MEMVAR MEMO
    IF SEEK(ORDER + STR(LineNo,6),'PLINFO')
      SELECT PLINFO
      GATHER MEMVAR MEMO
    ELSE
      SELECT PLINFO
      APPEND BLANK
      GATHER MEMVAR MEMO
    ENDIF
  ELSE
    IF SEEK(ORDLINE.ORDER + STR(ORDLINE.LineNo,6),'PLINFO')
      SELECT PLINFO
      =RLOCK()                  && Lock the record after saving
      DELETE
      UNLOCK
    ENDIF
    REPLACE ORDLINE.lplbl WITH .F.
  ENDIF

  *-102497,3 add new fields for modified records
  *B606309,1 TMI [Start] Update audit trail fields with editors data
  *IF laScrMode[3] AND ORDLINE.lplbl
  IF laScrMode[3]
  *B606309,1 TMI [End  ] Update audit trail fields with editors data
    REPLACE ORDLINE.lMod      WITH  .T.,;
            ORDLINE.cMod_user WITH  gcUser_id,;
            ORDLINE.cMod_Date WITH  gdSysDate
  ENDIF
  *-102497,3
  
ENDSCAN

*--to check for deleted record from remove butten
IF SEEK(laData[1],'PLINFO')
  SELECT PLINFO
  SCAN REST WHILE ORDER = laData[1]
    IF !SEEK(lcOrdType+laData[1],'ORDLINE')
      =RLOCK()                  && Lock the record after saving
      DELETE
      UNLOCK
    ENDIF  
  ENDSCAN
ENDIF

SELECT ORDLINE
SET ORDER TO &lcOldOrdr

*!*************************************************************
*! Name      : lfvLabDel
*! Developer : Mohame Shokry(MHM)
*! Date      : 01/22/2002
*! Purpose   : Delete current record
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvLabDel()
*!*************************************************************
*C102497,1
FUNCTION lfvLabDel
PRIVATE lnCurrRec
SELECT (lcTempLabl)

*--to validate only deleted record which we are stay on
IF ORDER <> ladata[1] OR LineNo <> &lcOrdLine..LineNo
  RETURN
ENDIF

lnCurrRec = RECNO()
LOCATE

IF EOF()
  RETURN
ENDIF

GOTO lnCurrRec
DELE 
laLabel1[lnLabel1,2] = ''
laLabel2[lnLabel2,2] = ''
laLabel3[lnLabel3,2] = ''
laLabel4[lnLabel4,2] = ''

laColour1[lnColour1,2] = ''
laColour2[lnColour2,2] = ''
laColour3[lnColour3,2] = ''
laColour4[lnColour4,2] = ''

lcFileNo11 = ''
lcFileNo22 = ''
lcFileNo33 = ''
lcFileNo44 = ''

lcFilDesc1 = ''
lcFilDesc2 = ''
lcFilDesc3 = ''
lcFilDesc4 = ''
*-102497,3
SHOW GETS
FOR J = 1 TO 4
  lcCurrObj = STR(J,1)
  FOR I = 1 TO ALEN(laTarget&lcCurrObj)
    laTarget&lcCurrObj[I] =  ''
  ENDFOR  
  STORE .T. TO llFilLoc&lcCurrObj
ENDFOR  
=lfwInit()
*-102497,3

*!*************************************************************
*! Name      : lplTab
*! Developer : Mohame Shokry(MHM)
*! Date      : 01/22/2002
*! Purpose   : Control trapping
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lplTab()
*!*************************************************************
*C102497,1
PROCEDURE lplTab

IF _CUROBJ = OBJNUM(PbClos)
   _CUROBJ = OBJNUM(lnLabel1)
ELSE
  _CUROBJ = _CUROBJ + 1
ENDIF

*!*************************************************************
*! Name      : lfUpdatMem
*! Developer : Mohame Shokry(MHM)
*! Date      : 01/22/2002
*! Purpose   : update memo field
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfUpdatMem()
*!*************************************************************
*C102497,1
FUNCTION lfUpdatMem
PARAMETER lcMemoOrd


FOR I = 1 TO ALEN(laTarget&lcMemoOrd)
  *--if first time then remove previous memo
  IF I = 1 
    IF !EMPTY(laTarget&lcMemoOrd[1])
      REPLACE &lcTempLabl..mfileloc&lcMemoOrd WITH gcDef_Path + laTarget&lcMemoOrd[1] + CHR(13)
    ELSE
      REPLACE &lcTempLabl..mfileloc&lcMemoOrd WITH '' + CHR(13)
    ENDIF  
  ELSE
    IF !EMPTY(laTarget&lcMemoOrd[1])
      REPLACE &lcTempLabl..mfileloc&lcMemoOrd WITH gcDef_Path + laTarget&lcMemoOrd[I] + CHR(13) ADDI
    ENDIF  
  ENDIF  
ENDFOR  

*!*************************************************************
*! Name      : lfERASETMP
*! Developer : Mohamed Shokry (MHM)
*! Date      : 01/20/2002
*! Purpose   : Erase temp file 
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Called Form        : 
*!*************************************************************
*! Example            : =lfERASETMP()
*!*************************************************************
*!102497,3
FUNCTION lfERASETMP
IF FILE (gcWorkDir+lcTempLabl+".DBF")
  IF USED(lcTempLabl)
    USE IN (lcTempLabl)
  ENDIF
  ERASE (gcWorkDir+lcTempLabl+".DBF")
  ERASE (gcWorkDir+lcTempLabl+".CDX")
ENDIF  

*C102564,1 BWA 04/24/2002 Add 3 fields to let know if the style has change or not.[START]
*!*************************************************************
*! Name      : LFCHNGEFCT
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 04/24/2002
*! Purpose   : To update the fields that check that for the modifying.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns   : NONE
*!*************************************************************
*! Called Form        : 
*!*************************************************************
*! Example   : =LFCHNGEFCT()
*!*************************************************************
FUNCTION LFCHNGEFCT

*--LEN(lcchangstr) > 10 In case the len greater than 10 that means there is fields changed in the screen.
IF SEEK( LADATA[1] , 'STYLE') AND LEN(lcchangstr) > 10

  SCAN REST WHILE STYLE = LADATA[1]
    REPLACE STYLE.lMod      WITH .T.       ,;
            STYLE.cMod_user WITH gcUser_id ,;
            STYLE.cMod_Date WITH gdSysDate
  ENDSCAN

  *--Update the user fields with the value of the date and the user and the flage
  =lfUpdUsrFl('LMOD'      , .T.       )
  =lfUpdUsrFl('CMOD_USER' , gcUser_id )
  =lfUpdUsrFl('CMOD_DATE' , gdSysDate )

ENDIF
GOTO lnSvStyRec

*--End of LFCHNGEFCT.
*!*************************************************************
*! Name      : lfUpdUsrFl
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 04/24/2002
*! Purpose   : Updating User defined fields.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns   : NONE
*!*************************************************************
*! Called Form        : 
*!*************************************************************
*! Example   : =lfUpdUsrFl()
*!*************************************************************
FUNCTION lfUpdUsrFl
PARAMETERS lcFldName,lcFldValue

lnFldPos = ASUBSCRIPT(laUsrFields,ASCAN(laUsrFields,lcFldName),1)
laUsrFields[lnFldPos,6] = lcFldValue

*--End of lfUpdUsrFl.
*C102564,1 BWA 04/24/2002.[END]


*:**************************************************************************
*:* Name        : lfAuditTrl  
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/05/2002
*:* Purpose     : Update the fields detecting last user Modified in the customer file
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfAuditTrl()
*:* Refer to    : C102565
*:***************************************************************************
FUNCTION lfAuditTrl
PRIVATE lcAlias
lcAlias = ALIAS()

=lfFllUsr('LMOD      ',.T.)
=lfFllUsr('CMOD_DATE ',gdSysDate)
=lfFllUsr('CMOD_USER ',gcUser_ID)

SELECT CUSTOMER
IF CUSTOMER.TYPE = 'M'
  *--If the customer No. is updated then update all stores
  =SEEK('S'+laData[2],'CUSTOMER')
  SCAN REST WHILE TYPE+ACCOUNT+STORE = 'S'+laData[2]
    REPLACE LMOD      WITH .T.       ,;
            CMOD_USER WITH gcUser_ID ,;
            CMOD_DATE WITH gdSysDate
  ENDSCAN
  =SEEK('M'+laData[2],'CUSTOMER')
ENDIF      

SELECT (lcAlias)
*-- end of lfAuditTrl  .

*:**************************************************************************
*:* Name        : lfFllUsr
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/07/02
*:* Purpose     : Update user defined fields with the needed value
*:***************************************************************************
*:* Called from : lfAuditTrl()
*:***************************************************************************
*:* Parameters : lcField : field to fill
*:*              lcValue : Value to fill with 
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfFllUsr()
*:* Refer to    : C102565,1 
*:***************************************************************************
FUNCTION lfFllUsr
PARAMETERS lcField,lcValue
PRIVATE lnPos
lnPos = ASCAN(laUsrFields, PADR(lcField,10) )
IF lnPos > 0
  lnRow = ASUBSCRIPT(laUsrFields , lnPos , 1 )
  IF TYPE('lcValue') = laUsrFields[lnRow,3]
    laUsrFields[lnRow,6] = lcValue
  ENDIF
ENDIF
*-- end of lfFllUsr.


*wab
*!**************************************************************************
*! Name      : lfJLOrdSav
*! Developer : Walid A. Wahab (WAB)
*! Date      : 02/10/2002
*! Purpose   : Save data to BomVar & Custom Thread file. 
*! Reference : C102231,1 ---- WAB ( C#102278)
*!**************************************************************************
*! Example   : lfJLOrdSav()
*!**************************************************************************
*
FUNCTION lfJLOrdSav

SELECT (lcT_BomVar)
LOCATE
*IF !EOF() AND (USED('ThreadCl') OR (laScrMode[4] AND !EMPTY(laData[43])))
IF RECCOUNT() > 0  
  *-- Open the thread color file.
  *--Change index to be on Serial instead of Thread Color.

  llOpnThClr = gfOpenFile(gcDataDir+'ThreadCl',gcDataDir+'ThreadClS','SH')
  
  PRIVATE lcAlias , lcSetDelet , lcOrdLnTag , lcBomVrTag
  lcAlias = ALIAS()  
  lcSetDelet = SET('DELETE')
  SET DELETE OFF
  
  IF laScrMode[3]
    PRIVATE lcTmpBomV2
    lcTmpBomV2 = gfTempName()
    
    SELECT BomVar
    lcBomVrTag = TAG()
    SET ORDER TO TAG cDsgnCode

    COPY STRUCTURE TO (gcWorkDir+lcTmpBomV2) WITH CDX
    USE (gcWorkDir+lcTmpBomV2) IN 0
  ENDIF  

  SELECT (lcOrdLine)
  lcOrdLnTag = TAG()
  SET ORDER TO TAG OrdLine

  *-- If Edit mode
  IF laScrMode[3]
    SCAN
      IF DELETED()
        SELECT (lcT_BomVar)
        SCAN
          SCATTER MEMVAR
          m.cItmMsjor = &lcOrdLine..Style
          m.LineNo    = &lcOrdLine..LineNo
         
          SELECT (lcTmpBomV2)
          APPEND BLANK
          GATHER MEMVAR
          DELETE
        ENDSCAN
      ELSE
        SELECT (lcT_BomVar)
        REPLACE ALL LineNo WITH &lcOrdLine..LineNo
        SCAN
          SCATTER MEMVAR
          SELECT (lcTmpBomV2)
          APPEND BLANK
          GATHER MEMVAR
         
          IF DELETED(lcT_BomVar)
            DELETE
          ENDIF
        ENDSCAN
      ENDIF
    ENDSCAN

    SELECT (lcTmpBomV2)
    SCAN FOR !EMPTY(cDsgnCode)
      IF SEEK(cIDType+cCost_ID+STR(LineNo,6)+cDsgnCode,'BomVar')
        SELECT BomVar
        =RLOCK()                  && Lock the record after saving
        REPLACE cNameDrp1 WITH &lcTmpBomV2..cNameDrp1 , ;
                cNameDrp2 WITH &lcTmpBomV2..cNameDrp2 , ;
                cNameDrp3 WITH &lcTmpBomV2..cNameDrp3 , ;
                nStitches WITH &lcTmpBomV2..nStitches , ;
                cStylePos WITH &lcTmpBomV2..cStylePos        

        UNLOCK                      && UnLock the record after saving        
        *--Update the BOMVAR file after deleting the record. [Start] 
        IF DELETED()
          RECALL
        ENDIF
        IF DELETED(lcTmpBomV2)
          =RLOCK()                  && Lock the record after saving
          DELETE
        ENDIF    
      ELSE
        IF !DELETED()
          SCATTER MEMVAR
          SELECT BomVar
          APPEND BLANK
          GATHER MEMVAR
        ENDIF        
      ENDIF
    ENDSCAN
    
    *--Update the thread color file.   [Begin]
    SELECT ThreadCl    
    =SEEK("SO"+laData[1]) 
    DELETE REST WHILE cIDType + cCost_ID + STR(LineNo,6) + cDsgnCode + cStylePos = "SO"+laData[1]
    SELECT (lcTmpThClr)
    SCAN FOR !EMPTY(cThreadCl) .AND. !DELETED() .AND. cCost_ID = laData[1]
      *--Update the THREADCL file. [Start]
      IF lfChkBom()
        SELECT (lcTmpThClr)
        SCATTER MEMVAR MEMO
        SELECT ThreadCl
        =SEEK("SO"+laData[1]) 
        LOCATE REST WHILE cIDType + cCost_ID + STR(LineNo,6) + cDsgnCode + cStylePos = "SO"+laData[1];
                          FOR DELETED()               
        IF FOUND()
          RECALL 
        ELSE
          APPEND BLANK
        ENDIF         
        GATHER MEMVAR MEMO
      ENDIF
    ENDSCAN    

    *--Update the Pick ticket type. [Begin]
    PRIVATE llDsgExist
    llDsgExist = .F.
    IF SEEK("SO"+laData[1],'BomVar')
      SELECT BomVar
      SCAN REST WHILE cidtype + ccost_id + STR(lineno,6) = "SO" + laData[1]
        IF !DELETED()
          llDsgExist = .T.
          EXIT
        ENDIF  
      ENDSCAN
      
      PRIVATE llOpnPikTk
      llOpnPikTk = gfOpenFile(gcDataDir+'PikTkt',gcDataDir+'OrdPik','SH')

      SELECT PikTkt
      IF SEEK(laData[1])
        SCAN REST WHILE Order + PikTkt = laData[1]
          REPLACE cPickType WITH IIF(llDsgExist,"A","B")
        ENDSCAN
      ENDIF
      IF llOpnPikTk
        USE IN PikTkt
      ENDIF      
    ENDIF

  ELSE           && Add mode laScrMode[4].
    SCAN
      IF !DELETED()
        SELECT (lcT_BomVar)
        REPLACE ALL LineNo WITH &lcOrdLine..LineNo , ;
                   cCost_ID WITH laData[1]
        SELECT BomVar
        APPEND FROM (gcWorkDir + lcT_BomVar) FOR !DELETED() AND !EMPTY(cDsgnCode)
      ENDIF
    ENDSCAN
    
    SELECT (lcTmpThClr)  
    REPLACE ALL cCost_ID WITH laData[1]
    
    *---Add only undeleted records. [Begin]
    SCAN FOR !DELETED() AND !EMPTY(cThreadCl)
      SCATTER MEMVAR MEMO
      INSERT INTO ThreadCl FROM MEMVAR
    ENDSCAN
    
  ENDIF
  *-- Endif of Edit mode

  SELECT (lcOrdLine)
  SET ORDER TO TAG (lcOrdLnTag)

  IF laScrMode[3]
    SELECT BomVar
    SET ORDER TO TAG (lcBomVrTag)
  ENDIF  

  IF USED('BomVar2')
    USE IN BomVar2
  ENDIF

  SELECT (lcTmpThClr)
  ZAP
  
  SET DELETE &lcSetDelet
  SELECT (lcAlias)    && Restore old alias.
  
ENDIF

*--Zap Temp thread color file. [Begin]
=lfJLZapTmp()

*-- End of lfJLOrdSav.

*!**************************************************************************
*! Name      : lfJLAddBar
*! Developer : Walid A. Wahab (WAB)
*! Date      : 02/10/2002
*! Purpose   : Add a new custom Bar to option's pad in Sales Order screen.
*! Reference : C102231,1 ---- WAB ( C#102278)
*!**************************************************************************
*! Example   : lfJLOrdSav()
*!**************************************************************************
*
FUNCTION lfJLAddBar
DEFINE BAR 17 OF _INQURYPOP PROMPT '\<Design Information Screen' SKIP FOR laScrMode[1] OR EMPTY(laData[2]) OR !llBomVarnt

*-- End of lfJLAddBar.

*!**************************************************************************
*! Name      : lfRunJLInf
*! Developer : Walid A. Wahab (WAB)
*! Date      : 02/10/2002
*! Purpose   : Run custom Design Info screen from Option's pad in Sales Order Screen.
*! Reference : C102231,1 ---- WAB ( C#102278)
*!**************************************************************************
*! Example   : lfRunJLInf()
*!**************************************************************************
*
FUNCTION lfRunJLInf
PRIVATE lcOrdAlias

lcOrdAlias = ALIAS()
  
*-- Clear the trapped keys.
PUSH KEY
ON KEY

IF BAR() = 17 OR BAR() = 18
  PUSH MENU _MSYSMENU
  RELEASE PAD _INQUIRY OF _MSYSMENU

  DO CASE
    CASE BAR() = 17
      DO (gcAppHome+'SOJLINF.FXP') WITH "SO" ,laData[1] ,m.LineNo ,m.Style ,laData[2] ,IIF(llMultiSt,SPACE(8),m.Store) ,lcT_BomVar ,lnActFolder      
      SHOW GET pbConfig,1 PROMPT lcConfig ENABLE
    
  ENDCASE    

  POP MENU _MSYSMENU
ELSE
  DO (gcAppHome+'SOJLINF.FXP') WITH "SO" ,laData[1] ,m.LineNo ,m.Style ,laData[2] ,IIF(llMultiSt,SPACE(8),m.Store) ,lcT_BomVar ,lnActFolder

  SHOW GET pbConfig,1 PROMPT lcConfig ENABLE
ENDIF  
  
*-- Restore the trapped keys.
POP KEY
SELECT (lcOrdAlias)
*-- End of lfRunJLInf.

*!**************************************************************************
*! Name      : lfJLDummy
*! Developer : Walid A. Wahab (WAB)
*! Date      : 02/10/2002
*! Purpose   : Dummy function with no use. 
*! Reference : C102231,1 ---- WAB ( C#102278)
*!**************************************************************************
*! Example   : lfJLDummy()
*!**************************************************************************
*
FUNCTION lfJLDummy
RETURN .T.
*-- End of lfJLDummy


*!**************************************************************************
*! Name      : lfJLZapTmp
*! Developer : Walid A. Wahab (WAB)
*! Date      : 02/10/2002
*! Purpose   : Zap the static files. 
*! Reference : C102231,1 ---- WAB ( C#102278)
*!**************************************************************************
*! Example   : lfJLZapTmp()
*!**************************************************************************
*
FUNCTION lfJLZapTmp
PRIVATE lcAlias
lcAlias = ALIAS()

IF USED(lcTmpThClr)
  SELECT (lcTmpThClr)
  ZAP
ENDIF  

SELECT (lcAlias)    && Restore file.
*-- End of lfJLZapTmp

*!**************************************************************************
*! Name      : lfAddDesnP
*! Developer : WAB ( Walid A. Wahab )
*! Date      : 02/06/2003
*! Purpose   : add the total desing price to the selected style
*! Reference : C102231,1 ---- WAB ( C#102278)
*!**************************************************************************
*! Example   : lfJLZapTmp()
*!**************************************************************************
FUNCTION lfAddDesnP
PRIVATE lnAlias ,lnDesnPrc ,lcAccount
lnAlias   = SELECT()
SELECT IcDesign
lnDesnPrc = 0
lcAccount = laData[2]
SELECT (lcT_BomVar)
SCAN
  IF SEEK(lcAccount+m.Store+&lct_BomVar..cDsgnCode,'IcDesign')
    lnDesnPrc = lnDesnPrc + IcDesign.nDsgnPrc
  ENDIF
ENDSCAN
m.Gros_Price = m.Gros_Price + lnDesnPrc
m.Price      = ROUND(m.Gros_Price*(100-m.Disc_Pcnt)/100,2)
SELECT (lnAlias)
RETURN 


*!**************************************************************************
*! Name      : lfGettotal
*! Developer : WAB ( Walid A. Wahab )
*! Date      : 02/06/2003
*! Purpose   : get the total desing price 
*! Reference : C102231,1 ---- WAB ( C#102278)
*!**************************************************************************
*! Example   : lfGettotal()
*!**************************************************************************
FUNCTION lfGettotal
PRIVATE lnAlias 
lnAlias   = SELECT()
SELECT IcDesign
lnDesnPrc = 0
SELECT (lcT_BomVar)
*-- Amin 
lcAccount = laData[2] 
lcStore = m.Store
*-- Amin
SCAN
  IF SEEK(lcAccount+lcStore+&lct_BomVar..cDsgnCode,'IcDesign')
    lnDesnPrc = lnDesnPrc + IcDesign.nDsgnPrc
  ENDIF
ENDSCAN
RETURN 



*-- Amin 
*!**************************************************************************
*! Name      : lfChkBom
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 06/04/2002
*! Purpose   : Get the updated values from BOMVAR to update THREADCL.
*! Reference : C102231,1 ---- WAB ( C#102278)
*!**************************************************************************
*! Example   : lfChkBom()
*!**************************************************************************
FUNCTION lfChkBom
PRIVATE lnAlias,llFound

lnAlias = SELECT(0)
llFound = .F.
SELECT BOMVAR
LOCATE FOR cidtype+ccost_id+STR(lineno,6)+cdsgncode+cstylepos = ;
           &lcTmpThClr..cIDType+&lcTmpThClr..cCost_ID+STR(&lcTmpThClr..LineNo,6)+ ;
           &lcTmpThClr..cDsgnCode+&lcTmpThClr..cStylePos

llFound = FOUND()
SELECT(lnAlias)
RETURN(llFound)
*-- End of lfChkBom.

*!**************************************************************************
*! Name      : lfHndFrght
*! Developer : NADER NABIL (NNA)
*! Date      : 09/02/2004
*! Purpose   : Add the Customer.nhand_chrg Value to the Freight in Invoice SO
*! Reference : C124347,1
*!**************************************************************************
*! Example   : lfHndFrght()
*!**************************************************************************
*!C124347,1
FUNCTION lfHndFrght
PRIVATE lnOldAlis
STORE 0 TO lnOldAlis
lnOldAlis = SELECT(0)
SELECT CUSTOMER
IF SEEK('M' + laData[4])  .AND. CUSTOMER.nhand_chrg >0
  M.FREIGHT = M.FREIGHT + CUSTOMER.nhand_chrg
SHOW GET M.FREIGHT
ENDIF
SELECT(lnOldAlis)
RETURN
*-- End of lfHndFrght.