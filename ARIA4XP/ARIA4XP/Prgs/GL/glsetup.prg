*:************************************************************************
*:  Program File: ARIA4XP\PRGS\GL\GLSETUP.PRG
*:  Module      : General Ledger
*:  Desc.       : GL SETUP SCREEN
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 04/02/2012
*:  Reference   : *E303101,1 
*:************************************************************************
PARAMETERS llFromSM,lcCompId,lcCompDir,lcDumii

*- Call the screen
lcRunScx = lfGetScx("GL\GLSETUP.scx")
DO FORM (lcRunScx) WITH llFromSM,lcCompId,lcCompDir,lcDumii

************************************************************
*! Name      : lfGetScx
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/02/2012
*! Purpose   : Get the scx path to run in SaaS environemt
************************************************************
FUNCTION lfGetScx
PARAMETERS lcScx
LOCAL lcRunScx
IF oAriaApplication.Multiinst AND FILE(oAriaApplication.clientscreenhome+lcScx)
  lcRunScx = oAriaApplication.clientscreenhome+lcScx
ELSE
  lcRunScx = oAriaApplication.screenhome+lcScx
ENDIF   
RETURN lcRunScx
 *- End of lfGetScx.

*!*************************************************************
*! Name      : lfFormInit
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 02/14/2012
*! Purpose   : called from the Screen init Method
*!*************************************************************
FUNCTION lfFormInit
PARAMETERS loFormSet,llFromSM,lcCompId,lcCompDir,lcDumii


loFormSet.Addproperty('llFromSM', IIF(TYPE('llFromSM')<>'L',.F.,llFromSM))
loFormSet.Addproperty('lcCompDir',IIF(TYPE('lcCompDir')='C',ALLTRIM(lcCompDir),' ')) && Variable to Hold the Data Dir

*- check the existence of some tables when the screen was called from SM
IF !lfCalledFromSM(loFormSet)
  RETURN .F.
ENDIF 

*- Save old set procedure
loFormSet.Addproperty('SetProc',SET("Procedure"))
*- Set functions to the APMAIN.FXP
lcPath = ADDBS(oAriaapplication.ApplicationHome)+'AP'
SET PROCEDURE TO (lcPath+'\APMAIN.FXP') ADDITIVE 

*- Open tables 
*GLSETUP,GLACCHAR,GLBATCH,FISHD,GLBUDHD,ACCOD,SYDFIELD|GLSETUP,ACCTCODE,BATCHNO,COMPFYEAR,BDCODYR,ACCSEGNO,CFLD_NAME
=lfOpenPRGFILES('GLSETUP')

*** Load program base file 
=lfAddProp(loFormSet,'lcBaseFile',ALLTRIM(sydObjct.cBaseFile))

*- Define variables 
=lfDefineVars(loFormSet)

*- Hide exchange rate for single currency's company
IF !gfGetMemVar('LLMULCURR')
  WITH loFormSet.Ariaform1
  .laData19.Visible = .F. 
  .lblExchRateColon.Visible = .F.
  .lblExchRate.Visible = .F.
  ENDWITH 
ENDIF 


*- End of lfFormInit

************************************************************
*! Name      : lfDefineVars
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 02/15/2012
*! Purpose   : Define variables as properties
************************************************************
FUNCTION lfDefineVars
PARAMETERS loFormSet

=lfAddProp(loFormSet,'ap1',CREATEOBJECT('ap'))

*substr(lcScfields , at(',',','+lcScfields,21))
lcScFields = 'CSETRETMJ,CSETSUSMJ,CSETDEBUD,LSETPBBBD,LSETCNTOT,LSETLOKPD,LSETACATE,LSETALBBE,DSETBBDAT,LSETCON,'+;
             'LSETFOREL,LSETFORAT,LSETALADD,NSETBATPW,CSETPCNT,LSETSINTN,LSETDON,NSETCOSTC,CSETEXMJ'
=lfAddProp(loFormSet,'lcScFields',lcScFields )

*!*	lcVars = 'lcArrow1,lcArrow2,lcArrow3,lcArrow4,lcArrow5,lcArrow6'
*!*	=lfAddProp(loFormSet,lcVars,' ')

*- Define laObjdisp
=lfAddProp(loFormSet,'laObjdisp[6]',.F.)

*- Get the values of laSegSize
DIMENSION laSegSize[1,3]
laSegSize = ''
SELECT ACCOD.NACSSEGNO,ACCOD.NACSSIZE,;
       ACCOD.CACSSHDES;
 FROM  ACCOD;
 WHERE NACSSEGNO <> 0  ;
 INTO  ARRAY laSegSize
=lfAddProp(loFormSet,'laSegSize[1]','')
DIMENSION loFormSet.laSegSize[ALEN(laSegSize,1),ALEN(laSegSize,2)]
=ACOPY(laSegSize,loFormSet.laSegSize)

=lfAddProp(loFormSet,'lnTotlen,lnXSize',0)    && Variable to Hold the X size of laData[1],laData[2] in the screen
=lfAddProp(loFormSet,'lnFrsSegSz', laSegSize[1,2] )

IF ALEN(laSegSize) > 1 .AND. !EMPTY(laSegSize[1,1])
  *** Calculate seg. postions & sizes. ***
  FOR lnCount = 1 TO ALEN(laSegSize,1)
    loFormSet.lnTotlen  = loFormSet.lnTotlen  + laSegSize [lnCount,1] + 4
    loFormSet.laObjdisp[lnCount]  = .T.
  ENDFOR
  loFormSet.lnXSize   = 15     && Variable to Hold the X size of laData[1],laData[2] in the screen
  loFormSet.lnFrsSegSz= laSegSize[1,2]
ENDIF

SELECT GLSETUP
GO TOP

SCATTER FIELDS &lcScFields TO laData

=lfAddProp(loFormSet,'laData[1]','')
DIMENSION loFormSet.laData[ALEN(laData)]
=ACOPY(laData,loFormSet.laData)

LOCAL i,k,lnLast
WITH loFormSet.Ariaform1
  FOR i = 1 TO ALEN(laData)
    k = ALLTRIM(STR(i))
    IF PADL(k,2) $ '17|18'
      LOOP
    ENDIF 
    
    IF PADL(k,2) $ ' 1| 2| 3|19'
      .laData&k..Keytextbox.ControlSource = 'Thisformset.laData[&k]'   &&, see the definition in GLSETUP3 , copy it and enable it to work
    ELSE
      .laData&k..ControlSource = 'Thisformset.laData[&k]'
    ENDIF 
  ENDFOR 
  
  FOR i = 1 TO 6
    k = ALLTRIM(STR(i))
    IF TYPE('laSegSize[i,3]')='C' AND !EMPTY(laSegSize[i,3])
      .lblCstCntr&k..Caption =laSegSize[i,3]
      .lblCstCntr&k..Visible = .T.
      lnLast = i
    ENDIF 
  ENDFOR 
ENDWITH 

WITH loFormSet.ariaform1
  .laData1.Keytextbox.InputMask = "X"+REPLICATE("9",loFormSet.lnFrsSegSz-1)
  .laData2.Keytextbox.InputMask = "X"+REPLICATE("9",loFormSet.lnFrsSegSz-1)
  .laData19.Keytextbox.InputMask = "X"+REPLICATE("9",loFormSet.lnFrsSegSz-1)
ENDWITH   

=lfAddProp(loFormSet,'lcDatPer','P')

=lfAddProp(loFormSet,'lcAccMask',' ')    && Variable to Hold the Account Mask
=lfAddProp(loFormSet,'lnAcsNoSeg',0) 

=lfAddProp(loFormSet,'llBatType,llOpenBy,llOpenSeg,llOpnAccCd', .F. ) 

SELECT GLBATCH
lcSavOrder = SET('ORDER')
SET ORDER TO BATTYPE
IF SEEK('B','GLBATCH')
  llBatType = .T.
ELSE
  llBatType = .F.
ENDIF  
SET ORDER TO &lcSavOrder
loFormSet.llBatType = llBatType

SELECT ACCOD
IF !EOF() 
  loFormSet.lcAccMask  = cAcsMask
  lnAcsNoSeg = nAcsNoSeg
  SKIP 1
  lnXSize = nAcsSize
ELSE
  lnXSize = 15
ENDIF
loFormSet.lnXSize = lnXSize

*SELECT GLSETUP
*SCATTER FIELDS &lcScFields TO laData 
WITH loFormSet
IF !laData[17]  &&-- Module has not setuped yet .
  laData[4]  = .F.  
  laData[5]  = .T.
  laData[6]  = .T.
  laData[7]  = .T.
  laData[8]  = .T.
  laData[9]  = IIF(EMPTY(.ap1.ldFisBgDat),{},.ap1.ldFisBgDat)
  laData[10] = .F.
  laData[11] = .F.
  laData[12] = .F.
  laData[13] = .T.
  laData[14] = 1
  laData[16] = .T.
  laData[15] = 'P'
  *lcDatPer   = laTDatePer[1,1]
  *lcDatPer   = 'P'
  *puDateper  = 1
  laData[18] = 1
  *loFormSet.lcArrow1   = ''
  =ACOPY(laData,.laData)
ELSE
  *puDatePer  = IIF(AT(ALLTRIM(laData[15]),"PD")>0,AT(ALLTRIM(laData[15]),"PD"),1)
  *lcDatPer   = laTDatePer[puDatePer,1]
  *lcDatPer   = IIF(AT(ALLTRIM(laData[15]),"PD")>0,laData[15],"P")
  *=lfPointer() 
ENDIF  

IF .laData[18]>0
  
  *- Set the cost centers positions
  lblW = .ariaform1.lblCstCntr1.Width
  lnPos = .ariaform1.Width/2 - lblW/2 - (lnLast-1)*(lblW/2)
  FOR i = 1 TO lnLast
    k = ALLTRIM(STR(i))
    Im = .ariaform1.Image&k.
    Lbl = .ariaform1.lblCstCntr&k. 
    STORE lnPos TO  Im.Left,Lbl.Left
    lnPos = lnPos + Lbl.Width
    IF i = .laData[18]
      Im.Visible = .T.
    ENDIF 
  ENDFOR  
ENDIF 
 
ENDWITH 


*- End of lfDefineVars.

************************************************************
*! Name      : lfCalledFromSM
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/02/2012
*! Purpose   : check the existence of some tables when the screen was called from SM
************************************************************
FUNCTION lfCalledFromSM
PARAMETERS loFormSet
llFromSM = loFormSet.llFromSM
lcCompDir = loFormSet.lcCompDir
glQuitting = .F.

IF llFromSM .AND. !EMPTY(lcCompDir)

  IF !FILE(lcCompDir+'GLSEGVAL.DBF')
    =gfModalGen("TRM00085B00000","Dialog")      
    glQuitting = .T.
  ENDIF  
  IF !FILE(lcCompDir+'GLSETUP.DBF')
    =gfModalGen("TRM00085B00000","Dialog")      
    glQuitting = .T.
  ENDIF  

  IF !FILE(lcCompDir+'GLBATCH.DBF')
    =gfModalGen("TRM00085B00000","Dialog")      
    glQuitting = .T.
  ENDIF

  IF !FILE(lcCompDir+'GLACCHAR.DBF')
    =gfModalGen("TRM00085B00000","Dialog")      
    glQuitting = .T.
  ENDIF  
ENDIF

RETURN !glQuitting
*- End of lfCalledFromSM.

*!**************************************************************************
*!
*!      Procedure: lpShow
*!
*!**************************************************************************
*
PROCEDURE lpShow
PARAMETERS loFormSet
*E300692,1 Change file name from SYCACCOD to ACCOD  
*SELECT SYCACCOD
*IF !SEEK(gcAct_Comp,"SYCACCOD")
SELECT ACCOD

*B802198,1 Since ACCOD became a data file , there is no field called [Begin]
*          comp_id any more so the following IF condition must be modified .
*IF !SEEK(gcAct_Comp,"ACCOD")
**E300692,1 end
WITH loFormSet.Ariaform1
GO TOP
STORE !EOF() TO .laData1.Enabled,;
               .laData2.Enabled,;
               .laData3.Enabled,;
               .laData19.Enabled

.laData9.Enabled = !loFormSet.llBatType
IF loFormSet.laData[14] = 1
    loFormSet.laData[15] = 'P'
    *Old : puDateper  = 1
    *Old : SHOW GET puDateper ENABLE
ELSE
    *Old : SHOW GET puDateper ENABLE
ENDIF  
.laData15.Enabled = .T.


*Old : IF laData[17]
*Old :   SHOW GET laData[10] DISABLE
*Old : ELSE
*Old :   SHOW GET laData[10] ENABLE
*Old : ENDIF
.laData10.Enabled = !loFormSet.laData[17]
ENDWITH 

*!**************************************************************************
*!
*!      Function: lfvData_1
*!
*!**************************************************************************
*
FUNCTION lfvData_1
PARAMETERS loFormSet,loFld

llRet = .T.

* Old : lcRetain = ALLTRIM(laData[1])+STRTRAN(SUBSTR(lcAccMask,AT("-",lcAccMask)),"#","0")
lcAccMask = loFormSet.lcAccMask
lnFrsSegSz = loFormSet.lnFrsSegSz

lcRetain = PADR(ALLTRIM(loFld.KeyTextBox.Value),lnFrsSegSz)+STRTRAN(SUBSTR(lcAccMask,AT("-",lcAccMask)),"#","0")
*IF llBrowse .OR. (!EMPTY(laData[1]) .AND. lcData1 <> laData[1])
llBrowse = loFld.Selectedfrombrowse 
IF llBrowse .OR. (!EMPTY(loFld.KeyTextBox.Value) .AND. loFld.KeyTextBox.Value <> loFld.KeyTextBox.OldValue)

  SELECT GLACCHAR
  lcSavOrder = SET('ORDER')
  SET ORDER TO ACCTCODE

  LOCATE FOR CACCTCODE = lcRetain .AND. LEFT(cTypeCode,1) = 'Q'

  IF !FOUND() .OR. ATC('?',loFld.KeyTextBox.Value) > 0 .OR. llBrowse

    LOCATE FOR LEFT(cTypeCode,1) = 'Q'

    IF FOUND()

      DIMENSION laTemp[1]
      laTemp[1] = ''
      *lcSavBrFld=lcBrfields
      *lcSavTitle=lcFile_Ttl

      lcBrfields="CACCTCODE :H= 'Account Code',;
                  CACCNLDES :H= 'Account Description',;
                  CTYPECODE :H= 'Type Code',;
                  CSTANDARD :H= 'Standard'"

      *E303101,4 TMI 08/08/2012 [Start] keep the order to be ACCTCODE
      *SET ORDER TO TAG TYPACTPOS
      SET FILTER TO CTYPECODE+CSEGACTIV+CSEGALPOS = 'Q'
      *E303101,4 TMI 08/08/2012 [End  ] 

      *E303101,4 TMI 08/08/2012 [Start] remove the Q from the browse filter expression
      *=gfbrows(["Q"FOR VAL(STRTRAN(SUBSTR(CACCTCODE,lnFrsSegSz+1),"-",""))=0],'CACCTCODE,CACCNLDES,CTYPECODE,CSTANDARD','laTemp')
      =gfbrows([FOR VAL(STRTRAN(SUBSTR(CACCTCODE,lnFrsSegSz+1),"-",""))=0],'CACCTCODE,CACCNLDES,CTYPECODE,CSTANDARD','laTemp')
      *E303101,4 TMI 08/08/2012 [End  ] 

      *lcFile_Ttl=lcSavTitle
      *lcBrfields=lcSavBrFld

      *E303101,4 TMI 08/08/2012 [Start] comment this line
      *SET ORDER TO
      SET FILTER TO 
      *E303101,4 TMI 08/08/2012 [End  ] 

      IF !EMPTY(laTemp[1])
        loFld.KeyTextBox.Value = SUBSTR(laTemp[1],1,lnFrsSegSz)
        lcRetain  = ALLTRIM(loFld.KeyTextBox.Value)+STRTRAN(SUBSTR(lcAccMask,AT("-",lcAccMask)),"#","0")
      ELSE
        loFld.KeyTextBox.Value = loFld.KeyTextBox.OldValue
        llRet = .F.
        *_CUROBJ=OBJNUM(laData[1])
      ENDIF   
    ELSE
      =gfModalGen("TRM00052B00000","Dialog")        
      loFld.KeyTextBox.Value = SPACE(lnFrsSegSz)
    ENDIF  
  ENDIF
  SET ORDER TO &lcSavOrder
ENDIF 

loFld.KeyTextBox.Value = SUBSTR(loFld.KeyTextBox.Value,1,lnFrsSegSz)

llBrowse   = .F.

*SHOW GET laData[1] ENABLE

SELECT GLSETUP
RETURN llRet

*!**************************************************************************
*!
*!      Function: lfvData_2
*!
*!**************************************************************************
*
FUNCTION lfvData_2
PARAMETERS loFormSet,loFld
llRet = .T.

lcAccMask = loFormSet.lcAccMask
lnFrsSegSz = loFormSet.lnFrsSegSz

lcSuspen = PADR(ALLTRIM(loFld.KeyTextBox.Value),lnFrsSegSz)+STRTRAN(SUBSTR(lcAccMask,AT("-",lcAccMask)),"#","0")

* Old : IF llBrowse .OR. (!EMPTY(laData[2]) .AND. lcData2 <> laData[2])
llBrowse = loFld.Selectedfrombrowse 
IF llBrowse .OR. (!EMPTY(loFld.KeyTextBox.Value) .AND. loFld.KeyTextBox.Value <> loFld.KeyTextBox.OldValue)

  SELECT GLACCHAR
  lcSavOrder = SET('ORDER')
  SET ORDER TO ACCTCODE

  LOCATE FOR CACCTCODE = lcSuspen .AND. cStandard = 'Y'

  IF !FOUND() .OR. ATC('?',loFld.KeyTextBox.Value) > 0 .OR. llBrowse

    LOCATE FOR cStandard = 'Y'

    IF FOUND()

      DIMENSION laTemp[1]
      laTemp[1] = ''
      *lcSavBrFld=lcBrfields
      *lcSavTitle=lcFile_Ttl

      lcBrfields="CACCTCODE :H= 'Account Code',;
                  CACCNLDES :H= 'Account Description',;
                  CTYPECODE :H= 'Type Code',;
                  CSTANDARD :H= 'Standard'"

      lcFile_Ttl = 'Chart of Account'

      *E303101,4 TMI 08/08/2012 [Start] keep the upper order ,i.e 
      *SET ORDER TO TAG STDACTPOS
      SET FILTER TO CSTANDARD+CSEGACTIV+CSEGALPOS = 'Y'
      *E303101,4 TMI 08/08/2012 [End  ] 

      *E303101,4 TMI 08/08/2012 [Start] remove the "Y" from the browse expression
      *=gfbrows(["Y"FOR VAL(STRTRAN(SUBSTR(CACCTCODE,lnFrsSegSz+1),"-",""))=0],'CACCTCODE,CACCNLDES,CTYPECODE,CSTANDARD','laTemp')
      =gfbrows([FOR VAL(STRTRAN(SUBSTR(CACCTCODE,lnFrsSegSz+1),"-",""))=0],'CACCTCODE,CACCNLDES,CTYPECODE,CSTANDARD','laTemp')
      *E303101,4 TMI 08/08/2012 [End  ] 

      *lcFile_Ttl=lcSavTitle
      *lcBrfields=lcSavBrFld

      *E303101,4 TMI 08/08/2012 [Start] comment this line
      *SET ORDER TO
      SET FILTER TO 
      *E303101,4 TMI 08/08/2012 [End  ] 

      IF !EMPTY(laTemp[1])
        loFld.KeyTextBox.Value = SUBSTR(laTemp[1],1,lnFrsSegSz)
        lcSuspen  = ALLTRIM(loFld.KeyTextBox.Value)+STRTRAN(SUBSTR(lcAccMask,AT("-",lcAccMask)),"#","0")
      ELSE
        loFld.KeyTextBox.Value = loFld.KeyTextBox.OldValue
        llRet = .F.
        *_CUROBJ=OBJNUM(laData[2])
      ENDIF   
    ELSE
      =gfModalGen("TRM00052B00000","Dialog")        
      loFld.KeyTextBox.Value = SPACE(lnFrsSegSz)
      *_CUROBJ=OBJNUM(laData[2])
    ENDIF  
  ENDIF
  SET ORDER TO &lcSavOrder
ENDIF

loFld.KeyTextBox.Value  = SUBSTR(loFld.KeyTextBox.Value,1,lnFrsSegSz)

llBrowse   = .F.
*SHOW GET laData[2] ENABLE

SELECT GLSETUP
RETURN llRet

*!**************************************************************************
*!
*!      Function: lfvData_3
*!
*!**************************************************************************
*
FUNCTION lfvData_3
PARAMETERS loFormSet,loFld

lcBudCod=PADR(ALLTRIM(loFld.KeyTextBox.Value),FSIZE('CBUDCODE','GLBUDHD'))
llBrowse = loFld.Selectedfrombrowse 
IF llBrowse .OR. (!EMPTY(loFld.KeyTextBox.Value) .AND. loFld.KeyTextBox.Value <> loFld.KeyTextBox.OldValue)
  SELECT GLBUDHD
  lcSavOrder = SET('ORDER')
  SET ORDER TO BDCODYR
  LOCATE FOR CBudCode = lcBudCod
  IF !FOUND() .OR. ATC('?',loFld.KeyTextBox.Value) > 0 .OR. llBrowse
      LOCATE FOR CBudCode = ALLTRIM(lcBudCod)
      DIMENSION laTemp[1]
      laTemp[1]  = ''
      *lcSavBrFld = lcBrfields
      *lcSavTitle = lcFile_Ttl
      lcBrfields = "CBudCode :H= 'Budget Code',;
                    CBudYear :H= 'Budget Year',;
                    CBudDes   :H= 'Budget Description'"
      lcFile_Ttl = 'Budget Codes'
      =gfbrows(.F.,'cBudCode,cBudYear,cBudDes','laTemp')
      *lcFile_Ttl = lcSavTitle
      *lcBrfields = lcSavBrFld
      SET ORDER TO
      IF !EMPTY(laTemp[1])
        loFld.KeyTextBox.Value = laTemp[1]
      ELSE
        loFld.KeyTextBox.Value = loFld.KeyTextBox.OldValue
      ENDIF
   ENDIF
   SET ORDER TO &lcSavOrder
ENDIF

*SHOW GET laData[3] ENABLE

SELECT GLSETUP

*!**************************************************************************
*!
*!      Function: lfvData_14
*!
*!**************************************************************************
*
FUNCTION lfvData_14
PARAMETERS loFormSet,loFld

*B600387,1  Make the range within 1-13
IF !BETWEEN(loFld.Value,1,13)
  *Message : "The batch posting window has to fall 
  *           within the range of [1 - 13]."
  *Button  : < Ok >
  =gfModalGen("TRM02244B00000","DIALOG")
  *laData[14] = lcOldBPW
  loFld.Value = loFld.OldValue  

ENDIF

*!**************************************************************************
*!
*!      Function: lfvData_15
*!
*!**************************************************************************
*
FUNCTION lfvData_15
PARAMETERS loFormSet,loFld

*Old :   IF puDateper =1
*Old :     laData[15] = 'P'
*Old :     SHOW GET puDateper ENABLE
*Old :   ELSE
*Old :     laData[15] = 'D'
*Old :     SHOW GET puDateper ENABLE
*Old :   ENDIF

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
PARAMETERS loFormSet

*MAN
WITH loFormSet
llFrstSet  = .laData[17]

.laData[17] = .T.
lnFrsSegSz = .lnFrsSegSz
lnXSize = .lnXSize
.laData[1]  = IIF(EMPTY(.laData[1]),SPACE(lnXSize),SUBSTR(.laData[1],1,lnFrsSegSz)) 
.laData[2]  = IIF(EMPTY(.laData[2]),SPACE(lnXSize),SUBSTR(.laData[2],1,lnFrsSegSz))
.laData[19] = IIF(EMPTY(.laData[19]),SPACE(lnXSize),SUBSTR(.laData[19],1,lnFrsSegSz))

SELECT GLSETUP

GO TOP
IF EOF()
  APPEND BLANK
ENDIF  
DIMENSION laData[ALEN(.laData)]
=ACOPY(.laData,laData)
lcScFields = .lcScFields
GATHER FROM laData FIELDS &lcScFields

=gfAdd_Info('GLSETUP')

FLUSH

*** Enter the default segments with zero ***
*** values except the first one...
*MAN Added IF llFrstSet to update the default seg. val. only in the first
*MAN setup
IF !llFrstSet
  SELECT GLSEGVAL
  FOR lnCount = 2 TO ALEN(.laSegSize,1)
    lcSegNo   = ALLTRIM(STR(.laSegSize[lnCount,1]))
    lcSegVal  = REPLICATE('0',.laSegSize[lnCount,2])
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
ENDWITH 

SELECT GLSETUP
=gfTableUpdate()

loFormset.Release



*!**************************************************************************
*!
*!      Function: lfvCancel
*!
*!**************************************************************************
*
FUNCTION lfvCancel
PARAMETERS loFormset
loFormset.Release

************************************************************
*! Name      : lfvCost
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/02/2012
*! Purpose   : Put a pointer on the selected segment as a cost center
************************************************************
FUNCTION lfvCost
PARAMETERS loFormSet,loFld
WITH loFormset.Ariaform1
  STORE .F. TO .Image1.Visible,;
             .Image2.Visible,;
             .Image3.Visible,;
             .Image4.Visible,;
             .Image5.Visible,;
             .Image6.Visible
  LOCAL k
  k = RIGHT(loFld.Name,1)
  .Image&k..Visible = .T.
  loFormSet.laData[18] = INT(VAL(k))
ENDWITH 
*- End of lfvCost.


*!**************************************************************************
*!
*!      FUNCTION : lfQuit
*!
*!**************************************************************************
*
FUNCTION X_lfQuit

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
*!      Function: lfvData_19
*!
*!**************************************************************************
*
FUNCTION lfvData_19
PARAMETERS loFormSet,loFld

llRet = .T.
lcAccMask = loFormSet.lcAccMask
lnFrsSegSz = loFormSet.lnFrsSegSz

lcExMaj = PADR(ALLTRIM(loFld.KeyTextBox.Value),lnFrsSegSz)+STRTRAN(SUBSTR(lcAccMask,AT("-",lcAccMask)),"#","0")

llBrowse = loFld.Selectedfrombrowse 
IF llBrowse .OR. (!EMPTY(loFld.KeyTextBox.Value) .AND. loFld.KeyTextBox.Value <> loFld.KeyTextBox.OldValue)

  SELECT GLACCHAR
  lcSavOrder = SET('ORDER')
  SET ORDER TO ACCTCODE

  LOCATE FOR CACCTCODE = lcExMaj .AND. cStandard = 'Y'

  IF !FOUND() .OR. ATC('?',loFld.KeyTextBox.Value) > 0 .OR. llBrowse

    LOCATE FOR cStandard = 'Y'

    IF FOUND()

      DIMENSION laTemp[1]
      laTemp[1] = ''
      *lcSavBrFld=lcBrfields
      *lcSavTitle=lcFile_Ttl

      lcBrfields="CACCTCODE :H= 'Account Code',;
                  CACCNLDES :H= 'Account Description',;
                  CTYPECODE :H= 'Type Code',;
                  CSTANDARD :H= 'Standard'"

      lcFile_Ttl = 'Chart of Account'

      *E303101,4 TMI 08/08/2012 [Start] keep the order to acccode , set filter instead
      *SET ORDER TO TAG STDACTPOS
      SET FILTER TO CSTANDARD+CSEGACTIV+CSEGALPOS = 'Y'
      *E303101,4 TMI 08/08/2012 [End  ] 

      *E303101,4 TMI 08/08/2012 [Start] remove the 'Y' from the browse filter expression
      *=gfbrows(["Y"FOR VAL(STRTRAN(SUBSTR(CACCTCODE,lnFrsSegSz+1),"-",""))=0],'CACCTCODE,CACCNLDES,CTYPECODE,CSTANDARD','laTemp')
      =gfbrows([FOR VAL(STRTRAN(SUBSTR(CACCTCODE,lnFrsSegSz+1),"-",""))=0],'CACCTCODE,CACCNLDES,CTYPECODE,CSTANDARD','laTemp')
      *E303101,4 TMI 08/08/2012 [End  ] 

      *lcFile_Ttl=lcSavTitle
      *lcBrfields=lcSavBrFld

      *E303101,4 TMI 08/08/2012 [Start] comment , release the filter 
      *SET ORDER TO
      SET FILTER TO 
      *E303101,4 TMI 08/08/2012 [End  ] 

      IF !EMPTY(laTemp[1])
        loFld.KeyTextBox.Value = SUBSTR(laTemp[1],1,lnFrsSegSz)
        lcExMaj    = ALLTRIM(loFld.KeyTextBox.Value)+STRTRAN(SUBSTR(lcAccMask,AT("-",lcAccMask)),"#","0")
      ELSE
        loFld.KeyTextBox.Value = loFld.KeyTextBox.OldValue
        llRet = .F.
        *_CUROBJ=OBJNUM(laData[19])
      ENDIF   
    ELSE
      =gfModalGen("TRM00052B00000","Dialog")        
      loFld.KeyTextBox.Value = SPACE(lnFrsSegSz)
      *_CUROBJ=OBJNUM(laData[19])
    ENDIF  
  ENDIF
  SET ORDER TO &lcSavOrder
ENDIF

loFld.KeyTextBox.Value  = SUBSTR(loFld.KeyTextBox.Value,1,lnFrsSegSz)

llBrowse   = .F.

*SHOW GET laData[19] ENABLE

SELECT GLSETUP

RETURN llRet
