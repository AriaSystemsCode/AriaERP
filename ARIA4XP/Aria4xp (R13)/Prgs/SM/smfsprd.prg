************************************************************
*modifications
*B610447,1 TMI 07/25/2013 fix problems in fiscal period screen
*B610472,1 TMI 08/19/2013 [Start] get the property value loFormSet.lcDataDir 
*B610691,1 TMI 03/09/2014 make sure that the periods are changed according to the new choosen # of periods [T20140210.0048]
*B610766,1 MMT 07/07/2014 Locking/Unlocking periods is not saved in fiscal year screen[T20140701.0030] 
*B612683,1 MMT 06/22/2023 Fix the error happens while opening periods screen in a new company[T-ERP-20230620.0001]
************************************************************

*B610447,1 TMI 07/22/2013 [Start] include llCloseYear parameter
*PARAMETERS loFormSet
PARAMETERS loFormSet,llCloseYear
*B610447,1 TMI 07/22/2013 [End  ] 

#INCLUDE R:\ARIA4XP\PRGS\SM\SMFSPRD.H

lcScx = lfGetScx('SM\SMFSPRD.scx')
*B610447,1 TMI 07/22/2013 [Start] include llCloseYear parameter
*DO FORM (lcScx) WITH loFormSet
DO FORM (lcScx) WITH loFormSet,llCloseYear
*B610447,1 TMI 07/22/2013 [End  ] 


************************************************************
*! Name      : lfSMFSPRDInit
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 02/14/2013
*! Purpose   : SMFSPRD init
************************************************************
FUNCTION lfSMFSPRDInit
PARAMETERS loBranFormSet
loFormSet = loBranFormSet.loFormSet

llCloseYear = IIF(TYPE('llCloseYear')='U',.F.,llCloseYear)
loBranFormSet.AddProperty('llCloseYear',llCloseYear)

*loBranFormSet.AriaForm1.txtName.Value = IIF(llCloseYear,SUBSTR(lcCompany,4,LEN(lcCompany)),lcComp)
lcCompany = loFormSet.Ariaform1.puComp.Value
WITH loBranFormSet.AriaForm1
  .txtName.Value = SUBSTR(lcCompany,4,LEN(lcCompany))
  .txtFiscalyear.Value = IIF(llCloseYear,lcFiscal,loFormSet.laData[2])
  .cmdOk.Enabled = loFormSet.ActiveMode $ 'AE'

  *N000682,1 04/16/2013 HES Globlization changes[Start]
*!*	  .Caption = 'Periods'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.Caption = LANG_Periods
.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Periods,loFormSet.GetHeaderText("LANG_Periods",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 04/16/2013 HES Globlization changes[End  ]
ENDWITH

*B610447,1 TMI 07/21/2013 [Start] get data 
*B612683,1 MMT 06/22/2023 Fix the error happens while opening periods screen in a new company[T-ERP-20230620.0001][Start]
*if !empty(loFormSet.pcComp_ID) and !used(loFormSet.lc_TempPR)
if !used(loFormSet.lc_TempPR)
*B612683,1 MMT 06/22/2023 Fix the error happens while opening periods screen in a new company[T-ERP-20230620.0001][End]
  lc_TempPR = loFormSet.lc_TempPR
  gcWorkDir = oAriaApplication.WorkDir
  *B610472,1 TMI 08/19/2013 [Start] get the property value loFormSet.lcDataDir 
  *lcDataDir = substr(oAriaApplication.DataDir,1,len(oAriaApplication.DataDir)-3)+oAriaApplication.activecompanyid+'\'
  lcDataDir = loFormSet.lcDataDir 
  IF !FILE(lcDataDir+"FSPRD.dbf")
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'You should write down the current year')
    RETURN .F.
  ENDIF 
  *B610472,1 TMI 08/19/2013 [End  ] 
  
  Dime laData[alen(loFormSet.laData)]
  =ACopy(loFormSet.laData,laData)
  
  SELECT *,RECNO() AS 'nRecNo', "S" AS 'cStatus';
      FROM (lcDataDir+"FSPRD") ;
     WHERE CFISFYEAR+CFSPPRDID = laData[2] ; 
      INTO DBF (oAriaApplication.WorkDir+loFormSet.lc_TempPR)

  SELECT(loFormSet.lc_TempPR)  
  INDEX ON CFISFYEAR+CFSPPRDID TAG lc_TempPR
  
  =lfAddPrds(laData[1],laData[2],VAL(laData[4]),VAL(laData[5]),laData[6],laData[7],"&gcWorkDir.&lc_TempPR")
  
Endif       
*B610447,1 TMI 07/21/2013 [End  ] 

*B610691,1 TMI 03/09/2014 20:46 [Start] 
*** If creation of new periods is required, in case adding fiscal year 
*** for a company or modifying existing periods
SELECT (loFormSet.lc_TempPR)
DIMENSION laData[ALEN(loFormSet.laData)]
=ACOPY(loFormSet.laData,laData)
gcWorkDir = oAriaApplication.WorkDir
IF loFormSet.llNewPrds 
  loFormSet.llNewPrds = .F.

  *** Mark old records in temp file with delete marker if collected from 
  *** the master file or with 'S' marker if was added to the same record
  REPLACE ALL cStatus WITH IIF(AT(cStatus,"ASM") > 0,;
                               SUBSTR("SDD",AT(cStatus,"ASM"),1),"S")
  DELETE ALL

  *** If creat default periods of the year is required
  IF loFormSet.llDefPrds
    loFormSet.llDefPrds = .F.
    llUpdate  = .T.          && Change update flag to accept the default

    *** Call Add periods funtion with parameters: 
    *** Company ID, Fscal year, No of periods,Currant period,Begin date,
    *** End Date,Temp file name
    
    IF llCloseYear
      *SELECT(loFormSet.lcTmpFisHD)
      *=lfAddPrds(laCompany[puCompany,2],cFisfyear,VAL(cfisnoprd),VAL(ccurr_prd),dfisbgdat,dfisendat,"&gcWorkDir.&lc_TempPR")
      *SELECT(loFormSet.lc_TempPR)
    ELSE
      =lfAddPrds(laData[1],laData[2],VAL(laData[4]),VAL(laData[5]),laData[6],laData[7],"&gcWorkDir.&lc_TempPR")
    ENDIF  
  ELSE
    FOR lnCount = 1 TO VAL(laData[4])
      
      INSERT INTO &gcWorkDir.&lc_TempPR (cFisFyear,cFspprdid,cStatus) ;
             VALUES (laData[2],RIGHT("0"+ALLTRIM(STR(lnCount)),2),'A')
      
    ENDFOR
    LOCATE 
    REPLACE DFSPPBGDT WITH loFormSet.laData[6]
    GO bottom
    REPLACE DFSPPENDT  WITH loFormSet.laData[7]
  ENDIF
ENDIF
*B610691,1 TMI 03/09/2014 21:54 [End  ] 

*- set the grid
=lfSetGridDataSource(loBranFormSet)

RETURN
*- End of lfSMFSPRDInit.


************************************************************
*! Name      : lfSetGridDataSource
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 22/05/2012
*! Purpose   : Set the Grid Data Source
************************************************************
FUNCTION lfSetGridDataSource
PARAMETERS loBRANFormSet
loFormSet = loBRANFormSet.loFormSet
oGrd = loBRANFormSet.Ariaform1.grdSMFSPRD
WITH oGrd
  .RecordSource = ''
  .RecordSource = loFormSet.lc_TempPR
ENDWITH

SELECT (loFormSet.lc_TempPR)
CURSORSETPROP("Buffering",5)
LOCATE

lc_TempPR = loFormSet.lc_TempPR

*N000682,1 04/16/2013 HES Globlization changes[Start]
*!*	lfSetColumnsProp('1',"&lc_TempPR..CFSPPRDID",'Prd'          ,30 ,oGrd)
*!*	lfSetColumnsProp('2',"&lc_TempPR..CFSPPDESC"  ,"Description"  ,150,oGrd)
*!*	lfSetColumnsProp('3',"&lc_TempPR..DFSPPBGDT","Begin Date"   ,90 ,oGrd)
*!*	lfSetColumnsProp('4',"&lc_TempPR..DFSPPENDT","End Date"     ,90 ,oGrd)
*!*	lfSetColumnsProp('5',"&lc_TempPR..DFSPPENDT - &lc_TempPR..DFSPPBGDT + 1",'Days'         ,40 ,oGrd)
*!*	lfSetColumnsProp('6',"&lc_TempPR..NFSPPARTN"  ,"Part"         ,50 ,oGrd)
*!*	lfSetColumnsProp('7',"&lc_TempPR..LFSPLOCKS","Locked"       ,50 ,oGrd)
*!*	lfSetColumnsProp('8',"&lc_TempPR..LFSPCLSDS","Closed"       ,50 ,oGrd)
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lfSetColumnsProp('1',"&lc_TempPR..CFSPPRDID",LANG_Priod           ,30 ,oGrd)
lfSetColumnsProp('1',"&lc_TempPR..CFSPPRDID",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Priod,loFormSet.GetHeaderText("LANG_Priod",loFormSet.HeaderAlias))           ,30 ,oGrd)
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lfSetColumnsProp('2',"&lc_TempPR..CFSPPDESC"  ,LANG_Description ,150,oGrd)
lfSetColumnsProp('2',"&lc_TempPR..CFSPPDESC"  ,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Description,loFormSet.GetHeaderText("LANG_Description",loFormSet.HeaderAlias)) ,150,oGrd)
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lfSetColumnsProp('3',"&lc_TempPR..DFSPPBGDT",LANG_Begin_Date    ,90 ,oGrd)
lfSetColumnsProp('3',"&lc_TempPR..DFSPPBGDT",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Begin_Date,loFormSet.GetHeaderText("LANG_Begin_Date",loFormSet.HeaderAlias))    ,90 ,oGrd)
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lfSetColumnsProp('4',"&lc_TempPR..DFSPPENDT",LANG_End_Date  ,90 ,oGrd)
lfSetColumnsProp('4',"&lc_TempPR..DFSPPENDT",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_End_Date,loFormSet.GetHeaderText("LANG_End_Date",loFormSet.HeaderAlias))  ,90 ,oGrd)
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lfSetColumnsProp('5',"&lc_TempPR..DFSPPENDT - &lc_TempPR..DFSPPBGDT + 1",LANG_Days   ,40 ,oGrd)
lfSetColumnsProp('5',"&lc_TempPR..DFSPPENDT - &lc_TempPR..DFSPPBGDT + 1",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Days,loFormSet.GetHeaderText("LANG_Days",loFormSet.HeaderAlias))   ,40 ,oGrd)
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lfSetColumnsProp('6',"&lc_TempPR..NFSPPARTN"  ,LANG_Part          ,50 ,oGrd)
lfSetColumnsProp('6',"&lc_TempPR..NFSPPARTN"  ,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Part,loFormSet.GetHeaderText("LANG_Part",loFormSet.HeaderAlias))          ,50 ,oGrd)
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lfSetColumnsProp('7',"&lc_TempPR..LFSPLOCKS",LANG_Locked     ,50 ,oGrd)
lfSetColumnsProp('7',"&lc_TempPR..LFSPLOCKS",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Locked,loFormSet.GetHeaderText("LANG_Locked",loFormSet.HeaderAlias))     ,50 ,oGrd)
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lfSetColumnsProp('8',"&lc_TempPR..LFSPCLSDS",LANG_closed     ,50 ,oGrd)
lfSetColumnsProp('8',"&lc_TempPR..LFSPCLSDS",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_closed,loFormSet.GetHeaderText("LANG_closed",loFormSet.HeaderAlias))     ,50 ,oGrd)
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 HES Globlization changes[End  ]
*- End of lfSetGridDataSource.

************************************************************
*! Name      : lfSetColumnsProp
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 05/23/2012
*! Purpose   : Set Columns Properties
************************************************************
FUNCTION lfSetColumnsProp
PARAMETERS lcCol,lcSrc,lcHeader,lnWidth,loGrd
lnWidth = IIF(EMPTY(lnWidth),50,lnWidth)
WITH loGrd
  .Column&lcCol..Header1.Caption = lcHeader
  .Column&lcCol..ControlSource   = lcSrc
  .Column&lcCol..Width           = lnWidth
ENDWITH
*- End of lfSetColumnsProp.
************************************************************
*! Name      : lfUpcStatus
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 02/24/2013
*! Purpose   : update the cStatus field
************************************************************
FUNCTION lfUpcStatus
PARAMETERS loFld
*B610766,1 MMT 07/07/2014 Locking/Unlocking periods is not saved in fiscal year screen[T20140701.0030][Start]
*IF loFld.Value <> loFld.OldValue
*B610766,1 MMT 07/07/2014 Locking/Unlocking periods is not saved in fiscal year screen[T20140701.0030][End]
REPLACE cStatus WITH 'M'
*B610766,1 MMT 07/07/2014 Locking/Unlocking periods is not saved in fiscal year screen[T20140701.0030][Start]  
*ENDIF
*B610766,1 MMT 07/07/2014 Locking/Unlocking periods is not saved in fiscal year screen[T20140701.0030][End]  
*- End of lfUpcStatus.

*!**************************************************************************
*!
*!      Function: lfvDateRng
*!
*!**************************************************************************
* Function to validate all end date fields
*
FUNCTION lfvDateRng
PARAMETERS loBranFormSet,loFld
loFormSet = loBranFormSet.loFormSet

lc_TempPR = loFormSet.lc_TempPR
lnPeriod = VAL(&lc_TempPR..CFSPPRDID)

  DO CASE
    *** User is not permited to skip end date without adding
    CASE EMPTY(loFld.Value)
      *** Periods End Date must be enterd sequentialy..!
      =gfModalGen("TRM00078B00000","DIALOG",STR(lnPeriod))
      loFld.Value = loFld.OldValue
      RETURN .F.

    *** Periods days not less than one day
    CASE loFld.Value < loFld.parent.parent.Column3.Text1.Value
      *** End date of period ð can not ***
      *** be less than begin date ..!
      *** <  Ok  > ***
      =gfModalGen("TRM00072B00000","DIALOG",ALLTRIM(STR(lnPeriod)))
      loFld.Value = loFld.OldValue
      RETURN .F.

    *** End Date has to be within the fiscal year. ***
    CASE loFld.Value >= IIF(loBranFormSet.llCloseYear,ldFisEnd,loFormSet.laData[7])
      *** End date of period ð can not be grater ***
      *** than end date of the year ..!
      *** <  Ok  > ***
      =gfModalGen("TRM00073B00000","DIALOG",STR(lnPeriod))
      loFld.Value = loFld.OldValue
      RETURN .F.

    *** Begin Date of next period is one day affter the end date
    OTHERWISE
      IF loBranFormSet.llCloseYear
        laPriod[lnPeriod+1,5] = MIN(EVALUATE(SYS(18))+1,ldFisEnd)
      ELSE
        *laPriod[lnPeriod+1,5] = MIN(EVALUATE(SYS(18))+1,laData[7])
        ldDate = loFld.Value
        IF lnPeriod<12
          SKIP
          replace DFSPPBGDT WITH ldDate + 1
          SKIP -1
        ENDIF
      ENDIF
  ENDCASE


IF loFld.Value <> loFld.OldValue
  REPLACE cStatus WITH 'M'
ENDIF


*!**************************************************************************
*!
*!      Function: lfvPartRng
*!
*!**************************************************************************
* This function is called from all the part fields
* Parts has to be from 1 to 6
FUNCTION lfvPartRng
PARAMETERS loBranFormSet,loFld
loFormSet = loBranFormSet.loFormSet

IF loFld.Value < 1 .OR. loFld.Value > 6
  =gfModalGen("TRM00071B00000","DIALOG")
  RETURN .F.
ENDIF

IF loFld.Value <> loFld.OldValue
  REPLACE cStatus WITH 'M'
ENDIF



*!**************************************************************************
*!
*!      Function: lfvOk
*!
*!**************************************************************************
*B610447,1 TMI 07/22/2013, Add this function from the A27 program
FUNCTION lfvOk
parameters loFormSet

llCloseYear = loFormSet.llCloseYear
Dime laData[alen(loFormSet.laData)]
=Acopy(loFormSet.laData,laData)

*** All End date of periods has to be filled
FOR lnCount = 1 TO IIF(llCloseYear,VAL(lcFisNoPrd),VAL(laData[4]))
  IF EMPTY(laPriod[lnCount,5]) .OR. EMPTY(laPriod[lnCount,6])
    =gfModalGen("TRM00075B00000","DIALOG",IIF(llCloseYear,lcFiscal,laData[2]))  
    RETURN .F.
  ENDIF
ENDFOR

*** Check if all end dates is enterd with the right validation
*** If not go to the wrong date to be reenterd

FOR lnCount = 1 TO IIF(llCloseYear,VAL(lcFisNoPrd),VAL(laData[4]))
  IF laPriod[lnCount,6] < laPriod[lnCount,5]
    *** End date of period ð can not be less than begin date ..!
    =gfModalGen("TRM00072B00000","DIALOG",STR(lnCount))
    laPriod[lnCount,6] = {}
    _CUROBJ = OBJNUM(laPriod[lnCount,6])
    lcObjName = "laPriod["+ALLTRIM(STR(lnCount))+",6]"
    SHOW GET &lcObjName
    RETURN .F.
  ENDIF
ENDFOR

*** Redimention the array with right legnth so the no of records to 
*** add is same as NO. of periods
IF llCloseYear 
  DECLARE laPriod[VAL(lcFisNoPrd), ALEN(laPriod,2)] 
ELSE 
  DECLARE laPriod[VAL(laData[4]),ALEN(laPriod,2)]
ENDIF  

SELECT (lc_TempPR)

=ADEL(laPriod , 1 , 2)

FOR lnCnt = 1 TO ALEN(laPriod , 1)
  IF laPriod[lnCnt,17] = 'M'
    SELECT (lc_TempPR)
    =SEEK(laPriod[lnCnt , 1] + laPriod[lnCnt , 2])
    REPLACE cFsppDesc WITH laPriod[lnCnt , 3] ;
            dFsppEndt WITH laPriod[lnCnt , 5] ;
            nFspPartN WITH laPriod[lnCnt , 6] ;
            lFspLocks WITH laPriod[lnCnt , 7] ;
            lFspClsds WITH laPriod[lnCnt , 8] ;
            cStatus   WITH IIF(cStatus $ 'SM' , 'M' , cStatus) ;
            dFsppBGdt WITH laPriod[lnCnt , 4]
  ENDIF
ENDFOR

DELETE ALL FOR cFspprdid = " "

*** Change the accept flag so if the screen reenterd, Dont miss the data
llAcptPrd = .T.

*** This flag to say not swap the old value.
llOldFlg  = .F.

*** Change the update periods flag so in the save of home screen
*** move recoreds to master file in case of editing old recorde
llUpdPr = .T.

llPeriod = .T.

