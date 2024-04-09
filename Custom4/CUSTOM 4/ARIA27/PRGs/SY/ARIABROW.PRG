*B500675,1 Hesham El-Sheltawi 04/27/95
*B500675,1 Change the variable lnRecNo to lnBrRecNO because of the
*B500675,1 confliction between the global browse and the calling programs
*B600455,1 MAN 06/16/95 Changed Calling GETEXPR to be AGETEXPR
*B600322 Hesham El-Sheltawi 06/18/95
*B600322 if the user pressed double click with the mouse in any of the
*B600322 scroll bars "Horz.,Vert." ignore it and dont select the active
*B600322 record
*B600440,1 Hesham El-Sheltawi 06/18/95
*B600440,1 if the user press fast enter we have to select
*B600440,1 the record insted of the cancel
*B600540,1 MFM 07/05/95 Changed to return false in the case of empty file
*B600540,1              to browse from.
*B800013,1 MAN 07/23/95 Added Facility to allow the incremental search
*B800013,1              to use different index than the one used for displaying.
*B800013,1              Notes :
*B800013,1              -If ARIABROW Has been called with a KEY Expr.
*B800013,1               the Alternate index ( Used For Searching) should have the
*B800013,1               same Key otherwise this feature won't work
*B800013,1              -To use a specific Index for the incremental search that
*B800013,1               is different than the one used for displaying you have to 
*B800013,1               define Variable lcSeekTag with the required Tag ID
*B800031,1 MAN Added Saving the the current record no. before the browse because
*B800031,1     the browse changes the record position if both clauses KEY and FOR
*B800031,1     are included
*B800038,1 Hesham El-Sheltawi 07/27/95
*B800038,1 Adding clearing for the keyboard buffer
*B600683,1 MFM 09/25/95 Fixed the bug when pressing tab more than once.
*B600689,1 MFM 09/26/95 Fixed the bug (Do Nesting to deep) on left arow.
*B800296,1 MFM 09/26/95 Changed the way of the incremental search.
*B600769,1 MFM 09/26/95 Fixed the bug (Do Nesting to deep).
*B600933,1 Hesham El-Sheltawi Checking for the read cycle level in the incrementel
*B600933,1 search for ignoring the too many reads error
*B600822,1 Hesham El-Sheltawi 07/21/96
*B600822,1 Create prefrence for each browse for any file according
*B600822,1 to the name of the function that is browsing the file
*B600822,1 and the file name
*B800824,1 TMI 11/11/96 Commented out the "SET RESORCE ON" to allow to the
*B800824,1              same User_Id to open the same browse in other machine
*B800824,1              without chicking if the current User_Id opens this
*B800824,1              brow in this moment or not.
*B601438,1 Hesham El-Sheltawi 26/11/96
*B601438,1 in find function if there is a browse key then seek for the key 
*B601438,1 before issuing the locate command and make the locate command
*B601438,1 locate rest
*B601447,1 Hesham El-sheltawi 11/28/96
*B601447,1 if the browse screen is branching to another browse screen
*B601447,1 sometimes the browse fields change when comming back to the
*B601447,1 first session of the browse
*E300500,1 Hesham El-Sheltawi 12/15/96
*E300500,1 Make the order by active with the Indexes that match the
*E300500,1 Key passed to the function if there is any
*B601510,1 Hesham El-Sheltawi 12/25/96
*B601510,1 Make the window name that use in the "order by" variable name
*B601517,1 Hesham El-Sheltawi 12/31/96
*B601517,1 Change the condition to take care about the asc and desc 
*B601517,1 enabling and disabling
*B801116,1 Hesham El-Sheltawi 06/12/97
*B801116,1 set the resoure on so if the user was logged in another workstation
*B801116,1 the prefrences work properly
*E301050,1 Hesham 11/05/98 Select from the browse using Mouse Right Click
*E301050,1 				   instead of Double click
*E301077,72 MAB 03/07/99 if no active order store null to it.
*B602499,1  MAB 05/18/99 Fix wrong order of letters in "LOCATE" screen if there 
*B602499,1  			 are too many nested reads.
*B604857,1 ADEL 09/04/01 Add the fields user wants to browse to lcBrFields.
*B605070,1 ADEL 10/29/01 Add the user fields to the filter popup when browsing.
*B605349,1 BWA 02/06/2002 Fix the bug of "Operator/Operand Type Mismatch" If you go to the
*B605349,1 BWA 02/06/2002 WIP from Style and then add a Filter By Completion date the previous error happens
*E301853,1 WAB 04/01/2002 - add the color description to the browse fields
*E302047,1 ABD 11/10/2002 Add new variable to hold the browse font for the aria brow. 'lcBrflFont'
*B606777,1 RAE            Fix Bug  subscript out of bounds 
*B607133,1 AMH 04/08/2003 Fix Bug  subscript out of bounds 
***************************************************************************************
PARAMETER lcFltrExp,lcBrowTitle,lnY1,lnX1,lnY2,lnX2,lcOnSelect,lcBrPushB,lcFieldsNam,lcArrName,llChckKey,lcAliasName,llGetTree

*B604857,1 (Begin) Save the browse file.
lcSavAls = Alias()
*B604857,1 (End)

REGIONAL laTags,laExp,gwdbrows,gwBbrowse,lcSavArea,lnSavRec,;
        lvExpres,lcNewObj,lcFindExp,gcFiltExp,lcFiltExp,;
        llFirstTime,cbIgnore,llTagBy,llClick,llMyOpen,lnBrCurObj,;
        llOpenBy,cbWrap,cbMAtch,llEmpty,llSelBrow,lnTimelimt,;
        lnCol,lnlenth,puTags,puField_H,lnLastKey,lnCount,rbScope,;
        lnCurR,laSavStat,laHeadTmp,laNameTmp,llIsFilter,lcFltrExp,;
        lcFieldsNam,lcArrName,lcBrowTitle,lcSavTag,lnCurRec,lcWinTitl,;
        LISTWIND,lcCurFilter,laFiltTmp,lnLineRange,lcForExp,lcOldProc,laScObj,lnY

*B601447,1 Hesham El-Sheltawi (Start)
*B601447,1 add the variable lcMenuStat to the REGIONAL variables
REGIONAL lcExpToSeek,lnSeeklimt,lcBrowHost,lcBrowCnt,lcBrPushB,laCntObj,lcEnter,lcEscape,;
        lnFirstObj,lnLastObj,lcBrowWind,llActBrow,llSelcted,lcToProc,llBrowTmnat,;
        lcOnSelect,lcFltrExp,lcBrowTitle,lnY1,lnX1,lnY2,lnX2,lcBrPushB,lcFieldsNam,;
        lcArrName,llUseMenu,lcOldBrow,llBrWasFltr,lcOldEsc,laBrObjCnt,lnEach,LNPUSHWD,;
        lnSoftSeek,llUnix,laKeyExp,llChckKey,lnBrRecNO,lvSought,lnY,lnX,lcMenuStat
*B601447,1 Hesham El-Sheltawi (End)


*B600335,1 MAN 05/25/95 (Begin) Fixed the bug when you enter the filter
*B600335,1              option in the standard browse from any browse in
*B600335,1              the system.
PRIVATE LcFN,;
        lcFT,;
        lcorder

IF !EMPTY(DBF())
  FOR lnCount=1 TO FCOUNT()
    PRIVATE (FIELD(lnCount))
    lcFN = FIELD(lnCount)
    lcFT = TYPE(FIELD(lnCount))
    DO CASE
      CASE lcFT $ "MC" 
        STORE ' ' TO ('M.'+LcFN) 
      CASE lcFT = "N"
        STORE 1 TO ('M.'+LcFN)
      CASE lcFT = "D"
        STORE  DATE() TO ('M.'+LcFN) 
      OTHERWISE
        STORE  .F. TO ('M.'+LcFN) 
    ENDCASE
  ENDFOR
ENDIF  
*B600335,1 MAN 05/25/95 (End)

*E302047,1 ABD - define the font for the browse font if not define before that. [Begin]
IF TYPE('lcBrflFont') # 'C' 
  lcBrflFont = "MS Sans Serif"
ENDIF
*E302047,1 ABD - [End]

lcOldBrow = IIF(TYPE('lcBrFields')<>'C','',lcBrFields)
lcOldEsc  = SET('ESCAPE') 
llunix = _UNIX
SET ESCAPE OFF

DIMENSION laScObj[1,5],laCntObj[6,5],laKeyExp[1]


llRemark = .T. &&!llUseMenu
PUSH KEY CLEAR
llEnterP=.f.   
ON KEY LABEL ENTER   llEnterP=.t.   
ON KEY LABEL ESC     lldymy=.t.   
STORE '' TO lcForExp,lcExpToSeek,lcEnter,lcEscape,lcToProc,laKeyExp
STORE 0 TO lnSeeklimt,lnLastObj,puTags
STORE 1 TO lnFirstObj,lnBrCurObj
STORE .F. TO  llIsFilter,llActBrow,llSelcted,llBrowTmnat,llBrWasFltr


lcOnSelect = IIF(TYPE('lcOnSelect')='C',lcOnSelect,"")



****************** Define the push buttons & its prompt &the  hotkeys

**** before changing anything please look carfuly to the screen set objects

lcBrPushB  = IIF(TYPE('lcBrPushB')='C' AND !EMPTY(lcBrPushB),IIF(EMPTY(lcBrPushB),";;;;\!\<Select;\?\<Cancel",lcBrPushB),"Fi\<nd;\<Order by;\<Descending;Fi\<lter;\!\<Select;\?\<Cancel")

lcPushNames = ",pbFind,pbTags,pbOrder,pbFilter,pbSelect,pbCancel,"

DIMENSION laBrObjCnt[6]

=gfSubStr(lcBrPushB,@laBrObjCnt,';')

lcValidFunc=",lfvFind(),lfvTags(),lfvOrder(),lfvFilter(),lfvSelect(),lfvCancel(),"

STORE .F. TO laCntObj 

lnNoPush = 1

lnPushWd = 12

FOR lnCount = 1 TO 6 

  IF !EMPTY(laBrObjCnt[lnCount])
    laCntObj[lnNoPush,4] = SUBSTR(lcPushNames,ATC(',',lcPushNames,lnCount)+1;
                          ,(ATC(',',lcPushNames,lnCount+1));
                          -ATC(',',lcPushNames,lnCount)-1)
     laCntObj[lnNoPush,1] = .T.
     laCntObj[lnNoPush,2] = laBrObjCnt[lnCount]
     laCntObj[lnNoPush,3] = lfGetHot(laBrObjCnt[lnCount],laCntObj[lnNoPush,4])
     laCntObj[lnNoPush,5] = SUBSTR(lcValidFunc,ATC(',',lcValidFunc,lnCount)+1;
                          ,(ATC(',',lcValidFunc,lnCount+1));
                          -ATC(',',lcValidFunc,lnCount)-1)

     lnNoPush = lnNoPush + 1
  ENDIF 
ENDFOR



***************

lnY1 = IIF(TYPE('lnY1')='N',lnY1,0)
lnX1 = IIF(TYPE('lnX1')='N',lnX1,0)
*lnY2 = IIF(TYPE('lnY2')='N',lnY2,30)
*lnX2 = IIF(TYPE('lnX2')='N',lnX2,105)

lnY2 = IIF(TYPE('lnY2')='N',lnY2,SROWS()-4)
lnX2 = IIF(TYPE('lnX2')='N',lnX2,SCOLS()-1)

lnNoPush = lnNoPush - 1
lnWindWidth=MAX(lnNoPush*(lnPushWd+1),lnX2-lnX1)
IF lnX2-lnX1 < lnWindWidth
  lnX2=lnWindWidth+lnX1
ENDIF

lnEach=INT(((lnWindWidth-(lnNoPush*lnPushWd))/lnNoPush+lnPushWd))
IF TYPE('lcFltrExp')='C'
  lcForExp=IIF(ATC('FOR ',lcFltrExp)>0,SUBSTR(lcFltrExp,ATC('FOR ',lcFltrExp)),'')
ENDIF  
lcFltrExp=IIF(!EMPTY(lcForExp),SUBSTR(lcFltrExp,1,ATC(lcForExp,lcFltrExp)-1),lcFltrExp)        
lcKeyExp = IIF(TYPE('lcFltrExp')='C' AND !EMPTY(lcFltrExp),lcFltrExp,'')
=gfSubStr(lcKeyExp,@laKeyExp,',')
lnLineRange=IIF(_WINDOWS,1.77,2)        
lcCurFilter=''
lcMenuStat = IIF(_WINDOWS,"NOMENU ","NOMENU ")
lcMenuStat = IIF(llUnix,lcMenuStat,lcMenuStat+" NOWAIT ")
*E600822,1 Hesham El-Sheltawi (Start)
*E600822,1 get a the prefrence name for the file
*lcMenuStat = IIF(_WINDOWS,"NOMENU","")
*E600822,1 Hesham El-Sheltawi (Start)
*E600822,1 SET the resource to off
*B800824,1 TMI 11/11/96 Commented out the next line to allow to the same
*B800824,1              User_Id to open the same browse in other machine
*B800824,1              without chicking if the current User_Id opens this 
*B800824,1              brow or not.
*SET RESOURCE ON
*B801116,1 Hesham El-sheltawi (Start)
*SET RESOURCE OFF
SET RESOURCE ON
*B801116,1 Hesham El-sheltawi (End)
*B800824,1 TMI 11/11/96 (End)
*E600822,1 Hesham El-Sheltawi (End)

*B604857,1 (Begin) Add the fields user wants to browse to lcBrFields.
lnSavAls = SELECT()
IF !USED('SYDFILES')
  =gfOpenFile(gcSysHome+'SYDFILES','Cfile_nam')
ENDIF
SELECT SYDFILES
lcBrFields = IIF(TYPE('lcBrFields')<>'C','',lcBrFields)
IF SEEK(lcSavAls)
   DIMENSION laUsr_BrFl[1]
   =gfSubStr(mUsr_Brow,@laUsr_BrFl,"|")
   FOR lnCount = 1 TO ALEN(laUsr_BrFl,1)
     IF !EMPTY(laUsr_BrFl[lnCount])
       lcBrFields = lcBrFields + IIF(AT(UPPER(laUsr_BrFl[lnCount]),UPPER(lcBrFields))<>0,;
                                '',IIF(!EMPTY(lcBrFields),',','')+laUsr_BrFl[lnCount])
     ENDIF                           
  ENDFOR
ENDIF  
SELECT (lnSavAls)
*B604857,1 (End)

*E301853,1 WAB (START) - cehck if the lcBrfields contain style field or color field to add the color description
PRIVATE  llUseColor ,lnClrStrt ,lnClrWidth ,lcNewFlds ,laFileStru ,llColor ,llCColor ,llStyle  
*--llUseColor --> hold true if the stule code structure contain COLOR CODE
*--lnClrStrt  --> hold the color strt position
*--lnClrWidth --> hold the the color width 
*--lcNewFlds  --> hold the new fileds thta have to be added to the lcBrFields ( browse fields )
*--laFileStru --> hold the structure of the current Alias file
*--llColor    --> hold .T. if the cuurent file contain COLOR field
*--llCColor   --> hold .T. if the cuurent file contain CCOLOR field
*--llStyle    --> hold .T. if the cuurent file contain Style  field

STORE .F. TO llUseColor ,llColor ,llCColor ,llStyle 
lnClrStrt  = 0
lnClrWidth = 0
lcNewFlds =''
*--get the structure of the current file 
=AFIELDS(laFileStru) 

*--check if the current file contain Style field AND the lcBrField didn't contain the Style major 
llStyle  = (ASCAN(laFileStru,'STYLE') > 0 ) AND ;
           ('STYLE' $ UPPER(lcBrFields)) AND !('CSTYMAJOR' $ UPPER(lcBrFields)) 

IF llStyle 
  =lfCheckClr()
  IF (ASCAN(laFileStru,'STYLE') > 0 ) AND EMPTY(SUBSTR(STYLE,lnClrStrt,lnClrWidth))
    llStyle = .F.
  ENDIF
ENDIF

*--check if the current file contain COLOR field AND the lcBrField didn't contain "******" 
*--( all Colors )
llColor  = !llStyle  AND (ASCAN(laFileStru,'COLOR') > 0 ) AND ;
           ("COLOR" $ UPPER(lcBrFields))  AND !("******" $ lcBrFields) 

*--check if the current file contain CCOLOR field AND the lcBrField didn't contain "******" 
*--( all Colors )
llCColor = !llStyle  AND !llColor  AND (ASCAN(laFileStru,'CCOLOR') > 0 )  AND ;
           ("CCOLOR" $ UPPER(lcBrFields)) AND !("******" $ lcBrFields)

*-- in the Above condations i have check the llStyle is false to check this condation becasue if 
*-- the browse file contain STYLE and FABRIC/COLOR fields so in this case we have to display the 
*-- color disc. of the Style field.


DO CASE
  CASE llStyle
    *--call function to check if the style code structure caontain Color Code .
    IF llUseColor  
      lcNewFlds = [,lcColor = gfCodDes(SUBSTR(STYLE,lnClrStrt,lnClrWidth),"COLOR"):h="Color Desc"]
    ENDIF
  CASE llColor 
    IF USED('FABRIC_A')
      lcNewFlds =[,lcColor = gfCodDes(FABRIC_A.COLOR,"COLOR"):h="Color Desc"]  
    ELSE
      lcNewFlds =[,lcColor = gfCodDes(COLOR,"COLOR"):h="Color Desc"]  
    ENDIF
  CASE llCCOLOR
    lcNewFlds =[,lcColor = gfCodDes(EVAL(ALIAS()+'.CCOLOR'),"COLOR"):h="Color Desc"]  
ENDCASE
lcBrFields = lcBrFields + lcNewFlds
*E301853,1 WAB (END) 

lcPrefName = gfBrowPref(lcBrFields,lcAliasName,llGetTree)
lcMenuStat = lcMenuStat+lcPrefName
*600822,1 MAN No need for this var.
*lcBrowFields = ''

*E600822,1 Hesham El-Sheltawi (End)
lcSavTag =SET('ORDER')
DECLARE laTags[1,2],laExp[1]
  lnCount=1
  DO WHILE !EMPTY(SYS(14,lnCount))       && Filling the Array with the 
    SET ORDER TO lnCount                 && file tags and the order of it.
    DIMENSION laSavStat[lnCount]  
    laSavStat[lnCount]=SET("ORDER")+IIF("DESCENDING"$ SET("ORDER"),""," ASCENDING")
    lnCount=lnCount+1
  ENDDO
  SET ORDER TO &lcSavTag
  ** Filling the Tags Array with the Field Header OR the Field Name **
  *E300500,1 Hesham El-Sheltawi (Start)
  *E300500,1 if there is any Key passed parameter get all the Tags that
  *E300500,1 it's expression start like the active Order otherwise get all
  *E300500,1 available tags
*  DO WHILE !EMPTY(SYS(14,lnCount))
*    IF !EMPTY(laTags[1])
*      DIMENSION laTags[ALEN(laTags,1)+1]
*    ENDIF  
*    laTags[ALEN(laTags,1)] = SYS(14,lnCount)
*    lnCount=lnCount+1
*  ENDDO  
  IF TYPE('lcFltrExp')='C' AND !EMPTY(lcFltrExp)

    *E301077,72 if no active order store null to it. [Begin
    *lcActiveTag = SYS(14,VAL(SYS(21)))
    lcActiveTag = IIF(VAL(SYS(21))=0,'',SYS(14,VAL(SYS(21))))
    *E301077,72 if no active order store null to it. [End

    PRIVATE laActTags,lnAvalLen,lcCheckExpr,lcAvalExp
    lcAvalExp = STRTRAN(IIF(ATC(LEFT(lcFltrExp,1)+',',lcFltrExp)>0,SUBSTR(lcFltrExp,1,ATC(LEFT(lcFltrExp,1)+',',lcFltrExp)-1),lcFltrExp),LEFT(lcFltrExp,1))
    IF TYPE(lcAvalExp)='C'
      lnAvalLen = LEN(EVAL(lcAvalExp))
    ELSE
      lnAvalLen = LEN(lcAvalExp)
    ENDIF  
    DIME laActTags[1,1]
    =gfSubStr(lcActiveTag,@laActTags,'+')
    lcCheckExpr=''
    FOR lnCount = 1 TO ALEN(laActTags,1)
      lcCheckExpr=lcCheckExpr+IIF(EMPTY(lcCheckExpr),'','+')+laActTags[lnCount,1]
      IF LEN(EVAL(lcCheckExpr)) >= lnAvalLen
        EXIT
      ENDIF
    ENDFOR
    lnCount = 1
    DO WHILE !EMPTY(SYS(14,lnCount))
      IF !EMPTY(laTags[1,1]) AND LEFT(SYS(14,lnCount),LEN(lcCheckExpr)) = lcCheckExpr
        DIMENSION laTags[ALEN(laTags,1)+1,2]
      ENDIF  
      IF LEFT(SYS(14,lnCount),LEN(lcCheckExpr)) = lcCheckExpr
        laTags[ALEN(laTags,1),1] = SYS(14,lnCount)
        laTags[ALEN(laTags,1),2] = TAG(lnCount)        
      ENDIF  
      lnCount=lnCount+1
    ENDDO  
    IF ALEN(laTags,1)>1
      llChckKey = .T.
    ENDIF
  ELSE
    lnCount=1  
    DO WHILE !EMPTY(SYS(14,lnCount))
      IF !EMPTY(laTags[1,1])
        DIMENSION laTags[ALEN(laTags,1)+1,2]
      ENDIF  
      laTags[ALEN(laTags,1),1] = SYS(14,lnCount)
      laTags[ALEN(laTags,1),2] = TAG(lnCount)      
      lnCount=lnCount+1
    ENDDO  
  ENDIF
  *E300500,1 Hesham El-Sheltawi (End)  
  ** End ofFilling of the Tags Array  **
  ** Adding NO ORDER to the Tags Array **
  *E300500,1 Hesham El-Sheltawi (Start)
  *E300500,1 Check if there is No Key to browse for before adding the
  *E300500,1 No Order
  IF !llChckKey
*  IF !llChckKey AND TYPE('lcFltrExp')='C' AND !EMPTY(lcFltrExp)
  *E300500,1 Hesham El-Sheltawi (End)  
    IF !EMPTY(laTags[1,1])
      DECLARE laTags[ALEN(laTags,1)+1,2]
      =AINS(laTags,1)
      laTags[1,1]="No Order"
      laTags[1,2]=""      
      lcOrder=SET("ORDER")
      IF !EMPTY(lcOrder)
        lcTemp=ALLTRIM(SUBSTR(SET("ORDER"),4,ATC('OF',SET("ORDER"))-4))
        FOR I=1 TO ALEN(laTags,1)-1
           IF TAG(I)=lcTemp
            puTags=I
          ENDIF
        ENDFOR 
      ENDIF  
    ENDIF
    puTags=puTags+1
    IF TYPE("laTags[1]")$"UL"
      laTags[1]=" " 
    ENDIF
  ENDIF    



******
lcBrowHost = SYS(2015)
lcBrowWind = SYS(2015)
lcBrowCnt  = SYS(2015)
****
*B601510,1 Hesham El-Sheltawi (Start)
*B601510,1 Make the window name that use in the "order by" variable name
*LISTWIND  = IIF(llIsFilter,'LisTWindFl','lisTWind')
LISTWIND  = SYS(2015)
*B601510,1 Hesham El-Sheltawi (End)
DEFINE POPUP puTags
PRIVATE laModeScr[5]
lcSavArea = SELECT(0)
IF EMPTY(DBF())
  =gfDialog('I',"There is no Data Base in use. Press any key to continue...") 
  POP KEY
  *B600540,1 MFM 07/05/95 (Begin) Changed to return false.
  *RETURN  
  RETURN .F.
  *B600540,1 MFM 07/05/95 (End).
ELSE
  lnSavRec = IIF(RECNO()>RECCOUNT(),0,RECNO())
   GO TOP        
  IF EOF()
      =gfDialog('I',"No records to display")
    _CUROBJ = _CUROBJ
    POP KEY
    *B600540,1 MFM 07/05/95 (Begin) Changed to return false.
    *RETURN  
    RETURN .F.
    *B600540,1 MFM 07/05/95 (End).
  ELSE
    *ashraf
    *IF lnSavRec<>0 
    IF BETWEEN(lnSavRec,1,RECCOUNT())
      GO lnSavRec
    ENDIF  
  ENDIF
ENDIF
STORE '' TO lvExpres,lcNewObj,lcFindExp
IF TYPE('lcFile_ttl') <> "U" .AND. !EMPTY(lcFile_ttl) AND TYPE('lcBrowTitle')<>'C'
  lcWinTitl = lcFile_ttl
  lcBrowTitle = lcFile_ttl
ELSE
  lcBrowTitle =IIF(TYPE('lcBrowTitle')<>'C',ALIAS(),lcBrowTitle)
  lcWinTitl =IIF(TYPE('lcBrowTitle') $ "UL",ALIAS(),lcBrowTitle)
ENDIF
lcWinTitl = ALIAS()
gcFiltExp  = SET("FILTER")
lcFiltExp  = ' '
llFirstTime=.T.
cbIgnore   =.T.
llTagBy    =.F.
llClick    =.F.
llMyOpen   =.F.
llOpenBy   =.F.
llNewObj   =.F.
cbWrap     =.F.
cbMAtch    =.F.
llEmpty    =.F.
llSelBrow  =.T.
lnTimelimt = 0
lnCol      = 0
lnlenth    = 0
puTags     = 0
puField_H  = 0
lnLastKey  = 1
lnCount    = 1
rbScope    = 1
lnCurR     = MROW()
IF ALIAS()=lcBaseFile
  EXTERNAL ARRAY laField_H,laField_N,laFiltrExp
*ELSE
* lcBrFields = gfDbfField(ALIAS())
ENDIF

IF ALIAS() <> lcBaseFile
  llSelBrow=.F.
  *DECLARE laHeadTmp[ALEN(laField_H,1),ALEN(laField_H,2)],;
          laNameTmp[ALEN(laField_N,1),ALEN(laField_N,2)],;
          laFiltTmp[ALEN(laFiltrExp,1),ALEN(laFiltrExp,2)]
  DECLARE laHeadTmp[ALEN(laField_H,1)],;
          laNameTmp[ALEN(laField_N,1)],;
          laFiltTmp[ALEN(laFiltrExp,1),ALEN(laFiltrExp,2)]

   =ACOPY(laField_H,laHeadTmp)
   =ACOPY(laField_N,laNameTmp)
   =ACOPY(laFiltrExp,laFiltTmp)

  *B607133,1 AMH The laField_N and laField_H has only one column [Start]
  *DECLARE laField_H[1,2],laField_N[1,2],laFiltrExp[1,ALEN(laFiltrExp,2)]
  DECLARE laField_H[1,1],laField_N[1,1],laFiltrExp[1,ALEN(laFiltrExp,2)]
  *B607133,1 AMH [End]
  
  STORE " " TO laField_H,laField_N,laFiltrExp
   
ENDIF
IF !USED("SYDFIELD")                
  llOpenBy =.T.
  SELECT 0
  USE &gcSysHome.SYDFIELD.DBF
  SET ORDER TO CFLD_NAME
ELSE
  SELECT SYDFIELD
  SET ORDER TO CFLD_NAME
  IF BOF() .OR. EOF()
    lnField = 0
  ELSE    
    lnField = RECNO()
  ENDIF  
ENDIF  

IF !USED("SYDFLFLD")
  llMyOpen=.T.
  SELECT 0
  USE &gcSysHome.SYDFLFLD.DBF
    SET ORDER TO TAG CFILE_NAM
ELSE
  SELECT SYDFLFLD
  SET ORDER TO TAG CFILE_NAM
  IF BOF() .OR. EOF()
    lnFlfld = 0 
  ELSE    
    lnFlfld = RECNO()
  ENDIF  
ENDIF  


** Limitation of the PopUp lenth **
lnlenth=IIF(lnlenth > 25,lnlenth,25)
SELECT (lcSavArea)
lcSavFilt= SET("FILTER")
lcSavTag = SET("ORDER")

*ashraf
*IF lnSavRec<>0 
IF BETWEEN(lnSavRec,1,RECCOUNT())
  GO lnSavRec 
ENDIF
*B500675,1 Change the variable lnRecNo to lnBrRecNO 
IF BOF() .OR. EOF()
  lnBrRecNO = 0
ELSE  
  lnBrRecNO= RECNO()
ENDIF  
lcOrder=SET("ORDER")

*************Filling of the Field Heading Array**********
lnCount = 1 
IF !SEEK(UPPER(ALIAS()),"SYDFLFLD")
  *B605349,1 BWA 02/06/2002 Fix the bug of "Operator/Operand Type Mismatch" If you go to the
  *B605349,1 BWA 02/06/2002 WIP from Style and then add a Filter By Completion date the previous error happens.[START]
  *DIMENSION laField_N[FCOUNT()],laField_H[FCOUNT()]
  *FOR lnCount=1 TO FCOUNT()
  *  laField_N[lnCount]=FIELD(lnCount)
  *  laField_H[lnCount]=FIELD(lnCount)
  *ENDFOR

  DIMENSION laField_N[FCOUNT(),1],laField_H[FCOUNT(),1]
  FOR lnCount=1 TO FCOUNT()
    laField_N[lnCount,1]=FIELD(lnCount)
    laField_H[lnCount,1]=FIELD(lnCount)
  ENDFOR
  *B605349,1 BWA 02/06/2002.[END]
ENDIF

*B605070,1 (Begin) Add the user fields to the fiolter popup when browsing.
IF SEEK(lcSavAls,'SYDFILES')
   FOR lnCount = 1 TO ALEN(laUsr_BrFl,1)
     IF !EMPTY(laUsr_BrFl[lnCount]) AND ASCAN(laField_H,laUsr_BrFl[lnCount])=0
       lnNewIt  = ALEN(laField_N,1)+1
       *B606777,1 RAE rania
       *DIMENSION laField_N[lnNewIt,1],laField_H[lnNewIt,1]
       
       *B607133,1 AMH The laField_N and laField_H has only one column [Start]
       *DIMENSION laField_N[lnNewIt,2],laField_H[lnNewIt,2]
       DIMENSION laField_N[lnNewIt,1],laField_H[lnNewIt,1]
       *B607133,1 AMH [End]
       
       *B606777,1 RAE 
       =SEEK(laUsr_BrFl[lnCount],'SYDFIELD')
       laField_N[lnNewIt,1]= laUsr_BrFl[lnCount]
       laField_H[lnNewIt,1]= IIF(EMPTY(sydField.cFld_Head),sydField.cFld_Name,sydField.cFld_Head)
       
       *B607133,1 AMH The laField_N and laField_H has only one column [Start]
       *IF SEEK(laUsr_BrFl[lnCount],'sydFlFld')
       *  laField_N[lnNewIt,2]= sydFlFld.nFld_Pos
       *  laField_H[lnNewIt,2]= sydFlFld.nFld_Pos
       *ENDIF
       *B607133,1 AMH [End]
       
     ENDIF                           
  ENDFOR
ENDIF  
SELECT (lnSavAls)
*B605070,1 (End)

IF ALIAS()=lcBaseFile
  puField_H = laField_H[1]       
  llEmpty=EMPTY(laField_N)
ENDIF  
*****************End of Array Filling********************

PUSH MENU _MSYSMENU
*RELEASE PAD _BROWSE OF _MSYSMENU

*B600683,1 MFM 09/25/95 (Begin) Commeted out.

*ON KEY LABEL BACKTAB DO lpBrowTrap with 2
*B600683,1 MFM 09/25/95 (End).

IF llUnix
	DEFINE WINDOW (lcbrowhost) ;
		FROM lnY1, lnX1  ;
		TO lnY2,lnX2 ;
		TITLE lcBrowTitle ;
		FLOAT ;
		NOCLOSE ;
		SHADOW ;
		MINIMIZE ;
		SYSTEM ;
  	  COLOR SCHEME 10

	DEFINE WINDOW (lcbrowwind) ;
		FROM IIF(llRemark, -1,1),0  ;
		TO WROW(lcBrowHost)-1,WCOL(lcBrowHost)-1 ;
		NOFLOAT ;
		NOCLOSE ;
		MINIMIZE ;
		SYSTEM ;
		  COLOR SCHEME 1 ;
		IN WINDOW (LCBROWHOST)
  ACTIVATE WINDOW (LCBROWHOST)				  
  DO WHILE !llBrowTmnat
    =lfTrapKey()
    =lfbrowse()
  ENDDO  
ELSE
  DO WHILE !llBrowTmnat
*-- MAN Changed the Spr Name and the related screens
*-- Because there is SPR with the same name embeded in the ARIA ADV. APP
*-- which gives the priority for the embeded SPR to be executed  
*    DO (qsd+'gwdBrows.SPR')
    DO gwdBrse.SPR
    RELEASE BAR 100 OF P01PU01    
    IF !EMPTY(lcToProc)
      *B601510,1 Hesham El-Sheltawi (Start)
      *B601510,1 save the popup of the tags
      *B601510,1 before running any branching program
      PUSH POPUP PUTAGS    
      *B601510,1 Hesham El-Sheltawi (End)
      &lcToProc
      lcToProc =''
      *B601510,1 Hesham El-Sheltawi (Start)      
      *B601510,1 restore the popup of the tags
      POP POPUP PUTAGS          
      *B601510,1 Hesham El-Sheltawi (End)      
    ENDIF
   ENDDO  
ENDIF
RELEASE WINDOW (lcbrowcnt),(lcbrowhost),(lcbrowwind),(lcWintitl)

POP MENU _MSYSMENU 
IF TYPE('lcOldExpr') = "C"
  RELEASE lcOldExpr
ENDIF  

IF llOpenBy
  USE IN SYDFIELD
ENDIF

** Close the files which opend by the program **
IF llMyOpen
  USE IN SYDFLFLD
ENDIF  

SELECT(lcSavArea)
 =lfResBrSet()
IF ALIAS()<>lcBaseFile
  
  DIMENSION laField_H[ALEN(laHeadTmp,1)],;
             laField_N[ALEN(laNameTmp,1)],;
             laFiltrExp[ALEN(laFiltTmp,1),ALEN(laFiltTmp,2)]
             
   =ACOPY(laHeadTmp,laField_H)
   =ACOPY(laNameTmp,laField_N)
   =ACOPY(laFiltTmp,laFiltrExp)
ENDIF  

IF (TYPE("lcFieldsNam")$"UL") .OR. (TYPE("lcArrName")$"UL")
  SHOW GETS
ENDIF  

RELEASE WINDOW (LISTWIND)
POP KEY
CLEAR TYPEAHEAD
SET ESCAPE &lcOldEsc
*RELEASE MENU HESHAM
*E600822,1 Hesham El-Sheltawi (Start)
*E600822,1 SET the resource to off
SET RESOURCE OFF
*E600822,1 Hesham El-Sheltawi (End)


RETURN llSelcted

*!**************************************************************
*!
*!      Function: lpBrowSh
*!
*!**************************************************************
*
FUNCTION lpBrowSh
IF RECCOUNT()=0
  SHOW GETS DISABLE ONLY
  SHOW GET  pbCancel Enable
ELSE  
  IF EMPTY(laTags[1])
    SHOW GET pbTags DISABLE
    SHOW GET pbOrder DISABLE
  ELSE  
    IF UPPER('Descending') $ UPPER(lcOrder)
      SHOW GET pbOrder,1 PROMPT ' \<Ascending'
    ELSE
      SHOW GET pbOrder,1 PROMPT '\<Descending'
    ENDIF  
  ENDIF  
ENDIF
IF EMPTY(lcOrder) AND !llChckKey
  SHOW GET pbOrder DISABLE
ELSE
  SHOW GET pbOrder ENABLE  
ENDIF
IF llisFilter
  SHOW GET pbFilter DISABLE
ELSE
  SHOW GET pbFilter ENABLE  
ENDIF
IF TYPE('lcFltrExp')='C' AND !EMPTY(lcFltrExp) AND !llChckKey
  SHOW GET pbTags DISABLE
ENDIF
IF llFirstTime
  lnFirstObj = OBJNUM(&laCntObj[1,4])
  lnLastObj  = OBJNUM(&laCntObj[lnNoPush,4])
  llActBrow = .T.
  _CUROBJ=lnLastObj  
ELSE
  llActBrow = .T.
  _CUROBJ=lnLastObj    
ENDIF  

*!**************************************************************
*!
*!      Function: lfvOrder
*!
*!**************************************************************
*
FUNCTION lfvOrder
CLEAR TYPEAHEAD
lcOrder=SET("ORDER")
=lfGetBrSet()
IF !EMPTY(lcOrder)
  IF 'DESCENDING' $ UPPER(lcOrder)
    lcOrder=SUBSTR(lcOrder,1,AT("DESCENDING",lcOrder)-1) + ' ASCENDING'
    SET ORDER TO &lcOrder
    SHOW GET pbOrder,1 PROMPT '\<Descending'
  ELSE
    lcOrder=lcOrder + ' DESCENDING'
    SET ORDER TO &lcOrder
    SHOW GET pbOrder,1 PROMPT ' Ascen\<ding'
  ENDIF 
  SHOW WINDOW (lcWinTitl) REFR    
  llActBrow = .T.
  _CUROBJ = lnLastObj  
ENDIF  

*!**************************************************************
*!
*!      Function: lfvTags
*!
*!**************************************************************
*
FUNCTION lfvTags1
PUSH KEY 
ON KEY
lnLastTag = puTags
CLEAR TYPEAHEAD
IF WVISIBLE(LISTWIND)
  ACTIVATE WINDOW (LISTWIND) SAME
ELSE
  ACTIVATE WINDOW  (LISTWIND) NOSHOW
ENDIF

DO CASE
  CASE _DOS
    @ 0.000,0.000 GET puTags ;	
      PICTURE "@&NT" ;
      FROM laTags ;
      SIZE 10,lnLenth*9/7 ;
      FONT "FOXFONT",9;
      COLOR SCHEME 9      
      
  CASE _WINDOWS
    @ 0.000,0.000 GET puTags ;
      PICTURE "@&T" ;
      FROM laTags ;
      SIZE 3,lnlenth*9/7;
      FONT "FOXFONT",9;
      COLOR SCHEME 9
      
ENDCASE
	
IF NOT WVISIBLE(LISTWIND)
  ACTIVATE WINDOW (LISTWIND)
ENDIF
READ 
HIDE WINDOW  (LISTWIND)
IF !INLIST(LASTKEY(),27,9) AND puTags>0
  DO CASE
*    CASE puTags = 1 AND !llChckKey
*      SHOW GET pbOrder DISABLE
*      SET ORDER TO
    CASE BETWEEN(puTags,1,ALEN(laTags,1)) 
        SHOW GET pbOrder ENABLE
      IF !EMPTY(laTags[putags,2])
        SET ORDER TO TAG (laTags[putags,2])
        SHOW GET pbOrder ENABLE
      ELSE
        SET ORDER TO
        SHOW GET pbOrder DISABLE
      ENDIF
*        SET ORDER TO puTags-IIF(!llChckKey,1,0)
*        SET ORDER TO TAG (LATAGS(puTags,2))
        lcOrder=SET("ORDER")    
*      ENDIF  
    OTHERWISE

  ENDCASE
ELSE
  puTags=lnLastTag  
ENDIF
IF !llChckKey
  IF puTags > 1
    IF UPPER('Descending') $ UPPER(lcOrder)
      SHOW GET pbOrder,1 PROMPT '\<Ascending'
    ELSE
      SHOW GET pbOrder,1 PROMPT '\<Descending'
    ENDIF  
*    SET ORDER TO puTags-1
    IF !EMPTY(laTags[putags,2])
      SET ORDER TO TAG (laTags[putags,2])
    ELSE
      SET ORDER TO
    ENDIF
    lcOrder=SET("ORDER")
  ENDIF  
ELSE
    IF UPPER('Descending') $ UPPER(lcOrder)
      SHOW GET pbOrder,1 PROMPT '\<Ascending'
    ELSE
      SHOW GET pbOrder,1 PROMPT '\<Descending'
    ENDIF  
    IF PUTAGS<>0
*      SET ORDER TO puTags
      IF !EMPTY(laTags[putags,2])
        SET ORDER TO TAG (laTags[putags,2])
      ELSE
        SET ORDER TO
      ENDIF
    ENDIF  
    lcOrder=SET("ORDER")  
ENDIF  
POP KEY
llActBrow = .T.
_CUROBJ=lnLastObj


FUNCTION lfvTags
=lfGetBrSet()
IF RDLEVEL()=5
  lcToProc=[=lfvTags1()]
  CLEAR READ
ELSE 
  =lfvTags1()
   SHOW WINDOW (lcWinTitl) REFR  
ENDIF

*!**************************************************************
*!
*!      Function: lfvFind
*!
*!**************************************************************
*
FUNCTION lfvFind
*=lfGetBrSet()
=lfDefFieldArr()
IF RDLEVEL()=5
  lnBrCurObj = _CUROBJ
  lcToProc=[DO lfProcScreen WITH "DO lfFind"]
  CLEAR READ
ELSE  
  PUSH KEY CLEAR
  CLEAR TYPEAHEAD
  DO lfFind
  POP KEY
  llActBrow = .T.
  _CUROBJ=lnLastObj
ENDIF  
SHOW WINDOW (lcWinTitl) REFR  

*!**************************************************************
*!
*!      Function: lfvSelect
*!
*!**************************************************************
*
FUNCTION lfvSelect
IF llUnix
  CLEAR TYPEAHEAD
  llSelcted = .T.
  IF !EMPTY(lcOnSelect)
    lnBrCurObj = _CUROBJ
    DO lfProcScreen WITH "DO lfvDoSelFun"
  ELSE
    IF (TYPE("lcFieldsNam")$"UL") .OR. (TYPE("lcArrName")$"UL")
      SCATTER FIELDS &lcScFields MEMO TO laData
      laScrMode    = .F.
      laScrMode[2] = .T.
    ELSE
      SCATTER FIELDS &lcFieldsNam MEMO TO (lcArrName)
    ENDIF   
    RELEASE WINDOW (lcWinTitl)
    llBrowTmnat = .T.
  ENDIF  
ELSE
  CLEAR TYPEAHEAD
  llSelcted = .T.
  ACTIVATE WINDOW (lcBrowCnt)
  IF !EMPTY(lcOnSelect)
    lnBrCurObj = _CUROBJ
    lcToProc=[DO lfProcScreen WITH "DO lfvDoSelFun"]
    CLEAR READ
  ELSE
    IF (TYPE("lcFieldsNam")$"UL") .OR. (TYPE("lcArrName")$"UL")
      SCATTER FIELDS &lcScFields MEMO TO laData
      laScrMode    = .F.
      laScrMode[2] = .T.
    ELSE
      SCATTER FIELDS &lcFieldsNam MEMO TO (lcArrName)
    ENDIF   
    llBrowTmnat = .T.
    CLEAR READ
  ENDIF  
ENDIF  

*!**************************************************************************
*!
*!      Function : lfvDoSelFun
*!
*!**************************************************************************
*
FUNCTION lfvDoSelFun
*B500675,1 Change the variable lnRecNo to lnBrRecNO 
REGIONAL lnCurAlias,lnBrRecNO
*HIDE WINDOW (lcBrowHost)
lnCurAlias = SELECT()
lnBrRecNO    = IIF(RECNO()>RECCOUNT(),0,RECNO())
POP KEY
DO (lcOnSelect)
PUSH KEY CLEAR
*600822,1 MAN Start
*lcBrowFields = lcOldBrow
lcBrFields = lcOldBrow
*600822,1 MAN End

CLEAR TYPEAHEAD
SELECT (lnCurAlias)
IF lnBrRecNO<>0
  GO lnBrRecNO
ENDIF

*SHOW WINDOW (lcBrowHost)  

*!**************************************************************
*!
*!      Function: lfvFilter
*!
*!**************************************************************
*
FUNCTION lfvFilter
=lfGetBrSet()
IF RDLEVEL()=5
  lnBrCurObj = _CUROBJ
  lcToProc=[DO lfProcScreen WITH "DO lfFilter"]
  CLEAR READ
ELSE  
  PUSH KEY 
  ON KEY
  CLEAR TYPEAHEAD
  DO lfFilter
  POP KEY
  llActBrow = .T.
  _CUROBJ=lnLastObj
ENDIF  

*!************************************************************"*
*!
*!      Function: lfvLocate
*!
*!**************************************************************
*
FUNCTION lfvLocate
=lfGetBrSet()
IF RDLEVEL()=5
  lnBrCurObj = _CUROBJ
  lcToProc=[DO lfProcScreen WITH "DO lfLocate with ]+lcCurField+'"'
  CLEAR READ
ELSE  
  PUSH KEY 
  ON KEY
  CLEAR TYPEAHEAD
  lcCurField=varread()
  DO lfLocate with lcCurField
  POP KEY
  llActBrow = .T.
  _CUROBJ=lnLastObj
ENDIF  

*!**************************************************************
*!
*!      Function: lfvGoTo
*!
*!**************************************************************
*
FUNCTION lfvGoTo
IF RDLEVEL()=5
  lnBrCurObj = _CUROBJ
  lcToProc=[DO lfProcScreen WITH "DO lfGoTo"]
  CLEAR READ
ELSE  
  PUSH KEY 
  ON KEY
  CLEAR TYPEAHEAD
  DO lfGoTo
  POP KEY
  SHOW WINDOW (lcWinTitl) REFR  
ENDIF  

*!**************************************************************
*!
*!      Function: lfvCancel
*!
*!**************************************************************
*
FUNCTION lfvCancel
*B500675,1 Change the variable lnRecNo to lnBrRecNO 
IF RECCOUNT() > 0
  IF lnBrRecNO <> 0
    GOTO lnBrRecNO
  ENDIF  
ENDIF
llBrowTmnat = .T.
llSelcted = .F.
IF llUnix
  RELEASE WINDOW (lcWinTitl)
ELSE
  ACTIVATE WINDOW (lcBrowCnt)
  CLEAR READ
ENDIF  

****************************************************************
* 
*         PROCEDURE  :  lpLeftKey 
* 
****************************************************************
*
** Traping for moving one field left **
PROCEDURE lpLeftkey
CLEAR TYPEAHEAD         &&B600689,1 MFM 09/26/95 Added to privent (Do Nesting to deep)
KEYBOARD "{HOME}"
KEYBOARD "{HOME}"

****************************************************************
* 
*         PROCEDURE  :  lpRightKey 
* 
****************************************************************
*
** Traping for moving one field Right **
PROCEDURE lpRightkey
CLEAR TYPEAHEAD         &&B600689,1 MFM 09/26/95 Added to privent (Do Nesting to deep)
KEYBOARD "{END}"
KEYBOARD "{END}"

****************************************************************
* 
*         PROCEDURE  :  lpLastKey 
* 
****************************************************************
*
PROCEDURE lpLastKey
PARAMETERS lcWhObject
lnLastKey = LASTKEY()
CLEAR TYPEAHEAD
IF _DOS
  HIDE MENU _MSYSMENU
  SHOW MENU _MSYSMENU
ENDIF
ACTIVATE WINDOW (lcBrowCnt)
_CUROBJ=lcWhObject
  
****************************************************************
* 
*         FUNCTION   : lpHotKey
* 
****************************************************************
*
PROCEDURE lpHotKey 
PARAMETERS lcToDo
IF EMPTY(lcToDo)
  RETURN
ENDIF
IF UPPER(lcToDo) $ "PBTAGS,PBORDER"
  IF EMPTY(laTags[1])
    RETURN
  ENDIF
ENDIF    
*POP KEY
CLEAR TYPEAHEAD
*IF _DOS
*  HIDE MENU _MSYSMENU
*  SHOW MENU _MSYSMENU
*ENDIF
ACTIVATE WINDOW (lcBrowCnt)
_CUROBJ = OBJNUM(&lcToDo)
KEYBOARD "{SPACEBAR}" PLAIN CLEAR


*!******************************************************************
*!
*!              Function: lfDclick
*!
*!******************************************************************
*
FUNCTION lfDclick
PARAMETERS lcKeyLable
****** Coordination Array ******
lnX =INT(MROW())
lnY =INT(MCOL())
*B600322 Hesham El-Sheltawi 06/18/95
*B600322 if the user pressed double click with the mouse in any of the
*B600322 scroll bars "Horz.,Vert." ignore it and dont select the active
*B600322 record, we added the checking if the MROW AND THE MCOL inside
*B600322 the browse window with out taking the position of the scroll
*B600322 bars in out consideration
IF  (MROW(lcWinTitl)<>-1) AND (MCOL(lcWinTitl)<>-1) AND (MROW(lcWinTitl)>lnLineRange);
      AND BETWEEN(MCOL(lcWinTitl),2,WCOL(lcWinTitl)-IIF(_DOS OR _UNIX,2,1.77));
     AND BETWEEN(MROW(lcWinTitl),2,WROWS(lcWinTitl)-IIF(_DOS OR _UNIX,2,1.77))
     
*    AND MCOL(lcWinTitl)<(WCOLS(lcWinTitl)+WLCOL(lcWinTitl)-IIF(_DOS OR _UNIX,2,3.77));
    AND MROW(lcWinTitl)<(WROWS(lcWinTitl)+WLROW(lcWinTitl)-lnLineRange)
  IF !llClick
    lnTimelimt = SECONDS()  
    lnCurR     = lnX
    llClick    = .T.
  ELSE
    IF SECONDS() < lnTimelimt + _DBLCLICK .AND. lnCurR  = INT(MROW()); 
       AND  lnX=lnCurR &&INT(ABS(MROW(lcBrowTtl)-lnCurR))=0
      llClick    = .F.
      KEYBOARD lcKeyLable CLEAR  
    ENDIF
    IF lnCurR    = INT(MROW())
      lnTimelimt = SECONDS()  
    ELSE
      llClick    = .F.
    ENDIF   
  ENDIF
ELSE
  llClick    = .F.
  lcObjName=lfGetObj(lnX,lnY)
 IF !EMPTY(lcObjName) AND UPPER(lcObjName)<>"DUM" AND TYPE(EVAL(lcObjName))!='U'
      lcObjName=EVAL(lcObjName)
   *B601517,1 Hesham El-Sheltawi (Start)   
   *B601517,1 Change the condition to take care about the asc and desc 
   *B601517,1 enabling and disabling
*   IF (EMPTY(laTags[1]) AND UPPER(lcObjName)="PBTAGS") OR (;
       TYPE('lcFltrExp')='C' AND !EMPTY(lcFltrExp) AND UPPER(lcObjName)="PBTAGS";
       AND !llChckKey) 
   IF (EMPTY(laTags[1]) AND UPPER(lcObjName)="PBTAGS") OR (;
       TYPE('lcFltrExp')='C' AND !EMPTY(lcFltrExp) AND UPPER(lcObjName)="PBTAGS";
       AND !llChckKey) OR ((UPPER(lcObjName) = 'PBORDER' AND !llChckKey AND EMPTY(ORDER())))
   *B601517,1 Hesham El-Sheltawi (End)          
     CLEAR TYPEAHEAD
     RETURN
   ENDIF
    ACTIVATE WINDOW (lcBrowCnt)
    _CUROBJ=OBJNUM(&lcObjName)
    KEYBOARD "{ENTER}" PLAIN CLEAR
*    KEYBOARD "{SPACEBAR}" PLAIN CLEAR
  ELSE
    CLEAR TYPEAHEAD  
  ENDIF
ENDIF

*!******************************************************************
*!
*!              Function: lfGetObj
*!
*!******************************************************************
*
FUNCTION lfGetObj
PARAMETER lnX,lnY
lcObj=''
FOR lnCount=1 TO ALEN(laScObj,1)
  IF BETWEEN(lnX,laScObj[lnCount,1],laScObj[lnCount,3]) AND BETWEEN(lnY,laScObj[lnCount,2],laScObj[lnCount,4])
    lcObj=laScObj[lnCount,5]
    EXIT
  ENDIF
ENDFOR
RETURN lcObj

*!**************************************************************************
*!
*!              Function: lfDefFieldArr
*!
*!**************************************************************************
*
*:->
FUNCTION lfDefFieldArr
EXTERNAL ARRAY laData,laKeyField,laField_H,laField_N
IF lcBaseFile <> ALIAS() 
  IF SEEK(UPPER(ALIAS()),"SYDFLFLD")
    IF !EMPTY(lcBrFields)
      lcCurFile = ALIAS()
      SELECT  DISTINCT IIF(EMPTY(sydField.cFld_Head),sydField.cFld_Name,;
              sydField.cFld_Head),sydFlFld.nFld_Pos ;
              FROM &gcSysHome.sydField,&gcSysHome.sydFlFld;
              WHERE sydField.cFld_Name=sydFlFld.cFld_Name .AND.;
              ALLTRIM(sydFlFld.cFile_Nam) = lcCurFile .AND.;
              ALLTRIM(sydFlFld.cFld_Name) $ UPPER(lcBrFields) ;
              ORDER BY nFld_Pos;
              INTO ARRAY laField_H

      SELECT  DISTINCT sydField.cFld_Name,sydFlFld.nFld_Pos ;
              FROM &gcSysHome.sydField,&gcSysHome.sydFlFld;
              WHERE sydField.cFld_Name=sydFlFld.cFld_Name .AND.;
              ALLTRIM(sydFlFld.cFile_Nam) = lcCurFile .AND.;
              ALLTRIM(sydFlFld.cFld_Name) $ UPPER(lcBrFields) ;
              ORDER BY nFld_Pos;
              INTO ARRAY laField_N
    ELSE 
      SELECT  DISTINCT IIF(EMPTY(sydField.cFld_Head),sydField.cFld_Name,;
              sydField.cFld_Head),sydFlFld.nFld_Pos ;
              FROM &gcSysHome.sydField,&gcSysHome.sydFlFld;
              WHERE sydField.cFld_Name=sydFlFld.cFld_Name .AND.;
              sydFlFld.cFile_Nam = lcCurFile;
              ORDER BY nFld_Pos;
              INTO ARRAY laField_H

       SELECT DISTINCT sydField.cFld_Name,sydFlFld.nFld_Pos ;
              FROM &gcSysHome.sydField,&gcSysHome.sydFlFld;
              WHERE sydField.cFld_Name=sydFlFld.cFld_Name .AND.;
              sydFlFld.cFile_Nam = lcCurFile;
              ORDER BY nFld_Pos;
              INTO ARRAY laField_N
     ENDIF
    =gfADel(@laField_H,2,2)  
    =gfADel(@laField_N,2,2)      
  ELSE
    *B605349,1 BWA 02/06/2002 Fix the bug of "Operator/Operand Type Mismatch" If you go to the
    *B605349,1 BWA 02/06/2002 WIP from Style and then add a Filter By Completion date the previous error happens.[START]
    *DIMENSION laField_N[FCOUNT()],laField_H[FCOUNT()]
    *FOR lnCount=1 TO FCOUNT()
    *  laField_N[lnCount]=FIELD(lnCount)
    *  laField_H[lnCount]=FIELD(lnCount)
    *ENDFOR

    DIMENSION laField_N[FCOUNT(),1],laField_H[FCOUNT(),1]
    FOR lnCount=1 TO FCOUNT()
      laField_N[lnCount,1]=FIELD(lnCount)
      laField_H[lnCount,1]=FIELD(lnCount)
    ENDFOR
    *B605349,1 BWA 02/06/2002.[END]
  ENDIF       
  llSelBrow=.T.
  puField_H=laField_H[1,1]
ENDIF


*!**************************************************************************
*!
*!              Function: lfBrowse
*!
*!**************************************************************************
*
*:->
FUNCTION lfBrowse
EXTERNAL ARRAY laData
*B800031,1 MAN Added Saving the the current record no. before the browse because
*B800031,1     the browse changes the record position if both clauses KEY and FOR
*B800031,1     are included
PRIVATE lnCurRec

lnCurRec =RECNO()
    *600822,1 MAN Start Changed the following line by adding to prevent executing
    *600822,1 the IF condition and always execute the ELSE condition since the 
    *600822,1 variable lcMenuStat always handle the fields clause 
    *IF TYPE('lcBrowFields') <>'U' .AND. !EMPTY(lcBrowFields)
    IF .F. .AND. TYPE('lcBrowFields') <>'U' .AND. !EMPTY(lcBrowFields)
      IF !EMPTY(lcFltrExp)
        *** Browse with parametters of exact Fields and Filter expression ***
        BROWSE NOEDIT;
               NOAPPEND;
               NODELETE;
               NOCLEAR; 
               TITLE lcWinTitl;
               COLOR SCHEME 10 FONT "MS Sans Serif", 9;
               WINDOW (lcBrowWind) IN WINDOW (lcBrowHost);
               FIELDS &lcBrowFields ;
               KEY &lcFltrExp ;
               &lcForExp ;
               &lcMenuStat
               
      ELSE

       *** Browse with Parameter of exact Fields ***  
        BROWSE NOEDIT;
               NOAPPEND;
               NODELETE;
               NOCLEAR;
               TITLE lcWinTitl;
               COLOR SCHEME 10 FONT "MS Sans Serif", 9;
               WINDOW (lcBrowWind) IN WINDOW (lcBrowHost);
               FIELDS &lcBrowFields;
               &lcForExp ;
               &lcMenuStat
      ENDIF
    ELSE
      *E302047,1 ABD - Remark the next line and browse with the define font 
      *E302047,1 ABD - that define before before that. [Begin]
      *IF !EMPTY(lcFltrExp)
      *** Browse with parametter with Filter expression ***  
      *  BROWSE NOEDIT;
      *         NOAPPEND;
      *         NODELETE;
      *         NOCLEAR;             
      *         TITLE lcWinTitl;
      *         COLOR SCHEME 10 FONT "MS Sans Serif", 9;
      *         WINDOW (lcBrowWind) IN WINDOW (lcBrowHost);
      *         KEY &lcFltrExp;
      *         &lcForExp ;
      *         &lcMenuStat
      *ELSE
      *** Normal Browse ***
      *BROWSE NOEDIT;
      *         NOAPPEND;
      *         NODELETE;
      *         NOCLEAR; 
      *         TITLE lcWinTitl;
      *         COLOR SCHEME 10 FONT "MS Sans Serif", 9;
      *         WINDOW (lcBrowWind) IN WINDOW (lcBrowHost);
      *         &lcForExp ;
      *         &lcMenuStat
      *ENDIF
      IF !EMPTY(lcFltrExp)
        *** Browse with parametter with Filter expression ***  
        BROWSE NOEDIT;
               NOAPPEND;
               NODELETE;
               NOCLEAR;             
               TITLE lcWinTitl;
               COLOR SCHEME 10 FONT lcBrflFont, 9;
               WINDOW (lcBrowWind) IN WINDOW (lcBrowHost);
               KEY &lcFltrExp;
               &lcForExp ;
               &lcMenuStat
      ELSE
        *** Normal Browse ***
        BROWSE NOEDIT;
               NOAPPEND;
               NODELETE;
               NOCLEAR; 
               TITLE lcWinTitl;
               COLOR SCHEME 10 FONT lcBrflFont, 9;
               WINDOW (lcBrowWind) IN WINDOW (lcBrowHost);
               &lcForExp ;
               &lcMenuStat
      ENDIF
      *E302047,1 ABD - [End]
    ENDIF

*B800031,1 MAN Start
GO TOP
IF BETWEEN(lnCurRec,1,RECCOUNT())
  GO lnCurRec
ENDIF 
*B800031,1 MAN End 

*!**************************************************************************
*!
*!      Function : lfTrapKey
*!
*!**************************************************************************
*
FUNCTION lfTrapKey
*B800038,1 Adding clearing for the keyboard buffer
CLEAR TYPEAHEAD
ON KEY
lnCurRec = IIF(RECNO()<=RECCOUNT(),RECNO(),1)
lcSeekExp= ""
IF llUnix
  ON KEY LABEL CTRL+W     lnDummy = 1
  ON KEY LABEL Ctrl+Q     lnDummy = 1
  ON KEY LABEL CTRL+HOME  GO TOP
  ON KEY LABEL CTRL+END   GO BOTT
  ON KEY LABEL ENTER      DO lfvSelect
  ON KEY LABEL CTRL+ENTER DO lfvSelect
  ON KEY LABEL ESC DO lfvCancel
  ON KEY LABEL BACKTAB    DO lpLeftkey
  ON KEY LABEL LEFTARROW  DO lpLeftkey
  ON KEY LABEL RIGHTARROW DO lpRightKey
ELSE
  ON KEY LABEL CTRL+W     lnDummy = 1
  ON KEY LABEL Ctrl+Q     lnDummy = 1
  ON KEY LABEL CTRL+HOME  GO TOP
  ON KEY LABEL CTRL+END   GO BOTT
  IF !llRemark
    ON KEY LABEL ALT+B DO clrkey
  ENDIF  
*E301050,1 Hesham (Start)  
*E301050,1 User Right mouse for selection insted of Leftmouse
*  ON KEY LABEL LEFTMOUSE  DO lfDclick WITH CHR(13) 
  ON KEY LABEL RIGHTMOUSE  DO lfRtMouse
*E301050,1 Hesham (End)    
  IF !EMPTY(lcEnter)
    ON KEY LABEL ENTER      DO lpHotKey WITH lcEnter
    ON KEY LABEL CTRL+ENTER      DO lpHotKey WITH lcEnter
  ENDIF  

  IF !EMPTY(lcEscape)
    ON KEY LABEL ESC          DO lpHotKey WITH lcEscape
  ELSE
    ON KEY LABEL ESC DO lfvCancel
  ENDIF  


  ON KEY LABEL TAB     DO lpBrowTrap WITH 1
  ON KEY LABEL BACKTAB DO lpBrowTrap WITH 2

*B600311,1 MAN 05/30/95 (Begin) Fixed the bug "VARIABLE NOT FOUND" if you
*B600311,1              pressed SHIFT+TAB key more than once in any browse
*  ON KEY LABEL BACKTAB    DO lpLastKey WITH lnLastObj
  ON KEY LABEL LEFTARROW  DO lpLeftkey
  ON KEY LABEL RIGHTARROW DO lpRightKey
  ON KEY LABEL CTRL+G     DO lfvGoTo
  ON KEY LABEL CTRL+F     DO lfvLocate

  FOR lnCount = 1 TO 6
    IF laCntObj[lnCount,1]
      lcToExec = [DO lpHotKey WITH "]+laCntObj[lnCount,4]+'"'
       ON KEY LABEL &laCntObj[lnCount,3] &lcToExec 
    ENDIF
  ENDFOR

  lcExpToSeek=""
  lcOrdExpr=""
  
  IF SYS(21)<>"0"
    lcOrdExpr   = SYS(14,EVAL(SYS(21)))
    lcOrdExpT   = TYPE(lcOrdExpr)
    lnStartTrap = IIF(lcOrdExpT='N',48,32)
    lnEndTrap   = IIF(lcOrdExpT='N',57,126)
    FOR lnChrToTrap = lnStartTrap TO lnEndTrap
      ON KEY LABEL (CHR(lnChrToTrap)) DO lfIncSearch
    ENDFOR
  ENDIF  
ENDIF
*B600311,1 MAN 05/30/95 (End)

*B600933,1 Spliting the lfIncSearch inTo Two functions so we can terminate the
*B600933,1 browse read cycle if the read level is 5
*!**************************************************************************
*!
*!              Function: lfIncSearch
*!
*!**************************************************************************
*B800296,1 MFM 11/26/95 Re-write the function.
*:->
FUNCTION lfIncSearch
PUSH KEY CLEAR
ON KEY
ON KEY LABEL F1 llDumi = .T.

PRIVATE lnBrRecNO,lcOrgTag

*B600933,1 remarked to change the location of the command
*KEYBOARD "{END}"

IF INLIST(LASTKEY(),27,13,9,15)
  CLEAR TYPEAHEAD
  POP KEY
  RETURN
ENDIF

*CLEAR TYPEAHEAD
*lcExpToSeek = UPPER(CHR(LASTKEY()))

*B600933,1 Define the window of the incremental search function

IF _DOS .OR. _UNIX
  DEFINE WINDOW lwIncSrch ;
    FROM 20, 5 ;
	TO   22,74 ;
	FLOAT NOCLOSE SHADOW NOMINIMIZE NONE COLOR SCHEME 5
  
  ACTIVATE WINDOW lwIncSrch NOSHOW


ELSE
  DEFINE WINDOW lwIncSrch ;
    AT 21.000, 10.000 ;
    SIZE 4.167,55.444 ;
    FONT "FoxFont", 9 ;
    STYLE "B" TITLE "Aria Apparel System" ;
    FLOAT NOCLOSE SHADOW NOMINIMIZE NONE COLOR RGB(,,,192,192,192)

  ACTIVATE WINDOW lwIncSrch NOSHOW
ENDIF

lcExpToSeek = UPPER(CHR(LASTKEY()))

*B600933,1 Hesham El-Sheltawi
*B600933,1 check if the read level = 5 then clear read before
*B600933,1 geting the value to search for otherwse start geting the
*B600933,1 value directly
IF RDLEVEL()=5
  lnBrCurObj = _CUROBJ
  lcToProc=[DO lfProcScreen WITH "DO lfActInSr"]

  CLEAR READ
  KEYBOARD "{ESCAPE}"

  *B602499,1 End of char. does not occur in softseek window. [Begin]
  *          Thus I comment out the following line and make a separate when function 
  *          when get softseek object. 
  *KEYBOARD "{END}"

ELSE  

  PUSH KEY CLEAR
  CLEAR TYPEAHEAD

  *B602499,1 End of char. does not occur in softseek window. [Begin]
  *          Thus I comment out the following line and make a separate when function 
  *          when get softseek object. 
  *          For Compatability only.
  *KEYBOARD "{END}"  

  DO lfActInSr
  POP KEY
  llActBrow = .T.
  _CUROBJ=lnLastObj
ENDIF  


*B600933,1 Hesham El-Sheltawi
*B600933,1 the second part of the incremental search function 
*B600933,1 to get the value of searching
FUNCTION lfActInSr
IF _DOS OR _UNIX
  @ 0,0 TO 2,69
  @ 1,6  SAY "Locate         :" SIZE 1,16, 0
  @ 1,23 GET lcExpToSeek SIZE 1,41 PICTURE '@!' MESSAGE "Press 'ENTER' To Locate"
ELSE

  *B602499,1 End of char. does not occur in softseek window. [Begin]
  *          Thus I comment out the following line and add when function softseek object.
  *          Note: if any errors in UNIX or DOS modes any one can do it easy.
  *@ 1.500,14.333 GET lcExpToSeek ;
  *  SIZE 1.167,41.000 PICTURE '@!' FONT "FoxFont", 9 MESSAGE "Press  'ENTER'  To  Locate"
  @ 1.500,14.333 GET lcExpToSeek ;
    SIZE 1.167,41.000 PICTURE '@!' FONT "FoxFont", 9 MESSAGE "Press  'ENTER'  To  Locate" ;
    WHEN lfEndObj()
	
  @ 1.334,14.111 TO 1.334,51.115 PEN 2, 8 STYLE "1" ;
    COLOR RGB(128,128,128,128,128,128)
	
  @ 2.667,14.222 TO 2.667,51.115 PEN 2, 8 STYLE "1" ;
    COLOR RGB(255,255,255,255,255,255)
	
  @ 1.417,14.111 TO 2.750,14.111 PEN 2, 8 ;
    COLOR RGB(128,128,128,128,128,128)
	
  @ 1.417,50.893 TO 2.750,50.893 PEN 2, 8 ;
    COLOR RGB(255,255,255,255,255,255)
	
  @ 1.417,4.333 SAY "Locate         :"  FONT "MS Sans Serif", 8 ;
    STYLE "B"
	
  @ 0.000,0.000 TO 0.000,55.444 PEN 1, 8 STYLE "1" ;
    COLOR RGB(255,255,255,255,255,255)
	
  @ 0.333,0.444 TO 0.333,55.000 PEN 1, 8 STYLE "1" ;
    COLOR RGB(128,128,128,128,128,128)
	
  @ 4.083,0.000 TO 4.083,55.444 PEN 1, 8 STYLE "1" ;
    COLOR RGB(128,128,128,128,128,128)
	
  @ 3.750,0.444 TO 3.750,55.000 PEN 1, 8 STYLE "1" ;
    COLOR RGB(255,255,255,255,255,255)
	
  @ 0.000,0.000 TO 4.167,0.000 PEN 1, 8 ;
    COLOR RGB(255,255,255,255,255,255)
	
  @ 0.333,0.444 TO 3.833,0.444 PEN 1, 8 ;
    COLOR RGB(128,128,128,128,128,128)
	
  @ 0.333,54.889 TO 3.833,54.889 PEN 1, 8 ;
    COLOR RGB(255,255,255,255,255,255)
	
  @ 0.000,55.333 TO 4.167,55.333 PEN 1, 8 ;
    COLOR RGB(128,128,128,128,128,128)
ENDIF

ACTIVATE WINDOW lwIncSrch TOP

READ MODAL
RELEASE WINDOW lwIncSrch
lcExpToSeek = ALLTRIM(lcExpToSeek)
IF LASTKEY() = 13
  lnBrRecNO = IIF(RECNO()>RECCOUNT(),0,RECNO())

  IF TYPE("lcSeekTag")='C' AND !EMPTY(lcSeekTag)
    lcOrgTag = SYS(22)
    lcOrgExp = SYS(14,VAL(SYS(21)))
    SET ORDER TO TAG (lcSeekTag)
  ENDIF
  
  FOR lnCount = 1 TO ALEN(laKeyExp,1)
    IF SEEK(&lakeyExp[lnCount]+lcExpToSeek)
      IF TYPE("lcOrgTag")='C'
        lcOrgKey = &lcOrgExp
        SET ORDER TO TAG lcOrgTag
        
        IF !EMPTY(lcOrgTag)
          SEEK lcOrgKey
        ENDIF  
      ENDIF
      
      CLEAR TYPEAHEAD
      POP KEY
      RETURN
    ELSE
      IF TYPE("lcOrgTag")='C'
        SET ORDER TO TAG lcOrgTag
      ENDIF
      IF lnBrRecNO>0
        GO lnBrRecNO
      ENDIF  
    ENDIF
  ENDFOR
ELSE
  lcExpToSeek = ''
ENDIF

ON KEY
CLEAR TYPEAHEAD
POP KEY
RETURN


FUNCTION lfGetHot
PARAMETERS lcBrObj,lcWhObj
lcEscape = IIF(ATC('\?',lcBrObj)>0,lcWhObj,lcEscape)
lcEnter  = IIF(ATC('\!',lcBrObj)>0,lcWhObj,lcEnter)
RETURN 'CTRL+'+SUBSTR(lcBrObj,ATC('\<',lcBrObj)+2,1)

*!**************************************************************************
*!
*!      Function : lpBrowTrap
*!
*!**************************************************************************
*
FUNCTION lpBrowTrap
PARAMETERS lnCaseSwicth

*B600769,1 MFM 09/26/95 (Begin) Added.
PUSH KEY CLEAR
ON KEY
*B600769,1 MFM 09/26/95 (End).

IF !WREAD(lcbrowcnt)
  POP KEY   &&B600769,1 MFM 09/26/95.
  RETURN
ENDIF
lcVarNam = VARREAD()

*B600683,1 MFM 09/25/95 (Begin) Added.
IF EMPTY(lcVarNam)
  POP KEY   &&B600769,1 MFM 09/26/95.
  RETURN
ENDIF
*B600683,1 MFM 09/25/95 (End).

DO CASE 
  CASE lnCaseSwich = 1
    DO CASE
      CASE WONTOP() = lcWinTitl 
        ACTIV WINDOW (lcBrowCnt)
      _CUROBJ = lnFirstObj  
     CASE !EMPTY(lcVarNam) AND OBJNUM(&lcVarNam) = lnLastObj
       KEYBOARD "{ALT+B}"
       *ACTIV WINDOW (lcWinTitl)
     OTHERWISE  
       KEYBOARD "{TAB}" PLAIN CLEAR
     ENDCASE
  CASE lnCaseSwich = 2
    DO CASE
      CASE WONTOP() = lcWinTitl 
        ACTIV WINDOW (lcBrowCnt)
      _CUROBJ = lnLastObj  
     CASE !EMPTY(lcVarNam) AND OBJNUM(&lcVarNam) = lnFirstObj
       KEYBOARD "{ALT+B}"     
*       ACTIV WINDOW (lcWinTitl)
     OTHERWISE  
       KEYBOARD "{BACKTAB}" PLAIN CLEAR
     ENDCASE
ENDCASE     

*B600769,1 MFM 09/26/95 (Begin) Added.
POP KEY
*B600769,1 MFM 09/26/95 (End).

*!**************************************************************************
*!
*!      Function : lfActBrow
*!
*!**************************************************************************
*
FUNCTION lfActBrow
IF llActBrow
  llActBrow = .F.
  *B600440,1 if the user press fast enter we have to select
  *B600440,1 the record insted of the cancel
  IF llEnterP
    =lfvSelect()
  ELSE
*    KEYBOARD "{TAB}"
    KEYBOARD "{ALT+B}"
  ENDIF
  llFirstTime = .F.  
  llEnterP = .F.
ENDIF

*!**************************************************************************
*!
*!      Function : DEFMENU
*!
*!**************************************************************************
*
FUNCTION DEFMENU
IF llRemark
  RETURN .T.
ENDIF
DEFINE MENU HESHAM;
	BAR AT LINE 0;
	IN WINDOW (lcBrowHost);
	 COLOR SCHEME 1 KEY ALT+B,"ALT+B"


DEFINE PAD _TEST OF HESHAM PROMPT SPACE(65)
DEFINE PAD _Browse OF HESHAM PROMPT "Browse" KEY CTRL+B, "CTRL+B"
*ON SELECTION PAD _Browse OF HESHAM DO clrkey 
ON PAD _Browse OF HESHAM ACTIVATE POPUP BROWSE

DEFINE POPUP browse MARGIN RELATIVE SHADOW COLOR SCHEME 4
ON SELECTION POPUP BROWSE DO lfToProc WITH STRTRAN(laCntObj[BAR(),5],'()')

FOR lnCount = 1 TO 6
  IF laCntObj[lnCount,1]
     DEFINE BAR lnCount OF browse PROMPT STRTRAN(STRTRAN(laCntObj[lnCount,2],'\?'),'\!')
  ENDIF
ENDFOR

*!**************************************************************************
*!
*!      Function : CLRKEY
*!
*!**************************************************************************
*
FUNCTION CLRKEY
ON KEY
ACTIVATE MENU HESHAM PAD _BROWSE

=lfTrapKey()

*!**************************************************************************
*!
*!      Function : lfToProc
*!
*!**************************************************************************
*
FUNCTION lfToProc
PARAMETERS lcProgToDo
DEACT POPUP BROWSE
DO (lcProgToDo)

*!**************************************************************************
*!
*!      Function : lfProcScreen
*!
*!**************************************************************************
*
FUNCTION lfProcScreen
PARAMETERS lcProgToDo
CLEAR TYPEAHEAD
ON KEY
RELEASE WINDOW (lcWinTitl)
&lcProgToDo
CLEAR TYPEAHEAD

*!**************************************************************************
*!
*!      Function : lfGetBrSet
*!
*!**************************************************************************
*
FUNCTION lfGetBrSet
****
IF !llBrWasFltr
  llBrWasFltr = .T.
  *************Filling of the Field Heading Array**********
*  =lfDefFieldArr()
  *****************End of Array Filling********************
  
  IF laCntObj[2,1]
    IF !EMPTY(laTags[1])
      lnCount=IIF(lnCount >=3,3,lnCount+1)
     ** Define the window for the PopUp **
      DO CASE
        CASE _DOS   
          DEFINE WINDOW (LISTWIND);
           FROM WLROW(lcBrowCnt)-2-lnCount,WCOLS(lcBrowCnt)-64;
             TO WLROW(lcBrowCnt)-1,WCOLS(lcBrowCnt)-63.5+;
                lnLenth*9/7;
                NOSHADOW;
                NONE;
                COLOR SCHEME 9
       CASE _WINDOWS
         DEFINE WINDOW (LISTWIND);
            AT WLROW(lcBrowCnt)-2-lnCount,WLCOL(lcBrowCnt);
            SIZE  2+lnCount,lnLenth*9/7;
            NOSHADOW;
            NONE    
       
*         DEFINE WINDOW (LISTWIND);
            FROM WLROW(lcBrowCnt)-2-lnCount,WCOLS(lcBrowCnt)-81;
              TO WLROW(lcBrowCnt)+0.100,WCOLS(lcBrowCnt)-81.900+;
                 lnLenth*9/7;
                 NOSHADOW;
                 NONE    ;
                 COLOR SCHEME 9
      ENDCASE  
      =gffillpop('puTags','LATAGS')
    ENDIF
  ENDIF  
ENDIF

*!**************************************************************************
*!
*!      Function : lfResBrSet
*!
*!**************************************************************************
*
FUNCTION lfResBrSet
*** Retrive the stat of Tags From Array ***
IF !(TYPE("laSavStat")$"UL")
  FOR lnCount=1  TO ALEN(laSavStat,1)
    IF !EMPTY(laSavStat[lnCount])
      SET ORDER TO &laSavStat[lnCount]
    ENDIF  
  ENDFOR
ENDIF  

IF !EMPTY(lcSavTag)
  IF !("DESCENDING" $ lcSavTag)
    lcSavTag = lcSavTag + ' ASCENDING'
  ENDIF
  SET ORDER TO &lcSavTag  
ELSE
  SET ORDER TO
ENDIF 

*** Retrive the stat of Filter ***
IF !EMPTY(lcSavFilt)
  SET FILTER TO &lcSavFilt
ELSE 
  SET FILTER TO  
ENDIF

*****filter screen
*!**************************************************************************
*!
*!      Function : lffilter
*!
*!**************************************************************************
*
FUNCTION lffilter
EXTERNAL ARRAY laData,laKeyField,laField_H,laField_N
PRIVATE lcCurFile
lnCount  = 1
=lfDefFieldArr()
lcOldExpr = lcFiltExp

IF TYPE('lcOldExpr') $ "UL"
  lcOldExpr = ""
ENDIF  


****************
*B600455,1 MAN 06/16/95 Changed Calling GETEXPR to be AGETEXPR
=gfBFiltr('laFiltrExp',@laField_H,@laField_N,'F')

*MAB HERE 09/28/1999 [Begin]
IF WEXIST(lcWinTitl)
  SHOW WINDOW (lcWinTitl) REFR  
ENDIF
*MAB HERE 09/28/1999 [End  ]

*=lfSetFilter()
LOCATE
IF !FOUND()
  =gfDialog('I',"There is no records in the scope please be sure of the filter ") 
  lcFiltExp=''
  =lfSetFilter()
ENDIF   
RETURN


*!**************************************************************************
*!
*!      Function : lfSetFilter
*!
*!**************************************************************************
*
FUNCTION lfSetFilter
IF !EMPTY(lcFiltExp)
  IF EMPTY(gcFiltExp) 
    lcFiltExp1= UPPER(ALLTRIM(lcFiltExp))
  ELSE   
    lcFiltExp1 = ALLTRIM(gcFiltExp)+" .AND. "+ALLTRIM(lcFiltExp)
  ENDIF
  SET FILTER TO &lcFiltExp1
ELSE
  IF EMPTY(gcFiltExp) 
    SET FILTER TO
  ELSE   
    SET FILTER TO &gcFiltExp
  ENDIF
ENDIF
RETURN

*****Valid functions for find screen 
*!**************************************************************************
*!
*!      Function: lfFind1
*!
*!**************************************************************************
*
FUNCTION lfFind1
EXTERNAL ARRAY laData,laKeyField,laField_H,laField_N
PRIVATE lcCurFile
rbValExp = 1
lnCount = 1
lvSought= " "
=lfDefFieldArr()

IF TYPE('lcOldExpr') $ "UL"
  PUBLIC lcOldExpr
  lcOldExpr = ""
ENDIF  
*-- MAN Changed the name of the screen LWFIND to LWASFIND to void calling
*-- MAN the existing one in Aria Advantage
DO (qsd+'LWASFIND.SPR')

*!**************************************************************************
*!
*!      Function: lfShowExp
*!
*!**************************************************************************
*
FUNCTION lfShowFind
IF RDLEVEL() > 4
  SHOW GET rbValExp,2 DISABLE
  SHOW GET puExp DISABLE
ENDIF

*!**************************************************************************
*!
*!      Function: lfvValExp
*!
*!**************************************************************************
*
FUNCTION lfvValExp
DO CASE
CASE rbValExp = 1
  SHOW GET puExp DISABLE
  lvExpres = " "
  SHOW GET lvExpres DISABLE
  SHOW GET puField_H ENABLE
  SHOW GET lvSought ENABLE
  SHOW GET puOperat ENABLE
CASE rbValExp = 2
  SHOW GET puExp ENABLE
  SHOW GET lvExpres ENABLE
  SHOW GET puField_H DISABLE
  lvSought = " "
  SHOW GET lvSought DISABLE
  SHOW GET puOperat DISABLE
ENDCASE
SHOW GETS

*!**************************************************************************
*!
*!      Function : lfvSought
*!
*!**************************************************************************
*
FUNCTION lfvSought
IF !EMPTY(lvSought)
  IF "'"$lvSought
    SET CURSOR OFF
    WAIT WINDOW "Can't have single quote in value" NOWAIT
    SET CURSOR ON
    RETURN .F.
  ENDIF
  IF TYPE('&puField_H') = "L"
    IF AT('.T.',UPPER(lvSought))=0 AND;
       AT('.F.',UPPER(lvSought))=0 AND ;
       NOT EMPTY(lvSought)
      WAIT WINDOW "Logical values must have periods around them" NOWAIT
      RETURN .F.
    ENDIF
  ENDIF
ENDIF  

*!**************************************************************************
*!
*!      Function : lfvExp
*!
*!**************************************************************************
*
FUNCTION lfvExp
IF llEmpty
  *B600455,1 MAN 06/16/95 Changed Calling GETEXPR to be AGETEXPR
  lvExpres=AGetExpr() 
ELSE
  *B600455,1 MAN 06/16/95 Changed Calling GETEXPR to be AGETEXPR
  lvExpres=AGetExpr(lvExpres,ALIAS()," ","laField_H",@laField_N)
ENDIF  
SHOW GET lvExpres
SHOW GETS 

*!**************************************************************************
*!
*!      Function : lfvExpres
*!
*!**************************************************************************
*
FUNCTION lfvExpres
IF !EMPTY(lvExpres)
  IF(TYPE(lvExpres)="U")
    WAIT WINDOW "Not a valid expression"  NOWAIT
  ENDIF  
ENDIF           

*!**************************************************************************
*!
*!      Procedure: PostLoc
*!
*!**************************************************************************
*
PROCEDURE PostLoc
IF !FOUND()
  SET CURSOR OFF
  WAIT WINDOW "Not found" NOWAIT
  SET CURSOR ON
  IF lvOldRec <> 0  
    GO lvOldRec
  ENDIF  
  _CUROBJ=IIF(rbValExp = 1,OBJNUM(lvSought),OBJNUM(lvExpres))
ELSE
  CLEAR READ 
ENDIF

*!**************************************************************************
*!
*!      Function : lfvFindExp
*!
*!**************************************************************************
*
FUNCTION lfvFindExp
REGIONAL lvHold, lvType, lvOldRec
IF BOF() OR EOF()
  lvOldRec=0
ELSE  
  lvOldRec = RECNO()
ENDIF
lvHold = UPPER(ALLTRIM(lvSought))
IF EMPTY(lvHold) OR EMPTY(puField_H)
  WAIT WINDOW "Missing field or value" NOWAIT
  RETURN .F.
  _CUROBJ=OBJNUM(lvSought)
ELSE
   IF OCCURS('"',lvHold)>0 .OR. OCCURS("'",lvHold)>0
     WAIT WINDOW "Can't have double quotes in value" NOWAIT
     RETURN .F.
     _CUROBJ=OBJNUM(lvSought)
   ENDIF         
   lvHold = laField_N[ASCAN(laField_H,puField_H)]+ALLTRIM(puOperat)+'"'+lvHold+'"'
   lnType = ASCAN(laField_H,puField_H)      
   lcChk  = "&laField_N["+ALLTRIM(STR(lnType,LEN(STR(lnType)),0))+"]"
   lvType = TYPE(laField_N[lnType])
   DO CASE
     CASE lvType = "C" OR lvType = "M" 
       lvHold = "ALLTRIM(UPPER("+SUBSTR(lvHold,1,AT(puOperat,lvHold)-1)+"))"+;
                 SUBSTR(lvHold,AT(puOperat,lvHold),;
                 LEN(lvHold)-AT(puOperat,lvHold)+1)           
     CASE lvType = "D"
       IF OCCURS('"',lvHold)>0
         lvHold = STRTRAN(lvHold,'"','CTOD("',1,1)
         lvHold = STRTRAN(lvHold,'"','")',2,1)
       ENDIF
     CASE lvType = "N"
      lvHold = STRTRAN(lvHold,'"','',1)
     CASE lvType = "L"
       lvHold = STRTRAN(lvHold,'"','',1)
   ENDCASE
   GO TOP
      *B601438,1 Hesham El-Sheltawi (Start)
      IF TYPE('lcFltrExp') = 'C' AND !EMPTY(lcFltrExp)
        PRIVATE lcKeyToSeek 
        lcKeyToSeek = lcFltrExp
        DO CASE
          CASE  ATC("',",lcFltrExp) > 0
            lcKeyToSeek = SUBSTR(lcFltrExp,1,ATC("',",lcFltrExp))
          CASE  ATC('",',lcFltrExp) > 0
            lcKeyToSeek = SUBSTR(lcFltrExp,1,ATC('",',lcFltrExp))
        ENDCASE  
        =SEEK(EVAL(lcKeyToSeek))
        LOCATE REST FOR &lvHold      
      ELSE
        LOCATE FOR &lvHold      
      ENDIF
*      LOCATE FOR &lvHold
      *B601438,1 Hesham El-Sheltawi (End)      
   DO PostLoc
 ENDIF
lcOldExpr = lvExpres

*!**************************************************************************
*!
*!      Function : lfvCancel
*!
*!**************************************************************************
*
FUNCTION lfCloseFind
CLEAR READ


****** validation functions for Goto screen
*!**************************************************************************
*!
*!      Function : lfGoTo1
*!
*!**************************************************************************
*
FUNCTION lfGoTo1
lcCurRec=RECNO()
lnRecord=0
DO (qsd+'lwASGoTo.SPR')

*!**************************************************************************
*!
*!      Function : lfvOption
*!
*!**************************************************************************
*
FUNCTION lfvOption
IF rbGoTo = 3
  SHOW GET lnRecord ENABLE
  _CUROBJ=OBJNUM(lnRecord)
ELSE
  SHOW GET lnRecord DISABLE  
ENDIF
 
*!**************************************************************************
*!
*!      Function : lfvRecord
*!
*!**************************************************************************
*
FUNCTION lfvRecord
 IF lnRecord > RECCOUNT()
   WAIT WINDOW "Record No. is out of range" NOWAIT
   lnRecord = 0  
   _CUROBJ=OBJNUM(lnRecord)
 ENDIF
   
*!**************************************************************************
*!
*!      Function : lfvGoTo
*!
*!**************************************************************************
*
FUNCTION lfvGoToProc
DO CASE
  CASE rbGoTo = 1
    GO TOP  
  CASE rbGoTo = 2
    GO BOTTOM
  CASE rbGoTo = 3
    IF lnRecord >0
      GO lnRecord
    ENDIF
ENDCASE

*!**************************************************************************
*!
*!      Function : lfvGoToCanc
*!
*!**************************************************************************
*
FUNCTION lfvGoToCanc
GO lcCurRec

**********Validation functions for locate screen
*!**************************************************************************
*!
*!      Function : lfvShowLoc
*!
*!**************************************************************************
*
FUNCTION lfLocate1
PARAMETERS lcCurField

lnCount  = 1
lnCurRec = RECNO()  
lcLocExp =""

DO (qsd+'lwASLocate.SPR')

*!**************************************************************************
*!
*!      Function : lfvShowLoc
*!
*!**************************************************************************
*
FUNCTION lfShowLoc
SHOW GET pbActFind,1 DISABLE

*!**************************************************************************
*!
*!      FUNCTION :  lfMakeExp
*!
*!**************************************************************************
*
FUNCTION lfMakeExp
PARAMETERS CTYPE,CFIELD
lcRet=""
** Making the Expression **
DO CASE
  CASE CTYPE="N"
  lcRet="lcFindExp"+IIF(cbMatch," = "," $ ")+IIF(cbIgnore,;
                  "UPPER(ALLTRIM(STR("+CFIELD+")))",";
                  ALLTRIM(STR("+CFIELD+"))")
  CASE CTYPE="D"
  lcRet="lcFindExp"+IIF(cbMatch," = "," $ ")+IIF(cbIgnore,;
                  "UPPER(ALLTRIM((DTOC("+CFIELD+")))",";
                  ALLTRIM(DTOC("+CFIELD+"))")
  CASE CTYPE="C" 
  lcRet="lcFindExp"+IIF(cbMatch," = "," $ ")+IIF(cbIgnore,;
                  "UPPER(ALLTRIM("+CFIELD+"))",";
                  ALLTRIM("+CFIELD+")")
ENDCASE
RETURN lcRet

*!**************************************************************************
*!
*!      Function : lfvLocateExp
*!
*!**************************************************************************
*
FUNCTION lfvLocateExp
lcLocExp=" "
lcLocExp=lcLocExp+lfMakeExp(TYPE(lcCurField),lcCurField) 
IF USED('CODES')
  lcBefCode=SELECT(0) 
  SELECT CODES 
  SET ORDER TO 1
  lnCodRec=IIF(RECNO()>RECCOUNT(),0,RECNO())
  IF SEEK(UPPER(lcCurField))
    GO TOP
    LOCATE FOR CFLD_NAME=UPPER(lcCurField) AND;
               cdiscrep =ALLTRIM(IIF(cbIgnore,UPPER(lcFindExp),lcFindExp))
               
     IF FOUND()
       lcFindExp=cCode_No
     ENDIF
  ENDIF   
  IF lnCodRec<>0
      GO lnCodRec
  ENDIF
  SELECT (lcBefCode)
ENDIF
  lcFindExp=ALLTRIM(IIF(cbIgnore,UPPER(lcFindExp),lcFindExp))

IF !EMPTY(lcLocExp)
  IF rbScope= 3
    GO Top
    LOCATE FOR &lcLocExp NEXT lnCurRec
  ELSE
    IF rbScope = 1
      GO TOP
      LOCATE FOR &lcLocExp
    ELSE
      LOCATE FOR &lcLocExp NEXT RECCOUNT()-RECNO()
    ENDIF
  ENDIF  
  IF !FOUND()
    WAIT WINDOW "Not found" NOWAIT
    GO lnCurRec
  ENDIF  
ENDIF

*!**************************************************************************
*!
*!      Function : lfvCancelLoc
*!
*!**************************************************************************
*
FUNCTION lfvCancelLoc
lcLocExp=" "
CLEAR READ

*E301050,1 Hesham (Start)
*!**************************************************************************
*!
*!      Function : lfRtMouse
*!
*!**************************************************************************
* Function To trap right mouse click
FUNCTION lfRtMouse
IF  (MROW(lcWinTitl)<>-1) AND (MCOL(lcWinTitl)<>-1) AND (MROW(lcWinTitl)>lnLineRange);
      AND BETWEEN(MCOL(lcWinTitl),2,WCOL(lcWinTitl)-IIF(_DOS OR _UNIX,2,1.77));
     AND BETWEEN(MROW(lcWinTitl),2,WROWS(lcWinTitl)-IIF(_DOS OR _UNIX,2,1.77))

  DO lfvSelect
ENDIF  

*!*************************************************************
*! Name      : lfEndObj
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 12/20/97
*! Purpose   : Put cursor at end of string.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfEndObj()
*!*************************************************************
*! Note      : I separate it for some case we want changes 
*!           : to affect refresh browse only.
*!*************************************************************
*
*B602499,1
FUNCTION lfEndObj
KEYBOARD "{END}"
*-- end of lfEndObj.


*!*************************************************************
*! Name      : lfCheckClr
*! Developer : Walid A. Wahab (WAB)
*! Date      : 04/01/2002
*! Purpose   : Check if the style code structure contain Color .
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfCheckClr()
*!*************************************************************
*! Note      : 
*!*************************************************************
*
*E301853,1
FUNCTION lfCheckClr
PRIVATE llStructUse,lnNoSeg,lnPosition,lnAlias
lnAlias=ALIAS()

llStructUse = .F.
IF !USED('ICISTRU')
  USE (gcDataDir+'ICISTRU') IN 0
  llStructUse = .T.
ENDIF
SELECT ICISTRU
SET ORDER TO TAG SEGNO
=SEEK('U1')
lnNoSeg = 0
lnPosition = 1
SCAN WHILE citemrecty+cisegno = 'U'
  IF ALLT(cisegtype) = 'C'
    llUseColor = .T.
    lnClrStrt  = lnPosition
    lnClrWidth = nisegsize 
  ENDIF
  lnPosition         = lnPosition + nisegsize  + LEN(ALLT(CISEGSEPR))
ENDSCAN

IF llStructUse AND USED('ICISTRU')
  USE IN ICISTRU
ENDIF

SELECT (lnAlias)
