*:*******************************************************************************
*: Program file  : SMRPTMP.Prg
*: Program desc. : Report templates program. (N00341)
*: For screen    : SMRPTMP.Scx
*:         System: ARIA APPAREL SYSTEM
*:         Module: SM
*:      Developer: AHMED MOUSTAFA (AHS)
*********************************************************************************
* Modifications:
* E302650,1 MMT 12/14/2009 Change report template screen design[T20091118.0003]
* E302650,2 MMT 12/22/2009 Change report template screen design[T20091118.0003]
* B609153,1 MMT 02/21/2010 Allow Style Profile to appear in all Types[T20091118.0003]
* E302694,1 MMT 05/16/2010 Add Shortcut to Project Monitor to complete tasks[T20100324.0007]
* B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[T20101104.0001]
*********************************************************************************
*: Calls :
*:         Functions  : lfvTmpCode
*:                    : lfvNew
*:                    : lfvModify
*:                    : lfvRemove
*:                    : lfvOk
*:                    : lfvclose
*:                    : lfvTmpCode
*:                    : lfvOper
*********************************************************************************
#INCLUDE R:\Aria4XP\Screens\sm\SMRPTMP.H
llInsert    = .F.                && variable to hold if insert or not.
lcTmpCode   = SPACE(3)           && variable to hold Template code.
lcDesc      = SPACE(30)          && variable to hold Description.
lnBrRecNo   = 0                  && variable to hold browse record number.
lcFillMemo  = ' '                && variable to hold operation memo field string.

DO FORM (oAriaApplication.ScreenHome+'SM\SMRPTMP.SCX')
*!*************************************************************
*! Name      : lfvStr2Arr
*! Developer : AHMED MOUSTAFA (AHS)
*! Date      : 1/12/2009
*! Purpose   : Put String elements into Array
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfvStr2Arr()
*!*************************************************************
FUNCTION lfvStr2Arr
PARAMETERS lcStr

DIMENSION laSelOprs[1]
laArr= laSelOprs[1]
lcTxt = lcStr + '|'

FOR lnN=1 TO OCCURS('|',lcTxt)
  lcSepPos = ATC('|',lcTxt)
  DIMENSION laSelOprs[lnN]

  *E302650,1 MMT 12/14/2009 Change report template screen design[Start]
  lcText1 = SUBSTR(lcTxt,1,lcSepPos-1)
  *E302650,1 MMT 12/14/2009 Change report template screen design[End]

  *E302650,1 MMT 12/14/2009 Change report template screen design[Start]
  *laSelOprs[lnN] = SUBSTR(lcTxt,1,lcSepPos-1)
  laSelOprs[lnN] = SUBSTR(lcText1 ,1,3) + ' - ' +SUBSTR(lcText1 ,4)
  *E302650,1 MMT 12/14/2009 Change report template screen design[End]
  lcTxt = SUBSTR(lcTxt,lcSepPos+1)
ENDFOR
*!*************************************************************
*! Name      : lfvRemove
*! Developer : AHMED MOUSTAFA (AHS)
*! Date      : 01/05/2009
*! Purpose   : validation on push button remove.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfvRemove()
*!*************************************************************
FUNCTION lfvRemove
PARAMETERS loRemove,loModify,loOper,loGrid

lnChoice = gfModalgen("TRM38228B00030","DIALOG")
SELECT PMRPRTM
lnBrRecNo = RECNO()

IF lnChoice = 1
  GOTO lnBrRecNo
  =gfdelete()

  lnTable = gfGetRemoteTable(SET("Datasession"),'PMRPRTM')
  IF lnTable<>0
    SELECT (oAriaApplication.laRemoteTable[lnTable].lcCursorUpdate)
    TABLEUPDATE()
    gfAdd_Info(oAriaApplication.laRemoteTable[lnTable].lcCursorUpdate)
    SELECT 'PMRPRTM'
  ENDIF

  =gftableupdate(.T.,'PMRPRTM')

  IF lnBrRecNo=1
     GO TOP
  ELSE
     GOTO lnBrRecNo-1
  ENDIF

  loGrid.Refresh()
  loGrid.setfocus()

ELSE

 GOTO lnBrRecNo
 RETURN

ENDIF

*-- check if there is no records in PMRPRTM file,
*-- no need from modify, or delete operation.
IF EMPTY(cTmp_Code)
  loRemove.enabled = .F.
  loModify.enabled = .F.
  loOper.enabled = .F.
ENDIF
*!*************************************************************
*! Name      : lfvOk
*! Developer : AHMED MOUSTAFA (AHS)
*! Date      : 1/12/2009
*! Purpose   : validation on push button Ok.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfvOk()
*!*************************************************************
FUNCTION lfvOk
PARAMETERS locmdOk,locmdTasks,lcTmpCode,lcDesc,loFormset,lnCurRec

  *-- Prevent user from saving template has no operation.
IF EMPTY(lcFillMemo)
  =gfModalgen("TRM38226B00000","DIALOG",'saving')
ELSE

  *-- Prevent add same code more than one time. (Begin)
  IF llInsert
    IF SEEK (lcTmpCode)
      lnChoice = gfModalgen("TRM38227B38031","DIALOG",TRIM(lcTmpCode))
      IF lnChoice = 1
        llInsert=.F.
        RETURN
      ENDIF
    ENDIF
  ENDIF

  *-- if inserted new record it means not update.
  IF llInsert
     locmdOk.enabled = .T.
     locmdTasks.enabled = .T.
    APPEND BLANK
  ELSE
    GOTO lnCurRec
  ENDIF
 * REPLACE cTmp_Code WITH lcTmpCode,;
           cTmp_Dsc  WITH lcDesc,;
          *mTmp_Oprt WITH lcFillMemo...

  gfreplace('cTmp_Code with lcTmpCode')
  gfreplace('cTmp_Dsc with lcDesc')
  gfreplace('mTmp_Oprt with lcFillMemo')
  *
  m.cAdd_user = oAriaApplication.User_id
  m.cadd_time = TIME()
  m.dadd_date = oAriaApplication.systemdate
  m.cadd_ver = oAriaApplication.cshortversion
  *
  gfAdd_Info('PMRPRTM')

  gfReplace('')

  loFormset.refresh()
  loFormset.release()

  *-- To update the table after adding records
  =gftableupdate(.T.,'PMRPRTM')

  llInsert   = .F.
  lnBrRecNo  = RECNO('PMRPRTM')
  CLEAR READ
ENDIF
*!*************************************************************
*! Name      : lfvTmpCode
*! Developer : AHMED MOUSTAFA (AHS)
*! Date      : 1/12/2009
*! Purpose   : Template Code validation.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfvTmpCode()
*!*************************************************************
FUNCTION lfvTmpCode
PARAMETERS locmdOper,locmdOk,lcTmpCode

IF SEEK(lcTmpCode)
  lnChoice = gfModalgen("TRM38227B00000","DIALOG",TRIM(lcTmpCode))
  lcTmpCode = SPACE(3)
ENDIF

IF EMPTY(lcTmpCode)
  lcDesc = SPACE(30)
  locmdOper.enabled = .F.
  locmdOk.enabled = .F.
ELSE
  locmdOper.ENABLED = .T.
  locmdOk.ENABLED = .T.
ENDIF

*E302650,1 MMT 12/14/2009 Change report template screen design[Start]
FUNCTION lfInitForm
PARAMETERS loFormSet
=gfOpenTable("PMCTGHD","PMCTGHD", "SH") && Contains Catogries
=gfOpenTable('PMRPRTM','PMRPRTM','SH')  && Contains Templates
=gfOpenTable("PMCTGDT","PMCTGDT", "SH") && Contains Catogries
=gfOpenTable("PMRPTMDT","PMRPTMDS", "SH")  && Contains Templates Details
=gfOpenTable("CODES","IDRLTFNAME", "SH")  && Codes
lfCrtTemp(loFormSet)

lnFileResult = oAriaApplication.remotesystemdata.execute;
        ("SELECT SYDFILES.cfile_nam,sydFiles.cfile_ttl  FROM SYDFILES ;
        WHERE SYDFILES.cfile_nam in ('POSHDR  ','POSLN   ','CUTPICK ','ITEM    ','PMPRJHD ')",'','TmpSqlFILES',"",oAriaApplication.cAria4Sysfiles,3,;
          "",SET("Datasession"))

IF lnFileResult > 0
  SELECT TmpSqlFILES
  =CURSORSETPROP("Buffering" ,3)
  INDEX on cfile_nam TAG 'SqlFILES'
ENDIF

lnFileResult = oAriaApplication.remotesystemdata.execute;
        ("SELECT SYDFILES.cfile_nam,sydFiles.cfile_ttl  FROM SYDFILES ;
        WHERE SYDFILES.cfile_nam in ('ORDHDR  ','ORDLINE ','CUSTOMER','STYLE   ','APVENDOR')",'','TmpFoxFILES',"",oAriaApplication.SystemConnectionString,3,;
          "",SET("Datasession"))
IF lnFileResult > 0
  SELECT TmpFoxFILES
   =CURSORSETPROP("Buffering" ,3)
  INDEX on cfile_nam TAG 'FoxFILES'
ENDIF


*Sql SyDField
*N000682,1 MMT 03/06/2013 Merge SysFiles and SqlDictionary folder[Start]
*!*	lnFielDResult = oAriaApplication.remotesystemdata.execute;
*!*	        ("SELECT sydfIEld.cfld_head,sydfIEld.cfld_name  FROM SYDFIELD",'','TmpSQLFIELD',"",oAriaApplication.cAria4Sysfiles,3,;
*!*	          "",SET("Datasession"))
lnFielDResult = oAriaApplication.remotesystemdata.execute;
        ("SELECT sydfIEld.cfld_head,sydfIEld.cfld_name  FROM SYDFIELD WHERE (CVER='A40' OR EMPTY(CVER))",'','TmpSQLFIELD',"",oAriaApplication.cAria4Sysfiles,3,;
          "",SET("Datasession"))
*N000682,1 MMT 03/06/2013 Merge SysFiles and SqlDictionary folder[End]          
   *N000682,1 MMT 12/09/2012 Globalization changes[Start]
   IF lnFielDResult> 0  AND oAriaApplication.oActivelang.cLang_ID <> "EN" AND USED("TmpSQLFIELD") 
     lcA40FIELDXML = OARIAAPPLICATION.LangPath + "Sysfiles40\SYDFIELD_DBF.XML"
    IF FILE(lcA40FIELDXML)
      lcA40FIELD =gfTempName()
      lnRecordCountReturned = XMLTOCURSOR(lcA40FIELDXML,lcA40FIELD ,512)
      IF lnRecordCountReturned > 0
        SELECT(lcA40FIELD)  
        INDEX on row_key TAG (lcA40FIELD)  
      ENDIF 
      SELECT TmpSQLFIELD
      LOCATE
      SCAN
        IF SEEK(ALLTRIM(TmpSQLFIELD.cfld_name) ,lcA40FIELD ,lcA40FIELD )
           REPLACE CFLD_HEAD WITH ALLTRIM(EVALUATE(lcA40FIELD +'.langtext'))
        ENDIF 
      ENDSCAN 
      TABLEUPDATE(.T.)
    ENDIF 
  ENDIF   
   *N000682,1 MMT 12/09/2012 Globalization changes[END]	    
          
IF lnFielDResult > 0
  SELECT TmpSQLFIELD
   =CURSORSETPROP("Buffering" ,3)
  INDEX on cfld_name TAG 'SQLFIELD'
  FOR lnC = 1 TO 8
    APPEND BLANK
    * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[Start]
*!*	    REPLACE cfld_name  WITH 'CLCOPN'+STR(lnC,1),;
*!*	            cfld_head  WITH 'Open PO Line Qty #'+STR(lnC,1)
*!*	    APPEND BLANK
*!*	    REPLACE cfld_name  WITH 'CLCREC'+STR(lnC,1),;
*!*	            cfld_head  WITH 'Received PO Line Qty #'+STR(lnC,1)
*!*	    APPEND BLANK
*!*	    REPLACE cfld_name  WITH 'CLCINT'+STR(lnC,1),;
*!*	            cfld_head  WITH 'In Transit PO Line Qty #'+STR(lnC,1)
    REPLACE cfld_name  WITH 'CLCOPN'+STR(lnC,1),;
            cfld_head  WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMRPTTMP_OPNQTYL,loFormSet.GetHeaderText("LANG_SMRPTTMP_OPNQTYL",loFormSet.HeaderAlias))+STR(lnC,1)

    APPEND BLANK
    REPLACE cfld_name  WITH 'CLCREC'+STR(lnC,1),;
            cfld_head  WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMRPTTMP_RCVQTYL,loFormSet.GetHeaderText("LANG_SMRPTTMP_RCVQTYL",loFormSet.HeaderAlias))+STR(lnC,1)


    APPEND BLANK
    REPLACE cfld_name  WITH 'CLCINT'+STR(lnC,1),;
            cfld_head  WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMRPTTMP_INTQTYL,loFormSet.GetHeaderText("LANG_SMRPTTMP_INTQTYL",loFormSet.HeaderAlias))+STR(lnC,1)


    * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[End]
  ENDFOR
  APPEND BLANK
  * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[Start]
*!*	  REPLACE cfld_name  WITH 'CPOSUM',;
*!*	          cfld_head  WITH 'Purchase Order Summary'

*!*	  APPEND BLANK
*!*	  REPLACE cfld_name  WITH 'CLCTOTOPN',;
*!*	          cfld_head  WITH 'Open PO Line TotQty'
*!*	
*!*	  APPEND BLANK
*!*	  REPLACE cfld_name  WITH 'CLCTOTINT',;
*!*	          cfld_head  WITH 'In Tran. PO Line TotQty'
*!*	
*!*	  APPEND BLANK
*!*	  REPLACE cfld_name  WITH 'CLCTOTREC',;
*!*	          cfld_head  WITH 'Total Rec. PO Line Qty'
*!*	  APPEND BLANK
*!*	  REPLACE cfld_name  WITH 'MUSRCOMMPO',;
*!*	          cfld_head  WITH 'Purchase Order Notes'
  REPLACE cfld_name  WITH 'CPOSUM',;
         cfld_head  WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMRPTTMP_POCTSUM,loFormSet.GetHeaderText("LANG_SMRPTTMP_POCTSUM",loFormSet.HeaderAlias))


  APPEND BLANK
  REPLACE cfld_name  WITH 'CLCTOTOPN',;
          cfld_head  WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMRPTTMP_OPNTOTQTY,loFormSet.GetHeaderText("LANG_SMRPTTMP_OPNTOTQTY",loFormSet.HeaderAlias))

  APPEND BLANK
  REPLACE cfld_name  WITH 'CLCTOTINT',;
          cfld_head  WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMRPTTMP_INTTOTQTY,loFormSet.GetHeaderText("LANG_SMRPTTMP_INTTOTQTY",loFormSet.HeaderAlias))


  APPEND BLANK
  REPLACE cfld_name  WITH 'CLCTOTREC',;
          cfld_head  WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMRPTTMP_RCVTOTQTY,loFormSet.GetHeaderText("LANG_SMRPTTMP_RCVTOTQTY",loFormSet.HeaderAlias))

  APPEND BLANK
  REPLACE cfld_name  WITH 'MUSRCOMMPO',;
          cfld_head  WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMRPTTMP_PONOTE	,loFormSet.GetHeaderText("LANG_SMRPTTMP_PONOTE	",loFormSet.HeaderAlias))


  APPEND BLANK
  REPLACE cfld_name  WITH 'DPORCDAT',;
         cfld_head  WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMRPTTMP_POLSTREC,loFormSet.GetHeaderText("LANG_SMRPTTMP_POLSTREC",loFormSet.HeaderAlias))

   * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[End]
ENDIF

*Fox
*N000682,1 MMT 03/06/2013 Merge SysFiles and SqlDictionary folder[Start]
*!*	lnFielDResult = oAriaApplication.remotesystemdata.execute;
*!*	        ("SELECT sydfIEld.cfld_head,sydfIEld.cfld_name  FROM SYDFIELD",'','TmpFoxFIELD',"",oAriaApplication.SystemConnectionString,3,;
*!*	          "",SET("Datasession"))
lnFielDResult = oAriaApplication.remotesystemdata.execute;
        ("SELECT sydfIEld.cfld_head,sydfIEld.cfld_name  FROM SYDFIELD WHERE (CVER='A27' OR EMPTY(CVER))",'','TmpFoxFIELD',"",oAriaApplication.SystemConnectionString,3,;
          "",SET("Datasession"))
*N000682,1 MMT 03/06/2013 Merge SysFiles and SqlDictionary folder[END]          
   *N000682,1 MMT 12/09/2012 Globalization changes[Start]
   IF lnFielDResult > 0  AND oAriaApplication.oActivelang.cLang_ID <> "EN" AND USED("TmpFoxFIELD") 
     lcA27FIELDXML = OARIAAPPLICATION.LangPath + "Sysfiles27\SYDFIELD_DBF.XML"
    IF FILE(lcA27FIELDXML)
      lcA27FIELD =gfTempName()
      lnRecordCountReturned = XMLTOCURSOR(lcA27FIELDXML,lcA27FIELD,512)
      IF lnRecordCountReturned > 0
        SELECT(lcA27FIELD)  
        INDEX on row_key TAG (lcA27FIELD)  
      ENDIF 
      SELECT TmpFoxFIELD
      LOCATE
      SCAN
        IF SEEK(ALLTRIM(TmpFoxFIELD.cfld_name) ,lcA27FIELD,lcA27FIELD)
           REPLACE CFLD_HEAD WITH ALLTRIM(EVALUATE(lcA27FIELD+'.langtext'))
        ENDIF 
      ENDSCAN 
      TABLEUPDATE(.T.)
    ENDIF 
  ENDIF   
   *N000682,1 MMT 12/09/2012 Globalization changes[END]	
          
IF lnFielDResult > 0
  SELECT TmpFoxFIELD
   =CURSORSETPROP("Buffering" ,3)
  INDEX on cfld_name TAG 'FoxFIELD'
  APPEND BLANK
  * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[Start]
*!*	  REPLACE cfld_name  WITH 'CSOSUM',;
*!*	          cfld_head  WITH 'Sales Order Summary'
*!*	  APPEND BLANK
*!*	  REPLACE cfld_name  WITH 'MUSRCOMMSO',;
*!*	          cfld_head  WITH 'Sales Order Notes'
  REPLACE cfld_name  WITH 'CSOSUM',;
          cfld_head  WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMRPTTMP_SOSUMM,loFormSet.GetHeaderText("LANG_SMRPTTMP_SOSUMM",loFormSet.HeaderAlias))
  APPEND BLANK
  REPLACE cfld_name  WITH 'MUSRCOMMSO',;
          cfld_head  WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMRPTTMP_SONOTE,loFormSet.GetHeaderText("LANG_SMRPTTMP_SONOTE",loFormSet.HeaderAlias))
 * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[End]

ENDIF



DIMENSION loFormSet.laTempArr[11,2]
STORE '' TO loFormSet.laTempArr
*!*  loFormSet.laTempArr[1,1] = 'Cutting Ticket'
*!*  loFormSet.laTempArr[1,2] = 'C'
*!*  loFormSet.laTempArr[2,1] = 'Purchase Order'
*!*  loFormSet.laTempArr[2,2] = 'P'
*!*  loFormSet.laTempArr[3,1] = 'Adorment Order'
*!*  loFormSet.laTempArr[3,2] = 'A'
*!*  loFormSet.laTempArr[4,1] = 'Dye Order'
*!*  loFormSet.laTempArr[4,2] = 'D'
*!*  loFormSet.laTempArr[5,1] = 'Inter-Location PO'
*!*  loFormSet.laTempArr[5,2] = 'N'
*!*  loFormSet.laTempArr[6,1] = 'Return PO'
*!*  loFormSet.laTempArr[6,2] = 'R'
*!*  loFormSet.laTempArr[7,1] = 'Sales Order'
*!*  loFormSet.laTempArr[7,2] = 'O'
*!*  loFormSet.laTempArr[8,1] = 'EDI Order'
*!*  loFormSet.laTempArr[8,2] = 'T'
*!*  loFormSet.laTempArr[9,1] = 'Style'
*!*  loFormSet.laTempArr[9,2] = 'S'
*!*  loFormSet.laTempArr[10,1] = 'Material'
*!*  loFormSet.laTempArr[10,2] = 'M'
*!*  loFormSet.laTempArr[11,1] = 'Other'
*!*  loFormSet.laTempArr[11,2] = 'H'

DIMENSION loFormSet.laTempArr[2,2]
STORE '' TO loFormSet.laTempArr
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.laTempArr[1,1] = LANG_MFPROJMON_STYLE
loFormSet.laTempArr[1,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFPROJMON_STYLE,loFormSet.GetHeaderText("LANG_MFPROJMON_STYLE",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

loFormSet.laTempArr[1,2]= 'S'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.laTempArr[2,1] = LANG_MFPROJMON_OTHER
loFormSet.laTempArr[2,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFPROJMON_OTHER,loFormSet.GetHeaderText("LANG_MFPROJMON_OTHER",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

loFormSet.laTempArr[2,2]= 'H'

IF 'MF' $ oAriaApplication.CompanyInstalledModules
  DIMENSION loFormSet.laTempArr[ALEN(loFormSet.laTempArr,1)+1,2]
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.laTempArr[ALEN(loFormSet.laTempArr,1),1] = LANG_MFPROJMON_CTKTKT
loFormSet.laTempArr[ALEN(loFormSet.laTempArr,1),1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFPROJMON_CTKTKT,loFormSet.GetHeaderText("LANG_MFPROJMON_CTKTKT",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  loFormSet.laTempArr[ALEN(loFormSet.laTempArr,1),2] = 'C'
ENDIF

IF 'PO' $ oAriaApplication.CompanyInstalledModules
  DIMENSION loFormSet.laTempArr[ALEN(loFormSet.laTempArr,1)+1,2]
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.laTempArr[ALEN(loFormSet.laTempArr,1),1] = LANG_MFPROJMON_PO
loFormSet.laTempArr[ALEN(loFormSet.laTempArr,1),1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFPROJMON_PO,loFormSet.GetHeaderText("LANG_MFPROJMON_PO",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  loFormSet.laTempArr[ALEN(loFormSet.laTempArr,1),2] = 'P'
ENDIF

IF 'SO' $ oAriaApplication.CompanyInstalledModules
  DIMENSION loFormSet.laTempArr[ALEN(loFormSet.laTempArr,1)+1,2]
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.laTempArr[ALEN(loFormSet.laTempArr,1),1] = LANG_MFPROJMON_SALORD
loFormSet.laTempArr[ALEN(loFormSet.laTempArr,1),1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFPROJMON_SALORD,loFormSet.GetHeaderText("LANG_MFPROJMON_SALORD",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  loFormSet.laTempArr[ALEN(loFormSet.laTempArr,1),2] = 'O'
ENDIF


IF 'MA' $ oAriaApplication.CompanyInstalledModules
  DIMENSION loFormSet.laTempArr[ALEN(loFormSet.laTempArr,1)+1,2]
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.laTempArr[ALEN(loFormSet.laTempArr,1),1] = LANG_MFPROJMON_MATERIAL
loFormSet.laTempArr[ALEN(loFormSet.laTempArr,1),1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFPROJMON_MATERIAL,loFormSet.GetHeaderText("LANG_MFPROJMON_MATERIAL",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  loFormSet.laTempArr[ALEN(loFormSet.laTempArr,1),2] = 'M'
ENDIF

loFormSet.ariaform1.cboTmpTyp.requery()

FUNCTION lfChangeMode
PARAMETERS loFormSet
lcTableDt = loFormSet.lcrpttdt
DO CASE
  CASE loFormSet.ActiveMode = 'V'
    SELECT (loFormSet.lcrptthdr)
    ZAP
    SELECT(loFormSet.lcrpttdt)
    ZAP
    SELECT 'PMRPRTM'
    =gfSeek(PMRPRTM.CTMP_TYPE+PMRPRTM.CTMP_CODE)
    loFormSet.AriaForm1.Tempkey.Keytextbox.Value =PMRPRTM.CTMP_CODE
    loFormSet.AriaForm1.txtName.Value =PMRPRTM.CTMP_DSC
    loFormSet.ariaform1.lstAdded.Clear()
    SCATTER MEMO MEMVAR
    INSERT INTO (loFormSet.lcrptthdr) FROM MEMVAR
    SELECT 'PMRPTMDT'
    =gfSetOrder('PMRPTMDS')
    =gfSeek(PMRPRTM.CTMP_TYPE+PMRPRTM.CTMP_CODE)
    SCAN REST WHILE CTMP_TYPE +CTMP_CODE+STR(NTMPSEQ,6)  = PMRPRTM.CTMP_TYPE+PMRPRTM.CTMP_CODE
      SCATTER MEMO MEMVAR
      INSERT INTO (loFormSet.lcrpttdt) FROM MEMVAR
      * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[Start]
      *loFormSet.ariaform1.lstAdded.AddItem(IIF(TYPE = 'F','Field',IIF(TYPE = 'P','Profile','Activity')))
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.ariaform1.lstAdded.AddItem(IIF(TYPE = 'F',LANG_SMRPTTMP_FIELD,IIF(TYPE = 'P',LANG_SMRPTTMP_PROFILE,LANG_SMRPTTMP_ACTIVITY)))
loFormSet.ariaform1.lstAdded.AddItem(IIF(TYPE = 'F',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMRPTTMP_FIELD,loFormSet.GetHeaderText("LANG_SMRPTTMP_FIELD",loFormSet.HeaderAlias)),IIF(TYPE = 'P',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMRPTTMP_PROFILE,loFormSet.GetHeaderText("LANG_SMRPTTMP_PROFILE",loFormSet.HeaderAlias)),IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMRPTTMP_ACTIVITY,loFormSet.GetHeaderText("LANG_SMRPTTMP_ACTIVITY",loFormSet.HeaderAlias)))))
*N000682,1 11/20/2012 MMT Globlization changes[End]

      * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[End]
      DO CASE
        CASE  TYPE = 'F'
  	    * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[Start]
          *IF INLIST(KEY1,'POSHDR  ','POSLN   ','CUTPICK ','ITEM    ','PMPRJHD ') AND SEEK(key1,'TmpSqlFILES')
          IF INLIST(SUBSTR(KEY1,1,8),'POSHDR  ','POSLN   ','CUTPICK ','ITEM    ','PMPRJHD ') AND SEEK(SUBSTR(KEY1,1,8),'TmpSqlFILES')
          * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[End]
            loFormSet.ariaform1.lstAdded.AddListItem(TmpSQLFILES.cfile_ttl ,loFormSet.ariaform1.lstAdded.NewItemID ,2)
            =SEEK(SUBSTR(KEY2,1,10),'TmpSQLFIELD')
            * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[Start]
            *loFormSet.ariaform1.lstAdded.AddListItem(TmpSQLFIELD.cfld_head,loFormSet.ariaform1.lstAdded.NewItemID ,3)
            loFormSet.ariaform1.lstAdded.AddListItem(IIF(EMPTY(PMRPTMDT.CDESCRIP) OR ISNULL(PMRPTMDT.CDESCRIP),TmpSQLFIELD.cfld_head,PMRPTMDT.CDESCRIP),loFormSet.ariaform1.lstAdded.NewItemID ,3)
            REPLACE cdesc WITH IIF(EMPTY(CDESCRIP) OR ISNULL(CDESCRIP),TmpSQLFIELD.cfld_head,CDESCRIP) IN (loFormSet.lcrpttdt)
		    * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[End]

          ELSE
            * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[Start]
            *IF INLIST(KEY1,'ORDHDR  ','ORDLINE ','CUSTOMER','STYLE   ','APVENDOR') AND SEEK(key1,'TmpFoxFILES')
            IF INLIST(SUBSTR(KEY1,1,8),'ORDHDR  ','ORDLINE ','CUSTOMER','STYLE   ','APVENDOR') AND SEEK(SUBSTR(KEY1,1,8),'TmpFoxFILES')
		    * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[End]
              loFormSet.ariaform1.lstAdded.AddListItem(TmpFoxFILES.cfile_ttl ,loFormSet.ariaform1.lstAdded.NewItemID ,2)
              =SEEK(SUBSTR(KEY2,1,10),'TmpFOXFIELD')
              * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[Start]
              *loFormSet.ariaform1.lstAdded.AddListItem(TmpFOXFIELD.cfld_head,loFormSet.ariaform1.lstAdded.NewItemID ,3)
              loFormSet.ariaform1.lstAdded.AddListItem(IIF(EMPTY(PMRPTMDT.CDESCRIP) OR ISNULL(PMRPTMDT.CDESCRIP),TmpFOXFIELD.cfld_head,PMRPTMDT.CDESCRIP),loFormSet.ariaform1.lstAdded.NewItemID ,3)
              REPLACE cdesc WITH IIF(EMPTY(CDESCRIP) OR ISNULL(CDESCRIP),TmpFOXFIELD.cfld_head,CDESCRIP) IN (loFormSet.lcrpttdt)
			  * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[End]
            ENDIF
          ENDIF
        CASE TYPE = 'A'
           =gfSeek(SUBSTR(KEY1,1,3),'PMCTGHD')
           loFormSet.ariaform1.lstAdded.AddListItem(  ALLTRIM(KEY1)+' '+PMCTGHD.CCTG_DSC,loFormSet.ariaform1.lstAdded.NewItemID ,2)
        OTHERWISE
          * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[Start]
          *loFormSet.ariaform1.lstAdded.AddListItem(IIF(KEY1 = 'ST','Style',IIF(KEY1 = 'CS','Customer','Vendor')),loFormSet.ariaform1.lstAdded.NewItemID ,2)
          loFormSet.ariaform1.lstAdded.AddListItem(IIF(ALLTRIM(KEY1) = 'ST','Style',IIF(ALLTRIM(KEY1) = 'CS','Customer','Vendor')),loFormSet.ariaform1.lstAdded.NewItemID ,2)
		  * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[End]
      ENDCASE

      IF TYPE = 'A' AND gfSeek(SUBSTR(KEY1,1,3)+SUBSTR(KEY2,1,5),'PMCTGDT','PMCTGDT')
        loFormSet.ariaform1.lstAdded.AddListItem(SUBSTR(KEY2,1,5)+' '+PMCTGDT.cOprt_Dsc ,loFormSet.ariaform1.lstAdded.NewItemID ,3)
      ELSE
        IF TYPE = 'P'
          lcFDesc   = gfCodDes(SUBSTR(KEY2 ,1,6), 'CPRO_CODE')
          loFormSet.ariaform1.lstAdded.AddListItem(lcFDesc   ,loFormSet.ariaform1.lstAdded.NewItemID,3)
*!*	        ELSE
*!*	          loFormSet.ariaform1.lstAdded.AddListItem(Key2,loFormSet.ariaform1.lstAdded.NewItemID,3)
        ENDIF
      ENDIF

      loFormSet.ariaform1.lstAdded.AddListItem(&lcTableDt..CTMP_TYPE +&lcTableDt..CTMP_CODE+&lcTableDt..TYPE+&lcTableDt..KEY1+&lcTableDt..KEY2 ,loFormSet.ariaform1.lstAdded.NewItemID,4)
    ENDSCAN
    loFormSet.ariaForm1.lstAdded.Enabled = .T.
    loFormSet.ariaForm1.lstAdded.MoverBars = .F.

  CASE loFormSet.ActiveMode = 'S'
    SELECT (loFormSet.lcrptthdr)
    ZAP
    SELECT(loFormSet.lcrpttdt)
    ZAP
    loFormSet.ariaform1.cboTmpTyp.Enabled = .T.
    loFormSet.ariaform1.cboTmpTyp.requery()
    loFormSet.ariaform1.cboTmpTyp.Value = 'S'
    loFormSet.ariaForm1.tempkey.Enabled = .T.
    loFormSet.ariaForm1.txtName.Enabled = .F.
    loFormSet.ariaform1.lstAdded.Clear()
    lcFltrExp = "('"+loFormSet.ariaForm1.cboTmpTyp.Value+"')"
    loFormSet.cBrowseFilter = "CTMP_TYPE in "+lcFltrExp +""
    loFormSet.ariaForm1.lstAdded.Enabled = .T.
    loFormSet.ariaForm1.lstAdded.MoverBars = .F.

  CASE loFormSet.ActiveMode = 'A'
    SELECT (loFormSet.lcrptthdr)
    ZAP
    SELECT(loFormSet.lcrpttdt)
    ZAP

    loFormSet.ariaform1.cboTmpTyp.Enabled = .F.
    loFormSet.ariaform1.cboTmpTyp.requery()
    loFormSet.ariaForm1.tempkey.Enabled = .F.
    loFormSet.ariaForm1.txtName.Enabled = .T.
    loFormSet.ariaform1.cmdActiv.Enabled = .T.
    loFormSet.ariaform1.cmdFlds.Enabled = .T.
    loFormSet.ariaform1.cmdProf.Enabled = IIF(loFormSet.ariaform1.cboTmpTyp.Value $ 'MH',.F.,.T.)
    loFormSet.ariaform1.lstAdded.Enabled = .T.
    loFormSet.ariaForm1.lstAdded.Enabled = .T.
    loFormSet.ariaForm1.lstAdded.MoverBars = .T.
    SELECT(loFormSet.lcrptthdr)
    APPEND BLANK
    REPLACE CTMP_TYPE  WITH loFormSet.ariaForm1.cboTmpTyp.Value,;
            CTMP_CODE  WITH loFormSet.ariaForm1.tempkey.Keytextbox.value
  CASE loFormSet.ActiveMode = 'E'
    loFormSet.ariaForm1.lstAdded.Enabled = .T.
    loFormSet.ariaForm1.lstAdded.MoverBars = .T.
    loFormSet.ariaform1.cboTmpTyp.Enabled = .F.
    loFormSet.ariaForm1.tempkey.Enabled = .F.
    loFormSet.ariaForm1.txtName.Enabled = .T.
ENDCASE

FUNCTION lfFldsClk
PARAMETERS loFormSet
lcType = loFormSet.ariaform1.cboTmpTyp.Value
lcTablesSql = "'PMPRJHD'"
lcTablesFox = ""
DIMENSION laTargetArr[1,2]
STORE '' TO laTargetArr

DO CASE
  CASE lcType = 'O' && Sales order
    *Ordhdr,Ordline,Style,Customer
    * IF PO Module is installed POSHDR,POSLN,CUTPICK,VENDOR
    lcTablesFox = lcTablesFox +  "'ORDHDR  ','ORDLINE ','STYLE   ','CUSTOMER'"
    IF 'PO' $ oAriaApplication.CompanyInstalledModules
      lcTablesFox = lcTablesFox + ",'APVENDOR'"
      lcTablesSql = lcTablesSql + ",'POSHDR  ','POSLN   ','CUTPICK '"
    ENDIF

  CASE lcType = 'M' && Material
    *Metrail
    lcTablesSql = lcTablesSql +  ",'ITEM    '"

  CASE lcType = 'P' && Purchase order
    *POSHDR,POSLN,CUTPICK,VENDOR
    *IF SO Module is installed Ordhdr,Ordline,Style,Customer
    lcTablesSql = lcTablesSql +  ",'POSHDR  ','POSLN   ','CUTPICK '"
    lcTablesFox = lcTablesFox + "'APVENDOR'"
    IF 'SO' $ oAriaApplication.CompanyInstalledModules
      lcTablesFox = lcTablesFox + ",'ORDHDR  ','ORDLINE ','STYLE   ','CUSTOMER'"
    ENDIF

  CASE lcType = 'C' && CUTTING TICKET
    *POSHDR,POSLN,CUTPICK,VENDOR
    *IF SO Module is installed Ordhdr,Ordline,Style,Customer
    lcTablesSql = lcTablesSql +  ",'POSHDR  ','POSLN   ','CUTPICK '"
    lcTablesFox = lcTablesFox + "'APVENDOR'"
    IF 'SO' $ oAriaApplication.CompanyInstalledModules
      lcTablesFox = lcTablesFox + ",'ORDHDR  ','ORDLINE ','STYLE   ','CUSTOMER'"
    ENDIF

  CASE lcType = 'S' && Style
    *Style
    lcTablesFox = "'STYLE   '"
ENDCASE
*,SYDFIELD.cdata_typ ,SYDFIELD.nfld_wdth ,SYDFIELD.nfld_dec
IF !EMPTY(lcTablesSql)

  *N000682,1 MMT 03/06/2013 Merge SysFiles and SqlDictionary folder[Start]
*!*	  lnFldResult = oAriaApplication.remotesystemdata.execute;
*!*	        ("SELECT SYDFIELD.CFLD_NAME,SYDFIELD.CFLD_HEAD,SYDFLFLD.cfile_nam FROM SYDFIELD,SYDFLFLD ;
*!*	        WHERE SYDFIELD.cfld_name = SYDFLFLD.cfld_name AND SYDFLFLD.cfile_nam in ("+lcTablesSql+;
*!*	        ") AND  SYDFLFLD.cfld_name NOT IN ('CADD_USER','CADD_TIME','DADD_DATE','LLOK_STAT','CLOK_USER','DLOK_DATE','CLOK_TIME','CEDIT_USER','CEDIT_TIME','DEDIT_DATE','CADD_VER','CEDT_VER') ORDER BY SYDFIELD.CFLD_HEAD",'','TmpFLFld',"",oAriaApplication.cAria4Sysfiles,3,;
*!*	          "",SET("Datasession"))
  lnFldResult = oAriaApplication.remotesystemdata.execute;
        ("SELECT SYDFIELD.CFLD_NAME,SYDFIELD.CFLD_HEAD,SYDFLFLD.cfile_nam FROM SYDFIELD,SYDFLFLD ;
        WHERE (SYDFIELD.CVER='A40' OR EMPTY(SYDFIELD.CVER)) AND SYDFIELD.cfld_name = SYDFLFLD.cfld_name AND SYDFLFLD.cfile_nam in ("+lcTablesSql+;
        ") AND  SYDFLFLD.cfld_name NOT IN ('CADD_USER','CADD_TIME','DADD_DATE','LLOK_STAT','CLOK_USER','DLOK_DATE','CLOK_TIME','CEDIT_USER','CEDIT_TIME','DEDIT_DATE','CADD_VER','CEDT_VER') ORDER BY SYDFIELD.CFLD_HEAD",'','TmpFLFld',"",oAriaApplication.cAria4Sysfiles,3,;
          "",SET("Datasession"))
   *N000682,1 MMT 03/06/2013 Merge SysFiles and SqlDictionary folder[END]       
   *N000682,1 MMT 12/09/2012 Globalization changes[Start]
   IF lnFldResult > 0  AND oAriaApplication.oActivelang.cLang_ID <> "EN" AND USED("TmpFLFld") 
     lcA40FIELDXML = OARIAAPPLICATION.LangPath + "Sysfiles40\SYDFIELD_DBF.XML"
    IF FILE(lcA40FIELDXML)
      lcA40FIELD =gfTempName()
      lnRecordCountReturned = XMLTOCURSOR(lcA40FIELDXML,lcA40FIELD ,512)
      IF lnRecordCountReturned > 0
        SELECT(lcA40FIELD)  
        INDEX on row_key TAG (lcA40FIELD)  
      ENDIF 
      SELECT TmpFLFld
      LOCATE
      SCAN
        IF SEEK(ALLTRIM(TmpFLFld.cfld_name) ,lcA40FIELD ,lcA40FIELD )
           REPLACE CFLD_HEAD WITH ALLTRIM(EVALUATE(lcA40FIELD +'.langtext'))
        ENDIF 
      ENDSCAN 
      TABLEUPDATE(.T.)
    ENDIF 
  ENDIF   
   *N000682,1 MMT 12/09/2012 Globalization changes[END]
  IF lnFldResult > 0
    SELECT 'TmpFLFld'
    * E302650,2 MMT 12/22/2009 Change report template screen design[Start]
    *lnFlNo = RECCOUNT()
    COUNT FOR !EMPTY(CFLD_HEAD) TO lnFlNo
    * E302650,2 MMT 12/22/2009 Change report template screen design[End]
    DIMENSION laFields[lnFlNo ,1]
    STORE '' TO laFields
    lnCntFld = 1
    SCAN FOR !EMPTY(CFLD_HEAD)
      * E302650,2 MMT 12/22/2009 Change report template screen design[Start]
      *laFields[lnCntFld ,1] = PADR(ALLTRIM(CFLD_HEAD) ,100)+"|"+CFLD_NAME+"|"+CFILE_NAM
      * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[Start]
      *laFields[lnCntFld ,1] = PADR(ALLTRIM(CFLD_HEAD) ,50)+"|"+CFLD_NAME+"|"+CFILE_NAM
      =SEEK(CFILE_NAM ,'TmpSqlFILES')
      laFields[lnCntFld ,1] =ALLTRIM(TmpSqlFILES.cfile_ttl) +'-'+PADR(ALLTRIM(CFLD_HEAD) ,50)+"|"+CFILE_NAM +"|"+CFLD_NAME
	  * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[End]
      * E302650,2 MMT 12/22/2009 Change report template screen design[End]
      lnCntFld = lnCntFld +  1
    ENDSCAN

  ENDIF
ENDIF
IF !EMPTY(lcTablesFox)
  *N000682,1 MMT 03/06/2013 Merge SysFiles and SqlDictionary folder[Start]
*!*	  lnFldResult = oAriaApplication.remotesystemdata.execute;
*!*	        ("SELECT SYDFIELD.CFLD_NAME,SYDFIELD.CFLD_HEAD,SYDFLFLD.cfile_nam  FROM SYDFIELD,SYDFLFLD ;
*!*	        WHERE SYDFIELD.cfld_name = SYDFLFLD.cfld_name AND SYDFLFLD.cfile_nam in ("+lcTablesFox+;
*!*	       ") AND  SYDFLFLD.cfld_name NOT IN ('CADD_USER','CADD_TIME','DADD_DATE','LLOK_STAT','CLOK_USER','DLOK_DATE','CLOK_TIME','CEDIT_USER','CEDIT_TIME','DEDIT_DATE','CADD_VER','CEDT_VER') ORDER BY SYDFIELD.CFLD_HEAD",'','TmpFLFld',"",oAriaApplication.SystemConnectionString,3,;
*!*	          "",SET("Datasession"))
  lnFldResult = oAriaApplication.remotesystemdata.execute;
        ("SELECT SYDFIELD.CFLD_NAME,SYDFIELD.CFLD_HEAD,SYDFLFLD.cfile_nam  FROM SYDFIELD,SYDFLFLD ;
        WHERE (SYDFIELD.CVER='A27' OR EMPTY(SYDFIELD.CVER)) AND SYDFIELD.cfld_name = SYDFLFLD.cfld_name AND SYDFLFLD.cfile_nam in ("+lcTablesFox+;
       ") AND  SYDFLFLD.cfld_name NOT IN ('CADD_USER','CADD_TIME','DADD_DATE','LLOK_STAT','CLOK_USER','DLOK_DATE','CLOK_TIME','CEDIT_USER','CEDIT_TIME','DEDIT_DATE','CADD_VER','CEDT_VER') ORDER BY SYDFIELD.CFLD_HEAD",'','TmpFLFld',"",oAriaApplication.SystemConnectionString,3,;
          "",SET("Datasession"))
  *N000682,1 MMT 03/06/2013 Merge SysFiles and SqlDictionary folder[END]          
  *N000682,1 MMT 12/09/2012 Globalization changes[Start]
   IF lnFldResult > 0  AND oAriaApplication.oActivelang.cLang_ID <> "EN" AND USED("TmpFLFld") 
     lcA27FIELDXML = OARIAAPPLICATION.LangPath + "Sysfiles27\SYDFIELD_DBF.XML"
    IF FILE(lcA27FIELDXML)
      lcA27FIELD =gfTempName()
      lnRecordCountReturned = XMLTOCURSOR(lcA27FIELDXML,lcA27FIELD,512)
      IF lnRecordCountReturned > 0
        SELECT(lcA27FIELD)  
        INDEX on row_key TAG (lcA27FIELD)  
      ENDIF 
      SELECT TmpFLFld
      LOCATE
      SCAN
        IF SEEK(ALLTRIM(TmpFLFld.cfld_name) ,lcA27FIELD,lcA27FIELD)
           REPLACE CFLD_HEAD WITH ALLTRIM(EVALUATE(lcA27FIELD+'.langtext'))
        ENDIF 
      ENDSCAN 
      TABLEUPDATE(.T.)
    ENDIF 
  ENDIF   
   *N000682,1 MMT 12/09/2012 Globalization changes[END]	
  IF lnFldResult > 0
    SELECT 'TmpFLFld'
    * E302650,2 MMT 12/22/2009 Change report template screen design[Start]
    *lnFlNo = RECCOUNT()
    COUNT FOR !EMPTY(CFLD_HEAD) TO lnFlNo
    * E302650,2 MMT 12/22/2009 Change report template screen design[End]
    lnCnt = 0
    IF EMPTY(laFields[1 ,1])
       DIMENSION laFields[lnFlNo ,1]
    ELSE
      lnCnt = ALEN(laFields,1)
      DIMENSION laFields[lnCnt +lnFlNo ,1]
    ENDIF
    lnCntFld = lnCnt + 1
    SCAN FOR !EMPTY(CFLD_HEAD)
      * E302650,2 MMT 12/22/2009 Change report template screen design[Start]
      *laFields[lnCntFld ,1] = PADR(ALLTRIM(CFLD_HEAD) ,100)+"|"+CFLD_NAME+"|"+CFILE_NAM
      * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[Start]
      *laFields[lnCntFld ,1] = PADR(ALLTRIM(CFLD_HEAD) ,50)+"|"+CFLD_NAME+"|"+CFILE_NAM
      =SEEK(CFILE_NAM ,'TmpFoxFILES')
      laFields[lnCntFld ,1] = ALLTRIM(TmpFoxFILES.cfile_ttl)+'-'+PADR(ALLTRIM(CFLD_HEAD) ,50)+"|"+CFILE_NAM+"|"+ CFLD_NAME
	  * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[End]
      * E302650,2 MMT 12/22/2009 Change report template screen design[End]
      lnCntFld = lnCntFld +  1
    ENDSCAN

  ENDIF
ENDIF
* B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[Start]
*IF lcType $ 'PO' AND 'PO' $ oAriaApplication.CompanyInstalledModules
IF lcType $ 'POC' AND ('PO' $ oAriaApplication.CompanyInstalledModules OR;
   'MF' $ oAriaApplication.CompanyInstalledModules)
* B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[End]
  lnCnt = 0
  IF EMPTY(laFields[1 ,1])
    * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[Start]
    *DIMENSION laFields[27,1]
    IF lcType $ 'PO'
      DIMENSION laFields[28,1]
    ELSE
      DIMENSION laFields[19,1]
	ENDIF
    * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[End]
  ELSE
    lnCnt = ALEN(laFields,1)
    * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[Start]
    *DIMENSION laFields[lnCnt +27,1]
    IF lcType $ 'PO'
	  DIMENSION laFields[lnCnt +28,1]
    ELSE
	  DIMENSION laFields[lnCnt +19,1]
    ENDIF
    * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[End]
  ENDIF
  * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[Start]
  =SEEK('POSLN   ','TmpSqlFILES')
  * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[End]
  lnCnter = lnCnt
  FOR lnT = 1 TO 8
    * E302650,2 MMT 12/22/2009 Change report template screen design[Start]
    *laFields[lnCnter + lnT ,1] = PADR('Open PO Line Qty #'+STR(lnT,1),100)+"|"+PADR('CLCOPN'+STR(lnT,1),10)+"|"+'POSLN   '
    * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[Start]
    *laFields[lnCnter + lnT ,1] = PADR('Open PO Line Qty #'+STR(lnT,1),50)+"|"+PADR('CLCOPN'+STR(lnT,1),10)+"|"+'POSLN   '
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*laFields[lnCnter + lnT ,1] = ALLTRIM(TmpSqlFILES.cfile_ttl) +'-'+PADR( LANG_SMRPTTMP_OPNQTYL+STR(lnT,1),50)+"|"+'POSLN   '+"|"+PADR('CLCOPN'+STR(lnT,1),10)
laFields[lnCnter + lnT ,1] = ALLTRIM(TmpSqlFILES.cfile_ttl) +'-'+PADR( IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMRPTTMP_OPNQTYL,loFormSet.GetHeaderText("LANG_SMRPTTMP_OPNQTYL",loFormSet.HeaderAlias))+STR(lnT,1),50)+"|"+'POSLN   '+"|"+PADR('CLCOPN'+STR(lnT,1),10)
*N000682,1 11/20/2012 MMT Globlization changes[End]

    * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[End]
    * E302650,2 MMT 12/22/2009 Change report template screen design[End]
  ENDFOR
  * E302650,2 MMT 12/22/2009 Change report template screen design[Start]
  *laFields[lnCnter + 9 ,1] = PADR('Open PO Line TotQty',100)+"|"+'CLCTOTOPN '+"|"+'POSLN   '
  * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[Start]
  *laFields[lnCnter + 9 ,1] = PADR('Open PO Line TotQty',50)+"|"+'CLCTOTOPN '+"|"+'POSLN   '
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*laFields[lnCnter + 9 ,1] = ALLTRIM(TmpSqlFILES.cfile_ttl) +'-'+PADR(LANG_SMRPTTMP_OPNTOTQTY,50)+"|"+'POSLN   '+"|"+'CLCTOTOPN '
laFields[lnCnter + 9 ,1] = ALLTRIM(TmpSqlFILES.cfile_ttl) +'-'+PADR(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMRPTTMP_OPNTOTQTY,loFormSet.GetHeaderText("LANG_SMRPTTMP_OPNTOTQTY",loFormSet.HeaderAlias)),50)+"|"+'POSLN   '+"|"+'CLCTOTOPN '
*N000682,1 11/20/2012 MMT Globlization changes[End]

  * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[End]
  * E302650,2 MMT 12/22/2009 Change report template screen design[End]
  * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[Start]
  IF lcType $ 'PO'
  * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[End]
    lnCnter = lnCnter + 9
    FOR lnT = 1 TO 8
      * E302650,2 MMT 12/22/2009 Change report template screen design[Start]
      *laFields[lnCnter + lnT ,1] = PADR('In Transit PO Line Qty #'+STR(lnT,1),100)+"|"+PADR('CLCINT'+STR(lnT,1),10)+"|"+'POSLN   '
      * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[Start]
      *laFields[lnCnter + lnT ,1] = PADR('In Transit PO Line Qty #'+STR(lnT,1),50)+"|"+PADR('CLCINT'+STR(lnT,1),10)+"|"+'POSLN   '
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*	  laFields[lnCnter + lnT ,1] = ALLTRIM(TmpSqlFILES.cfile_ttl) +'-'+PADR(LANG_SMRPTTMP_INTQTYL+STR(lnT,1),50)+"|"+'POSLN   '+"|"+PADR('CLCINT'+STR(lnT,1),10)
	  laFields[lnCnter + lnT ,1] = ALLTRIM(TmpSqlFILES.cfile_ttl) +'-'+PADR(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMRPTTMP_INTQTYL,loFormSet.GetHeaderText("LANG_SMRPTTMP_INTQTYL",loFormSet.HeaderAlias))+STR(lnT,1),50)+"|"+'POSLN   '+"|"+PADR('CLCINT'+STR(lnT,1),10)
*N000682,1 11/20/2012 MMT Globlization changes[End]

      * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[End]
      * E302650,2 MMT 12/22/2009 Change report template screen design[End]
    ENDFOR
    * E302650,2 MMT 12/22/2009 Change report template screen design[Start]
    *laFields[lnCnter + 9 ,1] = PADR('In Tran. PO Line TotQty',100)+"|"+'CLCTOTINT '+"|"+'POSLN   '
    * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[Start]
    *laFields[lnCnter + 9 ,1] = PADR('In Tran. PO Line TotQty',50)+"|"+'CLCTOTINT '+"|"+'POSLN   '
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*laFields[lnCnter + 9 ,1] =  ALLTRIM(TmpSqlFILES.cfile_ttl) +'-'+PADR(LANG_SMRPTTMP_INTTOTQTY,50)+"|"+'POSLN   '+"|"+'CLCTOTINT '
laFields[lnCnter + 9 ,1] =  ALLTRIM(TmpSqlFILES.cfile_ttl) +'-'+PADR(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMRPTTMP_INTTOTQTY,loFormSet.GetHeaderText("LANG_SMRPTTMP_INTTOTQTY",loFormSet.HeaderAlias)),50)+"|"+'POSLN   '+"|"+'CLCTOTINT '
*N000682,1 11/20/2012 MMT Globlization changes[End]

    * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[ENd]
    * E302650,2 MMT 12/22/2009 Change report template screen design[End]
  * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[Start]
  ENDIF
  * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[ENd]
  lnCnter = lnCnter + 9
  FOR lnT = 1 TO 8
    * E302650,2 MMT 12/22/2009 Change report template screen design[Start]
    * laFields[lnCnter + lnT ,1] = PADR('Received PO Line Qty #'+STR(lnT,1),100)+"|"+PADR('CLCREC'+STR(lnT,1),10)+"|"+'POSLN   '
    * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[Start]
    *laFields[lnCnter + lnT ,1] = PADR('Received PO Line Qty #'+STR(lnT,1),50)+"|"+PADR('CLCREC'+STR(lnT,1),10)+"|"+'POSLN   '
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*	laFields[lnCnter + lnT ,1] = ALLTRIM(TmpSqlFILES.cfile_ttl) +'-'+PADR(LANG_SMRPTTMP_RCVQTYL+STR(lnT,1),50)+"|"+'POSLN   '+"|"+PADR('CLCREC'+STR(lnT,1),10)
	laFields[lnCnter + lnT ,1] = ALLTRIM(TmpSqlFILES.cfile_ttl) +'-'+PADR(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMRPTTMP_RCVQTYL,loFormSet.GetHeaderText("LANG_SMRPTTMP_RCVQTYL",loFormSet.HeaderAlias))+STR(lnT,1),50)+"|"+'POSLN   '+"|"+PADR('CLCREC'+STR(lnT,1),10)
*N000682,1 11/20/2012 MMT Globlization changes[End]

    * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[End]
    * E302650,2 MMT 12/22/2009 Change report template screen design[End]
  ENDFOR
  * E302650,2 MMT 12/22/2009 Change report template screen design[Start]
  *laFields[lnCnter + 9,1] = PADR('Total Rec. PO Line Qty',100)+"|"+PADR('CLCTOTREC',10)+"|"+'POSLN   '
  * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[Start]
  *laFields[lnCnter + 9,1] = PADR('Total Rec. PO Line Qty',50)+"|"+PADR('CLCTOTREC',10)+"|"+'POSLN   '
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*laFields[lnCnter + 9,1] = ALLTRIM(TmpSqlFILES.cfile_ttl) +'-'+PADR(LANG_SMRPTTMP_RCVTOTQTY,50)+"|"+'POSLN   '+"|"+PADR('CLCTOTREC',10)
laFields[lnCnter + 9,1] = ALLTRIM(TmpSqlFILES.cfile_ttl) +'-'+PADR(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMRPTTMP_RCVTOTQTY,loFormSet.GetHeaderText("LANG_SMRPTTMP_RCVTOTQTY",loFormSet.HeaderAlias)),50)+"|"+'POSLN   '+"|"+PADR('CLCTOTREC',10)
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*laFields[lnCnter + 10 ,1] =ALLTRIM(TmpSqlFILES.cfile_ttl) +'-'+ PADR(LANG_SMRPTTMP_POLSTREC,50)+"|"+'POSLN   '+"|"+PADR('DPORCDAT',10)
laFields[lnCnter + 10 ,1] =ALLTRIM(TmpSqlFILES.cfile_ttl) +'-'+ PADR(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMRPTTMP_POLSTREC,loFormSet.GetHeaderText("LANG_SMRPTTMP_POLSTREC",loFormSet.HeaderAlias)),50)+"|"+'POSLN   '+"|"+PADR('DPORCDAT',10)
*N000682,1 11/20/2012 MMT Globlization changes[End]

  * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[End]
  * E302650,2 MMT 12/22/2009 Change report template screen design[End]
  * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[Start]
  *lnCnter = lnCnter + 9
  lnCnter = lnCnter + 10
  * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[End]
ENDIF
* B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[Start]
*IF lcType = 'O' AND 'PO' $ oAriaApplication.CompanyInstalledModules
IF lcType = 'O' AND ('PO' $ oAriaApplication.CompanyInstalledModules OR 'MF' $ oAriaApplication.CompanyInstalledModules)
* B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[End]
  DIMENSION laFields[ALEN(laFields,1)+1 ,1]
  * E302650,2 MMT 12/22/2009 Change report template screen design[Start]
  *laFields[ALEN(laFields,1) ,1] = PADR('Purchase Order Summary',100)+"|"+PADR('CPOSUM',10)+"|"+'POSHDR  '
  * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[Start]
  *laFields[ALEN(laFields,1) ,1] = PADR('Purchase Order Summary',50)+"|"+PADR('CPOSUM',10)+"|"+'POSHDR  '
  =SEEK('POSHDR  ','TmpSqlFILES')
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*laFields[ALEN(laFields,1) ,1] =ALLTRIM(TmpSqlFILES.cfile_ttl) +'-'+ PADR(LANG_SMRPTTMP_POCTSUM,50)+"|"+'POSHDR  '+"|"+PADR('CPOSUM',10)
laFields[ALEN(laFields,1) ,1] =ALLTRIM(TmpSqlFILES.cfile_ttl) +'-'+ PADR(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMRPTTMP_POCTSUM,loFormSet.GetHeaderText("LANG_SMRPTTMP_POCTSUM",loFormSet.HeaderAlias)),50)+"|"+'POSHDR  '+"|"+PADR('CPOSUM',10)
*N000682,1 11/20/2012 MMT Globlization changes[End]

  * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[End]
  * E302650,2 MMT 12/22/2009 Change report template screen design[End]
ENDIF
* B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[Start]
=SEEK('ORDHDR  ','TmpFoxFILES')
* B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[End]
IF lcType $ 'PC' AND 'SO' $ oAriaApplication.CompanyInstalledModules
  DIMENSION laFields[ALEN(laFields,1)+1 ,1]
  * E302650,2 MMT 12/22/2009 Change report template screen design[Start]
  *laFields[ALEN(laFields,1) ,1] = PADR('Sales Order Summary',100)+"|"+PADR('CSOSUM',10)+"|"+'ORDHDR  '
  * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[Start]
  *laFields[ALEN(laFields,1) ,1] = PADR('Sales Order Summary',50)+"|"+PADR('CSOSUM',10)+"|"+'ORDHDR  '
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*laFields[ALEN(laFields,1) ,1] = ALLTRIM(TmpFoxFILES.cfile_ttl)+'-'+PADR(LANG_SMRPTTMP_SOSUMM,50)+"|"+'ORDHDR  '+"|"+PADR('CSOSUM',10)
laFields[ALEN(laFields,1) ,1] = ALLTRIM(TmpFoxFILES.cfile_ttl)+'-'+PADR(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMRPTTMP_SOSUMM,loFormSet.GetHeaderText("LANG_SMRPTTMP_SOSUMM",loFormSet.HeaderAlias)),50)+"|"+'ORDHDR  '+"|"+PADR('CSOSUM',10)
*N000682,1 11/20/2012 MMT Globlization changes[End]

  * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[End]
  * E302650,2 MMT 12/22/2009 Change report template screen design[End]
ENDIF

IF !EMPTY(lcTablesFox) AND 'ORDHDR' $ lcTablesFox
  DIMENSION laFields[ALEN(laFields,1)+1 ,1]
  * E302650,2 MMT 12/22/2009 Change report template screen design[Start]
  * laFields[ALEN(laFields,1) ,1] = PADR('Sales Order Notes',100)+"|"+PADR('MUSRCOMMSO',10)+"|"+'ORDHDR  '
  * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[Start]
  *laFields[ALEN(laFields,1) ,1] = PADR('Sales Order Notes',50)+"|"+PADR('MUSRCOMMSO',10)+"|"+'ORDHDR  '
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*laFields[ALEN(laFields,1) ,1] = ALLTRIM(TmpFoxFILES.cfile_ttl)+'-'+PADR(LANG_SMRPTTMP_SONOTE,50)+"|"+'ORDHDR  '+"|"+PADR('MUSRCOMMSO',10)
laFields[ALEN(laFields,1) ,1] = ALLTRIM(TmpFoxFILES.cfile_ttl)+'-'+PADR(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMRPTTMP_SONOTE,loFormSet.GetHeaderText("LANG_SMRPTTMP_SONOTE",loFormSet.HeaderAlias)),50)+"|"+'ORDHDR  '+"|"+PADR('MUSRCOMMSO',10)
*N000682,1 11/20/2012 MMT Globlization changes[End]

  * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[End]
  * E302650,2 MMT 12/22/2009 Change report template screen design[End]
ENDIF
* B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[Start]
=SEEK('POSHDR  ','TmpSqlFILES')
* B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[End]

IF !EMPTY(lcTablesSql) AND 'POSHDR' $ lcTablesSql
  DIMENSION laFields[ALEN(laFields,1)+1 ,1]
  * E302650,2 MMT 12/22/2009 Change report template screen design[Start]
  *laFields[ALEN(laFields,1) ,1] = PADR('Purchase Order Notes',100)+"|"+PADR('MUSRCOMMPO',10)+"|"+'POSHDR  '
  * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[Start]
  *laFields[ALEN(laFields,1) ,1] = PADR('Purchase Order Notes',50)+"|"+PADR('MUSRCOMMPO',10)+"|"+'POSHDR  '
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*laFields[ALEN(laFields,1) ,1] = ALLTRIM(TmpSqlFILES.cfile_ttl) +'-'+ PADR(LANG_SMRPTTMP_PONOTE,50)+"|"+'POSHDR  '+"|"+PADR('MUSRCOMMPO',10)
laFields[ALEN(laFields,1) ,1] = ALLTRIM(TmpSqlFILES.cfile_ttl) +'-'+ PADR(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMRPTTMP_PONOTE,loFormSet.GetHeaderText("LANG_SMRPTTMP_PONOTE",loFormSet.HeaderAlias)),50)+"|"+'POSHDR  '+"|"+PADR('MUSRCOMMPO',10)
*N000682,1 11/20/2012 MMT Globlization changes[End]

  * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[End]
  * E302650,2 MMT 12/22/2009 Change report template screen design[End]
ENDIF
DIMENSION laTargetArr[1]
STORE '' TO laTargetArr
SELECT (loFormSet.lcrpttdt)
* E302650,2 MMT 12/22/2009 Change report template screen design[Start]
lcOldOrd = ORDER()
SET ORDER TO (loFormSet.lcrpttdt)
* E302650,2 MMT 12/22/2009 Change report template screen design[End]
SCAN FOR TYPE = 'F'
  lcFldHead  = ''
  * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[Start]
  lcFileTtl = ''
  * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[End]
  IF ALLTRIM(KEY1) $ lcTablesSql
    =SEEK(SUBSTR(KEY2,1,10),'TmpSQLFIELD')
    lcFldHead  = TmpSQLFIELD.cfld_head
    * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[Start]
    =SEEK(SUBSTR(KEY1,1,8),'TmpSqlFILES')
    lcFileTtl = ALLTRIM(TmpSqlFILES.cfile_ttl)
    * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[ENd]

  ELSE
    =SEEK(SUBSTR(KEY2,1,10),'TmpFOXFIELD')
    lcFldHead  = TmpFOXFIELD.cfld_head
    * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[Start]
    =SEEK(SUBSTR(KEY1,1,8),'TmpFoxFILES')
    lcFileTtl = ALLTRIM(TmpFoxFILES.cfile_ttl)
    * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[ENd]
  ENDIF
  lcFldName  = SUBSTR(KEY2,1,10)
  IF EMPTY(laTargetArr[1])
    * E302650,2 MMT 12/22/2009 Change report template screen design[Start]
    * laTargetArr[1] = PADR(ALLTRIM(lcFldHead),100)+"|"+PADR(lcFldName,10)+"|"+SUBSTR(key1,1,8)
    * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[Start]
    *laTargetArr[1] = PADR(ALLTRIM(lcFldHead),50)+"|"+PADR(lcFldName,10)+"|"+SUBSTR(key1,1,8)
	laTargetArr[1] = lcFileTtl +'-'+PADR(ALLTRIM(lcFldHead),50)+"|"+SUBSTR(key1,1,8)+"|"+PADR(lcFldName,10)
    * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[End]
    * E302650,2 MMT 12/22/2009 Change report template screen design[End]
  ELSE
    DIMENSION laTargetArr[ALEN(laTargetArr,1)+1]
    * E302650,2 MMT 12/22/2009 Change report template screen design[Start]
    * laTargetArr[ALEN(laTargetArr,1)] =PADR(ALLTRIM(lcFldHead),100)+"|"+PADR(lcFldName,10)+"|"+SUBSTR(key1,1,8)
    * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[Start]
    *laTargetArr[ALEN(laTargetArr,1)] =PADR(ALLTRIM(lcFldHead),50)+"|"+PADR(lcFldName,10)+"|"+SUBSTR(key1,1,8)
	laTargetArr[ALEN(laTargetArr,1)] =lcFileTtl +'-'+PADR(ALLTRIM(lcFldHead),50)+"|"+SUBSTR(key1,1,8)+"|"+PADR(lcFldName,10)
    * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[End]
    * E302650,2 MMT 12/22/2009 Change report template screen design[End]
  ENDIF
  * E302650,2 MMT 12/22/2009 Change report template screen design[Start]
  *ASORT(laTargetArr)
  * E302650,2 MMT 12/22/2009 Change report template screen design[End]
ENDSCAN
* E302650,2 MMT 12/22/2009 Change report template screen design[Start]
IF !EMPTY(lcOldOrd)
  SET ORDER to (lcOldOrd)
ENDIF
* E302650,2 MMT 12/22/2009 Change report template screen design[End]
ASORT(laFields)
* B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[Start]
*llReturn = gfMover(@laFields,@laTargetArr,'Fields',.T.,.F.,.T.,.F.,loFormSet)
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*llReturn = gfMover(@laFields,@laTargetArr,LANG_SMRPTTMP_FIELDS,.T.,.F.,.T.,.F.,loFormSet)
llReturn = gfMover(@laFields,@laTargetArr,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMRPTTMP_FIELDS,loFormSet.GetHeaderText("LANG_SMRPTTMP_FIELDS",loFormSet.HeaderAlias)),.T.,.F.,.T.,.F.,loFormSet)
*N000682,1 11/20/2012 MMT Globlization changes[End]

* B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[End]
lcTableDt = loFormSet.lcrpttdt
IF llReturn
  IF !EMPTY(laTargetArr[1])
    FOR lnA = 1 TO ALEN(laTargetArr,1)
      lnSepPos1 = ATC('|',laTargetArr[lnA],1)
      lnSepPos2 = ATC('|',laTargetArr[lnA],2)
      * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[Start]
      *lcTableName = ALLTRIM(SUBSTR(laTargetArr[lnA],lnSepPos2 +1))
      *lcFldName =  ALLTRIM(SUBSTR(laTargetArr[lnA],lnSepPos1 +1,lnSepPos2 -lnSepPos1-1))
      lnDashPos = ATC('-',laTargetArr[lnA],1)
      lcFldName = ALLTRIM(SUBSTR(laTargetArr[lnA],lnSepPos2 +1))
      lcTableName =  ALLTRIM(SUBSTR(laTargetArr[lnA],lnSepPos1 +1,lnSepPos2 -lnSepPos1-1))
      * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[End]
      m.KEY2 = lcFldName
      * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[Start]
  	*m.KEY1 = lcTableName
      m.KEY1 = PADR(lcTableName ,30)
      * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[End]
      m.CTMP_TYPE = loFormSet.ariaform1.cboTmpTyp.Value
      m.CTMP_CODE = loFormSet.ariaForm1.tempkey.KeyTextbox.Value
      m.TYPE = 'F'
      * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[Start]
      *m.cDesc = SUBSTR(laTargetArr[lnA],1,lnSepPos1 -1)
  	m.cDesc = SUBSTR(laTargetArr[lnA],lnDashPos+1,lnSepPos1-lnDashPos -1)
      * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[End]
      IF ALLTRIM(lcTableName) $ lcTablesFox
        =SEEK(PADR(lcTableName,8),'TmpFoxFILES')
        lcTableName = TmpFoxFILES.cfile_ttl
      ENDIF
      IF ALLTRIM(lcTableName) $ lcTablesSql
        =SEEK(PADR(lcTableName,8),'TmpSQLFILES')
        lcTableName = TmpSQLFILES.cfile_ttl
      ENDIF
      * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[Start]
      *IF !SEEK(m.CTMP_TYPE +m.CTMP_CODE+m.TYPE+PADR(m.KEY1,8)+PADR(m.KEY2,10),loFormSet.lcrpttdt,'PMRPTMDT')
      IF !SEEK(m.CTMP_TYPE +m.CTMP_CODE+m.TYPE+PADR(m.KEY1,30)+PADR(m.KEY2,10),loFormSet.lcrpttdt,'PMRPTMDT')
      * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[End]
        loFormSet.ariaform1.lstAdded.AddItem('Field')
        loFormSet.ariaform1.lstAdded.AddListItem(lcTableName ,loFormSet.ariaform1.lstAdded.NewItemID ,2)
        * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[Start]
        *loFormSet.ariaform1.lstAdded.AddListItem(SUBSTR(laTargetArr[lnA],1,lnSepPos1 -1),loFormSet.ariaform1.lstAdded.NewItemID,3)
		loFormSet.ariaform1.lstAdded.AddListItem( SUBSTR(laTargetArr[lnA],lnDashPos+1,lnSepPos1-lnDashPos -1),loFormSet.ariaform1.lstAdded.NewItemID,3)
        * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[End]
        INSERT INTO (loFormSet.lcrpttdt) FROM MEMVAR

        loFormSet.ariaform1.lstAdded.AddListItem(&lcTableDt..CTMP_TYPE +&lcTableDt..CTMP_CODE+&lcTableDt..TYPE+&lcTableDt..KEY1+&lcTableDt..KEY2 ,loFormSet.ariaform1.lstAdded.NewItemID,4)
      ENDIF
    ENDFOR

   	SELECT (loFormSet.lcrpttdt)
    SCAN FOR TYPE ='F'
      llRecFnd = .F.
      FOR lnA = 1 TO ALEN(laTargetArr,1)
        lnSepPos1 = ATC('|',laTargetArr[lnA],1)
        lnSepPos2 = ATC('|',laTargetArr[lnA],2)
        * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[Start]
        *lcTableName = ALLTRIM(SUBSTR(laTargetArr[lnA],lnSepPos2 +1))
        *lcFldName =  ALLTRIM(SUBSTR(laTargetArr[lnA],lnSepPos1 +1,lnSepPos2 -lnSepPos1-1))
 	   lnDashPos = ATC('-',laTargetArr[lnA],1)
        lcFldName = ALLTRIM(SUBSTR(laTargetArr[lnA],lnSepPos2 +1))
        lcTableName =  ALLTRIM(SUBSTR(laTargetArr[lnA],lnSepPos1 +1,lnSepPos2 -lnSepPos1-1))
        * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[End]
        m.KEY2 = PADR(lcFldName ,10)
        * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[Start]
		*m.KEY1 = PADR(lcTableName ,8)
        m.KEY1 = PADR(lcTableName ,30)
        * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[End]
        m.CTMP_TYPE = loFormSet.ariaform1.cboTmpTyp.Value
        m.CTMP_CODE = PADR(loFormSet.ariaForm1.tempkey.KeyTextbox.Value ,3)
        m.TYPE = 'F'
        * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[Start]
        *m.cDesc = SUBSTR(laTargetArr[lnA],1,lnSepPos1 -1)
        m.cDesc = SUBSTR(laTargetArr[lnA],lnDashPos+1,lnSepPos1-lnDashPos -1)
        * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[End]
        IF &lcTableDt..CTMP_TYPE +&lcTableDt..CTMP_CODE+&lcTableDt..TYPE+&lcTableDt..KEY1+&lcTableDt..KEY2;
           <> m.CTMP_TYPE + m.CTMP_CODE + m.TYPE + m.KEY1 + m.KEY2
          LOOP
        ELSE
          llRecFnd = .T.
          EXIT
        ENDIF
      ENDFOR
      IF !llRecFnd
        FOR i=1 TO loFormSet.ariaform1.lstAdded.ListCount
          IF loFormSet.ariaform1.lstAdded.List(i,4)<> &lcTableDt..CTMP_TYPE +&lcTableDt..CTMP_CODE+&lcTableDt..TYPE+&lcTableDt..KEY1+&lcTableDt..KEY2
            LOOP
          ELSE
            loFormSet.ariaform1.lstAdded.RemoveItem (i)
          ENDIF
        ENDFOR
        DELETE
      ENDIF
    ENDSCAN
  * E302650,2 MMT 12/22/2009 Change report template screen design[Start]
  ELSE
    SELECT (loFormSet.lcrpttdt)
    lcTableDt = loFormSet.lcrpttdt
    SCAN FOR TYPE = 'F'
      FOR i=1 TO loFormSet.ariaform1.lstAdded.ListCount
        IF loFormSet.ariaform1.lstAdded.List(i,4)<> &lcTableDt..CTMP_TYPE +&lcTableDt..CTMP_CODE+&lcTableDt..TYPE+&lcTableDt..KEY1+&lcTableDt..KEY2
          LOOP
        ELSE
          loFormSet.ariaform1.lstAdded.RemoveItem (i)
        ENDIF
      ENDFOR
      DELETE
    ENDSCAN
  * E302650,2 MMT 12/22/2009 Change report template screen design[END]


  ENDIF

ENDIF

FUNCTION lfActClk
PARAMETERS loFormSet
local lnAlias
lnAlias = ALIAS()
 *-- Declare Arrays + Open used tables
DIMENSION laAvailOprs[1],laSelOprs[1]
STORE '' TO laSelOprs,laAvailOprs
SELECT PMCTGDT
=gfSeek('')
*-- Put the catogries and tasks together in laavailoprs array
SELECT cOprt_Ctg + ' - ' + cOPrt_ID + ' ' + cOPrt_Dsc;
       FROM PMCTGDT ORDER BY cCtg_SEQ,cOprt_Ctg,cOPrt_Seq,cOPrt_ID;
       INTO ARRAY laAvailOprs

*-- Save the selected tasks in PMRPRTM table (mTmp_Oprt) field
SELECT (loFormSet.lcrptthdr)
IF !EMPTY(mTmp_Oprt)
  =lfvStr2Arr(ALLTRIM(mTmp_Oprt))
  IF ALEN(laSelOprs) > 0
    FOR lnC = 1 TO ALEN(laSelOprs)
      SELECT PMCTGDT
      lnPosition =ATC('-',laSelOprs[lnC])
      lcCatID =  SUBSTR(laSelOprs[lnC],1,lnPosition -2)
      lcOPer = SUBSTR(laSelOprs[lnC],lnPosition +2)
      =GFSEEK(PADR(lcCatID ,3)+ALLTRIM(lcOPer))
      laSelOprs[lnC] = PADR(lcCatID ,  3) + ' - '  +;
                        PADR(lcOPer , 5) + ' '  +;
                         PMCTGDT.cOprt_Dsc
    ENDFOR
  ENDIF
ENDIF
* B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[Start]
*=gfMover(@laAvailOprs, @laSelOprs,'Activities',.F.,.F.,.F.,.F.,loFormSet)
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfMover(@laAvailOprs, @laSelOprs,LANG_SMRPTTMP_ACTIVITIES,.F.,.F.,.F.,.F.,loFormSet)
=gfMover(@laAvailOprs, @laSelOprs,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMRPTTMP_ACTIVITIES,loFormSet.GetHeaderText("LANG_SMRPTTMP_ACTIVITIES",loFormSet.HeaderAlias)),.F.,.F.,.F.,.F.,loFormSet)
*N000682,1 11/20/2012 MMT Globlization changes[End]

* B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[End]
lcFillMemo  = ''
FOR lnCounter = 1 TO ALEN(laSelOprs)
  lcFillMemo = lcFillMemo + SUBSTR(laSelOprs[lnCounter], 1, 3) + ;
                            SUBSTR(laSelOprs[lnCounter], 7, 5) + '|'

ENDFOR
IF !EMPTY(lcFillMemo)
  lcFillMemo = SUBSTR(lcFillMemo , 1, LEN(lcFillMemo)-1)
ENDIF
REPLACE MTMP_OPRT WITH lcFillMemo   IN (loFormSet.lcrptthdr)
lcTableDt = loFormSet.lcrpttdt
IF !EMPTY(laSelOprs[1])
  FOR lnT = 1 TO ALEN(laSelOprs,1)
    m.NTMPSEQ = 0
    lcCateg = ''
    lcActivity = ''
    lcSepPos = ATC('-',laSelOprs[lnT])
    lcCateg  = SUBSTR(laSelOprs[lnT],1,lcSepPos-1)
    lcActivity = SUBSTR(laSelOprs[lnT],lcSepPos+1)
    m.KEY2 = SUBSTR(laSelOprs[lnT],lcSepPos+2,5)
    m.KEY1 = lcCateg
    m.CTMP_TYPE = loFormSet.ariaform1.cboTmpTyp.Value
    m.CTMP_CODE = loFormSet.ariaForm1.tempkey.KeyTextbox.Value
    m.TYPE = 'A'
    m.cDesc =lcActivity
    =gfSeek(SUBSTR(KEY1,1,3),'PMCTGHD')
    * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[Start]
    *IF !SEEK(m.CTMP_TYPE +m.CTMP_CODE+m.TYPE+PADR(m.KEY1,8)+PADR(m.KEY2,10),loFormSet.lcrpttdt,'PMRPTMDT')
    IF !SEEK(m.CTMP_TYPE +m.CTMP_CODE+m.TYPE+PADR(m.KEY1,30)+PADR(m.KEY2,10),loFormSet.lcrpttdt,'PMRPTMDT')
    * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[End]
      loFormSet.ariaform1.lstAdded.AddItem('Activity' )
      loFormSet.ariaform1.lstAdded.AddListItem(ALLTRIM(KEY1)+' '+PMCTGHD.CCTG_DSC ,loFormSet.ariaform1.lstAdded.NewItemID ,2)
      loFormSet.ariaform1.lstAdded.AddListItem(lcActivity,loFormSet.ariaform1.lstAdded.NewItemID ,3)
      INSERT INTO (loFormSet.lcrpttdt) FROM MEMVAR
      loFormSet.ariaform1.lstAdded.AddListItem(&lcTableDt..CTMP_TYPE +&lcTableDt..CTMP_CODE+&lcTableDt..TYPE+&lcTableDt..KEY1+&lcTableDt..KEY2 ,loFormSet.ariaform1.lstAdded.NewItemID,4)
    ENDIF
  ENDFOR
  SELECT (loFormSet.lcrpttdt)
  SCAN FOR TYPE = 'A'
    llRecFnd = .F.
    FOR lnI = 1 TO ALEN(laSelOprs,1)
      lcCateg = ''
      lcActivity = ''
      lcSepPos = ATC('-',laSelOprs[lnI])
      lcCateg  = SUBSTR(laSelOprs[lnI],1,lcSepPos-1)
      lcActivity = SUBSTR(laSelOprs[lnI],lcSepPos+1)
      m.KEY2 = PADR(SUBSTR(laSelOprs[lnI],lcSepPos+2,5),10)
      * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[Start]
      *m.KEY1 = PADR(lcCateg,8)
      m.KEY1 = PADR(lcCateg,30)
      * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[End]
      m.CTMP_TYPE = PADR(loFormSet.ariaform1.cboTmpTyp.Value,1)
      m.CTMP_CODE = PADR(loFormSet.ariaForm1.tempkey.KeyTextbox.Value,3)
      IF &lcTableDt..CTMP_TYPE +&lcTableDt..CTMP_CODE+&lcTableDt..TYPE+&lcTableDt..KEY1+&lcTableDt..KEY2;
         <> m.CTMP_TYPE + m.CTMP_CODE + m.TYPE + m.KEY1 + m.KEY2
        LOOP
      ELSE
        llRecFnd = .T.
        EXIT
      ENDIF
    ENDFOR
    IF !llRecFnd
      FOR i=1 TO loFormSet.ariaform1.lstAdded.ListCount
        IF loFormSet.ariaform1.lstAdded.List(i,4)<> &lcTableDt..CTMP_TYPE +&lcTableDt..CTMP_CODE+&lcTableDt..TYPE+&lcTableDt..KEY1+&lcTableDt..KEY2
          LOOP
        ELSE
          loFormSet.ariaform1.lstAdded.RemoveItem (i)
        ENDIF
      ENDFOR
      DELETE
    ENDIF
  ENDSCAN
  * E302650,2 MMT 12/22/2009 Change report template screen design[Start]
  ELSE
    SELECT (loFormSet.lcrpttdt)
    lcTableDt = loFormSet.lcrpttdt
    SCAN FOR TYPE = 'A'
      FOR i=1 TO loFormSet.ariaform1.lstAdded.ListCount
        IF loFormSet.ariaform1.lstAdded.List(i,4)<> &lcTableDt..CTMP_TYPE +&lcTableDt..CTMP_CODE+&lcTableDt..TYPE+&lcTableDt..KEY1+&lcTableDt..KEY2
          LOOP
        ELSE
          loFormSet.ariaform1.lstAdded.RemoveItem (i)
        ENDIF
      ENDFOR
      DELETE
    ENDSCAN
  * E302650,2 MMT 12/22/2009 Change report template screen design[End]

ENDIF

SELECT (lnAlias)


FUNCTION lfCrtTemp
PARAMETERS loFormSet

SELECT 'PMRPRTM'
DIMENSION laHdArr[1]
=AFIELDS(laHdArr)
=gfCrtTmp(loFormSet.lcrptthdr ,@laHdArr,'CTMP_TYPE+CTMP_CODE',loFormSet.lcrptthdr)

SELECT PMRPTMDT
DIMENSION laDtlArr[1]
lnDetCnt =AFIELDS(laDtlArr)
DIMENSION laDtlArr[lnDetCnt +1,18]
laDtlArr[lnDetCnt +1,1] = 'cDesc'
laDtlArr[lnDetCnt +1,2] = 'C'
laDtlArr[lnDetCnt +1,3] = 30
laDtlArr[lnDetCnt +1,4] = 0
STORE '' TO laDtlArr[lnDetCnt +1,7],laDtlArr[lnDetCnt +1,8],laDtlArr[lnDetCnt +1,9],;
            laDtlArr[lnDetCnt +1,10],laDtlArr[lnDetCnt +1,11],laDtlArr[lnDetCnt +1,12],;
            laDtlArr[lnDetCnt +1,13],laDtlArr[lnDetCnt +1,14],laDtlArr[lnDetCnt +1,15],;
            laDtlArr[lnDetCnt +1,16]
STORE 0 TO  laDtlArr[lnDetCnt +1,17],laDtlArr[lnDetCnt +1,18]

=gfCrtTmp(loFormSet.lcrpttdt,@laDtlArr,'CTMP_TYPE +CTMP_CODE+STR(NTMPSEQ,6)',loFormSet.lcrpttdt)
SELECT(loFormSet.lcrpttdt)
INDEX on CTMP_TYPE +CTMP_CODE+TYPE+KEY1+KEY2 TAG  'PMRPTMDT'


FUNCTION lfProfClk
PARAMETERS loFormSet
DIMENSION laTargetArr[1,2],laSourceArr[1]
STORE '' TO laTargetArr,laSourceArr
lcType = loFormSet.ariaform1.cboTmpTyp.Value
lcProType = ''
DO CASE
  CASE lcType = 'O'
    lcProType = 'SO'
  CASE lcType = 'P'
    lcProType = 'PO'
  CASE lcType = 'C'
    lcProType = 'CT'
ENDCASE
SELECT Codes
lcCodesField  = 'CPRO_CODE'
IF gfSEEK("NN"+lcCodesField)
  SCAN REST WHILE CDEFCODE+CRLTFIELD+CFLD_NAME = "NN"+lcCodesField
    lnRecNum = RECNO()
    m.cCode_No = cCode_No
    lcProfileKey = m.cCode_No
    * B609153,1 MMT 02/21/2010 Allow Style Profile to appear in all Types[STart]
    LOCAL laRelated[1, 2]
    PRIVATE lcPRO_TYPE
    lcPRO_TYPE = ''
    laRelated[1, 1]  = 'CPRO_TYPE'
    laRelated[1, 2]  = 'lcPRO_TYPE'
    = gfRltFld(lcProfileKey, @laRelated, 'CPRO_CODE')
    IF BETWEEN(lnRecNum,1,RECCOUNT('Codes'))
      GO RECORD lnRecNum
    ENDIF
    IF IIF(lcPRO_TYPE = 'ST' AND lcType $ 'PC',.F.,.T.)
    * B609153,1 MMT 02/21/2010 Allow Style Profile to appear in all Types[End]
      LOCAL lcProfileGettingType, laRelated[1, 2]
      PRIVATE lcPRO_SCR
      lcPRO_SCR = ''
      laRelated[1, 1]  = 'CPRO_SCR'
      laRelated[1, 2]  = 'lcPRO_SCR'
      = gfRltFld(lcProfileKey, @laRelated, 'CPRO_CODE')
      IF BETWEEN(lnRecNum,1,RECCOUNT('Codes'))
        GO RECORD lnRecNum
      ENDIF
      IF !EMPTY(lcProType)
        IF !(lcProType = ALLTRIM(lcPRO_SCR))
          LOOP
        ENDIF
      ENDIF
   * B609153,1 MMT 02/21/2010 Allow Style Profile to appear in all Types[Start]
   ENDIF
   * B609153,1 MMT 02/21/2010 Allow Style Profile to appear in all Types[End]
    m.cDiscrep = cDiscrep
    IF ASCAN(laSourceArr,m.cDiscrep) = 0
      IF EMPTY(laSourceArr[1])
        * E302650,2 MMT 12/22/2009 Change report template screen design[Start]
        *  laSourceArr[1] = PADR(m.cDiscrep ,100)+"|"+m.cCode_No
        laSourceArr[1] = PADR(m.cDiscrep ,50)+"|"+m.cCode_No
        * E302650,2 MMT 12/22/2009 Change report template screen design[End]
      ELSE
        DIMENSION laSourceArr[ALEN(laSourceArr,1)+1]
        * E302650,2 MMT 12/22/2009 Change report template screen design[Start]
        * laSourceArr[ALEN(laSourceArr,1)] = PADR(m.cDiscrep ,100)+"|"+m.cCode_No
        laSourceArr[ALEN(laSourceArr,1)] = PADR(m.cDiscrep ,50)+"|"+m.cCode_No
        * E302650,2 MMT 12/22/2009 Change report template screen design[End]
      ENDIF
    ENDIF
  ENDSCAN

ENDIF

DIMENSION laTargetArr[1]
STORE '' TO laTargetArr
SELECT (loFormSet.lcrpttdt)
* E302650,2 MMT 12/22/2009 Change report template screen design[Start]
lcOldOrd = ORDER()
SET ORDER TO (loFormSet.lcrpttdt)
* E302650,2 MMT 12/22/2009 Change report template screen design[End]
SCAN FOR TYPE = 'P'
  lcFDesc   = gfCodDes(SUBSTR(KEY2 ,1,6), 'CPRO_CODE')
  IF EMPTY(laTargetArr[1])
    * E302650,2 MMT 12/22/2009 Change report template screen design[Start]
    *laTargetArr[1] = PADR(ALLTRIM(lcFDesc),100)+"|"+SUBSTR(KEY2 ,1,6)
    laTargetArr[1] = PADR(ALLTRIM(lcFDesc),50)+"|"+SUBSTR(KEY2 ,1,6)
    * E302650,2 MMT 12/22/2009 Change report template screen design[End]
  ELSE
    DIMENSION laTargetArr[ALEN(laTargetArr,1)+1]
    * E302650,2 MMT 12/22/2009 Change report template screen design[Start]
    *laTargetArr[ALEN(laTargetArr,1)] =PADR(ALLTRIM(lcFDesc),100)+"|"+SUBSTR(KEY2 ,1,6)
    laTargetArr[ALEN(laTargetArr,1)] =PADR(ALLTRIM(lcFDesc),50)+"|"+SUBSTR(KEY2 ,1,6)
    * E302650,2 MMT 12/22/2009 Change report template screen design[End]
  ENDIF
ENDSCAN
* E302650,2 MMT 12/22/2009 Change report template screen design[Start]
IF !EMPTY(lcOldOrd)
  SET ORDER TO (lcOldOrd)
ENDIF
* E302650,2 MMT 12/22/2009 Change report template screen design[End]
* B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[Start]
*llRetun =gfMover(@laSourceArr, @laTargetArr,"Profiles",.F.,.F.,.F.,.F.,loFormSet)
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*llRetun =gfMover(@laSourceArr, @laTargetArr,LANG_SMRPTTMP_PROFILES,.F.,.F.,.F.,.F.,loFormSet)
llRetun =gfMover(@laSourceArr, @laTargetArr,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMRPTTMP_PROFILES,loFormSet.GetHeaderText("LANG_SMRPTTMP_PROFILES",loFormSet.HeaderAlias)),.F.,.F.,.F.,.F.,loFormSet)
*N000682,1 11/20/2012 MMT Globlization changes[End]

* B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[End]
lcTableDt = loFormSet.lcrpttdt

IF llRetun
  IF !EMPTY(laTargetArr[1])
    FOR lnA = 1 TO ALEN(laTargetArr,1)
      lnSepPos = ATC('|',laTargetArr[lnA])
      lcCodeNo = ALLTRIM(SUBSTR(laTargetArr[lnA],lnSepPos+1))
      LOCAL laRelated[1, 2]
      PRIVATE lcPRO_TYPE
      lcPRO_TYPE = ''
      laRelated[1, 1]  = 'CPRO_TYPE'
      laRelated[1, 2]  = 'lcPRO_TYPE'
      = gfRltFld(lcCodeNo , @laRelated, 'CPRO_CODE')
      m.KEY2 = PADR(lcCodeNo ,10)
      * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[Start]
      *m.KEY1 = PADR(lcPRO_TYPE,8)
      m.KEY1 = PADR(lcPRO_TYPE,30)
      * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[End]
      m.CTMP_TYPE = loFormSet.ariaform1.cboTmpTyp.Value
      m.CTMP_CODE = PADR(loFormSet.ariaForm1.tempkey.KeyTextbox.Value ,3)
      m.TYPE = 'P'
      m.cDesc =SUBSTR(laTargetArr[lnA],1,lnSepPos-1)
      * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[Start]
      *IF !SEEK(m.CTMP_TYPE +m.CTMP_CODE+m.TYPE+PADR(m.KEY1,8)+PADR(m.KEY2,10),loFormSet.lcrpttdt,'PMRPTMDT')
	  IF !SEEK(m.CTMP_TYPE +m.CTMP_CODE+m.TYPE+PADR(m.KEY1,30)+PADR(m.KEY2,10),loFormSet.lcrpttdt,'PMRPTMDT')
      * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[END]
        loFormSet.ariaform1.lstAdded.AddItem('Profile')
        * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[Start]
        *loFormSet.ariaform1.lstAdded.AddListItem(IIF(m.KEY1 = 'ST','Style',IIF(m.KEY1 = 'CS','Customer','Vendor')),loFormSet.ariaform1.lstAdded.NewItemID ,2)
		loFormSet.ariaform1.lstAdded.AddListItem(IIF(ALLTRIM(m.KEY1) = 'ST','Style',IIF(ALLTRIM(m.KEY1) = 'CS','Customer','Vendor')),loFormSet.ariaform1.lstAdded.NewItemID ,2)
        * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[End]
        loFormSet.ariaform1.lstAdded.AddListItem(SUBSTR(laTargetArr[lnA],1,lnSepPos-1),loFormSet.ariaform1.lstAdded.NewItemID ,3)
        INSERT INTO (loFormSet.lcrpttdt) FROM MEMVAR
        loFormSet.ariaform1.lstAdded.AddListItem(&lcTableDt..CTMP_TYPE +&lcTableDt..CTMP_CODE+&lcTableDt..TYPE+&lcTableDt..KEY1+&lcTableDt..KEY2 ,loFormSet.ariaform1.lstAdded.NewItemID,4)
      ENDIF
    ENDFOR
    SELECT (loFormSet.lcrpttdt)
    SCAN FOR TYPE = 'P'
      llRecFnd = .F.
      FOR lnA = 1 TO ALEN(laTargetArr,1)
        lnSepPos = ATC('|',laTargetArr[lnA])
        lcCodeNo = ALLTRIM(SUBSTR(laTargetArr[lnA],lnSepPos+1))

        LOCAL laRelated[1, 2]
        PRIVATE lcPRO_TYPE
        lcPRO_TYPE = ''
        laRelated[1, 1]  = 'CPRO_TYPE'
        laRelated[1, 2]  = 'lcPRO_TYPE'
        = gfRltFld(lcCodeNo , @laRelated, 'CPRO_CODE')
        m.KEY2 = PADR(lcCodeNo ,10)
        * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[Start]
        *m.KEY1 = PADR(lcPRO_TYPE,8)
		m.KEY1 = PADR(lcPRO_TYPE,30)
        * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[End]
        m.CTMP_TYPE = loFormSet.ariaform1.cboTmpTyp.Value
        m.CTMP_CODE = PADR(loFormSet.ariaForm1.tempkey.KeyTextbox.Value ,3)
        m.TYPE = 'P'
        m.cDesc =SUBSTR(laTargetArr[lnA],1,lnSepPos-1)
        IF &lcTableDt..CTMP_TYPE +&lcTableDt..CTMP_CODE+&lcTableDt..TYPE+&lcTableDt..KEY1+&lcTableDt..KEY2;
           <> m.CTMP_TYPE + m.CTMP_CODE + m.TYPE + m.KEY1 + m.KEY2
          LOOP
        ELSE
          llRecFnd = .T.
          EXIT
        ENDIF
      ENDFOR
      IF !llRecFnd
        FOR i=1 TO loFormSet.ariaform1.lstAdded.ListCount
          IF loFormSet.ariaform1.lstAdded.List(i,4)<> &lcTableDt..CTMP_TYPE +&lcTableDt..CTMP_CODE+&lcTableDt..TYPE+&lcTableDt..KEY1+&lcTableDt..KEY2
            LOOP
          ELSE
            loFormSet.ariaform1.lstAdded.RemoveItem (i)
          ENDIF
        ENDFOR
        DELETE
      ENDIF
    ENDSCAN
  * E302650,2 MMT 12/22/2009 Change report template screen design[Start]
  ELSE
    SELECT (loFormSet.lcrpttdt)
    lcTableDt = loFormSet.lcrpttdt
    SCAN FOR TYPE = 'P'
      FOR i=1 TO loFormSet.ariaform1.lstAdded.ListCount
        IF loFormSet.ariaform1.lstAdded.List(i,4)<> &lcTableDt..CTMP_TYPE +&lcTableDt..CTMP_CODE+&lcTableDt..TYPE+&lcTableDt..KEY1+&lcTableDt..KEY2
          LOOP
        ELSE
          loFormSet.ariaform1.lstAdded.RemoveItem (i)
        ENDIF
      ENDFOR
      DELETE
    ENDSCAN
  * E302650,2 MMT 12/22/2009 Change report template screen design[End]

  ENDIF
ENDIF
* E302650,2 MMT 12/22/2009 Change report template screen design[Start]
*FUNCTION lfBeforeSave
FUNCTION lfBeforeSaveT
* E302650,2 MMT 12/22/2009 Change report template screen design[End]
PARAMETERS loFormSet
SELECT (loFormSet.lcrpttdt)
LOCATE
IF EOF()
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*gfModalgen("TRM00000B00000","DIALOG",.F.,.F.,LANG_MFPROJMON_CANNOTSAVE)
gfModalgen("TRM00000B00000","DIALOG",.F.,.F.,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFPROJMON_CANNOTSAVE,loFormSet.GetHeaderText("LANG_MFPROJMON_CANNOTSAVE",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  RETURN .F.
ENDIF
*  E302694,1 MMT 05/16/2010 Add Shortcut to Project Monitor to complete tasks[Start]
*IF loFormSet.ariaform1.lstAdded.ListCount > 245
IF loFormSet.ariaform1.lstAdded.ListCount > 244
*  E302694,1 MMT 05/16/2010 Add Shortcut to Project Monitor to complete tasks[End]
  gfModalgen("TRM00000B00000","DIALOG",.F.,.F.,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFPROJMON_USRENTRY,loFormSet.GetHeaderText("LANG_MFPROJMON_USRENTRY",loFormSet.HeaderAlias))+;
			 ALLTRIM(STR(loFormSet.ariaform1.lstAdded.ListCount,5))+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFPROJMON_USRCOL,loFormSet.GetHeaderText("LANG_MFPROJMON_USRCOL",loFormSet.HeaderAlias))+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFPROJMON_MAXLIMIT,loFormSet.GetHeaderText("LANG_MFPROJMON_MAXLIMIT",loFormSet.HeaderAlias)))
  RETURN .F.
ENDIF



FUNCTION lfSaveTemp
PARAMETERS loFormSet
FOR i=1 TO loFormSet.ariaform1.lstAdded.ListCount
  lnSeq = i
  IF SEEK(loFormSet.ariaform1.lstAdded.List(i,4),loFormSet.lcrpttdt,'PMRPTMDT')
     REPLACE NTMPSEQ WITH lnSeq IN (loFormSet.lcrpttdt)
  ENDIF
ENDFOR
lnTablePMRPRTM = gfGetRemoteTable(SET("Datasession"),'PMRPRTM')
lcPMRPRTMTable = ''
IF lnTablePMRPRTM <>0 && Remote Table Object was Found
  lcPMRPRTMTable = oAriaApplication.laRemoteTable[lnTablePMRPRTM].lcCursorUpdate
ENDIF

IF loFormSet.ActiveMode = 'A'
  SELECT (loFormSet.lcrptthdr)
  SCATTER FIELDS MTMP_OPRT MEMO MEMVAR
  SELECT 'PMRPRTM'
  APPEND BLANK
  REPLACE CTMP_TYPE WITH loFormSet.ariaform1.cboTmpTyp.Value,;
          CTMP_CODE WITH loFormSet.ariaForm1.tempkey.KeyTextbox.Value ,;
          CTMP_DSC  WITH loFormSet.ariaForm1.txtName.Value
  =gfAdd_Info('PMRPRTM')
  =gfReplace('')
  Replace MTMP_OPRT WITH M.MTMP_OPRT IN (lcPMRPRTMTable)
  SELECT 'PMRPRTM'
  =gfTableUpdate()
  SELECT (loFormSet.lcrpttdt)
  LOCATE
  SCAN
    =gfAdd_Info(loFormSet.lcrpttdt)
    SCATTER MEMO MEMVAR
    * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[Start]
    m.CDESCRIP = m.cDesc
    * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[End]
    SELECT 'PMRPTMDT'
    APPEND BLANK
    GATHER MEMO MEMVAR
    =gfreplaCe('')
  ENDSCAN
  SELECT 'PMRPTMDT'
  =gfTableUpdate()
ELSE
  SELECT (loFormSet.lcrptthdr)
  SCATTER FIELDS MTMP_OPRT MEMO MEMVAR
  SELECT 'PMRPRTM'
  Replace CTMP_DSC  WITH loFormSet.ariaForm1.txtName.Value
  =gfAdd_Info('PMRPRTM')
  =gfReplace('')
  Replace MTMP_OPRT WITH M.MTMP_OPRT IN (lcPMRPRTMTable)
  SELECT 'PMRPRTM'
  =gfTableUpdate()
  SELECT 'PMRPTMDT'
  =gfSetOrder('PMRPTMDT')
  SELECT (loFormSet.lcrpttdt)
  LOCATE
  lcSetDel = SET("Deleted")
  SET DELETED OFF
  SCAN
    SCATTER MEMO MEMVAR
    IF DELETED()
      IF gfSeek(m.CTMP_TYPE +m.CTMP_CODE+m.TYPE+m.KEY1+m.KEY2,'PMRPTMDT')
        SELECT 'PMRPTMDT'
        =gfDelete()
      ENDIF
    ELSE
      =gfAdd_Info(loFormSet.lcrpttdt)
      SCATTER MEMO MEMVAR
      * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[Start]
       m.CDESCRIP = m.cDesc
      * B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[ENd]
      IF gfSeek(m.CTMP_TYPE +m.CTMP_CODE+m.TYPE+m.KEY1+m.KEY2,'PMRPTMDT')
        SELECT 'PMRPTMDT'
        GATHER MEMO MEMVAR
        =gfReplace('')
      ELSE
        SELECT 'PMRPTMDT'
        APPEND BLANK
        GATHER MEMO MEMVAR
        =gfReplace('')
      ENDIF
    ENDIF
  ENDSCAN
  SELECT 'PMRPTMDT'
  =gfTableUpdate()
  SET DELETED &lcSetDel.
ENDIF

FUNCTION lfDeleteTemp
PARAMETERS loFormSet

*B610333,1 fix media R13 issues TMI 19/05/2013 [Start] get the key to delete line using it
LOCAL lcID2Del
lcID2Del = PMRPRTM.CTMP_TYPE+PMRPRTM.CTMP_CODE
*B610333,1 fix media R13 issues TMI 19/05/2013 [End  ] 
SELECT 'PMRPTMDT'
=gfSeek(PMRPRTM.CTMP_TYPE+PMRPRTM.CTMP_CODE)
SCAN REST WHILE CTMP_TYPE +CTMP_CODE+STR(NTMPSEQ,6)  = PMRPRTM.CTMP_TYPE+PMRPRTM.CTMP_CODE
  =gfDelete()
ENDSCAN
=gfTableUpdate()
SELECT 'PMRPRTM'
*B610333,1 fix media R13 issues TMI 19/05/2013 [Start] get the parent record to delete it
gfSeek(lcID2Del)
*B610333,1 fix media R13 issues TMI 19/05/2013 [End  ] 
=gfDelete()
=gfTableUpdate()
*E302650,1 MMT 12/14/2009 Change report template screen design[End]

* B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[Start]
FUNCTION lfChngDesc
lPARAMETERS loFormSet
IF loFormSet.ActiveMode $ 'SV'
  RETURN
ENDIF

IF SEEK(loFormSet.ariaform1.lstAdded.ListItem (loFormSet.ariaform1.lstAdded.ListItemId,4),;
		loFormSet.lcrpttdt,'PMRPTMDT') AND EVALUATE(loFormSet.lcrpttdt+'.type') ='F'
  DO FORM (oAriaApplication.ScreenHome+'SM\SMRPTDSC.scx')		WITH loFormSet, EVALUATE(loFormSet.lcrpttdt+'.cDesc')

		loFormSet.ariaform1.lstAdded.AddListItem(EVALUATE(loFormSet.lcrpttdt+'.cDesc'),loFormSet.ariaform1.lstAdded.ListItemId ,3)
ENDIF

FUNCTION lfUpfldDesc
LPARAMETERS loFormSet ,lcFldNewDesc
REPLACE cDesc WITH lcFldNewDesc IN (loFormSet.lcrpttdt)
* B609473,1 MMT 12/22/2010 Modify Template Screen to display the logical files name[End]
