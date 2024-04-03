***
*** main.fxp
***
*
*** 
*** ReFox XI+  #AU157137  Mariam  Mariam [VFP90]
***
 PUBLIC gccurpath, gnconnhandler, ariasyspath, aria4syspath, ariaapppath
 lccurrentprocedure = UPPER(SYS(16, 1))
 lnpathstart = AT(":", lccurrentprocedure)-1
 lnlenofpath = RAT("\", lccurrentprocedure)-(lnpathstart)
 gccurpath = SUBSTR(lccurrentprocedure, lnpathstart, lnlenofpath)
 gnconnhandler = 0
 ariasyspath = ""
 ariaapppath = ""
 SET SAFETY OFF
 SET DEFAULT TO (gccurpath)
 SET DELETED ON
 SET CENTURY ON
 SET DATE DMY
 SET HOURS TO 12
 SET MULTILOCKS ON
 SET REPROCESS TO 0.1  SECONDS
 SET STATUS OFF
 SET NULLDISPLAY TO ''
 frmpath = gccurpath+'\forms'
 icopath = gccurpath+'\icons'
 tmppath = gccurpath+'\temp'
 CLOSE DATABASES
 IF  .NOT. gfopenconn()
    RETURN
 ENDIF
 USE SHARED (ariasyspath+"SYCINST") IN 0
 aria4syspath = ALLTRIM(sycinst.ca4sysdir)
 USE IN sycinst
 DO FORM (frmpath+"\admintool.scx")
 READ EVENTS
 RETURN
ENDPROC
**
FUNCTION gfOpenConn
 WAIT WINDOW NOWAIT "Connecting..."
 XMLTOCURSOR('Config.xml', 'Setting', 512)
 SELECT setting
 lcconnstr = "DRIVER=SQL Server;SERVER="+ALLTRIM(server)+";UID="+ALLTRIM(uid)+";PWD="+ALLTRIM(pwd)+";DATABASE="+ALLTRIM(database)+";"
 ariasyspath = ADDBS(setting.syspath)
 ariaapppath = ADDBS(setting.ariapath)
 USE
 gnconnhandler = SQLSTRINGCONNECT(lcconnstr)
 WAIT CLEAR
 IF gnconnhandler<1
    MESSAGEBOX("Unable to Connect to Database!", 16, "Translation Tool")
    RETURN .F.
 ELSE
    RETURN .T.
 ENDIF
ENDFUNC
**
*** 
*** ReFox - all is not lost 
***
***
*** admintool.scx
***
*
*
*  admintool.scx::mainhome
**
PROCEDURE Destroy
 CLEAR EVENTS
ENDPROC
**
PROCEDURE Init
**
** ReFox - this procedure is empty **
**
ENDPROC
**
*
*  admintool.scx::Command1
**
PROCEDURE Click
 IF gnconnhandler>0
    = SQLDISCONNECT(gnconnhandler)
 ENDIF
 thisform.release
ENDPROC
**
*
*  admintool.scx::Command2
**
PROCEDURE Click
 DO FORM (frmpath+"\language.scx")
ENDPROC
**
*
*  admintool.scx::Command4
**
PROCEDURE Click
 DO FORM (frmpath+"\langGen.scx")
ENDPROC
**
*
*  admintool.scx::Command5
**
PROCEDURE Click
 DO FORM (frmpath+"\langdest.scx")
ENDPROC
**
***
*** admintool.sct
***
*
***
*** aria.ico
***
*
***
*** close.bmp
***
*
***
*** language.scx
***
*
*
*  language.scx::Form1
**
PROCEDURE convertrec
 IF EOF('syclang')
    thisform.txtffont.value = ""
    thisform.txtrfont.value = ""
    thisform.txtdtformat.value = ""
    thisform.optiongroup1.option1.value = 0
    thisform.optiongroup1.option2.value = 0
    thisform.cmdfontf.enabled = .F.
    thisform.cmdfontr.enabled = .F.
    thisform.cmbformat.enabled = .F.
    thisform.optiongroup1.option1.enabled = .F.
    thisform.optiongroup1.option2.enabled = .F.
    thisform.cmdremove.enabled = .F.
    thisform.cmdsave.enabled = .F.
 ELSE
    thisform.cmdfontf.enabled = .T.
    thisform.cmdfontr.enabled = .T.
    thisform.cmbformat.enabled = .T.
    thisform.optiongroup1.option1.enabled = .T.
    thisform.optiongroup1.option2.enabled = .T.
    thisform.cmdremove.enabled = .T.
    thisform.cmdsave.enabled = .T.
    thisform.txtffont.value = ALLTRIM(syclang.cformfont)+", "+ALLTRIM(STR(syclang.nformfonts, 3))+","+syclang.cformfstyle
    thisform.txtrfont.value = ALLTRIM(syclang.crepfont)+", "+ALLTRIM(STR(syclang.nrepfonts, 3))+","+syclang.crepfstyle
    thisform.txtdtformat.value = ALLTRIM(syclang.cdate_format)+" "+ALLTRIM(syclang.ctime_format)
    IF OCCURS("PM", ctime_format)>0
       thisform.optiongroup1.option1.value = 1
       thisform.optiongroup1.option2.value = 0
    ELSE
       thisform.optiongroup1.option1.value = 0
       thisform.optiongroup1.option2.value = 1
    ENDIF
    IF SUBSTR(syclang.cdate_format, 1, 1)='y'
       thisform.cmbformat.value = "YMD"
    ELSE
       IF SUBSTR(syclang.cdate_format, 1, 1)='m'
          thisform.cmbformat.value = "MDY"
       ELSE
          thisform.cmbformat.value = "DMY"
       ENDIF
    ENDIF
 ENDIF
ENDPROC
**
PROCEDURE setdateformat
 lcdateformat = ""
 IF thisform.cmbformat.value="YMD"
    lcdateformat = "yyyy"+ALLTRIM(thisform.txtsep.value)+"mm"+ALLTRIM(thisform.txtsep.value)+"dd"
 ELSE
    IF thisform.cmbformat.value="MDY"
       lcdateformat = "mm"+ALLTRIM(thisform.txtsep.value)+"dd"+ALLTRIM(thisform.txtsep.value)+"yyyy"
    ELSE
       lcdateformat = "dd"+ALLTRIM(thisform.txtsep.value)+"mm"+ALLTRIM(thisform.txtsep.value)+"yyyy"
    ENDIF
 ENDIF
 REPLACE syclang.cdate_format WITH lcdateformat
 lcusepm = ""
 IF thisform.optiongroup1.option1.value=1
    lcusepm = "PM"
 ENDIF
 REPLACE syclang.ctime_format WITH "hh:mm "+lcusepm
 thisform.txtdtformat.value = lcdateformat+" "+"hh:mm "+lcusepm
ENDPROC
**
PROCEDURE addnewrec
 PARAMETER lclid, lcldesc
 SELECT syclang
 APPEND BLANK
 REPLACE clang_id WITH lclid
 REPLACE cshort_des WITH lcldesc
 REPLACE lis_rtl WITH .F.
 REPLACE cdate_format WITH "mm/dd/yyyy"
 REPLACE ctime_format WITH "hh:mm"
 REPLACE cformfont WITH "Tahoma"
 REPLACE nformfonts WITH 9
 REPLACE crepfont WITH "Tahoma"
 REPLACE nrepfonts WITH 9
 REPLACE cformfstyle WITH "N"
 REPLACE crepfstyle WITH "N"
 REPLACE cdateseparator WITH "/"
 REPLACE ccodepage WITH "1250"
ENDPROC
**
PROCEDURE Init
 thisform.cmdupdate.enabled = .T.
ENDPROC
**
PROCEDURE Load
 SQLEXEC(gnconnhandler, "select * from syclang", "SYCLANG")
ENDPROC
**
*
*  language.scx::txtDesc
**
PROCEDURE Valid
 thisform.refresh()
ENDPROC
**
PROCEDURE InteractiveChange
 thisform.cmdupdate.enabled = .F.
ENDPROC
**
*
*  language.scx::txtSep
**
PROCEDURE InteractiveChange
 thisform.setdateformat()
ENDPROC
**
PROCEDURE Valid
 thisform.cmdupdate.enabled = .F.
 thisform.setdateformat()
ENDPROC
**
*
*  language.scx::Check1
**
PROCEDURE InteractiveChange
 thisform.cmdupdate.enabled = .F.
ENDPROC
**
*
*  language.scx::Grid1
**
PROCEDURE Refresh
 thisform.convertrec()
ENDPROC
**
PROCEDURE AfterRowColChange
 LPARAMETERS ncolindex
 thisform.refresh()
ENDPROC
**
*
*  language.scx::cmdAdd
**
PROCEDURE Click
 lclid = "  "
 lcldesc = "  "
 DO FORM (frmpath+"\addlang.scx")
 IF  .NOT. EMPTY(lclid)
    thisform.cmdupdate.enabled = .F.
    thisform.addnewrec(lclid, lcldesc)
    thisform.refresh
 ENDIF
 RETURN
ENDPROC
**
*
*  language.scx::cmdRemove
**
PROCEDURE Click
 lnresult = SQLEXEC(gnconnhandler, "SELECT TOP 1 EnStr_Key FROM SydString WHERE cLang_ID='"+syclang.clang_id+"'", "TmpExist")
 SELECT tmpexist
 GOTO TOP
 llused =  .NOT. EOF()
 USE
 SELECT syclang
 IF llused
    MESSAGEBOX("This language is used, unable to delete!", 16)
 ELSE
    IF MESSAGEBOX("Are you sure you want to delete Language :"+syclang.clang_id, 36)=6
       lnresult = SQLEXEC(gnconnhandler, "DELETE FROM syclang WHERE CLANG_ID = '"+syclang.clang_id+"'")
       SELECT syclang
       DELETE
    ENDIF
    SELECT syclang
    GOTO TOP
    thisform.refresh
 ENDIF
ENDPROC
**
*
*  language.scx::cmdSave
**
PROCEDURE Click
 SELECT syclang
 LOCATE FOR EMPTY(cshort_des)
 IF FOUND()
    MESSAGEBOX("The language description cannot be empty!", 64)
    RETURN
 ENDIF
 GOTO TOP
 SCAN
    lbexist = .F.
    = SQLEXEC(gnconnhandler, "SELECT 1 AS CHK FROM syclang WHERE CLANG_ID = '"+syclang.clang_id+"'", "TmpCheck")
    SELECT tmpcheck
    GOTO TOP
    IF tmpcheck.chk=1
       lbexist = .T.
    ENDIF
    USE
    SELECT syclang
    IF  .NOT. lbexist
       lcstr = "INSERT INTO syclang "
       lcstr = lcstr+"([cLang_ID],[cShort_Des],[lIs_RTL],[cDate_Format],[cTime_Format],[cFormFont]"
       lcstr = lcstr+",[nFormFontS],[cRepFont],[nRepFontS],[cFormFStyle],[cRepFStyle],[cDateSeparator],[cCodePage]) VALUES "
       lcstr = lcstr+"('"+syclang.clang_id+"',"
       lcstr = lcstr+"'"+ALLTRIM(syclang.cshort_des)+"',"
       lcstr = lcstr+IIF(syclang.lis_rtl, "1,", "0,")
       lcstr = lcstr+"'"+syclang.cdate_format+"',"
       lcstr = lcstr+"'"+syclang.ctime_format+"',"
       lcstr = lcstr+"'"+syclang.cformfont+"',"
       lcstr = lcstr+ALLTRIM(STR(syclang.nformfonts, 3))+","
       lcstr = lcstr+"'"+syclang.crepfont+"',"
       lcstr = lcstr+ALLTRIM(STR(syclang.nrepfonts, 3))+","
       lcstr = lcstr+"'"+syclang.cformfstyle+"',"
       lcstr = lcstr+"'"+syclang.crepfstyle+"',"
       lcstr = lcstr+"'"+syclang.cdateseparator+"',"
       lcstr = lcstr+"'"+syclang.ccodepage+"')"
    ELSE
       lcstr = "Update syclang SET "
       lcstr = lcstr+" [cShort_Des] = '"+ALLTRIM(syclang.cshort_des)+"'"
       lcstr = lcstr+" ,[lIs_RTL] = "+IIF(syclang.lis_rtl, "1", "0")
       lcstr = lcstr+" ,[cDate_Format] = '"+syclang.cdate_format+"'"
       lcstr = lcstr+" ,[cTime_Format] = '"+syclang.ctime_format+"'"
       lcstr = lcstr+" ,[cFormFont] = '"+syclang.cformfont+"'"
       lcstr = lcstr+" ,[nFormFontS] = "+ALLTRIM(STR(syclang.nformfonts, 3))
       lcstr = lcstr+" ,[cRepFont] = '"+syclang.crepfont+"'"
       lcstr = lcstr+" ,[nRepFontS] = "+ALLTRIM(STR(syclang.nrepfonts, 3))
       lcstr = lcstr+" ,[cFormFStyle] = '"+syclang.cformfstyle+"'"
       lcstr = lcstr+" ,[cRepFStyle] = '"+syclang.crepfstyle+"'"
       lcstr = lcstr+" ,[cDateSeparator] = '"+syclang.cdateseparator+"'"
       lcstr = lcstr+" ,[cCodePage] = '"+syclang.ccodepage+"'"
       lcstr = lcstr+" WHERE CLANG_ID = '"+syclang.clang_id+"'"
    ENDIF
    = SQLEXEC(gnconnhandler, lcstr)
 ENDSCAN
 SELECT syclang
 GOTO TOP
 thisform.cmdupdate.enabled = .T.
 thisform.refresh
 MESSAGEBOX("Language sucsessfuly updated!", 64)
 RETURN
ENDPROC
**
*
*  language.scx::cmdClose
**
PROCEDURE Click
 CLOSE DATABASES
 thisform.release
ENDPROC
**
*
*  language.scx::cmdfontf
**
PROCEDURE Click
 oldvalue = thisform.txtffont.value
 lcfontname = ALLTRIM(syclang.cformfont)
 lnfontsize = syclang.nformfonts
 lcfontstyle = ALLTRIM(syclang.cformfstyle)
 thisform.txtffont.value = GETFONT(lcfontname, lnfontsize, lcfontstyle)
 IF  .NOT. EMPTY(thisform.txtffont.value)
    thisform.cmdupdate.enabled = .F.
    ln1stcoma = AT(",", thisform.txtffont.value, 1)
    ln2ndcoma = AT(",", thisform.txtffont.value, 2)
    lcfontname = ALLTRIM(SUBSTR(thisform.txtffont.value, 1, ln1stcoma-1))
    lnfontsize = INT(VAL(SUBSTR(thisform.txtffont.value, ln1stcoma+1, ln2ndcoma-ln1stcoma-1)))
    lcfontstyle = ALLTRIM(SUBSTR(thisform.txtffont.value, ln2ndcoma+1))
    REPLACE syclang.cformfont WITH lcfontname
    REPLACE syclang.nformfonts WITH lnfontsize
    REPLACE syclang.cformfstyle WITH lcfontstyle
 ELSE
    thisform.txtffont.value = oldvalue
 ENDIF
ENDPROC
**
*
*  language.scx::cmdfontr
**
PROCEDURE Click
 oldvalue = thisform.txtffont.value
 lcfontname = ALLTRIM(syclang.crepfont)
 lnfontsize = syclang.nrepfonts
 lcfontstyle = ALLTRIM(syclang.crepfstyle)
 thisform.txtrfont.value = GETFONT(lcfontname, lnfontsize, lcfontstyle)
 IF  .NOT. EMPTY(thisform.txtrfont.value)
    thisform.cmdupdate.enabled = .F.
    ln1stcoma = AT(",", thisform.txtrfont.value, 1)
    ln2ndcoma = AT(",", thisform.txtrfont.value, 2)
    lcfontname = ALLTRIM(SUBSTR(thisform.txtrfont.value, 1, ln1stcoma-1))
    lnfontsize = INT(VAL(SUBSTR(thisform.txtrfont.value, ln1stcoma+1, ln2ndcoma-ln1stcoma-1)))
    lcfontstyle = ALLTRIM(SUBSTR(thisform.txtrfont.value, ln2ndcoma+1))
    REPLACE syclang.crepfont WITH lcfontname
    REPLACE syclang.nrepfonts WITH lnfontsize
    REPLACE syclang.crepfstyle WITH lcfontstyle
 ELSE
    thisform.txtrfont.value = oldvalue
 ENDIF
ENDPROC
**
*
*  language.scx::txtdtformat
**
PROCEDURE Init
 this.value = 'HH:MM:SS'
ENDPROC
**
*
*  language.scx::cmbFormat
**
PROCEDURE InteractiveChange
 thisform.cmdupdate.enabled = .F.
 thisform.setdateformat()
ENDPROC
**
*
*  language.scx::Optiongroup1
**
PROCEDURE InteractiveChange
 thisform.setdateformat()
ENDPROC
**
PROCEDURE Valid
 thisform.cmdupdate.enabled = .F.
 thisform.setdateformat()
ENDPROC
**
*
*  language.scx::cmdUpdate
**
PROCEDURE Click
 LOCAL lnselect, lclangpath
 lnselect = SELECT(0)
 lclangpath = ariaapppath+"Lang\"
 = CURSORTOXML("Syclang", ariaapppath+"Lang\Syclang.XML", 2, 512, 0, "")
 thisform.refresh
 MESSAGEBOX("Language sucsessfuly updated!", 64)
 SELECT (lnselect)
 RETURN
ENDPROC
**
***
*** language.sct
***
*
***
*** edit1.bmp
***
*
***
*** tbnew.bmp
***
*
***
*** delete.bmp
***
*
***
*** save.bmp
***
*
***
*** undo.bmp
***
*
***
*** gen.bmp
***
*
***
*** folders.bmp
***
*
***
*** document.bmp
***
*
***
*** config.fpw
***
*
***
*** addlang.scx
***
*
*
*  addlang.scx::txtLangID
**
PROCEDURE InteractiveChange
 thisform.cmdsave.enabled =  .NOT. EMPTY(this.value)
ENDPROC
**
*
*  addlang.scx::cmdSave
**
PROCEDURE Click
 SELECT syclang
 LOCATE FOR clang_id=ALLTRIM(thisform.txtlangid.value)
 IF  .NOT. FOUND()
    lclid = thisform.txtlangid.value
    lcldesc = ALLTRIM(thisform.txtdesc.value)
    thisform.release
 ELSE
    MESSAGEBOX("This language already exist!")
 ENDIF
ENDPROC
**
*
*  addlang.scx::cmdClose
**
PROCEDURE Click
 thisform.release
ENDPROC
**
***
*** addlang.sct
***
*
***
*** langgen.scx
***
*
*
*  langgen.scx::Form1
**
FUNCTION getfileextention
 LPARAMETERS lcobjtype
 lcretval = "*"
 DO CASE
    CASE lcobjtype="F"
       lcretval = "SCX"
    CASE lcobjtype="R"
       lcretval = "FRX"
    CASE lcobjtype="C"
       lcretval = "VCX"
    CASE lcobjtype="S"
       lcretval = "DBF"
    CASE lcobjtype="H"
       lcretval = "H"
    CASE lcobjtype="L"
       lcretval = "LBX"
 ENDCASE
 RETURN lcretval
ENDFUNC
**
PROCEDURE screenmodes
 IF thisform.usemode='S'
    thisform.combo1.enabled = .T.
    IF thisform.combo1.value='S'
       thisform.combo3.enabled = .T.
       thisform.cmdsel1.enabled = .F.
       thisform.cmdsel2.enabled = .F.
       thisform.cbosysfile.enabled = .T.
       thisform.cbosysfile.visible = .T.
    ELSE
       thisform.combo3.enabled = .F.
       thisform.cmdsel1.enabled = .T.
       thisform.cmdsel2.enabled = .T.
       thisform.cbosysfile.visible = .F.
    ENDIF
    thisform.cmdload.enabled = .T.
    thisform.cmdcancel.caption = "Cancel"
    thisform.cmdcancel.enabled = .F.
    thisform.cmdsave.enabled = .F.
    thisform.cmdgen.enabled = .F.
    thisform.cmdselall.enabled = .F.
    thisform.cmdselnone.enabled = .F.
 ELSE
    IF thisform.usemode="G"
       thisform.combo1.enabled = .F.
       thisform.combo3.enabled = .F.
       thisform.cmdsel1.enabled = .F.
       thisform.cmdsel2.enabled = .F.
       IF thisform.combo1.value='S'
          thisform.cbosysfile.enabled = .F.
       ENDIF
       thisform.cmdload.enabled = .F.
       thisform.cmdcancel.caption = "Cancel Generated"
       thisform.cmdcancel.enabled = .T.
       thisform.cmdsave.enabled = .T.
       thisform.cmdgen.enabled = .F.
       thisform.cmdselall.enabled = .F.
       thisform.cmdselnone.enabled = .F.
       thisform.grid1.column8.readonly = .T.
    ELSE
       IF thisform.usemode="E"
          thisform.combo1.enabled = .F.
          thisform.combo3.enabled = .F.
          thisform.cmdsel1.enabled = .F.
          thisform.cmdsel2.enabled = .F.
          IF thisform.combo1.value='S'
             thisform.cbosysfile.enabled = .F.
          ENDIF
          thisform.cmdload.enabled = .F.
          thisform.cmdcancel.caption = "Cancel Loaded"
          thisform.cmdcancel.enabled = .T.
          thisform.cmdsave.enabled = .F.
          thisform.cmdgen.enabled = .T.
          thisform.cmdselall.enabled = .T.
          thisform.cmdselnone.enabled = .T.
          thisform.grid1.column8.readonly = .F.
       ENDIF
    ENDIF
 ENDIF
ENDPROC
**
PROCEDURE cleartables
 LPARAMETERS lctabletype
 IF lctabletype='O'
    SELECT sydobjrelation
    DELETE ALL
    SELECT sydlangobj
    DELETE ALL
    GOTO TOP
    thisform.grid1.refresh
 ENDIF
 IF lctabletype='L'
    SELECT hfiledb2
    DELETE ALL
    SELECT hfiledb1
    DELETE ALL
    SELECT sydenstring
    DELETE ALL
    GOTO TOP
    thisform.grid2.refresh
 ENDIF
 RETURN
ENDPROC
**
PROCEDURE loadobject
 LPARAMETERS lcobjname, lnnewkey
 lcext = ALLTRIM(thisform.getfileextention(thisform.combo1.value))
 lcver = ALLTRIM(STR(INT(VAL(thisform.combo3.displayvalue)*10), 3))
 IF thisform.combo1.value='S'
    lcobject = lcobjname+".DBF"
 ELSE
    lcobject = SUBSTR(lcobjname, ATC('\', lcobjname, OCCURS('\', lcobjname))+1)
 ENDIF
 lnkey = thisform.loadmain(lcobjname, lnnewkey, lcext, lcver, lcobject)
 IF ALLTRIM(thisform.combo1.value)="F" .OR. ALLTRIM(thisform.combo1.value)="C"
    SELECT 0
    USE SHARED (lcobjname) ALIAS "OBJDBF"
    SELECT objdbf
    LOCATE FOR  .NOT. EMPTY(reserved8)
    IF FOUND()
       thisform.loadrelatedh(lcobjname, lnnewkey, lcext, lcver, lcobject, lnkey)
    ENDIF
    IF ALLTRIM(thisform.combo1.value)="F"
       SELECT DISTINCT LEFT(classloc, 200) AS "CLIB" FROM OBJDBF WHERE  NOT EMPTY(LEFT(classloc, 200)) INTO CURSOR "TempClass"
       IF USED('TempClass')
          thisform.loadrelatedclass(lcobjname, lnnewkey, lcext, lcver, lcobject, lnkey)
       ENDIF
    ENDIF
    IF USED('OBJDBF')
       SELECT objdbf
       USE
    ENDIF
 ENDIF
 SELECT sydlangobj
 GOTO TOP
 thisform.grid1.refresh
 RETURN
ENDPROC
**
FUNCTION dogenerate
 LPARAMETERS lctype, lcfile, lnobjkey
 lret = .T.
 IF lctype='DBF'
    lret = thisform.generatedbf(lctype, lcfile, lnobjkey)
    RETURN lret
 ELSE
    IF  .NOT. FILE(lcfile)
       RETURN .F.
    ENDIF
 ENDIF
 IF (lctype='SCX' .OR. lctype='VCX')
    thisform.generateform(lctype, lcfile, lnobjkey)
 ENDIF
 IF lctype='H'
    thisform.generatehfile(lctype, lcfile, lnobjkey)
 ENDIF
 IF (lctype='FRX' .OR. lctype='LBX')
    thisform.generaterep(lctype, lcfile, lnobjkey)
 ENDIF
 RETURN lret
ENDFUNC
**
PROCEDURE dosavenew
 LPARAMETERS lnoldkey, lnnewkey
 SELECT sydenstring
 SCAN FOR obj_key=lnoldkey .AND.  .NOT. INLIST(ALLTRIM(fulltext), ":", ".")
    SCATTER MEMO MEMVAR
    m.obj_key = lnnewkey
    m.fulltext = m.originaltext
    lcstr = "INSERT INTO SydEnString(Obj_Key,Row_Key,cProperty ,OriginalText,FullText,OriginalWidth,OriginalControlName)"
    lcstr = lcstr+"VALUES (?m.Obj_Key,?m.Row_Key,?m.cProperty ,?m.OriginalText,?m.FullText,?m.OriginalWidth,?m.OriginalControlName)"
    lnres = SQLEXEC(gnconnhandler, lcstr)
 ENDSCAN
 SELECT sydobjrelation
 REPLACE obj_key WITH lnnewkey ALL FOR obj_key=lnoldkey
 REPLACE related_key WITH lnnewkey ALL FOR related_key=lnoldkey
 RETURN
ENDPROC
**
PROCEDURE dosaveupd
 LPARAMETERS lnobjkey
 lcstr = "SELECT EnStr_Key,cProperty,FullText FROM SydEnString WHERE Obj_Key="+ALLTRIM(STR(lnobjkey, 8))
 SQLEXEC(gnconnhandler, lcstr, "tb_LangStr")
 SELECT sydenstring
 LOCATE
 SCAN FOR obj_key=lnobjkey .AND.  .NOT. INLIST(ALLTRIM(fulltext), ":", ".")
    SCATTER MEMO MEMVAR
    SELECT tb_langstr
    LOCATE FOR enstr_key=sydenstring.enstr_key
    IF FOUND()
       IF ALLTRIM(tb_langstr.fulltext)<>ALLTRIM(m.fulltext)
          lcstr = "UPDATE SydEnString SET FullText=?m.FullText WHERE EnStr_Key=?m.EnStr_Key"
          lnres = SQLEXEC(gnconnhandler, lcstr)
       ENDIF
    ELSE
       lcstr = "INSERT INTO SydEnString(Obj_Key,Row_Key,cProperty ,OriginalText,FullText,OriginalWidth,OriginalControlName)"
       lcstr = lcstr+"VALUES (?m.Obj_Key,?m.Row_Key,?m.cProperty ,?m.OriginalText,?m.FullText,?m.OriginalWidth,?m.OriginalControlName)"
       lnres = SQLEXEC(gnconnhandler, lcstr)
    ENDIF
    SELECT sydenstring
 ENDSCAN
 SELECT tb_langstr
 USE
 lcstr = "DELETE FROM SydObjRelation WHERE Obj_Key="+ALLTRIM(STR(lnobjkey, 8))
 lnres = SQLEXEC(gnconnhandler, lcstr)
 SELECT sydlangobj
 RETURN
ENDPROC
**
PROCEDURE domerge
 LPARAMETERS lcobjstr, lcobjtype
 IF (VARTYPE(lcobjtype)<>"C")
    lcobjtype = ""
 ENDIF
 DELETE FROM SydEnString WHERE INLIST(ALLTRIM(fulltext), ":", ".")
 SQLEXEC(gnconnhandler, "SELECT EnStr_Key,Row_Key,cProperty,FullText,OriginalControlName FROM SydEnString WHERE Obj_Key="+lcobjstr, "tb_LangStr")
 SELECT tb_langstr
 SCAN
    SELECT sydenstring
    IF (lcobjtype=="SCX")
       LOCATE FOR ((ALLTRIM(originalcontrolname)=ALLTRIM(tb_langstr.originalcontrolname)) .AND. (ALLTRIM(cproperty)=ALLTRIM(tb_langstr.cproperty)))
    ELSE
       LOCATE FOR row_key=tb_langstr.row_key .AND. cproperty=tb_langstr.cproperty
    ENDIF
    IF FOUND()
       REPLACE enstr_key WITH tb_langstr.enstr_key, fulltext WITH ALLTRIM(tb_langstr.fulltext)
    ENDIF
    SELECT tb_langstr
 ENDSCAN
 SELECT tb_langstr
 USE
 RETURN
ENDPROC
**
PROCEDURE generateform
 LPARAMETERS lctype, lcfile, lnobjkey
 USE SHARED (lcfile) ALIAS "OBJFILE" IN 0
 IF  .NOT. USED("OBJFILE")
    RETURN
 ENDIF
 SELECT objfile
 SCAN FOR ('Caption'$properties) .OR. ('ToolTipText'$properties)
    lnmlines = MEMLINES(objfile.properties)
    lcparentobj = ALLTRIM(MLINE(objfile.parent, 1))
    lcorigconname = IIF(EMPTY(lcparentobj), "", lcparentobj+".")+ALLTRIM(MLINE(objfile.objname, 1))
    lcrowkey = objfile.uniqueid
    lnwidth = 0
    lccaption = ""
    lctooltip = ""
    DIMENSION latext[1, 2]
    latext = ""
    lxa = 0
    FOR lnx = 1 TO lnmlines
       lcline = ALLTRIM(MLINE(properties, lnx))
       IF ('Width'$lcline)
          lnwidth = INT(VAL(ALLTRIM(SUBSTR(lcline, ATC('=', lcline, 1)+1))))
       ENDIF
       IF ('Caption'$lcline)
          lclangstr1 = ALLTRIM(SUBSTR(lcline, ATC('"', lcline, 1)+1))
          lccaption = SUBSTR(lclangstr1, 1, LEN(lclangstr1)-1)
          lccaption = STRTRAN(lccaption, "\<", "")
          lxa = lxa+1
          DIMENSION latext[lxa, 2]
          latext[lxa, 1] = lccaption
          latext[lxa, 2] = ""
          IF ATC('.Caption', lcline)>0
             latext[lxa, 2] = "."+SUBSTR(lcline, 1, ATC('.Caption', lcline)-1)
             lclinenext = ALLTRIM(MLINE(properties, lnx+1))
             lnxname = ('Name'$lclinenext)
             lnloop = lnx+1
             DO WHILE ( .NOT. lnxname .AND. lnmlines>=lnloop)
                lclinenext = ALLTRIM(MLINE(properties, lnloop))
                lnxname = ('Name'$lclinenext)
                lnloop = lnloop+1
             ENDDO
             IF lnxname
                lclangstr2 = ALLTRIM(SUBSTR(lclinenext, ATC('"', lclinenext, 1)+1))
                lccapname = SUBSTR(lclangstr2, 1, LEN(lclangstr2)-1)
                latext[lxa, 2] = "."+lccapname
             ENDIF
          ENDIF
       ENDIF
       IF ('ToolTipText'$lcline)
          lclangstr1 = ALLTRIM(SUBSTR(lcline, ATC('"', lcline, 1)+1))
          lctooltip = SUBSTR(lclangstr1, 1, LEN(lclangstr1)-1)
          lctooltip = STRTRAN(lctooltip, "\<", "")
          lxa = lxa+1
          DIMENSION latext[lxa, 2]
          latext[lxa, 1] = lctooltip
          latext[lxa, 2] = "TTIP"
       ENDIF
    ENDFOR
    lcorigconname1 = ALLTRIM(lcorigconname)
    SELECT sydenstring
    IF  .NOT. EMPTY(latext(1, 1))
       FOR ln = 1 TO ALEN(latext, 1)
          lctxtval = ALLTRIM(latext(ln, 1))
          IF  .NOT. EMPTY(ALLTRIM(latext(ln, 2))) .AND. latext(ln, 2)<>'TTIP'
             lcorigconname1 = ALLTRIM(lcorigconname)+ALLTRIM(latext(ln, 2))
          ENDIF
          APPEND BLANK
          REPLACE enstr_key WITH 0, obj_key WITH lnobjkey, row_key WITH lcrowkey
          REPLACE originalcontrolname WITH lcorigconname1, originalwidth WITH IIF(latext(ln, 2)='TTIP', 0, lnwidth)
          REPLACE originaltext WITH lctxtval, fulltext WITH lctxtval, cproperty WITH IIF(latext(ln, 2)='TTIP', 'ToolTipText', 'Caption')
       ENDFOR
    ENDIF
    SELECT objfile
 ENDSCAN
 SELECT objfile
 USE
 IF  .NOT. EMPTY(sydlangobj.gendatetime)
    thisform.domerge(ALLTRIM(STR(lnobjkey, 8)), 'SCX')
 ENDIF
 RETURN
ENDPROC
**
PROCEDURE generatehfile
 LPARAMETERS lctype, lcfile, lnobjkey
 SET MEMOWIDTH TO 200
 SELECT hfiledb2
 DELETE ALL
 SELECT hfiledb1
 DELETE ALL
 APPEND BLANK
 APPEND MEMO txtfile FROM (lcfile) OVERWRITE
 lnmlines = MEMLINES(hfiledb1.txtfile)
 LOCAL lcorgleft, lcupperleft
 SELECT hfiledb2
 FOR lnx = 1 TO lnmlines
    lcline = ALLTRIM(MLINE(hfiledb1.txtfile, lnx))
    IF EMPTY(lcline) .OR. INLIST(LEFT(ALLTRIM(lcline), 1), "*", "&")
       LOOP
    ENDIF
    lcorgleft = LEFT(lcline, 12)
    lcupperleft = UPPER(lcorgleft)
    IF lcupperleft<>'#DEFINE LANG'
       LOOP
    ENDIF
    APPEND BLANK
    IF OCCURS("'", lcline)=2 .OR. OCCURS('"', lcline)=2
       REPLACE feildval WITH ALLTRIM(lcline)
    ELSE
       lcnxtline = ALLTRIM(MLINE(hfiledb.txtfile, lnx+1))
       REPLACE feildval WITH ALLTRIM(lcline)+" "+lcnxtline
    ENDIF
 ENDFOR
 SELECT hfiledb2
 REPLACE feildval WITH SUBSTR(feildval, 9) ALL
 REPLACE langval WITH SUBSTR(ALLTRIM(SUBSTR(feildval, IIF(ATC('"', feildval, 1)=0, ATC("'", feildval, 1), ATC('"', feildval, 1))+1)), 1, LEN(ALLTRIM(SUBSTR(feildval, IIF(ATC('"', feildval, 1)=0, ATC("'", feildval, 1), ATC('"', feildval, 1))+1)))-1) ALL
 REPLACE feildval WITH ALLTRIM(SUBSTR(feildval, 1, IIF(ATC('"', feildval, 1)=0, ATC("'", feildval, 1), ATC('"', feildval, 1))-1)) ALL
 REPLACE langval WITH STRTRAN(langval, "\<", "") ALL
 REPLACE feildval WITH UPPER(ALLTRIM(feildval)) ALL
 SELECT sydenstring
 INSERT INTO SydEnString (enstr_key, obj_key, row_key, originalcontrolname, originaltext, fulltext, cproperty) SELECT 0, lnobjkey, feildval, feildval, langval, langval, 'Caption' FROM Hfiledb2
 IF  .NOT. EMPTY(sydlangobj.gendatetime)
    thisform.domerge(ALLTRIM(STR(lnobjkey, 8)))
 ENDIF
ENDPROC
**
PROCEDURE generaterep
 LPARAMETERS lctype, lcfile, lnobjkey
 USE SHARED (lcfile) ALIAS "OBJFILE" IN 0
 IF  .NOT. USED("OBJFILE")
    RETURN
 ENDIF
 SELECT objfile
 SCAN
    SELECT objfile
    IF  .NOT. EMPTY(expr) .AND. objtype=5
       lctext = ALLTRIM(LEFT(expr, 200))
       lcfulltext = SUBSTR(lctext, 2, LEN(lctext)-2)
       lcfulltext = STRTRAN(lcfulltext, "\<", "")
       SELECT sydenstring
       IF  .NOT. EMPTY(lcfulltext)
          APPEND BLANK
          REPLACE enstr_key WITH 0, obj_key WITH lnobjkey, row_key WITH objfile.uniqueid, originalcontrolname WITH 'Label', originalwidth WITH 0
          REPLACE originaltext WITH lcfulltext, fulltext WITH lcfulltext, cproperty WITH 'Caption'
       ENDIF
    ENDIF
    SELECT objfile
    IF  .NOT. EMPTY(ALLTRIM(expr)) .AND. ("'"$ALLTRIM(expr) .OR. '"'$ALLTRIM(expr)) .AND. objtype=8 .AND. (platform<>'DOS')
       IF thisform.needtobetranslated()
          lcfulltext = ALLTRIM(expr)
          SELECT sydenstring
          IF  .NOT. EMPTY(lcfulltext)
             APPEND BLANK
             REPLACE enstr_key WITH 0, obj_key WITH lnobjkey, row_key WITH objfile.uniqueid, originalcontrolname WITH 'Text', originalwidth WITH 0
             REPLACE originaltext WITH lcfulltext, fulltext WITH lcfulltext, cproperty WITH 'Expr'
          ENDIF
       ENDIF
    ENDIF
    SELECT objfile
 ENDSCAN
 SELECT objfile
 USE
 IF  .NOT. EMPTY(sydlangobj.gendatetime)
    thisform.domerge(ALLTRIM(STR(lnobjkey, 8)))
 ENDIF
ENDPROC
**
FUNCTION loadmain
 LPARAMETERS lcobjname, lnnewkey, lcext, lcver, lcobject
 lcstr = "SELECT Obj_Key,GenDateTime FROM SydLangObject WHERE cObjectType='"+lcext+"' and cVersion='"+lcver+"' and cObject='"+UPPER(lcobject)+"'"
 SQLEXEC(gnconnhandler, lcstr, "tb_LangObj")
 SELECT tb_langobj
 GOTO TOP
 lnobj_key = IIF( .NOT. EOF(), tb_langobj.obj_key, (-1*lnnewkey))
 ltgendatetime = IIF( .NOT. EOF(), tb_langobj.gendatetime, "")
 USE
 SELECT sydlangobj
 LOCATE FOR cobjecttype=lcext .AND. cversion=lcver .AND. cobject=UPPER(lcobject)
 IF  .NOT. FOUND()
    APPEND BLANK
    REPLACE obj_key WITH lnobj_key, cobjecttype WITH lcext, cversion WITH lcver, cobject WITH UPPER(lcobject), sel WITH 1, objfullpath WITH lcobjname
    IF  .NOT. EMPTY(ltgendatetime)
       REPLACE gendatetime WITH ltgendatetime, sel WITH 0
    ENDIF
    SELECT sydobjrelation
    APPEND BLANK
    REPLACE obj_key WITH lnobj_key, related_key WITH sydlangobj.obj_key
 ENDIF
 RETURN lnobj_key
ENDFUNC
**
PROCEDURE loadrelatedh
 LPARAMETERS lcobjname, lnnewkey, lcext, lcver, lcobject, lnobj_key
 SELECT objdbf
 SCAN FOR  .NOT. EMPTY(reserved8)
    lchname = MLINE(reserved8, 1)
    lnslash = OCCURS('\', lchname)
    IF lnslash>0
       lchfile = SUBSTR(lchname, ATC('\', lchname, lnslash)+1)
    ELSE
       lchfile = ALLTRIM(lchname)
    ENDIF
    lcstr = "SELECT Obj_Key,GenDateTime FROM SydLangObject WHERE cObjectType='H' and cVersion='40' and cObject='"+UPPER(lchfile)+"'"
    SQLEXEC(gnconnhandler, lcstr, "tb_LangObj")
    SELECT tb_langobj
    GOTO TOP
    lnobj_keyh = IIF( .NOT. EOF(), tb_langobj.obj_key, (-1000*lnnewkey))
    ltgendateth = IIF( .NOT. EOF(), tb_langobj.gendatetime, "")
    USE
    lnslashh = OCCURS('\', lcobjname)
    lcobjpath = SUBSTR(lcobjname, 1, ATC('\', lcobjname, lnslashh))
    SELECT sydlangobj
    LOCATE FOR cobjecttype="H  " .AND. cversion=lcver .AND. cobject=UPPER(lchfile)
    IF  .NOT. FOUND()
       APPEND BLANK
       REPLACE obj_key WITH lnobj_keyh, cobjecttype WITH "H", cversion WITH lcver, cobject WITH UPPER(lchfile), sel WITH 1, objfullpath WITH lcobjpath+UPPER(lchfile)
       IF  .NOT. EMPTY(ltgendateth)
          REPLACE gendatetime WITH ltgendateth, sel WITH 0
       ENDIF
       SELECT sydobjrelation
       APPEND BLANK
       REPLACE obj_key WITH lnobj_key, related_key WITH sydlangobj.obj_key
    ENDIF
    SELECT objdbf
 ENDSCAN
 RETURN
ENDPROC
**
PROCEDURE loadrelatedclass
 LPARAMETERS lcobjname, lnnewkey, lcext, lcver, lcobject, lnobj_key
 lnlibcnt = 0
 SELECT tempclass
 SCAN
    xclassloc = ALLTRIM(LEFT(clib, 200))
    lnslash = OCCURS("\", xclassloc)
    IF lnslash>0
       lclibfile = SUBSTR(xclassloc, ATC("\", xclassloc, lnslash)+1)
       IF lnslash=1
          lclibpath = ariaapppath+SUBSTR(xclassloc, 1, ATC("\", xclassloc, 1))
       ELSE
          lclibpath = ariaapppath+SUBSTR(xclassloc, ATC("\", xclassloc, lnslash-1)+1, ATC("\", xclassloc, lnslash)-ATC("\", xclassloc, lnslash-1))
       ENDIF
    ELSE
       lclibfile = xclassloc
       lclibpath = ariaapppath+"Classes\"
    ENDIF
    lcstr = "SELECT Obj_Key,GenDateTime FROM SydLangObject WHERE cObjectType='VCX' and cVersion='40' and cObject='"+UPPER(lclibfile)+"'"
    SQLEXEC(gnconnhandler, lcstr, "tb_LangObj")
    SELECT tb_langobj
    GOTO TOP
    lnlibcnt = lnlibcnt+1
    lnobj_keylib = IIF( .NOT. EOF(), tb_langobj.obj_key, (-1000*lnnewkey)+lnlibcnt)
    ltgendatetlb = IIF( .NOT. EOF(), tb_langobj.gendatetime, "")
    USE
    SELECT sydlangobj
    LOCATE FOR cobjecttype="VCX" .AND. cversion=lcver .AND. cobject=UPPER(lclibfile)
    IF  .NOT. FOUND()
       APPEND BLANK
       REPLACE obj_key WITH lnobj_keylib, cobjecttype WITH "VCX", cversion WITH lcver, cobject WITH UPPER(lclibfile), sel WITH 1, objfullpath WITH lclibpath+UPPER(lclibfile)
       IF  .NOT. EMPTY(ltgendatetlb)
          REPLACE gendatetime WITH ltgendatetlb, sel WITH 0
       ENDIF
    ENDIF
    SELECT sydobjrelation
    LOCATE FOR obj_key=lnobj_key .AND. related_key=sydlangobj.obj_key
    IF  .NOT. FOUND()
       APPEND BLANK
       REPLACE obj_key WITH lnobj_key, related_key WITH sydlangobj.obj_key
    ENDIF
    SELECT tempclass
 ENDSCAN
 SELECT tempclass
 USE
 RETURN
ENDPROC
**
FUNCTION generatedbf
 LPARAMETERS lctype, lcfile, lnobjkey
 lcdbfile = UPPER(ALLTRIM(lcfile))
 IF thisform.combo3.value="B"
    SELECT 0
    USE SHARED (ariasyspath+"sycinst.DBF")
    lcsypath = ALLTRIM(sycinst.ca4sysdir)
    USE
 ELSE
    lcsypath = ariasyspath
 ENDIF
 IF  .NOT. FILE(ariasyspath+"SYDINDEX.DBF")
    MESSAGEBOX("SYDINDEX does not exist at "+ariasyspath)
    RETURN .F.
 ENDIF
 SELECT 0
 USE SHARED (ariasyspath+"SYDINDEX.DBF")
 SELECT sydindex
 LOCATE FOR cfile_nam=lcdbfile .AND. lunique
 IF  .NOT. FOUND()
    MESSAGEBOX("No Unique index found for table "+lcfile)
    RETURN .F.
 ENDIF
 lcprimary = sydindex.cindx_exp
 SELECT sydindex
 USE
 IF  .NOT. FILE(lcsypath+lcdbfile+".DBF")
    MESSAGEBOX(lcdbfile+" system file does not exist at "+lcsypath)
    RETURN .F.
 ENDIF
 USE SHARED (lcsypath+lcdbfile)
 IF thisform.combo3.value="B"
    lcversion = "40"
 ELSE
    lcversion = "27"
 ENDIF
 SELECT (lcdbfile)
 SCAN
    lcrowkey = &lcprimary
    lcorigconname = ""
    lccaption = ""
    SELECT syddict
    SCAN FOR cversion=lcversion .AND. ctable=lcdbfile
       lcorigconname = cfield
       lcfield = lcdbfile+"."+cfield
       lccaption = &lcfield
       lccaption = STRTRAN(lccaption, "\<", "")
       lccaption = STRTRAN(lccaption, "\-", "")
       lccaption = STRTRAN(lccaption, ";", '//')
       lccaption = STRTRAN(lccaption, "\!", '')
       lccaption = STRTRAN(lccaption, "\?", '')
       lccaption = STRTRAN(lccaption, 'ð', "<<>>")
       IF  .NOT. EMPTY(lccaption)
          SELECT sydenstring
          APPEND BLANK
          REPLACE enstr_key WITH 0, obj_key WITH lnobjkey, row_key WITH lcrowkey, originalcontrolname WITH 'Field Text', originalwidth WITH 0
          REPLACE originaltext WITH lccaption, fulltext WITH lccaption, cproperty WITH lcorigconname
       ENDIF
       SELECT syddict
    ENDSCAN
 ENDSCAN
 IF  .NOT. EMPTY(sydlangobj.gendatetime)
    thisform.domerge(ALLTRIM(STR(lnobjkey, 8)))
 ENDIF
 SELECT (lcdbfile)
 USE
 RETURN
ENDFUNC
**
FUNCTION needtobetranslated
 lcstring = ALLTRIM(expr)
 lcret = .F.
 IF AT("'", lcstring)>0
    noccurs = OCCURS("'", lcstring)
    IF noccurs>1
       FOR i1 = 2 TO noccurs STEP 2
          cstr = SUBSTR(lcstring, AT("'", lcstring, i1-1)+1, LEN(lcstring))
          cstr = SUBSTR(cstr, 1, AT("'", cstr)-1)
          IF LEN(cstr)>1 .AND.  .NOT. (SUBSTR(cstr, 1, 1)=='.') .AND.  .NOT. ('99'$cstr) .AND.  .NOT. (SUBSTR(cstr, 1, 1)=='@')
             lcret = .T.
             EXIT
          ENDIF
       ENDFOR
    ENDIF
 ENDIF
 IF lcret
    RETURN lcret
 ENDIF
 lcstring = ALLTRIM(expr)
 lcret = .F.
 IF AT('"', lcstring)>0
    noccurs = OCCURS('"', lcstring)
    IF noccurs>1
       FOR i1 = 2 TO noccurs STEP 2
          cstr = SUBSTR(lcstring, AT('"', lcstring, i1-1)+1, LEN(lcstring))
          cstr = SUBSTR(cstr, 1, AT('"', cstr)-1)
          IF LEN(cstr)>1 .AND.  .NOT. (SUBSTR(cstr, 1, 1)=='.') .AND.  .NOT. ('99'$cstr) .AND.  .NOT. (SUBSTR(cstr, 1, 1)=='@')
             lcret = .T.
             EXIT
          ENDIF
       ENDFOR
    ENDIF
 ENDIF
 RETURN lcret
ENDFUNC
**
PROCEDURE Init
 thisform.usemode = "S"
 thisform.screenmodes()
 IF  .NOT. EMPTY(ariaapppath)
    SET DEFAULT TO (ariaapppath)
 ENDIF
 SET DELETED ON
ENDPROC
**
PROCEDURE Load
 CREATE CURSOR "HFILEDB1" (txtfile MEMO)
 CREATE CURSOR "HFILEDB2" (feildval CHAR (200), langval CHAR (200))
 lcstr = "SELECT TOP 0 [Obj_Key],0 AS 'SEL',0 AS 'NEWREC',SPACE(250) AS 'Objfullpath ',[cObjectType],[cVersion],[cObject],[GenDateTime]   FROM SydLangObject"
 SQLEXEC(gnconnhandler, lcstr, "SydLangObj")
 lcstr = "SELECT TOP 0 Obj_Key,Related_Key FROM SydObjRelation"
 SQLEXEC(gnconnhandler, lcstr, "SydObjRelation")
 lcstr = "SELECT cVersion,cTable,cField FROM SydDict"
 SQLEXEC(gnconnhandler, lcstr, "SydDict")
 lcstr = "SELECT distinct cVersion ,cTable FROM SydDict"
 SQLEXEC(gnconnhandler, lcstr, "SysTables")
 lcstr = "SELECT TOP 0 [EnStr_Key],[Obj_Key] ,[Row_Key] ,[cProperty] ,[OriginalText] ,[FullText] ,[OriginalWidth] ,[OriginalControlName] FROM SydEnString"
 SQLEXEC(gnconnhandler, lcstr, "SydEnString")
ENDPROC
**
*
*  langgen.scx::cmdGen
**
PROCEDURE Click
 WAIT WINDOW NOWAIT "Generating..., Please wait!"
 SELECT sydlangobj
 LOCATE FOR sel=1
 IF  .NOT. FOUND()
    GOTO TOP
    MESSAGEBOX("Nothing selected to Generate!", 64, "Translation Tool")
    thisform.grid1.refresh
    RETURN
 ENDIF
 llret = .T.
 SELECT sydlangobj
 SCAN FOR sel=1
    llret = thisform.dogenerate(ALLTRIM(sydlangobj.cobjecttype), ALLTRIM(sydlangobj.objfullpath), sydlangobj.obj_key)
    IF  .NOT. llret
       REPLACE sel WITH 0
       IF ALLTRIM(sydlangobj.cobjecttype)="DBF"
          RETURN
       ENDIF
    ELSE
       SELECT sydlangobj
       IF EMPTY(gendatetime)
          REPLACE gendatetime WITH DATETIME(), newrec WITH 1
       ENDIF
    ENDIF
 ENDSCAN
 SELECT sydlangobj
 GOTO TOP
 thisform.grid1.refresh
 SELECT sydenstring
 GOTO TOP
 thisform.grid2.refresh
 WAIT CLEAR
 thisform.usemode = "G"
 thisform.screenmodes()
 RETURN
ENDPROC
**
*
*  langgen.scx::cmdSel2
**
PROCEDURE Click
 lcext = thisform.getfileextention(thisform.combo1.value)
 thisform.txtobject.value = GETFILE(lcext, "Select "+ALLTRIM(thisform.combo1.displayvalue))
 IF  .NOT. EMPTY(thisform.txtobject.value)
    thisform.selectionbase = "S"
    thisform.txtobject.tag = thisform.combo1.value
 ELSE
    thisform.txtobject.tag = ""
 ENDIF
ENDPROC
**
*
*  langgen.scx::cmdSel1
**
PROCEDURE Click
 thisform.txtobject.value = GETDIR()
 IF  .NOT. EMPTY(thisform.txtobject.value)
    thisform.selectionbase = "M"
 ENDIF
ENDPROC
**
*
*  langgen.scx::cmdLoad
**
PROCEDURE Click
 IF thisform.combo1.value='S'
    thisform.txtobject.value = ALLTRIM(thisform.cbosysfile.value)
    thisform.selectionbase = "S"
    thisform.txtobject.tag = thisform.combo1.value
 ENDIF
 IF  .NOT. EMPTY(thisform.txtobject.value)
    WAIT WINDOW NOWAIT "Loading..."
    IF thisform.selectionbase="S"
       thisform.combo1.value = thisform.txtobject.tag
       thisform.loadobject(ALLTRIM(thisform.txtobject.value), 1)
       thisform.usemode = "E"
       thisform.screenmodes()
    ELSE
       lcext = ALLTRIM(thisform.getfileextention(thisform.combo1.value))
       lcselectddir = ALLTRIM(thisform.txtobject.value)
       SET DEFAULT TO (lcselectddir)
       lnfilesnum = ADIR(ladir, '*.'+lcext)
       IF lnfilesnum>0
          lnnewkeycnt = 0
          FOR lnx = 1 TO lnfilesnum
             lnnewkeycnt = lnnewkeycnt+1
             thisform.loadobject(ALLTRIM(thisform.txtobject.value)+ALLTRIM(ladir(lnx, 1)), lnnewkeycnt)
          ENDFOR
          thisform.usemode = "E"
          thisform.screenmodes()
       ELSE
          WAIT CLEAR
          MESSAGEBOX("There is no "+ALLTRIM(thisform.combo1.displayvalue)+IIF(thisform.combo1.value="C", "e", "")+"s in selected directory, Nothing to load!", 64, "Translation Tool")
       ENDIF
       SET DEFAULT TO (ariaapppath)
    ENDIF
    WAIT CLEAR
 ELSE
    MESSAGEBOX("Nothing selected to load!", 64, "Translation Tool")
 ENDIF
ENDPROC
**
*
*  langgen.scx::cmdselall
**
PROCEDURE Click
 SELECT sydlangobj
 REPLACE sel WITH 1 ALL
 GOTO TOP
 thisform.grid1.refresh
ENDPROC
**
*
*  langgen.scx::cmdselnone
**
PROCEDURE Click
 SELECT sydlangobj
 REPLACE sel WITH 0 ALL
 GOTO TOP
 thisform.grid1.refresh
ENDPROC
**
*
*  langgen.scx::cmdSave
**
PROCEDURE Click
 WAIT WINDOW NOWAIT "Saving..., Please wait!"
 SELECT sydlangobj
 SCAN FOR sel=1
    SCATTER MEMVAR
    lcrelflpth = STRTRAN(UPPER(m.objfullpath), UPPER(ariaapppath), "")
    lcrelatvepath = STRTRAN(lcrelflpth, UPPER(ALLTRIM(m.cobject)), "")
    IF newrec=1
       lcstr = "INSERT INTO SydLangObject (cObjectType,cVersion,cObject,GenUser,GenDateTime,ModifyUser,ModifyDateTime,ObjectSource) "
       lcstr = lcstr+"VALUES (?m.cObjectType,?m.cVersion,?m.cObject,'',?m.GenDateTime,'',?m.GenDateTime,?lcRelatvePath )"
       lnres = SQLEXEC(gnconnhandler, lcstr)
       IF lnres>0
          lcstr = "SELECT Obj_Key FROM SydLangObject WHERE cObjectType=?m.cObjectType and cVersion=?m.cVersion and cObject=?m.cObject"
          SQLEXEC(gnconnhandler, lcstr, "tb_LangObj")
          lnkey = tb_langobj.obj_key
          SELECT tb_langobj
          USE
          SELECT sydlangobj
          thisform.dosavenew(m.obj_key, lnkey)
       ENDIF
    ELSE
       m.modifydatetime = DATETIME()
       lcstr = "UPDATE SydLangObject  SET ModifyUser = '',ModifyDateTime =?m.ModifyDateTime, ObjectSource=?lcRelatvePath "
       lcstr = lcstr+"WHERE Obj_Key=?m.Obj_Key"
       lnres = SQLEXEC(gnconnhandler, lcstr)
       IF lnres>0
          thisform.dosaveupd(m.obj_key)
       ENDIF
    ENDIF
    SELECT sydlangobj
 ENDSCAN
 SELECT sydobjrelation
 SCAN
    SCATTER MEMVAR
    IF m.related_key>0
       lcstr = "INSERT INTO SydObjRelation (Obj_Key,Related_Key) VALUES (?m.Obj_Key,?m.Related_Key)"
       lnres = SQLEXEC(gnconnhandler, lcstr)
    ENDIF
 ENDSCAN
 WAIT CLEAR
 MESSAGEBOX("Data successfully saved!", 64)
 thisform.usemode = "S"
 thisform.screenmodes()
 thisform.cleartables('O')
 thisform.cleartables('L')
 RETURN
ENDPROC
**
*
*  langgen.scx::cmdCancel
**
PROCEDURE Click
 IF thisform.usemode="E"
    thisform.usemode = "S"
    thisform.screenmodes()
    thisform.cleartables('O')
 ELSE
    IF thisform.usemode="G"
       thisform.usemode = "E"
       thisform.screenmodes()
       thisform.cleartables('L')
       SELECT sydlangobj
       REPLACE gendatetime WITH CTOT(''), newrec WITH 0 ALL FOR newrec=1
       GOTO TOP
       thisform.grid1.refresh
    ENDIF
 ENDIF
ENDPROC
**
*
*  langgen.scx::cmdExit
**
PROCEDURE Click
 thisform.cbosysfile.rowsource = ""
 thisform.cleartables('O')
 thisform.cleartables('L')
 CLOSE DATABASES
 thisform.release
ENDPROC
**
*
*  langgen.scx::Combo1
**
PROCEDURE InteractiveChange
 thisform.combo3.value = 'B'
 thisform.txtobject.value = ""
 IF this.value='S'
    lnaleas = SELECT()
    SELECT systables
    IF thisform.combo3.value="B"
       SET FILTER TO cversion="40"
    ELSE
       SET FILTER TO cversion="27"
    ENDIF
    this.requery
    this.refresh
    SELECT (lnaleas)
    thisform.combo3.enabled = .T.
    thisform.cmdsel1.enabled = .F.
    thisform.cmdsel2.enabled = .F.
    thisform.cbosysfile.visible = .T.
 ELSE
    thisform.combo3.enabled = .F.
    thisform.cmdsel1.enabled = .T.
    thisform.cmdsel2.enabled = .T.
    thisform.cbosysfile.visible = .F.
 ENDIF
ENDPROC
**
PROCEDURE Init
 this.value = 'F'
ENDPROC
**
*
*  langgen.scx::Combo3
**
PROCEDURE InteractiveChange
 lnaleas = SELECT()
 SELECT systables
 IF this.value="B"
    SET FILTER TO cversion="40"
 ELSE
    SET FILTER TO cversion="27"
 ENDIF
 thisform.cbosysfile.requery
 thisform.cbosysfile.refresh
 SELECT (lnaleas)
ENDPROC
**
PROCEDURE Init
 this.enabled = .F.
 this.value = 'B'
ENDPROC
**
***
*** langgen.sct
***
*
***
*** zlangdest.scx
***
*
*
*  zlangdest.scx::Form1
**
FUNCTION Init
 SELECT sydlangobj
 GOTO TOP
 IF EOF()
    USE
    MESSAGEBOX("You need to Generate Objects before start Distribute them.", 64)
    RETURN .F.
 ENDIF
 thisform.combo2.rowsource = "SycLang.cshort_des,clang_id"
 thisform.combo2.requery
 thisform.combo2.refresh
 thisform.combo2.value = syclang.clang_id
 thisform.cmdexport.enabled = .F.
ENDFUNC
**
PROCEDURE Load
 lcstr = "SELECT [Obj_Key],0 AS 'SEL',0 AS 'NEWREC',SPACE(250) AS 'Objfullpath ',[cObjectType],[cVersion],[cObject],[GenDateTime],[ObjectSource]  FROM SydLangObject Order by cObject"
 SQLEXEC(gnconnhandler, lcstr, "SydLangObj")
 SELECT sydlangobj
 GOTO TOP
 IF  .NOT. EOF()
    lcstr = "SELECT [cLang_ID]+' - '+[cShort_Des] as cShort_Des,[cLang_ID],ccodepage FROM SycLang"
    SQLEXEC(gnconnhandler, lcstr, "SycLang")
    lcstr = "SELECT TOP 0 [EnStr_Key] ,[LangText],[Obj_Key],[Row_Key],[cProperty],[OriginalText] ,[FullText],[OriginalWidth],[OriginalControlName] ,[cObjectType],[ObjectSource],[cObject],[cVersion] FROM vw_LangStr"
    SQLEXEC(gnconnhandler, lcstr, "vw_LangStr")
 ENDIF
ENDPROC
**
*
*  zlangdest.scx::cmdexport
**
PROCEDURE Click
 lclangid = ALLTRIM(thisform.cmddest.tag)
 SET DEFAULT TO (ariaapppath)
 IF  .NOT. DIRECTORY(ariaapppath+"Lang\", 1)
    MD (ariaapppath+"Lang\")
 ENDIF
 lclangdir = ariaapppath+"Lang\"+lclangid+"\"
 IF  .NOT. DIRECTORY(lclangdir, 1)
    MD (lclangdir)
 ENDIF
 SELECT syclang
 LOCATE FOR clang_id=lclangid
 lccp = ALLTRIM(ccodepage)
 WAIT WINDOW NOWAIT "Generating Language XML files..."
 SELECT DISTINCT cobjecttype, objectsource, obj_key, cobject, cversion FROM vw_LangStr INTO CURSOR tmpObj
 SELECT tmpobj
 SCAN
    lnobkey = tmpobj.obj_key
    IF tmpobj.cobjecttype="DBF"
       lctargloc = lclangdir+"Sysfiles"+cversion+"\"
    ELSE
       lctargloc = lclangdir+ALLTRIM(objectsource)
    ENDIF
    IF  .NOT. DIRECTORY(lctargloc, 1)
       MD (lctargloc)
    ENDIF
    lcxmlfile = lctargloc+STRTRAN(ALLTRIM(cobject), ".", "_")+".xml"
    lccursor = STRTRAN(ALLTRIM(cobject), ".", "_")+"_"+lclangid
    TRY
       SELECT row_key, cproperty, originaltext AS orgtext, originalcontrolname AS orgcol, langtext FROM vw_LangStr WHERE obj_key=lnobkey INTO CURSOR (lccursor)
       SELECT (lccursor)
       GOTO TOP
       IF  .NOT. EOF()
          IF FILE(lcxmlfile)
             DELETE FILE (lcxmlfile)
          ENDIF
          CURSORTOXML(lccursor, lcxmlfile, 2, 512, 0, "")
          lcsmlstr = FILETOSTR(lcxmlfile)
          lcsmlstr = STRTRAN(lcsmlstr, "1252", lccp)
          STRTOFILE(lcsmlstr, lcxmlfile)
       ENDIF
       SELECT (lccursor)
       USE
    CATCH
       WAIT WINDOW MESSAGE()
    ENDTRY
    SELECT tmpobj
 ENDSCAN
 SELECT tmpobj
 USE
 SET DEFAULT TO (gccurpath)
 WAIT CLEAR
 MESSAGEBOX("Data successfully exported!", 64)
 RETURN
ENDPROC
**
*
*  zlangdest.scx::cmdselall
**
PROCEDURE Click
 SELECT sydlangobj
 REPLACE sel WITH 1 ALL
 GOTO TOP
 thisform.grid1.refresh
ENDPROC
**
*
*  zlangdest.scx::cmdselnone
**
PROCEDURE Click
 SELECT sydlangobj
 REPLACE sel WITH 0 ALL
 GOTO TOP
 thisform.grid1.refresh
ENDPROC
**
*
*  zlangdest.scx::cmdExit
**
PROCEDURE Click
 thisform.combo2.rowsource = ""
 CLOSE DATABASES
 thisform.release
ENDPROC
**
*
*  zlangdest.scx::cmddest
**
PROCEDURE Click
 thisform.grid2.column3.header1.caption = "Translated Text"
 SELECT sydlangobj
 LOCATE FOR sel=1
 IF  .NOT. FOUND()
    MESSAGEBOX("Nothig selected, unable to proceed.", 64)
    RETURN
 ENDIF
 WAIT WINDOW NOWAIT "Loading Language Strings for distribution..."
 SELECT vw_langstr
 DELETE ALL
 lclang = ALLTRIM(thisform.combo2.value)
 LOCAL lcstr
 SELECT sydlangobj
 SCAN FOR sel=1
    WAIT WINDOW NOWAIT "Loading Language Strings for distribution for "+ALLTRIM(sydlangobj.cobject)
    lcstr = "SELECT EnStr_Key ,[LangText],[Obj_Key],[Row_Key],[cProperty],[OriginalText] ,[FullText],[OriginalWidth],[OriginalControlName]"+",[cObjectType],[ObjectSource],[cObject],[cVersion] FROM vw_LangStr WHERE cLang_ID='"+lclang+"'"
    lcstr = lcstr+" AND Obj_Key = "+ALLTRIM(STR(sydlangobj.obj_key))
    SQLEXEC(gnconnhandler, lcstr, "tm_LangStr")
    TRY
       SELECT tm_langstr
       SCAN
          SCATTER MEMO MEMVAR
          SELECT vw_langstr
          APPEND BLANK
          GATHER MEMO MEMVAR
       ENDSCAN
       SELECT tm_langstr
       USE
    CATCH
    ENDTRY
 ENDSCAN
 SELECT sydlangobj
 SCAN FOR sel=1
    lnobkey = sydlangobj.obj_key
    lclngscr1 = sydlangobj.objectsource
    lcstr = "SELECT cObject FROM vw_LangRelation WHERE Obj_Key="+ALLTRIM(STR(lnobkey, 10))+" and Related_Key<>"+ALLTRIM(STR(lnobkey, 10))
    SQLEXEC(gnconnhandler, lcstr, "vw_Relat")
    SELECT vw_relat
    SCAN
       lcrelob = ALLTRIM(cobject)
       IF ATC('.H', lcrelob)>0
          SELECT vw_langstr
          APPEND BLANK
          REPLACE obj_key WITH lnobkey, row_key WITH "XX_HEADER_XX", cproperty WITH lcrelob, originaltext WITH lclngscr1
       ENDIF
    ENDSCAN
    SELECT vw_relat
    USE
 ENDSCAN
 SELECT vw_langstr
 LOCATE FOR  .NOT. EMPTY(langtext)
 IF EOF()
    GOTO TOP
    thisform.grid2.refresh
    MESSAGEBOX("No text was translated for the target language and selected object(s)  , Unable to proceed.", 64)
 ELSE
    GOTO TOP
    thisform.grid2.refresh
    thisform.grid2.column3.header1.caption = ALLTRIM(thisform.combo2.displayvalue)+" Translated Text"
    thisform.cmddest.tag = ALLTRIM(thisform.combo2.value)
    thisform.cmdexport.enabled = .T.
 ENDIF
 WAIT CLEAR
 RETURN
ENDPROC
**
*
*  zlangdest.scx::Command1
**
PROCEDURE Click
**
** ReFox - this procedure is empty **
**
ENDPROC
**
***
*** zlangdest.sct
***
*
***
*** refresh.bmp
***
*
***
*** langdest.scx
***
*
*
*  langdest.scx::Form1
**
PROCEDURE exporttoxls
 LPARAMETERS ctabla, lcxlsfile
 oexcel = CREATEOBJECT("Excel.Application")
 oexcel.workbooks.add
 SELE &ctabla
 SET TALK OFF
 nnumregexp = RECCOUNT()
 GOTO TOP
 nrow = 0
 SCAN FOR errlen=1
    nrow = nrow+1
    WAIT WINDOW NOWAIT "Exporting data: "+ALLTRIM(STR(nrow))
    IF nrow=1
       FOR ncolumn = 1 TO FCOUNT()
          oexcel.activesheet.cells(nrow, ncolumn).value = FIELD(ncolumn)
       ENDFOR
    ENDIF
    FOR ncolumn = 1 TO FCOUNT()
       oexcel.activesheet.cells(nrow+1, ncolumn).value = IIF(TYPE(FIELD(ncolumn))=="G", CHR(34)+FIELD(ncolumn)+CHR(34), IIF(EMPTY(EVALUATE(FIELD(ncolumn))), "", EVALUATE(FIELD(ncolumn))))
    ENDFOR
 ENDSCAN
 ERASE (lcxlsfile)
 oexcel.activeworkbook.saveas(lcxlsfile)
 oexcel.activeworkbook.close
 oexcel.quit
ENDPROC
**
PROCEDURE Load
 lcstr = "SELECT [Obj_Key],0 AS 'SEL',0 AS 'NEWREC',SPACE(250) AS 'Objfullpath ',[cObjectType],[cVersion],[cObject],[GenDateTime],[ObjectSource]  FROM SydLangObject Order by cObject"
 SQLEXEC(gnconnhandler, lcstr, "SydLangObj")
 SELECT sydlangobj
 GOTO TOP
 IF  .NOT. EOF()
    lcstr = "SELECT [cLang_ID]+' - '+[cShort_Des] as cShort_Des,[cLang_ID],ccodepage FROM SycLang"
    SQLEXEC(gnconnhandler, lcstr, "SycLang")
    lcstr = "SELECT TOP 0 [EnStr_Key] ,[LangText],[Obj_Key],[Row_Key],[cProperty],[OriginalText] ,"+"[FullText],[OriginalWidth],[OriginalControlName] ,[cObjectType],[ObjectSource],[cObject],[cVersion],'          ' as [LenOrg],'          ' as [LenTran],0 as [ErrLen] FROM vw_LangStr"
    SQLEXEC(gnconnhandler, lcstr, "vw_LangStr")
 ENDIF
ENDPROC
**
FUNCTION Init
 SELECT sydlangobj
 GOTO TOP
 IF EOF()
    USE
    MESSAGEBOX("You need to Generate Objects before start Distribute them.", 64)
    RETURN .F.
 ENDIF
 thisform.combo2.rowsource = "SycLang.cshort_des,clang_id"
 thisform.combo2.requery
 thisform.combo2.refresh
 thisform.combo2.value = syclang.clang_id
 thisform.cmdexport.enabled = .F.
 thisform.cmderrorexport.enabled = .F.
ENDFUNC
**
*
*  langdest.scx::cmdexport
**
PROCEDURE Click
 lclangid = ALLTRIM(thisform.cmddest.tag)
 SET DEFAULT TO (ariaapppath)
 IF  .NOT. DIRECTORY(ariaapppath+"Lang\", 1)
    MD (ariaapppath+"Lang\")
 ENDIF
 lclangdir = ariaapppath+"Lang\"+lclangid+"\"
 IF  .NOT. DIRECTORY(lclangdir, 1)
    MD (lclangdir)
 ENDIF
 SELECT syclang
 LOCATE FOR clang_id=lclangid
 lccp = ALLTRIM(ccodepage)
 WAIT WINDOW NOWAIT "Generating Language XML files..."
 SELECT DISTINCT cobjecttype, objectsource, obj_key, cobject, cversion FROM vw_LangStr INTO CURSOR tmpObj
 SELECT tmpobj
 SCAN
    lnobkey = tmpobj.obj_key
    IF tmpobj.cobjecttype="DBF"
       lctargloc = lclangdir+"Sysfiles"+cversion+"\"
    ELSE
       lctargloc = lclangdir+ALLTRIM(objectsource)
    ENDIF
    IF  .NOT. DIRECTORY(lctargloc, 1)
       MD (lctargloc)
    ENDIF
    lcxmlfile = lctargloc+STRTRAN(ALLTRIM(cobject), ".", "_")+".xml"
    lcdbffile = lctargloc+STRTRAN(ALLTRIM(cobject), ".", "_")+".DBF"
    lccursor = STRTRAN(ALLTRIM(cobject), ".", "_")+"_"+lclangid
    TRY
       SELECT row_key, cproperty, originaltext AS orgtext, originalcontrolname AS orgcol, langtext FROM vw_LangStr WHERE obj_key=lnobkey INTO CURSOR (lccursor)
       SELECT (lccursor)
       GOTO TOP
       IF  .NOT. EOF()
          IF FILE(lcxmlfile)
             DELETE FILE (lcxmlfile)
          ENDIF
          CURSORTOXML(lccursor, lcxmlfile, 2, 512, 0, "")
          lcsmlstr = FILETOSTR(lcxmlfile)
          lcsmlstr = STRTRAN(lcsmlstr, "1252", lccp)
          STRTOFILE(lcsmlstr, lcxmlfile)
       ENDIF
       SELECT (lccursor)
       USE
    CATCH
       WAIT WINDOW NOWAIT MESSAGE()
    ENDTRY
    SELECT tmpobj
 ENDSCAN
 SELECT tmpobj
 USE
 SET DEFAULT TO (gccurpath)
 WAIT CLEAR
 MESSAGEBOX("Data successfully exported!", 64)
 RETURN
ENDPROC
**
*
*  langdest.scx::cmdselall
**
PROCEDURE Click
 SELECT sydlangobj
 REPLACE sel WITH 1 ALL
 GOTO TOP
 thisform.grid1.refresh
ENDPROC
**
*
*  langdest.scx::cmdselnone
**
PROCEDURE Click
 SELECT sydlangobj
 REPLACE sel WITH 0 ALL
 GOTO TOP
 thisform.grid1.refresh
ENDPROC
**
*
*  langdest.scx::cmdExit
**
PROCEDURE Click
 thisform.combo2.rowsource = ""
 CLOSE DATABASES
 thisform.release
ENDPROC
**
*
*  langdest.scx::cmddest
**
PROCEDURE Click
 thisform.grid2.column3.header1.caption = "Translated Text"
 SELECT sydlangobj
 LOCATE FOR sel=1
 IF  .NOT. FOUND()
    MESSAGEBOX("Nothig selected, unable to proceed.", 64)
    RETURN
 ENDIF
 WAIT WINDOW NOWAIT "Loading Language Strings for distribution..."
 SELECT vw_langstr
 DELETE ALL
 lclang = ALLTRIM(thisform.combo2.value)
 LOCAL lcstr
 SELECT sydlangobj
 SCAN FOR sel=1
    WAIT WINDOW NOWAIT "Loading Language Strings for distribution for "+ALLTRIM(sydlangobj.cobject)
    lcstr = "SELECT EnStr_Key ,[LangText],[Obj_Key],[Row_Key],[cProperty],[OriginalText] ,[FullText],[OriginalWidth],[OriginalControlName]"+",[cObjectType],[ObjectSource],[cObject],[cVersion] FROM vw_LangStr WHERE cLang_ID='"+lclang+"'"
    lcstr = lcstr+" AND Obj_Key = "+ALLTRIM(STR(sydlangobj.obj_key))
    SQLEXEC(gnconnhandler, lcstr, "tm_LangStr")
    TRY
       SELECT tm_langstr
       SCAN
          SCATTER MEMO MEMVAR
          SELECT vw_langstr
          APPEND BLANK
          GATHER MEMO MEMVAR
          clenorg = ALLTRIM(vw_langstr.originaltext)
          nlenorg = thisform.textwidth(clenorg)
          clenorg = ALLTRIM(STR(nlenorg))
          clentrn = ALLTRIM(vw_langstr.langtext)
          nlentrn = thisform.textwidth(clentrn)
          clentrn = ALLTRIM(STR(nlentrn))
          nflag = IIF(nlentrn<=(nlenorg+5), 0, 1)
          REPLACE 'LenOrg' WITH clenorg, 'LenTran' WITH clentrn, 'ErrLen' WITH nflag
       ENDSCAN
       SELECT tm_langstr
       USE
    CATCH
       WAIT WINDOW NOWAIT MESSAGE()
    ENDTRY
 ENDSCAN
 SELECT sydlangobj
 SCAN FOR sel=1
    lnobkey = sydlangobj.obj_key
    lclngscr1 = sydlangobj.objectsource
    lcstr = "SELECT cObject,Related_Key FROM vw_LangRelation WHERE Obj_Key="+ALLTRIM(STR(lnobkey, 10))+" and Related_Key<>"+ALLTRIM(STR(lnobkey, 10))
    TRY
       SQLEXEC(gnconnhandler, lcstr, "vw_Relat")
       SELECT vw_relat
       SCAN
          lclngscr2 = lclngscr1
          TRY
             lcstr = "SELECT ObjectSource FROM SydLangObject WHERE Obj_Key="+ALLTRIM(STR(vw_relat.related_key, 10))
             SQLEXEC(gnconnhandler, lcstr, "vw_Relat_P")
             SELECT vw_relat_p
             lclngscr2 = ALLTRIM(vw_relat_p.objectsource)
             USE
          CATCH
          ENDTRY
          SELECT vw_relat
          lcrelob = ALLTRIM(cobject)
          IF ATC('.H', lcrelob)>0
             SELECT vw_langstr
             APPEND BLANK
             REPLACE obj_key WITH lnobkey, row_key WITH "XX_HEADER_XX", cproperty WITH lcrelob, originaltext WITH lclngscr2
          ENDIF
       ENDSCAN
       SELECT vw_relat
       USE
    CATCH
    ENDTRY
 ENDSCAN
 SELECT vw_langstr
 LOCATE FOR  .NOT. EMPTY(langtext)
 IF EOF()
    GOTO TOP
    thisform.grid2.refresh
    MESSAGEBOX("No text was translated for the target language and selected object(s)  , Unable to proceed.", 64)
 ELSE
    GOTO TOP
    thisform.grid2.refresh
    thisform.grid2.column3.header1.caption = ALLTRIM(thisform.combo2.displayvalue)+" Translated Text"
    thisform.cmddest.tag = ALLTRIM(thisform.combo2.value)
    thisform.cmdexport.enabled = .T.
    thisform.cmderrorexport.enabled = .T.
 ENDIF
 WAIT CLEAR
 RETURN
ENDPROC
**
*
*  langdest.scx::Command1
**
PROCEDURE Click
 SELECT sydlangobj
 GOTO TOP
 lc = ALLTRIM(UPPER(thisform.text1.value))
 ln = LEN(lc)
 SCAN
    IF SUBSTR(ALLTRIM(UPPER(cobject)), 1, ln)==lc
       REPLACE sel WITH 1
    ENDIF
 ENDSCAN
 SCAN
    IF SUBSTR(ALLTRIM(UPPER(cobject)), 1, ln)==lc
       EXIT
    ENDIF
 ENDSCAN
 thisform.grid1.refresh
ENDPROC
**
*
*  langdest.scx::CmdErrorExport
**
PROCEDURE Click
 lclangid = ALLTRIM(thisform.cmddest.tag)
 SET DEFAULT TO (ariaapppath)
 IF  .NOT. DIRECTORY(ariaapppath+"Lang\", 1)
    MD (ariaapppath+"Lang\")
 ENDIF
 lclangdir = ariaapppath+"Lang\"+lclangid+"\"
 IF  .NOT. DIRECTORY(lclangdir, 1)
    MD (lclangdir)
 ENDIF
 SELECT syclang
 LOCATE FOR clang_id=lclangid
 lccp = ALLTRIM(ccodepage)
 WAIT WINDOW NOWAIT "Generating Language XML files..."
 thisform.exporttoxls("vw_LangStr", ADDBS(lclangdir)+"LenError.XLS")
 SELECT vw_langstr
 GOTO TOP
 COPY TO (ADDBS(lclangdir)+"LenError.dbf") FOR errlen=1
 SET DEFAULT TO (gccurpath)
 WAIT CLEAR
 MESSAGEBOX("Data successfully exported!", 64)
 RETURN
ENDPROC
**
***
*** langdest.sct
***
*
