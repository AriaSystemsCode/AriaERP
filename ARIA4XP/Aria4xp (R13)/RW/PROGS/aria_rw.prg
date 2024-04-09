*** 
*** ReFox XI+  #NK366474  Tarek  Tarek [VFP60]
***
 PARAMETER lcff_home, lcnosplash
 PRIVATE lcffstalk, lcffssafe, lcffspath, lcffsstatb, lcffsmsgtxt, lnwassel
 IF SET('TALK')='ON'
    SET TALK OFF
    lcffstalk = 'ON'
 ELSE
    lcffstalk = 'OFF'
 ENDIF
 PRIVATE llff_demo
 llff_demo = .F.
 PRIVATE llff_alone
 llff_alone = .F.
 lnwassel = SELECT()
 lcffssafe = SET('SAFETY')
 SET SAFETY OFF
 lcffspath = SET('PATH')
 IF (_WINDOWS .OR. _MAC)
    lcffsstatb = SET("STATUS BAR")
    lcffsmsgtxt = SET("MESSAGE", 1)
    SET STATUS BAR ON
    IF llff_demo
       SET MESSAGE TO SPACE(1)+"Loading Aria Report Writer Demo..."
    ELSE
       SET MESSAGE TO SPACE(1)+"Loading Aria Report Writer... "
    ENDIF
 ELSE
    IF llff_demo
       WAIT WINDOW NOWAIT SPACE(1)+"Loading Aria Report Writer Demo..."
    ELSE
       WAIT WINDOW NOWAIT SPACE(1)+"Loading Aria Report Writer..."
    ENDIF
 ENDIF
 PRIVATE lcwasdir, lctmppref, lcwaserror
 lcwasdir = FULLPATH("")
 IF  .NOT. EMPTY(m.lcff_home)
    SET DEFAULT TO (m.lcff_home)
 ELSE
    lctmppref = ""
    lcwaserror = ON("ERROR")
    ON ERROR *                   
    lctmppref = LOCFILE("FFPREFER.DBF", "DBF", "Where is FFPREFER.DBF")
    ON ERROR &lcwaserror
    IF  .NOT. EMPTY(m.lctmppref)
       SET DEFAULT TO LEFT(m.lctmppref, RAT("\", m.lctmppref))
       lcff_home = SYS(5)+CURDIR()
    ELSE
       WAIT WINDOW NOWAIT "Can't find Aria Report Writer's home directory!"
       DO ffclean
       RETURN
    ENDIF
 ENDIF
 IF  .NOT. env_check()
    RETURN
 ENDIF
 lcariapath = FULLPATH("")
 gcsyshome = LEFT(lcariapath, AT("\", lcariapath))+SUBSTR(lcariapath, AT("\", lcariapath, 1)+1, ATC("\", lcariapath, 2)-AT("\", lcariapath, 1))+"SYSFILES\"
 lcactkey = ''
 IF FILE(gcsyshome+"ACT_KEY.BIN")
    IF  .NOT. lfvact_key(lfopngetvr())
       = MESSAGEBOX('Invalid activation key !', "Aria Report Writer")
       RETURN
    ENDIF
 ELSE
    = MESSAGEBOX('Invalid activation key !', "Aria Report Writer")
    RETURN
 ENDIF
 IF FILE(gcsyshome+"R900INV.FXP")
    RESTORE FROM (gcsyshome+"R900INV.FXP") ADDITIVE
    IF TYPE("ldExpDate")='D'
       IF DATE()>ldexpdate
          MESSAGEBOX('The Demo version has been expired', 0, "Aria Report Writer")
          RETURN
       ENDIF
    ELSE
       MESSAGEBOX('The Demo version has been expired', 0, "Aria Report Writer")
       RETURN
    ENDIF
 ENDIF
 lloginvalue = .F.
 DO FORM login TO lloginvalue
 IF  .NOT. lloginvalue
    RETURN
 ENDIF
 SET PATH TO "DBFS;GOODIES;BMPS"
 PRIVATE lcedition
 lcedition = "Enterprise Edition"
 IF EMPTY(lcnosplash)
    DO FORM ffsplash
 ENDIF
 SELECT (lnwassel)
 CREATE CURSOR ff_batch (request C (8), action C (40), user_id C (15), user_dept C (15), config M, timeout N (4, 0), macrook L, noprtprmpt L, fltstrals M, flttitle M, fltarrays M, fltdisplay M, sendto C (10), outputfile M, dosprtr M, winprtr1 M, winprtr2 M, macprtr M, askatrun M, heading M, ff_filter M, ff_frxname M, ff_sql M, ff_count N (9, 0), system M, error M, prefset C (20), serverfile M, sqltype C (20), use_login L, conhandle N (2, 0), servername M)
 APPEND BLANK
 REPLACE action WITH "REQUESTS"
 REPLACE user_id WITH "System"
 REPLACE config WITH "ffconfig.prg"
 REPLACE use_login WITH .T.
 DO ("FOXFIRE")
 DO ffclean
 RETURN (.T.)
ENDFUNC
**
FUNCTION FFCLEAN
 IF FILE('SAMPLE\CARS.DBF')
    DO ffcarsam WITH "FINISH"
 ENDIF
 IF TYPE('ffsplash.Name')='C'
    ffsplash.release()
 ENDIF
 IF USED("FF_BATCH")
    USE IN ff_batch
 ENDIF
 IF lcffsstatb='OFF'
    SET STATUS BAR OFF
 ELSE
    SET MESSAGE TO lcffsmsgtxt
 ENDIF
 IF m.lcffssafe='ON'
    SET SAFETY ON
 ENDIF
 CLEAR PROGRAM
 SELECT (m.lnwassel)
 SET DEFAULT TO (m.lcwasdir)
 SET PATH TO "&lcFFSpath"
 IF m.lcffstalk='ON'
    SET TALK ON
 ENDIF
 RETURN (.T.)
ENDFUNC
**
FUNCTION ENV_CHECK
 PRIVATE lddosbuild, ldwinbuild, ldmacbuild, ldvfpwbuil
 PRIVATE all_ok, min_files, min_mvcnt
 PRIVATE cur_ver, build_cur, lcwassetdate
 all_ok = .T.
 min_files = 70
 min_mvcnt = 2400
 lcwassetdate = SET("DATE")
 SET DATE AMERICAN
 SET DATE &lcwassetdate
 RELEASE lcwassetdate
 IF  .NOT. _MAC .AND. (VAL(SYS(2010))<m.min_files)
    DEFINE WINDOW nofiles FROM 5, 5 TO 17, 75 SHADOW DOUBLE COLOR SCHEME 7
    ACTIVATE WINDOW nofiles
    PRIVATE laline
    DIMENSION laline[8]
    laline[1] = 'FoxPro reports a FILES setting of '+ALLTRIM(SYS(2010))+'.'
    laline[2] = 'This may be insufficient for Aria Report Writer to run successfully.'
    laline[3] = 'A FILES setting of at least '+ALLTRIM(STR(min_files))+' should ensure correct operation.'
    laline[4] = ''
    laline[5] = 'You can adjust the FILES setting by editing your CONFIG.SYS'
    laline[6] = 'file. Novell network users may also need to edit the file'
    laline[7] = 'SHELL.CFG or NET.CFG.  The new setting(s) will NOT take'
    laline[8] = 'effect until you reboot your computer.'
    PRIVATE lnx
    FOR lnx = 1 TO 8
       @ lnx, 0 SAY PADC(laline(lnx), WCOLS())
    ENDFOR
    WAIT WINDOW
    RELEASE laline
    RELEASE WINDOW nofiles
 ENDIF
 PRIVATE pro_config, pro_handle, mvcnt_ok, mvcnt_set
 mvcnt_ok = .F.
 mvcnt_set = 256
 pro_config = SYS(2019)
 IF  .NOT. EMPTY(pro_config)
    pro_handle = FOPEN(pro_config)
    IF pro_handle>0
       PRIVATE got_count, cur_line
       got_count = .F.
       DO WHILE  .NOT. FEOF(pro_handle)
          cur_line = UPPER(FGETS(pro_handle))
          IF 'MVCO'$cur_line
             mvcnt_set = VAL(SUBSTR(cur_line, AT('=', cur_line)+1))
             IF mvcnt_set>=min_mvcnt
                mvcnt_ok = .T.
                EXIT
             ENDIF
          ENDIF
       ENDDO
       = FCLOSE(pro_handle)
    ENDIF
 ENDIF
 IF  .NOT. mvcnt_ok
    PRIVATE laline
    DIMENSION laline[10]
    laline = ''
    DO CASE
       CASE EMPTY(pro_config)
          laline[1] = 'ERROR -- FoxPro configuration file missing.'
          laline[2] = 'Please consult your FoxPro manual for instructions'
          laline[3] = 'on establishing a CONFIG.FP'+IIF(_WINDOWS, 'W', '')+IIF(_MAC, 'M', '')+' file.'
          laline[4] = ''
          laline[5] = 'NOTE: Aria Report Writer requires a minimum memory variable'
          laline[6] = 'count (MVCOUNT = nnn) setting of '+ALLTRIM(STR(m.min_mvcnt))
       CASE pro_handle<1
          laline[1] = 'ERROR -- unable to open FoxPro configuration file.'
          laline[2] = 'Aria Report Writer requires a minimum memory variable count'
          laline[3] = 'setting (MVCOUNT = nnn) setting of '+ALLTRIM(STR(m.min_mvcnt))
          laline[4] = 'In order to verify the setting on your system,'
          laline[5] = 'Aria Report Writer must be able to read your configuration'
          laline[6] = 'file. According to FoxPro, the configuration file is:'
          laline[7] = ALLTRIM(pro_config)+'.'
          laline[8] = 'Please make this file available, use a different'
          laline[9] = 'configuration file, or disable the code which'
          laline[10] = 'checks this setting (in FFSTART.PRG).'
       OTHERWISE
          laline[1] = 'ERROR -- FoxPro memory variable count setting too low.'
          laline[2] = 'Aria Report Writer requires a minimum memory variable count'
          laline[3] = 'setting (MVCOUNT = nnn) setting of '+ALLTRIM(STR(m.min_mvcnt))
          laline[4] = 'According to FoxPro, your current configuration file'
          laline[5] = 'is: '+ALLTRIM(pro_config)+'.'
          laline[6] = 'Please modify the configuration file to include a'
          laline[7] = 'line reading   "MVCOUNT = '+ALLTRIM(STR(m.min_mvcnt))+'".'
    ENDCASE
    DEFINE WINDOW mvcntprob FROM 5, 5 TO 19, 74 SHADOW COLOR SCHEME 7
    ACTIVATE WINDOW mvcntprob
    @ 2, 0 SAY PADC(laline(1), WCOLS())
    ?? CHR(7)
    PRIVATE lny
    FOR lny = 2 TO 10
       @ 2+lny, 0 SAY PADC(laline(lny), WCOLS())
    ENDFOR
    WAIT WINDOW ' Aria Report Writer execution terminated. Press any key... '
    RELEASE WINDOW mvcntprob
    all_ok = .F.
 ENDIF
 RETURN (all_ok)
ENDFUNC
**
FUNCTION FFBLDCHK
 PARAMETER build_min, build_cur
 PRIVATE ret_val, fox_vers, datestr, daystr, yearstr, monstr
 fox_vers = VERSION(1)
 datestr = SUBSTR(fox_vers, AT('[', fox_vers)+1)
 datestr = SUBSTR(datestr, 1, AT(']', datestr)-10)
 daystr = ALLTRIM(SUBSTR(datestr, AT(' ', datestr)+1, 2))
 yearstr = RIGHT(datestr, 4)
 monstr = UPPER(LEFT(datestr, 3))
 monstr = STR((INT((4+AT(monstr, 'JAN|FEB|MAR|APR|MAY|JUN|JUL|AUG|SEP'+'|OCT|NOV|DEC'))/4)), 2)
 RETURN (ret_val)
ENDFUNC
**
FUNCTION ChkPlatfrm
 PRIVATE lccodebase, lcplatform, llretval
 llretval = .F.
 lccodebase = "WV"
 lcplatform = ""
 IF 'VISUAL'$UPPER(VERSION())
    DO CASE
       CASE _WINDOWS
          lcplatform = "V"
       CASE _MAC
          lcplatform = "X"
    ENDCASE
 ELSE
    DO CASE
       CASE _WINDOWS
          lcplatform = "W"
       CASE _DOS
          lcplatform = "D"
       CASE _MAC
          lcplatform = "M"
    ENDCASE
 ENDIF
 IF EMPTY(lcplatform) .OR.  .NOT. (lcplatform$lccodebase)
    PRIVATE lccodeplat, lnplats, lcthisplat, lcindent
    lcindent = SPACE(6)
    lccodeplat = "This application is built for use on the following platform(s): "
    FOR lnplats = 1 TO LEN(ALLTRIM(lccodebase))
       lcthisplat = SUBSTR(lccodebase, lnplats, 1)
       DO CASE
          CASE lcthisplat="D"
             lccodeplat = lccodeplat+CHR(13)+lcindent+"FoxPro v2.x for DOS"
          CASE lcthisplat="M"
             lccodeplat = lccodeplat+CHR(13)+lcindent+"FoxPro v2.x for Macintosh"
          CASE lcthisplat="W"
             lccodeplat = lccodeplat+CHR(13)+lcindent+"FoxPro v2.x for Windows"
          CASE lcthisplat="V"
             lccodeplat = lccodeplat+CHR(13)+lcindent+"Visual FoxPro v3.0 for Windows"
          CASE lcthisplat="X"
             lccodeplat = lccodeplat+CHR(13)+lcindent+"Visual FoxPro v3.0 for Macintosh"
       ENDCASE
    ENDFOR
    lccodeplat = lccodeplat+CHR(13)+"Your current platform is:"+SPACE(1)+CHR(13)+lcindent+ALLTRIM(VERSION())+IIF(_DOS, " [DOS]", "")
    IF LEN(lccodeplat)>254
       lccodeplat = LEFT(lccodeplat, 254)
    ENDIF
    WAIT WINDOW lccodeplat
 ELSE
    llretval = .T.
 ENDIF
 RETURN (llretval)
ENDFUNC
**
PROCEDURE OnWndDisplay
 RETURN
ENDPROC
**
FUNCTION lfvAct_Key
 PARAMETER lcact_key
 DIMENSION lamodulins[1, 2]
 STORE '' TO lamodulins, lcinsmodules, lcinsplat, lcretmodules, lcplatform
 STORE .F. TO llinstusd, llapplusd
 IF  .NOT. USED('sydappl')
    SELECT 0
    USE (gcsyshome+'sydappl')
    llapplusd = .T.
 ENDIF
 lcact_key = ALLTRIM(lcact_key)
 lckey = SUBSTR(lcact_key, 2)
 lckey = STUFF(lckey, LEN(lckey), 1, '')
 lckey = STRTRAN(lckey, '-')
 lckey = SUBSTR(lckey, 1, LEN(lckey)-1)
 llvaldkey = .F.
 SELECT capp_id, SYS(2007, capp_id) FROM (gcsyshome+'sydappl') WHERE capp_id<>'SY' ORDER BY 2 INTO ARRAY lamodulins
 FOR lncount = 1 TO ALEN(lamodulins, 1)
    lcinsmodules = lcinsmodules+lamodulins(lncount, 1)
    lcretmodules = lcretmodules+IIF(lncount=1, '', ',')+lamodulins(lncount, 1)
 ENDFOR
 lcplatform = RIGHT(lcact_key, 1)
 lnplatform = IIF(ASC(lcplatform)>=65, ASC(lcplatform)-55, VAL(lcplatform))
 lcbinary = ''
 lnprimary = lnplatform
 lcplatform = ''
 DO WHILE lnprimary>0
    lcbinary = STR(MOD(lnprimary, 2), 1)+lcbinary
    lcplatform = lcplatform+IIF(MOD(lnprimary, 2)=1, SUBSTR('DMUW', LEN(lcbinary), 1), '')
    lnprimary = INT(lnprimary/2)
 ENDDO
 lcinsplat = lcplatform
 lcnousers = SUBSTR(lcact_key, 1, 1)+SUBSTR(lcact_key, LEN(lcact_key)-1, 1)
 lnlenact = CEILING((LEN(STRTRAN(lckey, '-')))/2)
 lchidchar = ''
 lcoldact_key = ''
 FOR lncount = 1 TO lnlenact
    lchidchar = lchidchar+SUBSTR(lckey, ((lncount-1)*2)+1, 1)
    lcoldact_key = lcoldact_key+SUBSTR(lckey, ((lncount-1)*2)+2, 1)
 ENDFOR
 FOR lncount = VAL(lcnousers)*10 TO (VAL(lcnousers)*10)+9
    FOR lntrilver = 1 TO 2
       lltrilver = lntrilver=2
       lckeyconted = SYS(2007, PADR(lcinsmodules+lcinsplat+IIF(lltrilver, 'T', '')+STR(lncount), 80))
       lckeyact = ''
       FOR lnhidlen = 1 TO LEN(lchidchar)
          lckeyact = lckeyact+SUBSTR(lchidchar, lnhidlen, 1)+SUBSTR(lckeyconted, lnhidlen, 1)
       ENDFOR
       lckeyact = LEFT(lcnousers, 1)+lckeyact+RIGHT(lcnousers, 1)
       IF lckeyconted==lcoldact_key
          llvaldkey = .T.
          EXIT
       ENDIF
    ENDFOR
    IF llvaldkey=.T.
       EXIT
    ENDIF
 ENDFOR
 IF  .NOT. llvaldkey
    lcretmodules = ''
    lcplatform = ''
    lnnousers = 0
    llalldone = .F.
 ELSE
    lnnousers = lncount
    llalldone = .T.
    lcplatform = lcplatform+IIF(lltrilver, 'T', '')
 ENDIF
 IF USED('SYCINST') .AND. llinstusd
    USE IN sycinst
 ENDIF
 IF USED('SYDAPPL') .AND. llapplusd
    USE IN sydappl
 ENDIF
 RETURN llalldone
ENDFUNC
**
FUNCTION gfSubStr
 PARAMETER lcstring, lnaryorpos, lcsepta
 lcsubstr = ' '
 lnarydim = 1
 lnaryrows = 1
 lnarycols = 1
 lcsepta = IIF(TYPE('lcSepta')='C', lcsepta, ',')
 IF LEN(ALLTRIM(lcsepta))>1
    lccolsep = SUBSTR(lcsepta, 2, 1)
    lcsepta = LEFT(lcsepta, 1)
    lnarydim = IIF(OCCURS(lcsepta, lcstring)>0, OCCURS(lcsepta, lcstring)+IIF(RIGHT(lcstring, 1)<>lcsepta, 1, 0), lnarydim)
    lnarycols = IIF(OCCURS(lccolsep, lcstring)>0, OCCURS(lccolsep, lcstring)+IIF(RIGHT(lcstring, 1)<>lccolsep, 1, 0), lnarydim)
    lnaryrows = (lnarydim+(lnarycols-1))/lnarycols
    lnarydim = lnarydim+(lnarycols-1)
    lcstring = STRTRAN(lcstring, lccolsep, lcsepta)
 ELSE
    lnarydim = IIF(OCCURS(lcsepta, lcstring)>0, OCCURS(lcsepta, lcstring)+IIF(RIGHT(lcstring, 1)<>lcsepta, 1, 0), lnarydim)
 ENDIF
 DO CASE
    CASE TYPE('lnAryOrPos')='U'
       lnaryorpos = 1
    CASE TYPE('lnAryOrPos')$'C,L'
       IF lnarycols>1
          DIMENSION lnaryorpos[lnaryrows, lnarycols]
       ELSE
          IF ALEN(lnaryorpos, 2)>0
             DIMENSION lnaryorpos[lnarydim, ALEN(lnaryorpos, 2)]
          ELSE
             DIMENSION lnaryorpos[lnarydim]
          ENDIF
       ENDIF
       lnaryorpos = ' '
 ENDCASE
 FOR lnarelem = 1 TO lnarydim
    IF TYPE('lnAryOrPos')='N'
       lnarelem = lnaryorpos
    ENDIF
    DO CASE
       CASE lnarelem=1
          lcsubstr = SUBSTR(lcstring, 1, IIF(lcsepta$lcstring, AT(lcsepta, lcstring)-1, LEN(lcstring)))
       CASE lnarelem=lnarydim
          lcsubstr = SUBSTR(lcstring, AT(lcsepta, lcstring, lnarelem-1)+1)
          lcsubstr = IIF(RIGHT(lcsubstr, 1)=lcsepta, SUBSTR(lcsubstr, 1, LEN(lcsubstr)-1), lcsubstr)
       CASE lnarelem>1
          lcsubstr = SUBSTR(lcstring, AT(lcsepta, lcstring, lnarelem-1)+1, AT(lcsepta, lcstring, lnarelem)-AT(lcsepta, lcstring, lnarelem-1)-1)
    ENDCASE
    IF TYPE('lnAryOrPos')='N'
       RETURN lcsubstr
    ENDIF
    IF lnarycols>1
       lnaryorpos[(MOD((lnarelem-1), lnaryrows))+1, INT((lnarelem-1)/lnaryrows)+1] = lcsubstr
    ELSE
       lnaryorpos[lnarelem] = lcsubstr
    ENDIF
 ENDFOR
ENDFUNC
**
FUNCTION lfopngetvr
 PARAMETER lcfilepath, lcfilename
 STORE FOPEN(gcsyshome+"ACT_KEY.BIN") TO file_handle
 lcactkey = ''
 IF file_handle>0
    STORE FSEEK(file_handle, 12876) TO ifp_size
    STORE FREAD(file_handle, 76) TO lcstring
    IF  .NOT. EMPTY(lcstring)
       lccust_id = SUBSTR(lcstring, 03, 05)
       lccust_nam = SUBSTR(lcstring, 08, 030)
       lcactkey = SUBSTR(lcstring, 38, 20)
       ldexpr_date = IIF(EMPTY(SUBSTR(lcstring, 58, 10)), {}, SUBSTR(lcstring, 58, 10))
       lcactplat = SUBSTR(lcstring, 68, 04)
       lcsysactky = SUBSTR(lcstring, 72, 05)
       lcomparsys = SYS(2007, 'NADABDXMLX'+lccust_nam+lcactkey+ldexpr_date+lcactplat)
       = FCLOSE(file_handle)
    ENDIF
 ENDIF
 RETURN lcactkey
ENDFUNC
**
*** 
*** ReFox - all is not lost 
***
