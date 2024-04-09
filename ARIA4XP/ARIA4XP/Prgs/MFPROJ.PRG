*!*****************************************************************************************
*! Name      : mfproj.Prg
*! Developer : Mariam Mazher (MMT)
*! Date      : 05/10/2009
*! Purpose   : Project Screen
*! Entry no. : N037574
*!*****************************************************************************************
*Modifications
*B609028,1 MMT 10/15/2009 Enable user to enter on schedele date While creating Porject from PO,SO,.. Screen[T20080429.0012]
*B609091,1 MMT 11/18/2009 Fix bug of not calling default of Savefiles{T20091117.0011}
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[T20091118.0003]
*E302650,2 MMT 12/24/2009 Modify Project Screen to enable user to create project per line[T20091118.0003]
*E302741,1 WAM 08/25/2010 Convert Audit Trail file into SQL [T20100819.0020]
*B609420,1 MMT 10/05/2010 modify project screen scheduling to work with RB modifications
*B609473,1 MMT 01/02/2011 Error while selecting category activities[T20101104.0001]
*B609473,1 MMT 01/04/2011 Change scheduling to be independent on the request builder[T20101104.0001]
*B609555,1 MMT 03/27/2011 Task List screen gives error while completing task
*B609711,1 MMT 01/05/2012 Fix of Error 'gfOpenFile' is found while add activities[T20110808.0008]
*B609974,1 HIA 06/24/2012 Geeting subscrip error [T20120412.0009]
*!*****************************************************************************************
#include r:\aria4xp\prgs\mfproj.h

*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
*PARAMETERS lcTrPrjType,lcTRPrjID,lcTRPrjSty,lcTempID,llastStr,ldSchedlDate,llShowscreen
PARAMETERS lctrprjtype,lctrprjid,lctrprjsty,lntrlineno,lctempid,llaststr,ldschedldate,llshowscreen
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]

IF TYPE('lcTrPrjType') <> 'C' AND TYPE('lcTRPrjID') <>'C' AND TYPE('lcTRPrjSty') <> 'C'
  llshowscreen = .T.
ENDIF

*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
*!*	IF TYPE('lcTrPrjType') = 'C' AND TYPE('lcTRPrjID') = 'C' AND TYPE('lcTempID') = 'C' AND !llShowscreen
*!*	  DO FORM (oAriaApplication.ScreenHome+ 'MFPROJ.SCX') WITH lcTrPrjType,lcTRPrjID,lcTRPrjSty,lcTempID,llastStr,ldSchedlDate,llShowscreen  NOSHOW
*!*	ELSE
*!*	  DO FORM (oAriaApplication.ScreenHome+ 'MFPROJ.SCX') WITH lcTrPrjType,lcTRPrjID,lcTRPrjSty,lcTempID,llastStr,ldSchedlDate,llShowscreen
*!*	ENDIF
*B609974,1 HIA 06/24/2012 Geeting subscrip error [T20120412.0009] [Begin]
*B609555,1 MMT 03/27/2011 Task List screen gives error while completing task[Start]
IF TYPE('lcTrPrjType') = 'C' AND !EMPTY(lctrprjtype)
  DO CASE
  CASE lctrprjtype $ 'CAD'
    =gfopentable('POSHDR','POSHDR','SH')
    =gfopentable('POSLN','POSLN','SH')
    oariaapplication.activemoduleid = 'MF'
  CASE lctrprjtype $ 'PNR'
    =gfopentable('POSHDR','POSHDR','SH')
    =gfopentable('POSLN','POSLN','SH')
    oariaapplication.activemoduleid = 'PO'
  CASE lctrprjtype $ 'OT'
    =gfopentable('ORDHDR','ORDACCT','SH')
    =gfopentable('ORDLINE','ORDLINE','SH')
    oariaapplication.activemoduleid = 'SO'
  CASE lctrprjtype $ 'M'
    =gfopentable('ITEM','CSTYLE','SH')
    oariaapplication.activemoduleid = 'MA'
  ENDCASE
ENDIF
*B609555,1 MMT 03/27/2011 Task List screen gives error while completing task[END]
*B609974,1 HIA 06/24/2012 Geeting subscrip error [T20120412.0009] [End]

IF TYPE('lcTrPrjType') = 'C' AND TYPE('lcTRPrjID') = 'C' AND TYPE('lcTempID') = 'C' AND !llshowscreen
  DO FORM (oariaapplication.screenhome+ 'MFPROJ.SCX') WITH lctrprjtype,lctrprjid,lctrprjsty,lntrlineno,lctempid,llaststr,ldschedldate,llshowscreen  NOSHOW
ELSE
  *B609974,1 HIA 06/24/2012 Geeting subscrip error [T20120412.0009] [Begin]
  *!*	  *B609555,1 MMT 03/27/2011 Task List screen gives error while completing task[Start]
  *!*	  IF TYPE('lcTrPrjType') = 'C' AND !EMPTY(lcTrPrjType)
  *!*	    DO CASE
  *!*	      CASE lcTrPrjType $ 'CAD'
  *!*	        =gfOpenTable('POSHDR','POSHDR','SH')
  *!*	        =gfOpenTable('POSLN','POSLN','SH')
  *!*	        oAriaApplication.ActiveModuleID = 'MF'
  *!*	      CASE lcTrPrjType $ 'PNR'
  *!*	        =gfOpenTable('POSHDR','POSHDR','SH')
  *!*	        =gfOpenTable('POSLN','POSLN','SH')
  *!*	        oAriaApplication.ActiveModuleID = 'PO'
  *!*	      CASE lcTrPrjType $ 'OT'
  *!*	        =gfOpenTable('ORDHDR','ORDACCT','SH')
  *!*	        =gfOpenTable('ORDLINE','ORDLINE','SH')
  *!*	        oAriaApplication.ActiveModuleID = 'SO'
  *!*	      CASE lcTrPrjType $ 'M'
  *!*	        =gfOpenTable('ITEM','CSTYLE','SH')
  *!*	        oAriaApplication.ActiveModuleID = 'MA'
  *!*	    ENDCASE
  *!*	  ENDIF
  *!*	  *B609555,1 MMT 03/27/2011 Task List screen gives error while completing task[END]
  *!*	  *B609974,1 HIA 06/24/2012 Geeting subscrip error [T20120412.0009] [End]
  DO FORM (oariaapplication.screenhome+ 'MFPROJ.SCX') WITH lctrprjtype,lctrprjid,lctrprjsty,lntrlineno,lctempid,llaststr,ldschedldate,llshowscreen
ENDIF
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
*!*************************************************************
*! Name      : lfOpenFiles
*: Developer : Mariam Mazhar[MMT]
*: Date      : 05/10/2009
*! Purpose   : function to Open Tables used by Screen
*!*************************************************************
FUNCTION lfopenfiles
=gfopentable('PMPRJRL','PMPRJRL','SH')
=gfopentable('STYLE','STYLE','SH')
=gfopentable('PMPRJHD','PMPRJHD','SH')
=gfopentable('PMPTHHD','PMPTHHD','SH')
=gfopentable('PMPTHDT','PMPTHDTS','SH')
=gfopentable('PMPTHRL','PMPTHRL','SH')
=gfopentable('PMPRJDT','PMPRJDTS','SH')
=gfopentable('PMCTGHD','PMCTGHD','SH')
=gfopentable('PMCTGDT','PMCTGDT','SH')
=gfopentable('SYUUSER','Cuser_id','SH')
=gfopentable('SYUGROUP','CGROUP_ID','SH')
=gfopentable('PMPTHNTF','PMPTHNTF','SH')
=gfopentable('PMPRJNTF','PMPRJNTF','SH')
=gfopentable('PMCALDT','PMCALDT','SH')
=gfopentable('PMCALHD','PMCALHD','SH')
=gfopentable('SYSCHDUL','COPRUSR','SH')
=gfopentable('PMCTGRL','PMCTGRL')
=gfopentable('PMPCTGNT','PMPCTGNT')
=gfopentable('AUDTRAIL' , 'AUDTRAIL' , 'SH')
DO CASE
CASE oariaapplication.activemoduleid = 'MF'
  =gfopentable('POSHDR','POSHDR','SH')
  =gfopentable('POSLN','POSLN','SH')

CASE oariaapplication.activemoduleid = 'PO'
  =gfopentable('POSHDR','POSHDR','SH')
  =gfopentable('POSLN','POSLN','SH')

CASE oariaapplication.activemoduleid = 'SO'
  =gfopentable('ORDHDR','ORDACCT','SH')
  =gfopentable('ORDLINE','ORDLINE','SH')

CASE oariaapplication.activemoduleid = 'MA'
  =gfopentable('ITEM','CSTYLE','SH')
ENDCASE
*!*************************************************************
*! Name      : lfvOprtins
*: Developer : Mariam Mazhar[MMT]
*: Date      : 05/10/2009
*! Purpose   : function to get template information
*!*************************************************************
FUNCTION lfvoprtins
PARAMETERS loformset,lcprjtype , lcprojid , lcpathid , lcprjsty


IF TYPE('loFormSet') = 'O'
  lcprjhd = loformset.lcprjhdr
  lc_pmprjdt = loformset.lcprjdt
  lcprj_typ  = &lcprjhd..cprj_typ
  lcprj_id   = &lcprjhd..cprj_id
  lcstyle    = &lcprjhd..cstyle
  m.cpath_id = &lcprjhd..cpath_id
  lc_parser = loformset.lc_parser
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
  loformset.nlinenum = &lcprjhd..LINENO
  lnlineno = &lcprjhd..LINENO
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]

ENDIF
ldcurdate = oariaapplication.systemdate
PRIVATE lcoprt_id, lcoldoprt, lloutcall

IF TYPE('lcPrjType') = 'C' AND TYPE('lcProjID') = 'C' AND TYPE('lcPathID') = 'C'
  lloutcall   = .T.
  lcprj_typ   = lcprjtype
  lcprj_id    = lcprojid
  m.cpath_id  = lcpathid
  lcstyle     = lcprjsty
  m.cprj_stts = 'P'
  lnlatstrt   = 0
  llnewpath   = .T.
  DIMENSION loformset.laopertion[1],loformset.lausers[1]
  SELECT syuuser
  =gfseek('')
  SELECT cuser_id ;
    FROM syuuser ;
    INTO ARRAY loformset.lausers;
    ORDER BY cuser_id
ELSE
  lloutcall   = .F.
ENDIF


IF  loformset.activemode = 'V'
  SELECT pmprjhd
  IF BETWEEN(RECNO(), 1, RECCOUNT())
    GO RECNO()
  ENDIF
  SELECT pmprjdt

  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
  *=gfSeek(lcPrj_Typ + lcPrj_ID +lcStyle)
  =gfseek(lcprj_typ + lcprj_id +lcstyle+ STR(loformset.nlinenum,6))
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
ELSE
  IF &lcprjhd..cprj_stts $ 'CX'
    *Disable All
    SELECT pmprjhd
    IF BETWEEN(RECNO(), 1, RECCOUNT())
      GO RECNO()
    ENDIF
  ELSE
    IF loformset.activemode = 'E'
      SELECT (lc_pmprjdt)
      COUNT FOR !DELETED() TO lnnotdele
      loformset.llnewpath = (lnnotdele = 0)
    ENDIF
    IF loformset.llnewpath
      SELECT pmpthdt
      IF gfseek(&lcprjhd..cpath_id)
        loformset.lnnumoflin = 0
        SCAN REST WHILE cpath_id+coprt_ctg+coprt_id = &lcprjhd..cpath_id
          SCATTER MEMVAR MEMO

          *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
          *!*            INSERT INTO (lc_PMPrjDt);
          *!*                   (cPrj_Typ ,   cPrj_ID  ,  cOprt_ID  ,   cOprt_Dsc,;
          *!*                      nest_dur ,   nrem_dur ,  cOprt_Ctg ,   cOprt_Res,;
          *!*                      lShw2Cust,   mOprt_Com,  cUpdtMthd ,   mNotify  ,;
          *!*                      cAdd_User,   dAdd_Date,  cAdd_Time ,   cStatus  ,;
          *!*                      cStyle   ,   cOprt_Ctg,  cCtg_Seq  ,   cOprt_Seq,;
          *!*                      cCal_ID  ,   cGroup_id,lOrginal,cMComplt,lAddToAud);
          *!*             VALUES(lcPrj_Typ ,    lcPrj_ID  , m.cOprt_ID , m.cOprt_Dsc,;
          *!*                    m.nest_dur , m.nest_dur , m.cOprt_Ctg, m.cOprt_Res,;
          *!*                    m.lShw2Cust, m.mOprt_Com, m.cUpdtMthd, m.mNotify  ,;
          *!*                    oAriaApplication.User_ID, oAriaApplication.SystemDate  , TIME()     , 'A' ,;
          *!*                    lcStyle       , m.cOprt_Ctg, m.cCtg_Seq , m.cOprt_Seq,;
          *!*                    m.cCal_ID  , m.cGroup_id,.T.,'YYY',.T.)
          *B609555,1 MMT 03/27/2011 Task List screen gives error while completing task[Start]
          m.moprt_com = ''
          *B609555,1 MMT 03/27/2011 Task List screen gives error while completing task[END]
          INSERT INTO (lc_pmprjdt);
            (cprj_typ ,   cprj_id  ,  coprt_id  ,   coprt_dsc,;
            nest_dur ,   nrem_dur ,  coprt_ctg ,   coprt_res,;
            lshw2cust,   moprt_com,  cupdtmthd ,   mnotify  ,;
            cadd_user,   dadd_date,  cadd_time ,   cstatus  ,;
            cstyle   ,   coprt_ctg,  cctg_seq  ,   coprt_seq,;
            ccal_id  ,   cgroup_id,lorginal,cmcomplt,laddtoaud,LINENO);
            VALUES(lcprj_typ ,    lcprj_id  , m.coprt_id , m.coprt_dsc,;
            m.nest_dur , m.nest_dur , m.coprt_ctg, m.coprt_res,;
            m.lshw2cust, m.moprt_com, m.cupdtmthd, m.mnotify  ,;
            oariaapplication.user_id, oariaapplication.systemdate  , TIME()     , 'A' ,;
            lcstyle       , m.coprt_ctg, m.cctg_seq , m.coprt_seq,;
            m.ccal_id  , m.cgroup_id,.T.,'YYY',.T.,loformset.nlinenum)
          *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]

          *C201568,1 SAB 04/09/2013 Add new custom field called lShw2PO to the activity grid [T20120704.0021][Start]          
          IF ASCAN(loformset.laEvntTrig,PADR('ENASHW2PO',10),1,ALEN(loformset.laEvntTrig,1),1) > 0
            REPLACE lshw2po WITH m.lshw2po IN (lc_pmprjdt)
          ENDIF
          *C201568,1 SAB 04/09/2013 Add new custom field called lShw2PO to the activity grid [T20120704.0021][End]

          lcoprt_res = IIF(EMPTY(m.coprt_res),m.cgroup_id,m.coprt_res)

          *-- Fill operations array as well.
          *-- Add category code and operation description
          loformset.laopertion[ALEN(loFormSet.laOpertion)] = m.coprt_ctg + '\' + ;
            m.coprt_id  + ' ' + ;
            SUBSTR(m.coprt_dsc, 1, 18)
          DIMENSION  loformset.laopertion[ALEN(loFormSet.laOpertion) + 1]
          loformset.lnnumoflin = loformset.lnnumoflin  + 1

          *-- Append all predecessor lines to the temporary relations
          *-- file.
          SELECT pmpthrl
          *-- Add cOprt_Ctg
          IF gfseek(m.cpath_id + m.coprt_ctg + m.coprt_id)
            SCAN REST WHILE   cpath_id +   coprt_ctg +   coprt_id = ;
                m.cpath_id + m.coprt_ctg + m.coprt_id
              SCATTER MEMVAR MEMO
              *-- Add cOprt_Ctg, cPrd_Ctg

              *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
              *!*                INSERT INTO (loFormSet.lc_PMPrjRl);
              *!*                      (  cPrj_Typ ,   cPrj_ID  ,  cStyle   ,   cOprt_ID ,;
              *!*                         cPrd_ID  ,   nPrd_Lg  ,  cAdd_User,   dAdd_Date,;
              *!*                         cAdd_Time ,   cStatus ,  cOprt_Ctg, cPrd_Ctg);
              *!*                 VALUES( lcPrj_Typ ,   lcPrj_ID, lcStyle   , m.cOprt_ID ,;
              *!*                        m.cPrd_ID  , m.nPrd_Lg , oAriaApplication.User_ID,  ldCurDate ,;
              *!*                           TIME()     , 'A'    ,m.cOprt_Ctg,m.cPrd_Ctg)
              INSERT INTO (loformset.lc_pmprjrl);
                (  cprj_typ ,   cprj_id  ,  cstyle   ,   coprt_id ,;
                cprd_id  ,   nprd_lg  ,  cadd_user,   dadd_date,;
                cadd_time ,   cstatus ,  coprt_ctg, cprd_ctg,LINENO);
                VALUES( lcprj_typ ,   lcprj_id, lcstyle   , m.coprt_id ,;
                m.cprd_id  , m.nprd_lg , oariaapplication.user_id,  ldcurdate ,;
                TIME()     , 'A'    ,m.coprt_ctg,m.cprd_ctg,loformset.nlinenum)
              *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]

            ENDSCAN
          ENDIF

          SELECT pmpthntf
          IF gfseek(m.cpath_id + m.coprt_ctg + m.coprt_id)
            SCAN REST WHILE cpath_id+coprt_ctg+coprt_id+cuser_id = m.cpath_id + m.coprt_ctg + m.coprt_id
              SCATTER MEMVAR
              m.cprj_typ = lcprj_typ
              m.cprj_id  = &lcprjhd..cprj_id
              m.cstyle   = &lcprjhd..cstyle
              m.cstatus  = 'A'
              *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
              m.lineno = loformset.nlinenum
              *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
              INSERT INTO (loformset.lcpmprjntf) FROM MEMVAR
            ENDSCAN
          ENDIF

          SELECT pmpthdt
        ENDSCAN
        IF ALEN(loformset.laopertion) > 1
          DIMENSION loformset.laopertion[ALEN(loFormSet.laOpertion) - 1]
        ENDIF
        lc_pmprjdt = loformset.lcprjdt
        lc_pmprjrl = loformset.lc_pmprjrl
        lc_parser  = loformset.lc_parser
        ACOPY(loformset.laholidays,laholidays)
        =lfgetestdt(@lc_pmprjdt, @lc_pmprjrl,.F.)
      ENDIF
      *-- Collect available categories in an array
      SELECT DISTINCT coprt_ctg ;
        FROM (lc_pmprjdt);
        INTO ARRAY loformset.lacategrie
    ENDIF
    SELECT (lc_pmprjdt)
    GO TOP
    lcpmprjdt = lc_pmprjdt
    loformset.llchngoprt = .F.
  ENDIF

ENDIF
lcactmode = loformset.activemode
lc_pmprjdt = loformset.lcprjdt
lomastrform = loformset
lc_pmprjrl = loformset.lc_pmprjrl
lc_parser  = loformset.lc_parser
SET ORDER TO pmprjdts IN (lc_pmprjdt)

*B609091,1 MMT 11/18/2009 Fix bug of not calling default of Savefiles{Start}
lclastcat  = ''
ldestcmpdt = {}
lclastophaspred = ''
*B609091,1 MMT 11/18/2009 Fix bug of not calling default of Savefiles{End}

IF loformset.llshowscreen
  DO FORM (oariaapplication.screenhome+ 'MFPROJA.SCX') WITH lcactmode,loformset
  *B609028,1 MMT 10/15/2009 Fix bug of Wrong last task in group[Start]
ELSE
  *Check if Last Start date option  is selected to check if user will be asked for
  * on schedule date for ending task of each category
  IF llaststr
    lcbrowtable = loformset.lcprjdt
    SELECT (lcbrowtable)
    lnrecnumber = RECNO()
    SET ORDER TO pmprjdt IN (lc_pmprjdt)
    LOCATE
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
    *!*      SCAN FOR SEEK(&lc_PMPrjDt..cPrj_Typ+ &lc_PMPrjDt..cPrj_ID+;
    *!*                  &lc_PMPrjDt..cStyle+ &lc_PMPrjDt..cOprt_Ctg+;
    *!*                  &lc_PMPrjDt..cOprt_ID,lc_PMPrjRl)
    *!*      IF !SEEK(SUBSTR(&lc_PMPrjDt..cPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(&lc_PMPrjDt..cPrj_ID,1,LEN(cPrj_ID)) +;
    *!*                   SUBSTR(&lc_PMPrjDt..cStyle,1,LEN(cStyle)) + SUBSTR(&lc_PMPrjDt..cOprt_Ctg,1,LEN(cOprt_Ctg)) +;
    *!*                   SUBSTR(&lc_PMPrjDt..cOprt_ID,1,LEN(cOprt_ID)),lc_PMPrjRl,'PMPRJRLP')

    SCAN FOR SEEK(&lc_pmprjdt..cprj_typ+ &lc_pmprjdt..cprj_id+;
        &lc_pmprjdt..cstyle+STR(&lc_pmprjdt..LINENO,6)+ &lc_pmprjdt..coprt_ctg+;
        &lc_pmprjdt..coprt_id,lc_pmprjrl)

      IF !SEEK(SUBSTR(&lc_pmprjdt..cprj_typ,1,LEN(cprj_typ)) + SUBSTR(&lc_pmprjdt..cprj_id,1,LEN(cprj_id)) +;
          SUBSTR(&lc_pmprjdt..cstyle,1,LEN(cstyle))+STR(&lc_pmprjdt..LINENO,6) + SUBSTR(&lc_pmprjdt..coprt_ctg,1,LEN(coprt_ctg)) +;
          SUBSTR(&lc_pmprjdt..coprt_id,1,LEN(coprt_id)),lc_pmprjrl,'PMPRJRLP')
        *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
        lclastcat  = &lc_pmprjdt..coprt_ctg
      ENDIF
    ENDSCAN

    IF BETWEEN(lnrecnumber ,1,RECCOUNT())
      GO RECORD lnrecnumber
    ENDIF
    ldestcmpdt = {}

    SELECT (lc_pmprjdt)
    LOCATE
    SCAN
      ldestcmpdt = {}
      lclastophaspred = ''
      lcthscat = &lc_pmprjdt..coprt_ctg
      lncurrrec = RECNO()
      IF lclastcat <> &lc_pmprjdt..coprt_ctg
        *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
        *!*          SCAN FOR cOprt_Ctg  = lcThsCat AND  SEEK(&lc_PMPrjDt..cPrj_Typ+ &lc_PMPrjDt..cPrj_ID+;
        *!*                       &lc_PMPrjDt..cStyle+ &lc_PMPrjDt..cOprt_Ctg + &lc_PMPrjDt..cOprt_ID,lc_PMPrjRl)
        SCAN FOR coprt_ctg  = lcthscat AND  SEEK(&lc_pmprjdt..cprj_typ+ &lc_pmprjdt..cprj_id+;
            &lc_pmprjdt..cstyle+STR(&lc_pmprjdt..LINENO,6)+ &lc_pmprjdt..coprt_ctg + &lc_pmprjdt..coprt_id,lc_pmprjrl)
          *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
          SELECT (lc_pmprjrl)

          *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
          *!*            IF SEEK(&lc_PMPrjDt..cPrj_Typ+ &lc_PMPrjDt..cPrj_ID+;
          *!*                     &lc_PMPrjDt..cStyle+ &lc_PMPrjDt..cOprt_Ctg+;
          *!*                     &lc_PMPrjDt..cOprt_ID,lc_PMPrjRl,'PMPRJRLP')
          IF SEEK(&lc_pmprjdt..cprj_typ+ &lc_pmprjdt..cprj_id+;
              &lc_pmprjdt..cstyle+STR(&lc_pmprjdt..LINENO,6)+ &lc_pmprjdt..coprt_ctg+;
              &lc_pmprjdt..coprt_id,lc_pmprjrl,'PMPRJRLP')
            *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
            lcrlindex = ORDER()
            SET ORDER TO 'PMPRJRLP'

            *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
            *!*              LOCATE REST WHILE CPRJ_TYP+CPRJ_ID+CSTYLE+CPRD_CTG+CPRD_ID  = &lc_PMPrjDt..cPrj_Typ+ &lc_PMPrjDt..cPrj_ID+;
            *!*                     &lc_PMPrjDt..cStyle+ &lc_PMPrjDt..cOprt_Ctg + &lc_PMPrjDt..cOprt_ID AND COPRT_CTG <>  lcThsCat
            LOCATE REST WHILE cprj_typ+cprj_id+cstyle+STR(LINENO,6)+cprd_ctg+cprd_id  = ;
              &lc_pmprjdt..cprj_typ+ &lc_pmprjdt..cprj_id+;
              &lc_pmprjdt..cstyle +STR(&lc_pmprjdt..LINENO,6)+ &lc_pmprjdt..coprt_ctg + &lc_pmprjdt..coprt_id AND coprt_ctg <>  lcthscat
            *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]


            IF FOUND()
              IF EMPTY(lclastophaspred)
                lclastophaspred = &lc_pmprjdt..coprt_id
                ldestcmpdt = &lc_pmprjdt..dest_fnsh
              ELSE
                IF &lc_pmprjdt..dest_fnsh >= ldestcmpdt
                  lclastophaspred = &lc_pmprjdt..coprt_id
                  ldestcmpdt = &lc_pmprjdt..dest_fnsh
                ENDIF
              ENDIF
            ENDIF
            SET ORDER TO (lcrlindex)
          ELSE
            IF EMPTY(lclastophaspred)
              lclastophaspred = &lc_pmprjdt..coprt_id
              ldestcmpdt = &lc_pmprjdt..dest_fnsh
            ELSE
              IF &lc_pmprjdt..dest_fnsh >= ldestcmpdt
                lclastophaspred = &lc_pmprjdt..coprt_id
                ldestcmpdt = &lc_pmprjdt..dest_fnsh
              ENDIF
            ENDIF
          ENDIF
        ENDSCAN
      ENDIF
      IF BETWEEN(lncurrrec,1,RECCOUNT(lc_pmprjdt))
        GO RECORD lncurrrec IN (lc_pmprjdt)
      ENDIF
      IF lclastcat <> &lc_pmprjdt..coprt_ctg AND  lclastophaspred = &lc_pmprjdt..coprt_id AND !EMPTY(lclastophaspred)
        DO FORM (oariaapplication.screenhome+ 'MFSCHTSK.SCX') WITH &lc_pmprjdt..coprt_dsc,&lc_pmprjdt..dest_fnsh
      ENDIF
    ENDSCAN
    IF BETWEEN(lnrecnumber ,1,RECCOUNT())
      GO RECORD lnrecnumber
    ENDIF
    SELECT (lc_pmprjdt)
    SET ORDER TO pmprjdts IN (lc_pmprjdt)
  ENDIF
  *B609028,1 MMT 10/15/2009 Fix bug of Wrong last task in group[End]
ENDIF
loformset = lomastrform
WITH loformset.ariaform1
  IF lcactmode $ 'EA'
    *IF loFormSet.ariaform1.cboStatus.Value = 'P'
    IF &lcprjhd..cprj_stts = 'P'
      IF loformset.lnnumoflin = 0
        .dtpestend.VALUE = {}
      ELSE
        IF !loformset.llupdstrtd
          lcoldval    = .dtpeststrt.VALUE
          ldoldstrt   = .dtpeststrt.VALUE
          ldoldfnsh   = .dtpestend.VALUE
          *-- ReCalculate estimated dates if any change occurs.
          ACOPY(loformset.laholidays,laholidays)
          =lfgetestdt(@lc_pmprjdt, @lc_pmprjrl,.F.)
          ldoldests = .dtpeststrt.VALUE
          ldoldestf = .dtpestend.VALUE
          llfromoper = .T.
          =lfvlatstrt(loformset)
        ENDIF
      ENDIF
    ELSE
      IF !.chkreschd.VALUE AND loformset.llchngoprt
        .chkreschd.VALUE = loformset.llchngoprt
        llschedual = .chkreschd.VALUE
        REPLACE lschedual WITH loformset.llchngoprt IN (lcprjhd)
      ENDIF
    ENDIF
    =IIF(RECCOUNT(lc_pmprjdt) = 0,.T.,lfchkcomp())
    STORE .F. TO loformset.llchngoprt, loformset.llnewpath
    REPLACE dest_fnsh WITH .dtpestend.VALUE IN (lcprjhd)
  ENDIF
ENDWITH


*!*************************************************************
*! Name      : lfGetEstDt
*: Developer : Mariam Mazhar[MMT]
*: Date      : 05/10/2009
*! Purpose   : function to get estimated date
*!*************************************************************
FUNCTION lfgetestdt
PARAMETERS lcprjdetfl, lcprjrelfl, llfrmacept
PRIVATE lncuralias, lndettag
*-- Save the project start date to a variable so as to be used
*-- ldest_fnsh
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
*!*	IF loFormSet.lnlatStrt =  1
*!*	  ldEst_Fnsh = EVALUATE(loFormSet.lcprjhdr +'.dreq_fnsh')
*!*	ELSE
ldest_fnsh = loformset.ariaform1.dtpeststrt.VALUE
*!*	ENDIF
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
lncuralias = SELECT(0)

*-- Change the tag of Project relations file to predecessors,
SET ORDER TO pmprjrlp IN (lcprjrelfl)

*-- Store the current tag, and change tag to PMPRJDTS
SELECT (lcprjdetfl)
lcdettag = ORDER()
SET ORDER TO TAG pmprjdt

SELECT (lc_parser)
*-- Add category code field

*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
*!*  SET RELATION TO SUBSTR(lcPrj_Typ,1,LEN(&lcPrjDetFl..cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(&lcPrjDetFl..cPrj_ID)) +;
*!*                  SUBSTR(lcStyle,1,LEN(&lcPrjDetFl..cStyle))+ cOprt_Ctg + cOprt_ID INTO (lcPrjDetFl)
SET RELATION TO SUBSTR(lcprj_typ,1,LEN(&lcprjdetfl..cprj_typ)) + SUBSTR(lcprj_id,1,LEN(&lcprjdetfl..cprj_id)) +;
  SUBSTR(lcstyle,1,LEN(&lcprjdetfl..cstyle))+STR(loformset.nlinenum,6)+ coprt_ctg + coprt_id INTO (lcprjdetfl)
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]

SELECT (lcprjdetfl)
SET RELATION OFF INTO (lcprjrelfl)

IF !llfrmacept
  REPLACE ALL dest_strt WITH {},;
    dest_fnsh WITH {},;
    cstatus   WITH SUBSTR('MMA', AT(cstatus, 'SMA'), 1)
ENDIF
SELECT (lcprjdetfl)
SET ORDER TO pmprjdt IN (lcprjdetfl)

*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
*!*  SEEK SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) + SUBSTR(lcStyle,1,LEN(cStyle))
*!*  SCAN REST WHILE cprj_typ+cprj_id+cstyle+coprt_ctg+coprt_id = ;
*!*            SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
*!*            SUBSTR(lcStyle,1,LEN(cStyle)) ;
*!*            FOR EMPTY(dest_strt)
SEEK SUBSTR(lcprj_typ,1,LEN(cprj_typ)) + SUBSTR(lcprj_id,1,LEN(cprj_id)) + SUBSTR(lcstyle,1,LEN(cstyle))+STR(loformset.nlinenum,6)
SCAN REST WHILE cprj_typ+cprj_id+cstyle+STR(LINENO,6)+coprt_ctg+coprt_id = ;
    SUBSTR(lcprj_typ,1,LEN(cprj_typ)) + SUBSTR(lcprj_id,1,LEN(cprj_id)) +;
    SUBSTR(lcstyle,1,LEN(cstyle))+STR(loformset.nlinenum,6) ;
    FOR EMPTY(dest_strt)
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]

  lccuroprt = coprt_ctg + coprt_id


  *-- Update dates, and get the new finish date.
  SELECT (lcprjdetfl)
  ldadjestfn = {}
  *!*	  *MMMMT
  *!*	  IF loFormSet.ariaform1.chkLstStrt.Value AND !ISNULL(DONSCHDL) AND !EMPTY(DONSCHDL  )
  *!*
  *!*	*!*	       REPLACE dest_strt WITH lfAdjDate(loFormSet.ariaform1.dtpEstStrt.value, @ldAdjEstFn,;
  *!*	*!*	                                   MAX(0, nest_dur - 1), cCal_ID, '+'),;
  *!*	*!*	          dest_fnsh WITH ldAdjEstFn
  *!*
  *!*
  *!*	  REPLACE dest_fnsh WITH  lfAdjDate(DONSCHDL  , @ldAdjEstFn,;
  *!*	                   MAX(0, nest_dur - 1),;
  *!*	                   cCal_ID, '-')                   ,;
  *!*	          dest_strt WITH   ldAdjEstFn
  *!*
  *!*	  ELSE
  *!*	  *MMMT
  REPLACE dest_strt WITH lfadjdate(loformset.ariaform1.dtpeststrt.VALUE, @ldadjestfn,;
    MAX(0, nest_dur - 1), ccal_id, '+'),;
    dest_fnsh WITH ldadjestfn
  *!*	  ENDIF
  *!*	  *MMMMT
  ldest_fnsh = dest_fnsh
  lndurindic = MIN(nest_dur,1)

  *-- if the new operation is a predecessor to other operations,
  *-- copy their IDs to the cursor.
  SELECT (lcprjrelfl)
  SET ORDER TO pmprjdt IN (lcprjdetfl)
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
  *!*    IF SEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
  *!*            SUBSTR(lcStyle,1,LEN(cStyle)) + lcCurOprt)
  *!*      SCAN REST WHILE cPrj_Typ +  cPrj_ID + cStyle  +  cPrd_Ctg + cPrd_ID = ;
  *!*                      SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
  *!*                      SUBSTR(lcStyle,1,LEN(cStyle)) + lcCurOprt

  IF SEEK(SUBSTR(lcprj_typ,1,LEN(cprj_typ)) + SUBSTR(lcprj_id,1,LEN(cprj_id)) +;
      SUBSTR(lcstyle,1,LEN(cstyle))+ STR(loformset.nlinenum,6)+ lccuroprt)
    SCAN REST WHILE cprj_typ +  cprj_id + cstyle +STR(LINENO,6) +  cprd_ctg + cprd_id = ;
        SUBSTR(lcprj_typ,1,LEN(cprj_typ)) + SUBSTR(lcprj_id,1,LEN(cprj_id)) +;
        SUBSTR(lcstyle,1,LEN(cstyle)) +STR(loformset.nlinenum,6)+ lccuroprt
      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]

      lcnxtoprt = coprt_ctg + coprt_id
      INSERT INTO (lc_parser);
        VALUES(&lcprjrelfl..coprt_ctg, &lcprjrelfl..coprt_id,;
        ldest_fnsh, lndurindic)

    ENDSCAN
  ENDIF

  SELECT (lc_parser)
  GO TOP
  DO WHILE !EOF()
    lcnxtoprt  = coprt_ctg + coprt_id
    lnrecno    = RECNO()
    ldest_fnsh = dstrtdate
    lndurindic = ndurindic
    SELECT (lcprjdetfl)
    IF dest_strt <= ldest_fnsh
      ldadjestfn = {}
      REPLACE dest_strt WITH lfadjdate( IIF(nest_dur = 0, ldest_fnsh,;
        ldest_fnsh + lndurindic),;
        @ldadjestfn, MAX(0, nest_dur - 1), ccal_id, '+'),;
        dest_fnsh WITH ldadjestfn


    ENDIF

    ldest_fnsh = dest_fnsh
    lndurindic = MAX(lndurindic, MIN(nest_dur,1))
    SELECT (lcprjrelfl)
    SET ORDER TO pmprjdt IN (lcprjdetfl)
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
    *!*      IF SEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
    *!*              SUBSTR(lcStyle,1,LEN(cStyle)) + lcNxtOprt)
    *!*       SCAN REST WHILE  cPrj_Typ +  cPrj_ID +  cStyle + cPrd_Ctg + cPrd_ID = ;
    *!*                       SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
    *!*                       SUBSTR(lcStyle,1,LEN(cStyle)) + lcNxtOprt
    IF SEEK(SUBSTR(lcprj_typ,1,LEN(cprj_typ)) + SUBSTR(lcprj_id,1,LEN(cprj_id)) +;
        SUBSTR(lcstyle,1,LEN(cstyle)) +STR(loformset.nlinenum,6)+ lcnxtoprt)
      SCAN REST WHILE  cprj_typ +  cprj_id +  cstyle+STR(LINENO,6) + cprd_ctg + cprd_id = ;
          SUBSTR(lcprj_typ,1,LEN(cprj_typ)) + SUBSTR(lcprj_id,1,LEN(cprj_id)) +;
          SUBSTR(lcstyle,1,LEN(cstyle)) +STR(loformset.nlinenum,6)+ lcnxtoprt
        *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
        lcnxtonpth = coprt_ctg + coprt_id
        INSERT INTO (lc_parser);
          VALUES(&lcprjrelfl..coprt_ctg, &lcprjrelfl..coprt_id,;
          ldest_fnsh, lndurindic)

      ENDSCAN
    ENDIF
    SELECT (lc_parser)
    IF BETWEEN(lnrecno, 1, RECCOUNT())
      GO lnrecno
    ENDIF

    SKIP
  ENDDO

  SELECT (lc_parser)
  ZAP

  SELECT (lcprjdetfl)
  SET ORDER TO pmprjdt IN (lcprjdetfl)
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
  *!*    SEEK SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
  *!*         SUBSTR(lcStyle,1,LEN(cStyle)) + lcCurOprt
  SEEK SUBSTR(lcprj_typ,1,LEN(cprj_typ)) + SUBSTR(lcprj_id,1,LEN(cprj_id)) +;
    SUBSTR(lcstyle,1,LEN(cstyle)) +STR(loformset.nlinenum,6)+ lccuroprt
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
ENDSCAN

*-- After all operations are scanned, get the project estimated finish date.
*-- The project estimated finish date is the greatest estimated finish
*-- date of all operations/

DECLARE latmpval[1]
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
*!*  SELECT MAX(dest_fnsh) ;
*!*    FROM (lcPrjDetFl)   ;
*!*    WHERE cPrj_Typ +  cPrj_ID +  cStyle + cOprt_Ctg + cOprt_ID = ;
*!*          SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
*!*          SUBSTR(lcStyle,1,LEN(cStyle));
*!*    INTO ARRAY laTmpVal
SELECT MAX(dest_fnsh) ;
  FROM (lcprjdetfl)   ;
  WHERE cprj_typ +  cprj_id +  cstyle +STR(LINENO,6)+ coprt_ctg + coprt_id = ;
  SUBSTR(lcprj_typ,1,LEN(cprj_typ)) + SUBSTR(lcprj_id,1,LEN(cprj_id)) +;
  SUBSTR(lcstyle,1,LEN(cstyle))+STR(loformset.nlinenum,6);
  INTO ARRAY latmpval
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
IF !EMPTY(latmpval[1])
  loformset.ariaform1.dtpestend.VALUE  = latmpval[1]

ENDIF

SET ORDER TO pmprjrl IN (lcprjrelfl)
SELECT (lcprjdetfl)
SET ORDER TO (lcdettag)
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
*!*  SET RELATION TO cPrj_Typ + cPrj_ID + cStyle + cOprt_Ctg + cOprt_ID ;
*!*             INTO (lcPrjRelFl)
SET RELATION TO cprj_typ + cprj_id + cstyle +STR(LINENO,6)+ coprt_ctg + coprt_id ;
  INTO (lcprjrelfl)
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
SELECT (lncuralias)

*!*************************************************************
*! Name      : lfAdjDate
*: Developer : Mariam Mazhar[MMT]
*: Date      : 05/10/2009
*! Purpose   : function to adjust dates
*!*************************************************************
FUNCTION lfadjdate
PARAMETERS ldoprt_str, ldoprt_fsh, lnoprt_dur, lccal_id, lcoperator
PRIVATE lncount, ldoprt_fsh
IF gfseek(lccal_id, 'PMCALHD')
  DO WHILE STR(DOW(ldoprt_str),1) $ pmcalhd.ccal_wend .OR. ;
      ASCAN(laholidays,lccal_id + DTOC(ldoprt_str)) > 0
    ldoprt_str = ldoprt_str &lcoperator 1
  ENDDO

  lncount = 1
  ldoprt_fsh = ldoprt_str
  DO WHILE lncount <= lnoprt_dur
    ldoprt_fsh = ldoprt_fsh &lcoperator 1
    lncount    = lncount ;
      + IIF(STR(DOW(ldoprt_fsh),1) $ pmcalhd.ccal_wend .OR. ;
      ASCAN(laholidays,lccal_id + DTOC(ldoprt_fsh)) > 0 , 0 , 1)
  ENDDO
ELSE
  ldoprt_fsh = ldoprt_str &lcoperator lnoprt_dur
ENDIF
RETURN ldoprt_str


*!*************************************************************
*! Name      : lfvDates
*: Developer : Mariam Mazhar[MMT]
*: Date      : 05/10/2009
*! Purpose   : function to Validate date
*!*************************************************************
FUNCTION lfvdates
PARAMETERS lcsrc,loformset
PRIVATE ldcurobj
WITH loformset.ariaform1
  IF lcsrc = 'S'
    ldcurobj = .dtpreqstrt.VALUE
    ldoldval = .dtpreqstrt.oldvalue
  ELSE
    ldcurobj = .dtpreqend.VALUE
    ldoldval = .dtpreqend.oldvalue
  ENDIF
  IF ldcurobj <> ldoldval
    IF !EMPTY(DTOS(.dtpreqstrt.VALUE )) .AND. ;
        !EMPTY(DTOS(.dtpreqend.VALUE)) .AND. ;
        .dtpreqstrt.VALUE > .dtpreqend.VALUE;
        .AND. gfmodalgen("TRM38219B00000","DIALOG") = 1

      IF lcsrc = 'S'
        .dtpreqstrt.VALUE = ldoldval
      ELSE
        .dtpreqend.VALUE = ldoldval
      ENDIF

    ENDIF
  ENDIF

  IF lcsrc = 'S'
    REPLACE dreq_strt WITH .dtpreqstrt.VALUE IN (loformset.lcprjhdr)
  ELSE
    REPLACE dreq_fnsh WITH .dtpreqend.VALUE IN (loformset.lcprjhdr)
  ENDIF
ENDWITH
*!*************************************************************
*! Name      : lfvpactdat
*: Developer : Mariam Mazhar[MMT]
*: Date      : 05/10/2009
*! Purpose   : function to validate act. date
*!*************************************************************
FUNCTION lfvpactdat
PARAMETERS lcsrc,loformset
PRIVATE ldcurobj
WITH loformset.ariaform1
  IF lcsrc = 'S'
    ldcurobj = .dtpactstrt.VALUE
    ldoldval = .dtpactstrt.oldvalue
  ELSE
    ldcurobj = .dtpactend.VALUE
    ldoldval = .dtpactend.oldvalue
  ENDIF

  IF ldcurobj  <> ldoldval
    IF !EMPTY(DTOS(.dtpactstrt.VALUE)) .AND. ;
        !EMPTY(DTOS(.dtpactend.VALUE)) .AND. ;
        .dtpactstrt.VALUE > .dtpactend.VALUE .AND. ;
        gfmodalgen("TRM38220B00000","DIALOG") = 1
      IF lcsrc = 'S'
        .dtpactstrt.VALUE = .dtpactstrt.oldvalue
      ELSE
        .dtpactend.VALUE =  .dtpactend.oldvalue
      ENDIF
    ENDIF
  ENDIF

  IF lcsrc = 'S'
    REPLACE dact_strt WITH .dtpactstrt.VALUE IN (loformset.lcprjhdr)
  ELSE
    REPLACE dact_fnsh  WITH .dtpactend.VALUE IN (loformset.lcprjhdr)
  ENDIF
ENDWITH
*!*************************************************************
*! Name      : lfInit
*: Developer : Mariam Mazhar[MMT]
*: Date      : 05/10/2009
*! Purpose   : init function
*!*************************************************************
FUNCTION lfinit
PARAMETERS loformset
*-- Transaction type :
WITH loformset
  .lnlatstrt = 0
  .nlinenum = 0
  .lladdtoadt = .F.
  .llchngoprt= .F.
  .lnmajorlen =LEN(gfitemmask("PM"))

  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
  .lcnonmajpic = gfitemmask("MN")
  .lcnonmajpicmat= gfitemmask("MN",'','0002')
  .lnmajorlenmat =LEN(gfitemmask("PM",'','0002'))
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]

  STORE '' TO .latranstype
  DIMENSION .laprojtype [5,6]
  STORE '' TO .laprojtype
  lnrows  = 0
  DIMENSION .lastatus[5]
  .lastatus = ''
  .lastatus[1]   = lang_mfproj_maynotslct
  .lastatus[2]   = lang_mfproj_iscomplete
  .lastatus[3]   = lang_mfproj_iscanclled
  .lastatus[4]   = lang_mfproj_issummer
  .lastatus[5]   = lang_mfproj_isactulized
  .llupdstrtd= .F.
  .llnewpath  = .F.
  .laprojtype [1,1] = lang_mfproj_manacturing
  .laprojtype [1,2] = 'C'
  .laprojtype [1,3] = 'POSHDR'
  .laprojtype [1,4] = 'POSLN'
  .laprojtype [1,5] = ;
    [PO  :H=']+lang_mfproj_cutktktno +['   ,]+;
    [Style   :H=']+lang_mfproj_style+['   ,]+;
    [Status  =IIF(Status='O', ']+lang_mfproj_open+[', ']+lang_mfproj_hold+['):H=']+lang_mfproj_status+[',]+;
    [Entered :H=']+lang_mfproj_issue+['   ,]+;
    [Complete:H=']+lang_mfproj_complete +[',]+;
    [Season  :H=']+lang_mfproj_season+['      ,]+;
    [cDivision:H=']+lang_mfproj_division+['      ,]+;
    [NSTYORDER :H=']+lang_mfproj_budget+[':P='999999',]+;
    [RECEIVE   :H=']+lang_mfproj_recieved+[' :P='999999',]+;
    [DAMAGE    :H=']+lang_mfproj_damaged+[':P='999999',]+;
    [Open :H=']+lang_mfproj_open+['  :P='999999' ]

  .laprojtype [1,6] = 'PO'

  .laprojtype [2,1] = lang_mfproj_stypo
  .laprojtype [2,2] = 'P'
  .laprojtype [2,3] = 'POSHDR'
  .laprojtype [2,4] = 'POSLN'
  .laprojtype [2,5] = ;
    [cStyType:H=']+lang_mfproj_postype+[':8,]+;
    [PO      :H=']+lang_mfproj_pono+[',]+;
    [Status  =IIF(Status='O', ']+lang_mfproj_open+[', ']+lang_mfproj_hold+['):H=']+lang_mfproj_status+[',]+;
    [Vendor   ,]+;
    [Entered  ,]+;
    [Complete ,]+;
    [nstyorder:H=']+lang_mfproj_totqty+[':7,]+;
    [NTOT_COST :H=']+lang_mfproj_amount+[':10,]+;
    [Receive  :R :H=']+lang_mfproj_recvd+[':7,]+;
    [Open     :R :H=']+lang_mfproj_open+[':7]

  .laprojtype [2,6] = 'PO'

  .laprojtype [3,1] = lang_mfproj_salesorder
  .laprojtype [3,2] = 'O'
  .laprojtype [3,3] = 'ORDHDR'
  .laprojtype [3,4] = 'ORDLINE'
  *[cOrdType: H=']+LANG_MFPROJ_ORDERTYPE+[':10,]
  .laprojtype [3,5] = [Order   : H=']+lang_mfproj_orderno+[',]+;
    [Status  = IIF(Status='O', ']+lang_mfproj_open+[', ']+lang_mfproj_hold+['):H=']+lang_mfproj_status+[',]+;
    [Season  : H=']+lang_mfproj_season+[',]+;
    [cDivision: H=']+lang_mfproj_division+[',]+;
    [Account : H=']+lang_mfproj_account+[',]+;
    [Store=IIF(MULTI='Y',']+lang_mfproj_mutilstore+[',STORE):H=']+lang_mfproj_store+[']

  .laprojtype [3,6] = 'ORDER'

  .laprojtype [5,1] = lang_mfproj_material
  .laprojtype [5,2] = 'M'
  .laprojtype [5,3] = 'ITEM'
  .laprojtype [5,4] = ''
  .laprojtype [5,5] =  [cStyMajor : H=']+lang_mfproj_material+[',]+;
    [Desc  : H=']+lang_mfproj_desc+[',]+;
    [Status  = IIF(Status='A', ']+lang_mfproj_active+[',']+ lang_mfproj_onhold+['):H=']+lang_mfproj_status+[',]+;
    [Season  : H=']+lang_mfproj_season+[',]+;
    [cDivision: H=']+lang_mfproj_division+[']

  .laprojtype [5,6] = 'CSTYMAJOR'

  .laprojtype [4,1] = lang_mfproj_proddev
  .laprojtype [4,2] = 'S'
  .laprojtype [4,3] = 'STYLE'
  .laprojtype [4,4] = ''
  .laprojtype [4,5] =  [cStyMajor : H=']+lang_mfproj_style +[',]+;
    [Desc  : H=']+lang_mfproj_desc+[',]+;
    [Status  = IIF(Status='A', ']+lang_mfproj_active+[',']+ lang_mfproj_onhold+['):H=']+lang_mfproj_status+[',]+;
    [Season  : H=']+lang_mfproj_season+[',]+;
    [cDivision: H=']+lang_mfproj_division+[']

  .laprojtype [4,6] = 'CSTYMAJOR'


  STORE '' TO .lacurstatus
  DIMENSION .lacurstatus[5,2]
  .lacurstatus[1,1] = lang_mfproj_palnning
  .lacurstatus[1,2] = 'P'
  .lacurstatus[2,1] = lang_mfproj_inprorgress
  .lacurstatus[2,2] = 'I'
  .lacurstatus[3,1] = lang_mfproj_complete
  .lacurstatus[3,2] = 'C'
  .lacurstatus[4,1] = lang_mfproj_canc
  .lacurstatus[4,2] = 'X'
  .lacurstatus[5,1] = lang_mfproj_historey
  .lacurstatus[5,2] = 'H'


  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
  DIMENSION .lapjfor[3,2]
  STORE '' TO .lapjfor
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
  DO CASE
  CASE oariaapplication.activemoduleid = 'MF'
    DIMENSION .latranstype[3,2]
    .ariaform1.lbltranid.CAPTION = lang_mfproj_ctktktid
    .latranstype[1,1] = lang_mfproj_ctktkt
    .latranstype[1,2] = 'C'
    .latranstype[2,1] = lang_mfproj_adororder
    .latranstype[2,2] = 'A'
    .latranstype[3,1] = lang_mfproj_dyeorder
    .latranstype[3,2] = 'D'
    .ariaform1.cboprojtype.VALUE = 1

    .lapjfor[1,1] = lang_mfproj_ctktkt
    .lapjfor[1,2] = 'O'
    .lapjfor[2,1] = 'Style'
    .lapjfor[2,2] = 'S'
    .lapjfor[3,1] = 'Line'
    .lapjfor[3,2] = 'L'
    .ariaform1.cboprjfor.VALUE = 'O'



  CASE  oariaapplication.activemoduleid = 'PO'
    DIMENSION .latranstype[3,2]
    .ariaform1.lbltranid.CAPTION = lang_mfproj_poid
    .latranstype[1,1] = lang_mfproj_po
    .latranstype[1,2] = 'P'
    .latranstype[2,1] = lang_mfproj_intrpo
    .latranstype[2,2] = 'N'
    .latranstype[3,1] = lang_mfproj_retpo
    .latranstype[3,2] = 'R'
    .ariaform1.cboprojtype.VALUE = 2

    .lapjfor[1,1] = lang_mfproj_po
    .lapjfor[1,2] = 'O'
    .lapjfor[2,1] = 'Style'
    .lapjfor[2,2] = 'S'
    .lapjfor[3,1] = 'Line'
    .lapjfor[3,2] = 'L'
    .ariaform1.cboprjfor.VALUE = 'O'




  CASE oariaapplication.activemoduleid = 'SO'
    DIMENSION .latranstype[2,2]
    .ariaform1.lbltranid.CAPTION = lang_mfproj_salordid

    .latranstype[1,1] = lang_mfproj_salord
    .latranstype[1,2] = 'O'
    .latranstype[2,1] = lang_mfproj_tmpedi
    .latranstype[2,2] = 'T'
    .ariaform1.cboprojtype.VALUE = 3

    .lapjfor[1,1] = lang_mfproj_salord
    .lapjfor[1,2] = 'O'
    .lapjfor[2,1] = 'Style'
    .lapjfor[2,2] = 'S'
    .lapjfor[3,1] = 'Line'
    .lapjfor[3,2] = 'L'
    .ariaform1.cboprjfor.VALUE = 'O'


  CASE oariaapplication.activemoduleid = 'IC'
    DIMENSION .latranstype[2,2]
    .ariaform1.lbltranid.CAPTION =  lang_mfproj_projid
    .latranstype[1,1] = lang_mfproj_style
    .latranstype[1,2] = 'S'
    .latranstype[2,1] = lang_mfproj_other
    .latranstype[2,2] = 'H'
    .ariaform1.cboprojtype.VALUE = 4

  CASE oariaapplication.activemoduleid = 'MA'
    DIMENSION .latranstype[2,2]
    .ariaform1.lbltranid.CAPTION = lang_mfproj_projid
    .latranstype[1,1] = lang_mfproj_material
    .latranstype[1,2] = 'M'
    .latranstype[2,1] = lang_mfproj_other
    .latranstype[2,2] = 'H'
    .ariaform1.cboprojtype.VALUE = 5
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
    *.ariaForm1.lblStyle.Caption = LANG_MFPROJ_MATERIAL+"                   :"
    .ariaform1.lblstyle.CAPTION = lang_mfproj_material+":"
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[END]

  ENDCASE
  .ariaform1.chklststrt.VALUE = .F.
ENDWITH

lfopenfiles()

WITH loformset.ariaform1
  .dtpreqstrt.VALUE = {}
  .dtpeststrt.VALUE = {}
  .dtpcalcstrt.VALUE = {}
  .dtpactstrt.VALUE = {}
  .dtpreqend.VALUE = {}
  .dtpestend.VALUE = {}
  .dtpcalcend.VALUE = {}
  .dtpactend.VALUE = {}
  .dtpschdul.VALUE = {}
  .cbotrantype.REQUERY()
  .cbostatus.REQUERY()
ENDWITH

WITH loformset
  .cbrowsetabledbengine   = 'SQL'
  .nworkarea        = 'PMPRJHD'
  .DATAENVIRONMENT.INITIALSELECTEDALIAS = 'PMPRJHD'
  .cbrowsefilename        = "PMPRJHD"
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
  *!*	  .cBrowseIndexExpression = "CPRJ_TYP+CPRJ_ID+CSTYLE"
  *!*	  .cBrowseIndexFields     = "CPRJ_TYP,CPRJ_ID,CSTYLE"
  .cbrowseindexexpression = "CPRJ_TYP+CPRJ_ID+CSTYLE+STR(LINENO,6)"
  .cbrowseindexfields     = "CPRJ_TYP,CPRJ_ID,CSTYLE,LINENO"
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
  .cbrowseindexname       = "PMPRJHD"
  .cbrowsealiasname       = "PMPRJHD"
  .cbrowsetablename       = "PMPRJHD"
  lcfltrexp = "('"+loformset.ariaform1.cbotrantype.VALUE+"')"
  DO CASE
  CASE oariaapplication.activemoduleid = 'IC'
    lcfltrexp = "('S')"
  CASE oariaapplication.activemoduleid = 'PO'
    lcfltrexp = "('P')"
  CASE oariaapplication.activemoduleid = 'MF'
    lcfltrexp = "('C')"
  CASE oariaapplication.activemoduleid = 'SO'
    lcfltrexp = "('O')"
  ENDCASE

  IF TYPE("loFormSet.lctrprjtype") = 'C'
    lcfltrexp = "('"+loformset.lctrprjtype+"')"
  ENDIF

  .cbrowsefilter          = "cPrj_Typ in "+lcfltrexp +""
  .browsetitle 		  	  ="Project Header"
  .ariabrfields.edtbrowsefields.VALUE = ;
    "cPrj_Typ = '"+loformset.laprojtype [loFormSet.ariaForm1.cboProjType.Value,2]+"':H = '"+lang_mfproj_projtype+"':15,"+;
    "cPrj_ID  : H='"+lang_mfproj_projid+"',"+;
    "cStyle   : H='"+lang_mfproj_style+"',"+;
    "cPrj_SDsc: H='"+lang_mfproj_projdesc+"'"
ENDWITH

WITH loformset
  .ariaform1.cbostatus.VALUE = 'P'
  .lcprjdt = gftempname()
  .lcprjhdr = gftempname()
  .lcpmctgdt = gftempname()
  .lc_pmprjrl = gftempname()
  .lcpmprjntf = gftempname()
  .lc_prjaudt = gftempname()
  .lc_prjhist= gftempname()
  .lc_parser= gftempname()
  .lcbrowcurs= gftempname()
ENDWITH
lfcrttempfls (loformset)

*B609473,1 MMT 01/02/2011 Error while selecting category activities[Start]
lcoldcent=SET("Century")
SET CENTURY ON
*B609473,1 MMT 01/02/2011 Error while selecting category activities[End]

SELECT pmpthhd
gfsetorder('PMPTHHD')
gfseek('')

SELECT pmcaldt
gfseek('')

SELECT pmcaldt
SCAN
  ldcal_hfrm = dcal_hfrm
  DO WHILE ldcal_hfrm <= dcal_hto
    lnrows  = lnrows  + 1
    DIMENSION loformset.laholidays[lnRows]
    loformset.laholidays[lnRows] = ccal_id + DTOC(ldcal_hfrm)
    ldcal_hfrm = ldcal_hfrm + 1
  ENDDO
ENDSCAN

*B609473,1 MMT 01/02/2011 Error while selecting category activities[Start]
SET CENTURY &lcoldcent.
*B609473,1 MMT 01/02/2011 Error while selecting category activities[End]

*!*************************************************************
*! Name      : lfcrttempfls
*: Developer : Mariam Mazhar[MMT]
*: Date      : 05/10/2009
*! Purpose   : function to create temp. files
*!*************************************************************
FUNCTION lfcrttempfls
PARAMETERS loformset
SELECT pmprjhd
DIMENSION lahdrstr[1,18]
lnfllen = AFIELDS(lahdrstr)
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
*=gfCrtTmp(loFormSet.lcprjHdr,@laHdrStr,"CPRJ_TYP+CPRJ_ID+CSTYLE",loFormSet.lcprjHdr,.T.)
=gfcrttmp(loformset.lcprjhdr,@lahdrstr,"CPRJ_TYP+CPRJ_ID+CSTYLE+STR(LINENO,6)",loformset.lcprjhdr,.T.)
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[END]
SELECT(loformset.lcprjhdr)
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
*INDEX on CPRJ_TYP+CPRJ_ID+CSTYLE TAG 'PMPRJHD'
INDEX ON cprj_typ+cprj_id+cstyle+STR(LINENO,6) TAG 'PMPRJHD'
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[END]
SELECT pmprjdt
DIMENSION lafilestru[1,18]
lnfllen = AFIELDS(lafilestru)
lnorgln =lnfllen
DIMENSION lafilestru(lnfllen + 3, 18)
lnfilestru = lnfllen + 1
lafilestru[lnFileStru ,1] = 'cStatus'
lafilestru[lnFileStru ,2] = 'C'
lafilestru[lnFileStru ,3] = 1
lafilestru[lnFileStru ,4] = 0
lnfilestru = lnfilestru + 1
lafilestru[lnFileStru ,1] = 'nRecNo'
lafilestru[lnFileStru ,2] = 'N'
lafilestru[lnFileStru ,3] = 10
lafilestru[lnFileStru ,4] = 0
lnfilestru = lnfilestru + 1
lafilestru[lnFileStru ,1] = 'cMComplt'
lafilestru[lnFileStru ,2] = 'C'
lafilestru[lnFileStru ,3] = 3
lafilestru[lnFileStru ,4] = 0

FOR lncount = 1 TO 3
  STORE '' TO lafilestru[lnOrgLn +lnCount,7],lafilestru[lnOrgLn +lnCount,8],lafilestru[lnOrgLn +lnCount,9],;
    lafilestru[lnOrgLn +lnCount,10],lafilestru[lnOrgLn +lnCount,11],lafilestru[lnOrgLn +lnCount,12],;
    lafilestru[lnOrgLn +lnCount,13],lafilestru[lnOrgLn +lnCount,14],lafilestru[lnOrgLn +lnCount,15],;
    lafilestru[lnOrgLn +lnCount,16]
  STORE 0 TO  lafilestru[lnOrgLn +lnCount,17],lafilestru[lnOrgLn +lnCount,18]
ENDFOR
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
*=gfCrtTmp(loFormSet.lcprjdt ,@laFileStru,"cPrj_Typ+cPrj_Id+cStyle+DTOS(dEst_strt)",loFormSet.lcprjdt ,.T.)
=gfcrttmp(loformset.lcprjdt ,@lafilestru,"cPrj_Typ+cPrj_Id+cStyle+STR(LINENO,6)+DTOS(dEst_strt)",loformset.lcprjdt ,.T.)
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
SELECT (loformset.lcprjdt )
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
*!*  INDEX ON cPrj_Typ + cPrj_ID + cStyle + cOprt_Ctg + cOprt_ID TAG PMPRJDT
*!*  INDEX ON cPrj_Typ + cPrj_ID + cStyle + cCtg_Seq + cOprt_Seq TAG PMPRJDTS
INDEX ON cprj_typ + cprj_id + cstyle + STR(LINENO,6)+ coprt_ctg + coprt_id TAG pmprjdt
INDEX ON cprj_typ + cprj_id + cstyle + STR(LINENO,6)+ cctg_seq + coprt_seq TAG pmprjdts
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
INDEX ON cprj_typ+cprj_id+coprt_ctg+coprt_id+coprt_res      TAG pmprjusr

=gfcrttmp(loformset.lc_prjaudt,@lafilestru,"cOprt_Ctg + cOprt_ID",'PMPRJDT',.T.)

DIMENSION laparsarr[4,4]

laparsarr[1,1] = 'cOprt_Ctg'
laparsarr[1,2] = 'C'
laparsarr[1,3] = 3
laparsarr[1,4] = 0

laparsarr[2,1] = 'cOprt_ID'
laparsarr[2,2] = 'C'
laparsarr[2,3] = 5
laparsarr[2,4] = 0

laparsarr[3,1] = 'dStrtDate'
laparsarr[3,2] = 'D'
laparsarr[3,3] = 8
laparsarr[3,4] = 0

laparsarr[4,1] = 'nDurIndic'
laparsarr[4,2] = 'N'
laparsarr[4,3] = 1
laparsarr[4,4] = 0

=gfcrttmp(loformset.lc_parser,@laparsarr)



DIMENSION lahistfl[6,4]

lahistfl[1,1] = 'cOpr_ID'
lahistfl[1,2] = "C"
lahistfl[1,3] = 20
lahistfl[1,4] =  0

lahistfl[2,1] = 'cUser_ID'
lahistfl[2,2] = "C"
lahistfl[2,3] = 10
lahistfl[2,4] =  0

lahistfl[3,1] = 'cRemain'
lahistfl[3,2] = "C"
lahistfl[3,3] = 3
lahistfl[3,4] =  0

lahistfl[4,1] = 'cCompDate'
lahistfl[4,2] = "C"
lahistfl[4,3] = 8
lahistfl[4,4] =  0

lahistfl[5,1] = 'cActnDate'
lahistfl[5,2] = "C"
lahistfl[5,3] = 8
lahistfl[5,4] =  0

lahistfl[6,1] = 'cStatus'
lahistfl[6,2] = "C"
lahistfl[6,3] = 15
lahistfl[6,4] =  0

=gfcrttmp(loformset.lc_prjhist,@lahistfl,"cOpr_ID",'PMPRJDT',.T.)


SELECT pmctgdt
DIMENSION lapmcatstr[1,18]
lnfllen = AFIELDS(lapmcatstr)
=gfcrttmp(loformset.lcpmctgdt ,@lapmcatstr,"COPRT_CTG+COPRT_ID",loformset.lcpmctgdt ,.T.)

SELECT pmprjntf
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
*DIMENSION laPMPRJNTF[19,4]
DIMENSION lapmprjntf[20,4]
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
lapmprjntf[1,1] = 'CPrj_Typ'
lapmprjntf[1,2] = 'C'
lapmprjntf[1,3] = 1
lapmprjntf[1,4] = 0

lapmprjntf[2,1] = 'CPrj_ID'
lapmprjntf[2,2] = 'C'
lapmprjntf[2,3] = 6
lapmprjntf[2,4] = 0

lapmprjntf[3,1] = 'CStyle'
lapmprjntf[3,2] = 'C'
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
*laPMPRJNTF[3,3] = 12
lapmprjntf[3,3] = 19
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
lapmprjntf[3,4] = 0

lapmprjntf[4,1] = 'COprt_Ctg'
lapmprjntf[4,2] = 'C'
lapmprjntf[4,3] = 3
lapmprjntf[4,4] = 0

lapmprjntf[5,1] = 'COprt_ID'
lapmprjntf[5,2] = 'C'
lapmprjntf[5,3] = 5
lapmprjntf[5,4] = 0

lapmprjntf[6,1] = 'CUser_ID'
lapmprjntf[6,2] = 'C'
lapmprjntf[6,3] = 10
lapmprjntf[6,4] = 0

lapmprjntf[7,1] = 'LBfrStrt'
lapmprjntf[7,2] = 'L'
lapmprjntf[7,3] = 1
lapmprjntf[7,4] = 0

lapmprjntf[8,1] = 'NBfrStrtDy'
lapmprjntf[8,2] = 'N'
lapmprjntf[8,3] = 3
lapmprjntf[8,4] = 0

lapmprjntf[9,1] = 'LBfrCmplt'
lapmprjntf[9,2] = 'L'
lapmprjntf[9,3] = 1
lapmprjntf[9,4] = 0

lapmprjntf[10,1] = 'NBfrCmplDy'
lapmprjntf[10,2] = 'N'
lapmprjntf[10,3] = 3
lapmprjntf[10,4] = 0

lapmprjntf[11,1] = 'LOnStrt'
lapmprjntf[11,2] = 'L'
lapmprjntf[11,3] = 1
lapmprjntf[11,4] = 0

lapmprjntf[12,1] = 'LOnCmplt'
lapmprjntf[12,2] = 'L'
lapmprjntf[12,3] = 1
lapmprjntf[12,4] = 0

lapmprjntf[13,1] = 'LOnRedrct'
lapmprjntf[13,2] = 'L'
lapmprjntf[13,3] = 1
lapmprjntf[13,4] = 0

lapmprjntf[14,1] = 'NStrtDelay'
lapmprjntf[14,2] = 'N'
lapmprjntf[14,3] = 3
lapmprjntf[14,4] = 0

lapmprjntf[15,1] = 'NCmplDelay'
lapmprjntf[15,2] = 'N'
lapmprjntf[15,3] = 3
lapmprjntf[15,4] = 0

lapmprjntf[16,1] = 'CEMAIL_ADD'
lapmprjntf[16,2] = 'C'
lapmprjntf[16,3] = 60
lapmprjntf[16,4] = 0

lapmprjntf[17,1] = 'cStatus'
lapmprjntf[17,2] = 'C'
lapmprjntf[17,3] = 1
lapmprjntf[17,4] = 0

lapmprjntf[18,1] = 'lStrtDelay'
lapmprjntf[18,2] = 'L'
lapmprjntf[18,3] = 1
lapmprjntf[18,4] = 0

lapmprjntf[19,1] = 'lCmplDelay'
lapmprjntf[19,2] = 'L'
lapmprjntf[19,3] = 1
lapmprjntf[19,4] = 0
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
*=gfCrtTmp(loFormSet.lcPMPRJNTF ,@LAPMPRJNTF,"CPrj_Typ+CPrj_ID+CStyle+COprt_Ctg+COprt_ID+CUser_ID",loFormSet.lcPMPRJNTF,.T.)
lapmprjntf[20,1] = 'LINENO'
lapmprjntf[20,2] = 'N'
lapmprjntf[20,3] = 6
lapmprjntf[20,4] = 0

=gfcrttmp(loformset.lcpmprjntf ,@lapmprjntf,"CPrj_Typ+CPrj_ID+CStyle+STR(LINENO,6)+COprt_Ctg+COprt_ID+CUser_ID",loformset.lcpmprjntf,.T.)
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
SELECT pmprjrl
DIMENSION lafilestru[1,18]
lnfllen = AFIELDS(lafilestru)
lnordlen = lnfllen
DIMENSION lafilestru(lnfllen + 2, 18)
lnfllen = lnfllen + 1
lafilestru[lnFlLen ,1] = 'cStatus'
lafilestru[lnFlLen ,2] = 'C'
lafilestru[lnFlLen ,3] = 1
lafilestru[lnFlLen ,4] = 0
lnfllen = lnfllen + 1
lafilestru[lnFlLen ,1] = 'nRecNo'
lafilestru[lnFlLen ,2] = 'N'
lafilestru[lnFlLen ,3] = 10
lafilestru[lnFlLen ,4] = 0

FOR lncount = 1 TO 2
  STORE '' TO lafilestru[lnOrdLen +lnCount,7],lafilestru[lnOrdLen +lnCount,8],lafilestru[lnOrdLen +lnCount,9],;
    lafilestru[lnOrdLen +lnCount,10],lafilestru[lnOrdLen +lnCount,11],lafilestru[lnOrdLen +lnCount,12],;
    lafilestru[lnOrdLen +lnCount,13],lafilestru[lnOrdLen +lnCount,14],lafilestru[lnOrdLen +lnCount,15],;
    lafilestru[lnOrdLen +lnCount,16]
  STORE 0 TO  lafilestru[lnOrdLen +lnCount,17],lafilestru[lnOrdLen +lnCount,18]
ENDFOR
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
*=gfCrtTmp(loFormSet.lc_PMPrjRl ,@laFileStru,"CPRJ_TYP+CPRJ_ID+CSTYLE+COPRT_CTG+COPRT_ID",'PMPRJRL',.T.)
=gfcrttmp(loformset.lc_pmprjrl ,@lafilestru,"CPRJ_TYP+CPRJ_ID+CSTYLE+STR(LINENO,6)+COPRT_CTG+COPRT_ID",'PMPRJRL',.T.)
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
SELECT (loformset.lc_pmprjrl)
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
*!*  INDEX ON cPrj_Typ + cPrj_ID + cStyle + cPrd_Ctg  + cPrd_ID;
*!*          TAG PMPRJRLP
*DIMENSION laStyStruct[2,4]
INDEX ON cprj_typ + cprj_id + cstyle+STR(LINENO,6) + cprd_ctg  + cprd_id;
  TAG pmprjrlp
DIMENSION lastystruct[5,4]
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
lastystruct[1,1] = 'Style'
lastystruct[1,2] = 'C'
lastystruct[1,3] = '19'
lastystruct[1,4] = 0

lastystruct[2,1] = 'Desc'
lastystruct[2,2] = 'C'
lastystruct[2,3] = 30
lastystruct[2,4] = 0

*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
lastystruct[3,1] = 'COMPLETE'
lastystruct[3,2] = 'D'
lastystruct[3,3] = 8
lastystruct[3,4] = 0
lastystruct[4,1] = 'LINENO'
lastystruct[4,2] = 'N'
lastystruct[4,3] = 6
lastystruct[4,4] = 0
lastystruct[5,1] = 'TOTQTY'
lastystruct[5,2] = 'N'
lastystruct[5,3] = 9
lastystruct[5,4] = 0
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]

=gfcrttmp(loformset.lcbrowcurs,@lastystruct,"Style",loformset.lcbrowcurs,.T.)

*!*************************************************************
*! Name      : lfGetClcDt
*: Developer : Mariam Mazhar[MMT]
*: Date      : 05/10/2009
*! Purpose   : function to get calc. dates
*!*************************************************************
FUNCTION lfgetclcdt
PRIVATE lncuralias
lncuralias = SELECT(0)
lctmpprjdt = IIF(llmschdul,lc_pmprjdt,'PMPRJDT')
lctmpprjrl = IIF(llmschdul,lc_pmprjrl,'PMPRJRL')

*-- Change the tag of Project relations file to predecessors,
SET ORDER TO TAG pmprjrlp IN (lctmpprjrl)
lncurtag = VAL(SYS(21))
*-- Reset order tag

SELECT (lctmpprjdt)
SET ORDER TO TAG pmprjdt

IF llfromweb
  lc_parser = lctempname
  DIMENSION laparsarr[4,4]

  laparsarr[1,1] = 'cOprt_Ctg'
  laparsarr[1,2] = 'C'
  laparsarr[1,3] = 3
  laparsarr[1,4] = 0

  laparsarr[2,1] = 'cOprt_ID'
  laparsarr[2,2] = 'C'
  laparsarr[2,3] = 5
  laparsarr[2,4] = 0

  laparsarr[3,1] = 'dStrtDate'
  laparsarr[3,2] = 'D'
  laparsarr[3,3] = 8
  laparsarr[3,4] = 0

  laparsarr[4,1] = 'nDurIndic'
  laparsarr[4,2] = 'N'
  laparsarr[4,3] = 1
  laparsarr[4,4] = 0

  =gfcrttmp(lc_parser,@laparsarr)
ENDIF

SELECT (lc_parser)
*-- Add category code field
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
*!*  SET RELATION TO SUBSTR(lcPrj_Typ,1,LEN(&lcTmpPrjDt..cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(&lcTmpPrjDt..cPrj_ID)) +;
*!*                  SUBSTR(lcStyle,1,LEN(&lcTmpPrjDt..cStyle))+ cOprt_Ctg + cOprt_ID INTO (lcTmpPrjDt)
SET RELATION TO SUBSTR(lcprj_typ,1,LEN(&lctmpprjdt..cprj_typ)) + SUBSTR(lcprj_id,1,LEN(&lctmpprjdt..cprj_id)) +;
  SUBSTR(lcstyle,1,LEN(&lctmpprjdt..cstyle))+STR(loformset.nlinenum,6)+ coprt_ctg + coprt_id INTO (lctmpprjdt)

*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]

*-- Scan all operations of the current project
SELECT (lctmpprjdt)

SET RELATION OFF INTO pmprjrl
*-- Add category code field
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
*!*  SET FILTER TO  cPrj_Typ +  cPrj_ID +  cStyle + cOprt_Ctg + cOprt_ID = ;
*!*                SUBSTR(lcPrj_Typ,1,LEN(&lcTmpPrjRl..cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(&lcTmpPrjRl..cPrj_ID)) +;
*!*                SUBSTR(lcStyle,1,LEN(&lcTmpPrjRl..cStyle));
*!*                .AND. !lVoid
SET FILTER TO  cprj_typ +  cprj_id +  cstyle +STR(LINENO,6)+ coprt_ctg + coprt_id = ;
  SUBSTR(lcprj_typ,1,LEN(&lctmpprjrl..cprj_typ)) + SUBSTR(lcprj_id,1,LEN(&lctmpprjrl..cprj_id)) +;
  SUBSTR(lcstyle,1,LEN(&lctmpprjrl..cstyle))+STR(loformset.nlinenum,6);
  .AND. !lvoid
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]

*--Clear all calculated dates fields
REPLACE ALL dclc_strt WITH {},;
  dclc_fnsh WITH {}

SCAN FOR EMPTY(dclc_strt)
  *-- Save the current operation into a variable.
  lccuroprt = coprt_ctg + coprt_id


  *-- Update dates, and get the new finish date.
  SELECT (lctmpprjdt)

  *-- For the first operation, use the scheduling date as a
  *-- candidate calculated start date
  =lfupdclcdt(dsch_date)
  lldclc_fsh = dclc_fnsh
  lndurindic = MIN(nest_dur,1)

  *-- if the new operation is a predecessor to other operations,
  *-- copy their IDs to the cursor.
  SELECT (lctmpprjrl)


  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
  *!*    IF SEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
  *!*            SUBSTR(lcStyle,1,LEN(cStyle)) + lcCurOprt)
  *!*      SCAN REST WHILE cPrj_Typ +  cPrj_ID + cStyle  +  cPrd_Ctg + cPrd_ID = ;
  *!*                     SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
  *!*                     SUBSTR(lcStyle,1,LEN(cStyle)) + lcCurOprt
  IF SEEK(SUBSTR(lcprj_typ,1,LEN(cprj_typ)) + SUBSTR(lcprj_id,1,LEN(cprj_id)) +;
      SUBSTR(lcstyle,1,LEN(cstyle))+STR(loformset.nlinenum,6)  + lccuroprt)
    SCAN REST WHILE cprj_typ +  cprj_id + cstyle +STR(LINENO,6) +  cprd_ctg + cprd_id = ;
        SUBSTR(lcprj_typ,1,LEN(cprj_typ)) + SUBSTR(lcprj_id,1,LEN(cprj_id)) +;
        SUBSTR(lcstyle,1,LEN(cstyle))+STR(loformset.nlinenum,6)  + lccuroprt
      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
      lcnxtoprt = coprt_ctg + coprt_id
      INSERT INTO (lc_parser);
        VALUES(&lctmpprjrl..coprt_ctg, &lctmpprjrl..coprt_id, lldclc_fsh, lndurindic)
    ENDSCAN
  ENDIF

  SELECT (lc_parser)
  GO TOP
  IF EOF()
    SELECT (lctmpprjdt)
    SET ORDER TO pmprjdt IN (lctmpprjdt)
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
    *!*      =SEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
    *!*          SUBSTR(lcStyle,1,LEN(cStyle)) + lcCurOprt)
    =SEEK(SUBSTR(lcprj_typ,1,LEN(cprj_typ)) + SUBSTR(lcprj_id,1,LEN(cprj_id)) +;
      SUBSTR(lcstyle,1,LEN(cstyle)) +STR(loformset.nlinenum,6)+ lccuroprt)
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]

    IF !EMPTY(&lctmpprjdt..coprt_res) OR !EMPTY(&lctmpprjdt..cgroup_id)
      SELECT syschdul
      SET ORDER TO coprusr
      *B130984,1 HBG 12/04/2006 Add Style code as a new field to SYSCHDUL file and its index [Begin]
      *IF SEEK(SUBSTR(lcPrj_Typ,1,1) + SUBSTR(lcPrj_ID,1,6) + lcCurOprt)
      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
      *IF SEEK(SUBSTR(lcPrj_Typ,1,1) + SUBSTR(lcPrj_ID,1,6) + SUBSTR(lcStyle,1,12) + lcCurOprt)
      IF SEEK(SUBSTR(lcprj_typ,1,1) + SUBSTR(lcprj_id,1,6) + SUBSTR(lcstyle,1,19)+STR(loformset.nlinenum,6) + lccuroprt)
        *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
        *B130984,1 HBG 12/04/2006 [End]
        IF syschdul.coperstat = 'C'
          lcstauts = syschdul.coperstat
          *B130984,1 HBG 12/04/2006 Add Style code as a new field to SYSCHDUL file and its index [Begin]
          *LOCATE REST WHILE cconttype+cseqnumber+ccont_id+coperstat+cuser_id =;
          *            SUBSTR(lcPrj_Typ,1,1) + SUBSTR(lcPrj_ID,1,6) + lcCurOprt;
          *            FOR COPERSTAT <> lcStauts
          *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
          *!*            LOCATE REST WHILE cconttype+cseqnumber+cStyle+ccont_id+coperstat+cuser_id =;
          *!*                        SUBSTR(lcPrj_Typ,1,1) + SUBSTR(lcPrj_ID,1,6) + SUBSTR(lcStyle,1,12) + lcCurOprt;
          *!*                        FOR COPERSTAT <> lcStauts
          LOCATE REST WHILE cconttype+cseqnumber+cstyle+STR(LINENO,6)+ccont_id+coperstat+cuser_id =;
            SUBSTR(lcprj_typ,1,1) + SUBSTR(lcprj_id,1,6) + SUBSTR(lcstyle,1,19) +STR(loformset.nlinenum,6)+ lccuroprt;
            FOR coperstat <> lcstauts
          *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]

        ENDIF
        REPLACE dtrandate  WITH IIF(EMPTY(&lctmpprjdt..dclc_strt),&lctmpprjdt..dest_strt,&lctmpprjdt..dclc_strt),;
          dcmpltdate WITH IIF(EMPTY(&lctmpprjdt..dclc_fnsh),&lctmpprjdt..dest_fnsh,&lctmpprjdt..dclc_fnsh),;
          lpredcomp  WITH .T.
      ENDIF
    ENDIF
  ENDIF
  SELECT (lc_parser)
  DO WHILE !EOF()
    *-- Update the estimated date of the current detail record
    *-- Add category code field
    lcnxtoprt  = coprt_ctg + coprt_id
    lnrecno    = RECNO()

    SELECT (lctmpprjdt)

    =lfupdclcdt(IIF(nest_dur = 0, &lc_parser..dstrtdate,;
      &lc_parser..dstrtdate + &lc_parser..ndurindic))
    lldclc_fsh = dclc_fnsh

    lndurindic = MAX(lndurindic, MIN(nest_dur,1))
    SELECT (lctmpprjrl)
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
    *!*      IF SEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
    *!*              SUBSTR(lcStyle,1,LEN(cStyle)) + lcNxtOprt)
    *!*        SCAN REST WHILE cPrj_Typ + cPrj_ID + cStyle + cPrd_Ctg + cPrd_ID = ;
    *!*                       SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
    *!*                       SUBSTR(lcStyle,1,LEN(cStyle)) + lcNxtOprt
    IF SEEK(SUBSTR(lcprj_typ,1,LEN(cprj_typ)) + SUBSTR(lcprj_id,1,LEN(cprj_id)) +;
        SUBSTR(lcstyle,1,LEN(cstyle)) +STR(loformset.nlinenum,6)+ lcnxtoprt)
      SCAN REST WHILE cprj_typ + cprj_id + cstyle +STR(LINENO,6)+ cprd_ctg + cprd_id = ;
          SUBSTR(lcprj_typ,1,LEN(cprj_typ)) + SUBSTR(lcprj_id,1,LEN(cprj_id)) +;
          SUBSTR(lcstyle,1,LEN(cstyle)) +STR(loformset.nlinenum,6)+ lcnxtoprt
        *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
        lcnxtonpth = coprt_ctg + coprt_id
        INSERT INTO (lc_parser);
          VALUES(&lctmpprjrl..coprt_ctg, &lctmpprjrl..coprt_id, lldclc_fsh, lndurindic)
      ENDSCAN
    ENDIF

    IF !EMPTY(&lctmpprjdt..coprt_res) OR !EMPTY(&lctmpprjdt..cgroup_id)
      SELECT syschdul
      =gfsetorder('Coprusr')
      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
      *IF gfSEEK(SUBSTR(lcPrj_Typ,1,1) + SUBSTR(lcPrj_ID,1,6) + SUBSTR(lcStyle,1,12) + lcNxtOprt)
      IF gfseek(SUBSTR(lcprj_typ,1,1) + SUBSTR(lcprj_id,1,6) + SUBSTR(lcstyle,1,19) +STR(loformset.nlinenum,6)+ lcnxtoprt)
        *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
        IF syschdul.coperstat = 'C'
          lcstauts = syschdul.coperstat
          *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
          *!*            LOCATE REST WHILE cconttype+cseqnumber+cStyle+ccont_id+coperstat+cuser_id =;
          *!*                        SUBSTR(lcPrj_Typ,1,1) + SUBSTR(lcPrj_ID,1,6) + SUBSTR(lcStyle,1,12) + lcNxtOprt ;
          *!*                        FOR COPERSTAT <> lcStauts
          LOCATE REST WHILE cconttype+cseqnumber+cstyle+STR(LINENO,6)+ccont_id+coperstat+cuser_id =;
            SUBSTR(lcprj_typ,1,1) + SUBSTR(lcprj_id,1,6) + SUBSTR(lcstyle,1,19)+ STR(loformset.nlinenum,6)+ lcnxtoprt ;
            FOR coperstat <> lcstauts
          *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
        ENDIF
        REPLACE dtrandate  WITH IIF(EMPTY(&lctmpprjdt..dclc_strt),&lctmpprjdt..dest_strt,&lctmpprjdt..dclc_strt),;
          dcmpltdate WITH IIF(EMPTY(&lctmpprjdt..dclc_fnsh),&lctmpprjdt..dest_fnsh,&lctmpprjdt..dclc_fnsh)
      ENDIF
    ENDIF


    SELECT (lc_parser)
    IF BETWEEN(lnrecno, 1, RECCOUNT())
      GO lnrecno
    ENDIF

    SKIP
  ENDDO

  SELECT (lc_parser)
  ZAP

  SELECT (lctmpprjdt)
  SET ORDER TO pmprjdt IN (lctmpprjdt)
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
  *!*    =SEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
  *!*         SUBSTR(lcStyle,1,LEN(cStyle)) + lcCurOprt)
  =SEEK(SUBSTR(lcprj_typ,1,LEN(cprj_typ)) + SUBSTR(lcprj_id,1,LEN(cprj_id)) +;
    SUBSTR(lcstyle,1,LEN(cstyle)) +STR(loformset.nlinenum,6)+ lccuroprt)
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
ENDSCAN

*-- After all operations are scanned, get the project estimated finish date.
*-- The project estimated finish date is the greatest estimated finish
*-- date of all operations/
DECLARE latmpval[1]
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
*!*  SELECT MIN(dclc_strt), MAX(dclc_fnsh) ;
*!*    FROM (lcTmpPrjDt)   ;
*!*    WHERE cPrj_Typ +  cPrj_ID +  cStyle + cOprt_Ctg + cOprt_ID = ;
*!*                SUBSTR(lcPrj_Typ,1,LEN(&lcTmpPrjDt..cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(&lcTmpPrjDt..cPrj_ID)) +;
*!*                SUBSTR(lcStyle,1,LEN(&lcTmpPrjDt..cStyle)) ;
*!*         .AND. !lVoid;
*!*    INTO ARRAY laTmpVal
SELECT MIN(dclc_strt), MAX(dclc_fnsh) ;
  FROM (lctmpprjdt)   ;
  WHERE cprj_typ +  cprj_id +  cstyle +STR(LINENO,6)+ coprt_ctg + coprt_id = ;
  SUBSTR(lcprj_typ,1,LEN(&lctmpprjdt..cprj_typ)) + SUBSTR(lcprj_id,1,LEN(&lctmpprjdt..cprj_id)) +;
  SUBSTR(lcstyle,1,LEN(&lctmpprjdt..cstyle))+ STR(loformset.nlinenum,6);
  .AND. !lvoid;
  INTO ARRAY latmpval
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
IF !EMPTY(latmpval[1])
  m.dclc_strt = latmpval[1]
  m.dclc_fnsh = latmpval[2]
  IF llfromweb
    SELECT pmprjhd
    REPLACE dclc_strt WITH m.dclc_strt,;
      dclc_fnsh WITH m.dclc_fnsh

    SELECT (lctmpprjdt)
  ENDIF
ENDIF

*--  Restore relations and tags
SET ORDER TO TAG pmprjrl IN (lctmpprjrl)
SELECT (lctmpprjdt)
SET ORDER TO (lncurtag)

SET FILTER TO
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
*!*  SET RELATION TO cPrj_Typ + cPrj_ID + cStyle + cOprt_Ctg + cOprt_ID ;
*!*             INTO (lcTmpPrjRl)
SET RELATION TO cprj_typ + cprj_id + cstyle +STR(LINENO,6)+ coprt_ctg + coprt_id ;
  INTO (lctmpprjrl)
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
SELECT (lncuralias)



*!*************************************************************
*! Name      : lfGetClcDt
*: Developer : Mariam Mazhar[MMT]
*: Date      : 05/10/2009
*! Purpose   : function to add notes
*!*************************************************************
FUNCTION lfoprnote
PARAMETERS loformset
lc_pmprjdt = loformset.lcprjdt
DO FORM (oariaapplication.screenhome+ 'prjnotes.scx') WITH lc_pmprjdt ,'mOprt_Com',loformset.activemode  <> 'V'
REPLACE &lc_pmprjdt..cstatus   WITH SUBSTR('MMA', AT(&lc_pmprjdt..cstatus, 'SMA'), 1) IN (lc_pmprjdt)

*!*************************************************************
*! Name      : lfGetClcDt
*: Developer : Mariam Mazhar[MMT]
*: Date      : 05/10/2009
*! Purpose   : function to Open predecessors mover
*!*************************************************************
FUNCTION lfvpredecsr
PARAMETERS loformset
PRIVATE lapredoprs, lcoldpred, lncuralias
lc_pmprjdt = loformset.lcprjdt
lncuralias = SELECT(0)
*B609711,1 MMT 01/05/2012 Fix of Error 'gfOpenFile' is found while add activities[T20110808.0008][START]
*!*	lcSetProc = SET('PROCEDURE')
*!*	SET PROCEDURE TO
*B609711,1 MMT 01/05/2012 Fix of Error 'gfOpenFile' is found while add activities[T20110808.0008][END]
lcfilename = lc_pmprjdt
lcoprt_ctg =  &lcfilename..coprt_ctg
lcoprt_id = &lcfilename..coprt_id
lcprjtype = &lcfilename..cprj_typ
lcprjid = &lcfilename..cprj_id
lcstyle =&lcfilename..cstyle
lc_pmprjdt = loformset.lcprjdt
=ACOPY(loformset.laopertion, lapredoprs)
lnelement = ASCAN(lapredoprs, lcoprt_ctg + '\' + lcoprt_id)
lc_pmprjrl = loformset.lc_pmprjrl
IF !USED('PMPRJDT_A')
  =gfopentable('PMPRJDT','PMPRJDT','SH','PMPRJDT_A')
ENDIF
IF lnelement > 0
  IF ADEL(lapredoprs, lnelement) > 0
    IF ALEN(lapredoprs) > 1
      DIMENSION lapredoprs[ALEN(laPredOprs)-1]
    ELSE
      DIMENSION lapredoprs[1]
      lapredoprs = ''
    ENDIF
  ENDIF
ENDIF
lchdrfl = loformset.lcprjhdr
IF loformset.activemode = 'V'

  SELECT pmprjdt_a
  =gfsetorder("PMPRJDT")
  SELECT pmprjrl
  =gfsetorder("PMPRJRL")
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
  *!*    IF gfSEEK(SUBSTR( lcPrjType ,1,LEN(PMPRJRL.cPrj_Typ)) + SUBSTR(lcPrjId ,1,LEN(PMPRJRL.cPrj_ID)) +;
  *!*            SUBSTR(lcStyle ,1,LEN(PMPRJRL.cStyle))+SUBSTR(lcOprt_Ctg,1,LEN(PMPRJRL.cOprt_Ctg))+;
  *!*            SUBSTR(lcOprt_ID,1,LEN(PMPRJRL.cOprt_ID)))
  IF gfseek(SUBSTR( lcprjtype ,1,LEN(pmprjrl.cprj_typ)) + SUBSTR(lcprjid ,1,LEN(pmprjrl.cprj_id)) +;
      SUBSTR(lcstyle ,1,LEN(pmprjrl.cstyle))+STR(loformset.nlinenum,6)+SUBSTR(lcoprt_ctg,1,LEN(pmprjrl.coprt_ctg))+;
      SUBSTR(lcoprt_id,1,LEN(pmprjrl.coprt_id)))
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
    lni = 0
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
    *!*      SCAN REST WHILE cPrj_Typ + cPrj_ID + cStyle + cOprt_Ctg + cOprt_ID = ;
    *!*              SUBSTR(lcPrjType ,1,LEN(PMPRJRL.cPrj_Typ)) + SUBSTR(lcPrjId ,1,LEN(PMPRJRL.cPrj_ID)) +;
    *!*            SUBSTR(lcStyle ,1,LEN(PMPRJRL.cStyle))+SUBSTR(lcOprt_Ctg,1,LEN(PMPRJRL.cOprt_Ctg))+;
    *!*            SUBSTR(lcOprt_ID,1,LEN(PMPRJRL.cOprt_ID))
    SCAN REST WHILE cprj_typ + cprj_id + cstyle +STR(LINENO,6)+ coprt_ctg + coprt_id = ;
        SUBSTR(lcprjtype ,1,LEN(pmprjrl.cprj_typ)) + SUBSTR(lcprjid ,1,LEN(pmprjrl.cprj_id)) +;
        SUBSTR(lcstyle ,1,LEN(pmprjrl.cstyle))+STR(loformset.nlinenum,6)+SUBSTR(lcoprt_ctg,1,LEN(pmprjrl.coprt_ctg))+;
        SUBSTR(lcoprt_id,1,LEN(pmprjrl.coprt_id))
      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]

      lni = lni + 1
      DIMENSION lapredecsr[lnI],lapred[lnI]
      lapredecsr[lnI] = pmprjrl.cprd_ctg + '\' + pmprjrl.cprd_id
      lapred[lnI]     = pmprjrl.cprd_ctg + pmprjrl.cprd_id
    ENDSCAN
    FOR lni = 1 TO ALEN(lapred)

      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
      *!*        IF gfSEEK(SUBSTR(lcPrjType ,1,LEN(PMPRJRL.cPrj_Typ)) + SUBSTR(lcPrjId ,1,LEN(PMPRJRL.cPrj_ID)) +;
      *!*                SUBSTR(lcStyle ,1,LEN(PMPRJRL.cStyle))+laPred[lnI],'PMPRJDT_A')
      IF gfseek(SUBSTR(lcprjtype ,1,LEN(pmprjrl.cprj_typ)) + SUBSTR(lcprjid ,1,LEN(pmprjrl.cprj_id)) +;
          SUBSTR(lcstyle ,1,LEN(pmprjrl.cstyle))+STR(loformset.nlinenum,6)+lapred[lnI],'PMPRJDT_A')
        *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]

        lapredecsr[lnI] = lapredecsr[lnI] + ' ' + SUBSTR(pmprjdt_a.coprt_dsc, 1, 18)
      ENDIF
    ENDFOR
  ELSE
    DIMENSION lapredecsr[1]
    lapredecsr = ''
  ENDIF
  SELECT pmprjdt_a
  =gfsetorder('PMPRJDTS')
  =gfmover(@lapredoprs,@lapredecsr,;
    ALLTRIM(lcoprt_ctg) + '\' + ;
    ALLTRIM(lcoprt_id) + ' ' + ALLTRIM(pmprjdt_a.coprt_dsc) +;
    ' Predecessors',.F.,"",.F.,.F.,loformset)

ELSE
  SELECT (lc_pmprjrl)
  SET ORDER TO pmprjrl IN (lc_pmprjrl)
  SET ORDER TO pmprjdt IN (lc_pmprjdt)

  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
  *!*    IF SEEK(SUBSTR(lcPrj_Typ,1,LEN(&lc_PMPrjRl..cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(&lc_PMPrjRl..cPrj_ID)) +;
  *!*            SUBSTR(lcStyle,1,LEN(&lc_PMPrjRl..cStyle))+SUBSTR(lcOprt_Ctg,1,LEN(&lc_PMPrjRl..cOprt_Ctg))+;
  *!*            SUBSTR(lcOprt_ID,1,LEN(&lc_PMPrjRl..cOprt_ID)))
  IF SEEK(SUBSTR(lcprj_typ,1,LEN(&lc_pmprjrl..cprj_typ)) + SUBSTR(lcprj_id,1,LEN(&lc_pmprjrl..cprj_id)) +;
      SUBSTR(lcstyle,1,LEN(&lc_pmprjrl..cstyle))+STR(loformset.nlinenum,6)+SUBSTR(lcoprt_ctg,1,LEN(&lc_pmprjrl..coprt_ctg))+;
      SUBSTR(lcoprt_id,1,LEN(&lc_pmprjrl..coprt_id)))
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]

    lni = 0

    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
    *!*      SCAN REST WHILE cPrj_Typ + cPrj_ID + cStyle + cOprt_Ctg + cOprt_ID = ;
    *!*                SUBSTR(lcPrj_Typ,1,LEN(&lc_PMPrjRl..cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(&lc_PMPrjRl..cPrj_ID)) +;
    *!*                SUBSTR(lcStyle,1,LEN(&lc_PMPrjRl..cStyle))+SUBSTR(lcOprt_Ctg,1,LEN(&lc_PMPrjRl..cOprt_Ctg))+;
    *!*                SUBSTR(lcOprt_ID,1,LEN(&lc_PMPrjRl..cOprt_ID))
    SCAN REST WHILE cprj_typ + cprj_id + cstyle +STR(LINENO,6)+ coprt_ctg + coprt_id = ;
        SUBSTR(lcprj_typ,1,LEN(&lc_pmprjrl..cprj_typ)) + SUBSTR(lcprj_id,1,LEN(&lc_pmprjrl..cprj_id)) +;
        SUBSTR(lcstyle,1,LEN(&lc_pmprjrl..cstyle))+STR(loformset.nlinenum,6)+SUBSTR(lcoprt_ctg,1,LEN(&lc_pmprjrl..coprt_ctg))+;
        SUBSTR(lcoprt_id,1,LEN(&lc_pmprjrl..coprt_id))
      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]

      lni = lni + 1
      DIMENSION lapredecsr[lnI],lapred[lnI]
      lapredecsr[lnI] = &lc_pmprjrl..cprd_ctg + '\' + &lc_pmprjrl..cprd_id
      lapred[lnI]     = &lc_pmprjrl..cprd_ctg + &lc_pmprjrl..cprd_id
    ENDSCAN
    FOR lni = 1 TO ALEN(lapred)

      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
      *!*        IF SEEK(SUBSTR(lcPrj_Typ,1,LEN(&lc_PMPrjRl..cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(&lc_PMPrjRl..cPrj_ID)) +;
      *!*                SUBSTR(lcStyle,1,LEN(&lc_PMPrjRl..cStyle))+laPred[lnI],lc_PMPrjDt)
      IF SEEK(SUBSTR(lcprj_typ,1,LEN(&lc_pmprjrl..cprj_typ)) + SUBSTR(lcprj_id,1,LEN(&lc_pmprjrl..cprj_id)) +;
          SUBSTR(lcstyle,1,LEN(&lc_pmprjrl..cstyle))+STR(loformset.nlinenum,6)+lapred[lnI],lc_pmprjdt)
        *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]

        lapredecsr[lnI] = lapredecsr[lnI] + ' ' + SUBSTR(&lc_pmprjdt..coprt_dsc, 1, 18)
      ENDIF
    ENDFOR
  ELSE
    DIMENSION lapredecsr[1]
    lapredecsr = ''
  ENDIF
  SET ORDER TO pmprjdts IN (lc_pmprjdt)

  DIMENSION lapathelems[1]
  lapathelems = ''
  SELECT (lc_pmprjdt)
  SET RELATION OFF INTO (lc_pmprjrl)

  SELECT (lc_pmprjrl)
  SET ORDER TO TAG pmprjrlp
  lncount = 0

  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
  *!*    IF SEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
  *!*            SUBSTR(lcStyle,1,LEN(cStyle))+SUBSTR(lcOprt_Ctg,1,LEN(cOprt_Ctg))+;
  *!*            SUBSTR(lcOprt_ID,1,LEN(cOprt_ID)))
  *!*      SCAN REST WHILE cPrj_Typ + cPrj_ID + cStyle + cPrd_Ctg + cPrd_ID = ;
  *!*                       SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
  *!*                       SUBSTR(lcStyle,1,LEN(cStyle))+SUBSTR(lcOprt_Ctg,1,LEN(cOprt_Ctg))+;
  *!*                       SUBSTR(lcOprt_ID,1,LEN(cOprt_ID))
  IF SEEK(SUBSTR(lcprj_typ,1,LEN(cprj_typ)) + SUBSTR(lcprj_id,1,LEN(cprj_id)) +;
      SUBSTR(lcstyle,1,LEN(cstyle))+STR(loformset.nlinenum,6)+SUBSTR(lcoprt_ctg,1,LEN(coprt_ctg))+;
      SUBSTR(lcoprt_id,1,LEN(coprt_id)))
    SCAN REST WHILE cprj_typ + cprj_id + cstyle +STR(LINENO,6)+ cprd_ctg + cprd_id = ;
        SUBSTR(lcprj_typ,1,LEN(cprj_typ)) + SUBSTR(lcprj_id,1,LEN(cprj_id)) +;
        SUBSTR(lcstyle,1,LEN(cstyle))+STR(loformset.nlinenum,6)+SUBSTR(lcoprt_ctg,1,LEN(coprt_ctg))+;
        SUBSTR(lcoprt_id,1,LEN(coprt_id))
      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]

      lncount = lncount + 1
      DIMENSION lapathelems[lnCount]
      lapathelems[lnCount] = coprt_ctg +  coprt_id
    ENDSCAN

    lnnxtcount = 0
    DO WHILE lnnxtcount < ALEN(lapathelems)
      lnnxtcount = lnnxtcount + 1
      lcnxtoprt  = lapathelems[lnNxtCount]

      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
      *!*        IF SEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
      *!*                SUBSTR(lcStyle,1,LEN(cStyle))+ lcNxtOprt)
      *!*          SCAN REST WHILE cPrj_Typ +  cPrj_ID +  cStyle +  cPrd_Ctg + cPrd_ID = ;
      *!*                         SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
      *!*                         SUBSTR(lcStyle,1,LEN(cStyle)) + lcNxtOprt
      IF SEEK(SUBSTR(lcprj_typ,1,LEN(cprj_typ)) + SUBSTR(lcprj_id,1,LEN(cprj_id)) +;
          SUBSTR(lcstyle,1,LEN(cstyle))+ STR(loformset.nlinenum,6)+ lcnxtoprt)
        SCAN REST WHILE cprj_typ +  cprj_id +  cstyle +STR(LINENO,6)+  cprd_ctg + cprd_id = ;
            SUBSTR(lcprj_typ,1,LEN(cprj_typ)) + SUBSTR(lcprj_id,1,LEN(cprj_id)) +;
            SUBSTR(lcstyle,1,LEN(cstyle)) + STR(loformset.nlinenum,6)+ lcnxtoprt
          *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]

          DIMENSION lapathelems[ALEN(laPathElems) + 1]
          lapathelems[ALEN(laPathElems)] = coprt_ctg + coprt_id
        ENDSCAN
      ENDIF     &&ENDIF SEEK(lcNxtOprt)
    ENDDO    &&ENDDO WHILE lnNxtCount < ALEN(laPathElems)
  ENDIF     &&ENDIF SEEK(lcOprt_ID)
  lcoldpred = ''
  *  lcVldPrd = 'lfVldPrd(SUBSTR(IIF(lsSource = 0,laSource[1],laSource[lsSource]), 1, 3),;
  SUBSTR(IIF(lsSource = 0,laSource[1],laSource[lsSource]), 5, 5),;
  _CUROBJ = OBJNUM(pbAll))'
  IF loformset.ariaform1.cbostatus.VALUE $ 'PI'
    =gfmover(@lapredoprs,@lapredecsr,;
      'Select Predecessors for '+ ALLTRIM(lcoprt_ctg) + '\' + ;
      ALLTRIM(lcoprt_id) + ' ' + ALLTRIM(&lc_pmprjdt..coprt_dsc),;
      .T.,'lfVldPrd',.T.,.T.,loformset)
    SELECT (lc_pmprjrl)
    SET ORDER TO pmprjrl
    SELECT (lc_pmprjdt)

    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
    *!*      SET RELATION TO cPrj_Typ + cPrj_ID + cStyle + cOprt_Ctg + cOprt_ID;
    *!*                 INTO (lc_PMPrjRl)
    SET RELATION TO cprj_typ + cprj_id + cstyle +STR(LINENO,6)+ coprt_ctg + coprt_id;
      INTO (lc_pmprjrl)
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]

    SELECT (lc_pmprjrl)
    IF !EMPTY(lapredecsr[1])
      lndelelems = ALEN(lapredecsr)
      FOR lncount = 1 TO lndelelems
        lapredecsr[lnCount] = SUBSTR(lapredecsr[lnCount], 1, 3) + ;
          SUBSTR(lapredecsr[lnCount], 5, 5)
      ENDFOR

      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
      *!*        IF SEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
      *!*                SUBSTR(lcStyle,1,LEN(cStyle))+SUBSTR(lcOprt_Ctg,1,LEN(cOprt_Ctg))+;
      *!*                SUBSTR(lcOprt_ID,1,LEN(cOprt_ID)))
      *!*          SCAN REST WHILE cPrj_Typ + cPrj_ID + cStyle + cOprt_Ctg + cOprt_ID = ;
      *!*                       SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
      *!*                       SUBSTR(lcStyle,1,LEN(cStyle))+SUBSTR(lcOprt_Ctg,1,LEN(cOprt_Ctg))+;
      *!*                       SUBSTR(lcOprt_ID,1,LEN(cOprt_ID))
      IF SEEK(SUBSTR(lcprj_typ,1,LEN(cprj_typ)) + SUBSTR(lcprj_id,1,LEN(cprj_id)) +;
          SUBSTR(lcstyle,1,LEN(cstyle))+STR(loformset.nlinenum,6)+SUBSTR(lcoprt_ctg,1,LEN(coprt_ctg))+;
          SUBSTR(lcoprt_id,1,LEN(coprt_id)))
        SCAN REST WHILE cprj_typ + cprj_id + cstyle+STR(LINENO,6) + coprt_ctg + coprt_id = ;
            SUBSTR(lcprj_typ,1,LEN(cprj_typ)) + SUBSTR(lcprj_id,1,LEN(cprj_id)) +;
            SUBSTR(lcstyle,1,LEN(cstyle))+STR(loformset.nlinenum,6)+SUBSTR(lcoprt_ctg,1,LEN(coprt_ctg))+;
            SUBSTR(lcoprt_id,1,LEN(coprt_id))
          *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]

          lnelement  = ASCAN(lapredecsr, cprd_id)
          IF lnelement > 0
            =ADEL(lapredecsr, lnelement)
            lndelelems = lndelelems - 1
          ELSE
            REPLACE cstatus WITH SUBSTR('DDS', AT(cstatus, 'SMA'), 1)
          ENDIF
        ENDSCAN

        *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
        *!*          DELETE FOR cPrj_Typ + cPrj_ID + cStyle + cOprt_Ctg + cOprt_ID = ;
        *!*                       SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
        *!*                       SUBSTR(lcStyle,1,LEN(cStyle))+SUBSTR(lcOprt_Ctg,1,LEN(cOprt_Ctg))+;
        *!*                       SUBSTR(lcOprt_ID,1,LEN(cOprt_ID))
        DELETE FOR cprj_typ + cprj_id + cstyle +STR(LINENO,6)+ coprt_ctg + coprt_id = ;
          SUBSTR(lcprj_typ,1,LEN(cprj_typ)) + SUBSTR(lcprj_id,1,LEN(cprj_id)) +;
          SUBSTR(lcstyle,1,LEN(cstyle))+STR(loformset.nlinenum,6)+SUBSTR(lcoprt_ctg,1,LEN(coprt_ctg))+;
          SUBSTR(lcoprt_id,1,LEN(coprt_id))
        *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]

        FOR lncount = 1 TO lndelelems

          *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
          *!*            INSERT INTO (lc_PMPrjRl);
          *!*                       (cPrj_Typ , cPrj_ID  , cOprt_ID , cPrd_ID,;
          *!*                        cOprt_Ctg,  cPrd_Ctg, ;
          *!*                        cStyle  , cAdd_User, dAdd_Date, cAdd_Time, cStatus);
          *!*                 VALUES(lcPrj_Typ, lcPrj_ID , lcOprt_ID, SUBSTR(laPredecsr[lnCount], 4, 5),;
          *!*                        lcOprt_Ctg, SUBSTR(laPredecsr[lnCount], 1, 3),;
          *!*                        lcStyle , oariaapplication.user_id  , ldCurDate, TIME()   , 'A')
          INSERT INTO (lc_pmprjrl);
            (cprj_typ , cprj_id  , coprt_id , cprd_id,;
            coprt_ctg,  cprd_ctg, ;
            cstyle  , cadd_user, dadd_date, cadd_time, cstatus,LINENO);
            VALUES(lcprj_typ, lcprj_id , lcoprt_id, SUBSTR(lapredecsr[lnCount], 4, 5),;
            lcoprt_ctg, SUBSTR(lapredecsr[lnCount], 1, 3),;
            lcstyle , oariaapplication.user_id  , ldcurdate, TIME()   , 'A',loformset.nlinenum)

          *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
        ENDFOR
      ELSE
        FOR lncount = 1 TO ALEN(lapredecsr)

          *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
          *!*            INSERT INTO (lc_PMPrjRl);
          *!*                       (cPrj_Typ , cPrj_ID  , cOprt_ID , cPrd_ID,;
          *!*                        cOprt_Ctg,  cPrd_Ctg, ;
          *!*                        cStyle  , cAdd_User, dAdd_Date, cAdd_Time, cStatus);
          *!*             VALUES(lcPrj_Typ, lcPrj_ID , lcOprt_ID, SUBSTR(laPredecsr[lnCount], 4, 5),;
          *!*                        lcOprt_Ctg, SUBSTR(laPredecsr[lnCount], 1, 3),;
          *!*                        lcStyle , oariaapplication.user_id  , ldCurDate, TIME()   , 'A')
          INSERT INTO (lc_pmprjrl);
            (cprj_typ , cprj_id  , coprt_id , cprd_id,;
            coprt_ctg,  cprd_ctg, ;
            cstyle  , cadd_user, dadd_date, cadd_time, cstatus,LINENO);
            VALUES(lcprj_typ, lcprj_id , lcoprt_id, SUBSTR(lapredecsr[lnCount], 4, 5),;
            lcoprt_ctg, SUBSTR(lapredecsr[lnCount], 1, 3),;
            lcstyle , oariaapplication.user_id  , ldcurdate, TIME()   , 'A',loformset.nlinenum)
          *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
        ENDFOR
      ENDIF
    ELSE
      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
      *!*        IF SEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
      *!*                SUBSTR(lcStyle,1,LEN(cStyle))+SUBSTR(lcOprt_Ctg,1,LEN(cOprt_Ctg))+;
      *!*                SUBSTR(lcOprt_ID,1,LEN(cOprt_ID)))
      *!*          SCAN REST WHILE cPrj_Typ + cPrj_ID + cStyle + cOprt_Ctg + cOprt_ID = ;
      *!*                       SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
      *!*                       SUBSTR(lcStyle,1,LEN(cStyle))+SUBSTR(lcOprt_Ctg,1,LEN(cOprt_Ctg))+;
      *!*                       SUBSTR(lcOprt_ID,1,LEN(cOprt_ID))
      IF SEEK(SUBSTR(lcprj_typ,1,LEN(cprj_typ)) + SUBSTR(lcprj_id,1,LEN(cprj_id)) +;
          SUBSTR(lcstyle,1,LEN(cstyle))+STR(loformset.nlinenum,6)+SUBSTR(lcoprt_ctg,1,LEN(coprt_ctg))+;
          SUBSTR(lcoprt_id,1,LEN(coprt_id)))
        SCAN REST WHILE cprj_typ + cprj_id + cstyle +STR(LINENO,6)+ coprt_ctg + coprt_id = ;
            SUBSTR(lcprj_typ,1,LEN(cprj_typ)) + SUBSTR(lcprj_id,1,LEN(cprj_id)) +;
            SUBSTR(lcstyle,1,LEN(cstyle))+STR(loformset.nlinenum,6)+SUBSTR(lcoprt_ctg,1,LEN(coprt_ctg))+;
            SUBSTR(lcoprt_id,1,LEN(coprt_id))
          *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
          REPLACE cstatus WITH SUBSTR('DDS', AT(cstatus, 'SMA'), 1)

        ENDSCAN
        *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
        *!*          DELETE FOR cPrj_Typ + cPrj_ID + cStyle + cOprt_Ctg + cOprt_ID = ;
        *!*                       SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
        *!*                       SUBSTR(lcStyle,1,LEN(cStyle))+SUBSTR(lcOprt_Ctg,1,LEN(cOprt_Ctg))+;
        *!*                       SUBSTR(lcOprt_ID,1,LEN(cOprt_ID))
        DELETE FOR cprj_typ + cprj_id + cstyle +STR(LINENO,6)+ coprt_ctg + coprt_id = ;
          SUBSTR(lcprj_typ,1,LEN(cprj_typ)) + SUBSTR(lcprj_id,1,LEN(cprj_id)) +;
          SUBSTR(lcstyle,1,LEN(cstyle))+STR(loformset.nlinenum,6)+SUBSTR(lcoprt_ctg,1,LEN(coprt_ctg))+;
          SUBSTR(lcoprt_id,1,LEN(coprt_id))
        *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
      ENDIF
    ENDIF


    lcpredec   = lfgetpred()
    STORE .T. TO glupdated, loformset.llchngoprt
    loformset.llupdstrtd = .F.
  ELSE
    =gfmover(@lapredoprs,@lapredecsr,;
      ALLTRIM(lcoprt_ctg) + '\' + ;
      ALLTRIM(lcoprt_id) + ' ' + ALLTRIM(&lc_pmprjdt..coprt_dsc) +;
      ' Predecessors',.F.,.F.,.F.,.F.,loformset)
  ENDIF
ENDIF
*B609711,1 MMT 01/05/2012 Fix of Error 'gfOpenFile' is found while add activities[T20110808.0008][START]
*SET PROCEDURE TO &lcSetProc
*B609711,1 MMT 01/05/2012 Fix of Error 'gfOpenFile' is found while add activities[T20110808.0008][END]
SELECT (lncuralias)

*!*************************************************************
*! Name      : lfGetClcDt
*: Developer : Mariam Mazhar[MMT]
*: Date      : 05/10/2009
*! Purpose   : function to enter notes
*!*************************************************************
FUNCTION lfprjnotes
PARAMETERS loformset
lcprjhd = loformset.lcprjhdr

DO FORM (oariaapplication.screenhome+ 'prjnotes.scx') WITH loformset.lcprjhdr,'mprj_desc',loformset.activemode  <> 'V'

*!*************************************************************
*! Name      : lfGetClcDt
*: Developer : Mariam Mazhar[MMT]
*: Date      : 05/10/2009
*! Purpose   : function to Open schedule screen
*!*************************************************************
FUNCTION lfvschedul
PARAMETERS loformset
llschedule = .F.
lc_prjaudt = loformset.lc_prjaudt
lcprjhd = loformset.lcprjhdr
lcprj_id = &lcprjhd..cprj_id
lcprj_typ= &lcprjhd..cprj_typ
lcstyle= &lcprjhd..cstyle
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
lnlineno = loformset.nlinenum
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
dsch_date =  IIF(EMPTY(DTOS(loformset.ariaform1.dtpcalcstrt.VALUE)),loformset.ariaform1.dtpeststrt.VALUE,oariaapplication.systemdate)
DO FORM (oariaapplication.screenhome+ 'mfschd.scx') WITH loformset
lcprj_id = &lcprjhd..cprj_id
lcprj_typ= &lcprjhd..cprj_typ
lcstyle  = &lcprjhd..cstyle
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
lnlineno = &lcprjhd..LINENO
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]

lleditaddscr = .F.
IF llschedule
  *B609473,1 MMT 01/04/2011 Change scheduling to be independent on the request builder[Start]
  *!*	  lcXmlFlName = gfTempName()
  *!*	  lcExpr = gfOpGrid('PMPRJSCH',.T.,"R",.T.,.T.,.T.)
  *1MMT
  *	   loReport = CREATEOBJECT('pmprjsch.pmprjsch')
  *!*	   IF FILE(oAriaApplication.WorkDir+lcXmlFlName+'.XML') AND TYPE("loReport") = 'O'
  *!*	     loReport.Execute(oAriaApplication.ActiveCompanyID,oAriaApplication.WorkDir+lcXmlFlName+'.XML')
  *!*	   ENDIF
  *!*		 DO (oAriaApplication.ApplicationHome+'SM\PMPRJSCH.Fxp') WITH oAriaApplication.ActiveCompanyID,;
  oAriaApplication.OutputHome+lcXmlFlName+'.XML'

  *!*		  RETURN
  lcoldalias = SELECT()
  *!*	  TRY
  *!*		  IF FILE(oAriaApplication.OutputHome+lcXmlFlName+'.XML')
  *!*		    LOCAL loRequest
  *!*		    loRequest = CREATEOBJECT("Aria.DataTypes.RequestHandler.AriaRequest")
  *!*		    loRequest.CompleteNotification = CREATEOBJECT("Aria.DataTypes.Messaging.AriaEmail")
  *!*		    loRequest.ErrorNotification    = CREATEOBJECT("Aria.DataTypes.Messaging.AriaEmail")
  *!*		    loRequest.Context              = CREATEOBJECT("Aria.DataTypes.RequestHandler.AriaRequestContext")
  *!*		    loRequest.Context.CustomerName = "Aria"
  *!*		    loRequest.Context.CompanyName  = IIF(TYPE('oAriaApplication.ActiveCompanyID') = 'C' .AND. !EMPTY(oAriaApplication.ActiveCompanyID), oAriaApplication.ActiveCompanyID, "99")
  *!*		    loRequest.UserName = oAriaApplication.User_ID
  *!*		    lnRemResult = oAriaApplication.remotesystemdata.execute("Select cusr_pass from syuuser WHERE cUser_Id = '" +;
  *!*		    oAriaApplication.User_ID + ;
  *!*		    "'",'',"USERPASS","",oAriaApplication.SystemConnectionString,3,"",SET("Datasession"))
  *!*		    IF lnRemResult > 0
  *!*		      loRequest.Password = USERPASS.cusr_pass
  *!*		    ENDIF
  *!*		    loRequest.SetRequestLoginSettings(0, oAriaApplication.User_ID, cusr_pass)
  *!*		    LOCAL  loAriaArguments
  *!*		    loAriaArguments = CREATEOBJECT("Aria.DataTypes.ObjectDictionary.AriaArgumentList")
  *!*		    LOCAL loOptionGrid
  *!*		    loOptionGrid = CREATEOBJECT("Aria.DataTypes.AriaOptionGridXmlDataSet")
  *!*		    loOptionGrid.FileName =oAriaApplication.OutputHome+lcXmlFlName+'.XML'
  *!*		    loAriaArguments.AddArgument("OptionGrid", loOptionGrid)
  *!*		    loRequest.SetRequestMethodSettings("Aria4XP.ProjectScheduling", "Execute", loAriaArguments)
  *!*		    loRequest.Description = 'Scheduling Project: '+ &lcPrjHd..cprj_id
  *!*	*!*  	    local arrParams[1]
  *!*	*!*  	    arrParams[1] = loRequest
  *!*	*!*  	    LOCAL loWorker
  *!*	*!*  	    loWorker = createobject('VFPMTAPP.Worker.1')
  *!*	*!*  	    loWorker.Run("Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy","AddRequest",@arrParams)
  *!*	*!*  	    loWorker = .NULL.

  *!*		    LOCAL loProxy
  *!*		    loProxy = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy")
  *!*		    *B609420,1 MMT 10/05/2010 modify project screen scheduling to work with RB modifications[Start]
  *!*		    *loProxy.AddRequest(loRequest)
  *!*		    lcClientID = oAriaApplication.Readxml()
  *!*	        loProxy.AddRequest(loRequest,lcClientID)
  *!*		    *B609420,1 MMT 10/05/2010 modify project screen scheduling to work with RB modifications[End]
  *!*		    LOCAL request
  *!*		    *B609420,1 MMT 10/05/2010 modify project screen scheduling to work with RB modifications[Start]
  *!*		   * request = loProxy.GetRequest(loRequest.RequestID)
  *!*		    request = loProxy.GetRequest(loRequest.RequestID,lcClientID)
  *!*		    *B609420,1 MMT 10/05/2010 modify project screen scheduling to work with RB modifications[End]
  *!*		    DO WHILE ISNULL(request)
  *!*		      DECLARE Sleep IN Win32API INTEGER
  *!*		      SLEEP(100)
  *!*	 	     *B609420,1 MMT 10/05/2010 modify project screen scheduling to work with RB modifications[Start]
  *!*	       * request = loProxy.GetRequest(loRequest.RequestID)
  *!*		      request = loProxy.GetRequest(loRequest.RequestID,lcClientID)
  *!*	  	    *B609420,1 MMT 10/05/2010 modify project screen scheduling to work with RB modifications[End]
  *!*		    ENDDO
  *!*		    loOptionGrid= .NULL.
  *!*		    loAriaArguments= .NULL.
  *!*		    loRequest = .NULL.
  *!*		    gxObject = .NULL.
  *!*		    loProxy = NUll
  *!*		    request = Null
  *!*		    STORE '' TO arrParams
  *!*	   	 *B609420,1 MMT 10/05/2010 modify project screen scheduling to work with RB modifications[Start]
  *!*			DECLARE Sleep IN Win32API INTEGER
  *!*			SLEEP(2000)
  *!*			*B609420,1 MMT 10/05/2010 modify project screen scheduling to work with RB modifications[End]
  *!*		  ENDIF
  *!*	  CATCH
  *!*	  ENDTRY
  IF !USED('PMPRJRL_S')
    =gfopentable('PMPRJRL','PMPRJRL','SH','PMPRJRL_S')
  ENDIF
  SELECT 'PMPRJRL_S'
  =gfseek(SUBSTR(lcprj_typ,1,LEN(cprj_typ))+SUBSTR(lcprj_id,1,LEN(cprj_id))+SUBSTR(lcstyle,1,LEN(cstyle))+STR(loformset.nlinenum,6))
  IF !USED('PMPRJDT_S')
    =gfopentable('PMPRJDT','PMPRJDT','SH','PMPRJDT_S')
  ENDIF
  SELECT 'PMPRJDT_S'
  =gfseek(SUBSTR(lcprj_typ,1,LEN(cprj_typ))+SUBSTR(lcprj_id,1,LEN(cprj_id))+SUBSTR(lcstyle,1,LEN(cstyle))+STR(loformset.nlinenum,6))
  IF !USED('PMPRJHD_S')
    =gfopentable('PMPRJHD','PMPRJHD','SH','PMPRJHD_S')
  ENDIF
  SELECT 'PMPRJHD_S'
  =gfseek(SUBSTR(lcprj_typ,1,LEN(cprj_typ))+SUBSTR(lcprj_id,1,LEN(cprj_id))+SUBSTR(lcstyle,1,LEN(cstyle))+STR(loformset.nlinenum,6))

  lleditaddscr = .T.
  lleditadd = .T.
  lctmpprjrl = 'PMPRJRL_S'
  lctmpprjdt = 'PMPRJDT_S'
  lcprjhder = 'PMPRJHD_S'
  SELECT 'PMPRJHD_S'
  =gfreplace("llok_stat WITH .T.")
  =gftableupdate()

  DO lfschedule IN (oariaapplication.applicationhome+'SM\PMPRJSCH.Fxp') WITH;
    lcprj_typ,lcprj_id ,lcstyle  ,lnlineno ,dsch_date

  DIMENSION  latmpval[1]
  latmpval = ''

  SELECT(lctmpprjdt)
  SCAN
    =gfreplace('')
  ENDSCAN
  =gftableupdate()
  SELECT syschdul
  =gftableupdate()
  SELECT(lcprjhder)
  =gfreplace('')
  SELECT MIN(dclc_strt), MAX(dclc_fnsh) ;
    FROM (lctmpprjdt) ;
    WHERE cprj_typ +  cprj_id +  cstyle +STR(LINENO,6)+ coprt_ctg + coprt_id = ;
    SUBSTR(lcprj_typ,1,LEN(&lctmpprjdt..cprj_typ)) + SUBSTR(lcprj_id,1,LEN(&lctmpprjdt..cprj_id)) +;
    SUBSTR(lcstyle,1,LEN(&lctmpprjdt..cstyle)) +STR(lnlineno,6);
    .AND. !lvoid;
    INTO ARRAY latmpval
  IF !EMPTY(latmpval[1])
    m.dclc_strt = latmpval[1]
    m.dclc_fnsh = latmpval[2]
    gfreplace("dclc_strt WITH m.dclc_strt,"+;
      "dclc_fnsh WITH m.dclc_fnsh")
  ENDIF
  SELECT(lcprjhder)
  =gfreplace("llok_stat WITH .F.")
  =gfreplace('')
  =gftableupdate()
  *B609473,1 MMT 01/04/2011 Change scheduling to be independent on the request builder[END]
  SELECT(lcoldalias)
  STORE "" TO lcprog , lckey , lcapobjnam ,lcevent
  =lfupdvar(loformset)
  IF !USED('AUDTRAIL')
    =gfopentable('AUDTRAIL' , 'AUDTRAIL' , 'SH')
  ENDIF
  SELECT pmprjdt
  lctmpdtfl = 'PMPRJDT'
  =gfsetorder('PMPRJDT')
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
  *!*    =gfSEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ))+SUBSTR(lcPrj_ID,1,LEN(cPrj_ID))+SUBSTR(lcStyle,1,LEN(cStyle)))
  *!*    SCAN REST WHILE cprj_typ+cprj_id+cstyle+coprt_ctg+coprt_id = SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ))+;
  *!*                    SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) + SUBSTR(lcStyle,1,LEN(cStyle));
  *!*               FOR lSchAdToAd
  =gfseek(SUBSTR(lcprj_typ,1,LEN(cprj_typ))+SUBSTR(lcprj_id,1,LEN(cprj_id))+SUBSTR(lcstyle,1,LEN(cstyle))+STR(loformset.nlinenum,6))
  SCAN REST WHILE cprj_typ+cprj_id+cstyle+STR(LINENO,6)+coprt_ctg+coprt_id = SUBSTR(lcprj_typ,1,LEN(cprj_typ))+;
      SUBSTR(lcprj_id,1,LEN(cprj_id)) + SUBSTR(lcstyle,1,LEN(cstyle))+STR(loformset.nlinenum,6);
      FOR lschadtoad
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
    =lfprpaudt()
    SELECT pmprjdt
    gfreplace("lSchAdToAd WITH .F.")
  ENDSCAN
ENDIF
SELECT pmprjdt
=gftableupdate()
loformset.changemode('V')


*!*************************************************************
*! Name      : lfGetClcDt
*: Developer : Mariam Mazhar[MMT]
*: Date      : 05/10/2009
*! Purpose   : function of OK button of schedule screen
*!*************************************************************
FUNCTION lfvok
PARAMETERS lobrnformset
IF EMPTY(DTOS(lobrnformset.ariaform1.dtpschd.VALUE))
  =gfmodalgen("TRM38224B00000","DIALOG")
  lobrnformset.ariaform1.dtpschd.SETFOCUS
  RETURN .F.
ELSE
  dsch_date = lobrnformset.ariaform1.dtpschd.VALUE
  llschedule = .T.
ENDIF
lobrnformset.RELEASE()

*!*************************************************************
*! Name      : lfUpdVar
*: Developer : Mariam Mazhar[MMT]
*: Date      : 05/10/2009
*! Purpose   : Update variable for audit trail
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfUpdVar()
*!*************************************************************
FUNCTION lfupdvar
PARAMETERS loformset

lcapobjnam = 'MFPROJ'
*-- Update Audit Trail
DO CASE
CASE oariaapplication.activemoduleid = 'MF'
  lcprog = 'MFCUTKT'
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
  *lcKey  = lcPrj_ID
  lckey = lcprj_typ + lcprj_id + lcstyle +STR(lnlineno,6)
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
  lcevent = 'RESCHDCT'
  DO CASE
  CASE lcprj_typ = 'A'      && Adorment Order
    lcprog= 'MFADPO'
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
    *lcKey = lcPrj_Typ + lcPrj_ID
    lckey = lcprj_typ + lcprj_id + lcstyle +STR(lnlineno,6)
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
    lcevent = 'RESCHDPA'
  CASE lcprj_typ = 'D'      && Dye Order
    lcprog = 'MFDPO'
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
    *lcKey = lcPrj_Typ + lcPrj_ID
    lckey = lcprj_typ + lcprj_id+ lcstyle +STR(lnlineno,6)
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
    lcevent = 'RESCHDPD'
  ENDCASE
CASE oariaapplication.activemoduleid = 'PO'
  DO CASE
  CASE lcprj_typ = 'P'      && PO
    lcprog = 'POSTY'
    lcevent = 'RESCHDPO'
  CASE lcprj_typ = 'N'      && Inter-Location PO
    lcprog = 'POINTRC'
    lcevent = 'RESCHDPN'
  CASE lcprj_typ = 'R'      && Return PO
    lcprog = 'RETPO'
    lcevent = 'RESCHDPR'
  ENDCASE
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
  *lcKey = lcPrj_Typ + lcPrj_ID
  lckey = lcprj_typ + lcprj_id+ lcstyle +STR(lnlineno,6)
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]

CASE oariaapplication.activemoduleid = 'SO'
  DO CASE
  CASE lcprj_typ = 'O'      && SO
    lcprog = 'SOORD'
    lcevent = 'RESCHDSO'
  CASE lcprj_typ = 'T'      && EDI Order
    lcprog = 'SOEDORD'
    lcevent = 'RESCHDST'
  ENDCASE
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
  * lcKey = lcPrj_Typ + lcPrj_ID
  lckey = lcprj_typ + lcprj_id+ lcstyle +STR(lnlineno,6)
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]

CASE oariaapplication.activemoduleid = 'IC'
  DO CASE
  CASE lcprj_typ = 'S'      && Style
    lcprog  = 'ICSTYLE'
    lcevent = 'RESCHDSS'
    lckey   = lcstyle
  CASE lcprj_typ = 'H'      && Other
    lcprog  = 'MFPROJ'
    lcevent = 'RESCHDSH'
    lckey   = lcprj_typ + lcprj_id
  ENDCASE
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
CASE oariaapplication.activemoduleid = 'MA'
  DO CASE
  CASE lcprj_typ = 'M'      && Style
    lcprog  = 'ICITEM'
    lcevent = 'RESCHDSS'
    lckey   = lcstyle
  CASE lcprj_typ = 'H'      && Other
    lcprog  = 'MFPROJ'
    lcevent = 'RESCHDSH'
    lckey   = lcprj_typ + lcprj_id
  ENDCASE
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
ENDCASE

*!*************************************************************
*! Name      : lfPrpAudt
*: Developer : Mariam Mazhar[MMT]
*: Date      : 05/10/2009
*! Purpose   : Prepare audit trail data
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfPrpAudt()
*!*************************************************************
FUNCTION lfprpaudt
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
*!*  IF gfSEEK(&lcTmpDtFl..cPrj_Typ + &lcTmpDtFl..cPrj_ID +;
*!*  	    &lcTmpDtFl..cStyle + &lcTmpDtFl..cOprt_Ctg+&lcTmpDtFl..cOprt_ID,'SYSCHDUL')
IF gfseek(&lctmpdtfl..cprj_typ + &lctmpdtfl..cprj_id +;
    &lctmpdtfl..cstyle +STR(&lctmpdtfl..LINENO,6)+ &lctmpdtfl..coprt_ctg+&lctmpdtfl..coprt_id,'SYSCHDUL')
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
  IF !(&lctmpdtfl..lorginal)
    SELECT syschdul
    lcstauts = syschdul.coperstat

    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
    *!*      LOCATE REST WHILE cconttype+cseqnumber+cStyle+ccont_id+coperstat+cuser_id =;
    *!*                       &lcTmpDtFl..cPrj_Typ + &lcTmpDtFl..cPrj_ID + &lcTmpDtFl..cStyle + &lcTmpDtFl..cOprt_Ctg+&lcTmpDtFl..cOprt_ID ;
    *!*                       FOR COPERSTAT <> lcStauts
    LOCATE REST WHILE cconttype+cseqnumber+cstyle+STR(LINENO,6)+ccont_id+coperstat+cuser_id =;
      &lctmpdtfl..cprj_typ + &lctmpdtfl..cprj_id + &lctmpdtfl..cstyle +STR(&lctmpdtfl..LINENO,6)+ &lctmpdtfl..coprt_ctg+&lctmpdtfl..coprt_id ;
      FOR coperstat <> lcstauts
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
    IF !FOUND()
      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
      *!*        =gfSEEK(&lcTmpDtFl..cPrj_Typ + &lcTmpDtFl..cPrj_ID + &lcTmpDtFl..cStyle +;
      *!*        	    &lcTmpDtFl..cOprt_Ctg+&lcTmpDtFl..cOprt_ID+lcStauts,'SYSCHDUL')
      =gfseek(&lctmpdtfl..cprj_typ + &lctmpdtfl..cprj_id + &lctmpdtfl..cstyle +STR(&lctmpdtfl..LINENO,6)+;
        &lctmpdtfl..coprt_ctg+&lctmpdtfl..coprt_id+lcstauts,'SYSCHDUL')
      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
    ENDIF
  ENDIF

  IF syschdul.lpredcomp
    SELECT (lctmpdtfl)
    SCATTER MEMVAR MEMO
    ldest_fnsh = IIF(EMPTY(m.dact_fnsh),IIF(EMPTY(m.dclc_fnsh),m.dest_fnsh,m.dclc_fnsh),m.dact_fnsh)
    lnrem_dur  = IIF(EMPTY(m.nact_dur),m.nrem_dur,0)
    lcinform = ""
    =lfupdadtrl()
    SELECT (lc_prjaudt)
    =gfaudtrl(loformset , lcprog  , lckey , lcapobjnam , lcevent, lcinform)
    *E302741,1 WAM 08/25/2010 Convert Audit Trail file into SQL
    *!*	    SELECT AUDTRAIL
    *!*	    =gfReplace('')
    *!*	    =gfTableUpdate()
    *E302741,1 WAM 08/25/2010 (End)
  ENDIF
ELSE
  SELECT (lctmpdtfl)
  SCATTER MEMVAR MEMO
  ldest_fnsh = IIF(EMPTY(m.dact_fnsh),IIF(EMPTY(m.dclc_fnsh),m.dest_fnsh,m.dclc_fnsh),m.dact_fnsh)
  lnrem_dur  = IIF(EMPTY(m.nact_dur),m.nrem_dur,0)
  lcinform = ""
  =lfupdadtrl()
  SELECT (lc_prjaudt)
  =gfaudtrl(loformset , lcprog  , lckey , lcapobjnam , lcevent, lcinform)
  *E302741,1 WAM 08/25/2010 Convert Audit Trail file into SQL
  *!*	  =gfReplace('')
  *!*	  =gfTableUpdate()
  *E302741,1 WAM 08/25/2010 (End)
ENDIF


*!*************************************************************
*! Name      : lfUpdAdTrl
*: Developer : Mariam Mazhar[MMT]
*: Date      : 05/10/2009
*! Purpose   : update audit trail
*!*************************************************************
*! Calls     :
*!*************************************************************
*! Passed Parameters  :
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   :
*!*************************************************************
FUNCTION lfupdadtrl
PARAMETERS lcprojtype



SELECT (lc_prjaudt)
ZAP
IF TYPE('lcProjType') = 'C'
  lcprj_typ = lcprojtype
ENDIF
INSERT INTO (lc_prjaudt) (cprj_typ,coprt_ctg,coprt_id,coprt_dsc,dest_strt,dest_fnsh,nrem_dur,;
  coprt_res,lvoid);
  VALUES (lcprj_typ,m.coprt_ctg,m.coprt_id,m.coprt_dsc,;
  oariaapplication.systemdate,ldest_fnsh,lnrem_dur,;
  IIF(EMPTY(m.coprt_res),m.cgroup_id,m.coprt_res),m.lvoid)

*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
*IF gfSEEK(m.cPrj_Typ + m.cPrj_ID + m.cStyle + m.cOprt_Ctg+m.cOprt_ID,'SYSCHDUL')
IF gfseek(m.cprj_typ + m.cprj_id + m.cstyle+STR(m.lineno,6) + m.coprt_ctg+m.coprt_id,'SYSCHDUL')
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
  IF !(m.lorginal)
    SELECT syschdul
    lcstauts = syschdul.coperstat

    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
    *!*      LOCATE REST WHILE cconttype+cseqnumber+cStyle+ccont_id+coperstat+cuser_id =;
    *!*                      m.cPrj_Typ + m.cPrj_ID + m.cStyle + m.cOprt_Ctg+m.cOprt_ID ;
    *!*                      FOR COPERSTAT <> lcStauts
    LOCATE REST WHILE cconttype+cseqnumber+cstyle+STR(LINENO,6)+ccont_id+coperstat+cuser_id =;
      m.cprj_typ + m.cprj_id + m.cstyle +STR(m.lineno,6)+ m.coprt_ctg+m.coprt_id ;
      FOR coperstat <> lcstauts
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]

    IF !FOUND()
      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
      *=gfSEEK(m.cPrj_Typ + m.cPrj_ID + m.cStyle + m.cOprt_Ctg+m.cOprt_ID+lcStauts,'SYSCHDUL')
      =gfseek(m.cprj_typ + m.cprj_id + m.cstyle +STR(m.lineno,6)+ m.coprt_ctg+m.coprt_id+lcstauts,'SYSCHDUL')
      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
    ENDIF
  ENDIF
  IF !(m.lvoid)
    REPLACE &lc_prjaudt..cstatus WITH syschdul.coperstat
  ELSE
    REPLACE &lc_prjaudt..cstatus WITH 'X'
  ENDIF
ELSE
  IF !(m.lvoid)
    REPLACE &lc_prjaudt..cstatus WITH IIF(m.nact_dur <> 0  OR !EMPTY(m.dact_fnsh),'C','O')
  ELSE
    REPLACE &lc_prjaudt..cstatus WITH 'X'
  ENDIF
ENDIF

SELECT (lc_prjaudt)
LOCATE

IF !USED("PMCTGHD")
  =gfopentable('PMCTGHD','PMCTGHD','SH')
ENDIF
SELECT (lc_prjaudt)
=gfseek(coprt_ctg,"PMCTGHD")
lcinform = 'Operation Category: '           + pmctghd.cctg_dsc                     + CHR(13)+;
  'Activity: '                         + coprt_dsc                            + CHR(13)+;
  'User: '                         + coprt_res                            + CHR(13)+;
  'Remaining Time on Completion: ' + ALLTRIM(STR(nrem_dur,3)) + ' day(s)' + CHR(13)+;
  'Transaction Date: '             + DTOC(dest_strt)                      + CHR(13)+;
  'Completion Date: '              + DTOC(dest_fnsh)                      + CHR(13)+;
  'Status: '+IIF(cstatus=[O],[Open],IIF(cstatus=[C],[Complete],IIF(cstatus=[P],[In Work],;
  IIF(cstatus=[H],[Hold],IIF(cstatus=[X],[Void],[])))))


*!*************************************************************
*! Name      : lfUpdAdTrl
*: Developer : Mariam Mazhar[MMT]
*: Date      : 05/10/2009
*! Purpose   : Validate category
*!*************************************************************
FUNCTION lfvoprtctg
PARAMETERS lobranformset

WITH lobranformset.loparentform
  lc_pmprjdt = .lcprjdt
  PRIVATE lncuralias
  IF MDOWN()
    RETURN
  ENDIF

  lcprjhd    = .lcprjhdr
  lcprj_typ  = &lcprjhd..cprj_typ
  lcprj_id   = &lcprjhd..cprj_id
  lcstyle    = &lcprjhd..cstyle
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
  lnlineno = &lcprjhd..LINENO
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
  lc_pmprjrl = .lc_pmprjrl
ENDWITH

lcoprt_ctg   = PADR(ALLTRIM(lobranformset.ariaform1.cnttaskdet.kbcatid.keytextbox.VALUE), 3)
lncuralias   = SELECT(0)
llbrowse     = lobranformset.ariaform1.cnttaskdet.kbcatid.selectedfrombrowse .OR. '?' $ lcoprt_ctg
IF !llbrowse .AND. EMPTY(lcoprt_ctg)
  SET ORDER TO TAG pmprjdt IN (lc_pmprjdt)
  lobranformset.ariaform1.grdtasks.AFTERROWCOLCHANGE
ELSE
  IF !llbrowse .AND. ASCAN(lobranformset.loparentform.lacategrie, lcoprt_ctg) > 0
    lobranformset.ariaform1.cnttaskdet.kbcatid.ENABLED = .F.
    lobranformset.ariaform1.cnttaskdet.kbtaskid.ENABLED = .T.
  ELSE
    WITH  lobranformset
      .cbrowsetabledbengine   = 'SQL'
      .nworkarea        = 'PMCTGHD'
      .DATAENVIRONMENT.INITIALSELECTEDALIAS = 'PMCTGHD'
      .cbrowsefilename        = "PMCTGHD"
      .cbrowseindexexpression = "COPRT_CTG"
      .cbrowseindexfields     = "COPRT_CTG"
      .cbrowseindexname       = "PMCTGHD"
      .cbrowsealiasname       = "PMCTGHD"
      .cbrowsetablename       = "PMCTGHD"
    ENDWITH

    IF "?" $ lcoprt_ctg OR lobranformset.ariaform1.cnttaskdet.kbcatid.selectedfrombrowse
      lccateg = lfbrowcat()
      lcoprt_ctg = lccateg
      IF EMPTY(lccateg)
        lobranformset.ariaform1.cnttaskdet.kbcatid.keytextbox.VALUE = SPACE(3)
      ELSE
        lobranformset.ariaform1.cnttaskdet.kbcatid.keytextbox.VALUE = lccateg
      ENDIF
    ELSE
      lladdrec = .F.
      lccategory = lcoprt_ctg
      IF !gfseek(lcoprt_ctg,"PMCTGHD","PMCTGHD")
        IF lobranformset.seekrecord (lcoprt_ctg,'Category code ' + ALLTRIM(lcoprt_ctg)+" ") = 0
          lobranformset.ariaform1.cnttaskdet.kbcatid.keytextbox.VALUE = SPACE(3)
          RETURN .F.
        ENDIF
        IF !lladdrec
          lcoprt_ctg   = PADR(ALLTRIM(lobranformset.ariaform1.cnttaskdet.kbcatid.keytextbox.VALUE), 3)
        ELSE
          lcoprt_ctg   =lccategory
        ENDIF
      ENDIF
    ENDIF
    IF !EMPTY(lcoprt_ctg)
      lobranformset.ariaform1.cnttaskdet.kbcatid.ENABLED = .F.
    ENDIF


    IF ASCAN(lobranformset.loparentform.lacategrie, lcoprt_ctg) > 0 .OR. !lfaddoprs()
      * loBranFormSet.ariaform1.kbTaskID.Enabled = .T.
    ELSE
      WITH lobranformset.ariaform1.cnttaskdet
        .kbtaskid.ENABLED = .F.
        .txttaskname.ENABLED = .F.
        .cborespon.ENABLED = .F.
        .txtrespon.ENABLED = .F.
      ENDWITH
      SELECT (lc_pmprjdt)
      lncurtag = ORDER()
      SET ORDER TO TAG pmprjdt
      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
      *!*         SEEK SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
      *!*               SUBSTR(lcStyle,1,LEN(cStyle))+SUBSTR(lcOprt_Ctg,1,LEN(cOprt_Ctg))
      SEEK SUBSTR(lcprj_typ,1,LEN(cprj_typ)) + SUBSTR(lcprj_id,1,LEN(cprj_id)) +;
        SUBSTR(lcstyle,1,LEN(cstyle))+STR(lnlineno ,6)+SUBSTR(lcoprt_ctg,1,LEN(coprt_ctg))
      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
      SET ORDER TO (lncurtag)
      ACOPY(lobranformset.loparentform.laholidays,laholidays)
      =lfgetestdt(@lc_pmprjdt, @lc_pmprjrl,.F.)

      SELECT (lc_pmprjdt)
      lncurtag = ORDER()
      SET ORDER TO TAG pmprjdt
      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
      *!*        SEEK SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
      *!*               SUBSTR(lcStyle,1,LEN(cStyle))+SUBSTR(lcOprt_Ctg,1,LEN(cOprt_Ctg))
      SEEK SUBSTR(lcprj_typ,1,LEN(cprj_typ)) + SUBSTR(lcprj_id,1,LEN(cprj_id)) +;
        SUBSTR(lcstyle,1,LEN(cstyle))+STR(lnlineno ,6)+SUBSTR(lcoprt_ctg,1,LEN(coprt_ctg))
      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
      SET ORDER TO (lncurtag)
      lobranformset.ariaform1.grdtasks.AFTERROWCOLCHANGE
    ENDIF
  ENDIF
ENDIF
SELECT (lncuralias)

*!*************************************************************
*! Name      : lfGetData
*: Developer : Mariam Mazhar[MMT]
*: Date      : 05/10/2009
*! Purpose   : get project infor.
*!*************************************************************
FUNCTION lfgetdata
PARAMETERS loformset

loformset.lnnumoflin = 0
SELECT (loformset.lcprjhdr)
ZAP
SELECT  (loformset.lcprjdt)
ZAP
SELECT (loformset.lcpmctgdt)
ZAP
SELECT (loformset.lc_pmprjrl)
ZAP


SELECT pmprjhd
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
*=gfSeek(PMPRJHD.cprj_typ + PMPRJHD.cprj_id +PMPRJHD.cstyle)
=gfseek(pmprjhd.cprj_typ + pmprjhd.cprj_id +pmprjhd.cstyle+STR(pmprjhd.LINENO,6))
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
SCATTER MEMO MEMVAR
SELECT (loformset.lcprjhdr)
APPEND BLANK
GATHER  MEMO MEMVAR
lcprjhd = loformset.lcprjhdr
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
loformset.lcstyle = &lcprjhd..cstyle
loformset.nlinenum = &lcprjhd..LINENO
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
WITH loformset.ariaform1
  IF oariaapplication.activemoduleid = 'MF'
    .cboprojtype.VALUE = 1
  ENDIF
  .kbtranno.keytextbox.VALUE = &lcprjhd..cprj_id
  .cbotrantype.VALUE = &lcprjhd..cprj_typ
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
  * .kbStyle.keytextbox.Value = &lcPrjHd..cStyle
  .kbstyle.keytextbox.VALUE = SUBSTR(&lcprjhd..cstyle,1,loformset.lnmajorlen)
  IF &lcprjhd..LINENO <> 0
    .kbcolor.keytextbox.VALUE = RIGHT(&lcprjhd..cstyle,LEN(loformset.lcnonmajpic))
  ELSE
    IF !INLIST(oariaapplication.activemoduleid , 'MA','IC')
      .kbcolor.keytextbox.VALUE = STRTRAN(loformset.lcnonmajpic,'X','*')
    ELSE
      .kbcolor.keytextbox.VALUE = RIGHT(&lcprjhd..cstyle,LEN(loformset.lcnonmajpic))
      IF EMPTY(.kbcolor.keytextbox.VALUE) AND &lcprjhd..cprj_typ <> 'H'
        .kbcolor.keytextbox.VALUE = STRTRAN(loformset.lcnonmajpic,'X','*')
      ENDIF
    ENDIF
  ENDIF
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
  .kbstyle.ENABLED = .F.
  .txtnote.VALUE = &lcprjhd..cprj_sdsc
  .dtpreqstrt.VALUE = &lcprjhd..dreq_strt
  .dtpreqend.VALUE = &lcprjhd..dreq_fnsh
  .cbostatus.VALUE = &lcprjhd..cprj_stts
  .dtpactend.VALUE  = &lcprjhd..dact_fnsh
  .dtpactstrt.VALUE = &lcprjhd..dact_strt
  .dtpeststrt.VALUE =&lcprjhd..dest_strt
  .dtpestend.VALUE =&lcprjhd..dest_fnsh
  .dtpcalcstrt.VALUE =&lcprjhd..dclc_strt
  .dtpcalcend.VALUE =&lcprjhd..dclc_fnsh
  .kbtmpl.keytextbox.VALUE = &lcprjhd..cpath_id
  =gfseek(&lcprjhd..cpath_id,'PMPTHHD')
  .txttmpldesc.VALUE = pmpthhd.cpath_dsc
  .chklststrt.VALUE =  &lcprjhd..llaststrt
  loformset.lnlatstrt = IIF(&lcprjhd..llaststrt,1,0)
  .dtpschdul.VALUE  = &lcprjhd..ddta_date
  .chkreschd.VALUE =  &lcprjhd..lschedual
  .cbostatus.VALUE  =&lcprjhd..cprj_stts
ENDWITH

*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
IF !INLIST(oariaapplication.activemoduleid , 'MA','IC')
  IF SUBSTR(&lcprjhd..cstyle,1,loformset.lnmajorlen) = REPLICATE('*',loformset.lnmajorlen)
    loformset.ariaform1.cboprjfor.VALUE = 'O'
    loformset.ariaform1.cmdstyle.VISIBLE = .F.
    loformset.ariaform1.lststyle.VISIBLE = .F.
  ELSE
    loformset.ariaform1.lststyle.CLEAR ()
    loformset.ariaform1.cmdstyle.VISIBLE = .T.
    loformset.ariaform1.lststyle.VISIBLE = .T.
    IF &lcprjhd..LINENO <> 0
      loformset.ariaform1.lststyle.ADDITEM (&lcprjhd..cstyle,1)
      loformset.ariaform1.cboprjfor.VALUE = 'L'
      loformset.ariaform1.lststyle.HEIGHT = loformset.ariaform1.kbtranno.HEIGHT + loformset.ariaform1.cbostatus.HEIGHT + 7

      DO CASE
      CASE &lcprjhd..cprj_typ = 'P'

        IF gfseek('PP'+&lcprjhd..cprj_id+'0001'+&lcprjhd..cstyle+STR(&lcprjhd..LINENO,6)+"1",'POSLN','POSLN')
          loformset.ariaform1.lststyle.ADDITEM ("Complete:"+DTOC(posln.COMPLETE),2)
          loformset.ariaform1.lststyle.ADDITEM ("TotQty   :"+STR(posln.totqty,9),3)
        ENDIF
      CASE &lcprjhd..cprj_typ = 'C'
        IF gfseek('PU'+&lcprjhd..cprj_id+'0001'+&lcprjhd..cstyle+STR(&lcprjhd..LINENO,6)+"1",'POSLN','POSLN')
          loformset.ariaform1.lststyle.ADDITEM ("Complete:"+DTOC(posln.COMPLETE),2)
          loformset.ariaform1.lststyle.ADDITEM ("TotQty   :"+STR(posln.totqty,9),3)
        ENDIF

      CASE &lcprjhd..cprj_typ = 'O'
        IF gfseek('O'+&lcprjhd..cprj_id+STR(&lcprjhd..LINENO,6),'Ordline','Ordline')
          loformset.ariaform1.lststyle.ADDITEM ("Complete:"+DTOC(ordline.COMPLETE),2)
          loformset.ariaform1.lststyle.ADDITEM ("TotQty   :"+STR(ordline.totqty,9),3)
        ENDIF

      CASE &lcprjhd..cprj_typ = 'T'
        IF gfseek('T'+&lcprjhd..cprj_id+STR(&lcprjhd..LINENO,6),'Ordline','Ordline')
          loformset.ariaform1.lststyle.ADDITEM ("Complete:"+DTOC(ordline.COMPLETE),2)
          loformset.ariaform1.lststyle.ADDITEM ("TotQty   :"+STR(posln.totqty,9),3)
        ENDIF

      CASE &lcprjhd..cprj_typ = 'A'
        IF gfseek('PA'+&lcprjhd..cprj_id+'0001'+&lcprjhd..cstyle+STR(&lcprjhd..LINENO,6)+"1",'POSLN','POSLN')
          loformset.ariaform1.lststyle.ADDITEM ("Complete:"+DTOC(posln.COMPLETE),2)
          loformset.ariaform1.lststyle.ADDITEM ("TotQty   :"+STR(posln.totqty,9),3)
        ENDIF

      CASE &lcprjhd..cprj_typ = 'D'
        IF gfseek('PD'+&lcprjhd..cprj_id+'0001'+&lcprjhd..cstyle+STR(&lcprjhd..LINENO,6)+"1",'POSLN','POSLN')
          loformset.ariaform1.lststyle.ADDITEM ("Complete:"+DTOC(posln.COMPLETE),2)
          loformset.ariaform1.lststyle.ADDITEM ("TotQty   :"+STR(posln.totqty,9),3)
        ENDIF

      CASE &lcprjhd..cprj_typ = 'N'
        IF gfseek('NN'+&lcprjhd..cprj_id+'0001'+&lcprjhd..cstyle+STR(&lcprjhd..LINENO,6)+"1",'POSLN','POSLN')
          loformset.ariaform1.lststyle.ADDITEM ("Complete:"+DTOC(posln.COMPLETE),2)
          loformset.ariaform1.lststyle.ADDITEM ("TotQty   :"+STR(posln.totqty,9),3)
        ENDIF

      CASE &lcprjhd..cprj_typ = 'R'
        IF gfseek('RP'+&lcprjhd..cprj_id+'0001'+&lcprjhd..cstyle+STR(&lcprjhd..LINENO,6)+"1",'POSLN','POSLN')
          loformset.ariaform1.lststyle.ADDITEM ("Complete:"+DTOC(posln.COMPLETE),2)
          loformset.ariaform1.lststyle.ADDITEM ("TotQty   :"+STR(posln.totqty,9),3)
        ENDIF

      ENDCASE
    ELSE
      loformset.ariaform1.lststyle.ADDITEM (&lcprjhd..cstyle,1)
      loformset.ariaform1.cboprjfor.VALUE = 'S'
      loformset.ariaform1.lststyle.HEIGHT = loformset.ariaform1.cboprjfor.HEIGHT
    ENDIF
  ENDIF
ENDIF
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
DIMENSION  loformset.laopertion[1]
STORE '' TO loformset.laopertion
SELECT (loformset.lcpmprjntf)
ZAP
SELECT pmprjdt
=gfsetorder('PMPRJDTS')
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
*!*   =gfSeek(loFormSet.ariaform1.cboTranType.Value+EVALUATE(loFormSet.lcprjhdr+'.cPrj_ID')+EVALUATE(loFormSet.lcprjhdr+'.Cstyle'))
*!*   SCAN REST WHILE CPRJ_TYP+CPRJ_ID+CSTYLE+COPRT_CTG+COPRT_ID = ;
*!*      loFormSet.ariaform1.cboTranType.Value+EVALUATE(loFormSet.lcprjhdr+'.cPrj_ID')+EVALUATE(loFormSet.lcprjhdr+'.Cstyle')
=gfseek(loformset.ariaform1.cbotrantype.VALUE+EVALUATE(loformset.lcprjhdr+'.cPrj_ID')+EVALUATE(loformset.lcprjhdr+'.Cstyle')+STR(EVALUATE(loformset.lcprjhdr+'.LINENO'),6))
SCAN REST WHILE cprj_typ+cprj_id+cstyle+STR(LINENO,6)+coprt_ctg+coprt_id = ;
    loformset.ariaform1.cbotrantype.VALUE+EVALUATE(loformset.lcprjhdr+'.cPrj_ID')+EVALUATE(loformset.lcprjhdr+'.Cstyle')+STR(EVALUATE(loformset.lcprjhdr+'.LINENO'),6)
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
  SCATTER MEMO MEMVAR
  IF ISNULL(m.donschdl)
    m.donschdl = {}
  ENDIF
  m.nrecno  = RECNO()
  m.cstatus = 'S'
  lcmcomps = IIF(EMPTY(m.dact_strt),'Y','N')
  lcmcompf = IIF(EMPTY(m.dact_fnsh),'Y','N')
  lcmcompd = IIF(m.nact_dur = 0 ,'Y','N')
  m.cmcomplt = lcmcomps + lcmcompf + lcmcompd

  INSERT INTO (loformset.lcprjdt) FROM MEMVAR
  loformset.laopertion[ALEN(loFormSet.laOpertion)] = m.coprt_ctg + '\' + m.coprt_id + ;
    ' ' +SUBSTR(m.coprt_dsc, 1, 18)

  DIMENSION  loformset.laopertion[ALEN(loFormSet.laOpertion) + 1]

ENDSCAN


loformset.lnnumoflin = RECCOUNT(loformset.lcprjdt)
lcpmprjntf = loformset.lcpmprjntf
SELECT pmprjntf
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
*!*   IF gfSEEK(loFormSet.ariaform1.cboTranType.Value+EVALUATE(loFormSet.lcprjhdr+'.cPrj_ID')+EVALUATE(loFormSet.lcprjhdr+'.Cstyle'))
*!*     SCAN REST WHILE CPRJ_TYP+CPRJ_ID+CSTYLE+COPRT_CTG+COPRT_ID+CUSER_ID = loFormSet.ariaform1.cboTranType.Value+EVALUATE(loFormSet.lcprjhdr+'.cPrj_ID')+EVALUATE(loFormSet.lcprjhdr+'.Cstyle')
IF gfseek(loformset.ariaform1.cbotrantype.VALUE+EVALUATE(loformset.lcprjhdr+'.cPrj_ID')+EVALUATE(loformset.lcprjhdr+'.Cstyle')+STR(EVALUATE(loformset.lcprjhdr+'.LINENO'),6))
  SCAN REST WHILE cprj_typ+cprj_id+cstyle+STR(LINENO,6)+coprt_ctg+coprt_id+cuser_id = loformset.ariaform1.cbotrantype.VALUE+EVALUATE(loformset.lcprjhdr+'.cPrj_ID')+EVALUATE(loformset.lcprjhdr+'.Cstyle')++STR(EVALUATE(loformset.lcprjhdr+'.LINENO'),6)
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
    SCATTER MEMVAR
    m.cstatus = 'S'
    INSERT INTO (lcpmprjntf) FROM MEMVAR
  ENDSCAN
ENDIF

SELECT (loformset.lcprjdt)
LOCATE
lc_pmprjdt = loformset.lcprjdt
lc_pmprjrl = loformset.lc_pmprjrl
IF ALEN(loformset.laopertion) > 1
  DIMENSION loformset.laopertion[ALEN(loFormSet.laOpertion) - 1]
ENDIF

SELECT DISTINCT coprt_ctg ;
  FROM (lc_pmprjdt);
  INTO ARRAY loformset.lacategrie

SELECT (lc_pmprjrl)
ZAP

SELECT pmprjrl
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
*!*  IF gfSEEK(loFormSet.ariaform1.cboTranType.Value +EVALUATE(loFormSet.lcprjhdr+'.cPrj_ID')+ EVALUATE(loFormSet.lcprjhdr+'.Cstyle'))
*!*    SCAN REST WHILE  cprj_typ+cprj_id+cstyle+coprt_ctg+coprt_id = ;
*!*                     loFormSet.ariaform1.cboTranType.Value +EVALUATE(loFormSet.lcprjhdr+'.cPrj_ID')+ EVALUATE(loFormSet.lcprjhdr+'.Cstyle')
IF gfseek(loformset.ariaform1.cbotrantype.VALUE +EVALUATE(loformset.lcprjhdr+'.cPrj_ID')+ EVALUATE(loformset.lcprjhdr+'.Cstyle')+STR(EVALUATE(loformset.lcprjhdr+'.LINENO'),6))
  SCAN REST WHILE  cprj_typ+cprj_id+cstyle+STR(LINENO,6)+coprt_ctg+coprt_id = ;
      loformset.ariaform1.cbotrantype.VALUE +EVALUATE(loformset.lcprjhdr+'.cPrj_ID')+ EVALUATE(loformset.lcprjhdr+'.Cstyle')+STR(EVALUATE(loformset.lcprjhdr+'.LINENO'),6)
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
    SCATTER MEMVAR MEMO
    m.nrecno  = RECNO()
    m.cstatus = 'S'
    INSERT INTO (lc_pmprjrl) FROM MEMVAR
  ENDSCAN
ENDIF

*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
*!*   SELECT cOprt_Ctg + '\' + cOprt_ID + ' ' +SUBSTR(cOprt_Dsc, 1, 18);
*!*   FROM PMPRJDT  ;
*!*   WHERE cPrj_Typ + cPrj_ID + cStyle = loFormSet.ariaform1.cboTranType.Value +EVALUATE(loFormSet.lcprjhdr+'.cPrj_ID')+EVALUATE(loFormSet.lcprjhdr+'.Cstyle');
*!*   INTO ARRAY loFormSet.laOpertion
SELECT coprt_ctg + '\' + coprt_id + ' ' +SUBSTR(coprt_dsc, 1, 18);
  FROM pmprjdt  ;
  WHERE cprj_typ + cprj_id + cstyle+STR(LINENO,6) = loformset.ariaform1.cbotrantype.VALUE +EVALUATE(loformset.lcprjhdr+'.cPrj_ID')+EVALUATE(loformset.lcprjhdr+'.Cstyle')+STR(EVALUATE(loformset.lcprjhdr+'.LINENO'),6);
  INTO ARRAY loformset.laopertion
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]


*!*************************************************************
*! Name      : lfvNew
*: Developer : Mariam Mazhar[MMT]
*: Date      : 05/10/2009
*! Purpose   : new task button
*!*************************************************************
FUNCTION lfvnew
PARAMETERS lobranchform
WITH lobranchform.ariaform1
  .kbcalen.ENABLED = .F.
  .txtcatdesc.ENABLED = .F.
  .txtcatdesc.VALUE = ''
  .cmdnote.ENABLED = .F.
  .lblvoid.VISIBLE = .F.
  .kbtaskid.keytextbox.VALUE = ''
  .kbtaskid.ENABLED = .F.
  .kbcalen.keytextbox.VALUE = ''
  .txttaskname.ENABLED = .F.
  .txttaskname.VALUE = ''
  .cboupmeth.ENABLED = .F.
  .cborespon.ENABLED = .F.
  .cborespon.VALUE = 1
  .txtrespon.VALUE = ""
  .txtpred.VALUE = ""
  .txtnotify.VALUE = ""
  .cmdpred.ENABLED = .F.
  .cmdnotify.ENABLED = .F.
  .cboupmeth.VALUE = lang_mfproj_manual
  .chkshowcust.ENABLED = .F.
  .chkshowcust.VALUE = .T.
  .cmdsort.ENABLED = .F.
  .txtest.ENABLED = .F.
  .txtest.VALUE = 0
  .cmdvoid.ENABLED = .F.
  .txtrem.ENABLED = .F.
  .txtrem.VALUE = 0
  .txtact.ENABLED = .F.
  .txtact.VALUE = 0
  .txtperc.ENABLED = .F.
  .txtperc.VALUE = 0
  .dtpactend.ENABLED = .F.
  .dtpactend.VALUE = {}
  .dtcalcsrt.ENABLED = .F.
  .dtcalcsrt.VALUE = {}
  .dtpactstr.ENABLED = .F.
  .dtpactstr.VALUE = {}
  .dtpcalcend.ENABLED = .F.
  .dtpcalcend.VALUE = {}
  .dtpestend.ENABLED = .F.
  .dtpestend.VALUE = {}
  .dtpeststrt.ENABLED = .F.
  .dtpeststrt.VALUE = {}
  .kbcatid.keytextbox.VALUE = ''
  .kbcatid.ENABLED = .T.
  .kbcatid.keytextbox.SETFOCUS()
ENDWITH

*!*************************************************************
*! Name      : lfBrowCat
*: Developer : Mariam Mazhar[MMT]
*: Date      : 05/10/2009
*! Purpose   : browe category
*!*************************************************************
FUNCTION lfbrowcat
lccategno  = ''
SELECT pmctghd
=gfseek("")
LOCATE
lcbrfields =      "coprt_ctg  :H='"+lang_mfproj_category+"'," +;
  "cctg_dsc   :H='"+lang_mfproj_desc+"'," +;
  "cctg_seq   :H='Cctg_seq'," +;
  "cadd_user  :H='Cadd_user' ," +;
  "cadd_time  :H='Cadd_time'," +;
  "dadd_date  :H='Dadd_date'," +;
  "llok_stat  :H='Llok_stat'," +;
  "clok_user  :H='Clok_user'," +;
  "dlok_date  :H='Dlok_date'," +;
  "clok_time  :H='Clok_time'"

lccategno = IIF(ariabrow("",lang_mfproj_category,gnbrfsrow1, gnbrfscol1, gnbrfsrow2, ;
  gnbrfscol2,'','','COPRT_CTG','laBrowArr', .F.,'PMCTGHD',.F.,'PMCTGHD','','','PMCTGHD'),pmctghd.coprt_ctg ,SPACE(3))



RETURN   lccategno

*!*************************************************************
*! Name      : lfAddOprs
*: Developer : Mariam Mazhar[MMT]
*: Date      : 05/10/2009
*! Purpose   : Automatic addition of operations through a mover.
*!*************************************************************
*! Calls     : ARIAMOVR.PRG
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  .T. if operations are added to the file,
*!                       .F. otherwise.
*!*************************************************************
*! Example            :  =lfAddOprs()
*!*************************************************************
FUNCTION lfaddoprs
PRIVATE laavailoprs, laseloprs, lladdoprs
DECLARE laavailoprs[1], laseloprs[1]

*B609711,1 MMT 01/05/2012 Fix of Error 'gfOpenFile' is found while add activities[T20110808.0008][START]
*!*	lcSetProc = SET('PROCEDURE')
*!*	SET PROCEDURE TO
*B609711,1 MMT 01/05/2012 Fix of Error 'gfOpenFile' is found while add activities[T20110808.0008][END]
lcoprt_ctg   = PADR(ALLTRIM(lobranformset.ariaform1.cnttaskdet.kbcatid.keytextbox.VALUE), 3)

WITH lobranformset.loparentform
  lcprjhd    = .lcprjhdr
  lcpmprjntf = .lcpmprjntf
  lc_pmprjrl = .lc_pmprjrl
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
  lnlinenum = lobranformset.loparentform.nlinenum
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
ENDWITH

lcprj_id = &lcprjhd..cprj_id
lcprj_typ= &lcprjhd..cprj_typ
lcstyle= &lcprjhd..cstyle
ldcurdate = oariaapplication.systemdate
lladdoprs = .F.
SELECT  pmctgdt
=gfsetorder('PMCTGDT')
IF gfseek(lcoprt_ctg, 'PMCTGDT')

  SELECT coprt_id + ' ' + coprt_dsc ;
    FROM pmctgdt;
    WHERE coprt_ctg = lcoprt_ctg;
    ORDER BY coprt_seq;
    INTO ARRAY laavailoprs

  laseloprs = ''


  =gfmover(@laavailoprs,@laseloprs,;
    lang_mfproj_slctoper,.T.,"",.F.,.F.,lobranformset)




  IF !EMPTY(laseloprs[1])
    STORE .T. TO lobranformset.loparentform.llchngoprt, glupdated
    lobranformset.loparentform.llupdstrtd = .F.
    SELECT (lc_pmprjdt)
    lcnewctgseq = lfgetseqnum()
    FOR lncountr = 1 TO ALEN(laseloprs,1)
      IF gfseek(lcoprt_ctg + SUBSTR(laseloprs[lnCountr], 1, 5), 'PMCTGDT')
        SELECT pmctgdt
        SCATTER MEMVAR MEMO
        *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
        *!*          INSERT INTO (lc_PMPrjDt);
        *!*              ( cPrj_Typ , cPrj_ID  ,  cStyle,  ;
        *!*                cCtg_Seq , cOprt_Ctg, cOprt_Seq,;
        *!*                cOprt_ID , cOprt_Dsc, cOprt_Res,;
        *!*                lShw2Cust, mOprt_Com, cUpdtMthd, ;
        *!*                nest_dur , nrem_dur , cCal_id,;
        *!*                mNotify, cStatus,LORGINAL,cGroup_ID,;
        *!*                cAdd_User, dAdd_Date, cAdd_Time,cMComplt,lAddToAud);
        *!*              VALUES;
        *!*               (lcPrj_Typ, lcPrj_ID, lcStyle, ;
        *!*               lcNewCtgSeq, m.cOprt_Ctg, m.cOprt_Seq, ;
        *!*               m.cOprt_ID, m.cOprt_Dsc, m.cOprt_Res,;
        *!*               m.lShw2Cust, m.mOprt_Com, m.cUpdtMthd,;
        *!*               m.nest_dur, m.nest_dur, m.cCal_id,;
        *!*               m.mNotify,  'A',.T.,m.cGroup_ID,;
        *!*               oariaapplication.user_id  , ldCurDate, TIME(),'YYY',.T.)
        *B609555,1 MMT 03/27/2011 Task List screen gives error while completing task[Start]
        m.moprt_com = ''
        *B609555,1 MMT 03/27/2011 Task List screen gives error while completing task[END]

        INSERT INTO (lc_pmprjdt);
          ( cprj_typ , cprj_id  ,  cstyle,  ;
          cctg_seq , coprt_ctg, coprt_seq,;
          coprt_id , coprt_dsc, coprt_res,;
          lshw2cust, moprt_com, cupdtmthd, ;
          nest_dur , nrem_dur , ccal_id,;
          mnotify, cstatus,lorginal,cgroup_id,;
          cadd_user, dadd_date, cadd_time,cmcomplt,laddtoaud,LINENO);
          VALUES;
          (lcprj_typ, lcprj_id, lcstyle, ;
          lcnewctgseq, m.coprt_ctg, m.coprt_seq, ;
          m.coprt_id, m.coprt_dsc, m.coprt_res,;
          m.lshw2cust, m.moprt_com, m.cupdtmthd,;
          m.nest_dur, m.nest_dur, m.ccal_id,;
          m.mnotify,  'A',.T.,m.cgroup_id,;
          oariaapplication.user_id  , ldcurdate, TIME(),'YYY',.T.,lnlinenum )
        *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]

        lobranformset.loparentform.lnnumoflin = lobranformset.loparentform.lnnumoflin + 1
        IF !EMPTY(lobranformset.loparentform.laopertion[1])
          DIMENSION  lobranformset.loparentform.laopertion[ALEN(loBranFormSet.loparentform.laOpertion) + 1]
        ENDIF
        lobranformset.loparentform.laopertion[ALEN(loBranFormSet.loparentform.laOpertion)] = lcoprt_ctg + '\' + m.coprt_id + ;
          ' ' + SUBSTR(m.coprt_dsc, 1, 18)

        IF gfseek(lcoprt_ctg + m.coprt_id, 'PMCTGRL')
          SELECT pmctgrl
          SCAN REST WHILE coprt_ctg + coprt_id = lcoprt_ctg + m.coprt_id;
              FOR ASCAN(lobranformset.loparentform.laopertion, coprt_ctg + '\' + cprd_id ) > 0
            SCATTER MEMVAR MEMO
            INSERT INTO (lc_pmprjrl) FROM MEMVAR
            SELECT (lc_pmprjrl)
            REPLACE cprj_typ  WITH lcprj_typ,;
              cprj_id   WITH lcprj_id,;
              cstyle    WITH lcstyle,;
              cprd_ctg  WITH coprt_ctg,;
              cadd_user WITH oariaapplication.user_id                          ,;
              dadd_date WITH ldcurdate,;
              cadd_time WITH TIME(),;
              cstatus WITH 'A'
            *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
            REPLACE LINENO WITH lnlinenum
            *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[end]

          ENDSCAN
        ENDIF    && ENDIF SE EK(lcOprt_Ctg + m.cOprt_ID, 'PMCTGRL')


        IF gfseek(lcoprt_ctg + m.coprt_id, 'PMPCTGNT')
          SELECT pmpctgnt
          SCAN REST WHILE coprt_ctg + coprt_id = lcoprt_ctg + m.coprt_id
            SCATTER MEMVAR
            m.cprj_typ = lcprj_typ
            m.cprj_id  = lcprj_id
            m.cstyle   = lcstyle
            m.cstatus  = 'A'
            *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
            m.lineno = lnlinenum
            *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
            INSERT INTO (lcpmprjntf) FROM MEMVAR
          ENDSCAN
        ENDIF


      ENDIF    && ENDIF SEEK(lcOprt_Ctg + SUBSTR(laSelOprs[lnCountr]), 1, 5), 'PMCTGDT')
    ENDFOR   && ENDFOR lnCountr = 1 TO ALEN(laSelOprs,1)
    IF ALEN(lobranformset.loparentform.laopertion) > 1
      lobranformset.ariaform1.cnttaskdet.cmdpred.ENABLED = .T.
    ENDIF
    lladdoprs = .T.
    IF !EMPTY(lobranformset.loparentform.lacategrie[1])
      DIMENSION lobranformset.loparentform.lacategrie[ALEN(loBranFormSet.loparentform.laCategrie) + 1]
    ENDIF
    lobranformset.loparentform.lacategrie[ALEN(loBranFormSet.loparentform.laCategrie)] = lcoprt_ctg
  ENDIF    && ENDIF !EMTPY(laSelOprs[1])
  SELECT (lc_pmprjdt)
ELSE
  =gfmodalgen("TRM38225B00000","DIALOG",ALLTRIM(lcoprt_ctg))
  lladdoprs = .T.
ENDIF
*B609711,1 MMT 01/05/2012 Fix of Error 'gfOpenFile' is found while add activities[T20110808.0008][START]
*!*	SET PROCEDURE TO &lcSetProc
*B609711,1 MMT 01/05/2012 Fix of Error 'gfOpenFile' is found while add activities[T20110808.0008][END]
RETURN lladdoprs


*!*************************************************************
*! Name      : lfGetSeqNum
*: Developer : Mariam Mazhar[MMT]
*: Date      : 05/10/2009
*! Purpose   : Calculates a category sequence number
*!*************************************************************
*! Calls              : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  A correct category sequence number
*!*************************************************************
*! Example            :  lfGetSeqNum()
*!*************************************************************
FUNCTION lfgetseqnum
PRIVATE lncuralias, lncurtag, lncurrec, lcnewctgseq

lncuralias = SELECT(0)
SELECT (lc_pmprjdt)
lncurrec = RECNO()
lncurtag = ORDER()
SET ORDER TO  'PMPRJDT'

*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
*!*  IF SEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
*!*          SUBSTR(lcStyle,1,LEN(cStyle))+ SUBSTR(lcOprt_Ctg,1,LEN(cOprt_Ctg)))
IF SEEK(SUBSTR(lcprj_typ,1,LEN(cprj_typ)) + SUBSTR(lcprj_id,1,LEN(cprj_id)) +;
    SUBSTR(lcstyle,1,LEN(cstyle))+STR(lnlinenum,6)+ SUBSTR(lcoprt_ctg,1,LEN(coprt_ctg)))
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
  lcnewctgseq = cctg_seq
ELSE
  SELECT MAX(cctg_seq) ;
    FROM (lc_pmprjdt);
    INTO ARRAY latmparry
  lcnewctgseq = IIF(_TALLY > 0 AND !ISNULL(latmparry[1]), PADL(INT(VAL(latmparry))+1,  2, '0'), '01')
ENDIF
SET ORDER TO (lncurtag)
IF BETWEEN(lncurrec, 1, RECCOUNT())
  GO lncurrec
ENDIF
SELECT (lncuralias)
RETURN lcnewctgseq

*!*************************************************************
*! Name      : lfIsHold
*: Developer : Mariam Mazhar[MMT]
*: Date      : 05/10/2009
*! Purpose   : Check if the current opertion is hold
*!*************************************************************
*! Calls              : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfIsHold()
*!*************************************************************
FUNCTION lfishold
PRIVATE lnalias,lcoldorder,llret
llret = .F.
lnalias = SELECT(0)
SELECT syschdul
lcoldorder = ORDER()
=gfsetorder('COPRUSR')
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
*!*  IF gfSEEK(lcPrj_Typ+lcPrj_ID+lcStyle+&lc_PMPrjDt..cOprt_Ctg+&lc_PMPrjDt..cOprt_ID)
*!*    LOCATE REST WHILE cconttype+cseqnumber+cStyle+ccont_id+coperstat+cuser_id =;
*!*                      lcPrj_Typ+lcPrj_ID+lcStyle+&lc_PMPrjDt..cOprt_Ctg+&lc_PMPrjDt..cOprt_ID+'O'
IF gfseek(lcprj_typ+lcprj_id+lcstyle+STR(lnlineno ,6)+&lc_pmprjdt..coprt_ctg+&lc_pmprjdt..coprt_id)
  LOCATE REST WHILE cconttype+cseqnumber+cstyle+STR(LINENO,6)+ccont_id+coperstat+cuser_id =;
    lcprj_typ+lcprj_id+lcstyle+STR(lnlineno ,6)+&lc_pmprjdt..coprt_ctg+&lc_pmprjdt..coprt_id+'O'
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
  IF !FOUND()
    llret = .T.
  ENDIF
ENDIF
=gfsetorder(lcoldorder)
SELECT (lnalias)
RETURN llret
*!*************************************************************
*! Name      : lfChkPred
*: Developer : Mariam Mazhar[MMT]
*: Date      : 05/10/2009
*! Purpose   : Check period
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfChkPred()
*!*************************************************************
FUNCTION lfchkpred

lccurals = ALIAS()
lccurid  = &lc_pmprjdt..coprt_ctg+&lc_pmprjdt..coprt_id
DIMENSION lapred_id[1]
lapred_id[1] = ""
SET ORDER TO pmprjrl IN (lc_pmprjrl)
lni = 0
SELECT (lc_pmprjrl)
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
*!*  SCAN FOR cprj_typ+cprj_id+cstyle+coprt_ctg+coprt_id = ;
*!*                  SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID))+;
*!*                  SUBSTR(lcStyle,1,LEN(cStyle)) + &lc_PMPrjDt..cOprt_Ctg+&lc_PMPrjDt..cOprt_ID
SCAN FOR cprj_typ+cprj_id+cstyle+STR(LINENO,6)+coprt_ctg+coprt_id = ;
    SUBSTR(lcprj_typ,1,LEN(cprj_typ)) + SUBSTR(lcprj_id,1,LEN(cprj_id))+;
    SUBSTR(lcstyle,1,LEN(cstyle)) +STR(lnlineno ,6)+ &lc_pmprjdt..coprt_ctg+&lc_pmprjdt..coprt_id
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
  DIMENSION lapred_id[lnI+1]
  lni = ALEN(lapred_id,1)
  lapred_id[lnI] = &lc_pmprjrl..cprd_ctg+&lc_pmprjrl..cprd_id
ENDSCAN


lccurord = ORDER(lc_pmprjdt)
SET ORDER TO pmprjdt IN (lc_pmprjdt)
IF !EMPTY(lapred_id[1])
  FOR lni = 1 TO ALEN(lapred_id,1)
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
    *!*      IF SEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID))+;
    *!*              SUBSTR(lcStyle,1,LEN(cStyle)) + laPred_ID[1],lc_PMPrjDt) AND EMPTY(&lc_PMPrjDt..dAct_strt)
    IF SEEK(SUBSTR(lcprj_typ,1,LEN(cprj_typ)) + SUBSTR(lcprj_id,1,LEN(cprj_id))+;
        SUBSTR(lcstyle,1,LEN(cstyle))+STR(lnlineno,6) + lapred_id[1],lc_pmprjdt) AND EMPTY(&lc_pmprjdt..dact_strt)
      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
      llopen  = .T.
      EXIT
    ENDIF
  ENDFOR
ENDIF
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
*!*  =SEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID))+;
*!*              SUBSTR(lcStyle,1,LEN(cStyle)) + lcCurID ,lc_PMPrjDt)
=SEEK(SUBSTR(lcprj_typ,1,LEN(cprj_typ)) + SUBSTR(lcprj_id,1,LEN(cprj_id))+;
  SUBSTR(lcstyle,1,LEN(cstyle)) +STR(lnlineno,6)+ lccurid ,lc_pmprjdt)
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
SET ORDER TO (lccurord) IN (lc_pmprjdt)
SELECT (lccurals)

*!*************************************************************
*! Name      : lfGetPred
*: Developer : Mariam Mazhar[MMT]
*: Date      : 05/10/2009
*! Purpose   : get Period
*!*************************************************************
FUNCTION lfgetpred


PRIVATE lcprdstrng, lncuralias
lncuralias = SELECT(0)
lcprdstrng = ''

SELECT (lc_pmprjdt)


IF lcactmode  = 'V'
  SELECT (lc_pmprjdt)
  IF BETWEEN(RECNO(), 1, RECCOUNT())
    GO RECNO()
  ENDIF
  SELECT pmprjrl
  =gfsetorder('PMPRJRL')
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
  *!*    =gfSEEK(SUBSTR(&lc_PMPrjDt..cPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(&lc_PMPrjDt..cPrj_ID,1,LEN(cPrj_ID)) +;
  *!*                 SUBSTR(&lc_PMPrjDt..cStyle,1,LEN(cStyle)) + SUBSTR(&lc_PMPrjDt..cOprt_Ctg,1,LEN(cOprt_Ctg)) +;
  *!*                 SUBSTR(&lc_PMPrjDt..cOprt_ID,1,LEN(cOprt_ID)))
  *!*
  *!*    SCAN REST WHILE cPrj_Typ + cPrj_ID + cStyle + cOprt_Ctg + cOprt_ID = ;
  *!*                 SUBSTR(&lc_PMPrjDt..cPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(&lc_PMPrjDt..cPrj_ID,1,LEN(cPrj_ID)) +;
  *!*                 SUBSTR(&lc_PMPrjDt..cStyle,1,LEN(cStyle)) + SUBSTR(&lc_PMPrjDt..cOprt_Ctg,1,LEN(cOprt_Ctg)) +;
  *!*                 SUBSTR(&lc_PMPrjDt..cOprt_ID,1,LEN(cOprt_ID))
  =gfseek(SUBSTR(&lc_pmprjdt..cprj_typ,1,LEN(cprj_typ)) + SUBSTR(&lc_pmprjdt..cprj_id,1,LEN(cprj_id)) +;
    SUBSTR(&lc_pmprjdt..cstyle,1,LEN(cstyle)) +STR(lnlineno,6)+ SUBSTR(&lc_pmprjdt..coprt_ctg,1,LEN(coprt_ctg)) +;
    SUBSTR(&lc_pmprjdt..coprt_id,1,LEN(coprt_id)))

  SCAN REST WHILE cprj_typ + cprj_id + cstyle +STR(LINENO,6)+ coprt_ctg + coprt_id = ;
      SUBSTR(&lc_pmprjdt..cprj_typ,1,LEN(cprj_typ)) + SUBSTR(&lc_pmprjdt..cprj_id,1,LEN(cprj_id)) +;
      SUBSTR(&lc_pmprjdt..cstyle,1,LEN(cstyle)) +STR(lnlineno,6)+ SUBSTR(&lc_pmprjdt..coprt_ctg,1,LEN(coprt_ctg)) +;
      SUBSTR(&lc_pmprjdt..coprt_id,1,LEN(coprt_id))
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
    lcprdstrng = lcprdstrng + ALLTRIM(cprd_ctg) + '\' + ALLTRIM(cprd_id) +', '
  ENDSCAN

ELSE
  SELECT (lc_pmprjdt)
  IF BETWEEN(RECNO(), 1, RECCOUNT())
    GO RECNO()
  ENDIF
  SELECT (lc_pmprjrl)
  SET ORDER TO 'PMPRJRL'
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
  *!*    =SEEK(SUBSTR(&lc_PMPrjDt..cPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(&lc_PMPrjDt..cPrj_ID,1,LEN(cPrj_ID)) +;
  *!*                 SUBSTR(&lc_PMPrjDt..cStyle,1,LEN(cStyle)) + SUBSTR(&lc_PMPrjDt..cOprt_Ctg,1,LEN(cOprt_Ctg)) +;
  *!*                 SUBSTR(&lc_PMPrjDt..cOprt_ID,1,LEN(cOprt_ID)))

  *!*    SCAN REST WHILE cPrj_Typ + cPrj_ID + cStyle + cOprt_Ctg + cOprt_ID = ;
  *!*                SUBSTR(&lc_PMPrjDt..cPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(&lc_PMPrjDt..cPrj_ID,1,LEN(cPrj_ID)) +;
  *!*                 SUBSTR(&lc_PMPrjDt..cStyle,1,LEN(cStyle)) + SUBSTR(&lc_PMPrjDt..cOprt_Ctg,1,LEN(cOprt_Ctg)) +;
  *!*                 SUBSTR(&lc_PMPrjDt..cOprt_ID,1,LEN(cOprt_ID))
  =SEEK(SUBSTR(&lc_pmprjdt..cprj_typ,1,LEN(cprj_typ)) + SUBSTR(&lc_pmprjdt..cprj_id,1,LEN(cprj_id)) +;
    SUBSTR(&lc_pmprjdt..cstyle,1,LEN(cstyle)) +STR(lnlineno,6)+ SUBSTR(&lc_pmprjdt..coprt_ctg,1,LEN(coprt_ctg)) +;
    SUBSTR(&lc_pmprjdt..coprt_id,1,LEN(coprt_id)))

  SCAN REST WHILE cprj_typ + cprj_id + cstyle+STR(LINENO,6) + coprt_ctg + coprt_id = ;
      SUBSTR(&lc_pmprjdt..cprj_typ,1,LEN(cprj_typ)) + SUBSTR(&lc_pmprjdt..cprj_id,1,LEN(cprj_id)) +;
      SUBSTR(&lc_pmprjdt..cstyle,1,LEN(cstyle)) +STR(lnlineno,6)+ SUBSTR(&lc_pmprjdt..coprt_ctg,1,LEN(coprt_ctg)) +;
      SUBSTR(&lc_pmprjdt..coprt_id,1,LEN(coprt_id))
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
    lcprdstrng = lcprdstrng + ALLTRIM(cprd_ctg) + '\' + ALLTRIM(cprd_id) +', '
  ENDSCAN
ENDIF
SELECT (lncuralias)
RETURN IIF(!EMPTY(lcprdstrng),;
  SUBSTR(lcprdstrng, 1, RAT(',', lcprdstrng)-1) + '.', SPACE(40))

*!*************************************************************
*! Name      : lfvRemove
*: Developer : Mariam Mazhar[MMT]
*: Date      : 05/10/2009
*! Purpose   : Remove button of tasks
*!*************************************************************
FUNCTION lfvremove
PARAMETERS lobranchform
PRIVATE llmaydelte, lncuralias


WITH lobranchform.loparentform
  lc_pmprjdt = .lcprjdt
  lc_pmprjrl = .lc_pmprjrl
  lcactmode  = .activemode
  lcprjhd    = .lcprjhdr
  lcpmprjntf = .lcpmprjntf
  lcprj_id   = &lcprjhd..cprj_id
  lcprj_typ  = &lcprjhd..cprj_typ
  lcstyle    = &lcprjhd..cstyle
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
  lnlineno   = &lcprjhd..LINENO
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
  lc_pmprjdt = .lcprjdt
ENDWITH

lcoprt_id = &lc_pmprjdt..coprt_id
lcoprt_ctg =  &lc_pmprjdt..coprt_ctg
lncuralias = SELECT(0)
llvoiding = &lcprjhd..cprj_stts <> 'P' .AND. !EMPTY(&lc_pmprjdt..dest_strt)
llmaydelte = .F.
SET ORDER TO pmprjrlp IN (lc_pmprjrl)
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
*!*  IF SEEK(SUBSTR(lcPrj_Typ,1,LEN(&lc_PMPrjRl..cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(&lc_PMPrjRl..cPrj_ID)) +;
*!*        SUBSTR(lcStyle,1,LEN(&lc_PMPrjRl..cStyle))+SUBSTR(lcOprt_Ctg,1,LEN(&lc_PMPrjRl..cOprt_Ctg))+;
*!*        SUBSTR(lcOprt_ID,1,LEN(&lc_PMPrjRl..cOprt_ID)), lc_PMPrjRl)
IF SEEK(SUBSTR(lcprj_typ,1,LEN(&lc_pmprjrl..cprj_typ)) + SUBSTR(lcprj_id,1,LEN(&lc_pmprjrl..cprj_id)) +;
    SUBSTR(lcstyle,1,LEN(&lc_pmprjrl..cstyle))+STR(lnlineno   ,6)+SUBSTR(lcoprt_ctg,1,LEN(&lc_pmprjrl..coprt_ctg))+;
    SUBSTR(lcoprt_id,1,LEN(&lc_pmprjrl..coprt_id)), lc_pmprjrl)
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
  IF !llvoiding
    llmaydelte = gfmodalgen("TRM38234B38006","DIALOG",lang_mfproj_remove) = 1
  ELSE
    llmaydelte = gfmodalgen("TRM38234B38006","DIALOG",lang_mfproj_void) = 1
  ENDIF
ELSE
  IF !llvoiding
    llmaydelte = gfmodalgen("TRM38208B38006","DIALOG") = 1
  ELSE
    llmaydelte = gfmodalgen("TRM38236B38006","DIALOG") = 1
  ENDIF
ENDIF
llpred = .F.
IF llmaydelte
  SELECT (lc_pmprjrl)
  lni = 0
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
  *!*    SCAN FOR cPrj_Typ +  cPrj_ID +  cStyle +  cPrd_Ctg + cPrd_ID = ;
  *!*                    SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
  *!*                    SUBSTR(lcStyle,1,LEN(cStyle))+SUBSTR(lcOprt_Ctg,1,LEN(cOprt_Ctg))+;
  *!*                    SUBSTR(lcOprt_ID,1,LEN(cOprt_ID))
  SCAN FOR cprj_typ +  cprj_id +  cstyle +STR(LINENO,6)+  cprd_ctg + cprd_id = ;
      SUBSTR(lcprj_typ,1,LEN(cprj_typ)) + SUBSTR(lcprj_id,1,LEN(cprj_id)) +;
      SUBSTR(lcstyle,1,LEN(cstyle))+STR(lnlineno,6)+SUBSTR(lcoprt_ctg,1,LEN(coprt_ctg))+;
      SUBSTR(lcoprt_id,1,LEN(coprt_id))
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
    llpred = .T.
    REPLACE cstatus WITH SUBSTR('DDS', AT(cstatus, 'SMA'), 1)
    lni = lni + 1
    DIMENSION  laoper[lnI]
    laoper[lnI] = coprt_ctg + coprt_id




  ENDSCAN
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
  *!*    DELETE FOR  cPrj_Typ +  cPrj_ID +  cStyle +  cPrd_Ctg + cPrd_ID = ;
  *!*                  SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
  *!*                  SUBSTR(lcStyle,1,LEN(cStyle))+SUBSTR(lcOprt_Ctg,1,LEN(cOprt_Ctg))+;
  *!*                  SUBSTR(lcOprt_ID,1,LEN(cOprt_ID))
  DELETE FOR  cprj_typ +  cprj_id +  cstyle +STR(LINENO,6)+  cprd_ctg + cprd_id = ;
    SUBSTR(lcprj_typ,1,LEN(cprj_typ)) + SUBSTR(lcprj_id,1,LEN(cprj_id)) +;
    SUBSTR(lcstyle,1,LEN(cstyle))+STR(lnlineno,6)+SUBSTR(lcoprt_ctg,1,LEN(coprt_ctg))+;
    SUBSTR(lcoprt_id,1,LEN(coprt_id))
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
  SET ORDER TO pmprjrl
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
  *!*    REPLACE ALL cStatus WITH SUBSTR('DDS', AT(cStatus, 'SMA'), 1);
  *!*         FOR  cPrj_Typ +  cPrj_ID +  cStyle +  cOprt_Ctg + cOprt_ID = ;
  *!*              SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
  *!*              SUBSTR(lcStyle,1,LEN(cStyle))+SUBSTR(lcOprt_Ctg,1,LEN(cOprt_Ctg))+;
  *!*              SUBSTR(lcOprt_ID,1,LEN(cOprt_ID))

  *!*    DELETE FOR  cPrj_Typ +  cPrj_ID +  cStyle +  cOprt_Ctg + cOprt_ID = ;
  *!*              SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
  *!*              SUBSTR(lcStyle,1,LEN(cStyle))+SUBSTR(lcOprt_Ctg,1,LEN(cOprt_Ctg))+;
  *!*              SUBSTR(lcOprt_ID,1,LEN(cOprt_ID))
  REPLACE ALL cstatus WITH SUBSTR('DDS', AT(cstatus, 'SMA'), 1);
    FOR  cprj_typ +  cprj_id +  cstyle+STR(LINENO,6) +  coprt_ctg + coprt_id = ;
    SUBSTR(lcprj_typ,1,LEN(cprj_typ)) + SUBSTR(lcprj_id,1,LEN(cprj_id)) +;
    SUBSTR(lcstyle,1,LEN(cstyle))+STR(lnlineno,6)+SUBSTR(lcoprt_ctg,1,LEN(coprt_ctg))+;
    SUBSTR(lcoprt_id,1,LEN(coprt_id))

  DELETE FOR  cprj_typ +  cprj_id +  cstyle +STR(LINENO,6)+  coprt_ctg + coprt_id = ;
    SUBSTR(lcprj_typ,1,LEN(cprj_typ)) + SUBSTR(lcprj_id,1,LEN(cprj_id)) +;
    SUBSTR(lcstyle,1,LEN(cstyle))+STR(lnlineno,6)+SUBSTR(lcoprt_ctg,1,LEN(coprt_ctg))+;
    SUBSTR(lcoprt_id,1,LEN(coprt_id))
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]


  SELECT (lcpmprjntf)
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
  *!*    IF SEEK(PADR(lcPrj_Typ,1)+PADR(lcPrj_ID,6)+PADR(lcStyle,12)+lcOprt_Ctg+lcoprt_id)
  *!*      SCAN REST WHILE cPrj_Typ+cPrj_ID+cStyle+COprt_Ctg+COprt_ID+CUser_ID = PADR(lcPrj_Typ,1)+PADR(lcPrj_ID,6)+PADR(lcStyle,12)+lcOprt_Ctg+lcoprt_id
  IF SEEK(PADR(lcprj_typ,1)+PADR(lcprj_id,6)+PADR(lcstyle,19)+STR(lnlineno,6)+lcoprt_ctg+lcoprt_id)
    SCAN REST WHILE cprj_typ+cprj_id+cstyle+STR(LINENO,6)+coprt_ctg+coprt_id+cuser_id = PADR(lcprj_typ,1)+PADR(lcprj_id,6)+PADR(lcstyle,19)+STR(lnlineno,6)+lcoprt_ctg+lcoprt_id
      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[end]
      REPLACE cstatus WITH SUBSTR('DDS', AT(cstatus, 'SMA'), 1)

      DELETE
    ENDSCAN
  ENDIF


  SELECT (lc_pmprjdt)

  lcremcatg = coprt_ctg
  lcprvord = ORDER()
  SET ORDER TO pmprjdt
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
  *!*    IF SEEK(SUBSTR(lcPrj_Typ,1,LEN(&lc_PMPrjDt..cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(&lc_PMPrjDt..cPrj_ID)) +;
  *!*        SUBSTR(lcStyle,1,LEN(&lc_PMPrjDt..cStyle))+SUBSTR(lcOprt_Ctg,1,LEN(&lc_PMPrjDt..cOprt_Ctg))+;
  *!*        SUBSTR(lcOprt_ID,1,LEN(&lc_PMPrjDt..cOprt_ID)),lc_PMPrjDt)
  IF SEEK(SUBSTR(lcprj_typ,1,LEN(&lc_pmprjdt..cprj_typ)) + SUBSTR(lcprj_id,1,LEN(&lc_pmprjdt..cprj_id)) +;
      SUBSTR(lcstyle,1,LEN(&lc_pmprjdt..cstyle))+STR(lnlineno,6)+SUBSTR(lcoprt_ctg,1,LEN(&lc_pmprjdt..coprt_ctg))+;
      SUBSTR(lcoprt_id,1,LEN(&lc_pmprjdt..coprt_id)),lc_pmprjdt)
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
    IF llvoiding
      REPLACE lvoid     WITH .T.,;
        cstatus   WITH SUBSTR('MMS', AT(cstatus, 'SMA'), 1),;
        laddtoaud WITH .T.
      IF !(lobranchform.loparentform.activemode = 'A')
        IF llpred
          lni = 1
          *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
          *!*            SCAN FOR cPrj_Typ +  cPrj_ID +  cStyle +  cOprt_Ctg + cOprt_ID = ;
          *!*                    SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
          *!*                    SUBSTR(lcStyle,1,LEN(cStyle))+laOper[lnI]
          SCAN FOR cprj_typ +  cprj_id +  cstyle +STR(LINENO,6)+  coprt_ctg + coprt_id = ;
              SUBSTR(lcprj_typ,1,LEN(cprj_typ)) + SUBSTR(lcprj_id,1,LEN(cprj_id)) +;
              SUBSTR(lcstyle,1,LEN(cstyle))+STR(lnlineno,6)+laoper[lnI]
            *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
            REPLACE cstatus    WITH SUBSTR('MMS', AT(cstatus, 'SMA'), 1),;
              laddtoaud  WITH .T.,;
              lschadtoad WITH .T.
            IF lni < ALEN(laoper)
              lni = lni + 1
            ENDIF
          ENDSCAN
        ENDIF
      ENDIF
      loformset.lladdtoadt = .T.

    ELSE
      lobranchform.loparentform.lnnumoflin = lobranchform.loparentform.lnnumoflin - 1
      REPLACE cstatus WITH SUBSTR('DDS', AT(cstatus, 'SMA'), 1)
      DELETE
      IF !(lobranchform.loparentform.activemode = 'A')
        IF llpred
          lni = 1
          *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
          *!*            SCAN FOR cPrj_Typ +  cPrj_ID +  cStyle +  cOprt_Ctg + cOprt_ID = ;
          *!*                    SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
          *!*                    SUBSTR(lcStyle,1,LEN(cStyle))+laOper[lnI]
          SCAN FOR cprj_typ +  cprj_id +  cstyle +STR(LINENO,6)+  coprt_ctg + coprt_id = ;
              SUBSTR(lcprj_typ,1,LEN(cprj_typ)) + SUBSTR(lcprj_id,1,LEN(cprj_id)) +;
              SUBSTR(lcstyle,1,LEN(cstyle))+STR(lnlineno,6)+laoper[lnI]
            *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
            REPLACE cstatus    WITH SUBSTR('MMS', AT(cstatus, 'SMA'), 1),;
              laddtoaud  WITH .T.,;
              lschadtoad WITH .T.
            IF lni < ALEN(laoper)
              lni = lni + 1
            ENDIF
          ENDSCAN
        ENDIF
      ENDIF
    ENDIF
    SET ORDER TO (lcprvord)
    IF !EOF()
      SKIP
    ENDIF
    IF EOF() .AND. lobranchform.loparentform.lnnumoflin > 0
      SKIP -1
    ENDIF
  ENDIF

  lobranchform.loparentform.llchngoprt = .T.
  lobranchform.loparentform.llupdstrtd = .F.

  lnelement = ASCAN(lobranchform.loparentform.laopertion, lcoprt_ctg + '\' + lcoprt_id)
  IF lnelement > 0
    IF ADEL(lobranchform.loparentform.laopertion, lnelement) > 0
      IF ALEN(lobranchform.loparentform.laopertion) > 1
        DIMENSION lobranchform.loparentform.laopertion[ALEN(loBranchForm.loparentform.laOpertion)-1]
      ELSE
        DIMENSION lobranchform.loparentform.laopertion[1]
        lobranchform.loparentform.laopertion = ''
      ENDIF
    ENDIF
  ENDIF

  IF lobranchform.loparentform.lnnumoflin > 0
    lnrecno = RECNO()
    SET ORDER TO pmprjdt
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
    *!*      IF !SEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
    *!*               SUBSTR(lcStyle,1,LEN(cStyle)) + lcRemCatg)
    IF !SEEK(SUBSTR(lcprj_typ,1,LEN(cprj_typ)) + SUBSTR(lcprj_id,1,LEN(cprj_id)) +;
        SUBSTR(lcstyle,1,LEN(cstyle)) +STR(lnlineno,6)+ lcremcatg)
      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
      lnelement = ASCAN(lobranchform.loparentform.lacategrie,  lcremcatg)
      IF lnelement > 0 .AND. ADEL(lobranchform.loparentform.lacategrie, lnelement) > 0
        IF ALEN(lobranchform.loparentform.lacategrie) > 1
          DIMENSION lobranchform.loparentform.lacategrie[ALEN(loBranchForm.loparentform.laCategrie) -1]
        ELSE
          DIMENSION lobranchform.loparentform.lacategrie[1]
          lobranchform.loparentform.lacategrie = ''
        ENDIF
      ENDIF
    ENDIF
    SET ORDER TO pmprjdts
    IF BETWEEN(lnrecno, 1, RECCOUNT())
      GO lnrecno
    ENDIF
  ELSE
    lcoprt_ctg = SPACE(3)
    lcoprt_id  = SPACE(5)
    DIMENSION lobranchform.loparentform.lacategrie[1]
    lobranchform.loparentform.lacategrie = ''
  ENDIF
  IF ALEN(lobranchform.loparentform.laopertion) < 2
    lobranchform.ariaform1.cnttaskdet.cmdpred.ENABLED =.F.
  ENDIF
ENDIF
SET ORDER TO pmprjrl IN (lc_pmprjrl)
SELECT (lncuralias)

*!*************************************************************
*! Name      : lfSavScr
*: Developer : Mariam Mazhar[MMT]
*: Date      : 05/10/2009
*! Purpose   : function to Save project
*!*************************************************************
FUNCTION lfsavscr
PARAMETERS loformset
PRIVATE lcsetdel


WITH loformset
  lc_pmprjdt = .lcprjdt
  lc_pmprjrl= .lc_pmprjrl
  lcactmode = .activemode
  lcprjhd = .lcprjhdr
  lcpmprjntf = .lcpmprjntf
  lcprj_id   = &lcprjhd..cprj_id
  lcprj_typ  = &lcprjhd..cprj_typ
  lcstyle =  &lcprjhd..cstyle
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
  lnlineno =  &lcprjhd..LINENO
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
  lc_pmprjdt = .lcprjdt
  lc_prjaudt = .lc_prjaudt
ENDWITH



LOCAL  lctrantype
GO TOP IN (lc_pmprjdt)
IF EOF(lc_pmprjdt)
  =gfmodalgen("INM38217B00000","DIALOG")
  llcsave = .F.
  RETURN .F.
ENDIF
STORE "" TO lcprog , lckey , lcapobjnam ,lcevent
=lfupdvar()



IF lcactmode  = 'E'
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
  *= gfSEEK (lcPrj_Typ +lcPrj_ID   +lcStyle,'PMPRJHD','PMPRJHD')
  = gfseek (lcprj_typ +lcprj_id   +lcstyle+STR(lnlineno ,6),'PMPRJHD','PMPRJHD')
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
  DO CASE
    *-- If the project is marked as history, confirm from the user.
  CASE loformset.ariaform1.cbostatus.VALUE  = 'H'
    IF gfmodalgen("TRM38221B38006","DIALOG") = 1
      *-- Clear all related operaions
      SELECT (lc_pmprjdt)
      REPLACE ALL cstatus WITH SUBSTR('DDS', AT(cstatus, 'SMA'), 1)
      SELECT (lc_pmprjrl)
      REPLACE ALL cstatus WITH SUBSTR('DDS', AT(cstatus, 'SMA'), 1)
      SELECT(lcpmprjntf)
      REPLACE ALL cstatus WITH SUBSTR('DDS', AT(cstatus, 'SMA'), 1)
    ELSE
      RETURN .F.
    ENDIF
    *-- If the project is cancelled, confirm from the user.
  CASE loformset.ariaform1.cbostatus.VALUE  = 'X' .AND. pmprjhd.cprj_stts <> 'X'.AND.;
      gfmodalgen("TRM38222B38006","DIALOG") = 2
    RETURN .F.
  ENDCASE
ENDIF


SELECT (lcprjhd)
WITH loformset.ariaform1
  REPLACE cprj_sdsc WITH .txtnote.VALUE ,;
    dreq_strt WITH .dtpreqstrt.VALUE ,;
    dreq_fnsh WITH .dtpreqend.VALUE,;
    cprj_stts WITH .cbostatus.VALUE ,;
    dact_fnsh WITH .dtpactend.VALUE,;
    dact_strt WITH .dtpactstrt.VALUE ,;
    dest_strt WITH .dtpeststrt.VALUE,;
    dest_strt WITH .dtpeststrt.VALUE,;
    dest_fnsh WITH .dtpestend.VALUE,;
    dclc_strt WITH .dtpcalcstrt.VALUE,;
    dclc_fnsh WITH .dtpcalcend.VALUE,;
    cpath_id  WITH .kbtmpl.keytextbox.VALUE,;
    llaststrt WITH .chklststrt.VALUE,;
    ddta_date WITH .dtpschdul.VALUE ,;
    lschedual WITH .chkreschd.VALUE
ENDWITH
=gfadd_info(lcprjhd)




SELECT (lcprjhd)
SCATTER MEMO MEMVAR
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
*IF gfSEEK (lcPrj_Typ +lcPrj_ID   +lcStyle,'PMPRJHD','PMPRJHD')
IF gfseek (lcprj_typ +lcprj_id   +lcstyle+STR(lnlineno ,6),'PMPRJHD','PMPRJHD')
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
  m.llok_stat = .T.
ELSE
  INSERT INTO pmprjhd (llok_stat) VALUES (.T.)
ENDIF
SELECT pmprjhd
GATHER MEMVAR MEMO
=gfreplace('')
=gfreplace('mprj_desc with &lcPrjHd..mprj_desc ')

SELECT pmprjdt
=gfsetorder('PMPRJDT')

SELECT pmprjrl
=gfsetorder('PMPRJRL')


IF loformset.llnewpath
  SELECT pmpthdt
  IF gfseek(m.cpath_id)
    SCAN REST WHILE cpath_id+coprt_ctg+coprt_id = m.cpath_id
      SCATTER MEMVAR MEMO
      m.rec_no = ''
      *B609555,1 MMT 03/27/2011 Task List screen gives error while completing task[Start]
      m.moprt_com = ''
      *B609555,1 MMT 03/27/2011 Task List screen gives error while completing task[END]

      *-- Default remaining duration with the estimated duration for
      *-- every operation
      *-- Add cOprt_Ctg, cCtg_Seq, cOprt_Seq
      SELECT pmprjdt
      =gfsetorder('PMPRJDT')
      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
      *!*          IF !gfSEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) + SUBSTR(lcStyle,1,LEN(cStyle))+;
      *!*                 cOprt_Ctg+cOprt_ID)
      *!*            INSERT INTO PMPRJDT;
      *!*                 (  cPrj_Typ ,   cPrj_ID  ,  cOprt_ID  ,   cOprt_Dsc,;
      *!*                    nest_dur ,   nrem_dur ,  cOprt_Ctg ,   cOprt_Res,;
      *!*                    lShw2Cust,   mOprt_Com,  cUpdtMthd ,   mNotify  ,;
      *!*                    cAdd_User,   dAdd_Date,  cAdd_Time ,   cStyle   ,;
      *!*                    cCtg_Seq ,   cOprt_Seq,  LORGINAL);
      *!*            VALUES(lcPrj_Typ ,   lcPrj_ID , m.cOprt_ID , m.cOprt_Dsc,;
      *!*                  m.nest_dur , m.nest_dur , m.cOprt_Ctg, m.cOprt_Res,;
      *!*                  m.lShw2Cust, m.mOprt_Com, m.cUpdtMthd, m.mNotify  ,;
      *!*                  oariaapplication.user_id ,   oariaapplication.SystemDate, TIME(), lcStyle    ,;
      *!*                  m.cCtg_Seq , m.cOprt_Seq,.T.)

      IF !gfseek(SUBSTR(lcprj_typ,1,LEN(cprj_typ)) + SUBSTR(lcprj_id,1,LEN(cprj_id)) + SUBSTR(lcstyle,1,LEN(cstyle))+;
          STR(lnlineno ,6)+coprt_ctg+coprt_id)

        INSERT INTO pmprjdt;
          (  cprj_typ ,   cprj_id  ,  coprt_id  ,   coprt_dsc,;
          nest_dur ,   nrem_dur ,  coprt_ctg ,   coprt_res,;
          lshw2cust,   moprt_com,  cupdtmthd ,   mnotify  ,;
          cadd_user,   dadd_date,  cadd_time ,   cstyle   ,;
          cctg_seq ,   coprt_seq,  lorginal,LINENO);
          VALUES(lcprj_typ ,   lcprj_id , m.coprt_id , m.coprt_dsc,;
          m.nest_dur , m.nest_dur , m.coprt_ctg, m.coprt_res,;
          m.lshw2cust, m.moprt_com, m.cupdtmthd, m.mnotify  ,;
          oariaapplication.user_id ,   oariaapplication.systemdate, TIME(), lcstyle    ,;
          m.cctg_seq , m.coprt_seq,.T.,lnlineno )
        *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
      ELSE
        REPLACE coprt_dsc  WITH m.coprt_dsc,;
          nest_dur   WITH m.nest_dur ,;
          nrem_dur   WITH m.nest_dur ,;
          coprt_res  WITH m.coprt_res,;
          lshw2cust  WITH m.lshw2cust,;
          moprt_com  WITH m.moprt_com,;
          cupdtmthd  WITH m.cupdtmthd,;
          mnotify    WITH m.mnotify  ,;
          cadd_user  WITH oariaapplication.user_id,;
          dadd_date  WITH oariaapplication.systemdate,;
          cadd_time  WITH TIME()     ,;
          cctg_seq   WITH cctg_seq   ,;
          coprt_seq  WITH m.coprt_seq
      ENDIF
      = gfadd_info('PMPRJDT')
      = gfreplace("")
      = gfreplace('moprt_com with m.moprt_com')
      = gfreplace('mNotify with m.mNotify')


      *-- Update SYSCHDUL
      IF !EMPTY(m.coprt_res) OR !EMPTY(m.cgroup_id)
        SELECT syschdul
        *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
        *!*          IF !gfSEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) + SUBSTR(lcstyle,1,LEN(cStyle)) +;
        *!*                   cOprt_Ctg+cOprt_ID+'O')
        *!*
        *!*            INSERT INTO SYSCHDUL;
        *!*                  (cconttype , cseqnumber , cStyle , ccont_id , csubject,;
        *!*                   ctrantype,nestdur,ccompleted,cUser_Id,COPERSTAT,lPredComp);
        *!*            VALUES(lcPrj_Typ,lcPrj_ID,lcStyle,m.cOprt_Ctg+m.cOprt_ID,;
        *!*                   m.coprt_dsc,'W',m.nest_dur,'N',;
        *!*                   IIF(EMPTY(m.cOprt_res),m.cGroup_Id,m.cOprt_res),'O',.T.)
        IF !gfseek(SUBSTR(lcprj_typ,1,LEN(cprj_typ)) + SUBSTR(lcprj_id,1,LEN(cprj_id)) + SUBSTR(lcstyle,1,LEN(cstyle)) +STR(lnlineno ,6)+;
            coprt_ctg+coprt_id+'O')

          INSERT INTO syschdul;
            (cconttype , cseqnumber , cstyle , ccont_id , csubject,;
            ctrantype,nestdur,ccompleted,cuser_id,coperstat,lpredcomp,LINENO);
            VALUES(lcprj_typ,lcprj_id,lcstyle,m.coprt_ctg+m.coprt_id,;
            m.coprt_dsc,'W',m.nest_dur,'N',;
            IIF(EMPTY(m.coprt_res),m.cgroup_id,m.coprt_res),'O',.T.,lnlineno)
          *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[END]

        ELSE
          REPLACE cuser_id   WITH IIF(EMPTY(m.coprt_res),m.cgroup_id,m.coprt_res),;
            csubject   WITH m.coprt_dsc,;
            ctrantype  WITH 'W',;
            nestdur    WITH m.nest_dur ,;
            ccompleted WITH 'N'  ,;
            nactdur    WITH m.nest_dur,;
            coperstat  WITH 'O',;
            lpredcomp  WITH .T.
        ENDIF
        =gfadd_info('SYSCHDUL')
        =gfreplace("")
        *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
        *IF gfSEEK(m.cPrj_Typ+ m.cPrj_ID+m.cStyle+m.cOprt_Ctg+m.cOprt_ID,'SYSCHDUL')
        IF gfseek(m.cprj_typ+ m.cprj_id+m.cstyle+STR(m.lineno,6)+m.coprt_ctg+m.coprt_id,'SYSCHDUL')
          *B609555,1 MMT 03/27/2011 Task List screen gives error while completing task[Start]
          IF syschdul.coperstat= 'C'
            lnoldsel = SELECT()
            SELECT syschdul
            LOCATE REST WHILE cconttype+cseqnumber+cstyle+STR(LINENO,6)+ccont_id+coperstat+cuser_id=m.cprj_typ+ m.cprj_id+m.cstyle+STR(m.lineno,6)+m.coprt_ctg+m.coprt_id FOR coperstat <> 'C'
            IF !FOUND()
              =gfseek(m.cprj_typ+ m.cprj_id+m.cstyle+STR(m.lineno,6)+m.coprt_ctg+m.coprt_id,'SYSCHDUL')
            ENDIF
            SELECT(lnoldsel)
          ENDIF
          *B609555,1 MMT 03/27/2011 Task List screen gives error while completing task[End]
          *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
          IF m.nact_dur <> 0 OR !EMPTY(m.dact_fnsh)
            REPLACE syschdul.coperstat  WITH 'C',;
              syschdul.ccompleted WITH 'Y'
          ENDIF
          IF m.lvoid
            REPLACE syschdul.coperstat WITH 'X'
          ENDIF
        ENDIF
        =gfadd_info('SYSCHDUL')
        =gfreplace("")
      ENDIF

      *-- Append all predecessor lines to the temporary relations
      *-- file.
      SELECT pmpthrl
      *-- Add cOprt_Ctg
      SCAN REST WHILE   cpath_id + coprt_ctg   +  coprt_id = ;
          m.cpath_id + m.coprt_ctg + m.coprt_id
        SCATTER MEMVAR MEMO
        m.rec_no = ''
        *-- Add cOprt_Ctg,cPrd_Ctg
        SELECT pmprjrl
        *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
        *!*          IF !gfSEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) + SUBSTR(lcStyle,1,LEN(cStyle))+;
        *!*                   m.cOprt_Ctg+m.cOprt_ID)
        *!*            INSERT INTO PMPRJRL;
        *!*                   (  cPrj_Typ ,   cPrj_ID  ,  cStyle   ,   cOprt_ID ,;
        *!*                      cPrd_ID  ,   nPrd_Lg  ,  cAdd_User,   dAdd_Date,;
        *!*                      cAdd_Time,  cOprt_Ctg , cPrd_Ctg);
        *!*             VALUES( lcPrj_Typ ,   lcPrj_ID , lcStyle   , m.cOprt_ID ,;
        *!*                    m.cPrd_ID  , m.nPrd_Lg  , oAriaApplication.User_ID    ,   oariaapplication.SystemDate,;
        *!*                      TIME()   , m.cOprt_Ctg, m.cPrd_Ctg)
        IF !gfseek(SUBSTR(lcprj_typ,1,LEN(cprj_typ)) + SUBSTR(lcprj_id,1,LEN(cprj_id)) + SUBSTR(lcstyle,1,LEN(cstyle))+STR(m.lineno,6)+;
            m.coprt_ctg+m.coprt_id)
          INSERT INTO pmprjrl;
            (  cprj_typ ,   cprj_id  ,  cstyle   ,   coprt_id ,;
            cprd_id  ,   nprd_lg  ,  cadd_user,   dadd_date,;
            cadd_time,  coprt_ctg , cprd_ctg,LINENO);
            VALUES( lcprj_typ ,   lcprj_id , lcstyle   , m.coprt_id ,;
            m.cprd_id  , m.nprd_lg  , oariaapplication.user_id    ,   oariaapplication.systemdate,;
            TIME()   , m.coprt_ctg, m.cprd_ctg,m.lineno)
          *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
        ELSE
          REPLACE cprd_id  WITH m.cprd_id ,;
            nprd_lg  WITH m.nprd_lg ,;
            cadd_user WITH oariaapplication.user_id,;
            dadd_date WITH  oariaapplication.systemdate,;
            cadd_time WITH TIME()   ,;
            cprd_ctg  WITH m.cprd_ctg
        ENDIF
        =gfadd_info('PMPRJRL')
        =gfreplace("")
      ENDSCAN
      SELECT pmpthdt
    ENDSCAN
    lc_parser  = loformset.lc_parser
    ACOPY(loformset.laholidays,laholidays)
    =lfgetestdt('PMPRJDT', 'PMPRJRL', .T.)

    IF !EMPTY(pmprjdt.coprt_res) OR !EMPTY(pmprjdt.cgroup_id)
      SELECT syschdul
      REPLACE dtrandate  WITH IIF(EMPTY(pmprjdt.dclc_strt),pmprjdt.dest_strt,pmprjdt.dclc_strt),;
        dcmpltdate WITH IIF(EMPTY(pmprjdt.dclc_fnsh),pmprjdt.dest_fnsh,pmprjdt.dclc_fnsh)

      =gfadd_info('SYSCHDUL')
      =gfreplace("")
    ENDIF

    SELECT pmprjhd
    gfreplace([dest_strt WITH m.dest_strt,] + ;
      [dest_fnsh WITH m.dest_fnsh])


  ENDIF
ENDIF

lcsetdel   = SET('DELETED')
lcsetexact = SET('EXACT')

SET DELETED OFF
SET EXACT OFF

lcprjdtupdcurs = ''
lnprjdttable = gfgetremotetable(SET("Datasession"),"PMPRJDT")
IF lnprjdttable <> 0
  lcprjdtupdcurs = oariaapplication.laremotetable[lnPrjDtTable].lccursorupdate
ENDIF

SELECT(lc_pmprjdt)
SET RELATION TO
SELECT (lc_pmprjdt)
LOCATE
SCAN
  DO CASE
    *-- Case of a new line
  CASE cstatus = 'A'
    SCATTER MEMVAR MEMO
    m.rec_no = ''
    loformset.lladdtoadt  = .T.
    SELECT pmprjdt
    m.lorginal = .T.
    INSERT INTO pmprjdt FROM MEMVAR
    =gfreplace('')
    =gfreplace('moprt_com with m.moprt_com')
    = gfreplace('mNotify with m.mNotify')
    *-- Update SYSCHDUL
    SET DELETED &lcsetdel
    IF !EMPTY(m.coprt_res) OR !EMPTY(m.cgroup_id)
      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
      *!*          IF !gfSEEK(m.cPrj_Typ+ m.cPrj_ID+m.cStyle+m.cOprt_Ctg+m.cOprt_ID,'SYSCHDUL')
      *!*            INSERT INTO SYSCHDUL;
      *!*                  (cconttype , cseqnumber ,cStyle, ccont_id , csubject,;
      *!*                   ctrantype,nestdur,ccompleted,cUser_Id,dtrandate,dcmpltdate,COPERSTAT,;
      *!*                   lPredComp);
      *!*            VALUES(m.cPrj_Typ,m.cPrj_ID,m.cStyle,m.cOprt_Ctg+m.cOprt_ID,;
      *!*                   m.coprt_dsc,'W',m.nest_dur,'N',;
      *!*                   IIF(EMPTY(m.cOprt_res),m.cGroup_Id,m.cOprt_res),;
      *!*                   IIF(EMPTY(m.dclc_strt),m.dEst_strt,m.dclc_strt),;
      *!*                   IIF(EMPTY(m.dclc_Fnsh),m.dEst_fnsh,m.dclc_Fnsh),'O',.T.)
      IF !gfseek(m.cprj_typ+ m.cprj_id+m.cstyle+STR(m.lineno,6)+m.coprt_ctg+m.coprt_id,'SYSCHDUL')
        INSERT INTO syschdul;
          (cconttype , cseqnumber ,cstyle, ccont_id , csubject,;
          ctrantype,nestdur,ccompleted,cuser_id,dtrandate,dcmpltdate,coperstat,;
          lpredcomp,LINENO);
          VALUES(m.cprj_typ,m.cprj_id,m.cstyle,m.coprt_ctg+m.coprt_id,;
          m.coprt_dsc,'W',m.nest_dur,'N',;
          IIF(EMPTY(m.coprt_res),m.cgroup_id,m.coprt_res),;
          IIF(EMPTY(m.dclc_strt),m.dest_strt,m.dclc_strt),;
          IIF(EMPTY(m.dclc_fnsh),m.dest_fnsh,m.dclc_fnsh),'O',.T.,m.lineno)
        *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]

      ELSE
        REPLACE syschdul.cuser_id   WITH IIF(EMPTY(m.coprt_res),m.cgroup_id,m.coprt_res),;
          syschdul.csubject   WITH m.coprt_dsc,;
          syschdul.ctrantype  WITH 'W',;
          syschdul.nestdur    WITH m.nest_dur ,;
          syschdul.ccompleted WITH 'N'  ,;
          syschdul.dtrandate  WITH IIF(EMPTY(m.dclc_strt),m.dest_strt,m.dclc_strt),;
          syschdul.dcmpltdate WITH IIF(EMPTY(m.dclc_fnsh),m.dest_fnsh,m.dclc_fnsh),;
          syschdul.coperstat  WITH 'O',;
          syschdul.lpredcomp  WITH .T.
      ENDIF
      =gfadd_info('SYSCHDUL')
      =gfreplace("")

      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
      *IF gfSEEK(m.cPrj_Typ+ m.cPrj_ID+m.cStyle+m.cOprt_Ctg+m.cOprt_ID,'SYSCHDUL')
      IF gfseek(m.cprj_typ+ m.cprj_id+m.cstyle+STR(m.lineno,6)+m.coprt_ctg+m.coprt_id,'SYSCHDUL')
        *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
        *B609555,1 MMT 03/27/2011 Task List screen gives error while completing task[Start]
        IF syschdul.coperstat= 'C'
          lnoldsel = SELECT()
          SELECT syschdul
          LOCATE REST WHILE cconttype+cseqnumber+cstyle+STR(LINENO,6)+ccont_id+coperstat+cuser_id=m.cprj_typ+ m.cprj_id+m.cstyle+STR(m.lineno,6)+m.coprt_ctg+m.coprt_id;
            FOR coperstat <> 'C'
          IF !FOUND()
            =gfseek(m.cprj_typ+ m.cprj_id+m.cstyle+STR(m.lineno,6)+m.coprt_ctg+m.coprt_id,'SYSCHDUL')
          ENDIF
          SELECT(lnoldsel)
        ENDIF
        *B609555,1 MMT 03/27/2011 Task List screen gives error while completing task{End}

        IF m.nact_dur <> 0 OR !EMPTY(m.dact_fnsh)
          SELECT syschdul
          REPLACE syschdul.coperstat  WITH 'C',;
            syschdul.ccompleted WITH 'Y'
        ENDIF
        IF m.lvoid
          SELECT syschdul
          REPLACE syschdul.coperstat WITH 'X'
        ENDIF
        =gfadd_info('SYSCHDUL')
        =gfreplace("")
      ENDIF
    ENDIF


    *-- An old line that is modified in the current session.
  CASE cstatus = 'M'
    *-- Add audit information
    SCATTER MEMVAR MEMO
    m.cadd_user  = oariaapplication.user_id
    m.dadd_date  = oariaapplication.systemdate
    m.cadd_time  = TIME()

    SET DELETED &lcsetdel
    IF !EMPTY(m.coprt_res) OR !EMPTY(m.cgroup_id)
      SELECT syschdul
      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
      *IF gfSEEK(m.cPrj_Typ+ m.cPrj_ID+m.cStyle+m.cOprt_Ctg+m.cOprt_ID)
      IF gfseek(m.cprj_typ+ m.cprj_id+m.cstyle+STR(m.lineno,6)+m.coprt_ctg+m.coprt_id)
        *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
        lcstatus = syschdul.coperstat
        REPLACE syschdul.cuser_id   WITH IIF(EMPTY(m.coprt_res),m.cgroup_id,m.coprt_res),;
          syschdul.csubject   WITH m.coprt_dsc,;
          syschdul.ctrantype  WITH 'W',;
          syschdul.nestdur    WITH m.nest_dur ,;
          syschdul.ccompleted WITH IIF(lcstatus = 'C','Y','N'),;
          syschdul.dtrandate  WITH IIF(EMPTY(m.dclc_strt),m.dest_strt,m.dclc_strt),;
          syschdul.dcmpltdate WITH IIF(EMPTY(m.dclc_fnsh),m.dest_fnsh,m.dclc_fnsh)
      ELSE
        *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
        *!*            INSERT INTO SYSCHDUL;
        *!*                  (cconttype , cseqnumber ,cStyle, ccont_id , csubject,;
        *!*                   ctrantype,nestdur,ccompleted,cUser_Id,dtrandate,dcmpltdate,COPERSTAT,;
        *!*                   lPredComp);
        *!*            VALUES(m.cPrj_Typ,m.cPrj_ID,m.cstyle,m.cOprt_Ctg+m.cOprt_ID,;
        *!*                   m.coprt_dsc,'W',m.nest_dur,'N',;
        *!*                   IIF(EMPTY(m.cOprt_res),m.cGroup_Id,m.cOprt_res),;
        *!*                   IIF(EMPTY(m.dclc_strt),m.dEst_strt,m.dclc_strt),;
        *!*                   IIF(EMPTY(m.dclc_Fnsh),m.dEst_fnsh,m.dclc_Fnsh),'O',.T.)
        INSERT INTO syschdul;
          (cconttype , cseqnumber ,cstyle, ccont_id , csubject,;
          ctrantype,nestdur,ccompleted,cuser_id,dtrandate,dcmpltdate,coperstat,;
          lpredcomp,LINENO);
          VALUES(m.cprj_typ,m.cprj_id,m.cstyle,m.coprt_ctg+m.coprt_id,;
          m.coprt_dsc,'W',m.nest_dur,'N',;
          IIF(EMPTY(m.coprt_res),m.cgroup_id,m.coprt_res),;
          IIF(EMPTY(m.dclc_strt),m.dest_strt,m.dclc_strt),;
          IIF(EMPTY(m.dclc_fnsh),m.dest_fnsh,m.dclc_fnsh),'O',.T.,m.lineno)
        *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
      ENDIF
      =gfadd_info('SYSCHDUL')
      =gfreplace('')
      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
      *IF gfSEEK(m.cPrj_Typ+ m.cPrj_ID+m.cStyle+m.cOprt_Ctg+m.cOprt_ID,'SYSCHDUL')
      IF gfseek(m.cprj_typ+ m.cprj_id+m.cstyle+STR(m.lineno,6)+m.coprt_ctg+m.coprt_id,'SYSCHDUL')
        *B609555,1 MMT 03/27/2011 Task List screen gives error while completing task[Start]
        IF syschdul.coperstat= 'C'
          lnoldsel = SELECT()
          SELECT syschdul
          LOCATE REST WHILE cconttype+cseqnumber+cstyle+STR(LINENO,6)+ccont_id+coperstat+cuser_id=m.cprj_typ+ m.cprj_id+m.cstyle+STR(m.lineno,6)+m.coprt_ctg+m.coprt_id;
            FOR coperstat <> 'C'
          IF !FOUND()
            =gfseek(m.cprj_typ+ m.cprj_id+m.cstyle+STR(m.lineno,6)+m.coprt_ctg+m.coprt_id,'SYSCHDUL')
          ENDIF
          SELECT(lnoldsel)
        ENDIF
        *B609555,1 MMT 03/27/2011 Task List screen gives error while completing task[End]
        *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
        IF m.nact_dur <> 0 OR !EMPTY(m.dact_fnsh)
          SELECT syschdul
          REPLACE syschdul.coperstat  WITH 'C',;
            syschdul.ccompleted WITH 'Y'
        ENDIF
        IF m.lvoid
          SELECT syschdul
          REPLACE syschdul.coperstat WITH 'X'
        ENDIF
        =gfadd_info('SYSCHDUL')
        =gfreplace("")
      ENDIF
    ENDIF

    SELECT pmprjdt
    IF BETWEEN(&lc_pmprjdt..nrecno, 1, RECCOUNT())
      GO &lc_pmprjdt..nrecno
    ENDIF
    GATHER MEMVAR MEMO
    =gfadd_info('PMPRJDT')
    =gfreplace('')
    =gfreplace('moprt_com with m.moprt_com')
    = gfreplace('mNotify with m.mNotify')
    SET DELETED OFF

    *-- An old line that is deleted in the current session.
  CASE cstatus = 'D'
    SELECT pmprjdt
    IF BETWEEN(&lc_pmprjdt..nrecno, 1, RECCOUNT())
      GO &lc_pmprjdt..nrecno
    ENDIF

    SCATTER MEMVAR MEMO
    SET DELETED &lcsetdel
    SELECT syschdul
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
    *IF gfSEEK(m.cPrj_Typ+ m.cPrj_ID+m.cStyle+m.cOprt_Ctg+m.cOprt_ID)
    IF gfseek(m.cprj_typ+ m.cprj_id+m.cstyle+STR(m.lineno,6)+m.coprt_ctg+m.coprt_id)
      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
      gfdelete()
    ENDIF
    SET DELETED OFF
    SELECT pmprjdt
    =gfdelete()
  ENDCASE

  *-- Update related records for every line.
  SELECT (lc_pmprjrl)
  *-- Add cOprt_Ctg
  SET ORDER TO 'PMPRJRL'
  lncnttt = 0
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
  *!*    SCAN FOR cPrj_Typ + cPrj_ID + cStyle + cOprt_Ctg  + cOprt_ID = ;
  *!*                         &lc_PMPrjDt..cPrj_Typ + &lc_PMPrjDt..cPrj_Id+;
  *!*                        &lc_PMPrjDt..cStyle + &lc_PMPrjDt..cOprt_Ctg + &lc_PMPrjDt..cOprt_ID
  SCAN FOR cprj_typ + cprj_id + cstyle +STR(LINENO,6)+ coprt_ctg  + coprt_id = ;
      &lc_pmprjdt..cprj_typ + &lc_pmprjdt..cprj_id+;
      &lc_pmprjdt..cstyle + STR(&lc_pmprjdt..LINENO,6)+ &lc_pmprjdt..coprt_ctg + &lc_pmprjdt..coprt_id
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
    lncnttt= lncnttt+ 1
    SCATTER MEMVAR MEMO
    SET DELETED &lcsetdel
    DO CASE
      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
      *!*        CASE gfSEEK(m.cPrj_Typ+m.cPrj_Id+m.cStyle+m.cPrd_Ctg+m.cPrd_Id+'O','SYSCHDUL')
      *!*          =gfSEEK(m.cPrj_Typ+ m.cPrj_ID+m.cStyle+m.coprt_ctg+m.coprt_id+'O','SYSCHDUL')
    CASE gfseek(m.cprj_typ+m.cprj_id+m.cstyle+STR(m.lineno,6)+m.cprd_ctg+m.cprd_id+'O','SYSCHDUL')
      =gfseek(m.cprj_typ+ m.cprj_id+m.cstyle+STR(m.lineno,6)+m.coprt_ctg+m.coprt_id+'O','SYSCHDUL')
      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
      SELECT syschdul
      gfreplace("lPredComp WITH .F.")
      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
      *!*        CASE gfSEEK(m.cPrj_Typ+m.cPrj_Id+m.cStyle+m.cPrd_Ctg+m.cPrd_Id+'C','SYSCHDUL')
      *!*          =gfSEEK(m.cPrj_Typ+ m.cPrj_ID+m.cStyle+m.coprt_ctg+m.coprt_id+'O','SYSCHDUL')
    CASE gfseek(m.cprj_typ+m.cprj_id+m.cstyle+STR(m.lineno,6)+m.cprd_ctg+m.cprd_id+'C','SYSCHDUL')
      =gfseek(m.cprj_typ+ m.cprj_id+m.cstyle+STR(m.lineno,6)+m.coprt_ctg+m.coprt_id+'O','SYSCHDUL')
      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
      SELECT syschdul
      gfreplace('lPredComp WITH .T.')
      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
      *!*        CASE gfSEEK(m.cPrj_Typ+m.cPrj_Id+m.cStyle+m.cPrd_Ctg+m.cPrd_Id+'X','SYSCHDUL')
      *!*          =gfSEEK(m.cPrj_Typ+ m.cPrj_ID+m.cStyle+m.coprt_ctg+m.coprt_id+'O','SYSCHDUL')
    CASE gfseek(m.cprj_typ+m.cprj_id+m.cstyle+STR(m.lineno,6)+m.cprd_ctg+m.cprd_id+'X','SYSCHDUL')
      =gfseek(m.cprj_typ+ m.cprj_id+m.cstyle+STR(m.lineno,6)+m.coprt_ctg+m.coprt_id+'O','SYSCHDUL')
      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
      SELECT syschdul
      gfreplace('lPredComp WITH .T.')
    OTHERWISE
      *IF gfSEEK(m.cPrj_Typ + m.cPrj_ID + m.cStyle +m.cPrd_Ctg + m.cPrd_Id,'PMPRJDT') AND EMPTY(PMPRJDT.dAct_Fnsh)
      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
      *!*          IF (!EMPTY(lcPrjDtUpdCurs) AND SEEK(m.cPrj_Typ + m.cPrj_ID + m.cStyle +m.cPrd_Ctg + m.cPrd_Id,lcPrjDtUpdCurs,'PMPRJDT') AND EMPTY(&lcPrjDtUpdCurs..dAct_Fnsh))  OR ;
      *!*             (gfSEEK(m.cPrj_Typ + m.cPrj_ID + m.cStyle +m.cPrd_Ctg + m.cPrd_Id,'PMPRJDT') AND EMPTY(PMPRJDT.dAct_Fnsh))
      *!*            =gfSEEK(m.cPrj_Typ + m.cPrj_ID + m.cStyle + m.coprt_ctg + m.coprt_id + 'O','SYSCHDUL')
      IF (!EMPTY(lcprjdtupdcurs) AND SEEK(m.cprj_typ + m.cprj_id + m.cstyle +STR(m.lineno,6)+m.cprd_ctg + m.cprd_id,lcprjdtupdcurs,'PMPRJDT') AND EMPTY(&lcprjdtupdcurs..dact_fnsh))  OR ;
          (gfseek(m.cprj_typ + m.cprj_id + m.cstyle +STR(m.lineno,6)+m.cprd_ctg + m.cprd_id,'PMPRJDT') AND EMPTY(pmprjdt.dact_fnsh))
        =gfseek(m.cprj_typ + m.cprj_id + m.cstyle +STR(m.lineno,6)+ m.coprt_ctg + m.coprt_id + 'O','SYSCHDUL')
        *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
        SELECT syschdul
        gfreplace('lPredComp WITH .F.')
      ENDIF
    ENDCASE

    SELECT  syschdul
    =gfreplace('')
    SET DELETED OFF
    SELECT (lc_pmprjrl)
    DO CASE
      *-- Case of a new line
    CASE cstatus = 'A'
      SCATTER MEMVAR MEMO
      m.rec_no = ''
      SELECT pmprjrl
      INSERT INTO pmprjrl FROM MEMVAR
      =gfadd_info('PMPRJRL')
      =gfreplace('')
      =gfreplace('llok_stat with .F.')


      *-- An old line that is modified in the current session.
    CASE cstatus = 'M'
      *-- Add audit information
      SCATTER MEMVAR MEMO
      m.cadd_user  = oariaapplication.user_id
      m.dadd_date  = oariaapplication.systemdate
      m.cadd_time  = TIME()
      SELECT pmprjrl
      IF BETWEEN(&lc_pmprjrl..nrecno, 1, RECCOUNT())
        GO &lc_pmprjrl..nrecno
      ENDIF
      GATHER MEMVAR MEMO
      =gfadd_info('PMPRJRL')
      =gfreplace('llok_stat with .F.')
      =gfreplace("")

      *-- An old line that is deleted in the current session.
    CASE cstatus = 'D'
      SELECT pmprjrl
      IF BETWEEN(&lc_pmprjrl..nrecno, 1, RECCOUNT())
        GO &lc_pmprjrl..nrecno
      ENDIF

      SCATTER MEMVAR MEMO
      SET DELETED &lcsetdel
      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
      *IF gfSEEK(m.cPrj_Typ+ m.cPrj_ID+m.cStyle+m.cOprt_ctg+m.cOprt_id,'SYSCHDUL')
      IF gfseek(m.cprj_typ+ m.cprj_id+m.cstyle+STR(m.lineno,6)+m.coprt_ctg+m.coprt_id,'SYSCHDUL')
        *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
        SELECT syschdul
        REPLACE syschdul.lpredcomp WITH .T.
        =gfadd_info('SYSCHDUL')
        =gfreplace('')
      ENDIF
      SET DELETE OFF
      SELECT pmprjrl
      =gfdelete()
    ENDCASE
  ENDSCAN
  IF lncnttt = 0
  ENDIF

  SELECT (lcpmprjntf)
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
  *!*    IF SEEK(EVALUATE(lc_PMPrjDt+".cPrj_Typ+"+lc_PMPrjDt+".cPrj_ID+"+lc_PMPrjDt+".cStyle+"+lc_PMPrjDt+".COprt_Ctg+"+lc_PMPrjDt+".cOprt_ID"))
  *!*      SCAN REST WHILE cPrj_Typ+cPrj_ID+cStyle+COprt_Ctg+COprt_ID+CUser_ID = EVALUATE(lc_PMPrjDt+".cPrj_Typ+"+lc_PMPrjDt+".cPrj_ID+"+lc_PMPrjDt+".cStyle+"+lc_PMPrjDt+".COprt_Ctg+"+lc_PMPrjDt+".cOprt_ID")
  IF SEEK(EVALUATE(lc_pmprjdt+".cPrj_Typ+"+lc_pmprjdt+".cPrj_ID+"+lc_pmprjdt+".cStyle+STR("+lc_pmprjdt+".LINENO,6)+"+lc_pmprjdt+".COprt_Ctg+"+lc_pmprjdt+".cOprt_ID"))
    SCAN REST WHILE cprj_typ+cprj_id+cstyle+STR(LINENO,6)+coprt_ctg+coprt_id+cuser_id = EVALUATE(lc_pmprjdt+".cPrj_Typ+"+lc_pmprjdt+".cPrj_ID+"+lc_pmprjdt+".cStyle+STR("+lc_pmprjdt+".LINENO,6)+"+lc_pmprjdt+".COprt_Ctg+"+lc_pmprjdt+".cOprt_ID")
      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
      DO CASE
        *-- Case of a new line
      CASE cstatus = 'A'
        SCATTER MEMVAR
        m.rec_no = ''
        SELECT pmprjntf
        *INSERT INTO PMPRJNTF FROM MEMVAR
        gfappend('',.T.)
        =gfadd_info('PMPRJNTF')
        =gfreplace('')
        =gfreplace('llok_stat with .F.')

        *-- An old line that is modified in the current session.
      CASE cstatus = 'M'
        * Add audit information
        SCATTER MEMVAR
        m.cadd_user  = oariaapplication.user_id
        m.dadd_date  = oariaapplication.systemdate
        m.cadd_time  = TIME()
        SELECT pmprjntf
        *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
        *IF SEEK(m.cPrj_Typ+m.cPrj_ID+m.cStyle+m.COprt_Ctg+m.COprt_ID+m.CUser_ID)
        IF SEEK(m.cprj_typ+m.cprj_id+m.cstyle+STR(M.lineno,6)+m.coprt_ctg+m.coprt_id+m.cuser_id)
          *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
          GATHER MEMVAR
        ENDIF
        =gfadd_info('PMPRJNTF')
        =gfreplace('')
        =gfreplace('llok_stat with .F.')
        *-- An old line that is deleted in the current session.
      CASE cstatus = 'D'
        SCATTER MEMVAR
        SELECT pmprjntf
        *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
        *IF gfSEEK(m.cPrj_Typ+m.cPrj_ID+m.cStyle+m.COprt_Ctg+m.COprt_ID+m.CUser_ID)
        IF gfseek(m.cprj_typ+m.cprj_id+m.cstyle+STR(m.lineno,6)+m.coprt_ctg+m.coprt_id+m.cuser_id)
          *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
          gfdelete()
        ENDIF
      ENDCASE
    ENDSCAN
  ENDIF

  SELECT (lc_pmprjdt)
ENDSCAN
SET DELETED &lcsetdel
SET EXACT &lcsetexact

STORE "" TO lcprog , lckey , lcapobjnam ,lcevent
=lfupdvar()
IF !USED('AUDTRAIL')
  =gfopentable('AUDTRAIL' , 'AUDTRAIL' , 'SH')
ENDIF

*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
*IF !gfSEEK(PADR(lcProg,10)+PADR(lcKey,20),'AUDTRAIL') OR loFormSet.llAddToAdt
IF !gfseek(PADR(lcprog,10)+lckey,'AUDTRAIL') OR loformset.lladdtoadt
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
  SELECT (lc_pmprjdt)
  SCAN
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
    *   IF gfSEEK(&lc_PMPrjDt..cPrj_Typ + &lc_PMPrjDt..cPrj_ID + &lc_PMPrjDt..cStyle + &lc_PMPrjDt..cOprt_Ctg+&lc_PMPrjDt..cOprt_ID,'SYSCHDUL')
    IF gfseek(&lc_pmprjdt..cprj_typ + &lc_pmprjdt..cprj_id + &lc_pmprjdt..cstyle +STR(&lc_pmprjdt..LINENO,6)+ &lc_pmprjdt..coprt_ctg+&lc_pmprjdt..coprt_id,'SYSCHDUL')
      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
      IF !(&lc_pmprjdt..lorginal)
        SELECT syschdul
        lcstauts = syschdul.coperstat
        *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
        *!*          LOCATE REST WHILE cconttype+cseqnumber+cStyle+ccont_id+coperstat+cuser_id =;
        *!*                            &lc_PMPrjDt..cPrj_Typ + &lc_PMPrjDt..cPrj_ID + &lc_PMPrjDt..cStyle + &lc_PMPrjDt..cOprt_Ctg+&lc_PMPrjDt..cOprt_ID ;
        *!*                           FOR COPERSTAT <> lcStauts
        LOCATE REST WHILE cconttype+cseqnumber+cstyle+STR(LINENO,6)+ccont_id+coperstat+cuser_id =;
          &lc_pmprjdt..cprj_typ + &lc_pmprjdt..cprj_id + &lc_pmprjdt..cstyle +;
          STR(&lc_pmprjdt..LINENO,6)+ &lc_pmprjdt..coprt_ctg+&lc_pmprjdt..coprt_id ;
          FOR coperstat <> lcstauts
        *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
        IF !FOUND()
          *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
          *=gfSEEK(&lc_PMPrjDt..cPrj_Typ + &lc_PMPrjDt..cPrj_ID + &lc_PMPrjDt..cStyle + &lc_PMPrjDt..cOprt_Ctg+&lc_PMPrjDt..cOprt_ID+lcStauts,'SYSCHDUL')
          =gfseek(&lc_pmprjdt..cprj_typ + &lc_pmprjdt..cprj_id + &lc_pmprjdt..cstyle +STR(&lc_pmprjdt..LINENO,6)+ &lc_pmprjdt..coprt_ctg+&lc_pmprjdt..coprt_id+lcstauts,'SYSCHDUL')
          *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
        ENDIF
      ENDIF

      IF syschdul.lpredcomp AND &lc_pmprjdt..cstatus $ 'AM' AND &lc_pmprjdt..laddtoaud
        IF syschdul.coperstat = 'C'
          *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
          *!*            IF (!EMPTY(lcPrjDtUpdCurs) AND ;
          *!*                SEEK(&lc_PMPrjDt..cPrj_Typ + &lc_PMPrjDt..cPrj_ID +&lc_PMPrjDt..cStyle +&lc_PMPrjDt..cOprt_Ctg+ ;
          *!*                &lc_PMPrjDt..cOprt_ID,lcPrjDtUpdCurs,'PMPRJDT') AND SEEK(&lc_PMPrjDt..cPrj_Typ + &lc_PMPrjDt..cPrj_ID +&lc_PMPrjDt..cStyle +&lc_PMPrjDt..cOprt_Ctg+ ;
          *!*                &lc_PMPrjDt..cOprt_ID,'PMPRJDT','PMPRJDT')) OR ;
          *!*                (gfSEEK(&lc_PMPrjDt..cPrj_Typ + &lc_PMPrjDt..cPrj_ID+&lc_PMPrjDt..cStyle+;
          *!*                    &lc_PMPrjDt..cOprt_Ctg+&lc_PMPrjDt..cOprt_ID,'PMPRJDT','PMPRJDT'))
          IF (!EMPTY(lcprjdtupdcurs) AND ;
              SEEK(&lc_pmprjdt..cprj_typ + &lc_pmprjdt..cprj_id +&lc_pmprjdt..cstyle +STR(&lc_pmprjdt..LINENO,6)+&lc_pmprjdt..coprt_ctg+ ;
              &lc_pmprjdt..coprt_id,lcprjdtupdcurs,'PMPRJDT') AND SEEK(&lc_pmprjdt..cprj_typ + &lc_pmprjdt..cprj_id +&lc_pmprjdt..cstyle +STR(&lc_pmprjdt..LINENO,6)+&lc_pmprjdt..coprt_ctg+ ;
              &lc_pmprjdt..coprt_id,'PMPRJDT','PMPRJDT')) OR ;
              (gfseek(&lc_pmprjdt..cprj_typ + &lc_pmprjdt..cprj_id+&lc_pmprjdt..cstyle+STR(&lc_pmprjdt..LINENO,6)+;
              &lc_pmprjdt..coprt_ctg+&lc_pmprjdt..coprt_id,'PMPRJDT','PMPRJDT'))
            *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]

            SELECT pmprjdt
            gfreplace("LAUDTUPD WITH .T.")
          ENDIF
        ENDIF
        SELECT (lc_pmprjdt)
        SCATTER MEMVAR MEMO
        ldest_fnsh = IIF(EMPTY(m.dact_fnsh),IIF(EMPTY(m.dclc_fnsh),m.dest_fnsh,m.dclc_fnsh),m.dact_fnsh)
        lnrem_dur  = IIF(EMPTY(m.nact_dur),m.nrem_dur,0)
        lcinform = ""
        =lfupdadtrl()
        SELECT (lc_prjaudt)
        =gfaudtrl(loformset , lcprog  , lckey , lcapobjnam , lcevent, lcinform)
        *E302741,1 WAM 08/25/2010 Convert Audit Trail file into SQL
        *!*	        SELECT AUDTRAIL
        *!*	        =gfAdd_Info('AUDTRAIL')
        *!*	        =gfReplace('')
        *!*	        =gfTableUpdate()
        *E302741,1 WAM 08/25/2010 Convert Audit Trail file into SQL
      ENDIF
    ELSE
      IF &lc_pmprjdt..cstatus $ 'AM' AND &lc_pmprjdt..laddtoaud AND;
          (!EMPTY(&lc_pmprjdt..coprt_res) OR !EMPTY(&lc_pmprjdt..cgroup_id))
        SELECT (lc_pmprjdt)
        SCATTER MEMVAR MEMO
        ldest_fnsh = IIF(EMPTY(m.dact_fnsh),IIF(EMPTY(m.dclc_fnsh),m.dest_fnsh,m.dclc_fnsh),m.dact_fnsh)
        lnrem_dur  = IIF(EMPTY(m.nact_dur),m.nrem_dur,0)
        lcinform = ""
        =lfupdadtrl()
        SELECT (lc_prjaudt)
        =gfaudtrl(loformset , lcprog  , lckey , lcapobjnam , lcevent, lcinform)
        *E302741,1 WAM 08/25/2010 Convert Audit Trail file into SQL
        *!*	        SELECT AUDTRAIL
        *!*	        =gfAdd_Info('AUDTRAIL')
        *!*	        =gfReplace('')
        *!*	        =gfTableUpdate()
        *E302741,1 WAM 08/25/2010 (End)
      ENDIF
    ENDIF
    SELECT pmprjdt
    =gfsetorder('PMPRJDT')
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
    *!*      IF (!EMPTY(lcPrjDtUpdCurs) AND ;
    *!*                SEEK(&lc_PMPrjDt..cPrj_Typ + &lc_PMPrjDt..cPrj_ID +&lc_PMPrjDt..cStyle +&lc_PMPrjDt..cOprt_Ctg+ ;
    *!*                &lc_PMPrjDt..cOprt_ID,lcPrjDtUpdCurs,'PMPRJDT') AND SEEK(&lc_PMPrjDt..cPrj_Typ + &lc_PMPrjDt..cPrj_ID +&lc_PMPrjDt..cStyle +&lc_PMPrjDt..cOprt_Ctg+ ;
    *!*                &lc_PMPrjDt..cOprt_ID,'PMPRJDT','PMPRJDT')) OR  gfSEEK(&lc_PMPrjDt..cPrj_Typ + &lc_PMPrjDt..cPrj_ID+&lc_PMPrjDt..cStyle+;
    *!*              &lc_PMPrjDt..cOprt_Ctg+&lc_PMPrjDt..cOprt_ID,'PMPRJDT')
    IF (!EMPTY(lcprjdtupdcurs) AND ;
        SEEK(&lc_pmprjdt..cprj_typ + &lc_pmprjdt..cprj_id +&lc_pmprjdt..cstyle +STR(&lc_pmprjdt..LINENO,6)+&lc_pmprjdt..coprt_ctg+ ;
        &lc_pmprjdt..coprt_id,lcprjdtupdcurs,'PMPRJDT') AND SEEK(&lc_pmprjdt..cprj_typ + &lc_pmprjdt..cprj_id +&lc_pmprjdt..cstyle +STR(&lc_pmprjdt..LINENO,6)+&lc_pmprjdt..coprt_ctg+ ;
        &lc_pmprjdt..coprt_id,'PMPRJDT','PMPRJDT')) OR  gfseek(&lc_pmprjdt..cprj_typ + &lc_pmprjdt..cprj_id+&lc_pmprjdt..cstyle++STR(&lc_pmprjdt..LINENO,6)+;
        &lc_pmprjdt..coprt_ctg+&lc_pmprjdt..coprt_id,'PMPRJDT')
      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
      SELECT pmprjdt
      gfreplace("lAddToAud WITH .F.")

    ENDIF
  ENDSCAN
ENDIF


IF pmprjhd.cprj_stts = 'C'
  =lfcomschdl()
ELSE
  IF pmprjhd.cprj_stts = 'X'
    STORE "" TO lcprog , lckey
    =lfupdvar()
    SELECT syschdul
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
    *SCAN REST WHILE cconttype+cseqnumber+cStyle+ccont_id+coperstat+cuser_id = lcPrj_Typ+lcPrj_ID+lcStyle
    SCAN REST WHILE cconttype+cseqnumber+cstyle+STR(LINENO,6)+ccont_id+coperstat+cuser_id = lcprj_typ+lcprj_id+lcstyle+STR(lnlineno ,6)
      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
      REPLACE coperstat  WITH 'X'
      =gfadd_info('SYSCHDUL')
      =gfreplace("")
    ENDSCAN
    SELECT audtrail
    SCAN FOR capobjnam+KEY+caudtralid = PADR(lcprog,10)+PADR(lckey,20)
      gfdelete()
    ENDSCAN
  ENDIF
ENDIF

llexitloop = .T.
llenbsave  = .F.
llpathsel  = .F.
SELECT pmprjhd
gfreplace("llok_stat WITH .F.")

=gftableupdate()
SELECT pmprjdt
=gftableupdate()
SELECT syschdul
=gftableupdate()
SELECT audtrail
=gftableupdate()
SELECT pmprjrl
=gftableupdate()
SELECT pmprjntf
=gftableupdate()

*!*************************************************************
*! Name      : lfComSchdl
*: Developer : Mariam Mazhar[MMT]
*: Date      : 05/10/2009
*! Purpose   : Complete scheduled
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfComSchdl()
*!*************************************************************
FUNCTION lfcomschdl


STORE "" TO lcprog , lckey , lcapobjnam ,lcevent
=lfupdvar()
SELECT syschdul
=gfsetorder('Coprusr')
IF gfseek(lcprj_typ+lcprj_id)
  SELECT (lc_pmprjdt)
  lcprvord = ORDER()
  SELECT syschdul
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
  *SCAN REST WHILE cconttype+cseqnumber+cStyle+ccont_id+coperstat+cuser_id = lcPrj_Typ+lcPrj_ID+lcStyle
  SCAN REST WHILE cconttype+cseqnumber+cstyle+STR(LINENO,6)+ccont_id+coperstat+cuser_id = lcprj_typ+lcprj_id+lcstyle+STR(lnlineno ,6)
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
    REPLACE ccompleted WITH 'Y',;
      coperstat  WITH 'C'
    =gfreplace('')
    SELECT (lc_pmprjdt)
    SET ORDER TO pmprjusr
    IF SEEK(SUBSTR(lcprj_typ,1,LEN(cprj_typ))+SUBSTR(lcprj_id,1,LEN(cprj_id))+syschdul.ccont_id)
      SELECT pmprjdt
      =gfsetorder("PMPRJDT")
      SELECT (lc_pmprjdt)
      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
      *!*        IF SYSCHDUL.COPERSTAT = 'C' AND ((!EMPTY(lcPrjDtUpdCurs) AND ;
      *!*                SEEK(&lc_PMPrjDt..cPrj_Typ + &lc_PMPrjDt..cPrj_ID +&lc_PMPrjDt..cStyle +&lc_PMPrjDt..cOprt_Ctg+ ;
      *!*                &lc_PMPrjDt..cOprt_ID,lcPrjDtUpdCurs,'PMPRJDT') AND SEEK(&lc_PMPrjDt..cPrj_Typ + &lc_PMPrjDt..cPrj_ID +&lc_PMPrjDt..cStyle +&lc_PMPrjDt..cOprt_Ctg+ ;
      *!*                &lc_PMPrjDt..cOprt_ID,'PMPRJDT','PMPRJDT')) OR ;
      *!*                gfSEEK(&lc_PMPrjDt..cPrj_Typ + &lc_PMPrjDt..cPrj_ID+;
      *!*                &lc_PMPrjDt..cStyle+&lc_PMPrjDt..cOprt_Ctg+&lc_PMPrjDt..cOprt_ID,'PMPRJDT'))
      IF syschdul.coperstat = 'C' AND ((!EMPTY(lcprjdtupdcurs) AND ;
          SEEK(&lc_pmprjdt..cprj_typ + &lc_pmprjdt..cprj_id +&lc_pmprjdt..cstyle +STR(&lc_pmprjdt..LINENO,6)+&lc_pmprjdt..coprt_ctg+ ;
          &lc_pmprjdt..coprt_id,lcprjdtupdcurs,'PMPRJDT') AND SEEK(&lc_pmprjdt..cprj_typ + &lc_pmprjdt..cprj_id +&lc_pmprjdt..cstyle +STR(&lc_pmprjdt..LINENO,6)+&lc_pmprjdt..coprt_ctg+ ;
          &lc_pmprjdt..coprt_id,'PMPRJDT','PMPRJDT')) OR ;
          gfseek(&lc_pmprjdt..cprj_typ + &lc_pmprjdt..cprj_id+;
          &lc_pmprjdt..cstyle+STR(&lc_pmprjdt..LINENO,6)+&lc_pmprjdt..coprt_ctg+&lc_pmprjdt..coprt_id,'PMPRJDT'))
        *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
        IF pmprjdt.laudtupd
          LOOP
        ENDIF
      ENDIF

      SCATTER MEMVAR MEMO
      ldest_fnsh = IIF(EMPTY(m.dact_fnsh),IIF(EMPTY(m.dclc_fnsh),m.dest_fnsh,m.dclc_fnsh),m.dact_fnsh)
      lnrem_dur  = IIF(EMPTY(m.nact_dur),m.nrem_dur,0)
      lcinform = ""
      =lfupdadtrl()
      SELECT (lc_prjaudt)
      =gfaudtrl(loformset , lcprog  , lckey , lcapobjnam , lcevent, lcinform)
    ENDIF
  ENDSCAN
  SELECT (lc_pmprjdt)
  SET ORDER TO &lcprvord
ENDIF

*!*************************************************************
*! Name      : lpDelScr
*: Developer : Mariam Mazhar[MMT]
*: Date      : 05/10/2009
*! Purpose   : Valid function for push button < Delete > (pbDelete).
*!             Saves current modifications to the file.
*!*************************************************************
*! Calls     : lpshow()
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lpDelScr()
*!*************************************************************
FUNCTION lpdelscr
PARAMETERS loformset
PRIVATE ladelrec, lncuralias

lncuralias = SELECT(0)

WITH loformset
  lc_pmprjdt = .lcprjdt
  lc_pmprjrl = .lc_pmprjrl
  lcactmode  = .activemode
  lcprjhd    = .lcprjhdr
  lcpmprjntf = .lcpmprjntf
  lcprj_id   = &lcprjhd..cprj_id
  lcprj_typ  = &lcprjhd..cprj_typ
  lcstyle    = &lcprjhd..cstyle
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
  lnlineno  = &lcprjhd..LINENO
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
  lc_pmprjdt = .lcprjdt
ENDWITH

SELECT pmprjhd
IF RLOCK()
  REPLACE llok_stat WITH .T.
  =gfreplace('')
  UNLOCK
  SELECT pmprjdt
  =gfsetorder('PMPRJDT')
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
  *!*    =gfSeek(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
  *!*                       SUBSTR(lcStyle,1,LEN(cStyle)))
  *!*    *-- Scan through all the records of the same project ID in the details file.
  *!*    SCAN REST WHILE cprj_typ+cprj_id+cstyle+coprt_ctg+coprt_id  = SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
  *!*                       SUBSTR(lcStyle,1,LEN(cStyle))
  =gfseek(SUBSTR(lcprj_typ,1,LEN(cprj_typ)) + SUBSTR(lcprj_id,1,LEN(cprj_id)) +;
    SUBSTR(lcstyle,1,LEN(cstyle))+STR(lnlineno,6))
  *-- Scan through all the records of the same project ID in the details file.
  SCAN REST WHILE cprj_typ+cprj_id+cstyle+STR(LINENO,6)+coprt_ctg+coprt_id  = SUBSTR(lcprj_typ,1,LEN(cprj_typ)) + SUBSTR(lcprj_id,1,LEN(cprj_id)) +;
      SUBSTR(lcstyle,1,LEN(cstyle))+STR(lnlineno  ,6)
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
    lcoprt_ctg = coprt_ctg
    lcoprt_id  = coprt_id


    SELECT pmprjrl
    =gfsetorder('PMPRJRL')
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
    *!*      =gfSeek(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
    *!*               SUBSTR(lcStyle,1,LEN(cStyle))+SUBSTR(lcOprt_Ctg,1,LEN(cOprt_Ctg))+;
    *!*               SUBSTR(lcOprt_ID,1,LEN(cOprt_ID)))
    *!*
    *!*      SCAN REST WHILE  cprj_typ+cprj_id+cstyle+coprt_ctg+coprt_id  = ;
    *!*               SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
    *!*               SUBSTR(lcStyle,1,LEN(cStyle))+SUBSTR(lcOprt_Ctg,1,LEN(cOprt_Ctg))+;
    *!*               SUBSTR(lcOprt_ID,1,LEN(cOprt_ID))
    =gfseek(SUBSTR(lcprj_typ,1,LEN(cprj_typ)) + SUBSTR(lcprj_id,1,LEN(cprj_id)) +;
      SUBSTR(lcstyle,1,LEN(cstyle))+STR(lnlineno  ,6)+SUBSTR(lcoprt_ctg,1,LEN(coprt_ctg))+;
      SUBSTR(lcoprt_id,1,LEN(coprt_id)))

    SCAN REST WHILE  cprj_typ+cprj_id+cstyle+STR(LINENO,6)+coprt_ctg+coprt_id  = ;
        SUBSTR(lcprj_typ,1,LEN(cprj_typ)) + SUBSTR(lcprj_id,1,LEN(cprj_id)) +;
        SUBSTR(lcstyle,1,LEN(cstyle))+STR(lnlineno  ,6)+SUBSTR(lcoprt_ctg,1,LEN(coprt_ctg))+;
        SUBSTR(lcoprt_id,1,LEN(coprt_id))
      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
      =gfdelete()

    ENDSCAN


    SELECT pmprjntf
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
    *!*      IF gfSEEK(PMPRJDT.CPRJ_TYP+PMPRJDT.CPRJ_ID+PMPRJDT.CSTYLE+lcOprt_Ctg+lcOprt_ID)
    *!*
    *!*        SCAN REST WHILE cPrj_Typ+cPrj_ID+cStyle+COprt_Ctg+COprt_ID+CUser_ID =;
    *!*                          PMPRJDT.CPRJ_TYP+PMPRJDT.CPRJ_ID+PMPRJDT.CSTYLE+lcOprt_Ctg+lcOprt_ID
    IF gfseek(pmprjdt.cprj_typ+pmprjdt.cprj_id+pmprjdt.cstyle+STR(pmprjdt.LINENO,6)+lcoprt_ctg+lcoprt_id)

      SCAN REST WHILE cprj_typ+cprj_id+cstyle+STR(LINENO,6)+coprt_ctg+coprt_id+cuser_id =;
          pmprjdt.cprj_typ+pmprjdt.cprj_id+pmprjdt.cstyle+STR(pmprjdt.LINENO,6)+lcoprt_ctg+lcoprt_id
        *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
        =gfdelete()
      ENDSCAN
    ENDIF



    SELECT pmprjdt
    *-- for every line on the file, delete related predecessors.
    =gfdelete()
  ENDSCAN

  SELECT syschdul
  =gfsetorder('Coprusr')
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
  *!*    =gfSeek(lcPrj_Typ + lcPrj_ID + lcStyle)
  *!*    SCAN REST WHILE  cconttype+cseqnumber+cStyle+ccont_id+coperstat+cuser_id = lcPrj_Typ + lcPrj_ID + lcStyle
  =gfseek(lcprj_typ + lcprj_id + lcstyle+STR(lnlineno ,6))
  SCAN REST WHILE  cconttype+cseqnumber+cstyle+STR(LINENO ,6)+ccont_id+coperstat+cuser_id = lcprj_typ + lcprj_id + lcstyle+STR(lnlineno ,6)
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
    =gfdelete()
  ENDSCAN

  SELECT pmprjhd
  =gfdelete()

  STORE "" TO lcprog , lckey , lcapobjnam ,lcevent

  =lfupdvar()
  IF !USED('AUDTRAIL')
    =gfopentable('AUDTRAIL' , 'AUDTRAIL' , 'SH')
  ENDIF
  SELECT audtrail
  SCAN FOR capobjnam+KEY+caudtralid = PADR(lcprog,10)+PADR(lckey,20)
    =gfdelete()
  ENDSCAN
  SELECT pmprjhd
  =gftableupdate()
  SELECT pmprjdt
  =gftableupdate()
  SELECT syschdul
  =gftableupdate()
  SELECT audtrail
  =gftableupdate()
  SELECT pmprjrl
  =gftableupdate()
  SELECT pmprjntf
  =gftableupdate()
  *-- Go back to 'S'elect mode
ENDIF
SELECT (lncuralias)


*!*************************************************************
*! Name      : lfvPath_ID
*: Developer : Mariam Mazhar[MMT]
*: Date      : 05/10/2009
*! Purpose   : Valid function for get field cPath_ID
*!*************************************************************
*! Calls              :  lfVldKey()
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvPath_ID()
*!*************************************************************
FUNCTION lfvpath_id
PARAMETERS loformset
PRIVATE lcoldsmode
WITH loformset.ariaform1
  llbrowse  = .kbtmpl.selectedfrombrowse
  lc_pmprjdt = loformset.lcprjdt
  m.cpath_id = PADR(ALLTRIM(.kbtmpl.keytextbox.VALUE),4)
  lcoldval  = .kbtmpl.keytextbox.oldvalue
  lcprjhd = loformset.lcprjhdr
  IF llbrowse .OR. (!EMPTY(m.cpath_id) AND !gfseek(m.cpath_id,'PMPTHHD')) &&.OR. m.cPath_ID <> lcOldVal

    SELECT pmpthhd
    =gfseek('')
    *E302650,2 MMT 12/24/2009 Modify Project Screen to enable user to create project per line[Start]
    *!*      lcBrFields = "Cpath_id  :H='"+LANG_MFPROJ_TMPID+"'," +;
    *!*               "Cpath_dsc :H='"+LANG_MFPROJ_TMPNAME+"'," +;
    *!*               "Mpath_com :H='Mpath_com'," +;
    *!*               "Cadd_user  :H='Cadd_user' ," +;
    *!*               "Cadd_time :H='Cadd_time'," +;
    *!*               "Dadd_date  :H='Dadd_date'," +;
    *!*               "Llok_stat  :H='Llok_stat'," +;
    *!*               "Clok_user  :H='Clok_user'," +;
    *!*               "Dlok_date  :H='Dlok_date'," +;
    *!*               "Clok_time   :H='Clok_time'"
    lcbrfields = "Cpath_id  :H='"+lang_mfproj_tmpid+"'," +;
      "Cpath_dsc :H='"+lang_mfproj_tmpname+"'"
    *E302650,2 MMT 12/24/2009 Modify Project Screen to enable user to create project per line[End]
    DIMENSION lafields[1]
    SELECT pmpthhd
    LOCATE
    llsel = gfbrows('','Cpath_id','laFields',lang_mfproj_tmplate)
    IF llsel
      .txttmpldesc.VALUE = pmpthhd.cpath_dsc
      llpathsel = .T.
      .kbtmpl.keytextbox.VALUE = pmpthhd.cpath_id
      loformset.llnewpath = RECCOUNT(lc_pmprjdt) = 0 .OR. m.cpath_id <> lcoldval
      .txttmpldesc.ENABLED = .F.
    ELSE
      loformset.llnewpath  = .F.
      .txttmpldesc.VALUE  = SPACE(40)
      .kbtmpl.keytextbox.VALUE = ''
      .txttmpldesc.VALUE  = ''
    ENDIF
    IF .kbtmpl.selectedfrombrowse
      .kbtmpl.selectedfrombrowse= .F.
    ENDIF
  ELSE
    .kbtmpl.selectedfrombrowse = .F.
    .txttmpldesc.VALUE = pmpthhd.cpath_dsc
    loformset.llnewpath = RECCOUNT(lc_pmprjdt) = 0 .OR. m.cpath_id <> lcoldval
    .txttmpldesc.ENABLED = .F.
  ENDIF
  REPLACE cpath_id WITH PADR(ALLTRIM(.kbtmpl.keytextbox.VALUE),4) IN (lcprjhd)
ENDWITH
*!*************************************************************
*! Name      : lfvact_dat
*: Developer : Mariam Mazhar[MMT]
*: Date      : 05/10/2009
*! Purpose   : Valid function for  act. date
*!*************************************************************
FUNCTION lfvact_dat
PARAMETERS loformset,lcsrc
lc_pmprjdt = loformset.loparentform.lcprjdt
PRIVATE ldcurobj
lncuralias = SELECT(0)
SELECT (lc_pmprjdt)
WITH loformset.ariaform1
  IF lcsrc = 'S'
    lccurfld  = .dtpactstr.VALUE
    lcoldval= .dtpactstr.oldvalue
    REPLACE &lc_pmprjdt..dact_strt WITH .dtpactstr.VALUE,;
      &lc_pmprjdt..cstatus   WITH SUBSTR('MMA', AT(cstatus, 'SMA'), 1)
  ELSE
    lccurfld = .dtpactend.VALUE
    lcoldval = .dtpactend.oldvalue
    REPLACE &lc_pmprjdt..dact_fnsh WITH .dtpactend.VALUE ,;
      &lc_pmprjdt..cstatus   WITH SUBSTR('MMA', AT(cstatus, 'SMA'), 1)
  ENDIF

  lcpredopr = &lc_pmprjdt..coprt_ctg+&lc_pmprjdt..coprt_id
  IF lccurfld <> lcoldval
    IF !EMPTY(DTOS(.dtpactstr.VALUE))     .AND. ;
        !EMPTY(DTOS(.dtpactend.VALUE ))     .AND. ;
        .dtpactstr.VALUE > .dtpactend.VALUE .AND. ;
        gfmodalgen("TRM38220B00000","DIALOG") = 1

      IF lcsrc = 'S'
        .dtpactstr.VALUE =  lcoldval
        REPLACE &lc_pmprjdt..dact_strt WITH lcoldval
      ELSE
        .dtpactend.VALUE =  lcoldval
        REPLACE &lc_pmprjdt..dact_fnsh WITH lcoldval
      ENDIF

    ELSE
      SELECT (lc_pmprjdt)
      lnrecno = RECNO()
      lcpredec =.txtpred.VALUE
      SCAN FOR lcpredec $ (coprt_ctg + '\' + coprt_id)
        IF EMPTY(dact_strt)
          =gfmodalgen("INM38245B00000","DIALOG")
          IF BETWEEN(lnrecno, 1, RECCOUNT())
            GO lnrecno
          ENDIF
          IF lcsrc = 'S'
            .dtpactstr.VALUE ={}
            REPLACE &lc_pmprjdt..dact_strt WITH {},;
              &lc_pmprjdt..cstatus   WITH SUBSTR('MMA', AT(cstatus, 'SMA'), 1)

          ELSE
            .dtpactend.VALUE = {}
            REPLACE &lc_pmprjdt..dact_fnsh WITH {},;
              &lc_pmprjdt..cstatus   WITH SUBSTR('MMA', AT(cstatus, 'SMA'), 1)

          ENDIF
          SELECT (lncuralias)
          RETURN .F.
        ENDIF
      ENDSCAN
      IF BETWEEN(lnrecno, 1, RECCOUNT())
        GO lnrecno
      ENDIF
      IF lcsrc = 'E'
        IF EMPTY(dact_strt)
          SELECT (lc_pmprjdt)
          REPLACE &lc_pmprjdt..dact_strt WITH IIF(EMPTY(&lc_pmprjdt..dclc_strt),;
            &lc_pmprjdt..dest_strt,&lc_pmprjdt..dclc_strt)
        ENDIF
        lnwend = 0
        SELECT (lc_pmprjdt)
        lndur  = (&lc_pmprjdt..dact_fnsh - &lc_pmprjdt..dact_strt) + 1

        lfgethlidy()

        SELECT (lc_pmprjdt)
        IF BETWEEN(lnrecno, 1, RECCOUNT())
          GO lnrecno
        ENDIF

        REPLACE &lc_pmprjdt..nact_dur  WITH ((&lc_pmprjdt..dact_fnsh - &lc_pmprjdt..dact_strt) + 1 - lnwend),;
          &lc_pmprjdt..nrem_dur  WITH 0,;
          &lc_pmprjdt..cmcomplt  WITH 'YYY',;
          &lc_pmprjdt..laddtoaud WITH .T.,;
          &lc_pmprjdt..cstatus   WITH SUBSTR('MMA', AT(cstatus, 'SMA'), 1)
        =lfcomtask()
      ELSE
        SELECT (lc_pmprjdt)
        REPLACE &lc_pmprjdt..cstatus WITH SUBSTR('MMA', AT(cstatus, 'SMA'), 1)
      ENDIF
      SELECT (lc_pmprjdt)
      SET ORDER TO pmprjdt
      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
      *!*        =SEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID))+;
      *!*              SUBSTR(lcStyle,1,LEN(cStyle)) + lcPredOpr)
      =SEEK(SUBSTR(lcprj_typ,1,LEN(cprj_typ)) + SUBSTR(lcprj_id,1,LEN(cprj_id))+;
        SUBSTR(lcstyle,1,LEN(cstyle)) +STR(lnlineno ,6)+ lcpredopr)
      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
      STORE .T. TO loformset.loparentform.llchngoprt, glupdated
      loformset.loparentform.llupdstrtd = .F.
    ENDIF
  ENDIF
ENDWITH
*!*************************************************************
*! Name      : lfGetHlidy
*: Developer : Mariam Mazhar[MMT]
*: Date      : 05/10/2009
*! Purpose   : Valid function for get Hoildays
*!*************************************************************
FUNCTION lfgethlidy
DIMENSION laholidays[1]
laholidays = ''
lnrows  = 0
lc_pmprjdt = loformset.loparentform.lcprjdt
IF gfseek(&lc_pmprjdt..ccal_id,'PMCALDT')
  SELECT pmcaldt
  SCAN REST WHILE ccal_id = &lc_pmprjdt..ccal_id
    ldcal_hfrm = dcal_hfrm
    DO WHILE ldcal_hfrm <= dcal_hto
      lnrows  = lnrows  + 1
      DIMENSION laholidays[lnRows]
      laholidays[lnRows] = ccal_id + DTOC(ldcal_hfrm)
      ldcal_hfrm = ldcal_hfrm + 1
    ENDDO
  ENDSCAN
ENDIF

IF gfseek(&lc_pmprjdt..ccal_id,'PMCALHD')
  ldoprtdt = &lc_pmprjdt..dact_strt
  FOR lni = 1 TO lndur
    lnwend = lnwend + IIF(STR(DOW(ldoprtdt),1) $ pmcalhd.ccal_wend .OR. ;
      ASCAN(laholidays,pmprjdt.ccal_id+ DTOC(ldoprtdt)) > 0 , 1 , 0)
    ldoprtdt = ldoprtdt + 1
  ENDFOR
ENDIF
*!*************************************************************
*! Name      : lfComTask
*: Developer : Mariam Mazhar[MMT]
*: Date      : 05/10/2009
*! Purpose   : Valid function for Chack completed tasks
*!*************************************************************
FUNCTION lfcomtask
STORE "" TO lcprog , lckey , lcapobjnam ,lcevent
lc_pmprjrl = loformset.loparentform.lc_pmprjrl
lc_pmprjdt = loformset.loparentform.lcprjdt
=lfupdvar()
SELECT syschdul
=gfsetorder('COPRUSR')
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
*!*  IF gfSEEK(&lc_PMPrjDt..cPrj_Typ+&lc_PMPrjDt..cPrj_ID+&lc_PMPrjDt..cStyle+&lc_PMPrjDt..cOprt_Ctg+&lc_PMPrjDt..cOprt_ID)
IF gfseek(&lc_pmprjdt..cprj_typ+&lc_pmprjdt..cprj_id+&lc_pmprjdt..cstyle+STR(&lc_pmprjdt..LINENO,6)+&lc_pmprjdt..coprt_ctg+&lc_pmprjdt..coprt_id)
  *B609555,1 MMT 03/27/2011 Task List screen gives error while completing task[Start]
  IF syschdul.coperstat= 'C'
    lnoldsel = SELECT()
    SELECT syschdul
    LOCATE REST WHILE cconttype+cseqnumber+cstyle+STR(LINENO,6)+ccont_id+coperstat+cuser_id=&lc_pmprjdt..cprj_typ+&lc_pmprjdt..cprj_id+&lc_pmprjdt..cstyle+STR(&lc_pmprjdt..LINENO,6)+&lc_pmprjdt..coprt_ctg+&lc_pmprjdt..coprt_id;
      FOR coperstat <> 'C'
    IF !FOUND()
      =gfseek(&lc_pmprjdt..cprj_typ+&lc_pmprjdt..cprj_id+&lc_pmprjdt..cstyle+STR(&lc_pmprjdt..LINENO,6)+&lc_pmprjdt..coprt_ctg+&lc_pmprjdt..coprt_id,'SYSCHDUL')
    ENDIF
    SELECT(lnoldsel)
  ENDIF
  **B609555,1 MMT 03/27/2011 Task List screen gives error while completing task[End]
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
  gfreplace("ccompleted WITH 'Y',;
          COPERSTAT  WITH 'C'")
  loformset.loparentform.lladdtoadt  = .T.
ENDIF

lcprj_typ = &lc_pmprjdt..cprj_typ
lcprj_id = &lc_pmprjdt..cprj_id
lcstyle  = &lc_pmprjdt..cstyle

*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
lnlineno = &lc_pmprjdt..LINENO
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]

lcpredopr = &lc_pmprjdt..coprt_ctg+&lc_pmprjdt..coprt_id
DIMENSION lancont_id[1]
lancont_id[1] =""
SET ORDER TO pmprjrlp IN (lc_pmprjrl)
SELECT (lc_pmprjrl)
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
*!*  IF gfSEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID))+;
*!*          SUBSTR(lcStyle,1,LEN(cStyle)) + &lc_PMPrjDt..cOprt_Ctg+&lc_PMPrjDt..cOprt_ID,lc_PMPrjRl)
IF gfseek(SUBSTR(lcprj_typ,1,LEN(cprj_typ)) + SUBSTR(lcprj_id,1,LEN(cprj_id))+;
    SUBSTR(lcstyle,1,LEN(cstyle)) +STR(lnlineno ,6)+ &lc_pmprjdt..coprt_ctg+&lc_pmprjdt..coprt_id,lc_pmprjrl)
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
  lni = 0
  SELECT (lc_pmprjrl)
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
  *!*    SCAN REST WHILE cprj_typ+cprj_id+cstyle+cprd_ctg+cprd_id = ;
  *!*                    SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID))+;
  *!*                    SUBSTR(lcStyle,1,LEN(cStyle)) + &lc_PMPrjDt..cOprt_Ctg+&lc_PMPrjDt..cOprt_ID
  SCAN REST WHILE cprj_typ+cprj_id+cstyle+STR(LINENO,6)+cprd_ctg+cprd_id = ;
      SUBSTR(lcprj_typ,1,LEN(cprj_typ)) + SUBSTR(lcprj_id,1,LEN(cprj_id))+;
      SUBSTR(lcstyle,1,LEN(cstyle)) +STR(lnlineno ,6)+ &lc_pmprjdt..coprt_ctg+&lc_pmprjdt..coprt_id
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
    DIMENSION lancont_id[lnI+1]
    lni = ALEN(lancont_id,1)
    lancont_id[lnI] = &lc_pmprjrl..coprt_ctg+&lc_pmprjrl..coprt_id
  ENDSCAN
ENDIF

IF !EMPTY(lancont_id[1])
  FOR lni = 1 TO ALEN(lancont_id,1)

    DIMENSION lanprd_id[1]
    STORE "" TO lanprd_id
    SELECT pmprjrl
    =gfsetorder('PMPRJRL')
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
    *!*      IF gfSEEK(SUBSTR(lcPrj_Typ,1,LEN(PMPRJRL.cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(PMPRJRL.cPrj_ID))+;
    *!*              SUBSTR(lcStyle,1,LEN(PMPRJRL.cStyle))+lANCont_ID[lnI],'PMPRJRL')
    IF gfseek(SUBSTR(lcprj_typ,1,LEN(pmprjrl.cprj_typ)) + SUBSTR(lcprj_id,1,LEN(pmprjrl.cprj_id))+;
        SUBSTR(lcstyle,1,LEN(pmprjrl.cstyle))+STR(lnlineno ,6)+lancont_id[lnI],'PMPRJRL')
      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
      lnk = 0
      SELECT pmprjrl
      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
      *!*        SCAN REST WHILE cprj_typ+cprj_id+cstyle+coprt_ctg+coprt_id =;
      *!*                SUBSTR(lcPrj_Typ,1,LEN(PMPRJRL.cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(PMPRJRL.cPrj_ID))+;
      *!*                SUBSTR(lcStyle,1,LEN(PMPRJRL.cStyle))+lANCont_ID[lnI]
      SCAN REST WHILE cprj_typ+cprj_id+cstyle+STR(LINENO,6)+coprt_ctg+coprt_id =;
          SUBSTR(lcprj_typ,1,LEN(pmprjrl.cprj_typ)) + SUBSTR(lcprj_id,1,LEN(pmprjrl.cprj_id))+;
          SUBSTR(lcstyle,1,LEN(pmprjrl.cstyle))+STR(lnlineno ,6)+lancont_id[lnI]
        *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
        DIMENSION lanprd_id[lnK +1]
        lnk = ALEN(lanprd_id,1)
        lanprd_id[lnK] =pmprjrl.cprd_ctg+pmprjrl.cprd_id
      ENDSCAN
    ENDIF

    llloop = .F.
    FOR lnk = 1 TO ALEN(lanprd_id,1)
      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
      *IF gfSEEK(lcPrj_Typ+lcPrj_ID+lcStyle+laNPrd_ID[lnK],'SYSCHDUL') AND SYSCHDUL.cOperStat <> 'C'
      *B609555,1 MMT 03/27/2011 Task List screen gives error while completing task[Start]
      *IF gfSEEK(lcPrj_Typ+lcPrj_ID+lcStyle+STR(lnLineNo ,6)+laNPrd_ID[lnK],'SYSCHDUL') AND SYSCHDUL.cOperStat <> 'C'
      IF gfseek(lcprj_typ+lcprj_id+lcstyle+STR(lnlineno ,6)+lanprd_id[lnK],'SYSCHDUL')
        lnoldsel = SELECT()
        SELECT syschdul
        LOCATE REST WHILE cconttype+cseqnumber+cstyle+STR(LINENO,6)+ccont_id+coperstat+cuser_id = lcprj_typ+lcprj_id+lcstyle+STR(lnlineno ,6)+lanprd_id[lnK] FOR ;
          syschdul.coperstat <> 'C'
        IF FOUND()
          SELECT(lnoldsel)
          *B609555,1 MMT 03/27/2011 Task List screen gives error while completing task[End]
          *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
          llloop = .T.
          EXIT
          *B609555,1 MMT 03/27/2011 Task List screen gives error while completing task[Start]
        ENDIF
        SELECT(lnoldsel)
        *B609555,1 MMT 03/27/2011 Task List screen gives error while completing task[End]
      ENDIF
    ENDFOR
    IF llloop
      LOOP
    ENDIF
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
    *IF gfSEEK(lcPrj_Typ+lcPrj_ID+lcStyle+lANCont_ID[lnI],'SYSCHDUL') AND SYSCHDUL.cOperStat <> 'C'
    *B609555,1 MMT 03/27/2011 Task List screen gives error while completing task[Start]
    *IF gfSEEK(lcPrj_Typ+lcPrj_ID+lcStyle+STR(lnLineNo ,6)+lANCont_ID[lnI],'SYSCHDUL') AND SYSCHDUL.cOperStat <> 'C'
    IF gfseek(lcprj_typ+lcprj_id+lcstyle+STR(lnlineno ,6)+lancont_id[lnI],'SYSCHDUL')
      lnoldsel = SELECT()
      SELECT syschdul
      LOCATE REST WHILE cconttype+cseqnumber+cstyle+STR(LINENO,6)+ccont_id+coperstat+cuser_id=lcprj_typ+lcprj_id+lcstyle+STR(lnlineno ,6)+lancont_id[lnI] FOR ;
        syschdul.coperstat <> 'C'
      IF FOUND()
        *B609555,1 MMT 03/27/2011 Task List screen gives error while completing task[End]
        *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
        SELECT  syschdul
        gfreplace("lPredComp WITH .T.")
        IF syschdul.coperstat = 'H'
          gfreplace("cOperStat  WITH 'O'")
        ENDIF
        *B609555,1 MMT 03/27/2011 Task List screen gives error while completing task[Start]
      ENDIF
      SELECT(lnoldsel)
      *B609555,1 MMT 03/27/2011 Task List screen gives error while completing task[End]
    ENDIF
    loformset.loparentform.lladdtoadt = .T.
    SELECT (lc_pmprjdt)
    lcprvord = ORDER()
    SET ORDER TO pmprjusr
    =SEEK(SUBSTR(lcprj_typ,1,LEN(cprj_typ))+SUBSTR(lcprj_id,1,LEN(cprj_id))+lancont_id[lnI])
    REPLACE &lc_pmprjdt..cstatus   WITH SUBSTR('MMA', AT(cstatus, 'SMA'), 1),;
      &lc_pmprjdt..laddtoaud WITH .T.
    SET ORDER TO &lcprvord
  ENDFOR
ENDIF



dsch_date = oariaapplication.systemdate
lleditaddscr = .T.
lncurdtses = SET("Datasession")

WITH  loformset.loparentform
  lcdetfile = .lcprjdt
  lchdfile  = .lcprjhdr
  lcrelfile = .lc_pmprjrl
ENDWITH

lcxmlflname = gftempname()
lcexpr = gfopgrid('PMPRJSCH',.T.,"R",.T.,.T.,.T.)

IF FILE(oariaapplication.outputhome+lcxmlflname+'.XML')
  DO (oariaapplication.applicationhome+'SM\PMPRJSCH.Fxp') WITH oariaapplication.activecompanyid,;
    oariaapplication.outputhome+lcxmlflname+'.XML'
ENDIF
lleditaddscr = .F.


SET ORDER TO pmprjrlp IN (lc_pmprjrl)
SET ORDER TO pmprjdt  IN (lc_pmprjdt)
SELECT (lc_pmprjdt)
DO WHILE .T.
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
  *!*    IF SEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID))+;
  *!*       SUBSTR(lcStyle,1,LEN(cStyle)) + lcPredOpr,lc_PMPrjRl)
  IF SEEK(SUBSTR(lcprj_typ,1,LEN(cprj_typ)) + SUBSTR(lcprj_id,1,LEN(cprj_id))+;
      SUBSTR(lcstyle,1,LEN(cstyle)) +STR(lnlineno ,6)+ lcpredopr,lc_pmprjrl)
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
    lcoprtoup = &lc_pmprjrl..coprt_ctg+&lc_pmprjrl..coprt_id
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
    *!*      =SEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID))+;
    *!*            SUBSTR(lcStyle,1,LEN(cStyle)) + lcOprToUp,lc_PMPrjDt)
    =SEEK(SUBSTR(lcprj_typ,1,LEN(cprj_typ)) + SUBSTR(lcprj_id,1,LEN(cprj_id))+;
      SUBSTR(lcstyle,1,LEN(cstyle))+STR(lnlineno ,6) + lcoprtoup,lc_pmprjdt)
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
    SELECT (lc_pmprjdt)
    REPLACE &lc_pmprjdt..cstatus WITH SUBSTR('MMA', AT(cstatus, 'SMA'), 1)
  ELSE
    EXIT
  ENDIF
  lcpredopr = lcoprtoup
ENDDO

*!*************************************************************
*! Name      : lfvLatStrt
*: Developer : Mariam Mazhar[MMT]
*: Date      : 05/10/2009
*! Purpose   : valid function of latest start date.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvClose()
*!*************************************************************
FUNCTION lfvlatstrt
PARAMETERS loformset

m.llaststrt  = IIF(loformset.lnlatstrt =  1,.T.,.F.)
lcprjhd = loformset.lcprjhdr
IF loformset.ariaform1.cbostatus.VALUE  <> 'P' AND loformset.lnlatstrt = 0
  RETURN
ENDIF
lcprj_typ  = &lcprjhd..cprj_typ
lcprj_id   = &lcprjhd..cprj_id
lcstyle    = &lcprjhd..cstyle
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
lnlineno   = &lcprjhd..LINENO
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
WITH loformset.ariaform1
  m.dest_fnsh =.dtpestend.VALUE
  m.dreq_fnsh = .dtpreqend.VALUE

  ldoldestms  =  .dtpeststrt.VALUE
  ldoldestmf  =  .dtpestend.VALUE

  IF loformset.lnlatstrt =  1 AND !EMPTY(DTOS(.dtpestend.VALUE)) AND !loformset.llupdstrtd
    loformset.llupdstrtd = .T.
    IF !EMPTY(.dtpreqend.VALUE) AND !EMPTY(.dtpestend.VALUE) AND .dtpreqend.VALUE > .dtpestend.VALUE
      llfromfld   = .F.
      lcoldval    = .dtpeststrt.VALUE
      ldoldstrt   = .dtpeststrt.VALUE
      ldoldfnsh   = .dtpestend.VALUE
      lndefr = .dtpreqend.VALUE -  .dtpestend.VALUE
      .dtpeststrt.VALUE = .dtpeststrt.VALUE+ lndefr
      =lfvestdate(loformset)
      IF m.dest_fnsh > m.dreq_fnsh
        DO WHILE (m.dest_fnsh > m.dreq_fnsh)
          .dtpeststrt.VALUE = ldoldstrt
          .dtpestend.VALUE = ldoldfnsh
          lndefr = lndefr - 1
          .dtpeststrt.VALUE= .dtpeststrt.VALUE + lndefr
          =lfvestdate(loformset)
          IF lndefr = 0
            EXIT
          ENDIF
        ENDDO
      ENDIF
    ELSE
      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
      *IF loFormSet.ActiveMode = 'E' AND gfSEEK(lcPrj_Typ + lcPrj_ID+ lcStyle , 'PMPRJHD') AND PMPRJHD.lLaststrt
      IF loformset.activemode = 'E' AND gfseek(lcprj_typ + lcprj_id+ lcstyle+STR(lnlineno   ,6) , 'PMPRJHD') AND pmprjhd.llaststrt
        *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
        RETURN
      ENDIF

      IF !EMPTY(.dtpreqend.VALUE) AND !EMPTY(.dtpestend.VALUE)  AND  .dtpreqend.VALUE  < .dtpestend.VALUE


        IF gfmodalgen("INM38246B00006","DIALOG") = 1
          llfromfld   = .F.
          lcoldval    = .dtpeststrt.VALUE
          ldoldstrt   = .dtpeststrt.VALUE
          ldoldfnsh   = .dtpestend.VALUE
          lndefr = .dtpestend.VALUE - .dtpreqend.VALUE
          .dtpeststrt.VALUE = .dtpeststrt.VALUE - lndefr
          =lfvestdate(loformset)
          IF .dtpreqend.VALUE  < .dtpestend.VALUE
            DO WHILE (.dtpreqend.VALUE  < .dtpestend.VALUE )
              lndefr = lndefr + 1
              .dtpeststrt.VALUE = ldoldstrt - lndefr

              REPLACE dest_strt WITH .dtpeststrt.VALUE  IN (lcprjhd)
              =lfvestdate(loformset)
            ENDDO
          ENDIF
        ENDIF

      ENDIF
    ENDIF
  ELSE
    IF loformset.lnlatstrt = 0
      loformset.ariaform1.dtpeststrt.VALUE = ldoldestms
      loformset.ariaform1.dtpestend.VALUE = ldoldestmf
      REPLACE  dest_strt  WITH ldoldestms  ,;
        dest_fnsh  WITH ldoldestmf  IN (lcprjhd)
    ENDIF
  ENDIF
ENDWITH
*!*************************************************************
*! Name      : lfvEstDate
*: Developer : Mariam Mazhar[MMT]
*: Date      : 05/10/2009
*! Purpose   : Valid function for estimated start date.
*!*************************************************************
*! Calls              : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvEstDate()
*!*************************************************************
FUNCTION lfvestdate
PARAMETERS loformset

WITH loformset
  lcprjhd    = .lcprjhdr
  lc_pmprjdt = .lcprjdt
  lc_pmprjrl = .lc_pmprjrl
  lc_parser  = .lc_parser
ENDWITH

lcprj_typ  = &lcprjhd..cprj_typ
lcprj_id   = &lcprjhd..cprj_id
lcstyle    = &lcprjhd..cstyle
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
lnlineno   = &lcprjhd..LINENO
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
WITH loformset.ariaform1
  lcoldval = .dtpeststrt.oldvalue
  IF TYPE('lcOldVal') <> 'D'
    lcoldval = {}
  ENDIF
  ACOPY(loformset.laholidays,laholidays)
  IF .dtpeststrt.VALUE  <> lcoldval
    IF !EMPTY(.dtpeststrt.VALUE)
      IF loformset.lnnumoflin > 0
        =lfgetestdt(@lc_pmprjdt, @lc_pmprjrl,.F.)
      ENDIF
    ELSE
      .dtpeststrt.VALUE = lcoldval
    ENDIF
  ENDIF
  GO TOP IN (lc_pmprjrl)
  lcctgoprt = &lc_pmprjrl..cprd_ctg+&lc_pmprjrl..cprd_id
  lcprvord = ORDER(lc_pmprjdt)
  SET ORDER TO (lc_pmprjdt) IN (lc_pmprjdt)
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
  *  IF SEEK(lcPrj_typ+lcPrj_ID+SUBSTR(lcStyle,1,LEN(&lc_PmPrjDt..cStyle)),lc_PmPrjDt)
  IF SEEK(lcprj_typ+lcprj_id+SUBSTR(lcstyle,1,LEN(&lc_pmprjdt..cstyle))+STR(lnlineno   ,6),lc_pmprjdt)
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
    .dtpeststrt.VALUE = &lc_pmprjdt..dest_strt
    REPLACE dest_strt WITH .dtpeststrt.VALUE IN (lcprjhd)
  ENDIF
  SET ORDER TO &lcprvord IN (lc_pmprjdt)
ENDWITH

*!*************************************************************
*! Name      : lfChkComp
*: Developer : Mariam Mazhar[MMT]
*: Date      : 05/10/2009
*! Purpose   : function to Check completed
*!*************************************************************
FUNCTION lfchkcomp


llcomplete = .T.
SELECT (lc_pmprjdt)
LOCATE
IF EOF()
  llcomplete = .F.
ELSE
  SCAN
    IF EMPTY(DTOS(dact_fnsh))
      llcomplete = .F.
      EXIT
    ENDIF
  ENDSCAN
ENDIF

IF llcomplete
  loformset.ariaform1.dtpactstrt.VALUE = &lcprjhd..dclc_strt &&loformset.ariaform1.DtpCalcStrt.value
  loformset.ariaform1.dtpactend.VALUE  = &lcprjhd..dclc_fnsh  && loformset.ariaform1.DtpCalcEnd.value
  loformset.ariaform1.cbostatus.VALUE  = 'C'
  REPLACE  dact_strt WITH dclc_strt,;
    dact_fnsh WITH dclc_fnsh,;
    cprj_stts WITH 'C' IN (lcprjhd)
ENDIF

*!*************************************************************
*! Name      : lfVldPrd
*: Developer : Mariam Mazhar[MMT]
*: Date      : 05/10/2009
*! Purpose   : Checks a predecessor whether it would create a
*!             cyclic path or not.
*!*************************************************************
*! Called    : lfvMSource() in MAINPROC, from ARIAMOVR.SR
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Passed Parameters  :
*!*************************************************************
*! Returns            : .T. if the current operation may be
*!                      selected as a predecessor, .f. otherwise.
*!*************************************************************
*! Example            :  IF lfVldPrd(..)....
*!*************************************************************
FUNCTION lfvldprd
PARAMETERS lnsrcbtn,lnlstindx
IF lnsrcbtn = 3 OR lnsrcbtn = 4
  RETURN .T.
ENDIF
llmoveall  = (lnsrcbtn = 2)
lcarrctg  = SUBSTR(IIF(lnlstindx = 0,lasource[1],lasource[lnLstIndx]), 1, 3)
lcarroprt = SUBSTR(IIF(lnlstindx = 0,lasource[1],lasource[lnLstIndx]), 5, 5)
m.coprt_dsc = &lc_pmprjdt..coprt_dsc
*B609711,1 MMT 01/05/2012 Fix of Error 'gfOpenFile' is found while add activities[T20110808.0008][START]
*SET PROCEDURE TO &lcSetProc
*B609711,1 MMT 01/05/2012 Fix of Error 'gfOpenFile' is found while add activities[T20110808.0008][END]
lnretval = IIF(ASCAN(lapathelems, lcarrctg + lcarroprt) = 0, .T.,;
  IIF(gfmodalgen("TRM38214B00000","DIALOG",ALLTRIM(lcarrctg)+'\'+;
  ALLTRIM(lcarroprt)) = 1, ;
  .F., .T.))
IF !EMPTY(latarget)
  lcprevord = ORDER(lc_pmprjrl)
  IF llmoveall
    SET ORDER TO pmprjrl IN (lc_pmprjrl)
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
    *!*        IF lnRetVal AND (SEEK(lcPrj_typ+lcPrj_ID+SUBSTR(lcStyle,1,LEN(&lc_PMPrjRl..cStyle))+lcOprt_ctg+lcOprt_ID,lc_PMPrjRl) AND;
    *!*                         SEEK(lcPrj_typ+lcPrj_ID+SUBSTR(lcStyle,1,LEN(&lc_PMPrjRl..cStyle))+&lc_PMPrjRl..Cprd_Ctg+&lc_PMPrjRl..Cprd_ID,lc_PMPrjRl)) AND ;
    *!*                         (lcArrCtg = &lc_PMPrjRl..Cprd_Ctg AND lcArrOprt = &lc_PMPrjRl..Cprd_ID)
    IF lnretval AND (SEEK(lcprj_typ+lcprj_id+SUBSTR(lcstyle,1,LEN(&lc_pmprjrl..cstyle))+STR(lnlineno   ,6)+lcoprt_ctg+lcoprt_id,lc_pmprjrl) AND;
        SEEK(lcprj_typ+lcprj_id+SUBSTR(lcstyle,1,LEN(&lc_pmprjrl..cstyle))+STR(lnlineno   ,6)+&lc_pmprjrl..cprd_ctg+&lc_pmprjrl..cprd_id,lc_pmprjrl)) AND ;
        (lcarrctg = &lc_pmprjrl..cprd_ctg AND lcarroprt = &lc_pmprjrl..cprd_id)
      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
      =gfmodalgen("TRM38241B00000","DIALOG",;
        lcarrctg+"|"+lcarroprt+"|"+lcoprt_ctg+"|"+lcoprt_id+"|"+ALLTRIM(m.coprt_dsc))
      lnretval = .F.
    ELSE
      SET ORDER TO pmprjrlp IN (lc_pmprjrl)
      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
      *IF lnRetVal AND SEEK(lcPrj_typ+lcPrj_ID+SUBSTR(lcStyle,1,LEN(&lc_PMPrjRl..cStyle))+lcArrCtg+lcArrOprt,lc_PMPrjRl) AND ASCAN(laTarget,&lc_PMPrjRl..COprt_Ctg+'\'+&lc_PMPrjRl..COprt_ID) <> 0)
      IF lnretval AND SEEK(lcprj_typ+lcprj_id+SUBSTR(lcstyle,1,LEN(&lc_pmprjrl..cstyle))+STR(lnlineno   ,6)+lcarrctg+lcarroprt,lc_pmprjrl) AND ASCAN(latarget,&lc_pmprjrl..coprt_ctg+'\'+&lc_pmprjrl..coprt_id) <> 0)
        *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
        =gfmodalgen("TRM38241B00000","DIALOG",;
          lcarrctg+"|"+lcarroprt+"|"+lcoprt_ctg+"|"+lcoprt_id+"|"+ALLTRIM(m.coprt_dsc))
        lnretval = .F.
      ELSE
        SET ORDER TO pmprjrl IN (lc_pmprjrl)
        *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
        *IF lnRetVal AND SEEK(lcPrj_typ+lcPrj_ID+SUBSTR(lcStyle,1,LEN(&lc_PMPrjRl..cStyle))+lcArrCtg+lcArrOprt,lc_PMPrjRl) AND ASCAN(laTarget,&lc_PMPrjRl..CPrd_Ctg+'\'+&lc_PMPrjRl..CPrd_ID) <> 0)
        IF lnretval AND SEEK(lcprj_typ+lcprj_id+SUBSTR(lcstyle,1,LEN(&lc_pmprjrl..cstyle))+STR(lnlineno   ,6)+lcarrctg+lcarroprt,lc_pmprjrl) AND ASCAN(latarget,&lc_pmprjrl..cprd_ctg+'\'+&lc_pmprjrl..cprd_id) <> 0)
          *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
          =gfmodalgen("TRM38242B00000","DIALOG",;
            lcoprt_ctg+"|"+lcoprt_id+"|"+ALLTRIM(m.coprt_dsc)+"|"+lcarrctg+"|"+lcarroprt)
          lnretval = .F.
        ENDIF
      ENDIF
    ENDIF
  ELSE
    SET ORDER TO pmprjrl IN (lc_pmprjrl)
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
    *!*        IF lnRetVal AND (SEEK(lcPrj_typ+lcPrj_ID+SUBSTR(lcStyle,1,LEN(&lc_PMPrjRl..cStyle))+lcOprt_ctg+lcOprt_ID,lc_PMPrjRl) AND;
    *!*                         SEEK(lcPrj_typ+lcPrj_ID+SUBSTR(lcStyle,1,LEN(&lc_PMPrjRl..cStyle))+&lc_PMPrjRl..Cprd_Ctg+&lc_PMPrjRl..Cprd_ID,lc_PMPrjRl)) AND ;
    *!*                         (lcArrCtg = &lc_PMPrjRl..Cprd_Ctg AND lcArrOprt = &lc_PMPrjRl..Cprd_ID)
    IF lnretval AND (SEEK(lcprj_typ+lcprj_id+SUBSTR(lcstyle,1,LEN(&lc_pmprjrl..cstyle))+STR(lnlineno   ,6)+lcoprt_ctg+lcoprt_id,lc_pmprjrl) AND;
        SEEK(lcprj_typ+lcprj_id+SUBSTR(lcstyle,1,LEN(&lc_pmprjrl..cstyle))+STR(lnlineno   ,6)+&lc_pmprjrl..cprd_ctg+&lc_pmprjrl..cprd_id,lc_pmprjrl)) AND ;
        (lcarrctg = &lc_pmprjrl..cprd_ctg AND lcarroprt = &lc_pmprjrl..cprd_id)
      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
      =gfmodalgen("TRM38243B00000","DIALOG",;
        lcoprt_ctg+"|"+lcoprt_id+"|"+ALLTRIM(m.coprt_dsc))
      lnretval = .F.
    ELSE
      SET ORDER TO pmprjrlp IN (lc_pmprjrl)
      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
      *!*          IF lnRetVal AND SEEK(lcPrj_typ+lcPrj_ID+SUBSTR(lcStyle,1,LEN(&lc_PMPrjRl..cStyle))+lcArrCtg+lcArrOprt,lc_PMPrjRl) AND;
      *!*                          ASCAN(laTarget,&lc_PMPrjRl..COprt_Ctg+'\'+&lc_PMPrjRl..COprt_ID) <> 0
      IF lnretval AND SEEK(lcprj_typ+lcprj_id+SUBSTR(lcstyle,1,LEN(&lc_pmprjrl..cstyle))+STR(lnlineno   ,6)+lcarrctg+lcarroprt,lc_pmprjrl) AND;
          ASCAN(latarget,&lc_pmprjrl..coprt_ctg+'\'+&lc_pmprjrl..coprt_id) <> 0
        *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
        =gfmodalgen("TRM38243B00000","DIALOG",;
          lcoprt_ctg+"|"+lcoprt_id+"|"+ALLTRIM(m.coprt_dsc))
        lnretval = .F.
      ELSE
        SET ORDER TO pmprjrl IN (lc_pmprjrl)
        *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
        *IF lnRetVal AND SEEK(lcPrj_typ+lcPrj_ID+SUBSTR(lcStyle,1,LEN(&lc_PMPrjRl..cStyle))+lcArrCtg+lcArrOprt,lc_PMPrjRl) AND;
        ASCAN(laTarget,&lc_PMPrjRl..CPrd_Ctg+'\'+&lc_PMPrjRl..CPrd_ID) <> 0
        IF lnretval AND SEEK(lcprj_typ+lcprj_id+SUBSTR(lcstyle,1,LEN(&lc_pmprjrl..cstyle))+STR(lnlineno   ,6)+lcarrctg+lcarroprt,lc_pmprjrl) AND;
            ASCAN(latarget,&lc_pmprjrl..cprd_ctg+'\'+&lc_pmprjrl..cprd_id) <> 0
          *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
          =gfmodalgen("TRM38244B00000","DIALOG",;
            lcoprt_ctg+"|"+lcoprt_id+"|"+ALLTRIM(m.coprt_dsc))
          lnretval = .F.
        ENDIF
      ENDIF
    ENDIF
  ENDIF
  SET ORDER TO &lcprevord IN (lc_pmprjrl)
ENDIF
*B609711,1 MMT 01/05/2012 Fix of Error 'gfOpenFile' is found while add activities[T20110808.0008][START]
*SET PROCEDURE TO
*B609711,1 MMT 01/05/2012 Fix of Error 'gfOpenFile' is found while add activities[T20110808.0008][END]
RETURN lnretval


*!*************************************************************
*! Name      : lfvCal_ID
*: Developer : Mariam Mazhar[MMT]
*: Date      : 05/10/2009
*! Purpose   : Validate calendar
*!*************************************************************
FUNCTION lfvcal_id
PARAMETERS lobrformset

PRIVATE lncuralias
lncuralias = SELECT(0)
lc_pmprjdt = lobrformset.loparentform.lcprjdt
SELECT (lc_pmprjdt)
lcccal_id = PADR(lobrformset.ariaform1.cnttaskdet.kbcalen.keytextbox.VALUE ,4)
lcoldval = lobrformset.ariaform1.cnttaskdet.kbcalen.keytextbox.oldvalue
llbrowse  = lobrformset.ariaform1.cnttaskdet.kbcalen.selectedfrombrowse  .OR. '?' $ lcccal_id
REPLACE ccal_id WITH PADR(ALLTRIM(ccal_id),4)
lcsetexact = SET('EXACT')
SET EXACT ON
SELECT pmcalhd
IF !llbrowse
  IF !EMPTY(lcccal_id) .AND. !gfseek(lcccal_id, 'PMCALHD')
    IF BETWEEN(RECNO(0), 1, RECCOUNT())
      GO RECNO(0)
    ELSE
      GO TOP
    ENDIF
    llbrowse = .T.
  ELSE
    SELECT (lc_pmprjdt)
    REPLACE cstatus   WITH SUBSTR('MMA', AT(cstatus, 'SMA'), 1),;
      ccal_id   WITH lcccal_id ,;
      cadd_user WITH oariaapplication.user_id ,;
      dadd_date WITH oariaapplication.systemdate,;
      cadd_time WITH TIME()
    STORE .T. TO glupdated, lobrformset.loparentform.llchngoprt
    lobrformset.loparentform.llupdstrtd = .F.
  ENDIF
ELSE
  GO TOP
ENDIF

IF llbrowse
  lcbrfields = [cCal_ID : H = 'Calendar ID', cCal_Des : H = 'Description']
  DIMENSION laselected[1]
  lcfldname = 'cCal_ID'
  IF ariabrow('', 'Calendars',;
      gnbrfsrow1, gnbrfscol1, gnbrfsrow2, gnbrfscol2,.F.,.F.,lcfldname,'laSelected')
    SELECT (lc_pmprjdt)
    REPLACE ccal_id   WITH pmcalhd.ccal_id,;
      cstatus   WITH SUBSTR('MMA', AT(cstatus, 'SMA'), 1),;
      cadd_user WITH oariaapplication.user_id ,;
      dadd_date WITH ldcurdate,;
      cadd_time WITH TIME()
    STORE .T. TO glupdated, lobrformset.loparentform.llchngoprt
    lobrformset.loparentform.llupdstrtd = .F.
  ELSE
    SELECT (lc_pmprjdt)
    REPLACE ccal_id WITH lcoldval
  ENDIF
ENDIF
llbrowse = .F.
SET EXACT &lcsetexact
SELECT (lncuralias)

*!*************************************************************
*! Name      : lfInitStage
*! Developer : Mariam Mazhar[MMT]
*! Date      : 05/10/2009
*! Purpose   : Initialize current stage array
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  lcCurCode : project status
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfInitStage(m.cPrj_Stts)
*!*************************************************************
FUNCTION lfinitstage
PARAMETERS lccurcode,loformset
WITH loformset
  DO CASE
    *-- If in PLanning
  CASE lccurcode = 'P'
    DIMENSION .lacurstatus[3,2]
    .lacurstatus[1,1] = lang_mfproj_palnning
    .lacurstatus[1,2] = 'P'
    .lacurstatus[2,1] = lang_mfproj_complete
    .lacurstatus[2,2] = 'C'
    .lacurstatus[3,1] = lang_mfproj_canc
    .lacurstatus[3,2] = 'X'


  CASE lccurcode = 'I'
    DIMENSION .lacurstatus[3,2]
    .lacurstatus[1,1] = lang_mfproj_inprorgress
    .lacurstatus[1,2] = 'I'
    .lacurstatus[2,1] = lang_mfproj_complete
    .lacurstatus[2,2] = 'C'
    .lacurstatus[3,1] = lang_mfproj_canc
    .lacurstatus[3,2] = 'X'

  CASE lccurcode = 'C'
    DIMENSION .lacurstatus[2,2]
    .lacurstatus[1,1] = lang_mfproj_complete
    .lacurstatus[1,2] = 'C'
    .lacurstatus[2,1] = lang_mfproj_historey
    .lacurstatus[2,2] = 'H'

  CASE lccurcode = 'X'
    DIMENSION .lacurstatus[2,2]
    .lacurstatus[1,1] = lang_mfproj_canc
    .lacurstatus[1,2] = 'X'
    .lacurstatus[2,1] = lang_mfproj_historey
    .lacurstatus[2,2] = 'H'

  OTHERWISE
    DIMENSION .lacurstatus[5,2]
    .lacurstatus[1,1] = lang_mfproj_palnning
    .lacurstatus[1,2] = 'P'
    .lacurstatus[2,1] = lang_mfproj_inprorgress
    .lacurstatus[2,2] = 'I'
    .lacurstatus[3,1] = lang_mfproj_complete
    .lacurstatus[3,2] = 'C'
    .lacurstatus[4,1] = lang_mfproj_canc
    .lacurstatus[4,2] = 'X'
    .lacurstatus[5,1] = lang_mfproj_historey
    .lacurstatus[5,2] = 'H'
  ENDCASE
  .ariaform1.cbostatus.REQUERY()
  .ariaform1.cbostatus.VALUE = lccurcode
ENDWITH
*!*************************************************************
*! Name      : lfvCurStag
*! Developer : Mariam Mazhar
*! Date      : 05/10/2009
*! Purpose   : Initialize current stage array
*!*************************************************************
*! Calls     : gcActPop
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvCurStag()
*!*************************************************************
FUNCTION lfvcurstag
PARAMETERS loformset
lc_pmprjdt = loformset.lcprjdt

WITH  loformset.ariaform1
  lcoldval =.cbostatus.oldvalue
  m.cprj_stts = .cbostatus.VALUE

  IF lcoldval <> m.cprj_stts
    IF m.cprj_stts $ 'CXH'
      .dtpreqstrt.ENABLED = .F.
      .dtpreqend.ENABLED = .F.
      .dtpeststrt.ENABLED = .F.
      .dtpactstrt.ENABLED = .F.
      .dtpactend.ENABLED = .F.
      .kbtmpl.ENABLED = .F.
      .txtnote.ENABLED = .F.
      IF m.cprj_stts = 'H'
        .cmdtask.ENABLED = .F.
      ELSE
        .cmdtask.ENABLED = .T.
      ENDIF
    ELSE
      .txtnote.ENABLED = .T.
      IF loformset.lnnumoflin = 0
        .kbtmpl.ENABLED = .T.
      ELSE
        .kbtmpl.ENABLED = .F.
      ENDIF
      .dtpreqstrt.ENABLED = .T.
      .dtpreqend.ENABLED = .T.

      IF m.cprj_stts = 'P'
        .dtpeststrt.ENABLED = .T.
      ELSE
        .dtpeststrt.ENABLED = .F.
      ENDIF
      .dtpactstrt.ENABLED = .T.
      .dtpactend.ENABLED = .T.
    ENDIF
  ENDIF

  IF m.cprj_stts = 'C'
    .dtpactstrt.VALUE = .dtpcalcstrt.VALUE
    .dtpactend.VALUE  = .dtpcalcend.VALUE
    lcprvalis = SELECT(0)
    SELECT (lc_pmprjdt)
    SCAN
      lcmcomps = 'Y'
      lcmcompf = 'Y'
      lcmcompd = 'Y'

      IF !EMPTY(&lc_pmprjdt..dact_strt)
        lcmcomps = 'N'
      ENDIF
      IF !EMPTY(&lc_pmprjdt..dact_fnsh)
        lcmcompf = 'N'
      ENDIF
      IF &lc_pmprjdt..nact_dur <> 0
        lcmcompd = 'N'
      ENDIF
      lcmcomplt = lcmcomps + lcmcompf + lcmcompd

      REPLACE dact_strt WITH IIF(lcmcomps = 'N',&lc_pmprjdt..dact_strt,&lc_pmprjdt..dclc_strt),;
        dact_fnsh WITH IIF(lcmcompf = 'N',&lc_pmprjdt..dact_fnsh,&lc_pmprjdt..dclc_fnsh),;
        nact_dur  WITH IIF(lcmcompd = 'N',&lc_pmprjdt..nact_dur,&lc_pmprjdt..nrem_dur),;
        cstatus   WITH 'M',;
        cmcomplt  WITH lcmcomplt

    ENDSCAN
    SELECT (lcprvalis)
  ELSE
    IF m.cprj_stts <> 'H'
      .dtpactstrt.VALUE = {}
      .dtpactend.VALUE = {}

      lcprvalis = SELECT(0)
      SELECT (lc_pmprjdt)
      SCAN
        lcmcomps = SUBSTR(&lc_pmprjdt..cmcomplt,1,1)
        lcmcompf = SUBSTR(&lc_pmprjdt..cmcomplt,2,1)
        lcmcompd = SUBSTR(&lc_pmprjdt..cmcomplt,3,1)

        REPLACE dact_strt WITH IIF(lcmcomps = 'N',&lc_pmprjdt..dact_strt,{}),;
          dact_fnsh WITH IIF(lcmcompf = 'N',&lc_pmprjdt..dact_fnsh,{}),;
          nact_dur  WITH IIF(lcmcompd = 'N',&lc_pmprjdt..nact_dur,0),;
          cstatus   WITH 'M'
      ENDSCAN
      SELECT (lcprvalis)
    ENDIF
  ENDIF
ENDWITH
*!*************************************************************
*! Name      : lfvStyle
*! Developer : Mariam Mazhar
*! Date      : 05/10/2009
*! Purpose   : Valid function for get field Style
*!*************************************************************
*! Calls              :   ARIABROW()
*!*************************************************************
*! Passed Parameters  :   None
*!*************************************************************
*! Returns            :   None
*!*************************************************************
*! Example            :  =lfvStyle()
*!*************************************************************
FUNCTION lfvstyle
PARAMETERS loformset
PRIVATE lncuralias
lcprj_typ = loformset.ariaform1.cbotrantype.VALUE
IF lcprj_typ  = 'H'
  RETURN
ENDIF
WITH loformset.ariaform1
  lcprjtype = .cbotrantype.DISPLAYVALUE
  llbrowse  = .kbstyle.selectedfrombrowse
  m.cstyle =  .kbstyle.keytextbox.VALUE
  m.cprj_id = .kbtranno.keytextbox.VALUE
  m.cprj_typ = lcprj_typ
  IF loformset.activemode <> 'S' OR (loformset.activemode = 'S' AND !EMPTY(m.cprj_id))
    m.cstyle   = IIF(!llbrowse .AND. EMPTY(m.cstyle), REPLICATE('*', loformset.lnmajorlen),;
      PADR(ALLTRIM(m.cstyle), loformset.lnmajorlen))


    IF gfseek(lcprj_typ+m.cprj_id+ PADR(m.cstyle,19),'PMPRJHD','PMPRJHD')
      .kbstyle.keytextbox.VALUE = m.cstyle
      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
      loformset.ariaform1.lststyle.ADDITEM (m.cstyle ,1)
      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
      .kbstyle.ENABLED = .F.
      loformset.lcstyle =  m.cstyle
      IF REPLICATE('*', loformset.lnmajorlen) = ALLTRIM(m.cstyle)
        .kbtranno.sharedvalidation
      ENDIF
      RETURN
    ENDIF
  ENDIF

  lncuralias = SELECT(0)

  .kbstyle.keytextbox.VALUE = m.cstyle
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
  loformset.ariaform1.lststyle.ADDITEM (m.cstyle ,1)
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]

  llbrowse   = llbrowse .OR. '?' $ m.cstyle
  loformset.lcstyle =  m.cstyle
  IF lcprj_typ  = 'S' AND EMPTY(m.cstyle) AND !llbrowse
    RETURN
  ENDIF

  IF lcprj_typ="S" AND !llbrowse AND lfvldstyle()
    *.kbTranNo.sharedvalidation
    RETURN
  ENDIF

ENDWITH

IF !llbrowse
  IF loformset.activemode $ "SV"
    SELECT pmprjhd
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
    IF REPLICATE('*', loformset.lnmajorlen) = ALLTRIM(m.cstyle)AND gfseek(lcprj_typ + m.cprj_id + PADR(m.cstyle,19))
      loformset.lcstyle =  m.cstyle
      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
    ELSE
      *-- Reposition the file pointer.
      =gfseek(lcprj_typ + m.cprj_id)

      IF lfvldstyle()
        *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
        IF '***' $ m.cstyle
          *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
          loformset.ariaform1.kbtranno.sharedvalidation
          *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
        ENDIF
        *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
        RETURN
      ELSE      && ELSE IF lfVldStyle()
        *-- If the style does not belong to the order, browse, or
        *-- reenter message.
        lcmsg =  lang_mfproj_style+' ' + ALLTRIM(m.cstyle) + lang_mfproj_notfound +;
          lcprjtype + IIF(lcprj_typ  = 'S',lang_mfproj_file,' #' +ALLTRIM(m.cprj_id))+ '.'
        IF gfmodalgen("TRM00000B00014","DIALOG",.F.,.F.,lcmsg) = 1
          llbrowse = .T.
        ELSE
          m.cstyle = SPACE(loformset.lnmajorlen)
          loformset.ariaform1.kbstyle.keytextbox.VALUE = m.cstyle
          *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
          loformset.ariaform1.lststyle.ADDITEM (m.cstyle ,1)
          *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
          loformset.ariaform1.kbstyle.keytextbox.ENABLED = .T.
          loformset.llsetfcstrnid = .T.
          RETURN .F.
        ENDIF    && ENDIF gfModalgen(..)
      ENDIF    && ENDIF lfVldStyle()
    ENDIF    && ENDIF SEEK(lcPrj_Typ + m.cPrj_ID + m.cStyle)
  ELSE
    *-- If the style is valid for the selected project, continue,
    *-- otherwise, browse.
    IF !lfvldstyle()
      llbrowse = .T.
    ELSE


      IF REPLICATE('*', loformset.lnmajorlen) = ALLTRIM(m.cstyle)
        loformset.changemode('A')
      ENDIF



      RETURN
    ENDIF
  ENDIF
ENDIF    && ENDIF !llBrowse
IF llbrowse
  DO CASE
  CASE  oariaapplication.activemoduleid = 'PO'
    =gfseek(IIF(lcprj_typ = 'N','NN',IIF(lcprj_typ = 'R','RP','PP'))+ m.cprj_id,'POSLN')
    SELECT DISTINCT SUBSTR(STYLE,1,loformset.lnmajorlen) AS STYLE  FROM posln INTO CURSOR 'TempStyle'


  CASE oariaapplication.activemoduleid = 'SO'
    =gfseek(lcprj_typ + m.cprj_id,'Ordline')
    SELECT DISTINCT SUBSTR(STYLE,1,loformset.lnmajorlen) AS STYLE  FROM ordline ;
      WHERE  cordtype+ORDER+STR(LINENO,6) = lcprj_typ + m.cprj_id  INTO CURSOR 'TempStyle'

    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
    *CASE  oAriaApplication.ActiveModuleID = 'MF' AND lcPrj_Typ <> 'C'
    *     =gfSeek('P'+lcPrj_Typ + m.cPrj_ID,'POSLN')
  CASE  oariaapplication.activemoduleid = 'MF' &&AND lcPrj_Typ <> 'C'
    =gfseek('P'+IIF(lcprj_typ = 'C','U',lcprj_typ ) + m.cprj_id,'POSLN')
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
    SELECT DISTINCT SUBSTR(STYLE,1,loformset.lnmajorlen) AS STYLE  FROM posln INTO CURSOR 'TempStyle'


  ENDCASE
  IF USED('TempStyle')
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
    SELECT (loformset.lcbrowcurs)
    DELETE ALL
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]

    SELECT tempstyle
    SCAN
      IF !SEEK(ALLTRIM(tempstyle.STYLE),loformset.lcbrowcurs)
        =gfseek(ALLTRIM(tempstyle.STYLE),'Style','Style')
        INSERT INTO (loformset.lcbrowcurs)(STYLE,DESC) VALUES ( tempstyle.STYLE,STYLE.DESC)
      ENDIF
    ENDSCAN
    lcbrowcurs = loformset.lcbrowcurs
    SELECT (lcbrowcurs)
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
    LOCATE
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
  ENDIF

  IF oariaapplication.activemoduleid = 'IC'
    SELECT STYLE
    gfsetorder('cStyle')
    lcbrfields = [cStyMajor: H=']+lang_mfproj_style+;
      [', Desc : H=']+lang_mfproj_desc+[',Status,Season :H=']+lang_mfproj_season  +;
      [',Cdivision :H = ']+ lang_mfproj_division+[' ]
  ELSE
    lcbrfields = [Style, Desc : H=']+lang_mfproj_desc+[']
  ENDIF

  DIMENSION laselected[1]
  lcfldname = 'STYLE'
  IF ariabrow('', lang_mfproj_styles,;
      gnbrfsrow1, gnbrfscol1, gnbrfsrow2, gnbrfscol2,.F.,.F.,lcfldname,'laSelected')

    m.cstyle  = STYLE
    loformset.lcstyle =  m.cstyle
    IF oariaapplication.activemoduleid = 'IC'
      SELECT STYLE
      =gfsetorder("Style")
      m.cstyle  = STYLE.cstymajor
    ENDIF

    loformset.ariaform1.kbstyle.keytextbox.VALUE = m.cstyle
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
    loformset.ariaform1.lststyle.ADDITEM (m.cstyle ,1)
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]


    *-- If coming from 'S'elect mode, i.e. the project ID is already
    *-- found in the projects file. Look for the entered style with
    *-- the projects file.
    IF loformset.activemode $ "SV"
      SELECT pmprjhd
      *-- If found, 'V'iew the entry,
      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
      *!*        IF gfSEEK(lcPrj_Typ + m.cPrj_ID + SUBSTR(m.cStyle,1,LEN(PMPRJHD.cStyle)))
      *!*          loFormSet.ChangeMode('V')
      IF .T.
        *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
        *-- If not found in the projects file, validate the entry
        *-- from the styles of the order.
        *-- If found, present a message with Add, Browse, or
        *-- Reeneter options, Otherwise, Present a not found message
        *-- <Browse>, <Reenter> option.
      ELSE
        *-- Reposition the file pointer.
        *loFormSet.ariaForm1.kbTranNo.sharedvalidation
        RETURN
      ENDIF     && ENDIF SEEK(lcPrj_Typ + m.cPrj_ID + m.cStyle)
    ELSE
      IF oariaapplication.activemoduleid = 'IC'
        SELECT STYLE
        =gfsetorder("Style")
      ENDIF

      *loFormSet.ChangeMode('A')

    ENDIF
  ELSE      && ELSE IF ARIABROW(..)

    loformset.ariaform1.kbstyle.keytextbox.VALUE = SPACE(12)
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
    loformset.ariaform1.lststyle.CLEAR()
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
    loformset.ariaform1.kbstyle.ENABLED = .T.
  ENDIF     && ENDIF ARIABROW(..)
  llbrowse = .F.
ENDIF

IF EMPTY(loformset.ariaform1.kbstyle.keytextbox.VALUE) AND;
    INLIST(oariaapplication.activemoduleid ,'PO','SO','IC')
  loformset.ariaform1.kbstyle.ENABLED = .T.
ELSE
  loformset.ariaform1.kbstyle.ENABLED = .F.
ENDIF


SELECT (lncuralias)
*!*************************************************************
*! Name      : lfVldStyle
*! Developer : Mariam Mazhar
*! Date      : 05/10/2009
*! Purpose   : Validates a style as belonging to an order or not.
*!*************************************************************
*! Calls              :   ARIABROW()
*!*************************************************************
*! Passed Parameters  :   None
*!*************************************************************
*! Returns            :  .T. if the style belongs to the project ID
*!                       .F. otherwise.
*!*************************************************************
*! Example            :  =lfVldStyle()
*!*************************************************************
FUNCTION lfvldstyle
PRIVATE lncuralias, llvldstyle
llvldstyle = .F.
IF m.cstyle = REPLICATE('*', loformset.lnmajorlen)
  llvldstyle = .T.
ELSE
  lncuralias = SELECT(0)
  DO CASE
  CASE lcprj_typ = 'O' OR lcprj_typ = 'T'
    SELECT ordline
    IF gfseek(lcprj_typ +m.cprj_id)
      LOCATE REST WHILE cordtype+ORDER+STR(LINENO,6)= lcprj_typ + m.cprj_id FOR SUBSTR(STYLE,1,LEN(m.cstyle)) = m.cstyle
      llvldstyle = FOUND()
    ENDIF
  CASE lcprj_typ = 'P' OR lcprj_typ = 'A' OR lcprj_typ = 'D' OR;
      lcprj_typ = 'N' OR lcprj_typ = 'R'
    llvldstyle = gfseek('P'+IIF(lcprj_typ = 'C','U',lcprj_typ )+m.cprj_id +'0001'+ ALLTRIM(m.cstyle), 'POSLN')
  CASE lcprj_typ = 'S'
    llvldstyle = gfseek(ALLTRIM(m.cstyle), 'Style', 'Style')
  ENDCASE
  SELECT (lncuralias)
ENDIF
RETURN llvldstyle

*!*************************************************************
*! Name      : lfvShw2Cus
*! Developer : Mariam Mazhar
*! Date      : 05/10/2009
*! Purpose   : Validates Show to customer check box.(lcShw2Cust)
*!*************************************************************
*! Calls              :  None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvShw2Cus()
*!*************************************************************
FUNCTION lfvshw2cus
PARAMETERS lobrformset
lc_pmprjdt = lobrformset.loparentform.lcprjdt

REPLACE &lc_pmprjdt..lshw2cust WITH lobrformset.ariaform1.chkshowcust.VALUE,;
  &lc_pmprjdt..cstatus   WITH SUBSTR('MMA', AT(&lc_pmprjdt..cstatus, 'SMA'), 1),;
  &lc_pmprjdt..cadd_user WITH oariaapplication.user_id ,;
  &lc_pmprjdt..dadd_date WITH oariaapplication.systemdate  ,;
  &lc_pmprjdt..cadd_time WITH TIME()


*!*************************************************************
*! Name      : lfvNotifyBt
*! Developer : Mariam Mazhar
*! Date      : 05/10/2009
*! Purpose   : Valid function for push button < Notify... >
*!*************************************************************
*! Calls              : gfMover
*!                      gfStr2Ar
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvNotifyBt()
*!*************************************************************
FUNCTION lfvnotifybt
PARAMETERS  lobrformset
*B609711,1 MMT 01/05/2012 Fix of Error 'gfOpenFile' is found while add activities[T20110808.0008][START]
*!*	lcSetProc = SET('PROCEDURE')
*!*	SET PROCEDURE TO
*B609711,1 MMT 01/05/2012 Fix of Error 'gfOpenFile' is found while add activities[T20110808.0008][END]
lcpmprjntf= lobrformset.loparentform.lcpmprjntf
lc_pmprjdt = lobrformset.loparentform.lcprjdt

lcprj_typ  = &lc_pmprjdt..cprj_typ
lcprj_id   = &lc_pmprjdt..cprj_id
lcstyle    = &lc_pmprjdt..cstyle
lcoprt_ctg = &lc_pmprjdt..coprt_ctg
lcoprt_id  = &lc_pmprjdt..coprt_id
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
lnlineno = &lc_pmprjdt..LINENO
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
lncuralias = SELECT(0)

SELECT (lcpmprjntf)

*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
*SET KEY TO PADR(lcPrj_Typ,1)+PADR(lcPrj_ID,6)+PADR(lcStyle,12)+lcOprt_Ctg+lcoprt_id
SET KEY TO PADR(lcprj_typ,1)+PADR(lcprj_id,6)+PADR(lcstyle,19)+STR(lnlineno ,6)+lcoprt_ctg+lcoprt_id
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]

LOCATE
lnbrrecno = RECNO()
SCATTER MEMVAR
DO FORM (oariaapplication.screenhome+ 'SM\smntfset.SCX') WITH .F.,lcoprt_ctg,lcoprt_id,lobrformset.loparentform.activemode

IF (lobrformset.loparentform.activemode <> 'V' )
  lobrformset.ariaform1.txtnotify.VALUE = ''
  SELECT (lcpmprjntf)
  SCAN FOR !EMPTY(cuser_id)
    lobrformset.ariaform1.txtnotify.VALUE  = lobrformset.ariaform1.txtnotify.VALUE + ALLTRIM(cuser_id) + '|'
  ENDSCAN
  IF !EMPTY(lobrformset.ariaform1.txtnotify.VALUE)
    lobrformset.ariaform1.txtnotify.VALUE = SUBSTR(lobrformset.ariaform1.txtnotify.VALUE , 1, RAT('|', lobrformset.ariaform1.txtnotify.VALUE)-1)
  ENDIF
  SELECT (lc_pmprjdt)
  REPLACE mnotify   WITH ALLTRIM(lobrformset.ariaform1.txtnotify.VALUE ),;
    cstatus   WITH SUBSTR('MMA', AT(cstatus, 'SMA'), 1),;
    cadd_user WITH oariaapplication.user_id,;
    dadd_date WITH oariaapplication.systemdate ,;
    cadd_time WITH TIME()

  lcnotify   = IIF(!EMPTY(lobrformset.ariaform1.txtnotify.VALUE ), STRTRAN(lobrformset.ariaform1.txtnotify.VALUE , '|', ', ') + '.', '')
ENDIF
SELECT (lcpmprjntf)
SET KEY TO
*B609711,1 MMT 01/05/2012 Fix of Error 'gfOpenFile' is found while add activities[T20110808.0008][START]
*SET PROCEDURE TO &lcSetProc
*B609711,1 MMT 01/05/2012 Fix of Error 'gfOpenFile' is found while add activities[T20110808.0008][END]
SELECT (lncuralias)

*!*************************************************************
*! Name      : lfvEstDur
*! Developer : Mariam Mazhar
*! Date      : 05/10/2009
*! Purpose   : Valid function for Estimated duration
*!*************************************************************
FUNCTION lfvestdur
PARAMETERS lobrformset
lc_pmprjdt = lobrformset.loparentform.lcprjdt
lcoldval = lobrformset.ariaform1.txtest.oldvalue
lcprj_typ  = &lc_pmprjdt..cprj_typ
lcprj_id   = &lc_pmprjdt..cprj_id
lcstyle    = &lc_pmprjdt..cstyle

*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
lnlineno = &lc_pmprjdt..LINENO
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]

PRIVATE lccurfld
IF lcoldval <> lobrformset.ariaform1.txtest.VALUE
  lncuralias = SELECT(0)
  SELECT (lc_pmprjdt)
  IF lobrformset.ariaform1.txtest.VALUE < 0 .AND. gfmodalgen("TRM38223B00000","DIALOG") = 1
    REPLACE nest_dur  WITH lcoldval
  ELSE
    REPLACE nrem_dur  WITH lobrformset.ariaform1.txtest.VALUE,;
      nest_dur  WITH lobrformset.ariaform1.txtest.VALUE,;
      cstatus   WITH SUBSTR('MMA', AT(cstatus, 'SMA'), 1),;
      cadd_user WITH oariaapplication.user_id,;
      dadd_date WITH oariaapplication.systemdate,;
      cadd_time WITH TIME()
  ENDIF
  SELECT (lncuralias)
ENDIF

*!*************************************************************
*! Name      : lfvDurtion
*! Developer : Mariam Mazhar
*! Date      : 05/10/2009
*! Purpose   : Valid function for duration
*!*************************************************************
FUNCTION lfvdurtion
PARAMETERS lcsrc,lobrformset
loformset = lobrformset
lc_pmprjdt = lobrformset.loparentform.lcprjdt
PRIVATE lccurfld
lncuralias = SELECT(0)

WITH lobrformset.ariaform1
  lcpredec = .txtpred.VALUE
  DO CASE
  CASE  lcsrc = 'R'
    lccurfld = .txtrem.VALUE
    lcoldval = .txtrem.oldvalue
  CASE  lcsrc = 'A'
    lccurfld = .txtact.VALUE
    lcoldval = .txtact.oldvalue

  CASE  lcsrc = 'P'
    lccurfld = .txtperc.VALUE
    lcoldval = .txtperc.oldvalue
  ENDCASE
ENDWITH

lcpredopr = &lc_pmprjdt..coprt_ctg+&lc_pmprjdt..coprt_id
IF lcoldval <> lccurfld
  lladdtoadt = .T.
  SELECT (lc_pmprjdt)
  IF lcsrc = 'A'
    SELECT (lc_pmprjdt)
    lnrecno = RECNO()
    SCAN FOR lcpredec $ (coprt_ctg + '\' + coprt_id)
      IF EMPTY(DTOS(dact_strt))
        =gfmodalgen("INM38245B00000","DIALOG")
        IF BETWEEN(lnrecno, 1, RECCOUNT())
          GO lnrecno
        ENDIF
        REPLACE nact_dur WITH 0
        REPLACE cstatus  WITH SUBSTR('MMA', AT(cstatus, 'SMA'), 1)
        lobrformset.ariaform1.txtact.VALUE = 0
        SELECT (lncuralias)
        RETURN
      ENDIF
    ENDSCAN
    IF BETWEEN(lnrecno, 1, RECCOUNT())
      GO lnrecno
    ENDIF
  ENDIF

  IF lccurfld  < 0 .AND. gfmodalgen("TRM38223B00000","DIALOG") = 1
    DO CASE
    CASE  lcsrc = 'R'
      lobrformset.ariaform1.txtrem.VALUE = lcoldval
      REPLACE nrem_dur WITH lcoldval
    CASE  lcsrc = 'A'
      lobrformset.ariaform1.txtact.VALUE = lcoldval
      REPLACE nact_dur WITH lcoldval
    CASE  lcsrc = 'P'
      lobrformset.ariaform1.txtperc.VALUE = lcoldval
      REPLACE nprc_comp WITH lcoldval
    ENDCASE
    REPLACE cstatus  WITH SUBSTR('MMA', AT(cstatus, 'SMA'), 1)
  ELSE
    DO CASE
    CASE lcsrc = 'R'
      REPLACE nrem_dur WITH lobrformset.ariaform1.txtrem.VALUE
    CASE lcsrc = 'A'
      REPLACE nact_dur WITH lobrformset.ariaform1.txtact.VALUE
    CASE lcsrc = 'P'
      REPLACE nprc_comp WITH lobrformset.ariaform1.txtperc.VALUE
    ENDCASE

    REPLACE cstatus   WITH SUBSTR('MMA', AT(cstatus, 'SMA'), 1),;
      cadd_user WITH oariaapplication.user_id,;
      dadd_date WITH  oariaapplication.systemdate,;
      cadd_time WITH TIME()

    glupdated = .T.
    IF lcsrc $ 'AR' .AND. lcoldval <> lccurfld
      lobrformset.loparentform.llchngoprt = .T.
      lobrformset.loparentform.llupdstrtd = .F.
    ENDIF

    IF lcsrc = 'A'
      IF EMPTY(DTOS(dact_strt))
        SELECT (lc_pmprjdt)
        REPLACE &lc_pmprjdt..dact_strt WITH IIF(EMPTY(&lc_pmprjdt..dclc_strt),;
          &lc_pmprjdt..dest_strt,&lc_pmprjdt..dclc_strt)

        REPLACE cstatus  WITH SUBSTR('MMA', AT(cstatus, 'SMA'), 1)
      ENDIF

      SELECT (lc_pmprjdt)
      lnwend = 0
      lndur  = &lc_pmprjdt..nact_dur
      loformset = lobrformset
      =lfgethlidy()

      SELECT(lc_pmprjdt)
      REPLACE dact_fnsh WITH (dact_strt + (nact_dur - 1) + lnwend),;
        nrem_dur  WITH 0,;
        cmcomplt  WITH 'YYY',;
        laddtoaud WITH .T.
      REPLACE cstatus  WITH SUBSTR('MMA', AT(cstatus, 'SMA'), 1)
      =lfcomtask()
    ELSE
      SELECT (lc_pmprjdt)
      REPLACE &lc_pmprjdt..cstatus   WITH SUBSTR('MMA', AT(cstatus, 'SMA'), 1)
    ENDIF
    SELECT (lc_pmprjdt)
    SET ORDER TO pmprjdt
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
    *!*	    =SEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID))+;
    *!*	          SUBSTR(lcStyle,1,LEN(cStyle)) + lcPredOpr)
    =SEEK(SUBSTR(lcprj_typ,1,LEN(cprj_typ)) + SUBSTR(lcprj_id,1,LEN(cprj_id))+;
      SUBSTR(lcstyle,1,LEN(cstyle)) +STR(lnlineno,6)+ lcpredopr)
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
  ENDIF
  SELECT (lncuralias)
ENDIF

*!*************************************************************
*! Name      : lfvUser
*! Developer : Mariam Mazhar
*! Date      : 05/10/2009
*! Purpose   : Valid function for user ID
*!*************************************************************
FUNCTION lfvuser
PARAMETERS lobrformset
lc_pmprjdt = lobrformset.loparentform.lcprjdt
lobrformset.ariaform1.txtrespon.VALUE  = SPACE(10)
REPLACE coprt_res WITH SPACE(10),;
  cstatus   WITH SUBSTR('MMA', AT(cstatus, 'SMA'), 1)    IN (lc_pmprjdt)


*!*************************************************************
*! Name      : lfvDetails
*! Developer : Mariam Mazhar
*! Date      : 05/10/2009
*! Purpose   : Validates detail fields
*!*************************************************************
*! Calls              :  None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvDetails()
*!*************************************************************
FUNCTION lfvdetails
PARAMETERS lcsrc,lobrformset
lc_pmprjdt = lobrformset.loparentform.lcprjdt
PRIVATE lccurobj

lncuralias = SELECT(0)

IF lcsrc= 'R'
  lccurobj   = lobrformset.ariaform1.txtrespon.VALUE
  lcoprt_res = lobrformset.ariaform1.txtrespon.VALUE
  lcoldval   = lobrformset.ariaform1.txtrespon.oldvalue
  lcsavfield  = 'coprt_res'
ENDIF
lcsetexact = SET('EXACT')
SET EXACT ON

IF lcsrc= 'R'
  DO CASE
  CASE lobrformset.ariaform1.cborespon.VALUE  = 1
    SELECT syuuser
    lcfile  = 'SYUUSER'
    lcbrfields = [cUser_Id : H = ']+lang_mfproj_usrid+[', cUsr_Name : H = ']+lang_mfproj_usrname+[']
    lcfldname  = 'cUser_Id'
    lcsavfield = 'COPRT_RES'
  CASE lobrformset.ariaform1.cborespon.VALUE  = 2
    SELECT syugroup
    lcfile  = 'SYUGROUP'
    lcbrfields = [cGroup_Id : H = ']+lang_mfproj_grpid+[', cGroup_Nam : H = ']+lang_mfproj_grpname     +[']
    lcfldname  = 'cGroup_Id'
    lcsavfield = 'cGroup_Id'
  ENDCASE
  IF lcsrc= 'R' AND !EMPTY(lcoprt_res) AND !gfseek(ALLTRIM(lcoprt_res),lcfile)
    DIMENSION latemp[1]
    IF ariabrow('',lang_mfproj_respousr  ,gnbrfsrow1, gnbrfscol1, gnbrfsrow2, gnbrfscol2,.F.,.F.,lcfldname,'laTemp')
      lcoprt_res = latemp[1]
      lobrformset.ariaform1.txtrespon.VALUE = lcoprt_res
    ELSE
      lcoprt_res = SPACE(10)
      lobrformset.ariaform1.txtrespon.VALUE = lcoprt_res
    ENDIF
  ENDIF

ELSE
  lcfield    = lccurobj
ENDIF

SET EXACT &lcsetexact

IF lobrformset.ariaform1.txtrespon.VALUE <> lcoldval

  SELECT (lc_pmprjdt)
  IF lcsrc = 'R'
    REPLACE coprt_res  WITH ' ',;
      cgroup_id  WITH ' ',;
      laddtoaud  WITH .T.
    lladdtoadt = .T.
  ENDIF
  REPLACE &lcsavfield   WITH IIF(lcsrc = 'R',ALLTRIM(lobrformset.ariaform1.txtrespon.VALUE),EVALUATE(lccurobj)),;
    cstatus    WITH SUBSTR('MMA', AT(cstatus, 'SMA'), 1),;
    cadd_user  WITH oariaapplication.user_id,;
    dadd_date  WITH oariaapplication.systemdate,;
    cadd_time  WITH TIME()
  glupdated = .T.
ENDIF
SELECT (lncuralias)


*!*************************************************************
*! Name      : lfvUpdMthd
*! Developer : Mariam Mazhar
*! Date      : 05/10/2009
*! Purpose   : Update Method popup valid function.
*!*************************************************************
*! Calls              : gfActPop()
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvUpdMthd()
*!*************************************************************
FUNCTION lfvupdmthd
PARAMETERS lobrformset
lc_pmprjdt = lobrformset.loparentform.lcprjdt
lcupdtmthd = lobrformset.ariaform1.cboupmeth.VALUE

REPLACE &lc_pmprjdt..cupdtmthd WITH lcupdtmthd,;
  &lc_pmprjdt..cstatus   WITH SUBSTR('MMA', AT(&lc_pmprjdt..cstatus, 'SMA'), 1),;
  &lc_pmprjdt..cadd_user WITH oariaapplication.user_id,;
  &lc_pmprjdt..dadd_date WITH oariaapplication.systemdate,;
  &lc_pmprjdt..cadd_time WITH TIME()
glupdated   = .T.

*!*************************************************************
*! Name      : LFWHENFN
*! Developer : Mariam Mazhar
*! Date      : 05/10/2009
*! Purpose   : when function of option grid
*!*************************************************************
FUNCTION lfwhenfn
DIMENSION laprjdes[2],laprjval[2]
STORE '' TO laprjdes,laprjval
laprjdes[1] = lang_mfproj_style
laprjval[1] = 'S'
laprjdes[2] = lang_mfproj_other
laprjval[2] = 'H'
lnvalcnt = 3
IF 'MF' $ oariaapplication.companyinstalledmodules
  DIMENSION laprjdes[ALEN(laPrjDes)+3],laprjval[ALEN(laPrjVal)+3]
  laprjdes[lnValCnt] = lang_mfproj_ctktkt
  laprjval[lnValCnt] = 'C'
  lnvalcnt = lnvalcnt + 1
  laprjdes[lnValCnt] = lang_mfproj_adororder
  laprjval[lnValCnt] = 'A'
  lnvalcnt = lnvalcnt + 1
  laprjdes[lnValCnt] = lang_mfproj_dyeorder
  laprjval[lnValCnt] = 'D'
  lnvalcnt = lnvalcnt + 1
ENDIF

IF 'MA' $ oariaapplication.companyinstalledmodules
  DIMENSION laprjdes[ALEN(laPrjDes)+1],laprjval[ALEN(laPrjVal)+1]
  laprjdes[lnValCnt] =  lang_mfproj_material
  laprjval[lnValCnt] = 'M'
  lnvalcnt = lnvalcnt + 1
ENDIF


IF 'PO' $ oariaapplication.companyinstalledmodules
  DIMENSION laprjdes[ALEN(laPrjDes)+3],laprjval[ALEN(laPrjVal)+3]
  laprjdes[lnValCnt] = lang_mfproj_po
  laprjval[lnValCnt] = 'P'
  lnvalcnt = lnvalcnt + 1
  laprjdes[lnValCnt] = lang_mfproj_intrpo
  laprjval[lnValCnt] = 'N'
  lnvalcnt = lnvalcnt + 1
  laprjdes[lnValCnt] = lang_mfproj_retpo
  laprjval[lnValCnt] = 'R'
  lnvalcnt = lnvalcnt + 1
ENDIF

IF 'SO' $ oariaapplication.companyinstalledmodules
  DIMENSION laprjdes[ALEN(laPrjDes)+2],laprjval[ALEN(laPrjVal)+2]
  laprjdes[lnValCnt] = lang_mfproj_salord
  laprjval[lnValCnt] = 'O'
  lnvalcnt = lnvalcnt + 1
  laprjdes[lnValCnt] =  lang_mfproj_tmpedi
  laprjval[lnValCnt] = 'T'
  lnvalcnt = lnvalcnt + 1
ENDIF

IF TYPE('lcPrj_Typ') <> 'U'
  lcrpprjtyp = lcprj_typ
  ldrpschdt = dsch_date
  lctempprj = loogscroll.gftempname()
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
  *CREATE CURSOR (lcTempPrj) (KeyExp C(19))
  CREATE CURSOR (lctempprj) (keyexp c(33))
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
  SELECT (lctempprj )
  INDEX ON keyexp TAG (lctempprj )
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
  *INSERT INTO (lcTempPrj ) VALUES (PADR(lcPrj_ID,6)+'-'+SUBSTR(lcStyle,1,12))
  INSERT INTO (lctempprj) VALUES (PADR(lcprj_id,6)+'+'+SUBSTR(lcstyle,1,19)+'+'+STR(lnlineno,6))
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
  lnprjid= ASCAN(loogscroll.laogfxflt,"PMPRJHD.CPRJ_ID")
  IF lnprjid > 0
    lnprjid = ASUBSCRIPT(loogscroll.laogfxflt,lnprjid,1)
    loogscroll.laogfxflt[lnPrjId,6] = lctempprj
  ENDIF

  IF lleditaddscr
    lleditadd = .T.
    lctmpprjrl = lcrelfile
    lctmpprjdt = lcdetfile
    lcprjhder = lchdfile
    lnfrmdtse = lncurdtses
  ENDIF
  =loogscroll.refreshscroll()
  loogscroll.setfilterexpr()
  lcxmlstr = loogscroll.convertvariablestoxml()
  STRTOFILE(lcxmlstr,oariaapplication.outputhome+lcxmlflname+'.xml')
ENDIF
IF TYPE('llCallFromRequestHandler') = 'L' .AND. llcallfromrequesthandler
  RETURN .T.
ELSE
  RETURN .F.
ENDIF

*!*************************************************************
*! Name      : lfvDESC
*! Developer : Mariam Mazhar
*! Date      : 05/10/2009
*! Purpose   : function to update description
*!*************************************************************
FUNCTION lfvdesc
PARAMETERS lobrformset
lc_pmprjdt = lobrformset.loparentform.lcprjdt
REPLACE coprt_dsc WITH lobrformset.ariaform1.cnttaskdet.txttaskname.VALUE ,;
  cstatus   WITH SUBSTR('MMA', AT(cstatus, 'SMA'), 1)    IN (lc_pmprjdt)

*!*************************************************************
*! Name      : lfNtfInit
*! Developer : Mariam Mazhar
*! Date      : 05/10/2009
*! Purpose   : init function of Notification screen
*!*************************************************************
FUNCTION lfntfinit
PARAMETERS lontfrmset,lcpathid, lccatgid, lctaskid, lcmode

WITH lontfrmset
  .lcmode   = lcmode
  .lctaskid = lctaskid
  .lccatg   = lccatgid
  .lcpathid = lcpathid
ENDWITH

WITH lontfrmset.ariaform1.ariacontainer2
  .txtemail.INPUTMASK = REPLICATE('X',60)
  .userkey.keytextbox.INPUTMASK = REPLICATE('!',10)
  .txtnbc.VALUE = 0
  .txtnosd.VALUE = 0
  .txtnocd.VALUE = 0
  .txtnbs.VALUE = 0
ENDWITH

lontfrmset.lcscreentable = lcpmprjntf
IF !lontfrmset.llifok
  WITH lontfrmset.ariaform1.ariacontainer1.grdusers
    .RECORDSOURCE = lcpmprjntf
    .column1.CONTROLSOURCE = 'cuser_id'
    .column2.CONTROLSOURCE = 'nbfrstrtdy'
    .column3.CONTROLSOURCE = 'lonstrt'
    .column4.CONTROLSOURCE = 'nstrtdelay'
    .column5.CONTROLSOURCE = 'lonredrct'

    .column6.CONTROLSOURCE = 'nbfrcmpldy'
    .column7.CONTROLSOURCE = 'loncmplt'

    .column8.CONTROLSOURCE = 'ncmpldelay'
    lontfrmset.llifok = .T.
  ENDWITH
ENDIF

IF !EOF(lcpmprjntf)
  lontfrmset.ariaform1.ariacontainer1.grdusers.REFRESH()
  lontfrmset.ariaform1.ariacontainer1.grdusers.AFTERROWCOLCHANGE
ELSE
  WITH lontfrmset.ariaform1.ariacontainer2
    .userkey.ENABLED = .F.
    .txtemail.ENABLED = .F.
    .cbnotbefst.ENABLED = .F.
    .cbnotbefcom.ENABLED = .F.
    .cbnor.ENABLED = .F.
    .cbnoc.ENABLED = .F.
    .cbnos.ENABLED = .F.
    .txtnbc.ENABLED = .F.
    .txtnbs.ENABLED = .F.
    .txtnosd.ENABLED =.F.
    .txtnocd.ENABLED = .F.
    .chkcmpdely.ENABLED = .F.
    .chkstrtdely.ENABLED = .F.
  ENDWITH
ENDIF

lontfrmset.ariaform1.ariacontainer1.grdusers.REFRESH()
lontfrmset.changemode(lcmode)

IF EOF(lcpmprjntf)
  WITH lontfrmset.ariaform1.ariacontainer2
    .userkey.ENABLED = .F.
    .txtemail.ENABLED = .F.
    .cbnotbefst.ENABLED = .F.
    .cbnotbefcom.ENABLED = .F.
    .cbnor.ENABLED = .F.
    .cbnoc.ENABLED = .F.
    .cbnos.ENABLED = .F.
    .txtnbc.ENABLED = .F.
    .txtnbs.ENABLED = .F.
    .txtnosd.ENABLED =.F.
    .txtnocd.ENABLED = .F.
    .chkcmpdely.ENABLED = .F.
    .chkstrtdely.ENABLED = .F.
  ENDWITH
ENDIF


*!*************************************************************
*! Name      : lfChngMode
*! Developer : Mariam Mazhar
*! Date      : 05/10/2009
*! Purpose   : Change mode function of Notification screen
*!*************************************************************
FUNCTION lfchngmode
PARAMETERS lontfrmset

*!*************************************************************
*! Name      : lfAftrRowCol
*! Developer : Mariam Mazhar
*! Date      : 05/10/2009
*! Purpose   : grid after row clo. chnaged function of Notification screen
*!*************************************************************
FUNCTION lfaftrrowcol
PARAMETERS lontfrmset
lcscreentable = lontfrmset.lcscreentable
IF !EOF()
  WITH lontfrmset.ariaform1.ariacontainer2
    .userkey.keytextbox.VALUE = &lcscreentable..cuser_id
    .txtemail.VALUE = &lcscreentable..cemail_add
    .cbnotbefst.VALUE = &lcscreentable..lbfrstrt
    .cbnotbefcom.VALUE = &lcscreentable..lbfrcmplt
    .cbnor.VALUE = &lcscreentable..lonredrct
    .cbnoc.VALUE = &lcscreentable..loncmplt
    .cbnos.VALUE = &lcscreentable..lonstrt
    .txtnbc.VALUE = &lcscreentable..nbfrcmpldy
    .txtnbs.VALUE = &lcscreentable..nbfrstrtdy
    .txtnosd.VALUE = &lcscreentable..nstrtdelay
    .txtnocd.VALUE = &lcscreentable..ncmpldelay
    .chkcmpdely.VALUE  = &lcscreentable..lcmpldelay
    .chkstrtdely.VALUE  = &lcscreentable..lstrtdelay
  ENDWITH

  IF &lcscreentable..lbfrstrt
    lontfrmset.ariaform1.ariacontainer2.txtnbs.ENABLED = !(lontfrmset.activemode = 'V')
  ELSE
    lontfrmset.ariaform1.ariacontainer2.txtnbs.ENABLED = .F.
  ENDIF

  IF &lcscreentable..lbfrcmplt
    lontfrmset.ariaform1.ariacontainer2.txtnbc.ENABLED = !(lontfrmset.activemode = 'V')
  ELSE
    lontfrmset.ariaform1.ariacontainer2.txtnbc.ENABLED = .F.
  ENDIF

  IF &lcscreentable..lstrtdelay
    lontfrmset.ariaform1.ariacontainer2.txtnosd.ENABLED = !(lontfrmset.activemode = 'V')
  ELSE
    lontfrmset.ariaform1.ariacontainer2.txtnosd.ENABLED = .F.
  ENDIF

  IF &lcscreentable..lcmpldelay
    lontfrmset.ariaform1.ariacontainer2.txtnocd.ENABLED = !(lontfrmset.activemode = 'V')
  ELSE
    lontfrmset.ariaform1.ariacontainer2.txtnocd.ENABLED = .F.
  ENDIF
  lontfrmset.ariaform1.ariacontainer3.btnremove.ENABLED = !(lontfrmset.activemode = 'V')

  WITH lontfrmset.ariaform1.ariacontainer2
    .userkey.ENABLED = !(lontfrmset.activemode = 'V')
    .txtemail.ENABLED = !(lontfrmset.activemode = 'V')
    .cbnotbefst.ENABLED = !(lontfrmset.activemode = 'V')
    .cbnotbefcom.ENABLED = !(lontfrmset.activemode = 'V')
    .cbnor.ENABLED = !(lontfrmset.activemode = 'V')
    .cbnoc.ENABLED = !(lontfrmset.activemode = 'V')
    .cbnos.ENABLED = !(lontfrmset.activemode = 'V')
    .txtnbc.ENABLED = !(lontfrmset.activemode = 'V')
    .txtnbs.ENABLED = !(lontfrmset.activemode = 'V')
    .txtnosd.ENABLED =!(lontfrmset.activemode = 'V') AND &lcscreentable..lstrtdelay
    .txtnocd.ENABLED = !(lontfrmset.activemode = 'V') AND &lcscreentable..lcmpldelay
    .chkcmpdely.ENABLED = !(lontfrmset.activemode = 'V')
    .chkstrtdely.ENABLED = !(lontfrmset.activemode = 'V')

  ENDWITH
ENDIF

*!*************************************************************
*! Name      : lfvUsrId
*! Developer : Mariam Mazhar
*! Date      : 05/10/2009
*! Purpose   : validate user id function of Notification screen
*!*************************************************************
FUNCTION lfvusrid
PARAMETERS lontfrmset


WITH lontfrmset.ariaform1.ariacontainer2.userkey
  lcoldval   = .keytextbox.oldvalue
  m.cuser_id = .keytextbox.VALUE
  llbrowse   = .selectedfrombrowse
ENDWITH

IF (PADR(m.cuser_id,10) = PADR(lcoldval,10)) .AND. !llbrowse
  RETURN .T.
ENDIF

PRIVATE lncuralias
lncuralias = SELECT(0)
SELECT syuuser
lcbrfields = [cUser_Id : H = ']+lang_mfproj_usrid  +[', cUsr_Name : H = ']+lang_mfproj_usrname+[']
IF (!EMPTY(m.cuser_id) AND !gfseek(PADR(m.cuser_id,10),'SYUUSER')) .OR. llbrowse
  llbrowse = .F.
  DIMENSION latemp[2]
  IF ariabrow('',lang_mfproj_users,gnbrfsrow1, gnbrfscol1, gnbrfsrow2, gnbrfscol2,.F.,.F.,'cUser_Id,cEmail_Add','laTemp')
    m.cuser_id   = latemp[1]
    m.cemail_add = latemp[2]
  ELSE
    m.cuser_id   = lcoldval
  ENDIF
  lontfrmset.ariaform1.ariacontainer2.userkey.keytextbox.VALUE = m.cuser_id
  lcoldval = lontfrmset.ariaform1.ariacontainer2.userkey.keytextbox.oldvalue
  IF !EMPTY(m.cuser_id) &&AND m.cUser_ID <> lcOldVal
    lntmprecno = RECNO(lcpmprjntf)
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
    *IF !SEEK(PADR(lcPrj_Typ,1)+PADR(lcPrj_ID,6)+PADR(lcStyle,12)+lcOprt_Ctg+lcoprt_id+m.cUser_ID,lcPMPRJNTF)
    IF !SEEK(PADR(lcprj_typ,1)+PADR(lcprj_id,6)+PADR(lcstyle,19)+STR(lnlineno ,6)+lcoprt_ctg+lcoprt_id+m.cuser_id,lcpmprjntf)
      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
      m.cemail_add = syuuser.cemail_add
    ELSE
      WAIT WINDOW "User Exist"
      m.cuser_id = lcoldval
    ENDIF
    IF BETWEEN(lntmprecno,1,RECCOUNT(lcpmprjntf))
      GO lntmprecno IN (lcpmprjntf)
    ENDIF
  ENDIF

ELSE
  IF !EMPTY(m.cuser_id)
    lntmprecno = RECNO(lcpmprjntf)
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
    *IF !SEEK(PADR(lcPrj_Typ,1)+PADR(lcPrj_ID,6)+PADR(lcStyle,12)+lcOprt_Ctg+lcoprt_id+m.cUser_ID,lcPMPRJNTF)
    IF !SEEK(PADR(lcprj_typ,1)+PADR(lcprj_id,6)+PADR(lcstyle,19)+STR(lnlineno ,6)+lcoprt_ctg+lcoprt_id+m.cuser_id,lcpmprjntf)
      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
      m.cemail_add = syuuser.cemail_add
    ELSE
      WAIT WINDOW lang_mfproj_usrexist
      m.cuser_id = lcoldval
    ENDIF
    IF BETWEEN(lntmprecno,1,RECCOUNT(lcpmprjntf))
      GO lntmprecno IN (lcpmprjntf)
    ENDIF
  ENDIF
ENDIF

SELECT (lcpmprjntf)
REPLACE cuser_id   WITH m.cuser_id,;
  cemail_add WITH m.cemail_add,;
  cstatus    WITH SUBSTR('MMA', AT(cstatus, 'SMA'), 1)

glupdated = .T.
SELECT (lncuralias)
lontfrmset.ariaform1.ariacontainer1.grdusers.AFTERROWCOLCHANGE

*!*************************************************************
*! Name      : lfShow
*! Developer : Mariam Mazhar
*! Date      : 05/10/2009
*! Purpose   : refresh function of Notification screen
*!*************************************************************
FUNCTION lfshow
PARAMETERS lontfrmset
WITH lontfrmset.ariaform1.ariacontainer2
  .userkey.ENABLED = .T.
  .txtemail.ENABLED = .T.
  .cbnotbefst.ENABLED = .T.
  .cbnotbefcom.ENABLED = .T.
  .cbnor.ENABLED = .T.
  .cbnoc.ENABLED = .T.
  .cbnos.ENABLED = .T.
  .txtnbc.ENABLED = .T.
  .txtnbs.ENABLED = .T.
  .txtnosd.ENABLED = .T.
  .txtnocd.ENABLED = .T.
  .chkcmpdely.ENABLED = .T.
  .chkstrtdely.ENABLED = .T.
ENDWITH

WITH lontfrmset.ariaform1.ariacontainer3
  .btnremove.ENABLED = .T.
  .btnnew.ENABLED = .T.
  .btnclose.ENABLED = .T.
ENDWITH

*!*************************************************************
*! Name      : lfvEmail
*! Developer : Mariam Mazhar
*! Date      : 05/10/2009
*! Purpose   : Emial validation function of Notification screen
*!*************************************************************
FUNCTION lfvemail
PARAMETERS lontfrmset
lcvalue = lontfrmset.ariaform1.ariacontainer2.txtemail.VALUE
REPLACE cemail_add WITH lcvalue,;
  cstatus   WITH SUBSTR('MMA', AT(cstatus, 'SMA'), 1) IN  (lcpmprjntf)
glupdated = .T.
lontfrmset.ariaform1.ariacontainer1.grdusers.REFRESH



*!*************************************************************
*! Name      : lfvEmail
*! Developer : Mariam Mazhar
*! Date      : 05/10/2009
*! Purpose   : notify before start validation function of Notification screen
*!*************************************************************
FUNCTION lfvntb4strt
PARAMETERS lontfrmset
WITH lontfrmset.ariaform1.ariacontainer2
  REPLACE lbfrstrt WITH .cbnotbefst.VALUE ,;
    cstatus   WITH SUBSTR('MMA', AT(cstatus, 'SMA'), 1) IN  (lcpmprjntf)

  IF .cbnotbefst.VALUE
    .txtnbs.ENABLED = .T.
    .txtnbs.VALUE = 1
    REPLACE nbfrstrtdy WITH 1 IN (lcpmprjntf)
  ELSE
    .txtnbs.ENABLED = .F.
    .txtnbs.VALUE = 0
    REPLACE nbfrstrtdy WITH 0 IN (lcpmprjntf)
  ENDIF
ENDWITH
lontfrmset.ariaform1.ariacontainer1.grdusers.REFRESH

*!*************************************************************
*! Name      : lfvEmail
*! Developer : Mariam Mazhar
*! Date      : 05/10/2009
*! Purpose   : notify before start days validation function of Notification screen
*!*************************************************************
FUNCTION lfvntfb4strtdays
PARAMETERS lontfrmset

WITH lontfrmset.ariaform1
  REPLACE nbfrstrtdy  WITH .ariacontainer2.txtnbs.VALUE,;
    cstatus   WITH SUBSTR('MMA', AT(cstatus, 'SMA'), 1) IN  (lcpmprjntf)
  .ariacontainer1.grdusers.REFRESH
ENDWITH
*!*************************************************************
*! Name      : lfvEmail
*! Developer : Mariam Mazhar
*! Date      : 05/10/2009
*! Purpose   : notify before Complete validation function of Notification screen
*!*************************************************************
FUNCTION lfvntfb4comp
PARAMETERS lontfrmset

WITH lontfrmset.ariaform1.ariacontainer2
  REPLACE lbfrcmplt WITH .cbnotbefcom.VALUE ,;
    cstatus   WITH SUBSTR('MMA', AT(cstatus, 'SMA'), 1) IN  (lcpmprjntf)

  IF .cbnotbefcom.VALUE  = .F.
    .txtnbc.ENABLED = .F.
    .txtnbc.VALUE = 0
  ELSE
    .txtnbc.ENABLED = .T.
    .txtnbc.VALUE = 1
  ENDIF
  REPLACE nbfrcmpldy WITH .txtnbc.VALUE  IN  (lcpmprjntf)
ENDWITH
lontfrmset.ariaform1.ariacontainer1.grdusers.REFRESH

*!*************************************************************
*! Name      : lfvEmail
*! Developer : Mariam Mazhar
*! Date      : 05/10/2009
*! Purpose   : notify before Complete days validation function of Notification screen
*!*************************************************************
FUNCTION lfvntfb4cmpl
PARAMETERS lontfrmset
WITH lontfrmset.ariaform1
  REPLACE nbfrcmpldy WITH .ariacontainer2.txtnbc.VALUE,;
    cstatus   WITH SUBSTR('MMA', AT(cstatus, 'SMA'), 1)  IN  (lcpmprjntf)
  .ariacontainer1.grdusers.REFRESH
ENDWITH
*!*************************************************************
*! Name      : lfvEmail
*! Developer : Mariam Mazhar
*! Date      : 05/10/2009
*! Purpose   : notify before start delay validation function of Notification screen
*!*************************************************************
FUNCTION lfvntfstrtdl
PARAMETERS lontfrmset
WITH lontfrmset.ariaform1
  REPLACE nstrtdelay WITH .ariacontainer2.txtnosd.VALUE,;
    cstatus   WITH SUBSTR('MMA', AT(cstatus, 'SMA'), 1)  IN (lcpmprjntf)
  .ariacontainer1.grdusers.REFRESH
ENDWITH
*!*************************************************************
*! Name      : lfvEmail
*! Developer : Mariam Mazhar
*! Date      : 05/10/2009
*! Purpose   : notify before Complete delay validation function of Notification screen
*!*************************************************************
FUNCTION lfvntfcd
PARAMETERS lontfrmset
WITH lontfrmset.ariaform1
  REPLACE ncmpldelay WITH .ariacontainer2.txtnocd.VALUE,;
    cstatus   WITH SUBSTR('MMA', AT(cstatus, 'SMA'), 1)  IN (lcpmprjntf)
  .ariacontainer1.grdusers.REFRESH
ENDWITH

*!*************************************************************
*! Name      : lfvEmail
*! Developer : Mariam Mazhar
*! Date      : 05/10/2009
*! Purpose   : notify on Start validation function of Notification screen
*!*************************************************************
FUNCTION lfvntfonstrt
PARAMETERS lontfrmset
WITH lontfrmset.ariaform1
  REPLACE lonstrt WITH .ariacontainer2.cbnos.VALUE ,;
    cstatus   WITH SUBSTR('MMA', AT(cstatus, 'SMA'), 1) IN (lcpmprjntf)
  .ariacontainer1.grdusers.REFRESH
ENDWITH
*!*************************************************************
*! Name      : lfvEmail
*! Developer : Mariam Mazhar
*! Date      : 05/10/2009
*! Purpose   : notify on Complete validation function of Notification screen
*!*************************************************************
FUNCTION lfvntfoncomp
PARAMETERS lontfrmset
WITH lontfrmset.ariaform1
  REPLACE loncmplt WITH .ariacontainer2.cbnoc.VALUE,;
    cstatus   WITH SUBSTR('MMA', AT(cstatus, 'SMA'), 1)  IN (lcpmprjntf)
  .ariacontainer1.grdusers.REFRESH
ENDWITH
*!*************************************************************
*! Name      : lfvEmail
*! Developer : Mariam Mazhar
*! Date      : 05/10/2009
*! Purpose   : notify on redirect validation function of Notification screen
*!*************************************************************
FUNCTION lfvntfonredir
PARAMETERS lontfrmset
WITH lontfrmset.ariaform1
  REPLACE lonredrct WITH .ariacontainer2.cbnor.VALUE ,;
    cstatus   WITH SUBSTR('MMA', AT(cstatus, 'SMA'), 1) IN (lcpmprjntf)
  .ariacontainer1.grdusers.REFRESH
ENDWITH
*!*************************************************************
*! Name      : lfvEmail
*! Developer : Mariam Mazhar
*! Date      : 05/10/2009
*! Purpose   : Remove user validation function of Notification screen
*!*************************************************************
FUNCTION lfvrmvuser
PARAMETERS lontfrmset

lncuralias = SELECT(0)
IF gfmodalgen("TRM38208B38006","DIALOG") = 1
  SELECT (lcpmprjntf)
  REPLACE cstatus WITH SUBSTR('DDS', AT(cstatus, 'SMA'), 1)
  DELETE
  lnrenum = RECNO()
  lnnotdeleted = 0
  COUNT FOR !DELETED() TO lnnotdeleted
  IF lnnotdeleted = 0
    lontfrmset.lfclear()
  ENDIF
  SELECT (lcpmprjntf)
  IF BETWEEN(lnrenum,1,RECCOUNT())
    GO RECORD lnrenum
  ENDIF
  *LOCATE
  IF lnnotdeleted = 0
    lontfrmset.ariaform1.ariacontainer1.grdusers.REFRESH()
    lontfrmset.ariaform1.ariacontainer3.btnremove.ENABLED = .F.
    WITH lontfrmset.ariaform1.ariacontainer2
      .userkey.ENABLED = .F.
      .txtemail.ENABLED = .F.
      .cbnotbefst.ENABLED = .F.
      .cbnotbefcom.ENABLED = .F.
      .cbnor.ENABLED = .F.
      .cbnoc.ENABLED = .F.
      .cbnos.ENABLED = .F.
      .txtnbc.ENABLED = .F.
      .txtnbs.ENABLED = .F.
      .txtnosd.ENABLED = .F.
      .txtnocd.ENABLED = .F.
      .chkcmpdely.ENABLED = .F.
      .chkstrtdely.ENABLED = .F.
    ENDWITH
  ENDIF
  glupdated   = .T.
  lontfrmset.ariaform1.ariacontainer1.grdusers.REFRESH()
ENDIF
SELECT (lncuralias)

*!*************************************************************
*! Name      : lfvClosNtf
*! Developer : Mariam Mazhar
*! Date      : 05/10/2009
*! Purpose   : Close button function of Notification screen
*!*************************************************************
FUNCTION lfvclosntf
PARAMETERS lontfrmset
SELECT (lcpmprjntf)
SCAN FOR EMPTY(cuser_id)
  REPLACE cstatus WITH SUBSTR('DDS', AT(cstatus, 'SMA'), 1)
ENDSCAN
DELETE FOR EMPTY(cuser_id)

*!*************************************************************
*! Name      : lfvNewUsr
*! Developer : Mariam Mazhar
*! Date      : 05/10/2009
*! Purpose   : New  button function of Notification screen
*!*************************************************************
FUNCTION lfvnewusr
PARAMETERS lontfrmset
SELECT (lcpmprjntf)

*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
*!*  IF !SEEK(PADR(lcPrj_Typ,1)+PADR(lcPrj_ID,6)+PADR(lcStyle,12)+lcOprt_Ctg+lcOprt_id+SPACE(10))
*APPEND BLANK
IF !SEEK(PADR(lcprj_typ,1)+PADR(lcprj_id,6)+PADR(lcstyle,19)+STR(lnlineno ,6)+lcoprt_ctg+lcoprt_id+SPACE(10))

  m.cprj_typ  = lcprj_typ
  m.cprj_id  = lcprj_id
  m.cstyle = lcstyle
  m.coprt_ctg = lcoprt_ctg
  m.coprt_id = lcoprt_id
  m.lineno   =lnlineno
  INSERT INTO (lcpmprjntf) FROM MEMVAR
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
ENDIF
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
*!*  REPLACE cStatus   WITH "A",;
*!*          cPrj_Typ  WITH PADR(lcPrj_Typ,1),;
*!*          cPrj_ID   WITH PADR(lcPrj_ID,6),;
*!*          cStyle    WITH PADR(lcStyle,12),;
*!*          COprt_Ctg WITH lcOprt_Ctg,;
*!*          COprt_ID  WITH lcOprt_id
REPLACE cstatus   WITH "A",;
  cprj_typ  WITH PADR(lcprj_typ,1),;
  cprj_id   WITH PADR(lcprj_id,6),;
  cstyle    WITH PADR(lcstyle,19),;
  coprt_ctg WITH lcoprt_ctg,;
  coprt_id  WITH lcoprt_id,;
  LINENO    WITH lnlineno
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
WITH lontfrmset.ariaform1.ariacontainer2
  .userkey.ENABLED = .T.
  .txtemail.ENABLED = .F.
  .cbnotbefst.ENABLED = .F.
  .cbnotbefcom.ENABLED = .F.
  .cbnor.ENABLED = .F.
  .cbnoc.ENABLED = .F.
  .cbnos.ENABLED = .F.
  .txtnbc.ENABLED = .F.
  .txtnbs.ENABLED = .F.
  .txtnosd.ENABLED = .F.
  .txtnocd.ENABLED = .F.
  .chkcmpdely.ENABLED = .F.
  .chkstrtdely.ENABLED = .F.
ENDWITH
lontfrmset.ariaform1.ariacontainer1.grdusers.REFRESH()

*!*************************************************************
*! Name      : lfvSort
*! Developer : Mariam Mazhar
*! Date      : 05/10/2009
*! Purpose   : Task sort function
*!*************************************************************
FUNCTION lfvsort
PARAMETERS lobrformset

lcprjhd    = lobrformset.loparentform.lcprjhdr
lcprj_typ  = &lcprjhd..cprj_typ
lcprj_id   = &lcprjhd..cprj_id
lcstyle    = &lcprjhd..cstyle


DIMENSION lactgarr[1,3],lataskarr[1,2]
STORE ''  TO lactgarr,lataskarr
lc_pmprjdt = lobrformset.loparentform.lcprjdt
SELECT (lc_pmprjdt)
LOCATE
lncatorder = 1
SCAN FOR !EOF() AND ASCAN(lactgarr,coprt_ctg)=0
  lccatg = coprt_ctg
  lcctgdsc = ''
  *-* Select Category Description
  IF gfseek(lccatg,'PMCTGHD','PMCTGHD')
    lcctgdsc = pmctghd.cctg_dsc
  ENDIF
  lccatg = lccatg + '  ' + lcctgdsc
  IF EMPTY(lactgarr[1,1])
    lactgarr[1,1] = coprt_ctg
    lactgarr[1,2] = lncatorder
    lactgarr[1,3] = lccatg
  ELSE
    DIMENSION lactgarr[ALEN(laCtgArr,1)+1,3]
    lactgarr[ALEN(laCtgArr,1),1] = coprt_ctg
    lactgarr[ALEN(laCtgArr,1),2] = lncatorder
    lactgarr[ALEN(laCtgArr,1),3] = lccatg
  ENDIF
  lncatorder = lncatorder + 1
  IF EMPTY(lataskarr[1,1])
    lataskarr[1,1] = coprt_ctg
  ELSE
    lnrowno = ALEN(lataskarr,1) + 1
    DIMENSION  lataskarr[lnRowNo,2]
    lataskarr[lnRowNo,1] = coprt_ctg
  ENDIF
ENDSCAN
SELECT (lc_pmprjdt)
SET ORDER TO pmprjdts
GO TOP
lctask = ''
lnlen = ALEN(lataskarr,1)
LOCAL lnx
*-* prepare the Second Dim (Tasks) of the Array
FOR lnx = 1 TO lnlen
  lcctg =lataskarr(lnx,1)
  LOCATE FOR coprt_ctg = lcctg
  lnctgseq = cctg_seq
  lctask = ''
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
  *!*    IF SEEK(lcPrj_Typ+lcPrj_ID+lcStyle+lnCtgSeq)
  *!*      SCAN REST WHILE cPrj_Typ + cPrj_ID + cStyle + cCtg_Seq + cOprt_Seq  = lcPrj_Typ+lcPrj_ID+lcStyle+lnCtgSeq
  IF SEEK(lcprj_typ+lcprj_id+lcstyle+STR(lnlineno ,6)+lnctgseq)
    SCAN REST WHILE cprj_typ + cprj_id + cstyle +STR(LINENO,6)+ cctg_seq + coprt_seq  = lcprj_typ+lcprj_id+lcstyle+STR(lnlineno ,6)+lnctgseq
      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
      lctask = lctask + coprt_id +' '+coprt_dsc+ ','
    ENDSCAN
  ENDIF
  lataskarr[lnX,2] = lctask
ENDFOR

DO FORM (oariaapplication.screenhome + 'MFPSORT') WITH lactgarr,lataskarr


SELECT (lc_pmprjdt)
SET ORDER TO pmprjdt
lnctgcnt = ALEN(lactgarr,1)
FOR lnx = 1 TO lnctgcnt
  lcctgnam = SUBSTR(lactgarr[lnX,1],1,3)
  lnpos = ASCAN(lataskarr,lcctgnam)
  lnctgpos = ASUBSCRIPT(lataskarr,lnpos,1)
  lctasks = lataskarr(lnctgpos,2)
  lntskseq = 1
  DO WHILE !EMPTY(lctasks)
    lncom = AT(',',lctasks)
    lncomm = lncom - 1
    lncomp = lncom + 1
    lctask = PADR(SUBSTR(lctasks,1,lncomm),5)
    lctasks = SUBSTR(lctasks,lncomp)
    SELECT (lc_pmprjdt)
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
    *IF SEEK(lcPrj_Typ+lcPrj_ID+lcStyle+lcCtgNam+lcTask)
    IF SEEK(lcprj_typ+lcprj_id+lcstyle+STR(LINENO,6)+lcctgnam+lctask)
      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
      lcctgcnt = STR(lactgarr[lnX,2],2)
      lctskcnt = STR(lntskseq,2)
      REPLACE cctg_seq  WITH lcctgcnt  ,;
        coprt_seq WITH lctskcnt  ,;
        cstatus   WITH SUBSTR('MMA', AT(cstatus, 'SMA'), 1)
      lntskseq = lntskseq + 1
    ENDIF
  ENDDO
ENDFOR
SELECT (lc_pmprjdt)
SET ORDER TO pmprjdts
LOCATE
lobrformset.ariaform1.grdtasks.REFRESH

*!*************************************************************
*! Name      : lfvOprt_ID
*! Developer : Mariam Mazhar
*! Date      : 05/10/2009
*! Purpose   : Validate operation id function
*!*************************************************************
FUNCTION lfvoprt_id
PARAMETERS lobrformset
PRIVATE lncuralias

lcprjhd    = lobrformset.loparentform.lcprjhdr
lcprj_typ  = &lcprjhd..cprj_typ
lcprj_id   = &lcprjhd..cprj_id
lcstyle    = &lcprjhd..cstyle
lc_pmprjrl = lobrformset.loparentform.lc_pmprjrl
lc_pmprjdt = lobrformset.loparentform.lcprjdt
lcpmprjntf = lobrformset.loparentform.lcpmprjntf
IF MDOWN()
  RETURN
ENDIF

lcoprt_id    = PADR(ALLTRIM(lobrformset.ariaform1.cnttaskdet.kbtaskid.keytextbox.VALUE), 5)
lncuralias   = SELECT(0)
llbrowse     = lobrformset.ariaform1.cnttaskdet.kbtaskid.selectedfrombrowse  .OR. '?' $ lcoprt_id
lcoldoprt    = PADR(ALLTRIM(lobrformset.ariaform1.cnttaskdet.kbtaskid.keytextbox.oldvalue), 5)
lcoprt_ctg   = SUBSTR(lobrformset.ariaform1.cnttaskdet.kbcatid.keytextbox.VALUE,1,3)
IF !llbrowse .AND. EMPTY(lcoprt_id)
  SET ORDER TO TAG pmprjdt IN (lc_pmprjdt)
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
  *!*    SEEK SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
  *!*               SUBSTR(lcStyle,1,LEN(cStyle))+SUBSTR(lcOprt_Ctg,1,LEN(cOprt_Ctg)+lcOprt_ID)
  SEEK SUBSTR(lcprj_typ,1,LEN(cprj_typ)) + SUBSTR(lcprj_id,1,LEN(cprj_id)) +;
    SUBSTR(lcstyle,1,LEN(cstyle))+STR(lnlineno ,6)+SUBSTR(lcoprt_ctg,1,LEN(coprt_ctg)+lcoprt_id)
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
  IF EOF(lc_pmprjdt)
    lobrformset.ariaform1.cnttaskdet.kbcatid.keytextbox.VALUE = SPACE(3)
    lobrformset.ariaform1.cnttaskdet.kbtaskid.keytextbox.VALUE = ''
    lobrformset.ariaform1.cnttaskdet.kbtaskid.ENABLE = .F.
    lobrformset.ariaform1.cnttaskdet.txtrespon.ENABLED = .F.
  ELSE
    lobrformset.ariaform1.grdtasks.AFTERROWCOLCHANGE
  ENDIF
ELSE
  llshouldadd = .T.
  SET ORDER TO TAG pmprjdt IN (lc_pmprjdt)
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
  *!*    IF !llBrowse .AND. SEEK(SUBSTR(lcPrj_Typ,1,LEN(&lc_PMPrjDt..cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(&lc_PMPrjDt..cPrj_ID)) +;
  *!*                       SUBSTR(lcStyle,1,LEN(&lc_PMPrjDt..cStyle))+SUBSTR(lcOprt_Ctg,1,LEN(&lc_PMPrjDt..cOprt_Ctg))+;
  *!*                       SUBSTR(lcOprt_ID,1,LEN(&lc_PMPrjDt..cOprt_ID)), lc_PMPrjDt)
  *!*      LOCATE REST WHILE cPrj_Typ + cPrj_ID + cStyle + cOprt_Ctg + cOprt_ID =;
  *!*                       SUBSTR(lcPrj_Typ,1,LEN(&lc_PMPrjDt..cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(&lc_PMPrjDt..cPrj_ID)) +;
  *!*                       SUBSTR(lcStyle,1,LEN(&lc_PMPrjDt..cStyle))+SUBSTR(lcOprt_Ctg,1,LEN(&lc_PMPrjDt..cOprt_Ctg))+;
  *!*                       SUBSTR(lcOprt_ID,1,LEN(&lc_PMPrjDt..cOprt_ID));
  *!*                  FOR !lVoid
  IF !llbrowse .AND. SEEK(SUBSTR(lcprj_typ,1,LEN(&lc_pmprjdt..cprj_typ)) + SUBSTR(lcprj_id,1,LEN(&lc_pmprjdt..cprj_id)) +;
      SUBSTR(lcstyle,1,LEN(&lc_pmprjdt..cstyle))+STR(lnlineno ,6)+SUBSTR(lcoprt_ctg,1,LEN(&lc_pmprjdt..coprt_ctg))+;
      SUBSTR(lcoprt_id,1,LEN(&lc_pmprjdt..coprt_id)), lc_pmprjdt)
    LOCATE REST WHILE cprj_typ + cprj_id + cstyle +STR(LINENO,6)+ coprt_ctg + coprt_id =;
      SUBSTR(lcprj_typ,1,LEN(&lc_pmprjdt..cprj_typ)) + SUBSTR(lcprj_id,1,LEN(&lc_pmprjdt..cprj_id)) +;
      SUBSTR(lcstyle,1,LEN(&lc_pmprjdt..cstyle))+STR(lnlineno ,6)+SUBSTR(lcoprt_ctg,1,LEN(&lc_pmprjdt..coprt_ctg))+;
      SUBSTR(lcoprt_id,1,LEN(&lc_pmprjdt..coprt_id));
      FOR !lvoid
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]

    IF FOUND()
      SET ORDER TO TAG pmprjdts IN (lc_pmprjdt)
      lobrformset.ariaform1.grdtasks.AFTERROWCOLCHANGE

      llshouldadd = .F.
    ENDIF
  ENDIF
  IF llbrowse .OR. llshouldadd
    IF llbrowse
      *B609473,1 MMT 01/02/2011 Error while selecting category activities[start]
      *lcTaskID = lfBrowTsks(SUBSTR(loBrFormSet.ariaform1.kbCatID.keytextbox.Value,1,3))
      lctaskid = lfbrowtsks(SUBSTR(lobrformset.ariaform1.cnttaskdet.kbcatid.keytextbox.VALUE ,1,3))
      *B609473,1 MMT 01/02/2011 Error while selecting category activities[End]
      IF EMPTY(lctaskid )
        lobrformset.ariaform1.cnttaskdet.kbtaskid.keytextbox.VALUE = SPACE(5)
        lobrformset.ariaform1.cnttaskdet.kbcatid.keytextbox.VALUE = SPACE(3)
        SELECT (lc_pmprjdt)
        LOCATE
        lobrformset.ariaform1.grdtasks.AFTERROWCOLCHANGE
      ELSE
        lobrformset.ariaform1.cnttaskdet.kbtaskid.keytextbox.VALUE = lctaskid
      ENDIF
      RETURN
    ENDIF
    lncuralias = SELECT(0)
    WITH  lobrformset
      .cbrowsetabledbengine   = 'SQL'
      .nworkarea        = 'PMCTGDT'
      .DATAENVIRONMENT.INITIALSELECTEDALIAS = 'PMCTGDT'
      .cbrowsefilename        = "PMCTGDT"
      .cbrowseindexexpression = "COPRT_CTG+COPRT_ID"
      .cbrowseindexfields     = "COPRT_CTG,COPRT_ID"
      .cbrowseindexname       = "PMCTGDT"
      .cbrowsealiasname       = "PMCTGDT"
      .cbrowsetablename       = "PMCTGDT"
    ENDWITH

    lladdrec = .F.
    lccatetask= lcoprt_id
    IF lobrformset.seekrecord (lcoprt_ctg+lcoprt_id,lang_mfproj_catgtask+ ALLTRIM(lcoprt_ctg)+"\"+ALLTRIM(lcoprt_id)+" ") = 0
      *loBrFormSet.ariaform1.kbCatID.keytextbox.Value = SPACE(3)
      lobrformset.ariaform1.cnttaskdet.kbtaskid.keytextbox.VALUE = SPACE(5)
      RETURN .F.
    ENDIF

    IF !lladdrec
      lcoprt_id= PADR(ALLTRIM(lobrformset.ariaform1.cnttaskdet.kbtaskid.keytextbox.VALUE), 5)
    ELSE
      lcoprt_id= lccatetask
    ENDIF



    IF !EMPTY(lcoprt_id)


      lcstamp =  pmprjhd.cadd_user+IIF(EMPTY(pmprjhd.dadd_date),'',DTOC(pmprjhd.dadd_date))+;
        pmprjhd.cadd_time

      lccurrctg = lcoprt_ctg
      SELECT pmctgdt
      =gfsetorder('PMCTGDT')
      SELECT (lc_pmprjdt)
      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
      *!*        IF gfSEEK(SUBSTR(lcPrj_Typ,1,LEN(&lc_PMPrjDt..cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(&lc_PMPrjDt..cPrj_ID)) +;
      *!*                       SUBSTR(lcStyle,1,LEN(&lc_PMPrjDt..cStyle))+SUBSTR(lcOprt_Ctg,1,LEN(&lc_PMPrjDt..cOprt_Ctg))+;
      *!*                       SUBSTR(lcOprt_ID,1,LEN(&lc_PMPrjDt..cOprt_ID)),;
      *!*                lc_PMPrjDt)
      *!*          LOCATE REST WHILE cPrj_Typ + cPrj_ID + cStyle + cOprt_Ctg + cOprt_ID =;
      *!*                         SUBSTR(lcPrj_Typ,1,LEN(&lc_PMPrjDt..cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(&lc_PMPrjDt..cPrj_ID)) +;
      *!*                         SUBSTR(lcStyle,1,LEN(&lc_PMPrjDt..cStyle))+SUBSTR(lcOprt_Ctg,1,LEN(&lc_PMPrjDt..cOprt_Ctg))+;
      *!*                         SUBSTR(lcOprt_ID,1,LEN(&lc_PMPrjDt..cOprt_ID));
      *!*                         FOR !lVoid
      IF gfseek(SUBSTR(lcprj_typ,1,LEN(&lc_pmprjdt..cprj_typ)) + SUBSTR(lcprj_id,1,LEN(&lc_pmprjdt..cprj_id)) +;
          SUBSTR(lcstyle,1,LEN(&lc_pmprjdt..cstyle))+STR(lnlineno ,6)+SUBSTR(lcoprt_ctg,1,LEN(&lc_pmprjdt..coprt_ctg))+;
          SUBSTR(lcoprt_id,1,LEN(&lc_pmprjdt..coprt_id)),;
          lc_pmprjdt)
        LOCATE REST WHILE cprj_typ + cprj_id + cstyle+STR(LINENO,6) + coprt_ctg + coprt_id =;
          SUBSTR(lcprj_typ,1,LEN(&lc_pmprjdt..cprj_typ)) + SUBSTR(lcprj_id,1,LEN(&lc_pmprjdt..cprj_id)) +;
          SUBSTR(lcstyle,1,LEN(&lc_pmprjdt..cstyle))+STR(lnlineno ,6)+SUBSTR(lcoprt_ctg,1,LEN(&lc_pmprjdt..coprt_ctg))+;
          SUBSTR(lcoprt_id,1,LEN(&lc_pmprjdt..coprt_id));
          FOR !lvoid
        *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
        llfound = FOUND()
      ELSE
        llfound = .F.
      ENDIF
      IF !llfound
        SELECT pmctgdt
        IF gfseek(lcoprt_ctg + lcoprt_id)
          SCATTER MEMVAR MEMO
          STORE .T. TO llchngoprt, glupdated
          llupdstrtd = .F.
          lcnewseqnum = lfgetseqnum()
          *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
          *!*            INSERT INTO (lc_PMPrjDt);
          *!*              ( cPrj_Typ , cPrj_ID  ,  cStyle,  ;
          *!*                cCtg_Seq , cOprt_Ctg, cOprt_Seq,;
          *!*                cOprt_ID , cOprt_Dsc, cOprt_Res,;
          *!*                lShw2Cust, mOprt_Com, cUpdtMthd, ;
          *!*                nest_dur , nrem_dur , mNotify, cStatus,cCal_ID,lOrginal,cGroup_id,;
          *!*                cAdd_User, dAdd_Date, cAdd_Time,cMComplt,lAddToAud);
          *!*              VALUES;
          *!*               (lcPrj_Typ, lcPrj_ID, lcStyle, ;
          *!*               lcNewSeqNum, m.cOprt_Ctg, m.cOprt_Seq, ;
          *!*               m.cOprt_ID, m.cOprt_Dsc, m.cOprt_Res,;
          *!*               m.lShw2Cust, m.mOprt_Com, m.cUpdtMthd,;
          *!*               m.nest_dur, m.nest_dur, m.mNotify, 'A',m.cCal_ID,.T.,m.cGroup_id,;
          *!*               oAriaApplication.User_ID, oAriaApplication.SystemDate, TIME(),'YYY',.T.)
          *B609555,1 MMT 03/27/2011 Task List screen gives error while completing task[Start]
          m.moprt_com = ''
          *B609555,1 MMT 03/27/2011 Task List screen gives error while completing task[END]

          INSERT INTO (lc_pmprjdt);
            ( cprj_typ , cprj_id  ,  cstyle,  ;
            cctg_seq , coprt_ctg, coprt_seq,;
            coprt_id , coprt_dsc, coprt_res,;
            lshw2cust, moprt_com, cupdtmthd, ;
            nest_dur , nrem_dur , mnotify, cstatus,ccal_id,lorginal,cgroup_id,;
            cadd_user, dadd_date, cadd_time,cmcomplt,laddtoaud,LINENO);
            VALUES;
            (lcprj_typ, lcprj_id, lcstyle, ;
            lcnewseqnum, m.coprt_ctg, m.coprt_seq, ;
            m.coprt_id, m.coprt_dsc, m.coprt_res,;
            m.lshw2cust, m.moprt_com, m.cupdtmthd,;
            m.nest_dur, m.nest_dur, m.mnotify, 'A',m.ccal_id,.T.,m.cgroup_id,;
            oariaapplication.user_id, oariaapplication.systemdate, TIME(),'YYY',.T.,lnlineno )
          *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
          SET ORDER TO pmprjdts IN (lc_pmprjdt)
          SELECT MAX(coprt_seq) ;
            FROM (lc_pmprjdt);
            INTO ARRAY latmparry
          lcnoprseq = IIF(_TALLY > 0 AND  !ISNULL(latmparry[1]), PADL(INT(VAL(latmparry))+1,  2, '0'), '01')

          SET ORDER TO pmprjdt IN (lc_pmprjdt)
          *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
          *!*            IF SEEK(lcPrj_Typ+ lcPrj_ID+ lcStyle+m.cOprt_Ctg+m.cOprt_ID,lc_PMPrjDt)
          IF SEEK(lcprj_typ+ lcprj_id+ lcstyle+STR(lnlineno ,6)+m.coprt_ctg+m.coprt_id,lc_pmprjdt)
            *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
            SELECT (lc_pmprjdt)
            REPLACE &lc_pmprjdt..coprt_seq WITH lcnoprseq
          ENDIF

          glupdated = .T.
          lobrformset.loparentform.lnnumoflin = lobrformset.loparentform.lnnumoflin + 1

          IF !EMPTY(lobrformset.loparentform.laopertion[1])
            DIMENSION  lobrformset.loparentform.laopertion[ALEN(loBrFormSet.loParentForm.laOpertion) + 1]
          ENDIF

          lobrformset.loparentform.laopertion[ALEN(loBrFormSet.loParentForm.laOpertion)] = ;
            lcoprt_ctg + '\' + lcoprt_id + ;
            ' ' +SUBSTR(m.coprt_dsc, 1, 18)
          IF ALEN(lobrformset.loparentform.laopertion) > 1
            lobrformset.ariaform1.cnttaskdet.cmdpred.ENABLED =.T.
          ENDIF

          IF ASCAN(lobrformset.loparentform.lacategrie, lcoprt_ctg) = 0
            IF !EMPTY(lobrformset.loparentform.lacategrie[1])
              DIMENSION lobrformset.loparentform.lacategrie[ALEN(loBrFormSet.loParentForm.laCategrie) + 1]
            ENDIF
            lobrformset.loparentform.lacategrie[ALEN(loBrFormSet.loParentForm.laCategrie)] = lcoprt_ctg
          ENDIF
        ENDIF

        IF gfseek(lcoprt_ctg + m.coprt_id, 'PMCTGRL')
          SELECT pmctgrl
          SCAN REST WHILE coprt_ctg + coprt_id = lcoprt_ctg + m.coprt_id;
              FOR ASCAN(lobrformset.loparentform.laopertion, coprt_ctg + '\' + cprd_id ) > 0
            SCATTER MEMVAR MEMO
            INSERT INTO (lc_pmprjrl) FROM MEMVAR
            SELECT (lc_pmprjrl)
            REPLACE cprj_typ  WITH lcprj_typ,;
              cprj_id   WITH lcprj_id,;
              cstyle    WITH lcstyle,;
              cprd_ctg  WITH coprt_ctg,;
              cadd_user WITH oariaapplication.user_id,;
              dadd_date WITH  oariaapplication.systemdate,;
              cadd_time WITH TIME(),;
              cstatus WITH 'A'
            *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
            REPLACE LINENO WITH lnlineno
            *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
          ENDSCAN
        ENDIF

        IF gfseek(lcoprt_ctg + m.coprt_id, 'PMPCTGNT')
          SELECT pmpctgnt
          SCAN REST WHILE coprt_ctg + coprt_id = lcoprt_ctg + m.coprt_id
            SCATTER MEMVAR
            m.cprj_typ = lcprj_typ
            m.cprj_id  = lcprj_id
            m.cstyle   = lcstyle
            m.cstatus  = 'A'
            *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
            m.lineno = lnlineno
            *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
            INSERT INTO (lcpmprjntf) FROM MEMVAR
          ENDSCAN
        ENDIF

      ENDIF
      SELECT (lc_pmprjdt)
      SET ORDER TO TAG pmprjdts
      *-- ReCalculate estimated dates if any change occurs.
      ACOPY(lobrformset.loparentform.laholidays,laholidays)
      =lfgetestdt(@lc_pmprjdt, @lc_pmprjrl,.F.)

      lobrformset.ariaform1.grdtasks.AFTERROWCOLCHANGE
      lobrformset.ariaform1.grdtasks.REFRESH
    ELSE
      SET ORDER TO TAG pmprjdts IN (lc_pmprjdt)
      lobrformset.ariaform1.cnttaskdet.kbtaskid.keytextbox.VALUE = SPACE(5)
    ENDIF
  ENDIF   && ENDIF !llBrowse .AND. SEEK(lcOprt_Ctg + lcOprt_ID, lc_PMPrjDt)
ENDIF
SELECT (lncuralias)

*!*************************************************************
*! Name      : lfBrowTsks
*! Developer : Mariam Mazhar
*! Date      : 05/10/2009
*! Purpose   : browse operation id function
*!*************************************************************
FUNCTION lfbrowtsks
PARAMETERS lccatg
lcbrfields = [cOprt_ID : H = ']+lang_mfproj_taskid+[', cOprt_Dsc : H = ']+lang_mfproj_taskname+[', cOprt_Res:H=']+lang_mfproj_respon+[', nest_dur : H = ']+lang_mfproj_duration+[']
lctaskno  = ''
SELECT pmctgdt
=gfseek(lccatg)
LOCATE
lctaskno  = IIF(ariabrow("FOR cOprt_Ctg ='"+lccatg+"' ",lang_mfproj_catgtask,gnbrfsrow1, gnbrfscol1, gnbrfsrow2, ;
  gnbrfscol2,'','','COPRT_ID','laBrowArr', .F.,'PMCTGDT',.F.,'PMCTGDT','','','PMCTGDT'),pmctgdt.coprt_id,SPACE(5))



RETURN   lctaskno


FUNCTION lfvstrtdely
PARAMETERS lontfrmset
WITH lontfrmset.ariaform1.ariacontainer2

  REPLACE lstrtdelay WITH .chkstrtdely.VALUE ,;
    cstatus    WITH SUBSTR('MMA', AT(cstatus, 'SMA'), 1) IN  (lcpmprjntf)

  IF  .chkstrtdely.VALUE
    .txtnosd.ENABLED = .T.
    .txtnosd.VALUE = 1
    REPLACE nstrtdelay WITH 1 IN (lcpmprjntf)
  ELSE
    .txtnosd.ENABLED = .F.
    .txtnosd.VALUE = 0
    REPLACE nstrtdelay WITH 0 IN (lcpmprjntf)
  ENDIF
ENDWITH
lontfrmset.ariaform1.ariacontainer1.grdusers.REFRESH




FUNCTION lfvcompdly
PARAMETERS lontfrmset
WITH lontfrmset.ariaform1.ariacontainer2
  REPLACE lcmpldelay WITH .chkcmpdely.VALUE ,;
    cstatus    WITH SUBSTR('MMA', AT(cstatus, 'SMA'), 1) IN  (lcpmprjntf)

  IF  .chkcmpdely.VALUE
    .txtnocd.ENABLED = .T.
    .txtnocd.VALUE = 1
    REPLACE ncmpldelay WITH 1 IN (lcpmprjntf)
  ELSE
    .txtnocd.ENABLED = .F.
    .txtnocd.VALUE = 0
    REPLACE ncmpldelay WITH 0 IN (lcpmprjntf)
  ENDIF
ENDWITH
lontfrmset.ariaform1.ariacontainer1.grdusers.REFRESH

FUNCTION lfvitem
PARAMETERS loformset



PRIVATE lncuralias
lcprj_typ = loformset.ariaform1.cbotrantype.VALUE
IF lcprj_typ  = 'H'
  RETURN
ENDIF

WITH loformset.ariaform1
  lcprjtype = .cbotrantype.DISPLAYVALUE
  llbrowse  = .kbstyle.selectedfrombrowse
  m.cstyle =  .kbstyle.keytextbox.VALUE
  m.style =  PADR(.kbstyle.keytextbox.VALUE,19)
  m.cprj_id = .kbtranno.keytextbox.VALUE
  m.cprj_typ = lcprj_typ
  IF loformset.activemode <> 'S' OR (loformset.activemode = 'S' AND !EMPTY(m.cprj_id))
    IF gfseek(lcprj_typ+m.cprj_id+ m.cstyle,'PMPRJHD','PMPRJHD')
      .kbstyle.keytextbox.VALUE = m.cstyle
      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
      loformset.lcstyle =  m.cstyle
      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
      .kbstyle.ENABLED = .F.
      .kbtranno.sharedvalidation
      RETURN
    ENDIF
  ENDIF

  lncuralias = SELECT(0)

  .kbstyle.keytextbox.VALUE = m.cstyle
  llbrowse   = llbrowse .OR. '?' $ m.cstyle
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
  loformset.lcstyle =  m.cstyle
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
  IF lcprj_typ  = 'S' AND EMPTY(m.cstyle) AND !llbrowse
    RETURN
  ENDIF

  IF lcprj_typ="M" AND !llbrowse AND gfseek('0002'+m.style,'Item','cSTYLE')
    *.kbTranNo.sharedvalidation
    RETURN
  ENDIF

ENDWITH

IF !llbrowse
  IF loformset.activemode $ "SV"
    SELECT pmprjhd
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
    IF gfseek(lcprj_typ + m.cprj_id + SUBSTR(m.cstyle,1,LEN(pmprjhd.cstyle)))
    ELSE
      loformset.lcstyle =  m.cstyle
      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]


      *-- Reposition the file pointer.
      =gfseek(lcprj_typ + m.cprj_id)
      IF  gfseek('0002'+m.style,'Item','CSTYLE')
        loformset.ariaform1.kbtranno.sharedvalidation
        RETURN
      ELSE      && ELSE IF lfVldStyle()
        *-- If the style does not belong to the order, browse, or
        *-- reenter message.
        lcmsg =  lang_mfproj_material	+' ' + ALLTRIM(m.cstyle) + lang_mfproj_notfound +;
          lcprjtype + IIF(lcprj_typ  $ 'SM',lang_mfproj_file,' #' +ALLTRIM(m.cprj_id))+ '.'
        IF gfmodalgen("TRM00000B00014","DIALOG",.F.,.F.,lcmsg) = 1
          llbrowse = .T.
        ELSE
          m.cstyle = SPACE(loformset.lnmajorlen)
          loformset.ariaform1.kbstyle.keytextbox.VALUE = m.cstyle
          loformset.ariaform1.kbstyle.keytextbox.ENABLED = .T.
          loformset.llsetfcstrnid = .T.
          RETURN .F.
        ENDIF    && ENDIF gfModalgen(..)
      ENDIF    && ENDIF lfVldStyle()
    ENDIF    && ENDIF SEEK(lcPrj_Typ + m.cPrj_ID + m.cStyle)
  ELSE
    *-- If the style is valid for the selected project, continue,
    *-- otherwise, browse.
    IF !gfseek('0002'+m.style,'Item','cSTYLE')
      llbrowse = .T.
    ELSE
      loformset.changemode('A')
      RETURN
    ENDIF
  ENDIF
ENDIF    && ENDIF !llBrowse

IF llbrowse

  SELECT ITEM
  gfsetorder('cStyle')
  =gfseek('0002')
  CURSORSETPROP("Buffering" ,3)
  INDEX ON cstymajor TAG 'cStyle' UNIQUE
  lcbrfields = [cStyMajor: H=']+lang_mfproj_material+;
    [', Desc : H=']+lang_mfproj_desc+[',Status,Season :H=']+lang_mfproj_season  +;
    [',Cdivision :H = ']+ lang_mfproj_division+[' ]

  DIMENSION laselected[1]
  lcfldname = 'cStyMajor'
  IF ariabrow('', lang_mfproj_material,;
      gnbrfsrow1, gnbrfscol1, gnbrfsrow2, gnbrfscol2,.F.,.F.,lcfldname,'laSelected')

    m.cstyle  = STYLE
    m.cstyle  = ITEM.cstymajor

    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
    loformset.lcstyle =  m.cstyle
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
    loformset.ariaform1.kbstyle.keytextbox.VALUE = m.cstyle
    *-- If coming from 'S'elect mode, i.e. the project ID is already
    *-- found in the projects file. Look for the entered style with
    *-- the projects file.
    IF loformset.activemode $ "SV"
      SELECT pmprjhd
      *-- If found, 'V'iew the entry,
      IF gfseek(lcprj_typ + m.cprj_id + SUBSTR(m.cstyle,1,LEN(pmprjhd.cstyle)))
        loformset.changemode('V')
        *-- If not found in the projects file, validate the entry
        *-- from the styles of the order.
        *-- If found, present a message with Add, Browse, or
        *-- Reeneter options, Otherwise, Present a not found message
        *-- <Browse>, <Reenter> option.
      ELSE
        *-- Reposition the file pointer.
        loformset.ariaform1.kbtranno.sharedvalidation
        RETURN
      ENDIF     && ENDIF SEEK(lcPrj_Typ + m.cPrj_ID + m.cStyle)
    ELSE
      loformset.changemode('A')
    ENDIF
  ELSE      && ELSE IF ARIABROW(..)
    loformset.ariaform1.kbstyle.keytextbox.VALUE = SPACE(12)
    loformset.ariaform1.kbstyle.ENABLED = .T.
  ENDIF     && ENDIF ARIABROW(..)
  llbrowse = .F.
ENDIF

loformset.ariaform1.kbstyle.ENABLED = .F.

SELECT (lncuralias)

*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
FUNCTION lfvtaskon
LPARAMETERS lddateshd
REPLACE donschdl WITH lddateshd,;
  cstatus  WITH SUBSTR('MMA', AT(cstatus, 'SMA'), 1)

FUNCTION lfvstylecolor
PARAMETERS loformset

PRIVATE lncuralias
lcprj_typ = loformset.ariaform1.cbotrantype.VALUE
IF lcprj_typ  = 'H'
  RETURN
ENDIF

WITH loformset.ariaform1
  lcprjtype = .cbotrantype.DISPLAYVALUE
  llbrowse  = .kbcolor.selectedfrombrowse
  m.color  =  .kbcolor.keytextbox.VALUE
  m.cstyle =  .kbstyle.keytextbox.VALUE
  m.cprj_id = .kbtranno.keytextbox.VALUE
  m.cprj_typ = lcprj_typ
  IF (loformset.activemode <> 'S' OR (loformset.activemode = 'S' AND !EMPTY(m.cprj_id))) OR (lcprj_typ ='S' AND !EMPTY(m.cstyle))

    m.color = IIF(!llbrowse .AND. EMPTY(m.color), STRTRAN(loformset.lcnonmajpic,'X','*'),;
      PADR(ALLTRIM(m.color),LEN(loformset.lcnonmajpic)))
    loformset.nlinenum = 0
    *!*      IF gfSeek(lcPrj_Typ+m.cPrj_ID+ m.cStyle,'PMPRJHD','PMPRJHD')
    *!*        .kbStyle.keyTextBox.Value = m.cStyle
    *!*        .kbStyle.Enabled = .F.
    *!*        .kbTranNo.sharedvalidation
    *!*        RETURN
    *!*      ENDIF
  ENDIF

  lncuralias = SELECT(0)

  .kbcolor.keytextbox.VALUE = m.color
  llbrowse   = llbrowse .OR. '?' $ m.color

  IF lcprj_typ  = 'S' AND EMPTY(m.cstyle) AND !llbrowse
    RETURN
  ENDIF

  IF lcprj_typ="S" AND !llbrowse AND lfvldstyleclr()
    .kbtranno.sharedvalidation
    RETURN
  ENDIF

ENDWITH

IF !llbrowse
  llmorethanline = .F.
  IF loformset.activemode $ "SV"
    SELECT pmprjhd
    IF .T.
      *-- Reposition the file pointer.
      =gfseek(lcprj_typ + m.cprj_id)
      IF lfvldstyleclr()
        loformset.lcstyle = m.cstyle
        loformset.ariaform1.lststyle.ADDITEM (m.cstyle ,1)
        loformset.ariaform1.kbtranno.sharedvalidation
        RETURN
      ELSE      && ELSE IF lfVldStyle()
        *-- If the style does not belong to the order, browse, or
        *-- reenter message.
        IF llmorethanline
          llbrowse = .T.
        ELSE
          lcmsg =  lang_mfproj_style+' ' + ALLTRIM(m.cstyle)+"/"+ ALLTRIM(m.color)+ lang_mfproj_notfound +;
            lcprjtype + IIF(lcprj_typ  = 'S',lang_mfproj_file,' #' +ALLTRIM(m.cprj_id))+ '.'
          IF gfmodalgen("TRM00000B00014","DIALOG",.F.,.F.,lcmsg) = 1
            llbrowse = .T.
          ELSE
            m.color = SPACE(LEN(loformset.lcnonmajpic))
            loformset.ariaform1.kbcolor.keytextbox.VALUE = m.color
            loformset.ariaform1.kbcolor.keytextbox.ENABLED = .T.
            loformset.llsetfcstrnid = .T.
            RETURN .F.
          ENDIF    && ENDIF gfModalgen(..)
        ENDIF
      ENDIF    && ENDIF lfVldStyle()
    ENDIF    && ENDIF SEEK(lcPrj_Typ + m.cPrj_ID + m.cStyle)
  ELSE
    *-- If the style is valid for the selected project, continue,
    *-- otherwise, browse.
    IF !lfvldstyleclr()
      llbrowse = .T.
    ELSE
      loformset.changemode('A')
      RETURN
    ENDIF
  ENDIF
ENDIF    && ENDIF !llBrowse

IF llbrowse
  DO CASE
  CASE  oariaapplication.activemoduleid = 'PO'
    =gfseek(IIF(lcprj_typ = 'N','NN',IIF(lcprj_typ = 'R','RP','PP'))+ m.cprj_id,'POSLN')
    SELECT  STYLE,COMPLETE,LINENO,totqty  FROM posln WHERE SUBSTR(STYLE,1,loformset.lnmajorlen)=m.cstyle  INTO CURSOR 'TempStyle'


  CASE oariaapplication.activemoduleid = 'SO'
    =gfseek(lcprj_typ + m.cprj_id,'Ordline')
    SELECT STYLE,COMPLETE,LINENO,totqty FROM ordline ;
      WHERE  cordtype+ORDER+STR(LINENO,6) = lcprj_typ + m.cprj_id AND ;
      SUBSTR(STYLE,1,loformset.lnmajorlen)=m.cstyle  INTO CURSOR 'TempStyle'

  CASE  oariaapplication.activemoduleid = 'MF'
    =gfseek('P'+IIF(lcprj_typ='C','U', lcprj_typ)+ m.cprj_id,'POSLN')
    SELECT STYLE,COMPLETE,LINENO,totqty FROM posln WHERE SUBSTR(STYLE,1,loformset.lnmajorlen)=m.cstyle INTO CURSOR 'TempStyle'


  ENDCASE
  IF USED('TempStyle')
    SELECT (loformset.lcbrowcurs)
    DELETE ALL
    SELECT tempstyle
    SCAN
      =gfseek(ALLTRIM(tempstyle.STYLE),'Style','Style')
      INSERT INTO (loformset.lcbrowcurs) VALUES ( tempstyle.STYLE,STYLE.DESC,tempstyle.COMPLETE,tempstyle.LINENO,tempstyle.totqty)
    ENDSCAN
    lcbrowcurs = loformset.lcbrowcurs
    SELECT (lcbrowcurs)
    LOCATE
  ENDIF

  IF oariaapplication.activemoduleid = 'IC'
    SELECT STYLE
    gfsetorder('Style')
    lcbrfields = [STYLE: H=']+lang_mfproj_style+;
      [', Desc : H=']+lang_mfproj_desc+[',Status,Season :H=']+lang_mfproj_season  +;
      [',Cdivision :H = ']+ lang_mfproj_division+[' ]
  ELSE
    lcbrfields = [Style, Desc : H=']+lang_mfproj_desc+[',Complete : H='Complete Date',TOTQTY :H='TotQty']
  ENDIF

  DIMENSION laselected[1]
  lcfldname = 'STYLE'
  IF ariabrow('', lang_mfproj_styles,;
      gnbrfsrow1, gnbrfscol1, gnbrfsrow2, gnbrfscol2,.F.,.F.,lcfldname,'laSelected')
    m.color  = RIGHT(STYLE,LEN(loformset.lcnonmajpic))
    loformset.lcstyle = STYLE
    IF oariaapplication.activemoduleid = 'IC'
      SELECT STYLE
      =gfsetorder("Style")
      loformset.ariaform1.kbstyle.keytextbox.VALUE  =STYLE.cstymajor
    ELSE
      loformset.nlinenum = LINENO
      loformset.lcstyle = STYLE
      loformset.ariaform1.lststyle.ADDITEM (STYLE ,1)
      loformset.ariaform1.lststyle.ADDITEM ("Complete:"+DTOC(COMPLETE),2)
      loformset.ariaform1.lststyle.ADDITEM ("TotQty   :"+STR(totqty,9),3)
    ENDIF

    loformset.ariaform1.kbcolor.keytextbox.VALUE = m.color
    *-- If coming from 'S'elect mode, i.e. the project ID is already
    *-- found in the projects file. Look for the entered style with
    *-- the projects file.
    IF loformset.activemode $ "SV"
      SELECT pmprjhd
      *-- If found, 'V'iew the entry,
      IF gfseek(lcprj_typ + m.cprj_id + SUBSTR(m.cstyle,1,LEN(pmprjhd.cstyle))+STR(loformset.nlinenum,6))
        loformset.changemode('V')
        RETURN
      ELSE
        *-- Reposition the file pointer.

        loformset.ariaform1.kbtranno.sharedvalidation
        RETURN
      ENDIF     && ENDIF SEEK(lcPrj_Typ + m.cPrj_ID + m.cStyle)
    ELSE
      IF oariaapplication.activemoduleid = 'IC'
        SELECT STYLE
        =gfsetorder("Style")
      ENDIF

      loformset.changemode('A')
    ENDIF
  ELSE      && ELSE IF ARIABROW(..)
    loformset.ariaform1.kbcolor.keytextbox.VALUE = SPACE(0)
    loformset.ariaform1.kbcolor.ENABLED = .T.
  ENDIF     && ENDIF ARIABROW(..)
  llbrowse = .F.
ENDIF

IF EMPTY(loformset.ariaform1.kbcolor.keytextbox.VALUE) AND;
    INLIST(oariaapplication.activemoduleid ,'PO','SO','IC')
  loformset.ariaform1.kbcolor.ENABLED = .T.
ELSE
  loformset.ariaform1.kbcolor.ENABLED = .F.
ENDIF

SELECT (lncuralias)


*!*************************************************************
*! Name      : lfVldStyleClr
*! Developer : Mariam Mazhar
*! Date      : 12/02/2009
*! Purpose   : Validates a style is belonging to an order or not.
*!*************************************************************
*! Calls              :   ARIABROW()
*!*************************************************************
*! Passed Parameters  :   None
*!*************************************************************
*! Returns            :  .T. if the style belongs to the project ID
*!                       .F. otherwise.
*!*************************************************************
*! Example            :  =lfVldStyle()
*!*************************************************************
FUNCTION lfvldstyleclr
PRIVATE lncuralias, llvldstyle
llvldstyle = .F.

IF m.color = STRTRAN(loformset.lcnonmajpic,'X','*')
  llvldstyle = .T.
  loformset.nlinenum = 0
ELSE
  lncuralias = SELECT(0)
  DO CASE
  CASE lcprj_typ = 'O' OR lcprj_typ = 'T'
    SELECT ordline
    IF gfseek(lcprj_typ +m.cprj_id)
      LOCATE REST WHILE cordtype+ORDER+STR(LINENO,6)= lcprj_typ + m.cprj_id FOR;
        SUBSTR(STYLE,1,loformset.lnmajorlen) = PADR(m.cstyle,loformset.lnmajorlen) AND ;
        RIGHT(STYLE,LEN(loformset.lcnonmajpic)) = PADR(m.color,LEN(loformset.lcnonmajpic))
      llvldstyle = FOUND()
      IF llvldstyle
        loformset.nlinenum = ordline.LINENO
        loformset.lcstyle = ordline.STYLE

        SELECT ordline
        =gfseek(lcprj_typ +m.cprj_id)
        COUNT REST WHILE cordtype+ORDER+STR(LINENO,6)= lcprj_typ + m.cprj_id FOR;
          SUBSTR(STYLE,1,loformset.lnmajorlen) = PADR(m.cstyle,loformset.lnmajorlen) AND;
          RIGHT(STYLE,LEN(loformset.lcnonmajpic)) =  PADR(m.color,LEN(loformset.lcnonmajpic)) TO lnnumline
        IF lnnumline > 1
          loformset.nlinenum = 0
          loformset.lcstyle = ''
          llmorethanline = .T.
          llvldstyle = .F.
        ENDIF

      ENDIF
    ENDIF
  CASE lcprj_typ = 'P' OR lcprj_typ = 'A' OR lcprj_typ = 'D' OR;
      lcprj_typ = 'N' OR lcprj_typ = 'R' OR lcprj_typ = 'C'
    =gfseek('P'+IIF(lcprj_typ = 'C','U',lcprj_typ )+m.cprj_id +'0001'+ ALLTRIM(m.cstyle), 'POSLN')
    SELECT posln
    LOCATE REST WHILE cbusdocu+cstytype+po+cinvtype+STYLE+STR(LINENO,6)+trancd  =;
      'P'+IIF(lcprj_typ = 'C','U',lcprj_typ )+m.cprj_id +'0001';
      FOR SUBSTR(STYLE,1,loformset.lnmajorlen) = PADR(m.cstyle,loformset.lnmajorlen) AND;
      RIGHT(STYLE,LEN(loformset.lcnonmajpic)) =  PADR(m.color,LEN(loformset.lcnonmajpic))
    llvldstyle =FOUND()
    IF llvldstyle
      loformset.nlinenum = posln.LINENO
      loformset.lcstyle = posln.STYLE
      =gfseek('P'+IIF(lcprj_typ = 'C','U',lcprj_typ )+m.cprj_id +'0001'+ ALLTRIM(m.cstyle), 'POSLN')
      COUNT REST WHILE cbusdocu+cstytype+po+cinvtype+STYLE+STR(LINENO,6)+trancd  =;
        'P'+IIF(lcprj_typ = 'C','U',lcprj_typ )+m.cprj_id +'0001'+ ALLTRIM(m.cstyle);
        FOR RIGHT(STYLE,LEN(loformset.lcnonmajpic)) =  PADR(m.color,LEN(loformset.lcnonmajpic)) TO lnnumline
      IF lnnumline > 1
        llmorethanline = .T.
        llvldstyle = .F.
        loformset.nlinenum = 0
        loformset.lcstyle = ''
      ENDIF
    ENDIF
  CASE lcprj_typ = 'S'
    =gfseek(ALLTRIM(m.cstyle), 'Style', 'Style')
    SELECT STYLE
    LOCATE REST WHILE SUBSTR(STYLE,1,loformset.lnmajorlen)= PADR(m.cstyle,loformset.lnmajorlen) FOR ;
      RIGHT(STYLE,LEN(loformset.lcnonmajpic)) = PADR(m.color,LEN(loformset.lcnonmajpic))
    llvldstyle = FOUND()
    IF llvldstyle
      loformset.nlinenum = 0
      loformset.lcstyle = STYLE.STYLE
    ENDIF
  ENDCASE
  SELECT (lncuralias)
ENDIF

RETURN llvldstyle


*Materail Color Validations
FUNCTION lfvitemcolor
PARAMETERS loformset

PRIVATE lncuralias
lcprj_typ = loformset.ariaform1.cbotrantype.VALUE
IF lcprj_typ  = 'H'
  RETURN
ENDIF

WITH loformset.ariaform1
  lcprjtype = .cbotrantype.DISPLAYVALUE
  llbrowse  = .kbcolor.selectedfrombrowse
  m.color  =  .kbcolor.keytextbox.VALUE
  m.cstyle =  .kbstyle.keytextbox.VALUE
  m.cprj_id = SPACE(6)
  m.cprj_typ = lcprj_typ
  IF (loformset.activemode <> 'S' OR (loformset.activemode = 'S' AND !EMPTY(m.cprj_id))) OR (lcprj_typ ='M' AND !EMPTY(m.cstyle))

    m.color = IIF(!llbrowse .AND. EMPTY(m.color), STRTRAN(loformset.lcnonmajpicmat,'X','*'),;
      PADR(ALLTRIM(m.color),LEN(loformset.lcnonmajpicmat)))
    loformset.nlinenum = 0
  ENDIF

  lncuralias = SELECT(0)

  .kbcolor.keytextbox.VALUE = m.color
  llbrowse   = llbrowse .OR. '?' $ m.color

  IF lcprj_typ  = 'S' AND EMPTY(m.cstyle) AND !llbrowse
    RETURN
  ENDIF

  IF lcprj_typ="M" AND !llbrowse AND lfvldmatclr()
    .kbtranno.sharedvalidation
    RETURN
  ENDIF

ENDWITH

IF !llbrowse
  llmorethanline = .F.
  IF loformset.activemode $ "SV"
    SELECT pmprjhd
    IF .T.
      *-- Reposition the file pointer.
      =gfseek(lcprj_typ + m.cprj_id)
      IF lfvldmatclr()
        loformset.lcstyle = m.cstyle
        loformset.ariaform1.kbtranno.sharedvalidation
        RETURN
      ELSE      && ELSE IF lfVldStyle()
        *-- If the style does not belong to the order, browse, or
        *-- reenter message.
        IF llmorethanline
          llbrowse = .T.
        ELSE
          lcmsg =  lang_mfproj_material +' ' + ALLTRIM(m.cstyle)+"/"+ ALLTRIM(m.color)+ lang_mfproj_notfound +;
            lcprjtype + IIF(lcprj_typ  = 'M',lang_mfproj_file,' #' +ALLTRIM(m.cprj_id))+ '.'
          IF gfmodalgen("TRM00000B00014","DIALOG",.F.,.F.,lcmsg) = 1
            llbrowse = .T.
          ELSE
            m.color = SPACE(LEN(loformset.lcnonmajpic))
            loformset.ariaform1.kbcolor.keytextbox.VALUE = m.color
            loformset.ariaform1.kbcolor.keytextbox.ENABLED = .T.
            loformset.llsetfcstrnid = .T.
            RETURN .F.
          ENDIF    && ENDIF gfModalgen(..)
        ENDIF
      ENDIF    && ENDIF lfVldStyle()
    ENDIF    && ENDIF SEEK(lcPrj_Typ + m.cPrj_ID + m.cStyle)
  ELSE
    *-- If the style is valid for the selected project, continue,
    *-- otherwise, browse.
    IF !lfvldmatclr()
      llbrowse = .T.
    ELSE
      loformset.changemode('A')
      RETURN
    ENDIF
  ENDIF
ENDIF    && ENDIF !llBrowse

IF llbrowse

  IF oariaapplication.activemoduleid = 'MA'
    SELECT ITEM
    gfsetorder('Style')
    =gfseek('0002')
    IF !EMPTY(loformset.ariaform1.kbstyle.keytextbox.VALUE)
      =SEEK('0002'+loformset.ariaform1.kbstyle.keytextbox.VALUE)
    ENDIF
    lcbrfields = [STYLE: H=']+lang_mfproj_material +;
      [', Desc : H=']+lang_mfproj_desc+[',Status,Season :H=']+lang_mfproj_season  +;
      [',Cdivision :H = ']+ lang_mfproj_division+[' ]
  ENDIF

  DIMENSION laselected[1]
  lcfldname = 'STYLE'
  IF ariabrow('', lang_mfproj_material ,;
      gnbrfsrow1, gnbrfscol1, gnbrfsrow2, gnbrfscol2,.F.,.F.,lcfldname,'laSelected')
    m.color  = RIGHT(STYLE,LEN(loformset.lcnonmajpicmat))
    loformset.lcstyle = STYLE
    IF oariaapplication.activemoduleid = 'MA'
      SELECT ITEM
      =gfsetorder("Style")
      loformset.ariaform1.kbstyle.keytextbox.VALUE  =ITEM.cstymajor
    ENDIF

    loformset.ariaform1.kbcolor.keytextbox.VALUE = m.color
    *-- If coming from 'S'elect mode, i.e. the project ID is already
    *-- found in the projects file. Look for the entered style with
    *-- the projects file.
    IF loformset.activemode $ "SV"
      SELECT pmprjhd
      *-- If found, 'V'iew the entry,
      IF gfseek(lcprj_typ + m.cprj_id + SUBSTR(m.cstyle,1,LEN(pmprjhd.cstyle))+STR(loformset.nlinenum,6))
        loformset.changemode('V')
        RETURN
      ELSE
        *-- Reposition the file pointer.

        loformset.ariaform1.kbtranno.sharedvalidation
        RETURN
      ENDIF     && ENDIF SEEK(lcPrj_Typ + m.cPrj_ID + m.cStyle)
    ELSE
      IF oariaapplication.activemoduleid = 'MA'
        SELECT ITEM
        =gfsetorder("Style")
      ENDIF

      loformset.changemode('A')
    ENDIF
  ELSE      && ELSE IF ARIABROW(..)
    loformset.ariaform1.kbcolor.keytextbox.VALUE = SPACE(0)
    loformset.ariaform1.kbcolor.ENABLED = .T.
  ENDIF     && ENDIF ARIABROW(..)
  llbrowse = .F.
ENDIF

IF EMPTY(loformset.ariaform1.kbcolor.keytextbox.VALUE)
  loformset.ariaform1.kbcolor.ENABLED = .T.
ELSE
  loformset.ariaform1.kbcolor.ENABLED = .F.
ENDIF

SELECT (lncuralias)


*!*************************************************************
*! Name      : lfVldMatClr
*! Developer : Mariam Mazhar
*! Date      : 12/02/2009
*! Purpose   : Validates a Material Color.
*!*************************************************************
*! Calls              :   ARIABROW()
*!*************************************************************
*! Passed Parameters  :   None
*!*************************************************************
*! Returns            :  .T. if the style belongs to the project ID
*!                       .F. otherwise.
*!*************************************************************
*! Example            :  =lfVldStyle()
*!*************************************************************
FUNCTION lfvldmatclr
PRIVATE lncuralias, llvldstyle
llvldstyle = .F.

IF m.color = STRTRAN(loformset.lcnonmajpicmat,'X','*')
  llvldstyle = .T.
  loformset.nlinenum = 0
ELSE
  lncuralias = SELECT(0)
  =gfseek("0002"+ALLTRIM(m.cstyle), 'ITEM', 'Style')
  SELECT ITEM
  LOCATE REST WHILE SUBSTR(STYLE,1,loformset.lnmajorlenmat )= PADR(m.cstyle,loformset.lnmajorlenmat ) FOR ;
    RIGHT(STYLE,LEN(loformset.lcnonmajpicmat)) = PADR(m.color,LEN(loformset.lcnonmajpicmat))
  llvldstyle = FOUND()
  IF llvldstyle
    loformset.nlinenum = 0
    loformset.lcstyle = ITEM.STYLE
  ENDIF
  SELECT (lncuralias)
ENDIF

RETURN llvldstyle
*!*************************************************************
*! Name      : lfVldItem
*! Developer : Mariam Mazhar
*! Date      : 12/02/2009
*! Purpose   : Validate item
*!*************************************************************
*! Calls              :   ARIABROW()
*!*************************************************************
*! Passed Parameters  :   None
*!*************************************************************
*! Returns            :  .T. if the style belongs to the project ID
*!                       .F. otherwise.
*!*************************************************************
*! Example            :  =lfVldStyle()
*!*************************************************************
FUNCTION lfvlditem
PRIVATE lncuralias, llvldstyle
llvldstyle = .F.
IF m.cstyle = REPLICATE('*', loformset.lnmajorlenmat)
  llvldstyle = .T.
ELSE
  lncuralias = SELECT(0)
  llvldstyle = gfseek('0002'+ALLTRIM(m.cstyle), 'ITEM', 'Style')
  SELECT (lncuralias)
ENDIF
RETURN llvldstyle
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
