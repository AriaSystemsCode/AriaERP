*** 
*** ReFox XI+  #NK366474  Tarek  Tarek [FP25]
***
 lctime = gfgettime()
 PRIVATE lcstylscl
 STORE SPACE(0) TO lcstylscl
 lnfrstrec = 0
 lnfrstrec1 = 0
 llgnglcst = ALLTRIM(gfgetmemva('M_GL_COST'))='Y'
 lllinkgljl = ALLTRIM(gfgetmemva('M_LINK_GL'))='Y'
 lltextmode = (UPPER(ALLTRIM(lcrepmode))=='TEXT')
 DIMENSION lascals[ 8]
 STORE '' TO lascals
 IF lcrpsortby$'SW' .AND. ALEN(larpreptar, 1)=1 .AND.  .NOT. EMPTY(larpreptar)
    = lfswonetrn()
    RETURN
 ENDIF
 lcualoflds = 'UAlo1,UAlo2,UAlo3,UAlo4,UAlo5,UAlo6,UAlo7,UAlo8,TotUAlo'
 lciotsflds = 'IOTS1,IOTS2,IOTS3,IOTS4,IOTS5,IOTS6,IOTS7,IOTS8,TotIOTS'
 lcotsflds = 'OTS1,OTS2,OTS3,OTS4,OTS5,OTS6,OTS7,OTS8,TotOTS'
 lcbokflds = 'Bok1,Bok2,Bok3,Bok4,Bok5,Bok6,Bok7,Bok8,TotBok'
 lcrepnmttl = gfitemmask('HN')
 STORE SPACE(0) TO lcsortttl, lcgroupexp, lcsortexp, lcmajexp, lcnmajexp, lcdescexp, lcsortfld, lcsourflds, lctargflds, lcalltrns
 lcstkflds = 'nStkVWIP,nStkVSOH,nStkVPLA,nStkVOTS,nStkVIOTS,nStkVBOK,             nStkVSHP,nStkVRet,nStkVRetA,nStkVAlo,nStkVUAlo,nStkVInt,             nStkVWOrd,nStkVOrd'
 lcsalflds = 'nSalVWIP,nSalVSOH,nSalVPLA,nSalVOTS,nSalVIOTS,nSalVBOK,             nSalVSHP,nSalVRet,nSalVRetA,nSalVAlo,nSalVUAlo,nSalVInt,             nSalVWOrd,nSalVOrd'
 DIMENSION laallval[ 1], laualoval[ 9], laiotsval[ 9], laotsval[ 9], labokval[ 9], lastkval[ 14], lasalval[ 14]
 STORE 0 TO laallval, laualoval, laiotsval, laotsval, labokval, lastkval, lasalval
 STORE 0 TO lnmajsv, lnsorsv, lnrepsv
 STORE .T. TO llonlyots
 lcscale1 = SPACE(3)
 IF ALEN(larpreptar, 1)>0 .AND.  .NOT. EMPTY(larpreptar)
    IF llrpprnloc .AND.  .NOT. llmultiwh
       llrpwhdeta = .T.
    ENDIF
    = lfcrtmp()
    = lfdummfill()
    WAIT WINDOW NOWAIT 'Collecting data...'
    IF lcrpsortby='W'
       = lfdatcollw()
    ELSE
       = lfdatcolls()
    ENDIF
    WAIT CLEAR
 ELSE
    = gfmodalgen('INM42146B00000','Dialog')
    RETURN
 ENDIF
 lnoldrec = 0
 lcoldfld = SPACE(1)
 LOCATE
 IF EOF()
    WAIT WINDOW 'No records to display'
 ELSE
    IF lcrpsortby=='SE' .OR. lcrpsortby=='D'
       FOR lnlop = 1 TO 8
          lcsiz = 'SZ'+ALLTRIM(STR(lnlop))
          laScals[LnLop] = SCALE.&lcSiz
       ENDFOR
    ENDIF
    IF lcrpsortby=='SE' .OR. lcrpsortby=='D'
       GOTO BOTTOM
       STORE SPACE(6) TO lcseason, lcdivision
       lcseason = season
       lcdivision = division
       LOCATE FOR cendrep='A'
       REPLACE season WITH lcseason, division WITH lcdivision
       LOCATE
    ENDIF
    DO gfdispre WITH EVALUATE('lcRPFormNa')
 ENDIF
 IF USED(lctrns)
    SELECT (lctrns)
    SET RELATION TO
    USE IN (lctrns)
 ENDIF
 IF USED(lcdummy)
    SELECT (lcdummy)
    SET RELATION TO
    USE IN (lcdummy)
 ENDIF
 IF USED(lcstytmp)
    SELECT (lcstytmp)
    SET RELATION TO
    USE IN (lcstytmp)
 ENDIF
 IF USED(lctottmp)
    SELECT (lctottmp)
    SET RELATION TO
    USE IN (lctottmp)
 ENDIF
 ERASE (gcworkdir+lctrns+'.DBF')
 ERASE (gcworkdir+lctrns+'.CDX')
 ERASE (gcworkdir+lcdummy+'.DBF')
 ERASE (gcworkdir+lcdummy+'.CDX')
 ERASE (gcworkdir+lcstytmp+'.DBF')
 ERASE (gcworkdir+lcstytmp+'.CDX')
 ERASE (gcworkdir+lctottmp+'.DBF')
 ERASE (gcworkdir+lctottmp+'.CDX')
*
FUNCTION lfmajttget
 RETURN gfitemmask('HM')
*
FUNCTION lfnonmaj
 lnmajseg = gfitemmask('SM')
 DIMENSION lamajseg[ 1, 1]
 = gfitemmask(@lamajseg)
 llstopconc = .F.
 FOR lni = lnmajseg+1 TO ALEN(lamajseg, 1)
    lnnonmajpo = IIF(lnnonmajpo=0, lamajseg(lni,4), lnnonmajpo)
    IF lamajseg(lni,1)='F' .AND.  .NOT. llstopconc
       lcfreeclr = IIF(EMPTY(lcfreeclr), lamajseg(lni,1), lcfreeclr)
       lcnonmajpi = IIF(EMPTY(lcnonmajpi), lamajseg(lni,3), lcnonmajpi+lamajseg(lni-1,6)+lamajseg(lni,3))
       lcnonmajt = IIF(EMPTY(lcnonmajt), PADR(lamajseg(lni,2), LEN(lamajseg(lni,3))), lcnonmajt+lamajseg(lni-1,6)+PADR(lamajseg(lni,2), LEN(lamajseg(lni,3))))
    ENDIF
    IF lamajseg(lni,1)='C' .OR. ( .NOT. EMPTY(lcfreeclr) .AND. lamajseg(lni,1)<>'F')
       IF lamajseg(lni,1)='C'
          lnclrpo = lamajseg(lni,4)
          lcfreeclr = lamajseg(lni,1)
          lcnonmajpi = lamajseg(lni,3)
          lcnonmajt = PADR(lamajseg(lni,2), LEN(lamajseg(lni,3)))
          EXIT
       ELSE
          llstopconc = .T.
       ENDIF
    ENDIF
 ENDFOR
 STORE LEN(lcnonmajpi) TO lnfreelen, lncolorlen
 lccolortt = 'Only This '+ALLTRIM(lcnonmajt)
 DIMENSION lasetup[ 4, 2]
 lasetup[ 1, 1] = 'M_WAREHOUS'
 lasetup[ 2, 1] = 'M_WARELOC'
 lasetup[ 3, 1] = 'M_DYELOT'
 lasetup[ 4, 1] = 'M_COST_MET'
 = gfgetmemva(@lasetup)
 llmultiwh = ALLTRIM(lasetup(1,2))='Y'
 lltrakloc = ALLTRIM(lasetup(2,2))='Y'
 lldyelot = ALLTRIM(lasetup(3,2))='Y'
 lccstmeth = ALLTRIM(lasetup(4,2))
 llcostaccs = gfuserpriv('IC','ICSTYLE','COSTING')
 RETURN ''
*
PROCEDURE lfwrepwhen
 IF llcostaccs
    llshowcost = lcrpshow$'CB'
 ELSE
    llshowcost = .F.
 ENDIF
 IF llshowcost
    lccostmth = ALLTRIM(UPPER(gfgetmemva('M_COST_MET')))
 ENDIF
 llshowsale = lcrpshow$'SB'
 = lftransarr()
 = lfotsbstat()
 = lfdyedtsta()
 = lfwhdtstat()
 = lfwhsoptst()
 = lfprtsizst()
 = lfprtszscl()
*
PROCEDURE lfprtsizst
 PRIVATE lnvarszpos
 lnvarszpos = ASCAN(laogobjtyp, 'llRpPrtSiz')
 IF lnvarszpos>0
    lnvarszpos = ASUBSCRIPT(laogobjtyp, lnvarszpos, 1)
    laogobjcnt[ lnvarszpos] = ALEN(larpreptar, 1)=1
 ENDIF
 = lfogshowge('llRpPrtSiz')
*
PROCEDURE lfprtszscl
 PRIVATE lnvarszpos
 lnvarszpos = ASCAN(laogobjtyp, 'llRpScale')
 IF lnvarszpos>0
    lnvarszpos = ASUBSCRIPT(laogobjtyp, lnvarszpos, 1)
    laogobjcnt[ lnvarszpos] = llrpprtsiz
 ENDIF
 = lfogshowge('llRpScale')
*
PROCEDURE lfvprnreps
 = lftransarr()
 = gfmover(@larprepsou,@larpreptar,'Transactions to be printed',.T.,'lfvPrtSize',.F.,.T.)
 = lfprtsizst()
 = lfprtszscl()
 = lfchngform()
 = lfvsort()
 = lfotsbstat()
 = lfdyedtsta()
 = lfchkstyl()
*
PROCEDURE lfchkstyl
 llstyfnd = ASCAN(larpreptar, 'Stock')>0
 llnegdis = llstyfnd
 lnrpngatv = ASUBSCRIPT(laogobjtyp, ASCAN(laogobjtyp, 'llRpNgatv'), 1)
 laogobjcnt[ lnrpngatv] = llstyfnd
 = lfogshowge('llRpNgatv')
*
FUNCTION lfvprtsize
 PARAMETER lnbuttn
 PRIVATE lnmesbuttn, llreturn
 llreturn = .T.
 IF INLIST(lnbuttn, 1, 2)
    IF  .NOT. llrpprtsiz
       IF (ALEN(latarget, 1)=1 .AND.  .NOT. EMPTY(latarget)) .OR. lnbuttn=2
          lnmesbuttn = gfmodalgen('QRM42215B00012',.F.,'',.F.,'')
          llreturn = lnmesbuttn=1
          llrpprtsiz = llreturn
       ENDIF
    ENDIF
 ENDIF
 RETURN (llreturn)
*
FUNCTION lfmajpic
 lcmajpic = '@! '+gfitemmask('PM')
 RETURN lcmajpic
*
PROCEDURE lfvstyle
 lcstyle = VARREAD()
 lctag = ORDER('STYLE')
 SET ORDER TO cStyle IN style
 IF LASTKEY()=13 .AND.  .NOT. MDOWN()
    IF SEEK(&lcStyle.,'Style') 
       &lcStyle = STYLE.cStyMajor
    ELSE
       &lcStyle = gfStyBrw('M',"","",.F.)
    ENDIF
 ELSE
    &lcStyle = ''
 ENDIF
 SET ORDER TO lcTag IN style
*
PROCEDURE lfvwareho
 lcwareho = VARREAD()
 lctag = ORDER('WAREHOUS')
 SET ORDER TO WAREHOUS IN warehous
 IF LASTKEY()=13 .AND.  .NOT. MDOWN()
    IF SEEK(&lcWareHo.,'WAREHOUS') 
       &lcWareHo = WAREHOUS.cWareCode
    ELSE
       &lcWareHo = gfbrowware(.T.,.F.,.F.,.F.,.F.,'S')
    ENDIF
 ELSE
    &lcWareHo = ''
 ENDIF
 SET ORDER TO WAREHOUS IN warehous
*
PROCEDURE lfvfabric
 lcfabobj = VARREAD()
 lcFab    = &lcFabObj
 llusebyme = .F.
 IF  .NOT. USED('FABRIC')
    llusebyme = .T.
    USE IN 0 SHARED (gcdatadir+'FABRIC')
 ENDIF
 lctag = ORDER('FABRIC')
 SET ORDER TO FABRIC IN fabric
 IF LASTKEY()=13 .AND.  .NOT. MDOWN()
    IF SEEK(lcfab, 'FABRIC')
       &lcFabObj = FABRIC.Fabric
    ELSE
       = fabrow(@lcfab,'*')
       &lcFabObj = lcFab
    ENDIF
 ELSE
    &lcFabObj = ''
 ENDIF
 SET ORDER TO FABRIC IN fabric
 IF llusebyme
    USE IN fabric
 ENDIF
*
PROCEDURE lfvsort
 llplanfoun = ASCAN(larpreptar, 'Plan')>0
 IF (lcrpsortby='W' .OR. llrpwhdeta) .AND. llplanfoun
    = gfmodalgen('INM42147B00000','Dialog')
    lnarrlen = ALEN(larpreptar)
    = ADEL(larpreptar, ASCAN(larpreptar, 'Plan'))
    IF ALEN(larpreptar, 1)>1
       DIMENSION larpreptar[ lnarrlen-1]
    ELSE
       STORE SPACE(0) TO larpreptar
    ENDIF
 ENDIF
 = lfchngform()
 = lfotsbstat()
 = lfwhdtstat()
 = lfwhsoptst()
*
PROCEDURE lfvprnwhde
 = lfvsort()
 = lfwhsoptst()
*
PROCEDURE lfwhsoptst
 llwhsstat = lcrpsortby='W' .OR. llrpwhdeta
 lnwhspo = ASCAN(laogvrflt, 'STYDYE.CWARECODE')
 IF lnwhspo>0
    lnwhspo = ASUBSCRIPT(laogvrflt, lnwhspo, 1)
    laogobjcnt[ ALEN(laogobjcnt, 1)-ALEN(laogvrflt, 1)+lnwhspo] = llwhsstat
    = lfogshowge('laOGVrFlt['+ALLTRIM(STR(lnwhspo))+',6]')
 ENDIF
*
PROCEDURE lfvprint
 IF llcostaccs
    llshowcost = lcrpshow$'CB'
 ELSE
    llshowcost = .F.
 ENDIF
 IF llshowcost
    lccostmth = ALLTRIM(UPPER(gfgetmemva('M_COST_MET')))
 ENDIF
 llshowsale = lcrpshow$'SB'
 IF (lclastsel$'SCB' .AND. lcrpshow='N') .OR. (lclastsel='N' .AND. lcrpshow$'SCB')
    DIMENSION larpreptar[ 1]
    larpreptar = SPACE(0)
 ENDIF
 lclastsel = lcrpshow
 = lfotsbstat()
 = lfdyedtsta()
*
PROCEDURE lfotsbstat
 lnotssig = ASUBSCRIPT(laogobjtyp, ASCAN(laogobjtyp, 'lcRPOTSSig'), 1)
 lnotsmin = ASUBSCRIPT(laogobjtyp, ASCAN(laogobjtyp, 'lnRPOTSMin'), 1)
 lnotsb = ASUBSCRIPT(laogobjtyp, ASCAN(laogobjtyp, 'lcRPOTSB'), 1)
 llotsfoun = ASCAN(larpreptar, 'OTS')>0 .OR. ASCAN(larpreptar, 'Imm. OTS')>0
 IF llotsfoun
    IF ( .NOT. (lcrpsortby=='W') .AND.  .NOT. llrpwhdeta)
       llstatob = .T.
       llstatos = .T.
       llstatom = .T.
    ELSE
       lcrpotsb = 'W'
       llstatob = .F.
       llstatos = .T.
       llstatom = .T.
    ENDIF
 ELSE
    llstatob = .F.
    llstatos = .F.
    llstatom = .F.
 ENDIF
 laogobjcnt[ lnotsb] = llstatob
 laogobjcnt[ lnotssig] = llstatos
 laogobjcnt[ lnotsmin] = llstatom
 = lfogshowge('lcRPOTSB')
 = lfogshowge('lcRPOTSSig')
 = lfogshowge('lnRPOTSMin')
*
PROCEDURE lfvots
 DO CASE
    CASE lcrpotssig='P'
       lnrpotsmin = 1
    CASE lcrpotssig='N'
       lnrpotsmin = -1
 ENDCASE
*
PROCEDURE lfdyedtsta
 IF lldyelot
    llstat = ASCAN(larpreptar, 'Stock')>0
 ELSE
    llstat = .F.
 ENDIF
 lnprndyep = ASCAN(laogobjtyp, 'llRPPrnDye')
 IF lnprndyep>0
    lnprndyep = ASUBSCRIPT(laogobjtyp, lnprndyep, 1)
    laogobjcnt[ lnprndyep] = llstat
    IF  .NOT. llstat
       llrpprndye = .F.
    ENDIF
    = lfogshowge('llRPPrnDye')
 ENDIF
*
PROCEDURE lfwhdtstat
 llstat = llmultiwh .AND. lcrpsortby<>'W'
 lnpwdpo = ASCAN(laogobjtyp, 'llRPWhDeta')
 IF lnpwdpo>0
    lnpwdpo = ASUBSCRIPT(laogobjtyp, lnpwdpo, 1)
    laogobjcnt[ lnpwdpo] = llstat
    = lfogshowge('llRPWhDeta')
 ENDIF
 lnplopo = ASCAN(laogobjtyp, 'llRPPrnLoc')
 IF lnplopo>0
    lnplopo = ASUBSCRIPT(laogobjtyp, lnplopo, 1)
    laogobjcnt[ lnplopo] = lltrakloc .AND. ((llmultiwh .AND. llrpwhdeta) .OR.  .NOT. llmultiwh .OR. lcrpsortby='W')
    = lfogshowge('llRPPrnLoc')
 ENDIF
*
PROCEDURE lftransarr
 lctrns = gftempname()
 lcstytmp = gftempname()
 lctottmp = gftempname()
 lcdummy = gftempname()
 IF TYPE('laRPRepSou[1,1]')='U' .OR. EMPTY(larprepsou(1,1))
    DIMENSION larprepsou[ 1]
    STORE SPACE(0) TO larprepsou
 ENDIF
 IF TYPE('laRPRepTar[1,1]')='U' .OR. EMPTY(larpreptar(1,1))
    DIMENSION larpreptar[ 1]
    STORE SPACE(0) TO larpreptar
 ENDIF
 DIMENSION laalltrns[ 15, 2]
 laalltrns[ 01, 1] = 'WIP'
 laalltrns[ 02, 1] = 'Stock'
 laalltrns[ 03, 1] = 'Dyelot'
 laalltrns[ 04, 1] = 'Plan'
 laalltrns[ 05, 1] = 'Unallocated'
 laalltrns[ 06, 1] = 'OTS'
 laalltrns[ 07, 1] = 'Imm. OTS'
 laalltrns[ 08, 1] = 'Orders'
 laalltrns[ 09, 1] = 'Work orders'
 laalltrns[ 10, 1] = 'Intransit'
 laalltrns[ 11, 1] = 'Book'
 laalltrns[ 12, 1] = 'Shipped'
 laalltrns[ 13, 1] = 'Allocated'
 laalltrns[ 14, 1] = 'Return'
 laalltrns[ 15, 1] = 'Return auth.'
 laalltrns[ 01, 2] = 'WIP'
 laalltrns[ 02, 2] = 'SOH'
 laalltrns[ 03, 2] = 'SOH'
 laalltrns[ 04, 2] = 'PLA'
 laalltrns[ 05, 2] = 'UALO'
 laalltrns[ 06, 2] = 'OTS'
 laalltrns[ 07, 2] = 'IOTS'
 laalltrns[ 08, 2] = 'ORD'
 laalltrns[ 09, 2] = 'WORD'
 laalltrns[ 10, 2] = 'INT'
 laalltrns[ 11, 2] = 'BOK'
 laalltrns[ 12, 2] = 'SHP'
 laalltrns[ 13, 2] = 'ALO'
 laalltrns[ 14, 2] = 'RET'
 laalltrns[ 15, 2] = 'RETA'
 IF UPPER(lcrpshow)$'SCB'
    DIMENSION larprepsou[ 6]
    larprepsou[ 01] = 'WIP'
    larprepsou[ 02] = 'Stock'
    larprepsou[ 03] = 'Plan'
    larprepsou[ 04] = 'Unallocated'
    larprepsou[ 05] = 'OTS'
    larprepsou[ 06] = 'Imm. OTS'
 ELSE
    DIMENSION larprepsou[ 14]
    larprepsou[ 01] = 'WIP'
    larprepsou[ 02] = 'Stock'
    larprepsou[ 03] = 'Plan'
    larprepsou[ 04] = 'Unallocated'
    larprepsou[ 05] = 'OTS'
    larprepsou[ 06] = 'Imm. OTS'
    larprepsou[ 07] = 'Orders'
    larprepsou[ 08] = 'Work orders'
    larprepsou[ 09] = 'Intransit'
    larprepsou[ 10] = 'Book'
    larprepsou[ 11] = 'Shipped'
    larprepsou[ 12] = 'Allocated'
    larprepsou[ 13] = 'Return'
    larprepsou[ 14] = 'Return auth.'
 ENDIF
*
PROCEDURE lfcrtmp
 lni = 1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'StyCode'
 lafilestru[ lni, 2] = 'C'
 lafilestru[ lni, 3] = 19
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'StyDesc'
 lafilestru[ lni, 2] = 'C'
 lafilestru[ lni, 3] = 60
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'cStyMajor'
 lafilestru[ lni, 2] = 'C'
 lafilestru[ lni, 3] = 19
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'Status'
 lafilestru[ lni, 2] = 'C'
 lafilestru[ lni, 3] = 1
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'Scale'
 lafilestru[ lni, 2] = 'C'
 lafilestru[ lni, 3] = 3
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'Price'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 12
 lafilestru[ lni, 4] = 2
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'GL_Link'
 lafilestru[ lni, 2] = 'C'
 lafilestru[ lni, 3] = 6
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'TotCost'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 13
 lafilestru[ lni, 4] = 2
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'Ave_Cost'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 15
 lafilestru[ lni, 4] = 7
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'Dyelot'
 lafilestru[ lni, 2] = 'C'
 lafilestru[ lni, 3] = 10
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'WareCode'
 lafilestru[ lni, 2] = 'C'
 lafilestru[ lni, 3] = 6
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'WareDesc'
 lafilestru[ lni, 2] = 'C'
 lafilestru[ lni, 3] = 6
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'WIP1'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'WIP2'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'WIP3'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'WIP4'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'WIP5'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'WIP6'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'WIP7'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'WIP8'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'TotWIP'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'SOH1'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'SOH2'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'SOH3'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'SOH4'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'SOH5'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'SOH6'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'SOH7'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'SOH8'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'TotSOH'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'Pla1'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 10
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'Pla2'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 10
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'Pla3'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 10
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'Pla4'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 10
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'Pla5'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 10
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'Pla6'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 10
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'Pla7'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 10
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'Pla8'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 10
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'TotPla'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 10
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'UAlo1'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 10
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'UAlo2'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 10
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'UAlo3'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 10
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'UAlo4'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 10
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'UAlo5'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 10
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'UAlo6'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 10
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'UAlo7'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 10
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'UAlo8'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 10
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'TotUAlo'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 10
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'OTS1'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'OTS2'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'OTS3'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'OTS4'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'OTS5'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'OTS6'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'OTS7'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'OTS8'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'TotOTS'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'IOTS1'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'IOTS2'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'IOTS3'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'IOTS4'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'IOTS5'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'IOTS6'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'IOTS7'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'IOTS8'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'TotIOTS'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'Ord1'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'Ord2'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'Ord3'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'Ord4'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'Ord5'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'Ord6'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'Ord7'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'Ord8'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'TotOrd'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'WOrd1'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'WOrd2'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'WOrd3'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'WOrd4'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'WOrd5'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'WOrd6'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'WOrd7'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'WOrd8'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'TotWOrd'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'Int1'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'Int2'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'Int3'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'Int4'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'Int5'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'Int6'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'Int7'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'Int8'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'TotInt'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'Bok1'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'Bok2'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'Bok3'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'Bok4'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'Bok5'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'Bok6'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'Bok7'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'Bok8'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'TotBok'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'Shp1'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'Shp2'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'Shp3'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'Shp4'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'Shp5'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'Shp6'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'Shp7'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'Shp8'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'TotShp'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'Alo1'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'Alo2'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'Alo3'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'Alo4'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'Alo5'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'Alo6'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'Alo7'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'Alo8'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'TotAlo'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'Ret1'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'Ret2'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'Ret3'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'Ret4'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'Ret5'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'Ret6'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'Ret7'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'Ret8'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'TotRet'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'RetA1'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'RetA2'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'RetA3'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'RetA4'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'RetA5'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'RetA6'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'RetA7'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'RetA8'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'TotRetA'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 8
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'HasDye'
 lafilestru[ lni, 2] = 'L'
 lafilestru[ lni, 3] = 0
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'lPrnOTS'
 lafilestru[ lni, 2] = 'L'
 lafilestru[ lni, 3] = 0
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'lPrnIOTS'
 lafilestru[ lni, 2] = 'L'
 lafilestru[ lni, 3] = 0
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'Season'
 lafilestru[ lni, 2] = 'C'
 lafilestru[ lni, 3] = 6
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'Division'
 lafilestru[ lni, 2] = 'C'
 lafilestru[ lni, 3] = 6
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'FGroup'
 lafilestru[ lni, 2] = 'C'
 lafilestru[ lni, 3] = 7
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'ITEM_TYPE'
 lafilestru[ lni, 2] = 'C'
 lafilestru[ lni, 3] = 30
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'SGroup'
 lafilestru[ lni, 2] = 'C'
 lafilestru[ lni, 3] = 6
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'Loc'
 lafilestru[ lni, 2] = 'M'
 lafilestru[ lni, 3] = 0
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'HasLoc'
 lafilestru[ lni, 2] = 'L'
 lafilestru[ lni, 3] = 1
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'nStkVWIP'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 18
 lafilestru[ lni, 4] = 2
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'nStkVSOH'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 18
 lafilestru[ lni, 4] = 2
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'nStkVPLA'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 18
 lafilestru[ lni, 4] = 2
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'nStkVOTS'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 18
 lafilestru[ lni, 4] = 2
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'nStkVIOTS'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 18
 lafilestru[ lni, 4] = 2
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'nStkVBOK'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 18
 lafilestru[ lni, 4] = 2
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'nStkVSHP'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 18
 lafilestru[ lni, 4] = 2
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'nStkVRet'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 18
 lafilestru[ lni, 4] = 2
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'nStkVRetA'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 18
 lafilestru[ lni, 4] = 2
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'nStkVAlo'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 18
 lafilestru[ lni, 4] = 2
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'nStkVUAlo'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 18
 lafilestru[ lni, 4] = 2
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'nStkVInt'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 18
 lafilestru[ lni, 4] = 2
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'nStkVWOrd'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 18
 lafilestru[ lni, 4] = 2
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'nStkVOrd'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 18
 lafilestru[ lni, 4] = 2
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'nSalVWIP'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 18
 lafilestru[ lni, 4] = 2
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'nSalVSOH'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 18
 lafilestru[ lni, 4] = 2
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'nSalVPLA'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 18
 lafilestru[ lni, 4] = 2
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'nSalVOTS'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 18
 lafilestru[ lni, 4] = 2
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'nSalVIOTS'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 18
 lafilestru[ lni, 4] = 2
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'nSalVBOK'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 18
 lafilestru[ lni, 4] = 2
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'nSalVSHP'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 18
 lafilestru[ lni, 4] = 2
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'nSalVRet'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 18
 lafilestru[ lni, 4] = 2
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'nSalVRetA'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 18
 lafilestru[ lni, 4] = 2
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'nSalVAlo'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 18
 lafilestru[ lni, 4] = 2
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'nSalVUAlo'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 18
 lafilestru[ lni, 4] = 2
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'nSalVInt'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 18
 lafilestru[ lni, 4] = 2
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'nSalVWOrd'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 18
 lafilestru[ lni, 4] = 2
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'nSalVOrd'
 lafilestru[ lni, 2] = 'N'
 lafilestru[ lni, 3] = 18
 lafilestru[ lni, 4] = 2
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'cEndMaj'
 lafilestru[ lni, 2] = 'C'
 lafilestru[ lni, 3] = 1
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'cEndSort'
 lafilestru[ lni, 2] = 'C'
 lafilestru[ lni, 3] = 1
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'cEndRep'
 lafilestru[ lni, 2] = 'C'
 lafilestru[ lni, 3] = 1
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'NotScale'
 lafilestru[ lni, 2] = 'C'
 lafilestru[ lni, 3] = 1
 lafilestru[ lni, 4] = 0
 DIMENSION laindx[ 2, 2]
 laindx[ 1, 1] = 'WareCode+StyCode+Dyelot+cEndMaj+cEndSort+cEndRep+NotScale'
 laindx[ 1, 2] = 'WareSort'
 laindx[ 2, 1] = 'StyCode+WareCode+Dyelot+NotScale'
 laindx[ 2, 2] = 'StySort'
 = gfcrttmp(lctrns,@lafilestru,@laindx)
 SELECT (lctrns)
 = AFIELDS(lafilestru)
 DIMENSION laindx[ 1, 2]
 DO CASE
    CASE lcrpsortby=='S'
       laindx[ 1, 1] = 'StyCode+WareCode+Dyelot+cEndMaj+cEndSort+cEndRep+NotScale'
       laindx[ 1, 2] = 'StySort'
    CASE lcrpsortby=='SE'
       laindx[ 1, 1] = 'Season+StyCode+WareCode+Dyelot+cEndMaj+cEndSort+cEndRep+NotScale'
       laindx[ 1, 2] = 'SeaSort'
    CASE lcrpsortby=='D'
       laindx[ 1, 1] = 'Division+StyCode+WareCode+Dyelot+cEndMaj+cEndSort+cEndRep+NotScale'
       laindx[ 1, 2] = 'DivSort'
    CASE lcrpsortby=='FG'
       laindx[ 1, 1] = 'FGroup+StyCode+WareCode+Dyelot+cEndMaj+cEndSort+cEndRep+NotScale'
       laindx[ 1, 2] = 'FGrpSort'
    CASE lcrpsortby=='MT'
       laindx[ 1, 1] = 'ITEM_TYPE+StyCode+WareCode+Dyelot+cEndMaj+cEndSort+cEndRep+NotScale'
       laindx[ 1, 2] = 'MatTypSort'
    CASE lcrpsortby=='SG'
       laindx[ 1, 1] = 'SGroup+StyCode+WareCode+Dyelot+cEndMaj+cEndSort+cEndRep+NotScale'
       laindx[ 1, 2] = 'SGrpSort'
 ENDCASE
 = gfcrttmp(lcstytmp,@lafilestru,@laindx)
 DO CASE
    CASE lcrpsortby=='W'
       laindx[ 1, 1] = 'cEndMaj+cEndSort+cEndRep+WareCode+cStyMajor+NotScale'
       laindx[ 1, 2] = 'WareSort'
    CASE lcrpsortby=='S'
       laindx[ 1, 1] = 'cEndMaj+cEndSort+cEndRep+cStyMajor+NotScale'
       laindx[ 1, 2] = 'StySort'
    CASE lcrpsortby=='SE'
       laindx[ 1, 1] = 'cEndMaj+cEndSort+cEndRep+Season+cStyMajor+NotScale'
       laindx[ 1, 2] = 'SeaSort'
    CASE lcrpsortby=='D'
       laindx[ 1, 1] = 'cEndMaj+cEndSort+cEndRep+Division+cStyMajor+NotScale'
       laindx[ 1, 2] = 'DivSort'
    CASE lcrpsortby=='FG'
       laindx[ 1, 1] = 'cEndMaj+cEndSort+cEndRep+FGroup+cStyMajor+NotScale'
       laindx[ 1, 2] = 'FGrpSort'
    CASE lcrpsortby=='MT'
       laindx[ 1, 1] = 'cEndMaj+cEndSort+cEndRep+ITEM_TYPE+cStyMajor+NotScale'
       laindx[ 1, 2] = 'FGrpSort'
    CASE lcrpsortby=='SG'
       laindx[ 1, 1] = 'cEndMaj+cEndSort+cEndRep+SGroup+cStyMajor+NotScale'
       laindx[ 1, 2] = 'SGrpSort'
 ENDCASE
 = gfcrttmp(lctottmp,@lafilestru,@laindx)
 lni = 1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'Item'
 lafilestru[ lni, 2] = 'C'
 lafilestru[ lni, 3] = 4
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'Label'
 lafilestru[ lni, 2] = 'C'
 lafilestru[ lni, 3] = 14
 lafilestru[ lni, 4] = 0
 lni = ALEN(lafilestru, 1)+1
 DIMENSION lafilestru[ lni, 4]
 lafilestru[ lni, 1] = 'cRecNo'
 lafilestru[ lni, 2] = 'C'
 lafilestru[ lni, 3] = 2
 lafilestru[ lni, 4] = 0
 = gfcrttmp(lcdummy,@lafilestru,'cRecNo',lcdummy)
*
PROCEDURE lfdummfill
 = ACOPY(larpreptar, latararray)
 IF lcrpsortby='W'
    IF llrpprndye
       lnstkpo = ASCAN(latararray, 'Stock')
       IF lnstkpo>0
          DIMENSION latararray[ ALEN(latararray)+1]
          = AINS(latararray, lnstkpo+1)
          latararray[ lnstkpo+1] = 'Dyelot'
       ENDIF
    ELSE
       lndyepo = ASCAN(latararray, 'Dyelot')
       IF lndyepo>0
          = ADEL(latararray, lndyepo)
          DIMENSION latararray[ ALEN(latararray, 1)-1]
       ENDIF
    ENDIF
 ENDIF
 FOR lni = 1 TO ALEN(latararray)
    lnelemno = ASUBSCRIPT(laalltrns, ASCAN(laalltrns, latararray(lni)), 1)
    IF  .NOT. (UPPER(laalltrns(lnelemno,2))$'IOTS')
       llonlyots = .F.
    ENDIF
    lcalltrns = lcalltrns+','+laalltrns(lnelemno,2)+','
    IF  .NOT. (UPPER(laalltrns(lnelemno,2))$'OTS,IOTS,BOK') .AND. UPPER(laalltrns(lnelemno,2))<>'UALO'
       lctargflds = IIF(EMPTY(lctargflds), lctargflds, lctargflds+',')+laalltrns(lnelemno,2)+'1,'+laalltrns(lnelemno,2)+'2,'+laalltrns(lnelemno,2)+'3,'+laalltrns(lnelemno,2)+'4,'+laalltrns(lnelemno,2)+'5,'+laalltrns(lnelemno,2)+'6,'+laalltrns(lnelemno,2)+'7,'+laalltrns(lnelemno,2)+'8,'+'Tot'+laalltrns(lnelemno,2)
       = lfsourflds(UPPER(laalltrns(lnelemno,2)))
    ENDIF
    INSERT INTO (lcdummy) (item, label, crecno) VALUE (laalltrns(lnelemno,2), latararray(lni), PADL(lni, 2, '0'))
 ENDFOR
 INSERT INTO (lcdummy) (item, label, crecno) VALUE ('', '', PADL(RECNO(), 2, '0'))
*
PROCEDURE lfsourflds
 PARAMETER lctrnfld
 PRIVATE lctrnfld
 DO CASE
    CASE lctrnfld='WIP'
       lcsourflds = lcsourflds+IIF(EMPTY(lcsourflds), '', ',')+'WIP1,WIP2,WIP3,WIP4,WIP5,WIP6,WIP7,WIP8,TotWIP'
    CASE lctrnfld='SOH'
       lcsourflds = lcsourflds+IIF(EMPTY(lcsourflds), '', ',')+'STK1,STK2,STK3,STK4,STK5,STK6,STK7,STK8,TotSTK'
    CASE lctrnfld='PLA'
       lcsourflds = lcsourflds+IIF(EMPTY(lcsourflds), '', ',')+'PLAN1,PLAN2,PLAN3,PLAN4,PLAN5,PLAN6,PLAN7,PLAN8,TotPLAN'
    CASE lctrnfld='ORD'
       lcsourflds = lcsourflds+IIF(EMPTY(lcsourflds), '', ',')+'ORD1,ORD2,ORD3,ORD4,ORD5,ORD6,ORD7,ORD8,TotORD'
    CASE lctrnfld='WORD'
       lcsourflds = lcsourflds+IIF(EMPTY(lcsourflds), '', ',')+'NWO1,NWO2,NWO3,NWO4,NWO5,NWO6,NWO7,NWO8,NTotWO'
    CASE lctrnfld='INT'
       lcsourflds = lcsourflds+IIF(EMPTY(lcsourflds), '', ',')+'INTRANS1,INTRANS2,INTRANS3,INTRANS4,INTRANS5,INTRANS6,INTRANS7,INTRANS8,TotINTRN'
    CASE lctrnfld='SHP'
       lcsourflds = lcsourflds+IIF(EMPTY(lcsourflds), '', ',')+'SHP1,SHP2,SHP3,SHP4,SHP5,SHP6,SHP7,SHP8,TotSHP'
    CASE lctrnfld='ALO'
       lcsourflds = lcsourflds+IIF(EMPTY(lcsourflds), '', ',')+'ALO1,ALO2,ALO3,ALO4,ALO5,ALO6,ALO7,ALO8,TotALO'
    CASE lctrnfld=='RET'
       lcsourflds = lcsourflds+IIF(EMPTY(lcsourflds), '', ',')+'RET1,RET2,RET3,RET4,RET5,RET6,RET7,RET8,TotRET'
    CASE lctrnfld='RETA'
       lcsourflds = lcsourflds+IIF(EMPTY(lcsourflds), '', ',')+'RA1,RA2,RA3,RA4,RA5,RA6,RA7,RA8,TotRA'
 ENDCASE
*
FUNCTION lfcostsalp
 lcreturn = ''
 IF llshowcost
    IF  .NOT. llgnglcst .AND. lccstmeth='S'
       lcreturn = SPACE(15)+'Unit cost :    '
    ELSE
       lcreturn = SPACE(15)+'Average cost : '
    ENDIF
    IF lcrpsortby=='W'
       IF  .NOT. llgnglcst .AND. lccstmeth='S'
          lcReturn = lcReturn + ALLTRIM(STR(&lcTrns..TotCost,10,2))
       ELSE
          lcReturn = lcReturn + ALLTRIM(STR(&lcTrns..Ave_Cost,10,2))
       ENDIF
    ELSE
       IF  .NOT. llgnglcst .AND. lccstmeth='S'
          lcreturn = lcreturn+ALLTRIM(STR(EVALUATE(lcstytmp+'.TotCost'), 10, 2))
       ELSE
          lcreturn = lcreturn+ALLTRIM(STR(EVALUATE(lcstytmp+'.Ave_Cost'), 10, 2))
       ENDIF
    ENDIF
    IF llshowsale
       IF lcrpsortby=='W'
          lcreturn = lcreturn+SPACE(15)+'Unit Price  : '+ALLTRIM(STR(EVALUATE(lctrns+'.Price'), 12, 2))
       ELSE
          lcreturn = lcreturn+SPACE(15)+'Unit Price  : '+ALLTRIM(STR(EVALUATE(lcstytmp+'.Price'), 12, 2))
       ENDIF
    ENDIF
 ELSE
    IF llshowsale
       IF lcrpsortby=='W'
          lcreturn = SPACE(15)+'Unit Price : '+ALLTRIM(STR(EVALUATE(lctrns+'.Price'), 12, 2))
       ELSE
          lcreturn = SPACE(15)+'Unit Price : '+ALLTRIM(STR(EVALUATE(lcstytmp+'.Price'), 12, 2))
       ENDIF
    ENDIF
 ENDIF
 RETURN lcreturn
*
FUNCTION lfcheckdye
 RETURN (ALLTRIM(UPPER(gfgetmemva('M_DYELOT')))='Y')
*
FUNCTION lfextscale
 RETURN gfgetmemva('M_USEEXSSC')
*
PROCEDURE lfdatcollw
 PRIVATE lcstyle, lcdyelot, llhasdye, lcscale
 llhasdye = .F.
 SELECT style
 SET ORDER TO STYLE
 SET RELATION TO 'S'+scale INTO scale ADDITIVE
 SELECT stydye
 SET ORDER TO STYDYEW DESCENDING
 SET RELATION TO style INTO style ADDITIVE
 lcstyle = SPACE(19)
 lcscale = SPACE(3)
 IF llnegdis
    IF llrpngatv
       IF  .NOT. llrpwhdeta
          lcrpexp = IIF(EMPTY(lcrpexp), '(STYLE.TOTSTK < 0)', lcrpexp+' AND '+'(STYLE.TOTSTK < 0)')
       ELSE
          lcrpexp = IIF(EMPTY(lcrpexp), '(STYDYE.TOTSTK < 0)', lcrpexp+' AND '+'(STYDYE.TOTSTK < 0)')
       ENDIF
    ELSE
       lcrpexp = IIF(EMPTY(lcrpexp), '(STYLE.TOTSTK >= 0)', lcrpexp+' AND '+'(STYLE.TOTSTK >= 0)')
    ENDIF
 ENDIF
 
 **tmi** T20140807.0010 - File not valid error message when printing custom style summary
 lladded = .F.
 **tmi**
 
 
 SCAN FOR &lcRpExp
    IF EMPTY(lcscale) .OR. TYPE('laStyScale')='U'
       lcscale = scale.scale
    ENDIF
    lladded = .T.
    IF llrpprndye
       IF lcstyle<>stydye.style
          llhasdye =  .NOT. EMPTY(dyelot)
       ENDIF
       lcstyle = stydye.style
    ENDIF
    IF  .NOT. EMPTY(dyelot)
       LOOP
    ENDIF
    DO lpinsintmp WITH 'STYDYE'
    IF !llRPShwZer AND  (&lcTrns..TotWip  + &lcTrns..TotSOH + &lcTrns..TotPLA  + &lcTrns..TotOTS +  &lcTrns..TotIOTS + &lcTrns..TotBOK + &lcTrns..TotSHP  + &lcTrns..TotRet +  &lcTrns..TotRetA + &lcTrns..TotAlo + &lcTrns..TotUAlo + &lcTrns..TotInt +  &lcTrns..TotWOrd + &lcTrns..TotOrd) = 0
       SELECT (lctrns)
       DELETE
       lladded = .F.
    ELSE
       IF llOnlyOTS AND !&lcTrns..lPrnOTS AND !&lcTrns..lPrnIOTS
          SELECT (lctrns)
          DELETE
          lladded = .F.
       ELSE
          SELECT (lctrns)
          SCATTER MEMVAR FIELDS cstymajor, stycode, warecode, dyelot, scale, price, totcost, ave_cost
          SELECT (lctottmp)
          IF !SEEK('A  '+&lcTrns..WareCode+&lcTrns..cStyMajor)
             APPEND BLANK
             GATHER MEMVAR
             REPLACE cendmaj WITH 'A', notscale WITH 'Y'
          ENDIF
          = lfupdtotal('Maj',lctrns)
          IF !SEEK('AA '+&lcTrns..WareCode)
             APPEND BLANK
             GATHER MEMVAR
             REPLACE cendmaj WITH 'A', cendsort WITH 'A', notscale WITH 'Y'
          ENDIF
          = lfupdtotal('Sor',lctrns)
          IF  .NOT. SEEK('AAA')
             APPEND BLANK
             GATHER MEMVAR
             REPLACE cendmaj WITH 'A', cendsort WITH 'A', cendrep WITH 'A', notscale WITH 'Y'
          ENDIF
          = lfupdtotal('Rep',lctrns)
       ENDIF
       IF llrpprnloc
          IF SEEK(stydye.style+SPACE(6)+stydye.cwarecode, 'WHSLOC')
             SELECT whsloc
             lcloc = ''
             SCAN REST FOR  .NOT. EMPTY(clocation) WHILE style+color+cwarecode=stydye.style+SPACE(6)+stydye.cwarecode
                llhasloc = .T.
                lcloc = lcloc+IIF(EMPTY(lcloc), 'Bins                 ', SPACE(2))+whsloc.clocation
             ENDSCAN
             IF llhasloc
                SELECT (lctrns)
                REPLACE hasloc WITH llhasloc, loc WITH lcloc
             ENDIF
          ENDIF
       ENDIF
    ENDIF
    IF lladded
       SELECT (lctrns)
       IF lcscale=scale.scale
          SCATTER TO lastyscale
       ELSE
          APPEND BLANK
          GATHER FROM lastyscale
          REPLACE notscale WITH 'N'
          lcscale = scale.scale
       ENDIF
    ENDIF
 ENDSCAN
 IF lladded
    SELECT (lctrns)
    APPEND BLANK
    GATHER FROM lastyscale
    REPLACE notscale WITH 'N'
 ENDIF
 SELECT (lctrns)
 APPEND FROM (gcworkdir+lctottmp)
 SELECT style
 SET ORDER TO
 SET RELATION TO
 SELECT stydye
 SET ORDER TO
 SET RELATION TO
 DO lpwarerela
*
PROCEDURE lfdatcolls
 PRIVATE lcstyle, llhasdye, lcware, lni, lcprvmaj, lcscale
 DO CASE
    CASE lcrpsortby=='S'
       lcsortkey = "''"
    CASE lcrpsortby=='SE'
       lcsortkey = lcstytmp+'.Season'
    CASE lcrpsortby=='D'
       lcsortkey = lcstytmp+'.Division'
    CASE lcrpsortby=='FG'
       lcsortkey = lcstytmp+'.FGroup'
    CASE lcrpsortby=='MT'
       lcsortkey = lcstytmp+'.ITEM_TYPE'
    CASE lcrpsortby=='SG'
       lcsortkey = lcstytmp+'.SGroup'
 ENDCASE
 llhasdye = .F.
 lcstylewar = SPACE(25)
 SELECT stydye
 SET ORDER TO STYDYE DESCENDING
 SELECT style
 SET ORDER TO STYLE
 SET RELATION TO 'S'+scale INTO scale ADDITIVE
 SET RELATION TO style INTO stydye ADDITIVE
 SET RELATION TO fabric INTO fabric ADDITIVE
 IF llrpwhdeta
    = ACOPY(laoghdflt, lamyheddin)
    = ACOPY(laogfxflt, lamyfixed)
    = ACOPY(laogvrflt, lamyvaria)
    lni = 0
    FOR lni = 1 TO ALEN(laoghdflt, 1)
       IF  .NOT. ('STYLE.'$laoghdflt(lni,1))
          laoghdflt[ lni, 6] = SPACE(0)
       ENDIF
    ENDFOR
    lni = 0
    FOR lni = 1 TO ALEN(laogfxflt, 1)
       IF  .NOT. ('STYLE.'$laogfxflt(lni,1))
          laogfxflt[ lni, 6] = SPACE(0)
       ENDIF
    ENDFOR
    lni = 0
    FOR lni = 1 TO ALEN(laogvrflt, 1)
       IF  .NOT. ('STYLE.'$laogvrflt(lni,1))
          laogvrflt[ lni, 6] = SPACE(0)
       ENDIF
    ENDFOR
    lchdnexp = gfgenflt('laOGHdFlt',.T.)
    lcfxdexp = gfgenflt('laOGFxFlt',.T.)
    lcvarexp = gfgenflt('laOGVrFlt',.T.)
    lcmyrepexp = IIF( .NOT. EMPTY(lchdnexp), '('+lchdnexp+')', '')
    lcmyrepexp = lcmyrepexp+IIF( .NOT. EMPTY(lcfxdexp), ' AND ('+lcfxdexp+')', '')
    lcmyrepexp = lcmyrepexp+IIF( .NOT. EMPTY(lcvarexp), ' AND ('+lcvarexp+')', '')
    lcmyrepexp = IIF(EMPTY(lcmyrepexp), "''", lcmyrepexp)
    = ACOPY(lamyheddin, laoghdflt)
    = ACOPY(lamyfixed, lamyheddin)
    = ACOPY(lamyvaria, laogvrflt)
 ELSE
    lcmyrepexp = lcrpexp
 ENDIF
 lcprvmaj = SPACE(19)
 lnlocno = 0
 SELECT style
 IF llnegdis
    IF llrpngatv
       IF  .NOT. llrpwhdeta
          lcrpexp = IIF(EMPTY(lcrpexp), '(STYLE.TOTSTK < 0)', lcrpexp+' AND '+'(STYLE.TOTSTK < 0)')
       ELSE
          lcrpexp = IIF(EMPTY(lcrpexp), '(STYDYE.TOTSTK < 0)', lcrpexp+' AND '+'(STYDYE.TOTSTK < 0)')
       ENDIF
    ELSE
       lcmyrepexp = IIF(EMPTY(lcrpexp), '(STYLE.TOTSTK >= 0)', lcrpexp+' AND '+'(STYLE.TOTSTK >= 0)')
    ENDIF
 ENDIF
 lcscale = SPACE(3)
 SCAN FOR &lcMyRepExp
    lladded = .T.
    DO lpinsintmp WITH 'STYLE'
    IF !llRPShwZer AND  (&lcStyTmp..TotWip  + &lcStyTmp..TotSOH + &lcStyTmp..TotPLA  + &lcStyTmp..TotOTS + &lcStyTmp..TotIOTS + &lcStyTmp..TotBOK + &lcStyTmp..TotSHP  + &lcStyTmp..TotRet + &lcStyTmp..TotRetA + &lcStyTmp..TotAlo + &lcStyTmp..TotUAlo + &lcStyTmp..TotInt + &lcStyTmp..TotWOrd + &lcStyTmp..TotOrd) = 0
       SELECT (lcstytmp)
       DELETE
       lladded = .F.
    ELSE
       IF llOnlyOTS AND !&lcStyTmp..lPrnOTS AND !&lcStyTmp..lPrnIOTS
          SELECT (lcstytmp)
          DELETE
          lladded = .F.
       ELSE
          SELECT (lcstytmp)
          SCATTER MEMVAR FIELDS cstymajor, stycode, warecode, dyelot, season, division, fgroup, sgroup, scale, price, totcost, ave_cost, item_type
          SELECT (lctottmp)
          IF !SEEK('A  '+&lcSortKey+&lcStyTmp..cStyMajor)
             APPEND BLANK
          ENDIF
          GATHER MEMVAR
          REPLACE cendmaj WITH 'A', notscale WITH 'Y'
          = lfupdtotal('Maj',lcstytmp)
          IF  .NOT. (lcrpsortby=='S')
             IF !SEEK('AA '+&lcSortKey)
                APPEND BLANK
             ENDIF
             GATHER MEMVAR
             REPLACE cendmaj WITH 'A', cendsort WITH 'A', notscale WITH 'Y'
             = lfupdtotal('Sor',lcstytmp)
          ENDIF
          IF  .NOT. SEEK('AAA')
             APPEND BLANK
          ENDIF
          GATHER MEMVAR
          REPLACE cendmaj WITH 'A', cendsort WITH 'A', cendrep WITH 'A', notscale WITH 'Y'
          = lfupdtotal('Rep',lcstytmp)
          IF llrpwhdeta .OR. llrpprndye
             SELECT stydye
             SCAN WHILE Style = STYLE.Style FOR &lcRpExp
                llnew = lcstylewar<>stydye.style+stydye.cwarecode
                lcstylewar = stydye.style+stydye.cwarecode
                lnlocno = IIF(llnew, 0, lnlocno)
                IF llrpprndye .AND. llnew
                   llhasdye =  .NOT. EMPTY(dyelot)
                ENDIF
                IF  .NOT. EMPTY(dyelot)
                   LOOP
                ENDIF
                DO lpinsintmp WITH 'STYDYE'
                IF llOnlyOTS AND !&lcTrns..lPrnOTS AND !&lcTrns..lPrnIOTS
                   SELECT (lcstytmp)
                ELSE
                   IF llrpwhdeta .AND. llrpprnloc
                      IF SEEK(stydye.style+SPACE(6)+stydye.cwarecode, 'WHSLOC')
                         SELECT whsloc
                         lcloc = ''
                         SCAN REST FOR  .NOT. EMPTY(clocation) WHILE style+color+cwarecode=stydye.style+SPACE(6)+stydye.cwarecode
                            llhasloc = .T.
                            lcloc = lcloc+IIF(EMPTY(lcloc), 'Bins                 ', SPACE(2))+whsloc.clocation
                         ENDSCAN
                         IF llhasloc
                            SELECT (lctrns)
                            REPLACE hasloc WITH llhasloc, loc WITH lcloc
                         ENDIF
                      ENDIF
                   ENDIF
                ENDIF
             ENDSCAN
          ENDIF
       ENDIF
    ENDIF
    IF lladded
       IF lcscale<>scale.scale
          DO lpinsintmp WITH 'STYLE', .T.
          lcscale = scale.scale
       ENDIF
    ENDIF
 ENDSCAN
 SELECT (lcstytmp)
 APPEND FROM (gcworkdir+lctottmp)
 SELECT style
 SET ORDER TO
 SET RELATION TO
 SELECT stydye
 SET ORDER TO
 SET RELATION TO
 DO lpstylerel
*
PROCEDURE lpinsintmp
 PARAMETER lcfromfile, llscale
 PRIVATE lcfiletous, lccostfld, lcfromfile, lncuralias
 lncuralias = SELECT(0)
 lcfiletous = IIF(lcfromfile='STYDYE', (lctrns), (lcstytmp))
 SELECT (lcfiletous)
 APPEND BLANK
 REPLACE stycode WITH style.style, stydesc WITH style.desc1, cstymajor WITH style.cstymajor, status WITH style.status, scale WITH style.scale, price WITH style.pricea, totcost WITH style.totcost, season WITH style.season, division WITH style.cdivision, fgroup WITH style.fabric, sgroup WITH style.cstygroup
 REPLACE item_type WITH ALLTRIM(gfcoddes(fabric.item_type,'ITEM_TYPE'))
 IF lcfromfile='STYDYE'
    REPLACE warecode WITH stydye.cwarecode, dyelot WITH stydye.dyelot, waredesc WITH stydye.desc, hasdye WITH llhasdye
 ENDIF
 REPLACE AVE_COST  WITH &lcFromFile..AVE_COST, GL_LINK   WITH IIF(lcFromFile='STYLE',STYLE.Link_Code,STYDYE.GL_LINK)
 IF llscale
    REPLACE notscale WITH 'N'
 ELSE
    REPLACE notscale WITH 'Y'
    IF  .NOT. EMPTY(lcsourflds)
       SELECT (lcfromfile)
       SCATTER FIELDS &lcSourFlds. TO laAllVal
       SELECT (lcfiletous)
       GATHER FIELDS &lcTargFlds. FROM laAllVal
    ENDIF
    IF ',UALO,'$UPPER(lcalltrns)
       = lfualocalc()
       SELECT (lcfiletous)
       GATHER FIELDS &lcUALOFlds. FROM laUAloVal
    ENDIF
    IF ',IOTS,'$UPPER(lcalltrns)
       STORE 0 TO laiotsval
       = lfotscalc('IOTS')
       SELECT (lcfiletous)
       GATHER FIELDS &lcIOTSFlds. FROM laIOTSVal
    ENDIF
    IF ',OTS,'$UPPER(lcalltrns)
       STORE 0 TO laotsval
       = lfotscalc('OTS')
       SELECT (lcfiletous)
       GATHER FIELDS &lcOTSFlds. FROM laOTSVal
    ENDIF
    IF ',BOK,'$UPPER(lcalltrns)
       = lfbokcalc()
       SELECT (lcfiletous)
       GATHER FIELDS &lcBokFlds. FROM laBokVal
    ENDIF
    SELECT (lcfiletous)
    IF llshowcost
       IF lcrpsortby='W'
          lccostfld = 'Ave_Cost'
       ELSE
          IF lcfromfile='STYDYE'
             lccostfld = 'Ave_Cost'
          ELSE
             IF  .NOT. llgnglcst .AND. lccstmeth='S'
                lccostfld = 'TotCost'
             ELSE
                lccostfld = 'Ave_Cost'
             ENDIF
          ENDIF
       ENDIF
       laStkVal[01] = TotWip * &lcCostFld.
       IF llgnglcst
          laStkVal[02] = &lcFromFile..nStkVal
       ELSE
          laStkVal[02] = IIF(lcCstMeth = "S" , Style.TotCost * &lcFromFile..Totstk , &lcFromFile..nStkVal) 
       ENDIF
       laStkVal[03] = TotPLA  * &lcCostFld.
       laStkVal[04] = TotOTS  * &lcCostFld.
       laStkVal[05] = TotIOTS * &lcCostFld. 
       laStkVal[06] = TotBOK  * &lcCostFld.
       laStkVal[07] = TotSHP  * &lcCostFld.
       laStkVal[08] = TotRet  * &lcCostFld.
       laStkVal[09] = TotRetA * &lcCostFld. 
       laStkVal[10] = TotAlo  * &lcCostFld.
       laStkVal[11] = TotUAlo * &lcCostFld. 
       laStkVal[12] = TotInt  * &lcCostFld.
       laStkVal[13] = TotWOrd * &lcCostFld. 
       laStkVal[14] = TotOrd  * &lcCostFld.
       GATHER FIELDS &lcStkFlds. From laStkVal
    ENDIF
    IF llshowsale
       lasalval[ 01] = totwip*price
       lasalval[ 02] = totsoh*price
       lasalval[ 03] = totpla*price
       lasalval[ 04] = totots*price
       lasalval[ 05] = totiots*price
       lasalval[ 06] = totbok*price
       lasalval[ 07] = totshp*price
       lasalval[ 08] = totret*price
       lasalval[ 09] = totreta*price
       lasalval[ 10] = totalo*price
       lasalval[ 11] = totualo*price
       lasalval[ 12] = totint*price
       lasalval[ 13] = totword*price
       lasalval[ 14] = totord*price
       GATHER FIELDS &lcSalFlds. From laSalVal
    ENDIF
 ENDIF
 SELECT (lncuralias)
*
PROCEDURE lfotscalc
 PARAMETER lcitm
 PRIVATE lni, lcarray, lcarrayele, lcsz, lcartotele
 lcarray = 'la'+lcitm+'Val'
 lcartotele = lcarray+'[9]'
 STORE 0 TO &lcArray
 FOR lni = 1 TO 8
    lcsz = ALLTRIM(STR(lni))
    lcarrayele = lcarray+'['+lcsz+']'
    &lcArrayEle = EVAL(lcFromFile+'.STK'+lcSz)+ IIF(lcItm='IOTS',0,IIF(lcRPOTSB='W', EVAL(lcFromFile+'.WIP'+lcSz), EVAL('STYLE.Plan'+lcSz)))- EVAL(lcFromFile+'.Ord'+lcSz)
    DO CASE
       CASE lcRPOTSSig = 'P' AND &lcArrayEle <= 0
          &lcArrayEle = 0
       CASE lcRPOTSSig = 'N' AND &lcArrayEle >= 0
          &lcArrayEle = 0
    ENDCASE
    &lcArTotEle = &lcArTotEle + &lcArrayEle
 ENDFOR
 IF (lcRPOTSSig ='P' AND &lcArTotEle >= lnRPOTSMin) OR  (lcRPOTSSig ='N' AND &lcArTotEle <= lnRPOTSMin) OR  (lcRPOTSSig ='A')   
    DO CASE
       CASE lcitm='OTS'
          REPLACE lprnots WITH .T.
       CASE lcitm='IOTS'
          REPLACE lprniots WITH .T.
    ENDCASE
 ENDIF
*
PROCEDURE lfualocalc
 PRIVATE lcsz, lcarrayele
 STORE 0 TO laualoval
 FOR lni = 1 TO 8
    lcsz = ALLTRIM(STR(lni))
    lcarrayele = 'laUaloVal'+'['+lcsz+']'
    &lcArrayEle = EVAL(lcFromFile+".STK"+lcSz)-EVAL(lcFromFile+'.ALO'+lcSz)
    laUAloVal[9] = laUAloVal[9] + &lcArrayEle
 ENDFOR
*
PROCEDURE lfbokcalc
 PRIVATE lcsz, lcarrayele
 STORE 0 TO labokval
 FOR lni = 1 TO 8
    lcsz = ALLTRIM(STR(lni))
    lcarrayele = 'laBokVal'+'['+lcsz+']'
    &lcArrayEle = EVAL(lcFromFile+".Shp"+lcSz)+EVAL(lcFromFile+'.Ord'+lcSz)
    laBokVal[9] = laBokVal[9] + &lcArrayEle
 ENDFOR
*
PROCEDURE lfupdtotal
 PARAMETER lcrectype, lcusefile
 PRIVATE lcrectype, lcusefile
 DO CASE
    CASE lcrectype='Maj'
       lcusedarr = 'laT'
       REPLACE cendmaj WITH 'A'
    CASE lcrectype='Sor'
       lcusedarr = 'laSorT'
       REPLACE cendmaj WITH 'A', cendsort WITH 'A'
    CASE lcrectype='Rep'
       lcusedarr = 'laRepT'
       REPLACE cendmaj WITH 'A', cendsort WITH 'A', cendrep WITH 'A'
 ENDCASE
 REPLACE WIP1      WITH WIP1+&lcUseFile..WIP1 , WIP2      WITH WIP2+&lcUseFile..WIP2 , WIP3      WITH WIP3+&lcUseFile..WIP3 , WIP4      WITH WIP4+&lcUseFile..WIP4 , WIP5      WITH WIP5+&lcUseFile..WIP5 , WIP6      WITH WIP6+&lcUseFile..WIP6 , WIP7      WITH WIP7+&lcUseFile..WIP7 , WIP8      WITH WIP8+&lcUseFile..WIP8 , TOTWIP    WITH TotWip+&lcUseFile..TotWIP , SOH1      WITH Soh1+&lcUseFile..SOH1 , SOH2      WITH Soh2+&lcUseFile..SOH2 , SOH3      WITH Soh3+&lcUseFile..SOH3 , SOH4      WITH Soh4+&lcUseFile..SOH4 , SOH5      WITH Soh5+&lcUseFile..SOH5 , SOH6      WITH Soh6+&lcUseFile..SOH6 , SOH7      WITH Soh7+&lcUseFile..SOH7 , SOH8      WITH Soh8+&lcUseFile..SOH8 , TOTSOH    WITH TotSoh+&lcUseFile..TotSOH 
 REPLACE PLA1      WITH Pla1+&lcUseFile..Pla1 , PLA2      WITH Pla2+&lcUseFile..Pla2 , PLA3      WITH Pla3+&lcUseFile..Pla3 , PLA4      WITH Pla4+&lcUseFile..Pla4 , PLA5      WITH Pla5+&lcUseFile..Pla5 , PLA6      WITH Pla6+&lcUseFile..Pla6 , PLA7      WITH Pla7+&lcUseFile..Pla7 , PLA8      WITH Pla8+&lcUseFile..Pla8 , TOTPLA    WITH TotPla+&lcUseFile..TotPla , UALO1     WITH UAlo1+&lcUseFile..UAlo1 , UALO2     WITH UAlo2+&lcUseFile..UAlo2 , UALO3     WITH UAlo3+&lcUseFile..UAlo3 , UALO4     WITH UAlo4+&lcUseFile..UAlo4 , UALO5     WITH UAlo5+&lcUseFile..UAlo5 , UALO6     WITH UAlo6+&lcUseFile..UAlo6 , UALO7     WITH UAlo7+&lcUseFile..UAlo7 , UALO8     WITH UAlo8+&lcUseFile..UAlo8 , TOTUALO   WITH TotUAlo+&lcUseFile..TotUAlo 
 REPLACE OTS1      WITH OTS1+&lcUseFile..OTS1 , OTS2      WITH OTS2+&lcUseFile..OTS2 , OTS3      WITH OTS3+&lcUseFile..OTS3 , OTS4      WITH OTS4+&lcUseFile..OTS4 , OTS5      WITH OTS5+&lcUseFile..OTS5 , OTS6      WITH OTS6+&lcUseFile..OTS6 , OTS7      WITH OTS7+&lcUseFile..OTS7 , OTS8      WITH OTS8+&lcUseFile..OTS8 , TOTOTS    WITH TotOTS+&lcUseFile..TotOTS , IOTS1     WITH IOTS1+&lcUseFile..IOTS1 , IOTS2     WITH IOTS2+&lcUseFile..IOTS2 , IOTS3     WITH IOTS3+&lcUseFile..IOTS3 , IOTS4     WITH IOTS4+&lcUseFile..IOTS4 , IOTS5     WITH IOTS5+&lcUseFile..IOTS5 , IOTS6     WITH IOTS6+&lcUseFile..IOTS6 , IOTS7     WITH IOTS7+&lcUseFile..IOTS7 , IOTS8     WITH IOTS8+&lcUseFile..IOTS8 , TOTIOTS   WITH TotIOTS+&lcUseFile..TotIOTS 
 REPLACE ORD1      WITH Ord1+&lcUseFile..Ord1 , ORD2      WITH Ord2+&lcUseFile..Ord2 , ORD3      WITH Ord3+&lcUseFile..Ord3 , ORD4      WITH Ord4+&lcUseFile..Ord4 , ORD5      WITH Ord5+&lcUseFile..Ord5 , ORD6      WITH Ord6+&lcUseFile..Ord6 , ORD7      WITH Ord7+&lcUseFile..Ord7 , ORD8      WITH Ord8+&lcUseFile..Ord8 , TOTORD    WITH TotOrd+&lcUseFile..TotOrd , WORD1     WITH WOrd1+&lcUseFile..WOrd1 , WORD2     WITH WOrd2+&lcUseFile..WOrd2 , WORD3     WITH WOrd3+&lcUseFile..WOrd3 , WORD4     WITH WOrd4+&lcUseFile..WOrd4 , WORD5     WITH WOrd5+&lcUseFile..WOrd5 , WORD6     WITH WOrd6+&lcUseFile..WOrd6 , WORD7     WITH WOrd7+&lcUseFile..WOrd7 , WORD8     WITH WOrd8+&lcUseFile..WOrd8 , TOTWORD   WITH TotWOrd+&lcUseFile..TotWOrd 
 REPLACE INT1      WITH Int1+&lcUseFile..Int1 , INT2      WITH Int2+&lcUseFile..Int2 , INT3      WITH Int3+&lcUseFile..Int3 , INT4      WITH Int4+&lcUseFile..Int4 , INT5      WITH Int5+&lcUseFile..Int5 , INT6      WITH Int6+&lcUseFile..Int6 , INT7      WITH Int7+&lcUseFile..Int7 , INT8      WITH Int8+&lcUseFile..Int8 , TOTINT    WITH TotInt+&lcUseFile..TotInt , BOK1      WITH Bok1+&lcUseFile..Bok1 , BOK2      WITH Bok2+&lcUseFile..Bok2 , BOK3      WITH Bok3+&lcUseFile..Bok3 , BOK4      WITH Bok4+&lcUseFile..Bok4 , BOK5      WITH Bok5+&lcUseFile..Bok5 , BOK6      WITH Bok6+&lcUseFile..Bok6 , BOK7      WITH Bok7+&lcUseFile..Bok7 , BOK8      WITH Bok8+&lcUseFile..Bok8 , TOTBOK    WITH TotBok+&lcUseFile..TotBok 
 REPLACE SHP1      WITH Shp1+&lcUseFile..Shp1 , SHP2      WITH Shp2+&lcUseFile..Shp2 , SHP3      WITH Shp3+&lcUseFile..Shp3 , SHP4      WITH Shp4+&lcUseFile..Shp4 , SHP5      WITH Shp5+&lcUseFile..Shp5 , SHP6      WITH Shp6+&lcUseFile..Shp6 , SHP7      WITH Shp7+&lcUseFile..Shp7 , SHP8      WITH Shp8+&lcUseFile..Shp8 , TOTSHP    WITH TotShp+&lcUseFile..TotShp , ALO1      WITH Alo1+&lcUseFile..Alo1 , ALO2      WITH Alo2+&lcUseFile..Alo2 , ALO3      WITH Alo3+&lcUseFile..Alo3 , ALO4      WITH Alo4+&lcUseFile..Alo4 , ALO5      WITH Alo5+&lcUseFile..Alo5 , ALO6      WITH Alo6+&lcUseFile..Alo6 , ALO7      WITH Alo7+&lcUseFile..Alo7 , ALO8      WITH Alo8+&lcUseFile..Alo8 , TOTALO    WITH TotAlo+&lcUseFile..TotAlo 
 REPLACE RET1      WITH Ret1+&lcUseFile..Ret1 , RET2      WITH Ret2+&lcUseFile..Ret2 , RET3      WITH Ret3+&lcUseFile..Ret3 , RET4      WITH Ret4+&lcUseFile..Ret4 , RET5      WITH Ret5+&lcUseFile..Ret5 , RET6      WITH Ret6+&lcUseFile..Ret6 , RET7      WITH Ret7+&lcUseFile..Ret7 , RET8      WITH Ret8+&lcUseFile..Ret8 , TOTRET    WITH TotRet+&lcUseFile..TotRet , RETA1     WITH RetA1+&lcUseFile..RetA1 , RETA2     WITH RetA2+&lcUseFile..RetA2 , RETA3     WITH RetA3+&lcUseFile..RetA3 , RETA4     WITH RetA4+&lcUseFile..RetA4 , RETA5     WITH RetA5+&lcUseFile..RetA5 , RETA6     WITH RetA6+&lcUseFile..RetA6 , RETA7     WITH RetA7+&lcUseFile..RetA7 , RETA8     WITH RetA8+&lcUseFile..RetA8 , TOTRETA   WITH TotRetA+&lcUseFile..TotRetA 
 REPLACE nStkVWIP    WITH nStkVWIP +&lcUseFile..nStkVWIP , nStkVSOH    WITH nStkVSoh +&lcUseFile..nStkVSOH , nStkVPLA    WITH nStkVPla +&lcUseFile..nStkVPla , nStkVUALO   WITH nStkVUAlo+&lcUseFile..nStkVUAlo, nStkVOTS    WITH nStkVOTS +&lcUseFile..nStkVOTS , nStkVIOTS   WITH nStkVIOTS+&lcUseFile..nStkVIOTS, nStkVORD    WITH nStkVOrd +&lcUseFile..nStkVOrd , nStkVWORD   WITH nStkVWOrd+&lcUseFile..nStkVWOrd, nStkVINT    WITH nStkVInt +&lcUseFile..nStkVInt , nStkVBOK    WITH nStkVBok +&lcUseFile..nStkVBok , nStkVSHP    WITH nStkVShp +&lcUseFile..nStkVShp , nStkVALO    WITH nStkVAlo +&lcUseFile..nStkVAlo , nStkVRET    WITH nStkVRet +&lcUseFile..nStkVRet , nStkVRETA   WITH nStkVRetA+&lcUseFile..nStkVRetA
 REPLACE nSalVWIP    WITH nSalVWIP +&lcUseFile..nSalVWIP , nSalVSOH    WITH nSalVSoh +&lcUseFile..nSalVSOH , nSalVPLA    WITH nSalVPla +&lcUseFile..nSalVPla , nSalVUALO   WITH nSalVUAlo+&lcUseFile..nSalVUAlo, nSalVOTS    WITH nSalVOTS +&lcUseFile..nSalVOTS , nSalVIOTS   WITH nSalVIOTS+&lcUseFile..nSalVIOTS, nSalVORD    WITH nSalVOrd +&lcUseFile..nSalVOrd , nSalVWORD   WITH nSalVWOrd+&lcUseFile..nSalVWOrd, nSalVINT    WITH nSalVInt +&lcUseFile..nSalVInt , nSalVBOK    WITH nSalVBok +&lcUseFile..nSalVBok , nSalVSHP    WITH nSalVShp +&lcUseFile..nSalVShp , nSalVALO    WITH nSalVAlo +&lcUseFile..nSalVAlo , nSalVRET    WITH nSalVRet +&lcUseFile..nSalVRet , nSalVRETA   WITH nSalVRetA+&lcUseFile..nSalVRetA
*
PROCEDURE lpwarerela
 SELECT (lcdummy)
 SET ORDER TO (lcdummy)
 SELECT (lctrns)
 SET ORDER TO WARESORT
 SET RELATION TO IIF(notscale='Y', '', SPACE(10)) INTO (lcdummy) ADDITIVE
 SELECT stydye
 SET ORDER TO STYDYEW ASCENDING
 SELECT (lcdummy)
 SET RELATION TO IIF(&lcDummy..Label='Dyelot' AND &lcTrns..NotScale='Y', &lcTrns..WareCode+&lcTrns..StyCode , SPACE(25)) INTO STYDYE ADDITIVE
 SELECT (lctrns)
 SET SKIP TO (lcdummy), stydye
 SET RELATION TO 'S'+scale INTO scale ADDITIVE
 SET RELATION TO warecode INTO warehous ADDITIVE
 SET ORDER TO STYLE IN style
 SET RELATION TO stycode INTO style ADDITIVE
*
PROCEDURE lpstylerel
 SET ORDER TO StySort IN (lctrns)
 SELECT (lcstytmp)
 DO CASE
    CASE lcrpsortby=='S'
       lcsortttl = 'Style'
       lcgroupexp = "''"
       lcsortexp = "''"
       lcmajexp = "lcMajTtl + SPACE(1) + PADL(': ',2+MAX(MAX(LEN('Description'),LEN(ALLTRIM(lcRepNMTtl)))-LEN(lcMajTtl),0))"
       lcnmajexp = [ALLTRIM(lcRepNMTtl) + SPACE(1) + PADL(": ",2+MAX(MAX(LEN('Description'),LEN(lcMajTtl))-LEN(ALLTRIM(lcRepNMTtl)),0))]
       lcdescexp = "'Description'      + SPACE(1) + PADL(': ',2+MAX(MAX(LEN(lcMajTtl),LEN(ALLTRIM(lcRepNMTtl)))-LEN('Description'),0))"
    CASE lcrpsortby=='SE'
       lcsortfld = '.Season'
       lcsortttl = 'Season'
       lcgroupexp = lcstytmp+'.Season'
       lcsortexp = ['Season'+SPACE(1)+PADL(": ",2+MAX(LEN(lcMajTtl)-LEN('Season'),0))]
       lcmajexp = "lcMajTtl + SPACE(1) + PADL(': ',2+MAX(LEN('Season')-LEN(lcMajTtl),0))"
       lcnmajexp = [ALLTRIM(lcRepNMTtl)+SPACE(1)+ PADL(": ",2+MAX(LEN('Description')-LEN(ALLTRIM(lcRepNMTtl)),0)) ]
       lcdescexp = "'Description'+SPACE(1)+PADL(': ',2+MAX(LEN(ALLTRIM(lcRepNMTtl))-LEN('Description'),0))"
    CASE lcrpsortby=='D'
       lcsortfld = '.Division'
       lcsortttl = 'Division'
       lcgroupexp = lcstytmp+'.Division'
       lcsortexp = ['Division'+SPACE(1)+PADL(": ",2+MAX(LEN(lcMajTtl)-LEN('Division'),0))]
       lcmajexp = "lcMajTtl + SPACE(1) + PADL(': ',2+MAX(LEN('Division')-LEN(lcMajTtl),0))"
       lcnmajexp = [ALLTRIM(lcRepNMTtl)+SPACE(1)+ PADL(": ",2+MAX(LEN('Description')-LEN(ALLTRIM(lcRepNMTtl)),0))]
       lcdescexp = "'Description'+SPACE(1)+PADL(': ',2+MAX(LEN(ALLTRIM(lcRepNMTtl))-LEN('Description'),0))"
    CASE lcrpsortby=='FG'
       lcsortfld = '.FGroup'
       lcsortttl = 'Primary Fabric'
       lcgroupexp = lcstytmp+'.FGroup'
       lcsortexp = ['Primary Fabric'+SPACE(1)+PADL(": ",2+MAX(LEN(lcMajTtl)-LEN('Primary Fabric'),0))]
       lcmajexp = "lcMajTtl + SPACE(1) + PADL(': ',2+MAX(LEN('Primary Fabric')-LEN(lcMajTtl),0))"
       lcmajexp = "lcMajTtl + SPACE(1) + PADL(': ',2+MAX(LEN('Primary Fabric')-LEN(lcMajTtl),0))"
       lcnmajexp = [ALLTRIM(lcRepNMTtl)+SPACE(1)+ PADL(": ",2+MAX(LEN('Description')-LEN(ALLTRIM(lcRepNMTtl)),0))]
       lcdescexp = "'Description'+SPACE(1)+PADL(': ',2+MAX(LEN(ALLTRIM(lcRepNMTtl))-LEN('Description'),0))"
    CASE lcrpsortby=='MT'
       lcsortfld = '.ITEM_TYPE'
       lcsortttl = 'Material Type'
       lcgroupexp = lcstytmp+'.ITEM_TYPE'
       lcsortexp = ['Material Type'+SPACE(1)+PADL(": ",2+MAX(LEN(lcMajTtl)-LEN('Material Type'),0))]
       lcmajexp = "lcMajTtl + SPACE(1) + PADL(': ',2+MAX(LEN('Material Type')-LEN(lcMajTtl),0))"
       lcmajexp = "lcMajTtl + SPACE(1) + PADL(': ',2+MAX(LEN('Material Type')-LEN(lcMajTtl),0))"
       lcnmajexp = [ALLTRIM(lcRepNMTtl)+SPACE(1)+ PADL(": ",2+MAX(LEN('Description')-LEN(ALLTRIM(lcRepNMTtl)),0))]
       lcdescexp = "'Description'+SPACE(1)+PADL(': ',2+MAX(LEN(ALLTRIM(lcRepNMTtl))-LEN('Description'),0))"
    CASE lcrpsortby=='SG'
       lcsortfld = '.SGroup'
       lcsortttl = 'Style Group'
       lcgroupexp = lcstytmp+'.SGroup'
       lcsortexp = ['Style Group'+SPACE(1)+PADL(": ",2+MAX(LEN(lcMajTtl)-LEN('Style Group'),0))]
       lcmajexp = "lcMajTtl + SPACE(1) + PADL(': ',2+MAX(LEN('Style Group')-LEN(lcMajTtl),0))"
       lcnmajexp = [ALLTRIM(lcRepNMTtl)+SPACE(1)+ PADL(": ",2+MAX(LEN('Description')-LEN(ALLTRIM(lcRepNMTtl)),0))]
       lcdescexp = "'Description'+SPACE(1)+PADL(': ',2+MAX(LEN(ALLTRIM(lcRepNMTtl))-LEN('Description'),0))"
 ENDCASE
 SET ORDER TO (lcdummy) IN (lcdummy)
 SET ORDER TO STYDYE IN stydye ASCENDING
 SELECT (lcstytmp)
 SET RELATION TO IIF(notscale='Y', '', SPACE(10)) INTO (lcdummy) ADDITIVE
 SELECT (lcdummy)
 SET RELATION TO IIF(&lcStyTmp..NotScale='Y',&lcStyTmp..StyCode,SPACE(10)) INTO (lcTrns) ADDITIVE
 SELECT (lctrns)
 SET RELATION TO IIF(&lcStyTmp..NotScale='Y',StyCode+WareCode,SPACE(10)) INTO STYDYE ADDITIVE
 SELECT (lcstytmp)
 SET SKIP TO (lcdummy), (lctrns), stydye
 SET RELATION TO 'S'+scale INTO scale ADDITIVE
 SET ORDER TO STYLE IN style
 SET RELATION TO stycode INTO style ADDITIVE
*
FUNCTION lflastitem
 IF notscale='Y'
    lcLastItem = &lcDummy..Label
    lclaststy = stycode
 ENDIF
 RETURN ''
*
FUNCTION lflaststy
 IF notscale='Y'
    lcLastSty = &lcTrns..StyCode
 ENDIF
 RETURN ''
*
FUNCTION lflsttoitm
 IF notscale='Y'
    lcLstToItm = &lcDummy..Item
 ENDIF
 RETURN ''
*
FUNCTION lfprnstyit
 PARAMETER lcfld
 PRIVATE lcfld, lnret, lcusefile
 lcusefile = IIF(lcrpsortby='W', lctrns, lcstytmp)
 lnret = ''
 IF notscale='Y'
    IF Scale.Cnt >= VAL(lcFld) OR !EMPTY(&lcUseFile..cEndMaj+&lcUseFile..cEndSort+&lcUseFile..cEndRep)
       IF !EMPTY(&lcDummy..Item)
          IF (lcRPSortBy = "W" AND &lcDummy..Label <> 'Dyelot' AND EMPTY(&lcUseFile..Dyelot)) OR  lcRPSortBy <> "W"
             IF lcfld='ItmLabel'
                IF !(ALLTRIM(&lcDummy..Item) $ 'IOTS')
                   lnRet = &lcDummy..Label
                ELSE
                   IF (ALLTRIM(&lcDummy..Item) = 'IOTS' AND lPrnIOTS) OR  (ALLTRIM(&lcDummy..Item) = 'OTS' AND lPrnOTS)   OR  !EMPTY(&lcUseFile..cEndMaj)
                      lnRet = &lcDummy..Label
                   ELSE
                      lnret = ''
                   ENDIF
                ENDIF
             ELSE
                IF lcfld='WhsLabel'
                   IF llrpwhdeta .AND. EMPTY(cendmaj)
                      IF !(ALLTRIM(&lcDummy..Item) $ 'IOTS')
                         lnret = 'Location '
                      ELSE
                         IF (ALLTRIM(&lcDummy..Item) = 'IOTS' AND lPrnIOTS) OR  (ALLTRIM(&lcDummy..Item) = 'OTS' AND lPrnOTS)   OR  !EMPTY(&lcUseFile..cEndMaj)
                            lnret = 'Location '
                         ELSE
                            lnret = ''
                         ENDIF
                      ENDIF
                   ELSE
                      lnret = ''
                   ENDIF
                ELSE
                   IF  .NOT. INLIST(lcfld, 'Price', 'Cost')
                      lnRet = EVAL(lcUseFile+"."+IIF(lcFld='Tot','Tot'+ALLTRIM(&lcDummy..Item), ALLTRIM(&lcDummy..Item)+lcFld))
                      IF ALLTRIM(&lcDummy..Item) $ 'IOTS' AND EMPTY(cEndMaj)
                         IF !( (ALLTRIM(&lcDummy..Item) = 'IOTS' AND lPrnIOTS) OR  (ALLTRIM(&lcDummy..Item) = 'OTS' AND lPrnOTS) )
                            lnret = ''
                         ELSE
                            IF BETWEEN(lcfld, '1', '8')
                               DO CASE
                                  CASE lcrpotssig='P' .AND. lnret<=0
                                     lnret = ''
                                  CASE lcrpotssig='N' .AND. lnret>=0
                                     lnret = ''
                               ENDCASE
                            ENDIF
                         ENDIF
                      ENDIF
                   ELSE
                      IF lcfld='Cost'
                         IF !EMPTY(cEndMaj) OR  !( (ALLTRIM(&lcDummy..Item) = 'IOTS' AND !lPrnIOTS) OR  (ALLTRIM(&lcDummy..Item) = 'OTS'  AND !lPrnOTS ) )
                            lnRet = EVAL('nStkV'+ALLTRIM(&lcDummy..Item))
                         ELSE
                            lnret = ''
                         ENDIF
                      ENDIF
                      IF lcfld='Price'
                         IF !EMPTY(cEndMaj) OR  !( (ALLTRIM(&lcDummy..Item) = 'IOTS' AND !lPrnIOTS) OR  (ALLTRIM(&lcDummy..Item) = 'OTS'  AND !lPrnOTS ) )
                            lnRet = EVAL('nSalV'+ALLTRIM(&lcDummy..Item))
                         ELSE
                            lnret = ''
                         ENDIF
                      ENDIF
                   ENDIF
                ENDIF
             ENDIF
          ENDIF
       ENDIF
    ENDIF
 ENDIF
 RETURN lnret
*
FUNCTION lfprnwhsit
 PARAMETER lcfld
 PRIVATE lcfld, lnret
 lnret = ''
 IF notscale='Y'
    IF llRPWhDeta AND EMPTY(STYDYE.Dyelot) AND EMPTY(&lcStyTmp..cEndMaj) AND !EMPTY(&lcDummy..Item)
       IF lcfld='WhsCode'
          IF !(ALLTRIM(&lcDummy..Item) $ 'IOTS')
             lnRet = &lcTrns..WareCode
          ELSE
             IF (ALLTRIM(&lcDummy..Item) = 'IOTS' AND &lcTrns..lPrnIOTS) OR  (ALLTRIM(&lcDummy..Item) = 'OTS'  AND &lcTrns..lPrnOTS) 
                lnRet = &lcTrns..WareCode
             ELSE
                lnret = ''
             ENDIF
          ENDIF
       ELSE
          IF  .NOT. INLIST(lcfld, 'Price', 'Cost')
             lnRet = EVAL(lcTrns+'.'+IIF(lcFld='Tot','Tot'+ALLTRIM(&lcDummy..Item), ALLTRIM(&lcDummy..Item)+lcFld))
             IF ALLTRIM(&lcDummy..Item) $ 'IOTS'
                IF !( (ALLTRIM(&lcDummy..Item) = 'IOTS' AND &lcTrns..lPrnIOTS) OR  (ALLTRIM(&lcDummy..Item) = 'OTS'  AND &lcTrns..lPrnOTS) )
                   lnret = ''
                ELSE
                   IF BETWEEN(lcfld, '1', '8')
                      DO CASE
                         CASE lcrpotssig='P' .AND. lnret<=0
                            lnret = ''
                         CASE lcrpotssig='N' .AND. lnret>=0
                            lnret = ''
                      ENDCASE
                   ENDIF
                ENDIF
             ENDIF
          ELSE
             IF lcfld='Cost'
                IF !EMPTY(&lcStyTmp..cEndMaj) OR  !( (ALLTRIM(&lcDummy..Item) = 'IOTS' AND !&lcTrns..lPrnIOTS) OR  (ALLTRIM(&lcDummy..Item) = 'OTS'  AND !&lcTrns..lPrnOTS ) )
                   lnRet = EVAL(lcTrns+'.nStkV'+ALLTRIM(&lcDummy..Item))
                ELSE
                   lnret = ''
                ENDIF
             ENDIF
             IF lcfld='Price'
                IF !EMPTY(&lcStyTmp..cEndMaj) OR  !( (ALLTRIM(&lcDummy..Item) = 'IOTS' AND !&lcTrns..lPrnIOTS) OR  (ALLTRIM(&lcDummy..Item) = 'OTS'  AND !&lcTrns..lPrnOTS ) )
                   lnRet = EVAL(lcTrns+'.nSalV'+ALLTRIM(&lcDummy..Item))
                ELSE
                   lnret = ''
                ENDIF
             ENDIF
          ENDIF
       ENDIF
    ELSE
       lnret = ''
    ENDIF
 ENDIF
 RETURN lnret
*
FUNCTION lfdyltitm
 PARAMETER lcfld
 PRIVATE lcfld, lnretval
 lnretval = 0
 DO CASE
    CASE lcfld='Tot'
       lnretval = stydye.totstk
    CASE lcfld='nStkVal'
       lnretval = stydye.nstkval
    CASE lcfld='Price'
       lnretval = stydye.totstk*style.pricea
    OTHERWISE
       lnretval = EVALUATE('StyDye.Stk'+lcfld)
 ENDCASE
 RETURN lnretval
*
FUNCTION lfprnloc
 IF notscale='Y'
    IF (llrpwhdeta .OR. lcrpsortby='W') .AND. llrpprnloc .AND. EVALUATE(lcdummy+'.Label')<>'Dyelot' .AND. EMPTY(stydye.dyelot)
       IF ALLTRIM(&lcDummy..Item) $ 'IOTS'
          DO CASE
             CASE lcrpotssig='P'
                lcRet = IIF(!llRPPrnLoc,'', IIF(ALLTRIM(&lcDummy..Item) $ 'IOTS', IIF(EVAL(lcStyTmp+'.Tot'+ALLTRIM(&lcDummy..Item))>=lnRPOTSMin,&lcTrns..Loc,'') ,&lcTrns..Loc))
             CASE lcrpotssig='N'
                lcRet = IIF(!llRPWhDeta,'', IIF(ALLTRIM(&lcDummy..Item) $ 'IOTS', IIF(EVAL(lcStyTmp+'.Tot'+ALLTRIM(&lcDummy..Item))<=lnRPOTSMin,&lcTrns..Loc,'') ,&lcTrns..Loc))
          ENDCASE
       ELSE
          lcRet = &lcTrns..Loc
       ENDIF
    ELSE
       lcret = ''
    ENDIF
 ELSE
    lcret = ''
 ENDIF
 RETURN ALLTRIM(lcret)
*
FUNCTION lfscale
 IF NotScale = 'N' AND IIF(lcRPSortBy = 'W' ,  &lcTrns..SCALE #  lcScale1 , &lcStyTmp..SCALE #  lcScale1 )
    IF lltextmode
       lcret = SPACE(15)+PADL(ALLTRIM(scale.sz1), 8)+SPACE(1)+PADL(ALLTRIM(scale.sz2), 8)+SPACE(1)+PADL(ALLTRIM(scale.sz3), 8)+SPACE(1)+PADL(ALLTRIM(scale.sz4), 8)+SPACE(1)+PADL(ALLTRIM(scale.sz5), 8)+SPACE(1)+PADL(ALLTRIM(scale.sz6), 8)+SPACE(1)+PADL(ALLTRIM(scale.sz7), 8)+SPACE(1)+PADL(ALLTRIM(scale.sz8), 8)+SPACE(1)+PADL('Total', 8)
       IF llshowcost
          lcret = lcret+SPACE(1)+' Cost Val.'
       ENDIF
       IF llshowsale
          lcret = lcret+SPACE(1)+'Sales Val.'
       ENDIF
    ELSE
       lcret = SPACE(15)+PADL(ALLTRIM(scale.sz1), 8)+SPACE(1)+PADL(ALLTRIM(scale.sz2), 8)+SPACE(1)+PADL(ALLTRIM(scale.sz3), 8)+SPACE(1)+PADL(ALLTRIM(scale.sz4), 8)+SPACE(1)+PADL(ALLTRIM(scale.sz5), 8)+SPACE(1)+PADL(ALLTRIM(scale.sz6), 8)+SPACE(1)+PADL(ALLTRIM(scale.sz7), 8)+SPACE(1)+PADL(ALLTRIM(scale.sz8), 8)+SPACE(1)+PADL('Total', 8)
       FOR i = 1 TO 8
          z = STR(i, 1)
          laScals[I] = PADL(ALLTRIM(Scale.Sz&z),5)
       ENDFOR
       IF llshowcost
          lcret = lcret+SPACE(1)+' Cost Val.'
       ENDIF
       IF llshowsale
          lcret = lcret+SPACE(10)+'Sales Val.'
       ENDIF
    ENDIF
    lcscale1 = style.scale
 ELSE
    lcret = ''
 ENDIF
 RETURN lcret
*
FUNCTION lftotttl
 PRIVATE lcret
 IF lcrpsortby='W'
    IF notscale='N'
       lcret = ''
    ELSE
       IF EMPTY(cendmaj)
          IF lclaststy=EVALUATE(lctrns+'.StyCode')
             lcret = ''
          ELSE
             lcret = lfprnloc()
          ENDIF
       ELSE
          IF EVALUATE(lcdummy+'.cRecNo')<>'01' .OR. EMPTY(EVALUATE(lcdummy+'.Item'))
             lcret = ''
          ELSE
             IF EMPTY(cendsort)
                lcret = lcmajttl+' Total : '
             ELSE
                IF EMPTY(cendrep)
                   lcret = 'Warehouse Total : '
                ELSE
                   lcret = 'Grand Total : '
                ENDIF
             ENDIF
          ENDIF
       ENDIF
    ENDIF
 ELSE
    IF notscale='N'
       lcret = ''
    ELSE
       IF EMPTY(cendmaj) .OR. EMPTY(EVALUATE(lcdummy+'.Item')) .OR. EVALUATE(lcdummy+'.Label')=lclastitem
          lcret = ''
       ELSE
          IF EVALUATE(lcdummy+'.cRecNo')<>'01'
             lcret = ''
          ELSE
             IF EMPTY(cendsort)
                lcret = lcmajttl+' Total : '
             ELSE
                IF EMPTY(cendrep)
                   lcret = lcsortttl+' Total : '
                ELSE
                   lcret = 'Grand Total : '
                ENDIF
             ENDIF
          ENDIF
       ENDIF
    ENDIF
 ENDIF
 RETURN lcret
*
PROCEDURE lfswonetrn
 lcscalcode = ' '
 DIMENSION latranaray[ 8], latrnnonar[ 8]
 STORE '' TO lcmastfile, lcchldfile, lccost_val, latranaray, lcreptarvl, lcprntitle, lcrevfile, lccst_vlpn, lccst_vlop, lccst_vlgd, lcnonmjdes, lcextndflt, latrnnonar, lccst_vlcl, lclocbins, lcprintsty, lccst_vllc
 STORE '' TO lccst_vlc1, lccst_vlc2, lccst_vlp1, lccst_vlp2, lccst_vlo1, lccst_vlo2, lccst_vlg1, lccst_vlg2
 STORE '""' TO lcstygroup, lclocgroup, lcdyegroup
 STORE 0 TO lnendofsty, lnendofloc, lnendofdye, lnsize1, lnsize2, lnsize3, lnsize4, lnsize5, lnsize6, lnsize7, lnsize8, lnsize9, lnmaxcnt, lnnonprice, lnnoncost
 STORE 0 TO lnstysz1, lnstysz2, lnstysz3, lnstysz4, lnstysz5, lnstysz6, lnstysz7, lnstysz8, lnstysz9, lngrdsz1, lngrdsz2, lngrdsz3, lngrdsz4, lngrdsz5, lngrdsz6, lngrdsz7, lngrdsz8, lngrdsz9, lnnonprcop, lnnoncstop, lnnonprcgd, lnnoncstgd
 STORE 0 TO lnlocsz1, lnlocsz2, lnlocsz3, lnlocsz4, lnlocsz5, lnlocsz6, lnlocsz7, lnlocsz8, lnlocsz9, lnnonprclc, lnnoncstlc
 STORE 0 TO lnclrsz1, lnclrsz2, lnclrsz3, lnclrsz4, lnclrsz5, lnclrsz6, lnclrsz7, lnclrsz8, lnclrsz9, lnclrprice, lnclrcost, lncstavevl
 llprnclrln = .F.
 = lftranaray()
 IF EMPTY(latranaray(1))
    RETURN
 ENDIF
 lnmajorlen = LEN(gfitemmask('PM'))
 lcprntloc = SPACE(6)
 lcprntsty = SPACE(lnmajorlen)
 lcprntnon = SPACE(19-lnmajorlen)
 USE IN 0 (gcdatadir+'STYDYE.DBF') AGAIN ALIAS revstydy
 USE IN 0 (gcdatadir+'STYLE.DBF') AGAIN ALIAS revstyle
 SET ORDER TO STYLE IN style
 SET ORDER TO STYLE IN revstyle
 lcstygroup = 'PADR(STYDYE.STYLE,lnMajorLen)'
 IF lcrpsortby='S'
    SET ORDER TO STYDYE IN stydye
    SET ORDER TO STYDYE IN revstydy
    llprintclr = llrpwhdeta .OR. llrpprndye
    llschkdyeb = .F.
    IF llprintclr
       llschkdyeb = .T.
       lcmastfile = 'STYDYE'
       lcchldfile = 'STYLE'
       lcrevfile = 'REVSTYDY'
       IF llrpwhdeta .AND.  .NOT. llrpprndye
          lcextndflt = 'EMPTY(Dyelot)'
       ENDIF
    ELSE
       lcmastfile = 'STYLE'
       lcchldfile = 'STYDYE'
       lcrevfile = 'REVSTYLE'
    ENDIF
 ELSE
    llschkdyeb = .T.
    llprintclr = .F.
    SET ORDER TO STYDYEW IN stydye
    SET ORDER TO STYDYEW IN revstydy
    lcmastfile = 'STYDYE'
    lcchldfile = 'STYLE'
    lcrevfile = 'REVSTYDY'
    lclocgroup = 'STYDYE.CWARECODE'
 ENDIF
 IF  .NOT. llgnglcst .AND. lccstmeth='S'
    lclinecost = 'Style.TotCost'
    lcstycost = 'Style.TotCost'
 ELSE
    lclinecost = 'Ave_Cost'
    lcstycost = 'Style.Ave_Cost'
 ENDIF
 DO CASE
    CASE lcrpshow='S'
       lccost_val = IIF(lcrepmode='Text', ' Unit_Price'+' Sales_Value', SPACE(3)+' Unit_Price'+SPACE(5)+' Sales_Value')
       IF lcrepmode='Text'
          lccst_vlpn = "IIF(lnSize9=0 AND !llRpShwZer,'',' ' + TRANSFORM(STYLE.PRICEA,'999999999.99') +                 ' ' + TRANSFORM(lnNonPrice,'999999999.99'))"
       ELSE
          lccst_vlp1 = "IIF(lnSize9=0 AND !llRpShwZer,'',TRANSFORM(STYLE.PRICEA,'999999999.99') )"
          lccst_vlp2 = "IIF(lnSize9=0 AND !llRpShwZer,'',TRANSFORM(lnNonPrice,'999999999.99'))"
       ENDIF
       IF lcrepmode='Text'
          lccst_vlcl = "IIF(lnSize9=0 AND !llRpShwZer,'',' ' + TRANSFORM(STYLE.PRICEA,'999999999.99') +                 ' ' + TRANSFORM(lnClrPrice,'999999999.99'))"
       ELSE
          lccst_vlc1 = "IIF(lnSize9=0 AND !llRpShwZer,'',TRANSFORM(STYLE.PRICEA,'999999999.99') )"
          lccst_vlc2 = "IIF(lnSize9=0 AND !llRpShwZer,'',TRANSFORM(lnClrPrice,'999999999.99'))"
       ENDIF
       lccst_vllc = "' ' + SPACE(10) +' ' + TRANSFORM(lnNonPrcLc,'999999999.99')"
       IF lcrepmode='Text'
          lccst_vlop = "' ' + SPACE(10) +' ' + TRANSFORM(lnNonPrcOp,'999999999.99')"
       ELSE
          lccst_vlo1 = "' '"
          lccst_vlo2 = "TRANSFORM(lnNonPrcOp,'999999999.99')"
       ENDIF
       IF lcrepmode='Text'
          lccst_vlgd = "' ' + SPACE(10) +' ' + TRANSFORM(lnNonPrcGd,'999999999.99')"
       ELSE
          lccst_vlg1 = "' '"
          lccst_vlg2 = "TRANSFORM(lnNonPrcGd,'999999999.99')"
       ENDIF
    CASE lcrpshow='C'
       IF  .NOT. llgnglcst .AND. lccstmeth='S'
          lccost_val = IIF(lcrepmode='Text', '  Unit_Cost'+'  Cost_Value', SPACE(2)+'  Unit_Cost'+SPACE(5)+'  Cost_Value')
       ELSE
          lccost_val = IIF(lcrepmode='Text', '  Avrg_Cost'+'  Cost_Value', SPACE(2)+'  Avrg_Cost'+SPACE(5)+'  Cost_Value')
       ENDIF
       IF lcrepmode='Text'
          lccst_vlpn = "IIF(lnSize9=0 AND !llRpShwZer,'',' ' +                 TRANSFORM(IIF(lcMastFile = 'STYLE' OR EMPTY(DYELOT),EVALUATE(lcLineCost),lnCstAveVl)  ,'9999999.99') +                 ' ' + TRANSFORM(lnNonCost,'99999999.99'))"
       ELSE
          lccst_vlp1 = "IIF(lnSize9=0 AND !llRpShwZer,'',                   TRANSFORM(IIF(lcMastFile = 'STYLE' OR EMPTY(DYELOT),EVALUATE(lcLineCost),lnCstAveVl)  ,'9999999.99') )"
          lccst_vlp2 = "IIF(lnSize9=0 AND !llRpShwZer,'',TRANSFORM(lnNonCost,'99999999.99'))"
       ENDIF
       IF lcrepmode='Text'
          lccst_vlcl = "IIF(lnSize9=0 AND !llRpShwZer,'',' ' + TRANSFORM(EVALUATE(lcStyCost),'9999999.99') +                   ' ' + TRANSFORM(lnClrCost,'99999999.99'))"
       ELSE
          lccst_vlcl = "IIF(lnSize9=0 AND !llRpShwZer,'',TRANSFORM(EVALUATE(lcStyCost),'9999999.99') )"
          lccst_vlc2 = "IIF(lnSize9=0 AND !llRpShwZer,'',TRANSFORM(lnClrCost,'99999999.99'))"
       ENDIF
       lccst_vllc = "' ' + SPACE(10) +' ' + TRANSFORM(lnNonCstLc,'99999999.99')"
       IF lcrepmode='Text'
          lccst_vlop = "' ' + SPACE(10) +' ' + TRANSFORM(lnNonCstOp,'99999999.99')"
       ELSE
          lccst_vlo1 = "' '"
          lccst_vlo2 = "TRANSFORM(lnNonCstOp,'99999999.99')"
       ENDIF
       IF lcrepmode='Text'
          lccst_vlgd = "' ' + SPACE(10) +' ' + TRANSFORM(lnNonCstGd,'99999999.99')"
       ELSE
          lccst_vlg1 = "' '"
          lccst_vlg2 = "TRANSFORM(lnNonCstGd,'99999999.99')"
       ENDIF
    CASE lcrpshow='B'
       lccost_val = IIF(lcrepmode='Text', 'Sales_Value'+'  Cost_Value', SPACE(1)+'Sales_Value'+SPACE(2)+'Cost_Value')
       IF lcrepmode='Text'
          lccst_vlpn = "IIF(lnSize9=0 AND !llRpShwZer,'',' ' + TRANSFORM(lnNonPrice,'999999999.99') +                   ' ' + TRANSFORM(lnNonCost,'99999999.99'))"
       ELSE
          lccst_vlp1 = "IIF(lnSize9=0 AND !llRpShwZer,'',TRANSFORM(lnNonPrice,'999999999.99') )"
          lccst_vlp2 = "IIF(lnSize9=0 AND !llRpShwZer,'',TRANSFORM(lnNonCost,'99999999.99'))"
       ENDIF
       IF lcrepmode='Text'
          lccst_vlcl = "IIF(lnSize9=0 AND !llRpShwZer,'',' ' + TRANSFORM(lnClrPrice,'999999999.99') +                   ' ' + TRANSFORM(lnClrCost,'99999999.99'))"
       ELSE
          lccst_vlc1 = "IIF(lnSize9=0 AND !llRpShwZer,'',TRANSFORM(lnClrPrice,'999999999.99') )"
          lccst_vlc2 = "IIF(lnSize9=0 AND !llRpShwZer,'',TRANSFORM(lnClrCost,'99999999.99'))"
       ENDIF
       lccst_vllc = "' ' + TRANSFORM(lnNonPrcLc,'999999999.99') +                  ' ' + TRANSFORM(lnNonCstLc,'99999999.99')"
       IF lcrepmode='Text'
          lccst_vlop = "' ' + TRANSFORM(lnNonPrcOp,'999999999.99') +                    ' ' + TRANSFORM(lnNonCstOp,'99999999.99')"
       ELSE
          lccst_vlo1 = "TRANSFORM(lnNonPrcOp,'999999999.99')"
          lccst_vlo2 = "TRANSFORM(lnNonCstOp,'99999999.99')"
       ENDIF
       IF lcrepmode='Text'
          lccst_vlgd = "' ' + TRANSFORM(lnNonPrcGd,'999999999.99') +                    ' ' + TRANSFORM(lnNonCstGd,'99999999.99')"
       ELSE
          lccst_vlg1 = "TRANSFORM(lnNonPrcGd,'999999999.99')"
          lccst_vlg2 = "TRANSFORM(lnNonCstGd,'99999999.99')"
       ENDIF
 ENDCASE
 lcrpexp = STRTRAN(lcrpexp, lcmastfile+'.', '')
 DIMENSION latrnrevar[ 8]
 FOR lni = 1 TO 8
    latrnrevar[ lni] = STRTRAN(latrnnonar(lni), 'STYLE', lcrevfile)
 ENDFOR
 lcrevflt = 'EVAL(laTrnRevAr[1])+EVAL(laTrnRevAr[2])+'+'EVAL(laTrnRevAr[3])+EVAL(laTrnRevAr[4])+EVAL(laTrnRevAr[5])+'+'EVAL(laTrnRevAr[6])+EVAL(laTrnRevAr[7])+EVAL(laTrnRevAr[8]) <> 0'
 IF  .NOT. EMPTY(lcextndflt)
    lcrpexp = lcrpexp+' AND '+lcextndflt
    lcrevflt = lcrevflt+' AND '+lcextndflt
    SELECT revstydy
    SET FILTER TO &lcExtndFlt
 ENDIF
 IF  .NOT. llrpshwzer
    SELECT (lcrevfile)
    SET FILTER TO &lcRevFlt
 ENDIF
 IF  .NOT. llrpshwzer
    IF llschkdyeb
       lcrpexp = lcrpexp+' AND (EVAL(laTrnNonar[1])+EVAL(laTrnNonar[2])+'+'EVAL(laTrnNonar[3])+EVAL(laTrnNonar[4])+EVAL(laTrnNonar[5])+'+'EVAL(laTrnNonar[6])+EVAL(laTrnNonar[7])+EVAL(laTrnNonar[8]) <> 0 OR STYDYE.TOTSTK <> 0 )'
    ELSE
       lcrpexp = lcrpexp+' AND EVAL(laTrnNonar[1])+EVAL(laTrnNonar[2])+'+'EVAL(laTrnNonar[3])+EVAL(laTrnNonar[4])+EVAL(laTrnNonar[5])+'+'EVAL(laTrnNonar[6])+EVAL(laTrnNonar[7])+EVAL(laTrnNonar[8]) <> 0'
    ENDIF
 ENDIF
 IF lcrpsortby='W'
    lnfindtran = ASCAN(laalltrns, larpreptar(1))
    IF lnfindtran>0
       lnfindtran = ASUBSCRIPT(laalltrns, lnfindtran, 1)
       lcreptrans = ALLTRIM(laalltrns(lnfindtran,2))
    ENDIF
    IF (lcreptrans='OTS' .OR. lcreptrans='IOTS')
       FOR lni = 1 TO ALEN(latrnnonar)
          lcexp = STRTRAN(latrnnonar(lni), 'STYLE.', 'STYDYE.')
          latrnnonar[ lni] = lcexp
       ENDFOR
    ENDIF
 ENDIF
 SELECT style
 SET RELATION TO 'S'+scale INTO scale
 SELECT (lcmastfile)
 SET RELATION TO style INTO (lcchldfile) ADDITIVE
 IF llnegdis
    IF llrpngatv
       IF  .NOT. llrpwhdeta
          lcrpexp = IIF(EMPTY(lcrpexp), '(STYLE.TOTSTK < 0)', lcrpexp+' AND '+'(STYLE.TOTSTK < 0)')
       ELSE
          lcrpexp = IIF(EMPTY(lcrpexp), '(STYDYE.TOTSTK < 0)', lcrpexp+' AND '+'(STYDYE.TOTSTK < 0)')
       ENDIF
    ELSE
       lcrpexp = IIF(EMPTY(lcrpexp), '(STYLE.TOTSTK >= 0)', lcrpexp+' AND '+'(STYLE.TOTSTK >= 0)')
    ENDIF
 ENDIF
 lcnnmajtl = gfitemmask('HN')
 DO gfdispre WITH EVALUATE('lcRPFormNa'), 'FOR '+lcrpexp
 USE IN revstydy
 USE IN revstyle
*
PROCEDURE lftranaray
 PRIVATE lnfindtran
 lnfindtran = ASCAN(laalltrns, larpreptar(1))
 IF lnfindtran>0
    lnfindtran = ASUBSCRIPT(laalltrns, lnfindtran, 1)
    lcreptarvl = ALLTRIM(laalltrns(lnfindtran,2))
 ELSE
    WAIT WINDOW NOWAIT 'Error, No Transaction was selected !!!'
    RETURN
 ENDIF
 lcprntitle = ''
 DO CASE
    CASE lcreptarvl=='WIP'
       = lfdirctval('WIP')
       lcprntitle = 'WIP'
    CASE lcreptarvl=='SOH'
       = lfdirctval('STK')
       lcprntitle = 'Stock'
    CASE lcreptarvl=='PLA'
       = lfdirctval('PLAN')
       lcprntitle = 'Plan'
    CASE lcreptarvl=='ORD'
       = lfdirctval('ORD')
       lcprntitle = 'Ordered'
    CASE lcreptarvl=='WORD'
       = lfdirctval('NWO')
       lcprntitle = 'Work Ordered'
    CASE lcreptarvl=='INT'
       = lfdirctval('INTRANS')
       lcprntitle = 'Intransit'
    CASE lcreptarvl=='SHP'
       = lfdirctval('SHP')
       lcprntitle = 'Shipped'
    CASE lcreptarvl=='RET'
       = lfdirctval('RET')
       lcprntitle = 'Return'
    CASE lcreptarvl=='RETA'
       = lfdirctval('RA')
       lcprntitle = 'Return Auth.'
    CASE lcreptarvl=='ALO'
       = lfdirctval('ALO')
       lcprntitle = 'Allocated'
    CASE lcreptarvl=='UALO'
       = lfindirect('STK','-ALO')
       lcprntitle = 'Unallocated'
    CASE lcreptarvl=='BOK'
       = lfindirect('SHP','+ORD')
       lcprntitle = 'Booked'
    CASE lcreptarvl=='OTS'
       = lfindirect('STK','-ORD','+'+IIF(lcrpotsb='W', 'WIP', 'PLAN'),.T.)
       lcprntitle = 'Open to sell'
    CASE lcreptarvl=='IOTS'
       = lfindirect('STK','-ORD','',.T.)
       lcprntitle = 'Imm. Open to sell'
 ENDCASE
*
PROCEDURE lfdirctval
 PARAMETER lcdirctval
 PRIVATE lni
 lni = 0
 STORE '' TO latranaray, latrnnonar
 FOR lni = 1 TO 8
    latranaray[ lni] = lcdirctval+STR(lni, 1)
    latrnnonar[ lni] = 'STYLE.'+lcdirctval+STR(lni, 1)
 ENDFOR
*
PROCEDURE lfindirect
 PARAMETER lcvalue1, lcvalue2, lcvalue3, llspeccond
 PRIVATE lcclrval2, lcclrval3, lni
 lcclrval2 = STRTRAN(lcvalue2, '-', '-STYLE.')
 lcclrval2 = STRTRAN(lcclrval2, '+', '+STYLE.')
 IF TYPE('lcValue3')$'UL' .OR. EMPTY(lcvalue3)
    lcvalue3 = ''
 ELSE
    lcclrval3 = STRTRAN(lcvalue3, '-', '-STYLE.')
    lcclrval3 = STRTRAN(lcclrval3, '+', '+STYLE.')
 ENDIF
 lni = 0
 STORE '' TO latranaray, latrnnonar
 FOR lni = 1 TO 8
    latranaray[ lni] = lcvalue1+STR(lni, 1)+lcvalue2+STR(lni, 1)+IIF(EMPTY(lcvalue3), '', lcvalue3+STR(lni, 1))
    latrnnonar[ lni] = 'STYLE.'+lcvalue1+STR(lni, 1)+lcclrval2+STR(lni, 1)+IIF(EMPTY(lcvalue3), '', lcclrval3+STR(lni, 1))
    IF llspeccond
       latranaray[ lni] = "IIF(((lcRpOTSSig = 'P') AND "+latranaray(lni)+" < 0) OR ((lcRpOTSSig = 'N') AND "+latranaray(lni)+' > 0),0,'+latranaray(lni)+')'
       latrnnonar[ lni] = "IIF(((lcRpOTSSig = 'P') AND "+latrnnonar(lni)+" < 0) OR ((lcRpOTSSig = 'N') AND "+latrnnonar(lni)+' > 0),0,'+latrnnonar(lni)+')'
    ENDIF
 ENDFOR
*
PROCEDURE lfchngform
 IF lcrpsortby=='W'
    IF ALEN(larpreptar, 1)=1 .AND.  .NOT. EMPTY(larpreptar)
       lcrpformna = 'ICSTYERL'
    ELSE
       lcrpformna = 'ICSTYERW'
    ENDIF
    llrpwhdeta = .F.
 ELSE
    IF lcrpsortby=='S' .AND. ALEN(larpreptar, 1)=1 .AND.  .NOT. EMPTY(larpreptar)
       lcrpformna = 'ICSTYERS'
    ELSE
       lcrpformna = 'ICSTYERO'
    ENDIF
 ENDIF
 lcno1 = lcrepmode
 lcno2 = lcogplatfo
 = lfreppltfr(lcrpformna)
 IF lcrepmode<>lcno1 .OR. lcogplatfo<>lcno2
    lcrepmode = lcno1
    lcogplatfo = lcno2
    SHOW GET lcrepmode
 ENDIF
*
FUNCTION lfendofgrp
 PARAMETER lcvariable, lcequalexp
 PRIVATE lncuralis
 lncuralis = SELECT(0)
 lctagname = IIF(lcmastfile='STYLE', 'STYLE', IIF(lcrpsortby='S', 'STYDYE', 'STYDYEW'))
 SELECT (lcrevfile)
 lcoldfilte = FILTER()
 lccolors = laogvrflt(INT(ASCAN(laogvrflt, 'SUBSTR(STYLE.Style,lnClrPo,lnColorLen)')/7)+MOD(ASCAN(laogvrflt, 'SUBSTR(STYLE.Style,lnClrPo,lnColorLen)'), 7),6)
 lclocarr = '"'+STRTRAN(lccolors, '|', '","')+'"'
 lncnt = 0
 COUNT FOR SUBSTR(style, 1, lnclrpo)=PADR(style.style, lnmajorlen) TO lncnt
 IF  .NOT. EMPTY(lccolors) .AND. lncnt<>1
    SET FILTER TO INLIST(SUBSTR(Style,lnClrPo,lnColorLen),&lcLocArr)
 ENDIF
 SET ORDER TO (lctagname) DESCENDING
 = SEEK(lcequalexp)
 lcvariable = IIF(BETWEEN(RECNO(), 1, RECCOUNT()), RECNO(), 1)
 SET ORDER TO (lctagname) ASCENDING
 SET FILTER TO &lcOldFilter
 SELECT (lncuralis)
 RETURN ''
*
FUNCTION lfprinthdr
 PRIVATE lcprinthdr
 IF lcstylscl<>PADR(style.style, lnmajorlen)
    lcstylscl = PADR(style.style, lnmajorlen)
    lcscalcode = ' '
 ENDIF
 IF lcrpsortby='S'
    lcprinthdr = lfstyheadr()
 ELSE
    lcprinthdr = lflocheadr()
 ENDIF
 RETURN lcprinthdr
*
FUNCTION lfstyheadr
 PRIVATE lcprinthdr, lcsize, lnallsizes
 STORE '' TO lcnonmjdes, lcprinthdr, lclocbins
 lnallsizes = 0
 = lfinitvals()
 FOR lnallsizes = 1 TO 8
    lcsize = 'lnSize'+STR(lnallsizes, 1)
    &lcSize = EVALUATE(laTranAray[lnAllSizes])
    lnSize9 = lnSize9 + &lcSize
 ENDFOR
 IF ('OTS'$lcreptarvl) .AND. (((lcrpotssig='P') .AND. (lnsize9<lnrpotsmin)) .OR. ((lcrpotssig='N') .AND. (lnsize9>lnrpotsmin)))
    STORE 0 TO lnsize1, lnsize2, lnsize3, lnsize4, lnsize5, lnsize6, lnsize7, lnsize8, lnsize9
 ENDIF
 IF llrpshwzer .OR. lnsize9<>0
    lnnonprice = lnsize9*style.pricea
    IF lcmastfile='STYLE' .OR. EMPTY(dyelot)
       IF llgnglcst
          lnnoncost = IIF('SOH'$lcreptarvl, nstkval, IIF(lccstmeth='S', style.totcost, ave_cost)*lnsize9)
       ELSE
          lnnoncost = IIF('SOH'$lcreptarvl, IIF(lccstmeth='S', style.totcost*lnsize9, nstkval), IIF(lccstmeth='S', style.totcost, ave_cost)*lnsize9)
       ENDIF
       IF llgnglcst
          lncstavevl = IIF('SOH'$lcreptarvl, nstkval/lnsize9, IIF(lccstmeth='S', style.totcost, ave_cost))
       ELSE
          lncstavevl = IIF('SOH'$lcreptarvl, IIF(lccstmeth='S', style.totcost, nstkval/lnsize9), IIF(lccstmeth='S', style.totcost, ave_cost))
       ENDIF
    ELSE
       IF  .NOT. llrpwhdeta
          IF lccstmeth='A'
             = lfgetcosts(@lncstavevl)
          ELSE
             lncstavevl = style.totcost
          ENDIF
       ENDIF
       lnnoncost = lnsize9*lncstavevl
    ENDIF
    = lfnonmjdes()
    IF gcdevice='PRINTER' .AND. _PCOPIES>1
       IF lnfrstrec1=0 .OR. lnfrstrec1=RECNO()
          lnfrstrec1 = RECNO()
          lcprntsty = SPACE(lnmajorlen)
          lcscalcode = SPACE(3)
       ENDIF
    ENDIF
    IF  .NOT. (PADR(style.style, lnmajorlen)==lcprntsty)
       lcprntsty = PADR(style.style, lnmajorlen)
       IF  .NOT. llrpprtsiz
          lcprinthdr = PADR(ALLTRIM(lcmajttl)+' :'+ALLTRIM(EVALUATE(lcstygroup))+' '+PADR(style.desc, 15)+lfendofgrp(@lnendofsty,PADR(style.style, lnmajorlen)), 35)+SPACE(3)
       ELSE
          IF lltextmode
             lcprinthdr = PADR(ALLTRIM(lcmajttl)+' :'+ALLTRIM(EVALUATE(lcstygroup))+' '+PADR(style.desc, 15)+lfendofgrp(@lnendofsty,PADR(style.style, lnmajorlen)), 35)+SPACE(3)+IIF(lcscalcode=style.scale, '', PADL(ALLTRIM(scale.sz1), 5)+SPACE(3)+PADL(ALLTRIM(scale.sz2), 5)+SPACE(3)+PADL(ALLTRIM(scale.sz3), 5)+SPACE(3)+PADL(ALLTRIM(scale.sz4), 5)+SPACE(3)+PADL(ALLTRIM(scale.sz5), 5)+SPACE(3)+PADL(ALLTRIM(scale.sz6), 5)+SPACE(3)+PADL(ALLTRIM(scale.sz7), 5)+SPACE(3)+PADL(ALLTRIM(scale.sz8), 5))
          ELSE
             lcprinthdr = PADR(ALLTRIM(lcmajttl)+' :'+ALLTRIM(EVALUATE(lcstygroup))+' '+PADR(style.desc, 15)+lfendofgrp(@lnendofsty,lcprntsty), 35)
             FOR i = 1 TO 8
                z = STR(i, 1)
                laScals[I] = PADL(ALLTRIM(Scale.Sz&z),5)
             ENDFOR
          ENDIF
       ENDIF
       lnmaxcnt = scale.cnt
       = lfsumgroup('lnStySz','lnSize',.T.)
       lnnonprcop = lnnonprice
       lnnoncstop = lnnoncost
       lcscalcode = style.scale
    ELSE
       IF llrpscale
          IF style.scale<>lcscalcode
             IF  .NOT. llrpprtsiz
                lcprinthdr = PADR(ALLTRIM(lcmajttl)+' :'+ALLTRIM(EVALUATE(lcstygroup))+' '+PADR(style.desc, 15)+lfendofgrp(@lnendofsty,PADR(style.style, lnmajorlen)), 35)+SPACE(3)
             ELSE
                IF lltextmode
                   lcprinthdr = PADR(ALLTRIM(lcmajttl)+' :'+ALLTRIM(EVALUATE(lcstygroup))+' '+PADR(style.desc, 15)+lfendofgrp(@lnendofsty,PADR(style.style, lnmajorlen)), 35)+SPACE(3)+PADL(ALLTRIM(scale.sz1), 5)+SPACE(3)+PADL(ALLTRIM(scale.sz2), 5)+SPACE(3)+PADL(ALLTRIM(scale.sz3), 5)+SPACE(3)+PADL(ALLTRIM(scale.sz4), 5)+SPACE(3)+PADL(ALLTRIM(scale.sz5), 5)+SPACE(3)+PADL(ALLTRIM(scale.sz6), 5)+SPACE(3)+PADL(ALLTRIM(scale.sz7), 5)+SPACE(3)+PADL(ALLTRIM(scale.sz8), 5)
                   lcprinthdr = STRTRAN(lcprinthdr, SUBSTR(lcprinthdr, 1, 35), SPACE(35))
                ELSE
                   lcprinthdr = PADR(ALLTRIM(lcmajttl)+' :'+ALLTRIM(EVALUATE(lcstygroup))+' '+PADR(style.desc, 15)+lfendofgrp(@lnendofsty,lcprntsty), 35)
                   FOR i = 1 TO 8
                      z = STR(i, 1)
                      laScals[I] = PADL(ALLTRIM(Scale.Sz&z),5)
                   ENDFOR
                ENDIF
             ENDIF
             lcscalcode = style.scale
          ELSE
             lcprinthdr = ''
          ENDIF
       ENDIF
       lnmaxcnt = MAX(lnmaxcnt, scale.cnt)
       IF lnsize1<>0 .OR. lnsize2<>0 .OR. lnsize3<>0 .OR. lnsize4<>0 .OR. lnsize5<>0 .OR. lnsize6<>0 .OR. lnsize7<>0 .OR. lnsize8<>0
          IF gcdevice='PRINTER' .AND. _PCOPIES>1
             IF lnfrstrec1=0 .OR. lnfrstrec1=RECNO()
                lnfrstrec1 = RECNO()
                STORE 0 TO lnstysz1, lnstysz2, lnstysz3, lnstysz4, lnstysz5, lnstysz6, lnstysz7, lnstysz8, lnstysz9, lnnonprcop, lnnoncstop
             ENDIF
          ENDIF
          = lfsumgroup('lnStySz','lnSize')
          IF lcmastfile='STYLE' .OR. EMPTY(dyelot)
             lnnonprcop = lnnonprcop+lnnonprice
             lnnoncstop = lnnoncstop+lnnoncost
          ENDIF
       ENDIF
    ENDIF
    IF lnsize1<>0 .OR. lnsize2<>0 .OR. lnsize3<>0 .OR. lnsize4<>0 .OR. lnsize5<>0 .OR. lnsize6<>0 .OR. lnsize7<>0 .OR. lnsize8<>0
       IF gcdevice='PRINTER' .AND. _PCOPIES>1
          IF lnfrstrec=0 .OR. lnfrstrec=RECNO()
             lnfrstrec = RECNO()
             STORE 0 TO lngrdsz1, lngrdsz2, lngrdsz3, lngrdsz4, lngrdsz5, lngrdsz6, lngrdsz7, lngrdsz8, lngrdsz9, lnnonprcgd, lnnoncstgd
          ENDIF
       ENDIF
       = lfsumgroup('lnGrdSz','lnSize')
       IF lcmastfile='STYLE' .OR. EMPTY(dyelot)
          lnnonprcgd = lnnonprcgd+lnnonprice
          lnnoncstgd = lnnoncstgd+lnnoncost
       ENDIF
    ENDIF
    IF lcmastfile='STYDYE' .AND. llprintclr .AND.  .NOT. (RIGHT(style, 19-lnmajorlen)==lcprntnon)
       llprnclrln = .T.
       lcprntnon = RIGHT(style, 19-lnmajorlen)
       lnallsizes = 0
       lnclrsz9 = 0
       FOR lnallsizes = 1 TO 8
          lcsize = 'lnClrSz'+STR(lnallsizes, 1)
          &lcSize  = EVALUATE(laTrnNonAr[lnAllSizes])
          lnClrSz9 = lnClrSz9 + &lcSize
       ENDFOR
       lnclrprice = lnclrsz9*style.pricea
       IF llgnglcst
          lnclrcost = IIF('SOH'$lcreptarvl, style.nstkval, IIF(lccstmeth='S', style.totcost, style.ave_cost)*lnclrsz9)
       ELSE
          lnclrcost = IIF('SOH'$lcreptarvl, IIF(lccstmeth='S', style.totcost*lnclrsz9, style.nstkval), IIF(lccstmeth='S', style.totcost, style.ave_cost)*lnclrsz9)
       ENDIF
    ELSE
       llprnclrln = .F.
    ENDIF
 ENDIF
 RETURN lcprinthdr
*
PROCEDURE lfsumgroup
 PARAMETER lcsummedvr, lclinevar, llprimary
 IF lcmastfile='STYLE' .OR. EMPTY(dyelot)
    PRIVATE lnallsizes, lcvar, lcval
    lnallsizes = 0
    FOR lnallsizes = 1 TO 9
       lcvar = lcsummedvr+STR(lnallsizes, 1)
       lcval = lclinevar+STR(lnallsizes, 1)
       IF llprimary
          &lcVar = &lcVal
       ELSE
          &lcVar = &lcVar + &lcVal
       ENDIF
    ENDFOR
 ENDIF
*
PROCEDURE lfnonmjdes
 PRIVATE lni, lctemp
 STORE '' TO lctemp
 lni = 0
 FOR lni = lnmajseg+1 TO ALEN(lamajseg, 1)
    lctemp = ''
    DO CASE
       CASE lamajseg(lni,1)$'FOTQ'
          IF SEEK(STR(lni, 1)+SUBSTR(style, lamajseg(lni,4), LEN(lamajseg(lni,3))), 'ICSEGVAL')
             lctemp = ALLTRIM(icsegval.cisgvalsd)
          ENDIF
       CASE lamajseg(lni,1)$'ZCDG'
          DO CASE
             CASE lamajseg(lni,1)='Z'
                lccodeexpr = 'SEASON'
             CASE lamajseg(lni,1)='C'
                lccodeexpr = 'COLOR'
             CASE lamajseg(lni,1)='D'
                lccodeexpr = 'CDIVISION'
             OTHERWISE
                lccodeexpr = 'CSTYGROUP'
          ENDCASE
          lctemp = ALLTRIM(gfcoddes(SUBSTR(style, lamajseg(lni,4), LEN(lamajseg(lni,3))),lccodeexpr,.T.))
       OTHERWISE
          IF SEEK('S'+SUBSTR(style, lamajseg(lni,4), LEN(lamajseg(lni,3))), 'SCALE')
             lctemp = ALLTRIM(scale.cscl_desc)
          ENDIF
    ENDCASE
    lcnonmjdes = IIF(EMPTY(lcnonmjdes), lctemp, lcnonmjdes+IIF(EMPTY(lctemp), '', '-')+lctemp)
 ENDFOR
*
PROCEDURE lfinitvals
 STORE 0 TO lnsize1, lnsize2, lnsize3, lnsize4, lnsize5, lnsize6, lnsize7, lnsize8, lnsize9, lnnonprice, lnnoncost
 IF lcmastfile='STYDYE' .AND. llprintclr
    STORE 0 TO lnclrsz1, lnclrsz2, lnclrsz3, lnclrsz4, lnclrsz5, lnclrsz6, lnclrsz7, lnclrsz8, lnclrsz9, lnclrprice, lnclrcost
 ENDIF
*
PROCEDURE lfgetcosts
 PARAMETER lnthiscost
 PRIVATE lncurralis, lcseekexpr
 lncurralis = SELECT(0)
 SELECT revstydy
 lcseekexpr = IIF(lcrpsortby='S', stydye.style+stydye.cwarecode, stydye.cwarecode+stydye.style)
 IF SEEK(lcseekexpr)
    lnthiscost = ave_cost
 ELSE
    lnthiscost = 0
 ENDIF
 SELECT (lncurralis)
*
FUNCTION lfprntbin
 IF (lcrpsortby='L' .OR. llrpprnloc) .AND. EMPTY(dyelot) .AND. (llrpshwzer .OR. lnsize9<>0)
    PRIVATE lncurrals, lccurrord
    lncurrals = SELECT(0)
    SELECT whsloc
    lccurrord = ORDER()
    SET ORDER TO WHSLOCST
    IF SEEK(stydye.style+SPACE(6)+stydye.cwarecode)
       PRIVATE lnbinnum
       lnbinnum = 0
       SCAN REST WHILE style+color+cwarecode+clocation=stydye.style+SPACE(6)+stydye.cwarecode
          lclocbins = IIF(EMPTY(lclocbins), 'Bin : ', lclocbins+', ')+ALLTRIM(clocation)
          lnbinnum = lnbinnum+1
       ENDSCAN
       IF lnbinnum>1
          lclocbins = STRTRAN(lclocbins, 'Bin :', 'Bins :')
       ENDIF
    ENDIF
    SET ORDER TO &lcCurrOrd
    SELECT (lncurrals)
 ENDIF
 RETURN ''
*
PROCEDURE lfvsalval
 IF llrpsalval
    lcrpshow = 'S'
 ELSE
    lcrpshow = 'N'
 ENDIF
 = lfvprint()
*
FUNCTION lflocheadr
 PRIVATE lcprinthdr, lcsize, lnallsizes
 STORE '' TO lcnonmjdes, lcprinthdr, lclocbins, lcprintsty
 lnallsizes = 0
 = lfinitvals()
 FOR lnallsizes = 1 TO 8
    lcsize = 'lnSize'+STR(lnallsizes, 1)
    &lcSize = EVALUATE(laTranAray[lnAllSizes])
    lnSize9 = lnSize9 + &lcSize
 ENDFOR
 IF ('OTS'$lcreptarvl) .AND. (((lcrpotssig='P') .AND. (lnsize9<lnrpotsmin)) .OR. ((lcrpotssig='N') .AND. (lnsize9>lnrpotsmin)))
    STORE 0 TO lnsize1, lnsize2, lnsize3, lnsize4, lnsize5, lnsize6, lnsize7, lnsize8, lnsize9
 ENDIF
 IF llrpshwzer .OR. lnsize9<>0
    lnnonprice = lnsize9*style.pricea
    IF EMPTY(dyelot)
       IF llgnglcst
          lnnoncost = IIF('SOH'$lcreptarvl, nstkval, IIF(lccstmeth='A', ave_cost, style.totcost)*lnsize9)
          lncstavevl = IIF('SOH'$lcreptarvl, nstkval/lnsize9, IIF(lccstmeth='A', ave_cost, style.totcost))
       ELSE
          lnnoncost = IIF('SOH'$lcreptarvl, IIF(lccstmeth='S', style.totcost*lnsize9, nstkval), IIF(lccstmeth='A', ave_cost, style.totcost)*lnsize9)
          lncstavevl = IIF('SOH'$lcreptarvl, IIF(lccstmeth='S', style.totcost, nstkval/lnsize9), IIF(lccstmeth='A', ave_cost, style.totcost))
       ENDIF
       = lfnonmjdes()
    ELSE
       IF llrpprndye
          IF lccstmeth='A'
             = lfgetcosts(@lncstavevl)
          ELSE
             lncstavevl = style.totcost
          ENDIF
       ENDIF
       lnnoncost = lnsize9*lncstavevl
    ENDIF
    IF gcdevice='PRINTER' .AND. _PCOPIES>1
       IF lnfrstrec1=0 .OR. lnfrstrec1=RECNO()
          lnfrstrec1 = RECNO()
          lcprntloc = SPACE(6)
          lcprntsty = SPACE(lnmajorlen)
          lcscalcode = SPACE(3)
       ENDIF
    ENDIF
    IF  .NOT. (cwarecode==lcprntloc)
       lcprntloc = cwarecode
       lcprinthdr = 'Location : '+cwarecode+lfendofgrp(@lnendofloc,cwarecode)
       = lfsumgroup('lnLocSz','lnSize',.T.)
       lnnonprclc = lnnonprice
       lnnoncstlc = lnnoncost
    ELSE
       IF lnsize1<>0 .OR. lnsize2<>0 .OR. lnsize3<>0 .OR. lnsize4<>0 .OR. lnsize5<>0 .OR. lnsize6<>0 .OR. lnsize7<>0 .OR. lnsize8<>0
          IF gcdevice='PRINTER' .AND. _PCOPIES>1
             IF lnfrstrec1=0 .OR. lnfrstrec1=RECNO()
                lnfrstrec1 = RECNO()
                STORE 0 TO lnlocsz1, lnlocsz2, lnlocsz3, lnlocsz4, lnlocsz5, lnlocsz6, lnlocsz7, lnlocsz8, lnlocsz9, lnnonprclc, lnnoncstlc
             ENDIF
          ENDIF
          = lfsumgroup('lnLocSz','lnSize')
          IF EMPTY(dyelot)
             lnnonprclc = lnnonprclc+lnnonprice
             lnnoncstlc = lnnoncstlc+lnnoncost
          ENDIF
       ENDIF
    ENDIF
    IF  .NOT. (cwarecode+PADR(style.style, lnmajorlen)==lcprntsty)
       lcprntsty = cwarecode+PADR(style.style, lnmajorlen)
       IF  .NOT. llrpprtsiz
          lcprintsty = PADR(ALLTRIM(lcmajttl)+' :'+ALLTRIM(EVALUATE(lcstygroup))+' '+PADR(style.desc, 15)+lfendofgrp(@lnendofsty,lcprntsty), 35)+SPACE(3)
       ELSE
          IF lltextmode
             lcprintsty = PADR(ALLTRIM(lcmajttl)+' :'+ALLTRIM(EVALUATE(lcstygroup))+' '+PADR(style.desc, 15)+lfendofgrp(@lnendofsty,lcprntsty), 35)+SPACE(3)+IIF(lcscalcode=style.scale, '', PADL(ALLTRIM(scale.sz1), 5)+SPACE(3)+PADL(ALLTRIM(scale.sz2), 5)+SPACE(3)+PADL(ALLTRIM(scale.sz3), 5)+SPACE(3)+PADL(ALLTRIM(scale.sz4), 5)+SPACE(3)+PADL(ALLTRIM(scale.sz5), 5)+SPACE(3)+PADL(ALLTRIM(scale.sz6), 5)+SPACE(3)+PADL(ALLTRIM(scale.sz7), 5)+SPACE(3)+PADL(ALLTRIM(scale.sz8), 5))
          ELSE
             lcprintsty = PADR(ALLTRIM(lcmajttl)+' :'+ALLTRIM(EVALUATE(lcstygroup))+' '+PADR(style.desc, 15)+lfendofgrp(@lnendofsty,lcprntsty), 35)
             FOR i = 1 TO 8
                z = STR(i, 1)
                laScals[I] = PADL(ALLTRIM(Scale.Sz&z),5)
             ENDFOR
          ENDIF
       ENDIF
       lnmaxcnt = scale.cnt
       = lfsumgroup('lnStySz','lnSize',.T.)
       lnnonprcop = lnnonprice
       lnnoncstop = lnnoncost
       lcscalcode = style.scale
    ELSE
       IF llrpscale
          IF style.scale<>lcscalcode
             IF  .NOT. llrpprtsiz
                lcprinthdr = PADR(ALLTRIM(lcmajttl)+' :'+ALLTRIM(EVALUATE(lcstygroup))+' '+PADR(style.desc, 15)+lfendofgrp(@lnendofsty,PADR(style.style, lnmajorlen)), 35)+SPACE(3)
             ELSE
                IF lltextmode
                   lcprinthdr = PADR(ALLTRIM(lcmajttl)+' :'+ALLTRIM(EVALUATE(lcstygroup))+' '+PADR(style.desc, 15)+lfendofgrp(@lnendofsty,lcprntsty), 35)+SPACE(3)+PADL(ALLTRIM(scale.sz1), 5)+SPACE(3)+PADL(ALLTRIM(scale.sz2), 5)+SPACE(3)+PADL(ALLTRIM(scale.sz3), 5)+SPACE(3)+PADL(ALLTRIM(scale.sz4), 5)+SPACE(3)+PADL(ALLTRIM(scale.sz5), 5)+SPACE(3)+PADL(ALLTRIM(scale.sz6), 5)+SPACE(3)+PADL(ALLTRIM(scale.sz7), 5)+SPACE(3)+PADL(ALLTRIM(scale.sz8), 5)
                   lcprinthdr = STRTRAN(lcprinthdr, SUBSTR(lcprinthdr, 1, 35), SPACE(35))
                ELSE
                   lcprintsty = PADR(ALLTRIM(lcmajttl)+' :'+ALLTRIM(EVALUATE(lcstygroup))+' '+PADR(style.desc, 15)+lfendofgrp(@lnendofsty,lcprntsty), 35)
                   FOR i = 1 TO 8
                      z = STR(i, 1)
                      laScals[I] = PADL(ALLTRIM(Scale.Sz&z),5)
                   ENDFOR
                ENDIF
             ENDIF
             lcscalcode = style.scale
          ELSE
             lcprinthdr = ''
          ENDIF
       ENDIF
       lnmaxcnt = MAX(lnmaxcnt, scale.cnt)
       IF lnsize1<>0 .OR. lnsize2<>0 .OR. lnsize3<>0 .OR. lnsize4<>0 .OR. lnsize5<>0 .OR. lnsize6<>0 .OR. lnsize7<>0 .OR. lnsize8<>0
          IF gcdevice='PRINTER' .AND. _PCOPIES>1
             IF lnfrstrec1=0 .OR. lnfrstrec1=RECNO()
                lnfrstrec1 = RECNO()
                STORE 0 TO lnstysz1, lnstysz2, lnstysz3, lnstysz4, lnstysz5, lnstysz6, lnstysz7, lnstysz8, lnstysz9, lnnonprcop, lnnoncstop
             ENDIF
          ENDIF
          = lfsumgroup('lnStySz','lnSize')
          IF EMPTY(dyelot)
             lnnonprcop = lnnonprcop+lnnonprice
             lnnoncstop = lnnoncstop+lnnoncost
          ENDIF
       ENDIF
    ENDIF
    IF lnsize1<>0 .OR. lnsize2<>0 .OR. lnsize3<>0 .OR. lnsize4<>0 .OR. lnsize5<>0 .OR. lnsize6<>0 .OR. lnsize7<>0 .OR. lnsize8<>0
       IF gcdevice='PRINTER' .AND. _PCOPIES>1
          IF lnfrstrec=0 .OR. lnfrstrec=RECNO()
             lnfrstrec = RECNO()
             STORE 0 TO lngrdsz1, lngrdsz2, lngrdsz3, lngrdsz4, lngrdsz5, lngrdsz6, lngrdsz7, lngrdsz8, lngrdsz9, lnnonprcgd, lnnoncstgd
          ENDIF
       ENDIF
       = lfsumgroup('lnGrdSz','lnSize')
       IF EMPTY(dyelot)
          lnnonprcgd = lnnonprcgd+lnnonprice
          lnnoncstgd = lnnoncstgd+lnnoncost
       ENDIF
    ENDIF
 ENDIF
 RETURN lcprinthdr
*
PROCEDURE lfsrsty
 PARAMETER lcparm
 IF lcparm='S'
    USE IN 0 (gcdatadir+'Style') AGAIN ALIAS style_x ORDER Style
    SELECT style
    SET ORDER TO Cstyle
    SET RELATION TO style.style INTO style_x
    GOTO TOP IN style
 ELSE
    USE IN style_x
    SELECT style
    SET ORDER TO STYLE
 ENDIF
*
FUNCTION lfstysum
 PARAMETER lcsty, lccomp, lnaddtovar
 PRIVATE lnstyrec
 lntotcomp = 0
 IF RECCOUNT('STYLE')<>0
    lnstyrec = RECNO('STYLE')
    SELECT style_x
    SUM &lcCOMP TO lnTotcomp WHILE ALLTRIM(cStyMajor) == ALLTRIM(lcSty)
    SELECT style
    IF BETWEEN(lnstyrec, 1, RECCOUNT())
       GOTO lnstyrec
    ENDIF
    DO CASE
       CASE lnaddtovar=1
          lno_t_s = lntotcomp
       CASE lnaddtovar=2
          lno_t_s = lno_t_s+lntotcomp
       CASE lnaddtovar=3
          lno_t_s = lno_t_s-lntotcomp
    ENDCASE
 ENDIF
 RETURN INT(lntotcomp)
*
PROCEDURE lfclrread
 CLEAR READ
*
PROCEDURE lfpvrun
 PRIVATE lcsty1, lcsty2
 STORE 0 TO lcsty1, lcsty2
 DIMENSION laitemseg[ 1]
 STORE 0 TO lncolorlen
 = gfitemmask(@laitemseg)
 FOR lncount = 1 TO ALEN(laitemseg, 1)
    IF laitemseg(lncount,1)='C'
       lncolorlen = LEN(laitemseg(lncount,3))
       EXIT
    ENDIF
 ENDFOR
 lnclrsgpo = ASUBSCRIPT(laogvrflt, ASCAN(laogvrflt, 'SUBSTR(STYLE.Style,lnClrPo,lnColorLen)'), 1)
 lcsty1 = SUBSTR(laogvrflt(lnclrsgpo,6), 1, lncolorlen)
 FOR lncounter = 1 TO OCCURS('|', laogvrflt(lnclrsgpo,6))+1
    IF lncounter>1
       lnfirstpos = ATC('|', laogvrflt(lnclrsgpo,6), lncounter-1)
       lcsty2 = SUBSTR(laogvrflt(lnclrsgpo,6), lnfirstpos+1, lncolorlen)
    ENDIF
    IF  .NOT. EMPTY(lcsty2)
       lcsty1 = lcsty1+'|'+lcsty2
    ELSE
       lcsty1 = lcsty1
    ENDIF
 ENDFOR
 laogvrflt[ lnclrsgpo, 6] = lcsty1
*
FUNCTION lfrstgrdvr
 STORE 0 TO lngrdsz1, lngrdsz2, lngrdsz3, lngrdsz4, lngrdsz5, lngrdsz6, lngrdsz7, lngrdsz8, lngrdsz9, lnnonprcgd, lnnoncstgd
 RETURN ''
*
*** 
*** ReFox - all is not lost 
***
