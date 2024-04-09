*:***************************************************************************
*: Program file  : ICSYFXJL         (REFERENCE, C102228)
*: Program desc. : 
*: For Report    : NONE
*: System        : Aria Advantage Series.
*: Module        : Inventory Control (IC)
*: Developer     : Abdel-Rahim Koura-KAM
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO ICSYFXJL
*: B804328,1 ASH 08/01/2001 Both ordline and invline files show some styles with 
*: B804328,1                their original codes instead of the codes of the blank style (style components)
*: B804363,1 WAB 08/16/2001 Fix the bug of some styles didn't get the phisical inventory
*:********************************************************************

lcSorcCo = ''
IF !gfSetup()
  RETURN
ENDIF  


DO (gcScrDir+gcWinAppl+"\ICCONJL.SPX")

*!*************************************************************
*! Name      : lfvOk
*: Date      : 15/05/2001
*: Developer : Ahmed Maher (AMH)
*! Purpose   : Valid function of the Ok button.
*!*************************************************************
*! Example            : =lfvOk()
*!*************************************************************
*
FUNCTION lfvOk

*-- Open needed files.
IF !FILE(lcSorcCo+"CUTTKTH.DBF") .OR. !FILE(lcSorcCo+"CUTTKTL.DBF")
  WAIT WINDOW 'FILE NOT FOUND'
  RETURN 
ENDIF

=gfOpenFile(lcSorcCo+"CUTTKTH","CUTTKTH","SH")
=gfOpenFile(lcSorcCo+"CUTTKTL","Cuttktls","SH")

*--declartion part

STORE 0 TO lnClrLen,lnClrPos              				&& hold color position and color length
STORE .F. TO llGlLink,llDyelot,llMultiWH,llWareLoc   	&& variable(s) declare for using by lfsave 
STORE .T. TO llGoOn                       				&& variable declare for using by lfsave 
STORE '' TO lcTmpAdj,lcWare,lcCostMth     				&& variable(s) declare for using by lfsave 
ldDate = gdSysDate
STORE gdSysDate TO ldPstDate              				&& post date using by lfsave 
DIMENSION laOpFile[3]                     				&& this array using by lfsave
STORE .T. TO laOpFile                     				&& this variable using by lfsave function    
DIMENSION laScr[1],laTrg[1],laTrgDsc[1]   				&& array hold bom.citmmask and bom.item 
DIMENSION laSetupP[5,2],laStyP[1]         				&& arrays hold the type of PO
STORE '' To laStyP,laSetupP               				&& assign space to returned array 
lcTmpAdj  = gfTempName()                  				&& get temp name for adj file using by lfsave
lcTmpMjr  = gfTempName()                  				&& get name for file which contain ite name for deletion
lcOrder   = gfTempName()
lcInvoice = gfTempName()
lcRaLines = gfTempName()
lcRetlin  = gfTempName()
lcPikline = gfTempName()
lcPacklin = gfTempName()
lcConsInv = gfTempName()


lcType   = 'A'                            && variable using by lfsave function  
=lfOpenFil()                              && open master files
=lfGetTF()                                && get temp files for improving performance   
=lfCreatFil()                             && create tmp file for adj
=lfGetType()                              && get value of "BOM.type" 
lnArStyTyp=ALEN(laStyP,1)                 && get array type length 
lnItmMaj   = LEN(gfItemMask('PM'))        && get the length o major item
lnItmNonMj = LEN(gfItemMask('PN'))        && get length of nonmajor item
llWareLoc  = gfGetMemVar('M_WareLoc')  ='Y'
lcCostMth  = gfGetMemVar('M_Cost_Meth')   && get cost  method  
llGlLink   = gfGetMemVar('M_Link_GL')  ='Y'  && get link to GL or not
llMultiWH  = gfGetMemVar('M_WareHouse')='Y'  && get multiwarehous or not 
lnNewCost = 0
lnOldStk = 0
=lfGetClrD()                              && get color position in style
SELECT WAREHOUS                           && select warehous file to assign first recorcd 
LOCATE                                    && to lcware
lcWare=cwarecode                          && here assignment using for lfsave
SKIP
lcScndWare=cwarecode

IF llWareLoc 
  DIMENSION laSource[1],laTarget[1]
  IF !laOpFile[1]
    laOpFile[1]=gfOpenFile(gcDataDir+'WhsLoc','WhsLocSt','SH')
  ENDIF
  SELECT WhsLoc
  =AFIELDS(laStru)
  lcTemLoc  = gfTempName()   
  CREATE DBF (gcWorkDir+lcTemLoc) FROM ARRAY laStru
  INDEX ON STYLE+CWARECODE+CLOCATION TAG (lcTemLoc) OF (gcWorkDir+lcTemLoc)
  lcWareAny = SPACE(06)
ENDIF  
SELECT STYLE 
SCAN                                      && scan through style file
  WAIT WINDOW "STYLE :- " + STYLE.STYLE NOWAIT
  lnNewCost = Style.ave_cost
  lnOldStk  = Style.TotStk
  FOR lnCount=1  TO lnArStyTyp                  && loop for type number
    IF !EMPTY(laStyP[lnCount])                  && if element array not empty  
      *lcSearchKy=laStyP[X]+STYLE.STYLE    && get search key 
      *B804328,1 ASH 08/01/2001 [Begin] Both ordline and invline files show some styles with 
      *B804328,1                their original codes instead of the codes of the blank style (style components)
      *lcSearchKy =ALLTRIM(laStyP[lnCount]+SUBSTR(STYLE,1,lnItmMaj))
      lcSearchKy =ALLTRIM(laStyP[lnCount])+SUBSTR(STYLE,1,lnItmMaj)
      *B804328,1 ASH 08/01/2001 [End] 
      SELECT BOM
      IF SEEK(lcSearchKy)                    && search in bom file
        SCAN REST WHILE TYP+ITEM=lcSearchKy  && scan rest for that condition in bom file
          IF SUBSTR(item,lnClrPos,lnItmNonMj)="******" .OR.  ;
            SUBSTR(item,lnClrPos,lnItmNonMj)=SUBSTR(STYLE.STYLE,lnClrPos,lnItmNonMj)
            IF SUBSTR(cItmMask,lnClrPos,lnItmNonMj)="******" 
              lcScrFld=SUBSTR(citmMask,1,lnItmMaj+1)+SUBSTR(STYLE.STYLE,lnClrPos,lnClrLen)
            ELSE
              lcScrFld=cItmMask
            ENDIF
            =lfRplcOrd(lcScrFld,STYLE.STYLE,Style.Desc,STYLE.SCALE)
            *WAB
            *=lfRplcInv(lcScrFld,STYLE.STYLE,Style.Desc,STYLE.SCALE)
            **TMI [Start] to replace inside RaLine,RetLine , PikLine , Pack_Lin
            *=lfRplcRa(lcScrFld,STYLE.STYLE,Style.Desc)
            *=lfRplcRet(lcScrFld,STYLE.STYLE,Style.Desc)
            *=lfRplcPik(lcScrFld,STYLE.STYLE,Style.Desc)
            *SELECT PIKLINE 
            *LOCATE
            *REPLACE ALL STYLE WITH STYLE.STYLE , ;
            *            DESC1 WITH Style.Desc    ;          
            *           FOR PIKLINE.STYLE=lcScrFld
            *SELECT PACK_LIN 
            *LOCATE
            *REPLACE ALL STYLE WITH STYLE.STYLE FOR PACK_LIN.STYLE=lcScrFld
            *=lfRplcPack(lcScrFld,STYLE.STYLE,Style.Desc,STYLE.SCALE)
            **TMI  [End  ] 
            =lfGetQtyCT(lcScrFld,STYLE.STYLE)
            *--get item(s) for deletion process
            =lfGetItmD(BOM.cItmMask)
          ENDIF
        ENDSCAN
      ENDIF  
    ENDIF      && !EMPTY(laStyP[X])
  ENDFOR       && FOR X=1  TO lnArr
ENDSCAN        && scan through style file
=lfUpdOrdl()
*=lfUpInvl()
*=lfUpRal()
*=lfUpRetl()
**Update rest

SELECT(lcTmpAdj)
IF RECCOUNT()>0 
  *--call lfsave for inventory adjustment
  DO lfSave IN (gcapphome+gcwinappl+'\ICInvSav.PRG') WITH .F.        
ENDIF  
*--delete all style(s) which found in lcTmpMjr file from all file(s)
=lfDelAll()
=lfClosFil()  && close all files i opened 
=lfClosTF()   && close all temp files    
CLEAR READ
RETURN
*!*************************************************************
*! Name      : lfGetClrD
*! Developer : AbdElrahim Koura-KAM
*! Date      : 08/03/2001
*! Purpose   : To get color position
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example            :  =lfGetClrD()
*!*************************************************************
function lfGetClrD
DECLARE laItemSeg[1]
lcOldSelect=select()
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLen = LEN(laItemSeg[lnCount,3])
    lnClrPos = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR
SELECT(lcOldSelect)
*--Get Color Discreption  
*!*************************************************************
*! Name      : lfRplcOrd
*! Developer : AbdElrahim Koura-KAM
*! Date      : 08/03/2001
*! Purpose   : To replace ordline.style with bom.item
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example            :  =lfRplcOrd()
*!*************************************************************
FUNCTION lfRplcOrd
PARAMETERS lcSource,lcTarget,lcTrgDsc,lcScale
PRIVATE lcOldArea
lcOldArea=SELECT()
SELECT ORDLINE
IF SEEK(lcSource)
  SET DELETED OFF
  SCAN REST WHILE Style+DTOS(complete)+cOrdtype+Order+Store+STR(lineno,6)=lcSource
    WAIT WINDOW "STYLE :- " + STYLE NOWAIT
    SCATTER MEMVAR MEMO
    M.STYLE = lcTarget  
    M.DESC1 = lcTrgDsc
    STOR 0 TO M.CUT1,M.CUT2,M.CUT3,M.CUT4,M.CUT5,M.CUT6,M.CUT7,M.CUT8,M.TOTCUT   
    INSERT INTO (lcOrder) FROM MEMVAR
    REPLACE OrdHdr.TotCut WITH 0
    DELETE
  ENDSCAN
  SET DELETED ON
ENDIF
*wab
SELECT INVLINE
IF SEEK(lcSource)
  SET DELETED OFF
  SCAN REST WHILE style+invoice+STR(lineno,6)=lcSource
    WAIT WINDOW "STYLE :- " + STYLE NOWAIT  
    SCATTER MEMVAR MEMO
    M.STYLE     = lcTarget  
    M.DESC1     = lcTrgDsc
    M.SCALE     = lcScale
    M.cWareCode = lcScndWare
    IF !SEEK( M.STYLE + lcScndWare + SPACE(10),'STYDYE')
      DO gpAdStyWar WITH M.STYLE,SPACE(10),lcScndWare
    ENDIF
    INSERT INTO (lcInvoice) FROM MEMVAR
    DELETE
  ENDSCAN
  SET DELETED ON
ENDIF

*--replace in consinvl
SELECT CONSINVL
SET ORDER TO TAG ConsStyle
IF SEEK(lcSource)
  SET DELETED OFF
  SCAN REST WHILE STYLE=lcSource
    WAIT WINDOW "STYLE :- " + STYLE NOWAIT  
    SCATTER MEMVAR MEMO
    M.STYLE     = lcTarget  
    M.SCALE     = lcScale
    INSERT INTO (lcConsInv) FROM MEMVAR
    DELETE
  ENDSCAN
  SET DELETED ON
ENDIF
SELECT RALINE
IF SEEK(lcSource)
  SET DELETED OFF
  SCAN REST WHILE style+rano+cra_linno=lcSource
    WAIT WINDOW "STYLE :- " + STYLE NOWAIT  
    SCATTER MEMVAR MEMO
    M.STYLE = lcTarget  
    INSERT INTO (lcRaLines) FROM MEMVAR
    DELETE
  ENDSCAN
  SET DELETED ON
ENDIF
SELECT RETLINE
IF SEEK(lcSource)
  SET DELETED OFF
  SCAN REST WHILE Style+CrMemo=lcSource
    WAIT WINDOW "STYLE :- " + STYLE NOWAIT  
    SCATTER MEMVAR MEMO
    M.STYLE = lcTarget  
    INSERT INTO (lcRetlin) FROM MEMVAR
    DELETE
  ENDSCAN
  SET DELETED ON
ENDIF

SELECT PikLine
SET ORDER TO TAG PikStyle
IF SEEK(lcSource)
  SET DELETED OFF
  SCAN REST WHILE STYLE=lcSource
    WAIT WINDOW "STYLE :- " + STYLE NOWAIT  
    SCATTER MEMVAR MEMO
    M.STYLE     = lcTarget  
    M.DESC1     = lcTrgDsc
    INSERT INTO (lcPikline) FROM MEMVAR
    DELETE
  ENDSCAN
  SET DELETED ON
ENDIF

SELECT Pack_Lin
SET ORDER TO TAG PStyle
IF SEEK(lcSource)
  SET DELETED OFF
  SCAN REST WHILE STYLE=lcSource
    WAIT WINDOW "STYLE :- " + STYLE NOWAIT  
    SCATTER MEMVAR MEMO
    M.STYLE = lcTarget  
    INSERT INTO (lcPacklin) FROM MEMVAR
    DELETE
  ENDSCAN
  SET DELETED ON
ENDIF
SELECT(lcOldArea)
RETURN
*WAB
*!*************************************************************
*! Name      : lfGetQtyCT
*! Developer : AbdElrahim Koura-KAM
*! Date      : 08/03/2001
*! Purpose   : To compute cutticket qty for bom.citemmask means source 
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example            :  =lfRplcInv()
*!*************************************************************
FUNCTION lfGetQtyCT
PARAMETERS lcSource,lcTarget
PRIVATE lcOldArea,lcFlt,llFound
PRIVATE lnQty1,lnQty2,lnQty3,lnQty4,lnQty5,lnQty6,lnQty7,lnQty8,lnTotQty
STORE 0 TO lnQty1,lnQty2,lnQty3,lnQty4,lnQty5,lnQty6,lnQty7,lnQty8,lnTotQty
STORE '' TO lcFlt
STORE .T. TO llFound
lcOldArea=SELECT()
lcStyle = SUBSTR(lcSource,1,lnItmMaj)+SUBSTR(lcSource,lnItmMaj+2,lnItmNonMj)


SELECT CUTTKTL
IF !SEEK(lcStyle)
  SELECT(lcOldArea)
  RETURN
ENDIF

DIME laOpenWip[8]
laOpenWip=0
lcCutTkt=Cuttkt
SCAN REST WHILE CUTTKTL.STYLE+CUTTKTL.COLOR=lcSTyle
  IF CUTTKTH.STATUS $ "OAH"
    IF lcCutTkt <> Cuttkt
      FOR lnCount = 1 TO 8
        lcStr= ALLTRIM(STR(lnCount))
        lnQty&lcStr = lnQty&lcStr + ABS(laOpenWip[lnCount])
        lnTotQty    = lnTotQty + lnQty&lcStr
      ENDFOR
      laOpenWip   = 0
    ENDIF
    FOR lnCount = 1 TO 8
      lcStr= ALLTRIM(STR(lnCount))
      IF trancd = '1'
        laOpenWip[lnCount] = laOpenWip[lnCount] + Qty&lcStr
      ELSE
        laOpenWip[lnCount] = laOpenWip[lnCount] - Qty&lcStr
      ENDIF
    ENDFOR
  ENDIF
ENDSCAN

*B804363,1 WAB (Start)  - no need for this check
*IF CUTTKTH.STATUS $ "OAH"
*B804363,1 WAB (end) 

  FOR lnCount = 1 TO 8 
    lcStr= ALLTRIM(STR(lnCount))
    lnQty&lcStr = lnQty&lcStr + ABS(laOpenWip[lnCount])
    lnTotQty    = lnTotQty + lnQty&lcStr
  ENDFOR

*B804363,1 WAB (Start)
*ENDIF
*B804363,1 WAB (Start)

SELECT(lcTmpAdj)
IF !SEEK(lcTarget)
  APPEND BLANK 
  REPLACE Style      WITH lcTarget   ,;
          cFromWare  WITH lcWare     ,;
          Date       WITH ldDate     ,;
          Type       WITH lcType     ,; 
          Unt_Cost   WITH lnNewCost  ,;
          Old_Cost   WITH lnNewCost  ,;
          TotOld     WITH lnOldStk   ,; 
          dAdd_Date  WITH gdSysDate  ,;
          cAdd_Time  WITH TIME()     ,;
          cAdd_User  WITH gcUser_id  ,;
          GlFYear    WITH ALLTRIM(STR(YEAR(gdSysDate)))  ,;
          GlPeriod   WITH PADL(ALLTRIM(STR(MONTH(gdSysDate))),2,'0')
ENDIF           && !SEEK(lcTarget)
REPLACE Adj1       WITH Adj1+lnQty1     ,;
        Adj2       WITH Adj2+lnQty2     ,;
        Adj3       WITH Adj3+lnQty3     ,;
        Adj4       WITH Adj4+lnQty4     ,;
        Adj5       WITH Adj5+lnQty5     ,;
        Adj6       WITH Adj6+lnQty6     ,;
        Adj7       WITH Adj7+lnQty7     ,;
        Adj8       WITH Adj8+lnQty8     ,;                                                            
        TotAdj     WITH Adj1+Adj2+Adj3+Adj4+Adj5+Adj6+Adj7+Adj8 
SELECT(lcOldArea)
RETURN
*!*************************************************************
*! Name      : lfCreatFil
*! Developer : AbdElrahim Koura-KAM
*! Date      : 08/03/2001
*! Purpose   : To create tmp adj file
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example            :  =lfCreatFil()
*!*************************************************************
FUNCTION lfCreatFil
PRIVATE lcOldArea
lcOldArea=SELECT()
SELECT INVTADJ
=AFIELDS(laFStru)
lnNo1=ASCAN(laFStru,'UNT_COST')
lnNo2=ASCAN(laFStru,'OLD_COST')
*--Make the lenth of this two fields as ave_cost field.
STORE 15 TO laFStru(lnNo1+2),laFStru(lnNo2+2)
STORE  7 TO laFStru(lnNo1+3),laFStru(lnNo2+3)
lnFStru = ALEN(laFStru,1)
DIMENSION laFStru[lnFStru+2,4]
laFStru[lnFStru+1,1] = 'cAdjReason'
laFStru[lnFStru+1,2] = 'C'
laFStru[lnFStru+1,3] = 6
laFStru[lnFStru+1,4] = 0
laFStru[lnFStru+2,1] = 'cRefer'
laFStru[lnFStru+2,2] = 'C'
laFStru[lnFStru+2,3] = 6
laFStru[lnFStru+2,4] = 0
CREATE DBF (gcWorkDir+lcTmpAdj) FROM ARRAY laFStru
lcExpr=IIF(llDyelot,'Style+Dyelot+STR(RECNO(),6)','Style+STR(RECNO(),6)')
INDEX ON &lcExpr TAG &lcTmpAdj
SET RELATION TO Style INTO STYLE ADDITIVE
CREATE CURSOR (lcTmpMjr) (cSTYMAJOR C(19))
INDEX ON cStyMajor TAG "TmpIndx"
SELECT(lcOldArea)
RETURN
*!*************************************************************
*! Name      : lfOpenFil
*! Developer : AbdElrahim Koura-KAM
*! Date      : 08/03/2001
*! Purpose   : To open master files
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example            :  =lfCreatFil()
*!*************************************************************
FUNCTION lfOpenFil

IF !USED('STYLE')
=gfOpenFile(gcDataDir+'STYLE','Style','SH')
ENDIF

IF !USED('BOM')
=gfOpenFile(gcDataDir+'BOM','Bomitem','SH')
ENDIF  
IF !USED('ORDHDR')
  =gfOpenFile(gcDataDir+'OrdHdr','OrdHdr','SH')
ENDIF

IF !USED('ORLINE')
  =gfOpenFile(gcDataDir+'ORDLINE','Ordlines','SH')
ENDIF
SET RELATION TO 'O'+ Ordline.order INTO Ordhdr ADDITIVE

IF !USED('INVLINE')
  =gfOpenFile(gcDataDir+'INVLINE','Invlines','SH')
ENDIF


IF !USED('CutPick')
  IF gfOpenFile(gcDataDir+'CutPick','CutPick','EX')
    ZAP
  ENDIF
ENDIF

SELECT CUTTKTL
SET RELATION TO cuttkt INTO CUTTKTH

IF !USED('INVTADJ')
  =gfOpenFile(gcDataDir+'INVTADJ','Invtadj','SH')
ENDIF

IF !USED('GLDist')
  =gfOpenFile(gcDataDir+'GLDist','GLDistAc','SH')
ENDIF  

IF !USED('StyInvJl') 
  =gfOpenFile(gcDataDir+"StyInvJl","StyInvJl","SH")
ENDIF  

IF !USED('WAREHOUS') 
  =gfOpenFile(gcDataDir+"WAREHOUS","Warehous","SH")
ENDIF  

IF !USED('STYDYE') 
  =gfOpenFile(gcDataDir+"Stydye","Stydye","SH")
ENDIF  

IF !USED('RETLINE') 
  =gfOpenFile(gcDataDir+"RETLINE","Retlines","SH")
ENDIF  

IF !USED('RALINE') 
  =gfOpenFile(gcDataDir+"RALINE","Ralines","SH")
ENDIF  

IF !USED('Ordcanln') 
  =gfOpenFile(gcDataDir+"Ordcanln","Ordcanln","SH")
ENDIF  

IF !USED('ICSTYHST') 
  =gfOpenFile(gcDataDir+"ICSTYHST","Styhst","SH")
ENDIF  

IF !USED('Whsloc') 
  =gfOpenFile(gcDataDir+"Whsloc","Whslocst","SH")
ENDIF  

IF !USED('WIPADJ') 
  =gfOpenFile(gcDataDir+"WIPADJ","","SH")
ENDIF  

IF !USED('Consinvl') 
  =gfOpenFile(gcDataDir+"Consinvl","Consinvl","SH")
ENDIF  

IF !USED('Mdinvntl') 
  =gfOpenFile(gcDataDir+"Mdinvntl","Mdinvntls","SH")
ENDIF  

IF !USED('PIKLINE') 
  =gfOpenFile(gcDataDir+"PIKLINE","Pikline","SH")
ENDIF  

IF !USED('BOMLINE') 
  =gfOpenFile(gcDataDir+"BOMLINE","Bomline","SH")
ENDIF  

IF !USED('CtktBOM') 
  =gfOpenFile(gcDataDir+"CtktBOM","Ctktbom","SH")
ENDIF  

*TMI [START]
IF !USED('PACK_LIN') 
  =gfOpenFile(gcDataDir+"PACK_LIN","","SH")
ENDIF  
*TMI [END]
RETURN
*!*************************************************************
*! Name      : lfClosFil
*! Developer : AbdElrahim Koura-KAM
*! Date      : 08/03/2001
*! Purpose   : To close master files
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example            :  =lfClosFil()
*!*************************************************************
FUNCTION lfClosFil
IF USED('STYLE')
   USE IN STYLE
ENDIF

IF USED('BOM')
  USE IN BOM
ENDIF  

IF USED('ORLINE')
  USE IN ORLINE
ENDIF

IF USED('INVLINE')
  USE IN INVLINE
ENDIF

IF USED('CUTTKTH')
  USE IN CUTTKTH
ENDIF

SELECT CUTTKTL
SET RELATION TO 

IF USED('CUTTKTL')
  USE IN CUTTKTL
ENDIF


IF USED('INVTADJ')
  USE IN INVTADJ
ENDIF

IF USED('GLDist')
  USE IN GLDist
ENDIF  

IF USED('StyInvJl') 
  USE IN StyInvJl
ENDIF  

IF USED('WAREHOUS') 
  USE IN  WAREHOUS
ENDIF  

IF USED('STYDYE') 
  USE IN STYDYE
ENDIF  

IF USED('RETLINE') 
  USE IN RETLINE
ENDIF  

IF USED('RALINE') 
  USE IN RALINE
ENDIF  

IF USED('Ordcanln') 
  USE IN Ordcanln
ENDIF  

IF USED('ICSTYHST') 
  USE IN ICSTYHST
ENDIF  

IF USED('Whsloc') 
  USE IN Whsloc
ENDIF  

IF USED('WIPADJ') 
  USE IN WIPADJ
ENDIF  

IF USED('Consinvl') 
  USE IN Consinvl
ENDIF  

IF USED('Mdinvntl') 
  USE IN Mdinvntl
ENDIF  

IF USED('PIKLINE') 
  USE IN PIKLINE
ENDIF  

IF USED('Bomline') 
  USE IN Bomline
ENDIF  

IF USED('Ctktbom') 
  USE IN Ctktbom
ENDIF  

RETURN 

*!*************************************************************
*! Name      : lfGetType
*! Developer : AbdElrahim Koura-KAM
*! Date      : 08/03/2001
*! Purpose   : To get type for MF
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example            :  =lfGetType()
*!*************************************************************
FUNCTION lfGetType
PRIVATE lcPType1,lcPType2,lcPType3,lcPType4,lcPType5,lnAryCount
STORE '' TO lcPType1,lcPType2,lcPType3,lcPType4,lcPType5
STORE 0 To lnAryCount
laSetupP[1,1]  = 'M_cIType1'
laSetupP[2,1]  = 'M_cIType2'
laSetupP[3,1]  = 'M_cIType3'
laSetupP[4,1]  = 'M_cIType4'
laSetupP[5,1]  = 'M_cIType5'
*-- Use gfGetMemVar once per all variables.
=gfGetMemVar(@laSetupP, gcAct_Comp)  && get array for PO
*--lasetup for PO
lcPType1   = laSetupP[1,2]
lcPType2   = laSetupP[2,2]
lcPType3   = laSetupP[3,2]
lcPType4   = laSetupP[4,2]
lcPType5   = laSetupP[5,2]
FOR lnCheck = 1 TO 5
   lcStyCmpP  = "lcPType" + STR(lnCheck,1)
   IF EVAL(lcStyCmpP) = 'S'  
     IF EMPTY(laStyP[1])
       laStyp[1] = STR(lnCheck,1)
    ELSE
       lnAryCount=ALEN(laStyP,1)   
       DIMENSION laStyP[lnAryCount+1]
       laStyp[lnAryCount+1] = STR(lnCheck,1)
    ENDIF
  ENDIF
ENDFOR
RETURN 
*!*************************************************************
*! Name      : lfGetItmD
*! Developer : AbdElrahim Koura-KAM
*! Date      : 08/03/2001
*! Purpose   : To get item for deletion
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example            :  =lfGetItmD()
*!*************************************************************
FUNCTION lfGetItmD
PARAMETERS lcItemMjor
PRIVATE lcOldArea,lcMajor
lcOldArea=SELECT()
lcMajor  =SUBSTR(lcItemMjor,1,lnItmMaj)
lcMajor  =PADR(lcMajor,19)
SELECT(lcTmpMjr)
IF !SEEK(lcMajor)
  INSERT INTO (lcTmpMjr)(cSTYMAJOR) VALUES (lcMajor)
ENDIF
SELECT(lcOldArea)
RETURN
*!*************************************************************
*! Name      : lfdelall
*! Developer : AbdElrahim Koura-KAM
*! Date      : 08/03/2001
*! Purpose   : To delete item(s) from all file(s)
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example            :  =lfdelall()
*!*************************************************************
FUNCTION lfdelall
PRIVATE lcItem
SELECT(lcTmpMjr)
*wab
SCAN
  lcItem  =cSTYMAJOR
  lcStyMaj=cSTYMAJOR
  
  SELECT ICSTYHST
  IF SEEK(ALLTRIM(&lcTmpMjr..cSTYMAJOR))
    WAIT WINDOW "Delete form IcStyHst.dbf" NOWAIT
    DELETE REST WHILE SUBSTR(STYLE,1,lnItmMaj)=PADR(&lcTmpMjr..cSTYMAJOR,lnItmMaj)
  ENDIF 

  SELECT StyInvJl
  IF SEEK(ALLTRIM(&lcTmpMjr..cSTYMAJOR))
    WAIT WINDOW "Delete form StyInvJl.dbf" NOWAIT 
    DELETE REST WHILE SUBSTR(STYLE,1,lnItmMaj)=PADR(&lcTmpMjr..cSTYMAJOR,lnItmMaj)
  ENDIF

  SELECT STYLE
  SET ORDER TO TAG STYLE 
  IF SEEK(ALLTRIM(&lcTmpMjr..cSTYMAJOR))
    WAIT WINDOW "Delete form Style.dbf" NOWAIT
    DELETE REST WHILE SUBSTR(STYLE,1,lnItmMaj)=PADR(&lcTmpMjr..cSTYMAJOR,lnItmMaj)
  ENDIF

  *wab need to delete from stydye
  SELECT STYDYE
  IF SEEK(ALLTRIM(&lcTmpMjr..cSTYMAJOR))
    WAIT WINDOW "Delete form Stydye.dbf" NOWAIT
    DELETE REST WHILE SUBSTR(STYLE,1,lnItmMaj)=PADR(&lcTmpMjr..cSTYMAJOR,lnItmMaj)
  ENDIF
  
  SELECT BOMLINE
  SET ORDER TO TAG BOMSTYLE
  IF SEEK(ALLTRIM(&lcTmpMjr..cSTYMAJOR))
    WAIT WINDOW "Delete form BOMLine.dbf" NOWAIT
    DELETE REST WHILE SUBSTR(STYLE,1,lnItmMaj)=PADR(&lcTmpMjr..cSTYMAJOR,lnItmMaj)
  ENDIF

  SELECT BOM  
  SET ORDER TO TAG Bom
  IF SEEK(ALLTRIM(&lcTmpMjr..cSTYMAJOR))
    WAIT WINDOW "Delete form BOM.dbf" NOWAIT
    DELETE REST WHILE SUBSTR(citmmajor,1,lnItmMaj)=PADR(&lcTmpMjr..cSTYMAJOR,lnItmMaj)
  ENDIF
  SELECT CUTTKTH
  WAIT WINDOW "Delete form CtktBOM.dbf" NOWAIT
  IF SEEK(ALLTRIM(lcStyMaj))
    SCAN REST WHILE SUBSTR(Style,1,lnItmMaj)=PADR(&lcTmpMjr..cSTYMAJOR,lnItmMaj)
      SELECT CtktBOM
      IF SEEK('M'+CUTTKTH.cuttkt)
        DELETE REST WHILE cimtyp+cuttkt+typ+item+iclr+mfgcode+dyelot='M'+CUTTKTH.cuttkt
      ENDIF 
    ENDSCAN
  ENDIF
ENDSCAN
RETURN 
*!*************************************************************
*! Name      : lfvGetDir
*: Date      : 15/05/2001
*: Developer : Ahmed Maher (AMH)
*! Purpose   : To allow the user to select company directory.
*!*************************************************************
*! Example            : =lfvGetDir()
*!*************************************************************
*
FUNCTION lfvGetDir

lcSorcCo  = GETDIR('','Select source company')
SHOW GET lcSorcCo
IF EMPTY(lcSorcCo)
  SHOW GET pbOK DISABLE
ELSE
  SHOW GET pbOK ENABLE
ENDIF
*-- end of lfvGetDir.

*!*************************************************************
*! Name      : lfvCancel
*: Date      : 15/05/2001
*: Developer : Ahmed Maher (AMH)
*! Purpose   : Valid function of the cancel button.
*!*************************************************************
*! Example            : =lfvCancel()
*!*************************************************************
*
FUNCTION lfvCancel

CLEAR READ
*-- end of lfvCancel.
*!*************************************************************
*! Name      : lfGetTF
*! Developer : AbdElrahim Koura-KAM
*! Date      : 08/03/2001
*! Purpose   : To get temp files to improve performance
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example            :  =lfGetTF()
*!*************************************************************
FUNCTION lfGetTF
SELECT ORDLINE
COPY STRUCT TO (gcWorkDir+lcOrder)
=gfOpenFile(gcWorkDir+lcOrder,'','SH')
SELECT INVLINE
COPY STRUCT TO (gcWorkDir+lcInvoice)
=gfOpenFile(gcWorkDir+lcInvoice,'','SH')

*tmi [START] create temp file for Raline,Retline,PikLine,Pack_Lin
SELECT RALINE
COPY STRUCT TO (gcWorkDir+lcRaLines)
=gfOpenFile(gcWorkDir+lcRaLines,'','SH')

SELECT RETLINE
COPY STRUCT TO (gcWorkDir+lcRetlin)
=gfOpenFile(gcWorkDir+lcRetlin,'','SH')

SELECT PIKLINE
COPY STRUCT TO (gcWorkDir+lcPikline)
=gfOpenFile(gcWorkDir+lcPikline,'','SH')

SELECT PACK_LIN
COPY STRUCT TO (gcWorkDir+lcPacklin)
=gfOpenFile(gcWorkDir+lcPacklin,'','SH')

SELECT Consinvl
COPY STRUCT TO (gcWorkDir+lcConsInv)
=gfOpenFile(gcWorkDir+lcConsInv,'','SH')

*tmi [END  ] 
RETURN

*!*************************************************************
*! Name      : lfUpdOrdl
*! Developer : AbdElrahim Koura-KAM
*! Date      : 08/03/2001
*! Purpose   : To update ordline file
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example            :  =lfUpdOrdl()
*!*************************************************************
FUNCTION lfUpdOrdl
SELECT (lcOrder)
SCAN
  WAIT WINDOW "STYLE :- " + STYLE NOWAIT
  SCATTER MEMVAR MEMO
  INSERT INTO "ORDLINE" FROM MEMVAR
ENDSCAN

SELECT (lcInvoice)
SCAN 
  WAIT WINDOW "STYLE :- " + STYLE NOWAIT
  SCATTER MEMVAR 
  INSERT INTO "INVLINE" FROM MEMVAR
ENDSCAN

SELECT (lcRaLines)
SCAN
  WAIT WINDOW "STYLE :- " + STYLE NOWAIT
  SCATTER MEMVAR MEMO
  INSERT INTO "RALINE" FROM MEMVAR
ENDSCAN

SELECT (lcRetlin)
SCAN
  WAIT WINDOW "STYLE :- " + STYLE NOWAIT
  SCATTER MEMVAR MEMO
  INSERT INTO "RETLINE" FROM MEMVAR
ENDSCAN

SELECT (lcConsInv)
SCAN
  WAIT WINDOW "STYLE :- " + STYLE NOWAIT
  SCATTER MEMVAR MEMO
  INSERT INTO "CONSINVL" FROM MEMVAR
ENDSCAN

SELECT (lcPikline)
SCAN
  WAIT WINDOW "STYLE :- " + STYLE NOWAIT
  SCATTER MEMVAR MEMO
  INSERT INTO "PIKLINE" FROM MEMVAR
ENDSCAN

SELECT (lcPacklin)
SCAN
  WAIT WINDOW "STYLE :- " + STYLE NOWAIT
  SCATTER MEMVAR MEMO
  INSERT INTO "PACK_LIN" FROM MEMVAR
ENDSCAN

RETURN


*!*************************************************************
*! Name      : lfClosTF
*! Developer : AbdElrahim Koura-KAM
*! Date      : 08/03/2001
*! Purpose   : To delete temp files(s)
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example            :  =lfClosTF()
*!*************************************************************
FUNCTION lfClosTF
IF USED(lcOrder)
 USE IN (lcOrder) 
 ERASE (gcWorkDir+lcOrder+".DBF")
 ERASE (gcWorkDir+lcOrder+".CDX")
 ERASE (gcWorkDir+lcOrder+".FPT")
ENDIF

IF USED(lcInvoice)
 USE IN (lcInvoice) 
 ERASE (gcWorkDir+lcInvoice+".DBF")
 ERASE (gcWorkDir+lcInvoice+".CDX")
 ERASE (gcWorkDir+lcInvoice+".FPT")
ENDIF


IF USED(lcRaLines)
 USE IN (lcRaLines)
 ERASE (gcWorkDir+lcRaLines+".DBF")
 ERASE (gcWorkDir+lcRaLines+".CDX")
 ERASE (gcWorkDir+lcRaLines+".FPT")
ENDIF

IF USED(lcRetlin)
 USE IN (lcRetlin)
 ERASE (gcWorkDir+lcRetlin+".DBF")
 ERASE (gcWorkDir+lcRetlin+".CDX")
 ERASE (gcWorkDir+lcRetlin+".FPT")
ENDIF

IF USED(lcPikline)
 USE IN (lcPikline) 
 ERASE (gcWorkDir+lcPikline+".DBF")
 ERASE (gcWorkDir+lcPikline+".CDX")
 ERASE (gcWorkDir+lcPikline+".FPT")
ENDIF

IF USED(lcPacklin)
 USE IN (lcPacklin)
 ERASE (gcWorkDir+lcPacklin+".DBF")
 ERASE (gcWorkDir+lcPacklin+".CDX")
 ERASE (gcWorkDir+lcPacklin+".FPT")
ENDIF

IF USED(lcConsInv)
 USE IN (lcConsInv)
 ERASE (gcWorkDir+lcConsInv+".DBF")
 ERASE (gcWorkDir+lcConsInv+".CDX")
 ERASE (gcWorkDir+lcConsInv+".FPT")
ENDIF

RETURN







