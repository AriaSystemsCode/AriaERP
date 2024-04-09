****************************************************************************************************
*T20140424.0006 Sharon /In company J1, we need to implement the options for multiple size scales on a style 
* Convert the Co. J1 of Sharon Young to Extended 
*Sharon Young Data conversion program
* Developer : Tarek Mohamed Ibrahim 
*- 6/22/2014, the base scale will be that of the SCALE file, the replaced on got from the STY_exc file should be added manually to the SCALE screen after the conversion  
*-            in the J1 scale file, I changed PPB of the scale H to TTB
*-            the made the scale PAB to 

*- How to get the entiry files
*- you can obtain the style STY_exc downloading the file
*                            06/18/2014 05:29 PM	Garcia, Mary (Sharon Young, Inc)	 	 REVISED Style list with new scale
*  save it in excel 95 format , and imprt it to the file STY_EXC
*- fldmaplist IS a copy of the file SYDFDCHG with STYLE lines not handled by the program are kept
*- J1 : 
*- download the file
*                            06/18/2014 05:29 PM	Garcia, Mary (Sharon Young, Inc)	 	 REVISED - list of scale codes
*- save it in excel 95 and import it to J1
****************************************************************************************************
PARAMETERS lcCoID
lcCoID = IIF(EMPTY(lcCoID),'  ',lcCoID)
lnCnt = 0

lnStart = DATETIME() 
?lnStart 

CLOSE DATABASES 

lnAbort = 0

SET EXCLUSIVE OFF
SET DELETED ON 
SET CPDIALOG OFF
SET SAFETY OFF

IF EMPTY(lcCoID)
  MESSAGEBOX('Pls specifiy the Co. id as a prarmeter to the program')
  *T20140424.0006 TMI 07/27/2014 22:56 [Start] 
  MESSAGEBOX('Useage , from the vfp9 command window write : DO ExtConv with "XX", where xx is the Co. ID intended to be converted.',0,'Aria support')
  *T20140424.0006 TMI 07/27/2014 22:56 [End  ] 
  RETURN 
ELSE
  lcMsg = 'convert the Co. &lcCoID to be Extended'
  MESSAGEBOX('Pls confirm that you will '+lcMsg)
  IF MESSAGEBOX('Are you sure you want to '+lcMsg,4+256)<>6
    RETURN 
  ENDIF 
ENDIF 

lcSyspath = GETDIR('','Sysfiles Path')
IF EMPTY(lcSyspath) OR !FILE(lcSyspath+'SYDFDCHG.dbf')
  MESSAGEBOX('invalid sysfiles path')
  RETURN 
ENDIF 
 
*- connect to sql server database
SELECT 0
lnSqlConn = 0
USE (lcSyspath+'syccomp') ORDER 1
IF SEEK( lcCoID )  
  WAIT WINDOW NOWAIT 'CONNECT TO THE SQL DATABASE'
  lnSqlConn = SQLSTRINGCONNECT('driver=sql server;server='+ALLTRIM(syccomp.cconserver)+';database='+ALLTRIM(syccomp.ccondbname)+';uid='+ALLTRIM(syccomp.cconuserid)+';pwd='+ALLTRIM(syccomp.cconpaswrd)+';')
  IF lnSqlConn<0
    MESSAGEBOX('unable to connect to the sql database')
    RETURN 
  ENDIF 
ELSE
  MESSAGEBOX('wrong Co. ID passed')
  RETURN 
ENDIF 

lcStyleDataPath = ADDBS(ALLTRIM(SYCCOMP.CCOM_DDIR))

*- "Updating Scale File"
IF !FILE(lcStyleDataPath+'xls_scale.xls')
  MESSAGEBOX('Can not find the mapping file xls_scale.xls, get this file by importing the mapping SCALE excel file',0,'process will be aborted')
  RETURN
ENDIF 

SELECT 0
CD &lcStyleDataPath
llErr = .F.
ON ERROR llErr = .T.
IMPORT from xls_scale.xls TYPE xl5
IF llErr
  MESSAGEBOX('error while importing file xls_scale')
  RETURN 
ENDIF 
IF !FILE(lcStyleDataPath+'FldMapList.DBF')
  MESSAGEBOX('Can not find the field mapping file FldMapList',0,'process will be aborted')
  RETURN
ENDIF 

IF !FILE(lcStyleDataPath+'STY_EXC.xls')
  MESSAGEBOX('Can not find the STY_EXC file for the excluded style list with the shift applied')
  RETURN
ENDIF 
SELECT 0
IMPORT from sty_exc.xls TYPE xl5
IF llErr
  MESSAGEBOX('error while importing file xls_scale')
  RETURN   
ENDIF 

ON ERROR

*USE (lcStyleDataPath+'STY_EXC') IN 0 EXCLUSIVE 
SELECT STY_EXC
INDEX on PADR(B,19) TAG B

USE &lcSyspath.SYDFILES IN 0 ORDER CFILE_NAM   && CFILE_NAM
USE &lcSyspath.SYDFLFLD IN 0 ORDER CFLD_NAME   && CFLD_NAME+STR(NFLD_POS)
*T20140424.0006 TMI 07/27/2014 14:11 [Start] used to get fields defined, based on it we can do shift action
*!*	USE &lcSyspath.SYDFIELD IN 0 
*!*	SELECT SYDFIELD
*!*	SET FILTER TO '8'$CFLD_NAME AND cdata_typ = 'N'
*!*	LOCATE 
*T20140424.0006 TMI 07/27/2014 14:11 [End  ] 

USE lcStyleDataPath+"Setups.DBF" SHARED IN 0
SELECT Setups
SET ORDER TO VARNAME   && CFLD_NAME
IF Seek('M_USEEXSSC')
  REPLACE mdata_def  WITH ".T."
ENDIF 

IF Seek('M_EXTWIDTH')
  REPLACE mdata_def  WITH "2"
ENDIF 

SELECT Setups
USE  

USE lcStyleDataPath+"ICISTRU.DBF" SHARED IN 0
SELECT ICISTRU
SET ORDER TO SEGNO   && CITEMRECTY+CISEGNO
IF SEEK("U")
  SCAN REST WHILE CITEMRECTY+CISEGNO = 'U'
    IF ICISTRU.cisegno = "1"
      *T20140424.0006,3, style structure will be 9-6-3, with only one dash
      *REPLACE nisegsize WITH 8,;
              ciseghead WITH 'Style   -Color -Scl'
      REPLACE nisegsize WITH 9,;
              ciseghead WITH 'Style   -Color -Scl'
      *T20140424.0006,3, style structure will be 9-6-3, with only one dash
      LOOP 
    ENDIF 
    *tmi T20140424.0006,3, style structure will be 9-6-3, with only one dash
    *IF ICISTRU.cisegno = "2"      
    *  REPLACE cisegsepr  WITH '-'      
    *ENDIF 
    *tmi
  ENDSCAN 
  SELECT ICISTRU
  =SEEK("U")
  LOCATE REST WHILE CITEMRECTY+CISEGNO = 'U' FOR ICISTRU.cisegno = "3"
  IF !Found()
    APPEND BLANK 
    REPLACE citemrecty WITH  'U',; 
            cisegno    WITH "3",;
            nisegsize  WITH 3 ,;
            cisegsdes  WITH 'Sca',;
            cisegldes  WITH  'Scale',;
            cisegsepr  WITH  '',;
            cisegtype  WITH  'S',;
            ciseghead  WITH  '',;
            lsegendmaj WITH .F.
  ENDIF 
ENDIF 
SELECT ICISTRU
USE 

*USE lcStyleDataPath+'xls_scale.DBF' EXCL IN 0 
USE lcStyleDataPath+"Scale.DBF" SHARED IN 0
USE lcStyleDataPath+"SCALEHD.DBF" SHARED IN 0

*- Updating the scale file from the xls_scale mapping file
SELECT xls_scale
INDEX ON A TAG A
SCAN
  IF SEEK('S'+SUBSTR(xls_scale.a,1,3),'SCALE','SCALE')
    SELECT SCALE
    REPLACE SCALE WITH xls_scale.B
  ENDIF 
  *T20140424.0006 TMI 07/27/2014 11:58 [Start] add the scales KKA,C, PAB
  IF INLIST(ALLTRIM(xls_scale.B) , 'KKA' , 'KKC' , 'PAB' ) AND !SEEK('S'+ALLTRIM(xls_scale.B) ,'SCALE','SCALE')
    SCATTER MEMVAR
    lnCnt = IIF(EMPTY(xls_scale.c),0,1)+;
            IIF(EMPTY(xls_scale.d),0,1)+;
            IIF(EMPTY(xls_scale.e),0,1)+;
            IIF(EMPTY(xls_scale.f),0,1)+;
            IIF(EMPTY(xls_scale.g),0,1)+;
            IIF(EMPTY(xls_scale.h),0,1)+;
            IIF(EMPTY(xls_scale.i),0,1)+;
            IIF(EMPTY(xls_scale.j),0,1)
    SELECT SCALE
    APPEND BLANK
    REPLACE TYPE      WITH 'S';
            SCALE     WITH ALLTRIM(xls_scale.B) ;
            PREPAK    WITH '' ;
            CSCL_DESC WITH xls_scale.K ;
            CDIM1     WITH  '' ;
            CDIM2     WITH  '' ;
            CNT       WITH  lnCnt ;
            SZ1       WITH  PADC(ALLTRIM(xls_scale.C),5) ;
            SZ2       WITH  PADC(ALLTRIM(xls_scale.D),5) ;
            SZ3       WITH  PADC(ALLTRIM(xls_scale.E),5) ;
            SZ4       WITH  PADC(ALLTRIM(xls_scale.F),5) ;
            SZ5       WITH  PADC(ALLTRIM(xls_scale.G),5) ;
            SZ6       WITH  PADC(ALLTRIM(xls_scale.H),5) ;
            SZ7       WITH  PADC(ALLTRIM(xls_scale.I),5) ;
            SZ8       WITH  PADC(ALLTRIM(xls_scale.J),5) ;
            CADD_USER WITH  'TMI@ARIA4';
            CADD_TIME WITH  TIME() ;
            DADD_DATE WITH  DATE() ;
            CADD_VER  WITH 'A40'
  ENDIF 
  *T20140424.0006 TMI 07/27/2014 11:58 [End  ]   
ENDSCAN 


*- Add required lines to the SCALEHD file
WAIT WINDOW "Updating Scale File" NOWAIT 
SELECT scale 
SET ORDER TO SCALE   && TYPE+SCALE+PREPAK
LOCATE 
=Seek("S")
SCAN REST WHILE TYPE+SCALE+PREPAK = "S"
  SELECT SCALEHD
  APPEND BLANK 
  REPLACE SCALEHD.cextscale  WITH Scale.Scale,;
  		  SCALEHD.cscaledes  WITH scale.cscl_desc ,;
  		  SCALEHD.nnoofdim   WITH 1,;
  		  SCALEHD.cdim1desc  WITH "DIM1",;
  		  SCALEHD.nnofcodes  WITH 1
  *SELECT scale  		  
  *REPLACE Scale.Scale WITH  ALLTRIM(Scale.Scale)+ALLTRIM(Scale.Scale)+"1"
ENDSCAN  

SELECT Scale 
USE 
SELECT SCALEHD
USE 

WAIT WINDOW "Updating Style File" NOWAIT 

DIMENSION laChangedStyle[1,2]
STORE '' to laChangedStyle

 
  
CREATE TABLE (lcStyleDataPath+'OldNewStyle') (OldStyle C(19),NewStyle C(19),OldStyMaj C(19),NewStyMaj C(19),oldScale c(3),newScale c(3)) 
SELECT 'OldNewStyle' 
INDEX on OldStyle  TAG 'OldStyle' 
INDEX on OldStyMaj TAG 'OldStyMaj' 

USE lcStyleDataPath+"Style.DBF" SHARED IN 0
SELECT Style 
SET ORDER TO Style 
LOCATE 
SCAN
  lcOldStyle = Style.Style
  lcOldStyMaj = Style.CstyMajor
  lcNewStyMajor = ''
  *lnSepPos = AT('-',Style.Style)
  lcStyMajor = SUBSTR(Style.Style,1,12)
  lcColor    = PADR(SUBSTR(Style.Style,14,6),6)
  =SEEK(STYLE.SCALE,'xls_scale')
  
  *- 6/22/2014, the base scale will be that of the SCALE file, the replaced on got from the STY_exc file should be added manually to the SCALE screen after the conversion  
  lcScale    = IIF(SEEK(Style.Style,'STY_EXC'),STY_EXC.D,xls_scale.B)
  
  IF !SEEK(PADR(lcOldStyMaj,19),'OldNewStyle' ,'OldStyMaj' )
    IF LEN(RTRIM(lcStyMajor)) > 9
      lnCon = 1
      *tmi
      * Style major will be 9, there are 33 lines in the style file with major lenght is 10, 
      * these styles seem same copy of other styles with character 'P' attached to it
      * As example for the style DM1504P50, there is another one DM1504P50P
      * for DM1504P50P I'll remove the character before the 'P' and move 'P' to the left so it will be
      * DM1504P5P instead of 
      * DM1504P50P
      *
      *FOR lnI = 1 TO 99
      *  IF ASCAN(laChangedStyle,SUBSTR(lcStyMajor,1,5)+"_"+PADL(ALLTRIM(STR(lnI)) ,2,"0")) = 0  
      *    lnCon = lnI
      *    EXIT 
      *  ENDIF 
      *ENDFOR 
      **
      *lcNewStyMajor = SUBSTR(lcStyMajor,1,5)+"_"+PADL(ALLTRIM(STR(lnCon)) ,2,"0")
      lcNewStyMajor = SUBSTR(lcStyMajor,1,8)+SUBSTR(lcStyMajor,10,1)
      *tmi
    
      *tmi : no need for this array 
      *IF EMPTY(laChangedStyle[1,1])
      *  laChangedStyle[1,1] = lcStyMajor
      *  laChangedStyle[1,2] = lcNewStyMajor
      *ELSE
      *  DIMENSION laChangedStyle[ALEN(laChangedStyle,1)+1,2]
      *  laChangedStyle[ALEN(laChangedStyle,1),1] = lcStyMajor
      *  laChangedStyle[ALEN(laChangedStyle,1),2] = lcNewStyMajor
      *ENDIF 
      *tmi
    ELSE
      lcNewStyMajor =  RTRIM(lcStyMajor)
    ENDIF 
  ELSE
    lcNewStyMajor = RTRIM(OldNewStyle.NewStyMaj) 
  ENDIF  
  *tmi
  *lcNewStyle = PADR(lcNewStyMajor ,8)+'-'+lcColor+'-'+lcScale 
  lcNewStyle = PADR(lcNewStyMajor ,9)+'-'+lcColor+lcScale 
  *tmi
  WAIT WINDOW NOWAIT lcNewStyle
  INSERT INTO 'OldNewStyle' (OldStyle ,NewStyle ,OldStyMaj ,NewStyMaj ,oldScale,newScale) ;
     					VALUES (lcOldStyle ,lcNewStyle ,lcOldStyMaj ,lcNewStyMajor , xls_scale.A,lcScale)
ENDSCAN 
WAIT CLEAR 


*- update style file
SELECT Style
SET ORDER TO 
LOCATE 
SCAN
  WAIT WINDOW NOWAIT Style.Style 	
  IF SEEK(Style.Style,'OldNewStyle' ,'OldStyle')
    *T20140424.0006 TMI 08/03/2014 12:48 [Start] if this style width is 10 then place the original name in the cowner field
    IF LEN(ALLTRIM(cstymajor))=10
      replace cowner WITH cstymajor
    ENDIF 
    *T20140424.0006 TMI 08/03/2014 12:48 [End  ] 
    REPLACE Style.Style 	WITH OldNewStyle.NewStyle ,;
    		Style.cStyMajor WITH OldNewStyle.NewStyMaj,;
    		Style.Scale 	WITH SUBSTR(OldNewStyle.NewStyle,17,3)
  ENDIF   
ENDSCAN 

SELECT Style 
USE 

*IF SYS(0)='SDE_DEV8_SABER # Saber'
*RETURN 
*ENDIF 

*- updating other files
*- start with files Mariam updated specially at Kazu
=lfSqlFls()

*- files with STYLE field
SELECT SYDFLFLD
=SEEK('STYLE ')

SCAN REST WHILE cFld_Name = 'STYLE '
  IF PADR(CFILE_NAM,8) $ 'STYLE   ,RETLINE ,RALINE  ,POSLN   ,POSHDR  ,MFGOPRDT,ITMPRICE,ICITMHST,DYE_REL ,CUTPICK ,CTKTBOM ,BOMLINE ,BOMHEADR,BOMCOST ,BOM     '
    LOOP   && Style file handled above, other files are Sql and handled in 
  ENDIF 
  *-- Open necessary files
  =SEEK(SYDFLFLD.cfile_nam,'SYDFILES','CFILE_NAM')   && CFILE_NAM
  IF !lfOpClFile(ALLTRIM(SYDFILES.cFile_Nam),ALLTRIM(SYDFILES.cfile_tag))
    LOOP
  ENDIF
  WAIT WINDOW 'Updating ' + ALLTRIM(SYDFILES.cFile_Nam) + ' file.' NOWAIT
          
  *-- Replace Old value by new value                        
  IF SYDFILES.CVER = 'A27'              

     =lfUpdA27('STYLE')

  ELSE                          
      
    =lfUpdSqlFls('STYLE')
        
  ENDIF         
  
        
  WAIT CLEAR 
ENDSCAN
    
*- files with CSTYMAJOR field, not the STYLE file
SELECT SYDFLFLD
=SEEK('CSTYMAJOR')
SCAN REST WHILE CFLD_NAME+STR(NFLD_POS) = 'CSTYMAJOR' FOR UPPER(SYDFLFLD.cfile_nam) <> 'STYLE'

  =SEEK(SYDFLFLD.cfile_nam,'SYDFILES','CFILE_NAM')   && CFILE_NAM
  IF !lfOpClFile(ALLTRIM(SYDFILES.cFile_Nam),ALLTRIM(SYDFILES.cfile_tag))
    LOOP
  ENDIF
  
  IF SYDFILES.CVER = 'A27'              

    SELECT (ALLTRIM(SYDFLFLD.cfile_nam))
    LOCATE 
    SCAN       
       *T20140424.0006 TMI 07/27/2014 21:50 [Start] 
       IF lfAbort()
         EXIT
       ENDIF
       *T20140424.0006 TMI 07/27/2014 21:50 [End  ]  
      IF SEEK(CSTYMAJOR,'OldNewStyle' ,'OldStyMaj')
        REPLACE CSTYMAJOR	WITH OldNewStyle.NewStyMaj 
      ENDIF
    ENDSCAN
    USE IN (ALLTRIM(SYDFLFLD.cfile_nam))   
    
  ELSE
  
    lcFl = ALLTRIM(SYDFILES.cFile_Nam)
    lnSl = SQLEXEC(lnSqlConn,"SELECT * FROM &lcFl",lcFl)

    SELECT &lcFl
    SCAN  
       *T20140424.0006 TMI 07/27/2014 21:50 [Start] 
       IF lfAbort()
         EXIT
       ENDIF 
       *T20140424.0006 TMI 07/27/2014 21:50 [End  ]  
      IF SEEK(CSTYMAJOR,'OldNewStyle' ,'OldStyMaj')

        lcSQL = "UPDATE "+ALLTRIM(SYDFILES.cFile_Nam)+" "+;
              "SET CSTYMAJOR = '"+OldNewStyle.NewStyMaj +"' " +;
              "WHERE CSTYMAJOR= '"+OldNewStyle.OldStyMaj+"'"
        lnResu = SQLEXEC(lnSqlConn,lcSQL)                
        IF lnResu<0
          STRTOFILE(SYDFILES.cFile_Nam+':'+style,lcStyleDataPath+'errfile.txt',1)
        ENDIF 
      ENDIF 
    ENDSCAN    

  ENDIF 
ENDSCAN   

*- loop over files in the LIST where styles are saved in fields other than the STYLE one 
*  the file FldMapList is a copy of SYDFDCHG with files treated by sql fn removed 
USE (lcStyleDataPath+'FldMapList') IN 0 ALIAS SYDFDCHG
SELECT SYDFDCHG
*-- Open necessary files
SCAN     
  =SEEK(SYDFDCHG.cfile_nam,'SYDFILES','CFILE_NAM')   && CFILE_NAM
  IF !lfOpClFile(ALLTRIM(SYDFDCHG.cFile_Nam),ALLTRIM(SYDFDCHG.cBaseTag))
    LOOP
  ENDIF
  WAIT WINDOW 'Updating ' + ALLTRIM(SYDFDCHG.cFile_Nam) + ' file.' NOWAIT

  *-- Replace Old value by new value
  lcFldtmp = ALLTRIM(SYDFDCHG.cAltField)
  lcFilt = SYDFDCHG.mFltExpr
  IF SYDFILES.CVER = 'A27'
        
    =lfUpdA27(lcFldtmp,lcFilt)
        
  ELSE

    =lfUpdSqlFls(lcFldtmp,lcFilt)
    
  ENDIF 
  WAIT CLEAR 

ENDSCAN


*T20140424.0006 TMI 08/03/2014 11:41 [Start] - Calling the final program that adds the KKA lines to the STYLE & STYDYE
DO add_kka.prg

?DATETIME()
?DATETIME() - lnStart 

*T20140424.0006 TMI 08/03/2014 11:41 [End  ] 



*- show excel file with the styles converted
MESSAGEBOX('conversion done')
CLOSE DATABASES 
*!*	SELECT OldNewStyle
*!*	EXPORT TO lcStyleDataPath+'\StyleMap.xls' XLS
*!*	IF FILE(lcStyleDataPath+'\StyleMap.xls')
*!*	  loRun = CreateObject("WScript.Shell")
*!*	  loRun.Run(lcStyleDataPath+'\StyleMap.xls', 3)
*!*	  loRun = NULL
*!*	ENDIF



*----------------------------------------------------*
*- Open files with tags
*----------------------------------------------------*
FUNCTION lfOpClFile
PARAMETERS lcfil,lcTag
lcTag = IIF(EMPTY(lcTag),'',lcTag)

IF !SEEK(PADR(lcfil,30),'SYDFILES')
  RETURN .F.
ENDIF 
IF SYDFILES.CVER <> 'A27'
  RETURN 
ENDIF 
IF !FILE(lcStyleDataPath+ALLTRIM(lcfil)+'.DBF')
  RETURN .F.
ENDIF 
  
SELECT 0
USE (lcStyleDataPath+ALLTRIM(lcfil)) 
*SET ORDER TO &lcTag  
SET ORDER TO 

*----------------------------------------------------*
*- update A27 files 
*----------------------------------------------------*
FUNCTION lfUpdA27
PARAMETERS lcFld,lcFlt

lcFile = ALLTRIM(sydfiles.cfile_nam)
lcFlt = IIF(!EMPTY(lcFlt),'FOR '+lcFlt,'')
SELECT &lcFile
LOCATE 
SCAN &lcFlt 
       *T20140424.0006 TMI 07/27/2014 21:50 [Start] 
       IF lfAbort()
         EXIT
       ENDIF 
       *T20140424.0006 TMI 07/27/2014 21:50 [End  ]  
  
  WAIT WINDOW NOWAIT ALIAS()+':'+&lcFld
  IF SEEK(&lcFld,'OldNewStyle','OldStyle')
    replace &lcFld WITH OldNewStyle.newStyle
    IF TYPE('&lcFile..SCALE')='C'
      REPLACE SCALE WITH OldNewStyle.newScale
    ENDIF 
    
    *T20140424.0006 TMI 07/27/2014 13:22 [Start] shift function
    =lfShift('FOX')
    *T20140424.0006 TMI 07/27/2014 13:22 [End  ] 
    
    
  ENDIF 
ENDSCAN 

USE 

*----------------------------------------------------*
*- update sql files
*----------------------------------------------------*
FUNCTION lfUpdSqlFls
PARAMETERS lcFld,lcFlt

lcFlt = IIF(!EMPTY(lcFlt),' AND '+lcFlt,'')
lcSetScale = ''
IF !USED('SYDFLFLD_2')
  USE (lcSyspath+'SYDFLFLD') AGAIN IN 0 ALIAS SYDFLFLD_2
ENDIF 
IF SEEK(SYDFILES.cFile_Nam+'SCALE','SYDFLFLD_2','CFLFLD')   && CFILE_NAM+CFLD_NAME
  lcSetScale = ",SCALE = '"+OldNewStyle.newScale+"' "
ENDIF 

lcFl = ALLTRIM(SYDFILES.cFile_Nam)
lnSl = SQLEXEC(lnSqlConn,"SELECT * FROM &lcFl",lcFl)
SELECT &lcFl
SCAN 
       *T20140424.0006 TMI 07/27/2014 21:50 [Start] 
       IF lfAbort()
         EXIT
       ENDIF 
       *T20140424.0006 TMI 07/27/2014 21:50 [End  ]  
  IF SEEK(&lcFld,'OldNewStyle','OldStyle')
    WAIT WINDOW NOWAIT ALLTRIM(SYDFILES.cFile_Nam)+':'+OldNewStyle.newStyle

    lcSQL = "UPDATE "+ALLTRIM(SYDFILES.cFile_Nam)+" "+;
          "SET &lcFld = '"+OldNewStyle.newStyle+"' &lcSetScale" +;
          "WHERE &lcFld = '"+OldNewStyle.oldStyle+"' &lcFlt "
    lnResu = SQLEXEC(lnSqlConn,lcSQL)
    *T20140424.0006 TMI 07/27/2014 13:24 [Start] 
    =lfShift('SQL')
    *T20140424.0006 TMI 07/27/2014 13:24 [End  ] 
    IF lnResu<0
      STRTOFILE(SYDFILES.cFile_Nam+':'+style,lcStyleDataPath+'errfile.txt',1)
    ENDIF 
  ENDIF 
ENDSCAN 


*----------------------------------------------------*
FUNCTION lfSqlFls
*KAZ_similiar
* I got the following code from a program created for KAZU customer by Marima 
SELECT SYDFILES
SET ORDER TO CFILE_NAM   && CFILE_NAM

lcSqlConStr = lnSqlConn
*RETLINE
IF SEEK("RETLINE",'SYDFILES')
  WAIT WINDOW "Updating Retline Table" NOWAIT  
  lnGetData = SQLEXEC(lcSqlConStr,"Select * from retline",'retline_u')
  IF lnGetData > 0
     SELECT retline_u
     LOCATE 
     SCAN
     
       *T20140424.0006 TMI 07/27/2014 21:50 [Start] 
       IF lfAbort()
         EXIT
       ENDIF 
       *T20140424.0006 TMI 07/27/2014 21:50 [End  ]  
       
       WAIT WINDOW NOWAIT ALIAS() + STR(RECNO())
       lcCrmemo 	 = retline_u.crmemo
       lcCret_linno = retline_u.cret_linno
       lcCret_trncd = retline_u.cret_trncd
       lcRecNo     = retline_u.Rec_no
      
       IF SEEK(retline_u.Style,'OldNewStyle' ,'OldStyle') 
         lcNewStyle   = OldNewStyle.NewStyle 
*         lnUpdData = SQLEXEC(lcSqlConStr,"Update retline Set Style = '"+lcNewStyle   +;
        	                  						 "' WHERE crmemo = '"+lcCrmemo +"' AND cret_linno = '"+;
                       								   lcCret_linno+"' AND cret_trncd = '"+lcCret_trncd +"' AND Rec_no ='"+lcRecNo+"'")
         lnUpdData = SQLEXEC(lcSqlConStr,"Update retline Set Style = '"+lcNewStyle   +;
       	                  						 "' WHERE Rec_no ='"+lcRecNo+"'")
        *T20140424.0006 TMI 07/27/2014 13:24 [Start] 
        =lfShift('SQL')
        *T20140424.0006 TMI 07/27/2014 13:24 [End  ] 
       ENDIF 
       IF !EMPTY(retline_u.cretsty) AND  SEEK(retline_u.cretsty,'OldNewStyle' ,'OldStyle')
         lcNewStyle   = OldNewStyle.NewStyle 
*         lnUpdData = SQLEXEC(lcSqlConStr,"Update retline Set cretsty = '"+lcNewStyle+;
      	                 							   "' WHERE crmemo = '"+lcCrmemo +"' AND cret_linno = '"+;
                        								 lcCret_linno+"' AND cret_trncd = '"+lcCret_trncd +"' AND Rec_no ='"+lcRecNo+"'")
         lnUpdData = SQLEXEC(lcSqlConStr,"Update retline Set cretsty = '"+lcNewStyle+;
      	                 							   "' WHERE Rec_no ='"+lcRecNo+"'")
    
       ENDIF 
    ENDSCAN 
    SELECT retline_u
    USE 
  ENDIF   
ENDIF   

*RALINE
IF SEEK("RALINE",'SYDFILES')
  WAIT WINDOW "Updating Raline Table" NOWAIT  
  lnGetData = SQLEXEC(lcSqlConStr,"Select * from Raline ",'raline_u')
  IF lnGetData > 0
     SELECT raline_u
       WAIT WINDOW NOWAIT ALIAS() + STR(RECNO())
     LOCATE 
     SCAN 
       *T20140424.0006 TMI 07/27/2014 21:50 [Start] 
       IF lfAbort()
         EXIT
       ENDIF 
       *T20140424.0006 TMI 07/27/2014 21:50 [End  ]  
       WAIT WINDOW NOWAIT ALIAS() + STR(RECNO())+'/'+STR(RECCOUNT())
       lcRano       = raline_u.rano
       lcCra_linno =  raline_u.cra_linno
       lcRecNo     = raline_u.Rec_no
       
       IF SEEK(raline_u.Style,'OldNewStyle' ,'OldStyle') 
         lcNewStyle   = OldNewStyle.NewStyle 
*         lnUpdData = SQLEXEC(lcSqlConStr,"Update Raline Set Style = '"+lcNewStyle   +;
                                         "' WHERE rano = '"+lcRano +"' AND cra_linno= '"+;
                                          lcCra_linno+"' AND Rec_no = '"+lcRecNo+"'")
         lnUpdData = SQLEXEC(lcSqlConStr,"Update Raline Set Style = '"+lcNewStyle   +;
                                         "' WHERE Rec_no = '"+lcRecNo+"'")
        *T20140424.0006 TMI 07/27/2014 13:24 [Start] 
        =lfShift('SQL')
        *T20140424.0006 TMI 07/27/2014 13:24 [End  ] 
       ENDIF 
    ENDSCAN 
    SELECT raline_u
    USE 
  ENDIF   
ENDIF   


*POSLN
IF SEEK("POSLN",'SYDFILES')
  WAIT WINDOW "Updating POSLN Table" NOWAIT  
  lnGetData = SQLEXEC(lcSqlConStr,"Select * from POSLN ",'POSLN_U')
  IF lnGetData > 0
     SELECT POSLN_U
     LOCATE 
       WAIT WINDOW NOWAIT ALIAS() + STR(RECNO())
     SCAN 
       *T20140424.0006 TMI 07/27/2014 21:50 [Start] 
       IF lfAbort()
         EXIT
       ENDIF 
       *T20140424.0006 TMI 07/27/2014 21:50 [End  ]  
       WAIT WINDOW NOWAIT ALIAS() + STR(RECNO())+'/'+STR(RECCOUNT())
       lccbusdocu  =  POSLN_U.cbusdocu
       lccstytype  =  POSLN_U.cstytype
       lcPO        =  POSLN_U.PO
       lccinvtype  =  POSLN_U.cinvtype
       lnlineno    =  POSLN_U.lineno
       lctrancd    =  POSLN_U.trancd
       lcRecNo     =  POSLN_U.Rec_no
      
       IF SEEK(POSLN_U.Style,'OldNewStyle' ,'OldStyle') 
         lcNewStyle   = OldNewStyle.NewStyle 
*         lnUpdData = SQLEXEC(lcSqlConStr,"Update POSLN Set Style = '"+lcNewStyle   +;
                                         "',Scale = '"+SUBSTR(lcNewStyle,17,3)+"' WHERE cbusdocu = '"+lccbusdocu+"' AND cstytype= '"+;
                                          lccstytype+"' AND PO  = '"+lcPO +"' AND cinvtype = '"+;
                                          lccinvtype +"' AND [lineno] = "+ALLTRIM(STR(lnlineno))+;
                                          " AND trancd = '"+lctrancd+"' AND REC_no = '"+lcRecNo+"'")
         lnUpdData = SQLEXEC(lcSqlConStr,"Update POSLN Set Style = '"+lcNewStyle   +;
                                         "',Scale = '"+SUBSTR(lcNewStyle,17,3)+"' WHERE REC_no = '"+lcRecNo+"'")
        *T20140424.0006 TMI 07/27/2014 13:24 [Start] 
        =lfShift('SQL')
        *T20140424.0006 TMI 07/27/2014 13:24 [End  ] 
       ENDIF 
       
       IF !EMPTY(POSLN_U.CVENSTY) AND  SEEK(POSLN_U.CVENSTY,'OldNewStyle' ,'OldStyle') 
         lcNewStyle   = OldNewStyle.NewStyle 
         *lnUpdData = SQLEXEC(lcSqlConStr,"Update POSLN Set CVENSTY = '"+lcNewStyle   +;
                                         "' WHERE cbusdocu = '"+lccbusdocu+"' AND cstytype= '"+;
                                          lccstytype+"' AND PO  = '"+lcPO +"' AND cinvtype = '"+;
                                          lccinvtype +"' AND [lineno] = "+ALLTRIM(STR(lnlineno))+;
                                          " AND trancd = '"+lctrancd+"' AND REC_no = '"+lcRecNo+"'")
         lnUpdData = SQLEXEC(lcSqlConStr,"Update POSLN Set CVENSTY = '"+lcNewStyle   +;
                                         "' WHERE REC_no = '"+lcRecNo+"'")
       ENDIF 
       
       IF !EMPTY(POSLN_U.CRETSTY) AND SEEK(POSLN_U.CRETSTY,'OldNewStyle' ,'OldStyle') 
         lcNewStyle   = OldNewStyle.NewStyle 
*         lnUpdData = SQLEXEC(lcSqlConStr,"Update POSLN Set CRETSTY= '"+lcNewStyle   +;
                                         "' WHERE cbusdocu = '"+lccbusdocu+"' AND cstytype= '"+;
                                          lccstytype+"' AND PO  = '"+lcPO +"' AND cinvtype = '"+;
                                          lccinvtype +"' AND [lineno] = "+ALLTRIM(STR(lnlineno))+;
                                          " AND trancd = '"+lctrancd+"' AND REC_no = '"+lcRecNo+"'")
         lnUpdData = SQLEXEC(lcSqlConStr,"Update POSLN Set CRETSTY= '"+lcNewStyle   +;
                                         "' WHERE REC_no = '"+lcRecNo+"'")
       ENDIF 
       
    ENDSCAN 
    SELECT POSLN_U
    USE 
  ENDIF   
ENDIF   

*POSHDR
IF SEEK("POSHDR",'SYDFILES')
  WAIT WINDOW "Updating POSHDR Table" NOWAIT  
  lnGetData = SQLEXEC(lcSqlConStr,"Select * from POSHDR where STYle <> ''",'POSHDR_U')
  IF lnGetData > 0
     SELECT POSHDR_U
     LOCATE 
     SCAN 
       *T20140424.0006 TMI 07/27/2014 21:50 [Start] 
       IF lfAbort()
         EXIT
       ENDIF 
       *T20140424.0006 TMI 07/27/2014 21:50 [End  ]  
       WAIT WINDOW NOWAIT ALIAS() + STR(RECNO())+'/'+STR(RECCOUNT())
       lccbusdocu  =  POSHDR_U.cbusdocu
       lccstytype  =  POSHDR_U.cstytype
       lcPO        =  POSHDR_U.PO
       lcRecNo     =  POSHDR_U.Rec_no
      
       IF SEEK(POSHDR_U.Style,'OldNewStyle' ,'OldStyMaj') 
         lcNewStyle   = OldNewStyle.NewStyMaj 
*         lnUpdData = SQLEXEC(lcSqlConStr,"Update POSHDR Set Style = '"+lcNewStyle   +;
                                         "' WHERE cbusdocu = '"+lccbusdocu+"' AND cstytype= '"+;
                                          lccstytype+"' AND PO  = '"+lcPO +"' AND REC_no = '"+lcRecNo+"'")
         lnUpdData = SQLEXEC(lcSqlConStr,"Update POSHDR Set Style = '"+lcNewStyle   +;
                                         "' WHERE REC_no = '"+lcRecNo+"'")
        *T20140424.0006 TMI 07/27/2014 13:24 [Start] 
        =lfShift('SQL')
        *T20140424.0006 TMI 07/27/2014 13:24 [End  ] 
       ENDIF 
    ENDSCAN 
    SELECT POSHDR_U
    USE 
  ENDIF   
ENDIF   

*mfgoprdt
IF SEEK('MFGOPRDT','SYDFILES')
  WAIT WINDOW "Updating mfgoprdt Table" NOWAIT  
  lnGetData = SQLEXEC(lcSqlConStr,"Select * from mfgoprdt where cinvtype = '0001'",'MFGOPRDT_U')
  IF lnGetData > 0
     SELECT MFGOPRDT_U
       WAIT WINDOW NOWAIT ALIAS() + STR(RECNO())
     LOCATE 
     SCAN 
       *T20140424.0006 TMI 07/27/2014 21:50 [Start] 
       IF lfAbort()
         EXIT
       ENDIF 
       *T20140424.0006 TMI 07/27/2014 21:50 [End  ]  
       WAIT WINDOW NOWAIT ALIAS() + STR(RECNO())+'/'+STR(RECCOUNT())
       lccimtyp    =  MFGOPRDT_U.cimtyp
       lcctktno    =  MFGOPRDT_U.ctktno
       lccoprcode  =  MFGOPRDT_U.coprcode
       lcclotno    =  MFGOPRDT_U.clotno
       lctrancd    =  MFGOPRDT_U.trancd
       lccinvtype  =  MFGOPRDT_U.cinvtype
       lccdyelot   =  MFGOPRDT_U.cdyelot
       lcRecNo     =  MFGOPRDT_U.Rec_no
      
       IF SEEK(MFGOPRDT_U.ITEM,'OldNewStyle' ,'OldStyle') 
         lcNewStyle   = OldNewStyle.NewStyle 
*         lnUpdData = SQLEXEC(lcSqlConStr,"Update MFGOPRDT Set ITEM = '"+lcNewStyle +;
                                         "' WHERE cimtyp= '"+lccimtyp+"' AND ctktno= '"+;
                                          lcctktno +"' AND coprcode = '"+lccoprcode+;
                                          "' AND clotno = '"+lcclotno+"' AND trancd = '"+;
                                          lctrancd +"' AND cinvtype = '"+ lccinvtype +;
                                          "' AND cdyelot = '"+ lccdyelot +"' AND REC_no = '"+lcRecNo+"'")
         lnUpdData = SQLEXEC(lcSqlConStr,"Update MFGOPRDT Set ITEM = '"+lcNewStyle +;
                                         "' WHERE REC_no = '"+lcRecNo+"'")
        *T20140424.0006 TMI 07/27/2014 13:24 [Start] 
        =lfShift('SQL')
        *T20140424.0006 TMI 07/27/2014 13:24 [End  ] 
       ENDIF 
    ENDSCAN 
    SELECT MFGOPRDT_U
    USE 
  ENDIF   
ENDIF   

*ITMPRICE
IF SEEK('ITMPRICE','SYDFILES')
  WAIT WINDOW "Updating ITMPRICE Table" NOWAIT  
  lnGetData = SQLEXEC(lcSqlConStr,"Select * from ITMPRICE Where cinvtype = '0001'",'ITMPRICE_U')
  IF lnGetData > 0
     SELECT ITMPRICE_U
       WAIT WINDOW NOWAIT ALIAS() + STR(RECNO())
     LOCATE 
     SCAN 
       *T20140424.0006 TMI 07/27/2014 21:50 [Start] 
       IF lfAbort()
         EXIT
       ENDIF 
       *T20140424.0006 TMI 07/27/2014 21:50 [End  ]  
       WAIT WINDOW NOWAIT ALIAS() + STR(RECNO())+'/'+STR(RECCOUNT())
       lcccurrcode =  ITMPRICE_U.ccurrcode
       lccinvtype  =  ITMPRICE_U.cinvtype
       lcRecno     =  ITMPRICE_U.rec_no
       IF SEEK(ITMPRICE_U.Style,'OldNewStyle' ,'OldStyle') 
         lcNewStyle   = OldNewStyle.NewStyle 
*         lnUpdData = SQLEXEC(lcSqlConStr,"Update ITMPRICE Set STYLE = '"+lcNewStyle +;
                                         "' WHERE cinvtype= '"+lccinvtype  +"' AND ccurrcode= '"+;
                                          lcccurrcode +"' AND rec_no= '"+lcRecno+"'")
         lnUpdData = SQLEXEC(lcSqlConStr,"Update ITMPRICE Set STYLE = '"+lcNewStyle +;
                                         "' WHERE rec_no= '"+lcRecno+"'")
        *T20140424.0006 TMI 07/27/2014 13:24 [Start] 
        =lfShift('SQL')
        *T20140424.0006 TMI 07/27/2014 13:24 [End  ] 
       ENDIF 
    ENDSCAN 
    SELECT ITMPRICE_U
    USE 
  ENDIF   
ENDIF   


*ICITMHST  
IF SEEK('ICITMHST','SYDFILES')
  WAIT WINDOW "Updating ICITMHST Table" NOWAIT  
  lnGetData = SQLEXEC(lcSqlConStr,"Select * from ICITMHST Where cinvtype = '0001'",'ICITMHST_U')
  IF lnGetData > 0
     SELECT ICITMHST_U
       WAIT WINDOW NOWAIT ALIAS() + STR(RECNO())
     LOCATE 
     SCAN 
       *T20140424.0006 TMI 07/27/2014 21:50 [Start] 
       IF lfAbort()
         EXIT
       ENDIF 
       *T20140424.0006 TMI 07/27/2014 21:50 [End  ]  
       WAIT WINDOW NOWAIT ALIAS() + STR(RECNO())+'/'+STR(RECCOUNT())
       lccfisfyear =  ICITMHST_U.cfisfyear
       lccinvtype  =  ICITMHST_U.cinvtype
       lcRecno     =  ICITMHST_U.rec_no
       IF SEEK(ICITMHST_U.Style,'OldNewStyle' ,'OldStyle') 
         lcNewStyle   = OldNewStyle.NewStyle 
         *lnUpdData = SQLEXEC(lcSqlConStr,"Update ICITMHST Set STYLE = '"+lcNewStyle +;
                                         "' WHERE cinvtype= '"+lccinvtype  +"' AND cfisfyear= '"+;
                                          lccfisfyear +"' AND rec_no= '"+lcRecno+"'")
         lnUpdData = SQLEXEC(lcSqlConStr,"Update ICITMHST Set STYLE = '"+lcNewStyle +;
                                         "' WHERE rec_no= '"+lcRecno+"'")
        *T20140424.0006 TMI 07/27/2014 13:24 [Start] 
        =lfShift('SQL')
        *T20140424.0006 TMI 07/27/2014 13:24 [End  ] 
       ENDIF 
    ENDSCAN 
    SELECT ICITMHST_U
    USE 
  ENDIF   
ENDIF   

*dye_rel  
IF SEEK('DYE_REL','SYDFILES')
  WAIT WINDOW "Updating DYE_REL Table" NOWAIT  
  lnGetData = SQLEXEC(lcSqlConStr,"Select * from DYE_REL Where cinvtype = '0001'",'DYE_REL_U')
  IF lnGetData > 0
     SELECT DYE_REL_U
       WAIT WINDOW NOWAIT ALIAS() + STR(RECNO())
     LOCATE 
     SCAN 
       *T20140424.0006 TMI 07/27/2014 21:50 [Start] 
       IF lfAbort()
         EXIT
       ENDIF 
       *T20140424.0006 TMI 07/27/2014 21:50 [End  ]  
       WAIT WINDOW NOWAIT ALIAS() + STR(RECNO())+'/'+STR(RECCOUNT())
       lccdye_seq =  DYE_REL_U.cdye_seq
       lccinvtype  =  DYE_REL_U.cinvtype
       lcRecno     =  DYE_REL_U.rec_no
       IF SEEK(DYE_REL_U.citem,'OldNewStyle' ,'OldStyle') 
         lcNewStyle   = OldNewStyle.NewStyle 
*         lnUpdData = SQLEXEC(lcSqlConStr,"Update DYE_REL Set citem = '"+lcNewStyle +;
                                         "' WHERE cinvtype= '"+lccinvtype  +"' AND cdye_seq= '"+;
                                          lccdye_seq +"' AND rec_no= '"+lcRecno+"'")
         lnUpdData = SQLEXEC(lcSqlConStr,"Update DYE_REL Set citem = '"+lcNewStyle +;
                                         "' WHERE rec_no= '"+lcRecno+"'")
        *T20140424.0006 TMI 07/27/2014 13:24 [Start] 
        =lfShift('SQL')
        *T20140424.0006 TMI 07/27/2014 13:24 [End  ] 
       ENDIF 
    ENDSCAN 
    SELECT DYE_REL_U
    USE 
  ENDIF   
ENDIF   


*CUTPICK
IF SEEK('CUTPICK','SYDFILES')
  WAIT WINDOW "Updating CUTPICK Table" NOWAIT  
  lnGetData = SQLEXEC(lcSqlConStr,"Select * from CUTPICK ",'CUTPICK_U')
  IF lnGetData > 0
     SELECT CUTPICK_U
       WAIT WINDOW NOWAIT ALIAS() + STR(RECNO())
     LOCATE 
     SCAN 
       *T20140424.0006 TMI 07/27/2014 21:50 [Start] 
       IF lfAbort()
         EXIT
       ENDIF 
       *T20140424.0006 TMI 07/27/2014 21:50 [End  ]  
       WAIT WINDOW NOWAIT ALIAS() + STR(RECNO())+'/'+STR(RECCOUNT())
       lctrancd  =  CUTPICK_U.trancd
       lcctktno  =  CUTPICK_U.ctktno
       lcctktlineno    =  CUTPICK_U.ctktlineno
       lcOrder  =  CUTPICK_U.order
       lccordline  = CUTPICK_U.cordline
       lcRecNo     = CUTPICK_U.Rec_no
       
       IF SEEK(CUTPICK_U.Style,'OldNewStyle' ,'OldStyle') 
         lcNewStyle   = OldNewStyle.NewStyle 
*         lnUpdData = SQLEXEC(lcSqlConStr,"Update CUTPICK Set Style = '"+lcNewStyle +;
                                         "' WHERE trancd= '"+lctrancd  +"' AND ctktno= '"+;
                                          lcctktno  +"' AND ctktlineno= '"+lcctktlineno    +"' AND [order] = '"+;
                                          lcOrder  +"' AND cordline = '"+lccordline  +"' AND REC_no = '"+lcRecNo+"'")
         lnUpdData = SQLEXEC(lcSqlConStr,"Update CUTPICK Set Style = '"+lcNewStyle +;
                                         "' WHERE REC_no = '"+lcRecNo+"'")
        *T20140424.0006 TMI 07/27/2014 13:24 [Start] 
        =lfShift('SQL')
        *T20140424.0006 TMI 07/27/2014 13:24 [End  ] 
       ENDIF 
    ENDSCAN 
    SELECT CUTPICK_U
    USE 
  ENDIF   
ENDIF   


*CTKTBOM  
IF SEEK('CTKTBOM','SYDFILES')
  WAIT WINDOW "Updating CTKTBOM Table" NOWAIT  
  lnGetData = SQLEXEC(lcSqlConStr,"Select *  from ctktbom  where  CINVTYPe = '0001' and item<> ''",'CTKTBOM_U')
  IF lnGetData > 0
     SELECT CTKTBOM_U
       WAIT WINDOW NOWAIT ALIAS() + STR(RECNO())
     LOCATE 
     SCAN &&cimtyp, cuttkt, typ, cinvtype, item, mfgcode, dyelot
       *T20140424.0006 TMI 07/27/2014 21:50 [Start] 
       IF lfAbort()
         EXIT
       ENDIF 
       *T20140424.0006 TMI 07/27/2014 21:50 [End  ]  
       WAIT WINDOW NOWAIT ALIAS() + STR(RECNO())+'/'+STR(RECCOUNT())
       lccimtyp       =  CTKTBOM_U.cimtyp
       lccuttkt       =  CTKTBOM_U.cuttkt
       lctyp          =  CTKTBOM_U.typ
       lccinvtype     =  CTKTBOM_U.cinvtype
       lcmfgcode      = CTKTBOM_U.mfgcode
       lcDyelot       = CTKTBOM_U.dyelot
       lcRecNo        = CTKTBOM_U.Rec_no
       
       IF SEEK(CTKTBOM_U.item,'OldNewStyle' ,'OldStyle') 
         lcNewStyle   = OldNewStyle.NewStyle 
*         lnUpdData = SQLEXEC(lcSqlConStr,"Update ctktbom  Set item = '"+lcNewStyle +;
                                         "' WHERE cimtyp= '"+lccimtyp+"' AND cuttkt= '"+;
                                          lccuttkt +"' AND typ= '"+lctyp    +"' AND cinvtype= '"+;
                                          lccinvtype     +"' AND mfgcode= '"+lcmfgcode+;
                                          "' AND dyelot = '"+lcDyelot+"' AND Rec_no = '"+lcRecNo +"'")
         lnUpdData = SQLEXEC(lcSqlConStr,"Update ctktbom  Set item = '"+lcNewStyle +;
                                         "' WHERE Rec_no = '"+lcRecNo +"'")
        *T20140424.0006 TMI 07/27/2014 13:24 [Start] 
        =lfShift('SQL')
        *T20140424.0006 TMI 07/27/2014 13:24 [End  ] 
       ENDIF 
    ENDSCAN 
    SELECT CTKTBOM_U
    USE 
  ENDIF   
ENDIF   


*BOMLINE
IF SEEK('BOMLINE','SYDFILES')
  WAIT WINDOW "Updating BOMLINE Table" NOWAIT  
  lnGetData = SQLEXEC(lcSqlConStr,"Select *  from bomline  where  CINVTYPe = '0001' ",'BOMLINE_U')
  IF lnGetData > 0
     SELECT BOMLINE_U
       WAIT WINDOW NOWAIT ALIAS() + STR(RECNO())
     LOCATE 
     SCAN 
       *T20140424.0006 TMI 07/27/2014 21:50 [Start] 
       IF lfAbort()
         EXIT
       ENDIF 
       *T20140424.0006 TMI 07/27/2014 21:50 [End  ]  
       WAIT WINDOW NOWAIT ALIAS() + STR(RECNO())+'/'+STR(RECCOUNT())
       lccimtyp       = BOMLINE_U.cimtyp
       lcctktno       = BOMLINE_U.ctktno
       lcctype        = BOMLINE_U.ctype
       lccbomtyp      = BOMLINE_U.cbomtyp
       lccinvtype     = BOMLINE_U.cinvtype
       lnlineno       = BOMLINE_U.lineno
       lccinvtypc     = BOMLINE_U.cinvtypc
       lcitem         = BOMLINE_U.Item
       lcmfgcode      = BOMLINE_U.mfgcode
       lcRecNo        = BOMLINE_U.Rec_no
       
       IF SEEK(BOMLINE_U.Style,'OldNewStyle' ,'OldStyle') 
         lcNewStyle   = OldNewStyle.NewStyle 
         
         IF lccinvtypc = '0001' AND SEEK(BOMLINE_U.Item,'OldNewStyle' ,'OldStyle') 
           lcitem      = OldNewStyle.NewStyle           
         ENDIF 
         
*         lnUpdData = SQLEXEC(lcSqlConStr,"Update BOMLINE Set Style= '"+lcNewStyle +;
                                         "',ITEM = '"+lcitem+"' WHERE cimtyp= '"+lccimtyp+"' AND ctktno= '"+;
                                          lcctktno +"' AND ctype= '"+lcctype+"' AND cinvtype= '"+;
                                          lccinvtype     +"' AND mfgcode= '"+lcmfgcode+;
                                          "' AND cbomtyp = '"+lccbomtyp +;
                                          "' AND [lineno] = "+ALLTRIM(STR(lnlineno ))+;
                                          " AND cinvtypc = '"+ lccinvtypc +"' AND Rec_no = '"+lcRecNo +"'")
         lnUpdData = SQLEXEC(lcSqlConStr,"Update BOMLINE Set Style= '"+lcNewStyle +;
                                         "',ITEM = '"+lcitem+"' WHERE Rec_no = '"+lcRecNo +"'")
        *T20140424.0006 TMI 07/27/2014 13:24 [Start] 
        =lfShift('SQL')
        *T20140424.0006 TMI 07/27/2014 13:24 [End  ] 
       ENDIF 
    ENDSCAN 
    SELECT BOMLINE_U
    USE 
  ENDIF   
ENDIF   


*BOMHEADR 
IF SEEK("BOMHEADR",'SYDFILES')
  WAIT WINDOW "Updating BOMHEADR  Table" NOWAIT  
  lnGetData = SQLEXEC(lcSqlConStr,"Select *  from bomheadr where  CINVTYPe = '0001'",'BOMHEADR_U')
  IF lnGetData > 0
     SELECT BOMHEADR_U
       WAIT WINDOW NOWAIT ALIAS() + STR(RECNO())
     LOCATE 
     SCAN 
       *T20140424.0006 TMI 07/27/2014 21:50 [Start] 
       IF lfAbort()
         EXIT
       ENDIF 
       *T20140424.0006 TMI 07/27/2014 21:50 [End  ]  
       WAIT WINDOW NOWAIT ALIAS() + STR(RECNO())+'/'+STR(RECCOUNT())
       lcCinvtype  =  BOMHEADR_U.CINVTYPe
       lcRecNo     =  BOMHEADR_U.Rec_no
      
       IF SEEK(BOMHEADR_U.CITMMAJOR ,'OldNewStyle' ,'OldStyMaj') 
         lcNewStyle   = OldNewStyle.NewStyMaj 
*         lnUpdData = SQLEXEC(lcSqlConStr,"Update BOMHEADR Set CITMMAJOR = '"+lcNewStyle   +;
                                         "' WHERE CINVTYPe = '"+lcCinvtype  +"' AND Rec_no= '"+;
                                          lcRecNo+"'")
         lnUpdData = SQLEXEC(lcSqlConStr,"Update BOMHEADR Set CITMMAJOR = '"+lcNewStyle   +;
                                         "' WHERE Rec_no= '"+;
                                          lcRecNo+"'")
        *T20140424.0006 TMI 07/27/2014 13:24 [Start] 
        =lfShift('SQL')
        *T20140424.0006 TMI 07/27/2014 13:24 [End  ] 
       ENDIF 
    ENDSCAN 
    SELECT BOMHEADR_U
    USE 
  ENDIF   
ENDIF   


*BOMCOST
IF SEEK("BOMCOST",'SYDFILES')
  WAIT WINDOW "Updating BOMCOST Table" NOWAIT  
  lnGetData = SQLEXEC(lcSqlConStr,"Select *  from bomcost where  CINVTYPe = '0001' and item<> ''",'BOMCOST_U')
  IF lnGetData > 0
     SELECT BOMCOST_U
       WAIT WINDOW NOWAIT ALIAS() + STR(RECNO())
     LOCATE 
     SCAN 
       *T20140424.0006 TMI 07/27/2014 21:50 [Start] 
       IF lfAbort()
         EXIT
       ENDIF 
       *T20140424.0006 TMI 07/27/2014 21:50 [End  ]  
       WAIT WINDOW NOWAIT ALIAS() + STR(RECNO())+'/'+STR(RECCOUNT())
       lccbomtype  = BOMCOST_U.cbomtype
       lccimtyp    = BOMCOST_U.cimtyp
       lcctktno    = BOMCOST_U.ctktno
       lcmfgcode   = BOMCOST_U.mfgcode
       lccwarecode = BOMCOST_U.cwarecode
       lccdyelot   = BOMCOST_U.cdyelot  
       lccrsession = BOMCOST_U.crsession 
       lccisession = BOMCOST_U.cisession
       lcCinvtype  = BOMCOST_U.CINVTYPe
       lcRecNo     = BOMCOST_U.Rec_no
      
       IF SEEK(BOMCOST_U.ITeM ,'OldNewStyle' ,'OldStyle') 
         lcNewStyle   = OldNewStyle.NewStyle           
*         lnUpdData = SQLEXEC(lcSqlConStr,"Update BOMCOST Set ITeM  = '"+lcNewStyle+;
                                         "' WHERE cbomtype = '"+lccbomtype  +"' AND cimtyp = '"+  lccimtyp +;
                                         "' AND ctktno = '"+lcctktno+"' AND CINVTYPe = '"+lcCinvtype+;
                                         "' AND  mfgcode = '"+lcmfgcode+"' AND  cwarecode = '"+lccwarecode +;
                                         "' AND cdyelot = '"+ lccdyelot +;
                                         "' AND crsession = '"+ lccrsession +;
                                         "' AND cisession = '"+lccisession +;
                                         "' AND Rec_no= '"+lcRecNo+"'")
         lnUpdData = SQLEXEC(lcSqlConStr,"Update BOMCOST Set ITeM  = '"+lcNewStyle+;
                                         "' WHERE Rec_no= '"+lcRecNo+"'")
        *T20140424.0006 TMI 07/27/2014 13:24 [Start] 
        =lfShift('SQL')
        *T20140424.0006 TMI 07/27/2014 13:24 [End  ] 
       ENDIF 
    ENDSCAN 
    SELECT BOMCOST_U
    USE 
  ENDIF   
ENDIF   


*BOM
IF SEEK("BOM ",'SYDFILES')
  WAIT WINDOW "Updating BOM Table" NOWAIT  
  lnGetData = SQLEXEC(lcSqlConStr,"Select *  from bom where  CINVTYPe = '0001'",'BOM_U')
  IF lnGetData > 0
     SELECT BOM_U
       WAIT WINDOW NOWAIT ALIAS() + STR(RECNO())
     LOCATE 
     SCAN 
       *T20140424.0006 TMI 07/27/2014 21:50 [Start] 
       IF lfAbort()
         EXIT
       ENDIF 
       *T20140424.0006 TMI 07/27/2014 21:50 [End  ]  
       WAIT WINDOW NOWAIT ALIAS() + STR(RECNO())+'/'+STR(RECCOUNT())
       lcccstshttyp  = BOM_U.ccstshttyp
       lctyp         = BOM_U.typ
       lcccstsht_id    = BOM_U.ccstsht_id
       lcmfgcode   = BOM_U.mfgcode
       lccinvtypc   = BOM_U.cinvtypc
       lnnlineno = BOM_U.nlineno
       lcCinvtype  = BOM_U.CINVTYPe
       lcRecNo     = BOM_U.Rec_no
       lcItem = BOM_U.ITEM
       
       IF SEEK(BOM_U.CITMMAJOR ,'OldNewStyle' ,'OldStyMaj') 
         lcNewStyle   = OldNewStyle.NewStyMaj 
         lcNewItemMask = BOM_U.citmmask
         lnSepPos = 13 &&AT("-",BOM_U.citmmask)
         
         IF SUBSTR(BOM_U.citmmask,lnSepPos +1,6) = '******'
           lcNewItemMask = SUBSTR(lcNewStyle,1,8)+"-"+'******'+'-'+'***'&&SUBSTR(OldNewStyle.NewStyle,17,3)           
         ELSE
           IF SEEK(BOM_U.citmmask,'OldNewStyle' ,'OldStyle') 
             lcNewItemMask = SUBSTR(OldNewStyle.NewStyle,1,16)+'***'
           ENDIF 
         ENDIF 
         lcNewItem = lcItem
         
         IF BOM_U.cinvtypc = '0001' AND !EMPTY(lcItem)
           lnSepPos = 13&&AT("-",lcItem)
           IF SUBSTR(lcItem,lnSepPos +1,6) = '******'
             IF SEEK(PADR(SUBSTR(lcItem,1,lnSepPos -1),19) ,'OldNewStyle' ,'OldStyMaj') 
               lcNewItem = SUBSTR(OldNewStyle.NewStyMaj,1,8)+"-"+'******'+'-'+SUBSTR(OldNewStyle.NewStyle,17,3)  
             ENDIF            
           ELSE
             IF SEEK(lcItem,'OldNewStyle' ,'OldStyle') 
               lcNewItem = OldNewStyle.NewStyle
             ENDIF 
           ENDIF
         ENDIF 
         lcItem = lcNewItem 
         
*         lnUpdData = SQLEXEC(lcSqlConStr,"Update BOM Set CITMMAJOR = '"+lcNewStyle+;
                                         "',citmmask = '"+lcNewItemMask +;
                                         "',ITEM = '"+lcItem +"' WHERE ccstshttyp= '"+;
                                         lcccstshttyp+"' AND typ= '"+  lctyp   +;
                                         "' AND ccstsht_id = '"+lcccstsht_id    +"' AND CINVTYPe = '"+lcCinvtype+;
                                         "' AND  mfgcode = '"+lcmfgcode+"' AND  cinvtypc= '"+lccinvtypc+;
                                         "' AND nlineno= "+ ALLTRIM(STR(lnnlineno )) +;
                                         " AND Rec_no= '"+lcRecNo+"'")
         lnUpdData = SQLEXEC(lcSqlConStr,"Update BOM Set CITMMAJOR = '"+lcNewStyle+;
                                         "',citmmask = '"+lcNewItemMask +;
                                         "',ITEM = '"+lcItem +"' WHERE Rec_no= '"+lcRecNo+"'")
        *T20140424.0006 TMI 07/27/2014 13:24 [Start] 
        =lfShift('SQL')
        *T20140424.0006 TMI 07/27/2014 13:24 [End  ] 
       ENDIF 
    ENDSCAN 
    SELECT BOM_U
    USE 
  ENDIF   
ENDIF   




************************************************************************************************************************
*  Test code check 
************************************************************************************************************************
FUNCTION lfSTY_EXC
PARAMETERS lcSty,lcQtFld
IF SEEK(OldNewStyle.OldStyle,'STY_EXC')
  Replace scale WITH sty_exc.D
  replace Style WITH SUBSTR(Style,1,16)+sty_exc.D
ENDIF   



************************************************************************************************************************
*  Test code check 
************************************************************************************************************************
FUNCTION lfTestCode
*PARAMETERS lnCnt
IF SYS(0) = 'SDE_DEV8_SABER # Saber' 
  lnCnt = lnCnt + 1
  IF lnCnt>100
    lnCnt = 0
    RETURN .F.
    EXIT
  ENDIF 
ENDIF 

************************************************************
*! Name      : lfShift
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 07/27/2014
*! Purpose   : shift sizes action 
************************************************************
*T20140424.0006 TMI 07/27/2014 13:39 [Start] 
FUNCTION lfShift
PARAMETERS lcTyp


*- suppress the calling of thie function 
RETURN 
*- suppress the calling of thie function 


LOCAL lcAlias
lcAlias = ALIAS()

*- if not KKA, then no shift action is needed
IF OldNewStyle.newScale <> 'KKA'
  RETURN 
ENDIF

*- check the size 8, if 0 then no shift action is needed
CREATE cursor TotFld (fld c(10),totfld c(10),lupdate L)
INSERT INTO TotFld VALUES ('NACTIQTY','NTOTACTQTY',.F.)
INSERT INTO TotFld VALUES ('NAPAPLQTY','NAPTAPLQTY',.F.)
INSERT INTO TotFld VALUES ('NAPQTY','NAPTOTQTY',.F.)
INSERT INTO TotFld VALUES ('NAPAPRQTY','NAPTAPRQTY',.F.)
INSERT INTO TotFld VALUES ('NFORQTY','NFORTOTQTY',.F.)
INSERT INTO TotFld VALUES ('NOPNQTY','NTOTOPNQTY',.F.)
INSERT INTO TotFld VALUES ('PIK','TOTPIK',.F.)
INSERT INTO TotFld VALUES ('POALO','TOT_POALO',.F.)
INSERT INTO TotFld VALUES ('NWO','NTOTWO',.F.)
INSERT INTO TotFld VALUES ('ORD','TOTORD',.F.)
INSERT INTO TotFld VALUES ('WIP','TOTWIP',.F.)
INSERT INTO TotFld VALUES ('CUT','TOTCUT',.F.)
INSERT INTO TotFld VALUES ('STK','TOTSTK',.F.)
INSERT INTO TotFld VALUES ('ALO','TOTALO',.F.)
INSERT INTO TotFld VALUES ('SHP','TOTSHP',.F.)
INSERT INTO TotFld VALUES ('RET','TOTRET',.F.)
INSERT INTO TotFld VALUES ('PLAN','TOTPLAN',.F.)
INSERT INTO TotFld VALUES ('RA','TOTRA',.F.)
INSERT INTO TotFld VALUES ('INTRANS','TOTINTRN',.F.)
INSERT INTO TotFld VALUES ('NOLDTO','NTOTOLDTO',.F.)
INSERT INTO TotFld VALUES ('OLDQTY','TOTOLD',.F.)
INSERT INTO TotFld VALUES ('ADJ','TOTADJ',.F.)
INSERT INTO TotFld VALUES ('REQ_QTY','REQ_QTY',.F.)
INSERT INTO TotFld VALUES ('ISS_QTY ', 'ISSUE_QTY',.F.)
INSERT INTO TotFld VALUES ('QTY','TOTQTY',.F.)
INSERT INTO TotFld VALUES ('USED_QTY','USED_QTY',.F.)
INSERT INTO TotFld VALUES ('NSTK','NTOTSTK',.F.)    && STYINVJL

SELECT TotFld
REPLACE lUpdate WITH .F. all
LOCATE 
SCAN
  lcFld = ALLTRIM(TotFld.Fld)
  lcTotFld = ALLTRIM(TotFld.totFld)
  IF TYPE('&lcAlias..&lcFld.1') = 'N' AND ;
     TYPE('&lcAlias..&lcFld.2') = 'N' AND ;
     TYPE('&lcAlias..&lcFld.3') = 'N' AND ;
     TYPE('&lcAlias..&lcFld.4') = 'N' AND ;
     TYPE('&lcAlias..&lcFld.5') = 'N' AND ;
     TYPE('&lcAlias..&lcFld.6') = 'N' AND ;
     TYPE('&lcAlias..&lcFld.7') = 'N' AND ;
     TYPE('&lcAlias..&lcFld.8') = 'N' AND &lcAlias..&lcFld.8 <> 0     
     replace lUpdate WITH .T.
     SELECT &lcAlias
     lnQ8 = &lcFld.8
     REPLACE &lcFld.1 WITH 0,;
             &lcFld.2 WITH &lcFld.1,;
             &lcFld.3 WITH &lcFld.2,;
             &lcFld.4 WITH &lcFld.3,;
             &lcFld.5 WITH &lcFld.4,;
             &lcFld.6 WITH &lcFld.5,;
             &lcFld.7 WITH &lcFld.6,;
             &lcFld.8 WITH &lcFld.7,;
             &lcTotFld. WITH &lcFld.1+&lcFld.2+&lcFld.3+&lcFld.4+&lcFld.5+&lcFld.6+&lcFld.7+&lcFld.8
      IF lcAlias = 'STYINVJL'
        REPLACE NSTKVAL WITH NCOST*&lcTotFld.
      ENDIF
      SCATTER MEMVAR MEMO
      APPEND BLANK
      GATHER MEMVAR MEMO     
     REPLACE &lcFld.1 WITH lnQ8 ,;
             &lcFld.2 WITH 0,;
             &lcFld.3 WITH 0,;
             &lcFld.4 WITH 0,;
             &lcFld.5 WITH 0,;
             &lcFld.6 WITH 0,;
             &lcFld.7 WITH 0,;
             &lcFld.8 WITH 0,;
             &lcTotFld. WITH lnQ8
      IF lcAlias = 'STYINVJL'
        REPLACE NSTKVAL WITH NCOST*&lcTotFld.
      ENDIF      
      IF &lcAlias = 'ORDLINE' 
        IF !USED('ordhdr')
          USE (lcStyleDataPath+'ORDHDR') IN 0 ORDER ORDHDR   && CORDTYPE+ORDER
        ENDIF         
        m.LINENO = IIF(SEEK(M.CORDTYPE+M.ORDER,'ORDHDR','ORDHDR'),ORDHDR.LASTLINE,0)+1
        REPLACE ORDHDR.LASTLINE WITH M.LINENO
        REPLACE ORDLINE.LINENO WITH M.LINENO
      ENDIF 
      
  ENDIF
ENDSCAN

DO case
CASE lcTyp = 'FOX'
CASE lcTyp = 'SQL'
ENDCASE 
SELECT (lcAlias)
*- End of lfShift.


************************************************************************************************

*    FUNCTION lfAbort

************************************************************************************************
FUNCTION lfAbort
IF 'SDE_DEV8_SABER' $ SYS(0)
  IF ALIAS()='STYDYE'
    RETURN .F.
  ENDIF 
  lnAbort = lnAbort + 1
  IF lnAbort > 1000
    lnAbort = 0
    *RETURN .T.
  ENDIF 
ENDIF 

RETURN .F.
*- end of FUNCTION lfAbort