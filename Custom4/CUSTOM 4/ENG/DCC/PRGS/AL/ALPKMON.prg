*:*********************************************************************************
*: Program file  : ALPKMON.PRG
*: Program desc. : PICK AND PACK MONITOR 
*:        System : Aria4 XP.
*:        Module : AL(Allocation).
*:     Developer : Mariam Mazhar (MMT)
*:     Entry     : C201335==>A27,C201334==>A40[T20110401.0003]
*:*********************************************************************************
*: Modifications:
**: B609855,1 MMT 03/05/2012 Fix bug of wrong cursor position in custom Update Pick ticket program OG[T20110401.0003]
**: B610030,1 MMT 07/29/2012 Fix error while scrolling browse in Pick Pack Monitor screen[T20120620.0002]
**: B610180,1 SAB 12/24/2012 Fix Pick Pack Monitor screen scroll issue [T20110401.0003 - Issue 8]
*:*********************************************************************************

**: B610180,1 SAB 12/24/2012 Fix Pick Pack Monitor screen scroll issue [Start]
=gfOpenTable('PICKPACK','PICKPACK','SH','PICK_BROW')
**: B610180,1 SAB 12/24/2012 Fix Pick Pack Monitor screen scroll issue [End]

=gfOpenTable('PICKPACK','PICKPACK')
=gfOpenTable('PICKPACK','PICKPACK','SH','PICK_UPDT')
SELECT 'PICKPACK'
lcBrfields = "PIKTKT    :H='Piktkt' ,PIKDATE   :H='Pick Date' , CPSTATUS  :H='Status' ,ORDER :H='SO No.',cAdd_user  :H='Add User',"+;
					   	"Account  :H='Account' , STORE :H='Store' ,lcPickBy =lfPickby():H='Picked By',DPICKBY   :H='Picked By Date'"+;
					   	",CPICKTIME   :H='Picked By Time',LCOMPPICK   :H='Picked Complete',lcChkDesc=gfCodDes(SUBSTR(CHKEDBY,1,6),'CPACKERS  '):H='Checked By'"+;
					   	",DCHKBY    :H='Checked By Date',CCHKBYTIME :H='Checked By Time',CDESPBY :H='Despatch By',"+;
					   	"DDESPBY  :H='Despatch Date',CDESPTIME  :H='Despatch Time', CONSGMENT :H='Consign. No.',"+;
					   	"lcACTION = lfAction()   :H='Action',lcACTIONTO=lfACTIONTO() :H='Action To',ACTTOMAIL  :H='Action To Email',ACTIONDATE  :H='Action Date',"+;
					   	"ACTIONTIME :H='Action Time',lcNOTES=lfNotes() :H='Notes'"
                       
lcKeyFlt = ''&&"FOR CPSTATUS='Not Issued'"   
*: B609855,1 MMT 04/02/2012 Change Filter Expression to be able to apply filters[Start]
loCustObj = CREATEOBJECT('PickPackMon')
*: B609855,1 MMT 04/02/2012 Change Filter Expression to be able to apply filters[end]

**: B610180,1 SAB 12/24/2012 Fix Pick Pack Monitor screen scroll issue [Start]
*!*	*: B609855,1 MMT 03/05/2012 Fix bug of wrong cursor position in custom Update Pick ticket program OG[Start]                    
*!*	*!*	lcOldError = ON("Error")
*!*	*!*	llErrorHappend = .F.
*!*	*!*	ON ERROR llErrorHappend = .T.
*!*	*: B609855,1 MMT 03/05/2012 Fix bug of wrong cursor position in custom Update Pick ticket program OG[End]       
*!*	**: B610030,1 MMT 07/29/2012 Fix error while scrolling browse in Pick Pack Monitor screen[Start]     
*!*	lcOldErrorH = ON("Error")
*!*	llErrorHappend = .F.     
*!*	ON ERROR llErrorHappend = .T.   
*!*	**: B610030,1 MMT 07/29/2012 Fix error while scrolling browse in Pick Pack Monitor screen[End]             
*!*	=ARIABROW(lcKeyFlt , 'Pick Tickets', .F., .F., .F., .F., .F., .F., "PIKTKT", .F., .T., .F.,.F., 'PICKPACK')
*!*	*: B609855,1 MMT 03/05/2012 Fix bug of wrong cursor position in custom Update Pick ticket program OG[Start]                    
*!*	*!*	ON ERROR &lcOldError.
*!*	**: B610030,1 MMT 07/29/2012 Fix error while scrolling browse in Pick Pack Monitor screen[Start]     
*!*	ON ERROR &lcOldErrorH.
*!*	**: B610030,1 MMT 07/29/2012 Fix error while scrolling browse in Pick Pack Monitor screen[End]     
*!*	*: B609855,1 MMT 03/05/2012 Fix bug of wrong cursor position in custom Update Pick ticket program OG[End]

SELECT PICK_BROW
=gfSeek("")
LOCATE
=ARIABROW(lcKeyFlt , 'Pick Tickets', .F., .F., .F., .F., .F., .F., "PIKTKT", .F., .T., .F.,.F., 'PICK_BROW', .F., .F., .F., .F., ;
          .F., .F.,.F.,.F., 'PICKPACK')
**: B610180,1 SAB 12/24/2012 Fix Pick Pack Monitor screen scroll issue [End]

*:**************************************************************************
*:* Name        : lfPickby
*! Developer    : Mariam Mazhar
*! Date         : 05/11/2011
*:* Purpose     : Picked by object
*:***************************************************************************
FUNCTION lfPickby
  IF TYPE('oBrowse') <> 'O'
    lcExpRet = '<VFP:CustomObject ObjectName="Custariacodes"  SourceControl = "CPICKBY"'+' LPARAMATER = "CPICKERS"'
  ELSE
    lcExpRet =  '<VFP:CustomObject ObjectName="Custariacodes"  SourceControl = "CPICKBY"'+' LPARAMATER = "CPICKERS"'
  ENDIF
  IF TYPE('GLOBALBROWSEWINDOW') = 'O'
    GLOBALBROWSEWINDOW.Container1.cmdselect.VISIBLE = .F.
  ENDIF  
  *: B609855,1 MMT 04/02/2012 Change Filter Expression to be able to apply filters[Start]
  BINDEVENT(GLOBALBROWSEWINDOW,'GetFileFields',loCustObj,'lfAdjFlds',1)
  *: B609855,1 MMT 04/02/2012 Change Filter Expression to be able to apply filters[End]
  RETURN lcExpRet
  
*:**************************************************************************
*:* Name        : lfAction
*! Developer    : Mariam Mazhar
*! Date         : 05/11/2011
*:* Purpose     : Action object
*:***************************************************************************  
FUNCTION lfAction
  IF TYPE('oBrowse') <> 'O'
    lcExpRet =  '<VFP:CustomObject ObjectName="Custariacodes"  SourceControl = "ACTION"'+' LPARAMATER = "ACTION"'
  ELSE
    lcExpRet =  '<VFP:CustomObject ObjectName="Custariacodes"  SourceControl = "ACTION"'+' LPARAMATER = "ACTION"'
  ENDIF
  IF TYPE('GLOBALBROWSEWINDOW') = 'O'
    GLOBALBROWSEWINDOW.Container1.cmdselect.VISIBLE = .F.
  ENDIF  
  
  RETURN lcExpRet
*:**************************************************************************
*:* Name        : lfACTIONTO
*! Developer    : Mariam Mazhar
*! Date         : 05/11/2011
*:* Purpose     : Action To object
*:***************************************************************************
FUNCTION lfACTIONTO
  IF TYPE('oBrowse') <> 'O'
    lcExpRet =  '<VFP:CustomObject ObjectName="Custariacodes"  SourceControl = "ACTIONTO"'+' LPARAMATER = "ACTIONTO"'
  ELSE
    lcExpRet =  '<VFP:CustomObject ObjectName="Custariacodes"  SourceControl = "ACTIONTO"'+' LPARAMATER = "ACTIONTO"'
  ENDIF
  IF TYPE('GLOBALBROWSEWINDOW') = 'O'
    GLOBALBROWSEWINDOW.Container1.cmdselect.VISIBLE = .F.
  ENDIF  
  
  RETURN lcExpRet
*:**************************************************************************
*:* Name        : lfNotes
*! Developer    : Mariam Mazhar
*! Date         : 05/11/2011
*:* Purpose     : Notes object
*:***************************************************************************
FUNCTION lfNotes
  IF TYPE('oBrowse') <> 'O'
    lcExpRet = '<VFP:CustomObject ObjectName="custgridmemocolumn"  SourceControl = "NOTES"'
  ELSE
    lcExpRet =  '<VFP:CustomObject ObjectName="custgridmemocolumn"  SourceControl = "NOTES"'
  ENDIF
  IF TYPE('GLOBALBROWSEWINDOW') = 'O'
    GLOBALBROWSEWINDOW.Container1.cmdselect.VISIBLE = .F.
  ENDIF  

  RETURN lcExpRet
 *: B609855,1 MMT 04/02/2012 Change Filter Expression to be able to apply filters[Start]
 *:**************************************************************************
*:* Name        : PickPackMon class
*! Developer    : Mariam Mazhar
*! Date         : 04/02/2012
*:* Purpose     : adjust fields array
*:***************************************************************************
 DEFINE CLASS PickPackMon as Custom 
   FUNCTION lfAdjFlds
   IF TYPE('GLOBALBROWSEWINDOW.aFileFields[1,1]') = 'C'
     FOR lnCntArr = 1 TO ALEN(GLOBALBROWSEWINDOW.aFileFields,1)
       DO CASE 
        CASE ALLTRIM(UPPER(GLOBALBROWSEWINDOW.aFileFields[lnCntArr ,2])) == 'LFACTION()'
          GLOBALBROWSEWINDOW.aFileFields[lnCntArr ,2] = "lfGetCodeDesc(ACTION,'ACTION')"
        CASE ALLTRIM(UPPER(GLOBALBROWSEWINDOW.aFileFields[lnCntArr ,2])) == 'LFNOTES()'
          GLOBALBROWSEWINDOW.aFileFields[lnCntArr ,2] = 'NOTES'
		CASE ALLTRIM(UPPER(GLOBALBROWSEWINDOW.aFileFields[lnCntArr ,2])) == 'LFPICKBY()'
          GLOBALBROWSEWINDOW.aFileFields[lnCntArr ,2] ="lfGetCodeDesc(CPICKBY, 'CPICKERS')"
        CASE ALLTRIM(UPPER(GLOBALBROWSEWINDOW.aFileFields[lnCntArr ,2])) == 'LFACTIONTO()'
          GLOBALBROWSEWINDOW.aFileFields[lnCntArr ,2] = "lfGetCodeDesc(ACTIONTO, 'ACTIONTO')"
        CASE ALLTRIM(UPPER(GLOBALBROWSEWINDOW.aFileFields[lnCntArr ,2])) == 'GFCODDES(SUBSTR(CHKEDBY,1,6),"CPACKERS  ")'
          GLOBALBROWSEWINDOW.aFileFields[lnCntArr ,2] = "lfGetCodeDesc(CHKEDBY, 'CPACKERS')"&&'CHKEDBY'
       ENDCASE  
     ENDFOR
   ENDIF
 ENDDEFINE
*:**************************************************************************
*:* Name        : lfGetCodeDesc
*! Developer    : Mariam Mazhar
*! Date         : 04/02/2012
*:* Purpose     : Get Code Description function
*:***************************************************************************
 FUNCTION lfGetCodeDesc
 LPARAMETERS lcCodeNum,lcCodeFld
 RETURN GFCODDES(SUBSTR(lcCodeNum,1,6),PADR(lcCodeFld,10))&&"CPACKERS  ")
 *: B609855,1 MMT 04/02/2012 Change Filter Expression to be able to apply filters[End]