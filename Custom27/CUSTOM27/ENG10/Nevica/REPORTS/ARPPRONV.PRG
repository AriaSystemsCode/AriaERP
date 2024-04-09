*:***************************************************************************
*: Program file  : ARPPRONV.PRG
*: Program desc. : Customized Invoice Form Nevica (ENGLAND) ( 
*: Date          : 09/05/2007
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : Tarek MOHAMMED IBRAHIM
*: Tracking Job Number: C200850 - T20070726.0002 
* This form is based completly on the MBI ARPPROA form
* This custom is to allow to show one of the fields STYLE.CDESC01, ..., STYLE.CDESC20 
* There is added a field CUSTOMER.CSTYDESC that stores one of the above fields
* if this field is empty then show the style.desc otherwise show the contents of the stored field
*:***************************************************************************

*- Get the value of the lnClrLen , lnClrPos variables
=lfGetClr()

*- Get the last line in the report
GO BOTTOM
lnLastRec = RECNO()
LOCATE
IF EOF()
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'No records to display.')
ELSE 
  DO gfDispRe WITH EVAL('lcFormName')
ENDIF  
SET DEVICE TO SCREEN
llarpinv = .F.



*:**************************************************************************
*:* Name        : lfGetClr
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 09/05/2007
*:* Purpose     : Get color data
*:***************************************************************************
FUNCTION lfGetClr        
PRIVATE lnCount,laItemSeg,lcOldSelect
DECLARE laItemSeg[1]
lcOldSelect = select(0)

=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLen = LEN(laItemSeg[lnCount,3])
    lnClrPos = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR
SELE (lcOldSelect)

*-- end of lfGetClr

*:**************************************************************************
*:* Name        : lfGtStyDsc
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 09/02/2007
*:* Purpose     : get style description
*:***************************************************************************
*C200850,1 TMI
FUNCTION lfGtStyDsc
PRIVATE lcFldList,lcDesc,lcFld
lcFldList = 'CDESC01|CDESC02|CDESC03|CDESC04|CDESC05|CDESC06|CDESC07|CDESC08|CDESC09|CDESC10|'+;
            'CDESC11|CDESC12|CDESC13|CDESC14|CDESC15|CDESC16|CDESC17|CDESC18|CDESC19|CDESC20|'

lcDesc = ALLT(STYLE.DESC)
IF !EMPTY(CUSTOMER.CSTYDESC) .AND. ;
   UPPER(CUSTOMER.CSTYDESC) $ lcFldList .AND. ;
   !EMPTY(EVAL('STYLE.'+ALLT(CUSTOMER.CSTYDESC)))
  lcDesc = EVAL('STYLE.'+ALLT(CUSTOMER.CSTYDESC))
ENDIF

RETURN lcDesc

*-- end of lfGtStyDsc.


