*:************************************************************************
*: Procedure file: OGMemoFld.PRG
*:         System: ARIA 2.7
*:         Module: SY
*:         Author: MAB (Mohamed Atia Badran)
*:      Copyright (c) 
*:  Last modified: 12/04/2000
*:  Documented   : 12/04/2000
*:************************************************************************
*: Passed Parameters : 1) Filter Array Name such as "laOGFxFlt"
*:                   : 2) Postion within the Filter array.
*:************************************************************************
*: Called From       : gfOpGrid.Prg
*:************************************************************************
*!E500393,1 Use gfZoom to deal with Memo Fields.
*!C102077,1 HBG 01/02/2001 Call Trigger for Cathy Danials to Disabel the memo fields
*!B604395,1 WAB 04/26/2001 Give the user the ability to edit sales & prod. description (CATHY Danials)
*:************************************************************************

PARAMETERS lcFltArray , lnPosInArr
PRIVATE lcFltArray , lnPosInArr, laLstModes

*-- Save Screen modes Array [Begin]
DECLARE laLstModes[ALEN(laScrMode),1]
=ACOPY(laScrMode,laLstModes)
*-- Save Screen modes Array [End  ]

laScrMode = .F.
*-- if OG View Mode only.
IF lcRunDirct = 'X'
  laScrMode[2] = .T.
ELSE  && else OG Edit mode.
  laScrMode[3] = .T.
ENDIF

*!B604395,1 WAB (Start) commit the next line to givr the user ability to edit the memo field.
**C102077,1 HBG 01/02/2001 Call Trigger for Cathy Danials to Disabel the memo fields[Begin]
*IF ASCAN(laEvntTrig,PADR('CHGMODE',10)) <> 0
*  = gfDoTriger('ICSTYLE',PADR('CHGMODE',10))
*ENDIF   
**C102077,1 [End]
*!B604395,1 WAB (END)

=gfZoom("&lcFltArray[lnPosInArr,6]")

*-- Restore Screen modes Array [Begin]
=ACOPY(laLstModes,laScrMode)
*-- Restore Screen modes Array [End  ]

*-- end of OGMemoFld.