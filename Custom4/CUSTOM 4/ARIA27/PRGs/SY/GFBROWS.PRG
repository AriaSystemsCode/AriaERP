*:************************************************************************
*: Program file  : GFBROWSE.PRG
*: Program desc. : 
*: For screen    :
*:         System: Aria advantage series
*:         Module: Main system 
*:      Developer: 
*:************************************************************************
*: Calls : 
*:         Procedures :
*:         Functions  : 
*:************************************************************************
*: Passed Parameters  : 
*:************************************************************************
*B600383,1 Hesham On 06/07/95
*B600383,1 Clear the key field if browse and there is no records in 
*B600383,1 the file.
*E600823,1 Hesham El-Sheltawi 05/16/96
*E600823,1 Create prefrence for each browse for any file according
*E600823,1 to the name of the function that is browsing the file
*E600823,1 and the file name
*B601183,1 Hesham El-Sheltawi On 07/29/96
*B601183,1 Change the position of the trapping for then enter and
*B601183,1 escape after the checking for the emptines of the file
*B800703,1 Hesham 08/21/96 
*B800703,1 the for condition is clearing after the Enhancment 
*B800703,1 made for the incrementel search 
*B601437,1 make the browse for command browse rest for
*B601456,1 Hesham El-Sheltawi 12/05/96
*B601456,1 Shut down the trapping of the enter and esc buttons
*B601660,1 Hesham El-Sheltawi 12/03/97
*B601660,1 Make the Global Browse Use one read Level
*B601660,1 Change the validation of the push button in the global browse
*B601660,1 window that branch to another screen to Terminate the read
*B601660,1 of the browse
*:************************************************************************
*
PARAMETER lcFltrExp,lcFieldsNam,lcArrName,lcBrowTitle,llIsFilter,lcAliasName,llGetTree
RETURN ARIABROW(lcFltrExp,lcBrowTitle,.F.,.F.,.F.,.F.,.F.,.F.,lcFieldsNam,lcArrName,.F.,lcAliasName,llGetTree)
