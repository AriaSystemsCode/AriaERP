<%@ Control Language="C#" AutoEventWireup="true" CodeBehind="EmailTemplate.ascx.cs"
    Inherits="AriaDevExpress.Web.UserControls.OneTouchAway.Test" %>
<style type="text/css">
    .style1
    {
        width: 100%;
    }
    .style2
    {
        text-align: center;
    }
    .style3
    {
        width: 24px;
    }
</style>
<table class="style1">
    <tr>
        <td colspan="4" style="border-style: none none solid none; border-width: thin; border-color: #000000;
            text-align: center">
            <asp:DropDownList ID="ddlTemplate" ClientIDMode="Static" OnSelectedIndexChanged="ddlTemplate_SelectedIndexChanged"
                AutoPostBack="true" runat="server">
                <asp:ListItem Text="Please Select" Value="None" />
                <asp:ListItem Text="Contact US" Value="contactus" />
                <asp:ListItem Text="Course Registration" Value="coursereg" />
                <asp:ListItem Text="Downloads" Value="downloads" />
                <asp:ListItem Text="Free Trial" Value="freetrail" />
                <asp:ListItem Text="Become Partner" Value="bcmPartnr" />
                <asp:ListItem Text="Event Register" Value="eventreg" />
                <asp:ListItem Text="NewsLetter" Value="News" />
            </asp:DropDownList>
        </td>
    </tr>
    <tr>
        <td colspan="2" style="border-style: none none solid none; border-width: thin; border-color: #000000;
            text-align: center">
            To Aria
        </td>
        <td colspan="2" style="border-style: none none solid none; border-width: thin; border-color: #000000;
            text-align: center">
            To Customer
        </td>
    </tr>
    <tr>
        <td class="style2" style="border-style: none ridge none none; border-width: medium;
            border-color: #808080">
            To ( , seperated )
        </td>
        <td style="border-style: none; border-color: #808080">
            <asp:TextBox ID="txtToA" runat="server" Height="22px" Width="307px"></asp:TextBox>
            <asp:RegularExpressionValidator ID="RegularExpressionValidator1" runat="server" ControlToValidate="txtToA"
                ErrorMessage="Wrong" ValidationExpression="^(([a-zA-Z0-9_\-\.]+)@([a-zA-Z0-9_\-\.]+)\.([a-zA-Z]{2,5}){1,25})+([,.](([a-zA-Z0-9_\-\.]+)@([a-zA-Z0-9_\-\.]+)\.([a-zA-Z]{2,5}){1,25})+)*$"></asp:RegularExpressionValidator>
        </td>
        <td class="style3" rowspan="2" style="border-width: medium; border-color: #C0C0C0;
            border-style: none ridge none none;">
            Subject
        </td>
        <td rowspan="2" style="border-style: none">
            <asp:TextBox ID="txtSubjectC" runat="server" Height="22px" Width="307px"></asp:TextBox>
        </td>
    </tr>
    <tr>
        <td class="style2" style="border-style: none ridge none none; border-width: medium;
            border-color: #808080">
            Subject
        </td>
        <td style="border-style: none; border-color: #808080">
            <asp:TextBox ID="txtSubjectA" runat="server" Height="22px" Width="307px"></asp:TextBox>
        </td>
    </tr>
    <tr>
        <td colspan="2" style="border-style: none ridge none none; border-width: medium;
            border-color: #808080">
            <dx:ASPxHtmlEditor ID="ASPxHtmlEditor1" runat="server" Width="450px">
                <ClientSideEvents CustomCommand="function(s, e) {
	switch(e.commandName) {
               case 'email':
                   s.ExecuteCommand(ASPxClientCommandConsts.PASTEHTML_COMMAND,e.parameter);
                   break;
              }
}" />
                <Toolbars>
                    <dx:HtmlEditorToolbar Name="StandardToolbar1">
                        <Items>
                            <dx:ToolbarCutButton>
                            </dx:ToolbarCutButton>
                            <dx:ToolbarCopyButton>
                            </dx:ToolbarCopyButton>
                            <dx:ToolbarPasteButton>
                            </dx:ToolbarPasteButton>
                            <dx:ToolbarPasteFromWordButton>
                            </dx:ToolbarPasteFromWordButton>
                            <dx:ToolbarUndoButton BeginGroup="True">
                            </dx:ToolbarUndoButton>
                            <dx:ToolbarRedoButton>
                            </dx:ToolbarRedoButton>
                            <dx:ToolbarRemoveFormatButton BeginGroup="True">
                            </dx:ToolbarRemoveFormatButton>
                            <dx:ToolbarSuperscriptButton BeginGroup="True">
                            </dx:ToolbarSuperscriptButton>
                            <dx:ToolbarSubscriptButton>
                            </dx:ToolbarSubscriptButton>
                            <dx:ToolbarInsertOrderedListButton BeginGroup="True">
                            </dx:ToolbarInsertOrderedListButton>
                            <dx:ToolbarInsertUnorderedListButton>
                            </dx:ToolbarInsertUnorderedListButton>
                            <dx:ToolbarIndentButton BeginGroup="True">
                            </dx:ToolbarIndentButton>
                            <dx:ToolbarOutdentButton>
                            </dx:ToolbarOutdentButton>
                            <dx:ToolbarInsertLinkDialogButton BeginGroup="True">
                            </dx:ToolbarInsertLinkDialogButton>
                            <dx:ToolbarUnlinkButton>
                            </dx:ToolbarUnlinkButton>
                            <dx:ToolbarInsertImageDialogButton>
                            </dx:ToolbarInsertImageDialogButton>
                            <dx:ToolbarTableOperationsDropDownButton BeginGroup="True">
                                <Items>
                                    <dx:ToolbarInsertTableDialogButton BeginGroup="True">
                                    </dx:ToolbarInsertTableDialogButton>
                                    <dx:ToolbarTablePropertiesDialogButton BeginGroup="True">
                                    </dx:ToolbarTablePropertiesDialogButton>
                                    <dx:ToolbarTableRowPropertiesDialogButton>
                                    </dx:ToolbarTableRowPropertiesDialogButton>
                                    <dx:ToolbarTableColumnPropertiesDialogButton>
                                    </dx:ToolbarTableColumnPropertiesDialogButton>
                                    <dx:ToolbarTableCellPropertiesDialogButton>
                                    </dx:ToolbarTableCellPropertiesDialogButton>
                                    <dx:ToolbarInsertTableRowAboveButton BeginGroup="True">
                                    </dx:ToolbarInsertTableRowAboveButton>
                                    <dx:ToolbarInsertTableRowBelowButton>
                                    </dx:ToolbarInsertTableRowBelowButton>
                                    <dx:ToolbarInsertTableColumnToLeftButton>
                                    </dx:ToolbarInsertTableColumnToLeftButton>
                                    <dx:ToolbarInsertTableColumnToRightButton>
                                    </dx:ToolbarInsertTableColumnToRightButton>
                                    <dx:ToolbarSplitTableCellHorizontallyButton BeginGroup="True">
                                    </dx:ToolbarSplitTableCellHorizontallyButton>
                                    <dx:ToolbarSplitTableCellVerticallyButton>
                                    </dx:ToolbarSplitTableCellVerticallyButton>
                                    <dx:ToolbarMergeTableCellRightButton>
                                    </dx:ToolbarMergeTableCellRightButton>
                                    <dx:ToolbarMergeTableCellDownButton>
                                    </dx:ToolbarMergeTableCellDownButton>
                                    <dx:ToolbarDeleteTableButton BeginGroup="True">
                                    </dx:ToolbarDeleteTableButton>
                                    <dx:ToolbarDeleteTableRowButton>
                                    </dx:ToolbarDeleteTableRowButton>
                                    <dx:ToolbarDeleteTableColumnButton>
                                    </dx:ToolbarDeleteTableColumnButton>
                                </Items>
                            </dx:ToolbarTableOperationsDropDownButton>
                            <dx:ToolbarFullscreenButton BeginGroup="True">
                            </dx:ToolbarFullscreenButton>
                        </Items>
                    </dx:HtmlEditorToolbar>
                    <dx:HtmlEditorToolbar Name="StandardToolbar2">
                        <Items>
                            <dx:ToolbarParagraphFormattingEdit Width="120px">
                                <Items>
                                    <dx:ToolbarListEditItem Text="Normal" Value="p" />
                                    <dx:ToolbarListEditItem Text="Heading  1" Value="h1" />
                                    <dx:ToolbarListEditItem Text="Heading  2" Value="h2" />
                                    <dx:ToolbarListEditItem Text="Heading  3" Value="h3" />
                                    <dx:ToolbarListEditItem Text="Heading  4" Value="h4" />
                                    <dx:ToolbarListEditItem Text="Heading  5" Value="h5" />
                                    <dx:ToolbarListEditItem Text="Heading  6" Value="h6" />
                                    <dx:ToolbarListEditItem Text="Address" Value="address" />
                                    <dx:ToolbarListEditItem Text="Normal (DIV)" Value="div" />
                                </Items>
                            </dx:ToolbarParagraphFormattingEdit>
                            <dx:ToolbarFontNameEdit>
                                <Items>
                                    <dx:ToolbarListEditItem Text="Times New Roman" Value="Times New Roman" />
                                    <dx:ToolbarListEditItem Text="Tahoma" Value="Tahoma" />
                                    <dx:ToolbarListEditItem Text="Verdana" Value="Verdana" />
                                    <dx:ToolbarListEditItem Text="Arial" Value="Arial" />
                                    <dx:ToolbarListEditItem Text="MS Sans Serif" Value="MS Sans Serif" />
                                    <dx:ToolbarListEditItem Text="Courier" Value="Courier" />
                                </Items>
                            </dx:ToolbarFontNameEdit>
                            <dx:ToolbarFontSizeEdit>
                                <Items>
                                    <dx:ToolbarListEditItem Text="1 (8pt)" Value="1" />
                                    <dx:ToolbarListEditItem Text="2 (10pt)" Value="2" />
                                    <dx:ToolbarListEditItem Text="3 (12pt)" Value="3" />
                                    <dx:ToolbarListEditItem Text="4 (14pt)" Value="4" />
                                    <dx:ToolbarListEditItem Text="5 (18pt)" Value="5" />
                                    <dx:ToolbarListEditItem Text="6 (24pt)" Value="6" />
                                    <dx:ToolbarListEditItem Text="7 (36pt)" Value="7" />
                                </Items>
                            </dx:ToolbarFontSizeEdit>
                            <dx:ToolbarBoldButton BeginGroup="True">
                            </dx:ToolbarBoldButton>
                            <dx:ToolbarItalicButton>
                            </dx:ToolbarItalicButton>
                            <dx:ToolbarUnderlineButton>
                            </dx:ToolbarUnderlineButton>
                            <dx:ToolbarStrikethroughButton>
                            </dx:ToolbarStrikethroughButton>
                            <dx:ToolbarJustifyLeftButton BeginGroup="True">
                            </dx:ToolbarJustifyLeftButton>
                            <dx:ToolbarJustifyCenterButton>
                            </dx:ToolbarJustifyCenterButton>
                            <dx:ToolbarJustifyRightButton>
                            </dx:ToolbarJustifyRightButton>
                            <dx:ToolbarBackColorButton BeginGroup="True">
                            </dx:ToolbarBackColorButton>
                            <dx:ToolbarFontColorButton>
                            </dx:ToolbarFontColorButton>
                        </Items>
                    </dx:HtmlEditorToolbar>
                    <dx:HtmlEditorToolbar Name="email">
                        <Items>
                        </Items>
                    </dx:HtmlEditorToolbar>
                </Toolbars>
                <SettingsImageUpload>
                    <ValidationSettings AllowedFileExtensions=".jpe, .jpeg, .jpg, .gif, .png">
                    </ValidationSettings>
                </SettingsImageUpload>
                <SettingsImageSelector>
                    <CommonSettings AllowedFileExtensions=".jpe, .jpeg, .jpg, .gif, .png"></CommonSettings>
                </SettingsImageSelector>
                <SettingsDocumentSelector>
                    <CommonSettings AllowedFileExtensions=".rtf, .pdf, .doc, .docx, .odt, .txt, .xls, .xlsx, .ods, .ppt, .pptx, .odp">
                    </CommonSettings>
                </SettingsDocumentSelector>
            </dx:ASPxHtmlEditor>
        </td>
        <td colspan="2" style="border-style: none; vertical-align: middle;">
            <dx:ASPxHtmlEditor ID="ASPxHtmlEditor2" runat="server" Width="450px">
                <ClientSideEvents CustomCommand="function(s, e) {
	switch(e.commandName) {
               case 'email':
                   s.ExecuteCommand(ASPxClientCommandConsts.PASTEHTML_COMMAND,e.parameter);
                   break;
              }
}" />
                <Toolbars>
                    <dx:HtmlEditorToolbar Name="StandardToolbar1">
                        <Items>
                            <dx:ToolbarCutButton>
                            </dx:ToolbarCutButton>
                            <dx:ToolbarCopyButton>
                            </dx:ToolbarCopyButton>
                            <dx:ToolbarPasteButton>
                            </dx:ToolbarPasteButton>
                            <dx:ToolbarPasteFromWordButton>
                            </dx:ToolbarPasteFromWordButton>
                            <dx:ToolbarUndoButton BeginGroup="True">
                            </dx:ToolbarUndoButton>
                            <dx:ToolbarRedoButton>
                            </dx:ToolbarRedoButton>
                            <dx:ToolbarRemoveFormatButton BeginGroup="True">
                            </dx:ToolbarRemoveFormatButton>
                            <dx:ToolbarSuperscriptButton BeginGroup="True">
                            </dx:ToolbarSuperscriptButton>
                            <dx:ToolbarSubscriptButton>
                            </dx:ToolbarSubscriptButton>
                            <dx:ToolbarInsertOrderedListButton BeginGroup="True">
                            </dx:ToolbarInsertOrderedListButton>
                            <dx:ToolbarInsertUnorderedListButton>
                            </dx:ToolbarInsertUnorderedListButton>
                            <dx:ToolbarIndentButton BeginGroup="True">
                            </dx:ToolbarIndentButton>
                            <dx:ToolbarOutdentButton>
                            </dx:ToolbarOutdentButton>
                            <dx:ToolbarInsertLinkDialogButton BeginGroup="True">
                            </dx:ToolbarInsertLinkDialogButton>
                            <dx:ToolbarUnlinkButton>
                            </dx:ToolbarUnlinkButton>
                            <dx:ToolbarInsertImageDialogButton>
                            </dx:ToolbarInsertImageDialogButton>
                            <dx:ToolbarTableOperationsDropDownButton BeginGroup="True">
                                <Items>
                                    <dx:ToolbarInsertTableDialogButton BeginGroup="True">
                                    </dx:ToolbarInsertTableDialogButton>
                                    <dx:ToolbarTablePropertiesDialogButton BeginGroup="True">
                                    </dx:ToolbarTablePropertiesDialogButton>
                                    <dx:ToolbarTableRowPropertiesDialogButton>
                                    </dx:ToolbarTableRowPropertiesDialogButton>
                                    <dx:ToolbarTableColumnPropertiesDialogButton>
                                    </dx:ToolbarTableColumnPropertiesDialogButton>
                                    <dx:ToolbarTableCellPropertiesDialogButton>
                                    </dx:ToolbarTableCellPropertiesDialogButton>
                                    <dx:ToolbarInsertTableRowAboveButton BeginGroup="True">
                                    </dx:ToolbarInsertTableRowAboveButton>
                                    <dx:ToolbarInsertTableRowBelowButton>
                                    </dx:ToolbarInsertTableRowBelowButton>
                                    <dx:ToolbarInsertTableColumnToLeftButton>
                                    </dx:ToolbarInsertTableColumnToLeftButton>
                                    <dx:ToolbarInsertTableColumnToRightButton>
                                    </dx:ToolbarInsertTableColumnToRightButton>
                                    <dx:ToolbarSplitTableCellHorizontallyButton BeginGroup="True">
                                    </dx:ToolbarSplitTableCellHorizontallyButton>
                                    <dx:ToolbarSplitTableCellVerticallyButton>
                                    </dx:ToolbarSplitTableCellVerticallyButton>
                                    <dx:ToolbarMergeTableCellRightButton>
                                    </dx:ToolbarMergeTableCellRightButton>
                                    <dx:ToolbarMergeTableCellDownButton>
                                    </dx:ToolbarMergeTableCellDownButton>
                                    <dx:ToolbarDeleteTableButton BeginGroup="True">
                                    </dx:ToolbarDeleteTableButton>
                                    <dx:ToolbarDeleteTableRowButton>
                                    </dx:ToolbarDeleteTableRowButton>
                                    <dx:ToolbarDeleteTableColumnButton>
                                    </dx:ToolbarDeleteTableColumnButton>
                                </Items>
                            </dx:ToolbarTableOperationsDropDownButton>
                            <dx:ToolbarFullscreenButton BeginGroup="True">
                            </dx:ToolbarFullscreenButton>
                        </Items>
                    </dx:HtmlEditorToolbar>
                    <dx:HtmlEditorToolbar Name="StandardToolbar2">
                        <Items>
                            <dx:ToolbarParagraphFormattingEdit Width="120px">
                                <Items>
                                    <dx:ToolbarListEditItem Text="Normal" Value="p" />
                                    <dx:ToolbarListEditItem Text="Heading  1" Value="h1" />
                                    <dx:ToolbarListEditItem Text="Heading  2" Value="h2" />
                                    <dx:ToolbarListEditItem Text="Heading  3" Value="h3" />
                                    <dx:ToolbarListEditItem Text="Heading  4" Value="h4" />
                                    <dx:ToolbarListEditItem Text="Heading  5" Value="h5" />
                                    <dx:ToolbarListEditItem Text="Heading  6" Value="h6" />
                                    <dx:ToolbarListEditItem Text="Address" Value="address" />
                                    <dx:ToolbarListEditItem Text="Normal (DIV)" Value="div" />
                                </Items>
                            </dx:ToolbarParagraphFormattingEdit>
                            <dx:ToolbarFontNameEdit>
                                <Items>
                                    <dx:ToolbarListEditItem Text="Times New Roman" Value="Times New Roman" />
                                    <dx:ToolbarListEditItem Text="Tahoma" Value="Tahoma" />
                                    <dx:ToolbarListEditItem Text="Verdana" Value="Verdana" />
                                    <dx:ToolbarListEditItem Text="Arial" Value="Arial" />
                                    <dx:ToolbarListEditItem Text="MS Sans Serif" Value="MS Sans Serif" />
                                    <dx:ToolbarListEditItem Text="Courier" Value="Courier" />
                                </Items>
                            </dx:ToolbarFontNameEdit>
                            <dx:ToolbarFontSizeEdit>
                                <Items>
                                    <dx:ToolbarListEditItem Text="1 (8pt)" Value="1" />
                                    <dx:ToolbarListEditItem Text="2 (10pt)" Value="2" />
                                    <dx:ToolbarListEditItem Text="3 (12pt)" Value="3" />
                                    <dx:ToolbarListEditItem Text="4 (14pt)" Value="4" />
                                    <dx:ToolbarListEditItem Text="5 (18pt)" Value="5" />
                                    <dx:ToolbarListEditItem Text="6 (24pt)" Value="6" />
                                    <dx:ToolbarListEditItem Text="7 (36pt)" Value="7" />
                                </Items>
                            </dx:ToolbarFontSizeEdit>
                            <dx:ToolbarBoldButton BeginGroup="True">
                            </dx:ToolbarBoldButton>
                            <dx:ToolbarItalicButton>
                            </dx:ToolbarItalicButton>
                            <dx:ToolbarUnderlineButton>
                            </dx:ToolbarUnderlineButton>
                            <dx:ToolbarStrikethroughButton>
                            </dx:ToolbarStrikethroughButton>
                            <dx:ToolbarJustifyLeftButton BeginGroup="True">
                            </dx:ToolbarJustifyLeftButton>
                            <dx:ToolbarJustifyCenterButton>
                            </dx:ToolbarJustifyCenterButton>
                            <dx:ToolbarJustifyRightButton>
                            </dx:ToolbarJustifyRightButton>
                            <dx:ToolbarBackColorButton BeginGroup="True">
                            </dx:ToolbarBackColorButton>
                            <dx:ToolbarFontColorButton>
                            </dx:ToolbarFontColorButton>
                        </Items>
                    </dx:HtmlEditorToolbar>
                </Toolbars>
                <SettingsImageUpload>
                    <ValidationSettings AllowedFileExtensions=".jpe, .jpeg, .jpg, .gif, .png">
                    </ValidationSettings>
                </SettingsImageUpload>
                <SettingsImageSelector>
                    <CommonSettings AllowedFileExtensions=".jpe, .jpeg, .jpg, .gif, .png"></CommonSettings>
                </SettingsImageSelector>
                <SettingsDocumentSelector>
                    <CommonSettings AllowedFileExtensions=".rtf, .pdf, .doc, .docx, .odt, .txt, .xls, .xlsx, .ods, .ppt, .pptx, .odp">
                    </CommonSettings>
                </SettingsDocumentSelector>
            </dx:ASPxHtmlEditor>
        </td>
    </tr>
    <tr>
        <td class="style2">
            &nbsp;
        </td>
        <td>
            &nbsp;
        </td>
        <td class="style3" style="border: medium none #808080;">
            &nbsp;
        </td>
        <td>
            &nbsp;
        </td>
    </tr>
    <tr>
        <td class="style2" colspan="4">
            <asp:Button runat="server" ID="btnSave" Text="Submit" Width="129px" OnClick="btnSave_Click"
                UseSubmitBehavior="False" />
        </td>
    </tr>
</table>
