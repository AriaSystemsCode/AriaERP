<%@ Control Language="C#" AutoEventWireup="true" CodeBehind="SortApplicationModule.ascx.cs" Inherits="AriaDevExpress.Web.UserControls.WebSite.SortApplicationModule" %>
<table width="100%">
    <tr>
        <td width="30%">&nbsp;</td>
        <td style="vertical-align: top">
            <table cellspacing="5">
                <tr>
                    <td colspan="4" style="color: #000000; font-size: medium;" class="style3">&nbsp;<strong>Applications :</strong>
                    </td>
                </tr>
                <tr>
                    <td>&nbsp;</td>
                    <td>&nbsp;</td>
                    <td>&nbsp;</td>
                    <td>&nbsp;</td>
                </tr>
                <tr>
                    <td>&nbsp;</td>
                    <td>
                        <br />
                        <br />
                        &nbsp;</td>

                    <td>
                        <dx:aspxlistbox id="lstSelected" width="300px" rows="10" runat="server" clientinstancename="lstSelected" datasourceid="ApplicationLinqDataSource"
                            textfield="ShortDesc" valuefield="AppModID" height="240px" selectionmode="Multiple">
                            <ClientSideEvents SelectedIndexChanged="function(s, e) { UpdateButtonState(); }" />
                        </dx:aspxlistbox>
                    </td>
                    <td>
                        <dx:aspxbutton id="btnUp" runat="server" text="˄" clientinstancename="btnUp">
                                <ClientSideEvents Click="function(s, e) { MoveUp(s,e); UpdateButtonState();}"></ClientSideEvents>
                            </dx:aspxbutton>
                        <br />
                        <dx:aspxbutton id="btnDown" clientinstancename="btnDown" runat="server" text="˅">
                                <ClientSideEvents Click="function(s, e) { MoveDown(s,e); UpdateButtonState();}"></ClientSideEvents>
                            </dx:aspxbutton>
                    </td>
                </tr>
                <tr>
                    <td colspan="3">&nbsp;</td>
                    <td  align="center" style="text-align: right">
                        <dx:aspxbutton id="btnSave" runat="server" text="Update" onclick="btnSave_Click" />
                    </td>
                </tr>

            </table>
        </td>

    </tr>
</table>

<asp:LinqDataSource ID="ApplicationLinqDataSource" runat="server" ContextTypeName="AriaDevExpress.Module.DataContext.AriaOnlineDataContext" EntityTypeName="" OrderBy="AppRank" Select="new (AppModID, ToolTip, ShortDesc, AppRank)" TableName="ApplicationModules" Where="ParentID == null" />
