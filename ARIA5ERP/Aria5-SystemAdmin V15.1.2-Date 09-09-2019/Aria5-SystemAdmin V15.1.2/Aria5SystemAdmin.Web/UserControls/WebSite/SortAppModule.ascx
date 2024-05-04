<%@ Control Language="C#" AutoEventWireup="true" CodeBehind="SortAppModule.ascx.cs" Inherits="AriaDevExpress.Web.UserControls.WebSite.SortAppModule" %>

<dxge:ASPxGlobalEvents ID="GlobalEvents" runat="server">
    <ClientSideEvents ControlsInitialized="function(s, e) { UpdateButtonState(); }" />
</dxge:ASPxGlobalEvents>

<table cellspacing="5">
    <tr>
        <td colspan="4">
            <table>
                <tr>
                    <td width="60%"></td>
                    <td>
                        <strong>Applications :</strong>
                        <dx:aspxcombobox id="ddlApplicationModule" runat="server" valuetype="System.Int32" autopostback="True" datasourceid="ApplicationsDataSource" textfield="ShortDesc" valuefield="AppModID"></dx:aspxcombobox>


                    </td>
                </tr>
            </table>
        </td>
    </tr>
    <tr>
        <td>All Modules</td>
        <td>&nbsp;</td>
        <td><i><b>Selected Modules</b></i></td>
        <td>&nbsp;</td>
    </tr>
    <tr>
        <td>
            <dx:aspxlistbox id="lstNonSelected" runat="server" width="300px" rows="10" clientinstancename="lstNonSelected" datasourceid="AllModuleDataSource"
                textfield="ShortDesc" valuefield="AppModID" height="240px" selectionmode="Multiple">
                    <ClientSideEvents SelectedIndexChanged="function(s, e) { UpdateButtonState(); }" />
                        <ClientSideEvents ItemDoubleClick="function(s, e) { AddSelectedItems(); }" />
                        </dx:aspxlistbox>
        </td>
        <td>
            <dx:aspxbutton id="btnMoveAllItemsToRight" clientinstancename="btnMoveAllItemsToRight" runat="server" text=">>">
                                <ClientSideEvents Click="function(s, e) { AddAllItems(s,e); UpdateButtonState();}">
                                </ClientSideEvents>
                            </dx:aspxbutton>
            <br />
            <dx:aspxbutton id="btnMoveSelectedItemsToRight" clientinstancename="btnMoveSelectedItemsToRight" runat="server" text=">">
                                <ClientSideEvents Click="function(s, e) { AddSelectedItems(s,e); UpdateButtonState();}">
                                </ClientSideEvents>
                            </dx:aspxbutton>
            <br />
            <dx:aspxbutton id="btnMoveSelectedItemsToLeft" clientinstancename="btnMoveSelectedItemsToLeft" runat="server" text="<">
                                <ClientSideEvents Click="function(s, e) { RemoveSelectedItems(s,e); UpdateButtonState();}">
                                </ClientSideEvents>
                            </dx:aspxbutton>
            <br />
            <dx:aspxbutton id="btnMoveAllItemsToLeft" clientinstancename="btnMoveAllItemsToLeft" runat="server" text="<<">
                                <ClientSideEvents Click="function(s, e) { RemoveAllItems(s,e); UpdateButtonState();}">
                                </ClientSideEvents>
                            </dx:aspxbutton>

        </td>
        <td>
            <dx:aspxlistbox id="lstSelected" width="300px" rows="10" runat="server" clientinstancename="lstSelected" datasourceid="SelectedDataSource"
                textfield="ShortDesc" valuefield="AppModID" height="240px" selectionmode="Multiple">
                    <ClientSideEvents SelectedIndexChanged="function(s, e) { UpdateButtonState(); }" />
                <ClientSideEvents ItemDoubleClick="function(s, e) { RemoveSelectedItems(); }" />
            </dx:aspxlistbox>

        </td>
        <td>
            <div class="TopPadding">
                <dx:aspxbutton id="btnUp" runat="server" text="˄" clientinstancename="btnUp">
                                <ClientSideEvents Click="function(s, e) { MoveUp(s,e); UpdateButtonState();}"></ClientSideEvents>

                            </dx:aspxbutton>
            </div>
            <div class="BottomPadding">
                <dx:aspxbutton id="btnDown" clientinstancename="btnDown" runat="server" text="˅">
                                <ClientSideEvents Click="function(s, e) { MoveDown(s,e); UpdateButtonState();}">
                                </ClientSideEvents>
                            </dx:aspxbutton>
            </div>
        </td>
    </tr>
    <tr>
        <td colspan="3"></td>
        <td>
            <dx:aspxbutton id="btnSave" runat="server" text="Update" onclick="btnSave_Click" />
        </td>
    </tr>
</table>
<asp:LinqDataSource ID="ApplicationsDataSource" runat="server" ContextTypeName="AriaDevExpress.Module.DataContext.AriaOnlineDataContext" EntityTypeName="" Select="new (AppModID, ShortDesc, ToolTip)" TableName="ApplicationModules" Where="ParentID == null">
</asp:LinqDataSource>
<asp:LinqDataSource ID="SelectedDataSource" runat="server" ContextTypeName="AriaDevExpress.Module.DataContext.AriaOnlineDataContext" EntityTypeName="" OrderBy="AppRank" Select="new (AppModID, ShortDesc, ToolTip)" TableName="ApplicationModules" Where="ParentID ==  Convert.ToInt32(@ParentID)">
    <WhereParameters>
        <asp:ControlParameter ControlID="ddlApplicationModule" PropertyName="SelectedItem.Value" Name="ParentID" Type="Int32" />
    </WhereParameters>
</asp:LinqDataSource>
<asp:LinqDataSource ID="AllModuleDataSource" runat="server" ContextTypeName="AriaDevExpress.Module.DataContext.AriaOnlineDataContext" EntityTypeName="" Select="new (AppModID, ShortDesc, ToolTip)" TableName="ApplicationModules" Where="ParentID != Convert.ToInt32(@ParentID) || ParentID == null" OnSelecting="AllModuleDataSource_Selecting">
    <WhereParameters>
        <asp:ControlParameter ControlID="ddlApplicationModule" PropertyName="SelectedItem.Value" Name="ParentID" Type="Int32" />
    </WhereParameters>
</asp:LinqDataSource>
