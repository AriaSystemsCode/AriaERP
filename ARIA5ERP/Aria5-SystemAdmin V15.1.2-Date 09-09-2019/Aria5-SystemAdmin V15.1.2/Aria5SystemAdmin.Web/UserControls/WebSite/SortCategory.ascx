<%@ Control Language="C#" AutoEventWireup="true" CodeBehind="SortCategory.ascx.cs"
    Inherits="AriaDevExpress.Web.UserControls.WebSite.Sort" %>
<style>
    body
    {
        font-size: 13px;
        font-family: Verdana;
    }

    .style1
    {
        width: 100%;
        height: 381px;
    }

    .style2
    {
        width: 167px;
    }

    .style3
    {
        height: 49px;
        text-align: left;
    }
</style>

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
                        <strong>Category :</strong>
                        <dx:aspxcombobox id="CategoryDropDownList" runat="server" valuetype="System.Int32" autopostback="True" datasourceid="CategoryLinqDataSource" textfield="Category_Desc" valuefield="Category_ID"></dx:aspxcombobox>
                    </td>
                </tr>
            </table>
        </td>
    </tr>
    <tr>
        <td>All Articles</td>
        <td>&nbsp;</td>
        <td><i><b>Selected Articles</b></i></td>
        <td>&nbsp;</td>
    </tr>
    <tr>
        <td>
            <dx:aspxlistbox id="lstNonSelected" runat="server" width="300px" rows="10" clientinstancename="lstNonSelected" datasourceid="NonSelectedLinqDataSource"
                textfield="Title" valuefield="Article_ID" height="240px" selectionmode="Multiple">
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
            <dx:aspxlistbox id="lstSelected" width="300px" rows="10" runat="server" clientinstancename="lstSelected" datasourceid="SelectedLinqDataSource"
                textfield="Title" valuefield="Article_ID" height="240px" selectionmode="Multiple">
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

<asp:LinqDataSource ID="CategoryLinqDataSource" runat="server" ContextTypeName="AriaDevExpress.Module.DataContext.AriaOnlineDataContext" EntityTypeName="" Select="new (Category_ID, Category_Desc)" TableName="Categories" />
<asp:LinqDataSource ID="NonSelectedLinqDataSource" OnSelecting="NonSelectedLinqDataSource_Selecting" runat="server" ContextTypeName="AriaDevExpress.Module.DataContext.AriaOnlineDataContext" EntityTypeName="" OrderBy="VisibleRank" Select="new (Article_ID, VisibleRank, Title)" TableName="Articles" Where="Category_ID == null">
    <WhereParameters>
        <asp:ControlParameter ControlID="CategoryDropDownList" Name="Category_ID" PropertyName="SelectedItem.Value" Type="Int32" />
    </WhereParameters>
</asp:LinqDataSource>
<asp:LinqDataSource ID="SelectedLinqDataSource" runat="server" ContextTypeName="AriaDevExpress.Module.DataContext.AriaOnlineDataContext" EntityTypeName="" OrderBy="VisibleRank" Select="new (Article_ID, Title, VisibleRank)" TableName="Articles" Where="Category_ID == Convert.ToInt32(@Category_ID)">
    <WhereParameters>
        <asp:ControlParameter ControlID="CategoryDropDownList" Name="Category_ID" PropertyName="SelectedItem.Value" Type="Int32" />
    </WhereParameters>
</asp:LinqDataSource>

