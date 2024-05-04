<%@ Control Language="C#" AutoEventWireup="true" CodeBehind="BrowseFields.ascx.cs"
    Inherits="AriaDevExpress.Web.UserControls.SysFiles.BrowseFields" %>
<script type="text/javascript">
    // <![CDATA[
    function MoveUp(s, e) {
        lbChoosen.BeginUpdate();
        e.processOnServer = false;
        var items = lbChoosen.GetSelectedItems();
        for (var i = 0; i < items.length; i++) {
            var item = lbChoosen.FindItemByValue(items[i].value);
            if (item.index > 0) {
                lbChoosen.RemoveItem(item.index);
                lbChoosen.InsertItem(item.index - 1, item.text, item.value);
            }
        }
        lbChoosen.EndUpdate();
    }

    function MoveDown(s, e) {
        lbChoosen.BeginUpdate();
        e.processOnServer = false;
        var items = lbChoosen.GetSelectedItems();
        for (var i = items.length - 1; i >= 0; i = i - 1) {
            var item = lbChoosen.FindItemByValue(items[i].value);
            if (item.index < lbChoosen.itemsValue.length - 1) {
                lbChoosen.RemoveItem(item.index);
                lbChoosen.InsertItem(item.index + 1, item.text, item.value);
            }
        }
        lbChoosen.EndUpdate();
    }

    function AddSelectedItems() {
        MoveSelectedItems(lbAvailable, lbChoosen);
        UpdateButtonState();
    }
    function AddAllItems() {
        MoveAllItems(lbAvailable, lbChoosen);
        UpdateButtonState();
    }
    function RemoveSelectedItems() {
        MoveSelectedItems(lbChoosen, lbAvailable);
        UpdateButtonState();
    }
    function RemoveAllItems() {
        MoveAllItems(lbChoosen, lbAvailable);
        UpdateButtonState();
    }
    function MoveSelectedItems(srcListBox, dstListBox) {
        srcListBox.BeginUpdate();
        dstListBox.BeginUpdate();
        var items = srcListBox.GetSelectedItems();
        for (var i = 0; i < items.length; i++) {
            var item = srcListBox.FindItemByValue(items[i].value);
            dstListBox.AddItem(item.text, item.value);
            srcListBox.RemoveItem(item.index);
        }
        dstListBox.UnselectAll();
        srcListBox.UnselectAll();
        srcListBox.EndUpdate();
        dstListBox.EndUpdate();
    }
    function MoveAllItems(srcListBox, dstListBox) {
        srcListBox.BeginUpdate();
        dstListBox.BeginUpdate();

        var count = srcListBox.GetItemCount();
        for (var i = 0; i < count; i++) {
            var item = srcListBox.GetItem(i);
            dstListBox.AddItem(item.text, item.value);
        }
        srcListBox.ClearItems();

        dstListBox.UnselectAll();
        srcListBox.UnselectAll();
        srcListBox.EndUpdate();
        dstListBox.EndUpdate();

    }
    function IsFirstSelected(lb) {
        var items = lbChoosen.GetSelectedItems();
        var selected = false;
        for (var i = 0; i < items.length; i++) {
            if (items[i].index == 0) {
                selected = true;
                break;
            }
        }
        return selected;
    }
    function IsLastSelected(lb) {
        var items = lbChoosen.GetSelectedItems();
        var selected = false;
        for (var i = 0; i < items.length; i++) {
            if (items[i].index == lbChoosen.itemsValue.length - 1) {
                selected = true;
                break;
            }
        }
        return selected;
    }
    function UpdateButtonState() {
        btnMoveAllItemsToRight.SetEnabled(lbAvailable.GetItemCount() > 0);
        btnMoveAllItemsToLeft.SetEnabled(lbChoosen.GetItemCount() > 0);
        btnMoveSelectedItemsToRight.SetEnabled(lbAvailable.GetSelectedItems().length > 0);
        btnMoveSelectedItemsToLeft.SetEnabled(lbChoosen.GetSelectedItems().length > 0);
        btnUp.SetEnabled(lbChoosen.GetSelectedItems().length > 0 && !IsFirstSelected(lbChoosen));
        btnDown.SetEnabled(lbChoosen.GetSelectedItems().length > 0 && !IsLastSelected(lbChoosen));

    }
    window.document.
    // ]]> 
</script>
<dxge:ASPxGlobalEvents ID="GlobalEvents" runat="server">
    <ClientSideEvents ControlsInitialized="function(s, e) { UpdateButtonState(); }" />
</dxge:ASPxGlobalEvents>
<table cellpadding="0" cellspacing="0" width="100%">
    <tr>
        <td valign="top" style="width: 25%">
            <div class="BottomPadding">
                <dx:ASPxLabel ID="lblAvailable" runat="server" Text="Available:" />
            </div>
            <dx:ASPxListBox ID="lbAvailable" EnableViewState="False" runat="server" ClientInstanceName="lbAvailable"
                TextField="Field.Name" ValueField="Field.Name" Width="100%" Height="240px" SelectionMode="CheckColumn"
                OnDataBound="lbAvailable_DataBound">
                <ClientSideEvents SelectedIndexChanged="function(s, e) { UpdateButtonState(); }" />
            </dx:ASPxListBox>
        </td>
        <td valign="middle" align="center" style="padding: 10px; width: 20%">
            <div>
                <dx:ASPxButton ID="btnMoveSelectedItemsToRight" runat="server" ClientInstanceName="btnMoveSelectedItemsToRight"
                    AutoPostBack="False" Text="Add >" Width="130px" Height="23px" ClientEnabled="False"
                    ToolTip="Add selected items">
                    <ClientSideEvents Click="function(s, e) { AddSelectedItems(); }" />
                </dx:ASPxButton>
            </div>
            <div class="TopPadding">
                <dx:ASPxButton ID="btnMoveAllItemsToRight" runat="server" ClientInstanceName="btnMoveAllItemsToRight"
                    AutoPostBack="False" Text="Add All >>" Width="130px" Height="23px" ToolTip="Add all items">
                    <ClientSideEvents Click="function(s, e) { AddAllItems(); }" />
                </dx:ASPxButton>
            </div>
            <div style="height: 32px">
            </div>
            <div>
                <dx:ASPxButton ID="btnMoveSelectedItemsToLeft" runat="server" ClientInstanceName="btnMoveSelectedItemsToLeft"
                    AutoPostBack="False" Text="< Remove" Width="130px" Height="23px" ClientEnabled="False"
                    ToolTip="Remove selected items">
                    <ClientSideEvents Click="function(s, e) { RemoveSelectedItems(); }" />
                </dx:ASPxButton>
            </div>
            <div class="TopPadding">
                <dx:ASPxButton ID="btnMoveAllItemsToLeft" runat="server" ClientInstanceName="btnMoveAllItemsToLeft"
                    AutoPostBack="False" Text="<< Remove All" Width="130px" Height="23px" ClientEnabled="False"
                    ToolTip="Remove all items">
                    <ClientSideEvents Click="function(s, e) { RemoveAllItems(); }" />
                </dx:ASPxButton>
            </div>
        </td>
        <td valign="top" style="width: 25%">
            <div class="BottomPadding">
                <dx:ASPxLabel ID="lblChosen" EnableViewState="false" runat="server" Text="Chosen:" />
            </div>
            <dx:ASPxListBox ID="lbChoosen" runat="server" ClientInstanceName="lbChoosen" Width="100%"
                Height="240px" SelectionMode="CheckColumn">
                <ClientSideEvents SelectedIndexChanged="function(s, e) { UpdateButtonState(); }">
                </ClientSideEvents>
            </dx:ASPxListBox>
        </td>
        <td style="width: 15%">
            <div class="TopPadding">
                <dx:ASPxButton ID="btnUp" runat="server" Text="˄" ClientInstanceName="btnUp">
                    <ClientSideEvents Click="function(s, e) {MoveUp(s,e);UpdateButtonState();}"></ClientSideEvents>
                </dx:ASPxButton>
            </div>
            <div class="BottomPadding">
                <dx:ASPxButton ID="btnDown" ClientInstanceName="btnDown" runat="server" Text="˅">
                    <ClientSideEvents Click="function(s, e) {MoveDown(s,e);UpdateButtonState();}"></ClientSideEvents>
                </dx:ASPxButton>
            </div>
        </td>
    </tr>
</table>
