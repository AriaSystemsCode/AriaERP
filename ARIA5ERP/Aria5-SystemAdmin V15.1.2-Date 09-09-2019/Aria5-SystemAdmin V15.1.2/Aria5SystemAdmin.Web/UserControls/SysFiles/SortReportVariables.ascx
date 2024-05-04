<%@ Control Language="C#" AutoEventWireup="true" CodeBehind="SortReportVariables.ascx.cs"
    Inherits="AriaDevExpress.Web.UserControls.SysFiles.SortReportVariable" %>
<script type="text/javascript">
    // <![CDATA[
    function MoveUp(s, e) {
        lbAvailable.BeginUpdate();
        e.processOnServer = false;
        var items = lbAvailable.GetSelectedItems();
        for (var i = 0; i < items.length; i++) {
            var item = lbAvailable.FindItemByValue(items[i].value);
            if (item.index > 0) {
                lbAvailable.RemoveItem(item.index);
                lbAvailable.InsertItem(item.index - 1, item.text, item.value);
            }
        }
        lbAvailable.EndUpdate();
    }

    function MoveDown(s, e) {
        lbAvailable.BeginUpdate();
        e.processOnServer = false;
        var items = lbAvailable.GetSelectedItems();
        for (var i = items.length - 1; i >= 0; i = i - 1) {
            var item = lbAvailable.FindItemByValue(items[i].value);
            if (item.index < lbAvailable.itemsValue.length - 1) {
                lbAvailable.RemoveItem(item.index);
                lbAvailable.InsertItem(item.index + 1, item.text, item.value);
            }
        }
        lbAvailable.EndUpdate();
    }

    //    function AddSelectedItems() {
    //        MoveSelectedItems(lbAvailable, lbChoosen);
    //        UpdateButtonState();
    //    }
    //    function AddAllItems() {
    //        MoveAllItems(lbAvailable, lbChoosen);
    //        UpdateButtonState();
    //    }
    //    function RemoveSelectedItems() {
    //        MoveSelectedItems(lbChoosen, lbAvailable);
    //        UpdateButtonState();
    //    }
    //    function RemoveAllItems() {
    //        MoveAllItems(lbChoosen, lbAvailable);
    //        UpdateButtonState();
    //    }
    //    function MoveSelectedItems(srcListBox, dstListBox) {
    //        srcListBox.BeginUpdate();
    //        dstListBox.BeginUpdate();
    //        var items = srcListBox.GetSelectedItems();
    //        for (var i = 0; i < items.length; i++) {
    //            var item = srcListBox.FindItemByValue(items[i].value);
    //            dstListBox.AddItem(item.text, item.value);
    //            srcListBox.RemoveItem(item.index);
    //        }
    //        dstListBox.UnselectAll();
    //        srcListBox.UnselectAll();
    //        srcListBox.EndUpdate();
    //        dstListBox.EndUpdate();
    //    }
    //    function MoveAllItems(srcListBox, dstListBox) {
    //        srcListBox.BeginUpdate();
    //        dstListBox.BeginUpdate();

    //        var count = srcListBox.GetItemCount();
    //        for (var i = 0; i < count; i++) {
    //            var item = srcListBox.GetItem(i);
    //            dstListBox.AddItem(item.text, item.value);
    //        }
    //        srcListBox.ClearItems();

    //        dstListBox.UnselectAll();
    //        srcListBox.UnselectAll();
    //        srcListBox.EndUpdate();
    //        dstListBox.EndUpdate();

    //    }
    function IsFirstSelected(lb) {
        var items = lb.GetSelectedItems();
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
        var items = lb.GetSelectedItems();
        var selected = false;
        for (var i = 0; i < items.length; i++) {
            if (items[i].index == lb.itemsValue.length - 1) {
                selected = true;
                break;
            }
        }
        return selected;
    }
    function UpdateButtonState() {
        //        btnMoveAllItemsToRight.SetEnabled(lbAvailable.GetItemCount() > 0);
        //        btnMoveAllItemsToLeft.SetEnabled(lbChoosen.GetItemCount() > 0);
        //        btnMoveSelectedItemsToRight.SetEnabled(lbAvailable.GetSelectedItems().length > 0);
        //        btnMoveSelectedItemsToLeft.SetEnabled(lbChoosen.GetSelectedItems().length > 0);
        btnUp.SetEnabled(lbAvailable.GetSelectedItems().length > 0 && !IsFirstSelected(lbAvailable));
        btnDown.SetEnabled(lbAvailable.GetSelectedItems().length > 0 && !IsLastSelected(lbAvailable));

    }
    // ]]> 
</script>
<dxge:ASPxGlobalEvents ID="GlobalEvents" runat="server">
    <ClientSideEvents ControlsInitialized="function(s, e) { UpdateButtonState(); }" />
</dxge:ASPxGlobalEvents>
<table cellpadding="0" cellspacing="0" width="100%">
    <tr>
        <td style="width: 25%">
        </td>
        <td valign="top" style="width: 25%">
            <div class="BottomPadding">
                <dx:ASPxLabel ID="lblAvailable" runat="server" Text="Available:" />
            </div>
            <dx:ASPxListBox ID="lbAvailable" EnableViewState="False" runat="server" ClientInstanceName="lbAvailable"
                TextField="Name" ValueField="Name" Width="100%" Height="240px" SelectionMode="Single">
                <ClientSideEvents SelectedIndexChanged="function(s, e) { UpdateButtonState(); }" />
            </dx:ASPxListBox>
        </td>
        <td style="width: 10%;">
        </td>
        <td style="width: 15%">
            <div class="TopPadding">
                <dx:ASPxButton ID="btnUp" runat="server" Text="˄" ClientInstanceName="btnUp">
                    <ClientSideEvents Click="function(s, e) { MoveUp(s,e); UpdateButtonState();}"></ClientSideEvents>
                </dx:ASPxButton>
            </div>
            <div class="BottomPadding">
                <dx:ASPxButton ID="btnDown" ClientInstanceName="btnDown" runat="server" Text="˅">
                    <ClientSideEvents Click="function(s, e) { MoveDown(s,e); UpdateButtonState();}">
                    </ClientSideEvents>
                </dx:ASPxButton>
            </div>
        </td>
    </tr>
</table>
