//On Ready
$(document).ready(function () {
    //Double click settings for the LIST items
    $("select[name*='lstNonSelected']").dblclick(function () {
        AddSelectedItems();
    });

    $("select[name*='lstSelected']").dblclick(function () {
        RemoveSelectedItems();
    });
});

function AddSelectedItems() {
    MoveSelectedItems(lstNonSelected, lstSelected);
    UpdateButtonState();
}
function AddAllItems() {
    MoveAllItems(lstNonSelected, lstSelected);
    UpdateButtonState();
}
function RemoveSelectedItems() {
    MoveSelectedItems(lstSelected, lstNonSelected);
    UpdateButtonState();
}
function RemoveAllItems() {
    MoveAllItems(lstSelected, lstNonSelected);
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
function UpdateButtonState() {
    btnMoveAllItemsToRight.SetEnabled(lstNonSelected.GetItemCount() > 0);
    btnMoveAllItemsToLeft.SetEnabled(lstSelected.GetItemCount() > 0);
    btnMoveSelectedItemsToRight.SetEnabled(lstNonSelected.GetSelectedItems().length > 0);
    btnMoveSelectedItemsToLeft.SetEnabled(lstSelected.GetSelectedItems().length > 0);
    btnUp.SetEnabled(lstSelected.GetSelectedItems().length > 0 && !IsFirstSelected(lstSelected));
    btnDown.SetEnabled(lstSelected.GetSelectedItems().length > 0 && !IsLastSelected(lstSelected));

}
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

function MoveUp(s, e) {
    lstSelected.BeginUpdate();
    e.processOnServer = false;
    var items = lstSelected.GetSelectedItems();
    for (var i = 0; i < items.length; i++) {
        var item = lstSelected.FindItemByValue(items[i].value);
        if (item.index > 0) {
            lstSelected.RemoveItem(item.index);
            lstSelected.InsertItem(item.index - 1, item.text, item.value);
        }
    }
    lstSelected.EndUpdate();
}

function MoveDown(s, e) {
    lstSelected.BeginUpdate();
    e.processOnServer = false;
    var items = lstSelected.GetSelectedItems();
    for (var i = items.length - 1; i >= 0; i = i - 1) {
        var item = lstSelected.FindItemByValue(items[i].value);
        if (item.index < lstSelected.itemsValue.length - 1) {
            lstSelected.RemoveItem(item.index);
            lstSelected.InsertItem(item.index + 1, item.text, item.value);
        }
    }
    lstSelected.EndUpdate();
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