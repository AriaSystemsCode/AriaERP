
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

function UpdateButtonState() {
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