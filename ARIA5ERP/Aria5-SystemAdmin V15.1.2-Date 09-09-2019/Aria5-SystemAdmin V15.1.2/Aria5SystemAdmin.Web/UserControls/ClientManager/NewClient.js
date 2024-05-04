
function UpdateButtonState() {
    if (typeof btnDeleteCompany != "undefined")
        btnDeleteCompany.SetEnabled(lstComapnies.GetSelectedItems().length > 0);
    if (typeof btnDeleteUser != "undefined")
        btnDeleteUser.SetEnabled(lstUsers.GetSelectedItems().length > 0);
}
function Next() {
    var currentTabIndex = Tabs.GetActiveTabIndex();
    currentTabIndex++;
    Tabs.SetActiveTabIndex(currentTabIndex);
}

function Prev() {
    var currentTabIndex = Tabs.GetActiveTabIndex();
    currentTabIndex--;
    Tabs.SetActiveTabIndex(currentTabIndex);
}
function TabChanged() {
    var currentTabIndex = Tabs.GetActiveTabIndex();
    btnNext.SetEnabled(Tabs.GetTabCount() != currentTabIndex + 1);
    btnPrev.SetEnabled(currentTabIndex != 0);
}

function ClientCodeChanged() {
    var clientCode = txtClientCode.GetText();
    clientCode = clientCode.toUpperCase();
    txtClientCode.SetText(clientCode);


    var password = clientCode.substring(0, 3).toLowerCase();

    txtClientUserNameAria4.SetText(clientCode);
    txtClientPasswordAria4.SetText(password);
    txtDataBaseNameClientMaster.SetText(clientCode + ".Master");
    txtClientUserNameClientMaster.SetText(clientCode);
    txtClientPasswordClientMaster.SetText(password);
}

var readyStateCheckInterval = setInterval(function () {
    if (document.readyState === "complete") {
        init();
        clearInterval(readyStateCheckInterval);
    }
}, 10);

function init() {
    UpdateButtonState();
    TabChanged();
}