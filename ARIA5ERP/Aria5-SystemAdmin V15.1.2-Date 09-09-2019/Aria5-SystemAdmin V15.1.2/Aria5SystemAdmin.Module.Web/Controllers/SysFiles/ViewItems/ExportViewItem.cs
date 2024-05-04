// Developer Express Code Central Example:
// How to show custom windows in XAF (Example)
// 
// See the http://www.devexpress.com/scid=K18117 KB article for more
// information.
// 
// See
// also:
// http://www.devexpress.com/scid=K18119
// http://www.devexpress.com/scid=K18118
// http://www.devexpress.com/scid=E980
// ShowNavigationItemController.CustomShowNavigationItem
// Event
// (ms-help://DevExpress.Xaf/DevExpressExpressAppSystemModuleShowNavigationItemController_CustomShowNavigationItemtopic.htm)
// XafApplication.CustomProcessShortcut
// Event
// (ms-help://DevExpress.Xaf/DevExpressExpressAppXafApplication_CustomProcessShortcuttopic.htm)
// 
// You can find sample updates and versions for different programming languages here:
// http://www.devexpress.com/example=E911

using System;
using DevExpress.ExpressApp.Web;
using DevExpress.ExpressApp.Editors;
using DevExpress.ExpressApp.Model;
using System.Web.UI;

namespace AriaDevExpress.Module.Web.Controllers.SysFiles.ViewItems
{
    public interface IExportEmptyClassViewItem : IModelViewItem { }
    [ViewItemAttribute(typeof(IExportEmptyClassViewItem))]
    public class ExportViewItem : ViewItem
    {
        public ExportViewItem(Type objectType, string id) : base(objectType, id) { }
        public ExportViewItem(IExportEmptyClassViewItem model, Type objectType) : base(objectType, model.Id) { }

        protected override object CreateControlCore()
        {
            dynamic UserControl = WebWindow.CurrentRequestPage.LoadControl("UserControls/SysFiles/ExportUserControl.ascx");
            return UserControl;
        }
    }
}