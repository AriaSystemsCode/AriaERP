using System;
using DevExpress.ExpressApp.Web;
using DevExpress.ExpressApp.Editors;
using DevExpress.ExpressApp.Model;

namespace AriaDevExpress.Module.Web.Controllers.WebSite
{
    public interface ISortApplicationModuleViewItem : IModelViewItem { }
    [ViewItemAttribute(typeof(ISortApplicationModuleViewItem))]
    public class SortApplicationModuleViewItem : ViewItem
    {
        public SortApplicationModuleViewItem(Type objectType, string id) : base(objectType, id) { }
        public SortApplicationModuleViewItem(ISortApplicationModuleViewItem model, Type objectType) : base(objectType, model.Id) { }
        protected override object CreateControlCore()
        {
            return WebWindow.CurrentRequestPage.LoadControl("UserControls/WebSite/SortAppModule.ascx");
        }
    }
}
