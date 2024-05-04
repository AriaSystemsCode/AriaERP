using System;
using DevExpress.ExpressApp.Web;
using DevExpress.ExpressApp.Editors;
using DevExpress.ExpressApp.Model;

namespace AriaDevExpress.Module.Web.Controllers.WebSite
{
    public interface IApplicationViewItem : IModelViewItem { }
    [ViewItemAttribute(typeof(IApplicationViewItem))]
    public class ApplicationViewItem : ViewItem
    {
        public ApplicationViewItem(Type objectType, string id) : base(objectType, id) { }
        public ApplicationViewItem(IApplicationViewItem model, Type objectType) : base(objectType, model.Id) { }
        protected override object CreateControlCore()
        {
            return WebWindow.CurrentRequestPage.LoadControl("UserControls/WebSite/SortApplicationModule.ascx");
        }
    }
}
