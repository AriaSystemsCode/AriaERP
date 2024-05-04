using System;
using DevExpress.ExpressApp.Web;
using DevExpress.ExpressApp.Editors;
using DevExpress.ExpressApp.Model;

namespace AriaDevExpress.Module.Web.Controllers.OneTouchAway
{
    public interface IOTAWAYSortApplicationModuleViewItem : IModelViewItem { }
    [ViewItemAttribute(typeof(IOTAWAYSortApplicationModuleViewItem))]
    public class OTAWAYSortApplicationModuleViewItem : ViewItem
    {
        public OTAWAYSortApplicationModuleViewItem(Type objectType, string id) : base(objectType, id) { }
        public OTAWAYSortApplicationModuleViewItem(IOTAWAYSortApplicationModuleViewItem model, Type objectType) : base(objectType, model.Id) { }
        protected override object CreateControlCore()
        {
            return WebWindow.CurrentRequestPage.LoadControl("UserControls/OneTouchAway/SortAppModule.ascx");
        }
    }
}
