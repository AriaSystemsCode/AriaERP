using System;
using DevExpress.ExpressApp.Web;
using DevExpress.ExpressApp.Editors;
using DevExpress.ExpressApp.Model;

namespace AriaDevExpress.Module.Web.Controllers.OneTouchAway
{
    public interface IOTAWAYApplicationViewItem : IModelViewItem { }
    [ViewItemAttribute(typeof(IOTAWAYApplicationViewItem))]
    public class OTAWAYApplicationViewItem : ViewItem
    {
        public OTAWAYApplicationViewItem(Type objectType, string id) : base(objectType, id) { }
        public OTAWAYApplicationViewItem(IOTAWAYApplicationViewItem model, Type objectType) : base(objectType, model.Id) {  }
        protected override object CreateControlCore()
        {
            return WebWindow.CurrentRequestPage.LoadControl("UserControls/OneTouchAway/SortApplicationModule.ascx");
        }
    }
}
