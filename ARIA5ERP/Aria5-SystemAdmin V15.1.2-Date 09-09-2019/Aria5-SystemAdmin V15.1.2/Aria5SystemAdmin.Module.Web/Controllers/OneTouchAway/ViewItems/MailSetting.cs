using System;
using DevExpress.ExpressApp.Web;
using DevExpress.ExpressApp.Editors;
using DevExpress.ExpressApp.Model;

namespace AriaDevExpress.Module.Web.Controllers.OneTouchAway
{
    public interface IOTAWAYMailSettingViewItem : IModelViewItem { }
    [ViewItemAttribute(typeof(IOTAWAYMailSettingViewItem))]
    public class OTAWAYMailSettingViewItem : ViewItem
    {
        public OTAWAYMailSettingViewItem(Type objectType, string id) : base(objectType, id) { }
        public OTAWAYMailSettingViewItem(IOTAWAYMailSettingViewItem model, Type objectType) : base(objectType, model.Id) { }
        protected override object CreateControlCore()
        {
            return WebWindow.CurrentRequestPage.LoadControl("UserControls/OneTouchAway/EmailSetting.ascx");
        }
    }
}
