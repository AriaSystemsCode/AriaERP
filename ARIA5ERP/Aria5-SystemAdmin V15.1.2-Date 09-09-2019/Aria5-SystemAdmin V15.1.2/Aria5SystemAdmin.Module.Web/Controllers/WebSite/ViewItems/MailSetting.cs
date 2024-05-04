using System;
using DevExpress.ExpressApp.Web;
using DevExpress.ExpressApp.Editors;
using DevExpress.ExpressApp.Model;

namespace AriaDevExpress.Module.Web.Controllers.WebSite
{
    public interface IMailSettingViewItem : IModelViewItem { }
    [ViewItemAttribute(typeof(IMailSettingViewItem))]
    public class MailSettingViewItem : ViewItem
    {
        public MailSettingViewItem(Type objectType, string id) : base(objectType, id) { }
        public MailSettingViewItem(IMailSettingViewItem model, Type objectType) : base(objectType, model.Id) { }
        protected override object CreateControlCore()
        {
            return WebWindow.CurrentRequestPage.LoadControl("UserControls/WebSite/EmailSetting.ascx");
        }
    }
}
