using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using DevExpress.ExpressApp.Editors;
using System.ComponentModel;
using DevExpress.ExpressApp.Model;
using DevExpress.ExpressApp.Web;

namespace AriaDevExpress.Module.Web.Controllers.ClientManager.ViewItems
{
    public interface INewClientEmptyClassViewItem : IModelViewItem { }
    [ViewItemAttribute(typeof(INewClientEmptyClassViewItem))]
    public class NewClientViewItem : ViewItem
    {
        public NewClientViewItem(Type objectType, string id) : base(objectType, id) { }
        public NewClientViewItem(INewClientEmptyClassViewItem model, Type objectType) : base(objectType, model.Id) { }


        protected override object CreateControlCore()
        {
            return WebWindow.CurrentRequestPage.LoadControl("UserControls/ClientManager/NewClient.ascx");
        }
    }

    public interface IConfigrationEmptyClassViewItem : IModelViewItem { }
    [ViewItemAttribute(typeof(IConfigrationEmptyClassViewItem))]
    public class ConfigrationViewItem : ViewItem
    {
        public ConfigrationViewItem(Type objectType, string id) : base(objectType, id) { }
        public ConfigrationViewItem(IConfigrationEmptyClassViewItem model, Type objectType) : base(objectType, model.Id) { }


        protected override object CreateControlCore()
        {
            return WebWindow.CurrentRequestPage.LoadControl("UserControls/ClientManager/Configration.ascx");
        }
    }
}
