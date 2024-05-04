using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using DevExpress.ExpressApp.Editors;
using System.ComponentModel;
using DevExpress.ExpressApp.Model;
using DevExpress.ExpressApp.Web;

namespace AriaDevExpress.Module.Web.Controllers.Globalization.ViewItems
{
    public interface IGlovalizationEmptyClassViewItem : IModelViewItem { }


    [ViewItemAttribute(typeof(IGlovalizationEmptyClassViewItem))]
    public class GlovalizationViewItem : ViewItem
    {
        public GlovalizationViewItem(Type objectType, string id) : base(objectType, id) { }
        public GlovalizationViewItem(IGlovalizationEmptyClassViewItem model, Type objectType) : base(objectType, model.Id) { }


        protected override object CreateControlCore()
        {
            return WebWindow.CurrentRequestPage.LoadControl("UserControls/Globalization/Globalization.ascx");
        }
    }
}
