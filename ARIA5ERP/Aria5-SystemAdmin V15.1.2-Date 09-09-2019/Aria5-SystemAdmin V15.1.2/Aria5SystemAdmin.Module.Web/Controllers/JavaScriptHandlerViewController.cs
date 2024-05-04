using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using DevExpress.ExpressApp.Web;
using DevExpress.ExpressApp.Web.SystemModule;
using AriaDevExpress.Module.BusinessObjects.SysFiles;
using DevExpress.ExpressApp.Actions;
using DevExpress.ExpressApp;
using AriaDevExpress.Module.Web.Controllers.OneTouchAway;

namespace AriaDevExpress.Module.Web.Controllers.WebSite
{
    public class JavaScriptHandlerViewController : DevExpress.ExpressApp.ViewController
    {
        protected override void OnViewControlsCreated()
        {
            if (((DevExpress.ExpressApp.Frame)(DevExpress.ExpressApp.Web.WebWindow.CurrentRequestWindow)).View.Id == "SortApp_DetailView")
            {
                WebWindow.CurrentRequestWindow.RegisterClientScriptInclude("JS2", "http://ajax.googleapis.com/ajax/libs/jquery/1.4/jquery.min.js");
                WebWindow.CurrentRequestWindow.RegisterClientScriptInclude("JS", "UserControls/WebSite/SortAppModule.js");
            }
            else if (((DevExpress.ExpressApp.Frame)(DevExpress.ExpressApp.Web.WebWindow.CurrentRequestWindow)).View.Id == "MR_Application_DetailView")
            {
                WebWindow.CurrentRequestWindow.RegisterClientScriptInclude("JS2", "http://ajax.googleapis.com/ajax/libs/jquery/1.4/jquery.min.js");
                WebWindow.CurrentRequestWindow.RegisterClientScriptInclude("JS", "UserControls/WebSite/SortApplicationModule.js");
            }
            else if (((DevExpress.ExpressApp.Frame)(DevExpress.ExpressApp.Web.WebWindow.CurrentRequestWindow)).View.Id == "SortCategory_DetailView")
            {
                WebWindow.CurrentRequestWindow.RegisterClientScriptInclude("JS2", "http://ajax.googleapis.com/ajax/libs/jquery/1.4/jquery.min.js");
                WebWindow.CurrentRequestWindow.RegisterClientScriptInclude("JS", "UserControls/WebSite/SortAppModule.js");
            }
            else if (((DevExpress.ExpressApp.Frame)(DevExpress.ExpressApp.Web.WebWindow.CurrentRequestWindow)).View.Id == "NewClient_DetailView")
            {
                WebWindow.CurrentRequestWindow.RegisterClientScriptInclude("JS", "UserControls/ClientManager/NewClient.js");
            }

            else if (((DevExpress.ExpressApp.Frame)(DevExpress.ExpressApp.Web.WebWindow.CurrentRequestWindow)).View.Id == "OTAWAYSortApp_DetailView")
            {
                WebWindow.CurrentRequestWindow.RegisterClientScriptInclude("JS2", "http://ajax.googleapis.com/ajax/libs/jquery/1.4/jquery.min.js");
                WebWindow.CurrentRequestWindow.RegisterClientScriptInclude("JS", "UserControls/OneTouchAway/SortAppModule.js");
            }
            else if (((DevExpress.ExpressApp.Frame)(DevExpress.ExpressApp.Web.WebWindow.CurrentRequestWindow)).View.Id == "OTAWAYMR_Application_DetailView")
            {
                WebWindow.CurrentRequestWindow.RegisterClientScriptInclude("JS2", "http://ajax.googleapis.com/ajax/libs/jquery/1.4/jquery.min.js");
                WebWindow.CurrentRequestWindow.RegisterClientScriptInclude("JS", "UserControls/OneTouchAway/SortApplicationModule.js");
            }
            else if (((DevExpress.ExpressApp.Frame)(DevExpress.ExpressApp.Web.WebWindow.CurrentRequestWindow)).View.Id == "OTAWAYSortCategory_DetailView")
            {
                WebWindow.CurrentRequestWindow.RegisterClientScriptInclude("JS2", "http://ajax.googleapis.com/ajax/libs/jquery/1.4/jquery.min.js");
                WebWindow.CurrentRequestWindow.RegisterClientScriptInclude("JS", "UserControls/OneTouchAway/SortAppModule.js");
            }
        }
    }
}
