using System;
using System.Linq;
using System.Text;
using DevExpress.ExpressApp;
using DevExpress.Data.Filtering;
using System.Collections.Generic;
using DevExpress.Persistent.Base;
using DevExpress.ExpressApp.Utils;
using DevExpress.ExpressApp.Layout;
using DevExpress.ExpressApp.Actions;
using DevExpress.ExpressApp.Editors;
using DevExpress.ExpressApp.Templates;
using DevExpress.Persistent.Validation;
using DevExpress.ExpressApp.SystemModule;
using DevExpress.ExpressApp.Model.NodeGenerators;
using Aria5SystemAdmin.Module.Managers;
//using Microsoft.TeamFoundation.Framework.Client;
using Aria5SystemAdmin.Module.BusinessObjects;
using DevExpress.Xpo;
using DevExpress.ExpressApp.Xpo;
using Aria5SystemAdmin.Module.SubAutoTask1;
using System.Data.SqlClient;
using System.ServiceModel;

namespace Aria5SystemAdmin.Module.Controllers
{
    // For more typical usage scenarios, be sure to check out http://documentation.devexpress.com/#Xaf/clsDevExpressExpressAppViewControllertopic.
    public partial class TrackingTaskViewController : ViewController
    {

        public TrackingTaskViewController()
        {
            InitializeComponent();
            RegisterActions(components);
            // Target required Views (via the TargetXXX properties) and create their Actions.
        }
        protected override void OnActivated()
        {
            base.OnActivated();
            // Perform various tasks depending on the target View.
        }
        protected override void OnViewControlsCreated()
        {
            base.OnViewControlsCreated();
            // Access and customize the target View control.
        }
        protected override void OnDeactivated()
        {
            // Unsubscribe from previously subscribed events and release other references and resources.
            base.OnDeactivated();
        }

        private ATWSSoapClient clientAuto;
        private AutotaskIntegrations at_integrations;
        SqlConnection connection;
        SqlCommand command;
        SqlDataReader reader;
        private string auth_user_id = "PPMO@ariasystems.biz"; // user@domain.com
        private string auth_user_password = "password7asbyraby";
        private ATWSZoneInfo zoneInfo = null;
        private long resourceID;
        private BasicHttpBinding myBinding;
        private ATWSSoapClient client;
        private void GetRescources_Execute(object sender, SimpleActionExecuteEventArgs e)
        {



         
            TfsManager managerTFS = new TfsManager();

            //  List<TeamFoundationIdentity> UserIds = managerTFS.GetResourceList();
            IObjectSpace objectSpace = Application.CreateObjectSpace();
            TrackingTask ts = new TrackingTask(((XPObjectSpace)this.ObjectSpace).Session);
            //     var rr = ((XPObjectSpace)this.ObjectSpace).Session.FindObject<Resources>(CriteriaOperator.Parse("[Name] is not null"));

            bool exist = false;
            XPCollection collection = new XPCollection(((XPObjectSpace)this.ObjectSpace).Session, typeof(Resources), CriteriaOperator.Parse("[Name] is not null"));

            collection.Load();

            //foreach (TeamFoundationIdentity user in UserIds)
            //{

            //    if (collection.Count > 0)
            //    {
            //        foreach (Resources name in collection)
            //        {
            //            if (user.DisplayName == name.Name)
            //            {
            //                exist = true;
            //            }


            //        }
            //        if (exist == false)
            //        {
            //            Resources r = new Resources(((XPObjectSpace)this.ObjectSpace).Session);
            //            r.Name = user.DisplayName;
            //            r.Save();
            //            r.Session.CommitTransaction();
            //        }
            //        exist = false;
            //    }
            //    else
            //    {
            //        Resources r = new Resources(((XPObjectSpace)this.ObjectSpace).Session);
            //        r.Name = user.DisplayName;
            //        r.Save();
            //        r.Session.CommitTransaction();
            //    }
            //}
        }
    }}
    
