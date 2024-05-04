using System;
using System.Text;
using System.Linq;
using DevExpress.ExpressApp;
using System.ComponentModel;
using DevExpress.ExpressApp.DC;
using System.Collections.Generic;
using DevExpress.Persistent.Base;
using DevExpress.ExpressApp.Model;
using DevExpress.ExpressApp.Actions;
using DevExpress.ExpressApp.Editors;
using DevExpress.ExpressApp.Updating;
using DevExpress.ExpressApp.Model.Core;
using DevExpress.ExpressApp.Model.DomainLogics;
using DevExpress.ExpressApp.Model.NodeGenerators;
namespace Aria5.DevExpress.OneTouchAway.Module
{
    // For more typical usage scenarios, be sure to check out http://documentation.devexpress.com/#Xaf/clsDevExpressExpressAppModuleBasetopic.
    public sealed partial class ModuleModule : ModuleBase
    {
        public ModuleModule()
        {
             //AdditionalExportedTypes.Add(typeof(Aria5.DevExpress.OneTouchAway.Module.BusinessObjects.ClientEntity));
            // AdditionalExportedTypes.Add(typeof(Aria5.DevExpress.OneTouchAway.Module.BusinessObjects.Log));
            
            ModelNodesGeneratorSettings.SetIdPrefix(typeof(BusinessObjects.News), "ClientNews");
             ModelNodesGeneratorSettings.SetIdPrefix(typeof(BusinessObjects.Event),"ClientEvents");
             ModelNodesGeneratorSettings.SetIdPrefix(typeof(BusinessObjects.Food),"ClientFood");
             ModelNodesGeneratorSettings.SetIdPrefix(typeof(BusinessObjects.ADS),"ClienADS");
             ModelNodesGeneratorSettings.SetIdPrefix(typeof(BusinessObjects.Business),"ClientBusiness");
             ModelNodesGeneratorSettings.SetIdPrefix(typeof(BusinessObjects.Person),"ClientPerson");
             ModelNodesGeneratorSettings.SetIdPrefix(typeof(BusinessObjects.Contact),"ClientContact");
             ModelNodesGeneratorSettings.SetIdPrefix(typeof(BusinessObjects.HubPage),"ClientHubPage");
             ModelNodesGeneratorSettings.SetIdPrefix(typeof(BusinessObjects.Entry),"ClientEntry");
             ModelNodesGeneratorSettings.SetIdPrefix(typeof(BusinessObjects.Area),"ClientArea");
             ModelNodesGeneratorSettings.SetIdPrefix(typeof(BusinessObjects.OnlineGame),"ClientOnlineGame");
             ModelNodesGeneratorSettings.SetIdPrefix(typeof(BusinessObjects.ClientLocationType),"ClientLocationType");
             ModelNodesGeneratorSettings.SetIdPrefix(typeof(BusinessObjects.Information),"ClientInformation");
             ModelNodesGeneratorSettings.SetIdPrefix(typeof(BusinessObjects.GuideType),"ClientGuideType");
             ModelNodesGeneratorSettings.SetIdPrefix(typeof(BusinessObjects.DataFilter), "ClientDataFilter");
             ModelNodesGeneratorSettings.SetIdPrefix(typeof(BusinessObjects.DataFilterColumn), "DataFilterColumnFilter");
             ModelNodesGeneratorSettings.SetIdPrefix(typeof(BusinessObjects.DataSort), "DataSortFilter");
             ModelNodesGeneratorSettings.SetIdPrefix(typeof(BusinessObjects.DataSortColumn), "DataSortColumnFilter");
             ModelNodesGeneratorSettings.SetIdPrefix(typeof(BusinessObjects.Profile), "ClientProfile");
           
            
            InitializeComponent();
        }
        public override IEnumerable<ModuleUpdater> GetModuleUpdaters(IObjectSpace objectSpace, Version versionFromDB)
        {
            ModuleUpdater updater = new DatabaseUpdate.Updater(objectSpace, versionFromDB);
            return new ModuleUpdater[] { updater };
        }
        public override void Setup(XafApplication application)
        {
            base.Setup(application);
            // Manage various aspects of the application UI and behavior at the module level.
        }
        }
    }

