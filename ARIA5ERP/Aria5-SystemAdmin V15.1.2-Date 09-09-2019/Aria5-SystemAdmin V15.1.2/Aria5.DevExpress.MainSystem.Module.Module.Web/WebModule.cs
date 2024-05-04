using System;
using System.ComponentModel;
using System.Collections.Generic;

using DevExpress.ExpressApp;
using DevExpress.ExpressApp.Updating;

namespace Aria5SystemAdmin.Module.Web
{
    [ToolboxItemFilter("Xaf.Platform.Web")]
    public sealed partial class Aria5SystemAdminAspNetModule : ModuleBase
    {
        public Aria5SystemAdminAspNetModule()
        {
            InitializeComponent();
        }
        public override IEnumerable<ModuleUpdater> GetModuleUpdaters(IObjectSpace objectSpace, Version versionFromDB)
        {
            return ModuleUpdater.EmptyModuleUpdaters;
        }
    }
}
