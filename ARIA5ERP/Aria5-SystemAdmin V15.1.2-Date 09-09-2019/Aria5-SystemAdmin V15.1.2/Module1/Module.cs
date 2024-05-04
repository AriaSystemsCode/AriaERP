using System;
using System.Collections.Generic;

using DevExpress.ExpressApp;
using DevExpress.ExpressApp.Updating;

namespace Module1
{
    public sealed partial class Module1Module : ModuleBase
    {
        public Module1Module()
        {
            InitializeComponent();
        }
        public override IEnumerable<ModuleUpdater> GetModuleUpdaters(IObjectSpace objectSpace, Version versionFromDB)
        {
            ModuleUpdater updater = new DatabaseUpdate.Updater(objectSpace, versionFromDB);
            return new ModuleUpdater[] { updater };
        }
    }
}
