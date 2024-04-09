using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Configuration.Install;

namespace Aria.Services.RequestHandler
{
    [RunInstaller(true)]
    public partial class AriaRequestAgentServiceInstaller : Installer
    {
        public AriaRequestAgentServiceInstaller()
        {
            InitializeComponent();
        }
    }
}