using System;
using System.Collections;
using System.Collections.Generic;
using System.ComponentModel;
using System.Configuration.Install;


namespace Aria.Services.ShutdownHangedRequestService
{
    [RunInstaller(true)]
    public partial class ShutdownHangedRequestServiceInstaller : Installer
    {
        public ShutdownHangedRequestServiceInstaller()
        {
            InitializeComponent();
        }
    }
}
