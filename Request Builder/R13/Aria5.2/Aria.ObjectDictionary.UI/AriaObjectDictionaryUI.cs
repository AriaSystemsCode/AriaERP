using System;
using System.Collections.Generic;
using System.Text;
using System.EnterpriseServices;

namespace Aria.ObjectDictionary.UI
{
    public class AriaObjectDictionaryUI
    {

        //SAB 12-19-2013 Fix Dictionary Viewer Problem [Start]
        public string ServerName { get; set; }
        public string ServerPort { get; set; }
        //SAB 12-19-2013 Fix Dictionary Viewer Problem [End]

        public void showObjectDictionaryUI()
        {
            //SAB 12-19-2013 Fix Dictionary Viewer Problem [Start]
            //ObjectDictionaryViewer viewer = new ObjectDictionaryViewer();
            ObjectDictionaryViewer viewer = new ObjectDictionaryViewer(ServerName, ServerPort);
            //SAB 12-19-2013 Fix Dictionary Viewer Problem [End]
            viewer.Show();
        }
    }
}
