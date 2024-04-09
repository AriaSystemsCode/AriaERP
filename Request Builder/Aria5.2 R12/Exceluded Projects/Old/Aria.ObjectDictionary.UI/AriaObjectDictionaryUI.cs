using System;
using System.Collections.Generic;
using System.Text;
using System.EnterpriseServices;

namespace Aria.ObjectDictionary.UI
{
    public class AriaObjectDictionaryUI
    {
        public void showObjectDictionaryUI()
        {
            ObjectDictionaryViewer viewer = new ObjectDictionaryViewer();
            viewer.Show();
        }
    }
}
