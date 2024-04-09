using System;
using System.Collections.Generic;
using System.Text;
using AriaObjectBrowser.Forms;
using System.Windows.Forms;
using AriaObjectBrowser.DataTypes;

namespace AriaObjectBrowser
{
    public class Loader
    {
        public AriaObjectBrowserMain LoadForm(object ariaApplication, object ariaFrom, object interOperability, string connectionstring, string objectType, string objectKey)
        {
            AriaObjectBrowserMain form = new AriaObjectBrowserMain();
            form.InterOperability = interOperability;

            form.AriaApplication = ariaApplication;
            form.AriaFrom = ariaFrom;
            form.ObjectType = objectType;
            form.ObjectKey = objectKey;
            form.Connectionstring = connectionstring;
            //Moh 21-7-2009 Add property to hold objects save path start
            form.ObjectsPath = form.getObjectsPath();
            //Moh 21-7-2009 Add property to hold objects save path End
            form.LoadObjects();

            switch (objectType)
            {
                case "A":
                    form.Text = "Aria Object Browser - Customer: " + objectKey;
                    break;

                case "I":
                    form.Text = "Aria Object Browser - Item: " + objectKey;
                    break;

                case "G":
                    form.Text = "Aria Object Browser - Art Work: " + objectKey;
                    break;

                case "D":
                    form.Text = "Aria Object Browser - Color Rotation: " + objectKey;
                    break;

                case "S":
                    form.Text = "Aria Object Browser - Style: " + objectKey;
                    break;
            }
            form.Show();
            
            return form;
        }
    }
}
