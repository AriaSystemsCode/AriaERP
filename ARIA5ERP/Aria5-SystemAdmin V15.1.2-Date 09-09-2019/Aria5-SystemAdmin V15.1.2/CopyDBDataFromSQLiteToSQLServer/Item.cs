
using CopyDBDataFromSQLiteToSQLServer;
using System.Collections.Generic;
using System.Collections.ObjectModel;
namespace TopoSortDemo
{
    public class Item
    {
        public string Name { get; private set; }
        public Item[] Dependencies { get; set; }

        public Item(string name, params Item[] dependencies)
        {
            Name = name;
            Dependencies = dependencies;
        }

        public List<Aria5.DevExpress.SystemAdmin.WebServiceRole.Marshalling.SyncDataObject> syncObjList { get; set; }


        public override string ToString()
        {
            return Name;
        }
    }
}
