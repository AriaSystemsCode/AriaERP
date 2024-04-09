using System;
using System.Collections;
using Aria.DataTypes.ObjectDictionary;
using Aria.Data;

namespace Aria.EnterpriseServices.ObjectDictionary
{
    public partial class AriaObjectDictionaryDBCentric
    {
        public void MergeModification(ArrayList sortedobject, string objectIdProperty, string objectModificationProperty)
        {
            for (int index = sortedobject.Count - 1; index > 0; index--)
            {
                object dataObject = sortedobject[index];

                ArrayList ListParsedObjects = new ArrayList();

                if (ListParsedObjects.Contains(dataObject.GetType().GetProperty(objectIdProperty).GetValue(dataObject, null)))
                {
                    sortedobject.RemoveAt(index);
                }
                else
                {
                    ListParsedObjects.Add(ListParsedObjects.Contains(dataObject.GetType().GetProperty(objectIdProperty).GetValue(dataObject, null)));

                    if ((AriaModificationTypes)(dataObject.GetType().GetProperty(objectModificationProperty).GetValue(dataObject, null)) == 
                        AriaModificationTypes.Delete)
                    {
                        sortedobject.RemoveAt(index);
                    }
                }
            }
        }
    }
}
