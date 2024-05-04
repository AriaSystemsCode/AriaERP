using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Aria5.DevExpress.MainSystem.Module.BusinessObjects;
using DevExpress.Xpo;
using DevExpress.Data.Filtering;
using DevExpress.Xpo.DB;
using DevExpress.ExpressApp;
namespace Aria5.DevExpress.MainSystem.Module.Managers
{
    public static class IdentifierStructureManager
    {

        public static List<string> GetAllIdentifierStructures(IObjectSpace _session)
        {
            List<string> returnList = new List<string>();

            XPCollection collection = (XPCollection)_session.CreateCollection(typeof(IdentifierStructure));
            collection.Load();
            if (collection.GetType().Equals(typeof(XPCollection)) & collection.Count > 0)
            {
                for (int c = 0; c < collection.Count; c++)
                {
                    returnList.Add(((IdentifierStructure)collection[c]).IdentifierStructureId);
                }
            }
            return returnList;
        }

        public static List<string> GetAllIdentifierStructures()
        {
            List<string> returnList = new List<string>();

            XPCollection collection = new XPCollection(XpoDefault.Session, typeof(IdentifierStructure));
            collection.Load();
            if (collection.GetType().Equals(typeof(XPCollection)) & collection.Count > 0)
            {
                for (int c = 0; c < collection.Count; c++)
                {
                    returnList.Add(((IdentifierStructure)collection[c]).IdentifierStructureId);
                }
            }
            return returnList;
        }

        static void IdentifierStructure_SchemaInit(object sender, SchemaInitEventArgs e)
        {

        }



        public static string GetNextId(String identifierStructure, Dictionary<int, object> seg, IObjectSpace _session)
        {

            if (identifierStructure == "" | identifierStructure == null)
            {
                return "";
            }

            string returnValue = "";

            XPCollection collection = (XPCollection)_session.CreateCollection(typeof(IdentifierStructure), CriteriaOperator.Parse("IdentifierStructureId = '" + identifierStructure + "'"));
            collection.Load();
            // Safecode
            if (collection.Count == 0)
            {
                return "";
            }

            IdentifierStructure identifier = collection[0] as IdentifierStructure;

            SortingCollection sortCollection = new SortingCollection();
            sortCollection.Add(new SortProperty("Position", SortingDirection.Ascending));
            identifier.Segments.Sorting = sortCollection;
            int segmentsCounter = 0;

            foreach (IdentifierSegment segment in identifier.Segments)
            {
                if (segmentsCounter >= identifier.NoOfSegments)
                {
                    break;
                }
                segmentsCounter += 1;

                if (seg.ContainsKey(segment.Position))
                {
                    returnValue += seg[segment.Position];
                }
                else
                {
                    switch (segment.ValueType)
                    {
                        case ValueTypes.Identity:
                            if (segment.IdentifierSegmentFormat.Count != 0)
                            {
                                returnValue += segment.IdentifierSegmentFormat[0].NextIdentity.ToString();
                                if (segment.SeparatorAfter != null)
                                {
                                    returnValue += segment.SeparatorAfter.ToString();
                                }
                                segment.IdentifierSegmentFormat[0].NextIdentity += 1;
                                segment.IdentifierSegmentFormat[0].Save();
                            }
                            else
                            {
                                returnValue += segment.NextIdentity.ToString();
                                if (segment.SeparatorAfter != null)
                                {
                                    returnValue += segment.SeparatorAfter.ToString();
                                }
                                segment.NextIdentity += 1;
                                segment.Save();
                            }
                            break;

                        case ValueTypes.FixedValue:
                            // To Do 
                            //Get the default fixed value from IdentifierSegmentValidEntry table.
                            break;
                        case ValueTypes.ManuallyEntered:
                            // To Do 
                            break;
                        case ValueTypes.EntityValue:
                            // To Do 
                            break;
                        case ValueTypes.Expression:
                            // To Do 
                            break;
                        case ValueTypes.ValidEntries:
                            // To Do 
                            break;
                        default:
                            break;
                    }
                }
            }
            return returnValue.TrimEnd();

        }

        public static string GetNextId(String identifierStructure, Dictionary<int, object> seg)
        {

            if (identifierStructure == "" | identifierStructure == null)
            {
                return "";
            }

            string returnValue = "";

            XPCollection collection = new XPCollection(XpoDefault.Session, typeof(IdentifierStructure), CriteriaOperator.Parse("IdentifierStructureId = '" + identifierStructure + "'"));
            collection.Load();
            // Safecode
            if (collection.Count == 0)
            {
                return "";
            }

            IdentifierStructure identifier = collection[0] as IdentifierStructure;

            SortingCollection sortCollection = new SortingCollection();
            sortCollection.Add(new SortProperty("Position", SortingDirection.Ascending));
            identifier.Segments.Sorting = sortCollection;
            int segmentsCounter = 0;

            foreach (IdentifierSegment segment in identifier.Segments)
            {
                if (segmentsCounter >= identifier.NoOfSegments)
                {
                    break;
                }
                segmentsCounter += 1;

                if (seg.ContainsKey(segment.Position))
                {
                    returnValue += seg[segment.Position];
                }
                else
                {
                    switch (segment.ValueType)
                    {
                        case ValueTypes.Identity:
                            if (segment.IdentifierSegmentFormat.Count != 0)
                            {
                                returnValue += segment.IdentifierSegmentFormat[0].NextIdentity.ToString();
                                if (segment.SeparatorAfter != null)
                                {
                                    returnValue += segment.SeparatorAfter.ToString();
                                }
                                segment.IdentifierSegmentFormat[0].NextIdentity += 1;
                                segment.IdentifierSegmentFormat[0].Save();
                            }
                            else
                            {
                                returnValue += segment.NextIdentity.ToString();
                                if (segment.SeparatorAfter != null)
                                {
                                    returnValue += segment.SeparatorAfter.ToString();
                                }
                                segment.NextIdentity += 1;
                                segment.Save();
                            }
                            break;

                        case ValueTypes.FixedValue:
                            // To Do 
                            //Get the default fixed value from IdentifierSegmentValidEntry table.
                            break;
                        case ValueTypes.ManuallyEntered:
                            // To Do 
                            break;
                        case ValueTypes.EntityValue:
                            // To Do 
                            break;
                        case ValueTypes.Expression:
                            // To Do 
                            break;
                        case ValueTypes.ValidEntries:
                            // To Do 
                            break;
                        default:
                            break;
                    }
                }
            }
            return returnValue.TrimEnd();

        }


        //ATA add new function that used not default session to get next id 11/28/2016[start]
        public static string GetNextId(String identifierStructure, Dictionary<int, object> seg,Session session)
        {

            if (identifierStructure == "" | identifierStructure == null)
            {
                return "";
            }

            string returnValue = "";

            XPCollection collection = new XPCollection(session, typeof(IdentifierStructure), CriteriaOperator.Parse("IdentifierStructureId = '" + identifierStructure + "'"));
            collection.Load();
            // Safecode
            if (collection.Count == 0)
            {
                return "";
            }

            IdentifierStructure identifier = collection[0] as IdentifierStructure;

            SortingCollection sortCollection = new SortingCollection();
            sortCollection.Add(new SortProperty("Position", SortingDirection.Ascending));
            identifier.Segments.Sorting = sortCollection;
            int segmentsCounter = 0;

            foreach (IdentifierSegment segment in identifier.Segments)
            {
                if (segmentsCounter >= identifier.NoOfSegments)
                {
                    break;
                }
                segmentsCounter += 1;

                if (seg.ContainsKey(segment.Position))
                {
                    returnValue += seg[segment.Position];
                }
                else
                {
                    switch (segment.ValueType)
                    {
                        case ValueTypes.Identity:
                            if (segment.IdentifierSegmentFormat.Count != 0)
                            {
                                returnValue += segment.IdentifierSegmentFormat[0].NextIdentity.ToString();
                                if (segment.SeparatorAfter != null)
                                {
                                    returnValue += segment.SeparatorAfter.ToString();
                                }
                                segment.IdentifierSegmentFormat[0].NextIdentity += 1;
                                segment.IdentifierSegmentFormat[0].Save();
                            }
                            else
                            {
                                if (segment.Format != null)
                                {
                                    returnValue += segment.NextIdentity.ToString(segment.Format);
                                }
                                else
                                {
                                    returnValue += segment.NextIdentity.ToString();
                                }
                             
                                if (segment.SeparatorAfter != null)
                                {
                                    returnValue += segment.SeparatorAfter.ToString();
                                }
                                segment.NextIdentity += 1;
                                segment.Save();
                            }
                            break;

                        case ValueTypes.FixedValue:
                            // To Do 
                            //Get the default fixed value from IdentifierSegmentValidEntry table.
                            returnValue += segment.InputMask;
                            if (segment.SeparatorAfter != null)
                            {
                                returnValue += segment.SeparatorAfter.ToString();
                            }
                            break;
                        case ValueTypes.ManuallyEntered:
                            // To Do 
                            break;
                        case ValueTypes.EntityValue:
                            // To Do 
                            break;
                        case ValueTypes.Expression:
                            // To Do 
                            break;
                        case ValueTypes.ValidEntries:
                            // To Do 
                            break;
                        case ValueTypes.CurrentYearValue:
                            returnValue += DateTime.Now.ToString("yy") +"-";
                            if (segment.SeparatorAfter != null)
                            {
                                returnValue += segment.SeparatorAfter.ToString();
                            }
                            break;
                        default:
                            break;
                    }
                }
            }
            return returnValue.TrimEnd();

        }
        //ATA add new function that used not default session to get next id 11/28/2016[End]


    }
}
