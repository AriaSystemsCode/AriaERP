using System;
using System.Collections.Generic;
using Aria.DataTypes.ObjectDictionary;
using Aria.DataTypes;

namespace Aria.ParameterSubstitution
{
    public class AriaParameterizedText : AriaParameterizedItem
    {

        public AriaParameterizedText(AriaArgumentList arguments)
            : base(arguments)
        {
        }

        public AriaParameterizedText(AriaArgument argument)
            : base(argument)
        {
        }

        public string Substitute(string item)
        {
            string LeftRectangularBracket = Guid.NewGuid().ToString();
            
            item.Replace("<<", LeftRectangularBracket);

            string RightRectangularBracket = Guid.NewGuid().ToString();
            
            item.Replace(">>", RightRectangularBracket);
            
            string[] tempTags = item.Split('<', '>');

            string[] tags = new string[tempTags.Length / 2];

            for (int index = 1; index < tempTags.Length; index += 2)
            {
                tags[index / 2] = tempTags[index];
            }

            for (int tagIndex = 0; tagIndex < tags.Length; tagIndex++)
            {
                item = item.Replace("<" + tags[tagIndex] + ">", GetValue(tags[tagIndex]).ToString());
            }

            item.Replace(LeftRectangularBracket, "<");
            
            item.Replace(RightRectangularBracket, ">");

            return item;
        }

        public Dictionary<string, string> Substitute(Dictionary<string, string> items)
        {
            List<KeyValuePair<string, string>> keyList = new List<KeyValuePair<string, string>>();
            
            foreach (KeyValuePair<string, string> key in items)
            {
                keyList.Add(key);
            }

            for (int index = 0; index < keyList.Count; index++)
            {
                items[keyList[index].Key] = Substitute(items[keyList[index].Key]);
            }

            return items;
        }
    }
}
