using System;
using Aria.Xml;
using System.Collections;
using System.Text.RegularExpressions;
using System.IO;
using System.ComponentModel;

namespace Aria.DataTypes.Messaging
{
    /// <summary>
    /// a- create properties can used in send email.    c- Validated message before send.
    /// </summary>
    [Serializable, AriaSerializableAttribute, TypeConverter(typeof(ExpandableObjectConverter))]
    public class AriaEmail
    {
        private string _senderName = "";
        public string SenderName
        {
            get { return _senderName; }
            set { _senderName = value; }
        }

        private string _senderMail = "";
        public string SenderMail
        {
            get { return _senderMail; }
            set { _senderMail = value; }
        }

        private string _to = "";
        public string To
        {
            get { return _to; }
            set { _to = value; }
        }

        private string _cc = "";
        public string Cc
        {
            get { return _cc; }
            set { _cc = value; }
        }

        private string _bcc = "";
        public string Bcc
        {
            get { return _bcc; }
            set { _bcc = value; }
        }

        private string _subject = "";
        public string Subject
        {
            get { return _subject; }
            set { _subject = value; }
        }

        private string _body = "";
        public string Body
        {
            get { return _body; }
            set { _body = value; }
        }

        private Hashtable _attachment = new Hashtable();
        public Hashtable Attachment
        {
            get { return _attachment; }
            set { _attachment = value; }
        }

        public string ConvertAttachmentToString()
        {
            string attachment = "";
            bool flag = false;

            foreach (DictionaryEntry entry in _attachment)
            {
                if (flag)
                    attachment += ", ";

                attachment += entry.Key.ToString();
                flag = true;
            }

            return attachment;
        }

        public void ConvertAttachmentFromString(string attachment)
        {
            string[] array = attachment.Split(',');

            _attachment.Clear();

            for (int index = 0; index < array.Length; index++)
            {
                if (!array[index].Trim().Equals(""))
                {
                    _attachment.Add(array[index], array[index]);
                }
            }
        }

        public bool IsValid()
        {
            string pattern = @"^([\w-\.]+)@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.)|(([\w-]+\.)+))([a-zA-Z]{2,4}|[0-9]{1,3})(\]?)$";

            if (To.Trim() == "")
                return false;

            if (To != null && To.Trim() != "")
            {
                To = To.Trim();
                string[] emailTo = To.Split(',');
                for (int index = 0; index < emailTo.Length; index++)
                {
                    Match match = Regex.Match(emailTo[index].Trim(), pattern, RegexOptions.IgnoreCase);

                    if (match.Success)
                        continue;

                    if (emailTo[index].Trim().StartsWith("<") && emailTo[index].Trim().EndsWith(">"))
                        continue;


                    return false;
                }
            }

            if (Cc != null && Cc.Trim() != "")
            {
                Cc = Cc.Trim();
                string[] emailCc = Cc.Split(',');
                for (int index = 0; index < emailCc.Length; index++)
                {
                    Match match = Regex.Match(emailCc[index].Trim(), pattern, RegexOptions.IgnoreCase);

                    if (match.Success)
                        continue;

                    if (emailCc[index].Trim().StartsWith("<") && emailCc[index].Trim().EndsWith(">"))
                        continue;


                    return false;
                }
            }

            if (Bcc != null && Bcc.Trim() != "")
            {
                Bcc = Bcc.Trim();
                string[] emailBcc = Bcc.Split(',');
                for (int index = 0; index < emailBcc.Length; index++)
                {
                    Match match = Regex.Match(emailBcc[index].Trim(), pattern, RegexOptions.IgnoreCase);

                    if (match.Success)
                        continue;

                    if (emailBcc[index].Trim().StartsWith("<") && emailBcc[index].Trim().EndsWith(">"))
                        continue;


                    return false;
                }
            }

            if (Attachment != null)
            {
                foreach (DictionaryEntry entry in _attachment)
                {
                    FileInfo file = new FileInfo(entry.Key.ToString());

                    if (file.Exists)
                        continue;

                    return false;
                }
            }

            return true;
        }
    }
}
