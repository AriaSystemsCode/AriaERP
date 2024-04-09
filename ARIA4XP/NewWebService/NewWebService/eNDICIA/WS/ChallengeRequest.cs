using System;
using System.CodeDom.Compiler;
using System.ComponentModel;
using System.Diagnostics;
using System.Xml.Serialization;

namespace Endicia.WS
{
    [DebuggerStepThrough]
    [DesignerCategory("code")]
    [GeneratedCode("System.Xml", "4.0.30319.1")]
    [Serializable]
    [XmlType(Namespace = "www.envmgr.com/LabelService")]
    public class ChallengeRequest : DataValidator
    {
        private string requesterIDField;

        private string requestIDField;

       // private CertifiedIntermediary certifiedIntermediaryField;

        //private string newPassPhraseField;
        private string accountIDField;
        private string emailField;

        //public CertifiedIntermediary CertifiedIntermediary
        //{
        //    get
        //    {
        //        return this.certifiedIntermediaryField;
        //    }
        //    set
        //    {
        //        this.certifiedIntermediaryField = value;
        //    }
        //}

        //public string NewPassPhrase
        //{
        //    get
        //    {
        //        return this.newPassPhraseField;
        //    }
        //    set
        //    {
        //        this.newPassPhraseField = value;
        //    }
        //}

        public string RequesterID
        {
            get
            {
                return this.requesterIDField;
            }
            set
            {
                this.requesterIDField = value;
            }
        }

        public string RequestID
        {
            get
            {
                return this.requestIDField;
            }
            set
            {
                this.requestIDField = value;
            }
        }
        public string AccountID
        {
            get
            {
                return this.accountIDField;
            }
            set
            {
                this.accountIDField = value;
            }
        }
        public string Email
        {
            get
            {
                return this.emailField ;
            }
            set
            {
                this.emailField = value;
            }
        }

        public ChallengeRequest()
        {
        }
    }
}
