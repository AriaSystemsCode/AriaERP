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
	[XmlType(Namespace="www.envmgr.com/LabelService")]
	public class AccountStatusRequest : DataValidator
	{
		private string requesterIDField;

		private string requestIDField;

		private CertifiedIntermediary certifiedIntermediaryField;

		public CertifiedIntermediary CertifiedIntermediary
		{
			get
			{
				return this.certifiedIntermediaryField;
			}
			set
			{
				this.certifiedIntermediaryField = value;
			}
		}

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

		public AccountStatusRequest()
		{
		}
	}
}