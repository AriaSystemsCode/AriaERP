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
	public class RecreditRequestResponse
	{
		private int statusField;

		private string errorMessageField;

		private string requesterIDField;

		private string requestIDField;

		private CertifiedIntermediaryStatus certifiedIntermediaryField;

		public CertifiedIntermediaryStatus CertifiedIntermediary
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

		public string ErrorMessage
		{
			get
			{
				return this.errorMessageField;
			}
			set
			{
				this.errorMessageField = value;
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

		public int Status
		{
			get
			{
				return this.statusField;
			}
			set
			{
				this.statusField = value;
			}
		}

		public RecreditRequestResponse()
		{
		}
	}
}