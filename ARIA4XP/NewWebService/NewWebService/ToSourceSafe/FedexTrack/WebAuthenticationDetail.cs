using System;
using System.CodeDom.Compiler;
using System.ComponentModel;
using System.Diagnostics;
using System.Xml.Serialization;

namespace Fedex.FedexTrack
{
	[DebuggerStepThrough]
	[DesignerCategory("code")]
	[GeneratedCode("System.Xml", "4.0.30319.1")]
	[Serializable]
	[XmlType(Namespace="http://fedex.com/ws/track/v4")]
	public class WebAuthenticationDetail
	{
		private WebAuthenticationCredential userCredentialField;

		public WebAuthenticationCredential UserCredential
		{
			get
			{
				return this.userCredentialField;
			}
			set
			{
				this.userCredentialField = value;
			}
		}

		public WebAuthenticationDetail()
		{
		}
	}
}