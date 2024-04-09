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
	public class WebAuthenticationCredential
	{
		private string keyField;

		private string passwordField;

		public string Key
		{
			get
			{
				return this.keyField;
			}
			set
			{
				this.keyField = value;
			}
		}

		public string Password
		{
			get
			{
				return this.passwordField;
			}
			set
			{
				this.passwordField = value;
			}
		}

		public WebAuthenticationCredential()
		{
		}
	}
}