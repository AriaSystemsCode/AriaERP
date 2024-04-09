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
	public class NotificationParameter
	{
		private string idField;

		private string valueField;

		public string Id
		{
			get
			{
				return this.idField;
			}
			set
			{
				this.idField = value;
			}
		}

		public string Value
		{
			get
			{
				return this.valueField;
			}
			set
			{
				this.valueField = value;
			}
		}

		public NotificationParameter()
		{
		}
	}
}