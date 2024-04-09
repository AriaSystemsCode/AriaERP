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
	public class Notification
	{
		private NotificationSeverityType severityField;

		private string sourceField;

		private string codeField;

		private string messageField;

		private string localizedMessageField;

		private NotificationParameter[] messageParametersField;

		public string Code
		{
			get
			{
				return this.codeField;
			}
			set
			{
				this.codeField = value;
			}
		}

		public string LocalizedMessage
		{
			get
			{
				return this.localizedMessageField;
			}
			set
			{
				this.localizedMessageField = value;
			}
		}

		public string Message
		{
			get
			{
				return this.messageField;
			}
			set
			{
				this.messageField = value;
			}
		}

		[XmlElement("MessageParameters")]
		public NotificationParameter[] MessageParameters
		{
			get
			{
				return this.messageParametersField;
			}
			set
			{
				this.messageParametersField = value;
			}
		}

		public NotificationSeverityType Severity
		{
			get
			{
				return this.severityField;
			}
			set
			{
				this.severityField = value;
			}
		}

		public string Source
		{
			get
			{
				return this.sourceField;
			}
			set
			{
				this.sourceField = value;
			}
		}

		public Notification()
		{
		}
	}
}