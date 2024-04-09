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
	public class EMailNotificationDetail
	{
		private string personalMessageField;

		private EMailNotificationRecipient[] recipientsField;

		public string PersonalMessage
		{
			get
			{
				return this.personalMessageField;
			}
			set
			{
				this.personalMessageField = value;
			}
		}

		[XmlElement("Recipients")]
		public EMailNotificationRecipient[] Recipients
		{
			get
			{
				return this.recipientsField;
			}
			set
			{
				this.recipientsField = value;
			}
		}

		public EMailNotificationDetail()
		{
		}
	}
}