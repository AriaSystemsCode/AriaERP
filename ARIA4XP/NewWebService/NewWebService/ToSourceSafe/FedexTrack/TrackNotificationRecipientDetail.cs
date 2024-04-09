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
	public class TrackNotificationRecipientDetail
	{
		private bool notifyOnDeliveryField;

		private bool notifyOnDeliveryFieldSpecified;

		private bool notifyOnExceptionField;

		private bool notifyOnExceptionFieldSpecified;

		public bool NotifyOnDelivery
		{
			get
			{
				return this.notifyOnDeliveryField;
			}
			set
			{
				this.notifyOnDeliveryField = value;
			}
		}

		[XmlIgnore]
		public bool NotifyOnDeliverySpecified
		{
			get
			{
				return this.notifyOnDeliveryFieldSpecified;
			}
			set
			{
				this.notifyOnDeliveryFieldSpecified = value;
			}
		}

		public bool NotifyOnException
		{
			get
			{
				return this.notifyOnExceptionField;
			}
			set
			{
				this.notifyOnExceptionField = value;
			}
		}

		[XmlIgnore]
		public bool NotifyOnExceptionSpecified
		{
			get
			{
				return this.notifyOnExceptionFieldSpecified;
			}
			set
			{
				this.notifyOnExceptionFieldSpecified = value;
			}
		}

		public TrackNotificationRecipientDetail()
		{
		}
	}
}