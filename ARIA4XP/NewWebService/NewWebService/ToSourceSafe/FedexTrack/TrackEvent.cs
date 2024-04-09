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
	public class TrackEvent
	{
		private DateTime timestampField;

		private bool timestampFieldSpecified;

		private string eventTypeField;

		private string eventDescriptionField;

		private string statusExceptionCodeField;

		private string statusExceptionDescriptionField;

		private  Address addressField;

		private ArrivalLocationType arrivalLocationField;

		private bool arrivalLocationFieldSpecified;

		public  Address Address
		{
			get
			{
				return this.addressField;
			}
			set
			{
				this.addressField = value;
			}
		}

		public ArrivalLocationType ArrivalLocation
		{
			get
			{
				return this.arrivalLocationField;
			}
			set
			{
				this.arrivalLocationField = value;
			}
		}

		[XmlIgnore]
		public bool ArrivalLocationSpecified
		{
			get
			{
				return this.arrivalLocationFieldSpecified;
			}
			set
			{
				this.arrivalLocationFieldSpecified = value;
			}
		}

		public string EventDescription
		{
			get
			{
				return this.eventDescriptionField;
			}
			set
			{
				this.eventDescriptionField = value;
			}
		}

		public string EventType
		{
			get
			{
				return this.eventTypeField;
			}
			set
			{
				this.eventTypeField = value;
			}
		}

		public string StatusExceptionCode
		{
			get
			{
				return this.statusExceptionCodeField;
			}
			set
			{
				this.statusExceptionCodeField = value;
			}
		}

		public string StatusExceptionDescription
		{
			get
			{
				return this.statusExceptionDescriptionField;
			}
			set
			{
				this.statusExceptionDescriptionField = value;
			}
		}

		public DateTime Timestamp
		{
			get
			{
				return this.timestampField;
			}
			set
			{
				this.timestampField = value;
			}
		}

		[XmlIgnore]
		public bool TimestampSpecified
		{
			get
			{
				return this.timestampFieldSpecified;
			}
			set
			{
				this.timestampFieldSpecified = value;
			}
		}

		public TrackEvent()
		{
		}
	}
}