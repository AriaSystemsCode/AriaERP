using System;
using System.CodeDom.Compiler;
using System.Xml.Serialization;

namespace Fedex.FedexTrack
{
	[GeneratedCode("System.Xml", "4.0.30319.1")]
	[Serializable]
	[XmlType(Namespace="http://fedex.com/ws/track/v4")]
	public enum ArrivalLocationType
	{
		AIRPORT,
		CUSTOMER,
		CUSTOMS_BROKER,
		DELIVERY_LOCATION,
		DESTINATION_AIRPORT,
		DESTINATION_FEDEX_FACILITY,
		DROP_BOX,
		ENROUTE,
		FEDEX_FACILITY,
		FEDEX_OFFICE_LOCATION,
		INTERLINE_CARRIER,
		NON_FEDEX_FACILITY,
		ORIGIN_AIRPORT,
		ORIGIN_FEDEX_FACILITY,
		PICKUP_LOCATION,
		PLANE,
		PORT_OF_ENTRY,
		SORT_FACILITY,
		TURNPOINT,
		VEHICLE
	}
}