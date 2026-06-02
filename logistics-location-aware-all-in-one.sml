(* TIME unit is seconds*)
val minute = 60.0;
val hour = 60.0*minute;
val day = 24.0*hour;
val week = 7.0*day;

fun Mtime() = ModelTime.time():time; 
fun now() = toReal(Mtime());
(* fun Mtime() = valOf (Real.fromString (ModelTime.toString ModelTime.time():time))*)

(* 19472 is the number of days since 1970 *)
fun tuesday_april_25_2023() = 19472.0*day + 9.0*hour;
fun monday_may_22_2023() = 19499.0*day - 1.0*hour;
fun tuesday_may_30_2023() = 19507.0*day + 9.0*hour;

(* Start scheduling vehicles (~1 month in advance) *)
fun preprocess_start_time() = tuesday_april_25_2023();
(* Actual process *)
fun process_start_time() = monday_may_22_2023();
fun first_VH_time() = tuesday_may_30_2023();
fun start_time() = preprocess_start_time();

fun print_start_time() = Date.fmt "%Y-%m-%d %H:%M:%S" (Date.fromTimeLocal(Time.fromReal(start_time())));

(* TIME OUTPUT mySQL*)
fun t2s(t) = Date.fmt "%Y-%m-%d %H:%M:%S" (Date.fromTimeLocal(Time.fromReal(t+start_time())));

fun tinit2s() = Date.fmt "%Y-%m-%d %H:%M:%S" (Date.fromTimeLocal(Time.fromReal(0.0)));

(* TIME OUTPUT KEYVALUE*)
fun t2s_alt(t) = Date.fmt "%d-%m-%Y %H:%M:%S" (Date.fromTimeLocal(Time.fromReal(t+start_time())));


(* BETA DISTRIBUTION *)
fun ran_beta(low:real,high:real,a:real,b:real) = low + ((high-low)*beta(a,b)):real; 
fun mean_beta(low:real,high:real,a:real,b:real) = low + ((high-low)* (a/(a+b)));
fun mode_beta(low:real,high:real,a:real,b:real) = low + ((high-low)*((a-1.0)/(a+b-2.0)));
fun var_beta(low:real,high:real,a:real,b:real) = ((high-low)*(high-low)* ((a*b)/((a+b)*(a+b)*(a+b+1.0))));
fun stdev_beta(low:real,high:real,a:real,b:real) = Math.sqrt(var_beta(low,high,a,b));


(* TIME FUNCTIONS *)

fun t2date(t) = Date.fromTimeLocal(Time.fromReal(t+start_time()));


fun t2year(t) = Date.year(t2date(t)):int;
fun t2month(t) = Date.month(t2date(t)):Date.month;
fun t2day(t) = Date.day(t2date(t)):int;
fun t2hour(t) = Date.hour(t2date(t)):int;
fun t2minute(t) = Date.minute(t2date(t)):int;
fun t2second(t) = Date.second(t2date(t)):int;
fun t2weekday(t) = Date.weekDay(t2date(t)):Date.weekday;

fun t2monthstr(t) = Date.fmt "%b" (Date.fromTimeLocal(Time.fromReal(t+start_time())));
fun t2weekdaystr(t) = Date.fmt "%a" (Date.fromTimeLocal(Time.fromReal(t+start_time())));

fun remaining_time_hour(t) = hour - ((Real.fromInt(t2minute(t))*minute) + Real.fromInt(t2second(t)));


(* ARRIVAL TIME DISTRIBUTIONS *)

(* arrival time intensities vary from 0.0 to 1.0 and are the product of three factors: yearly influences, weekly influences, and daily influences *)

fun at_month_intensity(m:string) =
case m of 
 "Jan" => 1.0
|"Feb" => 1.0
|"Mar" => 1.0
|"Apr" => 1.0
|"May" => 1.0
|"Jun" => 1.0
|"Jul" => 1.0
|"Aug" => 1.0
|"Sep" => 1.0
|"Oct" => 1.0 
|"Nov" => 1.0
|"Dec" => 1.0
| _ => 1.0;

fun at_weekday_intensity(d:string) =
case d of 
 "Mon" => 1.0
|"Tue" => 1.0
|"Wed" => 1.0
|"Thu" => 1.0
|"Fri" => 0.7
|"Sat" => 0.0
|"Sun" => 0.0
| _ => 1.0;

fun at_hour_intensity(h:int) =
case h of 
 0 => 0.0
|1 => 0.0
|2 => 0.0
|3 => 0.0
|4 => 0.0
|5 => 0.0
|6 => 0.0
|7 => 0.0
|8 => 0.5
|9 => 1.0
|10 => 1.0
|11 => 1.0
|12 => 1.0
|13 => 1.0
|14 => 1.0
|15 => 1.0
|16 => 1.0
|17 => 1.0
|18 => 1.0
|19 => 0.5
|20 => 0.5
|21 => 0.0
|22 => 0.0
|23 => 0.0
| _ => 1.0;
 
(* overall intensity *)
fun at_intensity(t) = at_month_intensity(t2monthstr(t))*at_weekday_intensity(t2weekdaystr(t))*
at_hour_intensity(t2hour(t));

(* Use this function to sample interarrival times: t is the current time and d is the net delay: It moves forward based on intensities: the lower the intensity, the longer the delay in absolute time.*) 
fun rel_at_delay(t,d) = 
if d < 0.0001
   then 0.0
   else if d < remaining_time_hour(t)*at_intensity(t)
        then d/at_intensity(t)
        else rel_at_delay(t+remaining_time_hour(t),
            d-(remaining_time_hour(t)*at_intensity(t)))+hour; 

(* same but now without indicating current time explicitly *)
fun r_at_delay(d) = rel_at_delay(now(),d);

(* the average ratio between effective/net time (parameter d) and delay in actual time*)
val eff_at_factor = r_at_delay(52.0*week)/(52.0*week);

(* normalized interarrival time delay using the ratio above *)
fun norm_rel_at_delay(t,d) = rel_at_delay(t,d/eff_at_factor) ;


(* normalized  interarrival time delay using the ratio above *)
fun norm_r_at_delay(d) = r_at_delay(d/eff_at_factor) ;

(* SERVICE TIME DISTRIBUTIONS *)

(* service time intensities vary from 0.0 to 1.0 and are the product of three factors: yearly influences, weekly influences, and daily influences *)

fun st_month_intensity(m:string) =
case m of 
 "Jan" => 1.0
|"Feb" => 1.0
|"Mar" => 1.0
|"Apr" => 1.0
|"May" => 1.0
|"Jun" => 1.0
|"Jul" => 0.7
|"Aug" => 0.5
|"Sep" => 1.0
|"Oct" => 1.0 
|"Nov" => 1.0
|"Dec" => 0.8
| _ => 1.0;

fun st_weekday_intensity(d:string) =
case d of 
 "Mon" => 0.9
|"Tue" => 1.0
|"Wed" => 1.0
|"Thu" => 1.0
|"Fri" => 0.8
|"Sat" => 0.0
|"Sun" => 0.0
| _ => 1.0;

fun st_hour_intensity(h:int) =
case h of 
 0 => 0.0
|1 => 0.0
|2 => 0.0
|3 => 0.0
|4 => 0.0
|5 => 0.0
|6 => 0.0
|7 => 0.5
|8 => 0.5
|9 => 1.0
|10 => 1.0
|11 => 1.0
|12 => 0.0
|13 => 0.8
|14 => 1.0
|15 => 1.0
|16 => 0.8
|17 => 0.3
|18 => 0.1
|19 => 0.0
|20 => 0.0
|21 => 0.0
|22 => 0.0
|23 => 0.0
| _ => 1.0;
 

fun st_intensity(t) = st_month_intensity(t2monthstr(t))*
st_weekday_intensity(t2weekdaystr(t))*
st_hour_intensity(t2hour(t));


(* Use this function to sample service times: t is the current time and d is the net delay: It moves forward based on intensities: the lower the intensity, the longer the delay in absolute time.*)
fun rel_st_delay(t,d) = 
if d < 0.0001
   then 0.0
   else if d < remaining_time_hour(t)*st_intensity(t)
        then d/st_intensity(t)
        else rel_st_delay(t+remaining_time_hour(t),
            d-(remaining_time_hour(t)*st_intensity(t)))+hour;

(* same but now without indicating current time explicitly *)
fun r_st_delay(d) = rel_st_delay(now(),d);

(* the average ratio between effective/net time (parameter d) and delay in actual time*)
val eff_st_factor = r_st_delay(52.0*week)/(52.0*week);

(* normalized service time delay using the ratio above *)
fun norm_rel_st_delay(t,d) = rel_st_delay(t,d/eff_st_factor) ;

(* normalized service time delay using the ratio above *)
fun norm_r_st_delay(d) = r_st_delay(d/eff_st_factor);

fun expb_h(av:real, mn:real, high:real) = 
let 
	val x = exponential(1.0/av) 
in 
	if (x > high orelse x < mn)
	then expb_h(av,mn,high) 
	else x 
end
(*fun expb_st(av:real, high:real) = norm_r_st_delay(expb_h(av,high));
fun norm_st(av:real, stdv:real) = norm_r_st_delay(normal(av,stdv));*)
fun norm_st(av:real, stdv:real) = r_st_delay(normal(av,stdv));
fun expb_st(av:real, mn:real, high:real) = r_st_delay(expb_h(av,mn,high));(* Business Logic *)
fun createCO(id: int) = 
let
	val co_id = "co"^Int.toString id
	val hus = round(uniform(5.5, 30.5))
	val goods = hus*50
in
	(co_id, goods)
end;

fun expb_delay(m, mn, mx) = ModelTime.fromInt(round(expb_st(m, mn, mx)))
fun normal_delay(m, std) = ModelTime.fromInt(round(norm_st(m, std)))

fun CODelay() = ModelTime.fromInt(round(norm_r_at_delay(week/10.0)))
fun RegisterCODelay() = expb_delay(2.0*day, 30.0*minute, 5.0*day)
fun CreateTDDelay() = expb_delay(1.0*day, 30.0*minute, 2.0*day)
fun BookVHDelay() = expb_delay(1.0*hour, 5.0*minute, 2.0*hour)
fun OrderCRDelay() = expb_delay(3.0*hour, 10.0*minute, 5.0*hour)
fun CRToPlantDelay() = 
let 
	val b = uniform(0.0,1.0)
in
	if b < 0.85 
	then normal_delay(20.0*minute, 3.0*minute) 
	else normal_delay(75.0*minute, 10.0*minute)
end;

fun CollectGoodsDelay() = expb_delay(3.0*minute, 30.0, 8.0*minute)
fun LoadTruckDelay() = expb_delay(3.0*minute, 30.0, 5.0*minute)
fun DriveToTerminalDelay() = 
let 
	val b = uniform(0.0,1.0)
in
	if b < 0.6
	then normal_delay(10.0*minute, 2.0*minute)
	else normal_delay(40.0*minute, 5.0*minute)
end;
  

fun ToPickupDelay() = expb_delay(8.0*minute, 3.0*minute, 15.0*minute)

fun WeighDelay() = expb_delay(3.0*minute, 30.0, 6.0*minute)
(* Bounded exponentially distributed delay for transport time *)
fun WeighToBayDelay() = expb_delay(5.0*minute, 60.0, 8.0*minute)
fun WeighToStockDelay() = expb_delay(3.0*minute, 30.0, 5.0*minute)
fun StockToBayDelay() = expb_delay(5.0*minute, 1.0*minute, 5.0*minute)
fun LoadToVehicleDelay() = expb_delay(5.0*minute, 30.0, 8.0*minute)
fun SmallDelay() = expb_delay(5.0, 1.0, 10.0)

fun initialCOTime() = 
let
	val co_time_real = process_start_time()
	val co_time_clock = co_time_real - preprocess_start_time()
in
	ModelTime.fromInt(round(co_time_clock))
end;


(*DRIVING SIMUL*)
val truck_speed    = 80.0  (* km/h *)
val notice_upper_m = 1000
val notice_lower_m = 500

fun speed_mps() =
  truck_speed * 1000.0 / 3600.0;

fun remaining_distance_m(tm) =
  Real.fromInt(IntInf.toInt(tm)) * speed_mps();


fun noticeDelay(tm) =
let
  val trip_distance_m = remaining_distance_m(tm)
  val max_notice_m =
    Int.min(notice_upper_m - 1, Real.floor(trip_distance_m))
in
  if max_notice_m < notice_lower_m
  then ModelTime.fromInt(~1)
  else
    let
      val notice_m = Real.fromInt(discrete(notice_lower_m, max_notice_m))
    in
      ModelTime.fromInt(
        round(Real.fromInt(IntInf.toInt tm) - notice_m / speed_mps())
      )
    end
end;





fun newCRs(0, _, _, _) = [] | newCRs(nof_crs: INT, nof_hus: INT, td_id, id: INT) =
let 
	val cr_id = "cr"^(Int.toString id)
	val veh_id = "NONE"
	val hus = []
	val status = empt
	val cr_weight = ~1.0
	val rnd = uniform(0.0,1.0)
	val cr_prio = if rnd < 0.1 then crp_high else crp_normal
	val nof_hus_cr = if nof_crs = 1 then nof_hus else 6
	val loc = "unknown"
	val cr = (cr_id, td_id, veh_id, nof_hus_cr, [], status, cr_weight, cr_prio, loc)
in
	cr::newCRs(nof_crs - 1, nof_hus - 6, td_id, id + 1)
end;
fun TDForCO((oid, goods): CustomerOrder, id1: INT, id2: INT) =
let
	val td_id = "td"^Int.toString id1
	val nof_hus = goods div 50 
	val nof_crs = ((nof_hus - 1) div 6) + 1
	val crs = newCRs(nof_crs, nof_hus, td_id, id2)
in
	((td_id, crs), id2 + nof_crs)
end;

fun HUsForCR_recursive(cr_id, 0, hus, id) = (hus, id) | HUsForCR_recursive(cr_id, nof_hus: INT, hus, id) =
let
	val hu = ("hu"^Int.toString id, cr_id)
in 
	HUsForCR_recursive(cr_id, nof_hus - 1, hu::hus, id + 1)
end;
fun HUsForCR((cr_id, _, _, nof_hus, _, _, _, _, cr_location): Container, id: INT) = HUsForCR_recursive(cr_id, nof_hus, [], id);

fun deleteCR([], cr: Container) = [] | deleteCR(cr2::crs: Containers, cr: Container) = if #1 cr = #1 cr2 then crs else cr2::deleteCR(crs, cr)
fun decrPendingCRs((td_id, crs): TransportDocument, cr: Container) = (td_id, deleteCR(crs, cr))

fun departSoon(tm) =  ModelTime.add(tm, ModelTime.fromInt(60*60*24)) > time()

fun generateVH((_, clock): VHTime, id: int) = 
let
	val vh_id = "vh"^Int.toString id
	val idle_cap = round(uniform(0.0-0.5, 150.5))
in
	(vh_id, idle_cap, [], [], clock)
end;

fun initialVHTime() = 
let
	val vh_time_real = tuesday_may_30_2023()
	val vh_time_clock = vh_time_real - preprocess_start_time()
in
	(Tuesday, vh_time_clock)
end;

fun nextVHTime((weekday, clock): VHTime) = 
let
	val delta = if weekday = Tuesday then 3.0*day else 4.0*day
	val next_weekday = if weekday = Friday then Tuesday else Friday
	val next_time_0 = clock + delta
	(* summer / winter time mess handling *)
	val next_time = 
		if Date.hour(t2date(next_time_0)) = 10 then next_time_0 + hour else 
		if Date.hour(t2date(next_time_0)) = 12 then next_time_0 - hour else next_time_0
in
	(next_weekday, next_time)
end;


fun newForklift(id:int) = 
let
	val flid = "fl" ^ Int.toString id
	val loc = "asdf"
in
	(flid, loc)
end;


(* ***********************************************)
(* scheduling containers to vehicles logic *)
(* ***********************************************)

(* this is to catch the case that there is no vehicle with enough capacity for the order
	standard ml seems to not provide a proper exception handling, so identify this error through postprocessing (in the .ipynb) *)
fun ERROR_VEHICLE() = ("errorVH", 100000, [], [], now()+now())

fun assign_containers_to_vehicle([], scheduled_crs, cr_departure_times, assigned_veh) = (scheduled_crs, cr_departure_times, assigned_veh) |
	assign_containers_to_vehicle((cr_id, td_id, _, nofHus, hus, status, weight, prio, loc)::crs: Containers, scheduled_crs, cr_departure_times, veh) 
	= let 
		val (veh_id, idle_cap, scheduled_cr_ids, loaded_crs, clock) = veh
		val scheduled_cr = (cr_id, td_id, veh_id, nofHus, hus, status, weight, prio, loc)
		val cr_departure_time = (cr_id, clock)
		val assigned_veh = (veh_id, idle_cap - 1, cr_id::scheduled_cr_ids, loaded_crs, clock)
	in
		assign_containers_to_vehicle(crs, scheduled_cr::scheduled_crs, cr_departure_time::cr_departure_times, assigned_veh)
	end;

fun assign_vehicle_prio_high(crs, [], checked: Vehicles) 
= let
	(* error: no more vehicle available *)
	val veh = ERROR_VEHICLE();
	val (scheduled_crs, cr_departure_times, assigned_veh) = assign_containers_to_vehicle(crs, [], [], veh)
in
	(scheduled_crs, cr_departure_times, checked^^[assigned_veh])
end | assign_vehicle_prio_high(crs, veh::unchecked, checked)
(* greedy: take next fitting vehicle *)
= let
	val (vh_id, idle_cap, scheduled_cr_ids, loaded_crs, departure) = veh
in 
	if (idle_cap >= List.length crs) andalso (departure - now() >= 1.2*day)
	then let
			val (scheduled_crs, cr_departure_times, assigned_veh) = assign_containers_to_vehicle(crs, [], [], veh)
		in
			(scheduled_crs, cr_departure_times, checked^^[assigned_veh]^^unchecked)
		end
	else assign_vehicle_prio_high(crs, unchecked, checked^^[veh] )
end;

fun get_vehicle_idle_capacities([]) = [] | get_vehicle_idle_capacities((_, idle_cap, _, _, _)::vehs) = idle_cap::get_vehicle_idle_capacities(vehs);

fun assign_vehicle_prio_normal(crs: Containers, vehs: Vehicles) 
= let
	val idle_capacities = get_vehicle_idle_capacities(vehs)
	val b = uniform(0.0, 1.0)
	(* assumption: there are always (at least) 8 vehicles *)
	val n = if b < 0.05 then 1 else
			if b < 0.20 then 2 else
			if b < 0.40 then 3 else
			if b < 0.60 then 4 else
			if b < 0.80 then 5 else
			if b < 0.90 then 6 else 7
	val veh = List.nth(vehs, n)
in 
	if List.nth(idle_capacities, n) >= List.length crs
	then let
		val (scheduled_crs, cr_deparutre_times, assigned_veh) = assign_containers_to_vehicle(crs, [], [], veh)
		val earlier_vehs = List.take(vehs, n)
		val later_vehs = List.drop(vehs, n+1)
	in
		(scheduled_crs, cr_deparutre_times, earlier_vehs^^[assigned_veh]^^later_vehs)
	end
	(* note: this is not safe especially if no vehicle has enough idle capacity *)
	else assign_vehicle_prio_normal(crs, vehs)
end;

fun split_crs_by_prio_recursively(highs, normals, []) = (highs, normals) |
	split_crs_by_prio_recursively(highs, normals, cr::crs: Containers) =
	let
		val (cr_id, td_id, veh_id, nof_hus_cr, hus, status, cr_weight, cr_prio, cr_location) = cr
	in
		if cr_prio = crp_high 
		then split_crs_by_prio_recursively(cr::highs, normals, crs)
		else split_crs_by_prio_recursively(highs, cr::normals, crs)
	end;

fun split_crs_by_prio(crs) = split_crs_by_prio_recursively([], [], crs)

fun bookVehicles((td_id, crs): TransportDocument, vehs: Vehicles)
= let 
	(* 10% of the containers have high prio and are shipped jointly as soon as possible *)
	(* 90% are jointly assigned to some vehicle over the next few weeks *)
	val (high_prio_crs, normal_prio_crs) = split_crs_by_prio(crs)
	val vehicles_idle_caps = get_vehicle_idle_capacities(vehs)
	val (high_prio_crs, high_prio_dep_times, vehs) = assign_vehicle_prio_high(high_prio_crs, vehs, [])
	val (normal_prio_crs, normal_prio_dep_times, vehs) = assign_vehicle_prio_normal(normal_prio_crs, vehs)
	val crs = high_prio_crs^^normal_prio_crs
	val td: TransportDocument = (td_id, crs)
	val crdts = high_prio_dep_times^^normal_prio_dep_times
in
	(td, vehs, crdts)
end;

fun untilAboutOneDayBeforeDepartureTime((_, _, _, _, clock): Vehicle) = 
let 
	val slightly_more_than_one_day = clock - now() - 1.5*day
	val sample_in_days = normal(slightly_more_than_one_day / day, 0.05)
	val sample_in_seconds = round(sample_in_days * day)
in 
	ModelTime.fromInt(sample_in_seconds)
end;
fun untilDepartureTime((_, _, _, _, clock): Vehicle) = ModelTime.fromInt(round(clock - now()))


(* Container handling should be initiated at least three days before vehicle departure *)
fun samplePickTime(departure: Clock) = 
let
	val nowtime = now()
	val maxtime = departure - 3.0*day - nowtime
	val halftime = maxtime/2.0
	val depart_soon = maxtime < 0.0
in
	if depart_soon then ModelTime.fromInt(0)
	else let
			val b = uniform(0.0,1.0)
			val t = if b < 0.8 then uniform(0.0, halftime) else uniform(halftime, maxtime)
		in
			ModelTime.fromInt(round(t))
		end
end;(* FILE HANDLING *)

val OBJECT_TYPES = ["Customer Order", "Transport Document", "Container", "Truck", "Handling Unit", "Forklift", "Vehicle", "Location"]
val EVENT_TYPES = [
	"Register Customer Order",
	"Create Transport Document",
	"Book Vehicles",
	"Order Empty Containers",
	"Pick Up Empty Container",
	"Collect Goods",
	"Load Truck",
	"Send Truck Arrival Notice",
	"Drive to Terminal",
	"Unload Truck",
	"Weigh",
	"Place in Stock",
	"Pick from Stock",
	"Pick from Bay",
	"Bring to Loading Bay",
	"Load to Vehicle",
	"Reschedule Container",
	"Depart",
	"Enter Location",
	"Leave Location"
];
val SEP = ";";


fun list2string([]) = ""|
list2string(x::l) = x ^ (if l=[] then "" else SEP) ^ list2string(l);

(* attributes *)
fun eas_by_type(a: EventType) = 
	[];
	
fun oas_by_type(ot: ObjectType) = 
	if ot="Customer Order" then ["Amount of Goods"] else 
	if ot="Transport Document" then ["Amount of Containers", "Status"] else 
	if ot="Container" then ["Amount of Handling Units", "Status", "Weight", "Current Location"] else 
	if ot="Truck" then [] else 
	if ot="Forklift" then [] else 
	if ot="Vehicle" then ["Departure Date", "Current Location"] else 
	if ot="Handling Unit" then [] else 
	if ot="Location" then ["Descriptor"] else
	[];
	
(* table management *)
fun event_map_type(a: EventType) = 
	if a="Register Customer Order" then "RegisterCustomerOrder" else 
	if a="Create Transport Document" then "CreateTransportDocument" else
	if a="Book Vehicles" then "BookVehicles" else
	if a="Order Empty Containers" then "OrderEmptyContainers" else
	if a="Pick Up Empty Container" then "PickUpEmptyContainer" else
	if a="Collect Goods" then "CollectGoods" else
	if a="Load Truck" then "LoadTruck" else
	if a="Send Truck Arrival Notice" then "SendTruckArrivalNotice" else
	if a="Drive to Terminal" then "DriveToTerminal" else
	if a="Unload Truck" then "UnloadTruck" else
	if a="Weigh" then "Weigh" else
	if a="Place in Stock" then "PlaceInStock" else
	if a="Pick from Stock" then "PickFromStock" else
	if a="Pick from Bay" then "PickFromBay" else
	if a="Bring to Loading Bay" then "BringToLoadingBay" else
	if a="Reschedule Container" then "RescheduleContainer" else
	if a="Load to Vehicle" then "LoadToVehicle" else
	if a="Depart" then "Depart" else
	if a="Enter Location" then "EnterLocation" else
	if a="Leave Location" then "LeaveLocation" else
	"";
	
fun object_map_type(ot: ObjectType) = 
	if ot="Customer Order" then "CustomerOrder" else
	if ot="Transport Document" then "TransportDocument" else
	if ot="Container" then "Container" else
	if ot="Truck" then "Truck" else
	if ot="Forklift" then "Forklift" else
	if ot="Handling Unit" then "HandlingUnit" else
	if ot="Vehicle" then "Vehicle" else
	if ot="Location" then "Location" else
	"";

(* lifecycle values *)
val LC_ASSIGN = "assign";
val LC_START = "start";
val LC_COMPLETE = "complete";

(* helper: join strings with '_' *)
fun join_underscore([]) = ""
  | join_underscore([x]) = x
  | join_underscore(x::xs) = x ^ "_" ^ join_underscore(xs);

(* helper: build instance_id (no '|'; uses '_' and '-') *)
fun mk_instance_id(event_id, co_id, td_id, cr_id) =
let
	val co_part = if co_id = "" then [] else ["co-" ^ co_id]
	val td_part = if td_id = "" then [] else ["td-" ^ td_id]
	val cr_part = if cr_id = "" then [] else ["cr-" ^ cr_id]
	val parts = co_part^^td_part^^cr_part
in
	if List.length parts = 0 then "inst-" ^ event_id else join_underscore(parts)
end;

fun mk_instance_id_vh(event_id, vh_id) =
	if vh_id = "" then "inst-" ^ event_id else "vh-" ^ vh_id;

fun is_location_movement_event(et: EventType) = (et = "Enter Location" orelse et = "Leave Location");

(* Location object id helper *)
fun location_oid(location: string) =
	"loc_" ^ (String.implode (map (fn c => if c = #" " then #"_" else c) (String.explode location)));

val LOCATION_DESCRIPTORS = [
	"Truck Depot",
	"Terminal",
	"Terminal North",
	"Terminal South",
	"Forklift Depot",
	"Storage",
	"Loading Bay",
	"Weighbridge",
	"Dispatch Center",
	"Supplier",
	"Supplier Loading Area",
	"Road to Terminal",
	"Stock",
	"On Vehicle",
	"Outbound Route"
];

(* OCEL output folder handling
   The simulation is invoked by CPN Tools; this SML file is not standalone.
   We keep all CSV outputs in a dedicated folder that is cleared per run. *)
val OUTPUT_DIR = "ocel_output";

fun out_path(filename: string) = OS.Path.concat(OUTPUT_DIR, filename);

fun ensure_clean_output_dir() =
let
	val _ = (OS.FileSys.mkDir OUTPUT_DIR) handle _ => ()
	val dir = OS.FileSys.openDir OUTPUT_DIR
	fun loop() =
		case OS.FileSys.readDir dir of
			NONE => ()
		  | SOME entry =>
			(
				if entry = "." orelse entry = ".." then ()
				else (OS.FileSys.remove(out_path entry) handle _ => ());
				loop()
			)
	val _ = loop()
	val _ = OS.FileSys.closeDir dir
	val _ = (OS.FileSys.rmDir OUTPUT_DIR) handle _ => ()
	val _ = (OS.FileSys.mkDir OUTPUT_DIR) handle _ => ()
in
	()
end;

(* table initializations *)
fun create_event_table() = 
let
	val file_id = TextIO.openOut(out_path("event.csv"))
   val _ = TextIO.output(file_id, list2string(["ocel_id", "ocel_type"])) 
   val _ = TextIO.output(file_id, "\n")
in
   TextIO.closeOut(file_id)
end;

fun create_object_table() = 
let
	val file_id = TextIO.openOut(out_path("object.csv"))
   val _ = TextIO.output(file_id, list2string(["ocel_id", "ocel_type"])) 
   val _ = TextIO.output(file_id, "\n")
in
   TextIO.closeOut(file_id)
end;

fun create_event_object_table() = 
let
	val file_id = TextIO.openOut(out_path("event_object.csv"))
   val _ = TextIO.output(file_id, list2string(["ocel_event_id", "ocel_object_id", "ocel_qualifier"])) 
   val _ = TextIO.output(file_id, "\n")
in
   TextIO.closeOut(file_id)
end;

fun create_object_object_table() = 
let
	val file_id = TextIO.openOut(out_path("object_object.csv"))
   val _ = TextIO.output(file_id, list2string(["ocel_source_id", "ocel_target_id", "ocel_qualifier"])) 
   val _ = TextIO.output(file_id, "\n")
in
   TextIO.closeOut(file_id)
end;

fun write_event_map_types(file_id, []) = () | write_event_map_types(file_id, et::ets) = (TextIO.output(file_id, list2string([et, event_map_type(et)])); TextIO.output(file_id, "\n"); write_event_map_types(file_id, ets)) 

fun write_object_map_types(file_id, []) = () | write_object_map_types(file_id, ot::ots) = (TextIO.output(file_id, list2string([ot, object_map_type(ot)])); TextIO.output(file_id, "\n"); write_object_map_types(file_id, ots))

fun create_event_map_type_table() = 
let
	val file_id = TextIO.openOut(out_path("event_map_type.csv"))
   val _ = TextIO.output(file_id, list2string(["ocel_type", "ocel_type_map"])) 
   val _ = TextIO.output(file_id, "\n")
   val _ = write_event_map_types(file_id, EVENT_TYPES)
in
   TextIO.closeOut(file_id)
end;

fun create_object_map_type_table() = 
let
	val file_id = TextIO.openOut(out_path("object_map_type.csv"))
   val _ = TextIO.output(file_id, list2string(["ocel_type", "ocel_type_map"])) 
   val _ = TextIO.output(file_id, "\n")
   val _ = write_object_map_types(file_id, OBJECT_TYPES)
in
   TextIO.closeOut(file_id)
end;

fun create_event_type_table(a: EventType) = 
let
   val emt = event_map_type(a)
   val eas = eas_by_type(a)
	val file_id = TextIO.openOut(out_path("event_" ^ emt ^ ".csv"))
	val _ =
	   if is_location_movement_event(a)
	   then TextIO.output(file_id, list2string(["ocel_id", "ocel_time"]))
	   else TextIO.output(file_id, list2string(["ocel_id", "ocel_time", "lifecycle", "instance_id"]^^eas))
   val _ = TextIO.output(file_id, "\n")
in
   TextIO.closeOut(file_id)
end;

fun create_event_type_tables([]) = () | create_event_type_tables(a::a_s) = (create_event_type_table(a); create_event_type_tables(a_s));

fun create_object_type_table(ot: ObjectType) = 
let
   val omt = object_map_type(ot)
   val oas = oas_by_type(ot)
	val file_id = TextIO.openOut(out_path("object_" ^ omt ^ ".csv"))
   val _ = TextIO.output(file_id, list2string(["ocel_id", "ocel_time", "ocel_changed_field"]^^oas))
   val _ = TextIO.output(file_id, "\n")
in
   TextIO.closeOut(file_id)
end;

fun create_object_type_tables([]) = () | create_object_type_tables(ot::ots) = (create_object_type_table(ot); create_object_type_tables(ots));

(* Util *)
fun ints2strings([]) = [] |ints2strings(i::is) = Int.toString(i)::ints2strings(is)
fun nested_reverse([]) = [] | nested_reverse(l::ls) = (rev l)::nested_reverse(ls)
fun index_recursive([], _, _) = ~1 | index_recursive(y::xs, x, i) = if x=y then i else index_recursive(xs, x, i+1)
fun index([], _) = ~1 | index(xs, x) = index_recursive(xs, x, 0)
fun container_ids([]) = [] | container_ids((cr_id,_,_,_,_,_,_,_,cr_location)::crs: Containers) = cr_id::container_ids(crs)
fun transport_document_ids_recursive([], ids) = ids | 
	transport_document_ids_recursive((_,td_id,_,_,_,_,_,_,cr_location)::crs: Containers, ids) = 
		if mem ids td_id 
		then transport_document_ids_recursive(crs, ids)
		else transport_document_ids_recursive(crs, td_id::ids)
fun transport_document_ids(crs) = transport_document_ids_recursive(crs, [])

(* generic function for writing a list of strings to .csv *)
fun write_record(file_id, l) = 
let
   val file = TextIO.openAppend(file_id)
   val _ = TextIO.output(file, list2string(l))
   val _ = TextIO.output(file, "\n")
in
   TextIO.closeOut(file)
end;

(* write normal event to table "event" and respective event type table *)
fun write_event(event_id, et: EventType, lifecycle: string, instance_id: string, ea_values: string list) = 
let
	val event_file_id = out_path("event.csv")
	val event_type_file_id = out_path("event_" ^ event_map_type(et) ^ ".csv")
	val date = t2s(now())
	val _ = write_record(event_file_id, [event_id, et])
	val _ = write_record(event_type_file_id, [event_id, date, lifecycle, instance_id]^^ea_values)
in
   event_id
end;

(* write location movement event (Enter/Leave) with minimal schema to its per-type table *)
fun write_event_location(event_id, et: EventType) =
let
	val event_file_id = out_path("event.csv")
	val event_type_file_id = out_path("event_" ^ event_map_type(et) ^ ".csv")
	val date = t2s(now())
	val _ = write_record(event_file_id, [event_id, et])
	val _ = write_record(event_type_file_id, [event_id, date])
in
	event_id
end;

(* write qualified relations to table "object_object" *)
fun write_relations_recursively(file, []) = TextIO.closeOut(file) | 
write_relations_recursively(file, [obj_id1, obj_id2, qualifier]::qualified_pairs) = 
   (TextIO.output(file, list2string([obj_id1, obj_id2, qualifier]));
   TextIO.output(file, "\n");
   write_relations_recursively(file, qualified_pairs));

fun cartesian_single(_,[],_) = [] | cartesian_single(obj_id1, obj_id2::obj_ids, qualifier) = [obj_id1, obj_id2, qualifier]::cartesian_single(obj_id1, obj_ids, qualifier)

fun relationship_cartesian([],_,_) = [] | relationship_cartesian(obj_id1::obj_ids, obj_ids2, qualifier) = cartesian_single(obj_id1, obj_ids2, qualifier)^^relationship_cartesian(obj_ids, obj_ids2, qualifier)

fun write_e2o_relations(qualified_pairs) =
let
	val file = TextIO.openAppend(out_path("event_object.csv"))
in
   write_relations_recursively(file, qualified_pairs)
end;

fun write_o2o_relations(qualified_pairs) =
let
	val file = TextIO.openAppend(out_path("object_object.csv"))
in
   write_relations_recursively(file, qualified_pairs)
end;

(* write object to table "object" and respective object type table *)
fun initialize_objects_recursively(object_file, object_type_file, object_type, []) = (TextIO.closeOut(object_file); TextIO.closeOut(object_type_file)) |
initialize_objects_recursively(object_file, object_type_file, object_type, (object_id::oa_values)::objects_with_attribute_values) =
let
	(*val time = tinit2s()*)
	val time = t2s(now())
	val changed_field = ""
in
   (TextIO.output(object_file, list2string([object_id, object_type]));
   TextIO.output(object_file, "\n");
   TextIO.output(object_type_file, list2string([object_id, time, changed_field]^^oa_values));
   TextIO.output(object_type_file, "\n");  
   initialize_objects_recursively(object_file, object_type_file, object_type, objects_with_attribute_values))
end;

fun initialize_objects(object_type, objects_with_attribute_values) = 
let
	val object_file_id = out_path("object.csv")
	val object_type_file_id = out_path("object_" ^ object_map_type(object_type) ^ ".csv")
   val object_file = TextIO.openAppend(object_file_id)
   val object_type_file = TextIO.openAppend(object_type_file_id)
in
   initialize_objects_recursively(object_file, object_type_file, object_type, objects_with_attribute_values)
end;

(* initialize Location objects after initialize_objects is in scope *)
fun initialize_locations([]) = ()
  | initialize_locations(loc::locs) =
  let
		val oid = location_oid(loc)
		val _ = initialize_objects("Location", [oid::[loc]])
	  in
		initialize_locations(locs)
	  end;

fun create_logs() = (
	ensure_clean_output_dir();
	create_event_table(); 
	create_object_table(); 
	create_event_object_table(); 
	create_object_object_table(); 
	create_event_map_type_table(); 
	create_object_map_type_table(); 
	create_event_type_tables(EVENT_TYPES); 
	create_object_type_tables(OBJECT_TYPES);
	initialize_locations(LOCATION_DESCRIPTORS)
);


(***********************)
(* object initializers *)
(***********************)

fun initialize_vehicle((veh_id, idle_cap, td_ids, cr_ids, clock): Vehicle) = 
let
   val objects_with_attribute_values = [veh_id::[t2s(clock), "Terminal Gate"]]
in
   if List.length td_ids > 0 orelse List.length cr_ids > 0 then initialize_objects("Vehicle", objects_with_attribute_values)
   else ()
end;

fun initialize_customer_order((co_id, nof_goods): CustomerOrder) = 
let
   val objects_with_attribute_values = [co_id::[Int.toString nof_goods]]
in
   initialize_objects("Customer Order", objects_with_attribute_values)
end;

fun initialize_transport_document((td_id, crs): TransportDocument) = 
let
   val objects_with_attribute_values = [td_id::[Int.toString (List.length crs)]]
in
   initialize_objects("Transport Document", objects_with_attribute_values)
end;

fun containers_attribute_values([]) = [] | containers_attribute_values((cr_id, _, _, nof_hus, _, cr_status, _, _, cr_location)::crs: Containers) = 
let
	val status_str = if cr_status = empt then "empty" else "full" 
in 
	(cr_id::[Int.toString nof_hus, status_str, "Supplier"])::containers_attribute_values(crs)
end;
fun initialize_containers(crs: Containers) = 
let
   val objects_with_attribute_values = containers_attribute_values(crs)
in
   initialize_objects("Container", objects_with_attribute_values)
end;

fun initialize_handling_unit((hu_id, cr_id): HandlingUnit) = 
let
   val objects_with_attribute_values = [hu_id::[]]
in
   initialize_objects("Handling Unit", objects_with_attribute_values)
end;

fun initialize_truck(tr_id: Truck) = 
let
   val objects_with_attribute_values = [tr_id::[]]
in
   initialize_objects("Truck", objects_with_attribute_values)
end;

fun initialize_forklift((fl_id, loc): Forklift) = 
let
   val objects_with_attribute_values = [fl_id::[]]
in
   initialize_objects("Forklift", objects_with_attribute_values)
end;


(****************************)
(* object attribute updates *)
(****************************)

fun skipstrings(0) = [] | skipstrings(x) = ""::skipstrings(x-1)
fun update_object_attribute(object_type, object_id, changed_field, changed_value) =
let
	val object_type_file_id = out_path("object_" ^ object_map_type(object_type) ^ ".csv")
   val object_type_file = TextIO.openAppend(object_type_file_id)
   val time = t2s(now())
   val skips = skipstrings(index(oas_by_type(object_type), changed_field))
in
   (
   TextIO.output(object_type_file, list2string([object_id, time, changed_field]^^skips^^[changed_value]));
   TextIO.output(object_type_file, "\n");
   TextIO.closeOut(object_type_file)
   )
end;

fun update_cr_status((cr_id, _, _, _, _, cr_status, _, _, cr_location): Container) = 
let 
	val status = if cr_status = full then "full" else "empty" 
in 
	update_object_attribute("Container", cr_id, "Status", status) 
end;
fun update_cr_weight(cr_id, weight) = update_object_attribute("Container", cr_id, "Weight", Real.toString weight)

(* Control status of transport documents concerning full departures (update if all containers have departed) *)		
fun decrement_pending_crs(td_id, (td_id2, nof_crs)::tdcs) = 
if td_id = td_id2 then 
	let 
		val new_nof_crs = nof_crs - 1
	in
		if new_nof_crs = 0 then 
			let 
				val _ = update_object_attribute("Transport Document", td_id, "Status", "shipped")	
			in 
				tdcs
			end
		else (td_id, nof_crs - 1)::tdcs
	end
else (td_id2, nof_crs)::decrement_pending_crs(td_id, tdcs);

fun update_departed_recursive([], tdcs) = tdcs | 
	update_departed_recursive((_, td_id, _, _, _, _, _, _, cr_location)::crs, tdcs) = 
		update_departed_recursive(crs, decrement_pending_crs(td_id, tdcs))
fun update_departed((_, _, _, loaded_crs, _), tdcs) = update_departed_recursive(loaded_crs, tdcs)

fun updateTDStatus((td_id, _): TransportDocument, status) = update_object_attribute("Transport Document", td_id, "Status", status);


fun update_vehicle_location(veh_id, location) = update_object_attribute("Vehicle", veh_id, "Current Location", location)

(*********************************)
(* location-aware event helpers *)
(*********************************)


val loc_truck_depot = "Truck Depot"
val loc_terminal = "Terminal"
val loc_terminal_north = "Terminal North"
val loc_terminal_south = "Terminal South"
val loc_forklift_depot = "Forklift Depot"
val loc_forklift_init = "Forklift Depot"
val loc_storage = "Storage"
val loc_loading_bay = "Loading Bay"
val loc_weighbridge = "Weighbridge"

val loc_dispatch_center = "Dispatch Center"
val loc_supplier = "Supplier"
val loc_supplier_loading_area = "Supplier Loading Area"
val loc_road_to_terminal = "Road to Terminal"
val loc_on_vehicle = "On Vehicle"
val loc_outbound_route = "Outbound Route"

fun single_relation(event_id, obj_id, qualifier) = [[event_id, obj_id, qualifier]]
fun retag_relations(_, []) = [] | retag_relations(event_id, [_, obj_id, qualifier]::rels) = [event_id, obj_id, qualifier]::retag_relations(event_id, rels)

fun write_event_with_relations(event_id, et: EventType, lifecycle: string, instance_id: string, ea_values: string list, e2o_relations) =
(
	if is_location_movement_event(et)
	then write_event_location(event_id, et)
	else write_event(event_id, et, lifecycle, instance_id, ea_values);
	write_e2o_relations(e2o_relations)
);

(* 
fun write_enter_location(event_id, obj_id, qualifier, location, context) =
	write_event_with_relations(event_id, "Enter Location", [location, context], single_relation(event_id, obj_id, qualifier));

fun write_leave_location(event_id, obj_id, qualifier, location, context) =
	write_event_with_relations(event_id, "Leave Location", [location, context], single_relation(event_id, obj_id, qualifier));
	*)
	
val event_seq = ref 0;
fun generate_event_id(suffix) = 
let
    val _ = event_seq := !event_seq + 1
in
    Int.toString(!event_seq) ^ "_" ^ suffix
end;

fun concat_oids([]: OIds) = ""
  | concat_oids([obj_id]) = obj_id
  | concat_oids(obj_id::objs) = obj_id ^ "_" ^ concat_oids(objs);


fun relations_from_ids(_, []) = []
  | relations_from_ids(event_id, obj_id::objs) =
      single_relation(event_id, obj_id, "moved_object") ^^
      relations_from_ids(event_id, objs);

fun write_enter_location(location, obj_ids: OIds) =
let
	val obj_str = concat_oids(obj_ids)
    val event_id = generate_event_id("enter_" ^ location ^ "_" ^ obj_str)
	val moved_rels = relations_from_ids(event_id, obj_ids)
	val loc_rel = [event_id, location_oid(location), "location"]
in
	write_event_with_relations(
		event_id,
		"Enter Location",
		LC_COMPLETE,
		mk_instance_id(event_id, "", "", ""),
		[],
		moved_rels^^[loc_rel]
	)
end;

fun write_leave_location(location, obj_ids: OIds) =
let
	val obj_str = concat_oids(obj_ids)
    val event_id = generate_event_id("leave_" ^ location ^ "_" ^ obj_str)
	val moved_rels = relations_from_ids(event_id, obj_ids)
	val loc_rel = [event_id, location_oid(location), "location"]
in
	write_event_with_relations(
		event_id,
		"Leave Location",
		LC_COMPLETE,
		mk_instance_id(event_id, "", "", ""),
		[],
		moved_rels^^[loc_rel]
	)
end;

fun enter_location(location, obj_ids: OIds) =
    write_enter_location(location, obj_ids);

fun leave_location(location, obj_ids: OIds) =
    write_leave_location(location, obj_ids);
	
	(* alternative to create one event per object id 
	fun enter_location(_, []) = ()
	  | enter_location(location, obj_id::objs) =
		let
		  val event_id = generate_event_id("enter_" ^ location ^ obj_id)
		  val _ =
			write_event_with_relations(
			  event_id,
			  "Enter Location",
			  [location, ""],
			  single_relation(event_id, obj_id, "some_qualifier")
			)
		in
		  enter_location(location, objs)
		end;
	*)
	
	
fun write_lifecycle_event(event_id, et: EventType, lifecycle: string, instance_id: string, e2o_relations) =
	write_event_with_relations(event_id, et, lifecycle, instance_id, [], retag_relations(event_id, e2o_relations));

(* this is fully optional, there is no real assignment logic behind vehicle booking except for it having enough space *)
fun write_booking_assignments_recursive(_, []) = () |
	write_booking_assignments_recursive(td_id, (cr_id, _, veh_id, _, _, _, _, _, cr_location)::crs: Containers) =
	let
		val event_id = "assign_load_"^cr_id
		val e2o_relations = [[event_id, cr_id, "assigned CR"], [event_id, veh_id, "assigned VH"]]
		val instance_id = mk_instance_id(event_id, "", td_id, cr_id)
	in
		(
		write_lifecycle_event(event_id, "Load to Vehicle", LC_ASSIGN, instance_id, e2o_relations);
		write_booking_assignments_recursive(td_id, crs)
		)
	end;

(**********)
(* Events *)
(**********)

fun write_register_order((co_id, nof_goods): CustomerOrder) =
let
	val event_id = "reg_"^co_id 
	val event_order = [[event_id, co_id, "registered CO"]]
	val e2o_relations = event_order
	val instance_id = mk_instance_id(event_id, co_id, "", "")
in
	(
	write_event(event_id, "Register Customer Order", LC_COMPLETE, instance_id, []);
	initialize_customer_order((co_id, nof_goods));
	write_e2o_relations(e2o_relations)
	)
end;

fun write_create_document((co_id, nof_goods): CustomerOrder, td: TransportDocument) =
let
	val (td_id, crs) = td
	val event_id = "create_"^td_id 
	val event_order = [[event_id, co_id, "TD created for CO"]]
	val event_document = [[event_id, td_id, "created TD"]]
	val e2o_relations = event_order^^event_document
	val order_document = [[co_id, td_id, "TD for CO"]]
	val o2o_relations = order_document
	val instance_id = mk_instance_id(event_id, co_id, td_id, "")
in
	(
	write_event(event_id, "Create Transport Document", LC_COMPLETE, instance_id, []);
	initialize_transport_document(td);
	write_e2o_relations(e2o_relations);
	write_o2o_relations(o2o_relations)
	)
end;

fun booked_vehicles_ids_recursive(highs, normals, []) = (highs, normals) | booked_vehicles_ids_recursive(highs, normals, (_, _, veh_id, _, _, _, _, cr_prio, cr_location)::crs) = 
	if cr_prio = crp_high 
	then if mem highs veh_id then booked_vehicles_ids_recursive(highs, normals, crs) else booked_vehicles_ids_recursive(veh_id::highs, normals, crs)
	else if mem normals veh_id then booked_vehicles_ids_recursive(highs, normals, crs) else booked_vehicles_ids_recursive(highs, veh_id::normals, crs)
fun booked_vehicles_ids(crs) = booked_vehicles_ids_recursive([], [], crs)

fun write_book_vehicles(td: TransportDocument) = 
let
	val (td_id, crs) = td
	val (high_prio_veh_ids, normal_prio_veh_ids) = booked_vehicles_ids(crs)
	val veh_ids = high_prio_veh_ids^^normal_prio_veh_ids
	val event_id = "book_vehs_"^td_id 
	val event_td = [[event_id, td_id, "VHs booked for TD"]]
	val event_vehs = relationship_cartesian([event_id], veh_ids, "booked VHs")
	val td_vehs1 = relationship_cartesian([td_id], high_prio_veh_ids, "High-Prio VH for TD")
	val td_vehs2 = relationship_cartesian([td_id], normal_prio_veh_ids, "Regular VH for TD")
	val e2o_relations = event_td^^event_vehs
	val o2o_relations = td_vehs1^^td_vehs2
	val instance_id = mk_instance_id(event_id, "", td_id, "")
	val _ = (
		write_event(event_id, "Book Vehicles", LC_COMPLETE, instance_id, []);
		write_e2o_relations(e2o_relations);
		write_o2o_relations(o2o_relations)
	)
in
	()
end;

fun write_order_containers(td_id: TDId, crs: Containers) =
let
	val event_id = "order_crs_"^td_id 
	val cr_ids = container_ids(crs)
	val event_document = [[event_id, td_id, "ordered for TD"]]
	val event_containers = relationship_cartesian([event_id], cr_ids, "CRs ordered")
	val e2o_relations = event_document^^event_containers
	val document_containers = relationship_cartesian(cr_ids, [td_id], "CR for TD")
	val o2o_relations = document_containers
	val instance_id = mk_instance_id(event_id, "", td_id, "")
in
	(
	write_event(event_id, "Order Empty Containers", LC_COMPLETE, instance_id, []);
	write_e2o_relations(e2o_relations);
	write_o2o_relations(o2o_relations)
	)
end;

fun enter_loc() = 
let
    (* TODO *)
in
    ()
end;

fun enter_truck_depot(tr_id: Truck) = 
let
	val _ = enter_location(loc_truck_depot, [tr_id])
in
    ()
end;
fun leave_truck_depot(tr_id: Truck) = 
let
	val _ = leave_location(loc_truck_depot, [tr_id])
in
    ()
end;




fun enter_terminal(obj_ids: OIds) =
    enter_location(loc_terminal, obj_ids);

fun leave_terminal(obj_ids: OIds) =
    leave_location(loc_terminal, obj_ids);

fun leave_terminal_tr(tr_id:Truck, loc) =
    leave_location(loc, [tr_id]);


(* Forklift specific location enter for updating internal Object Location Attribute for Simulation *)
fun enter_location_fl((fl_id, new_loc): Forklift) =
let
    val _ = enter_location(new_loc, [fl_id])
in
    (fl_id, new_loc)
end;

fun leave_location_fl((fl_id, old_loc): Forklift) =
let
    val _ = leave_location(old_loc, [fl_id])
    (* optionally: add a "left"-tag to know if it's currently within the location
       or if this is just the most recent location *)
	(* old location is kept internally for task assignment though we should redo that *)
in
    (fl_id, old_loc)
end;

fun enter_location_cr((cr_id, td_id, veh_id, nof_hus, hus, cr_status, cr_weight, cr_prio, cr_location): Container, new_loc) =
let
    val _ = enter_location(new_loc, [cr_id])
in
    (cr_id, td_id, veh_id, nof_hus, hus, cr_status, cr_weight, cr_prio, new_loc)
end;
fun leave_location_cr((cr_id, td_id, veh_id, nof_hus, hus, cr_status, cr_weight, cr_prio, cr_location): Container) =
let
    val _ = leave_location(cr_location, [cr_id])
    (* optionally: add a "left"-tag to know if it's currently within the location
       or if this is just the most recent location *)
in
    (cr_id, td_id, veh_id, nof_hus, hus, cr_status, cr_weight, cr_prio, cr_location)
end;

fun enter_forklift_depot((fl_id, _): Forklift) = 
	enter_location_fl((fl_id, loc_forklift_depot));
	
fun leave_forklift_depot((fl_id, old_loc): Forklift) = 
	leave_location_fl((fl_id, old_loc));
	
fun enter_terminal_fl((fl_id, _): Forklift, term_loc) =
    enter_location_fl((fl_id, term_loc));

fun leave_terminal_fl((fl_id, old_loc): Forklift) =
    leave_location_fl((fl_id, old_loc));
	
fun leave_terminal_fl_cr
    ((fl_id, old_loc): Forklift,
     (cr_id, td_id, veh_id, nof_hus, hus, cr_status, cr_weight, cr_prio, cr_location): Container) =
let
    val fl2 = leave_location_fl((fl_id, old_loc))
    val cr2 = leave_location_cr((cr_id, td_id, veh_id, nof_hus, hus, cr_status, cr_weight, cr_prio, cr_location))
in
    (fl2, cr2)
end;

fun enter_terminal_tr_cr(tr_id: Truck, (cr_id, td_id, veh_id, nof_hus, hus, cr_status, cr_weight, cr_prio, cr_location): Container) =
let
    val new_loc = if uniform(0.0, 1.0) < 0.7 then loc_terminal_north else loc_terminal_south
    val _ = enter_location(new_loc, [tr_id])
    val cr2 = enter_location_cr((cr_id, td_id, veh_id, nof_hus, hus, cr_status, cr_weight, cr_prio, cr_location), new_loc)
in
    (cr2)
end;



fun enter_loading_bay_fl_cr
    ((fl_id, _): Forklift,
     (cr_id, td_id, veh_id, nof_hus, hus, cr_status, cr_weight, cr_prio, cr_location): Container) =
let
    val new_loc = loc_loading_bay
    val fl2 = enter_location_fl((fl_id, new_loc))
    val cr2 = enter_location_cr((cr_id, td_id, veh_id, nof_hus, hus, cr_status, cr_weight, cr_prio, cr_location), new_loc)
in
    (fl2, cr2)
end;

fun enter_loading_bay_fl((fl_id, _): Forklift) =
    enter_location_fl((fl_id, loc_loading_bay));

fun leave_loading_bay((fl_id, old_loc): Forklift) =
    leave_location_fl((fl_id, old_loc));

fun enter_loading_bay_veh((veh_id, idle_cap, scheduled_cr_ids, loaded_crs, clock): Vehicle) =
	enter_location(loc_loading_bay, [veh_id]);

fun leave_loading_bay_veh((veh_id, idle_cap, scheduled_cr_ids, loaded_crs, clock): Vehicle) =
	leave_location(loc_loading_bay, [veh_id]);
	
fun enter_storage_fl((fl_id, _): Forklift) = 
	enter_location_fl((fl_id, loc_storage));
	
fun leave_storage_fl((fl_id, old_loc): Forklift) = 
	leave_location_fl((fl_id, old_loc));

fun enter_storage
    ((fl_id, _): Forklift,
     (cr_id, td_id, veh_id, nof_hus, hus, cr_status, cr_weight, cr_prio, cr_location): Container) =
let
    val new_loc = loc_storage
    val fl2 = enter_location_fl((fl_id, new_loc))
    val cr2 = enter_location_cr((cr_id, td_id, veh_id, nof_hus, hus, cr_status, cr_weight, cr_prio, cr_location), new_loc)
in
    (fl2, cr2)
end;
fun leave_storage
    ((fl_id, old_loc): Forklift,
     (cr_id, td_id, veh_id, nof_hus, hus, cr_status, cr_weight, cr_prio, cr_location): Container) =
let
    val fl2 = leave_location_fl((fl_id, old_loc))
    val cr2 = leave_location_cr((cr_id, td_id, veh_id, nof_hus, hus, cr_status, cr_weight, cr_prio, cr_location))
in
    (fl2, cr2)
end;

(* Location based task assignment *)
val LOCATION_GRAPH = [
	(loc_terminal_north, [(loc_loading_bay, 2), (loc_storage, 1), (loc_weighbridge, 1), (loc_forklift_depot, 1), (loc_terminal_south, 3)]),
	(loc_terminal_south, [(loc_loading_bay, 1), (loc_weighbridge, 1), (loc_forklift_depot, 1), (loc_terminal_north, 3)]),
	(loc_loading_bay, [(loc_terminal_south, 1), (loc_terminal_north, 2)]),
	(loc_storage, [(loc_terminal_north, 1), (loc_weighbridge, 1)]),
	(loc_weighbridge, [(loc_terminal_north, 1), (loc_terminal_south, 1), (loc_storage, 1)]),
	(loc_forklift_depot, [(loc_terminal_north, 1), (loc_terminal_south, 1)])
];

(* Prototype-friendly distance overrides.
   Checked before graph shortest-path; use this as a simple matrix/list to tweak distances quickly. *)
val LOCATION_DISTANCE_OVERRIDES = [
	(* Loading bay closer to terminal south *)
	(loc_loading_bay, loc_terminal_south, 1),
	(loc_terminal_south, loc_loading_bay, 1),
	(loc_loading_bay, loc_terminal_north, 2),
	(loc_terminal_north, loc_loading_bay, 2),
	(* Storage closer to terminal north *)
	(loc_storage, loc_terminal_north, 1),
	(loc_terminal_north, loc_storage, 1),
	(loc_storage, loc_terminal_south, 3),
	(loc_terminal_south, loc_storage, 3),
	(* Weighbridge distance 1 to any terminal *)
	(loc_weighbridge, loc_terminal_north, 1),
	(loc_terminal_north, loc_weighbridge, 1),
	(loc_weighbridge, loc_terminal_south, 1),
	(loc_terminal_south, loc_weighbridge, 1),
	(* Forklift depot distance 1 to any terminal *)
	(loc_forklift_depot, loc_terminal_north, 1),
	(loc_terminal_north, loc_forklift_depot, 1),
	(loc_forklift_depot, loc_terminal_south, 1),
	(loc_terminal_south, loc_forklift_depot, 1),
	(* Weighbridge to storage *)
	(loc_weighbridge, loc_storage, 1),
	(loc_storage, loc_weighbridge, 1),
	(* Between terminals *)
	(loc_terminal_north, loc_terminal_south, 3),
	(loc_terminal_south, loc_terminal_north, 3)
];

fun lookup_distance(_, _, []) = NONE
	| lookup_distance(from_loc, to_loc, (f, t, d)::rest) =
		if from_loc = f andalso to_loc = t then SOME d
		else lookup_distance(from_loc, to_loc, rest);

fun neighbors(_, []) = []
	| neighbors(loc, (l, ns)::rest) = if loc = l then ns else neighbors(loc, rest);

fun member(_, []) = false
	| member(x, y::ys) = (x = y) orelse member(x, ys);

fun extract_min([x]) = (x, [])
	| extract_min((loc, dist)::rest) =
		let
			val ((loc2, dist2), rest2) = extract_min(rest)
		in
			if dist <= dist2 then ((loc, dist), rest) else ((loc2, dist2), (loc, dist)::rest2)
		end;

fun add_neighbors([], _, frontier) = frontier
	| add_neighbors((n, w)::ns, base_dist, frontier) = add_neighbors(ns, base_dist, (n, base_dist + w)::frontier);

fun shortest_path([], _, _) = 9999
	| shortest_path(frontier, visited, target) =
		let
			val ((loc, dist), frontier2) = extract_min(frontier)
		in
			if loc = target then dist
			else if member(loc, visited) then shortest_path(frontier2, visited, target)
			else
				let
					val ns = neighbors(loc, LOCATION_GRAPH)
					val frontier3 = add_neighbors(ns, dist, frontier2)
				in
					shortest_path(frontier3, loc::visited, target)
				end
		end;

fun location_distance(from_loc, to_loc) =
	if from_loc = to_loc then 0
	else
		case lookup_distance(from_loc, to_loc, LOCATION_DISTANCE_OVERRIDES) of
			SOME d => d
		  | NONE => shortest_path([(from_loc, 0)], [], to_loc);

fun allocate_forklift(target_loc, [fl]) = (fl, [])
  | allocate_forklift(target_loc, ((fl_id1, loc1): Forklift)::((fl_id2, loc2): Forklift)::rest) =
    let
        val d1 = location_distance(loc1, target_loc)
        val d2 = location_distance(loc2, target_loc)
        val (fl, fls2) =
            if d1 <= d2
            then allocate_forklift(target_loc, (fl_id1, loc1)::rest)
            else allocate_forklift(target_loc, (fl_id2, loc2)::rest)
    in
        if d1 <= d2
        then (fl, (fl_id2, loc2)::fls2)
        else (fl, (fl_id1, loc1)::fls2)
    end;

fun forklift_candidate_relations(_, []) = []
  | forklift_candidate_relations(event_id, ((fl_id, _): Forklift)::rest) =
	[event_id, fl_id, "candidate_object"]::forklift_candidate_relations(event_id, rest);

fun allocate_forklift_with_relations(event_id, target_loc, fls: Forklifts) =
let
	val (fl, fls2) = allocate_forklift(target_loc, fls)
	val (fl_id, _) = fl
	val candidates = forklift_candidate_relations(event_id, fls)
	val assigned = [[event_id, fl_id, "assigned_object"]]
	val ref_location = [[event_id, location_oid(target_loc), "location"]]
in
	(fl, fls2, candidates^^assigned^^ref_location)
end;
	

fun enter_pickup(tr_id: Truck) = 
let
	val _ = enter_location(loc_supplier, [tr_id])
in
    ()
end;
(* randomly decide to leave pickup location before pickup event for On-Transition *)
fun leave_pickup_pre(tr_id: Truck) = 
let
	val left = uniform(0.0, 1.0) < 0.5
	val _ =
		if left then
			leave_location(loc_supplier, [tr_id])
		else
			()
in
    left
end;

fun leave_pickup_post(tr_id: Truck, left) = 
let
	val _ =
		if not left then
			leave_location(loc_supplier, [tr_id])
		else
			()
in
    ()
end;


fun enter_loading((cr_id, td_id, veh_id, nof_hus, hus, cr_status, cr_weight, cr_prio, cr_location): Container,
     tr_id: Truck) = 
let
	(* this is the first enter location for the container *)
	val _ = enter_location(loc_supplier_loading_area, [tr_id])
	val cr2 = enter_location_cr((cr_id, td_id, veh_id, nof_hus, hus, cr_status, cr_weight, cr_prio, cr_location), loc_supplier_loading_area)
in
    (cr2)
end;
fun leave_loading((cr_id, td_id, veh_id, nof_hus, hus, cr_status, cr_weight, cr_prio, cr_location): Container,
     tr_id: Truck) = 
let
	val _ = leave_location(loc_supplier_loading_area, [tr_id])
	val cr2 = leave_location_cr((cr_id, td_id, veh_id, nof_hus, hus, cr_status, cr_weight, cr_prio, cr_location))
in
    (cr2)
end;




fun pickup_container_start
    ((cr_id, td_id, veh_id, nof_hus, hus, cr_status, cr_weight, cr_prio, cr_location): Container,
     tr_id: Truck) =
let
	val event_id = "pick_"^cr_id
	val lifecycle_relations = [[event_id, cr_id, "CR picked"], [event_id, tr_id, "TR moved"]]
	val instance_id = mk_instance_id(event_id, "", td_id, cr_id)
in
	write_lifecycle_event(event_id^"_start", "Pick Up Empty Container", LC_START, instance_id, lifecycle_relations)
end;

fun pickup_container_end((cr_id, td_id, veh_id, nof_hus, hus, cr_status, cr_weight, cr_prio, cr_location): Container, tr_id: Truck) =
let
	val cr = (cr_id, td_id, veh_id, nof_hus, hus, cr_status, cr_weight, cr_prio, cr_location)
	val event_id = "pick_"^cr_id 
	val lifecycle_relations = [[event_id, cr_id, "CR picked"], [event_id, tr_id, "TR moved"]]
	val truck_container = [[tr_id, cr_id, "TR loads CR"]]
	val o2o_relations = truck_container
	val instance_id = mk_instance_id(event_id, "", td_id, cr_id)
in
	(
	initialize_containers([cr]);
	write_lifecycle_event(event_id^"_complete", "Pick Up Empty Container", LC_COMPLETE, instance_id, lifecycle_relations);
	write_o2o_relations(o2o_relations);
	()
	)
end;

fun write_collect_goods((hu_id, cr_id): HandlingUnit) =
let
	val hu = (hu_id, cr_id)
	val event_id = "collect_"^hu_id 
	val event_handling_unit = [[event_id, hu_id, "HU collected"]]
	val e2o_relations = event_handling_unit
	val instance_id = mk_instance_id(event_id, "", "", cr_id)
in
	(
	initialize_handling_unit(hu);
	write_lifecycle_event(event_id^"_complete", "Collect Goods", LC_COMPLETE, instance_id, e2o_relations)
	)
end;

(* Load Truck side effect *)
fun loadHU((cr_id, td_id, veh_id, nof_hus, hus, _, cr_weight, cr_prio, cr_location): Container, (hu_id, cr_id2): HandlingUnit) = 
let
	val new_hus = (hu_id, cr_id)::hus
	val cr_status = if List.length new_hus = nof_hus then full else empt
in
	(cr_id, td_id, veh_id, nof_hus, new_hus, cr_status, cr_weight, cr_prio, cr_location)
end;

fun write_load_truck((cr_id, td_id, veh_id, nof_hus, hus, cr_status, cr_weight, cr_prio, cr_location): Container, tr_id: Truck, (hu_id, cr_id2): HandlingUnit) =
let
	val event_id = "load_truck_"^hu_id 
	val event_handling_unit = [[event_id, hu_id, "HU loaded"]]
	val event_container = [[event_id, cr_id, "CR laded"]]
	val event_truck = [[event_id, tr_id, "TR laded"]]
	val container_handling_unit = [[cr_id, hu_id, "CR contains HU"]]
	val e2o_relations = event_handling_unit^^event_container^^event_truck
	val o2o_relations = container_handling_unit
	val instance_id = mk_instance_id(event_id, "", td_id, cr_id)
in
	(
	(* write_lifecycle_event(event_id^"_start", "Load Truck", LC_START, instance_id, e2o_relations); *)
	write_o2o_relations(o2o_relations);
	write_lifecycle_event(event_id^"_complete", "Load Truck", LC_COMPLETE, instance_id, e2o_relations);
	()
	)
end;

fun drive_to_terminal_start(tr_id: Truck, (cr_id, td_id, veh_id, nof_hus, hus, cr_status, cr_weight, cr_prio, cr_location): Container) =
let
	val event_id = "drive_term_"^cr_id 
	val event_container = [[event_id, cr_id, "CR moved"]]
	val event_truck = [[event_id, tr_id, "TR moved"]]
	val e2o_relations = event_container^^event_truck
	val instance_id = mk_instance_id(event_id, "", td_id, cr_id)
in
	(
	write_lifecycle_event(event_id^"_start", "Drive to Terminal", LC_START, instance_id, e2o_relations);
	()
	)
end;

fun drive_to_terminal_end(tr_id: Truck, (cr_id, td_id, veh_id, nof_hus, hus, cr_status, cr_weight, cr_prio, cr_location): Container) =
let
	val event_id = "drive_term_"^cr_id
	val event_container = [[event_id, cr_id, "CR moved"]]
	val event_truck = [[event_id, tr_id, "TR moved"]]
	val e2o_relations = event_container^^event_truck
	val instance_id = mk_instance_id(event_id, "", td_id, cr_id)
in
	write_lifecycle_event(event_id^"_complete", "Drive to Terminal", LC_COMPLETE, instance_id, e2o_relations)
end;

fun send_truck_arrival_notice(tr_id: Truck, (cr_id, td_id, veh_id, nof_hus, hus, cr_status, cr_weight, cr_prio, cr_location): Container, tm) =
let
	val event_id = "arr_notice_" ^ cr_id
	fun opt_rel(obj_id, qualifier) = if obj_id = "" then [] else [[event_id, obj_id, qualifier]]
	val e2o_relations =
		opt_rel(tr_id, "TR arrival notice")^^
		opt_rel(cr_id, "CR arrival notice")
	val instance_id = mk_instance_id(event_id, "", td_id, cr_id)
	val _ = tm
in
	write_event_with_relations(event_id, "Send Truck Arrival Notice", LC_COMPLETE, instance_id, [], e2o_relations)
end;

fun unload_truck_allocate((cr_id, td_id, veh_id, nof_hus, hus, cr_status, cr_weight, cr_prio, cr_location): Container, 
	tr_id: Truck, fls: Forklifts ) =
let
	val container = (cr_id, td_id, veh_id, nof_hus, hus, cr_status, cr_weight, cr_prio, cr_location)
	val event_base_id = "unload_truck_" ^ cr_id
	val alloc_event_id = event_base_id ^ "_assign"
	val (fl, fls2, allocation_relations) = allocate_forklift_with_relations(alloc_event_id, cr_location, fls) (* use container location, not fixed loc_terminal *)
	val e2o_relations = (
		[[alloc_event_id, cr_id, "CR unload allocated"],
		 [alloc_event_id, tr_id, "TR unload allocated"]]^^
		allocation_relations
	)
	val instance_id = mk_instance_id(event_base_id, "", td_id, cr_id)
	val _ = write_lifecycle_event(alloc_event_id, "Unload Truck", LC_ASSIGN, instance_id, e2o_relations)
in
	(fl, fls2)
end;	



fun unload_truck_start((cr_id, td_id, veh_id, nof_hus, hus, cr_status, cr_weight, cr_prio, cr_location): Container, 
	tr_id: Truck, (fl_id, loc): Forklift) =
let
	val event_id = "unload_truck_" ^ cr_id
	val e2o_relations = (
		[[event_id, cr_id, "CR unloaded"],
		 [event_id, tr_id, "TR unloaded"],
		 [event_id, fl_id, "FL unloading"]]
	)
	val instance_id = mk_instance_id(event_id, "", td_id, cr_id)
in
	write_lifecycle_event(event_id ^ "_start", "Unload Truck", LC_START, instance_id, e2o_relations)
end;
fun unload_truck_end((cr_id, td_id, veh_id, nof_hus, hus, cr_status, cr_weight, cr_prio, cr_location): Container, 
	tr_id: Truck, (fl_id, loc): Forklift) =
let
	val event_id = "unload_truck_" ^ cr_id
	val e2o_relations = (
		[[event_id, cr_id, "CR unloaded"],
		 [event_id, tr_id, "TR unloaded"],
		 [event_id, fl_id, "FL unloading"]]
	)
	val instance_id = mk_instance_id(event_id, "", td_id, cr_id)
in
	write_lifecycle_event(event_id ^ "_complete", "Unload Truck", LC_COMPLETE, instance_id, e2o_relations)
end;

(* Weigh side effect *)
fun sample_weight_recursive(0) = 0.0 | sample_weight_recursive(nof_hus) = sample_weight_recursive(nof_hus - 1) + 
let
	val b = uniform(0.0,1.0)
in
	if b < 0.5 then 200.0
	else if b < 0.9 then 180.0
	else 250.0
end;
fun sample_weight(nof_hus) = sample_weight_recursive(nof_hus)


fun weigh_start((fl_id, loc): Forklift, (cr_id, td_id, veh_id, nof_hus, hus, cr_status, cr_weight, cr_prio, cr_location): Container) =
let
	val event_id = "weigh_"^cr_id
	val event_container = [[event_id, cr_id, "CR weighted"]]
	val event_forklift = [[event_id, fl_id, "FL weighing"]]
	val e2o_relations = event_container^^event_forklift
	val instance_id = mk_instance_id(event_id, "", td_id, cr_id)
in
	write_lifecycle_event(event_id^"_start", "Weigh", LC_START, instance_id, e2o_relations)
end;

fun weigh_end((fl_id, loc): Forklift, (cr_id, td_id, veh_id, nof_hus, hus, cr_status, cr_weight, cr_prio, cr_location): Container) =
let
	val event_id = "weigh_"^cr_id
	val weight = sample_weight(nof_hus)
	val event_container = [[event_id, cr_id, "CR weighted"]]
	val event_forklift = [[event_id, fl_id, "FL weighing"]]
	val e2o_relations = event_container^^event_forklift
	val instance_id = mk_instance_id(event_id, "", td_id, cr_id)
	val _ = (
		update_cr_weight(cr_id, weight);
		write_lifecycle_event(event_id^"_complete", "Weigh", LC_COMPLETE, instance_id, e2o_relations)
	)
in
	(* Return updated container object as side effect *)
	(cr_id, td_id, veh_id, nof_hus, hus, cr_status, weight, cr_prio, cr_location)
end;

fun place_in_stock((cr_id, td_id, veh_id, nof_hus, hus, cr_status, cr_weight, cr_prio, cr_location): Container, (fl_id, loc): Forklift) =
let
	val event_id = "place_stock_"^cr_id 
	val event_container = [[event_id, cr_id, "CR stored"]]
	val event_forklift = [[event_id, fl_id, "FL moved"]]
	val e2o_relations = event_container^^event_forklift
	val instance_id = mk_instance_id(event_id, "", td_id, cr_id)
in
	(
	write_lifecycle_event(event_id^"_complete", "Place in Stock", LC_COMPLETE, instance_id, e2o_relations)
	)
end;

fun pick_from_stock_allocate((cr_id, td_id, veh_id, nof_hus, hus, cr_status, cr_weight, cr_prio, cr_location): Container, fls: Forklifts) =
let
	val container = (cr_id, td_id, veh_id, nof_hus, hus, cr_status, cr_weight, cr_prio, cr_location)
	val event_base_id = "pick_stock_" ^ cr_id
	val alloc_event_id = event_base_id ^ "_assign"
	val (fl, fls2, allocation_relations) = allocate_forklift_with_relations(alloc_event_id, loc_storage, fls)
	val e2o_relations = (
		[[alloc_event_id, cr_id, "CR pick allocated"]]^^
		allocation_relations
	)
	val instance_id = mk_instance_id(event_base_id, "", td_id, cr_id)
	val _ = container
	val _ = write_lifecycle_event(alloc_event_id, "Pick from Stock", LC_ASSIGN, instance_id, e2o_relations)
in
	(fl, fls2)
end;

fun pick_from_stock((cr_id, td_id, veh_id, nof_hus, hus, cr_status, cr_weight, cr_prio, cr_location): Container, (fl_id, loc): Forklift) =
let
	val event_id = "pick_stock_"^cr_id 
	val event_container = [[event_id, cr_id, "CR picked"]]
	val event_forklift = [[event_id, fl_id, "FL moved"]]
	val e2o_relations = event_container^^event_forklift
	val instance_id = mk_instance_id(event_id, "", td_id, cr_id)
in
	(
	write_lifecycle_event(event_id^"_complete", "Pick from Stock", LC_COMPLETE, instance_id, e2o_relations)
	)
end;

fun pick_from_bay_allocate((cr_id, td_id, veh_id, nof_hus, hus, cr_status, cr_weight, cr_prio, cr_location): Container, fls: Forklifts) =
let
	val container = (cr_id, td_id, veh_id, nof_hus, hus, cr_status, cr_weight, cr_prio, cr_location)
	val event_base_id = "pick_bay_" ^ cr_id
	val alloc_event_id = event_base_id ^ "_assign"
	val (fl, fls2, allocation_relations) = allocate_forklift_with_relations(alloc_event_id, loc_loading_bay, fls)
	val e2o_relations = (
		[[alloc_event_id, cr_id, "CR pick allocated"]]^^
		allocation_relations
	)
	val instance_id = mk_instance_id(event_base_id, "", td_id, cr_id)
	val _ = container
	val _ = write_lifecycle_event(alloc_event_id, "Pick from Bay", LC_ASSIGN, instance_id, e2o_relations)
in
	(fl, fls2)
end;

fun pick_from_bay((cr_id, td_id, veh_id, nof_hus, hus, cr_status, cr_weight, cr_prio, cr_location): Container, (fl_id, loc): Forklift) =
let
	val event_id = "pick_bay_"^cr_id
	val event_container = [[event_id, cr_id, "CR picked"]]
	val event_forklift = [[event_id, fl_id, "FL moved"]]
	val e2o_relations = event_container^^event_forklift
	val instance_id = mk_instance_id(event_id, "", td_id, cr_id)
	val _ = loc
in
	(
	write_lifecycle_event(event_id^"_complete", "Pick from Bay", LC_COMPLETE, instance_id, e2o_relations)
	)
end;

fun bring_to_bay_start((cr_id, td_id, veh_id, nof_hus, hus, cr_status, cr_weight, cr_prio, cr_location): Container, (fl_id,loc): Forklift) =
let
	val event_id = "to_bay_"^cr_id
	val event_container = [[event_id, cr_id, "CR brought to bay"]]
	val event_forklift = [[event_id, fl_id, "FL moved"]]
	val e2o_relations = event_container^^event_forklift
	val instance_id = mk_instance_id(event_id, "", td_id, cr_id)
in
	write_lifecycle_event(event_id^"_start", "Bring to Loading Bay", LC_START, instance_id, e2o_relations)
end;

fun bring_to_bay_end((cr_id, td_id, veh_id, nof_hus, hus, cr_status, cr_weight, cr_prio, cr_location): Container, (fl_id,loc): Forklift) =
let
	val event_id = "to_bay_"^cr_id 
	val event_container = [[event_id, cr_id, "CR brought to bay"]]
	val event_forklift = [[event_id, fl_id, "FL moved"]]
	val e2o_relations = event_container^^event_forklift
	val instance_id = mk_instance_id(event_id, "", td_id, cr_id)
in
	write_lifecycle_event(event_id^"_complete", "Bring to Loading Bay", LC_COMPLETE, instance_id, e2o_relations)
end;

fun getVehicleById(_, []) = ERROR_VEHICLE() | getVehicleById(veh_id, (oid, idle_cap, scheduled_cr_ids, loaded_crs, clock)::vehs) = 
if veh_id = oid then (veh_id, idle_cap, scheduled_cr_ids, loaded_crs, clock) else getVehicleById(veh_id, vehs);

fun updateVH(new_vh, checked, []) = checked | updateVH(new_vh: Vehicle, checked: Vehicles, old_vh::unchecked: Vehicles) =
if (#1 new_vh = #1 old_vh) then checked^^[new_vh]^^unchecked 
else updateVH(new_vh, checked^^[old_vh], unchecked)

fun load_to_vehicle_start((cr_id, td_id, veh_id, nof_hus, hus, cr_status, cr_weight, cr_prio, cr_location): Container, (fl_id,loc): Forklift) =
let
	val event_id = "load_veh_"^cr_id^"_"^veh_id
	val event_container = [[event_id, cr_id, "CR loaded"]]
	val event_forklift = [[event_id, fl_id, "FL moved"]]
	val event_vehicle = [[event_id, veh_id, "VH loaded"]]
	val e2o_relations = event_container^^event_forklift^^event_vehicle
	val instance_id = mk_instance_id(event_id, "", td_id, cr_id)
in
	write_lifecycle_event(event_id^"_start", "Load to Vehicle", LC_START, instance_id, e2o_relations)
end;

fun load_to_vehicle_end((cr_id, td_id, veh_id, nof_hus, hus, cr_status, cr_weight, cr_prio, cr_location): Container, (fl_id,loc): Forklift, vehs: Vehicles) =
let
	val cr = (cr_id, td_id, veh_id, nof_hus, hus, cr_status, cr_weight, cr_prio, cr_location)
	val (oid, idle_cap, scheduled_cr_ids, loaded_crs, clock) = getVehicleById(veh_id, vehs)
	val missed = (oid = "errorVH")
	val vehs2 = 
		if missed then (* missed: reschedule container *) vehs
		else let
			(* proper loading event *)
			val event_id = "load_veh_"^cr_id^"_"^veh_id 
			val event_container = [[event_id, cr_id, "CR loaded"]]
			val event_forklift = [[event_id, fl_id, "FL moved"]]
			val event_vehicle = [[event_id, veh_id, "VH laded"]]
			val e2o_relations = event_container^^event_forklift^^event_vehicle
			val instance_id = mk_instance_id(event_id, "", td_id, cr_id)
			val _ = (
				update_vehicle_location(veh_id, loc_loading_bay);
				write_lifecycle_event(event_id^"_complete", "Load to Vehicle", LC_COMPLETE, instance_id, e2o_relations)
			)
			val new_vh = (veh_id, idle_cap, scheduled_cr_ids, cr::loaded_crs, clock)
			val vehs2 = updateVH(new_vh, [], vehs)
		in
			vehs2
		end
in
	(missed, vehs2)
end;

fun reschedule_container(cr: Container, vehs: Vehicles) =
let
	val (high_prio_crs, _, vehs) = assign_vehicle_prio_high([cr], vehs, [])
	val reassigned_cr = List.nth(high_prio_crs, 0)
	val (cr_id, td_id, veh_id, nof_hus, hus, cr_status, cr_weight, cr_prio, cr_location) = reassigned_cr
	val event_id = "resch_cr_"^cr_id
	val event_container = [[event_id, cr_id, "CR rescheduled"]]
	val event_vehicle = [[event_id, veh_id, "booked VH"]]
	val event_td = [[event_id, td_id, "TD with CR rescheduled"]]
	val td_vehicle = [[td_id, veh_id , "Ersatz VH for TD"]]
	val e2o_relations = event_container^^event_vehicle^^event_td
	val o2o_relations = td_vehicle
	val instance_id = mk_instance_id(event_id, "", td_id, cr_id)
	val _ = (
		write_event(event_id, "Reschedule Container", LC_COMPLETE, instance_id, []);
		write_e2o_relations(e2o_relations);
		write_o2o_relations(o2o_relations);
		write_lifecycle_event(event_id^"_assign", "Load to Vehicle", LC_ASSIGN, instance_id, e2o_relations)
	)
in
	(reassigned_cr, vehs)
end;

fun write_depart((veh_id, idle_cap, scheduled_cr_ids, loaded_crs, clock): Vehicle) =
let
	val event_id = "depart_"^veh_id 
	val event_vehicle = [[event_id, veh_id, "VH departed"]]
	val cr_ids = container_ids(loaded_crs)
	val td_ids = transport_document_ids(loaded_crs)
	val event_containers = relationship_cartesian([event_id], cr_ids, "CR departed")
	val event_tds = relationship_cartesian([event_id], td_ids, "TD with CR departure")
	val e2o_relations = event_vehicle^^event_containers^^event_tds
	val instance_id = mk_instance_id_vh(event_id, veh_id)
in
	if List.length cr_ids > 0 then (
	update_vehicle_location(veh_id, loc_outbound_route);
	write_lifecycle_event(event_id^"_complete", "Depart", LC_COMPLETE, instance_id, e2o_relations)
	)
	else ()
end;
