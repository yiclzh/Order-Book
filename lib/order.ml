type side = Buy | Sell

type order = {
  id : int;
  side : side;
  price : float;
  quantity : int;
  timestamp : float;
}

let next_order_id = ref 1

let fresh_id () =
  let id = !next_order_id in
  incr next_order_id;
  id

let is_buy order =
  match order.side with
  | Buy -> true
  | Sell -> false

let notional_value order =
  order.price *. float_of_int order.quantity

let make_order ~side ~price ~quantity =
  {
    id = fresh_id ();
    side;
    price;
    quantity;
    timestamp = Unix.gettimeofday ();
  }