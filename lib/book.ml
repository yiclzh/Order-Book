open Order

(* Bids sorted so highest price = best; we store price as-is and always
   read max_binding for best bid. Asks: min_binding = best ask. *)
module PriceMap = Map.Make (Float)

type t = {
  mutable bids : Price_level.t PriceMap.t;
  mutable asks : Price_level.t PriceMap.t;
}

type trade = {
  buy_order_id : int;
  sell_order_id : int;
  price : float;
  quantity : int;
}

let create () = { bids = PriceMap.empty; asks = PriceMap.empty }

let get_or_create_level price map =
  match PriceMap.find_opt price map with
  | Some level -> level
  | None -> Price_level.create price

let best_bid book =
  match PriceMap.max_binding_opt book.bids with
  | Some (price, _) -> Some price
  | None -> None

let best_ask book =
  match PriceMap.min_binding_opt book.asks with
  | Some (price, _) -> Some price
  | None -> None

(* Insert a resting order into the correct side/price level *)
let rest_order book order =
  match order.side with
  | Buy ->
    let level = get_or_create_level order.price book.bids in
    Price_level.add_order level order;
    book.bids <- PriceMap.add order.price level book.bids
  | Sell ->
    let level = get_or_create_level order.price book.asks in
    Price_level.add_order level order;
    book.asks <- PriceMap.add order.price level book.asks

(* Does this incoming order cross the book (i.e. can it match immediately)? *)
let crosses book order =
  match order.side with
  | Buy -> (
    match best_ask book with
    | Some ask_price -> order.price >= ask_price
    | None -> false)
  | Sell -> (
    match best_bid book with
    | Some bid_price -> order.price <= bid_price
    | None -> false)

(* Match an incoming order against the resting book, filling as much as
   possible at resting prices, returning (trades, remaining_quantity). *)
let rec match_order book order remaining_qty trades =
  if remaining_qty <= 0 then (List.rev trades, 0)
  else if not (crosses book { order with quantity = remaining_qty }) then
    (List.rev trades, remaining_qty)
  else
    let opposite_map, opposite_price =
      match order.side with
      | Buy -> (book.asks, Option.get (best_ask book))
      | Sell -> (book.bids, Option.get (best_bid book))
    in
    let level = PriceMap.find opposite_price opposite_map in
    match Price_level.peek_front level with
    | None -> (List.rev trades, remaining_qty)
    | Some resting_order ->
      let fill_qty = min remaining_qty resting_order.quantity in
      let trade =
        match order.side with
        | Buy ->
          { buy_order_id = order.id;
            sell_order_id = resting_order.id;
            price = opposite_price;
            quantity = fill_qty }
        | Sell ->
          { buy_order_id = resting_order.id;
            sell_order_id = order.id;
            price = opposite_price;
            quantity = fill_qty }
      in
      let _ = Price_level.pop_front level in
      let leftover_resting_qty = resting_order.quantity - fill_qty in
      if leftover_resting_qty > 0 then
        Price_level.add_order level { resting_order with quantity = leftover_resting_qty };
      (* clean up empty levels so best_bid/best_ask stay accurate *)
      (match order.side with
       | Buy -> if Price_level.is_empty level then book.asks <- PriceMap.remove opposite_price book.asks
       | Sell -> if Price_level.is_empty level then book.bids <- PriceMap.remove opposite_price book.bids);
      match_order book order (remaining_qty - fill_qty) (trade :: trades)

(* Public entry point: submit a new order. Matches what it can, rests the remainder. *)
let submit_order book order =
  let trades, remaining_qty = match_order book order order.quantity [] in
  if remaining_qty > 0 then
    rest_order book { order with quantity = remaining_qty };
  trades

let cancel_order book ~side ~price ~id =
  let map = match side with Buy -> book.bids | Sell -> book.asks in
  match PriceMap.find_opt price map with
  | None -> false
  | Some level ->
    let found = Price_level.cancel_order level id in
    if found then begin
      if Price_level.is_empty level then
        (match side with
         | Buy -> book.bids <- PriceMap.remove price book.bids
         | Sell -> book.asks <- PriceMap.remove price book.asks)
      else
        (match side with
         | Buy -> book.bids <- PriceMap.add price level book.bids
         | Sell -> book.asks <- PriceMap.add price level book.asks)
    end;
    found