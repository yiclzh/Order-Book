open Order

let random_order ~price_range =
  let is_buy = Random.bool () in
  let base = 100 in
  let offset = Random.int (2 * price_range) - price_range in
  let price = float_of_int (base + offset) in
  let quantity = 1 + Random.int 20 in
  make_order
    ~side:(if is_buy then Buy else Sell)
    ~price
    ~quantity

let run_simulation ~num_orders ~price_range =
  let book = Book.create () in
  let start_time = Unix.gettimeofday () in
  let total_trades = ref 0 in
  for _ = 1 to num_orders do
    let order = random_order ~price_range in
    let trades = Book.submit_order book order in
    total_trades := !total_trades + List.length trades
  done;
  let elapsed = Unix.gettimeofday () -. start_time in
  (book, !total_trades, elapsed)