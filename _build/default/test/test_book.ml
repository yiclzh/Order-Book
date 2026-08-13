open Order_book_lib
open Order

(* Basic unit tests first *)
let test_simple_match () =
  let book = Book.create () in
  let o1 = make_order ~side:Buy ~price:100.0 ~quantity:10 in
  let o2 = make_order ~side:Sell ~price:99.0 ~quantity:6 in
  let _ = Book.submit_order book o1 in
  let trades = Book.submit_order book o2 in
  assert (List.length trades = 1);
  assert ((List.hd trades).quantity = 6);
  assert (Book.best_bid book = Some 100.0);
  assert (Book.best_ask book = None);
  print_endline "test_simple_match passed"

let test_no_cross_no_trade () =
  let book = Book.create () in
  let o1 = make_order ~side:Buy ~price:99.0 ~quantity:10 in
  let o2 = make_order ~side:Sell ~price:100.0 ~quantity:10 in
  let _ = Book.submit_order book o1 in
  let trades = Book.submit_order book o2 in
  assert (List.length trades = 0);
  assert (Book.best_bid book = Some 99.0);
  assert (Book.best_ask book = Some 100.0);
  print_endline "test_no_cross_no_trade passed"

let test_cancel () =
  let book = Book.create () in
  let o1 = make_order ~side:Buy ~price:100.0 ~quantity:10 in
  let _ = Book.submit_order book o1 in
  let found = Book.cancel_order book ~side:Buy ~price:100.0 ~id:o1.id in
  assert found;
  assert (Book.best_bid book = None);
  print_endline "test_cancel passed"

(* Property-based test: quantity is always conserved.
   Total quantity submitted = total quantity traded (counted once per trade,
   since a trade removes qty from both sides equally) + total quantity resting. *)
let total_resting_quantity book =
  let sum_side map =
    Book.PriceMap.fold
      (fun _ level acc -> acc + Price_level.total_quantity level)
      map 0
  in
  sum_side book.Book.bids + sum_side book.Book.asks

let order_printer o =
  Printf.sprintf "{side=%s; price=%.1f; quantity=%d}"
    (match o.side with Buy -> "Buy" | Sell -> "Sell")
    o.price o.quantity

let orders_printer orders =
  "[" ^ String.concat "; " (List.map order_printer orders) ^ "]"

let prop_quantity_conserved =
  QCheck.Test.make ~count:200 ~name:"quantity conserved across random order flow"
    QCheck.(
      make
        ~print:orders_printer
        (Gen.list_size (Gen.int_range 1 20)
           (Gen.map
              (fun (is_buy, price, qty) ->
                make_order
                  ~side:(if is_buy then Buy else Sell)
                  ~price:(float_of_int price)
                  ~quantity:qty)
              (Gen.triple Gen.bool (Gen.int_range 90 110) (Gen.int_range 1 20)))))
    (fun orders ->
      let book = Book.create () in
      let total_submitted = List.fold_left (fun acc o -> acc + o.quantity) 0 orders in
      let total_traded =
        List.fold_left
          (fun acc o ->
            let trades = Book.submit_order book o in
            acc + List.fold_left (fun a (t : Book.trade) -> a + t.quantity) 0 trades)
          0 orders
      in
      let resting = total_resting_quantity book in
      total_submitted = resting + (2 * total_traded))

let () =
  test_simple_match ();
  test_no_cross_no_trade ();
  test_cancel ();
  let _ = QCheck_runner.run_tests ~verbose:true [ prop_quantity_conserved ] in
  ()