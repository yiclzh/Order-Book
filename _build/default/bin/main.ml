open Order_book_lib

let () =
  Random.self_init ();
  let num_orders = 100_000 in
  let book, total_trades, elapsed = Simulator.run_simulation ~num_orders:1_000_000 ~price_range:500 in
  Printf.printf "Processed %d orders in %.4f seconds\n" num_orders elapsed;
  Printf.printf "Throughput: %.0f orders/sec\n" (float_of_int num_orders /. elapsed);
  Printf.printf "Total trades executed: %d\n" total_trades;
  (match Book.best_bid book with
   | Some p -> Printf.printf "Final best bid: %.2f\n" p
   | None -> Printf.printf "No bids remaining\n");
  (match Book.best_ask book with
   | Some p -> Printf.printf "Final best ask: %.2f\n" p
   | None -> Printf.printf "No asks remaining\n")