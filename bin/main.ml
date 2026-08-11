open Order_book_lib

let () = 
  let o1 = Order.make_order ~side:Buy ~price:100.5 ~quantity:10 in
  let o2 = Order.make_order ~side:Sell ~price:101.0 ~quantity:5 in
  Printf.printf "Order 1: id=%d price=%f qty=%d\n" o1.id o1.price o1.quantity;
  Printf.printf "Order 2: id=%d price=%f qty=%d\n" o2.id o2.price o2.quantity