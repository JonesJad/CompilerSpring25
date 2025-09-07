(** This file should be used to write your tests of your other code. *)

open Batteries;;
open HatchCompiler.FileUtils;;
open OUnit2;;
open TestUtils;;

open HatchCompiler.Compiler;;
open HatchLanguage.Asts;;
open HatchCompiler.Environment;;



(*let get_params = 
  
  "test for the get_parameters function" >:: fun _ ->
  assert_equal  ~printer:(fun x -> x) ("Next free offset: -8\nx stored at [rbp+16]\ny stored at [rbp+24]\n") (string_of_environment (get_params  (["x"; "y"])))
  
;;*)

let all_tests =
  [
    
    test_success "test_code/4.bird" "4";
    test_success "test_code/after_4.bird" "5";
    test_success "test_code/before_4.bird" "3";
    test_success "test_code/before_after_4.bird" "4";
    test_success "test_code/moreafter.bird" "8";
    test_success "test_code/let1.bird" "2";
    test_success "test_code/plus1.bird" "2";
    test_success "test_code/plus2.bird" "6";
    test_success "test_code/plus3.bird" "6";
    test_success "test_code/minus1.bird" "3";
    test_success "test_code/minus2.bird" "-2";
    test_success "test_code/minus3.bird" "2";
    test_success "test_code/times1.bird" "8";
    test_success "test_code/times2.bird" "15";
    test_success "test_code/arithmetic.bird" "-9";
    test_success "test_code/let2.bird" "10";
    test_success "test_code/true.bird" "true";
    test_success "test_code/isboolfalse.bird" "true";
    test_success "test_code/isbooltrue.bird" "true";
    test_success "test_code/isbool2.bird" "false";
    test_success "test_code/isintfalse.bird" "false";
    test_success "test_code/isinttrue.bird" "false";
    test_success "test_code/isint3.bird" "true";
    test_success "test_code/trueorfalse.bird" "true";
    test_success "test_code/trueortrue.bird" "true";
    test_success "test_code/falseorfalse.bird" "false";
    test_success "test_code/falseandtrue.bird" "false";
    test_success "test_code/trueandtrue.bird" "true";
    test_success "test_code/falseandfalse.bird" "false";
    test_success "test_code/1equal1.bird" "true";
    test_success "test_code/1equal2.bird" "false";
    test_success "test_code/1greater2.bird" "false";
    test_success "test_code/2greater1.bird" "true";
    test_success "test_code/1less2.bird" "true";
    test_success "test_code/2less1.bird" "false";
    test_success "test_code/if1.bird" "true";
    test_success "test_code/if2.bird" "true";
    test_success "test_code/1times1.bird" "1";
    test_success "test_code/timesbig.bird" "4000000000000000000";
    test_success "test_code/print1.bird" "4\nfalse\nfalse";
    test_success "test_code/print2.bird" "false\nfalse";
    test_success "test_code/print3.bird" "4\n4";
    test_success "test_code/4let.bird" "1";
    test_success "test_code/printtrue.bird" "true\ntrue";
    test_success "test_code/print5.bird" "3\n2\n1";
    test_runtime_failure "test_code/aftererror.bird" 1;
    test_runtime_failure "test_code/falseplus.bird" 1;
    test_runtime_failure "test_code/equalfalse.bird" 1;
    test_runtime_failure "test_code/minusfalse.bird" 1;
    test_runtime_failure "test_code/and_error.bird" 2;
    test_runtime_failure "test_code/or_error.bird" 2;
    test_runtime_failure "test_code/error1.bird" 1;
    test_runtime_failure "test_code/and_error2.bird" 2;
    test_runtime_failure "test_code/or_error2.bird" 1;
    test_runtime_failure "test_code/ifint.bird" 2;
    (*test_success "test_code/istuple1.bird" "true";
    test_success "test_code/func_double.bird" "8";
    test_success "test_code/func_rec.bird" "6";
    test_success "test_code/func.bird" "5";
    test_success "test_code/func_mult_param.bird" "6";
    test_success "test_code/func_sum_rec.bird" "15";
    test_success "test_code/func_prints.bird" "1\n3\n4\n"; (*Cant figure out calling syntax for this*)
    test_success "test_code/tuple1.bird" "(1, 2)\n(1, 2)";
    test_success "test_code/tuple2.bird" "(3, 5)\n(3, 5)";
    test_success "test_code/tuple_index_1.bird" "2";
    test_success "test_code/tuple_index2.bird" "6";
    test_success "test_code/tuple_index3.bird" "5";
    test_success "test_code/tuple_nested.bird" "(1, (2, 3), 4)\n(1, (2, 3), 4)";
    test_success "test_code/func_sub.bird" "2"; (*tests that the parameters are being put in the correct order*)
    (*ALL FUNCTIONS MUST HAVE AT LEAST ONE PARAMETER*)
    (*test_success "test_code/func_labels.bird" "1"; *)(*tests for unique function labels*)
    test_success "test_code/func_morerec.bird" "0";
    test_success "test_code/declare_no_use.bird" "5";
    test_success "test_code/istuple_closure.bird" "false";
    (*test_success "test_code/closure1.bird" "<closure@000000000XXXXXXX>[0/2](?, ?)";*) (*we CAN print these*)
    
    test_compile_failure "test_code/func_bad.bird" "Tried to use an undefined variable g";
    (*test_compile_failure "test_code/func_bad2.bird" "Tried to call function f with an incorrect number of arguments";*)
    test_compile_failure "test_code/func_bad3.bird" "Tried to call a function f with duplicate parameter name x"; (*not working*)
    (*test_compile_failure "test_code/func_bad4.bird" "Tried to create duplicate definition of function f"; (*not work*)*)
    test_compile_failure "test_code/func_bad5.bird" "Tried to use an undefined variable y";
    get_params
*)
  ];;

let suite = "compiler test suite" >::: all_tests;;

run_test_tt_main suite;;