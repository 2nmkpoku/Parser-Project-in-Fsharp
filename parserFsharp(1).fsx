
(* Parser Sample *)
//Name: Nnamdi Mkpoku, Joyce Olaniyi

(* NOTE: This example uses generic OCaml compatable syntax where the syntax where possible, which is
 * in most places!  There *are* LIBRARY differences between F# and OCaml that necessitate slighly
 * different functions to be used for input and output.
 *
 * If using OCaml instead of F#, check out: https://learnxinyminutes.com/docs/ocaml/
*)

//if it appears on the left it not a token


// A sample grammar
//
// sentence	: np vp np sentence_tail 
// sentence_tail	: conj sentence | eos 
// np       	: art adj_list noun pp  	 
// adj_list 	: adj adj_tail | ε  
// adj_tail 	: comma adj_list | ε     // comma as in “,” 
// pp       	: prep np | ε  
// vp       	: adv verb | verb 

// Tokens
type Token =
    | Read of string
    | Write of string
    | Close_p 
    | Open_p
    | ID_stmt 
    | Add_op
    | Mult_op
    | Oper_op of string
    | For_Stmt 
    | To_Stmt 
    | Do_Stmt 
    | Step_Stmt 
    | Done_Stmt 
    | Then_stmt 
    | While_stmt 
    | Endwhile_stmt 
    | If_Stmt 
    | Endif_stmt 
    | Else_stmt 
    | Asgn_stmt
    | ID of string

    with static member tokenFromLexeme str = // Function to get a token from a lexeme (String)
        
            match str with
            
            | "read" -> Read str
            | "write" -> Write str
            | "(" -> Open_p 
            | ")" -> Close_p
            | "+"  | "-" ->  Add_op
            | "*" | "/" -> Mult_op 
            | ">" | "<" | "=" -> Oper_op str
            | "for" -> For_Stmt 
            | "to" -> To_Stmt 
            | "do" -> Do_Stmt 
            | "step" -> Step_Stmt 
            | "done" -> Done_Stmt 
            | "then" -> Then_stmt 
            | "while" -> While_stmt 
            | "od" -> Endwhile_stmt 
            | "if" -> If_Stmt 
            | "fi"-> Endif_stmt 
            | "else" -> Else_stmt 
            | ":=" -> Asgn_stmt 
            | x -> ID x 



////////////////////////////////////////////////////////////////////////////////////////////////////
//
// The following is but one of many possible structures. In fact F#/Ocaml has many
// features that make parsing complex grammars pretty easy... but... to understand those requires a
// much deeper understanding of the language than we have explored.  Unfortunately, the result is
// that the code will not be nearly as concise or elegant as it could otherwise be. However, if you
// which to explore the additional features of the language, feel free to explore!!!
//
//////////////////////////////////////////////////////////////////////////////////////////////////////

// NOTE: A Better code approach MIGHT BE to use "Active Patterns", but those are a little more
// difficult to understand, especially while still trying to grasp "static patterns".
// https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/active-patterns
// https://fsharpforfunandprofit.com/posts/convenience-active-patterns/



// NOTES:
// The |> operator sends (pipes) the output of one function directly to the next one in line.
// "and" just allows multiple, mutually recursive functions to be defined under a single "let"

let isStmtFirst = function
   | Read x | Write x | ID x -> true
   | _ -> false


let rec parse theList = program theList

and program x =  x |> stmt_list

//stmt_list ⟶ stmt stmt_list | ε
and stmt_list lst =
    if not(List.isEmpty lst) && isStmtFirst (List.head lst) then
        lst |> stmt |> stmt_list
    else 
        lst

        

//stmt ⟶ id := expr | read id | write expr | for_stmt | if_stmt
and stmt lst = 
    match lst with
    | ID x :: Asgn_stmt :: xs -> xs |> expr
    | Read _ :: ID _ :: xs -> xs
    | Write x :: xs -> xs |> expr 
    | For_Stmt :: xs -> xs |> for_stmt 
    | If_Stmt :: xs-> xs |> if_stmt 
    | ID x :: Add_op :: xs -> xs |> add_op
    | ID x :: Mult_op ::  xs -> xs  |> multi_op
    | Open_p :: xs -> xs |> factor
    | ID _ :: xs -> xs |>  cond  
    | _  ->  failwithf $"not a statment %A{lst}"
    | x :: xs -> failwithf $"not a statment %A{xs}"
   // | [] -> failwith "Unexpected end of input while processing For statement."


// //expr ⟶ term term_tail
and expr = function
    | x -> x |> term |> term_tail 
   // | x :: xs ->  failwithf $"Expected Expr, but found: %A{xs}"
   // | [] -> failwith "Unexpected end of input while processing Expression statement."
    

//term ⟶ factor factor_tail
and term = function
    |x -> x |> factor |> factor_tail
    
    
// term_tail ⟶ add_op term term_tail | ε
and term_tail = function
    |Add_op :: xs ->xs |> term |> term_tail
    //|xs -> xs
    | _->  failwith "Not an addition statement"




and asgn_Stmt = function
    | Asgn_stmt :: xs -> xs
    | _ -> failwith "asgn"



(*and expr1 = function
    | x :: xs -> xs
    | _ -> failwith "expr1"
*)


//factor_tail ⟶ mult_op factor factor_tail | ε
and factor_tail = function
    |Mult_op  :: xs -> xs |> factor |> factor_tail
    | xs -> xs

//factor ⟶ ( expr ) | id
and factor = function
    |Open_p _ :: expr :: Close_p _ :: xs -> xs
    | ID _ :: xs -> xs
    | x :: xs ->  failwithf $"Expected Expr, but found: %A{xs}"
    | [] -> failwith "Unexpected end of input while processing Factor statement."

//add_op ⟶ - | +
and add_op = function
    | Add_op :: xs -> xs
   // | Subtraction_op :: xs -> xs
    |  x :: xs ->  failwithf $"Expected addition or subtraction operator , but found: %A{xs}"
    | [] -> failwith "Unexpected end of input while processing Add_op statement."

//mult_op ⟶ * | /
and multi_op = function
    | Mult_op :: xs -> xs
   // | Division_op :: xs -> xs
    | x :: xs -> failwithf $"Not a multiplication or division operation, but found: %A{xs}"
    | [] -> failwith "Unexpected end of input while processing Mult_op statement."

//oper ⟶ > | < | =
and oper = function
    | Oper_op _ :: xs -> xs
    | x :: xs ->  failwithf $"Expected comparison, but found: %A{xs}"
    | [] -> failwith "Unexpected end of input while processing Comparison operator."

//cond ⟶ expr oper expr
and cond lst = lst |> expr |> oper |> expr 
    //| x :: xs ->  failwithf $"Expected comparison, but found: %A{xs}"
  //  | [] -> failwith "Unexpected end of input while processing Comparison operator."

 

(*
    //expr ⟶ term term_tail
and expr = function
    | x -> x |> term |> term_tail 
    
    *)
//for_stmt ⟶ for id = id to id step_stmt do stmt_list done
and for_stmt = function
    | For_Stmt :: ID x:: Oper_op "=":: ID y :: To_Stmt :: ID z :: Step_Stmt :: ID a :: Do_Stmt :: xs-> xs |> stmt_list |> check_for_done
    | For_Stmt :: ID x:: Oper_op "=" :: ID y :: To_Stmt :: ID z  :: Do_Stmt :: xs-> xs |> stmt_list |> check_for_done
    //| xs -> 
    | x:: xs ->  failwithf $"Expected For statement, but found: %A{xs}"
    | [] -> failwith "Unexpected end of input while processing For statement."



and check_for_done = function
    |Done_Stmt :: xs -> xs
    | _ -> failwith "Not a Done statement "

//step_stmt ⟶ step id | ε
and step_stmt = function
    | Step_Stmt :: ID x :: xs -> xs  
    | x :: xs -> failwithf $"Expected Step statement, but found: %A{ xs}"
    | [] -> failwith "Unexpected end of input while processing Step statement."


//if_stmt ⟶ if cond then stmt_list else_stmt
and if_stmt = function
    | If_Stmt :: xs -> xs  |> cond |> if_stmt_tail
    | x :: xs -> failwithf $"Expected If statement, but found: %A{ xs}"
    | [] -> failwith "Unexpected end of input while processing If statement."

    
and if_stmt_tail = function 
    | Then_stmt :: xs -> xs |> stmt_list |> else_stmt 
    | _ -> failwith "Not an If statement "


//else_stmt ⟶ else stmt_list fi | fi
and else_stmt = function
    | Else_stmt :: xs -> xs |> stmt_list |> else_stmt_tail
    |  x :: xs ->  failwithf $"Expected an Else, but found: %A{xs}"
    | [] -> failwith "Unexpected end of input while processing Else statement."
    
and else_stmt_tail = function
    | Endif_stmt :: xs -> xs
    | _ -> failwith "Not an Else statement "

   


(*
and sentenceTail = function
    | Conj :: xs -> sentence xs
    | [EOS] -> printfn "Parse Successful"; []
    | EOS :: xs ->  failwith "End of sentence marker found, but not at end!"
    | x :: xs -> failwithf $"Expected EOS but found: %A{x}"
    | [] -> failwith "Unexpected end of input while processing EOS"


// np ::= art adjLst noun pp
and np = function
    | Art theArticle :: xs -> xs |> adjList |> noun |> pp
    | x :: xs -> failwithf $"Expected article, but found: %A{x}"
    | [] -> failwith "article should not be empty"


// adjLst ::= adj adj_tail | <empty>
and adjList = function
    | Adj x :: xs -> xs |> adjTail
               // <empty> means the rule is empty (not the list), if there is no adjective, then…
    | xs -> xs // just resolve to what was passed (instead of failing)


// adjTail ::= comma adjLisy | <empty>
and adjTail = function
    | Comma :: xs -> xs |> adjList
    | xs -> xs // just resolve to what was passed (instead of failing)







// Process the noun (which should be at the head of the list once this method is reached.
// Note: This function could be (and probably should be) defined as a “Lambda”, and included
// in-line in the nounPhrase function above, but it’s just separated out here for clarity.
and noun = function
    | Noun n :: xs -> xs // It's just a noun
    | x -> failwithf $"Expected Noun, but found: %A{x}"
*)

(* Begin Parsing Process *)
let startParsing (str:string) =
    // Split the String (which creates an Array) -> convert the Array to a List -> MAP the list of strings into a list of Tokens.
    // (Note, though arrays are a lot like lists, lists are a bit easier to use for the pattern matching.)
    let tokenList =
        str.Split ' ' |>
        Array.toList |>
        List.map Token.tokenFromLexeme

    // Display our list of tokens... just for fun.
    printfn $"The initial String: %s{str}"
    printfn $"The initial List: %A{tokenList}\n"

    // Work the magic...
    try
        let parsedList = program tokenList
        in printfn $"The Final List:\n\t%A{parsedList}"
    with
        Failure msg -> printfn $"Error: %s{msg}";  System.Console.ReadLine () |> ignore




(* Get the user input and start parsing *)
// NOTE: To make the let assihnment be a function that accepts no parameters,
// an "empty tuple" must be accepted.
let promptAndGo () =
    (* TEST DATA *)
    // let userInput = "the fast , fast dog chases the fast cat ."
    // let userInput = "the noun chases the adj cat and the adj , adj , fast dog adv chases the cat prep a adj noun ."

    let userInput =
        printf "Enter String: ";
        // A case where it's easier to use the .NET ReadLine as opposed to the more restrictive OCaml native variant.
        System.Console.ReadLine ()

    in startParsing userInput


// RUN INTERACTIVELY AS A SCRIPT
promptAndGo ()

// Uncomment the following to pause at the end if it's running in a terminal which dissapears upon running.
// System.Console.ReadKey(true) |> ignore
