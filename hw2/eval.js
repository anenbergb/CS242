var acorn  = require("acorn");
var util   = require("util");
var assert = require("assert");

// If you're like me, you'll keep typing eval() when you mean
// evalExpression().  This override should help you out.
eval = function() { throw "eval(): You probably did not want to call this function"; }

// In this lab, you will complete the implementation of a META-CIRCULAR
// EVALUATOR for a subset of JavaScript; that is, you will write an
// interpreter for JavaScript in JavaScript.  One characteristic of
// a metacircular evaluator is that you can largely borrow
// implementation of primitives and control flow structures from
// the host language. (For example, you will not need to record
// control links in the environment: the stack of the *interpreter*
// will track that for you!)
//
// The most convenient way to run this lab is using node.js
// <http://nodejs.org>.  If you work on the corn dialup (ssh
// sunetid@corn.stanford.edu), this software will be installed for you.
//
// You can run this file by running the following command:
//
//      nodejs eval.js
//
// Initially, you should see a bunch of undefineds and failing tests.

// ----------------------------------------------------------------- //

//                          Tests

// ----------------------------------------------------------------- //

function closure_tests() {

// Before we look at the interpreter in earnest, we have to write for
// tests for it.  Tests come in the following form:

test("test262/S10.2.2_A1_T1",
     "var x = 0;" +
     "var f1 = function __impl_f1() {" +
     "  var x = 1;" +
     "  var f2 = function __impl_f2(){" +
     "      __check_scope(__env);" +
     "      return x;" +
     "  };" +
     "  return f2()" +
     "};" +
     "return f1() === 1",
     // We create an anonymous function and immediately evaluate
     // it in order to create a new lexical scope so we can define
     // some temporary variables.  You might find this style convenient.
     (function() {
        var s0 = {};
        s0.x  = 0;
        s0.f1 = {code:"__impl_f1", environment:s0};
        var s1 = { __parent: s0 };
        s1.x = 1;
        s1.f2 = {code:"__impl_f2", environment:s1};
        var s2 = { __parent: s1 }
        return s2;
     })());

// The first argument to 'test' is just a name for the test (it's not
// actually a file.)  The second argument is the source code that we are
// testing.  It is a JavaScript program which returns a boolean: the
// test passes if this boolean is true; furthermore, the second argument
// says what we expect the environment to look like when __check_scope
// is called.  The test program has a few pecularities, related to the
// subset of JavaScript we are supporting:
//
//      - Named function declarations are not supported, so instead
//        of "function f1() { ... }" we write a function expression
//        "var f1 = function () { ... }"
//
//      - Every function expression is *named*, e.g.
//        "var f1 = function __impl_f1() { ... }".  Unlike in ordinary
//        JavaScript, these names must be unique, so they can
//        identify a "code pointer" for the function.
//
//      - __check_scope(__env) is a built-in to indicate at what point
//        during execution we are querying about the environment.
//        In our tests, __check_scope is called only once.
//
// The format of an environment is of a number of objects (each
// representing environment frames) lexically linked together with the
// property __parent.  Each object maps string identifiers to values.
// Literal values are represented literally, while closures are
// represented as an object with a string code pointer (code) and the
// associated environment (environment).
//
// One note about named function expressions: in ECMAScript the name is
// technically supposed to be visible in the body of the function.  We
// do not support this for named function expressions; the name is
// simply a convenient way to identify a "code pointer" for a function.
//
// Complete the following test cases:

test("test262/S10.2.2_A1_T2",
     "var x = 0;" +
     "var f1 = function __impl_f1() {" +
     "  var f2 = function __impl_f2(){" +
     "      __check_scope(__env);" +
     "      return x;" +
     "  };" +
     "  return f2()" +
     "};" +
     "return f1() === 0",
     (function() {
// BEGIN test1 (DO NOT DELETE THIS LINE)
        var s0 = {};
        s0.x  = 0;
        s0.f1 = {code:"__impl_f1", environment:s0};
        var s1 = { __parent: s0};
        s1.f2 = {code:"__impl_f2",environment:s1};
        var s2 = { __parent: s1};
        return s2;
// END test1 (DO NOT DELETE THIS LINE)
     })());

test("test262/S10.2.2_A1_T4",
     "var x = 0;" +
     "var f1 = function __impl_f1() {" +
     "  var x = 1;" +
     "  var f2 = function __impl_f2(){" +
     "      __check_scope(__env);" +
     "      return x;" +
     "  };" +
     "  return f2()" +
     "};" +
     "return f1() === 1",
     (function() {
// BEGIN test2 (DO NOT DELETE THIS LINE)
        var s0 = {};
        s0.x  = 0;
        s0.f1 = {code:"__impl_f1", environment:s0};
        var s1 = { __parent: s0};
        s1.x = 1;
        s1.f2 = {code:"__impl_f2", environment:s1};
        var s2 = { __parent: s1};
        return s2;

// END test2 (DO NOT DELETE THIS LINE)
     })());

// JavaScript tip: {foo: undefined} is different from {},
// as "foo" in {foo:undefined} is true, while "foo" in {} is false.
//    ------------------------                -----------
test("lazy", // "Hey, this look familiar!"
     "var delay = function __impl_delay(f) {" +
     "  var delayed = true;" +
     "  var saved;" +
     "  return function __impl_thunk() {" +
     "      if (delayed) {" +
     "          saved = f();" +
     "          delayed = false;" +
     "      }" +
     "      return saved" +
     "  }" +
     "};" +
     "var i = 0;" +
     "var t = delay(function __impl_inc() {" +
     "                  i = i + 1;" +
     "                  __check_scope(__env);" +
     "                  return i });" +
     "t();" +
     "return (t() === 1 && i === 1)",
     (function() {
// BEGIN test3 (DO NOT DELETE THIS LINE)
        var s0 = {};
        s0.delay = {code:"__impl_delay", environment:s0};
        s0.i = 1;
        var s1 = { __parent: s0};
        s1.delayed = true;
        s1.saved = undefined;
        s1.f = {code:"__impl_inc", environment:s0};
        s0.t = {code:"__impl_thunk", environment:s1};
        var s2 = {__parent: s0};
        
        return s2; //returns scope.

// END test3 (DO NOT DELETE THIS LINE)
     })());

// Here are some extra tests to help you out.

test("test262/S10.2.2_A1_T3",
     "var x = 0;" +
     "var f1 = function __impl_f1() {" +
     "  var f2 = function __impl_f2(){" +
     "      __check_scope(__env);" +
     "      return x;" +
     "  };" +
     "  var r = f2();" +
     "  var x = 1;" +
     "  return r" +
     "};" +
     "return f1() === undefined",
     (function() {
        var s0 = {};
        s0.x  = 0;
        s0.f1 = {code: "__impl_f1", environment: s0};
        var s1 = { __parent: s0 };
        s1.x  = undefined;
        s1.r  = undefined;
        s1.f2 = {code: "__impl_f2", environment: s1};
        var s2 = { __parent: s1 }
        return s2;
     })());

test("test262/S10.4_A1.1_T1",
     "var y;" +
     "var f = function __impl_f() {" +
     "  var x;" +
     "  if (x === undefined) {" +
     "      x = 0;" +
     "  } else {" +
     "      x = 1;" +
     "  }" +
     "  return x" +
     "};" +
     "y = f();" +
     "y = f();" +
     "return (y === 0)");

}

// As well as some very easy tests for basic functionality:

function easy_tests() {
    test("simple/literal", "return true");
    test("simple/binop", "return (2 == 2)");
    test("simple/var", "var x = true, y = true; return y");
    test("simple/assign", "r = true; return r");
    test("medium/if-true", "var r; if (true) { r = true } else { }; return r");
    test("medium/if-false", "var r; if (false) { } else { r = true }; return r");
    test("medium/logic", "return (true && true)");
    test("medium/logic-and", "x = 1; y = 1; (x = 0) && (y = 0); return (x == 0 && y == 1)");
    test("medium/logic-or", "x = 0; y = 0; (x = 1) || (y = 1); return (x == 1 && y == 0)");
}

// Credit: Some of these tests are adapted from the test262 ECMAScript
// compliance suite: https://github.com/tc39/test262

// ----------------------------------------------------------------- //

//                          Environments

// ----------------------------------------------------------------- //

// Next, we will define some functions for manipulating environments,
// i.e. singly linked lists of environment frames with are represented
// as JavaScript objects. Coding tip: To test if a property is in an
// object, say ["foo" in obj].
//
// Guru Meditation: Using JavaScript objects as string maps is not
// considered good style, because some fields on an object have special
// meaning (e.g.  __proto__).  For clarity of exposition, this lab uses
// objects in this way, but in a real-world setting you would use
// something like: https://code.google.com/p/es-lab/source/browse/trunk/src/ses/StringMap.js

// lookupVariableValue takes an identifier and an environment, and
// returns the value of the variable according to the environment.
// If the variable is unbound, it throws an exception
// (e.g. throw "Unbound variable").

// BEGIN lookupVariableValue (DO NOT DELETE THIS LINE)
function lookupVariableValue(id, env) {

    function recurse(id0, env0) {
        if (id0 in env0) {
            return [true, env0[id0]];
        }
        for (var key in env0) { //Object.keys(env0)
            if (typeof(env0[key])=='object') {
                var inner = recurse(id0,env0[key]);
                if (inner[0]) {
                    return inner;
                }
            }
        }
        return [false,null];
    }
    result = recurse(id,env);
    if (result[0]) {
        return result[1];
    } else {
        throw "Unbound variable";
    }
}

// END lookupVariableValue (DO NOT DELETE THIS LINE)

console.log("lookupVariableValue examples");
console.log(lookupVariableValue("foo", {"foo": 1}));
console.log(lookupVariableValue("foo", {"foo": 1, "__parent": {"foo": 2}}));
console.log(lookupVariableValue("foo", {"__parent": {"foo": 2}}));
try {
    lookupVariableValue("foo", {"__parent": {}});
    console.log("unreachable");
} catch (e) {}

// extendEnvironment takes set of parameters and arguments,
// and returns an extended environment with a new frame for a fresh call.
// For this lab, you can assume that parameters has no duplicate
// entries.
//
// We'd also like you to handle variable hoisting in this function:
// extendEnvironment is also passed a list of identifiers mentioned in
// all "var" declarations in the body of the function (hoisted_ids).
// The new environment frame should include undefined entries for each
// such identifier, if the identifier was not already a parameter (in
// which case nothing happens).

// BEGIN extendEnvironment (DO NOT DELETE THIS LINE)
function extendEnvironment(params, args, hoisted_ids, env) {
    var fresh = {"__parent" : env};
    assert (params.length == args.length, "There should be same number of parameters their associated arguments.");
    for (var i = 0; i < params.length; i++) {
        fresh[params[i]] = args[i];
    }
    for (var i=0; i < hoisted_ids.length; i++) {
        if (!(hoisted_ids[i] in fresh)) {
            fresh[hoisted_ids[i]] = undefined;
        }
    }
    return fresh;



}
// END extendEnvironment (DO NOT DELETE THIS LINE)

console.log("extendEnvironment examples");
console.log(extendEnvironment(["x", "y"], [2, true], ["z"], {}));
console.log(extendEnvironment(["x"], [2], ["x", "z"], {x: 1}));

// Finally, setVariableValue takes an identifier and a value, and sets
// it in the environment.  In JavaScript, the default behavior when an
// identifier is not found in any environment frame is to set it in the
// *topmost* frame (at the end of the list of frames).

// BEGIN setVariableValue (DO NOT DELETE THIS LINE)
function setVariableValue(id, value, env) {
    if ( !(id in env) && ("__parent" in env) ) {
        return setVariableValue(id,value,env.__parent);
    } else {
        env[id] = value;
    }
}
// END setVariableValue (DO NOT DELETE THIS LINE)

console.log("setVariableValue examples");
function test_setVariableValue(id, value, env) {
    setVariableValue(id, value, env);
    console.log(env);
}

test_setVariableValue("x", 2, {x: 0, __parent: {x: 1}})
test_setVariableValue("x", 2, {__parent: {x: 1}});
test_setVariableValue("x", 2, {__parent: {}});

// ----------------------------------------------------------------- //

//                          The Evaluator

// ----------------------------------------------------------------- //
//
// The fragment of JavaScript our interpreter will support is described
// by the grammar below:
//
//      Program    ::= Statement ";" ...
//      Function   ::= "function" Identifier "(" Identifier ... ")"
//                     "{" Statement ... [ "return" Expression ] "}"
//      Statement  ::= Expression
//                   | "var" Identifier ["=" Expression]
//                   | "if" "(" Expression ")"
//                      "{" Statement ... "}"
//                      ["else" "{" Statement ... "}"]
//      Expression ::= Function
//                   | Identifier "=" Expression
//                   | Expression "(" Expression ... ")"
//                   | Identifier
//                   | Literal
//                   | Expression BinaryOperator Expression
//                   | Expression LogicalOperator Expression
//      Identifier ::= ID
//      Literal    ::= [ BOOLEAN | NULL | NUMBER ]
//      BinaryOperator  ::= "+" | "-" | ...
//      LogicalOperator ::= "&&" | "||"
//
// Beyond the omissions we discussed in the test suite, our subset has one
// more notable restriction: "return" is NOT supported as a control
// flow operator.  You can "return", but it is only valid at the end of
// a function body.
//
// We are using Acorn <https://github.com/marijnh/acorn> to parse
// JavaScript. The resulting AST is represented as an ESTree. You can
// read more about the structure of AST nodes at
// https://github.com/estree/estree/blob/master/spec.md

// This function is responsible for parsing the input JavaScript,
// and setting up an initial environment before evaluating statements.
// It returns the result of evaluating the JavaScript string.
function run(str, checkScope) {
    // Global frame does not coincide
    var program = acorn.parse(str, {allowReturnOutsideFunction: true});
    var hoisted_ids = hoist(program.body);
    // Initial top-level environment includes our __check_scope
    // built-in as well as an undefined entry for every
    var env = {__check_scope: checkScope};
    hoisted_ids.forEach(function(id) {
        env[id] = undefined;
    });
    return evalStatements(program.body, env);
}

// To evaluate a list of statements, we execute them one-by-one
// until we hit the last one, and then result of executing that.
// (Obviously, this wouldn't work for short-circuiting control
// flow operators, but we don't support any of those.)
function evalStatements(stmts, env) {
    var r;
    for (var i = 0; i < stmts.length; i++) {
        r = evalStatement(stmts[i], env);
    }
    return r;
}

// Next, we will define the mutually recursive 'evalStatement'
// and 'evalExpression', which respectively evaluate statements
// and expression, as well as 'apply' which is responsible for
// implementing function calls.
//
// You can implement these parts in any order you like, but our
// recommendation is to first do ExpressionStatement, ReturnStatement
// and VariableDeclaration, along with the expressions Literal, Identifier,
// AssignmentExpression and BinaryExpression.  With just these, all
// the simple/ tests should pass.



// This function evaluates a statement in JavaScript.  Write
// the evaluator for four types of statements:
//
// EmptyStatement
//      An empty statement just does nothing.
//
// ExpressionStatement
//      An expression statement evaluates just an expression by itself.
//      For example, "foo = 0;" and "bar();" are expression statements.
//      An expression statement returns undefined (see ReturnStatement).
//
// ReturnStatement
//      A return statement is either "return;" (stmt.argument is null)
//      or "return exp;" (in which case we evaluate the expression and
//      return its value).  (Note: the return you implement WILL NOT
//      short-circuit control flow.)
//
// VariableDeclaration
//      A variable declaration is something like "var x = 0, y = 2;"
//      Due to variable hoisting, a VariableDeclaration can be evaluated
//      as if it were simply "x = 0; y = 2;".  A variable declaration
//      returns undefined.
//
// IfStatement
//      An if statement has the form "if (test) { consequent } else {
//      alternate }".  If test evaluates to true, evaluate the
//      consequent statements, otherwise evaluate the alternate
//      statements.
//
// BEGIN evalStatement (DO NOT DELETE THIS LINE)
function evalStatement(stmt, env) { switch(stmt.type) {
    case "EmptyStatement":
        return;
    case "ExpressionStatement":
        // stmt.expression is an Expression
        return evalExpression(stmt.expression, env);
    case "ReturnStatement":
        // stmt.argument is either null or an Expression
        if (stmt.argument == null) {
            return;
        } else{
            return evalExpression(stmt.argument,env);
        }

    case "VariableDeclaration":
        // stmt.declarations is a list of VariableDeclarators
        // Let decl be a VariableDeclarator, then
        //    * decl.id.name is the String identifier being declared
        //    * decl.init is null or an Expression to initialize the
        //      variable with
        // You may assume this assert holds:
        stmt.declarations.forEach(function(decl) {
            assert.notStrictEqual(decl.id.name.indexOf("__"), 0);
        });
        stmt.declarations.forEach(function(decl) {
            //split the decl up from "var x = 0, y = 2;"
            if (decl.init == null) {
                env[decl.id.name] = undefined;
            } else {
                env[decl.id.name] = evalExpression(decl.init, env);
            }
        });
        return;
    case "IfStatement":
        // stmt.test is an Expression
        // stmt.consequent.body is a list of Statements
        // stmt.alternate.body is a list of Statements, IF stmt.alternate is not null
        // You may assume these asserts hold:
        assert.strictEqual(stmt.consequent.type, "BlockStatement");
        if (stmt.alternate) assert.strictEqual(stmt.alternate.type, "BlockStatement");
        if (evalExpression(stmt.test,env)) { //somehow get a boolean
            stmt.consequent.body.forEach(function(line) {
                evalStatement(line,env);
            });
        } else if (stmt.alternate != null) {
            stmt.alternate.body.forEach(function(line) {
                evalStatement(line,env);
            });
        }


        return;
    default:
        throw "Unsupported statement type:\n" + util.inspect(stmt);
}}
// END evalStatement (DO NOT DELETE THIS LINE)

// This function evaluates an expression in JavaScript.  We support
// quite a few different types of expression, but most of them are
// relatively simple.  Don't forget to use the environment functions
// you defined earlier in this lab.
//
// Literal
//      A literal is something like 0 or true.  No evaluating is necessary;
//      just return the literal.
//
// Identifier
//      An identifier like x is evaluated by looking it up in the
//      current environment.
//
// AssignmentExpression
//      An assignment expression has the form "x = e"; it is evaluated
//      by evaluating the expression e, and then setting the variable x
//      to that value in the current environment.
//
// BinaryExpression
//      A binary expression is something like "e1 + e2".  It is
//      evaluated by evaluating the left argument, then the right
//      argument, and then finally running the binary operand on
//      the resulting values.  For your convenience, we've provided the
//      function 'evalBinop', which takes the string operator name and
//      two values and evaluates it (similar to 'interpBinop' in the
//      first lab.)
//
// LogicalExpression
//      A logical expression is either "e1 && e2" or "e1 || e2".  They
//      are similar to binary expressions, except they are short
//      circuiting: in "e1 && e2", if e1 is falseish, then e2 is
//      not evaluated.
//
// FunctionExpression
//      A function expression has the form "function () { ... }".
//      It evaluates to a closure of the same form as in the
//      test case (an object with a code and environment property),
//      except instead of recording only the name of the code,
//      we record the entire AST of the function (exp).
//
// CallExpression
//      This is the complicated case.  A function call looks like "f(e1,
//      e2)".  We evaluate it by evaluating the callee as well as each
//      of the arguments, and then 'apply'ing it.  'apply' is a helper
//      function we will define shortly.
//
// BEGIN evalExpression (DO NOT DELETE THIS LINE)
function evalExpression(exp, env) { switch (exp.type) {
    case "Literal":
        // expr.value is the literal
        return exp;
    case "Identifier":
        // exp.name is the string of the identifier
        // You don't need to handle these special cases:
        if (exp.name == "__env")     return env;
        if (exp.name == "undefined") return undefined;
        // Your code here:
        return env[exp.name];
    case "AssignmentExpression":
        // exp.left.name is the String identifier being set
        // exp.right is an Expression
        // You may assume these asserts hold:
        assert.strictEqual(exp.operator, "=");
        assert.notStrictEqual(exp.left.name.indexOf("__"), 0);
        // Your code here:
        env[exp.left.name] = evalExpression(exp.right,env);
        //return undefined;
        return;
    case "BinaryExpression":
        // exp.operator is a String binary operator, e.g. "="
        // exp.left is an Expression
        // exp.right is an Expression
        // Note: evalBinop takes a String, the left value, and the right value
        return evalBinop(exp.operator,evalExpression(exp.left,env),evalExpression(exp.right,env));
    case "LogicalExpression":
        // exp.operator is either "&&" or "||"
        // exp.left is an Expression
        // exp.right is an Expression
        if (exp.operator == "||") {
            if (evalExpression(exp.left,env)) {
                return true;
            } else {
                return evalExpression(exp.right,env);
            }
        } else if (exp.operator == "&&") {
            if (!evalExpression(exp.left,env)) {
                return false;
            } else {
                return evalExpression(exp.right,env);
            }
        }
    case "FunctionExpression":
        // Use 'exp' as the code pointer
//      A function expression has the form "function () { ... }".
//      It evaluates to a closure of the same form as in the
//      test case (an object with a code and environment property),
//      except instead of recording only the name of the code,
//      we record the entire AST of the function (exp).
        return {code:exp, environment:env};
    case "CallExpression":
        // exp.callee is an Expression
        // exp.arguments is a list of Expressions
        // Note: apply takes a closure and a list of values to be applied
//      This is the complicated case.  A function call looks like "f(e1,
//      e2)".  We evaluate it by evaluating the callee as well as each
//      of the arguments, and then 'apply'ing it.  'apply' is a helper
//      function we will define shortly.
        return apply(evalExpression(exp.callee,env), exp.arguments.map(function(elem) {
            return evalExpression(elem,env);
        }));
    default:
        throw "Unsupported expression type:\n" + util.inspect(exp);
}}
// END evalExpression (DO NOT DELETE THIS LINE)

// Finally, apply is a helper function which takes a closure and a
// list of arguments, and executes a function call.  Specifically,
// it:
//
//      1. Creates a new environment frame with all the new bindings
//      for the parameters, as well as bindings for hoisted variables.
//      (You can compute the set of hoisted identifiers by calling
//      "hoist(code.body.body)")
//
//      2. Evaluates the statements in the body of the function in
//      this new environment.
//
// BEGIN apply (DO NOT DELETE THIS LINE)
function apply(closure, args) {
    // closure.code.body.body is a list of Statements
    // closure.code.params is a list of Identifiers
    //      (if p is an Identifier, then p.name is the String of the
    //      Identifier.  Hint: don't pass closure.code.params
    //      to extendEnvironment directly, that won't work!)
    // closure.environment is an environment
    if (typeof closure === "function") {
        // Special case to handle __check_scope
        closure.apply(null, args); //empty environment inside extend apply
    } else {
        // your code here
        param_names = closure.code.params.map(function (s) {
            return s.name;
        });
        var env = extendEnvironment(param_names,args,hoist(closure.code.body.body), closure.environment);
        return evalStatements(closure.code.body.body, env);

    }
}
// END apply (DO NOT DELETE THIS LINE)

// Once you have implemented these three functions, all of the tests
// should pass!

// ----------------------------------------------------------------- //

//                          Boring nonsense

// ----------------------------------------------------------------- //

// The contents of this section are the grotty bits to make the rest
// of the lab work.  You are welcome to read... or not.

// A bit tiresome to interpret all binary operators, but it has to be
// done!
function evalBinop(b, x, y) { switch (b) {
    case "==": return x == y;
    case "!=": return x != y;
    case "===": return x === y;
    case "!==": return x !== y;
    case "<": return x < y;
    case "<=": return x <= y;
    case ">": return x > y;
    case ">=": return x >= y;
    case "<<": return x << y;
    case ">>": return x >> y;
    case ">>>": return x >>> y;
    case "+": return x + y;
    case "-": return x - y;
    case "*": return x * y;
    case "/": return x / y;
    case "%": return x % y;
    case "|": return x | y;
    case "^": return x ^ y;
    case "&": return x & y;
    default:  throw "Unsupported binary operator " + b
}}

// This is a simple utility function which grovels through all
// expressions looking for variable declarations.  Given a list of
// statements, it returns a list of identifiers that are to be hoisted
// to the initial environment declaration.
function hoist(stmts) {
    var r = [];
    function hoistStatement(stmt) { switch (stmt.type) {
        case "EmptyStatement":
        case "ExpressionStatement":
        case "ReturnStatement":
            return;
        case "IfStatement":
            hoistStatement(stmt.consequent);
            if (stmt.alternate) hoistStatement(stmt.alternate);
            return;
        case "VariableDeclaration":
            return stmt.declarations.forEach(function(decl) { r.push(decl.id.name); });
        case "BlockStatement":
            return stmt.body.forEach(hoistStatement);
        default:
            throw "Unsupported statement type:\n" + util.inspect(stmt);
    }}
    stmts.forEach(hoistStatement);
    return r;
}

// The bulk of 'test' is responsible for taking an environment
// that you have passed (expected_env) and comparing it with the actual
// environment that was constructed at runtime.  This is essentially
// a deep equality test with a twist: we have to also figure out the
// sharing structure of environments and make sure they match up.  This
// is done by destructively labeling environments with an "__id"
// property as we traverse them.  In fact, this can be done
// non-destructively in ES6 using Maps, but I haven't ported the code
// to use this facility yet.
function test(name, str, expected_env) {
    console.log("testing " + name + "... ");
    // 'id_counter' is used to label nodes so that we can deal with
    // (1) circular closure/environment references, and
    // (2) determining sharing between environments.
    var id_counter = 1;
    var r = run(str, function(actual_env) {
        function compare_frame(expected, actual) {
            function error(msg) {
                e = new Error(msg);
                e.expected = expected;
                e.actual = actual;
                throw e;
            }
            if (!expected && !actual) return; // the base case
            if (!expected && actual) error("expected no frame but got frame");
            if (expected && !actual) error("expected frame but got no frame");
            if (expected.__id && actual.__id && expected.__id === actual.__id) return;
            if (expected.__id || actual.__id) error("frames structure not same");
            expected.__id = id_counter;
            actual.__id = id_counter;
            id_counter++;
            for (id in expected) {
                if (id.indexOf("__") === 0) continue;
                if (!(id in actual)) {
                    error("missing expected binding for " + id);
                }
                compare_value(id, expected, actual);
            }
            for (id in actual) {
                if (id.indexOf("__") === 0) continue;
                if (!(id in expected)) {
                    error("unexpected extra binding for " + id);
                }
            }
            compare_frame(expected.__parent, actual.__parent);
        }
        function compare_value(id, expected_env, actual_env) {
            function isExpectedClosure(x) {
                return x && x.code && x.environment;
            }
            function error(msg) {
                e = new Error(msg);
                e.expected = expected_env;
                e.actual = actual_env;
                e.id = id;
                throw e;
            }
            var expected = expected_env[id];
            var actual = actual_env[id];
            if (isExpectedClosure(expected)) {
                // Assymmetric here, because it's tough to actually
                // get the function body here
                if (expected.code != actual.code.id.name) {
                    error("expected closure code " + expected.code +
                            " but got " + actual.code.id.name);
                }
                try {
                    compare_frame(expected.environment, actual.environment);
                } catch (e) {
                    e.message += " (referenced from closure)";
                    throw e;
                }
            } else if (expected !== actual) {
                error("values do not match");
            }
        }
        try {
            compare_frame(expected_env, actual_env);
        } catch (e) {
            // Uncomment me if you need a call stack
            //console.log(e.stack);
            console.log(e.message);
            console.log("while comparing " + (e.id ? e.id + " in " : "") + "environments:");
            console.log(e.expected);
            console.log(e.actual);
        }
    });
    if (r !== true) console.log("    test did not return true!");
}

// Actually run the tests!
easy_tests();
closure_tests();
