#include <string>
#include <iostream>
#include <variant>
#include <map>
#include <exception>
#include <stdexcept>
#include "box.h"
#include "List.h"

using std::variant;
using std::string;

// Definition of Expr variants
struct Var {
    string name;
    Var(string _name): name(_name){};
    operator std::string() const { return "Var("+name+")"; }
};
struct Int {
    int val;
    Int(int _val): val(_val){};
    operator std::string() const { return "Int("+std::to_string(val)+")"; }
};
struct AUnit {
    AUnit() {};
    operator std::string() const { return "AUnit()"; }
};
using Expr = variant<Var, 
                     Int,
                     AUnit,
                     box<struct IsAUnit>, 
                     box<struct Add>, 
                     box<struct IfGreater>, 
                     box<struct MLet>,
                     box<struct Fun>, 
                     box<struct Closure>,
                     box<struct APair>,
                     box<struct Fst>,
                     box<struct Snd>,
                     box<struct Call>>; 

template<typename T> bool is(Expr e);

std::string toString(Expr e);

struct Add {
    Expr e1, e2;
    Add(Expr _e1, Expr _e2): e1(_e1), e2(_e2) {}; 
    operator std::string() const { 
        return "Add("+toString(e1)+", "+toString(e2)+")";
    }
};
struct IfGreater {
    Expr e1, e2, e3, e4;
    IfGreater(Expr _e1, Expr _e2, Expr _e3, Expr _e4): e1(_e1), e2(_e2), e3(_e3), e4(_e4) {};
    operator std::string() const {
        return "IfGreater("+toString(e1)+", "+toString(e2)+", "
                           +toString(e3)+", "+toString(e4)+")";
    }
};
struct MLet {
    string varName;
    Expr e1, e2;
    MLet(string _varName, Expr _e1, Expr _e2): varName(_varName), e1(_e1), e2(_e2) {}; 
    operator std::string() const { 
        return "MLet("+varName+", "+toString(e1)+", "+toString(e2)+")";
    }
};
struct APair {
    Expr e1, e2;
    APair(Expr _e1, Expr _e2): e1(_e1), e2(_e2) {};
    operator std::string() const { 
        return "APair("+toString(e1)+", "+toString(e2)+")";
    }
};
struct Fst {
    Expr e;
    Fst(Expr _e): e(_e) {};
    operator std::string() const { return "Fst("+toString(e)+")"; }
};
struct Snd {
    Expr e;
    Snd(Expr _e): e(_e) {};
    operator std::string() const { return "Snd("+toString(e)+")"; }
};
struct IsAUnit {
    Expr e;
    IsAUnit(Expr _e): e(_e) {};
    operator std::string() const { return "IsAUnit("+toString(e)+")"; }
};

struct Fun {
    string funName;
    string argName;
    Expr body;
    Fun(string _f, string _a, Expr _b): funName(_f), argName(_a), body(_b) {}; 
    operator std::string() const { 
        return "Fun("+funName+", "+argName+", "+toString(body)+")";
    }
};
struct Closure {
    std::map<string, Expr> env;
    Fun f;
    Closure(std::map<string, Expr> _env, Fun _f): env(_env), f(_f) {};
    operator std::string() const { 
        return "Closure(env, "+std::string(f)+")";
    }
};
struct Call {
    Expr funExpr, actual;
    Call(Expr _fe, Expr _a): funExpr(_fe), actual(_a) {};
    operator std::string() const { 
        return "Call("+toString(funExpr)+", "+toString(actual)+")";
    }
};
// End of Definition of Expr variants 

// Functions for check variants.
// e.g. is<APair>(e) or is<Int>(Expr(Int(42)))
template<typename T>
bool is(Expr e) { return std::holds_alternative<T>(e); }
template<>
bool is<Closure>(Expr e) { return std::holds_alternative<box<struct Closure>>(e); }
template<>
bool is<IsAUnit>(Expr e) { return std::holds_alternative<box<struct IsAUnit>>(e); }
template<>
bool is<Add>(Expr e) { return std::holds_alternative<box<struct Add>>(e); }
template<>
bool is<IfGreater>(Expr e) { return std::holds_alternative<box<struct IfGreater>>(e); }
template<>
bool is<MLet>(Expr e) { return std::holds_alternative<box<struct MLet>>(e); }
template<>
bool is<Fun>(Expr e) { return std::holds_alternative<box<struct Fun>>(e); }
template<>
bool is<APair>(Expr e) { return std::holds_alternative<box<struct APair>>(e); }
template<>
bool is<Fst>(Expr e) { return std::holds_alternative<box<struct Fst>>(e); }
template<>
bool is<Snd>(Expr e) { return std::holds_alternative<box<struct Snd>>(e); }
template<>
bool is<Call>(Expr e) { return std::holds_alternative<box<struct Call>>(e); }

// Converting Expr to std::string representation.
std::string toString(Expr e) {
    if (is<Int>(e)) {
        return std::get<Int>(e);
    } else if (is<Var>(e)) {
        return std::get<Var>(e);
    } else if (is<AUnit>(e)) {
        return std::get<AUnit>(e);
    } else if (is<IsAUnit>(e)) {
        return *std::get<box<struct IsAUnit>>(e);
    } else if (is<box<struct Add>>(e)) {
        Add add = *std::get<box<struct Add>>(e);
        return add;
    } else if (is<box<struct IfGreater>>(e)) {
        IfGreater ifgt = *std::get<box<struct IfGreater>>(e);
        return ifgt;
    } else if (is<box<struct MLet>>(e)) {
        MLet mlet = *std::get<box<struct MLet>>(e);
        return mlet;
    } else if (is<box<struct Fun>>(e)) {
        Fun fun = *std::get<box<struct Fun>>(e);
        return fun;
    } else if (is<box<struct Closure>>(e)) {
        Closure closure = *std::get<box<struct Closure>>(e);
        return closure;
    } else if (is<box<struct APair>>(e)) {
        return *std::get<box<struct APair>>(e);
    } else if (is<box<struct Fst>>(e)) {
        return *std::get<box<struct Fst>>(e);
    } else if (is<box<struct Snd>>(e)) {
        return *std::get<box<struct Snd>>(e);
    } else if (is<box<struct Call>>(e)) {
        Call call = *std::get<box<struct Call>>(e);
        return call;
    } else {
        throw std::runtime_error("toString(Expr): Unexpected Expr is given!");
    }
}

// Asserts that given Expr is a value in MUPL.
void assertValue(Expr e) {
    if (is<APair>(e)) {
        APair ap = *std::get<box<struct APair>>(e);
        assertValue(ap.e1);
        assertValue(ap.e2);
    } else if (!(is<Int>(e) || 
               is<Closure>(e) ||
               is<AUnit>(e))) {
        throw std::runtime_error(toString(e) + " is not a value!");
    }
}

// Make a new environment by copying from the passed environment.
std::map<string, Expr> makeNewEnvFrom(std::map<string, Expr> fromEnv) {
    std::map<string, Expr> newEnv(fromEnv);
    return newEnv;
}

Expr envlookup(std::map<string, Expr> env, Var v) {
    if (env.count(v.name) == 0) {
        throw std::runtime_error(toString(v)+" is not in the environment");
    } else {
        Expr val = env.at(v.name);
        assertValue(val);
        return val;
    }
}

Expr eval_under_env(Expr e, std::map<string, Expr> env) {
    return std::visit(overload {
        [&](Int& i) { return e;},
        [&](Var& v) {
          Expr val = envlookup(env, v);
          return val;
        },
        [&](box<struct Add>& a) {
          Expr e1 = eval_under_env(a->e1, env);
          Expr e2 = eval_under_env(a->e2, env);
          if (is<Int>(e1) && is<Int>(e2)) {
            Int i1 = std::get<Int>(e1);
            Int i2 = std::get<Int>(e2);
            Expr res(Int(i1.val+i2.val));
            return res;
          } else {
            throw std::runtime_error("Unexpected types for sub-expressions of Add");
          }
        },

        // TODO: Students need to implement following functions.
        [&](AUnit& au) { /* TODO OK */ 
            return e;
        },
        [&](box<struct IsAUnit>& isa) { 
            /* TODO OK */
            Expr e1 = eval_under_env(isa->e, env);
            Expr res = Int(0);
            if(is<AUnit>(e1))
                res = Int(1);
            return res;
        },
        [&](box<struct IfGreater>& ifgt) {
            /* TODO OK */
            Expr e1 = eval_under_env(ifgt->e1, env);
            Expr e2 = eval_under_env(ifgt->e2, env);
            if (is<Int>(e1) && is<Int>(e2)) {
              Int i1 = std::get<Int>(e1);
              Int i2 = std::get<Int>(e2);
              if (i1.val > i2.val) {
                return eval_under_env(ifgt->e3, env);
              } else {
                return eval_under_env(ifgt->e4, env);
              }
            } else {
                throw std::runtime_error("Unexpected types for the condition of IfGreater");
            }

            return ifgt->e4;
        }, 
        [&](box<struct MLet>& l) {
            /* TODO */
            Expr e1 = eval_under_env(l->e1, env);
            // std::cout << "MLet의 e1: " << toString(e1) << std::endl;
            std::map<string, Expr> let_env = makeNewEnvFrom(env);
            let_env.insert_or_assign(l->varName, e1);
            // std::cout << "MLet으로 인해 "<<l->varName<<": " << toString(e1) << std::endl<< std::endl;
            Expr e2 = eval_under_env(l->e2, let_env);

            return e2;
        },
        [&](box<struct Fun>& f) {
            /* TODO */
            std::map<string, Expr> fun_env = makeNewEnvFrom(env);
            //fun_env.insert_or_assign(f->funName, f); //@ ""이면 아예 이 코드가 실행이 안 되도록 해야하나?
            struct Closure closure = Closure(fun_env,*f);
            closure.env.insert_or_assign(f->funName, closure); //@ ""이면 아예 이 코드가 실행이 안 되도록 해야하나?
            return (Expr)closure;
        },
        [&](box<struct Closure>& c) {
            /* TODO */
            return e;
        },
        [&](box<struct APair>& ap) {
            /* TODO */
            Expr e1 = eval_under_env(ap->e1, env);
            Expr e2 = eval_under_env(ap->e2, env);
            Expr new_ap = APair(e1,e2);
            return new_ap;
        },
        [&](box<struct Fst>& fst) { 
            /* TODO */
            Expr ap_expr = eval_under_env(fst->e, env);
            APair ap = *std::get<box<struct APair>>(ap_expr);

            Expr res = eval_under_env(ap.e1,env);
            return res;
        },
        [&](box<struct Snd>& snd) { 
            /* TODO */
            Expr ap_expr = eval_under_env(snd->e, env);
            APair ap = *std::get<box<struct APair>>(ap_expr);

            Expr res = eval_under_env(ap.e2,env);
            return res;
        },
        [&](box<struct Call>& call) {
            /* TODO */
            Expr res = 0;
            std::cout<<"계산전 인자: "<<toString(call->actual)<<std::endl;
            Expr arg = eval_under_env(call->actual, env);
            std::cout<<"계산된 인자: "<<toString(arg)<<std::endl;
            std::cout<< "클로저 계산 시작 - "<< toString(call->funExpr) <<std::endl;
            Expr closure_expr = eval_under_env(call->funExpr, env);
            std::cout<< "클로저 계산 끝 - " << toString(closure_expr) <<std::endl<<std::endl;
            if (is<Closure>(closure_expr)) {
                Closure closure = *std::get<box<struct Closure>>(closure_expr);
                std::map<string, Expr> fun_env = makeNewEnvFrom(closure.env);
                Fun fun = closure.f;
                fun_env.insert_or_assign(fun.funName, closure); //@@

                fun_env.insert_or_assign(fun.argName, arg);
            std::cout<<"결국 인자: "<<toString(arg)<<std::endl;

            std::cout << "통곡의 벽 on with " << fun.funName << "- 뭐에서 걸렸냐?: " << toString(fun.body) << std::endl;
                res = eval_under_env(fun.body, fun_env);
            std::cout << "통곡의 벽 out with " << fun.funName  << std::endl << std::endl;
            } else {
                throw std::runtime_error("Unexpected types for the condition of Call's first argument. It should be a Closure.");
            }
            return res;
        },
      }, e);
}

Expr eval(Expr e) {
    std::map<string, Expr> env;
    return eval_under_env(e, env);
}


Expr makeIntList(int from, int to) {
    Expr next = AUnit();
    Expr res = AUnit();
    for (int i=to-1; i>=from; i--) {
        Expr tmp = APair(Int(i), next);
        res = tmp;
        next = tmp;
    }
    return res;
}

Expr IfAUnit(Expr e1, Expr e2, Expr e3) {
    return IfGreater(IsAUnit(e1), Int(0), e2, e3);
}

// Expr MuplMap_old(Fun f) {
//     /* Fun(함수이름, 인자이름, 함수 바디) */
//     return Fun("", "list" , IfAUnit(Var("list"), AUnit(), APair( Call( f, (Fst(Var("list")))), Call(MuplMap_old(f),Snd(Var("list")) ))) );
// }

Expr MuplMap() {
    /*
    pseudo code in ML:
    fn fun_arg =>
       let fun muplrec(lst) =
              if IsAUnit(lst)
              then AUnit()
              else APair(fun_arg(Fst(lst)),
                         muplrec(Snd(lst)))             
       in muplrec
       end
    */
    return Fun("맵", "fun_arg", MLet("muplrec",Fun("muplrec", "lst",IfAUnit(Var("lst"),AUnit(),APair(Call(Var("fun_arg"),Fst(Var("lst"))),Call(Var("muplrec"), Snd(Var("lst")))))),Var("muplrec")));
    //return Fun("", "fun_arg", Fun("muplrec", "lst",IfAUnit(Var("lst"),AUnit(),APair(Call(Var("fun_arg"),Fst(Var("lst"))),Call(Var("muplrec"), Snd(Var("lst")))))));
}


Expr MuplMapAddN() {
    // return Fun("", "I", MuplMap(Fun("", "x", Add(Var("x"),Var("I"))))); // old_version
    return MLet("map",MuplMap(),Fun("맵 애드 엔","I",Call(Var("map"),Fun("맵 안에 넣을 함수", "x", Add(Var("x"),Var("I"))))));
    // TODO
    // pseudo code in ML:
    // let val map = MuplMap()
    // in
    //    fn I => map(fn x => x+I)
    // end
}

/*
Expr MuplMap() {
    TODO

    pseudo code in ML:
    fn fun_arg =>
       let fun muplrec(lst) =
              if IsAUnit(lst)
              then AUnit()
              else APair(fun_arg(Fst(lst)),
                         muplrec(Snd(lst)))             
       in muplrec
       end
}

Expr MuplMapAddN() {
    // TODO
    // pseudo code in ML:
    // let val map = MuplMap() in
    //    fn I => map(fn x => x+I)
    // end
}
*/

//! warm-up

Expr ToMuplList(List<Expr> list){
    if(list.isEmpty())
        return AUnit();
    else
        return APair(list.head(),ToMuplList(list.tail()));
}

List<Expr> FromMuplList(Expr mupl_list){   
    if (is<AUnit>(mupl_list)){
        return List<Expr>();
    }
    else if (is<APair>(mupl_list)) {
        APair apair = *std::get<box<struct APair>>(mupl_list);
        return FromMuplList(apair.e2).cons(apair.e1);
    } else {
        throw std::runtime_error("Unexpected types for the condition of FromMuplList");
    }
}

template<class T>
void print2(List<T> lst)
{
    forEach(lst, [](T v) 
    {
        std::cout << "[" << toString(v) << "] "; 
    });
    std::cout << std::endl;
}

int main() {
    // Test code for eval()
    std::map<string, Expr> env;
    // env.insert_or_assign("a", Expr(Int(40)));

    // Expr e = Add(Var("a"), Int(2));
    // Expr res = eval_under_env(e, env);
    // Int i = std::get<Int>(res);
    // std::cout << toString(e) << " = " << i.val << std::endl;

    // e = IfGreater(Var("a"), Int(42), Int(1), Int(2));
    // res = eval_under_env(e, env);
    // i = std::get<Int>(res);
    // std::cout << toString(e) << " = " << i.val << std::endl;

    // e = IfGreater(Int(42), Var("a"), Int(1), Int(2));
    // res = eval_under_env(e, env);
    // i = std::get<Int>(res);
    // std::cout << toString(e) << " = " << i.val << std::endl;

    // e = IfGreater(Int(42), Var("a"), Add(Int(1),Int(1)), Add(Int(2),Int(2)));
    // res = eval_under_env(e, env);
    // i = std::get<Int>(res);
    // std::cout << toString(e) << " = " << i.val << std::endl;

    // // AUnit
    // e = AUnit();
    // res = eval_under_env(e, env);
    // std::cout << toString(e) << std::endl;

    // // isAUnit 1
    // e = IsAUnit(AUnit());
    // res = eval_under_env(e, env);
    // i = std::get<Int>(res);
    // std::cout << toString(e) << " = " << i.val << std::endl;

    // // isAUnit 2
    // e = IsAUnit(Int(4));
    // res = eval_under_env(e, env);
    // i = std::get<Int>(res);
    // std::cout << toString(e) << " = " << i.val << std::endl;

    // // Mlet
    // Expr e2 = MLet("a", Int(5), MLet("b", Int(10), Add(Var("a"), Var("b"))));
    // res = eval_under_env(e2, env);
    // i = std::get<Int>(res);
    // std::cout << toString(e2) << " = " << i.val << std::endl;

    // res = eval(e2);
    // i = std::get<Int>(res);
    // std::cout << toString(e2) << " = " << i.val << std::endl;


    // Expr e3 = Call(Fun("add1", "x", Add(Var("x"), Int(1))), Int(41));
    // res = eval_under_env(e3, env);
    // i = std::get<Int>(res);
    // std::cout << toString(e3) << " = " << i.val << std::endl; 

    // Expr e4 = IfGreater(Int(0), Int(1), Int(42), Int(-42));
    // res = eval_under_env(e4, env);
    // std::cout << toString(e4) << " = " << toString(res) << std::endl; 

    // Expr e5 = MLet("a", Int(5), Add(Var("a"), MLet("a", Int(10), Add(Var("a"), Int(1)))));
    // res = eval_under_env(e5, env);
    // std::cout << toString(e5) << " = " << toString(res) << std::endl;

    // Expr e6 = APair(Add(Int(0), Int(10)), APair(Int(1), AUnit()));
    // res = eval_under_env(e6, env);
    // std::cout << toString(e6) << " = " << toString(res) << std::endl;

    // Expr e7 = makeIntList(0, 2);
    // std::cout << toString(e7) << " = " << toString(e7) << std::endl;

    // List<Expr> e6_list = FromMuplList(e6);
    // print2(e6_list);
    // Expr e6_list_rollback = ToMuplList(e6_list);
    // std::cout << toString(e6_list_rollback) << std::endl;

    // List<Expr> e6_val_list = FromMuplList(res);
    // print2(e6_val_list);
    // Expr e6_val_list_rollback = ToMuplList(e6_val_list);
    // std::cout << toString(e6_val_list_rollback) << std::endl;


    Expr e8 = eval(Call(Call(MuplMapAddN(), Int(10)), makeIntList(0, 5)));
    std::cout << toString(e8) << " = " << toString(e8) << std::endl;
    

    return 0;
}
