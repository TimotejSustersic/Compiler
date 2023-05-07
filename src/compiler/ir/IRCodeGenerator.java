/**
 * @ Author: turk
 * @ Description: Generator vmesne kode.
 */

package compiler.ir;

import static common.RequireNonNull.requireNonNull;

import java.util.ArrayList;
import java.util.List;

import common.Constants;
import common.Report;
import compiler.common.Visitor;
import compiler.frm.Access;
import compiler.frm.Frame;
import compiler.frm.Access.Global;
import compiler.frm.Access.Local;
import compiler.frm.Frame.Label;
import compiler.ir.chunk.Chunk;
import compiler.ir.code.IRNode;
import compiler.ir.code.expr.*;
import compiler.ir.code.expr.BinopExpr.Operator;
import compiler.ir.code.stmt.*;
import compiler.parser.ast.def.*;
import compiler.parser.ast.def.FunDef.Parameter;
import compiler.parser.ast.expr.*;
import compiler.parser.ast.type.Array;
import compiler.parser.ast.type.Atom;
import compiler.parser.ast.type.TypeName;
import compiler.seman.common.NodeDescription;
import compiler.seman.type.type.Type;

public class IRCodeGenerator implements Visitor {
    /**
     * Preslikava iz vozlišč AST v vmesno kodo.
     */
    private NodeDescription<IRNode> imcCode;

    /**
     * Razrešeni klicni zapisi.
     */
    private final NodeDescription<Frame> frames;

    /**
     * Razrešeni dostopi.
     */
    private final NodeDescription<Access> accesses;

    /**
     * Razrešene definicije.
     */
    private final NodeDescription<Def> definitions;

    /**
     * Razrešeni tipi.
     */
    private final NodeDescription<Type> types;

    /**
     * **Rezultat generiranja vmesne kode** - seznam fragmentov.
     */
    public List<Chunk> chunks = new ArrayList<>();


    private void parseExpr(Expr expr) {
        if (expr instanceof Binary)
            visit((Binary) expr);       
        else if (expr instanceof Name)
            visit((Name) expr); 
        else if (expr instanceof Block)
            visit((Block) expr);
        else if (expr instanceof Where)
            visit((Where) expr);
        else if (expr instanceof For)
            visit((For) expr);
        else if (expr instanceof Literal)
            visit((Literal) expr);
        else if (expr instanceof IfThenElse)
            visit((IfThenElse) expr);
        else if (expr instanceof Call)
            visit((Call) expr);
        else if (expr instanceof While)
            visit((While) expr);
        else if (expr instanceof Unary)
            visit((Unary) expr);
    }

    private IRNode getIRNode(Expr expr) {
        this.parseExpr(expr);
        var IRE = this.imcCode.valueFor(expr);

        if (IRE.isPresent()) 
            return (IRNode) IRE.get();
        
        Report.error(expr.position, "Epression" + expr.getClass().getName() + " code not found");
        return null;
    }

    public IRCodeGenerator(
        NodeDescription<IRNode> imcCode,
        NodeDescription<Frame> frames, 
        NodeDescription<Access> accesses,
        NodeDescription<Def> definitions,
        NodeDescription<Type> types
    ) {
        requireNonNull(imcCode, frames, accesses, definitions, types);
        this.types = types;
        this.imcCode = imcCode;
        this.frames = frames;
        this.accesses = accesses;
        this.definitions = definitions;
    }

    @Override
    public void visit(Call call) {
        var frame = this.frames.valueFor(call);
        if (!frame.isPresent())
            Report.error(call.position, "Call frame not found");

        var args = new ArrayList<IRExpr>();

        for (Expr arg: call.arguments) {

            this.parseExpr(arg);
            var argCode = imcCode.valueFor(arg);

            if (argCode.isPresent())
                args.add((IRExpr) imcCode.valueFor(arg).get());
            else  
                Report.error(arg.position, "Argument code not found");
        }

        var code = new CallExpr(frame.get().label, args);
        imcCode.store(code, call);
    }

    @Override
    public void visit(Binary binary) {

        this.parseExpr(binary.left);
        this.parseExpr(binary.right);

        var lhs = imcCode.valueFor(binary.left);
        var rhs = imcCode.valueFor(binary.right);

        var Lmem = new MemExpr((IRExpr) lhs.get());
        var Rmem = new MemExpr((IRExpr) rhs.get());

        if (!(lhs.isPresent() && rhs.isPresent()))
            Report.error(binary.position, "Binary expression isn't present");

        if (binary.operator == compiler.parser.ast.expr.Binary.Operator.ASSIGN) {
            
            var move = new MoveStmt((IRExpr) lhs.get(), Rmem);
            imcCode.store(move, binary);  
        }
        else {            
            Operator op = Operator.valueOf(binary.operator.name());        
            var code = new BinopExpr(Lmem, Rmem, op);
            imcCode.store(code, binary);        
        }
    }

    @Override
    public void visit(Block block) {
        for (Expr expr: block.expressions)
           this.parseExpr(expr);
    }

    @Override
    public void visit(For forLoop) {
                 
        var stavki = new ArrayList<IRStmt>();

        var L0 = new LabelStmt(Label.nextAnonymous());
        var L1 = new LabelStmt(Label.nextAnonymous());
        var L2 = new LabelStmt(Label.nextAnonymous());

        var counter = this.getIRNode(forLoop.counter);
        var low = this.getIRNode(forLoop.low);
        var high = this.getIRNode(forLoop.high);               

        // assign 
        var LowMem = new MemExpr((IRExpr) low);
        var move = new MoveStmt((IRExpr) counter, LowMem);
        stavki.add(move);

        // start
        stavki.add(L0);

        // condition
        var CounterMem = new MemExpr((IRExpr) counter);
        var HighMem = new MemExpr((IRExpr) high);

        var bin = new BinopExpr(CounterMem, HighMem, Operator.LEQ);
        var cjump = new CJumpStmt(bin, L1.label, L2.label);
        stavki.add(cjump);        

        // body        
        stavki.add(L1);

        var body = this.getIRNode(forLoop.body);
        stavki.add(new ExpStmt((IRExpr) body));

        //step
        var step = this.getIRNode(forLoop.step);
        var StepMem = new MemExpr((IRExpr) step);

        var binStep = new BinopExpr(CounterMem, StepMem, Operator.ADD);

        var counterPlusStep = new MoveStmt((IRExpr) counter, binStep);
        stavki.add(counterPlusStep);

        // out
        stavki.add(L2);

        var code = new SeqStmt(stavki);
        imcCode.store(code, forLoop);
    }

    // tko da vedno vrne naslov in mors pol naknadno mem delat
    @Override
    public void visit(Name name) {
        var defined = this.definitions.valueFor(name);

        if (!defined.isPresent())
            Report.error(name.position, "Name nima definicije.");

        var acc = this.accesses.valueFor(defined.get());

        if (!acc.isPresent())
            Report.error(name.position, "Name nima accessa.");

       
        if (acc.get() instanceof Global) {
            var code = new NameExpr(((Global) acc.get()).label);
            this.imcCode.store(code, name); 
        }
        else if (acc.get() instanceof Local) {
            var dostop = (Local) acc.get();

            var offset = new ConstantExpr(dostop.offset);

            // TODO nevem kako bi dubu framepointer
            IRExpr FP = new ConstantExpr(0);

            var code = new BinopExpr(FP, offset, Operator.ADD);

            this.imcCode.store(code, name); 
        }
        else if (acc.get() instanceof compiler.frm.Access.Parameter) {
            var dostop = (compiler.frm.Access.Parameter) acc.get();

            var offset = new ConstantExpr(dostop.offset);

            // TODO nevem kako bi dubu framepointer Temp rabs uporabljat
            IRExpr FP = new ConstantExpr(0);

            var code = new BinopExpr(FP, offset, Operator.SUB);

            this.imcCode.store(code, name); 
        }
        else 
            Report.error(name.position, "Name is not global or local.?");
        
    }

    @Override
    public void visit(IfThenElse ifThenElse) {
                
        var stavki = new ArrayList<IRStmt>();

        var L0 = new LabelStmt(Label.nextAnonymous());
        var L1 = new LabelStmt(Label.nextAnonymous());
        var L2 = new LabelStmt(Label.nextAnonymous());

        // start
        stavki.add(L0);

        // condition
        this.parseExpr(ifThenElse.condition);
        var bin = imcCode.valueFor(ifThenElse.condition);
        if (!bin.isPresent())
            Report.error(ifThenElse.position, "While condition code not found.");

        var cjump = new CJumpStmt((IRExpr) bin.get(), L1.label, L2.label);
        stavki.add(cjump);

        // then
        stavki.add(L1);

        this.parseExpr(ifThenElse.thenExpression);
        var thenExpr = this.imcCode.valueFor(ifThenElse.thenExpression);

        if (!thenExpr.isPresent())
            Report.error(ifThenElse.position, "While body code not found.");

        stavki.add(new ExpStmt((IRExpr) thenExpr.get()));

        // else
        stavki.add(L2);

        if (ifThenElse.elseExpression.isPresent()) {

            this.parseExpr(ifThenElse.elseExpression.get());
            var elseExpr = this.imcCode.valueFor(ifThenElse.elseExpression.get());

            if (!elseExpr.isPresent())
                Report.error(ifThenElse.position, "While body code not found.");

            stavki.add(new ExpStmt((IRExpr) elseExpr.get()));
        }

        var code = new SeqStmt(stavki);
        imcCode.store(code, ifThenElse);
    }

    @Override
    public void visit(Literal literal) {
        var type = this.types.valueFor(literal);

        if (!type.isPresent())
            Report.error(literal.position, "No literal type found.");
        
        IRNode code;
        if (type.get().isInt())
            code = new ConstantExpr(Integer.parseInt(literal.value));
        else if (type.get().isLog()) {
            if (literal.value == "true")
                code = new ConstantExpr(1);
            else 
                code = new ConstantExpr(0);
        }
        // else if (type.get().isStr())
            code = new ConstantExpr(0);
         
            
        imcCode.store(code, literal);
    }

    @Override
    public void visit(Unary unary) {

        var op = Operator.ADD;
        if (unary.operator == compiler.parser.ast.expr.Unary.Operator.SUB)
            op = Operator.SUB;

        var lhs = new ConstantExpr(0);

        this.parseExpr(unary.expr);
        var rhs = imcCode.valueFor(unary.expr);

        if (rhs.isPresent()) {
            var code = new BinopExpr(lhs, (IRExpr) rhs.get(), op);
            imcCode.store(code, unary);
        }
        else 
            Report.error(unary.position, "Unary expression isn't present");
    }

    @Override
    public void visit(While whileLoop) {
        
        var stavki = new ArrayList<IRStmt>();

        var L0 = new LabelStmt(Label.nextAnonymous());
        var L1 = new LabelStmt(Label.nextAnonymous());
        var L2 = new LabelStmt(Label.nextAnonymous());

        // start
        stavki.add(L0);

        // condition
        this.parseExpr(whileLoop.condition);
        var bin = imcCode.valueFor(whileLoop.condition);
        if (!bin.isPresent())
            Report.error(whileLoop.position, "While condition code not found.");

        var cjump = new CJumpStmt((IRExpr) bin.get(), L1.label, L2.label);
        stavki.add(cjump);

        // then
        stavki.add(L1);

        this.parseExpr(whileLoop.body);
        var body = this.imcCode.valueFor(whileLoop.body);

        if (!body.isPresent())
            Report.error(whileLoop.position, "While body code not found.");

        stavki.add(new ExpStmt((IRExpr) body.get()));

        // else
        stavki.add(L2);

        var code = new SeqStmt(stavki);
        imcCode.store(code, whileLoop);
    }

    @Override
    public void visit(Where where) {

        this.visit(where.defs);
        this.parseExpr(where.expr);
    }

    @Override
    public void visit(Defs defs) {
        for (Def def : defs.definitions) {
            if (def instanceof FunDef)
                visit((FunDef) def);            
            else if (def instanceof TypeDef)
                visit((TypeDef) def);            
            else if (def instanceof VarDef)
                visit((VarDef) def);
        }
    }

    @Override
    public void visit(FunDef funDef) {
        this.parseExpr(funDef.body);
    }

    @Override
    public void visit(TypeDef typeDef) {
        // /
    }

    @Override
    public void visit(VarDef varDef) {
        // /
    }

    @Override
    public void visit(Parameter parameter) {
        // /
    }

    @Override
    public void visit(Array array) {
        // /
    }

    @Override
    public void visit(Atom atom) {
        // /
    }

    @Override
    public void visit(TypeName name) {
        // TODO Auto-generated method stub
        throw new UnsupportedOperationException("Unimplemented method 'visit'");
    }
}
