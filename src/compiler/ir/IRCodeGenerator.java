/**
 * @ Author: turk
 * @ Description: Generator vmesne kode.
 */

package compiler.ir;

import static common.RequireNonNull.requireNonNull;

import java.util.ArrayList;
import java.util.List;

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
import compiler.parser.ast.Ast;
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
        
        Report.error(expr.position, "Epression " + expr.getClass().getSimpleName() + " code not found.");
        return null;
    }

    private Frame getFrame(Ast expr) {
        var frame = this.frames.valueFor(expr);

        if (frame.isPresent()) 
            return  frame.get();
        
        Report.error(expr.position, "Frame for " + expr.getClass().getSimpleName() + " not found.");
        return null;
    }

    private Def getDefinition(Ast expr) {
        var definition = this.definitions.valueFor(expr);

        if (definition.isPresent()) 
            return  definition.get();
        
        Report.error(expr.position, "Definition for " + expr.getClass().getSimpleName() + " not found.");
        return null;
    }

    private Access getAccess(Ast expr) {
        var access = this.accesses.valueFor(expr);

        if (access.isPresent()) 
            return access.get();
        
        Report.error(expr.position, "Access for " + expr.getClass().getSimpleName() + " not found.");
        return null;
    }

    private Type getType(Ast expr) {
        var type = this.types.valueFor(expr);

        if (type.isPresent()) 
            return type.get();
        
        Report.error(expr.position, "Type for " + expr.getClass().getSimpleName() + " not found.");
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

        var frame = this.getFrame(call);
        var args = new ArrayList<IRExpr>();

        for (Expr arg: call.arguments) {
            var argCode = this.getIRNode(arg);
            args.add((IRExpr) argCode);
        }

        var code = new CallExpr(frame.label, args);
        imcCode.store(code, call);
    }

    @Override
    public void visit(Binary binary) {

        var lhs = getIRNode(binary.left);
        var rhs = getIRNode(binary.right);

        var Lmem = new MemExpr((IRExpr) lhs);
        var Rmem = new MemExpr((IRExpr) rhs);

        if (binary.operator == compiler.parser.ast.expr.Binary.Operator.ASSIGN) {
            
            var move = new MoveStmt((IRExpr) lhs, Rmem);
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
        for (Expr expr: block.expressions) {
           var code = this.getIRNode(expr);
           imcCode.store(code, block); 
        }
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
        var definition = this.getDefinition(name);
        var access = this.getAccess(definition);
       
        if (access instanceof Global) {
            var code = new NameExpr(((Global) access).label);
            this.imcCode.store(code, name); 

            var globalChunk = new Chunk.GlobalChunk((Global) access);
            this.chunks.add(globalChunk);
        }
        // local or parameter
        else {

            var dostop = (Local) access;
            var offset = new ConstantExpr(dostop.offset);

            // TODO nevem kako bi dubu framepointer, Temp rabs uporabljat
            IRExpr FP = new ConstantExpr(0);

            var op = Operator.SUB;
            if (access instanceof Local) 
                op = Operator.ADD;

            var code = new BinopExpr(FP, offset, op);

            this.imcCode.store(code, name); 
        }
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
        var condition = getIRNode(ifThenElse.condition);

        var cjump = new CJumpStmt((IRExpr) condition, L1.label, L2.label);
        stavki.add(cjump);

        // then
        stavki.add(L1);

        var thenExpr = getIRNode(ifThenElse.thenExpression);
        stavki.add(new ExpStmt((IRExpr) thenExpr));

        // else
        stavki.add(L2);

        if (ifThenElse.elseExpression.isPresent()) {

            var elseExpr = this.getIRNode(ifThenElse.elseExpression.get());
            stavki.add(new ExpStmt((IRExpr) elseExpr));
        }

        var code = new SeqStmt(stavki);
        imcCode.store(code, ifThenElse);
    }

    @Override
    public void visit(Literal literal) {
        var type = this.getType(literal);
        
        IRNode code;
        if (type.isInt())
            code = new ConstantExpr(Integer.parseInt(literal.value));
        else if (type.isLog()) {
            if (literal.value == "true")
                code = new ConstantExpr(1);
            else 
                code = new ConstantExpr(0);
        }
        else
        // else if (type.get().isStr())
            code = new ConstantExpr(0);
         
            
        imcCode.store(code, literal);
    }

    @Override
    public void visit(Unary unary) {       

        var lhs = new ConstantExpr(0);
        var rhs = getIRNode(unary.expr);

        Operator op = Operator.valueOf(unary.operator.name()); 

        var code = new BinopExpr(lhs, (IRExpr) rhs, op);
        imcCode.store(code, unary);
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
        var condition = this.getIRNode(whileLoop.condition);

        var cjump = new CJumpStmt((IRExpr) condition, L1.label, L2.label);
        stavki.add(cjump);

        // then
        stavki.add(L1);

        var body = this.getIRNode(whileLoop.body);
        stavki.add(new ExpStmt((IRExpr) body));

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
        var body = this.getIRNode(funDef.body);

        var frame = this.getFrame(funDef);

        if (body instanceof IRExpr) {
            var stmt = new ExpStmt((IRExpr) body);
            var chunk = new Chunk.CodeChunk(frame, stmt);
    
            this.chunks.add(chunk);
        }
        else {
            var chunk = new Chunk.CodeChunk(frame, (IRStmt) body);    
            this.chunks.add(chunk);
        }
 
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
