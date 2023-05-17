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
    
        var offset = new ConstantExpr(-4);
        var FP = NameExpr.FP();
        var bin = new BinopExpr(FP, offset, Operator.ADD);
        var mem = new MemExpr(bin);
        args.add(mem);

        for (Expr arg: call.arguments) {
            var argCode = this.getIRNode(arg);
            var argMem = new MemExpr((IRExpr) argCode);
            args.add(argMem);
        }

        var label = frame.label;
        if (call.name.equals(Constants.printIntLabel) || call.name.equals(Constants.printLogLabel) || call.name.equals(Constants.printStringLabel) || call.name.equals(Constants.randIntLabel) || call.name.equals(Constants.seedLabel))
        label = Label.named(call.name);

        var code = new CallExpr(label, args);
        imcCode.store(code, call);
    }

    @Override
    public void visit(Binary binary) {

        var lhs = getIRNode(binary.left);
        var rhs = getIRNode(binary.right);

        var Lmem = new MemExpr((IRExpr) lhs);
        //var Rmem = new MemExpr((IRExpr) rhs);

        if (binary.operator == compiler.parser.ast.expr.Binary.Operator.ASSIGN) {            
            
            // brez mem
            var move = new MoveStmt((IRExpr) lhs, (IRExpr) rhs);
            // result je mem od vrednosti kamor si glih kar nalozu vrednost
            var code = new EseqExpr(move, Lmem);
            imcCode.store(code, binary);  
        }
        else if (binary.operator == compiler.parser.ast.expr.Binary.Operator.ARR) {           

            // get naslov * type
            var typeConst = new ConstantExpr(this.getType(binary.left).asArray().get().elementSizeInBytes());
            var indexTypeMultipliyer = new BinopExpr((IRExpr) rhs, typeConst, Operator.MUL);

            var code = new BinopExpr((IRExpr) lhs, indexTypeMultipliyer, Operator.ADD);
            imcCode.store(code, binary);  
        }
        else {      
            Operator op = Operator.valueOf(binary.operator.name());
            var code = new BinopExpr((IRExpr) lhs, (IRExpr) rhs, op);
            imcCode.store(code, binary);        
        }
    }

    @Override
    public void visit(Block block) {

        var statements = new ArrayList<IRStmt>();
        IRExpr result = null;

        for (Expr expr: block.expressions) {
           var expression = this.getIRNode(expr);
           statements.add(new ExpStmt((IRExpr) expression));
           result = (IRExpr) expression;
        }

        var seqStmt = new SeqStmt(statements);
        var code = new EseqExpr(seqStmt, result);
        imcCode.store(code, block);
    }

    @Override
    public void visit(For forLoop) {
                 
        var stavki = new ArrayList<IRStmt>();

        var L0 = new LabelStmt(Label.nextAnonymous());
        var L1 = new LabelStmt(Label.nextAnonymous());
        var L2 = new LabelStmt(Label.nextAnonymous());

        // i
        var counter = this.getIRNode(forLoop.counter);
        var CounterMem = new MemExpr((IRExpr) counter);

        // 0
        var low = this.getIRNode(forLoop.low);  
        var LowMem = new MemExpr((IRExpr) low);      
          

        // 10
        var high = this.getIRNode(forLoop.high);
        var HighMem = new MemExpr((IRExpr) high);             

        // assign i = 0
        var move = new MoveStmt(CounterMem, LowMem);
        stavki.add(0, move);

        // start
        stavki.add(1, L0);

        // condition        

        var condition = new BinopExpr(CounterMem, HighMem, Operator.LEQ);
        var cjump = new CJumpStmt(condition, L1.label, L2.label);
        stavki.add(cjump);        

        // body        
        stavki.add(L1);

        var body = this.getIRNode(forLoop.body);
        stavki.add(new ExpStmt((IRExpr) body));

        //step
        var step = this.getIRNode(forLoop.step);
        var StepMem = new MemExpr((IRExpr) step);

        var binStep = new BinopExpr(CounterMem, StepMem, Operator.ADD);

        var counterPlusStep = new MoveStmt(CounterMem, binStep);
        stavki.add(counterPlusStep);

        var jump = new JumpStmt(L0.label);
        stavki.add(jump);

        // out
        stavki.add(L2);

        var seq = new SeqStmt(stavki);
        var code = new EseqExpr(seq, new ConstantExpr(-99));
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
        // local
        else if (access instanceof Local){

            var dostop = (Local) access;
            var offset = new ConstantExpr(dostop.offset);            
            var FP = NameExpr.FP();           

            var code = new BinopExpr(FP, offset, Operator.ADD);

            this.imcCode.store(code, name); 
        }
        else {
            var dostop = (compiler.frm.Access.Parameter) access;
            var offset = new ConstantExpr(dostop.offset);            
            var FP = NameExpr.FP();           

            var code = new BinopExpr(FP, offset, Operator.ADD);

            this.imcCode.store(code, name); 
        }
    }

    @Override
    public void visit(IfThenElse ifThenElse) {
                
        var stavki = new ArrayList<IRStmt>();
        
        var L1 = new LabelStmt(Label.nextAnonymous());
        var L2 = new LabelStmt(Label.nextAnonymous());
        var L3 = new LabelStmt(Label.nextAnonymous());

        // condition
        var condition = getIRNode(ifThenElse.condition);

        var cjump = new CJumpStmt((IRExpr) condition, L1.label, L2.label);
        stavki.add(cjump);

        // then
        stavki.add(L1);

        var thenExpr = getIRNode(ifThenElse.thenExpression);
        stavki.add(new ExpStmt((IRExpr) thenExpr));

        var jump = new JumpStmt(L3.label);
        stavki.add(jump);

        // else
        stavki.add(L2);

        if (ifThenElse.elseExpression.isPresent()) {

            var elseExpr = this.getIRNode(ifThenElse.elseExpression.get());
            stavki.add(new ExpStmt((IRExpr) elseExpr));
        }

        stavki.add(L3);

        var seq = new SeqStmt(stavki);
        var code = new EseqExpr(seq, new ConstantExpr(0));
        imcCode.store(code, ifThenElse);
    }

    @Override
    public void visit(Literal literal) {
        var type = this.getType(literal);
        
        IRNode code = null;
        if (type.isInt())
            code = new ConstantExpr(Integer.parseInt(literal.value));
        else if (type.isLog()) {
            if (literal.value.equals("true"))
                code = new ConstantExpr(1);
            else 
                code = new ConstantExpr(0);
        }
        else if (type.isStr()) {
            var stringLabel = Label.nextAnonymous();
            var access = new Access.Global(4, stringLabel);

            code = new NameExpr(stringLabel);

            var dataChunk = new Chunk.DataChunk(access, literal.value);
            this.chunks.add(dataChunk);
        }  
        // else if (type.isArray()) {
        //     Type.Array arr = (Type.Array) type;

        //     arr.
        // }
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

        var jump = new JumpStmt(L0.label);
        stavki.add(jump);

        // else
        stavki.add(L2);

        var seq = new SeqStmt(stavki);
        var code = new EseqExpr(seq, new ConstantExpr(0));
        imcCode.store(code, whileLoop);
    }

    @Override
    public void visit(Where where) {

        this.visit(where.defs);

        var body = this.getIRNode(where.expr);
        imcCode.store((IRExpr) body, where);
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

        var body = (IRExpr) this.getIRNode(funDef.body);
        var bodyExpr = new ExpStmt(body);
        var frame = this.getFrame(funDef);

        // more shrant vrednost
        //var FP = NameExpr.FP();
        //var result = new MoveStmt(FP, body);
        //imcCode.store(new EseqExpr(result, body), funDef);

        var chunk = new Chunk.CodeChunk(frame, bodyExpr);    
        this.chunks.add(chunk);
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
        // /
    }
}
