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

    public int parentStaticLevel = 1;


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

    private IRExpr getMem(IRExpr expr) {
        if (expr instanceof ConstantExpr) 
            return expr;        
        if (expr instanceof BinopExpr) {
            var leva = ((BinopExpr) expr).lhs;
            if (leva instanceof NameExpr)
                return new MemExpr(expr);
            return expr;
        }
        if (expr instanceof NameExpr) 
            return new MemExpr(expr);        
        if (expr instanceof TempExpr) 
            return new MemExpr(expr);        
        if (expr instanceof CallExpr) 
            return new MemExpr(expr);
        
        System.out.println("Unknown Class:" + expr.getClass().getSimpleName());
        return null;
    }

    // DONE mogoce sam ta mem ne vem fix pa prvi argument je verjetno narobe
    @Override
    public void visit(Call call) {

        var frame = this.getFrame(call);
        var args = new ArrayList<IRExpr>();           
    
        var FP = NameExpr.FP();

        //System.out.println(this.parentStaticLevel);
        //System.out.println(frame.staticLevel);
       

        int SLdiff = Math.abs(this.parentStaticLevel - frame.staticLevel);
        
        // add SP
        if (frame.staticLevel == 1) {
            args.add(FP);
        }
        else if (SLdiff == 0) {
            var mem = new MemExpr(FP);
            args.add(mem);
        }
        else if (SLdiff == 1) {
            args.add(FP);
        }
        else //if (SLdiff > 1) 
        {
            var multipleMEM = new MemExpr(FP);
            while (SLdiff > 0) {
                multipleMEM = new MemExpr(multipleMEM);
                SLdiff--;
            }
            args.add(multipleMEM);
        }

        for (Expr arg: call.arguments) {
            var argCode = this.getIRNode(arg);
            args.add((IRExpr) argCode);
        }

        var label = frame.label;
        if (call.name.equals(Constants.printIntLabel) || call.name.equals(Constants.printLogLabel) || call.name.equals(Constants.printStringLabel) || call.name.equals(Constants.randIntLabel) || call.name.equals(Constants.seedLabel))
            label = Label.named(call.name);

        var callExpr = new CallExpr(label, args);

        var eseq = new EseqExpr(
            new MoveStmt(
                new MemExpr(
                    new BinopExpr(
                        NameExpr.SP(), 
                        new ConstantExpr(frame.oldFPOffset()), 
                        Operator.SUB)
                ), NameExpr.FP())
            , callExpr);

        imcCode.store(eseq, call);
    }

    // TODO zrihti mem
    @Override
    public void visit(Binary binary) {

        var lhs = getIRNode(binary.left);
        var rhs = getIRNode(binary.right);

        var Lmem = this.getMem((IRExpr) lhs);
        //var Rmem = this.getMem((IRExpr) rhs);

        if (binary.operator == compiler.parser.ast.expr.Binary.Operator.ASSIGN) {            
            
            //  pri move je levi vedno mem sam odruge stvari pomen
            var move = new MoveStmt(Lmem, (IRExpr) rhs);
            // result je mem od vrednosti kamor si glih kar nalozu vrednost
            var code = new EseqExpr(move, Lmem);
            imcCode.store(code, binary);  
        }
        else if (binary.operator == compiler.parser.ast.expr.Binary.Operator.ARR) {           

            // get naslov * type
            var typeConst = new ConstantExpr(this.getType(binary.left).asArray().get().elementSizeInBytes());
            var indexTypeMultipliyer = new BinopExpr((IRExpr) rhs, typeConst, Operator.MUL);

            var code = new BinopExpr((IRExpr) lhs, indexTypeMultipliyer, Operator.ADD);

            // mem ali ne mem
            imcCode.store(new MemExpr(code), binary);
        }
        else {      
            Operator op = Operator.valueOf(binary.operator.name());
            var code = new BinopExpr((IRExpr) lhs, (IRExpr) rhs, op);
            imcCode.store(code, binary);        
        }
    }

    // DONE
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

    // DONE
    @Override
    public void visit(For forLoop) {
                 
        var stavki = new ArrayList<IRStmt>();

        var L0 = new LabelStmt(Label.nextAnonymous());
        var L1 = new LabelStmt(Label.nextAnonymous());
        var L2 = new LabelStmt(Label.nextAnonymous());

        // i
        var counter = this.getIRNode(forLoop.counter);
        var CounterMem = this.getMem((IRExpr) counter);

        // 0
        var low = this.getIRNode(forLoop.low);   
        var LowMem = this.getMem((IRExpr) low);                      

        // assign i = 0
        var move = new MoveStmt(CounterMem, LowMem);
        stavki.add(move);

        // start
        stavki.add(L0);

        // 10
        var high = this.getIRNode(forLoop.high);
        var HighMem = this.getMem((IRExpr) high);     

        // condition
        var condition = new BinopExpr(CounterMem, HighMem, Operator.LT);
        var cjump = new CJumpStmt(condition, L1.label, L2.label);
        stavki.add(cjump);        

        // body        
        stavki.add(L1);

        var body = this.getIRNode(forLoop.body);
        stavki.add(new ExpStmt((IRExpr) body));

        //step
        var step = this.getIRNode(forLoop.step);
        var StepMem = this.getMem((IRExpr) step);

        var binStep = new BinopExpr(CounterMem, StepMem, Operator.ADD);

        var counterPlusStep = new MoveStmt(CounterMem, binStep);
        stavki.add(counterPlusStep);

        var jump = new JumpStmt(L0.label);
        stavki.add(jump);

        // out
        stavki.add(L2);

        var seq = new SeqStmt(stavki);
        var code = new EseqExpr(seq, new ConstantExpr(-100));
        imcCode.store(code, forLoop);
    }

    // DONE
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

    // DONE
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
        var code = new EseqExpr(seq, new ConstantExpr(-99));
        imcCode.store(code, ifThenElse);
    }

    // DONE
    @Override
    public void visit(Literal literal) {
        var type = this.getType(literal);
        
        IRNode code = null;
        if (type.isInt())
            code = new ConstantExpr(Integer.parseInt(literal.value));
        else if (type.isLog()) {
            // optimizeri tko k za int
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
        imcCode.store(code, literal);
    }

    // DONE
    @Override
    public void visit(Unary unary) {       

        var lhs = new ConstantExpr(0);
        var rhs = getIRNode(unary.expr);

        Operator op = Operator.valueOf(unary.operator.name()); 

        var code = new BinopExpr(lhs, (IRExpr) rhs, op);
        imcCode.store(code, unary);
    }

    // DONE razn return
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
        var code = new EseqExpr(seq, new ConstantExpr(-98));
        imcCode.store(code, whileLoop);
    }

    // DONE bi reku
    @Override
    public void visit(Where where) {

        this.visit(where.defs);

        var body = this.getIRNode(where.expr);
        imcCode.store((IRExpr) body, where);
    }

    // DONE
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

    // DONE
    @Override
    public void visit(FunDef funDef) {

        var body = (IRExpr) this.getIRNode(funDef.body);
        var frame = this.getFrame(funDef);

        this.parentStaticLevel = frame.staticLevel;

        // more shrant vrednost
         // mnde mors narest mem na FP ker ma move vedno mem
        // tuki se se shrani current frame da lahko razliko izracunas callu
        var FP = new MemExpr(NameExpr.FP());
        var result = new MoveStmt(FP, body);

        var chunk = new Chunk.CodeChunk(frame, result);    
        this.chunks.add(chunk);
    }

    // DONE
    @Override
    public void visit(TypeDef typeDef) {
        // /
    }
 
    // DONE
    @Override
    public void visit(VarDef varDef) {

        if (this.definitions.valueFor(varDef).isPresent()) {

            var definition = this.getDefinition(varDef);
            var access = this.getAccess(definition);
        
            if (access instanceof Global) {
                var code = new NameExpr(((Global) access).label);
                this.imcCode.store(code, varDef); 

                var globalChunk = new Chunk.GlobalChunk((Global) access);
                this.chunks.add(globalChunk);
            }
        }
    }

    // DONE
    @Override
    public void visit(Parameter parameter) {
        // /
    }

    // DONE
    @Override
    public void visit(Array array) {
        // /
        System.out.println("array");
    }

    // DONE
    @Override
    public void visit(Atom atom) {
        // /
        System.out.println("atom");
    }

    // DONE
    @Override
    public void visit(TypeName name) {
        // /
        System.out.println("name");
    }
}
