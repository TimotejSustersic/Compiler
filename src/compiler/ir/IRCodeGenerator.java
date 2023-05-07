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

        Operator op = Operator.valueOf(binary.operator.name());

        this.parseExpr(binary.left);
        this.parseExpr(binary.right);

        var lhs = imcCode.valueFor(binary.left);
        var rhs = imcCode.valueFor(binary.right);

        if (lhs.isPresent() && rhs.isPresent()) {
            var code = new BinopExpr((IRExpr) lhs.get(), (IRExpr) rhs.get(), op);
            imcCode.store(code, binary);
        }
        else 
            Report.error(binary.position, "Binary expression isn't present");
    }

    @Override
    public void visit(Block block) {
        for (Expr expr: block.expressions)
           this.parseExpr(expr);
    }

    @Override
    public void visit(For forLoop) {
        // TODO Auto-generated method stub
        throw new UnsupportedOperationException("Unimplemented method 'visit'");
    }

    @Override
    public void visit(Name name) {
        // TODO Auto-generated method stub
        throw new UnsupportedOperationException("Unimplemented method 'visit'");
    }

    @Override
    public void visit(IfThenElse ifThenElse) {
        // TODO Auto-generated method stub
        throw new UnsupportedOperationException("Unimplemented method 'visit'");
    }

    @Override
    public void visit(Literal literal) {
        // TODO Auto-generated method stub
        throw new UnsupportedOperationException("Unimplemented method 'visit'");
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
        // TODO Auto-generated method stub
        throw new UnsupportedOperationException("Unimplemented method 'visit'");
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
