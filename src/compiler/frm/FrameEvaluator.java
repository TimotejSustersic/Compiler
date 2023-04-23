/**
 * @ Author: turk
 * @ Description: Analizator klicnih zapisov.
 */

package compiler.frm;

import static common.RequireNonNull.requireNonNull;

import java.util.Stack;

import compiler.common.Visitor;
import compiler.frm.Frame.Builder;
import compiler.frm.Frame.Label;
import compiler.parser.ast.def.*;
import compiler.parser.ast.def.FunDef.Parameter;
import compiler.parser.ast.expr.*;
import compiler.parser.ast.type.Array;
import compiler.parser.ast.type.Atom;
import compiler.parser.ast.type.TypeName;
import compiler.seman.common.NodeDescription;
import compiler.seman.type.type.Type;

public class FrameEvaluator implements Visitor {

    int staticLevel = 1;

    /**
     * Stack builderjev, ker builder ne mores zaklucit dokler ni konc klica zato mas stack
    */
    private Stack<Builder> builders;


    /**
     * Opis definicij funkcij in njihovih klicnih zapisov.
     */
    private NodeDescription<Frame> frames;

    /**
     * Opis definicij spremenljivk in njihovih dostopov.
     */
    private NodeDescription<Access> accesses;

    /**
     * Opis vozlišč in njihovih definicij.
     */
    private final NodeDescription<Def> definitions;

    /**
     * Opis vozlišč in njihovih podatkovnih tipov.
     */
    private final NodeDescription<Type> types;

    public FrameEvaluator(
        NodeDescription<Frame> frames, 
        NodeDescription<Access> accesses,
        NodeDescription<Def> definitions,
        NodeDescription<Type> types
    ) {
        requireNonNull(frames, accesses, definitions, types);
        this.frames = frames;
        this.accesses = accesses;
        this.definitions = definitions;
        this.types = types;
    }

    private void addAccess(Def def) {
        Type type = this.types.valueFor(def).get();

        // global
        if (this.staticLevel == 1) {
            Label label = Label.named(def.name);
            var access = new Access.Global(type.sizeInBytes(), label);
            this.accesses.store(access, def);
        }
        // lokal
        else {
            var access = new Access.Local(type.sizeInBytes(), 4, this.staticLevel);
            this.accesses.store(access, def);
        }
    }

    private void proccesExpr(Expr expr) {
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

    @Override
    public void visit(Defs defs) {

        for (Def def: defs.definitions) {
            if (def instanceof TypeDef) 
                this.visit((TypeDef) def);                     
            else if (def instanceof VarDef) 
                this.visit((VarDef) def);
            else if (def instanceof FunDef) 
                this.visit((FunDef) def);
        }
    }


    @Override
    public void visit(FunDef funDef) {
        this.addAccess(funDef);
        var builder = new Frame.Builder(Label.named(funDef.name), this.staticLevel);

        this.staticLevel++;

        for (Parameter param : funDef.parameters) {
            Type type = this.types.valueFor(param).get();
            builder.addParameter(type.sizeInBytesAsParam());
            var access = new Access.Parameter(type.sizeInBytesAsParam(), 4, this.staticLevel);
            this.accesses.store(access, param);
            
        }

        this.proccesExpr(funDef.body);

        this.frames.store(builder.build(), funDef);

        this.staticLevel--;
    }


    @Override
    public void visit(TypeDef typeDef) {
       this.addAccess(typeDef);
    }


    @Override
    public void visit(VarDef varDef) {
        this.addAccess(varDef);
    }


    @Override
    public void visit(Call call) {
        // call can be multiple so they have next anoynimus label
        var builder = new Frame.Builder(Label.nextAnonymous(), this.staticLevel);

        for (Expr arg: call.arguments) {
            this.proccesExpr(arg);
            builder.addParameter(this.types.valueFor(arg).get().sizeInBytes());
        }

        this.frames.store(builder.build(), call);
    }


    @Override
    public void visit(Binary binary) {
        this.proccesExpr(binary.left);
        this.proccesExpr(binary.right);
    }


    @Override
    public void visit(Block block) {
        this.staticLevel++;

        for (Expr expr : block.expressions) 
            this.proccesExpr(expr);

        this.staticLevel--;
    }


    @Override
    public void visit(For forLoop) {
        this.proccesExpr(forLoop.body);        
    }


    @Override
    public void visit(Name name) {
        
    }


    @Override
    public void visit(IfThenElse ifThenElse) {
        this.proccesExpr(ifThenElse.condition);
        this.proccesExpr(ifThenElse.thenExpression);

        if (ifThenElse.elseExpression.isPresent())
            this.proccesExpr(ifThenElse.elseExpression.get());
    }


    @Override
    public void visit(Literal literal) {
        
    }


    @Override
    public void visit(Unary unary) {
        this.proccesExpr(unary.expr);
    }


    @Override
    public void visit(While whileLoop) {
        this.proccesExpr(whileLoop.condition);
        this.proccesExpr(whileLoop.body);
    }


    @Override
    public void visit(Where where) {
        this.staticLevel++;

        this.proccesExpr(where.expr);
        visit((Defs) where.defs);

        this.staticLevel--;
    }

    @Override
    public void visit(Parameter parameter) {
        this.addAccess(parameter);
    }


    @Override
    public void visit(Array array) {
        
    }


    @Override
    public void visit(Atom atom) {
        
    }


    @Override
    public void visit(TypeName name) {
        
    }
}
