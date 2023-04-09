/**
 * @ Author: turk
 * @ Description: Preverjanje tipov.
 */

package compiler.seman.type;

import static common.RequireNonNull.requireNonNull;

import common.Report;
import compiler.common.Visitor;
import compiler.parser.ast.Ast;
import compiler.parser.ast.def.*;
import compiler.parser.ast.def.FunDef.Parameter;
import compiler.parser.ast.expr.*;
import compiler.parser.ast.type.*;
import compiler.seman.common.NodeDescription;
import compiler.seman.type.type.Type;

public class TypeChecker implements Visitor {
    /**
     * Opis vozlišč in njihovih definicij.
     * te definicije ze pridejo iz name chekerja
     */
    private final NodeDescription<Def> definitions;

    /**
     * Opis vozlišč, ki jim priredimo podatkovne tipe.
     * vsakmu tipu dodamo nek tip, ki ga dobimo iz otroka al pa iz definicij
     */
    private NodeDescription<Type> types;

    public TypeChecker(NodeDescription<Def> definitions, NodeDescription<Type> types) {
        requireNonNull(definitions, types);
        this.definitions = definitions;
        this.types = types;
    }

    // DFS
    private boolean isLoop() {
        return false;
    }

    private Type getType(Ast ast) {
        var type = this.types.valueFor(ast);
        if (type.isPresent())
            return type.get();
        else 
            Report.error(ast.position, "Type isn't present.");
        return null;
    }
    
    private void typeExpr(Expr expr) {
        //System.out.println(expr.getClass());
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
        else {
            System.out.println(expr.getClass());
            Report.error("Wrong expression class instance in naming.");
        }
    }
    
    private void typeType(compiler.parser.ast.type.Type type) {
        if (type instanceof TypeName)
            this.visit((TypeName) type);
        else if (type instanceof Array)
            this.visit((Array) type);
        else {
            this.visit((Atom) type);
        }
    }

    @Override
    public void visit(Defs defs) {

        for (Def def: defs.definitions) {
            if (def instanceof TypeDef) 
                this.visit((TypeDef) def);            
            else if (def instanceof FunDef) 
                this.visit((FunDef) def);            
            else if (def instanceof VarDef) 
                this.visit((VarDef) def);
            else 
                Report.error("Wrong definition class in type checker");
        }
    }

    // DONE
    @Override
    public void visit(FunDef funDef) {

        for (Parameter parameter: funDef.parameters)
            visit(parameter);
    
        this.typeType(funDef.type);
        this.typeExpr(funDef.body);

        var funType = this.getType(funDef.type);
        var bodyType = this.getType(funDef.body);

        if (funType.equals(bodyType))
            this.types.store(funType, funDef);
        else
            Report.error(funDef.position, "Fun.body and fun.type isn't equal.");
    }

    @Override
    public void visit(TypeDef typeDef) {
        this.typeType(typeDef.type);

        this.types.store(this.types.valueFor(typeDef.type).get(), typeDef);
    }

    @Override
    public void visit(VarDef varDef) {
        typeType(varDef.type);

        this.types.store(this.getType(varDef.type), varDef);
    }

    @Override
    public void visit(Parameter parameter) {
        typeType(parameter.type);
        this.types.store(this.getType(parameter.type), parameter);
    }

    @Override
    public void visit(Call call) {
        // TODO Auto-generated method stub
        throw new UnsupportedOperationException("Unimplemented method 'visit'");
    }

    @Override
    public void visit(Binary binary) {
        this.typeExpr(binary.left);
        this.typeExpr(binary.right);

        var leftType = this.types.valueFor(binary.left);
        var rightType = this.types.valueFor(binary.right);

        if (leftType.isPresent() && rightType.isPresent()) 
            if (leftType.get().equals(rightType.get()))
                this.types.store(leftType.get(), binary);
            else
                Report.error(binary.position, "Binary type isn't equal.");        
        else 
            Report.error(binary.position, "Binary Type doesn't exists.");        
    }

    @Override
    public void visit(Block block) {
        Type exprType = null;
        for (var expr: block.expressions) {
            this.typeExpr(expr);
            exprType = this.types.valueFor(expr).get();
            this.types.store(exprType, expr); 
        }
        this.types.store(exprType, block);     
    }

    @Override
    public void visit(For forLoop) {
        // TODO Auto-generated method stub
        throw new UnsupportedOperationException("Unimplemented method 'visit'");
    }

    @Override
    public void visit(Name name) {

        var def = this.definitions.valueFor(name);
        var type = this.types.valueFor(def.get());
        if (def.isPresent()) 
            if (type.isPresent()) 
                this.types.store(type.get(), name);       
            else
                Report.error(name.position, "TypName Type doesn't exist.");
        else
            Report.error(name.position, "TypName Definition doesn't exist.");
    }

    @Override
    public void visit(IfThenElse ifThenElse) {
        // TODO Auto-generated method stub
        throw new UnsupportedOperationException("Unimplemented method 'visit'");
    }

    @Override
    public void visit(Literal literal) {
        Type type = new Type.Atom(Type.Atom.Kind.LOG);
        if (literal.type == compiler.parser.ast.type.Atom.Type.INT) 
            type = new Type.Atom(Type.Atom.Kind.INT);
        else if (literal.type == compiler.parser.ast.type.Atom.Type.STR) 
            type = new Type.Atom(Type.Atom.Kind.STR);
        //else if (atom.type == compiler.parser.ast.type.Atom.Type.LOG) 
           // type = new Type.Atom(Type.Atom.Kind.LOG);
        
        this.types.store(type, literal);
    }

    @Override
    public void visit(Unary unary) {
        // TODO Auto-generated method stub
        throw new UnsupportedOperationException("Unimplemented method 'visit'");
    }

    @Override
    public void visit(While whileLoop) {
        // TODO Auto-generated method stub
        throw new UnsupportedOperationException("Unimplemented method 'visit'");
    }

    @Override
    public void visit(Where where) {
        // TODO Auto-generated method stub
        throw new UnsupportedOperationException("Unimplemented method 'visit'");
    }

    @Override
    public void visit(Array array) {
        // TODO Auto-generated method stub
        throw new UnsupportedOperationException("Unimplemented method 'visit'");
    }

    // DONE
    @Override
    public void visit(Atom atom) {
        Type type = new Type.Atom(Type.Atom.Kind.LOG);
        if (atom.type == compiler.parser.ast.type.Atom.Type.INT) 
            type = new Type.Atom(Type.Atom.Kind.INT);
        else if (atom.type == compiler.parser.ast.type.Atom.Type.STR) 
            type = new Type.Atom(Type.Atom.Kind.STR);
        //else if (atom.type == compiler.parser.ast.type.Atom.Type.LOG) 
           // type = new Type.Atom(Type.Atom.Kind.LOG);
        
        this.types.store(type, atom);
    }

    // DONE
    @Override
    public void visit(TypeName name) {
        
        var def = this.definitions.valueFor(name);
        var type = this.types.valueFor(def.get());
        if (def.isPresent()) 
            if (type.isPresent()) 
                this.types.store(type.get(), name);       
            else
                Report.error(name.position, "TypName Type doesn't exist.");
        else
            Report.error(name.position, "TypName Definition doesn't exist.");
    }
}
