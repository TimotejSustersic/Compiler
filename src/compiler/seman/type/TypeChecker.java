/**
 * @ Author: turk
 * @ Description: Preverjanje tipov.
 */

package compiler.seman.type;

import static common.RequireNonNull.requireNonNull;

import java.util.ArrayList;

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

    //private Ast recursionNode = null;

    public TypeChecker(NodeDescription<Def> definitions, NodeDescription<Type> types) {
        requireNonNull(definitions, types);
        this.definitions = definitions;
        this.types = types;
    }

    private Type getType(Ast ast) {
        // if (this.recursionNode != null)
        //     if (this.recursionNode == ast) {
        //         Report.error(ast.position, "Recursion in finding type.");
        //         return null;
        //     }

        var type = this.types.valueFor(ast);
        if (type.isPresent()) {
            var typeGet = type.get();
            if (typeGet.isFunction())
                typeGet = typeGet.asFunction().get().returnType;
            //else if (typeGet.isArray())
                //typeGet = typeGet.asArray().get().type;
                
            //this.recursionNode = null;
            return typeGet;
        }
        else {
            //this.recursionNode = ast;
            var def = new ArrayList<Def>();
            def.add((Def) ast);
            visit(new Defs(ast.position, def));

            return this.getType(ast);
        } 
    }
    
    private void typeExpr(Expr expr) {
        
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
        else 
            Report.error("Wrong expression class instance in naming.");        
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
            //if (def instanceof TypeDef) 
                //this.visit((TypeDef) def);                     
            //else if (def instanceof VarDef) 
                //this.visit((VarDef) def);
            if (def instanceof FunDef) {

                var listParams = new ArrayList<Type>();

                for (Parameter parameter: ((FunDef) def).parameters) {
                    visit(parameter);
                    listParams.add(this.getType(parameter));
                }
        
                this.typeType(((FunDef) def).type);

                Type funType = new Type.Function(listParams, this.getType(((FunDef) def).type));
                
                this.types.store(funType, ((FunDef) def));
            }
        }

        for (Def def: defs.definitions) {
            if (def instanceof TypeDef) 
                this.visit((TypeDef) def);                     
            else if (def instanceof VarDef) 
                this.visit((VarDef) def);
            else if (def instanceof FunDef) 
                this.visit((FunDef) def); 
            else 
                Report.error("Wrong definition class in type checker");
        }
    }

    // DONE
    @Override
    public void visit(FunDef funDef) {

        this.typeExpr(funDef.body);

        var funType = this.getType(funDef.type);
        var bodyType = this.getType(funDef.body);

        if (!(funType.equals(bodyType)))
            Report.error(funDef.position, "Fun.body and fun.type isn't equal.");
    }

    @Override
    public void visit(TypeDef typeDef) {
        this.typeType(typeDef.type);

        // if (this.recursionNode != null) {
        //     System.out.println(recursionNode.toString());
        //     Report.error(typeDef.position, "Recursion.");
        //     if (this.recursionNode == typeDef.type) {
        //         Report.error(typeDef.position, "Recursion in finding type.");
        //     }
        // }

        this.types.store(this.getType(typeDef.type), typeDef);
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

        var def = this.definitions.valueFor(call);       
        
        if (def.isPresent()) {
            var type = this.getType(def.get());

            // rule 8.
            this.types.store(type, call);

            for (var expr: call.arguments)
                this.typeExpr(expr);
        }
        else
            Report.error(call.position, "Call Definition doesn't exist.");
    }

    @Override
    public void visit(Binary binary) {
        this.typeExpr(binary.left);
        this.typeExpr(binary.right);

        var leftType = this.getType(binary.left);
        var rightType = this.getType(binary.right);

        var intType = new Type.Atom(Type.Atom.Kind.INT);
        //var strType = new Type.Atom(Type.Atom.Kind.STR);
        var logType = new Type.Atom(Type.Atom.Kind.LOG);
        //var voidType = new Type.Atom(Type.Atom.Kind.VOID);

        // rule 7.
        if (leftType.isArray() && rightType.isInt()) 
            this.types.store(leftType.asArray().get().type, binary);        
        // rule 4.
        else if (leftType.isLog() && rightType.isLog() &&  (
           binary.operator == Binary.Operator.AND
        || binary.operator == Binary.Operator.OR
        ))
            this.types.store(logType, binary);
        // rule 5.
        else if (leftType.isInt() && rightType.isInt() && (
           binary.operator == Binary.Operator.ADD
        || binary.operator == Binary.Operator.SUB
        || binary.operator == Binary.Operator.MUL
        || binary.operator == Binary.Operator.DIV
        || binary.operator == Binary.Operator.MOD
        ))
            this.types.store(intType, binary);
        // rule 6.
        else if (
           leftType.isInt() 
        || leftType.isLog() 
        || rightType.isInt() 
        || rightType.isLog() 
            && (
           binary.operator == Binary.Operator.EQ
        || binary.operator == Binary.Operator.NEQ
        || binary.operator == Binary.Operator.LT
        || binary.operator == Binary.Operator.LEQ
        || binary.operator == Binary.Operator.GT
        || binary.operator == Binary.Operator.GEQ 
        )) 
            this.types.store(logType, binary);
        // rule 10.
        else if (leftType.equals(rightType) && binary.operator == Binary.Operator.ASSIGN )
            this.types.store(leftType, binary);
        else
            Report.error(binary.position, "Binary type missmatch."); 
    }

    @Override
    public void visit(Block block) {
        
        Type exprType = null;
        for (var expr: block.expressions) {
            this.typeExpr(expr);
            exprType = this.getType(expr);

            // rule 13.
            this.types.store(exprType, expr); 
        }
        // probi ce se da brez
        this.types.store(exprType, block);     
    }

    @Override
    public void visit(For forLoop) {
        
        this.visit((Name) forLoop.counter);
        this.typeExpr(forLoop.low);
        this.typeExpr(forLoop.high);
        this.typeExpr(forLoop.step);        
        this.typeExpr(forLoop.body); 

        // rule 12.
        if (this.getType(forLoop.counter).isInt()
        && this.getType(forLoop.low).isInt()
        && this.getType(forLoop.high).isInt()
        && this.getType(forLoop.step).isInt()
        ) 
            this.types.store(new Type.Atom(Type.Atom.Kind.VOID), forLoop);    
        else
            Report.error(forLoop.position, "forLoop isn't the right type.");
    }

    @Override
    public void visit(Name name) {

        var def = this.definitions.valueFor(name);
       
        if (def.isPresent()) {
            var type = this.getType(def.get());
            this.types.store(type, name);
        }
        else
            Report.error(name.position, "Name Definition doesn't exist.");
    }

    @Override
    public void visit(IfThenElse ifThenElse) {

        this.typeExpr(ifThenElse.condition);
        this.typeExpr(ifThenElse.thenExpression);

        if (ifThenElse.elseExpression.isPresent())
            this.typeExpr(ifThenElse.elseExpression.get());

        // rule 11.
        if (this.getType(ifThenElse.condition).isLog()) 
                this.types.store(new Type.Atom(Type.Atom.Kind.VOID), ifThenElse);
        else
            Report.error(ifThenElse.position, "IfThenElse isn't the right type.");
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
        this.typeExpr(unary.expr);

        var exprType = this.getType(unary.expr);

        // rule 2.
        if (exprType.isLog() && unary.operator == Unary.Operator.NOT) 
            this.types.store(exprType, unary);  
        // rule 3.  
        else if (exprType.isInt() && (
           unary.operator == Unary.Operator.ADD
        || unary.operator == Unary.Operator.SUB
            )) 
            this.types.store(exprType, unary);    
        else
            Report.error(unary.position, "Unary isn't the right type.");
    }

    @Override
    public void visit(While whileLoop) {
        this.typeExpr(whileLoop.condition);
        this.typeExpr(whileLoop.body);

        // rule 11.
        if (this.getType(whileLoop.condition).isLog()) 
                this.types.store(new Type.Atom(Type.Atom.Kind.VOID), whileLoop);
        else
            Report.error(whileLoop.position, "IfThenElse isn't the right type.");
    }

    @Override
    public void visit(Where where) {

        this.visit(where.defs);
        this.typeExpr(where.expr);

        // rule 9.
        this.types.store(this.getType(where.expr), where);
    }

    @Override
    public void visit(Array array) {

        this.typeType(array.type);
        var arraytype = this.getType(array.type);
        
        Type arr = new Type.Array(array.size, arraytype);

        this.types.store(arr, array);
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
        
        if (def.isPresent()) {
            var type = this.getType(def.get());
                this.types.store(type, name);
        }
        else
            Report.error(name.position, "TypName Definition doesn't exist.");
    }
}
