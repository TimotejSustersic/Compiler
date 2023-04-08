/**
 * @ Author: turk
 * @ Description: Preverjanje in razreševanje imen.
 */

package compiler.seman.name;

import static common.RequireNonNull.requireNonNull;

import common.Report;
import compiler.common.Visitor;
import compiler.parser.ast.def.*;
import compiler.parser.ast.def.FunDef.Parameter;
import compiler.parser.ast.expr.*;
import compiler.parser.ast.type.*;
import compiler.seman.common.NodeDescription;
import compiler.seman.name.env.SymbolTable;
import compiler.seman.name.env.SymbolTable.DefinitionAlreadyExistsException;

public class NameChecker implements Visitor {
    /**
     * Opis vozlišč, ki jih povežemo z njihovimi
     * definicijami.
     * tuki ti notr posiljas te povezave k si jih naredu in bo pol pretty print jih iskal
     */
    private NodeDescription<Def> definitions;

    /**
     * Simbolna tabela.
     * tuki mas pa ti zase tabelo kamo shranjujes deklaracijo ki jo pol najdes in povezavo nardis v definitions
     */
    private SymbolTable symbolTable;

    /**
     * Ustvari nov razreševalnik imen.
     */
    public NameChecker(NodeDescription<Def> definitions, SymbolTable symbolTable) {
        requireNonNull(definitions, symbolTable);
        this.definitions = definitions;
        this.symbolTable = symbolTable;
    }

    private void nameType(Type type) {
        if (type instanceof TypeName)
            this.visit((TypeName) type);
        else if (type instanceof Array)
            this.visit((Array) type);
        else {
            // its atom
            //System.out.println(type.getClass());
            //Report.error("Wrong Type class instance in naming.");
        }
    }

    private void nameExpr(Expr expr) {
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

    private void insertName(Def def) {
        try {
            this.symbolTable.insert(def);
        } catch (DefinitionAlreadyExistsException e) {
            Report.error(def.position, def.name + " alredy exists.");
        }
    }

    // DONE
    @Override
    public void visit(Defs defs) {
        // first BFS
        for (Def def: defs.definitions)
            this.insertName(def);

        // second DFS
        for (Def def: defs.definitions) {
            if (def instanceof TypeDef) 
                this.visit((TypeDef) def);            
            else if (def instanceof FunDef) 
                this.visit((FunDef) def);            
            else if (def instanceof VarDef) 
                this.visit((VarDef) def);
            else 
                Report.error("Wrong definition class in naming");
        }
    }

    // DONE
    @Override
    public void visit(TypeDef typeDef) {
        // type def only have a type
        this.nameType(typeDef.type);  
    }

    // DONE
    @Override
    public void visit(FunDef funDef) {
        // everything after name is new scope
        this.symbolTable.pushScope();
        
        for (Parameter parameter: funDef.parameters)
            this.nameType(parameter.type);
        
        for (Parameter parameter: funDef.parameters)
            this.insertName(parameter);    
        
        this.nameType(funDef.type);
        this.nameExpr(funDef.body);

        this.symbolTable.popScope();
    }

    // DONE
    @Override
    public void visit(Parameter parameter) {
        this.nameType(parameter.type);
        this.insertName(parameter);        
    }

    // DONE
    @Override
    public void visit(VarDef varDef) {
        // only has type to define
        this.nameType(varDef.type);
    }

    // DONE
    @Override
    public void visit(Atom atom) {
        // there is nothing to be done since atomarni tip
    }

    @Override
    public void visit(Call call) {
        var nameVar = this.symbolTable.definitionFor(call.name);
        if (nameVar.isPresent()) 
            if (nameVar.get() instanceof FunDef)
                this.definitions.store(nameVar.get(), call);      
            else if (nameVar.get() instanceof VarDef)
                Report.error(call.position, "Fun Definition is a variable.");   
            else if (nameVar.get() instanceof TypeDef)
                Report.error(call.position, "Fun Definition is a type.");   
            else
                Report.error(call.position, "Fun Definition is an unknown class");       
        else
            Report.error(call.position, "Fun Definition is doesn't exist.");

        for (var expr: call.arguments)
            this.nameExpr(expr);
    }

    // DONE
    @Override
    public void visit(Binary binary) {
        this.nameExpr(binary.left);
        this.nameExpr(binary.right);
    }

    // DONE
    @Override
    public void visit(Block block) {
        for (var expr: block.expressions)
            this.nameExpr(expr);
    }

    // DONE
    @Override
    public void visit(For forLoop) {       

        this.visit((Name) forLoop.counter);
        this.nameExpr(forLoop.low);
        this.nameExpr(forLoop.high);
        this.nameExpr(forLoop.step);        
        this.nameExpr(forLoop.body);  
    }

    // DONE
    @Override
    public void visit(Name name) {    
        var nameVar = this.symbolTable.definitionFor(name.name);

        if (nameVar.isPresent()) 
            if (nameVar.get() instanceof VarDef)
                this.definitions.store(nameVar.get(), name);      
            else if (nameVar.get() instanceof FunDef)
                Report.error(name.position, "Var Definition is a function.");   
            else if (nameVar.get() instanceof TypeDef)
                Report.error(name.position, "Var Definition is a type.");   
            else
                Report.error(name.position, "Var Definition is an unknown class.");       
        else
            Report.error(name.position, "Var Definition is doesn't exist.");      
    }

    // DONE
    @Override
    public void visit(IfThenElse ifThenElse) {

        this.nameExpr(ifThenElse.condition);
        this.nameExpr(ifThenElse.thenExpression);

        if (ifThenElse.elseExpression.isPresent())
            this.nameExpr(ifThenElse.elseExpression.get());
    }

    // DONE
    @Override
    public void visit(Literal literal) {
        // nothing
    }

    @Override
    public void visit(Unary unary) {
        this.nameExpr(unary.expr);
    }

    // DONE
    @Override
    public void visit(While whileLoop) {
        this.nameExpr(whileLoop.condition);
        this.nameExpr(whileLoop.body);
    }

    // DONE
    @Override
    public void visit(Where where) {
        this.symbolTable.pushScope();

        this.visit(where.defs);

        this.nameExpr(where.expr);

        this.symbolTable.popScope();
    }

    // DONE
    @Override
    public void visit(Array array) {
        this.nameType(array.type);
    }

    // DONE
    @Override
    public void visit(TypeName name) {       
        var typeName = this.symbolTable.definitionFor(name.identifier);

        if (typeName.isPresent()) 
            if (typeName.get() instanceof TypeDef)
                this.definitions.store(typeName.get(), name);        
            else if (typeName.get() instanceof VarDef)
                Report.error(name.position, "Typ Definition is a variable.");   
            else if (typeName.get() instanceof FunDef)
                Report.error(name.position, "Typ Definition is a function.");   
            else
                Report.error(name.position, "Typ Definition is an unknown class.");       
        else
            Report.error(name.position, "Typ Definition is doesn't exist.");
    }
}
