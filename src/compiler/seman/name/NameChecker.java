/**
 * @ Author: turk
 * @ Description: Preverjanje in razreševanje imen.
 */

package compiler.seman.name;

import static common.RequireNonNull.requireNonNull;

import java.util.ArrayList;

import common.Constants;
import common.Report;
import compiler.common.Visitor;
import compiler.lexer.Position;
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

        var defLocation = new Position(0, 0, 0, 0);
        var paramsInt = new ArrayList<Parameter>();
        var paramsInt2 = new ArrayList<Parameter>();
        var paramsStr = new ArrayList<Parameter>();
        var paramsLog = new ArrayList<Parameter>();

        paramsInt.add(new Parameter(defLocation, "int", Atom.INT(defLocation)));
        paramsInt2.add(new Parameter(defLocation, "int", Atom.INT(defLocation)));
        paramsInt2.add(new Parameter(defLocation, "int", Atom.INT(defLocation)));
        paramsStr.add(new Parameter(defLocation, "int", Atom.STR(defLocation)));
        paramsLog.add(new Parameter(defLocation, "int", Atom.LOG(defLocation)));

        var printInt = new FunDef(defLocation, Constants.printIntLabel, paramsInt, Atom.INT(defLocation), new Block(defLocation, new ArrayList<Expr>()));
        var seed = new FunDef(defLocation, Constants.seedLabel, paramsInt, Atom.INT(defLocation), new Block(defLocation, new ArrayList<Expr>()));
        var printStr = new FunDef(defLocation, Constants.printStringLabel, paramsStr, Atom.STR(defLocation), new Block(defLocation, new ArrayList<Expr>()));
        var printLog = new FunDef(defLocation, Constants.printLogLabel, paramsLog, Atom.LOG(defLocation), new Block(defLocation, new ArrayList<Expr>()));
        var rand = new FunDef(defLocation, Constants.randIntLabel, paramsInt2, Atom.INT(defLocation), new Block(defLocation, new ArrayList<Expr>()));

        try {
            this.symbolTable.insert(printInt);
            this.symbolTable.insert(printStr);
            this.symbolTable.insert(printLog);
            this.symbolTable.insert(seed);
            this.symbolTable.insert(rand);
        } catch (DefinitionAlreadyExistsException e) {
            Report.error("Librariy issues");
        }
        
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
            else if (nameVar.get() instanceof VarDef || nameVar.get() instanceof Parameter)
                Report.error(call.position, "Fun Definition is a variable.");   
            else if (nameVar.get() instanceof TypeDef)
                Report.error(call.position, "Fun Definition is a type.");   
            else
                Report.error(call.position, "Fun Definition is an unknown class");       
        else
            Report.error(call.position, "Fun Definition doesn't exist.");

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
            if (nameVar.get() instanceof VarDef || nameVar.get() instanceof Parameter)
                this.definitions.store(nameVar.get(), name);     
            else if (nameVar.get() instanceof TypeDef)
                Report.error(name.position, "Var Definition is a type.");  
            else if (nameVar.get() instanceof FunDef)
                Report.error(name.position, "Var Definition is a function.");
            else
                Report.error(name.position, "Var Definition is an unknown class.");       
        else
            Report.error(name.position, "Var Definition doesn't exist.");      
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
            else if (typeName.get() instanceof VarDef || typeName.get() instanceof Parameter)
                Report.error(name.position, "Typ Definition is a variable.");   
            else if (typeName.get() instanceof FunDef)
                Report.error(name.position, "Typ Definition is a function.");   
            else
                Report.error(name.position, "Typ Definition is an unknown class.");       
        else
            Report.error(name.position, "Typ Definition doesn't exist.");
    }
}
