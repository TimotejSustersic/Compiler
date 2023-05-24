/**
 * @ Author: turk
 * @ Description: Navidezni stroj (intepreter).
 */

package compiler.interpret;

import static common.RequireNonNull.requireNonNull;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.nio.charset.Charset;
import java.util.Optional;
import java.util.Random;
import java.util.HashMap;
import java.util.Map; 

import common.Constants;
import compiler.frm.Frame;
import compiler.frm.Frame.Label;
import compiler.frm.Frame.Temp;
import compiler.gen.Memory;
import compiler.ir.chunk.Chunk.CodeChunk;
import compiler.ir.code.IRNode;
import compiler.ir.code.expr.*;
import compiler.ir.code.expr.BinopExpr.Operator;
import compiler.ir.code.stmt.*;
import compiler.ir.IRPrettyPrint;

// mamo funkcijo vgrajeno in izracunaj vsoto njenih argumentov
//lazje naloge so lokalne, vecje naloge so pa cez cel kompiler

public class Interpreter {
    /**
     * Pomnilnik navideznega stroja.
     */
    private Memory memory;
    
    /**
     * Izhodni tok, kamor izpisujemo rezultate izvajanja programa.
     * 
     * V primeru, da rezultatov ne želimo izpisovati, nastavimo na `Optional.empty()`.
     */
    private Optional<PrintStream> outputStream;

    /**
     * Generator naključnih števil.
     */
    private Random random;

    /**
     * Skladovni kazalec (kaže na dno sklada).
     */
    private int stackPointer;

    /**
     * Klicni kazalec (kaže na vrh aktivnega klicnega zapisa).
     */
    private int framePointer;

    public Interpreter(Memory memory, Optional<PrintStream> outputStream) {
        requireNonNull(memory, outputStream);
        this.memory = memory;
        this.outputStream = outputStream;
        this.stackPointer = memory.size - Constants.WordSize;
        this.framePointer = memory.size - Constants.WordSize;
    }

    // --------- izvajanje navideznega stroja ----------

    public void interpret(CodeChunk chunk) {
        memory.stM(framePointer + Constants.WordSize, 999); // argument v funkcijo main
        memory.stM(framePointer - chunk.frame.oldFPOffset(), framePointer); // oldFP
        internalInterpret(chunk, new HashMap<>());
    }

    private void internalInterpret(CodeChunk chunk, Map<Frame.Temp, Object> temps) {
        
        this.framePointer = this.stackPointer;
        this.stackPointer -= chunk.frame.size();
        
        Object result = null;
        if (chunk.code instanceof SeqStmt seq) {
            for (int pc = 0; pc < seq.statements.size(); pc++) {
                var stmt = seq.statements.get(pc);
                result = execute(stmt, temps);
                if (result instanceof Frame.Label label) {
                    for (int q = 0; q < seq.statements.size(); q++) {
                        if (seq.statements.get(q) instanceof LabelStmt labelStmt && labelStmt.label.equals(label)) {
                            pc = q;
                            break;
                        }
                    }
                }
            }
        } else {
            throw new RuntimeException("Linearize IR!");
        }

        this.framePointer = toInt(memory.ldM(framePointer - chunk.frame.oldFPOffset()));
        this.stackPointer += chunk.frame.size();
    }

    // DONE
    private Object execute(IRStmt stmt, Map<Frame.Temp, Object> temps) {
        if (stmt instanceof CJumpStmt cjump) {
            return execute(cjump, temps);
        } else if (stmt instanceof ExpStmt exp) {
            return execute(exp, temps);
        } else if (stmt instanceof JumpStmt jump) {
            return execute(jump, temps);
        } else if (stmt instanceof LabelStmt label) {
            return null;
        } else if (stmt instanceof MoveStmt move) {
            return execute(move, temps);
        } else {
            throw new RuntimeException("Cannot execute this statement!");
        }
    }

    // DONE
    private Object execute(CJumpStmt cjump, Map<Frame.Temp, Object> temps) {

        if (toBool(execute(cjump.condition, temps))) 
            return cjump.thenLabel;
        else 
            return cjump.elseLabel;
    }

    // DONE
    private Object execute(ExpStmt exp, Map<Frame.Temp, Object> temps) {
        return execute(exp.expr, temps);
    }

    // DONE 
    private Object execute(JumpStmt jump, Map<Frame.Temp, Object> temps) {    
        return jump.label;
    }

    // TEST
    private Object execute(MoveStmt move, Map<Frame.Temp, Object> temps) {
        //System.out.println("-------------------------------");
        //System.out.println("move");
        //prettyPrint(move);

        // desni del
        var source = execute(move.src, temps);

        if (move.dst instanceof TempExpr) {
            temps.put(((TempExpr) move.dst).temp, source);
            //System.out.println("naslov: " + ((TempExpr) move.dst).temp.id);
        }
        else {            
            try {      

                var memDest = (MemExpr) move.dst;
                var dest = execute(memDest.expr, temps);        
                //System.out.println("naslov: " + dest);         
                this.memory.stM(toInt(dest), source);
            } catch (IllegalArgumentException e) {  
                System.out.println(e.toString());
            }          
        }

        //System.out.println("vrednost: " + source);

        //System.out.println("moveend");
        //System.out.println("-------------------------------");
        return null; 
    }

    // DONE
    private Object execute(IRExpr expr, Map<Frame.Temp, Object> temps) {
        if (expr instanceof BinopExpr binopExpr) {
            return execute(binopExpr, temps);
        } else if (expr instanceof CallExpr callExpr) {
            return execute(callExpr, temps);
        } else if (expr instanceof ConstantExpr constantExpr) {
            return execute(constantExpr);
        } else if (expr instanceof EseqExpr eseqExpr) {
            throw new RuntimeException("Cannot execute ESEQ; linearize IRCode!");
        } else if (expr instanceof MemExpr memExpr) {
            return execute(memExpr, temps);
        } else if (expr instanceof NameExpr nameExpr) {
            return execute(nameExpr);
        } else if (expr instanceof TempExpr tempExpr) {
            return execute(tempExpr, temps);
        } else {
            throw new IllegalArgumentException("Unknown expr type");
        }
    }

    // DONE
    private Object execute(BinopExpr binop, Map<Frame.Temp, Object> temps) {

        var left = execute(binop.lhs, temps);
        var right = execute(binop.rhs, temps);
        
        if (binop.op == Operator.ADD) 
            return toInt(left) + toInt(right);
        else if (binop.op == Operator.SUB) 
            return toInt(left) - toInt(right);
        else if (binop.op == Operator.MUL) 
            return toInt(left) * toInt(right);
        else if (binop.op == Operator.DIV) 
            return toInt(left) / toInt(right);
        else if (binop.op == Operator.MOD) 
            return toInt(left) % toInt(right);
        else if (binop.op == Operator.EQ) 
            return toInt(left) == toInt(right);
        else if (binop.op == Operator.NEQ) 
            return toInt(left) != toInt(right);
        else if (binop.op == Operator.LT) 
            return toInt(left) < toInt(right);
        else if (binop.op == Operator.LEQ) 
            return toInt(left) <= toInt(right);
        else if (binop.op == Operator.GT) 
            return toInt(left) > toInt(right);
        else if (binop.op == Operator.GEQ) 
            return toInt(left) >= toInt(right);
        else if (binop.op == Operator.AND) 
            return toBool(left) && toBool(right);
        else if (binop.op == Operator.OR) 
            return toBool(left) || toBool(right);    
        else if (binop.op == Operator.NOT) 
            return !toBool(right);       

        return null;
    }

    // TODO
    private Object execute(CallExpr call, Map<Frame.Temp, Object> temps) {
        prettyPrint(call);
        if (call.label.name.equals(Constants.printIntLabel)) {
            if (call.args.size() != 2) { throw new RuntimeException("Invalid argument count!"); }
            var arg = execute(call.args.get(1), temps);
            outputStream.ifPresent(stream -> stream.println(arg));
            return null;
        } else if (call.label.name.equals(Constants.printStringLabel)) {
            if (call.args.size() != 2) { throw new RuntimeException("Invalid argument count!"); }
            var address = execute(call.args.get(1), temps);
            //var res = memory.ldM(toInt(address));
            // pace vedno bo label in ne vem zakaj je on cle castov kot integer ker so stringi vedno label
            var res = memory.ldM((Label) address);
            outputStream.ifPresent(stream -> stream.println("\""+res+"\""));
            return null;
        } else if (call.label.name.equals(Constants.printLogLabel)) {
            if (call.args.size() != 2) { throw new RuntimeException("Invalid argument count!"); }
            var arg = execute(call.args.get(1), temps);
            outputStream.ifPresent(stream -> stream.println(toBool(arg)));
            return null;
        } else if (call.label.name.equals(Constants.randIntLabel)) {
            if (call.args.size() != 3) { throw new RuntimeException("Invalid argument count!"); }
            var min = toInt(execute(call.args.get(1), temps));
            var max = toInt(execute(call.args.get(2), temps));
            return random.nextInt(min, max);
        } else if (call.label.name.equals(Constants.seedLabel)) {
            if (call.args.size() != 2) { throw new RuntimeException("Invalid argument count!"); }
            var seed = toInt(execute(call.args.get(1), temps));
            random = new Random(seed);
            return null;
        } else if (memory.ldM(call.label) instanceof CodeChunk chunk) {
            // ...
            // internalInterpret(chunk, new HashMap<>())
            //                          ~~~~~~~~~~~~~ 'lokalni registri'
            // ... 
            // se argumente mors shranit v pomnilnik
            var arguments = new HashMap<Frame.Temp, Object>();
            for (var arg: call.args) 
                arguments.put(Temp.next(), arg);
            // nakonc returnas rezultyt ki je kar na stackpointerju
            internalInterpret(chunk, arguments);
            //System.out.println(this.memory.ldM(this.stackPointer));
            try {
                return this.memory.ldM(this.stackPointer);
            } catch (Exception e) {
                return null;
            }
           
        } else {
            throw new RuntimeException("Only functions can be called!");
        }
    }

    // DONE
    private Object execute(ConstantExpr constant) {
        return constant.constant;
    }

    // TODO
    private Object execute(MemExpr mem, Map<Frame.Temp, Object> temps) {
        //System.out.println("///////////////////////////");
        //prettyPrint(mem);

        var naslov = execute(mem.expr, temps);
            
        try {
            System.out.println("INT: " + this.memory.ldM(toInt(naslov)));
            //System.out.println("///////////////////////////");
            return this.memory.ldM(toInt(naslov));
        } catch (IllegalArgumentException e) {
            System.out.println(e);
        }
        //System.out.println("///////////////////////////");
        return null;
    }

    // DONE
    private Object execute(NameExpr name) {

        if (name.label.name.equals("{FP}"))
            return this.framePointer;
        if (name.label.name.equals("{SP}"))
            return this.stackPointer;
        return name.label;
    }

    // DONE
    private Object execute(TempExpr temp, Map<Frame.Temp, Object> temps) {         
        return temps.get(temp.temp);
    }

    // ----------- pomožne funkcije -----------

    private int toInt(Object obj) {
        if (obj instanceof Integer integer) {
            return integer;
        }
        throw new IllegalArgumentException("Could not convert obj to integer!");
    }

    private boolean toBool(Object obj) {
        return toInt((boolean) obj) == 0 ? false : true;
    }

    private int toInt(boolean bool) {
        return bool ? 1 : 0;
    }

    private String prettyDescription(IRNode ir, int indent) {
        var os = new ByteArrayOutputStream();
        var ps = new PrintStream(os);
        new IRPrettyPrint(ps, indent).print(ir);
        return os.toString(Charset.defaultCharset());
    }

    private String prettyDescription(IRNode ir) {
        return prettyDescription(ir, 2);
    }

    private void prettyPrint(IRNode ir, int indent) {
        System.out.println(prettyDescription(ir, indent));
    }

    private void prettyPrint(IRNode ir) {
        System.out.println(prettyDescription(ir));
    }
}
