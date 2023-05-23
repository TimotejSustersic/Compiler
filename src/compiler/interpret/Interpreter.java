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
import compiler.frm.Frame.Temp;
import compiler.gen.Memory;
import compiler.ir.chunk.Chunk.CodeChunk;
import compiler.ir.code.IRNode;
import compiler.ir.code.expr.*;
import compiler.ir.code.expr.BinopExpr.Operator;
import compiler.ir.code.stmt.*;
import compiler.ir.IRPrettyPrint;

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

        this.framePointer = this.stackPointer;
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

    // TODO zrihti mem
    private Object execute(MoveStmt move, Map<Frame.Temp, Object> temps) {

        //System.out.println("move");
        //prettyPrint(move);

        // desni del
        var source = execute(move.src, temps);
        Object dest;   
        

        // levi more bit brez tempa
        if ((move.dst instanceof MemExpr))  {
            var memDest = (MemExpr) move.dst;
            dest = execute(memDest.expr, temps); 
        }
        else {
            dest = execute(move.dst, temps);
        }

        //System.out.println("naslov: " + dest);
        //System.out.println("vrednost: " + source);

        if (dest instanceof Frame.Temp) 
            this.memory.stT((Frame.Temp) dest, source);          
        else if (dest instanceof Frame.Label)
            this.memory.stM((Frame.Label) dest, source);
        else {            
            try {                
                this.memory.stM(toInt(dest), source);
            } catch (IllegalArgumentException e) {  
                System.out.println(e.toString());
                System.out.println(dest.getClass().getSimpleName());
            }            
        }

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

    // TODO pac vse
    private Object execute(BinopExpr binop, Map<Frame.Temp, Object> temps) {

        var left = execute(binop.lhs, temps);
        var right = execute(binop.rhs, temps);

        //Integer output = null;
        //FP
        try {
            toInt(left);
        } catch (IllegalArgumentException e) {
            left = this.framePointer;
        }        
        
        // FP
        try {
            toInt(right);
        } catch (IllegalArgumentException e) {
            right = this.framePointer;
        }
        
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

            // dej mem skini povsod razn v klicu funkcije pa name al nameexpr ne vem
        //System.out.println("binary: left: " + left + " right: " + right + " result: " + output);
        //return output;
        return null;
    }

    // TODO zrihti call pa probi pretvort v prvotno obliko
    private Object execute(CallExpr call, Map<Frame.Temp, Object> temps) {
        if (call.label.name.equals(Constants.printIntLabel)) {
            if (call.args.size() != 2) { throw new RuntimeException("Invalid argument count!"); }
            var arg = execute(call.args.get(1), temps);

            try {
            var output = this.memory.ldT((Temp) arg);
            outputStream.ifPresent(stream -> stream.println(output));
            }
            catch (IllegalArgumentException e) {
                System.out.println(e.toString());
            }
            
            return null;
        } else if (call.label.name.equals(Constants.printStringLabel)) {
            if (call.args.size() != 2) { throw new RuntimeException("Invalid argument count!"); }           
            var address = execute(call.args.get(1), temps);
            //var res = memory.ldM(toInt(address));
            try {
                var output = this.memory.ldT((Temp) address);
                outputStream.ifPresent(stream -> stream.println("\""+output+"\""));
            }
            catch (IllegalArgumentException e) {
                System.out.println(e.toString());
            }

            return null;
        } else if (call.label.name.equals(Constants.printLogLabel)) {
            if (call.args.size() != 2) { throw new RuntimeException("Invalid argument count!"); }
            var arg = execute(call.args.get(1), temps);
            // mal pofejkan
            try {
                var output = this.memory.ldT((Temp) arg);
                if ((Integer) output == 1)
                outputStream.ifPresent(stream -> stream.println("true"));
                else
                outputStream.ifPresent(stream -> stream.println("false"));
            }
            catch (IllegalArgumentException e) {
                    System.out.println(e.toString());
            }

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
            return execute((IRStmt) chunk.code, temps);
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

        var naslov = execute(mem.expr, temps);

        if (mem.expr instanceof ConstantExpr) 
            return naslov;

        if (naslov instanceof Frame.Temp) 
            return this.memory.ldT((Frame.Temp) naslov);    

        if (naslov instanceof Frame.Label)
            return this.memory.ldM((Frame.Label) naslov);
            
        try {
            return this.memory.ldM(toInt(naslov));
        } catch (IllegalArgumentException e) {
            //System.out.println("mem error");
            System.out.println(e.toString());

            prettyPrint(mem);
            //System.out.println("memend");
            return 0; 
        }       
    }

    // TODO mogoce mor sat ze kle mem
    private Object execute(NameExpr name) {
        return name.label;
    }

    // TODO mogoce je treba dat mem vedno in pol pr move pohandlat
    private Object execute(TempExpr temp, Map<Frame.Temp, Object> temps) {           
        return temp.temp;        
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
