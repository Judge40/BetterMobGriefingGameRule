/*
 * Better mobGriefing GameRule Copyright (c) 2016 Judge40
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */
package com.judge40.minecraft.bettermobgriefinggamerulecore;

import java.util.Iterator;
import java.util.ListIterator;

import org.objectweb.asm.ClassReader;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.tree.AbstractInsnNode;
import org.objectweb.asm.tree.ClassNode;
import org.objectweb.asm.tree.LdcInsnNode;
import org.objectweb.asm.tree.MethodInsnNode;
import org.objectweb.asm.tree.MethodNode;
import org.objectweb.asm.tree.VarInsnNode;

import com.judge40.minecraft.bettermobgriefinggamerule.BetterMobGriefingGameRule;

import net.minecraft.launchwrapper.IClassTransformer;

/**
 * Class transformer which replaces the mobGriefing game rule for entities which can not be handled
 * with events
 */
public class BetterMobGriefingGameRuleIClassTransformer implements IClassTransformer {

  /*
   * (non-Javadoc)
   * 
   * @see net.minecraft.launchwrapper.IClassTransformer#transform(java.lang.String,
   * java.lang.String, byte[])
   */
  @Override
  public byte[] transform(String name, String transformedName, byte[] basicClass) {
    if (transformedName.equals("net.minecraft.entity.monster.EntityEnderman")
        || transformedName.equals("net.minecraft.entity.boss.EntityDragon")) {
      basicClass = transformMobGriefingGameRule(basicClass);
    }

    return basicClass;
  }

  /**
   * Transform the class byte array and replaces the mobGriefing game rule with a new rule
   * 
   * @param basicClass The byte array to transform
   * @return The transformed byte array
   */
  private static byte[] transformMobGriefingGameRule(byte[] basicClass) {
    ClassNode classNode = new ClassNode();
    ClassReader classReader = new ClassReader(basicClass);
    classReader.accept(classNode, 0);

    Iterator<MethodNode> methodNodeIterator = classNode.methods.iterator();

    while (methodNodeIterator.hasNext()) {
      MethodNode methodNode = methodNodeIterator.next();
      ListIterator<AbstractInsnNode> instructionIterator = methodNode.instructions.iterator();

      while (instructionIterator.hasNext()) {
        AbstractInsnNode instruction = instructionIterator.next();

        if (instruction instanceof LdcInsnNode) {
          LdcInsnNode ldcNode = (LdcInsnNode) instruction;

          if (ldcNode.cst.equals(BetterMobGriefingGameRule.ORIGINAL)) {
            AbstractInsnNode nextInstruction = ldcNode.getNext();

            if (nextInstruction instanceof MethodInsnNode) {
              MethodInsnNode methodInsnNode = (MethodInsnNode) nextInstruction;

              if (methodInsnNode.name.equals("getGameRuleBooleanValue")
                  || methodInsnNode.name.equals("func_82766_b")) {
                // Found correct instructions to replace
                instructionIterator.next();

                while (instructionIterator.hasPrevious()) {
                  instructionIterator.remove();
                  AbstractInsnNode previousInstruction = instructionIterator.previous();

                  if (previousInstruction instanceof VarInsnNode) {
                    // Found entity variable
                    instructionIterator.next();
                    instructionIterator.add(new MethodInsnNode(Opcodes.INVOKESTATIC,
                        "com/judge40/minecraft/bettermobgriefinggamerule/BetterMobGriefingGameRule",
                        "isMobGriefingEnabled", "(Lnet/minecraft/entity/EntityLiving;)Z", false));
                    break;
                  }
                }
              }
            }
          }
        }
      }
    }

    ClassWriter classWriter = new ClassWriter(ClassWriter.COMPUTE_MAXS);
    classNode.accept(classWriter);
    return classWriter.toByteArray();
  }
}
