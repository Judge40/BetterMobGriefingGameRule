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

import java.util.HashMap;
import java.util.Iterator;
import java.util.ListIterator;
import java.util.Map;

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
import net.minecraft.launchwrapper.Launch;

/**
 * Class transformer which replaces the mobGriefing game rule for entities which can not be handled
 * with events
 */
public class BetterMobGriefingGameRuleIClassTransformer implements IClassTransformer {

  private static final Map<String, Map<String, Integer>> TRANSFORM_TARGETS = new HashMap<>();

  static {
    boolean deobfuscated = (Boolean) Launch.blackboard.get("fml.deobfuscatedEnvironment");

    Map<String, Integer> blockFarmLandTargets = new HashMap<>();
    blockFarmLandTargets.put(deobfuscated ? "onFallenUpon" : "func_180658_a", 5);
    TRANSFORM_TARGETS.put("net.minecraft.block.BlockFarmland", blockFarmLandTargets);

    Map<String, Integer> entityDragonTargets = new HashMap<>();
    entityDragonTargets.put(deobfuscated ? "destroyBlocksInAABB" : "func_70972_a", 0);
    TRANSFORM_TARGETS.put("net.minecraft.entity.boss.EntityDragon", entityDragonTargets);

    Map<String, Integer> entityEndermanTargets = new HashMap<>();
    entityEndermanTargets.put(deobfuscated ? "onLivingUpdate" : "func_70636_d", 0);
    TRANSFORM_TARGETS.put("net.minecraft.entity.monster.EntityEnderman", entityEndermanTargets);

    Map<String, Integer> entityLivingTargets = new HashMap<>();
    entityLivingTargets.put(deobfuscated ? "onLivingUpdate" : "func_70636_d", 0);
    TRANSFORM_TARGETS.put("net.minecraft.entity.EntityLiving", entityLivingTargets);

    Map<String, Integer> entitySilverfishTargets = new HashMap<>();
    entitySilverfishTargets.put(deobfuscated ? "onLivingUpdate" : "func_70636_d", 0);
    TRANSFORM_TARGETS.put("net.minecraft.entity.monster.EntitySilverfish", entitySilverfishTargets);

    Map<String, Integer> entityWitherTargets = new HashMap<>();
    entityWitherTargets.put(deobfuscated ? "updateAITasks" : "func_70619_bc", 0);
    TRANSFORM_TARGETS.put("net.minecraft.entity.boss.EntityWither", entityWitherTargets);
  }

  /*
   * (non-Javadoc)
   * 
   * @see net.minecraft.launchwrapper.IClassTransformer#transform(java.lang.String,
   * java.lang.String, byte[])
   */
  @Override
  public byte[] transform(String name, String transformedName, byte[] basicClass) {
    if (TRANSFORM_TARGETS.containsKey(transformedName)) {
      basicClass = transformMobGriefingGameRule(transformedName, basicClass);
    }

    return basicClass;
  }

  /**
   * Transform the class byte array and replaces the mobGriefing game rule with a new rule
   * 
   * @param transformedName The name of the class being transformed
   * @param basicClass The byte array to transform
   * @returnThe transformed byte array
   */
  private static byte[] transformMobGriefingGameRule(String transformedName, byte[] basicClass) {
    Map<String, Integer> transformTarget = TRANSFORM_TARGETS.get(transformedName);

    if (transformTarget == null) {
      return basicClass;
    }

    ClassNode classNode = new ClassNode();
    ClassReader classReader = new ClassReader(basicClass);
    classReader.accept(classNode, 0);

    Iterator<MethodNode> methodNodeIterator = classNode.methods.iterator();

    while (methodNodeIterator.hasNext()) {
      MethodNode methodNode = methodNodeIterator.next();
      int entityIndex = transformTarget.getOrDefault(methodNode.name, -1);

      if (entityIndex == -1) {
        continue;
      }

      for (ListIterator<AbstractInsnNode> instructionIterator =
          methodNode.instructions.iterator(); instructionIterator.hasNext();) {
        AbstractInsnNode instruction = instructionIterator.next();

        if (instruction instanceof MethodInsnNode) {
          MethodInsnNode methodInsnNode = (MethodInsnNode) instruction;

          if (methodInsnNode.name.equals("getGameRuleBooleanValue")
              || methodInsnNode.name.equals("func_82766_b")) {
            LdcInsnNode ldcNode = (LdcInsnNode) methodInsnNode.getPrevious();

            if (ldcNode.cst.equals(BetterMobGriefingGameRule.ORIGINAL)) {
              // Found correct instructions to replace

              while (instructionIterator.hasPrevious()) {
                instructionIterator.remove();
                instruction = instructionIterator.previous();

                if (instruction instanceof VarInsnNode) {
                  ((VarInsnNode) instruction).var = entityIndex;
                  instructionIterator.next();
                  instructionIterator.add(new MethodInsnNode(Opcodes.INVOKESTATIC,
                      "com/judge40/minecraft/bettermobgriefinggamerule/BetterMobGriefingGameRule",
                      "isMobGriefingEnabled", "(Lnet/minecraft/entity/Entity;)Z", false));
                  break;
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
