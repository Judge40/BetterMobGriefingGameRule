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

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;

import org.objectweb.asm.ClassReader;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.tree.AbstractInsnNode;
import org.objectweb.asm.tree.ClassNode;
import org.objectweb.asm.tree.FieldInsnNode;
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

  private static final Map<String, Map<String, List<AbstractInsnNode>>> TRANSFORM_TARGETS =
      new HashMap<>();

  static {
    boolean deobfuscated = (Boolean) Launch.blackboard.get("fml.deobfuscatedEnvironment");

    Map<String, List<AbstractInsnNode>> blockFarmLandTargets = new HashMap<>();
    blockFarmLandTargets.put(deobfuscated ? "onFallenUpon" : "func_180658_a",
        Collections.singletonList(new VarInsnNode(Opcodes.ALOAD, 5)));
    TRANSFORM_TARGETS.put("net.minecraft.block.BlockFarmland", blockFarmLandTargets);

    VarInsnNode instanceVariable = new VarInsnNode(Opcodes.ALOAD, 0);

    FieldInsnNode entityAiEatGrassFieldNode =
        new FieldInsnNode(Opcodes.GETFIELD, "net/minecraft/entity/ai/EntityAIEatGrass",
            "field_151500_b", "Lnet/minecraft/entity/EntityLiving;");
    List<AbstractInsnNode> entityAiEatGrassTargetInstructions =
        Arrays.asList(instanceVariable, entityAiEatGrassFieldNode);
    Map<String, List<AbstractInsnNode>> entityAiEatGrassTargets = new HashMap<>();
    entityAiEatGrassTargets.put(deobfuscated ? "updateTask" : "func_75246_d",
        entityAiEatGrassTargetInstructions);
    TRANSFORM_TARGETS.put("net.minecraft.entity.ai.EntityAIEatGrass", entityAiEatGrassTargets);

    List<AbstractInsnNode> instanceInstructions = Collections.singletonList(instanceVariable);

    Map<String, List<AbstractInsnNode>> entityDragonTargets = new HashMap<>();
    entityDragonTargets.put(deobfuscated ? "destroyBlocksInAABB" : "func_70972_a",
        instanceInstructions);
    TRANSFORM_TARGETS.put("net.minecraft.entity.boss.EntityDragon", entityDragonTargets);

    Map<String, List<AbstractInsnNode>> entityEndermanTargets = new HashMap<>();
    entityEndermanTargets.put(deobfuscated ? "onLivingUpdate" : "func_70636_d",
        instanceInstructions);
    TRANSFORM_TARGETS.put("net.minecraft.entity.monster.EntityEnderman", entityEndermanTargets);

    Map<String, List<AbstractInsnNode>> entityLivingTargets = new HashMap<>();
    entityLivingTargets.put(deobfuscated ? "onLivingUpdate" : "func_70636_d", instanceInstructions);
    TRANSFORM_TARGETS.put("net.minecraft.entity.EntityLiving", entityLivingTargets);

    Map<String, List<AbstractInsnNode>> entitySilverfishTargets = new HashMap<>();
    entitySilverfishTargets.put(deobfuscated ? "onLivingUpdate" : "func_70636_d",
        instanceInstructions);
    TRANSFORM_TARGETS.put("net.minecraft.entity.monster.EntitySilverfish", entitySilverfishTargets);

    Map<String, List<AbstractInsnNode>> entityWitherTargets = new HashMap<>();
    entityWitherTargets.put(deobfuscated ? "updateAITasks" : "func_70619_bc", instanceInstructions);
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
    Map<String, List<AbstractInsnNode>> transformTarget = TRANSFORM_TARGETS.get(transformedName);

    if (transformTarget == null) {
      return basicClass;
    }

    ClassNode classNode = new ClassNode();
    ClassReader classReader = new ClassReader(basicClass);
    classReader.accept(classNode, 0);

    Iterator<MethodNode> methodNodeIterator = classNode.methods.iterator();

    while (methodNodeIterator.hasNext()) {
      MethodNode methodNode = methodNodeIterator.next();
      List<AbstractInsnNode> replacementNodes =
          transformTarget.getOrDefault(methodNode.name, Collections.emptyList());

      if (replacementNodes.isEmpty()) {
        continue;
      }

      int instanceCount = 0;

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
                  instructionIterator.remove();

                  for (AbstractInsnNode replacementNode : replacementNodes) {
                    // Can not reuse the same instruction, so clone it if multiple are needed
                    if (instanceCount > 0) {
                      replacementNode = replacementNode.clone(Collections.emptyMap());
                    }

                    instructionIterator.add(replacementNode);
                  }

                  instructionIterator.add(new MethodInsnNode(Opcodes.INVOKESTATIC,
                      "com/judge40/minecraft/bettermobgriefinggamerule/BetterMobGriefingGameRule",
                      "isMobGriefingEnabled", "(Lnet/minecraft/entity/Entity;)Z", false));
                  instanceCount++;
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
