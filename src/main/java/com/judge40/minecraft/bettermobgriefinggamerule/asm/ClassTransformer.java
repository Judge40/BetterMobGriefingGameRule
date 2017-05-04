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
package com.judge40.minecraft.bettermobgriefinggamerule.asm;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
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
import com.judge40.minecraft.bettermobgriefinggamerule.ModInfoConstants;
import com.judge40.minecraft.bettermobgriefinggamerule.ObfuscationHelper;

import net.minecraft.launchwrapper.IClassTransformer;

/**
 * A class transformer which replaces the mobGriefing game rule calls which can not be handled with
 * events.
 */
public class ClassTransformer implements IClassTransformer {

  private static final Logger logger = LogManager.getLogger(ModInfoConstants.ID);

  // A map where the key is the class name to be transformed and the value is a map of method names
  // to replacement instructions.
  private static final Map<String, Map<String, List<AbstractInsnNode>>> TRANSFORM_TARGETS =
      new HashMap<>();

  static {
    // Create the base invocation of the isMobGriefingEnabled method, can be reused for all targets.
    MethodInsnNode invocation = new MethodInsnNode(Opcodes.INVOKESTATIC,
        "com/judge40/minecraft/bettermobgriefinggamerule/BetterMobGriefingGameRule",
        "isMobGriefingEnabled", "(Lnet/minecraft/entity/Entity;)Z", false);

    // Add BlockFarmland targets.
    Map<String, List<AbstractInsnNode>> blockFarmLandTargets = new HashMap<>();
    blockFarmLandTargets.put(ObfuscationHelper.convertName("func_180658_a"),
        Arrays.asList(new VarInsnNode(Opcodes.ALOAD, 5), invocation));
    TRANSFORM_TARGETS.put("net.minecraft.block.BlockFarmland", blockFarmLandTargets);

    // The target variable for classes whose own class instance is the target entity.
    VarInsnNode instanceVariable = new VarInsnNode(Opcodes.ALOAD, 0);

    // Add EntityAIEatGrass targets, the target entity is held in a field within the class.
    FieldInsnNode entityAiEatGrassFieldNode =
        new FieldInsnNode(Opcodes.GETFIELD, "net/minecraft/entity/ai/EntityAIEatGrass",
            "field_151500_b", "Lnet/minecraft/entity/EntityLiving;");
    List<AbstractInsnNode> entityAiEatGrassTargetInstructions =
        Arrays.asList(instanceVariable, entityAiEatGrassFieldNode, invocation);
    Map<String, List<AbstractInsnNode>> entityAiEatGrassTargets = new HashMap<>();
    entityAiEatGrassTargets.put(ObfuscationHelper.convertName("func_75246_d"),
        entityAiEatGrassTargetInstructions);
    TRANSFORM_TARGETS.put("net.minecraft.entity.ai.EntityAIEatGrass", entityAiEatGrassTargets);

    // Create the instructions for classes whose own class instance is the target entity.
    List<AbstractInsnNode> instanceInstructions = Arrays.asList(instanceVariable, invocation);

    // Add EnderDragon targets.
    Map<String, List<AbstractInsnNode>> entityDragonTargets = new HashMap<>();
    entityDragonTargets.put(ObfuscationHelper.convertName("func_70972_a"), instanceInstructions);
    TRANSFORM_TARGETS.put("net.minecraft.entity.boss.EntityDragon", entityDragonTargets);

    // Add Enderman targets.
    Map<String, List<AbstractInsnNode>> entityEndermanTargets = new HashMap<>();
    entityEndermanTargets.put(ObfuscationHelper.convertName("func_70636_d"), instanceInstructions);
    TRANSFORM_TARGETS.put("net.minecraft.entity.monster.EntityEnderman", entityEndermanTargets);

    // Add EntityLiving targets.
    Map<String, List<AbstractInsnNode>> entityLivingTargets = new HashMap<>();
    entityLivingTargets.put(ObfuscationHelper.convertName("func_70636_d"), instanceInstructions);
    TRANSFORM_TARGETS.put("net.minecraft.entity.EntityLiving", entityLivingTargets);

    // Add Silverfish targets.
    Map<String, List<AbstractInsnNode>> entitySilverfishTargets = new HashMap<>();
    entitySilverfishTargets.put(ObfuscationHelper.convertName("func_70626_be"),
        instanceInstructions);
    TRANSFORM_TARGETS.put("net.minecraft.entity.monster.EntitySilverfish", entitySilverfishTargets);

    // Add Wither targets.
    Map<String, List<AbstractInsnNode>> entityWitherTargets = new HashMap<>();
    entityWitherTargets.put(ObfuscationHelper.convertName("func_70619_bc"), instanceInstructions);
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
    Map<String, List<AbstractInsnNode>> methodNamesToReplacementNodes =
        TRANSFORM_TARGETS.get(transformedName);

    if (methodNamesToReplacementNodes != null) {
      logger.info("The \"{}\" class has been identified for transformation.", transformedName);
      basicClass = transformClass(basicClass, methodNamesToReplacementNodes);
    }
    return basicClass;
  }

  /**
   * Perform transformation of a class to replace calls to the original mob griefing game rule with
   * calls to BetterMobGriefingGameRule handling.
   * 
   * @param basicClass The bytes of the class to transform.
   * @param methodNamesToReplacementInstructions A map where the key is the name of methods which
   *        need transformation and the value is the list of new instructions to be called.
   * @return The byte of the transformed class.
   */
  private static byte[] transformClass(byte[] basicClass,
      Map<String, List<AbstractInsnNode>> methodNamesToReplacementInstructions) {
    // Create a class reader from the input byte array.
    ClassNode classNode = new ClassNode();
    ClassReader classReader = new ClassReader(basicClass);
    classReader.accept(classNode, 0);

    // Transform each method in the class which is able to be transformed.
    for (Iterator<MethodNode> iterator = classNode.methods.iterator(); iterator.hasNext();) {
      MethodNode methodNode = iterator.next();
      List<AbstractInsnNode> replacementInstructions = methodNamesToReplacementInstructions
          .getOrDefault(methodNode.name, Collections.emptyList());

      // If there are replacement instructions then the method is able to be transformed.
      if (!replacementInstructions.isEmpty()) {
        logger.info("The \"{}\" method has been identified for transformation.", methodNode.name);
        transformMethod(methodNode, replacementInstructions);
      }
    }

    // Return the transformed class as a byte array.
    ClassWriter classWriter = new ClassWriter(ClassWriter.COMPUTE_MAXS);
    classNode.accept(classWriter);
    return classWriter.toByteArray();
  }

  /**
   * Perform transformation of a method to replace calls to the original mob griefing game rule with
   * calls to BetterMobGriefingGameRule handling.
   * 
   * @param methodNode The node representing the method to transform.
   * @param replacementInstructions The list of new instructions to be called.
   */
  private static void transformMethod(MethodNode methodNode,
      List<AbstractInsnNode> replacementInstructions) {
    // If instructions are used more than once then they must be cloned.
    boolean cloneInstructions = false;

    // Check each method called to identify where to begin transformation.
    for (ListIterator<AbstractInsnNode> iterator = methodNode.instructions.iterator(); iterator
        .hasNext();) {
      AbstractInsnNode instruction = iterator.next();

      if (instruction instanceof MethodInsnNode) {
        MethodInsnNode methodInsnNode = (MethodInsnNode) instruction;

        // If the method call is to check a game rule then check which game rule.
        if (methodInsnNode.name.equals(ObfuscationHelper.convertName("func_82766_b"))) {
          LdcInsnNode ldcNode = (LdcInsnNode) methodInsnNode.getPrevious();

          // If the game rule being checked is "mobGriefing" then the correct point to begin
          // transformation has been found.
          if (ldcNode.cst.equals(BetterMobGriefingGameRule.ORIGINAL)) {
            transformInstructions(iterator, replacementInstructions, cloneInstructions);
            logger.info("An instance of mob griefing has been identified and replaced.");
            cloneInstructions |= true;
          }
        }
      }
    }
  }

  /**
   * Perform transformation of instructions to replace the call to the original mob griefing game
   * rule with a call to BetterMobGriefingGameRule handling.
   * 
   * @param instructionIterator The instruction iterator to remove instructions from, the iterator's
   *        position must be the call to "func_82766_b".
   * @param replacementInstructions The list of new instructions to be called.
   * @param cloneInstructions Whether the replacement instructions should be cloned before
   *        insertion, should be set to true when the replacement instructions may be reused.
   */
  private static void transformInstructions(ListIterator<AbstractInsnNode> instructionIterator,
      List<AbstractInsnNode> replacementInstructions, boolean cloneInstructions) {
    // Loop through and remove each instruction up to and including the source variable used to call
    // the "func_82766_b" method.
    while (instructionIterator.hasPrevious()) {
      instructionIterator.remove();
      AbstractInsnNode instruction = instructionIterator.previous();

      // If the instruction is a variable then the correct point to insert the replacements has been
      // found.
      if (instruction instanceof VarInsnNode) {
        instructionIterator.remove();

        for (AbstractInsnNode replacementNode : replacementInstructions) {
          // It is not possible to reuse the same instruction in multiple places, so create clones
          // if required.
          if (cloneInstructions) {
            replacementNode = replacementNode.clone(Collections.emptyMap());
          }

          instructionIterator.add(replacementNode);
        }
        break;
      }
    }
  }
}