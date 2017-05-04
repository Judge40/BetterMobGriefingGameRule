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

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import org.hamcrest.CoreMatchers;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.objectweb.asm.ClassReader;
import org.objectweb.asm.ClassWriter;

import com.google.common.primitives.Bytes;
import com.judge40.minecraft.bettermobgriefinggamerule.BetterMobGriefingGameRule;

import net.minecraft.block.BlockFarmland;
import net.minecraft.entity.Entity;
import net.minecraft.entity.EntityLiving;
import net.minecraft.entity.ai.EntityAIEatGrass;
import net.minecraft.entity.boss.EntityDragon;
import net.minecraft.entity.boss.EntityWither;
import net.minecraft.entity.monster.EntityEnderman;
import net.minecraft.entity.monster.EntitySilverfish;
import net.minecraft.launchwrapper.Launch;

/**
 * The unit tests for {@link ClassTransformer}.
 */
public class ClassTransformerTest {

  private ClassTransformer classTransformer;

  @Before
  public void setUp() {
    // Set the deobfuscation flag.
    Map<String, Object> blackboard = new HashMap<>();
    blackboard.put("fml.deobfuscatedEnvironment", true);
    Launch.blackboard = blackboard;

    classTransformer = new ClassTransformer();
  }

  /**
   * Test that the class is not processed for transformation when the transformation target class is
   * unsupported.
   */
  @Test
  public void testTransform_unhandledClass_transformGameRuleCalled() throws IOException {
    // Set up test data.
    String targetClassName = Entity.class.getName();

    ClassReader classReader = new ClassReader(targetClassName);
    ClassWriter classWriter = new ClassWriter(ClassWriter.COMPUTE_MAXS);
    classReader.accept(classWriter, ClassReader.SKIP_DEBUG);
    byte[] inputBytes = classWriter.toByteArray();

    // Call the method under test.
    byte[] transformedBytes = classTransformer.transform("", targetClassName, inputBytes);

    // Perform assertions.
    Assert.assertThat("The unsupported class was incorrectly transformed.", transformedBytes,
        CoreMatchers.is(inputBytes));
  }

  /**
   * Test that the mob griefing instructions are replaced when the transformation target class is
   * {@link BlockFarmland}.
   */
  @Test
  public void testTransform_blockFarmland_mobGriefingTransformed() throws IOException {
    // Set up test data.
    String targetClassName = BlockFarmland.class.getName();

    ClassReader classReader = new ClassReader(targetClassName);
    ClassWriter classWriter = new ClassWriter(ClassWriter.COMPUTE_MAXS);
    classReader.accept(classWriter, ClassReader.SKIP_DEBUG);
    byte[] inputBytes = classWriter.toByteArray();

    // Call the method under test.
    byte[] transformedBytes = classTransformer.transform("", targetClassName, inputBytes);

    // Perform assertions.
    byte[] originalSearchBytes =
        String.format("\"%s\"", BetterMobGriefingGameRule.ORIGINAL).getBytes();
    Assert.assertThat(
        "A reference to the mobGriefing game rule was still found in the transformed class.",
        Bytes.indexOf(transformedBytes, originalSearchBytes), CoreMatchers.is(-1));

    byte[] replacementSearchBytes = BetterMobGriefingGameRule.class.getSimpleName().getBytes();
    Assert.assertThat(
        "A reference to the better mobGriefing game rule was not found in the transformed class.",
        Bytes.indexOf(transformedBytes, replacementSearchBytes), CoreMatchers.not(-1));
  }

  /**
   * Test that the mob griefing instructions are replaced when the transformation target class is
   * {@link EntityAIEatGrass}.
   */
  @Test
  public void testTransform_entityAIEatGrass_mobGriefingTransformed() throws IOException {
    // Set up test data.
    String targetClassName = EntityAIEatGrass.class.getName();

    ClassReader classReader = new ClassReader(targetClassName);
    ClassWriter classWriter = new ClassWriter(ClassWriter.COMPUTE_MAXS);
    classReader.accept(classWriter, ClassReader.SKIP_DEBUG);
    byte[] inputBytes = classWriter.toByteArray();

    // Call the method under test.
    byte[] transformedBytes = classTransformer.transform("", targetClassName, inputBytes);

    // Perform assertions.
    byte[] originalSearchBytes =
        String.format("\"%s\"", BetterMobGriefingGameRule.ORIGINAL).getBytes();
    Assert.assertThat(
        "A reference to the mobGriefing game rule was still found in the transformed class.",
        Bytes.indexOf(transformedBytes, originalSearchBytes), CoreMatchers.is(-1));

    byte[] replacementSearchBytes = BetterMobGriefingGameRule.class.getSimpleName().getBytes();
    Assert.assertThat(
        "A reference to the better mobGriefing game rule was not found in the transformed class.",
        Bytes.indexOf(transformedBytes, replacementSearchBytes), CoreMatchers.not(-1));
  }

  /**
   * Test that the mob griefing instructions are replaced when the transformation target class is
   * {@link EntityDragon}.
   */
  @Test
  public void testTransform_entityDragon_mobGriefingTransformed() throws IOException {
    // Set up test data.
    String targetClassName = EntityDragon.class.getName();

    ClassReader classReader = new ClassReader(targetClassName);
    ClassWriter classWriter = new ClassWriter(ClassWriter.COMPUTE_MAXS);
    classReader.accept(classWriter, ClassReader.SKIP_DEBUG);
    byte[] inputBytes = classWriter.toByteArray();

    // Call the method under test.
    byte[] transformedBytes = classTransformer.transform("", targetClassName, inputBytes);

    // Perform assertions.
    byte[] originalSearchBytes =
        String.format("\"%s\"", BetterMobGriefingGameRule.ORIGINAL).getBytes();
    Assert.assertThat(
        "A reference to the mobGriefing game rule was still found in the transformed class.",
        Bytes.indexOf(transformedBytes, originalSearchBytes), CoreMatchers.is(-1));

    byte[] replacementSearchBytes = BetterMobGriefingGameRule.class.getSimpleName().getBytes();
    Assert.assertThat(
        "A reference to the better mobGriefing game rule was not found in the transformed class.",
        Bytes.indexOf(transformedBytes, replacementSearchBytes), CoreMatchers.not(-1));
  }

  /**
   * Test that the mob griefing instructions are replaced when the transformation target class is
   * {@link EntityEnderman}.
   */
  @Test
  public void testTransform_entityEnderman_mobGriefingTransformed() throws IOException {
    // Set up test data.
    String targetClassName = EntityEnderman.class.getName();

    ClassReader classReader = new ClassReader(targetClassName);
    ClassWriter classWriter = new ClassWriter(ClassWriter.COMPUTE_MAXS);
    classReader.accept(classWriter, ClassReader.SKIP_DEBUG);
    byte[] inputBytes = classWriter.toByteArray();

    // Call the method under test.
    byte[] transformedBytes = classTransformer.transform("", targetClassName, inputBytes);

    // Perform assertions.
    byte[] originalSearchBytes =
        String.format("\"%s\"", BetterMobGriefingGameRule.ORIGINAL).getBytes();
    Assert.assertThat(
        "A reference to the mobGriefing game rule was still found in the transformed class.",
        Bytes.indexOf(transformedBytes, originalSearchBytes), CoreMatchers.is(-1));

    byte[] replacementSearchBytes = BetterMobGriefingGameRule.class.getSimpleName().getBytes();
    Assert.assertThat(
        "A reference to the better mobGriefing game rule was not found in the transformed class.",
        Bytes.indexOf(transformedBytes, replacementSearchBytes), CoreMatchers.not(-1));
  }

  /**
   * Test that the mob griefing instructions are replaced when the transformation target class is
   * {@link EntityLiving}.
   */
  @Test
  public void testTransform_entityLiving_mobGriefingTransformed() throws IOException {
    // Set up test data.
    String targetClassName = EntityLiving.class.getName();

    ClassReader classReader = new ClassReader(targetClassName);
    ClassWriter classWriter = new ClassWriter(ClassWriter.COMPUTE_MAXS);
    classReader.accept(classWriter, ClassReader.SKIP_DEBUG);
    byte[] inputBytes = classWriter.toByteArray();

    // Call the method under test.
    byte[] transformedBytes = classTransformer.transform("", targetClassName, inputBytes);

    // Perform assertions.
    byte[] originalSearchBytes =
        String.format("\"%s\"", BetterMobGriefingGameRule.ORIGINAL).getBytes();
    Assert.assertThat(
        "A reference to the mobGriefing game rule was still found in the transformed class.",
        Bytes.indexOf(transformedBytes, originalSearchBytes), CoreMatchers.is(-1));

    byte[] replacementSearchBytes = BetterMobGriefingGameRule.class.getSimpleName().getBytes();
    Assert.assertThat(
        "A reference to the better mobGriefing game rule was not found in the transformed class.",
        Bytes.indexOf(transformedBytes, replacementSearchBytes), CoreMatchers.not(-1));
  }

  /**
   * Test that the mob griefing instructions are replaced when the transformation target class is
   * {@link EntitySilverfish}.
   */
  @Test
  public void testTransform_entitySilverfish_mobGriefingTransformed() throws IOException {
    // Set up test data.
    String targetClassName = EntitySilverfish.class.getName();

    ClassReader classReader = new ClassReader(targetClassName);
    ClassWriter classWriter = new ClassWriter(ClassWriter.COMPUTE_MAXS);
    classReader.accept(classWriter, ClassReader.SKIP_DEBUG);
    byte[] inputBytes = classWriter.toByteArray();

    // Call the method under test.
    byte[] transformedBytes = classTransformer.transform("", targetClassName, inputBytes);

    // Perform assertions.
    byte[] originalSearchBytes =
        String.format("\"%s\"", BetterMobGriefingGameRule.ORIGINAL).getBytes();
    Assert.assertThat(
        "A reference to the mobGriefing game rule was still found in the transformed class.",
        Bytes.indexOf(transformedBytes, originalSearchBytes), CoreMatchers.is(-1));

    byte[] replacementSearchBytes = BetterMobGriefingGameRule.class.getSimpleName().getBytes();
    Assert.assertThat(
        "A reference to the better mobGriefing game rule was not found in the transformed class.",
        Bytes.indexOf(transformedBytes, replacementSearchBytes), CoreMatchers.not(-1));
  }

  /**
   * Test that the mob griefing instructions are replaced when the transformation target class is
   * {@link EntityWither}.
   */
  @Test
  public void testTransform_entityWither_mobGriefingTransformed() throws IOException {
    // Set up test data.
    String targetClassName = EntityWither.class.getName();

    ClassReader classReader = new ClassReader(targetClassName);
    ClassWriter classWriter = new ClassWriter(ClassWriter.COMPUTE_MAXS);
    classReader.accept(classWriter, ClassReader.SKIP_DEBUG);
    byte[] inputBytes = classWriter.toByteArray();

    // Call the method under test.
    byte[] transformedBytes = classTransformer.transform("", targetClassName, inputBytes);

    // Perform assertions.
    byte[] originalSearchBytes =
        String.format("\"%s\"", BetterMobGriefingGameRule.ORIGINAL).getBytes();
    Assert.assertThat(
        "A reference to the mobGriefing game rule was still found in the transformed class.",
        Bytes.indexOf(transformedBytes, originalSearchBytes), CoreMatchers.is(-1));

    byte[] replacementSearchBytes = BetterMobGriefingGameRule.class.getSimpleName().getBytes();
    Assert.assertThat(
        "A reference to the better mobGriefing game rule was not found in the transformed class.",
        Bytes.indexOf(transformedBytes, replacementSearchBytes), CoreMatchers.not(-1));
  }
}