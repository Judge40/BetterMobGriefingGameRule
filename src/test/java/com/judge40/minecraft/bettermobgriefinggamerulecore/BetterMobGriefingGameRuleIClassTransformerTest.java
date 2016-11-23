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

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import org.hamcrest.CoreMatchers;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.objectweb.asm.ClassReader;
import org.objectweb.asm.ClassWriter;

import com.google.common.primitives.Bytes;

import mockit.Deencapsulation;
import mockit.Mock;
import mockit.MockUp;
import net.minecraft.entity.EntityList;
import net.minecraft.entity.EntityLivingBase;
import net.minecraft.entity.boss.EntityDragon;
import net.minecraft.entity.monster.EntityEnderman;
import net.minecraft.launchwrapper.Launch;

/**
 * Tests for BetterMobGriefingGameRuleIClassTransformer
 */
public class BetterMobGriefingGameRuleIClassTransformerTest {

  private BetterMobGriefingGameRuleIClassTransformer betterMobGriefingGameRuleIClassTransformer;

  /**
   * @throws java.lang.Exception
   */
  @Before
  public void setUp() throws Exception {
    Map<String, Object> blackboard = new HashMap<>();
    blackboard.put("fml.deobfuscatedEnvironment", true);
    Launch.blackboard = blackboard;

    betterMobGriefingGameRuleIClassTransformer = new BetterMobGriefingGameRuleIClassTransformer();
  }

  /**
   * @throws java.lang.Exception
   */
  @After
  public void tearDown() throws Exception {
    Launch.blackboard = null;
    betterMobGriefingGameRuleIClassTransformer = null;
  }

  /**
   * Test that the game rule transformation method is not called when the class is not handled
   */
  @Test
  public void testTransform_unhandledClass_transformGameRuleCalled() {
    new MockUp<BetterMobGriefingGameRuleIClassTransformer>() {
      @Mock(invocations = 0)
      byte[] transformMobGriefingGameRule(String transformedName, byte[] basicClass) {
        return new byte[0];
      }
    };

    betterMobGriefingGameRuleIClassTransformer.transform(this.getClass().getName(),
        this.getClass().getName(), new byte[0]);
  }

  /**
   * Test that the game rule transformation method is called with the correct parameters for
   * EntityDragon
   */
  @Test
  public void testTransform_entityDragon_transformGameRuleCalled() {
    byte[] inputBasicClass = new byte[0];

    new MockUp<BetterMobGriefingGameRuleIClassTransformer>() {
      @Mock(invocations = 1)
      byte[] transformMobGriefingGameRule(String transformedName, byte[] basicClass) {
        Assert.assertThat("Bytes to be transformed does not match the expected bytes.", basicClass,
            CoreMatchers.is(inputBasicClass));
        return basicClass;
      }
    };

    betterMobGriefingGameRuleIClassTransformer.transform(EntityDragon.class.getName(),
        EntityDragon.class.getName(), inputBasicClass);
  }

  /**
   * Test that the game rule transformation method is called with the correct parameters for
   * EntityEnderman
   */
  @Test
  public void testTransform_entityEnderman_transformGameRuleCalled() {
    byte[] inputBasicClass = new byte[0];

    new MockUp<BetterMobGriefingGameRuleIClassTransformer>() {
      @Mock(invocations = 1)
      byte[] transformMobGriefingGameRule(String transformedName, byte[] basicClass) {
        Assert.assertThat("Bytes to be transformed does not match the expected bytes.", basicClass,
            CoreMatchers.is(inputBasicClass));
        return basicClass;
      }
    };

    betterMobGriefingGameRuleIClassTransformer.transform(EntityEnderman.class.getName(),
        EntityEnderman.class.getName(), inputBasicClass);
  }

  /**
   * Test that the transform is ran as expected on EntityEnderman
   */
  @Test
  public void testTransformMobGriefingGameRule_entityEnderman_bytesTransformed()
      throws IOException {
    ClassReader classReader = new ClassReader(EntityEnderman.class.getName());
    ClassWriter classWriter = new ClassWriter(ClassWriter.COMPUTE_MAXS);
    classReader.accept(classWriter, ClassReader.SKIP_DEBUG);
    byte[] entityEndermanBytes = classWriter.toByteArray();

    byte[] transformedBytes =
        Deencapsulation.invoke(BetterMobGriefingGameRuleIClassTransformer.class,
            "transformMobGriefingGameRule", EntityEnderman.class.getName(), entityEndermanBytes);

    Assert.assertThat("mobGriefing game rule was still found in the transformed bytes.",
        Bytes.indexOf(transformedBytes, "\"mobGriefing\"".getBytes()), CoreMatchers.is(-1));
    Assert.assertThat("mobGriefingEnderman game rule was not found in the transformed bytes.",
        Bytes.indexOf(transformedBytes,
            ((String) EntityList.classToStringMapping.get(EntityEnderman.class)).getBytes()),
        CoreMatchers.not(-1));
  }

  /**
   * Test that the transform does not change the class bytes when no mobGriefing rule is used in the
   * class
   */
  @Test
  public void testTransformMobGriefingGameRule_noMobGriefingRule_bytesNotTransformed()
      throws IOException {
    ClassReader classReader = new ClassReader(EntityLivingBase.class.getName());
    ClassWriter classWriter = new ClassWriter(ClassWriter.COMPUTE_MAXS);
    classReader.accept(classWriter, ClassReader.SKIP_DEBUG);
    byte[] entityLivingBaseBytes = classWriter.toByteArray();

    byte[] transformedBytes =
        Deencapsulation.invoke(BetterMobGriefingGameRuleIClassTransformer.class,
            "transformMobGriefingGameRule", "", entityLivingBaseBytes);

    Assert.assertThat("mobGriefing game rule was still found in the transformed bytes.",
        transformedBytes, CoreMatchers.is(entityLivingBaseBytes));
  }
}
