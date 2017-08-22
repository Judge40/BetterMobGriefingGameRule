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

package com.judge40.minecraft.bettermobgriefinggamerule.entity.ai;

import com.judge40.minecraft.bettermobgriefinggamerule.BetterMobGriefingGameRule;

import mockit.Deencapsulation;
import mockit.Expectations;
import mockit.Mocked;
import net.minecraft.block.BlockDoor;
import net.minecraft.entity.EntityLiving;
import net.minecraft.entity.ai.EntityAIBreakDoor;
import net.minecraft.world.IBlockAccess;
import org.hamcrest.CoreMatchers;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

/**
 * The unit tests for {@link BetterBreakDoorAiTaskTest}.
 */
public class BetterBreakDoorAiTaskTest {

  private BetterBreakDoorAiTask aiTask;

  @Mocked
  private EntityLiving entity;

  @Before
  public void setUp() {
    aiTask = new BetterBreakDoorAiTask(entity);
  }

  /**
   * Test that true is returned when the parent returns true and the entity mob griefing is true.
   */
  @Test
  public void testShouldExecute_parentTrueEntityTrue_true(@Mocked EntityAIBreakDoor parentAiTask) {
    // Record expectations.
    new Expectations(BetterMobGriefingGameRule.class) {
      {
        parentAiTask.shouldExecute();
        result = true;

        BetterMobGriefingGameRule.isMobGriefingEnabled(entity);
        result = true;
      }
    };

    // Call the method under test.
    boolean shouldExecute = aiTask.shouldExecute();

    // Perform assertions.
    Assert.assertThat("The shouldExecute flag did not match the expected value.", shouldExecute,
        CoreMatchers.is(true));
  }

  /**
   * Test that false is returned when the parent returns true and the entity mob griefing is false.
   */
  @Test
  public void testShouldExecute_parentTrueEntityFalse_false(
      @Mocked EntityAIBreakDoor parentAiTask) {
    // Record expectations.
    new Expectations(BetterMobGriefingGameRule.class) {
      {
        parentAiTask.shouldExecute();
        result = true;

        BetterMobGriefingGameRule.isMobGriefingEnabled(entity);
        result = false;
      }
    };

    // Call the method under test.
    boolean shouldExecute = aiTask.shouldExecute();

    // Perform assertions.
    Assert.assertThat("The shouldExecute flag did not match the expected value.", shouldExecute,
        CoreMatchers.is(false));
  }

  /**
   * Test that false is returned when the parent returns false, the entity mob griefing is true and
   * there is no door.
   */
  @Test
  public void testShouldExecute_parentFalseEntityTrueDoorNull_false(
      @Mocked EntityAIBreakDoor parentAiTask) {
    // Set up test data.
    // Deencapsulation.setField(aiTask, (BlockDoor) null);

    // Record expectations.
    new Expectations(BetterMobGriefingGameRule.class) {
      {
        parentAiTask.shouldExecute();
        result = false;

        BetterMobGriefingGameRule.isMobGriefingEnabled(entity);
        result = true;
      }
    };

    // Call the method under test.
    boolean shouldExecute = aiTask.shouldExecute();

    // Perform assertions.
    Assert.assertThat("The shouldExecute flag did not match the expected value.", shouldExecute,
        CoreMatchers.is(false));
  }

  /**
   * Test that true is returned when the parent returns false, the entity mob griefing is true and
   * there is a breakable door.
   */
  @Test
  public void testShouldExecute_parentFalseEntityTrueDoorBreakable_true(
      @Mocked EntityAIBreakDoor parentAiTask, @Mocked BlockDoor door) {
    // Set up test data.
    Deencapsulation.setField(aiTask, door);

    // Record expectations.
    new Expectations(BetterMobGriefingGameRule.class) {
      {
        parentAiTask.shouldExecute();
        result = false;

        BetterMobGriefingGameRule.isMobGriefingEnabled(entity);
        result = true;

        door.func_150015_f((IBlockAccess) any, 0, 0, 0);
        result = false;
      }
    };

    // Call the method under test.
    boolean shouldExecute = aiTask.shouldExecute();

    // Perform assertions.
    Assert.assertThat("The shouldExecute flag did not match the expected value.", shouldExecute,
        CoreMatchers.is(true));
  }

  /**
   * Test that false is returned when the parent returns false, the entity mob griefing is true and
   * there is an unbreakable door.
   */
  @Test
  public void testShouldExecute_parentFalseEntityTrueDoorUnbreakable_false(
      @Mocked EntityAIBreakDoor parentAiTask, @Mocked BlockDoor door) {
    // Set up test data.
    Deencapsulation.setField(aiTask, door);

    // Record expectations.
    new Expectations(BetterMobGriefingGameRule.class) {
      {
        parentAiTask.shouldExecute();
        result = false;

        BetterMobGriefingGameRule.isMobGriefingEnabled(entity);
        result = true;

        door.func_150015_f((IBlockAccess) any, 0, 0, 0);
        result = true;
      }
    };

    // Call the method under test.
    boolean shouldExecute = aiTask.shouldExecute();

    // Perform assertions.
    Assert.assertThat("The shouldExecute flag did not match the expected value.", shouldExecute,
        CoreMatchers.is(false));
  }

  /**
   * Test that false is returned when the parent returns false and the entity mob griefing is false.
   */
  @Test
  public void testShouldExecute_parentFalseEntityFalse_false(
      @Mocked EntityAIBreakDoor parentAiTask) {
    // Record expectations.
    new Expectations(BetterMobGriefingGameRule.class) {
      {
        parentAiTask.shouldExecute();
        result = false;

        BetterMobGriefingGameRule.isMobGriefingEnabled(entity);
        result = false;
      }
    };

    // Call the method under test.
    boolean shouldExecute = aiTask.shouldExecute();

    // Perform assertions.
    Assert.assertThat("The shouldExecute flag did not match the expected value.", shouldExecute,
        CoreMatchers.is(false));
  }
}
