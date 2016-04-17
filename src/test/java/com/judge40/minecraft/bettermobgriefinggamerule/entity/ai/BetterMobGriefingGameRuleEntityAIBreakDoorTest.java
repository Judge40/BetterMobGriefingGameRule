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

import org.hamcrest.CoreMatchers;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.judge40.minecraft.bettermobgriefinggamerule.BetterMobGriefingGameRule;

import mockit.Deencapsulation;
import mockit.Mock;
import mockit.MockUp;
import net.minecraft.block.BlockDoor;
import net.minecraft.entity.EntityLiving;
import net.minecraft.entity.ai.EntityAIBreakDoor;
import net.minecraft.world.GameRules;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;

/**
 *
 */
public class BetterMobGriefingGameRuleEntityAIBreakDoorTest {

  BetterMobGriefingGameRuleEntityAIBreakDoor betterMobGriefingGameRuleEntityAIBreakDoor;
  private World world;

  /**
   * @throws java.lang.Exception
   */
  @Before
  public void setUp() throws Exception {
    EntityLiving entityLiving = Deencapsulation.newUninitializedInstance(EntityLiving.class);
    betterMobGriefingGameRuleEntityAIBreakDoor =
        new BetterMobGriefingGameRuleEntityAIBreakDoor(entityLiving);

    world = Deencapsulation.newUninitializedInstance(World.class);
    entityLiving.worldObj = world;

    new MockUp<World>() {
      @Mock
      GameRules getGameRules() {
        return new GameRules();
      }
    };
  }

  /**
   * @throws java.lang.Exception
   */
  @After
  public void tearDown() throws Exception {
    betterMobGriefingGameRuleEntityAIBreakDoor = null;
  }

  /**
   * Test that when the parent class returns true and mobGriefing is true that true is returned
   */
  @Test
  public void testShouldExecute_parentTrueMobGriefingTrue_true() {
    new MockUp<EntityAIBreakDoor>() {
      @Mock
      boolean shouldExecute() {
        return true;
      }
    };

    new MockUp<BetterMobGriefingGameRule>() {
      @Mock
      boolean isMobGriefingEnabled(EntityLiving entity) {
        return true;
      }
    };

    boolean shouldExecute = betterMobGriefingGameRuleEntityAIBreakDoor.shouldExecute();
    Assert.assertThat("AI task shouldExecute should have returned true.", shouldExecute,
        CoreMatchers.is(true));
  }

  /**
   * Test that when the parent class returns true and mobGriefing is false that false is returned
   */
  @Test
  public void testShouldExecute_parentTrueMobGriefingFalse_false() {
    new MockUp<EntityAIBreakDoor>() {
      @Mock
      boolean shouldExecute() {
        return true;
      }
    };

    new MockUp<BetterMobGriefingGameRule>() {
      @Mock
      boolean isMobGriefingEnabled(EntityLiving entity) {
        return false;
      }
    };

    boolean shouldExecute = betterMobGriefingGameRuleEntityAIBreakDoor.shouldExecute();
    Assert.assertThat("AI task shouldExecute should have returned false.", shouldExecute,
        CoreMatchers.is(false));
  }

  /**
   * Test that when the parent class returns false, mobGriefing is true and the BlockDoor function
   * is true that false is returned
   */
  @Test
  public void testShouldExecute_parentFalseMobGriefingTrueBlockDoorFunctionTrue_false() {
    new MockUp<EntityAIBreakDoor>() {
      @Mock
      boolean shouldExecute() {
        return false;
      }
    };

    new MockUp<BetterMobGriefingGameRule>() {
      @Mock
      boolean isMobGriefingEnabled(EntityLiving entity) {
        return true;
      }
    };

    new MockUp<BlockDoor>() {
      @Mock
      boolean func_150015_f(IBlockAccess p_150015_1_, int p_150015_2_, int p_150015_3_,
          int p_150015_4_) {
        return true;
      }
    };

    Deencapsulation.setField(betterMobGriefingGameRuleEntityAIBreakDoor, "field_151504_e",
        Deencapsulation.newUninitializedInstance(BlockDoor.class));

    boolean shouldExecute = betterMobGriefingGameRuleEntityAIBreakDoor.shouldExecute();
    Assert.assertThat("AI task shouldExecute should have returned false.", shouldExecute,
        CoreMatchers.is(false));
  }

  /**
   * Test that when the parent class returns false, mobGriefing is true and the BlockDoor function
   * is false that true is returned
   */
  @Test
  public void testShouldExecute_parentFalseMobGriefingTrueBlockDoorFunctionFalse_true() {
    new MockUp<EntityAIBreakDoor>() {
      @Mock
      boolean shouldExecute() {
        return false;
      }
    };

    new MockUp<BetterMobGriefingGameRule>() {
      @Mock
      boolean isMobGriefingEnabled(EntityLiving entity) {
        return true;
      }
    };

    new MockUp<BlockDoor>() {
      @Mock
      boolean func_150015_f(IBlockAccess p_150015_1_, int p_150015_2_, int p_150015_3_,
          int p_150015_4_) {
        return false;
      }
    };

    Deencapsulation.setField(betterMobGriefingGameRuleEntityAIBreakDoor, "field_151504_e",
        Deencapsulation.newUninitializedInstance(BlockDoor.class));

    boolean shouldExecute = betterMobGriefingGameRuleEntityAIBreakDoor.shouldExecute();
    Assert.assertThat("AI task shouldExecute should have returned true.", shouldExecute,
        CoreMatchers.is(true));
  }

  /**
   * Test that when the parent class returns false, mobGriefing is true and the BlockDoor is null
   * that false is returned
   */
  @Test
  public void testShouldExecute_parentFalseMobGriefingTrueBlockDoorNull_false() {
    new MockUp<EntityAIBreakDoor>() {
      @Mock
      boolean shouldExecute() {
        return false;
      }
    };

    new MockUp<BetterMobGriefingGameRule>() {
      @Mock
      boolean isMobGriefingEnabled(EntityLiving entity) {
        return true;
      }
    };

    Deencapsulation.setField(betterMobGriefingGameRuleEntityAIBreakDoor, "field_151504_e", null);

    boolean shouldExecute = betterMobGriefingGameRuleEntityAIBreakDoor.shouldExecute();
    Assert.assertThat("AI task shouldExecute should have returned false.", shouldExecute,
        CoreMatchers.is(false));
  }

  /**
   * Test that when the parent class returns false and mobGriefing is false that false is returned
   */
  @Test
  public void testShouldExecute_parentFalseMobGriefingFalse_false() {
    new MockUp<EntityAIBreakDoor>() {
      @Mock
      boolean shouldExecute() {
        return false;
      }
    };

    new MockUp<BetterMobGriefingGameRule>() {
      @Mock
      boolean isMobGriefingEnabled(EntityLiving entity) {
        return false;
      }
    };

    boolean shouldExecute = betterMobGriefingGameRuleEntityAIBreakDoor.shouldExecute();
    Assert.assertThat("AI task shouldExecute should have returned false.", shouldExecute,
        CoreMatchers.is(false));
  }
}
