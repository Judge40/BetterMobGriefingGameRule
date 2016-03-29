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
import mockit.Invocation;
import mockit.Mock;
import mockit.MockUp;
import net.minecraft.block.Block;
import net.minecraft.entity.Entity;
import net.minecraft.entity.EntityLiving;
import net.minecraft.entity.boss.EntityWither;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.world.GameRules;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;

/**
 * Tests for BetterMobGriefingGameRuleEntityAIOverrideMobGriefingBehaviour
 */
public class BetterMobGriefingGameRuleEntityAIOverrideMobGriefingBehaviourTest {

  private GameRules gameRules;
  private World world;

  /**
   * @throws java.lang.Exception
   */
  @Before
  public void setUp() throws Exception {
    gameRules = new GameRules();
    BetterMobGriefingGameRule.addMobGriefingGameRules(gameRules);

    world = Deencapsulation.newUninitializedInstance(World.class);

    new MockUp<World>() {
      @Mock
      GameRules getGameRules() {
        return gameRules;
      }
    };
  }

  /**
   * @throws java.lang.Exception
   */
  @After
  public void tearDown() throws Exception {
    gameRules = null;
    world = null;
  }

  /**
   * Test that the fields have the expected values after calling constructor
   */
  @Test
  public void testConstructor_fieldsInitalised() {
    EntityLiving entityLiving = Deencapsulation.newUninitializedInstance(EntityLiving.class);
    BetterMobGriefingGameRuleEntityAIOverrideMobGriefingBehaviour aiTask =
        new BetterMobGriefingGameRuleEntityAIOverrideMobGriefingBehaviour(entityLiving);

    EntityLiving entityField = Deencapsulation.getField(aiTask, "entity");
    Assert.assertThat("Entity field was not correctly populated.", entityLiving,
        CoreMatchers.is(entityField));

    int lastWitherBlockBreakCooldownField =
        Deencapsulation.getField(aiTask, "lastWitherBlockBreakCooldown");
    Assert.assertThat("lastWitherBlockBreakCooldown field was not correctly populated.",
        lastWitherBlockBreakCooldownField, CoreMatchers.is(20));
  }

  /**
   * Test that shouldExecute returns true
   */
  @Test
  public void testShouldExecute_true() {
    BetterMobGriefingGameRuleEntityAIOverrideMobGriefingBehaviour aiTask =
        new BetterMobGriefingGameRuleEntityAIOverrideMobGriefingBehaviour(null);
    boolean shouldExecute = aiTask.shouldExecute();
    Assert.assertThat("shouldExecute should return true", shouldExecute, CoreMatchers.is(true));
  }

  /**
   * Test when block break is performed when block break cooldown is zero and last cooldown value
   * was not zero
   */
  @Test
  public void testUpdateTask_entityWitherOriginalMobGriefingFalseBetterMobGriefingTrueWitherChargedLastBreakBlockCooldownTenBreakBlockCooldownZero_blockBreakPerformed() {
    new MockUp<EntityWither>() {
      @Mock
      int func_82212_n() {
        return 0;
      }
    };

    new MockUp<World>() {
      @Mock
      Block getBlock(int p_147439_1_, int p_147439_2_, int p_147439_3_) {
        return Deencapsulation.newUninitializedInstance(Block.class);
      }

      @Mock(minInvocations = 1)
      boolean func_147480_a(Invocation invocation, int p_147480_1_, int p_147480_2_,
          int p_147480_3_, boolean p_147480_4_) {
        return invocation.getInvocationCount() == 1;
      }

      @Mock
      void playAuxSFXAtEntity(EntityPlayer p_72889_1_, int p_72889_2_, int p_72889_3_,
          int p_72889_4_, int p_72889_5_, int p_72889_6_) {

      }
    };

    new MockUp<Block>() {
      @Mock
      boolean isAir(IBlockAccess world, int x, int y, int z) {
        return false;
      }

      @Mock
      boolean canEntityDestroy(IBlockAccess world, int x, int y, int z, Entity entity) {
        return true;
      }
    };

    gameRules.setOrCreateGameRule(BetterMobGriefingGameRule.ORIGINAL, "false");
    gameRules.setOrCreateGameRule(BetterMobGriefingGameRule.WITHER, "true");

    EntityWither entityWither = new EntityWither(null);
    entityWither.worldObj = world;
    Deencapsulation.setField(entityWither, "field_82222_j", 0);

    BetterMobGriefingGameRuleEntityAIOverrideMobGriefingBehaviour aiTask =
        new BetterMobGriefingGameRuleEntityAIOverrideMobGriefingBehaviour(entityWither);
    Deencapsulation.setField(aiTask, "lastWitherBlockBreakCooldown", 10);
    aiTask.updateTask();

    int blockBreakCooldown = Deencapsulation.getField(entityWither, "field_82222_j");
    Assert.assertThat("Field field_82222_j should not have been changed.", blockBreakCooldown,
        CoreMatchers.is(0));

    int lastBlockBreakCooldown = Deencapsulation.getField(aiTask, "lastWitherBlockBreakCooldown");
    Assert.assertThat("Field lastWitherBlockBreakCooldown should have been updated.",
        lastBlockBreakCooldown, CoreMatchers.is(0));
  }

  /**
   * Test when block break is not performed when block break cooldown is not zero
   */
  @Test
  public void testUpdateTask_entityWitherOriginalMobGriefingFalseBetterMobGriefingTrueWitherChargedLastBreakBlockCooldownTenBreakBlockCooldownNine_noActionPerformed() {
    new MockUp<EntityWither>() {
      @Mock
      int func_82212_n() {
        return 0;
      }
    };

    new MockUp<World>() {
      @Mock
      Block getBlock(int p_147439_1_, int p_147439_2_, int p_147439_3_) {
        return Deencapsulation.newUninitializedInstance(Block.class);
      }

      @Mock(invocations = 0)
      boolean func_147480_a(int p_147480_1_, int p_147480_2_, int p_147480_3_,
          boolean p_147480_4_) {
        return true;
      }
    };

    new MockUp<Block>() {
      @Mock
      boolean isAir(IBlockAccess world, int x, int y, int z) {
        return false;
      }

      @Mock
      boolean canEntityDestroy(IBlockAccess world, int x, int y, int z, Entity entity) {
        return true;
      }
    };

    gameRules.setOrCreateGameRule(BetterMobGriefingGameRule.ORIGINAL, "false");
    gameRules.setOrCreateGameRule(BetterMobGriefingGameRule.WITHER, "true");

    EntityWither entityWither = new EntityWither(null);
    entityWither.worldObj = world;
    Deencapsulation.setField(entityWither, "field_82222_j", 9);

    BetterMobGriefingGameRuleEntityAIOverrideMobGriefingBehaviour aiTask =
        new BetterMobGriefingGameRuleEntityAIOverrideMobGriefingBehaviour(entityWither);
    Deencapsulation.setField(aiTask, "lastWitherBlockBreakCooldown", 10);
    aiTask.updateTask();

    int blockBreakCooldown = Deencapsulation.getField(entityWither, "field_82222_j");
    Assert.assertThat("Field field_82222_j should not have been changed.", blockBreakCooldown,
        CoreMatchers.is(9));

    int lastBlockBreakCooldown = Deencapsulation.getField(aiTask, "lastWitherBlockBreakCooldown");
    Assert.assertThat("Field lastWitherBlockBreakCooldown should have been updated.",
        lastBlockBreakCooldown, CoreMatchers.is(9));
  }

  /**
   * Test when block break is not performed when last block break cooldown was zero, indicating that
   * the code will have already ran
   */
  @Test
  public void testUpdateTask_entityWitherOriginalMobGriefingFalseBetterMobGriefingTrueWitherChargedLastBreakBlockCooldownZeroBreakBlockCooldownTen_noActionPerformed() {
    new MockUp<EntityWither>() {
      @Mock
      int func_82212_n() {
        return 0;
      }
    };

    new MockUp<World>() {
      @Mock
      Block getBlock(int p_147439_1_, int p_147439_2_, int p_147439_3_) {
        return Deencapsulation.newUninitializedInstance(Block.class);
      }

      @Mock(invocations = 0)
      boolean func_147480_a(int p_147480_1_, int p_147480_2_, int p_147480_3_,
          boolean p_147480_4_) {
        return true;
      }
    };

    new MockUp<Block>() {
      @Mock
      boolean isAir(IBlockAccess world, int x, int y, int z) {
        return false;
      }

      @Mock
      boolean canEntityDestroy(IBlockAccess world, int x, int y, int z, Entity entity) {
        return true;
      }
    };

    gameRules.setOrCreateGameRule(BetterMobGriefingGameRule.ORIGINAL, "false");
    gameRules.setOrCreateGameRule(BetterMobGriefingGameRule.WITHER, "true");

    EntityWither entityWither = new EntityWither(null);
    entityWither.worldObj = world;
    Deencapsulation.setField(entityWither, "field_82222_j", 10);

    BetterMobGriefingGameRuleEntityAIOverrideMobGriefingBehaviour aiTask =
        new BetterMobGriefingGameRuleEntityAIOverrideMobGriefingBehaviour(entityWither);
    Deencapsulation.setField(aiTask, "lastWitherBlockBreakCooldown", 0);
    aiTask.updateTask();

    int blockBreakCooldown = Deencapsulation.getField(entityWither, "field_82222_j");
    Assert.assertThat("Field field_82222_j should not have been changed.", blockBreakCooldown,
        CoreMatchers.is(10));

    int lastBlockBreakCooldown = Deencapsulation.getField(aiTask, "lastWitherBlockBreakCooldown");
    Assert.assertThat("Field lastWitherBlockBreakCooldown should have been updated.",
        lastBlockBreakCooldown, CoreMatchers.is(10));
  }

  /**
   * Test that the block break is not performed when Wither is still charging
   */
  @Test
  public void testUpdateTask_entityWitherOriginalMobGriefingFalseBetterMobGriefingTrueWitherNotCharged_noActionPerformed() {
    new MockUp<EntityWither>() {
      @Mock
      int func_82212_n() {
        return 1;
      }
    };

    gameRules.setOrCreateGameRule(BetterMobGriefingGameRule.ORIGINAL, "false");
    gameRules.setOrCreateGameRule(BetterMobGriefingGameRule.WITHER, "true");

    EntityWither entityWither = new EntityWither(null);
    entityWither.worldObj = world;
    Deencapsulation.setField(entityWither, "field_82222_j", 10);

    BetterMobGriefingGameRuleEntityAIOverrideMobGriefingBehaviour aiTask =
        new BetterMobGriefingGameRuleEntityAIOverrideMobGriefingBehaviour(entityWither);
    Deencapsulation.setField(aiTask, "lastWitherBlockBreakCooldown", 15);
    aiTask.updateTask();

    int blockBreakCooldown = Deencapsulation.getField(entityWither, "field_82222_j");
    Assert.assertThat("Field field_82222_j should not have been changed.", blockBreakCooldown,
        CoreMatchers.is(10));

    int lastBlockBreakCooldown = Deencapsulation.getField(aiTask, "lastWitherBlockBreakCooldown");
    Assert.assertThat("Field lastWitherBlockBreakCooldown should not have been changed.",
        lastBlockBreakCooldown, CoreMatchers.is(15));
  }

  /**
   * Test that the block break cooldown is reset when original mobGriefing rule is true and better
   * mobGriefing rule is false
   */
  @Test
  public void testUpdateTask_entityWitherOriginalMobGriefingTrueBetterMobGriefingFalse_blockBreakCooldownReset() {
    gameRules.setOrCreateGameRule(BetterMobGriefingGameRule.ORIGINAL, "true");
    gameRules.setOrCreateGameRule(BetterMobGriefingGameRule.WITHER, "false");

    EntityWither entityWither = new EntityWither(null);
    entityWither.worldObj = world;
    Deencapsulation.setField(entityWither, "field_82222_j", 10);

    BetterMobGriefingGameRuleEntityAIOverrideMobGriefingBehaviour aiTask =
        new BetterMobGriefingGameRuleEntityAIOverrideMobGriefingBehaviour(entityWither);
    Deencapsulation.setField(aiTask, "lastWitherBlockBreakCooldown", 15);
    aiTask.updateTask();

    int blockBreakCooldown = Deencapsulation.getField(entityWither, "field_82222_j");
    Assert.assertThat("Field field_82222_j should have been reset.", blockBreakCooldown,
        CoreMatchers.is(20));

    int lastBlockBreakCooldown = Deencapsulation.getField(aiTask, "lastWitherBlockBreakCooldown");
    Assert.assertThat("Field lastWitherBlockBreakCooldown should not have been changed.",
        lastBlockBreakCooldown, CoreMatchers.is(15));
  }

  /**
   * Test that mob griefing is not performed for EntityWither when mobGriefing is true and the block
   * break cooldown is zero and previous cooldown is zero
   */
  @Test
  public void testUpdateTask_entityWitherOriginalMobGriefingAndBetterMobGriefingSame_noActionPerformed() {
    new MockUp<EntityWither>() {
      @Mock(invocations = 0)
      int func_82212_n() {
        return 0;
      }
    };

    gameRules.setOrCreateGameRule(BetterMobGriefingGameRule.ORIGINAL, "true");
    gameRules.setOrCreateGameRule(BetterMobGriefingGameRule.WITHER, "true");

    EntityWither entityWither = new EntityWither(null);
    entityWither.worldObj = world;
    Deencapsulation.setField(entityWither, "field_82222_j", 10);

    BetterMobGriefingGameRuleEntityAIOverrideMobGriefingBehaviour aiTask =
        new BetterMobGriefingGameRuleEntityAIOverrideMobGriefingBehaviour(entityWither);
    Deencapsulation.setField(aiTask, "lastWitherBlockBreakCooldown", 15);
    aiTask.updateTask();

    int blockBreakCooldown = Deencapsulation.getField(entityWither, "field_82222_j");
    Assert.assertThat("Field field_82222_j should not have been changed.", blockBreakCooldown,
        CoreMatchers.is(10));

    int lastBlockBreakCooldown = Deencapsulation.getField(aiTask, "lastWitherBlockBreakCooldown");
    Assert.assertThat("Field lastWitherBlockBreakCooldown should not have been changed.",
        lastBlockBreakCooldown, CoreMatchers.is(15));
  }
}
