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

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.judge40.minecraft.bettermobgriefinggamerule.BetterMobGriefingGameRule;

import mockit.Deencapsulation;
import mockit.Mock;
import mockit.MockUp;
import net.minecraft.block.Block;
import net.minecraft.block.BlockGrass;
import net.minecraft.block.BlockTallGrass;
import net.minecraft.entity.EntityLiving;
import net.minecraft.init.Blocks;
import net.minecraft.world.GameRules;
import net.minecraft.world.World;

/**
 *
 */
public class BetterMobGriefingGameRuleEntityAIEatGrassTest {

  private BetterMobGriefingGameRuleEntityAIEatGrass betterMobGriefingGameRuleEntityAIEatGrass;
  private World world;

  /**
   * @throws java.lang.Exception
   */
  @Before
  public void setUp() throws Exception {
    EntityLiving entityLiving = Deencapsulation.newUninitializedInstance(EntityLiving.class);

    world = Deencapsulation.newUninitializedInstance(World.class);
    entityLiving.worldObj = world;

    new MockUp<World>() {
      @Mock
      GameRules getGameRules() {
        return new GameRules();
      }
    };

    betterMobGriefingGameRuleEntityAIEatGrass =
        new BetterMobGriefingGameRuleEntityAIEatGrass(entityLiving);
    Deencapsulation.setField(betterMobGriefingGameRuleEntityAIEatGrass, "field_151502_a", 5);
  }

  /**
   * @throws java.lang.Exception
   */
  @After
  public void tearDown() throws Exception {
    betterMobGriefingGameRuleEntityAIEatGrass = null;
    world = null;
  }

  /**
   * Test that block damage occurs for tall grass when mobGriefing is true
   */
  @Test
  public void testUpdateTask_tallGrassMobGriefingTrue_doMobGriefing() {
    new MockUp<World>() {
      @Mock
      Block getBlock(int p_147439_1_, int p_147439_2_, int p_147439_3_) {
        return Blocks.tallgrass;
      }

      @Mock(invocations = 1)
      boolean func_147480_a(int p_147480_1_, int p_147480_2_, int p_147480_3_,
          boolean p_147480_4_) {
        return true;
      }

      @Mock(invocations = 0)
      boolean setBlock(int p_147465_1_, int p_147465_2_, int p_147465_3_, Block p_147465_4_,
          int p_147465_5_, int p_147465_6_) {
        return true;
      }
    };

    new MockUp<BetterMobGriefingGameRule>() {
      @Mock
      boolean isMobGriefingEnabled(EntityLiving entity) {
        return true;
      }
    };

    BlockTallGrass blockTallGrass = Deencapsulation.newUninitializedInstance(BlockTallGrass.class);
    Deencapsulation.setField(Blocks.class, "tallgrass", blockTallGrass);

    betterMobGriefingGameRuleEntityAIEatGrass.updateTask();
  }

  /**
   * Test that block damage does not occur for tall grass when mobGriefing is false
   */
  @Test
  public void testUpdateTask_tallGrassMobGriefingFalse_noMobGriefing() {
    new MockUp<World>() {
      @Mock
      Block getBlock(int p_147439_1_, int p_147439_2_, int p_147439_3_) {
        return Blocks.tallgrass;
      }

      @Mock(invocations = 0)
      boolean func_147480_a(int p_147480_1_, int p_147480_2_, int p_147480_3_,
          boolean p_147480_4_) {
        return true;
      }

      @Mock(invocations = 0)
      boolean setBlock(int p_147465_1_, int p_147465_2_, int p_147465_3_, Block p_147465_4_,
          int p_147465_5_, int p_147465_6_) {
        return true;
      }
    };

    new MockUp<BetterMobGriefingGameRule>() {
      @Mock
      boolean isMobGriefingEnabled(EntityLiving entity) {
        return false;
      }
    };

    BlockTallGrass blockTallGrass = Deencapsulation.newUninitializedInstance(BlockTallGrass.class);
    Deencapsulation.setField(Blocks.class, "tallgrass", blockTallGrass);

    betterMobGriefingGameRuleEntityAIEatGrass.updateTask();
  }

  /**
   * Test that block damage occurs for grass when mobGriefing is true
   */
  @Test
  public void testUpdateTask_grassMobGriefingTrue_doMobGriefing() {
    new MockUp<World>() {
      @Mock
      Block getBlock(int p_147439_1_, int p_147439_2_, int p_147439_3_) {
        return Blocks.grass;
      }

      @Mock(invocations = 0)
      boolean func_147480_a(int p_147480_1_, int p_147480_2_, int p_147480_3_,
          boolean p_147480_4_) {
        return true;
      }

      @Mock
      void playAuxSFX(int p_72926_1_, int p_72926_2_, int p_72926_3_, int p_72926_4_,
          int p_72926_5_) {

      }

      @Mock(invocations = 1)
      boolean setBlock(int p_147465_1_, int p_147465_2_, int p_147465_3_, Block p_147465_4_,
          int p_147465_5_, int p_147465_6_) {
        return true;
      }
    };

    new MockUp<BetterMobGriefingGameRule>() {
      @Mock
      boolean isMobGriefingEnabled(EntityLiving entity) {
        return true;
      }
    };

    BlockGrass blockGrass = Deencapsulation.newUninitializedInstance(BlockGrass.class);
    Deencapsulation.setField(Blocks.class, "grass", blockGrass);

    betterMobGriefingGameRuleEntityAIEatGrass.updateTask();
  }

  /**
   * Test that block damage does not occur for grass when mobGriefing is false
   */
  @Test
  public void testUpdateTask_grassMobGriefingFalse_noMobGriefing() {
    new MockUp<World>() {
      @Mock
      Block getBlock(int p_147439_1_, int p_147439_2_, int p_147439_3_) {
        return Blocks.grass;
      }

      @Mock(invocations = 0)
      boolean func_147480_a(int p_147480_1_, int p_147480_2_, int p_147480_3_,
          boolean p_147480_4_) {
        return true;
      }

      @Mock
      void playAuxSFX(int p_72926_1_, int p_72926_2_, int p_72926_3_, int p_72926_4_,
          int p_72926_5_) {

      }

      @Mock(invocations = 0)
      boolean setBlock(int p_147465_1_, int p_147465_2_, int p_147465_3_, Block p_147465_4_,
          int p_147465_5_, int p_147465_6_) {
        return true;
      }
    };

    new MockUp<BetterMobGriefingGameRule>() {
      @Mock
      boolean isMobGriefingEnabled(EntityLiving entity) {
        return false;
      }
    };

    BlockGrass blockGrass = Deencapsulation.newUninitializedInstance(BlockGrass.class);
    Deencapsulation.setField(Blocks.class, "grass", blockGrass);

    betterMobGriefingGameRuleEntityAIEatGrass.updateTask();
  }

  /**
   * Test that block damage does not occur for non-grass blocks
   */
  @Test
  public void testUpdateTask_nonGrassBlockMobGriefingTrue_noMobGriefing() {
    new MockUp<World>() {
      @Mock
      Block getBlock(int p_147439_1_, int p_147439_2_, int p_147439_3_) {
        return Blocks.air;
      }

      @Mock(invocations = 0)
      boolean func_147480_a(int p_147480_1_, int p_147480_2_, int p_147480_3_,
          boolean p_147480_4_) {
        return true;
      }

      @Mock
      void playAuxSFX(int p_72926_1_, int p_72926_2_, int p_72926_3_, int p_72926_4_,
          int p_72926_5_) {

      }

      @Mock(invocations = 0)
      boolean setBlock(int p_147465_1_, int p_147465_2_, int p_147465_3_, Block p_147465_4_,
          int p_147465_5_, int p_147465_6_) {
        return true;
      }
    };

    new MockUp<BetterMobGriefingGameRule>() {
      @Mock
      boolean isMobGriefingEnabled(EntityLiving entity) {
        return true;
      }
    };

    Block block = Deencapsulation.newUninitializedInstance(Block.class);
    Deencapsulation.setField(Blocks.class, "air", block);

    betterMobGriefingGameRuleEntityAIEatGrass.updateTask();
  }

  /**
   * Test that when EntityAIEatGrass field not five no processing is done
   */
  @Test
  public void testOnUpdate_fieldNotFive_noProcessing() {
    new MockUp<GameRules>() {
      @Mock(invocations = 0)
      boolean getGameRuleBooleanValue(String p_82766_1_) {
        return true;
      }
    };

    Deencapsulation.setField(betterMobGriefingGameRuleEntityAIEatGrass, "field_151502_a", 2);
    betterMobGriefingGameRuleEntityAIEatGrass.updateTask();
  }
}
