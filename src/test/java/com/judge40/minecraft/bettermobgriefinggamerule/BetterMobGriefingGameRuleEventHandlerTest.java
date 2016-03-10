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
package com.judge40.minecraft.bettermobgriefinggamerule;

import java.io.PrintStream;
import java.lang.reflect.Field;

import org.hamcrest.CoreMatchers;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.google.common.collect.Lists;

import mockit.Deencapsulation;
import mockit.Mock;
import mockit.MockUp;
import net.minecraft.entity.EntityLivingBase;
import net.minecraft.world.Explosion;
import net.minecraft.world.GameRules;
import net.minecraft.world.World;
import net.minecraftforge.event.world.ExplosionEvent.Detonate;

/**
 * Tests for BetterMobGriefingGameRuleEventHandler
 */
public class BetterMobGriefingGameRuleEventHandlerTest {

  private BetterMobGriefingGameRuleEventHandler eventHandler;
  private GameRules gameRules;
  private World world;

  /**
   * @throws java.lang.Exception
   */
  @Before
  public void setUp() throws Exception {
    eventHandler = new BetterMobGriefingGameRuleEventHandler();

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
    eventHandler = null;
  }

  /**
   * Test that explosion does damage blocks when original rule is true and entity specific rule is
   * true
   */
  @Test
  public void testOnDetonateEvent_mobGriefingTrueBetterMobGriefingTrue_blockDamage() {
    new MockUp<BetterMobGriefingGameRule>() {
      @Mock
      String getMobGriefingRule(GameRules gameRules, EntityLivingBase entity) {
        return "betterMobGriefing";
      }
    };

    gameRules.setOrCreateGameRule(BetterMobGriefingGameRule.ORIGINAL, "true");
    gameRules.setOrCreateGameRule("betterMobGriefing", "true");

    Explosion explosion = Deencapsulation.newUninitializedInstance(Explosion.class);
    Deencapsulation.setField(explosion, "affectedBlockPositions", Lists.newArrayList("dummyData"));
    Deencapsulation.setField(explosion, "isSmoking", true);

    Detonate detonateEvent = new Detonate(world, explosion, null);
    eventHandler.onDetonateEvent(detonateEvent);


    Assert.assertThat("Affected block position list size should be 1.",
        explosion.affectedBlockPositions.size(), CoreMatchers.is(1));
    Assert.assertThat("isSmoking should be true",
        (Boolean) Deencapsulation.getField(explosion, "isSmoking"), CoreMatchers.is(true));
  }

  /**
   * Test that explosion does not damage blocks when original rule is true and entity specific rule
   * is false
   */
  @Test
  public void testOnDetonateEvent_mobGriefingTrueBetterMobGriefingFalse_noBlockDamage() {
    new MockUp<BetterMobGriefingGameRule>() {
      @Mock
      String getMobGriefingRule(GameRules gameRules, EntityLivingBase entity) {
        return "betterMobGriefing";
      }
    };

    gameRules.setOrCreateGameRule(BetterMobGriefingGameRule.ORIGINAL, "true");
    gameRules.setOrCreateGameRule("betterMobGriefing", "false");

    Explosion explosion = Deencapsulation.newUninitializedInstance(Explosion.class);
    Deencapsulation.setField(explosion, "affectedBlockPositions", Lists.newArrayList("dummyData"));
    Deencapsulation.setField(explosion, "isSmoking", true);

    Detonate detonateEvent = new Detonate(world, explosion, null);
    eventHandler.onDetonateEvent(detonateEvent);

    Assert.assertThat("Affected block position list size should be 0.",
        explosion.affectedBlockPositions.size(), CoreMatchers.is(0));
    Assert.assertThat("isSmoking should be false",
        (Boolean) Deencapsulation.getField(explosion, "isSmoking"), CoreMatchers.is(false));

  }

  /**
   * Test that explosion does damage blocks when original rule is false and entity specific rule is
   * true
   */
  @Test
  public void testOnDetonateEvent_mobGriefingFalseBetterMobGriefingTrue_blockDamage() {
    new MockUp<BetterMobGriefingGameRule>() {
      @Mock
      String getMobGriefingRule(GameRules gameRules, EntityLivingBase entity) {
        return "betterMobGriefing";
      }
    };

    gameRules.setOrCreateGameRule(BetterMobGriefingGameRule.ORIGINAL, "false");
    gameRules.setOrCreateGameRule("betterMobGriefing", "true");

    Explosion explosion = Deencapsulation.newUninitializedInstance(Explosion.class);
    Deencapsulation.setField(explosion, "affectedBlockPositions", Lists.newArrayList("dummyData"));
    Deencapsulation.setField(explosion, "isSmoking", false);

    Detonate detonateEvent = new Detonate(world, explosion, null);
    eventHandler.onDetonateEvent(detonateEvent);

    Assert.assertThat("Affected block position list size should be 1.",
        explosion.affectedBlockPositions.size(), CoreMatchers.is(1));
    Assert.assertThat("isSmoking should be true",
        (Boolean) Deencapsulation.getField(explosion, "isSmoking"), CoreMatchers.is(true));
  }

  /**
   * Test that explosion does damage blocks when original rule is false and entity specific rule is
   * false
   */
  @Test
  public void testOnDetonateEvent_mobGriefingFalseBetterMobGriefingFalse_noBlockDamage() {
    new MockUp<BetterMobGriefingGameRule>() {
      @Mock
      String getMobGriefingRule(GameRules gameRules, EntityLivingBase entity) {
        return "betterMobGriefing";
      }
    };

    gameRules.setOrCreateGameRule(BetterMobGriefingGameRule.ORIGINAL, "false");
    gameRules.setOrCreateGameRule("betterMobGriefing", "false");

    Explosion explosion = Deencapsulation.newUninitializedInstance(Explosion.class);
    Deencapsulation.setField(explosion, "affectedBlockPositions", Lists.newArrayList("dummyData"));
    Deencapsulation.setField(explosion, "isSmoking", false);

    Detonate detonateEvent = new Detonate(world, explosion, null);
    eventHandler.onDetonateEvent(detonateEvent);

    Assert.assertThat("Affected block position list size should be 0.",
        explosion.affectedBlockPositions.size(), CoreMatchers.is(0));
    Assert.assertThat("isSmoking should be false",
        (Boolean) Deencapsulation.getField(explosion, "isSmoking"), CoreMatchers.is(false));
  }

  /**
   * Test that when an exception is thrown the mobGriefing behaviour reverts to the original
   * behaviour
   */
  @Test
  public void testOnDetonateEvent_mobGriefingTrueBetterMobGriefingFalseException_blockDamage() {
    new MockUp<BetterMobGriefingGameRule>() {
      @Mock
      String getMobGriefingRule(GameRules gameRules, EntityLivingBase entity) {
        return "betterMobGriefing";
      }
    };

    new MockUp<Field>() {
      @Mock
      void setBoolean(Object obj, boolean z) throws IllegalAccessException {
        throw new IllegalAccessException();
      }
    };

    new MockUp<PrintStream>() {
      @Mock(invocations = 1)
      PrintStream printf(String format, Object... args) {
        return null;
      }
    };

    gameRules.setOrCreateGameRule(BetterMobGriefingGameRule.ORIGINAL, "true");
    gameRules.setOrCreateGameRule("betterMobGriefing", "false");

    Explosion explosion = Deencapsulation.newUninitializedInstance(Explosion.class);
    Deencapsulation.setField(explosion, "exploder",
        Deencapsulation.newUninitializedInstance(EntityLivingBase.class));
    Deencapsulation.setField(explosion, "affectedBlockPositions", Lists.newArrayList("dummyData"));
    Deencapsulation.setField(explosion, "isSmoking", true);

    Detonate detonateEvent = new Detonate(world, explosion, null);
    eventHandler.onDetonateEvent(detonateEvent);

    Assert.assertThat("Affected block position list size should be 1.",
        explosion.affectedBlockPositions.size(), CoreMatchers.is(1));
    Assert.assertThat("isSmoking should be true",
        (Boolean) Deencapsulation.getField(explosion, "isSmoking"), CoreMatchers.is(true));
  }
}
