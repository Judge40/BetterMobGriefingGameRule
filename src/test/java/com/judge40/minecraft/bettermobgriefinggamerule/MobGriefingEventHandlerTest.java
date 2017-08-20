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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.hamcrest.CoreMatchers;
import org.junit.Assert;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import com.judge40.minecraft.bettermobgriefinggamerule.common.config.DefaultMobGriefingConfiguration;
import com.judge40.minecraft.bettermobgriefinggamerule.entity.ai.BetterBreakDoorAiTask;

import cpw.mods.fml.client.event.ConfigChangedEvent.OnConfigChangedEvent;
import mockit.Deencapsulation;
import mockit.Expectations;
import mockit.Mocked;
import mockit.Verifications;
import net.minecraft.entity.Entity;
import net.minecraft.entity.EntityLiving;
import net.minecraft.entity.ai.EntityAIBase;
import net.minecraft.entity.ai.EntityAIBreakDoor;
import net.minecraft.entity.ai.EntityAITasks.EntityAITaskEntry;
import net.minecraft.entity.monster.EntityZombie;
import net.minecraft.entity.projectile.EntityFireball;
import net.minecraft.launchwrapper.Launch;
import net.minecraft.world.Explosion;
import net.minecraft.world.GameRules;
import net.minecraft.world.World;
import net.minecraftforge.event.entity.EntityJoinWorldEvent;
import net.minecraftforge.event.world.ExplosionEvent.Detonate;

/**
 * The unit tests for {@link MobGriefingEventHandler}.
 */
public class MobGriefingEventHandlerTest {

  private MobGriefingEventHandler eventHandler;

  @Mocked
  private World world;

  @BeforeClass
  public static void setUpBeforeClass() {
    // Set the deobfuscation flag.
    Map<String, Object> blackboard = new HashMap<>();
    blackboard.put("fml.deobfuscatedEnvironment", true);
    Launch.blackboard = blackboard;
  }

  @Before
  public void setUp() {
    eventHandler = new MobGriefingEventHandler();
  }

  /**
   * Test that the configuration change event is handled when the configuration's mod ID matches
   * this mod.
   */
  @Test
  public void testOnConfigChangedEvent_defaultMobGriefingConfigurationChanged_handleChange(
      @Mocked DefaultMobGriefingConfiguration configuration) {
    // Set up test data.
    OnConfigChangedEvent event = new OnConfigChangedEvent(ModInfoConstants.ID, "", false, false);
    BetterMobGriefingGameRule entryPoint = new BetterMobGriefingGameRule();

    // Record expectations.
    new Expectations(entryPoint) {
      {
        BetterMobGriefingGameRule.getInstance();
        result = entryPoint;

        entryPoint.getDefaultMobGriefingConfiguration();
        result = configuration;
      }
    };

    // Call the method under test.
    eventHandler.onConfigChangedEvent(event);

    // Verify expectations.
    new Verifications() {
      {
        configuration.synchronize();
      }
    };
  }

  /**
   * Test that the configuration change event is not handled when the configuration's mod ID does
   * not match this mod.
   */
  @Test
  public void testOnConfigChangedEvent_nonDefaultMobGriefingConfigurationChanged_doNotHandleChange(
      @Mocked DefaultMobGriefingConfiguration configuration) {
    // Set up test data.
    OnConfigChangedEvent event = new OnConfigChangedEvent("dummyModId001", "", false, false);

    // Call the method under test.
    eventHandler.onConfigChangedEvent(event);

    // Verify expectations.
    new Verifications() {
      {
        configuration.synchronize();
        times = 0;
      }
    };
  }

  /**
   * Test that the living entity is used and the default behavior is used when the explosion is
   * created by a living entity and the global and entity mob griefing values match.
   */
  @Test
  public void testOnDetonateEvent_livingEntityGlobalAndEntityMatch_livingSourceUsedDefaultBehavior(
      @Mocked EntityLiving entity) {
    // Set up test data.
    Explosion explosion = new Explosion(null, entity, 0, 0, 0, 0);
    explosion.affectedBlockPositions = Collections.singletonList("dummyPosition");
    explosion.isSmoking = false;

    GameRules gameRules = new GameRules();
    Detonate detonateEvent = new Detonate(world, explosion, null);

    // Record expectations.
    new Expectations(gameRules, BetterMobGriefingGameRule.class) {
      {
        BetterMobGriefingGameRule.isMobGriefingEnabled(entity);
        result = true;

        world.getGameRules();
        result = gameRules;

        gameRules.getGameRuleBooleanValue(BetterMobGriefingGameRule.ORIGINAL);
        result = true;
      }
    };

    // Call the method under test.
    eventHandler.onDetonateEvent(detonateEvent);

    // Perform assertions.
    Assert.assertThat("The explosion's isSmoking field did not match the expected value.",
        explosion.isSmoking, CoreMatchers.is(false));
    Assert.assertThat("The explosion's affected blocks did not contain the expected blocks.",
        explosion.affectedBlockPositions,
        CoreMatchers.is(Collections.singletonList("dummyPosition")));
  }

  /**
   * Test that the living entity is used and the default behavior is overridden when the explosion
   * is created by a living entity and the global value is true and entity value is false.
   */
  @Test
  public void testOnDetonateEvent_livingEntityGlobalTrueEntityFalse_livingSourceUsedOverriddenBehavior(
      @Mocked EntityLiving entity) {
    // Set up test data.
    Explosion explosion = new Explosion(null, entity, 0, 0, 0, 0);
    explosion.affectedBlockPositions = new ArrayList<>(Collections.singleton("dummyData"));
    explosion.isSmoking = true;

    GameRules gameRules = new GameRules();
    Detonate detonateEvent = new Detonate(world, explosion, null);

    // Record expectations.
    new Expectations(gameRules, BetterMobGriefingGameRule.class) {
      {
        BetterMobGriefingGameRule.isMobGriefingEnabled(entity);
        result = false;

        world.getGameRules();
        result = gameRules;

        gameRules.getGameRuleBooleanValue(BetterMobGriefingGameRule.ORIGINAL);
        result = true;
      }
    };

    // Call the method under test.
    eventHandler.onDetonateEvent(detonateEvent);

    // Perform assertions.
    Assert.assertThat("The explosion's isSmoking field did not match the expected value.",
        explosion.isSmoking, CoreMatchers.is(false));
    Assert.assertThat("The explosion's affected blocks did not contain the expected blocks.",
        explosion.affectedBlockPositions, CoreMatchers.is(Collections.emptyList()));
  }

  /**
   * Test that the living entity is used and the default behavior is overridden when the explosion
   * is created by a living entity and the global value is false and entity value is true.
   */
  @Test
  public void testOnDetonateEvent_livingEntityGlobalFalseEntityTrue_livingSourceUsedOverriddenBehavior(
      @Mocked EntityLiving entity) {
    // Set up test data.
    Explosion explosion = new Explosion(null, entity, 0, 0, 0, 0);
    explosion.affectedBlockPositions = new ArrayList<>(Collections.singleton("dummyData"));
    explosion.isSmoking = false;

    GameRules gameRules = new GameRules();
    Detonate detonateEvent = new Detonate(world, explosion, null);

    // Record expectations.
    new Expectations(gameRules, BetterMobGriefingGameRule.class) {
      {
        BetterMobGriefingGameRule.isMobGriefingEnabled(entity);
        result = true;

        world.getGameRules();
        result = gameRules;

        gameRules.getGameRuleBooleanValue(BetterMobGriefingGameRule.ORIGINAL);
        result = false;
      }
    };

    // Call the method under test.
    eventHandler.onDetonateEvent(detonateEvent);

    // Perform assertions.
    Assert.assertThat("The explosion's isSmoking field did not match the expected value.",
        explosion.isSmoking, CoreMatchers.is(true));
    Assert.assertThat("The explosion's affected blocks did not contain the expected blocks.",
        explosion.affectedBlockPositions, CoreMatchers.is(Collections.singletonList("dummyData")));
  }

  /**
   * Test that the fireball's shooter is used and the default behavior is used when the explosion is
   * created by a fireball with a shooting entity and the global and entity mob griefing values
   * match.
   */
  @Test
  public void testOnDetonateEvent_fireBallWithShooterGlobalAndEntityMatch_livingSourceUsedDefaultBehavior(
      @Mocked EntityLiving entity, @Mocked EntityFireball fireball) {
    // Set up test data.
    fireball.shootingEntity = entity;

    Explosion explosion = new Explosion(null, fireball, 0, 0, 0, 0);
    explosion.affectedBlockPositions = Collections.singletonList("dummyPosition");
    explosion.isSmoking = false;

    GameRules gameRules = new GameRules();
    Detonate detonateEvent = new Detonate(world, explosion, null);

    // Record expectations.
    new Expectations(gameRules, BetterMobGriefingGameRule.class) {
      {
        BetterMobGriefingGameRule.isMobGriefingEnabled(entity);
        result = true;

        world.getGameRules();
        result = gameRules;

        gameRules.getGameRuleBooleanValue(BetterMobGriefingGameRule.ORIGINAL);
        result = true;
      }
    };

    // Call the method under test.
    eventHandler.onDetonateEvent(detonateEvent);

    // Perform assertions.
    Assert.assertThat("The explosion's isSmoking field did not match the expected value.",
        explosion.isSmoking, CoreMatchers.is(false));
    Assert.assertThat("The explosion's affected blocks did not contain the expected blocks.",
        explosion.affectedBlockPositions,
        CoreMatchers.is(Collections.singletonList("dummyPosition")));
  }

  /**
   * Test that the fireball's shooter is used and the default behavior is overridden when the
   * explosion is created by a fireball with a shooting entity and the global value is true and
   * entity value is false.
   */
  @Test
  public void testOnDetonateEvent_fireBallWithShooterGlobalTrueEntityFalse_livingSourceUsedOverriddenBehavior(
      @Mocked EntityLiving entity, @Mocked EntityFireball fireball) {
    // Set up test data.
    fireball.shootingEntity = entity;

    Explosion explosion = new Explosion(null, fireball, 0, 0, 0, 0);
    explosion.affectedBlockPositions = new ArrayList<>(Collections.singletonList("dummyData"));
    explosion.isSmoking = true;

    GameRules gameRules = new GameRules();
    Detonate detonateEvent = new Detonate(world, explosion, null);

    // Record expectations.
    new Expectations(gameRules, BetterMobGriefingGameRule.class) {
      {
        BetterMobGriefingGameRule.isMobGriefingEnabled(entity);
        result = false;

        world.getGameRules();
        result = gameRules;

        gameRules.getGameRuleBooleanValue(BetterMobGriefingGameRule.ORIGINAL);
        result = true;
      }
    };

    // Call the method under test.
    eventHandler.onDetonateEvent(detonateEvent);

    // Perform assertions.
    Assert.assertThat("The explosion's isSmoking field did not match the expected value.",
        explosion.isSmoking, CoreMatchers.is(false));
    Assert.assertThat("The explosion's affected blocks did not contain the expected blocks.",
        explosion.affectedBlockPositions, CoreMatchers.is(Collections.emptyList()));
  }

  /**
   * Test that the fireball's shooter is used and the default behavior is overridden when the
   * explosion is created by a fireball with a shooting entity and the global value is false and
   * entity value is true.
   */
  @Test
  public void testOnDetonateEvent_fireBallWithShooterGlobalFalseEntityTrue_livingSourceUsedOverriddenBehavior(
      @Mocked EntityLiving entity, @Mocked EntityFireball fireball) {
    // Set up test data.
    fireball.shootingEntity = entity;

    Explosion explosion = new Explosion(null, fireball, 0, 0, 0, 0);
    explosion.affectedBlockPositions = Collections.singletonList("dummyData");
    explosion.isSmoking = false;

    GameRules gameRules = new GameRules();
    Detonate detonateEvent = new Detonate(world, explosion, null);

    // Record expectations.
    new Expectations(gameRules, BetterMobGriefingGameRule.class) {
      {
        BetterMobGriefingGameRule.isMobGriefingEnabled(entity);
        result = true;

        world.getGameRules();
        result = gameRules;

        gameRules.getGameRuleBooleanValue(BetterMobGriefingGameRule.ORIGINAL);
        result = false;
      }
    };

    // Call the method under test.
    eventHandler.onDetonateEvent(detonateEvent);

    // Perform assertions.
    Assert.assertThat("The explosion's isSmoking field did not match the expected value.",
        explosion.isSmoking, CoreMatchers.is(true));
    Assert.assertThat("The explosion's affected blocks did not contain the expected blocks.",
        explosion.affectedBlockPositions, CoreMatchers.is(Collections.singletonList("dummyData")));
  }

  /**
   * Test that the matching affected entity is used and the default behavior is used when the
   * explosion is created by a fireball without a shooting entity, there is a matching affected
   * entity and the global and entity mob griefing values match.
   */
  @Test
  public void testOnDetonateEvent_fireBallWithoutShooterMatchingEntityGlobalAndEntityMatch_livingSourceUsedDefaultBehavior(
      @Mocked EntityLiving entity, @Mocked EntityFireball sourceFireball,
      @Mocked EntityFireball affectedFireball1, @Mocked EntityFireball affectedFireball2,
      @Mocked EntityFireball affectedFireball3, @Mocked EntityFireball affectedFireball4,
      @Mocked EntityFireball affectedFireball5) {
    // Set up test data.
    Explosion explosion = new Explosion(null, sourceFireball, 1, 1, 1, 0);
    explosion.affectedBlockPositions = Collections.singletonList("dummyPosition");
    explosion.isSmoking = false;

    GameRules gameRules = new GameRules();

    affectedFireball2.posX = 1;
    affectedFireball3.posX = 1;
    affectedFireball3.posY = 1;
    affectedFireball4.posX = 1;
    affectedFireball4.posY = 1;
    affectedFireball4.posZ = 1;
    affectedFireball5.posX = 1;
    affectedFireball5.posY = 1;
    affectedFireball5.posZ = 1;
    affectedFireball5.shootingEntity = entity;
    Detonate detonateEvent = new Detonate(world, explosion, Arrays.asList(null, affectedFireball1,
        affectedFireball2, affectedFireball3, affectedFireball4, affectedFireball5));

    // Record expectations.
    new Expectations(gameRules, BetterMobGriefingGameRule.class) {
      {
        BetterMobGriefingGameRule.isMobGriefingEnabled(entity);
        result = true;

        world.getGameRules();
        result = gameRules;

        gameRules.getGameRuleBooleanValue(BetterMobGriefingGameRule.ORIGINAL);
        result = true;
      }
    };

    // Call the method under test.
    eventHandler.onDetonateEvent(detonateEvent);

    // Perform assertions.
    Assert.assertThat("The explosion's isSmoking field did not match the expected value.",
        explosion.isSmoking, CoreMatchers.is(false));
    Assert.assertThat("The explosion's affected blocks did not contain the expected blocks.",
        explosion.affectedBlockPositions,
        CoreMatchers.is(Collections.singletonList("dummyPosition")));
  }

  /**
   * Test that the matching affected entity is used and the default behavior is overridden when the
   * explosion is created by a fireball without a shooting entity, there is a matching affected
   * entity and the global value is true and entity value is false.
   */
  @Test
  public void testOnDetonateEvent_fireBallWithoutShooterMatchingEntityGlobalTrueEntityFalse_livingSourceUsedOverriddenBehavior(
      @Mocked EntityLiving entity, @Mocked EntityFireball sourceFireball,
      @Mocked EntityFireball affectedFireball1, @Mocked EntityFireball affectedFireball2,
      @Mocked EntityFireball affectedFireball3, @Mocked EntityFireball affectedFireball4,
      @Mocked EntityFireball affectedFireball5) {
    // Set up test data.
    Explosion explosion = new Explosion(null, sourceFireball, 1, 1, 1, 0);
    explosion.affectedBlockPositions = new ArrayList<>(Collections.singletonList("dummyData"));
    explosion.isSmoking = true;

    GameRules gameRules = new GameRules();

    affectedFireball2.posX = 1;
    affectedFireball3.posX = 1;
    affectedFireball3.posY = 1;
    affectedFireball4.posX = 1;
    affectedFireball4.posY = 1;
    affectedFireball4.posZ = 1;
    affectedFireball5.posX = 1;
    affectedFireball5.posY = 1;
    affectedFireball5.posZ = 1;
    affectedFireball5.shootingEntity = entity;
    Detonate detonateEvent = new Detonate(world, explosion, Arrays.asList(null, affectedFireball1,
        affectedFireball2, affectedFireball3, affectedFireball4, affectedFireball5));

    // Record expectations.
    new Expectations(gameRules, BetterMobGriefingGameRule.class) {
      {
        BetterMobGriefingGameRule.isMobGriefingEnabled(entity);
        result = false;

        world.getGameRules();
        result = gameRules;

        gameRules.getGameRuleBooleanValue(BetterMobGriefingGameRule.ORIGINAL);
        result = true;
      }
    };

    // Call the method under test.
    eventHandler.onDetonateEvent(detonateEvent);

    // Perform assertions.
    Assert.assertThat("The explosion's isSmoking field did not match the expected value.",
        explosion.isSmoking, CoreMatchers.is(false));
    Assert.assertThat("The explosion's affected blocks did not contain the expected blocks.",
        explosion.affectedBlockPositions, CoreMatchers.is(Collections.emptyList()));
  }

  /**
   * Test that the matching affected entity is used and the default behavior is overridden when the
   * explosion is created by a fireball without a shooting entity, there is a matching affected
   * entity and the global value is false and entity value is true.
   */
  @Test
  public void testOnDetonateEvent_fireBallWithoutShooterMatchingEntityGlobalFalseEntityTrue_livingSourceUsedOverriddenBehavior(
      @Mocked EntityLiving entity, @Mocked EntityFireball sourceFireball,
      @Mocked EntityFireball affectedFireball1, @Mocked EntityFireball affectedFireball2,
      @Mocked EntityFireball affectedFireball3, @Mocked EntityFireball affectedFireball4,
      @Mocked EntityFireball affectedFireball5) {
    // Set up test data.
    Explosion explosion = new Explosion(null, sourceFireball, 1, 1, 1, 0);
    explosion.affectedBlockPositions = Collections.singletonList("dummyData");
    explosion.isSmoking = false;

    GameRules gameRules = new GameRules();

    affectedFireball2.posX = 1;
    affectedFireball3.posX = 1;
    affectedFireball3.posY = 1;
    affectedFireball4.posX = 1;
    affectedFireball4.posY = 1;
    affectedFireball4.posZ = 1;
    affectedFireball5.posX = 1;
    affectedFireball5.posY = 1;
    affectedFireball5.posZ = 1;
    affectedFireball5.shootingEntity = entity;
    Detonate detonateEvent = new Detonate(world, explosion, Arrays.asList(null, affectedFireball1,
        affectedFireball2, affectedFireball3, affectedFireball4, affectedFireball5));

    // Record expectations.
    new Expectations(gameRules, BetterMobGriefingGameRule.class) {
      {
        BetterMobGriefingGameRule.isMobGriefingEnabled(entity);
        result = true;

        world.getGameRules();
        result = gameRules;

        gameRules.getGameRuleBooleanValue(BetterMobGriefingGameRule.ORIGINAL);
        result = false;
      }
    };

    // Call the method under test.
    eventHandler.onDetonateEvent(detonateEvent);

    // Perform assertions.
    Assert.assertThat("The explosion's isSmoking field did not match the expected value.",
        explosion.isSmoking, CoreMatchers.is(true));
    Assert.assertThat("The explosion's affected blocks did not contain the expected blocks.",
        explosion.affectedBlockPositions, CoreMatchers.is(Collections.singletonList("dummyData")));
  }

  /**
   * Test that the the default behavior is used when the explosion is created by a fireball without
   * a shooting entity and there is no matching affected entity.
   */
  @Test
  public void testOnDetonateEvent_fireBallWithoutShooterNoMatchingEntity_defaultBehaviour(
      @Mocked EntityFireball sourceFireball, @Mocked EntityFireball affectedFireball1,
      @Mocked EntityFireball affectedFireball2, @Mocked EntityFireball affectedFireball3,
      @Mocked EntityFireball affectedFireball4) {
    // Set up test data.
    Explosion explosion = new Explosion(null, sourceFireball, 1, 1, 1, 0);
    explosion.affectedBlockPositions = Collections.singletonList("dummyData");
    explosion.isSmoking = false;

    affectedFireball2.posX = 1;
    affectedFireball3.posX = 1;
    affectedFireball3.posY = 1;
    affectedFireball4.posX = 1;
    affectedFireball4.posY = 1;
    affectedFireball4.posZ = 1;
    Detonate detonateEvent = new Detonate(world, explosion, Arrays.asList(null, affectedFireball1,
        affectedFireball2, affectedFireball3, affectedFireball4));

    // Call the method under test.
    eventHandler.onDetonateEvent(detonateEvent);

    // Perform assertions.
    Assert.assertThat("The explosion's isSmoking field did not match the expected value.",
        explosion.isSmoking, CoreMatchers.is(false));
    Assert.assertThat("The explosion's affected blocks did not contain the expected blocks.",
        explosion.affectedBlockPositions, CoreMatchers.is(Collections.singletonList("dummyData")));
  }

  /**
   * Test that the matching affected entity is used and the default behavior is used when the
   * explosion has no source, there is a matching affected entity and the global and entity mob
   * griefing values match.
   */
  @Test
  public void testOnDetonateEvent_noSourceWithoutShooterMatchingEntityGlobalAndEntityMatch_livingSourceUsedDefaultBehavior(
      @Mocked EntityLiving entity, @Mocked EntityFireball affectedFireball1,
      @Mocked EntityFireball affectedFireball2, @Mocked EntityFireball affectedFireball3,
      @Mocked EntityFireball affectedFireball4, @Mocked EntityFireball affectedFireball5) {
    // Set up test data.
    Explosion explosion = new Explosion(null, null, 1, 1, 1, 0);
    explosion.affectedBlockPositions = Collections.singletonList("dummyPosition");
    explosion.isSmoking = false;

    GameRules gameRules = new GameRules();

    affectedFireball2.posX = 1;
    affectedFireball3.posX = 1;
    affectedFireball3.posY = 1;
    affectedFireball4.posX = 1;
    affectedFireball4.posY = 1;
    affectedFireball4.posZ = 1;
    affectedFireball5.posX = 1;
    affectedFireball5.posY = 1;
    affectedFireball5.posZ = 1;
    affectedFireball5.shootingEntity = entity;
    Detonate detonateEvent = new Detonate(world, explosion, Arrays.asList(null, affectedFireball1,
        affectedFireball2, affectedFireball3, affectedFireball4, affectedFireball5));

    // Record expectations.
    new Expectations(gameRules, BetterMobGriefingGameRule.class) {
      {
        BetterMobGriefingGameRule.isMobGriefingEnabled(entity);
        result = true;

        world.getGameRules();
        result = gameRules;

        gameRules.getGameRuleBooleanValue(BetterMobGriefingGameRule.ORIGINAL);
        result = true;
      }
    };

    // Call the method under test.
    eventHandler.onDetonateEvent(detonateEvent);

    // Perform assertions.
    Assert.assertThat("The explosion's isSmoking field did not match the expected value.",
        explosion.isSmoking, CoreMatchers.is(false));
    Assert.assertThat("The explosion's affected blocks did not contain the expected blocks.",
        explosion.affectedBlockPositions,
        CoreMatchers.is(Collections.singletonList("dummyPosition")));
  }

  /**
   * Test that the matching affected entity is used and the default behavior is overridden when the
   * explosion has no source, there is a matching affected entity and the global value is true and
   * entity value is false.
   */
  @Test
  public void testOnDetonateEvent_noSourceMatchingEntityGlobalTrueEntityFalse_livingSourceUsedOverriddenBehavior(
      @Mocked EntityLiving entity, @Mocked EntityFireball affectedFireball1,
      @Mocked EntityFireball affectedFireball2, @Mocked EntityFireball affectedFireball3,
      @Mocked EntityFireball affectedFireball4, @Mocked EntityFireball affectedFireball5) {
    // Set up test data.
    Explosion explosion = new Explosion(null, null, 1, 1, 1, 0);
    explosion.affectedBlockPositions = new ArrayList<>(Collections.singletonList("dummyData"));
    explosion.isSmoking = true;

    GameRules gameRules = new GameRules();

    affectedFireball2.posX = 1;
    affectedFireball3.posX = 1;
    affectedFireball3.posY = 1;
    affectedFireball4.posX = 1;
    affectedFireball4.posY = 1;
    affectedFireball4.posZ = 1;
    affectedFireball5.posX = 1;
    affectedFireball5.posY = 1;
    affectedFireball5.posZ = 1;
    affectedFireball5.shootingEntity = entity;
    Detonate detonateEvent = new Detonate(world, explosion, Arrays.asList(null, affectedFireball1,
        affectedFireball2, affectedFireball3, affectedFireball4, affectedFireball5));

    // Record expectations.
    new Expectations(gameRules, BetterMobGriefingGameRule.class) {
      {
        BetterMobGriefingGameRule.isMobGriefingEnabled(entity);
        result = false;

        world.getGameRules();
        result = gameRules;

        gameRules.getGameRuleBooleanValue(BetterMobGriefingGameRule.ORIGINAL);
        result = true;
      }
    };

    // Call the method under test.
    eventHandler.onDetonateEvent(detonateEvent);

    // Perform assertions.
    Assert.assertThat("The explosion's isSmoking field did not match the expected value.",
        explosion.isSmoking, CoreMatchers.is(false));
    Assert.assertThat("The explosion's affected blocks did not contain the expected blocks.",
        explosion.affectedBlockPositions, CoreMatchers.is(Collections.emptyList()));
  }

  /**
   * Test that the matching affected entity is used and the default behavior is overridden when the
   * explosion has no source, there is a matching affected entity and the global value is false and
   * entity value is true.
   */
  @Test
  public void testOnDetonateEvent_noSourceMatchingEntityGlobalFalseEntityTrue_livingSourceUsedOverriddenBehavior(
      @Mocked EntityLiving entity, @Mocked EntityFireball affectedFireball1,
      @Mocked EntityFireball affectedFireball2, @Mocked EntityFireball affectedFireball3,
      @Mocked EntityFireball affectedFireball4, @Mocked EntityFireball affectedFireball5) {
    // Set up test data.
    Explosion explosion = new Explosion(null, null, 1, 1, 1, 0);
    explosion.affectedBlockPositions = Collections.singletonList("dummyData");
    explosion.isSmoking = false;

    GameRules gameRules = new GameRules();

    affectedFireball2.posX = 1;
    affectedFireball3.posX = 1;
    affectedFireball3.posY = 1;
    affectedFireball4.posX = 1;
    affectedFireball4.posY = 1;
    affectedFireball4.posZ = 1;
    affectedFireball5.posX = 1;
    affectedFireball5.posY = 1;
    affectedFireball5.posZ = 1;
    affectedFireball5.shootingEntity = entity;
    Detonate detonateEvent = new Detonate(world, explosion, Arrays.asList(null, affectedFireball1,
        affectedFireball2, affectedFireball3, affectedFireball4, affectedFireball5));

    // Record expectations.
    new Expectations(gameRules, BetterMobGriefingGameRule.class) {
      {
        BetterMobGriefingGameRule.isMobGriefingEnabled(entity);
        result = true;

        world.getGameRules();
        result = gameRules;

        gameRules.getGameRuleBooleanValue(BetterMobGriefingGameRule.ORIGINAL);
        result = false;
      }
    };

    // Call the method under test.
    eventHandler.onDetonateEvent(detonateEvent);

    // Perform assertions.
    Assert.assertThat("The explosion's isSmoking field did not match the expected value.",
        explosion.isSmoking, CoreMatchers.is(true));
    Assert.assertThat("The explosion's affected blocks did not contain the expected blocks.",
        explosion.affectedBlockPositions, CoreMatchers.is(Collections.singletonList("dummyData")));
  }

  /**
   * Test that the the default behavior is used when the explosion has no source and there is no
   * matching affected entity.
   */
  @Test
  public void testOnDetonateEvent_noSourceNoMatchingEntity_defaultBehaviour(
      @Mocked EntityFireball affectedFireball1, @Mocked EntityFireball affectedFireball2,
      @Mocked EntityFireball affectedFireball3, @Mocked EntityFireball affectedFireball4) {
    // Set up test data.
    Explosion explosion = new Explosion(null, null, 1, 1, 1, 0);
    explosion.affectedBlockPositions = Collections.singletonList("dummyData");
    explosion.isSmoking = false;

    affectedFireball2.posX = 1;
    affectedFireball3.posX = 1;
    affectedFireball3.posY = 1;
    affectedFireball4.posX = 1;
    affectedFireball4.posY = 1;
    affectedFireball4.posZ = 1;
    Detonate detonateEvent = new Detonate(world, explosion, Arrays.asList(null, affectedFireball1,
        affectedFireball2, affectedFireball3, affectedFireball4));

    // Call the method under test.
    eventHandler.onDetonateEvent(detonateEvent);

    // Perform assertions.
    Assert.assertThat("The explosion's isSmoking field did not match the expected value.",
        explosion.isSmoking, CoreMatchers.is(false));
    Assert.assertThat("The explosion's affected blocks did not contain the expected blocks.",
        explosion.affectedBlockPositions, CoreMatchers.is(Collections.singletonList("dummyData")));
  }

  /**
   * Test that the AI task field is updated and the task is not populated in the task list when the
   * AI task list is not already populated.
   */
  @Test
  public void testOnEntityJoinWorldEvent_zombieTaskNotPopulated_fieldUpdatedTaskNotPopulated() {
    // Set up test data.
    EntityZombie zombie = new EntityZombie(null);
    EntityJoinWorldEvent event = new EntityJoinWorldEvent(zombie, null);

    // Call the method under test.
    eventHandler.onEntityJoinWorldEvent(event);

    // Perform assertions.
    EntityAIBreakDoor breakDoorAiTask =
        Deencapsulation.getField(zombie, ObfuscationHelper.convertName("field_146075_bs"));
    Assert.assertThat("The Zombie's break door task was not replaced as expected.", breakDoorAiTask,
        CoreMatchers.instanceOf(BetterBreakDoorAiTask.class));

    List<EntityAIBase> taskActions = new ArrayList<>();

    for (Iterator<?> iterator = zombie.tasks.taskEntries.iterator(); iterator.hasNext();) {
      EntityAITaskEntry aiTaskEntry = (EntityAITaskEntry) iterator.next();
      taskActions.add(aiTaskEntry.action);
    }

    Assert.assertThat(
        "A break door AI task was found in the Zombie's task list, but should not have existed.",
        taskActions,
        CoreMatchers.not(CoreMatchers.hasItem(CoreMatchers.instanceOf(EntityAIBreakDoor.class))));
  }

  /**
   * Test that the AI task field is updated and the task is replaced in the task list when the AI
   * task list is already populated.
   */
  @Test
  public void testOnEntityJoinWorldEvent_zombieTaskPopulated_fieldUpdatedTaskReplaced() {
    // Set up test data.
    EntityZombie zombie = new EntityZombie(null);

    String breakDoorAiField = ObfuscationHelper.convertName("field_146075_bs");
    EntityAIBreakDoor originalBreakDoorAiTask = Deencapsulation.getField(zombie, breakDoorAiField);
    zombie.tasks.addTask(0, originalBreakDoorAiTask);

    EntityJoinWorldEvent event = new EntityJoinWorldEvent(zombie, null);

    // Call the method under test.
    eventHandler.onEntityJoinWorldEvent(event);

    // Perform assertions.
    EntityAIBreakDoor breakDoorAiTask = Deencapsulation.getField(zombie, breakDoorAiField);
    Assert.assertThat("The Zombie's break door task was not replaced as expected.", breakDoorAiTask,
        CoreMatchers.instanceOf(BetterBreakDoorAiTask.class));

    List<EntityAIBase> taskActions = new ArrayList<>();

    for (Iterator<?> iterator = zombie.tasks.taskEntries.iterator(); iterator.hasNext();) {
      EntityAITaskEntry aiTaskEntry = (EntityAITaskEntry) iterator.next();
      taskActions.add(aiTaskEntry.action);
    }

    Assert.assertThat(
        "An original break door AI task was found in the Zombie's task list, but should not have existed.",
        taskActions, CoreMatchers.not(CoreMatchers.hasItem(originalBreakDoorAiTask)));
    Assert.assertThat(
        "A better break door AI task was not found in the Zombie's task list, but should have existed.",
        taskActions, CoreMatchers.hasItem(CoreMatchers.instanceOf(BetterBreakDoorAiTask.class)));
  }

  /**
   * Test that no exception is thrown when an unhandled Entity is passed to the event handler.
   */
  @Test
  public void testOnEntityJoinWorldEvent_notZombie_noException(@Mocked Entity entity) {
    EntityJoinWorldEvent entityJoinWorldEvent = new EntityJoinWorldEvent(entity, null);
    eventHandler.onEntityJoinWorldEvent(entityJoinWorldEvent);
  }
}
