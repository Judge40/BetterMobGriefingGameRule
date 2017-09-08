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

package com.judge40.minecraft.bettermobgriefinggamerule.common;

import com.judge40.minecraft.bettermobgriefinggamerule.BetterMobGriefingGameRule;
import com.judge40.minecraft.bettermobgriefinggamerule.common.configuration.DefaultMobGriefingConfiguration;

import mockit.Expectations;
import mockit.Mocked;
import mockit.Verifications;
import net.minecraft.entity.EntityLiving;
import net.minecraft.entity.projectile.EntityFireball;
import net.minecraft.entity.projectile.EntityLargeFireball;
import net.minecraft.launchwrapper.Launch;
import net.minecraft.util.BlockPos;
import net.minecraft.world.Explosion;
import net.minecraft.world.GameRules;
import net.minecraft.world.World;
import net.minecraftforge.event.world.ExplosionEvent.Detonate;
import net.minecraftforge.fml.client.event.ConfigChangedEvent.OnConfigChangedEvent;
import net.minecraftforge.fml.relauncher.ReflectionHelper;
import org.hamcrest.CoreMatchers;
import org.junit.Assert;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * The unit tests for {@link MobGriefingEventHandler}.
 */
public class MobGriefingEventHandlerTest {

  private MobGriefingEventHandler eventHandler;

  @Mocked
  private World world;

  /**
   * Populate the {@code fml.deobfuscatedEnvironment} flag.
   */
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
  public void testOnConfigChanged_defaultMobGriefingConfigurationChanged_handleChange(
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
    eventHandler.onConfigChanged(event);

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
  public void testOnConfigChanged_nonDefaultMobGriefingConfigurationChanged_doNotHandleChange(
      @Mocked DefaultMobGriefingConfiguration configuration) {
    // Set up test data.
    OnConfigChangedEvent event = new OnConfigChangedEvent("dummyModId001", "", false, false);

    // Call the method under test.
    eventHandler.onConfigChanged(event);

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
  public void testOnDetonate_livingEntityGlobalAndEntityMatch_useEntityDefault(
      @Mocked EntityLiving entity) {
    // Set up test data.
    List<BlockPos> affectedBlockPositions = Collections.singletonList(new BlockPos(0, 0, 0));
    Explosion explosion =
        new Explosion(null, entity, 0, 0, 0, 0, true, false, affectedBlockPositions);

    GameRules gameRules = new GameRules();
    Detonate detonateEvent = new Detonate(world, explosion, null);

    // Record expectations.
    new Expectations(gameRules, BetterMobGriefingGameRule.class) {
      {
        BetterMobGriefingGameRule.isMobGriefingEnabled(entity);
        result = true;

        world.getGameRules();
        result = gameRules;

        gameRules.getBoolean(BetterMobGriefingGameRule.GLOBAL_RULE);
        result = true;
      }
    };

    // Call the method under test.
    eventHandler.onDetonate(detonateEvent);

    // Perform assertions.
    boolean isFlaming = ReflectionHelper.getPrivateValue(Explosion.class, explosion,
        ObfuscationHelper.convertName("field_77286_a"));
    Assert.assertThat("The explosion's isFlaming field did not match the expected value.",
        isFlaming, CoreMatchers.is(true));
    boolean isSmoking = ReflectionHelper.getPrivateValue(Explosion.class, explosion,
        ObfuscationHelper.convertName("field_82755_b"));
    Assert.assertThat("The explosion's isSmoking field did not match the expected value.",
        isSmoking, CoreMatchers.is(false));
    Assert.assertThat("The explosion's affected blocks did not contain the expected blocks.",
        explosion.getAffectedBlockPositions(), CoreMatchers.is(affectedBlockPositions));
  }

  /**
   * Test that the living entity is used and the default behavior is overridden when the explosion
   * is created by a living entity and the global value is true and entity value is false.
   */
  @Test
  public void testOnDetonate_livingEntityGlobalTrueEntityFalse_useEntityOverride(
      @Mocked EntityLiving entity) {
    // Set up test data.
    List<BlockPos> affectedBlockPositions = Collections.singletonList(new BlockPos(0, 0, 0));
    Explosion explosion =
        new Explosion(null, entity, 0, 0, 0, 0, false, true, affectedBlockPositions);

    GameRules gameRules = new GameRules();
    Detonate detonateEvent = new Detonate(world, explosion, null);

    // Record expectations.
    new Expectations(gameRules, BetterMobGriefingGameRule.class) {
      {
        BetterMobGriefingGameRule.isMobGriefingEnabled(entity);
        result = false;

        world.getGameRules();
        result = gameRules;

        gameRules.getBoolean(BetterMobGriefingGameRule.GLOBAL_RULE);
        result = true;
      }
    };

    // Call the method under test.
    eventHandler.onDetonate(detonateEvent);

    // Perform assertions.
    boolean isFlaming = ReflectionHelper.getPrivateValue(Explosion.class, explosion,
        ObfuscationHelper.convertName("field_77286_a"));
    Assert.assertThat("The explosion's isFlaming field did not match the expected value.",
        isFlaming, CoreMatchers.is(false));
    boolean isSmoking = ReflectionHelper.getPrivateValue(Explosion.class, explosion,
        ObfuscationHelper.convertName("field_82755_b"));
    Assert.assertThat("The explosion's isSmoking field did not match the expected value.",
        isSmoking, CoreMatchers.is(false));
    Assert.assertThat("The explosion's affected blocks did not contain the expected blocks.",
        explosion.getAffectedBlockPositions(), CoreMatchers.is(Collections.emptyList()));
  }

  /**
   * Test that the living entity is used and the default behavior is overridden when the explosion
   * is created by a living entity and the global value is false and entity value is true.
   */
  @Test
  public void testOnDetonate_livingEntityGlobalFalseEntityTrue_useEntityOverride(
      @Mocked EntityLiving entity) {
    // Set up test data.
    List<BlockPos> affectedBlockPositions = Collections.singletonList(new BlockPos(0, 0, 0));
    Explosion explosion =
        new Explosion(null, entity, 0, 0, 0, 0, false, false, affectedBlockPositions);

    GameRules gameRules = new GameRules();
    Detonate detonateEvent = new Detonate(world, explosion, null);

    // Record expectations.
    new Expectations(gameRules, BetterMobGriefingGameRule.class) {
      {
        BetterMobGriefingGameRule.isMobGriefingEnabled(entity);
        result = true;

        world.getGameRules();
        result = gameRules;

        gameRules.getBoolean(BetterMobGriefingGameRule.GLOBAL_RULE);
        result = false;
      }
    };

    // Call the method under test.
    eventHandler.onDetonate(detonateEvent);

    // Perform assertions.
    boolean isFlaming = ReflectionHelper.getPrivateValue(Explosion.class, explosion,
        ObfuscationHelper.convertName("field_77286_a"));
    Assert.assertThat("The explosion's isFlaming field did not match the expected value.",
        isFlaming, CoreMatchers.is(false));
    boolean isSmoking = ReflectionHelper.getPrivateValue(Explosion.class, explosion,
        ObfuscationHelper.convertName("field_82755_b"));
    Assert.assertThat("The explosion's isSmoking field did not match the expected value.",
        isSmoking, CoreMatchers.is(true));
    Assert.assertThat("The explosion's affected blocks did not contain the expected blocks.",
        explosion.getAffectedBlockPositions(), CoreMatchers.is(affectedBlockPositions));
  }

  /**
   * Test that the fireball's shooter is used and the default behavior is used when the explosion is
   * created by a fireball with a shooting entity and the global and entity mob griefing values
   * match.
   */
  @Test
  public void testOnDetonate_fireballHasShooterGlobalAndEntityMatch_useShooterDefault(
      @Mocked EntityLiving entity, @Mocked EntityFireball fireball) {
    // Set up test data.
    fireball.shootingEntity = entity;

    List<BlockPos> affectedBlockPositions = Collections.singletonList(new BlockPos(0, 0, 0));
    Explosion explosion =
        new Explosion(null, fireball, 0, 0, 0, 0, false, false, affectedBlockPositions);

    GameRules gameRules = new GameRules();
    Detonate detonateEvent = new Detonate(world, explosion, null);

    // Record expectations.
    new Expectations(gameRules, BetterMobGriefingGameRule.class) {
      {
        BetterMobGriefingGameRule.isMobGriefingEnabled(entity);
        result = true;

        world.getGameRules();
        result = gameRules;

        gameRules.getBoolean(BetterMobGriefingGameRule.GLOBAL_RULE);
        result = true;
      }
    };

    // Call the method under test.
    eventHandler.onDetonate(detonateEvent);

    // Perform assertions.
    boolean isFlaming = ReflectionHelper.getPrivateValue(Explosion.class, explosion,
        ObfuscationHelper.convertName("field_77286_a"));
    Assert.assertThat("The explosion's isFlaming field did not match the expected value.",
        isFlaming, CoreMatchers.is(false));
    boolean isSmoking = ReflectionHelper.getPrivateValue(Explosion.class, explosion,
        ObfuscationHelper.convertName("field_82755_b"));
    Assert.assertThat("The explosion's isSmoking field did not match the expected value.",
        isSmoking, CoreMatchers.is(false));
    Assert.assertThat("The explosion's affected blocks did not contain the expected blocks.",
        explosion.getAffectedBlockPositions(), CoreMatchers.is(affectedBlockPositions));
  }

  /**
   * Test that the fireball's shooter is used and the default behavior is overridden when the
   * explosion is created by a fireball with a shooting entity and the global value is true and
   * entity value is false.
   */
  @Test
  public void testOnDetonate_fireballHasShooterGlobalTrueEntityFalse_useShooterOverride(
      @Mocked EntityLiving entity, @Mocked EntityFireball fireball) {
    // Set up test data.
    fireball.shootingEntity = entity;

    List<BlockPos> affectedBlockPositions = Collections.singletonList(new BlockPos(0, 0, 0));
    Explosion explosion =
        new Explosion(null, fireball, 0, 0, 0, 0, false, true, affectedBlockPositions);

    GameRules gameRules = new GameRules();
    Detonate detonateEvent = new Detonate(world, explosion, null);

    // Record expectations.
    new Expectations(gameRules, BetterMobGriefingGameRule.class) {
      {
        BetterMobGriefingGameRule.isMobGriefingEnabled(entity);
        result = false;

        world.getGameRules();
        result = gameRules;

        gameRules.getBoolean(BetterMobGriefingGameRule.GLOBAL_RULE);
        result = true;
      }
    };

    // Call the method under test.
    eventHandler.onDetonate(detonateEvent);

    // Perform assertions.
    boolean isFlaming = ReflectionHelper.getPrivateValue(Explosion.class, explosion,
        ObfuscationHelper.convertName("field_77286_a"));
    Assert.assertThat("The explosion's isFlaming field did not match the expected value.",
        isFlaming, CoreMatchers.is(false));
    boolean isSmoking = ReflectionHelper.getPrivateValue(Explosion.class, explosion,
        ObfuscationHelper.convertName("field_82755_b"));
    Assert.assertThat("The explosion's isSmoking field did not match the expected value.",
        isSmoking, CoreMatchers.is(false));
    Assert.assertThat("The explosion's affected blocks did not contain the expected blocks.",
        explosion.getAffectedBlockPositions(), CoreMatchers.is(Collections.emptyList()));
  }

  /**
   * Test that the fireball's shooter is used and the default behavior is overridden when the
   * explosion is created by a fireball with a shooting entity and the global value is false and
   * entity value is true.
   */
  @Test
  public void testOnDetonate_fireballHasShooterGlobalFalseEntityTrue_useShooterOverride(
      @Mocked EntityLiving entity, @Mocked EntityFireball fireball) {
    // Set up test data.
    fireball.shootingEntity = entity;

    List<BlockPos> affectedBlockPositions = Collections.singletonList(new BlockPos(0, 0, 0));
    Explosion explosion =
        new Explosion(null, fireball, 0, 0, 0, 0, false, false, affectedBlockPositions);

    GameRules gameRules = new GameRules();
    Detonate detonateEvent = new Detonate(world, explosion, null);

    // Record expectations.
    new Expectations(gameRules, BetterMobGriefingGameRule.class) {
      {
        BetterMobGriefingGameRule.isMobGriefingEnabled(entity);
        result = true;

        world.getGameRules();
        result = gameRules;

        gameRules.getBoolean(BetterMobGriefingGameRule.GLOBAL_RULE);
        result = false;
      }
    };

    // Call the method under test.
    eventHandler.onDetonate(detonateEvent);

    // Perform assertions.
    boolean isFlaming = ReflectionHelper.getPrivateValue(Explosion.class, explosion,
        ObfuscationHelper.convertName("field_77286_a"));
    Assert.assertThat("The explosion's isFlaming field did not match the expected value.",
        isFlaming, CoreMatchers.is(false));
    boolean isSmoking = ReflectionHelper.getPrivateValue(Explosion.class, explosion,
        ObfuscationHelper.convertName("field_82755_b"));
    Assert.assertThat("The explosion's isSmoking field did not match the expected value.",
        isSmoking, CoreMatchers.is(true));
    Assert.assertThat("The explosion's affected blocks did not contain the expected blocks.",
        explosion.getAffectedBlockPositions(), CoreMatchers.is(affectedBlockPositions));
  }

  /**
   * Test that the matching affected entity is used and the default behavior is used when the
   * explosion is created by a fireball without a shooting entity, there is a matching affected
   * entity and the global and entity mob griefing values match.
   */
  @Test
  public void testOnDetonate_fireballNoShooterEntityMatchGlobalAndEntityMatch_useEntityDefault(
      @Mocked EntityLiving entity, @Mocked EntityFireball sourceFireball,
      @Mocked EntityFireball affectedFireball1, @Mocked EntityFireball affectedFireball2,
      @Mocked EntityFireball affectedFireball3, @Mocked EntityFireball affectedFireball4,
      @Mocked EntityFireball affectedFireball5) {
    // Set up test data.
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

    List<BlockPos> affectedBlockPositions = Collections.singletonList(new BlockPos(0, 0, 0));
    Explosion explosion =
        new Explosion(null, sourceFireball, 1, 1, 1, 0, false, false, affectedBlockPositions);

    Detonate detonateEvent = new Detonate(world, explosion, Arrays.asList(null, affectedFireball1,
        affectedFireball2, affectedFireball3, affectedFireball4, affectedFireball5));

    GameRules gameRules = new GameRules();

    // Record expectations.
    new Expectations(gameRules, BetterMobGriefingGameRule.class) {
      {
        BetterMobGriefingGameRule.isMobGriefingEnabled(entity);
        result = true;

        world.getGameRules();
        result = gameRules;

        gameRules.getBoolean(BetterMobGriefingGameRule.GLOBAL_RULE);
        result = true;
      }
    };

    // Call the method under test.
    eventHandler.onDetonate(detonateEvent);

    // Perform assertions.
    boolean isFlaming = ReflectionHelper.getPrivateValue(Explosion.class, explosion,
        ObfuscationHelper.convertName("field_77286_a"));
    Assert.assertThat("The explosion's isFlaming field did not match the expected value.",
        isFlaming, CoreMatchers.is(false));
    boolean isSmoking = ReflectionHelper.getPrivateValue(Explosion.class, explosion,
        ObfuscationHelper.convertName("field_82755_b"));
    Assert.assertThat("The explosion's isSmoking field did not match the expected value.",
        isSmoking, CoreMatchers.is(false));
    Assert.assertThat("The explosion's affected blocks did not contain the expected blocks.",
        explosion.getAffectedBlockPositions(), CoreMatchers.is(affectedBlockPositions));
  }

  /**
   * Test that the matching affected entity is used and the default behavior is overridden when the
   * explosion is created by a fireball without a shooting entity, there is a matching affected
   * entity and the global value is true and entity value is false.
   */
  @Test
  public void testOnDetonate_fireballNoShooterEntityMatchGlobalTrueEntityFalse_useEntityOverride(
      @Mocked EntityLiving entity, @Mocked EntityFireball sourceFireball,
      @Mocked EntityFireball affectedFireball1, @Mocked EntityFireball affectedFireball2,
      @Mocked EntityFireball affectedFireball3, @Mocked EntityFireball affectedFireball4,
      @Mocked EntityFireball affectedFireball5) {
    // Set up test data.
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

    List<BlockPos> affectedBlockPositions = Collections.singletonList(new BlockPos(0, 0, 0));
    Explosion explosion =
        new Explosion(null, sourceFireball, 1, 1, 1, 0, false, true, affectedBlockPositions);

    Detonate detonateEvent = new Detonate(world, explosion, Arrays.asList(null, affectedFireball1,
        affectedFireball2, affectedFireball3, affectedFireball4, affectedFireball5));

    GameRules gameRules = new GameRules();

    // Record expectations.
    new Expectations(gameRules, BetterMobGriefingGameRule.class) {
      {
        BetterMobGriefingGameRule.isMobGriefingEnabled(entity);
        result = false;

        world.getGameRules();
        result = gameRules;

        gameRules.getBoolean(BetterMobGriefingGameRule.GLOBAL_RULE);
        result = true;
      }
    };

    // Call the method under test.
    eventHandler.onDetonate(detonateEvent);

    // Perform assertions.
    boolean isFlaming = ReflectionHelper.getPrivateValue(Explosion.class, explosion,
        ObfuscationHelper.convertName("field_77286_a"));
    Assert.assertThat("The explosion's isFlaming field did not match the expected value.",
        isFlaming, CoreMatchers.is(false));
    boolean isSmoking = ReflectionHelper.getPrivateValue(Explosion.class, explosion,
        ObfuscationHelper.convertName("field_82755_b"));
    Assert.assertThat("The explosion's isSmoking field did not match the expected value.",
        isSmoking, CoreMatchers.is(false));
    Assert.assertThat("The explosion's affected blocks did not contain the expected blocks.",
        explosion.getAffectedBlockPositions(), CoreMatchers.is(Collections.emptyList()));
  }

  /**
   * Test that the matching affected entity is used and the default behavior is overridden when the
   * explosion is created by a fireball without a shooting entity, there is a matching affected
   * entity and the global value is false and entity value is true.
   */
  @Test
  public void testOnDetonate_fireballNoShooterEntityMatchGlobalFalseEntityTrue_useEntityOverride(
      @Mocked EntityLiving entity, @Mocked EntityFireball sourceFireball,
      @Mocked EntityFireball affectedFireball1, @Mocked EntityFireball affectedFireball2,
      @Mocked EntityFireball affectedFireball3, @Mocked EntityFireball affectedFireball4,
      @Mocked EntityFireball affectedFireball5) {
    // Set up test data.
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

    List<BlockPos> affectedBlockPositions = Collections.singletonList(new BlockPos(0, 0, 0));
    Explosion explosion =
        new Explosion(null, sourceFireball, 1, 1, 1, 0, false, false, affectedBlockPositions);

    Detonate detonateEvent = new Detonate(world, explosion, Arrays.asList(null, affectedFireball1,
        affectedFireball2, affectedFireball3, affectedFireball4, affectedFireball5));

    GameRules gameRules = new GameRules();

    // Record expectations.
    new Expectations(gameRules, BetterMobGriefingGameRule.class) {
      {
        BetterMobGriefingGameRule.isMobGriefingEnabled(entity);
        result = true;

        world.getGameRules();
        result = gameRules;

        gameRules.getBoolean(BetterMobGriefingGameRule.GLOBAL_RULE);
        result = false;
      }
    };

    // Call the method under test.
    eventHandler.onDetonate(detonateEvent);

    // Perform assertions.
    boolean isFlaming = ReflectionHelper.getPrivateValue(Explosion.class, explosion,
        ObfuscationHelper.convertName("field_77286_a"));
    Assert.assertThat("The explosion's isFlaming field did not match the expected value.",
        isFlaming, CoreMatchers.is(false));
    boolean isSmoking = ReflectionHelper.getPrivateValue(Explosion.class, explosion,
        ObfuscationHelper.convertName("field_82755_b"));
    Assert.assertThat("The explosion's isSmoking field did not match the expected value.",
        isSmoking, CoreMatchers.is(true));
    Assert.assertThat("The explosion's affected blocks did not contain the expected blocks.",
        explosion.getAffectedBlockPositions(), CoreMatchers.is(affectedBlockPositions));
  }

  /**
   * Test that the the default behavior is used when the explosion is created by a fireball without
   * a shooting entity and there is no matching affected entity.
   */
  @Test
  public void testOnDetonate_fireballNoShooterNoEntityMatch_defaultBehaviour(
      @Mocked EntityFireball sourceFireball, @Mocked EntityFireball affectedFireball1,
      @Mocked EntityFireball affectedFireball2, @Mocked EntityFireball affectedFireball3,
      @Mocked EntityFireball affectedFireball4) {
    // Set up test data.
    affectedFireball2.posX = 1;
    affectedFireball3.posX = 1;
    affectedFireball3.posY = 1;
    affectedFireball4.posX = 1;
    affectedFireball4.posY = 1;
    affectedFireball4.posZ = 1;

    List<BlockPos> affectedBlockPositions = Collections.singletonList(new BlockPos(0, 0, 0));
    Explosion explosion =
        new Explosion(null, sourceFireball, 1, 1, 1, 0, false, false, affectedBlockPositions);

    Detonate detonateEvent = new Detonate(world, explosion, Arrays.asList(null, affectedFireball1,
        affectedFireball2, affectedFireball3, affectedFireball4));

    // Call the method under test.
    eventHandler.onDetonate(detonateEvent);

    // Perform assertions.
    boolean isFlaming = ReflectionHelper.getPrivateValue(Explosion.class, explosion,
        ObfuscationHelper.convertName("field_77286_a"));
    Assert.assertThat("The explosion's isFlaming field did not match the expected value.",
        isFlaming, CoreMatchers.is(false));
    boolean isSmoking = ReflectionHelper.getPrivateValue(Explosion.class, explosion,
        ObfuscationHelper.convertName("field_82755_b"));
    Assert.assertThat("The explosion's isSmoking field did not match the expected value.",
        isSmoking, CoreMatchers.is(false));
    Assert.assertThat("The explosion's affected blocks did not contain the expected blocks.",
        explosion.getAffectedBlockPositions(), CoreMatchers.is(affectedBlockPositions));
  }

  /**
   * Test that the isFlaming flag is set when the explosion is created by a large fireball with a
   * shooter and the global and entity rules do not match.
   */
  @Test
  public void testOnDetonate_largeFireballHasShooterGlobalAndEntityNoMatch_isFlamingSet(
      @Mocked EntityLiving entity, @Mocked EntityLargeFireball largeFireball) {
    // Set up test data.
    largeFireball.shootingEntity = entity;

    List<BlockPos> affectedBlockPositions = Collections.singletonList(new BlockPos(0, 0, 0));
    Explosion explosion =
        new Explosion(null, largeFireball, 0, 0, 0, 0, true, true, affectedBlockPositions);

    GameRules gameRules = new GameRules();
    Detonate detonateEvent = new Detonate(world, explosion, null);

    // Record expectations.
    new Expectations(gameRules, BetterMobGriefingGameRule.class) {
      {
        BetterMobGriefingGameRule.isMobGriefingEnabled(entity);
        result = false;

        world.getGameRules();
        result = gameRules;

        gameRules.getBoolean(BetterMobGriefingGameRule.GLOBAL_RULE);
        result = true;
      }
    };

    // Call the method under test.
    eventHandler.onDetonate(detonateEvent);

    // Perform assertions.
    boolean isFlaming = ReflectionHelper.getPrivateValue(Explosion.class, explosion,
        ObfuscationHelper.convertName("field_77286_a"));
    Assert.assertThat("The explosion's isFlaming field did not match the expected value.",
        isFlaming, CoreMatchers.is(false));
  }

  /**
   * Test that the matching affected entity is used and the default behavior is used when the
   * explosion has no source, there is a matching affected entity and the global and entity mob
   * griefing values match.
   */
  @Test
  public void testOnDetonate_noSourceEntityMatchGlobalAndEntityMatch_useEntityDefault(
      @Mocked EntityLiving entity, @Mocked EntityFireball affectedFireball1,
      @Mocked EntityFireball affectedFireball2, @Mocked EntityFireball affectedFireball3,
      @Mocked EntityFireball affectedFireball4, @Mocked EntityFireball affectedFireball5) {
    // Set up test data.
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

    List<BlockPos> affectedBlockPositions = Collections.singletonList(new BlockPos(0, 0, 0));
    Explosion explosion =
        new Explosion(null, null, 1, 1, 1, 0, false, false, affectedBlockPositions);
    Detonate detonateEvent = new Detonate(world, explosion, Arrays.asList(null, affectedFireball1,
        affectedFireball2, affectedFireball3, affectedFireball4, affectedFireball5));

    GameRules gameRules = new GameRules();


    // Record expectations.
    new Expectations(gameRules, BetterMobGriefingGameRule.class) {
      {
        BetterMobGriefingGameRule.isMobGriefingEnabled(entity);
        result = true;

        world.getGameRules();
        result = gameRules;

        gameRules.getBoolean(BetterMobGriefingGameRule.GLOBAL_RULE);
        result = true;
      }
    };

    // Call the method under test.
    eventHandler.onDetonate(detonateEvent);

    // Perform assertions.
    boolean isSmoking = ReflectionHelper.getPrivateValue(Explosion.class, explosion,
        ObfuscationHelper.convertName("field_82755_b"));
    Assert.assertThat("The explosion's isSmoking field did not match the expected value.",
        isSmoking, CoreMatchers.is(false));
    Assert.assertThat("The explosion's affected blocks did not contain the expected blocks.",
        explosion.getAffectedBlockPositions(), CoreMatchers.is(affectedBlockPositions));
  }

  /**
   * Test that the matching affected entity is used and the default behavior is overridden when the
   * explosion has no source, there is a matching affected entity and the global value is true and
   * entity value is false.
   */
  @Test
  public void testOnDetonate_noSourceEntityMatchGlobalTrueEntityFalse_useEntityOverride(
      @Mocked EntityLiving entity, @Mocked EntityFireball affectedFireball1,
      @Mocked EntityFireball affectedFireball2, @Mocked EntityFireball affectedFireball3,
      @Mocked EntityFireball affectedFireball4, @Mocked EntityFireball affectedFireball5) {
    // Set up test data.
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

    List<BlockPos> affectedBlockPositions = Collections.singletonList(new BlockPos(0, 0, 0));
    Explosion explosion =
        new Explosion(null, null, 1, 1, 1, 0, false, true, affectedBlockPositions);

    Detonate detonateEvent = new Detonate(world, explosion, Arrays.asList(null, affectedFireball1,
        affectedFireball2, affectedFireball3, affectedFireball4, affectedFireball5));

    GameRules gameRules = new GameRules();

    // Record expectations.
    new Expectations(gameRules, BetterMobGriefingGameRule.class) {
      {
        BetterMobGriefingGameRule.isMobGriefingEnabled(entity);
        result = false;

        world.getGameRules();
        result = gameRules;

        gameRules.getBoolean(BetterMobGriefingGameRule.GLOBAL_RULE);
        result = true;
      }
    };

    // Call the method under test.
    eventHandler.onDetonate(detonateEvent);

    // Perform assertions.
    boolean isSmoking = ReflectionHelper.getPrivateValue(Explosion.class, explosion,
        ObfuscationHelper.convertName("field_82755_b"));
    Assert.assertThat("The explosion's isSmoking field did not match the expected value.",
        isSmoking, CoreMatchers.is(false));
    Assert.assertThat("The explosion's affected blocks did not contain the expected blocks.",
        explosion.getAffectedBlockPositions(), CoreMatchers.is(Collections.emptyList()));
  }

  /**
   * Test that the matching affected entity is used and the default behavior is overridden when the
   * explosion has no source, there is a matching affected entity and the global value is false and
   * entity value is true.
   */
  @Test
  public void testOnDetonate_noSourceEntityMatchGlobalFalseEntityTrue_useEntityOverride(
      @Mocked EntityLiving entity, @Mocked EntityFireball affectedFireball1,
      @Mocked EntityFireball affectedFireball2, @Mocked EntityFireball affectedFireball3,
      @Mocked EntityFireball affectedFireball4, @Mocked EntityFireball affectedFireball5) {
    // Set up test data.
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

    List<BlockPos> affectedBlockPositions = Collections.singletonList(new BlockPos(0, 0, 0));
    Explosion explosion =
        new Explosion(null, null, 1, 1, 1, 0, false, false, affectedBlockPositions);

    Detonate detonateEvent = new Detonate(world, explosion, Arrays.asList(null, affectedFireball1,
        affectedFireball2, affectedFireball3, affectedFireball4, affectedFireball5));

    GameRules gameRules = new GameRules();

    // Record expectations.
    new Expectations(gameRules, BetterMobGriefingGameRule.class) {
      {
        BetterMobGriefingGameRule.isMobGriefingEnabled(entity);
        result = true;

        world.getGameRules();
        result = gameRules;

        gameRules.getBoolean(BetterMobGriefingGameRule.GLOBAL_RULE);
        result = false;
      }
    };

    // Call the method under test.
    eventHandler.onDetonate(detonateEvent);

    // Perform assertions.
    boolean isSmoking = ReflectionHelper.getPrivateValue(Explosion.class, explosion,
        ObfuscationHelper.convertName("field_82755_b"));
    Assert.assertThat("The explosion's isSmoking field did not match the expected value.",
        isSmoking, CoreMatchers.is(true));
    Assert.assertThat("The explosion's affected blocks did not contain the expected blocks.",
        explosion.getAffectedBlockPositions(), CoreMatchers.is(affectedBlockPositions));
  }

  /**
   * Test that the the default behavior is used when the explosion has no source and there is no
   * matching affected entity.
   */
  @Test
  public void testOnDetonate_noSourceNoEntityMatch_defaultBehaviour(
      @Mocked EntityFireball affectedFireball1, @Mocked EntityFireball affectedFireball2,
      @Mocked EntityFireball affectedFireball3, @Mocked EntityFireball affectedFireball4) {
    // Set up test data.
    affectedFireball2.posX = 1;
    affectedFireball3.posX = 1;
    affectedFireball3.posY = 1;
    affectedFireball4.posX = 1;
    affectedFireball4.posY = 1;
    affectedFireball4.posZ = 1;

    List<BlockPos> affectedBlockPositions = Collections.singletonList(new BlockPos(0, 0, 0));
    Explosion explosion =
        new Explosion(null, null, 1, 1, 1, 0, false, false, affectedBlockPositions);

    Detonate detonateEvent = new Detonate(world, explosion, Arrays.asList(null, affectedFireball1,
        affectedFireball2, affectedFireball3, affectedFireball4));

    // Call the method under test.
    eventHandler.onDetonate(detonateEvent);

    // Perform assertions.
    boolean isSmoking = ReflectionHelper.getPrivateValue(Explosion.class, explosion,
        ObfuscationHelper.convertName("field_82755_b"));
    Assert.assertThat("The explosion's isSmoking field did not match the expected value.",
        isSmoking, CoreMatchers.is(false));
    Assert.assertThat("The explosion's affected blocks did not contain the expected blocks.",
        explosion.getAffectedBlockPositions(), CoreMatchers.is(affectedBlockPositions));
  }
}
