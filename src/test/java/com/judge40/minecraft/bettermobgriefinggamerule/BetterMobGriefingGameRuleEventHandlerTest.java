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
import java.util.Iterator;
import java.util.List;

import org.hamcrest.CoreMatchers;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.judge40.minecraft.bettermobgriefinggamerule.entity.ai.BetterMobGriefingGameRuleEntityAIBreakDoor;

import mockit.Deencapsulation;
import mockit.Mock;
import mockit.MockUp;
import net.minecraft.entity.Entity;
import net.minecraft.entity.EntityLiving;
import net.minecraft.entity.EntityLivingBase;
import net.minecraft.entity.ai.EntityAIBreakDoor;
import net.minecraft.entity.ai.EntityAITasks.EntityAITaskEntry;
import net.minecraft.entity.monster.EntityCreeper;
import net.minecraft.entity.monster.EntityGhast;
import net.minecraft.entity.monster.EntityZombie;
import net.minecraft.entity.projectile.EntityFireball;
import net.minecraft.entity.projectile.EntityLargeFireball;
import net.minecraft.world.Explosion;
import net.minecraft.world.GameRules;
import net.minecraft.world.World;
import net.minecraftforge.event.entity.EntityJoinWorldEvent;
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
   * Test that the explosion's explosionPlacedBy field is used when it is populated
   */
  @Test
  public void testOnDetonateEvent_explosionPlacedByEntityCreeper_entityCreeperUsed() {
    new MockUp<BetterMobGriefingGameRule>() {
      @Mock
      String getMobGriefingRule(GameRules gameRules, EntityLivingBase entity) {
        Assert.assertThat("Entity passed as parameter does not match the expected type.", entity,
            CoreMatchers.instanceOf(EntityCreeper.class));
        return "betterMobGriefing";
      }
    };

    Explosion explosion = new Explosion(null, new EntityCreeper(null), 0, 0, 0, 0);
    explosion.affectedBlockPositions = new ArrayList<>(Collections.singleton("dummyData"));
    explosion.isSmoking = false;

    Detonate detonateEvent = new Detonate(world, explosion, null);
    eventHandler.onDetonateEvent(detonateEvent);
  }

  /**
   * Test that the explosion's explosionPlacedBy field is used when it is populated
   */
  @Test
  public void testOnDetonateEvent_explosionPlacedByNullFireballShootingEntityGhast_entityGhastUsed() {
    new MockUp<BetterMobGriefingGameRule>() {
      @Mock
      String getMobGriefingRule(GameRules gameRules, EntityLivingBase entity) {
        Assert.assertThat("Entity passed as parameter does not match the expected type.", entity,
            CoreMatchers.instanceOf(EntityGhast.class));
        return "betterMobGriefing";
      }
    };

    Explosion explosion = new Explosion(null, null, 1, 1, 1, 0);
    explosion.affectedBlockPositions = new ArrayList<>(Collections.singleton("dummyData"));
    explosion.isSmoking = false;

    EntityFireball fireball1 = new EntityLargeFireball(null,
        Deencapsulation.newUninitializedInstance(EntityLiving.class), 0, 0, 0);
    fireball1.posX = 0;

    EntityFireball fireball2 = new EntityLargeFireball(null,
        Deencapsulation.newUninitializedInstance(EntityLiving.class), 0, 0, 0);
    fireball2.posX = 1;
    fireball2.posY = 0;

    EntityFireball fireball3 = new EntityLargeFireball(null,
        Deencapsulation.newUninitializedInstance(EntityLiving.class), 0, 0, 0);
    fireball3.posX = 1;
    fireball3.posY = 1;
    fireball3.posZ = 0;

    EntityFireball fireball4 = new EntityLargeFireball(null, new EntityGhast(null), 0, 0, 0);
    fireball4.posX = 1;
    fireball4.posY = 1;
    fireball4.posZ = 1;

    List<Entity> entityList = Arrays.asList(fireball1, fireball2, fireball3, fireball4);

    Detonate detonateEvent = new Detonate(world, explosion, entityList);
    eventHandler.onDetonateEvent(detonateEvent);
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

    Explosion explosion = new Explosion(null, new EntityCreeper(null), 0, 0, 0, 0);
    explosion.affectedBlockPositions = new ArrayList<>(Collections.singleton("dummyData"));
    explosion.isSmoking = true;

    Detonate detonateEvent = new Detonate(world, explosion, null);
    eventHandler.onDetonateEvent(detonateEvent);


    Assert.assertThat("Affected block position list size should be 1.",
        explosion.affectedBlockPositions.size(), CoreMatchers.is(1));
    Assert.assertThat("isSmoking should be true", explosion.isSmoking, CoreMatchers.is(true));
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

    Explosion explosion = new Explosion(null, new EntityCreeper(null), 0, 0, 0, 0);
    explosion.affectedBlockPositions = new ArrayList<>(Collections.singleton("dummyData"));
    explosion.isSmoking = true;

    Detonate detonateEvent = new Detonate(world, explosion, null);
    eventHandler.onDetonateEvent(detonateEvent);

    Assert.assertThat("Affected block position list size should be 0.",
        explosion.affectedBlockPositions.size(), CoreMatchers.is(0));
    Assert.assertThat("isSmoking should be false", explosion.isSmoking, CoreMatchers.is(false));

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

    Explosion explosion = new Explosion(null, new EntityCreeper(null), 0, 0, 0, 0);
    explosion.affectedBlockPositions = new ArrayList<>(Collections.singleton("dummyData"));
    explosion.isSmoking = false;

    Detonate detonateEvent = new Detonate(world, explosion, null);
    eventHandler.onDetonateEvent(detonateEvent);

    Assert.assertThat("Affected block position list size should be 1.",
        explosion.affectedBlockPositions.size(), CoreMatchers.is(1));
    Assert.assertThat("isSmoking should be true", explosion.isSmoking, CoreMatchers.is(true));
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

    Explosion explosion = new Explosion(null, new EntityCreeper(null), 0, 0, 0, 0);
    explosion.affectedBlockPositions = new ArrayList<>(Collections.singleton("dummyData"));
    explosion.isSmoking = false;

    Detonate detonateEvent = new Detonate(world, explosion, null);
    eventHandler.onDetonateEvent(detonateEvent);

    Assert.assertThat("Affected block position list size should be 0.",
        explosion.affectedBlockPositions.size(), CoreMatchers.is(0));
    Assert.assertThat("isSmoking should be false", explosion.isSmoking, CoreMatchers.is(false));
  }

  /**
   * Test that the EntityAIBreakDoor task is replaced as expected for EntityZombie when the task is
   * not already populated
   */
  @Test
  public void testOnEntityJoinWorldEvent_entityZombieTaskNotPopulated_breakDoorFieldReplacedTaskNotPopulated() {
    EntityZombie entityZombie = new EntityZombie(null);
    EntityJoinWorldEvent entityJoinWorldEvent = new EntityJoinWorldEvent(entityZombie, world);
    eventHandler.onEntityJoinWorldEvent(entityJoinWorldEvent);

    // Check that the field storing the break door task was replaced
    EntityAIBreakDoor entityAiBreakDoor = Deencapsulation.getField(entityZombie, "field_146075_bs");
    Assert.assertThat("Zombie's break door task was not replaced as expected.", entityAiBreakDoor,
        CoreMatchers.instanceOf(BetterMobGriefingGameRuleEntityAIBreakDoor.class));

    // Check that the Zombie's task list was not populated with the break door task
    Iterator<?> entityAiTaskEntryIterator = entityZombie.tasks.taskEntries.iterator();

    while (entityAiTaskEntryIterator.hasNext()) {
      EntityAITaskEntry entityAiTaskEntry = (EntityAITaskEntry) entityAiTaskEntryIterator.next();

      if (entityAiTaskEntry.action == entityAiBreakDoor) {
        Assert.fail(
            "EntityAIBreakDoor task was found in the Zombie's task list, but should not be populated");
      }
    }
  }

  /**
   * Test that the EntityAIBreakDoor task is replaced as expected for EntityZombie when the task is
   * already populated
   */
  @Test
  public void testOnEntityJoinWorldEvent_entityZombieTaskPopulated_breakDoorFieldReplacedPopulatedTaskReplaced() {
    EntityZombie entityZombie = new EntityZombie(null);
    EntityAIBreakDoor originalBreakDoorTask =
        Deencapsulation.getField(entityZombie, "field_146075_bs");
    entityZombie.tasks.addTask(0, originalBreakDoorTask);
    EntityJoinWorldEvent entityJoinWorldEvent = new EntityJoinWorldEvent(entityZombie, world);
    eventHandler.onEntityJoinWorldEvent(entityJoinWorldEvent);

    // Check that the field storing the break door task was replaced
    EntityAIBreakDoor entityAiBreakDoor = Deencapsulation.getField(entityZombie, "field_146075_bs");
    Assert.assertThat("Zombie's break door task was not replaced as expected.", entityAiBreakDoor,
        CoreMatchers.instanceOf(BetterMobGriefingGameRuleEntityAIBreakDoor.class));

    // Check that the break door task in the Zombie's task list was replaced
    Iterator<?> entityAiTaskEntryIterator = entityZombie.tasks.taskEntries.iterator();

    while (entityAiTaskEntryIterator.hasNext()) {
      EntityAITaskEntry entityAiTaskEntry = (EntityAITaskEntry) entityAiTaskEntryIterator.next();

      if (entityAiTaskEntry.action == originalBreakDoorTask) {
        Assert.fail(
            "Original EntityAIBreakDoor task was found in the Zombie's task list, but should have been replaced.");
      } else if (entityAiTaskEntry.action == entityAiBreakDoor) {
        break;
      }

      if (!entityAiTaskEntryIterator.hasNext()) {
        Assert.fail(
            "BetterMobGriefingGameRuleEntityAIBreakDoor task was not found in the Zombie's task list.");
      }
    }
  }

  /**
   * Test that no exception is thrown when an unhandled Entity type is passed to the event handler
   */
  @Test
  public void testOnEntityJoinWorldEvent_entityLiving_noException() {
    EntityLiving entityLiving = Deencapsulation.newUninitializedInstance(EntityLiving.class);
    EntityJoinWorldEvent entityJoinWorldEvent = new EntityJoinWorldEvent(entityLiving, world);
    eventHandler.onEntityJoinWorldEvent(entityJoinWorldEvent);
  }
}
