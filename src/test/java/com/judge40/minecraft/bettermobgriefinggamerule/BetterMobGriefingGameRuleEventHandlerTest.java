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
import java.util.Random;

import org.hamcrest.CoreMatchers;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.judge40.minecraft.bettermobgriefinggamerule.entity.ai.BetterMobGriefingGameRuleEntityAIBreakDoor;
import com.judge40.minecraft.bettermobgriefinggamerule.entity.ai.BetterMobGriefingGameRuleEntityAIEatGrass;
import com.judge40.minecraft.bettermobgriefinggamerule.entity.ai.BetterMobGriefingGameRuleEntityAIOverrideMobGriefingBehaviour;

import cpw.mods.fml.common.ObfuscationReflectionHelper;
import mockit.Deencapsulation;
import mockit.Invocation;
import mockit.Mock;
import mockit.MockUp;
import net.minecraft.block.Block;
import net.minecraft.entity.Entity;
import net.minecraft.entity.EntityLiving;
import net.minecraft.entity.ai.EntityAIBreakDoor;
import net.minecraft.entity.ai.EntityAIEatGrass;
import net.minecraft.entity.ai.EntityAITasks.EntityAITaskEntry;
import net.minecraft.entity.boss.EntityWither;
import net.minecraft.entity.monster.EntityCreeper;
import net.minecraft.entity.monster.EntityGhast;
import net.minecraft.entity.monster.EntitySilverfish;
import net.minecraft.entity.monster.EntityZombie;
import net.minecraft.entity.passive.EntitySheep;
import net.minecraft.entity.projectile.EntityFireball;
import net.minecraft.entity.projectile.EntityLargeFireball;
import net.minecraft.init.Blocks;
import net.minecraft.world.Explosion;
import net.minecraft.world.GameRules;
import net.minecraft.world.World;
import net.minecraftforge.event.entity.EntityJoinWorldEvent;
import net.minecraftforge.event.entity.living.LivingEvent.LivingUpdateEvent;
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
    world = null;
    gameRules = null;
    eventHandler = null;
  }

  /**
   * Test that the explosion's explosionPlacedBy field is used when it is populated
   */
  @Test
  public void testOnDetonateEvent_exploderIsEntityCreeper_entityCreeperUsed() {
    new MockUp<BetterMobGriefingGameRule>() {
      @Mock
      boolean isMobGriefingEnabled(EntityLiving entity) {
        Assert.assertThat("Entity passed as parameter does not match the expected type.", entity,
            CoreMatchers.instanceOf(EntityCreeper.class));
        return false;
      }
    };

    Explosion explosion = new Explosion(null, new EntityCreeper(null), 0, 0, 0, 0);
    explosion.affectedBlockPositions = new ArrayList<>(Collections.singleton("dummyData"));
    explosion.isSmoking = false;

    Detonate detonateEvent = new Detonate(world, explosion, null);
    eventHandler.onDetonateEvent(detonateEvent);
  }

  /**
   * Test that the when exploder is a fireball and shooting entity is not null that the shooting
   * entity is used.
   */
  @Test
  public void testOnDetonateEvent_exploderIsFireballShootingEntityNotNull_shootingEntityUsed() {
    EntityLiving shootingEntity = Deencapsulation.newUninitializedInstance(EntityLiving.class);

    new MockUp<BetterMobGriefingGameRule>() {
      @Mock
      boolean isMobGriefingEnabled(EntityLiving entity) {
        Assert.assertThat("Entity passed as parameter does not match the expected type.", entity,
            CoreMatchers.is(shootingEntity));
        return true;
      }
    };

    EntityFireball fireball = Deencapsulation.newUninitializedInstance(EntityFireball.class);
    fireball.shootingEntity = shootingEntity;

    Explosion explosion = new Explosion(world, fireball, 0, 0, 0, 0);
    explosion.affectedBlockPositions = new ArrayList<>(Collections.singleton("dummyData"));
    explosion.isSmoking = false;

    Detonate detonateEvent = new Detonate(world, explosion, null);
    eventHandler.onDetonateEvent(detonateEvent);
  }

  /**
   * Test that when the exploder is a fireball and the shooting entity is null the affected entities
   * are checked for the source of the explosion
   */
  @Test
  public void testOnDetonateEvent_exploderIsFireballShootingEntityNull_affectedEntitiesChecked() {
    new MockUp<BetterMobGriefingGameRule>() {
      @Mock
      boolean isMobGriefingEnabled(EntityLiving entity) {
        Assert.assertThat("Entity passed as parameter does not match the expected type.", entity,
            CoreMatchers.instanceOf(EntityGhast.class));
        return true;
      }
    };

    Explosion explosion = new Explosion(world,
        Deencapsulation.newUninitializedInstance(EntityFireball.class), 1, 1, 1, 0);
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
   * Test that the explosion's explosionPlacedBy field is used when it is populated
   */
  @Test
  public void testOnDetonateEvent_exploderIsNullFireballShootingEntityGhast_entityGhastUsed() {
    new MockUp<BetterMobGriefingGameRule>() {
      @Mock
      boolean isMobGriefingEnabled(EntityLiving entity) {
        Assert.assertThat("Entity passed as parameter does not match the expected type.", entity,
            CoreMatchers.instanceOf(EntityGhast.class));
        return true;
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
      boolean isMobGriefingEnabled(EntityLiving entity) {
        return true;
      }
    };

    gameRules.setOrCreateGameRule(BetterMobGriefingGameRule.ORIGINAL, "true");

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
      boolean isMobGriefingEnabled(EntityLiving entity) {
        return false;
      }
    };

    gameRules.setOrCreateGameRule(BetterMobGriefingGameRule.ORIGINAL, "true");

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
      boolean isMobGriefingEnabled(EntityLiving entity) {
        return true;
      }
    };

    gameRules.setOrCreateGameRule(BetterMobGriefingGameRule.ORIGINAL, "false");

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
      boolean isMobGriefingEnabled(EntityLiving entity) {
        return false;
      }
    };

    gameRules.setOrCreateGameRule(BetterMobGriefingGameRule.ORIGINAL, "false");

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
   * Test that when the entity is an EntityWither the mobGriefing override task is added to the task
   * list
   */
  @Test
  public void testOnEntityJoinWorldEvent_entityWither_mobGriefingOverrideTaskAdded() {
    EntityWither entityWither = new EntityWither(null);
    EntityJoinWorldEvent entityJoinWorldEvent = new EntityJoinWorldEvent(entityWither, world);
    eventHandler.onEntityJoinWorldEvent(entityJoinWorldEvent);

    Iterator<?> entityAiTaskEntryIterator = entityWither.tasks.taskEntries.iterator();

    while (entityAiTaskEntryIterator.hasNext()) {
      EntityAITaskEntry entityAiTaskEntry = (EntityAITaskEntry) entityAiTaskEntryIterator.next();

      if (entityAiTaskEntry.action instanceof BetterMobGriefingGameRuleEntityAIOverrideMobGriefingBehaviour) {
        break;
      }

      if (!entityAiTaskEntryIterator.hasNext()) {
        Assert.fail(
            "BetterMobGriefingGameRuleEntityAIOverrideMobGriefingBehaviour task was not found in the Wither's task list.");
      }
    }
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
   * Test that EntityAIEatGrass tasks are replaced when they exist
   */
  @Test
  public void testOnEntityJoinWorldEvent_entitySheepWithEatGrassTask_taskReplaced() {
    EntitySheep entitySheep = new EntitySheep(null);
    EntityJoinWorldEvent entityJoinWorldEvent = new EntityJoinWorldEvent(entitySheep, world);
    eventHandler.onEntityJoinWorldEvent(entityJoinWorldEvent);

    // Check that the eat grass task in the Sheep's task list was replaced
    Iterator<?> entityAiTaskEntryIterator = entitySheep.tasks.taskEntries.iterator();
    boolean taskFound = false;

    while (entityAiTaskEntryIterator.hasNext()) {
      EntityAITaskEntry entityAiTaskEntry = (EntityAITaskEntry) entityAiTaskEntryIterator.next();

      if (entityAiTaskEntry.action.getClass().equals(EntityAIEatGrass.class)) {
        Assert.fail(
            "Original EntityAIEatGrass task was found in the Sheep's task list, but should have been replaced.");
      } else if (entityAiTaskEntry.action instanceof BetterMobGriefingGameRuleEntityAIEatGrass) {
        taskFound = true;
      }
    }

    Assert.assertThat("Override version of the EntityAIEatGrass task was not found.", taskFound,
        CoreMatchers.is(true));
  }

  /**
   * Test that no exception is thrown when an unhandled Entity type is passed to the event handler
   */
  @Test
  public void testOnEntityJoinWorldEvent_entity_noException() {
    Entity entity = Deencapsulation.newUninitializedInstance(Entity.class);
    EntityJoinWorldEvent entityJoinWorldEvent = new EntityJoinWorldEvent(entity, world);
    eventHandler.onEntityJoinWorldEvent(entityJoinWorldEvent);
  }

  /**
   * Test that handleSilverfishSummonAlly is called when LivingUpdateEvent is for an
   * EntitySilverfish
   */
  @Test
  public void testOnLivingUpdateEvent_entitySilverfish_handleSilverfishSummonAllyCalled() {
    new MockUp<BetterMobGriefingGameRuleEventHandler>() {
      @Mock(invocations = 1)
      void handleSilverfishSummonAlly(EntitySilverfish entitySilverfish) {

      }
    };

    LivingUpdateEvent livingUpdateEvent = new LivingUpdateEvent(new EntitySilverfish(null));
    eventHandler.onLivingUpdateEvent(livingUpdateEvent);
  }

  /**
   * Test that the original cooldown is disabled and the new cooldown is set when the original
   * cooldown is greater than one, this means that after decrementing the cooldown it will still be
   * greater than zero and no processing occurs.
   */
  @Test
  public void testHandleSilverfishSummonAlly_originalCooldownGreaterThanOne_originalCooldownDisabledNewCooldownSetNoSummon() {
    EntitySilverfish entitySilverfish = new EntitySilverfish(null);
    Deencapsulation.setField(entitySilverfish, "allySummonCooldown", 10);
    Deencapsulation.setField(entitySilverfish, "worldObj", world);

    Deencapsulation.invoke(BetterMobGriefingGameRuleEventHandler.class,
        "handleSilverfishSummonAlly", entitySilverfish);

    int originalAllySummonCooldown =
        Deencapsulation.getField(entitySilverfish, "allySummonCooldown");
    Assert.assertThat("Original ally summon should have been disabled.", originalAllySummonCooldown,
        CoreMatchers.is(-1));

    int overrideAllySummonCooldown =
        entitySilverfish.getEntityData().getInteger("bettermobgriefinggamerulecooldown");
    Assert.assertThat("Ally summon cooldown should have been set in to the entity data.",
        overrideAllySummonCooldown, CoreMatchers.is(9));
  }

  /**
   * Test that the original cooldown is disabled and the new cooldown is set when the original
   * cooldown is one, this means that after decrementing the cooldown it will be zero and processing
   * occurs. mobGriefing is true so block damage should occur.
   */
  @Test
  public void testHandleSilverfishSummonAlly_originalCooldownIsOneMobGriefingTrue_originalCooldownDisabledNewCooldownSetDoSummonDoMobGriefing() {
    new MockUp<World>() {
      @Mock
      Block getBlock(int p_147439_1_, int p_147439_2_, int p_147439_3_) {
        return Blocks.monster_egg;
      }

      @Mock
      int getBlockMetadata(int p_72805_1_, int p_72805_2_, int p_72805_3_) {
        return -1;
      }

      @Mock(invocations = 0)
      boolean setBlock(int p_147465_1_, int p_147465_2_, int p_147465_3_, Block p_147465_4_,
          int p_147465_5_, int p_147465_6_) {
        return true;
      }

      @Mock(minInvocations = 1)
      public boolean func_147480_a(int p_147480_1_, int p_147480_2_, int p_147480_3_,
          boolean p_147480_4_) {
        return true;
      }
    };

    new MockUp<Block>() {
      @Mock
      void onBlockDestroyedByPlayer(World p_149664_1_, int p_149664_2_, int p_149664_3_,
          int p_149664_4_, int p_149664_5_) {

      }
    };

    new MockUp<Random>() {
      @Mock
      boolean nextBoolean(Invocation invocation) {
        return invocation.getInvocationCount() >= 500;
      }
    };

    new MockUp<BetterMobGriefingGameRule>() {
      @Mock
      boolean isMobGriefingEnabled(EntityLiving entity) {
        return true;
      }
    };

    Block block = Deencapsulation.newUninitializedInstance(Block.class);
    Deencapsulation.setField(Blocks.class, "monster_egg", block);

    EntitySilverfish entitySilverfish = new EntitySilverfish(null);
    Deencapsulation.setField(entitySilverfish, "allySummonCooldown", 1);
    Deencapsulation.setField(entitySilverfish, "rand", new Random());
    Deencapsulation.setField(entitySilverfish, "worldObj", world);

    Deencapsulation.invoke(BetterMobGriefingGameRuleEventHandler.class,
        "handleSilverfishSummonAlly", entitySilverfish);

    int originalAllySummonCooldown =
        Deencapsulation.getField(entitySilverfish, "allySummonCooldown");
    Assert.assertThat("Original ally summon should have been disabled.", originalAllySummonCooldown,
        CoreMatchers.is(-1));

    int overrideAllySummonCooldown =
        entitySilverfish.getEntityData().getInteger("bettermobgriefinggamerulecooldown");
    Assert.assertThat("Ally summon cooldown should have been set in to the entity data.",
        overrideAllySummonCooldown, CoreMatchers.is(0));
  }

  /**
   * Test that the original cooldown is disabled and the new cooldown is set when the original
   * cooldown is one, this means that after decrementing the cooldown it will be zero and processing
   * occurs. mobGriefing is false so no block damage should occur.
   */
  @Test
  public void testHandleSilverfishSummonAlly_originalCooldownIsOneMobGriefingFalse_originalCooldownDisabledNewCooldownSetDoSummonNoMobGriefing() {
    new MockUp<World>() {
      @Mock
      Block getBlock(int p_147439_1_, int p_147439_2_, int p_147439_3_) {
        return Blocks.monster_egg;
      }

      @Mock
      int getBlockMetadata(int p_72805_1_, int p_72805_2_, int p_72805_3_) {
        return -1;
      }

      @Mock(minInvocations = 1)
      boolean setBlock(int p_147465_1_, int p_147465_2_, int p_147465_3_, Block p_147465_4_,
          int p_147465_5_, int p_147465_6_) {
        return true;
      }

      @Mock(invocations = 0)
      public boolean func_147480_a(int p_147480_1_, int p_147480_2_, int p_147480_3_,
          boolean p_147480_4_) {
        return true;
      }
    };

    new MockUp<Block>() {
      @Mock
      void onBlockDestroyedByPlayer(World p_149664_1_, int p_149664_2_, int p_149664_3_,
          int p_149664_4_, int p_149664_5_) {

      }
    };

    new MockUp<Random>() {
      @Mock
      boolean nextBoolean(Invocation invocation) {
        return invocation.getInvocationCount() > 500;
      }
    };

    new MockUp<BetterMobGriefingGameRule>() {
      @Mock
      boolean isMobGriefingEnabled(EntityLiving entity) {
        return false;
      }
    };

    Block block = Deencapsulation.newUninitializedInstance(Block.class);
    Deencapsulation.setField(Blocks.class, "monster_egg", block);

    EntitySilverfish entitySilverfish = new EntitySilverfish(null);
    Deencapsulation.setField(entitySilverfish, "allySummonCooldown", 1);
    Deencapsulation.setField(entitySilverfish, "rand", new Random());
    Deencapsulation.setField(entitySilverfish, "worldObj", world);

    Deencapsulation.invoke(BetterMobGriefingGameRuleEventHandler.class,
        "handleSilverfishSummonAlly", entitySilverfish);

    int originalAllySummonCooldown =
        Deencapsulation.getField(entitySilverfish, "allySummonCooldown");
    Assert.assertThat("Original ally summon should have been disabled.", originalAllySummonCooldown,
        CoreMatchers.is(-1));

    int overrideAllySummonCooldown =
        entitySilverfish.getEntityData().getInteger("bettermobgriefinggamerulecooldown");
    Assert.assertThat("Ally summon cooldown should have been set in to the entity data.",
        overrideAllySummonCooldown, CoreMatchers.is(0));
  }

  /**
   * Test that the original cooldown is disabled and the new cooldown is set when the original
   * cooldown is zero.
   */
  @Test
  public void testHandleSilverfishSummonAlly_originalCooldownIsZero_originalCooldownDisabledNewCooldownSetNoSummon() {
    EntitySilverfish entitySilverfish = new EntitySilverfish(null);
    Deencapsulation.setField(entitySilverfish, "allySummonCooldown", 0);
    Deencapsulation.setField(entitySilverfish, "worldObj", world);

    Deencapsulation.invoke(BetterMobGriefingGameRuleEventHandler.class,
        "handleSilverfishSummonAlly", entitySilverfish);

    int originalAllySummonCooldown =
        Deencapsulation.getField(entitySilverfish, "allySummonCooldown");
    Assert.assertThat("Original ally summon should have been disabled.", originalAllySummonCooldown,
        CoreMatchers.is(-1));

    boolean hasKey = entitySilverfish.getEntityData().hasKey("bettermobgriefinggamerulecooldown");
    Assert.assertThat("Ally summon cooldown should have been set in to the entity data.", hasKey,
        CoreMatchers.is(true));
  }

  /**
   * Test that when the original cooldown is disabled the new cooldown is used if it was already
   * populated
   */
  @Test
  public void testHandleSilverfishSummonAlly_originalCooldownIsDisabledNewCooldownExists_newCooldownUsed() {
    EntitySilverfish entitySilverfish = new EntitySilverfish(null);
    Deencapsulation.setField(entitySilverfish, "allySummonCooldown", -1);
    Deencapsulation.setField(entitySilverfish, "worldObj", world);
    entitySilverfish.getEntityData().setInteger("bettermobgriefinggamerulecooldown", 10);

    Deencapsulation.invoke(BetterMobGriefingGameRuleEventHandler.class,
        "handleSilverfishSummonAlly", entitySilverfish);

    int originalAllySummonCooldown =
        Deencapsulation.getField(entitySilverfish, "allySummonCooldown");
    Assert.assertThat("Original ally summon should have been disabled.", originalAllySummonCooldown,
        CoreMatchers.is(-1));

    int overrideAllySummonCooldown =
        entitySilverfish.getEntityData().getInteger("bettermobgriefinggamerulecooldown");
    Assert.assertThat("Ally summon cooldown should have been set in to the entity data.",
        overrideAllySummonCooldown, CoreMatchers.is(9));
  }

  /**
   * Test that if the world is a client world no processing is done, all processing should be done
   * in the server world only.
   */
  @Test
  public void testHandleSilverfishSummonAlly_isRemoteWorld_skipProcessing() {
    new MockUp<ObfuscationReflectionHelper>() {
      @Mock(invocations = 0)
      <T, E> T getPrivateValue(Class<? super E> classToAccess, E instance, String... fieldNames) {
        return null;
      }
    };

    EntitySilverfish entitySilverfish = new EntitySilverfish(null);
    world.isRemote = true;
    Deencapsulation.setField(entitySilverfish, "worldObj", world);

    Deencapsulation.invoke(BetterMobGriefingGameRuleEventHandler.class,
        "handleSilverfishSummonAlly", entitySilverfish);
  }
}
