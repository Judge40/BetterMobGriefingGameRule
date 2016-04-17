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
import java.util.List;
import java.util.Set;

import org.hamcrest.CoreMatchers;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.judge40.minecraft.bettermobgriefinggamerule.command.BetterMobGriefingGameRuleCommandGameRule;
import com.judge40.minecraft.bettermobgriefinggamerule.world.BetterMobGriefingGameRuleWorldSavedData;

import cpw.mods.fml.common.event.FMLInitializationEvent;
import cpw.mods.fml.common.event.FMLServerStartingEvent;
import cpw.mods.fml.common.eventhandler.EventBus;
import mockit.Deencapsulation;
import mockit.Mock;
import mockit.MockUp;
import net.minecraft.command.CommandGameRule;
import net.minecraft.command.CommandHandler;
import net.minecraft.command.ICommand;
import net.minecraft.command.ICommandManager;
import net.minecraft.entity.EntityList;
import net.minecraft.entity.EntityLiving;
import net.minecraft.entity.monster.EntityZombie;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.server.MinecraftServer;
import net.minecraft.world.GameRules;
import net.minecraft.world.World;
import net.minecraft.world.storage.MapStorage;
import net.minecraft.world.storage.WorldInfo;

/**
 * Tests for BetterMobGriefingGameRule
 */
public class BetterMobGriefingGameRuleTest {

  private BetterMobGriefingGameRule betterMobGriefingGameRule;

  private World world;

  /**
   * @throws java.lang.Exception
   */
  @Before
  public void setUp() throws Exception {
    betterMobGriefingGameRule = new BetterMobGriefingGameRule();
    world = Deencapsulation.newUninitializedInstance(World.class);
    WorldInfo worldInfo = new WorldInfo(new NBTTagCompound());
    Deencapsulation.setField(world, "worldInfo", worldInfo);

    MapStorage mapStorage = new MapStorage(null);
    world.mapStorage = mapStorage;
  }

  /**
   * @throws java.lang.Exception
   */
  @After
  public void tearDown() throws Exception {
    betterMobGriefingGameRule = null;
    world = null;
  }

  /**
   * Test that the event handler is initialized on FMLInitializationEvent
   */
  @Test
  public void testOnFMLInitializationEvent_eventHandlerInitialized() {
    new MockUp<EventBus>() {
      @Mock(invocations = 1)
      void register(Object target) {
        Assert.assertThat("Initialized object is not the expected event handler", target,
            CoreMatchers.instanceOf(BetterMobGriefingGameRuleEventHandler.class));
      }
    };

    betterMobGriefingGameRule.onFMLInitializationEvent(new FMLInitializationEvent());
  }

  /**
   * Test that the new game rules are added on FMLServerStartedEvent
   */
  @Test
  public void testOnFMLServerStartingEvent_mobGriefingGameRulesAndCommandsAdded() {
    new MockUp<MinecraftServer>() {
      @Mock
      void $clinit() {

      }
    };

    CommandHandler commandHandler = new CommandHandler();
    CommandGameRule commandGameRule = new CommandGameRule();
    commandHandler.registerCommand(new CommandGameRule());

    MinecraftServer mockServer = new MockUp<MinecraftServer>() {
      @Mock
      ICommandManager getCommandManager() {
        return commandHandler;
      }
    }.getMockInstance();

    new MockUp<BetterMobGriefingGameRule>() {
      @Mock(invocations = 1)
      void addMobGriefingGameRules() {

      }
    };

    new MockUp<MigratePre030BetterMobGriefingGameRule>() {
      @Mock(invocations = 1)
      void migrateGameRulesToWorldData() {

      }
    };

    betterMobGriefingGameRule.onFMLServerStartingEvent(new FMLServerStartingEvent(mockServer));

    // Test that the command map contains the correct gamerule command
    ICommand commandMapCommand =
        (ICommand) commandHandler.getCommands().get(commandGameRule.getCommandName());
    Assert.assertThat("The game rule command in the command map is not of the expected type.",
        commandMapCommand, CoreMatchers.instanceOf(BetterMobGriefingGameRuleCommandGameRule.class));

    // Test that the original command was removed from the command set
    Set<ICommand> commandSet = Deencapsulation.getField(commandHandler, "commandSet");
    Assert.assertThat("The original game rule command was not removed from the command set.",
        commandSet, CoreMatchers.not(CoreMatchers.hasItem(commandGameRule)));
  }

  /**
   * Test that game rules are created for all expected entity classes
   */
  @Test
  public void testAddMobGriefingGameRules_allExpectedEntityClassesAdded() {
    List<Class<? extends EntityLiving>> addedEntityClasses = new ArrayList<>();

    new MockUp<BetterMobGriefingGameRule>() {
      @Mock
      void addMobGriefingGameRule(Class<? extends EntityLiving> entityClass) {
        addedEntityClasses.add(entityClass);
      }

    };

    BetterMobGriefingGameRule.addMobGriefingGameRules();

    List<Class<? extends EntityLiving>> expectedEntityClasses =
        Deencapsulation.getField(BetterMobGriefingGameRule.class, "MOB_GRIEFING_ENTITY_CLASSES");
    Assert.assertThat(
        "The entity classes to add game rules for does not match the expected classes.",
        addedEntityClasses, CoreMatchers.is(expectedEntityClasses));
  }

  /**
   * Test that the entity has a game rule created when the entity is registered in EntityList and
   * the rule does not already exist
   */
  @Test
  public void testAddMobGriefingGameRule_entityRegisteredRuleNotExists_entityGameRuleAdded() {
    new MockUp<MinecraftServer>() {
      @Mock
      World getEntityWorld() {
        return world;
      }

      @Mock
      MinecraftServer getServer() {
        return Deencapsulation.newUninitializedInstance(MinecraftServer.class);
      }
    };

    BetterMobGriefingGameRule.addMobGriefingGameRule(EntityLiving.class);
    BetterMobGriefingGameRuleWorldSavedData worldSavedData =
        BetterMobGriefingGameRuleWorldSavedData.forWorld(world);

    String entityName = (String) EntityList.classToStringMapping.get(EntityLiving.class);
    String entityMobGriefingValue = worldSavedData.entityNamesToMobGriefingValue.get(entityName);
    Assert.assertThat("The entity mob griefing value does not match the expected value.",
        entityMobGriefingValue, CoreMatchers.is(BetterMobGriefingGameRule.INHERIT));

    Assert.assertThat("The world saved data was expected to be marked as dirty.",
        worldSavedData.isDirty(), CoreMatchers.is(true));
  }

  /**
   * Test that the entities game rule is unchanged when the entity is registered in EntityList and
   * the rule already exists
   */
  @Test
  public void testAddMobGriefingGameRule_entityRegisteredRuleExists_entityGameRuleUnchanged() {
    new MockUp<MinecraftServer>() {
      @Mock
      World getEntityWorld() {
        return world;
      }

      @Mock
      MinecraftServer getServer() {
        return Deencapsulation.newUninitializedInstance(MinecraftServer.class);
      }
    };

    BetterMobGriefingGameRuleWorldSavedData worldSavedData =
        BetterMobGriefingGameRuleWorldSavedData.forWorld(world);
    String entityName = (String) EntityList.classToStringMapping.get(EntityLiving.class);
    worldSavedData.entityNamesToMobGriefingValue.put(entityName, Boolean.toString(false));

    BetterMobGriefingGameRule.addMobGriefingGameRule(EntityLiving.class);

    String entityMobGriefingValue = worldSavedData.entityNamesToMobGriefingValue.get(entityName);
    Assert.assertThat("The entity mob griefing value does not match the expected value.",
        entityMobGriefingValue, CoreMatchers.is(Boolean.toString(false)));

    Assert.assertThat("The world saved data was not expected to be marked as dirty.",
        worldSavedData.isDirty(), CoreMatchers.is(false));
  }

  /**
   * Test that the entity does not have a game rule created when the entity is not registered in
   * EntityList
   */
  @Test
  public void testAddMobGriefingGameRule_entityNotRegistered_noEntityGameRuleAdded() {
    new MockUp<MinecraftServer>() {
      @Mock
      World getEntityWorld() {
        return world;
      }

      @Mock
      MinecraftServer getServer() {
        return Deencapsulation.newUninitializedInstance(MinecraftServer.class);
      }
    };

    EntityLiving entityLiving = new EntityLiving(null) {};
    BetterMobGriefingGameRule.addMobGriefingGameRule(entityLiving.getClass());

    BetterMobGriefingGameRuleWorldSavedData worldSavedData =
        BetterMobGriefingGameRuleWorldSavedData.forWorld(world);
    String entityName = (String) EntityList.classToStringMapping.get(EntityLiving.class);
    String entityMobGriefingValue = worldSavedData.entityNamesToMobGriefingValue.get(entityName);
    Assert.assertThat("The entity mob griefing value does not match the expected value.",
        entityMobGriefingValue, CoreMatchers.nullValue());

    Assert.assertThat("The world saved data was not expected to be marked as dirty.",
        worldSavedData.isDirty(), CoreMatchers.is(false));
  }

  /**
   * Test that mob griefing is enabled when the entity is registered and the world data mob griefing
   * value for the entity is true
   */
  @Test
  public void testIsMobGriefingEnabled_entityRegisteredEntityMobGriefingValueTrue_mobGriefingEnabled() {
    new MockUp<GameRules>() {
      @Mock(invocations = 0)
      boolean getGameRuleBooleanValue(String gameRule) {
        return false;
      }
    };

    EntityLiving entity = new EntityZombie(null);
    entity.worldObj = world;

    BetterMobGriefingGameRuleWorldSavedData worldSavedData =
        new BetterMobGriefingGameRuleWorldSavedData(BetterMobGriefingGameRule.MODID);
    worldSavedData.entityNamesToMobGriefingValue.put(EntityList.getEntityString(entity),
        Boolean.toString(true));

    MapStorage mapStorage = new MapStorage(null);
    mapStorage.setData(BetterMobGriefingGameRule.MODID, worldSavedData);
    world.mapStorage = mapStorage;

    boolean mobGriefingEnabled = BetterMobGriefingGameRule.isMobGriefingEnabled(entity);
    Assert.assertThat("Mob griefing should be enabled.", mobGriefingEnabled, CoreMatchers.is(true));
  }

  /**
   * Test that mob griefing is disabled when the entity is registered and the world data mob
   * griefing value for the entity is false
   */
  @Test
  public void testIsMobGriefingEnabled_entityRegisteredEntityMobGriefingValueFalse_mobGriefingDisabled() {
    new MockUp<GameRules>() {
      @Mock(invocations = 0)
      boolean getGameRuleBooleanValue(String gameRule) {
        return false;
      }
    };

    EntityLiving entity = new EntityZombie(null);
    entity.worldObj = world;

    BetterMobGriefingGameRuleWorldSavedData worldSavedData =
        new BetterMobGriefingGameRuleWorldSavedData(BetterMobGriefingGameRule.MODID);
    worldSavedData.entityNamesToMobGriefingValue.put(EntityList.getEntityString(entity),
        Boolean.toString(false));

    MapStorage mapStorage = new MapStorage(null);
    mapStorage.setData(BetterMobGriefingGameRule.MODID, worldSavedData);
    world.mapStorage = mapStorage;

    boolean mobGriefingEnabled = BetterMobGriefingGameRule.isMobGriefingEnabled(entity);
    Assert.assertThat("Mob griefing should be disabled.", mobGriefingEnabled,
        CoreMatchers.is(false));
  }

  /**
   * Test that mob griefing defaults to the original game rule when the entity is registered and the
   * world data mob griefing value is inherit
   */
  @Test
  public void testIsMobGriefingEnabled_entityRegisteredEntityMobGriefingValueInherit_originalGameRuleValue() {
    new MockUp<GameRules>() {
      @Mock(invocations = 1)
      boolean getGameRuleBooleanValue(String gameRule) {
        return false;
      }
    };

    EntityLiving entity = new EntityZombie(null);
    entity.worldObj = world;

    BetterMobGriefingGameRuleWorldSavedData worldSavedData =
        new BetterMobGriefingGameRuleWorldSavedData(BetterMobGriefingGameRule.MODID);
    worldSavedData.entityNamesToMobGriefingValue.put(EntityList.getEntityString(entity),
        BetterMobGriefingGameRule.INHERIT);

    MapStorage mapStorage = new MapStorage(null);
    mapStorage.setData(BetterMobGriefingGameRule.MODID, worldSavedData);
    world.mapStorage = mapStorage;

    boolean mobGriefingEnabled = BetterMobGriefingGameRule.isMobGriefingEnabled(entity);
    Assert.assertThat("Mob griefing should be disabled.", mobGriefingEnabled,
        CoreMatchers.is(false));
  }

  /**
   * Test that mob griefing defaults to the original game rule when the entity is registered but the
   * world data mob griefing value does not exist
   */
  @Test
  public void testIsMobGriefingEnabled_entityRegisteredEntityMobGriefingValueNull_originalGameRuleValue() {
    new MockUp<GameRules>() {
      @Mock(invocations = 1)
      boolean getGameRuleBooleanValue(String gameRule) {
        return false;
      }
    };

    EntityLiving entity = new EntityZombie(null);
    entity.worldObj = world;

    BetterMobGriefingGameRuleWorldSavedData worldSavedData =
        new BetterMobGriefingGameRuleWorldSavedData(BetterMobGriefingGameRule.MODID);

    MapStorage mapStorage = new MapStorage(null);
    mapStorage.setData(BetterMobGriefingGameRule.MODID, worldSavedData);
    world.mapStorage = mapStorage;

    boolean mobGriefingEnabled = BetterMobGriefingGameRule.isMobGriefingEnabled(entity);
    Assert.assertThat("Mob griefing should be disabled.", mobGriefingEnabled,
        CoreMatchers.is(false));
  }

  /**
   * Test that mob griefing defaults to the original game rule when the entity is not registered
   */
  @Test
  public void testIsMobGriefingEnabled_entityNotRegistered_originalGameRuleValue() {
    new MockUp<GameRules>() {
      @Mock(invocations = 1)
      boolean getGameRuleBooleanValue(String gameRule) {
        return false;
      }
    };

    EntityLiving entity = new EntityLiving(null) {};
    entity.worldObj = world;

    BetterMobGriefingGameRuleWorldSavedData worldSavedData =
        new BetterMobGriefingGameRuleWorldSavedData(BetterMobGriefingGameRule.MODID);

    MapStorage mapStorage = new MapStorage(null);
    mapStorage.setData(BetterMobGriefingGameRule.MODID, worldSavedData);
    world.mapStorage = mapStorage;

    boolean mobGriefingEnabled = BetterMobGriefingGameRule.isMobGriefingEnabled(entity);
    Assert.assertThat("Mob griefing should be disabled.", mobGriefingEnabled,
        CoreMatchers.is(false));
  }
}
