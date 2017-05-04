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

import java.io.File;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.hamcrest.CoreMatchers;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.judge40.minecraft.bettermobgriefinggamerule.command.BetterMobGriefingGameRuleCommandGameRule;
import com.judge40.minecraft.bettermobgriefinggamerule.common.config.DefaultMobGriefingConfiguration;
import com.judge40.minecraft.bettermobgriefinggamerule.world.BetterMobGriefingGameRuleWorldSavedData;

import cpw.mods.fml.common.FMLCommonHandler;
import cpw.mods.fml.common.event.FMLInitializationEvent;
import cpw.mods.fml.common.event.FMLPreInitializationEvent;
import cpw.mods.fml.common.event.FMLServerStartingEvent;
import cpw.mods.fml.common.eventhandler.EventBus;
import mockit.Deencapsulation;
import mockit.Expectations;
import mockit.Injectable;
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
import net.minecraftforge.common.config.Configuration;

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
    Deencapsulation.setField(BetterMobGriefingGameRule.class, "defaultEntityRules",
        new HashMap<>());
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
    new MockUp<FMLCommonHandler>() {
      @Mock
      void $init() {

      }

      @Mock
      EventBus bus() {
        return new EventBus();
      }
    };

    new MockUp<EventBus>() {
      @Mock(invocations = 2)
      void register(Object target) {
        Assert.assertThat("Initialized object is not the expected event handler", target,
            CoreMatchers.instanceOf(BetterMobGriefingGameRuleEventHandler.class));
      }
    };

    betterMobGriefingGameRule.onFMLInitializationEvent(new FMLInitializationEvent());
  }

  /**
   * Test that the configuration is loaded as expected
   */
  @Test
  public void testOnFMLPreInitializationEvent_configLoadedAsExpected() {
    new MockUp<BetterMobGriefingGameRule>() {
      @Mock(invocations = 1)
      void populateDefaultMobGriefingRulesFromConfiguration() {

      }
    };

    File dummyFile = new File("");

    new MockUp<Configuration>() {
      @Mock(invocations = 1)
      void $init(File file) {
        Assert.assertThat("The config file used does not match the expected config file.", file,
            CoreMatchers.sameInstance(dummyFile));
      }

      @Mock(invocations = 1)
      void load() {

      }

      @Mock
      Configuration setCategoryLanguageKey(String category, String langKey) {
        return null;
      }
    };

    FMLPreInitializationEvent fmlPreInitializationEvent = new MockUp<FMLPreInitializationEvent>() {
      @Mock
      void $init(Object... data) {

      }

      @Mock(invocations = 1)
      File getSuggestedConfigurationFile() {
        return dummyFile;
      }
    }.getMockInstance();

    betterMobGriefingGameRule.onFMLPreInitializationEvent(fmlPreInitializationEvent);
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
   * Test that the mobGriefing rules are set to the correct value when the default values are
   * populated
   */
  @Test
  public void testAddMobGriefingGameRules_entityDefaultsPopulated_entityRulesAdded() {
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
    worldSavedData.entityNamesToMobGriefingValue.put("existingEntityName001", "existingValue001");
    worldSavedData.entityNamesToMobGriefingValue.put("dummyEntityName001", "existingValue002");

    Map<String, String> defaultEntityRules =
        Deencapsulation.getField(BetterMobGriefingGameRule.class, "defaultEntityRules");
    defaultEntityRules.put("dummyEntityName001", "dummyDefaultValue001");
    defaultEntityRules.put("dummyEntityName002", "dummyDefaultValue002");
    defaultEntityRules.put("dummyEntityName003", "dummyDefaultValue003");

    BetterMobGriefingGameRule.addMobGriefingGameRules();

    // Verify that the world saved data has the expected values
    Assert.assertThat(
        "The size of the entity name to mob griefing value map does not match the expected size.",
        worldSavedData.entityNamesToMobGriefingValue.size(), CoreMatchers.is(4));
    Assert.assertThat(
        "An existing entry in the entity name to mob griefing value map was incorrectly modified.",
        worldSavedData.entityNamesToMobGriefingValue.get("existingEntityName001"),
        CoreMatchers.is("existingValue001"));
    Assert.assertThat(
        "An existing entry in the entity name to mob griefing value map was incorrectly modified.",
        worldSavedData.entityNamesToMobGriefingValue.get("dummyEntityName001"),
        CoreMatchers.is("existingValue002"));
    Assert.assertThat(
        "An entry in the entity name to mob griefing value map was not added as expected.",
        worldSavedData.entityNamesToMobGriefingValue.get("dummyEntityName002"),
        CoreMatchers.is("dummyDefaultValue002"));
    Assert.assertThat(
        "An entry in the entity name to mob griefing value map was not added as expected.",
        worldSavedData.entityNamesToMobGriefingValue.get("dummyEntityName003"),
        CoreMatchers.is("dummyDefaultValue003"));
  }

  /**
   * Test that the global rule default is used when the world is new
   */
  @Test
  public void testAddMobGriefingGameRules_newWorld_globalRuleDefaultApplied() {
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

    new MockUp<GameRules>() {
      @Mock(invocations = 1)
      void setOrCreateGameRule(String gameRule, String value) {
        Assert.assertThat("The game rule being set does not match the expected game rule", gameRule,
            CoreMatchers.is(BetterMobGriefingGameRule.ORIGINAL));
        Assert.assertThat("The game rule value being set does not match the expected value", value,
            CoreMatchers.is(BetterMobGriefingGameRule.FALSE));
      }
    };

    Deencapsulation.setField(world.getWorldInfo(), "totalTime", 0);
    Deencapsulation.setField(BetterMobGriefingGameRule.class, "defaultGlobalRule",
        BetterMobGriefingGameRule.FALSE);

    BetterMobGriefingGameRule.addMobGriefingGameRules();
  }

  /**
   * Test that the global rule default is not used when the world is already existing
   */
  @Test
  public void testAddMobGriefingGameRules_existingWorld_globalRuleDefaultNotApplied() {
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

    new MockUp<GameRules>() {
      @Mock(invocations = 0)
      void setOrCreateGameRule(String gameRule, String value) {
        Assert.assertThat("The game rule being set does not match the expected game rule", gameRule,
            CoreMatchers.is(BetterMobGriefingGameRule.ORIGINAL));
        Assert.assertThat("The game rule value being set does not match the expected value", value,
            CoreMatchers.is(BetterMobGriefingGameRule.FALSE));
      }
    };

    Deencapsulation.setField(world.getWorldInfo(), "totalTime", 1);
    Deencapsulation.setField(BetterMobGriefingGameRule.class, "defaultGlobalRule",
        BetterMobGriefingGameRule.FALSE);

    BetterMobGriefingGameRule.addMobGriefingGameRules();
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

  /**
   * Test that the default values are populated when there is a valid configuration.
   */
  @Test
  public void testPopulateDefaultMobGriefingRulesFromConfiguration_validConfiguration_defaultValuesPopulated(
      @Injectable DefaultMobGriefingConfiguration configuration) {
    // Set up test data.
    BetterMobGriefingGameRule.configuration = configuration;
    String defaultGlobalValue = "defaultGlobalValue";
    Map<String, String> defaultEntityValues = new HashMap<>();
    defaultEntityValues.put("entityName1", "entityValue1");
    defaultEntityValues.put("entityName2", "entityValue2");

    // Record expectations.
    new Expectations() {
      {
        configuration.getGlobalMobGriefingValue();
        result = defaultGlobalValue;

        configuration.getEntityMobGriefingValues();
        result = defaultEntityValues;
      }
    };

    // Call the method under test.
    BetterMobGriefingGameRule.populateDefaultMobGriefingRulesFromConfiguration();

    // Perform assertions.
    Assert.assertThat("The default global mobGriefing value was not populated as expected.",
        Deencapsulation.getField(BetterMobGriefingGameRule.class, "defaultGlobalRule"),
        CoreMatchers.is(defaultGlobalValue));
    Assert.assertThat("The default entity mobGriefing values were not populated as expected.",
        Deencapsulation.getField(BetterMobGriefingGameRule.class, "defaultEntityRules"),
        CoreMatchers.is(defaultEntityValues));
  }
}

