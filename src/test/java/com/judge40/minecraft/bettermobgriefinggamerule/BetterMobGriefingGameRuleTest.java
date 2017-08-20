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
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.hamcrest.CoreMatchers;
import org.junit.Assert;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import com.judge40.minecraft.bettermobgriefinggamerule.command.BetterMobGriefingCommand;
import com.judge40.minecraft.bettermobgriefinggamerule.common.config.DefaultMobGriefingConfiguration;
import com.judge40.minecraft.bettermobgriefinggamerule.world.EntityMobGriefingData;

import cpw.mods.fml.common.FMLModContainer;
import cpw.mods.fml.common.Loader;
import cpw.mods.fml.common.event.FMLInitializationEvent;
import cpw.mods.fml.common.event.FMLPreInitializationEvent;
import cpw.mods.fml.common.event.FMLServerStartingEvent;
import cpw.mods.fml.common.eventhandler.EventBus;
import cpw.mods.fml.relauncher.ReflectionHelper;
import mockit.Deencapsulation;
import mockit.Expectations;
import mockit.Mocked;
import mockit.Verifications;
import net.minecraft.command.CommandGameRule;
import net.minecraft.command.CommandHandler;
import net.minecraft.command.ICommand;
import net.minecraft.entity.Entity;
import net.minecraft.entity.EntityList;
import net.minecraft.launchwrapper.Launch;
import net.minecraft.server.MinecraftServer;
import net.minecraft.world.GameRules;
import net.minecraft.world.World;
import net.minecraftforge.common.MinecraftForge;

/**
 * The unit tests for {@link BetterMobGriefingGameRule}.
 */
public class BetterMobGriefingGameRuleTest {

  private BetterMobGriefingGameRule betterMobGriefingGameRule;

  @BeforeClass
  public static void setUpBeforeClass() {
    // Set the deobfuscation flag.
    Map<String, Object> blackboard = new HashMap<>();
    blackboard.put("fml.deobfuscatedEnvironment", true);
    Launch.blackboard = blackboard;
  }

  /**
   * @throws java.lang.Exception
   */
  @Before
  public void setUp() throws Exception {
    betterMobGriefingGameRule = new BetterMobGriefingGameRule();
  }

  /**
   * Test that the entry point instance is retrieved from the Loader's mod list.
   */
  @Test
  public void testGetInstance(@Mocked FMLModContainer modContainer, @Mocked Loader loader) {
    // Set up test data.
    BetterMobGriefingGameRule entryPoint = new BetterMobGriefingGameRule();

    // Record expectations.
    new Expectations() {
      {
        loader.getIndexedModList();
        result = Collections.singletonMap(ModInfoConstants.ID, modContainer);

        modContainer.getMod();
        result = entryPoint;
      }
    };

    // Call the method under test.
    BetterMobGriefingGameRule instance = BetterMobGriefingGameRule.getInstance();

    // Perform assertions.
    Assert.assertThat("The expected instance was not retrieved from the Loader.", entryPoint,
        CoreMatchers.sameInstance(instance));
  }


  /**
   * Test that configuration is loaded when the FML pre-initialization event is fired.
   */
  @Test
  public void testOnFMLPreInitializationEvent_configurationLoaded() {
    // Set up test data.
    FMLPreInitializationEvent event = new FMLPreInitializationEvent(null, null);
    File configFile = new File("");

    // Record expectations.
    new Expectations(event, DefaultMobGriefingConfiguration.class) {
      {
        event.getSuggestedConfigurationFile();
        result = configFile;

        new DefaultMobGriefingConfiguration(configFile);
      }
    };

    // Call the method under test.
    betterMobGriefingGameRule.onFMLPreInitializationEvent(event);

    // Perform assertions.
    Assert.assertThat("The configuration did not match the expected value.",
        betterMobGriefingGameRule.getDefaultMobGriefingConfiguration(),
        CoreMatchers.isA(DefaultMobGriefingConfiguration.class));
  }

  /**
   * Test that the event handlers are registered when the FML initialization event is fired.
   */
  @Test
  public void testOnFMLInitializationEvent_eventHandlersRegistered(@Mocked EventBus eventBus,
      @Mocked Loader loader) {
    // Set up test data.
    FMLInitializationEvent event = new FMLInitializationEvent();

    // Record expectations.
    new Expectations(MinecraftForge.class) {
      {
        eventBus.register(withInstanceOf(MobGriefingEventHandler.class));
        times = 2;
      }
    };

    // Call the method under test.
    betterMobGriefingGameRule.onFMLInitializationEvent(event);
  }

  /**
   * Test that the game rule hander is replaced, global rule is replaced and entity rules are
   * populated when the FML server starting event is fired and the world is new.
   */
  @Test
  public void testOnFMLServerStartingEvent_newWorld_gameRuleCommandReplacedGlobalRuleReplacedEntityRulesPopulated(
      @Mocked CommandHandler commandHandler, @Mocked DefaultMobGriefingConfiguration configuration,
      @Mocked EntityMobGriefingData entityData, @Mocked MinecraftServer server,
      @Mocked World world) {
    // Set up test data.
    FMLServerStartingEvent event = new FMLServerStartingEvent(server);
    BetterMobGriefingCommand newGameRuleHandler = new BetterMobGriefingCommand();
    CommandGameRule originalGameRuleHandler = new CommandGameRule();

    Map<String, ICommand> commandMap = new HashMap<>();
    commandMap.put("commandName", originalGameRuleHandler);

    Set<ICommand> commandSet = new HashSet<>();
    commandSet.add(originalGameRuleHandler);

    GameRules gameRules = new GameRules();

    Deencapsulation.setField(betterMobGriefingGameRule, configuration);

    // Record expectations.
    new Expectations(gameRules, BetterMobGriefingCommand.class, ReflectionHelper.class) {
      {
        newGameRuleHandler.getCommandName();
        result = "commandName";

        commandHandler.getCommands();
        result = commandMap;

        ReflectionHelper.getPrivateValue(CommandHandler.class, commandHandler,
            ObfuscationHelper.convertName("field_71561_b"));
        result = commandSet;

        world.getTotalWorldTime();
        result = 0;

        world.getGameRules();
        result = gameRules;

        configuration.getGlobalMobGriefingValue();
        result = MobGriefingValue.FALSE;
      }
    };

    // Call the method under test.
    betterMobGriefingGameRule.onFMLServerStartingEvent(event);

    // Perform assertions.
    Assert.assertThat("The command set contained an unexpected game rule command.", commandSet,
        CoreMatchers.not(CoreMatchers.hasItem(originalGameRuleHandler)));

    // Verify expectations.
    new Verifications() {
      {
        commandHandler.registerCommand(withInstanceOf(BetterMobGriefingCommand.class));

        gameRules.setOrCreateGameRule(BetterMobGriefingGameRule.ORIGINAL,
            MobGriefingValue.FALSE.toExternalForm());

        entityData.populateFromConfiguration((DefaultMobGriefingConfiguration) any, false);
      }
    };
  }

  /**
   * Test that the game rule hander is replaced, global rule is replaced and entity rules are
   * populated when the FML server starting event is fired and the world is not new.
   */
  @Test
  public void testOnFMLServerStartingEvent_existingWorld_gameRuleCommandReplacedGlobalRuleReplacedEntityRulesPopulated(
      @Mocked CommandHandler commandHandler, @Mocked EntityMobGriefingData entityData,
      @Mocked MinecraftServer server, @Mocked World world) {
    // Set up test data.
    FMLServerStartingEvent event = new FMLServerStartingEvent(server);
    BetterMobGriefingCommand newGameRuleHandler = new BetterMobGriefingCommand();
    CommandGameRule originalGameRuleHandler = new CommandGameRule();

    Map<String, ICommand> commandMap = new HashMap<>();
    commandMap.put("commandName", originalGameRuleHandler);

    Set<ICommand> commandSet = new HashSet<>();
    commandSet.add(originalGameRuleHandler);

    // Record expectations.
    new Expectations(BetterMobGriefingCommand.class, ReflectionHelper.class) {
      {
        newGameRuleHandler.getCommandName();
        result = "commandName";

        commandHandler.getCommands();
        result = commandMap;

        ReflectionHelper.getPrivateValue(CommandHandler.class, commandHandler,
            ObfuscationHelper.convertName("field_71561_b"));
        result = commandSet;

        world.getTotalWorldTime();
        result = 1;
      }
    };

    // Call the method under test.
    betterMobGriefingGameRule.onFMLServerStartingEvent(event);

    // Perform assertions.
    Assert.assertThat("The command set contained an unexpected game rule command.", commandSet,
        CoreMatchers.not(CoreMatchers.hasItem(originalGameRuleHandler)));

    // Verify expectations.
    new Verifications() {
      {
        commandHandler.registerCommand(withInstanceOf(BetterMobGriefingCommand.class));

        entityData.populateFromConfiguration((DefaultMobGriefingConfiguration) any, false);
      }
    };
  }

  /**
   * Test that the global rule's value is returned when the entity's name can not be determined.
   */
  @Test
  public void testIsMobGriefingEnabled_entityNameNotFound_globalValue(@Mocked Entity entity,
      @Mocked GameRules gameRules, @Mocked World world) {
    // Set up test data.
    entity.worldObj = world;

    // Record expectations.
    new Expectations(EntityList.class) {
      {
        EntityList.getEntityString(entity);
        result = null;

        gameRules.getGameRuleBooleanValue(BetterMobGriefingGameRule.ORIGINAL);
        result = false;
        times = 1;
      }
    };

    // Call the method under test.
    boolean isMobGriefingEnabled = BetterMobGriefingGameRule.isMobGriefingEnabled(entity);

    // Perform assertions.
    Assert.assertThat("The isMobGriefingEnabled flag did not match the expected value.",
        isMobGriefingEnabled, CoreMatchers.is(false));
  }

  /**
   * Test that the global rule's value is returned when the entity does not have its own mob
   * griefing rule.
   */
  @Test
  public void testIsMobGriefingEnabled_entityRuleNotFound_globalValue(@Mocked Entity entity,
      @Mocked EntityMobGriefingData entityData, @Mocked GameRules gameRules, @Mocked World world) {
    // Set up test data.
    entity.worldObj = world;

    // Record expectations.
    new Expectations(EntityList.class) {
      {
        EntityList.getEntityString(entity);
        result = "entityName";

        entityData.getMobGriefingValue("entityName");
        result = null;

        gameRules.getGameRuleBooleanValue(BetterMobGriefingGameRule.ORIGINAL);
        result = false;
        times = 1;
      }
    };

    // Call the method under test.
    boolean isMobGriefingEnabled = BetterMobGriefingGameRule.isMobGriefingEnabled(entity);

    // Perform assertions.
    Assert.assertThat("The isMobGriefingEnabled flag did not match the expected value.",
        isMobGriefingEnabled, CoreMatchers.is(false));
  }

  /**
   * Test that the global rule's value is returned when the entity's mob griefing value is INHERIT.
   */
  @Test
  public void testIsMobGriefingEnabled_entityValueInherit_globalValue(@Mocked Entity entity,
      @Mocked EntityMobGriefingData entityData, @Mocked GameRules gameRules, @Mocked World world) {
    // Set up test data.
    entity.worldObj = world;

    // Record expectations.
    new Expectations(EntityList.class) {
      {
        EntityList.getEntityString(entity);
        result = "entityName";

        entityData.getMobGriefingValue("entityName");
        result = MobGriefingValue.INHERIT;

        gameRules.getGameRuleBooleanValue(BetterMobGriefingGameRule.ORIGINAL);
        result = false;
        times = 1;
      }
    };

    // Call the method under test.
    boolean isMobGriefingEnabled = BetterMobGriefingGameRule.isMobGriefingEnabled(entity);

    // Perform assertions.
    Assert.assertThat("The isMobGriefingEnabled flag did not match the expected value.",
        isMobGriefingEnabled, CoreMatchers.is(false));
  }

  /**
   * Test that true is returned when the entity's mob griefing value is TRUE.
   */
  @Test
  public void testIsMobGriefingEnabled_entityValueTrue_true(@Mocked Entity entity,
      @Mocked EntityMobGriefingData entityData, @Mocked GameRules gameRules, @Mocked World world) {
    // Set up test data.
    entity.worldObj = world;

    // Record expectations.
    new Expectations(EntityList.class) {
      {
        EntityList.getEntityString(entity);
        result = "entityName";

        entityData.getMobGriefingValue("entityName");
        result = MobGriefingValue.TRUE;
      }
    };

    // Call the method under test.
    boolean isMobGriefingEnabled = BetterMobGriefingGameRule.isMobGriefingEnabled(entity);

    // Perform assertions.
    Assert.assertThat("The isMobGriefingEnabled flag did not match the expected value.",
        isMobGriefingEnabled, CoreMatchers.is(true));

    // Verify expectations.
    new Verifications() {
      {
        gameRules.getGameRuleBooleanValue(BetterMobGriefingGameRule.ORIGINAL);
        times = 0;
      }
    };
  }

  /**
   * Test that false is returned when the entity's mob griefing value is FALSE.
   */
  @Test
  public void testIsMobGriefingEnabled_entityValueFalse_false(@Mocked Entity entity,
      @Mocked EntityMobGriefingData entityData, @Mocked GameRules gameRules, @Mocked World world) {
    // Set up test data.
    entity.worldObj = world;

    // Record expectations.
    new Expectations(EntityList.class) {
      {
        EntityList.getEntityString(entity);
        result = "entityName";

        entityData.getMobGriefingValue("entityName");
        result = MobGriefingValue.FALSE;
      }
    };

    // Call the method under test.
    boolean isMobGriefingEnabled = BetterMobGriefingGameRule.isMobGriefingEnabled(entity);

    // Perform assertions.
    Assert.assertThat("The isMobGriefingEnabled flag did not match the expected value.",
        isMobGriefingEnabled, CoreMatchers.is(false));

    // Verify expectations.
    new Verifications() {
      {
        gameRules.getGameRuleBooleanValue(BetterMobGriefingGameRule.ORIGINAL);
        times = 0;
      }
    };
  }
}
