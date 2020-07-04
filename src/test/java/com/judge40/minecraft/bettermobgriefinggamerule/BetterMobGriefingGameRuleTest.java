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

/**
 * The unit tests for {@link BetterMobGriefingGameRule}.
 */
public class BetterMobGriefingGameRuleTest {
//
//  private BetterMobGriefingGameRule betterMobGriefingGameRule;
//
//  /**
//   * Populate the {@code fml.deobfuscatedEnvironment} flag.
//   */
//  @BeforeClass
//  public static void setUpBeforeClass() {
//    // Set the deobfuscation flag.
//    Map<String, Object> blackboard = new HashMap<>();
//    blackboard.put("fml.deobfuscatedEnvironment", true);
//    Launch.blackboard = blackboard;
//  }
//
//  @Before
//  public void setUp() {
//    betterMobGriefingGameRule = new BetterMobGriefingGameRule();
//  }
//
//  /**
//   * Test that the entry point instance is retrieved from the Loader's mod list.
//   */
//  @Test
//  public void testGetInstance(@Mocked FMLModContainer modContainer, @Mocked Loader loader) {
//    // Set up test data.
//    BetterMobGriefingGameRule entryPoint = new BetterMobGriefingGameRule();
//
//    // Record expectations.
//    new Expectations() {
//      {
//        loader.getIndexedModList();
//        result = Collections.singletonMap(ModInfoConstants.ID, modContainer);
//
//        modContainer.getMod();
//        result = entryPoint;
//      }
//    };
//
//    // Call the method under test.
//    BetterMobGriefingGameRule instance = BetterMobGriefingGameRule.getInstance();
//
//    // Perform assertions.
//    Assert.assertThat("The expected instance was not retrieved from the Loader.", entryPoint,
//        CoreMatchers.sameInstance(instance));
//  }
//
//  /**
//   * Test that configuration is loaded when the FML pre-initialization event is fired.
//   */
//  @Test
//  public void testOnFmlPreInitializationEvent_configurationLoaded() {
//    // Set up test data.
//    FMLPreInitializationEvent event = new FMLPreInitializationEvent(null, null);
//    File configFile = new File("");
//
//    // Record expectations.
//    new Expectations(event, DefaultMobGriefingConfiguration.class) {
//      {
//        event.getSuggestedConfigurationFile();
//        result = configFile;
//
//        new DefaultMobGriefingConfiguration(configFile);
//      }
//    };
//
//    // Call the method under test.
//    betterMobGriefingGameRule.onFmlPreInitializationEvent(event);
//
//    // Perform assertions.
//    Assert.assertThat("The configuration did not match the expected value.",
//        betterMobGriefingGameRule.getDefaultMobGriefingConfiguration(),
//        CoreMatchers.isA(DefaultMobGriefingConfiguration.class));
//  }
//
//  /**
//   * Test that the event handlers are registered when the FML initialization event is fired.
//   */
//  @Test
//  public void testOnFmlInitializationEvent_eventHandlersRegistered(@Mocked EventBus eventBus,
//      @Mocked Loader loader) {
//    // Set up test data.
//    FMLInitializationEvent event = new FMLInitializationEvent();
//
//    // Record expectations.
//    new Expectations() {
//      {
//        eventBus.register(withInstanceOf(MobGriefingEventHandler.class));
//        times = 1;
//      }
//    };
//
//    // Call the method under test.
//    betterMobGriefingGameRule.onFmlInitializationEvent(event);
//  }
//
//  /**
//   * Test that the game rule hander is replaced, global rule is replaced and entity rules are
//   * populated when the FML server starting event is fired and the world is new.
//   */
//  @Test
//  public void testOnFmlServerStartingEvent_newWorld_originalsReplacedEntityRulesPopulated(
//      @Mocked CommandHandler commandHandler, @Mocked DefaultMobGriefingConfiguration configuration,
//      @Mocked EntityMobGriefingData entityData, @Mocked MinecraftServer server,
//      @Mocked World world) {
//    // Set up test data.
//    final BetterMobGriefingCommand newGameRuleHandler = new BetterMobGriefingCommand();
//    CommandGameRule originalGameRuleHandler = new CommandGameRule();
//
//    Map<String, ICommand> commandMap = new HashMap<>();
//    commandMap.put("commandName", originalGameRuleHandler);
//
//    Set<ICommand> commandSet = new HashSet<>();
//    commandSet.add(originalGameRuleHandler);
//
//    GameRules gameRules = new GameRules();
//
//    Deencapsulation.setField(betterMobGriefingGameRule, configuration);
//
//    FMLServerStartingEvent event = new FMLServerStartingEvent(server);
//
//    // Record expectations.
//    new Expectations(gameRules, BetterMobGriefingCommand.class, ReflectionHelper.class) {
//      {
//        newGameRuleHandler.getName();
//        result = "commandName";
//
//        commandHandler.getCommands();
//        result = commandMap;
//
//        ReflectionHelper.getPrivateValue(CommandHandler.class, commandHandler, "field_71561_b",
//            "commandSet");
//        result = commandSet;
//
//        world.getTotalWorldTime();
//        result = 0;
//
//        world.getGameRules();
//        result = gameRules;
//
//        configuration.getGlobalMobGriefingValue();
//        result = MobGriefingValue.FALSE;
//      }
//    };
//
//    // Call the method under test.
//    betterMobGriefingGameRule.onFmlServerStartingEvent(event);
//
//    // Perform assertions.
//    Assert.assertThat("The command set contained an unexpected game rule command.", commandSet,
//        CoreMatchers.not(CoreMatchers.hasItem(originalGameRuleHandler)));
//
//    // Verify expectations.
//    new Verifications() {
//      {
//        commandHandler.registerCommand(withInstanceOf(BetterMobGriefingCommand.class));
//
//        gameRules.setOrCreateGameRule(BetterMobGriefingGameRule.GLOBAL_RULE,
//            MobGriefingValue.FALSE.toExternalForm());
//
//        entityData.populateFromConfiguration((DefaultMobGriefingConfiguration) any);
//      }
//    };
//  }
//
//  /**
//   * Test that the game rule hander is replaced, global rule is replaced and entity rules are
//   * populated when the FML server starting event is fired and the world is not new.
//   */
//  @Test
//  public void testOnFmlServerStartingEvent_existingWorld_originalsReplacedEntityRulesPopulated(
//      @Mocked CommandHandler commandHandler, @Mocked EntityMobGriefingData entityData,
//      @Mocked MinecraftServer server, @Mocked World world) {
//    // Set up test data.
//    BetterMobGriefingCommand newGameRuleHandler = new BetterMobGriefingCommand();
//    CommandGameRule originalGameRuleHandler = new CommandGameRule();
//
//    Map<String, ICommand> commandMap = new HashMap<>();
//    commandMap.put("commandName", originalGameRuleHandler);
//
//    Set<ICommand> commandSet = new HashSet<>();
//    commandSet.add(originalGameRuleHandler);
//
//    FMLServerStartingEvent event = new FMLServerStartingEvent(server);
//
//    // Record expectations.
//    new Expectations(BetterMobGriefingCommand.class, ReflectionHelper.class) {
//      {
//        newGameRuleHandler.getName();
//        result = "commandName";
//
//        commandHandler.getCommands();
//        result = commandMap;
//
//        ReflectionHelper.getPrivateValue(CommandHandler.class, commandHandler, "field_71561_b",
//            "commandSet");
//        result = commandSet;
//
//        world.getTotalWorldTime();
//        result = 1;
//      }
//    };
//
//    // Call the method under test.
//    betterMobGriefingGameRule.onFmlServerStartingEvent(event);
//
//    // Perform assertions.
//    Assert.assertThat("The command set contained an unexpected game rule command.", commandSet,
//        CoreMatchers.not(CoreMatchers.hasItem(originalGameRuleHandler)));
//
//    // Verify expectations.
//    new Verifications() {
//      {
//        commandHandler.registerCommand(withInstanceOf(BetterMobGriefingCommand.class));
//
//        entityData.populateFromConfiguration((DefaultMobGriefingConfiguration) any);
//      }
//    };
//  }
//
//  /**
//   * Test that the global rule's value is returned when the entity's name can not be determined.
//   */
//  @Test
//  public void testIsMobGriefingEnabled_entityNameNotFound_globalValue(@Mocked Entity entity,
//      @Mocked GameRules gameRules, @Mocked World world) {
//    // Set up test data.
//    entity.world = world;
//
//    // Record expectations.
//    new Expectations(EntityList.class) {
//      {
//        EntityList.getKey(entity);
//        result = null;
//
//        gameRules.getBoolean(BetterMobGriefingGameRule.GLOBAL_RULE);
//        result = false;
//        times = 1;
//      }
//    };
//
//    // Call the method under test.
//    boolean isMobGriefingEnabled = BetterMobGriefingGameRule.isMobGriefingEnabled(entity);
//
//    // Perform assertions.
//    Assert.assertThat("The isMobGriefingEnabled flag did not match the expected value.",
//        isMobGriefingEnabled, CoreMatchers.is(false));
//  }
//
//  /**
//   * Test that the global rule's value is returned when the entity does not have its own mob
//   * griefing rule.
//   */
//  @Test
//  public void testIsMobGriefingEnabled_entityRuleNotFound_globalValue(@Mocked Entity entity,
//      @Mocked EntityMobGriefingData entityData, @Mocked GameRules gameRules, @Mocked World world) {
//    // Set up test data.
//    entity.world = world;
//
//    // Record expectations.
//    new Expectations(EntityList.class) {
//      {
//        EntityList.getKey(entity);
//        result = new ResourceLocation("entityName");
//
//        entityData.getMobGriefingValue("entityname");
//        result = null;
//
//        gameRules.getBoolean(BetterMobGriefingGameRule.GLOBAL_RULE);
//        result = false;
//        times = 1;
//      }
//    };
//
//    // Call the method under test.
//    boolean isMobGriefingEnabled = BetterMobGriefingGameRule.isMobGriefingEnabled(entity);
//
//    // Perform assertions.
//    Assert.assertThat("The isMobGriefingEnabled flag did not match the expected value.",
//        isMobGriefingEnabled, CoreMatchers.is(false));
//  }
//
//  /**
//   * Test that the global rule's value is returned when the entity's mob griefing value is INHERIT.
//   */
//  @Test
//  public void testIsMobGriefingEnabled_entityValueInherit_globalValue(@Mocked Entity entity,
//      @Mocked EntityMobGriefingData entityData, @Mocked GameRules gameRules, @Mocked World world) {
//    // Set up test data.
//    entity.world = world;
//
//    // Record expectations.
//    new Expectations(EntityList.class) {
//      {
//        EntityList.getKey(entity);
//        result = new ResourceLocation("entityName");
//
//        entityData.getMobGriefingValue("entityname");
//        result = MobGriefingValue.INHERIT;
//
//        gameRules.getBoolean(BetterMobGriefingGameRule.GLOBAL_RULE);
//        result = false;
//        times = 1;
//      }
//    };
//
//    // Call the method under test.
//    boolean isMobGriefingEnabled = BetterMobGriefingGameRule.isMobGriefingEnabled(entity);
//
//    // Perform assertions.
//    Assert.assertThat("The isMobGriefingEnabled flag did not match the expected value.",
//        isMobGriefingEnabled, CoreMatchers.is(false));
//  }
//
//  /**
//   * Test that true is returned when the entity's mob griefing value is TRUE.
//   */
//  @Test
//  public void testIsMobGriefingEnabled_entityValueTrue_true(@Mocked Entity entity,
//      @Mocked EntityMobGriefingData entityData, @Mocked GameRules gameRules, @Mocked World world) {
//    // Set up test data.
//    entity.world = world;
//
//    // Record expectations.
//    new Expectations(EntityList.class) {
//      {
//        EntityList.getKey(entity);
//        result = new ResourceLocation("entityName");
//
//        entityData.getMobGriefingValue("entityname");
//        result = MobGriefingValue.TRUE;
//      }
//    };
//
//    // Call the method under test.
//    boolean isMobGriefingEnabled = BetterMobGriefingGameRule.isMobGriefingEnabled(entity);
//
//    // Perform assertions.
//    Assert.assertThat("The isMobGriefingEnabled flag did not match the expected value.",
//        isMobGriefingEnabled, CoreMatchers.is(true));
//
//    // Verify expectations.
//    new Verifications() {
//      {
//        gameRules.getBoolean(BetterMobGriefingGameRule.GLOBAL_RULE);
//        times = 0;
//      }
//    };
//  }
//
//  /**
//   * Test that false is returned when the entity's mob griefing value is FALSE.
//   */
//  @Test
//  public void testIsMobGriefingEnabled_entityValueFalse_false(@Mocked Entity entity,
//      @Mocked EntityMobGriefingData entityData, @Mocked GameRules gameRules, @Mocked World world) {
//    // Set up test data.
//    entity.world = world;
//
//    // Record expectations.
//    new Expectations(EntityList.class) {
//      {
//        EntityList.getKey(entity);
//        result = new ResourceLocation("entityName");
//
//        entityData.getMobGriefingValue("entityname");
//        result = MobGriefingValue.FALSE;
//      }
//    };
//
//    // Call the method under test.
//    boolean isMobGriefingEnabled = BetterMobGriefingGameRule.isMobGriefingEnabled(entity);
//
//    // Perform assertions.
//    Assert.assertThat("The isMobGriefingEnabled flag did not match the expected value.",
//        isMobGriefingEnabled, CoreMatchers.is(false));
//
//    // Verify expectations.
//    new Verifications() {
//      {
//        gameRules.getBoolean(BetterMobGriefingGameRule.GLOBAL_RULE);
//        times = 0;
//      }
//    };
//  }
}
