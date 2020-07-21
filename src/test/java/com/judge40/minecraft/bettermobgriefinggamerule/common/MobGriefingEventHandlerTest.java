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

import mockit.Mocked;
import net.minecraft.world.World;
import org.junit.Before;

/**
 * The unit tests for {@link MobGriefingEventHandler}.
 */
public class MobGriefingEventHandlerTest {

  private MobGriefingEventHandler eventHandler;

  @Mocked
  private World world;

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

  @Before
  public void setUp() {
    eventHandler = new MobGriefingEventHandler();
  }

  /**
   * Test that the configuration change event is handled when the configuration's mod ID matches
   * this mod.
   */
//  @Test
//  public void testOnConfigChanged_defaultMobGriefingConfigurationChanged_handleChange(
//      @Mocked DefaultMobGriefingConfiguration configuration) {
//    // Set up test data.
//    OnConfigChangedEvent event = new OnConfigChangedEvent(ModInfoConstants.ID, "", false, false);
//    BetterMobGriefingGameRule entryPoint = new BetterMobGriefingGameRule();
//
//    // Record expectations.
//    new Expectations(entryPoint) {
//      {
//        BetterMobGriefingGameRule.getInstance();
//        result = entryPoint;
//
//        entryPoint.getDefaultMobGriefingConfiguration();
//        result = configuration;
//      }
//    };
//
//    // Call the method under test.
//    eventHandler.onConfigChanged(event);
//
//    // Verify expectations.
//    new Verifications() {
//      {
//        configuration.synchronize();
//      }
//    };
//  }
//
//  /**
//   * Test that the configuration change event is not handled when the configuration's mod ID does
//   * not match this mod.
//   */
//  @Test
//  public void testOnConfigChanged_nonDefaultMobGriefingConfigurationChanged_doNotHandleChange(
//      @Mocked DefaultMobGriefingConfiguration configuration) {
//    // Set up test data.
//    OnConfigChangedEvent event = new OnConfigChangedEvent("dummyModId001", "", false, false);
//
//    // Call the method under test.
//    eventHandler.onConfigChanged(event);
//
//    // Verify expectations.
//    new Verifications() {
//      {
//        configuration.synchronize();
//        times = 0;
//      }
//    };
//  }
//
//  /**
//   * Test that the event result is set to DEFAULT when the entity is null.
//   */
//  @Test
//  public void testOnMobGriefing_entityNull_default() {
//    // Set up test data.
//    EntityMobGriefingEvent event = new EntityMobGriefingEvent(null);
//
//    // Call the method under test.
//    eventHandler.onMobGriefing(event);
//
//    // Perform assertions.
//    Assert.assertThat("The event result is not match the expected value.", event.getResult(),
//        CoreMatchers.is(Result.DEFAULT));
//  }
//
//  /**
//   * Test that the event result is set to DENY when mob griefing is disabled.
//   */
//  @Test
//  public void testOnMobGriefing_disabled_deny(@Mocked Entity entity) {
//    // Set up test data.
//    EntityMobGriefingEvent event = new EntityMobGriefingEvent(entity);
//
//    // Record expectations.
//    new Expectations(BetterMobGriefingGameRule.class) {
//      {
//        BetterMobGriefingGameRule.isMobGriefingEnabled(entity);
//        result = false;
//      }
//    };
//
//    // Call the method under test.
//    eventHandler.onMobGriefing(event);
//
//    // Perform assertions.
//    Assert.assertThat("The event result is not match the expected value.", event.getResult(),
//        CoreMatchers.is(Result.DENY));
//  }
//
//  /**
//   * Test that the event result is set to ALLOW when mob griefing is enabled.
//   */
//  @Test
//  public void testOnMobGriefing_enabled_allow(@Mocked Entity entity) {
//    // Set up test data.
//    EntityMobGriefingEvent event = new EntityMobGriefingEvent(entity);
//
//    // Record expectations.
//    new Expectations(BetterMobGriefingGameRule.class) {
//      {
//        BetterMobGriefingGameRule.isMobGriefingEnabled(entity);
//        result = true;
//      }
//    };
//
//    // Call the method under test.
//    eventHandler.onMobGriefing(event);
//
//    // Perform assertions.
//    Assert.assertThat("The event result is not match the expected value.", event.getResult(),
//        CoreMatchers.is(Result.ALLOW));
//  }
}
