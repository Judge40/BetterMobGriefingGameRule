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

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.judge40.minecraft.bettermobgriefinggamerule.common.MobGriefingValue;
import com.judge40.minecraft.bettermobgriefinggamerule.common.config.Config;
import com.judge40.minecraft.bettermobgriefinggamerule.common.config.ConfigHolder;
import com.judge40.minecraft.bettermobgriefinggamerule.common.world.EntityMobGriefingData;
import com.mojang.datafixers.DataFixer;
import java.io.File;
import net.minecraft.command.Commands;
import net.minecraft.command.Commands.EnvironmentType;
import net.minecraft.server.MinecraftServer;
import net.minecraft.util.ResourceLocation;
import net.minecraft.world.GameRules;
import net.minecraft.world.GameRules.BooleanValue;
import net.minecraft.world.server.ServerWorld;
import net.minecraft.world.storage.DimensionSavedDataManager;
import net.minecraftforge.fml.ExtensionPoint;
import net.minecraftforge.fml.ModLoadingContext;
import net.minecraftforge.fml.config.ModConfig.Type;
import net.minecraftforge.fml.event.server.FMLServerStartingEvent;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.MockedStatic;

/**
 * The unit tests for {@link BetterMobGriefingGameRule}.
 */
class BetterMobGriefingGameRuleTest {

  private MinecraftServer server;
  private ServerWorld world;
  private GameRules gameRules;

  @BeforeAll
  static void setUpBeforeAll() throws IllegalAccessException {
    TestUtils.initializeTestEnvironment();
  }

  @BeforeEach
  void setUp() {
    gameRules = new GameRules();

    server = mock(MinecraftServer.class);
    when(server.getCommands()).thenReturn(new Commands(EnvironmentType.ALL));

    world = mock(ServerWorld.class);
    when(server.overworld()).thenReturn(world);
    when(world.getGameTime()).thenReturn(1L);
    when(world.getGameRules()).thenReturn(gameRules);
    DataFixer dataFixer = mock(DataFixer.class);
    when(world.getDataStorage()).thenReturn(new DimensionSavedDataManager(new File(""), dataFixer));
  }

  @Test
  void shouldRegisterConfig() {
    // Given.
    try (MockedStatic<ModLoadingContext> contextMock = mockStatic(ModLoadingContext.class)) {
      ModLoadingContext modLoadingContext = mock(ModLoadingContext.class);
      when(ModLoadingContext.get()).thenReturn(modLoadingContext);

      // When.
      new BetterMobGriefingGameRule();

      // Then.
      verify(modLoadingContext).registerConfig(Type.COMMON, ConfigHolder.COMMON_SPEC);
      verify(modLoadingContext).registerExtensionPoint(eq(ExtensionPoint.CONFIGGUIFACTORY), any());
    }
  }

  @ParameterizedTest(name = "Should override the global value when a new world is created and the"
      + " config value is {0}")
  @ValueSource(booleans = {true, false})
  void shouldOverrideGlobalValueWhenNewWorldCreation(boolean input) {
    // Given.
    Config.defaultGlobalValue = input;
    FMLServerStartingEvent event = new FMLServerStartingEvent(server);

    when(world.getGameTime()).thenReturn(0L);

    // When.
    BetterMobGriefingGameRule.onFmlServerStartingEvent(event);

    // Then.
    boolean mobGriefing = gameRules.getBoolean(GameRules.RULE_MOBGRIEFING);
    assertThat("Unexpected mobGriefing value.", mobGriefing, is(input));
  }

  @ParameterizedTest(name = "Should not override the global value when an existing world is loaded"
      + " and the config value is {0}")
  @ValueSource(booleans = {true, false})
  void shouldNotOverrideGlobalWhenExistingWorld(boolean input) {
    // Given.
    Config.defaultGlobalValue = input;
    FMLServerStartingEvent event = new FMLServerStartingEvent(server);

    BooleanValue booleanValue = gameRules.getRule(GameRules.RULE_MOBGRIEFING);
    booleanValue.set(false, server);

    // When.
    BetterMobGriefingGameRule.onFmlServerStartingEvent(event);

    // Then.
    boolean mobGriefing = gameRules.getBoolean(GameRules.RULE_MOBGRIEFING);
    assertThat("Unexpected mobGriefing value.", mobGriefing, is(false));
  }

  @ParameterizedTest(name =
      "Should set the entity values when the server is starting and the config"
          + " value is {0}")
  @EnumSource(MobGriefingValue.class)
  void shouldSetEntityValues(MobGriefingValue input) {
    // Given.
    ResourceLocation entityId = new ResourceLocation("test:entity1");
    Config.entityIdsToDefaultEntityValue.put(entityId, input);
    FMLServerStartingEvent event = new FMLServerStartingEvent(server);

    // When.
    BetterMobGriefingGameRule.onFmlServerStartingEvent(event);

    // Then.
    EntityMobGriefingData data = EntityMobGriefingData.forServer(server);
    MobGriefingValue mobGriefing = data.getMobGriefingValue(entityId);
    assertThat("Unexpected mobGriefing value.", mobGriefing, is(input));
  }
}
