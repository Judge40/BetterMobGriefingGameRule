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

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.judge40.minecraft.bettermobgriefinggamerule.TestUtils;
import com.judge40.minecraft.bettermobgriefinggamerule.common.world.EntityMobGriefingData;
import java.io.File;
import net.minecraft.entity.Entity;
import net.minecraft.entity.EntityType;
import net.minecraft.server.MinecraftServer;
import net.minecraft.util.ResourceLocation;
import net.minecraft.world.GameRules;
import net.minecraft.world.dimension.DimensionType;
import net.minecraft.world.server.ServerWorld;
import net.minecraft.world.storage.DimensionSavedDataManager;
import net.minecraftforge.event.entity.EntityMobGriefingEvent;
import net.minecraftforge.eventbus.api.Event.Result;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/**
 * The unit tests for {@link MobGriefingEventHandler}.
 */
class MobGriefingEventHandlerTest {

  private EntityMobGriefingData data;
  private Entity entity;

  @BeforeAll
  static void setUpBeforeAll() throws IllegalAccessException {
    TestUtils.initializeTestEnvironment();
  }

  @BeforeEach
  void setUp() {
    MinecraftServer server = mock(MinecraftServer.class);
    ServerWorld world = mock(ServerWorld.class);
    DimensionSavedDataManager savedDataManager = new DimensionSavedDataManager(new File(""), null);

    when(server.getGameRules()).thenReturn(new GameRules());
    when(server.func_71218_a(DimensionType.OVERWORLD)).thenReturn(world);
    when(world.getSavedData()).thenReturn(savedDataManager);

    data = EntityMobGriefingData.forServer(server);

    entity = mock(Entity.class);
    when(entity.getServer()).thenReturn(server);

    EntityType entityType = mock(EntityType.class);
    when(entity.getType()).thenReturn(entityType);
    when(entityType.getRegistryName()).thenReturn(new ResourceLocation("test:entity1"));
  }

  @Test
  void shouldNotOverrideGriefingWhenEntityNull() {
    // Given.
    EntityMobGriefingEvent event = new EntityMobGriefingEvent(null);

    // When.
    MobGriefingEventHandler.onMobGriefing(event);

    // Then.
    assertThat("Unexpected result.", event.getResult(), is(Result.DEFAULT));
  }

  @ParameterizedTest(name = "Should override mobGriefing when entity value is {0}")
  @CsvSource({"FALSE, DENY", "TRUE, ALLOW", "INHERIT, ALLOW"})
  void shouldOverrideGriefingWhenEntityExists(MobGriefingValue value, Result result) {
    // Given.
    EntityMobGriefingEvent event = new EntityMobGriefingEvent(entity);

    ResourceLocation entityId = entity.getType().getRegistryName();
    data.setMobGriefingValue(entityId, value);

    // When.
    MobGriefingEventHandler.onMobGriefing(event);

    // Then.
    assertThat("Unexpected result.", event.getResult(), is(result));
  }

  @Test
  void shouldUseGlobalValueWhenEntityNotExists() {
    // Given.
    EntityMobGriefingEvent event = new EntityMobGriefingEvent(entity);

    // When.
    MobGriefingEventHandler.onMobGriefing(event);

    // Then.
    assertThat("Unexpected result.", event.getResult(), is(Result.ALLOW));
  }
}
