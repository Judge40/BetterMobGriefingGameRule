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

package com.judge40.minecraft.bettermobgriefinggamerule.common.world;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.judge40.minecraft.bettermobgriefinggamerule.TestUtils;
import com.judge40.minecraft.bettermobgriefinggamerule.common.MobGriefingValue;
import com.mojang.datafixers.DataFixer;
import java.io.File;
import java.util.Collections;
import net.minecraft.entity.EntityType;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.server.MinecraftServer;
import net.minecraft.util.ResourceLocation;
import net.minecraft.world.dimension.DimensionType;
import net.minecraft.world.server.ServerWorld;
import net.minecraft.world.storage.DimensionSavedDataManager;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/**
 * The unit tests for {@link EntityMobGriefingData}.
 */
class EntityMobGriefingDataTest {

  private EntityMobGriefingData data;

  @BeforeEach
  void setUp() {
    MinecraftServer server = mock(MinecraftServer.class);
    ServerWorld world = mock(ServerWorld.class);
    DataFixer dataFixer = mock(DataFixer.class);
    DimensionSavedDataManager savedDataManager = new DimensionSavedDataManager(new File(""),
        dataFixer);

    when(server.func_71218_a(DimensionType.OVERWORLD)).thenReturn(world);
    when(world.getSavedData()).thenReturn(savedDataManager);

    data = EntityMobGriefingData.forServer(server);
  }

  @Test
  void shouldPopulateAllEntriesFromConfigurationWhenNewData() throws IllegalAccessException {
    // Given.
    TestUtils.initializeConfig(true, MobGriefingValue.FALSE, MobGriefingValue.TRUE,
        MobGriefingValue.INHERIT);

    // When.
    data.populateFromConfiguration();

    // Then.
    assertThat("Unexpected number of mobGriefing values.", data.size(), is(3));

    MobGriefingValue mobGriefingValue = data
        .getMobGriefingValue(new ResourceLocation("test:entity1"));
    assertThat("Unexpected mobGriefing value.", mobGriefingValue, is(MobGriefingValue.FALSE));

    mobGriefingValue = data.getMobGriefingValue(new ResourceLocation("test:entity2"));
    assertThat("Unexpected mobGriefing value.", mobGriefingValue, is(MobGriefingValue.TRUE));

    mobGriefingValue = data.getMobGriefingValue(new ResourceLocation("test:entity3"));
    assertThat("Unexpected mobGriefing value.", mobGriefingValue, is(MobGriefingValue.INHERIT));
  }

  @Test
  void shouldPopulateNewEntriesFromConfigurationWhenExistingData() throws IllegalAccessException {
    // Given.
    TestUtils.initializeConfig(true, MobGriefingValue.FALSE, MobGriefingValue.TRUE,
        MobGriefingValue.INHERIT);
    ResourceLocation entityId2 = new ResourceLocation("test:entity2");
    data.setMobGriefingValue(entityId2, MobGriefingValue.FALSE);

    // When.
    data.populateFromConfiguration();

    // Then.
    assertThat("Unexpected number of mobGriefing values.", data.size(), is(3));

    MobGriefingValue mobGriefingValue = data
        .getMobGriefingValue(new ResourceLocation("test:entity1"));
    assertThat("Unexpected mobGriefing value.", mobGriefingValue, is(MobGriefingValue.FALSE));

    mobGriefingValue = data.getMobGriefingValue(entityId2);
    assertThat("Unexpected mobGriefing value.", mobGriefingValue, is(MobGriefingValue.FALSE));

    mobGriefingValue = data.getMobGriefingValue(new ResourceLocation("test:entity3"));
    assertThat("Unexpected mobGriefing value.", mobGriefingValue, is(MobGriefingValue.INHERIT));
  }

  @Test
  void shouldReadFromNbt() {
    // Given.
    CompoundNBT nbt = new CompoundNBT();

    ResourceLocation invalidEntityId = new ResourceLocation("invalid-entity");
    nbt.putString(invalidEntityId.toString(), "inherit");

    ResourceLocation validEntityId = EntityType.CREEPER.getRegistryName();
    nbt.putString(validEntityId.toString(), "true");

    // When.
    data.read(nbt);

    // Then.
    assertThat("Unexpected number of mobGriefing values.", data.size(), is(1));

    MobGriefingValue mobGriefingValue = data.getMobGriefingValue(invalidEntityId);
    assertThat("Unexpected mobGriefing value.", mobGriefingValue, is(MobGriefingValue.INHERIT));

    mobGriefingValue = data.getMobGriefingValue(validEntityId);
    assertThat("Unexpected mobGriefing value.", mobGriefingValue, is(MobGriefingValue.TRUE));
  }

  @Test
  void shouldWriteToNbtWhenEntityDataPopulated() {
    // Given.
    ResourceLocation entityResource1 = new ResourceLocation("namespace1:path3");
    data.setMobGriefingValue(entityResource1, MobGriefingValue.TRUE);

    ResourceLocation entityResource2 = new ResourceLocation("path2");
    data.setMobGriefingValue(entityResource2, MobGriefingValue.FALSE);

    ResourceLocation entityResource3 = new ResourceLocation("namespace1:path1");
    data.setMobGriefingValue(entityResource3, MobGriefingValue.INHERIT);

    CompoundNBT nbt = new CompoundNBT();

    // When.
    data.write(nbt);

    // Then.
    assertThat("Unexpected NBT value.", nbt.getString("minecraft:path2"), is("false"));
    assertThat("Unexpected NBT value.", nbt.getString("namespace1:path1"), is("inherit"));
    assertThat("Unexpected NBT value.", nbt.getString("namespace1:path3"), is("true"));
  }

  @Test
  void shouldNotWriteToNbtWhenEntityDataNotPopulated() {
    // Given.
    CompoundNBT nbt = new CompoundNBT();

    // When.
    data.write(nbt);

    // Then.
    assertThat("Unexpected NBT.", nbt.keySet(), is(Collections.emptySet()));
  }

  @Test
  void shouldProduceEmptyStringWhenDataNotPopulated() {
    // When.
    String stringRepresentation = data.toString();

    // Then.
    assertThat("Unexpected string representation", stringRepresentation, is(""));
  }

  @Test
  void shouldProduceReadableStringWhenDataPopulated() {
    // Given.
    ResourceLocation entityResource1 = new ResourceLocation("namespace1:path3");
    data.setMobGriefingValue(entityResource1, MobGriefingValue.TRUE);

    ResourceLocation entityResource2 = new ResourceLocation("path2");
    data.setMobGriefingValue(entityResource2, MobGriefingValue.FALSE);

    ResourceLocation entityResource3 = new ResourceLocation("namespace1:path1");
    data.setMobGriefingValue(entityResource3, MobGriefingValue.INHERIT);

    // When.
    String stringRepresentation = data.toString();

    // Then.
    String expectedString = "minecraft:path2 = false\nnamespace1:path1 = inherit\n"
        + "namespace1:path3 = true";
    assertThat("Unexpected string representation.", stringRepresentation, is(expectedString));
  }
}
