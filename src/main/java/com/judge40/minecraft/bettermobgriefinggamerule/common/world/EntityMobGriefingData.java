/*
 * w * Better mobGriefing GameRule Copyright (c) 2016 Judge40
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

import com.judge40.minecraft.bettermobgriefinggamerule.common.MobGriefingValue;
import com.judge40.minecraft.bettermobgriefinggamerule.common.ModInfoConstants;
import com.judge40.minecraft.bettermobgriefinggamerule.common.config.Config;
import java.util.Comparator;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.stream.Collectors;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.server.MinecraftServer;
import net.minecraft.util.ResourceLocation;
import net.minecraft.world.dimension.DimensionType;
import net.minecraft.world.server.ServerWorld;
import net.minecraft.world.storage.DimensionSavedDataManager;
import net.minecraft.world.storage.WorldSavedData;
import net.minecraftforge.registries.ForgeRegistries;

/**
 * A custom {@link WorldSavedData} which stores the entity specific {@link MobGriefingValue
 * MobGriefingValues} for a {@link MinecraftServer}.
 */
public class EntityMobGriefingData extends WorldSavedData {

  private SortedMap<ResourceLocation, MobGriefingValue> entityIdsToMobGriefingValue = new TreeMap<>(
      Comparator.comparing(ResourceLocation::toString));

  private EntityMobGriefingData() {
    super(ModInfoConstants.ID);
  }

  /**
   * Retrieve the {@link EntityMobGriefingData} for the given {@link MinecraftServer}. If the {@code
   * EntityMobGriefingData} does not already exist then a new instance is created and added to the
   * {@code MinecraftServer}'s Overworld {@code MapStorage}.
   *
   * @param server The {@code MinecraftServer} to retrieve the {@code EntityMobGriefingData} for.
   * @return The {@code EntityMobGriefingData} for the {@code MinecraftServer}.
   */
  public static EntityMobGriefingData forServer(MinecraftServer server) {
    ServerWorld world = server.getWorld(DimensionType.OVERWORLD);
    DimensionSavedDataManager savedData = world.getSavedData();
    return savedData.getOrCreate(EntityMobGriefingData::new, ModInfoConstants.ID);
  }

  /**
   * Populate the {@link EntityMobGriefingData} with the {@link Config}'s values. Only entities
   * which are not already in the {@code EntityMobGriefingData} will be populated.
   */
  public void populateFromConfiguration() {
    Map<ResourceLocation, MobGriefingValue> configEntityIdsToMobGriefingValue = Config.entityIdsToDefaultEntityValue;

    // Set the MobGriefingValue for each entity in the configuration.
    for (Entry<ResourceLocation, MobGriefingValue> entry : configEntityIdsToMobGriefingValue
        .entrySet()) {
      ResourceLocation entityId = entry.getKey();

      // Only set the MobGriefingValue if the entity is not already in the world data.
      if (!entityIdsToMobGriefingValue.containsKey(entityId)) {
        setMobGriefingValue(entityId, entry.getValue());
      }
    }
  }

  @Override
  public void read(CompoundNBT nbt) {
    // Add the entity name and MobGriefingValue from each NBT entry to the world data.
    for (Iterator<String> iterator = nbt.keySet().iterator(); iterator.hasNext(); ) {
      String key = iterator.next();
      ResourceLocation entityId = new ResourceLocation(key);

      if (ForgeRegistries.ENTITIES.containsKey(entityId)) {
        String externalForm = nbt.getString(key);
        MobGriefingValue mobGriefingValue = MobGriefingValue.toEnumeration(externalForm);
        entityIdsToMobGriefingValue.put(entityId, mobGriefingValue);
      } else {
        // If the entity name is invalid then remove it.
        iterator.remove();
      }
    }
  }

  @Override
  public CompoundNBT write(CompoundNBT nbt) {
    // Add the entity name and MobGriefingValue from each world data entry to the NBT.
    entityIdsToMobGriefingValue.forEach((k, v) -> nbt.putString(k.toString(), v.toString()));
    return nbt;
  }

  /**
   * Get the {@link MobGriefingValue} for the given entity ID.
   *
   * @param entityId The id of the entity to get the {@code MobGriefingValue} of.
   * @return The {@code MobGriefingValue}, defaults to {@link MobGriefingValue#INHERIT}.
   */
  public MobGriefingValue getMobGriefingValue(ResourceLocation entityId) {
    return entityIdsToMobGriefingValue.getOrDefault(entityId, MobGriefingValue.INHERIT);
  }

  /**
   * Set the {@link MobGriefingValue} for the given entity name.
   *
   * @param entityId The id of the entity to set the {@code MobGriefingValue} of.
   * @param value    The {@code MobGriefingValue} to set.
   */
  public void setMobGriefingValue(ResourceLocation entityId, MobGriefingValue value) {
    MobGriefingValue previousValue = entityIdsToMobGriefingValue.put(entityId, value);

    // If the value was changed then mark the data as dirty so it will be stored.
    if (!value.equals(previousValue)) {
      this.markDirty();
    }
  }

  /**
   * Get the number of entities with values set.
   *
   * @return The number of entities.
   */
  public int size() {
    return entityIdsToMobGriefingValue.size();
  }

  @Override
  public String toString() {
    return entityIdsToMobGriefingValue.entrySet().stream()
        .map(entry -> String.format("%s = %s", entry.getKey(), entry.getValue()))
        .collect(Collectors.joining("\n"));
  }
}
