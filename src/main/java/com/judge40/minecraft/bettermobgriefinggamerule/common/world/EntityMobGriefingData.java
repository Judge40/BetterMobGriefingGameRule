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
import com.judge40.minecraft.bettermobgriefinggamerule.common.configuration.DefaultMobGriefingConfiguration;

import net.minecraft.entity.EntityList;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.util.ResourceLocation;
import net.minecraft.world.World;
import net.minecraft.world.storage.MapStorage;
import net.minecraft.world.storage.WorldSavedData;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

/**
 * A custom {@link WorldSavedData} which stores the entity specific {@link MobGriefingValue
 * MobGriefingValues} for a {@link World}.
 */
public class EntityMobGriefingData extends WorldSavedData {

  private Map<String, MobGriefingValue> entityNamesToMobGriefingValue = new HashMap<>();

  /**
   * {@link EntityMobGriefingData#forWorld(World)} should be used to construct an instance, instead
   * of this constructor.
   * 
   * @param modIdentifier The mod identifier to create the {@code EntityMobGriefingData} for.
   */
  public EntityMobGriefingData(String modIdentifier) {
    super(modIdentifier);
  }

  /**
   * Retrieve the {@link EntityMobGriefingData} for the given {@link World}. If the
   * {@code EntityMobGriefingData} does not already exist then a new instance is created and added
   * to the {@code World}'s {@link MapStorage}.
   * 
   * @param world The {@code World} to retrieve the {@code EntityMobGriefingData} for.
   * @return The {@code EntityMobGriefingData} for the {@code World}.
   */
  public static EntityMobGriefingData forWorld(World world) {
    // Get the world data instance from the World's MapStorage.
    MapStorage mapStorage = world.getMapStorage();
    EntityMobGriefingData entityMobGriefingData = (EntityMobGriefingData) mapStorage
        .getOrLoadData(EntityMobGriefingData.class, ModInfoConstants.ID);

    // If the MapStorage did not contain a world data instance then create a new one and store it in
    // the MapStorage.
    if (entityMobGriefingData == null) {
      entityMobGriefingData = new EntityMobGriefingData(ModInfoConstants.ID);
      mapStorage.setData(ModInfoConstants.ID, entityMobGriefingData);
    }

    return entityMobGriefingData;
  }

  /**
   * Populate the {@link EntityMobGriefingData} with the {@link DefaultMobGriefingConfiguration}'s
   * values. Only entities which are not already in the {@code EntityMobGriefingData} will be
   * populated.
   * 
   * @param configuration The {@code DefaultMobGriefingConfiguration} to get the values from.
   */
  public void populateFromConfiguration(DefaultMobGriefingConfiguration configuration) {
    Set<String> registeredEntityNames = getRegisteredEntityNames();
    Map<String, MobGriefingValue> configEntityNamesToMobGriefingValue =
        configuration.getEntityMobGriefingValues();

    // Set the MobGriefingValue for each entity in the configuration.
    for (Entry<String, MobGriefingValue> entry : configEntityNamesToMobGriefingValue.entrySet()) {
      String entityName = entry.getKey();

      // Only set the MobGriefingValue if the entity is not already in the world data.
      if (!registeredEntityNames.contains(entityName)) {
        setMobGriefingValue(entityName, entry.getValue());
      }
    }
  }

  @Override
  public void readFromNBT(NBTTagCompound nbtTagCompound) {
    // Add the entity name and MobGriefingValue from each NBTTagCompound entry to the world data, if
    // the entity name is invalid then record it.
    Map<String, MobGriefingValue> invalidNameToMobGriefingValue = new HashMap<>();

    for (Iterator<String> keys = nbtTagCompound.getKeySet().iterator(); keys.hasNext();) {
      String entityName = keys.next();
      ResourceLocation entityType = new ResourceLocation(entityName);
      String externalForm = nbtTagCompound.getString(entityName);
      MobGriefingValue mobGriefingValue = MobGriefingValue.toEnumeration(externalForm);

      if (EntityList.isRegistered(entityType)) {
        String entityPath = entityType.getResourcePath();
        entityNamesToMobGriefingValue.put(entityType.getResourcePath(), mobGriefingValue);

        if (!entityName.equals(entityPath)) {
          keys.remove();
        }
      } else {
        invalidNameToMobGriefingValue.put(entityName, mobGriefingValue);
      }
    }

    // If there are invalid names, check if the translation name is being used and convert it to the
    // id based entity name.
    if (!invalidNameToMobGriefingValue.isEmpty()) {
      // Remove any invalid names from the nbtTagCompound.
      for (String invalidName : invalidNameToMobGriefingValue.keySet()) {
        nbtTagCompound.removeTag(invalidName);
      }

      for (ResourceLocation entityType : EntityList.getEntityNameList()) {
        String translationName = EntityList.getTranslationName(entityType);
        MobGriefingValue mobGriefingValue = invalidNameToMobGriefingValue.remove(translationName);

        if (mobGriefingValue != null) {
          entityNamesToMobGriefingValue.put(entityType.getResourcePath(), mobGriefingValue);
        }

        if (invalidNameToMobGriefingValue.isEmpty()) {
          break;
        }
      }
    }
  }

  @Override
  public NBTTagCompound writeToNBT(NBTTagCompound nbtTagCompound) {
    // Add the entity name and MobGriefingValue from each world data entry to the NBTTagCompound.
    for (Entry<String, MobGriefingValue> entry : entityNamesToMobGriefingValue.entrySet()) {
      MobGriefingValue mobGriefingValue = entry.getValue();
      String externalForm = mobGriefingValue.toExternalForm();
      nbtTagCompound.setString(entry.getKey(), externalForm);
    }

    return nbtTagCompound;
  }

  /**
   * Get the {@link MobGriefingValue} for the given entity name.
   * 
   * @param entityName The name of the entity to get the {@code MobGriefingValue} of.
   * @return The {@code MobGriefingValue}.
   */
  public MobGriefingValue getMobGriefingValue(String entityName) {
    return entityNamesToMobGriefingValue.get(entityName);
  }

  /**
   * Set the {@link MobGriefingValue} for the given entity name.
   * 
   * @param entityName The name of the entity to set the {@code MobGriefingValue} of.
   * @param value The {@code MobGriefingValue} to set.
   */
  public void setMobGriefingValue(String entityName, MobGriefingValue value) {
    MobGriefingValue previousValue = entityNamesToMobGriefingValue.put(entityName, value);

    // If the value was changed then mark the data as dirty so it will be stored.
    if (!value.equals(previousValue)) {
      this.markDirty();
    }
  }

  /**
   * Get the names of all entities which have {@link MobGriefingValue} registered.
   * 
   * @return The registered entity names.
   */
  public Set<String> getRegisteredEntityNames() {
    return entityNamesToMobGriefingValue.keySet();
  }

  @Override
  public String toString() {
    StringBuilder stringBuilder = new StringBuilder();
    List<String> entityNames = new ArrayList<>(entityNamesToMobGriefingValue.keySet());
    Collections.sort(entityNames);

    for (Iterator<String> iterator = entityNames.iterator(); iterator.hasNext();) {
      String entityName = iterator.next();
      MobGriefingValue mobGriefingValue = entityNamesToMobGriefingValue.get(entityName);
      stringBuilder.append(String.format("%s = %s", entityName, mobGriefingValue.toExternalForm()));

      if (iterator.hasNext()) {
        stringBuilder.append(", ");
      }
    }

    return stringBuilder.toString();
  }
}
