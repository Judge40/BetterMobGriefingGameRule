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
package com.judge40.minecraft.bettermobgriefinggamerule.world;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import com.judge40.minecraft.bettermobgriefinggamerule.BetterMobGriefingGameRule;

import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.world.World;
import net.minecraft.world.WorldSavedData;
import net.minecraft.world.storage.MapStorage;

/**
 * World saved data to store data on a per world basis
 */
public class BetterMobGriefingGameRuleWorldSavedData extends WorldSavedData {

  public Map<String, String> entityNamesToMobGriefingValue = new HashMap<>();

  /**
   * @param identifier
   */
  public BetterMobGriefingGameRuleWorldSavedData(String identifier) {
    super(identifier);
  }

  /*
   * (non-Javadoc)
   * 
   * @see net.minecraft.world.WorldSavedData#readFromNBT(net.minecraft.nbt.NBTTagCompound)
   */
  @Override
  public void readFromNBT(NBTTagCompound nbtTagCompound) {
    Set<?> keys = nbtTagCompound.func_150296_c();

    for (Object key : keys) {
      String value = nbtTagCompound.getString((String) key);
      entityNamesToMobGriefingValue.put((String) key, value);
    }
  }

  /*
   * (non-Javadoc)
   * 
   * @see net.minecraft.world.WorldSavedData#writeToNBT(net.minecraft.nbt.NBTTagCompound)
   */
  @Override
  public void writeToNBT(NBTTagCompound nbtTagCompound) {
    for (Entry<String, String> entityNameToMobGriefingValue : entityNamesToMobGriefingValue
        .entrySet()) {
      nbtTagCompound.setString(entityNameToMobGriefingValue.getKey(),
          entityNameToMobGriefingValue.getValue());
    }
  }

  /**
   * Get the {@link BetterMobGriefingGameRuleWorldSavedData} for a given {@link World}. If world
   * saved data does not already exist then a new instance is created and added to the map storage.
   * 
   * @param world The world to get the saved data for
   * @return The world saved data
   */
  public static BetterMobGriefingGameRuleWorldSavedData forWorld(World world) {
    MapStorage mapStorage = world.mapStorage;
    BetterMobGriefingGameRuleWorldSavedData betterMobGriefingGameRuleWorldSavedData =
        (BetterMobGriefingGameRuleWorldSavedData) mapStorage.loadData(
            BetterMobGriefingGameRuleWorldSavedData.class, BetterMobGriefingGameRule.MODID);

    if (betterMobGriefingGameRuleWorldSavedData == null) {
      betterMobGriefingGameRuleWorldSavedData =
          new BetterMobGriefingGameRuleWorldSavedData(BetterMobGriefingGameRule.MODID);
      mapStorage.setData(BetterMobGriefingGameRule.MODID, betterMobGriefingGameRuleWorldSavedData);
    }

    return betterMobGriefingGameRuleWorldSavedData;
  }

  /*
   * (non-Javadoc)
   * 
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString() {
    StringBuilder stringBuilder = new StringBuilder();
    List<String> entityNames = new ArrayList<>(entityNamesToMobGriefingValue.keySet());
    Collections.sort(entityNames);
    Iterator<String> entityNameIterator = entityNames.iterator();

    while (entityNameIterator.hasNext()) {
      String entityName = entityNameIterator.next();
      String mobGriefingValue = entityNamesToMobGriefingValue.get(entityName);
      stringBuilder.append(String.format("%s %s = %s", BetterMobGriefingGameRule.ORIGINAL,
          entityName, mobGriefingValue));

      if (entityNameIterator.hasNext()) {
        stringBuilder.append(", ");
      }
    }

    return stringBuilder.toString();
  }
}
