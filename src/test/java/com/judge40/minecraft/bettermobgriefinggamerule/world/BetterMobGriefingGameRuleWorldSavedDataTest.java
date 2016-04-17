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

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import org.hamcrest.CoreMatchers;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.judge40.minecraft.bettermobgriefinggamerule.BetterMobGriefingGameRule;

import mockit.Deencapsulation;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.world.World;
import net.minecraft.world.WorldSavedData;
import net.minecraft.world.storage.MapStorage;

/**
 *
 */
public class BetterMobGriefingGameRuleWorldSavedDataTest {

  private BetterMobGriefingGameRuleWorldSavedData worldSavedData;

  private World world;

  /**
   * @throws java.lang.Exception
   */
  @Before
  public void setUp() throws Exception {
    worldSavedData = new BetterMobGriefingGameRuleWorldSavedData(BetterMobGriefingGameRule.MODID);

    world = Deencapsulation.newUninitializedInstance(World.class);
    MapStorage mapStorage = new MapStorage(null);
    world.mapStorage = mapStorage;
  }

  /**
   * @throws java.lang.Exception
   */
  @After
  public void tearDown() throws Exception {
    worldSavedData = null;
    world = null;
  }

  /**
   * Test that NBT data is correctly populated in to the entityNamesToMobGriefingValue map
   */
  @Test
  public void testReadFromNBT_dataExists_mapPopulated() {
    Map<String, String> expectedMap = new HashMap<>();
    expectedMap.put("dummyKey1", "dummyValue1");
    expectedMap.put("dummyKey2", "dummyValue2");

    NBTTagCompound nbtTagCompound = new NBTTagCompound();

    for (Entry<String, String> entry : expectedMap.entrySet()) {
      nbtTagCompound.setString(entry.getKey(), entry.getValue());
    }

    worldSavedData.readFromNBT(nbtTagCompound);
    Assert.assertThat(
        "The NBT data was correctly read and populated in to the entityNamesToMobGriefingValue map.",
        worldSavedData.entityNamesToMobGriefingValue, CoreMatchers.is(expectedMap));
  }

  /**
   * Test that entityNamesToMobGriefingValue map data is correctly populated in to the NBT data
   */
  @Test
  public void testWriteToNBT_dataExists_nbtPopulated() {
    worldSavedData.entityNamesToMobGriefingValue.put("dummyKey1", "dummyValue1");
    worldSavedData.entityNamesToMobGriefingValue.put("dummyKey2", "dummyValue2");

    NBTTagCompound nbtTagCompound = new NBTTagCompound();
    worldSavedData.writeToNBT(nbtTagCompound);

    Assert.assertThat("The incorrect number of keys were found in the NBT data.",
        nbtTagCompound.func_150296_c().size(),
        CoreMatchers.is(worldSavedData.entityNamesToMobGriefingValue.size()));

    for (Entry<String, String> entry : worldSavedData.entityNamesToMobGriefingValue.entrySet()) {
      String nbtValue = nbtTagCompound.getString(entry.getKey());
      Assert.assertThat("The value in the NBT data does not match the expected value.", nbtValue,
          CoreMatchers.is(entry.getValue()));
    }
  }

  /**
   * Test that when world saved data already exists in the map storage that it is reused
   */
  @Test
  public void testForWorld_worldSavedDataExists_reuseWorldSavedData() {
    world.mapStorage.setData(BetterMobGriefingGameRule.MODID, worldSavedData);
    BetterMobGriefingGameRuleWorldSavedData returnedWorldSavedData =
        BetterMobGriefingGameRuleWorldSavedData.forWorld(world);
    Assert.assertThat("The returned world saved data is not the existing instance.",
        returnedWorldSavedData, CoreMatchers.sameInstance(worldSavedData));

    WorldSavedData storedWorldSavedData = world.mapStorage
        .loadData(BetterMobGriefingGameRuleWorldSavedData.class, BetterMobGriefingGameRule.MODID);
    Assert.assertThat("The map storage does not contain the expected world saved data.",
        storedWorldSavedData, CoreMatchers.sameInstance(worldSavedData));
  }

  /**
   * Test that when world saved data does not already exist in the map storage that new world saved
   * data is created
   */
  @Test
  public void testForWorld_worldSavedDataNotExists_createWorldSavedData() {
    BetterMobGriefingGameRuleWorldSavedData returnedWorldSavedData =
        BetterMobGriefingGameRuleWorldSavedData.forWorld(world);
    Assert.assertThat("The returned world saved data is not the existing instance.",
        returnedWorldSavedData, CoreMatchers.not(CoreMatchers.sameInstance(worldSavedData)));

    WorldSavedData storedWorldSavedData = world.mapStorage
        .loadData(BetterMobGriefingGameRuleWorldSavedData.class, BetterMobGriefingGameRule.MODID);
    Assert.assertThat("The map storage does not contain the expected world saved data.",
        storedWorldSavedData, CoreMatchers.sameInstance(returnedWorldSavedData));
  }

  /**
   * Test that all data is correctly output as a string
   */
  @Test
  public void testToString_dataExists_dataOutput() {
    worldSavedData.entityNamesToMobGriefingValue.put("dummyKey1", "dummyValue1");
    worldSavedData.entityNamesToMobGriefingValue.put("dummyKey2", "dummyValue2");

    String worldSavedDataString = worldSavedData.toString();
    String expectedWorldSavedDataString =
        "mobGriefing dummyKey1 = dummyValue1, mobGriefing dummyKey2 = dummyValue2";
    Assert.assertThat("The returned world saved data string does not match the expected string.",
        worldSavedDataString, CoreMatchers.is(expectedWorldSavedDataString));
  }
}
