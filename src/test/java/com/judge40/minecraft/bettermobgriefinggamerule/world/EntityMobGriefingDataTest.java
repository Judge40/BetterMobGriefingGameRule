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

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.hamcrest.CoreMatchers;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.judge40.minecraft.bettermobgriefinggamerule.MobGriefingValue;
import com.judge40.minecraft.bettermobgriefinggamerule.ModInfoConstants;
import com.judge40.minecraft.bettermobgriefinggamerule.common.config.DefaultMobGriefingConfiguration;

import mockit.Expectations;
import mockit.Mocked;
import mockit.Verifications;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.world.World;
import net.minecraft.world.storage.MapStorage;

/**
 * The unit tests for {@link EntityMobGriefingData}.
 */
public class EntityMobGriefingDataTest {

  private EntityMobGriefingData entityMobGriefingData;

  @Before
  public void setUp() {
    entityMobGriefingData = new EntityMobGriefingData(ModInfoConstants.ID);
  }

  /**
   * Test that the existing {@link EntityMobGriefingData} is returned when an instance exists in the
   * {@link World}'s {@link MapStorage}.
   */
  @Test
  public void testForWorld_dataExists_existingDataInstance(@Mocked World world,
      @Mocked MapStorage mapStorage) {
    // Set up test data.
    world.mapStorage = mapStorage;

    // Record expectations.
    new Expectations() {
      {
        mapStorage.loadData(EntityMobGriefingData.class, ModInfoConstants.ID);
        result = entityMobGriefingData;
      }
    };

    // Call the method under test.
    EntityMobGriefingData entityMobGriefingData = EntityMobGriefingData.forWorld(world);

    // Perform assertions.
    Assert.assertThat("The EntityMobGriefingData instance did not match the expected instance.",
        entityMobGriefingData, CoreMatchers.sameInstance(this.entityMobGriefingData));

    // Verify expectations.
    new Verifications() {
      {
        mapStorage.setData(ModInfoConstants.ID, withInstanceOf(EntityMobGriefingData.class));
        times = 0;
      }
    };
  }

  /**
   * Test that a new instance of {@link EntityMobGriefingData} is returned when an instance does not
   * exist in the {@link World}'s {@link MapStorage}.
   */
  @Test
  public void testForWorld_dataNotExists_newDataInstance(@Mocked World world,
      @Mocked MapStorage mapStorage) {
    // Set up test data.
    world.mapStorage = mapStorage;

    // Record expectations.
    new Expectations() {
      {
        mapStorage.loadData(EntityMobGriefingData.class, ModInfoConstants.ID);
        result = null;

        new EntityMobGriefingData(ModInfoConstants.ID);

        mapStorage.setData(ModInfoConstants.ID, withInstanceOf(EntityMobGriefingData.class));
      }
    };

    // Call the method under test.
    EntityMobGriefingData entityMobGriefingData = EntityMobGriefingData.forWorld(world);

    // Perform assertions.
    Assert.assertThat("The EntityMobGriefingData instance did not match the expected class.",
        entityMobGriefingData, CoreMatchers.instanceOf(EntityMobGriefingData.class));
    Assert.assertThat("The EntityMobGriefingData instance did not match the expected instance.",
        entityMobGriefingData,
        CoreMatchers.not(CoreMatchers.sameInstance(this.entityMobGriefingData)));
  }

  /**
   * Test that the {@link EntityMobGriefingData} is not updated when all configured entity names
   * match, all configured {@link MobGriefingValue}s match and overwrite is true.
   */
  @Test
  public void testPopulateFromConfiguration_matchedNamesMatchedValuesOverwrite_entityDataNotUpdated(
      @Mocked DefaultMobGriefingConfiguration configuration) {
    // Set up test data.
    Map<String, MobGriefingValue> entityMobGriefingValues = new HashMap<>();
    entityMobGriefingValues.put("entityName1", MobGriefingValue.TRUE);
    entityMobGriefingValues.put("entityName2", MobGriefingValue.FALSE);
    entityMobGriefingValues.put("entityName3", MobGriefingValue.INHERIT);

    entityMobGriefingData.setMobGriefingValue("entityName1", MobGriefingValue.TRUE);
    entityMobGriefingData.setMobGriefingValue("entityName2", MobGriefingValue.FALSE);
    entityMobGriefingData.setMobGriefingValue("entityName3", MobGriefingValue.INHERIT);
    entityMobGriefingData.setDirty(false);

    // Record expectations.
    new Expectations() {
      {
        configuration.getEntityMobGriefingValues();
        result = entityMobGriefingValues;
      }
    };

    // Call the method under test.
    entityMobGriefingData.populateFromConfiguration(configuration, true);

    // Perform assertions.
    Assert.assertThat("The EntityMobGriefingData did not contain the expected value.",
        entityMobGriefingData.getMobGriefingValue("entityName1"),
        CoreMatchers.is(MobGriefingValue.TRUE));
    Assert.assertThat("The EntityMobGriefingData did not contain the expected value.",
        entityMobGriefingData.getMobGriefingValue("entityName2"),
        CoreMatchers.is(MobGriefingValue.FALSE));
    Assert.assertThat("The EntityMobGriefingData did not contain the expected value.",
        entityMobGriefingData.getMobGriefingValue("entityName3"),
        CoreMatchers.is(MobGriefingValue.INHERIT));

    Assert.assertThat("The EntityMobGriefingData's dirty flag did not match the expected value.",
        entityMobGriefingData.isDirty(), CoreMatchers.is(false));
  }

  /**
   * Test that the {@link EntityMobGriefingData} is not updated when all configured entity names
   * match, all configured {@link MobGriefingValue}s match and overwrite is false.
   */
  @Test
  public void testPopulateFromConfiguration_matchedNamesMatchedValuesNotOverwrite_entityDataNotUpdated(
      @Mocked DefaultMobGriefingConfiguration configuration) {
    // Set up test data.
    Map<String, MobGriefingValue> entityMobGriefingValues = new HashMap<>();
    entityMobGriefingValues.put("entityName1", MobGriefingValue.TRUE);
    entityMobGriefingValues.put("entityName2", MobGriefingValue.FALSE);
    entityMobGriefingValues.put("entityName3", MobGriefingValue.INHERIT);

    entityMobGriefingData.setMobGriefingValue("entityName1", MobGriefingValue.TRUE);
    entityMobGriefingData.setMobGriefingValue("entityName2", MobGriefingValue.FALSE);
    entityMobGriefingData.setMobGriefingValue("entityName3", MobGriefingValue.INHERIT);
    entityMobGriefingData.setDirty(false);

    // Record expectations.
    new Expectations() {
      {
        configuration.getEntityMobGriefingValues();
        result = entityMobGriefingValues;
      }
    };

    // Call the method under test.
    entityMobGriefingData.populateFromConfiguration(configuration, false);

    // Perform assertions.
    Assert.assertThat("The EntityMobGriefingData did not contain the expected value.",
        entityMobGriefingData.getMobGriefingValue("entityName1"),
        CoreMatchers.is(MobGriefingValue.TRUE));
    Assert.assertThat("The EntityMobGriefingData did not contain the expected value.",
        entityMobGriefingData.getMobGriefingValue("entityName2"),
        CoreMatchers.is(MobGriefingValue.FALSE));
    Assert.assertThat("The EntityMobGriefingData did not contain the expected value.",
        entityMobGriefingData.getMobGriefingValue("entityName3"),
        CoreMatchers.is(MobGriefingValue.INHERIT));

    Assert.assertThat("The EntityMobGriefingData's dirty flag did not match the expected value.",
        entityMobGriefingData.isDirty(), CoreMatchers.is(false));
  }

  /**
   * Test that the {@link EntityMobGriefingData} is updated when all configured entity names match,
   * not all configured {@link MobGriefingValue}s match and overwrite is true.
   */
  @Test
  public void testPopulateFromConfiguration_matchedNamesNotMatchedValuesOverwrite_entityDataUpdated(
      @Mocked DefaultMobGriefingConfiguration configuration) {
    // Set up test data.
    Map<String, MobGriefingValue> entityMobGriefingValues = new HashMap<>();
    entityMobGriefingValues.put("entityName1", MobGriefingValue.TRUE);
    entityMobGriefingValues.put("entityName2", MobGriefingValue.FALSE);
    entityMobGriefingValues.put("entityName3", MobGriefingValue.INHERIT);

    entityMobGriefingData.setMobGriefingValue("entityName1", MobGriefingValue.INHERIT);
    entityMobGriefingData.setMobGriefingValue("entityName2", MobGriefingValue.TRUE);
    entityMobGriefingData.setMobGriefingValue("entityName3", MobGriefingValue.FALSE);
    entityMobGriefingData.setDirty(false);

    // Record expectations.
    new Expectations() {
      {
        configuration.getEntityMobGriefingValues();
        result = entityMobGriefingValues;
      }
    };

    // Call the method under test.
    entityMobGriefingData.populateFromConfiguration(configuration, true);

    // Perform assertions.
    Assert.assertThat("The EntityMobGriefingData did not contain the expected value.",
        entityMobGriefingData.getMobGriefingValue("entityName1"),
        CoreMatchers.is(MobGriefingValue.TRUE));
    Assert.assertThat("The EntityMobGriefingData did not contain the expected value.",
        entityMobGriefingData.getMobGriefingValue("entityName2"),
        CoreMatchers.is(MobGriefingValue.FALSE));
    Assert.assertThat("The EntityMobGriefingData did not contain the expected value.",
        entityMobGriefingData.getMobGriefingValue("entityName3"),
        CoreMatchers.is(MobGriefingValue.INHERIT));

    Assert.assertThat("The EntityMobGriefingData's dirty flag did not match the expected value.",
        entityMobGriefingData.isDirty(), CoreMatchers.is(true));
  }

  /**
   * Test that the {@link EntityMobGriefingData} is not updated when all configured entity names
   * match, not all configured {@link MobGriefingValue}s match and overwrite is false.
   */
  @Test
  public void testPopulateFromConfiguration_matchedNamesNotMatchedValuesNotOverwrite_entityDataNotUpdated(
      @Mocked DefaultMobGriefingConfiguration configuration) {
    // Set up test data.
    Map<String, MobGriefingValue> entityMobGriefingValues = new HashMap<>();
    entityMobGriefingValues.put("entityName1", MobGriefingValue.TRUE);
    entityMobGriefingValues.put("entityName2", MobGriefingValue.FALSE);
    entityMobGriefingValues.put("entityName3", MobGriefingValue.INHERIT);

    entityMobGriefingData.setMobGriefingValue("entityName1", MobGriefingValue.INHERIT);
    entityMobGriefingData.setMobGriefingValue("entityName2", MobGriefingValue.TRUE);
    entityMobGriefingData.setMobGriefingValue("entityName3", MobGriefingValue.FALSE);
    entityMobGriefingData.setDirty(false);

    // Record expectations.
    new Expectations() {
      {
        configuration.getEntityMobGriefingValues();
        result = entityMobGriefingValues;
      }
    };

    // Call the method under test.
    entityMobGriefingData.populateFromConfiguration(configuration, false);

    // Perform assertions.
    Assert.assertThat("The EntityMobGriefingData did not contain the expected value.",
        entityMobGriefingData.getMobGriefingValue("entityName1"),
        CoreMatchers.is(MobGriefingValue.INHERIT));
    Assert.assertThat("The EntityMobGriefingData did not contain the expected value.",
        entityMobGriefingData.getMobGriefingValue("entityName2"),
        CoreMatchers.is(MobGriefingValue.TRUE));
    Assert.assertThat("The EntityMobGriefingData did not contain the expected value.",
        entityMobGriefingData.getMobGriefingValue("entityName3"),
        CoreMatchers.is(MobGriefingValue.FALSE));

    Assert.assertThat("The EntityMobGriefingData's dirty flag did not match the expected value.",
        entityMobGriefingData.isDirty(), CoreMatchers.is(false));
  }

  /**
   * Test that the {@link EntityMobGriefingData} is updated when not all configured entity names
   * match and overwrite is true.
   */
  @Test
  public void testPopulateFromConfiguration_notMatchedNamesOverwrite_entityDataUpdated(
      @Mocked DefaultMobGriefingConfiguration configuration) {
    // Set up test data.
    Map<String, MobGriefingValue> entityMobGriefingValues = new HashMap<>();
    entityMobGriefingValues.put("entityName1", MobGriefingValue.TRUE);
    entityMobGriefingValues.put("entityName2", MobGriefingValue.FALSE);

    entityMobGriefingData.setMobGriefingValue("entityName1", MobGriefingValue.INHERIT);
    entityMobGriefingData.setMobGriefingValue("entityName3", MobGriefingValue.INHERIT);
    entityMobGriefingData.setDirty(false);

    // Record expectations.
    new Expectations() {
      {
        configuration.getEntityMobGriefingValues();
        result = entityMobGriefingValues;
      }
    };

    // Call the method under test.
    entityMobGriefingData.populateFromConfiguration(configuration, true);

    // Perform assertions.
    Assert.assertThat("The EntityMobGriefingData did not contain the expected value.",
        entityMobGriefingData.getMobGriefingValue("entityName1"),
        CoreMatchers.is(MobGriefingValue.TRUE));
    Assert.assertThat("The EntityMobGriefingData did not contain the expected value.",
        entityMobGriefingData.getMobGriefingValue("entityName2"),
        CoreMatchers.is(MobGriefingValue.FALSE));
    Assert.assertThat("The EntityMobGriefingData did not contain the expected value.",
        entityMobGriefingData.getMobGriefingValue("entityName3"),
        CoreMatchers.is(MobGriefingValue.INHERIT));

    Assert.assertThat("The EntityMobGriefingData's dirty flag did not match the expected value.",
        entityMobGriefingData.isDirty(), CoreMatchers.is(true));
  }

  /**
   * Test that the {@link EntityMobGriefingData} is updated when not all configured entity names
   * match and overwrite is false.
   */
  @Test
  public void testPopulateFromConfiguration_notMatchedNamesNotOverwrite_entityDataUpdated(
      @Mocked DefaultMobGriefingConfiguration configuration) {
    // Set up test data.
    Map<String, MobGriefingValue> entityMobGriefingValues = new HashMap<>();
    entityMobGriefingValues.put("entityName1", MobGriefingValue.TRUE);
    entityMobGriefingValues.put("entityName2", MobGriefingValue.FALSE);

    entityMobGriefingData.setMobGriefingValue("entityName1", MobGriefingValue.INHERIT);
    entityMobGriefingData.setMobGriefingValue("entityName3", MobGriefingValue.INHERIT);
    entityMobGriefingData.setDirty(false);

    // Record expectations.
    new Expectations() {
      {
        configuration.getEntityMobGriefingValues();
        result = entityMobGriefingValues;
      }
    };

    // Call the method under test.
    entityMobGriefingData.populateFromConfiguration(configuration, false);

    // Perform assertions.
    Assert.assertThat("The EntityMobGriefingData did not contain the expected value.",
        entityMobGriefingData.getMobGriefingValue("entityName1"),
        CoreMatchers.is(MobGriefingValue.INHERIT));
    Assert.assertThat("The EntityMobGriefingData did not contain the expected value.",
        entityMobGriefingData.getMobGriefingValue("entityName2"),
        CoreMatchers.is(MobGriefingValue.FALSE));
    Assert.assertThat("The EntityMobGriefingData did not contain the expected value.",
        entityMobGriefingData.getMobGriefingValue("entityName3"),
        CoreMatchers.is(MobGriefingValue.INHERIT));

    Assert.assertThat("The EntityMobGriefingData's dirty flag did not match the expected value.",
        entityMobGriefingData.isDirty(), CoreMatchers.is(true));
  }

  /**
   * Test that the {@link EntityMobGriefingData} is populated when the {@link NBTTagCompound} is
   * populated.
   */
  @Test
  public void testReadFromNBT_nbtDataPopulated_entityDataPopulated() {
    // Set up test data.
    NBTTagCompound nbtTagCompound = new NBTTagCompound();
    nbtTagCompound.setString("entityName1", "true");
    nbtTagCompound.setString("entityName2", "false");
    nbtTagCompound.setString("entityName3", "inherit");

    // Call the method under test.
    entityMobGriefingData.readFromNBT(nbtTagCompound);

    // Perform assertions.
    Assert.assertThat("The EntityMobGriefingData did not contain the expected value.",
        entityMobGriefingData.getMobGriefingValue("entityName1"),
        CoreMatchers.is(MobGriefingValue.TRUE));
    Assert.assertThat("The EntityMobGriefingData did not contain the expected value.",
        entityMobGriefingData.getMobGriefingValue("entityName2"),
        CoreMatchers.is(MobGriefingValue.FALSE));
    Assert.assertThat("The EntityMobGriefingData did not contain the expected value.",
        entityMobGriefingData.getMobGriefingValue("entityName3"),
        CoreMatchers.is(MobGriefingValue.INHERIT));
  }

  /**
   * Test that the {@link EntityMobGriefingData} is not populated when the {@link NBTTagCompound} is
   * not populated.
   */
  @Test
  public void testReadFromNBT_nbtDataNotPopulated_entityDataNotPopulated() {
    // Set up test data.
    NBTTagCompound nbtTagCompound = new NBTTagCompound();

    // Call the method under test.
    entityMobGriefingData.readFromNBT(nbtTagCompound);

    // Perform assertions.
    Assert.assertThat("The EntityMobGriefingData did not contain the expected value.",
        entityMobGriefingData.getRegisteredEntityNames(), CoreMatchers.is(Collections.emptySet()));
  }

  /**
   * Test that the {@link NBTTagCompound} is populated when the {@link EntityMobGriefingData} is
   * populated.
   */
  @Test
  public void testWriteToNBT_entityDataPopulated_nbtDataPopulated() {
    // Set up test data.
    entityMobGriefingData.setMobGriefingValue("entityName1", MobGriefingValue.TRUE);
    entityMobGriefingData.setMobGriefingValue("entityName2", MobGriefingValue.FALSE);
    entityMobGriefingData.setMobGriefingValue("entityName3", MobGriefingValue.INHERIT);

    NBTTagCompound nbtTagCompound = new NBTTagCompound();

    // Call the method under test.
    entityMobGriefingData.writeToNBT(nbtTagCompound);

    // Perform assertions.
    Assert.assertThat("The NBT tag compound did not contain the expected value.",
        nbtTagCompound.getString("entityName1"), CoreMatchers.is("true"));
    Assert.assertThat("The NBT tag compound did not contain the expected value.",
        nbtTagCompound.getString("entityName2"), CoreMatchers.is("false"));
    Assert.assertThat("The NBT tag compound did not contain the expected value.",
        nbtTagCompound.getString("entityName3"), CoreMatchers.is("inherit"));
  }

  /**
   * Test that the {@link NBTTagCompound} is not populated when the {@link EntityMobGriefingData} is
   * not populated.
   */
  @Test
  public void testWriteToNBT_entityDataNotPopulated_nbtDataNotPopulated() {
    // Set up test data.
    NBTTagCompound nbtTagCompound = new NBTTagCompound();

    // Call the method under test.
    entityMobGriefingData.writeToNBT(nbtTagCompound);

    // Perform assertions.
    Assert.assertThat("The NBT tag compound did not contain the expected value.",
        nbtTagCompound.func_150296_c(), CoreMatchers.is(Collections.emptySet()));
  }

  /**
   * Test that a string containing a readable representation of the {@link EntityMobGriefingData} is
   * returned when {@code EntityMobGriefingData} is populated.
   */
  @Test
  public void testToString_entityDataPopulated_readableString() {
    // Set up test data.
    entityMobGriefingData.setMobGriefingValue("entityName1", MobGriefingValue.TRUE);
    entityMobGriefingData.setMobGriefingValue("entityName2", MobGriefingValue.FALSE);
    entityMobGriefingData.setMobGriefingValue("entityName3", MobGriefingValue.INHERIT);

    // Call the method under test.
    String stringRepresentation = entityMobGriefingData.toString();

    // Perform assertions.
    String expectedString = "entityName1 = true, entityName2 = false, entityName3 = inherit";
    Assert.assertThat(
        "The string representation of the EntityMobGriefingData did not match the expected value.",
        stringRepresentation, CoreMatchers.is(expectedString));
  }

  /**
   * Test that an empty string is returned when the {@link EntityMobGriefingData} is not populated;
   */
  @Test
  public void testToString_entityDataNotPopulated_emptyString() {
    // Call the method under test.
    String stringRepresentation = entityMobGriefingData.toString();

    // Perform assertions.
    Assert.assertThat(
        "The string representation of the EntityMobGriefingData did not match the expected value.",
        stringRepresentation, CoreMatchers.is(""));
  }
}
