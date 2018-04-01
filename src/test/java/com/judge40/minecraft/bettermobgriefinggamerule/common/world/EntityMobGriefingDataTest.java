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

import com.judge40.minecraft.bettermobgriefinggamerule.common.MobGriefingValue;
import com.judge40.minecraft.bettermobgriefinggamerule.common.ModInfoConstants;
import com.judge40.minecraft.bettermobgriefinggamerule.common.configuration.DefaultMobGriefingConfiguration;

import mockit.Expectations;
import mockit.Mocked;
import mockit.Verifications;
import net.minecraft.entity.EntityList;
import net.minecraft.launchwrapper.Launch;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.util.ResourceLocation;
import net.minecraft.world.World;
import net.minecraft.world.storage.MapStorage;
import org.hamcrest.CoreMatchers;
import org.junit.Assert;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;

/**
 * The unit tests for {@link EntityMobGriefingData}.
 */
public class EntityMobGriefingDataTest {

  private EntityMobGriefingData entityMobGriefingData;

  /**
   * Populate the {@code fml.deobfuscatedEnvironment} flag.
   */
  @BeforeClass
  public static void setUpBeforeClass() {
    // Set the deobfuscation flag.
    Map<String, Object> blackboard = new HashMap<>();
    blackboard.put("fml.deobfuscatedEnvironment", true);
    Launch.blackboard = blackboard;
  }

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
    // Record expectations.
    new Expectations() {
      {
        mapStorage.getOrLoadData(EntityMobGriefingData.class, ModInfoConstants.ID);
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
    // Record expectations.
    new Expectations() {
      {
        mapStorage.getOrLoadData(EntityMobGriefingData.class, ModInfoConstants.ID);
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
   * match and all configured {@link MobGriefingValue}s match.
   */
  @Test
  public void testPopulateFromConfiguration_matchedNamesMatchedValues_dataNotUpdated(
      @Mocked DefaultMobGriefingConfiguration configuration) {
    // Set up test data.
    Map<String, MobGriefingValue> entityMobGriefingValues = new HashMap<>();
    entityMobGriefingValues.put("entity_name1", MobGriefingValue.TRUE);
    entityMobGriefingValues.put("entity_name2", MobGriefingValue.FALSE);
    entityMobGriefingValues.put("entity_name3", MobGriefingValue.INHERIT);

    entityMobGriefingData.setMobGriefingValue("entity_name1", MobGriefingValue.TRUE);
    entityMobGriefingData.setMobGriefingValue("entity_name2", MobGriefingValue.FALSE);
    entityMobGriefingData.setMobGriefingValue("entity_name3", MobGriefingValue.INHERIT);
    entityMobGriefingData.setDirty(false);

    // Record expectations.
    new Expectations() {
      {
        configuration.getEntityMobGriefingValues();
        result = entityMobGriefingValues;
      }
    };

    // Call the method under test.
    entityMobGriefingData.populateFromConfiguration(configuration);

    // Perform assertions.
    Assert.assertThat("The EntityMobGriefingData did not contain the expected value.",
        entityMobGriefingData.getMobGriefingValue("entity_name1"),
        CoreMatchers.is(MobGriefingValue.TRUE));
    Assert.assertThat("The EntityMobGriefingData did not contain the expected value.",
        entityMobGriefingData.getMobGriefingValue("entity_name2"),
        CoreMatchers.is(MobGriefingValue.FALSE));
    Assert.assertThat("The EntityMobGriefingData did not contain the expected value.",
        entityMobGriefingData.getMobGriefingValue("entity_name3"),
        CoreMatchers.is(MobGriefingValue.INHERIT));

    Assert.assertThat("The EntityMobGriefingData's dirty flag did not match the expected value.",
        entityMobGriefingData.isDirty(), CoreMatchers.is(false));
  }

  /**
   * Test that the {@link EntityMobGriefingData} is not updated when all configured entity names
   * match and not all configured {@link MobGriefingValue}s match.
   */
  @Test
  public void testPopulateFromConfiguration_matchedNamesNotMatchedValues_dataNotUpdated(
      @Mocked DefaultMobGriefingConfiguration configuration) {
    // Set up test data.
    Map<String, MobGriefingValue> entityMobGriefingValues = new HashMap<>();
    entityMobGriefingValues.put("entity_name1", MobGriefingValue.TRUE);
    entityMobGriefingValues.put("entity_name2", MobGriefingValue.FALSE);
    entityMobGriefingValues.put("entity_name3", MobGriefingValue.INHERIT);

    entityMobGriefingData.setMobGriefingValue("entity_name1", MobGriefingValue.INHERIT);
    entityMobGriefingData.setMobGriefingValue("entity_name2", MobGriefingValue.TRUE);
    entityMobGriefingData.setMobGriefingValue("entity_name3", MobGriefingValue.FALSE);
    entityMobGriefingData.setDirty(false);

    // Record expectations.
    new Expectations() {
      {
        configuration.getEntityMobGriefingValues();
        result = entityMobGriefingValues;
      }
    };

    // Call the method under test.
    entityMobGriefingData.populateFromConfiguration(configuration);

    // Perform assertions.
    Assert.assertThat("The EntityMobGriefingData did not contain the expected value.",
        entityMobGriefingData.getMobGriefingValue("entity_name1"),
        CoreMatchers.is(MobGriefingValue.INHERIT));
    Assert.assertThat("The EntityMobGriefingData did not contain the expected value.",
        entityMobGriefingData.getMobGriefingValue("entity_name2"),
        CoreMatchers.is(MobGriefingValue.TRUE));
    Assert.assertThat("The EntityMobGriefingData did not contain the expected value.",
        entityMobGriefingData.getMobGriefingValue("entity_name3"),
        CoreMatchers.is(MobGriefingValue.FALSE));

    Assert.assertThat("The EntityMobGriefingData's dirty flag did not match the expected value.",
        entityMobGriefingData.isDirty(), CoreMatchers.is(false));
  }

  /**
   * Test that the {@link EntityMobGriefingData} is updated when not all configured entity names
   * match.
   */
  @Test
  public void testPopulateFromConfiguration_notMatchedNames_dataUpdated(
      @Mocked DefaultMobGriefingConfiguration configuration) {
    // Set up test data.
    Map<String, MobGriefingValue> entityMobGriefingValues = new HashMap<>();
    entityMobGriefingValues.put("entity_name1", MobGriefingValue.TRUE);
    entityMobGriefingValues.put("entity_name2", MobGriefingValue.FALSE);

    entityMobGriefingData.setMobGriefingValue("entity_name1", MobGriefingValue.INHERIT);
    entityMobGriefingData.setMobGriefingValue("entity_name3", MobGriefingValue.INHERIT);
    entityMobGriefingData.setDirty(false);

    // Record expectations.
    new Expectations() {
      {
        configuration.getEntityMobGriefingValues();
        result = entityMobGriefingValues;
      }
    };

    // Call the method under test.
    entityMobGriefingData.populateFromConfiguration(configuration);

    // Perform assertions.
    Assert.assertThat("The EntityMobGriefingData did not contain the expected value.",
        entityMobGriefingData.getMobGriefingValue("entity_name1"),
        CoreMatchers.is(MobGriefingValue.INHERIT));
    Assert.assertThat("The EntityMobGriefingData did not contain the expected value.",
        entityMobGriefingData.getMobGriefingValue("entity_name2"),
        CoreMatchers.is(MobGriefingValue.FALSE));
    Assert.assertThat("The EntityMobGriefingData did not contain the expected value.",
        entityMobGriefingData.getMobGriefingValue("entity_name3"),
        CoreMatchers.is(MobGriefingValue.INHERIT));

    Assert.assertThat("The EntityMobGriefingData's dirty flag did not match the expected value.",
        entityMobGriefingData.isDirty(), CoreMatchers.is(true));
  }

  /**
   * Test that the {@link EntityMobGriefingData} is populated when the {@link NBTTagCompound} is
   * populated and the entities are registered and the entity name and path do not match.
   */
  @Test
  public void testReadFromNbt_nbtDataPopulatedEntitiesRegisteredMismatch_entityDataPopulated() {
    // Set up test data.
    NBTTagCompound nbtTagCompound = new NBTTagCompound();
    nbtTagCompound.setString("entity_name1", "true");
    nbtTagCompound.setString("entity_name2", "false");
    nbtTagCompound.setString("Entity_Name3", "inherit");

    // Record expectations.
    new Expectations(EntityList.class) {
      {
        EntityList.isRegistered((ResourceLocation) any);
        result = true;
      }
    };

    // Call the method under test.
    entityMobGriefingData.readFromNBT(nbtTagCompound);

    // Perform assertions.
    Assert.assertThat("The EntityMobGriefingData did not contain the expected number of entries.",
        entityMobGriefingData.getRegisteredEntityNames().size(), CoreMatchers.is(3));
    Assert.assertThat("The EntityMobGriefingData did not contain the expected value.",
        entityMobGriefingData.getMobGriefingValue("entity_name1"),
        CoreMatchers.is(MobGriefingValue.TRUE));
    Assert.assertThat("The EntityMobGriefingData did not contain the expected value.",
        entityMobGriefingData.getMobGriefingValue("entity_name2"),
        CoreMatchers.is(MobGriefingValue.FALSE));
    Assert.assertThat("The EntityMobGriefingData did not contain the expected value.",
        entityMobGriefingData.getMobGriefingValue("entity_name3"),
        CoreMatchers.is(MobGriefingValue.INHERIT));

    Assert.assertThat("The NBT data contains an unexpected numbers of values.",
        nbtTagCompound.getSize(), CoreMatchers.is(2));
    Assert.assertThat("The NBT data contains an unexpected value.",
        nbtTagCompound.getString("Entity_Name3"), CoreMatchers.is(""));
  }

  /**
   * Test that the {@link EntityMobGriefingData} is populated when the {@link NBTTagCompound} is
   * populated and the entities are registered and the entity name and path match.
   */
  @Test
  public void testReadFromNbt_nbtDataPopulatedEntitiesRegisteredMatch_entityDataPopulated() {
    // Set up test data.
    NBTTagCompound nbtTagCompound = new NBTTagCompound();
    nbtTagCompound.setString("entity_name1", "true");
    nbtTagCompound.setString("entity_name2", "false");
    nbtTagCompound.setString("entity_name3", "inherit");

    // Record expectations.
    new Expectations(EntityList.class) {
      {
        EntityList.isRegistered((ResourceLocation) any);
        result = true;
      }
    };

    // Call the method under test.
    entityMobGriefingData.readFromNBT(nbtTagCompound);

    // Perform assertions.
    Assert.assertThat("The EntityMobGriefingData did not contain the expected number of entries.",
        entityMobGriefingData.getRegisteredEntityNames().size(), CoreMatchers.is(3));
    Assert.assertThat("The EntityMobGriefingData did not contain the expected value.",
        entityMobGriefingData.getMobGriefingValue("entity_name1"),
        CoreMatchers.is(MobGriefingValue.TRUE));
    Assert.assertThat("The EntityMobGriefingData did not contain the expected value.",
        entityMobGriefingData.getMobGriefingValue("entity_name2"),
        CoreMatchers.is(MobGriefingValue.FALSE));
    Assert.assertThat("The EntityMobGriefingData did not contain the expected value.",
        entityMobGriefingData.getMobGriefingValue("entity_name3"),
        CoreMatchers.is(MobGriefingValue.INHERIT));

    Assert.assertThat("The NBT data contains an unexpected numbers of values.",
        nbtTagCompound.getSize(), CoreMatchers.is(3));
  }

  /**
   * Test that the {@link EntityMobGriefingData} is populated when the {@link NBTTagCompound} is
   * populated and an entity is using its translation name.
   */
  @Test
  public void testReadFromNbt_nbtDataPopulatedEntityUsingTranslationName_entityDataPopulated() {
    // Set up test data.
    NBTTagCompound nbtTagCompound = new NBTTagCompound();
    nbtTagCompound.setString("entity_name1", "true");
    nbtTagCompound.setString("entityName2", "false");
    nbtTagCompound.setString("entityName3", "inherit");

    ResourceLocation entityType2 = new ResourceLocation("entity_name2");
    ResourceLocation entityType3 = new ResourceLocation("entity_name3");

    // Record expectations.
    new Expectations(EntityList.class) {
      {
        EntityList.isRegistered(new ResourceLocation("entity_name1"));
        result = true;

        EntityList.isRegistered((ResourceLocation) any);
        result = false;

        EntityList.getEntityNameList();
        result = new HashSet<>(Arrays.asList(entityType2, entityType3));

        EntityList.getTranslationName(entityType2);
        result = "entityName2";

        EntityList.getTranslationName(entityType3);
        result = "entityName3";
      }
    };

    // Call the method under test.
    entityMobGriefingData.readFromNBT(nbtTagCompound);

    // Perform assertions.
    Assert.assertThat("The EntityMobGriefingData did not contain the expected number of entries.",
        entityMobGriefingData.getRegisteredEntityNames().size(), CoreMatchers.is(3));
    Assert.assertThat("The EntityMobGriefingData did not contain the expected value.",
        entityMobGriefingData.getMobGriefingValue("entity_name1"),
        CoreMatchers.is(MobGriefingValue.TRUE));
    Assert.assertThat("The EntityMobGriefingData did not contain the expected value.",
        entityMobGriefingData.getMobGriefingValue("entity_name2"),
        CoreMatchers.is(MobGriefingValue.FALSE));
    Assert.assertThat("The EntityMobGriefingData did not contain the expected value.",
        entityMobGriefingData.getMobGriefingValue("entity_name3"),
        CoreMatchers.is(MobGriefingValue.INHERIT));

    Assert.assertThat("The NBTTagCompound contains an unexpected value.",
        nbtTagCompound.getKeySet(), CoreMatchers.not(CoreMatchers.hasItem("entityName2")));
    Assert.assertThat("The NBTTagCompound contains an unexpected value.",
        nbtTagCompound.getKeySet(), CoreMatchers.not(CoreMatchers.hasItem("entityName3")));
  }

  /**
   * Test that the {@link EntityMobGriefingData} is populated when the {@link NBTTagCompound} is
   * populated and an entity cannot be found by its name or translation name.
   */
  @Test
  public void testReadFromNbt_nbtDataPopulatedEntityNotFound_entityDataPopulated() {
    // Set up test data.
    NBTTagCompound nbtTagCompound = new NBTTagCompound();
    nbtTagCompound.setString("entity_name1", "true");
    nbtTagCompound.setString("unfoundEntityName2", "false");
    nbtTagCompound.setString("unfoundEntityName3", "inherit");

    ResourceLocation entityType2 = new ResourceLocation("entity_name2");
    ResourceLocation entityType3 = new ResourceLocation("entity_name3");

    // Record expectations.
    new Expectations(EntityList.class) {
      {
        EntityList.isRegistered(new ResourceLocation("entity_name1"));
        result = true;

        EntityList.isRegistered((ResourceLocation) any);
        result = false;

        EntityList.getEntityNameList();
        result = new HashSet<>(Arrays.asList(entityType2, entityType3));

        EntityList.getTranslationName(entityType2);
        result = "entityName2";

        EntityList.getTranslationName(entityType3);
        result = "entityName3";
      }
    };

    // Call the method under test.
    entityMobGriefingData.readFromNBT(nbtTagCompound);

    // Perform assertions.
    Assert.assertThat("The EntityMobGriefingData did not contain the expected number of entries.",
        entityMobGriefingData.getRegisteredEntityNames().size(), CoreMatchers.is(1));
    Assert.assertThat("The EntityMobGriefingData did not contain the expected value.",
        entityMobGriefingData.getMobGriefingValue("entity_name1"),
        CoreMatchers.is(MobGriefingValue.TRUE));

    Assert.assertThat("The NBTTagCompound contains an unexpected value.",
        nbtTagCompound.getKeySet(), CoreMatchers.not(CoreMatchers.hasItem("entityName2")));
    Assert.assertThat("The NBTTagCompound contains an unexpected value.",
        nbtTagCompound.getKeySet(), CoreMatchers.not(CoreMatchers.hasItem("entityName3")));
  }

  /**
   * Test that the {@link EntityMobGriefingData} is not populated when the {@link NBTTagCompound} is
   * not populated.
   */
  @Test
  public void testReadFromNbt_nbtDataNotPopulated_entityDataNotPopulated() {
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
  public void testWriteToNbt_entityDataPopulated_nbtDataPopulated() {
    // Set up test data.
    entityMobGriefingData.setMobGriefingValue("entity_name1", MobGriefingValue.TRUE);
    entityMobGriefingData.setMobGriefingValue("entity_name2", MobGriefingValue.FALSE);
    entityMobGriefingData.setMobGriefingValue("entity_name3", MobGriefingValue.INHERIT);

    NBTTagCompound nbtTagCompound = new NBTTagCompound();

    // Call the method under test.
    entityMobGriefingData.writeToNBT(nbtTagCompound);

    // Perform assertions.
    Assert.assertThat("The NBT tag compound did not contain the expected value.",
        nbtTagCompound.getString("entity_name1"), CoreMatchers.is("true"));
    Assert.assertThat("The NBT tag compound did not contain the expected value.",
        nbtTagCompound.getString("entity_name2"), CoreMatchers.is("false"));
    Assert.assertThat("The NBT tag compound did not contain the expected value.",
        nbtTagCompound.getString("entity_name3"), CoreMatchers.is("inherit"));
  }

  /**
   * Test that the {@link NBTTagCompound} is not populated when the {@link EntityMobGriefingData} is
   * not populated.
   */
  @Test
  public void testWriteToNbt_entityDataNotPopulated_nbtDataNotPopulated() {
    // Set up test data.
    NBTTagCompound nbtTagCompound = new NBTTagCompound();

    // Call the method under test.
    entityMobGriefingData.writeToNBT(nbtTagCompound);

    // Perform assertions.
    Assert.assertThat("The NBT tag compound did not contain the expected value.",
        nbtTagCompound.getKeySet(), CoreMatchers.is(Collections.emptySet()));
  }

  /**
   * Test that a string containing a readable representation of the {@link EntityMobGriefingData} is
   * returned when {@code EntityMobGriefingData} is populated.
   */
  @Test
  public void testToString_dataPopulated_readableString() {
    // Set up test data.
    entityMobGriefingData.setMobGriefingValue("entity_name1", MobGriefingValue.TRUE);
    entityMobGriefingData.setMobGriefingValue("entity_name2", MobGriefingValue.FALSE);
    entityMobGriefingData.setMobGriefingValue("entity_name3", MobGriefingValue.INHERIT);

    // Call the method under test.
    String stringRepresentation = entityMobGriefingData.toString();

    // Perform assertions.
    String expectedString = "entity_name1 = true, entity_name2 = false, entity_name3 = inherit";
    Assert.assertThat(
        "The string representation of the EntityMobGriefingData did not match the expected value.",
        stringRepresentation, CoreMatchers.is(expectedString));
  }

  /**
   * Test that an empty string is returned when the {@link EntityMobGriefingData} is not populated.
   */
  @Test
  public void testToString_dataNotPopulated_emptyString() {
    // Call the method under test.
    String stringRepresentation = entityMobGriefingData.toString();

    // Perform assertions.
    Assert.assertThat(
        "The string representation of the EntityMobGriefingData did not match the expected value.",
        stringRepresentation, CoreMatchers.is(""));
  }
}
