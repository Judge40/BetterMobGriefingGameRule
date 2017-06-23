/*
 * Better mobGriefing GameRule Copyright (c) 2017 Judge40
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
package com.judge40.minecraft.bettermobgriefinggamerule.common.config;

import java.util.Arrays;
import java.util.List;
import java.util.Map;

import org.hamcrest.CoreMatchers;
import org.junit.Assert;
import org.junit.Test;

import com.judge40.minecraft.bettermobgriefinggamerule.BetterMobGriefingGameRule;
import com.judge40.minecraft.bettermobgriefinggamerule.MobGriefingValue;

import mockit.Expectations;
import mockit.Mocked;
import mockit.Verifications;
import net.minecraft.client.resources.I18n;
import net.minecraft.entity.EntityList;
import net.minecraft.entity.EntityLiving;
import net.minecraft.entity.item.EntityItem;
import net.minecraftforge.common.config.ConfigCategory;
import net.minecraftforge.common.config.Configuration;
import net.minecraftforge.common.config.Property;
import net.minecraftforge.common.config.Property.Type;

/**
 * The unit tests for {@link DefaultMobGriefingConfiguration}.
 */
public class DefaultMobGriefingConfigurationTest {

  @Mocked
  private Configuration parentConfiguration;

  /**
   * Test that the default mob griefing configuration is added when the defaults are not already
   * configured.
   */
  @Test
  public void testDefaultMobGriefingConfiguration_defaultsNotConfigured_defaultsAdded() {
    // Set up test data.
    List<String> validGlobalValues = Arrays.asList(MobGriefingValue.TRUE.toExternalForm(),
        MobGriefingValue.FALSE.toExternalForm());
    List<String> validEntityValues = Arrays.asList(MobGriefingValue.TRUE.toExternalForm(),
        MobGriefingValue.FALSE.toExternalForm(), MobGriefingValue.INHERIT.toExternalForm());

    // Record expectations.
    new Expectations(I18n.class) {
      {
        I18n.format(DefaultMobGriefingConfigurationConstants.VALID_VALUES_KEY, validGlobalValues);
        result = "validGlobalValues";

        parentConfiguration.getString(BetterMobGriefingGameRule.ORIGINAL,
            DefaultMobGriefingConfigurationConstants.GLOBAL_RULE_CATEGORY,
            MobGriefingValue.TRUE.toExternalForm(), "validGlobalValues",
            validGlobalValues.toArray(new String[validGlobalValues.size()]));
        result = MobGriefingValue.TRUE.toExternalForm();

        I18n.format(DefaultMobGriefingConfigurationConstants.VALID_VALUES_KEY, validEntityValues);
        result = "validEntityValues";

        parentConfiguration.getString(anyString,
            DefaultMobGriefingConfigurationConstants.ENTITY_RULES_CATEGORY,
            MobGriefingValue.INHERIT.toExternalForm(), "validEntityValues",
            validEntityValues.toArray(new String[validEntityValues.size()]));
        result = MobGriefingValue.INHERIT.toExternalForm();
        times = DefaultMobGriefingConfigurationConstants.ENTITY_CLASSES.size();

        parentConfiguration.hasChanged();
        result = true;
      }
    };

    // Call the method under test.
    DefaultMobGriefingConfiguration configuration = new DefaultMobGriefingConfiguration(null);

    // Perform assertions.
    MobGriefingValue globalValue = configuration.getGlobalMobGriefingValue();
    Assert.assertThat("The global mob griefing value did not match the expected value.",
        globalValue, CoreMatchers.is(MobGriefingValue.TRUE));

    Map<String, MobGriefingValue> entityNamesToValue = configuration.getEntityMobGriefingValues();
    Assert.assertThat("The number of entity mob grieving values did not match the expected number.",
        entityNamesToValue.size(),
        CoreMatchers.is(DefaultMobGriefingConfigurationConstants.ENTITY_CLASSES.size()));

    for (MobGriefingValue entityValue : entityNamesToValue.values()) {
      Assert.assertThat("The entity mob griefing value did not match the expected value.",
          entityValue, CoreMatchers.is(MobGriefingValue.INHERIT));
    }

    // Verify expectations.
    new Verifications() {
      {
        parentConfiguration.load();

        parentConfiguration.setCategoryLanguageKey(
            DefaultMobGriefingConfigurationConstants.GLOBAL_RULE_CATEGORY,
            DefaultMobGriefingConfigurationConstants.GLOBAL_RULE_KEY);
        parentConfiguration.setCategoryLanguageKey(
            DefaultMobGriefingConfigurationConstants.ENTITY_RULES_CATEGORY,
            DefaultMobGriefingConfigurationConstants.ENTITY_RULES_KEY);

        parentConfiguration.save();
      }
    };
  }

  /**
   * Test that the default mob griefing configuration is updated when the default configuration has
   * invalid values.
   */
  @Test
  public void testDefaultMobGriefingConfiguration_defaultsInvalidValues_defaultsUpdated() {
    // Set up test data.
    List<String> validGlobalValues = Arrays.asList(MobGriefingValue.TRUE.toExternalForm(),
        MobGriefingValue.FALSE.toExternalForm());
    List<String> validEntityValues = Arrays.asList(MobGriefingValue.TRUE.toExternalForm(),
        MobGriefingValue.FALSE.toExternalForm(), MobGriefingValue.INHERIT.toExternalForm());

    // Record expectations.
    new Expectations(I18n.class) {
      {
        I18n.format(DefaultMobGriefingConfigurationConstants.VALID_VALUES_KEY, validGlobalValues);
        result = "validGlobalValues";

        parentConfiguration.getString(BetterMobGriefingGameRule.ORIGINAL,
            DefaultMobGriefingConfigurationConstants.GLOBAL_RULE_CATEGORY,
            MobGriefingValue.TRUE.toExternalForm(), "validGlobalValues",
            validGlobalValues.toArray(new String[validGlobalValues.size()]));
        result = "invalidValue";

        I18n.format(DefaultMobGriefingConfigurationConstants.VALID_VALUES_KEY, validEntityValues);
        result = "validEntityValues";

        parentConfiguration.getString(anyString,
            DefaultMobGriefingConfigurationConstants.ENTITY_RULES_CATEGORY,
            MobGriefingValue.INHERIT.toExternalForm(), "validEntityValues",
            validEntityValues.toArray(new String[validEntityValues.size()]));
        result = "invalidValue";
        times = DefaultMobGriefingConfigurationConstants.ENTITY_CLASSES.size();

        parentConfiguration.hasChanged();
        result = true;
      }
    };

    // Call the method under test.
    DefaultMobGriefingConfiguration configuration = new DefaultMobGriefingConfiguration(null);

    // Perform assertions.
    MobGriefingValue globalValue = configuration.getGlobalMobGriefingValue();
    Assert.assertThat("The global mob griefing value did not match the expected value.",
        globalValue, CoreMatchers.is(MobGriefingValue.TRUE));

    Map<String, MobGriefingValue> entityNamesToValue = configuration.getEntityMobGriefingValues();
    Assert.assertThat("The number of entity mob grieving values did not match the expected number.",
        entityNamesToValue.size(),
        CoreMatchers.is(DefaultMobGriefingConfigurationConstants.ENTITY_CLASSES.size()));

    for (MobGriefingValue entityValue : entityNamesToValue.values()) {
      Assert.assertThat("The entity mob griefing value did not match the expected value.",
          entityValue, CoreMatchers.is(MobGriefingValue.INHERIT));
    }

    // Verify expectations.
    new Verifications() {
      {
        parentConfiguration.load();

        parentConfiguration.setCategoryLanguageKey(
            DefaultMobGriefingConfigurationConstants.GLOBAL_RULE_CATEGORY,
            DefaultMobGriefingConfigurationConstants.GLOBAL_RULE_KEY);
        parentConfiguration.setCategoryLanguageKey(
            DefaultMobGriefingConfigurationConstants.ENTITY_RULES_CATEGORY,
            DefaultMobGriefingConfigurationConstants.ENTITY_RULES_KEY);

        parentConfiguration.save();
      }
    };
  }

  /**
   * Test that the default mob griefing configuration is not updated when the default configuration
   * has valid values.
   */
  @Test
  public void testDefaultMobGriefingConfiguration_defaultsValidValues_defaultsNotUpdated() {
    // Set up test data.
    List<String> validGlobalValues = Arrays.asList(MobGriefingValue.TRUE.toExternalForm(),
        MobGriefingValue.FALSE.toExternalForm());
    List<String> validEntityValues = Arrays.asList(MobGriefingValue.TRUE.toExternalForm(),
        MobGriefingValue.FALSE.toExternalForm(), MobGriefingValue.INHERIT.toExternalForm());

    // Record expectations.
    new Expectations(I18n.class) {
      {
        I18n.format(DefaultMobGriefingConfigurationConstants.VALID_VALUES_KEY, validGlobalValues);
        result = "validGlobalValues";

        parentConfiguration.getString(BetterMobGriefingGameRule.ORIGINAL,
            DefaultMobGriefingConfigurationConstants.GLOBAL_RULE_CATEGORY,
            MobGriefingValue.TRUE.toExternalForm(), "validGlobalValues",
            validGlobalValues.toArray(new String[validGlobalValues.size()]));
        result = MobGriefingValue.FALSE.toExternalForm();

        I18n.format(DefaultMobGriefingConfigurationConstants.VALID_VALUES_KEY, validEntityValues);
        result = "validEntityValues";

        parentConfiguration.getString(anyString,
            DefaultMobGriefingConfigurationConstants.ENTITY_RULES_CATEGORY,
            MobGriefingValue.INHERIT.toExternalForm(), "validEntityValues",
            validEntityValues.toArray(new String[validEntityValues.size()]));
        result = MobGriefingValue.TRUE.toExternalForm();
        times = DefaultMobGriefingConfigurationConstants.ENTITY_CLASSES.size();

        parentConfiguration.hasChanged();
        result = false;
      }
    };

    // Call the method under test.
    DefaultMobGriefingConfiguration configuration = new DefaultMobGriefingConfiguration(null);

    // Perform assertions.
    MobGriefingValue globalValue = configuration.getGlobalMobGriefingValue();
    Assert.assertThat("The global mob griefing value did not match the expected value.",
        globalValue, CoreMatchers.is(MobGriefingValue.FALSE));

    Map<String, MobGriefingValue> entityNamesToValue = configuration.getEntityMobGriefingValues();
    Assert.assertThat("The number of entity mob grieving values did not match the expected number.",
        entityNamesToValue.size(),
        CoreMatchers.is(DefaultMobGriefingConfigurationConstants.ENTITY_CLASSES.size()));

    for (MobGriefingValue entityValue : entityNamesToValue.values()) {
      Assert.assertThat("The entity mob griefing value did not match the expected value.",
          entityValue, CoreMatchers.is(MobGriefingValue.TRUE));
    }

    // Verify expectations.
    new Verifications() {
      {
        parentConfiguration.load();

        parentConfiguration.setCategoryLanguageKey(
            DefaultMobGriefingConfigurationConstants.GLOBAL_RULE_CATEGORY,
            DefaultMobGriefingConfigurationConstants.GLOBAL_RULE_KEY);
        parentConfiguration.setCategoryLanguageKey(
            DefaultMobGriefingConfigurationConstants.ENTITY_RULES_CATEGORY,
            DefaultMobGriefingConfigurationConstants.ENTITY_RULES_KEY);

        parentConfiguration.save();
        times = 0;
      }
    };
  }

  /**
   * Test that the default entity mob griefing value is not loaded from the configuration when the
   * entity name is not an entity.
   */
  @Test
  public void testDefaultMobGriefingConfiguration_notEntityName_valueNotLoaded() {
    // Set up test data.
    List<String> validGlobalValues = Arrays.asList(MobGriefingValue.TRUE.toExternalForm(),
        MobGriefingValue.FALSE.toExternalForm());
    List<String> validEntityValues = Arrays.asList(MobGriefingValue.TRUE.toExternalForm(),
        MobGriefingValue.FALSE.toExternalForm(), MobGriefingValue.INHERIT.toExternalForm());

    ConfigCategory configCategory =
        new ConfigCategory(DefaultMobGriefingConfigurationConstants.ENTITY_RULES_CATEGORY);
    String entityName = "invalidEntity";
    configCategory.put(entityName,
        new Property(entityName, MobGriefingValue.INHERIT.toExternalForm(), Type.STRING));

    // Record expectations.
    new Expectations(I18n.class) {
      {
        I18n.format(DefaultMobGriefingConfigurationConstants.VALID_VALUES_KEY, validGlobalValues);
        result = "validGlobalValues";

        parentConfiguration.getString(BetterMobGriefingGameRule.ORIGINAL,
            DefaultMobGriefingConfigurationConstants.GLOBAL_RULE_CATEGORY,
            MobGriefingValue.TRUE.toExternalForm(), "validGlobalValues",
            validGlobalValues.toArray(new String[validGlobalValues.size()]));
        result = MobGriefingValue.FALSE.toExternalForm();

        parentConfiguration
            .getCategory(DefaultMobGriefingConfigurationConstants.ENTITY_RULES_CATEGORY);
        result = configCategory;

        I18n.format(DefaultMobGriefingConfigurationConstants.VALID_VALUES_KEY, validEntityValues);
        result = "validEntityValues";

        parentConfiguration.getString(anyString,
            DefaultMobGriefingConfigurationConstants.ENTITY_RULES_CATEGORY,
            MobGriefingValue.INHERIT.toExternalForm(), "validEntityValues",
            validEntityValues.toArray(new String[validEntityValues.size()]));
        result = MobGriefingValue.TRUE.toExternalForm();
        times = DefaultMobGriefingConfigurationConstants.ENTITY_CLASSES.size();

        parentConfiguration.hasChanged();
        result = false;
      }
    };

    // Call the method under test.
    DefaultMobGriefingConfiguration configuration = new DefaultMobGriefingConfiguration(null);

    // Perform assertions.
    MobGriefingValue globalValue = configuration.getGlobalMobGriefingValue();
    Assert.assertThat("The global mob griefing value did not match the expected value.",
        globalValue, CoreMatchers.is(MobGriefingValue.FALSE));

    Map<String, MobGriefingValue> entityNamesToValue = configuration.getEntityMobGriefingValues();
    Assert.assertThat("The number of entity mob grieving values did not match the expected number.",
        entityNamesToValue.size(),
        CoreMatchers.is(DefaultMobGriefingConfigurationConstants.ENTITY_CLASSES.size()));
    Assert.assertThat("The entity mob griefing values contained an unexpected entity.",
        entityNamesToValue.keySet(), CoreMatchers.not(CoreMatchers.hasItems(entityName)));

    for (MobGriefingValue entityValue : entityNamesToValue.values()) {
      Assert.assertThat("The entity mob griefing value did not match the expected value.",
          entityValue, CoreMatchers.is(MobGriefingValue.TRUE));
    }

    // Verify expectations.
    new Verifications() {
      {
        parentConfiguration.load();

        parentConfiguration.setCategoryLanguageKey(
            DefaultMobGriefingConfigurationConstants.GLOBAL_RULE_CATEGORY,
            DefaultMobGriefingConfigurationConstants.GLOBAL_RULE_KEY);
        parentConfiguration.setCategoryLanguageKey(
            DefaultMobGriefingConfigurationConstants.ENTITY_RULES_CATEGORY,
            DefaultMobGriefingConfigurationConstants.ENTITY_RULES_KEY);

        parentConfiguration.save();
        times = 0;
      }
    };
  }

  /**
   * Test that the default entity mob griefing value is not loaded from the configuration when the
   * entity name is not a valid entity type.
   */
  @Test
  public void testDefaultMobGriefingConfiguration_invalidEntityName_valueNotLoaded() {
    // Set up test data.
    List<String> validGlobalValues = Arrays.asList(MobGriefingValue.TRUE.toExternalForm(),
        MobGriefingValue.FALSE.toExternalForm());
    List<String> validEntityValues = Arrays.asList(MobGriefingValue.TRUE.toExternalForm(),
        MobGriefingValue.FALSE.toExternalForm(), MobGriefingValue.INHERIT.toExternalForm());

    ConfigCategory configCategory =
        new ConfigCategory(DefaultMobGriefingConfigurationConstants.ENTITY_RULES_CATEGORY);
    String entityName = (String) EntityList.classToStringMapping.get(EntityItem.class);
    configCategory.put(entityName,
        new Property(entityName, MobGriefingValue.INHERIT.toExternalForm(), Type.STRING));

    // Record expectations.
    new Expectations(I18n.class) {
      {
        I18n.format(DefaultMobGriefingConfigurationConstants.VALID_VALUES_KEY, validGlobalValues);
        result = "validGlobalValues";

        parentConfiguration.getString(BetterMobGriefingGameRule.ORIGINAL,
            DefaultMobGriefingConfigurationConstants.GLOBAL_RULE_CATEGORY,
            MobGriefingValue.TRUE.toExternalForm(), "validGlobalValues",
            validGlobalValues.toArray(new String[validGlobalValues.size()]));
        result = MobGriefingValue.FALSE.toExternalForm();

        parentConfiguration
            .getCategory(DefaultMobGriefingConfigurationConstants.ENTITY_RULES_CATEGORY);
        result = configCategory;

        I18n.format(DefaultMobGriefingConfigurationConstants.VALID_VALUES_KEY, validEntityValues);
        result = "validEntityValues";

        parentConfiguration.getString(anyString,
            DefaultMobGriefingConfigurationConstants.ENTITY_RULES_CATEGORY,
            MobGriefingValue.INHERIT.toExternalForm(), "validEntityValues",
            validEntityValues.toArray(new String[validEntityValues.size()]));
        result = MobGriefingValue.TRUE.toExternalForm();
        times = DefaultMobGriefingConfigurationConstants.ENTITY_CLASSES.size();

        parentConfiguration.hasChanged();
        result = false;
      }
    };

    // Call the method under test.
    DefaultMobGriefingConfiguration configuration = new DefaultMobGriefingConfiguration(null);

    // Perform assertions.
    MobGriefingValue globalValue = configuration.getGlobalMobGriefingValue();
    Assert.assertThat("The global mob griefing value did not match the expected value.",
        globalValue, CoreMatchers.is(MobGriefingValue.FALSE));

    Map<String, MobGriefingValue> entityNamesToValue = configuration.getEntityMobGriefingValues();
    Assert.assertThat("The number of entity mob grieving values did not match the expected number.",
        entityNamesToValue.size(),
        CoreMatchers.is(DefaultMobGriefingConfigurationConstants.ENTITY_CLASSES.size()));
    Assert.assertThat("The entity mob griefing values contained an unexpected entity.",
        entityNamesToValue.keySet(), CoreMatchers.not(CoreMatchers.hasItems(entityName)));

    for (MobGriefingValue entityValue : entityNamesToValue.values()) {
      Assert.assertThat("The entity mob griefing value did not match the expected value.",
          entityValue, CoreMatchers.is(MobGriefingValue.TRUE));
    }

    // Verify expectations.
    new Verifications() {
      {
        parentConfiguration.load();

        parentConfiguration.setCategoryLanguageKey(
            DefaultMobGriefingConfigurationConstants.GLOBAL_RULE_CATEGORY,
            DefaultMobGriefingConfigurationConstants.GLOBAL_RULE_KEY);
        parentConfiguration.setCategoryLanguageKey(
            DefaultMobGriefingConfigurationConstants.ENTITY_RULES_CATEGORY,
            DefaultMobGriefingConfigurationConstants.ENTITY_RULES_KEY);

        parentConfiguration.save();
        times = 0;
      }
    };
  }

  /**
   * Test that the default entity mob griefing value is loaded from the configuration when the
   * entity name is a valid entity type.
   */
  @Test
  public void testDefaultMobGriefingConfiguration_validEntityName_valueLoaded() {
    // Set up test data.
    List<String> validGlobalValues = Arrays.asList(MobGriefingValue.TRUE.toExternalForm(),
        MobGriefingValue.FALSE.toExternalForm());
    List<String> validEntityValues = Arrays.asList(MobGriefingValue.TRUE.toExternalForm(),
        MobGriefingValue.FALSE.toExternalForm(), MobGriefingValue.INHERIT.toExternalForm());

    ConfigCategory configCategory =
        new ConfigCategory(DefaultMobGriefingConfigurationConstants.ENTITY_RULES_CATEGORY);
    String entityName = (String) EntityList.classToStringMapping.get(EntityLiving.class);
    configCategory.put(entityName,
        new Property(entityName, MobGriefingValue.INHERIT.toExternalForm(), Type.STRING));

    // Record expectations.
    new Expectations(I18n.class) {
      {
        I18n.format(DefaultMobGriefingConfigurationConstants.VALID_VALUES_KEY, validGlobalValues);
        result = "validGlobalValues";

        parentConfiguration.getString(BetterMobGriefingGameRule.ORIGINAL,
            DefaultMobGriefingConfigurationConstants.GLOBAL_RULE_CATEGORY,
            MobGriefingValue.TRUE.toExternalForm(), "validGlobalValues",
            validGlobalValues.toArray(new String[validGlobalValues.size()]));
        result = MobGriefingValue.FALSE.toExternalForm();

        parentConfiguration
            .getCategory(DefaultMobGriefingConfigurationConstants.ENTITY_RULES_CATEGORY);
        result = configCategory;

        I18n.format(DefaultMobGriefingConfigurationConstants.VALID_VALUES_KEY, validEntityValues);
        result = "validEntityValues";

        parentConfiguration.getString(anyString,
            DefaultMobGriefingConfigurationConstants.ENTITY_RULES_CATEGORY,
            MobGriefingValue.INHERIT.toExternalForm(), "validEntityValues",
            validEntityValues.toArray(new String[validEntityValues.size()]));
        result = MobGriefingValue.TRUE.toExternalForm();
        times = DefaultMobGriefingConfigurationConstants.ENTITY_CLASSES.size() + 1;

        parentConfiguration.hasChanged();
        result = false;
      }
    };

    // Call the method under test.
    DefaultMobGriefingConfiguration configuration = new DefaultMobGriefingConfiguration(null);

    // Perform assertions.
    MobGriefingValue globalValue = configuration.getGlobalMobGriefingValue();
    Assert.assertThat("The global mob griefing value did not match the expected value.",
        globalValue, CoreMatchers.is(MobGriefingValue.FALSE));

    Map<String, MobGriefingValue> entityNamesToValue = configuration.getEntityMobGriefingValues();
    Assert.assertThat("The number of entity mob grieving values did not match the expected number.",
        entityNamesToValue.size(),
        CoreMatchers.is(DefaultMobGriefingConfigurationConstants.ENTITY_CLASSES.size() + 1));
    Assert.assertThat("The entity mob griefing values contained an unexpected entity.",
        entityNamesToValue.keySet(), CoreMatchers.hasItems(entityName));

    for (MobGriefingValue entityValue : entityNamesToValue.values()) {
      Assert.assertThat("The entity mob griefing value did not match the expected value.",
          entityValue, CoreMatchers.is(MobGriefingValue.TRUE));
    }

    // Verify expectations.
    new Verifications() {
      {
        parentConfiguration.load();

        parentConfiguration.setCategoryLanguageKey(
            DefaultMobGriefingConfigurationConstants.GLOBAL_RULE_CATEGORY,
            DefaultMobGriefingConfigurationConstants.GLOBAL_RULE_KEY);
        parentConfiguration.setCategoryLanguageKey(
            DefaultMobGriefingConfigurationConstants.ENTITY_RULES_CATEGORY,
            DefaultMobGriefingConfigurationConstants.ENTITY_RULES_KEY);

        parentConfiguration.save();
        times = 0;
      }
    };
  }
}
