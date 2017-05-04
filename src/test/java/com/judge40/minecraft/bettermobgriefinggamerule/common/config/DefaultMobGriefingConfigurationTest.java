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

import java.io.File;
import java.util.Map;

import org.hamcrest.CoreMatchers;
import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.Test;

import com.judge40.minecraft.bettermobgriefinggamerule.BetterMobGriefingGameRule;

import mockit.Deencapsulation;
import mockit.Expectations;
import mockit.Injectable;
import mockit.Mocked;
import mockit.Tested;
import mockit.Verifications;
import net.minecraft.client.resources.I18n;
import net.minecraft.client.resources.Locale;
import net.minecraft.entity.EntityList;
import net.minecraft.entity.EntityLiving;
import net.minecraftforge.common.config.ConfigCategory;
import net.minecraftforge.common.config.Configuration;
import net.minecraftforge.common.config.Property;
import net.minecraftforge.common.config.Property.Type;

/**
 * The unit tests for {@link DefaultMobGriefingConfiguration}.
 */
public class DefaultMobGriefingConfigurationTest {

  @Tested
  private DefaultMobGriefingConfiguration configuration;

  @Injectable
  private File configFile;

  @Mocked
  private Configuration parentConfiguration;

  @BeforeClass
  public static void setUpBeforeClass() throws Exception {
    Deencapsulation.setField(I18n.class, "i18nLocale", new Locale());
  }

  /**
   * Test that the configuration is not saved when the global rule already exists in the
   * configuration.
   */
  @Test
  public void testGetGlobalMobGriefingValue_globalRuleInConfig_configNotSaved() {
    // Record expectations.
    new Expectations() {
      {
        configuration.getString(BetterMobGriefingGameRule.ORIGINAL,
            DefaultMobGriefingConfigurationConstants.GLOBAL_RULE_CATEGORY, "true", anyString,
            new String[] {"true", "false"});
        result = "globalValue";

        configuration.hasChanged();
        result = false;
      }
    };

    // Call the method under test.
    String globalValue = configuration.getGlobalMobGriefingValue();

    // Perform assertions.
    Assert.assertThat("The global value from the configuration does not match the expected value.",
        globalValue, CoreMatchers.is("globalValue"));

    // Verify executions.
    new Verifications() {
      {
        configuration.save();
        times = 0;
      }
    };
  }

  /**
   * Test that the configuration is saved when the global rule does not already exist in the
   * configuration.
   */
  @Test
  public void testGetGlobalMobGriefingValue_globalRuleNotInConfig_configSaved() {
    // Record expectations.
    new Expectations() {
      {
        configuration.getString(BetterMobGriefingGameRule.ORIGINAL,
            DefaultMobGriefingConfigurationConstants.GLOBAL_RULE_CATEGORY, "true", anyString,
            new String[] {"true", "false"});
        result = "globalValue";

        configuration.hasChanged();
        result = true;
      }
    };

    // Call the method under test.
    String globalValue = configuration.getGlobalMobGriefingValue();

    // Perform assertions.
    Assert.assertThat("The global value from the configuration does not match the expected value.",
        globalValue, CoreMatchers.is("globalValue"));

    // Verify executions.
    new Verifications() {
      {
        configuration.save();
        times = 1;
      }
    };
  }

  /**
   * Test that the configuration is not saved when the entity rules already exist in the
   * configuration.
   */
  @Test
  public void testGetEntityMobGriefingValues_configUnchanged_configNotSaved() {
    // Set up test data.
    Class<? extends EntityLiving> defaultEntityClass =
        DefaultMobGriefingConfigurationConstants.ENTITY_CLASSES.get(0);
    String defaultEntityName = (String) EntityList.classToStringMapping.get(defaultEntityClass);

    // Record expectations.
    new Expectations() {
      {
        configuration.getString(defaultEntityName,
            DefaultMobGriefingConfigurationConstants.ENTITY_RULES_CATEGORY, "inherit", anyString,
            new String[] {"true", "false", "inherit"});
        result = "entityValue";
        times = 1;

        configuration.getString(anyString,
            DefaultMobGriefingConfigurationConstants.ENTITY_RULES_CATEGORY, "inherit", anyString,
            new String[] {"true", "false", "inherit"});
        result = "entityValue";

        configuration.hasChanged();
        result = false;
      }
    };

    // Call the method under test.
    Map<String, String> entityValues = configuration.getEntityMobGriefingValues();

    // Perform assertions.
    Assert.assertThat(
        "The entity values from the configuration does not contain an expected entity.",
        entityValues.keySet(), CoreMatchers.hasItem(defaultEntityName));

    // Verify executions.
    new Verifications() {
      {
        configuration.save();
        times = 0;
      }
    };
  }

  /**
   * Test that the configuration is saved when an entity rule does not already exist in the
   * configuration.
   */
  @Test
  public void testGetEntityMobGriefingValues_configChanged_configSaved() {
    // Set up test data.
    ConfigCategory configCategory = new ConfigCategory("entityRules");
    configCategory.put("Entity", new Property("Entity", "inherit", Type.STRING));
    configCategory.put("invalidEntity", new Property("invalidEntity", "inherit", Type.STRING));

    Class<? extends EntityLiving> defaultEntityClass =
        DefaultMobGriefingConfigurationConstants.ENTITY_CLASSES.get(0);
    String defaultEntityName = (String) EntityList.classToStringMapping.get(defaultEntityClass);

    // Record expectations.
    new Expectations() {
      {
        configuration.getCategory(anyString);
        result = configCategory;

        configuration.getString(defaultEntityName,
            DefaultMobGriefingConfigurationConstants.ENTITY_RULES_CATEGORY, "inherit", anyString,
            new String[] {"true", "false", "inherit"});
        result = "entityValue";
        times = 1;

        configuration.getString(anyString,
            DefaultMobGriefingConfigurationConstants.ENTITY_RULES_CATEGORY, "inherit", anyString,
            new String[] {"true", "false", "inherit"});
        result = "entityValue";

        configuration.hasChanged();
        result = true;
      }
    };

    // Call the method under test.
    Map<String, String> entityValues = configuration.getEntityMobGriefingValues();

    // Perform assertions.
    Assert.assertThat(
        "The entity values from the configuration does not contain an expected entity.",
        entityValues.keySet(), CoreMatchers.hasItem(defaultEntityName));

    // Verify executions.
    new Verifications() {
      {
        configuration.save();
        times = 1;
      }
    };
  }

  /**
   * Test that an entity is included when the entity from the configuration is a supported entity.
   */
  @Test
  public void testGetEntityMobGriefingValues_supportedEntityInConfig_entityIncluded() {
    // Set up test data.
    ConfigCategory configCategory = new ConfigCategory("entityRules");
    configCategory.put("SupportedEntity", new Property("SupportedEntity", "inherit", Type.STRING));

    @SuppressWarnings("unchecked")
    Map<String, Class<? extends EntityLiving>> stringToClassMapping =
        EntityList.stringToClassMapping;
    stringToClassMapping.put("SupportedEntity", EntityLiving.class);

    // Record expectations.
    new Expectations() {
      {
        configuration.getCategory(anyString);
        result = configCategory;

        configuration.getString(anyString,
            DefaultMobGriefingConfigurationConstants.ENTITY_RULES_CATEGORY, "inherit", anyString,
            new String[] {"true", "false", "inherit"});
        result = "entityValue";

        configuration.hasChanged();
        result = false;
      }
    };

    // Call the method under test.
    Map<String, String> entityValues = configuration.getEntityMobGriefingValues();

    // Perform assertions.
    Assert.assertThat(
        "The entity values from the configuration does not contain an expected entity.",
        entityValues.keySet(), CoreMatchers.hasItem("SupportedEntity"));

    // Verify executions.
    new Verifications() {
      {
        configuration.save();
        times = 0;
      }
    };
  }

  /**
   * Test that an entity is excluded when the entity from the configuration is not a supported
   * entity.
   */
  @Test
  public void testGetEntityMobGriefingValues_unsupportedEntityInConfig_entityExcluded() {
    // Set up test data.
    ConfigCategory configCategory = new ConfigCategory("entityRules");
    configCategory.put("Entity", new Property("Entity", "inherit", Type.STRING));

    // Record expectations.
    new Expectations() {
      {
        configuration.getCategory(anyString);
        result = configCategory;

        configuration.getString(anyString,
            DefaultMobGriefingConfigurationConstants.ENTITY_RULES_CATEGORY, "inherit", anyString,
            new String[] {"true", "false", "inherit"});
        result = "entityValue";

        configuration.hasChanged();
        result = false;
      }
    };

    // Call the method under test.
    Map<String, String> entityValues = configuration.getEntityMobGriefingValues();

    // Perform assertions.
    Assert.assertThat("The entity values from the configuration contains an unsupported entity.",
        entityValues.keySet(), CoreMatchers.not(CoreMatchers.hasItem("Entity")));

    // Verify executions.
    new Verifications() {
      {
        configuration.save();
        times = 0;
      }
    };
  }

  /**
   * Test that an entity is excluded when the entity from the configuration is not a valid entity.
   */
  @Test
  public void testGetEntityMobGriefingValues_invalidEntityInConfig_entityExcluded() {
    // Set up test data.
    ConfigCategory configCategory = new ConfigCategory("entityRules");
    configCategory.put("invalidEntity", new Property("invalidEntity", "inherit", Type.STRING));

    // Record expectations.
    new Expectations() {
      {
        configuration.getCategory(anyString);
        result = configCategory;

        configuration.getString(anyString,
            DefaultMobGriefingConfigurationConstants.ENTITY_RULES_CATEGORY, "inherit", anyString,
            new String[] {"true", "false", "inherit"});
        result = "entityValue";

        configuration.hasChanged();
        result = false;
      }
    };

    // Call the method under test.
    Map<String, String> entityValues = configuration.getEntityMobGriefingValues();

    // Perform assertions.
    Assert.assertThat("The entity values from the configuration contains an invalid entity.",
        entityValues.keySet(), CoreMatchers.not(CoreMatchers.hasItem("invalidEntity")));

    // Verify executions.
    new Verifications() {
      {
        configuration.save();
        times = 0;
      }
    };
  }
}
