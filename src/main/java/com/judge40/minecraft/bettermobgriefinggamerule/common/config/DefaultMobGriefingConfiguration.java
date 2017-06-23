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
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.judge40.minecraft.bettermobgriefinggamerule.BetterMobGriefingGameRule;
import com.judge40.minecraft.bettermobgriefinggamerule.MobGriefingValue;

import net.minecraft.client.resources.I18n;
import net.minecraft.entity.EntityList;
import net.minecraft.entity.EntityLiving;
import net.minecraftforge.common.config.ConfigCategory;
import net.minecraftforge.common.config.Configuration;

/**
 * A custom {@link Configuration} which adds convenience methods for retrieving default mob griefing
 * configurations.
 */
public class DefaultMobGriefingConfiguration extends Configuration {

  private MobGriefingValue globalMobGriefingValue;

  private Map<String, MobGriefingValue> entityNamesToMobGriefingValue;

  /**
   * Creates a new or loads the existing configuration file and sets the category language keys.
   *
   * @param file The configuration file.
   */
  public DefaultMobGriefingConfiguration(File file) {
    // Load the configuration file, it will be created if it does not exist.
    super(file);
    load();

    // Set configuration category language keys
    setCategoryLanguageKey(DefaultMobGriefingConfigurationConstants.GLOBAL_RULE_CATEGORY,
        DefaultMobGriefingConfigurationConstants.GLOBAL_RULE_KEY);
    setCategoryLanguageKey(DefaultMobGriefingConfigurationConstants.ENTITY_RULES_CATEGORY,
        DefaultMobGriefingConfigurationConstants.ENTITY_RULES_KEY);

    // Synchronize the configuration values.
    synchronize();
  }

  /**
   * Synchronize the configuration's mob griefing values, the current values are retrieved from the
   * configuration file and stored in the {@link DefaultMobGriefingConfiguration}. Any property
   * values which are missing or invalid will be populated with their default values.
   */
  public void synchronize() {
    synchronizeGlobalValue();
    synchronizeEntityValues();

    // Save any changes made to the configuration.
    if (hasChanged()) {
      save();
    }
  }

  /**
   * Synchronize the configuration's global mob griefing value, the current value is retrieved from
   * the configuration file and stored in the {@link DefaultMobGriefingConfiguration}. If the
   * property value is missing or invalid it will be populated with its default value.
   */
  private void synchronizeGlobalValue() {
    List<String> validValues = Arrays.asList(MobGriefingValue.TRUE.toExternalForm(),
        MobGriefingValue.FALSE.toExternalForm());

    String globalPropertyValue = getString(
        DefaultMobGriefingConfigurationConstants.GLOBAL_RULE_CATEGORY,
        BetterMobGriefingGameRule.ORIGINAL, MobGriefingValue.TRUE.toExternalForm(), validValues);
    globalMobGriefingValue = MobGriefingValue.toEnumeration(globalPropertyValue);
  }

  /**
   * Synchronize the configuration's entity mob griefing values, the current values are retrieved
   * from the configuration file and stored in the {@link DefaultMobGriefingConfiguration}. Any
   * property values which are missing or invalid will be populated with their default values.
   */
  private void synchronizeEntityValues() {
    // Get the names of all configured entities.
    ConfigCategory category =
        getCategory(DefaultMobGriefingConfigurationConstants.ENTITY_RULES_CATEGORY);
    Set<String> entityNames = new HashSet<>(category.keySet());

    // Add all entities supported by default, this will cover newly supported entities and any
    // entities which have been removed from the configuration.
    for (Class<? extends EntityLiving> entityClass : DefaultMobGriefingConfigurationConstants.ENTITY_CLASSES) {
      String entityName = (String) EntityList.classToStringMapping.get(entityClass);
      entityNames.add(entityName);
    }

    // Get the property value for each entity.
    entityNamesToMobGriefingValue = new HashMap<>();
    List<String> validValues = Arrays.asList(MobGriefingValue.TRUE.toExternalForm(),
        MobGriefingValue.FALSE.toExternalForm(), MobGriefingValue.INHERIT.toExternalForm());

    for (String entityName : entityNames) {
      Class<?> entityClass = (Class<?>) EntityList.stringToClassMapping.get(entityName);

      // Verify that the entity is a valid entity.
      if (entityClass != null && EntityLiving.class.isAssignableFrom(entityClass)) {
        String entityPropertyValue =
            getString(DefaultMobGriefingConfigurationConstants.ENTITY_RULES_CATEGORY, entityName,
                MobGriefingValue.INHERIT.toExternalForm(), validValues);
        entityNamesToMobGriefingValue.put(entityName,
            MobGriefingValue.toEnumeration(entityPropertyValue));
      }
    }
  }

  /**
   * Get the string value of a property from the configuration file, any property values which are
   * missing or invalid will be populated with the default value.
   * 
   * @param categoryName The name of the property's category.
   * @param propertyName The name of the property.
   * @param defaultValue The default value for the property.
   * @param validValues The valid values for the property.
   * @return The property string value.
   */
  private String getString(String categoryName, String propertyName, String defaultValue,
      List<String> validValues) {
    String propertyValue = getString(propertyName, categoryName, defaultValue,
        I18n.format(DefaultMobGriefingConfigurationConstants.VALID_VALUES_KEY, validValues),
        validValues.toArray(new String[validValues.size()]));

    if (!validValues.contains(propertyValue)) {
      propertyValue = defaultValue;
    }

    return propertyValue;
  }

  /**
   * Get the configured default global mobGriefing value.
   * 
   * @return The configured default value for global mobGriefing.
   */
  public MobGriefingValue getGlobalMobGriefingValue() {
    return globalMobGriefingValue;
  }

  /**
   * Get the configured default mobGriefing values for all entities.
   * 
   * @return A map where the key is the entity name and the value is the configured default
   *         mobGriefing value.
   */
  public Map<String, MobGriefingValue> getEntityMobGriefingValues() {
    return entityNamesToMobGriefingValue;
  }
}
