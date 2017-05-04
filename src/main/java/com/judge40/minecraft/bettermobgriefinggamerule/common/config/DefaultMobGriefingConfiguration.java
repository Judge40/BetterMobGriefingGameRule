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
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import com.judge40.minecraft.bettermobgriefinggamerule.BetterMobGriefingGameRule;
import com.judge40.minecraft.bettermobgriefinggamerule.BetterMobGriefingGameRuleMessages;

import net.minecraft.entity.EntityList;
import net.minecraft.entity.EntityLiving;
import net.minecraftforge.common.config.ConfigCategory;
import net.minecraftforge.common.config.Configuration;
import scala.actors.threadpool.Arrays;

/**
 * A custom {@link Configuration} which adds convenience methods for retrieving default mob griefing
 * configurations.
 */
public class DefaultMobGriefingConfiguration extends Configuration {

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
    setCategoryLanguageKey(
        DefaultMobGriefingConfigurationConstants.DEFAULT_MOBGRIEFING_VALUES_CATEGORY,
        BetterMobGriefingGameRuleMessages.DEFAULT_MOBGRIEFING_VALUES_KEY);
    setCategoryLanguageKey(DefaultMobGriefingConfigurationConstants.GLOBAL_RULE_CATEGORY,
        BetterMobGriefingGameRuleMessages.GLOBAL_RULE_KEY);
    setCategoryLanguageKey(DefaultMobGriefingConfigurationConstants.ENTITY_RULES_CATEGORY,
        BetterMobGriefingGameRuleMessages.ENTITY_RULES_KEY);
  }

  /**
   * Get the configured default global mobGriefing value.
   * 
   * @return The configured default value for global mobGriefing.
   */
  public String getGlobalMobGriefingValue() {
    String[] validValues =
        new String[] {BetterMobGriefingGameRule.TRUE, BetterMobGriefingGameRule.FALSE};

    String globalValue = getString(BetterMobGriefingGameRule.ORIGINAL,
        DefaultMobGriefingConfigurationConstants.GLOBAL_RULE_CATEGORY,
        BetterMobGriefingGameRule.TRUE,
        BetterMobGriefingGameRuleMessages.VALID_VALUES(Arrays.asList(validValues)), validValues);

    // Save any changes to the configuration.
    if (hasChanged()) {
      save();
    }

    return globalValue;
  }

  /**
   * Get the default mobGriefing values for all configured entities.
   * 
   * @return A map where the key is the entity name and the value is the configured default
   *         mobGriefing value.
   */
  public Map<String, String> getEntityMobGriefingValues() {
    String[] validValues = new String[] {BetterMobGriefingGameRule.TRUE,
        BetterMobGriefingGameRule.FALSE, BetterMobGriefingGameRule.INHERIT};

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

    // Get the configured default value for each entity, if the configuration entry is missing for
    // an entity then a new entry is created.
    Map<String, String> defaultEntityRules = new HashMap<>();

    for (String entityName : entityNames) {
      Class<?> entityClass = (Class<?>) EntityList.stringToClassMapping.get(entityName);

      // Verify that the entity is a valid entity.
      if (entityClass != null && EntityLiving.class.isAssignableFrom(entityClass)) {
        String propertyValue =
            getString(entityName, DefaultMobGriefingConfigurationConstants.ENTITY_RULES_CATEGORY,
                BetterMobGriefingGameRule.INHERIT,
                BetterMobGriefingGameRuleMessages.VALID_VALUES(Arrays.asList(validValues)),
                validValues);
        defaultEntityRules.put(entityName, propertyValue);
      }
    }

    // Save any changes to the configuration.
    if (hasChanged()) {
      save();
    }

    return defaultEntityRules;
  }
}
