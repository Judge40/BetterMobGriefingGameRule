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
package com.judge40.minecraft.bettermobgriefinggamerule;

import net.minecraft.client.resources.I18n;

/**
 * Localisable UI messages for BetterMobGriefingGameRule
 */
public class BetterMobGriefingGameRuleMessages {
  // Configuration message keys
  public static final String DEFAULT_MOBGRIEFING_VALUES_KEY =
      "bettermobgriefinggamerule.config.defaultMobGriefingValues";
  public static final String GLOBAL_RULE_KEY = "bettermobgriefinggamerule.config.globalRule";
  public static final String ENTITY_RULES_KEY = "bettermobgriefinggamerule.config.entityRules";

  // Configuration messages
  public static final String DEFAULT_MOBGRIEFING_VALUES =
      I18n.format(DEFAULT_MOBGRIEFING_VALUES_KEY);
  public static final String GLOBAL_RULE = I18n.format(GLOBAL_RULE_KEY);
  public static final String ENTITY_RULES = I18n.format(ENTITY_RULES_KEY);

  public static final String VALID_VALUES(Object validValues) {
    return I18n.format("bettermobgriefinggamerule.config.validValues", validValues);
  }
}
