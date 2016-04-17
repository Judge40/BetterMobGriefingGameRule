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
package com.judge40.minecraft.bettermobgriefinggamerulecore;

import org.hamcrest.CoreMatchers;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

/**
 * Tests for BetterMobGriefingGameRuleIFMLLoadingPlugin
 */
public class BetterMobGriefingGameRuleIFMLLoadingPluginTest {

  private BetterMobGriefingGameRuleIFMLLoadingPlugin betterMobGriefingGameRuleIFMLLoadingPlugin;

  /**
   * @throws java.lang.Exception
   */
  @Before
  public void setUp() throws Exception {
    betterMobGriefingGameRuleIFMLLoadingPlugin = new BetterMobGriefingGameRuleIFMLLoadingPlugin();
  }

  /**
   * @throws java.lang.Exception
   */
  @After
  public void tearDown() throws Exception {
    betterMobGriefingGameRuleIFMLLoadingPlugin = null;
  }

  /**
   * Test that the BetterMobGriefingGameRule ASMTransformerClass is registered
   */
  @Test
  public void testGetASMTransformerClass_transformerClassRegistered() {
    String[] asmTransformerClass =
        betterMobGriefingGameRuleIFMLLoadingPlugin.getASMTransformerClass();
    Assert.assertThat("Too many transformers classes registered.", asmTransformerClass.length,
        CoreMatchers.is(1));
    Assert.assertThat("BetterMobGriefingGameRule transformer class not registered.",
        asmTransformerClass[0],
        CoreMatchers.is(BetterMobGriefingGameRuleIClassTransformer.class.getName()));
  }

  /**
   * Test that null is returned for the mod container class
   */
  @Test
  public void testGetModContainerClass_nullReturned() {
    String modContainerClass = betterMobGriefingGameRuleIFMLLoadingPlugin.getModContainerClass();
    Assert.assertThat("Registered mod container class should be null.", modContainerClass,
        CoreMatchers.nullValue());
  }

  /**
   * Test that null is returned for the setup class
   */
  @Test
  public void testGetSetupClass_nullReturned() {
    String setupClass = betterMobGriefingGameRuleIFMLLoadingPlugin.getSetupClass();
    Assert.assertThat("Registered setup class should be null.", setupClass,
        CoreMatchers.nullValue());
  }

  /**
   * Test for code coverage, method under test is expected to do nothing
   */
  @Test
  public void testInjectData_doNothing() {
    betterMobGriefingGameRuleIFMLLoadingPlugin.injectData(null);
  }

  /**
   * Test that null is returned for the access transformer class
   */
  @Test
  public void testGetAccessTransformerClass_nullReturned() {
    String accessTransformerClass =
        betterMobGriefingGameRuleIFMLLoadingPlugin.getAccessTransformerClass();
    Assert.assertThat("Registered access transformer class should be null.", accessTransformerClass,
        CoreMatchers.nullValue());
  }
}
