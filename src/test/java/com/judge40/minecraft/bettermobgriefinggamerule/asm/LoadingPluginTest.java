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

package com.judge40.minecraft.bettermobgriefinggamerule.asm;

import org.hamcrest.CoreMatchers;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

/**
 * The unit tests for {@link LoadingPlugin}.
 */
public class LoadingPluginTest {

  private LoadingPlugin loadingPlugin;

  @Before
  public void setUp() {
    loadingPlugin = new LoadingPlugin();
  }

  /**
   * Test that null is returned for the access transformer class.
   */
  @Test
  public void testGetAccessTransformerClass_null() {
    // Call the method under test.
    String accessTransformerClass = loadingPlugin.getAccessTransformerClass();

    // Perform assertions.
    Assert.assertThat("The registered access transformer class should be null.",
        accessTransformerClass, CoreMatchers.nullValue());
  }

  /**
   * Test that the BetterMobGriefingGameRule ASMTransformerClass is registered.
   */
  @Test
  public void testGetAsmTransformerClass_transformerClassRegistered() {
    // Call the method under test.
    String[] asmTransformerClass = loadingPlugin.getASMTransformerClass();

    // Perform assertions.
    Assert.assertThat("An unexpected number of ASM transformer classes are registered.",
        asmTransformerClass.length, CoreMatchers.is(1));
    Assert.assertThat("The BetterMobGriefingGameRule transformer class is not registered.",
        asmTransformerClass[0], CoreMatchers.is(ClassTransformer.class.getName()));
  }

  /**
   * Test that null is returned for the mod container class.
   */
  @Test
  public void testGetModContainerClass_null() {
    // Call the method under test.
    String modContainerClass = loadingPlugin.getModContainerClass();

    // Perform assertions.
    Assert.assertThat("The registered mod container class should be null.", modContainerClass,
        CoreMatchers.nullValue());
  }

  /**
   * Test that null is returned for the setup class.
   */
  @Test
  public void testGetSetupClass_null() {
    // Call the method under test.
    String setupClass = loadingPlugin.getSetupClass();

    // Perform assertions.
    Assert.assertThat("The registered setup class should be null.", setupClass,
        CoreMatchers.nullValue());
  }

  /**
   * This test added for code coverage only, the method under test is expected to do nothing.
   */
  @Test
  public void testInjectData_null_doNothing() {
    // Call the method under test.
    loadingPlugin.injectData(null);
  }
}
