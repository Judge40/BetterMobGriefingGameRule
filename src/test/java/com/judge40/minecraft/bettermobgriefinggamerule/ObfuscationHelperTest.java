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
package com.judge40.minecraft.bettermobgriefinggamerule;

import java.util.HashMap;
import java.util.Map;

import org.hamcrest.CoreMatchers;
import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.Test;

import mockit.Deencapsulation;
import net.minecraft.launchwrapper.Launch;

/**
 * The unit tests for {@link ObfuscationHelper}.
 */
public class ObfuscationHelperTest {

  private static final String MCP_NAME = "mcpName";
  private static final String SRG_NAME = "srgName";

  @BeforeClass
  public static void setUpBeforeClass() throws Exception {
    // Set the deobfuscation flag.
    Map<String, Object> blackboard = new HashMap<>();
    blackboard.put("fml.deobfuscatedEnvironment", true);
    Launch.blackboard = blackboard;

    // Populate MCP to SRG map with dummy data.
    Map<String, String> mcpToSrg = new HashMap<>();
    mcpToSrg.put(MCP_NAME, SRG_NAME);
    Deencapsulation.setField(ObfuscationHelper.class, "mcpToSrg", mcpToSrg);

    // Populate SRG to MCP map with dummy data.
    Map<String, String> srgToMcp = new HashMap<>();
    srgToMcp.put(SRG_NAME, MCP_NAME);
    Deencapsulation.setField(ObfuscationHelper.class, "srgToMcp", srgToMcp);
  }

  /**
   * Test that the MCP name is returned when passing an MCP name in a deobfuscated environment.
   */
  @Test
  public void testConvertName_deobfuscatedEnvironmentMcpName_mcpNameReturned() {
    // Set up test inputs.
    Deencapsulation.setField(ObfuscationHelper.class, "deobfuscated", true);

    // Call method under test.
    String convertedName = ObfuscationHelper.convertName(MCP_NAME);

    // Perform assertions.
    Assert.assertThat("The converted name does not match the expected name.", convertedName,
        CoreMatchers.is(MCP_NAME));
  }

  /**
   * Test that the MCP name is returned when passing an SRG name in a deobfuscated environment.
   */
  @Test
  public void testConvertName_deobfuscatedEnvironmentSrgName_mcpNameReturned() {
    // Set up test inputs.
    Deencapsulation.setField(ObfuscationHelper.class, "deobfuscated", true);

    // Call method under test.
    String convertedName = ObfuscationHelper.convertName(SRG_NAME);

    // Perform assertions.
    Assert.assertThat("The converted name does not match the expected name.", convertedName,
        CoreMatchers.is(MCP_NAME));
  }

  /**
   * Test that an IllegalArgumentException is thrown when passing an unmapped name in a deobfuscated
   * environment.
   */
  @Test(expected = IllegalArgumentException.class)
  public void testConvertName_deobfuscatedEnvironmentUnmappedName_illegalArgumentException() {
    // Set up test inputs.
    Deencapsulation.setField(ObfuscationHelper.class, "deobfuscated", true);

    // Call method under test.
    ObfuscationHelper.convertName("unmappedName");
  }

  /**
   * Test that the SRG name is returned when passing an MCP name in an obfuscated environment.
   */
  @Test
  public void testConvertName_obfuscatedEnvironmentMcgName_srgNameReturned() {
    // Set up test inputs.
    Deencapsulation.setField(ObfuscationHelper.class, "deobfuscated", false);

    // Call method under test.
    String convertedName = ObfuscationHelper.convertName(MCP_NAME);

    // Perform assertions.
    Assert.assertThat("The converted name does not match the expected name.", convertedName,
        CoreMatchers.is(SRG_NAME));
  }

  /**
   * Test that the SRG name is returned when passing an MCP name in an obfuscated environment.
   */
  @Test
  public void testConvertName_obfuscatedEnvironmentSrgName_srgNameReturned() {
    // Set up test inputs.
    Deencapsulation.setField(ObfuscationHelper.class, "deobfuscated", false);

    // Call method under test.
    String convertedName = ObfuscationHelper.convertName(SRG_NAME);

    // Perform assertions.
    Assert.assertThat("The converted name does not match the expected name.", convertedName,
        CoreMatchers.is(SRG_NAME));
  }

  /**
   * Test that an IllegalArgumentException is thrown when passing an unmapped name in an obfuscated
   * environment.
   */
  @Test(expected = IllegalArgumentException.class)
  public void testConvertName_obfuscatedEnvironmentUnmappedName_illegalArgumentException() {
    // Set up test inputs.
    Deencapsulation.setField(ObfuscationHelper.class, "deobfuscated", false);

    // Call method under test.
    ObfuscationHelper.convertName("unmappedName");
  }
}
