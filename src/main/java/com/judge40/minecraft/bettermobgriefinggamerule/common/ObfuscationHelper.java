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

package com.judge40.minecraft.bettermobgriefinggamerule.common;

import net.minecraft.launchwrapper.Launch;

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

/**
 * A helper to assist in conversion between SRG and MCP method and fields names based on whether or
 * not the environment is obfuscated.
 */
public class ObfuscationHelper {

  private static final Map<String, String> srgToMcp = new HashMap<>();
  private static final Map<String, String> mcpToSrg = new HashMap<>();

  static {
    srgToMcp.put("field_146075_bs", "field_146075_bs");
    srgToMcp.put("field_71561_b", "commandSet");
    srgToMcp.put("func_180658_a", "onFallenUpon");
    srgToMcp.put("func_70619_bc", "updateAITasks");
    srgToMcp.put("func_70626_be", "updateEntityActionState");
    srgToMcp.put("func_70636_d", "onLivingUpdate");
    srgToMcp.put("func_70972_a", "destroyBlocksInAABB");
    srgToMcp.put("func_75246_d", "updateTask");
    srgToMcp.put("func_82766_b", "getGameRuleBooleanValue");

    for (Entry<String, String> entry : srgToMcp.entrySet()) {
      mcpToSrg.put(entry.getValue(), entry.getKey());
    }
  }

  /**
   * Converts the given name to either the SRG or MCP name depending on whether or not the
   * environment is obfuscated.
   * 
   * @param name The name to convert between SRG and MCP.
   * @return The converted value if a mapping was found. The returned value will be the same as the
   *         input when the input is valid for the current environment.
   * @throws IllegalArgumentException Thrown when no mapping exists for the given name.
   */
  public static String convertName(String name) {
    String convertedName = null;

    if (name != null) {
      // If in a deobfuscated environment then attempt conversion from SRG to MCP, otherwise
      // attempt conversion from MCP to SRG.
      boolean deobfuscated = (boolean) Launch.blackboard.get("fml.deobfuscatedEnvironment");

      if (deobfuscated) {
        convertedName = srgToMcp.getOrDefault(name, mcpToSrg.containsKey(name) ? name : null);
      } else {
        convertedName = mcpToSrg.getOrDefault(name, srgToMcp.containsKey(name) ? name : null);
      }
    }

    // If there is no mapping available then throw an illegal argument exception.
    if (convertedName == null) {
      String message = "No mapping exists to convert the name '%s' between SRG and MCP names.";
      throw new IllegalArgumentException(String.format(message, name));
    }

    return convertedName;
  }
}
