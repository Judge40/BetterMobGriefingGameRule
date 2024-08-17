/*
 * Better mobGriefing GameRule Copyright (c) 2022 Judge40
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

package net.minecraftforge.fml.event.config;

import net.minecraftforge.fml.config.IConfigEvent.ConfigConfig;
import net.minecraftforge.fml.config.ModConfig;

/**
 * Test stub for Loading event as {@link ConfigConfig} causes issues with mocking.
 */
public class ModConfigEvent {

  /**
   * A test stuv of the Loading event.
   */
  public static class Loading extends ModConfigEvent {

    private final ModConfig config;

    public Loading(final ModConfig config) {
      this.config = config;
    }

    public ModConfig getConfig() {
      return config;
    }
  }
}
