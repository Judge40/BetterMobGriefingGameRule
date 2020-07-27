/*
 * Better mobGriefing GameRule Copyright (c) 2020 Judge40
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

package com.judge40.minecraft.bettermobgriefinggamerule.client.gui.widget;

import java.util.Collections;
import java.util.List;
import javax.annotation.Nonnull;
import net.minecraft.client.gui.FontRenderer;
import net.minecraft.client.gui.IGuiEventListener;
import net.minecraft.client.resources.I18n;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

@OnlyIn(Dist.CLIENT)
public class ConfigCategoryEntry extends AbstractEntry {

  private final FontRenderer fontRenderer;
  private final int parentWidth;

  private final String labelText;
  private final int labelWidth;

  ConfigCategoryEntry(FontRenderer fontRenderer, int parentWidth, String labelKey) {
    this.fontRenderer = fontRenderer;
    this.parentWidth = parentWidth;

    labelText = I18n.format(labelKey);
    labelWidth = fontRenderer.getStringWidth(labelText);
  }

  @Override
  public void render(int p_render_1_, int p_render_2_, int p_render_3_, int p_render_4_,
      int p_render_5_, int p_render_6_, int p_render_7_, boolean p_render_8_, float p_render_9_) {
    float x = (float) (parentWidth / 2 - labelWidth / 2);
    float y = (float) (p_render_2_ + p_render_5_ - 10);
    fontRenderer.drawString(labelText, x, y, 16777215);
  }

  @Override
  public boolean changeFocus(boolean p_changeFocus_1_) {
    return false;
  }

  @Nonnull
  @Override
  public List<? extends IGuiEventListener> children() {
    return Collections.emptyList();
  }
}
