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

package com.judge40.minecraft.bettermobgriefinggamerule.common.command;

import static org.hamcrest.CoreMatchers.hasItems;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.judge40.minecraft.bettermobgriefinggamerule.common.MobGriefingValue;
import com.mojang.brigadier.StringReader;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import com.mojang.brigadier.suggestion.Suggestion;
import com.mojang.brigadier.suggestion.Suggestions;
import com.mojang.brigadier.suggestion.SuggestionsBuilder;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

class BetterMobGriefingArgumentTest {

  private BetterMobGriefingArgument argument;

  @BeforeEach
  void setUp() {
    argument = new BetterMobGriefingArgument();
  }

  @ParameterizedTest(name = "Should parse when input is {0}")
  @CsvSource({"false, FALSE", "TRUE, TRUE", "Inherit, INHERIT"})
  void shouldParseWhenValid(String input, MobGriefingValue expected) throws CommandSyntaxException {
    // Given.
    StringReader reader = new StringReader(input);

    // When.
    MobGriefingValue parsedValue = argument.parse(reader);

    // Then.
    assertThat("Unexpected parsed value.", parsedValue, is(expected));
  }

  @Test
  void shouldNotParseWhenInvalid() {
    // Given.
    StringReader reader = new StringReader("invalid");

    // When.
    assertThrows(CommandSyntaxException.class, () -> argument.parse(reader));
  }

  @ParameterizedTest(name = "Should list suggestions when input is {0}")
  @CsvSource({"FALSE, false", "tRUE, true", "Inherit, inherit", "fa, false", "tru, true",
      "inhe, inherit"})
  void shouldListSuggestionWhenPartialMatch(String input, String expected)
      throws ExecutionException, InterruptedException {
    // Given.
    SuggestionsBuilder builder = new SuggestionsBuilder(input, 0);

    // When.
    CompletableFuture<Suggestions> suggestions = argument.listSuggestions(null, builder);

    // Then.
    List<Suggestion> suggestionList = suggestions.get().getList();
    assertThat("Unexpected number of suggestions.", suggestionList.size(), is(1));
    assertThat("Unexpected suggestion.", suggestionList.get(0).getText(), is(expected));
  }

  @ParameterizedTest(name = "Should not list suggestions when input is {0}")
  @CsvSource({"false, false", "true, true", "inherit, inherit"})
  void shouldNotListSuggestionWhenFullMatch(String input, String expected)
      throws ExecutionException, InterruptedException {
    // Given.
    SuggestionsBuilder builder = new SuggestionsBuilder(input, 0);

    // When.
    CompletableFuture<Suggestions> suggestions = argument.listSuggestions(null, builder);

    // Then.
    List<Suggestion> suggestionList = suggestions.get().getList();
    assertThat("Unexpected number of suggestions.", suggestionList.size(), is(0));
  }

  @Test
  void shouldListSuggestionsWhenEmpty() throws ExecutionException, InterruptedException {
    // Given.
    SuggestionsBuilder builder = new SuggestionsBuilder("", 0);

    // When.
    CompletableFuture<Suggestions> suggestions = argument.listSuggestions(null, builder);

    // Then.
    List<Suggestion> suggestionList = suggestions.get().getList();
    assertThat("Unexpected number of suggestions.", suggestionList.size(), is(3));
    assertThat("Unexpected suggestion.", suggestionList.get(0).getText(), is("false"));
    assertThat("Unexpected suggestion.", suggestionList.get(1).getText(), is("inherit"));
    assertThat("Unexpected suggestion.", suggestionList.get(2).getText(), is("true"));
  }

  @Test
  void shouldGiveExamples() {
    // When.
    Collection<String> examples = argument.getExamples();

    // Then.
    assertThat("Unexpected number of examples.", examples.size(), is(3));
    assertThat("Unexpected examples.", examples, hasItems("false", "true", "inherit"));
  }
}
