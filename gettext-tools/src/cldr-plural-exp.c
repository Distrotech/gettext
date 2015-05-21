/* Unicode CLDR plural rule parser and converter
   Copyright (C) 2015 Free Software Foundation, Inc.

   This file was written by Daiki Ueno <ueno@gnu.org>, 2015.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "unistr.h"
#include "xalloc.h"
#include <math.h>

#include "cldr-plural-exp.h"
#include "cldr-plural.h"

/* The grammar of Unicode CLDR plural rules is defined at:
   http://unicode.org/reports/tr35/tr35-numbers.html#Plural_rules_syntax

   This implementation only supports the "preferred" form, which
   doesn't support obsolete keywords "in", "is", "not", and "within".

   Unlike gettext, CLDR allows an unsigned decimal value as an
   operand, in addition to unsigned integers.  For simplicity, we
   treat decimal relations as if it has a constant truth value.

   The implementation is largely based on the idea of Michele Locati's
   cldr-to-gettext-plural-rules:
   https://github.com/mlocati/cldr-to-gettext-plural-rules  */

void
cldr_plural_range_free (struct cldr_plural_range_ty *range)
{
  if (range->start != range->end)
    free (range->start);
  free (range->end);
  free (range);
}

void
cldr_plural_range_list_free (struct cldr_plural_range_list_ty *ranges)
{
  while (ranges->nitems-- > 0)
    cldr_plural_range_free (ranges->items[ranges->nitems]);
  free (ranges->items);
  free (ranges);
}

void
cldr_plural_condition_free (struct cldr_plural_condition_ty *condition)
{
  if (condition->type == CLDR_PLURAL_CONDITION_AND
      || condition->type == CLDR_PLURAL_CONDITION_OR)
    {
      cldr_plural_condition_free (condition->value.conditions[0]);
      cldr_plural_condition_free (condition->value.conditions[1]);
    }
  else if (condition->type == CLDR_PLURAL_CONDITION_RELATION)
    cldr_plural_relation_free (condition->value.relation);
  free (condition);
}

void
cldr_plural_relation_free (struct cldr_plural_relation_ty *relation)
{
  free (relation->expression);
  cldr_plural_range_list_free (relation->ranges);
  free (relation);
}

static void
cldr_plural_rule_free (struct cldr_plural_rule_ty *rule)
{
  free (rule->name);
  cldr_plural_condition_free (rule->condition);
  free (rule);
}

static void
cldr_plural_rule_list_free (struct cldr_plural_rule_list_ty *rules)
{
  while (rules->nitems-- > 0)
    cldr_plural_rule_free (rules->items[rules->nitems]);
  free (rules->items);
  free (rules);
}

static struct cldr_plural_rule_list_ty *
cldr_plural_parse (const char *input)
{
  struct cldr_plural_parse_args arg;

  memset (&arg, 0, sizeof (struct cldr_plural_parse_args));
  arg.cp = input;
  arg.cp_end = input + strlen (input);
  arg.result = XMALLOC (struct cldr_plural_rule_list_ty);
  memset (arg.result, 0, sizeof (struct cldr_plural_rule_list_ty));

  if (yyparse (&arg) != 0)
    return NULL;

  return arg.result;
}

#define OPERAND_ZERO_P(o)                               \
    (((o)->type == CLDR_PLURAL_OPERAND_INTEGER          \
      && (o)->value.ival == 0)                          \
    || ((o)->type == CLDR_PLURAL_OPERAND_DECIMAL        \
        && (o)->value.dval.d == 0))

static enum cldr_plural_condition
eval_relation (struct cldr_plural_relation_ty *relation)
{
  switch (relation->expression->operand)
    {
    case 'n': case 'i':
      {
        /* Coerce decimal values in ranges into integers.  */
        size_t i;
        for (i = 0; i < relation->ranges->nitems; i++)
          {
            struct cldr_plural_range_ty *range = relation->ranges->items[i];
            if (range->start->type == CLDR_PLURAL_OPERAND_DECIMAL)
              {
                range->start->type = CLDR_PLURAL_OPERAND_INTEGER;
                range->start->value.ival = ceil (range->start->value.dval.d);
              }
            if (range->end->type == CLDR_PLURAL_OPERAND_DECIMAL)
              {
                range->end->type = CLDR_PLURAL_OPERAND_INTEGER;
                range->end->value.ival = floor (range->end->value.dval.d);
              }
          }
        relation->expression->operand = 'i';
      }
      break;
    case 'f': case 't':
    case 'v': case 'w':
      {
        /* Since plural expression in gettext only supports unsigned
           integer, turn relations whose operand is either 'f', 't',
           'v', or 'w' into a constant truth value.  */
        /* FIXME: check mod?  */
        size_t i;
        for (i = 0; i < relation->ranges->nitems; i++)
          {
            struct cldr_plural_range_ty *range = relation->ranges->items[i];
            if ((relation->type == CLDR_PLURAL_RELATION_EQUAL
                 && (!OPERAND_ZERO_P (range->start)
                     || !OPERAND_ZERO_P (range->end)))
                || (relation->type == CLDR_PLURAL_RELATION_NOT_EQUAL
                    && (OPERAND_ZERO_P (range->start)
                        || OPERAND_ZERO_P (range->end))))
              return CLDR_PLURAL_CONDITION_FALSE;
          }
        return CLDR_PLURAL_CONDITION_TRUE;
      }
      break;
    }
  return CLDR_PLURAL_CONDITION_RELATION;
}

static void
eval_condition (struct cldr_plural_condition_ty *condition)
{
  if (condition->type == CLDR_PLURAL_CONDITION_AND)
    {
      eval_condition (condition->value.conditions[0]);
      eval_condition (condition->value.conditions[1]);

      if (condition->value.conditions[0]->type
          == CLDR_PLURAL_CONDITION_FALSE
          || condition->value.conditions[1]->type
          == CLDR_PLURAL_CONDITION_FALSE)
        {
          cldr_plural_condition_free (condition->value.conditions[0]);
          cldr_plural_condition_free (condition->value.conditions[1]);
          condition->type = CLDR_PLURAL_CONDITION_FALSE;
        }
      else if (condition->value.conditions[0]->type
               == CLDR_PLURAL_CONDITION_TRUE
               && condition->value.conditions[1]->type
               == CLDR_PLURAL_CONDITION_TRUE)
        {
          cldr_plural_condition_free (condition->value.conditions[0]);
          cldr_plural_condition_free (condition->value.conditions[1]);
          condition->type = CLDR_PLURAL_CONDITION_TRUE;
        }
      else if (condition->value.conditions[0]->type
               == CLDR_PLURAL_CONDITION_TRUE)
        {
          struct cldr_plural_condition_ty *original
            = condition->value.conditions[1];
          cldr_plural_condition_free (condition->value.conditions[0]);
          condition->type = condition->value.conditions[1]->type;
          condition->value = condition->value.conditions[1]->value;
          free (original);
        }
      else if (condition->value.conditions[1]->type
               == CLDR_PLURAL_CONDITION_TRUE)
        {
          struct cldr_plural_condition_ty *original
            = condition->value.conditions[0];
          cldr_plural_condition_free (condition->value.conditions[1]);
          condition->type = condition->value.conditions[0]->type;
          condition->value = condition->value.conditions[0]->value;
          free (original);
        }
    }
  else if (condition->type == CLDR_PLURAL_CONDITION_OR)
    {
      eval_condition (condition->value.conditions[0]);
      eval_condition (condition->value.conditions[1]);

      if (condition->value.conditions[0]->type
          == CLDR_PLURAL_CONDITION_TRUE
          || condition->value.conditions[1]->type
          == CLDR_PLURAL_CONDITION_TRUE)
        {
          cldr_plural_condition_free (condition->value.conditions[0]);
          cldr_plural_condition_free (condition->value.conditions[1]);
          condition->type = CLDR_PLURAL_CONDITION_TRUE;
        }
      else if (condition->value.conditions[0]->type
               == CLDR_PLURAL_CONDITION_FALSE
               && condition->value.conditions[1]->type
               == CLDR_PLURAL_CONDITION_FALSE)
        {
          cldr_plural_condition_free (condition->value.conditions[0]);
          cldr_plural_condition_free (condition->value.conditions[1]);
          condition->type = CLDR_PLURAL_CONDITION_FALSE;
        }
      else if (condition->value.conditions[0]->type
               == CLDR_PLURAL_CONDITION_FALSE)
        {
          struct cldr_plural_condition_ty *original
            = condition->value.conditions[1];
          cldr_plural_condition_free (condition->value.conditions[0]);
          condition->type = condition->value.conditions[1]->type;
          condition->value = condition->value.conditions[1]->value;
          free (original);
        }
      else if (condition->value.conditions[1]->type
               == CLDR_PLURAL_CONDITION_FALSE)
        {
          struct cldr_plural_condition_ty *original
            = condition->value.conditions[0];
          cldr_plural_condition_free (condition->value.conditions[1]);
          condition->type = condition->value.conditions[0]->type;
          condition->value = condition->value.conditions[0]->value;
          free (original);
        }
    }
  else
    {
      enum cldr_plural_condition value =
        eval_relation (condition->value.relation);
      if (value == CLDR_PLURAL_CONDITION_TRUE
          || value == CLDR_PLURAL_CONDITION_FALSE)
        {
          cldr_plural_relation_free (condition->value.relation);
          condition->type = value;
        }
    }
}

#define MAX(a,b) ((a) > (b) ? (a) : (b))

static int
find_largest_modulus (struct cldr_plural_condition_ty *condition)
{
  if (condition->type == CLDR_PLURAL_CONDITION_AND
      || condition->type == CLDR_PLURAL_CONDITION_OR)
    {
      int modulus0 =
        find_largest_modulus (condition->value.conditions[0]);
      int modulus1 =
        find_largest_modulus (condition->value.conditions[1]);
      return MAX (modulus0, modulus1);
    }
  else if (condition->type == CLDR_PLURAL_CONDITION_RELATION)
    return condition->value.relation->expression->mod;
  else
    return 0;
}

static bool
apply_condition (struct cldr_plural_condition_ty *condition, int value)
{
  if (condition->type == CLDR_PLURAL_CONDITION_AND)
    return apply_condition (condition->value.conditions[0], value)
      && apply_condition (condition->value.conditions[1], value);
  else if (condition->type == CLDR_PLURAL_CONDITION_OR)
    return apply_condition (condition->value.conditions[0], value)
      || apply_condition (condition->value.conditions[1], value);
  else if (condition->type == CLDR_PLURAL_CONDITION_RELATION)
    {
      struct cldr_plural_relation_ty *relation
        = condition->value.relation;
      int number = value;
      size_t i;

      if (relation->expression->mod > 0)
        number %= relation->expression->mod;
      for (i = 0; i < relation->ranges->nitems; i++)
        {
          struct cldr_plural_range_ty *range = relation->ranges->items[i];
          if (range->start->value.ival <= number
              && number <= range->end->value.ival)
            return relation->type == CLDR_PLURAL_RELATION_EQUAL;
        }
      return relation->type != CLDR_PLURAL_RELATION_EQUAL;
    }
  return false;
}

static void
print_expression (struct cldr_plural_expression_ty *expression, bool space)
{
  if (expression->mod == 0)
    printf ("n");
  else
    printf (space ? "n %% %d" : "n%%%d", expression->mod);
}

static void
print_relation (struct cldr_plural_relation_ty *relation,
                enum cldr_plural_condition parent, bool space)
{
  if (relation->type == CLDR_PLURAL_RELATION_EQUAL)
    {
      size_t i;
      if (parent == CLDR_PLURAL_CONDITION_AND
          && relation->ranges->nitems > 1)
        putchar ('(');
      for (i = 0; i < relation->ranges->nitems; i++)
        {
          struct cldr_plural_range_ty *range = relation->ranges->items[i];
          if (i > 0)
            printf (" || ");
          if (range->start->value.ival == range->end->value.ival)
            {
              print_expression (relation->expression, space);
              printf (space && relation->ranges->nitems == 1
                      ? " == %d" : "==%d",
                      range->start->value.ival);
            }
          else if (range->start->value.ival == 0)
            {
              print_expression (relation->expression, false);
              printf ("<=%d", range->end->value.ival);
            }
          else
            {
              if (parent == CLDR_PLURAL_CONDITION_OR
                  || relation->ranges->nitems > 1)
                putchar ('(');
              print_expression (relation->expression, false);
              printf (">=%d", range->start->value.ival);
              printf (" && ");
              print_expression (relation->expression, false);
              printf ("<=%d", range->end->value.ival);
              if (parent == CLDR_PLURAL_CONDITION_OR
                  || relation->ranges->nitems > 1)
                putchar (')');
            }
        }
      if (parent == CLDR_PLURAL_CONDITION_AND
          && relation->ranges->nitems > 1)
        putchar (')');
    }
  else
    {
      size_t i;
      if (parent == CLDR_PLURAL_CONDITION_OR
          && relation->ranges->nitems > 1)
        putchar ('(');
      for (i = 0; i < relation->ranges->nitems; i++)
        {
          struct cldr_plural_range_ty *range = relation->ranges->items[i];
         if (i > 0)
            printf (" && ");
          if (range->start->value.ival == range->end->value.ival)
            {
              print_expression (relation->expression, space);
              printf (space && relation->ranges->nitems == 1
                      ? " != %d" : "!=%d", range->start->value.ival);
            }
          else if (range->start->value.ival == 0)
            {
              print_expression (relation->expression, false);
              printf (">%d", range->end->value.ival);
            }
          else
            {
              if (parent == CLDR_PLURAL_CONDITION_AND
                  || relation->ranges->nitems > 1)
                putchar ('(');
              print_expression (relation->expression, false);
              printf ("<%d", range->start->value.ival);
              printf (" || ");
              print_expression (relation->expression, false);
              printf (">%d", range->end->value.ival);
              if (parent == CLDR_PLURAL_CONDITION_AND
                  || relation->ranges->nitems > 1)
                putchar (')');
            }
        }
      if (parent == CLDR_PLURAL_CONDITION_OR
          && relation->ranges->nitems > 1)
        putchar (')');
    }
}

static bool
print_condition (struct cldr_plural_condition_ty *condition,
                 enum cldr_plural_condition parent, bool space)
{
  if (condition->type == CLDR_PLURAL_CONDITION_AND)
    {
      if (parent == CLDR_PLURAL_CONDITION_OR)
        putchar ('(');
      print_condition (condition->value.conditions[0],
                       CLDR_PLURAL_CONDITION_AND, false);
      printf (" && ");
      print_condition (condition->value.conditions[1],
                       CLDR_PLURAL_CONDITION_AND, false);
      if (parent == CLDR_PLURAL_CONDITION_OR)
        putchar (')');
      return true;
    }
  else if (condition->type == CLDR_PLURAL_CONDITION_OR)
    {
      if (parent == CLDR_PLURAL_CONDITION_AND)
        putchar ('(');
      print_condition (condition->value.conditions[0],
                       CLDR_PLURAL_CONDITION_OR, false);
      printf (" || ");
      print_condition (condition->value.conditions[1],
                       CLDR_PLURAL_CONDITION_OR, false);
      if (parent == CLDR_PLURAL_CONDITION_AND)
        putchar (')');
      return true;
    }
  else if (condition->type == CLDR_PLURAL_CONDITION_RELATION)
    {
      print_relation (condition->value.relation, parent, space);
      return true;
    }
  return false;
}

#define RULE_PRINTABLE_P(r)                                     \
  ((r)->condition->type != CLDR_PLURAL_CONDITION_TRUE           \
   && (r)->condition->type != CLDR_PLURAL_CONDITION_FALSE)

/* Convert n == N into n != N.  */
static bool
print_condition_negation (struct cldr_plural_condition_ty *condition)
{
  if (condition->type == CLDR_PLURAL_CONDITION_RELATION
      && condition->value.relation->type == CLDR_PLURAL_RELATION_EQUAL
      && condition->value.relation->ranges->nitems == 1
      && condition->value.relation->ranges->items[0]->start
      == condition->value.relation->ranges->items[0]->end)
    {
      printf ("nplurals=2; plural=(n != %d);\n",
              condition->value.relation->ranges->items[0]->start->value.ival);
      return true;
    }
  return false;
}

/* Convert n == 0,...,N into n > N.  */
static bool
print_condition_greater (struct cldr_plural_condition_ty *condition)
{
  if (condition->type == CLDR_PLURAL_CONDITION_RELATION
      && condition->value.relation->type == CLDR_PLURAL_RELATION_EQUAL)
    {
      int last = -1;
      size_t i;
      for (i = 0; i < condition->value.relation->ranges->nitems; i++)
        {
          struct cldr_plural_range_ty *range =
            condition->value.relation->ranges->items[i];
          if (range->start->type != CLDR_PLURAL_OPERAND_INTEGER
              || range->end->type != CLDR_PLURAL_OPERAND_INTEGER
              || range->start->value.ival != last + 1)
            break;
          last = range->end->value.ival;
        }
      if (i == condition->value.relation->ranges->nitems)
        {
          struct cldr_plural_range_ty *range =
            condition->value.relation->ranges->items[i - 1];
          printf ("nplurals=2; plural=(n > %d);\n",
                  range->end->value.ival);
          return true;
        }
    }
  return false;
}

typedef bool (*print_condition_function_ty) (struct cldr_plural_condition_ty *);
static print_condition_function_ty print_condition_functions[] =
  {
    print_condition_negation,
    print_condition_greater
  };

#define SIZEOF(a) (sizeof(a) / sizeof(a[0]))

static void
process_rule_list (struct cldr_plural_rule_list_ty *rules)
{
  size_t i;
  size_t count;
  size_t nplurals;
  int modulus_max = 0;

  /* Prune trivial conditions.  */
  for (i = 0; i < rules->nitems; i++)
    {
      struct cldr_plural_rule_ty *rule = rules->items[i];
      eval_condition (rule->condition);
    }

  /* Find the largest modulus.  */
  for (i = 0; i < rules->nitems; i++)
    {
      struct cldr_plural_rule_ty *rule = rules->items[i];
      int modulus = find_largest_modulus (rule->condition);
      if (modulus > modulus_max)
        modulus_max = modulus;
    }

  if (modulus_max > 0)
    {
      bool *values = XNMALLOC (modulus_max, bool);

      memset (values, 0, sizeof (bool) * modulus_max);
      for (i = 0; i < rules->nitems; i++)
        {
          struct cldr_plural_rule_ty *rule = rules->items[i];
          int j;

          for (j = 0; j < modulus_max; j++)
            {
              bool result = apply_condition (rule->condition, j + 1);
              if (result)
                values[j] = true;
            }

          /* Check if all bits are set.  Then we can omit one more rule.  */
          for (j = 0; j < modulus_max; j++)
            if (values[j] == false)
              break;
          if (j == modulus_max)
            break;
        }

      free (values);

      while (i < rules->nitems)
        cldr_plural_rule_free (rules->items[--rules->nitems]);
    }

  for (i = 0, nplurals = 1; i < rules->nitems; i++)
    if (RULE_PRINTABLE_P (rules->items[i]))
      nplurals++;

  /* Special case when rules is empty.  */
  if (nplurals == 1)
    {
      printf ("nplurals=1; plural=0;\n");
      return;
    }

  /* If we have only one printable rule, apply some heuristics.  */
  if (nplurals == 2)
    {
      struct cldr_plural_condition_ty *condition;
      size_t j;

      for (j = 0; j < rules->nitems; j++)
        if (RULE_PRINTABLE_P (rules->items[j]))
          break;

      condition = rules->items[j]->condition;
      for (j = 0; j < SIZEOF (print_condition_functions); j++)
        if (print_condition_functions[j] (condition))
          return;
    }

  /* If there are more printable rules, build a tertiary operator.  */
  printf ("nplurals=%zu; plural=(", nplurals);
  for (i = 0, count = 0; i < rules->nitems; i++)
    {
      struct cldr_plural_rule_ty *rule = rules->items[i];
      if (print_condition (rule->condition,
                           CLDR_PLURAL_CONDITION_FALSE,
                           nplurals == 2)
          && rules->nitems > 1)
        {
          bool printable_left = false;
          size_t j;

          for (j = i + 1; j < rules->nitems; j++)
            if (RULE_PRINTABLE_P (rules->items[j]))
              printable_left = true;

          if (i < rules->nitems - 1 && printable_left)
            printf (" ? %zu : ", count++);
        }
    }
  if (rules->nitems > 1)
    printf (" ? %zu : %zu", count, count + 1);
  printf (");\n");
}

int
main (int argc, char **argv)
{
  char *line = NULL;
  size_t line_size = 0;

  for (;;)
    {
      int line_len;
      struct cldr_plural_rule_list_ty *result;

      line_len = getline (&line, &line_size, stdin);
      if (line_len < 0)
        break;
      if (line_len > 0 && line[line_len - 1] == '\n')
        line[--line_len] = '\0';

      result = cldr_plural_parse (line);
      if (result)
        {
          process_rule_list (result);
          cldr_plural_rule_list_free (result);
        }
    }

  free (line);
  return 0;
}
