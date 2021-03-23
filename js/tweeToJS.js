#!/usr/bin/env node

// Important note, this completely ignores links, its only meant
// to translate code with linear control flow.
let fs = require('fs');

let macro_re = /^( *)<</;
// end of control structures, converted to } /* \1 */
let macro_end_re = /(if|switch|for)>>/;
// macros with no arguments simple replacements
let simple_macros_re  = /(else|break|continue|default|silently)|(\w+)>>/;
// control structures
let control_macros_re = /(if|for|elseif|switch|case) +(.*)>>/;
let print_re = /( *)(?:<<(?:=|print) (.*?)>>)|(/
let comment_re = /\/\\*(.*?)\\*\//;
const parse = (() => {
  const parseMap = Object.freeze({
    /* eslint-disable quote-props */
    // Story $variable sigil-prefix.
    '$'     : 'State.variables.',
    // Temporary _variable sigil-prefix.
    '_'     : 'State.temporary.',
    // Assignment operators.
    'to'    : '=',
    // Equality operators.
    'eq'    : '==',
    'neq'   : '!=',
    'is'    : '===',
    'isnot' : '!==',
    // Relational operators.
    'gt'    : '>',
    'gte'   : '>=',
    'lt'    : '<',
    'lte'   : '<=',
    // Logical operators.
    'and'   : '&&',
    'or'    : '||',
    // Unary operators.
    'not'   : '!',
    'def'   : '"undefined" !== typeof',
    'ndef'  : '"undefined" === typeof'
    /* eslint-enable quote-props */
  });
  const parseRe = new RegExp([
    '(""|\'\')',                                          // 1=Empty quotes
    '("(?:\\\\.|[^"\\\\])+")',                            // 2=Double quoted, non-empty
    "('(?:\\\\.|[^'\\\\])+')",                            // 3=Single quoted, non-empty
    '([=+\\-*\\/%<>&\\|\\^~!?:,;\\(\\)\\[\\]{}]+)',       // 4=Operator delimiters
    '([^"\'=+\\-*\\/%<>&\\|\\^~!?:,;\\(\\)\\[\\]{}\\s]+)' // 5=Barewords
  ].join('|'), 'g');
  const varTest = new RegExp(`^${Patterns.variable}`);

  function parse(rawCodeString) {
    if (parseRe.lastIndex !== 0) {
      throw new RangeError('Scripting.parse last index is non-zero at start');
    }

    let code  = rawCodeString;
    let match;

    while ((match = parseRe.exec(code)) !== null) {
      // no-op: Empty quotes | Double quoted | Single quoted | Operator delimiters

      /*
        Barewords.
      */
      if (match[5]) {
        let token = match[5];

        /*
          If the token is simply a dollar-sign or underscore, then it's either
          just the raw character or, probably, a function alias, so skip it.
        */
        if (token === '$' || token === '_') {
          continue;
        }

        /*
          If the token is a story $variable or temporary _variable, reset it
                                                to just its sigil—for later mapping.
        */
        else if (varTest.test(token)) {
          token = token[0];
        }

        /*
          If the token is `is`, check to see if it's followed by `not`, if so,
          convert them into the `isnot` operator.

          NOTE: This is a safety feature, since `$a is not $b` probably sounds
          reasonable to most users.
        */
        else if (token === 'is') {
          const start = parseRe.lastIndex;
          const part  = code.slice(start);

          if (/^\s+not\b/.test(part)) {
            code = code.splice(start, part.search(/\S/));
            token = 'isnot';
          }
        }

        /*
          If the finalized token has a mapping, replace it within the code string
          with its counterpart.

          NOTE: We must use `parseMap.hasOwnProperty(token)` here, rather than
          simply using something like `parseMap[token]`, otherwise tokens which
          match properties from the prototype chain will cause shenanigans.
        */
        if (parseMap.hasOwnProperty(token)) {
          code = code.splice(
            match.index,    // starting index
            token.length,   // replace how many
            parseMap[token] // replacement string
          );
          parseRe.lastIndex += parseMap[token].length - token.length;
        }
      }
    }

    return code;
  }

  return parse;
})();
