import { parser } from "instaparse";
import fs from 'node:fs';


const testDir = './test/examples/org-test/';
// const orgSource = '04h-formatting-properties-and-keywords';

// const testDir = './test/examples/';
// const orgSource = 'test';

function _parseOrgWithEbnf(grammarContents) {
  return function(orgText) {
    const result = parser(grammarContents)(orgText);
    return JSON.stringify({ data : result });
  };
}

function _writeEbnfJsonFor(sourceSlug) {
    return function() {
      fs.readFile('./src/Data/Text/Format/Org/Parse/org.ebnf', 'utf8', (err, grammar) => {
        if (err) {
          console.error(err);
          return;
        }

        fs.readFile(testDir + sourceSlug + '.org', 'utf8', (err, orgText) => {
          if (err) {
              console.error(err);
              return;
            }
          const result = parser(grammar)(orgText);
          const resultJson = JSON.stringify({ data : result });
          console.log(result);

          fs.writeFile(testDir + orgSource + '.ebnf.json', resultJson, 'utf8', (err) =>
          {
              console.log(err);
          });
        });

      });
    }
}

export const parseOrgWithEbnf = _parseOrgWithEbnf;
export const writeEbnfJsonFor = _writeEbnfJsonFor;