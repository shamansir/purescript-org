import { parser } from "instaparse";
import fs from 'node:fs';


// const testDir = './test/examples/org-test/';
// const orgSource = '04l-formatting-properties-and-keywords-ext';

const testDir = './test/examples/';
const orgSource = 'test';

fs.readFile('./test/ParseUtils/org.ebnf', 'utf8', (err, grammar) => {
  if (err) {
    console.error(err);
    return;
  }

  fs.readFile(testDir + orgSource + '.org', 'utf8', (err, orgText) => {
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
