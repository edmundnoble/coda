import * as assert from 'assert';
import * as vscode from 'vscode';
import * as myExtension from '../../code/extension';
suite("extension tests", () => {
    test("missing", () => {
        assert.equal(-1, [1, 2, 3].indexOf(5));
        assert.equal(-1, [1, 2, 3].indexOf(0));
    });
});
