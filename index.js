#!/usr/bin/env node
var pkg = require('./package.json');
var Main$foreign = require('./output/Main/foreign.js');
var Main = require('./output/Main');
Main$foreign.version = pkg.version;
Main.main();
