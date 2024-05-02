#!/usr/bin/env node
import pkg from './package.json' assert { type: 'json' };
import * as Main from './output/Main/index.js';
global.version = pkg.version;
Main.main();
