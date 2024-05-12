#!/usr/bin/env node
import pkg from './package.json' assert { type: 'json' };
import * as Main from './dist/index.js';
global.version = pkg.version;
Main.main();
