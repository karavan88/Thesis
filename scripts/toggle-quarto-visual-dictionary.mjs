#!/usr/bin/env node

import fs from "node:fs";
import path from "node:path";

const workspaceRoot = process.cwd();
const settingsPath = path.join(workspaceRoot, ".vscode", "settings.json");

function fail(message) {
  console.error(message);
  process.exit(1);
}

if (!fs.existsSync(settingsPath)) {
  fail(`settings file not found: ${settingsPath}`);
}

let raw;
try {
  raw = fs.readFileSync(settingsPath, "utf8");
} catch (err) {
  fail(`cannot read settings: ${err.message}`);
}

let settings;
try {
  settings = JSON.parse(raw);
} catch (err) {
  fail(`settings.json is not valid JSON: ${err.message}`);
}

const key = "quarto.visualEditor.spellingDictionary";
const current = settings[key];
const next = current === "ru_RU" ? "en_US" : "ru_RU";

settings["quarto.visualEditor.spelling"] = true;
settings[key] = next;

try {
  fs.writeFileSync(settingsPath, JSON.stringify(settings, null, 4) + "\n", "utf8");
} catch (err) {
  fail(`cannot write settings: ${err.message}`);
}

console.log(`Quarto visual dictionary switched: ${current ?? "(unset)"} -> ${next}`);
