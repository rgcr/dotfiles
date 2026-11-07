// Record only Pi's exact session reference for manual Herdr snapshots.
// No transcript content, credentials, or automatic resume actions.
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { spawnSync } from "node:child_process";

export default function(pi: any) {
  const root = process.env.HERDR_SESSION_DIR || path.join(os.homedir(), ".local/state/herdr-app-sessions");
  const dir = path.join(root, "agents");
  const file = path.join(dir, `${process.pid}.json`);
  const birth = spawnSync("ps", ["-p", String(process.pid), "-o", "lstart="], {encoding:"utf8", env:{...process.env, TZ:"UTC", LC_ALL:"C"}}).stdout?.trim();
  function record(ctx: any) {
    const ref = ctx.sessionManager.getSessionFile();
    if (!birth || !ref) return;
    fs.mkdirSync(dir, {recursive:true, mode:0o700});
    const temporary = file + ".new";
    fs.writeFileSync(temporary, JSON.stringify({agent:"pi", ref, pid:process.pid, birth, source:"Pi session hook"}), {mode:0o600});
    fs.renameSync(temporary, file);
  }
  for (const event of ["session_start", "session_tree", "session_info_changed", "agent_end"])
    pi.on(event, (_event: any, ctx: any) => { try { record(ctx); } catch {} });
}
