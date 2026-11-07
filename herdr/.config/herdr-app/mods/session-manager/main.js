import { View, div } from "gpui-kit";
import { v_flex, h_flex } from "gpui-base";
import { exec, theme, props, closeOverlay } from "herdr";

export default class SessionManager extends View {
  init(_props, cx) {
    this.busy = false;
    this.saved = null;
    this.snapshots = [];
    this.selecting = false;
    this.preview = null;
    this.details = false;
    this.message = "";
    this.error = "";
    this.perform("status", cx);
  }
  perform(action, cx) {
    if (this.busy) return;
    this.busy = true;
    this.error = "";
    this.message = action === "save" ? "Saving…" : action === "apply" ? "Restoring…" : "Checking…";
    cx.notify();
    cx.spawn(async cx => {
      try {
        const args = action === "apply" ? ["restore", "--apply"] : [action];
        if ((action === "restore" || action === "apply") && this.saved?.id)
          args.push("--snapshot", this.saved.id);
        const result = await exec("herdr-session", args);
        const data = JSON.parse(result.stdout);
        if (result.code !== 0 || data.error) throw new Error(data.error || result.stderr);
        if (action === "status" || action === "save") {
          this.snapshots = data.snapshots || [];
          this.saved = this.snapshots[0] || data;
          this.selecting = false;
          this.preview = null;
          this.message = action === "save" ? "Saved just now" : "";
        } else {
          this.preview = action === "restore" ? data : null;
          const pending = (data.actions || []).some(x => x.action !== "keep-live");
          this.message = action === "apply" ? data.message : pending ? "Ready to restore" : data.actions?.length ? "Everything is still running. Nothing to restore." : data.message;
        }
      } catch (error) { this.error = String(error).replace(/^Error: /, ""); this.message = ""; this.preview = null; }
      this.busy = false;
      cx.notify();
    });
  }
  render() {
    const t = theme();
    const d = this.saved;
    const missing = d?.unresolved || [];
    const pending = (this.preview?.actions || []).filter(x => x.action !== "keep-live");
    const allPi = missing.length > 0 && missing.every(x => x.agent === "pi");
    const button = (label, action, primary = false) => div().px(14).py(10).rounded(8)
      .bg(primary ? t.accent : t.raised).text_color(this.busy ? t.dim : primary ? t.accent_foreground : t.foreground)
      .cursor_pointer().on_click((_e, cx) => this.perform(action, cx)).child(label);
    const timestamp = value => {
      const date = new Date(value);
      return `${date.getMonth() + 1}/${date.getDate()} · ${String(date.getHours()).padStart(2,"0")}:${String(date.getMinutes()).padStart(2,"0")}:${String(date.getSeconds()).padStart(2,"0")}`;
    };
    const when = d?.saved_at ? timestamp(d.saved_at) : "";
    return v_flex().p(24).gap(18).bg(t.popover).text_color(t.foreground).text_size(13)
      .child(h_flex().items_center()
        .child(div().flex_1().font_semibold().text_size(18).child("Session"))
        .child(div().px(8).py(4).rounded(6).cursor_pointer().text_color(t.muted)
          .hover(el => el.bg(t.hover)).on_click(() => closeOverlay(props().overlay)).child("Close")))
      .child(v_flex().p(16).gap(8).rounded(10).bg(t.raised)
        .child(h_flex().items_center()
          .child(div().flex_1().text_color(t.muted).text_size(12).child(d?.saved_at ? "Selected session" : "No saved session"))
          .child(div().text_color(t.dim).text_size(12).child(when)))
        .when(d?.spaces !== undefined, el => el
          .child(div().text_size(20).font_semibold().child(`${d.spaces} spaces · ${d.tabs} tabs`))
          .child(div().text_color(t.muted).child(`${d.ready_agents} agents ready to resume`)))
        .when(!d?.saved_at, el => el.child(div().text_color(t.muted).child("Save your spaces and agent sessions before restarting."))))
      .when(this.snapshots.length > 1, el => el.child(v_flex().gap(6)
        .child(div().text_color(t.muted).cursor_pointer().on_click((_e, cx) => {
          if (this.busy) return;
          this.selecting = !this.selecting; cx.notify();
        }).child(`${this.selecting ? "▾" : "▸"} Saved sessions (${this.snapshots.length})`))
        .when(this.selecting, el => el.children(this.snapshots.map((s, index) =>
          v_flex().p(10).gap(4).rounded(8).bg(s.id === d?.id ? t.selected : t.raised).cursor_pointer()
            .on_click((_e, cx) => {
              if (this.busy) return;
              this.saved = s; this.preview = null; this.message = ""; this.error = "";
              this.details = false; this.selecting = false; cx.notify();
            })
            .child(div().child(`${s.id === d?.id ? "✓ " : ""}${timestamp(s.saved_at)}${index === 0 ? " · Newest" : ""}`))
            .child(div().text_size(11).text_color(t.muted).child(`${s.spaces} spaces · ${s.tabs} tabs · ${s.ready_agents} agents ready`)))))))
      .when(missing.length > 0, el => el.child(v_flex().gap(6)
        .child(div().text_color(t.blocked).child(`${missing.length} ${allPi ? "Pi sessions" : "agents"} still need saving`))
        .child(div().text_color(t.muted).child(allPi ? "Run /reload in each Pi tab, then save again." : "Add their session references, then save again."))))
      .when(Boolean(this.message), el => el.child(div().text_color(t.muted).child(this.message)))
      .when(Boolean(this.error), el => el.child(div().text_color(t.blocked).child(this.error)))
      .when(pending.length > 0, el => el.child(div().text_color(t.muted).child(
        `${pending.filter(x => x.action === "resume").length} agents to resume · ${pending.filter(x => x.action === "shell").length} shells to reopen`)))
      .when(missing.length > 0 || Boolean(d?.warnings?.length), el => el.child(v_flex().gap(8)
        .child(div().text_color(t.muted).cursor_pointer().on_click((_e, cx) => { this.details = !this.details; cx.notify(); })
          .child(this.details ? "▾ Hide details" : "▸ Details"))
        .when(this.details, el => el.child(v_flex().max_h(140).overflow_y_scrollbar().gap(5)
          .children(missing.map(r => div().text_size(12).text_color(t.muted).child(`${r.agent} · ${r.terminal}`)))
          .children((d?.warnings || []).map(w => div().text_size(12).text_color(t.muted).child(w)))))))
      .child(h_flex().gap(8)
        .child(button("Save current", "save", pending.length === 0))
        .when(Boolean(d?.saved_at), el => el.child(pending.length > 0 ? button("Restore session", "apply", true) : button("Preview restore", "restore"))))
      .child(div().text_size(11).text_color(t.dim).child("Manual save · Stored on this Mac"));
  }
}
