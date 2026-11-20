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
    this.report = null;
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
        const result = await exec("hctl", ["session", ...args]);
        const data = JSON.parse(result.stdout);
        if (result.code !== 0 || data.error) throw new Error(data.error || result.stderr);
        if (action === "status" || action === "save") {
          this.snapshots = data.snapshots || [];
          this.saved = this.snapshots[0] || data;
          this.selecting = false;
          this.preview = null;
          this.report = null;
          this.message = action === "save" ? "Saved what could be saved." : "";
        } else {
          this.report = data;
          this.preview = action === "restore" ? data : null;
          this.message = data.message;
        }
      } catch (error) { this.error = String(error).replace(/^Error: /, ""); this.message = ""; this.preview = null; }
      this.busy = false;
      cx.notify();
    });
  }
  render() {
    const t = theme();
    const d = this.saved;
    const missing = this.report ? (this.report.actions || []).filter(x => x.action === "skip") : [...(d?.unresolved || []), ...(d?.skipped || [])];
    const button = (label, action, primary = false) => div().px(12).py(7).rounded(6)
      .bg(primary ? t.accent : t.raised).text_color(this.busy ? t.dim : primary ? t.accent_foreground : t.foreground)
      .cursor_pointer().on_click((_e, cx) => this.perform(action, cx)).child(label);
    const timestamp = value => {
      const date = new Date(value);
      return `${date.getMonth() + 1}/${date.getDate()} · ${String(date.getHours()).padStart(2,"0")}:${String(date.getMinutes()).padStart(2,"0")}:${String(date.getSeconds()).padStart(2,"0")}`;
    };
    const when = d?.saved_at ? timestamp(d.saved_at) : "";
    return v_flex().p(18).gap(12).bg(t.popover).text_color(t.foreground).text_size(12)
      .child(h_flex().items_center()
        .child(div().flex_1().font_semibold().text_size(18).child("Session"))
        .child(div().px(8).py(4).rounded(6).cursor_pointer().text_color(t.muted)
          .hover(el => el.bg(t.hover)).on_click(() => closeOverlay(props().overlay)).child("Close")))
      .child(v_flex().p(12).gap(6).rounded(8).bg(t.raised)
        .child(h_flex().items_center()
          .child(div().flex_1().text_color(t.muted).text_size(12).child(d?.saved_at ? "Selected session" : "No saved session"))
          .child(div().text_color(t.dim).text_size(12).child(when)))
        .when(d?.spaces !== undefined, el => el
          .child(div().text_size(15).font_semibold().child(`${d.spaces} spaces · ${d.tabs} tabs · ${d.ready_agents} agents`)))
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
              this.saved = s; this.preview = null; this.report = null; this.message = ""; this.error = "";
              this.details = false; this.selecting = false; cx.notify();
            })
            .child(div().child(`${s.id === d?.id ? "✓ " : ""}${timestamp(s.saved_at)}${index === 0 ? " · Newest" : ""}`))
            .child(div().text_size(11).text_color(t.muted).child(`${s.spaces} spaces · ${s.tabs} tabs · ${s.ready_agents} agents ready`)))))))
      .when(Boolean(this.message), el => el.child(div().text_color(t.muted).child(this.message)))
      .when(Boolean(this.error), el => el.child(div().text_color(t.blocked).child(this.error)))
      .when(missing.length > 0 || Boolean(d?.warnings?.length) || Boolean(this.report?.warnings?.length), el => el.child(v_flex().gap(8)
        .child(div().text_color(t.muted).cursor_pointer().on_click((_e, cx) => { this.details = !this.details; cx.notify(); })
          .child(`${this.details ? "▾" : "▸"} ${missing.length ? `${missing.length} skipped` : "Notes"}`))
        .when(this.details, el => el.child(v_flex().max_h(140).overflow_y_scrollbar().gap(5)
          .children(missing.map(r => v_flex().gap(2).mb(5)
            .child(div().text_size(12).tooltip(`${r.location || ""} · ${r.terminal}`).child(r.label || `${r.agent || "Terminal"} · ${r.terminal}`))
            .child(div().text_size(11).text_color(t.muted).child(r.reason))))
          .children([...(d?.warnings || []), ...(this.report?.warnings || [])].map(w => div().text_size(11).text_color(t.muted).child(w)))))))
      .child(h_flex().gap(8)
        .child(button("Save", "save", true))
        .when(Boolean(d?.saved_at), el => el.child(button("Restore", "apply"))));
  }
}
