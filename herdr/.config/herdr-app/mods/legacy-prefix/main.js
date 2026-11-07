import { View, div } from "gpui-kit";
import { v_flex, EditorState, Editor } from "gpui-base";
import { focused, spaces, run, theme, props, closeOverlay, hc, notify, shared } from "herdr";

// Compatibility with the Ctrl+A bindings in ../herdr/config.toml.
export function actionFor(event, shortcuts = {}) {
  const k = event.keystroke;
  const plain = !event.modifiers.control && !event.modifiers.alt && !event.modifiers.platform;
  if (!plain) return null;
  const character = event.key_char || event.key;
  if (character === "|" || k === "shift-\\") return ["pane.split", { direction: "right" }];
  if (character === "$" || k === "shift-4") return ["space.rename"];
  if (character === "<" || k === "shift-,") return ["move", -1];
  if (character === ">" || k === "shift-.") return ["move", 1];
  if (k === "shift-x" || character === "X") return ["space.close"];
  if (k === "shift-w" || character === "W") return ["space.new"];
  if (k === "shift-e" || character === "E") return ["scrollback"];
  if (k === "shift-tab") return ["space.prev"];
  if (event.modifiers.shift) return null;
  // Only unused keys can select tabs; existing prefix commands always win.
  if ("1234567890bfimnopqrtuvyz".includes(k) && k.length === 1 && shortcuts[k])
    return ["quick-tab", shortcuts[k]];
  return {
    c: ["tab.new"],
    w: ["space-agents.focus"],
    s: ["mod.open", { name: "session-manager" }],
    x: ["tab.close"],
    g: ["mod.open", { name: "tab-finder" }],
    space: ["mod.open", { name: "tab-finder" }],
    d: ["cancel"],
    ",": ["tab.rename"],
    enter: ["tab.next"],
    backspace: ["tab.prev"],
    tab: ["space.next"],
    "-": ["pane.split", { direction: "down" }],
    h: ["pane.go", { direction: "left" }],
    left: ["pane.go", { direction: "left" }],
    j: ["pane.go", { direction: "down" }],
    down: ["pane.go", { direction: "down" }],
    k: ["pane.go", { direction: "up" }],
    up: ["pane.go", { direction: "up" }],
    l: ["pane.go", { direction: "right" }],
    right: ["pane.go", { direction: "right" }],
    e: ["layout.toggle", { id: "sidebar" }],
    a: ["send-prefix"],
    escape: ["cancel"],
  }[k] || null;
}

export default class LegacyPrefix extends View {
  init(initial, cx) {
    this.context = focused();
    this.shortcuts = shared("navigation.tabShortcuts") ?? {};
    this.handle = cx.focus_handle();
    this.scrollback = initial?.scrollback || props()?.scrollback;
    if (this.scrollback) {
      this.message = "Loading scrollback…";
      cx.spawn(async cx => {
        try {
          const result = await hc(["read", this.scrollback, "--lines", "10000"]);
          if (result.code !== 0) throw new Error(result.stderr || "Could not read terminal");
          this.editor = EditorState.new({ language: "txt", value: result.stdout, readonly: false });
        } catch (error) { this.message = String(error); }
        cx.notify();
      });
    } else {
      this.handle.focus();
      // The overlay installs its own focus when mounting; take it after mount.
      cx.timer.after(50, () => this.handle.focus());
    }
  }

  dispatch(event, cx) {
    cx.stop_propagation();
    if (event.is_held || this.busy) return;
    const action = actionFor(event, this.shortcuts);
    if (!action) return;
    const [command, args] = action;
    if (command === "send-prefix") {
      const terminal = this.context?.terminal;
      if (!terminal) { closeOverlay(props().overlay); return; }
      this.busy = true;
      cx.spawn(async () => {
        try {
          const result = await hc(["keys", terminal, "ctrl+a"]);
          if (result.code !== 0) throw new Error(result.stderr);
        } catch (error) { notify({ title: "Could not send Ctrl+A", body: String(error), level: "error" }); }
        closeOverlay(props().overlay);
      });
      return;
    }
    closeOverlay(props().overlay);
    if (command === "cancel") return;
    if (command === "space-agents.focus") run("layout.show", { id: "sidebar" });
    if (command === "quick-tab") {
      // Use the displayed ID, never a position that could have shifted meanwhile.
      if (spaces().some(s => s.id === args.space && s.tabs.some(t => t.id === args.tab)))
        run("tab.select", { id: args.tab });
      return;
    }
    if (command === "move") {
      const space = spaces().find(s => s.id === this.context?.space);
      const index = space?.tabs.findIndex(t => t.id === this.context?.tab) ?? -1;
      const destination = index + args;
      if (index >= 0 && destination >= 0 && destination < space.tabs.length)
        run("tab.move", { id: this.context.tab, index: destination + 1 });
    } else if (command === "scrollback") {
      if (this.context?.terminal && this.context?.pane)
        run("pane.split", { pane: this.context.pane, direction: "right", mod: "legacy-prefix",
          props: { scrollback: this.context.terminal } });
    } else {
      run(command, args);
    }
  }

  render() {
    const t = theme();
    if (this.scrollback)
      return v_flex().size_full().bg(t.background).text_color(t.foreground)
        .child(div().p(8).text_size(12).child("Scrollback snapshot · editable scratch buffer · not saved"))
        .child(this.editor ? Editor.new(this.editor).flex_1().min_h(0) : div().p(12).child(this.message));
    // Keep a focus target for the second key without painting a prefix UI.
    return div().id("prefix").size_full().track_focus(this.handle)
      .on_key_down((event, cx) => this.dispatch(event, cx));
  }
}
