-- Local, rounded window picker. No remote content or thumbnail capture.
local M = {}
local html = [=[
<!doctype html><html><head><meta charset="utf-8"><style>
*{box-sizing:border-box}html,body{margin:0;background:transparent;color:#edf0f7;font-family:-apple-system,BlinkMacSystemFont,sans-serif;font-size:13px}
body{padding:14px}main{background:#20232bee;border:1px solid #ffffff20;border-radius:18px;overflow:hidden;box-shadow:0 6px 14px #0006}
header{display:flex;align-items:center;gap:12px;padding:19px 20px;border-bottom:1px solid #ffffff0c}
.search-icon{width:16px;height:16px;border:1.7px solid #9ba6bb;border-radius:50%;position:relative;margin-right:3px}.search-icon:after{content:'';position:absolute;width:6px;height:1.7px;background:#9ba6bb;right:-5px;bottom:-2px;transform:rotate(45deg)}
input{flex:1;min-width:0;background:none;border:0;outline:none;font:400 17px -apple-system;color:#f4f6fb}input::placeholder{color:#8791a4}
kbd{font:10px -apple-system;color:#a5aec0;border:1px solid #ffffff1b;border-radius:5px;padding:3px 5px}
#list{height:410px;overflow-y:auto;padding:6px 8px}.group{padding:9px 12px 5px;color:#8d98ad;font-size:10px;font-weight:600;letter-spacing:.08em;text-transform:uppercase}
.row{display:flex;width:100%;align-items:center;gap:12px;padding:10px 12px;border:0;border-radius:10px;background:none;color:inherit;text-align:left;cursor:pointer}
.row.selected{background:#749cf526}.row img{width:32px;height:32px;object-fit:contain}.copy{flex:1;min-width:0}.app{font-size:13px;font-weight:600}.title{font-size:12px;color:#a0aabd;margin-top:3px;white-space:nowrap;overflow:hidden;text-overflow:ellipsis}.row.selected .title{color:#bbc9e4}
.empty{padding:70px 15px;text-align:center;color:#8d98ad}footer{border-top:1px solid #ffffff0c;display:flex;justify-content:space-between;align-items:center;padding:11px 20px;color:#8d98ad;font-size:11px}.keys{display:flex;gap:8px;align-items:center}
</style></head><body><main><header><span class="search-icon"></span><input id="query" placeholder="Find a window…" autocomplete="off" spellcheck="false" aria-label="Search windows"><kbd>ESC</kbd></header><div id="list" role="listbox" aria-label="Windows"></div><footer><span id="count"></span><span class="keys"><kbd>↑ ↓</kbd> navigate <kbd>↵</kbd> switch</span></footer></main><script>
let all=[],filtered=[],selected=0;
const query=document.getElementById('query'),list=document.getElementById('list');
const send=value=>window.webkit.messageHandlers.windowPicker.postMessage(value);
function pick(){if(filtered[selected])send({id:filtered[selected].windowID});}
function render(){
 list.replaceChildren();let group=null;
 filtered.forEach((item,index)=>{
  const label=item.currentDesktop?'Current desktop':'Other windows';
  if(label!==group){group=label;const heading=document.createElement('div');heading.className='group';heading.textContent=label;list.append(heading);}
  const row=document.createElement('button');row.type='button';row.tabIndex=-1;row.className='row'+(index===selected?' selected':'');row.setAttribute('role','option');row.setAttribute('aria-selected',String(index===selected));
  const icon=document.createElement('img');icon.src=item.icon;icon.alt='';
  const copy=document.createElement('div');copy.className='copy';
  const app=document.createElement('div');app.className='app';app.textContent=item.text;
  const title=document.createElement('div');title.className='title';title.textContent=item.subText||'Untitled window';
  copy.append(app,title);row.append(icon,copy);row.onclick=()=>send({id:item.windowID});list.append(row);
 });
 if(!filtered.length){const empty=document.createElement('div');empty.className='empty';empty.textContent='No matching windows';list.append(empty);}
 document.getElementById('count').textContent=filtered.length+' window'+(filtered.length===1?'':'s');
 const active=list.querySelector('.selected');if(active)active.scrollIntoView({block:'nearest'});
}
query.oninput=()=>{const terms=query.value.toLowerCase().trim().split(/\s+/);filtered=all.filter(x=>terms.every(t=>(x.text+' '+x.subText).toLowerCase().includes(t)));selected=0;render();};
document.onkeydown=e=>{
 if(e.key==='Escape'){e.preventDefault();send({cancel:true});}
 else if(e.key==='Enter'){e.preventDefault();pick();}
 else if(e.key==='ArrowDown'||e.key==='ArrowUp'||e.key==='Tab'){
  e.preventDefault();if(filtered.length){const step=e.key==='ArrowUp'||(e.key==='Tab'&&e.shiftKey)?-1:1;selected=(selected+step+filtered.length)%filtered.length;render();}
 }
};
window.setWindows=items=>{all=items;filtered=items;selected=0;query.value='';render();query.focus();};
</script></body></html>
]=]

function M.new(onChoose)
  local picker = { visible = false, choicesByID = {} }
  local controller = hs.webview.usercontent.new("windowPicker")
  picker.controller = controller
  local view = hs.webview.new({x=0,y=0,w=540,h=545}, {privateBrowsing=true}, controller)
  picker.view = view
  view:windowStyle({"borderless"}):transparent(true):allowTextEntry(true)
  view:allowNewWindows(false):level(hs.drawing.windowLevels.modalPanel)
  function picker:isVisible() return self.visible end
  function picker:hide(restore)
    self.visible = false
    view:hide()
    if restore and self.previous then self.previous:focus() end
  end
  controller:setCallback(function(message)
    local body = message.body or message
    if type(body) ~= "table" then return end
    if body.cancel then picker:hide(true); return end
    local choice = picker.choicesByID[body.id]
    if choice then picker:hide(false); onChoose(choice) end
  end)
  view:windowCallback(function(action, _, focused)
    if action == "focusChange" and not focused and picker.visible then
      -- macOS can deliver an old blur event while this window reopens.
      local generation = picker.generation
      hs.timer.doAfter(0.25, function()
        if not picker.visible or picker.generation ~= generation then return end
        local active, own = hs.window.focusedWindow(), view:hswindow()
        if not active or not own or active:id() ~= own:id() then picker:hide(false) end
      end)
    end
  end)
  view:navigationCallback(function(action)
    if action == "didFinishNavigation" and picker.visible then
      view:evaluateJavaScript("window.setWindows(" .. hs.json.encode(picker.pending) .. ")")
      local win = view:hswindow()
      if win then win:focus() end
      local generation = picker.generation
      hs.timer.doAfter(0.2, function()
        if picker.visible and picker.generation == generation then
          view:evaluateJavaScript("document.getElementById('query').focus()")
        end
      end)
    end
  end)
  function picker:show(choices)
    self.generation = (self.generation or 0) + 1
    self.previous = hs.window.frontmostWindow()
    self.choicesByID = {}
    self.pending = {}
    for _, choice in ipairs(choices) do
      self.choicesByID[choice.windowID] = choice
      self.pending[#self.pending+1] = {
        text=choice.text, subText=choice.subText, windowID=choice.windowID,
        currentDesktop=choice.currentDesktop,
        icon=choice.image:encodeAsURLString(),
      }
    end
    local screen = self.previous and self.previous:screen() or hs.screen.mainScreen()
    local frame = screen:frame()
    local width = math.min(540, frame.w-30)
    view:frame({x=frame.x+(frame.w-width)/2,y=frame.y+math.max(15,(frame.h-545)*0.5),w=width,h=545})
    self.visible = true
    view:html(html)
    view:show():bringToFront()
  end
  return picker
end
return M
