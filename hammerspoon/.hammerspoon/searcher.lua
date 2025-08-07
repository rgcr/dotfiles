--[[
  My own 'Searcher' Module for Hammerspoon
  ===============================

  This module provides a simple way to perform web searches using various engines
  through a keyword-based chooser interface in Hammerspoon.

  Features:
  ---------
  - Keyword-based engine selection (e.g., 'g hello' to search on Google)
  - Customizable list of search engines
  - Compact, dynamic chooser interface
  - Supports default or user-defined engine configurations

  Resolution Order:
  ------------------------
  When determining which search engines to use, the module follows this order:

  1. Engines passed explicitly via `setup(custom_engines)`
  2. Engines loaded from `~/.hammerspoon/search_engines.lua` if it exists and is valid
  3. Built-in default engines

  Usage:
  ------
  -- With default search engines:
  require("searcher").setup().show()

  -- With custom search engines:
  local engines = {
    { keyword = "g",  name = "Google", url = "https://www.google.com/search?q=%s" },
    { keyword = "gh", name = "GitHub", url = "https://github.com/search?q=%s" },
  }
  require("searcher").setup(engines).show()

  -- Or, if you prefer chaining later:
  local searcher = require("searcher").setup(engines)
  searcher.show()

  Notes:
  ------
  - Engines can also be defined in a separate Lua file at ~/.hammerspoon/search_engines.lua
    and will be loaded automatically if no custom engines are provided.
  - The %s in the URL will be replaced with the user’s search query (URL-encoded).

]]
local M = {}

-- Try to load ~/.hammerspoon/searcher_engines.lua if it exists, otherwise use defaults
local function load_default_engines()
  local ok, engines = pcall(require, "search_engines")
  if ok and type(engines) == "table" then return engines end
  return {
    { keyword = "g",    name = "Google",       url = "https://www.google.com/search?q=%s" },
    { keyword = "ddg",  name = "DuckDuckGo",   url = "https://duckduckgo.com/?q=%s" },
    { keyword = "gh",   name = "GitHub",       url = "https://github.com/search?q=%s" },
    { keyword = "wiki", name = "Wikipedia",    url = "https://en.wikipedia.org/w/index.php?search=%s" },
  }
end

function M.setup(engines)
  search_engines = engines or load_default_engines()
  print("Search engines:")
  for _, engine in ipairs(search_engines) do
    print(engine.keyword .. " -> " .. engine.name)
  end
  -- print(hs.inspect(search_engines))
  return M
end

local function get_engine_by_keyword(keyword)
  for _, engine in ipairs(search_engines) do
    if engine.keyword == keyword then return engine end
  end
end

local function launch_search(engine, query)
  local encoded = hs.http.encodeForQuery(query)
  local url = engine.url:gsub("%%s", encoded)
  hs.urlevent.openURL(url)
end

function M.show()
  if not search_engines then
    search_engines = load_default_engines()
  end

  local chooser = hs.chooser.new(function(choice)
    if not choice then return end
    local input = choice.input
    local keyword, rest = input:match("^(%S+)%s+(.*)$")
    if not keyword or not rest then return end

    local engine = get_engine_by_keyword(keyword)
    if engine then
      launch_search(engine, rest)
    end
  end)

  chooser:queryChangedCallback(function(input)
    local keyword = input:match("^(%S+)")
    local filtered = {}

    if keyword and #keyword > 0 then
      for _, e in ipairs(search_engines) do
        if e.keyword:sub(1, #keyword) == keyword then
          table.insert(filtered, {
            text = e.keyword .. " - " .. e.name,
            input = input,
          })
        end
      end
    end

    chooser:choices(filtered)
    chooser:rows(math.min(#filtered, 4))
  end)

  chooser:placeholderText("keyword + search")
  chooser:searchSubText(false)
  chooser:rows(1)
  chooser:width(25)
  chooser:show()
end

return M
