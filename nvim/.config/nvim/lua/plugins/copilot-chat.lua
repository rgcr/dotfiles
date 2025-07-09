-----------------------------------------------------------
-- File manager configuration file
-----------------------------------------------------------

-- Plugin: CopilotChat

-- Keybindings are defined in `core/keymaps.lua`:
local ok_cchat, copilot_chat = pcall(require, 'CopilotChat')
if not ok_cchat then
  return
end

local ok_hydra, Hydra = pcall(require, 'hydra')
if not ok_hydra then
  return
end

local prompts = {
  -- Code related prompts
  Explain = "You are a Sr DevOps and a Sr developer in all programming languages. Please explain how the following code works.",
  Review = "You are a Sr DevOps and a Sr developer in all programming languages. Please review the following code and provide suggestions for improvement.",
  Tests = "You are a Sr DevOps and a Sr developer in all programming languages. Please explain how the selected code works, then generate unit tests for it.",
  Refactor = "You are a Sr DevOps and a Sr developer in all programming languages. Please refactor the following code to improve its clarity and readability.",
  FixCode = "You are a Sr DevOps and a Sr developer in all programming languages. Please fix the following code to make it work as intended.",
  FixError = "You are a Sr DevOps and a Sr developer in all programming languages. Please explain the error in the following text and provide a solution.",
  BetterNamings = "You are a Sr DevOps and a Sr developer in all programming languages. Please provide better names for the following variables and functions.",
  Documentation = "You are a Sr DevOps and a Sr developer in all programming languages. Please provide documentation for the following code.",
  -- Text related prompts
  Summarize = "Please summarize the following text.",
  Spelling = "Please correct any grammar and spelling errors in the following text.",
  Wording = "Please improve the grammar and wording of the following text.",
  Concise = "Please rewrite the following text to make it more concise.",
} --end prompts

local opts = {
  question_header = ">>>> User ",
  answer_header = ">>>> Copilot ",
  error_header = ">>>> Error ",
  prompts = prompts,
  mappings = {
    -- Use tab for completion
    complete = {
      detail = "Use '<Tab>' for options.",
      insert = "<Tab>",
    },
    -- Close the chat
    close = {
      normal = "q",
      insert = "<C-c>",
    },
    -- Submit the prompt to Copilot
    submit_prompt = {
      normal = "<CR>",
      insert = "<C-CR>",
    },
    -- Accept the diff
    accept_diff = {
      normal = "<C-y>",
      insert = "<C-y>",
    },
    -- Show help
    show_help = {
      normal = "?",
    },
  },
} -- end opts

copilot_chat.setup(opts)

-- Define the Hydra for copilot_chat
local hint_copilot_chat = [[
       Copilot Chat
----------------------------
  _o_ : Open Copilot Chat
  _x_ : Reset Copilot chat
  _q_ : Close Copilot chat

  _<Esc>: Quit
]]


Hydra({
  name = "Copilot Chat",
  hint = hint_copilot_chat,
  config = {
    color = "teal",
    invoke_on_body = true,
    hint = {
      position = "middle",
    },
  },
  mode = 'n',
  body = "<space>C",
  heads = {
    { "<Esc>", nil, {exit = true, nowait = true}  },
    { "o"  , function() copilot_chat.open() end, { desc = "Open Copilot Chat" } },
    { "x", function() copilot_chat.reset() end, { desc = "Reset Copilot Chat" } },
    { "q", function() copilot_chat.close() end, { desc = "Close Copilot Chat" } },
  },
})

