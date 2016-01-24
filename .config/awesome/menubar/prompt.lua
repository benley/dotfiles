---------------------------------------------------------------------------
-- @author Julien Danjou &lt;julien@danjou.info&gt;
-- @modified by Alex Y. <yakushev.alex@gmail.com>
-- @copyright 2008 Julien Danjou
-- @release v3.4.11
---------------------------------------------------------------------------

-- Grab environment we need
local assert = assert
local io = io
local table = table
local math = math
local ipairs = ipairs
local pcall = pcall
local print = print
local capi =
   {
   keygrabber = keygrabber,
   selection = selection
}
local util = require("awful.util")
local beautiful = require("beautiful")

--- Prompt module for awful
module("menubar.prompt")

--- Private data
local data = {}
data.history = {}

local search_term = nil
local function itera (inc,a, i)
   i = i + inc
   local v = a[i]
   if v then return i,v end
end

-- Load history file in history table
-- @param id The data.history identifier which is the path to the filename
-- @param max Optional parameter, the maximum number of entries in file
local function history_check_load(id, max)
   if id and id ~= ""
      and not data.history[id] then
      data.history[id] = { max = 50, table = {} }

      if max then
         data.history[id].max = max
      end

      local f = io.open(id, "r")

      -- Read history file
      if f then
         for line in f:lines() do
            if util.table.hasitem(data.history[id].table, line) == nil then
               table.insert(data.history[id].table, line)
               if #data.history[id].table >= data.history[id].max then
                  break
               end
            end
         end
         f:close()
      end
   end
end

-- Save history table in history file
-- @param id The data.history identifier
local function history_save(id)
   if data.history[id] then
      local f = io.open(id, "w")
      if not f then
         local i = 0
         for d in id:gmatch(".-/") do
            i = i + #d
         end
         util.mkdir(id:sub(1, i - 1))
         f = assert(io.open(id, "w"))
      end
      for i = 1, math.min(#data.history[id].table, data.history[id].max) do
         f:write(data.history[id].table[i] .. "\n")
      end
      f:close()
   end
end

-- Return the number of items in history table regarding the id
-- @param id The data.history identifier
-- @return the number of items in history table, -1 if history is disabled
local function history_items(id)
   if data.history[id] then
      return #data.history[id].table
   else
      return -1
   end
end

-- Add an entry to the history file
-- @param id The data.history identifier
-- @param command The command to add
local function history_add(id, command)
   if data.history[id] then
      if command ~= ""
         and util.table.hasitem(data.history[id].table, command) == nil then
         table.insert(data.history[id].table, command)

         -- Do not exceed our max_cmd
         if #data.history[id].table > data.history[id].max then
            table.remove(data.history[id].table, 1)
         end

         history_save(id)
      end
   end
end


-- Draw the prompt text with a cursor.
-- @param args The table of arguments.
-- @param text The text.
-- @param font The font.
-- @param prompt The text prefix.
-- @param text_color The text color.
-- @param cursor_color The cursor color.
-- @param cursor_pos The cursor position.
-- @param cursor_ul The cursor underline style.
-- @param selectall If true cursor is rendered on the entire text.
local function prompt_text_with_cursor(args)
   local char, spacer, text_start, text_end, ret
   local text = args.text or ""
   local prompt = args.prompt or ""
   local underline = args.cursor_ul or "none"

   if args.selectall then
      if #text == 0 then char = " " else char = util.escape(text) end
      spacer = " "
      text_start = ""
      text_end = ""
   elseif #text < args.cursor_pos then
      char = " "
      spacer = ""
      text_start = util.escape(text)
      text_end = ""
   else
      char = util.escape(text:sub(args.cursor_pos, args.cursor_pos))
      spacer = " "
      text_start = util.escape(text:sub(1, args.cursor_pos - 1))
      text_end = util.escape(text:sub(args.cursor_pos + 1))
   end

   ret = prompt .. text_start .. "<span background=\"" .. util.color_strip_alpha(args.cursor_color) .. "\" foreground=\"" .. util.color_strip_alpha(args.text_color) .. "\" underline=\"" .. underline .. "\">" .. char .. "</span>" .. text_end .. spacer
   if args.font then ret = "<span font_desc='" .. args.font .. "'>" .. ret .. "</span>" end
   return ret
end

--- Run a prompt in a box.
-- @param args A table with optional arguments: fg_cursor, bg_cursor, ul_cursor, prompt, text, selectall, font, autoexec.
-- @param textbox The textbox to use for the prompt.
-- @param exe_callback The callback function to call with command as argument when finished.
-- @param completion_callback The callback function to call to get completion.
-- @param history_path Optional parameter: file path where the history should be saved, set nil to disable history
-- @param history_max Optional parameter: set the maximum entries in history file, 50 by default
-- @param done_callback Optional parameter: the callback function to always call without arguments, regardless of whether the prompt was cancelled.
function run(args, textbox, exe_callback, completion_callback,
             history_path, history_max, done_callback, change_callback, keypressed_callback)
   local theme = beautiful.get()
   if not args then args = {} end
   local command = args.text or ""
   local command_before_comp
   local cur_pos_before_comp
   local prettyprompt = args.prompt or ""
   local inv_col = args.fg_cursor or theme.fg_focus or "black"
   local cur_col = args.bg_cursor or theme.bg_focus or "white"
   local cur_ul = args.ul_cursor
   local text = args.text or ""
   local font = args.font or theme.font
   local selectall = args.selectall

   search_term=nil

   history_check_load(history_path, history_max)
   local history_index = history_items(history_path) + 1
   -- The cursor position
   local cur_pos = (selectall and 1) or text:wlen() + 1
   -- The completion element to use on completion request.
   local ncomp = 1
   if not textbox or not exe_callback then
      return
   end
   textbox.text = prompt_text_with_cursor{
      text = text, text_color = inv_col, cursor_color = cur_col,
      cursor_pos = cur_pos, cursor_ul = cur_ul, selectall = selectall,
      font = font, prompt = prettyprompt }

   local exec = function()
                   textbox.text = ""
                   history_add(history_path, command)
                   capi.keygrabber.stop()
                   exe_callback(command)
                   if done_callback then done_callback() end
                end

   capi.keygrabber.run(
      function (modifiers, key, event)
         -- Update textbox
         local function update()
            textbox.text = prompt_text_with_cursor{
               text = command, text_color = inv_col, cursor_color = cur_col,
               cursor_pos = cur_pos, cursor_ul = cur_ul, selectall = selectall,
               font = font, prompt = prettyprompt }
         end

         if event ~= "press" then
            return true
         end
         -- Convert index array to hash table
         local mod = {}
         for k, v in ipairs(modifiers) do
            mod[v] = true
         end
         -- Call user specified callback, if it returns true then
         -- return from function
         if keypressed_callback then
            local user_catched, new_command, new_prompt =
               keypressed_callback(mod, key, command)
            if user_catched then
               if new_command then
                  command = new_command
                  update()
               end
               if new_prompt then
                  prettyprompt = new_prompt
                  update()
               end
               change_callback(command)
               return true
            end
         end
         -- Get out cases
         if (mod.Control and (key == "c" or key == "g")) or (not mod.Control and key == "Escape") then
            textbox.text = ""
            if done_callback then done_callback() end
            return false
         elseif (mod.Control and (key == "j" or key == "m")) or (not mod.Control and key == "Return") or (not mod.Control and key == "KP_Enter") then
            exec()
            -- We already unregistered ourselves so we don't want to return
            -- true, otherwise we may unregister someone else.
            return true
         end

         -- Control cases
         if mod.Control then
            selectall = nil
            if key == "a" then
               cur_pos = 1
            elseif key == "b" then
               if cur_pos > 1 then
                  cur_pos = cur_pos - 1
               end
            elseif key == "d" then
               if cur_pos <= #command then
                  command = command:sub(1, cur_pos - 1) .. command:sub(cur_pos + 1)
               end
            elseif key == "e" then
               cur_pos = #command + 1
            elseif key == "r" then
               search_term = search_term or command:sub(1, cur_pos - 1)
               for i,v in (function(a,i) return itera(-1,a,i) end), data.history[history_path].table, history_index do
                  if v:find(search_term) ~= nil then
                     command=v
                     history_index=i
                     cur_pos=#command+1
                     break
                  end
               end
            elseif key == "s" then
               search_term = search_term or command:sub(1, cur_pos - 1)
               for i,v in (function(a,i) return itera(1,a,i) end), data.history[history_path].table, history_index do
                  if v:find(search_term) ~= nil then
                     command=v
                     history_index=i
                     cur_pos=#command+1
                     break
                  end
               end
            elseif key == "f" then
               if cur_pos <= #command then
                  cur_pos = cur_pos + 1
               end
            elseif key == "h" then
               if cur_pos > 1 then
                  command = command:sub(1, cur_pos - 2) .. command:sub(cur_pos)
                  cur_pos = cur_pos - 1
               end
            elseif key == "k" then
               command = command:sub(1, cur_pos - 1)
            elseif key == "u" then
               command = command:sub(cur_pos, #command)
               cur_pos = 1
            elseif key == "Up" then
               search_term = search_term or command:sub(1, cur_pos - 1)
               for i,v in (function(a,i) return itera(-1,a,i) end), data.history[history_path].table, history_index do
                  if v:find('^'..search_term) ~= nil then
                     command=v
                     history_index=i
                     break
                  end
               end
            elseif key == "Down" then
               search_term = search_term or command:sub(1, cur_pos - 1)
               for i,v in (function(a,i) return itera(1,a,i) end), data.history[history_path].table, history_index do
                  if v:find('^'..search_term) ~= nil then
                     command=v
                     history_index=i
                     break
                  end
               end
            elseif key == "w" or key == "BackSpace" then
               local wstart = 1
               local wend = 1
               local cword_start = 1
               local cword_end = 1
               while wend < cur_pos do
                  wend = command:find("[{[(,.:;_-+=@/ ]", wstart)
                  if not wend then wend = #command + 1 end
                  if cur_pos >= wstart and cur_pos <= wend + 1 then
                     cword_start = wstart
                     cword_end = cur_pos - 1
                     break
                  end
                  wstart = wend + 1
               end
               command = command:sub(1, cword_start - 1) .. command:sub(cword_end + 1)
               cur_pos = cword_start
            end
         else
            if completion_callback then
               if key == "Tab" or key == "ISO_Left_Tab" then
                  if key == "ISO_Left_Tab" then
                     if ncomp == 1 then return true end
                     if ncomp == 2 then
                        command = command_before_comp
                        textbox.text = prompt_text_with_cursor{
                           text = command_before_comp, text_color = inv_col, cursor_color = cur_col,
                           cursor_pos = cur_pos, cursor_ul = cur_ul, selectall = selectall,
                           font = font, prompt = prettyprompt }
                        return true
                     end

                     ncomp = ncomp - 2
                  elseif ncomp == 1 then
                     command_before_comp = command
                     cur_pos_before_comp = cur_pos
                  end
                  local matches
                  command, cur_pos, matches = completion_callback(command_before_comp, cur_pos_before_comp, ncomp)
                  ncomp = ncomp + 1
                  key = ""
                  -- execute if only one match found and autoexec flag set
                  if matches and #matches == 1 and args.autoexec then
                     exec()
                     return true
                  end
               else
                  ncomp = 1
               end
            end

            -- Typin cases
            if mod.Shift and key == "Insert" then
               local selection = capi.selection()
               if selection then
                  -- Remove \n
                  local n = selection:find("\n")
                  if n then
                     selection = selection:sub(1, n - 1)
                  end
                  command = command:sub(1, cur_pos - 1) .. selection .. command:sub(cur_pos)
                  cur_pos = cur_pos + #selection
               end
            elseif key == "Home" then
               cur_pos = 1
            elseif key == "End" then
               cur_pos = #command + 1
            elseif key == "BackSpace" then
               if cur_pos > 1 then
                  command = command:sub(1, cur_pos - 2) .. command:sub(cur_pos)
                  cur_pos = cur_pos - 1
               end
            elseif key == "Delete" then
               command = command:sub(1, cur_pos - 1) .. command:sub(cur_pos + 1)
            elseif key == "Left" then
               cur_pos = cur_pos - 1
            elseif key == "Right" then
               cur_pos = cur_pos + 1
            elseif key == "Up" then
               if history_index > 1 then
                  history_index = history_index - 1

                  command = data.history[history_path].table[history_index]
                  cur_pos = #command + 2
               end
            elseif key == "Down" then
               if history_index < history_items(history_path) then
                  history_index = history_index + 1

                  command = data.history[history_path].table[history_index]
                  cur_pos = #command + 2
               elseif history_index == history_items(history_path) then
                  history_index = history_index + 1

                  command = ""
                  cur_pos = 1
               end
            else
               -- wlen() is UTF-8 aware but #key is not,
               -- so check that we have one UTF-8 char but advance the cursor of # position
               if key:wlen() == 1 then
                  if selectall then command = "" end
                  command = command:sub(1, cur_pos - 1) .. key .. command:sub(cur_pos)
                  cur_pos = cur_pos + #key
               end
            end
            if cur_pos < 1 then
               cur_pos = 1
            elseif cur_pos > #command + 1 then
               cur_pos = #command + 1
            end
            selectall = nil
         end

         local success = pcall(update)
         while not success do
            -- TODO UGLY HACK TODO
            -- Setting the text failed. Most likely reason is that the user
            -- entered a multibyte character and pressed backspace which only
            -- removed the last byte. Let's remove another byte.
            if cur_pos <= 1 then
               -- No text left?!
               break
            end

            command = command:sub(1, cur_pos - 2) .. command:sub(cur_pos)
            cur_pos = cur_pos - 1
            success = pcall(update)
         end
         change_callback(command)
         return true
      end)
end
