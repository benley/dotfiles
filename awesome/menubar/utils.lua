-- Downloaded from https://github.com/alexander-yakushev/menubar
-- Partially backported from awesomeWM master:
-- https://github.com/awesomeWM/awesome/blob/d268dba0758b34a3dbb0658080d91508e05b2f52/lib/menubar/utils.lua.in

-- Grab environment

local io = io
local table = table
local ipairs = ipairs
local string = string

module("menubar.utils")

terminal = 'xterm'

default_icon = ""

icon_theme = nil

all_icon_sizes = {
   '128x128' ,
   '96x96',
   '72x72',
   '64x64',
   '48x48',
   '36x36',
   '32x32',
   '24x24',
   '22x22',
   '16x16'
}

icon_sizes = {}

local function file_exists(filename)
   local file = io.open(filename, 'r')
   local result = (file ~= nil)
   if result then
      file:close()
   end
   return result
end

function lookup_icon(icon_file)
   if not icon_file then
      return default_icon
   end
   if icon_file:sub(1, 1) == '/' and (icon_file:find('.+%.png') or icon_file:find('.+%.xpm')) then
      -- icons with absolute path and supported (AFAICT) formats
      return icon_file
   else
      local icon_path = {}
      local icon_theme_paths = {}
      if icon_theme then
         table.insert(icon_theme_paths, icon_theme .. '/')
         -- TODO also look in parent icon themes, as in freedesktop.org specification
      end
      table.insert(icon_theme_paths, '/usr/share/icons/hicolor/') -- fallback theme cf spec

      local isizes = {}
      for i, sz in ipairs(all_icon_sizes) do
         table.insert(isizes, sz)
      end

      for i, icon_theme_directory in ipairs(icon_theme_paths) do
         for j, size in ipairs(icon_file_sizes or isizes) do
            table.insert(icon_path, icon_theme_directory .. size .. '/apps/')
            table.insert(icon_path, icon_theme_directory .. size .. '/actions/')
            table.insert(icon_path, icon_theme_directory .. size .. '/devices/')
            table.insert(icon_path, icon_theme_directory .. size .. '/places/')
            table.insert(icon_path, icon_theme_directory .. size .. '/categories/')
            table.insert(icon_path, icon_theme_directory .. size .. '/status/')
         end
      end
      -- lowest priority fallbacks
      table.insert(icon_path, '/usr/share/pixmaps/')
      table.insert(icon_path, '/usr/share/icons/')

      for i, directory in ipairs(icon_path) do
         if (icon_file:find('.+%.png') or icon_file:find('.+%.xpm')) and file_exists(directory .. icon_file) then
            return directory .. icon_file
         elseif file_exists(directory .. icon_file .. '.xpm') then
            return directory .. icon_file .. '.xpm'
         elseif file_exists(directory .. icon_file .. '.png') then
            return directory .. icon_file .. '.png'
         end
      end
      return default_icon
   end
end

--- Parse a .desktop file
-- @param file The .desktop file
-- @return A table with file entries.
function parse(file)
   local program = { show = true, file = file }
   local desktop_entry = false

   for line in io.lines(file) do
      if line:find("^%s*#") then
         -- Skip comments.
      elseif not desktop_entry and line == "[Desktop Entry]" then
         desktop_entry = true
      else
         if line:sub(1, 1) == "[" and line:sub(-1) == "]" then
            -- A declaration of a new group - stop parsing
            break
         end

         -- Grab the values
         for key, value in line:gmatch("(%w+)%s*=%s*(.+)") do
            program[key] = value
         end
      end
   end

   if not desktop_entry then return nil end

   -- Don't show program if NoDisplay attribute is false
   if program.NoDisplay and string.lower(program.NoDisplay) == "true" then
      program.show = false
   end

   -- Only show the program if there is not OnlyShowIn attribute
   -- or if it's equal to 'awesome'
   if program.OnlyShowIn ~= nil and program.OnlyShowIn ~= "awesome" then
      program.show = false
   end

   -- Look up for a icon.
   if program.Icon then
      program.icon_path = lookup_icon(program.Icon)
   end

   -- Split categories into a table. Categories are written in one
   -- line separated by semicolon.
   if program.Categories then
      program.categories = {}
      for category in program.Categories:gfind('[^;]+') do
         table.insert(program.categories, category)
      end
   end

   if program.Exec then
      -- Substitute Exec special codes as specified in
      -- http://standards.freedesktop.org/desktop-entry-spec/1.1/ar01s06.html
      if program.Name == nil then
         program.Name = '[' .. file:match("([^/]+)%.desktop$") .. ']'
      end
      local cmdline = program.Exec:gsub('%%c', program.Name)
      cmdline = cmdline:gsub('%%[fuFU]', '')
      cmdline = cmdline:gsub('%%k', program.file)
      if program.icon_path then
         cmdline = cmdline:gsub('%%i', '--icon ' .. program.icon_path)
      else
         cmdline = cmdline:gsub('%%i', '')
      end
      if program.Terminal == "true" then
         cmdline = terminal .. ' -e ' .. cmdline
      end
      program.cmdline = cmdline
   end

   return program
end

--- Parse a directory with .desktop files
-- @param dir The directory.
-- @return A table with all .desktop entries.
function parse_dir(dir)
   local programs = {}
   local files = io.popen('find '.. dir ..' -maxdepth 1 -name "*.desktop" 2>/dev/null')
   for file in files:lines() do
      local program = parse(file)
      if program then
         table.insert(programs, program)
      end
   end
   return programs
end
