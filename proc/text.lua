--GlobalVoiceLines = {}
--GameData = {}
--Color = {}
--ConstantsData = {}
--WeaponSets = {}
--HeroVoiceLines = {}
Texts = dofile("output/HelpText.en.lua").Texts

print('[')

local function ArrayConcat(arrays)
	local result = {}
	for i,a in ipairs(arrays) do
		for j,v in ipairs(a) do
			table.insert(result, v)
		end
	end
	return result
end

local function RemoveFromList(list, item)
	for i,v in ipairs(list) do
		if v == item then
			table.remove(list, i)
			return
		end
	end
end

local function String(s)
	return '"' .. s .. '"'
end

local function Array(a)
	return '[' .. table.concat(a, ', ') .. ']'
end

local function ArrayOfStrings(a)
	local strings = {}
	for i,s in ipairs(a) do
		strings[i] = String(s)
	end
	return '[' .. table.concat(strings, ', ') .. ']'
end

local function Color255(color)
	return string.format('[%d,%d,%d]', color[1], color[2], color[3])
end

local function DisplayName(id)
	for i,data in ipairs(Texts) do
		if data.Id == id then
			if data.DisplayName then
				return data.DisplayName
			elseif data.InheritFrom then
				return DisplayName(data.InheritFrom)
			end
		end
	end
end

printed = {}

keywords =
  { "Armor"
  , "Attack"
  , "BladeRift"
  , "BlinkStrike"
  , "BossPlural"
  , "Cast"
  , "Charm"
  , "Chill"
  , "Cloud"
  , "Common"
  , "Crit"
  , "Curse"
  , "Dash"
  , "Deflect"
  , "Dodge"
  , "Encounter"
  , "EncounterAlt"
  , "EncounterPlural"
  , "ExtraChance"
  , "FishingPoint"
  , "Fountain"
  , "GodBoonPlural"
  , "Invulnerable"
  , "Mark"
  , "Poison"
  , "PomPlural"
  , "Rarity"
  , "Revenge"
  , "RoomAlt"
  , "Rupture"
  , "Special"
  , "Stagger"
  , "Static"
  , "Sturdy"
  , "Traps"
  , "Weak"
  , "Wrath"
  , "WrathDerivedStocks"
  , "WrathGauge"
  }

terms = keywords

items = {}
for i = 1, #terms, 1 do
	local item = ''
	local id = terms[i]
	item = item .. '  {\n'
	item = item .. '    "Id": "' .. id .. '",\n'
	item = item .. '    "DisplayName": ' .. String(DisplayName(id)) .. '\n'
	item = item .. '  }\n'
	table.insert(items, item)
end
print(table.concat(items, ',\n'))
print(']')
