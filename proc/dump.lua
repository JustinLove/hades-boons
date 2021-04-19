GlobalVoiceLines = {}
GameData = {}
Color = {}
ConstantsData = {}
WeaponSets = {}
HeroVoiceLines = {}
dofile("D:/Games/Epic Games/Hades/Content/Scripts/LootData.lua")
dofile("D:/Games/Epic Games/Hades/Content/Scripts/TraitData.lua")
dofile("D:/Games/Epic Games/Hades/Content/Scripts/ConsumableData.lua")
Texts = dofile("output/HelpText.en.lua").Texts
Animations = dofile("output/GUIAnimations.lua").Animations

print('[')
--print('  imagepath="pkg/GUI/textures/";')

local function ArrayConcat(arrays)
	local result = {}
	for i,a in ipairs(arrays) do
		for j,v in ipairs(a) do
			table.insert(result, v)
		end
	end
	return result
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

local function TraitName(id)
	for i,data in ipairs(Texts) do
		if data.Id == id then
			if data.DisplayName then
				return data.DisplayName
			elseif data.InheritFrom then
				return TraitName(data.InheritFrom)
			end
		end
	end
end

local function IconPath(id)
	local name = id .. '_Large'
	for i,data in ipairs(Animations) do
		if data.Name == name then
			if data.FilePath then
				return (data.FilePath .. ".png"):gsub("\\", "/")
			else
				return "GUI/Screens/BoonIcons/Athena_01_Large.png"
			end
		end
	end
	return name:gsub("\\", "/")
end

local function TraitIcon(trait)
	if TraitData[trait] then
		if TraitData[trait].Icon then
			return IconPath(TraitData[trait].Icon)
		elseif TraitData[trait].InheritFrom then
			for i,parent in ipairs(TraitData[trait].InheritFrom) do
				icon = TraitIcon(parent)
				if icon then
					return icon
				end
			end
		end
	end
	if ConsumableData[trait] and ConsumableData[trait].Icon then
		return IconPath(ConsumableData[trait].Icon)
	end
end

local function GodTrait(trait, extra)
	local name = trait
	local image = ""
	local tname = TraitName(trait)
	if tname then
		name = tname
	end
	local item = '      {\n'
	if extra then
		item = item .. '        ' .. extra .. ',\n'
	end
	icon = TraitIcon(trait)
	if icon then
		item = item .. '        "icon": ' .. String(icon) .. ',\n'
	end
	item = item .. '        "trait": ' .. String(trait) .. ',\n'
	item = item .. '        "name": ' .. String(name) .. '\n'
	item = item .. '      }'
	return item
end

local function DuoTrait(trait)
	return GodTrait(trait)
end

local function GodTraits(traits)
	local result = {}
	for i,trait in ipairs(traits) do
		table.insert(result, GodTrait(trait))
	end
	return result
end

printed = {}

order1 = {"HermesUpgrade","AphroditeUpgrade", "AresUpgrade", "AthenaUpgrade", "DemeterUpgrade", "DionysusUpgrade", "PoseidonUpgrade", "ArtemisUpgrade", "ZeusUpgrade"}
order2 = {"HermesUpgrade","AphroditeUpgrade", "AresUpgrade", "DemeterUpgrade", "DionysusUpgrade", "PoseidonUpgrade", "AthenaUpgrade", "ArtemisUpgrade", "ZeusUpgrade"}

order = order2

gods = {}
for i = 1, #order, 1 do
	local god = ''
	local godName = order[i]
	local data = LootData[godName]
	god = god .. '  {\n'
	god = god .. '    "Name": "' .. godName .. '",\n'
	god = god .. '    "LootColor": ' .. Color255(data.LootColor) .. ',\n'
	god = god .. '    "Color": ' .. Color255(data.Color) .. ',\n'
	if data.LinkedUpgrades then
		local traits = GodTraits(ArrayConcat({data.WeaponUpgrades, data.Traits}))
		if data.Consumables then
			for i,toget in ipairs(data.Consumables) do
				if ConsumableData[toget] then
					local extra
					if ConsumableData[toget].RequiredOneOfTraits then
						extra = String('OneOf') .. ':' .. ArrayOfStrings(ConsumableData[toget].RequiredOneOfTraits)
					end
					table.insert(traits, GodTrait(toget, extra))
				end
			end
		end
		god = god .. '    "Traits": [\n'
		god = god .. table.concat(traits, ',\n') .. '\n'
		god = god .. '\n    ],\n'

		local links = {}
		for toget,reqs in pairs(data.LinkedUpgrades) do
			local extra = ''
			if reqs.OneOf then
				extra = String('OneOf') .. ':' .. ArrayOfStrings(reqs.OneOf)
			end
			if reqs.OneFromEachSet then
				local sets = {}
				for s,set in ipairs(reqs.OneFromEachSet) do
					table.insert(sets, ArrayOfStrings(set))
				end
				extra = String('OneFromEachSet') .. ':' .. Array(sets)
			end
			table.insert(links, GodTrait(toget, extra))
		end
		god = god .. '    "LinkedUpgrades": [\n'
		god = god .. table.concat(links, ',\n') .. '\n'
		god = god .. '    ]\n'
		god = god .. '  }\n'
	end
	table.insert(gods, god)
end
print(table.concat(gods, ',\n'))
print(']')
