GlobalVoiceLines = {}
GameData = {}
Color = {}
ConstantsData = {}
WeaponSets = {}
HeroVoiceLines = {}
dofile("D:/Games/Epic Games/Hades/Content/Scripts/LootData.lua")
dofile("D:/Games/Epic Games/Hades/Content/Scripts/TraitData.lua")
Texts = dofile("output/HelpText.en.lua").Texts
Animations = dofile("output/GUIAnimations.lua").Animations

print('digraph G {')
print('  ranksep=9;')
print('  root=start;')
print('  ratio=auto;')
print('  imagepath="pkg/GUI/textures/";')
--print('  overlap=scale;')

local function Color255(color)
	return string.format('#%0.2x%0.2x%0.2x', color[1], color[2], color[3])
end

local function GodColor(data)
	return '"' .. Color255(data.LootColor) .. ';0.7:' .. Color255(data.Color) .. '"'
end

local function TraitName(id)
	for i,data in ipairs(Texts) do
		if data.Id == id then
			return data.DisplayName
		end
	end
end

local function TraitIcon(id)
	name = id .. '_Large'
	--print(name)
	for i,data in ipairs(Animations) do
		--print(data.Name)
		if data.Name == name then
			if data.FilePath then
				return data.FilePath .. ".png"
			else
				return "GUI\\Screens\\BoonIcons\\Athena_01_Large.png"
			end
		end
	end
	return name
end

local function GodTrait(trait, data)
	label = trait
	image = ""
	name = TraitName(trait)
	if name then
		label = name
	end
	if TraitData[trait] and TraitData[trait].Icon then
		--label = label .. '\n' .. TraitData[trait].Icon
		--image = TraitIcon(TraitData[trait].Icon)
	end
	print('    ' .. trait .. ' [ style="filled" fillcolor=' .. GodColor(data) .. ' label="' .. label .. '" image="' .. image .. '" ];')
end

local function DuoTrait(trait)
	label = trait
	image = ""
	name = TraitName(trait)
	if name then
		--label = name
	end
	if TraitData[trait] and TraitData[trait].Icon then
		--label = label .. '\n' .. TraitData[trait].Icon
		--image = TraitIcon(TraitData[trait].Icon)
	end
	print('    ' .. trait .. ' [ style="filled" label="' .. label .. '" image="' .. image .. '" ];')
end

local function GodTraits(god, table, data)
	for i,trait in ipairs(table) do
		GodTrait(trait, data)
		print('    ' .. god .. ' -> ' .. trait .. ' [ style="invis", len=0.4 ];')
	end
end

printed = {}

local function JoinKey(reqs, toget, weight)
	join = toget
	if #reqs > 1 then
		table.sort(reqs)
		join = table.concat(reqs, '_') .. 'reqs'
		if not printed[join] then
			print('    '..join..' [ label="", height=.1, width=.1 ];')
			printed[join] = true
		end
		print('    ' .. join .. ' -> ' .. toget .. ' [ weight='..weight..', len='..(1/weight)..'];')
	end
	for o,req in ipairs(reqs) do
		edge = req .. '-' .. join
		if not printed[edge] then
			print('    ' .. req .. ' -> ' .. join .. ' [ weight=2, len=0.5 ] ;')
			printed[edge] = true
		end
	end
	return join
end

print('  start [ shape="box" style="filled" ];')

order1 = {"AphroditeUpgrade", "AresUpgrade", "AthenaUpgrade", "DemeterUpgrade", "DionysusUpgrade", "PoseidonUpgrade", "ArtemisUpgrade", "ZeusUpgrade"}
order2 = {"AphroditeUpgrade", "AresUpgrade", "DemeterUpgrade", "DionysusUpgrade", "PoseidonUpgrade", "AthenaUpgrade", "ArtemisUpgrade", "ZeusUpgrade"}

order = order2

for i = #order, 1, -1 do
	god = order[i]
	data = LootData[god]
	print('    ' .. god .. ' [ shape="box", fontsize=64, style="filled", fillcolor=' .. GodColor(data) .. ' ];')
	print('    start -> ' .. god .. ' [ style="invis" ];')
end

--[[for god,data in pairs(LootData) do
	if data.LinkedUpgrades then
		print('    ' .. god .. ' [ shape="box", fontsize=64, style="filled", fillcolor=' .. GodColor(data) .. ' ];')
		if god ~= "HermesUpgrade" then
			print('    start -> ' .. god .. ' [ style="invis" ];')
		end
	end
end
]]

for god,data in pairs(LootData) do
	if data.LinkedUpgrades then
		print('  subgraph cluster' .. string.lower(god) .. ' {')
		print('    label="' .. god .. '"')
		GodTraits(god, data.WeaponUpgrades, data)
		GodTraits(god, data.Traits, data)
		GodTraits(god, data.Consumables, data)
		for toget,reqs in pairs(data.LinkedUpgrades) do
			if not printed[toget] then
				if reqs.OneOf then
					GodTrait(toget, data)
					join = JoinKey(reqs.OneOf, toget, 1)
					printed[toget] = true
				end
			end
		end
		print('  }')
	end
end
for god,data in pairs(LootData) do
	if data.LinkedUpgrades then
		for toget,reqs in pairs(data.LinkedUpgrades) do
			if not printed[toget] then
				DuoTrait(toget)
				if reqs.OneFromEachSet then
					for s,set in ipairs(reqs.OneFromEachSet) do
						join = JoinKey(set, toget, 0.5)
					end
					printed[toget] = true
				end
			end
			--print('    ' .. trait .. ';')
		end
	end
end
print('}')
