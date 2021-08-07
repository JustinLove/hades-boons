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

local function TableOfValues(t)
	local rows = {}
	for k,v in pairs(t) do
		table.insert(rows, String(k) .. ':' .. v)
	end
	return '{' .. table.concat(rows, ', ') .. '}'
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

local function TraitDescription(id)
	for i,data in ipairs(Texts) do
		if data.Id == id then
			if data.Description then
				local x = string.find(data.Description, "\n", 1, true)
				if x then
					return string.sub(data.Description, 1, x-2)
				else
					return data.Description
				end
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

local function TraitSlot(trait)
	if TraitData[trait] then
		if TraitData[trait].Slot then
			return TraitData[trait].Slot
		elseif TraitData[trait].InheritFrom then
			for i,parent in ipairs(TraitData[trait].InheritFrom) do
				slot = TraitSlot(parent)
				if slot then
					return slot
				end
			end
		end
	end
end

local function TraitRequiredSlotted(trait)
	if TraitData[trait] then
		if TraitData[trait].RequiredSlottedTrait then
			return TraitData[trait].RequiredSlottedTrait
		elseif TraitData[trait].InheritFrom then
			for i,parent in ipairs(TraitData[trait].InheritFrom) do
				slot = TraitRequiredSlotted(parent)
				if slot then
					return slot
				end
			end
		end
	end
end

local function TraitRequiredTraits(trait)
	if TraitData[trait] then
		if TraitData[trait].RequiredOneOfTraits then
			return TraitData[trait].RequiredOneOfTraits
		elseif TraitData[trait].RequiredTrait then
			return {TraitData[trait].RequiredTrait}
		elseif TraitData[trait].InheritFrom then
			for i,parent in ipairs(TraitData[trait].InheritFrom) do
				upvalue = TraitRequiredTraits(parent)
				if upvalue then
					return upvalue
				end
			end
		end
	end
end

local function TraitRequiredFalseTraits(trait)
	if TraitData[trait] then
		if TraitData[trait].RequiredFalseTraits then
			return TraitData[trait].RequiredFalseTraits
		elseif TraitData[trait].RequiredFalseTrait then
			return {TraitData[trait].RequiredFalseTrait}
		elseif TraitData[trait].TraitDependencyTextOverrides then
			exclude = {}
			for k,v in pairs(TraitData[trait].TraitDependencyTextOverrides) do
				table.insert(exclude, k)
			end
			return exclude
		end
	end
end

local function TraitRequiredMetaUpgradeSelected(trait)
	if TraitData[trait] then
		if TraitData[trait].RequiredMetaUpgradeSelected then
			return TraitData[trait].RequiredMetaUpgradeSelected
		elseif TraitData[trait].InheritFrom then
			for i,parent in ipairs(TraitData[trait].InheritFrom) do
				upvalue = TraitRequiredMetaUpgradeSelected(parent)
				if upvalue then
					return upvalue
				end
			end
		end
	end
end

tooltipUsed = {"AddShout.SuperDuration","AmmoFieldWeapon.Interval.Min","DisplayDelta1","NewTotal1","TooltipAffectChance","TooltipChillStacks","TooltipCritChance","TooltipCriticalChance","TooltipDeathThreshold","TooltipDefense","TooltipDelay","TooltipDuration","TooltipRequiredPoisonedEnemies","TooltipRoomInterval","TooltipSpeedBoost","TooltipSpreadRate","TooltipThreshold","TooltipDamageTaken","TooltipSuperGain"}

local function TooltipData(obj, tooltipData)
	if obj.TooltipSpreadRate then
		tooltipData.TooltipSpreadRate = obj.TooltipSpreadRate
	end
	if obj.AddShout and obj.AddShout.SuperDuration then
		if type(obj.AddShout.SuperDuration) == 'number' then
			tooltipData['AddShout.SuperDuration'] = obj.AddShout.SuperDuration
		else 
			tooltipData['AddShout.SuperDuration'] = obj.AddShout.SuperDuration.BaseValue
		end
	end
	if obj.AmmoFieldWeapon and obj.AmmoFieldWeapon.Interval and obj.AmmoFieldWeapon.Interval.Min then
		tooltipData['AmmoFieldWeapon.Interval.Min'] = obj.AmmoFieldWeapon.Interval.Min
	end
	if obj.ExtractValues then
		for i,extract in ipairs(obj.ExtractValues) do
			if obj[extract.Key] then
				for u,t in ipairs(tooltipUsed) do
					if t == extract.ExtractAs then
						value = obj[extract.Key]
						if type(value) == 'table' then
							value = value.BaseValue
						end
						if extract.Format == "Percent" then
							tooltipData[extract.ExtractAs] = obj[extract.Key]*100
						elseif extract.Format == "NegativePercentDelta" then
							tooltipData[extract.ExtractAs] = 100 - (obj[extract.Key]*100)
						else
							tooltipData[extract.ExtractAs] = obj[extract.Key]
						end
					end
				end
			elseif extract.BaseName == 'DemeterProjectile' then  
				tooltipData[extract.ExtractAs] = 5
			end
		end
	end
	if obj.ExtractValue then
		extract = obj.ExtractValue
		value = nil
		if obj.ChangeValue then
			value = obj.ChangeValue
		end
		if obj.BaseValue then
			value = obj.BaseValue
		end
		if value then
			for u,t in ipairs(tooltipUsed) do
				if t == extract.ExtractAs then
					if extract.Format == "Percent" then
						tooltipData[extract.ExtractAs] = value*100
					else
						tooltipData[extract.ExtractAs] = value
					end
				end
			end
		end
	end
end

local function TraitTooltipData(trait)
	if TraitData[trait] then
		tooltipData = {}
		TooltipData(TraitData[trait], tooltipData)
		if TraitData[trait].AddOnEffectWeapons then
			TooltipData(TraitData[trait].AddOnEffectWeapons, tooltipData)
		end
		if TraitData[trait].OnDamageEnemyFunction then
			TooltipData(TraitData[trait].OnDamageEnemyFunction.FunctionArgs, tooltipData)
		end
		if TraitData[trait].AddIncomingDamageModifier then
			TooltipData(TraitData[trait].AddIncomingDamageModifier, tooltipData)
		end
		if TraitData[trait].AddIncomingDamageModifiers then
			TooltipData(TraitData[trait].AddIncomingDamageModifiers, tooltipData)
		end
		if TraitData[trait].PropertyChanges then
			for i,prop in ipairs(TraitData[trait].PropertyChanges) do
				TooltipData(prop, tooltipData)
			end
		end
		if next(tooltipData) then
			return tooltipData
		end
	elseif ConsumableData[trait] then
		tooltipData = {}
		TooltipData(ConsumableData[trait], tooltipData)
		if next(tooltipData) then
			return tooltipData
		end
	end
	return {}
end

local function GodTrait(trait, extra)
	local name = trait
	local image = ""
	local tname = TraitName(trait)
	local desc = TraitDescription(trait)
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
	slot = TraitSlot(trait)
	if slot then
		item = item .. '        "slot": ' .. String(slot) .. ',\n'
	end
	reqSlot = TraitRequiredSlotted(trait)
	if reqSlot then
		item = item .. '        "RequiredSlottedTrait": ' .. String(reqSlot) .. ',\n'
	end
	reqTraits = TraitRequiredTraits(trait)
	if reqTraits then
		item = item .. '        "OneOf": ' .. ArrayOfStrings(reqTraits) .. ',\n'
	end
	falseTraits = TraitRequiredFalseTraits(trait)
	if falseTraits then
		RemoveFromList(falseTraits, trait)
		if #falseTraits > 0 then
			item = item .. '        "RequiredFalseTraits": ' .. ArrayOfStrings(falseTraits) .. ',\n'
		end
	end
	meta = TraitRequiredMetaUpgradeSelected(trait)
	if meta then
		item = item .. '        "RequiredMetaUpgradeSelected": ' .. String(meta) .. ',\n'
	end
	tooltipData = TraitTooltipData(trait)
	if tooltipData then
		item = item .. '        "TooltipData": ' .. TableOfValues(tooltipData) .. ',\n'
	end
	item = item .. '        "trait": ' .. String(trait) .. ',\n'
	item = item .. '        "name": ' .. String(name) .. ',\n'
	item = item .. '        "description": ' .. String(desc) .. '\n'
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

order0 = {"ZeusUpgrade", "PoseidonUpgrade", "AthenaUpgrade", "AphroditeUpgrade", "ArtemisUpgrade", "AresUpgrade", "DionysusUpgrade", "HermesUpgrade", "DemeterUpgrade"}
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
			if not printed[toget] then
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
				printed[toget] = true
			end
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
