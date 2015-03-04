print(os.time() - os.clock() * 1000)
math.randomseed(os.time() - os.clock() * 1000)
print(os.date())
print()
local matrix = require 'matrix'
qs = function(A, i, k)
	-- qs(Array, 1, #Array)
	local function ChooseMedian(a,b,c)
		if a<=b and a<=c then return math.min(b,c) end
		if b<=a and b<=c then return math.min(a,c) end
		if c<=b and c<=a then return math.min(b,a) end
	end
	local function Partition(array, left, right)
		local pivotIndex = ChooseMedian(math.floor((left+right)/2), left, right)
		local pivotValue = array[pivotIndex]
		array[pivotIndex], array[right] = array[right], array[pivotIndex]
		local storeIndex = left
		for i=left,(right - 1) do
			if array[i] <= pivotValue then
				array[i], array[storeIndex] = array[storeIndex], array[i]
				storeIndex = storeIndex + 1
			end
		end
		array[storeIndex], array[right] = array[right], array[storeIndex]
		return storeIndex
	end
	if i < k then
		local p = Partition(A, i, k)
		qs(A, i, p - 1)
		qs(A, p + 1, k)
	end
end

local timer = os.clock()-os.clock()
local counter = 0
local pauser = 0
-- Matrix notation
-- For scalars -> row = state, column = asset.
-- For VCV -> table = state, subtable = VCV for assets.
local example_stats = {
	assets = 3,
	filename = "ExampleFileName",
	title = "ExampleStats",
	opt = {
		curpos = 0, -- current position
		calibration = 999,
		tail = 1,
		period = 1,
		shorting = 0, -- use -math.huge for unrestricted, 0 for no shorting
	},
	timeseries = {
		title = {"Bonds","Stocks","LPE"},
		comment = {
			"JPMorgan Global Government Bond Index, monthly, 2002-2006",
			"MSCI World Index, monthly, 2002-2006",
			"LPX 50 Index, monthly, 2002-2006",},
		datanames = {},
		data = {},}, -- not provided by paper.
	mean = matrix({{0.006649,0.007918,0.001195}}), -- markov[1].means[2] = 1st period 2nd asset's mean matrice(=vector)
	vcv = matrix({
		{ 0.000390457600000, -0.000042782106276, -0.000729144905798},
		{-0.000042782106270,  0.001319868900000,  0.001325859884118},
		{-0.000729144905798,  0.001325859884118,  0.003760142400000},}),
	stdev = matrix({{0.01976,0.03633,0.06132}}),
	skewness = matrix({{0.1781, -0.8491, -0.6236}}),
	kurtosis = matrix({{0.0620,  1.5962,  0.3320}}),
	acl = matrix({{0.0762, 0.1009, 0.1937}}),
}
local example_params = {
	-- info
	states = 2,
	assets = 3,
	-- Randomized part (most of them)
	mean = matrix({
		{-0.00257,  0.01984,  0.03921},
		{ 0.01166, -0.00875, -0.02826},}),
	stdev = matrix({
		{0.01683, 0.01575, 0.03565},
		{0.02013, 0.04621, 0.06310},}),
	cor = {
		matrix({
			{ 1.0000,  0.0380, -0.9746},
			{ 0.0380,  1.0000,  0.0354},
			{-0.9746,  0.0354,  1.0000},}),
		matrix({
			{ 1.0000,  0.1234, -0.2860},
			{ 0.1234,  1.0000,  0.6333},
			{-0.2860,  0.6333,  1.0000},}),},
	acl = matrix({{0.06734,0.09448,0.21839}}),
	p = matrix({
		{0.6068, 0.3932},
		{0.4705, 0.5295},}),
	-- Mid-calc properties
	up = matrix({0.54474,0.45525}),
	var = matrix({
			{ 0.0002832489, 0.0002480625, 0.0012709225},
			{ 0.0004052169, 0.0021353641, 0.0039816100},}),
	vcv = {
		matrix({
			{ 0.00028324890000, 0.00001007275500, -0.0005847497667},
			{ 0.00000100727550, 0.00024806250000,  0.0000198766575},
			{-0.00058474976670, 0.00001987665750,  0.0012709225000},}),
		matrix({
			{ 0.00040521690000, 0.00011478758082, -0.0003632780580},
			{ 0.00011478758082, 0.00213536410000,  0.0018466084383},
			{-0.00036327805800, 0.00184660843830,  0.0039816100000},}),
		},
	b = {},
	-- Unconditional, output properties
	u = {
		mean = {},
		vcv = {},
		stdev = {}, -- vajag?
		cor = {}, -- vajag?
		skewness = {},
		kurtosis = {},
		acl = {},
		},
}

local function randaround(constr,old, margin)
	if old < constr.min and old + margin*(constr.max-constr.min) < constr.min then
		print(string.format("Out of lower constraint, value=%f, min=%f, max=%f, old=%f", old, constr.min, constr.max, old, margin))
		error(string.format("Out of lower constraint, value=%f, min=%f, max=%f, old=%f", old, constr.min, constr.max, old, margin))
	elseif old > constr.max and old - margin*(constr.max-constr.min) > constr.max then
		print(string.format("Out of upper constraint, value=%f, min=%f, max=%f, old=%f", old, constr.min, constr.max, old, margin))
		error(string.format("Out of upper constraint, value=%f, min=%f, max=%f, old=%f", old, constr.min, constr.max, old, margin))
	end
	t = math.huge
	local reps = 10000
	while t < constr.min or t > constr.max or t==0 do 
		t = old + 2*(math.random()-0.5)*margin*(constr.max-constr.min) + (reps<5000 and 0.000000001 or 0)
		reps = reps - 1
		if reps == 0 then
			print(string.format("Locked outside of constraints, min=%f, max=%f, old=%f, margin=%f", constr.min, constr.max, old, margin))
			error(string.format("Locked outside of constraints, min=%f, max=%f, old=%f, margin=%f", constr.min, constr.max, old, margin))
		end
	end
	return t
end
local function derive_cns(old,margin)
	local cns = {
		mean = {},
		stdev = {},
		cor = {},
		acl = {},
		p = {},
	}
	for i=1,old.assets do
		cns.acl[i] = {
			min = math.max(old.cns.acl[i].min, old.acl[1][i] - old.margin * (old.cns.acl[i].max - old.cns.acl[i].min) ),
			max = math.min(old.cns.acl[i].max, old.acl[1][i] + old.margin * (old.cns.acl[i].max - old.cns.acl[i].min) ),}
	end
	for i=1,old.states do
		cns.p[i] = {}
		for j=1,old.states do
			cns.p[i][j] = {
				min = math.max(old.cns.p[i][j].min, old.p[i][j] - old.margin * (old.cns.p[i][j].max - old.cns.p[i][j].min) ),
				max = math.min(old.cns.p[i][j].max, old.p[i][j] + old.margin * (old.cns.p[i][j].max - old.cns.p[i][j].min) ),}
		end
		cns.mean[i] = {}
		cns.stdev[i] = {}
		cns.cor[i] = {}
		for j=1,old.assets do
			cns.mean[i][j] = {
				min = math.max(old.cns.mean[i][j].min, old.mean[i][j] - old.margin * (old.cns.mean[i][j].max - old.cns.mean[i][j].min) ),
				max = math.min(old.cns.mean[i][j].max, old.mean[i][j] + old.margin * (old.cns.mean[i][j].max - old.cns.mean[i][j].min) ),}
			cns.stdev[i][j] = {
				min = math.max(old.cns.stdev[i][j].min, old.stdev[i][j] - old.margin * (old.cns.stdev[i][j].max - old.cns.stdev[i][j].min) ),
				max = math.min(old.cns.stdev[i][j].max, old.stdev[i][j] + old.margin * (old.cns.stdev[i][j].max - old.cns.stdev[i][j].min) ),}
			cns.cor[i][j] = {}
			for k=1,j-1 do
				cns.cor[i][j][k] = {
					min = math.max(old.cns.cor[i][j][k].min, old.cor[i][j][k] - old.margin * (old.cns.cor[i][j][k].max - old.cns.cor[i][j][k].min) ),
					max = math.min(old.cns.cor[i][j][k].max, old.cor[i][j][k] + old.margin * (old.cns.cor[i][j][k].max - old.cns.cor[i][j][k].min) ),}
			end
		end
	end
	return cns

end

local markov = {}
function markov:new(states, assets, cns)
	o = {}
	o.cns = cns or {
		mean = { -- its contextual, should be given!
			{min = -1, max = 1},
			{min = -1, max = 1},
			{min = -1, max = 1},},
		stdev = { -- its contextual, should be given!
			{min = -1, max = 1},
			{min = -1, max = 1},
			{min = -1, max = 1},},
		cor = {min = -1, max = 1},
		acl = {min = 0, max = 1},
		p = {min = 0, max = 1},
	}
	o.states = states or 2 -- Defaults
	o.assets = assets or 3 -- Defaults
	o.u = {}
	--o.b = {}
	--o.vcv = {}
	--o.mean = {}
	--o.stdev = {}
	--o.acl = {}
	--o.p = {}

	setmetatable(o, self)
	self.__index = self
	return o
end
function markov:around(old, margin, cns)
	assert(type(old)=="table")
	assert(type(margin)=="number") -- 0.5 gives no contraction if value at middle. 1 margin gives no contraction at all.
	o = {}
	o.cns = cns or derive_cns(old,margin)
	o.states = old.states
	o.assets = old.assets
	o.margin = margin
	o.old = {
		mean = matrix.mulnum(old.mean or old.old.mean,1), -- copy tables
		stdev = matrix.mulnum(old.stdev or old.old.stdev,1),
		acl = matrix.mulnum(old.acl or old.old.acl,1),
		p = matrix.mulnum(old.p or old.old.p,1),
		cor = {}
	}
	for i=1,old.states do
		o.old.cor[i] = matrix.mulnum(old.cor and old.cor[i] or old.old.cor[i],1)
	end
	o.states = states or 2 -- Defaults
	o.assets = assets or 3 -- Defaults
	o.u = {}
	--o.b = {}
	--o.vcv = {}
	--o.mean = {}
	--o.stdev = {}
	--o.acl = {}
	--o.p = {}

	setmetatable(o, self)
	self.__index = self
	return o
end
function markov:do_umean()
	if self.old and self.old.mean then
		self.mean = matrix(self.states,self.assets)
		for i=1,self.states do
			for j=1,self.assets do
				self.mean[i][j] = randaround(self.cns.mean[i][j], self.old.mean[i][j], self.margin)
			end
		end
	else
		self.mean = matrix(self.states,self.assets,function() return math.random()*(self.cns.mean[self.assets].max-self.cns.mean[self.assets].min)+self.cns.mean[self.assets].min end)
	end
	if self.old and self.old.p then
		self.p = matrix(self.states,self.states)
		for i=1,self.states do
			for j=1,self.states do
			self.p[i][j] = randaround(self.cns.p[i][j], self.old.p[i][j], self.margin)
			end
		end
	else
		self.p = matrix(self.states,self.states,function() return math.random()*(self.cns.p.max-self.cns.p.min)+self.cns.p.min end)
	end
	local rowsums = self.p*matrix(self.states,1,1)
	rowsums = rowsums*matrix(1,self.states,1)
	self.p = matrix.ebed(self.p,rowsums)
	
	self.up = matrix({ -- unconditional probability to be in state, ( = pi) ... ONLY FOR STATES=2
		{self.p[2][1]/(self.p[1][2]+self.p[2][1])},
		{self.p[1][2]/(self.p[1][2]+self.p[2][1])},
	})	
	self.u.mean = self.up^"T"*self.mean
end
function markov:do_uvcv()
	-- base = 80%
	if self.old and self.old.acl then -- 1.20%
		self.acl = matrix(1,self.assets)
		for i=1,self.assets do
			self.acl[1][i] = randaround(self.cns.acl[i], self.old.acl[1][i], self.margin)
		end
	else
		self.acl = matrix(1,self.assets,function() return math.random()*(self.cns.acl.max-self.cns.acl.min)+self.cns.acl.min end)
	end	
	
	if self.old and self.old.stdev then -- 1.70%
		self.stdev = matrix(self.states,self.assets)
		for i=1,self.states do
			for j=1,self.assets do
				self.stdev[i][j] = randaround(self.cns.stdev[i][j], self.old.stdev[i][j], self.margin)
			end
		end
	else
		self.stdev = matrix(self.states,self.assets,function() return math.random()*(self.cns.stdev[self.assets].max-self.cns.mean[self.assets].min)+self.cns.stdev[self.assets].min end)
	end
	
	self.var = matrix.ebef(self.stdev, function(x) return x^2 end) -- 3.90%
	self.cor = {}
	if self.old and self.old.cor then
		for i=1,self.states do
			local cor = {}
			for j=1,o.assets do
				cor[j] = {}
				for k=1,o.assets do
					if j==k then cor[j][k] = 1
					elseif j<k then 
						cor[j][k] = randaround(self.cns.cor[i][k][j], self.old.cor[i][j][k], self.margin)
					elseif j>k then cor[j][k] = cor[k][j]
					end
				end
			end
			self.cor[i] = matrix(cor)
		end
	else
		for i=1,self.states do 
			local cor = {}
			for j=1,self.assets do
				cor[j] = {}
				for k=1,self.assets do
					if j==k then cor[j][k] =1
					elseif j<k then cor[j][k] = math.random()*(self.cns.cor.max-self.cns.cor.min)+self.cns.cor.min
					elseif j>k then cor[j][k] = cor[k][j]
					end
				end
			end
			self.cor[i] = matrix(cor)
		end
	end

	local uvcv = {} -- sum ~ 0.300-0.500ms :((
	for i=1,self.assets do
		uvcv[i] = {}
		for j=1,self.assets do
			local a = matrix(self.states,1) -- 0.090ms
			for k=1,self.states do
				a[k][1] = (self.mean[k][i] - self.u.mean[1][i]) * (self.mean[k][j] - self.u.mean[1][j])
			end
				
			local var = matrix(self.states,1) -- 0.150ms
			for k=1,self.states do
				var[k][1] = self.stdev[k][i] * self.stdev[k][j] * self.cor[k][i][j]
			end
			local b = var / (1 - self.acl[1][i] * self.acl[1][j])
			
			local sum = 0 -- 0.020ms
			for k=1, self.states do
				sum = sum + self.up[k][1] * (a[k][1] + b[k][1])
			end
			uvcv[i][j] = sum
		end
	end
	self.u.vcv = matrix(uvcv) -- 1.1 %
	self.u.var = matrix({matrix.getdiag(self.u.vcv)})
	self.u.stdev = matrix.ebef(self.u.var, function(x) return x^(1/2) end)
	self.u.cor = matrix.todiag(matrix.ebef(self.u.stdev, function(x) return 1/x end))*self.u.vcv*matrix.todiag(matrix.ebef(self.u.stdev, function(x) return 1/x end))
end
function markov:do_uacl()
	local b = {}
	for i=1,self.states do
		b[i] = {}
		for j=1,self.states do
			b[i][j] = self.p[j][i]*self.up[j][1]/self.up[i][1]
		end
	end
	self.b = matrix(b)
	
	local acl2 = {{}}
	for i=1,self.assets do
		local means = self.mean:subm(1,i,self.states,i)
		local umean = self.u.mean[1][i]
		local acl = self.acl[1][i]
		local var = self.var:subm(1,i,self.states,i)
		local n = 1 -- states forward. We are interested in only n=1
		local cm = means-umean*matrix(self.states,1,1) -- central mean
		local a = self.up^"T"*matrix.ebem(self.b^n*cm,cm)
		local b = self.up^"T"*acl^n*(matrix(self.states,"I")-acl^2*self.b):invert()*var
		acl2[1][i] = matrix.toscalar(a+b) / self.u.var[1][i]
	end
	self.u.acl = matrix(acl2)
end
function markov:do_uskewness()
	local skewness = {{}}
	for i=1,self.assets do
		local means = self.mean:subm(1,i,self.states,i)
		local umean = self.u.mean[1][i]
		local acl = self.acl[1][i]
		local var = self.var:subm(1,i,self.states,i)
		
		local cm = means-umean*matrix(self.states,1,1) -- central mean
		local a = self.up^"T"*matrix.ebem(matrix.ebem(cm,cm),cm)
		local b = self.up^"T"
			*3
			*acl^2
			*( 
			matrix.ebem( 
			self.b*
			(matrix(self.states,"I")
			-acl^2
			*self.b):invert()
			*var,cm))
		local c = self.up^"T"*3*matrix.ebem(cm,var)
		skewness[1][i] = matrix.toscalar((a+b+c)) / self.u.var[1][i]^(3/2)
	end
	self.u.skewness = matrix(skewness)
end
function markov:do_ukurtosis()
	local kurtosis = {{}}
	for i=1,self.assets do
		local means = self.mean:subm(1,i,self.states,i)
		local umean = self.u.mean[1][i]
		local acl = self.acl[1][i]
		local var = self.var:subm(1,i,self.states,i)
		
		local cm = means-umean*matrix(self.states,1,1) -- central mean
		local a = self.up^"T"*matrix.ebem(matrix.ebem(matrix.ebem(cm,cm),cm),cm)
		local b = self.up^"T"*6*matrix.ebem(matrix.ebem(cm,cm),var)
		local murgs = self.b*(matrix(self.states,"I")-acl^2*self.b):invert()*var
		local c = self.up^"T"*
			(matrix(self.states,"I")-acl^4*self.b):invert()*
			(3*matrix.ebem(var,var)+6*acl^2*matrix.ebem(murgs,var))
		local d = self.up^"T"*6*acl^2*matrix.ebem(matrix.ebem(murgs,cm),cm)
		kurtosis[1][i] = matrix.toscalar(a+b+c+d) / self.u.var[1][i]^(2) - 3
	end
	self.u.kurtosis = matrix(kurtosis)
end
function markov:do_uextras() -- TODO .. broken!
	self.vcv = {}
	for i=1,self.states do
		local diagStDev = matrix.todiag(self.stdev:subm(i,1,i,3))
		self.vcv[i] = diagStDev*self.cor[i]*diagStDev
	end
	--print("\n uCor")
--	self.u.cor = matrix.todiag(matrix.ebef(self.u.stdev, function(x) return 1/x end))*self.u.vcv*matrix.todiag(matrix.ebef(self.u.stdev, function(x) return 1/x end))
end
function markov:do_all()
	self:do_umean()
	self:do_uvcv()
	self:do_uacl()
	self:do_uskewness()
	self:do_ukurtosis()
	self:do_uextras()
	return self
end
function markov:uprint(stream)
	stream = stream or io.stdout
	stream:write(string.format("Markow Autoregressive (lag 1) bivariate joint "..self.assets.." asset model uncond. params"),"\n")
	stream:write(string.format("uMean,    "..string.rep("% 13.9f,",self.assets),table.unpack(self.u.mean[1])),"\n")
	local stdev = {}
	for i=1, self.assets do
		stdev[i] = self.u.vcv[i][i]^(1/2)
	stream:write(string.format("VCV ("..i.."),  "..string.rep("% 13.9f,",self.assets),table.unpack(self.u.vcv[i])),"\n")
	end
	for i=1, self.assets do
	stream:write(string.format("Cor ("..i.."),  "..string.rep("% 13.9f,",self.assets),table.unpack(self.u.cor[i])),"\n")
	end
	stream:write(string.format("St.dev,   "..string.rep("% 13.9f,",self.assets),table.unpack(stdev)),"\n")
	stream:write(string.format("Skewness, "..string.rep("% 13.9f,",self.assets),table.unpack(self.u.skewness[1])),"\n")
	stream:write(string.format("Kurtosis, "..string.rep("% 13.9f,",self.assets),table.unpack(self.u.kurtosis[1])),"\n")
	stream:write(string.format("AutoCor,  "..string.rep("% 13.9f,",self.assets),table.unpack(self.u.acl[1])),"\n")
end
function markov:cprint(stream)
	stream = stream or io.stdout
	stream:write(string.format("Markow Autoregressive (lag 1) bivariate joint "..self.assets.." asset model cond. params"),"\n")
	stream:write(string.format("uProb., "..string.rep("% 13.9f,",self.states),table.unpack(table.unpack(self.up^"T"))),"\n")
	for i=1,self.states do
		stream:write(string.format("Prob ("..i.."), "..string.rep("% 13.9f,",self.states),table.unpack(self.p[i])),"\n")
	end
	for i=1,self.states do
		stream:write(string.format("Mean,    "..string.rep("% 13.9f,",self.assets),table.unpack(self.mean[i])),"\n")
		for j=1, self.assets do
			stream:write(string.format("Cor ("..j.."),  "..string.rep("% 13.9f,",self.assets),table.unpack(self.cor[i][j])),"\n")
		end
		local stdev = {}
		for j=1, self.assets do
			stdev[j] = self.vcv[i][j][j]^(1/2)
			stream:write(string.format("VCV ("..j.."),  "..string.rep("% 13.9f,",self.assets),table.unpack(self.vcv[i][j])),"\n")
		end
		stream:write(string.format("St.dev,   "..string.rep("% 13.9f,",self.assets),table.unpack(stdev)),"\n")
	end
	stream:write(string.format("AutoCor,  "..string.rep("% 13.9f,",self.assets),table.unpack(self.acl[1])),"\n")
end

local ols = {}
function ols:new(stats, assets)
	assert(type(stats)=="table", "No stats for benchmark!")
	local o = {
		assets = assets or 3, -- Default
		stats = stats,
		total = 0,
		subtotal = {
			mean = 0,
			vcv = 0,
			skewness = 0,
			kurtosis = 0,
			acl = 0,
		},
		mean = matrix(1,assets,0),
		vcv = matrix(assets,assets,0), -- half+diag is filled, 2nd half is empty
		skewness = matrix(1,assets,0),
		kurtosis = matrix(1,assets,0),
		acl = matrix(1,assets,0),
	}
	setmetatable(o, self)
	self.__index = self
	return o
end
function ols:do_mean(estimate, switch)
	assert(type(estimate)=="table","KUKU")
	local oldsum = self.subtotal.mean
	local sum = 0
	for i=1,self.assets do
		sum = sum + (self.stats.mean[1][i]-estimate[1][i])^2
	end
	sum = switch and sum/self.assets or sum -- weighted between moments if true
	self.total = self.total -self.subtotal.mean + sum
	self.subtotal.mean = sum
	return self.total
end
function ols:do_vcv(estimate, switch)
	assert(type(estimate)=="table")
	local oldsum = self.subtotal.vcv
	local sum = 0
	local count = 0
	for i=1,self.assets do
		for j=1,i do
			sum = sum + (self.stats.vcv[i][j]-estimate[i][j])^2
			count = count + 1
		end
	end
	sum = switch and sum/count or sum -- weighted between moments if true
	self.total = self.total -self.subtotal.vcv + sum
	self.subtotal.vcv = sum
	return self.total
end
function ols:do_skewness(estimate, switch)
	assert(type(estimate)=="table")
	local oldsum = self.subtotal.skewness
	local sum = 0
	for i=1,self.assets do
		sum = sum + (self.stats.skewness[1][i]-estimate[1][i])^2
	end
	sum = switch and sum/self.assets or sum -- weighted between moments if true
	self.total = self.total -self.subtotal.skewness + sum
	self.subtotal.skewness = sum
	return self.total
end
function ols:do_kurtosis(estimate, switch)
	assert(type(estimate)=="table")
	local oldsum = self.subtotal.kurtosis
	local sum = 0
	for i=1,self.assets do
		sum = sum + (self.stats.kurtosis[1][i]-estimate[1][i])^2
	end
	sum = switch and sum/self.assets or sum -- weighted between moments if true
	self.total = self.total -self.subtotal.kurtosis + sum
	self.subtotal.kurtosis = sum
	return self.total
end
function ols:do_acl(estimate, switch)
	assert(type(estimate)=="table")
	local oldsum = self.subtotal.acl
	local sum = 0
	for i=1,self.assets do
		sum = sum + (self.stats.acl[1][i]-estimate[1][i])^2
	end
	sum = switch and sum/self.assets or sum -- weighted between moments if true
	self.total = self.total - self.subtotal.acl + sum
	self.subtotal.acl = sum
	return self.total
end
function ols:do_all(estimate, switch)
	assert(type(switch)=="boolean")
	self:do_mean(estimate.mean, switch)
	self:do_vcv(estimate.vcv, switch)
	self:do_skewness(estimate.skewness, switch)
	self:do_kurtosis(estimate.kurtosis, switch)
	self:do_acl(estimate.acl, switch)
	return self.total
end
function ols:cls()
	self.total = 0
	self.subtotal.mean = 0
	self.subtotal.vcv = 0
	self.subtotal.skewness = 0
	self.subtotal.kurtosis = 0
	self.subtotal.acl = 0
end

local rls = {}
--[[
Ordinary least-squares (OLS) errors regression minimizes the sum of squared
errors for the regression models. These regression models include all kinds of
linear and nonlinear equations that describe therelationship between a 
dependent variable and one or more independent variables. Conceptually, OLS
regression treats all errors, in the dependent variable, as having an equal 
weight, concern, andimportance.There are cases where you need to reduce the
relative (or percentage) errors. Consider the example where you are measuring
distance versus time to calculate the speed. You have distance readings 
between 10 ft,and 100 ft in increments of 5 feet. Repeated measurements tell
you that you get an error of 0.5 ft for each reading. Of course, an error of
0.5 ft for a 10 ft reading is more serious than for that for the higher 
readings. You like to perform a curve fit that minimizes the least-squares
relative errors so that you regression model gives better prediction for
smaller distance values. This is where least-squares relative error(LSRE) 
regression is relevant.
--]]
function rls:new(stats, assets)
	assert(type(stats)=="table", "No stats for benchmark!")
	local o = {
		assets = assets or 3, -- Default
		stats = stats,
		total = 0,
		subtotal = {
			mean = 0,
			vcv = 0,
			skewness = 0,
			kurtosis = 0,
			acl = 0,
		},
		mean = matrix(1,assets,0),
		vcv = matrix(assets,assets,0), -- half+diag is filled, 2nd half is empty
		skewness = matrix(1,assets,0),
		kurtosis = matrix(1,assets,0),
		acl = matrix(1,assets,0),
	}
	setmetatable(o, self)
	self.__index = self
	return o
end
function rls:do_mean(estimate, switch)
	assert(type(estimate)=="table","KUKU")
	local oldsum = self.subtotal.mean
	local sum = 0
	for i=1,self.assets do
--		sum = sum + (1-estimate[1][i]/self.stats.mean[1][i])^2
		sum = sum + ((self.stats.mean[1][i]-estimate[1][i])*100000)^2
	end
	sum = switch and sum/self.assets or sum -- weighted between moments if true
	self.total = self.total -self.subtotal.mean + sum
	self.subtotal.mean = sum
	return self.total
end
function rls:do_vcv(estimate, switch)
	assert(type(estimate)=="table")
	local oldsum = self.subtotal.vcv
	local sum = 0
	local count = 0
	for i=1,self.assets do
		for j=1,i do
			--sum = sum + (1-estimate[i][j]/self.stats.vcv[i][j])^2
			sum = sum + (1-estimate[1][i]/0.0001)^2
			count = count + 1
		end
	end
	sum = switch and sum/count or sum -- weighted between moments if true
	self.total = self.total -self.subtotal.vcv + sum
	self.subtotal.vcv = sum
	return self.total
end
function rls:do_stdev(estimate, switch)
	assert(type(estimate)=="table","KUKU")
	local oldsum = self.subtotal.mean
	local sum = 0
	for i=1,self.assets do
--		sum = sum + (1-estimate[1][i]/self.stats.mean[1][i])^2
		sum = sum + ((self.stats.stdev[1][i]-estimate[1][i])*1000)^2
	end
	sum = switch and sum/self.assets or sum -- weighted between moments if true
	self.total = self.total -self.subtotal.mean + sum
	self.subtotal.mean = sum
	return self.total
end
function rls:do_cor(estimate, switch)
	assert(type(estimate)=="table")
	local oldsum = self.subtotal.vcv
	local sum = 0
	local count = 0
	for i=1,self.assets do
		for j=1,i-1 do
			--sum = sum + (1-estimate[i][j]/self.stats.vcv[i][j])^2
			sum = sum + ((self.stats.cor[i][j]-estimate[1][i])/2)^2
			count = count + 1
		end
	end
	sum = switch and sum/count or sum -- weighted between moments if true
	self.total = self.total -self.subtotal.vcv + sum
	self.subtotal.vcv = sum
	return self.total
end
function rls:do_skewness(estimate, switch)
	assert(type(estimate)=="table")
	local oldsum = self.subtotal.skewness
	local sum = 0
	for i=1,self.assets do
		--sum = sum + (1-estimate[1][i]/self.stats.skewness[1][i])^2
		sum = sum + ((self.stats.skewness[1][i]-estimate[1][i])*1)^2
	end
	sum = switch and sum/self.assets or sum -- weighted between moments if true
	self.total = self.total -self.subtotal.skewness + sum
	self.subtotal.skewness = sum
	return self.total
end
function rls:do_kurtosis(estimate, switch)
	assert(type(estimate)=="table")
	local oldsum = self.subtotal.kurtosis
	local sum = 0
	for i=1,self.assets do
		--sum = sum + (1-estimate[1][i]/self.stats.kurtosis[1][i])^2
		sum = sum + ((self.stats.kurtosis[1][i]-estimate[1][i])*1/10)^2
	end
	sum = switch and sum/self.assets or sum -- weighted between moments if true
	self.total = self.total -self.subtotal.kurtosis + sum
	self.subtotal.kurtosis = sum
	return self.total
end
function rls:do_acl(estimate, switch)
	assert(type(estimate)=="table")
	local oldsum = self.subtotal.acl
	local sum = 0
	for i=1,self.assets do
		--sum = sum + (1-estimate[1][i]/self.stats.acl[1][i])^2
		sum = sum + ((self.stats.acl[1][i]-estimate[1][i])*10)^2
	end
	sum = switch and sum/self.assets or sum -- weighted between moments if true
	self.total = self.total - self.subtotal.acl + sum
	self.subtotal.acl = sum
	return self.total
end
function rls:do_all(estimate, switch)
	assert(type(switch)=="boolean")
	self:do_mean(estimate.mean, switch)
	self:do_vcv(estimate.vcv, switch)
	self:do_skewness(estimate.skewness, switch)
	self:do_kurtosis(estimate.kurtosis, switch)
	self:do_acl(estimate.acl, switch)
	return self.total
end
function rls:do_all2(estimate)
	self:do_mean(estimate.mean, switch)
	self:do_stdev(estimate.stdev, switch)
	self:do_cor(estimate.cor, switch)
	self:do_skewness(estimate.skewness, switch)
	self:do_kurtosis(estimate.kurtosis, switch)
	self:do_acl(estimate.acl, switch)
	return self.total
end
function rls:get_constraints(spreadmean, spreadstdev, states)
	local con = {
		mean = {},
		stdev = {},
		cor = {},
		acl = {},
		p = {},
	}
	for i=1,self.assets do
		con.acl[i] = {
			min = -1,
			max = 1,}
	end
	for i=1,states do
		con.p[i] = {}
		for j=1,states do
			con.p[i][j] = {
				min = 0,
				max = 1,}
		end
		con.mean[i] = {}
		con.stdev[i] = {}
		con.cor[i] = {}
		for j=1,self.assets do
			con.mean[i][j] = {
				min = self.stats.mean[1][j]-spreadmean*self.stats.stdev[1][j],
				max = self.stats.mean[1][j]+spreadmean*self.stats.stdev[1][j],}
			con.stdev[i][j] = {
				min = self.stats.stdev[1][j]/spreadstdev,
				max = self.stats.stdev[1][j]*spreadstdev,}
			con.cor[i][j] = {}
			for k=1,j-1 do
				con.cor[i][j][k] = {
					min = -1,
					max = 1,}
			end
		end
	end
	return con
end
function rls:get_initguess(filename)
	if not filename then return {
			states = 2,
			assets = 3,
			margin = 0.5,
			old = {
				mean = matrix({
					{self.stats.mean[1][1], self.stats.mean[1][2], self.stats.mean[1][3]},
					{self.stats.mean[1][1], self.stats.mean[1][2], self.stats.mean[1][3]},
					}),
				stdev = matrix({
					{self.stats.stdev[1][1], self.stats.stdev[1][2], self.stats.stdev[1][3]},
					{self.stats.stdev[1][1], self.stats.stdev[1][2], self.stats.stdev[1][3]},
					}),
				cor = {
					matrix({
						{1, 0, 0},
						{0, 1, 0},
						{0, 0, 1},}),
					matrix({
						{1, 0, 0},
						{0, 1, 0},
						{0, 0, 1},}),
					},
				acl = matrix({{0.5, 0.5, 0.5}}),
				p = matrix({
					{0.5, 0.5},
					{0.5, 0.5},}),
			},
		}
	else
		local f = assert(io.open(filename, "r"))
		local t = {
			states = 2,
			assets = 3,
			margin = 0.5,
			old = {
				mean = matrix(2,3,0),
				stdev = matrix(2,3,0),
				cor = {matrix(3,3,0),matrix(3,3,0)},
				acl = matrix(1,3,0),
				p = matrix(2,2,0),}}
		_,_,_,t.score = string.match(f:read("*line"), string.rep("(.-);",4))
		for i=1,14 do f:read("line") end
		_,t.old.p[1][1],t.old.p[1][2] = string.match(f:read("*line"), string.rep("(.-),",3))
		_,t.old.p[2][1],t.old.p[2][2] = string.match(f:read("*line"), string.rep("(.-),",3))
		_,t.old.mean[1][1],  t.old.mean[1][2],  t.old.mean[1][3]   = string.match(f:read("*line"), string.rep("(.-),",4))
		_,t.old.cor[1][1][1],t.old.cor[1][1][2],t.old.cor[1][1][3] = string.match(f:read("*line"), string.rep("(.-),",4))
		_,t.old.cor[1][2][1],t.old.cor[1][2][2],t.old.cor[1][2][3] = string.match(f:read("*line"), string.rep("(.-),",4))
		_,t.old.cor[1][3][1],t.old.cor[1][3][2],t.old.cor[1][3][3] = string.match(f:read("*line"), string.rep("(.-),",4))
		f:read("line")
		f:read("line")
		f:read("line")
		_,t.old.stdev[1][1], t.old.stdev[1][2], t.old.stdev[1][3]  = string.match(f:read("*line"), string.rep("(.-),",4))
		_,t.old.mean[2][1],  t.old.mean[2][2],  t.old.mean[2][3]   = string.match(f:read("*line"), string.rep("(.-),",4))
		_,t.old.cor[2][1][1],t.old.cor[2][1][2],t.old.cor[2][1][3] = string.match(f:read("*line"), string.rep("(.-),",4))
		_,t.old.cor[2][2][1],t.old.cor[2][2][2],t.old.cor[2][2][3] = string.match(f:read("*line"), string.rep("(.-),",4))
		_,t.old.cor[2][3][1],t.old.cor[2][3][2],t.old.cor[2][3][3] = string.match(f:read("*line"), string.rep("(.-),",4))
		f:read("line")
		f:read("line")
		f:read("line")
		_,t.old.stdev[2][1], t.old.stdev[2][2], t.old.stdev[2][3]  = string.match(f:read("*line"), string.rep("(.-),",4))
		_,t.old.acl[1][1], t.old.acl[1][2], t.old.acl[1][3]  = string.match(f:read("*line"), string.rep("(.-),",4))
		f:close()
		return t		
	end
end
function rls:cls()
	self.total = 0
	self.subtotal.mean = 0
	self.subtotal.vcv = 0
	self.subtotal.skewness = 0
	self.subtotal.kurtosis = 0
	self.subtotal.acl = 0
end


local stats = {} -- See attached files for examples
function stats:new(filename,assets)
	assert(type(filename)=="string","No filename for CSV provided")
	local o = {
		assets = assets or 3, -- Default
		filename = filename,
		title = "",
		opt = {
			curpos = 0, -- current position
			calibration = 1,
			tail = 1,
			period = 1,
			shorting = 0, -- use -math.huge for unrestricted, 0 for no shorting
		},
		timeseries = {
			title = {},
			comment = {},
			datanames = {},
			data = {},
		},
		mean = {}, -- markov[1].means[2] = 1st period 2nd asset's mean matrice(=vector)
		vcv = {},
		stdev = {},
		skewness = {},
		kurtosis = {},
		acl = {},
	}
	setmetatable(o, self)
	self.__index = self
	return o
end	
function stats:readfile(curpos, skip) 
    local f = assert(io.open(self.filename, "r"))
	self.title,	self.opt.calibration, self.opt.tail, self.opt.period, self.opt.shorting
			= string.match(f:read("*line"), ".-;"..string.rep("(.-);",5))
	self.timeseries.title = {string.match(f:read("*line"), ".-;"..string.rep("(.-);",self.assets))}
	self.timeseries.comments = {string.match(f:read("*line"), ".-;"..string.rep("(.-);",self.assets))}
    local count = 1
	while true do
		local line = f:read("*line")
		if line == nil then break end
		self.timeseries.data[count] = {string.match(line, ".-;"..string.rep("(.-);",self.assets))}
		count = count + 1
	end
end
function stats:do_mean() 
	for i =1,self.assets do
		local sum = 0
		for j = math.max(1,self.opt.calibration+self.opt.curpos-self.opt.tail),self.opt.calibration+self.opt.curpos do 
			sum = sum + self.timeseries.data[j][i]
		end
		self.mean[i] = sum / (1+self.opt.calibration+self.opt.curpos - math.max(1,self.opt.calibration+self.opt.curpos-self.opt.tail))
		print((1+self.opt.calibration+self.opt.curpos - math.max(1,self.opt.calibration+self.opt.curpos-self.opt.tail)))
	end
	self.mean = matrix({self.mean})
end
function stats:do_vcv()
	for i =1,self.assets do
		self.vcv[i] = {}
		for j = 1,self.assets do
			local sum = 0
			if i>j then
				self.vcv[i][j] = self.vcv[j][i]
			else
				for k = math.max(1,self.opt.calibration+self.opt.curpos-self.opt.tail),self.opt.calibration+self.opt.curpos do 
					sum = sum + (self.timeseries.data[k][i]-self.mean[1][i])*(self.timeseries.data[k][j]-self.mean[1][j])
				end
				self.vcv[i][j] = sum/(1 + self.opt.calibration+self.opt.curpos - math.max(1,self.opt.calibration+self.opt.curpos-self.opt.tail))
			end
		end
		self.stdev[i] = self.vcv[i][i]^(1/2)
	end
	self.vcv = matrix(self.vcv)
	self.stdev = matrix({self.stdev})
	self.cor = matrix.todiag(matrix.ebef(self.stdev, function(x) return 1/x end))*self.vcv*matrix.todiag(matrix.ebef(self.stdev, function(x) return 1/x end))

end
function stats:do_skewness()
	for i = 1,self.assets do
		local sum = 0
		for k = math.max(1,self.opt.calibration+self.opt.curpos-self.opt.tail),self.opt.calibration+self.opt.curpos do 
			sum = sum + (self.timeseries.data[k][i]-self.mean[1][i])^3
		end
		self.skewness[i] = sum / (1+self.opt.calibration+self.opt.curpos-math.max(1,self.opt.calibration+self.opt.curpos-self.opt.tail)) / (self.vcv[i][i])^(3/2)
	end
	self.skewness = matrix({self.skewness})
end
function stats:do_kurtosis()
	for i = 1,self.assets do
		local sum = 0
		for k = math.max(1,self.opt.calibration+self.opt.curpos-self.opt.tail),self.opt.calibration+self.opt.curpos do 
			sum = sum + (self.timeseries.data[k][i]-self.mean[1][i])^4
		end
		self.kurtosis[i] = sum / (1+self.opt.calibration+self.opt.curpos-math.max(1,self.opt.calibration+self.opt.curpos-self.opt.tail)) / (self.vcv[i][i])^2
	end
	self.kurtosis = matrix({self.kurtosis})
end
function stats:do_acl()
	for i = 1,self.assets do
		local sum = 0
		for k = math.max(1,self.opt.calibration+self.opt.curpos-self.opt.tail),self.opt.calibration+self.opt.curpos-1 do 
			sum = sum + (self.timeseries.data[k][i]-self.mean[1][i])*(self.timeseries.data[k+1][i]-self.mean[1][i])
		end
		self.acl[i] = sum / (1-1+self.opt.calibration+self.opt.curpos-math.max(1,self.opt.calibration+self.opt.curpos-self.opt.tail)) / (self.vcv[i][i])
	end
	self.acl = matrix({self.acl})
end
function stats:do_all() 
	self:do_mean()
	self:do_vcv()
	self:do_skewness()
	self:do_kurtosis()
	self:do_acl()
	return self
end
function stats:print(stream)
	stream = stream or io.stdout
	stream:write("Statistical description of data for: ",self.title,"\n")
	stream:write(string.format("Calib/Tail/Per/Cur, %d, %d, %d, %d,",self.opt.calibration,self.opt.tail,self.opt.period,self.opt.curpos),"\n")
	stream:write(string.format("Title,    "..string.rep("%-13s,",self.assets),table.unpack(self.timeseries.title)),"\n")
	stream:write(string.format("Mean,     "..string.rep("% 13.9f,",self.assets),table.unpack(self.mean[1])),"\n")
	local stdev = {}
	for i=1, self.assets do
		stdev[i] = self.vcv[i][i]^(1/2)
	stream:write(string.format("VCV ("..i.."),  "..string.rep("% 13.9f,",self.assets),table.unpack(self.vcv[i])),"\n")
	end
	stream:write(string.format("St.dev,   "..string.rep("% 13.9f,",self.assets),table.unpack(stdev)),"\n")
	stream:write(string.format("Skewness, "..string.rep("% 13.9f,",self.assets),table.unpack(self.skewness[1])),"\n")
	stream:write(string.format("Kurtosis, "..string.rep("% 13.9f,",self.assets),table.unpack(self.kurtosis[1])),"\n")
	stream:write(string.format("AutoCor,  "..string.rep("% 13.9f,",self.assets),table.unpack(self.acl[1])),"\n")
end
setmetatable(example_stats, stats)
stats.__index = stats

--------------- REAL STUFF --------------------
--

local starttime = os.clock()
math.randomseed(os.clock())

--local rd = stats:new("ex1.csv",3)
local rd = stats:new("DataSet-ONE-2.csv",3)
rd:readfile()
rd:do_all()
rd:print()
--[[
local check = rls:new(rd,3)
local markovguess = markov:around(check:get_initguess(),0.001,check:get_constraints(1,5,2))
for k,v in pairs(markovguess.old) do print(k,v) end
markovguess:do_all()
markovguess:uprint()
print("\nAround from markov....")
markovguess = markov:around(markovguess,0.1)
markovguess:do_all()
markovguess:uprint()
--]]
--
local ferr = assert(io.open("errors.txt", "w"))

local ktime = os.clock()
local errmsg = function(markovtable, i, title)
		print("Try:",i,"Error in function:",title)
		ferr:write("\nTry:",i," Error!\n")
		if not pcall(markovtable.cprint, markovtable, ferr) then ferr:write("...error while printing\n") end
		if not pcall(markovtable.uprint, markovtable, ferr) then ferr:write("...error while printing\n") end
end

function mc(delta, margin, size, reps, lowest)
	local check = rls:new(rd,3)
	local function mc_around(hof,size,reps,margin, lowest)
		-- upvalues: check,
		local lowest = lowest or math.huge
		local tof = {} --temple of fame
		for i=1,size do
			tof[i] = markov:around(hof[i],0)
			tof[i]:do_all()
		end
		local tor = {[0] = 0} -- temple of records
		for i=1,size do
			tor[i] = hof.scores[i] or lowest
		end

		for h=1,hof.size do
			local perch = tor[h] or lowest
			local cns = derive_cns(hof[h],margin)
			local counts = {0,0,0,0,0}
			local i = 1
			while i<=reps or ((tor[h]==(hof.scores[h] or lowest)) and i<=reps*10) do
				check:cls() -- 0.12%
				math.randomseed(os.time() - os.clock() * 1000) -- 0.15%
				local markovguess = markov:around(hof[h],margin,cns) -- 4.45%
				markovguess:do_umean() -- 12.00%
				if check:do_mean(markovguess.u.mean, true) < perch then
				counter = os.clock()
					counts[1] = counts[1] + 1 -- 0.001% x GetIn
					markovguess:do_uvcv() -- 55% !!!!!!!!!!!!!
					check:do_stdev(markovguess.u.stdev)
				timer = timer + (os.clock()-counter)
					if check:do_cor(markovguess.u.cor, true) < perch then
						counts[2] = counts[2] + 1
						markovguess:do_uacl()
						if check:do_acl(markovguess.u.acl, true) < perch then
							counts[3] = counts[3] + 1
							markovguess:do_uskewness()
							if check:do_skewness(markovguess.u.skewness, true) < perch then
								counts[4] = counts[4] + 1
								markovguess:do_ukurtosis()
								if check:do_kurtosis(markovguess.u.kurtosis, true) < perch then
									counts[5] = counts[5] + 1
									markovguess:do_uextras()
									tof[h] = markovguess
									tor[h] = check.total
									perch = check.total
									--print(string.format("Try:%9d, %3d/%3d, Place=%3d, RLS=%9.7f, Perch=%9.7f",i,h,#hof,pos,check.total,perch))
								end
							end
						end
					end
				end
				i = i + 1
				if not(i<=reps or ((tor[h]==(hof.scores[h] or lowest)) and i<=reps*10)) then 
					--local diff = os.difftime(ktime,os.clock())
					print(string.format("Try:%7d, %3d/%3d, Perch: %8.5f, Updates=%2d/%2d, %5.3fs/%3dm%02ds",i-1,h,hof.size,perch, counts[1], counts[5], os.clock() - ktime, math.floor((os.clock() - starttime)/60),math.floor((os.clock()-starttime)%60)))
					--print(timer/(os.clock()-ktime)*100,"%",table.unpack(counts))
					timer = os.clock()-os.clock()
					ktime = os.clock()
				end
			end
		end
		
		local candidates = {}
		local scores = {}
		local scoresold = {}
		
		for i=1,hof.size do
				candidates[i] = tof[i]
				scores[i] = tor[i] or lowest
				scoresold[i] = tor[i] or lowest
		end
		
		qs(scores,1,#scores)

		local winners = {scores={}, size=size}
		local length = hof.size
		for i=1,size do
			for j,v in pairs(scoresold) do
				if scores[i] == v then
					winners[i] = candidates[j]
					winners.scores[i] = scores[i]
					candidates[j] = nil
					scoresold[j] = nil
					break
				end
			end
			--[[
			for j=1,length+1 do
				if scores[i] == scoresold[j] then
					winners[i] = candidates[j]
					winners.scores[i] = scores[i]
					candidates[j] = candidates[length]
					candidates[length] = nil
					scoresold[j] = scoresold[length]
					scoresold[length] = nil
					length = length -1
				end
			end
			--]]
			if not winners[i] then
				print("MISSING", i, scores[i])
				for k,v in pairs(winners[i] or {}) do print(k,v) end
			end
		end
		return winners
	end

	local margin = margin or 0.5 -- divided by i
	local size = size or 100 -- divided by i
	local reps = reps or 1000 -- multiplied by i
	local lowest = lowest or 1000
	
	local hof = {size=size, scores={}} -- hall of fame
	local inits = check:get_initguess()--"initdata-TWO3.txt")
	local cons = check:get_constraints(1,2,2)
	for i=1,hof.size do 
		hof[i] = markov:around(inits,margin,cons)
		hof.scores[i] = inits.score and inits.score * (1+9*(i-1)/hof.size) or nil
		hof[i]:do_all()
	end

	local i = delta or 2
	local count = 1
	local top = 99999999
	while true do
		hof = mc_around(hof,math.ceil(size),math.ceil(reps),margin,lowest)
		--for k,v in ipairs(hof) do print(k,v) end
		print(string.format("Size=%3d, Reps=%5d, Margin=%7.5f, Scores=%7.5f-%7.5f",math.ceil(size),math.ceil(reps),margin,hof.scores[1],hof.scores[math.ceil(size)]))

		local ffound = assert(io.open("found"..count.."-"..math.random(1,99)..".txt", "w"))
		ffound:write("Round No; ",count,"; best result RLS; ",hof.scores[1],"\n\n;;;\n")
		rd:print(ffound)
		for i=1,math.ceil(size) do
			ffound:write("No.; ",i,"; Score; ",hof.scores[i],";\n")
			hof[i]:uprint(ffound)
			hof[i]:cprint(ffound)
			ffound:write("\n")
		end
		ffound:close()

		--if (top - hof.scores[i])/top < 0.05 then break end
		top = hof.scores[50] and hof.scores[5] or hof.scores[3] or hof.score[1]
		margin = margin / 1.3
		size = size*(1 - 0.8/(count+1))
		reps = reps + reps/(count+1)
		count = count + 1
	end
	return hof
end

local hof = mc(2,0.30,1000,1000,100000)

print(os.date())
print("\nElapsed total", os.clock() - starttime)
--print("Levels:",table.unpack(counts))
print("Halloffame:")
for i=1,hof.size do print(i,"RLS = ",hof.scores[i]) end
print()
rd:print()
print("\n1st place RLS:",hof.scores[1])
hof[1]:uprint()
hof[1]:uprint(ffound)
print()
hof[1]:cprint()
hof[1]:cprint(ffound)

ferr:close()

--]]
--------------- DEBUG -------------------------
--[[

-- Test sample data
local test = markov:around(example_params, 0, constraints)
test:do_all()
local test_ols = ols:new(example_stats,3)
print("\nPaper precision")
print("OLS weighted", (test_ols:do_all(test.u, true)-test_ols.subtotal.mean)*15/4)  --*18/5
print("OLS pure", test_ols:do_all(test.u, false)-test_ols.subtotal.mean)
local test_rls = rls:new(example_stats,3)
print("RLS weighted", (test_rls:do_all(test.u, true)-test_rls.subtotal.mean)*15/4)  -- *18/5
print("RLS pure", test_rls:do_all(test.u, false)-test_rls.subtotal.mean)
print("\nStats")
example_stats:print()
print("Orig")
test:uprint()
--test:cprint()
rd = example_stats
local spread = 2
local spreadstdev = 5
test = markov:around(test, 1)
test:do_all()

print("\nNew")
local t2 = markov:around(initguess,0.1,constraints)
t2:do_all()
t2:uprint()
--]=]
--[=[
local ex1 = stats:new("ex1.csv",3)
ex1:readfile()
ex1:do_all()
ex1:print()

local mg = markov:new(2,3)
mg:do_all()
mg:cprint()
mg:uprint()
--]=]


--]]





