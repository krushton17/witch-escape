pico-8 cartridge // http://www.pico-8.com
version 27
__lua__
--system functions===============================

--initialize frame counter
frame=0

--keep track of all objects with tables
npcs,npcs_onscreen,particles,pickups,beams = {},{},{},{},{}
bgs,fgs={},{}

function _init()
	--implement spawn points, etc.
	map_init()

	title=true
	cls()
	print('welcome to witch escape!')
	print('')
	print('collect spell books and')
	print('escape the dungeon!')
	print('')
	print('\151:jump')
	print('\142:cast spell')
	print('\139,\145:move')
	print('\148:look/aim upward')
	print('\131:change spell')
	print('')
	print('press any key to begin!')
	flip()
	-- cam = {
	-- 	x=p.x-56,
	-- 	y=p.y-56,
	-- 	--placeholders if i decide to implement a more complex camera engine
	-- 	-- target_x=p.x-56,
	-- 	-- target_y=p.y-56,
	-- 	-- box_w=28,
	-- 	-- box_margin=84
	-- }
	cam_x=p.x-56
	cam_y=p.y-56
end

function _update()
	debug()
	if title then
		if(btn()>0)title=nil
		return
	end
	-- forget this; just know that when you reach the door to the outside you win. lol
	-- if win then
	-- 	cls()
	-- 	print('yay!')
	-- 	print('you won!')
	-- end
	
	--increment frame counter; reset if over 1000
	frame=(frame+1)%1000
	--apply transformation spells
	if(replace)replace_me()

	--update everything
	--reset this on every update
	npcs_onscreen={}
	foreach(npcs, function(npc)
		--ignore off-screen npcs, except eagles that have flown away and need to be removed
		if(npc.type!='eagle' and off_screen(npc))return
		--don't update stoned npcs
		if(not npc.stoned)npc:update()
		--for draw calls and player-npc collisions
		add(npcs_onscreen,npc)
	end)
	foreach(particles, function(p) p:update() end)
	foreach(beams, function(b) b:update() end)
	foreach(torches, function(t) 
		if(off_screen(t))return
		t:update()
	end)
	p:update()
end--end _update()

function _draw()
	if(title)return
	--clear the screen
	cls(1)
	--lock camera to player
	camera_update()

	--background
	palt(0,false)
	map(0,0,0,0,128,64,2^5)
	palt()
	foreach(bgs,function(e)
		if(not off_screen(e))e:draw()
	end)
	foreach(particles,function(p) p:draw() end)

	--midground
	--(white is the transparent color for map sprites)
	palt(0,false)
	palt(7,true)
	--flags given by bitfield: 2^flag#
	--2^5==bg; 2^6=mid-g; 2^7==fg
	map(0,0,0,0,128,64,1)
	--add padding on edges of map
	map_pad()
	--reset palette
	palt()

	foreach(pickups,function(p)
		if(not off_screen(p))p:draw()
	end)
	foreach(npcs_onscreen,function(e)
		if(e.stoned)stoned_cols(e)
		e:draw()
		pal()
	end)

	p:draw()

	foreach(fgs,function(e)
		if(not off_screen(e))e:draw()
	end)

	--debug hitboxes
	-- foreach(npcs,function(e) 
	-- 		return hitbox_test(e)
	-- 	end)
	--hitbox_test(p)
	-- pset(cam_x,cam_y,8)
	-- invert()
	-- pset(64,64,8)

	--ui overlay
	camera(-128,-128)
	for i=0,keys-1 do
		spr(72,-126+i*10,-12)
	end
end

--inheritance==============================

class = {}
--use o to pass default parameters for a class, or specific parameters for an instance
function class:new(o)
	--override default values
	local o = o or {}
	local params = params or {}
	setmetatable(o, self)
	self.__index = self
	--initalize new object using parent's initiator
	if(self.init)self.init(o)
	--pass a reference to the new object
	return o
end

--static objects===============================

obj=class:new({
	type='obj',
	--x and y will always change, but all objects will always have these (it will never be nil)
	x=0,y=0,
	hbl=0,hbr=8,hbt=0,hbb=8,
	sprt=0,spr_x=2,spr_y=2,
	spell=0
})
function obj:draw()
	spr(self.sprt,self.x,self.y,self.spr_x,self.spr_y,self.dir==-1)
end

--decor=======================================
dec=obj:new({spr_x=1,spr_y=1})
bg=dec:new({type='bg'})
function bg:init() add(bgs,self) end
fg=dec:new({type='fg'})
function fg:init() add(fgs,self) end

torch=fg:new({
	type='torch',
	spr_y=2,sprt=108
})
torches={}
function torch:init()
	add(fgs,self)
	add(torches,self)
end
function torch:update()
	for i=-1,1,.2 do
		if rnd(1)<.35 then
			local ember=particle:new({
				x=self.x+4+2*i^3,
				y=self.y+1,dy=.75*(i^2-1),
				spell=5,
				shift=particle_shift_fire,
				max=particle_max_fire
			})
			add(fgs,ember)
		end
	end
end

pickup=dec:new()
function pickup:init()
	--self.y+=1
	add(pickups,self)
end
function pickup:draw()
	if(off_screen(self))return
	spr(self.sprt,self.x,self.y)
end
door=pickup:new({type='door',sprt=83,hbl=-2,hbr=10,spr_y=2})
key=pickup:new({type='key',sprt=72,hbl=1,hbr=7})
book=pickup:new({type='book',sprt=88})
function book:draw()
	if(off_screen(self))return
	--color swap
	c1,c2=spell_cols(self.spell)
	pal(11,c1)
	pal(3,c2)
	spr(self.sprt,self.x,self.y)
	pal()
end

-- dynamic (moving) objects ==================

dyn=obj:new({
	dx=0,dy=0,dir=1,
	--i want to randomize t, but I can't do it here...
	t=0,st=0
})

--player======================================

--consts:
--jumping apex time in seconds*fps, height in tiles*pixels/tile
apext=.35*30
local apexy=5*8
termvel=6
--walking speed
p_vel=2
--ability to steer while jumping
p_jvel=1
--animation times and stuff
p_step=4
wand_reset=3
spell_reset=10
--calculate gravity from jump height and duration
--!!move to _init?
grav=2*apexy/apext^2
--calculate takeoff velocity
p_jump=grav*apext
--utility variables for player controls
jumped=false
fired=false
--pickup amounts
keys=0
--4 for debugging; makes the books work weird, but they work right if it starts at 0
spells=0
spells_max=4
--object definition:
p=dyn:new({
	type='player',
	hbl=5,hbr=10,hbt=3,hbb=13,
	spell=4
})
function p:new(params)
	for key, value in pairs(params) do
		p[key]=value
	end
end
function p:update()
	local dx,dy,dir,y_coll=self.dx,self.dy,self.dir,self.y_coll
	local sprt,spell,up=self.sprt,self.spell,self.up
	local whipped,bounce=self.whipped,self.bounce

	local b0=btn(0) and not btn(1)
	local b1=btn(1) and not btn(0)
	if(b0)dir=-1
	if(b1)dir=1

	--idle state
	if self.st==0 then
		sprt=0
		--decelerate
		if not whipped then
			if(dx>0)dx-=1
			if(dx<0)dx+=1
		end
		--change state if player starts walking
		if(b0 or b1)change_state(self,1)
	end
	--st=self.st
	--walk state
	if self.st==1 then
		--transition to idle if no lr input
		if(not (b0 or b1))self.st=0
		--acceleration
		if not whipped then
			if(dir==-1 and dx>p_vel*-1)dx-=1
			if(dir==1 and dx<p_vel)dx+=1
		end
		--sprite
		sprt=(time_since(self.t)%(p_step*2)>=p_step)and 2 or 0
	end

	--becoming airborne (jumping or falling)
	if self.st<2 then
		if not y_coll then
			self.st=2
			jumped=true
		--if grounded and not holding x, reset jumped
		elseif not btn(5) then
			jumped=false
		--btn(5) is implicitly true here
		elseif not jumped then
			self.st=2
			dy=-p_jump
			jumped=true
		end
	end

	--airborne state
	if self.st==2 then
		--sprite
		sprt=4
		if(dy>=0)sprt=6
		--lr movement while airborne
		if not whipped then
			if b0 then
				if(dx>-1*p_jvel)dx-=1
			elseif b1 then
				if(dx<p_jvel)dx+=1
			else
				if(dx>0)dx-=1
				if(dx<0)dx+=1
			end
		end
		if(y_coll and dy>=0)self.st=0
	end

	--apply gravity (even if grounded, for collisions)
	dy=min(dy+grav,termvel)
	--collision checks
	self.dx,self.dy=dx,dy
	self.x_coll,self.y_coll=collisions(self)

	--bouncing on a pig
	if bounce then
		self.y=bounce.y+bounce.hbt-self.hbb+bounce.amp
		self.dy=0
		if(bounce.stoned)self.bounce=nil
	end
	
	--down btn: switch spells
	if not btn(3) then
		spell_timer=nil
	elseif spell_timer then
		if(time_since(spell_timer)>spell_reset)spell_timer=nil
	elseif spells>0 then
		spell_timer=frame
		spell=(spell+1)%spells
	end

	--z btn: fire
	if wand_timer then
		if(time_since(wand_timer)>wand_reset)wand_timer=nil
	elseif btn(4) then
		--disable wand if character has no spells yet
		if not (fired or spells==0) then
			wand_timer=frame
			fired=true
			local fx=up and 4 or 8
			local fy=up and 3 or 7
			--not sure why this doesn't need to be relative to the centerpoint (x+8) instead of just x...
			beam:new({x=self.x+fx*dir,y=self.y+fy,dir=dir,spell=spell})
		end
	else
		fired=false
	end--end of wand timer logic

	--looking up
	if btn(2) then
		sprt+=32
		self.up=true
	else
		self.up=false
	end

	self.sprt=sprt
	self.dir=dir
	self.spell=spell
	self.whipped=false

	--pickup collisions
	foreach(pickups, function(pickup)
		if(aoe(pickup,p))pickup_collide(pickup)
	end)
end
function p:draw()
	local sprt,dir,x,y=self.sprt,self.dir,self.x,self.y
	--color-swap
	local c1,c2=spell_cols(self.spell)
	pal(11,c1)
	pal(3,c2)
	--draw main sprite
	spr(sprt,x,y,2,2,dir==-1)
	--draw wand arm
	if wand_timer then
		local fx=dir==1 and 8 or 0
		local fy=(sprt==34 or sprt==36)and 1 or 0
		if self.up then
			sspr(56,58,8,6,x+8-fx,y+fy+2,8,6,dir==-1)
		else
			sspr(56,56,8,2,x+fx,y+8,8,2,dir==-1)
		end
	end
	--end color swap
	pal()
end

--npcs====================================

npc=dyn:new({state=0})
function npc:init()
	--randomize timer so that all enemies don't move in unison
	self.t=frame+rnd(20)
	add(npcs,self)
end

--eagle consts:
eagle_a=.1
eagle_vel=.5
flap_dur=6--frames per animation state
--object definition
eagle=npc:new({
	type='eagle',
	hbl=2,hbr=10,hbt=2,hbb=15,
	stoned=true,sprt=10
})
function eagle:update()
	--advance animation
	new_st=self.st==3 and 1 or self.st+1
	if(time_since(self.t)>flap_dur)change_state(self,new_st)
	if(self.st==0)return
	--no collisions; no gravity
	self.dx=eagle_vel*self.dir
	self.dy-=eagle_a
	self.x+=self.dx
	self.y+=self.dy
	--if the eagle is safely off screen, delete it
	if(off_screen(self))del(npcs,self)
end
function eagle:draw()
	local st=self.st
	if st==1 then
		self.sprt=42
	elseif st==2 then
		self.sprt=44
		spr(12,self.x,self.y-16,2,2,self.dir==-1)
	elseif st==3 then
		self.sprt=14
		spr(46,self.x,self.y+16,2,2,self.dir==-1)
	end
	-- spr(self.sprt,self.x,self.y,self.spr_x,self.spr_y,self.dir==-1)
	obj.draw(self)
end

--frog consts:
frog_idle=90
croak_dur=5
croak_period=15
frog_apexy=apexy-8--1 tile shorter than player's
frog_jx=3*8--how far the frog can jump horizontally if there are no obstructions
frog_jy=sqrt(2*grav*frog_apexy)
--object definition
frog=npc:new({
	type='frog',
	hbl=5,hbr=15,hbt=1,hbb=7,
	jvel=frog_jy
})
function frog:update()
	--jumping sprite; placed here because this is the case in 2 states, whereas idle is only the case in 1
	self.sprt=66
	self.spr_y=2
	--idle
	if self.st==0 then
		--set sprite
		self.sprt=64
		self.spr_y=1
		--increment state timer
		local t=time_since(self.t)
		--apply croaking
		if(t%croak_period<croak_dur)self.sprt=80
		--if set to turn, do so halfway through idle state
		if self.turn and t>frog_idle/2 then
			flippy(self)
			self.turn=false
		end
		--when the idle timer resets, get ready to jump
		if(t>frog_idle)change_state(self,1)
	--jumping
	elseif self.st==1 then
		--create dummy hitbox to look forward and check for obstructions
		local hb=dummy_hitbox(self)
		--prevent frog from jumping into tiny corridors
		hb.y-=frog_apexy
		hb.hbb+=frog_apexy
		hb.dx=frog_jx*self.dir
		hb.dy=0
		--check for collisions, then set target landing for wherever the dummy hitbox winds up
		--if there is either a wall or a cliff at or before the end of the target path, the frog will turn around when it gets to it
		local x_dist=frog_jx
		if collisions(hb) then
			self.turn = true
			x_dist=abs(hb.x-self.x)
		end
		if cliff_check(hb) then 
			self.turn = true
			x_dist=cliff_dist(self,x_dist)
		end
		--character's x velocity = distance to the target landing point
		if(x_dist==nil)x_dist=0
		self.dx=x_dist*self.dir/(2*self.jvel/grav)
		self.dy=-self.jvel
		--once the trajectory is set, change state again
		change_state(self,2)
	end
	--apply gravity and check for collisions
	self.dy+=grav
	local x,y=collisions(self)
	--airborne
	--since state 2 actions depend on collisions, they come last
	if self.st==2 then
		--when the frog lands, go back to idle state
		--!!really this should only happen for downward collisions, but the effect is too quick to really notice
		if y then
			change_state(self,0)
			self.dx=0
		--if the frog hits a wall before landing, get ready to turn around
		elseif x then
			self.turn=true
		end
	end
end
--no frog draw function; uses defualt obj draw function

--guard consts:
guard_chase=2
guard_anim=20
guard_anim_body_down=guard_anim/5
guard_anim_body_up=guard_anim/1.5
guard_anim_head_up=guard_anim/3
--whip consts:
timer=0
whip_dur=12
--distance from shoulder to handle; elbow radius = this/2
swing_radius=8
whip_max_length=32
--object definition:
guard=npc:new({
	type='guard',
	hbl=3,hbr=13,hbt=0,hbb=15,
	step=0
})
function guard:init()
	sb=dummy_hitbox(self)
	sb.hbl,sb.hbr,sb.hbt,sb.hbb=
			-whip_max_length,16+whip_max_length,-8,16
	self.sb=sb
	if(self.dir==-1)flippy(self)
	add(npcs,self)
end
function guard:update()
	local sb,whip,dir=self.sb,self.whip,self.dir
	local x,y=self.x,self.y
	--advance animation
	self.step=time_since(self.t)%guard_anim
	--dummy hitbox to search for player
	if aoe(sb,p) and not whip then
		-- if sgn(p.x-x)!=dir then
		-- 	flippy(self)
		-- 	dir*=-1
		-- end
		whip=spawn('whip',x+8+6*dir,y+6,dir)
	end
	--make self less convoluted; some reused calculations from within whip_update
	if whip then
		--update whip coordinates
		local whip_time=time_since(whip.t)
		if whip_update(whip) then
			whip=nil
			shockwave(sb.x+8+whip_max_length*dir,y)
		end

		--shove player away from guard
		local whip_box=dummy_hitbox(self)
		whip_box.hbt-=8
		local adj=whip_max_length*(whip_time/whip_dur)^2
		if dir==1 then
			whip_box.hbr+=adj
		else
			whip_box.hbl-=adj
		end
		if aoe(whip_box,p) then
			p.dx=4*dir
			p.whipped=true
		end
	end
	self.whip=whip
end
function guard:draw()
local x,y,dir=self.x,self.y,self.dir
	local bounce_body,bounce_head,step=0,0,self.step
	if step<guard_anim_body_down or self.step>guard_anim_body_up then
		bounce_body=1
	end
	if step<guard_anim_head_up then
		bounce_head=1
	end
	--feather,cape wings
	--!!could save some tokens here, at the possible cost of making it even more confusing
	if bounce_body==0 and bounce_head==0 then
		sspr(8,48,3,5,x+6.5+.5*dir,y-7,3,5,dir==-1)
		sspr(12,56,4,6,x+6-6*dir,y+8,4,6,dir==-1)
		sspr(36,56,4,6,x+6+6*dir,y+8,4,6,dir==-1)
	elseif bounce_body==1 and bounce_head==1 then
		sspr(10,53,4,3,x+6-dir,y-7,4,3,dir==-1)
		sspr(8,56,4,6,x+6-6*dir,y+6,4,6,dir==-1)
		sspr(32,56,4,6,x+6+6*dir,y+6,4,6,dir==-1)
	else
		sspr(12,48,4,4,x+6+dir,y-7,4,4,dir==-1)
		sspr(12,56,4,6,x+6-6*dir,y+7,4,6,dir==-1)
		sspr(36,56,4,6,x+6+6*dir,y+7,4,6,dir==-1)
	end
	--render whip
	local whip=self.whip
	if whip then
		--whip
		foreach(whip.points,function(e)
			pset(e.x*self.dir+whip.x,e.y+whip.y,4)
		end)
		--hand
		local x1,y1
		x1=whip.x+(whip.handle.x-2)*dir
		y1=y+whip.handle.y+2
		local x2,y2=x1+2*dir,y1+2
		rectfill(x1,y1,x2,y2,8)
	end
	--back foot
	spr(112,x+4+4*dir,y+12,1,.5,dir==-1)
	--body
	spr(98,x,y-bounce_body,2,2,dir==-1)
	--front foot
	sspr(0,60,8,4,x+4-4*dir,y+12,8,4,dir==-1)
	--head
	spr(96,x+4+3*dir,y-3-bounce_body-bounce_head,1,1,dir==-1)
end

--pig consts:
pig_vel=.5
pig_step=5
--apex height for bouncing off a pig
local apexb=7.5*8
--calculate launch speed from bounce height and gravity
bounce_vel=sqrt(2*grav*apexb)
max_stretch=4--px
stretch_dur=2*30--length of bounce period in frames (seconds * frames/second)
stretch_freq=4--number of vibrations per second
--object definition:
pig = npc:new({
	type='pig',
	hbl=0,hbr=13,hbt=5,hbb=15,
	amp=0--stretch height
})
function pig:update()
	--set x-velocity, unless falling
	if(self.dy<1)self.dx=pig_vel*self.dir
	self.dy+=grav

	--if there is either a wall or a ledge, turn around
	if collisions(self) or cliff_check(self) then
		flippy(self)
		--stop rapid flipping on small platforms
		if(cliff_check(self))flippy(self)
	end

	--set sprite in pixels on the sprite grid
	self.sprt=(time_since(self.t)%(pig_step*2))<pig_step and 32 or 48
	--set bounce amplitude
	local amp
	if self.bounce then
		local period=time_since(self.bounce)
		--decrease amplitude of distortion over time; approximation of exponential decay
		env=max_stretch*(period/stretch_dur-1)^2
		--calculate amplitude
		amp=ceil(env*-sin(period*stretch_freq/30))
		--reset timer at end of bounce period
		if(period>stretch_dur)self.bounce=nil
		--launch player at the end of the first bounce
		if p.bounce==self and period>=15/stretch_freq then
			p.bounce=nil
			p.dy=-bounce_vel
		end
	else
		--no distortion
		amp=0
	end
	self.amp=amp
end
function pig:draw()
	local amp=self.amp
	--center the horizontal bouncing properly
	cx=(self.hbl+self.hbr)/2
	--apply distortion to sprite
	sspr(self.sprt,32,16,16,self.x-amp/2*cx/8,self.y+amp,16+amp,16-amp,self.dir==-1,false)
end

--other dynamic objects==========================

--beam consts
beam_vel=3
bound=ceil(beam_vel/2)
--definition
beam=dyn:new({
	type='beam',
	hbl=-bound,hbr=bound,hbt=-bound,hbb=bound
})
function beam:init()
	if p.up then
		self.dy=-beam_vel
	else
		self.dx=beam_vel*self.dir
	end
	--how to generalize this to all dyn objects?
	if(self.dir==-1)flippy(self)
	add(beams,self)
end
function beam:update()
	self.x+=self.dx
	self.y+=self.dy
	local x,y,spell=self.x,self.y,self.spell
	local l,r,t,b=minify_coords(self)
	--particle system
	for i=l,r do
		for j=t,b do
			if(rnd(1)<.5)then
				e=particle:new({x=i,y=j,spell=spell})
				e.dy=rnd(4)-1
			end
		end
	end
	--npc collisions
	foreach(npcs_onscreen,function(npc)
		if aoe(self,npc) then 
			beam_collide(npc,spell)
			del(beams,self)
			return
		end
	end)
	--wall collisions
	local hitx,hity=collisions(self)
	if self.queue_delete then
		del(beams,self)
		self.queue_delete=nil
	elseif off_screen(self) or hitx or hity then
		self.queue_delete=true
	end
end

--consts for how long particles last, in seconds
particle_shift_beam=3
particle_shift_fire=4
particle_max_beam=8
particle_max_fire=12
--definition
particle=dyn:new({
	type='particle',
	--set these within the beam and torch update functions to fix weird torch glitches
	shift=particle_shift_beam,
	max=particle_max_beam
})
function particle:init()
	self.t=frame
	add(particles,self)
end
function particle:update()
	self.y+=self.dy
	self.x+=self.dx
	if time_since(self.t)>rnd(self.max) then
		del(particles,self)
		del(fgs,self)
	end
end
function particle:draw()
	local c1,c2=spell_cols(self.spell)
	local c=time_since(self.t)>rnd(self.shift) and c2 or c1
	pset(self.x,self.y,c)
end

function spawn(type,x,y,dir,spell)
	--true for all or most characters
	local e = {
		type=type,
		x=x,y=y,dx=0,dy=0,dir=1,
		spell=spell,
		t=frame+rnd(20),
		--width and height of tiles on sprite map
		spr_x=2,spr_y=2
	}
	if type=='whip' then
		e.t=frame
		--not used, but necessary to avoid an error; maybe there's a better way to spawn the whip?
		e.hbl,e.hbr=0,0
	end
	return e
end--end of spawning function

--set the new state and the time of the change
function change_state(this,new_st)
	this.st=new_st
	this.t=frame
end

--preserve collision box location relative to tile
--'flip' is a system function
function flippy(this)
	--f==width of character sprite in pixels
	-- -1 because the first pixel is labeled 0
	local f=this.spr_x*8-1
	local temp=f-this.hbl
	this.hbl=f-this.hbr
	this.hbr=temp
	this.dir*=-1
end

function time_since(when)
	return(frame-when)%1000
end

--collisions=========================================

--all-purpose collision check; returns true or false for collisions in each axis
function collisions(this)
	local l,r,t,b=minify_coords(this)
	local d,dt,lead,coll={},{},{},{}
	d.x,d.y=this.dx,this.dy
	--distance traveled so far; used within the loop
	dt.x,dt.y=0,0
	--find the leading edge for each axis; only ones that need to be checked
	lead.x=this.dx<0 and l or r
	lead.y=this.dy<0 and t or b
	--check for collisions for at least each pixel between current and proposed location
	local step=max(abs(d.x),abs(d.y))
	--check for collisions in both directions, one step at a time
	for i=0,step do
		--define function to repeat for each axis
		--defined here so it has access to other local variables
		function axis_check(axis)
			--define opposite axis
			local other,j1,j2='y',t,b-1
			if(axis=='y')other,j1,j2='x',l,r-1
			--create container for coordinates to check
			--check one step ahead of current position; leading edge + target position + pixels per step for this axis (==1 for longer axis)
			local check={}
			check[axis]=lead[axis]+dt[axis]+d[axis]/step
			--check each pixel along the leading edge
			for j=j1,j2 do
				--calculate the missing coordinate for the pixel being checked
				check[other]=j+dt[other]

				--check for walls
				if fget(mget(check.x/8,check.y/8),0) then
					coll[axis]=true
					d[axis]=dt[axis]
					--if there's a wall, don't bother checking for npcs
					break
				end
				--check for npcs; npcs to not interact with each other
				if this.type=='player' then
					foreach(npcs_onscreen, function(npc)
						-- if npc.type!='torch' then
							if aoe_point(check.x,check.y,npc) then
								--if the npc is stoned, treat it like a wall
								if npc.stoned then
									coll[axis]=true
									d[axis]=dt[axis]
								else
									npc_collide(npc,axis)
								end
							end
						-- end
					end)
				end
			end
			--increment the distance traveled so far
			if(not coll[axis])dt[axis]+=d[axis]/step
		end--end of axis_check function definition
		--repeat the above function once for each axis, unless there's already been a collision
		if(not coll.x)axis_check('x')
		if(not coll.y)axis_check('y')
	end--end of actual collision check loop
	--apply results
	--!!move this elsewhere?
	--for some reason, d[axis]!=dt[axis] here...
	this.x+=d.x
	if(coll.x)this.dx=0
	this.y+=d.y
	if(coll.y)this.dy=0
	--return true or false for each axis
	return coll.x,coll.y
end--end of collision check function

--simple aoe check
function aoe(obj1,obj2)
	local l1,r1,t1,b1=minify_coords(obj1)
	local l2,r2,t2,b2=minify_coords(obj2)
	return l1 < r2 and r1 > l2 and t1 < b2 and b1 > t2
end

--aoe check for single pixel
function aoe_point(x,y,obj)
	local l,r,t,b=minify_coords(obj)
	return x>=l and x<=r and y>=t and y<=b
end

--beam-npc collisions
function beam_collide(npc,spell)
	--stoning spell (gray)
	if spell==0 then
		npc.stoned=true
	--de-stoning spell (tan)
	elseif spell==1 then
		npc.stoned=false
	--transformation spells
	else
		replace={
			e=npc,
			s=spell
		}
	end
end

function replace_me()
	local npc=replace.e
	local type=replace.s==2 and pig or frog
	--spawn the new npc
	local l,r,t,b=minify_coords(npc)
	local newc=type:new({
		--new character is aligned with old: same bottom, same middle on x axis
		--should the first l be npc.x instead?
		x=l+(r-l)/2-(type.hbl+type.hbr)/2,
		y=b-type.hbb,
		dir=npc.dir
	})
	-- delete the old npc
	del(npcs,npc)
	replace=nil
end

--check if the leading edge is hanging off a cliff
function cliff_check(this)
	local l,r,t,b=minify_coords(this)
	local lead=this.dir<0 and l or r
	return not fget(mget(lead/8,(b+1)/8),0)
end

--find out how far away the cliff is
function cliff_dist(this,dist)
	local hb=dummy_hitbox(this)
	for i=1,dist do
		hb.x+=this.dir
		if(cliff_check(hb))return i
	end
end

--this basically just creates a disposable clone of the character
function dummy_hitbox(this)
	return {
		x=this.x,y=this.y,
		dx=this.dx,dy=this.dy,
		hbl=this.hbl,hbr=this.hbr,
		hbt=this.hbt,hbb=this.hbb,
		dir=this.dir
	}
end

--player-npc collisions
function npc_collide(npc,axis)
	local type=npc.type
	if type=='pig' and
		--conditions required for bouncing
		not p.bounce and
		axis=='y' and
		p.dy>0 and
		--avoid bouncing in very tight corridors
		p.y+p.hbb<npc.y+8 then
			p.bounce=npc
			p.y=npc.y+npc.hbt-p.hbb
			p.dy=0
			p.coll=true
			p.jumped=true
			npc.bounce=frame
	elseif type=='guard' then
		p.dx=5*npc.dir
		p.x=npc.dir==1 and 
			npc.x+npc.hbr-p.hbl or
			npc.x+npc.hbl-p.hbr
	end
end

function pickup_collide(item)
	if item.type=='book' then
		p.spell=item.spell
		spells=min(spells+1,spells_max)
	elseif item.type=='door' then
		--must have key to unlock
		if(keys==0)return
		--remove the door
		local x,y=item.x/8,item.y/8
		for i=0,1 do
			mset(x,y-i,bg_tile(x,y))
		end
		--decrement keys
		keys-=1
	elseif item.type=='key' then
		keys+=1
	end
	del(pickups,item)
end

--convert hitbox corners to single tokens
function minify_coords(this)
	return this.x+this.hbl,
		this.x+this.hbr,
		this.y+this.hbt,
		this.y+this.hbb
end

function off_screen(this)
	local l,r,t,b=minify_coords(this)
	--includes buffer zone for when the sprite is larger than the character (really only true for guards, and kind of torches)
	return cam_x-16>r or
		cam_x+128+16<l or
		cam_y-16>b or
		cam_y+128+16<t
end

--graphics============================

function shockwave(x,y)
	for i=0,20 do
		local e = particle:new({x=x,y=y,spell=5})
		-- spawn('particle',x,y,1,5)
		local theta = rnd(1)
		e.dx=cos(theta)*rnd(2)
		e.dy=sin(theta)*rnd(2)
	end	
end

--right now, camera just tracks player
function camera_update()
	cam_x=p.x-56
	cam_y=p.y-56
	camera(cam_x,cam_y)
end

--return the palette swaps for the player & particles
function spell_cols(spell)
	--gray
	if spell==0 then
		return 6,5
	--tan
	elseif spell==1 then
		return 15,4
	--pink
	elseif spell==2 then
		return 14,2
	--green
	elseif spell==3 then
		return 11,3
	--blue (only true in the beginning)
	elseif spell==4 then
		return 12,13
	--orange and red; whip shockwave only
	else
		return 9,8
	end
end

function stoned_cols(this)
	local type=this.type
	local c1,c2,c3
	if type=='guard' then
		c1,c2,c3=2,4,8--purple,brown,red
		pal(9,7)--orange->white
	elseif type=='pig' then
		c1,c2,c3=2,14,15--purple,pink,peach
	elseif type=='frog' then
		c1,c2,c3=10,3,11--yellow,green,lime
	elseif type=='eagle' then
		c1,c2,c3=2,4,9--purple,brown,orange
		pal(10,6)--yellow->white
		pal(3,8)--green->red
	end
	pal(c1,5)--dark gray
	pal(c2,13)--medium (slate)
	pal(c3,6)--light gray
end

-- whip rendering======================

--this logic is hella confusing... sorry
function whip_update(this)
	--this avoids a weird error, not sure if it's really the behavior i want or why it's even necessary
	local timer=time_since(this.t)
	if timer>whip_dur then
		--instantiate shockwave particle generator
		this.t=frame
		return true
	end

    --normalize as a fraction of 1
    local timer_rel=timer/(whip_dur)
    --exponential growth for some things
    local timer_exp=timer_rel^2

	local points = {}
    -- post-loop (end of whip)
    loop_segment(
        --scale (maximum r)
        --bigger (straighter) as time goes on
        whip_max_length*(timer_rel+.25)*.75,
        .5,
        --start (position of end of whip relative to the post-loop portion)
        --smaller (straighter) as time goes on
		--adding .1 to the denominator avoids a weird artifact
        .75+timer_rel/4.1,
        1,
		points,
        false
    )
    -- main loop
    local main_timer = 2*timer_exp+.5
    loop_segment(
        --exponential decay from whip_max_length to 0 over the given duration
        --i don't know how i ended up with such a weird equation, but it works...
        whip_max_length*(whip_max_length)^(-timer_rel),
        0,
        0,
        --increases linearly until reaching 1
        min(main_timer,1),
		points,
        true
    )
    --pre-loop (handle side)
    --this part of the whip only appears once the main loop is closed
    if main_timer>=1 then
        loop_segment(
            whip_max_length*timer_rel,
            .25,
            0,
            max(0,timer_exp/2-.125),
			points,
            true
        )
    end

    --elbow rotates around pivot (shoulder), forming a quarter circle; handle rotates around elbow, forming a half heart-shape
    local elbow, handle={}, {}
    handle.x,handle.y=rotate_coords(swing_radius/2,0,.5-timer_rel/2-timer_rel/4)
    handle.x+=swing_radius/2
    elbow.x,elbow.y=rotate_coords(swing_radius/2,0,-timer_rel/4)
    handle.x,handle.y=rotate_coords(handle.x,handle.y,-timer_rel/4)

	--pretty much arbitrary, but oh well
	local master_angle=.5+.25*timer_rel
	--apply transformations
	foreach(points,function(e)
		e.x-=prev_x
		e.y-=prev_y
		e.x,e.y=rotate_coords(e.x,e.y,master_angle)
		e.x,e.y=handle.x-e.x,handle.y-e.y
	end)

	this.points=points
	this.handle=handle
end

function polar_to_cartesian(r, angle)
    local x=r*cos(angle)
    local y=r*sin(angle)
    return x,y
end

--apply rotation matrix
function rotate_coords(x,y,angle)
    return x*cos(angle)-y*sin(angle),x*sin(angle)+y*cos(angle)
end

function loop_segment(scale,rot,start,finish,table,flip)
    --start and finish: fraction from 0 to 1 (could extend beyond as well) indicating the fraction of one loop to draw
    for t=.25*start-.125,
        .25*finish-.125,
        .001 do
        --scale==maximum r
        local r=scale*sqrt(cos(2*t))
        local x,y=polar_to_cartesian(r,t)
        --draws the loop clockwise instead of counterclockwise (default)
        if(flip)y*=-1
        --rot: number of revolutions counterclockwise; 0=points right
        x,y=rotate_coords(x,y,rot)
        if(flr(x)!=flr(prev_x) or flr(y)!=flr(prev_y))add(table,{x=x,y=y})
        prev_x,prev_y=x,y
    end
end

--map====================================

function map_init()
	for i=0,128 do
		for j=0,128 do
			local tile = mget(i,j)
			local newt,newc,type
			--this needs to be done before the bricks
			if tile==108 then
				torch:new({x=i*8,y=j*8})
				newt=78
			-- bricks
			elseif fget(tile,1)then
				--check the neighboring tiles (i==[]) refers to the map bounds)
				-- t=fget(mget(i,j-1),1)
				b=j==63 or fget(mget(i,j+1),1)
				l=i==0 or fget(mget(i-1,j),1)
				r=i==127 or fget(mget(i+1,j),1)
				--figure out which tile to replace the current one with; nested ternaries
					newt=b and (r and (l and 78 or 77) or
						(l and 79 or 76)) or
						(r and (l and 94 or 93) or
						(l and 95 or 92))
			-- pillars
			elseif tile==126 and not fget(mget(i,j-1),2) then
				bg:new({x=(i-1)*8,y=j*8,sprt=109})
				bg:new({x=(i+1)*8,y=j*8,sprt=111})
				newt=110
			elseif tile==118 then
				type=p
			elseif tile==102 then
				type=eagle
			elseif tile==100 then
				type=frog
			elseif tile==117 then
				type=guard
			elseif tile==101 then
				type=pig
			
			--books
			elseif tile==88 then
				newc=book:new({x=i*8,y=j*8})
				if mget(i+1,j)==102 then--eagle/un-stone
					newc.spell=1
				elseif mget(i+1,j)==101 then--pig
					newc.spell=2
				elseif mget(i+1,j)==100 then--frog
					newc.spell=3
				else
					newc.spell=0--stone
				end
				mset(i+1,j,bg_tile(i,j))
				newt=bg_tile(i,j)
			--doors
			elseif tile==120 then
				door:new({x=i*8,y=j*8})
				mset(i,j-1,104)
			--keys
			elseif tile==72 then
				key:new({x=i*8,y=j*8})
				newt=bg_tile(i,j)
			--fg elements
			elseif tile==107 or
				tile==123 or
				tile==125 or
				tile==127 then
				fg:new({x=i*8,y=j*8,sprt=tile})
				newt=bg_tile(i,j)
			end
			--spawn queued characters
			if type then
				newc=type:new({
					x=(i+.5)*8-(type.hbl+type.hbr)/2,
					y=j*8+7-type.hbb
				})
				local dir=1
				if mget(i,j-1)==103 then
					flippy(newc)
					mset(i,j-1,bg_tile(i,j-1))
				end
				newt=bg_tile(i,j)
			end
			--apply changes to tile
			if(newt)mset(i,j,newt)
		end
	end
end

--add padded tiles around the edge of the map
function map_pad()
	for i=0,16 do
		local not_x=false
		for j=0,16 do
			local not_y=false
			if cam_y<0 then	
				spr(78,flr((cam_x)/8+i)*8,(j+1)*-8)
			elseif cam_y>64*8-128 then
				spr(78,flr((cam_x)/8+i)*8,(j+64)*8)
			--no vertical padding needed
			else not_y=true
			end
			if cam_x<0 then
				spr(78,(i+1)*-8,flr((cam_y)/8+j)*8)
			elseif cam_x>128*8-128 then
				spr(78,(i+128)*8,flr((cam_y)/8+j)*8)
			--no padding needed on either axis
			elseif not_y then
				not_x=true
				--break loop early so we don't check this 16 times
				break
			end
		end
		--break loop early so we don't check this 16 times
		if(not_x)break
	end
end

function bg_tile(i,j)
	return (mget(i-1,j)==75 or mget(i+1,j)==75) and 75 or 83
end

--============================================

function debug()
	stats()
end

function stats()
	printh('memory:'..stat(0))
	printh('cpu:'..stat(1))
	printh('fr:'..stat(7))
	printh('tfr:'..stat(8))
end

--for debugging purposes only
--!!replace the magic numbers with sprite boundaries?
-- function hitbox_test(this)
-- 	x,y=this.x,this.y
-- 	l,r,t,b=minify_coords(this)
-- 	pset(this.x,this.y,7)
-- 	pset(l,t,7)
-- 	pset(r,t,7)
-- 	pset(l,b,7)
-- 	pset(r,b,7)
-- 	pset(x,y+15,7)
-- 	pset(x+15,y,7)
-- 	pset(x+15,y+15,7)
-- end

-- function invert()
-- 	for i=cam_x,cam_x+64 do
-- 		for j=cam_y,cam_y+64 do
-- 			pset(i,j,15-pget(i,j))
-- 		end
-- 	end
-- end

__gfx__
000000bb000000000000000000000000000000000000000000000000000000000000000000000000000000499900000000000000000000000000000000000000
0000000bb000000000000000bb000000000000bb0000000000000000b000000000000000b0000000000049999900000000000000000000000000000000000000
0000000bbb00b0000000000bb00000000000000bb00000000000000bb00000000000000bb0000000000499999099990000000000000000000000000000999900
00000bbbbbbbb0000000000bbb00b0000000000bbb00b0000000000bbb00b0000000000bbb00b000004999994999944000000000000000000000000009999440
000000b3bbbb000000000bbbbbbbb00000000bbbbbbbb00000000bbbbbbbb00000000bbbbbbbb00000499999499394a4000090000000000000000000499394a4
0000000933300000000000b3bbbb0000000000b3bbbb0000000000b3bbbb0000000000b3bbbb00000449999944994aa400099000000000000000044944994aa4
0000000b99000000000000093330000000000009333000000000000933300000000000093330000004999999494444a4009990000000000400004444994444a4
000000bbbb0000000000000b990000000000000b9900000000000bb9999b000000000bb999900000044999942494204000999000000000440044994944942040
000000bbbb000000000000bbbb0000000000000bbb000000000000bb99b00000000000bb99bb8800444994942429200009999000000004440494499994292000
00000bbbbbb000000000bbbbbb000000000000bbbb0000000000b0bbbbb000000000b0bbbbb00000449444942242200099999400000444424949999994422000
0000bbbbbbb000000000bbbbbbbb0000000000bbbb0000000000bbbbbb0000000000bbbbbb000000244449422424200099999400004444422444999994242000
00000bbbbbbb000000000bbbbbb0000000000bbbbb0000000000bbbbbbbb00000000bbbbbbbb0000244944422242000094999940044444202249999994220000
000000bbbb000000000033bbbb3300000000bbbbbb00000000000bbbbbb0000000000bbbbbb00000224444222422000044999944044444202229999994420000
0000000330000000000003300330000000000bbbb0000000000033bbbb330000000033bbbb330000024422222220000044494994044442200249999999420000
00000000000000000000000000000000000003bbb0000000000003300330000000000330033000002a2200222200000004494494424442002a49499999422000
0000000000000000000000000000000000000033000000000000000000000000000000000000000002aa2002aa200000044444494244220002a4999999942000
00000000bbb000000000000000000000000000000000000000000000000000000000000000000000000004999000000004444444422222000004999999942000
0000bbbbbbbb000000000b00bbb0000000000000bbb0000000000b00bbb0000000000b00bbb00000000499999002200000444444422220000000499999994000
00000bb333bb000000000bbbbbbb00000000bbbbbbbb000000000bbbbbbb000000000bbbbbbb0000004999990099990000044444449999000000494999994200
0000bb399933000000000bb333bb000000000bb333bb000000000bb333bb000000000b8333bb0000044999990999944000049444499994400000044999994200
00003b39993000000000bb39993300000000bb39993300000000bb39993300000000bb899933000004999999499394a400099494499394a40000040499999220
000003339300000000003b399930000000003b399930000000003b399930000000003b89993000004499999944994aa40000994444994aa40000000049999420
0000003bb0000000000003339300000000000333930000000000003393000000000000b39300000049999999494444a400009949494444a40000000044999922
000000bbbb0000000000003bbb0000000000003bbb00000000000bbb3bbb0000000000bb3bbb0000499999999494204000999999449420400000000004449422
000000bbbb000000000000bbbb0000000000000bbb000000000000bbbbb00000000000bbbbb00000449999999429200004999944942920000000000000444440
00000bbbbbb000000000bbbbbb000000000000bbbb0000000000b0bbbbb000000000b0bbbbb00000444999499442200049494994244220000000000000004040
0000bbbbbbb000000000bbbbbbbb0000000000bbbb0000000000bbbbbb0000000000bbbbbb000000249499449944200024449444442420000000000000000000
00000bbbbbbb000000000bbbbbb0000000000bbbbb0000000000bbbbbbbb00000000bbbbbbbb0000224944949942200022424224224200000000000000000000
000000bbbb000000000033bbbb3300000000bbbbbb00000000000bbbbbb0000000000bbbbbb00000222424494494220022242222242200000000000000000000
0000000330000000000003300330000000000bbbb0000000000033bbbb330000000033bbbb330000022222444444400002222222222000000000000000000000
00000000000000000000000000000000000003bbb0000000000003300330000000000330033000002a220024420000002a220022220000000000000000000000
0000000000000000000000000000000000000033000000000000000000000000000000000000000002aa2002aa20000002aa2002aa2000000000000000000000
00000000000033300000000000003330000000000000000000000000000000000044490000000000000000001101101175d555555555555555d5555555d55555
0000000003333a330000000003333a3300000000000000000000000000000000049004900000000000000000000001005dddddd55dddd5dddd5dd5dddd5dd5d5
000000033333333b000000003333333b00000000000000000000000000000000049004900000000000000000101111015ddd5dd55ddddddddd5ddddddd5dddd5
0000b30333333bb00000000333333bb000000000000000000000000000000000004449000000000000000000101110115dddd5d55d5dddd5dd5dddd5dd5dddd5
0000b3333333bb00000000033333bb0000000000000000000000000eee000e00000490000000000000000000101111115555d55775d5555555d5555555d55555
0000bb333bb33000000000333bb33000e000000eee000ee0ee0000eeee00ee000004449000000000000000000000010075dd5dd575dddd5dddd5dd5dddd5dd57
0000bb33bb0b300000000333bb0b3000eeffffeeefffee000effffeeeffff000000490000000000000000000011110115ddd5dd5755ddd5ddd5ddd5ddd5ddd57
00000bb33000b3000003333bb000b3000fffffeeefffff000fffffeeff2ffeee000444900000000000000000111110115ddd5dd575dddd5ddddddd5ddddddd57
00000000000033300033bbb000000000fffffffeef2ffeeefffffffefffffeee3222222000000000000000000101101055555d555555555555d5555555d55555
0000000003333a33003b330000000000ffffffffeffffeeefffffffffffffff033bbbbbb0000000000000000010111015dddd5d55dddd5dddd5dd5dddd5dd5d5
000000033333333b03b0300000000000fffffffffffffff0fffffffffffffff033b33b3b0000000000000000010010105dddddd55ddddddddd5ddddddd5dddd5
0000b30333333bbb3b03b00000000000ffffffffffffff00ffffffffffffff0033bbbbbb0000000000000000010110105d5dd5575d5dddd5dd5dddd5dd5dddd5
0000b3333333bbb03b03b00000000000fffffffffffff000fffffffffffff00033b3b33b00000000000000000101101075d55dd575d5555555d5555555d55555
0000bb333bb33b0033b03300000000000fffffffffff00000fffffffffff000033bbbbbb0000000000000000101110105ddd5dd575dddd5ddddddd5ddddddd57
0000bb33bbbb3000303000000000000000ff220ff022000000ff2200ff22000033b33b3b0000000000000000010110015ddd5dd575dddd5ddddddd5dddd5dd57
00000bb33000b3000000000000000000000e200e0002000000e002000e20000003bbbbbb00000000000000000101101055557555755555755555557555575557
0000000000900999000000000000000000000000e000000000005550000000005555555d11001100000110110520025000099000000000555557755555000000
0088880009909990000000000000000000003333effeefe000055dd500440000ddddddd60001005cc500010002000250009aa900000005ddddd55ddddd500000
08888880990099000000000444440000003333a3fffefff00055d8dd04990000d06060d61010555cc555010102500500024aa420000005d555dddd555d500000
088228809900090000000449444444000333333bffff2fee05dd5d5d49994444d06060d610055c4cc4c550110050050002499420000005d5d555555d5d500000
088222200900000000004499944444400333333bffffffff05dd5d5099999999d06060d610554c4cc4c455000020050002444420000005ddd5dddd5ddd500000
08882220009990000004444944444499b33333b0fffffff05dddd50009990000d06060d6004b4c4cc4b4c4010050052002499420000000555555555555000000
00088880000999000004449299944920bb33b3000fffff00555d550000990000d06060d6044c4b4bc4c4b4400250005002244220000000005dddddd500000000
000088000000090000449992229992000bbb033000e00e000dd0dd0000000000ddddddd6044b4b4cb4b4b4400250025005244250000000007555555700000000
000000000049004900009822222220009000900000088800003b000000bb8800dddd60d6044b444bb444b4400520025000222200000000005d5dd5d500000000
000088000099009900008882229920009000490900482200000bb00000b00000ddddddd6044d454bb45444400200022500522500002400005d5ddd5700022420
0088888000949094000448822222200049004494094888400033330000000080dddd60d60444454bb45444400250005000055000022220005d55d5d502422224
0088888009944994000442222299200044994444449444900009900000000080ddddddd60444444bb44444400050000000000000424220005d5dd5d542224522
0000000094444444000442222222200044444440449999940003b00000000080ddddddd604444446644444400020000000000000222502405d5dd5d522455500
0008800094440444000442222992200044404400492222290003bb80000000b0ddddddd6044444466444444000520000000000000055222275ddd5d525005200
0008888000000000000442222222000000000000492292290033b000000000bbddddddd60444ddd66ddd44400252000000000000000255545d5dd55700052000
000088800000000000000000000000000000000008822884003bb300000000bbddddddd60ddd66666666ddd00220000000000000000250005d5dd5d500552000
e43535353535e7353535353535e73535353535353535353535e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4b4b4b4b4b4b466b4
b535663535353535353535e4e4e4b4b4b4b4b4b4b4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4
e43535353535e7353535353535e4e4e4e4e4e4e43535353535e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4b4b4b4b4b4b4e7b4
b535e73535353535353535e4c6e4b4b4b4b4b4b4b4e4c6e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4
e43535e4e435e7353535353535e4e4e4e4e4e4e43535353535e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4b446b4b446b4e7b4
b535e73535353535353535e4e4e4b4b456b4b456b4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4
e43535353535e7353535353535b5b4b4b4b4b53535353535353535b5b4b4b4b4b4b4b4b4b4b4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4
e4e4e4e4e4353535353535e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4
e43535353535e7353535353535b5b4b4b4b4b53535353535358735b587b4b487b4b4b4b4b4b4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4
e4e4e4e4e4353535353535e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4
e43535353535e7353535353535b5b4b4b4b4b5463535354635e4e4e4e4e4e4e4b4b4b4b4b4b4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4
e4e4e4e4e4353535353535e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4
e43535353535e4e4e4e4e4e4e4e4b4b4b4b4e4e4e4e4e4e4e4e4e4e4e4e4c6e4b4b4b4b4b4b4e4c6e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4
e4e4e4e4e4353535353535e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4
e4353535353535e4e4e4e4e4e4e4e4b4b4b4b4e4e4e4e4e4e4e4e4e4e4e4e4e4b4b4b4b4b4b4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4b4b4b4b4b4b535353535e4
e4e4e4e4e4353535563535e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4
e435463535353535e4e4e4e4c6e4e4e4b4b4b4b4e4e4e4e4e4e4e4e4e4e4e4e4b4b4b4b4b4b4e4e4e4e4e4e4e4e4e4e4e4e4e4c6e4b4b4b4b4b4b535353535e4
e4e4e4e4e43535e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4
e4e4e4e43535353535e4e4e4e4e4e4e4e4b4b4b4b4b4b4b4b4b4b4b4b6e4e4e4b4b4b4b4b4b4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4b484b4b457b535353535e4
e4353535353535e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4
e4353535353535353535e4e4e4e4e4e4e4e4b4b4b4b4b4b476b4b487b6e4c6e4b4b4b4b4b4b4e4c6e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e43535e4
e4353535353535b5b4b4b7b6b4b7b4b6b6b6b4b6b7b6b6e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4
e4353535353535353535b5e4e4e4e4c6e4e4e4b4b4b4b4b457b4b4e4b6e4e4e4b457b4b48556e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e43535353535
3535353535d735b587f7b4b687b4b4b7b6b6b4b7b4b6b7e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4
e435353535e4e4353535b5b4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4b6e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e43535353535
353535e4e4e4e4e4e4e4e4e4e4e4b4b4b6b6b4b4b4b6b4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4
e4353535353535353535b5b4b4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4b6e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e43535353535
353535e4e4e4e4e4e4e4e4e4c6e4b4b4b7b6b4b4b4b6b4e4c6e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4
e4353535353535353535b5b4b4b4e4e4e4e4c6e4e4e4e4e4e4e4e4e4b6e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e43535353535
353535e4e4e4e4e4e4e4e4e4e4e4b4b4b4b7b4b4b4b7b4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4
e43535e4353535353535b5b4b4b4b4e4e4e4e4e4e4e4e4e4e4e4e4e4b7b4b4b4b4b7b4b7b6e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e43535353535
353535e4e4e4e4e4e4e4e4e4e4e4b4b4b4b4b4b4b4b4b4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4
e4353535353535356635b5b4b466b4b6e4e4e4e4e4e4e4e4e4e4e4e4b484b4b4f7b4b4b4b6e4c6e4e4e4e4c6e4e4e4c6e4e4e4e4c6e4e4e4e4c6e43535353535
353535e4e4e4e4e4e4e4e4e4e4e4b4b4b4b48546b4b4b4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4
e435353535353535e735b5b4b4e7b4b6b6e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4b4b4b4b7e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e43535563535
d73556e4e4e4e4e4e4e4e4e4e4e4b4b4b4b4e7b4b4b4b4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4
e435353535353535e735b5b4b4e7b4b7b7b6b7b6b6b6b7b6b4e4e4e4e4e4e4e4b6b4b4b4b4b4b4b4b5353535e4e4e43535353535353535353535353535e4e4e4
e4e4e4e4e4e4e4e4e4e4e4e4c6e4b4b4b4b4e7b4b4b4b4e4c6e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4
e43546d7f74635d7e735b5d7b4e785b4f7b6d7b6b7b6b4b6b4b4e4e4e4e4e4b7b7b4b4b4b4b476b4b53535353535353535353535357635353535353535e4e4e4
e4e4e4e4e4e4e4e4e4e4e4e4e4e4b456b4f7e7b456b4d7e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4
e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4b6b4b7b4b4b7e4e4e4b6b4b7b4b4b4b4b457b4b53535353587353535353535355735353535353535e4e4e4
e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4
e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4b4b6b4b4b4b4b4e4e4e4b6b4b4b4b4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4
e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4
e4b6b4b7b4b7b4b7b6b7b6b6b6b6b6e4e4e4e4e4b4b7b4b4f7b4b4e4e4e4b6b4b4b4b4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4
e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4
e4b6b4b4f7b4d7b4b7b4b7b6b7b6b6e4e4e4e4e4b4b4b4e4e4b4b4e4e4e4b7b4b4b4b4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4
e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4
e4b6b4e4e4e4e4e4e4e4e4e4e4e4b6e4e4e4e4e4b4b4b4b6b7b4b4e4e4e4b4b4b4d7b4e4e4e4e4e4b7b4b4b7b6b6b7b6b6b6b4b7b6b6e4e4e4e4e4e4e4e4e4e4
e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4
e4b6b4e4e4e4e4e4e4e4e4e4e4e4b6e4e4e4e4e4d7b4b4b7b4b4b4e4e4e4b4b4b4e4e4e4e4e4e4b6b4b4b4b4b7b6b4b7b7b6b4b4b7b6e4e4e4e4e4e4e4e4e4e4
e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4
e4b7b4e4e4e4e4e4e4e4e4e4e4e4b6e4e4e4e4e4e4e4b4b4b4b4b4e4e4e4b4b4b4b4e4e4e4e4b4b7b4f7e4e4e4e4e4e4e4e4e4e4b4b6e4e4e4e4e4e4e4e4e4e4
e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4
e4b4b4b6b7b6b4b7e4e4e4e4e4e4b6b4b7b4b6b6b4b4b4b4b4b4b4e4c6e4b4b4b4b4b4e4c6e4b4b4b4e4e4e4e4e4c6e4e4e4e4e4b4b6e4e4e4e4e4e4e4e4e4e4
e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4
e4b4b4b6b4b684b4e4e4e4e4e4e4b6b4b4b4b6b7b4b4b4b4b4b4b4e4e4e4b4b4b4b4b4e4e4e4b4b4e4e4e4e4e4e4e4e4e4e4e4e4b4b7e4e4e4e4e4e4e4e4e4e4
e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4
e4b4b4b6b4b7e7b4e4e4e4e4c6e4b7b4b4b4b6b4b4b4b4f7b4b4b4b4b4b4b4b476b4b4b4b4b4b4b4b4b476b4b6b6b4b7b4b4b7b6b4b4e4e4e4e4e4e4e4e4e4e4
e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4
e4b446b6b446e7b4e4e4e4e4e4e484b457b4b6b4b4b4b4e4b467b4b487b4b4b457b4f7d787b4b4d7b4b466b4b6b7b4b48566b4b7b4b4e4e4e4e4e4e4e4e4e4e4
e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4
e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4
e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4
__label__
111111111111111115ddddddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5dddd511111111111111111111111111111111111111115ddddddddd5dddd
111111111111111115d5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd511111111111111111111111111111111111111115d5dddd5dd5dddd
1111111111111111115d5555555d5555555d5555555d5555555d5555555d5555555d55555111111111111111111111111111111111111111115d5555555d5555
1111111111111111115dddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd51111111111111111111111111111111111111111115dddd5ddddddd5
1111111111111111115dddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5dddd5dd51111111111111111111111111111111111111111115dddd5ddddddd5
11111111111111111155555155555551555555515555555155555551555555515555155511111111111111111111111111111111111111111155555155555551
111111111111111110101101011011011110110111101101111011011010110101111111111111111111111111111111111111111111111115555555d1111111
11111111111111111010111010000010000000100000001000000010001011101111111111111111111111111111111111111111111111111ddddddd61111111
11111111111111111010010101011110110111101101111011011110101001010111111111111111111111111111111111111111111111111d06060d61111111
11111111111111111010110101011101110111011101110111011101101011010111111111111111111111111111111111111111111111111d06060d61111111
11111111111111111010110101011111110111111101111111011111101011010111111111111111111111111111111111111111111111111d06060d61111111
11111111111111111101110100000010000000100000001000000010010111010111111111111111111111111111111111111111111111111d06060d61111111
11111111111111111010110010111101101111011011110110111101101011001111111111111111111111111111111111111111111111111d06060d61111111
11111111111111111010110101111101111111011111110111111101101011010111111111111111111111111111111111111111111111111ddddddd61111111
11111111111111111010110101101101111011011110110111101101101011010111111111111111111111111111111111111111111111111dddd60d61111111
11111111111111111010111010000010000000100000001000000010001011101111111111111111111111111111111111111111111111111ddddddd61111111
11111111111111111010010101011110110111101101111011011110101001010111111111111111111111111111111111111111111111111dddd60d61111111
11111111111111111010110101011101110111011101110111011101101011010111111111111111111111111111111111111111111111111ddddddd61111111
11111111111111111010110101011111110111111101111111011111101011010111111111111111111111111111111111111111111111111ddddddd61111111
11111111111111111101110100000010000000100000001000000010010111010111111111111111111111111111111111111111111111111ddddddd61111111
11111111111111111010110010111101101111011011110110111101101011001111111111111111111111111111111111111111111111111ddddddd61111111
11111111111111111010110101111101111111011111110111111101101011010111111111111111111111111111111111111111111111111ddddddd61111111
111111111111111110101101011011011110110111101101111011011010110101111111111111111111113331111111111111111111333115555555555d5555
111111111111111110101110100000100000001000000010000000100010111011111111111111111113333a331111111111111113333a3315dddd5dddd5dd5d
11111111111111111010010101011110110111101101111011011110101001010111111111111111133333333b111111111111133333333b15ddddddddd5dddd
111111111111111110101101010111011101110111011101110111011010110101111111111111b31333333bb11111111111b31333333bbb15d5dddd5dd5dddd
111111111111111110101101010111111101111111011111110111111010110101111111111111b3333333bb111111111111b3333333bbb1115d5555555d5555
111111111111111111011101000000100000001000000010000000100101110101111111111111bb333bb331111111111111bb333bb33b11115dddd5dddd5dd5
111111111111111110101100101111011011110110111101101111011010110011111111111111bb33bb1b31111111111111bb33bbbb31111155ddd5ddd5ddd5
1111111111111111101011010111110111111101111111011111110110101101011111111111111bb33111b31111111111111bb33111b311115dddd5ddddddd5
555d5555555d5555555d55555110110111101101111011011110110115555555555d5555555d5555555d5555555d5555555d5555555d5555555d5555555d5555
ddd5dd5dddd5dd5dddd5dd5d5000001000000010000000100000001005dddd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5d
ddd5ddddddd5ddddddd5dddd5101111011011110110111101101111015ddddddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5dddd
5dd5dddd5dd5dddd5dd5dddd5101110111011101110111011101110115d5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd
555d5555555d5555555d555551011111110111111101111111011111115d5555555d5555555d5555555d5555555d5555555d5555555d5555555d5555555d5555
dddd5dd5dddd5dd5dddd5dd510000010000000100000001000000010015dddd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5
ddd5ddd5ddd5ddd5ddd5ddd510111101101111011011110110111101115dddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5
ddddddd5ddddddd5ddddddd511111101111111011111110111111101115555515dddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5
555d5555555d5555555d5555555d55555110110111101101111011011110110115555555555d5555555d5555555d5555555d5555555d5555555d5555555d5555
ddd5dd5dddd5dd5dddd5dd5dddd5dd5d5000001000000010000000100000001005dddd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5d
ddd5ddddddd5ddddddd5ddddddd5dddd5101111011011110110111101101111015ddddddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5dddd
5dd5dddd5dd5dddd5dd5dddd5dd5dddd5101110111011101110111011101110115d5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd
555d5555555d5555555d5555555d555551011111110111111101111111011111115d5555555d5555555d5555555d5555555d5555555d5555555d5555555d5555
dddd5dd5dddd5dd5dddd5dd5dddd5dd510000010000000100000001000000010015dddd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5
ddd5ddd5ddd588d5ddd5ddd5ddd5ddd510111101101111011011110110111101115dddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5
ddddddd5dddd8885ddddddd5ddddddd511111101111111011111110111111101115555515dddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5
555d555555598985555d5555555d5555555d55555110110111101101111011011110110115555555555d5555555d5555555d5555555d5555555d5555555d5555
ddd5dd5dddd99a9dddd5dd5dddd5dd5dddd5dd5d5000001000000010000000100000001005dddd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5d
ddd5dddddd24aa42ddd5ddddddd5ddddddd5dddd5101111011011110110111101101111015ddddddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5dddd
5dd5dddd5d2499425dd5dddd5dd5dddd5dd5dddd5101110111011101110111011101110115d5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd
555d555555244442555d5555555d5555555d555551011111110111111101111111011111115d5555555d5555555d5555555d5555555d5555555d5555555d5555
dddd5dd5dd249942dddd5dd5dddd5dd5dddd5dd510000010000000100000001000000010015dddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5
ddd5ddd5dd224422ddd5ddd5ddd5ddd5ddd5ddd510111101101111011011110110111101115dddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5
ddddddd5dd524425ddddddd5ddddddd5ddddddd51111110111111101111111011111110111555551555555515555555155555551555555515555555155555551
555d555555522225555d5555555d5555555d5555555d555551101101111011011110110111101101111011011110110111101101111011011110110111101101
ddd5dd5dddd5225dddd5dd5dddd5dd5dddd5dd5dddd5dd5d50000010000000100000001000000010000000100000001000000010000900100000001000000010
ddd5ddddddd555ddddd5ddddddd5ddddddd5ddddddd5dddd51011110110111ff1101111011011110110111101101111011011110110991101101111011011110
5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd510111011101110ff101110111011101110111011101110111011101110199011101110111011101
555d5555555d5555555d5555555d5555555d5555555d5555510111111101111fff01f11111011111110111111101111111011111110199111101111111011111
dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd51000001000000ffffffff01000000010000000100000001000000010000090100000001000000010
ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd510111101101111f4ffff110140111101101111011011110110111101888811011011110110111101
ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd511111101111111094441110111111101111111041114110111111108888881011111110111111101
555d5555555d5555555d5555555d5555555d5555555d5555555d55555110110f9910110111101101111011011114110111101108822881011110110111101101
ddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5d500000ffff00001000000010000400100000001ff4000f12222880100000001000000010
ddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5dddd510111ffff011110110111101101111011411110f1011f12228884101101111011011110
5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd51011ffffff11101110111411101110111011401110111f8888449441101110111011101
555d5555555d5555555d5555555d5555555d5555555d5555555d55555101fffffff1111111011111110111111101111441011144884499944101111111011111
dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd58d5dddd5dd5dddd5dd510000fffffff001000000010000000400000001000f00994444449444400001000000010
ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd58dd5ddd5ddd5ddd5ddd5101111ffff11110110111101401114011011410110111129449992944411110110111101
5dddddd5ddddddd5ddddddd5ddddddd5dddd88d5ddddddd5ddddddd5111111044111110111111101111411011111110111111102999222999441110111111101
05555555555d5555555d5555555d555555589995555d5555555d5555555d55555110110111401101111011011110410111101101222222289940110111101101
15dddd5dddd5dd5dddd5dd5dddd5dd5dddd99a9dddd5dd5dddd5dd5dddd5dd5d5000001000000010000000100000001000000919299222888990001000000010
05ddddddddd5ddddddd5ddddddd5dddddd24aa42ddd5ddddddd5ddddddd5dddd5101111011011144110111101101111011011494222222884491911011011110
05d5dddd5dd5dddd5dd5dddd5dd5dddd5d2499425dd5dddd5dd5dddd5dd5dddd5101110111011101110111011101110111011444299222224499410111011101
015d5555555d5555555d5555555d555555244442555d5555555d5555555d55555101111111011111110111111101111111011144222222224444411111011111
015dddd5dddd5dd5dddd5dd5dddd5dd5dd249942dddd5dd5dddd5dd5dddd5dd51000001000000010000000100000001000000018229922228844001000000010
115dddd5ddd5ddd5ddd5ddd5ddd5ddd5dd224422ddd5ddd5ddd5ddd5ddd5ddd51011110110111101101111011011110110111188822222888811110110111101
015555515dddddd5ddddddd5ddddddd5dd524425ddddddd5ddddddd5ddddddd51111110111111401111111011111110111111188888111888111110111111101
0110110115555555555d5555555d555555522225555d5555555d5555555d5555555d5555555d5555555d5555555d5555555d5555555d5555555d5555555d5555
1000001005dddd5dddd5dd5dddd5dd5dddd5225dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5d
0101111015ddddddddd5ddddddd5ddddddd555ddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5dddd
0101110115d5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd
01011111115d5555555d5555555d5555555d5555555d5555555d5555555d5555555d5555555d5555555d5555555d5555555d5555555d5555555d5555555d5555
00000010015dddd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5
10111101115dddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5
01111101115555515dddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5
011011011110110115555555555d5555555d5555555d5555555d5555555d5555555d5555555d5555555d5555555d5555555d5555555d5555555d5555555d5555
100000100000001005dddd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5d
010111101101111015ddddddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5dddd
010111011101110115d5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd
0101111111011111115d5555555d5555555d5555555d5555555d5555555d5555555d5555555d5555555d5555555d5555555d5555555d5555555d5555555d5555
0000001000000010015dddd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5
1011110110111101115dddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd8ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5
0111110111111101115555515dddddd5ddddddd5ddddddd5ddddddd5ddd89dd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5
01101101111011011110110115555555555d5555555d5555555d5555555d9985555d5555555d5555555d5555555d5555555d5555555d5555555d5555555d5555
10000010000000100000001005dddd5dddd5dd5dddd5dd5dddd5dd5dddd9999dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5d
01011110110111101101111015ddddddddd5ddddddd5ddddddd5dddddd24aa42ddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5dddd
01011101110111011101110115d5dddd5dd5dddd5dd5dddd5dd5dddd5d2499425dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd
010111111101111111011111115d5555555d5555555d5555555d555555244442555d5555555d5555555d5555555d5555555d5555555d5555555d5555555d5555
000000100000001000000010015dddd5dddd5dd5dddd5dd5dddd5dd5dd249942dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5
101111011011110110111101115dddd5ddd5ddd5ddd5ddd5ddd5ddd5dd224422ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5
011111011111110111111101115555515dddddd5ddddddd5ddddddd5dd524425ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5
0110110111101101111011011110110115555555555d5555555d555555522225555d5555555d5555555d5555555d5555555d5555555d5555555d5555555d5555
1000001000000010000000100000001005dddd5dddd5dd5dddd5dd5dddd5225dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5d
0101111011011110110111101101111015ddddddddd5ddddddd5ddddddd555ddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5dddd
0101110111011101110111011101110115d5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd
01011111110111111101111111011111115d5555555d5555555d5555555d5555555d5555555d5555555d5555555d5555555d5555555d5555555d5555555d5555
00000010000000100000001000000010015dddd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5
10111101101111011011110110111101115dddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5
01111101111111011111110111111101115555515dddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5
011011011110110111101101111011011152112515555555555d5555555d5555555d5555555d5555555d5555555d5555555d5555555d5555555d5555555d5555
100000100000001000000010000000100020002505dddd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5d
010111101101111011011110110111101125115015ddddddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5dddd
010111011101110111011101110111011105115115d5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd
0101111111011111110111111101111111021151115d5555555d5555555d5555555d5555555d5555555d5555555d5555555d5555555d5555555d5555555d5555
0000001000000010000000100000001000050052015dddd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5
1011110110111101101111011011110110251105115dddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5
0111110111111101111111011111110111251125115555515dddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5
01101101111011055555115555501101115211251152112515555555555d5555555d5555555d5555555d5555555d5555555d5555555d5555555d5555555d5555
100000100000005ddddd55ddddd50010002000250020002505dddd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5d
010111101101115d555dddd555d51110112511501125115015ddddddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5dddd
010111011101115d5d555555d5d51101110511511105115115d5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd5dd5dddd
010111111101115ddd5dddd5ddd511111102115111021151115d5555555d5555555d5555555d5555555d5555555d5555555d5555555d5555555d5555555d5555
000000100000001555555555555000100005005200050052015dddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5dddd5dd5dddd5dd5
101111011011110115dddddd501111011025110510251105115dddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddddddd5ddd5ddd5ddd5ddd5
01111101111111011155555511111101112511251125112511555551555555515555555155555551555555515555555155555551555555515dddddd5ddddddd5
011011011110110115d5dd5d511011011152112511521125115211251152112511521125115211251152112511521125115211251110110115555555555d5555
100000100000001005d5ddd5100000100020002250200022502000250020002250200025002000250020002500200022502000250000001005dddd5dddd5dd5d

__gff__
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000400002003030303000000000000000004000020030303030000000010204000012020800340054000000000008000000920208003800580
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__map__
4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e
4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e
4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4b4b4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e
4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4b4b4b4b4b4b4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e
4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4b4b4b4b4b4b4b4b4b4b4b4b4b4b4e4e4b4b4b4b4b4b4b4b4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e
4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e6c4e4b4b4b4b4b4b4b4b4b4b4b4b4b4b4e4e4b4b4b4b4b4b4b4b4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e
4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4b4b4b4b4b4b4b4b4b4b4b4b674b4e4e4b4b4b4b4b4b674b4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e
4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4b484b4b754b4b4b4b4b4b4b664b4e4e4b664b4b4b4b664b4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e
4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4b4b4e4e4b4b7e4b4e4e4b7e4b4b4b4b7e4b4b4e4e6c4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e
4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4b4b4b4b4b4b7e4b4e4e4b7e4b4b4b4b7e4b4b4b4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e
4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4b4b4b4b4b4b7e4b4e4e4b7e4b696a4b7e4b4b4b4b4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e
4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4b4b4b4b4b4b7e484e4e4b7e4b797a4b7e4b4b4b4b4b4e4e6c4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e
4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4b4b4b4b4b4e4e4e4e4e4e4e4e4e4e4e4e4e4b4b4b4b4b4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e
4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4b4b4b4b4b4e4e4e4e4e4e4e4e4e4e4e6c4e4e4b4b4b4b4b4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e
4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4b4b4b4b4b4b4b4b4e4e4e4e4e4e4e4e4e4e4e4e4b4b4b4b4b4e4e6c4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e
4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4b664b4b4b4b4b4b4e4e4e4e4e4e4e4e4e4e4e4e4e4b4b4b4b4b4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e
4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4b7e4b4b4b4b4b4b4e4e4e4e4e4e4e4e4e4e4e6c4e4e4b4b4b4b4b4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e
4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4b7e4b4b4b4b4b4b4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4b4b4b4b4b4e4e6c4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e
4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4b7e4b4b4b4b4b4b4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4b4b4b4b4b4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e
4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e53535353535353535b4b4b4e4e4e4e4e4e4e4e4e4e4e4b7e4b4b4b4b4b4b4e4e4e4e4e4e4e4e4e4e4e4e4e4e6c4e4e4b4b4b4b4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e
4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e53535353535353535b4b4b4e4e4e4e4e4e4e4e4e4e4e4e4e4e4b4b4b4b4b4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4b4b4b4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e
4e4e4e4e4e4e4e4e4e4e5353535353535353535353534e4e4e4e4e4e4e4e4e4e4e4e53535353535353535b484b4e4e4e4e4e4e4e4e4e4e4e4e4e4e4b4b4b4b4b4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4b4b4b4b4b4b4b4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e
4e4e4e4e4e4e4e4e4e4e5353535353535353535353534e4e4e4e535353535353535353535353535353534e4e4e4e4e4b4b4b4b4b4b4b4e4e4b4b4b4b4b4b4b4b4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e6c4e4e4b4b784b4b4b4e6c4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e
4e4e4e4e4e4e4e4e4e4e5353535353535353535353534e4e4e4e535353535353535353535353535353534e4e4e4e4e4b484b4b4b4b4b4e4e4b4b4b4b4b4b4b4b4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4b4b4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e
4e5353535353535353535353535353535353534853534e4e4e4e5353535353535353535353534e5353534e4e4e4e4e4e4e4e4b4b4b4b4e4e4b4b4b4b4b4b654b4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4b4b4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e
4e5353535353535353535353535353535353537e53534e4e4e4e5353534e4e4e4e4e53535353535353534e4e4e4e4e4e4e4e4b4b4b4b4e4e4b4b4b4b4b4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4b4b4b4b4b4b4b4b4b4b4b4b4e6c4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e
4e5353535353535353535353535353534e53537e53534e4e4e4e5353534e4e4e4e4e53535353535353534e6c4e4e4e4e6c4e4b4b4b4b4e4e4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b784b4b4b4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e
4e5353535353535353535353535353535353537e53534e4e4e4e5353534e4e4e4e4e53535353535353534e4e4e4e4e4e4e4e4b4b4b4b4e4e4b4b4b4b4b4b4b4b4b674b4b4b4b4b4b4b674b4b4b4b4b4b4b4b4b4b4b4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e
4e5353535353675353535353534853535353537e5353535353535353535b4b4b4e4e53535353535353535b4b4b4b4b4b4b4b4b4b4b4b4e4e4b4b4b4b4b4b4b4b4b754b4b4b4b4b4b4b754b4b4b4b4b4b4b4b4b4b4b4e6c4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e
4e5353535353665353535353537e53535353537e5353535353535353675b4b4b4e4e53535353535353535b4b4b4b4b4b4b4b4b4b4b4b4e4e4b4b4b4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4b4b4b4b4b4b4b4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e
4e53535353537e5353535353537e53535353534e5353535353535353755b4b484e4e53535353535353534e4e4e4e4e4e4e4e4b4b4b4b4e4e4b4b4b4b4b4b4b4b5b53535353535353535353534e4e4b4b4b4b4b4b4b4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e
4e53535353537e5353535353537e53535353535353535353534e4e4e4e4e4e4e4e4e65535353535353654e6c4e4e4e4e6c4e4b654b654e4e4b4b4b4b4b4b4b4b5b53675353535353535353484e4e4b4b4b4b4b4b4b4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e
__sfx__
012000001a552245000050015550185510050015550005001455200500155500050015552005000050021550205520050000500215501f5521850000500215501e55200500215520050021552005000050000500
012000000b0200000000000000000c0200000000000000000e0200000000000000000000000000000000b0100c02000000100200b0100c02000000100200b0100c020000000f020000000f020000000000000000
0120002029010000000000000000270100000000000000002a010000000000000000000000000000000000000000000000200100000018000000001f0100000000000000001b010000001b010000001800000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
010b00003c61005650306503265034650356502b6502d6102f6103461030610326101a6101c6102861029610306103261034610356103b610306103e6103961026610396102961034610356103e6103c61001610
011000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__music__
01 00010244
02 40010244

