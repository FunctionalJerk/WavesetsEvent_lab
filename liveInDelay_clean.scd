
(
b = Buffer.alloc(s, s.sampleRate * 16);
c = Buffer.read(s, Platform.resourceDir +/+ "sounds/a11wlk01.wav");

~phaseBus = Bus.control(s);

~wsArray = WavesetsEvent.new.dup(16).collect{|x| x.setBuffer(b) };
~sampleRate = s.sampleRate;

~division = 0.5;
~repeats = 3;
~rand = 2;
~delayTime = 1;
~start = 0;
~num = 12;
~rate = 1;
~rate2 = 1;
~legato = 1;
~fixsustain = nil;
~spread = 0.0;
~minLength = 20;
~baseAmp = 0.4;

~play = true;

~w = Pseq(~wsArray,inf);
w = ~w.asStream;

t = TempoClock.default;

WavesetsEvent.prepareSynthDefs;
// new feature:
Wavesets2.printStats_(true);
)

(
var currWs = w.next;
var inBus = 2;
// envelopes für \wvst2glenv
q = q ? ();
q.envBuf_full = Buffer.sendCollection(s, Env([1, 1, 0], [1.0, 0.0]).discretize);
q.envBuf_sine = Buffer.sendCollection(s, Env.sine.discretize, 1);
q.envBuf_gaus = Buffer.sendCollection(s, Env([0, 1, 1, 0], [0.1, 0.8, 0.1], \sin).discretize);
q.envBuf_perc = Buffer.sendCollection(s, Env.perc(0.0001,0.9999).discretize, 1);

~env = q.envBuf_full;

SynthDef(\writeAndSend, {|recbuf,playbuf,run=1, hpf=100, out|

	// var in = PlayBuf.ar(1, playbuf, loop: 1);
	var in = SoundIn.ar(inBus);
	var phase = Phasor.ar(rate: BufRateScale.kr(recbuf)* run, end: BufFrames.kr(recbuf));
	var kphase = A2K.kr(phase);

	BufWr.ar(HPF.ar(in, hpf), recbuf, phase);

	ReplaceOut.kr(out,kphase)

}, \ir.dup ++ \kr).add;

~getLatestWs = {|ws,minLength=30,endFrame,numFrames|

	var wsbuf = ws.buffer;
	var bufSize = wsbuf.numFrames;
	var startFrame = endFrame - numFrames;

	// wavesets sollen nicht über mehr als den halben Buffer
	numFrames = numFrames.clip2(bufSize / 2);

	ws.setBuffer(wsbuf,
		minLength: minLength,
		startFrame: startFrame,
		endFrame: endFrame,
	);
};

~delayT = Tdef(\delay, {
	var phase, pan;
	var startFrame, numFrames, wsFrames, xings;

	inf.do{
		if(x.notNil && ~play) {
			if(currWs.buffer.notNil && (currWs.size != 0)) {

				xings = currWs.wavesets;
				pan = ~spread.rand2;

				~repeats.max(1).do{|x|
					var start = ~start + ~rand.rand;
					var num = ~num;
					var rate = ~rate;
					var rate2 = ~rate2;
					var delayTime = ~delayTime;
					var legato = ~legato;
					var env = ~env;
					var amp = ~baseAmp * (~repeats - x / ~repeats);
					var time = delayTime * x / ~division;
					var thisWs = currWs;

					t.sched(time, {

						wsFrames = xings.lengths[start..start+num].sum;
						startFrame = xings.xings[start];

						if(~fixsustain.notNil) {
							rate = wsFrames / (~fixsustain * ~sampleRate);
							rate2 = rate;
						};
						/*
						t.beats.postcln;
						"".postln;*/

						Synth(\wvst2glenv, [
							\out, 0,
							\pan, pan,
							\buf, thisWs.buffer,
							\startFrame, startFrame,
							\numFrames, wsFrames,
							\rate, rate,
							\rate2, rate2,
							\sustain, delayTime * legato,
							\amp, amp,
							\interpolation, 2,
							\envBuf, env
						]);
						nil;
					})
				}
			}
		};

		numFrames = (s.sampleRate * ~delayTime).ceil.asInteger;
		x = w.next;
		phase = ~phaseBus.getSynchronous - 1;
		fork { ~getLatestWs.(ws:x,numFrames:numFrames,endFrame:phase) };
		~delayTime.wait;
	}
})
)

// den "delay" starten
(
~phaseSynth = Synth(\writeAndSend, [\recbuf, b, \playbuf, c, \delayTime, ~delayTime, \out, ~phaseBus.index, \run, 1]);
~delayT.play;
)

~wsArray[0].

/////////////////////////////////////////////////
// live Coding
// ~repeats.do{|x| var time = t.beats; t.sched(~delayTime * x / ~division, { time.postln }) };

(
~division = 2;
~rand = 0;
~repeats = 1;
~delayTime = 0.1;
~env = q.envBuf_full;
~start = 0;
~num = 1;
~rate = 1;
~rate2 = 1;
~legato = 4;
// ~fixsustain = -0.125;
~fixsustain = 100.reciprocal;
~spread = 1.0;
~minLength = 20;
~baseAmp = 0.4;
)
(
~division = 0.5;
~delayTime = 0.25;
~rand = 4;
~repeats = 1;
~env = q.envBuf_sine;
~start = 0;
~num = 1;
~rate = 1;
~rate2 = 1;
~legato = 3;
~fixsustain = 100.reciprocal;
// ~fixsustain = nil;
~spread = 1.0;
~minLength = 20;
~baseAmp = 0.4;
)
(
~delayTime = 0.05;
~env = q.envBuf_sine;
~start = 0;
~num = 1;
~rate = 1;
~rate2 = 1;
~legato = 2;
~fixsustain = 200.reciprocal;
~spread = 0.2;
~minLength = 20;
~baseAmp = 0.4;
)

(
~delayTime = 0.125;
~env = q.envBuf_perc;
~start = 0;
~num = 6;
~rate = 1;
~rate2 = 1;
~legato = 4;
// ~fixsustain = 500.reciprocal;
~fixsustain = nil;
~spread = 0.2;
~minLength = 20;
~baseAmp = 0.5;
)


(
~delayTime = 1;
~env = q.envBuf_perc;
~start = 0;
~num = 2;
~rate = 1;
~rate2 = 1;
~legato = 6;
~fixsustain = 140.reciprocal;
// ~fixsustain = nil;
~spread = 0.0;
~minLength = 20;
~baseAmp = 0.8;
)


////////////////////////////////////////////////////
// als Pattern
//
(
~phaseSynth = Synth(\writeAndSend, [\recbuf, b, \playbuf, c, \delayTime, ~delayTime, \out, ~phaseBus.index, \run, 1]);
~delayTime = 0.5;
~play = false;
~delayT.play;
)

(
// aber so kann leider nicht auf Stille am Input reagiert werden
Pdef(\x,Pwavesets(Pbind(
	\instrument, \wvst2glenv,
	\envBuf, q.envBuf_sine,	//
	\wavesets, ~w,
	\start, 0,
	\num, 2,
	\rate, Pseq([1,0.5,0.25,0.5,1.25,1.5],inf),
	// \rate2, Pshuf([1,2,0.98,1.02],inf),
	\wsamp, 0.1,
	\dur, Pseq([0.25,0.25,0.5,0.25,0.75],inf) * 0.05,
	\dur, Pif(Pfunc{|args| ~test.(args) }, Rest(Pkey(\dur)).poll, Pkey(\dur)),
	\legato, 4,
	\func, Pfunc({|args| ~test2.(args) }),
	\sustain, Pkey(\dur) * Pkey(\legato),
	\out, 0
))).play;
~test = {|val| var ws = val[\wavesets]; ((ws == x) || (ws.size != 0)) };
~test2 = {|val| (val[\wavesets] == x).postln };
)

