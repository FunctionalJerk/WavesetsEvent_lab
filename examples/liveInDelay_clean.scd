Buffer.freeAll
(
b = Buffer.alloc(s, s.sampleRate * 16);
c = Buffer.read(s, Platform.resourceDir +/+ "sounds/a11wlk01.wav");

~phaseBus = Bus.control(s);

~wsArray = WavesetsEvent.new.dup(16).collect{|x| x.setBuffer(b) };
~sampleRate = s.sampleRate;

~w = Pseq(~wsArray,inf);
w = ~w.asStream;

WavesetsEvent.prepareSynthDefs;
// new feature:
Wavesets2.printStats_(false);
)

(
q = q ? ();
q.envBuf_full = Buffer.sendCollection(s, Env([1, 1, 0], [1.0, 0.0]).discretize);
q.envBuf_sine = Buffer.sendCollection(s, Env.sine.discretize, 1);
q.envBuf_gaus = Buffer.sendCollection(s, Env([0, 1, 1, 0], [0.1, 0.8, 0.1], \sin).discretize);
q.envBuf_perc = Buffer.sendCollection(s, Env.perc(0.0001,0.9999).discretize, 1);

SynthDef(\writeAndSend, {|recbuf,playbuf,run=1, hpf=100, out|

	// var in = PlayBuf.ar(1, playbuf, loop: 1);
	var in = SoundIn.ar(2);
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
)

// live Coding

(
~delayTime = 0.5;
~env = q.envBuf_full;
~start = 0;
~num = 30;
~rate = -1;
~rate2 = -1;
~overlap = 1;
// ~fixsustain = -0.125;
~fixsustain = nil;
~spread = 1.0;
~minLength = 20;
~baseAmp = 1;
)
(
~delayTime = 0.05;
~env = q.envBuf_sine;
~start = 0;
~num = 1;
~rate = 1;
~rate2 = 1;
~overlap = 2;
~fixsustain = 200.reciprocal;
~spread = 0.2;
~minLength = 20;
~baseAmp = 0.5;
)

(
~delayTime = 0.05;
~env = q.envBuf_perc;
~start = 0;
~num = 1;
~rate = 1;
~rate2 = 1;
~overlap = 2;
~fixsustain = 1000.reciprocal;
~spread = 0.2;
~minLength = 20;
~baseAmp = 0.5;
)


(
~delayTime = 1;
~env = q.envBuf_perc;
~start = 0;
~num = 1;
~rate = 1;
~rate2 = 0.8;
~overlap = 6;
~fixsustain = 140.reciprocal;
// ~fixsustain = nil;
~spread = 0.0;
~minLength = 20;
~baseAmp = 0.5;
)

(
var currWs = w.next;

~delayT = Tdef(\delay, {
	var phase;
	var startFrame, numFrames, wsFrames, xings;

	inf.do{
		if(currWs.notNil) {
			if(currWs.buffer.notNil && (currWs.size != 0)) {

				xings = currWs.wavesets;
				startFrame = xings.xings[~start];

				wsFrames = xings.lengths[~start..~start+~num].sum;

				if(~fixsustain.notNil) {
					~rate = wsFrames / (~fixsustain * ~sampleRate);
					~rate2 = ~rate;
				};

				Synth(\wvst2glenv, [
					\out, 0,
					\pan, ~spread.rand2,
					\buf, currWs.buffer,
					\startFrame, startFrame,
					\numFrames, wsFrames,
					\rate, ~rate,
					\rate2, ~rate2,
					\sustain, ~delayTime * ~overlap,
					\amp, ~baseAmp,
					\interpolation, 2,
					\envBuf, ~env
				]);
			}
		};

		numFrames = (s.sampleRate * ~delayTime).ceil.asInteger;
		currWs = w.next;
		phase = ~phaseBus.getSynchronous;
		fork { ~getLatestWs.(ws:currWs,numFrames:numFrames,endFrame:phase) };
		~delayTime.wait;
	}
})
)

~getLatestWs.(ws:~wsArray.choose,numFrames:20000,endFrame:~phaseBus.getSynchronous)

~phaseSynth = Synth(\writeAndSend, [\recbuf, b, \playbuf, c, \delayTime, ~delayTime, \out, ~phaseBus.index, \run, 1]);
~delayT.play(quant:1);

~phaseSynth.set(\hpf, 50)

~delayT.stop
~phaseSynth.release
~wsArray[0].buffer.play

(
// Als Pattern
Pdef(\x,Pwavesets(Pbind(
	\instrument, \ownWs,
	// \wavesets, Pseq(~wsArray,inf),
	\func, Pfunc({ w.next }),
	\wavesets, Pkey(\func), // Pif ?
	\start, 0,
	\num, 2,
	// \rate, 1,
	\legato, 4,
	\fixsustain, 150.reciprocal * Pkey(\num),
	\wsamp, 0.2,
	\envBuf, q.envBuf_perc,	//
	\dur, Pseq([0.25,0.25,0.5,0.5,0.25,0.75,0.5],inf) * 0.5,
	\repeats, Pkey(\dur) / Pkey(\fixsustain) * Pkey(\legato),
	// \repeats, 20,
	\key, Pfunc({|a| ~test.(a) }),
	\out, 0
))).play;
)

~t.asStream
~test = {|val| val[\wavesets].size.postln };

(

// not usable!
~phaseSynth.set(\run, 0);
~delayT.pause;

~freezeAndChange.(~phaseBus.getnSynchronous)
)

{ SoundIn.ar([2,3],1) }.play

Platform.userExtensionDir
