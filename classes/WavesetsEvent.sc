
/*

todo: - global cache like in the original Wavesets
(mine)- add argument "wrap", that makes ~num wrap around the numXings
- if endTime < startTime; switch end and start and make ~rate = ~rate.neg

*/


AbstractWavesetsEvent {
	classvar <all;

	*initClass {
		all = IdentityDictionary.new
	}

	add { |name|
		all.put(name, this)
	}

	isReady {
		^this.subclassResponsibility(thisMethod)
	}

	asEvent { |inevent|
		inevent = inevent ?? { () };
		inevent = inevent.copy;
		inevent.use { this.addToEvent };
		^inevent
	}

	addToEvent {
		this.addBuffersToEvent;
		this.addWavesetsToEvent;
		this.finalizeEvent;
	}

	*findWavesets { |inevent|
		^inevent.at(\wavesets) ?? { all.at(inevent.at(\name)) }
	}

	*asEvent { |inevent|
		var wavesets;
		wavesets = inevent.at(\wavesets);
		wavesets !? {
			^wavesets.asEvent(inevent)
		};
		wavesets = all.at(inevent.at(\name));
		if(wavesets.isNil) { "no wavesets with this name: %".format(inevent.at(\name)).warn; ^nil };
		if(wavesets.isReady.not) { "wavesets not initialised: %".format(inevent.at(\name)).warn; ^nil };
		^wavesets.asEvent(inevent)
	}

	makeEvent { |start=0, num, end, rate=1, rate2, legato, wsamp, useFrac|
		var event = (
			start: start,
			end: end, num: num,
			rate: rate,
			rate2: rate2,
			legato: legato,
			wsamp: wsamp,
			useFrac:useFrac
		);
		^this.asEvent(event);
	}


	// backwards compatibility
	eventFor { |startWs=0, numWs=5, repeats=3, rate=1, useFrac = true|
		^this.asEvent((start: startWs, num: numWs, repeats: repeats, rate: rate, useFrac: useFrac))
	}

	toBuffer { |buffer, onComplete|
		^this.shouldNotImplement(thisMethod)
	}



}



WavesetsEvent : AbstractWavesetsEvent {


	var <buffer, <wavesets;
	var <>restDur = 0.01; // when waveset contains no frames

	*read { |path, channel = 0, startFrame = 0, numFrames = -1, onComplete, server, minLength|
		^this.new.readChannel(path, channel, startFrame, numFrames, onComplete, server, minLength)
	}

	read { |path, channel = 0, startFrame = 0, numFrames = -1, onComplete, server, minLength|
		^this.readChannel(path, channel, startFrame, numFrames, onComplete, server, minLength)
	}

	readChannel { |path, channel = 0, startFrame = 0, numFrames = -1, onComplete, server, minLength|
		var finish, buffer;
		server = server ? Server.default;
		if(server.serverRunning.not) {
			"Reading WavesetsBuffer failed. Server % not running".format(server).warn;
			^this
		};
		finish = { this.setBuffer(buffer, onComplete, minLength) };
		buffer = Buffer.readChannel(server ? Server.default, path, startFrame, numFrames, channels: channel.asArray, action: finish);
	}

	/*
	setBuffer { |argBuffer, tmpBuffer, onComplete, minLength,startFrame,endFrame|
	wavesets = Wavesets2.fromBuffer(argBuffer, tmpBuffer, onComplete, minLength, startFrame, endFrame);
	buffer = argBuffer;
	}
	*/
	setBuffer { |argBuffer, onComplete, minLength,startFrame,endFrame|
		wavesets = Wavesets2.fromBuffer(argBuffer, onComplete, minLength, startFrame, endFrame);
		buffer = argBuffer;
	}

	free {
		// buffer.free;
		wavesets = nil;
	}

	size { ^wavesets.size }

	duration { ^buffer.duration }

	server { ^buffer.server }

	isReady { ^wavesets.notNil and: { buffer.notNil } }

	asBuffer { |server, onComplete|
		if(server != buffer.server) { Error("can't copy waveset to another server").throw };
		^buffer
	}

	addBuffersToEvent {
		~buf = buffer.bufnum;
		~sampleRate = buffer.sampleRate;
	}

	addWavesetsToEvent {
		var theseXings, startWs, endWs, useFrac, frames, rotations;
		useFrac = ~useFrac ? true;
		theseXings = if (useFrac) { wavesets.fracXings } { wavesets.xings };

		~startTime !? { ~start = wavesets.nextStartCrossingIndex(~startTime * ~sampleRate, useFrac) };
		startWs = ~start ? 0;
		~startFrame = theseXings.clipAt(startWs);
		// ~startFrame = theseXings.wrapAt(startWs);

		~endTime !? { ~end = wavesets.nextCrossingIndex(~endTime * ~sampleRate, useFrac) };

		if(~wsustain.notNil) {

			// why ~sustain.value? Mistake or does this mean we only want ~wsustain together with ~sustain?
			// doesn't this conflict with rate != 1 ?
			// when sustain is rounded to the next crossing, we dont need ~wsustain?
			endWs = wavesets.nextCrossingIndex(~startFrame + (~wsustain * ~sampleRate), useFrac);
		} {
			// in every case, not only if ~wssustain.notNil
			~num = if(~end.notNil) { max(~end - startWs, 1) } { ~num ? 1 };
			endWs = startWs + ~num;
		};
		/*
		///////////////////////////////////////////
		if(~sustain.notNil) {

		frames = ~sustain.value * ~sampleRate + ~startFrame;
		rotations = (frames / theseXings.last).floor * theseXings.last;
		// ~startFrame = theseXings.clipAt(startWs);
		~numFrames = rotations + wavesets.nextCrossing(~startFrame + frames % theseXings.last, ~useFrac) - ~startFrame;
		} {
		~endFrame = theseXings.clipAt(endWs); //  different
		~numFrames = ~endFrame - ~startFrame;
		};
		*/
		///////////////////////////////////////////////////
		~endFrame = theseXings.clipAt(endWs); //  different
		~numFrames = ~endFrame - ~startFrame;

		if(~fixsustain.notNil) {
			~rate = ~numFrames / (~fixsustain * ~sampleRate);
			if(~rate2.notNil) { ~rate2 = ~rate2 * ~rate };
		};
		if(~wsamp.notNil) { ~amp =  ~wsamp / wavesets.maximumAmp(~start, ~num) };
	}


	finalizeEvent {
		var averagePlaybackRate, reverse;
		currentEnvironment.useWithoutParents {
			if(~numFrames <= 0) {
				~type = \rest;
				~dur = restDur;
			} {
				~rate = ~rate ? 1.0;
				if(~rate2.notNil) {
					averagePlaybackRate = ~rate + ~rate2 * 0.5;
					if(~envBuf.notNil) {
						~instrument = ~instrument ? \ownWs;
					} {
						~instrument = ~instrument ? \wvst1gl;
					}
				} {
					averagePlaybackRate = ~rate;
					~instrument = ~instrument ? \wvst0;
					~rate2 = ~rate;
				};

				/*
				if(~sustain.notNil) {
				~sustain = abs(~numFrames / (~sampleRate * averagePlaybackRate.abs));
				} {
				~sustain = abs(~numFrames * (~repeats ? 1).floor.max(1) / (~sampleRate * averagePlaybackRate.abs))
				};
				*/
				~sustain = ~sustain ?? {
					abs(~numFrames * (~repeats ? 1).floor.max(1) / (~sampleRate * averagePlaybackRate.abs))
				};

				~dur ?? {
					~dur = if(~legato.isNil) { ~sustain } { ~sustain / ~legato };
					if(~dur.isArray) { ~dur = ~dur[0] };
					if(~dur < 0.0001) { ~type = \rest }; // this is ad hoc
				}
			}
		}
	}

	plot { |index = 0, length = 1|
		^wavesets.plot(index, length, buffer.sampleRate)
	}

	// equality

	== { |that|
		^this.compareObject(that, #[\wavesets, \buffer])
	}

	hash {
		^this.instVarHash(#[\wavesets, \buffer])
	}

	// fft
	// note that if you send too many requests, the corresponding action may not be called
	// this is because of the getn implementation, which has no id

	getFFT { |index, num, size, action|

		var real, imag, cosTable, complex;
		var start, numFrames, func;
		#start, numFrames = this.wavesets.frameFor(index, num, false);
		size = size ?? { numFrames.nextPowerOfTwo };
		func = { |arr|

			if(arr.size != size) { arr = arr.resamp1(size) };
			real = arr.as(Signal);
			imag = Signal.newClear(size);
			cosTable = Signal.fftCosTable(size);


			complex = fft(real, imag, cosTable);

			action.value(complex, real, imag);
		};

		if(buffer.server.isLocal) {
			this.buffer.loadToFloatArray(start, numFrames, func)
		} {
			this.buffer.getToFloatArray(start, numFrames, 0.01, 3, func)
		};
	}


	*prepareSynthDefs {
		/*
		q = q ? ();
		q.envBuf_full = Buffer.sendCollection(s, Env([1, 1, 0], [1.0, 0.0]).discretize);
		q.envBuf_sine = Buffer.sendCollection(s, Env.sine.discretize, 1);
		q.envBuf_gaus = Buffer.sendCollection(s, Env([0, 1, 1, 0], [0.1, 0.8, 0.1], \sin).discretize);
		q.envBuf_gau2 = Buffer.sendCollection(s, Env([0, 1, 1, 0], 1/3!3, \sin).discretize);
		q.envBuf_perc = Buffer.sendCollection(s, Env.perc(0.001,0.999).discretize, 1);
		q.envBuf_tri = Buffer.sendCollection(s, Env([0, 1, 0], [0.5, 0.5], \lin).discretize);
		q.envBuf_expodec = Buffer.sendCollection(s, Env([1, 0.001], [1], \exp).discretize);	// expoDec (exponential decay);
		q.envBuf_revexpodec = Buffer.sendCollection(s, Env([0.001, 1], [1], \exp).discretize); // revExpoDec (reverse exponential
		// q.envBuf_halfSinA = Buffer.sendCollection(s, Env.new(times: [0,1],curve: [\lin,\sin]).discretize);
		// q.envBuf_halfSinB = Buffer.sendCollection(s, Env.new(times: [1,0],curve: [\sin,\lin]).discretize);
		*/
		SynthDef(\wvst0, { | out = 0, buf = 0, startFrame = 0, numFrames = 441, rate = 1, sustain = 1, amp = 0.1, pan, interpolation = 2 |
			var phasor = Phasor.ar(0, BufRateScale.ir(buf) * rate * sign(numFrames), 0, abs(numFrames)) + startFrame;
			var env = EnvGen.ar(Env([amp, amp, 0], [sustain, 0]), doneAction: 2);
			var snd = BufRd.ar(1, buf, phasor, 1, interpolation) * env;

			OffsetOut.ar(out, Pan2.ar(snd, pan));
		}, \ir.dup(9)).add;

		SynthDef(\wvst1gl, { | out = 0, buf = 0, startFrame = 0, numFrames = 441, rate = 1, rate2 = 1, sustain = 1,
			amp = 0.1, pan, interpolation = 2 |
			var rateEnv = Line.ar(rate, rate2, sustain) * sign(numFrames);
			var phasor = Phasor.ar(0, BufRateScale.ir(buf) * rateEnv, 0, abs(numFrames)) + startFrame;
			var env = EnvGen.ar(Env([amp, amp, 0], [sustain, 0]), doneAction: 2);
			var snd = BufRd.ar(1, buf, phasor, 1, interpolation) * env;

			OffsetOut.ar(out, Pan2.ar(snd, pan));
		}, \ir.dup(10)).add;

		SynthDef(\ownWs, { | out = 0, pan = -1, buf = 0, startFrame = 0, numFrames = 441, rate = 1, rate2 = 1, sustain = 1, amp = 0.1, interpolation = 2, envBuf = 1|
			var rateEnv = Line.ar(rate, rate2, sustain) * sign(numFrames);
			var phasor = Phasor.ar(0, BufRateScale.ir(buf) * rateEnv, 0, abs(numFrames)) + startFrame;
			var env = Osc1.ar(envBuf, sustain);
			var snd = BufRd.ar(1, buf, phasor, 1, interpolation) * env * amp;
			OffsetOut.ar(out, Pan2.ar(snd,pan));
		}, \ir.dup(11)).add;

	}



}


