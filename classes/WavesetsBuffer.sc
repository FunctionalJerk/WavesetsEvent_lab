
/*

todo: global cache like in the original Wavesets

*/


AbstractWavesetsEvent {
	classvar <all;

	*initClass {
		all = IdentityDictionary.new
	}

	add { |name|
		var old = all.at(name);
		old.free;
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

	*asEvent { |inevent|
		var item = all.at(inevent.at(\name));
		if(item.isNil) { "no wavesets with this name: %".format(inevent.at(\name)).warn; ^nil };
		if(item.isReady.not) { "wavesets not initialised: %".format(inevent.at(\name)).warn; ^nil };
		^item.asEvent(inevent)
	}

	makeEvent { |start=0, num, end, rate=1, rate2, legato, wsamp, useFrac|
		var event = (start: start, end: end, num: num, rate: rate, rate2: rate2, legato: legato, wsamp: wsamp, useFrac:useFrac);
		^this.asEvent(event);
	}


	// backwards compatibility
	eventFor { |startWs=0, numWs=5, repeats=3, playRate=1, useFrac = true|
		^this.asEvent((start: startWs, length: numWs, repeats: repeats, rate: playRate, useFrac: useFrac))
	}

	toBuffer { |buffer, onComplete|
		^this.shouldNotImplement(thisMethod)
	}
}



WavesetsEvent : AbstractWavesetsEvent {


	var <buffer, <wavesets;

	*read { |path, channel = 0, startFrame = 0, numFrames = -1, onComplete, server|
		^this.new.readChannel(path, channel, startFrame, numFrames, onComplete, server)
	}

	readChannel { |path, channel = 0, startFrame = 0, numFrames = -1, onComplete, server|
		var finish, buffer;
		server = server ? Server.default;
		if(server.serverRunning.not) {
			"Reading WavesetsBuffer failed. Server % not running".format(server).warn;
			^this
		};
		finish = { this.setBuffer(buffer, onComplete) };
		buffer = Buffer.readChannel(server ? Server.default, path, startFrame, numFrames, channels: channel.asArray, action: finish);
	}

	setBuffer { |argBuffer, onComplete|
		wavesets = Wavesets2.new.fromBuffer(argBuffer, onComplete);
		buffer = argBuffer;
	}

	free {
		buffer.free;
		wavesets = nil;
	}

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
		var theseXings = if (~useFrac ? true) { wavesets.fracXings } { wavesets.xings };
		var startWs = ~start ? 0;
		~num = if(~end.notNil) { ~end - startWs } { ~num ? 1 };
		~startFrame = theseXings.clipAt(startWs);
		~endFrame = theseXings.clipAt(startWs + ~num);
		~numFrames = absdif(~endFrame, ~startFrame);
		~amp = if(~wsamp.isNil) { 1.0 } { ~amp =  ~wsamp / wavesets.maximumAmp(~start, ~num) };
	}

	finalizeEvent {
		~rate = ~rate ? 1.0;
		~sustain = ~numFrames / (~sampleRate * ~rate) * (~repeats ? 1);
		~legato !? {
			~dur = ~sustain / ~legato;
			if(~dur < 0.0001) { ~type = \rest }; // this is ad hoc
		};
		~instrument = if(~rate2.notNil) { \wvst1gl } { \wvst0 };
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


	*prepareSynthDefs {

		SynthDef(\wvst0, { | out = 0, buf = 0, startFrame = 0, numFrames = 441, rate = 1, sustain = 1, amp = 0.1, pan, interpolation = 2 |
			var phasor = Phasor.ar(0, BufRateScale.ir(buf) * rate, 0, numFrames) + startFrame;
			var env = EnvGen.ar(Env([amp, amp, 0], [sustain, 0]), doneAction: 2);
			var snd = BufRd.ar(1, buf, phasor, 1, interpolation) * env;

			OffsetOut.ar(out, Pan2.ar(snd, pan));
		}, \ir.dup(9)).add;

		SynthDef(\wvst1gl, { | out = 0, buf = 0, startFrame = 0, numFrames = 441, rate = 1, rate2 = 1, sustain = 1,
			amp = 0.1, pan, interpolation = 2 |
			var rateEnv = Line.ar(rate, rate2, sustain);
			var phasor = Phasor.ar(0, BufRateScale.ir(buf) * rateEnv, 0, numFrames) + startFrame;
			var env = EnvGen.ar(Env([amp, amp, 0], [sustain, 0]), doneAction: 2);
			var snd = BufRd.ar(1, buf, phasor, 1, interpolation) * env;

			OffsetOut.ar(out, Pan2.ar(snd, pan));
		}, \ir.dup(10)).add;

	}



}



WavesetsMultiEvent : AbstractWavesetsEvent {


	var <bufferArray, <wavesetsArray;

	*read { |path, channels = 0, startFrame = 0, numFrames = -1, onComplete, server|
		^this.new.readAllChannels(path, channels, startFrame, numFrames, onComplete, server)
	}

	readAllChannels { |path, channels = 0, startFrame = 0, numFrames = -1, onComplete, server|
		var finish, buffers, count;
		server = server ? Server.default;
		if(server.serverRunning.not) {
			"Reading WavesetsBuffer failed. Server % not running".format(server).warn;
			^this
		};
		channels = channels.asArray;
		count = channels.size;
		finish = { if(count > 1) { count = count - 1 } { this.setBufferArray(buffers, onComplete) } };

		buffers = channels.asArray.collect { |each|
			Buffer.readChannel(server ? Server.default, path, startFrame, numFrames, channels: each, action: finish)
		};
	}

	setBufferArray { |buffers, onComplete|
		var count = buffers.size;
		var finish = { if(count > 0, { count = count - 1 }, onComplete) };
		wavesetsArray = buffers.collect { |each| Wavesets2.new.fromBuffer(each, onComplete) };
		bufferArray = buffers;
	}

	isReady { ^wavesetsArray.notNil and: { bufferArray.notNil } }

	addToEvent {
		var lastIndex = wavesetsArray.size - 1;
		var guide = (~guide? 0).clip(0, lastIndex);
		var guideWavesets = wavesetsArray[guide];
		var theseXings = if (~useFrac ? true) { guideWavesets.fracXings } { guideWavesets.xings };
		var startWs = ~start ? 0;

		~num = if(~end.notNil) { ~end - startWs } { ~num ? 1 };
		~startFrame = theseXings.clipAt(startWs);
		~endFrame = theseXings.clipAt(startWs + ~num);
		~numFrames = absdif(~endFrame, ~startFrame);
		~amp = if(~wsamp.isNil) { 1.0 } { ~amp =  ~wsamp / guideWavesets.maximumAmp(~start, ~num) };


		~allStarts = wavesetsArray.collect { |each, i|
			if(guide == i) {
				~startFrame
			} {
				each.nextCrossing(~startFrame)
			};
		};

		~allEnds = wavesetsArray.collect { |each, i|
			if(guide == i) {
				~endFrame
			} {
				each.prevCrossing(~endFrame)
			};
		};

		~sampleDur = bufferArray[guide].sampleRate.reciprocal;
		~lag = ~allStarts - ~startFrame * ~sampleDur;
		~rate = ~rate ? 1.0;

		~startFrame = ~allStarts;
		~endFrame = ~allEnds;
		~sustain = (~endFrame - ~startFrame) * ~sampleDur * (~repeats ? 1);

		~legato !? {
			~dur = ~sustain[guide] / ~legato;
			if(~dur < 0.0001) { ~type = \rest }; // this is ad hoc
		};

		~busOffset = (0..lastIndex);
		~pan = -1;
		~instrument = if(~rate2.notNil) { \wvst1glmulti } { \wvst0multi }

	}


	// equality

	== { |that|
		^this.compareObject(that, #[\wavesetArray, \bufferArray])
	}

	hash {
		^this.instVarHash(#[\wavesetArray, \bufferArray])
	}


	*prepareSynthDefs {

		SynthDef(\wvst0multi, { | out = 0, buf = 0, startFrame = 0, numFrames = 441, rate = 1, sustain = 1, amp = 0.1, pan, interpolation = 2, busOffset = 0 |
			var phasor = Phasor.ar(0, BufRateScale.ir(buf) * rate, 0, numFrames) + startFrame;
			var env = EnvGen.ar(Env([amp, amp, 0], [sustain, 0]), doneAction: 2);
			var snd = BufRd.ar(1, buf, phasor, 1, interpolation) * env;

			OffsetOut.ar(out + busOffset, Pan2.ar(snd, pan));
		}, \ir.dup(10)).add;

		SynthDef(\wvst1glmulti, { | out = 0, buf = 0, startFrame = 0, numFrames = 441, rate = 1, rate2 = 1, sustain = 1,
			amp = 0.1, pan, interpolation = 2, busOffset = 0 |
			var rateEnv = Line.ar(rate, rate2, sustain);
			var phasor = Phasor.ar(0, BufRateScale.ir(buf) * rateEnv, 0, numFrames) + startFrame;
			var env = EnvGen.ar(Env([amp, amp, 0], [sustain, 0]), doneAction: 2);
			var snd = BufRd.ar(1, buf, phasor, 1, interpolation) * env;

			OffsetOut.ar(out + busOffset, Pan2.ar(snd, pan));
		}, \ir.dup(11)).add;

	}
}


