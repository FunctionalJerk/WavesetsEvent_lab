

Wavesets2 {

	var <signal, <buffer;

	var	<xings, <lengths, <fracXings, <fracLengths;
	var <amps, <maxima, <minima;
	var <minSet, <maxSet, <avgLength, <sqrAvgLength;
	var <minAmp, <maxAmp, <avgAmp, <sqrAvgAmp;

	classvar <>defaultMinLength = 10; // minLength reasonable? => 4.4 kHz maxfq.
	classvar <>printStats = true; // minLength reasonable? => 4.4 kHz maxfq.

	*fromBuffer { |buffer, onComplete, minLength, startFrame, endFrame|
		^this.new.fromBuffer(buffer, onComplete, minLength, startFrame, endFrame)
	}

	fromBuffer { |argBuffer, onComplete, minLength, startFrame, endFrame|
		buffer = argBuffer;
		startFrame = (startFrame ? 0).wrap(0,argBuffer.numFrames);
		endFrame = (endFrame ? argBuffer.numFrames-1).wrap(0,argBuffer.numFrames);

		if(startFrame > endFrame){
			buffer.loadToFloatArray(startFrame, buffer.numFrames - startFrame, { |array|
				buffer.loadToFloatArray(0, endFrame, { |brray|
					array = array ++ brray;
					this.setSignal(array, minLength, buffer, startFrame, endFrame);
					onComplete.value(this);
				})
			});
		} {
			buffer.loadToFloatArray(startFrame, endFrame - startFrame, { |array|
				this.setSignal(array, minLength, buffer, startFrame, endFrame);
				onComplete.value(this);
			});
		}
	}

	toBuffer { |buffer, onComplete|
		buffer.loadCollection(signal.asArray, action: onComplete);
	}

	asBuffer { |server, onComplete|
		^Buffer.loadCollection(server, signal.asArray, numChannels: 1, action: onComplete)
	}

	signal_ { |sig|
		signal = sig;
		this.analyse;
	}

	setSignal { |sig, minLength, argBuffer, startFrame, endFrame|
		signal = sig;
		buffer = argBuffer;
		this.analyse(minLength,startFrame,endFrame);
	}

	numFrames { ^signal.size }
	numXings { ^xings.size }
	size { ^xings.size }

	frameFor { |startWs, numWs = 1, useFrac = true|

		var theseXings = if (useFrac) { fracXings } { xings };
		var startFrame = theseXings.clipAt(startWs);
		var endFrame = theseXings.clipAt(startWs + numWs);
		var length = absdif(endFrame, startFrame);
		var sustain = length / buffer.sampleRate;

		^[startFrame, length, length/buffer.sampleRate]
	}

	maximumAmp { |index, length=1|
		var maxItem = 0;
		if(length == 1) { ^amps[index] ? 0.0 };
		length.do { |i| maxItem = max(maxItem, amps[index + i] ? 0.0) };
		^maxItem
	}
	// backwards compatibility
	ampFor { |index, length=1|
		^this.maximumAmp(index, length)
	}

	plot { |index = 0, length = 1, sampleRate|
		var data = this.frameFor(index, length, false).postln;
		var segment = signal.copyRange(data[0], data[0] + data[1] - 1);
		var peak = max(segment.maxItem, segment.minItem.abs);
		var sustain = (data[1] - data[0] / (sampleRate ? Server.default.sampleRate ? 44100)).round(0.000001);
		segment.plot(
			"Waveset: index %, length %, sustain %".format(index, length, sustain),
			minval: peak.neg,
			maxval: peak
		)
	}

	// the interesting bit

	analyse { |minLength, startFrame, endFrame|
		//	var chunkSize = 400, pause = 0.01;	// not used yet
		xings = Array.new;
		amps = Array.new;
		lengths = Array.new;
		fracXings = Array.new;
		maxima = Array.new; 	// indices of possible turnaround points
		minima = Array.new; 	//

		if(printStats) {
			"%: Analysing ...".format(this.class).inform;
			"StartFrame: % EndFrame: %".format(*[startFrame,endFrame].asInteger).inform;
		};
		this.analyseFromTo(startFrame: startFrame, endFrame: endFrame, minLength: minLength);
		this.calcAverages;
		if(printStats) {
			"\t ... done. (% xings)".format(xings.size).inform;
		}
	}

	analyseFromTo { |startFrame = 0, endFrame, minLength|

		var lengthCount = 0, prevSample = 0.0;
		var maxSamp = 0.0, minSamp = 0.0;
		var maxAmpIndex, minAmpIndex, wavesetAmp, frac;

		minLength = minLength ? defaultMinLength; // minLength reasonable? => 4.4 kHz maxFreq.

		// find xings, store indices, lengths, and amps.

		startFrame = max(0, startFrame).asInteger;
		endFrame = (endFrame ? signal.size - 1).min(signal.size - 1).asInteger;

		signal.size.do { |x,i|

			var thisSample;
			thisSample = signal.at(i);
			// if Xing from non-positive to positive:
			if(
				(prevSample <= 0.0) and:
				{ thisSample  > 0.0 } and:
				{ lengthCount >= minLength }
			) {

				if(xings.notEmpty) {
					// if we already have a first waveset,
					// keep results from analysis:
					wavesetAmp = max(maxSamp, minSamp.abs);
					amps = amps.add(wavesetAmp);
					lengths = lengths.add(lengthCount);
					maxima = maxima.add(maxAmpIndex);
					minima = minima.add(minAmpIndex);
				};

				xings = xings.add(i + startFrame);

				// lin interpol for fractional crossings
				frac = prevSample / (prevSample - thisSample);
				fracXings = fracXings.add(i - 1 + frac + startFrame);

				// reset vars for next waveset
				maxSamp = 0.0;
				minSamp = 0.0;
				lengthCount = 0;
			};

			lengthCount = lengthCount + 1;
			if(thisSample > maxSamp) { maxSamp = thisSample; maxAmpIndex = i + startFrame };
			if(thisSample < minSamp) { minSamp = thisSample; minAmpIndex = i + startFrame };
			prevSample = thisSample;
		}

	}

	// basic statistics
	calcAverages {
		// calculate maxAmp, minAmp, avgAmp, sqAvgAmp;
		// and maxSet, minSet, avgLength, sqAvgLength;

		var numXings = xings.size;

		minSet = lengths.minItem;
		maxSet = lengths.maxItem;
		minAmp = amps.minItem;
		maxAmp = amps.maxItem;

		if(numXings > 0) {
			fracLengths = fracXings.drop(1) - fracXings.drop(-1);

			avgLength = xings.last - xings.first / numXings;
			sqrAvgLength = (lengths.squared.sum / ( numXings - 1)).sqrt;

			avgAmp = amps.sum / numXings;
			sqrAvgAmp = (amps.squared.sum / numXings).sqrt;
		}
	}

	// peak detection on amplitudes
	// http://www.tcs-trddc.com/trddc_website/pdf/SRL/Palshikar_SAPDTS_2009.pdf

	peakAmpIdxs { |windowSize = 4, h = 3, s1|
		// 1 <= h <= 3

		var mean, sDev, aAbs, peaksSize;
		var rWindowSize = windowSize.reciprocal;
		var peakIdxs; //return value
		var idxs = List[];
		var a, hScaled;

		// peak estimation function
		if(s1.isNil) {
			s1 = {|left, item, right, rContextSize|
				((maxItem(item - left) ? 0) + (maxItem(item - right) ? 0)) * 0.5
			};
		};


		a = amps.collect { |val, i|
			// compute peak function value for each of the N points in T
			s1.value(
				amps.copyRange(i-windowSize, i-1),
				val,
				amps.copyRange(i+1, i + windowSize),
				val
			);
		};
		// Compute mean, standard deviation of all positive values
		aAbs = a.abs;
		mean = aAbs.mean;
		sDev = aAbs.stdDev;
		hScaled = h * sDev;

		a.do { |val, i| // collect all indices that are concidered big
			if ((a[i] > 0) and: { (a[i]-mean) > hScaled }) { idxs.add(i) }
		};

		peaksSize = idxs.size;

		// retain only one peak of any set of peaks within windowSize of each other
		peakIdxs = idxs.inject([0], { |last, now, i|
			if((now - last.last) <= windowSize) {
				if(amps[now] > amps[last.last]) {
					last.pop;
					last ++ now
				} {
					last
				}
			} {
				last ++ now
			}
		});

		^peakIdxs
	}

	nextCrossingIndex { |frame, useFrac = true|
		var theseXings = if (useFrac) { fracXings } { xings };
		^theseXings.indexOfGreaterThan(frame) ?? { theseXings.lastIndex }
	}

	nextCrossing { |frame, useFrac = true|
		var theseXings = if (useFrac) { fracXings } { xings };
		^theseXings[theseXings.indexOfGreaterThan(frame)]  ?? { theseXings.last }
	}

	prevCrossing { |frame, useFrac = true|
		var theseXings = if (useFrac) { fracXings } { xings };
		var index = theseXings.indexOfGreaterThan(frame);
		^if(index.notNil) { theseXings.clipAt(index - 1) }

	}

	nextStartCrossingIndex { |frame, useFrac = true|
		var theseXings = if (useFrac) { fracXings } { xings };
		var index = theseXings.indexOfGreaterThan(frame);
		var lastIndex = theseXings.lastIndex - 1;
		^if(index.notNil) { min(index, lastIndex) } { lastIndex }
	}


	// equality

	== { |that|
		^this.compareObject(that, #[\signal])
	}

	hash {
		^this.instVarHash(#[\signal])
	}


}



