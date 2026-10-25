# Improving improv's music quality

Findings from reading the code at commit `08b7e56` (October 2026), ordered by how much each is likely to change what you hear. Paths are relative to `src/main/scala/` unless they start with `src/`.

The short version: four bugs mean the genetic algorithm and the rating network barely do their jobs, and the musicians never share a key or a tempo. Fixing those two things should matter more than any new feature.

## 1. Bugs that switch off the genetic algorithm and the rating network

### The rating network is scored on unscaled inputs

`training/ann/NeuralNetworks.scala:37` trains the network on `mergedTrainingData.normalised`, which min-max scales every feature to 0–1. `genetic/ANNFitnessFunction.scala:21` then feeds it raw feature vectors. Raw values sit far outside 0–1 (durations, pitches up to 127, the key code of 100–270), so the sigmoid units saturate and the network most likely gives almost every phrase the same score. The min and max used in training aren't saved with `NN.nnet`, so the scaling can't be reproduced at play time today.

Fix: save the per-feature min and max next to the network, apply the same scaling in `ANNFitnessFunction`, then retrain.

### "Similarity" is actually a distance

`genetic/SimilarityMeasure.scala:17` returns the Euclidean distance between two phrases' feature vectors. `genetic/GASelector.scala:28` multiplies fitness by `pow(similarity, 3.5)`, so selection strongly favours the phrases *least* like the musician's previous phrase. That works against musical continuity and probably explains much of the jumpiness.

Fix: turn the distance into a similarity, such as `1 / (1 + d)` or `exp(-d / scale)`, computed on scaled features (section 2). Revisit the 3.5 exponent once the value means what its name says.

### The genetic algorithm doesn't evolve

Each generation in `genetic/GASelector.scala:25` rates `candidates`, the original population, instead of `population`. Whatever one generation breeds is thrown away by the next, so 20 generations do the work of one. Separately, `breed` (line 67) crosses every pair of survivors with `combinations(2)`, so n survivors produce n(n-1)/2 children: 20 survivors give 190.

Fix: rate `population` in the loop, breed a fixed number of children from chosen parent pairs (tournament selection, say), and keep the population size constant.

### Crossover drops the first segment and splices across keys

`genetic/MusicalCaseCrossoverOperator.scala:21` loops `for (i <- 1 until minNumSegments)`, so segment 0 of both parents is never used. The child alternates fixed-length time slices (`duration / 5`) from two parents that can be in different keys and tempos, cut wherever the time falls rather than on bar lines.

Fix: start at 0, cut on bar or beat boundaries, and transpose the second parent into the first parent's key before splicing (section 3).

## 2. Phrase retrieval compares the wrong things

`storage/KDTreeIndex.scala` searches a javaml `KDTree` by plain Euclidean distance over the raw vector from `CompositePhraseFeatureExtractor`. So "nearest phrase" is decided by whichever features have the biggest numbers:

- **Nothing is scaled.** Durations and the 128-bin pitch histogram of raw counts outweigh small-range features like direction of motion.
- **The instrument is counted 19 times.** `cbr/description/features/extractors/phrase/CompositePhraseFeatureExtractor.scala:52` prepends the instrument number to each of the 19 features, so "nearest" mostly means "same instrument". The composer already filters by instrument class (`actors/musicians/AIMusician.scala:158`), so this adds nothing useful.
- **The key code has the wrong distances.** `cbr/description/features/extractors/phrase/KeyExtractor.scala:11` stores the key as one number (`quality * 100 + sharps * 10 + flats`). C major is 100 and A minor is 200, although they use exactly the same notes, while G major (110) counts as close to C major.
- **Polyphonic phrases ignore their weights.** `combine` in the same composite extractor (line 37) computes duration weights for each voice and then takes the per-feature maximum (line 46), never using them.

Fix:

- Z-score each feature over the case library, storing the mean and standard deviation with the index.
- Use the instrument once, or only as a filter before the search.
- Store the key as a 12-value pitch-class profile, or drop it once cases are stored transposed (section 3).
- Turn the pitch histogram into proportions over 12 pitch classes, plus a separate register feature.
- Then tune the feature weights by ear, or learn them.

## 3. The musicians don't share a key or a beat

Nothing in the code transposes notes, and `instruments/JFugueInstrument.scala:53` plays each phrase at its own source tempo (`phrase.tempoBPM * 4`). A musician answers with a phrase taken from whichever training song matched, in that song's key and tempo, so the parts clash and drift apart. This is probably the largest audible problem.

Fix, in order:

1. At indexing time (`training/SystemTrainer.scala`), transpose every case to C major or A minor and store the offset.
2. Let the Director choose the ensemble's key, or infer it from what was just played, and transpose each chosen phrase into it before playing.
3. Let the Director own the tempo and a bar grid: play every phrase at that tempo, trimmed or padded to whole bars.
4. Optionally, let the Director own a chord progression and reject or penalise candidates that clash with the current chord, through the `constraints` argument that `compose` already takes.

## 4. Sound

Nothing loads a soundbank, so playback through JFugue's `Player` (`actors/musicians/JFugueSynchronizedPlayer.scala:18`) uses Java's built-in General MIDI synth, which sounds thin.

- Load a good `.sf2` SoundFont into the Java synthesizer before playback, or send the MIDI to FluidSynth or a DAW instead.
- Add small random variation to note velocity and start time so repeated notes don't sound mechanical.

## 5. A bigger change: a pretrained music model

The rating network learns from ratings typed in one phrase at a time (`demos/DemoCreateTrainingDataANN.scala`), so it will always have very little data. A pretrained symbolic music model could take over its job, in one of two ways:

- **As the fitness function:** score each candidate by how likely it is as a continuation of what the other musicians just played.
- **As the generator:** keep the case library as a style memory that conditions the model.

## Housekeeping: tests that open a browser

`src/test/scala/KDTreePerformanceAnalysis.scala` and `src/test/scala/training/segmentation/LBDMSplitTimeFinderTest.scala` (line 40) plot with wisp (`com.quantifind.charts.Highcharts`). Wisp starts a local web server and opens a browser tab, so every full test run pops up charts, even with `-Djava.awt.headless=true`. The first file is a benchmark, not a test: move it out of the test sources, or tag it and exclude the tag by default. The LBDM test should check the split times without plotting.

## Checking that a change helps

Before and after each change, record 30 seconds of the same demo (`demos/DemoCBROrchestra.scala`) with the same training data, and listen to the two side by side. Do section 1 first, then section 3, then section 4: each is cheap and should be clearly audible.
