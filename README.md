# free-pascal-prototypes

This repository contains Lazarus/Free Pascal prototype projects, packages and reusable units for behavioral experiments.

## Packages

### schedules package

A design time component (TSchedules) that allow following schedules:

- FR, FI, FT
- VR, VI, VT
- DRL, DRH
- CRF, EXT

If you don't want to install the package, you can reuse the units. They live inside `src/schedules/`.

## Units

All reusable units lives in `/src/`:

``` text
 	behavior.events.pas               // reusable
	controls.stimulusgrid.pas         // reusable
	controls.stimuluskey.pas          // reusable
	extctrls.stimulus.pas             // reusable
	serialtimer.pas                   // reusable
	sessions.pas                      // not reusable
	tabdelimitedreport.custom.pas 	  // not reusable
	tabdelimitedreport.pas 	          // reusable
	timestamps.pas                    // reusable
```

## Projects

Projects lives inside `/example/projects/` folder. They are simple Lazarus applications. It is recommended to use [Lazarus](http://lazarus-ide.org/) to  compile them.  

### keep_clicking_to_keep_the_star

Given an antecedent stimulus, clicking the stimulus shows a start for some time then hide the start. While the start is visible, clicking the stimulus resets the "hide" timer.

```text
if `S` then `R` -> `C`
if `S` and `C` then reset `C` timer
```
### pavlovian_induction

Uses the serial timer (SerialTimer.pas) to present stimuli (S) with pseudo-random (Tr) and predictable (Tp) intervals from each other. We do this in such a way so that S predicts S'.
```  
S  -> R
S' -> ?

if `S` then `Tp` then `S'` then `Tr` then `S` then [...]

S  -> R
S' -> R'
```  

### control_a_moving_ball

Ilustrates how to move a ball on screen using TShape and a TTimer. Also ilustrates how to control the direction of the ball using keyboard and mouse. Also ilustrates simple colision algorithm, the ball will

### pilot_project

Ilustrates how to track mouse and keyboard events with TForm and TGraphicalControl classes. Ilustrates how to make a cummulative record with thoses events. Uses units like: TabDelimitedReport, Timestamps and ExtCtrl.Stimulus.

### simultaneous_discrimination_session

- Prototypes for an experimental "Session" with "Trials".
- Ilustrates how to create a stimulus matrix grid on screen.
- Ilustrates how to load bmp files.
- Ilustrate how to load png files with transparency. 
