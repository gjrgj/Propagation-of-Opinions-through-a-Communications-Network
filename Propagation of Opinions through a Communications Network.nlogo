;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setup Procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

breed [ network-users network-user ] ;; network users
breed [ hubs hub ] ;; information hubs for the propagation of information
undirected-link-breed [ hublinks hublink ] ;; links between only hubs
undirected-link-breed [ userlinks userlink ] ;; links between hubs and network users
turtles-own [
  opinion ;; opinion value of a network-user or hub, defaults to zero
  next-opinion
  ;; Value of next opinion, necessary to propagate information consistently and evenly through network.
  ;; The way that NetLogo completes an action called on a set of agents is by iterating along the set and calling
  ;; it on them one by one. So, depending on where you start updating opinions in the network, a hub on the
  ;; opposite end could be immensely different from what it started at one tick before instead of only marginally different.
  ;; For example, say there are 3 hubs in a line (5 - 5 - 5), all with an opinion value of 5. During a tick where they 
  ;; 'pass' information to one another, what SHOULD happen is that each hub 'gains' an opinion value of some
  ;; proportion of 5, in this case a 1:1 ratio (10 - 10 - 10). But if we update opinion values before the tick has passed,
  ;; what can happen is that either of the end hubs pass their information first to the middle hub, updating it to 10, then
  ;; that middle hub passes its new information value to the next furthest hub, incrementing it by 10 instead of 5. The effect
  ;; is magnified exponentially as a network gains more hubs. That method of spreading opinion is too inconsistent and volatile.
]

network-users-own [
  fame ;; Quantitatively models societal influence of an individual user (i.e. famous doctors have
       ;; more weight attached to what they say compared to regular people. Fame is shown on the map
       ;; through a yellow glow diffused around network-users.
  initial-fame ;; fame of a user when the simulation is first initialized, used when restting fame 
]

hubs-own [
  max-size ;; maximum size of hub, calculated by multiplying the hub's current size by opinion_magnitude
]

patches-own [
  visual-fame ;; used to show fame visually, valued derived from the fame of the network-user above the patch
]

links-own [ 
  opinion-flow ;; the amount of opinion that has passed through a link in a given tick
] 

globals [
  beginning-opinion ;; the initial opinion value incited by some event, to be placed either on an agent or hub
  startpoint ;; the beginning point of opinion propagation
  total-size ;; the total size of every node in the system
  opinion_magnitude ;; magnitude of maximum or minimum opinion
  max-fame ;; maximum fame value
  current-pen ;; current pen we ar eplotting with
  propagation-rate ;; ranges from 0 to 1 based on how intense the beginning event was
  wrap-around ;; stores whether or not the graph has 'wrapped around' yet in terms of graphing trials
  event-added? ;; stores whether or not an event has been added to the simulation
  trial-ended? ;; stores whether the latest trial has ended
  previous-average-opinion ;; previous average opinion, used to calculate fastest rate of change
  
  ;; simulation output variables
  most-intense-overall-opinion ;; highest magnitude overall opinion (highest absolute value)
  time-to-reach-neutral-from-start ;; number of ticks required for the simulation to reach a neutral overall opinion from start
  fastest-rate-of-change ;; fastest rate of change of average opinion
  tick-of-fastest-change ;; tick where that change occurs
]

;; prints text output in box under graph
to print-results
  output-type "Highest magnitude opinion: " output-print (precision most-intense-overall-opinion 4) ;; round to 4 decimal places
  output-type "Time to reach neutral from opinion insertion: " output-type time-to-reach-neutral-from-start output-print " ticks"
  output-type "Fastest rate of change: " output-type (precision fastest-rate-of-change 4) output-type " avg opinion/tick at tick " output-print tick-of-fastest-change
end

;; set up simulation and network, do not propagate opinions yet
to setup
  clear-all
  display
  set-default-shape hubs "circle"
  set-default-shape network-users "square"
  
  ;; initialize globals
  set propagation-rate 0
  set optimize-layout? true
  set visuals? false
  set opinion_magnitude 1000
  set scaling-factor 1
  set opinion-intensity .75
  set max-fame 50
  set show-fame? true
  set fame-intensity 1
  set current-pen 1
  set-current-plot-pen "trial 1"
  set wrap-around false
  set trial-ended? false
  set event-added? false
  set most-intense-overall-opinion 0
  set time-to-reach-neutral-from-start 0
  set previous-average-opinion 0
  set tick-of-fastest-change 0
  
  ;; make the initial network of two hubs and an edge, then complete the network
  make-hub nobody       ;; first node, unattached
  make-hub hub 0        ;; second node, attached to first node
  complete-network
end


to go
  set optimize-layout? false ;; stop optimizing layout once a simulation begins in order to speed it up
  ;; overwrite pen only when starting a new trial
  set previous-average-opinion average-opinion ;; update previous average opinion before each tick of propagation
  if (trial-ended? = true) [ 
    set-pen 
    set trial-ended? false
  ]
  ;; check plot to see if current amount of displayed lines matches what the user has currently selected as the 'trials-to-show'
  let trial-checked 10
  while [trial-checked > trials-to-show] [
    translate-pen-number (trial-checked)
    plot-pen-reset
    set trial-checked (trial-checked - 1)
  ]
  translate-pen-number (current-pen) ;; return pen to what it was before the check
  propagate-opinions
  update-visuals
  tick
  let average average-opinion ;; use local variable here so NetLogo only has to average opinions once for the next few boolean statements
  ;; if current average opinion has a higher magnitude than the stored value of most-intense-overall-opinion, set most-intense-overall-opinion to that value
  if (abs average > abs most-intense-overall-opinion) [
    set most-intense-overall-opinion average
  ]
  if (abs (average - previous-average-opinion) > abs fastest-rate-of-change) [
    set fastest-rate-of-change (average - previous-average-opinion) ;; check for change in fastest rate of change (since the values checked will always be 1 tick away from one another, no need to divide by tick separation
    set tick-of-fastest-change ticks ;; set tick-of-fastest-change to the current tick value
  ]
end

;; runs simulation by one tick
to go-once
  go
  if (simulation-over?) [ 
    reset-simulation
    stop ;; stop if magnitude of average opinion reaches virtually zero
  ] 
end

;; runs simulation by ten ticks
to go-ten 
  repeat 10 [
    go
    if (simulation-over?) [ 
      reset-simulation
      stop ;; stop if magnitude of average opinion reaches virtually zero
    ] 
  ]
end

;; sets current plot pen   
to set-pen
  set current-pen current-pen + 1
  ;; wrap around to first pen if just used last one, last pen user-defined by the 'trials-to-show' slider
  if (current-pen > trials-to-show) [ 
    set wrap-around true
    set current-pen 1
  ]
  translate-pen-number (current-pen)
  if (wrap-around) [ plot-pen-reset ]
end

;; sets actual plot pen based on number input
to translate-pen-number[pen-number]
  if (pen-number = 1) [
    set-current-plot-pen "trial 1"
  ]
  if (pen-number = 2) [
    set-current-plot-pen "trial 2"
  ]
  if (pen-number = 3) [
    set-current-plot-pen "trial 3"
  ]
  if (pen-number = 4) [
    set-current-plot-pen "trial 4"
  ]
  if (pen-number = 5) [
    set-current-plot-pen "trial 5"
  ]
  if (pen-number = 6) [
    set-current-plot-pen "trial 6"
  ]
  if (pen-number = 7) [
    set-current-plot-pen "trial 7"
  ]
  if (pen-number = 8) [
    set-current-plot-pen "trial 8"
  ]
  if (pen-number = 9) [
    set-current-plot-pen "trial 9"
  ]
  if (pen-number = 10) [
    set-current-plot-pen "trial 10"
  ]
end

;; procedure taken at end of every simulation to reset
to reset-simulation
  set time-to-reach-neutral-from-start ticks ;; set time taken to final ticks value
  print-results
  set trial-ended? true
  set propagation-rate 0 ;; reset propagation rate
  set event-added? false ;; reset event-added 
  ;; use new pen for every trial, if trial count reaches specified maximum, wrap around and reuse first pen
  plot-pen-up
  reset-opinions ;; ensure opinion resets to 0 after each trial
  reset-ticks ;; clear ticks for next trial
  set time-to-reach-neutral-from-start 0 ;; reset recorded time for next trial
  set most-intense-overall-opinion 0 ;; reset most-intense-overall-opinion
  set fastest-rate-of-change 0 ;; reset fastest rate of change
  set tick-of-fastest-change 0 ;; reset tick of fastest change
  set optimize-layout? true ;; optimize layout again, since the simulaiton is over it won't slow it down
end

;;;;;;;;;;;;;;;;;;;;;;;
;;; Main Procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;

;; compile all methods into one to run the simulation all the way through and display stats
to complete-network
  repeat (number-hubs - 2) [
    ;; new edge is green, old edges are gray
    ask links [ set color gray ]
    make-hub found-hub         ;; find partner & use it as attachment point for new node
    ;; 
    if optimize-layout? [ layout ]
    if resize-hubs? [ resize ]
    if show-opinions? [ opinions ]
  ]
  ;; Add users after hubs are already made, number of users connected to each hub is randomly dependent on hub size
  ;; So big hubs generally get much bigger, and small ones only get slightly larger. 
  repeat (number-hubs * user-to-hub-ratio) [
  ;; new edge is green, old edges are gray
    ask links [ set color gray ]
    make-user found-hub         ;; find hub & use it as attachment point for new user
    ;;
    if optimize-layout? [ layout ]
    if resize-hubs? [ resize ]
    if show-opinions? [ opinions ]
    if show-fame? [ show-fame ]
  ]
  reset-ticks ; reset ticks to begin simulation at time 0
end

;; compiles all visual factors into one looping button
to update-visuals
  set visuals? true
  resize
  opinions
  show-fame
  layout
end

;; used for creating a new hub
to make-hub [old-hub]
  create-hubs 1
  [
    set color white
    if old-hub != nobody
      [ 
        create-hublink-with old-hub  
        ;; position the new hub near its partner
        move-to old-hub
        fd 10
      ]
  ]
end

;; used for adding network-users to the network
to make-user [current-hub]
  create-network-users 1
  [
    set fame (max-fame / 10 + random-float (1)) ;; 'normal people' are considered to be about 1/10 as popular as the value of max-fame
    set initial-fame fame
    set color white
    create-userlink-with current-hub  
    ;; position the new user near its hub
    move-to current-hub
    fd 10
  ]
end

;; This code is the heart of the "preferential attachment" mechanism, and acts like
;; a lottery where each hub gets a ticket for every connection it already has.
;; While the basic idea is the same as in the Lottery Example (in the Code Examples
;; section of the Models Library), things are made simpler here by the fact that we
;; can just use the links as if they were the "tickets": we first pick a random hublink,
;; and than we pick one of the two ends of that link. 
to-report found-hub
    report [one-of both-ends] of one-of hublinks
end

;;;;;;;;;;;;;;;;;;
;;;; Opinions ;;;;
;;;;;;;;;;;;;;;;;;


;; reports the current average opinion, used to graph points on the plot
;; formula used is based on weighted averaging
;; overall average opinion = ((sum of all hub opinions) / (sum of all (hub sizes * opinion_magnitude))) * ((sum of all hub opinions) / (sum of all turtle opinions)) + 
;; ((sum of all user opinions) / (user count * opinion_magnitude)) * ((sum of all user opinions) / (sum of all turtle opinions))

to-report average-opinion
  if (event-added? = false) [
    if (ticks = 0) [
      report 0 ;; if no simulation has started, report 0 (because no opinion has propagated)
      ] 
  ]
  ;; take a weighted average of hub and user opinions based on size
  let total-opinion 0 ;; adding up all turtle opinions
  let total-hub-opinion 0 ;; adding up all hub opinions
  let total-user-opinion 0 ;; adding up all network-user opinions
  let accumulated-size 0 ;; adding up all hub sizes
  let overall-opinion 0
  ask turtles [
    ifelse is-hub? self [
      set total-hub-opinion (total-hub-opinion + opinion)
      set accumulated-size (accumulated-size + size)
    ]
    [
      set total-user-opinion (total-user-opinion + opinion)
    ]
    set total-opinion (total-opinion + opinion)
  ]
  set overall-opinion ((total-hub-opinion / (opinion_magnitude * accumulated-size)) * (total-hub-opinion / total-opinion) + (total-user-opinion / (count network-users * opinion_magnitude)) * (total-user-opinion / total-opinion))
  report overall-opinion
end

;; checks if simulation is over
to-report simulation-over?
  ;; initialized as true,  changes to false if any node's opinion value is greater than .1% of its maximum
  let sim-over true
  ask turtles [
    ifelse is-network-user? self [
      if abs opinion > (opinion_magnitude * .001) [
        set sim-over false
      ]
    ]
    ;; hub opinions are calculated based on size
    [
      if abs opinion > (opinion_magnitude * size * .001) [
        set sim-over false
      ]
    ]
  ]
  report sim-over
end
  
;; propagate each hub/network-user opinion to its neighbors
to propagate-opinions
  ;; set of all hubs
  ask hubs [
    let temp-opinion opinion
    let temp-size size ;; so the next command uses the central node's size value rather than that of the surrounding nodes
    ;; set of all hubs and users connected to the current node
    ask link-neighbors [
      ;; impact of size is relative to mean size of system
      set next-opinion (next-opinion + (temp-opinion * (temp-size / (mean [size] of hubs * count hubs))))
    ]
  ]
  ;; set of all network-users
  ask network-users [   
    let temp-opinion opinion
    let temp-fame fame ;; so the next command uses the central node's fame value rather than that of the surrounding nodes
    ;; set of all hubs and users connected to the current node
    ask link-neighbors [
      ;; impact of fame is relative to mean fame of system
      set next-opinion (next-opinion + (temp-opinion  * (temp-fame / (mean [fame] of network-users * count network-users))))      
    ]
  ]
  ;; set new opinion values
  set-opinions
end   
  
;; set new opinion values
to set-opinions
  ;; set new opinion of network-users
  ask network-users [
    ;; propagation rate is factored in relative to intensity of the event
    set opinion (next-opinion * propagation-rate + opinion)
    set next-opinion 0 ;; reset next-opinion for next tick of propagation
    ;; apply the element of forgetfulness, as more time passes people forget more of what happened
    ;; x^2 function is arbitrary as well, for this simulation to be relevant we would need to run tests on real data sets to model the spread of information accurately
    set opinion (opinion / ((forgetfulness * ticks) ^ 2 + 1))
    ;; if the magnitude of the opinion is larger than the opinion_magnitude (or less than the negative of said value), return it to that value.
    if opinion > opinion_magnitude [ 
      set opinion opinion_magnitude
    ]
    if opinion < (- opinion_magnitude) [
      set opinion (- opinion_magnitude)
    ]
  ]
  
  ask hubs [
    set opinion (next-opinion * propagation-rate + opinion)
    set next-opinion 0 ;; reset next-opinion for next tick of propagation
    ;; apply the element of forgetfulness, as more time passes people forget more of what happened
    ;; x^2 function is arbitrary as well, for this simulation to be relevant we would need to run tests on real data sets to model the spread of information accurately
    set opinion (opinion / ((forgetfulness * ticks) ^ 2 + 1))
    ;; if the magnitude of the opinion is larger than the opinion_magnitude (or less than the negative of said value), return it to that value.
    if opinion > opinion_magnitude * size [ 
      set opinion opinion_magnitude * size
    ]
    if opinion < (- opinion_magnitude * size) [
      set opinion (- opinion_magnitude * size)
    ]
  ]
end

;; sets beginning-opinion and propagation rate based on what type of event is selected
to type-event
  ;; propagation rates are arbitrary and are just meant to show how different intensity events result in different propagation speeds (in general)
  ;; if multiple events are inputted, set the propagation rate of the whole simulation equal to whatever the most intense event is (for example,
  ;; if both 'death' and 'minor positive effects' are inputted, then the propagation rate will default to that of 'death' which is equal to 1.
  ;; first, set what the initial opinion will be based on what type of event occurs
  if type-of-event = "Death" [
    set beginning-opinion (- opinion_magnitude) ; set to max magnitude negative opinion
    if (propagation-rate < 1) [ set propagation-rate 1 ]
  ]
  
  if type-of-event = "Major negative side effects" [
    set beginning-opinion (- 2 * opinion_magnitude / 3) ; set to 2/3 max magnitude negative opinion
    if (propagation-rate < 1 / 2) [ set propagation-rate 1 / 2 ]
  ]
  
  if type-of-event = "Minor negative side effects" [
    set beginning-opinion (- opinion_magnitude / 3) ; set to 1/3 max magnitude negative opinion
    if (propagation-rate < 1 / 4) [ set propagation-rate 1 / 4 ]
  ]
  
  if type-of-event = "Nothing happened" [
    set beginning-opinion (0) ; set to 0
    set propagation-rate 0
  ]
  
  if type-of-event = "Minor positive effects" [
    set beginning-opinion (opinion_magnitude / 3) ; set to 1/3 max magnitude positive opinion
    if (propagation-rate < 1 / 4) [ set propagation-rate 1 / 4 ]
  ]
  
  if type-of-event = "Major positive effects" [
    set beginning-opinion (2 * opinion_magnitude / 3) ; set to 2/3 max magnitude positive opinion
    if (propagation-rate < 1 / 2) [ set propagation-rate 1 / 2 ]
  ]
  
  if type-of-event = "Condition fully treated" [
    set beginning-opinion (opinion_magnitude) ; set to max magnitude positive opinion
    if (propagation-rate < 1) [ set propagation-rate 1 ] 
  ] 
end
;
; Implements the user's choice of type of event, each situation correlates with a different level of opinion and speed of travel
to news-event 
  ;; then, set where the event begins (what size hub or what fame agent)
  if event-starting-point = "Normal network user" [
    set startpoint one-of network-users with [(abs (fame - min [fame] of network-users)) < (1.25 * standard-deviation [fame] of network-users)] ; selects user with fame within one standard deviation of minimum
  ]
  if event-starting-point = "Moderately famous network user" [
    set startpoint one-of network-users with [(abs (fame - mean [fame] of network-users)) < (1.25 * (standard-deviation [fame] of network-users))] ; selects user with fame within 1.25 standard deviations of mean
  ]
  if event-starting-point = "Very famous network user" [
    set startpoint one-of network-users with [(abs (fame - max [fame] of network-users)) < (1.25 * standard-deviation [fame] of network-users)] ; selects user with fame within one standard deviation of max
  ]
  if event-starting-point = "Most famous network user" [
    set startpoint max-one-of network-users [fame] ; selects most famous user
  ]
  if event-starting-point = "Small information hub" [
     set startpoint one-of hubs with [(abs (size - min [size] of hubs)) < (1.25 * standard-deviation [size] of hubs)] ; selects hub within one standard deviation of minimum
  ]
  if event-starting-point = "Medium information hub" [
     set startpoint one-of hubs with [(abs (size - mean [size] of hubs)) < (1.25 * (standard-deviation [size] of hubs))] ; selects hub with size within 1.25 standard deviations of mean
  ]
  if event-starting-point = "Large information hub" [
     set startpoint one-of hubs with [(abs (size - max [size] of hubs)) < (1.25 * standard-deviation [size] of hubs)] ; selects hub with size within one standard deviation of max
  ]
  if event-starting-point = "Largest information hub" [
     set startpoint max-one-of hubs [size] ; selects largest hub
  ]
end

;; inserts a random event into the simulation
to insert-event
  type-event
  news-event
  ask startpoint [
    ;; if hub, factor in size to magnitude of beginning opinion
    ifelse (is-hub? self)
    [ set opinion (beginning-opinion * size) ]
    [ set opinion beginning-opinion ]
  ]
  set event-added? true
end

;; inserts an event at a specific node, sets startpoint equal to that user/hub
to insert-event-at-point
 if mouse-down? [    
    if count turtles-on patch mouse-xcor mouse-ycor > 0 [
      type-event
      ;; prefer hubs because agents can be stacked on each other, creating issues with selection
      ifelse (count hubs-on patch mouse-xcor mouse-ycor > 0) [
        set startpoint one-of hubs-on patch mouse-xcor mouse-ycor
      ]
      [
        set startpoint one-of turtles-on patch mouse-xcor mouse-ycor
      ]
      ask startpoint [
        ifelse (is-hub? self)
        [ set opinion (beginning-opinion * size) ]
        [ set opinion beginning-opinion ]
      ]
      set event-added? true
      stop ;; stops input after one insertion to prevent tick counter from increasing constantly if mouse is held down
    ]   
  ]
end

;; sets custom fame of a user
to set-fame-of-user
   if mouse-down? [    
    if count network-users-on patch mouse-xcor mouse-ycor > 0 [
      type-event
      ;; only network-users have fame values
      set startpoint one-of network-users-on patch mouse-xcor mouse-ycor
      ask startpoint [
        set fame user-fame
      ]
      stop ;; stops input after one insertion to prevent tick counter from increasing constantly if mouse is held down
    ]   
  ]
end

;; recalculates fame based on how the simulation is initialized, allows for quick testing 
to reset-fame
  ask network-users [
    set fame initial-fame
  ]
end

;; resets all opinion values to 0, back to neutral
to reset-opinions 
  ask turtles [
    set opinion 0
  ]
  set event-added? false ;; reset event-added?
end

;;;;;;;;;;;;;;;
;;; Visuals ;;;
;;;;;;;;;;;;;;;

;; resize hubs, change back and forth from size based on degree to a size of 1
to resize
  ;; if resize-hubs? is on, change size of hubs
  ifelse resize-hubs?
  [
    ;; a node is a circle with diameter determined by
    ;; the SIZE variable; using SQRT makes the circle's
    ;; area proportional to its degree
    ask hubs [ set size scaling-factor * (sqrt count link-neighbors) ]
  ]
  [
    ask hubs [ set size 1 ]
  ]
end

;; shows opinion of each node by changing the color on a spectrum from red --> white --> green 
;; (corresponds to bad --> neutral --> good)
;; if the user wishes to not show opinions, nodes remain white
to opinions
  ifelse show-opinions?
  [
    ask hubs [ 
      ;; If opinion is less than 0, hub is red. 
      ifelse opinion < 0 
      [
        set color scale-color red (abs (size * opinion * opinion-intensity)) (opinion_magnitude * size) 0
      ]
      [
        ;; If opinion is greater than 0, hub is green. 
        ifelse opinion > 0
        [
          set color scale-color lime (size * opinion * opinion-intensity) (opinion_magnitude * size) 0
        ]
        [
          ;; If opinion is equal to 0, hub is white.
          set color white
        ]
      ]
    ]
    
    ;; show network user opinion, different from hubs because hubs have a different max/min opinion
    ask network-users [
      ;; If opinion is less than 0, hub is red. 
      ifelse opinion < 0 
      [
        set color scale-color red (abs opinion * opinion-intensity) opinion_magnitude 0
      ]
      [
        ;; If opinion is greater than 0, hub is green. 
        ifelse opinion > 0
        [
          set color scale-color lime (opinion * opinion-intensity) opinion_magnitude 0
        ]
        [
          ;; If opinion is equal to 0, hub is white.
          set color white
        ]
      ]
    ]
  ]    
  [
    ask turtles [ set color white ]
  ]
end

to show-fame
  ifelse show-fame? [
    ;; first reset visual-fame values to prevent current visuals from slowly growing in size
    ask patches [
      set visual-fame 0
    ]
    ask network-users [ ;; ask all network-users
      let temp-fame fame ;; get current fame value
      ask patch-here [ 
        set visual-fame temp-fame
      ]
    ]
    ;; diffuse fame values around users based on current fame-intensity
    diffuse visual-fame fame-intensity 
    ask patches [ set pcolor scale-color yellow visual-fame 0 max-fame ]
  ]
  [
    ask patches [ set pcolor black ] ; not showing fame, default to black
  ]
end
  
    
;; switch to show opinions or not
to show-opinions
  set show-opinions? not show-opinions?
end

;; switch to resize hubs or not
to resize-hubs
  set resize-hubs? not resize-hubs?
end

;; switch to optimize layout or not
to optimize-layout
  set optimize-layout? not optimize-layout?
end

;; organizes layout of hubs and network-users in an easier to see fashion
to layout
  if optimize-layout? [
    ;; layout-speed here is arbitrary; more repetitions slows down the
    ;; model, but too few gives poor layouts
    repeat layout-speed [  
      ;; numbers here are arbitrarily chosen for pleasing appearance
      layout-spring turtles links (1 / node-closeness) (7 / node-closeness) (1 / node-closeness)
      ifelse visuals? 
      [ display ] ;; for smooth animation
      [ no-display ]
    ]
    ;; don't bump the edges of the world
    let x-offset max [xcor] of turtles + min [xcor] of turtles
    let y-offset max [ycor] of turtles + min [ycor] of turtles
    ;; big jumps look funny, so only adjust a little each time
    set x-offset limit-magnitude x-offset .2
    set y-offset limit-magnitude y-offset .2
    ask turtles [ setxy (xcor - x-offset / 2) (ycor - y-offset / 2) ]
  ]
end

to-report limit-magnitude [number limit]
  if number > limit [ report limit ]
  if number < (- limit) [ report (- limit) ]
  report number
end


; Copyright 2005 Uri Wilensky.
; See Info tab for full copyright and license.
@#$#@#$#@
GRAPHICS-WINDOW
318
10
843
486
51
44
5.0
1
10
1
1
1
0
1
1
1
-51
51
-44
44
0
0
1
ticks
60.0

BUTTON
16
39
159
72
setup simulation
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
16
403
160
436
optimize-layout?
optimize-layout?
0
1
-1000

INPUTBOX
160
303
252
363
number-hubs
20
1
0
Number

SLIDER
160
403
308
436
layout-speed
layout-speed
1
75
25
1
1
NIL
HORIZONTAL

SLIDER
160
502
308
535
scaling-factor
scaling-factor
.5
1.5
1
.01
1
NIL
HORIZONTAL

BUTTON
16
72
159
105
update visuals
update-visuals
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
16
436
160
469
show-opinions?
show-opinions?
0
1
-1000

SWITCH
16
502
160
535
resize-hubs?
resize-hubs?
0
1
-1000

SLIDER
160
436
308
469
opinion-intensity
opinion-intensity
0
1
0.75
.01
1
NIL
HORIZONTAL

MONITOR
252
303
308
348
# nodes
count turtles
3
1
11

CHOOSER
16
270
160
315
type-of-event
type-of-event
"Death" "Major negative side effects" "Minor negative side effects" "Nothing happened" "Minor positive effects" "Major positive effects" "Condition fully treated"
0

TEXTBOX
20
371
209
396
Visual effect settings
18
0.0
1

TEXTBOX
86
208
270
235
Simulation settings
18
0.0
1

TEXTBOX
104
10
225
35
Control panel
18
0.0
1

CHOOSER
16
314
160
359
event-starting-point
event-starting-point
"Normal network user" "Moderately famous network user" "Very famous network user" "Most famous network user" "Small information hub" "Medium information hub" "Large information hub" "Largest information hub"
7

BUTTON
16
105
159
138
insert random event
insert-event
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

SLIDER
16
237
160
270
user-to-hub-ratio
user-to-hub-ratio
1
10
4
1
1
NIL
HORIZONTAL

PLOT
853
10
1313
304
Average opinion over time
time (ticks)
average opinion (proportion)
0.0
100.0
-1.0
1.0
true
true
"" "if (mean [opinion] of turtles = 0) [stop]"
PENS
"trial 1" 1.0 0 -2674135 true "plotxy 0 0" "if (trials-to-show >= 1)\n[\nif (current-pen = 1) \n[\n  plot average-opinion\n]\n]"
"trial 2" 1.0 0 -13840069 true "plotxy 0 0" "if (trials-to-show >= 2)\n[\nif (current-pen = 2) \n[\n  plot average-opinion\n]\n]"
"trial 3" 1.0 0 -13345367 true "plotxy 0 0" "if (trials-to-show >= 3)\n[\nif (current-pen = 3) \n[\n  plot average-opinion\n]\n]"
"trial 4" 1.0 0 -955883 true "plotxy 0 0" "if (trials-to-show >= 4)\n[\nif (current-pen = 4) \n[\n  plot average-opinion\n]\n]"
"trial 5" 1.0 0 -5825686 true "plotxy 0 0" "if (trials-to-show >= 5)\n[\nif (current-pen = 5) \n[\n  plot average-opinion\n]\n]"
"trial 6" 1.0 0 -7500403 true "plotxy 0 0" "if (trials-to-show >= 6)\n[\nif (current-pen = 6) \n[\n  plot average-opinion\n]\n]"
"trial 7" 1.0 0 -6459832 true "" "if (trials-to-show >= 7)\n[\nif (current-pen = 7) \n[\n  plot average-opinion\n]\n]"
"trial 8" 1.0 0 -1184463 true "" "if (trials-to-show >= 8)\n[\nif (current-pen = 8) \n[\n  plot average-opinion\n]\n]"
"trial 9" 1.0 0 -10899396 true "" "if (trials-to-show >= 9)\n[\nif (current-pen = 9) \n[\n  plot average-opinion\n]\n]"
"trial 10" 1.0 0 -14835848 true "" "if (trials-to-show >= 10)\n[\nif (current-pen = 10) \n[\n  plot average-opinion\n]\n]"

BUTTON
16
138
159
171
insert event at point
insert-event-at-point
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
159
39
308
72
run simulation
go-once
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
159
72
308
105
increment by one tick
go-once
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

SLIDER
160
237
308
270
forgetfulness
forgetfulness
.001
.01
0.0010
.001
1
NIL
HORIZONTAL

BUTTON
441
486
534
519
most famous user
watch max-one-of network-users [fame]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
527
519
635
552
RESET PERSPECTIVE
reset-perspective\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
635
519
728
552
largest hub
watch max-one-of hubs [size]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
348
486
441
519
least famous user
watch min-one-of network-users [fame]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
434
519
527
552
smallest hub
watch min-one-of hubs [size]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
720
486
813
519
happiest user
watch max-one-of network-users [opinion]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
534
486
627
519
unhappiest user
watch min-one-of network-users [opinion]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
627
486
720
519
most neutral user
watch min-one-of network-users [(abs opinion)]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

SWITCH
16
469
160
502
show-fame?
show-fame?
0
1
-1000

SLIDER
160
469
308
502
fame-intensity
fame-intensity
0
1
1
.01
1
NIL
HORIZONTAL

MONITOR
853
308
940
353
average opinion
average-opinion
4
1
11

BUTTON
159
105
308
138
increment by ten ticks
go-ten
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
1226
308
1313
353
reset plot
clear-plot\nset current-pen trials-to-show
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
218
368
308
401
visuals?
visuals?
0
1
-1000

SLIDER
160
535
308
568
node-closeness
node-closeness
1
4
3
.1
1
NIL
HORIZONTAL

BUTTON
159
138
308
171
set fame of user
set-fame-of-user
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

SLIDER
160
270
308
303
user-fame
user-fame
1
max-fame
50
1
1
NIL
HORIZONTAL

BUTTON
159
171
308
204
reset fame
reset-fame
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
16
171
159
204
reset opinions
reset-opinions
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

SLIDER
998
314
1170
347
trials-to-show
trials-to-show
1
10
10
1
1
trials
HORIZONTAL

OUTPUT
853
357
1313
562
12

@#$#@#$#@
## WHAT IS IT?

Communications networks like the internet are a set of linked hubs that provide a framework for the flow of information and ideas between network users. In some networks, a few central 'hubs' can have a lot of connections while all the others only have a few. This model is structured in such a way and is based partially on the 'Preferential Attachment' model contained within the NetLogo library in which new members added to the network prefer to make a connection to the more 'popular' (nodes with more connections) existing members.

This model seeks to explore the flow of opinion due to some arbitrary event inserted into the network at some arbitrary point. For GlaxoSmithKline, it will model how overall public opinion towards a drug changes when a related event is reported to the network (death, side effects, condition treatment, etc). This model allows us to see how different factors affect the spread and propagation of that opinion through the network.

There are several different factors that can affect this, such as:
-Where in the network the event is first reported, whether it is reported at a central 'hub' like Twitter or Facebook or a fringe 'hub' like a small exlusive forum affects how quickly and how far it travels within the network.
-How 'intense' the event is: for example, an event like death from unpredicted side effects will have a much more profound effect on overall opinion than an event regarding minor positive side effects.
-How 'famous' the source user is: for example, a normal person reporting their experience is going to hold a lot less sway than someone like Dr. Oz.
-How quickly the public as a whole forgets things. This data can be derived from Google search trends by studying various medical disasters and how long it took for search volume to drop from its peak to a level below 5%, 10%, etc.
-Whether there exists a conflicting opinion in the network at the same time. If two users have vastly different experiences with a drug (one good and one bad), which opinion wins out is determined by all of the above factors and by the proximity of the events to each other.

## HOW IT WORKS

The model shape is formed by first using the 'Preferential Attachment' algorithm to create a series of hub nodes. The algorithm works by first laying out two hubs attached to one another with a link. At each step, a new hub is added. A new hub picks an existing hub to connect to randomly - but with some bias. More specifically, a hub’s chance of being selected is directly proportional to the number of connections it already has, or its “degree.” This is the mechanism which is called “preferential attachment.”

After the network of hubs is complete, the model begins attaching network-users in the same manner (using the same algorithm). However, these network-users can only be attached to an existing hub and not to themselves. This simulates how actual users communicate ideas to the public within a network - first, that idea must pass through some public domain before being noticed by others.

Once the network is done being constructed, the user can insert an event at an arbitrary hub or user and run the simulation to see how the opinion propagates over the network. With every tick, some fraction of a node's opinion value (proportional to its relative fame and size depending on whether it is a network-user or a hub, respectively) is added to the opinion value of each node linked to it. The notion of forgetfulness is also factored in each tick after the opinion is added, by decreasing the magnitude of all opinions by a value proportional to both the amount of time that has passed, the rate of forgetfulness (because everything is forgotten eventually), and the current opinion value. Once overall average public opinion has reached essentially neutral (below a magnitude of .001), the simulation stops and is ready for another input.

Opinion value per node ranges from -opinion_magnitude --> opinion_magnitude. By default, opinion_magnitude is equal to 1000. Larger values result in slower-changing opinions. Very negative opinions are displayed in more intense shades of red, neutral opinions are displayed in light shades of red or green (completely neutral is displayed in white), and very positive opinions are displayed in brighter shades of green.

## HOW TO USE IT

###**CONTROL PANEL**

Pressing the **SETUP SIMULATION** button creates the network.

Pressing the **UPDATE VISUALS** button continuously updates the visual picture of the model based on what settings have been selected in the **VISUAL SETTINGS** category.

Pressing the **INSERT RANDOM EVENT** button inserts an event at a node in the simulation whose type and location is selected in the **SIMULATION SETTINGS** category.

Pressing the **INSERT EVENT AT POINT** button allows the user to insert an event at a custom location/node.

Pressing the **RUN SIMULATION** button propagates any current opinions present within the network outwards to connected nodes. The simulation is run until overall opinion magnitude reaches a level at or below .001.

Pressing the **INCREMENT BY ONE TICK** button increments the entire simulation by one tick.

Pressing the **INCREMENT BY TEN TICKS** button increments the entire simulation by ten ticks.

Pressing the **SET FAME OF USER** button allows the user to set the fame of any network-user to the value selected in the **SIMULATION SETTINGS** section. Network-users begin with a fame value randomly selected between 0.1 and 1/5 of whatever the current max-fame is set to (default 50).

###**SIMULATION SETTINGS**

Toggling the **USER-TO-HUB-RATIO** slider chooses how many users are inserted into the simulation based on how many hubs there are. If 40 hubs are in the network and the **USER-TO-HUB-RATIO** slider is set to a value of '4', then 160 users will be inserted into the simulation, giving a total value of 200 nodes. If the slider is set to '6', 240 users will be inserted, and so on and so forth. This allows the user to experiment with how user density affects opinion propagation and how clusters of very famous users affect information flow.

The **TYPE-OF-EVENT** chooser lets the user decide the level of intensity of the inputted event. Each setting corresponds to a consistent numeric value related to the current opinion_magnitude (default 1000):
-**DEATH** = (-1) * opinion_magnitude
-**MAJOR NEGATIVE SIDE EFFECTS** = (-2/3) * opinion_magnitude
-**MINOR NEGATIVE SIDE EFFECTS** = (-1/3) * opinion_magnitude
-**NOTHING HAPPENED** = (0) * opinion_magnitude
-**MINOR POSITIVE EFFECTS** = (1/3) * opinion_magnitude
-**MAJOR POSITIVE EFFECTS** = (2/3) * opinion_magnitude
-**CONDITION FULLY TREATED** = (1) * opinion_magnitude

The **EVENT-STARTING-POINT** chooser lets the user decide where the inputted event begins its path. Each setting corresponds to a different size hub or different fame network-user, the choice of standard deviation magnitude is arbitrary:
-**NORMAL NETWORK USER** = user whose fame value is within 1.25 standard deviations of the minimum fame value in the set of network-users.
-**MODERATELY FAMOUS NETWORK USER** = user whose fame value is within 1.25 standard deviations of the mean fame value in the set of network-users.
-**VERY FAMOUS NETWORK USER** = user whose fame value is within 1.25 standard deviations of the maximum value in the set of network-users.
-**MOST FAMOUS NETWORK USER** = user whose fame value is the highest of all the network-users.
-**SMALL INFORMATION HUB** = information hub whose size value is within 1.25 standard deviations of the minimum size value in the set of hubs.
-**MEDIUM INFORMATION HUB** = information hub whose size value is within 1.25 standard deviations of the mean size value in the set of hubs.
-**LARGE INFORMATION HUB** = information hub whose size value is within 1.25 standard deviations of the maximum size value in the set of hubs.
-**LARGEST INFORMATION HUB** = information hub whose size value is the highest of all the information hubs.

The **FORGETFULNESS** slider lets the user decide what level of forgetfulness to assign to the population. **FORGETFULNESS** factors in every tick when the opinion change per node is calculated. After opinion from each surrounding node is added to any given node, the magnitude of the node's opinion is decreased by a value proportional to the current tick-count and the selected value of **FORGETFULNESS**. So, as time passes the overall opinion will inevitably return to neutral eventually. The numbers on the slider are arbitrary and are chosen to best show trends in the simulation. 

The **NUMBER-OF-HUBS** input window lets the user input a custom number of hubs into the simulation. Note that the more total nodes (hubs + hubs * **USER_TO_HUB_RATIO**) there are to input into the simulation, the longer it will take to build. On weaker machines this becomes a significant issue, but with enough processing power it should be manageable.

The **# NODES** output window shows the user how close the network is to being completed. This is a nice status indicator to use when dealing with large networks that require a lot of processing time.

The **LEVEL-OF-FAME** slider is used to select a custom fame value for a network-user changed through the **SET FAME OF USER** button.

###**PERSPECTIVES**

The **SMALLEST HUB** button focuses the perspective on the hub in the network with the smallest 'size' value.

The **LARGEST HUB** button focuses the perspective on the hub in the network with the largest 'size' value.

The **LEAST FAMOUS USER** button focuses the perspective on the user in the network with the smallest 'fame' value.

The **MOST FAMOUS USER** button focuses the perspective on the user in the network with the largest 'fame' value.

The **UNHAPPIEST USER** button focuses the perspective on the user in the network with the lowest 'opinion' value.

The **MOST NEUTRAL USER** button focuses the perspective on the user in the network with an 'opinion' value closest to neutral (value of 0).

The **HAPPIEST USER** button focuses the perspective on the user in the network with the highest 'opinion' value.

The **RESET PERSPECTIVE** button sets the perspective back to normal.

###**GRAPHS AND OUTPUT**

The graph displays the average overall opinion in the simulation at whatever the current tick value is. It starts off blank, and stores max 10 'trials' of simulations before looping around and overwriting previous trials. You can reset the plot by either pressing the **SETUP SIMULATION** button or pressing **RESET PLOT**. Otherwise, there is no way to completely clear the plot (besides reopening the file).

If a trial is interrupted midway through by clicking the **RESET OPINIONS** button, the next graphed trial will use the same color as the interrupted trial.

The **TRIALS-TO-SHOW** slider lets the user decide how many trials to display at a time on the plot. Larger values suit the comparison of more data sets, while smaller values give a cleaner interface. If you change the value of this slider to something lower than the current 'max' graphed trial, every trial above what you set the new value to be will be erased upon the starting of a new trial.

The **AVERAGE OPINION** output window in the bottom left corner below the graph shows the current instantaneous output value.

The large output box displays some stats on the simulation once it has ended. For now, it only shows the maximum magnitude opinion reached, the time (in ticks) that it takes for the network to reach a neutral opinion from  the start, and the value of the fastest rate of change of opinion (and which tick it occurred at).

###**VISUAL EFFECT SETTINGS**

The **VISUALS?** toggle switch toggles whether or not the model updates any sort of visual settings. Turning it off will allow the model to run faster. It is turned off by default.

The **OPTIMIZE-LAYOUT?** toggle switch toggles whether or not network layout is optimized to be 'readable'. It is turned on by default. 

The **LAYOUT-SPEED** slider lets the user decide how quickly the model sets up the optimum node layout. A larger value will result in a prettier-looking initial network of nodes that requires little time to reach an optimum layout, but will also significantly increase the time it takes for a 'usable' simulation state to be reached (usable meaning a network where the nodes are spaced apart enough to not be overlapping one another). As the total number of nodes increases, smaller values of **LAYOUT-SPEED** are recommended for the initial model building.

The **SHOW-OPINIONS?** toggle switch toggles whether or not node opinions are displayed on the model. It is turned on by default.

The **OPINION-INTENSITY** slider lets the user decide how intense they want opinions to appear on the model. Changing this value is sometimes useful to visualize intense opinions better - extremely positive or negative values might be too dark to display properly on some screens.

The **SHOW-FAME?** toggle switch toggles whether or not fame is displayed on the model. It is turned on by default. Fame is shown by a small yellow 'glow' around a network-user: the more famous an agent is, the brighter they glow.

The **FAME-INTENSITY** slider lets the user decide how intensely they want fame to be displayed on the model.

The **RESIZE-HUBS?** toggle switch toggles whether or not hub size is visually displayed on the model. Size is proportionate to how many links are connected to that hub.

The **SCALING-FACTOR** slider lets the user decide how large or small they want hub size differences to be displayed. This is useful when trying to select a specific hub to insert an event into - only a small circle close to the center of the hub is actually 'clickable'. This is also useful for model visibility with large numbers of nodes.

The **NET-SPREAD** slider lets the user decide how close together they want nodes to be. This is also useful for model visibility with large numbers of nodes and to show nodes that might be 'hidden' by large hubs.

## THINGS TO NOTICE

The networks that result from running this model are often called “scale-free” or “power law” networks. These are networks in which the distribution of the number of connections of each node is not a normal distribution — instead it follows what is a called a power law distribution. Power law distributions are different from normal distributions in that they do not have a peak at the average, and they are more likely to contain extreme values (see Albert & Barabási 2002 for a further description of the frequency and significance of scale-free networks). Barabási and Albert originally described this mechanism for creating networks, but there are other mechanisms of creating scale-free networks and so the networks created by the mechanism implemented in this model are referred to as Barabási scale-free networks.

The layout of a network can significantly affect how opinion propagates through it. Networks created with the same number of hubs and network-users can have completely different layouts and thus completely different propagation rates. 

How famous the origin user is (or how large the origin hub is) can also have a profound effect on the rate that information travels. Their distance from the largest hub also plays a significant role in how much of the network is ultimately affected by the opinion.

## GENERAL MODEL TUTORIAL

To understand how the model works, let's go through a test-run simulation. 

Type in '50' for the number of hubs, and set the *USER-TO-HUB-RATIO* to 3. Click **SETUP SIMULATION** and then **UPDATE VISUALS**. When the # nodes counter reaches 200 (50 hubs + 50*3 users = 200), the network will be displayed. You should see a large web of nodes that is not evenly distributed - there is a clear 'central' hub with other branches trailing off of it. Try experimenting with turning different visual settings on and off and adjusting the sliders for each. How does the image of the model change? Can you think of situations in which certain settings would be more reasonable to use than others?

Now let's try inputting an event into the simulation. Set **TYPE-OF-EVENT** to **DEATH**, and **EVENT-STARTING-POINT** to **LARGEST INFORMATION HUB**. Set **FORGETFULNESS** to .008. Press **INSERT RANDOM EVENT**, and then **RUN SIMULATION**. Watch how the opinion gradually propagates across the network and chart the overall opinion through the graph on the right side. Once the network returns to normal, look at the stats that are printed. In what situations would they be different? Try experimenting with different values of **FORGETFULNESS** and different types of events/event start points. Hold some values constant while changing others. Do you notice any trends? 

## THINGS TO TRY

Large values of forgetfulness result in simulations that end earlier and have relatively smooth 'average opinion' curves. Smaller values result in simulations that end later and have 'rockier' curves. These rockier curves can be used to analyze the overall structure of the network - because a network is not evenly distributed, the random bends and slants towards the end of the curve correspond to different branches of the network gradually returning to neutral at slightly different rates. These bends and slants can be 'ironed out' by running simulations on several different networks all created under the same parameters and then averaging out all the data points. This might be easier with SQL integrated into the model (extensions and tutorials exist on the NetLogo GitHub page).

Try mimicking some real-world scenarios:

1) A new medication is released to the market after finishing its final round of testing. About a year later, a normal average joe reports major negative side effects to the internet on a fringe forum. The network as a whole experiences a temporary large-scale change in opinion that lasts an amount of time proportional to forgetfulness. How does the intensity of the person's experience affect the path of the opinion? How does the initial fame of the person affect the change in the network? How much influence does someone with 1 fame (perhaps corresponding to a homeless person) have on the propagation versus someone with 50 fame (the Surgeon General)? How does the person's proximity to the central hub affect the process? What if you start the opinion off at a hub instead of a person? 

2) Picture the same scenario as above except two conflicting opinions enter the network at different points: one positive opinion and one negative opinion. Test out the same changes in parameters including one new one: how far away from each other the opinions begin. Figure out which situations result in the 'victory' of one opinion over another. 

3) What else can you think of? The possibilities are endless, but this study focuses mainly on situations pertinent to pharmaceuticals. This model can be used to study the travel of any sort of information through any sort of communications network (however, for the results to be reasonably accurate a few parameters must be extended by the user).

## EXTENDING THE MODEL

The formulas for information propagation and how user fame/hub size affects it can and should be changed to more accurately model real-world phenomena. The current placeholder formulas only exist as a way to show general trends and the strength of agent-based modeling.

A variety of numeric stats can be pulled from the grpah itself: 
-How long it takes to reach a neutral opinion from the beginning of the simulation
-What the peak magnitude opinion is
-??? (integrals, equation transformation, etc)

The network creation algorithm can be modified to result in different node concentrations that can model a larger 

## RELATED MODELS

See 'Preferential Attachment' to understand the basis of the network setup algorithm.

## CREDITS AND REFERENCES

The layout algorithm is based on the Fruchterman-Reingold layout algorithm. More information about this algorithm can be obtained at: http://citeseer.ist.psu.edu/fruchterman91graph.html.

This model was constructed primarily for the use of GlaxoSmithKline's Medical Analytics Department. While the barebones network creation algorithm was pulled from the 'Preferential Attachment' model, everything else built on top of that was created by George Hito.
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

computer server
false
0
Rectangle -7500403 true true 75 30 225 270
Line -16777216 false 210 30 210 195
Line -16777216 false 90 30 90 195
Line -16777216 false 90 195 210 195
Rectangle -10899396 true false 184 34 200 40
Rectangle -10899396 true false 184 47 200 53
Rectangle -10899396 true false 184 63 200 69
Line -16777216 false 90 210 90 255
Line -16777216 false 105 210 105 255
Line -16777216 false 120 210 120 255
Line -16777216 false 135 210 135 255
Line -16777216 false 165 210 165 255
Line -16777216 false 180 210 180 255
Line -16777216 false 195 210 195 255
Line -16777216 false 210 210 210 255
Rectangle -7500403 true true 84 232 219 236
Rectangle -16777216 false false 101 172 112 184

computer workstation
false
0
Rectangle -7500403 true true 60 45 240 180
Polygon -7500403 true true 90 180 105 195 135 195 135 210 165 210 165 195 195 195 210 180
Rectangle -16777216 true false 75 60 225 165
Rectangle -7500403 true true 45 210 255 255
Rectangle -10899396 true false 249 223 237 217
Line -16777216 false 60 225 120 225

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.2.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
0
@#$#@#$#@
