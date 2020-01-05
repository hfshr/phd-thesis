## Figures

library(DiagrammeR)
library(DiagrammeRsvg)

 studydesign <- grViz(
 "
 digraph {

# graph attributes
graph [rankdir = LR, 
overlap = true, 
fontsize = 12]

# node attributes
node [shape = rectangle]

# edge attributes
edge [color = black,
len = 0.5]

# node statements

T1 [label = <T1 <br/> <i>n</i>  = 351 <br/> All measures <br/> (no injury reporting)>]
T2 [label = <T2 <br/> <i>n</i>  = 236 <br/> All measures>]
T3 [label = <T3 <br/> <i>n</i>  = 157 <br/> All measures>]
T4 [label = <T4 <br/> <i>n</i>  = 163 <br/> Only injury reporting>]

d1 [label = 'Sep \n 2016 and 2017', shape = plaintext]
d2 [label = 'Jan \n 2017 and 2018', shape = plaintext]
d3 [label = 'May \n 2017 and 2018', shape = plaintext]
d4 [label = 'Sep \n 2017 and 2018', shape = plaintext]

d1 -> d2 -> d3 -> d4

T1 -> T2 -> T3 -> T4 

}
"
)

 studydesign

 grViz(" 
      digraph CFA {
      a [label = 'Node 1', shape = rectangle, color=CornflowerBlue ]; 

      node [shape = ellipse, color=CornflowerBlue]
      T1    [label = <Node 2 <br/> <u>extra detail</u>>]; 
      T2    [label = 'Node 3']; 

      {rank = same; a T1 T2}

      # Connect nodes with edges and labels
      a -> T1
      T2 -> a[dir=back]
       }

      ")
 
 sessionprotocol <- grViz("digraph {

# graph attributes
graph [
rankdir = TB, 
overlap = true, 
fontsize = 14]

node [shape = rectangle, 
width = 2, 
fixedsize = true]

start [label = 'Participants separated into groups \n Informed consent (T1 only)',
fixedsize = false]
pm [label = 'Computer-based \n measures', shape = oval]
phys [label = 'Physical measures', shape = oval]
les [label = 'LESCA \n (15 minutes)']
rst [label = 'RST-PQ \n (10 minutes)']
in [label = 'Injury reporting \n (5 minutes)']
hrv [label = 'HRV \n (10 minutes)']
myo [label = 'Myoton \n (5 minutes)']
bal [label = 'Postural stability \n (5 minutes)']
endpm [label = 'Start computer based \n measures']
endphys [label = 'Start physical \n measures']

start -> pm [label = 'Group 1']

pm -> les -> rst -> in -> endphys

start -> phys [label = 'Group 2']

phys -> hrv -> myo -> bal -> endpm

}")
sessionprotocol
 
 
 studydesign2 <- 
 grViz("digraph {

# graph attributes
graph [rankdir = LR, 
overlap = true]

# node attributes
node [shape = rectangle]

# edge attributes
edge [color = black,
len = 0.5]

# node statements

T1 [label = <T1 <br/> <i>n</i>  = 51 <br/> Study 1 measures <br/> (no injury reporting) + <br/> HITS>, fontsize = 14 ]
T2 [label = <T2 <br/> <i>n</i>  = 50 <br/> Study 1 measures + <br/> HITS>, fontsize = 14]
T3 [label = <T3 <br/> <i>n</i>  = 45 <br/> Study 1 measures + <br/> HITS>, fontsize = 14]
T4 [label = <T4 <br/> <i>n</i>  = 40 <br/> Only injury reporting>, fontsize = 14]
d1 [label = 'Sep \n 2016 and 2017', shape = plaintext, fontsize = 14]
d2 [label = 'Jan \n 2017 and 2018', shape = plaintext, fontsize = 14]
d3 [label = 'May \n 2017 and 2018', shape = plaintext, fontsize = 14]
d4 [label = 'Sep \n 2017 and 2018', shape = plaintext, fontsize = 14]

d1 -> d2 -> d3 -> d4

T1 -> T2 -> T3 -> T4 

}")
studydesign2

study2protocol  
grViz("digraph {

# graph attributes
graph [rankdir = TB, 
overlap = true]

# node attributes
node [shape = rectangle,
fixedsize = true,
width = 2.6,
height = 0.7]


study1 [label = 'All measures from study 1 \n 1600:1650']
pre [label = 'Pre training sample \n 1650:1700' ]
training [label = 'High intensity training session \n 1700:1900']
post [label = 'Post training sample \n 1900:1910']
storage [label = 'Samples transported to storage']

study1 -> pre -> training -> post -> storage
}"
)

save(studydesign, sessionprotocol, studydesign2, study2protocol, file = paste0("datavars/figures", ".RData"))

# variables path diag

grViz("
      digraph SEM {
      graph [layout = neato,
      overlap = false,
      fontsize = 12]

      node [shape = rectangle]

      a[pos = '-3,0!', label = 'Average NLE \n score']
      b[pos = '-2,-1.5!', label = 'Muscle stiffness']
      c[pos = '-2,1.5!', label = 'Cortisol']
      d[pos = '1,0!', label = 'Days missed']

      b->d
      c->d
      a->d
      a->b [style = 'dashed']
      a->c [style = 'dashed']
      b->c [style = 'dashed']
      }"
)


grViz("digraph {

# graph attributes
graph [rankdir = TB, 
overlap = true, 
fontsize = 5]

node [shape = rectangle]

edge [color = black,
len = 0.5]

N1 [label = 'Child node \n (Injured/Healthy)']
N2 [label = 'Parent node A \n (High/Low)']
N3 [label = 'Parent node B \n (High/Low)']

N2 -> N1 [label = 'arc']
N3 -> N1 [label = 'arc']

}")
