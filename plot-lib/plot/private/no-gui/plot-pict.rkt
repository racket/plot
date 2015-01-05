#lang typed/racket/base

(require "../common/type-doc.rkt"
         "../common/types.rkt"
         "../common/parameters.rkt"
         "../common/nonrenderer.rkt"
         "../plot2d/renderer.rkt"
         "../plot3d/renderer.rkt"
         "plot2d.rkt"
         "plot3d.rkt"
         "evil.rkt")

(provide (all-defined-out))

(:: plot
    (->* [(Treeof (U renderer2d nonrenderer))]
         [#:x-min (U Real #f) #:x-max (U Real #f)
          #:y-min (U Real #f) #:y-max (U Real #f)
          #:width Positive-Integer
          #:height Positive-Integer
          #:title (U String #f)
          #:x-label (U String #f)
          #:y-label (U String #f)
          #:legend-anchor Anchor
          #:out-file (U Path-String Output-Port #f)
          #:out-kind (U 'auto Image-File-Format)]
         Pict))
(define (plot renderer-tree
              #:x-min [x-min #f] #:x-max [x-max #f]
              #:y-min [y-min #f] #:y-max [y-max #f]
              #:width [width (plot-width)]
              #:height [height (plot-height)]
              #:title [title (plot-title)]
              #:x-label [x-label (plot-x-label)]
              #:y-label [y-label (plot-y-label)]
              #:legend-anchor [legend-anchor (plot-legend-anchor)]
              #:out-file [out-file #f]
              #:out-kind [out-kind 'auto])
  (when out-file
    (plot-file renderer-tree out-file out-kind
               #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max #:width width #:height height
               #:title title #:x-label x-label #:y-label y-label #:legend-anchor legend-anchor))
  
  (plot-pict renderer-tree
             #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max #:width width #:height height
             #:title title #:x-label x-label #:y-label y-label #:legend-anchor legend-anchor))

(:: plot3d
    (->* [(Treeof (U renderer3d nonrenderer))]
         [#:x-min (U Real #f) #:x-max (U Real #f)
          #:y-min (U Real #f) #:y-max (U Real #f)
          #:z-min (U Real #f) #:z-max (U Real #f)
          #:width Positive-Integer
          #:height Positive-Integer
          #:angle Real #:altitude Real
          #:title (U String #f)
          #:x-label (U String #f)
          #:y-label (U String #f)
          #:z-label (U String #f)
          #:legend-anchor Anchor
          #:out-file (U Path-String Output-Port #f)
          #:out-kind (U 'auto Image-File-Format)]
         Pict))
(define (plot3d renderer-tree
                #:x-min [x-min #f] #:x-max [x-max #f]
                #:y-min [y-min #f] #:y-max [y-max #f]
                #:z-min [z-min #f] #:z-max [z-max #f]
                #:width [width (plot-width)]
                #:height [height (plot-height)]
                #:angle [angle (plot3d-angle)]
                #:altitude [altitude (plot3d-altitude)]
                #:title [title (plot-title)]
                #:x-label [x-label (plot-x-label)]
                #:y-label [y-label (plot-y-label)]
                #:z-label [z-label (plot-z-label)]
                #:legend-anchor [legend-anchor (plot-legend-anchor)]
                #:out-file [out-file #f]
                #:out-kind [out-kind 'auto])
  (when out-file
    (plot3d-file renderer-tree out-file out-kind
                 #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max #:z-min z-min #:z-max z-max
                 #:width width #:height height #:title title
                 #:angle (or angle (plot3d-angle)) #:altitude (or altitude (plot3d-altitude))
                 #:x-label x-label #:y-label y-label #:z-label z-label #:legend-anchor legend-anchor))
  
  (plot3d-pict renderer-tree
               #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max #:z-min z-min #:z-max z-max
               #:width width #:height height #:title title
               #:angle (or angle (plot3d-angle)) #:altitude (or altitude (plot3d-altitude))
               #:x-label x-label #:y-label y-label #:z-label z-label #:legend-anchor legend-anchor))
