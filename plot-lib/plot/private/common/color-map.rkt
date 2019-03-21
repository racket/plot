#lang typed/racket/base
(require racket/match
         racket/math
         "draw-attribs.rkt"
         "type-doc.rkt"
         "types.rkt"
         "parameters.rkt")


;;.................................... declare the predefined color maps ....

;; These color maps correspond to the Matplotlib 3.0.3 qualitative color maps
;; with the same names.  See
;; https://matplotlib.org/examples/color/colormaps_reference.html

(: color-map-pastel1 Color-Map)
(define color-map-pastel1
  #((251 180 174)
    (179 205 227)
    (204 235 197)
    (222 203 228)
    (254 217 166)
    (255 255 204)
    (229 216 189)
    (253 218 236)
    (242 242 242)))

(: color-map-pastel2 Color-Map)
(define color-map-pastel2
  #((179 226 205)
    (253 205 172)
    (203 213 232)
    (244 202 228)
    (230 245 201)
    (255 242 174)
    (241 226 204)
    (204 204 204)))

(: color-map-paired Color-Map)
(define color-map-paired
  #((166 206 227)
    (31 120 180)
    (178 223 138)
    (51 160 44)
    (251 154 153)
    (227 26 28)
    (253 191 111)
    (255 127 0)
    (202 178 214)
    (106 61 154)
    (255 255 153)
    (177 89 40)))

(: color-map-accent Color-Map)
(define color-map-accent
  #((127 201 127)
    (190 174 212)
    (253 192 134)
    (255 255 153)
    (56 108 176)
    (240 2 127)
    (191 91 22)
    (102 102 102)))

(: color-map-dark2 Color-Map)
(define color-map-dark2
  #((27 158 119)
    (217 95 2)
    (117 112 179)
    (231 41 138)
    (102 166 30)
    (230 171 2)
    (166 118 29)
    (102 102 102)))

(: color-map-set1 Color-Map)
(define color-map-set1
  #((228 26 28)
    (55 126 184)
    (77 175 74)
    (152 78 163)
    (255 127 0)
    (255 255 51)
    (166 86 40)
    (247 129 191)
    (153 153 153)))

(: color-map-set2 Color-Map)
(define color-map-set2
  #((102 194 165)
    (252 141 98)
    (141 160 203)
    (231 138 195)
    (166 216 84)
    (255 217 47)
    (229 196 148)
    (179 179 179)))

(: color-map-set3 Color-Map)
(define color-map-set3
  #((141 211 199)
    (255 255 179)
    (190 186 218)
    (251 128 114)
    (128 177 211)
    (253 180 98)
    (179 222 105)
    (252 205 229)
    (217 217 217)
    (188 128 189)
    (204 235 197)
    (255 237 111)))

(: color-map-tab10 Color-Map)
(define color-map-tab10
  #((31 119 180)
    (255 127 14)
    (44 160 44)
    (214 39 40)
    (148 103 189)
    (140 86 75)
    (227 119 194)
    (127 127 127)
    (188 189 34)
    (23 190 207)))

(: color-map-tab20 Color-Map)
(define color-map-tab20
  #((31 119 180)
    (174 199 232)
    (255 127 14)
    (255 187 120)
    (44 160 44)
    (152 223 138)
    (214 39 40)
    (255 152 150)
    (148 103 189)
    (197 176 213)
    (140 86 75)
    (196 156 148)
    (227 119 194)
    (247 182 210)
    (127 127 127)
    (199 199 199)
    (188 189 34)
    (219 219 141)
    (23 190 207)
    (158 218 229)))

(: color-map-tab20b Color-Map)
(define color-map-tab20b
  #((57 59 121)
    (82 84 163)
    (107 110 207)
    (156 158 222)
    (99 121 57)
    (140 162 82)
    (181 207 107)
    (206 219 156)
    (140 109 49)
    (189 158 57)
    (231 186 82)
    (231 203 148)
    (132 60 57)
    (173 73 74)
    (214 97 107)
    (231 150 156)
    (123 65 115)
    (165 81 148)
    (206 109 189)
    (222 158 214)))

(: color-map-tab20c Color-Map)
(define color-map-tab20c
  #((49 130 189)
    (107 174 214)
    (158 202 225)
    (198 219 239)
    (230 85 13)
    (253 141 60)
    (253 174 107)
    (253 208 162)
    (49 163 84)
    (116 196 118)
    (161 217 155)
    (199 233 192)
    (117 107 177)
    (158 154 200)
    (188 189 220)
    (218 218 235)
    (99 99 99)
    (150 150 150)
    (189 189 189)
    (217 217 217)))

;; New Tableau 10 color map from
;; https://www.tableau.com/about/blog/2016/7/colors-upgrade-tableau-10-56782
(: color-map-tab10n Color-Map)
(define color-map-tab10n
  #((78 121 165)
    (241 143 59)
    (224 88 91)
    (119 183 178)
    (90 161 85)
    (237 201 88)
    (175 122 160)
    (254 158 168)
    (156 117 97)
    (186 176 172)))

(define color-maps
  (hash-copy ; hash copy will make our hash table mutable, allowing register-color-map to work
   (hash
    'pastel1 color-map-pastel1
    'pastel2 color-map-pastel2
    'paired color-map-paired
    'dark2 color-map-dark2
    'set1 color-map-set1
    'set2 color-map-set2
    'set3 color-map-set3
    'tab10 color-map-tab10
    'tab10n color-map-tab10n
    'tab20 color-map-tab20
    'tab20b color-map-tab20b
    'tab20c color-map-tab20c)))



;;.................................................. color map interface ....

(: color-map-names (-> (Listof Symbol)))
(define (color-map-names)
  (hash-keys color-maps))

(: color-map-size (-> Symbol Nonnegative-Integer))
(define (color-map-size name)
  (define cm (hash-ref color-maps name
                       (lambda () (error (format "Unknown color map name: ~a" name)))))
  (vector-length cm))

(: register-color-map (-> Symbol Color-Map Any))
(define (register-color-map name cm)
  (hash-set! color-maps name cm))


(:: ->color-map-pen-color (-> Integer (List Byte Byte Byte)))
(define (->color-map-pen-color index)
  (define cm
    (cast 
     (if (symbol? (plot-pen-color-map))
         (hash-ref color-maps (plot-pen-color-map)
                   (lambda () default-pen-colors))
         default-pen-colors)
     Color-Map))
  (define i (modulo index (vector-length cm)))
  (vector-ref cm i))

(:: ->color-map-brush-color (-> Integer (List Byte Byte Byte)))
(define (->color-map-brush-color index)
  (define cm
    (cast
     (if (symbol? (plot-brush-color-map))
         (hash-ref color-maps (plot-brush-color-map)
                   (lambda () default-brush-colors))
         default-brush-colors)
     Color-Map))
  (define i (modulo index (vector-length cm)))
  (vector-ref cm i))

(:: ->pen-color (-> Plot-Color (List Real Real Real)))
(define (->pen-color c)
  (cond [(exact-integer? c)  (->color-map-pen-color c)]
        [else                (->color c)]))

(:: ->brush-color (-> Plot-Color (List Real Real Real)))
(define (->brush-color c)
  (cond [(exact-integer? c)  (->color-map-brush-color c)]
        [else                (->color c)]))

(provide
 color-map-names
 color-map-size
 register-color-map
 ->pen-color
 ->brush-color)

