;;; ==========================================================================
;;; ETARB.lsp - Etiquetas de numeracion para arboles                  (v6)
;;;
;;;  ETARB          Flujo: 1) seleccionar AREA  2) elegir TIPO de punto
;;;                 3) recorrido, estilo, altura, giro -> crea las etiquetas
;;;                 (capa "Etiquetas"), numeradas por tipo siguiendo el
;;;                 recorrido. Si el tipo ya tiene etiquetas pregunta:
;;;                 Continuar la numeracion o Reiniciar desde 1.
;;;  ETARBEDIT      Edita ESCALA y GIRO de todas las etiquetas.
;;;  ETARBESC       Solo escala (altura del numero o factor).
;;;  ETARBROT       Solo giro (angulo, 2 puntos, Vista o Incremento).
;;;  ETARBESTILO    Cambia el estilo de todas las etiquetas existentes.
;;;  ETARBACOMODAR  Recalcula la posicion de las etiquetas para evitar cruces.
;;;  ETARBFRENTE    Trae todas las etiquetas al frente.
;;;  ETARBBORRAR    Borra todas las etiquetas.
;;;
;;; Estilos:  1 Medallon  (aguja ahusada + anillo grueso/fino)
;;;           2 Llamada   (aguja ahusada + linea base ahusada)
;;;           3 Hexagono  (aguja ahusada + doble hexagono)
;;;
;;; Cada estilo existe en 8 direcciones (bloques ETQ_<ESTILO>_<angulo>); al
;;; crear, cada etiqueta usa la primera direccion que no choque con otras
;;; etiquetas ni con otros arboles. Cada bloque lleva un WIPEOUT de fondo
;;; (oculta lo que hay debajo) y las etiquetas se envian al frente.
;;;
;;; Tipo de arbol (criterio de agrupacion):
;;;   - Bloque (INSERT)            -> nombre del bloque (nombre efectivo si es dinamico)
;;;   - Punto Civil 3D (COGO)      -> nombre del estilo de punto
;;;   - Punto AutoCAD (POINT)      -> capa del punto
;;; El tipo se guarda en cada etiqueta como XDATA ("ETARB").
;;; ==========================================================================
(vl-load-com)

(setq *etq-h*    (cond (*etq-h*   *etq-h*)   (T 1.5)))   ; altura del numero
(setq *etq-rot*  (cond (*etq-rot* *etq-rot*) (T 0.0)))   ; giro (radianes)
(setq *etq-est*  (cond (*etq-est* *etq-est*) (T 1)))     ; estilo 1..3
(setq *etq-ss*   nil)                                    ; ss al definir bloques
(setq *etq-obs*  nil)                                    ; obstaculos (x y r)
(setq *etq-lock* nil)

(defun etq_capa   () "Etiquetas")
(defun etq_estilo () "ETIQ_ARB")
(defun etq_app    () "ETARB")
(defun etq_nombres () '("Medallon" "Llamada" "Hexagono"))

;; (base-del-bloque  altura-del-atributo)
(defun etq_estilos () '(("ETQ_MEDALLON" 0.75) ("ETQ_LLAMADA" 0.9) ("ETQ_HEXAGONO" 0.7)))
(defun etq_base (est) (car  (nth (1- est) (etq_estilos))))
(defun etq_ah   (est) (cadr (nth (1- est) (etq_estilos))))
(defun etq_blk  (est ang) (strcat (etq_base est) "_" (itoa ang)))
;; Direcciones (grados) por orden de preferencia
(defun etq_dirs () '(45 135 315 225 0 90 180 270))

;; "ETQ_HEXAGONO_135" -> (3 135); nil si no es un bloque de etiqueta
(defun etq_parse (nm / est r)
  (setq est 1 nm (strcase nm))
  (while (and (not r) (<= est 3))
    (if (wcmatch nm (strcat (etq_base est) "_*"))
      (setq r (list est (atoi (substr nm (+ (strlen (etq_base est)) 2)))))
      (setq est (1+ est))))
  r)

;;; ---------------------------------------------------------------- geometria del bloque
;; Geometria (unidades del bloque, arbol en 0,0) de un estilo y direccion:
;; (cx cy radio-de-choque  tx ty justif-vertical  ex ey sx)
;;   cx cy / radio: circulo que ocupa la etiqueta (para evitar cruces)
;;   tx ty / just.: posicion del numero;  ex ey: fin de la aguja;  sx: lado
(defun etq_geom (est ang / a ux uy dc sx ex ey phi m dl hit)
  (setq a (* ang (/ pi 180.0)) ux (cos a) uy (sin a))
  (cond
    ((= est 1)
     (setq dc 2.19)
     (list (* dc ux) (* dc uy) 1.05 (* dc ux) (* dc uy) 2
           (* 1.19 ux) (* 1.19 uy) 1.0))
    ((= est 3)
     (setq dc  2.26
           phi (rem (+ ang 180) 360)
           m   (rem (+ (- phi 30) 360) 60)
           dl  (min m (- 60 m))
           hit (/ (* 1.05 (cos (/ pi 6.0))) (cos (* dl (/ pi 180.0)))))
     (list (* dc ux) (* dc uy) 1.1 (* dc ux) (* dc uy) 2
           (* (- dc hit) ux) (* (- dc hit) uy) 1.0))
    (T
     (setq ex (* 1.27 ux) ey (* 1.27 uy)
           sx (if (>= ux -0.01) 1.0 -1.0))
     (list (+ ex (* sx 1.15)) (+ ey 0.55) 1.3
           (+ ex (* sx 1.15)) (+ ey 0.15) 1
           ex ey sx))))

;; entmake que ademas registra la entidad en *etq-ss* (para -BLOCK)
(defun etq_emk (l)
  (entmake l)
  (if *etq-ss* (ssadd (entlast) *etq-ss*)))

;; verts: lista de (x y ancho-ini ancho-fin bulge)
(defun etq_lwpv (verts cerrada)
  (etq_emk
    (append
      (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(8 . "0") '(62 . 0)
            '(100 . "AcDbPolyline") (cons 90 (length verts))
            (cons 70 (if cerrada 1 0)))
      (apply 'append
             (mapcar '(lambda (v)
                        (list (list 10 (car v) (cadr v))
                              (cons 40 (nth 2 v)) (cons 41 (nth 3 v))
                              (cons 42 (nth 4 v))))
                     verts)))))

;; disco relleno de radio r (polilinea de 2 arcos con grosor = r)
(defun etq_disco (cx cy r)
  (etq_lwpv (list (list (- cx (/ r 2.0)) cy r r 1.0)
                  (list (+ cx (/ r 2.0)) cy r r 1.0)) T))

;; anillo grueso de radio r y espesor w
(defun etq_anillo (cx cy r w)
  (etq_lwpv (list (list (- cx r) cy w w 1.0) (list (+ cx r) cy w w 1.0)) T))

(defun etq_circulo (cx cy r)
  (etq_emk (list '(0 . "CIRCLE") '(8 . "0") '(62 . 0) (list 10 cx cy 0.0) (cons 40 r))))

;; aguja ahusada desde el arbol (0,0) hasta (x,y); termina con espesor w
(defun etq_aguja (x y w)
  (etq_lwpv (list (list 0.0 0.0 0.0 w 0.0) (list x y 0.0 0.0 0.0)) nil))

;; vertices (x y) de un poligono regular de n lados
(defun etq_poli (cx cy r n / k a pts)
  (setq k 0)
  (repeat n
    (setq a   (* k (/ (* 2.0 pi) n))
          pts (cons (list (+ cx (* r (cos a))) (+ cy (* r (sin a)))) pts)
          k   (1+ k)))
  (reverse pts))

;; poligono regular con grosor w
(defun etq_poligono (cx cy r n w)
  (etq_lwpv (mapcar '(lambda (p) (list (car p) (cadr p) w w 0.0)) (etq_poli cx cy r n)) T))

;; Fondo: WIPEOUT a partir de una polilinea cerrada de lados rectos.
;; Si el comando no existe en este CAD, se omite sin error.
(defun etq_mascara (pts / p e)
  (entmake (append (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(8 . "0")
                         '(100 . "AcDbPolyline") (cons 90 (length pts)) '(70 . 1))
                   (mapcar '(lambda (q) (cons 10 q)) pts)))
  (setq p (entlast))
  (command "_.WIPEOUT" "_P" p "_Y")
  (while (> (getvar "CMDACTIVE") 0) (command ""))
  (setq e (entlast))
  (if (and e (entget e) (= (cdr (assoc 0 (entget e))) "WIPEOUT"))
    (progn
      (entmod (subst (cons 8 "0") (assoc 8 (entget e)) (entget e)))
      (ssadd e *etq-ss*))
    (if (and p (entget p)) (entdel p))))

;; Define el bloque de un estilo/direccion con -BLOCK (permite el WIPEOUT).
(defun etq_crear-bloque (est ang / nm g cx cy ex ey sx x1 os)
  (setq nm (etq_blk est ang)
        g  (etq_geom est ang)
        cx (nth 0 g) cy (nth 1 g) ex (nth 6 g) ey (nth 7 g) sx (nth 8 g)
        os (getvar "CMDECHO")
        *etq-ss* (ssadd))
  (setvar "CMDECHO" 0)
  ;; fondo primero: queda debajo del resto de entidades del bloque
  (cond
    ((= est 1) (etq_mascara (etq_poli cx cy 1.0 24)))
    ((= est 3) (etq_mascara (etq_poli cx cy 1.05 6)))
    (T (setq x1 (+ ex (* sx 2.3)))
       (etq_mascara (list (list ex (- ey 0.06)) (list x1 (- ey 0.06))
                          (list x1 (+ ey 1.3)) (list ex (+ ey 1.3))))))
  (etq_disco 0.0 0.0 0.12)                          ; marca del arbol
  (cond
    ((= est 1)
     (etq_aguja ex ey 0.10)
     (etq_anillo cx cy 1.0 0.10)
     (etq_circulo cx cy 0.84))
    ((= est 2)
     (etq_aguja ex ey 0.08)
     (etq_lwpv (list (list ex ey 0.05 0.0 0.0)
                     (list (+ ex (* sx 2.3)) ey 0.0 0.0 0.0)) nil))
    ((= est 3)
     (etq_aguja ex ey 0.10)
     (etq_poligono cx cy 1.05 6 0.10)
     (etq_poligono cx cy 0.90 6 0.0)))
  ;; numero
  (etq_emk (list '(0 . "ATTDEF") '(100 . "AcDbEntity") '(8 . "0") '(62 . 0)
                 '(100 . "AcDbText") (list 10 (nth 3 g) (nth 4 g) 0.0)
                 (cons 40 (etq_ah est)) '(1 . "0")
                 '(50 . 0.0) '(41 . 1.0) '(51 . 0.0) (cons 7 (etq_estilo))
                 '(71 . 0) '(72 . 1) (list 11 (nth 3 g) (nth 4 g) 0.0)
                 '(100 . "AcDbAttributeDefinition") '(280 . 0)
                 '(3 . "Numero") '(2 . "NUM") '(70 . 0) '(73 . 0)
                 (cons 74 (nth 5 g))))
  (command "_.-BLOCK" nm "_non" (trans '(0.0 0.0 0.0) 0 1) *etq-ss* "")
  (while (> (getvar "CMDACTIVE") 0) (command ""))
  (setvar "CMDECHO" os)
  (setq *etq-ss* nil)
  (if (not (tblsearch "BLOCK" nm))
    (progn (prompt (strcat "\nNo se pudo crear el bloque " nm ".")) (exit))))

(defun etq_init ()
  (regapp (etq_app))
  ;; Capa
  (if (not (tblsearch "LAYER" (etq_capa)))
    (entmake (list '(0 . "LAYER") '(100 . "AcDbSymbolTableRecord")
                   '(100 . "AcDbLayerTableRecord") (cons 2 (etq_capa))
                   '(70 . 0) '(62 . 7) '(6 . "Continuous"))))
  ;; Estilo de texto (Century Gothic; si no existe, Windows sustituye)
  (if (not (tblsearch "STYLE" (etq_estilo)))
    (entmake (list '(0 . "STYLE") '(100 . "AcDbSymbolTableRecord")
                   '(100 . "AcDbTextStyleTableRecord") (cons 2 (etq_estilo))
                   '(70 . 0) '(40 . 0.0) '(41 . 1.0) '(50 . 0.0) '(71 . 0)
                   '(42 . 1.0) '(3 . "GOTHIC.TTF") '(4 . "")
                   '(-3 ("ACAD" (1000 . "Century Gothic") (1071 . 0)))))))

;;; ---------------------------------------------------------------- capa
;; La capa se desbloquea/descongela mientras se trabaja y se restaura el bloqueo.
(defun etq_abrir (/ l)
  (setq l (vla-item (vla-get-Layers (vla-get-ActiveDocument (vlax-get-acad-object)))
                    (etq_capa)))
  (setq *etq-lock* (vla-get-Lock l))
  (vla-put-Lock l :vlax-false)
  (vl-catch-all-apply 'vla-put-Freeze (list l :vlax-false))
  (vla-put-LayerOn l :vlax-true))

(defun etq_cerrar (/ l)
  (if (equal *etq-lock* :vlax-true)
    (progn
      (setq l (vla-item (vla-get-Layers (vla-get-ActiveDocument (vlax-get-acad-object)))
                        (etq_capa)))
      (vla-put-Lock l :vlax-true)))
  (setq *etq-lock* nil))

;;; ---------------------------------------------------------------- utilidades
(defun etq_modelspace ()
  (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object))))

(defun etq_etiquetas ()
  (ssget "_X" (list '(0 . "INSERT")
                    (cons 2 "ETQ_MEDALLON_*,ETQ_LLAMADA_*,ETQ_HEXAGONO_*")
                    (cons 8 (etq_capa)) '(410 . "Model"))))

;; Tipo de arbol guardado en la etiqueta (XDATA).
(defun etq_tipo-de (en / x)
  (setq x (assoc -3 (entget en (list (etq_app)))))
  (if x (cdr (assoc 1000 (cdr (cadr x))))))

;; Lista de enames de las etiquetas de un tipo.
(defun etq_de-tipo (tipo / ss i en r)
  (if (setq ss (etq_etiquetas))
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq en (ssname ss i) i (1+ i))
        (if (equal (etq_tipo-de en) tipo) (setq r (cons en r))))))
  r)

(defun etq_borrar (/ ss i)
  (if (setq ss (etq_etiquetas))
    (progn (setq i 0)
           (repeat (sslength ss) (entdel (ssname ss i)) (setq i (1+ i)))
           (sslength ss))
    0))

(defun etq_es-etiqueta (nm) (etq_parse nm))

(defun etq_texto (o / r)
  (foreach a (vlax-invoke o 'GetAttributes)
    (if (= (strcase (vla-get-TagString a)) "NUM")
      (setq r (vla-get-TextString a))))
  (or r ""))

;; Estilo (1..3) de una referencia de etiqueta.
(defun etq_est-de (o)
  (or (car (etq_parse (vla-get-Name o))) 1))

;; Punto de insercion (3D) de una referencia.
(defun etq_ins (o)
  (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint o))))

;; Mayor numero entre las etiquetas dadas.
(defun etq_max-num (ens / m)
  (setq m 0)
  (foreach e ens (setq m (max m (atoi (etq_texto (vlax-ename->vla-object e))))))
  m)

;; Quita de pts los arboles que ya tienen etiqueta (misma posicion XY).
(defun etq_sin-etiqueta (pts ens / qs)
  (setq qs (mapcar '(lambda (e) (cdr (assoc 10 (entget e)))) ens))
  (vl-remove-if
    '(lambda (p)
       (vl-some '(lambda (q) (< (distance (list (car p) (cadr p)) (list (car q) (cadr q))) 1.0e-4))
                qs))
    pts))

(defun etq_tipo (en / ed typ o nm)
  (setq ed (entget en) typ (cdr (assoc 0 ed)))
  (cond
    ((= typ "INSERT")
     (setq o  (vlax-ename->vla-object en)
           nm (vl-catch-all-apply 'vla-get-EffectiveName (list o)))
     (if (or (vl-catch-all-error-p nm) (not nm) (= nm ""))
       (setq nm (cdr (assoc 2 ed))))
     (if (etq_es-etiqueta nm) nil nm))
    ((= typ "POINT") (strcat "PUNTO [" (cdr (assoc 8 ed)) "]"))
    (T ;; AECC_COGO_POINT (Civil 3D)
     (setq o  (vlax-ename->vla-object en)
           nm (vl-catch-all-apply
                '(lambda () (vlax-get-property (vlax-get-property o 'Style) 'Name))))
     (if (or (vl-catch-all-error-p nm) (not nm)) "PUNTO COGO" nm))))

(defun etq_pos (en / ed typ o x y)
  (setq ed (entget en) typ (cdr (assoc 0 ed)))
  (cond
    ((member typ '("INSERT" "POINT"))
     (list (cadr (assoc 10 ed)) (caddr (assoc 10 ed)) 0.0))
    (T
     (setq o (vlax-ename->vla-object en)
           x (vl-catch-all-apply '(lambda () (vlax-get-property o 'Easting)))
           y (vl-catch-all-apply '(lambda () (vlax-get-property o 'Northing))))
     (if (and (numberp x) (numberp y))
       (list x y 0.0)
       (list (cadr (assoc 10 ed)) (caddr (assoc 10 ed)) 0.0)))))

;; Agrupa la seleccion por tipo: ((tipo pt pt ...) ...) ordenado por nombre.
(defun etq_agrupar (ss / i en tipo g grupos)
  (setq i 0)
  (repeat (sslength ss)
    (setq en (ssname ss i) i (1+ i) tipo (etq_tipo en))
    (if tipo
      (if (setq g (assoc tipo grupos))
        (setq grupos (subst (cons tipo (cons (etq_pos en) (cdr g))) g grupos))
        (setq grupos (cons (list tipo (etq_pos en)) grupos)))))
  (vl-sort grupos '(lambda (a b) (< (car a) (car b)))))

;; Lista los tipos encontrados y pide cual etiquetar (numero o Todos).
(defun etq_elegir (grupos / k r)
  (prompt "\nTipos de arbol encontrados en el area:")
  (setq k 0)
  (foreach g grupos
    (setq k (1+ k))
    (prompt (strcat "\n  " (itoa k) ") " (car g) "  -  " (itoa (length (cdr g))) " und.")))
  (if (= k 1)
    grupos
    (progn
      (while (not r)
        (initget 6 "Todos")
        (setq r (getint (strcat "\nTipo a etiquetar [1-" (itoa k) "/Todos] <Todos>: ")))
        (cond ((null r) (setq r "Todos"))
              ((equal r "Todos"))
              ((> r k) (prompt "\nNumero fuera de rango.") (setq r nil))))
      (if (equal r "Todos") grupos (list (nth (1- r) grupos))))))

(defun etq_curva-p (en)
  (and en (not (vl-catch-all-error-p
                 (vl-catch-all-apply 'vlax-curve-getEndParam (list en))))))

(defun etq_dist-recorrido (path pt)
  (vlax-curve-getDistAtPoint path (vlax-curve-getClosestPointTo path pt)))

(defun etq_remnth (n l / i r)
  (setq i 0)
  (foreach x l (if (/= i n) (setq r (cons x r))) (setq i (1+ i)))
  (reverse r))

;; Sin eje: cadena de vecino mas cercano desde el punto de inicio.
(defun etq_vecino (pts p0 / res cur best bd bi i d)
  (setq cur p0)
  (while pts
    (setq best (car pts) bd (distance cur best) bi 0 i 0)
    (foreach p pts
      (if (< (setq d (distance cur p)) bd) (setq best p bd d bi i))
      (setq i (1+ i)))
    (setq res (cons best res) cur best pts (etq_remnth bi pts)))
  (reverse res))

(defun etq_ordenar (pts path p0)
  (if path
    (mapcar 'cdr
            (vl-sort (mapcar '(lambda (p) (cons (etq_dist-recorrido path p) p)) pts)
                     '(lambda (a b) (< (car a) (car b)))))
    (etq_vecino pts p0)))

;;; ---------------------------------------------------------------- evitar cruces
;; Circulo (x y r) que ocupa una etiqueta colocada en pt con estilo/direccion.
(defun etq_obs-de (pt est ang h rot / g s c n)
  (setq g (etq_geom est ang) s (/ h (etq_ah est)) c (cos rot) n (sin rot))
  (list (+ (car pt)  (* s (- (* (car g) c) (* (cadr g) n))))
        (+ (cadr pt) (* s (+ (* (car g) n) (* (cadr g) c))))
        (* s (caddr g))))

;; Obstaculo de una etiqueta ya existente en el dibujo.
(defun etq_obs-existente (en / ed pa o)
  (setq ed (entget en)
        pa (etq_parse (cdr (assoc 2 ed)))
        o  (vlax-ename->vla-object en))
  (if pa
    (etq_obs-de (cdr (assoc 10 ed)) (car pa) (cadr pa)
                (* (vla-get-XScaleFactor o) (etq_ah (car pa)))
                (vla-get-Rotation o))))

;; Holgura minima entre el circulo c y los obstaculos (cerca de h o mas = libre).
(defun etq_holgura (c h / m o dx dy lim d)
  (setq m 1.0e9)
  (foreach o *etq-obs*
    (setq lim (+ (caddr c) (caddr o) h)
          dx  (abs (- (car c) (car o))))
    (if (< dx lim)
      (progn
        (setq dy (abs (- (cadr c) (cadr o))))
        (if (< dy lim)
          (progn
            (setq d (- (sqrt (+ (* dx dx) (* dy dy))) (caddr c) (caddr o)))
            (if (< d m) (setq m d)))))))
  m)

;; Primera direccion (por preferencia) sin choque; si todas chocan, la de
;; mayor holgura.
(defun etq_mejor-dir (pt est h rot / ds a c cl gap bc best done)
  (setq ds (etq_dirs) gap (* 0.15 h) bc -1.0e9)
  (while (and ds (not done))
    (setq a  (car ds) ds (cdr ds)
          c  (etq_obs-de pt est a h rot)
          cl (etq_holgura c h))
    (cond ((>= cl gap) (setq best a done T))
          ((> cl bc)   (setq bc cl best a))))
  best)

;;; ---------------------------------------------------------------- insercion
;; Inserta una etiqueta: texto (string), estilo, direccion, h = altura del
;; numero, rot = giro, tipo = tipo de arbol (XDATA).
(defun etq_poner (msp pt txt est ang h rot tipo / nm blk s en)
  (setq nm (etq_blk est ang))
  (if (not (tblsearch "BLOCK" nm)) (etq_crear-bloque est ang))
  (setq s   (/ h (etq_ah est))
        blk (vla-InsertBlock msp (vlax-3d-point pt) nm s s s rot))
  (vla-put-Layer blk (etq_capa))
  (foreach a (vlax-invoke blk 'GetAttributes)
    (if (= (strcase (vla-get-TagString a)) "NUM")
      (vla-put-TextString a txt)))
  (if tipo
    (progn
      (setq en (vlax-vla-object->ename blk))
      (entmod (append (entget en)
                      (list (list -3 (list (etq_app) (cons 1000 tipo))))))))
  blk)

;; Elige direccion sin cruces, inserta y registra el obstaculo.
(defun etq_poner-auto (msp tipo txt pt rot h est / ang)
  (setq ang (etq_mejor-dir pt est h rot))
  (etq_poner msp pt txt est ang h rot tipo)
  (setq *etq-obs* (cons (etq_obs-de pt est ang h rot) *etq-obs*)))

;; Trae todas las etiquetas al frente (orden de dibujo).
(defun etq_frente (/ ss os)
  (if (setq ss (etq_etiquetas))
    (progn
      (setq os (getvar "CMDECHO"))
      (setvar "CMDECHO" 0)
      (command "_.DRAWORDER" ss "" "_F")
      (while (> (getvar "CMDACTIVE") 0) (command ""))
      (setvar "CMDECHO" os))))

;; Reconstruye todas las etiquetas existentes (conserva numero, tipo, posicion,
;; giro y altura) recalculando la direccion para evitar cruces. Si estn no es
;; nil, ademas cambia todas a ese estilo.
(defun etq_reconstruir (estn / ss i en ed o est0 items msp)
  (if (not (setq ss (etq_etiquetas)))
    (prompt "\nNo hay etiquetas.")
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq en   (ssname ss i) i (1+ i)
              o    (vlax-ename->vla-object en)
              est0 (etq_est-de o)
              items (cons (list (or (etq_tipo-de en) "")        ; 0 tipo
                                (etq_texto o)                   ; 1 numero
                                (etq_ins o)                     ; 2 posicion
                                (vla-get-Rotation o)            ; 3 giro
                                (* (vla-get-XScaleFactor o) (etq_ah est0)) ; 4 altura
                                (or estn est0))                 ; 5 estilo
                          items)))
      (setq items (vl-sort items
                           '(lambda (a b)
                              (if (= (car a) (car b))
                                (< (atoi (cadr a)) (atoi (cadr b)))
                                (< (car a) (car b))))))
      ;; borrar originales
      (setq i 0)
      (repeat (sslength ss) (entdel (ssname ss i)) (setq i (1+ i)))
      ;; obstaculos: los arboles (= puntos de insercion)
      (setq *etq-obs* (mapcar '(lambda (it) (list (car (nth 2 it)) (cadr (nth 2 it))
                                                  (* 0.3 (nth 4 it))))
                              items)
            msp (etq_modelspace))
      (foreach it items
        (etq_poner-auto msp (if (= (car it) "") nil (car it)) (cadr it)
                        (nth 2 it) (nth 3 it) (nth 4 it) (nth 5 it)))
      (etq_frente)
      (etq_regen)
      (prompt (strcat "\n" (itoa (length items)) " etiquetas reubicadas sin cruces.")))))

(defun etq_cada (ss f / i)
  (setq i 0)
  (repeat (sslength ss)
    (apply f (list (vlax-ename->vla-object (ssname ss i))))
    (setq i (1+ i))))

(defun etq_regen ()
  (vla-Regen (vla-get-ActiveDocument (vlax-get-acad-object)) 0))

;;; Texto horizontal en pantalla = girar el bloque el contrario del giro de vista.
(defun etq_angulo-vista ( / a)
  (setq a (- (getvar "VIEWTWIST")))
  (while (< a 0.0) (setq a (+ a (* 2.0 pi))))
  (while (>= a (* 2.0 pi)) (setq a (- a (* 2.0 pi))))
  a)

;;; Pide un angulo: numero, 2 puntos, "Vista" (alinea al giro de la vista)
;;; o -si kw lo incluye- "Incremento" (devuelve ese texto).
(defun etq_pedir-angulo (msg def kw / r)
  (initget kw)
  (setq r (getangle (strcat "\n" msg " ["
                            (if (wcmatch kw "*Incremento*") "Vista/Incremento" "Vista")
                            "] <" (angtos def 0 2) ">: ")))
  (cond ((null r) def)
        ((equal r "Vista") (etq_angulo-vista))
        (T r)))

(defun etq_pedir-estilo (def / r)
  (initget "Medallon Llamada Hexagono")
  (setq r (getkword (strcat "\nEstilo de etiqueta [Medallon/Llamada/Hexagono] <"
                            (nth (1- def) (etq_nombres)) ">: ")))
  (if r (1+ (vl-position r (etq_nombres))) def))

;;; ---------------------------------------------------------------- edicion en bloque
(defun etq_escalar (o s)
  (vla-put-XScaleFactor o s)
  (vla-put-YScaleFactor o s)
  (vla-put-ZScaleFactor o s))

(defun etq_aplicar-escala (ss / cur r f o)
  (setq o   (vlax-ename->vla-object (ssname ss 0))
        cur (* (vla-get-XScaleFactor o) (etq_ah (etq_est-de o))))
  (initget 6 "Factor")
  (setq r (getdist (strcat "\nAltura del numero [Factor] <" (rtos cur 2 2) ">: ")))
  (cond
    ((null r))
    ((equal r "Factor")
     (initget 6)
     (if (setq f (getreal "\nFactor de escala (2 = doble, 0.5 = mitad): "))
       (etq_cada ss '(lambda (o) (etq_escalar o (* f (vla-get-XScaleFactor o)))))))
    (T
     (setq *etq-h* r)
     (etq_cada ss '(lambda (o) (etq_escalar o (/ r (etq_ah (etq_est-de o)))))))))

(defun etq_aplicar-giro (ss / r d)
  (setq r (etq_pedir-angulo "Giro de TODAS las etiquetas" *etq-rot* "Vista Incremento"))
  (cond
    ((equal r "Incremento")
     (if (setq d (getangle "\nIncremento de giro <0>: "))
       (etq_cada ss '(lambda (o) (vla-put-Rotation o (+ (vla-get-Rotation o) d))))))
    (T
     (setq *etq-rot* r)
     (etq_cada ss '(lambda (o) (vla-put-Rotation o r))))))

;;; ---------------------------------------------------------------- comandos
(defun c:ETARB (/ ss grupos elegidos pe path p0 h r msp n tot tb tipo pts exist ini
                  trabajos ens)
  (etq_init)
  ;; 1) AREA
  (prompt "\nSeleccione el AREA con los arboles (ventana, cruce, WP, CP...): ")
  (if (not (setq ss (ssget '((-4 . "<OR") (0 . "INSERT") (0 . "POINT")
                              (0 . "AECC_COGO_POINT") (-4 . "OR>")))))
    (prompt "\nNada seleccionado.")
    (if (not (setq grupos (etq_agrupar ss)))
      (prompt "\nNo hay arboles (bloques o puntos) en el area.")
      (progn
        ;; 2) TIPO de punto
        (setq elegidos (etq_elegir grupos))
        ;; 3) otras opciones
        (setq pe (entsel "\nEje/polilinea del recorrido <Enter = indicar punto de inicio>: "))
        (if (and pe (etq_curva-p (car pe)))
          (setq path (car pe))
          (if (not (setq p0 (getpoint "\nPunto de inicio del recorrido: ")))
            (setq p0 (cadr (car elegidos)))))
        (setq *etq-est* (etq_pedir-estilo *etq-est*))
        (setq h (getdist (strcat "\nAltura del numero <" (rtos *etq-h* 2 2) ">: ")))
        (if h (setq *etq-h* h))
        (setq *etq-rot* (etq_pedir-angulo "Giro de las etiquetas" *etq-rot* "Vista"))
        (etq_abrir)
        ;; numeracion por tipo: continuar o reiniciar si ya hay etiquetas
        (foreach g elegidos
          (setq tipo (car g) pts (cdr g) ini 1 exist (etq_de-tipo tipo))
          (if exist
            (progn
              (initget "Continuar Reiniciar")
              (setq r (getkword (strcat "\n" tipo ": ya tiene " (itoa (length exist))
                                        " etiquetas. Numeracion [Continuar/Reiniciar] <Continuar>: ")))
              (if (equal r "Reiniciar")
                (foreach e exist (entdel e))
                (setq ini (1+ (etq_max-num exist))
                      pts (etq_sin-etiqueta pts exist)))))
          (setq trabajos (cons (list tipo pts ini) trabajos)))
        (setq trabajos (reverse trabajos))
        ;; obstaculos: todos los arboles del area + etiquetas que se conservan
        (setq *etq-obs* nil)
        (foreach g grupos
          (foreach p (cdr g)
            (setq *etq-obs* (cons (list (car p) (cadr p) (* 0.3 *etq-h*)) *etq-obs*))))
        (if (setq ens (etq_etiquetas))
          (etq_cada ens '(lambda (o)
                           (setq r (etq_obs-existente (vlax-vla-object->ename o)))
                           (if r (setq *etq-obs* (cons r *etq-obs*))))))
        ;; crear
        (setq msp (etq_modelspace) tot 0)
        (foreach tb trabajos
          (setq n (1- (caddr tb)))
          (foreach pt (etq_ordenar (cadr tb) path p0)
            (setq n (1+ n) tot (1+ tot))
            (etq_poner-auto msp (car tb) (itoa n) pt *etq-rot* *etq-h* *etq-est*))
          (prompt (strcat "\n  " (car tb) ": "
                          (itoa (length (cadr tb))) " etiqueta(s), numeradas "
                          (itoa (caddr tb)) "-" (itoa n))))
        (etq_frente)
        (etq_cerrar)
        (prompt (strcat "\nSe crearon " (itoa tot) " etiquetas en la capa " (etq_capa) ".")))))
  (princ))

(defun c:ETARBEDIT (/ ss)
  (etq_init)
  (if (setq ss (etq_etiquetas))
    (progn
      (etq_abrir)
      (prompt (strcat "\n" (itoa (sslength ss)) " etiquetas."))
      (etq_aplicar-escala ss)
      (etq_aplicar-giro ss)
      (etq_reconstruir nil)
      (etq_cerrar))
    (prompt "\nNo hay etiquetas."))
  (princ))

(defun c:ETARBESC (/ ss)
  (etq_init)
  (if (setq ss (etq_etiquetas))
    (progn (etq_abrir) (etq_aplicar-escala ss) (etq_reconstruir nil) (etq_cerrar))
    (prompt "\nNo hay etiquetas."))
  (princ))

(defun c:ETARBROT (/ ss)
  (etq_init)
  (if (setq ss (etq_etiquetas))
    (progn (etq_abrir) (etq_aplicar-giro ss) (etq_regen) (etq_cerrar))
    (prompt "\nNo hay etiquetas."))
  (princ))

;; Cambia el estilo de todas las etiquetas (conserva numero, tipo, posicion,
;; giro y altura) y las reubica sin cruces.
(defun c:ETARBESTILO ()
  (etq_init)
  (if (etq_etiquetas)
    (progn
      (setq *etq-est* (etq_pedir-estilo *etq-est*))
      (etq_abrir)
      (etq_reconstruir *etq-est*)
      (etq_cerrar))
    (prompt "\nNo hay etiquetas."))
  (princ))

(defun c:ETARBACOMODAR ()
  (etq_init)
  (if (etq_etiquetas)
    (progn (etq_abrir) (etq_reconstruir nil) (etq_cerrar))
    (prompt "\nNo hay etiquetas."))
  (princ))

(defun c:ETARBFRENTE ()
  (etq_init)
  (etq_abrir)
  (etq_frente)
  (etq_cerrar)
  (princ))

(defun c:ETARBBORRAR ()
  (etq_init)
  (etq_abrir)
  (prompt (strcat "\n" (itoa (etq_borrar)) " etiquetas borradas."))
  (etq_cerrar)
  (princ))

;;; Autocomprobacion de carga: avisa si alguna funcion no quedo definida
;;; (archivo truncado, copiado a medias, o error al cargar).
(setq *etq-faltan*
  (vl-remove-if-not
    '(lambda (f) (not (member (type (eval f)) '(USUBR SUBR))))
    '(etq_init etq_crear-bloque etq_geom etq_mascara etq_lwpv etq_emk etq_parse
      etq_etiquetas etq_tipo-de etq_de-tipo etq_borrar etq_texto etq_est-de
      etq_ins etq_max-num etq_sin-etiqueta etq_tipo etq_pos etq_agrupar etq_elegir
      etq_curva-p etq_dist-recorrido etq_ordenar etq_vecino etq_obs-de
      etq_obs-existente etq_holgura etq_mejor-dir etq_poner etq_poner-auto
      etq_frente etq_reconstruir etq_cada etq_regen etq_angulo-vista
      etq_pedir-angulo etq_pedir-estilo etq_escalar etq_aplicar-escala
      etq_aplicar-giro etq_abrir etq_cerrar
      c:ETARB c:ETARBEDIT c:ETARBESC c:ETARBROT c:ETARBESTILO c:ETARBACOMODAR
      c:ETARBFRENTE c:ETARBBORRAR)))

(if *etq-faltan*
  (prompt (strcat "\nATENCION: ETARB cargado INCOMPLETO. Faltan: "
                  (apply 'strcat (mapcar '(lambda (f) (strcat (vl-symbol-name f) " ")) *etq-faltan*))
                  "\nVuelva a copiar el archivo ETARB.lsp completo y cargue de nuevo."))
  (prompt "\nETARB v6 cargado: ETARB, ETARBEDIT, ETARBESC, ETARBROT, ETARBESTILO, ETARBACOMODAR, ETARBFRENTE, ETARBBORRAR."))
(princ)
