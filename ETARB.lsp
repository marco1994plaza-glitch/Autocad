;;; ==========================================================================
;;; ETARB.lsp - Etiquetas de numeracion para arboles
;;;
;;;  ETARB        Flujo: 1) seleccionar AREA  2) elegir TIPO de punto
;;;               3) recorrido, estilo, altura, giro  -> crea las etiquetas
;;;               (capa "Etiquetas"), numeradas 1..n por tipo siguiendo el
;;;               recorrido.
;;;  ETARBEDIT    Edita ESCALA y GIRO de todas las etiquetas de una vez.
;;;  ETARBESC     Solo escala (altura del numero o factor).
;;;  ETARBROT     Solo giro (angulo, 2 puntos, Vista o Incremento).
;;;  ETARBESTILO  Cambia el estilo de todas las etiquetas existentes.
;;;  ETARBBORRAR  Borra todas las etiquetas.
;;;
;;; Estilos:  1 Medallon  (aguja ahusada + anillo grueso/fino)
;;;           2 Llamada   (aguja ahusada + linea base ahusada)
;;;           3 Hexagono  (aguja ahusada + doble hexagono)
;;;
;;; Tipo de arbol (criterio de agrupacion):
;;;   - Bloque (INSERT)            -> nombre del bloque (nombre efectivo si es dinamico)
;;;   - Punto Civil 3D (COGO)      -> nombre del estilo de punto
;;;   - Punto AutoCAD (POINT)      -> capa del punto
;;;
;;; Cada etiqueta es UNA referencia de bloque (aguja + marco + atributo NUM)
;;; insertada en la posicion del arbol, con el tipo guardado como XDATA
;;; ("ETARB"). Escala y giro actuan alrededor del arbol.
;;; ==========================================================================
(vl-load-com)

(setq *etq-h*   (cond (*etq-h*   *etq-h*)   (T 1.5)))   ; altura del numero
(setq *etq-rot* (cond (*etq-rot* *etq-rot*) (T 0.0)))   ; giro (radianes)
(setq *etq-est* (cond (*etq-est* *etq-est*) (T 1)))     ; estilo 1..3

(defun etq_capa   () "Etiquetas")
(defun etq_estilo () "ETIQ_ARB")
(defun etq_app    () "ETARB")
(defun etq_nombres () '("Medallon" "Llamada" "Hexagono"))

;; (bloque  altura-atributo  x y del texto  justif.vertical 74)
(defun etq_bloques ()
  '(("ETQ_MEDALLON" 0.75 1.55 1.55 2)
    ("ETQ_LLAMADA"  0.9  2.05 1.05 1)
    ("ETQ_HEXAGONO" 0.7  1.6  1.6  2)))
(defun etq_blk (n) (car  (nth (1- n) (etq_bloques))))
(defun etq_ah  (n) (cadr (nth (1- n) (etq_bloques))))

;;; ---------------------------------------------------------------- geometria del bloque
;; verts: lista de (x y ancho-ini ancho-fin bulge)
(defun etq_lwpv (verts cerrada)
  (entmake
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
  (entmake (list '(0 . "CIRCLE") '(8 . "0") '(62 . 0) (list 10 cx cy 0.0) (cons 40 r))))

;; aguja ahusada desde el arbol (0,0) hasta (x,y); termina con espesor w
(defun etq_aguja (x y w)
  (etq_lwpv (list (list 0.0 0.0 0.0 w 0.0) (list x y 0.0 0.0 0.0)) nil))

;; poligono regular de n lados, radio r, primer vertice a 0 grados
(defun etq_poligono (cx cy r n w / k a pts)
  (setq k 0)
  (repeat n
    (setq a   (* k (/ (* 2.0 pi) n))
          pts (cons (list (+ cx (* r (cos a))) (+ cy (* r (sin a))) w w 0.0) pts)
          k   (1+ k)))
  (etq_lwpv (reverse pts) T))

(defun etq_crear-bloque (n / d cx cy dc tip a hit s2)
  (setq d (nth (1- n) (etq_bloques)) s2 (sqrt 2.0))
  (entmake (list '(0 . "BLOCK") (cons 2 (car d)) '(70 . 2) '(10 0.0 0.0 0.0)))
  (etq_disco 0.0 0.0 0.12)                       ; marca del arbol
  (cond
    ((= n 1)   ; MEDALLON
     (setq cx 1.55 cy 1.55 dc (* cx s2) tip (/ (- dc 1.0) s2))
     (etq_aguja tip tip 0.10)
     (etq_anillo cx cy 1.0 0.10)
     (etq_circulo cx cy 0.84))
    ((= n 2)   ; LLAMADA
     (etq_aguja 0.9 0.9 0.08)
     (etq_lwpv '((0.9 0.9 0.05 0.0 0.0) (3.2 0.9 0.0 0.0 0.0)) nil))
    ((= n 3)   ; HEXAGONO
     (setq cx 1.6 cy 1.6 dc (* cx s2)
           a   (* 1.05 (cos (/ pi 6.0)))
           hit (/ a (cos (/ pi 12.0)))
           tip (/ (- dc hit) s2))
     (etq_aguja tip tip 0.10)
     (etq_poligono cx cy 1.05 6 0.10)
     (etq_poligono cx cy 0.90 6 0.0)))
  ;; numero
  (entmake (list '(0 . "ATTDEF") '(100 . "AcDbEntity") '(8 . "0") '(62 . 0)
                 '(100 . "AcDbText") (list 10 (nth 2 d) (nth 3 d) 0.0)
                 (cons 40 (cadr d)) '(1 . "0")
                 '(50 . 0.0) '(41 . 1.0) '(51 . 0.0) (cons 7 (etq_estilo))
                 '(71 . 0) '(72 . 1) (list 11 (nth 2 d) (nth 3 d) 0.0)
                 '(100 . "AcDbAttributeDefinition") '(280 . 0)
                 '(3 . "Numero") '(2 . "NUM") '(70 . 0) '(73 . 0)
                 (cons 74 (nth 4 d))))
  (entmake '((0 . "ENDBLK") (8 . "0"))))

(defun etq_init (/ n)
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
                   '(-3 ("ACAD" (1000 . "Century Gothic") (1071 . 0))))))
  ;; Bloques de los 3 estilos (entidades en capa 0 / BYBLOCK)
  (setq n 1)
  (repeat 3
    (if (not (tblsearch "BLOCK" (etq_blk n))) (etq_crear-bloque n))
    (setq n (1+ n))))

;;; ---------------------------------------------------------------- utilidades
(defun etq_modelspace ()
  (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object))))

(defun etq_etiquetas ()
  (ssget "_X" (list '(0 . "INSERT")
                    (cons 2 (strcat (etq_blk 1) "," (etq_blk 2) "," (etq_blk 3)))
                    (cons 8 (etq_capa)) '(410 . "Model"))))

;; Tipo de arbol guardado en la etiqueta (XDATA).
(defun etq_tipo-de (en / x)
  (setq x (assoc -3 (entget en (list (etq_app)))))
  (if x (cdr (assoc 1000 (cdr (cadr x))))))

(defun etq_borrar-tipos (nombres / ss i en n)
  (setq n 0)
  (if (setq ss (etq_etiquetas))
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq en (ssname ss i) i (1+ i))
        (if (member (etq_tipo-de en) nombres)
          (progn (entdel en) (setq n (1+ n)))))))
  n)

(defun etq_borrar (/ ss i)
  (if (setq ss (etq_etiquetas))
    (progn (setq i 0)
           (repeat (sslength ss) (entdel (ssname ss i)) (setq i (1+ i)))
           (sslength ss))
    0))

(defun etq_es-etiqueta (nm)
  (member (strcase nm) (list (etq_blk 1) (etq_blk 2) (etq_blk 3))))

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

;; Inserta una etiqueta: texto (string), estilo 1..3, h = altura del numero,
;; tipo = nombre del tipo de arbol (se guarda como XDATA).
(defun etq_poner (msp pt txt est h rot tipo / blk s en)
  (setq s   (/ h (etq_ah est))
        blk (vla-InsertBlock msp (vlax-3d-point pt) (etq_blk est) s s s rot))
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

(defun etq_texto (o / r)
  (foreach a (vlax-invoke o 'GetAttributes)
    (if (= (strcase (vla-get-TagString a)) "NUM")
      (setq r (vla-get-TextString a))))
  (or r ""))

;; Estilo (1..3) de una referencia de etiqueta.
(defun etq_est-de (o / p)
  (setq p (vl-position (strcase (vla-get-Name o)) (mapcar 'car (etq_bloques))))
  (1+ (or p 0)))

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
(defun c:ETARB (/ ss grupos elegidos pe path p0 h r msp n tot)
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
        (initget "Si No")
        (setq r (getkword "\nReemplazar etiquetas anteriores de estos tipos? [Si/No] <Si>: "))
        (if (/= r "No") (etq_borrar-tipos (mapcar 'car elegidos)))
        ;; crear
        (setq msp (etq_modelspace) tot 0)
        (foreach g elegidos
          (setq n 0)
          (foreach pt (etq_ordenar (cdr g) path p0)
            (setq n (1+ n) tot (1+ tot))
            (etq_poner msp pt (itoa n) *etq-est* *etq-h* *etq-rot* (car g)))
          (prompt (strcat "\n  " (car g) ": " (itoa n) " arbol(es), numerados 1-" (itoa n))))
        (prompt (strcat "\nSe crearon " (itoa tot) " etiquetas en la capa " (etq_capa) ".")))))
  (princ))

(defun c:ETARBEDIT (/ ss)
  (if (setq ss (etq_etiquetas))
    (progn
      (prompt (strcat "\n" (itoa (sslength ss)) " etiquetas."))
      (etq_aplicar-escala ss)
      (etq_aplicar-giro ss)
      (etq_regen))
    (prompt "\nNo hay etiquetas."))
  (princ))

(defun c:ETARBESC (/ ss)
  (if (setq ss (etq_etiquetas))
    (progn (etq_aplicar-escala ss) (etq_regen))
    (prompt "\nNo hay etiquetas."))
  (princ))

(defun c:ETARBROT (/ ss)
  (if (setq ss (etq_etiquetas))
    (progn (etq_aplicar-giro ss) (etq_regen))
    (prompt "\nNo hay etiquetas."))
  (princ))

;; Reemplaza cada etiqueta por otra de distinto estilo conservando numero,
;; tipo, posicion, giro y altura del numero.
(defun c:ETARBESTILO (/ ss i en ed o pt rot h txt tipo msp)
  (etq_init)
  (if (not (setq ss (etq_etiquetas)))
    (prompt "\nNo hay etiquetas.")
    (progn
      (setq *etq-est* (etq_pedir-estilo *etq-est*)
            msp (etq_modelspace) i 0)
      (repeat (sslength ss)
        (setq en   (ssname ss i) i (1+ i)
              ed   (entget en)
              o    (vlax-ename->vla-object en)
              pt   (cdr (assoc 10 ed))
              rot  (cdr (assoc 50 ed))
              h    (* (cdr (assoc 41 ed)) (etq_ah (etq_est-de o)))
              txt  (etq_texto o)
              tipo (etq_tipo-de en))
        (etq_poner msp pt txt *etq-est* h rot tipo)
        (vla-Delete o))
      (etq_regen)
      (prompt (strcat "\nEstilo " (nth (1- *etq-est*) (etq_nombres)) " aplicado a "
                      (itoa (sslength ss)) " etiquetas."))))
  (princ))

(defun c:ETARBBORRAR ()
  (prompt (strcat "\n" (itoa (etq_borrar)) " etiquetas borradas."))
  (princ))

;;; Autocomprobacion de carga: avisa si alguna funcion no quedo definida
;;; (archivo truncado, copiado a medias, o error al cargar).
(setq *etq-faltan*
  (vl-remove-if-not
    '(lambda (f) (not (member (type (eval f)) '(USUBR SUBR))))
    '(etq_init etq_crear-bloque etq_lwpv etq_disco etq_anillo etq_circulo
      etq_aguja etq_poligono etq_etiquetas etq_tipo-de etq_borrar-tipos
      etq_borrar etq_tipo etq_pos etq_agrupar etq_elegir etq_curva-p
      etq_dist-recorrido etq_ordenar etq_vecino etq_poner etq_texto
      etq_est-de etq_cada etq_regen etq_angulo-vista etq_pedir-angulo
      etq_pedir-estilo etq_escalar etq_aplicar-escala etq_aplicar-giro
      c:ETARB c:ETARBEDIT c:ETARBESC c:ETARBROT c:ETARBESTILO c:ETARBBORRAR)))

(if *etq-faltan*
  (prompt (strcat "\nATENCION: ETARB cargado INCOMPLETO. Faltan: "
                  (apply 'strcat (mapcar '(lambda (f) (strcat (vl-symbol-name f) " ")) *etq-faltan*))
                  "\nVuelva a copiar el archivo ETARB.lsp completo y cargue de nuevo."))
  (prompt "\nETARB v5 cargado: ETARB, ETARBEDIT, ETARBESC, ETARBROT, ETARBESTILO, ETARBBORRAR."))
(princ)
