;;; ==========================================================================
;;; ETARB.lsp - Etiquetas de numeracion para arboles
;;;
;;;  ETARB        Crea las etiquetas (capa "Etiquetas"), numeradas por tipo
;;;               de arbol y siguiendo el orden de un recorrido.
;;;  ETARBEDIT    Edita ESCALA y GIRO de todas las etiquetas de una vez.
;;;  ETARBESC     Solo escala (altura del numero o factor).
;;;  ETARBROT     Solo giro (angulo, 2 puntos, Vista o Incremento).
;;;  ETARBESTILO  Cambia el estilo de todas las etiquetas existentes.
;;;  ETARBBORRAR  Borra todas las etiquetas.
;;;
;;; Estilos:  1 Clasico (guia + doble circulo)
;;;           2 Llamada (guia + numero sobre linea base)
;;;           3 Rombo   (guia + doble rombo)
;;;
;;; Tipo de arbol (criterio de agrupacion):
;;;   - Bloque (INSERT)            -> nombre del bloque (nombre efectivo si es dinamico)
;;;   - Punto Civil 3D (COGO)      -> nombre del estilo de punto
;;;   - Punto AutoCAD (POINT)      -> capa del punto
;;;
;;; Cada etiqueta es UNA referencia de bloque (guia + marco + atributo NUM)
;;; insertada en la posicion del arbol. Escala y giro se aplican alrededor del
;;; arbol, asi guia y numero se mueven juntos.
;;; ==========================================================================
(vl-load-com)

(setq *etq-h*   (cond (*etq-h*   *etq-h*)   (T 1.5)))   ; altura del numero
(setq *etq-rot* (cond (*etq-rot* *etq-rot*) (T 0.0)))   ; giro (radianes)
(setq *etq-est* (cond (*etq-est* *etq-est*) (T 1)))     ; estilo 1..3

(defun etq:capa   () "Etiquetas")
(defun etq:estilo () "ETIQ_ARB")
(defun etq:nombres () '("Clasico" "Llamada" "Rombo"))

;; (bloque  altura-atributo  x y del texto  justif.vertical 74)
(defun etq:bloques ()
  '(("ETQ_ARB"  0.75 1.5  1.5  2)
    ("ETQ_ARB2" 0.9  2.1  1.05 1)
    ("ETQ_ARB3" 0.7  1.29 1.29 2)))
(defun etq:blk (n) (car  (nth (1- n) (etq:bloques))))
(defun etq:ah  (n) (cadr (nth (1- n) (etq:bloques))))

;;; ---------------------------------------------------------------- infraestructura
(defun etq:lwp (pts cerrada ancho)
  (entmake (append (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(8 . "0") '(62 . 0)
                         '(100 . "AcDbPolyline") (cons 90 (length pts))
                         (cons 70 (if cerrada 1 0)))
                   (if ancho (list (cons 43 ancho)) nil)
                   (mapcar '(lambda (p) (cons 10 p)) pts))))

(defun etq:circulo (cx cy r)
  (entmake (list '(0 . "CIRCLE") '(8 . "0") '(62 . 0) (list 10 cx cy 0.0) (cons 40 r))))

(defun etq:crear-bloque (n / d)
  (setq d (nth (1- n) (etq:bloques)))
  (entmake (list '(0 . "BLOCK") (cons 2 (car d)) '(70 . 2) '(10 0.0 0.0 0.0)))
  ;; punto relleno en el arbol (polilinea con grosor = disco)
  (entmake '((0 . "LWPOLYLINE") (100 . "AcDbEntity") (8 . "0") (62 . 0)
             (100 . "AcDbPolyline") (90 . 2) (70 . 1) (43 . 0.12)
             (10 -0.06 0.0) (42 . 1.0) (10 0.06 0.0) (42 . 1.0)))
  (cond
    ((= n 1)   ; guia + doble circulo
     (etq:lwp '((0.0 0.0) (0.80 0.80)) nil nil)
     (etq:circulo 1.5 1.5 1.0)
     (etq:circulo 1.5 1.5 0.9))
    ((= n 2)   ; guia + linea base horizontal
     (etq:lwp '((0.0 0.0) (0.9 0.9) (3.3 0.9)) nil nil))
    ((= n 3)   ; guia + doble rombo
     (etq:lwp '((0.0 0.0) (0.64 0.64)) nil nil)
     (etq:lwp '((-0.01 1.29) (1.29 -0.01) (2.59 1.29) (1.29 2.59)) T nil)
     (etq:lwp '((0.14 1.29) (1.29 0.14) (2.44 1.29) (1.29 2.44)) T nil)))
  ;; numero
  (entmake (list '(0 . "ATTDEF") '(100 . "AcDbEntity") '(8 . "0") '(62 . 0)
                 '(100 . "AcDbText") (list 10 (nth 2 d) (nth 3 d) 0.0)
                 (cons 40 (cadr d)) '(1 . "0")
                 '(50 . 0.0) '(41 . 1.0) '(51 . 0.0) (cons 7 (etq:estilo))
                 '(71 . 0) '(72 . 1) (list 11 (nth 2 d) (nth 3 d) 0.0)
                 '(100 . "AcDbAttributeDefinition") '(280 . 0)
                 '(3 . "Numero") '(2 . "NUM") '(70 . 0) '(73 . 0)
                 (cons 74 (nth 4 d))))
  (entmake '((0 . "ENDBLK") (8 . "0"))))

(defun etq:init (/ n)
  ;; Capa
  (if (not (tblsearch "LAYER" (etq:capa)))
    (entmake (list '(0 . "LAYER") '(100 . "AcDbSymbolTableRecord")
                   '(100 . "AcDbLayerTableRecord") (cons 2 (etq:capa))
                   '(70 . 0) '(62 . 7) '(6 . "Continuous"))))
  ;; Estilo de texto (Century Gothic; si no existe, Windows sustituye)
  (if (not (tblsearch "STYLE" (etq:estilo)))
    (entmake (list '(0 . "STYLE") '(100 . "AcDbSymbolTableRecord")
                   '(100 . "AcDbTextStyleTableRecord") (cons 2 (etq:estilo))
                   '(70 . 0) '(40 . 0.0) '(41 . 1.0) '(50 . 0.0) '(71 . 0)
                   '(42 . 1.0) '(3 . "GOTHIC.TTF") '(4 . "")
                   '(-3 ("ACAD" (1000 . "Century Gothic") (1071 . 0))))))
  ;; Bloques de los 3 estilos (entidades en capa 0 / BYBLOCK)
  (setq n 1)
  (repeat 3
    (if (not (tblsearch "BLOCK" (etq:blk n))) (etq:crear-bloque n))
    (setq n (1+ n))))

;;; ---------------------------------------------------------------- utilidades
(defun etq:modelspace ()
  (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object))))

(defun etq:etiquetas ()
  (ssget "_X" (list '(0 . "INSERT")
                    (cons 2 (strcat (etq:blk 1) "," (etq:blk 2) "," (etq:blk 3)))
                    (cons 8 (etq:capa)) '(410 . "Model"))))

(defun etq:borrar (/ ss i)
  (if (setq ss (etq:etiquetas))
    (progn (setq i 0)
           (repeat (sslength ss) (entdel (ssname ss i)) (setq i (1+ i)))
           (sslength ss))
    0))

(defun etq:es-etiqueta (nm)
  (member (strcase nm) (list (etq:blk 1) (etq:blk 2) (etq:blk 3))))

(defun etq:tipo (en / ed typ o nm)
  (setq ed (entget en) typ (cdr (assoc 0 ed)))
  (cond
    ((= typ "INSERT")
     (setq o  (vlax-ename->vla-object en)
           nm (vl-catch-all-apply 'vla-get-EffectiveName (list o)))
     (if (or (vl-catch-all-error-p nm) (not nm) (= nm ""))
       (setq nm (cdr (assoc 2 ed))))
     (if (etq:es-etiqueta nm) nil nm))
    ((= typ "POINT") (strcat "PUNTO [" (cdr (assoc 8 ed)) "]"))
    (T ;; AECC_COGO_POINT (Civil 3D)
     (setq o  (vlax-ename->vla-object en)
           nm (vl-catch-all-apply
                '(lambda () (vlax-get-property (vlax-get-property o 'Style) 'Name))))
     (if (or (vl-catch-all-error-p nm) (not nm)) "PUNTO COGO" nm))))

(defun etq:pos (en / ed typ o x y)
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

(defun etq:curva-p (en)
  (and en (not (vl-catch-all-error-p
                 (vl-catch-all-apply 'vlax-curve-getEndParam (list en))))))

(defun etq:dist-recorrido (path pt)
  (vlax-curve-getDistAtPoint path (vlax-curve-getClosestPointTo path pt)))

(defun etq:remnth (n l / i r)
  (setq i 0)
  (foreach x l (if (/= i n) (setq r (cons x r))) (setq i (1+ i)))
  (reverse r))

;; Sin eje: cadena de vecino mas cercano desde el punto de inicio.
(defun etq:vecino (pts p0 / res cur best bd bi i d)
  (setq cur p0)
  (while pts
    (setq best (car pts) bd (distance cur best) bi 0 i 0)
    (foreach p pts
      (if (< (setq d (distance cur p)) bd) (setq best p bd d bi i))
      (setq i (1+ i)))
    (setq res (cons best res) cur best pts (etq:remnth bi pts)))
  (reverse res))

(defun etq:ordenar (pts path p0)
  (if path
    (mapcar 'cdr
            (vl-sort (mapcar '(lambda (p) (cons (etq:dist-recorrido path p) p)) pts)
                     '(lambda (a b) (< (car a) (car b)))))
    (etq:vecino pts p0)))

;; Inserta una etiqueta: texto (string), estilo 1..3, h = altura del numero.
(defun etq:poner (msp pt txt est h rot / blk s)
  (setq s   (/ h (etq:ah est))
        blk (vla-InsertBlock msp (vlax-3d-point pt) (etq:blk est) s s s rot))
  (vla-put-Layer blk (etq:capa))
  (foreach a (vlax-invoke blk 'GetAttributes)
    (if (= (strcase (vla-get-TagString a)) "NUM")
      (vla-put-TextString a txt)))
  blk)

(defun etq:texto (o / r)
  (foreach a (vlax-invoke o 'GetAttributes)
    (if (= (strcase (vla-get-TagString a)) "NUM")
      (setq r (vla-get-TextString a))))
  (or r ""))

;; Estilo (1..3) de una referencia de etiqueta.
(defun etq:est-de (o / p)
  (setq p (vl-position (strcase (vla-get-Name o)) (mapcar 'car (etq:bloques))))
  (1+ (or p 0)))

(defun etq:cada (ss f / i)
  (setq i 0)
  (repeat (sslength ss)
    (apply f (list (vlax-ename->vla-object (ssname ss i))))
    (setq i (1+ i))))

(defun etq:regen ()
  (vla-Regen (vla-get-ActiveDocument (vlax-get-acad-object)) 0))

;;; Texto horizontal en pantalla = girar el bloque el contrario del giro de vista.
(defun etq:angulo-vista ( / a)
  (setq a (- (getvar "VIEWTWIST")))
  (while (< a 0.0) (setq a (+ a (* 2.0 pi))))
  (while (>= a (* 2.0 pi)) (setq a (- a (* 2.0 pi))))
  a)

;;; Pide un angulo: numero, 2 puntos, "Vista" (alinea al giro de la vista)
;;; o -si kw lo incluye- "Incremento" (devuelve ese texto).
(defun etq:pedir-angulo (msg def kw / r)
  (initget kw)
  (setq r (getangle (strcat "\n" msg " ["
                            (if (wcmatch kw "*Incremento*") "Vista/Incremento" "Vista")
                            "] <" (angtos def 0 2) ">: ")))
  (cond ((null r) def)
        ((equal r "Vista") (etq:angulo-vista))
        (T r)))

(defun etq:pedir-estilo (def / r)
  (initget "Clasico Llamada Rombo")
  (setq r (getkword (strcat "\nEstilo de etiqueta [Clasico/Llamada/Rombo] <"
                            (nth (1- def) (etq:nombres)) ">: ")))
  (if r (1+ (vl-position r (etq:nombres))) def))

;;; ---------------------------------------------------------------- edicion en bloque
(defun etq:escalar (o s)
  (vla-put-XScaleFactor o s)
  (vla-put-YScaleFactor o s)
  (vla-put-ZScaleFactor o s))

(defun etq:aplicar-escala (ss / cur r f o)
  (setq o   (vlax-ename->vla-object (ssname ss 0))
        cur (* (vla-get-XScaleFactor o) (etq:ah (etq:est-de o))))
  (initget 6 "Factor")
  (setq r (getdist (strcat "\nAltura del numero [Factor] <" (rtos cur 2 2) ">: ")))
  (cond
    ((null r))
    ((equal r "Factor")
     (initget 6)
     (if (setq f (getreal "\nFactor de escala (2 = doble, 0.5 = mitad): "))
       (etq:cada ss '(lambda (o) (etq:escalar o (* f (vla-get-XScaleFactor o)))))))
    (T
     (setq *etq-h* r)
     (etq:cada ss '(lambda (o) (etq:escalar o (/ r (etq:ah (etq:est-de o)))))))))

(defun etq:aplicar-giro (ss / r d)
  (setq r (etq:pedir-angulo "Giro de TODAS las etiquetas" *etq-rot* "Vista Incremento"))
  (cond
    ((equal r "Incremento")
     (if (setq d (getangle "\nIncremento de giro <0>: "))
       (etq:cada ss '(lambda (o) (vla-put-Rotation o (+ (vla-get-Rotation o) d))))))
    (T
     (setq *etq-rot* r)
     (etq:cada ss '(lambda (o) (vla-put-Rotation o r))))))

;;; ---------------------------------------------------------------- comandos
(defun c:ETARB (/ ss i en tipo pt grupos g pe path p0 h r msp n tot)
  (etq:init)
  (prompt "\nSeleccione los arboles (bloques / puntos): ")
  (if (not (setq ss (ssget '((-4 . "<OR") (0 . "INSERT") (0 . "POINT")
                              (0 . "AECC_COGO_POINT") (-4 . "OR>")))))
    (progn (prompt "\nNada seleccionado.") (princ))
    (progn
      ;; agrupar por tipo
      (setq i 0)
      (repeat (sslength ss)
        (setq en (ssname ss i) i (1+ i)
              tipo (etq:tipo en))
        (if tipo
          (progn
            (setq pt (etq:pos en))
            (if (setq g (assoc tipo grupos))
              (setq grupos (subst (cons tipo (cons pt (cdr g))) g grupos))
              (setq grupos (cons (list tipo pt) grupos))))))
      ;; recorrido
      (setq pe (entsel "\nEje/polilinea del recorrido <Enter = indicar punto de inicio>: "))
      (if (and pe (etq:curva-p (car pe)))
        (setq path (car pe))
        (if (not (setq p0 (getpoint "\nPunto de inicio del recorrido: ")))
          (setq p0 (etq:pos (ssname ss 0)))))
      ;; parametros
      (setq *etq-est* (etq:pedir-estilo *etq-est*))
      (setq h (getdist (strcat "\nAltura del numero <" (rtos *etq-h* 2 2) ">: ")))
      (if h (setq *etq-h* h))
      (setq *etq-rot* (etq:pedir-angulo "Giro de las etiquetas" *etq-rot* "Vista"))
      (initget "Si No")
      (setq r (getkword "\nBorrar etiquetas existentes? [Si/No] <Si>: "))
      (if (/= r "No") (etq:borrar))
      ;; crear
      (setq msp (etq:modelspace) tot 0)
      (foreach g (vl-sort grupos '(lambda (a b) (< (car a) (car b))))
        (setq n 0)
        (foreach pt (etq:ordenar (cdr g) path p0)
          (setq n (1+ n) tot (1+ tot))
          (etq:poner msp pt (itoa n) *etq-est* *etq-h* *etq-rot*))
        (prompt (strcat "\n  " (car g) ": " (itoa n) " arbol(es), numerados 1-" (itoa n))))
      (prompt (strcat "\nSe crearon " (itoa tot) " etiquetas en la capa " (etq:capa) "."))
      (princ))))

(defun c:ETARBEDIT (/ ss)
  (if (setq ss (etq:etiquetas))
    (progn
      (prompt (strcat "\n" (itoa (sslength ss)) " etiquetas."))
      (etq:aplicar-escala ss)
      (etq:aplicar-giro ss)
      (etq:regen))
    (prompt "\nNo hay etiquetas."))
  (princ))

(defun c:ETARBESC (/ ss)
  (if (setq ss (etq:etiquetas))
    (progn (etq:aplicar-escala ss) (etq:regen))
    (prompt "\nNo hay etiquetas."))
  (princ))

(defun c:ETARBROT (/ ss)
  (if (setq ss (etq:etiquetas))
    (progn (etq:aplicar-giro ss) (etq:regen))
    (prompt "\nNo hay etiquetas."))
  (princ))

;; Reemplaza cada etiqueta por otra de distinto estilo conservando numero,
;; posicion, giro y altura del numero.
(defun c:ETARBESTILO (/ ss i en ed o pt rot h txt msp)
  (etq:init)
  (if (not (setq ss (etq:etiquetas)))
    (prompt "\nNo hay etiquetas.")
    (progn
      (setq *etq-est* (etq:pedir-estilo *etq-est*)
            msp (etq:modelspace) i 0)
      (repeat (sslength ss)
        (setq en  (ssname ss i) i (1+ i)
              ed  (entget en)
              o   (vlax-ename->vla-object en)
              pt  (cdr (assoc 10 ed))
              rot (cdr (assoc 50 ed))
              h   (* (cdr (assoc 41 ed)) (etq:ah (etq:est-de o)))
              txt (etq:texto o))
        (etq:poner msp pt txt *etq-est* h rot)
        (vla-Delete o))
      (etq:regen)
      (prompt (strcat "\nEstilo " (nth (1- *etq-est*) (etq:nombres)) " aplicado a "
                      (itoa (sslength ss)) " etiquetas."))))
  (princ))

(defun c:ETARBBORRAR ()
  (prompt (strcat "\n" (itoa (etq:borrar)) " etiquetas borradas."))
  (princ))

(prompt "\nETARB cargado: ETARB, ETARBEDIT, ETARBESC, ETARBROT, ETARBESTILO, ETARBBORRAR.")
(princ)
