;;; ==========================================================================
;;; ETARB.lsp - Etiquetas de numeracion para arboles
;;;
;;;  ETARB      Crea las etiquetas (capa "Etiquetas"), numeradas por tipo
;;;             de arbol y siguiendo el orden de un recorrido.
;;;  ETARBROT   Gira TODAS las etiquetas en bloque (para layouts girados).
;;;  ETARBBORRAR Borra todas las etiquetas.
;;;
;;; Tipo de arbol (criterio de agrupacion):
;;;   - Bloque (INSERT)            -> nombre del bloque (nombre efectivo si es dinamico)
;;;   - Punto Civil 3D (COGO)      -> nombre del estilo de punto
;;;   - Punto AutoCAD (POINT)      -> capa del punto
;;;
;;; Cada etiqueta es UNA referencia de bloque "ETQ_ARB" (guia + doble circulo
;;; + atributo con el numero) insertada en la posicion del arbol. Al girar el
;;; bloque, guia y texto giran juntos alrededor del arbol.
;;; ==========================================================================
(vl-load-com)

(setq *etq-h*   (cond (*etq-h*   *etq-h*)   (T 1.5)))   ; altura del numero
(setq *etq-rot* (cond (*etq-rot* *etq-rot*) (T 0.0)))   ; giro (radianes)

(defun etq:capa  () "Etiquetas")
(defun etq:blk   () "ETQ_ARB")
(defun etq:estilo () "ETIQ_ARB")

;;; ---------------------------------------------------------------- infraestructura
(defun etq:init ()
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
  ;; Bloque: punto de arbol + guia a 45 grados + doble circulo + atributo NUM.
  ;; Entidades en capa 0 y color BYBLOCK: heredan la capa/color del insert.
  (if (not (tblsearch "BLOCK" (etq:blk)))
    (progn
      (entmake (list '(0 . "BLOCK") (cons 2 (etq:blk)) '(70 . 2) '(10 0.0 0.0 0.0)))
      ;; punto relleno en el arbol (polilinea con grosor = disco)
      (entmake '((0 . "LWPOLYLINE") (100 . "AcDbEntity") (8 . "0") (62 . 0)
                 (100 . "AcDbPolyline") (90 . 2) (70 . 1) (43 . 0.12)
                 (10 -0.06 0.0) (42 . 1.0) (10 0.06 0.0) (42 . 1.0)))
      ;; guia fina
      (entmake '((0 . "LINE") (8 . "0") (62 . 0)
                 (10 0.0 0.0 0.0) (11 0.80 0.80 0.0)))
      ;; doble circulo
      (entmake '((0 . "CIRCLE") (8 . "0") (62 . 0) (10 1.5 1.5 0.0) (40 . 1.0)))
      (entmake '((0 . "CIRCLE") (8 . "0") (62 . 0) (10 1.5 1.5 0.0) (40 . 0.9)))
      ;; numero centrado
      (entmake (list '(0 . "ATTDEF") '(100 . "AcDbEntity") '(8 . "0") '(62 . 0)
                     '(100 . "AcDbText") '(10 1.5 1.5 0.0) '(40 . 0.75) '(1 . "0")
                     '(50 . 0.0) '(41 . 1.0) '(51 . 0.0) (cons 7 (etq:estilo))
                     '(71 . 0) '(72 . 1) '(11 1.5 1.5 0.0)
                     '(100 . "AcDbAttributeDefinition") '(280 . 0)
                     '(3 . "Numero") '(2 . "NUM") '(70 . 0) '(73 . 0) '(74 . 2)))
      (entmake '((0 . "ENDBLK") (8 . "0"))))))

;;; ---------------------------------------------------------------- utilidades
(defun etq:modelspace ()
  (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object))))

(defun etq:etiquetas (/ ss)
  (ssget "_X" (list '(0 . "INSERT") (cons 2 (etq:blk)) (cons 8 (etq:capa))
                    '(410 . "Model"))))

(defun etq:borrar (/ ss i)
  (if (setq ss (etq:etiquetas))
    (progn (setq i 0)
           (repeat (sslength ss) (entdel (ssname ss i)) (setq i (1+ i)))
           (sslength ss))
    0))

(defun etq:tipo (en / ed typ o nm)
  (setq ed (entget en) typ (cdr (assoc 0 ed)))
  (cond
    ((= typ "INSERT")
     (setq o  (vlax-ename->vla-object en)
           nm (vl-catch-all-apply 'vla-get-EffectiveName (list o)))
     (if (or (vl-catch-all-error-p nm) (not nm) (= nm ""))
       (setq nm (cdr (assoc 2 ed))))
     (if (= (strcase nm) (etq:blk)) nil nm))
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

(defun etq:poner (msp pt n h rot / blk)
  (setq blk (vla-InsertBlock msp (vlax-3d-point pt) (etq:blk)
                             (/ h 0.75) (/ h 0.75) (/ h 0.75) rot))
  (vla-put-Layer blk (etq:capa))
  (foreach a (vlax-invoke blk 'GetAttributes)
    (if (= (strcase (vla-get-TagString a)) "NUM")
      (vla-put-TextString a (itoa n))))
  blk)

;;; Pide un angulo: numero, 2 puntos, o "Vista" (alinea al giro de la vista).
(defun etq:pedir-angulo (msg def / r)
  (initget "Vista")
  (setq r (getangle (strcat "\n" msg " [Vista] <" (angtos def 0 2) ">: ")))
  (cond ((null r) def)
        ((= r "Vista") (etq:angulo-vista))
        (T r)))

;; Texto horizontal en pantalla = girar el bloque el contrario del giro de vista.
(defun etq:angulo-vista ( / a)
  (setq a (- (getvar "VIEWTWIST")))
  (while (< a 0.0) (setq a (+ a (* 2.0 pi))))
  (while (>= a (* 2.0 pi)) (setq a (- a (* 2.0 pi))))
  a)

;;; ---------------------------------------------------------------- comandos
(defun c:ETARB (/ ss i en tipo pt grupos g sel pe path p0 h r ang msp n tot)
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
      (setq h (getdist (strcat "\nAltura del numero <" (rtos *etq-h* 2 2) ">: ")))
      (if h (setq *etq-h* h))
      (setq *etq-rot* (etq:pedir-angulo "Giro de las etiquetas" *etq-rot*))
      (initget "Si No")
      (setq r (getkword "\nBorrar etiquetas existentes? [Si/No] <Si>: "))
      (if (/= r "No") (etq:borrar))
      ;; crear
      (setq msp (etq:modelspace) tot 0)
      (foreach g (vl-sort grupos '(lambda (a b) (< (car a) (car b))))
        (setq n 0)
        (foreach pt (etq:ordenar (cdr g) path p0)
          (setq n (1+ n) tot (1+ tot))
          (etq:poner msp pt n *etq-h* *etq-rot*))
        (prompt (strcat "\n  " (car g) ": " (itoa n) " arbol(es), numerados 1-" (itoa n))))
      (prompt (strcat "\nSe crearon " (itoa tot) " etiquetas en la capa " (etq:capa) "."))
      (princ))))

(defun c:ETARBROT (/ ss i ang)
  (if (not (setq ss (etq:etiquetas)))
    (prompt "\nNo hay etiquetas.")
    (progn
      (setq ang (etq:pedir-angulo "Nuevo giro de TODAS las etiquetas" *etq-rot*))
      (setq *etq-rot* ang i 0)
      (repeat (sslength ss)
        (vla-put-Rotation (vlax-ename->vla-object (ssname ss i)) ang)
        (setq i (1+ i)))
      (vla-Regen (vla-get-ActiveDocument (vlax-get-acad-object)) 0)
      (prompt (strcat "\n" (itoa (sslength ss)) " etiquetas giradas."))))
  (princ))

(defun c:ETARBBORRAR ()
  (prompt (strcat "\n" (itoa (etq:borrar)) " etiquetas borradas."))
  (princ))

(prompt "\nETARB cargado: ETARB (crear), ETARBROT (girar en bloque), ETARBBORRAR.")
(princ)
