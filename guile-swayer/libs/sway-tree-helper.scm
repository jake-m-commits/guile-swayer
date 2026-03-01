;; This is a helper library that provides functions to simplify
;; dealing with a sway tree record

(define-module (guile-swayer libs sway-tree-helper)
  #:use-module (guile-swayer swayipc)

  #:export (sway-tree-node-parent
            sway-tree-node-workspace
            sway-tree-node-focused
            sway-tree-node-find
            sway-tree-remove-container
            sway-tree-nodes-optimize
            sway-tree-move-node
            sway-tree-nodes-flat
            sway-tree-nodes-apps
			sway-move-container-under-container
            sway-tree-print))

(define (list-or lst)
  "Return first non false item from a list LST"
  (cond
    ((null? lst) #f)
    ((car lst) (car lst))
    (else (list-or (cdr lst)))))

(define* (sway-tree-node-find id #:optional (tree (sway-get-tree)))
  "Find the node of a given ID, a sway TREE can be
optionally passed, default value is sway's current tree"
  (define* (sway-tree-node-find-loop stree #:optional parent)
    (cond
     ((null? stree) #f)
     ((equal? (sway-tree-id stree) id) stree)
     ((null? (sway-tree-nodes stree)) #f)
     (else (list-or (map (lambda (n) (sway-tree-node-find-loop n stree))
                         (sway-tree-nodes stree))))))

  (sway-tree-node-find-loop tree))

(define* (sway-tree-node-parent node #:optional (tree (sway-get-tree)))
  "Find the parent of a given NODE, a sway TREE can be
optionally passed, default value is sway's current tree"
  (define* (sway-tree-node-parent-loop stree #:optional parent)
    (cond
     ((null? stree) #f)
     ((equal? (sway-tree-id stree) (sway-tree-id node)) parent)
     ((null? (sway-tree-nodes stree)) #f)
     (else (list-or (map (lambda (n) (sway-tree-node-parent-loop n stree))
                         (sway-tree-nodes stree))))))

  (sway-tree-node-parent-loop tree))

(define* (sway-tree-node-workspace node #:optional (tree (sway-get-tree)))
  "Find the parent workspace of a given NODE, a sway TREE can be
optionally passed, default value is sway current tree"
  (define* (sway-tree-node-workspace-loop child)
    (let ((parent (sway-tree-node-parent child tree)))
      (cond
       ((null? parent) #f)
       ((equal? (sway-tree-type parent) "workspace") parent)
       (else (sway-tree-node-workspace-loop parent)))))

  (sway-tree-node-workspace-loop node))

(define* (sway-tree-node-focused #:optional (tree (sway-get-tree)))
  "Find the currently focused container in a TREE, the TREE can be
optionally passed, default value is sway current tree"
  (define* (sway-tree-node-focused-loop node)
    (cond
     ((sway-tree-focused node) node)
     ((null? (sway-tree-nodes node)) #f)
     (else (list-or (map (lambda (n) (sway-tree-node-focused-loop n))
                         (sway-tree-nodes node))))))

  (sway-tree-node-focused-loop tree))

(define (sway-tree-remove-container node)
  "Remove the container NODE while keeping its children
The children will move to the parent of NODE"
  (for-each (lambda (n) (sway-move-container-to))
            (sway-tree-nodes node)))

(define (sway-tree-remove-children-layouts node)
  "Remove the children layouts from NODE while keeping its apps
The children will move to the parent of NODE"
  (for-each (lambda (n) (sway-move-container-to))
            (sway-tree-nodes node)))

(define (sway-tree-nodes-flat node)
  "Return a list of all containers (flat) under NODE"
  (cons node
        (apply append (map sway-tree-nodes-flat (sway-tree-nodes node)))))

(define (sway-tree-nodes-apps node)
  "Return a list of all apps under NODE"
  (filter (lambda (n) (sway-tree-app-id n))
          (sway-tree-nodes-flat node)))

(define (sway-tree-move-node node new-sibling-node)
  "Move the NODE to the NEW-PARENT-NODE"
  (let ((mark (format #f "temp-~a" (sway-tree-id new-sibling-node))))
    ;; set a mark on the new sibling
    (sway-dispatch-command
        (format #f "~a ~a"
                (sway-criteria #:con-id (sway-tree-id new-sibling-node))
                (sway-mark mark #:exec #f)))

    ;; move the node to the sibling mark
    (sway-dispatch-command
        (format #f "~a ~a"
                (sway-criteria #:con-id (sway-tree-id node))
                (sway-move-container-to-mark mark #:exec #f)))))

(define* (focused-workspace-name #:key (workspaces (sway-get-workspaces)))
  (cond
   ((null? workspaces) #f)
   ((equal? #t (sway-workspace-focused (car workspaces)))
    (sway-workspace-name (car workspaces)))
   (else (focused-workspace-name #:workspaces (cdr workspaces)))))

(define* (sway-tree-nodes-optimize node #:optional parent)
  "Remove any vertical/horizontal container that has only one child.
This function is necessary for strict some auto tiling layouts
It shouldn't be used for manual tiling."
  (let ((children (sway-tree-nodes node)))
    (cond
     ;; no children: nothing to do
     ((null? children) #t)

     ;; parent container is not of type workspace
     ;; and one of the children has layout none
     ((and (not (null? parent))
		   (not (equal? (sway-tree-type node) "workspace"))
           (not (equal? (sway-tree-type parent) "workspace"))
           (member "none" (map sway-tree-layout children)))
	  (format #t "moving container ~a to top of workspace ~a\n" (sway-tree-id node) (focused-workspace-name))

	  ;; find the id of the first container under current workspace
	  (let* ((focused (sway-tree-node-focused))
			 (workspace (sway-tree-node-workspace focused))
			 (con-id (sway-tree-id (car (sway-tree-nodes workspace)))))
		;; (sway-dispatch-command (format #f "~a; ~a ~a"
		;; 							   command
		;; 							   (sway-criteria #:con-id (sway-tree-id node))
		;; 							   (sway-move-container SWAY-DIRECTION-UP #:exec #f)))

		;; (sway-move-container-under-workspace (sway-tree-id node) (focused-workspace-name))
		;; (sway-dispatch-command (format #f "~a ~a"
		;; 					   (sway-criteria #:con-id (sway-tree-id focused))
		;; 					   (sway-move-container-to-workspace (focused-workspace-name) #:exec #f)))
		;; continue checking the child
		(sway-dispatch-command (format #f "~a ~a; ~a ~a"
							   (sway-criteria #:con-id (sway-tree-id node))
							   (sway-move-container-to-workspace "guile-swayer-scratchpad" #:exec #f)
							   (sway-criteria #:con-id (sway-tree-id node))
							   (sway-move-container-to-workspace (focused-workspace-name) #:exec #f)))

		(sway-tree-nodes-optimize (car children) node)))

     ;; otherwise, continue checking children
     (else (map (lambda (n) (sway-tree-nodes-optimize n node))
                children)))))

(define* (sway-move-container-under-container source-con-id target-con-id #:key (exec #t))
  "Moves focused container and place it under target container.
  parameters:
    - con-id: id of the target container"
  (let* ((mark-name "guile_swayer_target_con")
		 (mark-command (format #f "~a ~a"
							   (sway-criteria #:con-id target-con-id)
							   (sway-mark mark-name #:exec #f)))
		 (move-command (format #f "~a ~a"
							   (sway-criteria #:con-id source-con-id)
							   (sway-move-container-to-mark mark-name #:exec #f)))
		 (unmark-command (format #f "~a ~a"
								 (sway-criteria #:con-id target-con-id)
								 (sway-unmark mark-name #:exec #f)))
         (command (string-join (list mark-command move-command unmark-command) "; ")))
    (if exec (sway-dispatch-command command)
        command)))

(define (sway-tree-print node)
  "Print the structure of the tree and its containers in a clear tree view"
  (define (print-node n indent is-last)
    (let* ((prefix (if is-last "└── " "├── "))
           (children (append (sway-tree-nodes n) (sway-tree-floating-nodes n)))
           (type (sway-tree-type n))
           (layout (sway-tree-layout n))
           (id (sway-tree-id n))
           (name (sway-tree-name n))
           (app-id (sway-tree-app-id n))
           (focused (if (sway-tree-focused n) " [FOCUSED]" "")))
      (display indent)
      (display prefix)
      (format #t "[~a] id:~a layout:~a ~a~a~a\n"
              type id layout
              (if (and (string? name) (not (string-null? name))) (format #f "name:\"~a\" " name) "")
              (if (and (string? app-id) (not (string-null? app-id))) (format #f "app-id:\"~a\" " app-id) "")
              focused)
      (let ((new-indent (string-append indent (if is-last "    " "│   "))))
        (let loop ((nodes children))
          (unless (null? nodes)
            (print-node (car nodes) new-indent (null? (cdr nodes)))
            (loop (cdr nodes)))))))

  (let ((children (append (sway-tree-nodes node) (sway-tree-floating-nodes node)))
        (type (sway-tree-type node))
        (layout (sway-tree-layout node))
        (id (sway-tree-id node))
        (name (sway-tree-name node))
        (focused (if (sway-tree-focused node) " [FOCUSED]" "")))
    (format #t "[~a] id:~a layout:~a ~a~a\n"
            type id layout
            (if (and (string? name) (not (string-null? name))) (format #f "name:\"~a\" " name) "")
            focused)
    (let loop ((nodes children))
      (unless (null? nodes)
        (print-node (car nodes) "" (null? (cdr nodes)))
        (loop (cdr nodes))))))

