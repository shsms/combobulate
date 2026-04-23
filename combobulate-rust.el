;;; combobulate-rust.el --- rust support for combobulate -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Mickey Petersen

;; Author: Mickey Petersen <mickey@masteringemacs.org>
;; Keywords: convenience, tools, languages, rust

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'combobulate-settings)
(require 'combobulate-navigation)
(require 'combobulate-setup)
(require 'combobulate-manipulation)
(require 'combobulate-rules)

(defgroup combobulate-rust nil
  "Configuration switches for Rust."
  :group 'combobulate
  :prefix "combobulate-rust-")

(defun combobulate-rust--visibility-prefix (node)
  "Return the `pub ...' prefix of NODE, or empty if absent."
  (let ((vm (car (combobulate-filter-nodes
                  (combobulate-node-children node)
                  :keep-types '("visibility_modifier")))))
    (if vm (concat (combobulate-node-text vm) " ") "")))

(defun combobulate-rust--fn-modifiers (node)
  "Return any fn modifier keywords (async/unsafe/...) for NODE, space-separated."
  (let ((mods (car (combobulate-filter-nodes
                    (combobulate-node-children node)
                    :keep-types '("function_modifiers")))))
    (if mods (concat (combobulate-node-text mods) " ") "")))

(defun combobulate-rust--type-params (node)
  "Return the `<...>' generic parameters of NODE, or empty."
  (let ((tp (combobulate-node-child-by-field node "type_parameters")))
    (if tp (combobulate-node-text tp) "")))

(defun combobulate-rust-pretty-print-node-name (node default-name)
  "Pretty printer for Rust NODE name, falling back to DEFAULT-NAME."
  (combobulate-string-truncate
   (replace-regexp-in-string
    (rx (| (>= 2 " ") "\n")) ""
    (pcase (combobulate-node-type node)
      ((or "function_item" "function_signature_item")
       (concat (combobulate-rust--visibility-prefix node)
               (combobulate-rust--fn-modifiers node)
               "fn "
               (combobulate-node-text
                (combobulate-node-child-by-field node "name"))
               (combobulate-rust--type-params node)))
      ((or "struct_item" "enum_item" "trait_item" "union_item"
           "mod_item" "type_item" "const_item" "static_item")
       ;; The keyword is the node type sans "_item"; nodes that cannot
       ;; take generics simply have no type_parameters field.
       (concat (combobulate-rust--visibility-prefix node)
               (string-remove-suffix "_item" (combobulate-node-type node))
               " "
               (combobulate-node-text
                (combobulate-node-child-by-field node "name"))
               (combobulate-rust--type-params node)))
      ("impl_item"
       (let ((tp   (combobulate-rust--type-params node))
             (tr   (combobulate-node-child-by-field node "trait"))
             (ty   (combobulate-node-child-by-field node "type")))
         (concat "impl" tp " "
                 (if tr
                     (concat (combobulate-node-text tr) " for "
                             (and ty (combobulate-node-text ty)))
                   (and ty (combobulate-node-text ty))))))
      ("macro_definition"
       (concat "macro_rules! "
               (combobulate-node-text
                (combobulate-node-child-by-field node "name"))))
      ("let_declaration"
       (concat "let "
               (combobulate-node-text
                (combobulate-node-child-by-field node "pattern"))))
      ("identifier" (combobulate-node-text node))
      ("type_identifier" (combobulate-node-text node))
      ("field_identifier" (combobulate-node-text node))
      (_ default-name)))
   60))

(eval-and-compile
  (defvar combobulate-rust-definitions
    '((envelope-procedure-shorthand-alist
       '((general-statement
          . ((:activation-nodes
              ((:nodes ((rule "block")
                        (rule "declaration_list")
                        (rule "source_file"))
                       :has-parent ("block" "declaration_list" "source_file"))))))))
      (envelope-list
       '((:description
          "if ... { ... } [else { ... }]"
          :key "i"
          :mark-node t
          :shorthand general-statement
          :name "if-expression"
          :template
          ("if " @ (p cond "Condition") " {" n>
           (choice* :missing nil
                    :rest (r> n>)
                    :name "if-block")
           "}" >
           (choice* :missing nil
                    :rest (" else {" n> @ r> n> "}" > n>)
                    :name "else-block")))
         (:description
          "match ... { ... => ..., }"
          :key "m"
          :mark-node t
          :shorthand general-statement
          :name "match-expression"
          :template
          ("match " @ (p expr "Expression") " {" n>
           (p pat "Pattern") " => " @ r> "," n>
           (choice* :missing nil
                    :rest ((p pat2 "Next Pattern") " => " @ "," n>)
                    :name "add-arm")
           "}" > n>))
         (:description
          "for ... in ... { ... }"
          :key "f"
          :mark-node t
          :shorthand general-statement
          :name "for-loop"
          :template
          ("for " (p var "Variable") " in " (p iter "Iterator") " {" n>
           @ r> n>
           "}" > n>))
         (:description
          "while ... { ... }"
          :key "w"
          :mark-node t
          :shorthand general-statement
          :name "while-loop"
          :template
          ("while " (p cond "Condition") " {" n>
           @ r> n>
           "}" > n>))
         (:description
          "loop { ... }"
          :key "l"
          :mark-node t
          :shorthand general-statement
          :name "loop-expression"
          :template
          ("loop {" n> @ r> n> "}" > n>))
         (:description
          "fn ... ( ... ) -> ... { ... }"
          :key "F"
          :mark-node t
          :shorthand general-statement
          :name "function-item"
          :template
          ("fn " (p name "Name") "(" (p params "Parameters") ") {" n>
           @ r> n>
           "}" > n>))
         ;; Struct and enum bodies only admit fields/variants, so these
         ;; two do not mark (and thus wrap) the node at point; `r>'
         ;; still re-inserts an explicitly selected region.
         (:description
          "struct ... { field: Type, ... }"
          :key "s"
          :mark-node nil
          :shorthand general-statement
          :name "struct-item"
          :template
          ("struct " (p name "Name") " {" n>
           (p field "Field") ": " (p ty "Type") "," n>
           @ r> n>
           "}" > n>))
         (:description
          "enum ... { ... }"
          :key "e"
          :mark-node nil
          :shorthand general-statement
          :name "enum-item"
          :template
          ("enum " (p name "Name") " {" n>
           @ r> n>
           "}" > n>))
         (:description
          "impl ... { ... }"
          :key "I"
          :mark-node t
          :shorthand general-statement
          :name "impl-item"
          :template
          ("impl " (p ty "Type") " {" n>
           @ r> n>
           "}" > n>))
         (:description
          "if let Some(x) = ... { ... } else { ... }"
          :key "L"
          :mark-node t
          :shorthand general-statement
          :name "if-let"
          :template
          ("if let " (p pat "Pattern") " = " @ (p expr "Expression") " {" n>
           (choice* :missing nil :rest (r> n>) :name "if-let-block")
           "}" >
           (choice* :missing nil
                    :rest (" else {" n> @ r> n> "}" > n>)
                    :name "else-block")))
         (:description
          "let ... = ...;"
          :key "v"
          :mark-node t
          :shorthand general-statement
          :name "let-binding"
          :template
          ("let "
           (choice* :missing nil :rest ("mut ") :name "mut")
           (p name "Name") " = " @ r> ";" n>))
         (:description
          "impl Trait for Type { ... }"
          :key "T"
          :mark-node t
          :shorthand general-statement
          :name "impl-trait-for"
          :template
          ("impl " (p tr "Trait") " for " (p ty "Type") " {" n>
           @ r> n>
           "}" > n>))
         (:description
          "#[derive(...)]"
          :key "#"
          :mark-node nil
          :shorthand general-statement
          :name "derive-attribute"
          :template
          ("#[derive(" @ (p traits "Traits, comma-separated") ")]" n>))
         (:description
          "async fn ... ( ... ) [-> ...] { ... }"
          :key "a"
          :mark-node t
          :shorthand general-statement
          :name "async-fn"
          :template
          ("async fn " (p name "Name") "(" (p params "Parameters") ")"
           (choice* :missing nil
                    :rest (" -> " (p ret "Return Type"))
                    :name "return-type")
           " {" n>
           @ r> n>
           "}" > n>))
         (:description
          "|args| { body }"
          :key "|"
          :mark-node t
          :shorthand general-statement
          :name "closure"
          :template
          ("|" (p args "Args") "| {" n> @ r> n> "}" > n>))))
      (context-nodes
       '("identifier" "type_identifier" "field_identifier"
         "primitive_type" "boolean_literal" "integer_literal"
         "float_literal" "string_literal" "char_literal" "self"
         "lifetime" "shorthand_field_identifier"))
      (indent-after-edit nil)
      (envelope-indent-region-function #'indent-region)
      (procedures-edit nil)
      (pretty-print-node-name-function #'combobulate-rust-pretty-print-node-name)
      (procedures-sexp
       '((:activation-nodes
          ((:nodes ("function_item" "function_signature_item"
                    "struct_item" "enum_item" "trait_item" "impl_item"
                    "mod_item" "const_item" "static_item" "type_item"
                    "macro_definition" "union_item" "use_declaration"
                    "let_declaration" "expression_statement"
                    "match_arm" "match_expression"
                    "if_expression" "else_clause"
                    "while_expression" "for_expression"
                    "loop_expression" "closure_expression"
                    "block" "match_block" "async_block" "unsafe_block"
                    "struct_expression" "call_expression" "tuple_expression"
                    "array_expression" "parenthesized_expression"
                    "string_literal" "raw_string_literal"
                    "integer_literal" "float_literal" "char_literal"
                    "boolean_literal"
                    "field_declaration" "enum_variant"
                    "identifier" "type_identifier" "field_identifier"))))))
      (plausible-separators '(";" "," "\n"))
      (display-ignored-node-types
       '("let" "fn" "struct" "enum" "trait" "impl" "mod" "use"
         "const" "static" "type" "pub" "mutable_specifier" "ref" "extern"
         "async" "unsafe" "macro_rules!" "move"))
      (procedures-defun
       '((:activation-nodes
          ((:nodes ("function_item"
                    "function_signature_item"
                    "struct_item"
                    "enum_item"
                    "trait_item"
                    "impl_item"
                    "mod_item"
                    "const_item"
                    "static_item"
                    "type_item"
                    "macro_definition"
                    "union_item"))))))
      (procedures-logical
       '((:activation-nodes ((:nodes (all))))))
      ;; Skip line/block comments during navigation. Rust's grammar
      ;; uses these names rather than the generic "comment" default.
      (procedure-discard-rules '("line_comment" "block_comment"))
      (procedures-sibling
       `(;; Comments are discarded from results (via procedure-discard-rules)
         ;; but we still want to allow initiating nav FROM a comment, so
         ;; list them as activation nodes in the common containers.
         (:activation-nodes
          ((:nodes ("line_comment" "block_comment")
                   :position at
                   :has-parent ("block" "source_file"
                                "declaration_list"
                                "enum_variant_list"
                                "field_declaration_list"
                                "field_initializer_list"
                                "match_block"
                                "ordered_field_declaration_list"
                                "use_list" "token_tree"
                                "arguments" "parameters"
                                "tuple_expression")))
          :selector (:choose parent :match-children t))
         ;; match arms inside a match block
         (:activation-nodes
          ((:nodes ("match_arm")
                   :position at
                   :has-parent ("match_block")))
          :selector (:choose parent :match-children (:match-rules ("match_arm"))))
         ;; enum variants (and their attributes) inside the variant list
         (:activation-nodes
          ((:nodes ("enum_variant" "attribute_item")
                   :position at
                   :has-parent ("enum_variant_list")))
          :selector (:choose parent :match-children
                             (:match-rules ("enum_variant" "attribute_item"))))
         ;; struct fields (and their attributes) in a field declaration list
         (:activation-nodes
          ((:nodes ("field_declaration" "attribute_item")
                   :position at
                   :has-parent ("field_declaration_list")))
          :selector (:choose parent :match-children
                             (:match-rules ("field_declaration" "attribute_item"))))
         ;; where predicates: `where T: Foo, U: Bar'
         (:activation-nodes
          ((:nodes ("where_predicate")
                   :position at
                   :has-parent ("where_clause")))
          :selector (:choose parent :match-children (:match-rules ("where_predicate"))))
         ;; comma/semicolon-separated containers where every named child
         ;; is a sibling: tuple struct types, struct-expression
         ;; initializers, use lists, derive/attribute and macro args
         ;; (token_tree), call arguments, tuples, (closure/type)
         ;; parameters and arguments, module/impl/trait bodies, and
         ;; statements in a block or at the top level.
         (:activation-nodes
          ((:nodes ((rule "ordered_field_declaration_list"))
                   :position at
                   :has-parent ("ordered_field_declaration_list"))
           (:nodes ((rule "field_initializer_list"))
                   :position at
                   :has-parent ("field_initializer_list"))
           (:nodes ((rule "use_list"))
                   :position at
                   :has-parent ("use_list"))
           (:nodes ((rule "token_tree"))
                   :position at
                   :has-parent ("token_tree"))
           (:nodes ((rule "arguments"))
                   :position at
                   :has-parent ("arguments"))
           (:nodes ((rule "tuple_expression"))
                   :position at
                   :has-parent ("tuple_expression"))
           (:nodes ((rule "parameters"))
                   :position at
                   :has-parent ("parameters"))
           (:nodes ((rule "closure_parameters"))
                   :position at
                   :has-parent ("closure_parameters"))
           (:nodes ((rule "type_arguments"))
                   :position at
                   :has-parent ("type_arguments"))
           (:nodes ((rule "type_parameters"))
                   :position at
                   :has-parent ("type_parameters"))
           (:nodes ((rule "declaration_list"))
                   :position at
                   :has-parent ("declaration_list"))
           (:nodes ((rule "block")
                    (rule "source_file"))
                   :position at
                   :has-parent ("block" "source_file")))
          :selector (:choose parent :match-children t))
         ;; fallbacks for declarations and statements.
         (:activation-nodes
          ((:nodes (rule "_declaration_statement")))
          :selector (:choose node :match-children t))
         (:activation-nodes
          ((:nodes ((rx "statement" eol))))
          :selector (:choose node :match-children t))))
      (procedures-hierarchy
       `(;; From items with a block/body, dive DIRECTLY to the first
         ;; meaningful child inside the body, skipping the opening `{'
         ;; token and the intermediate block/list wrapper node.
         (:activation-nodes
          ((:nodes ("function_item") :position at))
          :selector (:choose node
                     :match-query
                     (:query ((function_item body: (block (_) @match)))
                      :engine treesitter)))
         (:activation-nodes
          ((:nodes ("impl_item" "trait_item" "mod_item") :position at))
          :selector (:choose node
                     :match-query
                     (:query ((_ body: (declaration_list (_) @match)))
                      :engine treesitter)))
         (:activation-nodes
          ((:nodes ("struct_item") :position at))
          :selector (:choose node
                     :match-query
                     (:query ((struct_item body: (_ (_) @match)))
                      :engine treesitter)))
         (:activation-nodes
          ((:nodes ("enum_item") :position at))
          :selector (:choose node
                     :match-query
                     (:query ((enum_item body: (enum_variant_list (_) @match)))
                      :engine treesitter)))
         (:activation-nodes
          ((:nodes ("if_expression") :position at))
          :selector (:choose node
                     :match-query
                     (:query ((if_expression consequence: (block (_) @match)))
                      :engine treesitter)))
         (:activation-nodes
          ((:nodes ("while_expression" "for_expression"
                    "loop_expression")
                   :position at))
          :selector (:choose node
                     :match-query
                     (:query ((_ body: (block (_) @match)))
                      :engine treesitter)))
         (:activation-nodes
          ((:nodes ("unsafe_block" "async_block") :position at))
          :selector (:choose node
                     :match-query
                     (:query ((_ (block (_) @match)))
                      :engine treesitter)))
         (:activation-nodes
          ((:nodes ("else_clause") :position at))
          :selector (:choose node
                     :match-query
                     (:query ((else_clause (block (_) @match)))
                      :engine treesitter)))
         (:activation-nodes
          ((:nodes ("match_expression") :position at))
          :selector (:choose node
                     :match-query
                     (:query ((match_expression body: (match_block (_) @match)))
                      :engine treesitter)))
         ;; When the user is standing right on `{' (at a block's start),
         ;; dive into the block's first named child. Same pattern as Go
         ;; and Python: `block' is an activation node with `:position at'
         ;; so `C-M-d' from `{' still works, and navigate-up from a
         ;; statement stops briefly at `{'.
         (:activation-nodes
          ((:nodes ("block" "match_block" "declaration_list"
                    "field_declaration_list" "enum_variant_list"
                    "field_initializer_list"
                    "ordered_field_declaration_list")
                   :position at))
          :selector (:choose node :match-children
                             (:match-rules ((rule "_declaration_statement")
                                            (rule "_expression")
                                            "expression_statement"
                                            "match_arm"
                                            "field_declaration"
                                            "field_initializer"
                                            "enum_variant"
                                            "shorthand_field_initializer"
                                            "attribute_item"))))
         ;; use declarations: dive into the argument (a path or use_list).
         (:activation-nodes
          ((:nodes ("use_declaration") :position at))
          :selector (:choose node :match-children
                             (:match-rules ("scoped_use_list" "use_list"
                                            "use_as_clause" "use_wildcard"
                                            "scoped_identifier" "identifier"))))
         (:activation-nodes
          ((:nodes ("scoped_use_list") :position at))
          :selector (:choose node :match-children
                             (:match-rules ("use_list"))))
         ;; macro invocations: dive into the token_tree args.
         (:activation-nodes
          ((:nodes ("macro_invocation") :position at))
          :selector (:choose node :match-children
                             (:match-rules ("token_tree"))))
         ;; attribute items: dive into the attribute, then its arguments.
         (:activation-nodes
          ((:nodes ("attribute_item" "inner_attribute_item") :position at))
          :selector (:choose node :match-children
                             (:match-rules ("attribute"))))
         (:activation-nodes
          ((:nodes ("attribute") :position at))
          :selector (:choose node :match-children
                             (:match-rules ("token_tree" "identifier"
                                            "scoped_identifier"))))
         ;; where clause: dive into predicates.
         (:activation-nodes
          ((:nodes ("where_clause") :position at))
          :selector (:choose node :match-children
                             (:match-rules ("where_predicate"))))
         ;; closure: dive into body.
         (:activation-nodes
          ((:nodes ("closure_expression") :position at))
          :selector (:choose node :match-children
                             (:match-rules ("closure_parameters" "block"
                                            (rule "_expression")))))
         ;; match arms: dive into the pattern/body.
         (:activation-nodes
          ((:nodes ("match_arm") :position at))
          :selector (:choose node :match-children
                             (:match-rules ("match_pattern"
                                            (rule "_expression")))))
         ;; fall back: arbitrary children navigation.
         (:activation-nodes
          ((:nodes ((rule "_expression")
                    (rule "_declaration_statement")
                    (rule "source_file"))
                   :position at))
          :selector (:choose node :match-children t)))))))

(define-combobulate-language
 :name rust
 :major-modes (rust-mode rust-ts-mode)
 :custom combobulate-rust-definitions
 :setup-fn combobulate-rust-setup)

(defun combobulate-rust-setup (_))

(provide 'combobulate-rust)
;;; combobulate-rust.el ends here
