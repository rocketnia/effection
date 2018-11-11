#lang parendown racket/base

(require #/only-in racket/contract/base -> any/c listof)
(require #/only-in racket/contract/region define/contract)
(require #/only-in racket/math natural?)

(require #/only-in lathe-comforts/trivial trivial?)

(require #/only-in effection/order/base
  dexableof fuse? name? table? tableof valid-dexable?)


(define/contract (extfx? v)
  (-> any/c boolean?)
  'TODO)

(define/contract (dspace? v)
  (-> any/c boolean?)
  'TODO)


(define/contract (extfx-noop)
  (-> extfx?)
  'TODO)

(define/contract (fuse-extfx)
  (-> fuse?)
  'TODO)


(define/contract (extfx-dspace-create-shadower ds then)
  (-> dspace? (-> dspace? extfx?) extfx?)
  'TODO)


(define/contract (authorized-name? v)
  (-> any/c boolean?)
  'TODO)

(define/contract (authorized-name-get-name n)
  (-> authorized-name? name?)
  'TODO)

(define/contract (name-subname key-name original-name)
  (-> name? name? name?)
  'TODO)

(define/contract (authorized-name-subname key-name original-name)
  (-> name? authorized-name? authorized-name?)
  'TODO)

(define/contract (extfx-authorized-name-split n times then)
  (-> authorized-name? natural? (-> (listof authorized-name?) extfx?)
    extfx?)
  'TODO)


(define/contract (extfx-put-once ds n value)
  (-> dspace? authorized-name? any/c extfx?)
  'TODO)

(define/contract (extfx-put-dexable ds n value)
  (-> dspace? authorized-name? valid-dexable? extfx?)
  'TODO)

(define/contract (extfx-get ds n then)
  (-> dspace? name? (-> any/c extfx?) extfx?)
  'TODO)


(define/contract
  (extfx-private-put-once ds putter-name getter-name value)
  (-> dspace? authorized-name? name? any/c extfx?)
  'TODO)

(define/contract
  (extfx-private-put-dexable ds putter-name getter-name value)
  (-> dspace? authorized-name? name? valid-dexable? extfx?)
  'TODO)

(define/contract (extfx-private-get ds putter-name getter-name then)
  (-> dspace? name? authorized-name? (-> any/c extfx?) extfx?)
  'TODO)



(define/contract (pub? v)
  (-> any/c boolean?)
  'TODO)

(define/contract (sub? v)
  (-> any/c boolean?)
  'TODO)

(define/contract (extfx-establish-pubsub ds pubsub-name then)
  (-> dspace? authorized-name? (-> pub? sub? extfx?) extfx?)
  'TODO)

(define/contract (extfx-pub-once ds p pubber-name arg)
  (-> dspace? pub? authorized-name? any/c extfx?)
  'TODO)

(define/contract (extfx-pub-dexable ds p pubber-name arg)
  (-> dspace? pub? authorized-name? valid-dexable? extfx?)
  'TODO)

(define/contract (extfx-sub-once ds s subber-name func)
  (-> dspace? sub? authorized-name? (-> any/c extfx?) extfx?)
  'TODO)

(define/contract (extfx-sub-dexable ds s subber-name func)
  (-> dspace? sub? authorized-name? (dexableof #/-> any/c extfx?)
    extfx?)
  'TODO)



(define/contract (familiarity-ticket? v)
  (-> any/c boolean?)
  'TODO)

(define/contract (extfx-ft-split-list ticket times then)
  (-> familiarity-ticket? natural?
    (-> (listof familiarity-ticket?) extfx?)
    extfx?)
  'TODO)

(define/contract (extfx-ft-split-table ticket times then)
  (-> familiarity-ticket? (tableof trivial?)
    (-> (tableof familiarity-ticket?) extfx?)
    extfx?)
  'TODO)

(define/contract (extfx-ft-obtain-subnames ticket key-names then)
  (-> familiarity-ticket? (tableof trivial?)
    (-> familiarity-ticket? (tableof familiarity-ticket?) extfx?)
    extfx?)
  'TODO)


(define/contract (extfx-ft-disburse ds hub-name ticket)
  (-> dspace? authorized-name? familiarity-ticket? extfx?)
  'TODO)

(define/contract (extfx-ft-claim ds t then)
  (-> dspace? familiarity-ticket? (-> familiarity-ticket? extfx?)
    extfx?)
  'TODO)


(define/contract
  (extfx-contribute-once ds collector-familiarity-ticket value)
  (-> dspace? familiarity-ticket? any/c extfx?)
  'TODO)

(define/contract
  (extfx-contribute-dexable ds collector-familiarity-ticket value)
  (-> dspace? familiarity-ticket? valid-dexable? extfx?)
  'TODO)

(define/contract (extfx-collect ds collector-name then)
  (-> dspace? authorized-name? (-> table? extfx?) extfx?)
  'TODO)
