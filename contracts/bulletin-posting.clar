;; Community Bulletin Board Contract
;; Allows posting local announcements with category filtering and expiration management

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_FOUND (err u404))
(define-constant ERR_UNAUTHORIZED (err u401))
(define-constant ERR_INVALID_CATEGORY (err u400))
(define-constant ERR_EXPIRED (err u410))

;; Categories for posts
(define-constant CATEGORY_GARAGE_SALE u1)
(define-constant CATEGORY_LOST_PET u2)
(define-constant CATEGORY_SERVICE u3)

;; Data structure for bulletin posts
(define-map bulletin-posts
  { post-id: uint }
  {
    author: principal,
    title: (string-ascii 100),
    content: (string-ascii 500),
    category: uint,
    created-at: uint,
    expires-at: uint
  }
)

(define-data-var next-post-id uint u1)

;; Post a new bulletin
(define-public (create-post (title (string-ascii 100))
                           (content (string-ascii 500))
                           (category uint)
                           (duration-blocks uint))
  (let ((post-id (var-get next-post-id))
        (current-height stacks-block-height)
        (expiry (+ current-height duration-blocks)))
    (asserts! (or (is-eq category CATEGORY_GARAGE_SALE)
                  (is-eq category CATEGORY_LOST_PET)
                  (is-eq category CATEGORY_SERVICE)) ERR_INVALID_CATEGORY)
    (map-set bulletin-posts
      { post-id: post-id }
      {
        author: tx-sender,
        title: title,
        content: content,
        category: category,
        created-at: current-height,
        expires-at: expiry
      }
    )
    (var-set next-post-id (+ post-id u1))
    (ok post-id)
  )
)

;; Get a specific post
(define-read-only (get-post (post-id uint))
  (match (map-get? bulletin-posts { post-id: post-id })
    post (if (<= stacks-block-height (get expires-at post))
           (ok post)
           ERR_EXPIRED)
    ERR_NOT_FOUND
  )
)

;; Delete a post (only by author)
(define-public (delete-post (post-id uint))
  (match (map-get? bulletin-posts { post-id: post-id })
    post (begin
           (asserts! (is-eq tx-sender (get author post)) ERR_UNAUTHORIZED)
           (map-delete bulletin-posts { post-id: post-id })
           (ok true))
    ERR_NOT_FOUND
  )
)

;; Check if post is active (not expired)
(define-read-only (is-post-active (post-id uint))
  (match (map-get? bulletin-posts { post-id: post-id })
    post (ok (<= stacks-block-height (get expires-at post)))
    ERR_NOT_FOUND
  )
)

;; Get total number of posts created
(define-read-only (get-total-posts)
  (ok (- (var-get next-post-id) u1))
)
