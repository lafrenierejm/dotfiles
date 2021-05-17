;; This channel file can be passed to 'guix pull -C' or to
;; 'guix time-machine -C' to obtain the Guix revision that was
;; used to populate this profile.

(list
 ;; Note: these other commits were also used to install some of the packages in this profile:
 ;;   "5b359e4ba58daee0e08857cfe3a2296a2ac6fca6"
 ;;   "e026b226cda7e6513cf34ee80b278f60fdc7fbf4"
 ;;   "d6aeebb23639258311fdfb9dbf5f903079fde51a"
 ;;   "069399ee9dbf75b7c89583f03346a63b2cfe4ac6"
 ;;   "f903bb7741f3a6933b29c07c7850cdd7ecb0df07"
 ;;   "56e4d7204b0d4f83ab8cf4aab24199a9f8dc082c"
 ;;   "10ecae4503addec38439a01868e641110a548082"
 ;;   "1059d1570b7078e6a83e067577b42ed24ee25b55"
 ;;   "2c93df3d11bf8ceeb5c203416a2533cf32275e1a"
 ;;   "799cc194f21b0f36f2b1cf83a47f75a30be56133"
 ;;   "4ebf6ccb3ecf2996ce5bb8719c2c851298833f19"
 ;;   "f91e1046c4050c0947429b9141c80024c261d7b5"
 ;;   "b7aca04856ffcbacb4179b3c485c20403bd19787"
 (channel
  (name 'guix)
  (url "https://git.savannah.gnu.org/git/guix.git")
  (commit
   "938ffcbb0589adc07dc12c79eda3e1e2bb9e7cf8")
  (introduction
   (make-channel-introduction
    "9edb3f66fd807b096b48283debdcddccfea34bad"
    (openpgp-fingerprint
     "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
 (channel
  (name 'nonguix)
  (url "https://gitlab.com/nonguix/nonguix")
  (commit
   "9b8e36975bd0dd81d47d8b06da4beffe8bac3a50")
  (introduction
   (make-channel-introduction
    "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
    (openpgp-fingerprint
     "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5")))))
