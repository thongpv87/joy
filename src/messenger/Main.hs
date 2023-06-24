{-
# Messenger
\* Users
  - User can send message to other user
  - A user is associated with an address
  - User can send message to a user or a group of users

\* Group
  - A user can join a group
  - A group can be
    - Public group: a user can join by just enter group id
    - Private group:
      + Protected by password
      + Invited by any/one of/some/all members of the group
    - A user can be remove from a group
    - A user can be muted by a user in a group or all member of a group

\* Messages
  - Message is group into nested conversation
    - A conversation may have a tile

\* Notification
  - Sent, received, seen
  - User enter/leave/muted in a group
  - A user stop receiving message from other user in a group

-}
main = do
  putStrLn "Hello world"
