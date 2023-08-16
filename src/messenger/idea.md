# Requirements
## Users
  - User can send message to other user
  - A user is associated with an address
  - User can send message to a user or a group of users

## Group
  - A user can join a group
  - A group can be
    - Public group: a user can join by just enter group id
    - Private group:
      + Protected by password
      + Invited by any/one of/some/all members of the group
    - A user can be remove from a group
    - A user can be muted by a user in a group or all member of a group
  - Each group may display a message in different format: text, pdf, html, blog, short form, long form, ...
  - A group could be external group such as a facebook fanpage, youtube channel, twitter
  - Channel/nested group

## Publisher
  - A user can send a message to group of groups, each of them has different format
    - For example: a user can send a message which is a post to facebook, short version of the post to twitter and long version of it as an article in a blog

## Messages
  - Message is group into nested conversation
    - A conversation may have a tile
  - Message has a timestamp
    - Timestamp ordering in distributed system
  - Message may have media, attachment, ...
  - Messsage may be in other format: html, pdf, ...
  - TCP/UDP, ...

## Notification
  - Sent, received, seen
  - User enter/leave/muted in a group
  - A user stop receiving message from other user in a group
## What kind of application
  - Chat app
  - In an autodriving car
    - Sensor send message to 


# Idea

```haskell
data Addr

data Endpoint
  = Single Addr
  | Both Endpoint Endpoint
  | Gateway Endpoint Endpoint

data Message
  = Message User Timestamp Text
  | ReplyTo Message Message

data Thread
  = Creator User
  | Content Text
  | Comment Timestamp User Thread

recv :: Message -> Inbox -> Inbox
send :: Message -> Outbox -> Outbox
```

## Q&A

- What is purpose of this app?
  - Allow a user to propagate a message in a topology
- How to define the topology?
  - Public vs private group
  - Message format is defined in topology definition
- Message in different formats?
  - `render :: Message -> [Format]`
- Message destination/addressing?
  - User address? Group address? Channel/subgroup address?
  - This could be similar to IP addressing
- User authentication?
  - PW authentication
