{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.Attributes
    ( get
    , post
    , target
    , swap
    , trigger
    )
where

import Text.Blaze.Html5 as H

get :: AttributeValue -> Attribute
get = customAttribute "hx-get"

post :: AttributeValue -> Attribute
post = customAttribute "hx-post"

target :: AttributeValue -> Attribute
target = customAttribute "hx-target"

swap :: AttributeValue -> Attribute
swap = customAttribute "hx-swap"

trigger :: AttributeValue -> Attribute
trigger = customAttribute "hx-trigger"
