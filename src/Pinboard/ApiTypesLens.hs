{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Pinboard.ApiTypesLens where

import Pinboard.ApiTypes

import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Time.Calendar (Day)

import Control.Applicative
import Data.Profunctor
import Data.Either
import Prelude hiding (words, unwords)

-- * Lens Aliases
type Lens_' s a = Lens_ s s a a

type Lens_ s t a b = forall (f :: * -> *). Functor f =>
                                           (a -> f b) -> s -> f t

type Prism_' s a = Prism_ s s a a

type Prism_ s t a b = forall (p :: * -> * -> *) (f :: * -> *). (Choice p
                                                               ,Applicative f) =>
                                                               p a (f b) -> p s (f t)

-- * Posts
postsDateL :: Lens_' Posts UTCTime
postsDateL f_acx6 (Posts x1_acx7 x2_acx8 x3_acx9) =
  fmap (\y1_acxa -> Posts y1_acxa x2_acx8 x3_acx9) (f_acx6 x1_acx7)

{-# INLINE postsDateL #-}

postsPostsL :: Lens_' Posts [Post]
postsPostsL f_acxb (Posts x1_acxc x2_acxd x3_acxe) =
  fmap (\y1_acxf -> Posts x1_acxc x2_acxd y1_acxf) (f_acxb x3_acxe)

{-# INLINE postsPostsL #-}

postsUserL :: Lens_' Posts Text
postsUserL f_acxg (Posts x1_acxh x2_acxi x3_acxj) =
  fmap (\y1_acxk -> Posts x1_acxh y1_acxk x3_acxj) (f_acxg x2_acxi)

{-# INLINE postsUserL #-}

-- * Post
postDescriptionL :: Lens_' Post Text
postDescriptionL f_aczI (Post x1_aczJ x2_aczK x3_aczL x4_aczM x5_aczN x6_aczO x7_aczP x8_aczQ x9_aczR) =
  fmap
    (\y1_aczS ->
        Post x1_aczJ y1_aczS x3_aczL x4_aczM x5_aczN x6_aczO x7_aczP x8_aczQ x9_aczR)
    (f_aczI x2_aczK)

{-# INLINE postDescriptionL #-}

postExtendedL :: Lens_' Post Text
postExtendedL f_aczT (Post x1_aczU x2_aczV x3_aczW x4_aczX x5_aczY x6_aczZ x7_acA0 x8_acA1 x9_acA2) =
  fmap
    (\y1_acA3 ->
        Post x1_aczU x2_aczV y1_acA3 x4_aczX x5_aczY x6_aczZ x7_acA0 x8_acA1 x9_acA2)
    (f_aczT x3_aczW)

{-# INLINE postExtendedL #-}

postHashL :: Lens_' Post Text
postHashL f_acA4 (Post x1_acA5 x2_acA6 x3_acA7 x4_acA8 x5_acA9 x6_acAa x7_acAb x8_acAc x9_acAd) =
  fmap
    (\y1_acAe ->
        Post x1_acA5 x2_acA6 x3_acA7 x4_acA8 y1_acAe x6_acAa x7_acAb x8_acAc x9_acAd)
    (f_acA4 x5_acA9)

{-# INLINE postHashL #-}

postHrefL :: Lens_' Post Text
postHrefL f_acAf (Post x1_acAg x2_acAh x3_acAi x4_acAj x5_acAk x6_acAl x7_acAm x8_acAn x9_acAo) =
  fmap
    (\y1_acAp ->
        Post y1_acAp x2_acAh x3_acAi x4_acAj x5_acAk x6_acAl x7_acAm x8_acAn x9_acAo)
    (f_acAf x1_acAg)

{-# INLINE postHrefL #-}

postMetaL :: Lens_' Post Text
postMetaL f_acAq (Post x1_acAr x2_acAs x3_acAt x4_acAu x5_acAv x6_acAw x7_acAx x8_acAy x9_acAz) =
  fmap
    (\y1_acAA ->
        Post x1_acAr x2_acAs x3_acAt y1_acAA x5_acAv x6_acAw x7_acAx x8_acAy x9_acAz)
    (f_acAq x4_acAu)

{-# INLINE postMetaL #-}

postSharedL :: Lens_' Post Bool
postSharedL f_acAB (Post x1_acAC x2_acAD x3_acAE x4_acAF x5_acAG x6_acAH x7_acAI x8_acAJ x9_acAK) =
  fmap
    (\y1_acAL ->
        Post x1_acAC x2_acAD x3_acAE x4_acAF x5_acAG x6_acAH y1_acAL x8_acAJ x9_acAK)
    (f_acAB x7_acAI)

{-# INLINE postSharedL #-}

postTagsL :: Lens_' Post [Tag]
postTagsL f_acAM (Post x1_acAN x2_acAO x3_acAP x4_acAQ x5_acAR x6_acAS x7_acAT x8_acAU x9_acAV) =
  fmap
    (\y1_acAW ->
        Post x1_acAN x2_acAO x3_acAP x4_acAQ x5_acAR x6_acAS x7_acAT x8_acAU y1_acAW)
    (f_acAM x9_acAV)

{-# INLINE postTagsL #-}

postTimeL :: Lens_' Post UTCTime
postTimeL f_acAX (Post x1_acAY x2_acAZ x3_acB0 x4_acB1 x5_acB2 x6_acB3 x7_acB4 x8_acB5 x9_acB6) =
  fmap
    (\y1_acB7 ->
        Post x1_acAY x2_acAZ x3_acB0 x4_acB1 x5_acB2 y1_acB7 x7_acB4 x8_acB5 x9_acB6)
    (f_acAX x6_acB3)

{-# INLINE postTimeL #-}

postToReadL :: Lens_' Post Bool
postToReadL f_acB8 (Post x1_acB9 x2_acBa x3_acBb x4_acBc x5_acBd x6_acBe x7_acBf x8_acBg x9_acBh) =
  fmap
    (\y1_acBi ->
        Post x1_acB9 x2_acBa x3_acBb x4_acBc x5_acBd x6_acBe x7_acBf y1_acBi x9_acBh)
    (f_acB8 x8_acBg)

{-# INLINE postToReadL #-}

-- * PostDates
postDatesCountL :: Lens_' PostDates [(Day, Int)]
postDatesCountL f_a1M4D (PostDates x1_a1M4E x2_a1M4F x3_a1M4G) =
  fmap (\y1_a1M4H -> PostDates x1_a1M4E x2_a1M4F y1_a1M4H) (f_a1M4D x3_a1M4G)

{-# INLINE postDatesCountL #-}

postDatesTagL :: Lens_' PostDates Text
postDatesTagL f_a1M4I (PostDates x1_a1M4J x2_a1M4K x3_a1M4L) =
  fmap (\y1_a1M4M -> PostDates x1_a1M4J y1_a1M4M x3_a1M4L) (f_a1M4I x2_a1M4K)

{-# INLINE postDatesTagL #-}

postDatesUserL :: Lens_' PostDates Text
postDatesUserL f_a1M4N (PostDates x1_a1M4O x2_a1M4P x3_a1M4Q) =
  fmap (\y1_a1M4R -> PostDates y1_a1M4R x2_a1M4P x3_a1M4Q) (f_a1M4N x1_a1M4O)

{-# INLINE postDatesUserL #-}

-- * NoteList
noteListCountL :: Lens_' NoteList Int
noteListCountL f_acwZ (NoteList x1_acx0 x2_acx1) =
  fmap (\y1_acx2 -> NoteList y1_acx2 x2_acx1) (f_acwZ x1_acx0)

{-# INLINE noteListCountL #-}

noteListItemsL :: Lens_' NoteList [NoteListItem]
noteListItemsL f_acx3 (NoteList x1_acx4 x2_acx5) =
  fmap (\y1_acx6 -> NoteList x1_acx4 y1_acx6) (f_acx3 x2_acx5)

{-# INLINE noteListItemsL #-}

-- * NoteListItem
noteListItemCreatedAtL :: Lens_' NoteListItem UTCTime
noteListItemCreatedAtL f_acx0 (NoteListItem x1_acx1 x2_acx2 x3_acx3 x4_acx4 x5_acx5 x6_acx6) =
  fmap
    (\y1_acx7 -> NoteListItem x1_acx1 x2_acx2 x3_acx3 x4_acx4 y1_acx7 x6_acx6)
    (f_acx0 x5_acx5)

{-# INLINE noteListItemCreatedAtL #-}

noteListItemHashL :: Lens_' NoteListItem Text
noteListItemHashL f_acx8 (NoteListItem x1_acx9 x2_acxa x3_acxb x4_acxc x5_acxd x6_acxe) =
  fmap
    (\y1_acxf -> NoteListItem x1_acx9 y1_acxf x3_acxb x4_acxc x5_acxd x6_acxe)
    (f_acx8 x2_acxa)

{-# INLINE noteListItemHashL #-}

noteListItemIdL :: Lens_' NoteListItem Text
noteListItemIdL f_acxg (NoteListItem x1_acxh x2_acxi x3_acxj x4_acxk x5_acxl x6_acxm) =
  fmap
    (\y1_acxn -> NoteListItem y1_acxn x2_acxi x3_acxj x4_acxk x5_acxl x6_acxm)
    (f_acxg x1_acxh)

{-# INLINE noteListItemIdL #-}

noteListItemLengthL :: Lens_' NoteListItem Int
noteListItemLengthL f_acxo (NoteListItem x1_acxp x2_acxq x3_acxr x4_acxs x5_acxt x6_acxu) =
  fmap
    (\y1_acxv -> NoteListItem x1_acxp x2_acxq x3_acxr y1_acxv x5_acxt x6_acxu)
    (f_acxo x4_acxs)

{-# INLINE noteListItemLengthL #-}

noteListItemTitleL :: Lens_' NoteListItem Text
noteListItemTitleL f_acxw (NoteListItem x1_acxx x2_acxy x3_acxz x4_acxA x5_acxB x6_acxC) =
  fmap
    (\y1_acxD -> NoteListItem x1_acxx x2_acxy y1_acxD x4_acxA x5_acxB x6_acxC)
    (f_acxw x3_acxz)

{-# INLINE noteListItemTitleL #-}

noteListItemUpdatedAtL :: Lens_' NoteListItem UTCTime
noteListItemUpdatedAtL f_acxE (NoteListItem x1_acxF x2_acxG x3_acxH x4_acxI x5_acxJ x6_acxK) =
  fmap
    (\y1_acxL -> NoteListItem x1_acxF x2_acxG x3_acxH x4_acxI x5_acxJ y1_acxL)
    (f_acxE x6_acxK)

{-# INLINE noteListItemUpdatedAtL #-}

noteCreatedAtL :: Lens_' Note UTCTime
noteCreatedAtL f_acx6 (Note x1_acx7 x2_acx8 x3_acx9 x4_acxa x5_acxb x6_acxc x7_acxd) =
  fmap
    (\y1_acxe -> Note x1_acx7 x2_acx8 x3_acx9 x4_acxa x5_acxb y1_acxe x7_acxd)
    (f_acx6 x6_acxc)

{-# INLINE noteCreatedAtL #-}

noteHashL :: Lens_' Note Text
noteHashL f_acxf (Note x1_acxg x2_acxh x3_acxi x4_acxj x5_acxk x6_acxl x7_acxm) =
  fmap
    (\y1_acxn -> Note x1_acxg y1_acxn x3_acxi x4_acxj x5_acxk x6_acxl x7_acxm)
    (f_acxf x2_acxh)

{-# INLINE noteHashL #-}

noteIdL :: Lens_' Note Text
noteIdL f_acxo (Note x1_acxp x2_acxq x3_acxr x4_acxs x5_acxt x6_acxu x7_acxv) =
  fmap
    (\y1_acxw -> Note y1_acxw x2_acxq x3_acxr x4_acxs x5_acxt x6_acxu x7_acxv)
    (f_acxo x1_acxp)

{-# INLINE noteIdL #-}

noteLengthL :: Lens_' Note Int
noteLengthL f_acxx (Note x1_acxy x2_acxz x3_acxA x4_acxB x5_acxC x6_acxD x7_acxE) =
  fmap
    (\y1_acxF -> Note x1_acxy x2_acxz x3_acxA x4_acxB y1_acxF x6_acxD x7_acxE)
    (f_acxx x5_acxC)

{-# INLINE noteLengthL #-}

noteTextL :: Lens_' Note Text
noteTextL f_acxG (Note x1_acxH x2_acxI x3_acxJ x4_acxK x5_acxL x6_acxM x7_acxN) =
  fmap
    (\y1_acxO -> Note x1_acxH x2_acxI x3_acxJ y1_acxO x5_acxL x6_acxM x7_acxN)
    (f_acxG x4_acxK)

{-# INLINE noteTextL #-}

noteTitleL :: Lens_' Note Text
noteTitleL f_acxP (Note x1_acxQ x2_acxR x3_acxS x4_acxT x5_acxU x6_acxV x7_acxW) =
  fmap
    (\y1_acxX -> Note x1_acxQ x2_acxR y1_acxX x4_acxT x5_acxU x6_acxV x7_acxW)
    (f_acxP x3_acxS)

{-# INLINE noteTitleL #-}

noteUpdatedAtL :: Lens_' Note UTCTime
noteUpdatedAtL f_acxY (Note x1_acxZ x2_acy0 x3_acy1 x4_acy2 x5_acy3 x6_acy4 x7_acy5) =
  fmap
    (\y1_acy6 -> Note x1_acxZ x2_acy0 x3_acy1 x4_acy2 x5_acy3 x6_acy4 y1_acy6)
    (f_acxY x7_acy5)

{-# INLINE noteUpdatedAtL #-}

-- * Suggested (Prism)
popularP :: Prism_' Suggested [Text]
popularP =
  dimap
    (\x_acHs ->
        case x_acHs of
          (Popular y1_acHt) -> Right y1_acHt
          _ -> Left x_acHs)
    (either pure (fmap Popular)) .
  right'

{-# INLINE popularP #-}

recommendedP :: Prism_' Suggested [Text]
recommendedP =
  dimap
    (\x_acHv ->
        case x_acHv of
          (Recommended y1_acHw) -> Right y1_acHw
          _ -> Left x_acHv)
    (either pure (fmap Recommended)) .
  right'

{-# INLINE recommendedP #-}
