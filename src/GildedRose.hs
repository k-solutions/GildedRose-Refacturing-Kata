module GildedRose
  ( updateQuality
  , GildedRose
  , Item (..)
  , ValidItem
  , mkItem
  , mkInventory
  )where

import Data.List (isPrefixOf)
import Data.Maybe (mapMaybe)

type GildedRose = [ValidItem]

newtype ValidItem = MkValidItem Item 
                  deriving Eq   

data Item = Item String Int Int
  deriving (Eq)

instance Show Item where
  show (Item name sellIn quality) =
    name ++ ", " ++ show sellIn ++ ", " ++ show quality
instance  Show ValidItem where
    show (MkValidItem item) = show item

mkItem :: Item -> Maybe ValidItem
mkItem (Item name sellIn quality)
  | "Sulfuras" `isPrefixOf` name
  && quality /= 80  = Nothing   
  | name == ""  
  || quality < 0 
  || quality > 50 = Nothing  
  | otherwise = Just . MkValidItem 
              . Item name sellIn $ quality

mkInventory :: [Item] -> GildedRose
mkInventory = mapMaybe mkItem 

updateQuality :: GildedRose -> GildedRose
updateQuality = map updateQualityItem

updateQualityItem :: ValidItem -> ValidItem
updateQualityItem (MkValidItem (Item name sellIn quality)) 
  = case mkItem newItem of
      Nothing -> MkValidItem . Item name newSellIn $ quality
      Just it -> it  
  where
    newItem = Item name newSellIn newQuality

    newSellIn
     | "Sulfuras" `isPrefixOf` name = sellIn
     | otherwise =  sellIn - 1        

    newQuality        
      | "Sulfuras" `isPrefixOf` name = quality 
      | name == "Aged Brie" = quality + 1
      | ("Backstage" `isPrefixOf` name) 
      && (newSellIn < 0) = 0 

      | "Backstage" `isPrefixOf` name = quality + backstageQltAdjust newSellIn 
      | otherwise = quality - qltyAdjust newSellIn
    qltyAdjust sIn 
      | sIn < 0 = 2
      | otherwise = 1   
    
    backstageQltAdjust sIn 
      | sIn `elem` [1..5] = 3
      | sIn `elem` [6..10] = 2
      | otherwise = 1   

--  let
--    quality' =
--      if name /= "Aged Brie"
--         && name /= "Backstage passes to a TAFKAL80ETC concert"
--      then
--        if quality > 0
--        then
--          if name /= "Sulfuras, Hand of Ragnaros"
--          then quality - 1
--          else quality
--        else quality
--      else
--        if quality < 50
--        then
--          quality + 1 +
--            (if name == "Backstage passes to a TAFKAL80ETC concert"
--             then
--               if sellIn < 11
--               then
--                 if quality < 49
--                 then
--                   1 + (if sellIn < 6
--                        then
--                          if quality < 48
--                          then 1
--                          else 0
--                        else 0)
--                 else 0
--               else 0
--             else 0)
--        else quality
--
--  in
--    case mkItem  
--      (if sellIn' < 0
--      then
--        if name /= "Aged Brie"
--        then
--          if name /= "Backstage passes to a TAFKAL80ETC concert"
--          then
----              if quality' > 0
----              then
--              if name /= "Sulfuras, Hand of Ragnaros"
--              then Item name sellIn' (quality' - 1)
--              else Item name sellIn' quality'
----              else Item name sellIn' quality'
--          else Item name sellIn' (quality' - quality')
--        else
--          if quality' < 50
--          then Item name sellIn' (quality' + 1)
--          else Item name sellIn' quality'
--      else Item name sellIn' quality') 
--    of
--      Nothing -> MkValidItem . Item name sellIn' $ quality
--      Just it -> it  
--
