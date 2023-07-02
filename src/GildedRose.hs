module GildedRose
  ( updateQuality
  , GildedRose
  , Item (..)
  , ValidItem
  , mkItem
  , mkInventory
  , getQuality
  , isItemPrefix
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

getQuality :: ValidItem -> Int
getQuality (MkValidItem (Item _ _ q)) = q

isItemPrefix :: String -> ValidItem -> Bool
isItemPrefix pref (MkValidItem (Item name _ _))
  = pref `isPrefixOf` name

mkItem :: Item -> Maybe ValidItem
mkItem (Item name sellIn quality)
  | "Sulfuras" `isPrefixOf` name = if quality /= 80
                                    then  Nothing
                                    else    Just 
                                          . MkValidItem 
                                          . Item name sellIn 
                                          $ 80  
  | name == ""  
  || quality < 0 
  || quality > 50 = Nothing  
  | otherwise = Just 
              . MkValidItem 
              . Item name sellIn 
              $ quality

mkInventory :: [Item] -> GildedRose
mkInventory = mapMaybe mkItem 

updateQuality :: GildedRose -> GildedRose
updateQuality = map updateQualityItem

updateQualityItem :: ValidItem -> ValidItem
updateQualityItem it@(MkValidItem (Item name sellIn quality)) 
  = case mkItem newItem of
      Nothing -> MkValidItem . Item name newSellIn $ quality
      Just it -> it  
  where
    newItem = Item name newSellIn newQuality

    newSellIn
     | "Sulfuras" `isItemPrefix` it = sellIn
     | otherwise =  sellIn - 1        

    newQuality        
      | "Sulfuras" `isItemPrefix` it = quality 
      | name == "Aged Brie" = quality + 1
      | ("Backstage" `isItemPrefix` it) 
      && (newSellIn < 0) = 0 
      | "Backstage" `isItemPrefix` it = quality + backstageQltAdjust newSellIn 
      | otherwise = quality - qltyAdjust newSellIn

    qltyAdjust sIn 
      | sIn < 0 = 2
      | otherwise = 1   
    
    backstageQltAdjust sIn 
      | sIn `elem` [1..5] = 3
      | sIn `elem` [6..10] = 2
      | otherwise = 1   

