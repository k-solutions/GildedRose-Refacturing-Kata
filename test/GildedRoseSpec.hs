module GildedRoseSpec (spec) where

import Test.Hspec
import GildedRose
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Data.Maybe (fromJust)

instance Arbitrary Item where
    arbitrary =validItem 
instance Arbitrary ValidItem where
    arbitrary = fromJust . mkItem <$> arbitrary
          

validQuality :: Gen Int
validQuality = choose (0, 50)

validSellIn :: Gen Int
validSellIn = choose (-10, 60)

validItem :: Gen Item
validItem = frequency 
          [ (1,  Item "Sulfuras, Hand of Ragnaros" <$> validSellIn <*> pure 80)
          , (5,  Item "Backstage passes to a TAFKAL80ETC concert" <$> validSellIn <*> validQuality)
          , (10, Item "Aged Brie" <$> validSellIn <*> validQuality)
          , (50, Item "Foo Rose" <$> validSellIn <*> validQuality)
          ]


prop_MakeItem :: Item -> Bool -- Property
prop_MakeItem item -- = collect (show item)
  = not . null . mkInventory 
  $  [ item ]  

prop_PositiveQuality :: ValidItem -> Bool  -- Property
prop_PositiveQuality item -- = collect (show item)
  = getQuality (head $ updateQuality [item]) >= 0 

prop_QualityMax :: ValidItem -> Property
prop_QualityMax item 
  =  not ("Sulfuras" `isItemPrefix` item) 
  ==> getQuality (head $ updateQuality [ item ]) < 51


spec :: Spec
spec =
  describe "updateQuality" $ do
    
    it "Backstage passes Quality increases by 3 when sellIn between [1...5]" $
       let inventory = mkInventory [Item "Backstage passes to a TAFKAL80ETC concert" 5 10]
           actual = updateQuality inventory
           expected = mkInventory [Item "Backstage passes to a TAFKAL80ETC concert" 4 13]
       in actual `shouldBe` expected

    it "Backstage passes Quality increases by 2 when sellIn between [5...10]" $
       let inventory = mkInventory [Item "Backstage passes to a TAFKAL80ETC concert" 10 10]
           actual = updateQuality inventory
           expected = mkInventory [Item "Backstage passes to a TAFKAL80ETC concert" 9 12]
       in actual `shouldBe` expected

    it "Backstage passes Quality increases" $
       let inventory = mkInventory [Item "Backstage passes to a TAFKAL80ETC concert" 12 10]
           actual = updateQuality inventory
           expected = mkInventory [Item "Backstage passes to a TAFKAL80ETC concert" 11 11]
       in actual `shouldBe` expected

    it "Backstage passes Quality drops to 0 after the concert" $
       let inventory = mkInventory [Item "Backstage passes to a TAFKAL80ETC concert" 0 10]
           actual = updateQuality inventory
           expected = mkInventory [Item "Backstage passes to a TAFKAL80ETC concert" (-1) 0]
       in actual `shouldBe` expected


    it "Sulfuras Quality is 80 and it never alters" $
       let inventory = mkInventory [Item "Sulfuras, Hand of Ragnaros" 1 80]
           actual = updateQuality . updateQuality $ inventory
           expected = mkInventory [Item "Sulfuras, Hand of Ragnaros" 1 80]
       in actual `shouldBe` expected   


    it "Sulfuras Quality could not be different than 80" $
       let inventory = mkInventory [Item "Sulfuras, Hand of Ragnaros" 1 15]
           actual = updateQuality inventory
           expected = mkInventory []
       in actual `shouldBe` expected   


    it "Aged Brie Quality increases the older it gets" $
       let inventory = mkInventory [Item "Aged Brie" 1 15]
           actual = updateQuality inventory
           expected = mkInventory [Item "Aged Brie" 0 16]
       in actual `shouldBe` expected   

    it "Daily Item update could not be over 50" $
       let inventory = mkInventory [Item "foo" 1 55]
           actual = updateQuality inventory
           expected = mkInventory []
       in actual `shouldBe` expected   

    it "Daily Item update both SellIn value and Quality" $
       let inventory = mkInventory [Item "foo" 1 1]
           actual = updateQuality inventory
           expected = mkInventory [Item "foo" 0 0]
       in actual `shouldBe` expected

    it "Selling Date passed Quality degrades twice as fast" $
       let inventory = mkInventory [Item "foo" 0 10]
           actual = updateQuality inventory
           expected = mkInventory [Item "foo" (-1) 8]
       in actual `shouldBe` expected

    it "Daily Item update SellIn value, but Quality as it is Min" $
       let inventory = mkInventory [Item "foo" 0 0]
           actual = updateQuality inventory
           expected = mkInventory [Item "foo" (-1) 0]
       in actual `shouldBe` expected

    prop "Generated Items are valid" prop_MakeItem
    
    prop "Updated quality is never negative" prop_PositiveQuality

    prop "Quality has not got over Max" prop_QualityMax
