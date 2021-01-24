data Dog = Dog 
data Cat = Cat  

handleCat :: Animal -> Maybe Cat
handleCat = undefined  

handleDog :: Animal -> Maybe Dog 
handleDog = undefined 

data Animal =  ACat  |  ADog  

-- If you really want the individual types, you could do the following:
-- data Dog a = Dog a
-- data Cat a = Cat a

-- data Animal a = AnimalDog (Dog a) | AnimalCat (Cat a)

-- you can use gadts here ->
-- data AnimalType = Dog | Cat

-- data Animal (a :: AnimalType) where
--    Dog :: Animal Dog
--    Cat :: Animal Cat