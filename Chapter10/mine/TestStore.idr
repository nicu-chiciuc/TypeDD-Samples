import DataStore

testStore : (DataStore (SString .+. SString .+. SInt))
testStore = addToStore ("Mercury", "Mariner 10", 1974) $
            addToStore ("Venus", "Venera", 1961) $
            addToStore ("Uranus", "Voyager 2", 1986) $
            addToStore ("Pluto", "New Horizons", 2015) $
            empty

listItems : DataStore schema -> List (SchemaType schema)
listItems input with (storeView input)
  listItems input | SNil = []
  listItems (addToStore value store) | (SAdd rec) = value :: listItems store | rec
