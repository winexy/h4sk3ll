ifEven f x = if even x then f x else x
genIfXEven x = (\f -> ifEven f x)

getRequestURL host apiKey resource id = 
  host ++
  "/" ++
  resource ++
  "/" ++
  id ++
  "?token=" ++
  apiKey

exampleUrlBuilder = getRequestURL "htts://example.com"

genApiRequestBuilder hostBuilder apiKey = (\resource id -> 
  hostBuilder apiKey resource id)

genApiResourceBuilder apiRequestBuilder resource = (\id ->
  apiRequestBuilder resource id)

myExampleUrlBuilder = exampleUrlBuilder "h4sk3ll"

bookResource = getRequestUrl "https://example.com" "h4sk3ll" "book"

-- print (getRequestURL "http://example.com" "123hAsk3ll" "book" "1234")

flipBinaryArgs binaryFunction = (\x y -> binaryFunction y x)


binaryPartialApplication fn x = (\y -> fn x y)