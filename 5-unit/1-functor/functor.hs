reverseMaybe :: Maybe String -> Maybe String
reverseMaybe (Just s) = Just $ reverse s
reverseMaybe Nothing = Nothing

successfulRequest :: Maybe Int
successfulRequest = Just 6

failedRequest :: Maybe Int
failedRequest = Nothing


successStr :: Maybe String
successStr = show <$> successfulRequest

failStr :: Maybe String
failStr = show <$> failedRequest