module ResourceNames where

-- Resource Names
data ResourceNames = JobNameField
                   | BusinessValueField
                   | TimeCriticalityField
                   | RROEField
                   | JobSizeField
                   | ListItem
                   | FilenameField
                   deriving (Eq, Ord, Show)
