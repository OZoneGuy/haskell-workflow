{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}

module Types where

import Data.Map (Map)

-- | A Class implemented by all Resources
class ResourceClass res

-- | A data type used to group all types of resources under one type
data Resource = forall r. ResourceClass r => Resource {res :: r}

-- | Used to create new Resources
data NewResource = NewResource
  { -- | The resource Type, could be converted to an Enum
    resType :: String,
    -- | The resource Properties, consider compiling this into `data` for better validation
    properties :: Map String String
  }

-- | The Job type
data Job = Job
  { -- | The name of the job
    name :: String,
    -- | Possibly changed to an Enum
    language :: String,
    -- | The actual script
    body :: String,
    -- | The jobs this jobs depends on
    jobDependencies :: [Job],
    -- | The resources required by this job
    resDep :: [Resource],
    -- | The expected input for the job
    input :: [Type]
  }

defaultJob :: Job
defaultJob =
  Job
    { name = "",
      language = "bash",
      body = "echo \"Default job output...\"",
      jobDependencies = [],
      resDep = [],
      input = []
    }

data Execution = Parallel {jobs :: [Job]} | Sequence {jobs :: [Job]}

class TypeClass a where

data Type = forall a. TypeClass a => Type a

data NewType = NewType
  { name :: String,
    fields :: Map String String
  }
