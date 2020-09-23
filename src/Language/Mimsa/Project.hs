module Language.Mimsa.Project
  ( module Language.Mimsa.Project.Persistence,
    module Language.Mimsa.Project.Default,
  )
where

import Language.Mimsa.Project.Default (defaultProject)
import Language.Mimsa.Project.Persistence
  ( getCurrentBindings,
    getCurrentTypeBindings,
    loadProject,
    saveProject,
  )
