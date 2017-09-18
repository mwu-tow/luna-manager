module Luna.Manager.Command.Options where

import Prologue

import Data.Text (Text)
import Options.Applicative as Opts
import Control.Monad.State.Layered

import qualified Luna.Manager.System.Info as Info


------------------------------
-- === Command options  === --
------------------------------

-- === Definition === --

data Options = Options
    { _globals :: GlobalOpts
    , _command :: Command
    } deriving (Show)

data GlobalOpts = GlobalOpts
    { _batchMode      :: Bool
    , _guiInstaller   :: Bool
    } deriving (Show)

data Command = Install       InstallOpts
             | Update
             | SwitchVersion SwitchVersionOpts
             | Develop       DevelopOpts
             | MakePackage   MakePackageOpts
             | Info
             deriving (Show)

data InstallOpts = InstallOpts
    { _selectedComponent        :: Maybe Text
    , _selectedVersion          :: Maybe Text
    , _selectedInstallationPath :: Maybe Text
    , _nightlyInstallation      :: Bool
    } deriving (Show)

data MakePackageOpts = MakePackageOpts
    { _cfgPath :: Text
    , _verbose :: Bool
    } deriving (Show)

data SwitchVersionOpts = SwitchVersionOpts
    { _targetVersion :: Text
    } deriving (Show)

data DevelopOpts = DevelopOpts
    { _target               :: Text
    , _repositoryPath       :: Maybe Text
    , _downloadDependencies :: Bool
    } deriving (Show)

makeLenses ''GlobalOpts
makeLenses ''Options
makeLenses ''InstallOpts
makeLenses ''MakePackageOpts
makeLenses ''SwitchVersionOpts
makeLenses ''DevelopOpts


-- === Instances === --

instance Default InstallOpts where def = InstallOpts def def def False



------------------------------
-- === Argument parsing === --
------------------------------

-- === Parsers === --

evalOptionsParserT :: MonadIO m => StateT Options m a -> m a
evalOptionsParserT m = evalStateT m =<< parseOptions

parseOptions :: MonadIO m => m Options
parseOptions = liftIO $ customExecParser (prefs showHelpOnEmpty) optsParser where
    commands           = mconcat [cmdInstall, cmdMkpkg, cmdUpdate, cmdDevelop, cmdSwitchVersion, cmdInfo]
    optsParser         = info (helper <*> optsProgram) (fullDesc <> header ("Luna ecosystem manager (" <> Info.version <> ")") <> progDesc Info.synopsis)

    -- Commands
    cmdInstall         = Opts.command "install"        . info optsInstall       $ progDesc "Install components"
    cmdUpdate          = Opts.command "update"         . info (pure Update)     $ progDesc "Update components"
    cmdSwitchVersion   = Opts.command "switch-version" . info optsSwitchVersion $ progDesc "Switch installed component version"
    cmdDevelop         = Opts.command "develop"        . info optsDevelop       $ progDesc "Setup development environment"
    cmdMkpkg           = Opts.command "make-package"   . info optsMkpkg         $ progDesc "Prepare installation package"
    cmdInfo            = Opts.command "info"           . info (pure Info)       $ progDesc "Shows environment information"

    -- Options
    optsProgram        = Options           <$> optsGlobal <*> hsubparser commands
    optsGlobal         = GlobalOpts        <$> Opts.switch (long "batch" <> help "Do not run interactive mode")
                                           <*> Opts.switch (long "gui" )
    optsMkpkg          = MakePackage       <$> optsMkpkg'
    optsMkpkg'         = MakePackageOpts   <$> strArgument (metavar "CONFIG"  <> help "Config file path")
                                           <*> Opts.switch (long "verbose")
    optsSwitchVersion  = SwitchVersion     <$> optsSwitchVersion'
    optsSwitchVersion' = SwitchVersionOpts <$> strArgument (metavar "VERSION" <> help "Target version")
    optsDevelop        = Develop           <$> optsDevelop'
    optsDevelop'       = DevelopOpts       <$> (strArgument $ metavar "TARGET" <> help "Config file path")
                                           <*> (optional . strOption $ long "path"      <> metavar "PATH"      <> help "Repository path" )
                                           <*> Opts.switch (long "download-dependencies")
    optsInstall        = Install           <$> optsInstall'
    optsInstall'       = InstallOpts       <$> (optional . strOption $ long "component" <> metavar "COMPONENT" <> help "Component to install")
                                           <*> (optional . strOption $ long "version"   <> metavar "VERSION"   <> help "Version to install"  )
                                           <*> (optional . strOption $ long "path"      <> metavar "PATH"      <> help "Installation path"   )
                                           <*> Opts.switch (long "nightly")
