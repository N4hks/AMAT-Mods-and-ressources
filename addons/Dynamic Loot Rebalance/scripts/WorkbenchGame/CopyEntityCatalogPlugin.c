[WorkbenchPluginAttribute(name: "Copy All Entity Catalogs", description: "Duplicates item catalogs to the active addon", wbModules: {"ResourceManager", "ScriptEditor"})]
class CopyAllEntityCatalogsPlugin : WorkbenchPlugin
{
    // =========================================================================
    // IMPORTANT: Change this to your exact Addon Project Name (the prefix)
    // =========================================================================
    const string MY_ADDON_NAME = "DynamicLootRebalance"; 

    override void Run()
    {
        Print("Starting Entity Catalog duplication...", LogLevel.NORMAL);
        
        // Define the known base game item catalogs with your valid GUIDs
        array<string> catalogPaths = {
            "{9D7E5804BB2E9B28}Configs/EntityCatalog/CIV/InventoryItems_EntityCatalog_CIV.conf",
            "{BB12292052E2F5B8}Configs/EntityCatalog/FactionLess/InventoryItems_EntityCatalog_Factionless.conf",
            "{E908001749419691}Configs/EntityCatalog/FIA/InventoryItems_EntityCatalog_FIA.conf",
            "{5F7EC52FC40A03E2}Configs/EntityCatalog/US/InventoryItems_EntityCatalog_US.conf",
            "{C53421647C3D0D2E}Configs/EntityCatalog/USSR/InventoryItems_EntityCatalog_USSR.conf"
        };

        int successCount = 0;

        foreach (string sourcePath : catalogPaths)
        {
            if (ProcessAndSaveCatalog(sourcePath))
            {
                successCount++;
            }
        }

        Print("Finished processing catalogs. Successfully copied: " + successCount, LogLevel.NORMAL);
    }

    bool ProcessAndSaveCatalog(string sourcePath)
    {
        // 1. Load the file as a Resource object first
        Resource resource = Resource.Load(sourcePath);
        if (!resource.IsValid())
        {
            Print("WARNING: Could not find or load resource: " + sourcePath, LogLevel.WARNING);
            return false;
        }

        // 2. Extract the BaseContainer from the resource safely
        BaseContainer catalogContainer = resource.GetResource().ToBaseContainer();
        if (!catalogContainer)
        {
            Print("WARNING: Could not extract BaseContainer from: " + sourcePath, LogLevel.WARNING);
            return false;
        }

        // Extract the raw file name (e.g., "InventoryItems_EntityCatalog_CIV.conf")
        int lastSlash = sourcePath.LastIndexOf("/");
        string fileName = sourcePath.Substring(lastSlash + 1, sourcePath.Length() - lastSlash - 1);

        // 3. Build the virtual file path
        string virtualFilePath = "$" + MY_ADDON_NAME + ":Configs/EntityCatalog/Copy/" + fileName;
        string absoluteTargetPath = string.Empty;
        
        // 4. Translate virtual path to absolute physical path using Workbench API
        Workbench.GetAbsolutePath(virtualFilePath, absoluteTargetPath);
Print("A :" + absoluteTargetPath);
        // If the file/folder doesn't exist yet, Workbench.GetAbsolutePath might fail on the file itself.
        // Fallback: Translate just the addon root folder virtual path if needed, or create directories first.
        if (absoluteTargetPath == string.Empty)
        {
            // Fallback translation using the addon root virtual path
            string virtualRootPath = "$" + MY_ADDON_NAME + ":";
            string absoluteRootPath = string.Empty;
            Workbench.GetAbsolutePath(virtualRootPath, absoluteRootPath);
Print("B :" + absoluteRootPath);

            if (absoluteRootPath != string.Empty)
            {
                absoluteRootPath.Replace("\\", "/");
                absoluteTargetPath = absoluteRootPath + "Configs/EntityCatalog/Copy/" + fileName;
            }
			Print("B :" + absoluteTargetPath);


        }

        if (absoluteTargetPath == string.Empty)
        {
            Print("ERROR: Could not resolve absolute path for: " + virtualFilePath, LogLevel.ERROR);
            return false;
        }

        // Create the directory structure on the hard drive if it doesn't exist yet
        int lastSlashTarget = absoluteTargetPath.LastIndexOf("/");
        string targetDir = absoluteTargetPath.Substring(0, lastSlashTarget);
		Print(targetDir);
        FileIO.MakeDirectory(targetDir);

        // Save the duplicated container
        bool success = BaseContainerTools.SaveContainer(catalogContainer, ResourceName.Empty, absoluteTargetPath);
        
        if (success)
        {
            Print("SUCCESS! Saved: " + absoluteTargetPath, LogLevel.NORMAL);
            return true;
        }
        
        Print("ERROR: Failed to save catalog to: " + absoluteTargetPath, LogLevel.ERROR);
        return false;
    }
}