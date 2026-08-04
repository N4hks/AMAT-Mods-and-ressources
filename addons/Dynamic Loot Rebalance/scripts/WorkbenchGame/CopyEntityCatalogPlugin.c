[WorkbenchPluginAttribute(name: "Copy All Entity Catalogs", description: "Duplicates item catalogs from all mods and base game into separate mod folders", wbModules: {"ResourceManager", "ScriptEditor"})]
class CopyAllEntityCatalogsPlugin : WorkbenchPlugin
{
    // =========================================================================
    // CONFIGURATION
    // =========================================================================
    const string MY_ADDON_NAME = "DynamicLootRebalance"; 
    
    // Set to true to store base game (ArmaReforger) files directly in Copy/
    // Set to false to store them inside Copy/ArmaReforger/
    const bool SKIP_BASE_GAME_FOLDER = true; 

    ref array<string> m_aCatalogPaths = {};

    override void Run()
    {
        Print("Starting Entity Catalog duplication...", LogLevel.NORMAL);
        
        m_aCatalogPaths.Clear();

        // Search for all .conf files across loaded mods and base game
        SearchResourcesFilter filter = new SearchResourcesFilter();
        filter.fileExtensions = {"conf"};
        ResourceDatabase.SearchResources(filter, OnResourceFound);

        Print(string.Format("Found %1 entity catalog files across loaded mods.", m_aCatalogPaths.Count()), LogLevel.NORMAL);

        int successCount = 0;
        foreach (string exactPath : m_aCatalogPaths)
        {
            if (ProcessAndCopyCatalog(exactPath))
            {
                successCount++;
            }
        }

        Print(string.Format("Finished! Successfully saved %1 / %2 catalog files.", successCount, m_aCatalogPaths.Count()), LogLevel.NORMAL);
    }

    void OnResourceFound(ResourceName resourceName, string exactPath)
    {
        string pathLower = exactPath;
        pathLower.ToLower();

        if (pathLower.Contains("entitycatalog") && pathLower.EndsWith(".conf"))
        {
            if (m_aCatalogPaths.Find(exactPath) == -1)
            {
                m_aCatalogPaths.Insert(exactPath);
            }
        }
    }

    bool ProcessAndCopyCatalog(string exactPath)
    {
        // 1. Load Resource handle
        Resource resource = BaseContainerTools.LoadContainer(exactPath);
        if (!resource || !resource.IsValid())
        {
            Print("ERROR: Could not load resource from path: " + exactPath, LogLevel.ERROR);
            return false;
        }

        // 2. Extract BaseContainer from Resource
        BaseContainer container = resource.GetResource().ToBaseContainer();
        if (!container)
        {
            Print("ERROR: Could not extract BaseContainer from path: " + exactPath, LogLevel.ERROR);
            return false;
        }

        string modName = GetModNameFromPath(exactPath);
        string relativePath = GetRelativeCatalogPath(exactPath);

        // Determine destination subfolder
        string modFolder = "";
        if (!SKIP_BASE_GAME_FOLDER || modName != "ArmaReforger")
        {
            modFolder = modName + "/";
        }

        // Resolve active addon root path
        string virtualRootPath = "$" + MY_ADDON_NAME + ":";
        string absoluteRootPath = string.Empty;
        Workbench.GetAbsolutePath(virtualRootPath, absoluteRootPath);

        if (absoluteRootPath.IsEmpty())
        {
            Print("ERROR: Could not resolve project root path for $" + MY_ADDON_NAME + ":", LogLevel.ERROR);
            return false;
        }

        absoluteRootPath.Replace("\\", "/");
        if (!absoluteRootPath.EndsWith("/"))
            absoluteRootPath += "/";

        // Build output path: <AddonRoot>/Configs/EntityCatalog/Copy/[ModName/]<RelativePath>
        string absoluteTargetPath = absoluteRootPath + "Configs/EntityCatalog/Copy/" + modFolder + relativePath;

        // Create target directory structure
        int lastSlash = absoluteTargetPath.LastIndexOf("/");
        if (lastSlash != -1)
        {
            string targetDir = absoluteTargetPath.Substring(0, lastSlash);
            FileIO.MakeDirectory(targetDir);
        }

        // Save container copy
        bool success = BaseContainerTools.SaveContainer(container, ResourceName.Empty, absoluteTargetPath);
        if (success)
        {
            Print(string.Format("SUCCESS [%1]: Saved to %2", modName, absoluteTargetPath), LogLevel.NORMAL);
            return true;
        }

        Print("ERROR: Failed to save catalog container to: " + absoluteTargetPath, LogLevel.ERROR);
        return false;
    }

    string GetModNameFromPath(string path)
    {
        path.Replace("\\", "/");

        // Format 1: Virtual path "$ModName:Configs/..."
        if (path.StartsWith("$"))
        {
            int colonIdx = path.IndexOf(":");
            if (colonIdx > 1)
            {
                return path.Substring(1, colonIdx - 1);
            }
        }

        // Format 2: Physical disk path ".../ModName/Configs/..."
        string lowerPath = path;
        lowerPath.ToLower();
        int configsIdx = lowerPath.IndexOf("/configs/");
        if (configsIdx != -1)
        {
            string sub = path.Substring(0, configsIdx);
            int lastSlash = sub.LastIndexOf("/");
            if (lastSlash != -1)
            {
                return sub.Substring(lastSlash + 1, sub.Length() - lastSlash - 1);
            }
            return sub;
        }

        return "ArmaReforger";
    }

    string GetRelativeCatalogPath(string path)
    {
        path.Replace("\\", "/");

        // Strip $ModName: prefix
        if (path.StartsWith("$"))
        {
            int colonIdx = path.IndexOf(":");
            if (colonIdx != -1)
                path = path.Substring(colonIdx + 1, path.Length() - colonIdx - 1);
        }

        string lowerPath = path;
        lowerPath.ToLower();

        int catalogIdx = lowerPath.IndexOf("configs/entitycatalog/");
        if (catalogIdx != -1)
        {
            return path.Substring(catalogIdx + 22, path.Length() - (catalogIdx + 22));
        }

        int configsIdx = lowerPath.IndexOf("configs/");
        if (configsIdx != -1)
        {
            return path.Substring(configsIdx + 8, path.Length() - (configsIdx + 8));
        }

        int lastSlash = path.LastIndexOf("/");
        if (lastSlash != -1)
            return path.Substring(lastSlash + 1, path.Length() - lastSlash - 1);

        return path;
    }
}