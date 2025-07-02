"""
Command-line interface for ClaudeLang package manager
"""

import argparse
import sys
from pathlib import Path
from typing import List, Optional

from .package_manager import PackageManager


def main(args: Optional[List[str]] = None) -> int:
    """Main CLI entry point"""
    parser = argparse.ArgumentParser(
        prog="claude-pkg",
        description="ClaudeLang package manager"
    )
    
    subparsers = parser.add_subparsers(dest="command", help="Available commands")
    
    # init command
    init_parser = subparsers.add_parser("init", help="Initialize a new package")
    init_parser.add_argument("--name", help="Package name")
    init_parser.add_argument("--version", default="0.1.0", help="Initial version")
    init_parser.add_argument("--description", help="Package description")
    init_parser.add_argument("--author", help="Package author")
    init_parser.add_argument("--main", default="src/main.cl", help="Main entry point")
    
    # install command
    install_parser = subparsers.add_parser("install", help="Install packages")
    install_parser.add_argument("package", nargs="?", help="Package name to install")
    install_parser.add_argument("--version", help="Specific version to install")
    install_parser.add_argument("--save", action="store_true", default=True,
                               help="Add to dependencies (default)")
    install_parser.add_argument("--save-dev", action="store_true",
                               help="Add to dev dependencies")
    install_parser.add_argument("--no-save", dest="save", action="store_false",
                               help="Don't add to dependencies")
    install_parser.add_argument("--force", "-f", action="store_true",
                               help="Force reinstall")
    
    # uninstall command
    uninstall_parser = subparsers.add_parser("uninstall", help="Uninstall a package")
    uninstall_parser.add_argument("package", help="Package name to uninstall")
    uninstall_parser.add_argument("--no-save", dest="save", action="store_false",
                                 default=True, help="Don't remove from dependencies")
    
    # update command
    update_parser = subparsers.add_parser("update", help="Update packages")
    update_parser.add_argument("package", nargs="?", help="Specific package to update")
    
    # list command
    list_parser = subparsers.add_parser("list", help="List installed packages")
    list_parser.add_argument("--global", "-g", dest="global_", action="store_true",
                            help="List globally installed packages")
    
    # search command
    search_parser = subparsers.add_parser("search", help="Search for packages")
    search_parser.add_argument("query", help="Search query")
    search_parser.add_argument("--limit", type=int, default=20,
                              help="Maximum results to show")
    
    # publish command
    publish_parser = subparsers.add_parser("publish", help="Publish a package")
    publish_parser.add_argument("--path", type=Path, default=Path.cwd(),
                               help="Package directory")
    
    # run command
    run_parser = subparsers.add_parser("run", help="Run a package script")
    run_parser.add_argument("script", help="Script name")
    run_parser.add_argument("args", nargs="*", help="Script arguments")
    
    # clean command
    clean_parser = subparsers.add_parser("clean", help="Remove unused packages")
    
    # cache command
    cache_parser = subparsers.add_parser("cache", help="Manage package cache")
    cache_subparsers = cache_parser.add_subparsers(dest="cache_command")
    cache_subparsers.add_parser("clear", help="Clear the cache")
    
    # Parse arguments
    args = parser.parse_args(args)
    
    # Create package manager
    pm = PackageManager()
    
    # Execute command
    try:
        if args.command == "init":
            kwargs = {}
            if args.name:
                kwargs["name"] = args.name
            if args.description:
                kwargs["description"] = args.description
            if args.author:
                kwargs["author"] = args.author
            if args.main:
                kwargs["main"] = args.main
            kwargs["version"] = args.version
            
            pm.init(**kwargs)
            
        elif args.command == "install":
            if args.package:
                pm.install(
                    args.package,
                    args.version,
                    save=args.save and not args.save_dev,
                    save_dev=args.save_dev,
                    force=args.force
                )
            else:
                pm.install(force=args.force)
                
        elif args.command == "uninstall":
            pm.uninstall(args.package, save=args.save)
            
        elif args.command == "update":
            pm.update(args.package)
            
        elif args.command == "list":
            pm.list(global_=args.global_)
            
        elif args.command == "search":
            pm.search(args.query, args.limit)
            
        elif args.command == "publish":
            pm.publish(args.path)
            
        elif args.command == "run":
            pm.run(args.script, *args.args)
            
        elif args.command == "clean":
            pm.clean()
            
        elif args.command == "cache":
            if args.cache_command == "clear":
                pm.cache_clear()
            else:
                print("Usage: claude-pkg cache clear")
                return 1
                
        else:
            parser.print_help()
            return 1
            
        return 0
        
    except KeyboardInterrupt:
        print("\nOperation cancelled")
        return 130
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())