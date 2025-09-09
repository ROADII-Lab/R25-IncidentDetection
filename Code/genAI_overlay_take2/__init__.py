from . import server2
import asyncio

def main():
    """Main entry point for the package."""
    asyncio.run(server2.main())

# Optionally expose other important items at package level
__all__ = ['main', 'server2']