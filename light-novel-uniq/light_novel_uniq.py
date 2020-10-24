"""
Module/Script to generate hashes of files (recursively) and calculate
unique files by these hashes
"""


import logging
import pathlib
import hashlib
import re
from typing import *
import json

## ========================================================================


def _log():
    return logging.getLogger( __name__ )


## ========================================================================


class FileHash( object ):
    """
    Represents a ash for a file, including the path to the source file
    and the path to the stored hash file.
    """

    def __init__( self,
                  hash_value: str,
                  source_file_path: pathlib.Path,
                  hash_file_path: pathlib.Path ) -> None:
        """
        Source file is that path for which the hash was computed.
        Hash File path is the file that stores the hash value.
        Hash value is the has itself.
        """
        self.hash_value = hash_value
        self.source_file_path = source_file_path
        self.hash_file_path = hash_file_path


## ========================================================================


class FileLocations( object ):
    """
    Represents the location(s) for a particular file.
    """

    def __init__(self, hashes: List[FileHash] ) -> None:
        """
        Given a list of file hashes, builds up a list of locations
        """
        all_hash_values = set(map(lambda h: h.hash_value, hashes))
        assert len(all_hash_values) == 1
        self.hash_value = list(all_hash_values)[0]
        self.locations = [ h.source_file_path for h in hashes ]
        self.hash_files = [ h.hash_file_path for h in hashes ]
        


## ========================================================================


def ensure_hash_for_file(
        path: pathlib.Path,
        hash_fuction: Callable[[pathlib.Path], str],
        hash_location_gen: Callable[[pathlib.Path], pathlib.Path]) -> FileHash:
    """
    """

    hash_file_path = hash_location_gen( path )
    if not hash_file_path.exists():
        compute_and_store_hash_for_file(
            path,
            hash_fuction,
            hash_file_path )
    if not hash_file_path.exists():
        raise RuntimeError( "Hash file '{}' not found!".format( hash_file_path))
    return FileHash(
        read_hash_file( hash_file_path ),
        path,
        hash_file_path )


## ========================================================================


def read_hash_file( path: pathlib.Path ) -> str:
    """
    reads in the hash value from the given path, which must exist
    """
    with open( path, 'rb' ) as f:
        hash_value = f.read().decode('utf8')
    return hash_value


## ========================================================================


def compute_and_store_hash_for_file(
        source_path: pathlib.Path,
        hash_fuction: Callable[[pathlib.Path],str],
        output_path: pathlib.Path) -> None:
    """
    Computes and stores the hash value of the source file.
    This will overwrite output file if it exists
    """

    with open( output_path, 'wb' ) as out_f:
        hash_value = hash_fuction( source_path )
        out_f.write( hash_value.encode('utf8') )

## ========================================================================


def sha256_hashfunction( path: pathlib.Path ) -> str:
    """
    computes the sha256 hash for the given file (by path )
    """
    with open( path, 'rb' ) as f:
        return hashlib.sha256( f.read() ).hexdigest()


## ========================================================================


def generate_hash_location_as_sibling(
        path: pathlib.Path ) -> pathlib.Path :
    """
    Given a path, generate a location for that file's hash file as a 
    simple siblin of the path with a .hash extension
    """
    return pathlib.Path( pathlib.Path(path).as_posix() + ".hash" )

## ========================================================================


def recursive_path_ensure_hashes(
        base_path: pathlib.Path,
        path_filter: Callable[[pathlib.Path],bool],
        hash_fuction: Callable[[pathlib.Path],str],
        hash_location_gen: Callable[[pathlib.Path],pathlib.Path]
) -> List[FileHash]:
    """
    Recurse down the base direcgtory.
    For each file that passes the filter (for filter(path) == True)
    we compute a hash for that file.
    We return the list of FileHash computed this way
    """
    hashes = []
    for p in pathlib.Path( base_path ).rglob('*'):
        if p.is_file() and path_filter(p):
            hashes.append( ensure_hash_for_file( p,
                                                 hash_fuction,
                                                 hash_location_gen ) )
        elif p.is_dir():
            _log().info( "Started directory '%s'", p )
    return hashes

## ========================================================================


def group_hashes( hashes: List[FileHash] ) -> List[ FileLocations ]:
    """
    Takes a list of hashes and forms a list of FileLocations from them.
    """

    hash_value_to_hashes = {}
    for h in hashes:
        value = h.hash_value
        if value not in hash_value_to_hashes:
            hash_value_to_hashes[value] = []
        hash_value_to_hashes[value].append( h )
    locations = [ FileLocations( hs )
                  for (value, hs) in hash_value_to_hashes.items() ]
    return locations

## ========================================================================


def save_locations_to_json( path: pathlib.Path,
                            locations: List[FileLocations] ) -> None:
    """
    Save it as a simply json list of tuples [hash,sources,hashfiles]
    """
    jsonified = [
        [ loc.hash_value,
          [ l.as_posix() for l in loc.locations ],
          [ l.as_posix() for l in loc.hash_files ] ]
        for loc in locations]
    with open( path, 'w' ) as f:
        json.dump( jsonified, f )


## ========================================================================
## ========================================================================
## ========================================================================
## ========================================================================
## ========================================================================
## ========================================================================
## ========================================================================
