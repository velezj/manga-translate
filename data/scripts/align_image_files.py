"""
Basic scripts to take raw and trsnlated image directories and find alignment
by filename based on image similarities
"""

import logging
import pathlib
from typing import Optional, Callable, Sequence, Tuple, Any, Dict
import datetime

import skimage
import skimage.color
import skimage.io
import skimage.transform
import skimage.metrics
import skimage.feature

import dill

## ======================================================================

def _log():
    return logging.getLogger( __name__ )

## ======================================================================

def compute_similarity(
        image_a,
        image_b ) -> Tuple[ float, Dict ]:
    """
    Given two images (float grayscale),
    returns the similarity score,
    from 0 (not similar)
    to 1 (same image).
    
    Also returns a dictionary with debug information
    """

    # min_shape = ( min( image_a.shape[0], image_b.shape[0] ),
    #               min( image_a.shape[1], image_b.shape[1] ) )
    # scaled_a = skimage.transform.resize( image_a, min_shape,
    #                                      anti_aliasing=True )
    # scaled_b = skimage.transform.resize( image_b, min_shape,
    #                                      anti_aliasing=True )

    # sim = skimage.metrics.structural_similarity( scaled_a, scaled_b )

    info = {}

    corner_keypoints_a = skimage.feature.corner_peaks(
        skimage.feature.corner_harris( image_a ),
        min_distance = 5,
        threshold_rel = 0.1 )
    corner_keypoints_b = skimage.feature.corner_peaks(
        skimage.feature.corner_harris( image_b ),
        min_distance = 5,
        threshold_rel = 0.1 )
    info['corner_keypoints_a'] = corner_keypoints_a
    info['corner_keypoints_b'] = corner_keypoints_b

    extractor = skimage.feature.BRIEF()
    extractor.extract( image_a, corner_keypoints_a )
    keypoints_a = corner_keypoints_a[ extractor.mask ]
    descriptors_a = extractor.descriptors
    extractor.extract( image_b, corner_keypoints_b )
    keypoints_b = corner_keypoints_b[ extractor.mask ]
    descriptors_b = extractor.descriptors
    info['keypoints_a'] = keypoints_a
    info['keypoints_b'] = keypoints_b
    info['descriptors_a'] = descriptors_a
    info['descriptors_b'] = descriptors_b

    warped_image_a = None
    use_raw_scaling = False
    if descriptors_a.shape[0] > 5 and descriptors_b.shape[0] > 5:

        matches = skimage.feature.match_descriptors(
            descriptors_a,
            descriptors_b,
            cross_check=True )
        matchpoints_a = keypoints_a[ matches[:,0] ]
        matchpoints_b = keypoints_b[ matches[:,1] ]


        estimated_transform = skimage.transform.SimilarityTransform()
        success = estimated_transform.estimate( matchpoints_a,
                                                matchpoints_b )

        info['matches'] = matches
        info['matchpoints_a'] = matchpoints_a
        info['matchpoints_b'] = matchpoints_b
        info['estimated_transform'] = estimated_transform

        if success:
            warped_image_a = skimage.transform.warp(
                image_a,
                estimated_transform,
                output_shape=image_b.shape )
        else:
            _log().warning( "  failed to estimate transform" )
            use_raw_scaling = True
    else:
        use_raw_scaling = True

    if use_raw_scaling:
        # _log().warning( "  raw scaling being used" )
        warped_image_a = skimage.transform.resize(
            image_a,
            image_b.shape,
            anti_aliasing=True )

    warped_image_a = skimage.img_as_float( warped_image_a )
    image_b = skimage.img_as_float( image_b )
    info['warped_image_a'] = warped_image_a
    info['image_b'] = image_b
    sim = skimage.metrics.structural_similarity( warped_image_a, image_b )
    info['sim'] = sim

    return ( sim, info )

## ======================================================================

def compute_similarity_from_paths(
        path_a: pathlib.Path,
        path_b: pathlib.Path ) -> Tuple[ float, Dict ]:
    """
    Returns the similarity between the images refered by the
    given file paths.

    Alsoreturns a dictionary with info/debug information
    """

    image_a = skimage.io.imread( path_a.as_posix(),
                                 as_gray=True )
    image_b = skimage.io.imread( path_b.as_posix(),
                                 as_gray=True )
    return compute_similarity( image_a, image_b )

## ======================================================================

def align_image_directories(
        source_dir_a: pathlib.Path,
        source_dir_b: pathlib.Path,
        sorting_order: Optional[Callable[[pathlib.Path],Any]] = None,
        path_filter: Optional[Callable[[pathlib.Path],bool]] = None,
        max_sequence_distance: Optional[int] = 10,
        min_similarity_threshold: float = 0.6 ) -> Sequence[
            Tuple[ pathlib.Path, pathlib.Path, float ] ]:
    """
    Given two directories with image files inside, returns an alignment
    of the image files as pairs of paths and similarity score.

    The files can be filtered and ordered.

    path_filter: Callable[ [pathlib.Path], bool ]
        If true, the given path will be used in aligment checks, otherwise
        the path is ignored and treated as if it does not exist
    sorting_order: Callable[ [pathlib.Path], Any ]
        If given, returns the sorting key used for the paths. This is
        used to define the "sequence" of images in the directory
    max_sequence_distance: int
       If given, only pairs of images that are no more than this many
       steps in the "sequence" will be checked for alignment.

    Returns pairs of aligned images by path alogn with the similarity
    score for the pair. There may not be a pair for
    every image in the source directories.
    """

    # sanity checks and munging
    source_dir_a = pathlib.Path( source_dir_a )
    source_dir_b = pathlib.Path( source_dir_b )
    if not source_dir_a.is_dir():
        raise RuntimeError( "Source A must be a DIRECTORY" )
    if not source_dir_b.is_dir():
        raise RuntimeError( "Source B must be a DIRECTORY" )
    if max_sequence_distance is None:
        max_sequence_distance = float('inf')
    if path_filter is None:
        path_filter = lambda p: True
    if sorting_order is None:
        sorting_order = lambda p: p
    _log().info( "Aligning image paths: " )
    _log().info( "  Sources A='%s' B='%s'", source_dir_a, source_dir_b )
    _log().info( "  Max Seq Distance=%s", max_sequence_distance )
    _log().info( "  Similarity Thresh=%s", min_similarity_threshold )

    # create log/info/debug directory for stuff
    now = datetime.datetime.now()
    base_info_path = pathlib.Path( "log_" + now.isoformat() + "/" )
    base_info_path.mkdir( parents=True, exist_ok=True )

    # build up sequences for a and b sources
    seq_a = sorted( [ p for p in source_dir_a.iterdir()
                      if path_filter( p ) ],
                    key=sorting_order )
    seq_b = sorted( [ p for p in source_dir_b.iterdir()
                      if path_filter( p ) ],
                    key=sorting_order )
    _log().info( "Sequence A: #path=%s '%s' ... '%s'",
                 len( seq_a ),
                 seq_a[0] if len( seq_a ) > 0 else None,
                 seq_a[-1] if len( seq_a ) > 0 else None )
    _log().info( "Sequence B: #path=%s '%s' ... '%s'",
                 len( seq_b ),
                 seq_b[0] if len( seq_b ) > 0 else None,
                 seq_b[-1] if len( seq_b ) > 0 else None )
    with open( base_info_path / "seq_a.dill", 'wb' ) as f:
        dill.dump( seq_a, f )
    with open( base_info_path / "seq_b.dill", 'wb' ) as f:
        dill.dump( seq_b, f )


    # just use a basic threshold over similarity around neighborhood
    aligned_files = []
    for index_a, path_a in enumerate( seq_a ):
        candidates = []
        for index_b, path_b in enumerate( seq_b ):
            if abs( index_a - index_b ) <= max_sequence_distance:
                candidates.append( path_b )
        _log().info( "[%s/%s] Path '%s' #candidates=%s",
                     index_a + 1,
                     len( seq_a ),
                     path_a,
                     len( candidates ) )
        with open( base_info_path / "{0:04d}-candidates.dill".format(index_a),
                   'wb' ) as f:
            dill.dump( candidates, f )
        similarities = sorted(
            map(
                lambda x: ( x[0][0], x[1], x[0][1] ),
                filter(
                    lambda x: x[0][0] >= min_similarity_threshold,
                    [ ( compute_similarity_from_paths( path_a, p ), p )
                      for p in candidates ] ) ),
            reverse=True )
        _log().info( "  Similarities: #=%s  %s ... %s",
                     len(similarities),
                     similarities[0][:2] if len(similarities) > 0 else None,
                     similarities[-1][:2] if len(similarities) > 0 else None )
        with open( base_info_path / "{0:04d}-similarities.dill".format(index_a),
                   'wb' ) as f:
            dill.dump( similarities, f )

        if len( similarities ) > 0:
            aligned_files.append(
                ( path_a, similarities[0][1], similarities[0][0] ) )

    with open( base_info_path / "alignedment.dill", 'wb' ) as f:
        dill.dump( aligned_files, f )

    return aligned_files

## ======================================================================
## ======================================================================
## ======================================================================
## ======================================================================
## ======================================================================
## ======================================================================
## ======================================================================
## ======================================================================
