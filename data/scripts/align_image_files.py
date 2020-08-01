"""
Basic scripts to take raw and trsnlated image directories and find alignment
by filename based on image similarities
"""

import logging
import pathlib
from typing import Optional, Callable, Sequence, Tuple, Any, Dict
import datetime
import re

import skimage
import skimage.color
import skimage.io
import skimage.transform
import skimage.metrics
import skimage.feature
import skimage.measure
import skimage.draw

import dill
import matplotlib.pyplot as plt
import numpy as np

## ======================================================================

def _log():
    return logging.getLogger( __name__ )

## ======================================================================

def compute_similarity(
        image_a,
        image_b,
        base_dir: pathlib.Path,
        preffix: str ) -> Tuple[ float, Dict ]:
    """
    Given two images (float grayscale),
    returns the similarity score,
    from 0 (not similar)
    to 1 (same image).

    Also returns a dictionary with debug information
    """
    base_dir.mkdir( parents=True, exist_ok=True )

    info = {}
    #info['image_a'] = image_a
    #info['image_b'] = image_b

    # corner_keypoints_a = skimage.feature.corner_peaks(
    #     skimage.feature.corner_harris( image_a ),
    #     min_distance = 5,
    #     threshold_rel = 0.1 )
    # corner_keypoints_b = skimage.feature.corner_peaks(
    #     skimage.feature.corner_harris( image_b ),
    #     min_distance = 5,
    #     threshold_rel = 0.1 )
    corner_keypoints_a = skimage.feature.blob_doh(
        image_a,
        min_sigma=20,
        max_sigma=100 )
    corner_keypoints_b = skimage.feature.blob_doh(
        image_b,
        min_sigma=20,
        max_sigma=100 )
    corner_keypoints_a = np.delete( corner_keypoints_a,
                                    2,
                                    1 )
    corner_keypoints_b = np.delete( corner_keypoints_b,
                                    2,
                                    1 )


    info['corner_keypoints_a'] = corner_keypoints_a
    info['corner_keypoints_b'] = corner_keypoints_b
    info['corner_keypoints_a_image'] = generate_corner_image(
        "corner_keypoints_a.png",
        image_a,
        corner_keypoints_a,
        base_dir,
        preffix )
    info['corner_keypoints_b_image'] = generate_corner_image(
        "corner_keypoints_b.png",
        image_b,
        corner_keypoints_b,
        base_dir,
        preffix)


    extractor = skimage.feature.BRIEF(
        descriptor_size=512,
        patch_size=101 )
    extractor.extract( image_a, corner_keypoints_a )
    keypoints_a = corner_keypoints_a[ extractor.mask ]
    descriptors_a = extractor.descriptors
    extractor.extract( image_b, corner_keypoints_b )
    keypoints_b = corner_keypoints_b[ extractor.mask ]
    descriptors_b = extractor.descriptors
    info['keypoints_a'] = keypoints_a
    info['keypoints_b'] = keypoints_b
    #info['descriptors_a'] = descriptors_a
    #info['descriptors_b'] = descriptors_b
    info['keypoints_a_image'] = generate_keypoints_image(
        "keypoints_a.png",
        image_a,
        keypoints_a,
        base_dir,
        preffix )
    info['keypoints_b_image'] = generate_keypoints_image(
        'keypoints_b.png',
        image_b,
        keypoints_b,
        base_dir,
        preffix )


    warped_image_a = None
    use_raw_scaling = False
    if descriptors_a.shape[0] > 5 and descriptors_b.shape[0] > 5:

        matches = skimage.feature.match_descriptors(
            descriptors_a,
            descriptors_b,
            cross_check=True )
        matchpoints_a = keypoints_a[ matches[:,0] ]
        matchpoints_b = keypoints_b[ matches[:,1] ]


        # estimated_transform = skimage.transform.SimilarityTransform()
        # success = estimated_transform.estimate( matchpoints_a,
        #                                         matchpoints_b )
        def is_valid_transform( trans, *data ):
            if trans is None:
                return False
            data_is_valid = True
            if data is not None and len(data) > 0:
                data_is_valid = np.all( np.isfinite( data ) )
            transform_is_finite = np.all( np.isfinite( trans.params))
            transform_is_finite = ( transform_is_finite
                                    and np.isfinite( trans.scale ) )
            return transform_is_finite and data_is_valid
        min_samples = min( matchpoints_a.shape[0] - 1, 20 )
        estimated_transform, inliers = skimage.measure.ransac(
        (matchpoints_a,matchpoints_b),
            skimage.transform.SimilarityTransform,
            min_samples = min_samples,
            residual_threshold = 10.0,
            is_model_valid = is_valid_transform )
        outliers = ( inliers == False )
        inlier_idxs = np.nonzero(inliers)[0]
        outlier_idxs = np.nonzero(outliers)[0]

        # we need to check this since ransac does not seem to :(
        success = ( is_valid_transform(estimated_transform)
                    and len(inlier_idxs) >= 5 )

        info['matches'] = matches
        #info['matchpoints_a'] = matchpoints_a
        #info['matchpoints_b'] = matchpoints_b
        info['estimated_transform'] = estimated_transform
        #info['inliers'] = inliers

        # plot matches, needs matplotlib axis :)
        fig, axes = plt.subplots(nrows=2,ncols=1)
        skimage.feature.plot_matches(
            axes[0],
            image_a,
            image_b,
            matchpoints_a,
            matchpoints_b,
            np.column_stack( ( inlier_idxs, inlier_idxs ) ),
            matches_color='b' )
        axes[0].axis( 'off' )
        axes[0].set_title( "Inlier Correspondances" )
        skimage.feature.plot_matches(
            axes[1],
            image_a,
            image_b,
            matchpoints_a,
            matchpoints_b,
            np.column_stack( ( outlier_idxs, outlier_idxs ) ),
            matches_color='r' )
        axes[0].axis( 'off' )
        axes[0].set_title( "Outlier Correspondances" )
        info['correspondance_image'] = generate_pyplot_image(
            "correspondance_image.png",
            fig,
            base_dir,
            preffix )
        plt.close(fig)

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
    #info['warped_image_a'] = warped_image_a
    #info['image_b_as_float'] = image_b
    info['warped_image_a_image'] = generate_image(
        "warped_image_a.png",
        warped_image_a,
        base_dir,
        preffix )
    info['image_b_as_float_image'] = generate_image(
        "image_b_as_float.png",
        image_b,
        base_dir,
        preffix )
    sim = skimage.metrics.structural_similarity( warped_image_a, image_b )
    info['sim'] = sim

    return ( sim, info )

## ======================================================================

def compute_similarity_from_paths(
        path_a: pathlib.Path,
        path_b: pathlib.Path,
        base_dir: pathlib.Path,
        preffix: str ) -> Tuple[ float, Dict ]:
    """
    Returns the similarity between the images refered by the
    given file paths.

    Alsoreturns a dictionary with info/debug information
    """

    image_a = skimage.img_as_float( skimage.io.imread( path_a.as_posix(),
                                                       as_gray=True ) )
    image_b = skimage.img_as_float( skimage.io.imread( path_b.as_posix(),
                                                       as_gray=True ) )
    return compute_similarity( image_a, image_b, base_dir, preffix )

## ======================================================================

def generate_file_path(
        name: str,
        base_dir: pathlib.Path,
        preffix: str ) -> pathlib.Path:
    """
    Returns the pathlib.Path object for the given name, base dir, and preffix
    """
    file_name = "{}{}".format( preffix, name )
    p = pathlib.Path( base_dir ) / file_name
    if len( p.suffix ) == 0:
        p = pathlib.Path( base_dir ) / (file_name + ".png" )
    return p

## ======================================================================

def generate_image( name: str,
                    image_arr,
                    base_dir: pathlib.Path,
                    preffix: str ) -> pathlib.Path:
    """
    Generates an image (as a file) and returns the resulting
    filename
    """

    filename = generate_file_path( name, base_dir, preffix )
    image_arr = skimage.img_as_ubyte( skimage.color.gray2rgb( image_arr ),
                                      force_copy=True)
    skimage.io.imsave( filename.as_posix(), image_arr, check_contrast=False )
    return filename

## ======================================================================

def generate_pyplot_image( name: str,
                           fig,
                           base_dir: pathlib.Path,
                           preffix: str ) -> pathlib.Path:
    """
    Generates an image form the matplotlib Figure and returns path to it
    """
    filename = generate_file_path( name, base_dir, preffix )
    fig.savefig( filename.as_posix() )
    return filename

## ======================================================================

def generate_corner_image(
        name: str,
        image_arr,
        corner_keypoints,
        base_dir: pathlib.Path,
        preffix: str ) -> pathlib.Path:
    """
    Generates an image representign the given corners and returns
    the path to the generated image
    """
    filename = generate_file_path( name, base_dir, preffix )
    image_arr = skimage.img_as_ubyte( skimage.color.gray2rgb( image_arr ),
                                      force_copy=True )
    for loc in corner_keypoints:
        rr, cc = skimage.draw.circle_perimeter( int(loc[0]), int(loc[1]), 10,
                                                shape=image_arr.shape)
        color = ( 0, 255, 0 )
        skimage.draw.set_color( image_arr,
                                ( rr, cc ),
                                color )
    skimage.io.imsave( filename.as_posix(), image_arr, check_contrast=False )
    return filename

## ======================================================================

def generate_keypoints_image(
        name: str,
        image_arr,
        keypoints,
        base_dir: pathlib.Path,
        preffix: str ) -> pathlib.Path:
    """
    Generates an image showing the locations of the given keypoints.
    Returns the path to the image.
    """
    filename = generate_file_path( name, base_dir, preffix )
    image_arr = skimage.img_as_ubyte( skimage.color.gray2rgb( image_arr ),
                                      force_copy=True )
    for loc in keypoints:
        rr, cc = skimage.draw.circle_perimeter( int(loc[0]), int(loc[1]), 10,
                                                shape=image_arr.shape)
        color = ( 0, 255, 0 )
        skimage.draw.set_color( image_arr,
                                ( rr, cc ),
                                color )
    skimage.io.imsave( filename.as_posix(), image_arr, check_contrast=False )
    return filename

## ======================================================================

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
    now_string = re.sub( r'\W+', '_', now.isoformat() )
    base_info_path = pathlib.Path( "log_" + now_string + "/" )
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
        all_similarities = [
            ( compute_similarity_from_paths(
                path_a,
                p,
                base_info_path / "index-{0:04d}".format(index_a),
                "sim-{0:04d}-".format(i) ), p )
            for (i,p) in enumerate( candidates ) ]
        similarities = sorted(
            map(
                lambda x: ( x[0][0], x[1], x[0][1] ),
                filter(
                    lambda x: x[0][0] >= min_similarity_threshold,
                    all_similarities ) ),
            reverse=True )
        _log().info( "  Similarities: #=%s  %s ... %s",
                     len(similarities),
                     similarities[0][:2] if len(similarities) > 0 else None,
                     similarities[-1][:2] if len(similarities) > 0 else None )
        with open( base_info_path / "{0:04d}-all-similarities.dill".format(index_a),
                   'wb' ) as f:
            dill.dump( all_similarities, f )
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
