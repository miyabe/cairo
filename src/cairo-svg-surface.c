/* vim: set sw=4 sts=4: -*- Mode: c; tab-width: 8; c-basic-offset: 4; indent-tabs-mode: t; -*- */
/* cairo - a vector graphics library with display and print output
 *
 * Copyright © 2004 Red Hat, Inc
 * Copyright © 2005-2007 Emmanuel Pacaud <emmanuel.pacaud@free.fr>
 * Copyright © 2006 Red Hat, Inc
 *
 * This library is free software; you can redistribute it and/or
 * modify it either under the terms of the GNU Lesser General Public
 * License version 2.1 as published by the Free Software Foundation
 * (the "LGPL") or, at your option, under the terms of the Mozilla
 * Public License Version 1.1 (the "MPL"). If you do not alter this
 * notice, a recipient may use your version of this file under either
 * the MPL or the LGPL.
 *
 * You should have received a copy of the LGPL along with this library
 * in the file COPYING-LGPL-2.1; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Suite 500, Boston, MA 02110-1335, USA
 * You should have received a copy of the MPL along with this library
 * in the file COPYING-MPL-1.1
 *
 * The contents of this file are subject to the Mozilla Public License
 * Version 1.1 (the "License"); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * This software is distributed on an "AS IS" basis, WITHOUT WARRANTY
 * OF ANY KIND, either express or implied. See the LGPL or the MPL for
 * the specific language governing rights and limitations.
 *
 * The Original Code is the cairo graphics library.
 *
 * The Initial Developer of the Original Code is University of Southern
 * California.
 *
 * Contributor(s):
 *	Kristian Høgsberg <krh@redhat.com>
 *	Emmanuel Pacaud <emmanuel.pacaud@free.fr>
 *	Carl Worth <cworth@cworth.org>
 */

#define _BSD_SOURCE /* for snprintf() */
#include "cairoint.h"
#include "cairo-svg.h"
#include "cairo-analysis-surface-private.h"
#include "cairo-error-private.h"
#include "cairo-image-info-private.h"
#include "cairo-recording-surface-private.h"
#include "cairo-output-stream-private.h"
#include "cairo-path-fixed-private.h"
#include "cairo-paginated-private.h"
#include "cairo-scaled-font-subsets-private.h"
#include "cairo-output-stream-private.h"
#include "cairo-surface-clipper-private.h"
#include "cairo-svg-surface-private.h"

/**
 * SECTION:cairo-svg
 * @Title: SVG Surfaces
 * @Short_Description: Rendering SVG documents
 * @See_Also: #cairo_surface_t
 *
 * The SVG surface is used to render cairo graphics to
 * SVG files and is a multi-page vector surface backend.
 */

/**
 * CAIRO_HAS_SVG_SURFACE:
 *
 * Defined if the SVG surface backend is available.
 * This macro can be used to conditionally compile backend-specific code.
 */

typedef struct cairo_svg_page cairo_svg_page_t;

static const int invalid_pattern_id = -1;

static const cairo_svg_version_t _cairo_svg_versions[] =
{
    CAIRO_SVG_VERSION_1_1,
    CAIRO_SVG_VERSION_1_2
};

#define CAIRO_SVG_VERSION_LAST ARRAY_LENGTH (_cairo_svg_versions)

static void
_cairo_svg_surface_emit_path (cairo_output_stream_t	*output,
			      cairo_path_fixed_t	*path,
			      const cairo_matrix_t	*ctm_inverse,
			      double precision);

static cairo_bool_t
_cairo_svg_version_has_page_set_support (cairo_svg_version_t version)
{
    return version > CAIRO_SVG_VERSION_1_1;
}

static const char * _cairo_svg_version_strings[CAIRO_SVG_VERSION_LAST] =
{
    "SVG 1.1",
    "SVG 1.2"
};

static const char * _cairo_svg_internal_version_strings[CAIRO_SVG_VERSION_LAST] =
{
    "1.1",
    "1.2"
};

#define GLYPH_PRECISION 1
#define PATH_PRECISION 100
#define MATRIX_PRECISION 1000000
#define COLOR_PRECISION 10

static double _round(double a, int precision) {
	if (a < 0) {
		return (double)(long)(a * precision - 0.5) / precision;
	}
	return (double)(long)(a * precision + 0.5) / precision;
}

#define STYLE_A 1
#define STYLE_B 2
#define STYLE_C 4
#define STYLE_D 8
#define STYLE_E 16

static const char utf8_skip_data[256] = {
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
    3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,5,5,5,5,6,6,1,1
};

static void _escape_html(const char *utf8, int *off, cairo_output_stream_t *out) {
	switch (utf8[*off]) {
	case '<':
		_cairo_output_stream_printf (out, "&lt;");
		break;
	case '>':
		_cairo_output_stream_printf (out, "&gt;");
		break;
	case '&':
		_cairo_output_stream_printf (out, "&amp;");
		break;
	case '"':
		_cairo_output_stream_printf (out, "&quot;");
		break;
	case '\'':
		_cairo_output_stream_printf (out, "&apos;");
		break;
	default: {
   	if ((unsigned char)utf8[*off] < 32 && utf8[*off] != 0x20 && utf8[*off] != 0x0D && utf8[*off] != 0x0A && utf8[*off] != 0x09) {
   		_cairo_output_stream_printf (out, "&#x%X;", 0xE000 + (unsigned char)utf8[*off]);
			break;
   		}
		int skip;
		for (skip = utf8_skip_data[(unsigned char)utf8[*off]]; skip > 0; --skip) {
			_cairo_output_stream_printf (out, "%c", utf8[*off]);
			++*off;
		}
		return;
 	}
	}
	++*off;
}

struct cairo_svg_page {
    unsigned int surface_id;
    unsigned int clip_level;
    cairo_output_stream_t *xml_node;
};

typedef struct _cairo_svg_style {
    cairo_hash_entry_t base;
    unsigned char* style;
    long length;
    int id;
} cairo_svg_style;

struct cairo_svg_document {
    cairo_output_stream_t *output_stream;
    unsigned long refcount;
    cairo_surface_t *owner;
    cairo_bool_t finished;

    double width;
    double height;

    cairo_output_stream_t *xml_node_defs;
    cairo_output_stream_t *tail;

    unsigned int linear_pattern_id;
    unsigned int radial_pattern_id;
    unsigned int pattern_id;
    unsigned int filter_id;
    unsigned int clip_id;
    unsigned int mask_id;

    cairo_bool_t alpha_filter;

    cairo_svg_version_t svg_version;

    cairo_svg_fontfile_t *fontfile;
    cairo_scaled_font_subsets_t *font_text_subsets;
    cairo_scaled_font_subsets_t *font_glyphs_subsets;

    unsigned int style_id;
    cairo_output_stream_t *css;
    unsigned int styles;
    cairo_hash_table_t *style_to_id;

    cairo_write_image_func_t	write_image_func;
    void	*write_image_closure;

    cairo_output_stream_t *text_x, *text_y, *text;
    cairo_svg_style *prev_style;
    char prev_char;
    cairo_bool_t preserve_space;
};

struct cairo_svg_font_output {
	cairo_scaled_font_subsets_t *font_subsets;
    cairo_output_stream_t *xml_node_glyphs;
    cairo_svg_fontfile_t *fontfile;
    const char *path;
    cairo_bool_t glyphs;
    cairo_bool_t scaled;
    int prev_id;
};

static cairo_status_t
_cairo_svg_document_create (cairo_output_stream_t	 *stream,
			    double			  width,
			    double			  height,
			    cairo_svg_version_t		  version,
			    cairo_svg_document_t	**document_out,
			    cairo_svg_fontfile *fontfile);

static cairo_status_t
_cairo_svg_document_destroy (cairo_svg_document_t *document);

static cairo_status_t
_cairo_svg_document_finish (cairo_svg_document_t *document);

static cairo_svg_document_t *
_cairo_svg_document_reference (cairo_svg_document_t *document);

static unsigned int
_cairo_svg_document_allocate_mask_id (cairo_svg_document_t *document);

static cairo_surface_t *
_cairo_svg_surface_create_for_document (cairo_svg_document_t	*document,
					cairo_content_t		 content,
					double			 width,
					double			 height);
static cairo_surface_t *
_cairo_svg_surface_create_for_stream_internal (cairo_output_stream_t	*stream,
					       double			 width,
					       double			 height,
					       cairo_svg_version_t	 version,
					       cairo_svg_fontfile_t *fontfile);

static const cairo_surface_backend_t cairo_svg_surface_backend;
static const cairo_paginated_surface_backend_t cairo_svg_surface_paginated_backend;

/**
 * cairo_svg_surface_create_for_stream:
 * @write_func: a #cairo_write_func_t to accept the output data, may be %NULL
 *              to indicate a no-op @write_func. With a no-op @write_func,
 *              the surface may be queried or used as a source without
 *              generating any temporary files.
 * @closure: the closure argument for @write_func
 * @width_in_points: width of the surface, in points (1 point == 1/72.0 inch)
 * @height_in_points: height of the surface, in points (1 point == 1/72.0 inch)
 *
 * Creates a SVG surface of the specified size in points to be written
 * incrementally to the stream represented by @write_func and @closure.
 *
 * Return value: a pointer to the newly created surface. The caller
 * owns the surface and should call cairo_surface_destroy() when done
 * with it.
 *
 * This function always returns a valid pointer, but it will return a
 * pointer to a "nil" surface if an error such as out of memory
 * occurs. You can use cairo_surface_status() to check for this.
 *
 * Since: 1.2
 */
cairo_surface_t *
cairo_svg_surface_create_for_stream (cairo_write_func_t		 write_func,
				     void			*closure,
				     double			 width,
				     double			 height)
{
    cairo_output_stream_t *stream;

    stream = _cairo_output_stream_create (write_func, NULL, closure);
    if (_cairo_output_stream_get_status (stream))
	return _cairo_surface_create_in_error (_cairo_output_stream_destroy (stream));

    return _cairo_svg_surface_create_for_stream_internal (stream, width, height, CAIRO_SVG_VERSION_1_1, NULL);
}

/**
 * cairo_svg_surface_create:
 * @filename: a filename for the SVG output (must be writable), %NULL may be
 *            used to specify no output. This will generate a SVG surface that
 *            may be queried and used as a source, without generating a
 *            temporary file.
 * @width_in_points: width of the surface, in points (1 point == 1/72.0 inch)
 * @height_in_points: height of the surface, in points (1 point == 1/72.0 inch)
 *
 * Creates a SVG surface of the specified size in points to be written
 * to @filename.
 *
 * The SVG surface backend recognizes the following MIME types for the
 * data attached to a surface (see cairo_surface_set_mime_data()) when
 * it is used as a source pattern for drawing on this surface:
 * %CAIRO_MIME_TYPE_JPEG, %CAIRO_MIME_TYPE_PNG,
 * %CAIRO_MIME_TYPE_URI. If any of them is specified, the SVG backend
 * emits a href with the content of MIME data instead of a surface
 * snapshot (PNG, Base64-encoded) in the corresponding image tag.
 *
 * The unofficial MIME type %CAIRO_MIME_TYPE_URI is examined
 * first. If present, the URI is emitted as is: assuring the
 * correctness of URI is left to the client code.
 *
 * If %CAIRO_MIME_TYPE_URI is not present, but %CAIRO_MIME_TYPE_JPEG
 * or %CAIRO_MIME_TYPE_PNG is specified, the corresponding data is
 * Base64-encoded and emitted.
 *
 * Return value: a pointer to the newly created surface. The caller
 * owns the surface and should call cairo_surface_destroy() when done
 * with it.
 *
 * This function always returns a valid pointer, but it will return a
 * pointer to a "nil" surface if an error such as out of memory
 * occurs. You can use cairo_surface_status() to check for this.
 *
 * Since: 1.2
 **/
cairo_surface_t *
cairo_svg_surface_create (const char	*filename,
			  double	 width,
			  double	 height)
{
    cairo_output_stream_t *stream;

    stream = _cairo_output_stream_create_for_filename (filename);
    if (_cairo_output_stream_get_status (stream))
	return _cairo_surface_create_in_error (_cairo_output_stream_destroy (stream));

    return _cairo_svg_surface_create_for_stream_internal (stream, width, height, CAIRO_SVG_VERSION_1_1, NULL);
}

cairo_svg_fontfile_t *
cairo_svg_fontfile_create (const char   *path) {
	cairo_svg_fontfile_t* fontfile;

	fontfile = malloc (sizeof (cairo_svg_fontfile_t));

	fontfile->font_subsets = _cairo_scaled_font_subsets_create_simple ();
    if (unlikely (fontfile->font_subsets == NULL)) {
	return NULL;
    }

	fontfile->path = malloc(strlen(path));
	strcpy(fontfile->path, path);

	return fontfile;
}

static cairo_status_t
_cairo_svg_document_emit_font_subsets_file (cairo_svg_font_output_t *fontout);

void
cairo_svg_fontfile_finish (cairo_svg_fontfile_t   *fontfile, const char* path) {
	cairo_svg_font_output_t fontout;
	cairo_status_t status;

    fontout.font_subsets = fontfile->font_subsets;
    fontout.fontfile = fontfile;
    fontout.path = path;
    fontout.glyphs = FALSE;
    fontout.scaled = FALSE;
    fontout.prev_id = -1;

   _cairo_svg_document_emit_font_subsets_file(&fontout);

	free(fontfile->path);
	free(fontfile);
 	_cairo_scaled_font_subsets_destroy(fontfile->font_subsets);
}

cairo_surface_t *
cairo_svg_surface_create_with_fontfile (const char	*filename,
			  double	 width,
			  double	 height,
			  cairo_svg_fontfile_t *fontfile)
{
    cairo_output_stream_t *stream;

    stream = _cairo_output_stream_create_for_filename (filename);
    if (_cairo_output_stream_get_status (stream))
	return _cairo_surface_create_in_error (_cairo_output_stream_destroy (stream));

    return _cairo_svg_surface_create_for_stream_internal (stream, width, height, CAIRO_SVG_VERSION_1_1, fontfile);
}

static cairo_bool_t
_cairo_surface_is_svg (cairo_surface_t *surface)
{
    return surface->backend == &cairo_svg_surface_backend;
}

/* If the abstract_surface is a paginated surface, and that paginated
 * surface's target is a svg_surface, then set svg_surface to that
 * target. Otherwise return FALSE.
 */
static cairo_bool_t
_extract_svg_surface (cairo_surface_t		 *surface,
		      cairo_svg_surface_t	**svg_surface)
{
    cairo_surface_t *target;
    cairo_status_t status_ignored;

    if (surface->status)
	return FALSE;
    if (surface->finished) {
	status_ignored = _cairo_surface_set_error (surface,
						   _cairo_error (CAIRO_STATUS_SURFACE_FINISHED));
        return FALSE;
    }

    if (! _cairo_surface_is_paginated (surface)) {
	status_ignored = _cairo_surface_set_error (surface,
						   _cairo_error (CAIRO_STATUS_SURFACE_TYPE_MISMATCH));
	return FALSE;
    }

    target = _cairo_paginated_surface_get_target (surface);
    if (target->status) {
	status_ignored = _cairo_surface_set_error (surface,
						   target->status);
	return FALSE;
    }
    if (target->finished) {
	status_ignored = _cairo_surface_set_error (surface,
						   _cairo_error (CAIRO_STATUS_SURFACE_FINISHED));
        return FALSE;
    }

    if (! _cairo_surface_is_svg (target)) {
	status_ignored = _cairo_surface_set_error (surface,
						   _cairo_error (CAIRO_STATUS_SURFACE_TYPE_MISMATCH));
	return FALSE;
    }

    *svg_surface = (cairo_svg_surface_t *) target;
    return TRUE;
}

/**
 * cairo_svg_surface_restrict_to_version:
 * @surface: a SVG #cairo_surface_t
 * @version: SVG version
 *
 * Restricts the generated SVG file to @version. See cairo_svg_get_versions()
 * for a list of available version values that can be used here.
 *
 * This function should only be called before any drawing operations
 * have been performed on the given surface. The simplest way to do
 * this is to call this function immediately after creating the
 * surface.
 *
 * Since: 1.2
 **/
void
cairo_svg_surface_restrict_to_version (cairo_surface_t		*abstract_surface,
				       cairo_svg_version_t	 version)
{
    cairo_svg_surface_t *surface = NULL; /* hide compiler warning */

    if (! _extract_svg_surface (abstract_surface, &surface))
	return;

    if (version < CAIRO_SVG_VERSION_LAST)
	surface->document->svg_version = version;
}

/**
 * cairo_svg_get_versions:
 * @versions: supported version list
 * @num_versions: list length
 *
 * Used to retrieve the list of supported versions. See
 * cairo_svg_surface_restrict_to_version().
 *
 * Since: 1.2
 **/
void
cairo_svg_get_versions (cairo_svg_version_t const	**versions,
                        int				 *num_versions)
{
    if (versions != NULL)
	*versions = _cairo_svg_versions;

    if (num_versions != NULL)
	*num_versions = CAIRO_SVG_VERSION_LAST;
}

/**
 * cairo_svg_version_to_string:
 * @version: a version id
 *
 * Get the string representation of the given @version id. This function
 * will return %NULL if @version isn't valid. See cairo_svg_get_versions()
 * for a way to get the list of valid version ids.
 *
 * Return value: the string associated to given version.
 *
 * Since: 1.2
 **/
const char *
cairo_svg_version_to_string (cairo_svg_version_t version)
{
    if (version >= CAIRO_SVG_VERSION_LAST)
	return NULL;

    return _cairo_svg_version_strings[version];
}

void
cairo_svg_surface_set_write_image_func (cairo_surface_t 		*abstract_surface,
						cairo_write_image_func_t 		write_image_func,
						void  	 *write_image_closure)
{
	cairo_svg_surface_t *surface = NULL; /* hide compiler warning */

    if (! _extract_svg_surface (abstract_surface, &surface))
	return;

	surface->document->write_image_func = write_image_func;
	surface->document->write_image_closure = write_image_closure;
}

void
cairo_svg_surface_link(cairo_surface_t *abstract_surface,
		const char* uri,
		double x, double y, double width, double height) {
	cairo_svg_surface_t *surface = (cairo_svg_surface_t*)_cairo_paginated_surface_get_target(abstract_surface);
	cairo_output_stream_t *output = surface->document->tail;
	_cairo_output_stream_printf (output, "<a xlink:href=\"");
	int len = strlen(uri);
	int i = 0;
	while(i < len) {
		_escape_html(uri, &i, output);
	}
   _cairo_output_stream_printf (output,
    					 "\"><rect x=\"%f\" y=\"%f\" width=\"%f\" height=\"%f\" opacity=\"0\"/></a>\n",
    					 x, y, width, height);
}

static cairo_bool_t
_cliprect_covers_surface (cairo_svg_surface_t *surface,
			  cairo_path_fixed_t *path)
{
    cairo_box_t box;

    if (_cairo_path_fixed_is_box (path, &box)) {
	if (box.p1.x <= 0 &&
	    box.p1.y <= 0 &&
	    _cairo_fixed_to_double (box.p2.x) >= surface->width &&
	    _cairo_fixed_to_double (box.p2.y) >= surface->height)
	{
	    return TRUE;
	}
    }

    return FALSE;
}

static void
_cairo_svg_end_text (cairo_svg_surface_t *surface)
{
	cairo_output_stream_t *output = surface->xml_node;
    if (surface->document->prev_style != NULL) {
    	if (surface->document->preserve_space) {
    		_cairo_output_stream_printf (output,
    					 " xml:space=\"preserve\"");
    	}
    	surface->document->preserve_space = FALSE;
    	surface->document->prev_char = 0x20;
		_cairo_output_stream_printf (output,
					 " x=\"");
    	_cairo_memory_stream_copy (surface->document->text_x, output);
		_cairo_output_stream_printf (output,
					 "\" y=\"");
    	_cairo_memory_stream_copy (surface->document->text_y, output);
		_cairo_output_stream_printf (output,
					 "\">");
		_cairo_memory_stream_copy (surface->document->text, output);
		_cairo_output_stream_printf (output,
					 "</text>\n");

		surface->document->prev_style = NULL;
		_cairo_output_stream_destroy(surface->document->text_x);
		_cairo_output_stream_destroy(surface->document->text_y);
		_cairo_output_stream_destroy(surface->document->text);
    }
}

static cairo_status_t
_cairo_svg_surface_clipper_intersect_clip_path (cairo_surface_clipper_t *clipper,
						cairo_path_fixed_t	*path,
						cairo_fill_rule_t	 fill_rule,
						double			 tolerance,
						cairo_antialias_t	 antialias)
{
    cairo_svg_surface_t *surface = cairo_container_of (clipper,
						       cairo_svg_surface_t,
						       clipper);
    cairo_svg_document_t *document = surface->document;
    unsigned int i;

    _cairo_svg_end_text(surface);

    if (path == NULL) {
	for (i = 0; i < surface->clip_level; i++)
	    _cairo_output_stream_printf (surface->xml_node, "</g>\n");

	surface->clip_level = 0;
	return CAIRO_STATUS_SUCCESS;
    }

    /* skip trivial whole-page clips */
    if (_cliprect_covers_surface (surface, path))
	return CAIRO_STATUS_SUCCESS;

    _cairo_output_stream_printf (document->xml_node_defs,
				 "<clipPath id=\"clip%d\">\n"
				 "  <path ",
				 document->clip_id);
    _cairo_svg_surface_emit_path (document->xml_node_defs, path, NULL, PATH_PRECISION);

    _cairo_output_stream_printf (document->xml_node_defs,
				 "/>\n"
				 "</clipPath>\n");

    _cairo_output_stream_printf (surface->xml_node,
				 "<g clip-path=\"url(#clip%d)\" "
				 "clip-rule=\"%s\">\n",
				 document->clip_id,
				 fill_rule == CAIRO_FILL_RULE_EVEN_ODD ?
				 "evenodd" : "nonzero");

    document->clip_id++;
    surface->clip_level++;

    return CAIRO_STATUS_SUCCESS;
}

static cairo_surface_t *
_cairo_svg_surface_create_for_document (cairo_svg_document_t	*document,
					cairo_content_t		 content,
					double			 width,
					double			 height)
{
    cairo_svg_surface_t *surface;
    cairo_surface_t *paginated;
    cairo_status_t status, status_ignored;

    surface = malloc (sizeof (cairo_svg_surface_t));
    if (unlikely (surface == NULL))
	return _cairo_surface_create_in_error (_cairo_error (CAIRO_STATUS_NO_MEMORY));

    _cairo_surface_init (&surface->base,
			 &cairo_svg_surface_backend,
			 NULL, /* device */
			 content);

    surface->width = width;
    surface->height = height;

    surface->document = _cairo_svg_document_reference (document);

    surface->clip_level = 0;
    _cairo_surface_clipper_init (&surface->clipper,
				 _cairo_svg_surface_clipper_intersect_clip_path);

    surface->base_clip = document->clip_id++;
    surface->is_base_clip_emitted = FALSE;

    surface->xml_node = _cairo_memory_stream_create ();
    status = _cairo_output_stream_get_status (surface->xml_node);
    if (unlikely (status))
	goto CLEANUP;

    _cairo_array_init (&surface->page_set, sizeof (cairo_svg_page_t));

    if (content == CAIRO_CONTENT_COLOR) {
    if ((document->styles & STYLE_A) == 0) {
    	_cairo_output_stream_printf(document->css, ".sa {opacity:1;stroke:none;fill:rgb(0,0,0);}\n");
    	document->styles |= STYLE_A;
    }
	_cairo_output_stream_printf (surface->xml_node,
				     "<rect width=\"%f\" height=\"%f\" "
				     "class=\"sa\"/>\n",
				     _round(width, PATH_PRECISION), _round(height, PATH_PRECISION));
	status = _cairo_output_stream_get_status (surface->xml_node);
	if (unlikely (status))
	    goto CLEANUP;
    }

    surface->paginated_mode = CAIRO_PAGINATED_MODE_ANALYZE;
    surface->force_fallbacks = FALSE;
    surface->content = content;

    paginated = _cairo_paginated_surface_create (&surface->base,
					         surface->content,
						 &cairo_svg_surface_paginated_backend);
    status = paginated->status;
    if (status == CAIRO_STATUS_SUCCESS) {
	/* paginated keeps the only reference to surface now, drop ours */
	cairo_surface_destroy (&surface->base);
	return paginated;
    }

    /* ignore status as we are on the error path */
CLEANUP:
    status_ignored = _cairo_output_stream_destroy (surface->xml_node);
    status_ignored = _cairo_svg_document_destroy (document);

    free (surface);

    return _cairo_surface_create_in_error (status);
}

static cairo_surface_t *
_cairo_svg_surface_create_for_stream_internal (cairo_output_stream_t	*stream,
					       double			 width,
					       double			 height,
					       cairo_svg_version_t	 version,
					       cairo_svg_fontfile_t *fontfile)
{
    cairo_svg_document_t *document = NULL; /* silence compiler */
    cairo_surface_t *surface;
    cairo_status_t status;

    status = _cairo_svg_document_create (stream,
	                                 width, height, version,
					 &document, fontfile);
    if (unlikely (status)) {
	surface =  _cairo_surface_create_in_error (status);
	/* consume the output stream on behalf of caller */
	status = _cairo_output_stream_destroy (stream);
	return surface;
    }

    surface = _cairo_svg_surface_create_for_document (document, CAIRO_CONTENT_COLOR_ALPHA,
						      width, height);
    if (surface->status) {
	status = _cairo_svg_document_destroy (document);
	return surface;
    }

    document->owner = surface;
    status = _cairo_svg_document_destroy (document);
    /* the ref count should be 2 at this point */
    assert (status == CAIRO_STATUS_SUCCESS);

    return surface;
}

static cairo_svg_page_t *
_cairo_svg_surface_store_page (cairo_svg_surface_t *surface)
{
    unsigned int i;
    cairo_svg_page_t page;
    cairo_output_stream_t *stream;
    cairo_status_t status;

    _cairo_svg_end_text(surface);

    stream = _cairo_memory_stream_create ();
    if (_cairo_output_stream_get_status (stream)) {
	status = _cairo_output_stream_destroy (stream);
	return NULL;
    }

    page.surface_id = surface->base.unique_id;
    page.clip_level = surface->clip_level;
    page.xml_node = surface->xml_node;

    if (_cairo_array_append (&surface->page_set, &page)) {
	status = _cairo_output_stream_destroy (stream);
	return NULL;
    }

    surface->xml_node = stream;
    surface->clip_level = 0;
    for (i = 0; i < page.clip_level; i++)
	_cairo_output_stream_printf (page.xml_node, "</g>\n");

    _cairo_surface_clipper_reset (&surface->clipper);

    return _cairo_array_index (&surface->page_set,
			       surface->page_set.num_elements - 1);
}

static cairo_int_status_t
_cairo_svg_surface_copy_page (void *abstract_surface)
{
    cairo_svg_surface_t *surface = abstract_surface;
    cairo_svg_page_t *page;

    page = _cairo_svg_surface_store_page (surface);
    if (unlikely (page == NULL))
	return _cairo_error (CAIRO_STATUS_NO_MEMORY);

    _cairo_memory_stream_copy (page->xml_node, surface->xml_node);

    return CAIRO_STATUS_SUCCESS;
}

static cairo_int_status_t
_cairo_svg_surface_show_page (void *abstract_surface)
{
    cairo_svg_surface_t *surface = abstract_surface;

    _cairo_svg_end_text(surface);
    if (unlikely (_cairo_svg_surface_store_page (surface) == NULL))
	return _cairo_error (CAIRO_STATUS_NO_MEMORY);

    return CAIRO_STATUS_SUCCESS;
}

static void
_cairo_svg_surface_emit_transform (cairo_output_stream_t *output,
				   char const		 *attribute_str,
				   const cairo_matrix_t	 *object_matrix,
				   const cairo_matrix_t  *parent_matrix)
{
    cairo_matrix_t matrix = *object_matrix;

    if (parent_matrix != NULL)
	cairo_matrix_multiply (&matrix, &matrix, parent_matrix);

    if (!_cairo_matrix_is_identity (&matrix))
	_cairo_output_stream_printf (output,
				     "%s=\"matrix(%f,%f,%f,%f,%f,%f)\"",
				     attribute_str,
				     _round(matrix.xx, MATRIX_PRECISION), _round(matrix.yx, MATRIX_PRECISION),
				     _round(matrix.xy, MATRIX_PRECISION), _round(matrix.yy, MATRIX_PRECISION),
				     _round(matrix.x0, MATRIX_PRECISION), _round(matrix.y0, MATRIX_PRECISION));
}

typedef struct {
    cairo_output_stream_t *output;
    const cairo_matrix_t *ctm_inverse;
    int precision;
} svg_path_info_t;

static cairo_status_t
_cairo_svg_path_move_to (void *closure,
			 const cairo_point_t *point)
{
    svg_path_info_t *info = closure;
    double x = _cairo_fixed_to_double (point->x);
    double y = _cairo_fixed_to_double (point->y);

    if (info->ctm_inverse)
	cairo_matrix_transform_point (info->ctm_inverse, &x, &y);

    _cairo_output_stream_printf (info->output, "M %f %f ",
    		_round(x, info->precision), _round(y, info->precision));

    return CAIRO_STATUS_SUCCESS;
}

static cairo_status_t
_cairo_svg_path_line_to (void *closure,
			 const cairo_point_t *point)
{
    svg_path_info_t *info = closure;
    double x = _cairo_fixed_to_double (point->x);
    double y = _cairo_fixed_to_double (point->y);

    if (info->ctm_inverse)
	cairo_matrix_transform_point (info->ctm_inverse, &x, &y);

    _cairo_output_stream_printf (info->output, "L %f %f ",
    		_round(x, info->precision), _round(y, info->precision));

    return CAIRO_STATUS_SUCCESS;
}

static cairo_status_t
_cairo_svg_path_curve_to (void          *closure,
			  const cairo_point_t *b,
			  const cairo_point_t *c,
			  const cairo_point_t *d)
{
    svg_path_info_t *info = closure;
    double bx = _cairo_fixed_to_double (b->x);
    double by = _cairo_fixed_to_double (b->y);
    double cx = _cairo_fixed_to_double (c->x);
    double cy = _cairo_fixed_to_double (c->y);
    double dx = _cairo_fixed_to_double (d->x);
    double dy = _cairo_fixed_to_double (d->y);

    if (info->ctm_inverse) {
	cairo_matrix_transform_point (info->ctm_inverse, &bx, &by);
	cairo_matrix_transform_point (info->ctm_inverse, &cx, &cy);
	cairo_matrix_transform_point (info->ctm_inverse, &dx, &dy);
    }

    _cairo_output_stream_printf (info->output,
				 "C %f %f %f %f %f %f ",
				 _round(bx, info->precision), _round(by, info->precision), _round(cx, info->precision), _round(cy, info->precision), _round(dx, info->precision), _round(dy, info->precision));

    return CAIRO_STATUS_SUCCESS;
}

static cairo_status_t
_cairo_svg_path_close_path (void *closure)
{
    svg_path_info_t *info = closure;

    _cairo_output_stream_printf (info->output, "Z ");

    return CAIRO_STATUS_SUCCESS;
}

static void
_cairo_svg_surface_emit_path (cairo_output_stream_t	*output,
			      cairo_path_fixed_t	*path,
			      const cairo_matrix_t	*ctm_inverse,
			      double precision)
{
    cairo_status_t status;
    svg_path_info_t info;

    _cairo_output_stream_printf (output, "d=\"");

    info.output = output;
    info.ctm_inverse = ctm_inverse;
    info.precision = precision;
    status = _cairo_path_fixed_interpret (path,
    					CAIRO_DIRECTION_FORWARD,
					  _cairo_svg_path_move_to,
					  _cairo_svg_path_line_to,
					  _cairo_svg_path_curve_to,
					  _cairo_svg_path_close_path,
					  &info);
    assert (status == CAIRO_STATUS_SUCCESS);

    _cairo_output_stream_printf (output, "\"");
}

static cairo_int_status_t
_cairo_svg_document_emit_outline_glyph_data (cairo_svg_font_output_t	*fontout,
					     cairo_scaled_font_t	*scaled_font,
					     unsigned long		 glyph_index)
{
    cairo_scaled_glyph_t *scaled_glyph;
    cairo_int_status_t status;

    status = _cairo_scaled_glyph_lookup (scaled_font,
					 glyph_index,
					 CAIRO_SCALED_GLYPH_INFO_METRICS|
					 CAIRO_SCALED_GLYPH_INFO_PATH,
					 &scaled_glyph);
    if (unlikely (status))
	return status;

    _cairo_output_stream_printf (fontout->xml_node_glyphs,
				 "<path style=\"stroke:none;\" ");

    _cairo_svg_surface_emit_path (fontout->xml_node_glyphs,
				  scaled_glyph->path, NULL, PATH_PRECISION);

    _cairo_output_stream_printf (fontout->xml_node_glyphs,
				 "/>\n");

    return status;
}

static cairo_int_status_t
_cairo_svg_document_emit_bitmap_glyph_data (cairo_svg_font_output_t	*fontout,
					    cairo_scaled_font_t		*scaled_font,
					    unsigned long		 glyph_index)
{
    cairo_scaled_glyph_t *scaled_glyph;
    cairo_image_surface_t *image;
    cairo_status_t status;
    uint8_t *row, *byte;
    int rows, cols;
    int x, y, bit;

    status = _cairo_scaled_glyph_lookup (scaled_font,
					 glyph_index,
					 CAIRO_SCALED_GLYPH_INFO_METRICS |
					 CAIRO_SCALED_GLYPH_INFO_SURFACE,
					 &scaled_glyph);
    if (unlikely (status))
	return status;

    image = _cairo_image_surface_coerce_to_format (scaled_glyph->surface,
					           CAIRO_FORMAT_A1);
    status = image->base.status;
    if (unlikely (status))
	return status;

    _cairo_output_stream_printf (fontout->xml_node_glyphs, "<g");
    _cairo_svg_surface_emit_transform (fontout->xml_node_glyphs, " transform",
				       &image->base.device_transform_inverse, NULL);
    _cairo_output_stream_printf (fontout->xml_node_glyphs, ">/n");

    for (y = 0, row = image->data, rows = image->height; rows; row += image->stride, rows--, y++) {
	for (x = 0, byte = row, cols = (image->width + 7) / 8; cols; byte++, cols--) {
	    uint8_t output_byte = CAIRO_BITSWAP8_IF_LITTLE_ENDIAN (*byte);
	    for (bit = 7; bit >= 0 && x < image->width; bit--, x++) {
		if (output_byte & (1 << bit)) {
		    _cairo_output_stream_printf (fontout->xml_node_glyphs,
						 "<rect x=\"%d\" y=\"%d\" width=\"1\" height=\"1\"/>\n",
						 x, y);
		}
	    }
	}
    }
    _cairo_output_stream_printf (fontout->xml_node_glyphs, "</g>\n");

    cairo_surface_destroy (&image->base);

    return CAIRO_STATUS_SUCCESS;
}

static cairo_status_t
_cairo_svg_document_emit_glyph (cairo_svg_font_output_t	*fontout,
				cairo_scaled_font_t	*scaled_font,
				unsigned long		 scaled_font_glyph_index,
				unsigned int		 font_id,
				unsigned int		 subset_glyph_index)
{
    cairo_status_t	     status;

    _cairo_output_stream_printf (fontout->xml_node_glyphs,
				 "<symbol overflow=\"visible\" id=\"glyph%d-%d\">\n",
				 font_id,
				 subset_glyph_index);

    status = _cairo_svg_document_emit_outline_glyph_data (fontout,
							  scaled_font,
							  scaled_font_glyph_index);
    if (status == CAIRO_INT_STATUS_UNSUPPORTED)
	status = _cairo_svg_document_emit_bitmap_glyph_data (fontout,
							     scaled_font,
							     scaled_font_glyph_index);
    if (unlikely (status))
	return status;

    _cairo_output_stream_printf (fontout->xml_node_glyphs, "</symbol>\n");

    return CAIRO_STATUS_SUCCESS;
}

static cairo_int_status_t
_cairo_svg_document_emit_outline_text_glyph_data (cairo_svg_font_output_t	*fontout,
					     cairo_scaled_font_t	*scaled_font,
					     unsigned long		 glyph_index,
						const char* utf8)
{
    cairo_scaled_glyph_t *scaled_glyph;
    cairo_matrix_t matrix;
    cairo_int_status_t status = CAIRO_STATUS_SUCCESS;
    long c;
    int len, i;
	double font_size;
	cairo_bool_t missing = FALSE;

    status = _cairo_scaled_glyph_lookup (scaled_font,
					 glyph_index,
					 CAIRO_SCALED_GLYPH_INFO_METRICS|
					 CAIRO_SCALED_GLYPH_INFO_PATH,
					 &scaled_glyph);
    if (unlikely (status))
	return status;

    font_size = scaled_font->font_matrix.xx;
    if (font_size == 0) {
    	font_size = -scaled_font->font_matrix.xy;
    }

	if (utf8 == NULL || ((unsigned char)utf8[0] == (unsigned char)0xEF && (unsigned char)utf8[1] == (unsigned char)0xBF && (unsigned char)utf8[2] == (unsigned char)0xBF)) {
		if (scaled_glyph->x_advance) {
		    _cairo_output_stream_printf (fontout->xml_node_glyphs,
						 "<missing-glyph horiz-adv-x=\"%d\"><path ",
						 (int)(scaled_glyph->x_advance * 2048 / font_size));
		}
		else {
		    _cairo_output_stream_printf (fontout->xml_node_glyphs,
						 "<missing-glyph><path ");
		}
		missing = TRUE;
	}
	else {
		_cairo_output_stream_printf (fontout->xml_node_glyphs,
					 "<glyph unicode=\"");
		len = strlen(utf8);
		for (i = 0; i < len;) {
			_escape_html(utf8, &i, fontout->xml_node_glyphs);
		}
		if (scaled_glyph->x_advance) {
			_cairo_output_stream_printf (fontout->xml_node_glyphs,
						 "\" horiz-adv-x=\"%d\" ", (int)(scaled_glyph->x_advance * 2048 / font_size));
		}
		else {
			_cairo_output_stream_printf (fontout->xml_node_glyphs,
						 "\" ");
		}
	}
	cairo_matrix_init(&matrix, 2048 / font_size, 0, 0, -2048 / font_size, 0, 0);

    _cairo_svg_surface_emit_path (fontout->xml_node_glyphs,
				  scaled_glyph->path, &matrix, GLYPH_PRECISION);

    if (missing) {
    	_cairo_output_stream_printf (fontout->xml_node_glyphs, "/></missing-glyph>\n");
    }
    else {
    	_cairo_output_stream_printf (fontout->xml_node_glyphs, "/>\n");
    }
    return status;
}


static cairo_int_status_t
_cairo_svg_surface_emit_font_face (cairo_output_stream_t	*output,
		const cairo_scaled_font_subset_t *subset)
{
    _cairo_output_stream_printf (output,
				 " font-family=\"font%d\"", subset->font_id);
   _cairo_output_stream_printf (output,
				 " units-per-em=\"2048.6\"");

    _cairo_output_stream_printf (output,
				 " ascent=\"2048\"");
    _cairo_output_stream_printf (output,
				 " descent=\"0\"");

    return CAIRO_STATUS_SUCCESS;
}

static cairo_status_t
_cairo_svg_document_emit_font_subset_element (cairo_scaled_font_subset_t	*font_subset,
		cairo_svg_font_output_t *fontout)
{
	cairo_status_t status = CAIRO_STATUS_SUCCESS;
	int i;
	double font_size;

    font_size = font_subset->scaled_font->font_matrix.xx;
    if (font_size == 0) {
    	font_size = -font_subset->scaled_font->font_matrix.xy;
    }

    if (fontout->prev_id != font_subset->font_id) {
		if (fontout->fontfile == NULL && fontout->prev_id != -1) {
			_cairo_output_stream_printf (fontout->xml_node_glyphs, "</font>\n");
		}
		fontout->prev_id = font_subset->font_id;
    	_cairo_output_stream_printf (fontout->xml_node_glyphs,
						 "<font id=\"font-%d\" horiz-adv-x=\"2048\"",
						 font_subset->font_id);
		_cairo_output_stream_printf (fontout->xml_node_glyphs,
						 ">\n");
		_cairo_output_stream_printf (fontout->xml_node_glyphs,
						 "<font-face");
		_cairo_svg_surface_emit_font_face(fontout->xml_node_glyphs, font_subset);
		_cairo_output_stream_printf (fontout->xml_node_glyphs,
						 "/>\n");
    }

	_cairo_scaled_font_freeze_cache (font_subset->scaled_font);
	for (i = 0; i < font_subset->num_glyphs; i++) {
	status = _cairo_svg_document_emit_outline_text_glyph_data (fontout,
							 font_subset->scaled_font,
							 font_subset->glyphs[i],
							 font_subset->utf8[i]);
	if (unlikely (status))
		break;
	}
	_cairo_scaled_font_thaw_cache (font_subset->scaled_font);
	return status;
}

static cairo_status_t
_cairo_svg_document_emit_font_subset (cairo_scaled_font_subset_t	*font_subset,
				      void				*closure)
{
	cairo_svg_font_output_t *fontout = closure;
    unsigned int i;
    cairo_status_t status = CAIRO_STATUS_SUCCESS;

    if (fontout->glyphs)
    {
		_cairo_scaled_font_freeze_cache (font_subset->scaled_font);
		for (i = 0; i < font_subset->num_glyphs; i++) {
		status = _cairo_svg_document_emit_glyph (fontout,
								 font_subset->scaled_font,
								 font_subset->glyphs[i],
								 font_subset->font_id, i);
		if (unlikely (status))
			return status;
		}
		_cairo_scaled_font_thaw_cache (font_subset->scaled_font);
    }
    else
    {
    	if (fontout->fontfile) {
    	    if (fontout->prev_id != font_subset->font_id) {
    			_cairo_output_stream_printf (fontout->xml_node_glyphs,
    					"@font-face {\n");
				_cairo_output_stream_printf (fontout->xml_node_glyphs,
						"	font-family: \"font%d\";\n", font_subset->font_id);
				_cairo_output_stream_printf (fontout->xml_node_glyphs,
						"	src: url(\"%s/font-%d.svg\") format(\"svg\");\n", fontout->fontfile->path, font_subset->font_id);
				fontout->prev_id = font_subset->font_id;
				_cairo_output_stream_printf (fontout->xml_node_glyphs, "}\n");
    	    }
    	}
    	else {
    		status = _cairo_svg_document_emit_font_subset_element(font_subset, fontout);
    	}
     }
    return status;
}

static cairo_status_t
_cairo_svg_document_emit_font_subset_file (cairo_scaled_font_subset_t	*font_subset,
				      void				*closure)
{
	cairo_svg_font_output_t *fontout = closure;
    cairo_status_t status = CAIRO_STATUS_SUCCESS;
    char file[256];

    if (fontout->prev_id == -1 || fontout->prev_id != font_subset->font_id) {
    	if (fontout->prev_id != -1) {
    		_cairo_output_stream_printf (fontout->xml_node_glyphs, "</font>\n");
    	    _cairo_output_stream_printf (fontout->xml_node_glyphs, "</defs>\n");
    	 	_cairo_output_stream_printf (fontout->xml_node_glyphs, "</svg>\n");
    	 	status = _cairo_output_stream_destroy (fontout->xml_node_glyphs);
    	    if (unlikely (status))
    	    	return status;
    	}
		sprintf(file, "%s/font-%d.svg", fontout->path, font_subset->font_id);
		fontout->xml_node_glyphs = _cairo_output_stream_create_for_filename (file);
		_cairo_output_stream_printf (fontout->xml_node_glyphs, "<svg xmlns=\"http://www.w3.org/2000/svg\">\n");
		_cairo_output_stream_printf (fontout->xml_node_glyphs, "<defs>\n");
    }

    status = _cairo_output_stream_get_status (fontout->xml_node_glyphs);
    if (unlikely (status))
    	return status;

    if (!fontout->glyphs) {
    	status = _cairo_svg_document_emit_font_subset_element(font_subset, fontout);
    }
 	return status;
}

static cairo_status_t
_cairo_svg_document_emit_font_subsets (cairo_svg_font_output_t *fontout)
{
    cairo_status_t status;
    cairo_scaled_font_subsets_t *font_subsets = fontout->font_subsets;

    if (!fontout->scaled) {
		status = _cairo_scaled_font_subsets_foreach_unscaled (font_subsets,
															_cairo_svg_document_emit_font_subset,
															fontout);
		if (fontout->prev_id != -1) {
			if (!fontout->fontfile) {
				_cairo_output_stream_printf (fontout->xml_node_glyphs, "</font>\n");
			}
		}
    }
    else {
        status = _cairo_scaled_font_subsets_foreach_scaled (font_subsets,
                                                            _cairo_svg_document_emit_font_subset,
                                                            fontout);
    }
    if (unlikely (status))
	goto FAIL;

    status = _cairo_scaled_font_subsets_foreach_user (font_subsets,
						      _cairo_svg_document_emit_font_subset,
						      fontout);

  FAIL:
  if (!fontout->fontfile) {
    _cairo_scaled_font_subsets_destroy (font_subsets);
  }
  fontout->font_subsets = NULL;

    return status;
}

static cairo_status_t
_cairo_svg_document_emit_font_subsets_file (cairo_svg_font_output_t *fontout)
{
    cairo_status_t status;
    cairo_scaled_font_subsets_t *font_subsets = fontout->font_subsets;

	status = _cairo_scaled_font_subsets_foreach_unscaled (font_subsets,
														_cairo_svg_document_emit_font_subset_file,
															fontout);
    if (unlikely (status))
	goto FAIL;

    status = _cairo_scaled_font_subsets_foreach_user (font_subsets,
						      _cairo_svg_document_emit_font_subset_file,
						      fontout);

	if (fontout->prev_id != -1) {
		_cairo_output_stream_printf (fontout->xml_node_glyphs, "</font>\n");
	    _cairo_output_stream_printf (fontout->xml_node_glyphs, "</defs>\n");
	 	_cairo_output_stream_printf (fontout->xml_node_glyphs, "</svg>\n");
	 	status = _cairo_output_stream_destroy (fontout->xml_node_glyphs);
	    if (unlikely (status))
	    	return status;
	}
  FAIL:
  if (!fontout->fontfile) {
    _cairo_scaled_font_subsets_destroy (font_subsets);
  }
  fontout->font_subsets = NULL;

    return status;
}

static char const *
_cairo_svg_surface_operators[] = {
    "clear",

    "src", "src-over", "src-in",
    "src-out", "src-atop",

    "dst", "dst-over", "dst-in",
    "dst-out", "dst-atop",

    "xor", "plus",
    "color-dodge", /* FIXME: saturate ? */

    "multiply",	"screen", "overlay",
    "darken", "lighten",
    "color-dodge", "color-burn",
    "hard-light", "soft-light",
    "difference", "exclusion"
};

static cairo_bool_t
_cairo_svg_surface_analyze_operator (cairo_svg_surface_t   *surface,
				      cairo_operator_t	     op)
{
    /* guard against newly added operators */
    if (op >= ARRAY_LENGTH (_cairo_svg_surface_operators))
	return CAIRO_INT_STATUS_UNSUPPORTED;

    /* allow operators being NULL if they are unsupported */
    if (_cairo_svg_surface_operators[op] == NULL)
	return CAIRO_INT_STATUS_UNSUPPORTED;

    return CAIRO_STATUS_SUCCESS;
}

static cairo_int_status_t
_cairo_svg_surface_analyze_operation (cairo_svg_surface_t   *surface,
				      cairo_operator_t	     op,
				      const cairo_pattern_t *pattern)
{
    cairo_svg_document_t *document = surface->document;

    if (surface->force_fallbacks &&
	surface->paginated_mode == CAIRO_PAGINATED_MODE_ANALYZE)
    {
	return CAIRO_INT_STATUS_UNSUPPORTED;
    }

    /* SVG doesn't support extend reflect for image pattern */
    if (pattern->type == CAIRO_PATTERN_TYPE_SURFACE &&
	pattern->extend == CAIRO_EXTEND_REFLECT)
	return CAIRO_INT_STATUS_UNSUPPORTED;

    if (document->svg_version >= CAIRO_SVG_VERSION_1_2)
	return _cairo_svg_surface_analyze_operator (surface, op);

    if (op == CAIRO_OPERATOR_OVER)
	return CAIRO_STATUS_SUCCESS;

    /* The SOURCE operator is only supported if there is nothing
     * painted underneath. */
    if (op == CAIRO_OPERATOR_SOURCE)
	return CAIRO_INT_STATUS_FLATTEN_TRANSPARENCY;

    return CAIRO_INT_STATUS_UNSUPPORTED;
}

static cairo_int_status_t
_cairo_svg_surface_operation_supported (cairo_svg_surface_t	*surface,
					cairo_operator_t	 op,
					const cairo_pattern_t	*pattern)
{
    return _cairo_svg_surface_analyze_operation (surface, op, pattern) != CAIRO_INT_STATUS_UNSUPPORTED;
}

static cairo_status_t
_cairo_svg_surface_finish (void *abstract_surface)
{
    cairo_status_t status, status2;
    cairo_svg_surface_t *surface = abstract_surface;
    cairo_svg_document_t *document = surface->document;
    cairo_svg_page_t *page;
    unsigned int i;

    if (_cairo_paginated_surface_get_target (document->owner) == &surface->base)
	status = _cairo_svg_document_finish (document);
    else
	status = CAIRO_STATUS_SUCCESS;

    if (surface->xml_node != NULL) {
	status2 = _cairo_output_stream_destroy (surface->xml_node);
	if (status == CAIRO_STATUS_SUCCESS)
	    status = status2;
    }

    for (i = 0; i < surface->page_set.num_elements; i++) {
	page = _cairo_array_index (&surface->page_set, i);
	status2 = _cairo_output_stream_destroy (page->xml_node);
	if (status == CAIRO_STATUS_SUCCESS)
	    status = status2;
    }
    _cairo_array_fini (&surface->page_set);

    _cairo_surface_clipper_reset (&surface->clipper);

    status2 = _cairo_svg_document_destroy (document);
    if (status == CAIRO_STATUS_SUCCESS)
	status = status2;

    return status;
}


static void
_cairo_svg_surface_emit_alpha_filter (cairo_svg_document_t *document)
{
    if (document->alpha_filter)
	return;
/*
    _cairo_output_stream_printf (document->xml_node_defs,
				 "<filter id=\"alpha\" "
				 "filterUnits=\"objectBoundingBox\" "
				 "x=\"0%%\" y=\"0%%\" "
				 "width=\"100%%\" height=\"100%%\">\n"
				 "  <feColorMatrix type=\"matrix\" "
				 "in=\"SourceGraphic\" "
				 "values=\"0 0 0 0 1 0 0 0 0 1 0 0 0 0 1 0 0 0 1 0\"/>\n"
				 "</filter>\n");
*/
    document->alpha_filter = TRUE;
}

typedef struct {
    cairo_output_stream_t *output;
    unsigned int in_mem;
    unsigned int trailing;
    unsigned char src[3];
} base64_write_closure_t;

static char const base64_table[64] =
"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

static cairo_status_t
base64_write_func (void *closure,
		   const unsigned char *data,
		   unsigned int length)
{
    base64_write_closure_t *info = (base64_write_closure_t *) closure;
    unsigned int i;
    unsigned char *src;

    src = info->src;

    if (info->in_mem + length < 3) {
	for (i = 0; i < length; i++) {
	    src[i + info->in_mem] = *data++;
	}
	info->in_mem += length;
	return CAIRO_STATUS_SUCCESS;
    }

    do {
	unsigned char dst[4];

	for (i = info->in_mem; i < 3; i++) {
	    src[i] = *data++;
	    length--;
	}
	info->in_mem = 0;

	dst[0] = base64_table[src[0] >> 2];
	dst[1] = base64_table[(src[0] & 0x03) << 4 | src[1] >> 4];
	dst[2] = base64_table[(src[1] & 0x0f) << 2 | src[2] >> 6];
	dst[3] = base64_table[src[2] & 0xfc >> 2];
	/* Special case for the last missing bits */
	switch (info->trailing) {
	    case 2:
		dst[2] = '=';
	    case 1:
		dst[3] = '=';
	    default:
		break;
	}
	_cairo_output_stream_write (info->output, dst, 4);
    } while (length >= 3);

    for (i = 0; i < length; i++) {
	src[i] = *data++;
    }
    info->in_mem = length;

    return _cairo_output_stream_get_status (info->output);
}

static cairo_int_status_t
_cairo_surface_base64_encode_jpeg (cairo_surface_t       *surface,
				   cairo_output_stream_t *output)
{
    const unsigned char *mime_data;
    unsigned long mime_data_length;
    cairo_image_info_t image_info;
    base64_write_closure_t info;
    cairo_status_t status;

    cairo_surface_get_mime_data (surface, CAIRO_MIME_TYPE_JPEG,
				 &mime_data, &mime_data_length);
    if (mime_data == NULL)
	return CAIRO_INT_STATUS_UNSUPPORTED;

    status = _cairo_image_info_get_jpeg_info (&image_info, mime_data, mime_data_length);
    if (unlikely (status))
	return status;

    _cairo_output_stream_printf (output, "data:image/jpeg;base64,");

    info.output = output;
    info.in_mem = 0;
    info.trailing = 0;

    status = base64_write_func (&info, mime_data, mime_data_length);
    if (unlikely (status))
	return status;

    if (info.in_mem > 0) {
	memset (info.src + info.in_mem, 0, 3 - info.in_mem);
	info.trailing = 3 - info.in_mem;
	info.in_mem = 3;
	status = base64_write_func (&info, NULL, 0);
    }

    return status;
}

static cairo_int_status_t
_cairo_surface_base64_encode_png (cairo_surface_t       *surface,
				  cairo_output_stream_t *output)
{
    const unsigned char *mime_data;
    unsigned long mime_data_length;
    base64_write_closure_t info;
    cairo_status_t status;

    cairo_surface_get_mime_data (surface, CAIRO_MIME_TYPE_PNG,
				 &mime_data, &mime_data_length);
    if (unlikely (surface->status))
	return surface->status;
    if (mime_data == NULL)
	return CAIRO_INT_STATUS_UNSUPPORTED;

    _cairo_output_stream_printf (output, "data:image/png;base64,");

    info.output = output;
    info.in_mem = 0;
    info.trailing = 0;

    status = base64_write_func (&info, mime_data, mime_data_length);
    if (unlikely (status))
	return status;

    if (info.in_mem > 0) {
	memset (info.src + info.in_mem, 0, 3 - info.in_mem);
	info.trailing = 3 - info.in_mem;
	info.in_mem = 3;
	status = base64_write_func (&info, NULL, 0);
    }

    return status;
}

static cairo_int_status_t
_cairo_surface_base64_encode (cairo_surface_t       *surface,
			      cairo_output_stream_t *output)
{
    cairo_status_t status;
    base64_write_closure_t info;

    status = _cairo_surface_base64_encode_jpeg (surface, output);
    if (status != CAIRO_INT_STATUS_UNSUPPORTED)
	return status;

    status = _cairo_surface_base64_encode_png (surface, output);
    if (status != CAIRO_INT_STATUS_UNSUPPORTED)
	return status;

    info.output = output;
    info.in_mem = 0;
    info.trailing = 0;

    _cairo_output_stream_printf (info.output, "data:image/png;base64,");

    status = cairo_surface_write_to_png_stream (surface, base64_write_func,
						(void *) &info);

    if (unlikely (status))
	return status;

    if (info.in_mem > 0) {
	memset (info.src + info.in_mem, 0, 3 - info.in_mem);
	info.trailing = 3 - info.in_mem;
	info.in_mem = 3;
	status = base64_write_func (&info, NULL, 0);
    }

    return status;
}

static void
_cairo_svg_surface_emit_operator (cairo_output_stream_t *output,
				  cairo_svg_surface_t   *surface,
				  cairo_operator_t	 op)
{
    if (surface->document->svg_version >= CAIRO_SVG_VERSION_1_2 &&
	op != CAIRO_OPERATOR_OVER) {
        if ((surface->document->styles & STYLE_D) == 0) {
        	_cairo_output_stream_printf(surface->document->css, ".sd {opacity:.7;}\n");
        	surface->document->styles |= STYLE_D;
        }
    	_cairo_output_stream_printf (output, " class=\"sd\"");
    // miyabe FIXME_cairo_output_stream_printf (output, " comp-op=\"%s\"", _cairo_svg_surface_operators[op]);
	if (!_cairo_operator_bounded_by_source (op))
	   _cairo_output_stream_printf (output, " clip-to-self=\"true\"");
    }
}

static void
_cairo_svg_surface_emit_operator_for_style (cairo_output_stream_t *output,
					    cairo_svg_surface_t   *surface,
					    cairo_operator_t	 op)
{
    if (surface->document->svg_version >= CAIRO_SVG_VERSION_1_2 &&
	op != CAIRO_OPERATOR_OVER) {
	_cairo_output_stream_printf (output, "comp-op:%s;", _cairo_svg_surface_operators[op]);
	if (!_cairo_operator_bounded_by_source (op))
	   _cairo_output_stream_printf (output, "clip-to-self:true;");
    }
}

/**
 * _cairo_svg_surface_emit_attr_value:
 *
 * Write the value to output the stream as a sequence of characters,
 * while escaping those which have special meaning in the XML
 * attribute's value context: &amp; and &quot;.
 **/
static void
_cairo_svg_surface_emit_attr_value (cairo_output_stream_t *stream,
				    const unsigned char *value,
				    unsigned int length)
{
    const unsigned char *p;
    const unsigned char *q;
    unsigned int i;

    /* we'll accumulate non-special chars in [q, p) range */
    p = value;
    q = p;
    for (i = 0; i < length; i++, p++) {
	if (*p == '&' || *p == '"') {
	    /* flush what's left before special char */
	    if (p != q) {
		_cairo_output_stream_write (stream, q, p - q);
		q = p + 1;
	    }

	    if (*p == '&')
		_cairo_output_stream_printf (stream, "&amp;");
	    else // p == '"'
		_cairo_output_stream_printf (stream, "&quot;");
	}
    }

    /* flush the trailing chars if any */
    if (p != q)
	_cairo_output_stream_write (stream, q, p - q);
}

static cairo_int_status_t
_cairo_surface_image_func (cairo_svg_surface_t       *svg_surface,
			     cairo_surface_t       *surface,
			     cairo_output_stream_t *out)
{
	char filename[256];
	cairo_status_t status;

	status = svg_surface->document->write_image_func(svg_surface->document->write_image_closure, surface, filename);
	if (status != CAIRO_INT_STATUS_UNSUPPORTED) {
		_cairo_svg_surface_emit_attr_value (out,
				    (const unsigned char*)filename, strlen(filename));
	}
	return status;
}

static cairo_status_t
_cairo_svg_surface_emit_surface (cairo_svg_surface_t *svg_surface,
				 cairo_surface_t *surface,
				 cairo_output_stream_t *out,
				 double x, double y,
				 double width, double height)
{
    cairo_svg_document_t *document;
    cairo_status_t status;
    const unsigned char *uri;
    unsigned long uri_len;

	document = svg_surface->document;
    if (_cairo_user_data_array_get_data (&surface->user_data,
					 (cairo_user_data_key_t *) document))
    {
	return CAIRO_STATUS_SUCCESS;
    }

    _cairo_output_stream_printf (out, "<image id=\"image%d\"", surface->unique_id);
    if (x != 0 || y != 0) {
        _cairo_output_stream_printf (out,
    				 " x=\"%f\" y=\"%f\"",
    				 _round(x, PATH_PRECISION), _round(y, PATH_PRECISION));
    }
    _cairo_output_stream_printf (out,
				 " width=\"%f\" height=\"%f\"",
				 _round(width, PATH_PRECISION), _round(height, PATH_PRECISION));

    _cairo_output_stream_printf (out, " xlink:href=\"");

    cairo_surface_get_mime_data (surface, CAIRO_MIME_TYPE_URI,
				 &uri, &uri_len);
    if (uri != NULL) {
	_cairo_svg_surface_emit_attr_value (out,
					    uri, uri_len);
    } else {
    	if (document->write_image_func != NULL) {
			status = _cairo_surface_image_func (svg_surface,
						   surface, out);
			if (unlikely (status))
			    return status;
		}
		if (document->write_image_func == NULL || status == CAIRO_INT_STATUS_UNSUPPORTED) {
			status = _cairo_surface_base64_encode (surface,
					out);
		}
		if (unlikely (status))
		    return status;
    }

    _cairo_output_stream_printf (out, "\"/>\n");

    /* and tag it */
    return _cairo_user_data_array_set_data (&surface->user_data,
					    (cairo_user_data_key_t *) document,
					    document, NULL);
}

static cairo_status_t
_cairo_svg_surface_emit_composite_surface_pattern (cairo_output_stream_t   *output,
						   cairo_svg_surface_t	 *svg_surface,
						   cairo_operator_t	  op,
						   cairo_surface_pattern_t *pattern,
						   int			  pattern_id,
						   const cairo_matrix_t	 *parent_matrix,
						   const char		 *extra_attributes)
{
    cairo_status_t status;
    cairo_matrix_t p2u;
	cairo_rectangle_int_t extents;
	cairo_bool_t is_bounded;
	is_bounded = _cairo_surface_get_extents (pattern->surface, &extents);
	assert (is_bounded);
	cairo_svg_document_t *document = svg_surface->document;

    p2u = pattern->base.matrix;
    status = cairo_matrix_invert (&p2u);
    /* cairo_pattern_set_matrix ensures the matrix is invertible */
    assert (status == CAIRO_STATUS_SUCCESS);

    if (parent_matrix != NULL)
	cairo_matrix_multiply (&p2u, &p2u, parent_matrix);

    status = _cairo_svg_surface_emit_surface (svg_surface,
					      pattern->surface, document->xml_node_defs,
					      0, 0, extents.width, extents.height);
    if (unlikely (status))
	return status;

    if (pattern_id != invalid_pattern_id) {
	assert (is_bounded);

	_cairo_output_stream_printf (output,
				     "<pattern id=\"pattern%d\" "
				     "patternUnits=\"userSpaceOnUse\" "
				     "width=\"%d\" height=\"%d\" ",
				     pattern_id,
				     extents.width, extents.height);
	_cairo_output_stream_printf (output, "x=\"%f\" y=\"%f\"",
			_round(p2u.x0, MATRIX_PRECISION), _round(p2u.y0, MATRIX_PRECISION));
	_cairo_output_stream_printf (output, ">\n");
    }

    _cairo_output_stream_printf (output,
				 "<use xlink:href=\"#image%d\"",
				 pattern->surface->unique_id);
	_cairo_output_stream_printf (output, " transform=\"matrix(%f,%f,%f,%f,0,0)\"",
		     _round(p2u.xx < 0.0 ? -p2u.xx : p2u.xx, MATRIX_PRECISION), _round(p2u.yx, MATRIX_PRECISION),
		     _round(p2u.xy, MATRIX_PRECISION), _round(p2u.yy < 0.0 ? -p2u.yy : p2u.yy, MATRIX_PRECISION));
    if (extra_attributes)
	_cairo_output_stream_printf (output, " %s", extra_attributes);

    if (pattern_id == invalid_pattern_id) {
	_cairo_svg_surface_emit_operator (output, svg_surface, op);
    }
    _cairo_output_stream_printf (output, "/>\n");


    if (pattern_id != invalid_pattern_id)
	_cairo_output_stream_printf (output, "</pattern>\n");

    return CAIRO_STATUS_SUCCESS;
}

static cairo_status_t
_cairo_svg_surface_emit_recording_surface (cairo_svg_document_t      *document,
					   cairo_recording_surface_t *source)
{
    cairo_status_t status;
    cairo_surface_t *paginated_surface;
    cairo_svg_surface_t *svg_surface;
    cairo_array_t *page_set;

    cairo_output_stream_t *contents;

    if (_cairo_user_data_array_get_data (&source->base.user_data,
					 (cairo_user_data_key_t *) document))
    {
	return CAIRO_STATUS_SUCCESS;
    }

    paginated_surface = _cairo_svg_surface_create_for_document (document,
								source->content,
								source->extents_pixels.width,
								source->extents_pixels.height);
    if (unlikely (paginated_surface->status))
	return paginated_surface->status;

    svg_surface = (cairo_svg_surface_t *)
    _cairo_paginated_surface_get_target (paginated_surface);
    cairo_surface_set_fallback_resolution (paginated_surface,
					   document->owner->x_fallback_resolution,
					   document->owner->y_fallback_resolution);
    cairo_surface_set_device_offset (&svg_surface->base,
				     -source->extents_pixels.x,
				     -source->extents_pixels.y);

    status = _cairo_recording_surface_replay (&source->base, paginated_surface);
    if (unlikely (status)) {
	cairo_surface_destroy (paginated_surface);
	return status;
    }

    cairo_surface_show_page (paginated_surface);
    status = cairo_surface_status (paginated_surface);
    if (unlikely (status)) {
	cairo_surface_destroy (paginated_surface);
	return status;
    }

    if (! svg_surface->is_base_clip_emitted) {
	svg_surface->is_base_clip_emitted = TRUE;
	_cairo_output_stream_printf (document->xml_node_defs,
				     "<clipPath id=\"clip%d\">\n"
				     "  <rect width=\"%f\" height=\"%f\"/>\n"
				     "</clipPath>\n",
				     svg_surface->base_clip,
				     _round(svg_surface->width, PATH_PRECISION),
				     _round(svg_surface->height, PATH_PRECISION));
    }

    if (source->content == CAIRO_CONTENT_ALPHA) {
	_cairo_svg_surface_emit_alpha_filter (document);
	_cairo_output_stream_printf (document->xml_node_defs,
				     "<g id=\"surface%d\" "
				     "clip-path=\"url(#clip%d)\" "
				     "filter=\"url(#alpha)\">\n",
				     source->base.unique_id,
				     svg_surface->base_clip);
    } else {
	_cairo_output_stream_printf (document->xml_node_defs,
				     "<g id=\"surface%d\" "
				     "clip-path=\"url(#clip%d)\">\n",
				     source->base.unique_id,
				     svg_surface->base_clip);
    }

    contents = svg_surface->xml_node;
    page_set = &svg_surface->page_set;

    if (_cairo_memory_stream_length (contents) > 0) {
	if (unlikely (_cairo_svg_surface_store_page (svg_surface) == NULL)) {
	    cairo_surface_destroy (paginated_surface);
	    return _cairo_error (CAIRO_STATUS_NO_MEMORY);
	}
    }

    if (page_set->num_elements > 0) {
	cairo_svg_page_t *page;

	page = _cairo_array_index (page_set, page_set->num_elements - 1);
	_cairo_memory_stream_copy (page->xml_node, document->xml_node_defs);
    }

    _cairo_output_stream_printf (document->xml_node_defs, "</g>\n");

    status = cairo_surface_status (paginated_surface);
    cairo_surface_destroy (paginated_surface);

    if (unlikely (status))
	return status;

    /* and tag it */
    return _cairo_user_data_array_set_data (&source->base.user_data,
					    (cairo_user_data_key_t *) document,
					    document, NULL);
}

static cairo_status_t
_cairo_svg_surface_emit_composite_recording_pattern (cairo_output_stream_t	*output,
						     cairo_svg_surface_t	*surface,
						     cairo_operator_t	         op,
						     cairo_surface_pattern_t	*pattern,
						     int			 pattern_id,
						     const cairo_matrix_t	*parent_matrix,
						     const char			*extra_attributes)
{
    cairo_svg_document_t *document = surface->document;
    cairo_recording_surface_t *recording_surface;
    cairo_matrix_t p2u;
    cairo_status_t status;

    _cairo_svg_end_text(surface);

    p2u = pattern->base.matrix;
    status = cairo_matrix_invert (&p2u);
    /* cairo_pattern_set_matrix ensures the matrix is invertible */
    assert (status == CAIRO_STATUS_SUCCESS);

    recording_surface = (cairo_recording_surface_t *) pattern->surface;
    status = _cairo_svg_surface_emit_recording_surface (document, recording_surface);
    if (unlikely (status))
	return status;

    if (pattern_id != invalid_pattern_id) {
	_cairo_output_stream_printf (output,
				     "<pattern id=\"pattern%d\" "
				     "patternUnits=\"userSpaceOnUse\" "
				     "width=\"%d\" height=\"%d\"",
				     pattern_id,
				     recording_surface->extents.width,
				     recording_surface->extents.height);
	_cairo_svg_surface_emit_transform (output, " patternTransform", &p2u, parent_matrix);
	_cairo_output_stream_printf (output, ">\n");
    }

    _cairo_output_stream_printf (output,
				 "<use xlink:href=\"#surface%d\"",
				 recording_surface->base.unique_id);

    if (pattern_id == invalid_pattern_id) {
	_cairo_svg_surface_emit_operator (output, surface, op);
	_cairo_svg_surface_emit_transform (output, " transform", &p2u, parent_matrix);
    }

    if (extra_attributes)
	_cairo_output_stream_printf (output, " %s", extra_attributes);

    _cairo_output_stream_printf (output, "/>\n");

    if (pattern_id != invalid_pattern_id)
	_cairo_output_stream_printf (output, "</pattern>\n");

    return CAIRO_STATUS_SUCCESS;
}

static cairo_status_t
_cairo_svg_surface_emit_composite_pattern (cairo_output_stream_t   *output,
					   cairo_svg_surface_t	   *surface,
					   cairo_operator_t	    op,
					   cairo_surface_pattern_t *pattern,
					   int			    pattern_id,
					   const cairo_matrix_t	   *parent_matrix,
					   const char		   *extra_attributes)
{
    if (_cairo_surface_is_recording (pattern->surface)) {
	return _cairo_svg_surface_emit_composite_recording_pattern (output, surface,
								    op, pattern,
								    pattern_id,
								    parent_matrix,
								    extra_attributes);
    }

    return _cairo_svg_surface_emit_composite_surface_pattern (output, surface,
							      op, pattern,
							      pattern_id,
							      parent_matrix,
							      extra_attributes);
}

static cairo_status_t
_cairo_svg_surface_emit_solid_pattern (cairo_svg_surface_t    *surface,
				       cairo_solid_pattern_t  *pattern,
				       cairo_output_stream_t  *style,
				       cairo_bool_t	       is_stroke)
{
    _cairo_output_stream_printf (style, is_stroke ?
				 "stroke:rgb(%f%%,%f%%,%f%%);stroke-opacity:%f;":
				 "fill:rgb(%f%%,%f%%,%f%%);fill-opacity:%f;",
				 _round(pattern->color.red * 100.0, COLOR_PRECISION),
				 _round(pattern->color.green * 100.0, COLOR_PRECISION),
				 _round(pattern->color.blue * 100.0, COLOR_PRECISION),
				 _round(pattern->color.alpha, COLOR_PRECISION));

    return CAIRO_STATUS_SUCCESS;
}

static cairo_status_t
_cairo_svg_surface_emit_surface_pattern (cairo_svg_surface_t	 *surface,
					 cairo_surface_pattern_t *pattern,
					 cairo_output_stream_t   *style,
					 cairo_bool_t		  is_stroke,
					 const cairo_matrix_t	 *parent_matrix)
{
    cairo_svg_document_t *document = surface->document;
    cairo_status_t status;
    int pattern_id;

    pattern_id = document->pattern_id++;
    status = _cairo_svg_surface_emit_composite_pattern (document->xml_node_defs,
	                                                surface, CAIRO_OPERATOR_SOURCE, pattern,
							pattern_id, parent_matrix, NULL);
    if (unlikely (status))
	return status;

    _cairo_output_stream_printf (style,
				 "%s:url(#pattern%d);",
				 is_stroke ? "stroke" : "fill",
				 pattern_id);

    return CAIRO_STATUS_SUCCESS;
}

static cairo_output_stream_t*
_cairo_svg_surface_start_style()
{
	return _cairo_memory_stream_create ();
}

static cairo_svg_style*
_cairo_svg_surface_end_style(cairo_svg_document_t *document,
		cairo_output_stream_t *css)
{
	cairo_svg_style style;
	cairo_svg_style *entry;
	_cairo_memory_stream_destroy(css, &style.style, &style.length);

	style.base.hash = _CAIRO_HASH_INIT_VALUE;
	style.base.hash = _cairo_hash_bytes(style.base.hash, style.style, style.length);
	entry = _cairo_hash_table_lookup(document->style_to_id, &style.base);
	if (entry) {
		free(style.style);
	} else {
		style.id = ++document->style_id;
		entry = malloc(sizeof(style));
		*entry = style;
		_cairo_hash_table_insert(document->style_to_id, entry);
	}
	return entry;
}

static int
_cairo_svg_surface_color_class(cairo_svg_document_t *document,
		cairo_color_stop_t color)
{
	cairo_output_stream_t *css = _cairo_svg_surface_start_style ();
	_cairo_output_stream_printf (css,
				 "stop-color:rgb(%f%%,%f%%,%f%%);stop-opacity:%f;",
				 _round(color.red * 100.0, COLOR_PRECISION),
				 _round(color.green * 100.0, COLOR_PRECISION),
				 _round(color.blue * 100.0, COLOR_PRECISION),
				 _round(color.alpha, COLOR_PRECISION));
	cairo_svg_style *style = _cairo_svg_surface_end_style(document, css);
	return style->id;
}

static cairo_status_t
_cairo_svg_surface_emit_pattern_stops (cairo_svg_document_t *document,
						cairo_output_stream_t          *output,
				       cairo_gradient_pattern_t const *pattern,
				       double			       start_offset,
				       cairo_bool_t		       reverse_stops,
				       cairo_bool_t		       emulate_reflect)
{
    cairo_gradient_stop_t *stops;
    double offset;
    unsigned int n_stops;
    unsigned int i;
    int id;

    if (pattern->n_stops < 1)
	return CAIRO_STATUS_SUCCESS;

    if (pattern->n_stops == 1) {
    	id = _cairo_svg_surface_color_class(document, pattern->stops[0].color);
	    _cairo_output_stream_printf (output,
					 "<stop offset=\"%f\" class=\"s%d\"/>\n",
					 _round(pattern->stops[0].offset, PATH_PRECISION), id);
	    return CAIRO_STATUS_SUCCESS;
    }

    if (emulate_reflect || reverse_stops) {
	n_stops = emulate_reflect ? pattern->n_stops * 2 - 2: pattern->n_stops;
	stops = _cairo_malloc_ab (n_stops, sizeof (cairo_gradient_stop_t));
	if (unlikely (stops == NULL))
	    return _cairo_error (CAIRO_STATUS_NO_MEMORY);

	for (i = 0; i < pattern->n_stops; i++) {
	    if (reverse_stops) {
		stops[i] = pattern->stops[pattern->n_stops - i - 1];
		stops[i].offset = 1.0 - stops[i].offset;
	    } else
		stops[i] = pattern->stops[i];
	    if (emulate_reflect) {
		stops[i].offset /= 2;
		if (i > 0 && i < (pattern->n_stops - 1)) {
		    if (reverse_stops) {
			stops[i + pattern->n_stops - 1] = pattern->stops[i];
			stops[i + pattern->n_stops - 1].offset =
			    0.5 + 0.5 * stops[i + pattern->n_stops - 1].offset;
		    } else {
			stops[i + pattern->n_stops - 1] = pattern->stops[pattern->n_stops - i - 1];
			stops[i + pattern->n_stops - 1].offset =
			    1 - 0.5 * stops[i + pattern->n_stops - 1].offset;
		    }
		}
	    }
	}
    } else {
	n_stops = pattern->n_stops;
	stops = pattern->stops;
    }

    if (start_offset >= 0.0)
	for (i = 0; i < n_stops; i++) {
	    offset = start_offset + (1 - start_offset ) * stops[i].offset;
	    id = _cairo_svg_surface_color_class(document, stops[i].color);
	    _cairo_output_stream_printf (output,
					 "<stop offset=\"%f\" class=\"s%d\"/>\n",
					 _round(offset, PATH_PRECISION), id);
	}
    else {
	cairo_bool_t found = FALSE;
	unsigned int offset_index;
	cairo_color_stop_t offset_color_start, offset_color_stop;

	for (i = 0; i < n_stops; i++) {
	    if (stops[i].offset >= -start_offset) {
		if (i > 0) {
		    if (stops[i].offset != stops[i-1].offset) {
			double x0, x1;
			cairo_color_stop_t *color0, *color1;

			x0 = stops[i-1].offset;
			x1 = stops[i].offset;
			color0 = &stops[i-1].color;
			color1 = &stops[i].color;
			offset_color_start.red = color0->red + (color1->red - color0->red)
			    * (-start_offset - x0) / (x1 - x0);
			offset_color_start.green = color0->green + (color1->green - color0->green)
			    * (-start_offset - x0) / (x1 - x0);
			offset_color_start.blue = color0->blue + (color1->blue - color0->blue)
			    * (-start_offset - x0) / (x1 - x0);
			offset_color_start.alpha = color0->alpha + (color1->alpha - color0->alpha)
			    * (-start_offset - x0) / (x1 - x0);
			offset_color_stop = offset_color_start;
		    } else {
			offset_color_stop = stops[i-1].color;
			offset_color_start = stops[i].color;
		    }
		} else
			offset_color_stop = offset_color_start = stops[i].color;
	    offset_index = i;
	    found = TRUE;
	    break;
	    }
	}

	if (!found) {
	    offset_index = n_stops - 1;
	    offset_color_stop = offset_color_start = stops[offset_index].color;
	}

	id = _cairo_svg_surface_color_class(document, offset_color_start);
	_cairo_output_stream_printf (output,
				     "<stop offset=\"0\" class=\"s%d\"/>\n", id);
	for (i = offset_index; i < n_stops; i++) {
		id = _cairo_svg_surface_color_class(document, stops[i].color);
	    _cairo_output_stream_printf (output,
					 "<stop offset=\"%f\" class=\"s%d\"/>\n",
					 _round(stops[i].offset + start_offset, PATH_PRECISION), id);
	}
	for (i = 0; i < offset_index; i++) {
		id = _cairo_svg_surface_color_class(document, stops[i].color);
	    _cairo_output_stream_printf (output,
					 "<stop offset=\"%f\" class=\"s%d\"/>\n",
					 _round(1.0 + stops[i].offset + start_offset, PATH_PRECISION), id);
	}

	id = _cairo_svg_surface_color_class(document, offset_color_stop);
	_cairo_output_stream_printf (output,
				     "<stop offset=\"1\" class=\"s%d\"/>\n", id);

    }

    if (reverse_stops || emulate_reflect)
	free (stops);

    return CAIRO_STATUS_SUCCESS;
}

static void
_cairo_svg_surface_emit_pattern_extend (cairo_output_stream_t *output,
					cairo_pattern_t       *pattern)
{
    switch (pattern->extend) {
	case CAIRO_EXTEND_REPEAT:
	    _cairo_output_stream_printf (output, "spreadMethod=\"repeat\" ");
	    break;
	case CAIRO_EXTEND_REFLECT:
	    _cairo_output_stream_printf (output, "spreadMethod=\"reflect\" ");
	    break;
	case CAIRO_EXTEND_NONE:
	case CAIRO_EXTEND_PAD:
	    break;
    }
}


static cairo_status_t
_cairo_svg_surface_emit_linear_pattern (cairo_svg_surface_t    *surface,
					cairo_linear_pattern_t *pattern,
					cairo_output_stream_t  *style,
					cairo_bool_t	        is_stroke,
					const cairo_matrix_t   *parent_matrix)
{
    cairo_svg_document_t *document = surface->document;
    double x0, y0, x1, y1;
    cairo_matrix_t p2u;
    cairo_status_t status;

    p2u = pattern->base.base.matrix;
    status = cairo_matrix_invert (&p2u);
    /* cairo_pattern_set_matrix ensures the matrix is invertible */
    assert (status == CAIRO_STATUS_SUCCESS);

    x0 = _cairo_fixed_to_double (pattern->p1.x);
    y0 = _cairo_fixed_to_double (pattern->p1.y);
    x1 = _cairo_fixed_to_double (pattern->p2.x);
    y1 = _cairo_fixed_to_double (pattern->p2.y);

    _cairo_output_stream_printf (document->xml_node_defs,
				 "<linearGradient id=\"linear%d\" "
				 "gradientUnits=\"userSpaceOnUse\" "
				 "x1=\"%f\" y1=\"%f\" x2=\"%f\" y2=\"%f\" ",
				 document->linear_pattern_id,
				 _round(x0, PATH_PRECISION), _round(y0, PATH_PRECISION), _round(x1, PATH_PRECISION), _round(y1, PATH_PRECISION));

    _cairo_svg_surface_emit_pattern_extend (document->xml_node_defs, &pattern->base.base),
    _cairo_svg_surface_emit_transform (document->xml_node_defs, "gradientTransform", &p2u, parent_matrix);
    _cairo_output_stream_printf (document->xml_node_defs, ">\n");

    status = _cairo_svg_surface_emit_pattern_stops (document,
    													document->xml_node_defs,
	                                            &pattern->base, 0.0,
						    FALSE, FALSE);
    if (unlikely (status))
	return status;

    _cairo_output_stream_printf (document->xml_node_defs,
				 "</linearGradient>\n");

    _cairo_output_stream_printf (style,
				 "%s:url(#linear%d);",
				 is_stroke ? "stroke" : "fill",
				 document->linear_pattern_id);

    document->linear_pattern_id++;

    return CAIRO_STATUS_SUCCESS;
}

static cairo_status_t
_cairo_svg_surface_emit_radial_pattern (cairo_svg_surface_t    *surface,
					cairo_radial_pattern_t *pattern,
					cairo_output_stream_t  *style,
					cairo_bool_t            is_stroke,
					const cairo_matrix_t   *parent_matrix)
{
    cairo_svg_document_t *document = surface->document;
    cairo_matrix_t p2u;
    cairo_extend_t extend;
    double x0, y0, x1, y1, r0, r1;
    double fx, fy;
    cairo_bool_t reverse_stops;
    cairo_status_t status;
    cairo_point_t *c0, *c1;
    cairo_fixed_t radius0, radius1;
    int id;

    extend = pattern->base.base.extend;

    if (pattern->r1 < pattern->r2) {
	c0 = &pattern->c1;
	c1 = &pattern->c2;
	radius0 = pattern->r1;
	radius1 = pattern->r2;
	reverse_stops = FALSE;
    } else {
	c0 = &pattern->c2;
	c1 = &pattern->c1;
	radius0 = pattern->r2;
	radius1 = pattern->r1;
	reverse_stops = TRUE;
    }

    x0 = _cairo_fixed_to_double (c0->x);
    y0 = _cairo_fixed_to_double (c0->y);
    r0 = _cairo_fixed_to_double (radius0);
    x1 = _cairo_fixed_to_double (c1->x);
    y1 = _cairo_fixed_to_double (c1->y);
    r1 = _cairo_fixed_to_double (radius1);

    p2u = pattern->base.base.matrix;
    status = cairo_matrix_invert (&p2u);
    /* cairo_pattern_set_matrix ensures the matrix is invertible */
    assert (status == CAIRO_STATUS_SUCCESS);

    if (pattern->r1 == pattern->r2) {
	unsigned int n_stops = pattern->base.n_stops;

	_cairo_output_stream_printf (document->xml_node_defs,
				     "<radialGradient id=\"radial%d\" "
				     "gradientUnits=\"userSpaceOnUse\" "
				     "cx=\"%f\" cy=\"%f\" "
				     "fx=\"%f\" fy=\"%f\" r=\"%f\" ",
				     document->radial_pattern_id,
				     _round(x1, PATH_PRECISION), _round(y1, PATH_PRECISION),
				     _round(x1, PATH_PRECISION), _round(y1, PATH_PRECISION), _round(r1, PATH_PRECISION));
	_cairo_svg_surface_emit_transform (document->xml_node_defs,
					   "gradientTransform",
					   &p2u, parent_matrix);
	_cairo_output_stream_printf (document->xml_node_defs, ">\n");

	if (extend == CAIRO_EXTEND_NONE || n_stops < 1) {
	    if ((surface->document->styles & STYLE_E) == 0) {
	    	_cairo_output_stream_printf(surface->document->css, ".se {stop-color:rgb(0%%,0%%,0%%);stop-opacity:0;}\n");
	    	surface->document->styles |= STYLE_E;
	    }
	    _cairo_output_stream_printf (document->xml_node_defs,
					 "<stop offset=\"0\" class=\"se\"/>\n");
	}
	else {
		id = _cairo_svg_surface_color_class(document, pattern->base.stops[0].color);
	    _cairo_output_stream_printf (document->xml_node_defs,
					 "<stop offset=\"0\" class=\"s%d\"/>\n", id);
	    if (n_stops > 1) {
		id = _cairo_svg_surface_color_class(document, pattern->base.stops[n_stops - 1].color);
		_cairo_output_stream_printf (document->xml_node_defs,
					     "<stop offset=\"0\" color=\"s%d\"/>\n", id);
	    }
	 }

    } else {
	double offset, r, x, y;
	cairo_bool_t emulate_reflect = FALSE;

	fx = (r1 * x0 - r0 * x1) / (r1 - r0);
	fy = (r1 * y0 - r0 * y1) / (r1 - r0);

	/* SVG doesn't support the inner circle and use instead a gradient focal.
	 * That means we need to emulate the cairo behaviour by processing the
	 * cairo gradient stops.
	 * The CAIRO_EXTENT_NONE and CAIRO_EXTENT_PAD modes are quite easy to handle,
	 * it's just a matter of stop position translation and calculation of
	 * the corresponding SVG radial gradient focal.
	 * The CAIRO_EXTENT_REFLECT and CAIRO_EXTEND_REPEAT modes require to compute a new
	 * radial gradient, with an new outer circle, equal to r1 - r0 in the CAIRO_EXTEND_REPEAT
	 * case, and 2 * (r1 - r0) in the CAIRO_EXTENT_REFLECT case, and a new gradient stop
	 * list that maps to the original cairo stop list.
	 */
	if ((extend == CAIRO_EXTEND_REFLECT
	     || extend == CAIRO_EXTEND_REPEAT)
	    && r0 > 0.0) {
	    double r_org = r1;

	    if (extend == CAIRO_EXTEND_REFLECT) {
		r1 = 2 * r1 - r0;
		emulate_reflect = TRUE;
	    }

	    offset = fmod (r1, r1 - r0) / (r1 - r0) - 1.0;
	    r = r1 - r0;

	    /* New position of outer circle. */
	    x = r * (x1 - fx) / r_org + fx;
	    y = r * (y1 - fy) / r_org + fy;

	    x1 = x;
	    y1 = y;
	    r1 = r;
	    r0 = 0.0;
	} else {
	    offset = r0 / r1;
	}

	_cairo_output_stream_printf (document->xml_node_defs,
				     "<radialGradient id=\"radial%d\" "
				     "gradientUnits=\"userSpaceOnUse\" "
				     "cx=\"%f\" cy=\"%f\" "
				     "fx=\"%f\" fy=\"%f\" r=\"%f\" ",
				     document->radial_pattern_id,
				     _round(x1, PATH_PRECISION), _round(y1, PATH_PRECISION),
				     _round(fx, PATH_PRECISION), _round(fy, PATH_PRECISION), _round(r1, PATH_PRECISION));

	if (emulate_reflect)
	    _cairo_output_stream_printf (document->xml_node_defs, "spreadMethod=\"repeat\" ");
	else
	    _cairo_svg_surface_emit_pattern_extend (document->xml_node_defs, &pattern->base.base);
	_cairo_svg_surface_emit_transform (document->xml_node_defs, "gradientTransform", &p2u, parent_matrix);
	_cairo_output_stream_printf (document->xml_node_defs, ">\n");

	/* To support cairo's EXTEND_NONE, (for which SVG has no similar
	 * notion), we add transparent color stops on either end of the
	 * user-provided stops. */
	if (extend == CAIRO_EXTEND_NONE) {
	    if ((surface->document->styles & STYLE_E) == 0) {
	    	_cairo_output_stream_printf(surface->document->css, ".se {stop-color:rgb(0%%,0%%,0%%);stop-opacity:0;}\n");
	    	surface->document->styles |= STYLE_E;
	    }
	    _cairo_output_stream_printf (document->xml_node_defs,
					 "<stop offset=\"0\" class=\"se\"/>\n");
	    if (r0 != 0.0)
		_cairo_output_stream_printf (document->xml_node_defs,
					     "<stop offset=\"%f\" class=\"se\"/>\n",
					     r0 / r1);
	}
	status = _cairo_svg_surface_emit_pattern_stops (document,
														document->xml_node_defs,
		                                        &pattern->base, offset,
							reverse_stops,
							emulate_reflect);
	if (unlikely (status))
	    return status;

	if (pattern->base.base.extend == CAIRO_EXTEND_NONE)
	    if ((surface->document->styles & STYLE_E) == 0) {
	    	_cairo_output_stream_printf(surface->document->css, ".se {stop-color:rgb(0%%,0%%,0%%);stop-opacity:0;}\n");
	    	surface->document->styles |= STYLE_E;
	    }
	    _cairo_output_stream_printf (document->xml_node_defs,
					 "<stop offset=\"1.0\" class=\"se\"/>\n");
    }

    _cairo_output_stream_printf (document->xml_node_defs,
				 "</radialGradient>\n");

    _cairo_output_stream_printf (style,
				 "%s:url(#radial%d);",
				 is_stroke ? "stroke" : "fill",
				 document->radial_pattern_id);

    document->radial_pattern_id++;

    return CAIRO_STATUS_SUCCESS;
}

static cairo_status_t
_cairo_svg_surface_emit_pattern (cairo_svg_surface_t   *surface,
				 const cairo_pattern_t       *pattern,
				 cairo_output_stream_t *output,
				 cairo_bool_t		is_stroke,
				 const cairo_matrix_t  *parent_matrix)
{
    switch (pattern->type) {
    case CAIRO_PATTERN_TYPE_SOLID:
	return _cairo_svg_surface_emit_solid_pattern (surface, (cairo_solid_pattern_t *) pattern,
						      output, is_stroke);

    case CAIRO_PATTERN_TYPE_SURFACE:
	return _cairo_svg_surface_emit_surface_pattern (surface, (cairo_surface_pattern_t *) pattern,
							output, is_stroke, parent_matrix);

    case CAIRO_PATTERN_TYPE_LINEAR:
	return _cairo_svg_surface_emit_linear_pattern (surface, (cairo_linear_pattern_t *) pattern,
						       output, is_stroke, parent_matrix);

    case CAIRO_PATTERN_TYPE_RADIAL:
	return _cairo_svg_surface_emit_radial_pattern (surface, (cairo_radial_pattern_t *) pattern,
						       output, is_stroke, parent_matrix);
    }
    return _cairo_error (CAIRO_STATUS_PATTERN_TYPE_MISMATCH);
}

static cairo_status_t
_cairo_svg_surface_emit_fill_style (cairo_output_stream_t	*output,
				    cairo_svg_surface_t		*surface,
				    cairo_operator_t		 op,
				    const cairo_pattern_t	*source,
				    cairo_fill_rule_t		 fill_rule,
				    const cairo_matrix_t	*parent_matrix)
{
	cairo_output_stream_t *css = _cairo_svg_surface_start_style ();

	_cairo_output_stream_printf (css,
				 "fill-rule:%s;",
				 fill_rule == CAIRO_FILL_RULE_EVEN_ODD ?
				 "evenodd" : "nonzero");
	_cairo_svg_surface_emit_operator_for_style (css, surface, op);
	_cairo_svg_surface_emit_pattern (surface, source, css, FALSE, parent_matrix);

	cairo_svg_style *style = _cairo_svg_surface_end_style(surface->document, css);
	_cairo_output_stream_printf (output, "s%d", style->id);
    return CAIRO_STATUS_SUCCESS;
}

static cairo_status_t
_cairo_svg_surface_emit_stroke_style (cairo_output_stream_t	   *output,
				      cairo_svg_surface_t	   *surface,
				      cairo_operator_t		    op,
				      const cairo_pattern_t	   *source,
				      const cairo_stroke_style_t   *stroke_style,
				      const cairo_matrix_t	   *parent_matrix)
{
    cairo_status_t status;
    const char *line_cap, *line_join;
    unsigned int i;
    cairo_output_stream_t *css = _cairo_svg_surface_start_style ();

    switch (stroke_style->line_cap) {
	case CAIRO_LINE_CAP_BUTT:
	    line_cap = "butt";
	    break;
	case CAIRO_LINE_CAP_ROUND:
	    line_cap = "round";
	    break;
	case CAIRO_LINE_CAP_SQUARE:
	    line_cap = "square";
	    break;
	default:
	    ASSERT_NOT_REACHED;
    }

    switch (stroke_style->line_join) {
	case CAIRO_LINE_JOIN_MITER:
	    line_join = "miter";
	    break;
	case CAIRO_LINE_JOIN_ROUND:
	    line_join = "round";
	    break;
	case CAIRO_LINE_JOIN_BEVEL:
	    line_join = "bevel";
	    break;
	default:
	    ASSERT_NOT_REACHED;
    }

    _cairo_output_stream_printf (css,
				 "stroke-width:%f;"
				 "stroke-linecap:%s;"
				 "stroke-linejoin:%s;",
				 _round(stroke_style->line_width, PATH_PRECISION),
				 line_cap,
				 line_join);

     status = _cairo_svg_surface_emit_pattern (surface, source, css, TRUE, parent_matrix);
     if (unlikely (status))
	 return status;

     _cairo_svg_surface_emit_operator_for_style (css, surface, op);

    if (stroke_style->num_dashes > 0) {
	_cairo_output_stream_printf (css, "stroke-dasharray:");
	for (i = 0; i < stroke_style->num_dashes; i++) {
	    _cairo_output_stream_printf (css, "%f",
	    		stroke_style->dash[i] < 0.2 ? 0.2 : stroke_style->dash[i]);
	    if (i + 1 < stroke_style->num_dashes)
		_cairo_output_stream_printf (css, ",");
	    else
		_cairo_output_stream_printf (css, ";");
	}
	if (stroke_style->dash_offset != 0.0) {
	    _cairo_output_stream_printf (css,
					 "stroke-dashoffset:%f;",
					 _round(stroke_style->dash_offset, PATH_PRECISION));
	}
    }
    _cairo_output_stream_printf (css,
				 "stroke-miterlimit:%f;",
				 _round(stroke_style->miter_limit, PATH_PRECISION));

    cairo_svg_style *style = _cairo_svg_surface_end_style(surface->document, css);
	_cairo_output_stream_printf (output, "s%d", style->id);
    return CAIRO_STATUS_SUCCESS;
}

static cairo_int_status_t
_cairo_svg_surface_fill_stroke (void			*abstract_surface,
				cairo_operator_t	 fill_op,
				const cairo_pattern_t	*fill_source,
				cairo_fill_rule_t	 fill_rule,
				double			 fill_tolerance,
				cairo_antialias_t	 fill_antialias,
				cairo_path_fixed_t	*path,
				cairo_operator_t	 stroke_op,
				const cairo_pattern_t	*stroke_source,
				const cairo_stroke_style_t	*stroke_style,
				const cairo_matrix_t		*stroke_ctm,
				const cairo_matrix_t		*stroke_ctm_inverse,
				double			 stroke_tolerance,
				cairo_antialias_t	 stroke_antialias,
				cairo_clip_t		*clip)
{
    cairo_svg_surface_t *surface = abstract_surface;
    cairo_status_t status;

    _cairo_svg_end_text(surface);
    status = _cairo_surface_clipper_set_clip (&surface->clipper, clip);
    if (unlikely (status))
	return status;

    _cairo_output_stream_printf (surface->xml_node, "<path class=\"");
    status = _cairo_svg_surface_emit_fill_style (surface->xml_node, surface, fill_op,
						 fill_source, fill_rule, stroke_ctm_inverse);
    if (unlikely (status))
	return status;

    _cairo_output_stream_printf (surface->xml_node, " ");
    status = _cairo_svg_surface_emit_stroke_style (surface->xml_node, surface, stroke_op,
						   stroke_source, stroke_style, stroke_ctm_inverse);
    if (unlikely (status))
	return status;

    _cairo_output_stream_printf (surface->xml_node, "\" ");

    _cairo_svg_surface_emit_path (surface->xml_node, path, stroke_ctm_inverse, PATH_PRECISION);

    _cairo_svg_surface_emit_transform (surface->xml_node, " transform", stroke_ctm, NULL);
    _cairo_output_stream_printf (surface->xml_node, "/>\n");

    return CAIRO_STATUS_SUCCESS;
}

static cairo_int_status_t
_cairo_svg_surface_fill (void			*abstract_surface,
			 cairo_operator_t	 op,
			 const cairo_pattern_t	*source,
			 cairo_path_fixed_t	*path,
			 cairo_fill_rule_t	 fill_rule,
			 double			 tolerance,
			 cairo_antialias_t	 antialias,
			 cairo_clip_t		*clip)
{
    cairo_svg_surface_t *surface = abstract_surface;
    cairo_status_t status;

    if (surface->paginated_mode == CAIRO_PAGINATED_MODE_ANALYZE)
	return _cairo_svg_surface_analyze_operation (surface, op, source);

    assert (_cairo_svg_surface_operation_supported (surface, op, source));

    _cairo_svg_end_text(surface);
    status = _cairo_surface_clipper_set_clip (&surface->clipper, clip);
    if (unlikely (status))
	return status;

    cairo_bool_t drawpath = TRUE;
    cairo_surface_pattern_t* sp;
    double x1, y1, x2, y2;
    if (op == CAIRO_OPERATOR_OVER
    		&& source != NULL && source->type == CAIRO_PATTERN_TYPE_SURFACE
    		&& fill_rule == CAIRO_FILL_RULE_WINDING
    		&& path->is_rectilinear
    		&& path->buf.base.num_points == 5
    		&& path->buf.base.points[0].y == path->buf.base.points[1].y
    		&& path->buf.base.points[0].y == path->buf.base.points[4].y
    		&& path->buf.base.points[2].y == path->buf.base.points[3].y
    		&& path->buf.base.points[1].x == path->buf.base.points[2].x
    		&& path->buf.base.points[0].x == path->buf.base.points[4].x
    		&& path->buf.base.points[0].x == path->buf.base.points[3].x) {
		cairo_surface_pattern_t* sp = (cairo_surface_pattern_t*)source;
		cairo_rectangle_int_t extents;
    	_cairo_surface_get_extents (sp->surface, &extents);
		double x, y;
	    cairo_matrix_t p2u = sp->base.matrix;
	    status = cairo_matrix_invert (&p2u);
		x = 0;
		y = 0;
		cairo_matrix_transform_point(&p2u, &x, &y);
		x1 = _cairo_fixed_to_double(path->buf.base.points[3].x);
		y1 = _cairo_fixed_to_double(path->buf.base.points[3].y);
		if (_round(x, 1) == _round(x1, 1)
				&& _round(y, 1) == _round(y1, 1)) {
			x = extents.width;
			y = extents.height;
			cairo_matrix_transform_point(&p2u, &x, &y);
			x2 = _cairo_fixed_to_double(path->buf.base.points[1].x);
			y2 = _cairo_fixed_to_double(path->buf.base.points[1].y);
			if (_round(x, 1) == _round(x2, 1)
					&& _round(y, 1) == _round(y2, 1)) {
				_cairo_svg_surface_emit_surface(surface, sp->surface, surface->xml_node,
						x1, y1, x2 - x1, y2 - y1);
				drawpath = FALSE;
			}
		}
    }
    if (drawpath) {
        if ((surface->document->styles & STYLE_B) == 0) {
        	_cairo_output_stream_printf(surface->document->css, ".sb {stroke:none;}\n");
        	surface->document->styles |= STYLE_B;
        }
        _cairo_output_stream_printf (surface->xml_node, "<path class=\"sb ");
        status = _cairo_svg_surface_emit_fill_style (surface->xml_node, surface, op, source, fill_rule, NULL);
        if (unlikely (status))
    	return status;
		_cairo_output_stream_printf (surface->xml_node, "\" ");
		_cairo_svg_surface_emit_path (surface->xml_node, path, NULL, PATH_PRECISION);
		_cairo_output_stream_printf (surface->xml_node, "/>\n");
    }

    return CAIRO_STATUS_SUCCESS;
}

static cairo_bool_t
_cairo_svg_surface_get_extents (void		        *abstract_surface,
				cairo_rectangle_int_t   *rectangle)
{
    cairo_svg_surface_t *surface = abstract_surface;

    rectangle->x = 0;
    rectangle->y = 0;

    /* XXX: The conversion to integers here is pretty bogus, (not to
     * mention the arbitrary limitation of width to a short(!). We
     * may need to come up with a better interface for get_size.
     */
    rectangle->width  = ceil (surface->width);
    rectangle->height = ceil (surface->height);

    return TRUE;
}

static cairo_status_t
_cairo_svg_surface_pattern_class(cairo_svg_surface_t *surface,
			cairo_output_stream_t *output,
			cairo_operator_t	      op,
	       const cairo_pattern_t	     *source)
{
	cairo_status_t status;
	cairo_output_stream_t *css = _cairo_svg_surface_start_style ();

	_cairo_svg_surface_emit_operator_for_style (css, surface, op);
	status = _cairo_svg_surface_emit_pattern (surface, source, css, FALSE, NULL);
	if (unlikely (status))
	return status;

	cairo_svg_style *style = _cairo_svg_surface_end_style(surface->document, css);
	_cairo_output_stream_printf (output, "s%d", style->id);
	return CAIRO_STATUS_SUCCESS;
}

static cairo_status_t
_cairo_svg_surface_emit_paint (cairo_output_stream_t *output,
			       cairo_svg_surface_t   *surface,
			       cairo_operator_t	      op,
			       const cairo_pattern_t	     *source,
			       const cairo_pattern_t	     *mask_source,
			       const cairo_matrix_t *parent_matrix,
			       const char	     *extra_attributes)
{
    cairo_status_t status;

    if (source->type == CAIRO_PATTERN_TYPE_SURFACE &&
	source->extend == CAIRO_EXTEND_NONE)
	return _cairo_svg_surface_emit_composite_pattern (output,
							  surface,
							  op,
							  (cairo_surface_pattern_t *) source,
							  invalid_pattern_id,
							  parent_matrix,
							  extra_attributes);

    if ((surface->document->styles & STYLE_B) == 0) {
    	_cairo_output_stream_printf(surface->document->css, ".sb {stroke:none;}\n");
    	surface->document->styles |= STYLE_B;
    }
	_cairo_output_stream_printf (output,
				 "<rect x=\"0\" y=\"0\" "
				 "width=\"%f\" height=\"%f\" "
				 "class=\"",
				 _round(surface->width, PATH_PRECISION), _round(surface->height, PATH_PRECISION));
	status = _cairo_svg_surface_pattern_class(surface, output, op, source);
	if (unlikely (status))
	return status;
	_cairo_output_stream_printf (output, " sb\"");
	if (extra_attributes)
	_cairo_output_stream_printf (output, " %s", extra_attributes);
	_cairo_output_stream_printf (output, "/>\n");

    return CAIRO_STATUS_SUCCESS;
}

static cairo_int_status_t
_cairo_svg_surface_paint (void		    *abstract_surface,
			  cairo_operator_t   op,
			  const cairo_pattern_t   *source,
			  cairo_clip_t		  *clip)
{
    cairo_status_t status;
    cairo_svg_surface_t *surface = abstract_surface;

    _cairo_svg_end_text(surface);

    /* Emulation of clear and source operators, when no clipping region
     * is defined. We just delete existing content of surface root node,
     * and exit early if operator is clear.
     */
    if ((op == CAIRO_OPERATOR_CLEAR || op == CAIRO_OPERATOR_SOURCE) &&
	clip == NULL)
    {
	switch (surface->paginated_mode) {
	case CAIRO_PAGINATED_MODE_FALLBACK:
	    ASSERT_NOT_REACHED;
	case CAIRO_PAGINATED_MODE_ANALYZE:
	    return CAIRO_STATUS_SUCCESS;

	case CAIRO_PAGINATED_MODE_RENDER:
		 _cairo_svg_end_text(surface);

	    status = _cairo_output_stream_destroy (surface->xml_node);
	    if (unlikely (status)) {
		surface->xml_node = NULL;
		return status;
	    }

	    surface->xml_node = _cairo_memory_stream_create ();
	    if (_cairo_output_stream_get_status (surface->xml_node)) {
		status = _cairo_output_stream_destroy (surface->xml_node);
		surface->xml_node = NULL;
		return status;
	    }

	    if (op == CAIRO_OPERATOR_CLEAR) {
		if (surface->content == CAIRO_CONTENT_COLOR) {
		    if ((surface->document->styles & STYLE_A) == 0) {
		    	_cairo_output_stream_printf(surface->document->css, ".sa {opacity:1;stroke:none;fill:rgb(0,0,0);}\n");
		    	surface->document->styles |= STYLE_A;
		    }
		    _cairo_output_stream_printf (surface->xml_node,
						 "<rect "
						 "width=\"%f\" height=\"%f\" "
						 "class=\"sa\"/>\n",
						 _round(surface->width, PATH_PRECISION), _round(surface->height, PATH_PRECISION));
		}
		return CAIRO_STATUS_SUCCESS;
	    }
	    break;
	}
    } else {
	if (surface->paginated_mode == CAIRO_PAGINATED_MODE_ANALYZE)
	    return _cairo_svg_surface_analyze_operation (surface, op, source);

	assert (_cairo_svg_surface_operation_supported (surface, op, source));
    }

    status = _cairo_surface_clipper_set_clip (&surface->clipper, clip);
    if (unlikely (status))
	return status;

    return _cairo_svg_surface_emit_paint (surface->xml_node,
					  surface, op, source, 0, NULL, NULL);
}

static cairo_int_status_t
_cairo_svg_surface_mask (void		    *abstract_surface,
			 cairo_operator_t     op,
			 const cairo_pattern_t	    *source,
			 const cairo_pattern_t	    *mask,
			 cairo_clip_t		    *clip)
{
    cairo_status_t status;
    cairo_svg_surface_t *surface = abstract_surface;
    cairo_svg_document_t *document = surface->document;
    cairo_output_stream_t *mask_stream;
    char buffer[64];
    cairo_bool_t discard_filter = FALSE;
    unsigned int mask_id;

    if (surface->paginated_mode == CAIRO_PAGINATED_MODE_ANALYZE) {
	cairo_status_t source_status, mask_status;

	source_status = _cairo_svg_surface_analyze_operation (surface, op, source);
	if (_cairo_status_is_error (source_status))
	    return source_status;

	if (mask->has_component_alpha) {
	    mask_status = CAIRO_INT_STATUS_UNSUPPORTED;
	} else {
	    mask_status = _cairo_svg_surface_analyze_operation (surface, op, mask);
	    if (_cairo_status_is_error (mask_status))
		return mask_status;
	}

	return _cairo_analysis_surface_merge_status (source_status,
						     mask_status);
    }

    assert (_cairo_svg_surface_operation_supported (surface, op, source));
    assert (_cairo_svg_surface_operation_supported (surface, CAIRO_OPERATOR_OVER, mask));

    _cairo_svg_end_text(surface);
    status = _cairo_surface_clipper_set_clip (&surface->clipper, clip);
    if (unlikely (status))
	return status;

    if (mask->type == CAIRO_PATTERN_TYPE_SURFACE) {
	const cairo_surface_pattern_t *surface_pattern = (const cairo_surface_pattern_t*) mask;
	cairo_content_t content = cairo_surface_get_content (surface_pattern->surface);
	if (content == CAIRO_CONTENT_ALPHA)
	    discard_filter = TRUE;
    }

    if (!discard_filter)
	_cairo_svg_surface_emit_alpha_filter (document);

   if (mask->type != CAIRO_PATTERN_TYPE_SOLID) {
		/* _cairo_svg_surface_emit_paint() will output a pattern definition to
		 * document->xml_node_defs so we need to write the mask element to
		 * a temporary stream and then copy that to xml_node_defs. */
		mask_stream = _cairo_memory_stream_create ();
		if (_cairo_output_stream_get_status (mask_stream))
		return _cairo_output_stream_destroy (mask_stream);

		mask_id = _cairo_svg_document_allocate_mask_id (document);

		_cairo_output_stream_printf (mask_stream,
					 "<mask id=\"mask%d\">\n"
					 "%s",
					 mask_id,
					 discard_filter ? "" : "  <g filter=\"url(#alpha)\">\n");
		status = _cairo_svg_surface_emit_paint (mask_stream, surface, CAIRO_OPERATOR_OVER, mask, source, &mask->matrix, NULL);
		if (unlikely (status)) {
		cairo_status_t ignore = _cairo_output_stream_destroy (mask_stream);
		return status;
		(void) ignore;
		}

		_cairo_output_stream_printf (mask_stream,
					 "%s"
					 "</mask>\n",
					 discard_filter ? "" : "  </g>\n");
		_cairo_memory_stream_copy (mask_stream, document->xml_node_defs);

		status = _cairo_output_stream_destroy (mask_stream);
		if (unlikely (status))
		return status;

		snprintf (buffer, sizeof buffer, "mask=\"url(#mask%d)\"",
			  mask_id);
   }
   else {
	   cairo_solid_pattern_t *pattern = (cairo_solid_pattern_t *) mask;
	   snprintf (buffer, sizeof buffer, "opacity=\"%f\"",
					 _round(pattern->color.alpha, COLOR_PRECISION));
   }
    status = _cairo_svg_surface_emit_paint (surface->xml_node, surface, op, source, 0, &source->matrix, buffer);
    if (unlikely (status))
	return status;

    return CAIRO_STATUS_SUCCESS;
}

static cairo_int_status_t
_cairo_svg_surface_stroke (void			*abstract_dst,
			   cairo_operator_t      op,
			   const cairo_pattern_t *source,
			   cairo_path_fixed_t	*path,
			   const cairo_stroke_style_t *stroke_style,
			   const cairo_matrix_t	*ctm,
			   const cairo_matrix_t	*ctm_inverse,
			   double		 tolerance,
			   cairo_antialias_t	 antialias,
			   cairo_clip_t		*clip)
{
    cairo_svg_surface_t *surface = abstract_dst;
    cairo_status_t status;

    if (surface->paginated_mode == CAIRO_PAGINATED_MODE_ANALYZE)
	return _cairo_svg_surface_analyze_operation (surface, op, source);

    assert (_cairo_svg_surface_operation_supported (surface, op, source));

    _cairo_svg_end_text(surface);
    status = _cairo_surface_clipper_set_clip (&surface->clipper, clip);
    if (unlikely (status))
	return status;

    if ((surface->document->styles & STYLE_C) == 0) {
    	_cairo_output_stream_printf(surface->document->css, ".sc {fill:none;}\n");
    	surface->document->styles |= STYLE_C;
    }
    _cairo_output_stream_printf (surface->xml_node, "<path class=\"sc ");
    status = _cairo_svg_surface_emit_stroke_style (surface->xml_node, surface, op,
						   source, stroke_style, ctm_inverse);
    if (unlikely (status))
	return status;

    _cairo_output_stream_printf (surface->xml_node, "\" ");

    _cairo_svg_surface_emit_path (surface->xml_node, path, ctm_inverse, PATH_PRECISION);

    _cairo_svg_surface_emit_transform (surface->xml_node, " transform", ctm, NULL);
    _cairo_output_stream_printf (surface->xml_node, "/>\n");

    return CAIRO_STATUS_SUCCESS;
}

static cairo_int_status_t
_cairo_svg_surface_show_glyphs (void			*abstract_surface,
				cairo_operator_t	 op,
				const cairo_pattern_t	*pattern,
				cairo_glyph_t		*glyphs,
				int			 num_glyphs,
				cairo_scaled_font_t	*scaled_font,
				cairo_clip_t		*clip,
				int			*remaining_glyphs)
{
    cairo_svg_surface_t *surface = abstract_surface;
    cairo_svg_document_t *document = surface->document;
    cairo_path_fixed_t path;
    cairo_status_t status;
    cairo_scaled_font_subsets_glyph_t subset_glyph;
    int i;

    if (surface->paginated_mode == CAIRO_PAGINATED_MODE_ANALYZE)
	return _cairo_svg_surface_analyze_operation (surface, op, pattern);

    assert (_cairo_svg_surface_operation_supported (surface, op, pattern));

    if (num_glyphs <= 0)
	return CAIRO_STATUS_SUCCESS;

    _cairo_svg_end_text(surface);
    status = _cairo_surface_clipper_set_clip (&surface->clipper, clip);
    if (unlikely (status))
	return status;

    /* FIXME it's probably possible to apply a pattern of a gradient to
     * a group of symbols, but I don't know how yet. Gradients or patterns
     * are translated by x and y properties of use element. */
    if (pattern->type != CAIRO_PATTERN_TYPE_SOLID)
	goto FALLBACK;

   _cairo_output_stream_printf (surface->xml_node, "<g class=\"");
	status = _cairo_svg_surface_pattern_class(surface, surface->xml_node, op, pattern);
	if (unlikely (status))
	return status;
	   _cairo_output_stream_printf (surface->xml_node, "\">\n");

    for (i = 0; i < num_glyphs; i++) {
	status = _cairo_scaled_font_subsets_map_glyph (document->font_glyphs_subsets,
						       scaled_font, glyphs[i].index,
						       NULL, 0,
                                                       &subset_glyph);
	if (status == CAIRO_INT_STATUS_UNSUPPORTED) {
	    _cairo_output_stream_printf (surface->xml_node, "</g>\n");

	    glyphs += i;
	    num_glyphs -= i;
	    goto FALLBACK;
	}

	if (unlikely (status))
	    return status;

	_cairo_output_stream_printf (surface->xml_node,
				     "  <use xlink:href=\"#glyph%d-%d\" "
				     "x=\"%f\" y=\"%f\"/>\n",
				     subset_glyph.font_id,
                                     subset_glyph.subset_glyph_index,
                                     _round(glyphs[i].x, PATH_PRECISION), _round(glyphs[i].y, PATH_PRECISION));
    }

    _cairo_output_stream_printf (surface->xml_node, "</g>\n");

    return CAIRO_STATUS_SUCCESS;

FALLBACK:
    _cairo_path_fixed_init (&path);

    status = _cairo_scaled_font_glyph_path (scaled_font,
					    (cairo_glyph_t *) glyphs,
					    num_glyphs, &path);

    if (unlikely (status)) {
	_cairo_path_fixed_fini (&path);
	return status;
    }

    status = _cairo_svg_surface_fill (abstract_surface, op, pattern,
				      &path, CAIRO_FILL_RULE_WINDING,
				      0.0, CAIRO_ANTIALIAS_SUBPIXEL,
				      clip);

    _cairo_path_fixed_fini (&path);

    return status;
}


static cairo_bool_t
_cairo_svg_surface_has_show_text_glyphs	(void	*abstract_surface)
{
    return TRUE;
}

static cairo_int_status_t
_cairo_svg_surface_emit_font_style (cairo_output_stream_t	*output,
				    cairo_svg_surface_t		*surface,
				    cairo_operator_t		op,
				    const cairo_pattern_t	*source,
				    const cairo_scaled_font_t	*scaled_font,
				    const cairo_scaled_font_subsets_glyph_t *subset_glyph)
{
    double font_size;
    cairo_status_t status;

    font_size = scaled_font->font_matrix.xx;
    if (font_size == 0) {
    	font_size = -scaled_font->font_matrix.xy;
    }
    _cairo_output_stream_printf (output,
				 "font:%fpx font%d;", _round(font_size, PATH_PRECISION), subset_glyph->font_id);

    status = _cairo_svg_surface_emit_pattern (surface, source,
					      output, FALSE, NULL);
    if (unlikely (status))
	return status;
    _cairo_svg_surface_emit_operator_for_style (output, surface, op);

    return CAIRO_STATUS_SUCCESS;
}

static cairo_bool_t
_cairo_svg_surface_equal_style(const void *key_a, const void *key_b) {
	cairo_svg_style *op_a = (cairo_svg_style*)key_a;
	cairo_svg_style *op_b = (cairo_svg_style*)key_b;
	return op_a->length == op_b->length &&
			memcmp(op_a->style, op_b->style, op_a->length) == 0;
}

static cairo_int_status_t
_cairo_svg_surface_show_text_glyphs (void			*abstract_surface,
				     cairo_operator_t		 op,
				     const cairo_pattern_t	*source,
				     const char			*utf8,
				     int			 utf8_len,
				     cairo_glyph_t		*glyphs,
				     int			 num_glyphs,
				     const cairo_text_cluster_t *clusters,
				     int			 num_clusters,
				     cairo_text_cluster_flags_t	 cluster_flags,
				     cairo_scaled_font_t	*scaled_font,
				     cairo_clip_t		*clip)
{
    cairo_svg_surface_t *surface = abstract_surface;
    cairo_output_stream_t *output = surface->xml_node;
    cairo_scaled_font_subsets_t	*subsets = surface->document->font_text_subsets;
    cairo_status_t status;
    int i, j, k, l;
    double font_size, x, y;
    cairo_matrix_t matrix;
    cairo_scaled_font_subsets_glyph_t subset_glyph;

    if (utf8_len == 0) {
    	return _cairo_svg_surface_show_glyphs(abstract_surface, op, source, glyphs, num_glyphs, scaled_font, clip, NULL);
    }

     if (surface->paginated_mode == CAIRO_PAGINATED_MODE_ANALYZE)
	return _cairo_svg_surface_analyze_operation (surface, op, source);

    assert (_cairo_svg_surface_operation_supported (surface, op, source));

    if (num_glyphs <= 0)
	return CAIRO_STATUS_SUCCESS;

    status = _cairo_surface_clipper_set_clip (&surface->clipper, clip);
    if (unlikely (status))
	return status;

    j = k = 0;
    for (i = 0; i < num_clusters; ++i) {
 		if (clusters[i].num_glyphs > 0)
    	status = _cairo_scaled_font_subsets_map_glyph (subsets,
							       scaled_font, glyphs[k].index,
							       &utf8[j], clusters[i].num_bytes, &subset_glyph);
		if (unlikely (status))
		    return status;
		j += clusters[i].num_bytes;
		k += clusters[i].num_glyphs;
    }

    font_size = scaled_font->font_matrix.xx;
 	if (font_size == 0) {
 		font_size = -scaled_font->font_matrix.xy;
 	}

 	i = l = 0;

 	cairo_output_stream_t *css = _cairo_svg_surface_start_style ();
	_cairo_svg_surface_emit_font_style(css, surface, op, source, scaled_font, &subset_glyph);
	cairo_svg_style *style = _cairo_svg_surface_end_style(surface->document, css);

    cairo_bool_t newtext = (surface->document->prev_style == NULL) ||
    		!_cairo_svg_surface_equal_style(surface->document->prev_style, style);
    if (newtext) {
    	_cairo_svg_end_text(surface);
		_cairo_output_stream_printf (output,
					 "<text class=\"s%d\"", style->id);
		surface->document->text_x = _cairo_memory_stream_create ();
		surface->document->text_y = _cairo_memory_stream_create ();
		surface->document->text = _cairo_memory_stream_create ();
		surface->document->prev_style = style;

		matrix = scaled_font->scale;
		cairo_matrix_scale(&matrix, 1 / font_size, 1 / font_size);
		_cairo_svg_surface_emit_transform(output, " transform", &matrix, NULL);
		cairo_matrix_invert(&matrix);
   }
    else {
 		matrix = scaled_font->scale;
 		cairo_matrix_scale(&matrix, 1 / font_size, 1 / font_size);
 		cairo_matrix_invert(&matrix);
    }
    j = i;
    for (i = l; i < utf8_len;) {
    	if (j < num_glyphs) {
			x = glyphs[j].x;
			y = glyphs[j].y;
			cairo_matrix_transform_point(&matrix, &x, &y);
			if (i > l || !newtext) {
				_cairo_output_stream_printf (surface->document->text_x, " ");
				_cairo_output_stream_printf (surface->document->text_y, " ");
			}
			_cairo_output_stream_printf (surface->document->text_x, "%f", _round(x, PATH_PRECISION));
			_cairo_output_stream_printf (surface->document->text_y, "%f", _round(y, PATH_PRECISION));

		}
    	++j;
    	if (utf8[i] == 0x20 && surface->document->prev_char == 0x20) {
    		surface->document->preserve_space = TRUE;
    	}
    	surface->document->prev_char = utf8[i];
    	_escape_html(utf8, &i, surface->document->text);
    }
    if (j > num_glyphs)
    	_cairo_svg_end_text(surface);

    // for XML space and Webkit performance.
    if (_cairo_memory_stream_length(surface->document->text)  >= 300) {
    	_cairo_svg_end_text(surface);
    }

    return _cairo_output_stream_get_status (output);
}

static void
_cairo_svg_surface_get_font_options (void                  *abstract_surface,
				     cairo_font_options_t  *options)
{
    _cairo_font_options_init_default (options);

    cairo_font_options_set_hint_style (options, CAIRO_HINT_STYLE_NONE);
    cairo_font_options_set_hint_metrics (options, CAIRO_HINT_METRICS_OFF);
    cairo_font_options_set_antialias (options, CAIRO_ANTIALIAS_GRAY);
}

static const cairo_surface_backend_t cairo_svg_surface_backend = {
	CAIRO_SURFACE_TYPE_SVG,
	NULL, /* create_similar: handled by wrapper */
	_cairo_svg_surface_finish,
	NULL, /* acquire_source_image */
	NULL, /* release_source_image */
	NULL, /* acquire_dest_image */
	NULL, /* release_dest_image */
	NULL, /* clone_similar */
	NULL, /* _cairo_svg_surface_composite, */
	NULL, /* _cairo_svg_surface_fill_rectangles, */
	NULL, /* _cairo_svg_surface_composite_trapezoids,*/
	NULL, /* create_span_renderer */
	NULL, /* check_span_renderer */
	_cairo_svg_surface_copy_page,
	_cairo_svg_surface_show_page,
	_cairo_svg_surface_get_extents,
	NULL, /* _cairo_svg_surface_old_show_glyphs, */
	_cairo_svg_surface_get_font_options,
	NULL, /* flush */
	NULL, /* mark dirty rectangle */
	NULL, /* scaled font fini */
	NULL, /* scaled glyph fini */
	_cairo_svg_surface_paint,
	_cairo_svg_surface_mask,
	_cairo_svg_surface_stroke,
	_cairo_svg_surface_fill,
	_cairo_svg_surface_show_glyphs,
	NULL, /* snapshot */
	NULL, /* is_similar */
	_cairo_svg_surface_fill_stroke,
	NULL, /* create_solid_pattern_surface */
	NULL, /* can_repaint_solid_pattern_surface */
	_cairo_svg_surface_has_show_text_glyphs,
	_cairo_svg_surface_show_text_glyphs,
};

static cairo_status_t
_cairo_svg_document_create (cairo_output_stream_t	 *output_stream,
			    double			  width,
			    double			  height,
			    cairo_svg_version_t		  version,
			    cairo_svg_document_t	**document_out,
			    cairo_svg_fontfile *fontfile)
{
   cairo_svg_document_t *document;
    cairo_status_t status, status_ignored;

    if (output_stream->status)
	return output_stream->status;

    document = malloc (sizeof (cairo_svg_document_t));
    if (unlikely (document == NULL))
	return _cairo_error (CAIRO_STATUS_NO_MEMORY);

    if (fontfile) {
    	document->font_text_subsets = fontfile->font_subsets;
    }
    else {
		/* The use of defs for font glyphs imposes no per-subset limit. */
		document->font_text_subsets = _cairo_scaled_font_subsets_create_simple ();
		if (unlikely (document->font_text_subsets == NULL)) {
		status = _cairo_error (CAIRO_STATUS_NO_MEMORY);
		goto CLEANUP_DOCUMENT;
		}
    }
	document->font_glyphs_subsets = _cairo_scaled_font_subsets_create_scaled ();
	if (unlikely (document->font_glyphs_subsets == NULL)) {
	status = _cairo_error (CAIRO_STATUS_NO_MEMORY);
	goto CLEANUP_DOCUMENT;
	}

    document->output_stream = output_stream;
    document->refcount = 1;
    document->owner = NULL;
    document->finished = FALSE;
    document->width = width;
    document->height = height;
    document->fontfile = fontfile;
    document->style_id = 0;
    document->css = _cairo_memory_stream_create ();
    status = _cairo_output_stream_get_status (document->css);
    if (unlikely (status))
	goto CLEANUP_NODE_DEFS;

    document->write_image_func = NULL;
    document->write_image_closure = NULL;

    document->prev_style = NULL;
    document->prev_char = 0x20;
    document->preserve_space = FALSE;
    document->styles = 0;
    document->style_to_id = _cairo_hash_table_create(_cairo_svg_surface_equal_style);

    document->linear_pattern_id = 0;
    document->radial_pattern_id = 0;
    document->pattern_id = 0;
    document->filter_id = 0;
    document->clip_id = 0;
    document->mask_id = 0;

    document->xml_node_defs = _cairo_memory_stream_create ();
    status = _cairo_output_stream_get_status (document->xml_node_defs);
    if (unlikely (status))
	goto CLEANUP_NODE_DEFS;

    document->tail = _cairo_memory_stream_create ();
    status = _cairo_output_stream_get_status (document->tail);
    if (unlikely (status))
	goto CLEANUP_NODE_DEFS;

    document->alpha_filter = FALSE;

    document->svg_version = version;

    *document_out = document;
    return CAIRO_STATUS_SUCCESS;

  CLEANUP_NODE_DEFS:
    status_ignored = _cairo_output_stream_destroy (document->xml_node_defs);
    status_ignored = _cairo_output_stream_destroy (document->tail);
    status_ignored = _cairo_output_stream_destroy (document->css);
    if (!fontfile) {
    	_cairo_scaled_font_subsets_destroy (document->font_text_subsets);
    }
    _cairo_scaled_font_subsets_destroy (document->font_glyphs_subsets);
  CLEANUP_DOCUMENT:
    free (document);
    return status;
}

static cairo_svg_document_t *
_cairo_svg_document_reference (cairo_svg_document_t *document)
{
    document->refcount++;

    return document;
}

static unsigned int
_cairo_svg_document_allocate_mask_id (cairo_svg_document_t *document)
{
    return document->mask_id++;
}

static void
_cairo_svg_surface_style_pluck (void *_entry, void *dict)
{
	cairo_svg_style *entry = (cairo_svg_style*)_entry;
	_cairo_hash_table_remove (dict, &entry->base);
	free(entry->style);
    free(entry);
}

static void
_cairo_svg_surface_style_css (void *_entry, void *_out)
{
	cairo_svg_style *entry = (cairo_svg_style*)_entry;
	cairo_output_stream_t *out = (cairo_output_stream_t*)_out;
	_cairo_output_stream_printf(out, ".s%d{", entry->id);
	_cairo_output_stream_write(out, entry->style, entry->length);
	_cairo_output_stream_printf(out, "}\n");
}

static cairo_status_t
_cairo_svg_document_destroy (cairo_svg_document_t *document)
{
    cairo_status_t status;

    document->refcount--;
    if (document->refcount > 0)
      return CAIRO_STATUS_SUCCESS;

   _cairo_hash_table_foreach (document->style_to_id,
    				_cairo_svg_surface_style_pluck,
    				document->style_to_id);
    _cairo_hash_table_destroy(document->style_to_id);
    status = _cairo_svg_document_finish (document);

    free (document);

    return status;
}

static cairo_status_t
_cairo_svg_document_finish (cairo_svg_document_t *document)
{
    cairo_status_t status, status2;
    cairo_output_stream_t *output = document->output_stream;
    cairo_svg_page_t *page;
    unsigned int i;
    cairo_svg_font_output_t fontout;

    if (document->finished)
	return CAIRO_STATUS_SUCCESS;

    /*
     * Should we add DOCTYPE?
     *
     * Google says no.
     *
     * http://tech.groups.yahoo.com/group/svg-developers/message/48562:
     *   There's a bunch of issues, but just to pick a few:
     *   - they'll give false positives.
     *   - they'll give false negatives.
     *   - they're namespace-unaware.
     *   - they don't wildcard.
     *   So when they say OK they really haven't checked anything, when
     *   they say NOT OK they might be on crack, and like all
     *   namespace-unaware things they're a dead branch of the XML tree.
     *
     * http://jwatt.org/svg/authoring/:
     *   Unfortunately the SVG DTDs are a source of so many issues that the
     *   SVG WG has decided not to write one for the upcoming SVG 1.2
     *   standard. In fact SVG WG members are even telling people not to use
     *   a DOCTYPE declaration in SVG 1.0 and 1.1 documents.
     */

    _cairo_output_stream_printf (output,
				 "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
				 "<svg xmlns=\"http://www.w3.org/2000/svg\" "
				 "xmlns:xlink=\"http://www.w3.org/1999/xlink\" "
				 "width=\"100%%\" height=\"100%%\" "
				 "viewBox=\"0 0 %f %f\" version=\"%s\">\n",
				 _round(document->width, 1), _round(document->height, 1),
				 _cairo_svg_internal_version_strings [document->svg_version]);

    fontout.font_subsets = document->font_text_subsets;
    fontout.fontfile = document->fontfile;
    fontout.glyphs = FALSE;
    fontout.scaled = FALSE;
    fontout.prev_id = -1;

    fontout.xml_node_glyphs = _cairo_memory_stream_create ();
    status = _cairo_output_stream_get_status (fontout.xml_node_glyphs);
    if (unlikely (status))
	goto CLEANUP_NODE_GLYPHS;

    status = _cairo_svg_document_emit_font_subsets (&fontout);

	_cairo_output_stream_printf (output, "<style type=\"text/css\">\n");
	if (_cairo_memory_stream_length (document->css) > 0) {
		_cairo_memory_stream_copy (document->css, output);
	}
    _cairo_hash_table_foreach (document->style_to_id,
    				_cairo_svg_surface_style_css,
    				output);
	_cairo_output_stream_printf (output, "</style>\n");

    if (fontout.fontfile) {
    	if (_cairo_memory_stream_length (fontout.xml_node_glyphs) > 0) {
    		_cairo_output_stream_printf (output, "<style type=\"text/css\">\n");
    		_cairo_memory_stream_copy (fontout.xml_node_glyphs, output);
    		_cairo_output_stream_printf (output, "</style>\n");
    	}
		if (_cairo_memory_stream_length (document->xml_node_defs) > 0) {
		_cairo_output_stream_printf (output, "<defs>\n");
		_cairo_memory_stream_copy (document->xml_node_defs, output);
		_cairo_output_stream_printf (output, "</defs>\n");
		}
    }
    else {
		if (_cairo_memory_stream_length (fontout.xml_node_glyphs) > 0 ||
		_cairo_memory_stream_length (document->xml_node_defs) > 0) {
		_cairo_output_stream_printf (output, "<defs>\n");
		if (_cairo_memory_stream_length (fontout.xml_node_glyphs) > 0) {
			_cairo_output_stream_printf (output, "<g>\n");
			_cairo_memory_stream_copy (fontout.xml_node_glyphs, output);
			_cairo_output_stream_printf (output, "</g>\n");
		}
		_cairo_memory_stream_copy (document->xml_node_defs, output);
		_cairo_output_stream_printf (output, "</defs>\n");
		}
    }

    status2 = _cairo_output_stream_destroy (fontout.xml_node_glyphs);
    if (status == CAIRO_STATUS_SUCCESS)
	status = status2;

    fontout.font_subsets = document->font_glyphs_subsets;
    fontout.fontfile = NULL;
    fontout.glyphs = TRUE;
    fontout.scaled = TRUE;
    fontout.prev_id = -1;

    fontout.xml_node_glyphs = _cairo_memory_stream_create ();
    status = _cairo_output_stream_get_status (fontout.xml_node_glyphs);
    if (unlikely (status))
	goto CLEANUP_NODE_GLYPHS;

    status = _cairo_svg_document_emit_font_subsets (&fontout);

	if (_cairo_memory_stream_length (fontout.xml_node_glyphs) > 0) {
		_cairo_output_stream_printf (output, "<defs>\n");
		_cairo_output_stream_printf (output, "<g>\n");
		_cairo_memory_stream_copy (fontout.xml_node_glyphs, output);
		_cairo_output_stream_printf (output, "</g>\n");
		_cairo_output_stream_printf (output, "</defs>\n");
	}

    if (document->owner != NULL) {
	cairo_svg_surface_t *surface;

	surface = (cairo_svg_surface_t *) _cairo_paginated_surface_get_target (document->owner);
	if (surface->xml_node != NULL &&
		_cairo_memory_stream_length (surface->xml_node) > 0) {
	    if (unlikely (_cairo_svg_surface_store_page (surface) == NULL)) {
		if (status == CAIRO_STATUS_SUCCESS)
		    status = _cairo_error (CAIRO_STATUS_NO_MEMORY);
	    }
	}

	if (surface->page_set.num_elements > 1 &&
	    _cairo_svg_version_has_page_set_support (document->svg_version)) {
	    _cairo_output_stream_printf (output, "<pageSet>\n");
	    for (i = 0; i < surface->page_set.num_elements; i++) {
		page = _cairo_array_index (&surface->page_set, i);
		_cairo_output_stream_printf (output, "<page>\n");
		_cairo_output_stream_printf (output,
					     "<g id=\"surface%d\">\n",
					     page->surface_id);
		_cairo_memory_stream_copy (page->xml_node, output);
		_cairo_output_stream_printf (output, "</g>\n</page>\n");
	    }
	    _cairo_output_stream_printf (output, "</pageSet>\n");
	} else if (surface->page_set.num_elements > 0) {
	    page = _cairo_array_index (&surface->page_set, surface->page_set.num_elements - 1);
	    _cairo_output_stream_printf (output,
					 "<g id=\"surface%d\">\n",
					 page->surface_id);
	    _cairo_memory_stream_copy (page->xml_node, output);
	    _cairo_output_stream_printf (output, "</g>\n");
	}
    }
	if (_cairo_memory_stream_length (document->tail) > 0) {
	_cairo_memory_stream_copy (document->tail, output);
	}
    _cairo_output_stream_printf (output, "</svg>\n");

    status2 = _cairo_output_stream_destroy (fontout.xml_node_glyphs);
    if (status == CAIRO_STATUS_SUCCESS)
	status = status2;

    status2 = _cairo_output_stream_destroy (document->xml_node_defs);
    if (status == CAIRO_STATUS_SUCCESS)
	status = status2;

    status2 = _cairo_output_stream_destroy (document->css);
    if (status == CAIRO_STATUS_SUCCESS)
	status = status2;

    status2 = _cairo_output_stream_destroy (output);
    if (status == CAIRO_STATUS_SUCCESS)
	status = status2;

    document->finished = TRUE;

    return status;

    CLEANUP_NODE_GLYPHS:
    status2 = _cairo_output_stream_destroy (fontout.xml_node_glyphs);
    return status;
}

static void
_cairo_svg_surface_set_paginated_mode (void			*abstract_surface,
				       cairo_paginated_mode_t	 paginated_mode)
{
    cairo_svg_surface_t *surface = abstract_surface;

    surface->paginated_mode = paginated_mode;
}

static cairo_bool_t
_cairo_svg_surface_supports_fine_grained_fallbacks (void	*abstract_surface)
{
    cairo_svg_surface_t *surface = abstract_surface;
    cairo_int_status_t status = CAIRO_INT_STATUS_UNSUPPORTED;

    if (surface->document->svg_version >= CAIRO_SVG_VERSION_1_2) {
	status =  _cairo_svg_surface_analyze_operator (surface,
						       CAIRO_OPERATOR_SOURCE);
    }

    return status == CAIRO_STATUS_SUCCESS;
}

static const cairo_paginated_surface_backend_t cairo_svg_surface_paginated_backend = {
    NULL /*_cairo_svg_surface_start_page*/,
    _cairo_svg_surface_set_paginated_mode,
    NULL, /* _cairo_svg_surface_set_bounding_box */
    NULL, /* _cairo_svg_surface_set_fallback_images_required */
    _cairo_svg_surface_supports_fine_grained_fallbacks,

};
