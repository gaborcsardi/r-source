/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-2022 The R Core Team
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

#include "grid.h"

/* We are assuming here that the R code has checked that 
 * info is a "RGlyphInfo"
 */
static void renderGlyphs(SEXP runs, SEXP glyphInfo, 
                         SEXP x, SEXP y, Rboolean draw) 
{
    int i, n, nruns = LENGTH(runs);
    double *gx, *gy;
    double vpWidthCM, vpHeightCM;
    double rotationAngle;
    const void *vmax;
    LViewportContext vpc;
    LTransform transform;
    SEXP currentvp, currentgp;
    /* Get the current device 
     */
    pGEDevDesc dd = getDevice();
    currentvp = gridStateElement(dd, GSS_VP);
    currentgp = gridStateElement(dd, GSS_GPAR);
    /* This copy is used to store/cache resolved gp$fill to avoid
     * stupid amounts of pattern resolving (resolving a resolved
     * pattern is basically a no-op), WITHOUT touching current gp
     * in 'grid' state. */
    currentgp = PROTECT(duplicate(currentgp));
    /* Do not need fill, so set gp$fill to "black" to avoid any
     * pattern resolution. */
    SET_VECTOR_ELT(currentgp, GP_FILL, mkString("black"));
    getViewportTransform(currentvp, dd, 
			 &vpWidthCM, &vpHeightCM, 
			 transform, &rotationAngle);
    /* getViewportContext(currentvp, &vpc); */
    fillViewportContextFromViewport(currentvp, &vpc);
    if (draw) {
	GEMode(1, dd);
    }
    int *glyphs = INTEGER(R_GE_glyphID(glyphInfo));
    n = LENGTH(R_GE_glyphID(glyphInfo));
        
    vmax = vmaxget();
    gx = (double *) R_alloc(n, sizeof(double));
    gy = (double *) R_alloc(n, sizeof(double));
    for (i=0; i<n; i++) {
        gx[i] = toDeviceX(REAL(x)[i], GE_INCHES, dd);
        gy[i] = toDeviceY(REAL(y)[i], GE_INCHES, dd);
    }

    int offset = 0;
    for (i=0; i<nruns; i++) {
        int runLength = INTEGER(runs)[i];
        char family[201];
        strncpy(family, CHAR(STRING_ELT(R_GE_glyphFamily(glyphInfo), offset)), 
                200);
        double weight = REAL(R_GE_glyphWeight(glyphInfo))[offset];
        int style = INTEGER(R_GE_glyphStyle(glyphInfo))[offset];
        char file[501];
        strncpy(file, CHAR(STRING_ELT(R_GE_glyphFile(glyphInfo), offset)), 500);
        int index = INTEGER(R_GE_glyphIndex(glyphInfo))[offset];
        double size = REAL(R_GE_glyphSize(glyphInfo))[offset];
        GEGlyph(runLength, 
                glyphs + offset, 
                gx + offset, 
                gy + offset, 
                family, weight, style, file, index, size, 
                dd);
        offset = offset + runLength;
    }
    vmaxset(vmax);
    if (draw) {
        GEMode(0, dd);
    }
    UNPROTECT(1);
}

SEXP L_glyph(SEXP runs, SEXP glyphInfo, SEXP x, SEXP y) {
    renderGlyphs(runs, glyphInfo, x, y, TRUE);
    return R_NilValue;    
}
