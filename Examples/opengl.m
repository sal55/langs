=importdll opengl32 =
    clang proc "glAccum"                           (u32,r32)
    clang proc "glAlphaFunc"                       (u32,r32)
    clang func "glAreTexturesResident"             (i32,ref u32,ref byte)byte
    clang proc "glArrayElement"                    (i32)
    clang proc "glBegin"                           (u32)
    clang proc "glBindTexture"                     (u32,u32)
    clang proc "glBitmap"                          (i32,i32,r32,r32,r32,r32,ref byte)
    clang proc "glBlendFunc"                       (u32,u32)
    clang proc "glCallList"                        (u32)
    clang proc "glCallLists"                       (i32,u32,ref void)
    clang proc "glClear"                           (u32)
    clang proc "glClearAccum"                      (r32,r32,r32,r32)
    clang proc "glClearColor"                      (r32,r32,r32,r32)
    clang proc "glClearDepth"                      (r64)
    clang proc "glClearIndex"                      (r32)
    clang proc "glClearStencil"                    (i32)
    clang proc "glClipPlane"                       (u32,ref r64)
    clang proc "glColor3b"                         (i8,i8,i8)
    clang proc "glColor3bv"                        (ref i8)
    clang proc "glColor3d"                         (r64,r64,r64)
    clang proc "glColor3dv"                        (ref r64)
    clang proc "glColor3f"                         (r32,r32,r32)
    clang proc "glColor3fv"                        (ref r32)
    clang proc "glColor3i"                         (i32,i32,i32)
    clang proc "glColor3iv"                        (ref i32)
    clang proc "glColor3s"                         (i16,i16,i16)
    clang proc "glColor3sv"                        (ref i16)
    clang proc "glColor3ub"                        (byte,byte,byte)
    clang proc "glColor3ubv"                       (ref byte)
    clang proc "glColor3ui"                        (u32,u32,u32)
    clang proc "glColor3uiv"                       (ref u32)
    clang proc "glColor3us"                        (u16,u16,u16)
    clang proc "glColor3usv"                       (ref u16)
    clang proc "glColor4b"                         (i8,i8,i8,i8)
    clang proc "glColor4bv"                        (ref i8)
    clang proc "glColor4d"                         (r64,r64,r64,r64)
    clang proc "glColor4dv"                        (ref r64)
    clang proc "glColor4f"                         (r32,r32,r32,r32)
    clang proc "glColor4fv"                        (ref r32)
    clang proc "glColor4i"                         (i32,i32,i32,i32)
    clang proc "glColor4iv"                        (ref i32)
    clang proc "glColor4s"                         (i16,i16,i16,i16)
    clang proc "glColor4sv"                        (ref i16)
    clang proc "glColor4ub"                        (byte,byte,byte,byte)
    clang proc "glColor4ubv"                       (ref byte)
    clang proc "glColor4ui"                        (u32,u32,u32,u32)
    clang proc "glColor4uiv"                       (ref u32)
    clang proc "glColor4us"                        (u16,u16,u16,u16)
    clang proc "glColor4usv"                       (ref u16)
    clang proc "glColorMask"                       (byte,byte,byte,byte)
    clang proc "glColorMaterial"                   (u32,u32)
    clang proc "glColorPointer"                    (i32,u32,i32,ref void)
    clang proc "glCopyPixels"                      (i32,i32,i32,i32,u32)
    clang proc "glCopyTexImage1D"                  (u32,i32,u32,i32,i32,i32,i32)
    clang proc "glCopyTexImage2D"                  (u32,i32,u32,i32,i32,i32,i32,i32)
    clang proc "glCopyTexSubImage1D"               (u32,i32,i32,i32,i32,i32)
    clang proc "glCopyTexSubImage2D"               (u32,i32,i32,i32,i32,i32,i32,i32)
    clang proc "glCullFace"                        (u32)
    clang proc "glDeleteLists"                     (u32,i32)
    clang proc "glDeleteTextures"                  (i32,ref u32)
    clang proc "glDepthFunc"                       (u32)
    clang proc "glDepthMask"                       (byte)
    clang proc "glDepthRange"                      (r64,r64)
    clang proc "glDisable"                         (u32)
    clang proc "glDisableClientState"              (u32)
    clang proc "glDrawArrays"                      (u32,i32,i32)
    clang proc "glDrawBuffer"                      (u32)
    clang proc "glDrawElements"                    (u32,i32,u32,ref void)
    clang proc "glDrawPixels"                      (i32,i32,u32,u32,ref void)
    clang proc "glEdgeFlag"                        (byte)
    clang proc "glEdgeFlagPointer"                 (i32,ref void)
    clang proc "glEdgeFlagv"                       (ref byte)
    clang proc "glEnable"                          (u32)
    clang proc "glEnableClientState"               (u32)
    clang proc "glEnd"                             ()
    clang proc "glEndList"                         ()
    clang proc "glEvalCoord1d"                     (r64)
    clang proc "glEvalCoord1dv"                    (ref r64)
    clang proc "glEvalCoord1f"                     (r32)
    clang proc "glEvalCoord1fv"                    (ref r32)
    clang proc "glEvalCoord2d"                     (r64,r64)
    clang proc "glEvalCoord2dv"                    (ref r64)
    clang proc "glEvalCoord2f"                     (r32,r32)
    clang proc "glEvalCoord2fv"                    (ref r32)
    clang proc "glEvalMesh1"                       (u32,i32,i32)
    clang proc "glEvalMesh2"                       (u32,i32,i32,i32,i32)
    clang proc "glEvalPoint1"                      (i32)
    clang proc "glEvalPoint2"                      (i32,i32)
    clang proc "glFeedbackBuffer"                  (i32,u32,ref r32)
    clang proc "glFinish"                          ()
    clang proc "glFlush"                           ()
    clang proc "glFogf"                            (u32,r32)
    clang proc "glFogfv"                           (u32,ref r32)
    clang proc "glFogi"                            (u32,i32)
    clang proc "glFogiv"                           (u32,ref i32)
    clang proc "glFrontFace"                       (u32)
    clang proc "glFrustum"                         (r64,r64,r64,r64,r64,r64)
    clang func "glGenLists"                        (i32)u32
    clang proc "glGenTextures"                     (i32,ref u32)
    clang proc "glGetBooleanv"                     (u32,ref byte)
    clang proc "glGetClipPlane"                    (u32,ref r64)
    clang proc "glGetDoublev"                      (u32,ref r64)
    clang func "glGetError"                        ()u32
    clang proc "glGetFloatv"                       (u32,ref r32)
    clang proc "glGetIntegerv"                     (u32,ref i32)
    clang proc "glGetLightfv"                      (u32,u32,ref r32)
    clang proc "glGetLightiv"                      (u32,u32,ref i32)
    clang proc "glGetMapdv"                        (u32,u32,ref r64)
    clang proc "glGetMapfv"                        (u32,u32,ref r32)
    clang proc "glGetMapiv"                        (u32,u32,ref i32)
    clang proc "glGetMaterialfv"                   (u32,u32,ref r32)
    clang proc "glGetMaterialiv"                   (u32,u32,ref i32)
    clang proc "glGetPixelMapfv"                   (u32,ref r32)
    clang proc "glGetPixelMapuiv"                  (u32,ref u32)
    clang proc "glGetPixelMapusv"                  (u32,ref u16)
    clang proc "glGetPointerv"                     (u32,ref ref void)
    clang proc "glGetPolygonStipple"               (ref byte)
    clang func "glGetString"                       (u32)ref byte
    clang proc "glGetTexEnvfv"                     (u32,u32,ref r32)
    clang proc "glGetTexEnviv"                     (u32,u32,ref i32)
    clang proc "glGetTexGendv"                     (u32,u32,ref r64)
    clang proc "glGetTexGenfv"                     (u32,u32,ref r32)
    clang proc "glGetTexGeniv"                     (u32,u32,ref i32)
    clang proc "glGetTexImage"                     (u32,i32,u32,u32,ref void)
    clang proc "glGetTexLevelParameterfv"          (u32,i32,u32,ref r32)
    clang proc "glGetTexLevelParameteriv"          (u32,i32,u32,ref i32)
    clang proc "glGetTexParameterfv"               (u32,u32,ref r32)
    clang proc "glGetTexParameteriv"               (u32,u32,ref i32)
    clang proc "glHint"                            (u32,u32)
    clang proc "glIndexMask"                       (u32)
    clang proc "glIndexPointer"                    (u32,i32,ref void)
    clang proc "glIndexd"                          (r64)
    clang proc "glIndexdv"                         (ref r64)
    clang proc "glIndexf"                          (r32)
    clang proc "glIndexfv"                         (ref r32)
    clang proc "glIndexi"                          (i32)
    clang proc "glIndexiv"                         (ref i32)
    clang proc "glIndexs"                          (i16)
    clang proc "glIndexsv"                         (ref i16)
    clang proc "glIndexub"                         (byte)
    clang proc "glIndexubv"                        (ref byte)
    clang proc "glInitNames"                       ()
    clang proc "glInterleavedArrays"               (u32,i32,ref void)
    clang func "glIsEnabled"                       (u32)byte
    clang func "glIsList"                          (u32)byte
    clang func "glIsTexture"                       (u32)byte
    clang proc "glLightModelf"                     (u32,r32)
    clang proc "glLightModelfv"                    (u32,ref r32)
    clang proc "glLightModeli"                     (u32,i32)
    clang proc "glLightModeliv"                    (u32,ref i32)
    clang proc "glLightf"                          (u32,u32,r32)
    clang proc "glLightfv"                         (u32,u32,ref r32)
    clang proc "glLighti"                          (u32,u32,i32)
    clang proc "glLightiv"                         (u32,u32,ref i32)
    clang proc "glLineStipple"                     (i32,u16)
    clang proc "glLineWidth"                       (r32)
    clang proc "glListBase"                        (u32)
    clang proc "glLoadIdentity"                    ()
    clang proc "glLoadMatrixd"                     (ref r64)
    clang proc "glLoadMatrixf"                     (ref r32)
    clang proc "glLoadName"                        (u32)
    clang proc "glLogicOp"                         (u32)
    clang proc "glMap1d"                           (u32,r64,r64,i32,i32,ref r64)
    clang proc "glMap1f"                           (u32,r32,r32,i32,i32,ref r32)
    clang proc "glMap2d"                           (u32,r64,r64,i32,i32,r64,r64,i32,i32,ref r64)
    clang proc "glMap2f"                           (u32,r32,r32,i32,i32,r32,r32,i32,i32,ref r32)
    clang proc "glMapGrid1d"                       (i32,r64,r64)
    clang proc "glMapGrid1f"                       (i32,r32,r32)
    clang proc "glMapGrid2d"                       (i32,r64,r64,i32,r64,r64)
    clang proc "glMapGrid2f"                       (i32,r32,r32,i32,r32,r32)
    clang proc "glMaterialf"                       (u32,u32,r32)
    clang proc "glMaterialfv"                      (u32,u32,ref r32)
    clang proc "glMateriali"                       (u32,u32,i32)
    clang proc "glMaterialiv"                      (u32,u32,ref i32)
    clang proc "glMatrixMode"                      (u32)
    clang proc "glMultMatrixd"                     (ref r64)
    clang proc "glMultMatrixf"                     (ref r32)
    clang proc "glNewList"                         (u32,u32)
    clang proc "glNormal3b"                        (i8,i8,i8)
    clang proc "glNormal3bv"                       (ref i8)
    clang proc "glNormal3d"                        (r64,r64,r64)
    clang proc "glNormal3dv"                       (ref r64)
    clang proc "glNormal3f"                        (r32,r32,r32)
    clang proc "glNormal3fv"                       (ref r32)
    clang proc "glNormal3i"                        (i32,i32,i32)
    clang proc "glNormal3iv"                       (ref i32)
    clang proc "glNormal3s"                        (i16,i16,i16)
    clang proc "glNormal3sv"                       (ref i16)
    clang proc "glNormalPointer"                   (u32,i32,ref void)
    clang proc "glOrtho"                           (r64,r64,r64,r64,r64,r64)
    clang proc "glPassThrough"                     (r32)
    clang proc "glPixelMapfv"                      (u32,i32,ref r32)
    clang proc "glPixelMapuiv"                     (u32,i32,ref u32)
    clang proc "glPixelMapusv"                     (u32,i32,ref u16)
    clang proc "glPixelStoref"                     (u32,r32)
    clang proc "glPixelStorei"                     (u32,i32)
    clang proc "glPixelTransferf"                  (u32,r32)
    clang proc "glPixelTransferi"                  (u32,i32)
    clang proc "glPixelZoom"                       (r32,r32)
    clang proc "glPointSize"                       (r32)
    clang proc "glPolygonMode"                     (u32,u32)
    clang proc "glPolygonOffset"                   (r32,r32)
    clang proc "glPolygonStipple"                  (ref byte)
    clang proc "glPopAttrib"                       ()
    clang proc "glPopClientAttrib"                 ()
    clang proc "glPopMatrix"                       ()
    clang proc "glPopName"                         ()
    clang proc "glPrioritizeTextures"              (i32,ref u32,ref r32)
    clang proc "glPushAttrib"                      (u32)
    clang proc "glPushClientAttrib"                (u32)
    clang proc "glPushMatrix"                      ()
    clang proc "glPushName"                        (u32)
    clang proc "glRasterPos2d"                     (r64,r64)
    clang proc "glRasterPos2dv"                    (ref r64)
    clang proc "glRasterPos2f"                     (r32,r32)
    clang proc "glRasterPos2fv"                    (ref r32)
    clang proc "glRasterPos2i"                     (i32,i32)
    clang proc "glRasterPos2iv"                    (ref i32)
    clang proc "glRasterPos2s"                     (i16,i16)
    clang proc "glRasterPos2sv"                    (ref i16)
    clang proc "glRasterPos3d"                     (r64,r64,r64)
    clang proc "glRasterPos3dv"                    (ref r64)
    clang proc "glRasterPos3f"                     (r32,r32,r32)
    clang proc "glRasterPos3fv"                    (ref r32)
    clang proc "glRasterPos3i"                     (i32,i32,i32)
    clang proc "glRasterPos3iv"                    (ref i32)
    clang proc "glRasterPos3s"                     (i16,i16,i16)
    clang proc "glRasterPos3sv"                    (ref i16)
    clang proc "glRasterPos4d"                     (r64,r64,r64,r64)
    clang proc "glRasterPos4dv"                    (ref r64)
    clang proc "glRasterPos4f"                     (r32,r32,r32,r32)
    clang proc "glRasterPos4fv"                    (ref r32)
    clang proc "glRasterPos4i"                     (i32,i32,i32,i32)
    clang proc "glRasterPos4iv"                    (ref i32)
    clang proc "glRasterPos4s"                     (i16,i16,i16,i16)
    clang proc "glRasterPos4sv"                    (ref i16)
    clang proc "glReadBuffer"                      (u32)
    clang proc "glReadPixels"                      (i32,i32,i32,i32,u32,u32,ref void)
    clang proc "glRectd"                           (r64,r64,r64,r64)
    clang proc "glRectdv"                          (ref r64,ref r64)
    clang proc "glRectf"                           (r32,r32,r32,r32)
    clang proc "glRectfv"                          (ref r32,ref r32)
    clang proc "glRecti"                           (i32,i32,i32,i32)
    clang proc "glRectiv"                          (ref i32,ref i32)
    clang proc "glRects"                           (i16,i16,i16,i16)
    clang proc "glRectsv"                          (ref i16,ref i16)
    clang func "glRenderMode"                      (u32)i32
    clang proc "glRotated"                         (r64,r64,r64,r64)
    clang proc "glRotatef"                         (r32,r32,r32,r32)
    clang proc "glScaled"                          (r64,r64,r64)
    clang proc "glScalef"                          (r32,r32,r32)
    clang proc "glScissor"                         (i32,i32,i32,i32)
    clang proc "glSelectBuffer"                    (i32,ref u32)
    clang proc "glShadeModel"                      (u32)
    clang proc "glStencilFunc"                     (u32,i32,u32)
    clang proc "glStencilMask"                     (u32)
    clang proc "glStencilOp"                       (u32,u32,u32)
    clang proc "glTexCoord1d"                      (r64)
    clang proc "glTexCoord1dv"                     (ref r64)
    clang proc "glTexCoord1f"                      (r32)
    clang proc "glTexCoord1fv"                     (ref r32)
    clang proc "glTexCoord1i"                      (i32)
    clang proc "glTexCoord1iv"                     (ref i32)
    clang proc "glTexCoord1s"                      (i16)
    clang proc "glTexCoord1sv"                     (ref i16)
    clang proc "glTexCoord2d"                      (r64,r64)
    clang proc "glTexCoord2dv"                     (ref r64)
    clang proc "glTexCoord2f"                      (r32,r32)
    clang proc "glTexCoord2fv"                     (ref r32)
    clang proc "glTexCoord2i"                      (i32,i32)
    clang proc "glTexCoord2iv"                     (ref i32)
    clang proc "glTexCoord2s"                      (i16,i16)
    clang proc "glTexCoord2sv"                     (ref i16)
    clang proc "glTexCoord3d"                      (r64,r64,r64)
    clang proc "glTexCoord3dv"                     (ref r64)
    clang proc "glTexCoord3f"                      (r32,r32,r32)
    clang proc "glTexCoord3fv"                     (ref r32)
    clang proc "glTexCoord3i"                      (i32,i32,i32)
    clang proc "glTexCoord3iv"                     (ref i32)
    clang proc "glTexCoord3s"                      (i16,i16,i16)
    clang proc "glTexCoord3sv"                     (ref i16)
    clang proc "glTexCoord4d"                      (r64,r64,r64,r64)
    clang proc "glTexCoord4dv"                     (ref r64)
    clang proc "glTexCoord4f"                      (r32,r32,r32,r32)
    clang proc "glTexCoord4fv"                     (ref r32)
    clang proc "glTexCoord4i"                      (i32,i32,i32,i32)
    clang proc "glTexCoord4iv"                     (ref i32)
    clang proc "glTexCoord4s"                      (i16,i16,i16,i16)
    clang proc "glTexCoord4sv"                     (ref i16)
    clang proc "glTexCoordPointer"                 (i32,u32,i32,ref void)
    clang proc "glTexEnvf"                         (u32,u32,r32)
    clang proc "glTexEnvfv"                        (u32,u32,ref r32)
    clang proc "glTexEnvi"                         (u32,u32,i32)
    clang proc "glTexEnviv"                        (u32,u32,ref i32)
    clang proc "glTexGend"                         (u32,u32,r64)
    clang proc "glTexGendv"                        (u32,u32,ref r64)
    clang proc "glTexGenf"                         (u32,u32,r32)
    clang proc "glTexGenfv"                        (u32,u32,ref r32)
    clang proc "glTexGeni"                         (u32,u32,i32)
    clang proc "glTexGeniv"                        (u32,u32,ref i32)
    clang proc "glTexImage1D"                      (u32,i32,i32,i32,i32,u32,u32,ref void)
    clang proc "glTexImage2D"                      (u32,i32,i32,i32,i32,i32,u32,u32,ref void)
    clang proc "glTexParameterf"                   (u32,u32,r32)
    clang proc "glTexParameterfv"                  (u32,u32,ref r32)
    clang proc "glTexParameteri"                   (u32,u32,i32)
    clang proc "glTexParameteriv"                  (u32,u32,ref i32)
    clang proc "glTexSubImage1D"                   (u32,i32,i32,i32,u32,u32,ref void)
    clang proc "glTexSubImage2D"                   (u32,i32,i32,i32,i32,i32,u32,u32,ref void)
    clang proc "glTranslated"                      (r64,r64,r64)
    clang proc "glTranslatef"                      (r32,r32,r32)
    clang proc "glVertex2d"                        (r64,r64)
    clang proc "glVertex2dv"                       (ref r64)
    clang proc "glVertex2f"                        (r32,r32)
    clang proc "glVertex2fv"                       (ref r32)
    clang proc "glVertex2i"                        (i32,i32)
    clang proc "glVertex2iv"                       (ref i32)
    clang proc "glVertex2s"                        (i16,i16)
    clang proc "glVertex2sv"                       (ref i16)
    clang proc "glVertex3d"                        (r64,r64,r64)
    clang proc "glVertex3dv"                       (ref r64)
    clang proc "glVertex3f"                        (r32,r32,r32)
    clang proc "glVertex3fv"                       (ref r32)
    clang proc "glVertex3i"                        (i32,i32,i32)
    clang proc "glVertex3iv"                       (ref i32)
    clang proc "glVertex3s"                        (i16,i16,i16)
    clang proc "glVertex3sv"                       (ref i16)
    clang proc "glVertex4d"                        (r64,r64,r64,r64)
    clang proc "glVertex4dv"                       (ref r64)
    clang proc "glVertex4f"                        (r32,r32,r32,r32)
    clang proc "glVertex4fv"                       (ref r32)
    clang proc "glVertex4i"                        (i32,i32,i32,i32)
    clang proc "glVertex4iv"                       (ref i32)
    clang proc "glVertex4s"                        (i16,i16,i16,i16)
    clang proc "glVertex4sv"                       (ref i16)
    clang proc "glVertexPointer"                   (i32,u32,i32,ref void)
    clang proc "glViewport"                        (i32,i32,i32,i32)
    clang func "gluErrorString"                    (u32)ref byte
    clang func "gluErrorUnicodeStringEXT"          (u32)ref u16
    clang func "gluGetString"                      (u32)ref byte
    clang proc "gluOrtho2D"                        (r64,r64,r64,r64)
    clang proc "gluPerspective"                    (r64,r64,r64,r64)
    clang proc "gluPickMatrix"                     (r64,r64,r64,r64,ref i32)
    clang proc "gluLookAt"                         (r64,r64,r64,r64,r64,r64,r64,r64,r64)
    clang func "gluProject"                        (r64,r64,r64,ref r64,ref r64,ref i32,ref r64,ref r64,ref r64)i32
    clang func "gluUnProject"                      (r64,r64,r64,ref r64,ref r64,ref i32,ref r64,ref r64,ref r64)i32
    clang func "gluScaleImage"                     (u32,i32,i32,u32,ref void,i32,i32,u32,ref void)i32
    clang func "gluBuild1DMipmaps"                 (u32,i32,i32,u32,u32,ref void)i32
    clang func "gluBuild2DMipmaps"                 (u32,i32,i32,i32,u32,u32,ref void)i32
    record GLUnurbs =
        int dummy    !empty record
    end

    record GLUquadric =
        int dummy    !empty record
    end

    record GLUtesselator =
        int dummy    !empty record
    end

    clang func "gluNewQuadric"                     ()ref GLUquadric
    clang proc "gluDeleteQuadric"                  (ref GLUquadric)
    clang proc "gluQuadricNormals"                 (ref GLUquadric,u32)
    clang proc "gluQuadricTexture"                 (ref GLUquadric,byte)
    clang proc "gluQuadricOrientation"             (ref GLUquadric,u32)
    clang proc "gluQuadricDrawStyle"               (ref GLUquadric,u32)
    clang proc "gluCylinder"                       (ref GLUquadric,r64,r64,r64,i32,i32)
    clang proc "gluDisk"                           (ref GLUquadric,r64,r64,i32,i32)
    clang proc "gluPartialDisk"                    (ref GLUquadric,r64,r64,i32,i32,r64,r64)
    clang proc "gluSphere"                         (ref GLUquadric,r64,i32,i32)
    clang proc "gluQuadricCallback"                (ref GLUquadric,u32,ref clang proc())
    clang func "gluNewTess"                        ()ref GLUtesselator
    clang proc "gluDeleteTess"                     (ref GLUtesselator)
    clang proc "gluTessBeginPolygon"               (ref GLUtesselator,ref void)
    clang proc "gluTessBeginContour"               (ref GLUtesselator)
    clang proc "gluTessVertex"                     (ref GLUtesselator,ref r64,ref void)
    clang proc "gluTessEndContour"                 (ref GLUtesselator)
    clang proc "gluTessEndPolygon"                 (ref GLUtesselator)
    clang proc "gluTessProperty"                   (ref GLUtesselator,u32,r64)
    clang proc "gluTessNormal"                     (ref GLUtesselator,r64,r64,r64)
    clang proc "gluTessCallback"                   (ref GLUtesselator,u32,ref clang proc())
    clang proc "gluGetTessProperty"                (ref GLUtesselator,u32,ref r64)
    clang func "gluNewNurbsRenderer"               ()ref GLUnurbs
    clang proc "gluDeleteNurbsRenderer"            (ref GLUnurbs)
    clang proc "gluBeginSurface"                   (ref GLUnurbs)
    clang proc "gluBeginCurve"                     (ref GLUnurbs)
    clang proc "gluEndCurve"                       (ref GLUnurbs)
    clang proc "gluEndSurface"                     (ref GLUnurbs)
    clang proc "gluBeginTrim"                      (ref GLUnurbs)
    clang proc "gluEndTrim"                        (ref GLUnurbs)
    clang proc "gluPwlCurve"                       (ref GLUnurbs,i32,ref r32,i32,u32)
    clang proc "gluNurbsCurve"                     (ref GLUnurbs,i32,ref r32,i32,ref r32,i32,u32)
    clang proc "gluNurbsSurface"                   (ref GLUnurbs,i32,ref r32,i32,ref r32,i32,i32,ref r32,i32,i32,u32)
    clang proc "gluLoadSamplingMatrices"           (ref GLUnurbs,ref r32,ref r32,ref i32)
    clang proc "gluNurbsProperty"                  (ref GLUnurbs,u32,r32)
    clang proc "gluGetNurbsProperty"               (ref GLUnurbs,u32,ref r32)
    clang proc "gluNurbsCallback"                  (ref GLUnurbs,u32,ref clang proc())
    clang proc "gluBeginPolygon"                   (ref GLUtesselator)
    clang proc "gluNextContour"                    (ref GLUtesselator,u32)
    clang proc "gluEndPolygon"                     (ref GLUtesselator)
    clang proc "exit"                              (i32)
    clang proc "glutInit"                          (ref i32,ref ref byte)
    clang proc "__glutInitWithExit"                (ref i32,ref ref byte,ref clang proc(i32))
    clang proc "glutInit_ATEXIT_HACK"              (ref i32,ref ref byte)
    clang proc "glutInitDisplayMode"               (u32)
    clang proc "glutInitDisplayString"             (ref byte)
    clang proc "glutInitWindowPosition"            (i32,i32)
    clang proc "glutInitWindowSize"                (i32,i32)
    clang proc "glutMainLoop"                      ()
    clang func "glutCreateWindow"                  (ref char)i32
    clang func "__glutCreateWindowWithExit"        (ref byte,ref clang proc(i32))i32
    clang func "glutCreateWindow_ATEXIT_HACK"      (ref byte)i32
    clang func "glutCreateSubWindow"               (i32,i32,i32,i32,i32)i32
    clang proc "glutDestroyWindow"                 (i32)
    clang proc "glutPostRedisplay"                 ()
    clang proc "glutPostWindowRedisplay"           (i32)
    clang proc "glutSwapBuffers"                   ()
    clang func "glutGetWindow"                     ()i32
    clang proc "glutSetWindow"                     (i32)
    clang proc "glutSetWindowTitle"                (ref byte)
    clang proc "glutSetIconTitle"                  (ref byte)
    clang proc "glutPositionWindow"                (i32,i32)
    clang proc "glutReshapeWindow"                 (i32,i32)
    clang proc "glutPopWindow"                     ()
    clang proc "glutPushWindow"                    ()
    clang proc "glutIconifyWindow"                 ()
    clang proc "glutShowWindow"                    ()
    clang proc "glutHideWindow"                    ()
    clang proc "glutFullScreen"                    ()
    clang proc "glutSetCursor"                     (i32)
    clang proc "glutWarpPointer"                   (i32,i32)
    clang proc "glutEstablishOverlay"              ()
    clang proc "glutRemoveOverlay"                 ()
    clang proc "glutUseLayer"                      (u32)
    clang proc "glutPostOverlayRedisplay"          ()
    clang proc "glutPostWindowOverlayRedisplay"    (i32)
    clang proc "glutShowOverlay"                   ()
    clang proc "glutHideOverlay"                   ()
    clang func "glutCreateMenu"                    (ref clang proc(i32))i32
    clang func "__glutCreateMenuWithExit"          (ref clang proc(i32),ref clang proc(i32))i32
    clang func "glutCreateMenu_ATEXIT_HACK"        (ref clang proc(i32))i32
    clang proc "glutDestroyMenu"                   (i32)
    clang func "glutGetMenu"                       ()i32
    clang proc "glutSetMenu"                       (i32)
    clang proc "glutAddMenuEntry"                  (ref byte,i32)
    clang proc "glutAddSubMenu"                    (ref byte,i32)
    clang proc "glutChangeToMenuEntry"             (i32,ref byte,i32)
    clang proc "glutChangeToSubMenu"               (i32,ref byte,i32)
    clang proc "glutRemoveMenuItem"                (i32)
    clang proc "glutAttachMenu"                    (i32)
    clang proc "glutDetachMenu"                    (i32)
    clang proc "glutDisplayFunc"                   (ref clang proc())
    clang proc "glutReshapeFunc"                   (ref clang proc(i32,i32))
    clang proc "glutKeyboardFunc"                  (ref clang proc(byte,i32,i32))
    clang proc "glutMouseFunc"                     (ref clang proc(i32,i32,i32,i32))
    clang proc "glutMotionFunc"                    (ref clang proc(i32,i32))
    clang proc "glutPassiveMotionFunc"             (ref clang proc(i32,i32))
    clang proc "glutEntryFunc"                     (ref clang proc(i32))
    clang proc "glutVisibilityFunc"                (ref clang proc(i32))
    clang proc "glutIdleFunc"                      (ref clang proc())
    clang proc "glutTimerFunc"                     (u32,ref clang proc(i32),i32)
    clang proc "glutMenuStateFunc"                 (ref clang proc(i32))
    clang proc "glutSpecialFunc"                   (ref clang proc(i32,i32,i32))
    clang proc "glutSpaceballMotionFunc"           (ref clang proc(i32,i32,i32))
    clang proc "glutSpaceballRotateFunc"           (ref clang proc(i32,i32,i32))
    clang proc "glutSpaceballButtonFunc"           (ref clang proc(i32,i32))
    clang proc "glutButtonBoxFunc"                 (ref clang proc(i32,i32))
    clang proc "glutDialsFunc"                     (ref clang proc(i32,i32))
    clang proc "glutTabletMotionFunc"              (ref clang proc(i32,i32))
    clang proc "glutTabletButtonFunc"              (ref clang proc(i32,i32,i32,i32))
    clang proc "glutMenuStatusFunc"                (ref clang proc(i32,i32,i32))
    clang proc "glutOverlayDisplayFunc"            (ref clang proc())
    clang proc "glutWindowStatusFunc"              (ref clang proc(i32))
    clang proc "glutKeyboardUpFunc"                (ref clang proc(byte,i32,i32))
    clang proc "glutSpecialUpFunc"                 (ref clang proc(i32,i32,i32))
    clang proc "glutJoystickFunc"                  (ref clang proc(u32,i32,i32,i32),i32)
    clang proc "glutSetColor"                      (i32,r32,r32,r32)
    clang func "glutGetColor"                      (i32,i32)r32
    clang proc "glutCopyColormap"                  (i32)
    clang func "glutGet"                           (u32)i32
    clang func "glutDeviceGet"                     (u32)i32
    clang func "glutExtensionSupported"            (ref byte)i32
    clang func "glutGetModifiers"                  ()i32
    clang func "glutLayerGet"                      (u32)i32
    clang proc "glutBitmapCharacter"               (ref void,i32)
    clang func "glutBitmapWidth"                   (ref void,i32)i32
    clang proc "glutStrokeCharacter"               (ref void,i32)
    clang func "glutStrokeWidth"                   (ref void,i32)i32
    clang func "glutBitmapLength"                  (ref void,ref byte)i32
    clang func "glutStrokeLength"                  (ref void,ref byte)i32
    clang proc "glutWireSphere"                    (r64,i32,i32)
    clang proc "glutSolidSphere"                   (r64,i32,i32)
    clang proc "glutWireCone"                      (r64,r64,i32,i32)
    clang proc "glutSolidCone"                     (r64,r64,i32,i32)
    clang proc "glutWireCube"                      (r64)
    clang proc "glutSolidCube"                     (r64)
    clang proc "glutWireTorus"                     (r64,r64,i32,i32)
    clang proc "glutSolidTorus"                    (r64,r64,i32,i32)
    clang proc "glutWireDodecahedron"              ()
    clang proc "glutSolidDodecahedron"             ()
    clang proc "glutWireTeapot"                    (r64)
    clang proc "glutSolidTeapot"                   (r64)
    clang proc "glutWireOctahedron"                ()
    clang proc "glutSolidOctahedron"               ()
    clang proc "glutWireTetrahedron"               ()
    clang proc "glutSolidTetrahedron"              ()
    clang proc "glutWireIcosahedron"               ()
    clang proc "glutSolidIcosahedron"              ()
    clang func "glutVideoResizeGet"                (u32)i32
    clang proc "glutSetupVideoResizing"            ()
    clang proc "glutStopVideoResizing"             ()
    clang proc "glutVideoResize"                   (i32,i32,i32,i32)
    clang proc "glutVideoPan"                      (i32,i32,i32,i32)
    clang proc "glutReportErrors"                  ()
    clang proc "glutIgnoreKeyRepeat"               (i32)
    clang proc "glutSetKeyRepeat"                  (i32)
    clang proc "glutForceJoystickFunc"             ()
    clang proc "glutGameModeString"                (ref byte)
    clang func "glutEnterGameMode"                 ()i32
    clang proc "glutLeaveGameMode"                 ()
    clang func "glutGameModeGet"                   (u32)i32
    const GL_MAP1_TEXTURE_COORD_3            = 0x0D95    ! macro
    const GLU_NURBS_ERROR37                  =  100287    ! macro
    const GL_MAP1_TEXTURE_COORD_4            =  0x0D96    ! macro
    const GLUT_WINDOW_STEREO                 = (121)    ! macro
    const GL_2_BYTES                         = 0x1407    ! macro
    const GL_COLOR_LOGIC_OP                  =  0x0BF2    ! macro
    const GL_UNPACK_LSB_FIRST                =  0x0CF1    ! macro
    const GLUT_DOWN                          =  0    ! macro
    const GLU_INVALID_VALUE                  =  100901    ! macro
    const GL_COLOR_TABLE_RED_SIZE_EXT        =  0x80DA    ! macro
    const GL_BLEND_SRC                       =  0x0BE1    ! macro
    const GLUT_MULTISAMPLE                   =  128    ! macro
    const GL_FASTEST                         =  0x1101    ! macro
    const GL_TEXTURE_COORD_ARRAY_STRIDE_EXT  =  0x808A    ! macro
    const GL_LIGHTING                        =  0x0B50    ! macro
    const GL_VERSION                         =  0x1F02    ! macro
    const GL_PIXEL_MAP_I_TO_A                =  0x0C75    ! macro
    const GLUT_KEY_REPEAT_DEFAULT            =  2    ! macro
    const GL_PIXEL_MAP_I_TO_B                =  0x0C74    ! macro
    const GL_COPY                            =  0x1503    ! macro
    const GL_TEXTURE_COORD_ARRAY_STRIDE      =  0x808A    ! macro
    const GL_BLUE_BITS                       =  0x0D54    ! macro
    const GL_EXTENSIONS                      =  0x1F03    ! macro
    const GL_LOAD                            =  0x0101    ! macro
!    const glutCreateMenu_                     =  glutCreateMenu_ATEXIT_HACK    ! macro
    const GL_NORMAL_ARRAY_STRIDE             =  0x807F    ! macro
    const GL_PIXEL_MAP_I_TO_G                =  0x0C73    ! macro
    const GL_MAP_COLOR                       =  0x0D10    ! macro
    const GL_VERTEX_ARRAY_STRIDE             =  0x807C    ! macro
    const GL_RGB5_A1                         =  0x8057    ! macro
    const GL_VERTEX_ARRAY_TYPE               =  0x807B    ! macro
    const GLUT_JOYSTICK_BUTTON_A             =  1    ! macro
    const GL_PIXEL_MAP_I_TO_I                =  0x0C70    ! macro
    const GL_ALPHA_TEST                      =  0x0BC0    ! macro
    const GLUT_JOYSTICK_BUTTON_B             =  2    ! macro
    const GL_MODELVIEW_MATRIX                =  0x0BA6    ! macro
    const GLUT_CURSOR_NONE                   =  101    ! macro
    const GLUT_JOYSTICK_BUTTON_C             =  4    ! macro
    const GL_FOG_COLOR                       =  0x0B66    ! macro
    const GLUT_HIDDEN                        =  0    ! macro
    const GL_RGB10                           =  0x8052    ! macro
    const GLUT_JOYSTICK_BUTTON_D             =  8    ! macro
    const GL_LINE_STIPPLE_REPEAT             =  0x0B26    ! macro
    const GLUT_CURSOR_BOTTOM_LEFT_CORNER     =  19    ! macro
    const GL_UNPACK_ROW_LENGTH               =  0x0CF2    ! macro
    const GL_ALWAYS                          =  0x0207    ! macro
    const GL_RGB12                           =  0x8053    ! macro
    const GL_OR                              =  0x1507    ! macro
    const GL_SMOOTH                          =  0x1D01    ! macro
    const GL_STENCIL_BUFFER_BIT              =  0x00000400    ! macro
    const GL_LINE_LOOP                       =  0x0002    ! macro
    const GLUT_KEY_PAGE_DOWN                 =  105    ! macro
    const GLU_SAMPLING_METHOD                =  100205    ! macro
    const GL_COLOR_INDEX2_EXT                =  0x80E3    ! macro
    const GL_RGB16                           =  0x8054    ! macro
    const GL_PIXEL_MAP_I_TO_R                =  0x0C72    ! macro
    const GL_MAP1_COLOR_4                    =  0x0D90    ! macro
    const GL_TEXTURE_MIN_FILTER              =  0x2801    ! macro
    const GLU_TESS_MISSING_BEGIN_POLYGON     =  GLU_TESS_ERROR1    ! macro
    const GLUT_INIT_WINDOW_WIDTH             = (502)    ! macro
    const GL_PROJECTION_MATRIX               = 0x0BA7    ! macro
    const GL_C4F_N3F_V3F                     =  0x2A26    ! macro
    const GL_MAX_TEXTURE_STACK_DEPTH         =  0x0D39    ! macro
    const GL_DRAW_BUFFER                     =  0x0C01    ! macro
    const GL_PHONG_WIN                       =  0x80EA    ! macro
    const GL_MAX_VIEWPORT_DIMS               =  0x0D3A    ! macro
    const GL_CURRENT_RASTER_DISTANCE         =  0x0B09    ! macro
    const GL_LUMINANCE16_ALPHA16             =  0x8048    ! macro
    const GLUT_GAME_MODE_POSSIBLE            = (1)    ! macro
    const GL_MODELVIEW_STACK_DEPTH           = 0x0BA3    ! macro
    const GL_VERTEX_ARRAY_TYPE_EXT           =  0x807B    ! macro
    const GL_MAX_ELEMENTS_INDICES_WIN        =  0x80E9    ! macro
    const GL_COLOR_ARRAY_STRIDE              =  0x8083    ! macro
    const GLUT_CURSOR_LEFT_SIDE              =  14    ! macro
    const GL_BACK_RIGHT                      =  0x0403    ! macro
    const GLU_DOMAIN_DISTANCE                =  100217    ! macro
    const GLU_TESS_WINDING_ABS_GEQ_TWO       =  100134    ! macro
    const GLU_AUTO_LOAD_MATRIX               =  100200    ! macro
    const GL_AND_INVERTED                    =  0x1504    ! macro
    const GL_CURRENT_TEXTURE_COORDS          =  0x0B03    ! macro
    const GL_MAP1_GRID_DOMAIN                =  0x0DD0    ! macro
    const GL_EYE_LINEAR                      =  0x2400    ! macro
    const GL_DOUBLE                          =  0x140A    ! macro
    const GL_PACK_ROW_LENGTH                 =  0x0D02    ! macro
    const GL_PIXEL_MAP_I_TO_R_SIZE           =  0x0CB2    ! macro
    const GL_FRONT_FACE                      =  0x0B46    ! macro
    const GL_TEXTURE_BINDING_1D              =  0x8068    ! macro
    const GLU_TESS_MAX_COORD                 =  1.0e150    ! macro
    const GL_T4F_V4F                         =  0x2A28    ! macro
    const GL_POLYGON_OFFSET_UNITS            =  0x2A00    ! macro
    const GLUT_FULLY_COVERED                 =  3    ! macro
    const GL_3_BYTES                         =  0x1408    ! macro
    const GL_TEXTURE_PRIORITY                =  0x8066    ! macro
    const GL_LINEAR                          =  0x2601    ! macro
    const GLUT_KEY_HOME                      =  106    ! macro
    const GLU_TESS_END                       =  100102    ! macro
    const GL_HINT_BIT                        =  0x00008000    ! macro
    const GLUT_WINDOW_RED_SIZE               = (107)    ! macro
    const GLU_CULLING                        = 100201    ! macro
    const GLUT_NUM_TABLET_BUTTONS            = (609)    ! macro
    const GL_SELECTION_BUFFER_SIZE           = 0x0DF4    ! macro
    const GLUT_RIGHT_BUTTON                  =  2    ! macro
    const GLU_TRUE                           =  GL_TRUE    ! macro
    const GL_BGR_EXT                         =  0x80E0    ! macro
    const GLUT_JOYSTICK_AXES                 = (615)    ! macro
    const GL_FOG_DENSITY                     = 0x0B62    ! macro
    const GL_EXT_bgra                        =  1    ! macro
    const GL_PIXEL_MAP_S_TO_S                =  0x0C71    ! macro
    const GL_PROJECTION                      =  0x1701    ! macro
    const GL_MAX_NAME_STACK_DEPTH            =  0x0D37    ! macro
    const GL_TEXTURE_BINDING_2D              =  0x8069    ! macro
    const GL_TEXTURE_1D                      =  0x0DE0    ! macro
    const GLUT_INIT_WINDOW_HEIGHT            = (503)    ! macro
    const GLUT_SCREEN_WIDTH_MM               = (202)    ! macro
!    const GLUT_BITMAP_8_BY_13                = (ref void(3))    ! macro
    const GLUT_BITMAP_8_BY_13                = 3    ! macro
    const GL_VERTEX_ARRAY_SIZE               = 0x807A    ! macro
    const GL_SPHERE_MAP                      =  0x2402    ! macro
    const GLU_TESS_COORD_TOO_LARGE           =  GLU_TESS_ERROR5    ! macro
    const GL_INT                             =  0x1404    ! macro
    const GL_SET                             =  0x150F    ! macro
    const GL_INDEX_OFFSET                    =  0x0D13    ! macro
    const GL_MAP2_GRID_SEGMENTS              =  0x0DD3    ! macro
    const GL_LINE_TOKEN                      =  0x0702    ! macro
    const GLUT_WINDOW_DOUBLEBUFFER           = (115)    ! macro
    const GL_MAX_CLIP_PLANES                 = 0x0D32    ! macro
    const GL_TEXTURE                         =  0x1702    ! macro
    const GL_DRAW_PIXEL_TOKEN                =  0x0705    ! macro
    const GL_STENCIL_INDEX                   =  0x1901    ! macro
    const GL_MAX_PROJECTION_STACK_DEPTH      =  0x0D38    ! macro
    const GLU_OUT_OF_MEMORY                  =  100902    ! macro
    const GL_TEXTURE_GEN_MODE                =  0x2500    ! macro
    const GL_SHADE_MODEL                     =  0x0B54    ! macro
    const GLUT_HAS_SPACEBALL                 = (602)    ! macro
    const GL_TEXTURE_2D                      = 0x0DE1    ! macro
    const GL_PIXEL_MODE_BIT                  =  0x00000020    ! macro
    const GL_LIGHT0                          =  0x4000    ! macro
    const GL_LIGHT1                          =  0x4001    ! macro
    const GL_UNPACK_SWAP_BYTES               =  0x0CF0    ! macro
!    const GLUT_STROKE_MONO_ROMAN             = (ref void(1))    ! macro
    const GLUT_STROKE_MONO_ROMAN             = 1    ! macro
    const GL_STENCIL_PASS_DEPTH_FAIL         = 0x0B95    ! macro
    const GL_LIGHT2                          =  0x4002    ! macro
    const GL_LIGHT3                          =  0x4003    ! macro
    const GL_LIGHT4                          =  0x4004    ! macro
    const GL_COLOR_TABLE_LUMINANCE_SIZE_EXT  =  0x80DE    ! macro
    const GL_LIGHT5                          =  0x4005    ! macro
    const GL_FEEDBACK_BUFFER_POINTER         =  0x0DF0    ! macro
    const GL_LIGHT6                          =  0x4006    ! macro
    const GLUT_NUM_MOUSE_BUTTONS             = (605)    ! macro
    const GL_CULL_FACE_MODE                  = 0x0B45    ! macro
    const GL_SHININESS                       =  0x1601    ! macro
    const GL_LIGHT7                          =  0x4007    ! macro
    const GL_C4UB_V2F                        =  0x2A22    ! macro
    const GLU_TESS_MISSING_BEGIN_CONTOUR     =  GLU_TESS_ERROR2    ! macro
    const GL_BGRA_EXT                        =  0x80E1    ! macro
    const GLUT_WINDOW_RGBA                   = (116)    ! macro
    const GL_ONE_MINUS_DST_COLOR             = 0x0307    ! macro
    const GLU_TESS_BEGIN_DATA                =  100106    ! macro
    const GLU_OUTLINE_POLYGON                =  100240    ! macro
    const GLUT_STENCIL                       =  32    ! macro
    const GL_EDGE_FLAG_ARRAY_EXT             =  0x8079    ! macro
    const GLUT_JOYSTICK_POLL_RATE            = (616)    ! macro
    const GL_NONE                            = 0    ! macro
    const GL_LOGIC_OP                        =  GL_INDEX_LOGIC_OP    ! macro
    const GL_POLYGON_OFFSET_LINE             =  0x2A02    ! macro
    const GL_4_BYTES                         =  0x1409    ! macro
    const GL_SRC_ALPHA_SATURATE              =  0x0308    ! macro
    const GLU_TESS_COMBINE                   =  100105    ! macro
    const GLUT_ACTIVE_ALT                    =  4    ! macro
    const GL_INDEX_ARRAY_TYPE_EXT            =  0x8085    ! macro
    const GL_DECAL                           =  0x2101    ! macro
    const GL_POSITION                        =  0x1203    ! macro
    const GL_INVERT                          =  0x150A    ! macro
    const GL_STENCIL_BITS                    =  0x0D57    ! macro
    const GL_3D_COLOR                        =  0x0602    ! macro
    const GL_ALPHA_SCALE                     =  0x0D1C    ! macro
    const GLU_PARAMETRIC_ERROR               =  100216    ! macro
    const GL_PACK_SWAP_BYTES                 =  0x0D00    ! macro
    const GL_DEPTH_FUNC                      =  0x0B74    ! macro
    const GL_C4UB_V3F                        =  0x2A23    ! macro
    const GL_PIXEL_MAP_I_TO_B_SIZE           =  0x0CB4    ! macro
    const GLUT_INIT_DISPLAY_MODE             = (504)    ! macro
    const GL_COPY_PIXEL_TOKEN                = 0x0706    ! macro
    const GLU_INCOMPATIBLE_GL_VERSION        =  100903    ! macro
    const GL_2D                              =  0x0600    ! macro
    const GL_PROJECTION_STACK_DEPTH          =  0x0BA4    ! macro
    const GL_NO_ERROR                        =  0    ! macro
    const GL_DEPTH_BIAS                      =  0x0D1F    ! macro
    const GLUT_CURSOR_RIGHT_SIDE             =  15    ! macro
    const GL_POLYGON_STIPPLE_BIT             =  0x00000010    ! macro
    const GL_ACCUM_BUFFER_BIT                =  0x00000200    ! macro
    const GL_STACK_OVERFLOW                  =  0x0503    ! macro
    const GL_RGBA_MODE                       =  0x0C31    ! macro
!    const GLUT_BITMAP_9_BY_15                = (ref void(2))    ! macro
    const GL_EDGE_FLAG_ARRAY_STRIDE_EXT      = 0x808C    ! macro
    const GLUT_KEY_LEFT                      =  100    ! macro
    const GL_TEXTURE_BIT                     =  0x00040000    ! macro
    const GL_LINE_RESET_TOKEN                =  0x0707    ! macro
    const GL_FOG_SPECULAR_TEXTURE_WIN        =  0x80EC    ! macro
    const GL_NAND                            =  0x150E    ! macro
    const GLU_SAMPLING_TOLERANCE             =  100203    ! macro
    const GL_DOUBLE_EXT                      =  GL_DOUBLE    ! macro
    const GLU_TESS_EDGE_FLAG_DATA            =  100110    ! macro
!    const gluErrorStringWIN                  = gluErrorString(errCode)    ! macro
    const GL_FRONT_LEFT                      = 0x0400    ! macro
    const GL_TEXTURE_COORD_ARRAY_TYPE        =  0x8089    ! macro
    const GLUT_XLIB_IMPLEMENTATION           =  15    ! macro
    const GL_MAX_CLIENT_ATTRIB_STACK_DEPTH   =  0x0D3B    ! macro
    const GL_PIXEL_MAP_I_TO_G_SIZE           =  0x0CB3    ! macro
    const GLUT_SINGLE                        =  0    ! macro
    const GLUT_SCREEN_HEIGHT                 = (201)    ! macro
    const GL_NOOP                            = 0x1505    ! macro
    const GL_3D                              =  0x0601    ! macro
    const GL_INDEX_ARRAY_POINTER_EXT         =  0x8091    ! macro
    const GL_MATRIX_MODE                     =  0x0BA0    ! macro
    const GL_PIXEL_MAP_G_TO_G_SIZE           =  0x0CB7    ! macro
    const GLU_INTERIOR                       =  100122    ! macro
    const GL_INVALID_ENUM                    =  0x0500    ! macro
    const GLU_TESS_MISSING_END_POLYGON       =  GLU_TESS_ERROR3    ! macro
    const GL_LUMINANCE                       =  0x1909    ! macro
    const GLUT_RED                           =  0    ! macro
    const GL_ALPHA                           =  0x1906    ! macro
    const GL_COEFF                           =  0x0A00    ! macro
    const GL_CURRENT_NORMAL                  =  0x0B02    ! macro
    const GL_CLIENT_ATTRIB_STACK_DEPTH       =  0x0BB1    ! macro
    const GL_MAP1_INDEX                      =  0x0D91    ! macro
    const GLUT_KEY_F10                       =  10    ! macro
    const GLU_TESS_ERROR1                    =  100151    ! macro
    const GLU_V_STEP                         =  100207    ! macro
    const GLU_DISPLAY_MODE                   =  100204    ! macro
    const GL_VIEWPORT                        =  0x0BA2    ! macro
    const GLUT_KEY_F11                       =  11    ! macro
    const GLU_TESS_ERROR2                    =  100152    ! macro
    const GLUT_KEY_F12                       =  12    ! macro
    const GLU_TESS_ERROR3                    =  100153    ! macro
    const GLUT_OVERLAY_POSSIBLE              = (800)    ! macro
    const GL_COLOR_ARRAY_TYPE_EXT            = 0x8082    ! macro
    const GLU_TESS_ERROR4                    =  100154    ! macro
    const GLUT_CURSOR_BOTTOM_SIDE            =  13    ! macro
    const GL_DEPTH                           =  0x1801    ! macro
    const GLU_TESS_ERROR5                    =  100155    ! macro
    const GL_COMPILE                         =  0x1300    ! macro
    const GLU_TESS_ERROR6                    =  100156    ! macro
    const GL_LIGHT_MODEL_LOCAL_VIEWER        =  0x0B51    ! macro
    const GLU_TESS_ERROR7                    =  100157    ! macro
    const GL_COLOR_TABLE_BLUE_SIZE_EXT       =  0x80DC    ! macro
    const GL_ONE_MINUS_SRC_COLOR             =  0x0301    ! macro
    const GLU_TESS_ERROR8                    =  100158    ! macro
    const GL_COLOR_ARRAY_POINTER_EXT         =  0x8090    ! macro
    const GL_INDEX_SHIFT                     =  0x0D12    ! macro
    const GL_ENABLE_BIT                      =  0x00002000    ! macro
    const GLUT_VIDEO_RESIZE_HEIGHT           = (909)    ! macro
    const GL_GREEN_BIAS                      = 0x0D19    ! macro
    const GL_EQUAL                           =  0x0202    ! macro
    const GL_INDEX_MODE                      =  0x0C30    ! macro
    const GL_COLOR_INDEX1_EXT                =  0x80E2    ! macro
    const GLUT_SCREEN_HEIGHT_MM              = (203)    ! macro
    const GL_FRONT                           = 0x0404    ! macro
    const GL_CCW                             =  0x0901    ! macro
    const GL_NEVER                           =  0x0200    ! macro
    const GL_NORMAL_ARRAY_POINTER_EXT        =  0x808F    ! macro
    const GL_EMISSION                        =  0x1600    ! macro
    const GLUT_GAME_MODE_WIDTH               = (2)    ! macro
    const GL_INVALID_VALUE                   = 0x0501    ! macro
    const GL_LIST_BIT                        =  0x00020000    ! macro
    const GL_NOR                             =  0x1508    ! macro
    const GL_ALPHA_BIAS                      =  0x0D1D    ! macro
    const GL_BACK                            =  0x0405    ! macro
    const GL_CLIENT_VERTEX_ARRAY_BIT         =  0x00000002    ! macro
    const GL_TEXTURE_GEN_Q                   =  0x0C63    ! macro
    const GL_VIEWPORT_BIT                    =  0x00000800    ! macro
    const GLUT_RGB                           =  0    ! macro
    const GL_STENCIL_WRITEMASK               =  0x0B98    ! macro
    const GL_TEXTURE_GEN_R                   =  0x0C62    ! macro
    const GL_MAP_STENCIL                     =  0x0D11    ! macro
    const GL_TEXTURE_GEN_S                   =  0x0C60    ! macro
!    const GLUT_STROKE_ROMAN                  = ref void(0)    ! macro
    const GL_BLUE_SCALE                      = 0x0D1A    ! macro
    const GL_TEXTURE_GEN_T                   =  0x0C61    ! macro
    const GLUT_WINDOW_HEIGHT                 = (103)    ! macro
!    const GLUT_BITMAP_HELVETICA_10           = ref void(6)    ! macro
    const GL_MAX_ELEMENTS_VERTICES_WIN       = 0x80E8    ! macro
    const GLUT_VIDEO_RESIZE_X_DELTA          = (902)    ! macro
!    const GLUT_BITMAP_HELVETICA_12           = ref void(7)    ! macro
    const GLUT_WINDOW_STENCIL_SIZE           = (105)    ! macro
    const GL_EDGE_FLAG_ARRAY_POINTER         = 0x8093    ! macro
    const GLU_TESS_WINDING_POSITIVE          =  100132    ! macro
    const GL_FOG_HINT                        =  0x0C54    ! macro
    const GL_POLYGON                         =  0x0009    ! macro
    const GL_STENCIL_PASS_DEPTH_PASS         =  0x0B96    ! macro
    const GLU_VERSION                        =  100800    ! macro
    const GLUT_VISIBLE                       =  1    ! macro
    const GL_TEXTURE_COORD_ARRAY_COUNT_EXT   =  0x808B    ! macro
    const GL_TEXTURE_COORD_ARRAY_SIZE        =  0x8088    ! macro
    const GL_PIXEL_MAP_R_TO_R                =  0x0C76    ! macro
!    const GLUT_BITMAP_HELVETICA_18           = ref void(8)    ! macro
    const GL_DEPTH_COMPONENT                 = 0x1902    ! macro
    const GL_NORMAL_ARRAY_EXT                =  0x8075    ! macro
    const GL_COLOR_BUFFER_BIT                =  0x00004000    ! macro
    const GL_MODULATE                        =  0x2100    ! macro
    const GL_UNSIGNED_SHORT                  =  0x1403    ! macro
    const GL_MAP2_VERTEX_3                   =  0x0DB7    ! macro
    const GLU_NONE                           =  100002    ! macro
    const GL_LESS                            =  0x0201    ! macro
    const GL_MULT                            =  0x0103    ! macro
    const GL_MAP2_VERTEX_4                   =  0x0DB8    ! macro
    const GLUT_GAME_MODE_DISPLAY_CHANGED     = (6)    ! macro
    const GL_AUTO_NORMAL                     = 0x0D80    ! macro
    const GL_CURRENT_RASTER_POSITION_VALID   =  0x0B08    ! macro
    const GL_MODELVIEW                       =  0x1700    ! macro
    const GL_MAP2_INDEX                      =  0x0DB1    ! macro
    const GLU_CW                             =  100120    ! macro
    const GL_SUBPIXEL_BITS                   =  0x0D50    ! macro
    const GLUT_HAS_OVERLAY                   = (802)    ! macro
    const GL_NORMALIZE                       = 0x0BA1    ! macro
    const GL_RED_BITS                        =  0x0D52    ! macro
    const GLUT_GAME_MODE_REFRESH_RATE        = (5)    ! macro
    const GL_EXT_paletted_texture            = 1    ! macro
    const GLUT_KEY_INSERT                    =  108    ! macro
    const GL_EDGE_FLAG_ARRAY_POINTER_EXT     =  0x8093    ! macro
    const GLU_TESS_MISSING_END_CONTOUR       =  GLU_TESS_ERROR4    ! macro
    const GLU_INVALID_ENUM                   =  100900    ! macro
    const GL_PACK_ALIGNMENT                  =  0x0D05    ! macro
    const GLUT_HAS_DIAL_AND_BUTTON_BOX       = (603)    ! macro
    const GL_COLOR_INDEX                     = 0x1900    ! macro
    const GL_DITHER                          =  0x0BD0    ! macro
    const GLUT_KEY_REPEAT_OFF                =  0    ! macro
    const GL_POLYGON_MODE                    =  0x0B40    ! macro
    const GL_MAX_ATTRIB_STACK_DEPTH          =  0x0D35    ! macro
    const GL_COLOR_MATERIAL_FACE             =  0x0B55    ! macro
    const GLUT_WINDOW_ACCUM_RED_SIZE         = (111)    ! macro
    const GL_COLOR_ARRAY_EXT                 = 0x8076    ! macro
    const GL_QUADRATIC_ATTENUATION           =  0x1209    ! macro
    const GLUT_KEY_UP                        =  101    ! macro
    const GLUT_WINDOW_BUFFER_SIZE            = (104)    ! macro
    const GLUT_CURSOR_UP_DOWN                = 10    ! macro
    const GLUT_NUM_SPACEBALL_BUTTONS         = (606)    ! macro
    const GLUT_LUMINANCE                     = 512    ! macro
    const GLUT_ACTIVE_SHIFT                  =  1    ! macro
    const GLUT_GAME_MODE_HEIGHT              = (3)    ! macro
    const GL_LINE_STRIP                      = 0x0003    ! macro
    const GLUT_LEFT                          =  0    ! macro
    const GL_LUMINANCE12                     =  0x8041    ! macro
    const GL_POLYGON_OFFSET_POINT            =  0x2A01    ! macro
    const GLU_TESS_BEGIN                     =  100100    ! macro
    const GL_MAP2_COLOR_4                    =  0x0DB0    ! macro
    const GL_LUMINANCE16                     =  0x8042    ! macro
    const GLUT_VIDEO_RESIZE_Y_DELTA          = (903)    ! macro
    const GLUT_WINDOW_PARENT                 = (117)    ! macro
    const GL_RETURN                          = 0x0102    ! macro
    const GL_LUMINANCE4_ALPHA4               =  0x8043    ! macro
    const GL_PIXEL_MAP_B_TO_B                =  0x0C78    ! macro
    const GL_PIXEL_MAP_I_TO_A_SIZE           =  0x0CB5    ! macro
    const GLUT_CURSOR_CYCLE                  =  5    ! macro
    const GLUT_RGBA                          =  GLUT_RGB    ! macro
    const GL_COLOR_MATERIAL_PARAMETER        =  0x0B56    ! macro
    const GL_T2F_C4F_N3F_V3F                 =  0x2A2C    ! macro
    const GLU_SMOOTH                         =  100000    ! macro
    const GL_OUT_OF_MEMORY                   =  0x0505    ! macro
    const GL_STENCIL_TEST                    =  0x0B90    ! macro
    const GL_DST_ALPHA                       =  0x0304    ! macro
    const GL_SPECULAR                        =  0x1202    ! macro
    const GL_ADD                             =  0x0104    ! macro
    const GL_ORDER                           =  0x0A01    ! macro
    const GL_LINEAR_ATTENUATION              =  0x1208    ! macro
    const GLUT_CURSOR_TEXT                   =  8    ! macro
    const GLU_VERTEX                         =  GLU_TESS_VERTEX    ! macro
    const GL_STENCIL_VALUE_MASK              =  0x0B93    ! macro
    const GL_CURRENT_INDEX                   =  0x0B01    ! macro
    const GL_MAX_LIST_NESTING                =  0x0B31    ! macro
    const GL_PIXEL_MAP_A_TO_A_SIZE           =  0x0CB9    ! macro
    const GL_PIXEL_MAP_G_TO_G                =  0x0C77    ! macro
    const GL_COLOR_CLEAR_VALUE               =  0x0C22    ! macro
    const GLU_TESS_WINDING_RULE              =  100140    ! macro
    const GL_POINT                           =  0x1B00    ! macro
    const GLUT_VIDEO_RESIZE_POSSIBLE         = (900)    ! macro
    const GL_CURRENT_RASTER_TEXTURE_COORDS   = 0x0B06    ! macro
    const GL_LINE_STIPPLE                    =  0x0B24    ! macro
    const GLU_PARAMETRIC_TOLERANCE           =  100202    ! macro
    const GLUT_DOUBLE                        =  2    ! macro
    const GLUT_CURSOR_HELP                   =  4    ! macro
    const GL_TEXTURE_COORD_ARRAY_TYPE_EXT    =  0x8089    ! macro
    const GLUT_WINDOW_X                      = (100)    ! macro
    const GL_GREATER                         = 0x0204    ! macro
    const GLU_U_STEP                         =  100206    ! macro
    const GLUT_WINDOW_Y                      = (101)    ! macro
    const GLUT_CURSOR_WAIT                   = 7    ! macro
    const GL_COLOR_MATERIAL                  =  0x0B57    ! macro
    const GL_COLOR_WRITEMASK                 =  0x0C23    ! macro
    const GL_CULL_FACE                       =  0x0B44    ! macro
    const GLUT_JOYSTICK_BUTTONS              = (614)    ! macro
    const GL_TEXTURE_COMPONENTS              = GL_TEXTURE_INTERNAL_FORMAT    ! macro
    const GL_DIFFUSE                         =  0x1201    ! macro
    const GL_LINE_BIT                        =  0x00000004    ! macro
    const GL_ALL_ATTRIB_BITS                 =  0x000fffff    ! macro
    const GL_NORMAL_ARRAY_TYPE_EXT           =  0x807E    ! macro
    const GL_COLOR_ARRAY                     =  0x8076    ! macro
    const GL_T2F_V3F                         =  0x2A27    ! macro
    const GLUT_KEY_PAGE_UP                   =  104    ! macro
    const GL_TEXTURE_LUMINANCE_SIZE          =  0x8060    ! macro
    const GL_VERTEX_ARRAY_EXT                =  0x8074    ! macro
    const GL_T2F_C4UB_V3F                    =  0x2A29    ! macro
    const GL_MAX_PIXEL_MAP_TABLE             =  0x0D34    ! macro
    const GL_UNPACK_SKIP_ROWS                =  0x0CF3    ! macro
    const GL_LINEAR_MIPMAP_LINEAR            =  0x2703    ! macro
    const GL_STENCIL_CLEAR_VALUE             =  0x0B91    ! macro
    const GL_VERTEX_ARRAY_SIZE_EXT           =  0x807A    ! macro
    const GL_TEXTURE_MAG_FILTER              =  0x2800    ! macro
    const GL_OBJECT_PLANE                    =  0x2501    ! macro
    const GL_BLUE                            =  0x1905    ! macro
    const GL_RENDER                          =  0x1C00    ! macro
    const GL_TEXTURE_COORD_ARRAY_EXT         =  0x8078    ! macro
    const GL_FLAT                            =  0x1D00    ! macro
    const GL_NEAREST_MIPMAP_LINEAR           =  0x2702    ! macro
    const GL_LINEAR_MIPMAP_NEAREST           =  0x2701    ! macro
    const GL_NAME_STACK_DEPTH                =  0x0D70    ! macro
    const GL_TEXTURE_COORD_ARRAY_POINTER_EXT =  0x8092    ! macro
    const GL_FOG_MODE                        =  0x0B65    ! macro
    const GL_MAP1_GRID_SEGMENTS              =  0x0DD1    ! macro
    const GLU_CCW                            =  100121    ! macro
    const GLUT_CURSOR_INFO                   =  2    ! macro
    const GL_COLOR_ARRAY_POINTER             =  0x8090    ! macro
    const GL_POINT_SIZE_GRANULARITY          =  0x0B13    ! macro
    const GL_MAP1_VERTEX_3                   =  0x0D97    ! macro
    const GL_PACK_LSB_FIRST                  =  0x0D01    ! macro
    const GL_TEXTURE_GREEN_SIZE              =  0x805D    ! macro
    const GL_MAP1_VERTEX_4                   =  0x0D98    ! macro
    const GL_POINTS                          =  0x0000    ! macro
    const GLUT_WINDOW_DEPTH_SIZE             = (106)    ! macro
    const GL_COMPILE_AND_EXECUTE             = 0x1301    ! macro
    const GL_BLUE_BIAS                       =  0x0D1B    ! macro
    const GL_FILL                            =  0x1B02    ! macro
    const GL_SRC_ALPHA                       =  0x0302    ! macro
    const GL_AMBIENT                         =  0x1200    ! macro
    const GL_COLOR                           =  0x1800    ! macro
    const GL_EDGE_FLAG_ARRAY                 =  0x8079    ! macro
    const GL_RGBA2                           =  0x8055    ! macro
    const GL_DOMAIN                          =  0x0A02    ! macro
    const GL_TEXTURE_ALPHA_SIZE              =  0x805F    ! macro
    const GL_RGBA4                           =  0x8056    ! macro
    const GLUT_HAS_TABLET                    = (604)    ! macro
    const GL_RGBA12                          = 0x805A    ! macro
    const GL_EQUIV                           =  0x1509    ! macro
    const GL_FEEDBACK_BUFFER_TYPE            =  0x0DF2    ! macro
    const GLUT_INDEX                         =  1    ! macro
    const GL_RGBA8                           =  0x8058    ! macro
    const GL_ACCUM_RED_BITS                  =  0x0D58    ! macro
    const GL_RGB10_A2                        =  0x8059    ! macro
    const GL_RGBA16                          =  0x805B    ! macro
    const GLU_POINT                          =  100010    ! macro
    const GL_BLEND                           =  0x0BE2    ! macro
    const GLUT_VIDEO_RESIZE_WIDTH_DELTA      = (904)    ! macro
    const GL_INTENSITY4                      = 0x804A    ! macro
    const GL_BITMAP_TOKEN                    =  0x0704    ! macro
    const GL_SPOT_CUTOFF                     =  0x1206    ! macro
    const GL_TEXTURE_WRAP_S                  =  0x2802    ! macro
    const GL_KEEP                            =  0x1E00    ! macro
    const GL_TEXTURE_WRAP_T                  =  0x2803    ! macro
    const GL_POINT_SMOOTH                    =  0x0B10    ! macro
    const GL_GREEN                           =  0x1904    ! macro
    const GL_INTENSITY8                      =  0x804B    ! macro
    const GL_PACK_SKIP_PIXELS                =  0x0D04    ! macro
    const GL_INCR                            =  0x1E02    ! macro
    const GL_FEEDBACK                        =  0x1C01    ! macro
    const GL_CURRENT_RASTER_INDEX            =  0x0B05    ! macro
    const GLU_EDGE_FLAG                      =  GLU_TESS_EDGE_FLAG    ! macro
    const GLUT_WINDOW_ACCUM_BLUE_SIZE        = (113)    ! macro
    const GL_FOG_BIT                         = 0x00000080    ! macro
    const GL_XOR                             =  0x1506    ! macro
    const GLU_TESS_VERTEX                    =  100101    ! macro
    const GL_SPOT_EXPONENT                   =  0x1205    ! macro
    const GL_LUMINANCE8_ALPHA8               =  0x8045    ! macro
    const GLUT_CURSOR_BOTTOM_RIGHT_CORNER    =  18    ! macro
    const GL_FALSE                           =  0    ! macro
    const GLU_VERSION_1_1                    =  1    ! macro
    const GL_PIXEL_MAP_R_TO_R_SIZE           =  0x0CB6    ! macro
    const GLU_VERSION_1_2                    =  1    ! macro
    const GL_NORMAL_ARRAY_COUNT_EXT          =  0x8080    ! macro
    const GLUT_WINDOW_GREEN_SIZE             = (108)    ! macro
    const GL_COLOR_ARRAY_COUNT_EXT           = 0x8084    ! macro
    const GLUT_WINDOW_COLORMAP_SIZE          = (119)    ! macro
    const GL_OBJECT_LINEAR                   = 0x2401    ! macro
    const GL_EXP2                            =  0x0801    ! macro
    const GL_REPEAT                          =  0x2901    ! macro
    const GLUT_NORMAL_DAMAGED                = (804)    ! macro
    const GL_LUMINANCE4                      = 0x803F    ! macro
    const GL_STEREO                          =  0x0C33    ! macro
    const GL_AMBIENT_AND_DIFFUSE             =  0x1602    ! macro
!    const GLUT_BITMAP_TIMES_ROMAN_10         = ref void(4)    ! macro
    const GL_SPOT_DIRECTION                  = 0x1204    ! macro
    const GLUT_FULLY_RETAINED                =  1    ! macro
    const GL_CONSTANT_ATTENUATION            =  0x1207    ! macro
    const GLUT_GAME_MODE_PIXEL_DEPTH         = (4)    ! macro
    const GL_LOGIC_OP_MODE                   = 0x0BF0    ! macro
    const GL_LUMINANCE8                      =  0x8040    ! macro
    const GL_INDEX_ARRAY_COUNT_EXT           =  0x8087    ! macro
    const GL_SCISSOR_BIT                     =  0x00080000    ! macro
    const GL_QUAD_STRIP                      =  0x0008    ! macro
    const GLUT_WINDOW_ALPHA_SIZE             = (110)    ! macro
    const GLUT_WINDOW_ACCUM_GREEN_SIZE       = (112)    ! macro
    const GL_PIXEL_MAP_A_TO_A                = 0x0C79    ! macro
    const GLUT_DISPLAY_MODE_POSSIBLE         = (400)    ! macro
    const GL_4D_COLOR_TEXTURE                = 0x0604    ! macro
    const GL_INDEX_LOGIC_OP                  =  0x0BF1    ! macro
    const GL_EVAL_BIT                        =  0x00010000    ! macro
    const GL_TRIANGLE_STRIP                  =  0x0005    ! macro
    const GL_POINT_SIZE_RANGE                =  0x0B12    ! macro
    const GL_FLOAT                           =  0x1406    ! macro
    const GL_ALPHA4                          =  0x803B    ! macro
    const GL_PERSPECTIVE_CORRECTION_HINT     =  0x0C50    ! macro
    const GLU_TESS_WINDING_NONZERO           =  100131    ! macro
    const GL_ALPHA8                          =  0x803C    ! macro
    const GLUT_ALPHA                         =  8    ! macro
    const GL_CLAMP                           =  0x2900    ! macro
    const GLUT_WINDOW_ACCUM_ALPHA_SIZE       = (114)    ! macro
    const GL_POLYGON_BIT                     = 0x00000008    ! macro
    const GL_FEEDBACK_BUFFER_SIZE            =  0x0DF1    ! macro
    const GLUT_WINDOW_FORMAT_ID              = (123)    ! macro
    const GL_Q                               = 0x2003    ! macro
!    const GLUT_BITMAP_TIMES_ROMAN_24         = ref void(5)    ! macro
    const GLUT_WINDOW_BLUE_SIZE              = (109)    ! macro
    const GL_R                               = 0x2002    ! macro
    const GLU_END                            =  GLU_TESS_END    ! macro
    const GL_S                               =  0x2000    ! macro
    const GL_COLOR_ARRAY_TYPE                =  0x8082    ! macro
    const GL_TEXTURE_RESIDENT                =  0x8067    ! macro
    const GL_T                               =  0x2001    ! macro
    const GLUT_HAS_MOUSE                     = (601)    ! macro
    const GL_MAX_LIGHTS                      = 0x0D31    ! macro
    const GL_INDEX_ARRAY_POINTER             =  0x8091    ! macro
    const GLUT_DEPTH                         =  16    ! macro
    const GL_INDEX_ARRAY_STRIDE              =  0x8086    ! macro
    const GLU_FLAT                           =  100001    ! macro
    const GL_ACCUM_GREEN_BITS                =  0x0D59    ! macro
    const GL_LINE_WIDTH                      =  0x0B21    ! macro
    const GLU_TESS_ERROR                     =  100103    ! macro
    const GLUT_OVERLAY                       = (1)    ! macro
    const GL_ACCUM_BLUE_BITS                 = 0x0D5A    ! macro
    const GLUT_KEY_RIGHT                     =  102    ! macro
    const GL_LIST_MODE                       =  0x0B30    ! macro
    const GLUT_CURSOR_SPRAY                  =  6    ! macro
    const GLUT_GAME_MODE_ACTIVE              = (0)    ! macro
    const GL_ACCUM                           = 0x0100    ! macro
    const GL_TEXTURE_STACK_DEPTH             =  0x0BA5    ! macro
    const GL_COLOR_INDEX4_EXT                =  0x80E4    ! macro
    const GL_NEAREST_MIPMAP_NEAREST          =  0x2700    ! macro
    const GLU_FILL                           =  100012    ! macro
    const GL_ACCUM_ALPHA_BITS                =  0x0D5B    ! macro
    const GLUT_WINDOW_CURSOR                 = (122)    ! macro
    const GL_TEXTURE_ENV                     = 0x2300    ! macro
    const GLU_BEGIN                          =  GLU_TESS_BEGIN    ! macro
    const GLU_FALSE                          =  GL_FALSE    ! macro
    const GL_TEXTURE_RED_SIZE                =  0x805C    ! macro
    const GL_STENCIL_FUNC                    =  0x0B92    ! macro
    const GL_REPLACE                         =  0x1E01    ! macro
    const GLU_PATH_LENGTH                    =  100215    ! macro
    const GL_LINE_SMOOTH                     =  0x0B20    ! macro
    const GL_MAP2_TEXTURE_COORD_1            =  0x0DB3    ! macro
    const GL_COLOR_ARRAY_SIZE_EXT            =  0x8081    ! macro
    const GL_MAP2_TEXTURE_COORD_2            =  0x0DB4    ! macro
    const GL_T4F_C4F_N3F_V4F                 =  0x2A2D    ! macro
    const GL_MAP2_TEXTURE_COORD_3            =  0x0DB5    ! macro
    const GLUT_ELAPSED_TIME                  = (700)    ! macro
    const GL_INDEX_ARRAY                     = 0x8077    ! macro
    const GL_MAP2_TEXTURE_COORD_4            =  0x0DB6    ! macro
    const GL_LIGHT_MODEL_AMBIENT             =  0x0B53    ! macro
    const GLUT_UP                            =  1    ! macro
    const GL_TEXTURE_ENV_COLOR               =  0x2201    ! macro
    const GLUT_CURSOR_TOP_SIDE               =  12    ! macro
    const GLU_OUTLINE_PATCH                  =  100241    ! macro
    const GL_ONE                             =  1    ! macro
    const GL_TEXTURE_MATRIX                  =  0x0BA8    ! macro
    const GLUT_CURSOR_RIGHT_ARROW            =  0    ! macro
    const GLU_INSIDE                         =  100021    ! macro
    const GL_DEPTH_WRITEMASK                 =  0x0B72    ! macro
    const GL_RIGHT                           =  0x0407    ! macro
    const GL_TEXTURE_BLUE_SIZE               =  0x805E    ! macro
    const GL_R3_G3_B2                        =  0x2A10    ! macro
    const GL_DST_COLOR                       =  0x0306    ! macro
    const GLUT_WINDOW_NUM_SAMPLES            = (120)    ! macro
    const GLUT_VIDEO_RESIZE_WIDTH            = (908)    ! macro
    const GL_DEPTH_BUFFER_BIT                = 0x00000100    ! macro
    const GL_NORMAL_ARRAY_STRIDE_EXT         =  0x807F    ! macro
    const GL_TEXTURE_INTERNAL_FORMAT         =  0x1003    ! macro
    const GL_OR_REVERSE                      =  0x150B    ! macro
    const GL_RED                             =  0x1903    ! macro
    const GL_MAP1_NORMAL                     =  0x0D92    ! macro
    const GL_TRIANGLE_FAN                    =  0x0006    ! macro
    const GLUT_DEVICE_IGNORE_KEY_REPEAT      = (610)    ! macro
    const GLUT_KEY_END                       = 107    ! macro
    const GL_ZERO                            =  0    ! macro
    const GL_CLIENT_ALL_ATTRIB_BITS          =  0xffffffff    ! macro
    const GL_MAP2_GRID_DOMAIN                =  0x0DD2    ! macro
    const GL_BLEND_DST                       =  0x0BE0    ! macro
    const GL_AND                             =  0x1501    ! macro
    const GL_COLOR_ARRAY_SIZE                =  0x8081    ! macro
    const GL_INDEX_ARRAY_TYPE                =  0x8085    ! macro
    const GL_AND_REVERSE                     =  0x1502    ! macro
    const GLUT_LEFT_BUTTON                   =  0    ! macro
    const GL_READ_BUFFER                     =  0x0C02    ! macro
    const GLUT_CURSOR_LEFT_ARROW             =  1    ! macro
    const GL_FOG_INDEX                       =  0x0B61    ! macro
    const GL_FRONT_RIGHT                     =  0x0401    ! macro
    const GL_EXP                             =  0x0800    ! macro
    const GLUT_NUM_DIALS                     = (608)    ! macro
    const GL_TRIANGLES                       = 0x0004    ! macro
    const GL_TEXTURE_BORDER_COLOR            =  0x1004    ! macro
    const GL_RENDERER                        =  0x1F01    ! macro
    const GL_PHONG_HINT_WIN                  =  0x80EB    ! macro
    const GL_WIN_draw_range_elements         =  1    ! macro
    const GLUT_CURSOR_TOP_RIGHT_CORNER       =  17    ! macro
    const GLUT_VIDEO_RESIZE_X                = (906)    ! macro
    const GLUT_VIDEO_RESIZE_Y                = (907)    ! macro
    const GLUT_NORMAL                        = (0)    ! macro
    const GLUT_VIDEO_RESIZE_HEIGHT_DELTA     = (905)    ! macro
    const GL_CW                              = 0x0900    ! macro
    const GLUT_PARTIALLY_RETAINED            =  2    ! macro
!    const glutInit_                           =  glutInit_ATEXIT_HACK    ! macro
    const GL_VERTEX_ARRAY_STRIDE_EXT         =  0x807C    ! macro
    const GLUT_CURSOR_LEFT_RIGHT             =  11    ! macro
    const GLUT_CURSOR_FULL_CROSSHAIR         =  102    ! macro
    const GL_PIXEL_MAP_B_TO_B_SIZE           =  0x0CB8    ! macro
    const GL_COLOR_INDEX12_EXT               =  0x80E6    ! macro
    const GL_DOUBLEBUFFER                    =  0x0C32    ! macro
!    macro GLUTAPI                            =  extern    ! macro
    const GL_POINT_TOKEN                     =  0x0701    ! macro
    const GL_INDEX_CLEAR_VALUE               =  0x0C20    ! macro
    const GL_RGB                             =  0x1907    ! macro
    const GLU_TESS_VERTEX_DATA               =  100107    ! macro
    const GLUT_ENTERED                       =  1    ! macro
    const GL_COLOR_TABLE_GREEN_SIZE_EXT      =  0x80DB    ! macro
    const GL_ALPHA12                         =  0x803D    ! macro
    const GL_POLYGON_OFFSET_FACTOR           =  0x8038    ! macro
!    const glutCreateWindow_                   =  glutCreateWindow_ATEXIT_HACK    ! macro
    const GL_CLIP_PLANE0                     =  0x3000    ! macro
    const GLUT_WINDOW_WIDTH                  = (102)    ! macro
    const GL_3D_COLOR_TEXTURE                = 0x0603    ! macro
    const GL_CLIP_PLANE1                     =  0x3001    ! macro
    const GL_DEPTH_RANGE                     =  0x0B70    ! macro
    const GL_DEPTH_BITS                      =  0x0D56    ! macro
    const GL_ALPHA16                         =  0x803E    ! macro
    const GL_CLIP_PLANE2                     =  0x3002    ! macro
    const GL_COLOR_ARRAY_STRIDE_EXT          =  0x8083    ! macro
    const GL_CLIP_PLANE3                     =  0x3003    ! macro
    const GL_DEPTH_CLEAR_VALUE               =  0x0B73    ! macro
    const GL_N3F_V3F                         =  0x2A25    ! macro
    const GL_CLIP_PLANE4                     =  0x3004    ! macro
    const GLUT_WINDOW_NUM_CHILDREN           = (118)    ! macro
    const GL_ACCUM_CLEAR_VALUE               = 0x0B80    ! macro
    const GL_CLIP_PLANE5                     =  0x3005    ! macro
    const GL_AUX_BUFFERS                     =  0x0C00    ! macro
    const GL_STENCIL_REF                     =  0x0B97    ! macro
    const GL_CLIENT_PIXEL_STORE_BIT          =  0x00000001    ! macro
    const GL_EDGE_FLAG                       =  0x0B43    ! macro
    const GL_SCISSOR_BOX                     =  0x0C10    ! macro
    const GL_MAX_EVAL_ORDER                  =  0x0D30    ! macro
    const GL_SRC_COLOR                       =  0x0300    ! macro
    const GL_ZOOM_X                          =  0x0D16    ! macro
    const GL_TEXTURE_COORD_ARRAY             =  0x8078    ! macro
    const GL_ZOOM_Y                          =  0x0D17    ! macro
    const GL_CURRENT_COLOR                   =  0x0B00    ! macro
    const GL_SHORT                           =  0x1402    ! macro
    const GL_BITMAP                          =  0x1A00    ! macro
    const GLUT_API_VERSION                   =  3    ! macro
    const GL_LIGHT_MODEL_TWO_SIDE            =  0x0B52    ! macro
    const GLU_SILHOUETTE                     =  100013    ! macro
    const GL_INVALID_OPERATION               =  0x0502    ! macro
    const GL_EYE_PLANE                       =  0x2502    ! macro
    const GLUT_NUM_BUTTON_BOX_BUTTONS        = (607)    ! macro
    const GL_LUMINANCE12_ALPHA12             = 0x8047    ! macro
    const GLUT_KEY_REPEAT_ON                 =  1    ! macro
    const GLU_TESS_ERROR_DATA                =  100109    ! macro
    const GL_DECR                            =  0x1E03    ! macro
    const GL_OR_INVERTED                     =  0x150D    ! macro
    const GL_COLOR_INDEX8_EXT                =  0x80E5    ! macro
    const GL_POLYGON_STIPPLE                 =  0x0B42    ! macro
    const GLU_TESS_EDGE_FLAG                 =  100104    ! macro
    const GL_TEXTURE_ENV_MODE                =  0x2200    ! macro
    const GLUT_OWNS_JOYSTICK                 = (613)    ! macro
    const GL_FRONT_AND_BACK                  = 0x0408    ! macro
    const GLU_NURBS_ERROR1                   =  100251    ! macro
    const GL_POINT_SIZE                      =  0x0B11    ! macro
    const GLU_NURBS_ERROR2                   =  100252    ! macro
    const GL_C3F_V3F                         =  0x2A24    ! macro
    const GL_INTENSITY12                     =  0x804C    ! macro
    const GLU_NURBS_ERROR3                   =  100253    ! macro
    const GLU_NURBS_ERROR4                   =  100254    ! macro
    const GL_POLYGON_SMOOTH                  =  0x0B41    ! macro
    const GL_RED_BIAS                        =  0x0D15    ! macro
    const GLU_NURBS_ERROR5                   =  100255    ! macro
    const GL_GREEN_BITS                      =  0x0D53    ! macro
    const GLU_NURBS_ERROR6                   =  100256    ! macro
    const GL_INTENSITY16                     =  0x804D    ! macro
    const GLU_NURBS_ERROR7                   =  100257    ! macro
    const GLU_TESS_COMBINE_DATA              =  100111    ! macro
    const GL_LINE_SMOOTH_HINT                =  0x0C52    ! macro
    const GLU_NURBS_ERROR8                   =  100258    ! macro
    const GL_CURRENT_BIT                     =  0x00000001    ! macro
    const GLU_NURBS_ERROR9                   =  100259    ! macro
    const GLU_MAP1_TRIM_2                    =  100210    ! macro
    const GLUT_HAS_JOYSTICK                  = (612)    ! macro
    const GLU_MAP1_TRIM_3                    = 100211    ! macro
    const GL_EXT_vertex_array                =  1    ! macro
    const GL_TEXTURE_HEIGHT                  =  0x1001    ! macro
    const GLU_UNKNOWN                        =  100124    ! macro
    const GL_LUMINANCE_ALPHA                 =  0x190A    ! macro
    const GL_SELECTION_BUFFER_POINTER        =  0x0DF3    ! macro
    const GL_NOTEQUAL                        =  0x0205    ! macro
    const GL_VERTEX_ARRAY_COUNT_EXT          =  0x807D    ! macro
    const GL_POLYGON_OFFSET_FILL             =  0x8037    ! macro
    const GL_ALPHA_BITS                      =  0x0D55    ! macro
    const GL_LINE_WIDTH_RANGE                =  0x0B22    ! macro
    const GL_T2F_N3F_V3F                     =  0x2A2B    ! macro
    const GL_TEXTURE_COORD_ARRAY_SIZE_EXT    =  0x8088    ! macro
    const GLUT_MIDDLE_BUTTON                 =  1    ! macro
    const GL_PIXEL_MAP_I_TO_I_SIZE           =  0x0CB0    ! macro
    const GL_ALPHA_TEST_REF                  =  0x0BC2    ! macro
    const GLU_TESS_END_DATA                  =  100108    ! macro
    const GL_LINES                           =  0x0001    ! macro
    const GL_QUADS                           =  0x0007    ! macro
    const GL_NORMAL_ARRAY_TYPE               =  0x807E    ! macro
    const GL_COLOR_INDEXES                   =  0x1603    ! macro
    const GL_TEXTURE_WIDTH                   =  0x1000    ! macro
    const GLU_OUTSIDE                        =  100020    ! macro
    const GL_LIST_BASE                       =  0x0B32    ! macro
    const GLUT_LAYER_IN_USE                  = (801)    ! macro
    const GL_V2F                             = 0x2A20    ! macro
    const GLUT_CURSOR_DESTROY                =  3    ! macro
    const GL_PROXY_TEXTURE_1D                =  0x8063    ! macro
    const GL_COLOR_TABLE_INTENSITY_SIZE_EXT  =  0x80DF    ! macro
    const GL_POLYGON_SMOOTH_HINT             =  0x0C53    ! macro
    const GL_LINE                            =  0x1B01    ! macro
    const GL_PIXEL_MAP_S_TO_S_SIZE           =  0x0CB1    ! macro
    const GL_EDGE_FLAG_ARRAY_COUNT_EXT       =  0x808D    ! macro
    const GLUT_KEY_DOWN                      =  103    ! macro
    const GL_LUMINANCE12_ALPHA4              =  0x8046    ! macro
    const GL_FOG                             =  0x0B60    ! macro
    const GL_UNSIGNED_INT                    =  0x1405    ! macro
    const GL_LIST_INDEX                      =  0x0B33    ! macro
    const GL_STACK_UNDERFLOW                 =  0x0504    ! macro
    const GLUT_MENU_NOT_IN_USE               =  0    ! macro
    const GL_POINT_BIT                       =  0x00000002    ! macro
    const GLUT_MENU_NUM_ITEMS                = (300)    ! macro
    const GL_NORMAL_ARRAY                    = 0x8075    ! macro
    const GLUT_TRANSPARENT_INDEX             = (803)    ! macro
    const GL_VERTEX_ARRAY                    = 0x8074    ! macro
    const GL_GREEN_SCALE                     =  0x0D18    ! macro
    const GL_COLOR_INDEX16_EXT               =  0x80E7    ! macro
    const GL_V3F                             =  0x2A21    ! macro
    const GLU_EXTENSIONS                     =  100801    ! macro
    const GL_PROXY_TEXTURE_2D                =  0x8064    ! macro
    const GLU_ERROR                          =  GLU_TESS_ERROR    ! macro
    const GL_SCISSOR_TEST                    =  0x0C11    ! macro
    const GL_GEQUAL                          =  0x0206    ! macro
    const GL_SELECT                          =  0x1C02    ! macro
    const GL_CURRENT_RASTER_COLOR            =  0x0B04    ! macro
    const GL_UNSIGNED_BYTE                   =  0x1401    ! macro
    const GL_UNPACK_ALIGNMENT                =  0x0CF5    ! macro
    const GL_T2F_C3F_V3F                     =  0x2A2A    ! macro
    const GLUT_KEY_F1                        =  1    ! macro
    const GLUT_VIDEO_RESIZE_IN_USE           = (901)    ! macro
    const GLUT_KEY_F2                        = 2    ! macro
    const GL_WIN_swap_hint                   =  1    ! macro
    const GLU_TESS_BOUNDARY_ONLY             =  100141    ! macro
    const GLUT_KEY_F3                        =  3    ! macro
    const GLUT_KEY_F4                        =  4    ! macro
    const GLUT_GREEN                         =  1    ! macro
    const GLUT_KEY_F5                        =  5    ! macro
    const GL_TEXTURE_COORD_ARRAY_POINTER     =  0x8092    ! macro
    const GLUT_KEY_F6                        =  6    ! macro
    const GLUT_BLUE                          =  2    ! macro
    const GLUT_KEY_F7                        =  7    ! macro
    const GLUT_KEY_F8                        =  8    ! macro
    const GL_RENDER_MODE                     =  0x0C40    ! macro
    const GLUT_KEY_F9                        =  9    ! macro
    const GL_LIGHTING_BIT                    =  0x00000040    ! macro
    const GLUT_CURSOR_INHERIT                =  100    ! macro
    const GL_INDEX_ARRAY_EXT                 =  0x8077    ! macro
    const GLU_TESS_WINDING_NEGATIVE          =  100133    ! macro
    const GL_DEPTH_SCALE                     =  0x0D1E    ! macro
    const GL_LEQUAL                          =  0x0203    ! macro
    const GL_TEXTURE_INTENSITY_SIZE          =  0x8061    ! macro
    const GL_BACK_LEFT                       =  0x0402    ! macro
    const GLUT_STEREO                        =  256    ! macro
    const GL_ONE_MINUS_DST_ALPHA             =  0x0305    ! macro
    const GLUT_DEVICE_KEY_REPEAT             = (611)    ! macro
    const GL_POINT_SMOOTH_HINT               = 0x0C51    ! macro
    const GL_TRANSFORM_BIT                   =  0x00001000    ! macro
    const GL_LINE_STIPPLE_PATTERN            =  0x0B25    ! macro
    const GL_PASS_THROUGH_TOKEN              =  0x0700    ! macro
    const GLUT_OVERLAY_DAMAGED               = (805)    ! macro
    const GL_DONT_CARE                       = 0x1100    ! macro
    const GL_COLOR_TABLE_ALPHA_SIZE_EXT      =  0x80DD    ! macro
    const GL_FOG_END                         =  0x0B64    ! macro
    const GL_ATTRIB_STACK_DEPTH              =  0x0BB0    ! macro
    const GL_PACK_SKIP_ROWS                  =  0x0D03    ! macro
    const GLU_EXTERIOR                       =  100123    ! macro
    const GLU_NURBS_ERROR10                  =  100260    ! macro
    const GLUT_HAS_KEYBOARD                  = (600)    ! macro
    const GLU_NURBS_ERROR11                  = 100261    ! macro
    const GL_CURRENT_RASTER_POSITION         =  0x0B07    ! macro
    const GL_VERSION_1_1                     =  1    ! macro
    const GL_RED_SCALE                       =  0x0D14    ! macro
    const GLU_NURBS_ERROR12                  =  100262    ! macro
    const GL_RGB4                            =  0x804F    ! macro
    const GL_UNPACK_SKIP_PIXELS              =  0x0CF4    ! macro
    const GLU_NURBS_ERROR13                  =  100263    ! macro
    const GL_RGB5                            =  0x8050    ! macro
    const GLUT_INIT_WINDOW_X                 = (500)    ! macro
    const GL_POLYGON_TOKEN                   = 0x0703    ! macro
    const GLU_NURBS_ERROR14                  =  100264    ! macro
    const GLUT_INIT_WINDOW_Y                 = (501)    ! macro
    const GLU_NURBS_ERROR15                  = 100265    ! macro
    const GLU_NURBS_ERROR16                  =  100266    ! macro
    const GL_COPY_INVERTED                   =  0x150C    ! macro
    const GL_RGB8                            =  0x8051    ! macro
    const GL_LEFT                            =  0x0406    ! macro
    const GL_INDEX_BITS                      =  0x0D51    ! macro
    const GLU_NURBS_ERROR17                  =  100267    ! macro
    const GLU_NURBS_ERROR18                  =  100268    ! macro
    const GL_DEPTH_TEST                      =  0x0B71    ! macro
    const GLU_NURBS_ERROR19                  =  100269    ! macro
    const GL_TEXTURE_BORDER                  =  0x1005    ! macro
    const GLUT_ACTIVE_CTRL                   =  2    ! macro
    const GLU_TESS_TOLERANCE                 =  100142    ! macro
    const GL_VERTEX_ARRAY_POINTER_EXT        =  0x808E    ! macro
    const GLUT_CURSOR_CROSSHAIR              =  9    ! macro
    const GL_LUMINANCE6_ALPHA2               =  0x8044    ! macro
    const GL_LINE_WIDTH_GRANULARITY          =  0x0B23    ! macro
    const GLUT_MENU_IN_USE                   =  1    ! macro
    const GL_NORMAL_ARRAY_POINTER            =  0x808F    ! macro
    const GL_AUX0                            =  0x0409    ! macro
    const GL_AUX1                            =  0x040A    ! macro
    const GL_STENCIL                         =  0x1802    ! macro
    const GL_CLEAR                           =  0x1500    ! macro
    const GL_AUX2                            =  0x040B    ! macro
    const GL_AUX3                            =  0x040C    ! macro
    const GL_MAP2_NORMAL                     =  0x0DB2    ! macro
    const GLU_NURBS_ERROR20                  =  100270    ! macro
    const GL_RGBA                            =  0x1908    ! macro
    const GL_INTENSITY                       =  0x8049    ! macro
    const GL_TRUE                            =  1    ! macro
    const GLU_NURBS_ERROR21                  =  100271    ! macro
    const GLU_TESS_NEED_COMBINE_CALLBACK     =  GLU_TESS_ERROR6    ! macro
    const GLU_NURBS_ERROR22                  =  100272    ! macro
    const GL_FOG_START                       =  0x0B63    ! macro
    const GLU_NURBS_ERROR23                  =  100273    ! macro
!    const GLUTCALLBACK                       =  __cdecl    ! macro
    const GLU_NURBS_ERROR24                  =  100274    ! macro
    const GL_BYTE                            =  0x1400    ! macro
    const GLU_NURBS_ERROR25                  =  100275    ! macro
    const GL_MAX_TEXTURE_SIZE                =  0x0D33    ! macro
    const GLU_NURBS_ERROR26                  =  100276    ! macro
    const GL_ALPHA_TEST_FUNC                 =  0x0BC1    ! macro
    const GL_VENDOR                          =  0x1F00    ! macro
    const GLU_NURBS_ERROR27                  =  100277    ! macro
    const GL_NICEST                          =  0x1102    ! macro
    const GLUT_ACCUM                         =  4    ! macro
    const GL_INDEX_WRITEMASK                 =  0x0C21    ! macro
    const GLU_NURBS_ERROR28                  =  100278    ! macro
    const GLU_NURBS_ERROR29                  =  100279    ! macro
    const GLU_TESS_WINDING_ODD               =  100130    ! macro
    const GLUT_NOT_VISIBLE                   =  0    ! macro
    const GLUT_CURSOR_TOP_LEFT_CORNER        =  16    ! macro
    const GLU_LINE                           =  100011    ! macro
    const GL_EDGE_FLAG_ARRAY_STRIDE          =  0x808C    ! macro
    const GL_NEAREST                         =  0x2600    ! macro
    const GLU_NURBS_ERROR30                  =  100280    ! macro
    const GL_COLOR_TABLE_FORMAT_EXT          =  0x80D8    ! macro
    const GLU_NURBS_ERROR31                  =  100281    ! macro
    const GL_MAX_MODELVIEW_STACK_DEPTH       =  0x0D36    ! macro
    const GL_COLOR_TABLE_WIDTH_EXT           =  0x80D9    ! macro
    const GLU_NURBS_ERROR32                  =  100282    ! macro
    const GL_ONE_MINUS_SRC_ALPHA             =  0x0303    ! macro
    const GLUT_SCREEN_WIDTH                  = (200)    ! macro
    const GLU_NURBS_ERROR33                  = 100283    ! macro
    const GL_STENCIL_FAIL                    =  0x0B94    ! macro
    const GL_VERTEX_ARRAY_POINTER            =  0x808E    ! macro
    const GLU_NURBS_ERROR34                  =  100284    ! macro
    const GL_MAP1_TEXTURE_COORD_1            =  0x0D93    ! macro
    const GLU_NURBS_ERROR35                  =  100285    ! macro
    const GL_INDEX_ARRAY_STRIDE_EXT          =  0x8086    ! macro
    const GL_MAP1_TEXTURE_COORD_2            =  0x0D94    ! macro
    const GLU_NURBS_ERROR36                  =  100286    ! macro
end

global type glenum		= u32
global type glboolean	= u8
global type glbitfield	= u32
global type glbyte		= i8
global type glshort		= i16
global type glint		= i32
global type glsizei		= i32
global type glubyte		= u8
global type glushort	= u16
global type gluint		= u8
global type glfloat		= r32
global type glclampf	= r32
global type gldouble	= r64
global type glclampd	= r64
global type glvoid		= void
