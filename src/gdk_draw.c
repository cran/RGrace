#include <R.h>
#include <Rinternals.h>
#include <gdk/gdk.h>
#include <gtk/gtk.h>

SEXP
draw_rubber_box(SEXP widget, int * w, int * h)
{
    GtkWidget *drawing_widget = (GtkWidget*) R_ExternalPtrAddr(widget);
    GdkColor Color;
    GdkGC * gc1;
    Color.red=0;
    Color.green=0;
    Color.blue=0;
    Color.pixel=16;
    gc1=gdk_gc_new(drawing_widget->window);
    gdk_gc_set_function(gc1,GDK_XOR);
    gdk_gc_set_foreground(gc1,&Color);
    gdk_gc_set_line_attributes(gc1,1,GDK_LINE_SOLID,GDK_CAP_PROJECTING,GDK_JOIN_MITER);
    gdk_gc_set_fill(gc1,GDK_SOLID);
    gdk_gc_ref(gc1);
    gdk_draw_line(drawing_widget->window,gc1,*w,0,*w,10000);
    gdk_draw_line(drawing_widget->window,gc1,0,*h,10000,*h);
    gdk_gc_unref(gc1);
    gdk_flush();
}

