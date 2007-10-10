\name{RGrace-GUI}
\alias{RGrace-GUI}
\alias{RGrace.GUI}
\title{RGrace main window GUI}
\description{
  The short description of RGrace's GUI controls (GTK version).
}
\details{
  Although you can use RGrace window from command line interface as
  standard R Graphic device with commands like \code{ggplot} and
  \code{current.Figure$...},\code{current.Panel$...} one of the goals of RGrace
  was to let user create and edit simple graphics through GUI
  without applying to \R command line. Typical usage of RGrace would be -
  opening text file with data table, plotting some of this data table
  columns on the current figure (menu item File->Download),
  changing the appearance of this data curve (with controls on
  Panel/Element notebook), adding text annotation to the graphic
  (Annotation page), making some sort of simple data analysis (Hooks
  menu), saving resulting graphic as \R script or/and printing to
  ps/pdf/png file (menu items File->Save as
  / File->Print->PS,PDF,PNG,PDFwrite).  

  RGrace figure has a notebook with "Panels", "Elements", "Annotations"
  pages, short menu ("File","Edit",...) and interaction context
  selection option menu. All these controls are located at the initially
  collapsed slide
  panel to the left of the drawing area, so to make them visible one should
  drag out slide panel by its handle. Let us look at the notebook:
  \begin{description}
  \item[Panels.]
  
    Here you can select and edit properties of \code{\link{panel}}s(axes) located in this figure.
  \begin{description}

    \item["Apply" button] - changes you made in GUI controls are submitted to panel after this button is pressed.

    \item["Add New" button] - add new blank panel with default values. Make it the current one.

    \item["Delete" button] - delete the current panel from figure.

    \item["Autorange" button] - rescales current panel so all elements
    in it will be shown.
    
    \item[Unnamed combo box] - list of panels in this
    figure. Selecting entry in it made selected panel current. Also
    updates GUI controls with the values of panel's properties.

    \item["X Range", "Y Range" text entries] - max and min values for panel's X,Y axes.

    \item[Notebook "Left","Bottom","Top","Right"] with two entries on
    each page for controlling corresponding axes ticks positions,labels
    and titles. Axis title may be plain text (in this case you should
    quote text title in Label text entry) or any mathematical formula
    (see \code{\link[base]{plotmath}}) and should be wrapped with
    \code{expression()} command. Example you may see on screenshot.Blank entry
    for "Ticks" means what ticks are evenly distributed along axes and
    located in places with "pretty" labels. Specify \code{Inf} if you do
    not want ticks be drawn on this axes. \code{c(1,2,3)} - if you want
    3 ticks in position 1,2,3. Note please that "ticks" parameter is
    string (class character) which is parsed and evaluated when it is
    first set or when panels's x/yscale are changed. This is different
    from other plot functions in \R which take a numeric vector as value
    for \code{at} parameter. Evaluation takes place twice in an figure's
    environment with \code{RANGE} and \code{TICK.LAB} variables set to
    the current panel's range and \code{TICK.LAB} is either "tick" or
    "label". This means  you can specify the  value
    \code{list(tick=c(0,18,22),label=c("Nothing","Enough","Too
      much"))[[TICK.LAB]]} for axis ticks and have a graph with 3 ticks
      at positions 0,18,22 labeled as "Nothing","Enough","Too
      much". \code{RANGE} variable is provided in case you will want to override default procedure of automatic calculation of tick placement (for example if you want to draw non-linear axis, e.g. logarithmic or reciprocal ones).

      \item["Grilled?" and "Ticks in?" check boxes] - add/remove grid from panel and controls direction of ticks (inward or outward of plotting area).

      \item["Panel placement" frame] - set the position of panel with respect to figure's border.
      \end{description}
      
      \item[Elements.]
	
	Here you can select and edit properties of elements (data lines) located in the panel you have selected beforehand on "Panels" page.
	\begin{description}
	\item["Apply" button] - changes you have made in GUI controls are submitted only after this button is pressed.

	\item["Delete" button] - delete the selected element.

	\item[Unnamed combo box] - list of elements in the current panel. Selecting entry in it updates GUI controls with element's properties values.

	\item["Symbol:..." combo box] - symbol to be drawn at data line's nodes.

	\item["Line:..." combo box] - type of line to be draw between data line's nodes.

	\item["Symbol size" spin box] - size of symbol in "chars" unit (see grid documentation).

	\item["Line width" spin box] - width of line in points. Zero means to use default line width (lwd field is not set inside gpar structure)

	\item["Outline color" combo box] - line's color ("default" - means what col field is not set in gpar structure).

	\item["Fill color" combo box] - the color data points are filled with ("default" - means what fill field is not set in gpar structure).

	\item["Data" frame] - simple editor which allows to edit 
	values of data line's points. "Index" spin-box selects index of
	data point to edit. Coordinates/error bars lengths of selected data point are
	displayed in  "X","Y","W" and "H" entries and there new values
	for this point can be entered.

	\item["Error Representation" combo box] - how \code{w},\code{h}
	parameters are drawn on graphic. Possible choices are: as error
	bars ("whiskers"), colored area ("areafill") or arrows.

	\item["Apply Color/Symbol Values to" combo box] - if individual
	points of selected data line have different fill
	color, symbol size and symbol type
	(i.e. at least one of the parameters \code{gp=gpar(fill=...)},
	\code{size}, \code{pch} to \code{\link{element}} or
	\code{\link{ggplot}} or \code{\link{grid.data}} functions is
	vector) then this combo box allow user to
	control the way changes made in the GUI controls are applied to
	selected data curve. In this case changing the value of "Index"
	spin-box in "Data" frame changes the selection of "Fill color",
	"Symbol:..." combo-boxes and the value of "Symbol size" spin-box
	too. If in this combo-box "All Points" item is selected then
	pressing "Apply" button sets the color/symbol value of all points in data
	curve to the values of selected point; "Selected point" -
	changes the values of currently selected point; "Points with the
	Same Values" - changes the value of points which have the symbol
	size or symbol type or fill color value the same as the selected
	one. Note that this operation does not change the length of the
	vectors (the vectors with different lengths are recycled as usual),
	so you can not use this controls to insert new "type" of data
	point into existing data line, but only to tune an appearance of
	already existing one. If the color/symbol values are the same
	along the curve then
	effect of selection made in this
	combo box is just the same.
	\end{description}

	\item[Annotations]
Here you can select and edit properties of text annotations located in
the panel you have selected beforehand on "Panels" page.

\begin{description}


\item["Apply" button] - changes you have made in GUI controls are submitted only after this button is pressed.

\item["Delete" button] - delete the selected annotation.

\item["Place new" button] - clicking this button and after that clicking left mouse button in plotting area will place new text annotations in mouse cursor position inside current panel with properties you have specified in GUI controls.

\item[Text widget] - annotation's text. Each line can either be a plain string (class character) or mathematical expression (class expression). The same rules regarding quoting/expression() as for axis title apply. There is also one exception: you can use command \code{\link{BULLET}(panel=n,element=m)} (with no quotes) to have a graphical representation of m'th element from the n'th panel in the selected annotation. Note that this representation is persistent and does not track changes that you have made to data line. So after editing data line  you have to re-Apply changes to the annotation containing \code{\link{BULLET}} comand representing this data line.

\item["Color" combobox] - foreground color of text("default" - means what col field is not set in gpar structure) .

\item["Text Style" frame] - select here font size (in points) and style (bold,italic...) for the text annotation. "Apply to panel" and "Apply to figure" buttons set all annotations and axis titles to this style and size. Zero text size and "default" text style means what fontsize and fontstyle fields are not set in gpar structure. Note, please, that the selected style is not applied to expressions (math text) - in that case you have to explicitly specify style inside expression() command.

\item["Rotation" spinbox] - rotates selected annotation counterclockwise.

\item["Framed?" check-box] - adds a 2-point frame around annotation.
\end{description}
\end{description}

At the top of slide panel there is a short menu bar with two items:
\begin{description}

\item["File" menu.]
Almost standard.
\begin{description}

\item["Save as","Save","Open" menu-items] - allow you to save figure in
  file and to open it later for further editing. It saves figure as
  \R-script which can be edited by your favorite text editor if something
  goes wrong. This "format" has also one nice feature - there is no
  questions of incompatibilities. You can save and open figure script
  files made in different versions of RGrace as long as \R versions you
  do it in is compatible between themselves. And one more note -
  although the figure file itself is a text file, some objects are
  dumped into it with \code{serialize()} call, so manually editing them
  may prove to be not so easy.

  \item["Download from File" menu item] - read ascii data tables and
  plot them on the figure. After clicking it the user presented with
  standard File selection dialog and after selecting appropriate file
  its content read with \code{\link{read.table}} function. The columns
  of resulting \code{data.frame} are fed into the form where user
  supposed to select how this \code{data.frame} will be plotted on the
  figure. The form itself has two columns - in the first column is the
  text entries with the names of data frame's columns (as the text of this entries
  will be parsed and evaled before plotting, you can enter here any
  valid \R expression), in the second the combo-boxes with the "role" of
  the data column (i.e. how the values of the column will be interpreted
  by RGrace). Suppose we have data.frame with columns
  \code{X},\code{Val1},\code{Val2},\code{Val1Err},\code{SampleGroup}
  (independent variable, results of two type of measurements, errors of
  the 1st type of measurements and numerical values identifying the
  samples these measurements was taken on). In this case in the form we
  will have five rows, the names of columns will be displayed in the
  text entries and combo boxes will contain 16 items
  "None", "X", "Y1"-"Y5", "ErrX", "ErrY1"-"ErrY5", "Color", "Symbol",
  "Size". Suppose we want to visualize our data at its best and plot not
  the \code{Val2} but its logarithm. In this case we select in
  combo-boxes: items "X" (x-value for all curves, if "X" is not present
  as a "role" then data of all curves will be plotted against theirs indices), "Y1" (value of first
  data-line), "Y2" (value of the second data line), "ErrY1" (vertical
  error bar of first curve) and "Color" (or "Size" or "Symbol", the difference
  between them is that in the first case data is scaled to
  fit in \code{palette()} range, in the second is plotted as is,
  i.e. size of data line's symbols is proportional to the data values
  and in the third data is coerced to integer and this value is used as
  a symbol code, does it fall in range 0-26 or not is not checked).  In the
  text entry corresponding to "Y2" we also type
  \code{log10(Val2)}. Pressing button "Apply" on the form will plot
  the data we have selected and "Quit" just closes the form.
  
 \item["Upload to File" menu item] - puts the selected data line to the
 ascii file as a text table.

 \item["Print" menu] - hardcopies the figure into file ("RPlots.*" in
  the current directory, extension depends on the selected print
  driver). The available choice of print drivers is "PS","PDF","PNG"
  with backends \code{\link{postscript}}, \code{\link{pdf}} and \code{\link{png}}; and
  "PDFwrite" with backend
  \code{\link{bitmap}(type="pdfwrite",...)} which requires  ghostscript
  being present in your system (this is just a shortcut for calling
  \code{postscript(...)} function and massaging the resulting ps file with
  ps2pdf ghostscript utility).

  \item["Quit" menu item] - close this figure. If it is also the
  \code{current.Figure}, then \code{current.Figure} variable is \code{NULL}-ed.
  \end{description}

  \item["Edit" menu.]
  Operations with internal copy/paste buffer (it has nothing to do with
  X-selection mechanism). You can copy selected panel or element ("Copy
  panel", "Copy element" menu items) into this buffer and paste ("Paste"
  menu item) selected structure into the figure/panel in the same
  \R-session.

  \item["Hooks" menu (optional)] - this menu is created in
  \code{\link{figure.startup}} script, so it is visible only if
  \code{\link{figure.startup}} is redefined with \code{hooks.user.menu} in
  .Rprofile file (my .Rprofile is in Example section below). Selecting
  menu item in this menu sets one of the RGrace's \code{\link{hooks}} to
  the sample scripts available in RGrace (i.e. this menu is an
  alternative to setting \code{on.select.xxx<-...} functions in command line
  interface, see \code{\link{hooks}} for detail). One of the reason not
  merging this menu code into RGrace is that user would like to have his
  own utility scripts which he would like to make available as subitem
  of "Hooks" menu. It is can be done easily by editing
  \code{RGrace/inst/figure.startup.exmpl.R} script.     
  \end{description}
Beside the menubar there is a "Select Something" option menu which
selects which one of the hooks
(\code{\link{on.select.annotation}},\code{\link{on.select.element}},
\code{\link{on.select.region}}, \code{\link{on.select.points}} ) will be played when we press mouse buttons
or keyboard keys inside plotting area (see \code{\link{hooks}} for more
details).
}\author{M.Kondrin}
\examples{
#do not run
#define code for future figure.startup function
user.menu <- function(...){
#build menu items to access mysql database
  db.user.menu(...)
#build menu with available hooks
  hooks.user.menu(...)
}

.First<-function(...){
  library(grDevices)
#redefine palette with all system's named colors
  palette(colors())
#source scripts from RGrace scripts directory 
  source(system.file("scripts","db.wizards.R",package="RGrace")) 
  source(system.file("scripts","figure.startup.exmpl.R",package="RGrace"))
#redefine figure.startup hook
  figure.startup<<-user.menu
  library(RGrace)
#install postscript fonts with cyrillic glyphs 
  ps.options(family=c(
system.file("CyrFonts","textb19.afm",package="RGrace"),
system.file("CyrFonts","textb20.afm",package="RGrace"),
system.file("CyrFonts","textb18.afm",package="RGrace"),
system.file("CyrFonts","textb18.afm",package="RGrace"),"sy______.afm"),
encoding=system.file("CyrFonts","koi8r.enc",package="RGrace"),override.check=T,reset=F)


}
}
\keyword{hplot}
\keyword{iplot}
\keyword{dynamic}
