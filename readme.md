On October 26th, 2018 I purchased the software assets of Full Moon Software.
Full Moon Software used to be known as Crescent Software.  They produced a line
of excellent development libraries for MS-DOS.  The supported environments were
QuickBASIC 4.x, Microsoft Professional Development System v7.x, and Visual 
Basic for DOS.

The idea behind obtaining these products was to release them to the public
domain to ensure that people could still access these things in the future.
While most developers will have no use for these products in a modern 
develoment environment, they still have value as an example of "how it was 
done" back in the heyday of x86 DOS development. 

The software in this repository hasn't been modified from how I received it 
from Ethan Winer, the original author.  While all the source files carry some 
kind of Copyright notice, the software is now in the public domain.

The contents of the installation floppies will be uploaded to the Internet
Archive soon and when the manuals are scanned, they'll be uploaded there
as well.  I'll update this readme file with a link to the manual scan when
it's available.

The original distribution disk files are available here:

http://annex.retroarchive.org/crescent/Gqscdisk.zip


Gene Buckle, October 27th, 2018

I've attached the text from Full Moon Software's catalog description of 
QuickScreen and Graphics QuickScreen below.  QuickScreen can be found in a 
separate repository.

-------------------------------------------------------------------------------
==============================================================================

QUICKSCREEN(tm) AND GRAPHICS QUICKSCREEN(tm)
============================================

Create Attractive Text- and Graphics-Based Data-Entry Screens in Minutes
------------------------------------------------------------------------

There's no disputing that a good screen design program can help you create 
attractive programs and complete them quickly. The old-fashioned approach 
using LOCATE, COLOR, and PRINT simply takes too long when the screens are 
complex, or when there are many data entry fields. Painting screens and 
creating forms with a sophisticated screen design program is far easier than 
the trial and error method and affords greater creativity and a better 
finished result. But equally important to the savings of time and effort is 
the reduction in your program's code size.
     QuickScreen and Graphics QuickScreen (two separate products) are the most 
elegant screen management systems ever developed for use with Microsoft 
compiled BASIC. QuickScreen lets you quickly design forms and input screens 
for display in text mode on any computer (the 43- and 50-line modes are also 
supported), while Graphics QuickScreen uses BASIC's graphics screen modes 9 
(EGA) and 12 (VGA) and supports 25, 30, 43, and 60 lines. Both programs 
include a powerful screen designer to create screens and input fields, and 
both use supplied code that's added to your programs to handle all aspects of 
data entry. We provide full source code for the routines, so you can modify 
them if you want.
     Quickly draw text and backgrounds, create data-entry fields with field-
level help text, and even specify formulas for automatic field calculations, 
such as subtotals. The screen and field definitions are kept in a disk file, 
letting you make changes without recompiling, and the same data-entry routines 
are used for all screens. Compare that to clunky code generators that create 
many redundant pages of code for each and every screen. QuickScreen images are 
stored in compressed form, and the field definitions for multiple screens can 
be combined to reduce the number of files you must distribute. Graphics 
screens are stored in the popular .PCX format, while text screens can be 
combined to a single file or optionally linked directly into your .EXE 
program.

AN ELEGANT WAY TO CREATE BEAUTIFUL SCREENS

Both products include a sophisticated menu-driven screen design program that 
lets you create the screens and  define the data entry fields. These screen 
designers are fashioned after Microsoft's BASIC editors, and they use the same 
familiar menus and shortcut keys. Portions of the screen may be painted any 
color. Blocks are easily moved, copied, or deleted. Boxes and lines are drawn 
dynamically without disturbing the rest of the screen. Text-mode screens may 
be displayed with a variety of attractive video effects, such as vertical and 
horizontal wipes, exploding box, dissolve, opening curtain, roll-away, and 
many more. Graphics QuickScreen's designer is even more capable, offering many 
of the features found in commercial Paint programs. These include the 
following:

     * A pop-up drawing and color palette that keeps the entire screen in
       progress visible at all times.

     * User-defined snap-to-grid settings that simplify the alignment of
       fields and other objects.

     * A re-color palette that easily lets you change all occurrences of one
       color to another.

     * A tile palette that provides 119 dithered colors and 24 different tile
       patterns.

     * A zoom feature that lets you easily edit individual pixels.

     * Scalable fonts for creating captions and labels.

QUICKSCREEN FIELD TYPES

QuickScreen supports 17 different field types, and Graphics QuickScreen adds 
single-line scrolling text, horizontal and vertical scroll bars, and push-
buttons. The native field types include string, proper string, upper case, 
numeric string, integer, long integer, single and double precision, currency, 
American and European date, phone, zip code, social security number, logical, 
multiple choice, and memo (multi-line text). Optional attributes may be 
applied to identify fields as protected (read-only), relational, and indexed.
     Proper name fields are used to enforce the correct capitalization for 
names. For example, if "john smith" is entered, it is converted automatically 
to "John Smith." Logical fields are similar to Yes/No fields, but any two 
choices may be specified. Multiple choice fields let you define a list of 
acceptable choices, which are then presented in a scrolling menu. This method 
also lets you save disk space by storing only the choice numbers instead of 
the equivalent text. Memo fields use a mini-word processor that features 
automatic word wrapping, and horizontal and vertical scrolling to accommodate 
more text than can fit in the allotted screen space. Date calculations are 
fully supported, and the field data is packed to only two bytes to save disk 
space.
     Relational and indexed fields are flagged by the editing subroutines, 
although any indexing and relational operations must be handled by your 
program. Protected fields may be viewed by the user, but they are protected 
from being changed. Push buttons let you easily accept commands, such as Save, 
Load, or Print, and scroll bars provide a convenient way for users to select a 
single value from a range of values.
     Formulas for calculated fields may be based on any combination of fields 
plus both numeric and string constants. String formulas may be used to 
concatenate other fields to the calculated field. All of BASIC's math, 
logical, and relational operators are supported for field calculations, as are 
the transcendental (Trig) functions. Full mouse support is built into both the 
screen designer programs and also the supplied data entry modules that are 
added to your programs. The editing routines are called in a unique polled 
mode, which lets your program display,  save, or print data, even while new 
data is being entered. All editing, movement from field to field, and field 
calculations are handled for you automatically.

USING QUICKSCREEN AND GRAPHICS QUICKSCREEN

The QuickScreen subroutines are provided as BASIC source code that you load 
along with your program. The actual field contents are passed in a string 
array, with each element corresponding to one data-entry field. You may also 
pre-load one or more elements with default values and examine each field upon 
return. Further, all of the fields are combined for you into a single string, 
ready to be written to disk. Likewise, a record that has been read from disk 
is unpacked into separate fields automatically. This saves you from the tedium 
of writing MKI$, CVS, LSET, and FIELD statements for each different data file.

QuickScreen also includes a screen capture TSR program that lets you import 
any text-mode screen from any program. Graphics QuickScreen includes a similar 
utility for graphics, saving the image in a standard .PCX file. Besides 
display screens and data-entry forms, Graphics QuickScreen lets you design 3D 
sizable buttons, menus, and scroll bars that look and operate just like 
Microsoft Visual Basic(tm). When a push button is clicked it actually 
depresses, just like the buttons in a real Windows program. Imagine being able 
to write programs that look just like Windows but with the blazing speed of 
DOS! This is the closest you can get to a true graphical user interface (GUI) 
but without having to program in Windows.

THE FULL MOON PHILOSOPHY

As with all our products, full source code is provided at no additional cost, 
so you can see how the routines were designed and even modify them if you 
want. We genuinely want you to understand how our libraries work and be able 
to learn from them. All of our products are reasonably priced and include free 
technical assistance, but they are licensed for use by only one person using 
one computer at a time. Royalty payments are not required when our routines 
are incorporated into your compiled applications. However, you may not 
distribute our source, object, or library files. If your customers need to 
rebuild your program, they will need their own copy of our product(s).

     "The documentation for Graphics QuickScreen is among the best I've seen.
     It is well organized and serves both as a good tutorial and an excellent
     reference guide...For those dissatisfied with Visual Basic for DOS's text
     mode interface, Graphics QuickScreen offers considerably more flexibility
     to the programmer. --Michael Yard, BASICPro, 4/93

THE BOTTOM LINE

QuickScreen and Graphics QuickScreen cost $149 (each), and they work with 
QuickBASIC 4.x, PDS 7.x, and VB/DOS. Add $8 for UPS ground shipping to US 
addresses only (no P.O. boxes); Connecticut residents must add 6.0% sales tax 
or show proof of tax-exempt status. Please call for overnight and foreign 
shipping costs. We accept checks, MasterCard, and VISA. We do accept purchase 
orders, but they must be accompanied by full payment.

QuickScreen(tm) and Graphics QuickScreen(tm) are trademarks of Crescent 
Software, Inc.
