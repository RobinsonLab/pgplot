/* Generated by the NeXT Project Builder 
   NOTE: Do NOT change this file -- Project Builder maintains it.
*/

#import <appkit/Application.h>

void main(int argc, char *argv[]) {

    [Application new];
    if ([NXApp loadNibSection:"pgview.nib" owner:NXApp withNames:NO])
	    [NXApp run];
	    
    [NXApp free];
    exit(0);
}
