


#include once "fbgfx.bi"

#if __FB_LANG__ = "fb"
Using FB '' Scan code constants are stored in the FB namespace in lang FB
#endif

#if __FB_LANG__ = "qb"
#define EXTCHAR Chr$(0)
#else
#define EXTCHAR Chr(255)
#endif

#define RGBA_R( c ) ( CUInt( c ) Shr 16 And 255 )
#define RGBA_G( c ) ( CUInt( c ) Shr  8 And 255 )
#define RGBA_B( c ) ( CUInt( c )        And 255 )
#define RGBA_A( c ) ( CUInt( c ) Shr 24         )

Const NULL As Any Ptr = 0
Const ConvertMagicPink as integer = 0 ' 0 = no, 1 = yes

    dim shared As any ptr CurrentImage
    dim shared as string filename, file1, file2
    dim As Integer Image_Width, Image_Height, Image_Pitch, ibypp, isize
    dim Image_Pixels As Any ptr
    
    dim as integer NumOfColors
    dim as ulong colors(10000)
    dim as ulong PixelColor
    
    dim shared as ubyte Convert1(16777216,1)  ' max size = 4096x4096

    dim shared as string datastring(4096)

'====================================================  Load BMP Routine
Function bmp_load( ByRef filename As Const String ) As Any Ptr

    Dim As Integer filenum, bmpwidth, bmpheight
    Dim As Any Ptr img

                  '' open BMP file
    filenum = FreeFile()
    If Open( filename For Binary Access Read As #filenum ) <> 0 Then Return NULL
                  '' retrieve BMP dimensions
    Get #filenum, 19, bmpwidth
    Get #filenum, 23, bmpheight
    Close #filenum
                  '' create image with BMP dimensions
    img = ImageCreate( bmpwidth, Abs(bmpheight) )
    If img = NULL Then Return NULL
                  '' load BMP file into image buffer
    If BLoad( filename, img ) <> 0 Then ImageDestroy( img ): Return NULL
    Return img
End Function
'====================================================

    screenres 640,480,32

    file1 = "redPenguin1" : file2 = ".bmpx" : filename = file1 + file2
    CurrentImage = bmp_load(filename)
    if CurrentImage = NULL then beep
    
    ' get the info from the image
    If 0 <> imageinfo(CurrentImage, Image_Width, Image_Height, ibypp, Image_Pitch, Image_Pixels,isize) Then _
            Print "unable to retrieve image information." : beep : Sleep : End

    cls
    'make screen big gray box
    line(0,0)-(639,479), rgb(32,32,32),BF
    put (100,100),CurrentImage, pset
    put (300,100),CurrentImage, trans
    locate 2,2 : print using "Image Size is (### x ###)";Image_Width; Image_Height

    
    'for y = 0 to Image_Height - 1
        'for x = 0 to Image_Width - 1
            PixelColor = point(0,0,CurrentImage)
            locate 4,2 : print using "Color (r### , g### , b### , a###)";RGBA_R(PixelColor);RGBA_G(PixelColor);RGBA_B(PixelColor);RGBA_A(PixelColor);

' analyze Image
    NumOfColors = 0
    dim as integer CurPixelNumber = 0
    
    for y as integer = 0 to Image_Height - 1
        for x as integer = 0 to Image_Width - 1
            PixelColor = point(x,y,CurrentImage)
            ' is it already in table?
            dim as integer IsInTable = 0
            if NumOfColors > 0 then
                for c as integer = 1 to NumOfColors
                    if Colors(c) = PixelColor then IsInTable = c  'should only 'hit' once, else there are two of the same colors in table....problem
                next c
            end if
            if IsInTable = 0 then
                NumOfColors += 1
                Colors(NumOfColors) = PixelColor
                IsInTable = NumOfColors
            end if
            ' right now, IsInTable = index to color of PixelColor
            
            ' put in datastring as test only!
            'datastring(y) = datastring(y) + chr(34+IsInTable)
            
            '
            ' Now add it to the Convert1 array
            ' Convert1(c,0) = color index
            ' Convert1(c,1) = number of sequential pixels of same color
            '
            if (Convert1(CurPixelNumber,0) = IsInTable) and (CurPixelNumber > 1) and (Convert1(CurPixelNumber,1) < 127) then
                ' if same color as the last pixel AND not the first pixel AND there are less than 128 pixels same color... then inc num of pixels of that color... for RLE
                Convert1(CurPixelNumber,1) += 1
            else
                ' this pixel is different color from the previous one...or is the first pixel or there are more than 128 same color pixels in sequence
                CurPixelNumber += 1
                Convert1(CurPixelNumber,1) = 1
                Convert1(CurPixelNumber,0) = IsInTable
            end if
        next x
    next y
    
    ' now take the Convert1 array and make it into strings with RLE
    ' RLE starts with special character 32 (space) and is encoded: <space><pixelcolor><repetitions> with each being 1 character or byte
    '
    dim as integer DataLineLength = 0, NumOfRows = 1
    for c as integer = 1 to CurPixelNumber
        if DataLineLength > 80 then
            NumOfRows += 1
            DataLineLength = 0
        end if
        
        ' is it RLE?
        if Convert1(c,1) > 3 then   'shall we RLE encode these pixels? (only saves space if there are more than 3 pixels of same color due to RLE overhead
            ' put in string <space><pixelcolor><repetitions>
            ' right now 'repetitions' is limited to the same 7 bit limitations as the Index color is...
            datastring(NumOfRows) = datastring(NumOfRows) + chr(32) + chr(34+ Convert1(c,0)) + chr(34+  Convert1(c,1))
            DataLineLength += 3
        else
            ' put the 1 to 3 same color pixels
            for c1 as integer = 1 to Convert1(c,1)
                ' add Convert1(c,0) to string
                datastring(NumOfRows) = datastring(NumOfRows) + chr(34+ Convert1(c,0))
                DataLineLength += 1
            next c1
        end if
    next c

    locate 6,2 : print using "Colors (####)";NumOfColors;

' for testing output as we go so far...
filename = file1 + ".txt"
Open filename For Output As #1
    write #1, Image_Width, Image_Height, NumOfRows
    write #1, NumOfColors
    for c as integer = 1 to NumOfColors
        write #1, hex(Colors(c))
    next c
    
'    for row as integer = 0 to Image_Height - 1   'this was for non-RLE
'        Write #1, datastring(row)
'    next row

    for row as integer = 1 to NumOfRows  'this does correct # of Rows with RLE
        Write #1, datastring(row)
    next row

Close #1

'
' READ file testing...
'

' first, clear out current data...
Image_Width = 0 : Image_Height=0
    for c as integer = 1 to NumOfColors
        Colors(c) = 0
    next c
NumOfColors = 0
    for row as integer = 1 to NumOfRows  'this does correct # of Rows with RLE
        datastring(row) =""
    next row
NumOfRows = 0

' need to check this part to see if reading in colors and data statements correctly!!!!!
dim as string test
filename = file1 + ".txt"
Open filename For input As #1
    input #1, Image_Width, Image_Height, NumOfRows
    input #1, NumOfColors
    print
    for c as integer = 1 to NumOfColors
        test = ""
        input #1, test
        Colors(c) = CULNG("&H"+test)  ' convert the hex string back into a ulong value...
        print test
    next c

    for row as integer = 1 to NumOfRows
        input #1, datastring(row)
    next row
Close #1

'
' DECODE RLE string testing...  NOT WORKING YET!!!!
'
dim as integer PixelPosition = 0, CurStringPos = 1, y, x

    for row as integer = 1 to NumOfRows
        dim as integer toTemp = len(datastring(row)), stepTemp = 1, char = 1
        Do While IIf(stepTemp >= 0, char <= toTemp, char >= toTemp)
            test = mid(datastring(row), char, 1)
            if test = " " then   'is it RLE encoded pixels?
                PixelColor = Colors(asc(mid(datastring(row), char+1, 1)) - 34)
                for p as integer = 1 to (Cint(mid(datastring(row), char+2, 1)) - 34)
                    y = int(PixelPosition/Image_Width)
                    x = PixelPosition - (y*Image_Width)
                    pset (100+x, 250+y), Pixelcolor
                    PixelPosition += 1
                next p
                char = char + 3
                
            else  'is just a regular old pixel
                PixelColor = colors(asc(test) - 34)
                y = int(PixelPosition/Image_Width)
                x = PixelPosition - (y*Image_Width)
                pset (100+x, 250+y), Pixelcolor
                PixelPosition += 1
                char = char + 1
            end if
        loop
    next row



'' for testing output as we go so far...
'filename = "ascii.txt"
'Open filename For Output As #2
'    for row as integer = 1 to 128
'        Write #2, row,chr(34+row)
'    next row
'
'Close #2


    

    sleep
    end
