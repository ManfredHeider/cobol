      *>*****************************************************
      *>                                                    *
      *> COBOL Vocabulary for Dialog Language               *
      *>                                                    *
      *> Copyright (C) 1994-2002,2011 Micro Focus.          *
      *>                      All Rights Reserved           *
      *>                                                    *
      *>                                                    *
      *>                                                    *
      *>                                                    *
      *>----------------------------------------------------*
      *>                                                    *
      *>                                                    *
      *>                                                    *
      *> Including this copyfile will allow selected        *
      *> Dialog System functions to be embedded in          *
      *> your COBOL program.                                *
      *>                                                    *
      *> The syntax, and number of parameters for each      *
      *> function is the same as in Dialog system.          *
      *> However the types of parameters allowed is not     *
      *> as extensive.                                      *
      *>                                                    *
      *> e.g. any function which returns an integer         *
      *>      value, i.e. GET-LIST-ITEM-STATE,              *
      *>      expects the return variable to be a           *
      *>      PIC 9(8) COMP-X data item.                    *
      *>                                                    *
      *>                                                    *
      *> Steps to use this Vocabulary                       *
      *> ----------------------------                       *
      *> 1)  From the Dialog System definition software     *
      *>     select 'File->Generate->Vocabulary Copybook'.  *
      *>                                                    *
      *>     This will generate a copyfile containing a     *
      *>     list of the ids for each named dialog          *
      *>     object.                                        *
      *>                                                    *
      *> 2)  Add the generated copyfile to your COBOL       *
      *>     program, in the WORKING-STORAGE SECTION.       *
      *>                                                    *
      *> 3)  Add this file, 'dslang.cpy', before the        *
      *>     IDENTIFICATION DIVISION of your COBOL          *
      *>     program.                                       *
      *>                                                    *
      *> 4)  Now add the Dialog Functions you require,      *
      *>     to your programs PROCEDURE DIVISION.           *
      *>                                                    *
      *>     e.g.  multiply rate    by total                *
      *>           REFRESH-OBJECT total-entry-field         *
      *>           SET-FOCUS total-entry-field              *
      *>                                                    *
      *> 5)  Compile your program including the directive   *
      *>     'MFOO'                                         *
      *>                                                    *
      *>*****************************************************

      $if ilgen set
       
       replace ==MOVE-WINDOW==            BY ==CALL "DSMovWin" USING==
          ==CREATE-WINDOW==          BY ==CALL "DSCreatWin" USING==                     
          ==SHOW-WINDOW==            BY ==CALL "DSShowWin" USING==
          ==SET-FOCUS==              BY ==CALL "DSSetFocus" USING==
          ==UNSHOW-WINDOW==          BY ==CALL "DSUnshowWin" USING== 
          ==DELETE-WINDOW==          BY ==CALL "DSDelWin" USING== 
          ==REFRESH-OBJECT==         BY ==CALL "DSRefreshObj" USING==
          ==CLEAR-OBJECT==           BY ==CALL "DSClearObj" USING==
          ==VALIDATE==               BY ==CALL "DSValidate" USING==
          ==GET-BUTTON-STATE==       BY ==CALL "DSGetButStat" USING==
          ==SET-BUTTON-STATE==       BY ==CALL "DSSetButStat" USING==
          ==TOGGLE-BUTTON-STATE==    BY ==CALL "DSTogButStat" USING==
          ==DISABLE-OBJECT==         BY ==CALL "DSDisObj" USING==
          ==ENABLE-OBJECT==          BY ==CALL "DSEnaObj" USING==
          ==DELETE-LIST-ITEM==       BY ==CALL "DSDelLstItm" USING==
          ==SET-SLIDER-RANGE==       BY ==CALL "DSSetSldrRang" USING==
          ==SET-PAGE-VALUE==         BY ==CALL "DSSetPagVal" USING==
          ==SET-LINE-VALUE==         BY ==CALL "DSSetLinVal" USING==
          ==SET-SLIDER-POS==         BY ==CALL "DSSetSldrPos" USING==
          ==SET-SLIDER-SIZE==        BY ==CALL "DSSetSldrSiz" USING==
          ==REPEAT-EVENT==           BY ==CALL "DSRepEvnt" USING==
          ==SET-TOP-LIST-ITEM==      BY ==CALL "DSSetTopLstItm" USING==
          ==SET-LIST-ITEM-STATE==    BY ==CALL "DSSetLstItmStat" USING==
          ==SET-AUTOSWIPE-STATE==    BY ==CALL "DSSetAutoSwStat" USING==
          ==SET-FIELD-LIMIT==        BY ==CALL "DSSetFldLim" USING==
          ==SET-PRIVILEGE==          BY ==CALL "DSSetPriv" USING== 
          ==SET-OCCURRENCE==         BY ==CALL "DSSetOccur" USING==
          ==COPY-PAGE==              BY ==CALL "DSCopPag" USING==
          ==DELETE-PAGE==            BY ==CALL "DSDelPag" USING==
          ==HIDE-OBJECT==            BY ==CALL "DSHideObj" USING==
          ==SHOW-OBJECT==            BY ==CALL "DSShowObj" USING==
          ==REFRESH-MENU==           BY ==CALL "DSRefMenu" USING==
          ==SET-FIRST-WINDOW==       BY ==CALL "DSSetFirstWin" USING==
          ==MAXIMIZE-WINDOW==        BY ==CALL "DSMaxWin" USING==
          ==MINIMIZE-WINDOW==        BY ==CALL "DSMinWin" USING==
          ==RESTORE-WINDOW==         BY ==CALL "DSRestWin" USING==
          ==GET-LIST-ITEM-STATE==    BY ==CALL "DSGetLstItmStat" USING==
          ==GET-OCCURRENCE==         BY ==CALL "DSGetOccur" USING==
          ==GET-DESKTOP-WINDOW==     BY ==CALL "DSGetDesktopWin" USING==      
          ==GET-SELECTED-LIST-ITEM== BY ==CALL "DSGetSelLstItm" USING==
          ==GET-TOP-LIST-ITTEM==     BY ==CALL "DSGetTopLstItm" USING==
          ==INSERT-LIST-ITEM==       BY ==CALL "DSInsLstItm" USING==
          ==INVOKE-MESSAGE-BOX==     BY ==CALL "DSInvokeMsgBox1" USING==
          ==MOVE-OBJECT-HANDLE==     BY ==CALL "DSMovObjHand" USING==
          ==SET-COLOR==              BY ==CALL "DSSetColor" USING==
          ==SET-MOUSE-SHAPE==        BY ==CALL "DSSetMouShape" USING==
          ==SET-OBJECT-LABEL==       BY ==CALL "DSSetObjLabel" USING==
          ==UPDATE-LIST-ITEM==       BY ==CALL "DSUpdLstItm" USING==
          .
       
      $else 
       *>
       *> MOVE-WINDOW window direction distance
       *>
       PROGRAM-ID. DSMovWin IS EXTERNAL.
CCC008*LINKAGE SECTION.
       01  Window-ID                    PIC 9(8) COMP-X.
       01  Direction                    PIC 9(8) COMP-X.
       01  Distance                     PIC 9(8) COMP-X.
CCC006 PROCEDURE DIVISION.
CCC007*PROCEDURE DIVISION USING Window-ID Direction Distance
CCC004* INVOKED AS ==MOVE-WINDOW <Window-ID> <Direction> <Distance> ==.
CCC004*END PROGRAM DSMovWin.

CCC004**>
CCC004**> CREATE-WINDOW window
CCC004**>
CCC004*PROGRAM-ID. DSCreatWin IS EXTERNAL.
CCC008*LINKAGE SECTION.
       01  Window-ID                    PIC 9(8) COMP-X.
CCC006 PROCEDURE DIVISION.
CCC007*PROCEDURE DIVISION USING Window-ID
CCC004* INVOKED AS ==CREATE-WINDOW <Window-ID> ==.
CCC004*END PROGRAM DSCreatWin.

CCC004**>
CCC004**> SHOW-WINDOW window
CCC004**>
CCC004*PROGRAM-ID. DSShowWin IS EXTERNAL.
CCC008*LINKAGE SECTION.
       01  Window-ID                    PIC 9(8) COMP-X.
CCC006 PROCEDURE DIVISION.
CCC007*PROCEDURE DIVISION USING Window-ID
CCC004* INVOKED AS ==SHOW-WINDOW <Window-ID> ==.
CCC004*END PROGRAM DSShowWin.

CCC004**>
CCC004**> SET-FOCUS object
CCC004**>
CCC004*PROGRAM-ID. DSSetFocus IS EXTERNAL.
CCC008*LINKAGE SECTION.
       01  Obj-ID                      PIC 9(8) COMP-X.
CCC006 PROCEDURE DIVISION.
CCC007*PROCEDURE DIVISION USING Obj-ID
CCC004* INVOKED AS ==SET-FOCUS <Obj-ID> ==.
CCC004*END PROGRAM DSSetFocus.

CCC004**>
CCC004**> UNSHOW-WINDOW window focus-target-object
CCC004**>
CCC004*PROGRAM-ID. DSUnshowWin IS EXTERNAL.
CCC008*LINKAGE SECTION.
       01  Window-ID                    PIC 9(8) COMP-X.
       01  Focus-Target-ID              PIC 9(8) COMP-X.
CCC006 PROCEDURE DIVISION.
CCC007*PROCEDURE DIVISION USING Window-ID Focus-Target-ID
CCC004* INVOKED AS ==UNSHOW-WINDOW <Window-ID> <Focus-Target-ID> ==.
CCC004*END PROGRAM DSUnshowWin.

CCC004**>
CCC004**> DELETE-WINDOW window focus-target-object
CCC004**>
CCC004*PROGRAM-ID. DSDelWin IS EXTERNAL.
CCC008*LINKAGE SECTION.
       01  Window-ID                    PIC 9(8) COMP-X.
       01  Focus-Target-ID              PIC 9(8) COMP-X.
CCC006 PROCEDURE DIVISION.
CCC007*PROCEDURE DIVISION USING Window-ID Focus-Target-ID
CCC004* INVOKED AS ==DELETE-WINDOW <Window-ID> <Focus-Target-ID> ==.
CCC004*END PROGRAM DSDelWin.

CCC004**>
CCC004**> REFRESH-OBJECT object
CCC004**>
CCC004*PROGRAM-ID. DSRefreshObj IS EXTERNAL.
CCC008*LINKAGE SECTION.
       01  Obj-ID                      PIC 9(8) COMP-X.
CCC006 PROCEDURE DIVISION.
CCC007*PROCEDURE DIVISION USING Obj-ID
CCC004* INVOKED AS ==REFRESH-OBJECT <Obj-ID> ==.
CCC004*END PROGRAM DSRefreshObj.

CCC004**>
CCC004**> CLEAR-OBJECT object
CCC004**>
CCC004*PROGRAM-ID. DSClearObj IS EXTERNAL.
CCC008*LINKAGE SECTION.
       01  Obj-ID                       PIC 9(8) COMP-X.
CCC006 PROCEDURE DIVISION.
CCC007*PROCEDURE DIVISION USING Obj-ID   
CCC004* INVOKED AS ==CLEAR-OBJECT <Obj-ID   > ==.
CCC004*end program DSClearObj.


CCC004**>
CCC004**> VALIDATE object
CCC004**>
CCC004*PROGRAM-ID. DSValidate IS EXTERNAL.
CCC008*LINKAGE SECTION.
       01  Obj-ID                       PIC 9(8) COMP-X.
CCC006 PROCEDURE DIVISION.
CCC007*PROCEDURE DIVISION USING Obj-ID   
CCC004* INVOKED AS ==VALIDATE <Obj-ID   > ==.
CCC004*END PROGRAM DSValidate.

CCC004**>
CCC004**> GET-BUTTON-STATE button state
CCC004**>
CCC004*PROGRAM-ID. DSGetButStat IS EXTERNAL.
CCC008*LINKAGE SECTION.
       01  Button-ID                    PIC 9(8) COMP-X.
       01  State-Return                 PIC 9(8) COMP-X.
CCC006 PROCEDURE DIVISION.
CCC007*PROCEDURE DIVISION USING Button-ID State-Return
CCC004* INVOKED AS ==GET-BUTTON-STATE <Button-ID> <State-Return> ==.
CCC004*END PROGRAM DSGetButStat.

CCC004**>
CCC004**> SET-BUTTON-STATE button state
CCC004**>
CCC004*PROGRAM-ID. DSSetButStat IS EXTERNAL.
CCC008*LINKAGE SECTION.
       01  Button-ID                    PIC 9(8) COMP-X.
       01  State-Flag                   PIC 9(8) COMP-X.
CCC006 PROCEDURE DIVISION.
CCC007*PROCEDURE DIVISION USING Button-ID State-Flag
CCC004* INVOKED AS ==SET-BUTTON-STATE <Button-ID> <State-Flag> ==.
CCC004*END PROGRAM DSSetButStat.

CCC004**>
CCC004**> TOGGLE-BUTTON-STATE button
CCC004**>
CCC004*PROGRAM-ID. DSTogButStat IS EXTERNAL.
CCC008*LINKAGE SECTION.
       01  Button-ID                    PIC 9(8) COMP-X.
CCC006 PROCEDURE DIVISION.
CCC007*PROCEDURE DIVISION USING Button-ID
CCC004* INVOKED AS ==TOGGLE-BUTTON-STATE <Button-ID> ==.
CCC004*END PROGRAM DSTogButStat.

CCC004**>
CCC004**> DISABLE-OBJECT object
CCC004**>
CCC004*PROGRAM-ID. DSDisObj IS EXTERNAL.
CCC008*LINKAGE SECTION.
       01  Obj-ID                       PIC 9(8) COMP-X.
CCC006 PROCEDURE DIVISION.
CCC007*PROCEDURE DIVISION USING Obj-ID   
CCC004* INVOKED AS ==DISABLE-OBJECT <Obj-ID   > ==.
CCC004*END PROGRAM DSDisObj.

CCC004**>
CCC004**> ENABLE-OBJECT object
CCC004**>
CCC004*PROGRAM-ID. DSEnaObj IS EXTERNAL.
CCC008*LINKAGE SECTION.
       01  Obj-ID                       PIC 9(8) COMP-X.
CCC006 PROCEDURE DIVISION.
CCC007*PROCEDURE DIVISION USING Obj-ID   
CCC004* INVOKED AS ==ENABLE-OBJECT <Obj-ID   > ==.
CCC004*END PROGRAM DSEnaObj.

CCC004**>
CCC004**> DELETE-LIST-ITEM list-or-selection-box position count
CCC004**>
CCC004*PROGRAM-ID. DSDelLstItm IS EXTERNAL.
CCC008*LINKAGE SECTION.
       01  Box-ID                       PIC 9(8) COMP-X.
       01  Pos                          PIC 9(8) COMP-X.
       01  Cnt                          PIC 9(8) COMP-X.
CCC006 PROCEDURE DIVISION.
CCC007*PROCEDURE DIVISION USING Box-ID Pos Cnt
CCC004* INVOKED AS ==DELETE-LIST-ITEM <Box-ID> <Pos> <Cnt> ==.
CCC004*END PROGRAM DSDelLstItm.

CCC004**>
CCC004**> SET-SLIDER-RANGE scroll-bar min max
CCC004**>
CCC004*PROGRAM-ID. DSSetSldrRang IS EXTERNAL.
CCC008*LINKAGE SECTION.
       01  Scroll-ID                    PIC 9(8) COMP-X.
       01  Min-R                        PIC 9(8) COMP-X.
       01  Max-R                        PIC 9(8) COMP-X.
CCC006 PROCEDURE DIVISION.
CCC007*PROCEDURE DIVISION USING Scroll-ID Min-R Max-R
CCC004* INVOKED AS ==SET-SLIDER-RANGE <Scroll-ID> <Min-R> <Max-R> ==.
CCC004*end program DSSetSldrRang.

CCC004**>
CCC004**> SET-PAGE-VALUE scroll-bar page-size
CCC004**>
CCC004*PROGRAM-ID. DSSetPagVal IS EXTERNAL.
CCC008*LINKAGE SECTION.
       01  Scroll-ID                    PIC 9(8) COMP-X.
       01  Page-Value                   PIC 9(8) COMP-X.
CCC006 PROCEDURE DIVISION.
CCC007*PROCEDURE DIVISION USING Scroll-ID Page-Value
CCC004* INVOKED AS ==SET-PAGE-VALUE <Scroll-ID> <Page-Value> ==.
CCC004*END PROGRAM DSSetPagVal.

CCC004**>
CCC004**> SET-LINE-VALUE scroll-bar line-size
CCC004**>
CCC004*PROGRAM-ID. DSSetLinVal IS EXTERNAL.
CCC008*LINKAGE SECTION.
       01  Scroll-ID                    PIC 9(8) COMP-X.
       01  Line-Size                    PIC 9(8) COMP-X.
CCC006 PROCEDURE DIVISION.
CCC007*PROCEDURE DIVISION USING Scroll-ID Line-SIze
CCC004* INVOKED AS ==SET-LINE-VALUE <Scroll-ID> <Line-Size> ==.
CCC004*END PROGRAM DSSetLinVal.

CCC004**>
CCC004**> SET-SLIDER-POSITION scroll-bar position
CCC004**>
CCC004*PROGRAM-ID. DSSetSldrPos IS EXTERNAL.
CCC008*LINKAGE SECTION.
       01  Scroll-ID                    PIC 9(8) COMP-X.
       01  Pos                          PIC 9(8) COMP-X.
CCC006 PROCEDURE DIVISION.
CCC007*PROCEDURE DIVISION USING Scroll-ID Pos
CCC004* INVOKED AS ==SET-SLIDER-POSITION <Scroll-ID> <Pos> ==.
CCC004*END PROGRAM DSSetSldrPos.

CCC004**>
CCC004**> SET-SLIDER-SIZE scroll-bar size
CCC004**>
CCC004*PROGRAM-ID. DSSetSldrSiz IS EXTERNAL.
CCC008*LINKAGE SECTION.
       01  Scroll-ID                    PIC 9(8) COMP-X.
       01  Siz                          PIC 9(8) COMP-X.
CCC006 PROCEDURE DIVISION.
CCC007*PROCEDURE DIVISION USING Scroll-ID Siz
CCC004* INVOKED AS ==SET-SLIDER-SIZE <Scroll-ID> <Siz> ==.
CCC004*END PROGRAM DSSetSldrSiz.


CCC004**>
CCC004**> REPEAT-EVENT
CCC004**>
CCC004*PROGRAM-ID. DSRepEvnt IS EXTERNAL.
       PROCEDURE DIVISION
        INVOKED AS ==REPEAT-EVENT ==.
       END PROGRAM DSRepEvnt.

       *>
       *> SET-TOP-LIST-ITEM list-box position
       *>
       PROGRAM-ID. DSSetTopLstItm IS EXTERNAL.
CCC008*LINKAGE SECTION.
       01 listbox-id                   PIC 9(8) COMP-X.
       01 listbox-pos                  PIC 9(8) COMP-X.
CCC006 PROCEDURE DIVISION.
CCC007*PROCEDURE DIVISION USING listbox-id listbox-pos
CCC004* INVOKED AS ==SET-TOP-LIST-ITEM <listbox-id> <listbox-pos> ==.
CCC004*END PROGRAM DSSetTopLstItm.

CCC004**>
CCC004**> SET-LIST-ITEM-STATE list-box state position
CCC004**>
CCC004*PROGRAM-ID. DSSetLstItmStat IS EXTERNAL.
CCC008*LINKAGE SECTION.
       01 listbox-id                   PIC 9(8) COMP-X.
       01 state                        PIC 9(8) COMP-X.
       01 listbox-pos                  PIC 9(8) COMP-X.
CCC006 PROCEDURE DIVISION.
CCC007*PROCEDURE DIVISION USING listbox-id state listbox-pos
CCC004* INVOKED AS ==SET-LIST-ITEM-STATE <listbox-id> <state> 
CCC004*    <listbox-pos> ==.
CCC004*END PROGRAM DSSetLstItmStat.

CCC004**>
CCC004**> SET-AUTOSWIPE-STATE entry-field state
CCC004**>
CCC004*PROGRAM-ID. DSSetAutoSwStat IS EXTERNAL.
CCC008*LINKAGE SECTION.
       01 entry-field                  PIC 9(8) COMP-X.
       01 state                        PIC 9(8) COMP-X.
CCC006 PROCEDURE DIVISION.
CCC007*PROCEDURE DIVISION USING entry-field state
CCC004* INVOKED AS ==SET-AUTOSWIPE-STATE <entry-field> <state> ==.
CCC004*END PROGRAM DSSetAutoSwStat.

CCC004**>
CCC004**> SET-FIELD-LIMIT entry-field len
CCC004**>
CCC004*PROGRAM-ID. DSSetFldLim IS EXTERNAL.
CCC008*LINKAGE SECTION.
       01 entry-field                  PIC 9(8) COMP-X.
       01 len                          PIC 9(8) COMP-X.
CCC006 PROCEDURE DIVISION.
CCC007*PROCEDURE DIVISION USING entry-field len
CCC004* INVOKED AS ==SET-FIELD-LIMIT <entry-field> <len> ==.
CCC004*END PROGRAM DSSetFldLim.

CCC004**>
CCC004**> SET-PRIVILEGE privilege-level
CCC004**>
CCC004*PROGRAM-ID. DSSetPriv IS EXTERNAL.
CCC008*LINKAGE SECTION.
       01 privilege-level              PIC 9(8) COMP-X.
CCC006 PROCEDURE DIVISION.
CCC007*PROCEDURE DIVISION USING privilege-level
CCC004* INVOKED AS ==SET-PRIVILEGE <privilege-level> ==.
CCC004*END PROGRAM DSSetPriv.

CCC004**>
CCC004**> SET-OCCURRENCE object occurrence
CCC004**>
CCC004*PROGRAM-ID. DSSetOccur IS EXTERNAL.
CCC008*LINKAGE SECTION.
       01 obj-id                       PIC 9(8) COMP-X.
       01 occurence                    PIC 9(8) COMP-X.
CCC006 PROCEDURE DIVISION.
CCC007*PROCEDURE DIVISION USING obj-id occurence
CCC004* INVOKED AS ==SET-OCCURRENCE <obj-id> <occurence> ==.
CCC004*END PROGRAM DSSetOccur.

CCC004**>
CCC003**> COPY-PAGE page position occurence
       *>
       PROGRAM-ID. DSCopPag IS EXTERNAL.
CCC008*LINKAGE SECTION.
       01 page-id                      PIC 9(8) COMP-X.
       01 pos                          PIC 9(8) COMP-X.
       01 occurence                    PIC 9(8) COMP-X.
CCC006 PROCEDURE DIVISION.
CCC007*PROCEDURE DIVISION USING page-id pos occurence
CCC003* INVOKED AS ==COPY-PAGE <page-id> <pos> <occurence> ==.
       END PROGRAM DSCopPag.

       *>
       *> DELETE-PAGE page
       *>
       PROGRAM-ID. DSDelPag IS EXTERNAL.
CCC008*LINKAGE SECTION.
       01 page-id                      PIC 9(8) COMP-X.
CCC006 PROCEDURE DIVISION.
CCC007*PROCEDURE DIVISION USING page-id
CCC004* INVOKED AS ==DELETE-PAGE <page-id> ==.
CCC004*END PROGRAM DSDelPag.

CCC004**>
CCC004**> HIDE-OBJECT object
CCC004**>
CCC004*PROGRAM-ID. DSHideObj IS EXTERNAL.
CCC008*LINKAGE SECTION.
       01 obj-id                       PIC 9(8) COMP-X.
CCC006 PROCEDURE DIVISION.
CCC007*PROCEDURE DIVISION USING obj-id
CCC004* INVOKED AS ==HIDE-OBJECT <obj-id> ==.
CCC004*END PROGRAM DSHideObj.

CCC004**>
CCC004**> SHOW-OBJECT object
CCC004**>
CCC004*PROGRAM-ID. DSShowObj IS EXTERNAL.
CCC008*LINKAGE SECTION.
       01 obj-id                       PIC 9(8) COMP-X.
CCC006 PROCEDURE DIVISION.
CCC007*PROCEDURE DIVISION USING obj-id
CCC004* INVOKED AS ==SHOW-OBJECT <obj-id> ==.
CCC004*END PROGRAM DSShowObj.

CCC004**>
CCC004**> REFRESH-MENU window
CCC004**>
CCC004*PROGRAM-ID. DSRefMenu IS EXTERNAL.
CCC008*LINKAGE SECTION.
       01 window-id                    PIC 9(8) COMP-X.
CCC006 PROCEDURE DIVISION.
CCC007*PROCEDURE DIVISION USING window-id
CCC004* INVOKED AS ==REFRESH-MENU <window-id> ==.
CCC004*END PROGRAM DSRefMenu.

CCC004**>
CCC004**> SET-FIRST-WINDOW window
CCC004**>
CCC004*PROGRAM-ID. DSSetFirstWin IS EXTERNAL.
CCC008*LINKAGE SECTION.
       01 window-id                    PIC 9(8) COMP-X.
CCC006 PROCEDURE DIVISION.
CCC007*PROCEDURE DIVISION USING window-id
CCC004* INVOKED AS ==SET-FIRST-WINDOW <window-id> ==.
CCC004*END PROGRAM DSSetFirstWin.

CCC004**>
CCC004**> MAXIMIZE-WINDOW window
CCC004**>
CCC004*PROGRAM-ID. DSMaxWin IS EXTERNAL.
CCC008*LINKAGE SECTION.
       01 window-id                    PIC 9(8) COMP-X.
CCC006 PROCEDURE DIVISION.
CCC007*PROCEDURE DIVISION USING window-id
CCC004* INVOKED AS ==MAXIMIZE-WINDOW <window-id> ==.
CCC004*END PROGRAM DSMaxWin.

CCC004**>
CCC004**> MINIMIZE-WINDOW
CCC004**>
CCC004*PROGRAM-ID. DSMinWin IS EXTERNAL.
CCC008*LINKAGE SECTION.
       01 window-id                    PIC 9(8) COMP-X.
CCC006 PROCEDURE DIVISION.
CCC007*PROCEDURE DIVISION USING window-id
CCC004* INVOKED AS ==MINIMIZE-WINDOW <window-id> ==.
CCC004*END PROGRAM DSMinWin.

CCC004**>
CCC004**> RESTORE-WINDOW
CCC004**>
CCC004*PROGRAM-ID. DSRestWin IS EXTERNAL.
CCC008*LINKAGE SECTION.
       01 window-id                    PIC 9(8) COMP-X.
CCC006 PROCEDURE DIVISION.
CCC007*PROCEDURE DIVISION USING window-id
CCC004* INVOKED AS ==RESTORE-WINDOW <window-id> ==.
CCC004*END PROGRAM DSRestWin.


CCC004**>
CCC004**> GET-LIST-ITEM-STATE listbox state position
CCC004**>
CCC004*PROGRAM-ID. DSGetLstItmStat IS EXTERNAL.
CCC008*LINKAGE SECTION.
       01 ObjId                        PIC 9(8) COMP-X.
       01 ObjState                     PIC 9(8) COMP-X.
       01 ObjPos                       PIC 9(8) COMP-X.
CCC006 PROCEDURE DIVISION.
CCC007*PROCEDURE DIVISION USING objid objstate objpos
CCC004*INVOKED AS ==GET-LIST-ITEM-STATE <objid> <objstate> <objpos> ==.
CCC004*END PROGRAM DSGetLstItmStat.


CCC004**>
CCC004**> GET-OCCURRENCE page occurence
CCC004**>
CCC004*PROGRAM-ID. DSGetOccur IS EXTERNAL.
CCC008*LINKAGE SECTION.
       01 ObjId                        PIC 9(8) COMP-X.
       01 Occurence                    PIC 9(8) COMP-X.
CCC006 PROCEDURE DIVISION.
CCC007*PROCEDURE DIVISION USING objid occurence
CCC004*INVOKED AS ==GET-OCCURRENCE <objid> <occurence> ==.
CCC004*END PROGRAM DSGetOccur.


CCC004**>
CCC004**> GET-DESKTOP-WINDOW object-id
CCC004**>
CCC004*PROGRAM-ID. DSGetDesktopWin IS EXTERNAL.
CCC008*LINKAGE SECTION.
       01 ObjId                        PIC 9(8) COMP-X.
CCC006 PROCEDURE DIVISION.
CCC007*PROCEDURE DIVISION USING objid
CCC004*INVOKED AS ==GET-DESKTOP-WINDOW <objid> ==.
CCC004*END PROGRAM DSGetDesktopWin.


CCC004**>
CCC004**> GET-SELECTED-LIST-ITEM listbox-id found-position start-position
CCC004**>
CCC004*PROGRAM-ID. DSGetSelLstItm IS EXTERNAL.
CCC008*LINKAGE SECTION.
       01 ObjId                        PIC 9(8) COMP-X.
       01 found-pos                    PIC 9(8) COMP-X.
       01 start-pos                    PIC 9(8) COMP-X.
CCC006 PROCEDURE DIVISION.
CCC007*PROCEDURE DIVISION USING objid found-pos start-pos
CCC004*INVOKED AS ==GET-SELECTED-LIST-ITEM 
      - <objid> <found-pos> <start-pos> ==.
CCC004*END PROGRAM DSGetSelLstItm.


CCC004**>
CCC004**> GET-TOP-LIST-ITEM listbox position
CCC004**>
CCC004*PROGRAM-ID. DSGetTopLstItm IS EXTERNAL.
CCC008*LINKAGE SECTION.
       01 ObjId                        PIC 9(8) COMP-X.
       01 Pos                          PIC 9(8) COMP-X.
CCC006 PROCEDURE DIVISION.
CCC007*PROCEDURE DIVISION USING objid pos
CCC004*INVOKED AS ==GET-TOP-LIST-ITEM <objid> <pos> ==.
CCC004*END PROGRAM DSGetTopLstItm.


CCC004**>
CCC004**> INSERT-LIST-ITEM listbox text position
CCC004**>
CCC004*PROGRAM-ID. DSInsLstItm IS EXTERNAL.
CCC008*LINKAGE SECTION.
       01 ObjId                        PIC 9(8) COMP-X.
       01 DataItem                     PIC x(255).
       01 Pos                          PIC 9(8) COMP-X.
CCC006 PROCEDURE DIVISION.
CCC007*PROCEDURE DIVISION USING objid dataitem DELIMITED pos
CCC004*INVOKED AS ==INSERT-LIST-ITEM <objid> <dataitem> <pos> ==.
CCC004*END PROGRAM DSInsLstItm.


CCC004**>
CCC004**> INVOKE-MESAGE-BOX message-box text return-value
CCC004**>
CCC004*PROGRAM-ID. DSInvokeMsgBox1 IS EXTERNAL.
CCC008*LINKAGE SECTION.
       01 mbox                         PIC 9(8) COMP-X.
       01 mtext                        PIC x(255).
       01 ret-value                    PIC 9(8) COMP-X.
CCC006 PROCEDURE DIVISION.
CCC007*PROCEDURE DIVISION USING mbox mtext DELIMITED ret-value
CCC004*INVOKED AS ==INVOKE-MESSAGE-BOX <mbox> <mtext> <ret-value> ==.
CCC004*END PROGRAM DSInvokeMsgBox1.


CCC004**>
CCC004**> MOVE-OBJECT-HANDLE object returned-handle
CCC004**>
CCC004*PROGRAM-ID. DSMovObjHand IS EXTERNAL.
CCC008*LINKAGE SECTION.
       01 obj                          PIC 9(8) COMP-X.
       01 handle                       PIC 9(8) COMP-X.
CCC006 PROCEDURE DIVISION.
CCC007*PROCEDURE DIVISION USING obj handle
CCC004*INVOKED AS ==MOVE-OBJECT-HANDLE <obj> <handle> ==.
CCC004*END PROGRAM DSMovObjHand.


CCC004**>
CCC004**> SET-COLOR object foreground-color background-color
CCC004**>
CCC004*PROGRAM-ID. DSSetColor IS EXTERNAL.
CCC008*LINKAGE SECTION.
       01 obj                          PIC 9(8) COMP-X.
       01 fground                      PIC x(255).
       01 bground                      PIC x(255).
CCC006 PROCEDURE DIVISION.
CCC007*PROCEDURE DIVISION USING obj fground DELIMITED bground DELIMITED
CCC004*INVOKED AS ==SET-COLOR <obj> <fground> <bground> ==.
CCC004*END PROGRAM DSSetColor.


CCC004**>
CCC004**> SET-MOUSE-SHAPE object mouse-shape
CCC004**>
CCC004*PROGRAM-ID. DSSetMouShape IS EXTERNAL.
CCC008*LINKAGE SECTION.
       01 obj                          PIC 9(8) COMP-X.
       01 shape                        PIC x(255).
CCC006 PROCEDURE DIVISION.
CCC007*PROCEDURE DIVISION USING obj shape DELIMITED
CCC004*INVOKED AS ==SET-MOUSE-SHAPE <obj> <shape> ==.
CCC004*END PROGRAM DSSetMouShape.


CCC004**>
CCC004**> SET-OBJECT-LABEL object label-text
CCC004**>
CCC004*PROGRAM-ID. DSSetObjLabel IS EXTERNAL.
CCC008*LINKAGE SECTION.
       01 obj                          PIC 9(8) COMP-X.
       01 otext                        PIC x(255).
CCC006 PROCEDURE DIVISION.
CCC007*PROCEDURE DIVISION USING obj otext DELIMITED
CCC004*INVOKED AS ==SET-OBJECT-LABEL <obj> <otext> ==.
CCC004*END PROGRAM DSSetObjLabel.


CCC004**>
CCC004**> UPDATE-LIST-ITEM listbox item-text position
CCC004**>
CCC004*PROGRAM-ID. DSUpdLstItm IS EXTERNAL.
CCC008*LINKAGE SECTION.
       01 obj                          PIC 9(8) COMP-X.
       01 itext                        PIC x(255).
       01 pos                          PIC 9(8) COMP-X.
CCC006 PROCEDURE DIVISION.
CCC007*PROCEDURE DIVISION USING obj itext DELIMITED pos
CCC004*INVOKED AS ==UPDATE-LIST-ITEM <obj> <itext> <pos> ==.
CCC004*END PROGRAM DSUpdLstItm.
      $end
