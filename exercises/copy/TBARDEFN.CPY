      *>----------------------------------------------------------------
      *> Menu bar and toolbar definitions for tbards.gs toolbar
      *> demonstration screenset.
      *>----------------------------------------------------------------

      * Name of resource file containing toolbar button bitmaps.
      * Please refer to the chapter "Using the Control Templates" in the
      * on-line book Dialog System User Guide for information on
      * creating a resource DLL.
       78 resourceDllName          value z"tbar.dll".

          copy "tbresid.cpy". *> Resource IDs for toolbar button bitmaps

      *>----------------------------------------------------------------
      *> Menu definitions.
      *>----------------------------------------------------------------

      *>----------------------------------------------------------------
      * Menu item types.
      * Each menu item added to the window can be one of:
      *    - sub menu
      *    - menu item
      *    - separator
      *
      * In addition, any of the above may be the last item within
      * a sub menu or the whole menu structure on the window.
      *
      * The menu type after the menu object reference is used to store 
      * the type of the menu item.
      *
      * The first 3 bits in the menu type are defined as follows:
      *
      *    bit 0 set   - indicates a sub menu (has children)
      *          unset - indicates a menu item
      *
      *    bit 1 set   - indicates last item in sub menu or window
      *                                                   (last sibling)
      *          unset - indicates not last item in sub menu or window
      *
      *    bit 2 set   - indicates a separator
      *          unset - indicates not a separator
      *
       78 typeSibling                  value 0.
       78 typeHasChildren              value 1.
       78 typeLastSibling              value 2.
       78 typeLastSiblingHasChildren   value typeLastSibling +
                                             typeHasChildren.
       78 typeSeparator                value 4.
       78 typeLastSiblingSeparator     value typeLastSibling +
                                             typeSeparator.
      *>----------------------------------------------------------------

      *>----------------------------------------------------------------
      * Other constants used by menu definition.
       78 noAcceleratorKeyDefined      value 0.

      * Virtual key with control key pressed. This is used to define
      * an accelerator key that has a virtual key code and needs to
      * have the ctrl key pressed.
       78 KS-Virt-Ctrl                 value KS-VIRTUAL-KEY +
                                             KS-CTRL-PRESSED.

      * The maximum number of menu items that can be stored within
      * the menu data table.
       78 maxMenuItems             value 100.

      * Maximum string length of callback entry point names.
       78 mCallbackSize            value 32.

      * Maximum length of menu item text and hint text combined.
       78 mStringSize              value 87.

      * Size of each menu record (one record per menu item).
       78 mRecSize                 value 128.
      *>----------------------------------------------------------------

      *>----------------------------------------------------------------
      *> To change the menu defined below, use the existing menu records
      *> as an example to change the menu structure, and if required
      *> add further menu records for any additional menu items.
      *>----------------------------------------------------------------

      ******************************************************************
      * NOTE - Each menu item record below must be mRecSize bytes.
      * ----   Unpredictable results may occur at run time if this
      *        table does not conform to the defined record structure.
      ******************************************************************
      
       01 mData.
       78 mDataStart               value NEXT.
      
      ***>>> Start USER MENU DATA <<<***
      *>----------------------------------------------------------------
      *   Menu item object reference.
          03   object reference.    *> Menu Index 01    

      *   Type of menu item.
          03   pic x comp-5         value typeHasChildren.

      *   Accelerator key definition (see keys.cpy for details).
          03   pic x(4) comp-5      value noAcceleratorKeyDefined.

      *   Name of callback to be invoked when menu item selected.
          03   pic x(mCallbackSize) value SPACES.

      *   Null terminated menu text followed by optional null
      *   terminated menu hint text.
      *   The whole string must be terminated by a further null.
          03   pic x(mStringSize)   value z"&File" & x"00".
      *>----------------------------------------------------------------
          03   object reference.    *> Menu Index 02
          03   pic x comp-5         value typeSibling.
          03   pic x comp-5         value KS-Virt-Ctrl.
          03   pic x                value "N".
          03   pic xx.
          03   pic x(mCallbackSize) value ProgramID & "OnItemSelected ".
          03   pic x(mStringSize)   value z"&New" &
                                    z"Create something new" & x"00".
      *>----------------------------------------------------------------
          03   object reference.    *> Menu Index 03
          03   pic x comp-5         value typeSibling.
          03   pic x comp-5         value KS-Virt-Ctrl.
          03   pic x                value "O".
          03   pic xx.
          03   pic x(mCallbackSize) value ProgramID & "OnItemSelected ".
          03   pic x(mStringSize)   value z"&Open" &
                                    z"Open something" & x"00".
      *>----------------------------------------------------------------
          03   object reference.    *> Menu Index 04
          03   pic x comp-5         value typeSeparator.
          03   pic x(4) comp-5      value noAcceleratorKeyDefined.
          03   pic x(mCallbackSize) value SPACES.
          03   pic x(mStringSize)   value x"0000".
      *>----------------------------------------------------------------
          03   object reference.    *> Menu Index 05
          03   pic x comp-5         value typeLastSibling.
          03   pic x(4) comp-5      value noAcceleratorKeyDefined.
          03   pic x(mCallbackSize) value ProgramID & "OnItemSelected ".
          03   pic x(mStringSize)   value z"E&xit" &
                                    z"Quit this demonstration" & x"00".
      *>----------------------------------------------------------------
          03   object reference.    *> Menu Index 06
          03   pic x comp-5         value typeHasChildren.
          03   pic x(4) comp-5      value noAcceleratorKeyDefined.
          03   pic x(mCallbackSize) value SPACES.
          03   pic x(mStringSize)   value z"&Colors" & x"00".
      *>----------------------------------------------------------------
          03   object reference.    *> Menu Index 07
          03   pic x comp-5         value typeSibling.
          03   pic x(4) comp-5      value noAcceleratorKeyDefined.
          03   pic x(mCallbackSize) value SPACES.
          03   pic x(mStringSize)   value z"&Purple" &
                                    z"This does nothing" & x"00".
      *>----------------------------------------------------------------
          03   object reference.    *> Menu Index 08
          03   pic x comp-5         value typeSibling.
          03   pic x(4) comp-5      value noAcceleratorKeyDefined.
          03   pic x(mCallbackSize) value SPACES.
          03   pic x(mStringSize)   value z"&Yellow" &
                                    z"This does nothing" & x"00".
      *>----------------------------------------------------------------
          03   object reference.    *> Menu Index 09
          03   pic x comp-5         value typeLastSiblingHasChildren.
          03   pic x(4) comp-5      value noAcceleratorKeyDefined.
          03   pic x(mCallbackSize) value SPACES.
          03   pic x(mStringSize)   value z"P&rimary" & x"00".
      *>----------------------------------------------------------------
          03   object reference.    *> Menu Index 10
          03   pic x comp-5         value typeSibling.
          03   pic x(4) comp-5      value noAcceleratorKeyDefined.
          03   pic x(mCallbackSize) value ProgramID & "OnItemSelected ".
          03   pic x(mStringSize)   value z"&Red" &
                                    z"Primary red" & x"00".
      *>----------------------------------------------------------------
          03   object reference.    *> Menu Index 11
          03   pic x comp-5         value typeSibling.
          03   pic x(4) comp-5      value noAcceleratorKeyDefined.
          03   pic x(mCallbackSize) value ProgramID & "OnItemSelected ".
          03   pic x(mStringSize)   value z"&Green" &
                                    z"Primary green" & x"00".
      *>----------------------------------------------------------------
          03   object reference.    *> Menu Index 12
          03   pic x comp-5         value typeLastSibling.
          03   pic x(4) comp-5      value noAcceleratorKeyDefined.
          03   pic x(mCallbackSize) value ProgramID & "OnItemSelected ".
          03   pic x(mStringSize)   value z"&Blue" &
                                    z"Primary blue" & x"00".
      *>----------------------------------------------------------------
          03   object reference.    *> Menu Index 13
          03   pic x comp-5         value typeHasChildren.
          03   pic x(4) comp-5      value noAcceleratorKeyDefined.
          03   pic x(mCallbackSize) value SPACES.
          03   pic x(mStringSize)   value z"Ch&eckMark" & x"00".
      *>----------------------------------------------------------------
          03   object reference.    *> Menu Index 14
          03   pic x comp-5         value typeLastSibling.
          03   pic x(4) comp-5      value noAcceleratorKeyDefined.
          03   pic x(mCallbackSize) value ProgramID & "OnItemSelected ".
          03   pic x(mStringSize)   value z"&Toggle" &
                                    z"Toggle checkmark" & x"00".
      *>----------------------------------------------------------------
          03   object reference.    *> Menu Index 15
          03   pic x comp-5         value typeLastSibling.
          03   pic x comp-5         value KS-Virtual-Key.
          03   pic x comp-5         value OVK-F1.
          03   pic xx.
          03   pic x(mCallbackSize) value ProgramID & "OnItemSelected ".
          03   pic x(mStringSize)   value z"&Help" &
                                    z"Display help" & x"00".
      *>----------------------------------------------------------------
      ***>>> End USER MENU DATA <<<***

       78 defaultMenuItems         value NEXT - mDataStart / mRecSize.
       
       01 mTable redefines mData occurs maxMenuItems.
          03 mReference            object reference.
          03 mType                 pic x comp-5.
             88 hasChildren        value 1 3.
             88 lastSibling        value 2 3 6.
             88 isSeparator        value 4 6.
          03 mAccelKey             K-keyDesc.
          03 mCallback             pic x(mCallbackSize).
          03 mString               pic x(mStringSize).
       01 numberOfMenuItems        pic x(4) comp-5 value
                                   defaultMenuItems.


      *>----------------------------------------------------------------
      *> Toolbar button definitions.
      *>----------------------------------------------------------------

      * The maximum number of buttons that can be stored within
      * the button data table.
       78 maxButtons               value 30.

      * Maximum length of tool tip text.
       78 bStringSize              value 55.

      * Size of each button record (one record per button).
       78 bRecSize                 value 64.

      *
      * CREATING A NEW TOOLBAR BUTTON STRCUTURE
      * ---------------------------------------
      *
      * The bData table structure below defines an ordered list
      * of buttons to be added to the toolbar. Buttons are added to
      * the toolbar in the order they appear in the list.
      *
      * Each element of the table defines a button on the toolbar
      * and is made up of five parts:
      *    1. Object reference of the created toolbar button.
      *    2. The index (within mData table) of the menu item to
      *       associate the button with (or zero for a separator).
      *    3. Resource ID of a bitmap within a resource file to use
      *       for the button when it is enabled.
      *    4. Resource ID of a bitmap within a resource file to use
      *       for the button when it is disabled.
      *    5. Tool tip to be displayed when mouse is over the button.
      *
      * To change the button structure defined below, use the existing
      * button records as an example to change the button structure, and
      * if required add further button records for any additional
      * buttons you wish to add to the toolbar.
      *

      ******************************************************************
      * NOTE - Each toolbar button record below must be bRecSize bytes.
      * ----   Unpredictable results may occur at run time if this
      *        table does not conform to the defined record structure.
      ******************************************************************
       
       01 bData.
       78 bDataStart               value NEXT.
      
      ***>>> Start USER BUTTON DATA <<<***
      *>----------------------------------------------------------------
      *   Button object reference.
          03   object reference.

      *   Menu item index to associate button with or zero if this
      *   button is to be a separator. This index refers to an index
      *   within the mData table above.
          03   pic x comp-5        value 14. *> "Toggle" menu item

      *   Resource ID of button bitmap.
          03   pic x(4) comp-5     value IDB-CHECK.

      *   Tool tip to be displayed when mouse is over button.
          03   pic x(bStringSize)  value z"Check/Uncheck".
      *>----------------------------------------------------------------
          03   object reference.
          03   pic x comp-5        value 0. *> separator
          03   pic x(4) comp-5     value 0.
          03   pic x(bStringSize).
      *>----------------------------------------------------------------
          03   object reference.
          03   pic x comp-5        value 10. *> "Red" menu item
          03   pic x(4) comp-5     value IDB-RED.
          03   pic x(bStringSize)  value z"Red".
      *>----------------------------------------------------------------
          03   object reference.
          03   pic x comp-5        value 11. *> "Green" menu item
          03   pic x(4) comp-5     value IDB-GREEN.
          03   pic x(bStringSize)  value z"Green".
      *>----------------------------------------------------------------
          03   object reference.
          03   pic x comp-5        value 12. *> "Blue" menu item
          03   pic x(4) comp-5     value IDB-BLUE.
          03   pic x(bStringSize)  value z"Blue".
      *>----------------------------------------------------------------
          03   object reference.
          03   pic x comp-5        value 0. *> separator
          03   pic x(4) comp-5     value 0.
          03   pic x(bStringSize).
      *>----------------------------------------------------------------
          03   object reference.
          03   pic x comp-5        value 5. *> "E&xit" menu item
          03   pic x(4) comp-5     value IDB-EXIT.
          03   pic x(bStringSize)  value z"Quit".
      *>----------------------------------------------------------------
          03   object reference.
          03   pic x comp-5        value 0. *> separator
          03   pic x(4) comp-5     value 0.
          03   pic x(bStringSize).
      *>----------------------------------------------------------------
          03   object reference.
          03   pic x comp-5        value 0. *> separator
          03   pic x(4) comp-5     value 0.
          03   pic x(bStringSize).
      *>----------------------------------------------------------------
          03   object reference.
          03   pic x comp-5        value 15. *> "Help" menu item
          03   pic x(4) comp-5     value IDB-HELP.
          03   pic x(bStringSize)  value z"Help".
      *>----------------------------------------------------------------
      ***>>> End USER BUTTON DATA <<<***
       
       78 defaultButtons           value NEXT - bDataStart / bRecSize.
       
       01 bTable redefines bData occurs maxButtons.
          03 bReference            object reference.
          03 bMenuIndex            pic x comp-5.
          03 bBitmapID             pic x(4) comp-5.
          03 bString               pic x(bStringSize).
       01 numberOfButtons          pic x(4) comp-5 value
                                   defaultButtons.
