      * This copyfile should be included as part of the EVALUATE
      * statement in toolbar template programs that require access
      * to menu and toolbar maintenance functions.

          WHEN "ADD-MENU-ITEM"

               PERFORM Add-Menu-Item

          WHEN "DELETE-MENU-ITEM"

               PERFORM Delete-Menu-Item

          WHEN "UPDATE-MENU-TEXT"

               PERFORM Update-Menu-Text

          WHEN "TOGGLE-CHECKMARK"

               PERFORM Toggle-CheckMark

          WHEN "ENABLE-OBJECT"

               PERFORM Enable-Object

          WHEN "DISABLE-OBJECT"

               PERFORM Disable-Object

          WHEN "ADD-TOOLBAR-BUTTON"

               PERFORM Add-Toolbar-Button
