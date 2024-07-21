      *****************************************************************
      *    COPY-MEMBER: DRVB0911                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0114                    *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    01.08.2008                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00309!01.08.08!2.1 ! NEU                       ! KLEEMEIER *
      * ASL00559!12.10.16!2.2 ! NEUE FUNKTION DRVB1152    ! H.WÖLFLIK *
      *         !        !    !                           !           *
      *                                                               *
010721* AUFTRAG    ! DATUM  ! AENDERUNG                   ! NAME      *
010721* -----------+--------+-----------------------------+---------- *
010721* VLA-2905   !01.07.21! DRVB1315 HINZU              ! KLEEMEIER *
      *            !        !                             !           *
      *                                                               *
      *****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN
      *                (IN SEPARATEN COPYS PRO FUNKTION)
      *
      *****************************************************************
      *
       01  PI-ZWVB0114.
           05  PI-ZWVB0114-FUNKTION            PIC  X(008).
               88 PI-ZWVB0114-DRVB0912              VALUE 'DRVB0912'.
               88 PI-ZWVB0114-DRVB1152              VALUE 'DRVB1152'.
010721         88 PI-ZWVB0114-DRVB1315              VALUE 'DRVB1315'.
      *
      ******* ENDE COPY-MEMBER ****************************************
      *