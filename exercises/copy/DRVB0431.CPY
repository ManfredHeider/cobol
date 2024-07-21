      *****************************************************************
      *    COPY-MEMBER: DRVB0431                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0020  BES                   *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Dürr                                             *
      *    DATUM:    20.07.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !20.07.99! 01 ! NEU                       ! Dürr     *
      * ASL00056!25.05.00! 02 ! Neue Funktion DRVB0482    ! Walb     *
      * ASL00102!27.06.01! 03 ! Neue Funktion DRVB0547    ! Walb     *
      * ASL00183!06.09.04! 04 ! Neue Funktion DRVB0738    ! Dürr     *
      * ASL00544!22.10.15! 05 ! Neue Funktion DRVB1124    ! J.Hering *
      * ASL00559!12.10.16! 06 ! Neue Funktion DRVB1153    ! H.Wölflik*
      * ASL00573!05.12.17! 07 ! Neue Funktion DRVB1180    ! G.Bruder *
      * ASL00569!23.02.18! 08 ! Neue Funktion DRVB1186    ! G.Bruder *
      *         !        !    !                           !          *
      * JIRA     ! DATUM  ! AENDERUNG                     ! NAME     *
      * ---------+--------+-------------------------------+----------*
      * VLA-4933 !15.06.22! DRVB1346 HINZU, PVCS-KOPF RAUS! KLEEMEIER*
      *          !        !                               !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN
      *                (IN SEPARATEN COPYS PRO FUNKTION)
      *
      ****************************************************************
      *
       01  PI-ZWVB0020.
           05  PI-ZWVB0020-FUNKTION            PIC  X(008).
               88 PI-ZWVB0020-DRVB0432              VALUE 'DRVB0432'.
250500         88 PI-ZWVB0020-DRVB0482              VALUE 'DRVB0482'.
270601         88 PI-ZWVB0020-DRVB0547              VALUE 'DRVB0547'.
060904         88 PI-ZWVB0020-DRVB0738              VALUE 'DRVB0738'.
221015         88 PI-ZWVB0020-DRVB1124              VALUE 'DRVB1124'.
121016         88 PI-ZWVB0020-DRVB1153              VALUE 'DRVB1153'.
051217         88 PI-ZWVB0020-DRVB1180              VALUE 'DRVB1180'.
230218         88 PI-ZWVB0020-DRVB1186              VALUE 'DRVB1186'.
150622         88 PI-ZWVB0020-DRVB1346              VALUE 'DRVB1346'.
      *
      ******* ENDE COPY-MEMBER **************************************
      *