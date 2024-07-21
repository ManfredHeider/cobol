      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0209                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0015                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHMIDT                                          *
      *    DATUM:    18.02.1998                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00005!18.02.98! 01 ! NEUERSTELLUNG             ! SCHMIDT  *
      * AAS00049!02.06.98! 02 ! NEUE FUNKTION DRVB0246    ! WALB     *
      * AKI00001!23.06.98! 03 ! NEUE FUNKTION DRVB0256    !zgur-vujic*
      * AAS00049!06.07.98! 04 ! NEUE FUNKTION DRVB0296    ! WALB     *
      * ASL00024!08.10.98! 05 ! NEUE FUNKTION DRVB0324    ! SCHMIDT,B*
      * AAS00051!23.04.99! 06 ! NEUE FUNKTION DRVB0386    ! TH.WILLIG*
      * AAS00051!02.09.99! 07 ! LÖSCHEN NICHT BENÖTIGTER  ! TH.WILLIG*
      *         !        !    ! FUNKTIONEN DRVB0385 UND   !          *
      *         !        !    ! DRVB0387                  !          *
      * AAS00143!22.09.99! 08 ! NEUE FUNKTION DRVB0444    ! P.Dürr   *
      * AVB00875!19.03.08! 09 ! NEUE FUNKTION DRVB0896    ! J.Maess  *
      * AVB01127!29.03.12!2.10! NEUE FUNKTION DRVB1053    !TH.WILLIG *
      * AVB01400!01.09.16!2.6 ! NEUE FUNKTION DRVB1148    ! Walb     *
      *         !        !    !                           !          *
      *                                                              *
      * JIRA     ! DATUM  ! AENDERUNG                     ! NAME     *         
      * ---------+--------+-------------------------------+----------*         
      * VLA-2627 !25.05.21! DRVB1310 HINZU, PVCS ENTFERNT ! KLEEMEIER*         
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
       01  PI-ZWVB0015.
           05  PI-ZWVB0015-FUNKTION            PIC  X(008).
               88 PI-ZWVB0015-DRVB0210              VALUE 'DRVB0210'.
               88 PI-ZWVB0015-DRVB0246              VALUE 'DRVB0246'.
               88 PI-ZWVB0015-DRVB0256              VALUE 'DRVB0256'.
060798         88 PI-ZWVB0015-DRVB0296              VALUE 'DRVB0296'.
081098         88 PI-ZWVB0015-DRVB0324              VALUE 'DRVB0324'.
230499         88 PI-ZWVB0015-DRVB0386              VALUE 'DRVB0386'.
220999         88 PI-ZWVB0015-DRVB0444              VALUE 'DRVB0444'.
110308         88 PI-ZWVB0015-DRVB0896              VALUE 'DRVB0896'.
290312         88 PI-ZWVB0015-DRVB1053              VALUE 'DRVB1053'.
010916         88 PI-ZWVB0015-DRVB1148              VALUE 'DRVB1148'.
250521         88 PI-ZWVB0015-DRVB1310              VALUE 'DRVB1310'.
      *
      ******* ENDE COPY-MEMBER DRVB0209 *****************************
      *