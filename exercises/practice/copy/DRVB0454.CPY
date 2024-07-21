      *****************************************************************
      *    COPY-MEMBER: DRVB0454                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0073  BA                    *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: B.SCHMIDT                                        *
      *    DATUM:    08.03.2000                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00477!08.03.00! 01 ! NEU                       !B.SCHMIDT *
      *                                                              *
      * JIRA     ! DATUM  ! AENDERUNG                     ! NAME     *
      * ---------+--------+-------------------------------+--------- *
      * VLA-5133 !04.08.22! DRVB1353 HINZU                ! KLEEMEIER*
      *          !        !                               !          *
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
       01  PI-ZWVB0073.
           05 PI-ZWVB0073-FUNKTION            PIC X(008).
              88 PI-ZWVB0073-DRVB0455             VALUE 'DRVB0455'.
040822        88 PI-ZWVB0073-DRVB1353             VALUE 'DRVB1353'.
      *