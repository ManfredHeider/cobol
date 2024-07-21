      *****************************************************************
      *    --------------------------------------------------------   *
      *    COPY-MEMBER: DRVB1346                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FUER DAS UPRO ZWVB0020                       *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO UEBERGEBEN WERDEN       *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    15.06.2022                                       *
      *****************************************************************
      ****************************************************************
      * AENDERUNGEN:                                                 *
      *                                                              *
      * JIRA     ! DATUM  ! AENDERUNG                     ! NAME     *
      * -------------------------------------------------------------*
      * VLA-4933 !15.06.22! NEU                           ! KLEEMEIER*
      *          !        !                               !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FUER DIE FUNKTION DRVB1346                                *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB1346.
          05 PI-DRVB1346-MA-ST-NR           PIC S9(009) COMP.
      *
       01 PO-DRVB1346.
          05 PO-DRVB1346-RC                 PIC  X(003).
             88 PO-DRVB1346-RC-OK                VALUE 'OK '.
             88 PO-DRVB1346-RC-NOT-FOUND         VALUE 'NFD'.

          05 PO-DRVB1346-DATEN.
             10 PO-DRVB1346-SPERRE-BIS      PIC  X(010).
      *
      *****************************************************************
      *---------------------- ENDE DRVB1346 --------------------------*
      *****************************************************************
