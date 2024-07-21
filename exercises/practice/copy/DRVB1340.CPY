      *****************************************************************
      *    --------------------------------------------------------   *
      *    COPY-MEMBER: DRVB1340                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FUER DAS UPRO ZWVB0012                       *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO UEBERGEBEN WERDEN       *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    26.04.2022                                       *
      *****************************************************************
      ****************************************************************
      * AENDERUNGEN:                                                 *
      *                                                              *
      * JIRA     ! DATUM  ! AENDERUNG                     ! NAME     *
      * -------------------------------------------------------------*
      * VLA-4642 !26.04.22! NEU                           ! KLEEMEIER*
      *          !        !                               !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FUER DIE FUNKTION DRVB1340                                *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB1340.
          05 PI-DRVB1340-MA-ST-NR           PIC S9(009) COMP.
          05 PI-DRVB1340-MS-NR              PIC S9(004) COMP.
          05 PI-DRVB1340-STICHTAG           PIC  X(010).
      *
       01 PO-DRVB1340.
          05 PO-DRVB1340-RC                 PIC  X(003).
             88 PO-DRVB1340-RC-OK                VALUE 'OK '.
             88 PO-DRVB1340-RC-LEER              VALUE 'LER'.
             88 PO-DRVB1340-RC-EOF               VALUE 'EOF'.
             88 PO-DRVB1340-RC-NOT-FOUND         VALUE 'NFD'.

          05 PO-DRVB1340-DATEN.
             10 PO-DRVB1340-MSBZ-DAT-VON    PIC  X(010).
      *
      *****************************************************************
      *---------------------- ENDE DRVB1340 --------------------------*
      *****************************************************************
