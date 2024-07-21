      *****************************************************************
      *    --------------------------------------------------------   *
      *    COPY-MEMBER: DRVB1339                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FUER DAS UPRO ZWVB0007                       *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO UEBERGEBEN WERDEN       *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    25.04.2022                                       *
      *****************************************************************
      ****************************************************************
      * AENDERUNGEN:                                                 *
      *                                                              *
      * JIRA     ! DATUM  ! AENDERUNG                     ! NAME     *
      * -------------------------------------------------------------*
      * VLA-4642 !25.04.22! NEU                           ! KLEEMEIER*
      *          !        !                               !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FUER DIE FUNKTION DRVB1339                                *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB1339.
          05 PI-DRVB1339-MA-ST-NR           PIC S9(009) COMP.
          05 PI-DRVB1339-VBP-NR             PIC S9(004) COMP.
          05 PI-DRVB1339-STICHTAG           PIC  X(010).
      *
       01 PO-DRVB1339.
          05 PO-DRVB1339-RC                 PIC  X(003).
             88 PO-DRVB1339-RC-OK                VALUE 'OK '.
             88 PO-DRVB1339-RC-LEER              VALUE 'LER'.
             88 PO-DRVB1339-RC-EOF               VALUE 'EOF'.
             88 PO-DRVB1339-RC-NOT-FOUND         VALUE 'NFD'.

          05 PO-DRVB1339-DATEN.
             10 PO-DRVB1339-VBPZ-DAT-VON    PIC  X(010).
      *
      *****************************************************************
      *---------------------- ENDE DRVB1339 --------------------------*
      *****************************************************************
