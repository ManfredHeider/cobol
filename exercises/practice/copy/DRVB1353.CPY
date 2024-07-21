      *****************************************************************
      *    COPY-MEMBER: DRVB1353                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0073                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    04.08.2022                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * JIRA     ! DATUM  ! AENDERUNG                     ! NAME      *
      * ---------+--------+-------------------------------+---------- *
      * VLA-5133 !04.08.22! NEU                           ! KLEEMEIER *
      *          !        !                               !           *
      *          !        !                               !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRVB1353 - ERMITTELN MAX(BA-DAT-VON) FÜR VBNR     *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01 PI-DRVB1353.
          05 PI-DRVB1353-DATEN.
             10 PI-DRVB1353-MA-ST-NR        PIC S9(009) COMP.
             10 PI-DRVB1353-BA-KTY-NR       PIC S9(004) COMP.
             10 PI-DRVB1353-BA-KNR-NR       PIC S9(004) COMP.
      *
       01 PO-DRVB1353.
          05 PO-DRVB1353-RC                 PIC  X(003).
             88 PO-DRVB1353-RC-OK                VALUE 'OK '.
             88 PO-DRVB1353-RC-NOT-FOUND         VALUE 'NFD'.
          05 PO-DRVB1353-DATEN.
             10 PO-DRVB1353-BA-DAT-VON      PIC  X(010).
      ****
      **** ENDE COPY DRVB1353 ****************************************
      ****
