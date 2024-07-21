      *****************************************************************
      *    COPY-MEMBER: DRVB1349                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0237                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    02.08.2022                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * JIRA     ! DATUM  ! AENDERUNG                     ! NAME      *
      * ---------+--------+-------------------------------+---------- *
      * VLA-5133 !02.08.22! NEU                           ! KLEEMEIER *
      *          !        !                               !           *
      *          !        !                               !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRVB1349 - ERMITTELN MAX. LAUFENDE NUMMER (BZLP)  *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01 PI-DRVB1349.
          05 PI-DRVB1349-DATEN.
             10 PI-DRVB1349-DUMMY           PIC  X(001).
      *
       01 PO-DRVB1349.
          05 PO-DRVB1349-RC                 PIC  X(003).
             88 PO-DRVB1349-RC-OK                VALUE 'OK '.
             88 PO-DRVB1349-RC-NOT-FOUND         VALUE 'NFD'.
          05 PO-DRVB1349-DATEN.
             10 PO-DRVB1349-BZLP-LFDNR      PIC S9(004) COMP.
      ****
      **** ENDE COPY DRVB1349 ****************************************
      ****
