      *****************************************************************
      *    COPY-MEMBER: DRVB1351                                      *
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
      *    FUNKTION DRVB1351 - ERMITTELN LAUFENDE NUMMER (BZLP) GES.  *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01 PI-DRVB1351.
          05 PI-DRVB1351-DATEN.
             10 PI-DRVB1351-BZLP-ART        PIC  X(001).
             10 PI-DRVB1351-KNR-GESELL-TYP  PIC S9(004) BINARY.
      *
       01 PO-DRVB1351.
          05 PO-DRVB1351-RC                 PIC  X(003).
             88 PO-DRVB1351-RC-OK                VALUE 'OK '.
             88 PO-DRVB1351-RC-NOT-FOUND         VALUE 'NFD'.
          05 PO-DRVB1351-DATEN.
             10 PO-DRVB1351-BZLP-LFDNR      PIC S9(004) COMP.
      ****
      **** ENDE COPY DRVB1351 ****************************************
      ****
