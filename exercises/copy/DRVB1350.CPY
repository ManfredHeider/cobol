      *****************************************************************
      *    COPY-MEMBER: DRVB1350                                      *
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
      *    FUNKTION DRVB1350 - ERMITTELN MIN. STATUS (BZLP)           *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01 PI-DRVB1350.
          05 PI-DRVB1350-DATEN.
             10 PI-DRVB1350-DUMMY           PIC  X(001).
      *
       01 PO-DRVB1350.
          05 PO-DRVB1350-RC                 PIC  X(003).
             88 PO-DRVB1350-RC-OK                VALUE 'OK '.
             88 PO-DRVB1350-RC-NOT-FOUND         VALUE 'NFD'.
          05 PO-DRVB1350-DATEN.
             10 PO-DRVB1350-BZLP-STATUS     PIC  X(001).
      ****
      **** ENDE COPY DRVB1350 ****************************************
      ****
