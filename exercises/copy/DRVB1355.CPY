      *****************************************************************
      *    COPY-MEMBER: DRVB1355                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0055                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    09.08.2022                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * JIRA     ! DATUM  ! AENDERUNG                     ! NAME      *
      * ---------+--------+-------------------------------+---------- *
      * VLA-5133 !09.08.22! NEU                           ! KLEEMEIER *
      *          !        !                               !           *
      *          !        !                               !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRVB1355 - ERMITTELN MAST-DATEN PER STICHTAG      *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01 PI-DRVB1355.
          05 PI-DRVB1355-DATEN.
             10 PI-DRVB1355-MA-ST-NR        PIC S9(009) COMP.
             10 PI-DRVB1355-KTY-MAST-TYP    PIC S9(004) COMP.
             10 PI-DRVB1355-KNR-MAST-TYP    PIC S9(004) COMP.
             10 PI-DRVB1355-STICHTAG        PIC  X(010).
      *
       01 PO-DRVB1355.
          05 PO-DRVB1355-RC                      PIC  X(003).
             88 PO-DRVB1355-RC-OK                     VALUE 'OK '.
             88 PO-DRVB1355-RC-NOT-FOUND              VALUE 'NFD'.
          05 PO-DRVB1355-NI-VIVB0055.
             10 NI-DRVB1355-MA-ST-NR             PIC S9(004) COMP.
             10 NI-DRVB1355-MAST-DAT-VON         PIC S9(004) COMP.
             10 NI-DRVB1355-KTY-MAST-TYP         PIC S9(004) COMP.
             10 NI-DRVB1355-KNR-MAST-TYP         PIC S9(004) COMP.
             10 NI-DRVB1355-MAST-LAST-UPDATE     PIC S9(004) COMP.
             10 NI-DRVB1355-MAST-DAT-BIS         PIC S9(004) COMP.
          05 PO-DRVB1355-DCL-VIVB0055.
             10 PO-DRVB1355-MA-ST-NR             PIC S9(009) COMP.
             10 PO-DRVB1355-MAST-DAT-VON         PIC  X(026).
             10 PO-DRVB1355-KTY-MAST-TYP         PIC S9(004) COMP.
             10 PO-DRVB1355-KNR-MAST-TYP         PIC S9(004) COMP.
             10 PO-DRVB1355-MAST-LAST-UPDATE     PIC  X(026).
             10 PO-DRVB1355-MAST-DAT-BIS         PIC  X(010).
      ****
      **** ENDE COPY DRVB1355 ****************************************
      ****
