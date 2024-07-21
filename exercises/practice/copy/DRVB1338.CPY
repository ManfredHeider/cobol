      *****************************************************************
      *    COPY-MEMBER: DRVB1338                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FUER DAS LESE-OZP ZWVBA302                   *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION UEBERGEBEN WERDEN   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: HITZEMANN                                        *
      *    DATUM:    13.04.2022                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * JIRA     | DATUM  | AENDERUNG                     | NAME      *
      * ---------+--------+-------------------------------+---------- *
      * VLA-4491 |13.04.22| NEU                           | HITZEMANN *
      *          |        |                               |           *
      *          |        |                               |           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHAELT DIE INFORMATIONEN, DIE EINMAL         *
      *    PRO FUNKTION UEBERGEBEN WERDEN --> PI/PO-BEREICH           *
      *                                                               *
      *    FUNKTION DRVB1338 - ERMITTELN EHNP AUS TABELLE DETAILNP F. * 
      *                        DIE SICHTEN: 12/12, 24/12, 36/12, 36/1 *
      *                                                               *
      *****************************************************************

       01 PI-DRVB1338.
          05 PI-DRVB1338-DATEN.
             10 PI-DRVB1338-DETAILNP-BETR      PIC S9(009)      BINARY.

       01 PO-DRVB1338.
         03 PO-DRVB1338-RC                     PIC  X(003).
             88 PO-DRVB1338-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1338-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB1338-DATEN.
          05 PO-DRVB1338-NI-VIVBA302.
             10 NI-DRVB1338-DETAILNP-VERSION   PIC S9(004)      BINARY.
             10 NI-DRVB1338-DETAILNP-EH-1212   PIC S9(004)      BINARY.
             10 NI-DRVB1338-DETAILNP-EH-2412   PIC S9(004)      BINARY.
             10 NI-DRVB1338-DETAILNP-EH-3612   PIC S9(004)      BINARY.
             10 NI-DRVB1338-DETAILNP-EH-3601   PIC S9(004)      BINARY.
          05 PO-DRVB1338-DCL-VIVBA302.
             10 PO-DRVB1338-DETAILNP-VERSION   PIC  X(007).
             10 PO-DRVB1338-DETAILNP-EH-1212   PIC S9(009)V9(01) 
                                                        PACKED-DECIMAL.
             10 PO-DRVB1338-DETAILNP-EH-2412   PIC S9(009)V9(01) 
                                                        PACKED-DECIMAL.
             10 PO-DRVB1338-DETAILNP-EH-3612   PIC S9(009)V9(01) 
                                                        PACKED-DECIMAL.
             10 PO-DRVB1338-DETAILNP-EH-3601   PIC S9(009)V9(01) 
                                                        PACKED-DECIMAL.
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1338   --------------------------*
      *---------------------------------------------------------------*