      *****************************************************************
      *    COPY-MEMBER: DRVB1016                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVBB099                    *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION ÜBERGEBEN WERDEN    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    02.05.2011                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB01168!02.05.11!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *                                                               *
110322* JIRA     ! DATUM  ! AENDERUNG                     ! NAME      *
110322* ------------------------------------------------------------- *
110322* VLA-4411 !11.03.22! EMAIL HINZU (C-VIEW) UND      !           *
110322*          !        ! PVCS-KOPF ENTFERNT            ! KLEEMEIER *
      *          !        !                               !           *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL          *
      *    PRO FUNKTION ÜBERGEBEN WERDEN --> PI/PO-BEREICH            *
      *                                                               *
      *    FUNKTION DRVB1016 - ERMITTELN SATZ AUS TABELLE NAMZ        *
      *                                                               *
      *****************************************************************

       01 PI-DRVB1016.
          05 PI-DRVB1016-DATEN.
             10 PI-DRVB1016-KTY-NAMZ-TYP       PIC S9(004)      BINARY.
             10 PI-DRVB1016-KNR-NAMZ-TYP       PIC S9(004)      BINARY.
             10 PI-DRVB1016-NAMZ-LFD           PIC S9(004)      BINARY.
             10 PI-DRVB1016-KTY-NAMZ-FUNK      PIC S9(004)      BINARY.
             10 PI-DRVB1016-KNR-NAMZ-FUNK      PIC S9(004)      BINARY.
             10 PI-DRVB1016-NAMZ-DAT-VON       PIC  X(026).

       01 PO-DRVB1016.
         03 PO-DRVB1016-RC                     PIC  X(003).
             88 PO-DRVB1016-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1016-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB1016-DATEN.
110322*** 05 PO-DRVB1016-NI-VIVBB099.
110322    05 PO-DRVB1016-NI-VIVBC099.
             10 NI-DRVB1016-KTY-NAMZ-TYP       PIC S9(004)      BINARY.
             10 NI-DRVB1016-KNR-NAMZ-TYP       PIC S9(004)      BINARY.
             10 NI-DRVB1016-NAMZ-LFD           PIC S9(004)      BINARY.
             10 NI-DRVB1016-KTY-ANREDE         PIC S9(004)      BINARY.
             10 NI-DRVB1016-KNR-ANREDE         PIC S9(004)      BINARY.
             10 NI-DRVB1016-NAMZ-TITEL-AKAD    PIC S9(004)      BINARY.
             10 NI-DRVB1016-NAMZ-NAME          PIC S9(004)      BINARY.
             10 NI-DRVB1016-NAMZ-VORNAME       PIC S9(004)      BINARY.
             10 NI-DRVB1016-KTY-NAMZ-FUNK      PIC S9(004)      BINARY.
             10 NI-DRVB1016-KNR-NAMZ-FUNK      PIC S9(004)      BINARY.
             10 NI-DRVB1016-NAMZ-DAT-VON       PIC S9(004)      BINARY.
             10 NI-DRVB1016-NAMZ-LAST-UPDATE   PIC S9(004)      BINARY.
             10 NI-DRVB1016-NAMZ-DAT-BIS       PIC S9(004)      BINARY.
110322       10 NI-DRVB1016-NAMZ-EMAIL         PIC S9(004)      BINARY.

110322*** 05 PO-DRVB1016-DCL-VIVBB099.
110322    05 PO-DRVB1016-DCL-VIVBC099.
             10 PO-DRVB1016-KTY-NAMZ-TYP       PIC S9(004)      BINARY.
             10 PO-DRVB1016-KNR-NAMZ-TYP       PIC S9(004)      BINARY.
             10 PO-DRVB1016-NAMZ-LFD           PIC S9(004)      BINARY.
             10 PO-DRVB1016-KTY-ANREDE         PIC S9(004)      BINARY.
             10 PO-DRVB1016-KNR-ANREDE         PIC S9(004)      BINARY.
             10 PO-DRVB1016-NAMZ-TITEL-AKAD    PIC  X(010).
             10 PO-DRVB1016-NAMZ-NAME          PIC  X(040).
             10 PO-DRVB1016-NAMZ-VORNAME       PIC  X(020).
             10 PO-DRVB1016-KTY-NAMZ-FUNK      PIC S9(004)      BINARY.
             10 PO-DRVB1016-KNR-NAMZ-FUNK      PIC S9(004)      BINARY.
             10 PO-DRVB1016-NAMZ-DAT-VON       PIC  X(026).
             10 PO-DRVB1016-NAMZ-LAST-UPDATE   PIC  X(026).
             10 PO-DRVB1016-NAMZ-DAT-BIS       PIC  X(010).
110322       10 PO-DRVB1016-NAMZ-EMAIL         PIC  X(080).
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1016   --------------------------*
      *---------------------------------------------------------------*
