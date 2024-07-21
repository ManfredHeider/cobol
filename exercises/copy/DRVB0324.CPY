      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0324.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0324.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:52   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   20 Nov 1998 12:18:38   T13AE42
      *ASL00024 - Neuerstellung wegen Delta-Rueckkonvertierung
      *
      *   Rev 1.0   09 Jul 1998 09:58:40   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0324                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0015                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHMIDT                                          *
      *    DATUM:    08.10.1998                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00024!08.10.98! 01 ! NEUERSTELLUNG             ! SCHMIDT  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0324                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRVB0324.
          05 PI-DRVB0324-MAAN-BEZUGS-NR     PIC S9(009) COMP.
      *
       01 PO-DRVB0324.

          05 PO-DRVB0324-RC                 PIC  X(003).
             88 PO-DRVB0324-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0324-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0324-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0324-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0324-DATEN.
             10 PO-DRVB0324-MAAN-STRASSE     PIC  X(040).
             10 PO-DRVB0324-MAAN-PLZ-AKTUELL PIC  X(009).
             10 PO-DRVB0324-MAAN-ORT         PIC  X(040).
             10 PO-DRVB0324-KNR-AUSL         PIC S9(004)  COMP.
             10 PO-DRVB0324-KTY-AUSL         PIC S9(004)  COMP.
             10 PO-DRVB0324-KNR-GRND         PIC S9(004)  COMP.
             10 PO-DRVB0324-KTY-GRND         PIC S9(004)  COMP.
      *
      ***** ENDE COPY-MEMBER DRVB0324 *****************************
